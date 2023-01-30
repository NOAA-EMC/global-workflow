#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from netCDF4 import Dataset
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_isotime, to_fv3time, to_timedelta
from pygw.fsutils import rm_p
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
from pygw.logger import logit
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    @logit(logger, name="AerosolAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_enkf = int(self.config['CASE_ENKF'][1:])
        _window_begin = self.current_cycle - to_timedelta(f"{self.config['assim_freq']}H") / 2

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
            'npx_ges': _res + 1,
            'npy_ges': _res + 1,
            'npz_ges': self.config.LEVS - 1,
            'npz': self.config.LEVS - 1,
            'npx_anl': _res_enkf + 1,
            'npy_anl': _res_enkf + 1,
            'npz_anl': self.config['LEVS'] - 1
            'AERO_WINDOW_BEGIN': to_isotime(_window_begin),
            'AERO_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
            'BKG_ISOTIME': to_isotime(self.current_cycle),
            'BKG_YYYYmmddHHMMSS': to_fv3time(self.current_cycle),
            'cdate_fv3': to_fv3time(self.current_cycle),
            'comin_ges_atm': self.config.COMIN_GES.replace('chem', 'atmos'),  # 'chem' is COMPONENT, aerosol fields are in 'atmos' tracers
            }
        )

        # task_config is everything that is this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        super().initialize()

        # stage CRTM fix files
        crtm_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_crtm_coeff.yaml')
        crtm_fix_list = YAMLFile(path=crtm_fix_list_path)
        crtm_fix_list = Template.substitute_structure(crtm_fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_jedi_fix.yaml')
        jedi_fix_list = YAMLFile(path=jedi_fix_list_path)
        jedi_fix_list = Template.substitute_structure(jedi_fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        # copy BUMP files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'bump_aero') in ['bump_aero']:
            FileHandler(AerosolAnalysis.get_berror_dict(self.task_config)).sync()

        # stage backgrounds
        FileHandler(self._get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate variational YAML file
        yaml_out = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        varda_yaml = YAMLFile(path=self.task_config['AEROVARYAML'])
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOUBLE_CURLY_BRACES, self.task_config.get)
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOLLAR_PARENTHESES, self.task_config.get)
        varda_yaml.save(yaml_out)

        # link var executable
        exe_src = self.task_config['JEDIVAREXE']
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        newdirs = [
            os.path.join(self.task_config['DATA'], 'anl'),
            os.path.join(self.task_config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def finalize(self: Analysis) -> None:
        # ---- tar up diags
        # path of output tar statfile
        aerostat = os.path.join(self.task_config['COMOUTaero'], f"{self.task_config['APREFIX']}aerostat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(aerostat, "w") as archive:
            for diagfile in diags:
                archive.add(f"{diagfile}.gz")

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        dest = os.path.join(self.task_config['COMOUTaero'], f"{self.task_config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        yaml_copy = {
            'mkdir': self.task_config['COMOUTaero'],
            'copy': [src, dest]
        }  # yaml_copy is not used, remove???

        # ---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        # ---- all of the rest of this method will need to be changed but requires model and JEDI changes
        # ---- copy RESTART fv_tracer files for future reference
        fms_bkg_file_template = os.path.join(self.task_config.comin_ges_atm, 'RESTART', f'{self.task_config.cdate_fv3}.fv_tracer.res.tile1.nc')
        bkglist = []
        for itile in range(1, self.task_config.ntiles+1):
            bkg_path = fms_bkg_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(self.task_config['COMOUTaero'], f'aeroges.{os.path.basename(bkg_path)}')
            bkglist.append([bkg_path, dest])
        FileHandler({'copy': bkglist}).sync()

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # ---- move increments to ROTDIR
        logger.info('Moving increments to ROTDIR')
        fms_inc_file_template = os.path.join(self.task_config['DATA'], 'anl', f'aeroinc.{self.task_config.cdate_fv3}.fv_tracer.res.tile1.nc')
        inclist = []
        for itile in range(1, self.task_config.ntiles+1):
            inc_path = fms_inc_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(self.task_config['COMOUTaero'], os.path.basename(inc_path))
            inclist.append([inc_path, dest])
        FileHandler({'copy': inclist}).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self: Analysis) -> None:
        """This method adds increments to RESTART files to get an analysis
        NOTE this is only needed for now because the model cannot read aerosol increments.
        This method will be assumed to be deprecated before this is implemented operationally
        """
        # only need the fv_tracer files
        fms_inc_file_template = os.path.join(self.task_config['DATA'], 'anl', f'aeroinc.{self.task_config.cdate_fv3}.fv_tracer.res.tileX.nc')
        fms_bkg_file_template = os.path.join(self.task_config.comin_ges_atm, 'RESTART', f'{self.task_config.cdate_fv3}.fv_tracer.res.tileX.nc')
        # TODO: this list should be read from the yaml file
        incvars = ['dust1', 'dust2', 'dust3', 'dust4', 'dust5',
                   'seas1', 'seas2', 'seas3', 'seas4',
                   'so4', 'oc1', 'oc2', 'bc1', 'bc2']
        super().add_fv3_increments(fms_inc_file_template, fms_bkg_file_template, incvars, self.task_config.ntiles)


@logit(logger)
def get_bkg_dict(task_config: Dict[str, Any]) -> Dict[str, List[str]]:
    """
    Return FileHandler task_config for model backgrounds
    """
    # NOTE for now this is FV3 RESTART files and just assumed to be fh006

    # get FV3 RESTART files, this will be a lot simpler when using history files
    rst_dir = os.path.join(task_config.comin_ges_atm, 'RESTART')  # for now, option later?

    # aerosol DA only needs core/tracer
    bkglist = []
    basename = f'{task_config.cdate_fv3}.coupler.res'
    bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
    for tt in range(1, task_config.ntiles + 1):
        basename = f'{task_config.cdate_fv3}.fv_core.res.tile{tt}.nc'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
        basename = f'{task_config.cdate_fv3}.fv_tracer.res.tile{tt}.nc'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config['DATA'], 'bkg', basename)])
    bkg_dict = {
        'mkdir': [os.path.join(task_config['DATA'], 'bkg')],
        'copy': bkglist,
    }
    return bkg_dict


@logit(logger)
def get_berror_dict(config: Dict[str, Any]) -> Dict[str, List[str]]:
    """
    Return FileHandler configuration for berror
    """

    # aerosol static-B needs nicas, cor_rh, cor_rv and stddev files.
    b_dir = config['BERROR_DATA_DIR']
    b_datestr = config['BERROR_DATE']
    berror_list = []
    berror_list.append([
        os.path.join(b_dir, b_datestr + '.cor_rh.coupler.res'),
        os.path.join(config['DATA'], 'berror', b_datestr + '.cor_rh.coupler.res')
    ])
    berror_list.append([
        os.path.join(b_dir, b_datestr + '.cor_rv.coupler.res'),
        os.path.join(config['DATA'], 'berror', b_datestr + '.cor_rv.coupler.res')
    ])
    berror_list.append([
        os.path.join(b_dir, b_datestr + '.stddev.coupler.res'),
        os.path.join(config['DATA'], 'berror', b_datestr + '.stddev.coupler.res')
    ])
    for tt in range(1, config.ntiles + 1):
        berror_list.append([
            os.path.join(b_dir, f'{b_datestr}.cor_rh.fv_tracer.res.tile{tt}.nc'),
            os.path.join(config['DATA'], 'berror', f'{b_datestr}.cor_rh.fv_tracer.res.tile{tt}.nc')
        ])
        berror_list.append([
            os.path.join(b_dir, f'{b_datestr}.cor_rv.fv_tracer.res.tile{tt}.nc'),
            os.path.join(config['DATA'], 'berror', f'{b_datestr}.cor_rv.fv_tracer.res.tile{tt}.nc')
        ])
        berror_list.append([
            os.path.join(b_dir, f'{b_datestr}.stddev.fv_tracer.res.tile{tt}.nc'),
            os.path.join(config['DATA'], 'berror', f'{b_datestr}.stddev.fv_tracer.res.tile{tt}.nc')
        ])

    nproc = config.ntiles * config['layout_x'] * config['layout_y']
    for nn in range(1, nproc + 1):
        berror_list.append([
            os.path.join(b_dir, f'nicas_aero_nicas_local_{nproc:06}-{nn:06}.nc'),
            os.path.join(config['DATA'], 'berror', f'nicas_aero_nicas_local_{nproc:06}-{nn:06}.nc')
        ])
    berror_dict = {
        'mkdir': [os.path.join(config['DATA'], 'berror')],
        'copy': berror_list,
    }
    return berror_dict
