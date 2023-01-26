from pygfs.task.analysis import Analysis
from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_isotime, to_fv3time, to_timedelta
from pygw.fsutils import rm_p
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
from pygw.logger import logit

import datetime as dt
import glob
import gzip
import netCDF4 as nc
import os
import tarfile
import logging

logger = logging.getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    @logit(logger, name=AerosolAnalysis)
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_enkf = int(self.config['CASE_ENKF'][1:])

        # Add to global config for convenience, or should this be in a "task_config"?
        tmp_dict = {
            'ntiles': 6,
            'npx_ges': _res + 1,
            'npy_ges': _res + 1,
            'npz_ges': self.config['LEVS'] - 1,
            'npz': self.config['LEVS'] - 1,
            'npx_anl': _res_enkf + 1,
            'npy_anl': _res_enkf + 1,
            'npz_anl': self.config['LEVS'] - 1,
        }
        self.config = AttrDict(self.config, **tmp_dict)

        # Add to runtime_config
        window_begin = self.current_cycle - to_timedelta(f"{self.config['assim_freq']}H") / 2

        tmp_dict = {
            'AERO_WINDOW_BEGIN': to_isotime(window_begin),
            'AERO_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
            'BKG_ISOTIME': to_isotime(self.current_cycle),
            'BKG_YYYYmmddHHMMSS': to_fv3time(self.current_cycle),
        }
        self.runtime_config = AttrDict(self.runtime_config, **tmp_dict)


    @logit(logger)
    def initialize(self):
        super().initialize()
        # stage fix files
        crtm_fix_list_path = os.path.join(self.config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_crtm_coeff.yaml')
        crtm_fix_list = YAMLFile(path=crtm_fix_list_path)
        crtm_fix_list = Template.substitute_structure(crtm_fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.runtime_config.get)
        FileHandler(crtm_fix_list).sync()
        jedi_fix_list_path = os.path.join(self.config['HOMEgfs'], 'parm', 'parm_gdas', 'aero_jedi_fix.yaml')
        jedi_fix_list = YAMLFile(path=jedi_fix_list_path)
        jedi_fix_list = Template.substitute_structure(jedi_fix_list, TemplateConstants.DOLLAR_PARENTHESES, self.runtime_config.get)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        if self.config.get('STATICB_TYPE', 'bump_aero') in ['bump_aero']:
            # copy BUMP files, otherwise it will assume ID matrix
            FileHandler(self.get_berror_dict()).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict()).sync()

        # generate variational YAML file
        yaml_out = os.path.join(self.config['DATA'], f"{self.config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        varda_yaml = YAMLFile(path=self.config['AEROVARYAML'])
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOUBLE_CURLY_BRACES, self.runtime_config.get)
        varda_yaml = Template.substitute_structure(varda_yaml, TemplateConstants.DOLLAR_PARENTHESES, self.runtime_config.get)
        varda_yaml.save(yaml_out)

        # link var executable
        exe_src = self.config['JEDIVAREXE']
        exe_dest = os.path.join(self.config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        newdirs = [
            os.path.join(self.config['DATA'], 'anl'),
            os.path.join(self.config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    def configure(self):
        """Compute additional variables and add them to the root configuration"""
        super().configure()


    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()
        # ---- tar up diags
        # path of output tar statfile
        aerostat = os.path.join(self.config['COMOUTaero'], f"{self.config['APREFIX']}aerostat")
        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.config['DATA'], 'diags', 'diag*nc4'))
        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)
        # open tar file for writing
        archive = tarfile.open(aerostat, "w")
        for diagfile in diags:
            archive.add(f"{diagfile}.gz")
        archive.close()
        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.config['DATA'], f"{self.config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        dest = os.path.join(self.config['COMOUTaero'], f"{self.config['CDUMP']}.t{self.cyc}z.aerovar.yaml")
        yaml_copy = {
            'mkdir': self.config['COMOUTaero'],
            'copy': [src, dest]
        }
        # ---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        # ---- all of the rest of this method will need to be changed but requires model and JEDI changes
        # ---- copy RESTART fv_tracer files for future reference
        cdate_fv3 = self.current_cycle.strftime('%Y%m%d.%H%M%S')
        comin_ges = self.config['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        fms_bkg_file_template = os.path.join(comin_ges_atm, 'RESTART', f'{cdate_fv3}.fv_tracer.res.tile1.nc')
        bkglist = []
        for itile in range(1, 7):
            bkg_path = fms_bkg_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(self.config['COMOUTaero'], f'aeroges.{os.path.basename(bkg_path)}')
            bkglist.append([bkg_path, dest])
        FileHandler({'copy': bkglist}).sync()
        # ---- add increments to RESTART files
        self.add_fms_cube_sphere_increments()
        # ---- move increments to ROTDIR
        fms_inc_file_template = os.path.join(self.config['DATA'], 'anl', f'aeroinc.{cdate_fv3}.fv_tracer.res.tile1.nc')
        inclist = []
        for itile in range(1, 7):
            inc_path = fms_inc_file_template.replace('tile1', f'tile{itile}')
            dest = os.path.join(self.config['COMOUTaero'], os.path.basename(inc_path))
            inclist.append([inc_path, dest])
        FileHandler({'copy': inclist}).sync()

    def clean(self):
        super().clean()

    def add_fms_cube_sphere_increments(self):
        """This method adds increments to RESTART files to get an analysis
        NOTE this is only needed for now because the model cannot read aerosol increments.
        This method will be assumed to be deprecated before this is implemented operationally
        """
        # only need the fv_tracer files
        cdate_fv3 = self.current_cycle.strftime('%Y%m%d.%H%M%S')
        fms_inc_file_template = os.path.join(self.config['DATA'], 'anl', f'aeroinc.{cdate_fv3}.fv_tracer.res.tile1.nc')
        comin_ges = self.config['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        fms_bkg_file_template = os.path.join(comin_ges_atm, 'RESTART', f'{cdate_fv3}.fv_tracer.res.tile1.nc')
        incvars = ['dust1', 'dust2', 'dust3', 'dust4', 'dust5',
                   'seas1', 'seas2', 'seas3', 'seas4',
                   'so4', 'oc1', 'oc2', 'bc1', 'bc2']
        for itile in range(1, 7):
            inc_path = fms_inc_file_template.replace('tile1', f'tile{itile}')
            bkg_path = fms_bkg_file_template.replace('tile1', f'tile{itile}')
            with nc.Dataset(inc_path, mode='r') as incfile:
                with nc.Dataset(bkg_path, mode='a') as rstfile:
                    for vname in incvars:
                        increment = incfile.variables[vname][:]
                        bkg = rstfile.variables[vname][:]
                        anl = bkg + increment
                        rstfile.variables[vname][:] = anl[:]
                        try:
                            rstfile.variables[vname].delncattr('checksum')  # remove the checksum so fv3 does not complain
                        except AttributeError:
                            pass  # checksum is missing, move on

    def get_bkg_dict(self):
        """
        Return FileHandler config for model backgrounds
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006
        comin_ges = self.config['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        rst_dir = os.path.join(comin_ges_atm, 'RESTART')  # for now, option later?
        # date variable string format
        cdate_fv3 = to_fv3time(self.current_cycle)
        # get FV3 RESTART files, this will be a lot simpler when using history files

        # aerosol DA only needs core/tracer
        bkglist = []
        basename = f'{cdate_fv3}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(self.config['DATA'], 'bkg', basename)])
        for t in range(1, self.config.ntiles + 1):
            basename = f'{cdate_fv3}.fv_core.res.tile{t}.nc'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(self.config['DATA'], 'bkg', basename)])
            basename = f'{cdate_fv3}.fv_tracer.res.tile{t}.nc'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(self.config['DATA'], 'bkg', basename)])
        bkg_dict = {
            'mkdir': [os.path.join(self.config['DATA'], 'bkg')],
            'copy': bkglist,
        }
        return bkg_dict

    def get_berror_dict(self):
        """
        Return FileHandler configuration for berror
        """

        # aerosol static-B needs nicas, cor_rh, cor_rv and stddev files.
        b_dir = self.config['BERROR_DATA_DIR']
        b_datestr = self.config['BERROR_DATE']
        berror_list = []
        berror_list.append([
            os.path.join(b_dir, b_datestr + '.cor_rh.coupler.res'),
            os.path.join(self.config['DATA'], 'berror', b_datestr + '.cor_rh.coupler.res')
        ])
        berror_list.append([
            os.path.join(b_dir, b_datestr + '.cor_rv.coupler.res'),
            os.path.join(self.config['DATA'], 'berror', b_datestr + '.cor_rv.coupler.res')
        ])
        berror_list.append([
            os.path.join(b_dir, b_datestr + '.stddev.coupler.res'),
            os.path.join(self.config['DATA'], 'berror', b_datestr + '.stddev.coupler.res')
        ])
        for t in range(1, self.config.ntiles + 1):
            berror_list.append([
                os.path.join(b_dir, f'{b_datestr}.cor_rh.fv_tracer.res.tile{t}.nc'),
                os.path.join(self.config['DATA'], 'berror', f'{b_datestr}.cor_rh.fv_tracer.res.tile{t}.nc')
            ])
            berror_list.append([
                os.path.join(b_dir, f'{b_datestr}.cor_rv.fv_tracer.res.tile{t}.nc'),
                os.path.join(self.config['DATA'], 'berror', f'{b_datestr}.cor_rv.fv_tracer.res.tile{t}.nc')
            ])
            berror_list.append([
                os.path.join(b_dir, f'{b_datestr}.stddev.fv_tracer.res.tile{t}.nc'),
                os.path.join(self.config['DATA'], 'berror', f'{b_datestr}.stddev.fv_tracer.res.tile{t}.nc')
            ])
        nproc = self.config.ntiles * int(self.config['layout_x']) * int(self.config['layout_y'])
        for t in range(1, nproc + 1):
            berror_list.append([
                os.path.join(b_dir, f'nicas_aero_nicas_local_{nproc:06}-{t:06}.nc'),
                os.path.join(self.config['DATA'], 'berror', f'nicas_aero_nicas_local_{nproc:06}-{t:06}.nc')
            ])
        berror_dict = {
            'mkdir': [os.path.join(self.config['DATA'], 'berror')],
            'copy': berror_list,
        }
        return berror_dict
