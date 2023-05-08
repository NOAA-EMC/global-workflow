#!/usr/bin/env python3

import os
import sys
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import add_to_datetime, to_fv3time, to_timedelta, to_YMD, to_YYYY, to_DOY
from pygw.fsutils import rm_p, chdir
from pygw.yaml_file import YAMLFile, parse_yamltmpl, parse_j2yaml, save_as_yaml
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class LandAnalysis(Analysis):
    """
    Class for global land analysis tasks
    """
    @logit(logger, name="LandAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_enkf = int(self.config['CASE_ENKF'][1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config['assim_freq']}H") / 2)
        _fv3jedi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config['cyc']:02d}z.letkfoi.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_enkf + 1,
                'npy_anl': _res_enkf + 1,
                'npz_anl': self.config['LEVS'] - 1,
                'LAND_WINDOW_BEGIN': _window_begin,
                'LAND_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'OPREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.runtime_config.previous_cycle.hour:02d}z.",
                'fv3jedi_yaml': _fv3jedi_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global land analysis

        This method will initialize a global land analysis using JEDI.
        This includes:
        - staging FV3-JEDI fix files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - linking the JEDI executable (TODO make it copyable, requires JEDI fix)
        - creating output directories

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        super().initialize()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'land_jedi_fix.yaml')
        logger.debug(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate letkfoi YAML file
        logger.debug(f"Generate letkfoi YAML file: {self.task_config.fv3jedi_yaml}")
        letkfoi_yaml = parse_j2yaml(self.task_config['LANDVARYAML'], self.task_config)
        save_as_yaml(letkfoi_yaml, self.task_config.fv3jedi_yaml)
        logger.info(f"Wrote letkfoi YAML to: {self.task_config.fv3jedi_yaml}")

        # link executables to DATA/ directory
        exe_src = self.task_config.JEDIEXE
        logger.debug(f"Link executable {exe_src} to DATA/")
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)
        exe_src = self.task_config.JEDIINCEXE
        logger.debug(f"Link executable {exe_src} to DATA/")
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)
        exe_src = self.task_config.CALCFIMS
        logger.debug(f"Link executable {exe_src} to DATA/")
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config['DATA'], 'anl'),
            os.path.join(self.task_config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def prepare(self: Analysis) -> None:
        """Prepare the IMS data for a global land analysis

        This method will prepare IMS data for a global land analysis using JEDI.
        This includes:
        - staging model backgrounds
        - generating a card file for IMS processing
        - creating IMS snowdepth data in IODA format.

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """

        # ---- copy the restart files from bkg to obs
        logger.info('Copying restart files from bkg to obs')
        template = f'{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            sfcdata = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'bkg', sfcdata)
            dest = os.path.join(self.task_config.DATA, 'obs', sfcdata)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

        # ---- copy the IMS obs files from COM_OBS to DATA/obs
        logger.info('Copying the IMS obs files from COM_OBS to obs')
        yyyydoy = f"{to_YYYY(self.task_config.PDY)}" + f"{to_DOY(self.task_config.PDY)}"
        inclist = []
        obsdata1 = f"{self.task_config.RUN}" + ".t" + f"{self.task_config.cyc:02d}" + "z.ims" + f"{yyyydoy}" + "_4km_v1.3.nc"
        obsdata2 = "ims" + f"{yyyydoy}" + "_4km_v1.3.nc"
        src = os.path.join(self.task_config.COM_OBS, obsdata1)
        dest = os.path.join(self.task_config.DATA, 'obs', obsdata2)
        inclist.append([src, dest])
        obsindex1 = f"{self.task_config.RUN}" + ".t" + f"{self.task_config.cyc:02d}" + "z.IMS4km_to_FV3_mapping." + f"{self.task_config.CASE}" + "_oro_data.nc"
        obsindex2 = "IMS4km_to_FV3_mapping." + f"{self.task_config.CASE}" + "_oro_data.nc"
        src = os.path.join(self.task_config.COM_OBS, obsindex1)
        dest = os.path.join(self.task_config.DATA, 'obs', obsindex2)
        inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

        # write a fims.nml
        workdir = os.path.join(self.task_config.DATA, 'obs')
        os.chdir(workdir)
        fname = 'fims.nml'
        text = '&fIMS_nml\n'
        text += ' idim=' + self.task_config.CASE[1:] + ', jdim=' + self.task_config.CASE[1:] + ',\n'
        text += ' otype="' + self.task_config.OROGTYPE + '",\n'
        text += ' jdate=' + to_YYYY(self.task_config.PDY) + to_DOY(self.task_config.PDY) + ',\n'
        text += ' yyyymmddhh=' + to_YMD(self.task_config.PDY) + '.' + f"{self.task_config.cyc:02d}" + ',\n'
        text += ' imsformat=2,\n'
        text += ' imsversion=1.3,\n'
        text += ' IMS_OBS_PATH="' + os.path.join(self.task_config.DATA, 'obs/') + '",\n'
        text += ' IMS_IND_PATH="' + os.path.join(self.task_config.DATA, 'obs/') + '"\n'
        text += '/'
        with open(fname, 'w') as f:
            f.write(text)

        # execute calcfIMS.exe to calculate IMS snowdepth
        exec_cmd = Executable(self.task_config.APRUN_LANDANL)
        exec_name = os.path.join(self.task_config.DATA, 'calcfIMS.exe')
        exec_cmd.add_default_arg(exec_name)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

        # Execute imspy to create the IMS obs data in IODA format
        pdycyc = f"{to_YMD(self.task_config.PDY)}" + f"{self.task_config.cyc:02d}"
        ifile = 'IMSscf.' + f'{to_YMD(self.task_config.PDY)}' + '.' + f'{self.task_config.OROGTYPE}' + '.nc'
        ofile = f"{self.task_config.RUN}" + ".t" + f"{self.task_config.cyc:02d}" + "z.ims_snow_" + f"{pdycyc}" + ".nc4"

        # Reference the python script which does the actual work
        imspy = os.path.join(self.task_config.HOMEgfs, 'ush/imsfv3_scf2ioda.py')

        print(f"sys.path: {sys.path}")
        cmd = f"python {imspy} -i {ifile} -o {ofile}"
        print(f"cmd: {cmd}")
        os.system(cmd)

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global land analysis

        This method will finalize a global land analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the guess files to the ROTDIR
        - applying the increments to the original RESTART files
        - moving the increment files to the ROTDIR

        Please note that some of these steps are temporary and will be modified
        once the model is able to read land increments.

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        # ---- tar up diags
        # path of output tar statfile
        landstat = os.path.join(self.task_config.COM_LAND_ANALYSIS, f"{self.task_config.APREFIX}landstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(landstat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config.DATA, f"{self.task_config.CDUMP}.t{self.runtime_config.cyc:02d}z.letkfoi.yaml")
        dest = os.path.join(self.task_config.COM_LAND_ANALYSIS, f"{self.task_config.CDUMP}.t{self.runtime_config.cyc:02d}z.letkfoi.yaml")
        yaml_copy = {
            'mkdir': [self.task_config.COM_LAND_ANALYSIS],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # ---- NOTE below is 'temporary', eventually we will not be using FMS RESTART formatted files
        # ---- all of the rest of this method will need to be changed but requires model and JEDI changes
        # ---- copy RESTART sfc_data files for future reference
        template = '{}.sfc_data.tile{}.nc'.format(to_fv3time(self.task_config.current_cycle), '{tilenum}')
        bkglist = []
        for itile in range(1, self.task_config.ntiles + 1):
            sfcdata = template.format(tilenum=itile)
            src = os.path.join(self.task_config.COM_ATMOS_RESTART_PREV, sfcdata)
            dest = os.path.join(self.task_config.COM_LAND_ANALYSIS, f'landges.{sfcdata}')
            bkglist.append([src, dest])
        FileHandler({'copy': bkglist}).sync()

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # ---- move increments to ROTDIR
        logger.info('Moving increments to ROTDIR')
        template = f'{to_fv3time(self.task_config.current_cycle)}.xainc.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            sfcdata = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', sfcdata)
            dest = os.path.join(self.task_config.COM_LAND_ANALYSIS, sfcdata)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

        # ---- move analysis to ROTDIR/RESTART
        logger.info('Moving analysis to ROTDIR')
        template = f'{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            sfcdata = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', sfcdata)
            dest = os.path.join(self.task_config.COM_LAND_ANALYSIS, sfcdata)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self: Analysis) -> None:
        """This method adds increments to RESTART files to get an analysis
        NOTE this is only needed for now because the model cannot read land increments.
        This method will be assumed to be deprecated before this is implemented operationally
        """

        # ---- copy the restart files from bkg to anl
        logger.info('Copying restart files from bkg to anl')
        template = f'{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            sfcdata = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'bkg', sfcdata)
            dest = os.path.join(self.task_config.DATA, 'anl', sfcdata)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

        # write a apply_incr_nml
        workdir = os.path.join(self.task_config.DATA, 'anl')
        os.chdir(workdir)
        fname = 'apply_incr_nml'
        text = '&noahmp_snow\n'
        text += ' date_str=' + to_YMD(self.task_config.PDY) + '\n'
        text += ' hour_str=' + f"{self.task_config.cyc:02d}" + '\n'
        text += ' res=' + self.task_config.CASE[1:] + '\n'
        text += ' frac_grid=' + f"{self.task_config.FRACGRID}" + '\n'
        text += ' orog_path="' + self.task_config.OROGPATH + '"\n'
        text += ' otype="' + self.task_config.OROGTYPE + '"\n'
        text += '/'
        with open(fname, 'w') as f:
            f.write(text)

        # execute apply_incr.exe to add incr to analysis
        exec_cmd = Executable(self.task_config.APRUN_LANDANL)
        exec_name = os.path.join(self.task_config.DATA, 'apply_incr.exe')
        exec_cmd.add_default_arg(exec_name)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global land DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(task_config.COM_ATMOS_RESTART_PREV)  # for now, option later?
        run_dir = os.path.join(task_config['DATA'], 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # land DA needs coupler
        basename = f'{to_fv3time(task_config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # land DA only needs afc_data
        for ftype in ['sfc_data']:
            template = f'{to_fv3time(self.task_config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
            for itile in range(1, task_config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist,
        }
        return bkg_dict
