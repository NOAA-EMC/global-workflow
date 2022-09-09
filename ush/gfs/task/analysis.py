from gfs.task.base import Task
import logging
import os
import shutil

class Analysis(Task):
    """
    Parent class for global analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)
        # for now config is assumed to be os.environ
        self.fv3jedi_fix = config['FV3JEDI_FIX']
        self.obs_list_yaml = config['OBS_LIST']
        self.obs_yaml_dir = config['OBS_YAML_DIR']
        self.component = config['COMPONENT']
        self.cdump = config['CDUMP']

    def initialize(self):
        super().initialize()
        obs_dict = self.get_obs_dict()
        self.stage_obs(obs_dict)

    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()

    def get_obs_dict(self):
        """
        Get a dictionary of observation files to copy/use
        based on the specified configuration
        """
        import ufsda # temporary until this is in workflow
        import yaml
        with open(self.obs_list_yaml, 'r') as yamlopen:
            obs_list_dict = yaml.safe_load(yamlopen)
        # need to replace vars
        obs_list_dict['OBS_YAML_DIR'] = self.obs_yaml_dir
        obs_list_dict = ufsda.yamltools.replace_vars(obs_list_dict)
        del obs_list_dict['OBS_YAML_DIR']
        # need a few extra variables defined
        obs_list_dict['OBS_DATE'] = self.cdate
        obs_list_dict['OBS_DIR'] = os.path.join(self.datadir, 'obs')
        obs_list_dict['OBS_PREFIX'] = os.environ['OPREFIX']
        obs_list_dict = ufsda.yamltools.parse_config(obs_list_dict)
        # get observers from master dictionary
        observers = obs_list_dict['observers']
        obs_dict = {}
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['obsfile']
            basename = os.path.basename(obfile)
            obs_dict[os.path.join(os.environ['COMIN_OBS'], basename)] = obfile
        return obs_dict

    def stage_obs(self, filedict):
        logging.info('Staging observations')
        self.stage(filedict, skip_missing=True)
        logging.info('Finished staging observations')

    def stage_bkg(self, filedict):
        logging.info('Staging model backgrounds')
        self.stage(filedict)
        logging.info('Finished staging backgrounds')

    def stage_fix(self, filedict):
        logging.info('Staging fix files')
        self.stage(filedict)
        logging.info('Finished staging fix files')

    def stage_berror(self, filedict):
        logging.info('Only using identity B for now... no staging performed')

    def stage(self, filedict, skip_missing=False):
        for src, dest in filedict.items():
            destdir = os.path.dirname(dest)
            if not os.path.exists(destdir):
                logging.info(f'{destdir} does not exist, creating directory.')
                os.makedirs(destdir)
            if os.path.exists(dest):
                os.remove(dest)
            if skip_missing:
                if not os.path.exists(src):
                    logging.warning(f'{src} does not exist. Will not copy.')
                    continue
            logging.info(f'Copying {src} to {dest}')
            shutil.copyfile(src, dest)

    def generate_yaml(self, extra_config={}):
        """
        Use existing tools to generate YAML from config and template
        """
        import copy
        import ufsda # temporary until this is in workflow
        output_yaml = f'{self.taskname}_{self.cdate}.yaml'
        output_yaml_path = os.path.join(self.datadir, output_yaml)
        template = self.yamltemplate
        logging.info(f'Generating analysis YAML using {template}')
        full_config = copy.deepcopy(self.config)
        full_config.update(extra_config)
        ufsda.yamltools.genYAML(full_config, template=template, output=output_yaml_path)
        logging.info(f'Wrote YAML file to {output_yaml_path}')


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)
        self.yamltemplate = config['AEROVARYAML']
        self.taskname = f'{self.cdump}aeroanl'

    def initialize(self):
        super().initialize()
        logging.info('Initializing global aerosol analysis')
        fix_dict = self.get_fix_file_dict()
        self.stage_fix(fix_dict)
        crtm_fix_dict = self.get_crtm_coeff_dict()
        self.stage_crtm(crtm_fix_dict)
        self.stage_berror({})
        bkg_dict = self.get_bkg_dict()
        self.stage_bkg(bkg_dict)
        yaml_config = {
            'BKG_DIR': 'bkg',
            'OBS_DIR': 'obs',
            'DIAG_DIR': 'diags',
            'CRTM_COEFF_DIR': 'crtm',
            'OBS_PREFIX': os.environ['OPREFIX'],
            'fv3jedi_staticb_dir': 'berror',
            'fv3jedi_fix_dir': 'fv3jedi',
            'fv3jedi_fieldmetadata_dir': 'fv3jedi',
            'OBS_DATE': os.environ['CDATE'],
            'ANL_DIR': 'anl',
            'INTERP_METHOD': 'barycentric',
            # for now making the below equal to eachother
            'AERO_WINDOW_LENGTH': '$(ATM_WINDOW_LENGTH)',
            'AERO_WINDOW_BEGIN': '$(ATM_WINDOW_BEGIN)',
            'window_begin': '$(ATM_WINDOW_BEGIN)',
            'layout_x': os.environ['layout_x'],
            'layout_y': os.environ['layout_y'],
            }
        self.generate_yaml(yaml_config)
        self.stage_exe()
        # need output dir for diags and anl
        newdirs = [
            os.path.join(self.datadir, 'anl'),
            os.path.join(self.datadir, 'diags'),
            ]
        for newdir in newdirs:
            if not os.path.exists(newdir):
                os.makedirs(newdir)
                logging.info(f'Creating directory {newdir}')


    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()

    def stage_crtm(self, filedict):
        logging.info('Staging CRTM coefficient files')
        self.stage(filedict)
        logging.info('Finished staging CRTM coefficient files')

    def stage_exe(self):
        logging.info('Staging FV3-JEDI executable')
        src = os.environ['JEDIVAREXE']
        dest = os.path.join(self.datadir, os.path.basename(src))
        if os.path.exists(dest):
            os.remove(dest)
        os.symlink(src, dest)
        logging.info(f'Linking {src} to {dest}')
        logging.info('Finished staging FV3-JEDI executable')

    def get_bkg_dict(self):
        """
        Return dict of src/dest pairs for model backgrounds
        """
        import datetime as dt
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006
        comin_ges = os.environ['COMIN_GES']
        # NOTE that while 'chem' is the $componenet, the aerosol fields are with the 'atmos' tracers
        comin_ges_atm = comin_ges.replace('chem', 'atmos')
        rst_dir = os.path.join(comin_ges_atm, 'RESTART') # for now, option later?
        # date variable string format
        cdate_fv3 = dt.datetime.strptime(self.cdate, '%Y%m%d%H').strftime('%Y%m%d.%H%M%S')
        # get FV3 RESTART files, this will be a lot simpler when using history files
        ntiles = 6 # global
        # aerosol DA only needs core/tracer
        bkg_dict = {}
        basename = f'{cdate_fv3}.coupler.res'
        bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
        for t in range(1,ntiles+1):
            basename = f'{cdate_fv3}.fv_core.res.tile{t}.nc'
            bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
            basename = f'{cdate_fv3}.fv_tracer.res.tile{t}.nc'
            bkg_dict[os.path.join(rst_dir, basename)] = os.path.join(self.datadir, 'bkg', basename)
        return bkg_dict

    def get_crtm_coeff_dict(self):
        coeff_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'AerosolCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'AerosolCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'CloudCoeff.bin'): os.path.join(self.datadir,
                                                           'crtm',
                                                           'CloudCoeff.bin'),
            # Note: Ideally we would only copy files for platforms used
            # but since it is just 2 VIIRS platforms, just copy them both regardless
            # We will fix this when there is a solution for ATM mode
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_npp.TauCoeff.bin'): os.path.join(self.datadir,
                                                                     'crtm',
                                                                     'v.viirs-m_npp.TauCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j1.SpcCoeff.bin'): os.path.join(self.datadir,
                                                                    'crtm',
                                                                    'v.viirs-m_j1.SpcCoeff.bin'),
            os.path.join(self.fv3jedi_fix,
                         'crtm', '2.3.0',
                         'v.viirs-m_j1.TauCoeff.bin'): os.path.join(self.datadir,
                                                                    'crtm',
                                                                    'v.viirs-m_j1.TauCoeff.bin'),
            # do we need other files? NPOESS/FASTEM6/USGS/etc.? Test to find out?
            }
        return coeff_file_dict


    def get_fix_file_dict(self):
        fix_file_dict = {
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         f'akbk{self.nlayers}.nc4'): os.path.join(self.datadir,
                                                                  'fv3jedi',
                                                                  'akbk.nc4'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'fmsmpp.nml'): os.path.join(self.datadir,
                                                     'fv3jedi',
                                                     'fmsmpp.nml'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fv3files',
                         'field_table_gfdl'): os.path.join(self.datadir,
                                                           'fv3jedi',
                                                           'field_table'),
            os.path.join(self.fv3jedi_fix,
                         'fv3jedi', 'fieldmetadata',
                         'gfs-aerosol.yaml'): os.path.join(self.datadir,
                                                           'fv3jedi',
                                                           'gfs-restart.yaml'),
            }
        return fix_file_dict
