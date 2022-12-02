from gfs.task.base import Task
import datetime as dt
import logging
import os
import shutil

crtmver = '2.3.0'

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
        self.berror_dir = os.path.join(config['FV3JEDI_FIX'],
                                       config['STATICB_TYPE'],
                                       config.get('CASE_BERROR', config.get('CASE_ANL', config['CASE'])))
        self.berror_yaml = config['BERROR_YAML']
        self.berror_yaml_dir = config['BERROR_YAML_DIR']
        self.component = config['COMPONENT']
        self.cdump = config['CDUMP']

    def initialize(self):
        super().initialize()
        obs_dict = self.get_obs_dict()
        self.stage_obs(obs_dict)

    def execute(self):
        """
        Note this is generally unused and a shell script
        """
        super().execute()

    def finalize(self):
        super().finalize()

    def get_obs_dict(self):
        """
        Get a dictionary of observation files to copy/use
        based on the specified configuration
        """
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
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            obs_dict[os.path.join(os.environ['COMIN_OBS'], basename)] = obfile
        return obs_dict

    def get_staticb_dict(self):
        """
        get dictionary of staticb files to copy/use
        based on the specified configuration.
        """
        with open(self.berror_yaml, 'r') as yamlopen:
            berror_yaml_dict = yaml.safe_load(yamlopen)

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
        logging.info('Staging staticb files')
        self.stage(filedict)
        logging.info('Finished staging staticb files')

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
        output_yaml = f'{self.taskname}_{self.cdate}.yaml'
        output_yaml_path = os.path.join(self.datadir, output_yaml)
        template = self.yamltemplate
        logging.info(f'Generating analysis YAML using {template}')
        full_config = copy.deepcopy(self.config)
        full_config.update(extra_config)
        ufsda.yamltools.genYAML(full_config, template=template, output=output_yaml_path)
        logging.info(f'Wrote YAML file to {output_yaml_path}')
