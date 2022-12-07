from pygw.task import Task
from pygw.attrdict import AttrDict
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
import datetime as dt
import logging
import os

crtmver = '2.3.0'

class Analysis(Task):
    """Parent class for GDAS tasks

    The Analysis class is the parent class for all
    Global Data Assimilation System (GDAS) tasks
    directly related to peforming an analysis
    """

    def __init__(self, config):
        super().__init__(config)

    def initialize(self):
        super().initialize()
        obs_dict = self.get_obs_dict()
        print(obs_dict)

    def configure(self):
        super().configure()

    def execute(self):
        """
        Note this is generally unused and instead a shell script
        """
        super().execute()

    def finalize(self):
        super().finalize()

    def clean(self):
        super().clean()

    def get_obs_dict(self):
        """Compile a dictionary of observation files to copy"""
        obs_list_config = YAMLFile(path=self.config['OBS_LIST'])
        # get observers from master dictionary
        observers = obs_list_config['observers']
        obs_dict = {}
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            obs_dict[os.path.join(self.config['COMIN_OBS'], basename)] = obfile
        return obs_dict

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
