from pygw.task import Task
from pygw.attrdict import AttrDict
from pygw.template import Template, TemplateConstants
from pygw.yaml_file import YAMLFile
from pygw.fileutils import FileHandler
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
        # all analyses need to stage observations
        obs_dict = self.get_obs_dict()
        FileHandler(obs_dict).sync()

    def configure(self):
        """Compute additional variables and add them to the root configuration"""
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
        """Compile a dictionary of observation files to copy
        
        This method uses the OBS_LIST configuration variable to generate a dictionary
        from a list of YAML files that specify what observation files are to be
        copied to the run directory from the observation input directory

        returns `obs_dict` - a dictionary for FileHandler 
        """
        obs_list_config = YAMLFile(path=self.config['OBS_LIST'])
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(self.config['COMIN_OBS'], basename), obfile])
        obs_dict = {
            'mkdir': [os.path.join(self.config['DATA'], 'obs')],
            'copy': copylist
        }
        return obs_dict
