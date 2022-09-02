from gfs.task.base import Task
import gfs.analysis.aerosol


class Analysis(Task):
    """
    Parent class for global analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)
        # for now config is assumed to be os.environ
        self.fv3jedi_fix = config['FV3JEDI_FIX']

    def initialize(self):
        super().initialize()

    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()

class GlobalAerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    def __init__(self, config):
        super().__init__(config)

    def initialize(self):
        super().initialize()
        from gfs.analysis.aerosol import  
        from gfs.stage import fix, observations, backgrounds

    def execute(self):
        super().execute()

    def finalize(self):
        super().finalize()
