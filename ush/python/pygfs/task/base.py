import os

class Task():
    """
    Parent class of GFS workflow tasks
    """
    def __init__(self, config):
        self.config = config
        self.cdate = config['CDATE']
        self.datadir = config['DATA']
        self.nlevs = config['LEVS']
        self.nlayers = int(self.nlevs) - 1
        if not os.path.exists(self.datadir):
            os.makedirs(self.datadir)

    def initialize(self):
        pass

    def execute(self):
        pass

    def finalize(self):
        pass
