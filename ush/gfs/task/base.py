import os

class Task():
    """
    Parent class of GFS workflow tasks
    """
    def __init__(self, config):
        self.config = config
        self.datadir = config['DATA']
        self.nlevs = config['NLEVS']
        self.nlayers = int(self.nlevs) - 1
        if not os.path.exists(self.datadir):
            os.makedirs(self.datadir)

    def initialize(self):
        print('initialized')

    def execute(self):
        print('running')

    def finalize(self):
        print('done')
