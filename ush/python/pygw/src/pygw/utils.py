from .yaml_file import YAMLFile
from .fsutils import cp, mkdir

__all__ = ['FileHandler']


class FileHandler():
    """Class to manipulate files in bulk for a given configuration

    Parameters
    ----------
    path : str, optional
          Path to input YAML file containing files to manipulate
    config : AttrDict, optional
          Configuration AttrDict containing files to manipulate

    NOTE
    ----
    While both `path` and `config` are optional, exactly one must be specified
    or else an exception will be raised.

    Attributes
    ----------
    config : AttrDict
            Configuration of files to manipulate
    """

    def __init__(self, path=None, config=None):

        if path and config:
            raise Exception('Both path and config are defined. Only one can be.')
        elif path:
            # read in the YAML file for the configuration
            self.config = YAMLFile(path=path)
        elif config:
            self.config = config
        else:
            raise Exception('Neither path nor config are defined. One needs to be.')

    def sync(self):
        """
        Method to execute bulk actions on files described in the configuration
        """
        sync_factory = {
            'copy': copy_files,
            'mkdir': make_dirs,
        }
        # loop through the configuration keys
        for action, files in self.config.items():
            sync_factory[action](files)
    
def copy_files(filelist):
    """Function to copy all files specified in the list

    `filelist` should be in the form:
    - [src, dest]

    Parameters
    ----------
    filelist : list
              List of lists of [src, dest]
    """
    for sublist in filelist:
        if len(sublist) != 2:
            raise Exception(f"List must be of the form ['src', 'dest'], not {sublist}")
        src = sublist[0]
        dest = sublist[1]
        cp(src, dest)
        print(f'Copied {src} to {dest}') # TODO use logger

def make_dirs(dirlist):
    """Function to make all directories specified in the list

    Parameters
    ----------
    dirlist : list
              List of directories to create
    """
    for d in dirlist:
        mkdir(d)
        print(f'Created {d}') # TODO use logger