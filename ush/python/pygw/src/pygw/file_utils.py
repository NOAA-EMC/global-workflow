from logging import getLogger

from .fsutils import cp, mkdir

__all__ = ['FileHandler']

logger = getLogger(__name__.split('.')[-1])


class FileHandler:
    """Class to manipulate files in bulk for a given configuration

    Parameters
    ----------
    config : dict
          A dictionary containing the "action" and the "act" in the form of a list

    NOTE
    ----
    "action" can be one of mkdir", "copy", etc.
    Corresponding "act" would be ['dir1', 'dir2'], [['src1', 'dest1'], ['src2', 'dest2']]

    Attributes
    ----------
    config : dict
            Dictionary of files to manipulate
    """

    def __init__(self, config):

        self.config = config

    def sync(self):
        """
        Method to execute bulk actions on files described in the configuration
        """
        sync_factory = {
            'copy': self._copy_files,
            'mkdir': self._make_dirs,
        }
        # loop through the configuration keys
        for action, files in self.config.items():
            sync_factory[action](files)

    @staticmethod
    def _copy_files(filelist):
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
                raise Exception(
                    f"List must be of the form ['src', 'dest'], not {sublist}")
            src = sublist[0]
            dest = sublist[1]
            cp(src, dest)
            logger.info(f'Copied {src} to {dest}')

    @staticmethod
    def _make_dirs(dirlist):
        """Function to make all directories specified in the list

        Parameters
        ----------
        dirlist : list
                List of directories to create
        """
        for dd in dirlist:
            mkdir(dd)
            logger.info(f'Created {dd}')
