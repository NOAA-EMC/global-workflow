import os
import logging
from functools import wraps
from wxflow import parse_j2yaml, Executable

logger = logging.getLogger(__name__.split('.')[-1])


def logit(logger):
    """Decorator to log method entry, exit, and exceptions."""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            logger.info(f"Starting {func.__name__}()")
            try:
                result = func(*args, **kwargs)
                logger.info(f"Completed {func.__name__}() successfully")
                return result
            except Exception as e:
                logger.error(f"Exception in {func.__name__}(): {e}", exc_info=True)
                raise
        return wrapper
    return decorator


class BufrFile:
    def __init__(self, bufr_file=None, work_dir=None, cycle=None):
        self.isready = False
        if bufr_file is None:
            return
        self.bufr_file = os.path.abspath(bufr_file)
        self.work_dir = work_dir or os.getcwd()
        self.cycle = cycle
        self.tmmark = "tm00"        # is obsolete
        self.split_files = []
        self.renamed_files = {}

        if not os.path.exists(self.bufr_file):
            raise FileNotFoundError(f"BUFR file not found: {self.bufr_file}")

    def is_ready(self):
        return self.isready

    def set_ready(self):
        self.isready - True

    @logit(logger)
    def split(self):
        """Split BUFR file by subset using external binary `split_by_subset`"""
        exec_cmd = Executable("split_by_subset")
        exec_cmd.add_default_arg(self.bufr_file)

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except Exception as e:
            raise RuntimeError(f"split_by_subset failed: {e}") from e

        self.split_files = [
            os.path.join(self.work_dir, f) for f in os.listdir(self.work_dir)
            if f.startswith("NC001")
        ]

        if not self.split_files:
            logger.warning("No split files found after running split_by_subset")

        return self.split_files

    @logit(logger)
    def rename(self, b2i_template, sfcshp_cycle_dict):
        """
        Rename split files based on a mapping defined in subclass.
        using the j2 template
        """
        if not hasattr(self, 'subset_mapping'):
            raise NotImplementedError("Subclass must define subset_mapping dict")

        if self.cycle is None:
            raise ValueError("Cycle must be set before renaming")

        for f in self.split_files:
            base = os.path.basename(f)
            obs_type = self.subset_mapping.get(base)
            if not obs_type:
                logger.warning(f"Unknown subset code '{base}', skipping rename.")
                continue

            # construct the new filename
            ob_cycle_dict = sfcshp_cycle_dict
            ob_cycle_dict['dump_tag'] = obs_type
            ob_cycle_config = parse_j2yaml(b2i_template, ob_cycle_dict)
            ob_filename = ob_cycle_config.local_dump_filename

            try:
                os.rename(f, ob_filename)
                logger.info(f"Renamed {base} → {ob_filename}")
                self.renamed_files[base] = ob_filename
            except OSError as e:
                logger.error(f"Failed to rename {base} to {ob_filename}: {e}")
                raise

        if not self.renamed_files:
            logger.warning("No files were renamed.")

        return self.renamed_files

    def has_provider_for(self, name):
        """
        Check if any value in subset_mapping matches the given name.

        Args:
            name (str): The observation type (e.g., "dbuoy")

        Returns:
            bool: True if mapping exists for this name, False otherwise.
        """
        if not hasattr(self, 'subset_mapping'):
            logger.warning("Subset mapping is not defined.")
            return False

        return name in self.subset_mapping.values()


class SfcShp(BufrFile):
    subset_mapping = {
        "NC001001": "ships",
        "NC001013": "shipsu",
        "NC001002": "dbuoy",
        "NC001003": "mbuoy",
        "NC001004": "lcman",
        "NC001007": "cstgd",
        "NC001101": "shipsb",
        "NC001102": "dbuoyb",
        "NC001103": "mbuoyb",
        "NC001104": "cmanb",
        "NC001113": "shipub"
    }
