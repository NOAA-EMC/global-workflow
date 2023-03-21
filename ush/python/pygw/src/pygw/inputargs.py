from argparse import ArgumentParser
from dataclasses import dataclass

from pygw.exceptions import InputArgsException
from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit
from pygw.schema import validate


# ----

logger = Logger(level="info", colored_log=True)

__all__ = ["InputArgs"]

# ----


@dataclass
class InputArgs:
    """
    Description
    -----------

    This is the base-class object for all command line argument(s)
    parsing.

    """

    def run(self, eval_schema=False, cls_schema=None) -> object:
        """
        Description
        -----------

        This method collects the arguments passed from the command
        line to the respective caller script and builds a Python
        object containing the respective arguments.

        The command line arguments may be specified as follows.

        user@host:$ python <caller_script>.py --key value

        user@host:$ python <caller_script>.py -key value

        Here `key` is the argument name and `value` is the value
        attributed to the respective argument/key.

        Returns
        --------

        options_obj: object

            A Python object containing the command line argument key
            and value pairs.

        Raises
        ------

        InputArgsException:

            * raised if an exception is encountered while parsing the
              command line arguments.

        """

        # Collect the command-line argument key and value pairs.
        (_, args) = ArgumentParser().parse_known_args()
        (arg_keys, arg_values) = ([item.strip("-")
                                   for item in args[::2]], args[1::2])

        # Build the Python object containing the command line
        # arguments.
        options_obj = AttrDict()

        for (idx, _) in enumerate(arg_keys):

            # Define the Python object attributes.
            options_obj[arg_keys[idx]] = args_value[idx]

        if eval_schema:

            try:

                # Build the Python dictionary containing the command
                # line arguments.
                cls_opts = AttrDict()

                for option in vars(options_obj):
                    cls_opts[option] = options_obj[option]

            except Exception as errmsg:

                msg = f"Arguments validation failed with error {errmsg}. Aborting!!!"
                raise InputArgsException(msg=msg) from errmsg

        return options_obj
