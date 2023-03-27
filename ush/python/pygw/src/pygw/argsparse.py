"""
Module
------

    argsparse.py

Description
-----------

    This module contains classes and methods to parse and
    appropriately format argumenst passed to a caller script.

Classes
-------

    ArgsParser()

        This is the base-class object for all command line argument(s)
        parsing.

Author(s)
---------

    Henry R. Winterbottom; 27 March 2023

History
-------

    2023-03-27: Henry Winterbottom -- Initial implementation.

"""

# ----

# pylint: disable=too-many-branches
# pylint: disable=unnecessary-list-index-lookup

# ----

from argparse import ArgumentParser
from dataclasses import dataclass
from typing import Union

from pygw.attrdict import AttrDict
from pygw.logger import Logger

# ----

logger = Logger(level="info", colored_log="True")

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"

# ----


@dataclass
class ArgsParse:
    """
    Description
    -----------

    This is the base-class object for all command line argument(s)
    parsing.

    Returns
    -------

    args_obj: object

        A Python object containing the command-line arguments; if no
        command-line arguments have been passed to the caller script,
        the returned value is NoneType.

    """

    def __check__(self, args_obj: AttrDict) -> AttrDict:
        """
        Description
        -----------

        This method checks that the argument values are formatted
        correctly; this method is necessary due to the Python
        interpretter assuming that all arguments passed via the Python
        `sys` module are formatted as strings.

        Parameters
        ----------

        in_dict: object

            A Python object containing the argument values to be
            evaluated and formatted as neccessary.

        Returns
        -------

        out_dict: object

            A Python object containing the formatted argument values.

        """

        # Define local function to sort and format the input Python
        # dictionary upon entry.
        def sorted_by_keys(in_dict):

            out_dict = AttrDict()
            for (key, value) in sorted(in_dict.items(), key=lambda key: key):

                if isinstance(value, dict):
                    setattr(out_dict, key, sorted_by_keys(value))

                else:
                    test_value = value

                    # Check if the key and value pair is a boolean
                    # type argument and proceed accordingly.
                    if isinstance(test_value, bool):
                        if test_value:
                            value = True

                        if not test_value:
                            value = False

                    # Check if the key and value pair is a string type
                    # argument and proceed accordingly.
                    if isinstance(test_value, str):
                        try:
                            dummy = float(test_value)
                            if "." in test_value:
                                value = float(test_value)
                            else:
                                value = int(test_value)

                        except ValueError:
                            if test_value.lower() == "none":
                                value = None

                            elif test_value.lower() == "true":
                                value = True

                            elif test_value.lower() == "false":
                                value = False

                            else:
                                value = str(test_value)

                    # Update the output dictionary key and value pair.
                    setattr(out_dict, key, value)

            return out_dict

        # Define the formatted output dictionary.
        out_dict = sorted_by_keys(in_dict=args_obj)

        return out_dict

    def run(self) -> Union[None, AttrDict]:
        """
        Description
        -----------

        This method collects the arguments passed from the command
        line to the respective caller script and builds a Python
        object containing the respective arguments.

        """

        # Collect the command-line argument key and value pairs.
        (_, args) = ArgumentParser().parse_known_args()
        (arg_keys, arg_values) = ([item.strip("-")
                                   for item in args[::2]], args[1::2])

        # Build/define the Python object to contain the argument(s)
        # attributes; proceed accordingly.
        if len(arg_keys) == 0:

            # If the number of command-line arguments is 0, set the
            # base-class attribute to NoneType.
            msg = (
                "No command line arguments have been specified; "
                "setting arguments object to NoneType."
            )
            logger.warning(msg=msg)

            args_obj = None

        else:

            # If the number of command-line arguments is non-zero,
            # build the command line arguments object.
            args_obj = AttrDict()

            for (idx, _) in enumerate(arg_keys):

                # Define the argument attributes.
                msg = f"Command line argument {arg_keys[idx]} is {arg_values[idx]}"
                logger.info(msg=msg)

                setattr(args_obj, arg_keys[idx], arg_values[idx])

            # Check that the command line arguments have the
            # appropriate format upon exit.
            args_obj = self.__check__(args_obj=args_obj)

        return args_obj
