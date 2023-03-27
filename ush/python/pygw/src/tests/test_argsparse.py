"""
Module
------

    test_argsparse.py

Description
-----------

    The following unit tests contain functions to evaluate
    command-line argument applications.

Functions
---------

    main()

        This is the dummy function to collect and return command line
        arguments via a Python AttrDict object.

    test_argsparse_args()

        This function tests the ArgsParse.run() method for command
        line argument instances.

Author(s)
---------

    Henry R. Winterbottom; 27 March 2023

History
-------

    2023-03-27: Henry Winterbottom -- Initial implementation.

"""

# ----

from unittest.mock import patch

from pygw.argsparse import ArgsParse

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"

# ----


def main() -> None:
    """
    Description
    -----------

    This is the dummy function to collect and return command line
    arguments via a Python AttrDict object.

    """

    # Parse (any) command line argument values.
    args_obj = ArgsParse().run()

    return args_obj


# ----


def test_argsparse_args() -> None:
    """
    Description
    -----------

    This function tests the ArgsParse.run() method for command line
    argument instances.

    """

    # Test argument parser with a single argument.
    with patch("sys.argv", ["main", "--filepath", "/path/to/file"]):
        args_obj = main()

        assert args_obj.filepath == "/path/to/file"
        assert isinstance(args_obj.filepath, str)
        assert not isinstance(args_obj.filepath, int)

    # Test the argument parser with multiple arguments; note that the
    # Python interpretor assumes that all attributes collected via
    # the sys instance are formatted as strings.
    with patch("sys.argv", ["main", "--filepath", "/path/to/file", "--intarg", "2"]):
        args_obj = main()

        assert args_obj.filepath == "/path/to/file"
        assert args_obj.intarg == 2

    # Test the argument parser with no arguments.
    with patch("sys.argv", ["main"]):
        args_obj = main()

        assert args_obj is None
