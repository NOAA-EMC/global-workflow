#!/usr/bin/env python3

import os
import argparse
from wxflow import Configuration


def get_config_vars(var_names, config_path):
    """
    GET_CONFIG_VARS Get configuration variables from a config file or directory.
    Parameters:
    var_names (list of str): The names of the configuration variables to retrieve.
    config_path (str): The path to the configuration file or directory.
    Returns:
    list of str: The values of the specified configuration variables.
    """
    if os.path.isfile(config_path):
        config_dir = os.path.dirname(config_path)
        config_file = os.path.basename(config_path)
    elif os.path.isdir(config_path):
        config_dir = config_path
        config_file = 'config.base'
    config = Configuration(config_dir)
    config_data = config.parse_config(config_file)
    return [config_data[var_name] for var_name in var_names]


if __name__ == "__main__":
    """
    Main entry point for the script.
    Parses command-line arguments and retrieves the specified configuration variables.
    """
    parser = argparse.ArgumentParser(description="Get configuration variables from a config file or directory.")
    parser.add_argument("var_names", nargs='+', help="The names of the configuration variables to retrieve.")
    parser.add_argument("config_path", help="The path to the configuration file or directory.")

    args = parser.parse_args()

    var_names = args.var_names
    config_path = args.config_path

    values = get_config_vars(var_names, config_path)
    print(" ".join(values))
