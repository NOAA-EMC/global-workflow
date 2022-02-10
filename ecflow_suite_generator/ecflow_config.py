#!/usr/bin/env python3

'''
    PROGRAM:
        ecFlow Configuration Loader
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. configuration file config.yml
    OUTPUT:
        None
'''

import yaml
import collections.abc
import os

def load_ecflow_config(configfile):
    with open(configfile, 'r') as file:
        base_config = yaml.safe_load(file)
    return base_config

def update_ecflow_config(configfile,envconfig):

    def runupdate(nested_dict, value):
        for k, v in nested_dict.items():
            if isinstance(v,str) and value in v:
                lookup = v.split('.')
                if lookup[1] in os.environ:
                    nested_dict[k] = os.environ[lookup[1]]
                else:
                    envvalue = envconfig['base'][lookup[1]]
                    nested_dict[k] = envvalue
            elif isinstance(v, collections.abc.Mapping):
                nested_dict[k] = runupdate(v, value)
        return nested_dict

    config = runupdate(configfile,'env.')
    return config

def print_config(config):
    print(config)
