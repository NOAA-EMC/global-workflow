#!/usr/bin/env python3

'''
    PROGRAM:
        Create the ecFlow workflow
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. configuration file config.json
        2. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail
    OUTPUT:
        1. ecFlow definition file
        2. ecFlow scripts for post processing
'''
import yaml
import collections.abc
import os
import json
import re
import datetime

try:
    from ecflow import Defs
except ImportError as err:
    raiseException("Error: Could not import ecflow module: %s" % err)

from ecflow_setup.ecflow_definitions import Ecflowsuite

class Ecflowsetup:

    def __init__(self,args,env_configs):
        # Setup the base variables
        self.args = args
        self.env_configs = env_configs
        self.DEFS = Defs()

        # Load in the ecflow configurations
        base_ecflowconfig = load_ecflow_config('%s' % args.ecflow_config )
        self.ecfconf = update_ecflow_config(base_ecflowconfig,env_configs)

    def generate_workflow(self):
        # Add in extern headers
        self.process_definition_header()

        # Process each of the suites
        for suite in self.ecfconf['suites'].keys():
            new_suite = Ecflowsuite(suite,self.ecfconf,self.env_configs)
            new_suite.add_suite_edits()
            self.setup_datastructure(new_suite)
            self.DEFS += new_suite.get_suite()

    def raiseException(self,e):
        print(e)
        sys.exit(1)


    def save(self):
        print("Saving definition File")

    def print(self):
        print(self.DEFS.check())
        print(self.DEFS)

    def setup_datastructure(self,suite):
        ecfhome = "%s/scripts" % self.env_configs['base']['ECFgfs']

        def find(name, path):
            for root, dirs, files in os.walk(path):
                if name in files:
                    return root.replace(ecfhome,'')

        # Setup the family and task structure
        for job in self.ecfconf['jobs']:
            if re.search(r"\{.*\}",job):
                loop_check = re.search("\{(.*)\}",job).group(1).strip()
                if isinstance(loop_check,str) and 'env.' in loop_check:
                    if loop_check.split('.')[1] in os.environ:
                        loop = os.environ[loop_check.split('.')[1]]
                    else:
                        loop = self.env_configs['base'][loop_check.split('.')[1]]
                elif loop_check.isdigit():
                    loop = int(loop_check)
                else:
                    loop = 0
                for counter in range(loop):
                    job_base_name = re.search("(.*)\{.*\}",job).group(1).strip()
                    job_name = "%s%03d" %( job_base_name, counter )
                    familypath = find("%s.ecf" % job_name, "%s/%s" %(ecfhome,suite.get_suite().name()))
                    families = familypath.split('/')[2:]
                    for family in families:
                        parents = ('_'.join(families[:families.index(family)]))
                        suite.add_family(family,parents)
                    suite.add_task(job_name,'_'.join(familypath.split('/')[2:]))
            else:
                familypath = find("%s.ecf" % job, "%s/%s" %(ecfhome,suite.get_suite().name()))
                families = familypath.split('/')[2:]
                for family in families:
                    parents = ('_'.join(families[:families.index(family)]))
                    suite.add_family(family,parents)
                suite.add_task(job,'_'.join(familypath.split('/')[2:]))

        # Add in triggers. We have to run through this twice because in creating
        # the references, some tasks might not be setup so we need them all setup
        # before adding in triggers or events.
        for job in self.ecfconf['jobs']:
            if re.search("\{.*\}",job):
                loop_check = re.search("\{(.*)\}",job).group(1).strip()
                if isinstance(loop_check,str) and 'env.' in loop_check:
                    if loop_check.split('.')[1] in os.environ:
                        loop = os.environ[loop_check.split('.')[1]]
                    else:
                        loop = self.env_configs['base'][loop_check.split('.')[1]]
                elif loop_check.isdigit():
                    loop = int(loop_check)
                else:
                    loop = 1
                for counter in range(loop):
                    if isinstance(self.ecfconf['jobs'][job],dict):
                        job_base_name = re.search("(.*)\{.*\}",job).group(1).strip()
                        job_name = "%s%03d" %( job_base_name, counter )
                        if 'triggers' in self.ecfconf['jobs'][job].keys():
                            suite.add_trigger(job_name,self.ecfconf['jobs'][job]['triggers'],counter)
                        if 'events' in self.ecfconf['jobs'][job].keys():
                            suite.add_event(job_name,self.ecfconf['jobs'][job]['events'])
                        if 'edits' in self.ecfconf['jobs'][job].keys():
                            suite.add_edit(job_name,self.ecfconf['jobs'][job]['edits'],counter)

            else:
                if isinstance(self.ecfconf['jobs'][job],dict):
                    if 'triggers' in self.ecfconf['jobs'][job].keys():
                        suite.add_trigger(job,self.ecfconf['jobs'][job]['triggers'])
                    if 'events' in self.ecfconf['jobs'][job].keys():
                        suite.add_event(job,self.ecfconf['jobs'][job]['events'])
                    if 'edits' in self.ecfconf['jobs'][job].keys():
                        suite.add_edit(job,self.ecfconf['jobs'][job]['edits'])

    def process_definition_header(self):
        if 'externs' in self.ecfconf.keys():
            for extern in self.ecfconf['externs']:
                self.DEFS.add_extern(extern)

def load_ecflow_config(configfile):
    with open(configfile, 'r') as file:
        base_config = yaml.safe_load(file)
    return base_config

def update_ecflow_config(configfile,envconfig):

    def runupdate(nested_dict, value):
        for k, v in nested_dict.items():
            if isinstance(v,str) and value in v:
                lookup = v.split('.')
                variable_lookup = re.findall("[\dA-Za-z_]*", lookup[1])[0]
                if variable_lookup in os.environ:
                    if isinstance(os.environ[variable_lookup],datetime.datetime):
                        nested_dict[k] = os.environ[variable_lookup].strftime("%Y%m%d%H")
                    else:
                        nested_dict[k] = os.environ[variable_lookup]

                else:
                    if isinstance(envconfig['base'][variable_lookup],datetime.datetime):
                        envvalue = envconfig['base'][variable_lookup].strftime("%Y%m%d%H")
                    else:
                        envvalue = envconfig['base'][variable_lookup]
                    nested_dict[k] = envvalue
            elif isinstance(v, collections.abc.Mapping):
                nested_dict[k] = runupdate(v, value)
        return nested_dict

    config = runupdate(configfile,'env.')
    return config
