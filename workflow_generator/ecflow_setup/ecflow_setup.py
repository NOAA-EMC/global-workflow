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
    print(f"Error: Could not import ecflow module: {err}")
    sys.exit(1)

from ecflow_setup.ecflow_definitions import Ecflowsuite

class Ecflowsetup:

    def __init__(self,args,env_configs):
        # Setup the base variables
        self.args = args
        self.env_configs = env_configs
        self.suite_array = {}
        self.DEFS = Defs()

        # Load in the ecflow configurations
        base_ecflowconfig = load_ecflow_config(f'{args.ecflow_config}')
        self.ecfconf = update_ecflow_config(base_ecflowconfig,env_configs)

        self.ecfhome = env_configs['base']['ECFgfs']

        if 'scriptrepo' in self.ecfconf.keys():
            self.env_configs['base']['scriptrepo'] = self.ecfconf['scriptrepo']
        elif 'scriptrepo' not in self.env_configs['base'].keys():
            self.env_configs['base']['scriptrepo'] = f"{self.ecfhome}/scripts"
        self.scriptrepo = self.env_configs['base']['scriptrepo']

        # Setup the default edits from the environment
        self.environment_edits = [
            'ACCOUNT',
            'queue',
            'machine',
            'RUN_ENVIR',
        ]

    def generate_workflow(self):

        def get_suite_names(suitename):
            if not re.search(r".*\[.*\].*",suitename):
                return [f"{suitename}"]

            name_token = re.search("(.*)\[(.*)\](.*)",suitename)
            base = name_token.group(1).strip()
            list_items = name_token.group(2).strip().split(',')
            suffix = name_token.group(3).strip()
            name_array = []
            for item in list_items:
                name_array.append(f"{base}{item}{suffix}")
            return name_array


        # Add in extern headers
        self.process_definition_header()

        # Process each of the suites
        for suite in self.ecfconf['suites'].keys():
            if suite not in {'externs','edits'}:
                for suite_name in get_suite_names(suite):
                    if suite_name not in self.suite_array.keys():
                        new_suite = Ecflowsuite(suite_name,self.env_configs['base']['ECFgfs'])
                    else:
                        new_suite = self.suite_array[suite_name]
                    if new_suite.get_suite_name() not in self.suite_array.keys():
                        self.add_environment_edits(new_suite)
                    self.add_suite_edits(new_suite,self.ecfconf['suites'][suite])
                    if self.check_dict(self.ecfconf['suites'][suite],'nodes'):
                        self.add_families(new_suite,self.ecfconf['suites'][suite]['nodes'])
                        self.add_tasks_and_edits(new_suite,self.ecfconf['suites'][suite]['nodes'])
                    self.suite_array[new_suite.get_suite_name()] = new_suite

        for suite in self.ecfconf['suites'].keys():
            if suite not in {'externs','edits'}:
                for suite_name in get_suite_names(suite):
                    if self.check_dict(self.ecfconf['suites'][suite],'nodes'):
                        self.add_triggers_and_events(self.suite_array[suite_name],self.ecfconf['suites'][suite]['nodes'])

        for suite_name,suite in self.suite_array.items():
            self.DEFS += suite.get_suite()

    def raiseException(self,e):
        print(e)
        sys.exit(1)

    def save(self):
        print("Saving definition File")
        savedir = self.args.savedir
        defs_file = f"{savedir}/ecflow_suite.def"
        self.DEFS.save_as_defs(defs_file)

    def print(self):
        print(self.DEFS.check())
        print(self.DEFS)

    def add_environment_edits(self,suite):

        # Add in the ECF Home and ECF Include edits.
        suite.add_edit({'ECF_HOME':self.ecfhome, 'ECF_INCLUDE':self.ecfhome})

        # Add in the edits for the environment.
        for edit in self.environment_edits:
            edit = edit.upper()
            if (
                edit in self.env_configs['base'].keys() and
                self.env_configs['base'][edit] is not None
                ):
                edit_dict = {edit : self.env_configs['base'][edit]}
            elif (
                edit.lower() in self.env_configs['base'].keys() and
                self.env_configs['base'][edit.lower()] is not None
            ):
                edit_dict = {edit : self.env_configs['base'][edit.lower()]}
            suite.add_edit(edit_dict)

    def check_dict(self,node,key,key_is_dict=True):
        if isinstance(node,dict) and f'{key}' in node.keys():
            if key_is_dict and isinstance(node[f'{key}'],dict):
                    return True
            elif not key_is_dict:
                return True
        else:
            return False


    def add_suite_edits(self,suite,suite_dict):
        # Baseline edits
        if 'edits' in self.ecfconf['suites'].keys():
            suite.add_edit(self.ecfconf['suites']['edits'])

        # Setup sutite specific edits
        suite_name = suite.get_suite_name()
        if (
            type(suite_dict) is dict and
            'edits' in suite_dict.keys()
        ):
            suite.add_edit(suite_dict['edits'])

    def process_definition_header(self):
        if 'externs' in self.ecfconf.keys():
            for extern in self.ecfconf['externs']:
                self.DEFS.add_extern(extern)

    def add_families(self,suite,nodes,parents=None):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item not in {'edits','tasks'} ):
                suite.add_family(item,parents)
                if parents:
                    family_path = f"{parents}>{item}"
                else:
                    family_path = item
                if self.check_dict(nodes[item],'edits'):
                    suite.add_edit(nodes[item]['edits'],family_path)
                if self.check_dict(nodes[item],'repeat',False):
                    suite.add_repeat(nodes[item]['repeat'],family_path)
                if self.check_dict(nodes[item],'defstatus',False):
                    suite.add_defstatus(nodes[item]['defstatus'],family_path)
                self.add_families(suite,nodes[item],family_path)

    def add_tasks_and_edits(self,suite,nodes,parents=None):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item == 'tasks' ):
                for task in nodes['tasks'].keys():
                    if self.check_dict(nodes['tasks'][task],'template',False):
                        task_template = nodes['tasks'][task]['template']
                    else:
                        task_template = None
                    updated_task = find_env_param(task,'env.',self.env_configs)
                    suite.add_task(updated_task,parents,self.scriptrepo,task_template)
                    if self.check_dict(nodes['tasks'][task],'edits'):
                        suite.add_task_edits(updated_task,nodes['tasks'][task]['edits'])
                    if self.check_dict(nodes['tasks'][task],'repeat',False):
                        suite.add_task_repeat(updated_task,nodes['tasks'][task]['repeat'])
                    if self.check_dict(nodes['tasks'][task],'defstatus',False):
                        suite.add_task_defstatus(updated_task,nodes['tasks'][task]['defstatus'])
            elif ( isinstance(nodes[item],dict) and
                item != 'edits' ):
                if parents:
                    family_path = f"{parents}>{item}"
                else:
                    family_path = item
                self.add_tasks_and_edits(suite,nodes[item],family_path)

    def add_triggers_and_events(self,suite,nodes):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item == 'tasks' ):
                for task in nodes['tasks'].keys():
                    updated_task = find_env_param(task,'env.',self.env_configs)
                    if self.check_dict(nodes['tasks'][task],'events',False):
                        suite.add_task_events(updated_task,nodes['tasks'][task]['events'])
                    if self.check_dict(nodes['tasks'][task],'triggers',False):
                        suite.add_task_triggers(updated_task,nodes['tasks'][task]['triggers'],self.suite_array)
            elif isinstance(nodes[item],dict):
                self.add_triggers_and_events(suite,nodes[item])

def load_ecflow_config(configfile):
    with open(configfile, 'r') as file:
        base_config = yaml.safe_load(file)
    return base_config

def find_env_param(node,value,envconfig):
    new_node = node
    if value in node:
        variable_lookup = re.search(f".*{value}([\dA-Za-z_]*)",node).group(1).strip()
        if variable_lookup in os.environ:
            if isinstance(os.environ[variable_lookup],datetime.datetime):
                new_variable = os.environ[variable_lookup].strftime("%Y%m%d%H")
            else:
                new_variable = os.environ[variable_lookup]
        else:
            if isinstance(envconfig['base'][variable_lookup],datetime.datetime):
                new_variable = envconfig['base'][variable_lookup].strftime("%Y%m%d%H")
            else:
                new_variable = envconfig['base'][variable_lookup]
        search_key = re.search(r"(.*)(env\.[\dA-Za-z_]*)(.*)",node)
        new_node = f"{search_key.group(1)} {new_variable} {search_key.group(3)}"
    return new_node

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
