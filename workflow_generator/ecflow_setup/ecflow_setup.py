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

        self.ecfhome = env_configs['base']['ECFgfs']

        if 'scriptrepo' in self.ecfconf.keys():
            self.env_configs['base']['scriptrepo'] = self.ecfconf['scriptrepo']
        elif 'scriptrepo' not in self.env_configs['base'].keys():
            self.env_configs['base']['scriptrepo'] = "%s/scripts" % self.ecfhome
        self.scriptrepo = self.env_configs['base']['scriptrepo']

        # Setup the default edits from the environment
        self.environment_edits = [
            'ACCOUNT',
            'queue',
            'machine',
            'RUN_ENVIR',
        ]

    def generate_workflow(self):
        # Add in extern headers
        self.process_definition_header()

        # Process each of the suites
        for suite in self.ecfconf['suites'].keys():
            if suite not in {'externs','edits'}:
                new_suite = Ecflowsuite(suite,self.env_configs['base']['ECFgfs'])
                self.add_environment_edits(new_suite)
                self.add_suite_edits(new_suite)
                self.add_families(new_suite,self.ecfconf['suites'][suite]['nodes'])
                self.add_tasks_and_edits(new_suite,self.ecfconf['suites'][suite]['nodes'])
                self.add_triggers_and_events(new_suite,self.ecfconf['suites'][suite]['nodes'])
                self.DEFS += new_suite.get_suite()


    def raiseException(self,e):
        print(e)
        sys.exit(1)

    def save(self):
        print("Saving definition File")

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

    def add_suite_edits(self,suite):

        # Baseline edits
        if 'edits' in self.ecfconf['suites'].keys():
            suite.add_edit(self.ecfconf['suites']['edits'])

        # Setup sutite specific edits
        suite_name = suite.get_suite_name()
        if (
            type(self.ecfconf['suites'][suite_name]) is dict and
            'edits' in self.ecfconf['suites'][suite_name].keys()
        ):
            suite.add_edit(self.ecfconf['suites'][suite_name]['edits'])

    def process_definition_header(self):
        if 'externs' in self.ecfconf.keys():
            for extern in self.ecfconf['externs']:
                self.DEFS.add_extern(extern)

    def invalid_node():
        raise Exception("Node is not definied")

    def add_families(self,suite,nodes,parents=None):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item not in {'edits','tasks'} ):
                suite.add_family(item,parents)
                if parents:
                    family_path = "%s_%s" %( parents, item)
                else:
                    family_path = item
                if (isinstance(nodes[item],dict) and
                    'edits' in nodes[item].keys() and
                    isinstance(nodes[item]['edits'],dict) ):
                    suite.add_edit(nodes[item]['edits'],family_path)
                self.add_families(suite,nodes[item],family_path)

    def add_tasks_and_edits(self,suite,nodes,parents=None):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item == 'tasks' ):
                for task in nodes['tasks'].keys():
                    if (isinstance(nodes['tasks'][task],dict) and
                        'template' in nodes['tasks'][task].keys() ):
                        task_template = nodes['tasks'][task]['template']
                    else:
                        task_template = None
                    updated_task = find_env_param(task,'env.',self.env_configs)
                    suite.add_task(updated_task,parents,self.scriptrepo,task_template)
                    if (isinstance(nodes['tasks'][task],dict) and
                        'edits' in nodes['tasks'][task].keys() and
                        isinstance(nodes['tasks'][task]['edits'],dict) ):
                        suite.add_task_edits(updated_task,nodes['tasks'][task]['edits'])
            elif ( isinstance(nodes[item],dict) and
                item != 'edits' ):
                if parents:
                    family_path = "%s_%s" %( parents, item)
                else:
                    family_path = item
                self.add_tasks_and_edits(suite,nodes[item],family_path)

    def add_triggers_and_events(self,suite,nodes):
        for item in nodes.keys():
            if ( isinstance(nodes[item],dict) and
                item == 'tasks' ):
                for task in nodes['tasks'].keys():
                    updated_task = find_env_param(task,'env.',self.env_configs)
                    if (isinstance(nodes['tasks'][task],dict) and
                        'events' in nodes['tasks'][task].keys()):
                        suite.add_task_events(updated_task,nodes['tasks'][task]['events'])
                    if (isinstance(nodes['tasks'][task],dict) and
                        'triggers' in nodes['tasks'][task].keys()):
                        suite.add_task_triggers(updated_task,nodes['tasks'][task]['triggers'])
            elif isinstance(nodes[item],dict):
                self.add_triggers_and_events(suite,nodes[item])

def load_ecflow_config(configfile):
    with open(configfile, 'r') as file:
        base_config = yaml.safe_load(file)
    return base_config

def find_env_param(node,value,envconfig):
    new_node = node
    if value in node:
        variable_lookup = re.search(".*%s([\dA-Za-z_]*)" % value,node).group(1).strip()
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
        new_node = "%s %s %s" %(search_key.group(1),new_variable,search_key.group(3))
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
