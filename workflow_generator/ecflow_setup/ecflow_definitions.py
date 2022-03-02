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

import sys
import os
import json
import re

try:
    from ecflow import (Defs, Suite, Family, Variable, Task)
    import ecflow
except ImportError as err:
    raiseException("Error: Could not import ecflow module: %s" % err)

class Ecflowsuite:

    def __init__(self,ecfsuite,ecfconf,env_configs):
        self.suitename = ecfsuite
        self.ecfsuite = self.add_suite(ecfsuite)
        self.ecfconf = ecfconf
        self.env_configs = env_configs
        self.ecfhome = "%s/scripts" % env_configs['base']['ECFgfs']
        self.ecfnodes = {}
        #self.taskskip = arguments.taskskip if arguments.taskskip else []
        #self.nodeskip = arguments.nodeskip if arguments.nodeskip else []

    def raiseException(self,e):
        print(e)
        sys.exit(1)

    def invalid_node():
        raise Exception("Node is not definied")

    def add_suite(self, suite):
        new_suite = Suite("%s" % suite)
        return new_suite

    def get_suite(self):
        return self.ecfsuite

    def add_suite_edits(self):

        def verify_edit(edit):
            edit = edit.upper()
            if (
                edit in self.env_configs['base'].keys() and
                self.env_configs['base'][edit] is not None
                ):
                self.ecfsuite += ecflow.Edit({edit : self.env_configs['base'][edit]})
            elif (
                edit.lower() in self.env_configs['base'].keys() and
                self.env_configs['base'][edit.lower()] is not None
            ):
                self.ecfsuite += ecflow.Edit({edit : self.env_configs['base'][edit.lower()]})

        default_edits = [
            'ACCOUNT',
            'queue',
            'machine',
            'RUN_ENVIR',
        ]

        for edit in default_edits:
            verify_edit(edit)

        # Setup ECF Home
        self.ecfsuite += ecflow.Edit(ECF_HOME=self.ecfhome, ECF_INCLUDE=self.ecfhome)

        # Baseline edits
        if 'edits' in self.ecfconf['suites'].keys():
            edits = ecflow.Edit(self.ecfconf['suites']['edits'])
            self.ecfsuite += edits

        # Setup sutite specific edits
        if (
            type(self.ecfconf['suites'][self.suitename]) is dict and
            'edits' in self.ecfconf['suites'][self.suitename].keys()
        ):
            edits = ecflow.Edit(self.ecfconf['suites'][self.suitename]['edits'])
            self.ecfsuite += edits

    def add_family(self,family,parents=None):
        family_name = "%s_%s" % (parents, family) if parents else family

        # If the name already exists, the family already exists
        if family_name not in self.ecfnodes.keys():
            self.ecfnodes[family_name] = Family(family)

        if parents:
            self.ecfnodes[parents] += self.ecfnodes[family_name]
        else:
            self.ecfsuite += self.ecfnodes[family_name]

    def add_task(self,task,parents):
        if task not in self.ecfnodes.keys():
            self.ecfnodes[task] = Task(task)
            self.ecfnodes[parents] += self.ecfnodes[task]

    def add_trigger(self,task,trigger_dict, counter=None):
        trigger_statement = []
        for trigger in trigger_dict:
            if re.search("\{.*\}",trigger['task']) and counter is not None:
                trigger_task_base_name = re.search("(.*)\{.*\}",trigger['task']).group(1).strip()
                if re.search("initial_count",trigger['task']):
                    initial_count = re.search(".*\{(.*)\}",trigger['task']).group(1).strip().split(':')[1]
                    trigger_task = "%s%03d" %( trigger_task_base_name, counter+int(initial_count) )
                elif re.search("increment",trigger['task']):
                    increment = re.search(".*\{(.*)\}",trigger['task']).group(1).strip().split(':')[1]
                    trigger_task = "%s%03d" %( trigger_task_base_name, counter*int(increment) )
                else:
                    trigger_task = "%s%03d" %( trigger_task_base_name, counter )
            else:
                trigger_task = trigger['task']
            trigger_path = self.ecfnodes[trigger_task].get_abs_node_path()
            if 'state' in trigger.keys():
                trigger_statement.append("%s == %s" %( trigger_path, trigger['state']))
            elif 'event' in trigger.keys():
                if re.search("\{.*\}",trigger['event']) and counter is not None:
                    trigger_event_base_name = re.search("(.*)\{.*\}",trigger['event']).group(1).strip()
                    if re.search("initial_count",trigger['event']):
                        initial_count = re.search(".*\{(.*)\}",trigger['event']).group(1).strip().split(':')[1]
                        trigger_event = "%s%03d" %( trigger_event_base_name, counter+int(initial_count) )
                    elif re.search("increment",trigger['event']):
                        increment = re.search(".*\{(.*)\}",trigger['event']).group(1).strip().split(':')[1]
                        trigger_event = "%s%03d" %( trigger_event_base_name, counter*int(increment) )
                    else:
                        trigger_event = "%s%03d" %( trigger_event_base_name, counter )
                else:
                    trigger_event = trigger['event']
                trigger_task_event_name = "%s_%s" %( trigger_task, trigger_event )
                trigger_statement.append("%s:%s" %( trigger_path, trigger_event ))
            else:
                trigger_statement.append(self.ecfnodes[trigger_task])
        self.ecfnodes[task].add( ecflow.Trigger( trigger_statement ))

    def add_event(self,task,event_dict, counter=None):
        for event in event_dict:
            if re.search(r"\{.*\}",event):
                if counter is None:
                    loop_check = re.search("\{(.*)\}",event).group(1).strip()
                    if isinstance(loop_check,str) and 'env.' in loop_check:
                        if loop_check.split('.')[1] in os.environ:
                            loop = os.environ[loop_check.split('.')[1]]
                        else:
                            loop = self.env_configs['base'][loop_check.split('.')[1]]
                    elif loop_check.isdigit():
                        loop = int(loop_check)
                    else:
                        loop = loop_check
                    for counter in range(loop):
                        base_event_name = re.search("(.*)\{.*\}",event).group(1).strip()
                        event_name = "%s_%s" %( task, base_event_name )
                        ecfnodes_name = "%s%03d" % ( event_name, counter )
                        event_item = "%s%03d" %( base_event_name, counter)
                        self.ecfnodes[ecfnodes_name] = ecflow.Event(event_item)
                        self.ecfnodes[task].add(self.ecfnodes[ecfnodes_name])
            else:
                event_name = "%s_%s" %( task, event )
                ecfnodes_name = "%s" % ( event_name )
                event_item = "%s" %( event )
                self.ecfnodes[ecfnodes_name] = ecflow.Event(event_item)
                self.ecfnodes[task].add(self.ecfnodes[ecfnodes_name])

    def add_edit(self,task,edit_dict, counter=None):
        edits_to_add = {}
        for edit in edit_dict:
            if re.search("\{.*\}",edit_dict[edit]) and counter is not None:
                edit_name = edit
                edit_base_value = re.search("(.*)\{.*\}",edit_dict[edit]).group(1).strip()
                if re.search("initial_count",edit_dict[edit]):
                    initial_count = re.search(".*\{(.*)\}",edit_dict[edit]).group(1).strip().split(':')[1]
                    edits_to_add[edit] = "%s%03d" %( edit_base_value, counter+int(initial_count) )
                elif re.search("increment",edit_dict[edit]):
                    increment = re.search(".*\{(.*)\}",edit_dict[edit]).group(1).strip().split(':')[1]
                    edits_to_add[edit] = "%s%03d" %( edit_base_value, counter*int(increment) )
                else:
                    edits_to_add[edit] = "%s%03d" %( edit_base_value, counter )
            else:
                edits_to_add[edit] = edit_dict[edit]
        self.ecfnodes[task].add( ecflow.Edit( edits_to_add ))

#class ecfEvent()

#class ecfTask(ecflow.Task):
#
#    def __init(self,name):
#        self.ecftask = Task(name)
#
#    def get_node():
#        return self.ecftask
#
#class ecfFamily(ecflow.Family):
#
#    def __init(self,name):
#        self.ecffamily = Family(name)
#
#    def get_node():
#        return self.ecffamily
#
#    def
