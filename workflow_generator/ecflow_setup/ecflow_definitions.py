#!/usr/bin/env python3

'''
    PROGRAM:
        Create the ecFlow workflow
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        2. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail
    OUTPUT:
        1. ecFlow definition file
        2. ecFlow scripts for post processing
'''

import sys
import os
import re
import shutil

try:
    import ecflow
except ImportError as err:
    raiseException("Error: Could not import ecflow module: %s" % err)

class Ecflowsuite:

    def __init__(self,ecfsuite,ecfhome,build_tree=True):

        # Initialize environment
        self.ecfnodes = {}
        self.ecfhome = ecfhome
        self.build_tree = build_tree

        # Create initial suite
        self.ecfsuite = self.add_suite(ecfsuite)

    def raiseException(self,e):
        print(e)
        sys.exit(1)

    def invalid_node():
        raise Exception("Node is not definied")

    def add_suite(self, suite):
        new_suite = ecfSuite("%s" % suite)
        if self.build_tree:
            new_suite.generate_folders(self.ecfhome)
        return new_suite

    def get_suite(self):
        return self.ecfsuite

    def get_suite_name(self):
        return self.ecfsuite.name()

    def get_base_name(self,node):
        return re.search("(.*)\{.*\}",node).group(1).strip()

    def get_range(self,node,initial_count=0,increment=1,max_value=1):
        count_string = re.search(".*\{(.*)\}.*",node).group(1).strip().split(',')
        for item in count_string:
            if 'initial_count' in item and initial_count==0:
                initial_count = int(item.split(':')[1].strip())
            if 'increment' in item and increment==1:
                increment = int(item.split(':')[1].strip())
            if item.strip().isdigit() and max_value==1:
                max_value = int(item.strip())
        max_value = max_value * increment
        if initial_count > 0:
            max_value += initial_count
        return range(initial_count,max_value,increment)

    def add_edit(self,edit_dict,parent=None):
        if parent:
            self.ecfnodes[parent] += ecflow.Edit(edit_dict)
        else:
            self.ecfsuite += ecflow.Edit(edit_dict)

    def add_family(self,family,parents=None):
        family_name = "%s_%s" % (parents, family) if parents else family

        # If the name already exists, the family already exists
        if family_name not in self.ecfnodes.keys():
            self.ecfnodes[family_name] = ecfFamily(family)
            if self.build_tree:
                self.ecfnodes[family_name].generate_folders(self.ecfhome,self.get_suite_name(),parents)

        if parents:
            self.ecfnodes[parents] += self.ecfnodes[family_name]
        else:
            self.ecfsuite += self.ecfnodes[family_name]

    def add_task(self,task,parents,scriptrepo,template=None):
        if re.search(r"\{.*\}",task):
            task_base_name = self.get_base_name(task)
            counter = self.get_range(task)
            for task_number in counter:
                task_name = "%s%03d" %( task_base_name, task_number )
                if task_name not in self.ecfnodes.keys():
                    self.ecfnodes[task_name] = ecfTask(task_name)
                    self.ecfnodes[task_name].set_scriptrepo(scriptrepo)
                    if self.build_tree:
                        self.ecfnodes[task_name].generate_ecflow_task(self.ecfhome,self.get_suite_name(),parents)
                    self.ecfnodes[parents] += self.ecfnodes[task_name]
        else:
            if task not in self.ecfnodes.keys():
                self.ecfnodes[task] = ecfTask(task)
                self.ecfnodes[task].set_scriptrepo(scriptrepo)
                if self.build_tree:
                    self.ecfnodes[task].generate_ecflow_task(self.ecfhome,self.get_suite_name(),parents)
                self.ecfnodes[parents] += self.ecfnodes[task]

    def add_task_edits(self,task,edit_dict):
        edits_to_add = {}
        if re.search(r"\{.*\}",task):
            task_base_name = self.get_base_name(task)
            task_counter = self.get_range(task)
            loop_index = 0
            for task_number in task_counter:
                task_name = "%s%03d" %( task_base_name, task_number )
                for edit in edit_dict:
                    if re.search(r"\{.*\}",edit_dict[edit]):
                        edit_base_name = self.get_base_name(edit_dict[edit])
                        keywords = ['initial_count','increment']
                        if any(match in edit_dict[edit] for match in keywords):
                            total_tasks=len(task_counter)
                            edit_range = self.get_range(edit_dict[edit],max_value=total_tasks)
                            edit_count = [*edit_range]
                            edit_value = "%s%03d" %(edit_base_name, edit_count[loop_index])
                        else:
                            edit_value = "%s%03d" %(edit_base_name, task_number)
                        self.add_edit({edit:edit_value},task_name)
                    else:
                        self.add_edit({edit:edit_dict[edit]},task_name)
                loop_index+=1
        else:
            for edit in edit_dict:
                self.add_edit({edit:edit_dict[edit]},task)


class ecfRoot():

    def get_base_name():
        return re.search("(.*)\{.*\}",self.name()).group(1).strip()

class ecfSuite(ecflow.Suite,ecfRoot):

    def generate_folders(self,ecfhome):
        folder_path = "%s/%s" %(ecfhome,self.name())
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

#class ecfEvent()

class ecfFamily(ecflow.Family,ecfRoot):

    def generate_folders(self,ecfhome,suite,parents):
        if parents:
            folder_path = "%s/%s/%s/%s" %(ecfhome,suite,parents.replace('_','/'),self.name())
        else:
            folder_path = "%s/%s/%s" %(ecfhome,suite,self.name())
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfTask(ecflow.Task,ecfRoot):

    def set_scriptrepo(self,repopath):
        self.scriptrepo = repopath

    def generate_ecflow_task(self,ecfhome,suite,parents):
        script_name = "%s.ecf" % self.name()
        ecfscript = None
        if parents:
            script_path = "%s/%s/%s/%s.ecf" %(ecfhome,suite,parents.replace('_','/'),script_name)
        else:
            script_path = "%s/%s/%s.ecf" %(ecfhome,suite,script_name)
        for root,dirs,files in os.walk(self.scriptrepo):
            if script_name in files and ecfscript is None:
                ecfscript = os.path.join(root, script_name)
            elif script_name in files:
                print("More than one script named %s. Using the first one found." % script_name )
        if ecfscript is not None:
            shutil.copyfile(ecfscript, script_path, follow_symlinks=True)
        else:
            raise Exception("Could not find the script %s. Exiting build." % script_name)
