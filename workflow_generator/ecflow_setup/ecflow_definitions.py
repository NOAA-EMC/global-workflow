#!/usr/bin/env python3

'''
    PROGRAM:
        Manage the ecflow definition structures
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. None
    OUTPUT:
        1. The ecFlow definition file
        2. The ecFlow folder structure and scripts

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

    def add_edit(self,edit_dict,parent=None):
        if parent:
            self.ecfnodes[parent] += ecflow.Edit(edit_dict)
        else:
            self.ecfsuite += ecflow.Edit(edit_dict)

    def add_event(self,event,parent=None):
        if parent:
            self.ecfnodes[parent] += ecflow.Event(event)

    def add_trigger(self,trigger,parent,state=None,event=None):
        if state is None and event is None:
            self.ecfnodes[parent].add(ecflow.Trigger([self.ecfnodes[trigger]]))
        elif state is not None and event is None:
            trigger_path = self.ecfnodes[trigger].get_abs_node_path()
            self.ecfnodes[parent].add(ecflow.Trigger("%s == %s" %( trigger_path, state)))
        elif state is None and event is not None:
            trigger_path = self.ecfnodes[trigger].get_abs_node_path()
            self.ecfnodes[parent].add(ecflow.Trigger("%s:%s" %( trigger_path, event)))


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
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop():
            for task_number in taskNode.get_range():
                task_name = "%s%03d" %( taskNode.get_base_name(), task_number )
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
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop():
            loop_index = 0
            for task_number in taskNode.get_range():
                task_name = "%s%03d" %( taskNode.get_base_name(), task_number )
                for edit, editvalue in edit_dict.items():
                    editNodeValue = ecfEditNode(editvalue)
                    if editNodeValue.is_loop():
                        edit_base_name = editNodeValue.get_base_name()
                        if editNodeValue.has_counter():
                            total_tasks=len(taskNode.get_range())
                            edit_range = editNodeValue.get_range(max_value=total_tasks)
                            edit_count = [*edit_range]
                            neweditvalue = "%s%03d" %(edit_base_name, edit_count[loop_index])
                        else:
                            neweditvalue = "%s%03d" %(edit_base_name, task_number)
                        self.add_edit({edit:neweditvalue},task_name)
                    else:
                        self.add_edit({edit:editvalue},task_name)
                loop_index+=1
        else:
            for edit in edit_dict:
                self.add_edit({edit:edit_dict[edit]},task)

    def add_task_events(self,task,events):
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop():
            for task_number in taskNode.get_range():
                task_name = "%s%03d" %( taskNode.get_base_name(), task_number )
                for event in events:
                    eventNode = ecfEventNode(event)
                    if eventNode.is_loop():
                        event_base_name = self.get_base_name(event)
                        if eventNode.has_counter():
                            event_counter = self.get_range(event)
                            for event_number in event_counter:
                                event_name = "%s%03d" %( event_base_name, event_number )
                                self.add_event(event_name,task_name)
                        else:
                            event_name = "%s%03d" %(event,task_number)
                            self.add_event(event_name,task_name)
                    else:
                        self.add_event(event,task_name)
        else:
            for event in events:
                eventNode = ecfEventNode(event)
                if eventNode.has_counter():
                    for event_number in eventNode.get_range():
                        event_name = "%s%03d" %( eventNode.get_base_name(), event_number )
                        self.add_event(event_name,task)
                else:
                    self.add_event(event,task)

    def add_task_triggers(self,task,triggers):

        def process_trigger(trigger_name,triggerTaskNode,task,task_loop_index=None,total_tasks=None,task_number=None):
            if triggerTaskNode.has_state():
                self.add_trigger(trigger_name,task,state=triggerTaskNode.get_state())
            elif triggerTaskNode.has_event():
                if triggerTaskNode.has_event_loop():
                    if triggerTaskNode.has_event_max_value():
                        for event_count in triggerTaskNode.get_event_range():
                            event_name = "%s%03d" %( triggerTaskNode.get_event_base_name(), event_count)
                            self.add_trigger(trigger_name,task,event=event_name)
                    elif triggerTaskNode.has_event_counter():
                        event_range = triggerTaskNode.get_event_range(max_value=total_tasks)
                        event_count = [*event_range]
                        event_name = "%s%03d" %(triggerTaskNode.get_event_base_name(),event_count[task_loop_index])
                        self.add_trigger(trigger_name,task,event=event_name)
                    else:
                        event_name = "%s%03d" %(triggerTaskNode.get_event_base_name(),task_number)
                        self.add_trigger(trigger_name,task,event=event_name)
                else:
                    self.add_trigger(trigger_name,task,event=triggerTaskNode.get_event())

            else:
                self.add_trigger(trigger_name,task)

        taskNode = ecfTaskNode(task)
        if taskNode.is_loop():
            task_loop_index=0
            for task_number in taskNode.get_range():
                task_name = "%s%03d" %( taskNode.get_base_name(), task_number )
                total_tasks=len(taskNode.get_range())
                for trigger in triggers:
                    triggerTaskNode = ecfTriggerNode(trigger)
                    if triggerTaskNode.is_loop():
                        if triggerTaskNode.has_max_value():
                            for trigger_flag in triggerTaskNode.get_range():
                                trigger_name = "%s%03d" %(triggerTaskNode.get_base_name(),trigger_flag)
                                process_trigger(trigger_name,triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                        else:
                            trigger_range = triggerTaskNode.get_range(max_value=total_tasks)
                            trigger_count = [*trigger_range]
                            trigger_name = "%s%03d" %(triggerTaskNode.get_base_name(),trigger_count[task_loop_index])
                            process_trigger(trigger_name,triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                    else:
                        process_trigger(triggerTaskNode.get_name(),triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                task_loop_index+=1
        else:
            for trigger in triggers:
                triggerTaskNode = ecfTriggerNode(trigger)
                if triggerTaskNode.is_loop():
                    if triggerTaskNode.has_max_value():
                        for trigger_flag in triggerTaskNode.get_range():
                            trigger_name = "%s%03d" %(triggerTaskNode.get_base_name(),trigger_flag)
                            process_trigger(trigger_name,triggerTaskNode,task)
                    else:
                        self.raiseException("Task: %s - Looping mechanism called without max value in a non looped task." % task)
                else:
                    process_trigger(triggerTaskNode.get_name(),triggerTaskNode,task)

class ecfNode():

    def __init__(self,ecfItem):
        self.name = ecfItem

    def get_name(self):
        return self.name

    def is_loop(self):
        if re.search(r"\{.*\}",self.name):
            return True
        else:
            return False

    def get_base_name(self):
        if re.search(r"\{.*\}",self.name):
            return re.search("(.*)\{.*\}",self.name).group(1).strip()
        else:
            return node

    def has_counter(self):
        if re.search(r"\{.*\}",self.name):
            count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
            for item in count_string:
                if ('initial_count' in item or
                    'increment' in item or
                    item.strip().isdigit()):
                    return True
            return False

    def has_initial_count(self):
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        for item in count_string:
            if 'initial_count' in item:
                return True
        return False

    def get_initial_count(self):
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        initial_count = 0
        for item in count_string:
            if 'initial_count' in item and initial_count==0:
                initial_count = int(item.split(':')[1].strip())
        return initial_count

    def has_max_value(self):
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        for item in count_string:
            if item.strip().isdigit():
                return True
        return False

    def get_max_value(self):
        max_value = 1
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        for item in count_string:
            if item.strip().isdigit() and max_value==1:
                max_value = int(item.strip())
        return max_value

    def get_increment(self):
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        for item in count_string:
            if 'increment' in item:
                return True
        return False

    def get_increment(self):
        increment = 1
        count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
        for item in count_string:
            if 'increment' in item and increment==1:
                increment = int(item.split(':')[1].strip())
        return increment

    def get_count_string(self):
        return re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')

    def get_range(self,initial_count=0,increment=1,max_value=1):
        if re.search(r"\{.*\}",self.name):
            count_string = re.search(".*\{(.*)\}.*",self.name).group(1).strip().split(',')
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
        else:
            return range(0,1,1)

class ecfTaskNode(ecfNode):

    def get_type(self):
        return 'task'

class ecfTriggerNode(ecfNode):

    def __init__(self,ecfItem):
        self.task_setup = ecfItem
        self.name = ecfItem['task']

    def get_type(self):
        return 'trigger'

    def get_state(self):
        return self.state

    def get_event(self):
        return self.event_string

    def has_state(self):
        if 'state' in self.task_setup.keys():
            self.state = self.task_setup['state']
            return True
        else:
            return False

    def has_event(self):
        if 'event' in self.task_setup.keys():
            self.event_string = self.task_setup['event']
            return True
        else:
            return False

    def has_event_max_value(self):
        if re.search(r"\{.*\}",self.event_string):
            count_string = re.search(".*\{(.*)\}.*",self.event_string).group(1).strip().split(',')
            for item in count_string:
                if item.strip().isdigit():
                    return True
        return False

    def has_event_loop(self):
        if re.search(r"\{.*\}",self.event_string):
            return True
        return False

    def get_event_base_name(self):
        if re.search(r"\{.*\}",self.event_string):
            return re.search("(.*)\{.*\}",self.event_string).group(1).strip()
        else:
            return node

    def has_event_counter(self):
        if re.search(r"\{.*\}",self.event_string):
            count_string = re.search(".*\{(.*)\}.*",self.event_string).group(1).strip().split(',')
            for item in count_string:
                if ('initial_count' in item or
                    'increment' in item or
                    item.strip().isdigit()):
                    return True
        return False

    def get_event_range(self,initial_count=0,increment=1,max_value=1):
        count_string = re.search(".*\{(.*)\}.*",self.event_string).group(1).strip().split(',')
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

class ecfEventNode(ecfNode):

    def get_type(self):
        return 'edit'

class ecfEditNode(ecfNode):

    def get_type(self):
        return 'edit'

class ecfRoot():

    def get_base_name():
        return re.search("(.*)\{.*\}",self.name()).group(1).strip()

class ecfSuite(ecflow.Suite,ecfRoot):

    def generate_folders(self,ecfhome):
        folder_path = "%s/%s" %(ecfhome,self.name())
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

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
