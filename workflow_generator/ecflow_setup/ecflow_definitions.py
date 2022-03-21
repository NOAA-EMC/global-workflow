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
    raise Exception(f"Error: Could not import ecflow module: {err}")

class Ecflowsuite:

    def __init__(self,ecfsuite,ecfhome,build_tree=True):

        # Initialize environment
        self.ecfnodes = {}
        self.ecfhome = ecfhome
        self.build_tree = build_tree

        # Create initial suite
        self.ecfsuite = self.add_suite(ecfsuite)

    def add_suite(self, suite):
        new_suite = ecfSuite(f"{suite}")
        if self.build_tree:
            new_suite.generate_folders(self.ecfhome)
        return new_suite

    def get_suite(self):
        return self.ecfsuite

    def get_suite_name(self):
        return self.ecfsuite.name()

    def get_task(self,task):
        return self.ecfnodes[task]

    def add_edit(self,edit_dict,parent=None):
        if parent:
            self.ecfnodes[parent] += ecflow.Edit(edit_dict)
        else:
            self.ecfsuite += ecflow.Edit(edit_dict)

    def add_event(self,event,parent=None):
        if parent:
            self.ecfnodes[parent] += ecflow.Event(event)

    def add_trigger(self,trigger,parent,state=None,event=None,suite=None,suite_array=None):
        if suite is not None:
            try:
                trigger_path = suite_array[suite].get_task(trigger).get_abs_node_path()
                if state is None and event is None:
                    self.ecfnodes[parent].add(ecflow.Trigger(f"{trigger_path} == complete" ))
                elif state is not None and event is None:
                    self.ecfnodes[parent].add(ecflow.Trigger(f"{trigger_path} == {state}" ))
                elif state is None and event is not None:
                    self.ecfnodes[parent].add(ecflow.Trigger(f"{trigger_path}:{event}" ))
            except KeyError as e:
                print(f"Suite {suite} for task/trigger {parent}/{trigger} is not available. Please check the configuration file.")
                print("Error {e}")
                sys.exit(1)
        else:
            try:
                if state is None and event is None:
                    self.ecfnodes[parent].add(ecflow.Trigger([self.ecfnodes[trigger]]))
                elif state is not None and event is None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    self.ecfnodes[parent].add(ecflow.Trigger(f"{trigger_path} == {state}" ))
                elif state is None and event is not None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    self.ecfnodes[parent].add(ecflow.Trigger(f"{trigger_path}:{event}" ))
            except KeyError as e:
                print(f"The task/trigger {parent}/{trigger} is not available in suite {self.get_suite_name()}. Please check the configuration file.")
                print(f"Error {e}")
                sys.exit(1)


    def add_family(self,family,parents=None):
        family_name = f"{parents}_{family}" if parents else family

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
        if taskNode.is_loop() or taskNode.is_list:
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                if task_name not in self.ecfnodes.keys():
                    self.ecfnodes[task_name] = ecfTask(task_name)
                    self.ecfnodes[task_name].setup_script(scriptrepo,template)
                    if self.build_tree:
                        self.ecfnodes[task_name].generate_ecflow_task(self.ecfhome,self.get_suite_name(),parents)
                    self.ecfnodes[parents] += self.ecfnodes[task_name]
        else:
            if task not in self.ecfnodes.keys():
                self.ecfnodes[task] = ecfTask(task)
                self.ecfnodes[task].setup_script(scriptrepo,template)
                if self.build_tree:
                    self.ecfnodes[task].generate_ecflow_task(self.ecfhome,self.get_suite_name(),parents)
                self.ecfnodes[parents] += self.ecfnodes[task]

    def add_task_edits(self,task,edit_dict):
        edits_to_add = {}
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            loop_index = 0
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                for edit, editvalue in edit_dict.items():
                    editNodeValue = ecfEditNode(editvalue)
                    if editNodeValue.is_loop():
                        if editNodeValue.use_parent_counter:
                            neweditvalue = f"{editNodeValue.get_full_name(task_number)}"
                        else:
                            total_tasks=len(taskNode.get_range())
                            edit_range = editNodeValue.get_range(max_value=total_tasks)
                            edit_count = [*edit_range]
                            neweditvalue = f"{editNodeValue.get_full_name(edit_count[loop_index])}"
                    elif editNodeValue.is_list:
                        try:
                            if len(editNodeValue.items) == len(taskNode.get_range()):
                                neweditvalue = f"{editNodeValue.get_full_name(loop_index)}"
                            else:
                                raise ConfigurationError
                        except:
                            print(f"The listed array of {edit} excceds the parent counter. Please check the configuration file")
                            sys.exit(1)
                    else:
                        neweditvalue = editvalue
                    self.add_edit({edit:neweditvalue},task_name)
                loop_index+=1
        else:
            for edit in edit_dict:
                self.add_edit({edit:edit_dict[edit]},task)

    def add_task_events(self,task,events):
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            for task_number in taskNode.get_range():
                loop_index = 0
                task_name = f"{taskNode.get_full_name(task_number)}"
                for event in events:
                    eventNode = ecfEventNode(event)
                    if eventNode.is_loop():
                        if eventNode.use_parent_counter:
                            event_name = f"{eventNode.get_full_name(task_number)}"
                            self.add_event(event_name,task_name)
                        else:
                            event_counter = eventNode.get_range()
                            for event_number in event_counter:
                                event_name = f"{eventNode.get_full_name(event_number)}"
                                self.add_event(event_name,task_name)
                    elif eventNode.is_list:
                        try:
                            if len(eventNode.items) == len(taskNode.get_range()):
                                neweditvalue = f"{editNodeValue.get_full_name(loop_index)}"
                            else:
                                raise ConfigurationError
                        except:
                            print(f"The listed array of {eventNode.get_name()} excceds the parent counter. Please check the configuration file")
                            sys.exit(1)

                    else:
                        self.add_event(event,task_name)
                loop_index+=1
        else:
            for event in events:
                eventNode = ecfEventNode(event)
                if eventNode.is_loop():
                    for event_number in eventNode.get_range():
                        event_name = f"{eventNode.get_full_name(event_number)}"
                        self.add_event(event_name,task)
                else:
                    self.add_event(event,task)

    def add_task_triggers(self,task,triggers,suite_array):

        def process_trigger(trigger_name,triggerTaskNode,task,task_loop_index=None,total_tasks=None,task_number=None):
            if triggerTaskNode.has_suite():
                suite = triggerTaskNode.get_suite()
            else:
                suite = None
            if triggerTaskNode.has_state():
                self.add_trigger(trigger_name,task,state=triggerTaskNode.get_state(),suite=suite,suite_array=suite_array)
            elif triggerTaskNode.has_event():
                if triggerTaskNode.is_event_loop():
                    if triggerTaskNode.has_event_max_value():
                        for event_count in triggerTaskNode.get_event_range():
                            event_name = f"{triggerTaskNode.get_event_full_name(event_count)}"
                            self.add_trigger(trigger_name,task,event=event_name,suite=suite,suite_array=suite_array)
                    elif triggerTaskNode.event_parent_counter:
                        event_name = f"{triggerTaskNode.get_event_full_name(task_number)}"
                        self.add_trigger(trigger_name,task,event=event_name,suite=suite,suite_array=suite_array)
                    else:
                        event_range = triggerTaskNode.get_event_range(max_value=total_tasks)
                        event_count = [*event_range]
                        event_name = f"{triggerTaskNode.get_event_full_name(event_count[task_loop_index])}"
                        self.add_trigger(trigger_name,task,event=event_name,suite=suite,suite_array=suite_array)
                else:
                    self.add_trigger(trigger_name,task,event=triggerTaskNode.get_event(),suite=suite,suite_array=suite_array)

            else:
                self.add_trigger(trigger_name,task,suite=suite,suite_array=suite_array)

        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            task_loop_index=0
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                total_tasks=len(taskNode.get_range())
                for trigger in triggers:
                    triggerTaskNode = ecfTriggerNode(trigger)
                    if triggerTaskNode.is_loop() or triggerTaskNode.is_list:
                        if triggerTaskNode.is_list or triggerTaskNode.has_max_value():
                            for trigger_flag in triggerTaskNode.get_range():
                                trigger_name = f"{triggerTaskNode.get_full_name(trigger_flag)}"
                                process_trigger(trigger_name,triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                        else:
                            trigger_range = triggerTaskNode.get_range(max_value=total_tasks)
                            trigger_count = [*trigger_range]
                            trigger_name = f"{triggerTaskNode.get_full_name(trigger_count[task_loop_index])}"
                            process_trigger(trigger_name,triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                    else:
                        process_trigger(triggerTaskNode.get_name(),triggerTaskNode,task_name,task_loop_index,total_tasks,task_number)
                task_loop_index+=1
        else:
            for trigger in triggers:
                triggerTaskNode = ecfTriggerNode(trigger)
                if triggerTaskNode.is_loop():
                    try:
                        if triggerTaskNode.has_max_value():
                            for trigger_flag in triggerTaskNode.get_range():
                                trigger_name = f"{triggerTaskNode.get_full_name(trigger_flag)}"
                                process_trigger(trigger_name,triggerTaskNode,task)
                        else:
                            raise ConfigurationError
                    except ConfigurationError:
                        print(f"Task: {task} - Looping mechanism called without max value in a non looped task." )
                        sys.exit(1)
                else:
                    process_trigger(triggerTaskNode.get_name(),triggerTaskNode,task)

class ecfNode():

    def __init__(self,ecfItem):
        if isinstance(ecfItem,str):
            if re.search(r".*\(.*\).*",ecfItem):
                self.name = ecfItem
                self.is_list = False
            elif re.search(r".*\[.*\].*",ecfItem):
                self.name = ecfItem
                self.is_list = True
                self.use_parent_counter = False
                self.items = re.search(".*\[(.*)\].*",self.name).group(1).strip().split(',')
            else:
                self.name = ecfItem
                self.is_list = False
        elif isinstance(ecfItem,list):
            self.name = ''
            self.is_list = True
            self.items = ecfItem
        else:
            self.name = ecfItem
            self.is_list = False

    def get_name(self):
        return self.name

    def is_loop(self):
        range_functions = {
            1: self.set_max_value,
            2: self.set_initial_max_value,
            3: self.set_initial_increment_max_value,
        }
        if re.search(r".*\(.*\).*",self.name):
            self.use_parent_counter = False
            range_token = re.search(".*\((.*)\).*",self.name).group(1).strip().split(',')
            range_functions.get(len(range_token),self.invalid_range)(range_token)
            return True
        else:
            return False

    def invalid_range(self,range_token):
        print(f"The range specified in {self.name} is out of bounds. Please review the configuration.")
        sys.exit(1)

    def set_max_value(self,range_token):
        self.initial_count = None
        self.increment = None
        if not range_token[0]:
            self.max_value = None
            self.use_parent_counter = True
        else:
            try:
                self.max_value = int(range_token[0])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def set_initial_max_value(self,range_token):
        try:
            self.initial_count = None if not range_token[0] else int(range_token[0])
        except TypeError:
            print(f"Initial count value for {self.name} is not an integer")
            sys.exit(1)
        self.increment = None
        if not range_token[1]:
            self.max_value = None
        else:
            try:
                self.max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def set_initial_increment_max_value(self,range_token):
        try:
            self.initial_count = None if not range_token[0] else int(range_token[0])
            self.increment = None if not range_token[2] else int(range_token[2])
        except TypeError:
            print(f"Initial count and increment values for {self.name} are not integers")
            sys.exit(1)
        if not range_token[1]:
            self.max_value = None
        else:
            try:
                self.max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def get_full_name(self,counter=None):
        try:
            if re.search(r"\(.*\)",self.name):
                name_token = re.search("(.*)\(.*\)(.*)",self.name)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                if isinstance(counter,int):
                    return f"{base}{counter:03}{suffix}"
                elif isinstance(counter,str):
                    return f"{base}{counter}{suffix}"
            elif re.search(r"\[.*\]",self.name):
                name_token = re.search("(.*)\[.*\](.*)",self.name)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                array_item = self.items[counter]
                if isinstance(array_item,int):
                    return f"{base}{array_item:03}{suffix}"
                elif isinstance(array_item,str):
                    return f"{base}{array_item}{suffix}"
            elif self.is_list:
                array_item = self.items[counter]
                if isinstance(array_item,int):
                    return f"{array_item:03}"
                elif isinstance(array_item,str):
                    return f"{array_item}"
            else:
                return self.name
        except ValueError as err:
            print(f"Problem getting full name of {self.name}. Error: {err}")

    def has_max_value(self):
        return True if self.max_value is not None else False

    def get_max_value(self):
        return self.max_value

    def get_range(self,initial_count=0,increment=1,max_value=1):
        if self.is_list:
            return range(initial_count,len(self.items),increment)
        else:
            if self.initial_count is not None: initial_count = self.initial_count
            if self.increment is not None: increment = self.increment
            if self.max_value is not None: max_value = self.max_value
            max_value = ( max_value * increment ) + initial_count
            return range(initial_count,max_value,increment)

class ecfTaskNode(ecfNode):

    def get_type(self):
        return 'task'

class ecfTriggerNode(ecfNode):

    def __init__(self,ecfItem):
        self.task_setup = ecfItem
        if isinstance(ecfItem['task'],str):
            if re.search(r".*\(.*\).*",ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = False
            elif re.search(r".*\[.*\].*",ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = True
                self.use_parent_counter = False
                self.items = re.search(".*\[(.*)\].*",ecfItem['task']).group(1).strip().split(',')
            else:
                self.name = ecfItem['task']
                self.is_list = False
        elif isinstance(ecfItem,list):
            self.name = ''
            self.is_list = True
            self.items = ecfItem['task']
        else:
            self.name = ecfItem['task']
            self.is_list = False

    def get_type(self):
        return 'trigger'

    def get_state(self):
        return self.state

    def get_event(self):
        return self.event_string

    def has_suite(self):
        if 'suite' in self.task_setup.keys():
            self.suite = self.task_setup['suite']
            return True
        else:
            return False

    def get_suite(self):
        return self.suite

    def has_state(self):
        if 'state' in self.task_setup.keys():
            self.state = self.task_setup['state']
            return True
        else:
            return False

    def has_event(self):
        if 'event' in self.task_setup.keys():
            if isinstance(self.task_setup['event'],str):
                if re.search(r".*\(.*\).*",self.task_setup['event']):
                    self.event_string = self.task_setup['event']
                    self.is_list = False
                elif re.search(r".*\[.*\].*",self.task_setup['event']):
                    self.event_string = self.task_setup['event']
                    self.is_event_list = True
                    self.items = re.search(".*\[(.*)\].*",self.task_setup['event']).group(1).strip().split(',')
                else:
                    self.event_string = self.task_setup['event']
                    self.is_event_list = False
            elif isinstance(self.task_setup['event'],list):
                self.is_event_list = True
                self.event_items = self.task_setup['event']
            else:
                self.event_string = self.task_setup['event']
                self.is_event_list = False
            return True
        else:
            return False

    def is_event_loop(self):
        range_functions = {
            1: self.set_event_max_value,
            2: self.set_event_initial_max_value,
            3: self.set_event_initial_increment_max_value,
        }
        if re.search(r"\(.*\)",self.event_string):
            self.event_parent_counter = False
            range_token = re.search(".*\((.*)\).*",self.event_string).group(1).strip().split(',')
            range_functions.get(len(range_token),self.invalid_range)(range_token)
            return True
        else:
            return False

    def invalid_event_range(self,range_token):
        print(f"The range specified in {self.name} is out of bounds. Please review the configuration.")
        sys.exit(1)

    def set_event_max_value(self,range_token):
        self.event_initial_count = None
        self.event_increment_value = None
        if not range_token[0]:
            self.event_max_value = None
            self.event_parent_counter = True
        else:
            try:
                self.event_max_value = int(range_token[0])
            except TypeError:
                print(f"Maximum value for {self.event_string} is not an integer")
                sys.exit(1)

    def set_event_initial_max_value(self,range_token):
        try:
            self.event_initial_count = None if not range_token[0] else int(range_token[0])
        except TypeError:
            print(f"Initial value for {self.event_string} is not an integer")
            sys.exit(1)
        self.event_increment_value = None
        if not range_token[1]:
            self.event_max_value = None
        else:
            try:
                self.event_max_value = range_token[1]
            except TypeError:
                print(f"Maximum value for {self.event_string} is not an integer")
                sys.exit(1)

    def set_event_initial_increment_max_value(self,range_token):
        try:
            self.event_initial_count = None if not range_token[0] else range_token[0]
            self.event_increment_value = None if not range_token[2] else range_token[2]
        except TypeError:
            print(f"Initial cound and increment values for {self.event_string} are not integers")
            sys.exit(1)
        if not range_token[1]:
            self.event_max_value = None
        else:
            try:
                self.event_max_value = range_token[1]
            except TypeError:
                print(f"Maximum value for {self.event_string} is not an integer")
                sys.exit(1)

    def get_event_full_name(self,counter=None):
        try:
            if re.search(r"\(.*\)",self.event_string):
                name_token = re.search("(.*)\(.*\)(.*)",self.event_string)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                if isinstance(counter,int):
                    return f"{base}{counter:03}{suffix}"
                elif isinstance(counter,str):
                    return f"{base}{counter}{suffix}"
            elif re.search(r"\[.*\]",self.event_string):
                name_token = re.search("(.*)\[.*\](.*)",self.event_string)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                array_item = self.event_items[counter]
                if isinstance(array_item,int):
                    return f"{base}{array_item:03}{suffix}"
                elif isinstance(array_item,str):
                    return f"{base}{array_item}{suffix}"
            elif self.is_list:
                array_item = self.event_items[counter]
                if isinstance(array_item,int):
                    return f"{array_item:03}"
                elif isinstance(array_item,str):
                    return f"{array_item}"
            else:
                return self.event_string
        except ValueError as err:
            print(f"Problem getting full name of {self.event_string}. Error: {err}")

    def has_event_max_value(self):
        return True if self.event_max_value is not None else False

    def get_event_max_value(self):
        return self.event_max_value

    def get_event_range(self,initial_count=0,increment=1,max_value=1):
        if self.event_initial_count is not None: initial_count = self.event_initial_count
        if self.event_increment_value is not None: increment = self.event_increment_value
        if self.event_max_value is not None: max_value = self.event_max_value
        max_value = ( max_value * increment ) + initial_count
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
        folder_path = f"{ecfhome}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfFamily(ecflow.Family,ecfRoot):

    def generate_folders(self,ecfhome,suite,parents):
        if parents:
            folder_path = f"{ecfhome}/{suite}/{parents.replace('_','/')}/{self.name()}"
        else:
            folder_path = f"{ecfhome}/{suite}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfTask(ecflow.Task,ecfRoot):

    def setup_script(self,repopath,template):
        self.scriptrepo = repopath
        self.template = template

    def generate_ecflow_task(self,ecfhome,suite,parents):
        script_name = f"{self.name()}.ecf"
        ecfscript = None
        search_script = f"{self.template}.ecf" if self.template is not None else script_name
        if parents:
            script_path = f"{ecfhome}/{suite}/{parents.replace('_','/')}/{script_name}"
        else:
            script_path = f"{ecfhome}/{suite}/{script_name}"
        for root,dirs,files in os.walk(self.scriptrepo):
            if search_script in files and ecfscript is None:
                ecfscript = os.path.join(root, search_script)
            elif script_name in files:
                print(f"More than one script named {script_name}. Using the first one found.")
        try:
            if ecfscript is not None:
                shutil.copyfile(ecfscript, script_path, follow_symlinks=True)
            else:
                raise ConfigurationError
        except ConfigurationError:
            print(f"Could not find the script {search_script}. Exiting build.")
            sys.exit(1)

# define Python user-defined exceptions
class Error(Exception):
    """Base class for other exceptions"""
    pass

class RangeError(Error):
    """Raised when the range in the configuration file is incorrect"""
    pass

class ConfigurationError(Error):
    """Raised when there is an error in the configuration file."""
    pass
