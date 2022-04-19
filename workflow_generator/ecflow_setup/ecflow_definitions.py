#!/usr/bin/env python3

"""
    PROGRAM:
        Manage the ecflow definitions setup. The main purpose of this class is
        to be called by the ecflow_setup.py module and create an Ecflowsuite
        object for each suite. Then the processing for triggers, breaking
        apart the list or loop strings and adding in the triggers an events
        using the ecflow module to call things like ecflow.Task,
        ecflow.Trigger, and so on.

        The creates a dictionary object of each of the items it creates and
        then uses the ecfsuite dict to reference so a task's trigger will
        reference an already existing task and thereby automatically
        populate the full path to the task as opposed to a more
        dynamic reference.

        At the bottom of this module are the custom objects created to extend
        the ecflow classes for tasks, families,
        etc.
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        None
    OUTPUT:
        None
"""
import sys
import os
import re
import shutil
from datetime import datetime, timedelta
try:
    import ecflow
except ImportError as err:
    raise Exception(f"Error: Could not import ecflow module: {err}")


class Ecflowsuite:
    """
    This class is the representation of an ecflow suite. It manages all of the
    items within using a dictionary. Names for the tasks are in the dictonary
    by their name so all task names in a suite need to be unique otherwise
    you'll have some issues.

    Families in the dictionary are represented by the parent nodes combined
    with the family name, so for example the family
    gfs:
      atmos:
        post:
    will be in the dictionary at the key gfs>atmos>post, referenced like so,
    self.nodes[gfs>atmos>post]. That will contain all of the objects for
    that family.

    Attributes
    ----------
    ecfnodes : dict
        Dictionary object that contains all the nodes within the suite. This
        includes tasks, families, triggers, events, etc.
    ecfhome : str
        The path to the base for the ecf items. This includes the ecf scripts
        repository and the storage location for all the suite script. In the
        default, it is generally assumed to be the ecfGFS parameter from the
        experiment setup.
    build_tree : bool
        A boolean that indicates if the application should build the folders
        and scripts as part of the run. If this is false, the scripts and
        folders are not created and assumed to already be in place.
    ecfsuite : str
        The name of the suite.

    Methods
    -------
    add_suite(suite)
        Creates the suite and if necessary creates the base folders.

    get_suite( )
        Get the ecfSuite object

    get_suite_name( )
        Returns the name of the suite

    get_task(task)
        Returns a specific task from the suite.

    add_edit(edit_dict, parent=None)
        Adds an edit to either a suite, task, or family. The parent defines
        what object will get the edit object.

    add_event(event, parent=None)
        Adds an event to the parent node. Events can only be associated with
        families or tasks so if the parent is None, nothing will be added.
        This was done to avoid errors.

    add_defstatus(defstatus, parent=None)
        Adds an defstatus to the parent node. Defstatus objects can only be
        associated with families or tasks so if the parent is None, nothing
        will be added. This was done to avoid errors.
    """

    def __init__(self, ecfsuite, ecfhome, build_tree=True):
        """
        Parameters
        ----------
        ecfhome : str
            The path to the base for the ecf items. This includes the ecf
            scripts repository and the storage location for all the suite
            script. In the default, it is generally assumed to be the ecfGFS
            parameter from the experiment setup.
        build_tree : bool
            A boolean that indicates if the application should build the
            folders and scripts as part of the run. If this is false, the
            scripts and folders are not created and assumed to already be
            in place.
        ecfsuite : str
            The name of the suite.
        """

        # Initialize environment
        self.ecfnodes = {}
        self.ecfhome = ecfhome
        self.build_tree = build_tree

        # Create initial suite
        self.ecfsuite = self.add_suite(ecfsuite)

    def add_suite(self, suite):
        """
        Creates the suite object and if necessary creates the base folders.

        Parameters
        ----------
        suite : str
            Name of the suite object.

        Returns
        -------
        new_suite : ecfSuite object
            An ecfSuite object
        """

        new_suite = ecfSuite(f"{suite}")
        if self.build_tree:
            new_suite.generate_folders(self.ecfhome)
        return new_suite

    def get_suite(self):
        """
        Get the ecfSuite object

        Returns
        -------
        ecfSuite
            The ecfsuite object that has all the contents
        """

        return self.ecfsuite

    def get_suite_name(self):
        """
        Returns the name of the suite

        Returns
        -------
        name : str
            The name of the suite.
        """

        return self.ecfsuite.name()

    def get_task(self, task):
        """
        Returns a specific task from the suite.

        Parameters
        ----------
        task : str
            The name of the task to lookup in the ecfnodes dictionary.

        Returns
        -------
        ecfTask
            An ecfTask that is an extension of the ecflow.task object.
        """

        return self.ecfnodes[task]

    def add_edit(self, edit_dict, parent=None):
        """
        Adds an edit to either a suite, task, or family. The parent defines
        what object will get the edit object.

        Parameters
        ----------
        edit_dict : dict
            Dictionary object that contains the edits in the form of
            {"edit" : "value"}
        parent : str
            String for the parent node that will get the edits added.
        """
        if parent:
            self.ecfnodes[parent] += ecflow.Edit(edit_dict)
        else:
            self.ecfsuite += ecflow.Edit(edit_dict)

    def add_event(self, event, parent=None):
        """
        Adds an event to the parent node. Events can only be associated with
        families or tasks so if the parent is None, nothing will be added.
        This was done to avoid errors.

        Parameters
        ----------
        event : str
            A string that is passed to the ecflow.Event object
        parent : str
            String for the parent node that will get the events added.
        """

        if parent:
            self.ecfnodes[parent] += ecflow.Event(event)

    def add_defstatus(self, defstatus, parent=None):
        """
        Adds an defstatus to the parent node. Defstatus objects can only be
        associated with families or tasks so if the parent is None, nothing
        will be added. This was done to avoid errors.

        Parameters
        ----------
        defstatus : str
            A string that is passed to the ecflow.Defstatus object
        parent : str
            String for the parent node that will get the defstatus added.
        """

        if parent:
            self.ecfnodes[parent] += ecflow.Defstatus(defstatus)

    def add_repeat(self, repeat, parent=None):
        repeat_token = re.search("(\d{8,10})( | to )(\d{10})( | by )(\d{1,2}:)?(\d{1,2}:\d{1,2})",repeat)
        start = repeat_token.group(1).strip()
        end = repeat_token.group(3).strip()
        byday = repeat_token.group(5).strip() if repeat_token.group(5) is not None else repeat_token.group(5)
        bytime = repeat_token.group(6).strip()

        startdate = datetime.strptime(start, "%Y%m%d%H") if len(start) == 10 else datetime.strptime(start, "%Y%m%d")
        enddate = datetime.strptime(end, "%Y%m%d%H")
        if byday is not None:
            delta = timedelta(days=int(byday.split(':')[0]),
                              hours=int(bytime.split(':')[0]),
                              minutes=int(bytime.split(':')[1]))
        else:
            delta = timedelta(hours=int(bytime.split(':')[0]),
                              minutes=int(bytime.split(':')[1]))

        total_runtime = enddate - startdate

        if parent:
            targetnode = self.ecfnodes[parent]
        else:
            targetnode = self.ecfsuite

        try:
            if total_runtime.total_seconds() < delta.total_seconds():
                raise ConfigurationError
        except ConfigurationError:
            if parent:
                print(f"Node: {parent} - "
                      "Repeat has a greater increment than total time.")
            else:
                print(f"Suite: {self.get_suite_name()} - " 
                      "Repeat has a greater increment than total time.")
            sys.exit(1)

        # Setup the start date.
        targetnode += ecflow.Date(f"{startdate.strftime('%d.%m.%Y')}")

        # If the dates are the same day, we only need a time string:
        if startdate.date() == enddate.date():
            deltahours, deltaminutes = delta.seconds // 3600, delta.seconds // 60 % 60
            time_string = f"{startdate.strftime('%H:%M')} {enddate.strftime('%H:%M')} {deltahours:02}:{deltaminutes:02}"
            targetnode += ecflow.Time(time_string)
        # If the days don't match up, we'll need to do some repeats.
        else:
            deltahours, deltaminutes = delta.seconds // 3600, delta.seconds // 60 % 60
            if delta.total_seconds() < 86400:
                position_time = startdate
                total_instances = 0
                while position_time <= enddate:
                    total_instances += 1
                    position_time = position_time + delta
                if len(start) == 10:
                    targetnode += ecflow.Time(f"{startdate.strftime('%H:%M')}")
                else:
                    targetnode += ecflow.Today(ecflow.TimeSlot(0, 0), True)
                targetnode += ecflow.Time(deltahours, deltaminutes, True)
                targetnode += ecflow.RepeatInteger("RUN", 1, total_instances)
            else:
                if deltahours == 0 and deltaminutes == 0:
                    position_time = startdate + delta
                    if len(start) == 10:
                        targetnode += ecflow.Time(f"{startdate.strftime('%H:%M')}")
                    else:
                        targetnode += ecflow.Time(00, 00, True)
                    while position_time <= enddate:
                        position_string = f"{position_time.strftime('%d.%m.%Y')}"
                        targetnode += ecflow.Date(position_string)
                        position_time = position_time + delta
                else:
                    position_time = startdate
                    while position_time <= enddate:
                        targetnode += ecflow.Cron(position_time.strftime('%H:%M'),
                                                  days_of_month=[int(position_time.strftime('%d'))],
                                                  months=[int(position_time.strftime('%m'))])
                        position_time = position_time + delta

    def add_trigger(self, trigger, parent, state=None, event=None, suite=None, suite_array=None, operand=None):
        if suite is not None:
            try:
                trigger_path = suite_array[suite].get_task(trigger).get_abs_node_path()
                if state is None and event is None:
                    addtrigger = ecflow.Trigger(f"{trigger_path} == complete" )
                elif state is not None and event is None:
                    addtrigger = ecflow.Trigger(f"{trigger_path} == {state}" )
                elif state is None and event is not None:
                    addtrigger = ecflow.Trigger(f"{trigger_path}:{event}" )
            except KeyError as e:
                print(f"Suite {suite} for task/trigger {parent}/{trigger}" 
                      " is not available. Please check the configuration file.")
                print("Error {e}")
                sys.exit(1)
        else:
            try:
                if state is None and event is None:
                    addtrigger = ecflow.Trigger([self.ecfnodes[trigger]])
                elif state is not None and event is None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    addtrigger = ecflow.Trigger(f"{trigger_path} == {state}")
                elif state is None and event is not None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    addtrigger = ecflow.Trigger(f"{trigger_path}:{event}")
            except KeyError as e:
                print(f"The task/trigger {parent}/{trigger} is not available in suite {self.get_suite_name()}." 
                      " Please check the configuration file.")
                print(f"Error {e}")
                sys.exit(1)
        if operand is not None:
            addTrigger = ecflow.Trigger(addtrigger.get_expression(), operand)
        self.ecfnodes[parent].add(addtrigger)

    def add_family(self, family, parents=None):
        family_name = f"{parents}>{family}" if parents else family

        # If the name already exists, the family already exists
        if family_name not in self.ecfnodes.keys():
            self.ecfnodes[family_name] = ecfFamily(family)
            if self.build_tree:
                self.ecfnodes[family_name].generate_folders(self.ecfhome, self.get_suite_name(), parents)

        if parents:
            self.ecfnodes[parents] += self.ecfnodes[family_name]
        else:
            self.ecfsuite += self.ecfnodes[family_name]

    def add_task(self, task, parents, scriptrepo, template=None):
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                if task_name not in self.ecfnodes.keys():
                    self.ecfnodes[task_name] = ecfTask(task_name)
                    self.ecfnodes[task_name].setup_script(scriptrepo, template)
                    if self.build_tree:
                        self.ecfnodes[task_name].generate_ecflow_task(self.ecfhome, self.get_suite_name(), parents)
                    self.ecfnodes[parents] += self.ecfnodes[task_name]
        else:
            if task not in self.ecfnodes.keys():
                self.ecfnodes[task] = ecfTask(task)
                self.ecfnodes[task].setup_script(scriptrepo, template)
                if self.build_tree:
                    self.ecfnodes[task].generate_ecflow_task(self.ecfhome, self.get_suite_name(), parents)
                self.ecfnodes[parents] += self.ecfnodes[task]

    def add_task_edits(self, task, edit_dict):
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
                        except ConfigurationError:
                            print(f"The listed array of {edit} exceeds the parent counter."
                                  " Please check the configuration file")
                            sys.exit(1)
                    else:
                        neweditvalue = editvalue
                    self.add_edit({edit: neweditvalue}, task_name)
                loop_index += 1
        else:
            for edit in edit_dict:
                self.add_edit({edit: edit_dict[edit]}, task)

    def add_task_repeat(self, task, repeat):
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                self.add_repeat(repeat, task_name)
        else:
            self.add_repeat(repeat, task)

    def add_task_defstatus(self, task, defstatus):
        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                self.add_defstatus(defstatus, task_name)
        else:
            self.add_defstatus(defstatus, task)

    def add_task_events(self, task, events):
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
                            self.add_event(event_name, task_name)
                        else:
                            event_counter = eventNode.get_range()
                            for event_number in event_counter:
                                event_name = f"{eventNode.get_full_name(event_number)}"
                                self.add_event(event_name, task_name)
                    elif eventNode.is_list:
                        try:
                            if len(eventNode.items) == len(taskNode.get_range()):
                                neweditvalue = f"{editNodeValue.get_full_name(loop_index)}"
                            else:
                                raise ConfigurationError
                        except ConfigurationError:
                            print(f"The listed array of {eventNode.get_name()} excceds the parent counter." 
                                  " Please check the configuration file")
                            sys.exit(1)

                    else:
                        self.add_event(event, task_name)
                loop_index += 1
        else:
            for event in events:
                eventNode = ecfEventNode(event)
                if eventNode.is_loop():
                    for event_number in eventNode.get_range():
                        event_name = f"{eventNode.get_full_name(event_number)}"
                        self.add_event(event_name, task)
                else:
                    self.add_event(event, task)

    def add_task_triggers(self, task, triggers, suite_array):

        def process_trigger(trigger_name, triggerTaskNode, task, task_loop_index=None,
                            total_tasks=None, task_number=None):
            if triggerTaskNode.has_suite():
                suite = triggerTaskNode.get_suite()
            else:
                suite = None
            operand = None
            if triggerTaskNode.has_operand():
                operand = bool(triggerTaskNode.get_operand())
            if triggerTaskNode.has_state():
                self.add_trigger(trigger_name, task, state=triggerTaskNode.get_state(), suite=suite,
                                 suite_array=suite_array, operand=operand)
            elif triggerTaskNode.has_event():
                if triggerTaskNode.is_event_loop():
                    if triggerTaskNode.has_event_max_value():
                        for event_count in triggerTaskNode.get_event_range():
                            event_name = f"{triggerTaskNode.get_event_full_name(event_count)}"
                            self.add_trigger(trigger_name, task, event=event_name, suite=suite,
                                             suite_array=suite_array, operand=operand)
                    elif triggerTaskNode.event_parent_counter:
                        event_name = f"{triggerTaskNode.get_event_full_name(task_number)}"
                        self.add_trigger(trigger_name, task, event=event_name, suite=suite,
                                         suite_array=suite_array, operand=operand)
                    else:
                        event_range = triggerTaskNode.get_event_range(max_value=total_tasks)
                        event_count = [*event_range]
                        event_name = f"{triggerTaskNode.get_event_full_name(event_count[task_loop_index])}"
                        self.add_trigger(trigger_name, task, event=event_name, suite=suite,
                                         suite_array=suite_array, operand=operand)
                else:
                    self.add_trigger(trigger_name, task, event=triggerTaskNode.get_event(), suite=suite,
                                     suite_array=suite_array, operand=operand)
            else:
                self.add_trigger(trigger_name, task, suite=suite,
                                 suite_array=suite_array, operand=operand)

        taskNode = ecfTaskNode(task)
        if taskNode.is_loop() or taskNode.is_list:
            task_loop_index = 0
            for task_number in taskNode.get_range():
                task_name = f"{taskNode.get_full_name(task_number)}"
                total_tasks = len(taskNode.get_range())
                for trigger in triggers:
                    triggerTaskNode = ecfTriggerNode(trigger)
                    if triggerTaskNode.is_loop() or triggerTaskNode.is_list:
                        if triggerTaskNode.is_list or triggerTaskNode.has_max_value():
                            for trigger_flag in triggerTaskNode.get_range():
                                trigger_name = f"{triggerTaskNode.get_full_name(trigger_flag)}"
                                process_trigger(trigger_name, triggerTaskNode, task_name,
                                                task_loop_index, total_tasks, task_number)
                        else:
                            trigger_range = triggerTaskNode.get_range(max_value=total_tasks)
                            trigger_count = [*trigger_range]
                            trigger_name = f"{triggerTaskNode.get_full_name(trigger_count[task_loop_index])}"
                            process_trigger(trigger_name, triggerTaskNode, task_name, task_loop_index,
                                            total_tasks, task_number)
                    else:
                        process_trigger(triggerTaskNode.get_name(), triggerTaskNode, task_name,
                                        task_loop_index, total_tasks, task_number)
                task_loop_index += 1
        else:
            for trigger in triggers:
                triggerTaskNode = ecfTriggerNode(trigger)
                if triggerTaskNode.is_loop():
                    try:
                        if triggerTaskNode.has_max_value():
                            for trigger_flag in triggerTaskNode.get_range():
                                trigger_name = f"{triggerTaskNode.get_full_name(trigger_flag)}"
                                process_trigger(trigger_name, triggerTaskNode, task)
                        else:
                            raise ConfigurationError
                    except ConfigurationError:
                        print(f"Task: {task} - Looping mechanism called without max value in a non looped task.")
                        sys.exit(1)
                else:
                    process_trigger(triggerTaskNode.get_name(), triggerTaskNode, task)


class ecfNode( ):

    def __init__(self, ecfItem):
        if isinstance(ecfItem, str):
            if re.search(r".*\(.*\).*", ecfItem):
                self.initial_count = None
                self.increment = None
                self.name = ecfItem
                self.is_list = False
            elif re.search(r".*\[.*\].*", ecfItem):
                self.initial_count = None
                self.increment = None
                self.name = ecfItem
                self.is_list = True
                self.use_parent_counter = False
                self.items = re.search(".*\[(.*)\].*",self.name).group(1).strip().split(',')
            else:
                self.name = ecfItem
                self.is_list = False
        elif isinstance(ecfItem, list):
            self.initial_count = None
            self.increment = None
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
        if re.search(r".*\(.*\).*", self.name):
            self.use_parent_counter = False
            range_token = re.search(".*\((.*)\).*", self.name).group(1).strip().split(',')
            range_functions.get(len(range_token), self.invalid_range)(range_token)
            return True
        else:
            return False

    def invalid_range(self, range_token):
        print(f"The range specified in {self.name} is out of bounds. Please review the configuration.")
        sys.exit(1)

    def set_max_value(self, range_token):
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

    def set_initial_increment_max_value(self, range_token):
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

    def get_full_name(self, counter=None):
        try:
            if re.search(r"\(.*\)", self.name):
                name_token = re.search("(.*)\(.*\)(.*)", self.name)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                if isinstance(counter, int):
                    return f"{base}{counter:03}{suffix}"
                elif isinstance(counter, str):
                    return f"{base}{counter}{suffix}"
            elif re.search(r"\[.*\]", self.name):
                name_token = re.search("(.*)\[.*\](.*)", self.name)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                array_item = self.items[counter]
                if isinstance(array_item, int):
                    return f"{base}{array_item:03}{suffix}"
                elif isinstance(array_item, str):
                    return f"{base}{array_item}{suffix}"
            elif self.is_list:
                array_item = self.items[counter]
                if isinstance(array_item, int):
                    return f"{array_item:03}"
                elif isinstance(array_item, str):
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

    def __init__(self, ecfItem):
        self.task_setup = ecfItem
        if isinstance(ecfItem['task'], str):
            if re.search(r".*\(.*\).*", ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = False
            elif re.search(r".*\[.*\].*", ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = True
                self.use_parent_counter = False
                self.items = re.search(".*\[(.*)\].*", ecfItem['task']).group(1).strip().split(',')
            else:
                self.name = ecfItem['task']
                self.is_list = False
        elif isinstance(ecfItem, list):
            self.name = ''
            self.is_list = True
            self.items = ecfItem['task']
        else:
            self.name = ecfItem['task']
            self.is_list = False

    def get_type(self):
        return 'trigger'

    def has_operand(self):
        if 'operand' in self.task_setup.keys():
            self.operand = self.task_setup['operand']
            return True
        else:
            return False

    def get_operand(self):
        return self.operand

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
                    self.is_event_list = False
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
        self.event_increment = None
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
        self.event_increment = None
        if not range_token[1]:
            self.event_max_value = None
        else:
            try:
                self.event_max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.event_string} is not an integer")
                sys.exit(1)

    def set_event_initial_increment_max_value(self,range_token):
        try:
            self.event_initial_count = None if not range_token[0] else int(range_token[0])
            self.event_increment = None if not range_token[2] else int(range_token[2])
        except TypeError:
            print(f"Initial cound and increment values for {self.event_string} are not integers")
            sys.exit(1)
        if not range_token[1]:
            self.event_max_value = None
        else:
            try:
                self.event_max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.event_string} is not an integer")
                sys.exit(1)

    def get_event_full_name(self, counter=None):
        try:
            if re.search(r"\(.*\)", self.event_string):
                name_token = re.search("(.*)\(.*\)(.*)", self.event_string)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                if isinstance(counter, int):
                    return f"{base}{counter:03}{suffix}"
                elif isinstance(counter, str):
                    return f"{base}{counter}{suffix}"
            elif re.search(r"\[.*\]", self.event_string):
                name_token = re.search("(.*)\[.*\](.*)", self.event_string)
                base = name_token.group(1).strip()
                suffix = name_token.group(2).strip()
                array_item = self.event_items[counter]
                if isinstance(array_item, int):
                    return f"{base}{array_item:03}{suffix}"
                elif isinstance(array_item, str):
                    return f"{base}{array_item}{suffix}"
            elif self.is_list:
                array_item = self.event_items[counter]
                if isinstance(array_item, int):
                    return f"{array_item:03}"
                elif isinstance(array_item, str):
                    return f"{array_item}"
            else:
                return self.event_string
        except ValueError as err:
            print(f"Problem getting full name of {self.event_string}. Error: {err}")

    def has_event_max_value(self):
        return True if self.event_max_value is not None else False

    def get_event_max_value(self):
        return self.event_max_value

    def get_event_range(self, initial_count=0, increment=1, max_value=1):
        if self.is_event_list:
            return range(initial_count, len(self.event_items), increment)
        else:
            if self.event_initial_count is not None:
                initial_count = self.event_initial_count
            if self.event_increment is not None:
                increment = self.event_increment
            if self.event_max_value is not None:
                max_value = self.event_max_value
            max_value = (max_value * increment) + initial_count
            return range(initial_count, max_value, increment)

class ecfEventNode(ecfNode):

    def get_type(self):
        return 'edit'

class ecfEditNode(ecfNode):

    def get_type(self):
        return 'edit'

class ecfRoot( ):

    def get_base_name():
        return re.search("(.*)\{.*\}",self.name()).group(1).strip()

class ecfSuite(ecflow.Suite, ecfRoot):

    def generate_folders(self,ecfhome):
        folder_path = f"{ecfhome}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfFamily(ecflow.Family, ecfRoot):

    def generate_folders(self,ecfhome,suite,parents):
        if parents:
            folder_path = f"{ecfhome}/{suite}/{parents.replace('>','/')}/{self.name()}"
        else:
            folder_path = f"{ecfhome}/{suite}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfTask(ecflow.Task, ecfRoot):

    def setup_script(self,repopath,template):
        self.scriptrepo = repopath
        self.template = template

    def generate_ecflow_task(self,ecfhome,suite,parents):
        if self.template == "skip":
            return
        script_name = f"{self.name()}.ecf"
        ecfscript = None
        search_script = f"{self.template}.ecf" if self.template is not None else script_name
        if parents:
            script_path = f"{ecfhome}/{suite}/{parents.replace('>','/')}/{script_name}"
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
            print(f"Could not find the script {search_script}. Exiting build")
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
