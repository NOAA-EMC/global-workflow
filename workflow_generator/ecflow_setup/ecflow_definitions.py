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

    add_repeat(repeat, parent=None)
        Adds in a repeat to the parent node. Repeats can be parts of a family,
        task, or suite. If the parent is none it will be added to the suite.

    add_trigger(trigger, parent, state=None, event=None, suite=None,
                suite_array=None, operand=None)
        Adds a trigger to the parent node. Triggers can be added to families
        and tasks.

    add_family(family, parents=None)
        Adds a family to the suite. If the parents value is set to none, then
        it will be added as a top level family. Otherwise, it will be added as
        a sub-family to the parents.

    add_task(task, parents, scriptrepo, template=None)
        Adds a task to the parent node. If the build is set to true then the
        method also calls the creation method in the ecfTask class to deploy
        the script to the proper location. The script repo is where it will
        look for the script. If template is set, it will look for that template
        and then copy and change the name of the template at the destination to
        the name of the task.

    add_task_edits(task, edit_dict)
        Adds edits to a task. This takes in the edit_dict and then calls the
        add_edit method to apply them to that task.

    add_task_repeat(task, repeat)
        Adds a repeats to task nodes. This function primarily breaks down the
        tasks into lists or ranges based on the task string and then adds the
        repeat to the breakout.

    add_task_defstatus(task, defstatus)
        Adds a defstatus to a task node. This function breaks down the task
        string into a range or list if necessary and then adds the calls the
        add_defstatus method.

    add_task_events(task, events)
        Adds events to a task. This function breaks down the task string into
        ranges or lists if necessary but also breaks down the events if those
        are a list or range. It then passes the fully formed pieces to the
        add_event method to add them to the suite.

    add_task_triggers(task, triggers, suite_array)
        Adds triggers to a task. This is a fairly complex method and might be
        able to be broken into smaller pieces at some point. The triggers
        can be loops in themselves, based on a task with an event or a loop of
        events. Or even a loop of other tasks from other suites. This function
        breaks down the tasks themselves and then also any loop/list logic that
        exists within the trigger and applies them to the task with the
        add_trigger method.
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

        Returns
        -------
        None
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

        Returns
        -------
        None
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

        Returns
        -------
        None
        """

        if parent:
            self.ecfnodes[parent] += ecflow.Defstatus(defstatus)

    def add_repeat(self, repeat, parent=None):
        """
        Adds in a repeat to the parent node. Repeats can be parts of a family,
        task, or suite. If the parent is none it will be added to the suite.

        This will calculate the difference between the two dates and use the
        interval value from the third entry to identify how often. Due to the
        fact that ecflow has a very simplistic time/date/interval
        implementation, this function can render the dates in multiple
        different fashions.

        If the start and end are the same day, it'll just use a time set. If
        it is different days, it'll do a relative time set with the dates and
        also a start time. If it is multiple dates it will throw in repeats
        based on relative values.

        Parameters
        ----------
        repeat : str
            This is a date string in the format of YYYYMMDDHH to YYYYMMDDHH by
            DD:HH:MM. The hours on the second date string are optional as are
            the day parameters in the time string.
        parent : str

        Returns
        -------
        None
        """

        repeat_token = re.search(
            "(\d{8,10})( | to )(\d{10})( | by )(\d{1,2}:)?(\d{1,2}:\d{1,2})",
            repeat)
        start = repeat_token.group(1).strip()
        end = repeat_token.group(3).strip()
        byday = repeat_token.group(5).strip() if repeat_token.group(5) is not \
                                                 None else repeat_token.group(5)
        bytime = repeat_token.group(6).strip()

        startdate = datetime.strptime(start, "%Y%m%d%H") if len(start) == 10 \
            else datetime.strptime(start, "%Y%m%d")
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
            time_string = (f"{startdate.strftime('%H:%M')} "
                           f"{enddate.strftime('%H:%M')} "
                           f"{deltahours:02}:{deltaminutes:02}")
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

    def add_trigger(self, trigger, parent, state=None, event=None, suite=None,
                    suite_array=None, operand=None):
        """
        Adds a trigger to the parent node. Triggers can be added to families
        and tasks.

        Parameters
        ----------
        trigger : str
            The trigger string to add to the parent node.
        parent : str
            The parent node that will accept the trigger
        state : str
            The state of the trigger. Generally looking for complete, active,
            or queued.
        event : str
            If there is an event associated with a task, this will add it to
            the trigger definition.
        suite : str
            If the trigger is looking outside the current suite, this will
            pull in the details from the other suites and attach the trigger.
        suite_array : dict
            This is the array of suites in the event that the suite value is
            populated, the details of the suite need to be made available to
            the function
        operand : bool
            This is a true/false value that is looking to define if the trigger
            is an AND or an OR. If it is TRUE it is an AND, if it is FALSE, it
            is an OR.

        Returns
        -------
        None
        """

        if suite is not None:
            try:
                trigger_path = suite_array[suite].get_task(trigger).get_abs_node_path()
                if state is None and event is None:
                    add_trigger = ecflow.Trigger(f"{trigger_path} == complete")
                elif state is not None and event is None:
                    add_trigger = ecflow.Trigger(f"{trigger_path} == {state}")
                elif state is None and event is not None:
                    add_trigger = ecflow.Trigger(f"{trigger_path}:{event}")
            except KeyError as e:
                print(f"Suite {suite} for task/trigger {parent}/{trigger}" 
                      " is not available. Please check the configuration file.")
                print("Error {e}")
                sys.exit(1)
        else:
            try:
                if state is None and event is None:
                    add_trigger = ecflow.Trigger([self.ecfnodes[trigger]])
                elif state is not None and event is None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    add_trigger = ecflow.Trigger(f"{trigger_path} == {state}")
                elif state is None and event is not None:
                    trigger_path = self.ecfnodes[trigger].get_abs_node_path()
                    add_trigger = ecflow.Trigger(f"{trigger_path}:{event}")
            except KeyError as e:
                print(f"The node/trigger {parent}/{trigger} is not available "
                      f"in suite {self.get_suite_name()}." 
                      " Please check the configuration file.")
                print(f"Error {e}")
                sys.exit(1)
        if operand is not None:
            add_trigger = ecflow.Trigger(add_trigger.get_expression(), operand)
        self.ecfnodes[parent].add(add_trigger)

    def add_family(self, family, parents=None):
        """
        Adds a family to the suite. If the parents value is set to none, then
        it will be added as a top level family. Otherwise, it will be added as
        a sub-family to the parents.

        Parameters
        ----------
        family : str
            The name of the family that is to be added to the suite.
        parents : str
            The string representation of the parent nodes that the family needs
            to be added to.

        Returns
        -------
        None
        """

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
        """
        Adds a task to the parent node. If the build is set to true then the
        method also calls the creation method in the ecfTask class to deploy
        the script to the proper location. The script repo is where it will
        look for the script. If template is set, it will look for that template
        and then copy and change the name of the template at the destination to
        the name of the task.

        Parameters
        ----------
        task : str
            The name of the task
        parents : str
            The name of the parent nodes to get the task
        scriptrepo : str
            File path to the script repository to look for the task.ecf scripts
        template : str
            Name of the template file to use instead of searching for the name
            of the task in the script repo.

        Returns
        -------
        None
        """

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            for task_number in task_node.get_range():
                task_name = f"{task_node.get_full_name(task_number)}"
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
        """
        Adds edits to a task. This takes in the edit_dict and then calls the
        add_edit method to apply them to that task.

        This function also breaks apart any lists or ranges that are passed in
        to the tasks and applies it to all of them. It also applies any loop
        logic that is applied to the parent task to the edits themselves.

        Parameters
        ----------
        task : str
            The name of the task. Can also include a list or range object in
            the string.
        edit_dict : dict
            A dictionary of the edits that are to be applied to the tasks.

        Returns
        -------
        None
        """

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            loop_index = 0
            for task_number in task_node.get_range():
                task_name = f"{task_node.get_full_name(task_number)}"
                for edit, editvalue in edit_dict.items():
                    editNodeValue = ecfEditNode(editvalue)
                    if editNodeValue.is_loop():
                        if editNodeValue.use_parent_counter:
                            neweditvalue = f"{editNodeValue.get_full_name(task_number)}"
                        else:
                            total_tasks=len(task_node.get_range())
                            edit_range = editNodeValue.get_range(max_value=total_tasks)
                            edit_count = [*edit_range]
                            neweditvalue = f"{editNodeValue.get_full_name(edit_count[loop_index])}"
                    elif editNodeValue.is_list:
                        try:
                            if len(editNodeValue.items) == len(task_node.get_range()):
                                neweditvalue = f"{editNodeValue.get_full_name(loop_index)}"
                            else:
                                raise ConfigurationError
                        except ConfigurationError:
                            print(f"The listed array of {edit} " 
                                  "exceeds the parent counter."
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
        """
        Adds a repeats to task nodes. This function primarily breaks down the
        tasks into lists or ranges based on the task string and then adds the
        repeat to the breakout.

        Parameters
        ----------
        task : str
            The name of the task or list/range of tasks to add the repeat.
        repeat : str
            The repeat string to be passed to the add_repeat method.

        Returns
        -------
        None
        """

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            for task_number in task_node.get_range():
                task_name = f"{task_node.get_full_name(task_number)}"
                self.add_repeat(repeat, task_name)
        else:
            self.add_repeat(repeat, task)

    def add_task_defstatus(self, task, defstatus):
        """
        Adds a defstatus to a task node. This function breaks down the task
        string into a range or list if necessary and then adds the calls the
        add_defstatus method.

        Parameters
        ----------
        task : str
            The task string to add the defstatus pieces to. Can be a range or
            list as well.
        defstatus : str
            String that represents the defstatus, like complete.

        Returns
        -------
        None
        """

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            for task_number in task_node.get_range():
                task_name = f"{task_node.get_full_name(task_number)}"
                self.add_defstatus(defstatus, task_name)
        else:
            self.add_defstatus(defstatus, task)

    def add_task_events(self, task, events):
        """
        Adds events to a task. This function breaks down the task string into
        ranges or lists if necessary but also breaks down the events if those
        are a list or range. It then passes the fully formed pieces to the
        add_event method to add them to the suite.

        Parameters
        ----------
        task : str
            The task string to add the event to.
        events : str
            The events string that will be added to the task.

        Returns
        -------
        None
        """

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            for task_number in task_node.get_range():
                loop_index = 0
                task_name = f"{task_node.get_full_name(task_number)}"
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
                            if len(eventNode.items) == len(task_node.get_range()):
                                event_name = f"{eventNode.get_full_name(loop_index)}"
                                self.add_event(event_name, task_name)
                            else:
                                raise ConfigurationError
                        except ConfigurationError:
                            print(f"The listed array of {eventNode.get_name()} "
                                  "exceeds the parent counter." 
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
        """
        Adds triggers to a task. This is a fairly complex method and might be
        able to be broken into smaller pieces at some point. The triggers
        can be loops in themselves, based on a task with an event or a loop of
        events. Or even a loop of other tasks from other suites. This function
        breaks down the tasks themselves and then also any loop/list logic that
        exists within the trigger and applies them to the task with the
        add_trigger method.

        Parameters
        ----------
        task : str
            The task string, list, range or static, that is to be broken down
            and then the triggers applied.
        triggers : dict
            The dictionary of triggers to add to the task.
        suite_array : dict
            In case the triggers are from another suite, this calls the trigger
            from the other suite.

        Methods
        -------
        process_trigger(trigger_name, triggerTaskNode, task,
                        task_loop_index=None, total_tasks=None,
                        task_number=None)
            Since processing the triggers for each of the tasks has a lot of
            repetative break downs, it is useful to create a method within
            the add_task_trigger method to reduce the repetative code.

        Returns
        -------
        None
        """

        def process_trigger(trigger_name, triggerTaskNode, task,
                            task_loop_index=None, total_tasks=None,
                            task_number=None):
            """
            Since processing the triggers for each of the tasks has a lot of
            repetative break downs, it is useful to create a method within
            the add_task_trigger method to reduce the repetative code.

            Parameters
            ----------
            trigger_name : str
                The name of the trigger
            triggerTaskNode : ecfTaskNode
                This is the full object of the task. Needed because the
                methods of that class allow this method to identify if there
                are any events, loops, etc.
            task : str
                The task to add the triggers to.
            task_loop_index : int
                If the parent task is a list/loop, this is the current index
                value to it can be a 1 to 1 match. I.E. the second value of the
                list in the task uses the second value of the list in the
                trigger.
            total_tasks : int
                This is used in the event that the task is a range and can be
                used with the range function in the ecfTaskNode class to get
                the appropriate values
            task_number : int
                The task ID number to get the full name of the task in the
                event that the task is a list.

            Returns
            -------
            None
            """

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

        task_node = ecfTaskNode(task)
        if task_node.is_loop() or task_node.is_list:
            task_loop_index = 0
            for task_number in task_node.get_range():
                task_name = f"{task_node.get_full_name(task_number)}"
                total_tasks = len(task_node.get_range())
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
                        print(f"Task: {task} - Looping mechanism called "
                              "without max value in a non looped task.")
                        sys.exit(1)
                else:
                    process_trigger(triggerTaskNode.get_name(), triggerTaskNode, task)


class ecfNode():
    """
    This is the base class for the other classes that are used to identify any
    loops, lists, or what the item might be and also assign the name to the
    object. This reduces the overhead for code and also makes it easier to
    add in additional node type objects. Most of the objects extend this class
    so this one is the main functions that apply to all node types.

    Attributes
    ----------
    initial_count : int
        In the event that the node is a range this value will hold the initial
        count value for the object.
    increment : int
        In the event that the node is a range or list, this holds the amount
        to increment the counter.
    max_value : int
        In the event that the node is a range or list, this holds the max value
        associated with it.
    name : str
        Name of the object.
    is_list : bool
        If the node contains the [ ] list syntax. True if it does, false
        otherwise.
    use_parent_counter : bool
        If the node use a list or range syntax but has no internal values,
        indicating that it should use the range of the parent node.

    Methods
    -------
    get_name()
        Returns the name of the node.

    is_loop()
        Checks to see if the ecfNode is a loop. If it is, this function also
        calls the supporting functions to set the range values, if there is
        a max, min, interval, or list.

    invalid_range()
        Helper function to ensure that the range is valid. Exits if it is not.

    set_max_value(range_token)
        The range token is passed in and if only one value is set in the range
        then it is set to max value and the initial is set to 0 and the
        interval is set to 1.

    set_initial_max_value(range_token)
        If the range token is passed two parameters, they are assumed to be
        the initial and max values. This sets those values for the node. The
        interval is set to 1.

    set_initial_increment_max_value(range_token)
        If three values are sent in through the range token, this sets the max,
        initial, and increment values.

    get_full_name(counter=None)
        This method uses the counter object if the item is a list to identify
        the position in a list, the item in the range or if there is no counter
        associated with it, the base name.

    has_max_value()
        Returns true if the node object range has a maximum value.

    get_max_value()
        Returns the maximum value for the node.

    get_range(initial_count=0, increment=1, max_value=1)
        If the node has a list or range associated with it, this returns the
        range of items or the range of the array.
    """

    def __init__(self, ecfItem):
        """
        Parameters
        ----------
        ecfItem : str
            Name of the ecfNode item. If it contains a range or list
            identifier, the other values are populated to identify what kind
            of node it is.
        """

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
        """
        Returns the name of the node.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The name in string format.
        """

        return self.name

    def is_loop(self):
        """
        Checks to see if the ecfNode is a loop. If it is, this function also
        calls the supporting functions to set the range values, if there is
        a max, min, interval, or list.

        The range is split into a tokenized array.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            True if the node is a loop format defined by ( ).
        """

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

    def invalid_range(self):
        """
        Helper function to ensure that the range is valid. Exits if it is not.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        print(f"The range specified in {self.name} is out of bounds. "
              "Please review the configuration.")
        sys.exit(1)

    def set_max_value(self, range_token):
        """
        The range token is passed in and if only one value is set in the range
        then it is set to max value and the initial is set to 0 and the
        interval is set to 1.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

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

    def set_initial_max_value(self, range_token):
        """
        If the range token is passed two parameters, they are assumed to be
        the initial and max values. This sets those values for the node. The
        interval is set to 1.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

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
        """
        If three values are sent in through the range token, this sets the max,
        initial, and increment values.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

        try:
            self.initial_count = None if not range_token[0] else int(range_token[0])
            self.increment = None if not range_token[2] else int(range_token[2])
        except TypeError:
            print(f"Initial count and increment values for {self.name} "
                  "are not integers")
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
        """
        This method uses the counter object if the item is a list to identify
        the position in a list, the item in the range or if there is no counter
        associated with it, the base name.

        Parameters
        ----------
        counter : str or int
            If it is a str, returns the list item in that position. If it is
            an int, then return the counter position for it.

        Returns
        -------
        None
        """

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
        """
        Returns true if the node object range has a maximum value.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            True if there is a max value, false otherwise.
        """

        return True if self.max_value is not None else False

    def get_max_value(self):
        """
        Returns the maximum value for the node.

        Parameters
        ----------
        None

        Returns
        -------
        int
            The max value if one is set.
        """

        return self.max_value

    def get_range(self, initial_count=0, increment=1, max_value=1):
        """
        If the node has a list or range associated with it, this returns the
        range of items or the range of the array.

        Parameters
        ----------
        initial_count : int
            The initial count which is defaulted to 1 in case it wasn't defined
        increment : int
            The increment value to use for the range in case it wasn't defined
        max_value : int
            The maximum value for the range.

        Returns
        -------
        None
        """

        if self.is_list:
            return range(initial_count, len(self.items), increment)
        else:
            if self.initial_count is not None:
                initial_count = self.initial_count
            if self.increment is not None:
                increment = self.increment
            if self.max_value is not None:
                max_value = self.max_value
            max_value = (max_value * increment) + initial_count
            return range(initial_count, max_value, increment)


class ecfTaskNode(ecfNode):
    """
    Extension class for the ecfNodes to identify tasks.

    Methods
    -------
    get_type()
        Returns that this node is a task type.
    """

    def get_type(self):
        """
        Returns that this node is a task type.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The value of task to identify the node is a task.
        """

        return 'task'


class ecfTriggerNode(ecfNode):
    """
    Extension class for the ecfNodes to identify triggers. Overloads the
    constructors since triggers can have multiple levels within themselves
    for events and such.

    Attributes
    ----------
    name : str
    is_list : bool
    items : dict or array
    use_parent_counter : bool
    operand : str

    Methods
    -------
    get_type()
        Returns that this node is a trigger type.

    has_operand()
        If the trigger has an operand to indciate if it needs to be added as an
        OR or AND in the trigger statement, set the value and return True,
        otherwise false.

    get_operand()
        Returns the operand associated with the trigger.

    get_state()
        Returns the state associated with the trigger if one was defined.

    get_event()
        Returns the event_string associated with the trigger if one was defined

    has_suite()
        If a suite was passed in as part of the parameters in the keys, this
        returns True and sets the suite attribute to the suite name.

    get_suite()
        Returns the suite name.

    has_state()
        If a state was passed in with the YAML parameters, return true and set
        the state attribute to the state of the trigger.

    get_state()
        Returns the state for the trigger.

    has_event()
        If the trigger has an event associated with it, it is possible that the
        event has a loop. This method determines if the trigger has an event
        and if it does identifies the event string and items associated with it
        so that it can be used in other functions later. If it does have the
        loop or list identifiers then it returns true, otherwise false.

    is_event_loop()
        If the event that exists as part of the trigger is a loop,
        this breaks down the event loop into the appropriate
        values and returns true. Otherwise returns false.

    invalid_event_range()
        Helper method to exit the application if the event range is invalid.

    set_event_max_value(range_token)
        The range token is passed in and if only one value is set in the range
        then it is set to max value and the initial is set to 0 and the
        interval is set to 1.

    set_event_initial_max_value(range_token)
        If the range token is passed two parameters, they are assumed to be
        the initial and max values. This sets those values for the node. The
        interval is set to 1.

    set_event_initial_increment_max_value(range_token)
        If three values are sent in through the range token, this sets the max,
        initial, and increment values.

    get_event_full_name(counter=None)
        This method uses the counter object if the item is a list to identify
        the position in a list, the item in the range or if there is no counter
        associated with it, the base name.

    has_event_max_value()
        Returns true if the node object range has a maximum value.

    get_event_max_value()
        Returns the maximum value for the node.

    get_event_range(initial_count=0, increment=1, max_value=1)
        If the node has a list or range associated with it, this returns the
        range of items or the range of the array.
    """

    def __init__(self, ecfItem):
        """
        Parameters
        ----------
        ecfItem : dict or str
            A dictionary or string item that represents the current node.
        """
        self.task_setup = ecfItem
        if isinstance(ecfItem['task'], str):
            if re.search(r".*\(.*\).*", ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = False
            elif re.search(r".*\[.*\].*", ecfItem['task']):
                self.name = ecfItem['task']
                self.is_list = True
                self.use_parent_counter = False
                self.items = re.search(".*\[(.*)\].*",
                                       ecfItem['task']).group(1).strip().split(',')
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
        """
        Returns that this node is a trigger type.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The value trigger to identify that this is a trigger node.
        """

        return 'trigger'

    def has_operand(self):
        """
        If the trigger has an operand to indciate if it needs to be added as an
        OR or AND in the trigger statement, set the value and return True,
        otherwise false.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            True if there is an operand associated with the trigger.
        """

        if 'operand' in self.task_setup.keys():
            self.operand = self.task_setup['operand']
            return True
        else:
            return False

    def get_operand(self):
        """
        Returns the operand associated with the trigger.

        Parameters
        ----------
        None

        Returns
        -------
        str or bool
            Return the operand.
        """

        return self.operand

    def get_state(self):
        """
        Returns the state associated with the trigger if one was defined.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The state in string format.
        """

        return self.state

    def get_event(self):
        """
        Returns the event_string associated with the trigger if one was defined

        Parameters
        ----------
        None

        Returns
        -------
        str
            The event in string format.
        """

        return self.event_string

    def has_suite(self):
        """
        If a suite was passed in as part of the parameters in the keys, this
        returns True and sets the suite attribute to the suite name.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            If there is a suite associated with the trigger, return true
            otherwise false.
        """
        if 'suite' in self.task_setup.keys():
            self.suite = self.task_setup['suite']
            return True
        else:
            return False

    def get_suite(self):
        """
        Returns the suite name.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The name of the suite in string format.
        """
        return self.suite

    def has_state(self):
        """
        If a state was passed in with the YAML parameters, return true and set
        the state attribute to the state of the trigger.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            Returns true if there is a state value in the keys, otherwise
            false.
        """
        if 'state' in self.task_setup.keys():
            self.state = self.task_setup['state']
            return True
        else:
            return False

    def has_event(self):
        """
        If the trigger has an event associated with it, it is possible that the
        event has a loop. This method determines if the trigger has an event
        and if it does identifies the event string and items associated with it
        so that it can be used in other functions later. If it does have the
        loop or list identifiers then it returns true, otherwise false.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            Returns true if the trigger has an event in either list or
            string format.
        """
        if 'event' in self.task_setup.keys():
            if isinstance(self.task_setup['event'], str):
                if re.search(r".*\(.*\).*", self.task_setup['event']):
                    self.event_string = self.task_setup['event']
                    self.is_event_list = False
                elif re.search(r".*\[.*\].*", self.task_setup['event']):
                    self.event_string = self.task_setup['event']
                    self.is_event_list = True
                    self.items = re.search(".*\[(.*)\].*",
                                           self.task_setup['event']).group(1).strip().split(',')
                else:
                    self.event_string = self.task_setup['event']
                    self.is_event_list = False
            elif isinstance(self.task_setup['event'], list):
                self.is_event_list = True
                self.event_items = self.task_setup['event']
            else:
                self.event_string = self.task_setup['event']
                self.is_event_list = False
            return True
        else:
            return False

    def is_event_loop(self):
        """
        If the event that exists as part of the trigger is a loop,
        this breaks down the event loop into the appropriate
        values and returns true. Otherwise returns false.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            If there is an event loop, returns true, otherwise false.
        """
        range_functions = {
            1: self.set_event_max_value,
            2: self.set_event_initial_max_value,
            3: self.set_event_initial_increment_max_value,
        }
        if re.search(r"\(.*\)", self.event_string):
            self.event_parent_counter = False
            range_token = re.search(".*\((.*)\).*", self.event_string).group(1).strip().split(',')
            range_functions.get(len(range_token), self.invalid_range)(range_token)
            return True
        else:
            return False

    def invalid_event_range(self):
        """
        Helper method to exit the application if the event range is invalid.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        print(f"The range specified in {self.name} is out of bounds. "
              "Please review the configuration.")
        sys.exit(1)

    def set_event_max_value(self, range_token):
        """
        The range token is passed in and if only one value is set in the range
        then it is set to max value and the initial is set to 0 and the
        interval is set to 1.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

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

    def set_event_initial_max_value(self, range_token):
        """
        If the range token is passed two parameters, they are assumed to be
        the initial and max values. This sets those values for the node. The
        interval is set to 1.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

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

    def set_event_initial_increment_max_value(self, range_token):
        """
        If three values are sent in through the range token, this sets the max,
        initial, and increment values.

        Parameters
        ----------
        range_token : array
            The range token from the is_loop method.

        Returns
        -------
        None
        """

        try:
            self.event_initial_count = None if not range_token[0] else int(range_token[0])
            self.event_increment = None if not range_token[2] else int(range_token[2])
        except TypeError:
            print(f"Initial cound and increment values for "
                  f"{self.event_string} are not integers")
            sys.exit(1)
        if not range_token[1]:
            self.event_max_value = None
        else:
            try:
                self.event_max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.event_string} "
                      "is not an integer")
                sys.exit(1)

    def get_event_full_name(self, counter=None):
        """
        This method uses the counter object if the item is a list to identify
        the position in a list, the item in the range or if there is no counter
        associated with it, the base name.

        Parameters
        ----------
        counter : str or int
            If it is a str, returns the list item in that position. If it is
            an int, then return the counter position for it.

        Returns
        -------
        None
        """

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
            print(f"Problem getting full name of {self.event_string}. "
                  "Error: {err}")

    def has_event_max_value(self):
        """
        Returns true if the node object range has a maximum value.

        Parameters
        ----------
        None

        Returns
        -------
        bool
            True if there is an event max value associated with the trigger.
        """
        return True if self.event_max_value is not None else False

    def get_event_max_value(self):
        """
        Returns the maximum value for the node.

        Parameters
        ----------
        None

        Returns
        -------
        int
            The max value associated with the event.
        """
        return self.event_max_value

    def get_event_range(self, initial_count=0, increment=1, max_value=1):
        """
        If the event with the trigger has a list or range associated with it,
        this returns the range of items or the range of the array.

        Parameters
        ----------
        initial_count : int
            The initial count which is defaulted to 1 in case it wasn't defined
        increment : int
            The increment value to use for the range in case it wasn't defined
        max_value : int
            The maximum value for the range.

        Returns
        -------
        range
            The range of items within the event. If it is a list it uses the
            total number of items in the list, otherwise it pulls the values
            defined in the YAML.
        """

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
    """
    Extension class for the ecfNodes to identify events.

    Methods
    -------
    get_type()
        Returns that this node is an event type.
    """

    def get_type(self):
        """
        Returns that this node is an event type.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The string event to identify this as an event node.
        """
        return 'event'

class ecfEditNode(ecfNode):
    """
    Extension class for the ecfNodes to identify edits.

    Methods
    -------
    get_type()
        Returns that this node is an edit type.
    """

    def get_type(self):
        """
        Returns that this node is an edit type.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The string edit to identify this as an edit node.
        """

        return 'edit'

class ecfRoot( ):
    """
    A root level class that is not an ecfNode object from above but an
    object that will extend a class from the ecflow module.

    Methods
    -------
    get_base_name()
        Returns the prefix to a node.
    """

    def get_base_name():
        """
        Returns the prefix to a node.
        * Not currently in use, may be removed at a later date.

        Parameters
        ----------
        None

        Returns
        -------
        str
            The name of the node if it has a prefix, this strips out the
            surrounding range and just returns the beginning.
        """
        return re.search("(.*)\{.*\}",self.name()).group(1).strip()

class ecfSuite(ecflow.Suite, ecfRoot):
    """
    Extends the ecfRoot and ecflow.Suite classes to provide an additional
    function when defining the suite that also it can generate the folders
    for the suite and populate the families/tasks.

    Methods
    -------
    generate_folders(ecfhome)
        This function uses the ecfhome directory as a base and if it doesn't
        exist makes the suite folder at the ecfhome.
    """

    def generate_folders(self, ecfhome):
        """
        This function uses the ecfhome directory as a base and if it doesn't
        exist makes the suite folder at the ecfhome.

        Parameters
        ----------
        ecfhome : str
            Path to the root level directory for the ecfhome.

        Returns
        -------
        None
        """

        folder_path = f"{ecfhome}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfFamily(ecflow.Family, ecfRoot):
    """
    Extends the ecflow.Family and ecfRoot classes to provide the folder
    generation structure for families at the ecfhome location.

    Methods
    -------
    generate_folders(ecfhome,suite,parents)
        Uses the ecfhome as the root, then looks in the suite directory to
        determine if the family name has been created. It also splits out the
        parent folders to put everything in the proper tier.
    """

    def generate_folders(self, ecfhome, suite, parents):
        """
        Uses the ecfhome as the root, then looks in the suite directory to
        determine if the family name has been created. It also splits out the
        parent folders to put everything in the proper tier.

        Parameters
        ----------
        ecfhome : str
            The root level directory as a string
        suite : str
            The suite name to be appended to the ecfhome.
        parents : str
            Any of the parent families to ensure that the folder structure is
            setup correctly.

        Returns
        -------
        None
        """
        if parents:
            folder_path = f"{ecfhome}/{suite}/{parents.replace('>','/')}/{self.name()}"
        else:
            folder_path = f"{ecfhome}/{suite}/{self.name()}"
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)

class ecfTask(ecflow.Task, ecfRoot):
    """
    Extends the ecflow.Task and ecfRoot classes to allow the task scripts to
    be defined and then also created. If there is a template associated with
    the task, it will use that to create the script name in the appropriate
    location.

    Methods
    -------
    setup_script(repopath,template)
        Sets the parameters for the script if there is a repo path for the
        script repo that isn't the default and template if that is also
        defined for a task.

    generate_ecflow_task(ecfhome,suite,parents)
        Uses the parameters passed in to define the folder path and then
        looks in the script repository for the task name with a .ecf suffix or
        template name with a .ecf suffix and then copies that script content
        from the script repo over to the destination provided by the parameters
    """

    def setup_script(self, repopath, template):
        """
        Sets the parameters for the script if there is a repo path for the
        script repo that isn't the default and template if that is also
        defined for a task.

        Parameters
        ----------
        scriptrepo : str
            Path to the script repository used to populate the destination.
        template : str
            The template script if needed so the application will use that
            instead of searching for the task name in the script repo.

        Returns
        -------
        None
        """
        self.scriptrepo = repopath
        self.template = template

    def generate_ecflow_task(self, ecfhome, suite, parents):
        """
        Uses the parameters passed in to define the folder path and then
        looks in the script repository for the task name with a .ecf suffix or
        template name with a .ecf suffix and then copies that script content
        from the script repo over to the destination provided by the parameters

        Parameters
        ----------
        ecfhome : str
            Path to the root level directory to place the scripts.
        suite : str
            Suite name to add the scripts to that will be appended to the
            ecfhome
        parents: str
            Any parent folders that are appended to the ecfhome and suite
            folders.

        Returns
        -------
        None
        """
        if self.template == "skip":
            return
        script_name = f"{self.name()}.ecf"
        ecfscript = None
        search_script = f"{self.template}.ecf" if self.template is not \
                                                  None else script_name
        if parents:
            script_path = f"{ecfhome}/{suite}/{parents.replace('>','/')}/{script_name}"
        else:
            script_path = f"{ecfhome}/{suite}/{script_name}"
        for root,dirs,files in os.walk(self.scriptrepo):
            if search_script in files and ecfscript is None:
                ecfscript = os.path.join(root, search_script)
            elif script_name in files:
                print(f"More than one script named {script_name}. " 
                      "Using the first one found.")
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