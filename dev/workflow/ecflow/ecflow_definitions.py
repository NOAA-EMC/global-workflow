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
    raise ImportError(f"Error: Could not import ecflow module: {err}")


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
    ecfsuite_nodes : dict
        Dictionary object that contains all the nodes within the suite. This
        includes tasks, families, triggers, events, etc.
    ecf_nodes : dict
        While the ecfsuite_nodes dictionary tracks the actual ecflow API
        defined nodes, this dictionary tracks the custom nodes that are
        defined in the bottom of this module.
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
        Get the EcfSuite object

    get_suite_name( )
        Returns the name of the suite

    add_ecfsuite_node(name, node)
        Adds the ecfsuite node to the dictionary

    get_node(task)
        Returns a specific node from the suite.

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

    add_family_edits(edits, family, family_node, index)
        Since the families and tasks are handled slightly differently with the
        families being called from the ecflow_setup module and the tasks
        being handled in this module, a separate function to add family edits
        to the definition is required.

    add_task(task, parents, scriptrepo, template=None,
             parent_node=None, index=None)
        Adds a task to the parent node. If the build is set to true then the
        method also calls the creation method in the EcfTask class to deploy
        the script to the proper location. The script repo is where it will
        look for the script. If template is set, it will look for that template
        and then copy and change the name of the template at the destination to
        the name of the task.

    add_task_edits(task, edit_dict, parent_node=None, index=None)
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

    add_task_events(task, events, parent_node=None, index=None)
        Adds events to a task. This function breaks down the task string into
        ranges or lists if necessary but also breaks down the events if those
        are a list or range. It then passes the fully formed pieces to the
        add_event method to add them to the suite.

    add_suite_triggers(task, triggers, suite_array, parents, parent_node=None,
                        index=None)
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
        self.ecfsuite_nodes = {}
        self.ecf_nodes = {}
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
        new_suite : EcfSuite object
            An EcfSuite object
        """

        new_suite = EcfSuite(f"{suite}")
        if self.build_tree:
            new_suite.generate_folders(self.ecfhome)
        return new_suite

    def get_suite(self):
        """
        Get the EcfSuite object

        Parameters
        ----------
        None

        Returns
        -------
        EcfSuite
            The ecfsuite object that has all the contents
        """

        return self.ecfsuite

    def get_suite_name(self):
        """
        Returns the name of the suite

        Parameters
        ----------
        None

        Returns
        -------
        name : str
            The name of the suite.
        """

        return self.ecfsuite.name()

    def add_ecfsuite_node(self, name, node):
        """
        Adds the ecfsuite node to the dictionary

        Parameters
        ----------
        name : str
            The string name of the object
        node : EcfNode
            The actual node object.
        """

        self.ecfsuite_nodes[name] = node

    def get_node(self, node):
        """
        Returns a specific task from the suite.

        Parameters
        ----------
        task : str
            The name of the task to lookup in the EcfNodes dictionary.

        Returns
        -------
        EcfTask
            An EcfTask that is an extension of the ecflow.task object.
        """

        return self.ecf_nodes[node]

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
            self.ecf_nodes[parent] += ecflow.Edit(edit_dict)
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
            self.ecf_nodes[parent] += ecflow.Event(event)

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
            self.ecf_nodes[parent] += ecflow.Defstatus(defstatus)

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
            r"(\d{8,10})( | to )(\d{10})( | by )(\d{1,2}:)?(\d{1,2}:\d{1,2})",
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
            targetnode = self.ecf_nodes[parent]
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
                trigger_path = suite_array[suite].get_node(trigger).get_abs_node_path()
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
                    add_trigger = ecflow.Trigger([self.ecf_nodes[trigger]])
                elif state is not None and event is None:
                    trigger_path = self.ecf_nodes[trigger].get_abs_node_path()
                    add_trigger = ecflow.Trigger(f"{trigger_path} == {state}")
                elif state is None and event is not None:
                    trigger_path = self.ecf_nodes[trigger].get_abs_node_path()
                    add_trigger = ecflow.Trigger(f"{trigger_path}:{event}")
            except KeyError as e:
                print(f"The node/trigger {parent}/{trigger} is not available "
                      f"in suite {self.get_suite_name()}."
                      " Please check the configuration file.")
                print(f"Error {e}")
                sys.exit(1)
        if (operand is not None and
                self.ecf_nodes[parent].get_trigger() is not None):
            add_trigger = ecflow.Trigger(add_trigger.get_expression(), operand)
        self.ecf_nodes[parent].add(add_trigger)

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
        if family_name not in self.ecf_nodes.keys():
            self.ecf_nodes[family_name] = EcfFamily(family)
            if self.build_tree:
                self.ecf_nodes[family_name].generate_folders(self.ecfhome,
                                                             self.get_suite_name(),
                                                             parents)

        if parents:
            self.ecf_nodes[parents] += self.ecf_nodes[family_name]
        else:
            self.ecfsuite += self.ecf_nodes[family_name]

    def add_family_edits(self, edits, family, family_node, index):
        """
        Since the families and tasks are handled slightly differently with the
        families being called from the ecflow_setup module and the tasks
        being handled in this module, a separate function to add family edits
        to the definition is required.

        Parameters
        ----------
        edits : dict
            The dictionary that contains the edits to be added to the family.
        family : str
            This is a string representation of the current family. If it is
            a looping family, this will be the actual name for whatever the
            index is using.
        family_node : str
            This is the string of the family node that includes any looping
            mechanisms. This is necessary in case the edit needs to use the
            index or is a loop mechanism itself.
        index : int
            The current position of the loop in the event that the family is
            a loop so the edit value can reference the correct object.

        Returns
        -------
        None
        """

        for node in edits:
            edit_node = EcfEditNode(node, family_node)
            value_node = EcfEditNode(edits[node], family_node)
            for edit in edit_node.get_full_name_items(index):
                for value in value_node.get_full_name_items(index):
                    self.add_edit({edit: value}, family)

    def add_task(self, task, parents, scriptrepo, template=None,
                 parent_node=None, index=None):
        """
        Adds a task to the parent node. If the build is set to true then the
        method also calls the creation method in the EcfTask class to deploy
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
        parent_node : EcfNode
            This is the parent node string that contains any looping details
            if it is a list or a range object so that can be passed into the
            task objects.
        index : int
            The current position of the parent_node in its loop so if the task
            is dependent on the parent_node for position it is obtained from
            this value.

        Returns
        -------
        None
        """
        task_node = EcfTaskNode(task, parent_node)
        self.ecfsuite_nodes[task] = task_node
        for task_name in task_node.get_full_name_items(index):
            if task_name not in self.ecf_nodes.keys():
                self.ecf_nodes[task_name] = EcfTask(task_name)
                self.ecf_nodes[task_name].setup_script(scriptrepo, template)
                if self.build_tree:
                    self.ecf_nodes[task_name].generate_ecflow_task(self.ecfhome,
                                                                   self.get_suite_name(),
                                                                   parents)
                self.ecf_nodes[parents] += self.ecf_nodes[task_name]

    def add_task_edits(self, task, edit_dict, parent_node=None, index=None):
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
        parent_node : str
            This is the parent node string that contains any looping details
            if it is a list or a range object so that can be passed into the
            task objects.
        index : int
            The current position of the parent_node in its loop so if the edit
            is dependent on the parent_node for position it is obtained from
            this value.

        Returns
        -------
        None
        """

        task_node = self.ecfsuite_nodes[task]
        if len(task_node.get_full_name_items(index)) > 1:
            node_for_edits = task_node
        else:
            node_for_edits = parent_node
        for task_name in task_node.get_full_name_items(index):
            task_index = task_node.get_full_name_items().index(task_name)
            for node in edit_dict:
                edit_node = EcfEditNode(node, node_for_edits)
                value_node = EcfEditNode(edit_dict[node], node_for_edits)
                for edit in edit_node.get_full_name_items(task_index):
                    for value in value_node.get_full_name_items(task_index):
                        self.add_edit({edit: value}, task_name)

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

        task_node = self.ecfsuite_nodes[task]
        for task_name in task_node.get_full_name_items(index):
            self.add_repeat(repeat, task_name)

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

        task_node = self.ecfsuite_nodes[task]
        for task_name in task_node.get_full_name_items(index):
            self.add_defstatus(defstatus, task_name)

    def add_task_events(self, task, events, parent_node=None, index=None):
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
        parent_node : str
            This is the parent node string that contains any looping details
            if it is a list or a range object so that can be passed into the
            task objects.
        index : int
            The current position of the parent_node in its loop so if the task
            is dependent on the parent_node for position it is obtained from
            this value.

        Returns
        -------
        None
        """

        task_node = self.ecfsuite_nodes[task]
        for task_name in task_node.get_full_name_items(index):
            if task_node.is_list or task_node.is_range:
                node_for_events = task_node
                task_index = task_node.get_full_name_items().index(task_name)
            else:
                node_for_events = parent_node
                task_index = index
            for event_item in events:
                event_node = EcfEventNode(event_item, node_for_events)
                for node in event_node.get_full_name_items(task_index):
                    self.add_event(node, task_name)

    def add_suite_triggers(self, node, triggers, suite_array, parents,
                           parent_node=None, index=None):
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
        node : str
            The task string, list, range or static, that is to be broken down
            and then the triggers applied.
        triggers : dict
            The dictionary of triggers to add to the task.
        suite_array : dict
            In case the triggers are from another suite, this calls the trigger
            from the other suite.
        parents : str
            The string of the parents, this is used to identify the family in
            the event that the trigger is associated with a family.
        parent_node : str
            This is the parent node string that contains any looping details
            if it is a list or a range object so that can be passed into the
            task objects.
        index : int
            The current position of the parent_node in its loop so if the task
            is dependent on the parent_node for position it is obtained from
            this value.

        Returns
        -------
        None
        """

        working_node = self.ecfsuite_nodes[node]
        for item in working_node.get_full_name_items(index):
            if working_node.get_type() == "family":
                node_name = (f"{parents}>{item}")
            else:
                node_name = item
            for trigger_item in triggers:
                suite = None
                operand = None
                trigger_node = ecfTriggerNode(trigger_item, working_node)
                if trigger_node.has_suite():
                    suite = trigger_node.get_suite()
                if trigger_node.has_operand():
                    operand = trigger_node.get_operand()

                if working_node.is_list or working_node.is_range:
                    trigger_index = working_node. \
                        get_full_name_items(index). \
                        index(item)
                else:
                    trigger_index = index

                for trigger in trigger_node.get_full_name_items(trigger_index):
                    if trigger_node.trigger_type == "family":
                        trigger_name = trigger.replace('/', '>')
                    else:
                        trigger_name = trigger

                    if trigger_node.has_event():
                        if trigger_node.is_list or trigger_node.is_range:
                            event_index = trigger_node.\
                                get_full_name_items(index).\
                                index(trigger_name)
                        elif working_node.is_list or working_node.is_range:
                            event_index = trigger_index
                        else:
                            event_index = index
                        trigger_events = trigger_node.get_event()
                        for event in trigger_events.\
                                get_full_name_items(event_index):
                            if trigger_node.has_state():
                                state = trigger_node.get_state()
                                if not isinstance(state, list):
                                    state = [state]
                                for state_item in state:
                                    self.add_trigger(trigger_name, node_name,
                                                     suite=suite,
                                                     suite_array=suite_array,
                                                     event=event,
                                                     state=state_item,
                                                     operand=operand)
                            else:
                                self.add_trigger(trigger_name, node_name,
                                                 suite=suite,
                                                 suite_array=suite_array,
                                                 event=event,
                                                 operand=operand)
                    else:
                        if trigger_node.has_state():
                            state = trigger_node.get_state()
                            if not isinstance(state, list):
                                state = [state]
                            for state_item in state:
                                self.add_trigger(trigger_name, node_name,
                                                 suite=suite,
                                                 suite_array=suite_array,
                                                 state=state_item,
                                                 operand=operand)
                        else:
                            self.add_trigger(trigger_name, node_name,
                                             suite=suite,
                                             suite_array=suite_array,
                                             operand=operand)


class EcfNode():
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
    items : array
        Is the array of items within a list if the node object has a list
    full_name_items : array
        This is an array that consists of the counter plus any prefix or suffix
        in the node string.
    use_parent_counter : bool
        If the node use a list or range syntax but has no internal values,
        indicating that it should use the range of the parent node.

    Methods
    -------
    get_name()
        Returns the name of the node.

    __check_range(ecfitem)
        Checks to see if the EcfNode is a loop. If it is, this function also
        calls the supporting functions to set the range values, if there is
        a max, min, interval, or list.

    invalid_range()
        Helper function to ensure that the range is valid. Exits if it is not.

    get_items()
        Returns the list of items for the ecf node so that it can be distributed
        to the child nodes.

    get_full_name_items(counter=0)
        If the item is a range or list, it returns the full names of the items
        with the prefix and suffix strings included, if it is a single then it
        just returns an array of one item. If it uses the parent counter it
        returns an array of one item in the position of the parent counter.

    __set_max_value(range_token)
        The range token is passed in and if only one value is set in the range
        then it is set to max value and the initial is set to 0 and the
        interval is set to 1.

    __set_initial_max_value(range_token)
        If the range token is passed two parameters, they are assumed to be
        the initial and max values. This sets those values for the node. The
        interval is set to 1.

    __set_initial_increment_max_value(range_token)
        If three values are sent in through the range token, this sets the max,
        initial, and increment values.

    __setup_items_list(ecfparent)
        In the event that the items list wasn't already defined, this sets up
        the list of items for the node by pulling in the parent items if
        necessary, modifying the increment setup as necessary as well. After
        this method is called the items array should be fully populated.

    __populate_full_name_items()
        Called after the items list is populated. If there is a range or list
        then this uses the items list with the prefix and suffix strings to
        create a new array for full names and populates that array. After this
        is called the full_name_items array should be used. If the node isn't
        a range or list then it is just an array of one item.

    get_full_name(counter=None)
        This method uses the counter object if the item is a list to identify
        the position in a list, the item in the range or if there is no counter
        associated with it, the base name.

    get_position_name(counter=None)
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

    def __init__(self, ecfitem, ecfparent=None):
        """
        Parameters
        ----------
        ecfitem : str
            Name of the EcfNode item. If it contains a range or list
            identifier, the other values are populated to identify what kind
            of node it is.
        ecfparent : str
            Name of the parent for the EcfNode item. This will help determine
            if the parent has the counter or if one is defined for this class
        """
        self.__items = []
        self.__full_name_items = []
        self.__check_range(ecfitem)
        self.__setup_items_list(ecfparent)
        self.__populate_full_name_items()
        if (ecfparent and self.__max_value is None and
            (ecfparent.is_list or ecfparent.is_range) and
                len(self.__items) == len(ecfparent.get_full_name_items())):
            self.use_parent_counter = True

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

    def __check_range(self, ecfitem):
        """
        Checks to see if the EcfNode is a loop. If it is, this function also
        calls the supporting functions to set the range values, if there is
        a max, min, interval, or list.

        The range is split into a tokenized array.

        Parameters
        ----------
        ecfitem : str
            The item that is to be processed. This is just the name of the
            item, not the supporting dictionary if there is one.


        Returns
        -------
        bool
            True if the node is a loop format defined by ( ).
        """

        self.is_list = False
        self.is_range = False
        self.use_parent_counter = False
        self.__base = ''
        self.__suffix = ''
        self.initial_count = None
        self.increment = None
        self.__max_value = None

        if isinstance(ecfitem, str):
            self.name = ecfitem
            if re.search(r".*\(.*\).*", ecfitem):
                self.is_range = True
                range_functions = {
                    1: self.__set_max_value,
                    2: self.__set_initial_max_value,
                    3: self.__set_initial_increment_max_value,
                }
                range_token = re.search(r"(.*)\((.*)\)(.*)", self.name)
                range_type = range_token.group(2).strip().split(',')
                self.__base = range_token.group(1).strip()
                self.__suffix = range_token.group(3).strip()
                range_functions.get(len(range_type),
                                    self.invalid_range)(range_type)
            elif re.search(r".*\[.*\].*", ecfitem):
                self.is_list = True
                list_token = re.search(r"(.*)\[(.*)\](.*)", ecfitem)
                list_type = list_token.group(2).strip().split(',')
                self.__base = list_token.group(1).strip()
                self.__suffix = list_token.group(3).strip()
                if not list_type[0]:
                    self.use_parent_counter = True
                else:
                    self.__items = list_type
            else:
                self.__items = [ecfitem]
        elif isinstance(ecfitem, list):
            self.name = ''.join(str(i) for i in ecfitem)
            self.is_list = True
            self.__items = ecfitem
        else:
            self.name = ecfitem
            self.is_list = False
            self.__items = [ecfitem]

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

    def get_items(self):
        """
        Returns the list of items for the ecf node so that it can be distributed
        to the child nodes.

        Parameters
        ----------
        None

        Returns
        -------
        items : array
            The array of items for the node.
        """

        return self.__items

    def get_full_name_items(self, counter=0):
        """
        If the item is a range or list, it returns the full names of the items
        with the prefix and suffix strings included, if it is a single then it
        just returns an array of one item. If it uses the parent counter it
        returns an array of one item in the position of the parent counter.

        Parameters
        ----------
        counter : int
            The position of the parent counter. If the parent counter is not
            used it defaults to 0 and is not used.

        Returns
        -------
        array
            The array of the full name items either as a full list or an
            array of one.
        """

        if self.use_parent_counter:
            return [self.__full_name_items[counter]]
        else:
            return self.__full_name_items

    def __set_max_value(self, range_token):
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

        if not range_token[0]:
            self.__max_value = None
            self.use_parent_counter = True
        else:
            try:
                self.__max_value = int(range_token[0])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def __set_initial_max_value(self, range_token):
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
            self.__max_value = None
            self.use_parent_counter = True
        else:
            try:
                self.__max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def __set_initial_increment_max_value(self, range_token):
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
            self.__max_value = None
            self.use_parent_counter = True
        else:
            try:
                self.__max_value = int(range_token[1])
            except TypeError:
                print(f"Maximum value for {self.name} is not an integer")
                sys.exit(1)

    def __setup_items_list(self, ecfparent):
        """
        In the event that the items list wasn't already defined, this sets up
        the list of items for the node by pulling in the parent items if
        necessary, modifying the increment setup as necessary as well. After
        this method is called the items array should be fully populated.

        Parameters
        ----------
        ecfparent : str
            The parent node in case the current node uses the parent counter.
            This is passed in to get those parameters.

        Returns
        -------
        None
        """

        # First check to see if the list object is already set or if this
        # is even a list. If they are just return, nothing else to do.
        if self.is_list:
            return
        if self.__items:
            return

        if self.use_parent_counter:
            if self.is_list:
                self.__items = ecfparent.get_items()
            elif self.is_range:
                if self.initial_count is None:
                    self.initial_count = ecfparent.initial_count
                if self.increment is None:
                    self.increment = ecfparent.increment
                item_range = self.get_range(max_value=len(ecfparent.get_items()))
                self.__items = [*item_range]
        else:
            if self.is_range and self.has_max_value():
                item_range = self.get_range()
                self.__items = [*item_range]

    def __populate_full_name_items(self):
        """
        Called after the items list is populated. If there is a range or list
        then this uses the items list with the prefix and suffix strings to
        create a new array for full names and populates that array. After this
        is called the full_name_items array should be used. If the node isn't
        a range or list then it is just an array of one item.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        if not self.is_range and not self.is_list:
            self.__full_name_items = self.__items
            return

        for item in self.__items:
            if isinstance(item, int):
                self.__full_name_items.append(f"{self.__base}"
                                              f"{item:03}"
                                              f"{self.__suffix}")
            elif isinstance(item, str):
                self.__full_name_items.append(f"{self.__base}"
                                              f"{item}"
                                              f"{self.__suffix}")

    def get_position_name(self, counter=None):
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
            if self.is_range:
                if isinstance(counter, int):
                    return f"{self.__base}{counter:03}{self.__suffix}"
                elif isinstance(counter, str):
                    return f"{self.__base}{counter}{self.__suffix}"
            elif re.search(r"\[.*\]", self.name):
                array_item = self.__items[counter]
                if isinstance(array_item, int):
                    return f"{self.__base}{array_item:03}{self.__suffix}"
                elif isinstance(array_item, str):
                    return f"{self.__base}{array_item}{self.__suffix}"
            elif self.is_list:
                array_item = self.__items[counter]
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

        return True if self.__max_value is not None else False

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

        return self.__max_value

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
            return range(initial_count, len(self.__items), increment)
        else:
            if self.initial_count is not None:
                initial_count = self.initial_count
            if self.increment is not None:
                increment = self.increment
            if self.__max_value is not None:
                max_value = self.__max_value
            max_value = (max_value * increment) + initial_count
            return range(initial_count, max_value, increment)


class EcfTaskNode(EcfNode):
    """
    Extension class for the EcfNodes to identify tasks.

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


class EcfFamilyNode(EcfNode):
    """
    Extension class for the EcfNodes to identify tasks.

    Methods
    -------
    get_type()
        Returns that this node is a task type.
    """

    def get_type(self):
        """
        Returns that this node is a task type.
        """

        return 'family'


class EcfEventNode(EcfNode):
    """
    Extension class for the EcfNodes to identify events.

    Methods
    -------
    get_type()
        Returns that this node is an event type.
    """

    def get_type(self):
        """
        Returns that this node is an event type.
        """

        return 'event'


class ecfTriggerNode(EcfNode):
    """
    Extension class for the EcfNodes to identify triggers. Overloads the
    constructors since triggers can have multiple levels within themselves
    for events and such.

    Attributes
    ----------
    EcfNode : object
        This pulls in the attributes from the EcfNode class as well.
    task_setup : dict
        This stores the dictionary object that tracks the task setup for the
        trigger.
    ecfparent : str
        The ecfparent string object used to track loops/list. This is used in
        case there is a multi-level loop.
    trigger_type : str
        Used to identify if the trigger is of a family or of a task.

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

    has_event()
        If the trigger has an event associated with it, it is possible that the
        event has a loop. This method determines if the trigger has an event
        and if it does identifies the event string and items associated with it
        so that it can be used in other functions later. If it does have the
        loop or list identifiers then it returns true, otherwise false.

    invalid_event_range()
        Helper method to exit the application if the event range is invalid.
    """

    def __init__(self, ecfitem, ecfparent=None):
        """
        Parameters
        ----------
        ecfItem : dict or str
            A dictionary or string item that represents the current node.
        """

        self._EcfNode__items = []
        self._EcfNode__full_name_items = []
        if 'family' in ecfitem.keys():
            trigger_type = 'family'
        else:
            trigger_type = 'task'
        self._EcfNode__check_range(ecfitem[trigger_type])
        self._EcfNode__setup_items_list(ecfparent)
        self._EcfNode__populate_full_name_items()
        self.task_setup = ecfitem
        self.ecfparent = ecfparent
        self.trigger_type = trigger_type

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

        if self.operand == "OR" or self.operand == "or":
            return False
        else:
            return True

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

        return self.event

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
            if self.is_list or self.is_range:
                self.event = EcfEventNode(self.task_setup['event'], self)
            else:
                self.event = EcfEventNode(self.task_setup['event'],
                                          self.ecfparent)
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


class EcfEventNode(EcfNode):
    """
    Extension class for the EcfNodes to identify events.

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


class EcfEditNode(EcfNode):
    """
    Extension class for the EcfNodes to identify edits.

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


class EcfRoot():
    """
    A root level class that is not an EcfNode object from above but an
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
        return re.search(r"(.*)\{.*\}", self.name()).group(1).strip()


class EcfSuite(ecflow.Suite, EcfRoot):
    """
    Extends the EcfRoot and ecflow.Suite classes to provide an additional
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


class EcfFamily(ecflow.Family, EcfRoot):
    """
    Extends the ecflow.Family and EcfRoot classes to provide the folder
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


class EcfTask(ecflow.Task, EcfRoot):
    """
    Extends the ecflow.Task and EcfRoot classes to allow the task scripts to
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
        for root, dirs, files in os.walk(self.scriptrepo):
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
