#!/usr/bin/env python3

"""
    PROGRAM:
        ecflow_setup: This setup is to read in the configuration from the YAML
        file passed in by setup_workflow.py, populate the environment variables
        and then pass that to the ecflow_definitions.py module to create the
        suite definitions and to break down the lists, add triggers, etc.
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        None
    OUTPUT:
        This will return a dictionary object of suites and then save that to
        a file based on the calls from the setup_workflow.py module.
"""
import yaml
import collections.abc
import os
import re
import sys
import datetime
from ecFlow.ecflow_definitions import Ecflowsuite, EcfFamilyNode

try:
    from ecflow import Defs
except ImportError as err:
    raise ImportError(f"Error: Could not import ecflow module: {err}")


class Ecflowsetup:
    """
    This class pulls in the configurations from the ecflow config file. Then
    it calls the ecflow_definitions.py module to create a suite definition
    from each of the suites in the YAML file. Then each of the edits, tasks,
    triggers, etc. are processed and added to the suite.

    All suites are then put together as part of a definition file and finally
    that file is saved.

    Attributes
    ----------
    suite_array : dict
        A dictionary that contains ecflow.Suite objects provided by the ecflow
        module.
    DEFS : ecflow.Defs
        A definition object provided by the ecflow module that holds all of the
        suites.

    Methods
    -------
    generate_workflow()
        This is the main method, used to setup the suites from the YAML file and
        then call each of the supporting methods, like edits, tasks, etc. to
        populate the suites with each of the items.

    raiseexception(e)
        This is just a simple method that is called if an exception is raised to
        print out the error message and then call a sys.exit(1) so the app stops

    save()
        This saves the suite definition file to the save dir with the file name
        ecflow_suite.def.

    print()
        Prints out what would be populated to the suite definition file.

    add_environment_edits(suite)
        The suite is passed in and the edits from the environment are added. The
        environment edits are defined in the init method.

    check_dict(node, key, key_is_dict=True)
        This function checks for the presence of they key inside of the node.
        Used to identify it various addons need to be added into the suite.

    add_suite_edits(suite, suite_dict)
        Method used to parse through the YAML file and identify any edits that
        apply to the suite itself and parse them so they can be added.

    process_definition_header()
        If there is an externs section in the YAML file for a suite, this adds
        the externs to the header.

    add_families(suite, nodes, parents=None)
        Parses through the YAML file contents and adds the nodes that are
        identified as families to either the parent suite or the parent family.

    add_tasks_and_edits(suite,nodes,parents=None)
        After the families are added to the suite, the individual tasks, edits,
        repeats, defstatus, and room for other task addons are appended.

    add_triggers_and_events(suite, nodes)
        After the families and tasks are added, then the triggers and events
        are processed. This needs to come after the families and tasks and
        independently because of the interdependencies that exist. For example
        a trigger for a task cannot be added until the task exists, otherwise
        and error will be thrown.
    """

    def __init__(self, args, env_configs):
        """
        Parameters
        ----------
        args : dict
            The arguments passed in by the command line.
        env_configs : dict
            The environment variables pulled in from the experiement directory.

        Returns
        -------
        None
        """

        # Setup the base variables
        self.args = args
        self.env_configs = env_configs
        self.suite_array = {}
        self.DEFS = Defs()

        # Load in the ecflow configurations
        base_ecflowconfig = load_ecflow_config(f'{args.ecflow_config}')
        self.ecfconf = update_ecflow_config(base_ecflowconfig, env_configs)

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
        """
        This is the main method, used to setup the suites from the YAML file and
        then call each of the supporting methods, like edits, tasks, etc. to
        populate the suites with each of the items.

        Methods
        -------
        get_suite_names(suitename)
            In the event that the suite uses a list definition [X,Y,Z...], this
            method will generate an array of the properly formatted names.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        def get_suite_names(suitename):
            """
            In the event that the suite uses a list definition [X,Y,Z...], this
            method will generate an array of the properly formatted names.

            This is internal to the generate_workflow method and is only called
            from within. The names are split out using regex if it is a list.

            Parameters
            ----------
            suitename : str
                A string representation of the

            Returns
            -------
            array
                If not a list, returns an array with the suitename paraemeter as
                the only object. If it is a list, return all the names.
            """

            # Check to see if the name actually has a list, if not return an
            # array with just the suite name as object in place 0.
            if not re.search(r".*\[.*\].*", suitename):
                return [f"{suitename}"]

            # If the name does have a list, break apart the prefix and suffix
            # from the list and then run it through a for loop to get all
            # possible values.
            name_token = re.search(r"(.*)\[(.*)\](.*)", suitename)
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
            if suite not in {'externs', 'edits'}:
                for suite_name in get_suite_names(suite):
                    # The first thing to do is add in all of the families and
                    # tasks. Triggers and edits cannot be added until the tasks
                    # and families are parsed.
                    if suite_name not in self.suite_array.keys():
                        new_suite = Ecflowsuite(suite_name, self.env_configs['base']['ECFgfs'])
                    else:
                        new_suite = self.suite_array[suite_name]
                    if new_suite.get_suite_name() not in self.suite_array.keys():
                        self.add_environment_edits(new_suite)
                    self.add_suite_edits(new_suite, self.ecfconf['suites'][suite])
                    if self.check_dict(self.ecfconf['suites'][suite], 'nodes'):
                        self.add_families(new_suite, self.ecfconf['suites'][suite]['nodes'])
                        self.add_tasks_and_edits(new_suite, self.ecfconf['suites'][suite]['nodes'])
                    self.suite_array[new_suite.get_suite_name()] = new_suite

        # Now that the families and tasks are setup, run through the triggers
        # and events and add them to the respective tasks/family objects.
        for suite in self.ecfconf['suites'].keys():
            if suite not in {'externs', 'edits'}:
                for suite_name in get_suite_names(suite):
                    if self.check_dict(self.ecfconf['suites'][suite], 'nodes'):
                        self.add_triggers_and_events(self.suite_array[suite_name],
                                                     self.ecfconf['suites'][suite]['nodes'])

        # Add each suite to the definition object that will be used for the save
        # or print.
        for suite_name, suite in self.suite_array.items():
            self.DEFS += suite.get_suite()

    def raiseexception(self, e):
        """
        This is just a simple method that is called if an exception is raised to
        print out the error message and then call a sys.exit(1) so the app stops

        Calling this method will cause the application to exit with a status
        code of 1.

        Parameters
        ----------
        e : str
            The error in string format to print out.

        Returns
        -------
        None
        """

        print(e)
        sys.exit(1)

    def save(self):
        """
        This saves the suite definition file to the save dir with the file name
        ecflow_suite.def.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        print("Saving definition File")
        savedir = self.args.savedir
        defs_file = f"{savedir}/ecflow_suite.def"
        self.DEFS.save_as_defs(defs_file)

    def print(self):
        """
        Prints out what would be populated to the suite definition file.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """
        print(self.DEFS.check())
        print(self.DEFS)

    def add_environment_edits(self, suite):
        """
        The suite is passed in and the edits from the environment are added. The
        environment edits are defined in the init method.

        This method assumes that there are environment edits that have been set
        by the experiement setup.

        Parameters
        ----------
        suite : str
            The name of the suite that will be used to add the environment edits

        Returns
        -------
        None
        """

        # Add in the ECF Home and ECF Include edits.
        suite.add_edit({'ECF_HOME': self.ecfhome, 'ECF_INCLUDE': self.ecfhome})

        # Add in the edits for the environment.
        for edit in self.environment_edits:
            edit = edit.upper()
            if (edit in self.env_configs['base'].keys() and
                    self.env_configs['base'][edit] is not None):
                edit_dict = {edit: self.env_configs['base'][edit]}
            elif (edit.lower() in self.env_configs['base'].keys() and
                  self.env_configs['base'][edit.lower()] is not None):
                edit_dict = {edit: self.env_configs['base'][edit.lower()]}
            suite.add_edit(edit_dict)

    def check_dict(self, node, key, key_is_dict=True):
        """
        This function checks for the presence of they key inside of the node.
        Used to identify it various addons need to be added into the suite.

        If the node is a dict, it checks for the presence of the key but it also
        needs to know if the key it is looking for is a dictionary or not.

        Parameters
        ----------
        node : dict or str
            The dictionary or string object to search for the presence of the
            key
        key : str
            The search string to look for in the node objects
        key_is_dict : bool
            Checks if the key is a dictionary or if it should be searching for
            a string.

        Returns
        -------
        bool
            True if the key is present, false otherwise.
        """

        if isinstance(node, dict) and f'{key}' in node.keys():
            if key_is_dict and isinstance(node[f'{key}'], dict):
                return True
            elif not key_is_dict:
                return True
        else:
            return False

    def add_suite_edits(self, suite, suite_dict):
        """
        Method used to parse through the YAML file and identify any edits that
        apply to the suite itself and parse them so they can be added.

        Parameters
        ----------
        suite : string
            Name of the suite that needs the edits added.
        suite_dict :
            The dictionary for the suite that was passed in.

        Returns
        -------
        None
        """

        # Baseline edits
        if 'edits' in self.ecfconf['suites'].keys():
            suite.add_edit(self.ecfconf['suites']['edits'])

        # Setup sutite specific edits
        if type(suite_dict) is dict and 'edits' in suite_dict.keys():
            suite.add_edit(suite_dict['edits'])

    def process_definition_header(self):
        """
        If there is an externs section in the YAML file for a suite, this adds
        the externs to the header.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        if 'externs' in self.ecfconf.keys():
            for extern in self.ecfconf['externs']:
                self.DEFS.add_extern(extern)

    def add_families(self, suite, nodes, parents=None, parent_node=None):
        """
        Parses through the YAML file contents and adds the nodes that are
        identified as families to either the parent suite or the parent family.

        This function is recursive to build the family architecture.

        While adding families, this method also adds in the edits, repeats,
        defstatus, and time parameters to the families.

        Parameters
        ----------
        suite : str
            The suite that the families are to be added to
        nodes : dict
            The nodes within the suite, can be families or tasks but only the
            families are processed in this method.
        parents : str
            If this family is not a top level one for the suite, this string is
            the list of families that came before it, used to populate the
            dictionary object in the ecflow_definitions module.
        parent_node : dict
            This is the node for the parent object. Separate from the parents
            object, this contains the full name of the parent. The parents
            object is a string and doesn't contain the information for any
            loop object.

        Returns
        -------
        None
        """

        for item in nodes.keys():
            if isinstance(nodes[item], dict) and item not in {'edits',
                                                              'tasks',
                                                              'triggers'}:
                family_node = EcfFamilyNode(item, parent_node)
                suite.add_ecfsuite_node(item, family_node)
                for family in family_node.get_full_name_items():
                    suite.add_family(family, parents)
                    index = family_node.get_full_name_items().index(family)
                    if parents:
                        family_path = f"{parents}>{family}"
                    else:
                        family_path = family
                    if self.check_dict(nodes[item], 'edits'):
                        suite.add_family_edits(nodes[item]['edits'],
                                               family_path, family_node, index)
                    if self.check_dict(nodes[item], 'repeat', False):
                        suite.add_repeat(nodes[item]['repeat'], family_path)
                    if self.check_dict(nodes[item], 'defstatus', False):
                        suite.add_defstatus(nodes[item]['defstatus'],
                                            family_path)
                    self.add_families(suite, nodes[item],
                                      family_path, family_node)

    def add_tasks_and_edits(self, suite, nodes,
                            parents=None, parent_node=None,
                            index=None):
        """
        After the families are added to the suite, the individual tasks, edits,
        repeats, defstatus, and room for other task addons are appended.

        This is a recursive function that parses through the whole dictionary
        of tasks and families to identify any tasks and add them to a family.

        This also adds in the defstatus, ediuts, repeats, times, etc. for the
        tasks.

        Parameters
        ----------
        suite : str
            The suite the tasks need to be added to.
        nodes : dict
            Contains all the tasks and families for the parent node.
        parents : str
            The parent family for any of the tasks
        parent_node : dict
            This is the actual parent node that would contain any looping
            information or range information unlike the parent string which
            contains the full name of the parents.
        index : int
            This is the index position of the current node being worked. This
            is tracked so if the current node relies on the parent index, this
            tells the current node what position object to use.

        Returns
        -------
        None
        """

        for item in nodes.keys():
            if isinstance(nodes[item], dict) and item == 'tasks':
                for task in nodes['tasks'].keys():
                    if self.check_dict(nodes['tasks'][task], 'template', False):
                        task_template = nodes['tasks'][task]['template']
                    else:
                        task_template = None
                    updated_task = find_env_param(task, 'env.',
                                                  self.env_configs)
                    suite.add_task(updated_task, parents,
                                   self.scriptrepo, task_template,
                                   parent_node, index)
                    if self.check_dict(nodes['tasks'][task],
                                       'edits'):
                        suite.add_task_edits(updated_task,
                                             nodes['tasks'][task]['edits'],
                                             parent_node, index)
                    if self.check_dict(nodes['tasks'][task],
                                       'repeat', False):
                        suite.add_task_repeat(updated_task,
                                              nodes['tasks'][task]['repeat'],
                                              parent_node, index)
                    if self.check_dict(nodes['tasks'][task],
                                       'defstatus', False):
                        suite.add_task_defstatus(updated_task,
                                                 nodes['tasks']
                                                 [task]['defstatus'])

            elif isinstance(nodes[item], dict) and item not in {'edits',
                                                                'triggers'}:
                family_node = EcfFamilyNode(item, parent_node)
                for family in family_node.get_full_name_items():
                    index = family_node.get_full_name_items().index(family)
                    if parents:
                        family_path = f"{parents}>{family}"
                    else:
                        family_path = family
                    self.add_tasks_and_edits(suite, nodes[item],
                                             family_path, family_node, index)

    def add_triggers_and_events(self, suite, nodes, parents=None,
                                parent_node=None, index=None):
        """
        After the families and tasks are added, then the triggers and events
        are processed. This needs to come after the families and tasks and
        independently because of the interdependencies that exist. For example
        a trigger for a task cannot be added until the task exists, otherwise
        and error will be thrown.

        This is a recursive function and will parse through each family/task
        to identify the work.

        Parameters
        ----------
        suite : str
            The suite to key off for adding the triggers
        nodes : dict
            The families/tasks that need to be parsed.
        parents : str
            The parent family for any of the tasks
        parent_node : dict
            This is the actual parent node that would contain any looping
            information or range information unlike the parent string which
            contains the full name of the parents.
        index : int
            This is the index position of the current node being worked. This
            is tracked so if the current node relies on the parent index, this
            tells the current node what position object to use.

        Returns
        -------
        None
        """

        for item in nodes.keys():
            if self.check_dict(nodes[item], 'triggers', False):
                updated_family = find_env_param(item, 'env.',
                                                self.env_configs)
                suite.add_suite_triggers(updated_family,
                                         nodes[item]['triggers'],
                                         self.suite_array, parents,
                                         parent_node, index)
            elif isinstance(nodes[item], dict) and item == 'tasks':
                for task in nodes['tasks'].keys():
                    updated_task = find_env_param(task, 'env.',
                                                  self.env_configs)
                    if self.check_dict(nodes['tasks'][task], 'events', False):
                        suite.add_task_events(updated_task,
                                              nodes['tasks'][task]['events'],
                                              parent_node, index)
                    if self.check_dict(nodes['tasks'][task], 'triggers', False):
                        suite.add_suite_triggers(updated_task,
                                                 nodes['tasks'][task]['triggers'],
                                                 self.suite_array, parents,
                                                 parent_node, index)
            elif isinstance(nodes[item], dict):
                family_node = EcfFamilyNode(item, parent_node)
                for family in family_node.get_full_name_items():
                    index = family_node.get_full_name_items().index(family)
                    if parents:
                        family_path = f"{parents}>{item}"
                    else:
                        family_path = item
                    self.add_triggers_and_events(suite, nodes[item],
                                                 family_path, family_node,
                                                 index)


def load_ecflow_config(configfile):
    """
    This is the function to safely load the configuration file for the ecflow
    environment. This is the base YAML that is built specifically for this
    application and then returns it.

    Parameters
    ----------
    configfile : str
        The path to the configuration file that is to be loaded as part of the
        ecflow config.

    Returns
    -------
    dict
        The dictionary results of the YAML safe load from the configuration
        file.
    """

    with open(configfile, 'r') as file:
        base_config = yaml.safe_load(file)
    return base_config


def find_env_param(node, value, envconfig):
    """
    Since there are components of the configuration that might get passed in
    that are supposed to be replaced by environment variables AFTER the
    configuration file has been loaded, this function is called in some of the
    Ecflowsetup functions to allow the replacement of those parameters as
    needed.

    Parameters
    ----------
    node : dict
        A dictionary object of the items that need to be scanned for replacing
    value : str
        A string object that is the prefix to be scanned and then the value
        identified after identifier string is replaced with an environment
        variable.
    envconfig : dict
        The dictionary of existing environment variables that are read in from
        the experiment setup.

    Returns
    -------
    new_node : dict
        The updated dictionary object that will replace the node object that
        was passed in when the function was called.
    """

    new_node = node
    if value in node:
        variable_lookup = re.search(fr".*{value}([\dA-Za-z_]*)", node).group(1).strip()
        if variable_lookup in os.environ:
            if isinstance(os.environ[variable_lookup], datetime.datetime):
                new_variable = os.environ[variable_lookup].strftime("%Y%m%d%H")
            else:
                new_variable = os.environ[variable_lookup]
        else:
            if isinstance(envconfig['base'][variable_lookup],
                          datetime.datetime):
                new_variable = envconfig['base'][variable_lookup].strftime("%Y%m%d%H")
            else:
                new_variable = envconfig['base'][variable_lookup]
        search_key = re.search(r"(.*)(env\.[\dA-Za-z_]*)(.*)", node)
        new_node = f"{search_key.group(1)} {new_variable} {search_key.group(3)}"
    return new_node


def update_ecflow_config(configfile, envconfig):
    """
    After the YAML file that drives the application is loaded in, the configs
    need to be updated with anything that has the env. prefix to it and replace
    that value with the environment variable.

    Parameters
    ----------
    configfile : dict
        The dictionary of the YAML configuration file read in.
    envconfig : dict
        The dictionary of objects that were read in from the experiment setup
        on the supercomputer.

    Returns
    -------
    config : dict
        The updated configuration with the environment variables replaced.
    """

    def runupdate(nested_dict, value):
        """
        To scan through the entire nested dictionary the run update was an easy
        local function to use to provide recursion given that the parent
        function did not work properly when trying to use a recursive call.

        Parameters
        ----------
        nested_dict : dict
            The nested dictionary to scan and replace the values.
        value : str
            The string to search for the replacement, currently set to env.

        Returns
        -------
        nested_dict : dict
            The updated dictionary with all of the values replaced as necessary.
        """
        for k, v in nested_dict.items():
            if isinstance(v, str) and value in v:
                lookup = v.split('.')
                variable_lookup = re.findall(r"[\dA-Za-z_]*", lookup[1])[0]
                if variable_lookup in os.environ:
                    if isinstance(os.environ[variable_lookup], datetime.datetime):
                        nested_dict[k] = os.environ[variable_lookup].strftime("%Y%m%d%H")
                    else:
                        nested_dict[k] = os.environ[variable_lookup]

                else:
                    if isinstance(envconfig['base'][variable_lookup], datetime.datetime):
                        envvalue = envconfig['base'][variable_lookup].strftime("%Y%m%d%H")
                    else:
                        envvalue = envconfig['base'][variable_lookup]
                    nested_dict[k] = envvalue
            elif isinstance(v, collections.abc.Mapping):
                nested_dict[k] = runupdate(v, value)
        return nested_dict

    config = runupdate(configfile, 'env.')
    return config
