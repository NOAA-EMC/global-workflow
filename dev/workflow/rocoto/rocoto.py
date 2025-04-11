#!/usr/bin/env python3

from typing import Union, List, Dict, Any

'''
    MODULE:
        rocoto.py

    ABOUT:
        Helper module to create tasks, metatasks, and dependencies for Rocoto
        Rocoto documentation is available at https://christopherwharrop.github.io/rocoto
'''

__all__ = ['create_task',
           'add_dependency', 'create_dependency',
           'create_envar', 'create_entity', 'create_cycledef']


def create_task(task_dict: Dict[str, Any]) -> List[str]:
    """
    Create XML for a rocoto task or metatask

    Creates the XML required to define a task and returns the lines
    as a list of strings. Tasks can be nested to create metatasks by
    defining a key 'task_dict' within the task_dict. When including
    a nested task, you also need to provide a 'var_dict' key that
    contains a dictionary of variables to loop over.

    All task dicts must include a 'task_name'.

    Innermost tasks (regular tasks) additionally require a 'resources'
    key containing a dict of values defining the HPC settings.

    Parameters
    ----------
    task_dict: dict
        Dictionary of task definitions

    Returns
    -------
    str
        Strings containing the XML code defining the task

    Raises
    ------
    KeyError
        If a required key is missing

    """

    inner_task_dict = task_dict.pop('task_dict', None)

    if inner_task_dict is None:
        strings = _create_innermost_task(task_dict)

    else:
        # There is a nested task_dict, so this is a metatask
        metataskname = f"{task_dict.get('task_name', 'demometatask')}"
        metataskmode = 'serial' if task_dict.get('is_serial', False) else 'parallel'
        var_dict = task_dict.get('var_dict', None)

        strings = [f'<metatask name="{metataskname}" mode="{metataskmode}">\n',
                   '\n']

        if var_dict is None:
            msg = f'Task {metataskname} has a nested task dict, but has no var_dict'
            raise KeyError(msg)

        for key in var_dict.keys():
            value = str(var_dict[key])
            strings.append(f'\t<var name="{key}">{value}</var>\n')

        strings.append('\n')
        task_dict.update(inner_task_dict)
        tasklines = create_task(task_dict).splitlines(True)
        for tl in tasklines:
            strings.append(f'{tl}') if tl == '\n' else strings.append(f'\t{tl}')
        strings.append('\n')
        strings.append('</metatask>\n')

    return ''.join(strings)


def _create_innermost_task(task_dict: Dict[str, Any]) -> List[str]:
    """
    Create XML for a regular rocoto task

    Creates the XML required to define a task and returns the lines
    as a list of strings.

    All task dicts must include a 'task_name' and a 'resources'
    key containing a dict of values defining the HPC settings.

    Parameters
    ----------
    task_dict: dict
        Dictionary of task definitions

    Returns
    -------
    List[str]
        List of strings containing the XML code defining the task

    Raises
    ------
    KeyError
        If a required key is missing

    """

    # Grab task info from the task_names
    taskname = task_dict.get('task_name', 'demotask')
    cycledef = task_dict.get('cycledef', 'democycle')
    maxtries = task_dict.get('maxtries', 3)
    final = task_dict.get('final', False)
    command = task_dict.get('command', 'sleep 10')
    jobname = task_dict.get('job_name', 'demojob')
    resources_dict = task_dict['resources']
    account = resources_dict.get('account', 'batch')
    queue = resources_dict.get('queue', 'debug')
    partition = resources_dict.get('partition', None)
    walltime = resources_dict.get('walltime', '00:01:00')
    native = resources_dict.get('native', None)
    memory = resources_dict.get('memory', None)
    nodes = resources_dict.get('nodes', 1)
    ppn = resources_dict.get('ppn', 1)
    threads = resources_dict.get('threads', 1)
    log = task_dict.get('log', 'demo.log')
    envar = task_dict.get('envars', None)
    dependency = task_dict.get('dependency', [])

    str_maxtries = str(maxtries)
    str_final = ' final="true"' if final else ''
    envar = envar if isinstance(envar, list) else [envar]

    strings = [f'<task name="{taskname}" cycledefs="{cycledef}" maxtries="{str_maxtries}"{str_final}>\n',
               '\n',
               f'\t<command>{command}</command>\n',
               '\n',
               f'\t<jobname><cyclestr>{jobname}</cyclestr></jobname>\n',
               f'\t<account>{account}</account>\n',
               f'\t<queue>{queue}</queue>\n']

    if partition is not None:
        strings.append(f'\t<partition>{partition}</partition>\n')
    strings.append(f'\t<walltime>{walltime}</walltime>\n')
    strings.append(f'\t<nodes>{nodes}:ppn={ppn}:tpp={threads}</nodes>\n')
    if memory is not None:
        strings.append(f'\t<memory>{memory}</memory>\n')
    if native is not None:
        strings.append(f'\t<native>{native}</native>\n')
    strings.append('\n')
    strings.append(f'\t<join><cyclestr>{log}</cyclestr></join>\n')
    strings.append('\n')

    if envar[0] is not None:
        for e in envar:
            strings.append(f'\t{e}\n')
        strings.append('\n')

    if dependency is not None and len(dependency) > 0:
        strings.append('\t<dependency>\n')
        for d in dependency:
            strings.append(f'\t\t{d}\n')
        strings.append('\t</dependency>\n')
        strings.append('\n')

    strings.append('</task>\n')

    return strings


def add_dependency(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple Rocoto dependency given a dictionary with dependency information
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple dependency
    :rtype: str
    """

    tag_map = {'task': _add_task_tag,
               'metatask': _add_task_tag,
               'data': _add_data_tag,
               'cycleexist': _add_cycle_tag,
               'taskvalid': _add_taskvalid_tag,
               'streq': _add_streq_tag,
               'strneq': _add_streq_tag,
               'sh': _add_sh_tag}

    dep_condition = dep_dict.get('condition', None)
    dep_type = dep_dict.get('type', None)

    try:
        string = tag_map[dep_type](dep_dict)
    except KeyError:
        raise KeyError(f'{dep_type} is an unknown dependency type.\n' +
                       'Currently supported dependency types are:\n' +
                       f'{" | ".join(tag_map.keys())}')

    if dep_condition is not None:
        string = f'<{dep_condition}>{string}</{dep_condition}>'

    return string


def _add_task_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple task or metatask tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    """

    dep_type = dep_dict.get('type', None)
    dep_name = dep_dict.get('name', None)
    dep_offset = dep_dict.get('offset', None)

    if dep_name is None:
        msg = f'a {dep_type} name is necessary for {dep_type} dependency'
        raise KeyError(msg)

    string = '<'
    string += f'{dep_type}dep {dep_type}="{dep_name}"'
    if dep_offset is not None:
        string += f' cycle_offset="{dep_offset}"'
    string += '/>'

    return string


def _add_data_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple data tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    """

    dep_type = dep_dict.get('type', None)
    dep_data = dep_dict.get('data', None)
    dep_offset = dep_dict.get('offset', None)
    dep_age = dep_dict.get('age', None)

    if dep_data is None:
        msg = f'a data value is necessary for {dep_type} dependency'
        raise KeyError(msg)

    if not isinstance(dep_data, list):
        dep_data = [dep_data]

    if not isinstance(dep_offset, list):
        dep_offset = [dep_offset]

    assert len(dep_data) == len(dep_offset)

    if dep_age is None:
        strings = ['<datadep>']
    else:
        strings = [f'<datadep age="{dep_age}">']
    for data, offset in zip(dep_data, dep_offset):
        if '@' in data:
            offset_str = '' if offset in [None, ''] else f' offset="{offset}"'
            offset_string_b = f'<cyclestr{offset_str}>'
            offset_string_e = '</cyclestr>'
        else:
            offset_string_b = ''
            offset_string_e = ''

        strings.append(f'{offset_string_b}{data}{offset_string_e}')

    strings.append('</datadep>')

    return ''.join(strings)


def _add_cycle_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple cycle exist tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    """

    dep_type = dep_dict.get('type', None)
    dep_offset = dep_dict.get('offset', None)

    if dep_offset is None:
        msg = f'an offset value is necessary for {dep_type} dependency'
        raise KeyError(msg)

    string = f'<cycleexistdep cycle_offset="{dep_offset}"/>'

    return string


def _add_taskvalid_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a validtask tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto validtask dependency
    :rtype: str
    """

    dep_type = dep_dict.get('type', None)
    dep_name = dep_dict.get('name', None)

    if dep_name is None:
        msg = f'a {dep_type} name is necessary for {dep_type} dependency'
        raise KeyError(msg)

    string = f'<{dep_type} task="{dep_name}"/>'

    return string


def _add_streq_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple string comparison tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    """

    dep_type = dep_dict.get('type', None)
    dep_left = dep_dict.get('left', None)
    dep_right = dep_dict.get('right', None)

    fail = False
    msg = ''
    if dep_left is None:
        msg += f'a left value is necessary for {dep_type} dependency'
        fail = True
    else:
        dep_left = str(dep_left)
    if dep_right is None:
        if fail:
            msg += '\n'
        msg += f'a right value is necessary for {dep_type} dependency'
        fail = True
    else:
        dep_right = str(dep_right)
    if fail:
        raise KeyError(msg)

    if '@' in dep_left:
        dep_left = f'<cyclestr>{dep_left}</cyclestr>'
    if '@' in dep_right:
        dep_right = f'<cyclestr>{dep_right}</cyclestr>'

    string = f'<{dep_type}><left>{dep_left}</left><right>{dep_right}</right></{dep_type}>'

    return string


def _add_sh_tag(dep_dict: Dict[str, Any]) -> str:
    """
    create a simple shell execution tag
    :param: dep_dict: shell command to execute
    :type dep_dict: dict
    :return: Rocoto simple shell execution dependency
    :rtype: str
    """

    shell = dep_dict.get('shell', '/bin/sh')
    command = dep_dict.get('command', 'echo "Hello World"')

    if '@' in command:
        offset_string_b = f'<cyclestr>'
        offset_string_e = '</cyclestr>'
    else:
        offset_string_b = ''
        offset_string_e = ''
    cmd = f'{offset_string_b}{command}{offset_string_e}'

    string = f'<sh shell="{shell}">{cmd}</sh>'

    return string


def _traverse(o, tree_types=(list, tuple)):
    """
    Traverse through a list of lists or tuples and yield the value
    Objective is to flatten a list of lists or tuples
    :param o: list of lists or not
    :type o: list, tuple, scalar
    :param tree_types: trees to travers
    :type tree_types: tuple
    :return: value in the list or tuple
    :rtype: scalar
    """

    if isinstance(o, tree_types):
        for value in o:
            for subvalue in _traverse(value, tree_types):
                yield subvalue
    else:
        yield o


def create_dependency(dep_condition=None, dep=[]) -> List[str]:
    """
    create a compound dependency given a list of dependencies, and compounding condition
    the list of dependencies are created using add_dependency
    :param dep_condition: dependency condition
    :type dep_condition: boolean e.g. and, or, true, false
    :param dep: dependency
    :type dep: str or list
    :return: Rocoto compound dependency
    :rtype: list
    """

    dep = dep if isinstance(dep, list) else [dep]

    strings = []

    if len(dep) > 0:
        if dep_condition is not None:
            strings.append(f'<{dep_condition}>')

        for d in dep:
            if dep_condition is None:
                strings.append(f'{d}')
            else:
                for e in _traverse(d):
                    strings.append(f'\t{e}')

        if dep_condition is not None:
            strings.append(f'</{dep_condition}>')

    return strings


def create_envar(name: str, value: Union[str, float, int]) -> str:
    """
    create a Rocoto environment variable given name and value
    returns the environment variable as a string
    :param name: name of the environment variable
    :type name: str
    :param value: value of the environment variable
    :type value: str or float or int or unicode
    :return: Rocoto environment variable key-value pair
    :rtype: str
    """

    return f'<envar><name>{name}</name><value>{str(value)}</value></envar>'


def create_cycledef(group=None, start=None, stop=None, step=None):
    """
    create a Rocoto cycle definition
    returns the environment variable as a string
    :param group: cycle definition group name
    :type group: str
    :param start: cycle start datetime
    :type start: str
    :param step: cycle interval (timedelta)
    :type stop: str
    :param step: cycle interval (timedelta)
    :return: Rocoto cycledef variable string
    :rtype: str
    """

    return f'<cycledef group="{group}">{start} {stop} {step}</cycledef>'


def create_entity(name: str, value: Union[str, float, int]) -> str:
    """
    create an XML ENTITY variable given name and value
    returns the variable as a string
    :param name: name of the variable
    :type name: str
    :param value: value of the variable
    :type value: str or float or int or unicode
    :return: XML entity variable key-value pair
    :rtype: str
    """

    return f'<!ENTITY {name} "{str(value)}">'
