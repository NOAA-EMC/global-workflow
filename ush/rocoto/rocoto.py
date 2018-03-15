#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################
'''
    MODULE:
        rocoto.py

    ABOUT:
        Helper module to create tasks, metatasks, and dependencies

    AUTHOR:
        Rahul.Mahajan
        rahul.mahajan@noaa.gov
'''

def create_metatask(task_dict, metatask_dict):
    '''
    create a Rocoto metatask given a dictionary containing task and metatask information
    :param metatask_dict: metatask key-value parameters
    :type metatask_dict: dict
    :param task_dict: task key-value parameters
    :type task_dict: dict
    :return: Rocoto metatask
    :rtype: list
    '''

    # Grab metatask info from the metatask_dict
    metataskname = metatask_dict.get('metataskname', 'demometatask')
    varname = metatask_dict.get('varname', 'demovar')
    varval = metatask_dict.get('varval', 1)
    vardict = metatask_dict.get('vardict', None)

    strings = []

    strings.append('<metatask name="%s">\n' % metataskname)
    strings.append('\n')
    strings.append('\t<var name="%s">%s</var>\n' % (varname, str(varval)))
    if vardict is not None:
        for key in vardict.keys():
            value = str(vardict[key])
            strings.append('\t<var name="%s">%s</var>\n' % (key, value))
    strings.append('\n')
    tasklines = create_task(task_dict)
    for tl in tasklines:
        strings.append('%s' % tl) if tl == '\n' else strings.append('\t%s' % tl)
    strings.append('\n')
    strings.append('</metatask>\n')

    return strings


def create_task(task_dict):
    '''
    create a Rocoto task given a dictionary containing task information
    :param task_dict: task key-value parameters
    :type task_dict: dict
    :return: Rocoto task
    :rtype: list
    '''

    # Grab task info from the task_dict
    taskname = task_dict.get('taskname', 'demotask')
    cycledef = task_dict.get('cycledef', 'democycle')
    maxtries = task_dict.get('maxtries', 3)
    final = task_dict.get('final', False)
    command = task_dict.get('command', 'sleep 10')
    jobname = task_dict.get('jobname', 'demojob')
    account = task_dict.get('account', 'batch')
    queue = task_dict.get('queue', 'debug')
    walltime = task_dict.get('walltime', '00:01:00')
    log = task_dict.get('log', 'demo.log')
    native = task_dict.get('native', None)
    memory = task_dict.get('memory', None)
    resources = task_dict.get('resources', None)
    envar = task_dict.get('envar', None)
    dependency = task_dict.get('dependency', None)

    str_maxtries = str(maxtries)
    str_final = ' final="true"' if final else ''
    envar = envar if isinstance(envar, list) else [envar]

    strings = []

    strings.append('<task name="%s" cycledefs="%s" maxtries="%s"%s>\n' % \
            (taskname, cycledef, str_maxtries, str_final))
    strings.append('\n')
    strings.append('\t<command>%s</command>\n' % command)
    strings.append('\n')
    strings.append('\t<jobname><cyclestr>%s</cyclestr></jobname>\n' % jobname)
    strings.append('\t<account>%s</account>\n' % account)
    strings.append('\t<queue>%s</queue>\n' % queue)
    if resources is not None:
        strings.append('\t%s\n' % resources)
    strings.append('\t<walltime>%s</walltime>\n' % walltime)
    if memory is not None:
        strings.append('\t<memory>%s</memory>\n' % memory)
    if native is not None:
        strings.append('\t<native>%s</native>\n' % native)
    strings.append('\n')
    strings.append('\t<join><cyclestr>%s</cyclestr></join>\n' % log)
    strings.append('\n')

    if envar[0] is not None:
        for e in envar:
            strings.append('\t%s\n' % e)
        strings.append('\n')

    if dependency is not None:
        strings.append('\t<dependency>\n')
        for d in dependency:
            strings.append('\t\t%s\n' % d)
        strings.append('\t</dependency>\n')
        strings.append('\n')

    strings.append('</task>\n')

    return strings


def add_dependency(dep_dict):
    '''
    create a simple Rocoto dependency given a dictionary with dependency information
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple dependency
    :rtype: str
    '''

    dep_condition = dep_dict.get('condition', None)
    dep_type = dep_dict.get('type', None)

    if dep_type in ['task', 'metatask']:

        string = add_task_tag(dep_dict)

    elif dep_type in ['data']:

        string = add_data_tag(dep_dict)

    elif dep_type in ['cycleexist']:

        string = add_cycle_tag(dep_dict)

    elif dep_type in ['streq', 'strneq']:

        string = add_streq_tag(dep_dict)

    else:

        msg = 'Unknown dependency type %s' % dep_dict['type']
        raise KeyError(msg)

    if dep_condition is not None:
        string = '<%s>%s</%s>' % (dep_condition, string, dep_condition)

    return string


def add_task_tag(dep_dict):
    '''
    create a simple task or metatask tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    '''

    dep_type = dep_dict.get('type', None)
    dep_name = dep_dict.get('name', None)
    dep_offset = dep_dict.get('offset', None)

    if dep_name is None:
        msg = 'a %s name is necessary for %s dependency' % (dep_type, dep_type)
        raise KeyError(msg)

    string = '<'
    string += '%sdep %s="%s"' % (dep_type, dep_type, dep_name)
    if dep_offset is not None:
        string += ' cycle_offset="%s"' % dep_offset
    string += '/>'

    return string

def add_data_tag(dep_dict):
    '''
    create a simple data tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    '''

    dep_type = dep_dict.get('type', None)
    dep_data = dep_dict.get('data', None)
    dep_offset = dep_dict.get('offset', None)

    if dep_data is None:
        msg = 'a data value is necessary for %s dependency' % dep_type
        raise KeyError(msg)

    if dep_offset is None:
        if '@' in dep_data:
            offset_string_b = '<cyclestr>'
            offset_string_e = '</cyclestr>'
        else:
            offset_string_b = ''
            offset_string_e = ''
    else:
        offset_string_b = '<cyclestr offset="%s">' % dep_offset
        offset_string_e = '</cyclestr>'

    string = '<datadep>'
    string += '%s%s%s' % (offset_string_b, dep_data, offset_string_e)
    string += '</datadep>'

    return string

def add_cycle_tag(dep_dict):
    '''
    create a simple cycle exist tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    '''

    dep_type = dep_dict.get('type', None)
    dep_offset = dep_dict.get('offset', None)

    if dep_offset is None:
        msg = 'an offset value is necessary for %s dependency' % dep_type
        raise KeyError(msg)

    string = '<cycleexistdep cycle_offset="%s"/>' % dep_offset

    return string

def add_streq_tag(dep_dict):
    '''
    create a simple string comparison tag
    :param dep_dict: dependency key-value parameters
    :type dep_dict: dict
    :return: Rocoto simple task or metatask dependency
    :rtype: str
    '''

    dep_type = dep_dict.get('type', None)
    dep_left = dep_dict.get('left', None)
    dep_right = dep_dict.get('right', None)

    fail = False
    msg = ''
    if dep_left is None:
        msg += 'a left value is necessary for %s dependency' % dep_type
        fail = True
    if dep_right is None:
        if fail:
            msg += '\n'
        msg += 'a right value is necessary for %s dependency' % dep_type
        fail = True
    if fail:
        raise KeyError(msg)

    string = '<%s><left>%s</left><right>%s</right></%s>' % (dep_type, dep_left, dep_right, dep_type)

    return string


def _traverse(o, tree_types=(list, tuple)):
    '''
    Traverse through a list of lists or tuples and yeild the value
    Objective is to flatten a list of lists or tuples
    :param o: list of lists or not
    :type o: list, tuple, scalar
    :param tree_types: trees to travers
    :type tree_types: tuple
    :return: value in the list or tuple
    :rtype: scalar
    '''

    if isinstance(o, tree_types):
        for value in o:
            for subvalue in _traverse(value, tree_types):
                yield subvalue
    else:
        yield o


def create_dependency(dep_condition=None, dep=None):
    '''
    create a compound dependency given a list of dependendies, and compounding condition
    the list of dependencies are created using add_dependency
    :param dep_condition: dependency condition
    :type dep_condition: boolean e.g. and, or, true, false
    :param dep: dependency
    :type dep: str or list
    :return: Rocoto compound dependency
    :rtype: list
    '''

    dep = dep if isinstance(dep, list) else [dep]

    strings = []

    if dep_condition is not None:
        strings.append('<%s>' % dep_condition)

    if dep[0] is not None:
        for d in dep:
            if dep_condition is None:
                strings.append('%s' % d)
            else:
                for e in _traverse(d):
                    strings.append('\t%s' % e)

    if dep_condition is not None:
        strings.append('</%s>' % dep_condition)

    return strings


def create_envar(name=None,value=None):
    '''
    create an Rocoto environment variable given name and value
    returns the environment variable as a string
    :param name: name of the environment variable
    :type name: str
    :param value: value of the environment variable
    :type value: str or float or int or unicode
    :return: Rocoto environment variable key-value pair
    :rtype: str
    '''

    string = ''
    string += '<envar>'
    string += '<name>%s</name>' % name
    string += '<value>%s</value>' % str(value)
    string += '</envar>'

    return string
