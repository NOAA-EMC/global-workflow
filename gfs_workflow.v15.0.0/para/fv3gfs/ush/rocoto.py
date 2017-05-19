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
    metataskname = metatask_dict['metataskname'] if 'metataskname' in metatask_dict else 'demometatask'
    varname = metatask_dict['varname'] if 'varname' in metatask_dict else 'demovar'
    varval = metatask_dict['varval'] if 'varval' in metatask_dict else 1

    str_varval= str(varval)

    strings = []

    strings.append('<metatask name="%s">\n' % metataskname)
    strings.append('\n')
    strings.append('\t<var name="%s">%s</var>\n' % (varname, str_varval))
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
    taskname = task_dict['taskname'] if 'taskname' in task_dict else 'demotask'
    cycledef = task_dict['cycledef'] if 'cycledef' in task_dict else 'democycle'
    maxtries = task_dict['maxtries'] if 'maxtries' in task_dict else 3
    final = task_dict['final'] if 'final' in task_dict else None
    command = task_dict['command'] if 'command' in task_dict else 'sleep 10'
    jobname = task_dict['jobname'] if 'jobname' in task_dict else 'demojob'
    account = task_dict['account'] if 'account' in task_dict else 'batch'
    queue = task_dict['queue'] if 'queue' in task_dict else 'debug'
    walltime = task_dict['walltime'] if 'walltime' in task_dict else '00:01:00'
    log = task_dict['log'] if 'log' in task_dict else 'demo.log'
    native = task_dict['native'] if 'native' in task_dict else None
    resources = task_dict['resources'] if 'resources' in task_dict else None
    envar = task_dict['envar'] if 'envar' in task_dict else None
    dependency = task_dict['dependency'] if 'dependency' in task_dict else None

    str_maxtries = str(maxtries)
    str_final = '' if final is None else ' final="%s"' % final
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

    dep_condition= dep_dict['condition'] if 'condition' in dep_dict else None
    dep_name = dep_dict['name'] if 'name' in dep_dict else None
    dep_type = dep_dict['type'] if 'type' in dep_dict else None
    dep_offset = dep_dict['offset'] if 'offset' in dep_dict else None
    dep_data = dep_dict['data'] if 'data' in dep_dict else None

    if dep_type in [None]:
        string = '<'
    elif dep_type in ['task']:
        string = '<taskdep task="%s"' % dep_name
    elif dep_type in ['metatask']:
        string = '<metataskdep metatask="%s"' % dep_name
    elif dep_type in ['cycleexist']:
        if dep_offset is None:
            msg = 'dep_offset cannot be None if dep_type is cycleexist'
            raise msg
        string = '<cycleexistdep'
    elif dep_type in ['data']:
        if dep_data is None:
            msg = 'dep_data cannot be None if dep_type is data'
            raise msg
        string = '<datadep>'
    else:
        msg = 'unknown dependency type = %s' % dep_type
        raise msg

    if dep_type in ['data']:
        if dep_offset is not None:
            string += '<cyclestr offset="%s">%s</cyclestr>' % (dep_offset, dep_data)
        else:
            string += '%s' % (dep_data)

    if dep_type in ['data']:
        string += '</datadep>'
    else:
        string += '/>' if dep_offset is None else ' cycle_offset="%s"/>' % dep_offset

    if dep_condition is not None:
        string = '<%s>%s</%s>' % (dep_condition, string, dep_condition)

    return string


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
                strings.append('\t%s' % d)

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
