import sys, random, subprocess, os, glob
from datetime import datetime

DATE_ENV_VARS=['CDATE','SDATE','EDATE']
SCHEDULER_MAP={'ZEUS':'moabtorque',
               'THEIA':'moabtorque',
               'WCOSS':'lsf',
               'WCOSS_C':'lsfcray'}

class UnknownMachineError(Exception): pass
class UnknownConfigError(Exception): pass
class ShellScriptException(Exception):
    def __init__(self,scripts,errors):
        self.scripts = scripts
        self.errors = errors
        super(ShellScriptException,self).__init__(
            str(errors)+
            ': error processing'+
            (' '.join(scripts)))

def get_shell_env(scripts):
    vars=dict()
    runme=''.join([ 'source %s ; '%(s,) for s in scripts ])
    magic='--- ENVIRONMENT BEGIN %d ---'%random.randint(0,64**5)
    runme+='/bin/echo -n "%s" ; /usr/bin/env -0'%(magic,)
    with open('/dev/null','wb+') as null:
        env=subprocess.Popen(runme,shell=True,stdin=null.fileno(),
                       stdout=subprocess.PIPE)
        (out,err)=env.communicate()
    begin=out.find(magic)
    if begin<0:
        raise ShellScriptException(scripts,'Cannot find magic string; '
                                   'at least one script failed: '+repr(out))
    for entry in out[begin+len(magic):].split('\x00'):
        iequal=entry.find('=')
        vars[entry[0:iequal]] = entry[iequal+1:]
    return vars

def get_script_env(scripts):
    default_env=get_shell_env([])
    and_script_env=get_shell_env(scripts)
    vars_just_in_script=set(and_script_env)-set(default_env)
    union_env=dict(default_env)
    union_env.update(and_script_env)
    return dict([ (v,union_env[v]) for v in vars_just_in_script ])

def cast_or_not(type,value):
    try:
        return type(value)
    except ValueError:
        return value

def script_config_parser(files,date_env_vars=DATE_ENV_VARS):
    """
    Given the name of config file, key-value pair of all variables in the config file is returned as a dictionary
    :param filename: config file
    :type filename: str or unicode
    :return: Key value pairs representing the environment variables defined
            in the script.
    :rtype: dict
    """
    if isinstance(files,basestring):
        files=[files]
    varbles=dict()
    for key,value in get_script_env(files).iteritems():
        if key in date_env_vars: # likely a date, convert to datetime
            varbles[key] = datetime.strptime(value,'%Y%m%d%H')
        elif '.' in value: # Likely a number and that too a float
            varbles[key] = cast_or_not(float,value)
        else: # Still could be a number, may be an integer
            varbles[key] = cast_or_not(int,value)
    return varbles

def get_scheduler(machine):
    """Determine the scheduler"""
    try:
        return SCHEDULER_MAP[machine]
    except KeyError:
        raise UnknownMachineError('Unknown machine: %s'%(machine,))


def find_config(config_name, configs):

    for config in configs:
        if config_name == os.path.basename(config):
            return config

    raise UnknownConfigError("%s does not exist (known: %s), ABORT!" % (
        config_name,repr(basenames)))


def get_configs(expdir):
    """
        Given an experiment directory containing config files,
        return a list of configs minus the ones ending with ".default"
    """
    result=list()
    for config in glob.glob('%s/config.*' % expdir):
        if not config.endswith('.default'):
            result.append(config)
    return result


