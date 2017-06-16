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
        shellvars.py

    AUTHOR:
        Rahul.Mahajan
        rahul.mahajan@noaa.gov


    INPUT:
        str or list of shell scripts to source

    THANKS:
        module shellvars.py from the Internet, heavily modified and adapted for local use.
'''

import os as _os
from subprocess import Popen as _Popen
from subprocess import PIPE as _PIPE

class ShellScriptException(Exception):
    def __init__(self, scripts, errors):
        self.scripts = scripts
        self.errors = errors
        msg = []
        msg.append("Error processing:\n")
        for script in scripts:
            msg.append('   %s\n' % script)

        msg.append('Error:\n')
        msg.append(errors)
        ''.join(msg)

        print ''.join(msg)

        Exception.__init__(self, 'ABORT!')


class ShellVars():
    """
    Module that sources the shell script and returns an object that can be used to get a key
    value pair for a single variable or all variables or just a list of variables defined in the
    script.
    ShellVars.list_vars : return a list list all variables in the script
    ShellVars.get_vars  : return a dictionary of key value pairs for all variables in the script
    ShellVars.get_var   : return a key value pair for desired variable in the script
    """

    def __init__(self, scripts, ignore=None):
        """
        Given a shell script or a list of shell scripts, initializes the class ShellVars
        :param scripts: Path to the shell scripts
        :type scripts: list or str or unicode
        :param ignore: variable names to ignore.  By default we ignore variables
                        that env injects into the script's environment.
                        See IGNORE_DEFAULT.
        :type ignore: iterable
        """
        IGNORE_DEFAULT = set(["SHLVL", "PWD", "_"])

        self.scripts = scripts if isinstance(scripts,list) else [scripts]
        self.ignore = IGNORE_DEFAULT if ignore is None else ignore
        self._shell_cmd = self._shell_command()

        return

    def _shell_command(self):

        sc = []
        for script in self.scripts:
            if _os.path.isfile(script):
                sc.append('. %s > /dev/null 2>&1' % script)
            else:
                raise self._noscripterror(script)

        return ' ; '.join(sc)


    def _noscripterror(self,script):
        return IOError("File does not exist: %s" % script)


    def list_vars(self):
        """
        Given a shell script, returns a list of shell variable names.
        Note: this method executes the script, so beware if it contains side-effects.
        :return: Key value pairs representing the environment variables defined
                in the script.
        :rtype: list
        """

        cmd = ['bash']

        # Get all variables in the environment
        input = ("""env | awk -F = '/[a-zA-Z_][a-zA-Z_0-9]*=/ """ +
                 """{ if (!system("[ -n \\"${" $1 "}\\" ]")) print $1 }'""")
        p = _Popen(cmd, stdout=_PIPE, stdin=_PIPE, stderr=_PIPE)
        stdout_env, stderr_env = p.communicate(input=input)
        lstdout_env = stdout_env.split()

        # Get all variables in the environment + those in the script
        input = ("""%s; env | awk -F = '/[a-zA-Z_][a-zA-Z_0-9]*=/ """ % self._shell_cmd +
                 """{ if (!system("[ -n \\"${" $1 "}\\" ]")) print $1 }'""")
        p = _Popen(cmd, stdout=_PIPE, stdin=_PIPE, stderr=_PIPE)
        stdout_data, stderr_data = p.communicate(input=input)
        lstdout_data = stdout_data.split()

        # Remove variables from the default environment, keeping only those in the script
        lines = list(set(lstdout_data).difference(lstdout_env))

        # Return variables without the ones being ignored
        return [elt for elt in lines if elt not in self.ignore]


    def get_vars(self):
        """
        Gets the values of environment variables defined in a shell script.
        Note: this method executes the script potentially many times.
        :return: Key value pairs representing the environment variables defined
                in the script.
        :rtype: dict
        """

        # Iterate over every var independently:
        # This is slower than using env, but enables us to capture multiline variables
        return dict((var, self.get_var(var)) for var in self.list_vars())


    def get_var(self, var):
        """
        Given the name of an environment variable, returns the
        value of the environment variable.
        :param var: environment variable name
        :type var: str or unicode
        :return: str
        """
        input = '%s; echo -n "$%s"\n'% (self._shell_cmd, var)
        pipe = _Popen(["bash"], stdout=_PIPE, stdin=_PIPE, stderr=_PIPE)
        stdout_data, stderr_data = pipe.communicate(input=input)
        if stderr_data:
            raise ShellScriptException(self.scripts, stderr_data)
        else:
            return stdout_data

