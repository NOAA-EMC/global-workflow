import glob
import os
import random
import subprocess
from datetime import datetime
from pathlib import Path
from pprint import pprint
from typing import Union, List, Dict, Any

from pygw.attrdict import AttrDict

__all__ = ['Configuration']


class ShellScriptException(Exception):
    def __init__(self, scripts, errors):
        self.scripts = scripts
        self.errors = errors
        super(ShellScriptException, self).__init__(
            str(errors) +
            ': error processing' +
            (' '.join(scripts)))


class UnknownConfigError(Exception):
    pass


class Configuration:
    """
    Configuration parser for the global-workflow
    (or generally for sourcing a shell script into a python dictionary)
    """

    DATE_ENV_VARS = ['CDATE', 'SDATE', 'EDATE']
    TRUTHS = ['y', 'yes', 't', 'true', '.t.', '.true.']
    BOOLS = ['n', 'no', 'f', 'false', '.f.', '.false.'] + TRUTHS
    BOOLS = [x.upper() for x in BOOLS] + BOOLS

    def __init__(self, config_dir: Union[str, Path]):
        """
        Given a directory containing config files (config.XYZ),
        return a list of config_files minus the ones ending with ".default"
        """

        self.config_dir = config_dir
        self.config_files = self._get_configs

    @property
    def _get_configs(self) -> List[str]:
        """
        Given a directory containing config files (config.XYZ),
        return a list of config_files minus the ones ending with ".default"
        """
        result = list()
        for config in glob.glob(f'{self.config_dir}/config.*'):
            if not config.endswith('.default'):
                result.append(config)

        return result

    def find_config(self, config_name: str) -> str:
        """
            Given a config file name, find the full path of the config file
        """

        for config in self.config_files:
            if config_name == os.path.basename(config):
                return config

        raise UnknownConfigError(
            f'{config_name} does not exist (known: {repr(config_name)}), ABORT!')

    def parse_config(self, files: Union[str, bytes, list]) -> Dict[str, Any]:
        """
        Given the name of config file(s), key-value pair of all variables in the config file(s)
        are returned as a dictionary
        :param files: config file or list of config files
        :type files: list or str or unicode
        :return: Key value pairs representing the environment variables defined
                in the script.
        :rtype: dict
        """
        if isinstance(files, (str, bytes)):
            files = [files]
        files = [self.find_config(file) for file in files]
        varbles = AttrDict()
        for key, value in self._get_script_env(files).items():
            if key in self.DATE_ENV_VARS:  # likely a date, convert to datetime
                varbles[key] = datetime.strptime(value, '%Y%m%d%H')
            elif value in self.BOOLS:  # Likely a boolean, convert to True/False
                varbles[key] = self._true_or_not(value)
            elif '.' in value:  # Likely a number and that too a float
                varbles[key] = self._cast_or_not(float, value)
            else:  # Still could be a number, may be an integer
                varbles[key] = self._cast_or_not(int, value)

        return varbles

    def print_config(self, files: Union[str, bytes, list]) -> None:
        """
        Given the name of config file(s), key-value pair of all variables in the config file(s) are printed
        Same signature as parse_config
        :param files: config file or list of config files
        :type files: list or str or unicode
        :return: None
        """
        config = self.parse_config(files)
        pprint(config, width=4)

    @classmethod
    def _get_script_env(cls, scripts: List) -> Dict[str, Any]:
        default_env = cls._get_shell_env([])
        and_script_env = cls._get_shell_env(scripts)
        vars_just_in_script = set(and_script_env) - set(default_env)
        union_env = dict(default_env)
        union_env.update(and_script_env)
        return dict([(v, union_env[v]) for v in vars_just_in_script])

    @staticmethod
    def _get_shell_env(scripts: List) -> Dict[str, Any]:
        varbls = dict()
        runme = ''.join([f'source {s} ; ' for s in scripts])
        magic = f'--- ENVIRONMENT BEGIN {random.randint(0,64**5)} ---'
        runme += f'/bin/echo -n "{magic}" ; /usr/bin/env -0'
        with open('/dev/null', 'w') as null:
            env = subprocess.Popen(runme, shell=True, stdin=null.fileno(),
                                   stdout=subprocess.PIPE)
            (out, err) = env.communicate()
        out = out.decode()
        begin = out.find(magic)
        if begin < 0:
            raise ShellScriptException(scripts, 'Cannot find magic string; '
                                       'at least one script failed: '+repr(out))
        for entry in out[begin+len(magic):].split('\x00'):
            iequal = entry.find('=')
            varbls[entry[0:iequal]] = entry[iequal+1:]
        return varbls

    @staticmethod
    def _cast_or_not(type, value):
        try:
            return type(value)
        except ValueError:
            return value

    @staticmethod
    def _true_or_not(value):
        try:
            return value.lower() in Configuration.TRUTHS
        except AttributeError:
            return value
