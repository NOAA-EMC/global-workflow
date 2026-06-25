#!/usr/bin/env python3
# exglobal_marinebmat_initialize.py
# This script creates an marineBmat object
# and runs the execute method
# which executes all the steps necessary to create the global marine B-matrix
import os
import sys

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
_workflow_dir = os.path.join(os.environ.get('HOMEglobal', ''), 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

from wxflow import Logger, cast_strdict_as_dtypedict  # noqa: E402
from pygfs.task.marine_bmat import MarineBMat  # noqa: E402

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Inject COM_*_TMPL defaults from com_paths.py; environment values win.
    _com_defaults = get_com_templates()
    for _key in ('COM_OCEAN_HISTORY_TMPL', 'COM_ICE_HISTORY_TMPL'):
        if _key not in config and _key in _com_defaults:
            config[_key] = _com_defaults[_key]

    # Create an instance of the MarineBMat task
    marineBMat = MarineBMat(config)
    marineBMat.initialize()
