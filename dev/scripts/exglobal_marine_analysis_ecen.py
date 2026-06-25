#!/usr/bin/env python3
# exgdas_global_marine_analysis_ecen.py
# This script creates an MarineRecenter class
# and runs the initialize, run, and finalize  methods
import os
import sys

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
_workflow_dir = os.path.join(os.environ.get('HOMEglobal', ''), 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

from wxflow import Logger, cast_strdict_as_dtypedict  # noqa: E402
from pygfs.task.marine_recenter import MarineRecenter  # noqa: E402

# Initialize root logger
logger = Logger(level='DEBUG', colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Inject COM_*_TMPL defaults from com_paths.py; environment values win.
    _com_defaults = get_com_templates()
    # Inject COM_*_TMPL templates: canonical defaults from com_paths.py
    # overridden by any matching values present in the environment.
    com_templates = get_com_templates()
    env_overrides = {k: v for k, v in os.environ.items()
                     if k.startswith('COM_') and k.endswith('_TMPL')}
    com_templates.update(env_overrides)
    config.update(com_templates)

    MarineRecen = MarineRecenter(config)
    MarineRecen.initialize()
    MarineRecen.execute('ens_handler')
    MarineRecen.finalize()
