#!/usr/bin/env python3
# exglobal_snow_analysis.py
# This script creates an SnowAnalysis class
# and runs the initialize, execute and finalize methods
# for a global Snow Depth analysis
import os
import sys

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
_workflow_dir = os.path.join(os.environ.get('HOMEglobal', ''), 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

from wxflow import Logger, cast_strdict_as_dtypedict  # noqa: E402
from pygfs.task.snow_analysis import SnowAnalysis  # noqa: E402

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Inject COM_*_TMPL defaults from com_paths.py; environment values win.
    _com_defaults = get_com_templates()
    for _key in ('COM_ATMOS_RESTART_TMPL', 'COM_SNOW_ANALYSIS_TMPL'):
        if _key not in config and _key in _com_defaults:
            config[_key] = _com_defaults[_key]

    # Instantiate the snow analysis task
    snow_anl = SnowAnalysis(config)

    # Initialize JEDI 2DVar snow analysis
    snow_anl.initialize()

    # Process SNOCVR and SNOMAD (if applicable)
    if snow_anl.task_config.DO_SNOCVR_SNOMAD:
        snow_anl.prepare_SNOCVR_SNOMAD()

    # Process IMS snow cover (if applicable)
    if snow_anl.task_config.DO_IMS_SCF:
        snow_anl.execute('scf_to_ioda')

    # Execute JEDI snow analysis
    snow_anl.execute('snowanlvar')

    # Add increments
    snow_anl.add_increments()

    # Finalize JEDI snow analysis
    snow_anl.finalize()
