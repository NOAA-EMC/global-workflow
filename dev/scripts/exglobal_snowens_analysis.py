#!/usr/bin/env python3
# exglobal_snowens_analysis.py
# This script creates an SnowEnsAnalysis class,
# which will compute the ensemble mean of the snow forecast,
# run a 2DVar analysis, and provide increments
# to create an ensemble of snow analyses
import os
import sys

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
_workflow_dir = os.path.join(os.environ.get('HOMEglobal', ''), 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

from wxflow import Logger, cast_strdict_as_dtypedict  # noqa: E402
from pygfs.task.snowens_analysis import SnowEnsAnalysis  # noqa: E402

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


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

    # Instantiate the snow ensemble analysis task
    snow_ens_anl = SnowEnsAnalysis(config)

    # Initialize JEDI 2DVar snow analysis
    snow_ens_anl.initialize()

    # Calculate ensemble mean
    snow_ens_anl.execute('esnowanlensmean')

    # stage ensemble mean backgrounds

    # Process SNOCVR and SNOMAD (if applicable)
    if snow_ens_anl.task_config.DO_SNOCVR_SNOMAD:
        snow_ens_anl.prepare_SNOCVR_SNOMAD()

    # Process IMS snow cover (if applicable)
    if snow_ens_anl.task_config.DO_IMS_SCF:
        snow_ens_anl.execute('scf_to_ioda')

    # Execute JEDI snow analysis
    snow_ens_anl.execute('snowanlvar')

    # Add increments
    snow_ens_anl.add_increments()

    # Finalize JEDI snow analysis
    snow_ens_anl.finalize()
