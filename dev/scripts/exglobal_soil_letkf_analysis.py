#!/usr/bin/env python3
# exglobal_soil_letkf_analysis.py
# (adapted from exglobal_snowens_analysis.py)
# This script creates a SoilLetkfAnalysis class instance from soil_letkf_analysis,
# which will process soil observations, run a LETKF analysis, and add increments
# to background ensembles to create an ensemble of soil analyses
import os

from wxflow import Logger, cast_strdict_as_dtypedict
from pygfs.task.soil_letkf_analysis import SoilLetkfAnalysis

# Initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


if __name__ == '__main__':

    # Take configuration from environment and cast it as python dictionary
    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the soil ensemble analysis task
    soil_letkf_anl = SoilLetkfAnalysis(config)

    # Initialize JEDI LETKF soil analysis
    soil_letkf_anl.initialize()

    # Execute JEDI soil analysis
    soil_letkf_anl.execute('soilletkfanl')

    # non-liau: Add increments & and smc/stc consistency adj  
    #TODO: recentering if needed
    if soil_letkf_anl.task_config.ADD_SOIL_INC:    
        soil_letkf_anl.add_increments()     

    # Finalize JEDI soil analysis
    soil_letkf_anl.finalize()
