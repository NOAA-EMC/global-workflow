#! /usr/env/bin python

# -----------------------------------------------------------------------------
#
# Program Name:          exglobal_ufs_forecast_init.py
#
# Author(s)/Contacts(s): Henry R. Winterbottom (henry.winterbottom@noaa.gov)
#
# Abstract:              A Python 3.5+ script to initialize a global UFS
#                        forecast application.
#
# History Log:
#
#   - 2023-03-28: Henry R. Winterbottom -- Original version.
#
# Usage: user@host:$ python exglobal_ufs_forecast_init.py
#
# -----------------------------------------------------------------------------

import os
import time

from pygw.logger import Logger
from pygw.configuration import cast_strdict_as_dtypedict

from pygfs.ufswm.gfs import GFS

# ----

logger = Logger()

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"

# ----


def main() -> None:
    """
    Description
    -----------

    This is the driver-level function to invoke the tasks within this
    script.

    """

    # Take configuration from environment and cast it as Python
    # dictionary.
    script_name = os.path.basename(__file__)
    start_time = time.time()
    config = cast_strdict_as_dtypedict(os.environ)

    # Launch the task.
    task = GFS(config=config)
    task.initialize()

    stop_time = time.time()
    msg = f"Completed application {script_name}."
    Logger().info(msg=msg)
    total_time = stop_time - start_time
    msg = f"Total Elapsed Time: {total_time} seconds."

# ----


if __name__ == '__main__':
    main()
