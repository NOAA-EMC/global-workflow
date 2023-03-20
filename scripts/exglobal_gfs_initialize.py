import os
import time

from schema import validate
from pygw.logger import Logger, logit
from pygw.configuration import cast_strdict_as_dtypedict

from pygfs.ufswm.gfs import GFS

# ----

# Initialize root logger.
base_logger = Logger(level='DEBUG', colored_log=True)

# ----


@logit(base_logger)
if __name__ == '__main__':

    # Define the schema attributes; proceed accordingly.
    cls_schema = {
        "yaml_file": str
    }

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
