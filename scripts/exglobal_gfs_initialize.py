import os
import time

from pygw.logger import Logger, logit
from pygw.configuration import cast_strdict_as_dtypedict

from pygfs.ufswm.gfs import GFS

# ----

# Initialize root logger.
base_logger = Logger(level='DEBUG', colored_log=True)

# ----


def main() -> None:

    # Take configuration from environment and cast it as Python
    # dictionary.
    script_name = os.path.basename(__file__)
    start_time = time.time()
    config = cast_strdict_as_dtypedict(os.environ)

    for item in config:
        print(item)

    quit()
    

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
