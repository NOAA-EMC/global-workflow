#!/usr/bin/env python3

import os
import logging
import shutil
from time import sleep
from typing import Any, Dict, List
import re
import copy
from datetime import datetime, timezone

from wxflow import (AttrDict, Task, to_YMDH, logit, parse_yaml, Jinja, which, ProcessError, to_datetime,
                    CommandNotFoundError)

logger = logging.getLogger(__name__.split('.')[-1])
logging.basicConfig(encoding='utf-8', level=logging.DEBUG, format='%(asctime)s %(message)s')


class GlobusHpss(Task):
    """Task to send tarballs (created by the archive task) to HPSS via Globus
    """

    @logit(logger, name="GlobusHpss")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the GlobusHpss task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        # Declare these here so the jinja-templated scripts can be shellchecked
        cycle_YMDH = to_YMDH(self.task_config.current_cycle)

        # Instantiate all of the executables we will need to run
        try:
            self.forsven = which("forsven", required=True)
        except CommandNotFoundError:
            raise CommandNotFoundError("FATAL ERROR Could not find the forsven executable!")

        # TODO Move the globus interface to wxflow
        try:
            self.globus = which("globus", required=True)
        except CommandNotFoundError:
            raise FileNotFoundError("FATAL ERROR Could not find the globus command!")

        self.wd = os.getcwd()

        # Prep some globus commands
        self.globus_rm = copy.deepcopy(self.globus)
        self.globus_xfr = copy.deepcopy(self.globus)
        self.globus_mkdir = copy.deepcopy(self.globus)
        self.globus_wait = copy.deepcopy(self.globus)

        # Recursively remove the target, notify on failure, and ignore missing files
        self.globus_rm.add_default_arg(["rm", "--notify", "failed", "-f", "-r"])
        # Transfer file, notify on failure, preserve modification times, only
        # output task ID
        self.globus_xfr.add_default_arg(["transfer", "--notify", "failed",
                                         "--preserve-mtime", "--sync-level", "mtime",
                                         "--jmespath", "task_id", "--format=UNIX"])

        # Make a directory on a target system via globus
        self.globus_mkdir.add_default_arg(["mkdir", "--format=UNIX"])

        # Wait on a task ID to finish and output the status of the transfer when complete
        self.globus_wait.add_default_arg(["task", "wait", "--jmespath", "status",
                                          "--format=UNIX", "--timeout", "120"])

        self.CLIENT_GLOBUS_UUID = self.task_config.CLIENT_GLOBUS_UUID
        self.SERVER_GLOBUS_UUID = self.task_config.SERVER_GLOBUS_UUID
        self.server_home = self.task_config.SERVER_HOME
        self.server_name = self.task_config.SERVER_NAME

        local_dict = AttrDict({
            'sven_dropbox': (f"{self.task_config.SVEN_DROPBOX_ROOT}"),
            'hpss_target_dir': f"{self.task_config.ATARDIR}/{cycle_YMDH}",
            'server_home': f"{self.server_home}"
        })

        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def configure(self, globus_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Collects the list of tarballs created by the arch task and writes instructions to
        send them to HPSS via Globus and verify success.

        There are two services running that handle passing and running scripts.
        On the client (e.g. Hercules), there is Sven.  On the server (i.e. Mercury), there is
        the Doorman.  Sven packages up the file list and scripts that need to run on the server
        and the Doorman executes the scripts on each of the files.  The six files involved are

        dm.conf - One line indicating the location of the the scripts on the client.
        location - The location of the file on the client to send to the server.
        todo - A bash script that executes on each file once they are transferred to the server.
               For our purposes, this is mainly pushing to HPSS and writing a log file with
               either "SUCCESS" or "FAILURE" as the last line.
        verify - A bash script that reads the log file to verify success.
        return - Where to send the output of verify (globus address and folder location).
        run_doorman.sh - A bash script to actually run the server-side service.  This will be
                         automated by GDIT at some point, but for now must be executed on the
                         client via a pseudo terminal (ssh -t).

        The configuration method separates the file list into rstprod and non-rstprod (standard)
        files, then constructs the dm.conf, todo, verify, return, and run_doorman.sh scripts in
        memory from Jinja templates.  The todo is different for rstprod so the data can be
        protected.

        Parameters
        ----------
        globus_dict : Dict[str, Any]
            Task specific keys, e.g. the name of the input YAML.

        Return
        ------
        transfer_sets : Dict[str, Any]
            Sets of tarballs and instructions for sending them to HPSS via Globus
        """

        globus_parm = os.path.join(globus_dict.PARMgfs, "globus")

        com_conf = globus_dict.COMIN_CONF

        # Collect the files and properties from the input YAML
        group = globus_dict.get("ENSGRP", -1)
        if group < 0:
            backup_yaml = os.path.join(com_conf, "backup_tarballs.yaml")
        else:
            backup_yaml = os.path.join(com_conf, f"backup_tarballs_group{group}.yaml")

        # Parse the list of tarballs to archive
        if os.path.isfile(backup_yaml):
            backup_set = AttrDict(**parse_yaml(backup_yaml))
        else:
            raise FileNotFoundError(f"Backup tarball YAML is missing! ({backup_yaml})")

        # Create a standard and rstprod backup sets for any restricted tarballs
        standard_backup_set = []
        rstprod_backup_set = []
        for archive_name in backup_set:
            if backup_set[archive_name]["has_rstprod"]:
                rstprod_backup_set.append(backup_set[archive_name]['target'])
            else:
                standard_backup_set.append(backup_set[archive_name]['target'])

        # Start parsing scripts and storing in the output dictionary
        transfer_sets = {
            "standard": {"locations": standard_backup_set},
            "rstprod": {"locations": rstprod_backup_set}
        }

        # Write a script with the location of the dropbox on the client
        dm_conf = f'export dropbox="{globus_dict.sven_dropbox}"'

        # Make the dropbox and clean it out
        if os.path.exists(globus_dict.sven_dropbox):
            shutil.rmtree(globus_dict.sven_dropbox)

        os.mkdir(globus_dict.sven_dropbox)

        # Parse the return script
        return_jinja = os.path.join(globus_parm, "return.j2")
        return_script = Jinja(return_jinja, data=globus_dict, allow_missing=False).render

        # Create a todo script for rstprod and non-rstprod tarballs
        todo_jinja = os.path.join(globus_parm, "todo.sh.j2")
        todo_script = Jinja(todo_jinja, data=globus_dict, allow_missing=False).render
        transfer_sets["standard"]["todo"] = todo_script

        rstprod_todo_jinja = os.path.join(globus_parm, "todo_rstprod.sh.j2")
        rstprod_todo_script = Jinja(rstprod_todo_jinja, data=globus_dict, allow_missing=False).render
        transfer_sets["rstprod"]["todo"] = rstprod_todo_script

        # Create a common verify script for all tarballs
        vrfy_jinja = os.path.join(globus_parm, "verify.sh.j2")
        vrfy_script = Jinja(vrfy_jinja, data=globus_dict, allow_missing=False).render

        # Create the server initialization script
        init_xfer_jinja = os.path.join(globus_parm, "init_xfer.sh.j2")
        init_xfer_script = Jinja(init_xfer_jinja, data=globus_dict, allow_missing=False).render

        # Keep a list of server run directories so they can be cleaned at the end of the job
        self._server_job_dirs = []

        # Add the remaining scripts and definitions to transfer_sets
        for transfer_set in transfer_sets:
            server_job_dir = f"{self.server_home}/doorman/{globus_dict.jobid}/{transfer_set}"
            transfer_sets[transfer_set]["server_job_dir"] = server_job_dir
            self._server_job_dirs.append(server_job_dir)

            # Render the run_doorman script
            doorman_dict = globus_dict
            doorman_dict["run_directory"] = server_job_dir
            doorman_jinja = os.path.join(globus_parm, "run_doorman.sh.j2")
            doorman_script = Jinja(doorman_jinja, data=doorman_dict, allow_missing=False).render
            transfer_sets[transfer_set]["run_doorman.sh"] = doorman_script

            # Common scripts
            transfer_sets[transfer_set]["dm.conf"] = dm_conf
            transfer_sets[transfer_set]["return"] = return_script
            transfer_sets[transfer_set]["verify"] = vrfy_script
            transfer_sets[transfer_set]["init_xfer.sh"] = init_xfer_script

        return transfer_sets

    @logit(logger)
    def execute_transfer_data(self, transfer_set: Dict[str, Any], has_rstprod: bool) -> None:
        """Interface function with Sven to send tarballs to HPSS via Mercury.

        Parameters
        ----------
        transfer_set: Dict[str, Any]
            Set of tarballs and properties applicable to their transfer.

        Return
        ------
        None
        """

        with open("dm.conf", "w") as conf_f:
            conf_f.write(transfer_set["dm.conf"])
        with open("todo", "w") as todo_f:
            todo_f.write(transfer_set["todo"])
        with open("verify", "w") as verify_f:
            verify_f.write(transfer_set["verify"])
        with open("return", "w") as return_f:
            return_f.write(transfer_set["return"])
        with open("run_doorman.sh", "w") as doorman_f:
            doorman_f.write(transfer_set["run_doorman.sh"])
        with open("init_xfer.sh", "w") as init_f:
            init_f.write(transfer_set["init_xfer.sh"])

        # Initialize the server
        self._init_server(transfer_set["server_job_dir"])

        # Initialize a list of status files.
        transfer_set["status_files"] = []
        transfer_set["xfer_ids"] = []
        transfer_set["completed"] = []
        transfer_set["successes"] = []

        # Tell Sven we have files to send, one at a time
        for location in transfer_set["locations"]:
            with open("location", "w") as location_f:
                location_f.write(location + "\n")
            try:
                logger.info(f"Preparing package for {location}")
                sven_output = self.forsven(output=str)
                logger.debug(sven_output)
            except ProcessError as pe:
                raise ProcessError("FATAL ERROR Sven failed to package the request "
                                   f"for {location}") from pe

            # Parse Sven's output to get the name of the return status file
            match = re.search("\"(status_.*)\" in your dropbox", sven_output)
            status_file = match.group(1)
            transfer_set["xfer_ids"].append(status_file.replace("status_", ""))
            transfer_set["status_files"].append(os.path.join(self.task_config.sven_dropbox, status_file))

            # Initialize 'completed' and 'success' to false for each file
            transfer_set["completed"].append(False)
            transfer_set["successes"].append(False)

        # Transfer the doorman script to Mercury.
        # Note, this assumes we have unattended transfer capability.
        try:
            # Now transfer and rename the script
            server_run_script = f"{transfer_set['server_job_dir']}/run_doorman.sh"
            logger.debug(f"Transfer run_doorman.sh to {self.server_name}:{server_run_script}")
            self._wait_on_task_id(self.globus_xfr(
                f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/run_doorman.sh",
                f"{self.SERVER_GLOBUS_UUID}:{server_run_script}",
                output=str, error=str
            ))

            logger.debug("Successfully transferred the doorman script")
        except (ProcessError, ConnectionError) as pe:
            raise ProcessError("FATAL ERROR Failed to send doorman run script to Mercury") from pe

        # Now wait for the doorman script to run via cron on Mercury.
        # Once complete, Sven's dropbox should fill up with status files.
        wait_count = 0
        sleep_time = 60  # s
        timeout_time = 5.75 * 3600  # s
        max_wait_count = int(timeout_time / sleep_time)

        # Initialize transfer status
        transfer_failed = False
        check_log_count = 0
        log_read = False
        logger.debug(f"Waiting for the service to complete on {self.server_name}")
        while not all(transfer_set["completed"]) and wait_count < max_wait_count:
            sleep(sleep_time)
            for i in range(len(transfer_set["locations"])):
                status_file = transfer_set["status_files"][i]
                if os.path.exists(status_file):
                    # If this is a new status file, check if the transfer was successful
                    if not transfer_set["completed"][i]:
                        transfer_set["completed"][i] = True
                        with open(status_file) as status_handle:
                            status_string = status_handle.readline().rstrip()
                            transfer_set["successes"][i] = status_string == f"status.{transfer_set['xfer_ids'][i]} SUCCESS"

                        if transfer_set["successes"][i]:
                            logger.info(f"Successfully archived {transfer_set['locations'][i]} to HPSS!")
                        else:
                            # Exit the loop immediately, but allow the log file to be downloaded.
                            if has_rstprod:
                                logger.error(
                                    f"FATAL ERROR HPSS archiving of restricted file {transfer_set['locations'][i]} failed!"
                                    "\nPlease verify that the file has been deleted from HPSS!"
                                )
                                transfer_failed = True
                                break
                            else:
                                logger.error(f"FATAL ERROR HPSS archiving failed for {transfer_set['locations'][i]}.")
                                transfer_failed = True

            # Retrieve the log file (if it exists) from the server and check if it failed
            try:
                self._wait_on_task_id(self.globus_xfr(
                    f"{self.SERVER_GLOBUS_UUID}:{transfer_set['server_job_dir']}/run_doorman.log",
                    f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/run_doorman.log",
                    output=str, error=str
                ))

            except (ProcessError, ConnectionError):
                check_log_count += 1
                if check_log_count > 3:
                    logger.error("FATAL ERROR Unable to retrieve the run_doorman.log file")
                    transfer_failed = True
            else:
                with open("run_doorman.log") as doorman_log:
                    doorman_lines = doorman_log.readlines()

                log_read = True

                if "FAILURE" in doorman_lines[-1]:
                    logger.error(f"FATAL ERROR The doorman failed to run on {self.server_name}")
                    transfer_failed = True

            if transfer_failed:
                break

            wait_count += 1
            wait_time = wait_count * sleep_time

            complete_count = sum(transfer_set["completed"])

            logger.debug(f"{complete_count} files transferred in {wait_time} seconds.")

        # Sleep a couple more seconds to ensure all status files finish transferring
        sleep(5)

        # Write out the log file if it is present
        if log_read:
            logger.debug('\n'.join(doorman_lines))

        # Check for a failed transfer and/or timeouts
        if transfer_failed or not all(transfer_set["successes"]):
            raise ProcessError("FATAL ERROR Some/all files failed to archive to HPSS")

        return

    @logit(logger)
    def _init_server(self, server_dir):
        # This method sends a request to create a working directory and transfers
        # the initialization script.

        req_file = f"req_mkdir.{self.task_config.jobid}"
        with open(req_file, "w") as mkdir_f:
            mkdir_f.write(f"{server_dir}")

        pslot = self.task_config.PSLOT

        try:
            self._wait_on_task_id(self.globus_xfr(
                f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/{req_file}",
                f"{self.SERVER_GLOBUS_UUID}:{self.server_home}/{req_file}",
                output=str.split, error=str.split
            ))

        except (ProcessError, ConnectionError):
            raise ProcessError("FATAL ERROR Failed to request a mkdir on the server!")

        try:
            self._wait_on_task_id(self.globus_mkdir(
                f"{self.CLIENT_GLOBUS_UUID}:{self.wd}", suppress_errors=True
            ))
        except ProcessError:
            logger.info("Globus reported that it could not create the directory.  This is likely because it already exists.  Continuing.")

        try:
            # If globus was unable to mkdir for another reason, this will fail.
            self._wait_on_task_id(self.globus_xfr(
                f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/init_xfer.sh",
                f"{self.SERVER_GLOBUS_UUID}:{self.server_home}/init_xfer_{pslot}.sh",
                output=str, error=str
            ))
        except (ProcessError, ConnectionError):
            raise ProcessError("FATAL ERROR Failed send the driver script to the server!")

        logger.info("Sleeping 1 minute to let the server initialize")
        sleep(60)

        # Check that the server initialized successfully
        try:
            self._wait_on_task_id(self.globus_xfr(
                f"{self.SERVER_GLOBUS_UUID}:{self.server_home}/{pslot}_crontab_active.log",
                f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/crontab.log",
                output=str, error=str
            ))
        except (ProcessError, ConnectionError) as pe:
            raise ProcessError(
                "FATAL ERROR failed to retrieve the server log file!\n"
                f"Check that the crontab is active on {self.server_name}."
            ) from pe

        # Check the date in the log
        with open("crontab.log", "r") as crontab_f:
            cron_date = crontab_f.read()

        cron_datetime = to_datetime(cron_date)
        # Establish the timezone
        cron_datetime = cron_datetime.replace(tzinfo=timezone.utc)
        cron_td = datetime.now(timezone.utc) - cron_datetime

        if cron_td.total_seconds() > 600:
            # The log file is too old (perhaps from another test case)
            raise ProcessError("FATAL ERROR The server failed to initialize!")

        logger.info("Server initialized successfully!")

    @logit(logger)
    def _wait_on_task_id(self, task_id, suppress_errors=False):

        # The task_id usually has a newline character at the end.  Strip that to begin.
        task_id = task_id.strip()

        status = self.globus_wait(task_id, output=str).strip()
        if status != "SUCCEEDED" and not suppress_errors:
            raise ConnectionError(f"Globus failed on task ID {task_id}")

    @logit(logger)
    def clean(self):
        """
        Remove the temporary directories/files created by the GlobusHpss task.
        """

        # Write requests to delete the working directories on Mercury
        req_file = f"req_rmdir.{self.task_config.jobid}"
        for job_dir in self._server_job_dirs:
            with open(req_file, "w") as rmdir_f:
                rmdir_f.write(f"{job_dir}")

            try:
                self._wait_on_task_id(self.globus_xfr(
                    f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/{req_file}",
                    f"{self.SERVER_GLOBUS_UUID}:{self.server_home}/{req_file}",
                    output=str, error=str
                ))
            except (ProcessError, ConnectionError):
                raise ProcessError("FATAL ERROR Failed to request an rmdir command on the server!")

            logger.info("Sleeping 5 minute to give the server time to delete the working directory")
            # It probably takes much less time than this, but it may take a little while at high res
            sleep(300)

            # If it was successful, then the request should be gone
            try:
                self._wait_on_task_id(self.globus_xfr(
                    f"{self.SERVER_GLOBUS_UUID}:{self.server_home}/{req_file}",
                    f"{self.CLIENT_GLOBUS_UUID}:{self.wd}/{req_file}",
                    output=str, error=str
                ))
                raise RuntimeError(f"FATAL ERROR Failed to delete the run directory on {self.server_name}")
            except (ProcessError, ConnectionError):
                pass

        return
