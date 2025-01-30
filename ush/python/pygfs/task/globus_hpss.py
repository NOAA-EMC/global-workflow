#!/usr/bin/env python3

import os
from logging import getLogger
from pathlib import Path
from typing import Any, Dict, List

from wxflow import AttrDict, Task, to_YMD, to_YMDH, strftime, logit, parse_yaml, Jinja, which, ProcessError

logger = getLogger(__name__.split('.')[-1])


class GlobusHpss(Task):
    """Task to send tarballs (created by the archive task) to HPSS via Globus
       NOTE: For this to work, an entry in ~/.ssh/config titled "niagara" must
             be present.  If it is not, then see the wiki on how to set it up.
             TODO: Add link to the wiki.
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
        cycle_YMD = to_YMD(self.task_config.current_cycle),
        cycle_YMDH = to_YMDH(self.task_config.current_cycle),
        cycle_HH = strftime(self.task_config.current_cycle, '%H')

        # Instantiate all of the executables we will need to run
        self.forsven = which("forsven")
        self.scp = which("scp")
        self.ssh = which("ssh")

        if self.forsven is None:
            raise FileNotFoundError("FATAL ERROR Could not find the forsven executable!")
        if self.scp is None:
            raise FileNotFoundError("FATAL ERROR Could not find scp!")
        if self.ssh is None:
            raise FileNotFoundError("FATAL ERROR Could not find ssh!")

        # Get the user's server username from their ~/.ssh/config file

        if self.ssh is None:
            raise FileNotFoundError("FATAL ERROR Failed to locate ssh!")

        server_name = self.task_config.SERVER_NAME

        try:
            ssh_output = self.ssh("-G", f"{server_name}", output=str)
        except ProcessError as pe:
            raise ProcessError("FATAL ERROR No host information on niagara!\n"
                               f"Please add an entry for {server_name} into ~/.ssh/config!") from pe

        # Parse the ssh output to find the user's Niagara username
        ssh_output = ssh_output.split("\n")
        for line in ssh_output:
            if line.startswith("user "):
                server_username = line.split()[1]

        # Update the home directory on the server with the username
        server_home = self.task_config.SERVER_HOME.replace(
                                 "{{LOGNAME}}", server_username
                                )

        logger.debug(f"Server username detected as {server_username}")

        local_dict = AttrDict({
            'sven_dropbox': (f"{self.task_config.SVEN_DROPBOX_ROOT}"),
            'doorman_gendel': (f"{server_home}/GENERAL_DELIVERY/"
                               f"{self.task_config.PSLOT}/{self.task_config.RUN}.{cycle_YMD}/{cycle_HH}"),
            'hpss_target_dir': f"{self.task_config.ATARDIR}/{cycle_YMDH}",
            'server_home': server_home
        })

        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def configure(self, globus_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Collects the list of tarballs created by the arch task and writes instructions to
        send them to HPSS via Globus and verify success.

        There are two services running that handle passing and running scripts.
        On the client (e.g. Hercules), there is Sven.  On the server (i.e. Niagara), there is
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
        backup_yaml = os.path.join(com_conf, globus_dict.DATASETS_YAML)

        # Parse the list of tarballs to archive
        if os.path.isfile(backup_yaml):
            backup_set = AttrDict(**parse_yaml(backup_yaml))
        else:
            raise FileNotFoundError("Backup tarball YAML is missing! ({backup_yaml})")

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

        # Parse the doorman setup script
        doorman_jinja = os.path.join(globus_parm, "run_doorman.sh.j2")
        doorman_script = Jinja(doorman_jinja, data=globus_dict, allow_missing=False).render

        # Write a script with the location of the dropbox on the client
        dm_conf = f'export dropbox="{globus_dict.sven_dropbox}"'

        # Make the dropbox and clean it out
        Path(globus_dict.sven_dropbox).mkdir(exist_ok=True)

        # Parse the return script
        return_jinja = os.path.join(globus_parm, "return.sh.j2")
        return_script = Jinja(return_jinja, data=globus_dict, allow_missing=False).render

        # Create a todo script for rstprod and non-rstprod tarballs
        todo_jinja = os.path.join(globus_parm, "todo.sh.j2")
        todo_script = Jinja(todo_jinja, data=globus_dict, allow_missing=False).render
        transfer_sets["standard"]["todo"] = todo_script

        rstprod_todo_jinja = os.path.join(globus_parm, "rstprod_todo.sh.j2")
        rstprod_todo_script = Jinja(rstprod_todo_jinja, data=globus_dict, allow_missing=False).render
        transfer_sets["rstprod"]["todo"] = rstprod_todo_script

        # Create a common verify script for all tarballs
        vrfy_jinja = os.path.join(globus_parm, "verify.sh.j2")
        vrfy_script = Jinja(vrfy_jinja, data=globus_dict, allow_missing=False).render

        # Add common scripts to both standard and rstprod
        for transfer_set in transfer_sets:
            transfer_sets[transfer_set]["run_doorman.sh"] = doorman_script
            transfer_sets[transfer_set]["dm.conf"] = dm_conf
            transfer_sets[transfer_set]["return"] = return_script
            transfer_sets[transfer_set]["verify"] = vrfy_script
            transfer_sets[transfer_set]["server_name"] = globus_dict.SERVER_NAME
            transfer_sets[transfer_set]["homedir"] = (
                f"{globus_dict.server_home}/doorman/{globus_dict.jobid}/"
                f"{transfer_set}"
            )

        return transfer_sets

    @logit(logger)
    def execute_transfer_data(self, transfer_set: Dict[str, Any]) -> None:
        """Interface function with Sven to send tarballs to HPSS via Niagara.

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

        # Make run_doorman.sh executable
        os.chmod("run_doorman.sh", 0o740)

        server_homedir = transfer_set["homedir"]
        server_name = transfer_set["server_name"]

        # Tell Sven we have files to send, one at a time
        for location in transfer_set["locations"]:
            print(location)
            with open("location", "w") as location_f:
                location_f.write(location+"\n")
            try:
                logger.info(f"Preparing package for {location}")
                self.forsven(output=str.split)
            except ProcessError as pe:
                raise ProcessError("FATAL ERROR Sven failed to package the request"
                                   f"for {location}") from pe

        # Transfer the doorman script to Niagara.
        # Note, this assumes we have unattended transfer capability.
        try:
            # Start by making the directory it will run in
            logger.debug(f"Making the run directory {server_homedir}/doorman_rundir on {server_name}")
            self.ssh("-tt", server_name, f"mkdir -p {server_homedir}/doorman_rundir", output=str.split, error=str.split)
        except ProcessError as pe:
            raise ProcessError("FATAL ERROR Failed to create temporary working directoryon Niagara") from pe

        try:
            # Now transfer and rename the script
            server_run_script = f"{server_homedir}/doorman_rundir/run_doorman.sh"
            logger.debug(f"Transfer run_doorman.sh to {server_name}:{server_run_script}")
            self.scp(
                "run_doorman.sh", f"{server_name}:{server_run_script}",
                output=str.split, error=str.split
            )
        except ProcessError as pe:
            raise ProcessError("FATAL ERROR Failed to send doorman run script to Niagara") from pe

        # Now actually run the doorman script
        try:
            logger.debug(f"Run {server_run_script} remotely")
            self.ssh(
                     "-tt", server_name, f"{server_run_script}",
                     output=str.split, error=str.split
            )
        except ProcessError as pe:
            # Try and retrieve the log file
            try:
                self.scp(f"{server_name}:{server_homedir}/run_doorman.log", ".")
            except ProcessError:
                logger.warning("WARNING unable to transfer the doorman log back after failure")
            else:
                logger.info("The doorman failed to run.  Printing output of the log:")
                with open('run_doorman.log', 'r') as doorman_log:
                    print(doorman_log.read())

            raise ProcessError(f"FATAL ERROR Failed to run the Doorman service on {server_name}") from pe

        # Retrieve and print the Doorman log file from the server
        try:
            self.scp(f"{server_name}:{server_homedir}/run_doorman.log", '.')
            with open('run_doorman.log', 'r') as doorman_log:
                print(doorman_log.read())

        except ProcessError as pe:
            raise ProcessError("FATAL ERROR Failed to retrieve the doorman log file from {server_name}") from pe

        # Lastly, check the response from the doorman in Sven's dropbox
        # TODO

        return

    @logit(logger)
    def clean(self):
        """
        Remove the temporary directories/files created by the GlobusHpss task.
        """

        return
