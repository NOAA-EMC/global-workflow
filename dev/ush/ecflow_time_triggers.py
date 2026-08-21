#!/usr/bin/env python3

import ecflow
import sys
import os
import datetime
import re
import time
import operator

# TODO: Make this path a configurable parameter based on platform
#       This will require this script to be able to determine the platform
#       Then, use $HOMEglobal/workflow/hosts/$PLATFORM.yaml
# Assume WCOSS2
ROOT_DUMP_DIR = "/lfs/h2/emc/global/noscrub/emc.global/dump"


def find_tasks_with_time_triggers(node):
    """
    Recursively scans an ecFlow node structure to find Tasks that have
    a trigger expression evaluating time.
    TODO: Extend this function to also check for explicit time attributes (e.g., times, todays, crons) in addition to trigger expressions.
    """
    # matched_tasks will include the task name and the time-related information
    matched_tasks = []

    # If the current node is a Task, evaluate its time dependencies
    if isinstance(node, ecflow.Task):

        # Check for trigger expressions evaluating time variables (e.g., :TIME == 0600)
        has_time_expression = False
        trigger = node.get_trigger()
        if trigger:
            trigger_expr = str(trigger).upper()
            if "TIME" in trigger_expr:
                has_time_expression = True

        # If it passes, capture it
        if has_time_expression:
            time_attributes = {
                "trigger": str(trigger) if has_time_expression else None
            }

            matched_tasks.append([node, time_attributes])

    # If the node is a Suite or Family, recurse through its children
    if hasattr(node, 'nodes'):
        for child in node.nodes:
            matched_tasks.extend(find_tasks_with_time_triggers(child))

    return matched_tasks


def task_missed_launch_window(time_attributes, PDYcyc):
    """
    Determines if a task has missed its launch window based on its time attributes and the current date/time.
    """

    trigger_time_info = time_attributes.get("trigger")

    # Use ecflow's built-in time variables to determine if the task has missed its
    # launch window.

    # Get current time in UTC
    current_dt = datetime.datetime.utcnow()
    current_time = current_dt.strftime("%Y%m%d%H")
    PDYcyc_dt = datetime.datetime.strptime(PDYcyc, "%Y%m%d%H")
    PDYcyc_dt_plus_12h = PDYcyc_dt + datetime.timedelta(hours=12)

    # If the PDYcyc is more than 12 hous in the past, we can assume the task has missed its launch window.
    if current_dt > PDYcyc_dt_plus_12h:
        return True

    # Extract the time from the trigger expression
    if trigger_time_info:
        trigger_time_info = trigger_time_info.upper()
        # Count the number of :TIME operators (==, <, >, <=, >=)
        time_matches = re.findall(r":TIME\s*(==|<|>|<=|>=)\s*(\d{4})", trigger_time_info)
        if time_matches:
            count_times = len(time_matches)
            # If count is greather than 1, determine how they are combined
            if count_times > 1:
                # Connectors can be 'AND', 'OR', '&&', or '||'
                connectors = re.findall(r"\s*(AND|OR|&&|\|\|)\s*", trigger_time_info)
                len_connectors = len(connectors)
                # Connectors should be one less than the number of time matches
                # If not, something is wrong with the expression.
                if len_connectors != count_times - 1:
                    raise ValueError(f"Invalid trigger expression: {trigger_time_info}. Mismatched connectors and time matches.")

                if len_connectors > 0:
                    # Map the connectors to logical operators for evaluation
                    logical_ops = []
                    for connector in connectors:
                        if connector in ["AND", "&&"]:
                            logical_ops.append(operator.and_)
                        elif connector in ["OR", "||"]:
                            logical_ops.append(operator.or_)
                        elif connector != "==":
                            raise ValueError(f"Invalid connector in trigger expression: {connector}")

            # For now, assume one of two construcions:
            #    :TIME == HHMM     --- Launch if we are past the time HHMM
            #    :TIME >(=) HHMM and :TIME <(=) HHMM    --- Launch if we are past the second time
            # Get the last time match
            last_operator, last_time = time_matches[-1]
            # Convert last_time to a datetime object for comparison
            last_time_dt = datetime.datetime.strptime(last_time, "%H%M").replace(
                year=PDYcyc_dt.year, month=PDYcyc_dt.month, day=PDYcyc_dt.day
            )

            if last_time_dt < current_dt:
                return True
            else:
                # It is possible for the trigger to be on the next day
                # If last_time is less than "0600" and $cyc is "18", then the trigger is
                # for the next day.
                if last_time < "0600" and PDYcyc_dt.hour == 18:
                    last_time_dt_next_day = last_time_dt + datetime.timedelta(days=1)
                    if last_time_dt_next_day < current_dt:
                        return True

            # TODO: Expand this to handle more complex expressions and make use of
            #       the logical_ops list to evaluate the entire expression correctly.
            """ Partial solution to the TODO
            send_command = False
            for operator, trigger_time in time_matches:
                # Convert trigger_time to a datetime object for comparison
                trigger_dt = datetime.datetime.strptime(trigger_time, "%H%M").replace(
                    year=current_dt.year, month=current_dt.month, day=current_dt.day
                )

                # Compare current time with the trigger time based on the operator
                if operator == "==":
                    # Two or more time matches with '==' does not make sense
                    if len_connectors > 0:
                        raise ValueError(f"Invalid trigger expression: {trigger_time_info}. Multiple '==' operators with connectors.")
                    if current_dt >= trigger_dt:
                        send_command = True
                elif operator == "<":
                    if current_dt >= trigger_dt:
                        send_command = True
                        else:
                            return command
                    else:
                        if len_connectors > 1:
                            continue
                        else:
                            return None
            """

    return False


def build_trigger_command(task, time_attributes, PDYcyc):
    """
    Determines if a task has missed its launch window based on the current date/time and the provided runtime date/time (PDYcyc).
    Constructs an ecflow_client command to trigger a task immediately if the time window has passed.
    Otherwise returns None, indicating no action is needed.
    """

    task_path = task.get_abs_node_path()
    command = f"ecflow_client --run {task_path}"

    if task_missed_launch_window(time_attributes, PDYcyc):
        return command
    else:
        return None


def build_complete_command(task, time_attributes, PDYcyc):
    """
    Constructs an ecflow_client command to complete a task immediately.
    """

    task_path = task.get_abs_node_path()
    command = f"ecflow_client --force=complete {task_path}"

    if task_missed_launch_window(time_attributes, PDYcyc):
        return command
    else:
        return None


def build_syndata_copy_commands(complete_command, PDYcyc):
    """
    Builds a list of commands to copy syndata for skipped tasks instead of triggering them.
    """
    # Split PDYcyc into PDY (YYYYMMDD) and cyc (HH)
    PDY = PDYcyc[:8]
    cyc = PDYcyc[8:]

    # Get COMROOT to know where we are copying to
    comroot = get_comroot()

    copy_commands = []
    # Extract the task path from the command
    match = re.search(r"ecflow_client --force=complete .*/([^/]+)/prep/", complete_command)
    if match:
        task_path = match.group(1)
        # Get the RUN from the match (gdas or gfs)
        if "gdas" in task_path:
            run = "gdas"
        elif "gfs" in task_path:
            run = "gfs"
        else:
            raise ValueError(f"Unknown RUN type in task path: {task_path}")

        # Assuming the syndata is located in a specific directory structure
        source_path = f"{ROOT_DUMP_DIR}/{run}.{PDY}/{cyc}/atmos/{run}.t{cyc}z.syndata.tcvitals.tm00"
        dest_path = f"{comroot}/{run}.{PDY}/{cyc}/obs/{run}.t{cyc}z.syndata.tcvitals.tm00"

        # It is possible that the source file does not exist yet (i.e. operations
        # hasn't produced it yet or it hasn't been copied to the development machine
        # yet). The archive commands run every 15 minutes, wait up to 30.
        wait_count = 0
        while wait_count < 30:
            if os.path.exists(source_path):
                break
            else:
                print(f"Waiting for source file to exist: {source_path} (waited {wait_count} minutes)")
                time.sleep(60)
                wait_count += 1

        # Make directories verbosely
        print(f"Creating destination directory: {os.path.dirname(dest_path)}")
        os.makedirs(os.path.dirname(dest_path), exist_ok=True)

        copy_command = f"cp {source_path} {dest_path}"
        copy_commands.append(copy_command)
    return copy_commands


def get_comroot():
    # Get COMROOT from the version file
    # This is not straightforward as this is usually determined by compath.py in real
    # time, but we can simulate it by splicing the versions/run.wcoss2.ver file's
    # declaration of COMPATH. COMROOT will be the last entry, split by colons, then
    # appended by the run.wcoss2.ver file's `gfs_ver` variable's 'v<major>.<minor>'
    # value (ignore the patch version). This is a bit of a hack, but it should work for
    # now.
    # Get the path to this script's directory to build the path to
    # versions/run.wcoss2.ver
    script_dir = os.path.dirname(os.path.realpath(__file__))
    run_ver_path = os.path.join(script_dir, "..", "..", "versions", "run.wcoss2.ver")
    compath_line = None
    gfs_ver_line = None
    with open(run_ver_path, "r") as f:
        lines = f.readlines()

    for line in lines:
        # Strip any leading/trailing whitespace
        line = line.strip()

        # The lines may or may not start with export. Strip 'export ' if it is found
        # and then check for the variable name.
        line = line.replace("export ", "")

        if line.startswith("COMPATH"):
            compath_line = line
        elif line.startswith("gfs_ver"):
            gfs_ver_line = line

        if compath_line and gfs_ver_line:
            break

    if compath_line and gfs_ver_line:
        compath_value = compath_line.split("=")[1].strip().strip('"')
        gfs_ver_value = gfs_ver_line.split("=")[1].strip().strip('"')
        # Extract the major and minor version from gfs_ver_value
        gfs_ver_parts = gfs_ver_value.split(".")
        if len(gfs_ver_parts) >= 2:
            major_minor_version = f"{gfs_ver_parts[0]}.{gfs_ver_parts[1]}"
            COMROOT = os.path.join(compath_value.split(":")[-1], major_minor_version)
            print(f"Determined COMROOT: {COMROOT}")
            return COMROOT
        else:
            raise ValueError(f"Invalid gfs_ver format in {run_ver_path}: '{gfs_ver_value}'")
    else:
        if not compath_line:
            print(f"COMPATH not found in {run_ver_path}.")
        if not gfs_ver_line:
            print(f"gfs_ver not found in {run_ver_path}.")
        raise ValueError(f"Could not find COMPATH and/or gfs_ver in {run_ver_path}: {compath_line}, {gfs_ver_line}")


# This version of __main__ assumes a definition file.
# TODO: Add support for connecting to a running ecFlow server.
if __name__ == "__main__":
    # Arguments:
    #   # 1. Path to the ecFlow definition file
    #   # 2. Absolute path to the target family within the suite
    #   # 3. Runtime date/time in the format ${PDY}${cyc} (e.g., 2023091500)

    # This script assumes we are running on WCOSS2. Check if that is true by checking if
    # /lfs/h2 exists.
    if not os.path.exists("/lfs/h2"):
        raise ImplementationError("This script is designed to run on WCOSS2 for now.")

    if len(sys.argv) != 4:
        print("Usage: python ecflow_time_triggers.py <definition_file.def> <target_family_path> ${PDY}${cyc}")
        sys.exit(1)

    def_file_path = sys.argv[1]
    target_family_path = sys.argv[2]
    PDYcyc = sys.argv[3]

    # Check that PDYcyc is a valid YYYYMMDDHH format
    if not re.match(r"^\d{10}$", PDYcyc):
        raise ValueError(f"Invalid PDYcyc format: '{PDYcyc}'. Expected format is YYYYMMDDHH.")

    # Check the date format actually represents a valid year, month, day, and hour
    yyyy = int(PDYcyc[0:4])
    mm = int(PDYcyc[4:6])
    dd = int(PDYcyc[6:8])
    hh = int(PDYcyc[8:10])
    if not (1 <= mm <= 12 and 1 <= dd <= 31 and 0 <= hh <= 23 and yyyy >= 2000 and yyyy <= 2100):
        raise ValueError(f"Invalid PDYcyc value: '{PDYcyc}'. Date and time values are out of range.")

    try:
        # Load the file directly into memory
        print(f"Loading definition file: {def_file_path}...")
        defs = ecflow.Defs(def_file_path)

        # Check that defs is not empty
        if defs is None or len(defs) == 0:
            raise ValueError(f"Definition file '{def_file_path}' is empty or could not be loaded.")

        # Define the absolute path to your target family
        target_node = defs.find_abs_node(target_family_path)

        if target_node:
            print(f"Scanning tasks under: {target_family_path}...")
            time_tasks = find_tasks_with_time_triggers(target_node)

            print(f"\n--- Found {len(time_tasks)} Task(s) ---")
            commands = []
            for task in time_tasks:
                print(f"Task: {task[0].get_abs_node_path()}, Time Attributes: {task[1]}")
                # Check the current date/time, runtime date/time, and determine
                # if the task has missed its launch window. If so, write an
                # ecflow_client (shell) command to trigger it immediately.
                # Write all commands trigger_timed_tasks.sh.

                # If we missed a tropcy_qc window, skip it. These jobs are not meant to be
                # triggered outside of real-time. Instead, we will 'complete' them
                # and copy the syndata they produce from an archive.
                if "tropcy_qc" in task[0].get_abs_node_path():
                    complete_command = build_complete_command(task[0], task[1], PDYcyc)

                    if complete_command:
                        print("Task has missed its launch window. Will complete the task and copy syndata instead of triggering it.")
                        # Build the copy commands
                        # these should execute before the complete command so we have a valid state.
                        # If the copy fails, don't complete the task.
                        copy_commands = build_syndata_copy_commands(complete_command, PDYcyc)
                        commands.extend(copy_commands)
                        commands.append(complete_command)
                        print(commands)
                else:
                    command = build_trigger_command(task[0], task[1], PDYcyc)
                    # If command is not None, we'll run it.
                    if command:
                        commands.append(command)

            if len(commands) > 0:
                with open("trigger_timed_tasks.sh", "w") as f:
                    f.write("#!/bin/bash\nset -ex\n")
                    for cmd in commands:
                        f.write(f"{cmd}\n")

                print("\nCommands to trigger tasks have been written to 'trigger_timed_tasks.sh'.")

        else:
            raise ValueError(f"Target family '{target_family_path}' not found in the definition file.")

    except RuntimeError as e:
        print(f"Failed to parse or process the definition file: {e}")
