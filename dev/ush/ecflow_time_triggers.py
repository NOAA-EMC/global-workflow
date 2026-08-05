#!/usr/bin/env python3

import ecflow
import sys
import os
import datetime
import re
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


def build_trigger_command(task, time_attributes, PDYcyc):
    """
    Determines if a task has missed its launch window based on the current date/time and the provided runtime date/time (PDYcyc).
    Constructs an ecflow_client command to trigger a task immediately if the time window has passed.
    Otherwise returns None, indicating no action is needed.
    """

    task_path = task.get_abs_node_path()
    command = f"ecflow_client --run {task_path}"

    time_info = time_attributes.get("trigger")

    # Use ecflow's built-in time variables to determine if the task has missed its
    # launch window.

    # Get current time in UTC
    current_dt = datetime.datetime.utcnow()
    current_time = current_dt.strftime("%Y%m%d%H")
    PDYcyc_dt = datetime.datetime.strptime(PDYcyc, "%Y%m%d%H")
    PDYcyc_dt_plus_12h = PDYcyc_dt + datetime.timedelta(hours=12)

    # If the PDYcyc is more than 12 hous in the past, we can assume the task has missed its launch window.
    if current_dt > PDYcyc_dt_plus_12h:
        return command

    # Extract the time from the trigger expression
    if time_info:
        # Count the number of :TIME operators (==, <, >, <=, >=)
        time_matches = re.findall(r":TIME\s*(==|<|>|<=|>=)\s*(\d{4})", time_info)
        if time_matches:
            count_times = len(time_matches)
            # If count is greather than 1, determine how they are combined
            if count_times > 1:
                # Connectors can be 'AND', 'OR', 'and', 'or', '&&', or '||'
                connectors = re.findall(r"\s*(AND|OR|and|or|&&|\|\|)\s*", time_info)
                len_connectors = len(connectors)
                # Connectors should be one less than the number of time matches
                # If not, something is wrong with the expression.
                if len_connectors != count_times - 1:
                    raise ValueError(f"Invalid trigger expression: {time_info}. Mismatched connectors and time matches.")

                if len_connectors > 0:
                    # Map the connectors to logical operators for evaluation
                    logical_ops = []
                    for connector in connectors:
                        if connector.upper() in ["AND", "&&"]:
                            logical_ops.append(operator.and_)
                        elif connector.upper() in ["OR", "||"]:
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
                return command
            else:
                # It is possible for the trigger to be on the next day
                # If last_time is less than "0600" and $cyc is "18", then the trigger is
                # for the next day.
                if last_time < "0600" and PDYcyc_dt.hour == 18:
                    last_time_dt_next_day = last_time_dt + datetime.timedelta(days=1)
                    if last_time_dt_next_day < current_dt:
                        return command

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
                        raise ValueError(f"Invalid trigger expression: {time_info}. Multiple '==' operators with connectors.")
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

    return None


def build_syndata_copy_commands(skip_commands, PDYcyc):
    """
    Builds a list of commands to copy syndata for skipped tasks instead of triggering them.
    """
    # Split PDYcyc into PDY (YYYYMMDD) and cyc (HH)
    PDY = PDYcyc[:8]
    cyc = PDYcyc[8:]

    copy_commands = []
    for cmd in skip_commands:
        # Extract the task path from the command
        match = re.search(r"ecflow_client --run (.+)", cmd)
        if match:
            task_path = match.group(1)
            # Get the RUN from the match (gdas or gfs)
            if "gdas" in task_path:
                run = "gdas"
            elif "gfs" in task_path:
                run = "gfs"
            else
                raise ValueError(f"Unknown RUN type in task path: {task_path}")

            # Assuming the syndata is located in a specific directory structure
            source_path = f"{ROOT_DUMP_DIR}/{run}.{PDY}/{cyc}/atmos/{run}.t{cyc}z.syndata.tcvitals.tm00"
            dest_path = f"/path/to/destination/{task_path.split('/')[-1]}"
            copy_command = f"cp {source_path} {dest_path}"
            copy_commands.append(copy_command)
    return copy_commands


# This version of __main__ assumes a definition file.
# TODO: Add support for connecting to a running ecFlow server.
if __name__ == "__main__":
    # Arguments:
    #   # 1. Path to the ecFlow definition file
    #   # 2. Absolute path to the target family within the suite
    #   # 3. Runtime date/time in the format ${PDY}${cyc} (e.g., 2023091500)

    if len(sys.argv) != 4:
        print("Usage: python ecflow_time_triggers.py <definition_file.def> <target_family_path> ${PDY}${cyc}")
        sys.exit(1)

    def_file_path = sys.argv[1]
    target_family_path = sys.argv[2]
    PDYcyc = sys.argv[3]
    comroot =

    # Check that PDYcyc is a valid YYYYMMDDHH format
    if not re.match(r"^\d{10}$", PDYcyc):
        raise ValueError(f"Invalid PDYcyc format: '{PDYcyc}'. Expected format is YYYYMMDDHH.")

    try:
        # Load the file directly into memory
        print(f"Loading definition file: {def_file_path}...")
        defs = ecflow.Defs(def_file_path)

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

                command = build_trigger_command(task[0], task[1], PDYcyc)

                # If command is not None, print it.
                if command:
                    commands.append(command)

            if len(commands) > 0:
                skip_commands = []
                with open("trigger_timed_tasks.sh", "w") as f:
                    f.write("#!/bin/bash\nset -ex\n")
                    for cmd in commands:
                        # Skip the tropcy_qc jobs. These jobs are not meant to be
                        # triggered outside of the normal workflow. Instead, we will
                        # copy the syndata they produce from an archive.
                        if "tropcy_qc" in cmd:
                            skip_commands.append(cmd)
                            continue

                        f.write(f"{cmd}\n")
                print("\nCommands to trigger tasks have been written to 'trigger_timed_tasks.sh'.")

                if len(skip_commands) > 0:
                    print("\nThe following commands were skipped (not written to the script):")
                    for cmd in skip_commands:
                        print(f"  {cmd}")

                    # Build the copy commands
                    copy_commands = build_syndata_copy_commands(skip_commands, PDYcyc)
        else:
            raise ValueError(f"Target family '{target_family_path}' not found in the definition file.")

    except RuntimeError as e:
        print(f"Failed to parse or process the definition file: {e}")
