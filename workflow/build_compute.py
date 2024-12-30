#!/usr/bin/env python3

"""
Entry point for setting up a compute-node build
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from typing import Dict

from wxflow import parse_yaml, AttrDict

from hosts import Host
import rocoto.rocoto as rocoto


_here = os.path.dirname(__file__)
HOMEgfs = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))


def input_args(*argv):
    """
    Method to collect user arguments for `compute_build.py`
    """

    description = """
        Setup files and directories to start a compute build.
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='Input YAML file',
                        type=str, required=False, default='build_opts.yaml')
    parser.add_argument('--account', help='HPC account to use; default is host-dependent', required=False, default=os.getenv('HPC_ACCOUNT'))
    parser.add_argument('--systems', help='System(s) to build (options: gfs, gefs, sfs, gsi, gdas, or all)', required=False, default='gfs')

    inputs = parser.parse_args(list(*argv) if len(argv) else None)

    return inputs


def get_task_spec(task_name: str, task_spec: Dict, host_spec: Dict) -> Dict:
    """
    Generate a task specification dictionary for a given task.

    Parameters
    ----------
    task_name: str
        The name of the task.
    task_spec: Dict
        The specification of the task, containing command, walltime, and cores.
    host_spec: Dict
        The specification of the host, containing account, queue, partition, and native.

    Returns:
    --------
    task_dict: Dict
        A dictionary containing the task specification, including resources and other task-related information.
    """

    task_dict = AttrDict()
    task_dict.task_name = task_name
    task_dict.cycledef = "build"
    task_dict.maxtries = 1
    task_dict.command = f"cd {HOMEgfs}/sorc/; {task_spec.command}"
    task_dict.job_name = task_name
    task_dict.log = f"{HOMEgfs}/sorc/logs/{task_name}.log"

    task_dict.resources = AttrDict()
    task_dict.resources.account = host_spec.account
    task_dict.resources.queue = host_spec.queue
    task_dict.resources.partition = host_spec.partition
    task_dict.resources.walltime = task_spec.walltime
    task_dict.resources.native = host_spec.native
    task_dict.resources.memory = None
    task_dict.resources.nodes = 1
    task_dict.resources.ntasks = task_spec.cores
    task_dict.resources.ppn = task_spec.cores
    task_dict.resources.threads = 1

    return task_dict


def get_host_specs(host: Dict) -> Dict:
    """Generate host specs for the build.xml file based on Host() info

    Parameters
    ----------
    host : Dict
        Host information returned by Host()

    Returns
    -------
    specs: Dict
        Consolidated compute specifics needed for the XML
    """

    native = None
    partition = None

    if host.info.SCHEDULER in ['pbspro']:
        native = '-l place=vscatter'
    elif host.info.SCHEDULER in ['slurm']:
        native = '--export=NONE'
        if host.info.PARTITION_BATCH not in [""]:
            partition = host.info.PARTITION_BATCH

    if host.info.RESERVATION not in [""]:
        native += f' --reservation={host.info.RESERVATION}'

    if host.info.CLUSTERS not in [""]:
        native += f' --clusters={host.info.CLUSTERS}'

    specs = AttrDict()
    specs.scheduler = host.info.SCHEDULER
    specs.account = host.info.ACCOUNT
    specs.queue = host.info.QUEUE
    specs.partition = partition
    specs.native = native

    return specs


def main(*argv):

    user_inputs = input_args(*argv)
    host_specs = get_host_specs(Host())

    # Update the default host account if the user supplied one
    if user_inputs.account is not None:
        host_specs.account = user_inputs.account

    build_specs = AttrDict(parse_yaml(user_inputs.yaml))

    systems = user_inputs.systems.split() if "all" not in user_inputs.systems else ["all"]

    # Determine systems to build
    builds = set()
    if systems[0] == "all":
        builds = build_specs.build
    else:
        builds.update(build_specs.systems["common"])
        try:
            for system in systems:
                builds.update(build_specs.systems[system])
        except KeyError as e:
            raise KeyError(f"{system} is not a valid global-workflow system!") from e

    # Build the task specs from the build specs and host specs
    task_specs = AttrDict()
    for task_name, task_spec in build_specs.build.items():
        if task_name in builds:
            task_specs[task_name] = get_task_spec(task_name, task_spec, host_specs)

    # Start building the XML
    strings = ['<?xml version="1.0"?>',
               '<!DOCTYPE workflow', '[', ']>',
               f'<workflow realtime="F" scheduler="{host_specs.scheduler}" cyclethrottle="1" taskthrottle="25">',
               f'\t<log verbosity="10">{HOMEgfs}/sorc/logs/build.log</log>',
               '\t<cycledef group="build">190001010000 190001010000 24:00:00</cycledef>',
               '\n']
    xml_header = '\n'.join(strings)
    xml_footer = '\n</workflow>\n'

    task_list = []
    for _, task_spec in task_specs.items():
        task_list.append(rocoto.create_task(task_spec))
    xml_tasks = '\n'.join(task_list)

    xml = ''.join([xml_header, xml_tasks, xml_footer])
    xml_file = f"{HOMEgfs}/sorc/build.xml"
    with open(xml_file, 'w') as fh:
        fh.write(xml)


if __name__ == '__main__':
    main()
