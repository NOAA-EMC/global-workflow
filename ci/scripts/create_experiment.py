#!/usr/bin/env python3

'''Basic python script to create an experiment directory on the fly from a given

yaml file for the arguments to the two scripts below in ${HOMEgfs}/workflow

where ${HOMEgfs} is specified within the input yaml file.

 ${HOMEgfs}/workflow/setup_expt.py
 ${HOMEgfs}/workflow/setup_xml.py

The yaml file are simply the argments for these two scripts.
After this scripts runs these two the use will have an experiment ready for launching
'''

import os, sys, socket
from pathlib import Path

from pygw.yaml_file import YAMLFile
from pygw.logger import Logger
from pygw.executable import Executable

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from workflow.hosts import Host

logger = Logger(level='DEBUG', colored_log=True)

def input_args():
        """Method to collect user arguments for `create_experiment.py`
        """
    description =
        """Single agument as a yaml file containing the
        key value pairs as arguments to setup_expt.py
        """
    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='yaml configuration file per experiment', type=str, required=True )

    args = parser.parse_args()
    return args

if __name__ == '__main__':

    user_inputs = input_args()

    try:
       host = Host()
       logger.info( f'Running on HOST:{host.machine}')
    except:
        logger.error(f'HOST:{socket.gethostname()} is not currently supported')
        sys.exit(1)

    setup_expt_args = YAMLFile(path=user_inputs.yaml)

    rotdir = str(Path.absolute(Path(setup_expt_args.environment.rotdir)))
    HOMEgfs = setup_expt_args.environment.HOMEgfs

    mode = setup_expt_args.experiment.mode

    setup_expt_cmd = Executable(Path.absolute(Path.joinpath(Path(HOMEgfs),'workflow','setup_expt.py')))
    setup_expt_cmd.add_default_arg(mode)

    for conf,value in setup_expt_args.arguments.items():
         setup_expt_cmd.add_default_arg(f'--{conf}')
         setup_expt_cmd.add_default_arg(str(value))

    logger.info(f'Run command: {setup_expt_cmd.command}')
    setup_expt_cmd(output='stdout_expt', error='stderr_expt') 

    setup_xml_cmd = Executable(Path.absolute(Path.joinpath(Path(HOMEgfs),'workflow','setup_xml.py')))
    expdir = Path.absolute(Path.joinpath(Path(setup_expt_args.arguments.expdir),Path(setup_expt_args.arguments.pslot)))
    setup_xml_cmd.add_default_arg(str(expdir))

    logger.info(f'Run command: {setup_xml_cmd.command}')
    setup_xml_cmd(output='stdout_setupxml', error='stderr_setupxml')
