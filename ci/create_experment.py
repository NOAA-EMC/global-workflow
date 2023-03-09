#!/usr/bin/env python3

import os, sys
import socket
from pathlib import Path

from pygw.yaml_file import YAMLFile
from pygw.logger import Logger
from pygw.executable import Executable

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from hosts import Host

logger = Logger(level='DEBUG', colored_log=True)
_here = Path.absolute(Path(__file__)).parents[0]
_top  = Path.absolute(Path(__file__)).parents[1]

def input_args():
    """
    Method to collect user arguments for `create_experment.py`
    """
    description = """
        Single agument as a yaml file
        """
    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='yaml configuration file per experment', type=str, required=True )

    args = parser.parse_args()
    return args

if __name__ == '__main__':

    user_inputs = input_args()

    try:
       host = Host()
       print( "HOST:", host.machine )
    except:
        print(socket.gethostname(),"is not specificly supported")

    setup_expt_args = YAMLFile(path=user_inputs.yaml)

    mode = setup_expt_args.experment.mode
    icdir = setup_expt_args.environment.icdir
    rotdir = setup_expt_args.environment.rotdir

    setup_expt_cmd = Executable(Path.absolute(Path.joinpath(_top,'workflow','setup_expt.py')))
    setup_expt_cmd.add_default_arg(mode)
    for conf,value in setup_expt_args.arguments.items():
         setup_expt_cmd.add_default_arg(f'--{conf}')
         setup_expt_cmd.add_default_arg(str(value))

    logger.info(f'run command: {setup_expt_cmd.command}')
    setup_expt_cmd(output='stdout_expt', error='stderr_expt') 

    # Make symlinks of initial conitions into ROTDIR
    link_ic_cmd = Executable('cp')
    link_ic_cmd.add_default_arg('-as')
    link_ic_cmd.add_default_arg(os.path.abspath(icdir+'/'))
    link_ic_cmd.add_default_arg(os.path.abspath(rotdir))

    logger.info(f'run command: {link_ic_cmd.command}')
    link_ic_cmd(output='stdout_linkic', error='stderr_linkic')

    setup_xml_cmd = Executable(Path.absolute(Path.joinpath(_top,'workflow','setup_xml.py')))
    expdir = Path.absolute(Path.joinpath(Path(setup_expt_args.arguments.expdir),Path(setup_expt_args.arguments.pslot)))
    setup_xml_cmd.add_default_arg(str(expdir))

    logger.info(f'run command: {setup_xml_cmd.command}')
    setup_xml_cmd(output='stdout_setupxml', error='stderr_setupxml')