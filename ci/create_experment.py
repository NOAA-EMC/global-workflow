#!/usr/bin/env python3

import os
import socket

from pygw.yaml_file import YAMLFile
from pygw.logger import logit
from pygw.executable import Executable

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from hosts import Host


# @logit(logger)

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))

def input_args():
    """
    Method to collect user arguments for `create_experment.py`
    """
    description = """
        Single agument as a yaml file
        """
    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='Defaults to substitute from', type=str,
                          required=True, default=os.path.join(_top, 'ci/experment1.yaml'))

    args = parser.parse_args()
    return args

if __name__ == '__main__':

    user_inputs = input_args()

    try:
       host = Host()
       print( "HOST:", host.machine )
    except:
        print(socket.gethostname(),"is not specificly supported")

    #link_IC
    #rotdir="${NOSCRUB}/global-workflow/RUNTEST/${pslot}"
    #rm -Rf "${rotdir}"
    #cp -as "${icdir}/" "${rotdir}"


    setup_expt_args = YAMLFile(path=user_inputs.yaml)

    mode = setup_expt_args.mode
    icdir = setup_expt_args.icdir
    rotdir = setup_expt_args.rotdir
    del setup_expt_args['mode']
    del setup_expt_args['icdir']
    del setup_expt_args['rotdir']

    setup_expt_cmd = Executable(os.path.abspath(os.path.join(_top,'workflow/setup_expt.py')))
    setup_expt_cmd.add_default_arg(mode)
    for conf,value in setup_expt_args.items():
         setup_expt_cmd.add_default_arg(f'--{conf}')
         setup_expt_cmd.add_default_arg(str(value))

    print( setup_expt_cmd.command )
    #setup_expt_cmd(output='stdout', error='stderr') 

    link_ic_cmd = Executable('cp')
    link_ic_cmd.add_defaut_arg('-as')
    link_ic_cmd.add_defaut_arg(os.path.abspath(icdir+'/'))
    link_ic_cmd.add_defaut_arg(os.path.abspath(rotdir))
    print( link_ic_cmd.command )

