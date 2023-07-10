#!/usr/bin/env python3

"""
Basic python script to create an experiment directory on the fly from a given

yaml file for the arguments to the two scripts below in ${HOMEgfs}/workflow

where ${HOMEgfs} is specified within the input yaml file.

 ${HOMEgfs}/workflow/setup_expt.py
 ${HOMEgfs}/workflow/setup_xml.py

The yaml file are simply the arguments for these two scripts.
After this scripts runs these two the use will have an experiment ready for launching

Output
------

Functionally an experiment is setup as a result running the two scripts described above
with an error code of 0 upon success.
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

from wxflow import YAMLFile, Logger, logit, Executable


logger = Logger(level='DEBUG', colored_log=True)


@logit(logger)
def input_args():
    """
    Method to collect user arguments for `create_experiment.py`

    Input
    -----

    A single key valued argument: --yaml <full path to YAML file>

    Description
    -----------

    A full path to a YAML file with the following format with required sections: experiment, arguments

    experiment:
        mode: <cycled> <forecast-only>
            used to hold the only required positional argument to setup_expt.py

    arguments:
        holds all the remaining key values pairs for all requisite arguments documented for setup_expt.py
        Note: the argument pslot is derived from the basename of the yamlfile itself

    Returns
    -------

    args: Namespace

        Namespace with the value of the file path to a yaml file from the key yaml
    """

    description = """Single argument as a yaml file containing the
    key value pairs as arguments to setup_expt.py
    """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='yaml configuration file per experiment', type=str, required=True)
    parser.add_argument('--dir', help='full path to top level of repo of global-workflow', type=str, required=True)

    args = parser.parse_args()
    return args


if __name__ == '__main__':

    user_inputs = input_args()
    setup_expt_args = YAMLFile(path=user_inputs.yaml)

    HOMEgfs = Path.absolute(Path(user_inputs.dir))
    pslot = os.environ['pslot']
    type = setup_expt_args.experiment.type
    mode = setup_expt_args.experiment.mode

    setup_expt_cmd = Executable(Path.joinpath(HOMEgfs, 'workflow', 'setup_expt.py'))

    setup_expt_cmd.add_default_arg(type)
    setup_expt_cmd.add_default_arg(mode)

    for conf, value in setup_expt_args.arguments.items():
        setup_expt_cmd.add_default_arg(f'--{conf}')
        setup_expt_cmd.add_default_arg(str(value))

    setup_expt_cmd.add_default_arg('--pslot')
    setup_expt_cmd.add_default_arg(pslot)

    logger.info(f'Run command: {setup_expt_cmd.command}')
    setup_expt_stderr = str(Path.joinpath(HOMEgfs, 'ci', 'scripts', 'setup_expt.stderr'))
    setup_expt_stdout = str(Path.joinpath(HOMEgfs, 'ci', 'scripts', 'setup_expt.stdout'))
    print(setup_expt_stderr)
    setup_expt_cmd(output=setup_expt_stdout, error=setup_expt_stderr)

    setup_xml_cmd = Executable(Path.joinpath(HOMEgfs, 'workflow', 'setup_xml.py'))
    expdir = Path.absolute(Path.joinpath(Path(setup_expt_args.arguments.expdir), Path(pslot)))
    setup_xml_cmd.add_default_arg(str(expdir))

    logger.info(f'Run command: {setup_xml_cmd.command}')
    setup_xml_stderr = str(Path.joinpath(HOMEgfs, 'ci', 'scripts', 'setup_xml.stderr'))
    setup_xml_stdout = str(Path.joinpath(HOMEgfs, 'ci', 'scripts', 'setup_xml.stdout'))
    setup_xml_cmd(output=setup_xml_stdout, error=setup_xml_stderr)
