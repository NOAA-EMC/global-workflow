#!/usr/bin/env python3

"""
Basic python script to create an experiment directory on the fly from a given
yaml file for the arguments to the two scripts below in ${HOMEgfs}/workflow
where ${HOMEgfs} is specified within the input yaml file.

 ${HOMEgfs}/workflow/setup_expt.py
 ${HOMEgfs}/workflow/setup_xml.py

The yaml file are simply the arguments for these two scripts.
After this scripts runs the experiment is ready for launch.

Output
------
Functionally an experiment is setup as a result running the two scripts described above
with an error code of 0 upon success.
"""

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

from wxflow import YAMLFile, Logger, logit


_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../..'))
logger = Logger(level='DEBUG', colored_log=True)


# TODO: move create_experiment.py to workflow/ and remove this sys.path.insert business
sys.path.insert(0, os.path.join(_top, 'workflow'))
import setup_expt
import setup_xml


@logit(logger)
def input_args():
    """
    Description
    -----------

    Method to collect user arguments for `create_experiment.py`

    Parameters
    ----------

    None

    Returns
    -------

    argparse.Namespace:
        argparse.Namespace with the value of the file path to a yaml file from the key yaml
    """

    description = """Create a global-workflow experiment"""

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='full path to yaml file describing the experiment configuration', type=str, required=True)

    return parser.parse_args()


if __name__ == '__main__':

    user_inputs = input_args()
    testconf = YAMLFile(path=user_inputs.yaml)
    experiment_dir = Path.absolute(Path.joinpath(Path(testconf.arguments.expdir), Path(testconf.arguments.pslot)))

    # Create a list of arguments to setup_expt.py
    setup_expt_args = [testconf.experiment.type, testconf.experiment.mode]  # TODO: rename 'type' as 'system' in case.yaml
    for kk, vv in testconf.arguments.items():
        setup_expt_args.append(f"--{kk}")
        setup_expt_args.append(str(vv))

    logger.info(f'Call: setup_expt.main()')
    setup_expt.main(setup_expt_args)

    # Create a list of arguments to setup_xml.py
    setup_xml_args = [str(experiment_dir)]

    logger.info(f"Call: setup_xml.main()")
    setup_xml.main(setup_xml_args)
