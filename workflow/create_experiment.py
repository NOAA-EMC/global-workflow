#!/usr/bin/env python3

"""
Basic python script to create an experiment directory on the fly from a given
yaml file for the arguments to the two scripts below in ${HOMEgfs}/workflow
where ${HOMEgfs} is determined from the location of this script.

 ${HOMEgfs}/workflow/setup_expt.py
 ${HOMEgfs}/workflow/setup_xml.py

The yaml file are simply the arguments for these two scripts.
After this scripts runs the experiment is ready for launch.

Environmental variables
-----------------------
    pslot
        Name of the experiment

    RUNTESTS
        Root directory where the test EXPDIR and COMROOT will be placed

Output
------
Functionally an experiment is setup as a result running the two scripts described above
with an error code of 0 upon success.
"""

import os

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

from wxflow import AttrDict, parse_j2yaml, Logger, logit

import setup_expt
import setup_xml

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))

# Setup the logger
logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"), level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


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

    parser.add_argument(
        '-y', '--yaml', help='full path to yaml file describing the experiment configuration', type=Path, required=True)
    parser.add_argument(
        '-o', '--overwrite', help='overwrite previously created experiment', action="store_true", required=False)

    return parser.parse_args()


if __name__ == '__main__':

    user_inputs = input_args()

    # Create a dictionary to pass to parse_j2yaml for parsing the yaml file
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)
    testconf = parse_j2yaml(path=user_inputs.yaml, data=data)

    # Create a list of arguments to setup_expt.py
    setup_expt_args = [testconf.experiment.system, testconf.experiment.mode]
    for kk, vv in testconf.arguments.items():
        setup_expt_args.append(f"--{kk}")
        setup_expt_args.append(str(vv))

    if user_inputs.overwrite:
        setup_expt_args.append("--overwrite")

    logger.info(f"Call: setup_expt.main()")
    logger.debug(f"setup_expt.py {' '.join(setup_expt_args)}")
    setup_expt.main(setup_expt_args)

    # Create a list of arguments to setup_xml.py
    experiment_dir = Path.absolute(Path.joinpath(
        Path(testconf.arguments.expdir), Path(testconf.arguments.pslot)))

    setup_xml_args = [str(experiment_dir)]

    logger.info(f"Call: setup_xml.main()")
    logger.debug(f"setup_xml.py {' '.join(setup_xml_args)}")
    setup_xml.main(setup_xml_args)
