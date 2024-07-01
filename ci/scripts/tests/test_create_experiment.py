from wxflow import Executable
from shutil import rmtree
import os

_here = os.path.dirname(__file__)
HOMEgfs = os.sep.join(_here.split(os.sep)[:-3])
RUNDIR = os.path.join(_here, 'testdata/RUNDIR')
case = "C48_ATM"


def test_create_experiment():

    env = os.environ.copy()
    env['pslot'] = case
    env['RUNTESTS'] = RUNDIR

    create_experiment_script = Executable('../../../workflow/create_experiment.py')
    create_experiment_script.add_default_arg(['-y', f'../../cases/pr/{case}.yaml', '--overwrite'])
    create_experiment_script(env=env)
    assert (create_experiment_script.returncode == 0)

    rmtree(RUNDIR)
