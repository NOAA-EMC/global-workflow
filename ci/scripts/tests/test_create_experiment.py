from wxflow import Executable
from shutil import rmtree
import os
import copy

_here = os.path.dirname(__file__)
HOMEgfs = os.sep.join(_here.split(os.sep)[:-3])
RUNDIR = os.path.join(_here, 'testdata/RUNDIR')
case = "C48_ATM"


def test_create_experiment():

    create_experiment_script = Executable(f'{HOMEgfs}/workflow/create_experiment.py')

    env = os.environ.copy()
    env['pslot'] = case
    env['RUNTESTS'] = RUNDIR

    yaml_dir = os.path.join(HOMEgfs, 'ci/cases/pr')
    for case in os.listdir(yaml_dir):
        if case.endswith('.yaml'):
            create_experiment = copy.deepcopy(create_experiment_script)
            create_experiment.add_default_arg(['-y', case, '--overwrite'])
            create_experiment(env=env)
            assert (create_experiment.returncode == 0)

    rmtree(RUNDIR)
