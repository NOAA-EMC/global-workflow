from wxflow import Executable
from shutil import rmtree
import os
import copy

_here = os.path.dirname(__file__)
HOMEgfs = os.sep.join(_here.split(os.sep)[:-3])
RUNDIR = os.path.join(_here, 'testdata/RUNDIR')


def test_create_experiment():

    create_experiment_script = Executable(f'{HOMEgfs}/workflow/create_experiment.py')
    yaml_dir = yaml_dir = os.path.join(HOMEgfs, 'ci/cases/pr')
    env = os.environ.copy()
    env['RUNTESTS'] = RUNDIR

    for case in os.listdir(yaml_dir):
        if case.endswith('.yaml'):
            with open(os.path.join(yaml_dir, case), 'r') as file:
                file_contents = file.read()
                if 'ICSDIR_ROOT' not in file_contents:
                    create_experiment = copy.deepcopy(create_experiment_script)
                    create_experiment.add_default_arg(['-y', f'../../cases/pr/{case}', '--overwrite'])
                    env['pslot'] = os.path.splitext(case)[0]
                    create_experiment(env=env)
                    assert (create_experiment.returncode == 0)

    rmtree(RUNDIR)
