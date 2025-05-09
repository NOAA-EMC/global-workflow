import os
from shutil import rmtree
from pathlib import Path
from wxflow import Executable, find_upward

HOMEgfs = find_upward('.github')
current_dir = os.path.dirname(os.path.abspath(__file__))
RUNDIR_FAKE = os.path.join(current_dir, 'testdata/RUNDIR')
ICSDIR_FAKE = os.path.join(current_dir, 'testdata/ICSDIR')


def test_create_experiment():

    create_experiment = Executable(f'{HOMEgfs}/dev/workflow/create_experiment.py')
    create_experiment.add_default_arg(['--overwrite'])
    yaml_dir = yaml_dir = os.path.join(HOMEgfs, 'dev/ci/cases/pr')
    env = os.environ.copy()
    env['RUNTESTS'] = RUNDIR_FAKE
    env['ICSDIR_ROOT'] = ICSDIR_FAKE

    err = 0
    for case in os.listdir(yaml_dir):
        if case.endswith('.yaml'):
            env['pslot'] = os.path.splitext(case)[0]
            cmd_args = ['-y', f'{yaml_dir}/{case}']
            create_experiment(*cmd_args, env=env)
            if create_experiment.returncode:
                print(f"FATAL ERROR: Failed to create experiment for {case}")
                err = 1
    assert err == 0, f"create_experiment.py failed!"

    rmtree(RUNDIR_FAKE)
