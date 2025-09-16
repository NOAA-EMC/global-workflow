import os
import pytest
import tempfile
from shutil import rmtree

from wxflow import Executable, Configuration, ProcessError, find_upward

HOMEgfs = find_upward('.github')
current_dir = os.path.dirname(os.path.abspath(__file__))
RUNDIR = os.path.join(current_dir, 'testdata/RUNTESTS')
pslot = "C48_ATM"
# Note: account is now set within each test function as needed


def test_setup_expt():
    # Set the HPC_ACCOUNT environment variable for this test
    test_account = "test_account_123"
    original_hpc_account = os.environ.get('HPC_ACCOUNT')
    os.environ['HPC_ACCOUNT'] = test_account

    # Create a temporary .gwrc file with the Jinja2 template
    gwrc_content = """user:
  ACCOUNT: {{ 'HPC_ACCOUNT' | getenv }}
"""

    # Create a temporary file for the .gwrc
    with tempfile.NamedTemporaryFile(mode='w', suffix='.gwrc', delete=False) as temp_gwrc:
        temp_gwrc.write(gwrc_content)
        temp_gwrc_path = temp_gwrc.name

    try:
        arguments = [
            "gfs", "forecast-only",
            "--pslot", pslot, "--app", "ATM", "--resdetatmos", "48",
            "--comroot", RUNDIR, "--expdir", RUNDIR,
            "--idate", "2021032312", "--edate", "2021032312", "--overwrite",
            "--gwrc", temp_gwrc_path
        ]
        setup_expt_script = Executable(os.path.join(HOMEgfs, "dev/workflow/setup_expt.py"))
        setup_expt_script.add_default_arg(arguments)
        setup_expt_script()
        assert (setup_expt_script.returncode == 0)

        cfg = Configuration(f"{RUNDIR}/{pslot}")
        base = cfg.parse_config('config.base')
        # Assert that the account matches our test value
        assert base.ACCOUNT == test_account
        assert "UNKNOWN" not in base.values()

    finally:
        # Clean up the temporary .gwrc file
        os.unlink(temp_gwrc_path)
        # Restore the original HPC_ACCOUNT environment variable
        if original_hpc_account is not None:
            os.environ['HPC_ACCOUNT'] = original_hpc_account
        else:
            # Remove the environment variable if it wasn't set originally
            os.environ.pop('HPC_ACCOUNT', None)


def test_setup_xml():

    setup_xml_script = Executable(os.path.join(HOMEgfs, "dev/workflow/setup_xml.py"))
    setup_xml_script.add_default_arg(f"{RUNDIR}/{pslot}")
    setup_xml_script()
    assert (setup_xml_script.returncode == 0)

    # Get the account value from the config file
    cfg = Configuration(f"{RUNDIR}/{pslot}")
    base = cfg.parse_config('config.base')
    account_value = base.ACCOUNT

    with open(f"{RUNDIR}/{pslot}/{pslot}.xml", 'r') as file:
        contents = file.read()
    assert contents.count(account_value) > 5

    rmtree(RUNDIR)


def test_setup_xml_fail_config_env_cornercase():

    script_content = ('''#!/usr/bin/env bash
export HOMEgfs=foobar
../../../workflow/setup_xml.py "${1}"\n
''')

    with open('run_setup_xml.sh', 'w') as file:
        file.write(script_content)
    os.chmod('run_setup_xml.sh', 0o755)

    try:
        setup_xml_script = Executable(os.path.join(HOMEgfs, "dev/ci/scripts/tests/run_setup_xml.sh"))
        setup_xml_script.add_default_arg(f"{RUNDIR}/{pslot}")
        setup_xml_script()
        assert (setup_xml_script.returncode == 0)

        cfg = Configuration(f"{RUNDIR}/{pslot}")
        base = cfg.parse_config('config.base')
        # Get the account value from the config
        account_value = base.ACCOUNT

        assert "UNKNOWN" not in base.values()

        with open(f"{RUNDIR}/{pslot}/{pslot}.xml", 'r') as file:
            contents = file.read()
        assert contents.count(account_value) > 5

    except ProcessError as e:
        # We expect this fail becuse ACCOUNT=fv3-cpu in config.base and environment
        pass
    except Exception as e:
        # If an exception occurs, pass the test with a custom message
        pytest.fail(f"Expected exception occurred: {e}")

    finally:
        # Cleanup code to ensure it runs regardless of test outcome
        os.remove('run_setup_xml.sh')
        try:
            rmtree(RUNDIR)
        except FileNotFoundError:
            pass
