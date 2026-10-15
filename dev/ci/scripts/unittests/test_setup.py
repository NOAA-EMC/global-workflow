import os
import pytest
import tempfile
from shutil import rmtree

from wxflow import Executable, Configuration, ProcessError, find_upward

HOMEglobal = find_upward('.github')
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
        setup_expt_script = Executable(os.path.join(HOMEglobal, "dev/workflow/setup_expt.py"))
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


def test_setup_workflow():

    setup_workflow_script = Executable(os.path.join(HOMEglobal, "dev/workflow/setup_workflow.py"))
    cmd_args = [f"{RUNDIR}/{pslot}", "rocoto"]
    setup_workflow_script(*cmd_args)
    assert (setup_workflow_script.returncode == 0)

    # Get the account value from the config file
    cfg = Configuration(f"{RUNDIR}/{pslot}")
    base = cfg.parse_config('config.base')
    account_value = base.ACCOUNT

    with open(f"{RUNDIR}/{pslot}/{pslot}.xml", 'r') as file:
        contents = file.read()
    assert contents.count(account_value) > 5

    rmtree(RUNDIR)  # TODO: should this be cleaned here or at end of all tests?


def test_setup_workflow_fail_config_env_cornercase(tmp_path):

    setup_workflow_script = Executable(os.path.join(HOMEglobal, "dev/workflow/setup_workflow.py"))
    cmd_args = [f"{RUNDIR}/{pslot}", "rocoto"]
    env = os.environ.copy()
    env['HOMEglobal'] = 'foobar'  # Intentionally incorrect to trigger failure

    try:
        setup_workflow_script(*cmd_args, env=env)
        assert (setup_workflow_script.returncode == 0)

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
        try:
            rmtree(RUNDIR)
        except FileNotFoundError:
            pass


# --- fix_files: end-to-end through setup_expt.py -----------------------------
#
# These use their own pslot and clean up after themselves so they do not
# interact with the order-dependent tests above.  Only `add:` is exercised on
# the build path: on the GitHub runner link_workflow.sh leaves ${HOMEglobal}/fix
# populated with dangling links, so nothing there "exists" for `replace:`.
# The base-tree rules themselves are covered by test_fix_overlay.py.

FIX_PSLOT = "C48_ATM_fix"


def _run_setup_expt(yaml_path, fail_on_error=True):
    setup_expt_script = Executable(os.path.join(HOMEglobal, "dev/workflow/setup_expt.py"))
    setup_expt_script.add_default_arg([
        "gfs", "forecast-only",
        "--pslot", FIX_PSLOT, "--app", "ATM", "--resdetatmos", "48",
        "--comroot", RUNDIR, "--expdir", RUNDIR,
        "--idate", "2021032312", "--edate", "2021032312", "--overwrite",
        "--yaml", str(yaml_path),
    ])
    setup_expt_script(fail_on_error=fail_on_error, output=os.devnull, error=os.devnull)
    return setup_expt_script.returncode


def _exp_yaml(tmp_path, fix_files_block=""):
    yaml_path = tmp_path / 'exp.yaml'
    header = "defaults:\n  !INC {{ HOMEglobal }}/dev/parm/config/gfs/yaml/defaults.yaml\n"
    yaml_path.write_text(header + fix_files_block)
    return yaml_path


@pytest.fixture
def fix_expdir():
    yield f"{RUNDIR}/{FIX_PSLOT}"
    rmtree(f"{RUNDIR}/{FIX_PSLOT}", ignore_errors=True)
    rmtree(f"{RUNDIR}/COMROOT/{FIX_PSLOT}", ignore_errors=True)


def test_setup_expt_fix_files(tmp_path, fix_expdir):
    src = tmp_path / 'my_fix' / 'MOM'
    src.mkdir(parents=True)
    (src / 'regional.mom6.nc').write_text('regional')
    (src / 'MOM_input').write_text('mom_input')

    yaml_path = _exp_yaml(tmp_path, f"""
fix_files:
  add:
    mom6/008/regional.mom6.nc: {src}/regional.mom6.nc
    mom6/008:
      - {src}/MOM_input
""")
    assert _run_setup_expt(yaml_path) == 0

    # FIXglobal is rendered into config.base and points at the overlay
    base = Configuration(fix_expdir).parse_config('config.base')
    assert base.FIXglobal == f"{fix_expdir}/fix"

    # the overlay holds the requested links, a manifest, and single links for untouched components
    assert os.path.isfile(f"{fix_expdir}/fix/.fix_overlay.yaml")
    assert os.readlink(f"{fix_expdir}/fix/mom6/008/regional.mom6.nc") == f"{src}/regional.mom6.nc"
    assert os.readlink(f"{fix_expdir}/fix/mom6/008/MOM_input") == f"{src}/MOM_input"
    assert os.readlink(f"{fix_expdir}/fix/product") == f"{HOMEglobal}/fix/product"  # tracked in git, always present
