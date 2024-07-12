from wxflow import Executable, Configuration, ProcessError
from shutil import rmtree
import pytest
import os

_here = os.path.dirname(__file__)
HOMEgfs = os.sep.join(_here.split(os.sep)[:-3])
RUNDIR = os.path.join(_here, 'testdata/RUNDIR')
pslot = "C48_ATM"
account = "fv3-cpu"
foobar = "foobar"


def test_setup_expt():

    arguments = [
        "gfs", "forecast-only",
        "--pslot", pslot, "--app", "ATM", "--resdetatmos", "48",
        "--comroot", f"{RUNDIR}", "--expdir", f"{RUNDIR}",
        "--idate", "2021032312", "--edate", "2021032312", "--overwrite"
    ]
    setup_expt_script = Executable(os.path.join(HOMEgfs, "workflow", "setup_expt.py"))
    setup_expt_script.add_default_arg(arguments)
    setup_expt_script()
    assert (setup_expt_script.returncode == 0)


def test_setup_xml():

    setup_xml_script = Executable(os.path.join(HOMEgfs, "workflow/setup_xml.py"))
    setup_xml_script.add_default_arg(f"{RUNDIR}/{pslot}")
    setup_xml_script()
    assert (setup_xml_script.returncode == 0)

    cfg = Configuration(f"{RUNDIR}/{pslot}")
    base = cfg.parse_config('config.base')
    assert base.ACCOUNT == account

    assert "UNKNOWN" not in base.values()

    with open(f"{RUNDIR}/{pslot}/{pslot}.xml", 'r') as file:
        contents = file.read()
    assert contents.count(account) > 5

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
        setup_xml_script = Executable(os.path.join(HOMEgfs, "ci", "scripts", "tests", "run_setup_xml.sh"))
        setup_xml_script.add_default_arg(f"{RUNDIR}/{pslot}")
        setup_xml_script()
        assert (setup_xml_script.returncode == 0)

        cfg = Configuration(f"{RUNDIR}/{pslot}")
        base = cfg.parse_config('config.base')
        assert base.ACCOUNT == account

        assert foobar not in base.values()
        assert "UNKNOWN" not in base.values()

        with open(f"{RUNDIR}/{pslot}/{pslot}.xml", 'r') as file:
            contents = file.read()
        assert contents.count(account) > 5

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
