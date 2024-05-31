from wxflow import Executable
from shutil import rmtree
import os

_here = os.path.dirname(__file__)
HOMEgfs = os.sep.join(_here.split(os.sep)[:-3])
RUNDIR = os.path.join(_here, 'testdata/RUNDIR')


def test_setup_expt():

    arguments = [
        "gfs", "forecast-only",
        "--pslot", "C48_ATM", "--app", "ATM", "--resdetatmos", "48",
        "--comroot", f"{RUNDIR}/COMROT", "--expdir", f"{RUNDIR}/EXPDIR",
        "--idate", "2021032312", "--edate", "2021032312", "--overwrite"
    ]
    setup_expt_script = Executable(os.path.join(HOMEgfs, "workflow", "setup_expt.py"))
    setup_expt_script.add_default_arg(arguments)
    setup_expt_script()
    assert (setup_expt_script.returncode == 0)


def test_setup_xml():

    env = os.environ.copy()
    env['ACCOUNT'] = "foo"
    env['HOMEgfs'] = "bar"

    setup_xml_script = Executable(os.path.join(HOMEgfs, "workflow", "setup_xml.py"))
    setup_xml_script.add_default_arg(f"{RUNDIR}/EXPDIR/C48_ATM")
    setup_xml_script(env=env)
    assert (setup_xml_script.returncode == 0)
    rmtree(RUNDIR)
