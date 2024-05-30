from wxflow import Executable
import os

HOMEgfs = os.path.dirname(os.path.join(os.path.dirname(__file__), '../../../..'))


def test_setup_expt():

    setup_expt_py = os.path.join(HOMEgfs,"workflow", "setup_expt.py")

    arguments = [
                "gfs", "forecast-only",
                "--pslot", "C48_ATM", "--app", "ATM", "--resdetatmos", "48",
                "--comroot", "RUNDIR/COMROT", "--expdir", "RUNDIR/EXPDIR",
                "--idate", "2021032312", "--edate", "2021032312", "--overwrite"
            ]
    setup_expt_script = Executable(setup_expt_py)
    setup_expt_script.add_default_arg(arguments)
    setup_expt_script()
    assert(setup_expt_script.returncode == 0)


def test_setup_xml():
    
    setup_xml_py = os.path.join(HOMEgfs,"workflow", "setup_xml.py")

    arguments = [
                "--maxtries", "2", "--cyclethrottle", "3", "--taskthrottle", "25", "--verbosity", "10",
                "RUNDIR/EXPDIR/C48_ATM"
            ]

    env = os.environ.copy()
    #env['ACCOUNT'] = 'foo'
    
    setup_xml_script = Executable(setup_xml_py)
    setup_xml_script.add_default_arg(arguments)
    #setup_xml_script(env=env)
    setup_xml_script()
    assert(setup_xml_script.returncode == 0)
