import sys
import os

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.join(os.path.dirname(script_dir), 'utils'))

from rocotostat import rocoto_statcount, rocotostat_summary, CommandNotFoundError
from wxflow import which

workflow_file = os.path.join(script_dir, "testdata/rocotostat/workflow.xml")
database_file = os.path.join(script_dir, "testdata/rocotostat/database.db")

try:
    rocotostat = which("rocotostat")
except CommandNotFoundError:
    raise CommandNotFoundError("rocotostat not found in PATH")
rocotostat.add_default_arg(['-w', workflow_file, '-d', database_file])


def test_rocoto_statcount():

    result = rocoto_statcount(rocotostat)

    assert result['SUCCEEDED'] == 20
    assert result['FAIL'] == 0
    assert result['DEAD'] == 0
    assert result['RUNNING'] == 0
    assert result['SUBMITTING'] == 0
    assert result['QUEUED'] == 0


def test_rocoto_summary():

    result = rocotostat_summary(rocotostat)

    assert result['CYCLES_TOTAL'] == 1
    assert result['CYCLES_DONE'] == 1
