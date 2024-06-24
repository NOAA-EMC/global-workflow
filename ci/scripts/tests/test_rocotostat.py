import sys
import os
from shutil import rmtree

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.join(os.path.dirname(script_dir), 'utils'))

from rocotostat import rocoto_statcount, rocotostat_summary, is_done, is_stalled, CommandNotFoundError
from wxflow import which

test_data_url = 'https://noaa-nws-global-pds.s3.amazonaws.com/data/CI/'

testdata_path = 'testdata/rocotostat'
testdata_full_path = os.path.join(script_dir, testdata_path)

wget = which('wget')
if not os.path.isfile(os.path.join(testdata_full_path, 'database.db')):
    wget.add_default_arg([
        '-P', testdata_full_path, test_data_url + str(testdata_path) + '/workflow.xml',
        test_data_url + str(testdata_path) + '/database.db'])
    wget()

try:
    rocotostat = which('rocotostat')
except CommandNotFoundError:
    raise CommandNotFoundError("rocotostat not found in PATH")

rocotostat.add_default_arg(['-w', os.path.join(testdata_path, 'workflow.xml'), '-d', os.path.join(testdata_path, 'database.db')])


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


def test_rocoto_done():

    result = rocotostat_summary(rocotostat)

    assert is_done(result)

    rmtree(testdata_full_path)


def test_rocoto_stalled():
    testdata_path = 'testdata/rocotostat_stalled'
    testdata_full_path = os.path.join(script_dir, testdata_path)
    xml = os.path.join(testdata_full_path, 'stalled.xml')
    db = os.path.join(testdata_full_path, 'stalled.db')

    wget = which('wget')
    if not os.path.isfile(os.path.join(testdata_full_path, 'stalled.db')):
        wget.add_default_arg([
            '-P', testdata_full_path, test_data_url + str(testdata_path) + '/stalled.xml',
            test_data_url + str(testdata_path) + '/stalled.db'])
        wget()

    rocotostat = which('rocotostat')
    rocotostat.add_default_arg(['-w', xml, '-d', db])

    result = rocoto_statcount(rocotostat)

    assert result['SUCCEEDED'] == 10
    assert is_stalled(result)

    rmtree(testdata_full_path)
