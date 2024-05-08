
import pytest
from rocotostat import rocoto_statcount, CommandNotFoundError

import os

workflow_file = os.path.join(os.getcwd(), "workflow.xml")
database_file = os.path.join(os.getcwd(), "database.db")

def test_rocoto_statcount(
    result = rocoto_statcount(w=workflow_file, d=database_file)

    assert result['CYCLES_TOTAL'] == 1
    assert result['CYCLES_DONE'] == 1
    assert result['SUCCEDED'] == 20
    assert result['ROCOTO_STATE'] == 'DONE'
