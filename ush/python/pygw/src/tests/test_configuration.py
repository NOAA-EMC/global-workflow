import os
import pytest
from datetime import datetime

from pygw.configuration import Configuration, cast_as_dtype

file0 = """#!/bin/bash
export SOME_ENVVAR1="${USER}"
export SOME_LOCALVAR1="myvar1"
export SOME_LOCALVAR2="myvar2.0"
export SOME_LOCALVAR3="myvar3_file0"
export SOME_PATH1="/path/to/some/directory"
export SOME_PATH2="/path/to/some/file"
export SOME_DATE1="20221225"
export SOME_DATE2="2022122518"
export SOME_DATE3="202212251845"
export SOME_INT1=3
export SOME_INT2=15
export SOME_INT3=-999
export SOME_FLOAT1=0.2
export SOME_FLOAT2=3.5
export SOME_FLOAT3=-9999.
export SOME_BOOL1=YES
export SOME_BOOL2=.true.
export SOME_BOOL3=.T.
export SOME_BOOL4=NO
export SOME_BOOL5=.false.
export SOME_BOOL6=.F.
"""

file1 = """#!/bin/bash
export SOME_LOCALVAR3="myvar3_file1"
export SOME_LOCALVAR4="myvar4"
export SOME_BOOL7=.TRUE.
"""

file0_dict = {
    'SOME_ENVVAR1': os.environ['USER'],
    'SOME_LOCALVAR1': "myvar1",
    'SOME_LOCALVAR2': "myvar2.0",
    'SOME_LOCALVAR3': "myvar3_file0",
    'SOME_PATH1': "/path/to/some/directory",
    'SOME_PATH2': "/path/to/some/file",
    'SOME_DATE1': datetime(2022, 12, 25, 0, 0, 0),
    'SOME_DATE2': datetime(2022, 12, 25, 18, 0, 0),
    'SOME_DATE3': datetime(2022, 12, 25, 18, 45, 0),
    'SOME_INT1': 3,
    'SOME_INT2': 15,
    'SOME_INT3': -999,
    'SOME_FLOAT1': 0.2,
    'SOME_FLOAT2': 3.5,
    'SOME_FLOAT3': -9999.,
    'SOME_BOOL1': True,
    'SOME_BOOL2': True,
    'SOME_BOOL3': True,
    'SOME_BOOL4': False,
    'SOME_BOOL5': False,
    'SOME_BOOL6': False
}

file1_dict = {
    'SOME_LOCALVAR3': "myvar3_file1",
    'SOME_LOCALVAR4': "myvar4",
    'SOME_BOOL7': True
}

str_dtypes = [
    ('HOME', 'HOME'),
]

int_dtypes = [
    ('1', 1),
]

float_dtypes = [
    ('1.0', 1.0),
]

bool_dtypes = [
    ('y', True), ('n', False),
    ('Y', True), ('N', False),
    ('yes', True), ('no', False),
    ('Yes', True), ('No', False),
    ('YES', True), ('NO', False),
    ('t', True), ('f', False),
    ('T', True), ('F', False),
    ('true', True), ('false', False),
    ('True', True), ('False', False),
    ('TRUE', True), ('FALSE', False),
    ('.t.', True), ('.f.', False),
    ('.T.', True), ('.F.', False),
]

datetime_dtypes = [
    ('20221215', datetime(2022, 12, 15, 0, 0, 0)),
    ('2022121518', datetime(2022, 12, 15, 18, 0, 0)),
    ('2022121518Z', datetime(2022, 12, 15, 18, 0, 0)),
    ('20221215T1830', datetime(2022, 12, 15, 18, 30, 0)),
    ('20221215T1830Z', datetime(2022, 12, 15, 18, 30, 0)),
]


def evaluate(dtypes):
    for pair in dtypes:
        print(f"Test: '{pair[0]}' ==> {pair[1]}")
        assert pair[1] == cast_as_dtype(pair[0])


def test_cast_as_dtype_str():
    evaluate(str_dtypes)


def test_cast_as_dtype_int():
    evaluate(int_dtypes)


def test_cast_as_dtype_float():
    evaluate(float_dtypes)


def test_cast_as_dtype_bool():
    evaluate(bool_dtypes)


def test_cast_as_dtype_datetimes():
    evaluate(datetime_dtypes)


@pytest.fixture
def create_configs(tmp_path):

    file_path = tmp_path / 'config.file0'
    with open(file_path, 'w') as fh:
        fh.write(file0)

    file_path = tmp_path / 'config.file1'
    with open(file_path, 'w') as fh:
        fh.write(file1)


def test_configuration_config_dir(tmp_path, create_configs):
    cfg = Configuration(tmp_path)
    assert cfg.config_dir == tmp_path


def test_configuration_config_files(tmp_path, create_configs):
    cfg = Configuration(tmp_path)
    config_files = [str(tmp_path / 'config.file0'), str(tmp_path / 'config.file1')]
    assert config_files == cfg.config_files


def test_find_config(tmp_path, create_configs):
    cfg = Configuration(tmp_path)
    file0 = cfg.find_config('config.file0')
    assert str(tmp_path / 'config.file0') == file0


@pytest.mark.skip(reason="fails in GH runner, passes on localhost")
def test_parse_config1(tmp_path, create_configs):
    cfg = Configuration(tmp_path)
    f0 = cfg.parse_config('config.file0')
    assert file0_dict == f0


@pytest.mark.skip(reason="fails in GH runner, passes on localhost")
def test_parse_config2(tmp_path, create_configs):
    cfg = Configuration(tmp_path)
    ff = cfg.parse_config(['config.file0', 'config.file1'])
    ff_dict = file0_dict.copy()
    ff_dict.update(file1_dict)
    assert ff_dict == ff
