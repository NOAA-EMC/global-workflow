import os
from pathlib import Path
import pytest
from pygw.executable import Executable, which, CommandNotFoundError


script = """#!/bin/bash
echo ${USER}
"""


def test_executable(tmp_path):
    """
    Tests the class `Executable`
    Parameters:
    -----------
    tmp_path : Path
        temporary path created by pytest
    """
    whoami = os.environ['USER']

    test_file = tmp_path / 'whoami.x'
    Path(test_file).touch(mode=0o755)
    with open(test_file, 'w') as fh:
        fh.write(script)

    cmd = Executable(str(test_file))
    assert cmd.exe == [str(test_file)]

    stdout_file = tmp_path / 'stdout'
    stderr_file = tmp_path / 'stderr'
    cmd(output=str(stdout_file), error=str(stderr_file))
    with open(str(stdout_file)) as fh:
        assert fh.read() == whoami + '\n'


def test_which(tmpdir):
    """
    Tests the `which()` function.
    `which` should return `None` if the executable is not found
    Parameters
    ----------
    tmpdir : Path
        path to a temporary directory created by pytest
    """
    os.environ["PATH"] = str(tmpdir)
    assert which('test.x') is None

    with pytest.raises(CommandNotFoundError):
        which('test.x', required=True)

    path = str(tmpdir.join("test.x"))

    # create a test.x executable in the tmpdir
    with tmpdir.as_cwd():
        Path('test.x').touch(mode=0o755)

        exe = which("test.x")
        assert exe is not None
        assert exe.path == path
