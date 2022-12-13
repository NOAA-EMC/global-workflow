import os
from pygw.file_utils import FileHandler


def test_mkdir(tmp_path):
    """
    Test for creating directories:
    Parameters
    ----------
    tmp_path - pytest fixture
    """

    dir_path = tmp_path / 'my_test_dir'
    d1 = f'{dir_path}1'
    d2 = f'{dir_path}2'
    d3 = f'{dir_path}3'

    # Create config object for FileHandler
    config = {'mkdir': [d1, d2, d3]}

    # Create d1, d2, d3
    FileHandler(config).sync()

    # Check if d1, d2, d3 were indeed created
    for dd in config['mkdir']:
        assert os.path.exists(dd)


def test_copy(tmp_path):
    """
    Test for copying files:
    Parameters
    ----------
    tmp_path - pytest fixture
    """

    input_dir_path = tmp_path / 'my_input_dir'

    # Create the input directory
    config = {'mkdir': [input_dir_path]}
    FileHandler(config).sync()

    # Put empty files in input_dir_path
    src_files = [input_dir_path / 'a.txt', input_dir_path / 'b.txt']
    for ff in src_files:
        ff.touch()

    # Create output_dir_path and expected file names
    output_dir_path = tmp_path / 'my_output_dir'
    config = {'mkdir': [output_dir_path]}
    FileHandler(config).sync()
    dest_files = [output_dir_path / 'a.txt', output_dir_path / 'bb.txt']

    copy_list = []
    for src, dest in zip(src_files, dest_files):
        copy_list.append([src, dest])

    # Create config object for FileHandler
    config = {'copy': copy_list}

    # Copy input files to output files
    FileHandler(config).sync()

    # Check if files were indeed copied
    for ff in dest_files:
        assert os.path.isfile(ff)
