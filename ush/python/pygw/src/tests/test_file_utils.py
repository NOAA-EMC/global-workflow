from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler


def test_make_dirs(tmp_path):
    """
    Test for creating directories:
    Parameters
    ----------
    tmp_path - pytest fixture
    """

    dir_path = tmp_path / 'my_test_dir'
    f1 =  f'{dir_path}/my_file1'
    f2 =  f'{dir_path}/my_file2'
    f3 =  f'{dir_path}/my_file3'

    # Create config object for FileHandler
    config = Attrdict()
    config.mkdir = [f1, f2, f3]

    FileHandler(config).sync()

    for dd in config.mkdir:
        assert os.path.exists(dd)

