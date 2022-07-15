
import os
import pathlib
import tempfile
from config_manager.config_manager import ConfigManager


def test_configManager_loadsFromPath():
    c = ConfigManager(pathlib.Path("../parm/config"))
    expected = 0
    actual = len(c)
    assert expected < actual

def test_configFile_showsCorrectPath():
    c = ConfigManager(pathlib.Path("../parm/config"))
    expected = "config.eupd"
    actual = str(c.eupd.path)
    assert expected in actual

def test_configFile_showsCorrectName():
    c = ConfigManager(pathlib.Path("../parm/config"))
    expected = "eupd"
    actual = str(c.eupd.name)
    assert expected == actual

def test_configFile_showsCorrectContent():
    c = ConfigManager(pathlib.Path("../parm/config"))
    expected = "########## config.eupd ##########\n"
    actual = c.eupd.content
    assert expected in actual

def test_configFile_writesCorrectContent():
    c = ConfigManager(pathlib.Path("../parm/config"))
    expected = "########## config.eupd ##########\n"
    with tempfile.TemporaryDirectory() as tmpdirname:
        print('created temporary directory', tmpdirname)
        output_file = pathlib.Path(os.path.join(tmpdirname, "eupd.sh"))
        c.eupd.write_to(output_file)
        with open(output_file) as _file:
            assert expected in _file.readlines()




# print(c)
#     print(c.eupd.path)
#     print(c.eupd.name)
#     print(c.eupd.content)
#     c.eupd.write_to(pathlib.Path("./test_test_test.sh"))