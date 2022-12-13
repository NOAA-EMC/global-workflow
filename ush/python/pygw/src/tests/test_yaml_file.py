import os
from pygw.yaml_file import YAMLFile

host_yaml = """
host:
    hostname: test_host
    host_user: !ENV ${USER}
"""

conf_yaml = """
config:
    config_file: !ENV ${TMP_PATH}/config.yaml
    user: !ENV ${USER}
    host_file: !INC ${TMP_PATH}/host.yaml
"""


def test_yaml_file(tmp_path):

    # Create temporary yaml files w/ tags
    config_file_path = tmp_path / 'config.yaml'
    with open(config_file_path, 'w') as conf_file:
        conf_file.write(conf_yaml)

    with open(tmp_path / 'host.yaml', 'w') as host_file:
        host_file.write(host_yaml)

    # Set env. variable
    os.environ['TMP_PATH'] = str(tmp_path)
    conf = YAMLFile(path=config_file_path)

    # Write out yaml file
    yaml_out = tmp_path / 'output.yaml'
    conf.save(yaml_out)

    # Read in the yaml file and compare w/ conf
    yaml_in = YAMLFile(path=str(yaml_out))

    assert yaml_in == conf
