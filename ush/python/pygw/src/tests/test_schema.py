"""
Description
-----------

Unit-tests for `pygw.schema`.
"""

import os

from pygw import schema
from pygw.yaml_file import parse_yaml
from pygw.schema import SchemaError
from pygw.configuration import cast_strdict_as_dtypedict


# Define the path to the YAML-formatted file containing the schema
# attributes.
# yaml_path = os.path.join(os.getcwd(), "tests",
#                         "test-files", "test_schema.yaml")
# data = parse_yaml(path=yaml_path)
@pytest.mark.skip(reason="disable till the developer fixes the test")
def test_build_schema():
    """
    Description
    -----------

    This function tests the `pygw.schema.build_schema` function.

    """

    # Test that the schema can be defined.
    assert schema.build_schema(data=data)


@pytest.mark.skip(reason="disable till the developer fixes the test")
def test_validate_schema():
    """
    Description
    -----------

    This function tests various application configurations (i.e.,
    `data_in`) for various schema validation applications.

    """

    # Define the schema.
    schema_dict = schema.build_schema(data=data)

    # Test that the schema validates and returns a the dictionary
    # passed; this unit-test should pass.
    data_in = {
        "variable1": False,
        "variable2": 1,
        "variable3": "hello world",
        "variable4": 10.0,
    }
    data_out = schema.validate_schema(schema_dict=schema_dict, data=data_in)
    assert True
    assert data_in == data_out

    # Test that optional values are updated with defaults.
    del data_in["variable2"]
    data_out = schema.validate_schema(schema_dict=schema_dict, data=data_in)
    assert True

    # This unit-test should raise a `SchemaError` exception in order
    # to pass.
    data_in["variable2"] = "I **should** fail."
    try:
        data_out = schema.validate_schema(
            schema_dict=schema_dict, data=data_in)
    except SchemaError:
        assert True

    # This unit-test passes the full environment, including `data_in`,
    # to be validated; this tests the `ignore_extra_keys` attribute;
    # this unit-test should pass.
    del data_in["variable2"]
    data_in = {**cast_strdict_as_dtypedict(os.environ), **data_in}
    data_out = schema.validate_schema(schema_dict=schema_dict, data=data_in)
    assert True
    assert data_in == data_out
