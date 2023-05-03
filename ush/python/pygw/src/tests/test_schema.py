import os
import pytest

from pygw import schema

# ----

# Define the path to the YAML-formatted file containing the schema
# attributes.
yaml_path = os.path.join(os.getcwd(), "test-files", "test_schema.yaml")

# ----


def test_build_schema():
    """
    Description
    -----------

    This function tests the `pygw.schema.build_schema` function.

    """

    # Test that the schema can be defined.
    assert schema.build_schema(yaml_path=yaml_path)
