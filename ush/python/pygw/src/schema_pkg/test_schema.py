#!/usr/bin/env python3

# ----

import os
import os

# TODO: Do we want to add the `global-workflow`/`pygw` specific class
# (e.g., `BuildSchema`, `CheckSchema`, etc.,) to `pygw.schema.py`?
import pygw.schema

# TODO: This is a package containing all schema attribute tools; see
# the above comment for clarification/questions.
from build_schema import BuildSchema
from check_schema import CheckSchema
from write_schema import WriteFromTemplate

# ----

schema_yaml = os.path.join(
    "", "test_files", "model_configure.schema.yaml")
tmpl_path = os.path.join("", "test_files", "model_configure.template")
out_path = os.path.join("", "model_configure.out")

# ----

# The following is a schema to be validated against the schema defined
# within `schema_yaml`; the validated schema with then be used to
# populate the template specified within `tmpl_path` and write it to
# `out_path`.
TEST_SCHEMA = {"DT_ATMOS": 1200,
               "FHMAX": 120,
               "IMO": 384,
               "JMO": 192,
               "SYEAR": 2000,
               "SMONTH": 1,
               "SDAY": 1,
               "SHOUR": 0,
               "WRITE_GROUP": 1,
               "WRTTASK_PER_GROUP": 4,

               # TODO: This needs to be transformed via `f90_bool`.
               "WRITE_DOPOST": True,
               }

# ----


def main() -> None:
    """
    Description
    -----------

    This is the driver-level function to invoke the tasks within this
    script.

    """

    # Build the schema.
    cls_schema = BuildSchema(yaml_file=schema_yaml).build()

    # Attempt to validate the schemas.
    cfg = CheckSchema(cls_schema=cls_schema,
                      cls_opts=TEST_SCHEMA).validate()

    # Create the file based on the specified template.
    WriteFromTemplate(cfg=cfg, tmpl=tmpl_path,
                      output=out_path).write()


# ----
if __name__ == '__main__':
    main()
