import pytest

from pygw.exceptions import WorkflowException

# ----


class TestError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    pygw/errors unit-tests module; it is a sub-class of Error.

    """

# ----


def test_errors() -> None:
    """
    Description
    -----------

    This function provides a unit test for the errors module.

    """

    # Raise the base-class exception.
    with pytest.raises(Exception):
        msg = "Testing exception raise."
        raise TestError(msg=msg)

    assert True
