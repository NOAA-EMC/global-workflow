import pytest
from unittest.mock import patch, MagicMock
from rocotostat import rocoto_statcount, CommandNotFoundError

class MockCommand:
    def __init__(self):
        self.add_default_arg = MagicMock()

@patch('os.path.abspath', return_value='path/to/workflow.xml')
@patch('rocotostat.which', return_value=MockCommand())
@patch('rocotostat.input_args')
def test_rocoto_statcount(mock_input_args, mock_which):
    # Arrange
    mock_input_args.return_value = MagicMock(w=MagicMock(name='workflow.xml'), d=MagicMock(name='database.db'))

    # Act
    rocoto_statcount()

    # Assert
    mock_which.assert_called_with('rocotostat')
    mock_input_args.assert_called_once()

@patch('rocotostat.which', side_effect=CommandNotFoundError('rocotostat not found in PATH'))
@patch('rocotostat.input_args')
def test_rocoto_statcount_raises_error_when_rocotostat_not_found(mock_input_args, mock_which, mock_abspath):
    # Arrange
    mock_input_args.return_value = MagicMock(w=MagicMock(name='workflow.xml'), d=MagicMock(name='database.db'))

    # Act and Assert
    with pytest.raises(CommandNotFoundError):
        rocoto_statcount()