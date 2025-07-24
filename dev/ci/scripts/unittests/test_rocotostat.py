#!/usr/bin/env python3

import time
import sys
import os
import pytest
from unittest.mock import Mock, patch

# Add the utils directory to the path to import rocotostat
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'utils'))
from rocotostat import attempt_multiple_times, logger


def test_attempt_multiple_times_success_first_try():
    """Test that attempt_multiple_times succeeds on first try"""
    mock_func = Mock(return_value="success")

    result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0.1)

    assert result == "success"
    mock_func.assert_called_once()
def test_attempt_multiple_times_success_after_retries():
    """Test that attempt_multiple_times succeeds after some failures"""
    mock_func = Mock(side_effect=[Exception("fail"), Exception("fail"), "success"])

    result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0.1)

    assert result == "success"
    assert mock_func.call_count == 3
def test_attempt_multiple_times_max_attempts_exceeded():
    """Test that attempt_multiple_times fails after max attempts"""
    mock_func = Mock(side_effect=Exception("always fail"))

    with pytest.raises(Exception, match="always fail"):
        attempt_multiple_times(mock_func, max_attempts=2, sleep_duration=0.1)

    assert mock_func.call_count == 2
def test_attempt_multiple_times_with_args_and_kwargs():
    """Test that attempt_multiple_times passes arguments correctly"""
    mock_func = Mock(return_value="success")

    result = attempt_multiple_times(
        mock_func,
        max_attempts=3,
        sleep_duration=0.1
    )

    assert result == "success"
    mock_func.assert_called_with()


def test_attempt_multiple_times_delay():
    """Test that attempt_multiple_times respects delay between attempts"""
    mock_func = Mock(side_effect=[Exception("fail"), "success"])

    start_time = time.time()
    result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0.2)
    end_time = time.time()

    assert result == "success"
    assert mock_func.call_count == 2
    # Should take at least 0.2 seconds due to the delay
    assert end_time - start_time >= 0.15  # Allow some tolerance


@patch('rocotostat.logger')
def test_attempt_multiple_times_logging(mock_logger):
    """Test that attempt_multiple_times logs retry attempts"""
    mock_func = Mock(side_effect=[Exception("fail"), "success"])

    result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0.1)

    assert result == "success"
    assert mock_func.call_count == 2
    # Should have logged the retry attempt with warning for failure and info for success
    mock_logger.warning.assert_called()
    mock_logger.info.assert_called()


if __name__ == '__main__':
    pytest.main([__file__])
