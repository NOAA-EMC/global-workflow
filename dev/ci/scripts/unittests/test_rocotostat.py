#!/usr/bin/env python3

import pytest
import time
import sys
import os
from unittest.mock import Mock, patch

# Add the utils directory to the path to import rocotostat
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'utils'))
from rocotostat import attempt_multiple_times, logger


class TestAttemptMultipleTimes:
    """Test cases for the attempt_multiple_times function."""

    def test_success_on_first_attempt(self):
        """Test that function returns immediately on successful first attempt."""
        mock_func = Mock(return_value="success")

        start_time = time.time()
        result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=1)
        end_time = time.time()

        assert result == "success"
        assert mock_func.call_count == 1
        # Should complete quickly (no sleep)
        assert end_time - start_time < 0.5

    def test_success_after_retries(self):
        """Test that function succeeds after some failures."""
        mock_func = Mock(side_effect=[Exception("fail1"), Exception("fail2"), "success"])

        result = attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0)

        assert result == "success"
        assert mock_func.call_count == 3

    def test_failure_after_max_attempts(self):
        """Test that function raises last exception after max attempts."""
        mock_func = Mock(side_effect=RuntimeError("persistent error"))

        with pytest.raises(RuntimeError, match="persistent error"):
            attempt_multiple_times(mock_func, max_attempts=3, sleep_duration=0)

        assert mock_func.call_count == 3

    def test_telescoping_delay(self):
        """Test that telescoping delay increases properly between attempts."""
        mock_func = Mock(side_effect=[Exception("fail1"), Exception("fail2"), "success"])

        # Mock sleep to capture the delay values
        with patch('rocotostat.sleep') as mock_sleep:
            result = attempt_multiple_times(
                mock_func,
                max_attempts=3,
                sleep_duration=2,
                use_telescoping_delay=True
            )

        assert result == "success"
        # Should sleep with telescoping delays: 2s, 4s
        expected_calls = [pytest.approx(2), pytest.approx(4)]
        actual_calls = [call[0][0] for call in mock_sleep.call_args_list]
        assert actual_calls == expected_calls

    def test_fixed_delay(self):
        """Test that fixed delay remains constant between attempts."""
        mock_func = Mock(side_effect=[Exception("fail1"), Exception("fail2"), "success"])

        with patch('rocotostat.sleep') as mock_sleep:
            result = attempt_multiple_times(
                mock_func,
                max_attempts=3,
                sleep_duration=1,
                use_telescoping_delay=False
            )

        assert result == "success"
        # Should sleep with fixed delays: 1s, 1s
        expected_calls = [1, 1]
        actual_calls = [call[0][0] for call in mock_sleep.call_args_list]
        assert actual_calls == expected_calls

    def test_no_sleep_duration(self):
        """Test that no sleep occurs when sleep_duration is 0."""
        mock_func = Mock(side_effect=[Exception("fail1"), "success"])

        with patch('rocotostat.sleep') as mock_sleep:
            result = attempt_multiple_times(mock_func, max_attempts=2, sleep_duration=0)

        assert result == "success"
        mock_sleep.assert_not_called()

    def test_specific_exception_class(self):
        """Test that only specific exception types are caught."""
        mock_func = Mock(side_effect=ValueError("specific error"))

        # Should catch ValueError
        with pytest.raises(ValueError):
            attempt_multiple_times(
                mock_func,
                max_attempts=2,
                sleep_duration=0,
                exception_class=ValueError
            )

        # Should NOT catch RuntimeError (different exception type)
        mock_func.side_effect = RuntimeError("different error")
        with pytest.raises(RuntimeError):
            attempt_multiple_times(
                mock_func,
                max_attempts=2,
                sleep_duration=0,
                exception_class=ValueError
            )

    @patch('rocotostat.logger')
    def test_logging_messages(self, mock_logger):
        """Test that appropriate log messages are generated."""
        mock_func = Mock(side_effect=[Exception("fail1"), "success"])

        # Mock time to return predictable values - need enough for all time() calls
        with patch('rocotostat.time', side_effect=[0, 0.5, 0.5, 1.0, 1.0, 1.5]):
            result = attempt_multiple_times(mock_func, max_attempts=2, sleep_duration=0)

        assert result == "success"

        # Check that warning and info messages were logged
        warning_calls = [call for call in mock_logger.warning.call_args_list]
        info_calls = [call for call in mock_logger.info.call_args_list]

        assert len(warning_calls) == 1  # One failure
        # Should have: 2 thread count logs + 1 success message = 3 info calls
        assert len(info_calls) == 3

        # Check log message content - find the rocoto-specific messages
        warning_msg = warning_calls[0][0][0]
        assert "Rocoto call failed on attempt 1" in warning_msg
        assert "call_time=0.00s" in warning_msg  # Should be 0.00s based on our mock

        # Find the success message (not the thread count messages)
        success_msgs = [call[0][0] for call in info_calls if "Rocoto call successful" in call[0][0]]
        assert len(success_msgs) == 1
        success_msg = success_msgs[0]
        assert "Rocoto call successful on attempt 2" in success_msg
        assert "call_time=0.00s" in success_msg
        assert "total_time=1.00s" in success_msg

    @patch('rocotostat.log_thread_count')
    def test_thread_count_logging(self, mock_log_thread_count):
        """Test that thread count is logged for success and failure."""
        mock_func = Mock(side_effect=[Exception("fail1"), "success"])

        result = attempt_multiple_times(mock_func, max_attempts=2, sleep_duration=0)

        assert result == "success"

        # Should log thread count for failure and success
        expected_calls = [
            (("ROCOTO_FAILED_ATTEMPT_1",), {}),
            (("ROCOTO_SUCCESS_ATTEMPT_2",), {})
        ]
        assert mock_log_thread_count.call_args_list == expected_calls

    def test_performance_timing(self):
        """Test that timing measurements are reasonable."""
        def slow_func():
            time.sleep(0.1)  # 100ms delay
            return "success"

        with patch('rocotostat.logger') as mock_logger:
            start_time = time.time()
            result = attempt_multiple_times(slow_func, max_attempts=1, sleep_duration=0)
            end_time = time.time()

        assert result == "success"

        # Should take at least 100ms
        assert end_time - start_time >= 0.1

        # Check that timing was logged correctly
        info_calls = mock_logger.info.call_args_list
        # Find the success message (not the thread count message)
        success_msgs = [call[0][0] for call in info_calls if "Rocoto call successful" in call[0][0]]
        assert len(success_msgs) == 1
        success_msg = success_msgs[0]
        assert "call_time=" in success_msg
        assert "total_time=" in success_msg

    def test_edge_case_single_attempt(self):
        """Test behavior with max_attempts=1."""
        mock_func = Mock(return_value="success")

        result = attempt_multiple_times(mock_func, max_attempts=1, sleep_duration=1)

        assert result == "success"
        assert mock_func.call_count == 1

    def test_edge_case_zero_attempts(self):
        """Test behavior with max_attempts=0 (should not execute)."""
        mock_func = Mock(return_value="success")

        with pytest.raises(Exception):  # Should raise the last_exception which is None initially
            attempt_multiple_times(mock_func, max_attempts=0, sleep_duration=0)

        assert mock_func.call_count == 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
