"""
Module
------

    pygfs.utils.timestamps (pygfs/utils/timestamps.py)

Description
-----------

    This module contains the base-class module for all time-stamp
    attributes definitions and derivations relative to a specified
    time-stamp and optional formatting.

Classes
-------

    Timestamps(datestr, fmt="%Y-%m-%d %H:%M:%S")

        This is the base-class object for all time-stamp attribute
        definitions.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

import datetime
import sqlite3
from dataclasses import dataclass

from pygfs.exceptions import TimestampsError
from pygw.attrdict import AttrDict
from pygw.decorators import private
from pygw.timetools import strftime, strptime

# ----


@dataclass
class Timestamps:
    """
    Description
    -----------

    This is the base-class object for all time-stamp attribute
    definitions.

    Parameters
    ----------

    datestr: str

        A Python string specifying the the time-stamp; if the
        parameter `fmt` is not specified upon entry, the module
        assumes that the time-stamp is formatted as "%Y-%m-%d
        %H:%M:%S".

    Keywords
    --------

    fmt: str

        A Python string specifying the POSIX-format for the `datestr`
        parameter upon entry.

    Raises
    ------

    TimestampsError:

        - raised if an exception is encountered while discerning the
          format of the attribute `datestr` upon entry.

        - raised if an exception is encountered while defining the
          derived time-stamp attributes.

    """

    def __init__(self: dataclass, datestr: str, fmt: str = "%Y-%m-%d %H:%M:%S"):
        """
        Description
        -----------

        Creates a new Timestamps object.

        """

        # Define the base-class attributes.
        self.timestamps = AttrDict()

        # Define the time-stamp attributes relative to the application
        # initialization time; enforce input attribute `datestr` to be
        # Python type string.
        try:
            time_str = strptime(dtstr=str(datestr), fmt=fmt)

        except Exception as errmsg:
            msg = (
                f"Initializing the attributes for input timestamp {datestr} "
                f"failed with error {errmsg}. Aborting!!!"
            )
            raise TimestampsError(msg=msg) from errmsg

        time_attr_dict = {
            "year": "%Y",
            "month": "%m",
            "day": "%d",
            "hour": "%H",
            "minute": "%M",
            "second": "%S",
            "month_name_long": "%B",
            "month_name_short": "%b",
            "century_short": "%C",
            "year_short": "%y",
            "weekday_long": "%A",
            "weekday_short": "%a",
            "day_of_year": "%j",
            "day_of_week": "%u",
            "timezone": "%Z",
            "week_of_year": "%W",
        }

        for (time_attr, time_attr_value) in time_attr_dict.items():
            value = strftime(dt=time_str, fmt=time_attr_value)
            try:
                setattr(self.timestamps, time_attr, int(value))
            except ValueError:
                setattr(self.timestamps, time_attr, value)

        # Define the derived time-stamp attributes; proceed
        # accordingly.
        try:
            self.julianday()
            self.epoch()

        except Exception as errmsg:
            msg = (
                "Defining timestamp attributes failed with error "
                f"{errmsg}. Aborting!!!"
            )
            raise TimestampsError(msg=msg) from errmsg

    @private
    def epoch(self: dataclass):
        """
        Description
        -----------

        This method defines the epoch time relative to the respective
        timestamp specified upon entry.

        """

        # Define the epoch time (i.e., number of seconds since 0000
        # UTC 01 January 1970).
        self.timestamps.epoch = datetime.datetime(
            int(self.timestamps.year),
            int(self.timestamps.month),
            int(self.timestamps.day),
            int(self.timestamps.hour),
            int(self.timestamps.minute),
            int(self.timestamps.second),
        ).timestamp()

    @private
    def julianday(self: dataclass):
        """
        Description
        -----------

        This method define the Julian day relative to the respective
        timestamp specified upon entry.

        """

        # Define the Julian day.
        connect = sqlite3.connect(":memory:")
        datestr = (
            f"{self.timestamps.year}-"
            f"{self.timestamps.month}-"
            f"{self.timestamps.day} "
            f"{self.timestamps.hour}:"
            f"{self.timestamps.minute}:"
            f"{self.timestamps.second}"
        )

        self.timestamps.julian_day = list(
            connect.execute(f"select julianday('{datestr}')")
        )[0][0]
