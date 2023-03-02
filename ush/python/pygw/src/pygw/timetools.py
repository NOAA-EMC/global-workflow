import re
import datetime


__all__ = ["to_datetime", "to_timedelta",
           "datetime_to_YMDH", "datetime_to_YMD",
           "timedelta_to_HMS",
           "strftime", "strptime"]


_DATETIME_RE = re.compile(
    r"(?P<year>\d{4})(-)?(?P<month>\d{2})(-)?(?P<day>\d{2})"
    r"(T)?(?P<hour>\d{2})?(:)?(?P<minute>\d{2})?(:)?(?P<second>\d{2})?(Z)?")

_TIMEDELTA_HOURS_RE = re.compile(
    r"(?P<sign>[+-])?"
    r"((?P<days>\d+)[d])?(T)?((?P<hours>\d+)[H])?((?P<minutes>\d+)[M])?((?P<seconds>\d+)[S])?(Z)?")
_TIMEDELTA_TIME_RE = re.compile(
    r"(?P<sign>[+-])?"
    r"((?P<days>\d+)\s+day(s)?,\s)?(T)?(?P<hours>\d{1,2})?(:(?P<minutes>\d{1,2}))?(:(?P<seconds>\d{1,2}))?")


def to_datetime(dtstr):
    """
    Translate a string into a datetime object in a generic way.
    The string can also support ISO 8601 representation.

    Formats accepted (T, Z, -, :) are optional:
    YYYY-mm-dd
    YYYY-mm-ddTHHZ
    YYYY-mm-ddTHH:MMZ
    YYYY-mm-ddTHH:MM:SSZ
    """

    mm = _DATETIME_RE.match(dtstr)
    if mm:
        return datetime.datetime(**{kk: int(vv) for kk, vv in mm.groupdict().items() if vv})
    else:
        raise Exception(f"Bad datetime string: '{dtstr}'")


def to_timedelta(tdstr):
    """
    Translate a string into a timedelta object in a generic way

    Formats accepted (<sign>, T, Z) are optional:
    <sign><dd>dT<hh>H<mm>M<ss>SZ
    <sign><dd>day(s), hh:mm:ss

    <sign> can be +/-, default is +
    """

    time_dict = {'sign': '+',
                 'days': 0,
                 'hours': 0,
                 'minutes': 0,
                 'seconds': 0}

    if any(x in tdstr for x in ['day', 'days', ':']):
        mm = _TIMEDELTA_TIME_RE.match(tdstr)  # timedelta representation
    else:
        mm = _TIMEDELTA_HOURS_RE.match(tdstr)  # ISO 8601 representation

    if mm:
        nmm = {kk: vv if vv is not None else time_dict[kk]
               for kk, vv in mm.groupdict().items()}
        sign = nmm['sign']
        del nmm['sign']
        nmm = {kk: float(vv) for kk, vv in nmm.items()}
        dt = datetime.timedelta(**nmm)
        if mm.group('sign') is not None and mm.group('sign') == '-':
            dt = -dt
        return dt
    else:
        raise Exception(f"Bad timedelta string: '{tdstr}'")


def datetime_to_YMDH(dt):
    """
    Translate a datetime object to 'YYYYmmddHH' format.
    """
    try:
        return dt.strftime('%Y%m%d%H')
    except Exception as ee:
        raise Exception(f"Bad datetime: '{dt}'")


def datetime_to_YMD(dt):
    """
    Translate a datetime object to 'YYYYmmdd' format.
    """
    try:
        return dt.strftime('%Y%m%d')
    except Exception as ee:
        raise Exception(f"Bad datetime: '{dt}'")


def timedelta_to_HMS(td):
    """
    Translate a timedelta object to 'HHMMSS' format.
    """
    try:
        hours, remainder = divmod(int(td.total_seconds()), 3600)
        minutes, seconds = divmod(remainder, 60)
        return f"{hours:02d}:{minutes:02d}:{seconds:02d}"
    except Exception as ee:
        raise Exception(f"Bad timedelta: '{td}'")


def strftime(dt, fmt):
    """
    Return a formatted string from a datetime object.
    """
    try:
        return dt.strftime(fmt)
    except Exception as ee:
        raise Exception(f"Bad datetime (format): '{dt} ({fmt})'")


def strptime(dtstr, fmt):
    """
    Translate a formatted string into datetime object.
    """
    try:
        return datetime.datetime.strptime(dtstr, fmt)
    except Exception as ee:
        raise Exception(f"Bad datetime string (format): '{dtstr} ({fmt})'")


def to_isotime(dt):
    """
    Return a ISO formatted '%Y-%m-%dT%H:%M:%SZ' string from a datetime object.
    """
    return strftime(dt, '%Y-%m-%dT%H:%M:%SZ')


def to_fv3time(dt):
    """
    Return a FV3 formatted '%Y%m%d.%H%M%S' string from a datetime object.
    """
    return strftime(dt, '%Y%m%d.%H%M%S')
