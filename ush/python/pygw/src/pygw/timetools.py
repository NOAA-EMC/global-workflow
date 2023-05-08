import re
import datetime


__all__ = ["to_datetime", "to_timedelta",
           "datetime_to_YMDH", "datetime_to_YMD", "datetime_to_YYYY", "datetime_to_DOY",
           "timedelta_to_HMS",
           "strftime", "strptime",
           "to_YMDH", "to_YMD", "to_YYYY", "to_DOY",
           "to_isotime", "to_fv3time",
           "add_to_datetime", "add_to_timedelta"]


_DATETIME_RE = re.compile(
    r"(?P<year>\d{4})(-)?(?P<month>\d{2})(-)?(?P<day>\d{2})"
    r"(T)?(?P<hour>\d{2})?(:)?(?P<minute>\d{2})?(:)?(?P<second>\d{2})?(Z)?")

_TIMEDELTA_HOURS_RE = re.compile(
    r"(?P<sign>[+-])?"
    r"((?P<days>\d+)[d])?"
    r"(T)?((?P<hours>\d+)[H])?((?P<minutes>\d+)[M])?((?P<seconds>\d+)[S])?(Z)?")
_TIMEDELTA_TIME_RE = re.compile(
    r"(?P<sign>[+-])?"
    r"((?P<days>\d+)(\s)day(s)?,(\s)?)?"
    r"(T)?(?P<hours>\d{1,2})?(:(?P<minutes>\d{1,2}))?(:(?P<seconds>\d{1,2}))?")


def to_datetime(dtstr: str) -> datetime.datetime:
    """
    Description
    -----------
    Translate a string into a datetime object in a generic way.
    The string can also support ISO 8601 representation.

    Formats accepted (T, Z, -, :) are optional:
    YYYY-mm-dd
    YYYY-mm-ddTHHZ
    YYYY-mm-ddTHH:MMZ
    YYYY-mm-ddTHH:MM:SSZ

    Parameters
    ----------
    dtstr : str
        String to be translated into a datetime object

    Returns
    -------
    datetime.datetime
        Datetime object
    """

    mm = _DATETIME_RE.match(dtstr)
    if mm:
        return datetime.datetime(**{kk: int(vv) for kk, vv in mm.groupdict().items() if vv})
    else:
        raise Exception(f"Bad datetime string: '{dtstr}'")


def to_timedelta(tdstr: str) -> datetime.timedelta:
    """
    Description
    -----------
    Translate a string into a timedelta object in a generic way

    Formats accepted (<sign>, T, Z) are optional:
    <sign><dd>dT<hh>H<mm>M<ss>SZ
    <sign><dd>day(s), hh:mm:ss

    <sign> can be +/-, default is +
    <dd> can be any integer, default is 0
    <hh> can be any integer, default is 0
    <mm> can be any integer, default is 0
    <ss> can be any integer, default is 0

    Parameters
    ----------
    tdstr : str
        String to be translated into a timedelta object

    Returns
    -------
    datetime.timedelta
        Timedelta object
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
        del nmm['sign']
        nmm = {kk: float(vv) for kk, vv in nmm.items()}
        dt = datetime.timedelta(**nmm)
        if mm.group('sign') is not None and mm.group('sign') == '-':
            dt = -dt
        return dt
    else:
        raise Exception(f"Bad timedelta string: '{tdstr}'")


def datetime_to_YMDH(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Translate a datetime object to 'YYYYmmddHH' format.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to translate.

    Returns
    -------
    str: str
        Formatted string in 'YYYYmmddHH' format.
    """
    try:
        return dt.strftime('%Y%m%d%H')
    except Exception:
        raise Exception(f"Bad datetime: '{dt}'")


def datetime_to_YMD(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Translate a datetime object to 'YYYYmmdd' format.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to translate.

    Returns
    -------
    str: str
        Formatted string in 'YYYYmmdd' format.
    """
    try:
        return dt.strftime('%Y%m%d')
    except Exception:
        raise Exception(f"Bad datetime: '{dt}'")


def datetime_to_YYYY(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Translate a datetime object to 'YYYY' format.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to translate.

    Returns
    -------
    str: str
        Formatted string in 'YYYY' format.
    """
    try:
        return dt.strftime('%Y')
    except Exception:
        raise Exception(f"Bad datetime: '{dt}'")


def datetime_to_DOY(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Translate a datetime object to 'DOY' format.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to translate.

    Returns
    -------
    str: str
        Formatted string in 'DOY' format.
    """
    try:
        return dt.strftime('%j')
    except Exception:
        raise Exception(f"Bad datetime: '{dt}'")


def timedelta_to_HMS(td: datetime.timedelta) -> str:
    """
    Description
    -----------
    Translate a timedelta object to 'HH:MM:SS' format.

    Parameters
    ----------
    td : datetime.timedelta
        Timedelta object to translate.

    Returns
    -------
    str: str
        Formatted string in 'HH:MM:SS' format.
    """
    try:
        hours, remainder = divmod(int(td.total_seconds()), 3600)
        minutes, seconds = divmod(remainder, 60)
        return f"{hours:02d}:{minutes:02d}:{seconds:02d}"
    except Exception:
        raise Exception(f"Bad timedelta: '{td}'")


def strftime(dt: datetime.datetime, fmt: str) -> str:
    """
    Return a formatted string from a datetime object.
    """
    try:
        return dt.strftime(fmt)
    except Exception:
        raise Exception(f"Bad datetime (format): '{dt} ({fmt})'")


def strptime(dtstr: str, fmt: str) -> datetime.datetime:
    """
    Description
    -----------
    Translate a formatted string into datetime object.

    Parameters
    ----------
    dtstr : str
        Datetime string to translate.
    fmt : str
        Datetime string format.

    Returns
    -------
    datetime.datetime: datetime.datetime
        Datetime object.
    """
    try:
        return datetime.datetime.strptime(dtstr, fmt)
    except Exception:
        raise Exception(f"Bad datetime string (format): '{dtstr} ({fmt})'")


def to_isotime(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Return a ISO formatted '%Y-%m-%dT%H:%M:%SZ' string from a datetime object.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to format.

    Returns
    -------
    str: str
        Formatted string in ISO format.
    """
    return strftime(dt, '%Y-%m-%dT%H:%M:%SZ')


def to_fv3time(dt: datetime.datetime) -> str:
    """
    Description
    -----------
    Return a FV3 formatted string from a datetime object.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to format.

    Returns
    -------
    str: str
        Formatted string in FV3 format.
    """
    return strftime(dt, '%Y%m%d.%H%M%S')


def add_to_datetime(dt: datetime.datetime, td: datetime.timedelta) -> datetime.datetime:
    """
    Description
    -----------
    Adds a timedelta to a datetime object.

    Parameters
    ----------
    dt : datetime.datetime
        Datetime object to add to.
    td : datetime.timedelta
        Timedelta object to add.

    Returns
    -------
    datetime.datetime
    """
    return dt + td


def add_to_timedelta(td1, td2):
    """
    Description
    -----------
    Adds two timedelta objects.

    Parameters
    ----------
    td1 : datetime.timedelta
        First timedelta object to add.
    td2 : datetime.timedelta
        Second timedelta object to add.

    Returns
    -------
    datetime.timedelta
    """
    return td1 + td2


to_YMDH = datetime_to_YMDH
to_YMD = datetime_to_YMD
to_YYYY = datetime_to_YYYY
to_DOY = datetime_to_DOY
