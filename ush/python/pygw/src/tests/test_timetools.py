from datetime import datetime, timedelta
from pygw.timetools import *

current_date = datetime.now()


def test_to_datetime():

    assert to_datetime('20220314') == datetime(2022, 3, 14)
    assert to_datetime('2022031412') == datetime(2022, 3, 14, 12)
    assert to_datetime('202203141230') == datetime(2022, 3, 14, 12, 30)
    assert to_datetime('2022-03-14') == datetime(2022, 3, 14)
    assert to_datetime('2022-03-14T12Z') == datetime(2022, 3, 14, 12)
    assert to_datetime('2022-03-14T12:30Z') == datetime(2022, 3, 14, 12, 30)
    assert to_datetime('2022-03-14T12:30:45') == datetime(2022, 3, 14, 12, 30, 45)
    assert to_datetime('2022-03-14T12:30:45Z') == datetime(2022, 3, 14, 12, 30, 45)


def test_to_timedelta():
    assert to_timedelta('2d3H4M5S') == timedelta(days=2, hours=3, minutes=4, seconds=5)
    assert to_timedelta('-3H15M') == timedelta(hours=-3, minutes=-15)
    assert to_timedelta('1:30:45') == timedelta(hours=1, minutes=30, seconds=45)
    assert to_timedelta('5 days, 12:30:15') == timedelta(days=5, hours=12, minutes=30, seconds=15)


def test_datetime_to_ymdh():
    assert datetime_to_YMDH(current_date) == current_date.strftime('%Y%m%d%H')


def test_datetime_to_ymd():
    assert datetime_to_YMD(current_date) == current_date.strftime('%Y%m%d')


def test_timedelta_to_hms():
    td = timedelta(hours=5, minutes=39, seconds=56)
    assert timedelta_to_HMS(td) == '05:39:56'
    td = timedelta(days=4, hours=5, minutes=39, seconds=56)
    assert timedelta_to_HMS(td) == '101:39:56'


def test_strftime():
    assert strftime(current_date, '%Y%m%d') == current_date.strftime('%Y%m%d')
    assert strftime(current_date, '%Y%m%d %H') == current_date.strftime('%Y%m%d %H')


def test_strptime():
    assert strptime(current_date.strftime('%Y%m%d'), '%Y%m%d') == \
        datetime.strptime(current_date.strftime('%Y%m%d'), '%Y%m%d')


def test_to_isotime():
    assert to_isotime(current_date) == current_date.strftime('%Y-%m-%dT%H:%M:%SZ')


def test_to_fv3time():
    assert to_fv3time(current_date) == current_date.strftime('%Y%m%d.%H%M%S')


def test_to_julian():
    assert to_julian(current_date) == current_date.strftime('%Y%j')


def test_add_to_timedelta():
    assert add_to_timedelta(timedelta(days=1), timedelta(hours=3)) == \
        timedelta(days=1, hours=3)
    assert add_to_timedelta(timedelta(hours=5, minutes=30), timedelta(minutes=15)) == \
        timedelta(hours=5, minutes=45)
    assert add_to_timedelta(timedelta(seconds=45), timedelta(milliseconds=500)) == \
        timedelta(seconds=45, milliseconds=500)


def test_add_to_datetime():
    dt = datetime(2023, 3, 14, 12, 0, 0)
    td = timedelta(days=1, hours=6)
    negative_td = timedelta(days=-1, hours=-6)
    zero_td = timedelta()

    assert add_to_datetime(dt, td) == datetime(2023, 3, 15, 18, 0, 0)
    assert add_to_datetime(dt, negative_td) == datetime(2023, 3, 13, 6, 0, 0)
    assert add_to_datetime(dt, zero_td) == datetime(2023, 3, 14, 12, 0, 0)
