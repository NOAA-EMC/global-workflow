from datetime import datetime, timedelta
from pygw.timetools import *

current_date = datetime.now()


def test_to_datetime():
    datetime_pairs = [
        ('20221215', datetime(2022, 12, 15, 0, 0, 0)),
        ('2022121518', datetime(2022, 12, 15, 18, 0, 0)),
        ('202212151830', datetime(2022, 12, 15, 18, 30, 0)),
        ('2022121518Z', datetime(2022, 12, 15, 18, 0, 0)),
        ('20221215T1830', datetime(2022, 12, 15, 18, 30, 0)),
        ('20221215T1830Z', datetime(2022, 12, 15, 18, 30, 0)),
        ('2022-12-15T18:30', datetime(2022, 12, 15, 18, 30, 0)),
        ('2022-12-1518:30', datetime(2022, 12, 15, 18, 30, 0)),
        ('2022-12-15T18:30Z', datetime(2022, 12, 15, 18, 30, 0)),
        ('2022-12-15T18:30:45Z', datetime(2022, 12, 15, 18, 30, 45)),
    ]
    for pair in datetime_pairs:
        assert to_datetime(pair[0]) == pair[1]


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


def test_add_to_datetime():
    td = timedelta(hours=5, minutes=39, seconds=56)
    dt = datetime(2022, 12, 15, 18, 30, 0)
    assert add_to_datetime(dt, td) == datetime(2022, 12, 16, 0, 9, 56)


def test_add_to_timedelta():
    td1 = timedelta(hours=5, minutes=39, seconds=56)
    td2 = timedelta(hours=0, minutes=20, seconds=4)
    assert add_to_timedelta(td1, td2) == timedelta(hours=6, minutes=0, seconds=0)
