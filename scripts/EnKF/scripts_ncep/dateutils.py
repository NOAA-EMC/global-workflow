import datetime, calendar

"""
utilities for working with dates using datetime module (Python 2.3 or later)

Jeff Whitaker <jeffrey.s.whitaker@noaa.gov>
"""


hrsgregstart = 13865688 # hrs from 00010101 to 15821015 in Julian calendar.
# times in many datasets use mixed Gregorian/Julian calendar, datetime 
# module uses a proleptic Gregorian calendar. So, I use datetime to compute
# hours since start of Greg. calendar (15821015) and add this constant to
# get hours since 1-Jan-0001 in the mixed Gregorian/Julian calendar.
gregstart = datetime.datetime(1582,10,15) # datetime.datetime instance
day1 = datetime.datetime(1,1,1) # datetime.datetime instance

def dateto_hrs_since_day1CE(curdate,mixedcal=True):
    """given datetime.datetime instance, compute hours since 1-Jan-0001"""
    if mixedcal:
        if curdate < gregstart:
            msg = 'date must be after start of gregorian calendar (15821015)!'
            raise ValueError, msg
        difftime = curdate-gregstart
        hrsdiff = 24*difftime.days + difftime.seconds/3600
        return hrsdiff+hrsgregstart
    else:
        difftime = curdate-day1
        return 24.*(difftime.days+1)+difftime.seconds/3600.

def hrs_since_day1CE_todate(hrs,mixedcal=True):
    """return datetime.datetime instance given hours since 1-Jan-0001"""
    if hrs < 0.0:
        msg = "hrs must be positive!"
        raise ValueError, msg
    delta = datetime.timedelta(hours=1)
    if mixedcal:
        hrs_sincegreg = hrs - hrsgregstart
        curdate = gregstart + hrs_sincegreg*delta
    else:
        curdate = hrs*delta
    return curdate

def dateshift(analdate,fcsthr):
    """
 verifdate = incdate(analdate, fcsthr)

 compute verification date given analysis date string (yyyymmddhh) and
 fcst hour.
    """
    yyyy,mm,dd,hh = splitdate(analdate)
    analdate = datetime.datetime(yyyy,mm,dd,hh)
    verifdate = analdate + fcsthr*datetime.timedelta(hours=1)
    verifdate = makedate(verifdate.year,verifdate.month,verifdate.day,verifdate.hour)
    return verifdate


def splitdate(yyyymmddhh):
    """
 yyyy,mm,dd,hh = splitdate(yyyymmddhh)

 give an date string (yyyymmddhh) return integers yyyy,mm,dd,hh.
    """
    yyyy = int(yyyymmddhh[0:4])
    mm = int(yyyymmddhh[4:6])
    dd = int(yyyymmddhh[6:8])
    hh = int(yyyymmddhh[8:10])
    return yyyy,mm,dd,hh

def makedate(yyyy,mm,dd,hh):
    """
 yyyymmddhh = makedate(yyyy,mm,dd,hh)

 return a date string of the form yyyymmddhh given integers yyyy,mm,dd,hh.
    """
    return '%0.4i'%(yyyy)+'%0.2i'%(mm)+'%0.2i'%(dd)+'%0.2i'%(hh)

def hrstodate(hrs,mixedcal=True):
    """
 yyyymmddhh = hrstodate(hrs)

 return a date string of the form yyyymmddhh given hrs since day 1 CE.
    """
    date = hrs_since_day1CE_todate(hrs,mixedcal=mixedcal)
    return makedate(date.year,date.month,date.day,date.hour)

def datetohrs(yyyymmddhh,mixedcal=True):
    """
 hrs = hrstodate(yyyymmddhh)

 return hrs since day 1 CE given a date string of the form yyyymmddhh.
    """
    yyyy,mm,dd,hh = splitdate(yyyymmddhh)
    return dateto_hrs_since_day1CE(datetime.datetime(yyyy,mm,dd,hh),mixedcal=mixedcal)

def daterange(date1,date2,hrinc):
    """
 date_list = daterange(date1,date2,hrinc)

 return of list of date strings of the form yyyymmddhh given
 a starting date, ending date and an increment in hours.
    """
    date = date1
    delta = datetime.timedelta(hours=1)
    yyyy,mm,dd,hh = splitdate(date)
    d = datetime.datetime(yyyy,mm,dd,hh)
    n = 0
    dates = [date]
    while date < date2:
       d = d + hrinc*delta
       date = makedate(d.year,d.month,d.day,d.hour)
       dates.append(date)
       n = n + 1
    return dates

def dayofyear(yyyy,mm,dd):
    """
 return integer day of year given yyyy,mm,dd
    """
    d = datetime.datetime(yyyy,mm,dd)
    d0 = datetime.datetime(yyyy,1,1)
    return (d-d0).days

def getyrmon(day_of_year,yyyy=2001):
    d1 = datetime.datetime(yyyy,1,1)
    if calendar.isleap(d1.year) and day_of_year > 366:
        raise ValueError, 'not that many days in the year'
    if not calendar.isleap(d1.year) and day_of_year > 365:
        raise ValueError, 'not that many days in the year'
    d2 = d1 + (day_of_year-1)*datetime.timedelta(days=1)
    return d2.month,d2.day

def daysinmonth(yyyy,mm):
    """
 return number of days in month given yyyy,mm
    """
    return calendar.monthrange(yyyy,mm)[1]

if __name__ == "__main__":
    print dayofyear(2000,2,29)
    print daysinmonth(2000,2)
    print datetohrs('0001010100',mixedcal=False)
    print datetohrs('2001010100',mixedcal=False)
    print datetohrs('2001010100',mixedcal=True)
