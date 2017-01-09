import sys
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
date = sys.argv[1]
yyyy,mm,dd,hh = splitdate(date)
#if dd%2 and hh == 6:
if dd==1 and hh == 6:
    sys.stdout.write('0')
else:
    sys.stdout.write('1')
