import sys
from dateutils import *
import random
date1 = sys.argv[1]
date2 = sys.argv[2]
nanals = int(sys.argv[3])
dates = random.sample(daterange(date1,date2,6),nanals)
for date in dates:
    datem1 = dateshift(date,-24)
    print '%s %s' % (date,datem1)
