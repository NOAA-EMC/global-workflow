import sys
import random
n1 = int(sys.argv[1])
nens = int(sys.argv[2])
array = random.sample(xrange(n1), nens)
for a1 in array:
    print '%s' % (a1)
