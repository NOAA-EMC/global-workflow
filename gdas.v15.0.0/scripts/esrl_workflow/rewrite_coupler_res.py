import sys
filein = sys.argv[1]
fileout = sys.argv[2]
lines = list(open(filein))
fout = open(fileout,'w')
# reset model start time to current model time
for nline in range(len(lines)):
    if nline == 1:
        fout.write(lines[-1][0:36]+lines[nline][36:]) # FIXME: hardcoded ranges
    else:
        fout.write(lines[nline])
fout.close()
