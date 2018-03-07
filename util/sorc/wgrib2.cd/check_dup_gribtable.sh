#!/bin/sh

# check for duplicate names
# {0,1,0,255,0,0,0,0, "TMP", "Temperature", "K"},

in=gribtable.dat

sed -e 's/^[^"]*"//'  -e 's/".*//' <$in | sort  >junk
sort -u <junk >junk2
diff junk junk2 >junk.diff
echo "finished"
