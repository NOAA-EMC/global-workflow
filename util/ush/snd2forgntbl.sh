#  UTILITY SCRIPT NAME :  snd2forgntbl.sh
#______________________________________________________________________________
# NAME     : snd2forgntbl.sh
# History  : 1997-03-04 Paula Stone
# MODIFIED : 1997-03-04 Paula Stone Remove -i from grep. Because
#          	        sometimes gets more than one record.
#          : 1999-12-02 Peter Henrichsen modified to run on the IBM SP.
#          : 2000-01-20 Peter Henrichsen modify to pass sndfrc to script
#            snd2forgn and to use USHutil
#
#  Abstract:  This utility script sends a file via snd2forgn
# 	      using the ftype and foreign fields defined in the
#             graph_snd2forgn.names table.
#             Variables to be exported to this script:
#             FIXgraph - where graph_snd2forgn.names resides)
#             JOB - name of this job.
#
# Location : This script is found on hp36 as:
#          : /tmp_mnt/export/sgi73/peterhen/util/scripts/ibm/snd2forgntbl.sh
#          : on ncosp as:
#          : /nfsuser/g02/wx12ph/util/ush/snd2forgntbl.sh or
#            /nwprod/util/ush/snd2forgntbl.sh
#
#     Input:  three variables are passed through the call
#             1 ... sendkey -- key used in snd2forgn.names table.
#             2 ... filename -- name of file on local host.
#             3 ... dirname -- name of directory where file resides.
#

set -x

if test $# -ne 3
then
  echo "Error: usage snd2table.sh <sendkey> <directory>"
  exit
fi

sendkey=$1
filename=$2
dirname=$3
echo Search snd2forgn table for $sendkey
# grep $sendkey $FIXshared/graph_snd2forgn.names >> sendline
grep $sendkey ${UTILgfs}/fix/graph_snd2forgn.names >> sendline
if [ -s sendline ]
then
   ftype=`awk '{print $2}' sendline`
   foreign=`awk '{print $3}' sendline`
   sndfrc=0
   echo $sndfrc >$DATA/sndfrc


   snd2forgn $ftype $foreign $job $dirname/$filename
       read sndfrc < $DATA/sndfrc

      if test $sndfrc -eq '0'
      then
        msg="snd2forgntbl.sh successfully ended!"
        postmsg "$jlogfile" "$msg"
      else
        msg="ERROR $filename NOT POSTED!"
        postmsg "$jlogfile" "$msg"
        msg="snd2forgn: ABNORMAL STOP = $sndfrc!:"
        postmsg "$jlogfile" "$msg"
      fi
else
   echo Sendkey $sendkey is not in the snd2forgn.names table
   exit -1
fi
rm sendline
exit
