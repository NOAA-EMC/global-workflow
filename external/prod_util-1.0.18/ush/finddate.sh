# finddate.sh
# author:  Luke Lin    phone:  457-5047           24 June 1998
# abstract:  This script looks in ether forward or backward in time to
# generate  either a variable containing sequential date/time stamps
# for a period up to a month or just the date/time stamp occurring
# at the end of such a period.
# Time stamp is in the form yyyyddmm.  The script should be good for many
# years. Leap years are accounted for.  Years go 1998, 1999, 2000, 2001,
#   2002, 2003, ....
# etc.
#
# usage:  examples assume todays date is 19990929.
# To generate a sequence looking 10 days forward then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 s+10`
# To generate just the date/time 10 days from now then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 d+10`
# To generate a sequence looking 10 days backward then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 s-10`
# To generate just the date/time 10 days ago then execute:
#     list=`sh /nwprod/util/scripts/finddate.sh 19990929 d-10`
# list will contain 10 time stamps starting with 19990929.  Time stamps
# are separated by blanks.
#
# This script will work for periods up to a month.  The number indicating
# the period in question should be two digits.  For single digits 1-9
# use 01, 02, 03, etc.
set +x
unset pdstr
today=$1
var=$2
yy=`echo $today | cut -c1-4 `
mm=`echo $today | cut -c5-6 `
dd=`echo $today | cut -c7-8 `
nxtyy=$yy
pyy=$yy
what=`echo $var | cut -c1`
up=`echo $var | cut -c2`
num=`echo $var | cut -c3-4`
if test "$up" = '+'
then
  if test "$num" -gt 28
  then
    >&2 echo "WARNING: This script does not support periods more than a month into the future."
  fi
else
  if test "$num" -gt 59
  then
    >&2 echo "WARNING: This script does not support periods more than two months into the past."
  fi
fi
mod=`expr \( $yy / 4 \) \* 4 - $yy `
leap=0
if test "$mod" -eq 0
then
leap=1
fi
case $mm in
01)  mday=31
     pday=31
     pmon=12
     pyy=`expr $yy - 1`
     if test $pyy -lt '0'
     then
       pyy='1999'
     fi
     nxtmon=02;
     nextday=`expr "$leap" + 28 `;
     nnxtmon=03;
     nnxtday=31;
     nnyy=$yy;
     ppmon=11;
     ppday=30;
     ppyy=$pyy;;
02)  mday=`expr "$leap" + 28 `
     pday=31
     pmon=01
     nxtmon=03;
     nxtday=31;
     nnxtmon=04;
     nnxtday=30;
     nnyy=$yy;
     ppmon=12;
     ppyy=`expr $yy - 1`
     if test $ppyy -lt '0'
     then
       ppyy='1999'
     fi
     ppday=31;;
03)  mday=31
     pday=`expr "$leap" + 28 `
     pmon=02
     nxtmon=04;
     nxtday=30;
     nnxtmon=05;
     nntday=31;
     nnyy=$yy;
     ppmon=01;
     ppday=31;
     ppyy=$pyy;;
04)  mday=30
     pday=31
     pmon=03
     nxtmon=05;
     nxtday=31;
     nnxtmon=06;
     nnxtday=30;
     ppmon=02;
     ppday=`expr "$leap" + 28 `;
     ppyy=$pyy;;
05)  mday=31
     pday=30
     pmon=04
     nxtmon=06;
     nxtday=30;
     nnxtmon=07;
     nnxtday=31;
     ppmon=03;
     ppday=31;
     ppyy=$pyy;;
06)  mday=30
     pday=31
     pmon=05
     nxtmon=07;
     nxtday=31;
     nnxtmon=08;
     nnxtday=31;
     ppmon=04;
     ppday=30;
     ppyy=$pyy;;
07)  mday=31
     pday=30
     pmon=06
     nxtmon=08;
     nxtday=31;
     nnxtmon=09;
     nnxtday=30;
     ppmon=05;
     ppday=31;
     ppyy=$pyy;;
08)  mday=31
     pday=31
     pmon=07
     nxtmon=09;
     nxtday=30;
     nnxtmon=10;
     nnxtday=31;
     ppmon=06;
     ppday=30;
     ppyy=$pyy;;
09)  mday=30
     pday=31
     pmon=08
     nxtmon=10;
     nxtday=31;
     nnxtmon=11;
     nnxtday=30;
     ppmon=07;
     ppday=31;
     ppyy=$pyy;;
10)  mday=31
     pday=30
     pmon=09
     nxtmon=11;
     nxtday=30;
     nnxtmon=12;
     nnxtday=31;
     ppmon=08;
     ppday=31;
     ppyy=$pyy;;
11)  mday=30
     pday=31
     pmon=10
     nxtmon=12;
     nxtday=31;
     nnxtmon=01;
     nnxtday=31;
     ppmon=09;
     ppday=30;
     ppyy=$pyy;;
12)  mday=31
     pday=30
     pmon=11
     nxtmon=01
     nxtyy=`expr $yy + 1 `
     if test $yy -eq 1999
     then
       nxtyy=2000
     fi ;
     nxtday=31;
     nnxtmon=02;
     nnxtday=`expr "$leap" + 28 `;
     ppmon=10;
     ppday=31;
     ppyy=$pyy;;
*)   echo mon=$mon is illegal
     exit 99 ;;
esac
 
if test $dd -gt $mday
then
  echo "day=$dd is illegal.  In month=$mon there are only $mday days."
  exit 16
fi
 
i=1
n=0
while test $i -le $num
do
  if test "$up" = '+'
  then
    ddn=`expr $dd + $i`
    mmn=$mm
    yyn=$yy
    if test $ddn -gt $mday
    then
      n=`expr $n + 1`
      if [ $n -le 0 ]; 
      then
        ddn=`expr $ppday + $n`
        mmn=$ppmon
        yyn=$ppyy
      else
      ddn=$n
      mmn=$nxtmon
      yyn=$nxtyy
      fi
    fi
    if test $ddn -lt 10
    then
      ddn="0$ddn"
    fi
  elif test "$up" = '-'
  then
    ddn=`expr $dd - $i`
    mmn=$mm
    yyn=$yy
    if test $ddn -le '0'
    then
      n=`expr $pday + $ddn`
      if [ $n -le 0 ];
      then
        ddn=`expr $ppday + $n`
        mmn=$ppmon
        yyn=$ppyy
      else
      ddn=$n
      mmn=$pmon
      yyn=$pyy
      fi
    fi
    if test $ddn -lt 10
    then
      ddn="0$ddn"
    fi
  else
    echo '+ or - are allowed for 2nd variable in argument.'
    echo "You tried $up, this is illegal."
    exit 16
  fi
  i=`expr $i + 1 `
  if test "$what" = 's'
  then
  pdstr=$pdstr"$yyn$mmn$ddn "
  else
  pdstr=$yyn$mmn$ddn
  fi
done
echo $pdstr
