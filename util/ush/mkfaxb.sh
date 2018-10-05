# UTILITY SCRIPT NAME : mkfaxb.sh
#              AUTHOR : Steve Lilly
# 
# ABSTRACT : This utility script produces FAX CHARTS
#
#            *Note:  This utility expects a parm fild for
#                    the label on the map.  If you don't
#                    have a parm fild, the calling script
#                    will input NONE.
#
# INPUT: 4 arguments are input to this script.
#         1st argument - hour1   - Fcst hour of 1st grib file
#         2nd argument - faxparm - parameter input.
#         3rd argument - out     - fax chart file
#         4th argument - sendkey for snd2forgntbl.sh.
#         5th argument - [optional] old plot vpap tape55,
#
echo "History: AUG     1999 - Modified for IBM SP"
# History: AUG 2005 Lilly - remove snd2forgn and  change 6bit
#                           format to T4.
#

set +x
hour_list=$1
faxparm=$2
out=$3
sendkey=$4
num=$#

if test $num -eq 4 -o $num -eq 5
then
   echo " Appropriate number of arguments were passed"
   set -x
   if [ -z "$DATA" ]
   then
      export DATA=`pwd`
      cd $DATA
      setpdy.sh
      . PDY
   fi
   export do_label=${do_label:-NO}
else
   echo ""
   echo "Usage: mkfaxb.sh "hour_list" faxparm outputname sendkey [plottape55]"
   echo ""
   exit 16
fi

if test "$do_label" = "YES"
then
   read parm < parm
   newparm="PARM='$parm'"
   echo "newparm = $newparm"
else
   unset newparm
fi

set +x
echo "########################################################"
echo "#  Begin making the VARIAN FAX CHART $out maps"
echo "########################################################"
set -x

msg="Enter Make FAX GRAPHICS utility."
postmsg "$jlogfile" "$msg"

##############################
# Process FAX Chart
##############################
# cp $FIXshared/graph_gphbg/nh4004.pur nh4004.pur
cp $UTILgfs/fix/graph_gphbg/nh4004.pur nh4004.pur

#cp $FIXshared/graph_gphbg/nh2005.pur nh2005.pur
cp $UTILgfs/fix/graph_gphbg/nh2005.pur nh2005.pur

# cp $FIXshared/graph_gphbg/nh4006.pur nh4006.pur
cp $UTILgfs/fix/graph_gphbg/nh4006.pur nh4006.pur

# cp $FIXshared/graph_gphbg/nh2007.pur nh2007.pur
cp $UTILgfs/fix/graph_gphbg/nh2007.pur nh2007.pur

# cp $FIXshared/graph_gphbg/nh4005.pur nh4005.pur
cp $UTILgfs/fix/graph_gphbg/nh4005.pur nh4005.pur

# cp $FIXshared/graph_gphbg/sh4001.pur sh4001.pur
cp $UTILgfs/fix/graph_gphbg/sh4001.pur sh4001.pur

if test $sendkey = 'fax.brknel' -o $sendkey = 'fax.bcmrfmp1'
then
#  export pgm=$FAXMAKRX
  export pgm=$UTILgfs/exec/faxmakrx
else
#  export pgm=$FAXMAKR 
  export pgm=$UTILgfs/exec/faxmakr
fi
prep_step

echo sendkey=$sendkey
echo pgm=$pgm
# read any
##############################
# Copy Input Field to $DATA
##############################
pgbunit=11
pgbiunit=31

for i in $hour_list
do
   echo $i
   case $i in
         [0-9][0-9]|[0-9][0-9][0-9])case $NET in
               gfs|mrf)grbext=pgrbf0${i}
                       grbiext=pgrbif0${i}
                       grb2ext=pgrb2.1p00.f0${i}
                       ;;
               nam) if [ "$CGRID" = "YES" ] ; then
                      grbext=grb_fm${i}.tm00
                      grbiext=grb_fmi${i}
                    else
                      grbext=grb5fm${i}.tm00
                      grbiext=grb5fmi${i}
                    fi
                   ;;
                 *)grbext=pgrbf0${i}
                   grbiext=pgrbif0${i}
                   ;;
            esac
            ;;
        anl)grbext=pgrbanl
            grbiext=pgrbianl
            grb2ext=pgrb2.1p00.anl
            ;;
        D+3)grbext=D+3.z500.grib
            grbiext=D+3.z500.gribi
            ;;
          *)grbext=$i
            grbiext=${i}i
            ;;
   esac

   if test ! -f $grbext
   then
      cp $COMIN/${RUN}.${cycle}.$grb2ext .
      $CNVGRIB -g21 ${RUN}.${cycle}.$grb2ext  $grbext
   fi

   if test ! -f $grbiext
   then
      $GRBINDEX $grbext $grbiext
   fi

   eval export FORT${pgbunit}="$grbext"
   eval export FORT${pgbiunit}="$grbiext"

   pgbunit=`expr $pgbunit + 1`
   pgbiunit=`expr $pgbiunit + 1`
   if [ $pgbunit -eq 15 ] ; then
     pgbunit=`expr $pgbunit + 1`
     pgbiunit=`expr $pgbiunit + 1`
   fi
done

# export FORT15="$PARMshared/$faxparm"
export FORT15="$UTILgfs/parm/$faxparm"

# export FORT48="$FIXshared/graph_awpseed"
export FORT48="$UTILgfs/fix/graph_awpseed"

export FORT49="ncepdate"
if test $num -eq 5
then
   export FORT54="$5"
fi
export FORT55="label55"
export FORT60="label60"
export FORT61="label61"
export FORT62="label62"
export FORT63="label63"
export FORT71="$out"    # fax chart file
export FORT76="outdgn1" # output design file
export FORT77="outdgn2" # output design file
export FORT78="outdgn3" # output design file
export FORT79="outdgn4" # output design file
export FORT80="faxext8" # temp output file
export FORT89="pureras"
 
startmsg
$pgm $newparm >> $pgmout 2> errfile
export err=$?; err_chk

msg="mkfax ${hour} hour completed normally"
postmsg "$jlogfile" "$msg"

exit
