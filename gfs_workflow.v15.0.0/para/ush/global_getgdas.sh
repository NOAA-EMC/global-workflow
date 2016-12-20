#!/bin/sh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_getgdas.sh
# Script description:  Copies a global gdas
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-07-15
#
# Abstract: This script copies a global gdas.
#
# Script history log:
# 1999-05-01  Mark Iredell
# 2002-03-08  Mark Iredell  generalize suffixes
# 2005-01-24  Mark Iredell  transition to blue
#
# Usage:  global_getgdas.sh CDATE CDUMP GFILES
#
#   Input script positional parameters:
#     1             Current date in YYYYMMDDHH form
#                   defaults to $CDATE; required
#     2             Current dump (gfs or fnl)
#                   defaults to $CDUMP, then to fnl
#     3             Dump file list
#                   defaults to $GFILES, then to all possible
#
#   Imported Shell Variables:
#     COMOUT        output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     PREOUT        Prefix to add to output filenames
#                   defaults to none
#     SUFOUT        Suffix to add to output filenames
#                   defaults to .$CDUMP.$CDATE
#     NCP           Copy command
#                   defaults to cp
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####

################################################################################
#  Set environment.

export CDATE=${1:-${CDATE:?}}
export CDUMP=${2:-${CDUMP:-fnl}}
nshift=$#
[[ $nshift -le 2 ]]||nshift=2
shift $nshift
export GFILES="${*:-${GFILES}}"
if [[ -z "$GFILES" ]]
then
   GFILES="$GFILES prepqc biascr satang $SFCISUF $SIGISUF"
   GFILES="$GFILES ${SFCOSUF}f06 ${SIGOSUF}f03 ${SIGOSUF}f06 ${SIGOSUF}f09"
   GFILES="$GFILES siggm3 sigges siggp3"
   GFILES="$GFILES pgbanl pgbf00 pgbf03 pgbf06 pgbf09"
fi

export HOST1=${HOST1:-$(hostname|cut -c1)}
export COMOUT=${COMOUT:-$(pwd)}
export PREOUT=${PREOUT:-""}
export SUFOUT=${SUFOUT:-".$CDUMP.$CDATE"}
export NCP=${NCP:-cp}
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

################################################################################
#  Define old filenames

day=$(echo $CDATE|cut -c1-8)
cyc=t$(echo $CDATE|cut -c9-10)z
   if [[ "$CDUMP" = gfs || "$CDUMP" = avn ]]
   then
      com=/com/gfs/prod/gfs.$day/gfs.$cyc
   else
      com=/com/gfs/prod/gdas.$day/gdas1.$cyc
   fi
O_prepqc=$com.prepbufr
O_biascr=$com.abias
O_satang=$com.satang
O_sfcanl=$com.sfcanl
O_siganl=$com.sanl
O_sfcf06=$com.bf06
O_sigf03=$com.sf03
O_sigf06=$com.sf06
O_sigf09=$com.sf09
O_siggm3=$com.sgm3prep
O_sigges=$com.sgesprep
O_siggp3=$com.sgp3prep
O_pgbanl=$com.pgrbanl
O_pgbf00=$com.pgrbanl
O_pgbf03=$com.pgrbf03
O_pgbf06=$com.pgrbf06
O_pgbf09=$com.pgrbf09

################################################################################
#  Copy to new filenames

eval preout=$PREOUT
eval sufout=$SUFOUT
cpy=""
cpn=""
for ft in $GFILES
do
   eval fto=\$O_$ft
   ftn=$COMOUT/$preout$ft$sufout
   ${NCP:-cp} -p $fto $ftn
   if [[ $? -eq 0 ]]
   then
      chmod 644 $ftn
      cpy="$cpy $ft"
   else
      cpn="$cpn $ft"
   fi
done
echo File types     copied: $cpy
echo File types NOT copied: $cpn

################################################################################
#  Postprocessing
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
