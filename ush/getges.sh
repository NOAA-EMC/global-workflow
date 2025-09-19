#! /usr/bin/env bash

################################################################################
#
# Name:  getges.sh            Author:  Mark Iredell
#
# Abstract:
# This script copies the valid global guess file to a given file.
# Alternatively, it writes the name of the guess file to standard output.
# Specify option "-n network" for the job network (default global).
# Other options are gdas, gfs, cdas, mrf, prx, etc.
# Specify option "-e environment" for the job environment (default prod).
# Another option is test.
# Specify option "-f fhour" for the specific forecast hour wanted (default any).
# Specify option "-q" for quiet mode to turn off script messages.
# Specify option "-r resolution" for the resolution wanted (default high).
# Other options are 25464 17042, 12628, low, 6228, namopl, any.
# Specify option "-t filetype" for the filetype wanted from among these choices:
# sigges (default), siggm3, siggm2, siggm1, siggp1, siggp2, siggp3,
# sfcges, sfcgm3, sfcgm2, sfcgm1, sfcgp1, sfcgp2, sfcgp3,
# biascr, satang, satcnt, gesfil
# pgbges, pgiges, pgbgm6, pgigm6, pgbgm3, pgigm3, pgbgp3, pgigp3,
# sigcur, sfccur, pgbcur, pgicur, prepqc, tcvg12, tcvges, tcvitl,
# enggrb, enggri, icegrb, icegri, snogrb, snogrb_high, snogri, sstgrb, sstgri.
# natges, natgm3, natgm2, natgm1, natgp1, natgp2, natgp3, natcur,
# nsfges, nsfgm3, nsfgm2, nsfgm1, nsfgp1, nsfgp2, nsfgp3, nsfcur,
# nstcur, nflges, nflgp3
# Specify option "-v valid" for the valid date wanted (default $PDY$cyc).
# Currently, the valid hours specified must be a multiple of 3.
# Either 2-digit or 4-digit years are currently allowed.
# Specify positional argument to be the file to which to copy the guess.
# If missing, the NAME of the guess file is written to standard output.
# A nonzero return code from this script means either the arguments are invalid
# or the guess could not be found; a message is written to standard error in
# this case, but neither a file copy nor a standard output write will be done.
# The file returned is guaranteed to exist and be readable.
# The script uses the utility command NHOUR.
#
# Example 1. Copy the production sigma guess for 1998100100 to the file sges.
#  getges.sh -e prod -t sigges -v 1998100100 sges
#
# Example 2. Assign the pressure grib guess for the date 1998100121.
#  export XLFUNIT_12="$(getges.sh -qt pgbges||echo /dev/null)"
#
# Example 3. Get the PRX pgb analysis or the best valid guess at 1998100112.
#  getges -e prx -t pgbcur -v 1998100112 pgbfile
#
# Example 5. Get the 24-hour GFS forecast sigma file valid at 1998100112.
#  getges -t sigcur -v 1998100112 -f 24 -e gfs sigfile
#
# History: 1996 December    Iredell       Initial implementation
#          1997 March       Iredell       Nine new filetypes
#          1997 April       Iredell       Two new filetypes and -f option
#          1997 December    Iredell       Four new filetypes
#          1998 April       Iredell       4-digit year allowed;
#                                         sigges internal date no longer checked
#          1998 May         Iredell       T170L42 defaulted; four new filetypes
#                                         and two filetypes deleted
#          1998 June        Rogers        Nam types added
#          1998 September   Iredell       high is default resolution
#          2000 March       Iredell       Cdas and -n option
#          2000 June        Iredell       Eight new filetypes
#          2002 April       Treadon       T254L64 defaulted; add angle dependent
#                                         bias correction file
#          2003 March       Iredell       GFS network out to 384 hours
#          2003 August      Iredell       Hourly global guesses
#          2005 September   Treadon       Add satellite data count file (satcnt)
#          2006 September   Gayno         Add high-res snow analysis
#          2009 January     Rogers        Added sfluxgrb file
#          2011 April       Rogers        Added GFS pg2ges file
#          2016 May         Menlove       Changed GETGES_COM variable to $COMINmodel
#          2016 November    Iredell       Adapted getges for NEMS GSM
#                                         Also removed a lot of dead wood
#
################################################################################
#-------------------------------------------------------------------------------

# Set some default parameters.
fhbeg=03                         # hour to begin searching backward for guess
fhinc=03                         # hour to increment backward in search
fhend=384                        # hour to end searching backward for guess

#-------------------------------------------------------------------------------
# Get options and arguments.
netwk=global                     # default network
envir=prod                       # default environment
fhour=any                        # default forecast hour
quiet=YES                        # default quiet mode
resol=high                       # default resolution
typef=sigges                     # default filetype
valid=${PDY}${cyc}               # default valid date
err=0

while getopts n:e:f:qr:t:v: opt;do
 case $opt in
  n) netwk="$OPTARG";;
  e) envir="$OPTARG";;
  f) fhour="$OPTARG";;
  q) quiet=NO;;
  r) resol="$OPTARG";;
  t) typef="$OPTARG";;
  v) valid="$OPTARG";;
  \?) err=1;;
 esac
done
shift $(($OPTIND-1))
gfile=$1
if [[ -z $valid ]];then
 echo "$0: either -v option or environment variables PDY and cyc must be set" >&2
elif [[ $# -gt 1 ]];then
 echo "$0: too many positional arguments" >&2
elif [[ $err -ne 0 ]];then
 echo "$0: invalid option" >&2
fi
if [[ $gfile = '?' || $# -gt 1 || $err -ne 0 || -z $valid ||\
      $netwk = '?' || $envir = '?' || $fhour = '?' || $resol = '?' ||\
      $typef = '?' || $valid = '?' ]];then
 echo "Usage: getges.sh [-n network] [-e environment] [-f fhour] [-q] [-r resolution]" >&2
 echo "                 [-t filetype] [-v valid] [gfile]" >&2
 if [[ $netwk = '?' ]];then
  echo "         network choices:" >&2
  echo "           global (default), namopl, gdas, gfs, cdas, etc." >&2
 elif [[ $envir = '?' ]];then
  echo "         environment choices:" >&2
  echo "           prod (default), test, para, dump, prx" >&2
  echo "           (some network values allowed for compatibility)" >&2
 elif [[ $fhour = '?' ]];then
  echo "         fhour is optional specific forecast hour" >&2
 elif [[ $resol = '?' ]];then
  echo "         resolution choices:" >&2
  echo "           high (default), 25464, 17042, 12628, low, 6228, namopl, any" >&2
 elif [[ $typef = '?' ]];then
  echo "         filetype choices:" >&2
  echo "           sigges (default), siggm3, siggm2, siggm1, siggp1, siggp2, siggp3," >&2
  echo "           sfcges, sfcgm3, sfcgm2, sfcgm1, sfcgp1, sfcgp2, sfcgp3," >&2
  echo "           sfgges, sfggp3, biascr, satang, satcnt, gesfil" >&2
  echo "           pgbges, pgiges, pgbgm6, pgigm6, pgbgm3, pgigm3, pgbgp3, pgigp3," >&2
  echo "           sigcur, sfccur, pgbcur, pgicur, prepqc, tcvg12, tcvges, tcvitl," >&2
  echo "           enggrb, enggri, icegrb, icegri, snogrb, snogri, sstgrb, sstgri," >&2
  echo "           pg2cur, pg2ges, restrt," >&2
  echo "           natges, natgm3, natgm2, natgm1, natgp1, natgp2, natgp3, natcur," >&2
  echo "           nsfges, nsfgm3, nsfgm2, nsfgm1, nsfgp1, nsfgp2, nsfgp3, nsfcur," >&2
  echo "           nstcur, nflges, nflgp3," >&2
 elif [[ $valid = '?' ]];then
  echo "         valid is the valid date in yyyymmddhh or yymmddhh form" >&2
  echo "         (default is environmental variable $PDY$cyc)" >&2
 elif [[ $gfile = '?' ]];then
  echo "         gfile is the guess file to write" >&2
  echo "         (default is to write the guess file name to stdout)" >&2
 else
  echo "         (Note: set a given option to '?' for more details)" >&2
 fi
 exit 1
fi

if [[ $envir != prod && $envir != test && $envir != para && $envir != dump && $envir != pr? && $envir != dev ]];then
 netwk=$envir
 envir=prod
 echo '************************************************************' >&2
 echo '* WARNING: Using "-e" is deprecated in this case.          *' >&2
 echo '*          Please use "-n" instead.                        *' >&2
 echo '************************************************************' >&2
fi
if [[ "$netwk" = "namopl" || "$resol" = "namopl" ]];then
  netwk=namopl
  typef=restrt
  resol=namopl
fi
if [[ "${resol}" == "57464" || "${resol}" == "38264" || "${resol}" == "19064" || "${resol}" == "25464" || "${resol}" == "17042" || "${resol}" == "12628" ]]; then
    resol=high
fi
if [[ "${resol}" == "6228" ]]; then
    resol=low
fi
resolsuf=""
if [[ ${resol} == *deg ]]; then
    resolsuf=.$resol
fi
fhbeg=$(${NHOUR:?} $valid)
if [[ ${fhbeg} -le 0 ]]; then
    fhbeg=03
fi
((fhbeg=(10#${fhbeg}-1)/3*3+3))
if [[ $fhbeg -lt 10 ]]; then
    fhbeg="0${fhbeg}"
fi
if [[ $typef = enggrb ]];then
 typef=icegrb
 echo '************************************************************' >&2
 echo '* WARNING: Using "-t enggrb" is now deprecated.            *' >&2
 echo '*          Please use "-t icegrb".                         *' >&2
 echo '************************************************************' >&2
elif [[ $typef = enggri ]];then
 typef=icegri
 echo '************************************************************' >&2
 echo '* WARNING: Using "-t enggri" is now deprecated.            *' >&2
 echo '*          Please use "-t icegri".                         *' >&2
 echo '************************************************************' >&2
fi

#-------------------------------------------------------------------------------
# Assemble guess list in descending order from the best guess.
geslist=""
getlist00=""


# Check validity of options.
if [[ $fhour != any ]];then
  fhbeg=$fhour
  fhend=$fhour
fi
if [[ $valid -lt 20000000 ]];then
 valid=20$valid
 echo '************************************************************' >&2
 echo '* WARNING: A 2-digit year was converted to a 4-digit year. *' >&2
 echo '*          Please use full a 4-digit year in this utility. *' >&2
 echo '************************************************************' >&2
fi
if [[ -z "$geslist" ]];then
 echo getges.sh: filetype $typef or resolution $resol not recognized >&2
 exit 2
fi

#-------------------------------------------------------------------------------
# Loop until guess is found.
fh=$fhbeg
if [ -z "$PDY" ];then echo "getges.sh WARNING: \$PDY variable not set" >&2; fi
while [[ $fh -le $fhend ]];do
 ((fhm6=10#${fh}-6))
 if [[ ${fhm6} -lt 10 && ${fhm6} -ge 0 ]]; then
     fhm6=0${fhm6}
 fi
 ((fhm5=10#${fh}-5))
 if [[ ${fhm5} -lt 10 && ${fhm5} -ge 0 ]]; then
     fhm5=0${fhm5}
 fi
 ((fhm4=10#${fh}-4))
 if [[ ${fhm4} -lt 10 && ${fhm4} -ge 0 ]]; then
     fhm4=0${fhm4}
 fi
 ((fhm3=10#${fh}-3))
 if [[ ${fhm3} -lt 10 && ${fhm3} -ge 0 ]]; then
     fhm3=0${fhm3}
 fi
 ((fhm2=10#${fh}-2))
 if [[ ${fhm2} -lt 10 && ${fhm2} -ge 0 ]]; then
     fhm2=0${fhm2}
 fi
 ((fhm1=10#${fh}-1))
 if [[ ${fhm1} -lt 10 && ${fhm1} -ge 0 ]]; then
     fhm1=0${fhm1}
 fi
 ((fhp1=10#${fh}+1))
 if [[ ${fhp1} -lt 10 ]]; then
     fhp1=0${fhp1}
 fi
 ((fhp2=10#${fh}+2))
 if [[ ${fhp2} -lt 10 ]]; then
     fhp2=0${fhp2}
 fi
 ((fhp3=10#${fh}+3))
 if [[ ${fhp3} -lt 10 ]]; then
     fhp3=0${fhp3}
 fi
 gh=$fh;[[ $gh -lt 100 ]]&&gh=0$gh
 ghm6=$fhm6;[[ $ghm6 -lt 100 ]]&&ghm6=0$ghm6
 ghm5=$fhm5;[[ $ghm5 -lt 100 ]]&&ghm5=0$ghm5
 ghm4=$fhm4;[[ $ghm4 -lt 100 ]]&&ghm4=0$ghm4
 ghm3=$fhm3;[[ $ghm3 -lt 100 ]]&&ghm3=0$ghm3
 ghm2=$fhm2;[[ $ghm2 -lt 100 ]]&&ghm2=0$ghm2
 ghm1=$fhm1;[[ $ghm1 -lt 100 ]]&&ghm1=0$ghm1
 ghp1=$fhp1;[[ $ghp1 -lt 100 ]]&&ghp1=0$ghp1
 ghp2=$fhp2;[[ $ghp2 -lt 100 ]]&&ghp2=0$ghp2
 ghp3=$fhp3;[[ $ghp3 -lt 100 ]]&&ghp3=0$ghp3
 id=$(date --utc +%Y%m%d%H -d "${valid:0:8} ${valid:8:2} - ${fh} hours")

 day=$(echo $id | xargs | cut -c8)
 cyc=$(echo $id | xargs | rev | cut -c1-2 | rev)
 eval list=\$getlist$fh
 if [[ -z "${list}" ]]; then
     list=${geslist}
 fi
 for ges_var in $list;do
  # Replace variables in guess with their values
  eval ges_val=$ges_var
  # Replace the current PDY with the valid date
  ges=${ges_val/$PDY\//$day/}
  if [[ "${quiet}" == "NO" ]]; then
      echo Checking: "${ges}" >&2
  fi
  if [[ -r "${ges}" ]]; then
      break 2
  fi
 done
 fh=$((10#${fh}+10#${fhinc}))
 if [[ ${fh} -lt 10 ]]; then
     fh=0${fh}
 fi
done
if [[ $fh -gt $fhend ]];then
 echo getges.sh: unable to find $netwk.$envir.$typef.$resol.$valid >&2
 exit 8
fi

#-------------------------------------------------------------------------------
# Either copy guess to a file or write guess name to standard output.
if [[ -z "$gfile" ]];then
 echo ${ges}
 err=$?
else
 cp ${ges} ${gfile}
 err=$?
fi

exit ${err}
