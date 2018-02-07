#!/bin/ksh
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
# Specify option "-v valid" for the valid date wanted (default $CDATE).
# Currently, the valid hours specified must be a multiple of 3.
# Either 2-digit or 4-digit years are currently allowed.
# Specify positional argument to be the file to which to copy the guess.
# If missing, the NAME of the guess file is written to standard output.
# A nonzero return code from this script means either the arguments are invalid
# or the guess could not be found; a message is written to standard error in
# this case, but neither a file copy nor a standard output write will be done.
# The file returned is guaranteed to exist and be readable.
# The script uses the utility commands NDATE and NHOUR.
#
# Example 1. Copy the production sigma guess for 1998100100 to the file sges.
#  getges.sh -e prod -t sigges -v 1998100100 sges 
#
# Example 2. Assign the pressure grib guess for the date 1998100121.
#  export CDATE=1998100121
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
valid=${CDATE:-'?'}              # default valid date
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
 echo "$0: either -v option or environment variable CDATE must be set" >&2
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
  echo "         (default is environmental variable CDATE)" >&2
 elif [[ $gfile = '?' ]];then
  echo "         gfile is the guess file to write" >&2
  echo "         (default is to write the guess file name to stdout)" >&2
 else
  echo "         (Note: set a given option to '?' for more details)" >&2 
 fi
 exit 1
fi
[[ $quiet = NO ]]&&set -x
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
[[ $resol = 57464 || $resol = 38264 || $resol = 19064 || $resol = 25464 || $resol = 17042 || $resol = 12628 ]]&&resol=high
[[ $resol = 6228 ]]&&resol=low
resolsuf=""
[[ $resol == *deg ]]&&resolsuf=.$resol
fhbeg=$(${NHOUR:?} $valid)
[[ $fhbeg -le 0 ]]&&fhbeg=03
((fhbeg=(10#$fhbeg-1)/3*3+3))
[[ $fhbeg -lt 10 ]]&&fhbeg=0$fhbeg
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

# GDAS
if [[ "$netwk" = "gdas" ]];then
 if [ -z "$COMINgdas" ]; then
   echo "getges.sh ERROR: The \$COMINgdas variable must be defined." >&2
   exit 1
 fi
 fhend=12
 case $typef in
  biascr) geslist='
   $COMINgdas/gdas.t${cyc}z.abias'
   ;;
  biascr_pc) geslist='
   $COMINgdas/gdas.t${cyc}z.abias_pc'
   ;;
  biascr_air) geslist='
   $COMINgdas/gdas.t${cyc}z.abias_air'
   ;;
  radstat) geslist='
   $COMINgdas/gdas.t${cyc}z.radstat'
   ;;
  pgbges) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrbh$fh 
   $COMINgdas/gdas.t${cyc}z.pgrbf$fh'
   ;;
  pg2ges) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$gh'
   ;;
  pgbgm6) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhm6 
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhm6'
   ;;
  pgbgm3) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhm3 
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhm3'
   ;;
  pgbgp3) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhp3 
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhp3'
   ;;
  pgbcur) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrbh$fh 
   $COMINgdas/gdas.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pg2cur) geslist='
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$gh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $COMINgdas/gdas.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvg12) geslist='
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=12
   fhend=12
   ;;
  tcvges) geslist='
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=06
   fhend=06
   ;;
  tcvitl) geslist='
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $COMINgdas/gdas.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $COMINgdas/gdas.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_574) geslist='
   $COMINgdas/gdas.t${cyc}z.snogrb_t574.1152.576'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_1534) geslist='
   $COMINgdas/gdas.t${cyc}z.snogrb_t1534.3072.1536'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $COMINgdas/gdas.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  natges) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$gh.nemsio'
   ;;
  natgm3) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghm3.nemsio'
   ;;
  natgm2) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghm2.nemsio'
   ;;
  natgm1) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghm1.nemsio'
   ;;
  natgp1) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghp1.nemsio'
   ;;
  natgp2) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghp2.nemsio'
   ;;
  natgp3) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$ghp3.nemsio'
   ;;
  natcur) geslist='
   $COMINgdas/gdas.t${cyc}z.atmf$gh.nemsio'
   getlist00='
   $COMINgdas/gdas.t${cyc}z.atmanl.nemsio'
   fhbeg=00
   ;;
  nsfges) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$gh.nemsio'
   ;;
  nsfgm3) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm3.nemsio'
   ;;
  nsfgm2) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm2.nemsio'
   ;;
  nsfgm1) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm1.nemsio'
   ;;
  nsfgp1) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp1.nemsio'
   ;;
  nsfgp2) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp2.nemsio'
   ;;
  nsfgp3) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp3.nemsio'
   ;;
  nsfcur) geslist='
   $COMINgdas/gdas.t${cyc}z.sfcf$gh.nemsio'
   getlist00='
   $COMINgdas/gdas.t${cyc}z.sfcanl.nemsio'
   fhbeg=00
   ;;
  nstcur) geslist='
   $COMINgdas/gdas.t${cyc}z.nstf$gh.nemsio'
   getlist00='
   $COMINgdas/gdas.t${cyc}z.nstanl.nemsio'
   fhbeg=00
   ;;
  nflges) geslist='
   $COMINgdas/gdas.t${cyc}z.flxf$gh.nemsio'
   ;;
  nflgp3)  geslist='
   $COMINgdas/gdas.t${cyc}z.flxf$ghp3.nemsio'
   ;;
  nflcur) geslist='
   $COMINgdas/gdas.t${cyc}z.flxf$gh.nemsio'
   fhbeg=00
   ;;
 esac

# CFS-CDAS
elif [[ "$netwk" = "cfs-cdas" ]];then
 if [ -z "$COMINcfs_cdas" ]; then
   echo "getges.sh ERROR: The \$COMINcfs_cdas variable must be defined." >&2
   exit 1
 fi
 fhend=12
 case $typef in
  sigges)  geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fh'
   ;;
  siggm3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhm3'
   ;;
  siggm2) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhm2'
   ;;
  siggm1) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhm1'
   ;;
  siggp1) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhp1'
   ;;
  siggp2) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhp2'
   ;;
  siggp3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fhp3'
   ;;
  sfcges) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fh}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fh'
   ;;
  sfcgm3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhm3}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhm3'
   ;;
  sfcgm2) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhm2}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhm2'
   ;;
  sfcgm1) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhm1}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhm1'
   ;;
  sfcgp1) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhp1}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhp1'
   ;;
  sfcgp2) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhp2}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhp2'
   ;;
  sfcgp3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf${fhp3}.LIS
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fhp3'
   ;;
  biascr) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.abias'
   ;;
  satang) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.satang'
   ;;
  satcnt) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.satcnt'
   ;;
  gesfil) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.gesfile'
   fhbeg=00
   fhend=00
   ;;
  sfgges)  geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sfluxgrbf$fh'
   ;;
  sfggp3)  geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sfluxgrbf$fhp3'
   ;;
  pgbges) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbh$fh 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbf$fh'
   ;;
  pgiges) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbih$fh 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbif$fh'
   ;;
  pgbgm6) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbh$fhm6 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbf$fhm6'
   ;;
  pgigm6) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbih$fhm6 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbif$fhm6'
   ;;
  pgbgm3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbh$fhm3 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbf$fhm3'
   ;;
  pgigm3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbih$fhm3 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbif$fhm3'
   ;;
  pgbgp3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbh$fhp3 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbf$fhp3'
   ;;
  pgigp3) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbih$fhp3 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbif$fhp3'
   ;;
  sigcur) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sf$fh'
   getlist00='
   $COMINcfs_cdas/cdas1.t${cyc}z.sanl'
   fhbeg=00
   ;;
  sfccur) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.bf$fh'
   getlist00='
   $COMINcfs_cdas/cdas1.t${cyc}z.sfcanl'
   fhbeg=00
   ;;
  pgbcur) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbh$fh 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pgicur) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbih$fh 
   $COMINcfs_cdas/cdas1.t${cyc}z.pgrbif$fh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvg12) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=12
   fhend=12
   ;;
  tcvges) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=06
   fhend=06
   ;;
  tcvitl) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  icegri) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.engicegrb.index'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_high) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb_t574
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb_t382'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_382) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb_t382'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_574) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb_t574'
   fhbeg=00
   fhinc=06
   ;;
  snogri) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.snogrb.index'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  sstgri) geslist='
   $COMINcfs_cdas/cdas1.t${cyc}z.sstgrb.index'
   fhbeg=00
   fhinc=06
   ;;
 esac

# GFS
elif [[ "$netwk" = "gfs" ]];then
 if [ -z "$COMINgfs" ]; then
   echo "getges.sh ERROR: The \$COMINgfs variable must be defined." >&2
   exit 1
 fi
 fhend=384
 case $typef in
  pgbcur) geslist='
   $COMINgfs/gfs.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pg2cur) geslist='
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$gh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $COMINgfs/gfs.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvitl) geslist='
   $COMINgfs/gfs.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $COMINgfs/gfs.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $COMINgfs/gfs.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_1534) geslist='
   $COMINgfs/gfs.t${cyc}z.snogrb_t1534.3072.1536'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $COMINgfs/gfs.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  natcur) geslist='
   $COMINgfs/gfs.t${cyc}z.atmf$gh.nemsio'
   getlist00='
   $COMINgfs/gfs.t${cyc}z.atmanl.nemsio'
   fhbeg=00
   ;;
  nsfcur) geslist='
   $COMINgfs/gfs.t${cyc}z.sfcf$gh.nemsio'
   getlist00='
   $COMINgfs/gfs.t${cyc}z.sfcanl.nemsio'
   fhbeg=00
   ;;
  nstcur) geslist='
   $COMINgfs/gfs.t${cyc}z.nstf$gh.nemsio'
   getlist00='
   $COMINgfs/gfs.t${cyc}z.nstanl.nemsio'
   fhbeg=00
   ;;
  nflcur) geslist='
   $COMINgfs/gfs.t${cyc}z.flxf$gh.nemsio'
   fhbeg=00
   ;;
 esac

# CDAS
elif [[ "$netwk" = "cdas" ]];then
 if [ -z "$COMINcdas" ]; then
   echo "getges.sh ERROR: The \$COMINcdas variable must be defined." >&2
   exit 1
 fi
 fhbeg=06
 fhend=06
 case $typef in
  sigges)  geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fh'
   ;;
  siggm3) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhm3'
   ;;
  siggm2) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhm2'
   ;;
  siggm1) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhm1'
   ;;
  siggp1) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhp1'
   ;;
  siggp2) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhp2'
   ;;
  siggp3) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fhp3'
   ;;
  sfcges) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fh'
   ;;
  sfcgm3) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhm3'
   ;;
  sfcgm2) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhm2'
   ;;
  sfcgm1) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhm1'
   ;;
  sfcgp1) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhp1'
   ;;
  sfcgp2) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhp2'
   ;;
  sfcgp3) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fhp3'
   ;;
  biascr) geslist='
   $COMINcdas/cdas.t${cyc}z.abias'
   ;;
  satang) geslist='
   $COMINcdas/cdas.t${cyc}z.satang'
   ;;
  satcnt) geslist='
   $COMINcdas/cdas.t${cyc}z.satcnt'
   ;;
  gesfil) geslist='
   $COMINcdas/cdas.t${cyc}z.gesfile'
   fhbeg=00
   fhend=00
   ;;
  pgbges) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbf$fh'
   ;;
  pgiges) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbif$fh'
   ;;
  pgbgm6) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbf$fhm6'
   ;;
  pgigm6) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbif$fhm6'
   ;;
  pgbgm3) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbf$fhm3'
   ;;
  pgigm3) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbif$fhm3'
   ;;
  pgbgp3) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbf$fhp3'
   ;;
  pgigp3) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbif$fhp3'
   ;;
  sigcur) geslist='
   $COMINcdas/cdas.t${cyc}z.sf$fh'
   getlist00='
   $COMINcdas/cdas.t${cyc}z.sanl'
   fhbeg=00
   ;;
  sfccur) geslist='
   $COMINcdas/cdas.t${cyc}z.bf$fh'
   getlist00='
   $COMINcdas/cdas.t${cyc}z.sfcanl'
   fhbeg=00
   ;;
  pgbcur) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pgicur) geslist='
   $COMINcdas/cdas.t${cyc}z.pgrbif$fh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $COMINcdas/cdas.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvg12) geslist='
   $COMINcdas/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=12
   fhend=12
   ;;
  tcvges) geslist='
   $COMINcdas/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=06
   fhend=06
   ;;
  tcvitl) geslist='
   $COMINcdas/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $COMINcdas/cdas.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  icegri) geslist='
   $COMINcdas/cdas.t${cyc}z.engicegrb.index'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $COMINcdas/cdas.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogri) geslist='
   $COMINcdas/cdas.t${cyc}z.snogrb.index'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $COMINcdas/cdas.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  sstgri) geslist='
   $COMINcdas/cdas.t${cyc}z.sstgrb.index'
   fhbeg=00
   fhinc=06
   ;;
 esac

# CDC CDAS
elif [[ "$netwk" = "cdc" ]];then
 if [ -z "$COMINcdc" ]; then
   echo "getges.sh ERROR: The \$COMINcdc variable must be defined." >&2
   exit 1
 fi
 fhbeg=06
 fhend=06
 case $typef in
  sigges)  geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fh'
   ;;
  siggm3) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhm3'
   ;;
  siggm2) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhm2'
   ;;
  siggm1) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhm1'
   ;;
  siggp1) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhp1'
   ;;
  siggp2) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhp2'
   ;;
  siggp3) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fhp3'
   ;;
  sfcges) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fh'
   ;;
  sfcgm3) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhm3'
   ;;
  sfcgm2) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhm2'
   ;;
  sfcgm1) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhm1'
   ;;
  sfcgp1) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhp1'
   ;;
  sfcgp2) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhp2'
   ;;
  sfcgp3) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fhp3'
   ;;
  biascr) geslist='
   $COMINcdc/cdas.t${cyc}z.abias'
   ;;
  satang) geslist='
   $COMINcdc/cdas.t${cyc}z.satang'
   ;;
  satcnt) geslist='
   $COMINcdc/cdas.t${cyc}z.satcnt'
   ;;
  gesfil) geslist='
   $COMINcdc/cdas.t${cyc}z.gesfile'
   fhbeg=00
   fhend=00
   ;;
  pgbges) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbf$fh'
   ;;
  pgiges) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbif$fh'
   ;;
  pgbgm6) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbf$fhm6'
   ;;
  pgigm6) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbif$fhm6'
   ;;
  pgbgm3) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbf$fhm3'
   ;;
  pgigm3) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbif$fhm3'
   ;;
  pgbgp3) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbf$fhp3'
   ;;
  pgigp3) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbif$fhp3'
   ;;
  sigcur) geslist='
   $COMINcdc/cdas.t${cyc}z.sf$fh'
   getlist00='
   $COMINcdc/cdas.t${cyc}z.sanl'
   fhbeg=00
   ;;
  sfccur) geslist='
   $COMINcdc/cdas.t${cyc}z.bf$fh'
   getlist00='
   $COMINcdc/cdas.t${cyc}z.sfcanl'
   fhbeg=00
   ;;
  pgbcur) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pgicur) geslist='
   $COMINcdc/cdas.t${cyc}z.pgrbif$fh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $COMINcdc/cdas.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvg12) geslist='
   $COMINcdc/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=12
   fhend=12
   ;;
  tcvges) geslist='
   $COMINcdc/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=06
   fhend=06
   ;;
  tcvitl) geslist='
   $COMINcdc/cdas.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $COMINcdc/cdas.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  icegri) geslist='
   $COMINcdc/cdas.t${cyc}z.engicegrb.index'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $COMINcdc/cdas.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogri) geslist='
   $COMINcdc/cdas.t${cyc}z.snogrb.index'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $COMINcdc/cdas.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  sstgri) geslist='
   $COMINcdc/cdas.t${cyc}z.sstgrb.index'
   fhbeg=00
   fhinc=06
   ;;
 esac

# Any resolution production
elif [[ "$netwk" = "global" ]];then
 if [ -z "$COMINgdas" ]; then
   echo "getges.sh ERROR: The \$COMINgdas variable must be defined." >&2
   exit 1
 fi
 if [ -z "$COMINgfs" ]; then
   echo "getges.sh ERROR: The \$COMINgfs variable must be defined." >&2
   exit 1
 fi
 GETGES_NWG=${GETGES_NWG:-${GESROOT:?}}
 case $typef in
  biascr) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.abias
   $COMINgdas/gdas.t${cyc}z.abias
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.abias
   $COMINgfs/gfs.t${cyc}z.abias'
   fhbeg=06
   fhinc=06
   ;;
  pgbges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbh$fh
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbf$fh
   $COMINgdas/gdas.t${cyc}z.pgrbh$fh
   $COMINgdas/gdas.t${cyc}z.pgrbf$fh
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrbf$fh
   $COMINgfs/gfs.t${cyc}z.pgrbf$fh'
   ;;
  pgbgm6) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbh$fhm6
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbf$fhm6
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhm6
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhm6
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrbf$fhm6
   $COMINgfs/gfs.t${cyc}z.pgrbf$fhm6'
   ;;
  pgbgm3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbh$fhm3
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbf$fhm3
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhm3
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhm3
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrbf$fhm3
   $COMINgfs/gfs.t${cyc}z.pgrbf$fhm3'
   ;;
  pgbgp3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbh$fhp3
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbf$fhp3
   $COMINgdas/gdas.t${cyc}z.pgrbh$fhp3
   $COMINgdas/gdas.t${cyc}z.pgrbf$fhp3
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrbf$fhp3
   $COMINgfs/gfs.t${cyc}z.pgrbf$fhp3'
   ;;
  pg2ges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$gh
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$gh
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$gh
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$gh'
   ;;
  pg2gm6) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm6
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm6
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm6
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm6'
   ;;
  pg2gm5) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm5
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm5
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm5
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm5'
   ;;
  pg2gm4) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm4
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm4
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm4
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm4'
   ;;
  pg2gm3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm3
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm3
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm3
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm3'
   ;;
  pg2gm2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm2
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm2
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm2
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm2'
   ;;
  pg2gm1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghm1
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghm1
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghm1
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghm1'
   ;;
  pg2gp1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghp1
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghp1
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghp1
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghp1'
   ;;
  pg2gp2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghp2
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghp2
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghp2
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghp2'
   ;;
  pg2gp3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p25.f$ghp3
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p25.f$ghp3
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p25.f$ghp3
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p25.f$ghp3'
   ;;
  pgbcur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbh$fh
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrbf$fh
   $COMINgdas/gdas.t${cyc}z.pgrbh$fh
   $COMINgdas/gdas.t${cyc}z.pgrbf$fh
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrbf$fh
   $COMINgfs/gfs.t${cyc}z.pgrbf$fh'
   fhbeg=00
   ;;
  pg2cur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.pgrb2.0p50.f$gh
   $COMINgdas/gdas.t${cyc}z.pgrb2.0p50.f$gh
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.pgrb2.0p50.f$gh
   $COMINgfs/gfs.t${cyc}z.pgrb2.0p50.f$gh'
   fhbeg=00
   ;;
  prepqc) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.prepbufr
   $COMINgdas/gdas.t${cyc}z.prepbufr
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.prepbufr
   $COMINgfs/gfs.t${cyc}z.prepbufr'
   fhbeg=00
   fhend=00
   ;;
  tcvg12) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.syndata.tcvitals.tm00
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.syndata.tcvitals.tm00
   $COMINgfs/gfs.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=12
   fhend=12
   ;;
  tcvges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.syndata.tcvitals.tm00
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.syndata.tcvitals.tm00
   $COMINgfs/gfs.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=06
   fhend=06
   ;;
  tcvitl) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.syndata.tcvitals.tm00
   $COMINgdas/gdas.t${cyc}z.syndata.tcvitals.tm00
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.syndata.tcvitals.tm00
   $COMINgfs/gfs.t${cyc}z.syndata.tcvitals.tm00'
   fhbeg=00
   fhend=00
   ;;
  icegrb) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.engicegrb
   $COMINgdas/gdas.t${cyc}z.engicegrb
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.engicegrb
   $COMINgfs/gfs.t${cyc}z.engicegrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.snogrb
   $COMINgdas/gdas.t${cyc}z.snogrb
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.snogrb
   $COMINgfs/gfs.t${cyc}z.snogrb'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_574) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.snogrb_t574.1152.576
   $COMINgdas/gdas.t${cyc}z.snogrb_t574.1152.576
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.snogrb_t574.1152.576
   $COMINgfs/gfs.t${cyc}z.snogrb_t574.1152.576'
   fhbeg=00
   fhinc=06
   ;;
  snogrb_1534) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.snogrb_t1534.3072.1536
   $COMINgdas/gdas.t${cyc}z.snogrb_t1534.3072.1536
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.snogrb_t1534.3072.1536
   $COMINgfs/gfs.t${cyc}z.snogrb_t1534.3072.1536'
   fhbeg=00
   fhinc=06
   ;;
  sstgrb) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sstgrb
   $COMINgdas/gdas.t${cyc}z.sstgrb
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sstgrb
   $COMINgfs/gfs.t${cyc}z.sstgrb'
   fhbeg=00
   fhinc=06
   ;;
  natges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$gh.nemsio'
   ;;
  natgm3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghm3.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghm3.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghm3.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghm3.nemsio'
   ;;
  natgm2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghm2.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghm2.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghm2.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghm2.nemsio'
   ;;
  natgm1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghm1.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghm1.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghm1.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghm1.nemsio'
   ;;
  natgp1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghp1.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghp1.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghp1.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghp1.nemsio'
   ;;
  natgp2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghp2.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghp2.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghp2.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghp2.nemsio'
   ;;
  natgp3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$ghp3.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$ghp3.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$ghp3.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$ghp3.nemsio'
   ;;
  natcur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.atmf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.atmf$gh.nemsio'
   getlist00='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.atmanl.nemsio
   $COMINgdas/gdas.t${cyc}z.atmanl.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.atmanl.nemsio
   $COMINgfs/gfs.t${cyc}z.atmanl.nemsio'
   fhbeg=00
   ;;
  nsfges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$gh.nemsio'
   ;;
  nsfgm3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghm3.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm3.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghm3.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghm3.nemsio'
   ;;
  nsfgm2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghm2.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm2.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghm2.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghm2.nemsio'
   ;;
  nsfgm1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghm1.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghm1.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghm1.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghm1.nemsio'
   ;;
  nsfgp1) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghp1.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp1.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghp1.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghp1.nemsio'
   ;;
  nsfgp2) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghp2.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp2.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghp2.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghp2.nemsio'
   ;;
  nsfgp3) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$ghp3.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$ghp3.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$ghp3.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$ghp3.nemsio'
   ;;
  nsfcur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcf$gh.nemsio'
   getlist00='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.sfcanl.nemsio
   $COMINgdas/gdas.t${cyc}z.sfcanl.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.sfcanl.nemsio
   $COMINgfs/gfs.t${cyc}z.sfcanl.nemsio'
   fhbeg=00
   ;;
  nstcur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.nstf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.nstf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.nstf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.nstf$gh.nemsio'
   getlist00='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.nstanl.nemsio
   $COMINgdas/gdas.t${cyc}z.nstanl.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.nstanl.nemsio
   $COMINgfs/gfs.t${cyc}z.nstanl.nemsio'
   fhbeg=00
   ;;
  nflges) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.flxf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.flxf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.flxf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.flxf$gh.nemsio'
   ;;
  nflgp3)  geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.flxf$ghp3.nemsio
   $COMINgdas/gdas.t${cyc}z.flxf$ghp3.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.flxf$ghp3.nemsio
   $COMINgfs/gfs.t${cyc}z.flxf$ghp3.nemsio'
   ;;
  nflcur) geslist='
   $GETGES_NWG/$envir/gdas.$day/gdas.t${cyc}z.flxf$gh.nemsio
   $COMINgdas/gdas.t${cyc}z.flxf$gh.nemsio
   $GETGES_NWG/$envir/gfs.$day/gfs.t${cyc}z.flxf$gh.nemsio
   $COMINgfs/gfs.t${cyc}z.flxf$gh.nemsio'
   fhbeg=00
   ;;
 esac
fi

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
if [[ $($NDATE 0 $valid 2>/dev/null) != $valid ]];then
 echo getges.sh: invalid date $valid >&2
 exit 2
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
 ((fhm6=10#$fh-6))
 [[ $fhm6 -lt 10 && $fhm6 -ge 0 ]]&&fhm6=0$fhm6
 ((fhm5=10#$fh-5))
 [[ $fhm5 -lt 10 && $fhm5 -ge 0 ]]&&fhm5=0$fhm5
 ((fhm4=10#$fh-4))
 [[ $fhm4 -lt 10 && $fhm4 -ge 0 ]]&&fhm4=0$fhm4
 ((fhm3=10#$fh-3))
 [[ $fhm3 -lt 10 && $fhm3 -ge 0 ]]&&fhm3=0$fhm3
 ((fhm2=10#$fh-2))
 [[ $fhm2 -lt 10 && $fhm2 -ge 0 ]]&&fhm2=0$fhm2
 ((fhm1=10#$fh-1))
 [[ $fhm1 -lt 10 && $fhm1 -ge 0 ]]&&fhm1=0$fhm1
 ((fhp1=10#$fh+1))
 [[ $fhp1 -lt 10 ]]&&fhp1=0$fhp1
 ((fhp2=10#$fh+2))
 [[ $fhp2 -lt 10 ]]&&fhp2=0$fhp2
 ((fhp3=10#$fh+3))
 [[ $fhp3 -lt 10 ]]&&fhp3=0$fhp3
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
 id=$($NDATE -$fh $valid)
 typeset -L8 day=$id
 typeset -R2 cyc=$id
 eval list=\$getlist$fh
 [[ -z "$list" ]]&&list=${geslist}
 for ges_var in $list;do
  # Replace variables in guess with their values
  eval ges_val=$ges_var
  # Replace the current PDY with the valid date
  ges=${ges_val/$PDY\//$day/}
  [[ $quiet = NO ]]&&echo Checking: $ges >&2
  [[ -r $ges ]]&&break 2
 done
 fh=$((10#$fh+10#$fhinc))
 [[ $fh -lt 10 ]]&&fh=0$fh
done
if [[ $fh -gt $fhend ]];then
 echo getges.sh: unable to find $netwk.$envir.$typef.$resol.$valid >&2
 exit 8
fi

#-------------------------------------------------------------------------------
# Either copy guess to a file or write guess name to standard output.
if [[ -z "$gfile" ]];then
 echo $ges
 exit $?
else
 cp $ges $gfile
 exit $?
fi
