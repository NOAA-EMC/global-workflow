
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_nceppost.sh           
# Script description:  Posts the global pressure GRIB file
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-05-01
#
# Abstract: This script reads a single global GFS IO file and (optionally)
#   a global flux file and creates a global pressure GRIB file.
#   The resolution and generating code of the output GRIB file can also
#   be set in the argument list.
#
# Script history log:
# 1999-05-01  Mark Iredell
# 2007-04-04  Huiya Chuang: Modify the script to run unified post
# 2012-06-04  Jun Wang: add grib2 option
# 2015-03-20  Lin Gan: add Perl for Post XML performance upgrade
# 2016-02-08  Lin Gan: Modify to use Vertical Structure
#
# Usage:  global_postgp.sh SIGINP FLXINP FLXIOUT PGBOUT PGIOUT IGEN
#
#   Input script positional parameters:
#     1             Input sigma file
#                   defaults to $SIGINP
#     2             Input flux file
#                   defaults to $FLXINP
#     3             Output flux index file
#                   defaults to $FLXIOUT
#     4             Output pressure GRIB file
#                   defaults to $PGBOUT
#     5             Output pressure GRIB index file
#                   defaults to $PGIOUT, then to none
#     8             Model generating code,
#                   defaults to $IGEN, then to input sigma generating code
#
#   Imported Shell Variables:
#     SIGINP        Input sigma file
#                   overridden by $1
#     FLXINP        Input flux file
#                   overridden by $2
#     FLXIOUT       Output flux index file
#                   overridden by $3
#     PGBOUT        Output pressure GRIB file
#                   overridden by $4. If not defined,
#                   post will use the filename specified in
#                   the control file
#     PGIOUT        Output pressure GRIB index file
#                   overridden by $5; defaults to none
#     IGEN          Model generating code
#                   overridden by $8; defaults to input sigma generating code
##### Moorthi: Add new imported shell variable for running chgres
#     CHGRESSH      optional: the script to run chgres
#		    default to to ${USHglobal}/global_chgres.sh
#     SIGLEVEL      optional: the coordinate text file
#		    default to to /nwprod/fix/global_hyblev.l${LEVS}.txt
##### Chuang: Add new imported Shell Variable for ncep post
#     OUTTYP        Output file type read in by post 
#                   1: if user has a sigma file and needs post to run chgres to convert to gfs io file
#                   2: if user already has a gfs io file
#                   3: if user uses post to read sigma file directly
#                   0: if user wishes to generate both gfsio and sigma files
#                   4: if user uses post to read nemsio file directly
#     VDATE         Verifying date 10 digits yyyymmddhh
#     GFSOUT        Optional, output file name from chgres which is input file name to nceppost 
#                   if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
#     CTLFILE       Optional, Your version of control file if not using operational one
#     OVERPARMEXEC  Optional, the executable for changing Grib KPDS ID
#		    default to to ${EXECglobal}/overparm_grib 
#     CHGRESTHREAD  Optional, speed up chgres by using multiple threads 
#		    default to 1
#     FILTER        Optional, set to 1 to filter SLP and 500 mb height using copygb
#     D3DINP        Optional, Inout D3D file, if not defined, post will run
#                   without processing D3D file
#     D3DOUT        Optional, output D3D file, if not defined, post will
#                   use the file name specified in the control file
#     IPVOUT        Optional, output IPV file, if not defined, post will
#                   use the file name specified in the control file
#    GENPSICHI      Optional, set to YES will generate psi and chi and
#                   append it to the end of PGBOUT.  Default to NO
#    GENPSICHIEXE   Optional, specify where executable is for generating
#                   psi and chi.
########################################################################       
#     EXECUTIL      Directory for utility executables
#                   defaults to /nwprod/util/exec
#     USHUTIL       Directory for utility scripts
#                   defaults to /nwprod/util/ush
#     EXECglobal    Directory for global executables
#                   defaults to /nwprod/exec
#     USHglobal     Directory for global scripts
#                   defaults to /nwprod/ush
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     MP            Multi-processing type ("p" or "s")
#                   defaults to "p", or "s" if LOADL_STEP_TYPE is not PARALLEL
#     XC            Suffix to add to executables
#                   defaults to none
#     POSTGPEXEC    Global post executable
#                   defaults to ${EXECglobal}/ncep_post
#     GRBINDEX      GRIB index maker
#                   defaults to ${EXECUTIL}/grbindex$XC
#     ANOMCATSH     Global anomaly GRIB script
#                   defaults to ${USHglobal/global_anomcat.sh
#     POSTGPLIST    File containing further namelist inputs
#                   defaults to /dev/null
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     POSTGPVARS    Other namelist inputs to the global post executable
#                   such as IDRT,KO,PO,KTT,KT,PT,KZZ,ZZ,
#                           NCPUS,MXBIT,IDS,POB,POT,MOO,MOOA,MOW,MOWA,
#                           ICEN,ICEN2,IENST,IENSI
#                   defaults to none set
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 64000000
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#     PGMOUT        Executable standard output
#                   defaults to $pgmout, then to '&1'
#     PGMERR        Executable standard error
#                   defaults to $pgmerr, then to '&1'
#     pgmout        Executable standard output default
#     pgmerr        Executable standard error default
#     REDOUT        standard output redirect ('1>' or '1>>')
#                   defaults to '1>', or to '1>>' to append if $PGMOUT is a file
#     REDERR        standard error redirect ('2>' or '2>>')
#                   defaults to '2>', or to '2>>' to append if $PGMERR is a file
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#                  $ANOMCATSH
#
#     programs   : $POSTGPEXEC
#                  $GRBINDEX
#
#     input data : $1 or $SIGINP
#                  $2 or $SFCINP
#                  $POSTGPLIST
#
#     output data: $3 or $FLXIOUT
#                  $4 or $PGBOUT
#                  $5 or $PGIOUT
#                  $PGMOUT
#                  $PGMERR
#
#     scratch    : ${DATA}/postgp.inp.sig
#                  ${DATA}/postgp.inp.flx
#                  ${DATA}/postgp.out.pgb
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#   Control variable resolution priority
#     1 Command line argument.
#     2 Environment variable.
#     3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####
################################################################################
#  Set environment.
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
#  Command line arguments.
export SIGINP=${1:-${SIGINP}}
export FLXINP=${2:-${FLXINP}}
export FLXIOUT=${3:-${FLXIOUT}}
export PGBOUT=${4:-${PGBOUT}}
#export PGIOUT=${5:-${PGIOUT}}
export PGIOUT=${PGIOUT:-pgb.idx}
export IO=${6:-${IO:-0}}
export JO=${7:-${JO:-0}}
export IGEN=${8:-${IGEN:-0}}
#  Directories.
export NWPROD=${NWPROD:-/nwprod}
#export EXECUTIL=${EXECUTIL:-$NWPROD/util/exec}
export USHUTIL=${USHUTIL:-$NWPROD/util/ush}
export EXECglobal=${EXECglobal:-$NWPROD/exec}
export USHglobal=${USHglobal:-$NWPROD/ush}
export DATA=${DATA:-$(pwd)}
#  Filenames.
export MP=${MP:-$([[ $LOADL_STEP_TYPE = PARALLEL ]]&&echo "p"||echo "s")}
export XC=${XC}
export POSTGPEXEC=${POSTGPEXEC:-${EXECglobal}/ncep_post}
export OVERPARMEXEC=${OVERPARMEXEC:-${EXECglobal}/overparm_grib}
export ANOMCATSH=${ANOMCATSH:-${USHglobal}/global_anomcat.sh}
export CHGRESSH=${CHGRESSH:-${USHglobal}/global_chgres.sh}
export POSTGPLIST=${POSTGPLIST:-/dev/null}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
export GFSOUT=${GFSOUT:-gfsout}
export CTLFILE=${CTLFILE:-$NWPROD/parm/gfs_cntrl.parm}
export MODEL_OUT_FORM=${MODEL_OUT_FORM:-binarynemsiompiio}
export GRIBVERSION=${GRIBVERSION:-'grib1'}
#  Other variables.
export POSTGPVARS=${POSTGPVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-64000000}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export CHGRESTHREAD=${CHGRESTHREAD:-1}
export FILTER=${FILTER:-1}
export GENPSICHI=${GENPSICHI:-NO}
export GENPSICHIEXE=${GENPSICHIEXE:-${EXECglobal}/genpsiandchi}
export ens=${ens:-NO}
#export D3DINP=${D3DINP:-/dev/null}
typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}
################################################################################
#  Preprocessing
$INISCRIPT

# Chuang: Run chgres if OUTTYP=1 or 0

export APRUN=${APRUNP:-${APRUN:-""}}

# exit if SIGINP does not exist
if [ ${OUTTYP} -le 3 ] ; then
 if [ ! -s $SIGINP ] ; then
  echo "sigma file not found, exitting"
  exit 111
 fi
fi

export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr} 
export IDRT=${IDRT:-4}

if [ ${OUTTYP} -le 1 ] ; then
 export JCAP=${JCAP:-`echo jcap|$SIGHDR ${SIGINP}`}
 export LEVS=${LEVS:-`echo levs|$SIGHDR ${SIGINP}`}
 export IDVC=${IDVC:-$(echo idvc|$SIGHDR ${SIGINP})}
 export IDVM=${IDVM:-$(echo idvm|$SIGHDR ${SIGINP})}
 export NVCOORD=${NVCOORD:-$(echo nvcoord|$SIGHDR ${SIGINP})}
 export IVSSIG=${IVSSIG:-$(echo ivs|$SIGHDR ${SIGINP})}
 export LATCH=${LATCH:-8}
 if [ ${OUTTYP} -eq 1 ] ; then 
  export CHGRESVARS="IDVC=$IDVC,IDVM=$IDVM,NVCOORD=$NVCOORD,IVSSIG=$IVSSIG,LATCH=$LATCH,"  
 elif [ ${OUTTYP} -eq 0 ] ; then
  export CHGRESVARS="LATCH=$LATCH,$CHGRESVARS"
 fi 
 #export SIGLEVEL=${SIGLEVEL:-""}
 export SIGLEVEL=${SIGLEVEL:-"$NWPROD/fix/global_hyblev.l${LEVS}.txt"}
 # specify threads for running chgres
 export OMP_NUM_THREADS=$CHGRESTHREAD 
 export NTHREADS=$OMP_NUM_THREADS
 if [ ${JCAP} -eq 574 -a ${IDRT} -eq 4 ]
 then
    export NTHSTACK=1024000000
 fi   
 export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"

 $CHGRESSH

 export ERR=$?
 export err=$ERR
 $ERRSCRIPT||exit 1
 
# run post to read sigma file directly if OUTTYP=3
elif [ ${OUTTYP} -eq 3 ] ; then
 export LONB=${LONB:-`echo lonb|$SIGHDR ${SIGINP}`}
 export LATB=${LATB:-`echo latb|$SIGHDR ${SIGINP}`}
 export MODEL_OUT_FORM=sigio
 export GFSOUT=${SIGINP}

# run post to read nemsio file if OUTTYP=4
elif [ ${OUTTYP} -eq 4 ] ; then
 export nemsioget=${nemsioget:-$EXECglobal/nemsio_get}
 export LONB=${LONB:-$($nemsioget $NEMSINP dimx | awk '{print $2}')}
 export LATB=${LATB:-$($nemsioget $NEMSINP dimy | awk '{print $2}')}
 export JCAP=${JCAP:-`expr $LATB - 2`}
# export LONB=${LONB:-$($nemsioget $NEMSINP lonf |grep -i "lonf" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
# export LATB=${LATB:-$($nemsioget $NEMSINP latg |grep -i "latg" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
# export JCAP=${JCAP:-$($nemsioget $NEMSINP jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}

 export MODEL_OUT_FORM=${MODEL_OUT_FORM:-binarynemsiompiio}
 export GFSOUT=${NEMSINP}
 ln -sf $FIXglobal/fix_am/global_lonsperlat.t${JCAP}.${LONB}.${LATB}.txt  ./lonsperlat.dat 
 ln -sf $FIXglobal/fix_am/global_hyblev.l${LEVS}.txt                      ./global_hyblev.txt
fi

# allow threads to use threading in Jim's sp lib
# but set default to 1 
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}

pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
################################################################################
#  Post GRIB
export PGM=$POSTGPEXEC
export pgm=$PGM
$LOGSCRIPT
cat <<EOF >postgp.inp.nml$$
 &NAMPGB
 $POSTGPVARS
EOF

cat <<EOF >>postgp.inp.nml$$
 /
EOF
if [[ "$VERBOSE" = "YES" ]]
then
   cat postgp.inp.nml$$
fi

# making the time stamp format for ncep post
export YY=`echo $VDATE | cut -c1-4`
export MM=`echo $VDATE | cut -c5-6`
export DD=`echo $VDATE | cut -c7-8`
export HH=`echo $VDATE | cut -c9-10`

cat > itag <<EOF
$GFSOUT
${MODEL_OUT_FORM}
${GRIBVERSION}
${YY}-${MM}-${DD}_${HH}:00:00
GFS
$FLXINP
$D3DINP
EOF

cat postgp.inp.nml$$ >> itag

cat itag

rm -f fort.*

#ln -sf $SIGINP     postgp.inp.sig$$
#ln -sf $FLXINP     postgp.inp.flx$$
#ln -sf $PGBOUT     postgp.out.pgb$$

# change model generating Grib number 
if [ ${GRIBVERSION} = grib1 ]; then

  if [ ${IGEN} -le 9 ] ; then
   cat ${CTLFILE}|sed s:00082:0000${IGEN}:>./gfs_cntrl.parm
  elif [ ${IGEN} -le 99 ] ; then
   cat ${CTLFILE}|sed s:00082:000${IGEN}:>./gfs_cntrl.parm
  elif [ ${IGEN} -le 999 ] ; then
   cat ${CTLFILE}|sed s:00082:00${IGEN}:>./gfs_cntrl.parm
  else
   ln -sf ${CTLFILE} ./gfs_cntrl.parm
  fi
  ln -sf ./gfs_cntrl.parm fort.14

elif [ ${GRIBVERSION} = grib2 ]; then
  cp ${POSTGRB2TBL} .
  cp ${PostFlatFile} ./postxconfig-NT.txt
  if [ ${ens} = "YES" ] ; then
    sed < ${PostFlatFile} -e "s#negatively_pert_fcst#${ens_pert_type}#" > ./postxconfig-NT.txt
  fi
#  cp ${CTLFILE} postcntrl.xml

fi
export CTL=`basename $CTLFILE`

ln -sf griddef.out fort.110
cp ${PARMglobal}/nam_micro_lookup.dat ./eta_micro_lookup.dat

${APRUN:-mpirun.lsf} $POSTGPEXEC < itag > outpost_gfs_${VDATE}_${CTL}

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

if [ $FILTER = "1" ] ; then

# Filter SLP and 500 mb height using copygb, change GRIB ID, and then
# cat the filtered fields to the pressure GRIB file, from Iredell

if [ $GRIBVERSION = grib1 ]; then
  $COPYGB -x -i'4,0,80' -k'4*-1,1,102' $PGBOUT tfile
  ln -s -f tfile fort.11
  ln -s -f prmsl fort.51
  echo 0 2|$OVERPARMEXEC
  $COPYGB -x -i'4,1,5' -k'4*-1,7,100,500' $PGBOUT tfile
  ln -s -f tfile fort.11
  ln -s -f h5wav fort.51
  echo 0 222|$OVERPARMEXEC

#cat $PGBOUT prmsl h5wav >> $PGBOUT
  cat  prmsl h5wav >> $PGBOUT

elif [ $GRIBVERSION = grib2 ]; then
  if [ ${ens} = YES ] ; then
    $COPYGB2 -x -i'4,0,80' -k'1 3 0 7*-9999 101 0 0' $PGBOUT tfile
  else
    $COPYGB2 -x -i'4,0,80' -k'0 3 0 7*-9999 101 0 0' $PGBOUT tfile
  fi
  $WGRIB2 tfile -set_byte 4 11 1 -grib prmsl
  if [ ${ens} = YES ] ; then
    $COPYGB2 -x -i'4,1,5' -k'1 3 5 7*-9999 100 0 50000' $PGBOUT tfile
  else
    $COPYGB2 -x -i'4,1,5' -k'0 3 5 7*-9999 100 0 50000' $PGBOUT tfile
  fi
  $WGRIB2 tfile -set_byte 4 11 193 -grib h5wav

#cat $PGBOUT prmsl h5wav >> $PGBOUT

  cat  prmsl h5wav >> $PGBOUT

fi

fi

################################################################################
#  Anomaly concatenation
#  for now just do anomaly concentration for grib1
if [ $GRIBVERSION = grib1 ]; then

 if [[ -x $ANOMCATSH ]]
 then
   if [[ -n $PGIOUT ]]
   then
     $GRBINDEX $PGBOUT $PGIOUT
   fi
   export PGM=$ANOMCATSH
   export pgm=$PGM
   $LOGSCRIPT

   eval $ANOMCATSH $PGBOUT $PGIOUT

   export ERR=$?
   export err=$ERR
   $ERRSCRIPT||exit 3
 fi
fi
################################################################################
#  Make GRIB index file
if [[ -n $PGIOUT ]]
then
   if [ $GRIBVERSION = grib2 ]; then
     # JY $GRBINDEX2 $PGBOUT $PGIOUT
     $GRB2INDEX $PGBOUT $PGIOUT
   else
     $GRBINDEX $PGBOUT $PGIOUT
   fi
fi
if [[ -r $FLXINP && -n $FLXIOUT && $OUTTYP -le 3 ]]
then
   $GRBINDEX $FLXINP $FLXIOUT
fi
################################################################################
# generate psi and chi
echo "GENPSICHI= " $GENPSICHI
if [ $GENPSICHI = YES ] ; then
#echo "PGBOUT PGIOUT=" $PGBOUT $PGIOUT
#echo "YY MM=" $YY $MM
 export psichifile=./psichi.grb
 $GENPSICHIEXE < postgp.inp.nml$$
 rc=$?
 if [[ $rc -ne 0 ]] ; then echo 'Nonzero return code rc= '$rc ; exit 3 ; fi
 cat ./psichi.grb >> $PGBOUT
fi
################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
