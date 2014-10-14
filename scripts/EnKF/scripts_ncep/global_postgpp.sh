
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_postgp.sh           
# Script description:  Posts the global pressure GRIB file
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-05-01
#
# Abstract: This script reads a single global sigma file and (optionally)
#   a global flux file and creates a global pressure GRIB file.
#   The resolution and generating code of the output GRIB file can also
#   be set in the argument list.
#
# Script history log:
# 1999-05-01  Mark Iredell
#
# Usage:  global_postgp.sh SIGINP FLXINP FLXIOUT PGBOUT PGIOUT IO JO IGEN
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
#     6             Number of longitudes
#                   defaults to $IO, then to resolution-dependent defaults
#     7             Number of latitudes
#                   defaults to $JO, then to resolution-dependent defaults
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
#                   overridden by $4
#     PGIOUT        Output pressure GRIB index file
#                   overridden by $5; defaults to none
#     IO            Number of longitudes
#                   overridden by $6; default is resolution-dependent
#     JO            Number of latitudes
#                   overridden by $7; default is resolution-dependent
#     IGEN          Model generating code
#                   overridden by $8; defaults to input sigma generating code
#     EXECUTIL      Directory for utility executables
#                   defaults to /nwprod/util/exec
#     USHUTIL       Directory for utility scripts
#                   defaults to /nwprod/util/ush
#     EXECGLOBAL    Directory for global executables
#                   defaults to /nwprod/exec
#     USHGLOBAL     Directory for global scripts
#                   defaults to /nwprod/ush
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     MP            Multi-processing type ("p" or "s")
#                   defaults to "p", or "s" if LOADL_STEP_TYPE is not PARALLEL
#     XC            Suffix to add to executables
#                   defaults to none
#     POSTGPEXEC    Global post executable
#                   defaults to ${EXECGLOBAL}/global_postg$MP$XC
#     GRBINDEX      GRIB index maker
#                   defaults to ${EXECUTIL}/grbindex$XC
#     ANOMCATSH     Global anomaly GRIB script
#                   defaults to ${USHGLOBAL}/global_anomcat.sh
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
export PGIOUT=${5:-${PGIOUT}}
export IO=${6:-${IO:-0}}
export JO=${7:-${JO:-0}}
export IGEN=${8:-${IGEN:-0}}
#  Directories.
export EXECUTIL=${EXECUTIL:-/tg4/projects/reanl/whitaker/ncepgfs/bin}
export USHUTIL=${USHUTIL:-/tg4/projects/reanl/whitaker/ncepgfs/scripts}
export EXECGLOBAL=${EXECGLOBAL:-/tg4/projects/reanl/whitaker/ncepgfs/bin}
export USHGLOBAL=${USHGLOBAL:-/tg4/projects/reanl/whitaker/ncepgfs/scripts}
export DATA=${DATA:-$(pwd)}
#  Filenames.
export MP=${MP:-$([[ $LOADL_STEP_TYPE = PARALLEL ]]&&echo "p"||echo "s")}
export XC=${XC}
export POSTGPEXEC=${POSTGPEXEC:-${EXECGLOBAL}/global_postgs}
export GRBINDEX=${GRBINDEX:-${EXECUTIL}/grbindex$XC}
export ANOMCATSH=${ANOMCATSH:-${USHGLOBAL}/global_anomcat.sh}
export POSTGPLIST=${POSTGPLIST:-/dev/null}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export POSTGPVARS=${POSTGPVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-64000000}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
#typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
#typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}
################################################################################
#  Preprocessing
$INISCRIPT
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
export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
export PGM=$POSTGPEXEC
export pgm=$PGM
$LOGSCRIPT
cat <<EOF >global_postgp.nml
 &NAMPGB
  DDSIG='postgp.inp.sig$$',
  DDFLX='postgp.inp.flx$$',
  DDPGB='postgp.out.pgb$$',
  IO=$IO, JO=$JO, IGEN=$IGEN,
 $POSTGPVARS
EOF
    cat $POSTGPLIST >>global_postgp.nml
    cat <<EOF >>global_postgp.nml
 /
EOF
#if [[ "$VERBOSE" = "YES" ]]
#then
   cat global_postgp.nml
#fi
ln -sf $SIGINP     postgp.inp.sig$$
ls -l postgp.inp.sig$$
ln -sf $FLXINP     postgp.inp.flx$$
ls -l postgp.inp.flx$$
ln -sf $PGBOUT     postgp.out.pgb$$

#eval $POSTGPEXEC
$MPICH/bin/mpirun_rsh -hostfile $HOSTFILE -np 1 $POSTGPEXEC
#$MPICH/bin/mpiexec -hostfile $HOSTFILE -n 1 $POSTGPEXEC

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2
rm global_postgp.nml postgp.inp.sig$$ postgp.inp.flx$$ postgp.out.pgb$$
################################################################################
#  Anomaly concatenation
if [[ -x $ANOMCATSH ]]
then
   if [[ -n $PGIOUT ]]
   then
      $GRBINDEX $PGBOUT $PGIOUT
   fi
   export PGM=$ANOMCATSH
   export pgm=$PGM
   $LOGSCRIPT

   eval $ANOMCATSH $PGBOUT $PGIOUT $REDOUT$PGMOUT $REDERR$PGMERR

   export ERR=$?
   export err=$ERR
   $ERRSCRIPT||exit 3
fi
################################################################################
#  Make GRIB index file
#if [[ -n $PGIOUT ]]
#then
#   $GRBINDEX $PGBOUT $PGIOUT
#fi
#if [[ -r $FLXINP && -n $FLXIOUT ]]
#then
#   $GRBINDEX $FLXINP $FLXIOUT
#fi
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
