#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_select_obs.sh
# Script description:  Compute global_gsi innovations
#
# Author: Rahul Mahajan     Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script computes global_gsi innovations
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS-Cray/Theia
#
################################################################################

# Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)

# Utilities
export NLN=${NLN:-"/bin/ln -sf"}

# Scripts.
ANALYSISSH=${ANALYSISSH:-$HOMEgfs/scripts/exglobal_atmos_analysis.sh}

# Prefix and Suffix Variables.
export APREFIX=${APREFIX:-""}
export ASUFFIX=${ASUFFIX:-$SUFFIX}

# Select obs
export RUN_SELECT=${RUN_SELECT:-"YES"}
export USE_SELECT=${USE_SELECT:-"NO"}
export SELECT_OBS=${SELECT_OBS:-$COMOUT/${APREFIX}obsinput}

# Observation Operator GSI namelist initialization
SETUP_INVOBS=${SETUP_INVOBS:-""}
GRIDOPTS_INVOBS=${GRIDOPTS_INVOBS:-""}
BKGVERR_INVOBS=${BKGVERR_INVOBS:-""}
ANBKGERR_INVOBS=${ANBKGERR_INVOBS:-""}
JCOPTS_INVOBS=${JCOPTS_INVOBS:-""}
STRONGOPTS_INVOBS=${STRONGOPTS_INVOBS:-""}
OBSQC_INVOBS=${OBSQC_INVOBS:-""}
OBSINPUT_INVOBS=${OBSINPUT_INVOBS:-""}
SUPERRAD_INVOBS=${SUPERRAD_INVOBS:-""}
SINGLEOB_INVOBS=${SINGLEOB_INVOBS:-""}
LAGDATA_INVOBS=${LAGDATA_INVOBS:-""}
HYBRID_ENSEMBLE_INVOBS=${HYBRID_ENSEMBLE_INVOBS:-""}
RAPIDREFRESH_CLDSURF_INVOBS=${RAPIDREFRESH_CLDSURF_INVOBS:-""}
CHEM_INVOBS=${CHEM_INVOBS:-""}

################################################################################
#  Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 8

[[ ! -d $COMOUT ]] && mkdir -p $COMOUT

################################################################################
# ObsInput file from ensemble mean
rm -f obs*input*
$NLN $SELECT_OBS obsinput.tar

# Whether to save or skip obs
if [ $RUN_SELECT = "YES" -a $USE_SELECT = "NO" ]; then
   lread_obs_save=".true."
   lread_obs_skip=".false."
elif [ $RUN_SELECT = "NO" -a $USE_SELECT = "YES" ]; then
   lread_obs_save=".false."
   lread_obs_skip=".true."
fi

################################################################################
# Innovation Specific setup for ANALYSISSH
export DIAG_SUFFIX=${DIAG_SUFFIX:-""}
export DIAG_COMPRESS=${DIAG_COMPRESS:-"NO"}
export DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
export DOHYBVAR="NO"
export DO_CALC_INCREMENT="NO"
export DO_CALC_ANALYSIS="NO"
export USE_CORRELATED_OBERRS="NO"
export write_fv3_increment=".false."

# GSI Namelist options for observation operator only
export SETUP="miter=0,niter=1,lread_obs_save=$lread_obs_save,lread_obs_skip=$lread_obs_skip,lwrite_predterms=.true.,lwrite_peakwt=.true.,reduce_diag=.true.,$SETUP_INVOBS"
export GRIDOPTS="$GRIDOPTS_INVOBS"
export BKGVERR="bkgv_flowdep=.false.,$BKGVERR_INVOBS"
export ANBKGERR="$ANBKGERR_INVOBS"
export JCOPTS="$JCOPTS_INVOBS"
export STRONGOPTS="tlnmc_option=0,nstrong=0,nvmodes_keep=0,baldiag_full=.false.,baldiag_inc=.false.,$STRONGOPTS_INVOBS"
export OBSQC="$OBSQC_INVOBS"
export OBSINPUT="$OBSINPUT_INVOBS"
export SUPERRAD="$SUPERRAD_INVOBS"
export SINGLEOB="$SINGLEOB_INVOBS"
export LAGDATA="$LAGDATA_INVOBS"
export HYBRID_ENSEMBLE=""
export RAPIDREFRESH_CLDSURF="$RAPIDREFRESH_CLDSURF_INVOBS"
export CHEM="$CHEM_INVOBS"

################################################################################
# Execute GSI as a forward operator

$ANALYSISSH
export err=$?; err_chk

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
