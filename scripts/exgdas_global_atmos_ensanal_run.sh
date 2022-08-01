#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_global_atmos_analysis_run.sh
# Script description:  Runs the global atmospheric analysis with FV3-JEDI
#
# Author: Cory Martin        Org: NCEP/EMC     Date: 2021-12-28
#
# Abstract: This script makes a global model atmospheric analysis using FV3-JEDI
#           and also (for now) updates increment files using a python ush utility
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: Orion
#
################################################################################

#  Set environment.
source "$HOMEgfs/ush/preamble.sh"

#  Directories
pwd=$(pwd)

#  Utilities
export NLN=${NLN:-"/bin/ln -sf"}
export INCPY=${INCPY:-"$HOMEgfs/sorc/gdas.cd/ush/jediinc2fv3.py"}
export GENYAML=${GENYAML:-"$HOMEgfs/sorc/gdas.cd/ush/genYAML"}
export GETOBSYAML=${GETOBSYAML:-"$HOMEgfs/sorc/gdas.cd/ush/get_obs_list.py"}

################################################################################
# make subdirectories
mkdir -p $DATA/fv3jedi
mkdir -p $DATA/obs
mkdir -p $DATA/diags
mkdir -p $DATA/bc
mkdir -p $DATA/anl

################################################################################
# generate YAML file
cat > $DATA/temp.yaml << EOF
template: ${ATMENSYAML}
output: $DATA/fv3jedi_ens.yaml
config:
  atm: true
  BERROR_YAML: $BERROR_YAML
  OBS_DIR: obs
  DIAG_DIR: diags
  CRTM_COEFF_DIR: crtm
  BIAS_IN_DIR: obs
  BIAS_OUT_DIR: bc
  OBS_PREFIX: $OPREFIX
  BIAS_PREFIX: $GPREFIX
  OBS_LIST: $OBS_LIST
  OBS_YAML_DIR: $OBS_YAML_DIR
  BKG_DIR: bkg
  fv3jedi_staticb_dir: berror
  fv3jedi_fix_dir: fv3jedi
  fv3jedi_fieldset_dir: fv3jedi
  fv3jedi_fieldmetadata_dir: fv3jedi
  OBS_DATE: '$CDATE'
  BIAS_DATE: '$GDATE'
  ANL_DIR: anl/
  NMEM_ENKF: '$NMEM_ENKF'
  INTERP_METHOD: '$INTERP_METHOD'
EOF
$GENYAML --config $DATA/temp.yaml

################################################################################
# link observations to $DATA
$GETOBSYAML --config $DATA/fv3jedi_ens.yaml --output $DATA/${OPREFIX}obsspace_list
files=$(cat $DATA/${OPREFIX}obsspace_list)
for file in $files; do
  basefile=$(basename $file)
  $NLN $COMIN/$basefile $DATA/obs/$basefile
done

# link backgrounds to $DATA
# linking FMS RESTART files for now
# change to (or make optional) for cube sphere history later
##$NLN ${COMIN_GES}/RESTART $DATA/bkg


# Link ensemble backgrounds to $DATA.  Make directories
# for ensemble output
if [ $DOHYBVAR = "YES" -o $DO_JEDIENS = "YES" ]; then
   mkdir -p $DATA/bkg
   for imem in $(seq 1 $NMEM_ENKF); do
      memchar="mem"$(printf %03i $imem)
      mkdir -p $DATA/bkg/$memchar
      $NLN ${COMIN_GES_ENS}/$memchar/RESTART $DATA/bkg/$memchar
      mkdir -p $DATA/anl/$memchar
   done
fi
    
################################################################################
# link fix files to $DATA
# static B
##CASE_BERROR=${CASE_BERROR:-${CASE_ANL:-$CASE}}
##$NLN $FV3JEDI_FIX/bump/$CASE_BERROR/ $DATA/berror

# vertical coordinate
LAYERS=$(expr $LEVS - 1)
$NLN $FV3JEDI_FIX/fv3jedi/fv3files/akbk${LAYERS}.nc4 $DATA/fv3jedi/akbk.nc4

# other FV3-JEDI fix files
$NLN $FV3JEDI_FIX/fv3jedi/fv3files/fmsmpp.nml $DATA/fv3jedi/fmsmpp.nml
$NLN $FV3JEDI_FIX/fv3jedi/fv3files/field_table_gfdl $DATA/fv3jedi/field_table

# fieldmetadata
$NLN $FV3JEDI_FIX/fv3jedi/fieldmetadata/gfs-restart.yaml $DATA/fv3jedi/gfs-restart.yaml

# fieldsets
fieldsets="dynamics.yaml ufo.yaml"
for fieldset in $fieldsets; do
  $NLN $FV3JEDI_FIX/fv3jedi/fieldsets/$fieldset $DATA/fv3jedi/$fieldset
done

# CRTM coeffs
$NLN $FV3JEDI_FIX/crtm/2.3.0_jedi $DATA/crtm

#  Link executable to $DATA
$NLN $JEDIENSEXE $DATA/fv3jedi_ens.x

################################################################################
# run executable
export pgm=$JEDIVAREXE
. prep_step
$APRUN_ATMENSANAL $DATA/fv3jedi_ens.x $DATA/fv3jedi_ens.yaml 1>&1 2>&2
export err=$?; err_chk

################################################################################
# translate FV3-JEDI increment to FV3 readable format
for imem in $(seq 1 $NMEM_ENKF); do
    memchar="mem"$(printf %03i $imem)
    atminc_jedi=$DATA/anl/$memchar/atminc.${PDY}_${cyc}0000z.nc4
    atminc_fv3=$COMOUT_ENS/$memchar/${CDUMP}.${cycle}.atminc.nc
    mkdir -p $COMOUT_ENS/$memchar
    if [ -s $atminc_jedi ]; then
	$INCPY $atminc_jedi $atminc_fv3
	export err=$?
    else
	echo "***WARNING*** missing $atminc_jedi   ABORT"
	export err=99
    fi
    err_chk
done

################################################################################
# Create log file noting creating of analysis increment file
echo "$CDUMP $CDATE atminc done at $(date)" > $COMOUT_ENS/${CDUMP}.${cycle}.loginc.txt

################################################################################
# Copy diags and YAML to $COMOUT
cp -r $DATA/fv3jedi_ens.yaml $COMOUT_ENS/${CDUMP}.${cycle}.fv3jedi_ens.yaml
cp -rf $DATA/diags $COMOUT_ENS/


################################################################################

exit $err

################################################################################
