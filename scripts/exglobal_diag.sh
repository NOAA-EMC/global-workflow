#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_diag.sh
# Script description:  Creates diagnostic files after GSI analysis is performed
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2020-03-03
#
# Abstract: This script creates GSI diagnostic files after GSI exits successfully
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS-Dell / Hera
#
################################################################################

#  Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

#  Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2001010100"}
CDUMP=${CDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
GDATE=$($NDATE -$assim_freq $CDATE)
BDATE=$($NDATE -3 $CDATE)
PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)
bPDY=$(echo $BDATE | cut -c1-8)
bcyc=$(echo $BDATE | cut -c9-10)

# Utilities
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NEMSIOGET=${NEMSIOGET:-${NWPROD}/exec/nemsio_get}
export NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}
export CATEXEC=${CATEXEC:-$HOMEgfs/exec/ncdiag_cat.x}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}

# Guess files

# Analysis files
export APREFIX=${APREFIX:-""}
export ASUFFIX=${ASUFFIX:-$SUFFIX}
RADSTAT=${RADSTAT:-${COMOUT}/${APREFIX}radstat}
PCPSTAT=${PCPSTAT:-${COMOUT}/${APREFIX}pcpstat}
CNVSTAT=${CNVSTAT:-${COMOUT}/${APREFIX}cnvstat}
OZNSTAT=${OZNSTAT:-${COMOUT}/${APREFIX}oznstat}

# Remove stat file if file already exists
[[ -s $RADSTAT ]] && rm -f $RADSTAT
[[ -s $PCPSTAT ]] && rm -f $PCPSTAT
[[ -s $CNVSTAT ]] && rm -f $CNVSTAT
[[ -s $OZNSTAT ]] && rm -f $OZNSTAT

# Obs diag
GENDIAG=${GENDIAG:-"YES"}
DIAG_SUFFIX=${DIAG_SUFFIX:-""}
if [ $netcdf_diag = ".true." ] ; then
   DIAG_SUFFIX="${DIAG_SUFFIX}.nc4"
fi
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ $CFP_MP = "YES" ]; then
    nm=0
fi
DIAG_DIR=${DIAG_DIR:-${COMOUT}/gsidiags}
REMOVE_DIAG_DIR=${REMOVE_DIAG_DIR:-"NO"}

# Set script / GSI control parameters
lrun_subdirs=${lrun_subdirs:-".true."}


################################################################################
# If requested, generate diagnostic files
if [ $GENDIAG = "YES" ] ; then
   if [ $lrun_subdirs = ".true." ] ; then
      for pe in $DIAG_DIR/dir.*; do
         pedir="$(basename -- $pe)"
         $NLN $pe $DATA/$pedir
      done
   else
      err_exit "***FATAL ERROR*** lrun_subdirs must be true.  Abort job"
   fi

   # Set up lists and variables for various types of diagnostic files.
   ntype=3

   diagtype[0]="conv conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_tcp conv_uv conv_spd"
   diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
   diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura ompsnp_npp ompstc8_npp  ompstc8_n20 ompsnp_n20 ompstc8_n21 ompsnp_n21 ompslp_npp gome_metop-c"
   diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp atms_n20 hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_metop-b avhrr_n18 avhrr_n19 avhrr_metop-a amsr2_gcom-w1 gmi_gpm saphir_meghat ahi_himawari8 abi_g16 abi_g17 amsua_metop-c mhs_metop-c iasi_metop-c avhrr_metop-c viirs-m_npp viirs-m_j1 abi_g18 ahi_himawari9 viirs-m_j2 cris-fsr_n21 atms_n21"

   diaglist[0]=listcnv
   diaglist[1]=listpcp
   diaglist[2]=listozn
   diaglist[3]=listrad

   diagfile[0]=$CNVSTAT
   diagfile[1]=$PCPSTAT
   diagfile[2]=$OZNSTAT
   diagfile[3]=$RADSTAT

   numfile[0]=0
   numfile[1]=0
   numfile[2]=0
   numfile[3]=0

   # Set diagnostic file prefix based on lrun_subdirs variable
   if [ $lrun_subdirs = ".true." ]; then
      prefix=" dir.*/"
   else
      prefix="pe*"
   fi

   if [ $USE_CFP = "YES" ]; then
      [[ -f $DATA/diag.sh ]] && rm $DATA/diag.sh
      [[ -f $DATA/mp_diag.sh ]] && rm $DATA/mp_diag.sh
      cat > $DATA/diag.sh << EOFdiag
#!/bin/sh
lrun_subdirs=\$1
binary_diag=\$2
type=\$3
loop=\$4
string=\$5
CDATE=\$6
DIAG_COMPRESS=\$7
DIAG_SUFFIX=\$8
if [ \$lrun_subdirs = ".true." ]; then
   prefix=" dir.*/"
else
   prefix="pe*"
fi
file=diag_\${type}_\${string}.\${CDATE}\${DIAG_SUFFIX}
if [ \$binary_diag = ".true." ]; then
   cat \${prefix}\${type}_\${loop}* > \$file
else
   $CATEXEC -o \$file \${prefix}\${type}_\${loop}*
fi
if [ \$DIAG_COMPRESS = "YES" ]; then
   $COMPRESS \$file
fi
EOFdiag
      chmod 755 $DATA/diag.sh
   fi

   # Collect diagnostic files as a function of loop and type.
   # Loop over first and last outer loops to generate innovation
   # diagnostic files for indicated observation types (groups)
   #
   # NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
   #        loop 03 will contain innovations with respect to
   #        the analysis.  Creation of o-a innovation files
   #        is triggered by write_diag(3)=.true.  The setting
   #        write_diag(1)=.true. turns on creation of o-g
   #        innovation files.

   loops="01 03"
   for loop in $loops; do
      case $loop in
         01) string=ges;;
         03) string=anl;;
          *) string=$loop;;
      esac
      echo $(date) START loop $string >&2
      n=-1
      while [ $((n+=1)) -le $ntype ] ;do
         for type in $(echo ${diagtype[n]}); do
            count=$(ls ${prefix}${type}_${loop}* 2>/dev/null | wc -l)
            if [ $count -gt 1 ]; then
               if [ $USE_CFP = "YES" ]; then
                  echo "$nm $DATA/diag.sh $lrun_subdirs $binary_diag $type $loop $string $CDATE $DIAG_COMPRESS $DIAG_SUFFIX" | tee -a $DATA/mp_diag.sh
		  if [ ${CFP_MP:-"NO"} = "YES" ]; then
		      nm=$((nm+1))
		  fi
               else
                  if [ $binary_diag = ".true." ]; then
                     cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                  else
                     $CATEXEC -o diag_${type}_${string}.${CDATE}${DIAG_SUFFIX} ${prefix}${type}_${loop}*
                  fi
               fi
               echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
               numfile[n]=$(expr ${numfile[n]} + 1)
            elif [ $count -eq 1 ]; then
                cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                if [ $DIAG_COMPRESS = "YES" ]; then
		    $COMPRESS diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
                fi
                echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
                numfile[n]=$(expr ${numfile[n]} + 1)
            fi
         done
      done
      echo $(date) END loop $string >&2
   done

   # We should already be in $DATA, but extra cd to be sure.
   cd $DATA

   # If requested, compress diagnostic files
   if [ $DIAG_COMPRESS = "YES" -a $USE_CFP = "NO" ]; then
      echo $(date) START $COMPRESS diagnostic files >&2
      for file in $(ls diag_*${CDATE}${DIAG_SUFFIX}); do
         $COMPRESS $file
      done
      echo $(date) END $COMPRESS diagnostic files >&2
   fi

   if [ $USE_CFP = "YES" ] ; then
      chmod 755 $DATA/mp_diag.sh
      ncmd=$(cat $DATA/mp_diag.sh | wc -l)
      if [ $ncmd -gt 0 ]; then
         ncmd_max=$((ncmd < npe_node_max ? ncmd : npe_node_max))
         APRUNCFP_DIAG=$(eval echo $APRUNCFP)
         $APRUNCFP_DIAG $DATA/mp_diag.sh
         export err=$?; err_chk
      fi
   fi

   # Restrict diagnostic files containing rstprod data
   rlist="conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_uv saphir"
   for rtype in $rlist; do
       ${CHGRP_CMD} *${rtype}*
   done
   
   # If requested, create diagnostic file tarballs
   if [ $DIAG_TARBALL = "YES" ]; then
      echo $(date) START tar diagnostic files >&2
      n=-1
      while [ $((n+=1)) -le $ntype ] ;do
         TAROPTS="-uvf"
         if [ ! -s ${diagfile[n]} ]; then
            TAROPTS="-cvf"
         fi
         if [ ${numfile[n]} -gt 0 ]; then
            tar $TAROPTS ${diagfile[n]} $(cat ${diaglist[n]})
            export err=$?; err_chk
         fi
      done

      # Restrict CNVSTAT
      chmod 750 $CNVSTAT
      ${CHGRP_CMD} $CNVSTAT

      # Restrict RADSTAT
      chmod 750 $RADSTAT
      ${CHGRP_CMD} $RADSTAT

      echo $(date) END tar diagnostic files >&2
   fi
fi # End diagnostic file generation block - if [ $GENDIAG = "YES" ]

################################################################################
# Postprocessing
# If no processing error, remove $DIAG_DIR
if [[ "$REMOVE_DIAG_DIR" = "YES" && "$err" = "0" ]]; then
    rm -rf $DIAG_DIR
fi

cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA

set +x
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err

