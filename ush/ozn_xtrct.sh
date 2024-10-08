#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

#------------------------------------------------------------------
#  ozn_xtrct.sh
#
#  This script performs the data extraction from the oznstat
#  diagnostic files.  The resulting data (*.ieee_d) files, GrADS
#  control files and stdout files will be moved to the 
#  $TANKverf_ozn.  
#
#  Calling scripts must define: 
#       $TANKverf_ozn
#       $PDY
#       $cyc
#
#  Return values are 
#	0 = normal 
#	2 = unable to generate satype list; may indicate no diag
#		files found in oznstat file
#------------------------------------------------------------------

#--------------------------------------------------
#  check_diag_files
#  
#  Compare $satype (which contains the contents of 
#  gdas_oznmon_satype.txt to $avail_satype which is
#  determined by the contents of the oznstat file.
#  Report any missing diag files in a file named
#  bad_diag.$PDY$cyc
#
check_diag_files() {
   pdate=$1
   found_satype=$2
   avail_satype=$3

   out_file="bad_diag.${pdate}"

   echo ""; echo ""; echo "--> check_diag_files"

   for type in ${found_satype}; do
      len_check=$(echo "${avail_satype}" | grep "${type}" | wc -c)

      if [[ ${len_check} -le 1 ]]; then
         echo "missing diag file -- diag_${type}_ges.${pdate}.gz not found" >> "./${out_file}"
      fi
   done

   echo "<-- check_diag_files"; echo ""; echo ""
}


iret=0
export NCP=${NCP:-/bin/cp}
VALIDATE_DATA=${VALIDATE_DATA:-0}
nregion=${nregion:-6}
DO_DATA_RPT=${DO_DATA_RPT:-0}

netcdf_boolean=".false."
if [[ ${OZNMON_NETCDF} -eq 1 ]]; then
   netcdf_boolean=".true."
fi

OZNMON_NEW_HDR=${OZNMON_NEW_HDR:-0}
new_hdr="F"
if [[ ${OZNMON_NEW_HDR} -eq 1 ]]; then
   new_hdr="T"
fi

#------------------------------------------------------------------
# if VALIDATE_DATA then locate and untar base file
#
validate=".FALSE."
if [[ ${VALIDATE_DATA} -eq 1 ]]; then
   if [[ ! -e ${ozn_val_file} && ! -h ${ozn_val_file} ]]; then
      echo "WARNING:  VALIDATE_DATA set to 1, but unable to locate ${ozn_val_file}"
      echo "          Setting VALIDATE_DATA to 0/OFF"
      VALIDATE_DATA=0
   else
      validate=".TRUE."
      val_file=$(basename "${ozn_val_file}")
      ${NCP} "${ozn_val_file}" "${val_file}"
      tar -xvf "${val_file}"
   fi
fi
echo "VALIDATE_DATA, validate = ${VALIDATE_DATA}, ${validate} "



#------------------------------------------------------------------
# ozn_ptype here is the processing type which is intended to be "ges" 
# or "anl".  Default is "ges".  
#
ozn_ptype=${ozn_ptype:-"ges anl"}


#---------------------------------------------------------------------------
#  Build satype list from the available diag files.
#
#  An empty satype list means there are no diag files to process.  That's
#  a problem, reported by an iret value of 2
#

avail_satype=$(ls -1 d*ges* | sed -e 's/_/ /g;s/\./ /' | gawk '{ print $2 "_" $3 }')

if [[ ${DO_DATA_RPT} -eq 1 ]]; then
   if [[ -e ${SATYPE_FILE} ]]; then
      satype=$(cat "${SATYPE_FILE}")
      check_diag_files "${PDY}${cyc}" "${satype}" "${avail_satype}"
   else
      echo "WARNING:  missing ${SATYPE_FILE}"
   fi
fi

len_satype=$(echo -n "${satype}" | wc -c)

if [[ ${len_satype} -le 1 ]]; then
   satype=${avail_satype}
fi

echo "${satype}"


len_satype=$(echo -n "${satype}" | wc -c)

if [[ ${DO_DATA_RPT} -eq 1 && ${len_satype} -lt 1 ]]; then
   iret=2 

else

   #--------------------------------------------------------------------
   #   Copy extraction programs to working directory
   #
   ${NCP} "${EXECgfs}/oznmon_time.x" ./oznmon_time.x
   if [[ ! -e oznmon_time.x ]]; then
      iret=2
      exit ${iret}
   fi
   ${NCP} "${EXECgfs}/oznmon_horiz.x" ./oznmon_horiz.x
   if [[ ! -e oznmon_horiz.x ]]; then
      iret=3
      exit ${iret}
   fi


   #---------------------------------------------------------------------------
   #  Outer loop over $ozn_ptype (default values 'ges', 'anl')
   #
   for ptype in ${ozn_ptype}; do

      iyy="${PDY:0:4}"
      imm="${PDY:4:2}"
      idd="${PDY:6:2}"
      ihh=${cyc}
 
      for type in ${avail_satype}; do
         if [[ -f "diag_${type}_${ptype}.${PDY}${cyc}.gz" ]]; then
            mv "diag_${type}_${ptype}.${PDY}${cyc}.gz" "${type}.${ptype}.gz"
            gunzip "./${type}.${ptype}.gz"

            echo "processing ptype, type:  ${ptype}, ${type}"
            rm -f input

cat << EOF > input
            &INPUT
            satname='${type}',
            iyy=${iyy},
            imm=${imm},
            idd=${idd},
            ihh=${ihh},
            idhh=-720,
            incr=6,
            nregion=${nregion},
            region(1)='global',    rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
            region(2)='70N-90N',   rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 70.0,rlatmax(2)= 90.0,
            region(3)='20N-70N',   rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)= 20.0,rlatmax(3)= 70.0,
            region(4)='20S-20N',   rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
            region(5)='20S-70S',   rlonmin(5)=-180.0,rlonmax(5)=180.0,rlatmin(5)=-70.0,rlatmax(5)=-20.0,
            region(6)='70S-90S',   rlonmin(6)=-180.0,rlonmax(6)=180.0,rlatmin(6)=-90.0,rlatmax(6)=-70.0,
            validate=${validate},
            new_hdr=${new_hdr},
            ptype=${ptype},
            netcdf=${netcdf_boolean}
         /
EOF


            echo "oznmon_time.x HAS STARTED ${type}"
   
            ./oznmon_time.x < input > "stdout.time.${type}.${ptype}"

            echo "oznmon_time.x HAS ENDED ${type}"

            if [[ ! -d ${TANKverf_ozn}/time ]]; then
               mkdir -p "${TANKverf_ozn}/time"
            fi
            ${NCP} "${type}.${ptype}.ctl"                  "${TANKverf_ozn}/time/"
            ${NCP} "${type}.${ptype}.${PDY}${cyc}.ieee_d"  "${TANKverf_ozn}/time/"
   
            ${NCP} bad* "${TANKverf_ozn}/time/"
   
            rm -f input

cat << EOF > input
            &INPUT
            satname='${type}',
            iyy=${iyy},
            imm=${imm},
            idd=${idd},
            ihh=${ihh},
            idhh=-18,
            incr=6,
            new_hdr=${new_hdr},
            ptype=${ptype},
            netcdf=${netcdf_boolean}
         /
EOF

            echo "oznmon_horiz.x HAS STARTED ${type}"
   
            ./oznmon_horiz.x < input > "stdout.horiz.${type}.${ptype}"

            echo "oznmon_horiz.x HAS ENDED ${type}"

            if [[ ! -d ${TANKverf_ozn}/horiz ]]; then
               mkdir -p "${TANKverf_ozn}/horiz"
            fi
            ${NCP} "${type}.${ptype}.ctl"                   "${TANKverf_ozn}/horiz/"

            ${COMPRESS} "${type}.${ptype}.${PDY}${cyc}.ieee_d"
            ${NCP} "${type}.${ptype}.${PDY}${cyc}.ieee_d.${Z}" "${TANKverf_ozn}/horiz/"
      

            echo "finished processing ptype, type:  ${ptype}, ${type}"

         else
            echo "diag file for ${type}.${ptype} not found"
         fi

      done  # type in satype

   done	 # ptype in $ozn_ptype

   tar -cvf stdout.horiz.tar stdout.horiz*
   ${COMPRESS} stdout.horiz.tar
   ${NCP} "stdout.horiz.tar.${Z}" "${TANKverf_ozn}/horiz/"

   tar -cvf stdout.time.tar stdout.time*
   ${COMPRESS} stdout.time.tar
   ${NCP} "stdout.time.tar.${Z}" "${TANKverf_ozn}/time/"
fi

exit ${iret}
