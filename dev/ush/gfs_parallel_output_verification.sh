#!/bin/bash
set -eux

# This utility script is designed to compare the COM from two parallels for reproducibility
#   The requirement of these two parallels (control and target) if running GFS realtime are:
#     1. Same RUN
#     2. Same CDATE
#     3. Same Package checkout with no know result changing difference
#     4. Identical setup. Such as frequency, resolution, ETC.
#     5. Only the the control parallel will run the prep jobs:
#        For GFS v17, there are four prep jobs - 
#          gfs/gdas atmos prep
#          gfs/gdas marine_prepoceanobs
#     6. The COM for control will need to remove all softlink by copy follow the link
#     7. The target parallel will copy the COM from the control and set the prep step to complete
#     8. Resume both parallel workflow
#     9. Run this program after the cycle is completed.
#   The configuration parameters are:
#     COMROOT_Parallel_1 - The COMROOT of the parallel served as the control of this comparison
#     COMROOT_Parallel_2 - The COMROOT of the parallel to be certified as reproduce the control
#     PDY
#     cyc
#     Upto 6 directory can be compared under the COMROOT (default to "NONE") - 
#       Example for GFS v17 parallel:
#         array_element_com[0]=gfs.${PDY}/${cyc}
#         array_element_com[1]=gfs.${PDY}/${cyc}
#         array_element_com[2]=gfs.${PDY}/${cyc}
#         array_element_com[3]="NONE"
#         array_element_com[4]="NONE"
#         array_element_com[5]="NONE"
#     DATA - Working directory that can be removed after program complete
#   The design:
#     - Find all netcdf and grib2 file to compare.
#     - Do hash comparison first for performance optimization
#     - Use "nccmp -d -B --warn=format" to do all netcdf file comparison.
#     - Use cmp as needed.
#     - Ignore all other file types.
#     - See out.*.log file for nccmp DIFFER for each netcdf files that was found mismatch in hash code.
#     - See grib2_differ_report.txt for grib2 cmp comparison results.
#  Designed by Lin Gan

# Configuration area (Modify this area before run the tool)
echo "Start configuration"
# Example - enter real data before proceed
#export PDY="20260430"
#export cyc="00"
#export cycle=t${cyc}z
#export CDATE="${PDY}${cyc}"
#export COMROOT_Parallel_1="/lfs/h2/emc/gfstemp/emc.global/comroot/retrov17_01_realtime"
#export COMROOT_Parallel_2="/lfs/f2/t2o/ptmp/emc/emc.global/ecflow_gfsv17_rt/ptmp/emc.global/ecflow_gfs/para/com"
#export DATAROOT="/lfs/h3/emc/eib/noscrub/ptmp/lin.gan/ecflow_gfs/para/output/prod/today/dir_scan_data_${CDATE}_$$"
# End of Example
array_element_com[0]="gfs.${PDY}/${cyc}"          #COMgfs
array_element_com[1]="gdas.${PDY}/${cyc}"         #COMgdas
array_element_com[2]="enkfgdas.${PDY}/${cyc}"     #COMenkfgdas
array_element_com[3]="NONE"
array_element_com[4]="NONE"
array_element_com[5]="NONE"

# Reg test on one cycle
# array_element_com[0]="NONE"
# array_element_com[1]="gdas.${PDY}/${cyc}"
# array_element_com[2]="NONE"
# array_element_com[3]="NONE"
# array_element_com[4]="NONE"
# array_element_com[5]="NONE"

echo "Summary of the configuration:"
echo "  CDATE: ${CDATE}"
echo "  COMROOT_Parallel_1:"
echo "    ${COMROOT_Parallel_1}"
echo "  COMROOT_Parallel_2:"
echo "    ${COMROOT_Parallel_2}"
echo "  DATAROOT:"
echo "    ${DATAROOT}"
echo "Complete configuration"
# end of Configuration area

# Exception handling - checking for DATA
if [ -d ${DATAROOT} ]; then
  echo "FATAL error: DATA already existing"
  exit 8
fi
# Exception handling - checking for DATA

# Loading modules
set +x
module reset
module load prod_envir prod_util intel/19.1.3.304
module load python/3.12.0
module list
set -x
# Loading modules

HOMEbase=$(pwd)

for arr_elm_idx in $(seq 0 5); do
  if [ ! ${array_element_com[$((10#$arr_elm_idx))]} == "NONE" ]; then
    DATA=${DATAROOT}/${arr_elm_idx}
    mkdir -p $DATA
    CONTROL_DIR_SCAN=${COMROOT_Parallel_1}/${array_element_com[$((10#$arr_elm_idx))]}
    TARGET_DIR_SCAN=${COMROOT_Parallel_2}/${array_element_com[$((10#$arr_elm_idx))]}
    echo "Start comparison process on #${arr_elm_idx}"
    echo "Control directory ${TARGET_DIR_SCAN}"
    echo "Target directory ${TARGET_DIR_SCAN}"
    # Reg. test for one array element
    # This example is to compare gfs v17 ecflow and rocoto parallel gdas output
    # CONTROL_DIR_SCAN="/lfs/h2/emc/gfstemp/emc.global/comroot/retrov17_01_realtime/gdas.20260429/00"
    # TARGET_DIR_SCAN="/lfs/f2/t2o/ptmp/emc/emc.global/ecflow_gfsv17_rt/ptmp/emc.global/ecflow_gfs/para/com/gdas.20260429/00"
    [ ! -d "${CONTROL_DIR_SCAN}" ]&& exit 7
    [ ! -d "${TARGET_DIR_SCAN}" ]&& exit 7
    cpreq ${HOMEbase}/gfs_parallel_output_verification_cfp.py $DATA
    # Compare netcdf files
    cd ${CONTROL_DIR_SCAN}
    find -L . -type f -name "*.nc" | sed 's|^\./||' | sort > "${DATA}/file_list.txt"
    if [ $(cat ${DATA}/file_list.txt|wc -l) -gt 0 ]; then
      echo "--- Hashing Control Files ---"
      # Use the sorted list to generate hashes.
      cat "${DATA}/file_list.txt" | parallel -k "sha256sum {}" > "${DATA}/control_hashes.data"
      echo "--- Hashing Target Files ---"
      # iterate through the EXACT SAME sorted list for the target directory
      cd "${TARGET_DIR_SCAN}"
      cat "${DATA}/file_list.txt" | parallel -k "
        if [ -f \"{}\" ]; then
          sha256sum \"{}\"
        else
          echo \"MISSING_IN_TARGET  {}\"
        fi
      " > "${DATA}/target_hashes.data"

      echo "--- Generating differ.data ---"
      # Compare the two files. Since both were generated from the same sorted file_list.txt
      # and used -k (keep order) in parallel, they are now perfectly aligned.
      awk 'NR==FNR { hash[$2]=$1; next }
      {
        if ($1 == "MISSING_IN_TARGET") {
            print $2 " (MISSING)"
        } else if (hash[$2] != $1) {
            print $2 " (MISMATCH)"
        }
      }' "${DATA}/control_hashes.data" "${DATA}/target_hashes.data" > "${DATA}/differ.data"

      if [ $(cat ${DATA}/differ.data|wc -l) -gt 0 ]; then
        sed -i 's/ (MISMATCH)//' ${DATA}/differ.data
        cat ${DATA}/differ.data|awk -v CONTROL_DIR_SCAN="$CONTROL_DIR_SCAN" -v TARGET_DIR_SCAN="$TARGET_DIR_SCAN" '{print "echo \"Running: ${MPI_RANK} nccmp -d -B --warn=format "CONTROL_DIR_SCAN"/"$1,TARGET_DIR_SCAN"/"$1,"\" >>","out.${PMI_RANK}.log;","nccmp -d -B --warn=format "CONTROL_DIR_SCAN"/"$1,TARGET_DIR_SCAN"/"$1," >> out.${PMI_RANK}.log 2>&1 || true "}' &> ${DATA}/differ_netcdf_nccmp.sh

        export GAN_MPI_RANK_CT=$(cat ${DATA}/differ_netcdf_nccmp.sh|wc -l)
        export LIN_MPI_RANK_CMD_DATA=${DATA}
        touch ${DATA}/parallel_comparison_cfp.sh
        which python
        python ${DATA}/gfs_parallel_output_verification_cfp.py
        echo "See ${DATA}/parallel_comparison_cfp.o* file to ensure nccmp job complete."
        echo "See ${DATA}/out.*.log for nccmp result."
      fi
    fi
    # Compare grib2 files
    cd ${CONTROL_DIR_SCAN}
    find -L . -type f -name "*.grib2" | sed 's|^\./||' | sort > "${DATA}/grib2_file_list.txt"
    if [ $(cat ${DATA}/grib2_file_list.txt|wc -l) -gt 0 ]; then
      echo "--- Hashing Control Files ---"
      cat "${DATA}/grib2_file_list.txt" | parallel -k "sha256sum {}" > "${DATA}/grib2_control_hashes.data"
      echo "--- Hashing Target Files ---"
      cd "${TARGET_DIR_SCAN}"
      cat "${DATA}/grib2_file_list.txt" | parallel -k "
        if [ -f \"{}\" ]; then
          sha256sum \"{}\"
        else
          echo \"MISSING_IN_TARGET  {}\"
        fi
      " > "${DATA}/grib2_target_hashes.data"
      echo "--- Generating grib2_differ.data ---"
      awk 'NR==FNR { hash[$2]=$1; next }
      {
        if ($1 == "MISSING_IN_TARGET") {
            print $2 " (MISSING)"
        } else if (hash[$2] != $1) {
            print $2 " (MISMATCH)"
        }
      }' "${DATA}/grib2_control_hashes.data" "${DATA}/grib2_target_hashes.data" > "${DATA}/grib2_differ_report.txt"

      echo "See ${DATA}/grib2_differ_report.txt for list of differ files."
    fi
  fi
  echo "Finish comparison process on #${arr_elm_idx}"
done
echo "gfs parallel output verification program successfully completed"
