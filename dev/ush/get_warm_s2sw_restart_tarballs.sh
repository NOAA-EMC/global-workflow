#!/bin/bash
set -eu

# This script untars retro tarballs from HPSS to a local directory.
# Note that this script assumes that there are 80 ensemble members at high res (C384)
# Also, this only works on Gaea C6 right now.
# TODO: Modify this script to accept a different number of ensemble groups.
# TODO: Enable ATM-only and other coupled configurations
# TODO: Extend support to additional systems.
# Usage : get_warm_s2sw_restart_tarballs.sh YYYYMMDDHH HPSS_ROOT_DIR UNTAR_DIR
if [[ $# -ne 4 ]]; then
    echo "Usage: $0 YYYYMMDDHH HPSS_ROOT_DIR UNTAR_DIR HPC_ACCOUNT"
    exit 1
fi
# Cycle is in YYYYMMDDHH format and is passed as an argument.
cycle=$1
# The location on HPSS where the tarballs are stored
hpss_root_dir=$2
# The local directory where the tarballs will be untarred
untar_dir=$3
# HPC account for sbatch jobs
hpc_account=$4
# The previous cycle is 6 hours earlier
pcycle=$(date -d "${cycle:0:8} ${cycle:8:2} -6 hours" +%Y%m%d%H)

hpss_dir="${hpss_root_dir}/${cycle}"
phpss_dir="${hpss_root_dir}/${pcycle}"

if ! mkdir -p "${untar_dir}"; then
    echo "Error: Unable to create untar directory ${untar_dir}"
    exit 1
fi

cd "${untar_dir}"

ptargets=("enkfgdas_restartb_grp1.tar" "enkfgdas_restartb_grp2.tar" "enkfgdas_restartb_grp3.tar" "enkfgdas_restartb_grp4.tar" "enkfgdas_restartb_grp5.tar" "enkfgdas_restartb_grp6.tar" "enkfgdas_restartb_grp7.tar" "enkfgdas_restartb_grp8.tar" "gdas_restartb.tar" "gdasocean_restart.tar" "gdaswave_restart.tar")

targets=("enkfgdas_restarta_grp1.tar" "enkfgdas_restarta_grp2.tar" "enkfgdas_restarta_grp3.tar" "enkfgdas_restarta_grp4.tar" "enkfgdas_restarta_grp5.tar" "enkfgdas_restarta_grp6.tar" "enkfgdas_restarta_grp7.tar" "enkfgdas_restarta_grp8.tar" "gdas_restarta.tar" "gdasocean_analysis.tar")

# This is all specific to Gaea C6
clusters="es"
partition="dtn_f5_f6"
constraint="f6"
qos="hpss"
time="12:00:00"
nodes=1
tasks=1

# Construct a wrapper script in a loop to submit to the sbatch system
for tarball in "${targets[@]}"; do
    sbatch << EOF
#!/bin/bash
#SBATCH --job-name=get_retro_${tarball}
#SBATCH --output=get_retro_${tarball}.out
#SBATCH --partition=${partition}
#SBATCH --constraint=${constraint}
#SBATCH --account=${hpc_account}
#SBATCH --qos=${qos}
#SBATCH --time=${time}
#SBATCH --nodes=${nodes}
#SBATCH --ntasks=${tasks}
#SBATCH --clusters=${clusters}

  echo "Retrieving and untarring ${tarball} from HPSS..."
  # What does set -euo pipefail do?
  # -e: Exit immediately if a command exits with a non-zero status.
  # -u: Treat unset variables as an error when substituting.
  # o: Prevents errors in a pipeline from being masked. If any command in a pipeline fails, that return code will be used as the return code of the whole pipeline.

  set -euo pipefail
  module use /usw/hpss/modulefiles
  module load hsi
  htar -xvf "${hpss_dir}/${tarball}"
  retstat='$?'
  if [[ '${retstat}' -ne 0 ]]; then
    echo "Error: htar command failed with return status '${retstat}'"
    exit '${retstat}'
  fi
EOF
done

# Now do the same for the previous cycle tarballs
for tarball in "${ptargets[@]}"; do
    sbatch << EOF
#!/bin/bash
#SBATCH --job-name=get_retro_${tarball}
#SBATCH --output=get_retro_${tarball}.out
#SBATCH --partition=${partition}
#SBATCH --constraint=${constraint}
#SBATCH --qos=${qos}
#SBATCH --time=${time}
#SBATCH --nodes=${nodes}
#SBATCH --ntasks=${tasks}
#SBATCH --clusters=${clusters}
#SBATCH --mail-type=ALL
#SBATCH --mail-user=david.huber@noaa.gov

  echo "Retrieving and untarring ${tarball} from HPSS..."
  module use /usw/hpss/modulefiles
  module load hsi
  htar -xvf "${phpss_dir}/${tarball}"
  retstat='$?'
  if [[ '${retstat} -ne 0 ]]; then
    echo "Error: htar command failed with return status '${retstat}'"
    exit '${retstat}'
  fi
EOF
done
