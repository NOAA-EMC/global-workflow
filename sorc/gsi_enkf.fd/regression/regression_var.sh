#!/bin/sh
# It is now possible to run all regression tests (except RTMA) using the hybrid ensemble option with
# internally generated random ensemble perturbations.  No script changes are required.
#  To run with hybrid ensemble option on, change HYBENS_GLOBAL and/or HYBENS_REGIONAL from "false" to "true".
#  These are located at the end of this script.

if [ "$#" = 7 ] ; then
  export basedir=$1
  export builddir=$2
  export gsisrc=$3
  export gsiexec_updat=$4
  export enkfexec_updat=$5
  export gsiexec_contrl=$6
  export enkfexec_contrl=$7
  export fixgsi="$gsisrc/fix"
  export scripts="$gsisrc/regression"
  export modulefiles="$gsisrc/modulefiles"
  export ush="$gsisrc/ush"
  export cmaketest="true"
  export clean="false"
  dir_root="${builddir%/*}"
  export ptmpName="${dir_root##*/}"
else
  # Name of the branch being tested
  updat="XXXXXXXX"
  contrl="XXXXXXXX"
  export cmaketest="false"
  export clean="false"
  export ptmpName=""
fi

# Determine the machine
if [[ -d /scratch1 ]]; then # Hera
  export machine="Hera"
elif [[ -d /mnt/lfs4 || -d /jetmon || -d /mnt/lfs5 ]]; then # Jet
  export machine="Jet"
elif [[ -d /discover ]]; then # NCCS Discover
  export machine="Discover"
elif [[ -d /gpfs/f5 ]]; then # GaeaC5
  export machine="gaeac5"
elif [[ -d /gpfs/f6 ]]; then # GaeaC6
  export machine="gaeac6"
elif [[ -d /data/prod ]]; then # S4
  export machine="S4"
elif [[ -d /work ]]; then # Orion or Hercules
  mount=$(findmnt -n -o SOURCE /home)
  if [[ ${mount} =~ "hercules" ]]; then
    export machine="Hercules"
  else
    export machine="Orion"
  fi
elif [[ -d /lfs/h2 ]]; then # wcoss2 or acorn
  if [[ $(hostname -f) =~ "alogin" ]]; then
    export machine="acorn"
  else
    export machine="wcoss2"
  fi
fi
echo "Running Regression Tests on '$machine'";

case $machine in
  gaeac5)
    export queue="normal"
    export group="ufs-ard"
    export noscrub="/gpfs/f5/${group}/scratch/${USER}/$LOGNAME/gsi_tmp/noscrub"
    export ptmp="/gpfs/f5/${group}/scratch/${USER}/$LOGNAME/gsi_tmp/ptmp"
    export casesdir="/gpfs/f5/ufs-ard/world-shared/GSI_data/CASES/regtest"

    export check_resource="no"
    export accnt="ufs-ard"
  ;;
  gaeac6)
    export queue="normal"
    export group="bil-fire8"
    export noscrub="/gpfs/f6/${group}/scratch/${USER}/${LOGNAME}/gsi_tmp/noscrub"
    export ptmp="/gpfs/f6/${group}/scratch/${USER}/${LOGNAME}/gsi_tmp/ptmp"
    export casesdir="/gpfs/f6/bil-fire8/world-shared/GSI_data/CASES/regtest"

    export check_resource="no"
    export accnt="bil-fire8"
  ;;
  wcoss2 | acorn)
      export local_or_default="${local_or_default:-/lfs/h2/emc/da/noscrub/$LOGNAME}"
      if [ -d $local_or_default ]; then
          export noscrub="$local_or_default/noscrub"
      elif [ -d /lfs/h2/emc/global/noscrub/$LOGNAME ]; then
          export noscrub="/lfs/h2/emc/global/noscrub/$LOGNAME/noscrub"
      fi

      export queue="${queue:-dev}"
      export group="${group:-global}"
      if [[ "$cmaketest" = "false" ]]; then
	  export basedir="/lfs/h2/emc/da/noscrub/$LOGNAME/gsi"
      fi
      export ptmp="${ptmp:-/lfs/h2/emc/ptmp/$LOGNAME/$ptmpName}"

      export casesdir="/lfs/h2/emc/da/noscrub/russ.treadon/CASES/regtest"

      export check_resource="no"
      export accnt="${accnt:-GFS-DEV}"
  ;;      
  Orion | Hercules)
      export local_or_default="${local_or_default:-/work/noaa/da/$LOGNAME}"
      if [ -d $local_or_default ]; then
         export noscrub="$local_or_default/noscrub"
      elif [ -d /work/noaa/global/$LOGNAME ]; then
         export noscrub="/work/noaa/global/$LOGNAME/noscrub"
      fi

      export queue="${queue:-batch}"

      if [[ "${machine}" == "Orion" ]]; then
         export partition="${partition:-orion}"
      else
         export partition="${partition:-hercules}"
      fi

      export group="${group:-global}"
      if [[ "$cmaketest" = "false" ]]; then
         export basedir="/work/noaa/da/$LOGNAME/gsi"
      fi
      export ptmp="${ptmp:-/work/noaa/stmp/$LOGNAME/$ptmpName}"

      export casesdir="/work/noaa/da/rtreadon/CASES/regtest"

      export check_resource="no"
      export accnt="${accnt:-da-cpu}"
  ;;      
  Hera)

    export local_or_default="${local_or_default:-/scratch1/NCEPDEV/da/$LOGNAME}"
    if [ -d $local_or_default ]; then
      export noscrub="$local_or_default/noscrub"
    elif [ -d /scratch1/NCEPDEV/global/$LOGNAME ]; then
      export noscrub="/scratch1/NCEPDEV/global/$LOGNAME/noscrub"
     elif [ -d /scratch2/BMC/gsienkf/$LOGNAME ]; then
      export noscrub="/scratch2/BMC/gsienkf/$LOGNAME"
    fi

    export group="${group:-global}"
    export queue="${queue:-batch}"
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/scratch1/NCEPDEV/da/$LOGNAME/git/gsi"
    fi

    export ptmp="${ptmp:-/scratch1/NCEPDEV/stmp2/$LOGNAME/$ptmpName}"

    export casesdir="/scratch1/NCEPDEV/da/Russ.Treadon/CASES/regtest"

    export check_resource="no"
    export accnt="${accnt:-da-cpu}"

    #  On Hera, there are no scrubbers to remove old contents from stmp* directories.
    #  After completion of regression tests, will remove the regression test subdirecories
    export clean=".false."
  ;;
  Jet)

    export noscrub=/lfs5/NESDIS/nesdis-rdo2/$LOGNAME/noscrub
    export ptmp=/lfs5/NESDIS/nesdis-rdo2/$LOGNAME/ptmp
    export casesdir="/lfs5/NESDIS/nesdis-rdo2/David.Huber/save/CASES/regtest"
    export check_resource="no"
    export accnt="nesdis-rdo2"

    export group="global"
    export queue="batch"
    if [[ "$cmaketest" = "false" ]]; then
      export basedir="/lfs5/NESDIS/nesdis-rdo2/$LOGNAME/save/git/gsi"
    fi

    #  On Jet, there are no scrubbers to remove old contents from stmp* directories.
    #  After completion of regression tests, will remove the regression test subdirecories
    export clean=".true."
  ;;
  Discover)
    if [[ "$cmaketest" = "false" ]]; then
        echo "Regression tests on Discover need to be run via ctest"
        exit 1
    fi
    export ptmp=$basedir
    export ptmp=$basedir
    export noscrub=$basedir
    export casesdir="/discover/nobackup/projects/gmao/obsdev/wrmccart/NCEP_regression/CASES"
    export check_resource="no"
    export accnt="g0613"
    export queue="compute"
    export clean=".false."
  ;;
  *)
    echo "Regression tests are not setup on '$machine', ABORT!"
    exit 1
  ;;
esac

# We are dealing with *which* endian files
export endianness="Big_Endian"

# Paths to tmpdir and savedir base on ptmp
export tmpdir="$ptmp"
export savdir="$ptmp"

# Variables with the same values are defined below.

# Default resolution
export JCAP="62"

# Case Study analysis dates
export global_adate="2024022300"
export rtma_adate="2020022420"
export rrfs_enkf_adate="2023061012"
export rrfs_3denvar_rdasens_adate="2023061012"
export hafs_envar_adate="2020082512"

# Paths for canned case data.
export global_data="$casesdir/gfs/prod"
export rtma_obs="$casesdir/regional/rtma_binary/$rtma_adate"
export rtma_ges="$casesdir/regional/rtma_binary/$rtma_adate"
export rrfs_enkf_diag="$casesdir/regional/rrfs/$rrfs_enkf_adate/diag"
export rrfs_enkf_ges="$casesdir/regional/rrfs/$rrfs_enkf_adate/ens"
export rrfs_3denvar_rdasens_obs="$casesdir/regional/rrfs/$rrfs_3denvar_rdasens_adate/obs"
export rrfs_3denvar_rdasens_ges="$casesdir/regional/rrfs/$rrfs_3denvar_rdasens_adate/ges"
export rrfs_3denvar_rdasens_ens="$casesdir/regional/rrfs/$rrfs_3denvar_rdasens_adate/ens"
export hafs_envar_obs="$casesdir/regional/hafs_RTdata/$hafs_envar_adate/obs"
export hafs_envar_ges="$casesdir/regional/hafs_RTdata/$hafs_envar_adate/ges"
export hafs_envar_ens="$casesdir/regional/hafs_RTdata/$hafs_envar_adate/ens"


# Define type of GPSRO data to be assimilated (refractivity or bending angle)
export gps_dtype="gps_bnd"

# Regression vfydir
export regression_vfydir="$noscrub/regression"

# Define debug variable - If you want to run the debug tests, set this variable to .true.  Default is .false.
export debug=".false."

# Define parameters for global_4denvar
export minimization="lanczos"  # If "lanczos", use sqrtb lanczos minimization algorithm.  Otherwise use "pcgsoi".
export nhr_obsbin="6"          # Time window for observation binning.  Use "6" for 3d4dvar test.  Otherwise use "1"

# Define parameters for hybrid ensemble option test.
# (default is set to false, so no hybrid ensemble option test.)

export HYBENS_GLOBAL=".false."
export ENSEMBLE_SIZE_GLOBAL="10"
export HYBENS_UV_GLOBAL=".true."
export BETA_S0_GLOBAL="0.5"
export HYBENS_HOR_SCALE_GLOBAL="1500"
export HYBENS_VER_SCALE_GLOBAL="20"
export GENERATE_ENS_GLOBAL=".true."
export HYBENS_ANISO_GLOBAL=".false."

export HYBENS_REGIONAL=".false."
export ENSEMBLE_SIZE_REGIONAL="10"
export HYBENS_UV_REGIONAL=".true."
export BETA_S0_REGIONAL="0.5"
export HYBENS_HOR_SCALE_REGIONAL="1500"
export HYBENS_VER_SCALE_REGIONAL="20"
export GENERATE_ENS_REGIONAL=".true."
export HYBENS_ANISO_REGIONAL=".false."
export NLON_ENS_REGIONAL="0"
export NLAT_ENS_REGIONAL="0"
export JCAP_ENS_REGIONAL="0"
export JCAP_ENS_TEST_REGIONAL="0"

# Toggle EnKF update code bias correction flag: lupd_satbiasc
# TRUE  =        compute and update radiance bias correction
# FALSE = do NOT compute or  update radiance bias correction
# default is FALSE (as done in NCEP operations)
export lupd_satbiasc=".false."
