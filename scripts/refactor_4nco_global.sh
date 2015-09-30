#!/bin/ksh

set -x

options=${1:-""}
source_path=${2:-$LOGNAME/source_path}
target_path=${3:-$LOGNAME/target_path}
gfs_ver=${4:-v6.1.0}
gdas_ver=${5:-v6.1.0}
global_shared_ver=${6:-v6.1.0}


# Set target directory pathnames
target_gfs=$target_path/gfs.$gfs_ver
target_gdas=$target_path/gdas.$gdas_ver
target_shared=$target_path/global_shared.$global_shared_ver


# Set commands
if [[ "$options" = "svn" ]]; then
  svnmkdir="svn mkdir"
  svnmkdirp="svn mkdir --parents"
  svncopy="svn copy"
elif [[ "$options" = "copy" ]]; then
   svnmkdir="mkdir"
   svnmkdirp="mkdir -p"
   svncopy="cp"
elif [[ "$options" = "link" ]]; then
   svnmkdir="mkdir"
   svnmkdirp="mkdir -p"
   svncopy="ln -fs"
else
   echo " "
   echo "INVALID OPTION $options   Valid options are svn, copy, link"
   exit
fi


# Create base directory
$svnmkdir  $target_path
$svnmkdirp $target_shared



# Create and populate gfs directory
$svnmkdirp $target_gfs
$svnmkdirp $target_gfs/jobs
flist="JGFS_ANALYSIS"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_gfs/jobs/
done



# Create and populate gdas directory
$svnmkdirp $target_gdas


# Create and populate gdas fix
$svnmkdirp $target_gdas/fix
flist="global_hybens_smoothinfo.l64.txt"
for file in $flist; do
   $svncopy $source_path/fix/$file $target_gdas/fix/
done


# Create and populate gdas jobs
$svnmkdirp $target_gdas/jobs
flist="JGDAS_ANALYSIS_HIGH"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_gdas/jobs/
done
flist="JGDAS_ENKF_FCST JGDAS_ENKF_INFLATE_RECENTER JGDAS_ENKF_INNOVATE_OBS JGDAS_ENKF_POST JGDAS_ENKF_SELECT_OBS JGDAS_ENKF_UPDATE"
for file in $flist; do
   $svncopy $source_path/scripts/EnKF/scripts_ncep/$file $target_gdas/jobs/
done


# Create and populate gdas scripts
$svnmkdirp $target_gdas/scripts
flist="exglobal_enkf_innovate_obs.sh.ecf"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_gdas/scripts/
done
flist="exglobal_enkf_fcst.sh.ecf exglobal_enkf_inflate_recenter.sh.ecf exglobal_enkf_post.sh.ecf exglobal_enkf_update.sh.ecf"
for file in $flist; do
   $svncopy $source_path/scripts/EnKF/scripts_ncep/$file $target_gdas/scripts/
done


# Create and populate gdas sorc
$svnmkdirp $target_gdas/sorc/enkf_update.fd
flist="configure make Make README"
for file in $flist; do
   $svncopy $source_path/src/${file}* $target_gdas/sorc/enkf_update.fd/
done
flist=".c .f90 .F90 .H"
for file in $flist; do
   $svncopy $source_path/src/*${file} $target_gdas/sorc/enkf_update.fd/
done
$svnmkdirp $target_gdas/sorc/enkf_update.fd/enkf
$svncopy $source_path/src/enkf/* $target_gdas/sorc/enkf_update.fd/enkf/

flist="adderrspec_nmcmeth_spec.fd getsfcensmeanp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd"
for code in $flist; do
   $svnmkdirp $target_gdas/sorc/$code
   $svncopy $source_path/util/EnKF/gfs/src/$code/* $target_gdas/sorc/$code/
done

flist="build_gdas.sh"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_gdas/sorc/build.sh
done


# Create and populate gdas ush
$svnmkdirp $target_gdas/ush
$svncopy $source_path/scripts/gfs_truncate_enkf.sh $target_gdas/ush/



# Create and populate global_shared directory
$svnmkdirp $target_shared


# Create and populate global_shared fix
$svnmkdir $target_shared/fix
flist="atms_beamwidth.txt global_aeroinfo.txt global_anavinfo.l64.txt global_convinfo.txt global_convinfo.txt.2014041318 global_convinfo.txt.3hr_satwnd global_hybens_locinfo.l64.txt global_insituinfo.txt global_ozinfo.txt global_pcpinfo.txt global_satangbias.txt global_satinfo.txt global_satinfo.txt.2013041500 global_satinfo.txt.2013070318 global_satinfo.txt.2013082012 global_satinfo.txt.2014092612 global_satinfo.txt.2014101512 global_satinfo.txt.2015011412 global_satinfo.txt.2015070218 global_scaninfo.txt prepobs_errtable.global prepobs_errtable.global.3hr_satwnd"
for file in $flist; do
   $svncopy $source_path/fix/$file $target_shared/fix/
done
$svnmkdirp $target_shared/fix/Big_Endian
flist="global_berror.l64y1154.f77 global_berror.l64y130.f77 global_berror.l64y192.f77 global_berror.l64y258.f77 global_berror.l64y290.f77 global_berror.l64y386.f77 global_berror.l64y578.f77 global_berror.l64y882.f77 global_berror.l64y96.f77"
for file in $flist; do
   $svncopy $source_path/fix/Big_Endian/$file $target_shared/fix/Big_Endian/
done


# Create and populate global_shared scripts
$svnmkdirp $target_shared/scripts
flist="exglobal_analysis.sh.ecf"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_shared/scripts/
done


# Create and populate global_shared sorc
$svnmkdirp $target_shared/sorc/gsi.fd
flist="configure make Make README"
for file in $flist; do
   $svncopy $source_path/src/${file}* $target_shared/sorc/gsi.fd/
done
flist=".c .f90 .F90 .H"
for file in $flist; do
   $svncopy $source_path/src/*${file} $target_shared/sorc/gsi.fd/
done

flist="build_global_shared.sh"
for file in $flist; do
   $svncopy $source_path/scripts/$file $target_shared/sorc/build.sh
done
