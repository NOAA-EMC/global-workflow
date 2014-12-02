set -x

# Set svn source
SVNROOT=https://svnemc.ncep.noaa.gov/projects
SVNGSI=gsi/branches/GDAS-4DEnVar

# Set version numbers
gfs_ver=v1.0.0
gdas_ver=v1.0.0
global_shared_ver=v1.0.0


# Set directory pathnames
target_path=/global/save/$LOGNAME/TEST
target_gfs=$target_path/gfs.$gfs_ver
target_gdas=$target_path/gdas.$gdas_ver
target_shared=$target_path/global_shared.$global_shared_ver


# Create directories
mkdir -p $target_path
mkdir -p $target_gfs
mkdir -p $target_gdas
mkdir -p $target_shared


# Checkout gfs and gdas jobs
mkdir -p $target_gfs/jobs
svn checkout $SVNROOT/$SVNGSI/scripts $target_gfs/jobs
list="arw EnKF exglobal Get gfs global JGDAS hwrf link llsub multi nmm refactor regress rtma run sub"
for type in $list; do
   rm -rf $target_gfs/jobs/*${type}*
done

mkdir -p $target_gdas/jobs
svn checkout $SVNROOT/$SVNGSI/scripts $target_gdas/jobs
mv $target_gdas/jobs/EnKF/scripts_ncep/JGDAS* $target_gdas/jobs
list="arw EnKF exglobal Get gfs global JGFS hwrf link llsub multi nmm refactor regress rtma run sub"
for type in $list; do
   rm -rf $target_gdas/jobs/*${type}*
done


# Checkout global_shared fix
mkdir -p $target_shared/fix
svn checkout $SVNROOT/$SVNGSI/fix $target_shared/fix
list="aod arw bufrtab cmaq gsd hwrf ndas nems rtma example ozonly global_ch4 global_co2 global_co_esrl reg_test global_n2o Little_Endian nam prepobs_prep rtma rap"
for type in $list; do
   rm -rf $target_shared/fix/*${type}*
done


# Checkout global_shared scripts
mkdir -p $target_shared/scripts
svn checkout $SVNROOT/$SVNGSI/scripts $target_shared/scripts
mv $target_shared/scripts/EnKF/scripts_ncep/exglobal* $target_shared/scripts
list="arw EnKF Get gfs JGDAS JGFS hwrf link llsub multi nmm refactor regress rtma run sub"
for type in $list; do
   rm -rf $target_shared/scripts/*${type}*
done
rm -rf $target_shared/scripts/global_*


# Checkout global_shared sorc
mkdir -p $target_shared/sorc/gsi.fd
svn checkout $SVNROOT/$SVNGSI/sorc $target_shared/sorc/gsi.fd

list="adderrspec_nmcmeth_spec.fd getsfcensmeanp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd"
for code in $list; do
   mkdir -p $target_shared/sorc/$code
   svn checkout $SVNROOT/$SVNGSI/util/EnKF/gfs/src/$code $target_shared/sorc/$code
done


# Checkout global_shared ush
mkdir -p $target_shared/ush
svn checkout $SVNROOT/$SVNGSI/scripts $target_shared/ush
list="arw EnKF Get global JGDAS JGFS hwrf link llsub multi nmm refactor regress rtma run_arw rungsi sub"
for type in $list; do
   rm -rf $target_shared/ush/*${type}*
done
