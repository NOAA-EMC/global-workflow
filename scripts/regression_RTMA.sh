#!/bin/sh

. regression_var.sh

#@ job_name=regression_test
#@ step_name=gsi_rtma_update
#@ error=gsi_rtma_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ bulkxfer=yes
#@ class= dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ queue

#@ step_name=gsi_rtma_update2
#@ error=gsi_rtma_update2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ bulkxfer=yes
#@ class= dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ dependency = (gsi_rtma_update==0)
#@ queue

#@ step_name=gsi_rtma_benchmark
#@ error=gsi_rtma_benchmark.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ bulkxfer=yes
#@ class= dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ dependency = (gsi_rtma_update2==0)
#@ queue

#@ step_name=gsi_rtma_benchmark2
#@ error=gsi_rtma_benchmark2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ bulkxfer=yes
#@ class= dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ dependency = (gsi_rtma_benchmark==0)
#@ queue

#@ step_name=rtma_regression
#@ error=rtma_regression.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class= dev
#@ group= dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ dependency=(gsi_rtma_benchmark2==0)
#@ queue

case $LOADL_STEP_NAME in
  gsi_rtma_update)

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_SYNC_QP=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
exp=$rtma.sub.1node
adate=$adate_regional

# Set path/file for gsi executable
gsipath=/global/save/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${rtma}/${exp}
savdir=$ptmp_loc/outreg/${rtma}/${exp}

# Specify GSI fixed field and data directories.
fixgsi=/nwprod/fix
fixjif=/global/save/wx20rt/2jif/Q1FY09_DA/fix
fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi

datobs=$datobs_rtma/$adate
datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $adate`
cya=`echo $adate | cut -c9-10`
cyg=`echo $gdate | cut -c9-10`
prefixa=nam.t${cya}z
prefixg=na12snmm.t${cyg}z
suffix=tm00.bufr_d

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

GRIDOPTS="JCAP_B=$JCAP_B"

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=1.0,factqmax=1.0,deltim=$DELTIM,
   ndat=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   nhr_assimilation=3,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   as=0.35,0.35,0.35,0.50,0.50,0.80,1.00,1.00,
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=2.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',  dplat(01)=' ', dsis(01)='ps',  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',   dplat(02)=' ', dsis(02)='t',   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',   dplat(03)=' ', dsis(03)='q',   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',  dplat(04)=' ', dsis(04)='uv',  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd', dplat(05)=' ', dsis(05)='spd', dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   uselist  =
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   reject   =
#   slmask   =
#   flt*     =

berror=$fixgsi/rtma_regional_nmm_berror.f77

emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixjif/global_satinfo.txt
satangl=$fixjif/global_satangbias.txt
ozinfo=$fixjif/global_ozinfo.txt
convinfo=$fixjif/global_convinfo.txt

errtable=$fixgsi/rtma_nam_errtable.r3dv
##convinfo=$fixgsi/rtma_regional_convinfo.txt

uselist=$fixgsi/rtma_mesonet_uselist.txt
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable
reject=$fixgsi/rtma_mass_rejectlist_static.txt
slmask=$fixgsi/rtma_ndfd_slmask_umd_grads.dat

flt_chi=$fixgsi/rtma_fltnorm.dat_chi
flt_ist=$fixgsi/rtma_fltnorm.dat_ist
flt_ps=$fixgsi/rtma_fltnorm.dat_ps
flt_lst=$fixgsi/rtma_fltnorm.dat_lst
flt_oz=$fixgsi/rtma_fltnorm.dat_oz
flt_pseudorh=$fixgsi/rtma_fltnorm.dat_pseudorh
flt_psi=$fixgsi/rtma_fltnorm.dat_psi
flt_qw=$fixgsi/rtma_fltnorm.dat_qw
flt_sst=$fixgsi/rtma_fltnorm.dat_sst
flt_t=$fixgsi/rtma_fltnorm.dat_t

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror        ./berror_stats
$ncp $errtable      ./errtable
$ncp $convinfo      ./convinfo
$ncp $errtable      ./errtable
$ncp $uselist       ./mesonetuselist
$ncp $bufrtable     ./prepobs_prep.bufrtable
$ncp $reject        ./mass_rejectlist_tmp
$ncp $slmask        ./ndfd_slmask_umd_grads.dat

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_rtma_update2)

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_SYNC_QP=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
exp=$exp2_rtma_sub_2node
adate=$adate_regional

# Set path/file for gsi executable
gsipath=/global/save/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${rtma}/${exp}
savdir=$ptmp_loc/outreg/${rtma}/${exp}

# Specify GSI fixed field and data directories.
fixgsi=/nwprod/fix
fixjif=/global/save/wx20rt/2jif/Q1FY09_DA/fix
fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi

datobs=$datobs_rtma/$adate
datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $adate`
cya=`echo $adate | cut -c9-10`
cyg=`echo $gdate | cut -c9-10`
prefixa=nam.t${cya}z
prefixg=na12snmm.t${cyg}z
suffix=tm00.bufr_d

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

GRIDOPTS="JCAP_B=$JCAP_B"

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=1.0,factqmax=1.0,deltim=$DELTIM,
   ndat=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   nhr_assimilation=3,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   as=0.35,0.35,0.35,0.50,0.50,0.80,1.00,1.00,
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=2.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',  dplat(01)=' ', dsis(01)='ps',  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',   dplat(02)=' ', dsis(02)='t',   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',   dplat(03)=' ', dsis(03)='q',   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',  dplat(04)=' ', dsis(04)='uv',  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd', dplat(05)=' ', dsis(05)='spd', dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   uselist  =
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   reject   =
#   slmask   =
#   flt*     =

berror=$fixgsi/rtma_regional_nmm_berror.f77

emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixjif/global_satinfo.txt
satangl=$fixjif/global_satangbias.txt
ozinfo=$fixjif/global_ozinfo.txt
convinfo=$fixjif/global_convinfo.txt

errtable=$fixgsi/rtma_nam_errtable.r3dv
##convinfo=$fixgsi/rtma_regional_convinfo.txt

uselist=$fixgsi/rtma_mesonet_uselist.txt
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable
reject=$fixgsi/rtma_mass_rejectlist_static.txt
slmask=$fixgsi/rtma_ndfd_slmask_umd_grads.dat

flt_chi=$fixgsi/rtma_fltnorm.dat_chi
flt_ist=$fixgsi/rtma_fltnorm.dat_ist
flt_ps=$fixgsi/rtma_fltnorm.dat_ps
flt_lst=$fixgsi/rtma_fltnorm.dat_lst
flt_oz=$fixgsi/rtma_fltnorm.dat_oz
flt_pseudorh=$fixgsi/rtma_fltnorm.dat_pseudorh
flt_psi=$fixgsi/rtma_fltnorm.dat_psi
flt_qw=$fixgsi/rtma_fltnorm.dat_qw
flt_sst=$fixgsi/rtma_fltnorm.dat_sst
flt_t=$fixgsi/rtma_fltnorm.dat_t

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror        ./berror_stats
$ncp $errtable      ./errtable
$ncp $convinfo      ./convinfo
$ncp $errtable      ./errtable
$ncp $uselist       ./mesonetuselist
$ncp $bufrtable     ./prepobs_prep.bufrtable
$ncp $reject        ./mass_rejectlist_tmp
$ncp $slmask        ./ndfd_slmask_umd_grads.dat

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_rtma_benchmark)

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_SYNC_QP=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
exp=$exp1_rtma_bench_1node
adate=$adate_regional

# Set path/file for gsi executable
gsipath=/global/save/wx20rt/gsi_anl
##gsiexec=/global/save/wx20ml/q1fy10_new/global_gsi
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${rtma}/${exp}
savdir=$ptmp_loc/outreg/${rtma}/${exp}

# Specify GSI fixed field and data directories.
fixgsi=/nwprod/fix
fixjif=/global/save/wx20rt/2jif/Q1FY09_DA/fix
fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi

datobs=$datobs_rtma/$adate
datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $adate`
cya=`echo $adate | cut -c9-10`
cyg=`echo $gdate | cut -c9-10`
prefixa=nam.t${cya}z
prefixg=na12snmm.t${cyg}z
suffix=tm00.bufr_d

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

GRIDOPTS="JCAP_B=$JCAP_B"

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=1.0,factqmax=1.0,deltim=$DELTIM,
   ndat=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   nhr_assimilation=3,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   as=0.35,0.35,0.35,0.50,0.50,0.80,1.00,1.00,
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=2.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',  dplat(01)=' ', dsis(01)='ps',  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',   dplat(02)=' ', dsis(02)='t',   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',   dplat(03)=' ', dsis(03)='q',   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',  dplat(04)=' ', dsis(04)='uv',  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd', dplat(05)=' ', dsis(05)='spd', dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   uselist  =
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   reject   =
#   slmask   =
#   flt*     =

berror=$fixgsi/rtma_regional_nmm_berror.f77

emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixjif/global_satinfo.txt
satangl=$fixjif/global_satangbias.txt
ozinfo=$fixjif/global_ozinfo.txt
convinfo=$fixjif/global_convinfo.txt

errtable=$fixgsi/rtma_nam_errtable.r3dv
##convinfo=$fixgsi/rtma_regional_convinfo.txt

uselist=$fixgsi/rtma_mesonet_uselist.txt
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable
reject=$fixgsi/rtma_mass_rejectlist_static.txt
slmask=$fixgsi/rtma_ndfd_slmask_umd_grads.dat

flt_chi=$fixgsi/rtma_fltnorm.dat_chi
flt_ist=$fixgsi/rtma_fltnorm.dat_ist
flt_ps=$fixgsi/rtma_fltnorm.dat_ps
flt_lst=$fixgsi/rtma_fltnorm.dat_lst
flt_oz=$fixgsi/rtma_fltnorm.dat_oz
flt_pseudorh=$fixgsi/rtma_fltnorm.dat_pseudorh
flt_psi=$fixgsi/rtma_fltnorm.dat_psi
flt_qw=$fixgsi/rtma_fltnorm.dat_qw
flt_sst=$fixgsi/rtma_fltnorm.dat_sst
flt_t=$fixgsi/rtma_fltnorm.dat_t

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror        ./berror_stats
$ncp $errtable      ./errtable
$ncp $convinfo      ./convinfo
$ncp $errtable      ./errtable
$ncp $uselist       ./mesonetuselist
$ncp $bufrtable     ./prepobs_prep.bufrtable
$ncp $reject        ./mass_rejectlist_tmp
$ncp $slmask        ./ndfd_slmask_umd_grads.dat

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_rtma_benchmark2)

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_SYNC_QP=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
exp=$exp2_rtma_bench_2node
adate=$adate_regional

# Set path/file for gsi executable
gsipath=/global/save/wx20rt/gsi_anl
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${rtma}/${exp}
savdir=$ptmp_loc/outreg/${rtma}/${exp}

# Specify GSI fixed field and data directories.
fixgsi=/nwprod/fix
fixjif=/global/save/wx20rt/2jif/Q1FY09_DA/fix
fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi

datobs=$datobs_rtma/$adate
datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $adate`
cya=`echo $adate | cut -c9-10`
cyg=`echo $gdate | cut -c9-10`
prefixa=nam.t${cya}z
prefixg=na12snmm.t${cyg}z
suffix=tm00.bufr_d

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

GRIDOPTS="JCAP_B=$JCAP_B"

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=1.0,factqmax=1.0,deltim=$DELTIM,
   ndat=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   nhr_assimilation=3,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   as=0.35,0.35,0.35,0.50,0.50,0.80,1.00,1.00,
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=2.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',  dplat(01)=' ', dsis(01)='ps',  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',   dplat(02)=' ', dsis(02)='t',   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',   dplat(03)=' ', dsis(03)='q',   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',  dplat(04)=' ', dsis(04)='uv',  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd', dplat(05)=' ', dsis(05)='spd', dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   uselist  =
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   reject   =
#   slmask   =
#   flt*     =

berror=$fixgsi/rtma_regional_nmm_berror.f77

emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixjif/global_satinfo.txt
satangl=$fixjif/global_satangbias.txt
ozinfo=$fixjif/global_ozinfo.txt
convinfo=$fixjif/global_convinfo.txt

errtable=$fixgsi/rtma_nam_errtable.r3dv
##convinfo=$fixgsi/rtma_regional_convinfo.txt

uselist=$fixgsi/rtma_mesonet_uselist.txt
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable
reject=$fixgsi/rtma_mass_rejectlist_static.txt
slmask=$fixgsi/rtma_ndfd_slmask_umd_grads.dat

flt_chi=$fixgsi/rtma_fltnorm.dat_chi
flt_ist=$fixgsi/rtma_fltnorm.dat_ist
flt_ps=$fixgsi/rtma_fltnorm.dat_ps
flt_lst=$fixgsi/rtma_fltnorm.dat_lst
flt_oz=$fixgsi/rtma_fltnorm.dat_oz
flt_pseudorh=$fixgsi/rtma_fltnorm.dat_pseudorh
flt_psi=$fixgsi/rtma_fltnorm.dat_psi
flt_qw=$fixgsi/rtma_fltnorm.dat_qw
flt_sst=$fixgsi/rtma_fltnorm.dat_sst
flt_t=$fixgsi/rtma_fltnorm.dat_t

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror        ./berror_stats
$ncp $errtable      ./errtable
$ncp $convinfo      ./convinfo
$ncp $errtable      ./errtable
$ncp $uselist       ./mesonetuselist
$ncp $bufrtable     ./prepobs_prep.bufrtable
$ncp $reject        ./mass_rejectlist_tmp
$ncp $slmask        ./ndfd_slmask_umd_grads.dat

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  rtma_regression)

set -ax

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the benchmark run

exp1=$exp1_rtma_sub_1node
exp2=$exp1_rtma_bench_1node
exp3=$exp2_rtma_sub_2node

# Choose global, regional, or RTMA
input=tmpreg_${rtma}

# Name output file
output=$rtma_regression

# Give location of analysis results, and choose location for regression output
savdir=$ptmp_loc/$input
vfydir=$regression_vfydir

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$ptmp_loc/${compare}/$input/${exp1}_vs_${exp2}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=1200
# Dew/Mist=26 GB/16 tasks per node
##maxmem=$((1500000*1))
# Vapor=110 GB/48 tasks per node
##maxmem=$((2300000*1))
# Cirrus=110 GB/32 tasks per node
maxmem=$((3400000*1))

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
for exp in $list; do
   $ncp $savdir/$exp/stdout ./stdout.$exp
   $ncp $savdir/$exp/fort.220 ./fort.220.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'a,b' fort.220.$exp > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.1node.txt with penalty.10node.txt)
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
# (i.e., output from increased number of nodes)

exp1_scale=$exp2_rtma_sub_2node
exp2_scale=$exp2_rtma_bench_2node

# Copy stdout for additional scalability testing
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time' stdout.$exp_scale > runtime.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=10
memdiff=10
scaledif=10

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from benchmark as avg values here

time2=$(awk '{ print $8 }' runtime.$exp2.txt)
time1=$(awk '{ print $8 }' runtime.$exp1.txt)
mem=$(awk '{ print $8 }' memory.$exp2.txt)

timethresh=$((time2 / timedif + time2))
memthresh=$((mem / memdiff + mem))

# Fill time variables with scalability data

if [[ -n $exp2_scale ]]; then

time_scale1=$(awk '{ print $8 }' runtime.$exp1_scale.txt)
time_scale2=$(awk '{ print $8 }' runtime.$exp2_scale.txt)

# Now, figure out difference in time between two runs

scale1=$((time1 / time_scale1))
scale2=$((time2 / time_scale2))

# Calculate maximum allowable deviation for scalability

scalability=$((scale2 / scaledif + scale2))

fi

# Begin applying threshold tests
# First, wall time (both maximum allowable time and max/min allowable deviation)

{

# This part is for the maximum allowable time (operationally)

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $maxtime ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable operational time of '$maxtime' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the maximum allowable operational time of '$maxtime' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# This part is for deviation of wall time

{

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $timethresh ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has
exceeded maximum allowable threshold time of '$timethresh' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the allowable threshold time of '$timethresh' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size (both harware limitation and percent difference)
# First, hardware limitation

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $maxmem ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $memthresh ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable memory of '$memthresh' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, reproducibility

{

if [[ $(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
fi

} >> $output

# Finally, scalability

{

if [[ -z $exp1_scale ]]; then
   echo 'No scalability test will be run due to no additional cases selected'
elif [[ $scale1 -gt $scalability ]]; then
   echo 'The case has failed the scalability regression test.'
   echo 'Please make sure that the same number of nodes/tasks were used.'
else
   echo 'The case has successfully passed the scalability test.'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit
