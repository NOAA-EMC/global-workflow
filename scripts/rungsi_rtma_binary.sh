#!/bin/sh

# NOTE:  To ensure reproducible results, must use same number of
#        MPI tasks AND nodes for each run.  blocking=unlimited
#        leads to roundoff differences in mpi_allreduce.

## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=gsi_rtma_binary
#@ error=gsi_rtma_binary.e$(jobid)
#@ job_type=parallel
#@ network.MPI=csss,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ class= dev
#@ group=devonprod
#@ account_no = RDAS-MTN
#@ wall_clock_limit = 1:00:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ queue

set -x


# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes


# Set environment variables for threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=64000000"


# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes


# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=svn
adate=2005061700



# Set path/file variables
basedir=/global/save
gsiexec=$basedir/wx20rt/svn/sorc/gsi.fd/gsi_anl
fixpath=$basedir/wx20rt/svn/fix
datobs=$basedir/wx20rt/gsi_anl/cases/regional/rtma_binary/$adate
datges=$datobs
utilpath=$basedir/wx20rt/svn/util/sorc



# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export DELTIM=1200


# Set runtime and save directories
tmpdir=/ptmp/$LOGNAME/tmpreg/rtma_binary/$exp
savdir=/ptmp/$LOGNAME/outreg/rtma_binary/$exp


# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=YES
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefixa=nam.t${hha}z
prefixg=nam.t${hhg}z
suffix=tm00.bufr_d



# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


# Make gsi namelist
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
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
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=1.,nord_f2a=4
 /
 &JCOPTS
   jcterm=.false.,jcdivt=.false.,bamp_ext1=1.0e6,bamp_ext2=1.0e6,
   bamp_int1=1.0e5, bamp_int2=1.0e4
 /
 &STRONGOPTS
   jcstrong=.false.,nstrong=1,nvmodes_keep=20,period_max=3.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,
   vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=15.0,dmesh(2)=25.0,dmesh(3)=20.0,dmesh(4)=16.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /
EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fixpath/rtma_nmm_berror.f77
errtable=$fixpath/rtma_errtable.r3dv
convinfo=$fixpath/rtma_convinfo.txt
mesonet=$fixpath/rtma_mesonet_uselist.txt


# Only need this file for single obs test
bufrtable=$fixpath/rtma_prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=/nwprod/fix/bufrtab.012


# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror    ./berror_stats
$ncp $convinfo  ./convinfo
$ncp $errtable  ./errtable
$ncp $mesonet   ./mesonetuselist
$ncp $bufrtable ./prepobs_prep.bufrtable

$ncp $bftab_sst ./bftab_sstphr


# Copy observational data to $tmpdir
$ncp $datobs/prepbufr.nam.meso_no.$adate ./prepbufr


# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datges/wrfges.nam.meso_no.$adate   ./wrf_inout

cp wrf_inout wrf_ges


# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?


# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${adate}



# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

loops="01 03"
for loop in $loops; do

# Collect diagnostic files for obs types (groups) below
   list="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16"
for type in $list; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ count -gt 0 ]]; then
         cat ${tmpdir}/dir.*/${type}_${loop}* > ${tmpdir}/diag_${type}_${loop}
         compress ${tmpdir}/diag_${type}_${loop}
         if [[ "$loop" = "01" ]]; then
            mv ${tmpdir}/diag_${type}_${loop}.Z $tmpdir/diag_${type}_ges.${adate}.Z
            ${ncp:-cp} $tmpdir/diag_${type}_ges.${adate}.Z $savdir/diag_${type}_ges.${adate}.Z
         elif [[ "$loop" = "03" ]]; then
            mv ${tmpdir}/diag_${type}_${loop}.Z $tmpdir/diag_${type}_anl.${adate}.Z
            ${ncp:-cp} $tmpdir/diag_${type}_anl.${adate}.Z $savdir/diag_${type}_anl.${adate}.Z
         else
            mv ${tmpdir}/diag_${type}_${loop}.Z $tmpdir/diag_${type}_${loop}.${adate}.Z
            ${ncp:-cp} $tmpdir/diag_${type}_${loop}.${adate}.Z $savdir/diag_${type}_${loop}.${adate}.Z
         fi
      fi
   done
done


# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir/*
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit
