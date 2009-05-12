#!/bin/sh

# NOTE:  To ensure reproducible results, must use same number of
#        MPI tasks AND nodes for each run.  blocking=unlimited
#        leads to roundoff differences in mpi_allreduce.

## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=gsi_nmm
#@ error=gsi_nmm.e$(jobid)
#@ job_type=parallel
#@ network.MPI=csss,shared,us
#@ node = 3
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ bulkxfer=yes
#@ class= dev
#@ group=devonprod
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 1:00:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ queue

set -x


# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes


# Set environment variables for no threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export XLSMPOPTS="parthds=1:stack=128000000"


# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes


# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=update
adate=2007122000


# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
io_format=binary
##io_format=netcdf

NETCDF=.false.
FORMAT=binary
if [[ "$io_format" = "netcdf" ]];
   NETCDF=.true.
   FORMAT=netcdf
fi


# Set path/file for gsi executable
gsipath=/global/save/wx****/gsi_anl
gsiexec=$gsipath/sorc/update/gsi_anl


# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export DELTIM=1200


# Set runtime and save directories
tmpdir=/ptmp/wx****/tmpreg_nmm_$FORMAT/$exp
savdir=/ptmp/wx****/outreg/nmm_$FORMAT/$exp


# Specify GSI fixed field and data directories.
fixgsi=$gsipath/fix/update
fixcrtm=/nwprod/fix/crtm_gfs

datobs=$gsipath/cases/regional/nmm_$FORMAT/$adate
datges=$datobs


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
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   niter_no_qc(1)=250,niter_no_qc(2)=500,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=51,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=$NETCDF,
 /
 &BKGERR
   as=1.0,1.0,0.5 ,0.7,0.7,0.5,1.0,1.0,
   hzscl=0.373,0.746,1.50,
   vs=1.0,bw=0.,fstat=.true., 
   bkgv_flowdep=.false.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
   jcterm=.false.,jcdivt=.false.,bamp_ext1=1.0e6,bamp_ext2=1.0e6,
   bamp_int1=1.0e5, bamp_int2=1.0e4
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,perturb_obs=.false.,oberrflg=.false.,
   perturb_fact=0.1,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',    dsis(01)='ps',                dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',    dsis(02)='t',                 dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',    dsis(03)='q',                 dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',    dsis(04)='pw',                dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',    dsis(05)='uv',                dval(05)=1.0,  dthin(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',    dsis(06)='spd',               dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',    dsis(07)='dw',                dval(07)=1.0,  dthin(07)=0,
   dfile(08)='radarbufr'  dtype(08)='rw',        dplat(08)=' ',    dsis(08)='rw',                dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',    dsis(09)='sst',               dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsbufr',   dtype(10)='gps_ref',   dplat(10)=' ',    dsis(10)='gps_ref',           dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmibufr',  dtype(11)='pcp_ssmi',  dplat(11)='dmsp', dsis(11)='pcp_ssmi',          dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmibufr',   dtype(12)='pcp_tmi',   dplat(12)='trmm', dsis(12)='pcp_tmi',           dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',  dsis(13)='sbuv8_n16',         dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',  dsis(14)='sbuv8_n17',         dval(14)=1.0,  dthin(14)=0,
   dfile(15)='omi',       dtype(15)='omi',       dplat(15)='aura', dsis(15)='omi_aura',          dval(15)=1.0,  dthin(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',  dsis(16)='hirs2_n14',         dval(16)=6.0,  dthin(16)=1,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',  dsis(17)='hirs3_n16',         dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',  dsis(18)='hirs3_n17',         dval(18)=6.0,  dthin(18)=1,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',  dsis(19)='hirs4_n18',         dval(19)=1.0,  dthin(19)=1,
   dfile(20)='gsndrbufr', dtype(20)='sndr',      dplat(20)='g11',  dsis(20)='sndr_g11',          dval(20)=6.0,  dthin(20)=1,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g12',  dsis(21)='sndr_g12',          dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gimgrbufr', dtype(22)='goes_img',  dplat(22)='g11',  dsis(22)='imgr_g11',          dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g12',  dsis(23)='imgr_g12',          dval(23)=0.0,  dthin(23)=1,
   dfile(24)='airsbufr',  dtype(24)='airs',      dplat(24)='aqua', dsis(24)='airs281SUBSET_aqua',dval(24)=20.0, dthin(24)=1,
   dfile(25)='ssmisbufr', dtype(25)='ssmis_env', dplat(25)='f16',  dsis(25)='ssmis_f16',         dval(25)=1.0,  dthin(25)=4,
   dfile(26)='amsuabufr', dtype(26)='amsua',     dplat(26)='n15',  dsis(26)='amsua_n15',         dval(26)=10.0, dthin(26)=2,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n16',  dsis(27)='amsua_n16',         dval(27)=1.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n17',  dsis(28)='amsua_n17',         dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n18',  dsis(29)='amsua_n18',         dval(29)=10.0, dthin(29)=2,
   dfile(30)='airsbufr',  dtype(30)='amsua',     dplat(30)='aqua', dsis(30)='amsua_aqua',        dval(30)=10.0, dthin(30)=2,
   dfile(31)='amsubbufr', dtype(31)='amsub',     dplat(31)='n15',  dsis(31)='amsub_n15',         dval(31)=3.0,  dthin(31)=3,
   dfile(32)='amsubbufr', dtype(32)='amsub',     dplat(32)='n16',  dsis(32)='amsub_n16',         dval(32)=3.0,  dthin(32)=3,
   dfile(33)='amsubbufr', dtype(33)='amsub',     dplat(33)='n17',  dsis(33)='amsub_n17',         dval(33)=3.0,  dthin(33)=3,
   dfile(34)='mhsbufr',   dtype(34)='mhs',       dplat(34)='n18',  dsis(34)='mhs_n18',           dval(34)=3.0,  dthin(34)=3,
   dfile(35)='ssmitbufr', dtype(35)='ssmi',      dplat(35)='f13',  dsis(35)='ssmi_f13',          dval(35)=1.0,  dthin(35)=4,
   dfile(36)='ssmitbufr', dtype(36)='ssmi',      dplat(36)='f14',  dsis(36)='ssmi_f14',          dval(36)=1.0,  dthin(36)=4,
   dfile(37)='ssmitbufr', dtype(37)='ssmi',      dplat(37)='f15',  dsis(37)='ssmi_f15',          dval(37)=1.0,  dthin(37)=4,
   dfile(38)='amsrebufr', dtype(38)='amsre_low', dplat(38)='aqua', dsis(38)='amsre_aqua',        dval(38)=1.0,  dthin(38)=4,
   dfile(39)='amsrebufr', dtype(39)='amsre_mid', dplat(39)='aqua', dsis(39)='amsre_aqua',        dval(39)=1.0,  dthin(39)=4,
   dfile(40)='amsrebufr', dtype(40)='amsre_hig', dplat(40)='aqua', dsis(40)='amsre_aqua',        dval(40)=1.0,  dthin(40)=4,
   dfile(41)='ssmisbufr', dtype(41)='ssmis_las', dplat(41)='f16',  dsis(41)='ssmis_f16',         dval(41)=1.0,  dthin(41)=4,
   dfile(42)='ssmisbufr', dtype(42)='ssmis_uas', dplat(42)='f16',  dsis(42)='ssmis_f16',         dval(42)=1.0,  dthin(42)=4,
   dfile(43)='ssmisbufr', dtype(43)='ssmis_img', dplat(43)='f16',  dsis(43)='ssmis_f16',         dval(43)=1.0,  dthin(43)=4,
   dfile(44)='gsnd1bufr', dtype(44)='sndrd3',    dplat(44)='g11',  dsis(44)='sndrD3_g11',        dval(44)=1.5,  dthin(44)=5,
   dfile(45)='gsnd1bufr', dtype(45)='sndrd4',    dplat(45)='g11',  dsis(45)='sndrD4_g11',        dval(45)=1.5,  dthin(45)=5,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g12',  dsis(46)='sndrD1_g12',        dval(46)=1.5,  dthin(46)=5,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g12',  dsis(47)='sndrD2_g12',        dval(47)=1.5,  dthin(47)=5,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g12',  dsis(48)='sndrD3_g12',        dval(48)=1.5,  dthin(48)=5,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g12',  dsis(49)='sndrD4_g12',        dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd1',    dplat(50)='g11',  dsis(50)='sndrD1_g11',        dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd2',    dplat(51)='g11',  dsis(51)='sndrD2_g11',        dval(51)=1.5,  dthin(51)=5,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
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

berror=$fixgsi/nam_nmm_berror.f77
emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixgsi/nam_satinfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
errtable=$fixgsi/nam_errtable.r3dv
convinfo=$fixgsi/nam_convinfo.txt


# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=/nwprod/fix/bufrtab.012


# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/No_AC/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefixa}.prepbufr.tm00.nr   ./prepbufr
$ncp $datobs/${prefixa}.goesnd.${suffix}   ./gsndrbufr
$ncp $datobs/${prefixa}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefixa}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefixa}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefixa}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefixa}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefixa}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefixa}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefixa}.airs.${suffix}     ./airsbufr
$ncp $datobs/${prefixa}.radwnd.${suffix}   ./radarbufr
$ncp $datobs/${prefixa}.nexrad.${suffix}   ./l2rwbufr


# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datges/${prefixg}.abias              ./satbias_in
$ncp $datges/${prefixg}.satang             ./satbias_angle
$ncp $datges/${prefixa}.wrfrst_d01.ges     ./wrf_inout
cp wrf_inout wrf_ges


# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?


# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${adate}

if [[ "$io_format" = "binary" ]]; then
   rr2gg=$gsipath/util/wrf_nmm_binary_grads.fd/wrf_nmm_binary_grads
   $rr2gg wrf_inout anl
   $rr2gg wrf_ges   ges
   $ncp anl.dat $savdir/
   $ncp anl.ctl $savdir/
   $ncp ges.dat $savdir/
   $ncp ges.ctl $savdir/
fi


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

cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

# Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua goes_img_g08 goes_img_g10 goes_img_g11 goes_img_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done



# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit
