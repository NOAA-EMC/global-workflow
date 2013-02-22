#!/bin/sh

#  test script to recreate A. Mizzi 1 point and analysis test cases with arw model using 10 member ensemble:  

#   set SINGLE_OBS_TEST =  .true. for single obs case
#       SINGLE_OBS_TEST = .false. for analysis with some conventional data

#@ job_name=regression_debug_test
#@ error=arw_netcdf_hybens_testcase.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class=debug
#@ group=dev
#@ account_no = RDAS-MTN
#@ wall_clock_limit = 0:05:00
#@ notification=error
#@ restart=no
#@ queue

. regression_var.sh

set -x

#
# SET HYBRID NAMELIST VARIABLES
      export HYBRID_ENS=.true.
      export HYBRID_ENS_UV=.true.
      export ENSEMBLE_SIZE=10
      export ENSEMBLE_GEN=.false.
      export BETA1_INV=.5
      export HYBRID_HOR_SCALE=1500
      export HYBRID_VER_SCALE=20
      export HYBRID_ANISO=.false.
      export HYBRID_NLON=139
      export HYBRID_NLAT=93
      export HYBRID_JCAP=0
      export JCAP_B=0

      export DATE=2007081506
      export NLAT=93
      export NLON=139
      export NLEV=56
      export JCAP=0

# GSI SINGLE OB DATA
      export SINGLE_OBS_TEST=.false.
      export MAGINNO=1.
      export MAGOERR=1.
      export OB_TYPE="'"t"'"
      export OB_LAT=13.5
      export OB_LON=283.
      export OB_PRES=850.
      export OB_DATE=${DATE}
# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set analysis date
#adate=$adate_regional

# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
##io_format=binary
io_format=netcdf

if [[ "$io_format" = "binary" ]]; then
   NETCDF=.false.
   FORMAT=binary
elif [[ "$io_format" = "netcdf" ]]; then
   NETCDF=.true.
   FORMAT=netcdf
else
   echo "***ERRROR*** INVALID io_format = $io_format"
   exit
fi

# Set experiment name
if [[ "$SINGLE_OBS_TEST" = ".true." ]]; then
   exp=arw_netcdf_hybens_1pt_testcase.$gps_dtype
elif [[ "$SINGLE_OBS_TEST" = ".false." ]]; then
   exp=arw_netcdf_hybens_analysis_testcase.$gps_dtype
fi

# Set path/file for gsi executable
gsiexec=$updat

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${arw_netcdf}/${exp}
savdir=$ptmp_loc/outreg/${arw_netcdf}/${exp}

# Specify GSI fixed field and data directories.


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
#sdate=`echo $adate |cut -c1-8`
#odate=`$ndate +18 $adate`
#hha=`echo $adate | cut -c9-10`
#hho=`echo $odate | cut -c9-10`
#prefixo=ndas.t${hho}z
#prefixa=ndas.t${hha}z
#suffix=tm06.bufr_d

datobs=/meso/noscrub/wx23dp/ARW_GSI_HYBENS_TESTCASE/INPUT_FILES
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

set +x
# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./global_co2_data.txt
        while [ $yyyy -ge 1957 ] ;do
                co2=$co2dir/global_co2historicaldata_$yyyy.txt
                if [ -s $co2 ] ; then
                        $ncp $co2 ./global_co2_data.txt
                break
                fi
                ((yyyy-=1))
        done
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
set -x

#  nlon_ens=${HYBRID_NLON},
#  nlat_ens=${HYBRID_NLAT},
#  jcap_ens=${HYBRID_JCAP},
#  jcap_ens_test=${HYBRID_JCAP},

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=59,iguess=-1,
   oneobtest=${SINGLE_OBS_TEST},
   retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,print_diag_pcg=.true.,
   use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
 /
 &GRIDOPTS
   JCAP=${JCAP},JCAP_B=${JCAP_B},NLAT=${NLAT},NLON=${NLON},nsig=${NLEV},
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=$NETCDF,
   nlat_regional=${NLAT},
   nlon_regional=${NLON},
 /
 &BKGERR
   hzscl=0.373,0.746,1.50,
   vs=1.0,bw=0.,fstat=.true.,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=0,tlnmc_type=3,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=1.0,  dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=1.0,  dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='radarbufr', dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',                  dval(06)=1.0,  dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=1.0,  dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',                 dval(08)=1.0,  dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='prepbufr',  dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',                  dval(09)=1.0,  dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='$gps_dtype',   dplat(10)=' ',         dsis(10)='gps',             dval(10)=1.0,  dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=1.0,  dthin(11)=-1,dsfcalc(11)=0,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=1.0,  dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=1.0,  dthin(13)=0, dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=1.0,  dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=1.0,  dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='omi',       dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',            dval(16)=1.0,  dthin(16)=6, dsfcalc(16)=0,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',           dval(17)=6.0,  dthin(17)=1, dsfcalc(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1, dsfcalc(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=6.0,  dthin(19)=1, dsfcalc(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1, dsfcalc(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1, dsfcalc(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1, dsfcalc(22)=0,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1, dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1, dsfcalc(24)=0,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1, dsfcalc(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=2.0,  dthin(27)=2, dsfcalc(27)=1,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=10.0, dthin(28)=2, dsfcalc(28)=1,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=2, dsfcalc(29)=1,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=2, dsfcalc(30)=1,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=10.0, dthin(31)=2, dsfcalc(31)=1,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2, dsfcalc(32)=1,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=5.0,  dthin(33)=2, dsfcalc(33)=1,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=3.0,  dthin(34)=3, dsfcalc(34)=1,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=3.0,  dthin(35)=3, dsfcalc(35)=1,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=3.0,  dthin(36)=3, dsfcalc(36)=1,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=3.0,  dthin(37)=3, dsfcalc(37)=1,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=3.0,  dthin(38)=3, dsfcalc(38)=1,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=4, dsfcalc(39)=0,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=4, dsfcalc(40)=0,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=4, dsfcalc(41)=0,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=4, dsfcalc(42)=1,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=4, dsfcalc(43)=1,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=4, dsfcalc(44)=1,
   dfile(45)='ssmisbufr', dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=4, dsfcalc(45)=1,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=1.5,  dthin(46)=5, dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=1.5,  dthin(47)=5, dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=1.5,  dthin(48)=5, dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=1.5,  dthin(49)=5, dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=1.5,  dthin(50)=5, dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=1.5,  dthin(51)=5, dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=1.5,  dthin(52)=5, dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=1.5,  dthin(53)=5, dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=1.5,  dthin(54)=5, dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=1.5,  dthin(55)=5, dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=1.5,  dthin(56)=5, dsfcalc(56)=0,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=1.5,  dthin(57)=5, dsfcalc(57)=0,
   dfile(58)='iasibufr',  dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi616_metop-a',     dval(58)=20.0, dthin(58)=1, dsfcalc(58)=1,
   dfile(59)='gomebufr',  dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=1.0,  dthin(59)=6, dsfcalc(59)=0,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${HYBRID_ENS},
   n_ens=${ENSEMBLE_SIZE},
   uv_hyb_ens=${HYBRID_ENS_UV},
   beta1_inv=${BETA1_INV},
   s_ens_h=${HYBRID_HOR_SCALE},
   s_ens_v=${HYBRID_VER_SCALE},
   generate_ens=${ENSEMBLE_GEN},
   aniso_a_en=${HYBRID_ANISO},
   nlon_ens=0,
   nlat_ens=0,
   jcap_ens=0,
   jcap_ens_test=0,
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=${MAGINNO},
   magoberr=${MAGOERR},
   oneob_type=${OB_TYPE},
   oblat=${OB_LAT},
   oblon=${OB_LON},
   obpres=${OB_PRES},
   obdattim=${OB_DATE},
   obhourset=0.,
   pctswitch=.false.,
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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_arw_netcdf
if [[ "$io_format" = "binary" ]]; then
   berror=$fix_file/$endianness/nam_glb_berror.f77.gcv
elif [[ "$io_format" = "netcdf" ]]; then
   berror=$fix_file/$endianness/nam_glb_berror.f77.gcv
fi
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
scaninfo=$fix_file/global_scaninfo.txt
satangl=$fix_file/nam_global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
pcpinfo=$fix_file/nam_global_pcpinfo.txt
ozinfo=$fix_file/nam_global_ozinfo.txt
errtable=$fix_file/nam_errtable.r3dv
convinfo=$fix_file/nam_regional_convinfo.txt
mesonetuselist=$fix_file/nam_mesonet_uselist.txt

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
set +x
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $crtm_coef/SpcCoeff/Big_Endian/${file}.SpcCoeff.bin ./
   $ncp $crtm_coef/TauCoeff/Big_Endian/${file}.TauCoeff.bin ./
done
set -x

# Copy observational data to $tmpdir
#$ncp $datobs/${prefixo}.prepbufr.tm06   ./prepbufr
$ncp $datobs/ob.bufr   ./prepbufr
#$ncp $datobs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
#$ncp $datobs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
#$ncp $datobs/${prefixo}.1bamua.$suffix  ./amsuabufr
#$ncp $datobs/${prefixo}.1bamub.$suffix  ./amsubbufr
#$ncp $datobs/${prefixo}.1bmhs.$suffix   ./mhsbufr
#$ncp $datobs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
#$ncp $datobs/${prefixo}.airsev.$suffix  ./airsbufr
#$ncp $datobs/${prefixo}.radwnd.$suffix  ./radarbufr
#if [[ "$io_format" = "netcdf" ]]; then
#   $ncp $datobs/${prefixo}.nexrad.$suffix  ./l2rwbufr
#fi

# Copy bias correction and atmosphere/surface guess files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
#$ncp $datobs/${prefixo}.satbias.tm06      ./satbias_in
>./satbias_in
#$ncp $datobs/${prefixo}.satang.tm06        ./satbias_angle
#if [[ "$io_format" = "binary" ]]; then
#   $ncp $datges/wrfinput_d01_arw_binary        ./wrf_inout
#elif [[ "$io_format" = "netcdf" ]]; then
#   $ncp $datges/wrfout_d01_2008-05-11_12:00:00 ./wrf_inout
#fi
$ncp $datges/wrf_input ./wrf_inout
cp wrf_inout wrf_ges
#
# COPY ENSEMBLE MEMBERS TO WORK DIRECTORY
#
# Cycling start files
      export ENSEMBLE_FILE=wrf_mass_forecast
      let MEM=1
      while [[ $MEM -le $ENSEMBLE_SIZE ]]; do
         export CMEM=e$MEM
         if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
         if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi
         cp ${datges}/${ENSEMBLE_FILE}.${CMEM} ./wrf_mass_forecast.${CMEM}
         let MEM=$MEM+1
      done

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" = "0" ]]; then
   cd $regression_vfydir
   {
    echo
    echo $exp "debug test has passed"
   } >> $arw_netcdf_regression
fi

exit
