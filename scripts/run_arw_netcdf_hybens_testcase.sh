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
   iguess=-1,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                  dval    dthin  dsfcalc
   prepbufr       ps          null        ps                    1.0      0      0
   prepbufr       t           null        t                     1.0      0      0
   prepbufr       q           null        q                     1.0      0      0
   prepbufr       uv          null        uv                    1.0      0      0
   prepbufr       spd         null        spd                   1.0      0      0
   radarbufr      rw          null        rw                    1.0      0      0
   prepbufr       dw          null        dw                    1.0      0      0
   prepbufr       sst         null        sst                   1.0      0      0
   prepbufr       pw          null        pw                    1.0      0      0
   gpsrobufr      $gps_dtype  null        gps                   1.0      0      0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi              1.0     -1      0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi               1.0     -1      0
   sbuvbufr       sbuv2       n16         sbuv8_n16             1.0      0      0
   sbuvbufr       sbuv2       n17         sbuv8_n17             1.0      0      0
   sbuvbufr       sbuv2       n18         sbuv8_n18             1.0      0      0
   omi            omi         aura        omi_aura              1.0      6      0
   hirs2bufr      hirs2       n14         hirs2_n14             6.0      1      1
   hirs3bufr      hirs3       n16         hirs3_n16             0.0      1      1
   hirs3bufr      hirs3       n17         hirs3_n17             6.0      1      1
   hirs4bufr      hirs4       n18         hirs4_n18             0.0      1      1
   hirs4bufr      hirs4       metop-a     hirs4_metop-a         6.0      1      1
   gsndrbufr      sndr        g11         sndr_g11              0.0      1      0
   gsndrbufr      sndr        g12         sndr_g12              0.0      1      0
   gimgrbufr      goes_img    g11         imgr_g11              0.0      1      0
   gimgrbufr      goes_img    g12         imgr_g12              0.0      1      0
   airsbufr       airs        aqua        airs281SUBSET_aqua   20.0      1      1
   msubufr        msu         n14         msu_n14               2.0      2      1
   amsuabufr      amsua       n15         amsua_n15            10.0      2      1
   amsuabufr      amsua       n16         amsua_n16             0.0      2      1
   amsuabufr      amsua       n17         amsua_n17             0.0      2      1
   amsuabufr      amsua       n18         amsua_n18            10.0      2      1
   amsuabufr      amsua       metop-a     amsua_metop-a        10.0      2      1
   airsbufr       amsua       aqua        amsua_aqua            5.0      2      1
   amsubbufr      amsub       n15         amsub_n15             3.0      3      1
   amsubbufr      amsub       n16         amsub_n16             3.0      3      1
   amsubbufr      amsub       n17         amsub_n17             3.0      3      1
   mhsbufr        mhs         n18         mhs_n18               3.0      3      1
   mhsbufr        mhs         metop-a     mhs_metop-a           3.0      3      1
   ssmitbufr      ssmi        f13         ssmi_f13              0.0      4      0
   ssmitbufr      ssmi        f14         ssmi_f14              0.0      4      0
   ssmitbufr      ssmi        f15         ssmi_f15              0.0      4      0
   amsrebufr      amsre_low   aqua        amsre_aqua            0.0      4      1
   amsrebufr      amsre_mid   aqua        amsre_aqua            0.0      4      1
   amsrebufr      amsre_hig   aqua        amsre_aqua            0.0      4      1
   ssmisbufr      ssmis       f16         ssmis_f16             0.0      4      1
   gsnd1bufr      sndrd1      g12         sndrD1_g12            1.5      5      0
   gsnd1bufr      sndrd2      g12         sndrD2_g12            1.5      5      0
   gsnd1bufr      sndrd3      g12         sndrD3_g12            1.5      5      0
   gsnd1bufr      sndrd4      g12         sndrD4_g12            1.5      5      0
   gsnd1bufr      sndrd1      g11         sndrD1_g11            1.5      5      0
   gsnd1bufr      sndrd2      g11         sndrD2_g11            1.5      5      0
   gsnd1bufr      sndrd3      g11         sndrD3_g11            1.5      5      0
   gsnd1bufr      sndrd4      g11         sndrD4_g11            1.5      5      0
   gsnd1bufr      sndrd1      g13         sndrD1_g13            1.5      5      0
   gsnd1bufr      sndrd2      g13         sndrD2_g13            1.5      5      0
   gsnd1bufr      sndrd3      g13         sndrD3_g13            1.5      5      0
   gsnd1bufr      sndrd4      g13         sndrD4_g13            1.5      5      0
   iasibufr       iasi        metop-a     iasi616_metop-a      20.0      1      1
   gomebufr       gome        metop-a     gome_metop-a          1.0      6      0
::
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
