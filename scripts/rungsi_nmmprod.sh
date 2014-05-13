#/bin/sh
#
#@ job_name=gsi_nmm
#@ error=gsi_nmm.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ total_tasks = 32
#@ node = 4
#@ node_usage = not_shared
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class=dev
#@ group=dev
#@ account_no = NAM-T2O
#@ wall_clock_limit = 0:30:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ queue


set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
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

# What is the endianness of the machine? (default is Big_Endian)
export endianness=Big_Endian

# Choose type of GPSRO data to assimilate (default is gps_ref)
export gps_dtype="gps_ref"

# Set experiment name and analysis date
ndate=/nwprod/util/exec/ndate

adate=2010112900

exp=${USER}_nmm.$adate.$gps_dtype

TM=00
TM2=03


# Set resoltion and other dependent parameters
export JCAP=62
export NLAT=180
export NLON=360
export LEVS=60
export DELTIM=1200

tmpdir=/ptmp/$USER/nam_gsi_oper/${exp}
savdir=/ptmp/$USER/outreg/nam_gsi_oper/${exp}


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
#FIXnam=/global/save/$USER/mlueken/fix
FIXnam=/u/wx20xs/home/gsi/xsu/fix
CRTMnam=/global/save/wx20ml/CRTM_REL-2.1.3/Big_Endian

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$FIXnam}
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
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
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
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',
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
   satwndbufr     uv          null        uv                    1.0      0      0
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
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /
EOF

anavinfo=$FIXnam/anavinfo_ndas_binary
berror=$FIXnam/$endianness/nam_nmmstat_na.gcv

emiscoef_IRwater=$CRTMnam/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$CRTMnam/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$CRTMnam/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$CRTMnam/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$CRTMnam/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$CRTMnam/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$CRTMnam/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$CRTMnam/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$CRTMnam/FASTEM5.MWwater.EmisCoeff.bin
aercoef=$CRTMnam/AerosolCoeff.bin
cldcoef=$CRTMnam/CloudCoeff.bin
satinfo=$FIXnam/nam_regional_satinfo.txt
scaninfo=$FIXnam/global_scaninfo.txt
satangl=$FIXnam/nam_global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
pcpinfo=$FIXnam/nam_global_pcpinfo.txt
ozinfo=$FIXnam/nam_global_ozinfo.txt
errtable=$FIXnam/nam_errtable.r3dv
convinfo=$FIXnam/nam_regional_convinfo.txt
mesonetuselist=$FIXnam/nam_mesonet_uselist.txt

cp $anavinfo ./anavinfo
cp $berror   ./berror_stats
cp $errtable ./errtable
cp $emiscoef ./EmisCoeff.bin
cp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
cp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
cp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
cp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
cp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
cp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
cp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
cp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
cp $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
cp $aercoef  ./AerosolCoeff.bin
cp $cldcoef  ./CloudCoeff.bin
cp $satangl  ./satbias_angle
cp $satinfo  ./satinfo
cp $scaninfo ./scaninfo
cp $pcpinfo  ./pcpinfo
cp $ozinfo   ./ozinfo
cp $convinfo ./convinfo
cp $mesonetuselist ./mesonetuselist


# Copy executable and fixed files to $tmpdir
gsiexec=/u/wx20xs/home/gsi/xsu/src/global_gsi
cp $gsiexec  ./gsi.x

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $CRTMnam/${file}.SpcCoeff.bin ./
   $ncp $CRTMnam/${file}.TauCoeff.bin ./
done
set -x

CDATE=$adate
PDY=`echo $CDATE | cut -c1-8`
CYC=`echo $CDATE | cut -c9-10`

if [ $TM = 00 ] ; then
   datdir=/com/nam/prod/nam.$PDY
   cp $datdir/nam.t${CYC}z.prepbufr.tm${TM}       ./prepbufr
   cp $datdir/nam.t${CYC}z.satwnd.tm${TM}.bufr_d       ./satwndbufr
   cp $datdir/nam.t${CYC}z.1bhrs3.tm${TM}.bufr_d  ./hirs3bufr
   cp $datdir/nam.t${CYC}z.1bamub.tm${TM}.bufr_d  ./amsubbufr
   cp $datdir/nam.t${CYC}z.1bamua.tm${TM}.bufr_d  ./amsuabufr
   cp $datdir/nam.t${CYC}z.1bmhs.tm${TM}.bufr_d   ./mhsbufr
   cp $datdir/nam.t${CYC}z.1bhrs4.tm${TM}.bufr_d  ./hirs4bufr
   cp $datdir/nam.t${CYC}z.radwnd.tm${TM}.bufr_d  ./radarbufr
   cp $datdir/nam.t${CYC}z.nexrad.tm${TM}.bufr_d  ./l2rwbufr
   cp $datdir/nam.t${CYC}z.goesfv.tm${TM}.bufr_d  ./gsnd1bufr
   cp $datdir/nam.t${CYC}z.airsev.tm${TM}.bufr_d  ./airsbufr
else
   datdir=/com/nam/prod/ndas.$PDY
   cp $datdir/ndas.t${CYC}z.prepbufr.tm${TM}       ./prepbufr
   cp $datdir/ndas.t${CYC}z.satwnd.tm${TM}.bufr_d  ./satwndbufr
   cp $datdir/ndas.t${CYC}z.1bhrs3.tm${TM}.bufr_d  ./hirs3bufr
   cp $datdir/ndas.t${CYC}z.1bamub.tm${TM}.bufr_d  ./amsubbufr
   cp $datdir/ndas.t${CYC}z.1bamua.tm${TM}.bufr_d  ./amsuabufr
   cp $datdir/ndas.t${CYC}z.1bmhs.tm${TM}.bufr_d   ./mhsbufr
   cp $datdir/ndas.t${CYC}z.1bhrs4.tm${TM}.bufr_d  ./hirs4bufr
   cp $datdir/ndas.t${CYC}z.radwnd.tm${TM}.bufr_d  ./radarbufr
   cp $datdir/ndas.t${CYC}z.nexrad.tm${TM}.bufr_d  ./l2rwbufr
   cp $datdir/ndas.t${CYC}z.goesfv.tm${TM}.bufr_d  ./gsnd1bufr
   cp $datdir/ndas.t${CYC}z.airsev.tm${TM}.bufr_d  ./airsbufr
fi

cp /com/nam/prod/ndas.${PDY}/ndas.t${CYC}z.satang.tm${TM2}  ./satbias_angle
cp /com/nam/prod/ndas.${PDY}/ndas.t${CYC}z.satbias.tm${TM2} ./satbias_in
cp /com/nam/prod/nam.${PDY}/nam.t${CYC}z.wrfrst_d01.ges     ./wrf_inout



# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit
