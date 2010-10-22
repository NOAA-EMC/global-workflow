#!/bin/sh

## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=gsi_global
#@ error=gsi_global.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node_usage = not_shared
#@ tasks_per_node = 32
#@ node = 1
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class= dev
#@ group= devonprod
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:20:00
#@ startdate = 07/06/09 10:15
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


# Set experiment name and analysis date
adate=2010052500
exp=${USER}_global.$adate

# Set path/file for gsi executable
gsiexec=/global/save/$USER/svn1/src/global_gsi



# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=$JCAP


# Set runtime and save directories
tmpdir=/ptmp/$USER/tmp${JCAP}_sigmap/${exp}
savdir=/ptmp/$USER/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field
fixgsi=/global/save/$USER/mlueken/fix

# Use with CRTM REL-2.0
fixcrtm=/global/save/wx20ml/CRTM_REL-2.0/CRTM_Coefficients



# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "878" ]]; then
   export LONA=1760
   export LATA=880
   export DELTIM=400
   export resol=1
elif [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=120
   export resol=1
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "254" ]]; then
   export LONA=512
   export LATA=256
   export DELTIM=300
   export resol=1
elif [[ "$JCAP" = "190" ]]; then
   export LONA=576
   export LATA=288
   export DELTIM=600
   export resol=1
elif [[ "$JCAP" = "126" ]]; then
   export LONA=384
   export LATA=190
   export DELTIM=600
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT_A=$((${LATA}+2))
export NLON_A=$LONA


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=/com/gfs/prod/gdas.$adate0
datges=/com/gfs/prod/gdas.$gdate0




# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# Make gsi namelist

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
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
LAGDATA=""
HYBRID_ENSEMBLE=""
RAPIDREFRESH_CLDSURF=""
SINGLEOB=""


cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=$IGEN,factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=64,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT_A,NLON=$NLON_A,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=2.5e7,
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                 dval(01)=1.0, dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                  dval(02)=1.0, dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                  dval(03)=1.0, dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                 dval(04)=1.0, dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                 dval(05)=1.0, dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',                dval(06)=1.0, dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',                 dval(07)=1.0, dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='radarbufr', dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',                 dval(08)=1.0, dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',                dval(09)=1.0, dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',            dval(10)=1.0, dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',           dval(11)=1.0, dthin(11)=-1,dsfcalc(11)=0,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',            dval(12)=1.0, dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',          dval(13)=1.0, dthin(13)=0, dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',          dval(14)=1.0, dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',          dval(15)=1.0, dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',          dval(16)=6.0, dthin(16)=1, dsfcalc(16)=0,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',          dval(17)=0.0, dthin(17)=1, dsfcalc(17)=0,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',          dval(18)=6.0, dthin(18)=1, dsfcalc(18)=0,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',          dval(19)=0.0, dthin(19)=1, dsfcalc(19)=0,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',      dval(20)=6.0, dthin(20)=1, dsfcalc(20)=0,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',           dval(21)=0.0, dthin(21)=1, dsfcalc(21)=0,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',           dval(22)=0.0, dthin(22)=1, dsfcalc(22)=0,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',           dval(23)=0.0, dthin(23)=1, dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',           dval(24)=0.0, dthin(24)=1, dsfcalc(24)=0,
   dfile(25)='airsbufr',  dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua', dval(25)=20.0,dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='msubufr',   dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',            dval(26)=2.0, dthin(26)=2, dsfcalc(26)=0,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',          dval(27)=10.0,dthin(27)=2, dsfcalc(27)=0,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',          dval(28)=0.0, dthin(28)=2, dsfcalc(28)=0,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',          dval(29)=0.0, dthin(29)=2, dsfcalc(29)=0,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',          dval(30)=10.0,dthin(30)=2, dsfcalc(30)=0,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',      dval(31)=10.0,dthin(31)=2, dsfcalc(31)=0,
   dfile(32)='airsbufr',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',         dval(32)=5.0, dthin(32)=2, dsfcalc(32)=0,
   dfile(33)='amsubbufr', dtype(33)='amsub',     dplat(33)='n15',     dsis(33)='amsub_n15',          dval(33)=3.0, dthin(33)=3, dsfcalc(33)=0,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n16',     dsis(34)='amsub_n16',          dval(34)=3.0, dthin(34)=3, dsfcalc(34)=0,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n17',     dsis(35)='amsub_n17',          dval(35)=3.0, dthin(35)=3, dsfcalc(35)=0,
   dfile(36)='mhsbufr',   dtype(36)='mhs',       dplat(36)='n18',     dsis(36)='mhs_n18',            dval(36)=3.0, dthin(36)=3, dsfcalc(36)=0,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='metop-a', dsis(37)='mhs_metop-a',        dval(37)=3.0, dthin(37)=3, dsfcalc(37)=0,
   dfile(38)='ssmitbufr', dtype(38)='ssmi',      dplat(38)='f13',     dsis(38)='ssmi_f13',           dval(38)=0.0, dthin(38)=4, dsfcalc(38)=0,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f14',     dsis(39)='ssmi_f14',           dval(39)=0.0, dthin(39)=4, dsfcalc(39)=0,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f15',     dsis(40)='ssmi_f15',           dval(40)=0.0, dthin(40)=4, dsfcalc(40)=0,
   dfile(41)='amsrebufr', dtype(41)='amsre_low', dplat(41)='aqua',    dsis(41)='amsre_aqua',         dval(41)=0.0, dthin(41)=4, dsfcalc(41)=0,
   dfile(42)='amsrebufr', dtype(42)='amsre_mid', dplat(42)='aqua',    dsis(42)='amsre_aqua',         dval(42)=0.0, dthin(42)=4, dsfcalc(42)=0,
   dfile(43)='amsrebufr', dtype(43)='amsre_hig', dplat(43)='aqua',    dsis(43)='amsre_aqua',         dval(43)=0.0, dthin(43)=4, dsfcalc(43)=0,
   dfile(44)='ssmisbufr', dtype(44)='ssmis',     dplat(44)='f16',     dsis(44)='ssmis_f16',          dval(44)=0.0, dthin(44)=4, dsfcalc(44)=0,
   dfile(45)='gsnd1bufr', dtype(45)='sndrd1',    dplat(45)='g12',     dsis(45)='sndrD1_g12',         dval(45)=1.5, dthin(45)=5, dsfcalc(45)=0,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd2',    dplat(46)='g12',     dsis(46)='sndrD2_g12',         dval(46)=1.5, dthin(46)=5, dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd3',    dplat(47)='g12',     dsis(47)='sndrD3_g12',         dval(47)=1.5, dthin(47)=5, dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd4',    dplat(48)='g12',     dsis(48)='sndrD4_g12',         dval(48)=1.5, dthin(48)=5, dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd1',    dplat(49)='g11',     dsis(49)='sndrD1_g11',         dval(49)=1.5, dthin(49)=5, dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd2',    dplat(50)='g11',     dsis(50)='sndrD2_g11',         dval(50)=1.5, dthin(50)=5, dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd3',    dplat(51)='g11',     dsis(51)='sndrD3_g11',         dval(51)=1.5, dthin(51)=5, dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g11',     dsis(52)='sndrD4_g11',         dval(52)=1.5, dthin(52)=5, dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd1',    dplat(53)='g13',     dsis(53)='sndrD1_g13',         dval(53)=1.5, dthin(53)=5, dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd2',    dplat(54)='g13',     dsis(54)='sndrD2_g13',         dval(54)=1.5, dthin(54)=5, dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd3',    dplat(55)='g13',     dsis(55)='sndrD3_g13',         dval(55)=1.5, dthin(55)=5, dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd4',    dplat(56)='g13',     dsis(56)='sndrD4_g13',         dval(56)=1.5, dthin(56)=5, dsfcalc(56)=0,
   dfile(57)='iasibufr',  dtype(57)='iasi',      dplat(57)='metop-a', dsis(57)='iasi616_metop-a',    dval(57)=20.0,dthin(57)=1, dsfcalc(57)=0,
   dfile(58)='gomebufr',  dtype(58)='gome',      dplat(58)='metop-a', dsis(58)='gome_metop-a',       dval(58)=1.0, dthin(58)=6, dsfcalc(58)=0,
   dfile(59)='omibufr',   dtype(59)='omi',       dplat(59)='aura',    dsis(59)='omi_aura',           dval(59)=1.0, dthin(59)=6, dsfcalc(59)=0,
   dfile(60)='sbuvbufr',  dtype(60)='sbuv2',     dplat(60)='n19',     dsis(60)='sbuv8_n19',          dval(60)=1.0, dthin(60)=0, dsfcalc(60)=0,
   dfile(61)='hirs4bufr', dtype(61)='hirs4',     dplat(61)='n19',     dsis(61)='hirs4_n19',          dval(61)=6.0, dthin(61)=1, dsfcalc(61)=0,
   dfile(62)='amsuabufr', dtype(62)='amsua',     dplat(62)='n19',     dsis(62)='amsua_n19',          dval(62)=10.0,dthin(62)=2, dsfcalc(62)=0,
   dfile(63)='mhsbufr',   dtype(63)='mhs',       dplat(63)='n19',     dsis(63)='mhs_n19',            dval(63)=3.0, dthin(63)=3, dsfcalc(63)=0,
   dfile(64)='tcvitl'     dtype(64)='tcp',       dplat(64)=' ',       dsis(64)='tcp',                dval(64)=1.0, dthin(64)=0, dsfcalc(64)=0,
   $OBSINPUT
 /
 &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
   $RAPIDREFRESH_CLDSURF
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fixgsi/global_anavinfo.l64.txt
if [[ "$JCAP" -ne "878" ]]; then
   berror=$fixgsi/global_berror.l${LEVS}y${NLAT_A}.f77.gcv
elif [[ "$JCAP" = "878" ]]; then
   berror=$fixgsi/global_berror.l${LEVS}y${NLAT_A}.f77_SL878.gcv
fi
emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fix_file/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt

errtable=$fixgsi/prepobs_errtable.global


# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012


# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
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
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done


# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./gomebufr
$ncp $datobs/${prefix_obs}.omi.${suffix}      ./omibufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.esamua.${suffix}   ./amsuabufrears
$ncp $datobs/${prefix_obs}.esamub.${suffix}   ./amsubbufrears
$ncp $datobs/${prefix_obs}.eshrs3.${suffix}   ./hirs3bufrears
$ncp $datobs/${prefix_obs}.syndata.tcvitals.tm00 ./tcvitl

$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr


# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

if [[ "$JCAP" = "382" ]]; then
   $ncp $datges/${prefix_sfc}.bf03               ./sfcf03
   $ncp $datges/${prefix_sfc}.bf06               ./sfcf06
   $ncp $datges/${prefix_sfc}.bf09               ./sfcf09

   $ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
   $ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
   $ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09
elif [[ "$JCAP" -ne "382" ]]; then
# Use chgres to change resolution from T382/64L to T62/64L

# first copy required files to working directory

   $ncp $datges/gdas1.t${hhg}z.bf03               ./gdas1.t${hhg}z.bf03
   $ncp $datges/gdas1.t${hhg}z.bf06               ./gdas1.t${hhg}z.bf06
   $ncp $datges/gdas1.t${hhg}z.bf09               ./gdas1.t${hhg}z.bf09

   $ncp $datobs/gdas1.t${hha}z.sgm3prep           ./gdas1.t${hha}z.sgm3prep
   $ncp $datobs/gdas1.t${hha}z.sgesprep           ./gdas1.t${hha}z.sgesprep
   $ncp $datobs/gdas1.t${hha}z.sgp3prep           ./gdas1.t${hha}z.sgp3prep

   export SIGLEVEL=/nwprod/fix/global_hyblev.l64.txt
   SDATE=`echo $adate | cut -c1-8`
   HH=`echo $adate | cut -c9-10`

   export JCAP=$JCAP
   export LEVS=$LEVS
   export LONB=$LONA
   export LATB=$LATA

   export VERBOSE="YES"

   # Operational chgres for operational sigio
   export DATA=$tmpdir
   export CHGRESSH=/nwprod/ush/global_chgres.sh

   list="sgm3prep sgesprep sgp3prep"
   for fhr in $list; do
      export SIGINP=$tmpdir/gdas1.t${hha}z.${fhr}
      export SFCINP=/dev/null
      export SIGOUT=$tmpdir/gdas2.t${hha}z.${fhr}
      export SFCOUT=
      export GFSOUT=
      $CHGRESSH
   done

   list="bf03 bf06 bf09"
   for fhr in $list; do
      export SIGINP=/dev/null
      export SFCINP=$tmpdir/gdas1.t${hhg}z.${fhr}
      export SIGOUT=/dev/null
      export SFCOUT=$tmpdir/gdas2.t${hhg}z.${fhr}
      export GFSOUT=
      $CHGRESSH
   done

   mv gdas2.t${hhg}z.bf03       sfcf03
   mv gdas2.t${hhg}z.bf06       sfcf06
   mv gdas2.t${hhg}z.bf09       sfcf09

   mv gdas2.t${hha}z.sgm3prep   sigf03
   mv gdas2.t${hha}z.sgesprep   sigf06
   mv gdas2.t${hha}z.sgp3prep   sigf09

   rm -f gdas1.t${hhg}z.bf03
   rm -f gdas1.t${hhg}z.bf06
   rm -f gdas1.t${hhg}z.bf09

   rm -f gdas1.t${hha}z.sgm3prep
   rm -f gdas1.t${hha}z.sgesprep
   rm -f gdas1.t${hha}z.sgp3prep

   rm -f chgres.out.grd
   rm -f fort.11
   rm -f fort.51

fi

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit

# Save output
mkdir -p $savdir

##cat stdout fort.2* > $savdir/stdout.anl.${adate}
##$ncp siganl          $savdir/siganl.${adate}
##$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
##$ncp satbias_out     $savdir/biascr.${adate}
##$ncp sfcf06          $savdir/sfcf06.${gdate}
##$ncp sigf06          $savdir/sigf06.${gdate}

##ss2gg=/global/save/wx20mi/bin/ss2gg
##$ss2gg siganl siganl.bin siganl.ctl 4 $NLON_A $NLAT_A
##$ss2gg sigf06 sigges.bin sigges.ctl 4 $NLON_A $NLAT_A

##exit

##sfc2gg=/u/wx20mi/bin/sfc2gg
##$sfc2gg sfcanl.gsi sfcanl.bin sfcanl.ctl
##$sfc2gg sfcf06     sfcges.bin sfcges.ctl

##$ncp s*anl.bin $savdir/
##$ncp s*anl.ctl $savdir/
##$ncp s*ges.bin $savdir/
##$ncp s*ges.ctl $savdir/

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


echo "Time before diagnostic loop is `date` "
cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "



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
