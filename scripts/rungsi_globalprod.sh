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
#@ class= jcsda
#@ group= jcsda
#@ account_no = JCSDA014-RES
#@ wall_clock_limit = 0:20:00
#@ startdate = 07/06/09 10:15
#@ notification=error
#@ queue

set -x

# Set default top-level directory
if [ -d /global ]; then
  TOPDIR=/global   # This would be the CCS
  MACHINE=CSS
elif [ -d /jcsda ]; then
  TOPDIR=/jcsda    # This is vapor
  MACHINE=VAPOR
else 
  echo CANNOT FIND A VALID TOP-LEVEL DIRECTORY
  exit 1
fi

#=================================================================================================
#  Most commom parameters to edit:
#=================================================================================================

# Set experiment name and analysis date
adate=2010121512
exp=globalprod.$adate

# Set path/file for gsi executable
#gsiexec=${TOPDIR}/save/$USER/svn1/src/global_gsi
gsiexec=${TOPDIR}/save/${USER}/GSI/trunk/src/global_gsi


# Specify GSI fixed field
fixgsi=${TOPDIR}/save/$USER/GSI/trunk/fix

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=$JCAP


# Set data, runtime and save directories
datdir=/ptmp/${USER}/data_sigmap/${exp}
tmpdir=/ptmp/$USER/tmp${JCAP}_sigmap/${exp}
savdir=/ptmp/$USER/out${JCAP}/sigmap/${exp}

# Use with CRTM REL-2.0
fixcrtm=/nwprod/fix/crtm_2.0.2

# Other Executables and scripts
export SIGHDR=/nwprod/exec/global_sighdr
export CHGRESSH=/nwprod/ush/global_chgres.sh
export ndate=/nwprod/util/exec/ndate
export ncp=/bin/cp

#=================================================================================================

# Refractive Index or Bending Angle for GPS?
export gps_dtype="gps_ref"

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

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO

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
prefix_obs=gdas1.t${hha}z.
prefix_prep=$prefix_obs
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas

# Look for required input files in ${datdir}
# if ${datdir}/gdas1.t${hha}z.sgm3prep is present assume we have 
# everything we need, else look elsewhere.

if [ $MACHINE = CSS ]; then
  if [ -s ${datdir}/gdas1.t${hha}z.sgm3prep ]; then 
    datobs=${datdir}
    datges=${datdir}
    datprep=${datobs}
  elif [ -s /com/gfs/prod/gdas.${gdate0}/gdas1.t${hha}z.sgm3prep ]; then
    datges=/com/gfs/prod/gdas.$gdate0
    datobs=/com/gfs/prod/gdas.$adate0
    datprep=${datobs}
  else
    echo Initital files are missing from disk.  
    echo Use Get_Initial_Files.sh to get them
    exit 1
  fi
elif  [ $MACHINE = VAPOR ]; then    
  if [ -s ${datdir}/gdas1.t${hha}z.sgm3prep ]; then 
    datobs=${datdir}
    datges=${datdir}
    datprep=${datobs}
  elif [ -s /com/gfs/prod/gdas.${gdate0}/gdas1.t${hha}z.sgm3prep ]; then   # Not all data files are stored on /com
    datges=/com/gfs/prod/gdas.$gdate0
    if [ -s /shared/glopara/dump/${gdate}/gdas -a \
         -s /shared/glopara/dump/${adate}/gdas ]; then
      datobs=/shared/glopara/dump/${adate}/gdas
      datprep=/com/gfs/prod/gdas.${adate0}
      prefix_obs=
      suffix=gdas.$adate 
    else
      echo Initital files are missing from disk.  
      echo Use Get_Initial_Files.sh to get them
      exit 1
    fi
  else
    echo Initital files are missing from disk.  
    echo Use Get_Initial_Files.sh to get them
    exit 1
  fi
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi

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
        co2dir=${CO2DIR:-$fixgsi}
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
CHEM=""
SINGLEOB=""


cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=$IGEN,factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=71,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=10,gpstop=30.,
   use_gfs_nemsio=.false.,
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
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,dmesh(7)=145.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                 dval(01)=1.0, dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                  dval(02)=1.0, dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                  dval(03)=1.0, dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                 dval(04)=1.0, dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='satwnd',    dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                 dval(05)=1.0, dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='uv',        dplat(06)=' ',       dsis(06)='uv',                 dval(06)=1.0, dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='spd',       dplat(07)=' ',       dsis(07)='spd',                dval(07)=1.0, dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',       dsis(08)='dw',                 dval(08)=1.0, dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='radarbufr', dtype(09)='rw',        dplat(09)=' ',       dsis(09)='rw',                 dval(09)=1.0, dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='prepbufr',  dtype(10)='sst',       dplat(10)=' ',       dsis(10)='sst',                dval(10)=1.0, dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='gps_ref',   dplat(11)=' ',       dsis(11)='gps_ref',            dval(11)=1.0, dthin(11)=0, dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',    dsis(12)='pcp_ssmi',           dval(12)=1.0, dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',    dsis(13)='pcp_tmi',            dval(13)=1.0, dthin(13)=-1,dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',     dsis(14)='sbuv8_n16',          dval(14)=1.0, dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',     dsis(15)='sbuv8_n17',          dval(15)=1.0, dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',     dsis(16)='sbuv8_n18',          dval(16)=1.0, dthin(16)=0, dsfcalc(16)=0,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',     dsis(17)='hirs2_n14',          dval(17)=6.0, dthin(16)=1, dsfcalc(17)=0,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',     dsis(18)='hirs3_n16',          dval(18)=0.0, dthin(18)=1, dsfcalc(18)=0,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',     dsis(19)='hirs3_n17',          dval(19)=6.0, dthin(19)=1, dsfcalc(19)=0,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',     dsis(20)='hirs4_n18',          dval(20)=0.0, dthin(20)=1, dsfcalc(20)=0,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a', dsis(21)='hirs4_metop-a',      dval(21)=6.0, dthin(21)=1, dsfcalc(21)=0,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',     dsis(22)='sndr_g11',           dval(22)=0.0, dthin(22)=1, dsfcalc(22)=0,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',     dsis(23)='sndr_g12',           dval(23)=0.0, dthin(23)=1, dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',     dsis(24)='imgr_g11',           dval(24)=0.0, dthin(24)=1, dsfcalc(24)=0,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',     dsis(25)='imgr_g12',           dval(25)=0.0, dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',    dsis(26)='airs281SUBSET_aqua', dval(26)=20.0,dthin(26)=1, dsfcalc(26)=0,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',     dsis(27)='msu_n14',            dval(27)=2.0, dthin(27)=2, dsfcalc(27)=0,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',     dsis(28)='amsua_n15',          dval(28)=10.0,dthin(28)=2, dsfcalc(28)=0,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',     dsis(29)='amsua_n16',          dval(29)=0.0, dthin(29)=2, dsfcalc(29)=0,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',     dsis(30)='amsua_n17',          dval(30)=0.0, dthin(30)=2, dsfcalc(30)=0,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',     dsis(31)='amsua_n18',          dval(31)=10.0,dthin(31)=2, dsfcalc(31)=0,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a', dsis(32)='amsua_metop-a',      dval(32)=10.0,dthin(32)=2, dsfcalc(32)=0,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',    dsis(33)='amsua_aqua',         dval(33)=5.0, dthin(33)=2, dsfcalc(33)=0,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',     dsis(34)='amsub_n15',          dval(34)=3.0, dthin(34)=3, dsfcalc(34)=0,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',     dsis(35)='amsub_n16',          dval(35)=3.0, dthin(35)=3, dsfcalc(35)=0,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',     dsis(36)='amsub_n17',          dval(36)=3.0, dthin(36)=3, dsfcalc(36)=0,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',     dsis(37)='mhs_n18',            dval(37)=3.0, dthin(37)=3, dsfcalc(37)=0,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a', dsis(38)='mhs_metop-a',        dval(38)=3.0, dthin(38)=3, dsfcalc(38)=0,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',     dsis(39)='ssmi_f13',           dval(39)=0.0, dthin(39)=4, dsfcalc(39)=0,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',     dsis(40)='ssmi_f14',           dval(40)=0.0, dthin(40)=4, dsfcalc(40)=0,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',     dsis(41)='ssmi_f15',           dval(41)=0.0, dthin(41)=4, dsfcalc(41)=0,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',    dsis(42)='amsre_aqua',         dval(42)=0.0, dthin(42)=4, dsfcalc(42)=0,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',    dsis(43)='amsre_aqua',         dval(43)=0.0, dthin(43)=4, dsfcalc(43)=0,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',    dsis(44)='amsre_aqua',         dval(44)=0.0, dthin(44)=4, dsfcalc(44)=0,
   dfile(45)='ssmisbufr', dtype(45)='ssmis',     dplat(45)='f16',     dsis(45)='ssmis_f16',          dval(45)=0.0, dthin(45)=4, dsfcalc(45)=0,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g12',     dsis(46)='sndrD1_g12',         dval(46)=1.5, dthin(46)=5, dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g12',     dsis(47)='sndrD2_g12',         dval(47)=1.5, dthin(47)=5, dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g12',     dsis(48)='sndrD3_g12',         dval(48)=1.5, dthin(48)=5, dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g12',     dsis(49)='sndrD4_g12',         dval(49)=1.5, dthin(49)=5, dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd1',    dplat(50)='g11',     dsis(50)='sndrD1_g11',         dval(50)=1.5, dthin(50)=5, dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd2',    dplat(51)='g11',     dsis(51)='sndrD2_g11',         dval(51)=1.5, dthin(51)=5, dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd3',    dplat(52)='g11',     dsis(52)='sndrD3_g11',         dval(52)=1.5, dthin(52)=5, dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd4',    dplat(53)='g11',     dsis(53)='sndrD4_g11',         dval(53)=1.5, dthin(53)=5, dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd1',    dplat(54)='g13',     dsis(54)='sndrD1_g13',         dval(54)=1.5, dthin(54)=5, dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd2',    dplat(55)='g13',     dsis(55)='sndrD2_g13',         dval(55)=1.5, dthin(55)=5, dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd3',    dplat(56)='g13',     dsis(56)='sndrD3_g13',         dval(56)=1.5, dthin(56)=5, dsfcalc(56)=0,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd4',    dplat(57)='g13',     dsis(57)='sndrD4_g13',         dval(57)=1.5, dthin(57)=5, dsfcalc(57)=0,
   dfile(58)='iasibufr',  dtype(58)='iasi',      dplat(58)='metop-a', dsis(58)='iasi616_metop-a',    dval(58)=20.0,dthin(58)=1, dsfcalc(58)=0,
   dfile(59)='gomebufr',  dtype(59)='gome',      dplat(59)='metop-a', dsis(59)='gome_metop-a',       dval(59)=1.0, dthin(59)=6, dsfcalc(59)=0,
   dfile(60)='omibufr',   dtype(60)='omi',       dplat(60)='aura',    dsis(60)='omi_aura',           dval(60)=1.0, dthin(60)=6, dsfcalc(60)=0,
   dfile(61)='sbuvbufr',  dtype(61)='sbuv2',     dplat(61)='n19',     dsis(61)='sbuv8_n19',          dval(61)=1.0, dthin(61)=0, dsfcalc(61)=0,
   dfile(62)='hirs4bufr', dtype(62)='hirs4',     dplat(62)='n19',     dsis(62)='hirs4_n19',          dval(62)=6.0, dthin(62)=1, dsfcalc(62)=0,
   dfile(63)='amsuabufr', dtype(63)='amsua',     dplat(63)='n19',     dsis(63)='amsua_n19',          dval(63)=10.0,dthin(63)=2, dsfcalc(63)=0,
   dfile(64)='mhsbufr',   dtype(64)='mhs',       dplat(64)='n19',     dsis(64)='mhs_n19',            dval(64)=3.0, dthin(64)=3, dsfcalc(64)=0,
   dfile(65)='tcvitl'     dtype(65)='tcp',       dplat(65)=' ',       dsis(65)='tcp',                dval(65)=1.0, dthin(65)=0, dsfcalc(65)=0,
   dfile(66)='modisbufr', dtype(66)='modis',     dplat(66)='aqua',    dsis(66)='modis_aqua',         dval(66)=1.0, dthin(66)=6, dsfcalc(66)=0,
   dfile(67)='modisbufr', dtype(67)='modis',     dplat(67)='terra',   dsis(67)='modis_terra',        dval(67)=1.0, dthin(67)=6, dsfcalc(67)=0,
   dfile(68)='mlsbufr',   dtype(68)='mls',       dplat(68)='aura',    dsis(68)='mls_aura',           dval(68)=1.0, dthin(68)=0, dsfcalc(68)=0,
   dfile(69)='seviribufr',dtype(69)='seviri',    dplat(69)='m08',     dsis(69)='seviri_m08',         dval(69)=0.0, dthin(69)=7, dsfcalc(69)=0,
   dfile(70)='seviribufr',dtype(70)='seviri',    dplat(70)='m09',     dsis(70)='seviri_m09',         dval(69)=0.0, dthin(70)=7, dsfcalc(70)=0,
   dfile(71)='seviribufr',dtype(71)='seviri',    dplat(71)='m10',     dsis(71)='seviri_m10',         dval(71)=0.0, dthin(71)=7, dsfcalc(71)=0,
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
   dfi_radar_latent_heat_time_period=30.0,
   $RAPIDREFRESH_CLDSURF
 /
 &CHEM
   $CHEM
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
scaninfo=$fixgsi/global_scaninfo.txt
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
if [ -r $datprep/${prefix_prep}prepbufr ]; then
  ln -s -f $datprep/${prefix_prep}prepbufr           ./prepbufr
elif [ -r $datprep/${prefix_prep}prepbufr.nr ]; then    # Look for this file if you do not have restricted data access
  ln -s -f $datprep/${prefix_prep}prepbufr.nr           ./prepbufr
else
  echo You do not have access to a readable prepbufr file
  exit 1
fi
ln -s -f $datobs/${prefix_obs}satwnd.${suffix}    ./satwnd
ln -s -f $datobs/${prefix_obs}gpsro.${suffix}    ./gpsrobufr
ln -s -f $datobs/${prefix_obs}spssmi.${suffix}   ./ssmirrbufr
ln -s -f $datobs/${prefix_obs}sptrmm.${suffix}   ./tmirrbufr
ln -s -f $datobs/${prefix_obs}osbuv8.${suffix}   ./gomebufr
ln -s -f $datobs/${prefix_obs}omi.${suffix}      ./omibufr
ln -s -f $datobs/${prefix_obs}osbuv8.${suffix}   ./sbuvbufr
ln -s -f $datobs/${prefix_obs}goesfv.${suffix}   ./gsnd1bufr
ln -s -f $datobs/${prefix_obs}1bamua.${suffix}   ./amsuabufr
ln -s -f $datobs/${prefix_obs}1bamub.${suffix}   ./amsubbufr
ln -s -f $datobs/${prefix_obs}1bhrs2.${suffix}   ./hirs2bufr
ln -s -f $datobs/${prefix_obs}1bhrs3.${suffix}   ./hirs3bufr
ln -s -f $datobs/${prefix_obs}1bhrs4.${suffix}   ./hirs4bufr
ln -s -f $datobs/${prefix_obs}1bmhs.${suffix}    ./mhsbufr
ln -s -f $datobs/${prefix_obs}1bmsu.${suffix}    ./msubufr
ln -s -f $datobs/${prefix_obs}airsev.${suffix}   ./airsbufr
ln -s -f $datobs/${prefix_obs}sevcsr.${suffix}   ./seviribufr
ln -s -f $datobs/${prefix_obs}mtiasi.${suffix}   ./iasibufr
ln -s -f $datobs/${prefix_obs}esamua.${suffix}   ./amsuabufrears
ln -s -f $datobs/${prefix_obs}esamub.${suffix}   ./amsubbufrears
ln -s -f $datobs/${prefix_obs}eshrs3.${suffix}   ./hirs3bufrears
ln -s -f $datobs/${prefix_obs}syndata.tcvitals.tm00 ./tcvitl

ln -s -f $datobs/${prefix_obs}ssmit.${suffix}    ./ssmitbufr
ln -s -f $datobs/${prefix_obs}amsre.${suffix}    ./amsrebufr
ln -s -f $datobs/${prefix_obs}ssmis.${suffix}    ./ssmisbufr


# Copy bias correction, atmospheric and surface files
ln -s -f $datges/${prefix_tbc}.abias              ./satbias_in
ln -s -f $datges/${prefix_tbc}.satang             ./satbias_angle

# Determine resolution of the guess files
JCAP_GUESS=`$SIGHDR $datprep/${prefix_atm}.sgesprep JCAP`

# Change resolution of input files with chgres if $JCAP is 
# inconsistent with $JCAP_GUESS 
if [[ "$JCAP" = "$JCAP_GUESS" ]]; then
   ln -s -f $datges/${prefix_sfc}.bf03               ./sfcf03
   ln -s -f $datges/${prefix_sfc}.bf06               ./sfcf06
   ln -s -f $datges/${prefix_sfc}.bf09               ./sfcf09

   ln -s -f $datprep/${prefix_atm}.sgm3prep           ./sigf03
   ln -s -f $datprep/${prefix_atm}.sgesprep           ./sigf06
   ln -s -f $datprep/${prefix_atm}.sgp3prep           ./sigf09
else
# first copy required files to working directory

   ln -s -f $datges/gdas1.t${hhg}z.bf03               ./gdas1.t${hhg}z.bf03
   ln -s -f $datges/gdas1.t${hhg}z.bf06               ./gdas1.t${hhg}z.bf06
   ln -s -f $datges/gdas1.t${hhg}z.bf09               ./gdas1.t${hhg}z.bf09

   ln -s -f $datprep/gdas1.t${hha}z.sgm3prep           ./gdas1.t${hha}z.sgm3prep
   ln -s -f $datprep/gdas1.t${hha}z.sgesprep           ./gdas1.t${hha}z.sgesprep
   ln -s -f $datprep/gdas1.t${hha}z.sgp3prep           ./gdas1.t${hha}z.sgp3prep

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

# Save output
mkdir -p $savdir

##cat stdout fort.2* > $savdir/stdout.anl.${adate}
##$ncp siganl          $savdir/siganl.${adate}
##$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
##$ncp satbias_out     $savdir/biascr.${adate}
##$ncp sfcf06          $savdir/sfcf06.${gdate}
##$ncp sigf06          $savdir/sigf06.${gdate}

##ss2gg=${TOPDIR}/save/wx20mi/bin/ss2gg
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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10"
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
