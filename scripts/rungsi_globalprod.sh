#!/bin/ksh
#=======================================================
## Below are LSF (WCOSS queueing system) commands
#BSUB -a poe
#BSUB -e gsi_global.o%J
#BSUB -o gsi_global.o%J
#BSUB -J gsi_global
#BSUB -network type=sn_all:mode=US
#BSUB -q dev
#BSUB -n 32
#BSUB -R span[ptile=8]
#BSUB -R affinity[core(2):distribute=balance]
#BSUB -x
#BSUB -W 01:00
#=======================================================
## Below are PBS (Linux queueing system) commands
#PBS -o gsi_global.e${jobid} 
#PBS -N gsi_global
#PBS -q batch
#PBS -l walltime=00:30:00 
#PBS -l nodes=2:ppn=12
#PBS -j eo                
#PBS -A ada
#PBS -V
#=======================================================

set -x

arch="`uname -s | awk '{print $1}'`"        
echo "Time starting the job is `date` "
# Set default top-level directory
if [ -d /da ]; then
  TOPDIR=/da   # This would be the WCOSS
  MACHINE=WCOSS
elif [ -d /scratch1/portfolios/NCEPDEV/da ]; then
  TOPDIR=/scratch1/portfolios/NCEPDEV/da     #This is zeus 
  MACHINE=ZEUS
else 
  echo CANNOT FIND A VALID TOP-LEVEL DIRECTORY
  exit 1
fi

#=================================================================================================
#  Most commom parameters to edit:
#=================================================================================================

# Set experiment name and analysis date
adate=2013090100
expnm=globalprod    
exp=globalprod.$adate
expid=${expnm}.$adate.wcoss

# Set path/file for gsi executable
gsiexec=/da/save/$USER/trunk/src/global_gsi

# Specify GSI fixed field
fixgsi=/da/save/$USER/trunk/fix

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=$JCAP
export lrun_subdirs=.true.


# Set data, runtime and save directories
if [ $MACHINE = WCOSS ]; then
   datdir=/ptmp/$USER/data_sigmap/${exp}
   tmpdir=/ptmp/$USER/tmp${JCAP}_sigmap/${expid}  
   savdir=/ptmp/$USER/out${JCAP}/sigmap/${expid}  
   fixcrtm=/nceplibs/fix/crtm_v2.1.3
   endianness=Big_Endian
   COMPRESS=gzip 
   DIAG_COMPRESS=YES 
   DIAG_SUFFIX="" 
   DIAG_TARBALL=YES 
elif [ $MACHINE = ZEUS ]; then
   datdir=/scratch2/portfolios/NCEPDEV/ptmp/$USER/data_sigmap/${exp}
   tmpdir=/scratch2/portfolios/NCEPDEV/ptmp/$USER/tmp${JCAP}_sigmap/${expid}  
   savdir=/scratch2/portfolios/NCEPDEV/ptmp/$USER/out${JCAP}/sigmap/${expid} 
   fixcrtm=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/sorc/CRTM_REL-2.1.3/Big_Endian
   endianness=Big_Endian
#  endianness=Little_Endian - once all background fields are available in little endian format, uncomment this option and remove Big_Endian
   COMPRESS=gzip
   DIAG_COMPRESS=YES 
   DIAG_SUFFIX="" 
   DIAG_TARBALL=YES
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi

# Other Executables and scripts
if [ $MACHINE = WCOSS ]; then
   export SIGHDR=/nwprod/exec/global_sighdr
   export CHGRESSH=/nwprod/ush/global_chgres.sh
   export ndate=/nwprod/util/exec/ndate
   export ncp=/bin/cp
   export wc=/usr/bin/wc
elif [ $MACHINE = ZEUS ]; then
   export SIGHDR=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/exec/global_sighdr
   export FIXGLOBAL=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/fix/fix_am 
   export CHGRESEXEC=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/exec/global_chgres
   export CHGRESSH=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/ush/global_chgres_uf_gaea.sh 
   export ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
   export ncp=/bin/cp
   export wc=/usr/bin/wc
else
  echo "Unsupported machine $MACHINE (not sure how you got to here)"
  exit 1
fi

#=================================================================================================

# Refractive Index or Bending Angle for GPS?
export gps_dtype="gps_ref"


if [[  $MACHINE = WCOSS  ]]; then
  export MP_EAGER_LIMIT=65536 
  export MP_COREFILE_FORMAT=lite
  export MP_MPILIB=mpich2
  export MP_LABELIO=yes
  export MP_USE_BULK_XFER=yes
  export MP_SHARED_MEMORY=yes
  export MPICH_ALLTOALL_THROTTLE=0
  export MP_COLLECTIVE_OFFLOAD=yes
  export KMP_STACKSIZE=2048m
fi

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
   export resol=2     # emily: test this
   export resol=1
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
datobs=/com/gfs/prod/gdas.$adate0
datges=/com/gfs/prod/gdas.$gdate0

# Look for required input files in ${datdir}
# if ${datdir}/gdas1.t${hha}z.sgm3prep is present assume we have 
# everything we need, else look elsewhere.

if [ $MACHINE = WCOSS ]; then
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
elif  [ $MACHINE = ZEUS ]; then    
  if [ -s ${datdir}/gdas1.t${hha}z.sgm3prep ]; then 
    datobs=${datdir}
    datges=${datdir}
    datprep=${datobs}
  elif [ -s /NCEPPROD/com/gfs/prod/gdas.${gdate0}/gdas1.t${hha}z.sgm3prep ]; then   # Not all data files are stored on /com
    datges=/NCEPPROD/com/gfs/prod/gdas.$gdate0
    if [ -s /scratch2/portfolios/NCEPDEV/global/noscrub/dump/${gdate}/gdas -a \
         -s /scratch2/portfolios/NCEPDEV/global/noscrub/dump/${adate}/gdas ]; then
      datobs=/scratch2/portfolios/NCEPDEV/global/noscrub/dump/${adate}/gdas
      datprep=/NCEPPROD/com/gfs/prod/gdas.${adate0}
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
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                $ncp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
#CH4 file decision
ICH4=${ICH4:-0}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${CH4DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                $ncp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
   fi
fi
IN2O=${IN2O:-0}
if [ $IN2O -gt 0 ] ; then
#        # Copy n2o files to $tmpdir
        n2odir=${N2ODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                $ncp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
   fi
fi
ICO=${ICO:-0}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${CODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                $ncp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
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
   gencode=$IGEN,factqmin=5.0,factqmax=5.0,deltim=$DELTIM,
   ndat=64,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.false.,lrun_subdirs=${lrun_subdirs},
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,diag_precon=.true.,
   step_start=1.e-3,emiss_bc=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT_A,NLON=$NLON_A,nsig=$LEVS,
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
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                 dval(01)=0.0, dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                  dval(02)=0.0, dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                  dval(03)=0.0, dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                 dval(04)=0.0, dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='satwndbufr',dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                 dval(05)=0.0, dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='uv',        dplat(06)=' ',       dsis(06)='uv',                 dval(06)=0.0, dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='spd',       dplat(07)=' ',       dsis(07)='spd',                dval(07)=0.0, dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',       dsis(08)='dw',                 dval(08)=0.0, dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='radarbufr', dtype(09)='rw',        dplat(09)=' ',       dsis(09)='rw',                 dval(09)=0.0, dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='prepbufr',  dtype(10)='sst',       dplat(10)=' ',       dsis(10)='sst',                dval(10)=0.0, dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='$gps_dtype',   dplat(11)=' ',       dsis(11)='gps',             dval(11)=0.0, dthin(11)=0, dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',    dsis(12)='pcp_ssmi',           dval(12)=0.0, dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',    dsis(13)='pcp_tmi',            dval(13)=0.0, dthin(13)=-1,dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',     dsis(14)='sbuv8_n16',          dval(14)=0.0, dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',     dsis(15)='sbuv8_n17',          dval(15)=0.0, dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',     dsis(16)='sbuv8_n18',          dval(16)=0.0, dthin(16)=0, dsfcalc(16)=0,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n17',     dsis(17)='hirs3_n17',          dval(17)=0.0, dthin(17)=1, dsfcalc(17)=1,
   dfile(18)='hirs4bufr', dtype(18)='hirs4',     dplat(18)='metop-a', dsis(18)='hirs4_metop-a',      dval(18)=0.0, dthin(18)=1, dsfcalc(18)=1,
   dfile(19)='gimgrbufr', dtype(19)='goes_img',  dplat(19)='g11',     dsis(19)='imgr_g11',           dval(19)=0.0, dthin(19)=1, dsfcalc(19)=0,
   dfile(20)='gimgrbufr', dtype(20)='goes_img',  dplat(20)='g12',     dsis(20)='imgr_g12',           dval(20)=0.0, dthin(20)=1, dsfcalc(20)=0,
   dfile(21)='airsbufr',  dtype(21)='airs',      dplat(21)='aqua',    dsis(21)='airs281SUBSET_aqua', dval(21)=0.0, dthin(21)=1, dsfcalc(21)=1,
   dfile(22)='amsuabufr', dtype(22)='amsua',     dplat(22)='n15',     dsis(22)='amsua_n15',          dval(22)=0.0, dthin(22)=1, dsfcalc(22)=1,
   dfile(23)='amsuabufr', dtype(23)='amsua',     dplat(23)='n18',     dsis(23)='amsua_n18',          dval(23)=0.0, dthin(23)=1, dsfcalc(23)=1,
   dfile(24)='amsuabufr', dtype(24)='amsua',     dplat(24)='metop-a', dsis(24)='amsua_metop-a',      dval(24)=0.0, dthin(24)=1, dsfcalc(24)=1,
   dfile(25)='airsbufr',  dtype(25)='amsua',     dplat(25)='aqua',    dsis(25)='amsua_aqua',         dval(25)=0.0, dthin(25)=1, dsfcalc(25)=1,
   dfile(26)='amsubbufr', dtype(26)='amsub',     dplat(26)='n17',     dsis(26)='amsub_n17',          dval(26)=0.0, dthin(26)=1, dsfcalc(26)=1,
   dfile(27)='mhsbufr',   dtype(27)='mhs',       dplat(27)='n18',     dsis(27)='mhs_n18',            dval(27)=0.0, dthin(27)=1, dsfcalc(27)=1,
   dfile(28)='mhsbufr',   dtype(28)='mhs',       dplat(28)='metop-a', dsis(28)='mhs_metop-a',        dval(28)=0.0, dthin(28)=1, dsfcalc(28)=1,
   dfile(29)='ssmitbufr', dtype(29)='ssmi',      dplat(29)='f14',     dsis(29)='ssmi_f14',           dval(29)=0.0, dthin(29)=1, dsfcalc(29)=0,
   dfile(30)='ssmitbufr', dtype(30)='ssmi',      dplat(30)='f15',     dsis(30)='ssmi_f15',           dval(30)=0.0, dthin(30)=1, dsfcalc(30)=0,
   dfile(31)='amsrebufr', dtype(31)='amsre_low', dplat(31)='aqua',    dsis(31)='amsre_aqua',         dval(31)=0.0, dthin(31)=1, dsfcalc(31)=0,
   dfile(32)='amsrebufr', dtype(32)='amsre_mid', dplat(32)='aqua',    dsis(32)='amsre_aqua',         dval(32)=0.0, dthin(32)=1, dsfcalc(32)=0,
   dfile(33)='amsrebufr', dtype(33)='amsre_hig', dplat(33)='aqua',    dsis(33)='amsre_aqua',         dval(33)=0.0, dthin(33)=1, dsfcalc(33)=0,
   dfile(34)='ssmisbufr', dtype(34)='ssmis',     dplat(34)='f16',     dsis(34)='ssmis_f16',          dval(34)=0.0, dthin(34)=1, dsfcalc(34)=0,
   dfile(35)='gsnd1bufr', dtype(35)='sndrd1',    dplat(35)='g12',     dsis(35)='sndrD1_g12',         dval(35)=0.0, dthin(35)=1, dsfcalc(35)=0,
   dfile(36)='gsnd1bufr', dtype(36)='sndrd2',    dplat(36)='g12',     dsis(36)='sndrD2_g12',         dval(36)=0.0, dthin(36)=1, dsfcalc(36)=0,
   dfile(37)='gsnd1bufr', dtype(37)='sndrd3',    dplat(37)='g12',     dsis(37)='sndrD3_g12',         dval(37)=0.0, dthin(37)=1, dsfcalc(37)=0,
   dfile(38)='gsnd1bufr', dtype(38)='sndrd4',    dplat(38)='g12',     dsis(38)='sndrD4_g12',         dval(38)=0.0, dthin(38)=1, dsfcalc(38)=0,
   dfile(39)='gsnd1bufr', dtype(39)='sndrd1',    dplat(39)='g11',     dsis(39)='sndrD1_g11',         dval(39)=0.0, dthin(39)=1, dsfcalc(39)=0,
   dfile(40)='gsnd1bufr', dtype(40)='sndrd2',    dplat(40)='g11',     dsis(40)='sndrD2_g11',         dval(40)=0.0, dthin(40)=1, dsfcalc(40)=0,
   dfile(41)='gsnd1bufr', dtype(41)='sndrd3',    dplat(41)='g11',     dsis(41)='sndrD3_g11',         dval(41)=0.0, dthin(41)=1, dsfcalc(41)=0,
   dfile(42)='gsnd1bufr', dtype(42)='sndrd4',    dplat(42)='g11',     dsis(42)='sndrD4_g11',         dval(42)=0.0, dthin(42)=1, dsfcalc(42)=0,
   dfile(43)='gsnd1bufr', dtype(43)='sndrd1',    dplat(43)='g13',     dsis(43)='sndrD1_g13',         dval(43)=0.0, dthin(43)=1, dsfcalc(43)=0,
   dfile(44)='gsnd1bufr', dtype(44)='sndrd2',    dplat(44)='g13',     dsis(44)='sndrD2_g13',         dval(44)=0.0, dthin(44)=1, dsfcalc(44)=0,
   dfile(45)='gsnd1bufr', dtype(45)='sndrd3',    dplat(45)='g13',     dsis(45)='sndrD3_g13',         dval(45)=0.0, dthin(45)=1, dsfcalc(45)=0,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd4',    dplat(46)='g13',     dsis(46)='sndrD4_g13',         dval(46)=0.0, dthin(46)=1, dsfcalc(46)=0,
   dfile(47)='iasibufr',  dtype(47)='iasi',      dplat(47)='metop-a', dsis(47)='iasi616_metop-a',    dval(47)=0.0, dthin(47)=1, dsfcalc(47)=1,
   dfile(48)='gomebufr',  dtype(48)='gome',      dplat(48)='metop-a', dsis(48)='gome_metop-a',       dval(48)=0.0, dthin(48)=2, dsfcalc(48)=0,
   dfile(49)='omibufr',   dtype(49)='omi',       dplat(49)='aura',    dsis(49)='omi_aura',           dval(49)=0.0, dthin(49)=2, dsfcalc(49)=0,
   dfile(50)='sbuvbufr',  dtype(50)='sbuv2',     dplat(50)='n19',     dsis(50)='sbuv8_n19',          dval(50)=0.0, dthin(50)=0, dsfcalc(50)=0,
   dfile(51)='hirs4bufr', dtype(51)='hirs4',     dplat(51)='n19',     dsis(51)='hirs4_n19',          dval(51)=0.0, dthin(51)=1, dsfcalc(51)=1,
   dfile(52)='amsuabufr', dtype(52)='amsua',     dplat(52)='n19',     dsis(52)='amsua_n19',          dval(52)=0.0, dthin(52)=1, dsfcalc(52)=1,
   dfile(53)='mhsbufr',   dtype(53)='mhs',       dplat(53)='n19',     dsis(53)='mhs_n19',            dval(53)=0.0, dthin(53)=1, dsfcalc(53)=1,
   dfile(54)='tcvitl'     dtype(54)='tcp',       dplat(54)=' ',       dsis(54)='tcp',                dval(54)=0.0, dthin(54)=0, dsfcalc(54)=0,
   dfile(55)='seviribufr',dtype(55)='seviri',    dplat(55)='m08',     dsis(55)='seviri_m08',         dval(55)=0.0, dthin(55)=1, dsfcalc(55)=0,
   dfile(56)='seviribufr',dtype(56)='seviri',    dplat(56)='m09',     dsis(56)='seviri_m09',         dval(56)=0.0, dthin(56)=1, dsfcalc(56)=0,
   dfile(57)='seviribufr',dtype(57)='seviri',    dplat(57)='m10',     dsis(57)='seviri_m10',         dval(57)=0.0, dthin(57)=1, dsfcalc(57)=0,
   dfile(58)='hirs4bufr', dtype(58)='hirs4',     dplat(58)='metop-b', dsis(58)='hirs4_metop-b',      dval(58)=0.0, dthin(58)=1, dsfcalc(58)=0,
   dfile(59)='amsuabufr', dtype(59)='amsua',     dplat(59)='metop-b', dsis(59)='amsua_metop-b',      dval(59)=0.0, dthin(59)=1, dsfcalc(59)=0,
   dfile(60)='mhsbufr',   dtype(60)='mhs',       dplat(60)='metop-b', dsis(60)='mhs_metop-b',        dval(60)=0.0, dthin(60)=1, dsfcalc(60)=0,
   dfile(61)='iasibufr',  dtype(61)='iasi',      dplat(61)='metop-b', dsis(61)='iasi616_metop-b',    dval(61)=0.0, dthin(61)=1, dsfcalc(61)=0,
   dfile(62)='gomebufr',  dtype(62)='gome',      dplat(62)='metop-b', dsis(62)='gome_metop-b',       dval(62)=0.0, dthin(62)=2, dsfcalc(62)=0,
   dfile(63)='atmsbufr',  dtype(63)='atms',      dplat(63)='npp',     dsis(63)='atms_npp',           dval(63)=0.0, dthin(63)=1, dsfcalc(63)=0,
   dfile(64)='crisbufr',  dtype(64)='cris',      dplat(64)='npp',     dsis(64)='cris_npp',           dval(64)=0.0, dthin(64)=1, dsfcalc(64)=0,
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
berror=$fixgsi/$endianness/global_berror.l${LEVS}y${NLAT_A}.f77
emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin   
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin               
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin             
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin             
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin             
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin                   
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin                   
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin                 
emiscoef_MWwater=$fixcrtm/FASTEM5.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
#satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt
atmsbeamdat=$fixgsi/atms_beamwidth.txt

errtable=$fixgsi/prepobs_errtable.global


# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012


# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
$ncp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin               
$ncp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin             
$ncp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin             
$ncp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin             
$ncp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin           
$ncp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin           
$ncp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin                 
$ncp $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
#$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $atmsbeamdat ./atms_beamwidth.txt
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr


# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $fixcrtm/${file}.SpcCoeff.bin ./
   $ncp $fixcrtm/${file}.TauCoeff.bin ./
done


# Copy observational data to $tmpdir
if [ -r $datprep/${prefix_prep}prepbufr ]; then
  $ncp $datprep/${prefix_prep}prepbufr           ./prepbufr
elif [ -r $datprep/${prefix_prep}prepbufr.nr ]; then    # Look for this file if you do not have restricted data access
  $ncp $datprep/${prefix_prep}prepbufr.nr        ./prepbufr
else
  echo You do not have access to a readable prepbufr file
  exit 1
fi

$ncp $datobs/${prefix_obs}satwnd.${suffix}   ./satwndbufr
$ncp $datobs/${prefix_obs}gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}gome.${suffix}     ./gomebufr
$ncp $datobs/${prefix_obs}omi.${suffix}      ./omibufr
$ncp $datobs/${prefix_obs}osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}sevcsr.${suffix}   ./seviribufr
$ncp $datobs/${prefix_obs}mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}esamua.${suffix}   ./amsuabufrears
$ncp $datobs/${prefix_obs}esamub.${suffix}   ./amsubbufrears
$ncp $datobs/${prefix_obs}eshrs3.${suffix}   ./hirs3bufrears
$ncp $datobs/${prefix_obs}ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}ssmisu.${suffix}   ./ssmisbufr   
$ncp $datobs/${prefix_obs}atms.${suffix}     ./atmsbufr
$ncp $datobs/${prefix_obs}cris.${suffix}     ./crisbufr
$ncp $datobs/${prefix_obs}syndata.tcvitals.tm00 ./tcvitl


# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
#ln -s -f $datges/${prefix_tbc}.abias_pc           ./satbias_pc
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle
$ncp $datges/${prefix_tbc}.radstat            ./radstat.gdas

/da/save/$USER/trunk/util/Radiance_bias_correction_Utilities/write_biascr_option.x -newpc4pred -adp_anglebc 4

cp satbias_in satbias_in.orig
cp satbias_in.new satbias_in

listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
for type in $listdiag; do
   diag_file=`echo $type | cut -d',' -f1`
   fname=`echo $diag_file | cut -d'.' -f1`
   date=`echo $diag_file | cut -d'.' -f2`
   $UNCOMPRESS $diag_file
   fnameanl=$(echo $fname|sed 's/_ges//g')
   mv $fname.$date $fnameanl
done


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

   #emily
   if [  $MACHINE = WCOSS  ]; then
      export SIGLEVEL=/NCEPDEV/rstprod/nwprod/fix/global_hyblev.l64.txt
   elif [  $MACHINE = ZEUS  ]; then
      export SIGLEVEL=/NCEPPROD/nwprod/fix/global_hyblev.l64.txt
   fi

   export JCAP=$JCAP
   export LEVS=$LEVS
   export LONB=$LONA
   export LATB=$LATA

   export VERBOSE="YES"

   # Operational chgres for operational sigio
   export DATA=$tmpdir

   export SIGINP=$tmpdir/gdas1.t${hha}z.sgm3prep
   export SFCINP=$tmpdir/gdas1.t${hhg}z.bf03
   export SIGOUT=$tmpdir/gdas2.t${hha}z.sgm3prep
   export SFCOUT=$tmpdir/gdas2.t${hhg}z.bf03
   $CHGRESSH

   export SIGINP=$tmpdir/gdas1.t${hha}z.sgesprep
   export SFCINP=$tmpdir/gdas1.t${hhg}z.bf06
   export SIGOUT=$tmpdir/gdas2.t${hha}z.sgesprep
   export SFCOUT=$tmpdir/gdas2.t${hhg}z.bf06
   $CHGRESSH

   export SIGINP=$tmpdir/gdas1.t${hha}z.sgp3prep
   export SFCINP=$tmpdir/gdas1.t${hhg}z.bf09
   export SIGOUT=$tmpdir/gdas2.t${hha}z.sgp3prep
   export SFCOUT=$tmpdir/gdas2.t${hhg}z.bf09
   $CHGRESSH

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
if [  $MACHINE = ZEUS  ]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   /bin/ksh --login
   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"
   rc=$?

elif [  $MACHINE = WCOSS  ]; then

   # Run gsi under Parallel Operating Environment (poe) on NCEP IBM
   mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout
   rc=$?
fi

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/gsistat.$dumpobs.$adate
$ncp siganl          $savdir/siganl.$dumpobs.$adate
$ncp sfcanl.gsi      $savdir/sfcanl_gsi.$dumpobs.$adate
$ncp satbias_out     $savdir/biascr.$dumpobs.$adate
$ncp sfcf06          $savdir/sfcf06.$dumpges.$gdate
$ncp sigf06          $savdir/sigf06.$dumpges.$gdate

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

CNVSTAT=$savdir/cnvstat.$dumpobs.$adate
PCPSTAT=$savdir/pcpstat.$dumpobs.$adate
OZNSTAT=$savdir/oznstat.$dumpobs.$adate
RADSTAT=$savdir/radstat.$dumpobs.$adate

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

cd $tmpdir    # we should already be in $DATA, but extra cd to be sure. 

# Set up lists and variables for various types of diagnostic files. 
ntype=3 

diagtype[0]="conv" 
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm" 
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura mls_aura" 
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 ssmis_las_f17 ssmis_uas_f17 ssmis_img_f17 ssmis_env_f17 ssmis_las_f18 ssmis_uas_f18 ssmis_img_f18 ssmis_env_f18 ssmis_las_f19 ssmis_uas_f19 ssmis_img_f19 ssmis_env_f19 ssmis_las_f20 ssmis_uas_f20 ssmis_img_f20 ssmis_env_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp atms_npp hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b gome_metop-b" 

diaglist[0]=listcnv 
diaglist[1]=listpcp 
diaglist[2]=listozn 
diaglist[3]=listrad 

diagfile[0]=$CNVSTAT 
diagfile[1]=$PCPSTAT 
diagfile[2]=$OZNSTAT 
diagfile[3]=$RADSTAT 

numfile[0]=0 
numfile[1]=0 
numfile[2]=0 
numfile[3]=0 


# Set diagnostic file prefix based on lrun_subdirs variable 
if [ $lrun_subdirs = ".true." ]; then 
   prefix=" dir.*/" 
else 
   prefix="pe*" 
fi 

# Collect diagnostic files as a function of loop and type. 
loops="01 03"
for loop in $loops; do
   case $loop in 
      01) string=ges;; 
      03) string=anl;; 
       *) string=$loop;; 
   esac 
   n=-1 
   while [ $((n+=1)) -le $ntype ] ;do 
      for type in `echo ${diagtype[n]}`; do 
         count=`ls ${prefix}${type}_${loop}* | $wc -l` 
         if [ $count -gt 0 ]; then 
            cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${adate}${DIAG_SUFFIX} 
            echo "diag_${type}_${string}.${adate}*" >> ${diaglist[n]} 
            numfile[n]=`expr ${numfile[n]} + 1` 
         fi 
      done 
   done 
done 

cd $tmpdir    # we should already be in $DATA, but extra cd to be sure. 

# If requested, compress diagnostic files 
if [[ $DIAG_COMPRESS = YES ]]; then 
   for file in `ls diag_*${adate}${DIAG_SUFFIX}`; do 
      $COMPRESS $file 
   done 
fi 

# If requested, create diagnostic file tarballs 
if [[ $DIAG_TARBALL = YES ]]; then 
   n=-1 
   while [ $((n+=1)) -le $ntype ] ;do 
      TAROPTS="-uvf" 
      if [ ! -s ${diagfile[n]} ]; then 
         TAROPTS="-cvf"
      fi
      if [ ${numfile[n]} -gt 0 ]; then 
         tar $TAROPTS ${diagfile[n]} `cat ${diaglist[n]}`
      fi
   done

#  Restrict CNVSTAT 
   chmod 750 $CNVSTAT 
   chgrp rstprod $CNVSTAT 
fi

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
