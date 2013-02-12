
set -x

# Set experiment name and analysis date

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#basedir=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken
#gsiexec=$gsiexec
#gsiexec=$basedir/EXP-port/src/global_gsi


# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
#export JCAP=62
export LEVS=64
export JCAP_B=$JCAP

# Set runtime and save directories
tmpdir=$tmpdir/tmp${JCAP}/${exp}
#tmpdir=/scratch2/portfolios/NCEPDEV/ptmp/Michael.Lueken/tmp${JCAP}/${exp}
savdir=$savdir/out${JCAP}/${exp}
#savdir=/scratch2/portfolios/NCEPDEV/ptmp/Michael.Lueken/out${JCAP}/${exp}

# Specify GSI fixed field and data directories.
#fixgsi=$fixgsi
#fixgsi=$basedir/EXP-port/fix
#fixcrtm=$fixcrtm
#fixcrtm=$basedir/nwprod/lib/sorc/CRTM_REL-2.0.5/fix

#datobs=$datobs
#datobs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/sigmap/$adate
#datges=$datges
#datges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/sigmap/$adate

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
ncp=/bin/cp


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=120
   export resol=1
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "126" ]]; then
   export LONA=256
   export LATA=128
   export DELTIM=600
   export resol=2
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $global_T62_adate`
hha=`echo $global_T62_adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z.
prefix_prep=$prefix_obs
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
suffix=tm00.bufr_d


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
#        # Copy ch4 files to $tmpdir
        n2odir=${N2ODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
LAGDATA=""
HYBRID_ENSEMBLE=""

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=82,factqmin=0.1,factqmax=0.1,deltim=$DELTIM,
   ndat=75,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.false.,
   nhr_assimilation=6,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
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
   dmesh(1)=145.0,dmesh(2)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',              dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',               dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',               dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',              dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',              dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='satwndbufr',dtype(06)='uv',        dplat(06)=' ',       dsis(06)='uv',              dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='spd',       dplat(07)=' ',       dsis(07)='spd',             dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',       dsis(08)='dw',              dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='radarbufr', dtype(09)='rw',        dplat(09)=' ',       dsis(09)='rw',              dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='prepbufr',  dtype(10)='sst',       dplat(10)=' ',       dsis(10)='sst',             dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='gps_bnd',   dplat(11)=' ',       dsis(11)='gps',             dval(11)=0.0,  dthin(11)=0,  dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',    dsis(12)='pcp_ssmi',        dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',    dsis(13)='pcp_tmi',         dval(13)=0.0,  dthin(13)=-1, dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',     dsis(14)='sbuv8_n16',       dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',     dsis(15)='sbuv8_n17',       dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',     dsis(16)='sbuv8_n18',       dval(16)=0.0,  dthin(16)=0,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n17',     dsis(17)='hirs3_n17',       dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs4bufr', dtype(18)='hirs4',     dplat(18)='metop-a', dsis(18)='hirs4_metop-a',   dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=1,
   dfile(19)='gimgrbufr', dtype(19)='goes_img',  dplat(19)='g11',     dsis(19)='imgr_g11',        dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='gimgrbufr', dtype(20)='goes_img',  dplat(20)='g12',     dsis(20)='imgr_g12',        dval(20)=0.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='airsbufr',  dtype(21)='airs',      dplat(21)='aqua',    dsis(21)='airs281SUBSET_aqua',dval(21)=0.0,dthin(21)=1, dsfcalc(21)=1,
   dfile(22)='amsuabufr', dtype(22)='amsua',     dplat(22)='n15',     dsis(22)='amsua_n15',       dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=1,
   dfile(23)='amsuabufr', dtype(23)='amsua',     dplat(23)='n18',     dsis(23)='amsua_n18',       dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=1,
   dfile(24)='amsuabufr', dtype(24)='amsua',     dplat(24)='metop-a', dsis(24)='amsua_metop-a',   dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=1,
   dfile(25)='airsbufr',  dtype(25)='amsua',     dplat(25)='aqua',    dsis(25)='amsua_aqua',      dval(25)=0.0,  dthin(25)=1,  dsfcalc(25)=1,
   dfile(26)='amsubbufr', dtype(26)='amsub',     dplat(26)='n17',     dsis(26)='amsub_n17',       dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=1,
   dfile(27)='mhsbufr',   dtype(27)='mhs',       dplat(27)='n18',     dsis(27)='mhs_n18',         dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=1,
   dfile(28)='mhsbufr',   dtype(28)='mhs',       dplat(28)='metop-a', dsis(28)='mhs_metop-a',     dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=1,
   dfile(29)='ssmitbufr', dtype(29)='ssmi',      dplat(29)='f14',     dsis(29)='ssmi_f14',        dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='ssmitbufr', dtype(30)='ssmi',      dplat(30)='f15',     dsis(30)='ssmi_f15',        dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsrebufr', dtype(31)='amsre_low', dplat(31)='aqua',    dsis(31)='amsre_aqua',      dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='amsrebufr', dtype(32)='amsre_mid', dplat(32)='aqua',    dsis(32)='amsre_aqua',      dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='amsrebufr', dtype(33)='amsre_hig', dplat(33)='aqua',    dsis(33)='amsre_aqua',      dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='ssmisbufr', dtype(34)='ssmis_las', dplat(34)='f16',     dsis(34)='ssmis_f16',       dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='ssmisbufr', dtype(35)='ssmis_uas', dplat(35)='f16',     dsis(35)='ssmis_f16',       dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='ssmisbufr', dtype(36)='ssmis_img', dplat(36)='f16',     dsis(36)='ssmis_f16',       dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='ssmisbufr', dtype(37)='ssmis_env', dplat(37)='f16',     dsis(37)='ssmis_f16',       dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='gsnd1bufr', dtype(38)='sndrd1',    dplat(38)='g12',     dsis(38)='sndrD1_g12',      dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='gsnd1bufr', dtype(39)='sndrd2',    dplat(39)='g12',     dsis(39)='sndrD2_g12',      dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='gsnd1bufr', dtype(40)='sndrd3',    dplat(40)='g12',     dsis(40)='sndrD3_g12',      dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='gsnd1bufr', dtype(41)='sndrd4',    dplat(41)='g12',     dsis(41)='sndrD4_g12',      dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='gsnd1bufr', dtype(42)='sndrd1',    dplat(42)='g11',     dsis(42)='sndrD1_g11',      dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='gsnd1bufr', dtype(43)='sndrd2',    dplat(43)='g11',     dsis(43)='sndrD2_g11',      dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='gsnd1bufr', dtype(44)='sndrd3',    dplat(44)='g11',     dsis(44)='sndrD3_g11',      dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr', dtype(45)='sndrd4',    dplat(45)='g11',     dsis(45)='sndrD4_g11',      dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g13',     dsis(46)='sndrD1_g13',      dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g13',     dsis(47)='sndrD2_g13',      dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g13',     dsis(48)='sndrD3_g13',      dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g13',     dsis(49)='sndrD4_g13',      dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=0,
   dfile(50)='iasibufr',  dtype(50)='iasi',      dplat(50)='metop-a', dsis(50)='iasi616_metop-a', dval(50)=0.0,  dthin(50)=1,  dsfcalc(50)=1,
   dfile(51)='gomebufr',  dtype(51)='gome',      dplat(51)='metop-a', dsis(51)='gome_metop-a',    dval(51)=0.0,  dthin(51)=2,  dsfcalc(51)=0,
   dfile(52)='omibufr',   dtype(52)='omi',       dplat(52)='aura',    dsis(52)='omi_aura',        dval(52)=0.0,  dthin(52)=2,  dsfcalc(52)=0,
   dfile(53)='sbuvbufr',  dtype(53)='sbuv2',     dplat(53)='n19',     dsis(53)='sbuv8_n19',       dval(53)=0.0,  dthin(53)=0,  dsfcalc(53)=0,
   dfile(54)='hirs4bufr', dtype(54)='hirs4',     dplat(54)='n19',     dsis(54)='hirs4_n19',       dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=1,
   dfile(55)='amsuabufr', dtype(55)='amsua',     dplat(55)='n19',     dsis(55)='amsua_n19',       dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=1,
   dfile(56)='mhsbufr',   dtype(56)='mhs',       dplat(56)='n19',     dsis(56)='mhs_n19',         dval(56)=0.0,  dthin(56)=1,  dsfcalc(56)=1,
   dfile(57)='tcvitl'     dtype(57)='tcp',       dplat(57)=' ',       dsis(57)='tcp',             dval(57)=0.0,  dthin(57)=0,  dsfcalc(57)=0,
   dfile(58)='seviribufr',dtype(58)='seviri',    dplat(58)='m08',     dsis(58)='seviri_m08',      dval(58)=0.0,  dthin(58)=1,  dsfcalc(58)=0,
   dfile(59)='seviribufr',dtype(59)='seviri',    dplat(59)='m09',     dsis(59)='seviri_m09',      dval(59)=0.0,  dthin(59)=1,  dsfcalc(59)=0,
   dfile(60)='seviribufr',dtype(60)='seviri',    dplat(60)='m10',     dsis(60)='seviri_m10',      dval(60)=0.0,  dthin(60)=1,  dsfcalc(60)=0,
   dfile(61)='hirs4bufr', dtype(61)='hirs4',     dplat(61)='metop-b', dsis(61)='hirs4_metop-b',   dval(61)=0.0,  dthin(61)=1,  dsfcalc(61)=0,
   dfile(62)='amsuabufr', dtype(62)='amsua',     dplat(62)='metop-b', dsis(62)='amsua_metop-b',   dval(62)=0.0,  dthin(62)=1,  dsfcalc(62)=0,
   dfile(63)='mhsbufr',   dtype(63)='mhs',       dplat(63)='metop-b', dsis(63)='mhs_metop-b',     dval(63)=0.0,  dthin(63)=1,  dsfcalc(63)=0,
   dfile(64)='iasibufr',  dtype(64)='iasi',      dplat(64)='metop-b', dsis(64)='iasi616_metop-b', dval(64)=0.0,  dthin(64)=1,  dsfcalc(64)=0,
   dfile(65)='gomebufr',  dtype(65)='gome',      dplat(65)='metop-b', dsis(65)='gome_metop-b',    dval(65)=0.0,  dthin(65)=2,  dsfcalc(65)=0,
   dfile(66)='atmsbufr',  dtype(66)='atms',      dplat(66)='npp',     dsis(66)='atms_npp',        dval(66)=0.0,  dthin(66)=1,  dsfcalc(66)=0,
   dfile(67)='crisbufr',  dtype(67)='cris',      dplat(67)='npp',     dsis(67)='cris_npp',        dval(67)=0.0,  dthin(67)=1,  dsfcalc(67)=0,
   dfile(68)='gsnd1bufr', dtype(68)='sndrd1',    dplat(68)='g14',     dsis(68)='sndrD1_g14',      dval(68)=0.0,  dthin(68)=1,  dsfcalc(68)=0,
   dfile(69)='gsnd1bufr', dtype(69)='sndrd2',    dplat(69)='g14',     dsis(69)='sndrD2_g14',      dval(69)=0.0,  dthin(69)=1,  dsfcalc(69)=0,
   dfile(70)='gsnd1bufr', dtype(70)='sndrd3',    dplat(70)='g14',     dsis(70)='sndrD3_g14',      dval(70)=0.0,  dthin(70)=1,  dsfcalc(70)=0,
   dfile(71)='gsnd1bufr', dtype(71)='sndrd4',    dplat(71)='g14',     dsis(71)='sndrD4_g14',      dval(71)=0.0,  dthin(71)=1,  dsfcalc(71)=0,
   dfile(72)='gsnd1bufr', dtype(72)='sndrd1',    dplat(72)='g15',     dsis(72)='sndrD1_g15',      dval(72)=0.0,  dthin(72)=1,  dsfcalc(72)=0,
   dfile(73)='gsnd1bufr', dtype(73)='sndrd2',    dplat(73)='g15',     dsis(73)='sndrD2_g15',      dval(73)=0.0,  dthin(73)=1,  dsfcalc(73)=0,
   dfile(74)='gsnd1bufr', dtype(74)='sndrd3',    dplat(74)='g15',     dsis(74)='sndrD3_g15',      dval(74)=0.0,  dthin(74)=1,  dsfcalc(74)=0,
   dfile(75)='gsnd1bufr', dtype(75)='sndrd4',    dplat(75)='g15',     dsis(75)='sndrD4_g15',      dval(75)=0.0,  dthin(75)=1,  dsfcalc(75)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.false.,n_ens=10,beta1_inv=0.5,s_ens_h=800,s_ens_v=-0.7,generate_ens=.false.,uv_hyb_ens=.false.,jcap_ens=62,
   nlat_ens=96,nlon_ens=192,ANISO_A_EN=.false.,jcap_ens_test=62,oz_univ_static=.false.,readin_localization=.false.,
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM

 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${global_T62_adate},
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

berror=$fixgsi/$endianness/global_berror.l${LEVS}y${NLAT}.f77

emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satangl=$fixgsi/global_satangbias.txt

satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo.txt
anavinfo=$fixgsi/global_anavinfo.l64.txt
ozinfo=$fixgsi/global_ozinfo.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
hybens_locinfo=$fixgsi/global_hybens_locinfo.l64.txt
errtable=$fixgsi/prepobs_errtable.global
atmsbeaminfo=$fixgsi/atms_beamwidth.txt

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp = $global_T62_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_T62_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_T62_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $global_T62_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

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
$ncp $anavinfo ./anavinfo
$ncp $hybens_locinfo ./hybens_locinfo
$ncp $atmsbeaminfo ./atms_beamwidth.txt

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
ln -s -f $global_T62_obs/${prefix_obs}prepbufr           ./prepbufr
ln -s -f $global_T62_obs/${prefix_obs}satwnd.${suffix}   ./satwndbufr
ln -s -f $global_T62_obs/${prefix_obs}gpsro.${suffix}    ./gpsrobufr
ln -s -f $global_T62_obs/${prefix_obs}spssmi.${suffix}   ./ssmirrbufr
ln -s -f $global_T62_obs/${prefix_obs}sptrmm.${suffix}   ./tmirrbufr
ln -s -f $global_T62_obs/${prefix_obs}gome.${suffix}     ./gomebufr
ln -s -f $global_T62_obs/${prefix_obs}omi.${suffix}      ./omibufr
ln -s -f $global_T62_obs/${prefix_obs}osbuv8.${suffix}   ./sbuvbufr
ln -s -f $global_T62_obs/${prefix_obs}goesfv.${suffix}   ./gsnd1bufr
ln -s -f $global_T62_obs/${prefix_obs}1bamua.${suffix}   ./amsuabufr
ln -s -f $global_T62_obs/${prefix_obs}1bamub.${suffix}   ./amsubbufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs2.${suffix}   ./hirs2bufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs3.${suffix}   ./hirs3bufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs4.${suffix}   ./hirs4bufr
ln -s -f $global_T62_obs/${prefix_obs}1bmhs.${suffix}    ./mhsbufr
ln -s -f $global_T62_obs/${prefix_obs}1bmsu.${suffix}    ./msubufr
ln -s -f $global_T62_obs/${prefix_obs}airsev.${suffix}   ./airsbufr
ln -s -f $global_T62_obs/${prefix_obs}sevcsr.${suffix}   ./seviribufr
ln -s -f $global_T62_obs/${prefix_obs}mtiasi.${suffix}   ./iasibufr
ln -s -f $global_T62_obs/${prefix_obs}esamua.${suffix}   ./amsuabufrears
ln -s -f $global_T62_obs/${prefix_obs}esamub.${suffix}   ./amsubbufrears
ln -s -f $global_T62_obs/${prefix_obs}eshrs3.${suffix}   ./hirs3bufrears
ln -s -f $global_T62_obs/${prefix_obs}ssmit.${suffix}    ./ssmitbufr
ln -s -f $global_T62_obs/${prefix_obs}amsre.${suffix}    ./amsrebufr
ln -s -f $global_T62_obs/${prefix_obs}ssmis.${suffix}    ./ssmisbufr
ln -s -f $global_T62_obs/${prefix_obs}syndata.tcvitals.tm00 ./tcvitl


# Copy bias correction, atmospheric and surface files
ln -s -f $global_T62_ges/${prefix_tbc}.abias              ./satbias_in
ln -s -f $global_T62_ges/${prefix_tbc}.satang             ./satbias_angle

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $global_T62_ges/${prefix_sfc}.bf03            ./sfcf03
   ln -s -f $global_T62_ges/${prefix_sfc}.bf06            ./sfcf06
   ln -s -f $global_T62_ges/${prefix_sfc}.bf09            ./sfcf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $global_T62_ges/${prefix_sfc}.bf03.le         ./sfcf03
   ln -s -f $global_T62_ges/${prefix_sfc}.bf06.le         ./sfcf06
   ln -s -f $global_T62_ges/${prefix_sfc}.bf09.le         ./sfcf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $global_T62_obs/${prefix_atm}.sgm3prep        ./sigf03
   ln -s -f $global_T62_obs/${prefix_atm}.sgesprep        ./sigf06
   ln -s -f $global_T62_obs/${prefix_atm}.sgp3prep        ./sigf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $global_T62_obs/${prefix_atm}.sgm3prep.le     ./sigf03
   ln -s -f $global_T62_obs/${prefix_atm}.sgesprep.le     ./sigf06
   ln -s -f $global_T62_obs/${prefix_atm}.sgp3prep.le     ./sigf09
fi

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$arch" = "Linux" ]]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$arch" = "AIX" ]]; then

   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit




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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${global_T62_adate}
         compress diag_${type}_${string}.${global_T62_adate}
         $ncp diag_${type}_${string}.${global_T62_adate}.Z $savdir/
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
