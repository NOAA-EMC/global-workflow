#!/bin/sh
#$ -j y
#$ -cwd
#$ -l h_rt=01:00:00
#$ -A rtgfsenkf
#$ -N run_gsi_hybrid
#$ -o run_gsi_hybrid.out
#$ -e run_gsi_hybrid.err
#$ -pe thfip 128
#$ -S /bin/sh

echo "Time starting at `date` "

VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]]; then
   set -x
fi
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
export OMP_STACKSIZE=${OMP_STACKSIZE:-256M}
export machine=${machine:='wcoss'}

# Set experiment name and analysis date
adate=${analdate:-2010081900}
nens=${nanals:-80}

HXONLY=${HXONLY:-"NO"}
HRLY_BKG=${HRLY_BKG:-"YES"}
DOSFCANL=${DOSFCANL:-"YES"}
NOSAT=${NOSAT:-"NO"}
skipcat=${skipcat:-"false"}

nprocs=${nprocs:-$PBS_NP}



# Set path/file for gsi executable
basedir=${HOMEGLOBAL}
gsipath=${gsipath:-${basedir}/gsi}
gsiexec=${gsiexec:-$gsipath/EXP-port/src/global_gsi}
angupdatexec=${angupdatexec:- $gsipath/util/global_angupdate/global_angupdate}
charnanal=${charnanal:-'ensmean'}
# name just for diag files.
charnanal2=${charnanal2:-$charnanal}

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`${incdate} $adate -${ANALINC}`
hha=`echo $adate | cut -c9-10`
hham1=`echo $analdatem1 | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
RUN=${RUN:-gfs}
prefix_obs=${RUN}.t${hha}z
prefix_obsm1=${RUN}.t${hham1}z
prefix_tbc=${RUN}.t${hhg}z
suffix=tm00.bufr_d

datges=${datapath2:-/lfs1/projects/fim/whitaker/gfsenkf_test/$adate}
datgesm1=${datapathm1:-/lfs1/projects/fim/whitaker/gfsenkf_test/$gdate}
adate0=`echo $adate | cut -c1-8`
echo "adate = $adate"
iy=`echo $adate | cut -c1-4`
im=`echo $adate | cut -c5-6`
id=`echo $adate | cut -c7-8`
ih=`echo $adate | cut -c9-10`
echo "iy,im,id,ih = $iy $im $id $ih"
fdatei=`$nemsioget ${datges}/bfg_${adate}_fhr03_${charnanal} idate | tail -1 | cut -f2 -d"="`
fhr=`$nemsioget ${datges}/bfg_${adate}_fhr03_${charnanal} nfhour | cut -f2 -d"="`
fdatev=`${incdate} $fdatei $fhr`
echo "fdatei=$fdatei fhr=$fhr fdatev=$fdatev"
gdate0=`echo $gdate | cut -c1-8`
obs_datapath=${obs_datapath:-/lfs1/projects/fim/whitaker/bufr}
datobs=$obs_datapath/bufr_$adate
datobsm1=$obs_datapath/bufr_$analdatem1

# Set runtime and save directories
tmpdir=${tmpdir:-$datges/gsitmp$$}
mkdir -p $tmpdir
savdir=${savdir:-$datges}

# Specify GSI fixed field and data directories.
export fixgsi=${fixgsi:-$gsipath/fix}
export fixcrtm=${fixcrtm:-$basedir/nwprod/CRTM_Coefficients}
export HOMEGLOBAL=${HOMEGLOBAL:-/lfs1/projects/fim/whitaker/gfsenkf}
export FIXGLOBAL=${FIXGLOBAL:-$HOMEGLOBAL/fix}
export EXECGLOBAL=${EXECGLOBAL:-$HOMEGLOBAL/bin}
export CYCLESH=${CYCLESH:-$gsipath/scripts/global_cyclep.sh}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

ncp="/bin/cp -f"
nmv="/bin/mv -f"
nln="/bin/ln -fs"

# copy symlinks if needed.
if [[ "$lread_obs_save" = ".false." && "$HXONLY" = "YES" ]]; then
tmpdir_ensmean=${datges}/gsitmp_ensmean
mkdir -p $tmpdir
for filein in ${tmpdir_ensmean}/obs_input*; do
  file=`basename ${filein}`
  ln -fs $filein ${tmpdir}/${file}
done
for filein in ${tmpdir_ensmean}/*bin; do
  file=`basename ${filein}`
  /bin/cp -a $filein ${tmpdir}/${file}
done
for filein in ${tmpdir_ensmean}/*bufr; do
  file=`basename ${filein}`
  /bin/cp -a $filein ${tmpdir}/${file}
done
for filein in ${tmpdir_ensmean}/*bufrears; do
  file=`basename ${filein}`
  /bin/cp -a $filein ${tmpdir}/${file}
done
for filein in ${tmpdir_ensmean}/*bufrears; do
  file=`basename ${filein}`
  /bin/cp -a $filein ${tmpdir}/${file}
done
/bin/cp -a ${tmpdir_ensmean}/tcvitals ${tmpdir}/tcvitals
/bin/cp -a ${tmpdir_ensmean}/satbias_in ${tmpdir}/satbias_in
/bin/cp -a ${tmpdir_ensmean}/satbias_angle ${tmpdir}/satbias_angle
fi

# go to $tmpdir
cd $tmpdir

echo "Time before global cycle `date` "

# do surface analysis (twice, one at beginning of window and one in middle of window)
# cat the current and previous analysis time sst,snow and ice files to make
# sure -3 hour time is found.

# change date in grib file (hack so that global_cycle finds analysis time)
#/contrib/anaconda/2.3.0/bin/python ${enkfscripts}/fixgrb.py $datobs/${prefix_obs}.sstgrb sstgrb $fdatev
#export FNTSFA=sstgrb
#/home/Jeffrey.S.Whitaker/bin/wgrib -PDS10 sstgrb
#/contrib/anaconda/2.3.0/bin/python ${enkfscripts}/fixgrb.py $datobs/${prefix_obs}.engicegrb icegrb $fdatev
#export FNACNA=icegrb
#/contrib/anaconda/2.3.0/bin/python ${enkfscripts}/fixgrb.py $datobs/${prefix_obs}.snogrb snogrb $fdatev
#export FNSNOA=snogrb
export FNTSFA=$datobs/${prefix_obs}.sstgrb
export FNACNA=$datobs/${prefix_obs}.engicegrb
export FNSNOA=$datobs/${prefix_obs}.snogrb 

if [[ "$IAU" = ".true." ]]; then
   export KEEPFH="YES"
   SFCG03=${SFCG03:-$datges/bfg_${adate}_fhr03_${charnanal}}
   SFCANLm3=${SFCANLm3:-$savdir/sfcanlm3.${adate}}
   if [[ "$DOSFCANL" = "YES" && "$SFCANLm3" != "NO" ]]; then
   # change date of 3-h forecast to analysis time
   #time ${utilexec}/chgdatesfc.x $SFCG03 ${analdate} 0 $tmpdir/sfcf03
   $nln $SFCG03               ./sfcf03
   time $CYCLESH ./sfcf03 sfcanlm3 
   # change date of resulting sfc anl back to 3-h forecast
   ls -l $tmpdir/sfcanlm3
   #time ${utilexec}/chgdatesfc.x $tmpdir/sfcanlm3 ${analdatem1} 3 $tmpdir/sfcanlm3
   #ls -l $tmpdir/sfcanlm3
   rc=$?
   if [[ $rc -ne 0 ]];then
     echo "global_cycle failed with exit code $rc"
     exit $rc
   fi
   if  [ ! -s ./sfcanlm3 ]; then
     echo "sfcanl file missing, exiting ..."
     exit 1
   fi
   /bin/rm -f ./sfcf03
   fi
fi   

export KEEPFH="NO"
SFCG06=${SFCG06:-$datges/bfg_${adate}_fhr06_${charnanal}}
$nln $SFCG06               ./sfcf06
if [[ "$DOSFCANL" = "YES" ]]; then
SFCANL=${SFCANL:-$savdir/sfcanl.${adate}}
time $CYCLESH $SFCG06 sfcanl
rc=$?
if [[ $rc -ne 0 ]];then
  echo "global_cycle failed with exit code $rc"
  exit $rc
fi
if  [ ! -s ./sfcanl ]; then
  echo "sfcanl file missing, exiting ..."
  exit 1
fi
fi

if [[ "$DOSFCANL" = "YES" ]]; then
   /bin/cp -f sfcanl  $SFCANL
   $nemsioget $SFCANL idate
   $nemsioget $SFCANL nfhour
   if [[ "$IAU" = ".true." ]]; then
      /bin/cp -f sfcanlm3  $SFCANLm3
      $nemsioget $SFCANLm3 idate
      $nemsioget $SFCANLm3 nfhour
   fi
   #exit 0
fi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP_A=${JCAP_A:-$JCAP}
export LEVS=${LEVS:-64}
export JCAP_B=${JCAP_B:-$JCAP}


export NLAT=$((${LATA}+2))
JCAP_ENS=${JCAP_ENS:-$JCAP}
LATA_ENS=${LATA_ENS:-$LATA}
LONA_ENS=${LONA_ENS:-$LONA}
export NLAT_ENS=$((${LATA_ENS}+2))


SIGANL=${SIGANL:-$savdir/siganl.${adate}}
SATANGO=${SATANGO:-$savdir/${RUN}.t${hha}z.satang}
BIASO=${BIASO:-$savdir/${RUN}.t${hha}z.abias}
BIASOAIR=${BIASOAIR:-$savdir/${RUN}.t${hha}z.abias_air}
BIASO_PC=${BIASO_PC:-$savdir/${RUN}.t${hha}z.abias_pc}


# Make gsi namelist
# CO2 namelist and file decisions
#ICO2=${ICO2:-0}
#if [ $ICO2 -gt 0 ] ; then
#        # Copy co2 files to $tmpdir
#        co2dir=${CO2DIR:-$fixgsi}
#        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
#        rm ./global_co2_data.txt
#        while [ $yyyy -ge 1957 ] ;do
#                co2=$co2dir/global_co2historicaldata_$yyyy.txt
#                #co2=$co2dir/global_co2.gcmscl_$yyyy.txt
#                if [ -s $co2 ] ; then
#                        $ncp $co2 ./global_co2_data.txt
#                break
#                fi
#                ((yyyy-=1))
#        done
#        if [ ! -s ./global_co2_data.txt ] ; then
#                echo "\./global_co2_data.txt" not created
#                exit 1
#   fi
#fi

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

if [[ "$IAU" = ".true." ]]; then
   lwrite4danl=.true.
else
   lwrite4danl=.false.
fi

SETUP="reduce_diag=.false.,lwrite_peakwt=.false.,lread_obs_save=$lread_obs_save,lread_obs_skip=$lread_obs_skip,l4densvar=.true.,ens_nstarthr=3,iwrtinc=-1,nhr_assimilation=6,nhr_obsbin=$FHOUT,use_prepb_satwnd=.false.,lwrite4danl=$lwrite4danl,passive_bc=.true.,newpc4pred=.true.,adp_anglebc=.true.,angord=4,use_edges=.false.,diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,lobsdiag_forenkf=.false."

if [[ "$HXONLY" = "YES" ]]; then
   SETUP="$SETUP,miter=0,niter=1,reduce_diag=.true.,lwrite_peakwt=.true.,lobsdiag_forenkf=$lobsdiag_forenkf"
fi
STRONGOPTS="tlnmc_option=3,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,baldiag_full=.true.,baldiag_inc=.true.,"
# no strong bal constraint
#STRONGOPTS="tlnmc_option=0,nstrong=0,nvmodes_keep=0,baldiag_full=.false.,baldiag_inc=.false.,"
if [[ "$HXONLY" = "YES" ]]; then
   STRONGOPTS="tlnmc_option=0,nstrong=0,nvmodes_keep=0,baldiag_full=.false.,baldiag_inc=.false.,"
fi
# no strong bal constraint
#STRONGOPTS="tlnmc_option=0,nstrong=0,nvmodes_keep=0,baldiag_full=.false.,baldiag_inc=.false.,"
if [[ $beta1_inv > 0.999 ]]; then
   STRONGOPTS="tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,baldiag_full=.true.,baldiag_inc=.true.,"
fi
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
#  use tcv_mod, only: init_tcps_errvals,tcp_refps,tcp_width,tcp_ermin,tcp_ermax
OBSQC="tcp_ermax=3.0,aircraft_t_bc=.true.,biaspredt=1000.0,upd_aircraft=.true." # error variance goes from tcp_ermin (when O-F=0) to tcp_ermax (when O-F=tcp_width=50)
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
LAGDATA=""
RAPIDREFRESH_CLDSURF=""
CHEM=""
#      l_hyb_ens:  logical variable, if .true., then turn on hybrid ensemble option, default = .false. 
#      n_ens:      ensemble size, default = 0
#      beta1_inv:  value between 0 and 1, relative weight given to static background B, default = 1.0
#      s_ens_h:    horizontal localization correlation length (units of km), default = 2828.0
#      s_ens_v:    vertical localization correlation length (grid units), default = 30.0
#      generate_ens:  if .true., generate ensemble perturbations internally as random samples of background B.
#                       (used primarily for testing/debugging)
#                     if .false., read external ensemble perturbations (not active yet)
#      aniso_a_en: if .true., then allow anisotropic localization correlation (not active yet)
#      uv_hyb_ens: if .true., then ensemble perturbation wind stored as u,v
#                  if .false., ensemble perturbation wind stored as psi,chi.
#                   (this is useful for regional application, where there is ambiguity in how to
#                      define psi,chi from u,v)
beta1_inv=${beta1_inv:-0.25}
s_ens_h=${s_ens_h:-800}
s_ens_v=${s_ens_v:-0.8}
if [ "$HXONLY" = "NO" ] && [[ $beta1_inv < 0.999 ]]; then
HYBRIDENSDATA="l_hyb_ens=.true.,n_ens=$nens,beta1_inv=$beta1_inv,s_ens_h=$s_ens_h,s_ens_v=$s_ens_v,generate_ens=.false.,uv_hyb_ens=.true.,jcap_ens=$JCAP_ENS,nlat_ens=$NLAT_ENS,nlon_ens=$LONA_ENS,aniso_a_en=.false.,jcap_ens_test=$JCAP_ENS,readin_localization=$readin_localization,write_ens_sprd=.false.,oz_univ_static=.false.,q_hyb_ens=.false."
else
HYBRIDENSDATA=""
fi

# Create global_gsi namelist
cat <<EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=150,
   niter_no_qc(1)=25,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.true.
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP_A,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.false.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.false.,bamp_jcpdry=5.0e7,ljc4tlevs=.true.
   $JCOPTS
 /
 &STRONGOPTS
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.true.,c_varqc=0.02,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,
   $OBSQC
 /
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,time_window_max=3.0,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                 dval    dthin  dsfcalc
   prepbufr       ps          null        ps                   0.0     0      0
   prepbufr       t           null        t                    0.0     0      0
   prepbufr       q           null        q                    0.0     0      0
   prepbufr       pw          null        pw                   0.0     0      0
   prepbufr_profl t           null        t                    0.0     0     0
   prepbufr_profl q           null        q                    0.0     0     0
   prepbufr_profl uv          null        uv                   0.0     0     0
   satwndbufr     uv          null        uv                   0.0     0      0
   prepbufr       uv          null        uv                   0.0     0      0
   prepbufr       spd         null        spd                  0.0     0      0
   prepbufr       dw          null        dw                   0.0     0      0
   radarbufr      rw          null        rw                   0.0     0      0
   prepbufr       sst         null        sst                  0.0     0      0
   gpsrobufr      gps_bnd     null        gps                  0.0     0      0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi             0.0    -1      0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi              0.0    -1      0
   sbuvbufr       sbuv2       n11         sbuv8_n11            0.0     0      0
   sbuvbufr       sbuv2       n14         sbuv8_n14            0.0     0      0
   sbuvbufr       sbuv2       n16         sbuv8_n16            0.0     0      0
   sbuvbufr       sbuv2       n17         sbuv8_n17            0.0     0      0
   sbuvbufr       sbuv2       n18         sbuv8_n18            0.0     0      0
   sbuvbufr       sbuv2       n19         sbuv8_n19            0.0     0      0
   hirs2bufr      hirs2       n14         hirs2_n14            0.0     1      1
   hirs3bufr      hirs3       n15         hirs3_n15            0.0     1      1
   hirs3bufr      hirs3       n16         hirs3_n16            0.0     1      1
   hirs3bufr      hirs3       n17         hirs3_n17            0.0     1      1
   hirs4bufr      hirs4       metop-a     hirs4_metop-a        0.0     1      1
   gimgrbufr      goes_img    g11         imgr_g11             0.0     1      0
   gimgrbufr      goes_img    g12         imgr_g12             0.0     1      0
   airsbufr       airs        aqua        airs281SUBSET_aqua   0.0     1      1
   msubufr        msu         n14         msu_n14              0.0     1      1
   ssubufr        ssu         n14         ssu_n14              0.0     1      1
   amsuabufr      amsua       n15         amsua_n15            0.0     1      1
   amsuabufr      amsua       n16         amsua_n16            0.0     1      1
   amsuabufr      amsua       n17         amsua_n17            0.0     1      1
   amsuabufr      amsua       n18         amsua_n18            0.0     1      1
   amsuabufr      amsua       metop-a     amsua_metop-a        0.0     1      1
   airsbufr       amsua       aqua        amsua_aqua           0.0     1      1
   amsubbufr      amsub       n15         amsub_n15            0.0     1      1
   amsubbufr      amsub       n16         amsub_n16            0.0     1      1
   amsubbufr      amsub       n17         amsub_n17            0.0     1      1
   mhsbufr        mhs         n18         mhs_n18              0.0     1      1
   mhsbufr        mhs         metop-a     mhs_metop-a          0.0     1      1
   ssmitbufr      ssmi        f14         ssmi_f14             0.0     1      0
   ssmitbufr      ssmi        f15         ssmi_f15             0.0     1      0
   amsrebufr      amsre_low   aqua        amsre_aqua           0.0     1      0
   amsrebufr      amsre_mid   aqua        amsre_aqua           0.0     1      0
   amsrebufr      amsre_hig   aqua        amsre_aqua           0.0     1      0
   ssmisbufr      ssmis       f16         ssmis_f16            0.0     1      0
   gsnd1bufr      sndr        g08         sndr_g08             0.0     1      0
   gsnd1bufr      sndr        g09         sndr_g09             0.0     1      0
   gsnd1bufr      sndr        g10         sndr_g10             0.0     1      0
   gsnd1bufr      sndr        g11         sndr_g11             0.0     1      0
   gsnd1bufr      sndr        g12         sndr_g12             0.0     1      0
   gsnd1bufr      sndrd1      g12         sndrD1_g12           0.0     1      0
   gsnd1bufr      sndrd2      g12         sndrD2_g12           0.0     1      0
   gsnd1bufr      sndrd3      g12         sndrD3_g12           0.0     1      0
   gsnd1bufr      sndrd4      g12         sndrD4_g12           0.0     1      0
   gsnd1bufr      sndrd1      g11         sndrD1_g11           0.0     1      0
   gsnd1bufr      sndrd2      g11         sndrD2_g11           0.0     1      0
   gsnd1bufr      sndrd3      g11         sndrD3_g11           0.0     1      0
   gsnd1bufr      sndrd4      g11         sndrD4_g11           0.0     1      0
   gsnd1bufr      sndrd1      g13         sndrD1_g13           0.0     1      0
   gsnd1bufr      sndrd2      g13         sndrD2_g13           0.0     1      0
   gsnd1bufr      sndrd3      g13         sndrD3_g13           0.0     1      0
   gsnd1bufr      sndrd4      g13         sndrD4_g13           0.0     1      0
   iasibufr       iasi        metop-a     iasi616_metop-a      0.0     1      1
   gomebufr       gome        metop-a     gome_metop-a         0.0     2      0
   omibufr        omi         aura        omi_aura             0.0     2      0
   hirs4bufr      hirs4       n18         hirs4_n18            0.0     1      1
   hirs4bufr      hirs4       n19         hirs4_n19            0.0     1      1
   amsuabufr      amsua       n19         amsua_n19            0.0     1      1
   mhsbufr        mhs         n19         mhs_n19              0.0     1      1
   tcvitl         tcp         null        tcp                  0.0     0      0
   seviribufr     seviri      m08         seviri_m08           0.0     1      0
   seviribufr     seviri      m09         seviri_m09           0.0     1      0
   seviribufr     seviri      m10         seviri_m10           0.0     1      0
   hirs4bufr      hirs4       metop-b     hirs4_metop-b        0.0     1      0
   amsuabufr      amsua       metop-b     amsua_metop-b        0.0     1      0
   mhsbufr        mhs         metop-b     mhs_metop-b          0.0     1      0
   iasibufr       iasi        metop-b     iasi616_metop-b      0.0     1      0
   gomebufr       gome        metop-b     gome_metop-b         0.0     2      0
   atmsbufr       atms        npp         atms_npp             0.0     1      0
   crisbufr       cris        npp         cris_npp             0.0     1      0
::
   $OBSINPUT
 /
 &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   $HYBRIDENSDATA
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
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${CDATE},
   obhourset=0.,
   $SINGLEOB
 /
 &NST
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

berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
# temporary fix until jeff moved this file into the master location
#if [ $JCAP -eq 1152 ]; then
#   berror=/lfs1/projects/gfsenkf/fix/global_berror.l64y1154.f77
#fi
#emiscoef_IRwater=$fixcrtm/EmisCoeff/IR_Water/Big_Endian/Nalli.IRwater.EmisCoeff.bin   
#emiscoef_IRice=$fixcrtm/EmisCoeff/IR_Ice/SEcategory/Big_Endian/NPOESS.IRice.EmisCoeff.bin               
#emiscoef_IRland=$fixcrtm/EmisCoeff/IR_Land/SEcategory/Big_Endian/NPOESS.IRland.EmisCoeff.bin
#emiscoef_IRsnow=$fixcrtm/EmisCoeff/IR_Snow/SEcategory/Big_Endian/NPOESS.IRsnow.EmisCoeff.bin             
#emiscoef_VISice=$fixcrtm/EmisCoeff/VIS_Ice/SEcategory/Big_Endian/NPOESS.VISice.EmisCoeff.bin             
#emiscoef_VISland=$fixcrtm/EmisCoeff/VIS_Land/SEcategory/Big_Endian/NPOESS.VISland.EmisCoeff.bin                   
#emiscoef_VISsnow=$fixcrtm/EmisCoeff/VIS_Snow/SEcategory/Big_Endian/NPOESS.VISsnow.EmisCoeff.bin                   
#emiscoef_VISwater=$fixcrtm/EmisCoeff/VIS_Water/SEcategory/Big_Endian/NPOESS.VISwater.EmisCoeff.bin                 
#emiscoef_MWwater=$fixcrtm/EmisCoeff/MW_Water/Big_Endian/FASTEM6.MWwater.EmisCoeff.bin
emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin   
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin               
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin             
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin             
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin                   
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin                   
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin                 
emiscoef_MWwater=$fixcrtm/FASTEM6.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
satinfo=${SATINFO:-$fixgsi/global_satinfo.txt}
atmsfilter=${ATMSFILTER:-$fixgsi/atms_beamwidth.txt}
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=${OZINFO:-$fixgsi/global_ozinfo.txt}
convinfo=${CONVINFO:-$fixgsi/global_convinfo.txt}
errtable=$fixgsi/prepobs_errtable.global
anavinfo=${ANAVINFO:-$fixgsi/global_anavinfo.l64.txt}


# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ "$lread_obs_skip" = ".false." ]]; then

$nln $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$nln $berror   ./berror_stats
$ncp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
$ncp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin               
$ncp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin             
$ncp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin             
$ncp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin             
$ncp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin           
$ncp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin           
$ncp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin                 
$ncp $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
$nln $aercoef  ./AerosolCoeff.bin
$nln $cldcoef  ./CloudCoeff.bin
$nln $satangl  ./satbias_angle
$nln $satinfo  ./satinfo
$nln $atmsfilter ./atms_beamwidth.txt
$nln $scaninfo ./scaninfo
$nln $pcpinfo  ./pcpinfo
$nln $ozinfo   ./ozinfo
$nln $convinfo ./convinfo
$nln $errtable ./errtable

$nln $bufrtable ./prepobs_prep.bufrtable
$nln $bftab_sst ./bftab_sstphr


GBIAS=${GBIAS:-$datgesm1/${prefix_tbc}.abias}
GBIAS_PC=${GBIAS_PC:-$datgesm1/${prefix_tbc}.abias_pc}
GBIASAIR=${GBIASAIR:-$datgesm1/${prefix_tbc}.abias_air}
GSATANG=${GSATANG:-$datgesm1/${prefix_tbc}.satang}

#coeffiles=`ls -1 ${fixcrtm}/SpcCoeff/Big_Endian/*.SpcCoeff.bin`
#for coeffile in $coeffiles;  do
#for coeffile in ${fixcrtm}/SpcCoeff/Big_Endian/*.SpcCoeff.bin;  do
for coeffile in ${fixcrtm}/*.SpcCoeff.bin;  do
    satsen=`basename $coeffile .SpcCoeff.bin`
    count=`grep -c $satsen $satinfo`
    if [[ $count -gt 0 ]]; then
    spccoeff=${satsen}.SpcCoeff.bin
    #if  [[ -s $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ]]; then
    #   #$nln $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
    if  [[ -s $fixcrtm/$spccoeff ]]; then
       $nln $fixcrtm/$spccoeff ./
    fi
    taucoeff=${satsen}.TauCoeff.bin
    #if  [[ -s $fixcrtm/TauCoeff/Big_Endian/$taucoeff ]]; then
       #$nln $fixcrtm/TauCoeff/Big_Endian/$taucoeff ./
    if  [[ -s $fixcrtm/$taucoeff ]]; then
       $nln $fixcrtm/$taucoeff ./
    fi
    fi
done
#ls -l *bin


# Copy observational data to $tmpdir
if [[ ! -s $datobs/${prefix_obs}.prepbufr ]]; then
 echo "no prepbufr file!"
 exit 1
fi 

$nln $datobs/${prefix_obs}.prepbufr           ./prepbufr
if [[ -s $datobs/${prefix_obs}.prepbufr.acft_profiles ]]; then
$nln $datobs/${prefix_obs}.prepbufr.acft_profiles ./prepbufr_profl
fi
if [[ -s $datobs/${prefix_obs}.syndata.tcvitals.tm00 ]]; then
$nln $datobs/${prefix_obs}.syndata.tcvitals.tm00     ./tcvitl
fi
if [[ -s $datobs/${prefix_obs}.gpsro.${suffix} ]]; then
$nln $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
fi
if [[ -s $datobs/${prefix_obs}.satwnd.${suffix} ]]; then
$nln $datobs/${prefix_obs}.satwnd.${suffix}      ./satwndbufr
fi

if [[ "$NOSAT" = "NO" ]]; then
if [[ -s $datobs/${prefix_obs}.osbuv8.${suffix} ]]; then
$nln $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
fi
if [[ -s $datobs/${prefix_obs}.1bamua.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
fi
if [[ -s $datobs/${prefix_obs}.1bmsu.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bmsu.${suffix}   ./msubufr
fi
if [[ -s $datobs/${prefix_obs}.1bssu.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bssu.${suffix}   ./ssubufr
fi
if [[ -s $datobs/${prefix_obs}.1bmhs.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
fi
if [[ -s $datobs/${prefix_obs}.esamua.${suffix} ]]; then
$nln $datobs/${prefix_obs}.esamua.${suffix}   ./amsuabufrears
fi
if [[ -s $datobs/${prefix_obs}.atms.${suffix} ]]; then
$nln $datobs/${prefix_obs}.atms.${suffix}      ./atmsbufr
fi
if [[ -s $datobs/${prefix_obs}.goesnd.${suffix} ]]; then
$nln $datobs/${prefix_obs}.goesnd.${suffix}   ./gsnd1bufr
fi
if [[ -s $datobs/${prefix_obs}.goesnd.${suffix} ]]; then
$nln $datobs/${prefix_obs}.geoimr.${suffix}   ./gimgrbufr
fi
if [[ -s $datobs/${prefix_obs}.1bamub.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
fi
if [[ -s $datobs/${prefix_obs}.1bhrs2.${suffix}  ]]; then
$nln $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
fi
if [[ -s $datobs/${prefix_obs}.1bhrs3.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
fi
if [[ -s $datobs/${prefix_obs}.1bhrs4.${suffix} ]]; then
$nln $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
fi
if [[ -s $datobs/${prefix_obs}.airsev.${suffix} ]]; then
$nln $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
fi
if [[ -s $datobs/${prefix_obs}.mtiasi.${suffix} ]]; then
$nln $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
fi
if [[ -s $datobs/${prefix_obs}.ssmit.${suffix} ]]; then
$nln $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
fi
if [[ -s  $datobs/${prefix_obs}.amsre.${suffix} ]]; then
$nln $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
fi
if [[ -s $datobs/${prefix_obs}.ssmis.${suffix} ]]; then
$nln $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr
fi
if [[ -s $datobs/${prefix_obs}.ssmit.${suffix} ]]; then
$nln $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
fi
if [[ -s $datobs/${prefix_obs}.eshrs3.${suffix} ]]; then
$nln $datobs/${prefix_obs}.eshrs3.${suffix}   ./hirs3bufrears
fi
if [[ -s $datobs/${prefix_obs}.esamub.${suffix} ]]; then
$nln $datobs/${prefix_obs}.esamub.${suffix}   ./amsubbufrears
fi
if [[ -s $datobs/${prefix_obs}.eshrs3.${suffix} ]]; then
$nln $datobs/${prefix_obs}.eshrs3.${suffix}   ./hirs3bufrears
fi
if [[ -s $datobs/${prefix_obs}.gome.${suffix} ]]; then
$nln $datobs/${prefix_obs}.gome.${suffix}     ./gomebufr
fi
if [[ -s $datobs/${prefix_obs}.omi.${suffix} ]]; then
$nln $datobs/${prefix_obs}.omi.${suffix}      ./omibufr
fi
if [[ -s $datobs/${prefix_obs}.sevcsr.${suffix} ]]; then
$nln $datobs/${prefix_obs}.sevcsr.${suffix}      ./seviribufr
fi
if [[ -s $datobs/${prefix_obs}.cris.${suffix} ]]; then
$nln $datobs/${prefix_obs}.cris.${suffix}      ./crisbufr
fi
if [[ -s $datobs/${prefix_obs}.spssmi.${suffix} ]]; then
$nln $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
fi
if [[ -s $datobs/${prefix_obs}.sptrmm.${suffix} ]]; then
$nln $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
fi
fi # NOSAT

# link bias correction, atmospheric and surface files
$nln $GBIAS              ./satbias_in
$nln $GBIAS_PC           ./satbias_pc
$nln $GSATANG            ./satbias_angle
$nln $GBIASAIR           ./aircftbias_in

SFCG03=${SFCG03:-$datges/bfg_${adate}_fhr03_${charnanal}}
$nln $SFCG03               ./sfcf03
SFCG09=${SFCG09:-$datges/bfg_${adate}_fhr09_${charnanal}}
$nln $SFCG09               ./sfcf09

SIGG03=${SIGG03:-$datges/sfg_${adate}_fhr03_${charnanal}}
$nln $SIGG03               ./sigf03
SIGG06=${SIGG06:-$datges/sfg_${adate}_fhr06_${charnanal}}
$nln $SIGG06               ./sigf06
SIGG09=${SIGG09:-$datges/sfg_${adate}_fhr09_${charnanal}}
$nln $SIGG09               ./sigf09

if [[ "$HRLY_BKG" = "YES" ]]; then
SFCG04=${SFCG04:-$datges/bfg_${adate}_fhr04_${charnanal}}
$nln $SFCG04               ./sfcf04
SFCG05=${SFCG05:-$datges/bfg_${adate}_fhr05_${charnanal}}
$nln $SFCG05               ./sfcf05
SFCG07=${SFCG07:-$datges/bfg_${adate}_fhr07_${charnanal}}
$nln $SFCG07               ./sfcf07
SFCG08=${SFCG08:-$datges/bfg_${adate}_fhr08_${charnanal}}
$nln $SFCG08               ./sfcf08
SIGG04=${SIGG04:-$datges/sfg_${adate}_fhr04_${charnanal}}
$nln $SIGG04               ./sigf04
SIGG05=${SIGG05:-$datges/sfg_${adate}_fhr05_${charnanal}}
$nln $SIGG05               ./sigf05
SIGG07=${SIGG07:-$datges/sfg_${adate}_fhr07_${charnanal}}
$nln $SIGG07               ./sigf07
SIGG08=${SIGG08:-$datges/sfg_${adate}_fhr08_${charnanal}}
$nln $SIGG08               ./sigf08
fi

if [[ $beta1_inv < 0.999 ]]; then
ln -s $datges/ensmem*.pe* .
ln -s $datges/control*.pe* .
fh=3
while [ $fh -le 9 ] ;do
for ensfile in $datges/sfg_${adate}*fhr0${fh}*mem???; do
 ensfilename=`basename $ensfile`
 memnum=`echo $ensfilename | cut -f4 -d"_" | cut -c4-6`
 $nln $ensfile ./sigf0${fh}_ens_mem${memnum}
done
((fh+=$FHOUT))
done
fi
fi

# make symlinks for diag files to initialize angle dependent bias correction for new channels.
satdiag="ssu_n14 hirs2_n14 msu_n14 sndr_g08 sndr_g09 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs2_n14 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 gome_metop-a omi_aura mls_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 ssmis_las_f17 ssmis_uas_f17 ssmis_img_f17 ssmis_env_f17 ssmis_las_f18 ssmis_uas_f18 ssmis_img_f18 ssmis_env_f18 ssmis_las_f19 ssmis_uas_f19 ssmis_img_f19 ssmis_env_f19 ssmis_las_f20 ssmis_uas_f20 ssmis_img_f20 ssmis_env_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp atms_npp hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b gome_metop-b"
alldiag="$satdiag pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n11 sbuv2_n14 sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19"
string='ges'
for type in $satdiag; do
    if [[ "$cold_start_bias" = "true" ]]; then
       if [ -s $datges/diag_${type}_${string}.${adate}_${charnanal2} ]; then
         ln -fs $datges/diag_${type}_${string}.${adate}_${charnanal2} diag_${type}
       fi
    fi
done

# Run gsi.
#if [ -s ./satbias_in ] && [ -s ./satbias_angle ] && [ -s ./sfcf03 ] && [ -s ./sfcf06 ] && [ -s ./sfcf09 ] && [ -s ./sigf03 ] && [ -s ./sigf06 ] && [ -s ./sigf09 ] ; then
if [ -s ./satbias_in ] && [ -s ./sfcf03 ] && [ -s ./sfcf06 ] && [ -s ./sfcf09 ] && [ -s ./sigf03 ] && [ -s ./sigf06 ] && [ -s ./sigf09 ] ; then
cat gsiparm.anl
ulimit -s unlimited

pwd
ls -l
echo "Time before GSI `date` "
export PGM=$tmpdir/gsi.x
sh ${enkfscripts}/runmpi
rc=$?
if [[ $rc -ne 0 ]];then
  echo "GSI failed with exit code $rc"
  exit $rc
fi
else
echo "some input files missing, exiting ..."
ls -l ./satbias_in
ls -l ./satbias_angle
ls -l ./sfcf03
ls -l ./sfcf06
ls -l ./sfcf09
ls -l ./sfcanl
ls -l ./sigf03
ls -l ./sigf06
ls -l ./sigf09
exit 1
fi

# Save output
mkdir -p $savdir

#if [[ "$DOSFCANL" = "YES" ]]; then
#$nmv sfcanl  $SFCANL
#if [[ "$IAU" = ".true." ]]; then
#   $nmv sfcanlm3  $SFCANLm3
#fi
#fi
#$nmv ens_spread.grd $savdir

cat fort.2* > $savdir/gsistats.${adate}_${charnanal2}

#ls -l
if [[ "$HXONLY" = "NO" ]]; then
   if [ -s ./siganl ] && [ -s ./satbias_out ]; then
      if [ -s ./siga03 ]; then
         $nmv siga03          $SIGANL03
      fi
      if [ -s ./siga04 ]; then
         $nmv siga04          $SIGANL04
      fi
      if [ -s ./siga05 ]; then
         $nmv siga05          $SIGANL05
      fi
      $nmv siganl          $SIGANL
      ln -fs $SIGANL $SIGANL06
      if [ -s ./siga07 ]; then
          $nmv siga07          $SIGANL07
      fi
      if [ -s ./siga08 ]; then
         $nmv siga08          $SIGANL08
      fi
      if [ -s ./siga09 ]; then
         $nmv siga09          $SIGANL09
      fi
      $nmv satbias_out $BIASO
      $nmv satbias_pc.out $BIASO_PC
      if [ -s aircftbias_out ]; then
      $nmv aircftbias_out $BIASOAIR
      fi
  else
      exit 1
  fi
fi
#if [[ "$HXONLY" = "NO" ]]; then
#if [ -s ./siganl ] && [ -s ./satbias_out ]; then
#$nmv siganl          $SIGANL
##$ncp satbias_out     $savdir/biascr.${adate}
#$nmv satbias_out $BIASO
##$ncp sfcf06          $savdir/sfcf06.${gdate}
##$ncp sigf06          $savdir/sigf06.${gdate}
#else
#exit 1
#fi
#fi
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


if [[ "$skipcat" = "false" ]]; then

echo "Time before diagnostic loop is `date` "
if [[ "$HXONLY" = "YES" ]]; then
  loops="01"
else
  loops="01 03"
fi

for loop in $loops; do

   case $loop in
     01) string=ges;;
     03) string=anl;;
      *) string=$loop;;
   esac
   
   #  Collect diagnostic files for obs types (groups) below
   for type in $alldiag; do
       count=`ls pe*${type}_${loop}* | wc -l`
       if [[ $count -gt 0 ]]; then
          cat pe*${type}_${loop}* > $savdir/diag_${type}_${string}.${adate}_${charnanal2}
       fi
   done

done
echo "Time after diagnostic loop is `date` "

if [ ! -s $savdir/diag_conv_ges.${adate}_${charnanal2} ]; then
   exit 1
fi

fi # skipcat

if [[ "$SKIP_ANGUPDATE" = "YES" || "$NOSAT" = "YES" ]]; then
   # If requested, clean up $tmpdir
   if [[ "$CLEAN" = "YES" ]];then
     cd $tmpdir
     cd ../
     /bin/rm -rf $tmpdir
   fi
   exit 0
fi

$nln ./satbias_angle  ./satbias_ang.in
ls -l  ./satbias_ang.in
cat  ./satbias_ang.in
for type in $alldiag; do
   if [[ -s $savdir/diag_${type}_ges.${adate}_${charnanal2} ]]; then
      $nln $savdir/diag_${type}_ges.${adate}_${charnanal2}  ./diag_${type}.$adate
   fi
done
cat  > global_angupdate.namelist <<EOF
 &SETUP
  jpch=2680,nstep=90,nsize=20,wgtang=0.008333333,wgtlap=0.0,
  iuseqc=1,dtmax=1.0,
  iyy1=${iy},imm1=${im},idd1=${id},ihh1=${ih},
  iyy2=${iy},imm2=${im},idd2=${id},ihh2=${ih},
  dth=06,ndat=50,
  $SETUPANG
 /
 &OBS_INPUT
dtype(01)='hirs2',     dplat(01)='tirosn',   dsis(01)='hirs2_tirosn',
dtype(02)='hirs2',     dplat(02)='n06',       dsis(02)='hirs2_n06',
dtype(03)='hirs2',     dplat(03)='n07',       dsis(03)='hirs2_n07',
dtype(04)='hirs2',     dplat(04)='n08',       dsis(04)='hirs2_n08',
dtype(05)='hirs2',     dplat(05)='n09',       dsis(05)='hirs2_n09',
dtype(06)='hirs2',     dplat(06)='n10',       dsis(06)='hirs2_n10',
dtype(07)='hirs2',     dplat(07)='n11',       dsis(07)='hirs2_n11',
dtype(08)='hirs2',     dplat(08)='n12',       dsis(08)='hirs2_n12',
dtype(09)='hirs2',     dplat(09)='n14',       dsis(09)='hirs2_n14',
dtype(10)='hirs3',     dplat(10)='n15',       dsis(10)='hirs3_n15',
dtype(11)='hirs3',     dplat(11)='n16',       dsis(11)='hirs3_n16',
dtype(12)='hirs3',     dplat(12)='n17',       dsis(12)='hirs3_n17',
dtype(13)='hirs4',     dplat(13)='n19',       dsis(13)='hirs4_n19',
dtype(14)='hirs4',     dplat(14)='metop-a',   dsis(14)='hirs4_metop-a',
dtype(15)='sndr',      dplat(15)='g08',       dsis(15)='sndr_g08',
dtype(16)='sndr',      dplat(16)='g10',       dsis(16)='sndr_g10',
dtype(17)='sndr',      dplat(17)='g11',       dsis(17)='sndr_g11',
dtype(18)='sndr',      dplat(18)='g12',       dsis(18)='sndr_g12',
dtype(19)='sndr',      dplat(19)='g13',       dsis(19)='sndr_g13',
dtype(20)='sndr',      dplat(20)='g09',       dsis(20)='sndr_g09',
dtype(21)='goes_img',  dplat(21)='g10',       dsis(21)='imgr_g10',
dtype(22)='goes_img',  dplat(22)='g11',       dsis(22)='imgr_g11',
dtype(23)='goes_img',  dplat(23)='g12',       dsis(23)='imgr_g12',
dtype(24)='goes_img',  dplat(24)='g13',       dsis(24)='imgr_g13',
dtype(25)='airs',      dplat(25)='aqua',      dsis(25)='airs281SUBSET_aqua',
dtype(26)='msu',       dplat(26)='tirosn',   dsis(26)='msu_tirosn',
dtype(27)='msu',       dplat(27)='n06',       dsis(27)='msu_n06',
dtype(28)='msu',       dplat(28)='n07',       dsis(28)='msu_n07',
dtype(29)='msu',       dplat(29)='n08',       dsis(29)='msu_n08',
dtype(30)='msu',       dplat(30)='n09',       dsis(30)='msu_n09',
dtype(31)='msu',       dplat(31)='n10',       dsis(31)='msu_n10',
dtype(32)='msu',       dplat(32)='n11',       dsis(32)='msu_n11',
dtype(33)='msu',       dplat(33)='n12',       dsis(33)='msu_n12',
dtype(34)='msu',       dplat(34)='n14',       dsis(34)='msu_n14',
dtype(35)='amsua',     dplat(35)='n15',       dsis(35)='amsua_n15',
dtype(36)='amsua',     dplat(36)='n16',       dsis(36)='amsua_n16',
dtype(37)='amsua',     dplat(37)='n17',       dsis(37)='amsua_n17',
dtype(38)='amsua',     dplat(38)='n18',       dsis(38)='amsua_n18',
dtype(39)='amsua',     dplat(39)='metop-a',   dsis(39)='amsua_metop-a',
dtype(40)='amsua',     dplat(40)='aqua',      dsis(40)='amsua_aqua',
dtype(41)='amsub',     dplat(41)='n15',       dsis(41)='amsub_n15',
dtype(42)='amsub',     dplat(42)='n16',       dsis(42)='amsub_n16',
dtype(43)='amsub',     dplat(43)='n17',       dsis(43)='amsub_n17',
dtype(44)='mhs',       dplat(44)='n18',       dsis(44)='mhs_n18',
dtype(45)='mhs',       dplat(45)='metop-a',   dsis(45)='mhs_metop-a',
dtype(46)='ssmi',      dplat(46)='f08',       dsis(46)='ssmi_f08',
dtype(47)='ssmi',      dplat(47)='f10',       dsis(47)='ssmi_f10',
dtype(48)='ssmi',      dplat(48)='f11',       dsis(48)='ssmi_f11',
dtype(49)='ssmi',      dplat(49)='f13',       dsis(49)='ssmi_f13',
dtype(50)='ssmi',      dplat(50)='f14',       dsis(50)='ssmi_f14',
dtype(51)='ssmi',      dplat(51)='f15',       dsis(51)='ssmi_f15',
dtype(52)='amsre_low', dplat(52)='aqua',      dsis(52)='amsre_aqua',
dtype(53)='amsre_mid', dplat(53)='aqua',      dsis(53)='amsre_aqua',
dtype(54)='amsre_hig', dplat(54)='aqua',      dsis(54)='amsre_aqua',
dtype(55)='ssmis_las', dplat(55)='f16',       dsis(55)='ssmis_f16',
dtype(56)='ssmis_uas', dplat(56)='f16',       dsis(56)='ssmis_f16',
dtype(57)='ssmis_img', dplat(57)='f16',       dsis(57)='ssmis_f16',
dtype(58)='ssmis_env', dplat(58)='f16',       dsis(58)='ssmis_f16',
dtype(59)='sndrd1',    dplat(59)='g12',       dsis(59)='sndrD1_g12',
dtype(60)='sndrd2',    dplat(60)='g12',       dsis(60)='sndrD2_g12',
dtype(61)='sndrd3',    dplat(61)='g12',       dsis(61)='sndrD3_g12',
dtype(62)='sndrd4',    dplat(62)='g12',       dsis(62)='sndrD4_g12',
dtype(63)='sndrd1',    dplat(63)='g11',       dsis(63)='sndrD1_g11',
dtype(64)='sndrd2',    dplat(64)='g11',       dsis(64)='sndrD2_g11',
dtype(65)='sndrd3',    dplat(65)='g11',       dsis(65)='sndrD3_g11',
dtype(66)='sndrd4',    dplat(66)='g11',       dsis(66)='sndrD4_g11',
dtype(67)='sndrd1',    dplat(67)='g13',       dsis(67)='sndrD1_g13',
dtype(68)='sndrd2',    dplat(68)='g13',       dsis(68)='sndrD2_g13',
dtype(69)='sndrd3',    dplat(69)='g13',       dsis(69)='sndrD3_g13',
dtype(70)='sndrd4',    dplat(70)='g13',       dsis(70)='sndrD4_g13',
dtype(71)='ssu',       dplat(71)='tirosn',   dsis(71)='ssu_tirosn',
dtype(72)='ssu',       dplat(72)='n06',       dsis(72)='ssu_n06',
dtype(73)='ssu',       dplat(73)='n07',       dsis(73)='ssu_n07',
dtype(74)='ssu',       dplat(74)='n08',       dsis(74)='ssu_n08',
dtype(75)='ssu',       dplat(75)='n09',       dsis(75)='ssu_n09',
dtype(76)='ssu',       dplat(76)='n11',       dsis(76)='ssu_n11',
dtype(77)='ssu',       dplat(77)='n14',       dsis(77)='ssu_n14',
 /
EOF

cat global_angupdate.namelist
export PGM=$angupdatexec
${enkfscripts}/runmpi
rc=$?
if [[ $rc -ne 0 ]];then
  echo "global_angupdate failed with exit code $rc"
  sleep 10
  exit 1
fi
# Output file
##$ncp satbias_ang.out     $savdir/satang.${adate}
if [ -s satbias_ang.out ]; then
   $ncp satbias_ang.out  $SATANGO
else
   exit 1
fi


# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   cd $tmpdir
   cd ../
   /bin/rm -rf $tmpdir
fi


# End of script
exit
