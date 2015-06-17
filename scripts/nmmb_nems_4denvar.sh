
set -x

# Set analysis date
#adate=2015061000

# Set experiment name
exp=$jobname

TM=00
TM2=03
tmmark=tm${TM}


# Set path/file for gsi executable
#gsiexec=/meso/save/Wanshu.Wu/Code/trunk/trunk_40320/src/global_gsi_org
#gsiexec=/da/save/Michael.Lueken/trunk/src/global_gsi

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_nems_nmmb_4denvar/${exp}
savdir=$savdir/outreg_nems_nmmb_4denvar/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
ncp=/bin/cp


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir

#FIXnam=/da/save/Michael.Lueken/trunk/fix
#FIXCRTM=/da/save/Michael.Lueken/CRTM_REL-2.2.1/crtm_v2.2.1/fix

berror=$fixgsi/nam_nmm_berror.f77.gcv
anavinfo=$fixgsi/anavinfo_nems_nmmb


# Make gsi namelist

. $scripts/regression_nl_update.sh

SETUP="$SETUP_update"
GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
SINGLEOB="$SINGLEOB_update"

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh
else
   . $scripts/regression_namelists_db.sh
fi

#   dmesh(1)=120.0,time_window_max=1.5,ext_sonde=.true.,

cat << EOF > gsiparm.anl

$nems_nmmb_4denvar_namelist

EOF

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
satinfo=$fixgsi/nam_regional_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
pcpinfo=$fixgsi/nam_global_pcpinfo.txt
ozinfo=$fixgsi/nam_global_ozinfo.txt
errtable=$fixgsi/nam_errtable.r3dv
convinfo=$fixgsi/nam_regional_convinfo.txt
mesonetuselist=$fixgsi/nam_mesonet_uselist.txt
stnuselist=$fixgsi/nam_mesonet_stnuselist.txt
qdaylist=$fixgsi/rtma_q_day_rejectlist
qnightlist=$fixgsi/rtma_q_night_rejectlist
tdaylist=$fixgsi/rtma_t_day_rejectlist
tnightlist=$fixgsi/rtma_t_night_rejectlist
wbinuselist=$fixgsi/rtma_wbinuselist
locinfo=$fixgsi/nam_hybens_d01_locinfo

# Copy executable and fixed files to $tmpdir
if [[ "$exp" = $nmmb_nems_4denvar_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $nmmb_nems_4denvar_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $nmmb_nems_4denvar_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ "$exp" = $nmmb_nems_4denvar_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

cp $anavinfo ./anavinfo
cp $berror   ./berror_stats
cp $errtable ./errtable
cp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
cp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
cp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
cp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
cp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
cp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
cp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
cp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
cp $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
cp $aercoef  ./AerosolCoeff.bin
cp $cldcoef  ./CloudCoeff.bin
cp $satinfo  ./satinfo
cp $scaninfo ./scaninfo
cp $pcpinfo  ./pcpinfo
cp $ozinfo   ./ozinfo
cp $convinfo ./convinfo
cp $mesonetuselist ./mesonetuselist
cp $stnuselist ./mesonet_stnuselist
cp $qdaylist ./q_day_rejectlist
cp $qnightlist ./q_night_rejectlist
cp $tdaylist ./t_day_rejectlist
cp $tnightlist ./t_night_rejectlist
cp $wbinuselist ./wbinuselist
#cp $locinfo ./hybens_locinfo


###### crtm coeff's #######################
set +x
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   cp $FIXCRTM/${file}.SpcCoeff.bin ./
   cp $FIXCRTM/${file}.TauCoeff.bin ./
done
set -x

PDY=`echo $adate | cut -c1-8`
CYC=`echo $adate | cut -c9-10`

#datdir=/meso/noscrub/Wanshu.Wu/CASE/$adate

cp $nmmb_nems_4denvar_obs/nam.t${CYC}z.radwnd.tm${TM}.bufr_d ./radarbufr
cp $nmmb_nems_4denvar_obs/nam.t${CYC}z.nexrad.tm${TM}.bufr_d  ./l2rwbufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.prepbufr ./prepbufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.mtiasi.tm${TM}.bufr_d  ./iasibufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.gpsro.tm${TM}.bufr_d  ./gpsrobufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.1bamua.tm${TM}.bufr_d  ./amsuabufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.esamua.tm${TM}.bufr_d  ./amsuabufrears
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.1bmhs.tm${TM}.bufr_d   ./mhsbufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.1bhrs4.tm${TM}.bufr_d  ./hirs4bufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.goesfv.tm${TM}.bufr_d  ./gsnd1bufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.airsev.tm${TM}.bufr_d  ./airsbufr
cp $nmmb_nems_4denvar_obs/gdas1.t${CYC}z.satwnd.tm${TM}.bufr_d  ./satwndbufr

   cp $nmmb_nems_4denvar_ges/wrf_inout03 .
   cp $nmmb_nems_4denvar_ges/wrf_inout06 .
   cp $nmmb_nems_4denvar_ges/wrf_inout09 .

cp $nmmb_nems_4denvar_ges/ndas.t${CYC}z.satbiaspc.tm${TM2} ./satbias_pc
cp $nmmb_nems_4denvar_ges/ndas.t${CYC}z.satbiasc.tm${TM2} ./satbias_in
cp $nmmb_nems_4denvar_ges/ndas.t${CYC}z.radstat.tm${TM2}   ./radstat.gdas

cp $nmmb_nems_4denvar_ges/rtma2p5.t${CYC}z.w_rejectlist ./w_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t${CYC}z.t_rejectlist ./t_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t${CYC}z.p_rejectlist ./p_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t${CYC}z.q_rejectlist ./q_rejectlist

#####  connect with gefs ensemble #################
gdate=`/nwprod/util/exec/ndate -6 $adate`
cycg=`echo $gdate | cut -c9-10`
ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr06_ensmean > filelist06
typeset -Z2 nsum
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr06s_mem0$nsum >> filelist06
         nsum=`expr $nsum + 1`
   done
ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr03_ensmean > filelist03
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr03s_mem0$nsum >> filelist03
     nsum=`expr $nsum + 1`
   done
ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr09_ensmean > filelist09
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmm_nems_4denvar_ges/sfg_${gdate}_fhr09s_mem0$nsum >> filelist09
     nsum=`expr $nsum + 1`
   done


#####  connect with gdas for ozges ################
       cp $nmmb_nems_4denvar_ges/gdas1.t${cycg}z.sf06  ./gfs_sigf06

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$machine" = "WCOSS" ]]; then

mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit
