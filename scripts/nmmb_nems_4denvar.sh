
set -x

# Set analysis date
#adate=2015061000

# Set experiment name
exp=$jobname

#TM=00
#TM2=03
#tmmark=tm${TM}


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

UNCOMPRESS=gunzip
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
   cp $fixcrtm/${file}.SpcCoeff.bin ./
   cp $fixcrtm/${file}.TauCoeff.bin ./
done
set -x

PDY=`echo $adate | cut -c1-8`
CYC=`echo $adate | cut -c9-10`

#datdir=/meso/noscrub/Wanshu.Wu/CASE/$adate

cp $nmmb_nems_4denvar_obs/nam.t00z.radwnd.tm00.bufr_d    ./radarbufr
cp $nmmb_nems_4denvar_obs/nam.t00z.nexrad.tm00.bufr_d    ./l2rwbufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.prepbufr            ./prepbufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.mtiasi.tm00.bufr_d  ./iasibufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.gpsro.tm00.bufr_d   ./gpsrobufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.1bamua.tm00.bufr_d  ./amsuabufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.esamua.tm00.bufr_d  ./amsuabufrears
cp $nmmb_nems_4denvar_obs/gdas1.t00z.1bmhs.tm00.bufr_d   ./mhsbufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.1bhrs4.tm00.bufr_d  ./hirs4bufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.goesfv.tm00.bufr_d  ./gsnd1bufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.airsev.tm00.bufr_d  ./airsbufr
cp $nmmb_nems_4denvar_obs/gdas1.t00z.satwnd.tm00.bufr_d  ./satwndbufr

   cp $nmmb_nems_4denvar_ges/wrf_inout03 .
   cp $nmmb_nems_4denvar_ges/wrf_inout06 .
   cp $nmmb_nems_4denvar_ges/wrf_inout09 .

cp $nmmb_nems_4denvar_ges/ndas.t00z.satbiaspc.tm03  ./satbias_pc
cp $nmmb_nems_4denvar_ges/ndas.t00z.satbiasc.tm03   ./satbias_in
cp $nmmb_nems_4denvar_ges/ndas.t00z.radstat.tm03    ./radstat.gdas

listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
for type in $listdiag; do
   diag_file=`echo $type | cut -d',' -f1`
   fname=`echo $diag_file | cut -d'.' -f1`
   date=`echo $diag_file | cut -d'.' -f2`
   $UNCOMPRESS $diag_file
   fnameanl=$(echo $fname|sed 's/_ges//g')
   mv $fname.$date $fnameanl
done

cp $nmmb_nems_4denvar_ges/rtma2p5.t00z.w_rejectlist ./w_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t00z.t_rejectlist ./t_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t00z.p_rejectlist ./p_rejectlist
cp $nmmb_nems_4denvar_ges/rtma2p5.t00z.q_rejectlist ./q_rejectlist

#####  connect with gefs ensemble #################
#gdate=`/nwprod/util/exec/ndate -6 $adate`
#cycg=`echo $gdate | cut -c9-10`
ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr06_ensmean > filelist06
#typeset -Z2 nsum
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr06s_mem00$nsum >> filelist06
         nsum=`expr $nsum + 1`
   done
ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr03_ensmean > filelist03
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr03s_mem00$nsum >> filelist03
     nsum=`expr $nsum + 1`
   done
ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr09_ensmean > filelist09
   nsum=1
   while [[ $nsum -lt 10 ]]; do
     ls $nmmb_nems_4denvar_ges/sfg_2015060918_fhr09s_mem00$nsum >> filelist09
     nsum=`expr $nsum + 1`
   done


#####  connect with gdas for ozges ################
       cp $nmmb_nems_4denvar_ges/gdas1.t18z.sf06  ./gfs_sigf06

if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then
   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   #export module="/usr/bin/modulecmd sh"

#  module load intel
#  module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "$launcher -v -np $PBS_NP $tmpdir/gsi.x > stdout"

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
elif [[ "$machine" = "WCOSS" ]]; then

mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit
