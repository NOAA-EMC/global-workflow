
set -x

# Set experiment name and analysis date

exp=$jobname


# Set the JCAP resolution which you want.
export JCAP=48
export LEVS=127
export NMEM_ENKF=10

# Set runtime and save directories
tmpdir=$tmpdir/$tmpregdir/${exp}

# Specify EnKf fixed field and data directories.
export DATA=$tmpdir

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
nln="/bin/ln -fs"


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "96" ]]; then
   export LONA=384
   export LATA=192
   export DELTIM=1200
elif [[ "$JCAP" = "48" ]]; then
   export LONA=192
   export LATA=96
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`date +%Y%m%d%H -d "${global_adate:0:8} ${global_adate:8:2} - 6 hours"`
PDYa=`echo $global_adate | cut -c1-8`
cyca=`echo $global_adate | cut -c9-10`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

prefix_obs=enkfgdas.t${cyca}z
prefix_ens=enkfgdas.t${cycg}z
suffix=tm00.bufr_d

dumpges=gdas
COMROOTgfs=$casesdir/gfs/prod
datobs=$COMROOTgfs/enkfgdas.$PDYa/${cyca}/ensstat/analysis/atmos
datens=$COMROOTgfs/enkfgdas.$PDYg/${cycg}


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Make enkf namelist
export USE_CORRELATED_OBERRS=${USE_CORRELATED_OBERRS:-"NO"}
export NMEM_ENKF=${NMEM_ENKF:-10}
export NAM_ENKF="smoothparm=35,"
export SATOBS_ENKF=${SATOBS_ENKF:-""}
export OZOBS_ENKF=${OZOBS_ENKF:-""}
export use_correlated_oberrs=${use_correlated_oberrs:-".false."}
if [ $USE_CORRELATED_OBERRS == "YES" ]; then
    export use_correlated_oberrs=".true."
fi
export lupp=${lupp:-".true."}
export corrlength=${corrlength:-1250}
export lnsigcutoff=${lnsigcutoff:-2.75}
export analpertwt=${analpertwt:-0.85}
export readin_localization_enkf=".false."
export letkf_flag=${letkf_flag:-".true."}
export getkf=${getkf:-".true."}
export denkf=${denkf:-".true."}
export nobsl_max=${nobsl_max:-10000}
export lobsdiag_forenkf=${lobsdiag_forenkf:-".true."}
export write_spread_diag=${write_spread_diag:-".false."}
export cnvw_option=${cnvw_option:-".false."}
export netcdf_diag=${netcdf_diag:-".true."}
export modelspace_vloc=${modelspace_vloc:-".true."} # if true, 'vlocal_eig.dat' is needed
export taperanalperts=${taperanalperts:-".true."}
export IAUFHRS_ENKF=${IAUFHRS_ENKF:-'3,6,9'}
export DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}
export INCREMENTS_TO_ZERO="'liq_wat_inc','icmr_inc','rwmr_inc','snmr_inc','grle_inc'"
export use_gfs_ncio=".true."
export use_gfs_nemsio=".false."
export paranc=${paranc:-".true."}
if [ $DO_CALC_INCREMENT = "YES" ]; then
    export write_fv3_incr=".false."
else
    export write_fv3_incr=".true."
    export WRITE_INCR_ZERO="incvars_to_zero= $INCREMENTS_TO_ZERO,"
fi


. $scripts/regression_namelists.sh global_enkf

cat << EOF > enkf.nml

$gsi_namelist

EOF

# Set fixed files
#   anavinfo = text file with information about control vector
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   scaninfo = text file with scan angle information
#   ozinfo   = text file with information about assimilation of ozone data
#   convinfo = text file with information about assimilation of conventional data
#   hybens_info = text file with localization informztion

satangl=$fixgsi/global_satangbias.txt
scaninfo=$fixgsi/global_scaninfo.txt
satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
ozinfo=$fixgsi/global_ozinfo.txt
hybens_info=$fixgsi/global_hybens_info.l${LEVS}.txt
anavinfo=$fixgsi/global_anavinfo.l${LEVS}.txt
vlocaleig=$fixgsi/vlocal_eig_l${LEVS}.dat


# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $enkfexec_updat  ./enkf.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $enkfexec_contrl ./enkf.x
fi

$ncp $satangl     ./satbias_angle
$ncp $scaninfo    ./scaninfo
$ncp $satinfo     ./satinfo
$ncp $ozinfo      ./ozinfo
$ncp $convinfo    ./convinfo
$ncp $vlocaleig   ./vlocal_eig.dat
$ncp $hybens_info ./hybens_info
$ncp $anavinfo    ./anavinfo


# Copy ensemble data to $tmpdir
list="cnvstat oznstat radstat"

for type in $list; do
   $ncp $datobs/${prefix_obs}.${type}.ensmean ./${type}_ensmean
   tar -xvf ${type}_ensmean
done

nfhrs=`echo $IAUFHRS_ENKF | sed 's/,/ /g'`
for fhr in $nfhrs; do
    for imem in $(seq 1 $NMEM_ENKF); do
	memchar="mem"$(printf %03i $imem)
	$nln $datens/$memchar/model/atmos/history/${prefix_ens}.atmf00${fhr}.nc sfg_${global_adate}_fhr0${fhr}_${memchar}
        if [ $cnvw_option = ".true." ]; then
            $nln $datens/$memchar/model/atmos/history/${prefix_ens}sfcf00${fhr}.nc sfgsfc_${global_adate}_fhr0${fhr}_${memchar}
        fi
	(( imem = $imem + 1 ))
    done
    $nln $datens/ensstat/model/atmos/history/${prefix_ens}.atmf00${fhr}.ensmean.nc sfg_${global_adate}_fhr0${fhr}_ensmean
    if [ $cnvw_option = ".true." ]; then
        $nln $datens/${prefix_ens}.sfcf00${fhr}.ensmean.nc sfgsfc_${global_adate}_fhr0${fhr}_ensmean
    fi
done

$nln $datobs/${prefix_obs}.abias_int.ensmean ./satbias_in


cd $tmpdir
echo "run enkf now"
rm stdout stderr
eval "$APRUN $tmpdir/enkf.x 1>stdout 2>stderr"
rc=$?
exit $rc
