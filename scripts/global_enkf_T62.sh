
set -x

# Set experiment name and analysis date

exp=$jobname


# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export NMEM_ENKF=20
export NVARS=6

# Set runtime and save directories
tmpdir=$tmpdir/$tmpregdir/${exp}
savdir=$savdir/out${JCAP}_enkf/${exp}

# Specify EnKf fixed field and data directories.
export DATA=$tmpdir

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
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


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
export adate=$global_enkf_T62_adate
gdate=`$ndate -06 $global_enkf_T62_adate`
yyg=`echo $gdate | cut -c1-8`
hhg=`echo $gdate | cut -c9-10`
yya=`echo $global_enkf_T62_adate | cut -c1-8`
hha=`echo $global_enkf_T62_adate | cut -c9-10`

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# Make enkf namelist

. $scripts/regression_nl_update.sh

export NAM_ENKF="$SETUP_enkf"


. $scripts/regression_namelists.sh global_enkf_T62

cat << EOF > enkf.nml

$gsi_namelist

EOF

# Set fixed files
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   scaninfo = text file with scan angle information
#   ozinfo   = text file with information about assimilation of ozone data
#   convinfo = text file with information about assimilation of conventional data
#   hybens_locinfo = text file with localization informztion

satangl=$fixgsi/global_satangbias.txt
scaninfo=$fixgsi/global_scaninfo.txt
satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
ozinfo=$fixgsi/global_ozinfo.txt
hybens_locinfo=$fixgsi/global_hybens_locinfo.l64.txt
### add 9 tables
errtable_pw=$fixgsi/prepobs_errtable_pw.global
errtable_ps=$fixgsi/prepobs_errtable_ps.global_nqcf
errtable_t=$fixgsi/prepobs_errtable_t.global_nqcf
errtable_q=$fixgsi/prepobs_errtable_q.global_nqcf
errtable_uv=$fixgsi/prepobs_errtable_uv.global_nqcf
btable_ps=$fixgsi/nqc_b_ps.global_nqcf
btable_t=$fixgsi/nqc_b_t.global_nqcf
btable_q=$fixgsi/nqc_b_q.global_nqcf
btable_uv=$fixgsi/nqc_b_uv.global_nqcf



# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $enkfexec_updat  ./enkf.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $enkfexec_contrl ./enkf.x
fi

$ncp $satangl  ./satbias_angle
$ncp $scaninfo ./scaninfo
$ncp $satinfo  ./satinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $hybens_locinfo ./hybens_locinfo
#add 9 tables for new varqc
$ncp $errtable_pw           ./errtable_pw
$ncp $errtable_ps           ./errtable_ps
$ncp $errtable_t           ./errtable_t
$ncp $errtable_q           ./errtable_q
$ncp $errtable_uv           ./errtable_uv
$ncp $btable_ps           ./btable_ps
$ncp $btable_t           ./btable_t
$ncp $btable_q           ./btable_q
$ncp $btable_uv           ./btable_uv



# Copy ensemble data to $tmpdir
list="cnvstat oznstat radstat"
for type in $list; do
   $ncp $global_enkf_T62_datobs/${type}_${adate}_ensmean ./${type}_ensmean
   tar -xvf ${type}_ensmean
done
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="_mem"`printf %03i $imem`
   list="cnvstat oznstat radstat"
   for type in $list; do
      $ncp $global_enkf_T62_datobs/${type}_${adate}${member} ./${type}${member}
      tar -xvf ${type}${member}
   done
   sigens=$global_enkf_T62_datges/sfg_${gdate}_fhr06s${member}
   $ncp $sigens sfg_${global_enkf_T62_adate}_fhr06${member}
   (( imem = $imem + 1 ))
done


# Copy bias correction, atmospheric and surface files
$ncp $global_enkf_T62_datges/biascr_int_${gdate}_ensmean ./satbias_in
$ncp $global_enkf_T62_datges/satang.gdas.$gdate          ./satbias_ang.in

$ncp $global_enkf_T62_datges/sfg_${gdate}_fhr06_ensmean ./sfg_${global_enkf_T62_adate}_fhr06_ensmean
$ncp $global_enkf_T62_datges/bfg_${gdate}_fhr06_ensmean ./bfg_${global_enkf_T62_adate}_fhr06_ensmean

cd $tmpdir
echo "run enkf now"
eval "$APRUN $tmpdir/enkf.x > stdout 2>&1"
rc=$?
exit $rc
