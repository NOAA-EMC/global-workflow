#!/bin/sh

#####
## This script defines functions for data I/O and namelist.
## different applications could share the same function
## or have their own.
##
## This is a child script of modular
## forecast script. This script is function definition.
## need to call these functions in the parent script 
## for execution.
#####

VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

data_link_GEFS(){
	echo SUB ${FUNCNAME[0]}: Linking input data for FV3 $RUN
# soft link commands insert here
}

data_link_GFS(){

echo "SUB ${FUNCNAME[0]}: Linking input data for FV3 $RUN"

#-------------------------------------------------------
# determine if restart IC exists to continue from a previous forecast
RERUN="NO"
filecount=$(find $RSTDIR_TMP -type f | wc -l)
if [ $CDUMP = "gfs" -a $restart_interval -gt 0 -a $FHMAX -gt $restart_interval -a $filecount -gt 10 ]; then
    SDATE=$($NDATE +$FHMAX $CDATE)
    EDATE=$($NDATE +$restart_interval $CDATE)
    while [ $SDATE -gt $EDATE ]; do
        PDYS=$(echo $SDATE | cut -c1-8)
        cycs=$(echo $SDATE | cut -c9-10)
        flag1=$RSTDIR_TMP/${PDYS}.${cycs}0000.coupler.res
        flag2=$RSTDIR_TMP/coupler.res
        if [ -s $flag1 ]; then
            mv $flag1 ${flag1}.old
            if [ -s $flag2 ]; then mv $flag2 ${flag2}.old ;fi
            RERUN="YES"
            CDATE_RST=$($NDATE -$restart_interval $SDATE)
            break
        fi
        SDATE=$($NDATE -$restart_interval $SDATE)
    done
fi

#-------------------------------------------------------
# member directory
if [ $MEMBER -lt 0 ]; then
  prefix=$CDUMP
  rprefix=$rCDUMP
  memchar=""
else
  prefix=enkf$CDUMP
  rprefix=enkf$rCDUMP
  memchar=mem$(printf %03i $MEMBER)
fi
memdir=$ROTDIR/${prefix}.$PDY/$cyc/$memchar
if [ ! -d $memdir ]; then mkdir -p $memdir; fi

GDATE=$($NDATE -$assim_freq $CDATE)
gPDY=$(echo $GDATE | cut -c1-8)
gcyc=$(echo $GDATE | cut -c9-10)
gmemdir=$ROTDIR/${rprefix}.$gPDY/$gcyc/$memchar

#-------------------------------------------------------
# initial conditions
warm_start=${warm_start:-".false."}
read_increment=${read_increment:-".false."}
restart_interval=${restart_interval:-0}

# Determine if this is a warm start or cold start
if [ -f $gmemdir/RESTART/${PDY}.${cyc}0000.coupler.res ]; then
  export warm_start=".true."
fi

echo $warm_start
echo $RERUN
#-------------------------------------------------------
if [ $warm_start = ".true." -o $RERUN = "YES" ]; then
#-------------------------------------------------------
#.............................
  if [ $RERUN = "NO" ]; then
#.............................

  # Link all (except sfc_data) restart files from $gmemdir
  for file in $gmemdir/RESTART/${PDY}.${cyc}0000.*.nc; do
    file2=$(echo $(basename $file))
    file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
    fsuf=$(echo $file2 | cut -d. -f1)
    if [ $fsuf != "sfc_data" ]; then
       $NLN $file $DATA/INPUT/$file2
    fi
  done

  # Link sfcanl_data restart files from $memdir
  for file in $memdir/RESTART/${PDY}.${cyc}0000.*.nc; do
    file2=$(echo $(basename $file))
    file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
    fsufanl=$(echo $file2 | cut -d. -f1)
    if [ $fsufanl = "sfcanl_data" ]; then
      file2=$(echo $file2 | sed -e "s/sfcanl_data/sfc_data/g")
      $NLN $file $DATA/INPUT/$file2
    fi
  done

  # Handle coupler.res file for DA cycling
  if [ ${USE_COUPLER_RES:-"NO"} = "YES" ]; then
    # In DA, this is not really a "true restart",
    # and the model start time is the analysis time
    # The alternative is to replace
    # model start time with current model time in coupler.res
    file=$gmemdir/RESTART/${PDY}.${cyc}0000.coupler.res
    file2=$(echo $(basename $file))
    file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
    $NLN $file $DATA/INPUT/$file2
  fi

  increment_file=$memdir/${CDUMP}.t${cyc}z.atminc.nc
  if [ -f $increment_file ]; then
    $NLN $increment_file $DATA/INPUT/fv3_increment.nc
    read_increment=".true."
    res_latlon_dynamics="fv3_increment.nc"
  else
    read_increment=".false."
    res_latlon_dynamics="''"
  fi

#.............................
  else  ##RERUN                         

    PDYT=$(echo $CDATE_RST | cut -c1-8)
    cyct=$(echo $CDATE_RST | cut -c9-10)
    for file in $RSTDIR_TMP/${PDYT}.${cyct}0000.*; do
      file2=$(echo $(basename $file))
      file2=$(echo $file2 | cut -d. -f3-)
      $NLN $file $DATA/INPUT/$file2
    done

  fi
#.............................

else ## cold start                            

  for file in $memdir/INPUT/*.nc; do
    file2=$(echo $(basename $file))
    fsuf=$(echo $file2 | cut -c1-3)
    if [ $fsuf = "gfs" -o $fsuf = "sfc" ]; then
      $NLN $file $DATA/INPUT/$file2
    fi
  done

fi 

if [ $machine = 'sandbox' ]; then
	echo SUB ${FUNCNAME[0]}: Checking initial condition, overriden in sandbox mode!
else
	nfiles=$(ls -1 $DATA/INPUT/* | wc -l)
	if [ $nfiles -le 0 ]; then
		  echo SUB ${FUNCNAME[0]}: Initial conditions must exist in $DATA/INPUT, ABORT!
		  msg=”"SUB ${FUNCNAME[0]}: Initial conditions must exist in $DATA/INPUT, ABORT!"
		  postmsg "$jlogfile" "$msg"
		  exit 1
	fi
fi

#--------------------------------------------------------------------------
# Grid and orography data
for n in $(seq 1 $ntiles); do
  $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc     $DATA/INPUT/${CASE}_grid.tile${n}.nc
  $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
done
$NLN $FIXfv3/$CASE/${CASE}_mosaic.nc  $DATA/INPUT/grid_spec.nc

# GFS standard input data

IALB=${IALB:-1}
IEMS=${IEMS:-1}
ISOL=${ISOL:-2}
IAER=${IAER:-111}
ICO2=${ICO2:-2}

if [ ${new_o3forc:-YES} = YES ]; then
    O3FORC=ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77
else
    O3FORC=global_o3prdlos.f77
fi
H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
$NLN $FIX_AM/${O3FORC}                         $DATA/global_o3prdlos.f77
$NLN $FIX_AM/${H2OFORC}                        $DATA/global_h2oprdlos.f77
$NLN $FIX_AM/global_solarconstant_noaa_an.txt  $DATA/solarconstant_noaa_an.txt
$NLN $FIX_AM/global_sfc_emissivity_idx.txt     $DATA/sfc_emissivity_idx.txt

$NLN $FIX_AM/global_co2historicaldata_glob.txt $DATA/co2historicaldata_glob.txt
$NLN $FIX_AM/co2monthlycyc.txt                 $DATA/co2monthlycyc.txt
if [ $ICO2 -gt 0 ]; then
  for file in $(ls $FIX_AM/fix_co2_proj/global_co2historicaldata*) ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

$NLN $FIX_AM/global_climaeropac_global.txt     $DATA/aerosol.dat
if [ $IAER -gt 0 ] ; then
  for file in $(ls $FIX_AM/global_volcanic_aerosols*) ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

# Fix files
FNGLAC=${FNGLAC:-"$FIX_AM/global_glacier.2x2.grb"}
FNMXIC=${FNMXIC:-"$FIX_AM/global_maxice.2x2.grb"}
FNTSFC=${FNTSFC:-"$FIX_AM/RTGSST.1982.2012.monthly.clim.grb"}
FNSNOC=${FNSNOC:-"$FIX_AM/global_snoclim.1.875.grb"}
FNZORC=${FNZORC:-"igbp"}
FNALBC2=${FNALBC2:-"$FIX_AM/global_albedo4.1x1.grb"}
FNAISC=${FNAISC:-"$FIX_AM/CFSR.SEAICE.1982.2012.monthly.clim.grb"}
FNTG3C=${FNTG3C:-"$FIX_AM/global_tg3clim.2.6x1.5.grb"}
FNVEGC=${FNVEGC:-"$FIX_AM/global_vegfrac.0.144.decpercent.grb"}
FNMSKH=${FNMSKH:-"$FIX_AM/seaice_newland.grb"}
FNVMNC=${FNVMNC:-"$FIX_AM/global_shdmin.0.144x0.144.grb"}
FNVMXC=${FNVMXC:-"$FIX_AM/global_shdmax.0.144x0.144.grb"}
FNSLPC=${FNSLPC:-"$FIX_AM/global_slope.1x1.grb"}
FNALBC=${FNALBC:-"$FIX_AM/global_snowfree_albedo.bosu.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNVETC=${FNVETC:-"$FIX_AM/global_vegtype.igbp.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNSOTC=${FNSOTC:-"$FIX_AM/global_soiltype.statsgo.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNABSC=${FNABSC:-"$FIX_AM/global_mxsnoalb.uariz.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNSMCC=${FNSMCC:-"$FIX_AM/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}

# If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
[[ ! -f $FNALBC ]] && FNALBC="$FIX_AM/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb"
[[ ! -f $FNVETC ]] && FNVETC="$FIX_AM/global_vegtype.igbp.t1534.3072.1536.rg.grb"
[[ ! -f $FNSOTC ]] && FNSOTC="$FIX_AM/global_soiltype.statsgo.t1534.3072.1536.rg.grb"
[[ ! -f $FNABSC ]] && FNABSC="$FIX_AM/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb"
[[ ! -f $FNSMCC ]] && FNSMCC="$FIX_AM/global_soilmgldas.statsgo.t1534.3072.1536.grb"

#------------------------------------------------------------------
# make symbolic links to write forecast files directly in memdir
cd $DATA
if [ $QUILTING = ".true." -a $OUTPUT_GRID = "gaussian_grid" ]; then
  fhr=$FHMIN
  while [ $fhr -le $FHMAX ]; do
    FH3=$(printf %03i $fhr)
    atmi=atmf${FH3}.$OUTPUT_FILE
    sfci=sfcf${FH3}.$OUTPUT_FILE
    logi=logf${FH3}
    atmo=$memdir/${CDUMP}.t${cyc}z.atmf${FH3}.$OUTPUT_FILE
    sfco=$memdir/${CDUMP}.t${cyc}z.sfcf${FH3}.$OUTPUT_FILE
    logo=$memdir/${CDUMP}.t${cyc}z.logf${FH3}.$OUTPUT_FILE
    eval $NLN $atmo $atmi
    eval $NLN $sfco $sfci
    eval $NLN $logo $logi
    FHINC=$FHOUT
    if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 -a $fhr -lt $FHMAX_HF ]; then
      FHINC=$FHOUT_HF
    fi
    fhr=$((fhr+FHINC))
  done
else
  for n in $(seq 1 $ntiles); do
    eval $NLN nggps2d.tile${n}.nc       $memdir/nggps2d.tile${n}.nc
    eval $NLN nggps3d.tile${n}.nc       $memdir/nggps3d.tile${n}.nc
    eval $NLN grid_spec.tile${n}.nc     $memdir/grid_spec.tile${n}.nc
    eval $NLN atmos_static.tile${n}.nc  $memdir/atmos_static.tile${n}.nc
    eval $NLN atmos_4xdaily.tile${n}.nc $memdir/atmos_4xdaily.tile${n}.nc
  done
fi

}

FV3_nml(){
	# namelist output for a certain component
	echo SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3
	if [ $machine = 'sandbox' ]; then
		cd $SCRIPTDIR
		echo "MAIN: !!!Sandbox mode, writing to current directory!!!"
	fi
	# Call child scripts in current script directory
	source $SCRIPTDIR/parsing_namelists_FV3.sh
	source $SCRIPTDIR/parsing_model_configure_FV3.sh
	FV3_model_configure
	FV3_namelists
	echo SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created
}

data_out_GFS()
{
# data in take for FV3GFS
# Arguments: None 
# 
#------------------------------------------------------------------
# make symbolic links to write forecast files directly in memdir
echo "SUB ${FUNCNAME[0]}: copying output data for FV3"
#------------------------------------------------------------------
if [ $SEND = "YES" ]; then
  # Copy model restart files
  if [ $CDUMP = "gdas" -a $restart_interval -gt 0 ]; then
    cd $DATA/RESTART
    mkdir -p $memdir/RESTART
  # Only save restarts at single time in RESTART directory
  # Either at restart_interval or at end of the forecast
#  if [ $restart_interval -eq 0 -o $restart_interval -eq $FHMAX ]; then

    # Add time-stamp to restart files at FHMAX
#    RDATE=$($NDATE +$FHMAX $CDATE)
#    rPDY=$(echo $RDATE | cut -c1-8)
#    rcyc=$(echo $RDATE | cut -c9-10)
#    for file in $(ls * | grep -v 0000); do
#      $NMV $file ${rPDY}.${rcyc}0000.$file
#    done
#  else
    # time-stamp exists at restart_interval time, just copy
    RDATE=$($NDATE +$restart_interval $CDATE)
    rPDY=$(echo $RDATE | cut -c1-8)
    rcyc=$(echo $RDATE | cut -c9-10)
    for file in ${rPDY}.${rcyc}0000.* ; do
      $NCP $file $memdir/RESTART/$file
    done
  fi
fi
echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
}

WW3_in()
{
	echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"
	# soft link commands insert here
}

WW3_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for WW3"
	sh parsing_namelist_WW3.sh
}

WW3_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
	# soft link commands insert here
}

HYCOM_in()
{
	echo "SUB ${FUNCNAME[0]}: Linking input data for HYCOM"
	# soft link commands insert here
}

HYCOM_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for HYCOM"
	sh parsing_namelist_HYCOM.sh
}

HYCOM_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for HYCOM"
	# soft link commands insert here
}

CICE_in()
{
	echo "SUB ${FUNCNAME[0]}: Linking input data for CICE"
	# soft link commands insert here
}

CICE_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
	sh parsing_namelist_HYCOM.sh
}

CICE_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"
	# soft link commands insert here
}

GSD_in()
{
	echo "SUB ${FUNCNAME[0]}: Linking input data for GSD"
	# soft link commands insert here
}

GSD_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for GSD"
	sh parsing_namelist_GSD.sh
}

GSD_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for GSD"
	# soft link commands insert here
}
