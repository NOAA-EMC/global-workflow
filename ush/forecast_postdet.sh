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

FV3_GEFS_postdet(){
	echo SUB ${FUNCNAME[0]}: Linking input data for FV3 $RUN
# soft link commands insert here
}

FV3_GFS_postdet(){

	echo "SUB ${FUNCNAME[0]}: $RERUN and $warm_start determined for $RUN"

	echo $warm_start
	echo $RERUN

        if [ ${TYPE} = "nh" ]; then # non-hydrostatic options

          hydrostatic=".false."
          phys_hydrostatic=".false."     # enable heating in hydrostatic balance in non-hydrostatic simulation
          use_hydro_pressure=".false."   # use hydrostatic pressure for physics
          if [ $warm_start = ".true." ]; then
            make_nh=".false."              # restarts contain non-hydrostatic state
          else
            make_nh=".true."               # re-initialize non-hydrostatic state
          fi

        else # hydrostatic options

          hydrostatic=".true."
          phys_hydrostatic=".false."     # ignored when hydrostatic = T
          use_hydro_pressure=".false."   # ignored when hydrostatic = T
          make_nh=".false."              # running in hydrostatic mode

        fi

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

          # Need a coupler.res when doing IAU
          if [ $DOIAU = "YES" ]; then
             rm -f $DATA/INPUT/coupler.res
             cat >> $DATA/INPUT/coupler.res << EOF
              2        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
          ${gPDY:0:4}  ${gPDY:4:2}  ${gPDY:6:2}  ${gcyc}     0     0        Model start time:   year, month, day, hour, minute, second
          ${sPDY:0:4}  ${sPDY:4:2}  ${sPDY:6:2}  ${scyc}     0     0        Current model time: year, month, day, hour, minute, second
EOF
          fi

          # Link increments
          if [ $DOIAU = "YES" ]; then
            for i in $(echo $IAUFHRS | sed "s/,/ /g" | rev); do
               incfhr=$(printf %03i $i)
               if [ $incfhr = "006" ]; then
                 increment_file=$memdir/${CDUMP}.t${cyc}z.atminc.nc
               else
                 increment_file=$memdir/${CDUMP}.t${cyc}z.atmi${incfhr}.nc
               fi
               if [ ! -f $increment_file ]; then
                 echo "ERROR: DOIAU = $DOIAU, but missing increment file for fhr $incfhr at $increment_file"
                 echo "Abort!"
                 exit 1
               fi
               $NLN $increment_file $DATA/INPUT/fv_increment$i.nc
               IAU_INC_FILES="'fv_increment$i.nc',$IAU_INC_FILES"
            done
            read_increment=".false."
            res_latlon_dynamics=""
          else
	    increment_file=$memdir/${CDUMP}.t${cyc}z.atminc.nc
	    if [ -f $increment_file ]; then
	       $NLN $increment_file $DATA/INPUT/fv3_increment.nc
	       read_increment=".true."
	       res_latlon_dynamics="fv3_increment.nc"
	    fi
          fi

	#.............................
	  else  ##RERUN               
          
	    export warm_start=".true."
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

        # If doing IAU, change forecast hours
        if [[ "$DOIAU" = "YES" ]]; then
          FHMAX=$((FHMAX+6))
          if [ $FHMAX_HF -gt 0 ]; then
            FHMAX_HF=$((FHMAX_HF+6))
          fi
        fi

	#--------------------------------------------------------------------------
	# Grid and orography data
	for n in $(seq 1 $ntiles); do
	  $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc     $DATA/INPUT/${CASE}_grid.tile${n}.nc
	  $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
	done
        if [ $cpl = ".false." ] ; then
	  $NLN $FIXfv3/$CASE/${CASE}_mosaic.nc  $DATA/INPUT/grid_spec.nc
        fi

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

	#------------------------------------------------------------------
	# changeable parameters
	# dycore definitions
	res=$(echo $CASE |cut -c2-5)
	resp=$((res+1))
	npx=$resp
	npy=$resp
	npz=$((LEVS-1))
	io_layout="1,1"
	#ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

	# spectral truncation and regular grid resolution based on FV3 resolution
	JCAP_CASE=$((2*res-2))
	LONB_CASE=$((4*res))
	LATB_CASE=$((2*res))
        if [ $LATB_CASE -eq 192 ]; then
           LATB_CASE=190 # berror file is at this resolution
        fi

	JCAP=${JCAP:-$JCAP_CASE}
	LONB=${LONB:-$LONB_CASE}
	LATB=${LATB:-$LATB_CASE}

	LONB_IMO=${LONB_IMO:-$LONB_CASE}
	LATB_JMO=${LATB_JMO:-$LATB_CASE}

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

	# NSST Options
	# nstf_name contains the NSST related parameters
	# nstf_name(1) : NST_MODEL (NSST Model) : 0 = OFF, 1 = ON but uncoupled, 2 = ON and coupled
	# nstf_name(2) : NST_SPINUP : 0 = OFF, 1 = ON,
	# nstf_name(3) : NST_RESV (Reserved, NSST Analysis) : 0 = OFF, 1 = ON
	# nstf_name(4) : ZSEA1 (in mm) : 0
	# nstf_name(5) : ZSEA2 (in mm) : 0
	# nst_anl      : .true. or .false., NSST analysis over lake
	NST_MODEL=${NST_MODEL:-0}
	NST_SPINUP=${NST_SPINUP:-0}
	NST_RESV=${NST_RESV-0}
	ZSEA1=${ZSEA1:-0}
	ZSEA2=${ZSEA2:-0}
	nstf_name=${nstf_name:-"$NST_MODEL,$NST_SPINUP,$NST_RESV,$ZSEA1,$ZSEA2"}
	nst_anl=${nst_anl:-".false."}

	# blocking factor used for threading and general physics performance
	#nyblocks=`expr \( $npy - 1 \) \/ $layout_y `
	#nxblocks=`expr \( $npx - 1 \) \/ $layout_x \/ 32`
	#if [ $nxblocks -le 0 ]; then nxblocks=1 ; fi
	blocksize=${blocksize:-32}

	# the pre-conditioning of the solution
	# =0 implies no pre-conditioning
	# >0 means new adiabatic pre-conditioning
	# <0 means older adiabatic pre-conditioning
	na_init=${na_init:-1}
	[[ $warm_start = ".true." ]] && na_init=0

	# variables for controlling initialization of NCEP/NGGPS ICs
	filtered_terrain=${filtered_terrain:-".true."}
	gfs_dwinds=${gfs_dwinds:-".true."}

	# various debug options
	no_dycore=${no_dycore:-".false."}
	dycore_only=${adiabatic:-".false."}
	chksum_debug=${chksum_debug:-".false."}
	print_freq=${print_freq:-6}

	# Conserve total energy as heat globally
	consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics

	# time step parameters in FV3
	k_split=${k_split:-2}
	n_split=${n_split:-6}

	if [ $(echo $MONO | cut -c-4) = "mono" ];  then # monotonic options
	
	  d_con=${d_con_mono:-"0."}
	  do_vort_damp=".false."
	  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
	    hord_mt=${hord_mt_nh_mono:-"10"}
	    hord_xx=${hord_xx_nh_mono:-"10"}
	  else # hydrostatic
	    hord_mt=${hord_mt_hydro_mono:-"10"}
	    hord_xx=${hord_xx_hydro_mono:-"10"}
	  fi
	
	else # non-monotonic options

	  d_con=${d_con_nonmono:-"1."}
	  do_vort_damp=".true."
	  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
	    hord_mt=${hord_mt_nh_nonmono:-"5"}
	    hord_xx=${hord_xx_nh_nonmono:-"5"}
	  else # hydrostatic
	    hord_mt=${hord_mt_hydro_nonmono:-"10"}
	    hord_xx=${hord_xx_hydro_nonmono:-"10"}
	  fi

	fi

	if [ $(echo $MONO | cut -c-4) != "mono" -a $TYPE = "nh" ]; then
	  vtdm4=${vtdm4_nh_nonmono:-"0.06"}
	else
	  vtdm4=${vtdm4:-"0.05"}
	fi

	if [ $warm_start = ".true." ]; then # warm start from restart file

	  nggps_ic=".false."
	  ncep_ic=".false."
	  external_ic=".false."
	  mountain=".true."
	  if [ $read_increment = ".true." ]; then # add increment on the fly to the restarts
	    res_latlon_dynamics="fv3_increment.nc"
	  else
	    res_latlon_dynamics='""'
	  fi

	else # CHGRES'd GFS analyses

	  nggps_ic=${nggps_ic:-".true."}
	  ncep_ic=${ncep_ic:-".false."}
	  external_ic=".true."
	  mountain=".false."
	  read_increment=".false."
	  res_latlon_dynamics='""'

	fi

	# Stochastic Physics Options
	if [ ${SET_STP_SEED:-"YES"} = "YES" ]; then
	  ISEED_SKEB=$((CDATE*1000 + MEMBER*10 + 1))
	  ISEED_SHUM=$((CDATE*1000 + MEMBER*10 + 2))
	  ISEED_SPPT=$((CDATE*1000 + MEMBER*10 + 3))
	else
	  ISEED=${ISEED:-0}
	fi
	DO_SKEB=${DO_SKEB:-"NO"}
	DO_SPPT=${DO_SPPT:-"NO"}
	DO_SHUM=${DO_SHUM:-"NO"}
	JCAP_STP=${JCAP_STP:-$JCAP_CASE}
	LONB_STP=${LONB_STP:-$LONB_CASE}
	LATB_STP=${LATB_STP:-$LATB_CASE}

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

FV3_GFS_nml(){
	# namelist output for a certain component
	echo SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3
	if [ $machine = 'sandbox' ]; then
		cd $SCRIPTDIR
		echo "MAIN: !!!Sandbox mode, writing to current directory!!!"
	fi
	# Call child scripts in current script directory
	source $SCRIPTDIR/parsing_namelists_FV3.sh
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
        source $SCRIPTDIR/parsing_namelists_WW3.sh
	WW3_namelists
}

WW3_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
	# soft link commands insert here
}

MOM6_postdet()
{
	echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

	# Copy MOM6 ICs (from CFSv2 file)
	cp -pf $ICSDIR/$CDATE/mom6_da/MOM*nc $DATA/INPUT/

	# Copy MOM6 fixed files
	cp -pf $FIXmom/INPUT/* $DATA/INPUT/
        cp -pf $FIXmom/INPUT/MOM_input_update $DATA/INPUT/MOM_input

	# Copy grid_spec and mosaic files
	cp -pf $FIXgrid/$CASE/${CASE}_mosaic* $DATA/INPUT/
	cp -pf $FIXgrid/$CASE/grid_spec.nc $DATA/INPUT/
	cp -pf $FIXgrid/$CASE/ocean_mask.nc $DATA/INPUT/
	cp -pf $FIXgrid/$CASE/land_mask* $DATA/INPUT/

        # Copy mediator restart files to RUNDIR
        if [ $runtyp = 'continue' ]; then
               cp $ROTDIR/$CDUMP.$PDY/$cyc/mediator_* $DATA/
        fi

	echo "SUB ${FUNCNAME[0]}: MOM6 input data linked/copied"
}

MOM6_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for MOM6"
        source $SCRIPTDIR/parsing_namelists_MOM6.sh
        MOM6_namelists
}

MOM6_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for MOM6"

	export ENSMEM=${ENSMEM:-01}

        export IDATE=$CDATE

	if [ $RUN_ENVIR = "nco" ]; then
	    export COMIN=${COMIN:-$ROTDIR/$RUN.$PDY/$cyc}
	    export COMOUT=${COMOUT:-$ROTDIR/$RUN.$PDY/$cyc}
	else
	    export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
	    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
	fi
	[[ ! -d $COMOUT ]] && mkdir -m 775 -p $COMOUT

        if [ $runtyp = 'initial' ]; then
               cp $DATA/mediator_* $COMOUT/
               status=$?
               exit $status
        fi

        if [ $FHRGRP -eq 0 ]; then
            fhrlst="anl"
        else
            fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/\[/ /g; s/\]/ /g; s/f/ /g; s/,/ /g')
        fi

	# copy ocn files
	for fhr in $fhrlst; do
	  export fhr=$fhr
	  if [[ 10#$fhr -ge 6 ]]; then
       	  hh_inc_m=$((10#$FHOUT/2))
	#hh_inc_m=3
	#hh_in_o=6
	  hh_inc_o=$((10#$FHOUT  ))

  # ------------------------------------------------------
  #  adjust the dates on the mom filenames and save
  # ------------------------------------------------------
	  VDATE=$($NDATE $fhr $IDATE)
	  YYYY=`echo $VDATE | cut -c1-4`
	  MM=`echo $VDATE | cut -c5-6`
	  DD=`echo $VDATE | cut -c7-8`
	  HH=`echo $VDATE | cut -c9-10`
	  SS=$((10#$HH*3600))

	#  m_date=$($NDATE $hh_inc_m $DDATE)
	#  p_date=$($NDATE $hh_inc_o $DDATE)

	  m_date=$($NDATE -$hh_inc_m $VDATE)
	  p_date=$VDATE

	  # This loop probably isn't needed
	    year=`echo $m_date | cut -c1-4`
	    month=`echo $m_date | cut -c5-6`
	    day=`echo $m_date | cut -c7-8`
	    hh=`echo $m_date | cut -c9-10`

	    export ocnfile=ocn_${year}_${month}_${day}_${hh}.nc

	    echo "cp -p $ocnfile $COMOUT/ocn$p_date.$ENSMEM.$IDATE.nc"
	    $NCP -p $ocnfile $COMOUT/ocn$p_date.$ENSMEM.$IDATE.nc
	    status=$?
	    [[ $status -ne 0 ]] && exit $status
	  fi
	done
	$NCP -p $DATA/SST*nc $COMOUT/
        $NCP -p $DATA/input.nml $COMOUT/
        $NCP -p $DATA/ice_in $COMOUT/
        $NCP -p $DATA/INPUT/MOM_input $COMOUT/
}

CICE_postdet()
{
	echo "SUB ${FUNCNAME[0]}: CICE after run type determination"
	# Copy CICE5 IC - pre-generated from CFSv2
        cp -p $ICSDIR/$CDATE/cice5_model_0.25.res_$CDATE.nc $DATA/cice5_model.res_$CDATE.nc
	#cp -p $ICSDIR/$CDATE/cpc/cice5_model_0.25.res_$CDATE.nc ./cice5_model.res_$CDATE.nc

        # Copy CICE5 fixed files, and namelists
        cp -p $FIXcice/kmtu_cice_NEMS_mx025.nc $DATA/
        cp -p $FIXcice/grid_cice_NEMS_mx025.nc $DATA/

        # Copy grid_spec and mosaic files
        cp -pf $FIXgrid/$CASE/${CASE}_mosaic* $DATA/INPUT/
        cp -pf $FIXgrid/$CASE/grid_spec.nc $DATA/INPUT/
        cp -pf $FIXgrid/$CASE/ocean_mask.nc $DATA/INPUT/
        cp -pf $FIXgrid/$CASE/land_mask* $DATA/INPUT/

	iceic=cice5_model.res_$CDATE.nc
	year=$(echo $CDATE|cut -c 1-4)
	#BL2018
	stepsperhr=$((3600/$ICETIM))
	#BL2018
	nhours=$($NHOUR $CDATE ${year}010100)
	steps=$((nhours*stepsperhr))
	npt=$((FHMAX*$stepsperhr))      # Need this in order for dump_last to work

	histfreq_n=${histfreq_n:-6}
	restart_interval=${restart_interval:-1296000}    # restart write interval in seconds, default 15 days
	dumpfreq_n=$restart_interval                     # restart write interval in seconds

	#BL2018
	#dumpfreq='d'
	#dumpfreq='s'

}

CICE_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
        source $SCRIPTDIR/parsing_namelists_CICE.sh
        CICE_namelists
}

CICE_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"
        export ENSMEM=${ENSMEM:-01}
        export IDATE=$CDATE
        if [ $FHRGRP -eq 0 ]; then
            fhrlst="anl"
        else
	    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/\[/ /g; s/\]/ /g; s/f/ /g; s/,/ /g')
        fi

	for fhr in $fhrlst; do
	  export fhr=$fhr
	  #  --------------------------------------
	  #  cp cice data to COMOUT directory
	  #  --------------------------------------
	  YYYY0=`echo $IDATE | cut -c1-4`
	  MM0=`echo $IDATE | cut -c5-6`
	  DD0=`echo $IDATE | cut -c7-8`
	  HH0=`echo $IDATE | cut -c9-10`
	  SS0=$((10#$HH0*3600))

	  VDATE=$($NDATE $fhr $IDATE)
	  YYYY=`echo $VDATE | cut -c1-4`
	  MM=`echo $VDATE | cut -c5-6`
	  DD=`echo $VDATE | cut -c7-8`
	  HH=`echo $VDATE | cut -c9-10`
	  SS=$((10#$HH*3600))

	#  DDATE=$($NDATE -$FHOUT $VDATE)

	  if [[ 10#$fhr -eq 0 ]]; then
	    $NCP -p $DATA/history/iceh_ic.${YYYY0}-${MM0}-${DD0}-`printf "%5.5d" ${SS0}`.nc $COMOUT/iceic$VDATE.$ENSMEM.$IDATE.nc
	    status=$?
	    [[ $status -ne 0 ]] && exit $status
	    echo "fhr is 0, only copying ice initial conditions... exiting"
	  else
	    $NCP -p $DATA/history/iceh_`printf "%0.2d" $FHOUT`h.${YYYY}-${MM}-${DD}-`printf "%5.5d" ${SS}`.nc $COMOUT/ice$VDATE.$ENSMEM.$IDATE.nc
	    status=$?
	    [[ $status -ne 0 ]] && exit $status
	  fi

	done
}

GSD_in()
{
	echo "SUB ${FUNCNAME[0]}: Linking input data for GSD"
	# soft link commands insert here
}

GSD_nml()
{
	echo "SUB ${FUNCNAME[0]}: Creating name list for GSD"
	sh parsing_namelists_GSD.sh
}

GSD_out()
{
	echo "SUB ${FUNCNAME[0]}: Copying output data for GSD"
	# soft link commands insert here
}
