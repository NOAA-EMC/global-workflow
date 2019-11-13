
if [ $RUN_ENVIR = "nco" ]; then
    export COMIN=${COMIN:-$ROTDIR/$RUN.$PDY/$cyc}
    export COMOUT=${COMOUT:-$ROTDIR/$RUN.$PDY/$cyc}
else
    export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
fi
[[ ! -d $COMOUT ]] && mkdir -m 775 -p $COMOUT

export OMP_NUM_THREADS=1
export ENSMEM=${ENSMEM:-01}

export IDATE=$CDATE

$NCP -p $COMIN/*ic .
# copy over every ic file

#---------------------------------------------------------------
echo "PT DEBUG fhrlst is $fhrlst"

FHOUT=$FHOUT_GFS

#BL2019
  #  --------------------------
  #  interpolate ocn/ice netcdf output data to regular grid and generate grib2 files 
  #  --------------------------
for fhr in $fhrlst; do
#  export fhr=$fhr
  VDATE=$($NDATE $fhr $IDATE)
  # Regrid the MOM6 and CICE5 output from tripolar to regular grid via NCL
  # This can take .25 degree input and convert to .5 degree - other opts avail
  # The regrid scripts use CDATE for the current day, restore it to IDATE afterwards
  export CDATE=$VDATE
  cd $DATA
if [ $fhr -gt 0 ]; then
  export MOM6REGRID=$UGCSsrc/mom6_regrid_025
  $MOM6REGRID/run_regrid.sh
  status=$?
  [[ $status -ne 0 ]] && exit $status

  # Convert the netcdf files to grib2
  export executable=$MOM6REGRID/exec/reg2grb2.x
  $MOM6REGRID/run_reg2grb2.sh
  status=$?
  [[ $status -ne 0 ]] && exit $status
fi

done
# Restore CDATE to what is expected
  export CDATE=$IDATE
  echo $pwd
  $NMV SST*nc $COMOUT/
  $NMV ocnr*.nc $COMOUT
  $NMV ocnr*.grb2 $COMOUT
  status=$?
  [[ $status -ne 0 ]] && exit $status

# clean up working folder
#rm -Rf $DATA
#BL2019
###############################################################
# Exit out cleanly
exit 0
