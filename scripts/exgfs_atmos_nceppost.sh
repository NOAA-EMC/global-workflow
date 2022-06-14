#####################################################################
echo "-----------------------------------------------------"
echo " exgfs_nceppost.sh" 
echo " Apr 99 - Michaud - Generated to post global forecast"
echo " Mar 03 - Zhu - Add post for 0.5x0.5 degree"
echo " Nov 03 - Gilbert - Modified from exglobal_post.sh.sms"
echo "                    to run only one master post job."
echo " Jan 07 - Cooke - Add DBNet Alert for Master files"
echo " May 07 - Chuang - Modified scripts to run unified post"
echo " Feb 10 - Carlis - Add 12-hr accum precip bucket at f192"
echo " Jun 12 - Wang   - Add option for grb2"
echo " Jul 14 - Carlis - Add 0.25 deg master "
echo " Mar 17 - F Yang -  Modified for running fv3gfs"
echo " Aug 17 - Meng   - Add flags for turning on/off flx, gtg " 
echo "                   and satellite look like file creation"   
echo "                   and use 3-digit forecast hour naming"
echo "                   post output files"
echo " Dec 17 - Meng - Link sfc data file to flxfile "
echo "                 since fv3gfs does not output sfc files any more." 
echo " Dec 17 - Meng - Add fv3gfs_downstream_nems.sh for pgb processing "
echo " Jan 18 - Meng - Add flag PGBF for truning on/off pgb processing. "
echo " Jan 18 - Meng - For EE2 standard, move IDRT POSTGPVARS setting" 
echo "                 from j-job script."
echo " Feb 18 - Meng - Removed legacy setting for generating grib1 data"
echo "                 and reading sigio model outputs."
echo " Aug 20 - Meng - Remove .ecf extentsion per EE2 review."
echo " Sep 20 - Meng - Update clean up files per EE2 review."
echo " Dec 20 - Meng - Add alert for special data file."
echo " Mar 21 - Meng - Update POSTGRB2TBL default setting."
echo " Jun 21 - Mao  - Instead of err_chk, catch err and print out"
echo "                 WAFS failure warnings to avoid job crashing"
echo " Oct 21 - Meng - Remove jlogfile for wcoss2 transition."
echo " Feb 22 - Lin - Exception handling if anl input not found."
echo "-----------------------------------------------------"
#####################################################################

set -x

cd $DATA

# specify model output format type: 4 for nemsio, 3 for sigio
msg="HAS BEGUN on $(hostname)"
postmsg "$msg"

export POSTGPSH=${POSTGPSH:-$USHgfs/gfs_nceppost.sh}
export GFSDOWNSH=${GFSDOWNSH:-$USHgfs/fv3gfs_downstream_nems.sh}
export GFSDOWNSHF=${GFSDOWNSHF:-$USHgfs/inter_flux.sh}
export GFSDWNSH=${GFSDWNSH:-$USHgfs/fv3gfs_dwn_nems.sh}
export TRIMRH=${TRIMRH:-$USHgfs/trim_rh.sh}
export MODICEC=${MODICEC:-$USHgfs/mod_icec.sh}
export INLINE_POST=${INLINE_POST:-".false."}

############################################################
#  Define Variables:
#  -----------------
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off post.
############################################################
export IO=${LONB:-1440}
export JO=${LATB:-721}
export OUTTYP=${OUTTYP:-4}
export FLXF=${FLXF:-"YES"}
export FLXGF=${FLXGF:-"YES"} 
export GOESF=${GOESF:-"YES"}
export WAFSF=${WAFSF:-"NO"}
export PGBF=${PGBF:-"YES"}
export TCYC=${TCYC:-".t${cyc}z."}
export OUTPUT_FILE=${OUTPUT_FILE:-"nemsio"}
export PREFIX=${PREFIX:-${RUN}${TCYC}}
if [ $OUTTYP -eq 4 ]; then
  if [ $OUTPUT_FILE = "netcdf" ]; then
    export SUFFIX=".nc"
  else
    export SUFFIX=".nemsio"
  fi
else
  export SUFFIX=
fi
export machine=${machine:-WCOSS_C}

###########################
# Specify Output layers
###########################
export POSTGPVARS="KPO=57,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,40.,30.,20.,15.,10.,7.,5.,3.,2.,1.,0.7,0.4,0.2,0.1,0.07,0.04,0.02,0.01,"

##########################################################
# Specify variable to directly output pgrb2 files for GDAS/GFS
##########################################################
export IDRT=${IDRT:-0} # IDRT=0 is setting for outputting grib files on lat/lon grid

############################################################
# Post Analysis Files before starting the Forecast Post
############################################################
# Process analysis when post_times is 00
export stime=$(echo $post_times | cut -c1-3)
if [ $OUTTYP -eq 4 ]; then
  export loganl=$COMIN/${PREFIX}atmanl${SUFFIX}
else
  export loganl=$COMIN/${PREFIX}sanl
fi

if [ ${stime} = "anl" ]; then
  if [ -f $loganl ]; then
    # add new environmental variables for running new ncep post
    # Validation date
    export VDATE=${PDY}${cyc}
    # specify output file name from chgres which is input file name to nceppost
    # if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
    # new imported variable for global_nceppost.sh
    export GFSOUT=${PREFIX}gfsioanl

    # specify smaller control file for GDAS because GDAS does not
    # produce flux file, the default will be /nwprod/parm/gfs_cntrl.parm
    if [ $GRIBVERSION = 'grib2' ]; then
      # use grib2 nomonic table in product g2tmpl directory as default 
      export POSTGRB2TBL=${POSTGRB2TBL:-${g2tmpl_ROOT}/share/params_grib2_tbl_new}
      export PostFlatFile=${PostFlatFile:-$PARMpost/postxconfig-NT-GFS-ANL.txt}
      export CTLFILE=$PARMpost/postcntrl_gfs_anl.xml
    fi

    [[ -f flxfile ]] && rm flxfile ; [[ -f nemsfile ]] && rm nemsfile
    if [ $OUTTYP -eq 4 ]; then
      ln -fs $COMIN/${PREFIX}atmanl${SUFFIX} nemsfile
      export NEMSINP=nemsfile
      ln -fs $COMIN/${PREFIX}sfcanl${SUFFIX} flxfile
      export FLXINP=flxfile
    fi

    export PGBOUT=pgbfile
    export PGIOUT=pgifile
    export PGBOUT2=pgbfile.grib2
    export PGIOUT2=pgifile.grib2.idx
    export IGEN=$IGEN_ANL
    export FILTER=0

    $POSTGPSH
    export err=$?; err_chk

    if [ $GRIBVERSION = 'grib2' ]; then
      mv $PGBOUT $PGBOUT2
    fi

    #  Process pgb files
    if [ "$PGBF" = 'YES' ]; then
      export FH=-1
      export downset=${downset:-2}
      $GFSDOWNSH
      export err=$?; err_chk
    fi

    if [ "$SENDCOM" = 'YES' ]; then
      export fhr3=anl
      if [ $GRIBVERSION = 'grib2' ]; then
        MASTERANL=${PREFIX}master.grb2${fhr3}
        MASTERANLIDX=${PREFIX}master.grb2i${fhr3}
        cp $PGBOUT2 $COMOUT/${MASTERANL}
        $GRB2INDEX $PGBOUT2 $COMOUT/${MASTERANLIDX}
      fi

      if [ "$SENDDBN" = 'YES' ]; then
        $DBNROOT/bin/dbn_alert MODEL GFS_MSC_sfcanl $job $COMOUT/${PREFIX}sfcanl${SUFFIX}
        $DBNROOT/bin/dbn_alert MODEL GFS_SA $job $COMOUT/${PREFIX}atmanl${SUFFIX}
        #alert removed in v15.0       $DBNROOT/bin/dbn_alert MODEL GFS_MASTER $job $COMOUT/${MASTERANL}
        if [ "$PGBF" = 'YES' ]; then
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P25 $job $COMOUT/${PREFIX}pgrb2.0p25.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P25_WIDX $job $COMOUT/${PREFIX}pgrb2.0p25.anl.idx
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P25 $job $COMOUT/${PREFIX}pgrb2b.0p25.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P25_WIDX $job $COMOUT/${PREFIX}pgrb2b.0p25.anl.idx

          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P5 $job $COMOUT/${PREFIX}pgrb2.0p50.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P5_WIDX $job $COMOUT/${PREFIX}pgrb2.0p50.anl.idx
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P5 $job $COMOUT/${PREFIX}pgrb2b.0p50.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P5_WIDX $job $COMOUT/${PREFIX}pgrb2b.0p50.anl.idx

          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_1P0 $job $COMOUT/${PREFIX}pgrb2.1p00.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_1P0_WIDX $job $COMOUT/${PREFIX}pgrb2.1p00.anl.idx
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_1P0 $job $COMOUT/${PREFIX}pgrb2b.1p00.anl
          $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_1P0_WIDX $job $COMOUT/${PREFIX}pgrb2b.1p00.anl.idx
        fi
      fi
    fi
    [[ -f pgbfile.grib2 ]] && rm pgbfile.grib2 
    #   ecflow_client --event release_pgrb2_anl

    ##########################  WAFS U/V/T analysis start ##########################
    # U/V/T on ICAO standard atmospheric pressure levels for WAFS verification
    if [ $WAFSF = "YES" ]; then
      if [[ $RUN = gfs && $GRIBVERSION = 'grib2' ]]; then
        export OUTTYP=${OUTTYP:-4}

        export PostFlatFile=$PARMpost/postxconfig-NT-GFS-WAFS-ANL.txt
        export CTLFILE=$PARMpost/postcntrl_gfs_wafs_anl.xml

        export PGBOUT=wafsfile
        export PGIOUT=wafsifile

        $POSTGPSH
        export err=$?
        if [ $err -ne 0 ]; then
          echo " *** GFS POST WARNING: WAFS output failed for analysis, err=$err"
        else
          # WAFS package doesn't process this part.
          # Need to be saved for WAFS U/V/T verification, 
          # resolution higher than WAFS 1.25 deg for future compatibility
          wafsgrid="latlon 0:1440:0.25 90:721:-0.25"
          $WGRIB2 $PGBOUT -set_grib_type same -new_grid_winds earth \
          -new_grid_interpolation bilinear -set_bitmap 1 \
          -new_grid $wafsgrid ${PGBOUT}.tmp

          if [ $SENDCOM = "YES" ]; then
            cp ${PGBOUT}.tmp $COMOUT/${PREFIX}wafs.0p25.anl
            $WGRIB2 -s ${PGBOUT}.tmp > $COMOUT/${PREFIX}wafs.0p25.anl.idx

            # if [ $SENDDBN = YES ]; then
            #    $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_GB2 $job $COMOUT/${PREFIX}wafs.0p25.anl
            #    $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_GB2__WIDX $job $COMOUT/${PREFIX}wafs.0p25.anl.idx
            # fi
          fi
          rm $PGBOUT ${PGBOUT}.tmp
        fi
      fi
    fi
    ##########################  WAFS U/V/T analysis end  ##########################
  else
    #### atmanl file not found need failing job
    echo " *** FATAL ERROR: No model anl file output "
    export err=9
    err_chk
  fi
else   ## not_anl if_stime
  SLEEP_LOOP_MAX=$(expr $SLEEP_TIME / $SLEEP_INT)

  ############################################################
  # Loop Through the Post Forecast Files 
  ############################################################

  for fhr in $post_times; do
    echo 'Start processing fhr='$post_times
    ###############################
    # Start Looping for the 
    # existence of the restart files
    ###############################
    set -x
    export pgm="postcheck"
    ic=1
    while [ $ic -le $SLEEP_LOOP_MAX ]; do
      if [ -f $restart_file${fhr}.txt ]; then
        break
      else
        ic=$(expr $ic + 1)
        sleep $SLEEP_INT
      fi
      ###############################
      # If we reach this point assume
      # fcst job never reached restart 
      # period and error exit
      ###############################
      if [ $ic -eq $SLEEP_LOOP_MAX ]; then
        echo " *** FATAL ERROR: No model output in nemsio for f${fhr} "
        export err=9
        err_chk
      fi
    done
    set -x

    msg="Starting post for fhr=$fhr"
    postmsg "$msg"

    ###############################
    # Put restart files into /nwges 
    # for backup to start Model Fcst
    ###############################
    [[ -f flxfile ]] && rm flxfile ; [[ -f nemsfile ]] && rm nemsfile
    if [ $OUTTYP -eq 4 ]; then
      ln -fs $COMIN/${PREFIX}atmf${fhr}${SUFFIX} nemsfile
      export NEMSINP=nemsfile
      ln -fs $COMIN/${PREFIX}sfcf${fhr}${SUFFIX} flxfile
      export FLXINP=flxfile
    fi

    if [ $fhr -gt 0 ]; then
      export IGEN=$IGEN_FCST
    else
      export IGEN=$IGEN_ANL
    fi

    export VDATE=$(${NDATE} +${fhr} ${PDY}${cyc})
    export OUTTYP=${OUTTYP:-4}
    export GFSOUT=${PREFIX}gfsio${fhr}

    if [ $GRIBVERSION = 'grib2' ]; then
      export POSTGRB2TBL=${POSTGRB2TBL:-${g2tmpl_ROOT}/share/params_grib2_tbl_new}
      export PostFlatFile=${PostFlatFile:-$PARMpost/postxconfig-NT-GFS.txt}

      if [ $RUN = gfs ]; then
        export IGEN=$IGEN_GFS
        if [ $fhr -gt 0 ]; then export IGEN=$IGEN_FCST ; fi
      else
        export IGEN=$IGEN_GDAS_ANL
        if [ $fhr -gt 0 ]; then export IGEN=$IGEN_FCST ; fi
      fi
      if [[ $RUN = gfs ]]; then
        if [ $fhr -eq 0 ]; then
          export PostFlatFile=$PARMpost/postxconfig-NT-GFS-F00.txt
          export CTLFILE=$PARMpost/postcntrl_gfs_f00.xml
        else
          export CTLFILE=${CTLFILEGFS:-$PARMpost/postcntrl_gfs.xml}
        fi
      else
        if [ $fhr -eq 0 ]; then
          export PostFlatFile=$PARMpost/postxconfig-NT-GFS-F00.txt
          export CTLFILE=${CTLFILEGFS:-$PARMpost/postcntrl_gfs_f00.xml}
        else
          export CTLFILE=${CTLFILEGFS:-$PARMpost/postcntrl_gfs.xml}
        fi
      fi
    fi

    export FLXIOUT=flxifile
    export PGBOUT=pgbfile
    export PGIOUT=pgifile
    export PGBOUT2=pgbfile.grib2
    export PGIOUT2=pgifile.grib2.idx
    export FILTER=0 
    if [ $GRIBVERSION = 'grib2' ]; then
      MASTERFL=${PREFIX}master.grb2f${fhr}
      MASTERFLIDX=${PREFIX}master.grb2if${fhr}
    fi

    if [ $INLINE_POST = ".false." ]; then
      $POSTGPSH
    else
      cp -p $COMOUT/${MASTERFL} $PGBOUT 
    fi
    export err=$?; err_chk

    if [ $GRIBVERSION = 'grib2' ]; then
      mv $PGBOUT $PGBOUT2
    fi

    #  Process pgb files
    if [ "$PGBF" = 'YES' ]; then
      export FH=$(expr $fhr + 0)
      export downset=${downset:-2}
      $GFSDOWNSH
      export err=$?; err_chk
    fi

    if [ $SENDCOM = "YES" ]; then
      if [ $GRIBVERSION = 'grib2' ]; then
        if [ $INLINE_POST = ".false." ]; then 
          cp $PGBOUT2 $COMOUT/${MASTERFL} 
        fi
        $GRB2INDEX $PGBOUT2  $COMOUT/${MASTERFLIDX}
      fi

      if [ "$SENDDBN" = 'YES' ]; then
        if [ $GRIBVERSION = 'grib2' ]; then
          if [ "$PGBF" = 'YES' ]; then
            $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P25 $job $COMOUT/${PREFIX}pgrb2.0p25.f${fhr}
            $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P25_WIDX $job $COMOUT/${PREFIX}pgrb2.0p25.f${fhr}.idx
            $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P25 $job $COMOUT/${PREFIX}pgrb2b.0p25.f${fhr}
            $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P25_WIDX $job $COMOUT/${PREFIX}pgrb2b.0p25.f${fhr}.idx

            if [ -s $COMOUT/${PREFIX}pgrb2.0p50.f${fhr} ]; then
              $DBNROOT/bin/dbn_alert  MODEL GFS_PGB2_0P5 $job $COMOUT/${PREFIX}pgrb2.0p50.f${fhr}
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_0P5_WIDX $job $COMOUT/${PREFIX}pgrb2.0p50.f${fhr}.idx
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P5 $job $COMOUT/${PREFIX}pgrb2b.0p50.f${fhr}
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_0P5_WIDX $job $COMOUT/${PREFIX}pgrb2b.0p50.f${fhr}.idx
            fi

            if [ -s $COMOUT/${PREFIX}pgrb2.1p00.f${fhr} ]; then
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_1P0 $job $COMOUT/${PREFIX}pgrb2.1p00.f${fhr}
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2_1P0_WIDX $job $COMOUT/${PREFIX}pgrb2.1p00.f${fhr}.idx
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_1P0 $job $COMOUT/${PREFIX}pgrb2b.1p00.f${fhr}
              $DBNROOT/bin/dbn_alert MODEL GFS_PGB2B_1P0_WIDX $job $COMOUT/${PREFIX}pgrb2b.1p00.f${fhr}.idx
            fi
          fi
        fi 
      fi

      export fhr
      $USHgfs/gfs_transfer.sh
    fi
    [[ -f pgbfile.grib2 ]] && rm pgbfile.grib2


    # use post to generate GFS Grib2 Flux file as model generated Flux file
    # will be in nemsio format after FY17 upgrade.
    if [ $OUTTYP -eq 4 -a $FLXF = "YES" ]; then
      if [ $fhr -eq 0 ]; then
        export PostFlatFile=$PARMpost/postxconfig-NT-GFS-FLUX-F00.txt
        export CTLFILE=$PARMpost/postcntrl_gfs_flux_f00.xml
      else
        export PostFlatFile=$PARMpost/postxconfig-NT-GFS-FLUX.txt
        export CTLFILE=$PARMpost/postcntrl_gfs_flux.xml
      fi
      export PGBOUT=fluxfile
      export FILTER=0
      export FLUXFL=${PREFIX}sfluxgrbf${fhr}.grib2
      FLUXFLIDX=${PREFIX}sfluxgrbf${fhr}.grib2.idx

      #Add extra flux.1p00 file for coupled
      if [ "$FLXGF" = 'YES' ]; then                    
        export FH=$(expr $fhr + 0)     
        $GFSDOWNSHF                  
        export err=$?; err_chk
      fi   

      if [ $INLINE_POST = ".false." ]; then
        $POSTGPSH
        export err=$?; err_chk
        mv fluxfile $COMOUT/${FLUXFL}
      fi
      $WGRIB2 -s $COMOUT/${FLUXFL} > $COMOUT/${FLUXFLIDX}

      if [ "$SENDDBN" = 'YES' ]; then
        $DBNROOT/bin/dbn_alert MODEL GFS_SGB_GB2 $job $COMOUT/${FLUXFL}
        $DBNROOT/bin/dbn_alert MODEL GFS_SGB_GB2_WIDX $job $COMOUT/${FLUXFLIDX}
      fi
    fi

    # process satellite look alike separately so that master pgb gets out in time    
    # set outtyp to 2 because master post already generates gfs io files
    if [ $GOESF = "YES" ]; then
      export OUTTYP=${OUTTYP:-4}     

      # specify output file name from chgres which is input file name to nceppost 
      # if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
      # new imported variable for global_nceppost.sh

      export GFSOUT=${PREFIX}gfsio${fhr}     

      # link satellite coefficients files, use hwrf version as ops crtm 2.0.5
      # does not new coefficient files used by post
      export FIXCRTM=${FIXCRTM:-${CRTM_FIX}}
      $USHgfs/link_crtm_fix.sh $FIXCRTM

      if [ $GRIBVERSION = 'grib2' ]; then 
        export PostFlatFile=$PARMpost/postxconfig-NT-GFS-GOES.txt      
        export CTLFILE=$PARMpost/postcntrl_gfs_goes.xml
      fi
      export FLXINP=flxfile
      export FLXIOUT=flxifile
      export PGBOUT=goesfile
      export PGIOUT=goesifile
      export FILTER=0
      export IO=0
      export JO=0
      export IGEN=0

      if [ $NET = gfs ]; then
        $POSTGPSH
        export err=$?; err_chk
      fi

      if [ $GRIBVERSION = 'grib2' ]; then
        SPECIALFL=${PREFIX}special.grb2
        SPECIALFLIDX=${PREFIX}special.grb2i
      fi
      fhr3=$fhr

      if [ $SENDCOM = "YES" ]; then
        #       echo "$PDY$cyc$pad$fhr" > $COMOUT/${RUN}.t${cyc}z.master.control

        mv goesfile $COMOUT/${SPECIALFL}f$fhr
        mv goesifile $COMOUT/${SPECIALFLIDX}f$fhr

        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL GFS_SPECIAL_GB2 $job $COMOUT/${SPECIALFL}f$fhr
        fi
      fi
    fi
    # end of satellite processing

    ##########################  WAFS start ##########################
    # Generate WAFS products on ICAO standard level.
    # Do not need to be sent out to public, WAFS package will process the data.
    if [[ $WAFSF = "YES"  && $fhr -le 120 ]]; then
      if [[ $RUN = gfs && $GRIBVERSION = 'grib2' ]]; then
        export OUTTYP=${OUTTYP:-4}

        # Extend WAFS icing and gtg up to 120 hours
        export PostFlatFile=$PARMpost/postxconfig-NT-GFS-WAFS.txt
        export CTLFILE=$PARMpost/postcntrl_gfs_wafs.xml

        # gtg has its own configurations
        cp $PARMpost/gtg.config.gfs gtg.config
        cp $PARMpost/gtg_imprintings.txt gtg_imprintings.txt

        export PGBOUT=wafsfile
        export PGIOUT=wafsifile

        # WAFS data is processed:
        #   hourly if fhr<=24
        #   every 3 forecast hour if 24<fhr<=48
        #   every 6 forecast hour if 48<fhr<=120
        if [  $fhr -le 24  ]; then
          $POSTGPSH
        elif [  $fhr -le 48  ]; then
          if [  $((10#$fhr%3)) -eq 0  ]; then
            $POSTGPSH
          fi
        elif [  $((10#$fhr%6)) -eq 0  ]; then
          $POSTGPSH
        fi

        export err=$?; err_chk
        if [ $err -ne 0 ]; then
          echo " *** GFS POST WARNING: WAFS output failed for f${fhr}, err=$err"
        else
          if [ -e $PGBOUT ]; then
            if [ $SENDCOM = "YES" ]; then
              cp $PGBOUT $COMOUT/${PREFIX}wafs.grb2f$fhr
              cp $PGIOUT $COMOUT/${PREFIX}wafs.grb2if$fhr
            fi
          fi
        fi
      fi
      [[ -f wafsfile ]] && rm wafsfile ; [[ -f wafsifile ]] && rm wafsifile
    fi
    ###########################  WAFS  end ###########################
  done

  #----------------------------------
fi   ## end_if_stime

#cat $pgmout
#msg='ENDED NORMALLY.'
#postmsg "$jlogfile" "$msg"

exit 0

################## END OF SCRIPT #######################
