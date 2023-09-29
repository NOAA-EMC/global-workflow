#! /usr/bin/env bash

#####################################################################
# echo "-----------------------------------------------------"
# echo " exgfs_nceppost.sh" 
# echo " Apr 99 - Michaud - Generated to post global forecast"
# echo " Mar 03 - Zhu - Add post for 0.5x0.5 degree"
# echo " Nov 03 - Gilbert - Modified from exglobal_post.sh.sms"
# echo "                    to run only one master post job."
# echo " Jan 07 - Cooke - Add DBNet Alert for Master files"
# echo " May 07 - Chuang - Modified scripts to run unified post"
# echo " Feb 10 - Carlis - Add 12-hr accum precip bucket at f192"
# echo " Jun 12 - Wang   - Add option for grb2"
# echo " Jul 14 - Carlis - Add 0.25 deg master "
# echo " Mar 17 - F Yang -  Modified for running fv3gfs"
# echo " Aug 17 - Meng   - Add flags for turning on/off flx, gtg " 
# echo "                   and satellite look like file creation"   
# echo "                   and use 3-digit forecast hour naming"
# echo "                   post output files"
# echo " Dec 17 - Meng - Link sfc data file to flxfile "
# echo "                 since fv3gfs does not output sfc files any more." 
# echo " Dec 17 - Meng - Add fv3gfs_downstream_nems.sh for pgb processing "
# echo " Jan 18 - Meng - Add flag PGBF for truning on/off pgb processing. "
# echo " Jan 18 - Meng - For EE2 standard, move IDRT POSTGPVARS setting" 
# echo "                 from j-job script."
# echo " Feb 18 - Meng - Removed legacy setting for generating grib1 data"
# echo "                 and reading sigio model outputs."
# echo " Aug 20 - Meng - Remove .ecf extentsion per EE2 review."
# echo " Sep 20 - Meng - Update clean up files per EE2 review."
# echo " Dec 20 - Meng - Add alert for special data file."
# echo " Mar 21 - Meng - Update POSTGRB2TBL default setting."
# echo " Oct 21 - Meng - Remove jlogfile for wcoss2 transition."
# echo " Feb 22 - Lin - Exception handling if anl input not found."
# echo "-----------------------------------------------------"
#####################################################################

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1

export POSTGPSH=${POSTGPSH:-${USHgfs}/gfs_post.sh}
export GFSDOWNSH=${GFSDOWNSH:-${USHgfs}/fv3gfs_downstream_nems.sh}
export GFSDOWNSHF=${GFSDOWNSHF:-${USHgfs}/inter_flux.sh}
export GFSDWNSH=${GFSDWNSH:-${USHgfs}/fv3gfs_dwn_nems.sh}
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
export PGBF=${PGBF:-"YES"}
export TCYC=${TCYC:-".t${cyc}z."}
export PREFIX=${PREFIX:-${RUN}${TCYC}}
export machine=${machine:-WCOSS2}

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
stime="$(echo "${post_times}" | cut -c1-3)"
export stime
export loganl="${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"

if [[ "${stime}" = "anl" ]]; then
  if [[ -f "${loganl}" ]]; then
    # add new environmental variables for running new ncep post
    # Validation date
    export VDATE=${PDY}${cyc}
    # specify output file name from chgres which is input file name to nceppost
    # if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
    # new imported variable for global_nceppost.sh
    export GFSOUT=${PREFIX}gfsioanl

    # specify smaller control file for GDAS because GDAS does not
    # produce flux file, the default will be /nwprod/parm/gfs_cntrl.parm
    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      # use grib2 nomonic table in product g2tmpl directory as default 
      export POSTGRB2TBL=${POSTGRB2TBL:-${g2tmpl_ROOT}/share/params_grib2_tbl_new}
      export PostFlatFile=${PostFlatFile:-${PARMpost}/postxconfig-NT-GFS-ANL.txt}
      export CTLFILE=${PARMpost}/postcntrl_gfs_anl.xml
    fi

    [[ -f flxfile ]] && rm flxfile ; [[ -f nemsfile ]] && rm nemsfile
    ln -fs "${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc" nemsfile
    export NEMSINP=nemsfile
    ln -fs "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc" flxfile
    export FLXINP=flxfile

    export PGBOUT=pgbfile
    export PGIOUT=pgifile
    export PGBOUT2=pgbfile.grib2
    export PGIOUT2=pgifile.grib2.idx
    export IGEN=${IGEN_ANL}
    export FILTER=0

    ${POSTGPSH}
    export err=$?; err_chk

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      mv "${PGBOUT}" "${PGBOUT2}"
    fi

    #  Process pgb files
    if [[ "${PGBF}" = 'YES' ]]; then
      export FH=-1
      export downset=${downset:-2}
      ${GFSDOWNSH}
      export err=$?; err_chk
    fi

    if [[ "${SENDCOM}" = 'YES' ]]; then
      export fhr3=anl
      if [[ "${GRIBVERSION}" = 'grib2' ]]; then
        MASTERANL=${PREFIX}master.grb2${fhr3}
        MASTERANLIDX=${PREFIX}master.grb2i${fhr3}
        cp "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERANL}"
        ${GRB2INDEX} "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERANLIDX}"
      fi

      if [[ "${SENDDBN}" = 'YES' ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL GFS_MSC_sfcanl "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL GFS_SA "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"
        if [[ "${PGBF}" = 'YES' ]]; then
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl.idx"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl.idx"

          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl.idx"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl.idx"

          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl.idx"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl.idx"
        fi
      fi
    fi
    [[ -f pgbfile.grib2 ]] && rm pgbfile.grib2 
    #   ecflow_client --event release_pgrb2_anl
  else
    #### atmanl file not found need failing job
    echo " *** FATAL ERROR: No model anl file output "
    export err=9
    err_chk
  fi
else   ## not_anl if_stime
  SLEEP_LOOP_MAX=$(( SLEEP_TIME / SLEEP_INT ))

  ############################################################
  # Loop Through the Post Forecast Files 
  ############################################################

  for fhr3 in ${post_times}; do
    echo "Start processing fhr=${fhr3}"
    fhr=$(( 10#${fhr3} ))
    ###############################
    # Start Looping for the 
    # existence of the restart files
    ###############################
    export pgm="postcheck"
    ic=1
    while (( ic <= SLEEP_LOOP_MAX )); do
      if [[ -f "${restart_file}${fhr3}.txt" ]]; then
        break
      else
        ic=$(( ic + 1 ))
        sleep "${SLEEP_INT}"
      fi
      ###############################
      # If we reach this point assume
      # fcst job never reached restart 
      # period and error exit
      ###############################
      if (( ic == SLEEP_LOOP_MAX )); then
        echo " *** FATAL ERROR: No model output for f${fhr3} "
        export err=9
        err_chk
      fi
    done

    ###############################
    # Put restart files into /nwges 
    # for backup to start Model Fcst
    ###############################
    [[ -f flxfile ]] && rm flxfile ; [[ -f nemsfile ]] && rm nemsfile
    ln -fs "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr3}.nc" nemsfile
    export NEMSINP=nemsfile
    ln -fs "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr3}.nc" flxfile
    export FLXINP=flxfile

    if (( fhr > 0 )); then
      export IGEN=${IGEN_FCST}
    else
      export IGEN=${IGEN_ANL}
    fi

    # No shellcheck, NDATE is not a typo
    # shellcheck disable=SC2153
    VDATE="$(${NDATE} "+${fhr}" "${PDY}${cyc}")"
    # shellcheck disable=
    export VDATE
    export OUTTYP=${OUTTYP:-4}
    export GFSOUT="${PREFIX}gfsio${fhr3}"

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      export POSTGRB2TBL="${POSTGRB2TBL:-${g2tmpl_ROOT}/share/params_grib2_tbl_new}"
      export PostFlatFile="${PostFlatFile:-${PARMpost}/postxconfig-NT-GFS.txt}"

      if [[ "${RUN}" = "gfs" ]]; then
        export IGEN=${IGEN_GFS}
        if (( fhr > 0 )); then export IGEN=${IGEN_FCST} ; fi
      else
        export IGEN=${IGEN_GDAS_ANL}
        if (( fhr > 0 )); then export IGEN=${IGEN_FCST} ; fi
      fi
      if [[ "${RUN}" = "gfs" ]]; then
        if (( fhr == 0 )); then
          export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-F00.txt"
          export CTLFILE="${PARMpost}/postcntrl_gfs_f00.xml"
        else
          export CTLFILE="${CTLFILEGFS:-${PARMpost}/postcntrl_gfs.xml}"
        fi
      else
        if (( fhr == 0 )); then
          export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-F00.txt"
          export CTLFILE="${CTLFILEGFS:-${PARMpost}/postcntrl_gfs_f00.xml}"
        else
          export CTLFILE="${CTLFILEGFS:-${PARMpost}/postcntrl_gfs.xml}"
        fi
      fi
    fi

    export FLXIOUT=flxifile
    export PGBOUT=pgbfile
    export PGIOUT=pgifile
    export PGBOUT2=pgbfile.grib2
    export PGIOUT2=pgifile.grib2.idx
    export FILTER=0 
    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      MASTERFL=${PREFIX}master.grb2f${fhr3}
      MASTERFLIDX=${PREFIX}master.grb2if${fhr3}
    fi

    if [[ "${INLINE_POST}" = ".false." ]]; then
      ${POSTGPSH}
    else
      cp -p "${COM_ATMOS_MASTER}/${MASTERFL}" "${PGBOUT}" 
    fi
    export err=$?; err_chk

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      mv "${PGBOUT}" "${PGBOUT2}"
    fi

    #  Process pgb files
    if [[ "${PGBF}" = 'YES' ]]; then
      export FH=$(( fhr ))
      export downset=${downset:-2}
      ${GFSDOWNSH}
      export err=$?; err_chk
    fi

    if [[ "${SENDCOM}" = "YES" ]]; then
      if [[ "${GRIBVERSION}" = 'grib2' ]]; then
        if [[ "${INLINE_POST}" = ".false." ]]; then 
          cp "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERFL}"
        fi
        ${GRB2INDEX} "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERFLIDX}"
      fi

      if [[ "${SENDDBN}" = 'YES' ]]; then
        if [[ "${GRIBVERSION}" = 'grib2' ]]; then
          if [[ "${PGBF}" = 'YES' ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}"
            "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}.idx"
            "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}"
            "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}.idx"

            if [[ -s "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}" ]]; then
              "${DBNROOT}/bin/dbn_alert"  MODEL GFS_PGB2_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}.idx"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}.idx"
            fi

            if [[ -s "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}" ]]; then
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}.idx"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}"
              "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGB2B_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}.idx"
            fi
          fi
        fi 
      fi

      export fhr
      "${USHgfs}/gfs_transfer.sh"
    fi
    [[ -f pgbfile.grib2 ]] && rm pgbfile.grib2


    # use post to generate GFS Grib2 Flux file as model generated Flux file
    # will be in nemsio format after FY17 upgrade.
    if (( OUTTYP == 4 )) && [[ "${FLXF}" == "YES" ]]; then
      if (( fhr == 0 )); then
        export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-FLUX-F00.txt"
        export CTLFILE="${PARMpost}/postcntrl_gfs_flux_f00.xml"
      else
        export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-FLUX.txt"
        export CTLFILE="${PARMpost}/postcntrl_gfs_flux.xml"
      fi
      export PGBOUT=fluxfile
      export FILTER=0
      export FLUXFL=${PREFIX}sfluxgrbf${fhr3}.grib2
      FLUXFLIDX=${PREFIX}sfluxgrbf${fhr3}.grib2.idx

      if [[ "${INLINE_POST}" = ".false." ]]; then
        ${POSTGPSH}
        export err=$?; err_chk
        mv fluxfile "${COM_ATMOS_MASTER}/${FLUXFL}"
      fi
      ${WGRIB2} -s "${COM_ATMOS_MASTER}/${FLUXFL}" > "${COM_ATMOS_MASTER}/${FLUXFLIDX}"

      #Add extra flux.1p00 file for coupled
      if [[ "${FLXGF}" = 'YES' ]]; then
        export FH=$(( fhr ))
        ${GFSDOWNSHF}
        export err=$?; err_chk
      fi

      if [[ "${SENDDBN}" = 'YES' ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL GFS_SGB_GB2 "${job}" "${COM_ATMOS_MASTER}/${FLUXFL}"
        "${DBNROOT}/bin/dbn_alert" MODEL GFS_SGB_GB2_WIDX "${job}" "${COM_ATMOS_MASTER}/${FLUXFLIDX}"
      fi
    fi

    # process satellite look alike separately so that master pgb gets out in time    
    # set outtyp to 2 because master post already generates gfs io files
    if [[ "${GOESF}" = "YES" ]]; then
      export OUTTYP=${OUTTYP:-4}

      # specify output file name from chgres which is input file name to nceppost 
      # if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
      # new imported variable for global_post.sh

      export GFSOUT=${PREFIX}gfsio${fhr3}

      # link satellite coefficients files, use hwrf version as ops crtm 2.0.5
      # does not new coefficient files used by post
      export FIXCRTM="${FIXCRTM:-${CRTM_FIX}}"
      "${USHgfs}/link_crtm_fix.sh" "${FIXCRTM}"

      if [[ "${GRIBVERSION}" = 'grib2' ]]; then 
        export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-GOES.txt"
        export CTLFILE="${PARMpost}/postcntrl_gfs_goes.xml"
      fi
      export FLXINP=flxfile
      export FLXIOUT=flxifile
      export PGBOUT=goesfile
      export PGIOUT=goesifile
      export FILTER=0
      export IO=0
      export JO=0
      export IGEN=0

      if [[ "${NET}" = "gfs" ]]; then
        ${POSTGPSH}
        export err=$?; err_chk
      fi

      if [[ "${GRIBVERSION}" = 'grib2' ]]; then
        SPECIALFL="${PREFIX}special.grb2"
        SPECIALFLIDX="${PREFIX}special.grb2i"
      fi

      if [[ "${SENDCOM}" = "YES" ]]; then
        mv goesfile "${COM_ATMOS_GOES}/${SPECIALFL}f${fhr3}"
        mv goesifile "${COM_ATMOS_GOES}/${SPECIALFLIDX}f${fhr3}"

        if [[ "${SENDDBN}" = "YES" ]]; then
          "${DBNROOT}/bin/dbn_alert" MODEL GFS_SPECIAL_GB2 "${job}" "${COM_ATMOS_GOES}/${SPECIALFL}f${fhr3}"
        fi
      fi
    fi
    # end of satellite processing
  done

  #----------------------------------
fi   ## end_if_stime



exit 0

################## END OF SCRIPT #######################
