#! /usr/bin/env bash

#####################################################################
# echo "-----------------------------------------------------"
# echo " exgdas_nceppost.sh" 
# echo " Sep 07 - Chuang - Modified script to run unified post"
# echo " July 14 - Carlis - Changed to 0.25 deg grib2 master file"
# echo " Feb 16 - Lin - Modify to use Vertical Structure"
# echo " Aug 17 - Meng - Modify to use 3-digit forecast hour naming"
# echo "                 master and flux files"
# echo " Dec 17 - Meng - Link sfc data file to flxfile "
# echo "                 since fv3gfs does not output sfc files any more."
# echo " Dec 17 - Meng - Add fv3gfs_downstream_nems.sh for pgb processing "
# echo "                 and remove writing data file to /nwges"
# echo " Jan 18 - Meng - For EE2 standard, move IDRT POSTGPVARS setting"
# echo "                 from j-job script."
# echo " Feb 18 - Meng - Removed legacy setting for generating grib1 data"
# echo "                 and reading sigio model outputs."
# echo " Aug 20 - Meng - Remove .ecf extentsion per EE2 review."
# echo " Sep 20 - Meng - Update clean up files per EE2 review."
# echo " Mar 21 - Meng - Update POSTGRB2TBL default setting."
# echo " Oct 21 - Meng - Remove jlogfile for wcoss2 transition."
# echo " Feb 22 - Lin - Exception handling if anl input not found."
# echo "-----------------------------------------------------"
#####################################################################

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1

export POSTGPSH=${POSTGPSH:-${USHgfs}/gfs_post.sh}
export GFSDOWNSH=${GFSDOWNSH:-${USHgfs}/fv3gfs_downstream_nems.sh}
export GFSDWNSH=${GFSDWNSH:-${USHgfs}/fv3gfs_dwn_nems.sh}
export TRIMRH=${TRIMRH:-${USHgfs}/trim_rh.sh}
export MODICEC=${MODICEC:-${USHgfs}/mod_icec.sh}
export INLINE_POST=${INLINE_POST:-".false."}

############################################################
#  Define Variables:
#  -----------------
#  fhr          is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off post.
############################################################

export IO=${LONB:-1440}
export JO=${LATB:-721}
# specify default model output format: 3 for sigio and 4
# for nemsio
export OUTTYP=${OUTTYP:-4}
export PREFIX=${PREFIX:-${RUN}.t${cyc}z.}
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
# Chuang: modify to process analysis when post_times is 00
stime="$(echo "${post_times}" | cut -c1-3)"
export stime
export loganl="${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"

if [[ "${stime}" = "anl" ]]; then
  if [[ -f "${loganl}" ]]; then
    # add new environmental variables for running new ncep post
    # Validation date

    export VDATE=${PDY}${cyc}

    # set outtyp to 1 because we need to run chgres in the post before model start running chgres
    # otherwise set to 0, then chgres will not be executed in global_nceppost.sh

    export OUTTYP=${OUTTYP:-4}

    # specify output file name from chgres which is input file name to nceppost
    # if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
    # new imported variable for global_nceppost.sh

    export GFSOUT=${RUN}.${cycle}.gfsioanl

    # specify smaller control file for GDAS because GDAS does not
    # produce flux file, the default will be /nwprod/parm/gfs_cntrl.parm

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
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
    export IGEN="${IGEN_ANL}"
    export FILTER=0

    # specify fhr even for analysis because postgp uses it
    #  export fhr=00

    ${POSTGPSH}
    export err=$?; err_chk

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      mv "${PGBOUT}" "${PGBOUT2}"

      #Proces pgb files
      export FH=-1
      export downset=${downset:-1}
      ${GFSDOWNSH}
      export err=$?; err_chk
    fi

    if [[ "${SENDCOM}" = 'YES' ]]; then
      if [[ "${GRIBVERSION}" = 'grib2' ]]; then
        MASTERANL=${PREFIX}master.grb2anl
        ##########XXW Accord to Boi, fortran index should use *if${fhr}, wgrib index use .idx
        #MASTERANLIDX=${RUN}.${cycle}.master.grb2${fhr3}.idx
        MASTERANLIDX=${PREFIX}master.grb2ianl
        cp "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERANL}"
        ${GRB2INDEX} "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERANLIDX}"
      fi

      if [[ "${SENDDBN}" = 'YES' ]]; then
        run="$(echo "${RUN}" | tr '[:lower:]' '[:upper:]')"
        if [[ "${GRIBVERSION}" = 'grib2' ]]; then
          "${DBNROOT}/bin/dbn_alert" MODEL "${run}_MSC_sfcanl" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc"
          "${DBNROOT}/bin/dbn_alert" MODEL "${run}_SA" "${job}" "${COMIN}/${PREFIX}atmanl.nc"
          "${DBNROOT}/bin/dbn_alert" MODEL "GDAS_PGA_GB2" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl"
          "${DBNROOT}/bin/dbn_alert" MODEL "GDAS_PGA_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl.idx"
        fi
      fi
    fi
    rm pgbfile.grib2
  else
    #### atmanl file not found need failing job
    echo " *** FATAL ERROR: No model anl file output "
    export err=9
    err_chk
  fi
else   ## not_anl if_stimes
  SLEEP_LOOP_MAX=$(( SLEEP_TIME / SLEEP_INT ))

  ############################################################
  # Loop Through the Post Forecast Files
  ############################################################

  for fhr in ${post_times}; do
    # Enforce decimal math expressions
    d_fhr=$((10#${fhr}))
    ###############################
    # Start Looping for the
    # existence of the restart files
    ###############################
    export pgm="postcheck"
    ic=1
    while (( ic <= SLEEP_LOOP_MAX )); do
      if [[ -f "${restart_file}${fhr}.txt" ]]; then
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
        echo " *** FATAL ERROR: No model output for f${fhr} "
        export err=9
        err_chk
      fi
    done

    ###############################
    # Put restart files into /nwges
    # for backup to start Model Fcst
    ###############################
    [[ -f flxfile ]] && rm flxfile
    [[ -f nemsfile ]] && rm nemsfile
    ln -sf "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr}.nc" nemsfile
    export NEMSINP=nemsfile
    ln -sf "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr}.nc" flxfile
    export FLXINP=flxfile

    if (( d_fhr > 0 )); then
      export IGEN=${IGEN_FCST}
    else
      export IGEN=${IGEN_ANL}
    fi

    # add new environmental variables for running new ncep post
    # Validation date

    # No shellcheck, NDATE is not a typo
    # shellcheck disable=SC2153
    VDATE="$(${NDATE} "+${fhr}" "${PDY}${cyc}")"
    # shellcheck disable=
    export VDATE

    # set to 3 to output lat/lon grid

    export OUTTYP=${OUTTYP:-4}

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      export POSTGRB2TBL="${POSTGRB2TBL:-${g2tmpl_ROOT}/share/params_grib2_tbl_new}"
      export PostFlatFile="${PARMpost}/postxconfig-NT-GFS.txt"
      if [[ "${RUN}" = gfs ]]; then
        export IGEN="${IGEN_GFS}"
        if (( d_fhr > 0 )); then export IGEN="${IGEN_FCST}" ; fi
      else
        export IGEN="${IGEN_GDAS_ANL}"
        if (( d_fhr > 0 )); then export IGEN="${IGEN_FCST}" ; fi
      fi
      if [[ "${RUN}" = gfs ]]; then
        if (( d_fhr == 0 )); then
          export PostFlatFile="${PARMpost}/postxconfig-NT-GFS-F00.txt"
          export CTLFILE="${PARMpost}/postcntrl_gfs_f00.xml"
        else
          export CTLFILE="${CTLFILEGFS:-${PARMpost}/postcntrl_gfs.xml}"
        fi
      else
        if (( d_fhr == 0 )); then
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
    export fhr3=${fhr}
    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      MASTERFHR=${PREFIX}master.grb2f${fhr}
      MASTERFHRIDX=${PREFIX}master.grb2if${fhr}
    fi

    if [[ "${INLINE_POST}" = ".false." ]]; then
      ${POSTGPSH}
    else
      cp "${COM_ATMOS_MASTER}/${MASTERFHR}" "${PGBOUT}"
    fi
    export err=$?; err_chk

    if [[ "${GRIBVERSION}" = 'grib2' ]]; then
      mv "${PGBOUT}" "${PGBOUT2}"
    fi

    #wm Process pgb files
    export FH=$(( 10#${fhr} + 0 ))
    export downset=${downset:-1}
    ${GFSDOWNSH}
    export err=$?; err_chk

    if [[ "${SENDDBN}" = "YES" ]]; then
      run="$(echo "${RUN}" | tr '[:lower:]' '[:upper:]')"
      "${DBNROOT}/bin/dbn_alert" MODEL "${run}_PGB2_0P25" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${run}_PGB2_0P25_WIDX ""${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr}.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${run}_PGB_GB2" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${run}_PGB_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr}.idx"
    fi


    if [[ "${SENDCOM}" = 'YES' ]]; then
      if [[ "${GRIBVERSION}" = 'grib2' ]]; then
        if [[ "${INLINE_POST}" = ".false." ]]; then
          cp "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERFHR}"
        fi
        ${GRB2INDEX} "${PGBOUT2}" "${COM_ATMOS_MASTER}/${MASTERFHRIDX}"
      fi

      # Model generated flux files will be in nemsio after FY17 upgrade
      # use post to generate Grib2 flux files

      if (( OUTTYP == 4 )) ; then
        export NEMSINP=${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr}.nc
        export FLXINP=${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr}.nc
        if (( d_fhr == 0 )); then
          export PostFlatFile=${PARMpost}/postxconfig-NT-GFS-FLUX-F00.txt
          export CTLFILE=${PARMpost}/postcntrl_gfs_flux_f00.xml
        else
          export PostFlatFile=${PARMpost}/postxconfig-NT-GFS-FLUX.txt
          export CTLFILE=${PARMpost}/postcntrl_gfs_flux.xml
        fi
        export PGBOUT=fluxfile
        export FILTER=0
        FLUXFL=${PREFIX}sfluxgrbf${fhr}.grib2
        FLUXFLIDX=${PREFIX}sfluxgrbf${fhr}.grib2.idx

        if [[ "${INLINE_POST}" = ".false." ]]; then
          ${POSTGPSH}
          export err=$?; err_chk
          mv fluxfile "${COM_ATMOS_MASTER}/${FLUXFL}"
        fi
        ${WGRIB2} -s "${COM_ATMOS_MASTER}/${FLUXFL}" > "${COM_ATMOS_MASTER}/${FLUXFLIDX}"
      fi

      if [[ "${SENDDBN}" = 'YES' ]] && [[ "${RUN}" = 'gdas' ]] && (( d_fhr % 3 == 0 )); then
        "${DBNROOT}/bin/dbn_alert" MODEL "${run}_SF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${run}_BF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${run}_SGB_GB2" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${run}_SGB_GB2_WIDX ""${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr}.grib2.idx"
      fi
    fi

    [[ -f pgbfile.grib2 ]] && rm pgbfile.grib2
    [[ -f flxfile ]] && rm flxfile
  done
fi   ## end_if_times

exit 0

################## END OF SCRIPT #######################
