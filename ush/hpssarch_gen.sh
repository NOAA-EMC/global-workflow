#! /usr/bin/env bash

###################################################
# Fanglin Yang, 20180318
# --create bunches of files to be archived to HPSS
###################################################
source "${USHgfs}/preamble.sh"

type=${1:-gfs}                ##gfs, gdas, enkfgdas or enkfggfs

ARCH_GAUSSIAN=${ARCH_GAUSSIAN:-"YES"}
ARCH_GAUSSIAN_FHMAX=${ARCH_GAUSSIAN_FHMAX:-36}
ARCH_GAUSSIAN_FHINC=${ARCH_GAUSSIAN_FHINC:-6}

# Set whether to archive downstream products
DO_DOWN=${DO_DOWN:-"NO"}
if [[ ${DO_BUFRSND} = "YES" ]]; then
  export DO_DOWN="YES"
fi

#-----------------------------------------------------
if [[ ${type} = "gfs" ]]; then
#-----------------------------------------------------
  FHMIN_GFS=${FHMIN_GFS:-0}
  FHMAX_GFS=${FHMAX_GFS:-384}
  FHOUT_GFS=${FHOUT_GFS:-3}
  FHMAX_HF_GFS=${FHMAX_HF_GFS:-120}
  FHOUT_HF_GFS=${FHOUT_HF_GFS:-1}

  rm -f "${DATA}/gfsa.txt"
  rm -f "${DATA}/gfsb.txt"
  rm -f "${DATA}/gfs_restarta.txt"
  touch "${DATA}/gfsa.txt"
  touch "${DATA}/gfsb.txt"
  touch "${DATA}/gfs_restarta.txt"

  if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
    rm -f "${DATA}/gfs_pgrb2b.txt"
    rm -f "${DATA}/gfs_netcdfb.txt"
    rm -f "${DATA}/gfs_flux.txt"
    touch "${DATA}/gfs_pgrb2b.txt"
    touch "${DATA}/gfs_netcdfb.txt"
    touch "${DATA}/gfs_flux.txt"

    if [[ ${MODE} = "cycled" ]]; then
      rm -f "${DATA}/gfs_netcdfa.txt"
      touch "${DATA}/gfs_netcdfa.txt"
    fi
  fi

  if [[ ${DO_DOWN} = "YES" ]]; then
    rm -f "${DATA}/gfs_downstream.txt"
    touch "${DATA}/gfs_downstream.txt"
  fi

  head="gfs.t${cyc}z."

  if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
    {
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.anl"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.anl.idx"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.anl"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.anl.idx"
    } >> "${DATA}/gfs_pgrb2b.txt"

    if [[ ${MODE} = "cycled" ]]; then
      {
        echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmanl.nc"
        echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}sfcanl.nc"
        echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmi*.nc"
        gsida_files=("dtfanl.nc"
                     "loginc.txt")
        for file in "${gsida_files[@]}"; do
          [[ -s ${COM_ATMOS_ANALYSIS}/${head}${file} ]] && echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}${file}"
        done
      } >> "${DATA}/gfs_netcdfa.txt"
    fi

    fh=0
    while (( fh <= ARCH_GAUSSIAN_FHMAX )); do
      fhr=$(printf %03i "${fh}")
      {
        echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atmf${fhr}.nc"
        echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}sfcf${fhr}.nc"
      } >> "${DATA}/gfs_netcdfb.txt"
      fh=$((fh+ARCH_GAUSSIAN_FHINC))
    done
  fi

  #..................
  # Exclude the gfsarch.log file, which will change during the tar operation
  #  This uses the bash extended globbing option
  {
    echo "./logs/${PDY}${cyc}/gfs!(arch).log"
    echo "${COM_CONF/${ROTDIR}\//}/ufs.input.nml"

    if [[ ${MODE} = "cycled" ]]; then
      if [[ -s "${COM_ATMOS_ANALYSIS}/${head}gsistat" ]]; then
         echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}gsistat"
      fi
      gsiob_files=("nsstbufr"
                   "prepbufr"
                   "prepbufr.acft_profiles")
      for file in "${gsiob_files[@]}"; do
        [[ -s ${COM_OBS}/${head}${file} ]] && echo "${COM_OBS/${ROTDIR}\//}/${head}${file}"
      done
      if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atmvar.yaml" ]]; then
         echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmvar.yaml"
      fi
      if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atmstat" ]]; then
         echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmstat"
      fi
    fi

    echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.anl"
    echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.anl.idx"

    #Only generated if there are cyclones to track
    cyclone_files=("avno.t${cyc}z.cyclone.trackatcfunix"
                   "avnop.t${cyc}z.cyclone.trackatcfunix"
                   "trak.gfso.atcfunix.${PDY}${cyc}"
                   "trak.gfso.atcfunix.altg.${PDY}${cyc}")

    for file in "${cyclone_files[@]}"; do
      [[ -s ${COM_ATMOS_TRACK}/${file} ]] && echo "${COM_ATMOS_TRACK/${ROTDIR}\//}/${file}"
    done

    genesis_files=("storms.gfso.atcf_gen.${PDY}${cyc}"
                   "storms.gfso.atcf_gen.altg.${PDY}${cyc}")
    for file in "${genesis_files[@]}"; do
      [[ -s ${COM_ATMOS_GENESIS}/${file} ]] && echo "${COM_ATMOS_GENESIS/${ROTDIR}\//}/${file}"
    done

    # GSI Monitor job output

    if [[ ${DO_VMINMON} = "YES" ]]; then
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.costs.txt"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.cost_terms.txt"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.gnorms.ieee_d"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.reduction.ieee_d"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/gnorm_data.txt"
    fi

  } >> "${DATA}/gfsa.txt"

  {
    if [[ ${DO_DOWN} = "YES" ]]; then
      if [[ ${DO_BUFRSND} = "YES" ]]; then
        echo "${COM_ATMOS_GEMPAK/${ROTDIR}\//}/gfs_${PDY}${cyc}.sfc"
        echo "${COM_ATMOS_GEMPAK/${ROTDIR}\//}/gfs_${PDY}${cyc}.snd"
        echo "${COM_ATMOS_WMO/${ROTDIR}\//}/gfs_collective*.postsnd_${cyc}"
        echo "${COM_ATMOS_BUFR/${ROTDIR}\//}/bufr.t${cyc}z"
        echo "${COM_ATMOS_BUFR/${ROTDIR}\//}/gfs.t${cyc}z.bufrsnd.tar.gz"
      fi
    fi
  } >> "${DATA}/gfs_downstream.txt"

  {
    echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.anl"
    echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.anl.idx"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl.idx"
  } >> "${DATA}/gfsb.txt"


  fh=0
  while (( fh <= FHMAX_GFS )); do
    fhr=$(printf %03i "${fh}")
    if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
      {
        echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2"
        echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2.idx"
      } >> "${DATA}/gfs_flux.txt"

      {
        echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.f${fhr}"
        echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.f${fhr}.idx"
        if [[ -s "${COM_ATMOS_GRIB_1p00}/${head}pgrb2b.1p00.f${fhr}" ]]; then
           echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.f${fhr}"
           echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.f${fhr}.idx"
        fi
      } >> "${DATA}/gfs_pgrb2b.txt"
    fi

    {
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}.idx"
      echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atm.logf${fhr}.txt"
    } >> "${DATA}/gfsa.txt"


    {
      if [[ -s "${COM_ATMOS_GRIB_0p50}/${head}pgrb2.0p50.f${fhr}" ]]; then
         echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.f${fhr}"
         echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.f${fhr}.idx"
      fi
      if [[ -s "${COM_ATMOS_GRIB_1p00}/${head}pgrb2.1p00.f${fhr}" ]]; then
         echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}"
         echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}.idx"
      fi
    } >> "${DATA}/gfsb.txt"

    inc=${FHOUT_GFS}
    if (( FHMAX_HF_GFS > 0 && FHOUT_HF_GFS > 0 && fh < FHMAX_HF_GFS )); then
      inc=${FHOUT_HF_GFS}
    fi

    fh=$((fh+inc))
  done

  #..................
  {
    if [[ ${MODE} = "cycled" ]]; then
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile1.nc"
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile2.nc"
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile3.nc"
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile4.nc"
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile5.nc"
      echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile6.nc"
    elif [[ ${MODE} = "forecast-only" ]]; then
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_ctrl.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile1.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile2.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile3.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile4.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile5.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/gfs_data.tile6.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile1.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile2.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile3.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile4.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile5.nc"
      echo "${COM_ATMOS_INPUT/${ROTDIR}\//}/sfc_data.tile6.nc"
    fi
  } >> "${DATA}/gfs_restarta.txt"


  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf "${DATA}/gfswave.txt"
    touch "${DATA}/gfswave.txt"

    head="gfswave.t${cyc}z."

    #...........................
    {
      echo "${COM_WAVE_HISTORY/${ROTDIR}\//}/ww3_multi*"
      echo "${COM_WAVE_GRID/${ROTDIR}\//}/${head}*"
      echo "${COM_WAVE_STATION/${ROTDIR}\//}/${head}*"
    } >> "${DATA}/gfswave.txt"
  fi

  if [[ "${DO_OCN}" == "YES" ]]; then

    head="gfs.ocean.t${cyc}z."
    rm -f "${DATA}/ocean_6hravg.txt"; touch "${DATA}/ocean_6hravg.txt"
    rm -f "${DATA}/ocean_daily.txt"; touch "${DATA}/ocean_daily.txt"
    rm -f "${DATA}/ocean_grib2.txt"; touch "${DATA}/ocean_grib2.txt"

    echo "${COM_OCEAN_HISTORY/${ROTDIR}\//}/${head}6hr_avg.f*.nc" >> "${DATA}/ocean_6hravg.txt"
    echo "${COM_OCEAN_HISTORY/${ROTDIR}\//}/${head}daily.f*.nc" >> "${DATA}/ocean_daily.txt"

    {
      if [[ -d "${COM_OCEAN_GRIB}/5p00" ]]; then
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/5p00/${head}5p00.f*.grib2"
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/5p00/${head}5p00.f*.grib2.idx"
      fi
      if [[ -d "${COM_OCEAN_GRIB}/1p00" ]]; then
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/1p00/${head}1p00.f*.grib2"
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/1p00/${head}1p00.f*.grib2.idx"
      fi
      if [[ -d "${COM_OCEAN_GRIB}/0p25" ]]; then
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/0p25/${head}0p25.f*.grib2"
        echo "${COM_OCEAN_GRIB/${ROTDIR}\//}/0p25/${head}0p25.f*.grib2.idx"
      fi
    } >> "${DATA}/ocean_grib2.txt"

    # Also save fluxes from atmosphere
    head="gfs.t${cyc}z."
    rm -f "${DATA}/gfs_flux_1p00.txt"; touch "${DATA}/gfs_flux_1p00.txt"
    {
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}flux.1p00.f???"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}flux.1p00.f???.idx"
    } >> "${DATA}/gfs_flux_1p00.txt"
  fi

  if [[ "${DO_ICE}" == "YES" ]]; then
    head="gfs.ice.t${cyc}z."
    rm -f "${DATA}/ice_6hravg.txt"; touch "${DATA}/ice_6hravg.txt"
    rm -f "${DATA}/ice_grib2.txt"; touch "${DATA}/ice_grib2.txt"

    {
      echo "${COM_ICE_HISTORY/${ROTDIR}\//}/${head}ic.nc"
      echo "${COM_ICE_HISTORY/${ROTDIR}\//}/${head}6hr_avg.f*.nc"
    } >> "${DATA}/ice_6hravg.txt"

    {
      if [[ -d "${COM_ICE_GRIB}/5p00" ]]; then
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/5p00/${head}5p00.f*.grib2"
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/5p00/${head}5p00.f*.grib2.idx"
      fi
      if [[ -d "${COM_ICE_GRIB}/1p00" ]]; then
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/1p00/${head}1p00.f*.grib2"
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/1p00/${head}1p00.f*.grib2.idx"
      fi
      if [[ -d "${COM_ICE_GRIB}/0p25" ]]; then
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/0p25/${head}0p25.f*.grib2"
        echo "${COM_ICE_GRIB/${ROTDIR}\//}/0p25/${head}0p25.f*.grib2.idx"
      fi
    } >> "${DATA}/ice_grib2.txt"
  fi

  if [[ ${DO_AERO} = "YES" ]]; then
    head="gocart"

    rm -f "${DATA}/chem.txt"
    touch "${DATA}/chem.txt"

    echo "${COM_CHEM_HISTORY/${ROTDIR}\//}/${head}*" >> "${DATA}/chem.txt"
  fi

#-----------------------------------------------------
fi   ##end of gfs
#-----------------------------------------------------



#-----------------------------------------------------
if [[ ${type} == "gdas" ]]; then
#-----------------------------------------------------

  rm -f "${DATA}/gdas.txt"
  rm -f "${DATA}/gdas_restarta.txt"
  rm -f "${DATA}/gdas_restartb.txt"
  touch "${DATA}/gdas.txt"
  touch "${DATA}/gdas_restarta.txt"
  touch "${DATA}/gdas_restartb.txt"

  head="gdas.t${cyc}z."

  #..................
  {
    echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.anl"
    echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.anl.idx"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl.idx"
    echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmanl.nc"
    echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}sfcanl.nc"
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atmvar.yaml" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmvar.yaml"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atmstat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmstat"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}gsistat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}gsistat"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atmanl.ensres.nc" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmanl.ensres.nc"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atma003.ensres.nc" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atma003.ensres.nc"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}atma009.ensres.nc" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atma009.ensres.nc"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}cnvstat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}cnvstat"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}oznstat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}oznstat"
    fi
    if [[ ${DO_AERO} = "YES" ]]; then
       if [[ -s "${COM_CHEM_ANALYSIS}/${head}aerostat" ]]; then
          echo "${COM_CHEM_ANALYSIS/${ROTDIR}\//}/${head}aerostat"
       fi
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}radstat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}radstat"
    fi
    for fstep in prep anal fcst verfozn verfrad vminmon; do
      if [[ -s "${ROTDIR}/logs/${PDY}${cyc}/gdas${fstep}.log" ]]; then
        echo "./logs/${PDY}${cyc}/gdas${fstep}.log"
      fi
    done
    echo "./logs/${PDY}${cyc}/gdas*prod*.log"
    if [[ "${WRITE_DOPOST}" == ".false." ]]; then
       echo "./logs/${PDY}${cyc}/gdas*upp*.log"
    fi

    fh=0
    while [[ ${fh} -le 9 ]]; do
      fhr=$(printf %03i "${fh}")
      echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2"
      echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2.idx"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}.idx"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}.idx"
      echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atm.logf${fhr}.txt"
      echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atmf${fhr}.nc"
      echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}sfcf${fhr}.nc"
      fh=$((fh+3))
    done
    flist="001 002 004 005 007 008"
    for fhr in ${flist}; do
      file="${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2"
      if [[ -s "${file}" ]]; then
        echo "${file}"
        echo "${file}.idx"
      fi
    done

    # GSI Monitor jobs output

    if [[ ${DO_VERFOZN} = "YES" ]]; then
      for type in horiz time; do
        if [[ ${type} = "horiz" ]]; then
          suffix=".gz"
        elif [[ ${type} = "time" ]]; then
          suffix=""
          echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/bad_cnt.${PDY}${cyc}"
          echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/bad_diag.${PDY}${cyc}"
          echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/bad_pen.${PDY}${cyc}"
        fi
        subtyplist="gome_metop-b omi_aura ompslp_npp ompsnp_n20 ompsnp_npp ompstc8_n20 ompstc8_npp sbuv2_n19"
        for subtype in ${subtyplist}; do
          # On occassion, data is not available for some of these satellites.  Check for existence.
          if [[ -s "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/${subtype}.ges.${PDY}${cyc}.ieee_d${suffix}" ]]; then
             echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/${subtype}.anl.${PDY}${cyc}.ieee_d${suffix}"
             echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/${subtype}.anl.ctl"
             echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/${subtype}.ges.${PDY}${cyc}.ieee_d${suffix}"
             echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/${subtype}.ges.ctl"
          fi
        done
        echo "${COM_ATMOS_OZNMON/${ROTDIR}\//}/${type}/stdout.${type}.tar.gz"
      done
    fi

    if [[ ${DO_VERFRAD} = "YES" ]]; then
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/bad_diag.${PDY}${cyc}"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/bad_pen.${PDY}${cyc}"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/low_count.${PDY}${cyc}"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/radmon_angle.tar.gz"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/radmon_bcoef.tar.gz"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/radmon_bcor.tar.gz"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/radmon_time.tar.gz"
      echo "${COM_ATMOS_RADMON/${ROTDIR}\//}/warning.${PDY}${cyc}"
    fi

    if [[ ${DO_VMINMON} = "YES" ]]; then
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.costs.txt"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.cost_terms.txt"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.gnorms.ieee_d"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/${PDY}${cyc}.reduction.ieee_d"
      echo "${COM_ATMOS_MINMON/${ROTDIR}\//}/gnorm_data.txt"
    fi

  } >> "${DATA}/gdas.txt"

  #..................
  if [[ -s "${COM_ATMOS_ANALYSIS}/${head}cnvstat" ]]; then
     echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}cnvstat" >> "${DATA}/gdas_restarta.txt"
  fi
  if [[ -s "${COM_ATMOS_ANALYSIS}/${head}radstat" ]]; then
     echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}radstat" >> "${DATA}/gdas_restarta.txt"
  fi

  {
    gsiob_files=("nsstbufr"
                 "prepbufr"
                 "prepbufr.acft_profiles")
    for file in "${gsiob_files[@]}"; do
      [[ -s ${COM_OBS}/${head}${file} ]] && echo "${COM_OBS/${ROTDIR}\//}/${head}${file}"
    done

    gsida_files=("abias"
                 "abias_air"
                 "abias_int"
                 "abias_pc"
                 "dtfanl.nc"
                 "loginc.txt")
    for file in "${gsida_files[@]}"; do
      [[ -s ${COM_ATMOS_ANALYSIS}/${head}${file} ]] && echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}${file}"
    done

    ufsda_files=("amsua_n19.satbias.nc4"
                 "amsua_n19.satbias_cov.nc4"
                 "amsua_n19.tlapse.txt")
    for file in "${ufsda_files[@]}"; do
      [[ -s ${COM_ATMOS_ANALYSIS}/${head}${file} ]] && echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}${file}"
    done

    echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}atmi*nc"

    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile1.nc"
    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile2.nc"
    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile3.nc"
    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile4.nc"
    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile5.nc"
    echo "${COM_ATMOS_RESTART/${ROTDIR}\//}/*0000.sfcanl_data.tile6.nc"
  } >> "${DATA}/gdas_restarta.txt"

  #..................
  echo "${COM_ATMOS_RESTART/${ROTDIR}\//}" >> "${DATA}/gdas_restartb.txt"

  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf "${DATA}/gdaswave.txt"
    touch "${DATA}/gdaswave.txt"
    rm -rf "${DATA}/gdaswave_restart.txt"
    touch "${DATA}/gdaswave_restart.txt"

    head="gdaswave.t${cyc}z."

    #...........................
    {
      echo "${COM_WAVE_GRID/${ROTDIR}\//}/${head}*"
      echo "${COM_WAVE_STATION/${ROTDIR}\//}/${head}*"
    } >> "${DATA}/gdaswave.txt"

    echo "${COM_WAVE_RESTART/${ROTDIR}\//}/*" >> "${DATA}/gdaswave_restart.txt"

  fi

  #..................
  if [[ ${DO_OCN} = "YES" ]]; then

    rm -rf "${DATA}/gdasocean.txt"
    touch "${DATA}/gdasocean.txt"
    rm -rf "${DATA}/gdasocean_restart.txt"
    touch "${DATA}/gdasocean_restart.txt"

    head="gdas.t${cyc}z."

    #...........................
    {
      echo "${COM_OCEAN_HISTORY/${ROTDIR}\//}/${head}*"
      echo "${COM_OCEAN_INPUT/${ROTDIR}\//}"
    } >> "${DATA}/gdasocean.txt"

    {
      echo "${COM_OCEAN_RESTART/${ROTDIR}\//}/*"
      echo "${COM_MED_RESTART/${ROTDIR}\//}/*"
    } >> "${DATA}/gdasocean_restart.txt"

    {
      echo "${COM_OCEAN_ANALYSIS/${ROTDIR}\//}/${head}*"
      echo "${COM_OCEAN_ANALYSIS/${ROTDIR}\//}/gdas.t??z.ocngrid.nc"
      echo "${COM_OCEAN_ANALYSIS/${ROTDIR}\//}/diags"
      echo "${COM_OCEAN_ANALYSIS/${ROTDIR}\//}/yaml"
    } >> "${DATA}/gdasocean_analysis.txt"

  fi

  if [[ ${DO_ICE} = "YES" ]]; then

    rm -rf "${DATA}/gdasice.txt"
    touch "${DATA}/gdasice.txt"
    rm -rf "${DATA}/gdasice_restart.txt"
    touch "${DATA}/gdasice_restart.txt"

    head="gdas.t${cyc}z."

    #...........................
    {
      echo "${COM_ICE_HISTORY/${ROTDIR}\//}/${head}*"
      echo "${COM_ICE_INPUT/${ROTDIR}\//}/ice_in"
    } >> "${DATA}/gdasice.txt"

    echo "${COM_ICE_RESTART/${ROTDIR}\//}/*" >> "${DATA}/gdasice_restart.txt"

 fi


#-----------------------------------------------------
fi   ##end of gdas
#-----------------------------------------------------


#-----------------------------------------------------
if [[ ${type} == "enkfgdas" || ${type} == "enkfgfs" ]]; then
#-----------------------------------------------------

  IAUFHRS_ENKF=${IAUFHRS_ENKF:-6}
  lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}
  IFS=',' read -ra nfhrs <<< ${IAUFHRS_ENKF}
  NMEM_ENS=${NMEM_ENS:-80}
  NMEM_EARCGRP=${NMEM_EARCGRP:-10}               ##number of ens memebers included in each tarball
  NTARS=$((NMEM_ENS/NMEM_EARCGRP))
  [[ ${NTARS} -eq 0 ]] && NTARS=1
  [[ $((NTARS*NMEM_EARCGRP)) -lt ${NMEM_ENS} ]] && NTARS=$((NTARS+1))
  ##NTARS2=$((NTARS/2))  # number of earc groups to include analysis/increments
  NTARS2=${NTARS}

  head="${RUN}.t${cyc}z."

  #..................
  rm -f "${DATA}/${RUN}.txt"
  touch "${DATA}/${RUN}.txt"

  {
    gsida_files=("enkfstat"
                 "gsistat.ensmean"
                 "cnvstat.ensmean"
                 "oznstat.ensmean"
                 "radstat.ensmean")
    for file in "${gsida_files[@]}"; do
      [[ -s ${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}${file} ]] && echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}${file}"
    done

    ufsda_files=("atmens.yaml"
                 "atmensstat")
    for file in "${ufsda_files[@]}"; do
      [[ -s ${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}${file} ]] && echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}${file}"
    done

    for FHR in "${nfhrs[@]}"; do  # loop over analysis times in window
      if [[ ${FHR} -eq 6 ]]; then
        if [[ -s "${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}atmanl.ensmean.nc" ]]; then
          echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}atmanl.ensmean.nc"
        fi
        if [[ -s "${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}atminc.ensmean.nc" ]]; then
          echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}atminc.ensmean.nc"
        fi
      else
        if [[ -s "${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}atma00${FHR}.ensmean.nc" ]]; then
          echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}atma00${FHR}.ensmean.nc"
        fi
        if [[ -s "${COM_ATMOS_ANALYSIS_ENSSTAT}/${head}atmi00${FHR}.ensmean.nc" ]]; then
          echo "${COM_ATMOS_ANALYSIS_ENSSTAT/${ROTDIR}\//}/${head}atmi00${FHR}.ensmean.nc"
        fi
      fi
    done # loop over FHR
    for fstep in fcst epos ; do
      echo "logs/${PDY}${cyc}/${RUN}${fstep}*.log"
    done

  # eobs, ecen, esfc, and eupd are not run on the first cycle
    for fstep in eobs ecen esfc eupd ; do
       for log in "${ROTDIR}/logs/${PDY}${cyc}/${RUN}${fstep}"*".log"; do
          if [[ -s "${log}" ]]; then
             echo "logs/${PDY}${cyc}/${RUN}${fstep}*.log"
          fi
       done
    done

  # eomg* are optional jobs
    for log in "${ROTDIR}/logs/${PDY}${cyc}/${RUN}eomg"*".log"; do
       if [[ -s "${log}" ]]; then
          echo "logs/${PDY}${cyc}/${RUN}eomg*.log"
       fi
       break
    done

  # Ensemble spread file only available with netcdf output
    fh=3
    while [ $fh -le 9 ]; do
        fhr=$(printf %03i $fh)
        echo "${COM_ATMOS_HISTORY_ENSSTAT/${ROTDIR}\//}/${head}atmf${fhr}.ensmean.nc"
        echo "${COM_ATMOS_HISTORY_ENSSTAT/${ROTDIR}\//}/${head}sfcf${fhr}.ensmean.nc"
        if [[ -s "${COM_ATMOS_HISTORY_ENSSTAT}/${head}atmf${fhr}.ensspread.nc" ]]; then
            echo "${COM_ATMOS_HISTORY_ENSSTAT/${ROTDIR}\//}/${head}atmf${fhr}.ensspread.nc"
        fi
        fh=$((fh+3))
    done
  } >> "${DATA}/${RUN}.txt"

  #...........................
  n=1
  while (( n <= NTARS )); do
    #...........................

    rm -f "${DATA}/${RUN}_grp${n}.txt"
    rm -f "${DATA}/${RUN}_restarta_grp${n}.txt"
    rm -f "${DATA}/${RUN}_restartb_grp${n}.txt"
    touch "${DATA}/${RUN}_grp${n}.txt"
    touch "${DATA}/${RUN}_restarta_grp${n}.txt"
    touch "${DATA}/${RUN}_restartb_grp${n}.txt"

    m=1
    while (( m <= NMEM_EARCGRP && (n-1)*NMEM_EARCGRP+m <= NMEM_ENS )); do
      nm=$(((n-1)*NMEM_EARCGRP+m))
      mem=$(printf %03i ${nm})
      head="${RUN}.t${cyc}z."

      MEMDIR="mem${mem}" YMD=${PDY} HH=${cyc} generate_com \
        COM_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL \
        COM_ATMOS_RESTART_MEM:COM_ATMOS_RESTART_TMPL \
        COM_ATMOS_HISTORY_MEM:COM_ATMOS_HISTORY_TMPL

      #---
      for FHR in "${nfhrs[@]}"; do  # loop over analysis times in window
        if [ "${FHR}" -eq 6 ]; then
          {
            if (( n <= NTARS2 )); then
              if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}atmanl.nc" ]] ; then
                echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}atmanl.nc"
              fi
                if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratminc.nc" ]] ; then
                    echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratminc.nc"
                fi
            fi
          } >> "${DATA}/${RUN}_grp${n}.txt"

          if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratminc.nc" ]] ; then
            echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratminc.nc" \
              >> "${DATA}/${RUN}_restarta_grp${n}.txt"
          fi

        else
          {
            if (( n <= NTARS2 )); then
              if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}atma00${FHR}.nc" ]] ; then
                echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}atma00${FHR}.nc"
              fi
              if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratmi00${FHR}.nc" ]] ; then
                echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratmi00${FHR}.nc"
              fi
             fi
          } >> "${DATA}/${RUN}_grp${n}.txt"
          if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratmi00${FHR}.nc" ]] ; then
            echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratmi00${FHR}.nc" \
              >> "${DATA}/${RUN}_restarta_grp${n}.txt"
          fi
        fi
        {
          echo "${COM_ATMOS_HISTORY_MEM/${ROTDIR}\//}/${head}atmf00${FHR}.nc"
          if (( FHR == 6 )); then
            echo "${COM_ATMOS_HISTORY_MEM/${ROTDIR}\//}/${head}sfcf00${FHR}.nc"
          fi
        } >> "${DATA}/${RUN}_grp${n}.txt"
      done # loop over FHR

      if [[ ${lobsdiag_forenkf} == ".false." ]] ; then
        {
          echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}gsistat"
          if [[ -s "${COM_ATMOS_RESTART_MEM}/${head}cnvstat" ]] ; then
            echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}cnvstat"
          fi
        } >> "${DATA}/${RUN}_grp${n}.txt"

        {
           if [[ -s "${COM_ATMOS_RESTART_MEM}/${head}radstat" ]]; then
              echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}radstat"
           fi
           if [[ -s "${COM_ATMOS_RESTART_MEM}/${head}cnvstat" ]]; then
              echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}cnvstat"
           fi
           echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}abias"
           echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}abias_air"
           echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}abias_int"
           echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}abias_pc"
        } >> "${DATA}/${RUN}_restarta_grp${n}.txt"
      fi
      #---
      {
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile1.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile2.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile3.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile4.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile5.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile6.nc"
      } >> "${DATA}/${RUN}_restarta_grp${n}.txt"
      #---
      echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}" >> "${DATA}/${RUN}_restartb_grp${n}.txt"

      m=$((m+1))
    done


  #...........................
  n=$((n+1))
  done
  #...........................


#-----------------------------------------------------
fi   ##end of enkfgdas or enkfgfs
#-----------------------------------------------------

exit 0
