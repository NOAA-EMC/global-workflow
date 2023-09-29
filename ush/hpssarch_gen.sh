#! /usr/bin/env bash

###################################################
# Fanglin Yang, 20180318
# --create bunches of files to be archived to HPSS
###################################################
source "${HOMEgfs}/ush/preamble.sh"

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

  rm -f gfsa.txt
  rm -f gfsb.txt
  rm -f gfs_restarta.txt
  touch gfsa.txt
  touch gfsb.txt
  touch gfs_restarta.txt

  if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
    rm -f gfs_pgrb2b.txt
    rm -f gfs_netcdfb.txt
    rm -f gfs_flux.txt
    touch gfs_pgrb2b.txt
    touch gfs_netcdfb.txt
    touch gfs_flux.txt

    if [[ ${MODE} = "cycled" ]]; then
      rm -f gfs_netcdfa.txt
      touch gfs_netcdfa.txt
    fi
  fi

  if [[ ${DO_DOWN} = "YES" ]]; then
    rm -f gfs_downstream.txt
    touch gfs_downstream.txt
  fi

  head="gfs.t${cyc}z."

  if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
    {
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.anl"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.anl.idx"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.anl"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2b.1p00.anl.idx"
    } >> gfs_pgrb2b.txt

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
      } >> gfs_netcdfa.txt
    fi

    fh=0
    while (( fh <= ARCH_GAUSSIAN_FHMAX )); do
      fhr=$(printf %03i "${fh}")
      {
        echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atmf${fhr}.nc"
        echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}sfcf${fhr}.nc"
      } >> gfs_netcdfb.txt
      fh=$((fh+ARCH_GAUSSIAN_FHINC))
    done
  fi

  #..................
  # Exclude the gfsarch.log file, which will change during the tar operation
  #  This uses the bash extended globbing option
  {
    echo "./logs/${PDY}${cyc}/gfs!(arch).log"
    echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/input.nml"

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
  } >> gfsa.txt

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
  } >> gfs_downstream.txt

  {
    echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.anl"
    echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.anl.idx"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl"
    echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.anl.idx"
  } >> gfsb.txt


  fh=0
  while (( fh <= FHMAX_GFS )); do
    fhr=$(printf %03i "${fh}")
    if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
      {
        echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2"
        echo "${COM_ATMOS_MASTER/${ROTDIR}\//}/${head}sfluxgrbf${fhr}.grib2.idx"
      } >> gfs_flux.txt

      {
        echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.f${fhr}"
        echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2b.0p25.f${fhr}.idx"
        if [[ -s "${COM_ATMOS_GRIB_1p00}/${head}pgrb2b.1p00.f${fhr}" ]]; then
           echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/{head}pgrb2b.1p00.f${fhr}"
           echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/{head}pgrb2b.1p00.f${fhr}.idx"
        fi
      } >> gfs_pgrb2b.txt
    fi

    {
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}"
      echo "${COM_ATMOS_GRIB_0p25/${ROTDIR}\//}/${head}pgrb2.0p25.f${fhr}.idx"
      echo "${COM_ATMOS_HISTORY/${ROTDIR}\//}/${head}atm.logf${fhr}.txt"
    } >> gfsa.txt


    {
      if [[ -s "${COM_ATMOS_GRIB_0p50}/${head}pgrb2.0p50.f${fhr}" ]]; then
         echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.f${fhr}"
         echo "${COM_ATMOS_GRIB_0p50/${ROTDIR}\//}/${head}pgrb2.0p50.f${fhr}.idx"
      fi
      if [[ -s "${COM_ATMOS_GRIB_1p00}/${head}pgrb2.1p00.f${fhr}" ]]; then
         echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}"
         echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}pgrb2.1p00.f${fhr}.idx"
      fi
    } >> gfsb.txt

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
  } >> gfs_restarta.txt


  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf gfswave.txt
    touch gfswave.txt

    head="gfswave.t${cyc}z."

    #...........................
    {
      echo "${COM_WAVE_HISTORY/${ROTDIR}\//}/ww3_multi*"
      echo "${COM_WAVE_GRID/${ROTDIR}\//}/${head}*"
      echo "${COM_WAVE_STATION/${ROTDIR}\//}/${head}*"
    } >> gfswave.txt
  fi

  if [[ ${DO_OCN} = "YES" ]]; then

    head="gfs.t${cyc}z."

    rm -f gfs_flux_1p00.txt
    rm -f ocn_ice_grib2_0p5.txt
    rm -f ocn_ice_grib2_0p25.txt
    rm -f ocn_2D.txt
    rm -f ocn_3D.txt
    rm -f ocn_xsect.txt
    rm -f ocn_daily.txt
    touch gfs_flux_1p00.txt
    touch ocn_ice_grib2_0p5.txt
    touch ocn_ice_grib2_0p25.txt
    touch ocn_2D.txt
    touch ocn_3D.txt
    touch ocn_xsect.txt
    touch ocn_daily.txt
    echo "${COM_OCEAN_INPUT/${ROTDIR}\//}/MOM_input" >> ocn_2D.txt
    echo "${COM_OCEAN_2D/${ROTDIR}\//}/ocn_2D*" >> ocn_2D.txt
    echo "${COM_OCEAN_3D/${ROTDIR}\//}/ocn_3D*" >> ocn_3D.txt
    echo "${COM_OCEAN_XSECT/${ROTDIR}\//}/ocn*EQ*" >> ocn_xsect.txt
    echo "${COM_OCEAN_HISTORY/${ROTDIR}\//}/ocn_daily*" >> ocn_daily.txt
    echo "${COM_OCEAN_GRIB_0p50/${ROTDIR}\//}/ocn_ice*0p5x0p5.grb2" >> ocn_ice_grib2_0p5.txt
    echo "${COM_OCEAN_GRIB_0p25/${ROTDIR}\//}/ocn_ice*0p25x0p25.grb2" >> ocn_ice_grib2_0p25.txt

    # Also save fluxes from atmosphere
    {
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}flux.1p00.f???"
      echo "${COM_ATMOS_GRIB_1p00/${ROTDIR}\//}/${head}flux.1p00.f???.idx"
    } >> gfs_flux_1p00.txt
  fi

  if [[ ${DO_ICE} = "YES" ]]; then
    head="gfs.t${cyc}z."

    rm -f ice.txt
    touch ice.txt
    {
      echo "${COM_ICE_INPUT/${ROTDIR}\//}/ice_in"
      echo "${COM_ICE_HISTORY/${ROTDIR}\//}/ice*nc"
    } >> ice.txt
  fi

  if [[ ${DO_AERO} = "YES" ]]; then
    head="gocart"

    rm -f chem.txt
    touch chem.txt

    echo "${COM_CHEM_HISTORY/${ROTDIR}\//}/${head}*" >> chem.txt
  fi

#-----------------------------------------------------
fi   ##end of gfs
#-----------------------------------------------------



#-----------------------------------------------------
if [[ ${type} == "gdas" ]]; then
#-----------------------------------------------------

  rm -f gdas.txt
  rm -f gdas_restarta.txt
  rm -f gdas_restartb.txt
  touch gdas.txt
  touch gdas_restarta.txt
  touch gdas_restartb.txt

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
    if [[ -s "${COM_CHEM_ANALYSIS}/${head}aerostat" ]]; then
       echo "${COM_CHEM_ANALYSIS/${ROTDIR}\//}/${head}aerostat"
    fi
    if [[ -s "${COM_ATMOS_ANALYSIS}/${head}radstat" ]]; then
       echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}radstat"
    fi
    for fstep in prep anal fcst vrfy radmon minmon oznmon; do
      if [[ -s "${ROTDIR}/logs/${PDY}${cyc}/gdas${fstep}.log" ]]; then
        echo "./logs/${PDY}${cyc}/gdas${fstep}.log"
      fi
    done
    echo "./logs/${PDY}${cyc}/gdaspost*.log"

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
  } >> gdas.txt

  #..................
  if [[ -s "${COM_ATMOS_ANALYSIS}/${head}cnvstat" ]]; then
     echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}cnvstat" >> gdas_restarta.txt
  fi
  if [[ -s "${COM_ATMOS_ANALYSIS}/${head}radstat" ]]; then
     echo "${COM_ATMOS_ANALYSIS/${ROTDIR}\//}/${head}radstat" >> gdas_restarta.txt
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
  } >> gdas_restarta.txt

  #..................
  echo "${COM_ATMOS_RESTART/${ROTDIR}\//}" >> gdas_restartb.txt

  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf gdaswave.txt
    touch gdaswave.txt
    rm -rf gdaswave_restart.txt
    touch gdaswave_restart.txt

    head="gdaswave.t${cyc}z."

    #...........................
    {
      echo "${COM_WAVE_GRID/${ROTDIR}\//}/${head}*"
      echo "${COM_WAVE_STATION/${ROTDIR}\//}/${head}*"
    } >> gdaswave.txt

    echo "${COM_WAVE_RESTART/${ROTDIR}\//}/*" >> gdaswave_restart.txt

  fi

  #..................
  if [[ ${DO_OCN} = "YES" ]]; then

    rm -rf gdasocean.txt
    touch gdasocean.txt
    rm -rf gdasocean_restart.txt
    touch gdasocean_restart.txt

    head="gdas.t${cyc}z."

    #...........................
    {
      echo "${COM_OCEAN_HISTORY/${ROTDIR}\//}/${head}*"
      echo "${COM_OCEAN_INPUT/${ROTDIR}\//}"
    } >> gdasocean.txt

    {
      echo "${COM_OCEAN_RESTART/${ROTDIR}\//}/*"
      echo "${COM_MED_RESTART/${ROTDIR}\//}/*"
    } >> gdasocean_restart.txt

  fi

  if [[ ${DO_ICE} = "YES" ]]; then

    rm -rf gdasice.txt
    touch gdasice.txt
    rm -rf gdasice_restart.txt
    touch gdasice_restart.txt

    head="gdas.t${cyc}z."

    #...........................
    {
      echo "${COM_ICE_HISTORY/${ROTDIR}\//}/${head}*"
      echo "${COM_ICE_INPUT/${ROTDIR}\//}/ice_in"
    } >> gdasice.txt

    echo "${COM_ICE_RESTART/${ROTDIR}\//}/*" >> gdasice_restart.txt

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
  rm -f "${RUN}.txt"
  touch "${RUN}.txt"

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
    for fstep in eobs ecen esfc eupd efcs epos ; do
     echo "logs/${PDY}${cyc}/${RUN}${fstep}*.log"
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
  } >> "${RUN}.txt"

  #...........................
  n=1
  while (( n <= NTARS )); do
    #...........................

    rm -f "${RUN}_grp${n}.txt"
    rm -f "${RUN}_restarta_grp${n}.txt"
    rm -f "${RUN}_restartb_grp${n}.txt"
    touch "${RUN}_grp${n}.txt"
    touch "${RUN}_restarta_grp${n}.txt"
    touch "${RUN}_restartb_grp${n}.txt"

    m=1
    while (( m <= NMEM_EARCGRP )); do
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
          } >> "${RUN}_grp${n}.txt"

          if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratminc.nc" ]] ; then
            echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratminc.nc" \
              >> "${RUN}_restarta_grp${n}.txt"
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
          } >> "${RUN}_grp${n}.txt"
          if [[ -s "${COM_ATMOS_ANALYSIS_MEM}/${head}ratmi00${FHR}.nc" ]] ; then
            echo "${COM_ATMOS_ANALYSIS_MEM/${ROTDIR}\//}/${head}ratmi00${FHR}.nc" \
              >> "${RUN}_restarta_grp${n}.txt"
          fi
        fi
        {
          echo "${COM_ATMOS_HISTORY_MEM/${ROTDIR}\//}/${head}atmf00${FHR}.nc"
          if (( FHR == 6 )); then
            echo "${COM_ATMOS_HISTORY_MEM/${ROTDIR}\//}/${head}sfcf00${FHR}.nc"
          fi
        } >> "${RUN}_grp${n}.txt"
      done # loop over FHR

      if [[ ${lobsdiag_forenkf} == ".false." ]] ; then
        {
          echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}gsistat"
          if [[ -s "${COM_ATMOS_RESTART_MEM}/${head}cnvstat" ]] ; then
            echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/${head}cnvstat"
          fi
        } >> "${RUN}_grp${n}.txt"

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
        } >> "${RUN}_restarta_grp${n}.txt"
      fi
      #---
      {
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile1.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile2.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile3.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile4.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile5.nc"
        echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}/*0000.sfcanl_data.tile6.nc"
      } >> "${RUN}_restarta_grp${n}.txt"
      #---
      echo "${COM_ATMOS_RESTART_MEM/${ROTDIR}\//}" >> "${RUN}_restartb_grp${n}.txt"

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

