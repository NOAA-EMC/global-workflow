#! /usr/bin/env bash

###################################################
# Fanglin Yang, 20180318
# --create bunches of files to be archived to HPSS
###################################################
source "${HOMEgfs}/ush/preamble.sh"

type=${1:-gfs}                ##gfs, gdas, enkfgdas or enkfggfs

CDATE=${CDATE:-2018010100}
PDY=$(echo $CDATE | cut -c 1-8)
cyc=$(echo $CDATE | cut -c 9-10)
ARCH_GAUSSIAN=${ARCH_GAUSSIAN:-"YES"}
ARCH_GAUSSIAN_FHMAX=${ARCH_GAUSSIAN_FHMAX:-36}
ARCH_GAUSSIAN_FHINC=${ARCH_GAUSSIAN_FHINC:-6}

# Set whether to archive downstream products
DO_DOWN=${DO_DOWN:-"NO"}
if [ ${DO_BUFRSND} = "YES" -o ${WAFSF} = "YES" ]; then
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

    if [ $MODE = "cycled" ]; then
      rm -f gfs_netcdfa.txt
      touch gfs_netcdfa.txt
    fi
  fi

  if [[ ${DO_DOWN} = "YES" ]]; then
    rm -f gfs_downstream.txt
    touch gfs_downstream.txt
  fi

  dirpath="gfs.${PDY}/${cyc}/atmos/"
  dirname="./${dirpath}"
  obs_dirname="./gfs.${PDY}/${cyc}/obs/"

  head="gfs.t${cyc}z."

  if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
    echo  "${dirname}${head}pgrb2b.0p25.anl                  " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.0p25.anl.idx              " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.1p00.anl                  " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.1p00.anl.idx              " >>gfs_pgrb2b.txt

    if [ $MODE = "cycled" ]; then
      echo  "${dirname}${head}atmanl.nc            " >>gfs_netcdfa.txt
      echo  "${dirname}${head}sfcanl.nc            " >>gfs_netcdfa.txt
      echo  "${dirname}${head}atmi*.nc                   " >>gfs_netcdfa.txt
      echo  "${dirname}${head}dtfanl.nc                  " >>gfs_netcdfa.txt
      echo  "${dirname}${head}loginc.txt                 " >>gfs_netcdfa.txt
    fi

    fh=0
    while [ $fh -le $ARCH_GAUSSIAN_FHMAX ]; do
      fhr=$(printf %03i $fh)
      echo  "${dirname}${head}atmf${fhr}.nc        " >>gfs_netcdfb.txt
      echo  "${dirname}${head}sfcf${fhr}.nc        " >>gfs_netcdfb.txt
      fh=$((fh+ARCH_GAUSSIAN_FHINC))
    done
  fi

  #..................
  # Exclude the gfsarch.log file, which will change during the tar operation
  #  This uses the bash extended globbing option
  echo  "./logs/${CDATE}/gfs!(arch).log                    " >>gfsa.txt
  echo  "${dirname}input.nml                               " >>gfsa.txt
  if [[ ${MODE} = "cycled" ]]; then
    echo  "${dirname}${head}gsistat                          " >>gfsa.txt
    echo  "${obs_dirname}${head}nsstbufr                         " >>gfsa.txt
    echo  "${obs_dirname}${head}prepbufr                         " >>gfsa.txt
    echo  "${obs_dirname}${head}prepbufr.acft_profiles           " >>gfsa.txt
  fi
  echo  "${dirname}${head}pgrb2.0p25.anl                   " >>gfsa.txt
  echo  "${dirname}${head}pgrb2.0p25.anl.idx               " >>gfsa.txt
  #Only generated if there are cyclones to track
  cyclone_files=(avno.t${cyc}z.cyclone.trackatcfunix
                 avnop.t${cyc}z.cyclone.trackatcfunix
                 trak.gfso.atcfunix.${PDY}${cyc}
                 trak.gfso.atcfunix.altg.${PDY}${cyc}
                 storms.gfso.atcf_gen.${PDY}${cyc}
                 storms.gfso.atcf_gen.altg.${PDY}${cyc})

  for file in ${cyclone_files[@]}; do
    [[ -s ${ROTDIR}/${dirname}${file} ]] && echo "${dirname}${file}" >>gfsa.txt
  done

  if [[ ${DO_DOWN} = "YES" ]]; then
   if [[ ${DO_BUFRSND} = "YES" ]]; then
    echo  "${dirname}gempak/gfs_${PDY}${cyc}.sfc              " >>gfs_downstream.txt
    echo  "${dirname}gempak/gfs_${PDY}${cyc}.snd              " >>gfs_downstream.txt
    echo  "${dirname}wmo/gfs_collective*.postsnd_${cyc}       " >>gfs_downstream.txt
    echo  "${dirname}bufr.t${cyc}z                            " >>gfs_downstream.txt
    echo  "${dirname}gfs.t${cyc}z.bufrsnd.tar.gz              " >>gfs_downstream.txt
   fi
   if [[ ${WAFSF} = "YES" ]]; then
    echo  "${dirname}wafsgfs*.t${cyc}z.gribf*.grib2           " >>gfs_downstream.txt
    echo  "${dirname}gfs.t${cyc}z.wafs_grb45f*.grib2          " >>gfs_downstream.txt
    echo  "${dirname}gfs.t${cyc}z.wafs_grb45f*.nouswafs.grib2 " >>gfs_downstream.txt
    echo  "${dirname}WAFS_blended_${PDY}${cyc}f*.grib2        " >>gfs_downstream.txt
    echo  "${dirname}gfs.t*z.gcip.f*.grib2                    " >>gfs_downstream.txt
    echo  "${dirname}gfs.t${cyc}z.wafs_0p25.f*.grib2          " >>gfs_downstream.txt
    echo  "${dirname}gfs.t${cyc}z.wafs_0p25_unblended.f*.grib2" >>gfs_downstream.txt
    echo  "${dirname}WAFS_0p25_blended_${PDY}${cyc}f*.grib2   " >>gfs_downstream.txt
   fi
  fi

  echo  "${dirname}${head}pgrb2.0p50.anl                   " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.0p50.anl.idx               " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.1p00.anl                   " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.1p00.anl.idx               " >>gfsb.txt


  fh=0
  while [[ ${fh} -le ${FHMAX_GFS} ]]; do
    fhr=$(printf %03i ${fh})
    if [[ ${ARCH_GAUSSIAN} = "YES" ]]; then
      echo  "${dirname}${head}sfluxgrbf${fhr}.grib2           " >>gfs_flux.txt
      echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx       " >>gfs_flux.txt

      echo  "${dirname}${head}pgrb2b.0p25.f${fhr}             " >>gfs_pgrb2b.txt
      echo  "${dirname}${head}pgrb2b.0p25.f${fhr}.idx         " >>gfs_pgrb2b.txt
      if [[ -s ${ROTDIR}/${dirpath}${head}pgrb2b.1p00.f${fhr} ]]; then
         echo  "${dirname}${head}pgrb2b.1p00.f${fhr}         " >>gfs_pgrb2b.txt
         echo  "${dirname}${head}pgrb2b.1p00.f${fhr}.idx     " >>gfs_pgrb2b.txt
      fi
    fi

    echo  "${dirname}${head}pgrb2.0p25.f${fhr}              " >>gfsa.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}.idx          " >>gfsa.txt
    echo  "${dirname}${head}logf${fhr}.txt                  " >>gfsa.txt

    if [[ -s ${ROTDIR}/${dirpath}${head}pgrb2.0p50.f${fhr} ]]; then
       echo  "${dirname}${head}pgrb2.0p50.f${fhr}          " >>gfsb.txt
       echo  "${dirname}${head}pgrb2.0p50.f${fhr}.idx      " >>gfsb.txt
    fi
    if [[ -s ${ROTDIR}/${dirpath}${head}pgrb2.1p00.f${fhr} ]]; then
       echo  "${dirname}${head}pgrb2.1p00.f${fhr}          " >>gfsb.txt
       echo  "${dirname}${head}pgrb2.1p00.f${fhr}.idx      " >>gfsb.txt
    fi

    inc=${FHOUT_GFS}
    if [ ${FHMAX_HF_GFS} -gt 0 -a ${FHOUT_HF_GFS} -gt 0 -a ${fh} -lt ${FHMAX_HF_GFS} ]; then
     inc=${FHOUT_HF_GFS}
    fi

    fh=$((fh+inc))
  done

  #..................
  if [[ ${MODE} = "cycled" ]]; then
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>gfs_restarta.txt
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>gfs_restarta.txt
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>gfs_restarta.txt
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>gfs_restarta.txt
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>gfs_restarta.txt
    echo  "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>gfs_restarta.txt
  elif [[ ${MODE} = "forecast-only" ]]; then
    echo  "${dirname}INPUT/gfs_ctrl.nc        " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile1.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile2.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile3.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile4.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile5.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/gfs_data.tile6.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile1.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile2.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile3.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile4.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile5.nc  " >>gfs_restarta.txt
    echo  "${dirname}INPUT/sfc_data.tile6.nc  " >>gfs_restarta.txt
  fi

  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf gfswave.txt
    touch gfswave.txt

    dirpath="gfs.${PDY}/${cyc}/wave/"
    dirname="./${dirpath}"

    head="gfswave.t${cyc}z."

    #...........................
    echo  "${dirname}rundata/ww3_multi*   " >>gfswave.txt
    echo "${dirname}gridded/${head}*      " >>gfswave.txt
    echo "${dirname}station/${head}*      " >>gfswave.txt

  fi

  if [[ ${DO_OCN} = "YES" ]]; then
    dirpath="gfs.${PDY}/${cyc}/ocean/"
    dirname="./${dirpath}"

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
    echo  "${dirname}MOM_input                  " >>ocn_2D.txt
    echo  "${dirname}ocn_2D*                    " >>ocn_2D.txt
    echo  "${dirname}ocn_3D*                    " >>ocn_3D.txt
    echo  "${dirname}ocn*EQ*                    " >>ocn_xsect.txt
    echo  "${dirname}ocn_daily*                 " >>ocn_daily.txt
    echo  "${dirname}ocn_ice*0p5x0p5.grb2       " >>ocn_ice_grib2_0p5.txt
    echo  "${dirname}ocn_ice*0p25x0p25.grb2     " >>ocn_ice_grib2_0p25.txt

    dirpath="gfs.${PDY}/${cyc}/atmos/"
    dirname="./${dirpath}"
    echo  "${dirname}${head}flux.1p00.f???      " >>gfs_flux_1p00.txt
    echo  "${dirname}${head}flux.1p00.f???.idx  " >>gfs_flux_1p00.txt
  fi

  if [[ ${DO_ICE} = "YES" ]]; then
    dirpath="gfs.${PDY}/${cyc}/ice/"
    dirname="./${dirpath}"

    head="gfs.t${cyc}z."

    rm -f ice.txt
    touch ice.txt
    echo  "${dirname}ice_in                     " >>ice.txt
    echo  "${dirname}ice*nc                     " >>ice.txt
  fi

  if [[ ${DO_AERO} = "YES" ]]; then
    dirpath="gfs.${PDY}/${cyc}/chem"
    dirname="./${dirpath}"

    head="gocart"

    rm -f chem.txt
    touch chem.txt

    echo "${dirname}/${head}*" >> chem.txt
  fi

#-----------------------------------------------------
fi   ##end of gfs
#-----------------------------------------------------



#-----------------------------------------------------
if [[ ${type} = "gdas" ]]; then
#-----------------------------------------------------

  rm -f gdas.txt
  rm -f gdas_restarta.txt
  rm -f gdas_restartb.txt
  touch gdas.txt
  touch gdas_restarta.txt
  touch gdas_restartb.txt

  dirpath="gdas.${PDY}/${cyc}/atmos/"
  dirname="./${dirpath}"
  obs_dirname="./gdas.${PDY}/${cyc}/obs/"
  head="gdas.t${cyc}z."

  #..................
  echo  "${dirname}${head}gsistat                    " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}atmanl.nc            " >>gdas.txt
  echo  "${dirname}${head}sfcanl.nc            " >>gdas.txt
  if [ -s $ROTDIR/${dirpath}${head}atmanl.ensres.nc ]; then
     echo  "${dirname}${head}atmanl.ensres.nc  " >>gdas.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}atma003.ensres.nc ]; then
     echo  "${dirname}${head}atma003.ensres.nc  " >>gdas.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}atma009.ensres.nc ]; then
     echo  "${dirname}${head}atma009.ensres.nc  " >>gdas.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}cnvstat ]]; then
     echo  "${dirname}${head}cnvstat                 " >>gdas.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}oznstat ]]; then
     echo  "${dirname}${head}oznstat                 " >>gdas.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}radstat ]]; then
     echo  "${dirname}${head}radstat                 " >>gdas.txt
  fi
  for fstep in prep anal gldas fcst vrfy radmon minmon oznmon; do
   if [[ -s ${ROTDIR}/logs/${CDATE}/gdas${fstep}.log ]]; then
     echo  "./logs/${CDATE}/gdas${fstep}.log         " >>gdas.txt
   fi
  done
  echo  "./logs/${CDATE}/gdaspost*.log               " >>gdas.txt

  fh=0
  while [[ ${fh} -le 9 ]]; do
    fhr=$(printf %03i ${fh})
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2      " >>gdas.txt
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx  " >>gdas.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}         " >>gdas.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}.idx     " >>gdas.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}         " >>gdas.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}.idx     " >>gdas.txt
    echo  "${dirname}${head}logf${fhr}.txt             " >>gdas.txt
    echo  "${dirname}${head}atmf${fhr}.nc              " >>gdas.txt
    echo  "${dirname}${head}sfcf${fhr}.nc              " >>gdas.txt
    fh=$((fh+3))
  done
  flist="001 002 004 005 007 008"
  for fhr in ${flist}; do
    file="${dirname}${head}sfluxgrbf${fhr}.grib2"
    # Only add to list if file is present.
    if [[ -s "${file}" ]]; then
      echo  "${file}"      >>gdas.txt
      echo  "${file}.idx"  >>gdas.txt
    fi
  done
  


  #..................
  if [[ -s ${ROTDIR}/${dirpath}${head}cnvstat ]]; then
     echo  "${dirname}${head}cnvstat               " >>gdas_restarta.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}radstat ]]; then
     echo  "${dirname}${head}radstat               " >>gdas_restarta.txt
  fi
  echo  "${obs_dirname}${head}nsstbufr                 " >>gdas_restarta.txt
  echo  "${obs_dirname}${head}prepbufr                 " >>gdas_restarta.txt
  echo  "${obs_dirname}${head}prepbufr.acft_profiles   " >>gdas_restarta.txt
  echo  "${dirname}${head}abias                    " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_air                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_int                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_pc                 " >>gdas_restarta.txt
  echo  "${dirname}${head}atmi*nc                  " >>gdas_restarta.txt
  echo  "${dirname}${head}dtfanl.nc                " >>gdas_restarta.txt
  echo  "${dirname}${head}loginc.txt               " >>gdas_restarta.txt

  echo  "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>gdas_restarta.txt


  #..................
  echo  "${dirname}RESTART " >>gdas_restartb.txt

  #..................
  if [[ ${DO_WAVE} = "YES" ]]; then

    rm -rf gdaswave.txt
    touch gdaswave.txt
    rm -rf gdaswave_restart.txt
    touch gdaswave_restart.txt

    dirpath="gdas.${PDY}/${cyc}/wave/"
    dirname="./${dirpath}"

    head="gdaswave.t${cyc}z."

    #...........................
    echo "${dirname}gridded/${head}*      " >>gdaswave.txt
    echo "${dirname}station/${head}*      " >>gdaswave.txt

    echo "${dirname}restart/*             " >>gdaswave_restart.txt

  fi

  #..................
  if [[ ${DO_OCN} = "YES" ]]; then

    rm -rf gdasocean.txt
    touch gdasocean.txt
    rm -rf gdasocean_restart.txt
    touch gdasocean_restart.txt

    dirpath="gdas.${PDY}/${cyc}/ocean/"
    dirname="./${dirpath}"

    head="gdas.t${cyc}z."

    #...........................
    echo "${dirname}/${head}*             " >>gdasocean.txt
    echo "${dirname}/MOM_input            " >>gdasocean.txt

    echo "${dirname}/RESTART/*            " >>gdasocean_restart.txt

    dirpath="gdas.${PDY}/${cyc}/med/"
    dirname="./${dirpath}"

    echo "${dirname}/RESTART/*            " >>gdasocean_restart.txt

  fi

  if [[ ${DO_ICE} = "YES" ]]; then

    rm -rf gdasice.txt
    touch gdasice.txt
    rm -rf gdasice_restart.txt
    touch gdasice_restart.txt

    dirpath="gdas.${PDY}/${cyc}/ice/"
    dirname="./${dirpath}"

    head="gdas.t${cyc}z."

    #...........................
    echo "${dirname}/${head}*             " >>gdasice.txt
    echo "${dirname}/ice_in               " >>gdasice.txt

    echo "${dirname}/RESTART/*            " >>gdasice_restart.txt

 fi


#-----------------------------------------------------
fi   ##end of gdas
#-----------------------------------------------------


#-----------------------------------------------------
if [ ${type} = "enkfgdas" -o ${type} = "enkfgfs" ]; then
#-----------------------------------------------------

  IAUFHRS_ENKF=${IAUFHRS_ENKF:-6}
  lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}
  nfhrs=$(echo ${IAUFHRS_ENKF} | sed 's/,/ /g')
  NMEM_ENKF=${NMEM_ENKF:-80}
  NMEM_EARCGRP=${NMEM_EARCGRP:-10}               ##number of ens memebers included in each tarball
  NTARS=$((NMEM_ENKF/NMEM_EARCGRP))
  [[ ${NTARS} -eq 0 ]] && NTARS=1
  [[ $((NTARS*NMEM_EARCGRP)) -lt ${NMEM_ENKF} ]] && NTARS=$((NTARS+1))
##NTARS2=$((NTARS/2))  # number of earc groups to include analysis/increments
  NTARS2=${NTARS}

  dirpath="${RUN}.${PDY}/${cyc}/"
  dirname="./${dirpath}"
  head="${RUN}.t${cyc}z."

  #..................
  rm -f ${RUN}.txt
  touch ${RUN}.txt

  echo  "${dirname}${head}enkfstat                   " >>${RUN}.txt
  echo  "${dirname}${head}gsistat.ensmean            " >>${RUN}.txt
  if [[ -s ${ROTDIR}/${dirpath}${head}cnvstat.ensmean ]]; then
       echo  "${dirname}${head}cnvstat.ensmean       " >>${RUN}.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}oznstat.ensmean ]]; then
       echo  "${dirname}${head}oznstat.ensmean       " >>${RUN}.txt
  fi
  if [[ -s ${ROTDIR}/${dirpath}${head}radstat.ensmean ]]; then
       echo  "${dirname}${head}radstat.ensmean       " >>${RUN}.txt
  fi
  for FHR in $nfhrs; do  # loop over analysis times in window
     if [ $FHR -eq 6 ]; then
        if [ -s $ROTDIR/${dirpath}${head}atmanl.ensmean.nc ]; then
            echo  "${dirname}${head}atmanl.ensmean.nc      " >>${RUN}.txt
	fi
        if [ -s $ROTDIR/${dirpath}${head}atminc.ensmean.nc ]; then
            echo  "${dirname}${head}atminc.ensmean.nc      " >>${RUN}.txt
        fi
     else
        if [ -s $ROTDIR/${dirpath}${head}atma00${FHR}.ensmean.nc ]; then
	    echo  "${dirname}${head}atma00${FHR}.ensmean.nc      " >>${RUN}.txt
        fi
        if [ -s $ROTDIR/${dirpath}${head}atmi00${FHR}.ensmean.nc ]; then
            echo  "${dirname}${head}atmi00${FHR}.ensmean.nc      " >>${RUN}.txt
        fi
     fi 
  done # loop over FHR
  for fstep in eobs ecen esfc eupd efcs epos ; do
   echo  "logs/${CDATE}/${RUN}${fstep}*.log        " >>${RUN}.txt
  done

# eomg* are optional jobs
  for log in ${ROTDIR}/logs/${CDATE}/${RUN}eomg*.log; do
     if [[ -s "${log}" ]]; then
        echo  "logs/${CDATE}/${RUN}eomg*.log        " >>${RUN}.txt
     fi
     break
  done


# Ensemble spread file only available with netcdf output
  fh=3
  while [ $fh -le 9 ]; do
      fhr=$(printf %03i $fh)
      echo  "${dirname}${head}atmf${fhr}.ensmean.nc       " >>${RUN}.txt
      echo  "${dirname}${head}sfcf${fhr}.ensmean.nc       " >>${RUN}.txt
      if [ -s $ROTDIR/${dirpath}${head}atmf${fhr}.ensspread.nc ]; then
          echo  "${dirname}${head}atmf${fhr}.ensspread.nc     " >>${RUN}.txt
      fi
      fh=$((fh+3))
  done

  #...........................
  n=1
  while [[ ${n} -le ${NTARS} ]]; do
  #...........................

  rm -f ${RUN}_grp${n}.txt
  rm -f ${RUN}_restarta_grp${n}.txt
  rm -f ${RUN}_restartb_grp${n}.txt
  touch ${RUN}_grp${n}.txt
  touch ${RUN}_restarta_grp${n}.txt
  touch ${RUN}_restartb_grp${n}.txt

  m=1
  while [[ ${m} -le ${NMEM_EARCGRP} ]]; do
    nm=$(((n-1)*NMEM_EARCGRP+m))
    mem=$(printf %03i $nm)
    dirpath="${RUN}.${PDY}/${cyc}/mem${mem}/atmos/"
    dirname="./${dirpath}"
    head="${RUN}.t${cyc}z."

    #---
    for FHR in $nfhrs; do  # loop over analysis times in window
      if [ $FHR -eq 6 ]; then
         if [ $n -le $NTARS2 ]; then
            if [ -s $ROTDIR/${dirpath}${head}atmanl.nc ] ; then
                echo "${dirname}${head}atmanl.nc      " >>${RUN}_grp${n}.txt
            fi
	    if [ -s $ROTDIR/${dirpath}${head}ratminc.nc ] ; then
		echo "${dirname}${head}ratminc.nc      " >>${RUN}_grp${n}.txt
	    fi
         fi
         if [ -s $ROTDIR/${dirpath}${head}ratminc.nc ] ; then
             echo "${dirname}${head}ratminc.nc      " >>${RUN}_restarta_grp${n}.txt
         fi

      else
         if [ $n -le $NTARS2 ]; then
             if [ -s $ROTDIR/${dirpath}${head}atma00${FHR}.nc ] ; then
                 echo "${dirname}${head}atma00${FHR}.nc      " >>${RUN}_grp${n}.txt
             fi
             if [ -s $ROTDIR/${dirpath}${head}ratmi00${FHR}.nc ] ; then
                 echo "${dirname}${head}ratmi00${FHR}.nc      " >>${RUN}_grp${n}.txt
             fi
         fi
         if [ -s $ROTDIR/${dirpath}${head}ratmi00${FHR}.nc ] ; then
             echo "${dirname}${head}ratmi00${FHR}.nc      " >>${RUN}_restarta_grp${n}.txt
         fi

      fi 
      echo "${dirname}${head}atmf00${FHR}.nc       " >>${RUN}_grp${n}.txt
      if [ $FHR -eq 6 ]; then
	  echo "${dirname}${head}sfcf00${FHR}.nc       " >>${RUN}_grp${n}.txt
      fi
    done # loop over FHR

    if [[ lobsdiag_forenkf = ".false." ]] ; then
       echo "${dirname}${head}gsistat              " >>${RUN}_grp${n}.txt
       if [[ -s ${ROTDIR}/${dirpath}${head}cnvstat ]] ; then
          echo "${dirname}${head}cnvstat           " >>${RUN}_grp${n}.txt
       fi
       if [[ -s ${ROTDIR}/${dirpath}${head}radstat ]]; then
          echo "${dirname}${head}radstat           " >>${RUN}_restarta_grp${n}.txt
       fi
       if [[ -s ${ROTDIR}/${dirpath}${head}cnvstat ]]; then
          echo "${dirname}${head}cnvstat           " >>${RUN}_restarta_grp${n}.txt
       fi
       echo "${dirname}${head}abias                " >>${RUN}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_air            " >>${RUN}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_int            " >>${RUN}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_pc             " >>${RUN}_restarta_grp${n}.txt
    fi
    #---
    echo "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>${RUN}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>${RUN}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>${RUN}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>${RUN}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>${RUN}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>${RUN}_restarta_grp${n}.txt

    #---
    echo "${dirname}RESTART                     " >>${RUN}_restartb_grp${n}.txt

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

