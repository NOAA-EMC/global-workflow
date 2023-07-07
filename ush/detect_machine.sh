#!/bin/bash

# First detect w/ hostname
case $(hostname -f) in

  adecflow0[12].acorn.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### acorn
  alogin0[12].acorn.wcoss2.ncep.noaa.gov)    MACHINE_ID=wcoss2 ;; ### acorn
  clogin0[1-9].cactus.wcoss2.ncep.noaa.gov)  MACHINE_ID=wcoss2 ;; ### cactus01-9
  clogin10.cactus.wcoss2.ncep.noaa.gov)      MACHINE_ID=wcoss2 ;; ### cactus10
  dlogin0[1-9].dogwood.wcoss2.ncep.noaa.gov) MACHINE_ID=wcoss2 ;; ### dogwood01-9
  dlogin10.dogwood.wcoss2.ncep.noaa.gov)     MACHINE_ID=wcoss2 ;; ### dogwood10

  gaea9)               MACHINE_ID=gaea ;; ### gaea9
  gaea1[0-6])          MACHINE_ID=gaea ;; ### gaea10-16
  gaea9.ncrc.gov)      MACHINE_ID=gaea ;; ### gaea9
  gaea1[0-6].ncrc.gov) MACHINE_ID=gaea ;; ### gaea10-16

  hfe0[1-9]) MACHINE_ID=hera ;; ### hera01-9
  hfe1[0-2]) MACHINE_ID=hera ;; ### hera10-12
  hecflow01) MACHINE_ID=hera ;; ### heraecflow01

  s4-submit.ssec.wisc.edu) MACHINE_ID=s4 ;; ### s4

  fe[1-8]) MACHINE_ID=jet ;; ### jet01-8
  tfe[12]) MACHINE_ID=jet ;; ### tjet1-2

  Orion-login-[1-4].HPC.MsState.Edu) MACHINE_ID=orion ;; ### orion1-4

  cheyenne[1-6].cheyenne.ucar.edu)     MACHINE_ID=cheyenne ;; ### cheyenne1-6
  cheyenne[1-6].ib0.cheyenne.ucar.edu) MACHINE_ID=cheyenne ;; ### cheyenne1-6
  chadmin[1-6].ib0.cheyenne.ucar.edu)  MACHINE_ID=cheyenne ;; ### cheyenne1-6

  login[1-4].stampede2.tacc.utexas.edu) MACHINE_ID=stampede ;; ### stampede1-4

  login0[1-2].expanse.sdsc.edu) MACHINE_ID=expanse ;; ### expanse1-2

  discover3[1-5].prv.cube) MACHINE_ID=discover ;; ### discover31-35
  *) MACHINE_ID=UNKNOWN ;;  # Unknown platform
esac

if [[ ${MACHINE_ID} == "UNKNOWN" ]]; then 
   case ${PW_CSP:-} in
      "aws" | "google" | "azure") MACHINE_ID=noaacloud ;;
      *) PW_CSP="UNKNOWN"
   esac
fi

# Overwrite auto-detect with MACHINE if set
MACHINE_ID=${MACHINE:-${MACHINE_ID}}

# If MACHINE_ID is no longer UNKNNOWN, return it
if [[ "${MACHINE_ID}" != "UNKNOWN" ]]; then
  return
fi

# Try searching based on paths since hostname may not match on compute nodes
if [[ -d /lfs/f1 ]] ; then
  # We are on NOAA Cactus or Dogwood
  MACHINE_ID=wcoss2
elif [[ -d /mnt/lfs1 ]] ; then
  # We are on NOAA Jet
  MACHINE_ID=jet
elif [[ -d /scratch1 ]] ; then
  # We are on NOAA Hera
  MACHINE_ID=hera
elif [[ -d /work ]] ; then
  # We are on MSU Orion
  MACHINE_ID=orion
elif [[ -d /glade ]] ; then
  # We are on NCAR Yellowstone
  MACHINE_ID=cheyenne
elif [[ -d /lustre && -d /ncrc ]] ; then
  # We are on GAEA.
  MACHINE_ID=gaea
elif [[ -d /data/prod ]] ; then
  # We are on SSEC's S4
  MACHINE_ID=s4
else
  echo WARNING: UNKNOWN PLATFORM 1>&2
fi
