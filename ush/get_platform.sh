#!/bin/sh

###############################################################
# Auto-detect platform and return string identifier
###############################################################

get_platform() {

  local hname=`hostname -d`
  local pname="unknown"

  if [[ $hname = 'stampede2.tacc.utexas.edu' ]] ; then
    # We are on Xsede stampede2
    pname="stampede"
  elif [[ $hname = 'hpc.msstate.edu' ]] ; then
    # We are on MSU Orion
    pname="orion"
  elif [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
    pname="jet"
  elif [[ -d /scratch1 ]] ; then
    # We are on NOAA Hera
    pname="hera.intel"
  elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
    pname="wcoss_c"
  elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
    # We are on NOAA Mars or Venus
    pname="wcoss_dell_p3"
  elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
    pname="wcoss"
  elif [[ -d /glade ]] ; then
    # We are on NCAR Cheyenne
    pname="cheyenne"
  elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
    pname="gaea"
  else
    echo WARNING: UNKNOWN PLATFORM 
  fi

  echo "${pname}"
}
