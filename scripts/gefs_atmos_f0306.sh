#!/bin/ksh

set -xa
#cdate=$1
DATA=$2

#foutmax=6 #last lead hour to extract 
cd "${DATA}" || exit 1

#ddate="echo $cdate | cut -c1-8"
#YYYY=`echo $cdate | cut -c1-4`
#MONTH=`echo $cdate | cut -c5-6`
#DAY=`echo $cdate | cut -c7-8`

# COMIN_00and03 and {COMIN_master} are directory containing the files that we want to extract
#Extract c00 files for f003 from PSL reanalysis data

      fnh=03
      echo "extracting f${fnh}"
      oufile=${DATA}/gefs.t00z.master.grb2f0${fnh}

      infile=${COMIN_00and03}/GFSPRS.GrbF03
      if [[ -f "${infile}" ]]; then #check if input file exists before extraction

      ${WGRIB2} "${infile}" | grep "TSNOWP" | ${WGRIB2} -i "${infile}" -grib tmp ||true
      ${WGRIB2} tmp -for "2:2" -append -grib  "${oufile}">/dev/null || true 
 
      ${WGRIB2} "${infile}" | grep ":APCP:surface" | ${WGRIB2} -i "${infile}" -grib tmp ||true
      ${WGRIB2} tmp -for "1:1" -append -grib  "${oufile}">/dev/null || true

      ${WGRIB2} "${infile}" | grep ":ACPCP:surface" | ${WGRIB2} -i "${infile}" -grib tmp ||true
      ${WGRIB2} tmp -for "1:1" -append -grib  "${oufile}">/dev/null || true

      ${WGRIB2} "${infile}" | grep ":NCPCP:surface" | ${WGRIB2} -i "${infile}" -grib tmp ||true
      ${WGRIB2} tmp -for "1:1" -append -grib  "${oufile}">/dev/null || true

      ${WGRIB2} "${infile}" | grep ":HCDC:high cloud layer:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
      ${WGRIB2} "${infile}" | grep ":MCDC:middle cloud layer:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
      ${WGRIB2} "${infile}" | grep ":LCDC:low cloud layer:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true

      rm tmp
   else
    echo "${infile} does not exist"
   fi 

      infile=${COMIN_00and03}/GFSFLX.GrbF03
      if [[ -f "${infile}" ]]; then #check if input file exists before extraction
        ${WGRIB2} "${infile}" | grep "WATR" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "SNOWC" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "SNOHF" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":DLWRF:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":ULWRF:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":DSWRF:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":USWRF:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":USWRF:top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":ULWRF:top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":UFLX:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":VFLX:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":SHTFL:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":LHTFL:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":PRATE:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":CPRAT:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":ALBDO:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":TCDC:entire" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":TCDC:boundary" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":GFLUX:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":U-GWD:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep ":V-GWD:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "TMP:middle cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "TMP:low cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "TMP:high cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:high cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:middle cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:low cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:high cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:middle cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "PRES:low cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "CWORK" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "DUVB" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "CDUVB" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "TMAX" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
        ${WGRIB2} "${infile}" | grep "TMIN" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
      else
        echo "${infile} does not exist"
      fi 
#  done #fnh

  #Extract individual member files for f006 master data
  fnh=006

   echo "extracting f${fnh}"
    infile=${COMIN_master}/gefs.t00z.master.grb2f${fnh}
    oufile=${DATA}/gefs.t00z.master.grb2f${fnh}

   if [[ -f "${infile}" ]]; then #check if input file exists before extraction
#    rm -f ${outfile}/gefs.t00z.master.grb2f${fnh}  #remove outfile if it already exists before extraction
    ${WGRIB2} "${infile}" | grep "TSNOWP" | ${WGRIB2} -i "${infile}" -grib tmp
    ${WGRIB2} tmp -for "2:2" -append -grib  "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "WATR" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "SNOWC" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "SNOHF" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":DLWRF:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":ULWRF:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":DSWRF:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":USWRF:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":USWRF:top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":ULWRF:top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":UFLX:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":VFLX:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":SHTFL:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":LHTFL:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":PRATE:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":CPRAT:surface:0" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":ALBDO:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":TCDC:entire" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":TCDC:boundary" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":HCDC:high cloud layer:0-6" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":MCDC:middle cloud layer:0-6" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":LCDC:low cloud layer:0-6" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":GFLUX:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":U-GWD:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":V-GWD:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":APCP:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":ACPCP:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep ":NCPCP:surface" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "TMP:middle cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "TMP:low cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "TMP:high cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:high cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:middle cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:low cloud top" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:high cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:middle cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "PRES:low cloud bottom" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "CWORK" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "DUVB" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "CDUVB" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "TMAX" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true
    ${WGRIB2} "${infile}" | grep "TMIN" | ${WGRIB2} -i "${infile}" -append -grib "${oufile}">/dev/null || true

   else
    echo "${infile} does not exist"
   fi

 export exec_dir=${EXECacc}
 export sorc_dir=${SORCacc}
 export sorc_name=gefs_6h_ave_1mem

#   cd $DATA

   "${exec_dir}/${sorc_name}" >sorc_name.exe.out
   cat sorc_name.exe.out

#output f06
   infile=${COMIN_master}/gefs.t00z.master.grb2f006

    ${WGRIB2} "${infile}" | grep "TSNOWP" | ${WGRIB2} -i "${infile}" -grib TSNOWP2.dat
    ${WGRIB2}  TSNOWP2.dat -for "1:1" -append -grib  TSNOWP1.dat >/dev/null || true

   ${WGRIB2} "${infile}" -not "(ULWRF|USWRF)" -not "(TSNOWP|DLWRF|DSWRF|UFLX|VFLX|SHTFL|LHTFL|PRATE|CPRAT|ALBDO|GFLUX|U-GWD|V-GWD)" -not "TCDC:(entire|boundary)" -not "HCDC:high cloud layer:0-6" -not "LCDC:low cloud layer:0-6" -not "MCDC:middle cloud layer:0-6" -not "(APCP|ACPCP|NCPCP)" -not "(TMIN|TMAX|DUVB|CDUVB|CWORK|SNOHF|SNOWC|WATR)" -not "(TMP:middle cloud top|TMP:low cloud top|TMP:high cloud top)" -not "(PRES:high cloud top|PRES:middle cloud top|PRES:low cloud top|PRES:high cloud bottom|PRES:middle cloud bottom|PRES:low cloud bottom)" -grib out1.grb2
   
   cat out1.grb2 gefs.t00z.pgrb2af006 TSNOWP1.dat > out2.grb

   mv "${COMIN_master}/gefs.t00z.master.grb2f006" "${COMIN_master}/gefs.t00z.master.grb2f006_org"
   mv "${COMIN_master}/gefs.t00z.master.grb2if006" "${COMIN_master}/gefs.t00z.master.grb2if006_org"

   mv out2.grb "${COMIN_master}/gefs.t00z.master.grb2f006"
   ${GRB2INDEX} "${COMIN_master}/gefs.t00z.master.grb2f006"  "${COMIN_master}/gefs.t00z.master.grb2if006"

   rm -fr out1.grb2 out2.grb TSNOWP*.dat

#output f03
   infile=${COMIN_master}/gefs.t00z.master.grb2f003
#
    ${WGRIB2} "${infile}" | grep "TSNOWP" | ${WGRIB2} -i "${infile}" -grib TSNOWP2.dat
    ${WGRIB2}  TSNOWP2.dat -for "1:1" -grib  out.grb >/dev/null || true
    ${WGRIB2}   out.grb -set_ftime "0-3 hour acc fcst" -grib TSNOWP1.dat 

   ${WGRIB2} "${infile}" -not "(TSNOWP|ULWRF|USWRF)" -not "(DLWRF|DSWRF|UFLX|VFLX|SHTFL|LHTFL|PRATE|CPRAT|ALBDO|GFLUX|U-GWD|V-GWD)" -not "TCDC:(entire|boundary)" -not "HCDC:high cloud layer:182-185 min" -not "LCDC:low cloud layer:182-185 min" -not "MCDC:middle cloud layer:182-185 min" -not "(APCP|ACPCP|NCPCP)" -not "(TMIN|TMAX|DUVB|CDUVB|CWORK|SNOHF|SNOWC|WATR)" -not "(TMP:middle cloud top|TMP:low cloud top|TMP:high cloud top)" -not "(PRES:high cloud top|PRES:middle cloud top|PRES:low cloud top|PRES:high cloud bottom|PRES:middle cloud bottom|PRES:low cloud bottom)" -grib out1.grb

   ${WGRIB2}   out1.grb -set_ftime "3 hour fcst" -grib out2.grb
   cat out2.grb gefs.t00z.pgrb2af003 TSNOWP1.dat > out3.grb

   mv "${COMIN_master}/gefs.t00z.master.grb2f003" "${COMIN_master}/gefs.t00z.master.grb2f003_org"
   mv "${COMIN_master}/gefs.t00z.master.grb2if003" "${COMIN_master}/gefs.t00z.master.grb2if003_org"

   mv out3.grb "${COMIN_master}/gefs.t00z.master.grb2f003"
   ${GRB2INDEX} "${COMIN_master}/gefs.t00z.master.grb2f003"  "${COMIN_master}/gefs.t00z.master.grb2if003"

   rm -fr out.grb out1.grb out2.grb

   exit
