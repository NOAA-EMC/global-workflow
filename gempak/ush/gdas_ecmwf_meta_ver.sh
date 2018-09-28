#!/bin/sh
#
# Metafile Script : gdas_ecmwf_meta_ver
#
# Creates a loop comparing the 6 hr gdas fcst to the pervious 7 days
# of ecmwf fcsts
#
# Log :
# J. Carr/HPC   3/2001   New metafile for verification of ecmwf.
# J. Carr/HPC   5/2001   Added a mn variable for a/b side dbnet root variable.
# M. Klein/HPC 11/2004   Changed verification grid from fnl to gdas
# M. Klein/HPC  2/2005   Changed location of working directory to /ptmp
# M. Klein/HPC 11/2006   Modify to run in production.
#

#cd $DATA

set -xa

if [ $cyc -ne "06" ] ; then
    exit
fi

export pgm=gdplot2_nc;. prep_step; startmsg

cyc=12
device="nc | ecmwfver.meta"
PDY2=`echo ${PDY} | cut -c3-`

#
# Copy in datatype table to define gdfile type
#
cp $FIXgempak/datatype.tbl datatype.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File datatype.tbl does not exist."
   exit $err
fi

#
# DEFINE YESTERDAY
date1=`$NDATE -24 ${PDY}${cyc} | cut -c -8`
sdate1=`echo ${date1} | cut -c 3-`
# DEFINE 2 DAYS AGO
date2=`$NDATE -48 ${PDY}${cyc} | cut -c -8`
sdate2=`echo ${date2} | cut -c 3-`
# DEFINE 3 DAYS AGO
date3=`$NDATE -72 ${PDY}${cyc} | cut -c -8`
sdate3=`echo ${date3} | cut -c 3-`
# DEFINE 4 DAYS AGO
date4=`$NDATE -96 ${PDY}${cyc} | cut -c -8`
sdate4=`echo ${date4} | cut -c 3-`
# DEFINE 5 DAYS AGO
date5=`$NDATE -120 ${PDY}${cyc} | cut -c -8`
sdate5=`echo ${date5} | cut -c 3-`
# DEFINE 6 DAYS AGO
date6=`$NDATE -144 ${PDY}${cyc} | cut -c -8`
sdate6=`echo ${date6} | cut -c 3-`
# DEFINE 7 DAYS AGO
date7=`$NDATE -168 ${PDY}${cyc} | cut -c -8`
sdate7=`echo ${date7} | cut -c 3-`

vergrid="F-GDAS | ${PDY2}/0600"
fcsthr="0600f006"

# GENERATING THE METAFILES.
areas="SAM NAM"
verdays="${date1} ${date2} ${date3} ${date4} ${date5} ${date6} ${date7}"

for area in $areas
    do
    if [ $area == "NAM" ] ; then
        garea="5.1;-124.6;49.6;-11.9"
        proj="STR/90.0;-95.0;0.0"
        latlon="0"
        run="run"
    else
        garea="-33.7;-150.5;8.0;-35.0"
        proj="str/-85;-70;0"
        latlon="1/10/1/2/10;10"
        run=" "
    fi
    for verday in $verdays
        do
        verddate=`echo ${verday} | cut -c 3-`
        if [ ${verday} -eq ${date1} ] ; then
            dgdattim=f024
            sdatenum=$sdate1
        elif [ ${verday} -eq ${date2} ] ; then
            dgdattim=f048
            sdatenum=$sdate2
        elif [ ${verday} -eq ${date3} ] ; then
            dgdattim=f072
            sdatenum=$sdate3
        elif [ ${verday} -eq ${date4} ] ; then
            dgdattim=f096
            sdatenum=$sdate4
        elif [ ${verday} -eq ${date5} ] ; then
            dgdattim=f120
            sdatenum=$sdate5
        elif [ ${verday} -eq ${date6} ] ; then
            dgdattim=f144
            sdatenum=$sdate6
        elif [ ${verday} -eq ${date7} ] ; then
            dgdattim=f168
            sdatenum=$sdate7
        fi
        # JY grid="$COMROOT/nawips/${envir}/ecmwf.20${sdatenum}/ecmwf_glob_20${sdatenum}12"
        grid="${COMINecmwf}.20${sdatenum}/ecmwf_glob_20${sdatenum}12"

# 500 MB HEIGHT METAFILE

$GEMEXE/gdplot2_nc << EOFplt
\$MAPFIL = mepowo.gsf
PROJ     = ${proj}
GAREA    = ${garea}
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c
latlon   = ${latlon}

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ GDAS 500 MB HGT (6-HR FCST)|~${area} 500 HGT DF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF 500 MB HGT
clear    = no
latlon   = 0
r

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~${area} PMSL DF
clear    = yes
latlon   = ${latlon}
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF PMSL
clear    = no
r

PROJ     = 
GAREA    = bwus
gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~US PMSL DIFF
clear    = yes
latlon   = ${latlon}
${run}

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF PMSL
clear    = no
${run}

ex
EOFplt

    done
done

export err=$?;err_chk
#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l ecmwfver.meta
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
    mkdir -p -m 775 ${COMOUTecmwf}.${PDY}/meta
    mv ecmwfver.meta ${COMOUTecmwf}.${PDY}/meta/ecmwfver_${PDY}_${cyc}
    export err=$?
    if [[ $err -ne 0 ]] ; then
       echo " File ecmwfver.meta does not exist."
       exit $err
    fi

    if [ $SENDDBN = "YES" ] ; then
        ${DBNROOT}/bin/dbn_alert MODEL ECMWFVER_HPCMETAFILE $job \
        ${COMOUTecmwf}.${PDY}/meta/ecmwfver_${PDY}_${cyc}
    fi
fi

exit
