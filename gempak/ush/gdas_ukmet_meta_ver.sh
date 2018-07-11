#!/bin/sh
#
# Metafile Script : gdas_ukmet_meta_ver
#
# Creates a comparison loop between the 6 hr gdas fcst and the prvs
# 9 cycles of the ukmet
#
# Log :
# J. Carr/HPC     3/2001   Added new metafile
# J. Carr/HPC     5/2001   Added a mn variable for a/b side dbnet root variable.
# M. Klein/HPC   11/2004   Change fnl/FNL to gdas/GDAS (name change only)
# M. Klein/HPC    2/2005   Changed location of working directory to /ptmp
# M. Klein/HPC   11/2006   Modify to run in production.
#

#cd $DATA

set -xa

if [ $cyc -ne "06" ] ; then
    exit
fi

export pgm=gdplot2_nc;. prep_step; startmsg

device="nc | ukmetver_12.meta"
PDY2=`echo ${PDY} | cut -c3-`

#
# Copy in datatype table to define gdfile type
#
cp $FIXgempak/datatype.tbl datatype.tbl

#
# DEFINE 1 CYCLE AGO
dc1=`$NDATE -06 ${PDY}${cyc} | cut -c -10`
date1=`echo ${dc1} | cut -c -8`
sdate1=`echo ${dc1} | cut -c 3-8`
cycle1=`echo ${dc1} | cut -c 9,10`
# DEFINE 2 CYCLES AGO
dc2=`$NDATE -18 ${PDY}${cyc} | cut -c -10`
date2=`echo ${dc2} | cut -c -8`
sdate2=`echo ${dc2} | cut -c 3-8`
cycle2=`echo ${dc2} | cut -c 9,10`
# DEFINE 3 CYCLES AGO
dc3=`$NDATE -30 ${PDY}${cyc} | cut -c -10`
date3=`echo ${dc3} | cut -c -8`
sdate3=`echo ${dc3} | cut -c 3-8`
cycle3=`echo ${dc3} | cut -c 9,10`
# DEFINE 4 CYCLES AGO
dc4=`$NDATE -42 ${PDY}${cyc} | cut -c -10`
date4=`echo ${dc4} | cut -c -8`
sdate4=`echo ${dc4} | cut -c 3-8`
cycle4=`echo ${dc4} | cut -c 9,10`
# DEFINE 5 CYCLES AGO
dc5=`$NDATE -54 ${PDY}${cyc} | cut -c -10`
date5=`echo ${dc5} | cut -c -8`
sdate5=`echo ${dc5} | cut -c 3-8`
cycle5=`echo ${dc5} | cut -c 9,10`
# DEFINE 6 CYCLES AGO
dc6=`$NDATE -66 ${PDY}${cyc} | cut -c -10`
date6=`echo ${dc6} | cut -c -8`
sdate6=`echo ${dc6} | cut -c 3-8`
cycle6=`echo ${dc6} | cut -c 9,10`
# DEFINE 7 CYCLES AGO
dc7=`$NDATE -90 ${PDY}${cyc} | cut -c -10`
date7=`echo ${dc7} | cut -c -8`
sdate7=`echo ${dc7} | cut -c 3-8`
cycle7=`echo ${dc7} | cut -c 9,10`
# DEFINE 8 CYCLES AGO
dc8=`$NDATE -114 ${PDY}${cyc} | cut -c -10`
date8=`echo ${dc8} | cut -c -8`
sdate8=`echo ${dc8} | cut -c 3-8`
cycle8=`echo ${dc8} | cut -c 9,10`
# DEFINE 9 CYCLES AGO
dc9=`$NDATE -138 ${PDY}${cyc} | cut -c -10`
date9=`echo ${dc9} | cut -c -8`
sdate9=`echo ${dc9} | cut -c 3-8`
cycle9=`echo ${dc9} | cut -c 9,10`

# SET CURRENT CYCLE AS THE VERIFICATION GRIDDED FILE.
vergrid="F-GDAS | ${PDY2}/0600"
fcsthr="0600f006"

# SET WHAT RUNS TO COMPARE AGAINST BASED ON MODEL CYCLE TIME.
areas="SAM NAM"
verdays="${dc1} ${dc2} ${dc3} ${dc4} ${dc5} ${dc6} ${dc7} ${dc8} ${dc9}"

# GENERATING THE METAFILES.
for area in $areas
    do 
    if [ ${area} = "NAM" ] ; then
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
        if [ ${verday} -eq ${dc1} ] ; then
            dgdattim=f012
            sdatenum=$sdate1
            cyclenum=$cycle1
        elif [ ${verday} -eq ${dc2} ] ; then
            dgdattim=f024
            sdatenum=$sdate2
            cyclenum=$cycle2
        elif [ ${verday} -eq ${dc3} ] ; then
            dgdattim=f036
            sdatenum=$sdate3
            cyclenum=$cycle3
        elif [ ${verday} -eq ${dc4} ] ; then
            dgdattim=f048
            sdatenum=$sdate4
            cyclenum=$cycle4
        elif [ ${verday} -eq ${dc5} ] ; then
            dgdattim=f060
            sdatenum=$sdate5
            cyclenum=$cycle5
        elif [ ${verday} -eq ${dc6} ] ; then
            dgdattim=f072
            sdatenum=$sdate6
            cyclenum=$cycle6
        elif [ ${verday} -eq ${dc7} ] ; then
            dgdattim=f096
            sdatenum=$sdate7
            cyclenum=$cycle7
        elif [ ${verday} -eq ${dc8} ] ; then
            dgdattim=f120
            sdatenum=$sdate8
            cyclenum=$cycle8
        elif [ ${verday} -eq ${dc9} ] ; then
            dgdattim=f144
            sdatenum=$sdate9
            cyclenum=$cycle9
        fi
        # JY grid="$COMROOT/nawips/${envir}/ukmet.20${sdatenum}/ukmet_20${sdatenum}${cyclenum}${dgdattim}"
        grid="${COMINukmet}.20${sdatenum}/ukmet_20${sdatenum}${cyclenum}${dgdattim}"

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
title    = 6/-2/~ GDAS 500 MB HGT (6-HR FCST)|~${area} 500 HGT DIFF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ UKMET 500 MB HGT
clear    = no
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
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~${area} PMSL DIFF
clear    = yes
latlon   = ${latlon}
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ UKMET PMSL
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
title    = 5/-1/~ UKMET PMSL
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
ls -l ukmetver_12.meta
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
    mkdir -p -m 775 ${COMOUTukmet}/ukmet.${PDY}/meta/
    mv ukmetver_12.meta ${COMOUTukmet}/ukmet.${PDY}/meta/ukmetver_${PDY}_12
    if [ $SENDDBN = "YES" ] ; then
        ${DBNROOT}/bin/dbn_alert MODEL UKMETVER_HPCMETAFILE $job \
        ${COMOUTukmet}/ukmet.${PDY}/meta/ukmetver_${PDY}_12
    fi
fi

exit
