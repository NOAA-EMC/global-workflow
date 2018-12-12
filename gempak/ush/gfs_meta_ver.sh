#!/bin/sh
#
# Metafile Script : gfs_meta_ver_new
#
# Log :
# J. Carr/HPC     1/98   Added new metafile
# J. Carr/HPC     5/98   Converted to gdplot2
# J. Carr/HPC     8/98   Changed map to medium resolution
# J. Carr/HPC     2/99   Changed skip to 0
# J. Carr/HPC     6/99   Added latlon and a filter to map
# J. Carr/HPC     7/99   Added South American area.
# J. Carr/HPC   2/2001   Edited to run on the IBM.
# J. Carr/HPC   5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC   8/2001   Changed to a korn shell for turnover to production.
# J. Carr/HPC   8/2001   Submitted.
# J. Carr/PMB  11/2004   Inserted a ? into all title/TITLE lines.
#                        Changed contur from 1 to a 2.
#                        Added logic to take the script from f126 to f228.
#                        This will remove need for mrfver.
#                        Removed logic which differentiated cycles since all cycles run to F384.
#                        Added a South American area for International desk.
#
# Set up Local Variables
#
set -x
export PS4='VER:$SECONDS + '
mkdir -p -m 775 $DATA/VER
cd $DATA/VER
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL=GFS
metatype="ver"
metaname="gfsver_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo ${PDY} | cut -c3-`
#
# DEFINE 1 CYCLE AGO
dc1=`$NDATE -06 ${PDY}${cyc} | cut -c -10`
date1=`echo ${dc1} | cut -c -8`
sdate1=`echo ${dc1} | cut -c 3-8`
cycle1=`echo ${dc1} | cut -c 9,10`
# DEFINE 2 CYCLES AGO
dc2=`$NDATE -12 ${PDY}${cyc} | cut -c -10`
date2=`echo ${dc2} | cut -c -8`
sdate2=`echo ${dc2} | cut -c 3-8`
cycle2=`echo ${dc2} | cut -c 9,10`
# DEFINE 3 CYCLES AGO
dc3=`$NDATE -18 ${PDY}${cyc} | cut -c -10`
date3=`echo ${dc3} | cut -c -8`
sdate3=`echo ${dc3} | cut -c 3-8`
cycle3=`echo ${dc3} | cut -c 9,10`
# DEFINE 4 CYCLES AGO
dc4=`$NDATE -24 ${PDY}${cyc} | cut -c -10`
date4=`echo ${dc4} | cut -c -8`
sdate4=`echo ${dc4} | cut -c 3-8`
cycle4=`echo ${dc4} | cut -c 9,10`
# DEFINE 5 CYCLES AGO
dc5=`$NDATE -30 ${PDY}${cyc} | cut -c -10`
date5=`echo ${dc5} | cut -c -8`
sdate5=`echo ${dc5} | cut -c 3-8`
cycle5=`echo ${dc5} | cut -c 9,10`
# DEFINE 6 CYCLES AGO
dc6=`$NDATE -36 ${PDY}${cyc} | cut -c -10`
date6=`echo ${dc6} | cut -c -8`
sdate6=`echo ${dc6} | cut -c 3-8`
cycle6=`echo ${dc6} | cut -c 9,10`
# DEFINE 7 CYCLES AGO
dc7=`$NDATE -42 ${PDY}${cyc} | cut -c -10`
date7=`echo ${dc7} | cut -c -8`
sdate7=`echo ${dc7} | cut -c 3-8`
cycle7=`echo ${dc7} | cut -c 9,10`
# DEFINE 8 CYCLES AGO
dc8=`$NDATE -48 ${PDY}${cyc} | cut -c -10`
date8=`echo ${dc8} | cut -c -8`
sdate8=`echo ${dc8} | cut -c 3-8`
cycle8=`echo ${dc8} | cut -c 9,10`
# DEFINE 9 CYCLES AGO
dc9=`$NDATE -54 ${PDY}${cyc} | cut -c -10`
date9=`echo ${dc9} | cut -c -8`
sdate9=`echo ${dc9} | cut -c 3-8`
cycle9=`echo ${dc9} | cut -c 9,10`
# DEFINE 10 CYCLES AGO
dc10=`$NDATE -60 ${PDY}${cyc} | cut -c -10`
date10=`echo ${dc10} | cut -c -8`
sdate10=`echo ${dc10} | cut -c 3-8`
cycle10=`echo ${dc10} | cut -c 9,10`
# DEFINE 11 CYCLES AGO
dc11=`$NDATE -66 ${PDY}${cyc} | cut -c -10`
date11=`echo ${dc11} | cut -c -8`
sdate11=`echo ${dc11} | cut -c 3-8`
cycle11=`echo ${dc11} | cut -c 9,10`
# DEFINE 12 CYCLES AGO
dc12=`$NDATE -72 ${PDY}${cyc} | cut -c -10`
date12=`echo ${dc12} | cut -c -8`
sdate12=`echo ${dc12} | cut -c 3-8`
cycle12=`echo ${dc12} | cut -c 9,10`
# DEFINE 13 CYCLES AGO
dc13=`$NDATE -78 ${PDY}${cyc} | cut -c -10`
date13=`echo ${dc13} | cut -c -8`
sdate13=`echo ${dc13} | cut -c 3-8`
cycle13=`echo ${dc13} | cut -c 9,10`
# DEFINE 14 CYCLES AGO
dc14=`$NDATE -84 ${PDY}${cyc} | cut -c -10`
date14=`echo ${dc14} | cut -c -8`
sdate14=`echo ${dc14} | cut -c 3-8`
cycle14=`echo ${dc14} | cut -c 9,10`
# DEFINE 15 CYCLES AGO
dc15=`$NDATE -90 ${PDY}${cyc} | cut -c -10`
date15=`echo ${dc15} | cut -c -8`
sdate15=`echo ${dc15} | cut -c 3-8`
cycle15=`echo ${dc15} | cut -c 9,10`
# DEFINE 16 CYCLES AGO
dc16=`$NDATE -96 ${PDY}${cyc} | cut -c -10`
date16=`echo ${dc16} | cut -c -8`
sdate16=`echo ${dc16} | cut -c 3-8`
cycle16=`echo ${dc16} | cut -c 9,10`
# DEFINE 17 CYCLES AGO
dc17=`$NDATE -102 ${PDY}${cyc} | cut -c -10`
date17=`echo ${dc17} | cut -c -8`
sdate17=`echo ${dc17} | cut -c 3-8`
cycle17=`echo ${dc17} | cut -c 9,10`
# DEFINE 18 CYCLES AGO
dc18=`$NDATE -108 ${PDY}${cyc} | cut -c -10`
date18=`echo ${dc18} | cut -c -8`
sdate18=`echo ${dc18} | cut -c 3-8`
cycle18=`echo ${dc18} | cut -c 9,10`
# DEFINE 19 CYCLES AGO
dc19=`$NDATE -114 ${PDY}${cyc} | cut -c -10`
date19=`echo ${dc19} | cut -c -8`
sdate19=`echo ${dc19} | cut -c 3-8`
cycle19=`echo ${dc19} | cut -c 9,10`
# DEFINE 20 CYCLES AGO
dc20=`$NDATE -120 ${PDY}${cyc} | cut -c -10`
date20=`echo ${dc20} | cut -c -8`
sdate20=`echo ${dc20} | cut -c 3-8`
cycle20=`echo ${dc20} | cut -c 9,10`
# DEFINE 21 CYCLES AGO
dc21=`$NDATE -126 ${PDY}${cyc} | cut -c -10`
date21=`echo ${dc21} | cut -c -8`
sdate21=`echo ${dc21} | cut -c 3-8`
cycle21=`echo ${dc21} | cut -c 9,10`
# DEFINE 22 CYCLES AGO
dc22=`$NDATE -132 ${PDY}${cyc} | cut -c -10`
date22=`echo ${dc22} | cut -c -8`
sdate22=`echo ${dc22} | cut -c 3-8`
cycle22=`echo ${dc22} | cut -c 9,10`
# DEFINE 23 CYCLES AGO
dc23=`$NDATE -138 ${PDY}${cyc} | cut -c -10`
date23=`echo ${dc23} | cut -c -8`
sdate23=`echo ${dc23} | cut -c 3-8`
cycle23=`echo ${dc23} | cut -c 9,10`
# DEFINE 24 CYCLES AGO
dc24=`$NDATE -144 ${PDY}${cyc} | cut -c -10`
date24=`echo ${dc24} | cut -c -8`
sdate24=`echo ${dc24} | cut -c 3-8`
cycle24=`echo ${dc24} | cut -c 9,10`
# DEFINE 25 CYCLES AGO
dc25=`$NDATE -150 ${PDY}${cyc} | cut -c -10`
date25=`echo ${dc25} | cut -c -8`
sdate25=`echo ${dc25} | cut -c 3-8`
cycle25=`echo ${dc25} | cut -c 9,10`
# DEFINE 26 CYCLES AGO
dc26=`$NDATE -156 ${PDY}${cyc} | cut -c -10`
date26=`echo ${dc26} | cut -c -8`
sdate26=`echo ${dc26} | cut -c 3-8`
cycle26=`echo ${dc26} | cut -c 9,10`
# DEFINE 27 CYCLES AGO
dc27=`$NDATE -162 ${PDY}${cyc} | cut -c -10`
date27=`echo ${dc27} | cut -c -8`
sdate27=`echo ${dc27} | cut -c 3-8`
cycle27=`echo ${dc27} | cut -c 9,10`
# DEFINE 28 CYCLES AGO
dc28=`$NDATE -168 ${PDY}${cyc} | cut -c -10`
date28=`echo ${dc28} | cut -c -8`
sdate28=`echo ${dc28} | cut -c 3-8`
cycle28=`echo ${dc28} | cut -c 9,10`
# DEFINE 29 CYCLES AGO
dc29=`$NDATE -174 ${PDY}${cyc} | cut -c -10`
date29=`echo ${dc29} | cut -c -8`
sdate29=`echo ${dc29} | cut -c 3-8`
cycle29=`echo ${dc29} | cut -c 9,10`
# DEFINE 30 CYCLES AGO
dc30=`$NDATE -180 ${PDY}${cyc} | cut -c -10`
date30=`echo ${dc30} | cut -c -8`
sdate30=`echo ${dc30} | cut -c 3-8`
cycle30=`echo ${dc30} | cut -c 9,10`
# DEFINE 31 CYCLES AGO
dc31=`$NDATE -192 ${PDY}${cyc} | cut -c -10`
date31=`echo ${dc31} | cut -c -8`
sdate31=`echo ${dc31} | cut -c 3-8`
cycle31=`echo ${dc31} | cut -c 9,10`
# DEFINE 32 CYCLES AGO
dc32=`$NDATE -204 ${PDY}${cyc} | cut -c -10`
date32=`echo ${dc32} | cut -c -8`
sdate32=`echo ${dc32} | cut -c 3-8`
cycle32=`echo ${dc32} | cut -c 9,10`
# DEFINE 33 CYCLES AGO
dc33=`$NDATE -216 ${PDY}${cyc} | cut -c -10`
date33=`echo ${dc33} | cut -c -8`
sdate33=`echo ${dc33} | cut -c 3-8`
cycle33=`echo ${dc33} | cut -c 9,10`

# SET CURRENT CYCLE AS THE VERIFICATION GRIDDED FILE.
vergrid="F-${MDL} | ${PDY2}/${cyc}00"
fcsthr="f00"

# SET WHAT RUNS TO COMPARE AGAINST BASED ON MODEL CYCLE TIME.
#if [ ${cyc} -eq 00 ] ; then
#    verdays="${dc1} ${dc2} ${dc3} ${dc4} ${dc5} ${dc6} ${dc7} ${dc8} ${dc9} ${dc10} ${dc11} ${dc12} ${dc13} ${dc14} ${dc15} ${dc16} ${dc17} ${dc18} ${dc19} ${dc20} ${dc21}"
#elif [ ${cyc} -eq 12 ] ; then
#    verdays="${dc1} ${dc2} ${dc3} ${dc4} ${dc5} ${dc6} ${dc7} ${dc8} ${dc9} ${dc10} ${dc11} ${dc12} ${dc13} ${dc14} ${dc15} ${dc16} ${dc17} ${dc18} ${dc19} ${dc20} ${dc21}"
#else
#    verdays="${dc1} ${dc2} ${dc3} ${dc4} ${dc5} ${dc6} ${dc7} ${dc8} ${dc9} ${dc10} ${dc11} ${dc12} ${dc13} ${dc14} ${dc15} ${dc16} ${dc17} ${dc18} ${dc19} ${dc20} ${dc21}"
#fi 

verdays="${dc1} ${dc2} ${dc3} ${dc4} ${dc5} ${dc6} ${dc7} ${dc8} ${dc9} ${dc10} ${dc11} ${dc12} ${dc13} ${dc14} ${dc15} ${dc16} ${dc17} ${dc18} ${dc19}
${dc20} ${dc21} ${dc22} ${dc23} ${dc24} ${dc25} ${dc26} ${dc27} ${dc28} ${dc29} ${dc30} ${dc31} ${dc32} ${dc33}"


#GENERATING THE METAFILES.
MDL2="GFSHPC"
for verday in ${verdays}
    do
    cominday=`echo ${verday} | cut -c -8`
    #XXW export HPCGFS=$COMROOT/nawips/prod/${mdl}.${cominday}
    # BV export HPCGFS=$COMROOT/nawips/${envir}/${mdl}.${cominday}
    export HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cyc}/gempak

    if [ ${verday} -eq ${dc1} ] ; then
        dgdattim=f006
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle1}/gempak
        grid="F-${MDL2} | ${sdate1}/${cycle1}00"
    elif [ ${verday} -eq ${dc2} ] ; then
        dgdattim=f012
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle2}/gempak
        grid="F-${MDL2} | ${sdate2}/${cycle2}00"
    elif [ ${verday} -eq ${dc3} ] ; then
        dgdattim=f018
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle3}/gempak
        grid="F-${MDL2} | ${sdate3}/${cycle3}00"
    elif [ ${verday} -eq ${dc4} ] ; then
        dgdattim=f024
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle4}/gempak
        grid="F-${MDL2} | ${sdate4}/${cycle4}00"
    elif [ ${verday} -eq ${dc5} ] ; then
        dgdattim=f030
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle5}/gempak
        grid="F-${MDL2} | ${sdate5}/${cycle5}00"
    elif [ ${verday} -eq ${dc6} ] ; then
        dgdattim=f036
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle6}/gempak
        grid="F-${MDL2} | ${sdate6}/${cycle6}00"
    elif [ ${verday} -eq ${dc7} ] ; then
        dgdattim=f042
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle7}/gempak
        grid="F-${MDL2} | ${sdate7}/${cycle7}00"
    elif [ ${verday} -eq ${dc8} ] ; then
        dgdattim=f048
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle8}/gempak
        grid="F-${MDL2} | ${sdate8}/${cycle8}00"
    elif [ ${verday} -eq ${dc9} ] ; then
        dgdattim=f054
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle9}/gempak
        grid="F-${MDL2} | ${sdate9}/${cycle9}00"
    elif [ ${verday} -eq ${dc10} ] ; then
        dgdattim=f060
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle10}/gempak
        grid="F-${MDL2} | ${sdate10}/${cycle10}00"
    elif [ ${verday} -eq ${dc11} ] ; then
        dgdattim=f066
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle11}/gempak
        grid="F-${MDL2} | ${sdate11}/${cycle11}00"
    elif [ ${verday} -eq ${dc12} ] ; then
        dgdattim=f072
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle12}/gempak
        grid="F-${MDL2} | ${sdate12}/${cycle12}00"
    elif [ ${verday} -eq ${dc13} ] ; then
        dgdattim=f078
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle13}/gempak
        grid="F-${MDL2} | ${sdate13}/${cycle13}00"
    elif [ ${verday} -eq ${dc14} ] ; then
        dgdattim=f084
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle14}/gempak
        grid="F-${MDL2} | ${sdate14}/${cycle14}00"
    elif [ ${verday} -eq ${dc15} ] ; then
        dgdattim=f090
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle15}/gempak
        grid="F-${MDL2} | ${sdate15}/${cycle15}00"
    elif [ ${verday} -eq ${dc16} ] ; then
        dgdattim=f096
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle16}/gempak
        grid="F-${MDL2} | ${sdate16}/${cycle16}00"
    elif [ ${verday} -eq ${dc17} ] ; then
        dgdattim=f102
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle17}/gempak
        grid="F-${MDL2} | ${sdate17}/${cycle17}00"
    elif [ ${verday} -eq ${dc18} ] ; then
        dgdattim=f108
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle18}/gempak
        grid="F-${MDL2} | ${sdate18}/${cycle18}00"
    elif [ ${verday} -eq ${dc19} ] ; then
        dgdattim=f114
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle19}/gempak
        grid="F-${MDL2} | ${sdate19}/${cycle19}00"
    elif [ ${verday} -eq ${dc20} ] ; then
        dgdattim=f120
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle20}/gempak
        grid="F-${MDL2} | ${sdate20}/${cycle20}00"
    elif [ ${verday} -eq ${dc21} ] ; then
        dgdattim=f126
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle21}/gempak
        grid="F-${MDL2} | ${sdate21}/${cycle21}00"
    elif [ ${verday} -eq ${dc22} ] ; then
        dgdattim=f132
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle22}/gempak
        grid="F-${MDL2} | ${sdate22}/${cycle22}00"
    elif [ ${verday} -eq ${dc23} ] ; then
        dgdattim=f138
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle23}/gempak
        grid="F-${MDL2} | ${sdate23}/${cycle23}00"
    elif [ ${verday} -eq ${dc24} ] ; then
        dgdattim=f144
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle24}/gempak
        grid="F-${MDL2} | ${sdate24}/${cycle24}00"
    elif [ ${verday} -eq ${dc25} ] ; then
        dgdattim=f150
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle25}/gempak
        grid="F-${MDL2} | ${sdate25}/${cycle25}00"
    elif [ ${verday} -eq ${dc26} ] ; then
        dgdattim=f156
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle26}/gempak
        grid="F-${MDL2} | ${sdate26}/${cycle26}00"
    elif [ ${verday} -eq ${dc27} ] ; then
        dgdattim=f162
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle27}/gempak
        grid="F-${MDL2} | ${sdate27}/${cycle27}00"
    elif [ ${verday} -eq ${dc28} ] ; then
        dgdattim=f168
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle28}/gempak
        grid="F-${MDL2} | ${sdate28}/${cycle28}00"
    elif [ ${verday} -eq ${dc29} ] ; then
        dgdattim=f174
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle29}/gempak
        grid="F-${MDL2} | ${sdate29}/${cycle29}00"
    elif [ ${verday} -eq ${dc30} ] ; then
        dgdattim=f180
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle30}/gempak
        grid="F-${MDL2} | ${sdate30}/${cycle30}00"
    elif [ ${verday} -eq ${dc31} ] ; then
        dgdattim=f192
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle31}/gempak
        grid="F-${MDL2} | ${sdate31}/${cycle31}00"
    elif [ ${verday} -eq ${dc32} ] ; then
        dgdattim=f204
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle32}/gempak
        grid="F-${MDL2} | ${sdate32}/${cycle32}00"
    elif [ ${verday} -eq ${dc33} ] ; then
        dgdattim=f216
        HPCGFS=${COMINgempak}/${mdl}.${cominday}/${cycle33}/gempak
        grid="F-${MDL2} | ${sdate33}/${cycle33}00"
    fi

# 500 MB HEIGHT METAFILE

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOFplt
PROJ     = STR/90.0;-95.0;0.0
GAREA    = 5.1;-124.6;49.6;-11.9
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ ? GFS 500 MB HGT|~500 HGT DIFF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS 500 MB HGT
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
title    = 6/-2/~ ? GFS PMSL|~PMSL DIFF
clear    = yes
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS PMSL
clear    = no
r

!PROJ     = 
!GAREA    = bwus
!gdfile   = ${vergrid}
!gdattim  = ${fcsthr}
!gdpfun   = sm5s(pmsl)
!glevel   = 0
!gvcord   = none
!scale    = 0
!cint     = 4
!line     = 6/1/3
!contur   = 2
!title    = 6/-2/~ ? GFS PMSL |~US PMSL DIFF
!clear    = yes
!r
!
!gdfile   = ${grid}
!gdattim  = ${dgdattim}
!line     = 5/1/3
!contur   = 4
!title    = 5/-1/~ ? GFS PMSL
!clear    = no
!r

! SOUTH AMERICAN AREA.
! 500 MB

PROJ     = 
GAREA    = samps
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ ? GFS 500 MB HGT|~SAM 500 HGT DIFF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS 500 MB HGT
clear    = no
r

! SOUTH AMERICAN AREA.
! PMSL

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ ? GFS PMSL|~SAM PMSL DIFF
clear    = yes
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS PMSL
clear    = no
r

ex
EOFplt
export err=$?;err_chk
#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

done

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/gfsver_${PDY}_${cyc}
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
      ${COMOUT}/gfsver_${PDY}_${cyc}
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/gfsver_${PDY}_${cyc}
      fi
   fi
fi


exit
