#! /bin/sh
#
# Metafile Script : gfs_meta_precip.sh
#
# Log :
# M. Klein/WPC    01/29/2014   Created.  Adapted from gfs_meta_qpf.sh
#
# Set up Local Variables
#
set -x
export PS4='qpf:$SECONDS + '
mkdir -p -m 775 $DATA/precip
cd $DATA/precip
cp $FIXgempak/datatype.tbl datatype.tbl

#
# Set model and metafile naming conventions
#
mdl=gfs
MDL=GFS
metatype="precip"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`

#
# Set range of forecast hours.  GFS is available every 6 hours through F192, then 
# every 12 hours after.  The request was to have the fields go to F216, so will run
# the gdplot for the ranges set below, then for the 12-hour and greater QPF periods,
# run the gdplot2 from F204-F216.  6-hour QPF will stop at F192. 
#

gdatpcpn06="F006-F192-06"
gdatpcpn12="F012-F192-06"
gdatpcpn24="F024-F192-06"
gdatpcpn48="F048-F192-06"
gdatpcpn60="F060-F192-06"
gdatpcpn72="F072-F192-06"
gdatpcpn84="F084-F192-06"
gdatpcpn96="F096-F192-06"
gdatpcpn120="F120-F192-06"
gdatpcpn144="F144-F192-06"
gdatpcpn168="F168-F192-06"
run="r"

#
# For CPC - Day 6-10 and Day 8-14 QPFs using a North American regional display
#
garea_cpc="17.529;-129.296;53.771;-22.374"
proj_cpc="str/90;-105;0"

# Notes -- 
#  00Z cycle - 8-14 Day -- No F198 file, so started at F204. Makes a P156I, not P162I.
#  06Z cycle - 6-10 Day -- No F258 file, so ended at F252.  Makes a P108I, not P114I.
#            - 8-14 Day -- No F354 file, so ended at F348.  Makes a P156I, not P162I.            
#  12Z cycle - 8-14 Day -- No F210 file, so started at F216. Makes a P156I, not P162I.
#  18Z cycle - 6-10 Day -- No F270 file, so ended at F264.  Makes a P108I, not P114I.
#            - 8-14 Day -- No F366 file, so ended at F360. Makes a P156I, not P162I.

gdattim_6to10=""
gdattim_8to14=""
gdpfun_6to10="p114i"
gdpfun_8to14="p162i"
if [ ${cyc} = "00" ] ; then
    gdattim_6to10="${PDY2}/${cyc}00F264"
    gdattim_8to14="${PDY2}/${cyc}00F360"
    gdpfun_6to10="p114i"
    gdpfun_8to14="p156i"
elif [ ${cyc} = "06" ] ; then
    #gdattim_6to10="${PDY2}/${cyc}00F258"
    #gdattim_8to14="${PDY2}/${cyc}00F354"
    gdattim_6to10="${PDY2}/${cyc}00F252"
    gdattim_8to14="${PDY2}/${cyc}00F348"
    gdpfun_6to10="p108i"
    gdpfun_8to14="p156i"
elif [ ${cyc} = "12" ] ; then
    gdattim_6to10="${PDY2}/${cyc}00F276"
    gdattim_8to14="${PDY2}/${cyc}00F372"
    gdpfun_6to10="p114i"
    gdpfun_8to14="p156i"
elif [ ${cyc} = "18" ] ; then
    #gdattim_6to10="${PDY2}/${cyc}00F270"
    #gdattim_8to14="${PDY2}/${cyc}00F366"
    gdattim_6to10="${PDY2}/${cyc}00F264"
    gdattim_8to14="${PDY2}/${cyc}00F360"
    gdpfun_6to10="p108i"
    gdpfun_8to14="p156i"
fi

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOFplt
gdfile   = F-${MDL} | ${PDY2}/${cyc}00
garea    = us
proj     = 
map      = 1/1/2/yes
device   = ${device}
clear    = yes
text     = m/22/2/hw
panel    = 0
contur   = 2
latlon   = 0
gdattim  = ${gdatpcpn06}
glevel   = 0!0
gvcord   = none!none
skip     = 0   !0
scale    = 0   !0
gdpfun   = p06i!sm5s(pmsl)
type     = f   !c
cint     =     !4
line     =     !17/1/3
fint     = .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9
fline    = 0;21-30;14-20;5
hilo     = 31;0/x#2/.01-20//50;50/y!17/H#;L#/1020-1070;900-1012
hlsym    = 1.5!1;1//22;22/2;2/hw
clrbar   = 1
wind     = bk0
refvec   =
title    = 1/-2/~ ? ${MDL} 6-HOUR TOTAL PCPN, MSLP |~6-HR TOTAL PCPN & MSLP!0
filter   = no
ijskip   = 0
r

gdpfun   = c06i!sm5s(pmsl)
title    = 1/-2/~ ? ${MDL} 6-HOUR CONV PCPN, MSLP |~6-HR CONV PCPN & MSLP!0
r

glevel   = 0
gvcord   = none
skip     = 0
scale    = 0
gdpfun   = p06i
type     = f
cint     =
line     = 
hilo     = 31;0/x#2/.01-20//50;50/y
hlsym    = 1.5
wind     = 
title    = 1/-2/~ ? ${MDL} 6-HOUR TOTAL PCPN|~6-HR TOTAL PCPN!0
l
r

gdattim  = ${gdatpcpn12} 
gdpfun   = p12i
title    = 1/-2/~ ? ${MDL} 12-HOUR TOTAL PCPN|~12-HR TOTAL PCPN!0
l
r

gdattim  = F204-F216-12
l
r

gdattim  = ${gdatpcpn24}       
gdpfun   = p24i
title    = 1/-2/~ ? ${MDL} 24-HOUR TOTAL PCPN|~24-HR TOTAL PCPN!0
l
r

gdattim  = F204-F216-12
l
r

gdattim  = ${gdatpcpn48}
gdpfun   = p48i
title    = 1/-2/~ ? ${MDL} 48 HOUR TOTAL PCPN|~48-HR TOTAL PCPN!0
l
r

gdattim  = F204-F216-12
l
r

gdattim  = ${gdatpcpn60}
gdpfun   = p60i
title    = 1/-2/~ ? ${MDL} 60 HOUR TOTAL PCPN|~60-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn72}       
gdpfun   = p72i
title    = 1/-2/~ ? ${MDL} 72 HOUR TOTAL PCPN|~72-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn84}       
gdpfun   = p84i
title    = 1/-2/~ ? ${MDL} 84 HOUR TOTAL PCPN|~84-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn96}
gdpfun   = p96i
title    = 1/-2/~ ? ${MDL} 96 HOUR TOTAL PCPN|~96-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn120}
gdpfun   = p120i
title    = 1/-2/~ ? ${MDL} 120 HOUR TOTAL PCPN|~120-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn144}
gdpfun   = p144i
title    = 1/-2/~ ? ${MDL} 144 HOUR TOTAL PCPN|~144-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

gdattim  = ${gdatpcpn168}
gdpfun   = p168i
title    = 1/-2/~ ? ${MDL} 168 HOUR TOTAL PCPN|~168-HR TOTAL PCPN!0
r

gdattim  = F204-F216-12
r

garea    = ${garea_cpc}
proj     = ${proj_cpc}
gdattim  = ${gdattim_6to10}
gdpfun   = ${gdpfun_6to10}
hilo     = 31;0/x#2/.01-20/2/70;70/y
hlsym    = 1.5!1;1//22;22/2;2/hw
title    = 1/-2/~ ? ${MDL} 6-10 DAY PCPN|~6-10 DAY PCPN
run

gdattim  = ${gdattim_8to14}
gdpfun   = ${gdpfun_8to14}
title    = 1/-2/~ ? ${MDL} 8-14 DAY PCPN|~8-14 DAY PCPN
run

exit
EOFplt

export err=$?;err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
      ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
      fi
   fi
fi

exit
