#! /bin/sh
# Metafile Script : gfs_meta_mar_comp.sh
#
# This is a script which creates a metafile that runs a comparison of 500 MB 
# heights and PMSL between the older GFS model run and the newer one. The 
# metafile also generates a comparison between the UKMET older run and the newer
# GFS model run.
#
# Log :
# J. Carr/PMB    12/07/2004    Pushed into production.

# Set up Local Variables
#
set -x
#
export PS4='MAR_COMP_F${fend}:$SECONDS + '
rm -Rf $DATA/GEMPAK_META_MAR
mkdir -p -m 775 $DATA/GEMPAK_META_MAR $DATA/MAR_COMP

cd $DATA/MAR_COMP
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="mar_comp"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`
#
# BV export MODEL=/com/nawips/prod
#XXW export HPCGFS=${MODEL}/${mdl}.$PDY
# BV export HPCGFS=${COMROOT}/nawips/${envir}/${mdl}.$PDY
export HPCGFS=${COMINgempak}/${mdl}.${PDY}/${cyc}/gempak
export COMIN00=${COMINgempak}/${mdl}.${PDY}/00/gempak
export COMIN06=${COMINgempak}/${mdl}.${PDY}/06/gempak
export COMIN12=${COMINgempak}/${mdl}.${PDY}/12/gempak
export COMIN18=${COMINgempak}/${mdl}.${PDY}/18/gempak
if [ ${cyc} -eq 00 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_MAR
elif [ ${cyc} -eq 06 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_MAR
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_MAR
elif [ ${cyc} -eq 12 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_MAR
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_MAR
   cp $COMIN12/gfs_${PDY}12f* $DATA/GEMPAK_META_MAR
elif [ ${cyc} -eq 18 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_MAR
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_MAR
   cp $COMIN12/gfs_${PDY}12f* $DATA/GEMPAK_META_MAR
   cp $COMIN18/gfs_${PDY}18f* $DATA/GEMPAK_META_MAR
fi
export COMIN=$DATA/GEMPAK_META_MAR

# export HPCNAM=${COMINnam}.$PDY
export HPCNAM=${COMINnam}.$PDY

# export HPCNGM=${MODEL}/ngm.$PDY
#
# DEFINE YESTERDAY
PDYm1=`$NDATE -24 ${PDY}${cyc} | cut -c -8`
PDY2m1=`echo $PDYm1 | cut -c 3-`
#
# DEFINE 2 DAYS AGO
PDYm2=`$NDATE -48 ${PDY}${cyc} | cut -c -8`
PDY2m2=`echo $PDYm2 | cut -c 3-`
#
# DEFINE 3 DAYS AGO
PDYm3=`$NDATE -72 ${PDY}${cyc} | cut -c -8`
PDY2m3=`echo $PDYm3 | cut -c 3-`
#
# THE 1200 UTC CYCLE
#
if [ ${cyc} -eq 12 ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in NAtl NPac 
    do
        if [ ${gareas} = "NAtl" ] ; then
            garea="natl"
            proj=" "
            latlon="18/2/1/1/10"
        elif [ ${gareas} = "NPac" ] ; then
            garea="mpac"
            proj=" "
            latlon="18/2/1/1/10"
        fi  
        for runtime in 06 00
        do
            if [ ${runtime} = "06" ] ; then
                cyc2="06"
                grid2="F-${MDL} | ${PDY2}/0600"
                add="06"
                testgfsfhr="78"
            elif [ ${runtime} = "00" ] ; then
                cyc2="00"
                grid2="F-${MDL} | ${PDY2}/0000"
                add="12"
                testgfsfhr="114"
            fi
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126
            do
                gfsoldfhr=F`expr ${gfsfhr} + ${add}`
                gfsfhr2=`echo ${gfsfhr}`
                gfsfhr=F${gfsfhr}
                if [ ${gfsfhr2} -gt ${testgfsfhr} ] ; then
                    grid="F-${MDL} | ${PDY2}/${cyc}00"
                    grid2=" "
                    gfsoldfhr=" "
                    gdpfun1="sm5s(hght)"
                    gdpfun2="sm5s(pmsl)"
                    line="5/1/3/2/2"
                    hilo1=`echo ${hilo3}`
                    hilo2=`echo ${hilo4}`
                    title1=`echo ${title3}`
                    title2=`echo ${title4}`
                fi

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj} 
LATLON  = ${latlon}
SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
FINT    = 
FLINE   = 
REFVEC  =                                                                         
WIND    = 0 

GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
GLEVEL  = 500                                                                    
GVCORD  = PRES
GDPFUN  = ${gdpfun1}
LINE    = ${line}
SCALE   = -1
CTYPE   = c
CINT    = 6
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
HILO    = ${hilo1}
TITLE   = ${title1}
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = ${gdpfun2}
CINT    = 4
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
LINE    = ${line}
HILO    = ${hilo2}
TITLE   = ${title2}
run

EOF
export err=$?;err_chk
            done
        done
        # COMPARE THE 1200 UTC GFS MODEL TO THE 0000 UTC UKMET MODEL
        grid="F-${MDL} | ${PDY2}/${cyc}00"
        export HPCUKMET=${COMINukmet}.${PDY}
        grid2="F-UKMETHPC | ${PDY2}/0000"
        # for gfsfhr in 00 12 24 36 48 60 84 108
        for gfsfhr in 00 12 24 84 108
        do
            ukmetfhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}
	    
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
CTYPE   = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1;1//21//hw 
TEXT    = s/21//hw                                                                       
WIND    = 0              
REFVEC  =                                                                         
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} 12Z vs UKM 00Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ukmetfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? UKMET @ HGT (00Z CYAN)!0
l
ru

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} 12Z vs UKM 00Z PMSL!0
l
ru

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ukmetfhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? UKMET PMSL (00Z CYAN)!0
l
ru

EOF
export err=$?;err_chk
        done
        # COMPARE THE 1200 UTC GFS MODEL TO THE 1200 UTC ECMWF FROM YESTERDAY
        grid="F-${MDL} | ${PDY2}/${cyc}00"
        grid2=${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12 
        for gfsfhr in 00 24 48 72 96 120
        do
            ecmwffhr=F`expr ${gfsfhr} + 24`
	    gfsfhr=F${gfsfhr}
		
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
CTYPE   = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1;1//21//hw 
TEXT    = s/21//hw                                                                       
WIND    = 0              
REFVEC  =                                                                         
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} 12Z vs ECM yest 12Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ecmwffhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? ECMWF @ HGT (12Z YEST CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} 12Z vs ECM yest 12Z PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ecmwffhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? ECMWF PMSL (12Z YEST CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
        # COMPARE THE 1200 UTC GFS MODEL TO THE 1200 UTC NAM AND NGM
        grid="F-${MDL} | ${PDY2}/${cyc}00"
        grid2="F-NAMHPC | ${PDY2}/${cyc}00"
        # grid2ngm="F-NGMHPC | ${PDY2}/${cyc}00"
        for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84
        do
            namfhr=F${gfsfhr}
        #   ngmfhr=F${gfsfhr}
            gfsfhr=F${gfsfhr}
		
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon} 
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
TYPE    = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1;1//21//hw 
TEXT    = s/21//hw                                                                       
WIND    =               
REFVEC  =                                                                         
HILO    = 3/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} ${MDL}/NAM/NGM 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${namfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? NAM @ HGT (12Z CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 3/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} ${MDL}/NAM/NGM PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${namfhr}
LINE    = 6/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? NAM PMSL (12Z CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
    done
fi

if [ ${cyc} = "00" ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in NAtl NPac
    do
        if [ ${gareas} = "NAtl" ] ; then
            garea="natl" 
            proj=" "
            latlon="0"
        elif [ ${gareas} = "NPac" ] ; then
            garea="mpac"
            proj=" "
            latlon="18/2/1/1/10"
        fi
        for runtime in 18 12
        do
            if [ ${runtime} = "18" ] ; then
                cyc2="18"
		#XXW export HPCGFS=${MODEL}/${mdl}.${PDYm1}
		# BV export HPCGFS=$COMROOT/nawips/${envir}/${mdl}.${PDYm1}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1800"
                add="06"
                testgfsfhr="114"
            elif [ ${runtime} = "12" ] ; then
                cyc2="12"
		#XXW export HPCGFS=${MODEL}/${mdl}.${PDYm1}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1200"
                add="12"
                testgfsfhr="114"
            fi
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126
            do
                gfsoldfhr=F`expr ${gfsfhr} + ${add}`
                gfsfhr2=`echo ${gfsfhr}`
                gfsfhr=F${gfsfhr}
                if [ ${gfsfhr2} -gt ${testgfsfhr} ] ; then
                    grid="F-${MDL} | ${PDY2}/${cyc}00"
                    grid2=" "
                    gfsoldfhr=" "
                    gdpfun1="sm5s(hght)"
                    gdpfun2="sm5s(pmsl)"
                    line="5/1/3/2/2"
                    hilo1=`echo ${hilo3}`
                    hilo2=`echo ${hilo4}`
                    title1=`echo ${title3}`
                    title2=`echo ${title4}`
                fi
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj} 
LATLON  = ${latlon}
SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
FINT    = 
FLINE   = 
REFVEC  =                                                                         
WIND    = 0 

GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
GLEVEL  = 500                                                                    
GVCORD  = PRES
GDPFUN  = ${gdpfun1}
LINE    = ${line}
SCALE   = -1
CTYPE   = c
CINT    = 6
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
HILO    = ${hilo1}
TITLE   = ${title1}
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = ${gdpfun2}
CINT    = 4
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
LINE    = ${line}
HILO    = ${hilo2}
TITLE   = ${title2}
run

EOF
export err=$?;err_chk

            done
        done
        # COMPARE THE 0000 UTC GFS MODEL TO THE 1200 UTC UKMET FROM YESTERDAY
        grid="F-${MDL} | ${PDY2}/${cyc}00"
	export HPCUKMET=${COMINukmet}.${PDYm1}
        grid2="F-UKMETHPC | ${PDY2m1}/1200"
        # for gfsfhr in 00 12 24 36 48 60 84 108
        for gfsfhr in 00 12 24 84 108
        do 
            ukmetfhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}
export pgm=gdplot2_nc;. prep_step; startmsg    
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
TYPE    = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1.2;1.2//21//hw 
TEXT    = s/21//hw                                                                       
WIND    =               
REFVEC  =                                                                         
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HEIGHTS (00Z YELLOW)|~${gareas} 00Z vs UKM 12Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ukmetfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? UKMET @ HEIGHTS (12Z YEST CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
HLSYM   = 1.2;1.2//21//hw                                                           
TEXT    = s/21//hw                                                                
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} 00Z vs UKM 12Z PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ukmetfhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? UKMET PMSL (12Z YEST CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
        # COMPARE THE 0000 UTC GFS MODEL TO THE 1200 UTC ECMWF FROM YESTERDAY
        grid="F-${MDL} | ${PDY2}/${cyc}00"
        grid2="${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12"
        for gfsfhr in 12 36 60 84 108
        do
            ecmwffhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}
	    
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
TYPE    = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1.2;1.2//21//hw 
TEXT    = s/21//hw                                                                       
WIND    =               
REFVEC  =                                                                         
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (00Z YELLOW)|~${gareas} 00Z vs ECM 12Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ecmwffhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? ECMWF @ HGT (12Z YEST CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
HLSYM   = 1.2;1.2//21//hw                                                           
TEXT    = s/21//hw                                                                
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} 00Z vs ECM 12Z PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ecmwffhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? ECMWF PMSL (12Z YEST CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
        # COMPARE THE 0000 UTC GFS MODEL TO THE 0000 UTC NAM AND NGM
        grid="F-${MDL} | ${PDY2}/${cyc}00"
        grid2="F-NAMHPC | ${PDY2}/${cyc}00"
        for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84
        do
            namfhr=F${gfsfhr}
            gfsfhr=F${gfsfhr}
		
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0            
PANEL   = 0
CONTUR  = 2
CLRBAR  = 
GLEVEL  = 500                                                                     
GVCORD  = PRES 
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2         
SCALE   = -1           
TYPE    = c            
CINT    = 6            
FINT    = 
FLINE   = 
HLSYM   = 1.2;1.2//21//hw 
TEXT    = s/21//hw                                                                       
WIND    =               
REFVEC  =                                                                         
HILO    = 3/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (00Z YELLOW)|~${gareas} ${MDL}/NAM/NGM 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${namfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? NAM @ HGT (00Z CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4                                           
HLSYM   = 1.2;1.2//21//hw                                                           
TEXT    = s/21//hw                                                                
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 3/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} ${MDL}/NAM/NGM PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${namfhr}
LINE    = 6/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? NAM PMSL (CYAN)!0
l
run

EOF
export err=$?;err_chk

        done
    done
fi

if [ ${cyc} -eq "18" ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in NAtl NPac
    do
        if [ ${gareas} = "NAtl" ] ; then
            garea="natl"
            proj=" "
            latlon="0"
        elif [ ${gareas} = "NPac" ] ; then
            garea="mpac"
            proj=" "
            latlon="18/2/1/1/10"
        fi
        for runtime in 12 06
        do
            if [ ${runtime} = "12" ] ; then
                cyc2="12"
                grid2="F-${MDL} | ${PDY2}/1200"
                add="06"
                testgfsfhr="84"
            elif [ ${runtime} = "06" ] ; then
                cyc2="06"
                grid2="F-${MDL} | ${PDY2}/0600"
                add="12"
                testgfsfhr="72"
            fi   
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84
            do
                gfsoldfhr=F`expr ${gfsfhr} + ${add}`
                gfsfhr2=`echo ${gfsfhr}`
                gfsfhr="F${gfsfhr}"
                if [ ${gfsfhr2} -gt ${testgfsfhr} ] ; then
                    grid="F-${MDL} | ${PDY2}/${cyc}00"
                    grid2=" "
                    gfsoldfhr=" "
                    gdpfun1="sm5s(hght)"
                    gdpfun2="sm5s(pmsl)"
                    line="5/1/3/2/2"
                    hilo1=`echo ${hilo3}`
                    hilo2=`echo ${hilo4}`
                    title1=`echo ${title3}`
                    title2=`echo ${title4}`
                fi
export pgm=gdplot2_nc;. prep_step; startmsg

$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
SKIP    = 0     
PANEL   = 0
CONTUR  = 1
CLRBAR  =
FINT    =
FLINE   =
REFVEC  =
WIND    = 0

GDFILE  = ${grid}!${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
GLEVEL  = 500   
GVCORD  = PRES
GDPFUN  = ${gdpfun1}
LINE    = ${line}
SCALE   = -1
TYPE    = c
CINT    = 6
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
HILO    = ${hilo1}
TITLE   = ${title1}
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = ${gdpfun2}
CINT    = 4
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
LINE    = ${line}
HILO    = ${hilo2}
TITLE   = ${title2}
run

ex
EOF
export err=$?;err_chk
            done
        done
    done
fi

if [ ${cyc} -eq "06" ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in NAtl NPac
    do
        if [ ${gareas} = "NAtl" ] ; then
            garea="natl"
            proj=" "
            latlon="0"
        elif [ ${gareas} = "NPac" ] ; then
            garea="mpac"
            proj=" "
            latlon="18/2/1/1/10"
        fi
        for runtime in 00 18
        do
            if [ ${runtime} = "00" ] ; then
                cyc2="00"
                grid2="F-${MDL} | ${PDY2}/0000"
                add="06"
                testgfsfhr="84"
            elif [ ${runtime} = "18" ] ; then
                cyc2="18"
		#XXW export HPCGFS=${MODEL}/${mdl}.${PDYm1}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak
                grid2="F-GFSHPC | ${PDY2m1}/1800"
                add="12"
                testgfsfhr="72"
            fi   
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z vs ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84
            do
                gfsoldfhr=F`expr ${gfsfhr} + ${add}`
                gfsfhr2=`echo ${gfsfhr}`
                gfsfhr="F${gfsfhr}"
                if [ ${gfsfhr2} -gt ${testgfsfhr} ] ; then
                    grid="F-${MDL} | ${PDY2}/${cyc}00"
                    grid2=" "
                    gfsoldfhr=" "
                    gdpfun1="sm5s(hght)"
                    gdpfun2="sm5s(pmsl)"
                    line="5/1/3/2/2"
                    hilo1=`echo ${hilo3}`
                    hilo2=`echo ${hilo4}`
                    title1=`echo ${title3}`
                    title2=`echo ${title4}`
                fi
export pgm=gdplot2_nc;. prep_step; startmsg

$GEMEXE/gdplot2_nc << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
SKIP    = 0     
PANEL   = 0
CONTUR  = 1
CLRBAR  =
FINT    =
FLINE   =
REFVEC  =
WIND    = 0

GDFILE  = ${grid}!${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
GLEVEL  = 500   
GVCORD  = PRES
GDPFUN  = ${gdpfun1}
LINE    = ${line}
SCALE   = -1
TYPE    = c
CINT    = 6
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
HILO    = ${hilo1}
TITLE   = ${title1}
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = ${gdpfun2}
CINT    = 4
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
LINE    = ${line}
HILO    = ${hilo2}
TITLE   = ${title2}
run

ex
EOF
export err=$?;err_chk

            done
        done
    done
fi

####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l ${metaname}
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk
if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_comp
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_comp
   fi
fi

exit
