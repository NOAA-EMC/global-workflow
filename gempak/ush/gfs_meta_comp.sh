#! /bin/sh
# Metafile Script : gfs_meta_comp.sh
#
# This is a script which creates a metafile that runs a comparison of 500 MB 
# heights and PMSL between the older GFS model run and the newer one. The 
# metafile also generates a comparison between the UKMET older run and the newer
# GFS model run.
#
# Log :
# J. Carr/HPC    5/12/97   Developed Script
# J. Carr/HPC    8/05/98   Changed map to medium resolution and redefined yesterday code
# J. Carr/HPC    2/01/99   Changed skip to 0
# J. Carr/HPC    4/12/99   Added gfs model out to 84 hours.
# J. Carr/HPC       6/99   put a filter on map
# J. Carr/HPC     4/2000   Upped the eta comp to 60 hrs.
# J. Carr/HPC     2/2001   Edited to run on the IBM.
# J. Carr/HPC     5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC     7/2001   Added more comparison times.
# J. Carr/HPC     7/2001   Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC     7/2001   Submitted.
# J. Carr/HPC    11/2004   Changed all eta/ETA entries to nam/NAM.
#                          Inserted a ? in all title/TITLE lines.
#
# Set up Local Variables
#
set -x
#
export PS4='COMP:$SECONDS + '
rm -Rf $DATA/COMP $DATA/GEMPAK_META_COMP
mkdir -p -m 775 $DATA/COMP  $DATA/GEMPAK_META_COMP
cd $DATA/COMP
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL=GFS
metatype="comp"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`
#
#XXW export MODEL=$COMROOT/nawips/prod
# BV export MODEL=$COMROOT/nawips/${envir}
# BV export HPCGFS=${MODEL}/${mdl}.$PDY
export HPCGFS=${COMINgempak}/${mdl}.${PDY}/${cyc}/gempak
export COMIN00=${COMINgempak}/${mdl}.${PDY}/00/gempak
export COMIN06=${COMINgempak}/${mdl}.${PDY}/06/gempak
export COMIN12=${COMINgempak}/${mdl}.${PDY}/12/gempak
export COMIN18=${COMINgempak}/${mdl}.${PDY}/18/gempak
if [ ${cyc} -eq 00 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_COMP
elif [ ${cyc} -eq 06 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_COMP
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_COMP
elif [ ${cyc} -eq 12 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_COMP
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_COMP
   cp $COMIN12/gfs_${PDY}12f* $DATA/GEMPAK_META_COMP
elif [ ${cyc} -eq 18 ] ; then
   cp $COMIN00/gfs_${PDY}00f* $DATA/GEMPAK_META_COMP
   cp $COMIN06/gfs_${PDY}06f* $DATA/GEMPAK_META_COMP
   cp $COMIN12/gfs_${PDY}12f* $DATA/GEMPAK_META_COMP
   cp $COMIN18/gfs_${PDY}18f* $DATA/GEMPAK_META_COMP
fi
export COMIN=$DATA/GEMPAK_META_COMP

#XXW export HPCNAM=${MODEL}/nam.$PDY
#XXW export HPCNGM=${MODEL}/ngm.$PDY
# BV export HPCNAM=$COMROOT/nawips/prod/nam.$PDY
export HPCNAM=${COMINnam}.$PDY

# export HPCNGM=$COMROOT/nawips/prod/ngm.$PDY
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
    for gareas in US NP
    do
        if [ ${gareas} = US ] ; then
            garea="bwus"
            proj=" "
            latlon="0"
        elif [ ${gareas} = NP ] ; then
            garea="5;-177;45;-72"
            proj="STR/90.0;-155.0;0.0"
            latlon="1/1/1/1/10"
        fi
        for runtime in 06 00 12y 122d
        do
            if [ ${runtime} = "06" ] ; then
                cyc2="06"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/0600"
                add="06"
                testgfsfhr="120"
            elif [ ${runtime} = "00" ] ; then
                cyc2="00"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/0000"
                add="12"
                testgfsfhr="114"
            elif [ ${runtime} = "12y" ] ; then
                cyc2="12"
                desc="Y"
                #XXW export HPCGFS=${MODEL}/gfs.${PDYm1}
                # BV export HPCGFS=$COMROOT/nawips/${envir}/gfs.${PDYm1}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1200"
                add="24"
                testgfsfhr="102"
            elif [ ${runtime} = "122d" ] ; then
                cyc2="12"
                desc="Y2"
                #XXW export HPCGFS=${MODEL}/gfs.${PDYm2}
                # BV export HPCGFS=$COMROOT/nawips/${esnvir}/gfs.${PDYm2}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm2}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m2}/1200"
                add="48"
                testgfsfhr="96"
            fi
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z ${desc} CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z ${desc} CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL"
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
\$MAPFIL= mepowo.gsf
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
        # JY export HPCUKMET=$COMROOT/nawips/prod/ukmet.${PDY}
        export HPCUKMET=${COMINukmet}.${PDY}
        grid2="F-UKMETHPC | ${PDY2}/0000"
        # for gfsfhr in 00 12 24 36 48 60 84 108
        for gfsfhr in 00 12 24 84 108
        do
            ukmetfhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL= mepowo.gsf
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
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} 12Z VS UK 00Z 500 HGT!0
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
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} 12Z VS UK 00Z PMSL!0
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
        #XXW grid2=${MODEL}/ecmwf.${PDYm1}/ecmwf_glob_${PDYm1}12 
        grid2=${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12 
        for gfsfhr in 00 24 48 72 96 120
        do
            ecmwffhr=F`expr ${gfsfhr} + 24`
	    gfsfhr=F${gfsfhr}
		
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL= mepowo.gsf
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
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} 12Z VS EC Y 12Z 500 HGT!0
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
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} 12Z VS EC Y 12Z PMSL!0
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
		
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL= mepowo.gsf
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
LINE    = 3/1/3/2         
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
TITLE   = 3/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${gareas} ${MDL}/NAM/NGM 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${namfhr}
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-2/~ ? NAM @ HGT (12Z CYAN)!0
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
LINE    = 3/1/3/2
HILO    = 3/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 3/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${gareas} ${MDL}/NAM/NGM PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${namfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-2/~ ? NAM PMSL (12Z CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
    done
fi

if [ ${cyc} -eq 00 ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in US NP 
    do
        if [ ${gareas} = US ] ; then
            garea="bwus" 
            proj=" "
            latlon="0"
        elif [ ${gareas} = NP ] ; then
            garea="5;-177;45;-72"
            proj="STR/90.0;-155.0;0.0"
            latlon="1/1/1/1/10"
        fi
        for runtime in 18 12 00y 002d
        do
            if [ ${runtime} = "18" ] ; then
                cyc2="18"
                desc="Y"
# BV            export HPCGFS=${MODEL}/gfs.${PDYm1}
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1800"
                add="06"
                testgfsfhr="120"
            elif [ ${runtime} = "12" ] ; then
                cyc2="12"
                desc="Y"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1200"
                add="12"
                testgfsfhr="114"
            elif [ ${runtime} = "00y" ] ; then
                cyc2="00"
                desc="Y"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/0000"
                add="24"
                testgfsfhr="102"
            elif [ ${runtime} = "002d" ] ; then
                cyc2="00"
                desc="Y2"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm2}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m2}/0000"
                add="48"
                testgfsfhr="96"
            fi
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z ${desc} CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z ${desc} CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL"
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
\$MAPFIL= mepowo.gsf
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
        #XXW export HPCUKMET=${MODEL}/ukmet.${PDYm1}
        export HPCUKMET=${COMINukmet}.${PDYm1}
        grid2="F-UKMETHPC | ${PDY2m1}/1200"
        # for gfsfhr in 00 12 24 36 48 60 84 108
        for gfsfhr in 00 12 24 84 108
        do
            ukmetfhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}
	    
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL= mepowo.gsf
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
TITLE   = 5/-1/~ ? ${MDL} @ HEIGHTS (00Z YELLOW)|~${gareas} 00Z VS UK Y 12Z 500 HGT!0
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
TITLE   = 5/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} 00Z VS UK Y 12Z PMSL!0
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
        # JY grid2="$COMROOT/nawips/prod/ecmwf.${PDYm1}/ecmwf_glob_${PDYm1}12"
        grid2="${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12"
        for gfsfhr in 12 36 60 84 108
        do
            ecmwffhr=F`expr ${gfsfhr} + 12`
            gfsfhr=F${gfsfhr}
	    
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL= mepowo.gsf
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
TITLE   = 5/-1/~ ? ${MDL} @ HGT (00Z YELLOW)|~${gareas} 00Z VS EC Y 12Z 500 HGT!0
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
TITLE   = 5/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} 00Z VS EC Y 12Z PMSL!0
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
\$MAPFIL= mepowo.gsf
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
LINE    = 3/1/3/2         
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
TITLE   = 3/-1/~ ? ${MDL} @ HGT (00Z YELLOW)|~${gareas} ${MDL}/NAM/NGM 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${namfhr}
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-2/~ ? NAM @ HGT (00Z CYAN)!0
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
LINE    = 3/1/3/2
HILO    = 3/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 3/-1/~ ? ${MDL} PMSL (00Z YELLOW) |~${gareas} ${MDL}/NAM/NGM PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${namfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-2/~ ? NAM PMSL (CYAN)!0
l
run

EOF
export err=$?;err_chk
        done
    done
fi

if [ ${cyc} -eq 18 ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in US NP
    do
        if [ ${gareas} = US ] ; then
            garea="bwus"
            proj=" "
            latlon="0"
        elif [ ${gareas} = NP ] ; then
            garea="5;-177;45;-72"
            proj="STR/90.0;-155.0;0.0"
            latlon="1/1/1/1/10"
        fi
        for runtime in 12 06 00 18y
        do
            if [ ${runtime} = "12" ] ; then
                cyc2="12"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/1200"
                add="06"
                testgfsfhr="120"
            elif [ ${runtime} = "06" ] ; then
                cyc2="06"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/0600"
                add="12"
                testgfsfhr="114"
            elif [ ${runtime} = "00" ] ; then
                cyc2="00"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/0000"
                add="18"
                testgfsfhr="108"
            elif [ ${runtime} = "18y" ] ; then
                cyc2="18"
                desc="Y"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1800"
                add="24"
                testgfsfhr="102"
            fi   
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z ${desc} CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z ${desc} CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126
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
\$MAPFIL= mepowo.gsf
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

if [ ${cyc} -eq 06 ] ; then
    grid="F-${MDL} | ${PDY2}/${cyc}00"
    for gareas in US NP
    do
        if [ ${gareas} = US ] ; then
            garea="bwus"
            proj=" "
            latlon="0"
        elif [ ${gareas} = NP ] ; then
            garea="5;-177;45;-72"
            proj="STR/90.0;-155.0;0.0"
            latlon="1/1/1/1/10"
        fi
        for runtime in 00 18 12 06
        do
            if [ ${runtime} -eq 00 ] ; then
                cyc2="00"
                desc="T"
                grid2="F-${MDL} | ${PDY2}/0000"
                add="06"
                testgfsfhr="120"
            elif [ ${runtime} -eq 18 ] ; then
                cyc2="18"
                desc="Y"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1800"
                add="12"
                testgfsfhr="114"
            elif [ ${runtime} -eq 12 ] ; then
                cyc2="12"
                desc="Y"
                export HPCGFS=${COMINgempak}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/1200"
                add="18"
                testgfsfhr="108"
            elif [ ${runtime} -eq 06 ] ; then
                cyc2="06"
                desc="Y"
                export HPCGFS=${COMINgempak}/${NET}/${envir}/${mdl}.${PDYm1}/${cyc2}/gempak

                grid2="F-GFSHPC | ${PDY2m1}/0600"
                add="24"
                testgfsfhr="102"
            fi   
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            hilo3="5/H#;L#//5/5;5/y"
            hilo4="5/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${cyc2}Z ${desc} CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL!6/-3/~ ? ${MDL} PMSL (${cyc2}Z ${desc} CYAN)"
            title3="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z 500 HGT"
            title4="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${gareas} ${cyc}Z VS ${desc} ${cyc2}Z PMSL"
            for gfsfhr in 00 06 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126
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
\$MAPFIL= mepowo.gsf
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
    if [ $fhr -eq 126 ] ; then
     ${DBNROOT}/bin/dbn_alert MODEL GFS_METAFILE_LAST $job \
       ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
    fi
   fi
fi


exit
