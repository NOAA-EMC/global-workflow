#! /bin/sh
#
# Metafile Script :  gfs_meta_opc_sw_vgf.sh
#
# Log :
# J.L. Partain/MPC	12/5/97	Original creation - create VGF files of AVN guidance for 
#                               24-hr SFC progs for ATL and PAC
# J.L. Partain/MPC	12/11/97 Modified 06z and 18z runs to produce 30-hr guidance
# J.L. Partain/MPC      12/12/97 smoother contours & better overlay characteristics
# J.L. Partain/MPC       6/6/98 Chg map bg for PAC to match IG
# J.Partain/MPC          6/26/98 Chg smoothing to get less vertices
# J.Partain/MPC          7/22/98 Chg EPAC background 
# J.Partain/MPC          8/21/98 Add AVN wind barb VGF
# J.Partain/MPC          11/4/98 Chg from 5 to 9-pt smoother for PMSL
# J.Partain/MPC		 1/20/99 Setup to run from hpcops rather than hp47:modtdg
# J.L.Partain/MPC        2/23/99 cp VGF files to hp68 for backup
# J.L.Partain/MPC        2/24/99 add smoothing attr; chg contour labels
# J.L.Partain/MPC	10/05/99 Y2K CHECK COMPLETE (for 10/15 model name changes)
# J.L.Partain/MPC	09/29/00 ready for ops on IBM in mpcops account
# J.L.Partain/MPC	10/03/00 make 2-digit years for VG filenames
# J.L.Partain/MPC	01/02/01 chg garea and proj for epac
# J.L.Partain/MPC	05/07/01 add ftp to hp43
# J.L.Partain/MPC	05/22/01 chgs to run on either asp or bsp
# J.L.Partain/MPC	06/12/02 extend 4swpac area to include AK
# C.Janota/OPC		01/13/04 changed ftp hp41 to scp opcpr
# C.Janota/OPC		01/16/04 changed ftp hp68 to scp opcol
# C.Janota/OPC		01/16/04 changed ftp hp49 to scp opcar
# C.Janota/OPC		01/30/04 changed hp43 to opcbkup
# J. Carr/PMB           12/02/04 Pushed into production.
# C.Janota/OPC		10/14/05 changed 4sw garea
#
# Set up Local Variables
#
set -x
#
export PS4='OPC_SW_VGF:$SECONDS + '
workdir="${DATA}/OPC_SW_VGF"
mkdir -p -m 775  ${workdir}
cd ${workdir}

cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
PDY2=`echo $PDY | cut -c3-`

if [ ${cyc} -eq 06 ] ; then
    fhr="030"
elif [ ${cyc} -eq 18 ] ; then
    fhr="030"
else
    fhr="024"
fi

atlfilename="${mdl}_${PDY2}_${cyc}_4swatl.vgf"
pacfilename="${mdl}_${PDY2}_${cyc}_4swpac.vgf"
atlwindname="${mdl}_${PDY2}_${cyc}_4wwatl_wind.vgf"
pacwindname="${mdl}_${PDY2}_${cyc}_4wwpac_wind.vgf"

device1="vg|${atlfilename}"
device2="vg|${pacfilename}"
device3="vg|${atlwindname}"
device4="vg|${pacwindname}"

export pgm=gdplot2_vg;. prep_step; startmsg

$GEMEXE/gdplot2_vg << EOFplt
GDFILE    = F-${MDL} | ${PDY2}/${cyc}00
GDATTIM   = F${fhr}
GAREA     = 25;-84;46;-38
PROJ      = str/90;-67;1
LATLON    = 
MAP       = 0
CLEAR     = y
DEVICE    = ${device1}
GLEVEL    = 0
GVCORD    = none
PANEL     = 0
SKIP      = 0
SCALE     = 0
GDPFUN    = sm9s(pmsl)
TYPE      = c
CONTUR    = 0
CINT      = 4
LINE      = 5/1/3/-5/2/.13
FINT      = 
FLINE     = 
!HILO     = 7;7/h#;l#
!HLSYM    = 1//21/1/hw
HILO      =
HLSYM     =
CLRBAR    = 0
WIND      = 
REFVEC    =
TITLE     =
TEXT      = 1.3/21/2/121/c/s/hw
li
ru

GAREA	= 4;135;89;-102
PROJ    = ced
DEVICE  = ${device2}
CLEAR   = Y
li
run

GAREA   = 25;-84;46;-38
PROJ    = str/90;-67;1
DEVICE  = ${device3}
CLEAR   = Y
SKIP    = /-4;1
GDPFUN  = vmsk(kntv(wnd@9950%sgma),sea)
TYPE    = b
CONTUR  = 0
CINT    =
LINE    =
FILTER  = n
HILO    = 0
HLSYM   =
WIND    = bk25/1.6/803/214
l
run

GAREA   = 12;-134;69;-132
PROJ    = str/90;-100;1
DEVICE  = ${device4}
SKIP	= /-10;1
WIND    = bk25/1.6/803/214
CLEAR   = Y
l
run
exit
EOFplt

export err=$?;err_chk
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

export DBN_ALERT_TYPE=VGF
export DBN_ALERT_SUBTYPE=OPC

if [ $SENDCOM = "YES" ] ; then
   mv *.vgf ${COMOUT}
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_4swatl.vgf
      ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_4swpac.vgf
      ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_4wwatl_wind.vgf
      ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_4wwpac_wind.vgf
   fi
fi

exit
