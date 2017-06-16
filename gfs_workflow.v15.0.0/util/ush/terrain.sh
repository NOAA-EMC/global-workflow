#!/bin/ksh
set -x

#--------------------------------------------------------------------------------
# Create GFS orography, land-sea mask, mountain variance and other terrain features
# 2014.05 -- Source from .svnemc./projects/gfs/branches/moorthi/para/util/oro_etc
# 2014.05 -- F. YANG  merged two scripts into one, reorganized for including this 
#                     tool in GFS util directory.  Currently only works on WCOSS.
# 2016.01 -- Shrivinas Moorthi, added output in binary format; added Cubic and 
#                     Quadratic grid.
#--------------------------------------------------------------------------------
# to execute the script, run either interactively or submit as a batch job
# /u/Fanglin.Yang/bin/sub_wcoss -a GFS-T2O -q dev2 -p 1/1/N -r 27000/1 -t 06:00:00 -o terrain.out terrain.sh
#--------------------------------------------------------------------------------

export WORKFLOW=${WORKFLOW:-/global/noscrub/emc.glopara/svn/gfs/q3fy17/gfs_workflow.v14.1.0/para}
export UTILDIR=$WORKFLOW/util
export GLOBAL_SHARED=${GLOBAL_SHARED:-/global/noscrub/emc.glopara/svn/gfs/q3fy17/global_shared.v14.0.0}
export FIXDIR=$GLOBAL_SHARED/fix/fix_am           #lonsperlat dir

export TERRAINSH=$UTILDIR/ush/ml7_slm30g.sh
export TERRAINEXEC=$UTILDIR/exec/terrain.x          

#export OCLSM=oclsm         #use ocean land/sea mask option, must have txt file from ocean
#export TERRAINEXEC=$UTILDIR/exec/terrain_oclsm.x    

export ptmp=/ptmpp1
export RUNDIR=$ptmp/$LOGNAME/terrain
export SAVEDIR=$ptmp/$LOGNAME/terrain/save
mkdir -p $RUNDIR $SAVEDIR

#--examples of quadratic grids
# jcap="62 126 170 190 254 382 574 878"
#--examples of linear grids
# jcap="L92 L126 L254 L382 L574 L878 L1148 L1534"
#--examples of Cubic Gris   
# jcap="C1150 C1534"

#----------------------------
export jcaplist="L92 L126 L254 L382 L574 L670 L1534"
for jcapl in $jcaplist ; do
#----------------------------
 nc=$(echo $jcapl | wc -c)
 fc=$(echo $jcapl | cut -c1-1)
 if [ $fc == L ] ; then
  export jcap=$(echo $jcapl | cut -c2-$((nc-1)))
  echo ' Using linear grid for JCAP=' $jcap
 elif [ $fc == Q ] ; then
  export jcap=$(echo $jcapl | cut -c2-$((nc-1)))
  echo ' Using quadratic grid for JCAP=' $jcap
 elif [ $fc == C ] ; then
  export jcap=$(echo $jcapl | cut -c2-$((nc-1)))
  echo ' Using cubic grid for JCAP=' $jcap
 else
  export jcap=$jcapl
 fi
 echo $jcap

export red_grd=YES   #compute stats on reduced grid?
if [ $red_grd = YES ] ; then 
 export SUFIN='.rg'
else
 export LONSPERLAT=/dev/null
 export SUFIN=""
fi
#
#for no filter, set filt1=jcap+1 and filt2=filt1+1

#......................
if [ $fc = L ] ; then
#......................
 if [ $jcap -eq 92 ] ; then
  export lonb=192 ; export latb=94 ; export filt1=42 ; export filt2=64

 elif [ $jcap -eq 126 ] ; then
  export lonb=384 ; export latb=190 ; export filt1=84 ; export filt2=128

 elif [ $jcap -eq 254 ] ; then
  export lonb=512 ; export latb=256 ; export filt1=128 ; export filt2=172

 elif [ $jcap -eq 382 ] ; then
   export lonb=768 ; export latb=384 ; export filt1=256 ; export filt2=384

 elif [ $jcap -eq 510 ] ; then
   export lonb=1024 ; export latb=512 ; export filt1=384 ; export filt2=512

 elif [ $jcap -eq 670 ] ; then
   export lonb=1344 ; export latb=672 ; export filt1=384 ; export filt2=672

 elif [ $jcap -eq 574 ] ; then
   export lonb=1152 ; export latb=576 ; export filt1=384 ; export filt2=576

 elif [ $jcap -eq 878 ] ; then
   export lonb=1760 ; export latb=880 ; export filt1=384 ; export filt2=880

 elif [ $jcap -eq 1148 ] ; then
   export lonb=2304 ; export latb=1152 ; export filt1=384 ; export filt2=1148

 elif [ $jcap -eq 1500 ] ; then
   export lonb=3072 ; export latb=1536 ; export filt1=576 ; export filt2=$jcap

 elif [ $jcap -eq 1534 ] ; then
   export lonb=3072 ; export latb=1536 ; export filt1=576 ; export filt2=$jcap
 fi
#......................
else                       
#......................
 if [ $jcap -eq 62 ] ; then
  export lonb=192 ; export latb=94 ; export filt1=42 ; export filt2=64
# export lonb=192 ; export latb=94 ; export filt1=64 ; export filt2=64

 elif [ $jcap -eq 126 ] ; then
  export lonb=384 ; export latb=190 ; export filt1=84 ; export filt2=128

 elif [ $jcap -eq 170 ] ; then
  export lonb=512 ; export latb=256 ; export filt1=128 ; export filt2=172

 elif [ $jcap -eq 190 ] ; then
  export lonb=576 ; export latb=288 ; export filt1=128 ; export filt2=192

 elif [ $jcap -eq 254 ] ; then
  export lonb=768 ; export latb=384 ; export filt1=192 ; export filt2=256

 elif [ $jcap -eq 382 ] ; then
  #export lonb=1152 ; export latb=576 ; export filt1=0 ; export filt2=0
  export lonb=1152 ; export latb=576 ; export filt1=256 ; export filt2=384

 elif [ $jcap -eq 510 ] ; then
  export lonb=1536 ; export latb=766 ; export filt1=384 ; export filt2=512

 elif [ $jcap -eq 574 ] ; then
  export lonb=1760 ; export latb=880 ; export filt1=384 ; export filt2=576

 elif [ $jcap -eq 878 ] ; then
  export lonb=2640 ; export latb=1320 ; export filt1=384 ; export filt2=880

 elif [ $jcap -eq 1148 ] ; then
   export lonb=3456 ; export latb=1728 ; export filt1=384 ; export filt2=1148
 fi
#......................
fi
#......................

export lin=".$lonb.$latb"
export WRKDIR=$RUNDIR/terr_$jcap.$lonb.${latb}
mkdir -p $WRKDIR                
cd $WRKDIR || exit 8                  

string=t$jcap.$lonb.${latb}$SUFIN
export slmgb=global_slmask.$string.grb
export orogb=global_orography.$string.grb
export orogb_uf=global_orography_uf.$string.grb
export mtnvar14=global_mtnvar.$string.f77
export oro_bin=global_orography.$string.f77
export oro_bin_uf=global_orography_uf.$string.f77
export slm_bin=global_slmask.$string.f77


#export FIX_TERR=$UTILDIR/fix
export FIX_TERR=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/fv3gfs/fix/fix_orog
export MTNDIR=$FIX_TERR
export MTN_SLM=TOP8M_slm.80I1.asc
export HIRES_TERR=$FIX_TERR/thirty.second.antarctic.new.bin
export FINE_TERR=$FIX_TERR/gtopo30_gg.fine
export LANDCOVER30=$FIX_TERR/landcover30.fixed
export a_ocean_mask=$FIX_TERR/a_ocean_mask${lonb}x${latb}.txt

$TERRAINSH $slmgb $orogb $mtnvar14 $lonb $latb $jcap $filt1 $filt2 14 $orogb_uf $oro_bin $oro_bin_uf $slm_bin> $WRKDIR/out_$jcap.out

cp -p $WRKDIR/global_*                                       $SAVEDIR/.
cp -p $FIXDIR/global_lonsperlat.t${jcap}.${lonb}.${latb}.txt $SAVEDIR/.
#----------------------------
done
#----------------------------
exit

