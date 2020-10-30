#!/bin/sh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exuma2p5_gsianl.sh
# Script description:  Runs regional GSI variational analysis
#
# Author:        Manuel Pondeca      Org: NP22        Date: 2006-06-23
#
# Script history log:
# 2006-06-23 Manuel Pondeca
#
################################################################################

set -x

cd $DATA

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

. $COMIN/${RUN}.t${cyc}z.envir.sh

# Set resoltion and other dependent parameters
export JCAP=${JCAP:-62}
export LEVS=${LEVS:-60}
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}

cp $COMIN/${RUN}.t${cyc}z.gridname_input gridname_input


# Copy in the prepbufr file
if [ -s $COMURMA/urma.t${cyc}z.prepbufr.tm00 ]
  then
    echo `ls -l $COMURMA/urma.t${cyc}z.prepbufr.tm00`
    cp $COMURMA/urma.t${cyc}z.prepbufr.tm00 prepbufr #this is the observation input file. Must know
                                                     #where it's stored. Comes from Dennis Keyser's job
    echo `ls -l prepbufr`

#   cp $COMURMA/urma.t${cyc}z.satwnd.tm00.bufr_d ./satwnd  #use only if conventional data also available
    cp /com2/${NET}/prod/${NET}.$PDY/urma.t${cyc}z.satwnd.tm00.bufr_d ./satwnd  #use only if conventional data also available


    if [ -s $DCOMIN/urma.t${cyc}z.goessky.tm00.bufr_d ] #use only if conventional data also available
      then
        echo `ls -l $DCOMIN/urma.t${cyc}z.goessky.tm00.bufr_d`
        cp $DCOMIN/urma.t${cyc}z.goessky.tm00.bufr_d goessky
        cp $DCOMIN/urma.t${cyc}z.goessky.tm00.bufr_d $COMURMA #just to help with runhistory
        echo `ls -l goessky`
    else
       echo "***Warning: $DCOMIN/urma.t${cyc}z.goessky.tm00.bufr_d opportunity data is not available ..."
    fi

    if [[ $cyc == $cyc_mitm ]]  ; then
       if [ -s $COMURMA2/${NET}.${PDY}.mintobs.dat ] #use only if conventional data also available
         then
           cp $COMURMA2/${NET}.${PDY}.mintobs.dat mitmdat
        else
          echo "***Warning: minT observation file $COMURMA2/${NET}.${PDY}.mintobs.dat is not available ..."
       fi
    fi

    if [[ $cyc == $cyc_mxtm ]]  ; then
       if [ -s $COMURMA2m1/${NET}.${PDYm1}.maxtobs.dat ] #use only if conventional data also available
         then
           cp $COMURMA2m1/${NET}.${PDYm1}.maxtobs.dat mxtmdat
        else
          echo "***Warning: maxT observation file $COMURMA2m1/${NET}.${PDYm1}.maxtobs.dat is not available ..."
       fi
    fi

   else
    echo "prepbufr file not found"
    echo "fatal error"
    err_exit
fi

# Define the Analysis Date
adate=$PDY$cyc

#   for plotting correlations, set an_test_output = .true.
#   variable names expected for var_plotcor are
#    st  -- stream function
#    vp  -- velocity potential
#    ps  -- surface pressure
#    tv  -- virtual temperature
#    q   -- specific humidity
#    oz  -- ozone
#    sst -- sea surface temperature
#    stl -- skin temp over land
#    sti -- skin temp over ice
#    cw  -- cloud water
#
#   coordinates of desired correlation test point are
#
#      i_plotcor, j_plotcor, k_plotcor
#

# Make gsi namelist

nhr_assimilation=6
min_offset=180

cat << EOF > gsiparm.anl
 &SETUP
   miter=3,niter(1)=50,niter(2)=50,niter(3)=25
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,write_diag(4)=.true.
   gencode=78,qoption=1,tsensible=.true.,lgschmidt=.false.,
   factqmin=1.0,factqmax=1.0,factv=0.1,factcldch=0.1,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,diag_aero=.false.,
   nhr_assimilation=${nhr_assimilation},min_offset=${min_offset},
   use_prepb_satwnd=.false.
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=1.,nord_f2a=4,
   rtma_subdomain_option=.true.,triad4=.true.,nsmooth=0,nsmooth_shapiro=0,lreadnorm=.true.,
 /
 &JCOPTS
 /
 &STRONGOPTS
   nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.true.,
   c_varqc=0.02,vadfile='prepbufr',hilbert_curve=.true.,
   buddycheck_t=.true.,buddydiag_save=.true.,njqc=.true.,vqc=.false.,
 /
 &OBS_INPUT
 dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=3.0,
 /
OBS_INPUT::
!  dfile          dtype     dplat     dsis     dval    dthin  dsfcalc
   prepbufr       ps        null      ps       1.0     0      0
   prepbufr       t         null      t        1.0     0      0
   prepbufr       q         null      q        1.0     0      0
   prepbufr       uv        null      uv       1.0     0      0
   satwnd         uv        null      uv       1.0     0      0
   prepbufr       spd       null      spd      1.0     0      0
   prepbufr       wspd10m   null      wspd10m  1.0     0      0
   satwnd         wspd10m   null      wspd10m  1.0     0      0
   prepbufr       gust      null      gust     1.0     0      0
   prepbufr       vis       null      vis      1.0     0      0
   prepbufr       tcamt     null      tcamt    1.0     0      0
   goessky        tcamt     null      tcamt    1.0     0      0
   prepbufr       cldch     null      cldch    1.0     0      0
   mitmdat        mitm      null      mitm     1.0     0      0
   mxtmdat        mxtm      null      mxtm     1.0     0      0
::
 &SUPEROB_RADAR
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
/
 &SINGLEOB_TEST
   maginnov=5.,magoberr=1.,oneob_type='v',
   oblat=36.,oblon=264.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
EOF
cat << EOF > parmcard_input
&parmcardanisof
    latdepend=.false.,
    scalex1=1.,
    scalex2=1.,
    scalex3=1.,
    afact0=1.,
    hsteep=500.,
    lsmoothterrain=.true.,
    hsmooth_len=0.5,
    volpreserve=.false.,
    lwater_scaleinfl=.true.,
    water_scalefactpsi=1.,
    water_scalefactchi=1.,
    water_scalefacttemp=2.,
    water_scalefactq=2.,
    water_scalefactpsfc=1.,
    water_scalefactgust=2.,
    water_scalefactvis=2.,
    water_scalefactpblh=2.,
    water_scalefacttcamt=1.,
    water_scalefactlcbas=1.,
    nhscale_pass=1,
    rltop_wind=50000.,
    rltop_temp=800.,
    rltop_q=800.,
    rltop_psfc=800.,
    rltop_gust=1400.,
    rltop_vis=2000.,
    rltop_pblh=1200.,
    rltop_wspd10m=1400.,
    rltop_td2m=800.,
    rltop_mxtm=800.,
    rltop_mitm=800.,
    rltop_pmsl=800.,
    rltop_howv=50000000.,
    rltop_tcamt=5000.,
    rltop_lcbas=10000.,
    rltop_cldch=5000.,
    svpsi=0.21,
    svchi=0.2888,
    svpsi_w=0.21,
    svchi_w=0.2888,
    svpsfc=0.7,
    svtemp=0.6666,
    svshum=0.75,
    svgust=1.0,
    svgust_w=1.0,
    svvis=0.25,
    svpblh=1.0,
    svwspd10m=1.0,
    svwspd10m_w=1.0,
    svtd2m=0.6666,
    svmxtm=0.6666,
    svmitm=0.6666,
    svpmsl=0.7,
    svhowv=1.0,
    svtcamt=1.0,
    svtcamt_w=1.0,
    svlcbas=1.0,
    svlcbas_w=1.0,
    svcldch=0.25,
    sclpsi=0.324,
    sclchi=0.324,
    sclpsi_w=0.4725,
    sclchi_w=0.4725,
    sclpsfc=0.324,
    scltemp=0.54,
    sclhum=0.54
    sclgust=0.463,
    sclgust_w=0.6752,
    sclvis=0.60,
    sclpblh=0.45,
    sclwspd10m=0.463,
    sclwspd10m_w=0.6752,
    scltd2m=0.54,
    sclmxtm=0.54,
    sclmitm=0.54,
    sclpmsl=0.324,
    sclhowv=1.0
    scltcamt=0.5,
    scltcamt_w=1.0,
    scllcbas=1.0,
    scllcbas_w=1.0
    sclcldch=1.0,
/
&parmcardreadprepb
    cgrid='cohresext',
    ladjusterr=.false.,
    oberrinflfact=5.0,
    ineighbour=3,
    jneighbour=3,
    lshoreline=.false.,
    slmland=0.9,
    valleygcheck=.true.
/
&parmcardhcurve
    random_cvgrp=.true.,
    usagecv=3.,
    ngrps_tob=5,
    ngrps_qob=5,
    ngrps_psob=5,
    ngrps_uvob=8,
    ngrps_gustob=8,
    ngrps_visob=8,
    ngrps_mitmob=8,
    ngrps_mxtmob=8
/
EOF

#cat << EOF > oberrors_for_maxtmint_input
#&oberrors_for_maxtmint
#   maxtmint_oberrors(180)=1.0,
#   maxtmint_oberrors(181)=1.0,
#   maxtmint_oberrors(182)=1.0,
#   maxtmint_oberrors(183)=1.2,
#   maxtmint_oberrors(184)=4.0,
#   maxtmint_oberrors(185)=4.0,
#   maxtmint_oberrors(186)=4.0,
#   maxtmint_oberrors(187)=1.0,
#   maxtmint_oberrors(188)=1.2,
#   maxtmint_oberrors(189)=4.0,
#   maxtmint_oberrors(192)=1.0,
#   maxtmint_oberrors(193)=1.0,
#   maxtmint_oberrors(194)=1.2,
#   maxtmint_oberrors(195)=1.2,
#   maxtmint_oberrors(196)=1.2,
#   maxtmint_oberrors(197)=1.2,
#   maxtmint_oberrors(198)=1.2,
#   maxtmint_oberrors(199)=1.2
#/
#EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = OPTRAN spectral coefficients
#   trncoef  = OPTRAN transmittance coefficients
#   emiscoef = coefficients for IR sea surface emissivity model
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

if [[ $cyc == $cyc_mitm ]]  ; then
   cp $FIXurma/urma2p5_anavinfo_mitm anavinfo
 elif [[ $cyc == $cyc_mxtm ]]  ; then
   cp $FIXurma/urma2p5_anavinfo_mxtm anavinfo
 else
   cp $FIXurma/urma2p5_anavinfo anavinfo
fi
cp $FIXurma/urma2p5_regional_berror.f77 berror_stats
cp $FIXurma/urma2p5_errtable.r3dv errtable    #error table if using ECMWF varqc

cp $FIXurma/urma2p5.prepobs_errtable_pw.njqc errtable_pw

cp $FIXurma/urma2p5.prepobs_errtable_t.njqc  errtable_t
cp $FIXurma/urma2p5.prepobs_errtable_q.njqc  errtable_q
cp $FIXurma/urma2p5.prepobs_errtable_ps.njqc errtable_ps
cp $FIXurma/urma2p5.prepobs_errtable_uv.njqc errtable_uv
cp $FIXurma/urma2p5.nlqc_b_ps.njqc btable_ps
cp $FIXurma/urma2p5.nlqc_b_t.njqc  btable_t
cp $FIXurma/urma2p5.nlqc_b_q.njqc  btable_q
cp $FIXurma/urma2p5.nlqc_b_uv.njqc btable_uv

cp $FIXurma/urma2p5_convinfo.txt convinfo
cp $FIXurma/urma2p5_mesonet_uselist.txt mesonetuselist
cp $FIXurma/urma2p5_ruc2_wind-uselist-noMETAR_AUGMENTED.dat mesonet_stnuselist
cp $FIXurma/urma2p5_wbinuselist wbinuselist
cp $FIXurma/urma2p5_t_day_rejectlist t_day_rejectlist
cp $FIXurma/urma2p5_t_night_rejectlist t_night_rejectlist
cp $FIXurma/urma2p5_q_day_rejectlist q_day_rejectlist
cp $FIXurma/urma2p5_q_night_rejectlist q_night_rejectlist
cp $COMIN/${RUN}.t${cyc}z.t_rejectlist t_rejectlist
cp $COMIN/${RUN}.t${cyc}z.p_rejectlist p_rejectlist
cp $COMIN/${RUN}.t${cyc}z.q_rejectlist q_rejectlist
cp $COMIN/${RUN}.t${cyc}z.w_rejectlist w_rejectlist
if [[ $cyc == $cyc_mitm ]]  ; then
   cp $COMIN/${RUN}.t${cyc}z.mitm_rejectlist mitm_rejectlist
fi
if [[ $cyc == $cyc_mxtm ]]  ; then
   cp $COMIN/${RUN}.t${cyc}z.mxtm_rejectlist mxtm_rejectlist
fi
cp $FIXurma/urma2p5_slmask.dat_nolakes rtma_slmask.dat
cp $FIXurma/urma2p5_terrain.dat rtma_terrain.dat
cp $FIXurma/urma2p5_prepobs_prep.bufrtable prepobs_prep.bufrtable


# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
cp $COMIN/${RUN}.t${cyc}z.2dvar_input  ./wrf_inout

if [[ "$usefgat" = ".true." ]] ; then
    cp $COMIN/${RUN}.t${cyc}z.2dvar_input_bi2     ./wrf_inou2
    cp $COMIN/${RUN}.t${cyc}z.2dvar_input_bi3     ./wrf_inou3

    if [ -s $COMIN/${RUN}.t${cyc}z.2dvar_input_bi4 ]; then
       cp $COMIN/${RUN}.t${cyc}z.2dvar_input_bi4  ./wrf_inou4
    fi

    if [ -s $COMIN/${RUN}.t${cyc}z.2dvar_input_bi5 ]; then
       cp $COMIN/${RUN}.t${cyc}z.2dvar_input_bi5  ./wrf_inou5
    fi
fi

cp $FIXurma/urma2p5_fltnorm.dat_psi       $DATA/fltnorm.dat_psi
cp $FIXurma/urma2p5_fltnorm.dat_chi       $DATA/fltnorm.dat_chi
cp $FIXurma/urma2p5_fltnorm.dat_ps        $DATA/fltnorm.dat_ps
cp $FIXurma/urma2p5_fltnorm.dat_t         $DATA/fltnorm.dat_t
cp $FIXurma/urma2p5_fltnorm.dat_pseudorh  $DATA/fltnorm.dat_pseudorh 
cp $FIXurma/urma2p5_fltnorm.dat_gust      $DATA/fltnorm.dat_gust
cp $FIXurma/urma2p5_fltnorm.dat_vis       $DATA/fltnorm.dat_vis
cp $FIXurma/urma2p5_fltnorm.dat_tcamt     $DATA/fltnorm.dat_tcamt
cp $FIXurma/urma2p5_fltnorm.dat_wspd10m   $DATA/fltnorm.dat_wspd10m
cp $FIXurma/urma2p5_fltnorm.dat_cldch     $DATA/fltnorm.dat_cldch

if [[ $cyc == $cyc_mitm ]]  ; then
   cp $FIXurma/urma2p5_fltnorm.dat_mitm   $DATA/fltnorm.dat_mitm
fi

if [[ $cyc == $cyc_mxtm ]]  ; then
   cp $FIXurma/urma2p5_fltnorm.dat_mxtm   $DATA/fltnorm.dat_mxtm
fi

cp $FIXurma/urma2p5_random_flips           $DATA/random_flips

export pgm=urma2p5_gsianl
. prep_step

mpirun.lsf $EXECurma/urma_gsianl < gsiparm.anl>>$pgmout 2>errfile
export err=$?; err_chk
rc=$?


ls -lrt gradx.dat_01_* | grep -c gradx.dat > used_iterations.dat

if [ $SENDCOM = YES ]
then
   nhr_assimilation=`printf %02d $nhr_assimilation`
   cp wrf_inout $COMOUT/${RUN}.t${cyc}z.wrfanl
   cp siganl $COMOUT/${RUN}.t${cyc}z.siganl
   cp sigf06 $COMOUT/${RUN}.t${cyc}z.sigf06
   if [[ "$usefgat" = ".true." ]] ; then
      cp sigf07 $COMOUT/${RUN}.t${cyc}z.sigf07
      cp sigf08 $COMOUT/${RUN}.t${cyc}z.sigf08
      if [ -s sigf09 ]; then
         cp sigf09 $COMOUT/${RUN}.t${cyc}z.sigf09
      fi
      if [ -s sigf10 ]; then
         cp sigf10 $COMOUT/${RUN}.t${cyc}z.sigf10
      fi
   fi
   cp sigf${nhr_assimilation} $COMOUT/${RUN}.t${cyc}z.sigges
   cp sigfupdate02 $COMOUT/${RUN}.t${cyc}z.sigfupdate02
   cp sigfupdate03 $COMOUT/${RUN}.t${cyc}z.sigfupdate03
   cp bckg_dxdy.dat $COMOUT/${RUN}.t${cyc}z.bckg_dxdy.dat
   cp bckg_qsat.dat $COMOUT/${RUN}.t${cyc}z.bckg_qsat.dat
   cp bckg_psfc.dat $COMOUT/${RUN}.t${cyc}z.bckg_psfc.dat
   cp bckgvar.dat_psi $COMOUT/${RUN}.t${cyc}z.bckgvar_psi.dat
   cp bckgvar.dat_chi $COMOUT/${RUN}.t${cyc}z.bckgvar_chi.dat
   cp bckgvar.dat_ps $COMOUT/${RUN}.t${cyc}z.bckgvar_ps.dat
   cp bckgvar.dat_t $COMOUT/${RUN}.t${cyc}z.bckgvar_t.dat
   cp bckgvar.dat_pseudorh $COMOUT/${RUN}.t${cyc}z.bckgvar_pseudorh.dat
   cp bckgvar.dat_gust $COMOUT/${RUN}.t${cyc}z.bckgvar_gust.dat
   cp bckgvar.dat_vis $COMOUT/${RUN}.t${cyc}z.bckgvar_vis.dat
   cp bckgvar.dat_tcamt $COMOUT/${RUN}.t${cyc}z.bckgvar_tcamt.dat
   cp bckgvar.dat_wspd10m $COMOUT/${RUN}.t${cyc}z.bckgvar_wspd10m.dat
   cp bckgvar.dat_cldch $COMOUT/${RUN}.t${cyc}z.bckgvar_cldch.dat

   cp anavinfo $COMOUT/${RUN}.t${cyc}z.anavinfo

   if [[ $cyc == $cyc_mitm ]]  ; then 
      cp bckgvar.dat_mitm $COMOUT/${RUN}.t${cyc}z.bckgvar_mitm.dat
   fi

   if [[ $cyc == $cyc_mxtm ]]  ; then
      cp bckgvar.dat_mxtm $COMOUT/${RUN}.t${cyc}z.bckgvar_mxtm.dat
   fi

   cp bckg_z.dat $COMOUT/${RUN}.t${cyc}z.bckg_z.dat
   cp used_iterations.dat $COMOUT/${RUN}.t${cyc}z.used_iterations.dat

   cp tobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.tobs_allcv_groups
   cp qobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.qobs_allcv_groups
   cp psobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.psobs_allcv_groups
   cp uvobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.uvobs_allcv_groups
   cp gustobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.gustobs_allcv_groups
   cp visobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.visobs_allcv_groups
   cp tcamtobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.tcamtobs_allcv_groups
   cp cldchobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.cldchobs_allcv_groups
   if [[ $cyc == $cyc_mitm ]]  ; then
      cp mitmobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.mitmobs_allcv_groups
   fi
   if [[ $cyc == $cyc_mxtm ]]  ; then 
      cp mxtmobs_allcv_groups $COMOUT/${RUN}.t${cyc}z.mxtmobs_allcv_groups
   fi

nfiles=`cat used_iterations.dat`
   ic=0
   while [ $ic -le $(($nfiles-1)) ] ; do
       fic=`printf %03d $ic`
       cp gradx.dat_01_${fic} $TMPDATA/${RUN}.t${cyc}z.gradx.dat_01_${fic}
       cp grady.dat_01_${fic} $TMPDATA/${RUN}.t${cyc}z.grady.dat_01_${fic}
       let "ic=ic+1"
   done

   cp prepbufr $COMOUT/${RUN}.t${cyc}z.prepbufr
   chgrp rstprod $COMOUT/${RUN}.t${cyc}z.prepbufr
   cp satwnd $COMOUT/${RUN}.t${cyc}z.satwnd
   chgrp rstprod $COMOUT/${RUN}.t${cyc}z.satwnd
   cp goessky $COMOUT/${RUN}.t${cyc}z.goessky
   chgrp rstprod $COMOUT/${RUN}.t${cyc}z.goessky
   if [[ $cyc == $cyc_mitm ]]  ; then
      cp mitmdat $COMOUT/${RUN}.t${cyc}z.${NET}.${PDY}.mintobs.dat
      chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${NET}.${PDY}.mintobs.dat
   fi
   if [[ $cyc == $cyc_mxtm ]]  ; then
      cp mxtmdat $COMOUT/${RUN}.t${cyc}z.${NET}.${PDYm1}.maxtobs.dat
      chgrp rstprod $COMOUT/${RUN}.t${cyc}z.${NET}.${PDYm1}.maxtobs.dat
   fi
   cp fort.201 $COMOUT/${RUN}.t${cyc}z.pressfcfits
   cp fort.202 $COMOUT/${RUN}.t${cyc}z.windfits
   cp fort.203 $COMOUT/${RUN}.t${cyc}z.tempfits
   cp fort.204 $COMOUT/${RUN}.t${cyc}z.shumidfits
   cp fort.218 $COMOUT/${RUN}.t${cyc}z.gustfits
   cp fort.219 $COMOUT/${RUN}.t${cyc}z.visfits
   cp fort.229 $COMOUT/${RUN}.t${cyc}z.tcamtfits
   cp fort.231 $COMOUT/${RUN}.t${cyc}z.cldchfits

   if [[ $cyc == $cyc_mitm ]]  ; then
      cp fort.226 $COMOUT/${RUN}.t${cyc}z.mitmfits
   fi

   if [[ $cyc == $cyc_mxtm ]]  ; then
      cp fort.225 $COMOUT/${RUN}.t${cyc}z.mxtmfits
   fi

   cp fort.220 $COMOUT/${RUN}.t${cyc}z.penaltyinfo

   cp gsiparm.anl $COMOUT/${RUN}.t${cyc}z.gsiparm.anl
   cp parmcard_input $COMOUT/${RUN}.t${cyc}z.parmcard_input
   cp valley_map.dat $COMOUT/${RUN}.t${cyc}z.valley_map.dat
fi

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

echo "Time before diagnostic loop is `date` "

loops="01 02 03 04"
for loop in $loops; do

case $loop in
  01) string=ges;;
  04) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="conv"
   for type in $listall; do
      count=`ls ${DATA}/pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat ${DATA}/pe*.${type}_${loop}* > ${DATA}/diag_${type}_${string}
         gzip -S .Z diag_${type}_${string}
         if [ $SENDCOM = YES ]; then
            cp ${DATA}/diag_${type}_${string}.Z $COMOUT/${RUN}.t${cyc}z.diag_${type}_${string}.Z
            chgrp rstprod $COMOUT/${RUN}.t${cyc}z.diag_${type}_${string}.Z
         fi
      fi
   done
done

echo "Time after diagnostic loop is `date` "
########################################################

msg='$job ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
################## END OF SCRIPT #######################
