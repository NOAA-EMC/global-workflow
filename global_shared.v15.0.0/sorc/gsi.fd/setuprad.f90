   subroutine setuprad(lunin,mype,aivals,stats,nchanl,nreal,nobs,&
     obstype,isis,is,rad_diagsave,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprad    compute rhs of oi equation for radiances
!   prgmmr: derber           org: np23                date: 1995-07-06
!
! abstract: read in data, first guess, and obtain rhs of oi equation
!        for radiances.
!
! program history log:
!   1995-07-06  derber
!   1996-11-xx  wu, data from prepbufr file
!   1996-12-xx  mcnally, changes for diagnostic file and bugfix
!   1998-04-30  weiyu yang    mpi version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - remove sigma assumptions (use pressure)
!   2004-05-28  kleist - subroutine call update
!   2004-06-17  treadon - update documenation
!   2004-07-23  weng,yan,okamoto - incorporate MW land and snow/ice emissivity
!                                  models for AMSU-A/B and SSM/I
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - modifications for nonlinear qc
!   2004-10-15  derber  - modify parts of IR quality control
!   2004-10-28  treadon - replace parameter tiny with tiny_r_kind
!   2004-11-22  derber  - remove weight, add logical for boundary point
!   2004-11-30  xu li   - add SST physical retrieval algorithm
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-01-20  okamoto - add ssm/i radiance assimilation 
!   2005-01-22  okamoto - add TB jacobian with respect to ocean surface wind 
!                         through MW ocean emissivity model
!   2005-02-22  derber  - alter surface determination and improve quality control
!   2005-02-28  treadon - increase size of character variable holding diagnostic 
!                         file name
!   2005-03-02  derber  - modify use of surface flages and quality control 
!                         and adjoint of surface emissivity
!   2005-03-04  xu li   - restructure code related to sst retrieval
!   2005-03-07  todling,treadon - place lower bound on sum2
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-04-18  treadon - modify sections of code related to sst retrieval
!   2005-06-01  treadon - add code to load/use extended vertical profile arrays in rtm
!   2005-07-06  derber  - modify for mhs and hirs/4
!   2005-07-29  treadon - modify tnoise initialization; add varinv_use
!   2005-09-20  xu,pawlak - modify sections of code related to ssmis
!   2005-09-28  derber  - modify for new radinfo and surface info input from read routines
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-17  treadon - generalize accessing of elements from obs array
!   2005-10-20  kazumori - modify sections of code related to amsre
!   2005-11-04  derber - place lower bound (0.0) on computed clw
!   2005-11-14  li - modify avhrr related code
!   2005-11-18  treadon - correct thin snow test to apply to microwave
!   2005-11-18  kazumori - modify sections of amsre diagnostic file
!   2005-11-29  parrish - remove call to deter_sfc_reg (earlier patch for regional mode)
!   2005-12-16  derber - add check on skin temperature to clw bias correction
!   2005-12-20  derber - add transmittance qc check to mw sensors
!   2006-01-09  treadon - introduce get_ij
!   2006-01-12  treadon - replace pCRTM with CRTM
!   2006-01-31  todling - add obs time to output diag files
!   2006-02-01  liu - add ssu
!   2006-02-02  treadon - rename prsi(l) as ges_prsi(l)
!   2006-02-03  derber - add new obs control and change printed stats
!   2006-03-21  treadon - add optional perturbation to observation
!   2006-03-24  treadon - bug fix - add iuse_rad to microwave channel varinv check
!   2006-04-19  treadon - rename emisjac as dtbduv_on (accessible via obsmod)
!   2006-04-27  derber - remove rad_tran_k, process data one  profile at a time
!                        write data in jppf chunks
!   2006-05-10  derber - add check on maximum number of levels for RT
!   2006-05-30  derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-27  kazumori - modify factor of bc predictor(clw) for AMSR-E
!                          and input of qcssmi
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc and add satellite and solar azimuth angles
!   2006-07-31  kleist  - change call to intrppx, no longer get ps at ob location
!   2006-12-21  sienkiewicz - add 'no85GHz' flag for F8 SSM/I 
!   2007-01-24  kazumori- modify to qcssmi subroutine output and use ret_ssmis
!                         for ssmis_las only (assumed UKMO SSMIS data)
!   2007-03-09      su  - remove the perturbation to the observation
!   2007-03-19  tremolet - binning of observations
!   2007-04-04  wu      - do not load ozone jacobian if running regional mode
!   2007-05-30  h.liu   - replace c1 with constoz in ozone jacobian
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_rad_file
!   2007-06-29  jung    - update CRTM interface
!   2008-01-30  h.liu/treadon - add SSU cell pressure correction block
!   2008-05-21  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-12-07  b.yan   - changed qc for channel 5 (relaxed)
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check(), and
!			  new arguments init_pass and last_pass.
!   2009-12-08  guo     - cleaned diag output rewind with open(position='rewind')
!			- fixed a bug in diag header output while is not init_pass.
!   2010-03-01  gayno - allow assimilation of "mixed" amsua fovs
!   2010-03-30  collard - changes for interface with CRTM v2.0. 
!   2010-03-30  collard - Add CO2 interface (fixed value for now).
!   2010-04-08  h.liu   -add SEVIRI assimilation 
!   2010-04-16  hou/kistler add interface to module ncepgfs_ghg
!   2010-04-29  zhu     - add option newpc4pred for new preconditioning for predictors
!   2010-05-06  zhu     - add option adp_anglebc variational angle bias correction
!   2010-05-13  zhu     - add option passive_bc for bias correction of passive channels
!   2010-05-19  todling - revisit intrppx CO2 handle
!   2010-06-10  todling - reduce pointer check by getting CO2 pointer at this level
!                       - start adding hooks of aerosols influence on RTM
!   2010-07-15  kleist  - reintroduce capability to write out predictor terms (not predicted bias) and
!                         pressure level that corresponds to peak of weighting function
!   2010-07-16  yan     - update quality control of mw water vapor sounding channels (amsu-b and mhs)
!                       - add a new input (tbc) to in call qcssmi(..) and
!                         remove 'ssmis_uas,ssmis_las,ssmis_env,ssmis_img' in call qcssmi(..)
!                         Purpose: to keep the consistent changes with qcssmi.f90
!   2010-08-10  wu      - setup corresponding vegetation types (nmm_to_crtm) for IGBP in regional
!                         parameter nvege_type: old=24, IGBP=20
!   2010-08-17  derber  - move setup input and crtm call to crtm_interface (intrppx) to simplify routine
!   2010-09-30  zhu     - re-order predterms and predbias
!   2010-12-16  treadon - move cbias update before calc_clw
!   2011-02-17  todling - add knob to turn off O3 Jacobian from IR instruments (per Emily Liu's work)
!   2011-03-13  li      - (1) associate nst_gsi and nstinfo (use radinfo) to handle nst fields
!                       - (2) modify to save nst analysis related diagnostic variables
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-05-04  todling - partially merge in Min-Jeong Kim's cloud clear assimilation changes (connect to Metguess)
!   2011-05-16  todling - generalize handling of jacobian matrix entries
!   2011-05-20  mccarty - updated for ATMS
!   2011-06-08  zhu     - move assignments of tnoise_cld values to satinfo file via varch_cld, use lcw4crtm
!   2011-06-09  sienkiewicz - call to qc_ssu needs tb_obs instead of tbc
!   2011-07-10  zhu     - add jacobian assignments for regional cloudy radiance
!   2011-09-28  collard - Fix error trapping for CRTM failures.         
!   2012-05-12  todling - revisit opts in gsi_metguess_get (4crtm)
!   2012-11-02  collard - Use cloud detection channel flag for IR.
!   2013-02-13  eliu    - Add options for SSMIS instruments
!                       - Add two additional bias predictors for SSMIS radiances  
!                       - Tighten up QC checks for SSMIS  

!   2013-02-19  sienkiewicz - add adjustable preweighting for SSMIS bias terms
!   2013-07-10  zhu     - add upd_pred as an update indicator for bias correction coeficitient
!   2013-07-19  zhu     - add emissivity sensitivity predictor for radiance bias correction
!   2013-11-19  sienkiewicz - merge back in changes for adjustable preweighting for SSMIS bias terms
!   2013-11-21  todling - inquire diag-file version using get_radiag
!   2013-12-10  zhu     - apply bias correction to tb_obs for ret_amsua calculation
!   2013-12-21  eliu    - add amsu-a obs errors for allsky condition 
!   2013-12-21  eliu    - add error handling for CLWP calculation for allsky
!   2014-01-17  zhu     - add cld_rbc_idx for bias correction sample to handle cases with cloud 
!                         inconsistency between obs and first guess for all-sky microwave radiance
!   2014-01-19  zhu     - add scattering index calculation, add it as a predictor for allsky
!                       - calculate retrieved clw using bias-corrected tsim 
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-01-31  mkim    - Remove abs(60.0degree) boundary which existed for all-sky MW radiance DA 
!   2014-02-01  mkim    - Move all-sky mw obserr to subroutine obserr_allsky_mw
!   2014-02-05  todling - Remove overload of diagbufr slot (not allowed)
!   2014-04-17  todling - Implement inter-channel ob correlated covariance capability
!   2014-04-27  eliu    - change qc_amsua/atms interface
!   2014-04-27  eliu    - change call_crtm interface to output clear-sky Tb under all-sky condition (optional)
!   2014-04-27  eliu    - add cloud effect calculation for AMSU-A/ATMS under all-sky condition
!   2014-05-29  thomas  - add lsingleradob capability (originally of mccarty)
!   2014-08-01  zhu     - remove scattering index predictor 
!                       - add all-sky obs error adjustment based on scattering index, diff of clw, 
!                         cloud mismatch info, and surface wind speed
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-01-15  zhu     - change amsua quality control interface to apply emissivity sensitivity
!                         screen to all-sky AMSUA and ATMS radiance
!   2015-01-16  ejones  - Added call to qc_gmi for gmi observations
!                       - Added saphir
!   2015-02-12  ejones  - Write gwp to diag file for GMI
!   2015-03-11  ejones  - Added call to qc_amsr2 for amsr2 observations
!   2015-03-23  ejones  - Added call to qc_saphir for saphir observations
!   2015-03-23  zaizhong ma - add Himawari-8 ahi
!   2015-03-31  zhu     - move cloudy AMSUA radiance observation error adjustment to qcmod.f90;
!                         change quality control interface for AMSUA and ATMS.
!   2015-09-04  J.Jung  - Added mods for CrIS full spectral resolution (FSR).
!   2015-09-30  ejones  - Pull AMSR2 sun azimuth and sun zenith angles for passing to quality control,
!                         modify qc_amsr2 function call
!   2016-07-19  W. Gu   - add isis to obs type
!   2016-07-19  W. Gu   - include the dependence of the correlated obs errors on the surface types
!   2016-07-19  kbathmann -move eigendecomposition for correlated obs here
!
!  input argument list:
!     lunin   - unit from which to read radiance (brightness temperature, tb) obs
!     mype    - mpi task id
!     nchanl  - number of channels per obs
!     nreal   - number of pieces of non-tb information per obs
!     nobs    - number of tb observations to process
!     obstype - type of tb observation
!     isis    - sensor/instrument/satellite id  ex.amsua_n15
!     is      - integer counter for number of observation types to process
!     rad_diagsave - logical to switch on diagnostic output (.false.=no output)
!     channelinfo - structure containing satellite sensor information
!
!   output argument list:
!     aivals - array holding sums for various statistics as a function of obs type
!     stats  - array holding sums for various statistics as a function of channel
!! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpeu_util, only: die,perr,getindex
  use kinds, only: r_kind,r_single,i_kind
  use crtm_spccoeff, only: sc
  use radinfo, only: nuchan,tlapmean,predx,cbias,ermax_rad,tzr_qc,&
      npred,jpch_rad,varch,varch_cld,iuse_rad,icld_det,nusis,fbias,retrieval,b_rad,pg_rad,&
      air_rad,ang_rad,adp_anglebc,angord,ssmis_precond,emiss_bc,upd_pred, &
      passive_bc,ostats,rstats,newpc4pred,radjacnames,radjacindxs,nsigradjac
  use gsi_nstcouplermod, only: nstinfo
  use read_diag, only: get_radiag,ireal_radiag,ipchan_radiag
  use guess_grids, only: sfcmod_gfs,sfcmod_mm5,comp_fact10
  use obsmod, only: ianldate,ndat,mype_diaghdr,nchan_total, &
      dplat,dtbduv_on,radhead,radtail,radheadm,radtailm,&
      i_rad_ob_type,obsdiags,obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,&
      dirname,time_offset,lwrite_predterms,lwrite_peakwt,reduce_diag
  use obsmod, only: rad_ob_type
  use obsmod, only: obs_diag,luse_obsdiag,dval_use
  use gsi_4dvar, only: nobs_bins,hr_obsbin,l4dvar
  use gridmod, only: nsig,regional,get_ij
  use satthin, only: super_val1
  use constants, only: quarter,half,tiny_r_kind,zero,one,deg2rad,rad2deg,one_tenth, &
      two,three,cg_term,wgtlim,r100,r10,r0_01,pi
  use jfunc, only: jiter,miter,jiterstart
  use sst_retrieval, only: setup_sst_retrieval,avhrr_sst_retrieval,&
      finish_sst_retrieval,spline_cub
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use crtm_interface, only: init_crtm,call_crtm,destroy_crtm,sensorindex,surface, &
      isatid,itime,ilon,ilat,ilzen_ang,ilazi_ang,iscan_ang,iscan_pos,iszen_ang,isazi_ang, &
      ifrac_sea,ifrac_lnd,ifrac_ice,ifrac_sno,its_sea,its_lnd,its_ice,its_sno,itsavg, &
      ivty,ivfr,isty,istp,ism,isn,izz,idomsfc,isfcr,iff10,ilone,ilate, &
      isst_hires,isst_navy,idata_type,iclr_sky,iclavr,itref,idtw,idtc,itz_tr 
  use clw_mod, only: calc_clw, ret_amsua 
  use qcmod, only: qc_ssmi,qc_seviri,qc_ssu,qc_avhrr,qc_goesimg,qc_msu,qc_irsnd,qc_amsua,qc_mhs,qc_atms,qc_gmi,qc_amsr2,qc_saphir
  use qcmod, only: igood_qc,ifail_gross_qc,ifail_interchan_qc,ifail_crtm_qc,ifail_satinfo_qc,qc_noirjaco3,ifail_cloud_qc
  use qcmod, only: setup_tzr_qc,ifail_outside_range,ifail_scanedge_qc
  use gsi_metguess_mod, only: gsi_metguess_get
  use control_vectors, only: cvars3d
  use oneobmod, only: lsingleradob,obchan,oblat,oblon,oneob_type
  use radinfo, only: radinfo_adjust_jacobian,radinfo_get_rsqrtinv 






  implicit none

! Declare passed variables
  logical                           ,intent(in   ) :: rad_diagsave
  character(10)                     ,intent(in   ) :: obstype
  character(20)                     ,intent(in   ) :: isis
  integer(i_kind)                   ,intent(in   ) :: lunin,mype,nchanl,nreal,nobs,is
  real(r_kind),dimension(40,ndat)   ,intent(inout) :: aivals
  real(r_kind),dimension(7,jpch_rad),intent(inout) :: stats
  logical                           ,intent(in   ) :: init_pass,last_pass    ! state of "setup" processing

! Declare external calls for code analysis
  external:: stop2

! Declare local parameters
  real(r_kind),parameter:: r1e10=1.0e10_r_kind
  character(len=*),parameter:: myname="setuprad"


! Declare local variables
  character(128) diag_rad_file

  integer(i_kind) iextra,jextra,error_status,istat
  integer(i_kind) ich9,isli,icc,iccm,mm1,ixx
  integer(i_kind) m,mm,jc,j,k,i,icw4crtm,ier,nguess
  integer(i_kind) n,nlev,kval,ibin,ioff,ioff0,iii
  integer(i_kind) ii,jj,idiag,inewpc,nchanl_diag
  integer(i_kind) nadir,kraintype,ierrret
  integer(i_kind) ioz,ius,ivs,iwrmype
  integer(i_kind) iqs,iqg,iqh,iqr
  integer(i_kind) iversion_radiag, istatus
  integer(i_kind) isfctype

  real(r_single) freq4,pol4,wave4,varch4,tlap4
  real(r_kind) node 
  real(r_kind) term,tlap,tb_obsbc1
  real(r_kind) drad,dradnob,varrad,error,errinv,useflag
  real(r_kind) cg_rad,wgross,wnotgross,wgt,arg,exp_arg
  real(r_kind) tzbgr,tsavg5,trop5,pangs,cld,cldp
  real(r_kind) cenlon,cenlat,slats,slons,zsges,zasat,dtime
! real(r_kind) wltm1,wltm2,wltm3  
  real(r_kind) ys_bias_sst,cosza,val_obs
  real(r_kind) sstnv,sstcu,sstph,dtp_avh,dta,dqa
  real(r_kind) bearaz,sun_zenith,sun_azimuth
  real(r_kind) sfc_speed,frac_sea,clw,tpwc,sgagl,clwp_amsua,tpwc_amsua,tpwc_guess_retrieval
  real(r_kind) gwp,clw_obs
  real(r_kind) scat,scatp
  real(r_kind) dtsavg,r90,coscon,sincon
  real(r_kind) bias       
  real(r_kind) factch6    

  logical hirs2,msu,goessndr,hirs3,hirs4,hirs,amsua,amsub,airs,hsb,goes_img,ahi,mhs
  logical avhrr,avhrr_navy,lextra,ssu,iasi,cris,seviri,atms
  logical ssmi,ssmis,amsre,amsre_low,amsre_mid,amsre_hig,amsr2,gmi,saphir
  logical ssmis_las,ssmis_uas,ssmis_env,ssmis_img
  logical sea,mixed,land,ice,snow,toss,l_may_be_passive
  logical microwave, microwave_low
  logical no85GHz
  logical in_curbin, in_anybin
  logical lcw4crtm
  logical account_for_corr_obs
  logical,dimension(nobs):: zero_irjaco3_pole

! Declare local arrays

  real(r_single),dimension(ireal_radiag):: diagbuf
  real(r_single),allocatable,dimension(:,:):: diagbufex
  real(r_single),allocatable,dimension(:,:):: diagbufchan

  real(r_kind),dimension(npred+2):: predterms
  real(r_kind),dimension(npred+2,nchanl):: predbias
  real(r_kind),dimension(npred,nchanl):: pred,predchan
  real(r_kind),dimension(nchanl):: obvarinv,utbc,adaptinf,wgtjo 
  real(r_kind),dimension(nchanl):: varinv,varinv_use,error0,errf,errf0
  real(r_kind),dimension(nchanl):: tb_obs,tbc,tbcnob,tlapchn,tb_obs_sdv
  real(r_kind),dimension(nchanl):: tnoise,tnoise_cld
  real(r_kind),dimension(nchanl):: emissivity,ts,emissivity_k
  real(r_kind),dimension(nchanl):: tsim,wavenumber,tsim_bc
  real(r_kind),dimension(nchanl):: tsim_clr,cldeff_obs,cldeff_sim 
  real(r_kind),dimension(nchanl):: cclr,ccld    
  real(r_kind),dimension(nsig,nchanl):: wmix,temp,ptau5
  real(r_kind),dimension(nsigradjac,nchanl):: jacobian
  real(r_kind),dimension(nreal+nchanl,nobs)::data_s
  real(r_kind),dimension(nsig):: qvp,tvp
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind),dimension(nchanl):: weightmax
  real(r_kind),dimension(nchanl):: cld_rbc_idx
  real(r_kind) :: ptau5deriv, ptau5derivmax
  real(r_kind) :: clw_guess,clw_guess_retrieval,clwtmp
! real(r_kind) :: predchan6_save   
  real(r_kind) :: cldeff_obs5
  real(r_kind),dimension(:,:), allocatable :: rsqrtinv

  integer(i_kind),dimension(nchanl):: ich,id_qc,ich_diag
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  integer(i_kind),dimension(nchanl):: kmax
  integer(i_kind):: iinstr
  integer(i_kind) :: chan_count
  integer(i_kind),allocatable,dimension(:) :: sc_index

  logical channel_passive
  logical,dimension(nobs):: luse

  character(10) filex
  character(12) string

  type(rad_ob_type),pointer:: my_head,my_headm
  type(obs_diag),pointer:: my_diag

  n_alloc(:)=0
  m_alloc(:)=0
!**************************************************************************************
! Initialize variables and constants.
  mm1        = mype+1
  r90        = 90._r_kind
  coscon     = cos( (r90-55.0_r_kind)*deg2rad )
  sincon     = sin( (r90-55.0_r_kind)*deg2rad )

  factch6 = zero  
  cld   = zero
  cldp  = zero
  tpwc  = zero
  sgagl = zero
  dtp_avh=zero
  icc   = 0
  iccm  = 0
  ich9  = min(9,nchanl)
  do i=1,nchanl
     do j=1,npred
        pred(j,i)=zero
     end do
  end do

! Initialize logical flags for satellite platform

  hirs2      = obstype == 'hirs2'
  hirs3      = obstype == 'hirs3'
  hirs4      = obstype == 'hirs4'
  hirs       = hirs2 .or. hirs3 .or. hirs4
  msu        = obstype == 'msu'
  ssu        = obstype == 'ssu'
  goessndr   = obstype == 'sndr'  .or. obstype == 'sndrd1' .or.  &
               obstype == 'sndrd2'.or. obstype == 'sndrd3' .or.  &
               obstype == 'sndrd4'
  amsua      = obstype == 'amsua'
  amsub      = obstype == 'amsub'
  mhs        = obstype == 'mhs'
  airs       = obstype == 'airs'
  hsb        = obstype == 'hsb'
  goes_img   = obstype == 'goes_img'
  ahi        = obstype == 'ahi'
  avhrr      = obstype == 'avhrr'
  avhrr_navy = obstype == 'avhrr_navy'
  ssmi       = obstype == 'ssmi'
  amsre_low  = obstype == 'amsre_low'
  amsre_mid  = obstype == 'amsre_mid'
  amsre_hig  = obstype == 'amsre_hig'
  amsre      = amsre_low .or. amsre_mid .or. amsre_hig
  amsr2      = obstype == 'amsr2'
  gmi        = obstype == 'gmi'
  ssmis      = obstype == 'ssmis'
  ssmis_las  = obstype == 'ssmis_las'
  ssmis_uas  = obstype == 'ssmis_uas'
  ssmis_img  = obstype == 'ssmis_img'
  ssmis_env  = obstype == 'ssmis_env'
  iasi       = obstype == 'iasi'
  cris       = obstype == 'cris' .or. obstype == 'cris-fsr'
  seviri     = obstype == 'seviri'
  atms       = obstype == 'atms'
  saphir     = obstype == 'saphir'

  ssmis=ssmis_las.or.ssmis_uas.or.ssmis_img.or.ssmis_env.or.ssmis 

  microwave=amsua .or. amsub  .or. mhs .or. msu .or. hsb .or. &
            ssmi  .or. ssmis  .or. amsre .or. atms .or. &
            amsr2 .or. gmi  .or.  saphir

  microwave_low =amsua  .or.  msu .or. ssmi .or. ssmis .or. amsre

! Determine whether or not cloud-condensate is present in MetGuess
  lcw4crtm=.false.
  call gsi_metguess_get('dim',nguess,ier)
  if (nguess>0) then
     call gsi_metguess_get ('clouds_4crtm_jac::3d', icw4crtm, ier)
     if(icw4crtm >0) lcw4crtm = .true.
  end if

! lcw4crtm=lcw4crtm .and. (amsua .or. atms)  
  lcw4crtm=lcw4crtm .and.  amsua            !leave ATMS as clear-sky for now          

  if (lcw4crtm) then
!    Parameters for the observation error model 
!    cclr [kg/m2] & ccld [kg/m2]: range of cloud amounts over which the main
!    increase in error take place
     cclr(:)=zero
     ccld(:)=zero

     cclr( 1)=0.05_r_kind
     cclr( 2)=0.03_r_kind
     cclr( 3)=0.03_r_kind
     cclr( 4)=0.02_r_kind
     cclr( 5)=0.00_r_kind
     cclr( 6)=0.10_r_kind
     cclr(15)=0.03_r_kind

     ccld( 1)=0.60_r_kind
     ccld( 2)=0.45_r_kind
     ccld( 3)=0.40_r_kind
     ccld( 4)=0.45_r_kind
     ccld( 5)=1.00_r_kind
     ccld( 6)=1.50_r_kind
     ccld(15)=0.20_r_kind
  endif

! Initialize channel related information
  tnoise = r1e10
  tnoise_cld = r1e10
  l_may_be_passive = .false.
  toss = .true.
  jc=0

  do j=1,jpch_rad
     if(isis == nusis(j))then 
        jc=jc+1
        if(jc > nchanl)then
           write(6,*)'SETUPRAD:  ***ERROR*** in channel numbers, jc,nchanl=',jc,nchanl,&
              '  ***STOP IN SETUPRAD***'
           call stop2(71)
        end if

!       Load channel numbers into local array based on satellite type

        ich(jc)=j
        do i=1,npred
           predchan(i,jc)=predx(i,j)
        end do
!
!       Set error instrument channels
        tnoise(jc)=varch(j)
        channel_passive=iuse_rad(j)==-1 .or. iuse_rad(j)==0
        if (iuse_rad(j)< -1 .or. (channel_passive .and.  &
           .not.rad_diagsave)) tnoise(jc)=r1e10
        if (passive_bc .and. channel_passive) tnoise(jc)=varch(j)
        if (iuse_rad(j)>0) l_may_be_passive=.true.
        if (tnoise(jc) < 1.e4_r_kind) toss = .false.

        tnoise_cld(jc)=varch_cld(j)
        if (iuse_rad(j)< -1 .or. (iuse_rad(j) == -1 .and.  &
           .not.rad_diagsave)) tnoise_cld(jc)=r1e10
        if (passive_bc .and. (iuse_rad(j)==-1)) tnoise_cld(jc)=varch_cld(j)
     end if
  end do

  if(nchanl > jc) write(6,*)'SETUPRAD:  channel number reduced for ', &
     obstype,nchanl,' --> ',jc
  if(jc == 0) then
     if(mype == 0) write(6,*)'SETUPRAD: No channels found for ', obstype,isis
     if(nobs > 0)read(lunin)
     go to 135
  end if

  if (toss) then
     if(mype == 0)write(6,*)'SETUPRAD: all obs var > 1e4.  do not use ',&
        'data from satellite is=',isis
     if(nobs >0)read(lunin)                    
     goto 135
  endif

  if ( mype == 0 .and. .not.l_may_be_passive) write(6,*)mype,'setuprad: passive obs',is,isis

!  Logic to turn off print of reading coefficients if not first interation or not mype_diaghdr or not init_pass
  iwrmype=-99
  if(mype==mype_diaghdr(is) .and. init_pass .and. jiterstart == jiter)iwrmype = mype_diaghdr(is)

! Initialize radiative transfer and pointers to values in data_s
  call init_crtm(init_pass,iwrmype,mype,nchanl,isis,obstype)

! Get indexes of variables in jacobian to handle exceptions down below
  ioz =getindex(radjacnames,'oz')
  if(ioz>0) then
     ioz=radjacindxs(ioz)
  endif
  ius =getindex(radjacnames,'u')
  ivs =getindex(radjacnames,'v')
  if(ius>0.and.ivs>0) then
     ius=radjacindxs(ius)
     ivs=radjacindxs(ivs)
  endif
  if (regional .and. lcw4crtm) then
     iqs=getindex(radjacnames,'qs')
     if (iqs>0) iqs=radjacindxs(iqs)
     iqg=getindex(radjacnames,'qg')
     if (iqg>0) iqg=radjacindxs(iqg)
     iqh=getindex(radjacnames,'qh')
     if (iqh>0) iqh=radjacindxs(iqh)
     iqr=getindex(radjacnames,'qr')
     if (iqr>0) iqr=radjacindxs(iqr)
  end if 

! Initialize ozone jacobian flags to .false. (retain ozone jacobian)
  zero_irjaco3_pole = .false.

!  These variables are initialized in init_crtm
! isatid    = 1     ! index of satellite id
! itime     = 2     ! index of analysis relative obs time 
! ilon      = 3     ! index of grid relative obs location (x)
! ilat      = 4     ! index of grid relative obs location (y)
! ilzen_ang = 5     ! index of local (satellite) zenith angle (radians)
! ilazi_ang = 6     ! index of local (satellite) azimuth angle (radians)
! iscan_ang = 7     ! index of scan (look) angle (radians)
! iscan_pos = 8     ! index of integer scan position 
! iszen_ang = 9     ! index of solar zenith angle (degrees)
! isazi_ang = 10    ! index of solar azimuth angle (degrees)
! ifrac_sea = 11    ! index of ocean percentage
! ifrac_lnd = 12    ! index of land percentage
! ifrac_ice = 13    ! index of ice percentage
! ifrac_sno = 14    ! index of snow percentage
! its_sea   = 15    ! index of ocean temperature
! its_lnd   = 16    ! index of land temperature
! its_ice   = 17    ! index of ice temperature
! its_sno   = 18    ! index of snow temperature
! itsavg    = 19    ! index of average temperature
! ivty      = 20    ! index of vegetation type
! ivfr      = 21    ! index of vegetation fraction
! isty      = 22    ! index of soil type
! istp      = 23    ! index of soil temperature
! ism       = 24    ! index of soil moisture
! isn       = 25    ! index of snow depth
! izz       = 26    ! index of surface height
! idomsfc   = 27    ! index of dominate surface type
! isfcr     = 28    ! index of surface roughness
! iff10     = 29    ! index of ten meter wind factor
! ilone     = 30    ! index of earth relative longitude (degrees)
! ilate     = 31    ! index of earth relative latitude (degrees)
! itref     = 34/36 ! index of foundation temperature: Tr
! idtw      = 35/37 ! index of diurnal warming: d(Tw) at depth zob
! idtc      = 36/38 ! index of sub-layer cooling: d(Tc) at depth zob
! itz_tr    = 37/39 ! index of d(Tz)/d(Tr)

! Initialize sensor specific array pointers
! if (goes_img) then
!    iclr_sky      =  7 ! index of clear sky amount
! elseif (avhrr_navy) then
!    isst_navy     =  7 ! index of navy sst (K) retrieval
!    idata_type    = 30 ! index of data type (151=day, 152=night)
!    isst_hires    = 31 ! index of interpolated hires sst (K)
! elseif (avhrr) then
!    iclavr        = 32 ! index CLAVR cloud flag with AVHRR data
!    isst_hires    = 33 ! index of interpolated hires sst (K)
! elseif (seviri) then
!    iclr_sky      =  7 ! index of clear sky amount
! endif
! Special setup for SST retrieval (output)
  if (retrieval.and.init_pass) call setup_sst_retrieval(obstype,dplat(is),mype)

! Special setup for Tz retrieval
  if (tzr_qc>0) call setup_tzr_qc(obstype)

! Get version of rad-diag file
  call get_radiag ('version',iversion_radiag,istatus)
  if(istatus/=0) then
     write(6,*)'SETUPRAD: trouble getting version of diag file'
     call stop2(999)
  endif

! If SSM/I, check for non-use of 85GHz channel, for QC workaround
! set no85GHz true if any 85GHz is not used, and other freq channel is used
  no85GHz = .false.
  if (ssmi) then
     if (iuse_rad(ich(6)) < 1 .or. iuse_rad(ich(7)) < 1 ) then
        do j = 1,5
           if (iuse_rad(ich(j)) >= 1) then
              no85GHz = .true.
              cycle
           endif
        enddo
        if (no85GHz .and. mype == 0) write(6,*) &
           'SETUPRAD: using no85GHZ workaround for SSM/I ',isis
     endif
  endif



!  Find number of channels written to diag file
  if(reduce_diag)then
     nchanl_diag=0
     do i=1,nchanl
        if(iuse_rad(ich(i)) >= 1)then
           nchanl_diag=nchanl_diag+1
           ich_diag(nchanl_diag)=i
        end if
     end do
     if(mype == mype_diaghdr(is))write(6,*)'SETUPRAD:  reduced number of channels ',&
        nchanl_diag,' of ',nchanl,' written to diag file '
  else
     nchanl_diag=nchanl
     do i=1,nchanl_diag
        ich_diag(i)=i
     end do
  end if

! Set number of extra pieces of information to write to diagnostic file
! For most satellite sensors there is no extra information.  However, 
! for GOES Imager data we write additional information.
  iextra=0
  jextra=0
  if (goes_img .or. lwrite_peakwt) then
     jextra=nchanl_diag
     iextra=1
  end if
! If both, iextra=2
  if (goes_img .and. lwrite_peakwt) then
     iextra=2
  end if

  lextra = (iextra>0)


! Allocate array to hold channel information for diagnostic file and/or lobsdiagsave option
  idiag=ipchan_radiag+npred+2
  ioff0=idiag
  if (lobsdiagsave) idiag=idiag+4*miter+1
  allocate(diagbufchan(idiag,nchanl_diag))

  allocate(sc_index(nchanl))
  sc_index(:) = 0
  satinfo_chan: do i=1, nchanl
     n = ich(i)
     spec_coef: do k=1, sc(1)%n_channels
         if ( nuchan(n) == sc(1)%sensor_channel(k)) then
            sc_index(i) = k
            exit spec_coef
         endif
      end do spec_coef
   end do satinfo_chan

  do i=1,nchanl
     wavenumber(i)=sc(sensorindex)%wavenumber(sc_index(i))
  end do

! If diagnostic file requested, open unit to file and write header.
  if (rad_diagsave .and. nchanl_diag > 0) then
     filex=obstype
     write(string,1976) jiter
1976 format('_',i2.2)
     diag_rad_file= trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // trim(string)
     if(init_pass) then
        open(4,file=trim(diag_rad_file),form='unformatted',status='unknown',position='rewind')
     else
        open(4,file=trim(diag_rad_file),form='unformatted',status='old',position='append')
     endif
     if (lextra) allocate(diagbufex(iextra,jextra))

!    Initialize/write parameters for satellite diagnostic file on
!    first outer iteration.
     if (init_pass .and. mype==mype_diaghdr(is)) then
        inewpc=0
        if (newpc4pred) inewpc=1
        write(4) isis,dplat(is),obstype,jiter,nchanl_diag,npred,ianldate,ireal_radiag,ipchan_radiag,iextra,jextra,&
           idiag,angord,iversion_radiag,inewpc,ioff0
        write(6,*)'SETUPRAD:  write header record for ',&
           isis,npred,ireal_radiag,ipchan_radiag,iextra,jextra,idiag,angord,iversion_radiag,&
                      ' to file ',trim(diag_rad_file),' ',ianldate
        do i=1,nchanl
           n=ich(i)
           if( n < 1  .or. (reduce_diag .and. iuse_rad(n) < 1))cycle
           varch4=varch(n)
           tlap4=tlapmean(n)
           freq4=sc(sensorindex)%frequency(sc_index(i))
           pol4=sc(sensorindex)%polarization(sc_index(i))
           wave4=wavenumber(i)
           write(4)freq4,pol4,wave4,varch4,tlap4,iuse_rad(n),&
              nuchan(n),ich(i)
        end do
     endif
  endif

! Load data array for current satellite
  read(lunin) data_s,luse 

  if (nobskeep>0) then
     write(6,*)'setuprad: nobskeep',nobskeep
     call stop2(275)
  end if

! PROCESSING OF SATELLITE DATA

! Loop over data in this block
  call dtime_setup()
  do n = 1,nobs
!    Extract analysis relative observation time.
     dtime = data_s(itime,n)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then

        id_qc = igood_qc
        if(luse(n))aivals(1,is) = aivals(1,is) + one

!       Extract lon and lat.
        slons  = data_s(ilon,n)    ! grid relative longitude
        slats  = data_s(ilat,n)    ! grid relative latitude                     
        cenlon = data_s(ilone,n)   ! earth relative longitude (degrees)
        cenlat = data_s(ilate,n)   ! earth relative latitude (degrees)                       
!       Extract angular information         
        zasat  = data_s(ilzen_ang,n)
        cosza  = cos(zasat)
        zsges=data_s(izz,n)
        nadir = nint(data_s(iscan_pos,n))
        pangs  = data_s(iszen_ang,n)
!       Extract warm load temperatures
!       wltm1 = data_s(isty,n)
!       wltm2 = data_s(istp,n)
!       wltm3 = data_s(ism,n)

!  If desired recompute 10meter wind factor 
        if(sfcmod_gfs .or. sfcmod_mm5) then
           isli=nint(data_s(idomsfc,n))
           call comp_fact10(slats,slons,dtime,data_s(itsavg,n),data_s(isfcr,n), &
              isli,mype,data_s(iff10,n))
        end if

        if(seviri .and. abs(data_s(iszen_ang,n)) > 180.0_r_kind) data_s(iszen_ang,n)=r100
 
 
!  Set land/sea, snow, ice percentages and flags (no time interpolation)

        sea  = data_s(ifrac_sea,n)  >= 0.99_r_kind
        land = data_s(ifrac_lnd,n)  >= 0.99_r_kind
        ice  = data_s(ifrac_ice,n)  >= 0.99_r_kind
        snow = data_s(ifrac_sno,n)  >= 0.99_r_kind
        mixed = .not. sea  .and. .not. ice .and.  &
                .not. land .and. .not. snow

       if(sea) then
          isfctype=0
       else if(land) then
          isfctype=1
       else if(ice) then
          isfctype=2
       else if(snow) then
          isfctype=3
       else if(mixed) then
          isfctype=4
       endif
         
!       Count data of different surface types
        if(luse(n))then
           if (mixed) then
              aivals(5,is) = aivals(5,is) + one
           else if (ice .or. snow) then
              aivals(4,is) = aivals(4,is) + one
           else if (land) then
              aivals(3,is) = aivals(3,is) + one
           end if
        end if

!       Set relative weight value
        val_obs=one
        if(dval_use)then
           ixx=nint(data_s(nreal-nstinfo,n))
           if (ixx > 0 .and. super_val1(ixx) >= one) then
              val_obs=data_s(nreal-nstinfo-1,n)/super_val1(ixx)
           endif
        endif

!       Load channel data into work array.
        do i = 1,nchanl
           tb_obs(i) = data_s(i+nreal,n)
        end do
 

!       Interpolate model fields to observation location, call crtm and create jacobians
!       Output both tsim and tsim_clr for allsky
        if (lcw4crtm) then
           call call_crtm(obstype,dtime,data_s(1,n),nchanl,nreal,ich, &
                tvp,qvp,clw_guess,prsltmp,prsitmp, &
                trop5,tzbgr,dtsavg,sfc_speed, &
                tsim,emissivity,ptau5,ts,emissivity_k, &
                temp,wmix,jacobian,error_status,tsim_clr=tsim_clr)
        else
           call call_crtm(obstype,dtime,data_s(1,n),nchanl,nreal,ich, &
                tvp,qvp,clw_guess,prsltmp,prsitmp, &
                trop5,tzbgr,dtsavg,sfc_speed, &
                tsim,emissivity,ptau5,ts,emissivity_k, &
                temp,wmix,jacobian,error_status)
        endif 
! If the CRTM returns an error flag, do not assimilate any channels for this ob 
! and set the QC flag to ifail_crtm_qc.
! We currently go through the rest of the QC steps, ensuring that the diagnostic
! files are populated, but this could be changed if it causes problems.  
        if (error_status == 0) then
           varinv(1:nchanl) = val_obs
        else
           id_qc(1:nchanl) = ifail_crtm_qc
           varinv(1:nchanl) = zero
        endif

!  For SST retrieval, use interpolated NCEP SST analysis
        if (retrieval) then
           if( avhrr_navy )then
              dtp_avh = data_s(idata_type,n)
              sstcu=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
              sstnv=data_s(isst_navy,n)
           elseif ( avhrr) then
              if ( pangs <= 89.0_r_kind) then              ! day time
                 dtp_avh = 151.0_r_kind
              else
                 dtp_avh = 152.0_r_kind
              endif
              sstcu=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
              sstnv=data_s(isst_hires,n)      ! not available, assigned as interpolated sst
           endif
           tsavg5 = data_s(isst_hires,n)
        else
           tsavg5=data_s(itsavg,n)
           tsavg5=tsavg5+dtsavg
        endif

!       If using adaptive angle dependent bias correction, update the predicctors
!       for this part of bias correction.  The AMSUA cloud liquid water algorithm
!       uses total angle dependent bias correction for channels 1 and 2
        if (adp_anglebc) then
           do i=1,nchanl
              mm=ich(i)
              if (goessndr .or. goes_img .or. ahi .or. seviri .or. ssmis) then
                 pred(npred,i)=nadir*deg2rad
              else
                 pred(npred,i)=data_s(iscan_ang,n)
              end if
              do j=2,angord
                 pred(npred-j+1,i)=pred(npred,i)**j
              end do
              cbias(nadir,mm)=zero
              do j=1,angord
                 cbias(nadir,mm)=cbias(nadir,mm)+predchan(npred-j+1,i)*pred(npred-j+1,i)
              end do
           end do
        end if

!       Compute microwave cloud liquid water or graupel water path for bias correction and QC.
        clw=zero
        clwp_amsua=zero
        clw_obs=zero
        clw_guess_retrieval=zero
        gwp=zero
        tpwc_amsua=zero
        tpwc_guess_retrieval=zero
        scatp=zero
        scat=zero  
        ierrret=0
        tpwc=zero
        kraintype=0
        cldeff_obs=zero 
        cldeff_sim=zero  
        if(microwave .and. sea) then 
           if(lcw4crtm) then                            
              call ret_amsua(tb_obs,nchanl,tsavg5,zasat,clwp_amsua,ierrret,scat)
              scatp=scat 
           else
              call calc_clw(nadir,tb_obs,tsim,ich,nchanl,no85GHz,amsua,ssmi,ssmis,amsre,atms, &
                   amsr2,gmi,saphir,tsavg5,sfc_speed,zasat,clw,tpwc,gwp,kraintype,ierrret)
                if(gmi .or. amsr2) then   ! set clw_obs for gmi and amsr2
                  clw_obs = clw
                endif
           end if
           if (ierrret /= 0) then
             if (amsua) then 
                varinv(1:6)=zero
                id_qc(1:6) = ifail_cloud_qc
                varinv(15)=zero
                id_qc(15) = ifail_cloud_qc
             else if (atms) then 
                varinv(1:7)=zero
                id_qc(1:7) = ifail_cloud_qc
                varinv(16:22)=zero
                id_qc(16) = ifail_cloud_qc
             else       
                varinv(1:nchanl)=zero
                id_qc(1:nchanl) = ifail_cloud_qc
             endif
           endif
        endif

        predbias=zero
        do i=1,nchanl
           mm=ich(i)


!*****
!     COMPUTE AND APPLY BIAS CORRECTION TO SIMULATED VALUES
!*****

!       Construct predictors for 1B radiance bias correction.
           if (.not. newpc4pred) then
              pred(1,i) = r0_01
              pred(2,i) = one_tenth*(one/cosza-one)**2-.015_r_kind
              if(ssmi .or. ssmis .or. amsre .or. gmi .or. amsr2)pred(2,i)=zero
           else
              pred(1,i) = one
              if (adp_anglebc) then
                 pred(2,i) = zero
              else
                 pred(2,i) = (one/cosza-one)**2
              end if
           end if

           pred(3,i) = zero
           if (amsre) then
              pred(3,i) = clw
           else
              pred(3,i) = clw*cosza*cosza
           end if
           if(lcw4crtm .and. sea) pred(3,i ) = zero 
 



!       Apply bias correction
 
           kmax(i) = 0
           if (lwrite_peakwt .or. passive_bc) then
              ptau5derivmax = -9.9e31_r_kind
! maximum of weighting function is level at which transmittance
! (ptau5) is changing the fastest.  This is used for the level
! assignment (needed for vertical localization).
              weightmax(i) = zero
              do k=2,nsig
                 ptau5deriv = abs( (ptau5(k-1,i)-ptau5(k,i))/ &
                    (log(prsltmp(k-1))-log(prsltmp(k))) )
                 if (ptau5deriv > ptau5derivmax) then
                    ptau5derivmax = ptau5deriv
                    kmax(i) = k
                    weightmax(i) = r10*prsitmp(k) ! cb to mb.
                 end if
              enddo
           end if

           tlapchn(i)= (ptau5(2,i)-ptau5(1,i))*(tsavg5-tvp(2))
           do k=2,nsig-1
              tlapchn(i)=tlapchn(i)+&
                 (ptau5(k+1,i)-ptau5(k,i))*(tvp(k-1)-tvp(k+1))
           end do
           if (.not. newpc4pred) tlapchn(i) = r0_01*tlapchn(i)
           tlap = tlapchn(i)-tlapmean(mm)
           pred(4,i)=tlap*tlap
           pred(5,i)=tlap

!          additional bias predictor (as/ds node) for SSMIS         
           pred(6,i)= zero                                      
           pred(7,i)= zero                                     
           node = data_s(ilazi_ang,n)                              
           if (ssmis .and. node < 1000) then                                         
              if (.not. newpc4pred) then                           
                 pred(6,i)= ssmis_precond*node*cos(cenlat*deg2rad)          
                 pred(7,i)= ssmis_precond*sin(cenlat*deg2rad)               
              else                                               
                 pred(6,i)= node*cos(cenlat*deg2rad)            
                 pred(7,i)= sin(cenlat*deg2rad)                  
              endif                                                
           endif                                                   

!          emissivity sensitivity bias predictor
           if (adp_anglebc .and. emiss_bc) then 
              pred(8,i)=zero
              if (.not.sea .and. abs(emissivity_k(i))>0.001_r_kind) then
                 pred(8,i)=emissivity_k(i)
              end if
           end if

           do j=1, npred-angord                              
              pred(j,i)=pred(j,i)*air_rad(mm)
           end do
           if (adp_anglebc) then
              do j=npred-angord+1, npred                                         
                 pred(j,i)=pred(j,i)*ang_rad(mm)
              end do
           end if

           do j = 1,npred
              predbias(j,i) = predchan(j,i)*pred(j,i)
           end do
           predbias(npred+1,i) = cbias(nadir,mm)*ang_rad(mm)      !global_satangbias

!          Apply SST dependent bias correction with cubic spline
           if (retrieval) then
              call spline_cub(fbias(:,mm),tsavg5,ys_bias_sst)
              predbias(npred+2,i) = ys_bias_sst
           endif

!          tbc    = obs - guess after bias correction
!          tbcnob = obs - guess before bias correction
           tbcnob(i)    = tb_obs(i) - tsim(i)  
           tbc(i)       = tbcnob(i)                     
 
           do j=1, npred-angord
              tbc(i)=tbc(i) - predbias(j,i) !obs-ges with bias correction
           end do
           tbc(i)=tbc(i) - predbias(npred+1,i)
           tbc(i)=tbc(i) - predbias(npred+2,i)

!          Calculate cloud effect for QC
           if (lcw4crtm) then
              cldeff_sim(i) = tsim(i)-tsim_clr(i)      ! simulated cloud delta
              cldeff_obs(i) = tb_obs(i)-tsim_clr(i)    ! observed cloud delta (no bias correction)                
              ! need to apply bias correction ? need to think about this
              bias = zero
              do j=1, npred-angord
                 bias = bias+predbias(j,i)
              end do
              bias = bias+predbias(npred+1,i)
              bias = bias+predbias(npred+2,i)
              cldeff_obs(i)=cldeff_obs(i) - bias       ! observed cloud delta (bias corrected)                
           endif

!       End of loop over channels
        end do
 
!       Compute retrieved microwave cloud liquid water and 
!       assign cld_rbc_idx for bias correction in allsky conditions
        cld_rbc_idx=one
        if (lcw4crtm .and. sea)  then
           ierrret=0
           do i=1,nchanl
              mm=ich(i)
              tsim_bc(i)=tsim(i)
              do j=1,npred-angord
                 tsim_bc(i)=tsim_bc(i)+predbias(j,i)
              end do
              tsim_bc(i)=tsim_bc(i)+predbias(npred+1,i)
              tsim_bc(i)=tsim_bc(i)+predbias(npred+2,i)
           end do
           call ret_amsua(tsim_bc,nchanl,tsavg5,zasat,clw_guess_retrieval,ierrret)
           do i=1,nchanl
!             if (clwp_amsua<=cclr(i) .and. clw_guess_retrieval> cclr(i)) cld_rbc_idx(i)=zero  
!             if (clwp_amsua> cclr(i) .and. clw_guess_retrieval<=cclr(i)) cld_rbc_idx(i)=zero  
              if ((clwp_amsua-cclr(i))*(clw_guess_retrieval-cclr(i))<zero .and.  &
                   abs(clwp_amsua-clw_guess_retrieval)>=0.005_r_kind) cld_rbc_idx(i)=zero
           end do

           if (ierrret /= 0) then
             if (amsua) then 
                varinv(1:6)=zero
                id_qc(1:6) = ifail_cloud_qc
                varinv(15)=zero
                id_qc(15) = ifail_cloud_qc
             else if (atms) then 
                varinv(1:7)=zero
                id_qc(1:7) = ifail_cloud_qc
                varinv(16:22)=zero
                id_qc(16) = ifail_cloud_qc
             else       
                varinv(1:nchanl)=zero
                id_qc(1:nchanl) = ifail_cloud_qc
             endif
           endif
        end if ! end of (lcw4crtm .and. sea)
        

        do i=1,nchanl
           mm=ich(i)
           error0(i) = tnoise(i) 
           errf0(i) = error0(i)

           if(lcw4crtm .and. sea)  then   
              clwtmp=half*(clwp_amsua+clw_guess_retrieval)
              if(clwtmp <= cclr(i)) then
                 error0(i) = tnoise(i)
              else if(clwtmp > cclr(i) .and. clwtmp < ccld(i)) then
                 error0(i) = tnoise(i) + &
                     (clwtmp-cclr(i))*(tnoise_cld(i)-tnoise(i))/(ccld(i)-cclr(i))
              else
                 error0(i) = tnoise_cld(i) 
              endif
           endif

           channel_passive=iuse_rad(ich(i))==-1 .or. iuse_rad(ich(i))==0
           if(tnoise(i) < 1.e4_r_kind .or. (channel_passive .and. rad_diagsave) &
                  .or. (passive_bc .and. channel_passive))then
              varinv(i)     = varinv(i)/error0(i)**2
              errf(i)       = error0(i)
           else
              if(id_qc(i) == igood_qc) id_qc(i)=ifail_satinfo_qc
              varinv(i)     = zero
              errf(i)       = zero
           endif

!       End of loop over channels         
        end do

!******
!    QC OBSERVATIONS BASED ON VARIOUS CRITERIA
!            Separate blocks for various instruments.
!******
     
!  ---------- IR -------------------
!       QC HIRS/2, GOES, HIRS/3 and AIRS sounder data
!
        ObsQCs: if (hirs .or. goessndr .or. airs .or. iasi .or. cris) then

           frac_sea=data_s(ifrac_sea,n)

!  NOTE:  The qc in qc_irsnd uses the inverse squared obs error.
!     The loop below loads array varinv_use accounting for whether the 
!     cloud detection flag is set.  Array
!     varinv_use is then used in the qc calculations.
!     For the case when all channels of a sensor are passive, all
!     channels with iuse_rad=-1 or 0 are used in cloud detection.

           do i=1,nchanl
              m=ich(i)
              if (varinv(i) < tiny_r_kind) then
                 varinv_use(i) = zero
              else
                 if ((icld_det(m)>0)) then
                    varinv_use(i) = varinv(i)
                 else
                    varinv_use(i) = zero
                 end if
              end if
           end do
           call qc_irsnd(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse(n),goessndr, &
              cris,zsges,cenlat,frac_sea,pangs,trop5,zasat,tzbgr,tsavg5,tbc,tb_obs,tnoise,  &
              wavenumber,ptau5,prsltmp,tvp,temp,wmix,emissivity_k,ts,                 &
              id_qc,aivals,errf,varinv,varinv_use,cld,cldp,kmax,zero_irjaco3_pole(n))

!  --------- MSU -------------------
!       QC MSU data
        else if (msu) then

           call qc_msu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse(n), &
              zsges,cenlat,tbc,ptau5,emissivity_k,ts,id_qc,aivals,errf,varinv)

!  ---------- AMSU-A -------------------
!       QC AMSU-A data
        else if (amsua) then

           if (adp_anglebc) then
              tb_obsbc1=tb_obs(1)-cbias(nadir,ich(1))-predx(1,ich(1))
           else
              tb_obsbc1=tb_obs(1)-cbias(nadir,ich(1))
           end if
           cldeff_obs5=cldeff_obs(5)   ! observed cloud effect for channel 5       
           call qc_amsua(nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse(n),   &
              zsges,cenlat,tb_obsbc1,cosza,clw,tbc,ptau5,emissivity_k,ts, & 
              pred,predchan,id_qc,aivals,errf,errf0,clwp_amsua,varinv,cldeff_obs5,factch6, &
              cld_rbc_idx,sfc_speed,error0,clw_guess_retrieval,scatp)                    

!  If cloud impacted channels not used turn off predictor

           do i=1,nchanl
              if ( (i <= 5 .or. i == 15) .and. (varinv(i)<1.e-9_r_kind) ) then
                 pred(3,i) = zero
              end if
           end do


!  ---------- AMSU-B -------------------
!       QC AMSU-B and MHS data

        else if (amsub .or. hsb .or. mhs) then

           call qc_mhs(nchanl,ndat,nsig,is,sea,land,ice,snow,mhs,luse(n),   &
              zsges,tbc,tb_obs,ptau5,emissivity_k,ts,      &
              id_qc,aivals,errf,varinv,clw,tpwc)

!  ---------- ATMS -------------------
!       QC ATMS data

        else if (atms) then

           if (adp_anglebc) then
              tb_obsbc1=tb_obs(1)-cbias(nadir,ich(1))-predx(1,ich(1))
              cldeff_obs5=cldeff_obs(6)   ! observed cloud effect for ATMS channel 6        
           else
              tb_obsbc1=tb_obs(1)-cbias(nadir,ich(1))
           end if
           call qc_atms(nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse(n),    &
              zsges,cenlat,tb_obsbc1,cosza,clw,tbc,ptau5,emissivity_k,ts, & 
              pred,predchan,id_qc,aivals,errf,errf0,clwp_amsua,varinv,cldeff_obs5,factch6, &
              cld_rbc_idx,sfc_speed,error0,clw_guess_retrieval,scatp)                   

!  ---------- GOES imager --------------
!       GOES imager Q C
!
        else if(goes_img)then


           cld = data_s(iclr_sky,n)
           do i = 1,nchanl
              tb_obs_sdv(i) = data_s(i+29,n)
           end do
           call qc_goesimg(nchanl,is,ndat,nsig,ich,dplat(is),sea,land,ice,snow,luse(n), &
              zsges,cld,tzbgr,tb_obs,tb_obs_sdv,tbc,tnoise,temp,wmix,emissivity_k,ts,id_qc,aivals,errf,varinv)
           

!  ---------- SEVIRI  -------------------
!       SEVIRI Q C

        else if (seviri) then

           cld = 100-data_s(iclr_sky,n)

           call qc_seviri(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse(n), &
              zsges,tzbgr,tbc,tnoise,temp,wmix,emissivity_k,ts,id_qc,aivals,errf,varinv)
!

!  ---------- AVRHRR --------------
!       NAVY AVRHRR Q C

        else if (avhrr_navy .or. avhrr) then

           frac_sea=data_s(ifrac_sea,n)

!  NOTE:  The qc in qc_avhrr uses the inverse squared obs error.
!     The loop below loads array varinv_use accounting for whether the 
!     cloud detection flag is set.  Array
!     varinv_use is then used in the qc calculations.
!     For the case when all channels of a sensor are passive, all
!     channels with iuse_rad=-1 or 0 are used in cloud detection.
           do i=1,nchanl
              m=ich(i)
              if (varinv(i) < tiny_r_kind) then
                 varinv_use(i) = zero
              else
                 if ((icld_det(m)>0)) then
                    varinv_use(i) = varinv(i)
                 else
                    varinv_use(i) = zero
                 end if
              end if
           end do

           call qc_avhrr(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse(n),   &
              zsges,cenlat,frac_sea,pangs,trop5,tzbgr,tsavg5,tbc,tb_obs,tnoise,     &
              wavenumber,ptau5,prsltmp,tvp,temp,wmix,emissivity_k,ts, &
              id_qc,aivals,errf,varinv,varinv_use,cld,cldp)


!  ---------- SSM/I , SSMIS, AMSRE  -------------------
!       SSM/I, SSMIS, & AMSRE Q C

        else if( ssmi .or. amsre .or. ssmis )then   

           if(amsre)then
              bearaz= (270._r_kind-data_s(ilazi_ang,n))*deg2rad
              sun_zenith=data_s(iszen_ang,n)*deg2rad
              sun_azimuth=(r90-data_s(isazi_ang,n))*deg2rad
              sgagl =  acos(coscon * cos( bearaz ) * cos( sun_zenith ) * cos( sun_azimuth ) + &
                       coscon * sin( bearaz ) * cos( sun_zenith ) * sin( sun_azimuth ) +  &
                       sincon *  sin( sun_zenith )) * rad2deg
           end if
           call qc_ssmi(nchanl,nsig,ich, &
              zsges,luse(n),sea,mixed, &
              temp,wmix,ts,emissivity_k,ierrret,kraintype,tpwc,clw,sgagl,tzbgr, &
              tbc,tbcnob,tsim,tnoise,ssmi,amsre_low,amsre_mid,amsre_hig,ssmis, &
              varinv,errf,aivals(1,is),id_qc)

!  ---------- AMSR2  -------------------
!       AMSR2 Q C

        else if (amsr2) then
  
           sun_azimuth=data_s(isazi_ang,n)
           sun_zenith=data_s(iszen_ang,n)

          call qc_amsr2(nchanl,zsges,luse(n),sea, &
              kraintype,clw_obs,tsavg5,tb_obs,sun_azimuth,sun_zenith,amsr2,varinv,aivals(1,is),id_qc)

!  ---------- GMI  -------------------
!       GMI Q C

        else if (gmi) then

           call qc_gmi(nchanl,zsges,luse(n),sea,cenlat, &
              kraintype,clw_obs,tsavg5,tb_obs,gmi,varinv,aivals(1,is),id_qc)

!  ---------- SAPHIR -----------------
!       SAPHIR Q C
        
        else if (saphir) then

        call qc_saphir(nchanl,zsges,luse(n),sea, &
              kraintype,varinv,aivals(1,is),id_qc)
        
!  ---------- SSU  -------------------
!       SSU Q C

        elseif (ssu) then

           call qc_ssu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse(n), &
              zsges,cenlat,tb_obs,ptau5,emissivity_k,ts,id_qc,aivals,errf,varinv)
            
        end if ObsQCs

!       Done with sensor qc blocks.  Now make final qc decisions.

!       Apply gross check to observations.  Toss obs failing test.
        do i = 1,nchanl
           if (varinv(i) > tiny_r_kind ) then
              m=ich(i)
              if(lcw4crtm .and. sea) then 
                 if (i <= 3 .or. i==15) then         
                    errf(i) = 3.00_r_kind*errf(i)    
                 else if (i == 4) then                     
                    errf(i) = 3.00_r_kind*errf(i)           
                 else if (i == 5) then
                    errf(i) = 3.00_r_kind*errf(i)
                 else
                    errf(i) = min(three*errf(i),ermax_rad(m))
                 endif
              else if (ssmis) then
                 errf(i) = min(1.5_r_kind*errf(i),ermax_rad(m))  ! tighten up gross check for SSMIS
              else if (gmi .or. saphir .or. amsr2) then
                 errf(i) = ermax_rad(m)     ! use ermax for GMI, SAPHIR, and AMSR2 gross check
              else
                 errf(i) = min(three*errf(i),ermax_rad(m))
              endif
              if (abs(tbc(i)) > errf(i)) then
!                If mean obs-ges difference around observations
!                location is too large and difference at the 
!                observation location is similarly large, then
!                toss the observation.
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_gross_qc
                 varinv(i) = zero
                 if(luse(n))stats(2,m) = stats(2,m) + one
                 if(luse(n))aivals(7,is) = aivals(7,is) + one
              end if
           end if
        end do

        if(amsua .or. amsub .or. mhs .or. msu .or. hsb)then
           if(amsua)nlev=6
           if(amsub .or. mhs)nlev=5
           if(hsb)nlev=4
           if(msu)nlev=4
           kval=0
           do i=2,nlev
!          do i=1,nlev
              channel_passive=iuse_rad(ich(i))==-1 .or. iuse_rad(ich(i))==0
              if (varinv(i)<tiny_r_kind .and. ((iuse_rad(ich(i))>=1) .or. &
                  (passive_bc .and. channel_passive))) then
                 kval=max(i-1,kval)
                 if(amsub .or. hsb .or. mhs)kval=nlev
                 if(amsua .and. i <= 3)kval = zero
              end if
           end do
           if(kval > 0)then
              do i=1,kval
                 varinv(i)=zero
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_interchan_qc
              end do
              if(amsua)then
                 varinv(15)=zero
                 if(id_qc(15) == igood_qc)id_qc(15)=ifail_interchan_qc
              end if
           end if
        end if


!       If requested, generate SST retrieval (output)
        if(retrieval) then
           if(avhrr_navy .or. avhrr) then
              call avhrr_sst_retrieval(dplat(is),nchanl,tnoise,&
                 varinv,tsavg5,sstph,temp,wmix,ts,tbc,cenlat,cenlon,&
                 dtime,dtp_avh,tb_obs,dta,dqa,luse(n))
           endif
        endif

        icc = 0
        iccm= 0

        do i = 1,nchanl

!          Reject radiances for single radiance test
           if (lsingleradob) then
              ! if the channels are beyond 0.01 of oblat/oblon, specified
              ! in gsi namelist, or aren't of type 'oneob_type', reject
              if ( (abs(cenlat - oblat) > one/r100 .or. &
                    abs(cenlon - oblon) > one/r100) .or. &
                    obstype /= oneob_type ) then
                 varinv(i) = zero
                 varinv_use(i) = zero
                 if (id_qc(i) == igood_qc) id_qc(i) = ifail_outside_range
              else
                 ! if obchan <= zero, keep all footprints, if obchan > zero,
                 ! keep only that which has channel obchan
                 if (i /= obchan .and. obchan > zero) then
                    varinv(i) = zero
                    varinv_use(i) = zero
                    if (id_qc(i) == igood_qc) id_qc(i) = ifail_outside_range
                 endif
              endif !cenlat/lon
           endif !lsinglerad

!          Only process observations to be assimilated

           if (varinv(i) > tiny_r_kind ) then

              m = ich(i)
              if(luse(n))then
                 drad    = tbc(i)   
                 dradnob = tbcnob(i)
                 varrad  = drad*varinv(i)
                 stats(1,m)  = stats(1,m) + one              !number of obs
!                stats(3,m)  = stats(3,m) + drad             !obs-mod(w_biascor)
!                stats(4,m)  = stats(4,m) + tbc(i)*drad      !(obs-mod(w_biascor))**2
!                stats(5,m)  = stats(5,m) + tbc(i)*varrad    !penalty contribution
!                stats(6,m)  = stats(6,m) + dradnob          !obs-mod(w/o_biascor)
                 stats(3,m)  = stats(3,m) + drad*cld_rbc_idx(i)        !obs-mod(w_biascor)
                 stats(4,m)  = stats(4,m) + tbc(i)*drad*cld_rbc_idx(i) !(obs-mod(w_biascor))**2
                 stats(5,m)  = stats(5,m) + tbc(i)*varrad    !penalty contribution
                 stats(6,m)  = stats(6,m) + dradnob*cld_rbc_idx(i)     !obs-mod(w/o_biascor)

                 exp_arg = -half*(tbc(i)/error0(i))**2
                 error=sqrt(varinv(i))
                 if (pg_rad(m) > tiny_r_kind .and. error > tiny_r_kind) then
                    arg  = exp(exp_arg)
                    wnotgross= one-pg_rad(m)
                    cg_rad=b_rad(m)*error
                    wgross = cg_term*pg_rad(m)/(cg_rad*wnotgross)
                    term = log((arg+wgross)/(one+wgross))
                    wgt  = one-wgross/(arg+wgross)
                 else
                    term = exp_arg
                    wgt  = one
                 endif
                 stats(7,m)  = stats(7,m) -two*(error0(i)**2)*varinv(i)*term
              end if
           
!             Only "good" obs are included in J calculation.
              if (iuse_rad(m) >= 1)then
                 if(luse(n))then
                    aivals(40,is) = aivals(40,is) + tbc(i)*varrad
                    aivals(39,is) = aivals(39,is) -two*(error0(i)**2)*varinv(i)*term
                    aivals(38,is) = aivals(38,is) +one
                    if(wgt < wgtlim) aivals(2,is)=aivals(2,is)+one

!                   summation of observation number
                    if (newpc4pred) then
                       ostats(m)  = ostats(m) + one*cld_rbc_idx(i)
                    end if
                 end if

                 icc=icc+1

!             End of use data block
              end if

!             At the end of analysis, prepare for bias correction for monitored channels
!             Only "good monitoring" obs are included in J_passive calculation.
              channel_passive=iuse_rad(m)==-1 .or. iuse_rad(m)==0
              if (passive_bc .and. (jiter>miter) .and. channel_passive) then
!                summation of observation number,
!                skip ostats accumulation for channels without coef. initialization 
                 if (newpc4pred .and. luse(n) .and. any(predx(:,m)/=zero)) then
                    ostats(m)  = ostats(m) + one*cld_rbc_idx(i)
                 end if
                 iccm=iccm+1
              end if


!          End of varinv>tiny_r_kind block
           endif

!       End loop over channels.
        end do

     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (l_may_be_passive .and. .not. retrieval) then

        if(in_curbin) then
!          Load data into output arrays
           if(icc > 0)then
              nchan_total=nchan_total+icc
 
              if(.not. associated(radhead(ibin)%head))then
                 allocate(radhead(ibin)%head,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write radhead '
                 radtail(ibin)%head => radhead(ibin)%head
              else
                 allocate(radtail(ibin)%head%llpoint,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write radtail%llpoint '
                 radtail(ibin)%head => radtail(ibin)%head%llpoint
              end if

              m_alloc(ibin) = m_alloc(ibin) +1
              my_head => radtail(ibin)%head
              my_head%idv = is
              my_head%iob = n

              allocate(radtail(ibin)%head%res(icc),radtail(ibin)%head%err2(icc), &
                       radtail(ibin)%head%raterr2(icc),radtail(ibin)%head%pred(npred,icc), &
                       radtail(ibin)%head%dtb_dvar(nsigradjac,icc), &
                       radtail(ibin)%head%ich(icc),&  
                       radtail(ibin)%head%icx(icc))
              if (luse_obsdiag) allocate(radtail(ibin)%head%diags(icc))

              call get_ij(mm1,slats,slons,radtail(ibin)%head%ij(:),radtail(ibin)%head%wij(:))
              radtail(ibin)%head%time=dtime
              radtail(ibin)%head%luse=luse(n)
              radtail(ibin)%head%ich(:)=-1  
              radtail(ibin)%head%isis=isis
              radtail(ibin)%head%isfctype=isfctype
              utbc=tbc
              wgtjo= varinv     ! weight used in Jo term
!              adaptinf = error0 ! on input 
              adaptinf = varinv
              obvarinv = error0
              account_for_corr_obs = radinfo_adjust_jacobian(iinstr,isis,isfctype,nchanl,nsigradjac,ich,varinv,&
                                                             utbc,obvarinv,adaptinf,wgtjo,jacobian)
              iii=0
              do ii=1,nchanl
                 m=ich(ii)
                 if (varinv(ii)>tiny_r_kind .and. iuse_rad(m)>=1) then

                    iii=iii+1

                    if(account_for_corr_obs) then
                      radtail(ibin)%head%res(iii)= utbc(ii)                   ! evecs(R)*[obs-ges innovation]
                      radtail(ibin)%head%err2(iii)= obvarinv(ii)              ! 1/eigenvalue(R)
                      radtail(ibin)%head%raterr2(iii)=adaptinf(ii)            ! inflation factor 
                    else
                      radtail(ibin)%head%res(iii)= tbc(ii)                    ! obs-ges innovation
                      radtail(ibin)%head%err2(iii)= one/error0(ii)**2         ! 1/(obs error)**2  (original uninflated error)
                      radtail(ibin)%head%raterr2(iii)=error0(ii)**2*varinv(ii) ! (original error)/(inflated error)
                    endif
                    radtail(ibin)%head%icx(iii)= m                         ! channel index
                    radtail(ibin)%head%ich(iii)=ii              
                    do k=1,npred
                       radtail(ibin)%head%pred(k,iii)=pred(k,ii)*cld_rbc_idx(ii)*upd_pred(k)
                    end do

                    do k=1,nsigradjac
                       radtail(ibin)%head%dtb_dvar(k,iii)=jacobian(k,ii)
                    end do

!                   Load jacobian for ozone (dTb/doz).  For hirs and goes channel 9
!                   (ozone channel) we do not let the observations change the ozone.
!                   There currently is no ozone analysis when running in the NCEP 
!                   regional mode, therefore set ozone jacobian to 0.0
                    if (ioz>=0) then
                       if (regional .or. qc_noirjaco3 .or. zero_irjaco3_pole(n) .or. & 
                          ((hirs .or. goessndr).and.(varinv(ich9) < tiny_r_kind))) then
                          do k = 1,nsig
                             radtail(ibin)%head%dtb_dvar(ioz+k,iii) = zero
                          end do
                       endif
                    endif

!                   Load Jacobian for wind speed (dTb/du, dTb/dv)
                    if(ius>=0.and.ivs>=0) then
                       if( .not. dtbduv_on .or. .not. microwave) then
                          radtail(ibin)%head%dtb_dvar(ius+1,iii) = zero
                          radtail(ibin)%head%dtb_dvar(ivs+1,iii) = zero
                       endif
                    end if

!                   Load Jacobian for hydrometeors
                    if (regional .and. lcw4crtm) then
                       if (iqs>0) then
                          do k = 1,nsig
                             radtail(ibin)%head%dtb_dvar(iqs+k,iii) = zero
                          end do
                       end if
                       if (iqg>0) then
                          do k = 1,nsig
                             radtail(ibin)%head%dtb_dvar(iqg+k,iii) = zero
                          end do
                       end if
                       if (iqh>0) then
                          do k = 1,nsig
                             radtail(ibin)%head%dtb_dvar(iqh+k,iii) = zero
                          end do
                       end if
                       if (iqr>0) then
                          do k = 1,nsig
                             radtail(ibin)%head%dtb_dvar(iqr+k,iii) = zero
                          end do
                       end if
                    end if

                    my_head%ich(iii)=ii

!                   compute hessian contribution from Jo bias correction terms
                    if (newpc4pred .and. luse(n)) then
                       do k=1,npred
                          rstats(k,m)=rstats(k,m)+radtail(ibin)%head%pred(k,iii) &
                               *radtail(ibin)%head%pred(k,iii)*varinv(ii)
                       end do
                    end if  ! end of newpc4pred loop

                 end if
              end do
              radtail(ibin)%head%nchan  = iii         ! profile observation count
              radtail(ibin)%head%use_corr_obs=.false.
              if (account_for_corr_obs) then
                 chan_count=(radtail(ibin)%head%nchan*(radtail(ibin)%head%nchan+1))/2
                 allocate(radtail(ibin)%head%rsqrtinv(chan_count)) 
                 allocate(rsqrtinv(radtail(ibin)%head%nchan,radtail(ibin)%head%nchan))
                 radtail(ibin)%head%rsqrtinv=zero
                 rsqrtinv=zero
                 call radinfo_get_rsqrtinv(iinstr,radtail(ibin)%head%nchan,radtail(ibin)%head%icx,radtail(ibin)%head%ich,&
                                           radtail(ibin)%head%err2,rsqrtinv)
                 chan_count=0
                 do ii=1,radtail(ibin)%head%nchan
                    do jj=ii,radtail(ibin)%head%nchan
                       chan_count=chan_count+1
                       radtail(ibin)%head%rsqrtinv(chan_count)=rsqrtinv(ii,jj)
                    end do
                 end do
                 deallocate(rsqrtinv)
                 radtail(ibin)%head%use_corr_obs=.true.
              end if

           end if ! icc
        endif ! (in_curbin)

!       Link obs to diagnostics structure
        iii=0
        do ii=1,nchanl
          m=ich(ii)
          if (luse_obsdiag) then
           if (.not.lobsdiag_allocated) then
              if (.not.associated(obsdiags(i_rad_ob_type,ibin)%head)) then
                 allocate(obsdiags(i_rad_ob_type,ibin)%head,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setuprad: failure to allocate obsdiags',istat
                    call stop2(276)
                 end if
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%head
              else
                 allocate(obsdiags(i_rad_ob_type,ibin)%tail%next,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setuprad: failure to allocate obsdiags',istat
                    call stop2(277)
                 end if
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%tail%next
              end if
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%muse(miter+1))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%nldepart(miter+1))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%tldepart(miter))
              allocate(obsdiags(i_rad_ob_type,ibin)%tail%obssen(miter))
              obsdiags(i_rad_ob_type,ibin)%tail%indxglb=(n-1)*nchanl+ii 
              obsdiags(i_rad_ob_type,ibin)%tail%nchnperobs=-99999
              obsdiags(i_rad_ob_type,ibin)%tail%luse=.false.
              obsdiags(i_rad_ob_type,ibin)%tail%muse(:)=.false.
              obsdiags(i_rad_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
              obsdiags(i_rad_ob_type,ibin)%tail%tldepart(:)=zero
              obsdiags(i_rad_ob_type,ibin)%tail%wgtjo=-huge(zero)
              obsdiags(i_rad_ob_type,ibin)%tail%obssen(:)=zero

              n_alloc(ibin) = n_alloc(ibin) +1
              my_diag => obsdiags(i_rad_ob_type,ibin)%tail
              my_diag%idv = is
              my_diag%iob = n 
              my_diag%ich = ii
           else
              if (.not.associated(obsdiags(i_rad_ob_type,ibin)%tail)) then
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%head
              else
                 obsdiags(i_rad_ob_type,ibin)%tail => obsdiags(i_rad_ob_type,ibin)%tail%next
              end if

              if (obsdiags(i_rad_ob_type,ibin)%tail%indxglb/=(n-1)*nchanl+ii) then 
                 write(6,*)'setuprad: index error'
                 call stop2(278)
              endif
           endif

           if(in_curbin) then
              if (ii==1) obsptr => obsdiags(i_rad_ob_type,ibin)%tail
              if (ii==1) obsdiags(i_rad_ob_type,ibin)%tail%nchnperobs = nchanl
              obsdiags(i_rad_ob_type,ibin)%tail%luse = luse(n)
              obsdiags(i_rad_ob_type,ibin)%tail%nldepart(jiter) = tbc(ii)
              obsdiags(i_rad_ob_type,ibin)%tail%wgtjo=wgtjo(ii) 
 
!             Load data into output arrays
              m=ich(ii)
              if (varinv(ii)>tiny_r_kind .and. iuse_rad(m)>=1) then
                 iii=iii+1
                 radtail(ibin)%head%diags(iii)%ptr => obsdiags(i_rad_ob_type,ibin)%tail
                 obsdiags(i_rad_ob_type,ibin)%tail%muse(jiter) = .true.
 
                 ! verify the pointer to obsdiags

                 my_head => radtail(ibin)%head
                 my_diag => radtail(ibin)%head%diags(iii)%ptr

                 if(my_head%idv      /= my_diag%idv .or. &
                    my_head%iob      /= my_diag%iob .or. &
                    my_head%ich(iii) /= my_diag%ich ) then
                    call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                          (/is,i,ii,ibin/))
                    call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,my_head%ich(iii)/))
                    call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
                    call die(myname)
                 endif

              endif
           endif ! (in_curbin)
          end if
        enddo
        if(in_curbin .and. luse_obsdiag) then
           if(.not. retrieval.and.(iii/=icc)) then
              write(6,*)'setuprad: error iii icc',iii,icc
              call stop2(279)
           endif
        endif ! (in_curbin)
 
!    End of l_may_be_passive block
     endif


!    Load passive data into output arrays
     if (passive_bc .and. (jiter>miter) .and. .not. retrieval) then
        if(in_curbin) then
           if(iccm > 0)then
              if(.not. associated(radheadm(ibin)%head))then

                 allocate(radheadm(ibin)%head,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write radheadm '
                 radtailm(ibin)%head => radheadm(ibin)%head
              else
                 allocate(radtailm(ibin)%head%llpoint,stat=istat)
                 if(istat /= 0)write(6,*)' failure to write radtailm%llpoint '
                 radtailm(ibin)%head => radtailm(ibin)%head%llpoint
              end if

              my_headm => radtailm(ibin)%head
              my_headm%idv = is
              my_headm%iob = n

              allocate(radtailm(ibin)%head%res(iccm),radtailm(ibin)%head%err2(iccm), &
                       radtailm(ibin)%head%raterr2(iccm),radtailm(ibin)%head%pred(npred,iccm), &
                       radtailm(ibin)%head%ich(iccm), & 
                       radtailm(ibin)%head%icx(iccm))

              radtailm(ibin)%head%nchan  = iccm        ! profile observation count
              radtailm(ibin)%head%time=dtime
              radtailm(ibin)%head%luse=luse(n)
              radtailm(ibin)%head%ich(:)=-1
              radtailm(ibin)%head%isis=isis
              radtailm(ibin)%head%isfctype=isfctype
              iii=0
              do ii=1,nchanl
                 m=ich(ii)
                 channel_passive=iuse_rad(m)==-1 .or. iuse_rad(m)==0
                 if (varinv(ii)>tiny_r_kind .and. channel_passive) then

                    iii=iii+1
                    radtailm(ibin)%head%res(iii)=tbc(ii)                 ! obs-ges innovation
                    radtailm(ibin)%head%err2(iii)=one/error0(ii)**2      ! 1/(obs error)**2  (original uninflated error)
                    radtailm(ibin)%head%raterr2(iii)=error0(ii)**2*varinv(ii) ! (original error)/(inflated error)
                    radtailm(ibin)%head%icx(iii)=m                       ! channel index
                    do k=1,npred
                       radtailm(ibin)%head%pred(k,iii)=pred(k,ii)*cld_rbc_idx(ii)*upd_pred(k)
                    end do

                    my_headm%ich(iii)=ii 

!                   compute hessian contribution,
!                   skip rstats accumulation for channels without coef. initialization
                    if (newpc4pred .and. luse(n) .and. any(predx(:,m)/=zero)) then
                       do k=1,npred
                          rstats(k,m)=rstats(k,m)+radtailm(ibin)%head%pred(k,iii) &
                             *radtailm(ibin)%head%pred(k,iii)*varinv(ii)
                       end do
                    end if  ! end of newpc4pred loop

                 end if
              end do

              if (iii /= iccm) then
                 write(6,*)'setuprad: error iii iccm',iii,iccm
                 call stop2(279)
              endif

              radtailm(ibin)%head%nchan = iii         ! profile observation count
           end if ! <iccm>
        endif ! (in_curbin)
     end if   !    End of passive_bc block


     if(in_curbin) then
!       Write diagnostics to output file.
        if (rad_diagsave .and. luse(n) .and. nchanl_diag > 0) then
           diagbuf(1)  = cenlat                         ! observation latitude (degrees)
           diagbuf(2)  = cenlon                         ! observation longitude (degrees)
           diagbuf(3)  = zsges                          ! model (guess) elevation at observation location
 
           diagbuf(4)  = dtime-time_offset              ! observation time (hours relative to analysis time)

           diagbuf(5)  = data_s(iscan_pos,n)            ! sensor scan position 
           diagbuf(6)  = zasat*rad2deg                  ! satellite zenith angle (degrees)
           diagbuf(7)  = data_s(ilazi_ang,n)            ! satellite azimuth angle (degrees)
           diagbuf(8)  = pangs                          ! solar zenith angle (degrees)
           diagbuf(9)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
           diagbuf(10) = sgagl                          ! sun glint angle (degrees) (sgagl)
 
           diagbuf(11) = surface(1)%water_coverage         ! fractional coverage by water
           diagbuf(12) = surface(1)%land_coverage          ! fractional coverage by land
           diagbuf(13) = surface(1)%ice_coverage           ! fractional coverage by ice
           diagbuf(14) = surface(1)%snow_coverage          ! fractional coverage by snow
           if(.not. retrieval)then
              diagbuf(15) = surface(1)%water_temperature      ! surface temperature over water (K)
              diagbuf(16) = surface(1)%land_temperature       ! surface temperature over land (K)
              diagbuf(17) = surface(1)%ice_temperature        ! surface temperature over ice (K)
              diagbuf(18) = surface(1)%snow_temperature       ! surface temperature over snow (K)
              diagbuf(19) = surface(1)%soil_temperature       ! soil temperature (K)
              if (gmi .or. saphir) then
                diagbuf(20) = gwp                             ! graupel water path
              else
                diagbuf(20) = surface(1)%soil_moisture_content  ! soil moisture
              endif
              diagbuf(21) = surface(1)%land_type              ! surface land type
           else
              diagbuf(15) = tsavg5                            ! SST first guess used for SST retrieval
              diagbuf(16) = sstcu                             ! NCEP SST analysis at t            
              diagbuf(17) = sstph                             ! Physical SST retrieval             
              diagbuf(18) = sstnv                             ! Navy SST retrieval               
              diagbuf(19) = dta                               ! d(ta) corresponding to sstph
              diagbuf(20) = dqa                               ! d(qa) corresponding to sstph
              diagbuf(21) = dtp_avh                           ! data type             
           endif
           if(lcw4crtm .and. sea) then  
           !  diagbuf(22) = tpwc_amsua   
              diagbuf(22) = scat                              ! scattering index from AMSU-A 
              diagbuf(23) = clw_guess                         ! integrated CLWP (kg/m**2) from background                
           else
              diagbuf(22) = surface(1)%vegetation_fraction    ! vegetation fraction
              diagbuf(23) = surface(1)%snow_depth             ! snow depth
           endif
           diagbuf(24) = surface(1)%wind_speed             ! surface wind speed (m/s)
 
!          Note:  The following quantities are not computed for all sensors
           if (.not.microwave) then
              diagbuf(25)  = cld                              ! cloud fraction (%)
              diagbuf(26)  = cldp                             ! cloud top pressure (hPa)
           else
              if((lcw4crtm .and. sea) .or. gmi .or. amsr2) then
                 if (gmi .or. amsr2) then
                   diagbuf(25)  = clw_obs                       ! clw (kg/m**2) from retrievals
                 else
                   diagbuf(25)  = clwp_amsua                    ! cloud liquid water (kg/m**2)
                 endif
                 diagbuf(26)  = clw_guess_retrieval        ! retrieved CLWP (kg/m**2) from simulated BT                   
              else
                 diagbuf(25)  = clw                           ! cloud liquid water (kg/m**2)
                 diagbuf(26)  = tpwc                          ! total column precip. water (km/m**2)
              endif
           endif

!          For NST
           diagbuf(27) = data_s(itref,n)
           diagbuf(28) = data_s(idtw,n)
           diagbuf(29) = data_s(idtc,n)
           diagbuf(30) = data_s(itz_tr,n)

           if (lwrite_peakwt) then
              do i=1,nchanl_diag
                 diagbufex(1,i)=weightmax(ich_diag(i))   ! press. at max of weighting fn (mb)
              end do
              if (goes_img) then
                 do i=1,nchanl_diag
                    diagbufex(2,i)=tb_obs_sdv(ich_diag(i))
                 end do
              end if
           else if (goes_img .and. .not.lwrite_peakwt) then
              do i=1,nchanl_diag
                 diagbufex(1,i)=tb_obs_sdv(ich_diag(i))
              end do
          end if

           do i=1,nchanl_diag
              diagbufchan(1,i)=tb_obs(ich_diag(i))       ! observed brightness temperature (K)
              diagbufchan(2,i)=tbc(ich_diag(i))          ! observed - simulated Tb with bias corrrection (K)
              diagbufchan(3,i)=tbcnob(ich_diag(i))       ! observed - simulated Tb with no bias correction (K)
              errinv = sqrt(varinv(ich_diag(i)))
              diagbufchan(4,i)=errinv                    ! inverse observation error
              useflag=one
              if (iuse_rad(ich(ich_diag(i))) < 1) useflag=-one
              diagbufchan(5,i)= id_qc(ich_diag(i))*useflag            ! quality control mark or event indicator

              if (lcw4crtm) then             
                 diagbufchan(6,i)=error0(ich_diag(i))
              else
                 diagbufchan(6,i)=emissivity(ich_diag(i))             ! surface emissivity
              endif
              diagbufchan(7,i)=tlapchn(ich_diag(i))                   ! stability index
              if (lcw4crtm) then
                 diagbufchan(8,i)=cld_rbc_idx(ich_diag(i))            ! indicator of cloudy consistency
              else
                 diagbufchan(8,i)=ts(ich_diag(i))                     ! d(Tb)/d(Ts)
              end if

              if (lwrite_predterms) then
                 predterms=zero
                 do j = 1,npred
                    predterms(j) = pred(j,ich_diag(i))
                 end do
                 predterms(npred+1) = cbias(nadir,ich(ich_diag(i)))

                 do j=1,npred+2
                    diagbufchan(ipchan_radiag+j,i)=predterms(j) ! Tb bias correction terms (K)
                 end do
              else   ! Default to write out predicted bias
                 do j=1,npred+2
                    diagbufchan(ipchan_radiag+j,i)=predbias(j,ich_diag(i)) ! Tb bias correction terms (K)
                 end do
              end if
           end do

           if (lobsdiagsave) then
              if (l_may_be_passive) then
                 do ii=1,nchanl_diag
                    if (.not.associated(obsptr)) then
                       write(6,*)'setuprad: error obsptr'
                       call stop2(280)
                    end if
                    if (obsptr%indxglb/=(n-1)*nchanl+ii) then 
                       write(6,*)'setuprad: error writing diagnostics'
                       call stop2(281)
                    end if
 
                    ioff=ipchan_radiag+npred+2
                    do jj=1,miter
                       ioff=ioff+1
                       if (obsptr%muse(jj)) then
                          diagbufchan(ioff,ich_diag(ii)) = one
                       else
                          diagbufchan(ioff,ich_diag(ii)) = -one
                       endif
                    enddo
                    do jj=1,miter+1
                       ioff=ioff+1
                       diagbufchan(ioff,ich_diag(ii)) = obsptr%nldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ich_diag(ii)) = obsptr%tldepart(jj)
                    enddo
                    do jj=1,miter
                       ioff=ioff+1
                       diagbufchan(ioff,ich_diag(ii)) = obsptr%obssen(jj)
                    enddo

                    obsptr => obsptr%next
                 enddo
              else
                 ioff=ipchan_radiag+npred+2
                 diagbufchan(ioff+1:ioff+4*miter+1,1:nchanl_diag) = zero
              endif
           endif

           if (.not.lextra) then
              write(4) diagbuf,diagbufchan
           else
              write(4) diagbuf,diagbufchan,diagbufex
           endif

        end if
     endif ! (in_curbin)


! End of n-loop over obs
  end do

! If retrieval, close open bufr sst file (output)
  if (retrieval.and.last_pass) call finish_sst_retrieval

! Jump here when there is no data to process for current satellite
! Deallocate arrays
  deallocate(diagbufchan)
  deallocate(sc_index)

  if (rad_diagsave) then
     call dtime_show(myname,'diagsave:rad',i_rad_ob_type)
     close(4)
     if (lextra .and. allocated(diagbufex)) deallocate(diagbufex)
  endif

  call destroy_crtm

135 continue

! End of routine
  return

 end subroutine setuprad

