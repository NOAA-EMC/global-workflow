!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupt --- Compute rhs of oi for temperature obs
!
! !INTERFACE:
!
subroutine setupt(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)

! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind

  use obsmod, only: ttail,thead,sfcmodel,perturb_obs,oberror_tune,&
       i_t_ob_type,obsdiags,lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: t_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use qcmod, only: npres_print,dfact,dfact1,ptop,pbot,buddycheck_t
  use qcmod, only: njqc,vqc

  use oneobmod, only: oneobtest
  use oneobmod, only: maginnov
  use oneobmod, only: magoberr

  use gridmod, only: nsig,twodvar_regional,regional
  use gridmod, only: get_ijk
  use jfunc, only: jiter,last,jiterstart,miter

  use guess_grids, only: nfldsig, hrdifsig,ges_lnprsl,&
       geop_hgtl,ges_tsen,pt_ll,pbl_height

  use constants, only: zero, one, four,t0c,rd_over_cp,three,rd_over_cp_mass,ten
  use constants, only: tiny_r_kind,half,two,cg_term
  use constants, only: huge_single,r1000,wgtlim,r10,fv
  use constants, only: one_quad
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,icsubtype
  use converr_t, only: ptabl_t 
  use converr, only: ptabl
  use rapidrefresh_cldsurf_mod, only: l_gsd_terrain_match_surftobs,l_sfcobserror_ramp_t
  use rapidrefresh_cldsurf_mod, only: l_pbl_pseudo_surfobst, pblh_ration,pps_press_incr
  use rapidrefresh_cldsurf_mod, only: i_use_2mt4b,i_sfct_gross

  use aircraftinfo, only: npredt,predt,aircraft_t_bc_pof,aircraft_t_bc, &
       aircraft_t_bc_ext,ostats_t,rstats_t,upd_pred_t

  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use buddycheck_mod, only: buddy_check_t

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind)                                  , intent(in   ) :: lunin   ! file unit from which to read observations
  integer(i_kind)                                  , intent(in   ) :: mype    ! mpi task id
  integer(i_kind)                                  , intent(in   ) :: nele    ! number of data elements per observation
  integer(i_kind)                                  , intent(in   ) :: nobs    ! number of observations
  integer(i_kind)                                  , intent(in   ) :: is      ! ndat index
  logical                                          , intent(in   ) :: conv_diagsave   ! logical to save innovation dignostics


! !INPUT/OUTPUT PARAMETERS:

                                                            ! array containing information ...
  real(r_kind),dimension(npres_print,nconvtype,5,3), intent(inout) :: bwork !  about o-g stats
  real(r_kind),dimension(100+7*nsig)               , intent(inout) :: awork !  for data counts and gross checks

! !DESCRIPTION:  For temperature observations, this routine
! \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
! \end{enumerate}
!
! !REVISION HISTORY:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of twork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  -modified variational qc and diagnostic output
!   2005-10-27  su - correct error in longitude index for diagnostic output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29 derber - remove psfcg and use ges_lnps instead
!   2005-12-20  parrish - add boundary layer forward model option
!   2005-12-20  parrish - correct dimension error in declaration of prsltmp
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - optimize and fix bugs due to virtual temperature
!   2006-04-11  park    - reset land mask for surface data based on observation type
!   2006-04-27  park    - remove sensitivity test for surface TLM routine
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc for surface model
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2006-09-28  treadon - add 10m wind factor to sfc_wtq_fwd call
!   2006-10-28       su - turn off rawinsonde Vqc at south hemisphere
!   2007-03-09      su - modify the observation perturbation 
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the observation gross check error 
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-21  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-07  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2010-06-10  Hu      - add call for terrain match for surface T obs    
!   2011-05-06  Su      - modify the observation gross check error
!   2011-12-14  wu      - add code for rawinsonde level enhancement ( ext_sonde )
!   2011-10-14  Hu      - add code for adjusting surface temperature observation error
!   2011-10-14  Hu      - add code for producing pseudo-obs in PBL 
!                                       layer based on surface obs T
!   2011-10-14  Hu      - add code for using 2-m temperature as background to
!                            calculate surface temperauture observation
!                            innovation
!   2013-01-26  parrish - change grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a11,
!                          tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-05-17  zhu     - add contribution from aircraft temperature bias correction
!                       - with option aircraft_t_bc_pof or aircraft_t_bc
!   2013-05-24  wu      - move rawinsonde level enhancement ( ext_sonde ) to read_prepbufr
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (idia) to header of diagfile
!   2014-03-04  sienkiewicz - implementation of option aircraft_t_bc_ext (external table)
!   2014-04-12  su      - add non linear qc from Purser's scheme
!   2014-10-01  zhu     - apply aircraft temperature bias correction to kx=130
!   2014-10-06  carley  - add call to buddy check for twodvar_regional option
!   2014-12-30  derber  - Modify for possibility of not using obsdiag
!   2015-12-21  yang    - Parrish's correction to the previous code in new
!   
!
! !REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind

  character(len=*),parameter :: myname='setupt'

! Declare external calls for code analysis
  external:: SFC_WTQ_FWD
  external:: get_tlm_tsfc
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  
  real(r_double) rstation_id
  real(r_kind) rsig,drpx,rsigp
  real(r_kind) psges,sfcchk,pres_diff,rlow,rhgh,ramp
  real(r_kind) pof_idx,poaf,effective
  real(r_kind) tges
  real(r_kind) obserror,ratio,val2,obserrlm,ratiosfc
  real(r_kind) residual,ressw2,scale,ress,ratio_errors,tob,ddiff
  real(r_kind) val,valqc,dlon,dlat,dtime,dpres,error,prest,rwgt,var_jb
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind) cg_t,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2,qcgross
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(npredt):: predbias
  real(r_kind),dimension(npredt):: pred
  real(r_kind),dimension(npredt):: predcoef
  real(r_kind) tgges,roges
  real(r_kind),dimension(nsig):: tvtmp,qtmp,utmp,vtmp,hsges
  real(r_kind) u10ges,v10ges,t2ges,q2ges,psges2,f10ges
  real(r_kind),dimension(34) :: ptablt
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  real(r_kind),dimension(nsig):: prsltmp2

  integer(i_kind) i,j,nchar,nreal,k,ii,jj,l,nn,ibin,idia,idia0,ix,ijb
  integer(i_kind) mm1,jsig,iqt
  integer(i_kind) itype,msges
  integer(i_kind) ier,ilon,ilat,ipres,itob,id,itime,ikx,iqc,iptrb,icat,ipof,ivvlc,idx
  integer(i_kind) ier2,iuse,ilate,ilone,ikxx,istnelv,iobshgt,izz,iprvd,isprvd
  integer(i_kind) regime,istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr

  integer(i_kind),dimension(nobs):: buddyuse

  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical,dimension(nobs):: luse,muse
  logical sfctype
  logical iqtflg
  logical aircraftobst

  logical:: in_curbin, in_anybin
  logical proceed
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(t_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  real(r_kind) :: thisPBL_height,ratio_PBL_height,prestsfc,diffsfc,dthetav
  real(r_kind) :: tges2m,qges2m

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_q2
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_th2

  n_alloc(:)=0
  m_alloc(:)=0

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!  call GSD terrain match for surface temperature observation
  if(l_gsd_terrain_match_surftobs) then
     call gsd_terrain_match_surfTobs(mype,nele,nobs,data)
  endif

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  itob=5      ! index of t observation
  id=6        ! index of station id
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of ob type
  iqt=9       ! index of flag indicating if moisture ob available
  iqc=10      ! index of quality mark
  ier2=11     ! index of original-original obs error ratio
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  istnelv=19  ! index of station elevation (m)
  iobshgt=20  ! index of observation height (m)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  ijb=25      ! index of non linear qc parameter
  if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) then
     ipof=26     ! index of data pof
     ivvlc=27    ! index of data vertical velocity
     idx=28      ! index of tail number
     iptrb=29    ! index of t perturbation
  else
     iptrb=26    ! index of t perturbation
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  if (twodvar_regional .and. buddycheck_t) call buddy_check_t(is,data,luse,mype,nele,nobs,muse,buddyuse)
  var_jb=zero

!  handle multiple reported data at a station
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k) == data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=19
    if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) &
          nreal=nreal+npredt+2
     idia0=nreal
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     rdiagbuf=zero
  end if
  scale=one
  rsig=float(nsig)
  mm1=mype+1

!  rsli=isli
  rsigp=rsig+one
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        ! Convert obs lats and lons to grid coordinates
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        dpres=data(ipres,i)
        error=data(ier2,i)
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx)
        rstation_id     = data(id,i)
        prest=r10*exp(dpres)     ! in mb
        sfctype=(itype>179.and.itype<190).or.(itype>=192.and.itype<=199)
  
        iqtflg=nint(data(iqt,i)) == 0
        var_jb=data(ijb,i)
!       write(6,*) 'SETUPT:itype,var_jb,ijb=',itype,var_jb,ijb

!       Load observation value and observation error into local variables
        tob=data(itob,i)
        obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
     endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_t_ob_type,ibin)%head)) then
              allocate(obsdiags(i_t_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupt: failure to allocate obsdiags',istat
                 call stop2(298)
              end if
              obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%head
           else
              allocate(obsdiags(i_t_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupt: failure to allocate obsdiags',istat
                 call stop2(298)
              end if
              obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_t_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_t_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_t_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_t_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_t_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_t_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_t_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_t_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_t_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_t_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_t_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_t_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_t_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_t_ob_type,ibin)%tail)) then
              obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%head
           else
              obsdiags(i_t_ob_type,ibin)%tail => obsdiags(i_t_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_t_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupt: index error'
              call stop2(300)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

!    Compute bias correction for aircraft data
     if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) then 
        pof_idx = zero
        do j = 1, npredt
           pred(j) = zero
           predbias(j) = zero
        end do
     end if

!    aircraftobst = itype>129.and.itype<140
     aircraftobst = (itype==131) .or. (itype>=133 .and. itype<=135) .or. (itype==130) !for currently known types
     ix = 0
     if (aircraftobst .and. (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext)) then 
        ix = data(idx,i)
        if (ix==0) then
!          Inflate obs error for new tail number
           if ( .not. aircraft_t_bc_ext )  &
              data(ier,i) = 1.2_r_kind*data(ier,i)
        else
!          Bias for existing tail numbers
           do j = 1, npredt
              predcoef(j) = predt(j,ix)
           end do

!          inflate obs error for any uninitialized tail number
           if (all(predcoef==zero) .and. .not. aircraft_t_bc_ext) then
              data(ier,i) = 1.2_r_kind*data(ier,i)
           end if

!          define predictors
           if (aircraft_t_bc) then
              pof_idx = one
              pred(1) = one
              if (abs(data(ivvlc,i))>=50.0_r_kind) then
                 pred(2) = zero
                 pred(3) = zero
                 data(ier,i) = 1.2_r_kind*data(ier,i)
              else
                 pred(2) = data(ivvlc,i)
                 pred(3) = data(ivvlc,i)*data(ivvlc,i)
              end if
           end if
           if (aircraft_t_bc_pof) then
!             data(ipof,i)==5 (ascending); 6 (descending); 3 (cruise level)
              if (data(ipof,i) == 3.0_r_kind) then 
                 pof_idx = one
                 pred(1) = one
                 pred(2) = zero
                 pred(3) = zero
              else if (data(ipof,i) == 6.0_r_kind) then 
                 pof_idx = one
                 pred(1) = zero
                 pred(2) = zero
                 pred(3) = one
              else if (data(ipof,i) == 5.0_r_kind) then
                 pof_idx = one
                 pred(1) = zero
                 pred(2) = one
                 pred(3) = zero
              else
                 pof_idx = zero
                 pred(1) = one
                 pred(2) = zero
                 pred(3) = zero
              end if
           end if

           if (aircraft_t_bc_ext) pred(1) = one

           do j = 1, npredt
              predbias(j) = predcoef(j)*pred(j)
           end do
        end if
     end if

! Interpolate log(ps) & log(pres) at mid-layers to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     drpx=zero
     if(sfctype .and. .not.twodvar_regional) then
        drpx=abs(one-((one/exp(dpres-log(psges))))**rd_over_cp)*t0c
     end if

!    Put obs pressure in correct units to get grid coord. number
     call grdcrd1(dpres,prsltmp(1),nsig,-1)

! Implementation of forward model ----------

     if(sfctype.and.sfcmodel) then
        tgges=data(iskint,i)
        roges=data(isfcr,i)

        msges = 0
        if(itype == 180 .or. itype == 182 .or. itype == 183) then    !sea
           msges=0
        elseif(itype == 181 .or. itype == 187 .or. itype == 188) then  !land
           msges=1
        endif

        call tintrp2a1(ges_tv,tvtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_q,qtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_u,utmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_v,vtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(geop_hgtl,hsges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
  
        psges2  = psges          ! keep in cb
        prsltmp2 = exp(prsltmp)  ! convert from ln p to cb
        call SFC_WTQ_FWD (psges2, tgges,&
             prsltmp2(1), tvtmp(1), qtmp(1), utmp(1), vtmp(1), &
             prsltmp2(2), tvtmp(2), qtmp(2), hsges(1), roges, msges, &
             f10ges,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)
        tges = t2ges

     else
        if(iqtflg)then
!          Interpolate guess tv to observation location and time
           call tintrp31(ges_tv,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,mype,nfldsig)

        else
!          Interpolate guess tsen to observation location and time
           call tintrp31(ges_tsen,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,mype,nfldsig)
        end if

        if(i_use_2mt4b>0 .and. sfctype) then
!          Interpolate guess th 2m to observation location and time
           call tintrp2a11(ges_th2,tges2m,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
           tges2m=tges2m*(r10*psges/r1000)**rd_over_cp_mass  ! convert to sensible T         
           if(iqtflg)then
              call tintrp2a11(ges_q2,qges2m,dlat,dlon,dtime,hrdifsig,&
                     mype,nfldsig)
              tges2m=tges2m*(one+fv*qges2m)  ! convert to virtual T
           endif
        endif

     endif

!    Get approximate k value of surface by using surface pressure
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp(1),nsig,-1)

!    Check to see if observations is above the top of the model (regional mode)
     if(sfctype)then
        if(abs(dpres)>four) drpx=1.0e10_r_kind
        pres_diff=prest-r10*psges
        if (twodvar_regional .and. abs(pres_diff)>=r1000) drpx=1.0e10_r_kind
     end if
     rlow=max(sfcchk-dpres,zero)
! linear variation of observation ramp [between grid points 1(~3mb) and 15(~45mb) below the surface]
     if(l_sfcobserror_ramp_t) then
        ramp=min(max(((rlow-1.0_r_kind)/(15.0_r_kind-1.0_r_kind)),0.0_r_kind),1.0_r_kind)
     else
        ramp=rlow
     endif

     rhgh=max(zero,dpres-rsigp-r0_001)

     if(sfctype.and.sfcmodel)  dpres = one     ! place sfc T obs at the model sfc

     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if
     
     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+r8*ramp)
     error=one/error
!    if (dpres > rsig) ratio_errors=zero
     if (dpres > rsig )then
        if( regional .and. prest > pt_ll )then
           dpres=rsig
        else
           ratio_errors=zero
        endif
     endif

! Compute innovation
     if(i_use_2mt4b>0 .and. sfctype) then
        ddiff = tob-tges2m
     else
        ddiff = tob-tges
     endif

! Apply bias correction to innovation
     if (aircraftobst .and. (aircraft_t_bc_pof .or. aircraft_t_bc .or. &
            aircraft_t_bc_ext)) then
        do j = 1, npredt
           ddiff = ddiff - predbias(j) 
        end do
     end if

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff = maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross error checks

     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     ratiosfc = ddiff/obserrlm

 ! modify gross check limit for quality mark=3
     if(data(iqc,i) == three ) then
        qcgross=r0_7*cgross(ikx)
     else
        qcgross=cgross(ikx)
     endif

     if (twodvar_regional) then

        ! Gross error relaxation for when buddycheck_t==.true.
        if (buddycheck_t) then 
           if (buddyuse(i)==1) then
              ! - Passed buddy check, relax gross qc
              qcgross=three*qcgross
              data(iuse,i)=data(iuse,i)+0.50_r_kind ! So we can identify obs with relaxed gross qc
                                                    ! in diag files  (will show as an extra 0.50 appended) 
           else if (buddyuse(i)==0) then
              ! - Buddy check did not run (too few buddies, rusage >= 100, outside twindow, etc.)
              ! - In the case of an isolated ob in complex terrain, see about relaxing the the gross qc 
              if ( (data(iuse,i)-real(int(data(iuse,i)),kind=r_kind)) == 0.25_r_kind) then 
                 qcgross=three*qcgross               ! Terrain aware modification
                                                     ! to gross error check
              end if         
           else if (buddyuse(i)==-1) then
              ! - Observation has failed the buddy check - reject.
              ratio_errors = zero
           end if
        else if ( (data(iuse,i)-real(int(data(iuse,i)),kind=r_kind)) == 0.25_r_kind) then 
          qcgross=three*qcgross               ! Terrain aware modification
                                              ! to gross error check       
        end if  
     endif

     if (sfctype .and. i_sfct_gross==1) then
! extend the threshold for surface T
        if(i_use_2mt4b<=0) tges2m=tges
        if ( tges2m < 5.0_r_single) then
           if (ratiosfc > 1.4_r_single*qcgross &
              .or. ratiosfc < -2.4_r_single*qcgross  &
              .or. ratio_errors < tiny_r_kind) then
              if (luse(i)) awork(4) = awork(4)+one
              error = zero
              ratio_errors = zero
           else
              ratio_errors = ratio_errors/sqrt(dup(i))
           end if
        else
           if (ratiosfc > qcgross .or. ratiosfc < -1.4_r_single*qcgross  &
              .or. ratio_errors < tiny_r_kind) then
              if (luse(i)) awork(4) = awork(4)+one
              error = zero
              ratio_errors = zero
           else
              ratio_errors = ratio_errors/sqrt(dup(i))
           end if
        endif
     else
        if (ratio > qcgross .or. ratio_errors < tiny_r_kind) then
           if (luse(i)) awork(4) = awork(4)+one
           error = zero
           ratio_errors = zero
        else
           ratio_errors = ratio_errors/sqrt(dup(i))
        end if
     endif
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_t_ob_type,ibin)%tail%muse(nobskeep)

!    Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              ddiff=ddiff+data(iptrb,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           ddiff=ddiff+data(iptrb,i)/error/ratio_errors
        endif
     endif

!    Compute penalty terms
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if(njqc .and. var_jb>tiny_r_kind .and. var_jb < 10.0_r_kind .and. error >tiny_r_kind)  then
           if(exp_arg  == zero) then
              wgt=one
           else
              wgt=ddiff*error/sqrt(two*var_jb)
              wgt=tanh(wgt)/wgt
           endif
           term=-two*var_jb*rat_err2*log(cosh((val)/sqrt(two*var_jb)))
           rwgt = wgt/wgtlim
           valqc = -two*term
        else if (vqc .and. cvar_pg(ikx)> tiny_r_kind .and. error >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_t=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_t*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        else
           term = exp_arg
           wgt  = one 
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        endif

!       Accumulate statistics for obs belonging to this task
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
           awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
           awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2*rat_err2
        end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
        ress   = ddiff*scale
        ressw2 = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(prest >ptop(k) .and. prest <= pbot(k))then
              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress           ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2         ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
              
           end if
        end do
     end if

!    Fill obs diagnostics structure
     if(luse_obsdiag)then
        obsdiags(i_t_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_t_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_t_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_t_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if

!    If obs is "acceptable_te", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
!    if ( .not. last .and. muse(i)) then
     if (muse(i)) then

        if(.not. associated(thead(ibin)%head))then
           allocate(thead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write thead '
           ttail(ibin)%head => thead(ibin)%head
        else
           allocate(ttail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write ttail%llpoint '
           ttail(ibin)%head => ttail(ibin)%head%llpoint
        end if

        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => ttail(ibin)%head
        my_head%idv = is
        my_head%iob = i

        allocate(ttail(ibin)%head%pred(npredt))

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,ttail(ibin)%head%ij(1),ttail(ibin)%head%wij(1))

        ttail(ibin)%head%res     = ddiff
        ttail(ibin)%head%err2    = error**2
        ttail(ibin)%head%raterr2 = ratio_errors**2      
        ttail(ibin)%head%time    = dtime
        ttail(ibin)%head%b       = cvar_b(ikx)
        ttail(ibin)%head%pg      = cvar_pg(ikx)
        ttail(ibin)%head%jb      = var_jb
        ttail(ibin)%head%use_sfc_model = sfctype.and.sfcmodel
        if(ttail(ibin)%head%use_sfc_model) then
           call get_tlm_tsfc(ttail(ibin)%head%tlm_tsfc(1), &
                psges2,tgges,prsltmp2(1), &
                tvtmp(1),qtmp(1),utmp(1),vtmp(1),hsges(1),roges,msges, &
                regime,iqtflg)
        else
           ttail(ibin)%head%tlm_tsfc = zero
        endif
        ttail(ibin)%head%luse    = luse(i)
        ttail(ibin)%head%tv_ob   = iqtflg

        if (aircraft_t_bc_pof .or. aircraft_t_bc) then
           effective=upd_pred_t*pof_idx
           ttail(ibin)%head%idx = data(idx,i)
           do j=1,npredt
              ttail(ibin)%head%pred(j) = pred(j)*effective
           end do
        end if


!       summation of observation number
        if (luse(i) .and. aircraftobst .and. (aircraft_t_bc_pof .or. aircraft_t_bc) .and. ix/=0) then
           do j=1,npredt
              if (aircraft_t_bc_pof) then
                 poaf=data(ipof,i)
                 if (poaf==3.0_r_kind .or. poaf==5.0_r_kind .or. poaf==6.0_r_kind) then
                    if (j==1 .and. poaf == 3.0_r_kind) ostats_t(1,ix)  = ostats_t(1,ix) + one_quad
                    if (j==2 .and. poaf == 5.0_r_kind) ostats_t(2,ix)  = ostats_t(2,ix) + one_quad
                    if (j==3 .and. poaf == 6.0_r_kind) ostats_t(3,ix)  = ostats_t(3,ix) + one_quad
                    rstats_t(j,ix)=rstats_t(j,ix)+ttail(ibin)%head%pred(j) &
                              *ttail(ibin)%head%pred(j)*(ratio_errors*error)**2*effective
                 end if
              end if

              if (aircraft_t_bc) then
                 if (j==1) ostats_t(1,ix)  = ostats_t(1,ix) + one_quad*effective
                 rstats_t(j,ix)=rstats_t(j,ix)+ttail(ibin)%head%pred(j) &
                               *ttail(ibin)%head%pred(j)*(ratio_errors*error)**2*effective
              end if

           end do
        end if

        if(oberror_tune) then
           ttail(ibin)%head%kx=ikx
           ttail(ibin)%head%tpertb=data(iptrb,i)/error/ratio_errors
           if (njqc) then
              ptablt=ptabl_t
           else
              ptablt=ptabl
           endif
             
           if(prest > ptablt(2))then
              ttail(ibin)%head%k1=1
           else if( prest <= ptablt(33)) then
              ttail(ibin)%head%k1=33
           else
              k_loop: do k=2,32
                 if(prest > ptablt(k+1) .and. prest <= ptablt(k)) then
                    ttail(ibin)%head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        if(luse_obsdiag)then
           ttail(ibin)%head%diags => obsdiags(i_t_ob_type,ibin)%tail

           my_head => ttail(ibin)%head
           my_diag => ttail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                    (/is,i,ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        endif

     endif

! Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = prest              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = data(iqt,i)        ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif
        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

!rdiagbuf(13,ii) is the combination of var_jb and non-linear qc relative weight
! in the format of:  var_jb*1.0e+6 + rwgt
        rdiagbuf(13,ii) = var_jb*1.0e+6 + rwgt ! combination of var_jb and rwgt
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)

        rdiagbuf(17,ii) = data(itob,i)       ! temperature observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = tob-tges           ! obs-ges w/o bias correction (K) (future slot)
        if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) then
           rdiagbuf(20,ii) = data(ipof,i)       ! data pof
           rdiagbuf(21,ii) = data(ivvlc,i)      ! data vertical velocity
           do j=1,npredt
              rdiagbuf(21+j,ii) = predbias(j)
           end do
        end if
        idia=idia0
        if (lobsdiagsave) then
           do jj=1,miter
              idia=idia+1
              if (obsdiags(i_t_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,ii) = one
              else
                 rdiagbuf(idia,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_t_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

        if (twodvar_regional) then
           rdiagbuf(idia+1,ii) = data(idomsfc,i) ! dominate surface type
           rdiagbuf(idia+2,ii) = data(izz,i)     ! model terrain at observation location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

     end if


!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!
     if( .not. last .and. l_pbl_pseudo_surfobst .and.         &
         ( itype==181 .or. itype==183 .or.itype==187 )  .and. &
           muse(i) .and. dpres > -1.0_r_kind ) then
        prestsfc=prest
        diffsfc=ddiff
        dthetav=ddiff*(r1000/prestsfc)**rd_over_cp_mass

        call tintrp2a11(pbl_height,thisPBL_height,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
!
        if (dthetav< -1.0_r_kind) then
           call tune_pbl_height(mype,station_id,dlat,dlon,prestsfc,thisPBL_height,dthetav)
        endif
!
        ratio_PBL_height = (prest - thisPBL_height) * pblh_ration
        if(ratio_PBL_height > zero) thisPBL_height = prest - ratio_PBL_height
        prest = prest - pps_press_incr
        DO while (prest > thisPBL_height)
           ratio_PBL_height=1.0_r_kind-(prestsfc-prest)/(prestsfc-thisPBL_height)

           allocate(ttail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write ttail%llpoint '
           ttail(ibin)%head => ttail(ibin)%head%llpoint

!!! find tob (tint)
           tob=data(itob,i)

!    Put obs pressure in correct units to get grid coord. number
           dpres=log(prest/r10)
           call grdcrd1(dpres,prsltmp(1),nsig,-1)

!!! find tges (tgint)
           if(iqtflg)then
!          Interpolate guess tv to observation location and time
              call tintrp31(ges_tv,tges,dlat,dlon,dpres,dtime, &
                   hrdifsig,mype,nfldsig)

           else
!          Interpolate guess tsen to observation location and time
              call tintrp31(ges_tsen,tges,dlat,dlon,dpres,dtime, &
                   hrdifsig,mype,nfldsig)
           endif

!!! Set (i,j,k) indices of guess gridpoint that bound obs location
           call get_ijk(mm1,dlat,dlon,dpres,ttail(ibin)%head%ij(1),ttail(ibin)%head%wij(1))
!!! find ddiff       
           ddiff = diffsfc*(0.5_r_kind + 0.5_r_kind*ratio_PBL_height)


           error=one/data(ier2,i)
           ttail(ibin)%head%res     = ddiff
           ttail(ibin)%head%err2    = error**2
           ttail(ibin)%head%raterr2 = ratio_errors**2
           ttail(ibin)%head%time    = dtime
           ttail(ibin)%head%b       = cvar_b(ikx)
           ttail(ibin)%head%pg      = cvar_pg(ikx)
           ttail(ibin)%head%jb      = var_jb
           ttail(ibin)%head%use_sfc_model = sfctype.and.sfcmodel
           if(ttail(ibin)%head%use_sfc_model) then
              call get_tlm_tsfc(ttail(ibin)%head%tlm_tsfc(1), &
                   psges2,tgges,prsltmp2(1), &
                   tvtmp(1),qtmp(1),utmp(1),vtmp(1),hsges(1),roges,msges, &
                   regime,iqtflg)
           else
              ttail(ibin)%head%tlm_tsfc = zero
           endif
           ttail(ibin)%head%luse    = luse(i)
           ttail(ibin)%head%tv_ob   = iqtflg

           if(luse_obsdiag)then
              ttail(ibin)%head%diags => obsdiags(i_t_ob_type,ibin)%tail

              my_head => ttail(ibin)%head
              my_diag => ttail(ibin)%head%diags
              if(my_head%idv /= my_diag%idv .or. &
                 my_head%iob /= my_diag%iob ) then
                 call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                       (/is,i,ibin/))
                 call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
                 call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
                 call die(myname)
              endif
           endif

           prest = prest - pps_press_incr

        ENDDO

     endif  ! 181,183,187
!!!!!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!!!!!!!!

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     write(7)'  t',nchar,nreal,ii,mype,idia0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     if (twodvar_regional) then
        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
     endif
  end if


! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_u(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_u(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_v(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_v(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
     if(i_use_2mt4b>0) then
!    get th2m ...
        varname='th2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_z))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_th2(size(rank2,1),size(rank2,2),nfldsig))
            ges_th2(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_th2(:,:,ifld)=rank2
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            call stop2(999)
        endif
!    get q2m ...
        varname='q2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_z))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_q2(size(rank2,1),size(rank2,2),nfldsig))
            ges_q2(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_q2(:,:,ifld)=rank2
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            call stop2(999)
        endif
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupt


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  ifind --- find character string in sorted list
!
! !INTERFACE:
integer function ifind (sid,xsid,nsid)

! !USES:

  implicit none

! !INPUT PARAMETERS:
  integer nsid
  character(len=8) sid(nsid), xsid

! !DESCRIPTION:  Find character string in a sorted list - used to
!                find aircraft tail id from list for bias correction
!
! !REVISION HISTORY:
!
!   2013-04-23  sienkiewicz   Original routine
!
!EOP
!-------------------------------------------------------------------------


! Declare local variables
  integer istart,iend,imid
  
  if (xsid .gt. sid(nsid) .or. xsid .lt. sid(1)) then
     ifind = 0
     return
  end if
  istart=0
  iend=nsid+1
  do while (iend-istart > 1)
     imid=(istart+iend)/2
     if (xsid .eq. sid(imid)) then
        ifind = imid
        return
     else if (xsid .gt. sid(imid)) then
        istart = imid
     else 
        iend = imid
     endif
  end do
  
  if (xsid .eq. sid(iend)) then
     ifind = imid
  else
     ifind = 0
  end if
  return
end function ifind
