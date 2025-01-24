module q_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupq; end interface

contains
subroutine setupq(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupq      compute rhs of oi for moisture observations
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  For moisture observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of qwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-06  treadon - lower huge_error to prevent overflow 
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  - modify variational qc and diagonose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-21  kleist - change to call to genqsat
!   2005-11-21  derber - correct error in use of qsges
!   2005-11-22  wu     - add option to perturb conventional obs
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-03  derber  - fix bug in counting rlow and rhgh
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - eliminate unused arrays
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-09      su - modify obs perturbation
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify gross check error  
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-05-06  Su      - modify the observation gross check error
!   2011-08-09  pondeca - correct bug in qcgross use
!   2011-10-14  Hu      - add code for adjusting surface moisture observation error
!   2011-10-14  Hu      - add code for producing pseudo-obs in PBL 
!   2011-12-14  wu      - add code for rawinsonde level enhancement ( ext_sonde )
!                                       layer based on surface obs Q
!   2013-01-26  parrish - change grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a11,
!                                           tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-05-24  wu      - move rawinsonde level enhancement ( ext_sonde ) to read_prepbufr
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-03-24  Hu      - Use 2/3 of 2m Q and 1/3 of 1st level Q as background
!                           to calculate O-B for the surface moisture observations
!   2014-04-04  todling - revist q2m implementation (slightly)
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2014-11-30  Hu      - more option on use 2-m Q as background
!   2014-12-30  derber  - Modify for possibility of not using obsdiag
!   2015-02-09  Sienkiewicz - handling new KX drifting buoys (formerly ID'd by subtype 562)
!   2015-10-01  guo     - full res obvsr: index to allow redistribution of obsdiags
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-11-29  shlyaeva - save linearized H(x) for EnKF
!   2016-12-09  mccarty - add netcdf_diag capability
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2017-03-31  Hu      -  addd option l_closeobs to use closest obs to analysis
!                                     time in analysis
!   2017-03-31  Hu      -  addd option i_coastline to use observation operater
!                                     for coastline area
!   2018-04-09  pondeca -  introduce duplogic to correctly handle the characterization of
!                          duplicate obs in twodvar_regional applications
!   2019-05-24  Su      -  remove current VQC part and add subroutine call and
!                          add new variational QC option 
!  
!                          duplicate obs in twodvar_regional applications  
!   2020-01-27  Winterbottom - moved the linear regression derived
!                              coefficients for the dynamic observation
!                              error (DOE) calculation to the namelist
!                              level; they are now loaded by
!                              aircraftinfo.
!   2021-10-xx  pondeca/morris/zhao - added observation provider/subprovider
!                         information in diagonostic file, which is used
!                         in offline observation quality control program (AutoObsQC) 
!                         for 3D-RTMA (if l_obsprvdiag is true).
!   2023-03-09 Draper added option to interpolate screen-level q from model 2m output.
!              (hofx_2m_sfcfile)
!   2024-01-11 zhao     - added tdry/tvflg in obs diagnostic files for (2D/3D)RTMA
!
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr,getindex
  use kinds, only: r_kind,r_single,r_double,i_kind

  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,perturb_obs,oberror_tune,&
       lobsdiagsave,nobskeep,lobsdiag_allocated,&
       time_offset,lobsdiag_forenkf,aircraft_recon
  use m_obsNode, only: obsNode
  use m_qNode, only: qNode
  use m_qNode, only: qNode_appendto
  use m_qNode, only: qNode_ich0, qNode_ich0_PBL_Pseudo
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag,ianldate
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use obsmod, only: l_obsprvdiag
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use oneobmod, only: oneobtest,maginnov,magoberr
  use guess_grids, only: ges_lnprsl,hrdifsig,nfldsig,ges_tsen,ges_prsl,pbl_height,ges_qsat
  use gridmod, only: lat2,lon2,nsig,get_ijk,twodvar_regional
  use constants, only: zero,one,r1000,r10,r100
  use constants, only: huge_single,wgtlim,three
  use constants, only: tiny_r_kind,five,half,two,huge_r_kind,r0_01
  use qcmod, only: npres_print,ptopq,pbotq,dfact,dfact1,njqc,vqc,nvqc
  use jfunc, only: jiter,last,jiterstart,miter,superfact,limitqobs,hofx_2m_sfcfile
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: ibeta,ikapa
  use convinfo, only: icsubtype
  use converr_q, only: ptabl_q 
  use converr, only: ptabl 
  use m_dtime, only: dtime_setup, dtime_check
  use rapidrefresh_cldsurf_mod, only: l_sfcobserror_ramp_q
  use rapidrefresh_cldsurf_mod, only: l_pbl_pseudo_surfobsq,pblh_ration,pps_press_incr, &
                                      i_use_2mq4b,l_closeobs,i_coastline
  use rapidrefresh_cldsurf_mod, only: l_rtma3d
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use state_vectors, only: svars3d, levels
  use hdraobmod, only: nhdq,hdqlist

  ! The following variables are the coefficients that describe the
  ! linear regression fits that are used to define the dynamic
  ! observation error (DOE) specifications for all reconnissance
  ! observations collected within hurricanes/tropical cyclones; these
  ! apply only to the regional forecast models (e.g., HWRF); Henry
  ! R. Winterbottom (henry.winterbottom@noaa.gov).
  
  use obsmod, only: q_doe_a_136,q_doe_a_137,q_doe_b_136,q_doe_b_137

  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare local parameters
  real(r_kind),parameter:: small1=0.0001_r_kind
  real(r_kind),parameter:: small2=0.0002_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r8=8.0_r_kind
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r1e16=1.e16_r_kind
  real(r_kind),parameter:: r3p5 = 3.5_r_kind
  character(len=*),parameter:: myname='setupq'

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: genqsat
  external:: stop2

! Declare local variables  
  
  real(r_double) rstation_id
  real(r_kind) qob,qges,qsges,q2mges,q2mges_water,qsges_o
  real(r_kind) ratio_errors,dlat,dlon,dtime,dpres,rmaxerr,error
  real(r_kind) rsig,dprpx,rlow,rhgh,presq,tfact,ramp
  real(r_kind) psges,sfcchk,ddiff,errorx
  real(r_kind) cg_t,cvar,wgt,rat_err2,qcgross
  real(r_kind) grsmlt,ratio,val2,obserror
  real(r_kind) obserrlm,residual,ressw2,scale,ress,huge_error,var_jb
  real(r_kind) val,valqc,rwgt,prest
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(lat2,lon2,nsig,nfldsig):: qg
  real(r_kind),dimension(lat2,lon2,nfldsig):: qg2m
  real(r_kind),dimension(lat2,lon2,nfldsig):: qg2m_o
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(34):: ptablq
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbufp


  integer(i_kind) i,j,nchar,nreal,ii,l,jj,mm1,itemp,iip
  integer(i_kind) jsig,itype,k,nn,ikxx,iptrb,ibin,ioff,ioff0,icat,ijb
  integer(i_kind) ier,ilon,ilat,ipres,iqob,id,itime,ikx,iqmax,iqc
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,iobshgt,izz,iprvd,isprvd
  integer(i_kind) idomsfc,iderivative
  integer(i_kind) iqt
  integer(i_kind) ibb,ikk,idddd
  real(r_kind) :: delz
  type(sparr2) :: dhx_dx
  integer(i_kind) :: iz, q_ind, nind, nnz,iprev_station

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf,cdiagbufp
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8),allocatable,dimension(:):: cprvstgp,csprvstgp ! <-- provider info array for pseudo obs
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical ice,proceed
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID

  logical duplogic

  logical:: in_curbin, in_anybin, save_jacobian
  type(qNode),pointer:: my_head
  type(obs_diag),pointer:: jj_diag
  type(obs_diag),pointer:: my_diag
  type(obs_diag),pointer:: my_diag_pbl
  type(obs_diags),pointer:: my_diagLL

  real(r_kind) :: thispbl_height,ratio_PBL_height,prestsfc,diffsfc
  real(r_kind) :: hr_offset

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_q2m
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_t2m

  logical:: l_pbl_pseudo_itype
  integer(i_kind):: ich0
  type(obsLList),pointer,dimension(:):: qhead

  logical :: landsfctype

  real(r_kind) :: delta_z,  lapse_error, q_delta_terrain
  real(r_kind), parameter :: T_lapse = -0.0045 ! standard lapse rate, K/m
! use 4.5 K/km, in place of more standard 6.5 K/km, following
! https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019EA000984
! lapse_error_frac around 0.5 ~ 2K/km, from Figure 2 of above.
  real(r_kind), parameter :: lapse_error_frac = 0.5 ! inflation factor for obs error when vertically interpolating
  real(r_kind), parameter :: max_delta_z = 300. ! max. vertical mismatch allowed (later: relax this)

  qhead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  iqob=5      ! index of q observation
  id=6        ! index of station id
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of ob type
  iqmax=9     ! index of max error
  itemp=10    ! index of dry temperature
  iqc=11      ! index of quality mark
  ier2=12     ! index of original-original obs error ratio
  iuse=13     ! index of use parameter
  idomsfc=14  ! index of dominant surface type
  ilone=15    ! index of longitude (degrees)
  ilate=16    ! index of latitude (degrees)
  istnelv=17  ! index of station elevation (m)
  iobshgt=18  ! index of observation height (m)
  izz=19      ! index of surface height
  iprvd=20    ! index of observation provider
  isprvd=21   ! index of observation subprovider
  icat =22    ! index of data level category
  ijb  =23    ! index of non linear qc parameter
  iptrb=24    ! index of q perturbation
  if (l_rtma3d .or. twodvar_regional) then
    iqt  =25  ! index of flag indicating if virtual temp is associated to this moisture obs
  end if

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) <  8
  end do
!  If HD raobs available move prepbufr version to monitor
  if(nhdq > 0)then
     iprev_station=0
     do i=1,nobs
        ikx=nint(data(ikxx,i))
        itype=ictype(ikx)
        if(itype == 120) then
           rstation_id     = data(id,i)
           read(station_id,'(i5,3x)',err=1200) idddd
           if(idddd == iprev_station)then
             data(iuse,i)=108._r_kind
             muse(i) = .false.
           else
              stn_loop:do j=1,nhdq
                if(idddd == hdqlist(j))then
                   iprev_station=idddd
                   data(iuse,i)=108._r_kind
                   muse(i) = .false.
                   exit stn_loop
                end if
              end do stn_loop
           end if
        end if
1200    continue
     end do
  end if

  var_jb=zero

! choose only one observation--arbitrarily choose the one with positive time departure
!  handle multiple-reported data at a station

  hr_offset=min_offset/60.0_r_kind
  dup=one
  do k=1,nobs
     ikx=nint(data(ikxx,k))
     itype=ictype(ikx)
     landsfctype =( itype==181 .or. itype==183 .or. itype==187 )
     do l=k+1,nobs
        if (twodvar_regional .or. (hofx_2m_sfcfile .and. landsfctype) ) then
           duplogic=data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)
         else
           duplogic=data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k) == data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)
        end if

        if (duplogic) then
           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
              endif
!              write(*,'(a,2f10.5,2I8,2L10)') 'chech Q obs time==',&
!              data(itime,k)-hr_offset,data(itime,l)-hr_offset,k,l,&
!                           muse(k),muse(l)
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     iip=0
     nchar=1
     ioff0=21
     if (l_rtma3d .or. twodvar_regional) ioff0 = ioff0 + 2 ! 22:tdry; 23:tvflag (in binary obsdiag for 2D/3DRTMA)
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional .or. l_obsprvdiag) then
       nreal=nreal+2                            ! account for idomsfc,izz
       allocate(cprvstg(nobs),csprvstg(nobs))   ! obs provider info
       if(l_pbl_pseudo_surfobsq) allocate(cprvstgp(nobs*3),csprvstgp(nobs*3)) ! obs provider info for pseudo obs
     endif
     if (save_jacobian) then
       nnz   = 2                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 1
       call new(dhx_dx, nnz, nind)
       nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if(l_pbl_pseudo_surfobsq) allocate(cdiagbufp(nobs*3),rdiagbufp(nreal,nobs*3))
     if(netcdf_diag) call init_netcdf_diag_
  end if
  rsig=nsig

  mm1=mype+1
  grsmlt=five  ! multiplier factor for gross error check
  huge_error = huge_r_kind/r1e16
  scale=one

  ice=.false.   ! get larger (in rh) q obs error for mixed and ice phases

  iderivative=0

  ! calculate qsat and 2m qsat
  do jj=1,nfldsig
     call genqsat(qg(:,:,:,jj),ges_tsen(:,:,:,jj),ges_prsl(:,:,:,jj),lat2,lon2,nsig,ice,iderivative)
     if (i_use_2mq4b > 0) then  ! use lowest model level
       qg2m(:,:,jj)=qg(:,:,1,jj)
     elseif ( hofx_2m_sfcfile ) then  ! calculate from 2m model output
       call genqsat(qg2m(:,:,jj),ges_t2m(:,:,jj),ges_ps(:,:,jj),lat2,lon2,1,ice,iderivative)
     endif
  end do


! Prepare specific humidity data
  call dtime_setup()
  do i=1,nobs
     ikx=nint(data(ikxx,i))
     itype=ictype(ikx)
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     landsfctype =( itype==181 .or. itype==183 .or. itype==187 )

     ! Flag static conditions to create PBL_pseudo_surfobsq obs.
     l_pbl_pseudo_itype = l_pbl_pseudo_surfobsq .and. landsfctype

     if(in_curbin) then
!       Convert obs lats and lons to grid coordinates
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        dpres=data(ipres,i)

        rmaxerr=data(iqmax,i)
        rstation_id     = data(id,i)
        error=data(ier2,i)
        prest=r10*exp(dpres)     ! in mb
        var_jb=data(ijb,i)
     endif ! (in_curbin)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        my_diag => null()
        my_diag_pbl => null()

        ich0=qNode_ich0; if(l_pbl_pseudo_itype) ich0=qNode_ich0_pbl_pseudo
        do jj=1,ich0+1
          jj_diag => obsdiagLList_nextNode(my_diagLL    ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = jj             ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

          if(.not.associated(jj_diag)) then
            call perr(myname,'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
            call perr(myname,'                            ich =', jj)
            call die(myname)
          endif

          select case(jj)
          case(1); my_diag     => jj_diag
          case(2); my_diag_pbl => jj_diag
          end select
        end do
     endif

     if(.not.in_curbin) cycle

! Interpolate log(ps) & log(pres) at mid-layers to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     presq=r10*exp(dpres)
     itype=ictype(ikx)
     dprpx=zero

     if ( hofx_2m_sfcfile .and. landsfctype) then
        dpres = one ! put obs on surface
     else
        if(((itype > 179 .and. itype < 190) .or. itype == 199) &
           .and. .not.twodvar_regional)then
            dprpx=abs(one-exp(dpres-log(psges)))*r10
        endif

!    Put obs pressure in correct units to get grid coord. number
        call grdcrd1(dpres,prsltmp(1),nsig,-1)

!    Get approximate k value of surface by using surface pressure
        sfcchk=log(psges)
        call grdcrd1(sfcchk,prsltmp(1),nsig,-1)

!    Check to see if observations is above the top of the model (regional mode)
        if( dpres>=nsig+1)dprpx=1.e6_r_kind
        if((itype > 179 .and. itype < 186) .or. itype == 199) dpres=one

     endif

     qob = data(iqob,i) 
     if(limitqobs) then
        call tintrp31(ges_qsat,qsges,dlat,dlon,dpres,dtime,hrdifsig,&
          mype,nfldsig)
        qob=min(qob,superfact*qsges)
     end if

! get qsges, to be used to scale the obs error
     call tintrp31(qg,qsges,dlat,dlon,dpres,dtime,hrdifsig,&
          mype,nfldsig)

! overwrite qsges with 2-m qs if sfc obs scheme
     if( ( (i_use_2mq4b > 0) .and. ((itype > 179 .and. itype < 190) .or. itype == 199) &
            .and.  .not.twodvar_regional) .or. (hofx_2m_sfcfile .and. landsfctype)  )then
        call tintrp2a11(qg2m,qsges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)
     endif

!    Load obs error and value into local variables
     obserror = max(cermin(ikx)*r0_01,min(cermax(ikx)*r0_01,data(ier,i)))

     rmaxerr=rmaxerr*qsges
     rmaxerr=max(small2,rmaxerr)
     errorx =(data(ier,i)+dprpx)*qsges

! qges: Interpolate guess moisture to observation location and time 

     if (.not. ( hofx_2m_sfcfile .and. landsfctype) ) then 
        call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
           hrdifsig,mype,nfldsig)
     else
        ! only use land locations
        if (int(data(idomsfc,i)) .NE. 1  ) muse(i) = .false.

        call tintrp2a11(ges_q2m,qges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)

        ! terrain correction: assume RH_zo = RH_zm, and correct T with 
        ! same lapse rate as used for T2m terrain correction

        delta_z = data(istnelv,i) - data(izz,i) ! obs -model

        do jj=1,nfldsig
           ! qsat in model at height of obs
           call genqsat(qg2m_o(:,:,jj),ges_t2m(:,:,jj)+delta_z*T_lapse,ges_ps(:,:,jj),lat2,lon2,1,ice,iderivative)
        enddo

        call tintrp2a11(qg2m_o,qsges_o,dlat,dlon,dtime,hrdifsig,mype,nfldsig)
        q_delta_terrain = (qsges/qsges_o - 1)*qob
        qob = qob * ( qsges/qsges_o)

        !update the station elevation
        data(istnelv,i) = data(izz,i)

     endif

     ddiff=qob-qges 
  
!    Setup dynamic ob error specification for aircraft recon in hurricanes 

     if (aircraft_recon) then
       if (itype == 136 ) then

         errorx = q_doe_a_136*abs(ddiff)+q_doe_b_136
         
       endif

       if (itype == 137 ) then

         errorx = q_doe_a_137*abs(ddiff)+q_doe_b_137
         
       endif
     endif

     errorx =max(small1,errorx)

!    Adjust observation error to reflect the size of the residual.
!    If extrapolation occurred, then further adjust error according to
!    amount of extrapolation.

     if (.not. (hofx_2m_sfcfile  .and. landsfctype) ) then
         rlow=max(sfcchk-dpres,zero)
! linear variation of observation ramp [between grid points 1(~3mb) and 15(~45mb) below the surface]
         if(l_sfcobserror_ramp_q) then
            ramp=min(max(((rlow-1.0_r_kind)/(15.0_r_kind-1.0_r_kind)),0.0_r_kind),1.0_r_kind)*0.001_r_kind
         else
            ramp=rlow
         endif
     else
        rlow = zero
        ramp = zero
     endif

     rhgh=max(dpres-r0_001-rsig,zero)
     
     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if

!    inflate error for uncertainty in the terrain adjustment
     lapse_error = 0.
     if  ( hofx_2m_sfcfile  .and. landsfctype) then
        if (abs(delta_z)<max_delta_z) then  ! if height discrepency >max_delta_z do not assim.
                ! inflate obs error to account for error in lapse_rate
                ! also include some representativity error here (assuming
                ! delta_z ~ heterogeneity)
                lapse_error = abs(lapse_error_frac*q_delta_terrain)
        else
                muse(i)=.false.
        endif
     endif

     ratio_errors=error*qsges/(errorx+1.0e6_r_kind*rhgh+r8*ramp + lapse_error)

!    Check to see if observations is above the top of the model (regional mode)
     if (dpres > rsig) ratio_errors=zero
     error=one/(error*qsges)

     iz = max(1, min( int(dpres), nsig))
     delz = max(zero, min(dpres - real(iz,r_kind), one))


     if (save_jacobian) then
        q_ind =getindex(svars3d,'q')
        if (q_ind < 0) then
           print *, 'Error: no variable q in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1)  = iz + sum(levels(1:q_ind-1))
        dhx_dx%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:q_ind-1))

        dhx_dx%val(1) = one - delz         ! weight for iz's level
        dhx_dx%val(2) = delz               ! weight for iz+1's level
     endif

! i_use_2mq4b: Interpolate 2-m q to obs locations/times
     if(i_use_2mq4b>0 .and. itype > 179 .and. itype < 190 .and.  .not.twodvar_regional)then

        if(i_coastline==2 .or. i_coastline==3) then
!     Interpolate guess th 2m to observation location and time
           call tintrp2a11_csln(ges_q2m,q2mges,q2mges_water,dlat,dlon,dtime,hrdifsig,mype,nfldsig)   
           if(abs(qob-q2mges) > abs(qob-q2mges_water)) q2mges=q2mges_water
        else
           call tintrp2a11(ges_q2m,q2mges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)
        endif

        if(i_use_2mq4b==1)then
           qges=0.33_r_single*qges+0.67_r_single*q2mges
        elseif(i_use_2mq4b==2) then
           if(q2mges >= qges) then
              q2mges=min(q2mges, 1.15_r_single*qges)
           else
              q2mges=max(q2mges, 0.85_r_single*qges)
           end if
           qges=q2mges
        else
           write(6,*) 'Invalid i_use_2mq4b number=',i_use_2mq4b
           call stop2(100)
        endif
        ddiff=qob-qges
     endif ! i_use_2mq4b


!    If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov*1.e-3_r_kind
        error=one/(magoberr*1.e-3_r_kind)
        ratio_errors=one
     end if

!    Gross error checks

     if(abs(ddiff) > grsmlt*data(iqmax,i)) then
        error=zero
        ratio_errors=zero


        if(luse(i))awork(5)=awork(5)+one
     end if
     obserror=min(one/max(ratio_errors*error,tiny_r_kind),huge_error)
     obserror=obserror*r100/qsges
     obserrlm=max(cermin(ikx),min(cermax(ikx),obserror))
     residual=abs(ddiff*r100/qsges)
     ratio=residual/obserrlm

! modify gross check limit for quality mark=3
     if(data(iqc,i) == three ) then
        qcgross=r0_7*cgross(ikx)
     else
        qcgross=cgross(ikx)
     endif

     if (twodvar_regional) then
        if ( (data(iuse,i)-real(int(data(iuse,i)),kind=r_kind)) == 0.25_r_kind) &
               qcgross=r3p5*qcgross
     endif

     if(ratio > qcgross .or. ratio_errors < tiny_r_kind) then
        if(luse(i))awork(4)=awork(4)+one
        error=zero
        ratio_errors=zero

     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0 .and. luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

!   Oberror Tuning and Perturb Obs
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
     if(nvqc .and. ibeta(ikx) >0  ) ratio_errors=0.8_r_kind*ratio_errors
     if(luse(i))then
        val2     = val*val
        if(vqc) then
           cg_t=cvar_b(ikx)
           cvar=cvar_pg(ikx)
        else
           cg_t=zero
           cvar=zero
        endif
        if(nvqc) then
           ibb=ibeta(ikx)
           ikk=ikapa(ikx)
        else
           ibb=0
           ikk=0
        endif
 
        call vqc_setup(val,ratio_errors,error,cvar,cg_t,ibb,ikk,&
                      var_jb,rat_err2,wgt,valqc)
        rwgt = wgt/wgtlim 
!       Accumulate statistics for obs belonging to this task
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+val2*rat_err2
           awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+one
           awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
        end if
! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
        ress  = scale*r100*ddiff/qsges
        ressw2= ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presq > ptopq(k) .and. presq <= pbotq(k))then
 
              bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one             ! count
              bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress            ! (o-g)
              bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ressw2          ! (o-g)**2
              bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2*rat_err2   ! penalty
              bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc           ! nonlin qc penalty
           end if
        end do
     end if

     if (luse_obsdiag) then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        call qNode_appendto(my_head,qhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%ich0= qNode_ich0        ! a marker of ordinary obs.
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        my_head%dlev= dpres
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)
        
        my_head%res    = ddiff
        my_head%err2   = error**2
        my_head%raterr2= ratio_errors**2   
        my_head%time   = dtime
        my_head%b      = cvar_b(ikx)
        my_head%pg     = cvar_pg(ikx)
        my_head%jb     = var_jb
        my_head%ib     = ibeta(ikx)
        my_head%ik     = ikapa(ikx)
        my_head%luse   = luse(i)

        if(oberror_tune) then
           my_head%qpertb=data(iptrb,i)/error/ratio_errors
           my_head%kx=ikx
           if (njqc) then
              ptablq=ptabl_q
           else
              ptablq=ptabl
           endif
           if(presq > ptablq(2))then
              my_head%k1=1
           else if( presq <= ptablq(33)) then
              my_head%k1=33
           else
              k_loop: do k=2,32
                 if(presq > ptablq(k+1) .and. presq <= ptablq(k)) then
                    my_head%k1=k
                    exit k_loop
                 endif
              enddo k_loop
           endif
        endif

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,my_head%ich0+1, myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif
        
        my_head => null()
     endif

! Save select output for diagnostic file
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        err_input = data(ier2,i)*qsges            ! convert rh to q
        err_adjst = data(ier,i)*qsges             ! convert rh to q
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)
        
     end if

!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!
     if( .not. last .and. l_pbl_pseudo_itype .and.         &
           muse(i) .and. dpres > -1.0_r_kind ) then
        prestsfc=prest
        diffsfc=ddiff
        call tintrp2a11(pbl_height,thispbl_height,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
        ratio_PBL_height = (prest - thispbl_height) * pblh_ration
        if(ratio_PBL_height > zero) thispbl_height = prest - ratio_PBL_height
        prest = prest - pps_press_incr
        DO while (prest > thisPBL_height)
           ratio_PBL_height=1.0_r_kind-(prestsfc-prest)/(prestsfc-thisPBL_height)

           allocate(my_head)
           call qNode_appendto(my_head,qhead(ibin))
           my_head%idv = is
           my_head%iob = ioid(i)
           my_head%ich0= qNode_ich0_PBL_pseudo  ! a marker of GSI created PBL_pseudo obs.
           my_head%elat= data(ilate,i)
           my_head%elon= data(ilone,i)

!!! find qob 
           qob = data(iqob,i)

!    Put obs pressure in correct units to get grid coord. number
           dpres=log(prest/r10)
           call grdcrd1(dpres,prsltmp(1),nsig,-1)


! Interpolate guess moisture to observation location and time
           call tintrp31(ges_q,qges,dlat,dlon,dpres,dtime, &
                             hrdifsig,mype,nfldsig)
           call tintrp31(qg,qsges,dlat,dlon,dpres,dtime,hrdifsig,&
                       mype,nfldsig)

!!! Set (i,j,k) indices of guess gridpoint that bound obs location
           my_head%dlev= dpres
           call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)
!!! find ddiff       

! Compute innovations
           ddiff=diffsfc*(0.3_r_kind + 0.7_r_kind*ratio_PBL_height)

           error=one/(data(ier2,i)*qsges)

           my_head%res     = ddiff
           my_head%err2    = error**2
           my_head%raterr2 = ratio_errors**2
           my_head%time    = dtime
           my_head%b       = cvar_b(ikx)
           my_head%pg      = cvar_pg(ikx)
           my_head%jb      = var_jb
           my_head%ib      = ibeta(ikx)
           my_head%ik      = ikapa(ikx)
           my_head%luse    = luse(i)

           if (luse_obsdiag) then
             call obsdiagNode_assert(my_diag_pbl, &
                my_head%idv,my_head%iob,my_head%ich0+1,myname,'my_diag_pbl:my_head')

             call obsdiagNode_set(my_diag_pbl, wgtjo=(error*ratio_errors)**2, &
                jiter=jiter, muse=.true., nldepart=my_head%res)

             my_head%diags => my_diag_pbl
           endif

! Save select output for diagnostic file
           if(conv_diagsave .and. luse(i))then
              iip=iip+1
              if(iip <= 3*nobs ) then
                 rstation_id     = data(id,i)

                 err_input = data(ier2,i)*qsges            ! convert rh to q
                 err_adjst = data(ier,i)*qsges             ! convert rh to q
                 if (ratio_errors*error>tiny_r_kind) then
                    err_final = one/(ratio_errors*error)
                 else
                    err_final = huge_single
                 endif

                 errinv_input = huge_single
                 errinv_adjst = huge_single
                 errinv_final = huge_single
                 if (err_input>tiny_r_kind) errinv_input = one/err_input
                 if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
                 if (err_final>tiny_r_kind) errinv_final = one/err_final

                 if(binary_diag) call contents_binary_diagp_(my_diag_pbl)
              else
                 iip=3*nobs
              endif
              if(netcdf_diag) call contents_netcdf_diagp_(my_diag_pbl)
           endif    !conv_diagsave .and. luse(i))

           prest = prest - pps_press_incr

           my_head => null()
        ENDDO

     endif  ! l_pbl_pseudo_itype
!!!!!!!!!!!!!!!!!!  PBL pseudo surface obs  !!!!!!!!!!!!!!!!!!!!!!!

! End of loop over observations
  end do
  
! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave) then
    if(netcdf_diag) call nc_diag_write
    if(binary_diag .and. ii>0)then
       write(7)'  q',nchar,nreal,ii+iip,mype,ioff0,iip
       if(l_pbl_pseudo_surfobsq .and. iip>0) then
          write(7)cdiagbuf(1:ii),cdiagbufp(1:iip),rdiagbuf(:,1:ii),rdiagbufp(:,1:iip)
       else
          write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       endif

       if (twodvar_regional .or. l_obsprvdiag) then
          if(l_pbl_pseudo_surfobsq .and. iip>0) then
             write(7)cprvstg(1:ii),cprvstgp(1:iip),csprvstg(1:ii),csprvstgp(1:iip)
          else
             write(7)cprvstg(1:ii),csprvstg(1:ii)
          endif
          deallocate(cprvstg,csprvstg)
          if(l_pbl_pseudo_surfobsq) deallocate(cprvstgp,csprvstgp)
       endif
    endif
    deallocate(cdiagbuf,rdiagbuf)
    if(l_pbl_pseudo_surfobsq) deallocate(cdiagbufp,rdiagbufp)
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
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
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
         if(allocated(ges_ps))then
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
!    get q2m ...
     if (i_use_2mq4b>0 .or. hofx_2m_sfcfile) then
        varname='q2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_q2m))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_q2m(size(rank2,1),size(rank2,2),nfldsig))
            ges_q2m(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_q2m(:,:,ifld)=rank2
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            call stop2(999)
        endif
     endif ! i_use_2mq4b
     if (hofx_2m_sfcfile) then
        varname='t2m'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
        if (istatus==0) then
            if(allocated(ges_t2m))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_t2m(size(rank2,1),size(rank2,2),nfldsig))
            ges_t2m(:,:,1)=rank2
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
               ges_t2m(:,:,ifld)=rank2
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
            call stop2(999)
        endif
     endif ! hofx_2m_sfcfile
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('conv_q_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        if (save_jacobian) then
          call nc_diag_header("jac_nnz", nnz)
          call nc_diag_header("jac_nind", nind)
        endif
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
    type(obs_diag),pointer,intent(in):: odiag

        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = presq              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = var_jb             ! non linear qc b parameter
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one                    
        endif

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse observation error
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error

        rdiagbuf(17,ii) = data(iqob,i)       ! observation
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis
        rdiagbuf(19,ii) = qob-qges           ! obs-ges w/o bias correction (future slot)

        rdiagbuf(20,ii) = qsges              ! guess saturation specific humidity
        rdiagbuf(21,ii) = 1e+10_r_single     ! spread (filled in by EnKF)

        if (l_rtma3d .or. twodvar_regional) then   ! in binary obsdiag for 2D/3DRTMA
          rdiagbuf(22,ii) = data(itemp,i)    ! dry temperature associated to qob
          rdiagbuf(23,ii) = data(iqt,  i)    ! tv flag (0: virtual temp; 1: sensible temp)
        end if

        ioff=ioff0
        if (lobsdiagsave) then
           do jj=1,miter 
              ioff=ioff+1 
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
        endif

        if (twodvar_regional .or. l_obsprvdiag) then
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(idomsfc,i) ! dominate surface type
           ioff = ioff + 1
           rdiagbuf(ioff,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_

  subroutine contents_binary_diagp_(odiag)
    type(obs_diag),pointer,intent(in):: odiag

        cdiagbufp(iip)    = station_id         ! station id

        rdiagbufp(1,iip)  = ictype(ikx)        ! observation type
        rdiagbufp(2,iip)  = -1                 ! observation subtype (-1 for pseudo obs sub-type)
            
        rdiagbufp(3,iip)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbufp(4,iip)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbufp(5,iip)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbufp(6,iip)  = prest !presq              ! observation pressure (hPa)
        rdiagbufp(7,iip)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbufp(8,iip)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbufp(9,iip)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbufp(10,iip) = var_jb             ! non linear qc b parameter
        rdiagbufp(11,iip) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbufp(12,iip) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbufp(12,iip) = -one                    
        endif

        rdiagbufp(13,iip) = rwgt               ! nonlinear qc relative weight
        rdiagbufp(14,iip) = errinv_input       ! prepbufr inverse observation error
        rdiagbufp(15,iip) = errinv_adjst       ! read_prepbufr inverse obs error
        rdiagbufp(16,iip) = errinv_final       ! final inverse observation error

        rdiagbufp(17,iip) = data(iqob,i)       ! observation
        rdiagbufp(18,iip) = ddiff              ! obs-ges used in analysis
        rdiagbufp(19,iip) = ddiff              !qob-qges           ! obs-ges w/o bias correction (future slot)

        rdiagbufp(20,iip) = qsges              ! guess saturation specific humidity
        rdiagbufp(21,iip) = 1e+10_r_single     ! spread (filled in by EnKF)

        if (l_rtma3d .or. twodvar_regional) then  ! in binary obsdiag for 2D/3DRTMA
          rdiagbufp(22,ii) = data(itemp,i)    ! dry temperature associated to qob
          rdiagbufp(23,ii) = data(iqt,  i)    ! tv flag (0: virtual temp; 1: sensible temp)
        end if

        ioff=ioff0
!----
        if (lobsdiagsave) then
           do jj=1,miter 
              ioff=ioff+1 
              if (odiag%muse(jj)) then
                 rdiagbufp(ioff,iip) = one
              else
                 rdiagbufp(ioff,iip) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbufp(ioff,iip) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbufp(ioff,iip) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbufp(ioff,iip) = odiag%obssen(jj)
           enddo
        endif

        if (twodvar_regional .or. l_obsprvdiag) then
           ioff = ioff + 1
           rdiagbufp(ioff,iip) = -9999._r_single !  data(idomsfc,i) ! dominate surface type
           ioff = ioff + 1
           rdiagbufp(ioff,iip) = -9999._r_single !  data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstgp(iip)         = '88888888'    !c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstgp(iip)        = '88888888'    !c_sprvstg       ! subprovider name
        endif
!----
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbufp(ioff+1:nreal,iip))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diagp_

  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '      q'
  real(r_kind),dimension(miter) :: obsdiag_iuse

           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata_to_single("Latitude",      data(ilate,i)          )
           call nc_diag_metadata_to_single("Longitude",     data(ilone,i)          )
! this is the obs height after being interpolated to the model (=model height)
           call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)     )
           call nc_diag_metadata_to_single("Pressure",      presq                  )
! this is the original obs height (= stn elevation,  before being interpolated)
           call nc_diag_metadata_to_single("Height",        data(iobshgt,i)        )
           call nc_diag_metadata_to_single("Time",          dtime,time_offset,'-'  )
           call nc_diag_metadata_to_single("Prep_QC_Mark",  data(iqc,i)            )
           call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)            )
           call nc_diag_metadata_to_single("Nonlinear_QC_Var_Jb",var_jb            )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt             )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -1.0_r_single          )
           endif
           call nc_diag_metadata_to_single("Errinv_Input",  errinv_input           )
           call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst            )
           call nc_diag_metadata_to_single("Errinv_Final",  errinv_final           )

           call nc_diag_metadata_to_single("Observation",  data(iqob,i)            )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff     )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",qob,qges,'-')
           call nc_diag_metadata_to_single("Forecast_Saturation_Spec_Hum",qsges    )
           if (l_rtma3d .or. twodvar_regional) then
              call nc_diag_metadata_to_single("Observation_Tdry", data(itemp,i)    )
              call nc_diag_metadata_to_single("Setup_QC_Mark",    data(iqt,  i)    )
           endif
           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )
           endif

           if (twodvar_regional .or. l_obsprvdiag) then
              call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i)              )
              call nc_diag_metadata("Model_Terrain",     data(izz,i)                  )
              r_prvstg            = data(iprvd,i)
              call nc_diag_metadata("Provider_Name",     c_prvstg                     )
              r_sprvstg           = data(isprvd,i)
              call nc_diag_metadata("Subprovider_Name",  c_sprvstg                    )
           endif
           if (save_jacobian) then
             call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
           endif

  end subroutine contents_netcdf_diag_

  subroutine contents_netcdf_diagp_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '      q'
  real(r_kind),dimension(miter) :: obsdiag_iuse

           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     -1                     )  ! (-1 for pseudo obs sub-type)
           call nc_diag_metadata_to_single("Latitude",      data(ilate,i)          )
           call nc_diag_metadata_to_single("Longitude",     data(ilone,i)          )
           call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)     )
           call nc_diag_metadata_to_single("Pressure",      presq                  )
           call nc_diag_metadata_to_single("Height",        data(iobshgt,i)        )
           call nc_diag_metadata_to_single("Time",          dtime,time_offset,'-'  )
           call nc_diag_metadata_to_single("Prep_QC_Mark",  data(iqc,i)            )
           call nc_diag_metadata_to_single("Prep_Use_Flag", data(iuse,i)           )
           call nc_diag_metadata_to_single("Nonlinear_QC_Var_Jb",var_jb            )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt             )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -1.0_r_single          )
           endif
           call nc_diag_metadata_to_single("Errinv_Input",  errinv_input           )
           call nc_diag_metadata_to_single("Errinv_Adjust", errinv_adjst           )
           call nc_diag_metadata_to_single("Errinv_Final",  errinv_final           )

           call nc_diag_metadata_to_single("Observation",   data(iqob,i)           )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff     )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",ddiff   )
           call nc_diag_metadata_to_single("Forecast_Saturation_Spec_Hum",qsges    )
           if (l_rtma3d .or. twodvar_regional) then
              call nc_diag_metadata_to_single("Observation_Tdry", data(itemp,i)    )
              call nc_diag_metadata_to_single("Setup_QC_Mark",    data(iqt,  i)    )
           endif
!----
           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )
           endif

           if (twodvar_regional .or. l_obsprvdiag) then
              call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i)              )
              call nc_diag_metadata("Model_Terrain",     data(izz,i)                  )
              r_prvstg            = data(iprvd,i)
              call nc_diag_metadata("Provider_Name",     "88888888"                   )
              r_sprvstg           = data(isprvd,i)
              call nc_diag_metadata("Subprovider_Name",  "88888888"                   )
           endif
!----
           if (save_jacobian) then
             call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
             call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
           endif

  end subroutine contents_netcdf_diagp_

  subroutine final_vars_
    if(allocated(ges_q2m)) deallocate(ges_q2m)
    if(allocated(ges_t2m)) deallocate(ges_t2m)
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupq
end module q_setup
