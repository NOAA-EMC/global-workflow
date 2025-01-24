module spd_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupspd; end interface

contains
subroutine setupspd(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupspd    compute rhs of oi for wind speed obs
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  For wind speed observations, this routine
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
!   2004-10-06  parrish - increase size of vwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-08  treadon - correct vertical dimension (nsig) in call tintrp2a(ges_tv...)
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - add option to perturb observation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-09      su - modify the observation perturbation
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the observation gross check error 
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name, and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2012-01-12  hu      - add code to get vertical grid coordinate ibased on height for
!                         260 (nacelle) and 261 (tower) 
!   2013-01-26  parrish - convert grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a11,
!                                    tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-11-29  shlyaeva - save linearized H(x) for EnKF
!   2017-02-06  todling - add netcdf_diag capability; hidden as contained code
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2020-01-27  Winterbottom - moved the linear regression derived
!                              coefficients for the dynamic observation
!                              error (DOE) calculation to the namelist
!                              level; they are now loaded by
!                              aircraftinfo.  
!   2021-10-xx  pondeca/morris/zhao - added observation provider/subprovider
!                         information in diagonostic file, which is used
!                         in offline observation quality control program (AutoObsQC) 
!                         for 3D-RTMA (if l_obsprvdiag is true).
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

  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,&
                    lobsdiag_forenkf,aircraft_recon
  use obsmod, only: netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: l_obsprvdiag
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use m_obsNode, only: obsNode
  use m_spdNode, only: spdNode
  use m_spdNode, only: spdNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use guess_grids, only: nfldsig,hrdifsig,ges_lnprsl, &
           comp_fact10,sfcmod_gfs,sfcmod_mm5
  use guess_grids, only: geop_hgtl
  use gridmod, only: nsig,get_ij,twodvar_regional
  use qcmod, only: npres_print,ptop,pbot
  use constants, only: one,grav,rd,zero,four,tiny_r_kind, &
       half,two,cg_term,huge_single,r1000,wgtlim
  use jfunc, only: jiter,last,miter,jiterstart
  use state_vectors, only: svars3d, levels
  use qcmod, only: dfact,dfact1
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use m_dtime, only: dtime_setup, dtime_check
  use sparsearr, only: sparr2, new, size, writearray, fullarray

  ! The following variables are the coefficients that describe the
  ! linear regression fits that are used to define the dynamic
  ! observation error (DOE) specifications for all reconnissance
  ! observations collected within hurricanes/tropical cyclones; these
  ! apply only to the regional forecast models (e.g., HWRF); Henry
  ! R. Winterbottom (henry.winterbottom@noaa.gov).
  
  use obsmod, only: uv_doe_a_213,uv_doe_b_213
  
  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare local variables
  real(r_kind),parameter:: ten=10.0_r_kind
  character(len=*),parameter:: myname='setupspd'

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables
  
  real(r_double) rstation_id
  
  real(r_kind) uob,vob,spdges,spdob,spdob0,goverrd,ratio_errors
  real(r_kind) presw,factw,dpres,ugesin,vgesin,sfcr,skint
  real(r_kind) scale
  real(r_kind) val2,ressw,ress,error,ddiff,dx10,rhgh,prsfc,r0_001
  real(r_kind) sfcchk,prsln2,rwgt,tfact                        
  real(r_kind) thirty,rsig,ratio,residual,obserrlm,obserror
  real(r_kind) val,valqc,psges,drpx,dlat,dlon,dtime,dpresave,rlow
  real(r_kind) cg_spd,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) mm1,ibin,ioff,ioff0
  integer(i_kind) ii,jj,i,nchar,nreal,k,j,l,nty,nn,ikxx
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx
  integer(i_kind) ihgt,iqc,ier2,iuse,ilate,ilone,istnelv,izz,iprvd,isprvd
  integer(i_kind) idomsfc,iskint,iff10,isfcr,isli

  type(sparr2) :: dhx_dx
  integer(i_kind) :: iz, u_ind, v_ind, nnz, nind
  real(r_kind) :: delz
  
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical:: in_curbin, in_anybin, save_jacobian
  type(spdNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  logical z_height
  real(r_kind) zsges,dstn
  real(r_kind),dimension(nsig):: zges
  real(r_kind) dz,zob,z1,z2,p1,p2,dz21,dlnp21,pobl
  integer(i_kind) k1,k2

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  type(obsLList),pointer,dimension(:):: spdhead
  spdhead => obsLL(:)

!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

!        index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  iuob=5      ! index of u observation
  ivob=6      ! index of v observation
  id=7        ! index of station id
  itime=8     ! index of observation time in data array
  ikxx=9      ! index of ob type
  ihgt=10     ! index of observation elevation
  iqc=11      ! index of quality mark
  ier2=12     ! index of original-original obs error ratio
  iuse=13     ! index of use parameter
  idomsfc=14  ! index of dominant surface type
  iskint=15   ! index of surface skin temperature
  iff10=16    ! index of 10 meter wind factor
  isfcr=17    ! index of surface roughness
  ilone=18    ! index of longitude (degrees)
  ilate=19    ! index of latitude (degrees)
  istnelv=20  ! index of station elevation (m)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of  observation subprovider

  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  r0_001=0.001_r_kind
  goverrd=grav/rd
  
  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=21
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (twodvar_regional .or. l_obsprvdiag) then
       nreal=nreal+2                          ! account for idomsfc,izz
       allocate(cprvstg(nobs),csprvstg(nobs)) ! obs provider info
     endif
     if (save_jacobian) then
       nnz    = 4                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 2
       call new(dhx_dx, nnz, nind)
       nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if


  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k)== data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(l) .and. muse(k))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
 
        dpres=data(ipres,i)
        error=data(ier2,i)
        ikx=nint(data(ikxx,i))
     endif

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
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

        if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
     endif

     if(.not.in_curbin) cycle

!    Load obs error and u,v obs
     obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
     uob = data(iuob,i)
     vob = data(ivob,i)
 
 
     spdob=sqrt(uob*uob+vob*vob)
     call tintrp2a1(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     factw = data(iff10,i)
     if(sfcmod_gfs .or. sfcmod_mm5)then
        sfcr = data(isfcr,i)
        skint = data(iskint,i)
        isli=data(idomsfc,i)
        call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
     end if

     nty=ictype(ikx)

     z_height = .false.
!    if ( nty == 260 .or. nty == 261) z_height = .true.
!    nty == 213 is temporarily assigned to SFMR retrieved wind speed from recon
!    and is subjet to change in the future
     if ( nty == 260 .or. nty == 261 .or. nty == 213) z_height = .true.

!    Process observations reported with height differently than those
!    reported with pressure.  Type 260=nacelle 261=tower wind spd are
!    encoded in NCEP prepbufr files with geometric height above
!    sea level. 
     
     if (z_height) then
        
        drpx = zero
        dpres = data(ihgt,i)
        dstn = data(istnelv,i)
        call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)

!       Get guess surface elevation and geopotential height profile
!       at observation location.
        call tintrp2a1(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)

!       Convert observation height (in dpres) from meters to grid relative
!       units.  Save the observation height in zob for later use.
        zob = dpres
        call grdcrd1(dpres,zges,nsig,1)
        factw=one
        rlow=zero
        rhgh=zero

!       Compute observation pressure (only used for diagnostics)
!       Set indices of model levels below (k1) and above (k2) observation.
        if (dpres<one) then
           z1=zero;    p1=log(psges)
           z2=zges(1); p2=prsltmp(1)
        elseif (dpres>nsig) then
           z1=zges(nsig-1); p1=prsltmp(nsig-1)
           z2=zges(nsig);   p2=prsltmp(nsig)
           drpx = 1.e6_r_kind
        else
           k=dpres
           k1=min(max(1,k),nsig)
           k2=max(1,min(k+1,nsig))
           z1=zges(k1); p1=prsltmp(k1)
           z2=zges(k2); p2=prsltmp(k2)
        endif

        dz21     = z2-z1
        dlnp21   = p2-p1
        dz       = zob-z1
        pobl     = p1 + (dlnp21/dz21)*dz
        presw    = ten*exp(pobl)

!    Process observations with reported pressure
     else

        presw = ten*exp(dpres)
        dpres = dpres-log(psges)
        drpx=zero
        if(nty >= 280 .and. nty < 290)then
           dpresave=dpres
           dpres=-goverrd*data(ihgt,i)/tges(1)
           if(nty < 283)drpx=abs(dpres-dpresave)*factw*thirty
        end if

        prsfc=psges
        prsln2=log(exp(prsltmp(1))/prsfc)
        sfcchk=log(psges)
        if(dpres <= prsln2)then
           factw=one
        else
           dx10=-goverrd*ten/tges(1)
           if (dpres < dx10)then
              factw=(dpres-dx10+factw*(prsln2-dpres))/(prsln2-dx10)
           end if
        end if

!    Put obs pressure in correct units to get grid coord. number
        dpres=log(exp(dpres)*prsfc)
        call grdcrd1(dpres,prsltmp(1),nsig,-1)

!    Get approx k value of sfc by using surface pressure of 1st ob
        call grdcrd1(sfcchk,prsltmp(1),nsig,-1)


!    Check to see if observations is below what is seen to be the surface
        rlow=max(sfcchk-dpres,zero)

        rhgh=max(dpres-r0_001-rsig,zero)

     endif  ! end of process observations with reported pressure

     if(luse(i))then
        awork(1) = awork(1) + one
        if(rlow/=zero) awork(2) = awork(2) + one
        if(rhgh/=zero) awork(3) = awork(3) + one
     end if

     ratio_errors=error/(data(ier,i)+drpx+1.0e6_r_kind*rhgh+four*rlow)
     

! Interpolate guess u and v to observation location and time.
     call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime, &
        hrdifsig,mype,nfldsig)
     call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime, &
        hrdifsig,mype,nfldsig)
 

! Apply 10-meter wind reduction factor to guess winds.  Compute
! guess wind speed.
     ugesin=factw*ugesin
     vgesin=factw*vgesin
     spdges=sqrt(ugesin*ugesin+vgesin*vgesin)

     iz = max(1, min( int(dpres), nsig))
     delz = max(zero, min(dpres - real(iz,r_kind), one))

     if (save_jacobian) then

        u_ind = getindex(svars3d, 'u')
        if (u_ind < 0) then
           print *, 'Error: no variable u in state vector. Exiting.'
           call stop2(1300)
        endif
        v_ind = getindex(svars3d, 'v')
        if (v_ind < 0) then
           print *, 'Error: no variable v in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1)  = iz               + sum(levels(1:u_ind-1))
        dhx_dx%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:u_ind-1))

        dhx_dx%val(1) = (one - delz) * two * ugesin
        dhx_dx%val(2) = delz * two * ugesin

        dhx_dx%st_ind(2)  = iz               + sum(levels(1:v_ind-1))
        dhx_dx%end_ind(2) = min(iz + 1,nsig) + sum(levels(1:v_ind-1))

        dhx_dx%val(3) = (one - delz) * two * vgesin
        dhx_dx%val(4) = delz * two * ugesin
     endif


     ddiff = spdob-spdges
     
     if (aircraft_recon) then
       if ( nty == 213 ) then 
         ratio_errors=error/(uv_doe_a_213*abs(ddiff)+uv_doe_b_213)
         if (spdob < 10._r_kind) ratio_errors=zero
       endif 
     endif
   
     error=one/error

!    Check to see if observations is above the top of the model (regional mode)
     if (dpres>rsig) ratio_errors=zero


!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio>cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
        muse(i)=.false.
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     val2     = val*val
     exp_arg  = -half*val2
     rat_err2 = ratio_errors**2
     if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
        arg  = exp(exp_arg)
        wnotgross= one-cvar_pg(ikx)
        cg_spd=cvar_b(ikx)
        wgross = cg_term*cvar_pg(ikx)/(cg_spd*wnotgross)
        term = log((arg+wgross)/(one+wgross))
        wgt  = one-wgross/(arg+wgross)
        wgt  = wgt/wgtlim
     else
        term = exp_arg
        wgt  = wgtlim
        rwgt = wgt/wgtlim
     endif
     valqc = -two*rat_err2*term


!       Accumulate statistics for obs belonging to this task
     if (luse(i) .and. muse(i)) then
        if(rwgt < one) awork(61) = awork(61)+one
        awork(5)=awork(5) + val2*rat_err2
        awork(6)=awork(6) + one
        awork(22)=awork(22) + valqc
     end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
     do k = 1,npres_print
        if(luse(i) .and.presw >ptop(k) .and. presw<=pbot(k))then
           ress  = scale*ddiff
           ressw = ress*ress
           nn=1
           if (.not. muse(i)) then
              nn=2
              if(ratio_errors*error >=tiny_r_kind)nn=3
           end if
           bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
           bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
           bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
           bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
           bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty

        end if
     end do

     if (luse_obsdiag) then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=spdob-sqrt(ugesin*ugesin+vgesin*vgesin))
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        call spdNode_appendto(my_head,spdhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij,my_head%wij)

        my_head%factw=factw
        do j=1,4
           my_head%wij(j)=factw*my_head%wij(j)
        end do
        my_head%raterr2= ratio_errors**2     
        my_head%res    = spdob
        my_head%uges   = ugesin
        my_head%vges   = vgesin
        my_head%err2   = error**2
        my_head%time   = dtime
        my_head%luse   = luse(i)
        my_head%b      = cvar_b(ikx)
        my_head%pg     = cvar_pg(ikx)

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     end if
! Save select output for diagnostic file
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        spdob0      = sqrt(data(iuob,i)*data(iuob,i)+data(ivob,i)*data(ivob,i))
        rstation_id = data(id,i)
        err_input   = data(ier2,i)
        err_adjst   = data(ier,i)
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

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave) then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        write(7)'spd',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)

        if (twodvar_regional .or. l_obsprvdiag) then
           write(7)cprvstg(1:ii),csprvstg(1:ii)
           deallocate(cprvstg,csprvstg)
        endif
     end if
     deallocate(cdiagbuf,rdiagbuf)
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
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
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
900  format('conv_spd_',i2.2,'.nc4')
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
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1

        rdiagbuf(17,ii) = spdob              ! wind speed observation (m/s)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = spdob0-spdges      ! obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(20,ii) = factw              ! 10m wind reduction factor

        rdiagbuf(21,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in by EnKF)

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
           r_prvstg          = data(iprvd,i)
           cprvstg(ii)       = c_prvstg        ! provider name
           r_sprvstg         = data(isprvd,i)
           csprvstg(ii)      = c_sprvstg       ! subprovider name
        endif

        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '    spd'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata_to_single("Latitude",data(ilate,i)      )
           call nc_diag_metadata_to_single("Longitude",data(ilone,i)      )
           call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)  )
           call nc_diag_metadata_to_single("Pressure",presw              )
           call nc_diag_metadata_to_single("Height",data(ihgt,i)       )
           call nc_diag_metadata_to_single("Time",dtime,time_offset,'-')
           call nc_diag_metadata_to_single("Prep_QC_Mark",data(iqc,i)        )
           call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)       )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb                 )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt               )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",   -1.0_r_single           )
           endif

           call nc_diag_metadata_to_single("Errinv_Input",errinv_input       )
           call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst       )
           call nc_diag_metadata_to_single("Errinv_Final",errinv_final       )

           call nc_diag_metadata_to_single("Observation",spdob        )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff        )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted", spdob0,spdges,'-')
 
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

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupspd
end module spd_setup
