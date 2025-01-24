module rw_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setuprw; end interface

contains
subroutine setuprw(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprw     compute rhs of oi for radar radial winds
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: For radar radial wind observations, this routine
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
!   2004-10-06  parrish - increase size of rwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-03  treadon - correct error in index values for data array
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-21  parrish - new forward model based on beam vertical uncertainty
!   2006-05-23  parrish - use model terrain at station location for zsges
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-17  tong - modifed to use airborne radar data
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-03-28  s.liu     - add subtype to radial wind
!   2011-05-25  s.liu/parrish     - correct error in height assigned to radial wind
!   2012-02-08  wu      - bug fix to keep from using below ground radar obs, with extra printout
!                           added to identify which obs are below ground.  
!   2013-01-22  parrish - change grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a11,
!                             tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-01-22  parrish - WCOSS debug compile execution error rwgt not assigned a value.
!                             set rwgt = 1 at beginning of obs loop.
!   2013-02-15  parrish - WCOSS debug compile execution error, k1=k2 but data(iobs_type,i) <=3, causes 0./0.
!   2013-06-07  tong    - add a factor to adjust tdr obs gross error and add an option to adjust
!                         tdr obs error
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-06-23  lippi  - Add vertical velocity to observation operator. Now,
!                        costilt is multiplied here instead of factored into wij.
!                        nml option include_w is used. Add a conditional to use 
!                        maginnov and magoberr parameters from single ob namelist.   
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!
!   2016-02-15  Johnson, Y. Wang, X. Wang - Develop the radial velocity
!                                           operator by including vetical velocity and
!                                           considering the terminal velocity of
!                                           target hydrometeors (Johnson et al.
!                                           2015 MWR; Wang and Wang 2016 MWR)
!                                           POC: xuguang.wang@ou.edu
!   2017-09-xx  CAPS(G. Zhao) - added options enabing CAPS rw DA capabilities
!   2019-07-11  todling - introduced wrf_vars_mod (though truly not needed)
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
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind

  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,lobsdiag_forenkf,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,&
                    if_vterminal, ens_hx_dbz_cut, if_model_dbz, &
                    doradaroneob,oneobddiff,oneobvalue, if_vrobs_raw, if_use_w_vr
  use obsmod, only: netcdf_diag, binary_diag, dirname,ianldate
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use m_obsNode, only: obsNode
  use m_rwNode, only: rwNode
  use m_rwNode, only: rwNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot,tdrerr_inflate
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,&
       ges_lnprsl,sfcmod_gfs,sfcmod_mm5,comp_fact10, ges_tsen, ges_rho
  use gridmod, only: nsig,get_ijk
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad
  use constants, only: tiny_r_kind,cg_term,huge_single,r2000,three,one
  use jfunc, only: jiter,last,miter,jiterstart
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use setupdbz_lib, only:hx_dart
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  use directDA_radaruse_mod, only: rw_obs4wrd_bmwth, l_set_oerr_ratio_rw
  use directDA_radaruse_mod, only: l_plt_diag_rw, l_chk_bmwth
  use directDA_radaruse_mod, only: l_use_rw_columntilt


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
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind
  real(r_kind),parameter:: r200   = 200.0_r_kind

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables
  real(r_kind) rlow,rhgh,rsig
  real(r_kind) dz,factelv,factdif
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges,zsges0
  real(r_kind),dimension(nsig):: zges,hges,ugesprofile,vgesprofile
  real(r_kind),dimension(nsig):: wgesprofile!,vTgesprofile,refgesprofile
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk  
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat
  real(r_kind) qrgesin,qsgesin,qggesin,rhogesin,tempgesin, rhogesin0
  real(r_kind) rdBZ, vterminal,dbzgesin
  real(r_kind) sinazm,cosazm,sintilt,costilt,cosazm_costilt,sinazm_costilt
  real(r_kind) ratio_errors,qcgross
  real(r_kind) ugesin,vgesin,wgesin,factw,skint,sfcr
  real(r_kind) rwwind,presw
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) i,nchar,nreal,k,j,k1,ii
  integer(i_kind) mm1,jj,k2,isli
  integer(i_kind) jsig,ikxx,nn,ibin,ioff,ioff0
  integer(i_kind) ier,ilat,ilon,ihgt,irwob,ikx,itime,iuse
  integer(i_kind):: ielev,id,itilt,iazm,ilone,ilate,irange
  integer(i_kind):: izsges,ier2,idomsfc,isfcr,iskint,iff10,iobs_type
  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical debugging
  logical proceed
  logical include_w

  equivalence(rstation_id,station_id)
  real(r_kind) addelev,wrange,beamdepth,elevtop,elevbot
  integer(i_kind) kbeambot,kbeamtop,kbeamdiffmax,kbeamdiffmin
  real(r_kind) uminmin,umaxmax
  integer(i_kind) numequal,numnotequal,kminmin,kmaxmax
  real(r_kind) rwwindprofile

  type(sparr2) :: dhx_dx
  integer(i_kind) :: nnz, nind

  logical:: in_curbin, in_anybin, save_jacobian
  type(rwNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  character(len=*),parameter:: myname='setuprw'

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_w

  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qr
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qs
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_qg
  real(r_kind),allocatable,dimension(:,:,:,: ) :: ges_dbz

  integer(i_kind)   :: i_plt_diag 

  type(obsLList),pointer,dimension(:):: rwhead
  rwhead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf
! Check to see if required guess fields are available
  call check_vars_(proceed,include_w)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_(include_w)

  if ( l_use_rw_columntilt) then
!
     select case (rw_obs4wrd_bmwth)
        case (1)
           write(6,*)'SETUPRW: USE GSI RW obs forward operator (uminmax):',rw_obs4wrd_bmwth
        case (2)
           write(6,*)'SETUPRW: USE simple interpolation for rw obs forward operator :',rw_obs4wrd_bmwth
        case (3)
           write(6,*)'SETUPRW: USE ARPS weighted average interpolation for rw obs forward operator  :',rw_obs4wrd_bmwth
           write(6,*)'SETUPRW: not ready yet. ABORT'
           call stop2(999)
        case default
           write(6,*)'SETUPRW: invalid option for RW obs forward operator and ABORT. #:',rw_obs4wrd_bmwth
           call stop2(999)
     end select

! moreopts
     i_plt_diag = 0
     if (l_plt_diag_rw) then
         i_plt_diag = 1
         if (l_chk_bmwth) then
             i_plt_diag = 2
         end if
     else
         i_plt_diag = 0
         l_chk_bmwth = .false.
     end if
  end if

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ihgt=4      ! index of obs elevation
  irwob=5     ! index of radial wind observation
  iazm=6      ! index of azimuth angle in data array
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of obs type in data array
  itilt=9     ! index of tilt angle in data array
  ielev=10    ! index of radar elevation
  id=11       ! index of station id
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  irange=19   ! index of range in km of obs from radar
  izsges=20   ! index of model (guess) elevation for radar associated with vad wind
  ier2=21     ! index of original-original obs error
  iobs_type=22

  numequal=0
  numnotequal=0


! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=24
     nreal=ioff0
     if ( l_use_rw_columntilt) then
        if (l_plt_diag_rw) then
            ioff0=ioff0+5     ! 24-->29  25:dlat; 26:dlon; 27: dbz; 28: azm_old; 29: tilt_old
            if (l_chk_bmwth) then
                ioff0=ioff0+4 ! 29-->33  30:bmwth; 31: zob; 32: vr_min; 33;vr_max
            end if
        end if
     end if
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (save_jacobian) then
        nnz = 0
        nind = 0
        call new(dhx_dx, nnz, nind)
        nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if

  mm1=mype+1
  scale=one
  rsig=nsig

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
  kbeamdiffmin=huge(kbeamdiffmin)
  kbeamdiffmax=-huge(kbeamdiffmax)

  call dtime_setup()
  do i=1,nobs
     rwgt=one
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
 
        dpres=data(ihgt,i)
        ikx = nint(data(ikxx,i))
        error=data(ier2,i)
        slat=data(ilate,i)*deg2rad
        wrange=data(irange,i)
        zsges0=data(izsges,i)
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

!    Interpolate log(surface pressure),  
!    log(pres) at mid-layers, and geopotenital height to 
!    observation location.

     factw=data(iff10,i)
     if(sfcmod_gfs .or. sfcmod_mm5 .and. (.not. l_plt_diag_rw) ) then
        sfcr=data(isfcr,i)
        skint=data(iskint,i)
        isli=data(idomsfc,i)
        call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
     end if

     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     if(zsges>=dpres)then
        write(6,*) 'SETUPRW: zsges = ',zsges,'is greater than dpres ',dpres,'. Rejecting ob.'
        cycle
     endif
     dpres=dpres-zsges
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

!    Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23
     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
          ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
          two*flattening*sin2)
     termrg = (termg/grav)*termr
     do k=1,nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
     end do

!    Given observation height, (1) adjust 10 meter wind factor if
!    necessary, (2) convert height to grid relative units, (3) compute
!    compute observation pressure (for diagnostic purposes only), and
!    (4) compute location of midpoint of first model layer above surface
!    in grid relative units

!    Adjust 10m wind factor if necessary.  Rarely do we have a
!    lidar obs within 10 meters of the surface.  Almost always,
!    the code below resets the 10m wind factor to 1.0 i.e., no
!    reduction in wind speed due to surface friction).
     if (dpres<ten) then
        if(data(iobs_type,i) <= three)then
          dz = ten-dpres
          factw = factw + (factw-zero)/dz
        else
          term = max(dpres,zero)/ten
          factw = term*factw
        endif
     else
        factw=one
     endif

!    Convert observation height (in dpres) from meters to grid relative
!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd1(dpres,zges,nsig,1)

!    Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
     k1=max(1,k)
     k2=min(k+1,nsig)

!    Compute observation pressure (only used for diagnostics)
     if(k2>k1) then    !???????????? to fix problem where k1=k2, which should only happen if k1=k2=nsig
        dz     = zges(k2)-zges(k1)
        dlnp   = prsltmp(k2)-prsltmp(k1)
        pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
     else
        write(6,*)' iobs_type,data(iobs_type,i),k,k1,k2,nsig,zob,zges(k1),prsltmp(k1)=',&     !  diagnostic only??????????????
                          iobs_type,data(iobs_type,i),k,k1,k2,nsig,zob,zges(k1),prsltmp(k1)
        pobl   = prsltmp(k1)
     end if
        
     presw  = ten*exp(pobl)

!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp,nsig,-1)

!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)

!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)

!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if
     
!    Adjust observation error.
     if ( l_use_rw_columntilt .and. .not.l_set_oerr_ratio_rw ) then
        factelv=one
        factdif=one
     else
!    Increase error for observations over high topography
        factelv=one
        if (data(iobs_type,i) <= three) then
           if (data(ielev,i) > r2000) then
              factelv=(r2000/data(ielev,i))**2
              if(luse(i))awork(5) = awork(5) + one
           endif
        endif

!    Increase error if model and observation topography too different
        factdif=one
        if (data(iobs_type,i) <= three) then
           if (abs(zsges0-data(ielev,i)) > r200) then
              factdif= (r200/(abs(zsges0-data(ielev,i))))**2
              if(luse(i))awork(6) = awork(6) + one
           endif
        endif
     end if
     
!    Obtain estimated beam spread in vertical
     if (data(iobs_type,i) <= three) then
         addelev=max(half*abs(zsges0-data(ielev,i)),ten*wrange)
     else
         addelev=17.4*wrange   ! TDR radar beam width is 1.9 to 2.0 degree
     endif
     beamdepth=two*addelev
     elevtop=zob+addelev     !  this is based on 100ft/Nm = 16.5m/km beam spread
     elevbot=zob-addelev     !  for .95 deg beam angle (multiplied by 1.2 to allow
                             !  for propagation uncertainty)
                             !  also, a minimum uncertainty based on difference between
                             !  model surface elevation and actual radar elevation
                             ! for TDR radars, beam width is 1.9 for NOAA Parabolic 
                             ! and 2.0 degree for French dual-plate  

     call grdcrd1(elevtop,zges,nsig,1)
     call grdcrd1(elevbot,zges,nsig,1)
     kbeamtop=ceiling(elevtop)
     kbeambot=floor(elevbot)
     kbeamtop=max(1,min(kbeamtop,nsig))
     kbeambot=max(1,min(kbeambot,nsig))
     kbeamdiffmax=max(kbeamtop-kbeambot,kbeamdiffmax)
     kbeamdiffmin=min(kbeamtop-kbeambot,kbeamdiffmin)

     if ( l_use_rw_columntilt .and. .not.l_set_oerr_ratio_rw ) then
        ratio_errors = one
     else
        ratio_errors = factdif*factelv*error/(abs(data(ier,i) + 1.0e6_r_kind*rhgh +  &
             r8*rlow))
     end if
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero
     wgesin=zero

!    Interpolate guess u, v, and w to observation location and time.
     call tintrp31(ges_u,ugesin,dlat,dlon,dpres,dtime,&
          hrdifsig,mype,nfldsig)
     call tintrp31(ges_v,vgesin,dlat,dlon,dpres,dtime,&
          hrdifsig,mype,nfldsig)
     if(include_w) then
          call tintrp31(ges_w,wgesin,dlat,dlon,dpres,dtime,&
          hrdifsig,mype,nfldsig)
     end if

     call tintrp2a1(ges_u,ugesprofile,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(ges_v,vgesprofile,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     if( if_vterminal )then

       call tintrp31(ges_rho,rhogesin,dlat,dlon,dpres,dtime,&
            hrdifsig,mype,nfldsig)
       ! === In order to obtain the surface air density: rhogesin0
       call tintrp31(ges_rho,rhogesin0,dlat,dlon,0.0,dtime,&
            hrdifsig,mype,nfldsig)
  
       if( if_model_dbz ) then
         ! Interpolate guess reflectivity to observation location and time.
         call tintrp31(ges_dbz,dbzgesin,dlat,dlon,dpres,dtime,&
            hrdifsig,mype,nfldsig)
         rdBZ = dbzgesin
       else
  
         ! Interpolate guess qr, qs, qg, and rho to observation location and time.
         call tintrp31(ges_qr,qrgesin,dlat,dlon,dpres,dtime,&
              hrdifsig,mype,nfldsig)
         call tintrp31(ges_qs,qsgesin,dlat,dlon,dpres,dtime,&
              hrdifsig,mype,nfldsig)
         call tintrp31(ges_qg,qggesin,dlat,dlon,dpres,dtime,&
              hrdifsig,mype,nfldsig)
         call tintrp31(ges_tsen,tempgesin,dlat,dlon,dpres,dtime,&
              hrdifsig,mype,nfldsig)
    
         qrgesin  = max(qrgesin,1.e-6_r_kind)
         qsgesin  = max(qsgesin,1.e-8_r_kind)
         qggesin  = max(qggesin,1.e-9_r_kind)
         debugging = .false.
    
         call hx_dart(qrgesin,qggesin,qsgesin,rhogesin,tempgesin,rdBZ,debugging)
    
       end if ! end if-block if_model_dbz
  
       if(miter == 0) then !ie an enkf run
         if(rDBZ < 0.0_r_kind) rDBZ=0.0_r_kind ! should be the same as in the read_dbz when nopcp=.true.
       endif
       if(miter == 0 .and. ens_hx_dbz_cut) then !ie an enkf run
         if(rDBZ > 60.0_r_kind) rDBZ=60.0_r_kind
       endif
  
       ! === From (Atlas et al. 1973)
       vterminal = 2.65_r_kind*(rhogesin0/rhogesin)*rdBZ**0.114_r_kind
     else
       vterminal = 0.0_r_kind
     end if

     if(include_w) then 
          call tintrp2a1(ges_w,wgesprofile,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig) 
     end if

!    Convert guess u,v wind components to radial value consident with obs
     cosazm  = cos(data(iazm,i))  ! cos(azimuth angle)
     sinazm  = sin(data(iazm,i))  ! sin(azimuth angle)

     if( if_vrobs_raw ) then
       costilt = cos(data(itilt,i))
       if(include_w) then
         call dhdrange(data(itilt,i),data(irange,i),sintilt)
         costilt=sqrt(1.0_r_kind-sintilt*sintilt)
         rwwind = (ugesin*cosazm+vgesin*sinazm)*costilt*factw+(wgesin-vterminal)*sintilt*factw
       endif
     else ! if_vrobs_raw

       costilt = cos(data(itilt,i)) ! cos(tilt angle)
       sintilt = sin(data(itilt,i)) ! sin(tilt angle)
       cosazm_costilt = cosazm*costilt
       sinazm_costilt = sinazm*costilt
       !vTgesprofile= 5.40_r_kind*(exp((refgesprofile -43.1_r_kind)/17.5_r_kind)) 
!      rwwind = (ugesin*cosazm+vgesin*sinazm)*costilt*factw
  
       if ( .not. l_use_rw_columntilt ) then ! original code
         umaxmax=-huge(umaxmax)
         uminmin=huge(uminmin)
         kminmin=kbeambot
         kmaxmax=kbeamtop
         do k=kbeambot,kbeamtop
            rwwindprofile=ugesprofile(k)*cosazm_costilt+vgesprofile(k)*sinazm_costilt
            if(include_w) then
               rwwindprofile=rwwindprofile+wgesprofile(k)*sintilt 
            end if
        
            if(umaxmax<rwwindprofile) then
               umaxmax=rwwindprofile
               kmaxmax=k
            end if
            if(uminmin>rwwindprofile) then
               uminmin=rwwindprofile
               kminmin=k
            end if
         end do
         rwwind=data(irwob,i)
         if(data(irwob,i)<uminmin) then
            rwwind=uminmin
            dpres=kminmin
         end if
         if(data(irwob,i)>umaxmax) then
            rwwind=umaxmax
            dpres=kmaxmax
         end if
         if(rwwind==data(irwob,i)) then
            numequal=numequal+1
         else
            numnotequal=numnotequal+1
         end if
       else ! l_use_rw_columntilt uses rw_obs4wrd_bmwth flag to differentiate methods.
!             Add different options for consideration on uncertainty due to beam depth

         select case(rw_obs4wrd_bmwth)
            case(1)
               umaxmax=-huge(umaxmax)
               uminmin=huge(uminmin)
               kminmin=kbeambot
               kmaxmax=kbeamtop
               do k=kbeambot,kbeamtop
                  rwwindprofile=ugesprofile(k)*cosazm_costilt+vgesprofile(k)*sinazm_costilt
                  if(include_w) then
                     rwwindprofile=rwwindprofile+wgesprofile(k)*sintilt
                  end if

                  if(umaxmax<rwwindprofile) then
                     umaxmax=rwwindprofile
                     kmaxmax=k
                  end if
                  if(uminmin>rwwindprofile) then
                     uminmin=rwwindprofile
                     kminmin=k
                  end if
               end do
               rwwind=data(irwob,i)
               if(data(irwob,i)<uminmin) then
                  rwwind=uminmin
                  dpres=kminmin
               end if
               if(data(irwob,i)>umaxmax) then
                  rwwind=umaxmax
                  dpres=kmaxmax
               end if
               if(rwwind==data(irwob,i)) then
                  numequal=numequal+1
               else
                  numnotequal=numnotequal+1
               end if
            case(2)
               rwwind = (ugesin*cosazm+vgesin*sinazm)*costilt
               if (include_w) then
                  rwwind = rwwind+wgesin*sintilt
               end if
               umaxmax=rwwind
               uminmin=rwwind
               kminmin=dpres
               kmaxmax=dpres
            case(3)
               umaxmax=-huge(umaxmax)
               uminmin=huge(uminmin)
               kminmin=kbeambot
               kmaxmax=kbeamtop
               do k=kbeambot,kbeamtop
                  rwwindprofile=(ugesprofile(k)*cosazm+vgesprofile(k)*sinazm)*costilt
                  if(umaxmax<rwwindprofile) then
                     umaxmax=rwwindprofile
                     kmaxmax=k
                  end if
                  if(uminmin>rwwindprofile) then
                     uminmin=rwwindprofile
                     kminmin=k
                  end if
               end do
               write(6,*) 'SETUPRW: weighted-average of multiple layers is not ready yet.  ABORT:',rw_obs4wrd_bmwth
               call stop2(999)
            case default
               write(6,*) 'SETUPRW: option for vertical interpolation is valid. ABORT:',rw_obs4wrd_bmwth
               call stop2(999)
         end select
       end if ! END of l_use_rw_columntilt
     end if
     
     ddiff = data(irwob,i) - rwwind

     if (doradaroneob) then
       if(oneobvalue > -900_r_kind) then
         data(irwob,i) = oneobvalue
         ddiff = data(irwob,i) - rwwind
       else
         ddiff = oneobddiff
         data(irwob,i) = rwwind + ddiff
       endif
     endif

!    adjust obs error for TDR data 
     if( ratio_errors*error > tiny_r_kind &
        .and. tdrerr_inflate) then
        if(data(iobs_type,i) > three) then
           ratio_errors = data(ier2,i)/abs(data(ier,i) + 1.0e6_r_kind*rhgh +  &
             r8*rlow + min(max((abs(ddiff)-ten),zero)/ten,one)*data(ier,i))
        end if
!    apply same error adjustment for 88D data in HWRF with a 5 m/s
!    minimum, which is also the same for TDR. This results in roughly
!    2.5 m/s RMS fit with 10-km thinning
        if(data(iobs_type,i) <= three) then
           ratio_errors = data(ier2,i)/(5.0_r_kind + abs( 1.0e6_r_kind*rhgh +  &
             r8*rlow + min(max((abs(ddiff)-ten),zero)/ten,one)*5.0_r_kind))
        end if
     end if 

 !    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     qcgross=cgross(ikx)

     if ( l_use_rw_columntilt .and. .not.l_set_oerr_ratio_rw ) then
        ratio_errors = one
     else
        if (ratio > qcgross .or. ratio_errors < tiny_r_kind) then
           if (luse(i)) awork(4) = awork(4)+one
           error = zero
           ratio_errors = zero
        end if
     end if
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     !-- if (nobskeep>0.and.luse_obsdiag) muse(i)=obsdiags(i_rw_ob_type,ibin)%tail%muse(nobskeep)
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))
     
     val     = error*ddiff

!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
        
!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+val2*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        ress  = scale*ddiff
        ressw = ress*ress
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
              
           end if
        end do
     end if

     if (luse_obsdiag) then
        if ( l_use_rw_columntilt ) then
           call obsdiagNode_set(my_diag, luse=luse(i),wgtjo=(error*ratio_errors)**2, &
              jiter=jiter, muse=muse(i), nldepart=ddiff)
        else
           call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
              jiter=jiter, muse=muse(i), nldepart=ddiff)
        end if
     endif
             
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        allocate(my_head)
        call rwNode_appendto(my_head,rwhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        my_head%dlev = dpres
        my_head%factw= factw
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)

        do j=1,8
           my_head%wij(j)=factw*my_head%wij(j)  
        end do
        my_head%raterr2 = ratio_errors**2  
        my_head%cosazm_costilt = cosazm_costilt
        my_head%sinazm_costilt = sinazm_costilt
        my_head%sintilt = sintilt
        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%time    = dtime
        my_head%luse    = luse(i)
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)


        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     endif

!    Save select output for diagnostic file
     if(conv_diagsave)then
        ii=ii+1
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

        if (binary_diag) call contents_binary_diag_(my_diag)
        if (netcdf_diag) call contents_netcdf_diag_(my_diag)

     end if
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        if ( l_use_rw_columntilt) then
           write(7)' rw',nchar,nreal,ii,mype,ioff0,i_plt_diag
        else
           write(7)' rw',nchar,nreal,ii,mype,ioff0
        end if
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
  end if

! End of routine

  return
  contains

  subroutine check_vars_ (proceed, include_w)
  logical,intent(inout) :: proceed
  logical,intent(inout) :: include_w
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
  call gsi_metguess_get ('var::w' , ivar, istatus )
  if (if_use_w_vr.and.ivar>0) then
     include_w=.true.
     if(if_vterminal)then
      if( .not. if_model_dbz ) then
        call gsi_metguess_get ('var::qr', ivar, istatus )
        proceed=proceed.and.ivar>0
        call gsi_metguess_get ('var::qs', ivar, istatus )
        proceed=proceed.and.ivar>0
        call gsi_metguess_get ('var::qg', ivar, istatus )
        proceed=proceed.and.ivar>0
      else
        call gsi_metguess_get ('var::dbz', ivar, istatus )
        proceed=proceed.and.ivar>0
      endif
     end if
  else
     include_w=.false.
  endif
  end subroutine check_vars_ 

  subroutine init_vars_(include_w)

  logical,intent(in   ):: include_w
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

     if(if_vterminal)then
       if(if_model_dbz)then
         !    get dbz ....
         varname='dbz'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
           if(allocated(ges_dbz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
           endif
           allocate(ges_dbz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_dbz(:,:,:,1)=rank3
           do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_dbz(:,:,:,ifld)=rank3
           enddo
         else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
         endif
       else
         ! get qr ...
         varname='qr'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
             if(allocated(ges_qr))then
                write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                call stop2(999)
             endif
             allocate(ges_qr(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
             ges_qr(:,:,:,1)=rank3
             do ifld=2,nfldsig
                call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                ges_qr(:,:,:,ifld)=rank3
             enddo
         else
             write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
             call stop2(999)
         endif
    
    !    get qs ...
         varname='qs'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
             if(allocated(ges_qs))then
                write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                call stop2(999)
             endif
             allocate(ges_qs(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
             ges_qs(:,:,:,1)=rank3
             do ifld=2,nfldsig
                call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                ges_qs(:,:,:,ifld)=rank3
             enddo
         else
             write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
             call stop2(999)
         endif
    !    get qg ...
         varname='qg'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
             if(allocated(ges_qg))then
                write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
                call stop2(999)
             endif
             allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
             ges_qg(:,:,:,1)=rank3
             do ifld=2,nfldsig
                call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
                ges_qg(:,:,:,ifld)=rank3
             enddo
         else
             write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
             call stop2(999)
         endif
    
    
       endif
     endif


!    get w ...
     if(include_w) then
        varname='w'
        call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
        if (istatus==0) then
            if(allocated(ges_w))then
               write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
               call stop2(999)
            endif
            allocate(ges_w(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
            ges_w(:,:,:,1)=rank3
            do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
               ges_w(:,:,:,ifld)=rank3
            enddo
        else
            write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle,ier= ',istatus
            call stop2(999)
        endif
     end if
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
900  format('conv_rw_',i2.2,'.nc4')
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
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

!       rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(9,ii)  = data(iobs_type,i)  !    observation subtype 
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

        rdiagbuf(17,ii) = data(irwob,i)      ! radial wind speed observation (m/s)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = data(irwob,i)-rwwind  ! obs-ges w/o bias correction (m/s) (future slot)


        rdiagbuf(20,ii)=data(iazm,i)*rad2deg ! azimuth angle
        rdiagbuf(21,ii)=data(itilt,i)*rad2deg! tilt angle
        rdiagbuf(22,ii) = factw              ! 10m wind reduction factor

        rdiagbuf(23,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)
        rdiagbuf(24,ii) = 1.e+10_r_single    ! ges ensemble spread (filled in EnKF)

!-----------------------------------------------------------------------------------------------------!
!       l_use_rw_columntilt / add extra elements to diagbuf for plotting diag of rw  (moreopts)
!       put original azimuth, tilt and dbz into cdata for plot diag file.
        if (l_use_rw_columntilt) then
           if (l_plt_diag_rw) then
              rdiagbuf(25,ii) = data(ilat,i)       ! obs grid-y
              rdiagbuf(26,ii) = data(ilon,i)       ! obs grid-x
              rdiagbuf(27,ii) = data(isfcr,i)      ! obs reflectivity at same point from same radar
              rdiagbuf(28,ii) = data(idomsfc,i)    ! azimuth_old (degree, put in element for skint)
              rdiagbuf(29,ii) = data(iskint,i)     ! tilt_old (degree, put in element for skint)

              if (l_chk_bmwth) then
                 rdiagbuf(30,ii) = beamdepth      ! obs subtype --> beamdepth(meter)
                 rdiagbuf(31,ii) = zob            ! obs pressure --> zob (meter)
                 rdiagbuf(32,ii) = uminmin        ! iuse --> uminmin (Vr_min)
                 rdiagbuf(33,ii) = umaxmax        ! errinv_adjust --> uminmin(Vr_min)
              end if

           end if
        end if
!-----------------------------------------------------------------------------------------------------!

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
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '     rw'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata_to_single("Latitude",data(ilate,i)      )
           call nc_diag_metadata_to_single("Longitude",data(ilone,i)      )
           call nc_diag_metadata_to_single("Station_Elevation",data(ielev,i)      )
           call nc_diag_metadata_to_single("Pressure",presw              )
           call nc_diag_metadata_to_single("Height",data(ihgt,i)       )
           call nc_diag_metadata_to_single("Time",dtime,time_offset,'-')
           call nc_diag_metadata("Prep_QC_Mark",            0.0_r_single           )
           call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)       )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb                 )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt               )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -1.0_r_single          )
           endif

           call nc_diag_metadata_to_single("Errinv_Input",errinv_input       )
           call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst       )
           call nc_diag_metadata_to_single("Errinv_Final",errinv_final       )

           call nc_diag_metadata_to_single("Observation",data(irwob,i)  )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff            )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",data(irwob,i),rwwind,'-')

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
           if (save_jacobian) then
              call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
              call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
              call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
           endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_w )) deallocate(ges_w )
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_w )) deallocate(ges_w )
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qs)) deallocate(ges_qs)
    if(allocated(ges_qr)) deallocate(ges_qr)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_dbz)) deallocate(ges_dbz)
  end subroutine final_vars_

SUBROUTINE dhdrange(elvang,range,dhdr)
  use kinds, only: r_kind,r_single,r_double,i_kind

  IMPLICIT NONE
  REAL(r_kind), INTENT(IN) :: range
  REAL(r_kind), INTENT(IN) :: elvang
  REAL(r_kind), INTENT(OUT) :: dhdr
!
  DOUBLE PRECISION :: eradius,frthrde,eighthre,fthsq,deg2rad
  PARAMETER (eradius=6371.0_r_kind,                                          &
             frthrde=(4._r_kind*eradius/3._r_kind),                                   &
             eighthre=(8._r_kind*eradius/3._r_kind),                                  &
             fthsq=(frthrde*frthrde),                                   &
             deg2rad=(3.14592654_r_kind/180._r_kind))
!
  DOUBLE PRECISION :: sinelv,dhdrdb,drange
!
  drange=DBLE(range)
  sinelv=SIN(DBLE(elvang))
  dhdrdb = (drange+frthrde*sinelv)/                                     &
         SQRT(drange*drange + fthsq + eighthre*drange*sinelv)
  dhdr = dhdrdb
!
  RETURN
END SUBROUTINE dhdrange


end subroutine setuprw
end module rw_setup
