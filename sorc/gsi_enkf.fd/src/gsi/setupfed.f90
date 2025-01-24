module fed_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupfed; end interface

contains
subroutine setupfed(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,fed_diagsave,init_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupfed     compute rhs of flash extent density 
!   orig. prgmmr: 
!                 Rong Kong   CAPS/OU   2018-01-21 (modified based on setupdbz.f90)
!   modified:
!                 Yaping Wang         CIMMS/OU   2019-11-11
!                 David Dowell (DCD)  NOAA GSL   2021-07-01
!                 - added a second option (tanh) for observation operator, based on the
!                   work of Sebok and Back (2021, unpublished)
!                 - capped maximum model FED
!                Hongli Wang         NOAA GSL    2023-09-14
!                 - Add option to use fed from background file to calculate fed innov
!                 - cleanup code, removed hardcoded obs height (6500m)
!
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert
  use obsmod, only: rmiss_single,&
       lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset                                                   
  use obsmod, only: oberror_tune
  use obsmod, only: if_model_fed,innov_use_model_fed,dofedoneob,oneobddiff,oneobvalue
  use m_obsNode, only: obsNode
  use m_fedNode, only: fedNode
  use m_fedNode, only: fedNode_appendto
  use obsmod, only: luse_obsdiag, netcdf_diag, binary_diag, dirname, ianldate
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim,nc_diag_read_close
  use m_obsLList, only: obsLList
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr
  use guess_grids, only: hrdifsig,nfldsig,ges_prsi
  use guess_grids, only: ges_lnprsl, geop_hgtl
  use gridmod, only: lat2, lon2
  use gridmod, only: nsig, get_ij,get_ijk,tll2xy
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim
  use constants, only: half,one,two,grav_equator,eccentricity,somigliana
  use constants, only: deg2rad,r60,tiny_r_kind,cg_term,huge_single
  use constants, only: r10,r100,r1000
  use constants, only: grav,tpwcon
  use qcmod, only: npres_print,ptopq,pbotq
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use converr, only: ptabl 
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use state_vectors, only: nsdim
 
  use gsi_bundlemod,    only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: gsi_metguess_get, GSI_MetGuess_Bundle

  use netcdf


  implicit none
! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: fed_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index
  logical                                          ,intent(in   ) :: init_pass ! state of "setup" parameters


! Declare local parameters
  integer(i_kind),parameter:: fed_obs_ob_shape = 2       ! 1 = linear (Allen et al.)
                                                         ! 2 = tanh (Sebok and Back)
! coefficients for tanh operator, from Sebok and Back (2021)
!  real(r_kind),parameter:: a_coeff = 8.4_r_kind       ! a (flashes/min) in tanh operator
!  real(r_kind),parameter:: b_coeff = 12.248_r_kind    ! b (flashes/min) in tanh operator
!  real(r_kind),parameter:: c_coeff = 5.0e-10_r_kind   ! c (radians/kg) in tanh operator
!  real(r_kind),parameter:: d_coeff = 1.68e9_r_kind    ! d (kg) in tanh operator
!  real(r_kind),parameter:: fed_highbnd = 18.0_r_kind  ! DCD:  Sebok and Back (2021, unpublished)

! coefficients for tanh operator, from work by A. Back with regional FV3 (2023)
  real(r_kind),parameter:: a_coeff = -3.645_r_kind      ! a (flashes/min) in tanh operator
  real(r_kind),parameter:: b_coeff = 15.75_r_kind       ! b (flashes/min) in tanh operator
  real(r_kind),parameter:: c_coeff = 1.939e-10_r_kind   ! c (radians/kg) in tanh operator
  real(r_kind),parameter:: d_coeff = -1.215e9_r_kind    ! d (kg) in tanh operator
  real(r_kind),parameter:: fed_highbnd = 8.0_r_kind    ! DCD:  Back (2023, unpublished) for FV3

  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind
  real(r_kind),parameter:: D608=0.608_r_kind 
  character(len=*),parameter:: myname='setupfed'

! Declare external calls for code analysis
  external:: tintrp2a1
  external:: tintrp2a11
  external:: tintrp2a1116
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables
  real(r_kind) rlow,rhgh,rsig
  real(r_kind) dz
  real(r_kind) jqg_num,jqg
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges
  real(r_kind),dimension(nsig):: zges,hges
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk 
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat,dlat8km,dlon8km
  real(r_kind) ratio_errors
  real(r_kind) presw
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(lat2,lon2,nfldsig)::rp
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qg,ges_qg_mask
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_fed
  real(r_kind) :: presq
  real(r_kind) :: T1D,RHO
  real(r_kind) :: glmcoeff = 2.088_r_kind*10.0**(-8.0)   ! Allen et al. (2016,MWR)
  real(r_kind) :: CM = 0.5_r_kind    ! tuning factor in eq. 14 of Kong et al. 2020

  integer(i_kind) i,nchar,nreal,k,j,k1,ii,jj
  integer(i_kind) mm1,k2
  integer(i_kind) jsig,ikxx,nn,ibin,ioff,ioff0
  integer(i_kind) ier,ilat,ilon,ifedob,ikx,itime,iuse
  integer(i_kind) id,ilone,ilate
  integer(i_kind) ier2

  integer(i_kind) nlat_ll,nlon_ll,nsig_ll,nfld_ll

  integer(i_kind) ipres,iqmax,iqc,icat,itemp
  integer(i_kind) istnelv,iobshgt,izz,iprvd,isprvd,iptrb
  integer(i_kind) idomsfc,iskint,isfcr,iff10
 
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(80):: string
  character(128):: diag_file
  logical :: diagexist
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed

  equivalence(rstation_id,station_id)
  integer(i_kind) numequal,numnotequal
 
  type(fedNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  real(r_kind),dimension(nsig+1):: prsitmp


!------------------------------------------------!

  integer(i_kind) :: itmp,jtmp

  integer(i_kind), parameter :: ntimesfed=1
  integer(i_kind),parameter :: nxfed=99, nyfed=99, nzfed=1, nfldfed=3
  real(r_kind),dimension(nobs) :: FEDMdiag,FEDMdiagTL
  integer(i_kind) :: npt
  real(r_kind) :: dlat_earth,dlon_earth

! YPW added the next lines
  logical :: l_set_oerr_ratio_fed=.False.
  logical :: l_gpht2gmht = .True.
  real(r_kind),dimension(nobs) :: dlatobs,dlonobs
  integer(i_kind):: ngx,ngy,igx,jgy
  real(r_kind):: dx_m, dy_m

  type(obsLList),pointer,dimension(:):: fedhead
  fedhead => obsLL(:)

!============================================================================================
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

  write(6,*)myname,'(pe=',mype,') nele nobs =',nele,nobs,     &
      'luse_obsdiag=',luse_obsdiag,'lat2,lon2=',lat2,lon2

  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ifedob=5     ! index of fed observation
  id=6        ! index of station id
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of ob type
  iqmax=9     ! index of max error
  itemp=10    ! index of dry temperature
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
  iobshgt=21  ! index of observation height (m)
  izz=22      ! index of surface height
  iprvd=23    ! index of observation provider
  isprvd=24   ! index of observation subprovider
  icat =25    ! index of data level category
  iptrb=26    ! index of fed perturbation
  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

  if (dofedoneob) then
    muse=.true.
  end if

  numequal=0
  numnotequal=0

! 
! If requested, save select data for output to diagnostic file
  if(fed_diagsave)then
     ii=0
     nchar=1_i_kind
     ioff0=26_i_kind               ! 21 + 5 (22->Zr; 23->Zs; 24->Zg; 25->tsenges;26->RHO;)
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     rdiagbuf=zero
     if(netcdf_diag) call init_netcdf_diag_
  end if
  mm1=mype+1
  scale=one
  rsig=nsig


 
 !============================================================================================
!
! Check to see if required guess fields are available
! vars. list: ps, z, q
! vars. list: qr, qs, qg
 !============================================================================================

  call check_vars_(proceed)
  if(.not.proceed) then
      write(6,*) myname,': some or all necessary variables are not available for fed obs operator. Quit!'
      return  ! not all vars available, simply return
  end if

! If require guess vars available, extract from bundle ...
  call init_vars_
!  qscalar=zero 
 
 !============================================================================================
 ! 1) Calculate the graupel-mass and graupel-volume based flash extent density
 ! (FED) on model space, added by R. Kong, 07/05/2018
 !============================================================================================
  ges_qg_mask=ges_qg
  where(ges_qg>0.0005)  !Count the volume where qg > 0.5/kg
     ges_qg_mask=1.0
  elsewhere
     ges_qg_mask=0.0
  endwhere

  ! Operator start here
  ! set ngx and ngy =2, so the integrated domain is 15kmx15km
  ngx = 2
  ngy = 2
  dx_m = 3000.
  dy_m = 3000.
  print*,'FED Operator start here!,ngx=',ngx,'ngy=',ngy
  rp=zero

  print*, 'mype = ', mype
  print*, 'nfldsig = ', nfldsig
  print*, 'nsig = ', nsig
  print*, 'lon2 = ', lon2
  print*, 'lat2 = ', lat2
  if (.not. innov_use_model_fed .or. .not. if_model_fed) then
! compute graupel mass, in kg per 15 km x 15 km column
    do jj=1,nfldsig
      do k=1,nsig
        do i=1,lon2
           do j=1,lat2   !How to handle MPI????
              do igx=1,2*ngx+1 !horizontal integration of qg around point to calculate FED
                do jgy=1,2*ngy+1
                  itmp = i-ngx+igx-1
                  jtmp = j-ngy+jgy-1
                  itmp = min(max(1,itmp),lon2)
                  jtmp = min(max(1,jtmp),lat2)
                    rp(j,i,jj)=rp(j,i,jj) + ges_qg(jtmp,itmp,k,jj)* &
                               dx_m*dy_m*(ges_prsi(jtmp,itmp,k,jj)-ges_prsi(jtmp,itmp,k+1,jj))*&
                               tpwcon * r10
                end do   !igx
              end do  !jgy
           end do !j
        end do !i
      end do !k
    end do !jj

! compute FED, in flashes/min
    do jj=1,nfldsig
      do i=1,lon2
        do j=1,lat2
           if (fed_obs_ob_shape .eq. 1) then
              rp(j,i,jj) = CM * glmcoeff * rp(j,i,jj)
           else if (fed_obs_ob_shape .eq. 2) then
              rp(j,i,jj) = a_coeff + b_coeff &
                           * tanh(c_coeff * (rp(j,i,jj) - d_coeff))
           else
              write(6,*) ' unknown fed_obs_ob_shape: ', fed_obs_ob_shape
              write(6,*) ' aborting setupfed'
              call stop2(999)
           end if
           if (rp(j,i,jj) .gt. fed_highbnd) rp(j,i,jj) = fed_highbnd
        end do !j
      end do !i
    end do !jj
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     write(6,*) 'fed_obs_ob_shape=',fed_obs_ob_shape
     if (fed_obs_ob_shape .eq. 2) then
       write(6,*) 'a_coeff=',a_coeff
       write(6,*) 'b_coeff=',b_coeff
       write(6,*) 'c_coeff=',c_coeff
       write(6,*) 'd_coeff=',d_coeff
     end if
     write(6,*) 'fed_highbnd=',fed_highbnd
     write(6,*) 'maxval(ges_qg)=',maxval(ges_qg),'pe=',mype
  end if ! .not. innov_use_model_fed .or. .not. if_model_fed

 !============================================================================================

  nlat_ll=size(ges_qg,1)
  nlon_ll=size(ges_qg,2)
  nsig_ll=size(ges_qg,3)
  nfld_ll=size(ges_qg,4)
  
  do i=1,nobs
     dtime=data(itime,i)
     dlat=data(ilat,i)
     dlon=data(ilon,i) 

     dlon8km=data(iprvd,i)   !iprvd=23
     dlat8km=data(isprvd,i)   !isprvd=24

     dpres=data(ipres,i)                        ! from rdararef_mosaic2: this height abv MSL
     ikx = nint(data(ikxx,i))
     error=data(ier2,i)
     slat=data(ilate,i)*deg2rad                 ! needed when converting geophgt to 
     dlon_earth = data(ilone,i)                 !the lontitude and latitude on the obs pts.
     dlat_earth = data(ilate,i)
                                                   ! geometric hgh (hges --> zges below)

     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     end if

     if (ibin<1.or.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
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
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     end if

!     Interpolate terrain height(model elevation) to obs location.
     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&    
          mype,nfldsig)
!  print*,'i,after tintrp2all',i,mype,dlat,zsges
!     1. dpres (MRMS obs height is height above MSL) is adjusted by zsges, so it
!        is changed to height relative to model elevation (terrain).
!        because in GSI, geop_hgtl is the height relative to terrain (ges_z)
!        (subroutine guess_grids)
     dpres=dpres-zsges
     if (dpres<zero) cycle  !  temporary fix to prevent out of bounds array reference in zges,prsltmp
!     Interpolate log(ps) & log(pres) at mid-layers and geo-potential height to
!     obs locations/times
!     Note: geop_hgtl is relative to model terrain, i.e. height - ges_z (ref. to
!     subroutine guess_grids)
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)
     call tintrp2a1(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

     call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime, &
          hrdifsig,nsig+1,mype,nfldsig)
!   
!     2. Convert geopotential height at layer midpoints to geometric height
!     using
!        equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!        measures of altitude" (2001).  Available on the web at
!        http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!        termg  = equation 17
!        termr  = equation 21
!        termrg = first term in the denominator of equation 23
!        zges   = equation 23

     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
            ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
             two*flattening*sin2)
     termrg = (termg/grav)*termr
! 
     if (l_gpht2gmht) then
          do k=1,nsig
              zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
          end do
     else
          do k=1,nsig
              zges(k) = hges(k)
          end do
     end if

!!    Convert observation height (in dpres) from meters to grid relative
!!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd1(dpres,zges(1),nsig,1)

!     Set indices of model levels below (k1) and above (k2) observation.
!     wm - updated so {k1,k2} are at min {1,2} and at max {nsig-1,nsig}
     k=dpres
     k1=min(max(1,k),nsig-1)
     k2=min(k1+1,nsig)

!  print*,'Compute observation pressure',i,mype
!!    Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
     presw  = r10*exp(pobl)
     presq  = presw
!!    Determine location in terms of grid units for midpoint of
!!    first layer above surface
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp(1),nsig,-1)
!!    Check to see if observation is below midpoint of first
!!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)
!!    Check to see if observation is above midpoint of layer
!!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)
!!    Increment obs counter along with low and high obs counters
     if(luse(i))then
         awork(1)=awork(1)+one
         if(rhgh/=zero) awork(2)=awork(2)+one
         if(rlow/=zero) awork(3)=awork(3)+one
     end if

!    Adjust observation error.   
!    Observation error currently assumed from user-defined namelist (oe_dbz) 
!    and is *not* adjusted
     if(l_set_oerr_ratio_fed) then
         ratio_errors = error/(abs(data(ier,i) + 1.0e6_r_kind*rhgh +  &
             r8*rlow))
     else
         ratio_errors = one
     end if
!
!    Not adjusting obs error based upon ob vertical location relative to grid
!    box
     if(l_set_oerr_ratio_fed) then
         ratio_errors = error/(abs(data(ier,i)))
     else
         ratio_errors = one
     end if

     error = one/error
     if(dpres < zero .or. dpres > rsig)ratio_errors = zero

!----------------------------------------------------------------------------!
!                                                                            !
! Implementation of forward operator for flash extend densit ----------------!
!                                                                            !
!----------------------------------------------------------------------------!

 !============================================================================================
 ! 3) H(x), interpolate the FED from model space on the local domain to obs space (FEDMdiag)
 !============================================================================================

     npt = 0
     FEDMdiag(i) = 0.
     if (if_model_fed .and. innov_use_model_fed) then
        !use fed from background file
        call tintrp31(ges_fed,FEDMdiag(i),      dlat,dlon,dpres,dtime,hrdifsig,mype,nfldsig)       
     else
        call tintrp2a11(rp,FEDMdiag(i),dlat,dlon,dtime,hrdifsig,mype,nfldsig)
     end if
     dlonobs(i) = dlon_earth
     dlatobs(i) = dlat_earth

     ! also Jacobian used for TLM and ADM
     !FEDMdiagTL, used for gsi-3dvar,will be implemented in future......
     FEDMdiagTL(i) = 0.
     jqg_num = FEDMdiagTL(i)     !=dFED/Dqg
     jqg  = jqg_num
   
   
     if(FEDMdiag(i)==data(ifedob,i)) then
        numequal=numequal+1
     else
        numnotequal=numnotequal+1
     end if
 
!!!!!!!!!!!!!!!!!END H(x)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
     ! Compute innovations
     !--------------Calculate departure from observation----------------!
     
     ddiff = data(ifedob,i) - FEDMdiag(i)
     if (dofedoneob) then
        !use magoberr to define obs error, but oneobtest=.false.
        if(magoberr <= zero) magoberr=1.0_r_kind 
        error=one/(magoberr)
        ratio_errors=one
        if (jiter==1) then
          if (oneobvalue > 0_r_kind) then
            data(ifedob,i) = oneobvalue
            ddiff = data(ifedob,i) - FEDMdiag(i) 
          else
            ddiff = oneobddiff
            data(ifedob,i) = FEDMdiag(i)+ddiff
            oneobvalue = data(ifedob,i) 
          endif
          write(6,*)"FED_ONEOB: O_Val,B_Val= ",data(ifedob,i),FEDMdiag(i)
          write(6,*)"FED_ONEOB: Innov,Error= ",ddiff,magoberr
        else
          data(ifedob,i) = oneobvalue
          ddiff = data(ifedob,i) - FEDMdiag(i)
        end if 
     end if !oneob
           
!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)     
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)              != y-H(xb)      
     ratio    = residual/obserrlm       != y-H(xb)/sqrt(R)   
     
     if (l_set_oerr_ratio_fed) then
         if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
            if (luse(i)) awork(4) = awork(4)+one
            error = zero
            ratio_errors = zero
         end if
     else
         ratio_errors = one
     end if
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
!     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_fed_ob_type,ibin)%tail%muse(nobskeep)
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))
     
     val     = error*ddiff             !=y-H(xb)/sqrt(R)   
             
!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val                     !(o-g)**2/R, would be saved in awork
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
        end if
        valqc = -two*rat_err2*term
       
!     print*,'Compute penalty terms'
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
!          if(presw >=ptop(k) .and. presw<=pbot(k))then
           if(presq >=ptopq(k) .and. presq<=pbotq(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
             
           end if
        end do
     end if

     if(luse_obsdiag)then
         call obsdiagNode_set(my_diag, luse=luse(i), wgtjo=(error*ratio_errors)**2, &
                              jiter=jiter, muse=muse(i), nldepart=ddiff)
     end if
     
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then             

        allocate(my_head)                ! YPW added
        call fedNode_appendto(my_head,fedhead(ibin))

        my_head%idv=is
        my_head%iob=i
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        my_head%dlev= dpres
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij) 
        my_head%res     = ddiff                            ! Observation - ges
        my_head%err2    = error**2
        my_head%raterr2 = ratio_errors**2
        my_head%time    = dtime
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%luse    = luse(i)

        if(oberror_tune) then
         !  my_head%fedpertb=data(iptrb,i)/error/ratio_errors
           my_head%kx=ikx
           if(presq > ptabl(2))then
              my_head%k1=1
           else if( presq <= ptabl(33)) then
              my_head%k1=33
           else
              k_loop: do k=2,32
                 if(presq > ptabl(k+1) .and. presq <= ptabl(k)) then
                    my_head%k1=k
                    exit k_loop
                 end if
              end do k_loop
           end if
        end if
!-------------------------------------------------

        if(luse_obsdiag)then
            call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
            my_head%diags => my_diag
        end if

        my_head => null()
     end if

!    Save select output for diagnostic file
     if(.not.luse(i))write(6,*)' luse, mype',luse(i),mype
     if(fed_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        end if
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)

     end if
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(fed_diagsave .and. netcdf_diag) call nc_diag_write
  if(fed_diagsave .and. binary_diag .and. ii>0)then

     write(string,600) jiter
600  format('fed_',i2.2)
     diag_file=trim(dirname) // trim(string)
     if(init_pass) then
        open(66,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
     else
        inquire(file=trim(diag_file),exist=diagexist)
        if (diagexist) then
           open(66,file=trim(diag_file),form='unformatted',status='old',position='append')
        else
           open(66,file=trim(diag_file),form='unformatted',status='unknown',position='rewind')
        end if
     end if
     if(init_pass .and. mype == 0) then
        write(66) ianldate
        write(6,*)'SETUPFED:   write time record to file ',&
                trim(diag_file), ' ',ianldate
     end if

!     call dtime_show(myname,'diagsave:fed',i_fed_ob_type)
     write(66)'fed',nchar,nreal,ii,mype,ioff0
     write(66)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
 !    write(6,*)'fed,nchar,nreal,ii,mype',nchar,nreal,ii,mype
     deallocate(cdiagbuf,rdiagbuf)
     close(66)
  end if

! End of routine


!  return

  contains

  subroutine check_vars_ (proceed)


  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q' , ivar, istatus )
  proceed=proceed.and.ivar>0
! call gsi_metguess_get ('var::tv' , ivar, istatus )
! proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::qs', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::qg', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::qr', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_


  subroutine init_vars_

!  use radaremul_cst, only: mphyopt
  use obsmod, only: if_model_fed
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
         end if
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         end do
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     end if
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         end if
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         end do

     if(if_model_fed)then
     !    get fed ....
         varname='fed'
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
         if (istatus==0) then
           if(allocated(ges_fed))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
           endif
           allocate(ges_fed(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
           ges_fed(:,:,:,1)=rank3
           do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_fed(:,:,:,ifld)=rank3
           enddo
         else
           write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
           call stop2(999)
         endif
     endif

     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     end if
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         end if
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         end do
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     end if
!    get tv ...
!    varname='tv'
!    call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
!    if (istatus==0) then
!        if(allocated(ges_tv))then
!           write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
!           call stop2(999)
!        end if
!        allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
!        ges_tv(:,:,:,1)=rank3
!        do ifld=2,nfldsig
!           call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
!           ges_tv(:,:,:,ifld)=rank3
!           ges_tv(:,:,:,ifld)=rank3
!        end do
!    else
!        write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
!        call stop2(999)
!    end if
!    get qr ...
!    get qg ...
     varname='qg'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qg))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         end if
         allocate(ges_qg(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         if(.not. allocated(ges_qg_mask))then
           allocate(ges_qg_mask(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         end if

         ges_qg(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qg(:,:,:,ifld)=rank3
         end do
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     end if
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  end if
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('conv_fed_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists. Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append                                          
        end if
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?                                                   
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     end if
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
        rdiagbuf(8,ii)  = (dtime*r60)-time_offset  ! obs time (sec relative to analysis time)
        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        end if

        rdiagbuf(13,ii) = rwgt                 ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input         ! prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(15,ii) = errinv_adjst         ! read_prepbufr inverse obs error (dBZ)**-1
        rdiagbuf(16,ii) = errinv_final         ! final inverse observation error (dBZ)**-1
        rdiagbuf(17,ii) = data(ifedob,i)       ! radar reflectivity observation (dBZ)
        rdiagbuf(18,ii) = ddiff                ! obs-ges (dBZ)
        rdiagbuf(19,ii) = data(ifedob,i)-FEDMdiag(i)  ! obs-ges w/o bias correction (dBZ) (future slot)
        rdiagbuf(20,ii)  = dlat8km        ! j-index on 8km bufr obs grid
        rdiagbuf(21,ii)  = dlon8km        ! i-index on 8km bufr obs grid

!        print*,'data(ilat,i)=',data(ilat,i),'data(ilon,i)=',data(ilon,i)

        rdiagbuf(22,ii) = FEDMdiag(i)                ! dBZ from rain water

        rdiagbuf(23,ii) = T1D                  ! temperature (sensible, K)
        rdiagbuf(24,ii) = RHO                  ! air density (kg/m**3) 

        if (lobsdiagsave) then
            write(6,*)'wrong here, stop in setupfed.f90 '
            stop
           ioff=nreal
           do jj=1,miter
              ioff=ioff+1
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              end if
           end do
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           end do
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           end do
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           end do
        end if

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '    fed'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id          )
           call nc_diag_metadata("Observation_Class",       obsclass            )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)         )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)      )
           call nc_diag_metadata("Latitude",                sngl(data(ilate,i)) )
           call nc_diag_metadata("Longitude",               sngl(data(ilone,i)) )
           call nc_diag_metadata("Station_Elevation",       sngl(data(istnelv,i)) )
           call nc_diag_metadata("Pressure",                sngl(presq)         )
           call nc_diag_metadata("Height",                  sngl(data(iobshgt,i)) )
           call nc_diag_metadata("Time",                    sngl(dtime*r60-time_offset))
           call nc_diag_metadata("Prep_QC_Mark",            sngl(data(iqc,i))   )
           call nc_diag_metadata("Prep_Use_Flag",           sngl(data(iuse,i))  )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb   !          )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(rwgt)          )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)          )
           end if

           call nc_diag_metadata("Errinv_Input",            sngl(errinv_input)  )
           call nc_diag_metadata("Errinv_Adjust",           sngl(errinv_adjst)  )
           call nc_diag_metadata("Errinv_Final",            sngl(errinv_final)  )

           call nc_diag_metadata("Observation",             sngl(data(ifedob,i)) )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)   )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(data(ifedob,i)-FEDMdiag(i)) )
           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 end if
              end do

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse              )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen"  , odiag%obssen   )
           end if

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_q )) deallocate(ges_q )
!   if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_qg)) deallocate(ges_qg)
    if(allocated(ges_fed)) deallocate(ges_fed)
  end subroutine final_vars_

  subroutine init_qcld(t_cld, qxmin_cld, icat_cld, t_dpnd) 
      use kinds, only: r_kind,r_single,r_double,i_kind
      implicit none
      real(r_kind),     intent(in   )   :: t_cld
      real(r_kind),     intent(inout)   :: qxmin_cld
      integer,          intent(in   )   :: icat_cld
      logical,          intent(in   )   :: t_dpnd
!
!     local variables
      real  :: tr_ll, qrmin_ll, tr_hl, qrmin_hl
      real  :: ts_ll, qsmin_ll, ts_hl, qsmin_hl
      real  :: tg_ll, qgmin_ll, tg_hl, qgmin_hl
      real  :: qr_min, qs_min, qg_min
!------------------------------------------------------

      qr_min = 5.0E-6_r_kind
      qs_min = 5.0E-6_r_kind
      qg_min = 5.0E-6_r_kind
      tr_ll = 275.65; qrmin_ll = 5.0E-6_r_kind; 
      tr_hl = 270.65; qrmin_hl = 1.0E-8_r_kind;
      ts_ll = 275.65; qsmin_ll = 1.0E-8_r_kind; 
      ts_hl = 270.65; qsmin_hl = 5.0E-6_r_kind;
      tg_ll = 275.65; qgmin_ll = 1.0E-6_r_kind; 
      tg_hl = 270.65; qgmin_hl = 5.0E-6_r_kind;
      
      select case (icat_cld)
      case (1)
          if ( t_dpnd ) then
              if (t_cld <= tr_hl) then
                  qxmin_cld = qrmin_hl
              else if (t_cld >= tr_ll) then
                  qxmin_cld = qrmin_ll
              else
                  qxmin_cld = (qrmin_hl + qrmin_ll) * 0.5
              end if
          else
              qxmin_cld = qr_min
          end if
      case default
          write(6,*) 'wrong cloud hydrometer category ID',icat_cld
      end select

      return

  end subroutine init_qcld

end subroutine setupfed
end module fed_setup
