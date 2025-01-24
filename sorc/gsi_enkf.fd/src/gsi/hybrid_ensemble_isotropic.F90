#define LATER
module hybrid_ensemble_isotropic
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_isotropic
!   prgmmr: parrish          org: np22                date: 2009-09-28
!
! abstract: contains routines for localization of the hybrid ensemble
!            control variable a_en.  this application is for 
!            localization with a regional model with homogeneous scale in horizontal
!            and vertical scale a function of vertical only.
!
! program history log:
!   2009-09-28  parrish, initial documentation.
!   2010-02-26  parrish, remove redundant special spectral and sub2grid/grid2sub code.  replaced
!                 by general purpose code as part of adding dual resolution option.
!   2010-03-17  zhu  - use vlevs from gridmod
!   2010-04-06  parrish - fix dimension error in ensemble_forward_model_ad_dual_res and
!                           add array deallocation in ensemble_forward_model_ad_dual_res and
!                            ensemble_forward_model_dual_res
!   2010-05-14  parrish - repair code to get dual resolution option working again.  remove
!                           error stops when dual resolution invoked.
!   2010-05-20  todling - renamed all cstate to bundle to avoid confusion; the
!                         bundles here are not necessarily idendical to the control vector,
!                         but rather what this module take the ensemble to be composed of
!   2010-07-29  kleist  - combine hybrid_ensemble_isotropic global/regional modules 
!   2011-02-28  parrish - introduce more complete use of gsi_bundlemod to eliminate hard-wired variables
!   2011-06-28  parrish - add code to allow sqrt of localization correlation so hybrid ensemble option
!                            can be extended to sqrt(B) minimization option.
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
!   2011-10-26  kleist - add time dimension to en_perts for 4d extensions
!   2011-11-01  kleist - 4d-ensemble-var available as part of ensemble forward models(s) and adjoints,
!                        as described in Liu et al. (2008), MWR, vol.36 and Buehner et al. (2010), 
!                        MWR, vol.138.  This specific implementation uses the linearized observation 
!                        operators as in Buehner et al., but also has the capability to allow for a 
!                        contribution from a static, time-invariant (3DVAR) B.  Capability also exists
!                        for actual hybrid Ens-4DVAR (with TL/AD).
!   2012-01-17  wu      - option "pwgtflg": psfc with vertically integrated contribution 
!                          in forward and adjoint routines
!   2012-02-08  parrish - add changes to allow regional dual res 
!   2012-02-08  parrish - cleanup
!   2012-10-11  wu      - dual resolution for regional hybens options; 
!                         use ensemble dimensions on control variable: alpha
!   2013-04-17  wu      - bug fix in normalizing the recursive filter
!   2014-05-22  wu      - increase dimension of variables used in the recursive filter 
!                         for vertically varying ability
!   2014-12-02  derber  - many optimization changes
!   2015-04-07  carley  - bug fix to allow grd_loc%nlat=grd_loc%nlon
!   2016-05-13  parrish - remove beta12mult
!   2018-02-15  wu      - add code for fv3_regional option
!   2022-09-15  yokota  - add scale/variable/time-dependent localization
!   2024-02-20  yokota  - add MGBF-based localization
!
! subroutines included:
!   sub init_rf_z                         - initialize localization recursive filter (z direction)
!   sub init_rf_x                         - initialize localization recursive filter (x direction)
!   sub init_rf_y                         - initialize localization recursive filter (y direction)
!   sub new_factorization_rf_z            - localization recursive filter (z direction)
!   sub new_factorization_rf_x            - localization recursive filter (x direction)
!   sub new_factorization_rf_y            - localization recursive filter (y direction)
!   sub normal_new_factorization_rf_z     - normalize localization recursive filter (z direction)
!   sub normal_new_factorization_rf_x     - normalize localization recursive filter (x direction)
!   sub normal_new_factorization_rf_y     - normalize localization recursive filter (y direction)
!   sub create_ensemble                   - allocate space for ensemble perturbations
!   sub load_ensemble                     - read/generate ensemble perturbations
!   sub rescale_ensemble_rh_perturbations - for internal generated perturbations only, normalize by ges q
!   sub ensemble_forward_model            - add ensemble contribution to analysis increment
!   sub ensemble_forward_model_ad         - adjoint of ensemble_forward_model
!   sub ensemble_forward_model_dual_res   - dual resolution version of ensemble_forward_model
!   sub ensemble_forward_model_ad_dual_res- adjoint of ensemble_forward_model_dual_res
!   get_new_alpha_beta - 
!   bkerror_a_en - 
!   ckgcov_a_en_new_factorization - 
!   ckgcov_a_en_new_factorization_ad - 
!   hybens_grid_setup - 
!   hybens_localization_setup - 
!   convert_km_to_grid_units - 
!   get_region_lat_lon_ens - 
!   get_region_dx_dy_ens - 
!   get_regional_dual_res_grid - 
!   acceptable_for_essl_fft - 
!
! Variable Definitions:
!   def yyyy      - what yyyy is
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use mpimod, only: mype
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundleset
  use gsi_bundlemod, only: gsi_bundleunset
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate

  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use string_utility, only: StrUpCase

! For MGBF
  use mg_intstate
  use mg_timers

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rf_z
  public :: init_rf_x
  public :: init_rf_y
  public :: new_factorization_rf_z
  public :: new_factorization_rf_x
  public :: new_factorization_rf_y
  public :: normal_new_factorization_rf_z
  public :: normal_new_factorization_rf_x
  public :: normal_new_factorization_rf_y
  public :: create_ensemble
  public :: load_ensemble
  public :: destroy_ensemble
  public :: rescale_ensemble_rh_perturbations
  public :: ensemble_forward_model
  public :: ensemble_forward_model_dual_res
  public :: ensemble_forward_model_ad
  public :: ensemble_forward_model_ad_dual_res
  public :: sqrt_beta_s_mult
  public :: sqrt_beta_e_mult
  public :: init_sf_xy
  public :: sf_xy
  public :: sqrt_sf_xy
  public :: sqrt_sf_xy_ad
  public :: get_new_alpha_beta
  public :: bkerror_a_en
  public :: ckgcov_a_en_new_factorization
  public :: ckgcov_a_en_new_factorization_ad
  public :: hybens_grid_setup
  public :: hybens_localization_setup
  public :: convert_km_to_grid_units
  public :: get_region_lat_lon_ens
  public :: get_region_dx_dy_ens
  public :: get_regional_dual_res_grid
  public :: acceptable_for_essl_fft

  interface sqrt_beta_s_mult
    module procedure sqrt_beta_s_mult_cvec
    module procedure sqrt_beta_s_mult_bundle
  end interface

  interface sqrt_beta_e_mult
    module procedure sqrt_beta_e_mult_cvec
    module procedure sqrt_beta_e_mult_bundle
  end interface

! set passed variables to public

  character(len=*),parameter::myname='hybrid_ensemble_isotropic'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      following variables are filter parameters for isotropic
!       homogeneous localization of hybrid control variable a_en

  real(r_kind),allocatable:: fmatz(:,:,:,:,:)
  real(r_kind),allocatable:: fmat0z(:,:,:,:)
  real(r_kind),allocatable:: fmatx(:,:,:,:,:,:)
  real(r_kind),allocatable:: fmat0x(:,:,:,:,:)
  real(r_kind),allocatable:: fmaty(:,:,:,:,:)
  real(r_kind),allocatable:: fmat0y(:,:,:,:)
  real(r_kind),allocatable:: znorm_new(:,:,:)
  real(r_kind),allocatable:: xnorm_new(:,:,:,:)
  real(r_kind),allocatable:: ynorm_new(:,:,:)
  real(r_kind),allocatable:: psbar(:)

! Other local variables for horizontal/spectral localization
  real(r_kind),allocatable,dimension(:,:,:)  :: spectral_filter,sqrt_spectral_filter
  integer(i_kind),allocatable,dimension(:) :: k_index

  integer(r_kind) :: nval_loc_en

! For MGBF
  type (mg_intstate_type), allocatable, dimension(:) :: obj_mgbf
  real(r_kind), allocatable, dimension(:,:,:) :: work_mgbf

!    following is for special subdomain to slab variables used when internally generating ensemble members

  integer(i_kind) nval2f,nscl
  integer(i_kind) nh_0,nh_1,nv_0,nv_1
  integer(i_kind),allocatable,dimension(:):: nsend_sd2h,ndsend_sd2h,nrecv_sd2h,ndrecv_sd2h
  integer(i_kind),allocatable,dimension(:):: i_recv,k_recv

  logical,parameter:: debug=.false.

contains

subroutine init_rf_z(z_len)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_z    initialize vertical recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize vertical recursive filter for hybrid ensemble control variable a_en
!            call this one first, then init_rf_x, init_rf_y
!
! program history log:
!   2009-12-16  parrish
!   2010-04-06  parrish - add 2nd option for units of vertical localization:
!                             if z_len < 0, then abs(z_len) is vertical localization length scale in
!                             units of ln(p).  otherwise, when z_len > 0, localization is in vertical
!                             grid units.  The ln(p) distance measurement is approximate, based on a
!                             fixed surface pressure of 1000mb.  This is because at the point where this
!                             is currently called, the background 3d pressure field is not yet available.
!                             A later version will correct this.
!                             For the current s_ens_v > 0, the measure is vertical grid units.  
!                             s_ens_v = 20 and s_ens_v = -0.44 are roughly comparable, and
!                             connection of .44 is .44 = (sqrt(.15)/sqrt(2))*1.6, where 1.6 is the value used
!                             by Jeff Whitaker for his distance in which the Gaspari-Cohn function 1st = 0.
!   2011-07-19  tong    - add the calculation of pressure vertical profile for regional model,
!                             when vertical localization length scale is in units of ln(p)
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS
!                             core
!
!   input argument list:
!     z_len    - filter length scale in grid units
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use gridmod, only: nsig,ak5,bk5,eta1_ll,eta2_ll,pt_ll,pdtop_ll,twodvar_regional, &
                     wrf_nmm_regional,nems_nmmb_regional,wrf_mass_regional,cmaq_regional, &
                     regional,fv3_regional
  use constants, only: half,one,rd_over_cp,zero,one_tenth,ten,two
  use hybrid_ensemble_parameters, only: grd_ens
  use hybrid_ensemble_parameters, only: ps_bar
  use hybrid_ensemble_parameters, only: naensloc

  implicit none

  real(r_kind)   ,intent(in) :: z_len(grd_ens%nsig,naensloc)

  integer(i_kind) k,nxy,i,ii,jj,j,l,ig
  real(r_kind) aspect(nsig),p_interface(nsig+1),ln_p_int(nsig+1)
  real(r_kind) dlnp,kap1,kapr,d1,rnsig
  real(r_kind),dimension(:,:,:),allocatable:: fmatz_tmp
  real(r_kind),dimension(:,:),allocatable:: fmat0z_tmp

  kap1=rd_over_cp+one
  kapr=one/rd_over_cp
  nxy=grd_ens%latlon11
  rnsig=real(nsig,r_kind)

!    use new factorization:

  if(.not.allocated(fmatz))  allocate(fmatz(nxy,2,nsig,2,naensloc))
  if(.not.allocated(fmat0z)) allocate(fmat0z(nxy,nsig,2,naensloc))
  allocate(fmatz_tmp(2,nsig,2),fmat0z_tmp(nsig,2))

  do ig=1,naensloc
!   for z_len < zero, use abs val z_len and assume localization scale is in units of ln(p)
  if(maxval(z_len(:,ig)) > zero) then

!  z_len is in grid units
     do k=1,nsig
        aspect(k)=z_len(k,ig)**2
     enddo

     do i=1,nxy
       call get_new_alpha_beta(aspect,nsig,fmatz_tmp,fmat0z_tmp)
       do l=1,2
         do k=1,nsig
           do j=1,2
             fmatz(i,j,k,l,ig)=fmatz_tmp(j,k,l)
           enddo
         enddo
       enddo
       do l=1,2
         do k=1,nsig
           fmat0z(i,k,l,ig)=fmat0z_tmp(k,l)
         enddo
       enddo
     enddo

  else

!  abs(z_len) is in units of ln(p)
!              put in approximate vertical scale which depends on ln(p)
!              use ensemble mean ps (ps_bar) to construct pressure
!              profiles using GFS sigma-p coordinate [ add mods to be able
!              to use fully generalized coordinate later** ]
!
     i=0
     do jj=1,grd_ens%lon2
        do ii=1,grd_ens%lat2
           i=i+1

           if(regional)then

              do k=1,nsig+1
                 if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional) then
                    p_interface(k)= one_tenth* &
                          (eta1_ll(k)*pdtop_ll + &
                           eta2_ll(k)*(ten*ps_bar(ii,jj,1)-pdtop_ll-pt_ll) + &
                           pt_ll)
                 endif
                 if (fv3_regional) then
                    p_interface(k)=eta1_ll(k)+ eta2_ll(k)*ps_bar(ii,jj,1)
                 endif
                 if (twodvar_regional) then
                    p_interface(k)=one_tenth*(eta1_ll(k)*(ten*ps_bar(ii,jj,1)-pt_ll)+pt_ll)
                 endif
                 if (wrf_mass_regional) then
                    p_interface(k)=one_tenth*(eta1_ll(k)*(ten*ps_bar(ii,jj,1)-pt_ll)+&
                                              eta2_ll(k) + pt_ll)
                 endif
                 ln_p_int(k)=log(max(p_interface(k),0.0001_r_kind))
              enddo

           else
              do k=1,nsig+1
                 p_interface(k)=ak5(k)+(bk5(k)*ps_bar(ii,jj,1))
                 ln_p_int(k)=log(max(p_interface(k),0.0001_r_kind))
              enddo
           endif

           do k=1,nsig
              dlnp=abs(ln_p_int(k)-ln_p_int(k+1))
              d1=abs(z_len(k,ig))/dlnp
              d1=min(rnsig,d1)
              aspect(k)=d1**2
!!            if(mype == 0) write(400,'(" k, vertical localization in grid units for ln(p) scaling =",i4,f10.2,f10.2,f10.2)') &
!!                                         k,sqrt(aspect(k))
           enddo
   
           call get_new_alpha_beta(aspect,nsig,fmatz_tmp,fmat0z_tmp)
           do l=1,2
             do k=1,nsig
               do j=1,2
                 fmatz(i,j,k,l,ig)=fmatz_tmp(j,k,l)
               enddo
             enddo
           enddo
           do l=1,2
             do k=1,nsig
               fmat0z(i,k,l,ig)=fmat0z_tmp(k,l)
             enddo
           enddo
        enddo
     enddo
  end if
  enddo !ig loop
  deallocate(fmatz_tmp,fmat0z_tmp)
  return

end subroutine init_rf_z

subroutine init_rf_x(x_len,kl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_x    initialize x direction recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize longitude recursive filters for hybrid ensemble control variable a_en
!
! program history log:
!   2009-12-16  parrish
!   2014-05-22  wu  modification to allow vertically varying scales
!
!   input argument list:
!     x_len -- filter length scale in grid units
!     kl    -- z dimemsion of x_len
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use hybrid_ensemble_parameters, only: grd_loc
  use hybrid_ensemble_parameters, only: region_dx_ens,region_dy_ens
  use hybrid_ensemble_parameters, only: naensloc
  use constants, only: half

  implicit none

  real(r_kind),intent(in   ) :: x_len(kl,naensloc)
  integer(i_kind),intent(in   ) :: kl

  integer(i_kind) i,j,k,l,kk,ig
  real(r_kind) aspect(grd_loc%nlon)
  real(r_kind) fmatc(2,grd_loc%nlon,2),fmat0c(grd_loc%nlon,2)

!    use new factorization:
  if(allocated(fmatx)) deallocate(fmatx)
  if(allocated(fmat0x)) deallocate(fmat0x)
  allocate(fmatx(grd_loc%nlat,2,grd_loc%nlon,2,kl,naensloc),fmat0x(grd_loc%nlat,grd_loc%nlon,2,kl,naensloc))
  do ig=1,naensloc
     do k=1,kl
        do i=1,grd_loc%nlat
           do j=1,grd_loc%nlon
              ! only works for rotated lat-lon grids
              aspect(j)=(x_len(k,ig)*region_dy_ens(grd_loc%nlat/2,grd_loc%nlon/2)/region_dx_ens(i,j))**2
           enddo
           call get_new_alpha_beta(aspect,grd_loc%nlon,fmatc,fmat0c)
           do kk=1,2
              do j=1,grd_loc%nlon
                 do l=1,2
                    fmatx(i,l,j,kk,k,ig)=fmatc(l,j,kk)
                 enddo
                 fmat0x(i,j,kk,k,ig)=fmat0c(j,kk)
              enddo
           enddo
        enddo
     enddo
  enddo !ig loop
  return

end subroutine init_rf_x

subroutine init_rf_y(y_len,kl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rf_y    initialize y direction recursive filter
!   prgmmr: parrish          org: np22                date: 2009-12-16
!
! abstract: initialize latitude recursive filters for hybrid ensemble control variable a_en
!
! program history log:
!   2009-12-16  parrish
!   2014-05-22  wu  modification to allow vertically varying scales
!
!   input argument list:
!     y_len -- filter length scale in grid units
!     kl    -- z dimemsion of y_len
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use hybrid_ensemble_parameters, only: grd_loc
  use hybrid_ensemble_parameters, only: naensloc
  use constants, only: half

  implicit none

  real(r_kind),intent(in   ) :: y_len(kl,naensloc)
  integer(i_kind),intent(in   ) :: kl

  real(r_kind) aspect(grd_loc%nlat)
  integer(i_kind) i,k,ig

!    use new factorization:
  if(allocated(fmaty)) deallocate(fmaty)
  if(allocated(fmat0y)) deallocate(fmat0y)
  allocate(fmaty(2,grd_loc%nlat,2,kl,naensloc),fmat0y(grd_loc%nlat,2,kl,naensloc))
  do ig=1,naensloc
     do k=1,kl
        do i=1,grd_loc%nlat
           aspect(i)=y_len(k,ig)**2
        enddo
        call get_new_alpha_beta(aspect,grd_loc%nlat,fmaty(1,1,1,k,ig),fmat0y(1,1,k,ig))
     enddo
  enddo !ig loop
  return

end subroutine init_rf_y

subroutine new_factorization_rf_z(f,iadvance,iback,ig)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_z
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in z (vertical) dimension.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-04-14  kleist   modifications for ln(ps) localization option
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!     ig       - number for smoothing scales
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: grd_ens
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback,ig
  real(r_kind)   ,intent(inout) :: f(grd_ens%latlon11,grd_ens%nsig)

  integer(i_kind) i,k,l,nxy,nz

  nxy=grd_ens%latlon11 ; nz=grd_ens%nsig
  if(iadvance == 1) then
     do k=1,nz
        do i=1,nxy
           f(i,k)=znorm_new(i,k,ig)*f(i,k)
        enddo
     enddo
  end if
  do k=1,nz
     do l=1,min(2,k-1)
        do i=1,nxy
           f(i,k)=f(i,k)-fmatz(i,l,k,iadvance,ig)*f(i,k-l)
        enddo
     enddo
     do i=1,nxy
        f(i,k)=fmat0z(i,k,iadvance,ig)*f(i,k)
     enddo
  enddo
  do k=nz,1,-1
     do l=1,min(2,nz-k)
        do i=1,nxy
           f(i,k)=f(i,k)-fmatz(i,l,k+l,iback,ig)*f(i,k+l)
        enddo
     enddo
     do i=1,nxy
        f(i,k)=fmat0z(i,k,iback,ig)*f(i,k)
     enddo
  enddo
  if(iadvance == 2) then
     do k=1,nz
        do i=1,nxy
           f(i,k)=znorm_new(i,k,ig)*f(i,k)
        enddo
     enddo
  end if
  return

end subroutine new_factorization_rf_z

subroutine new_factorization_rf_x(f,iadvance,iback,nlevs,ig)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_x
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in x (longitude) direction.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  adjust for possibility that nlevs=0
!   2014-05-22  wu  modification to allow vertically varying scales
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!     nlevs    - number of vertical levels for smoothing
!     ig       - number for smoothing scales
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use hybrid_ensemble_parameters, only: grd_loc,vvlocal
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback,nlevs,ig
  real(r_kind)   ,intent(inout) :: f(grd_loc%nlat,grd_loc%nlon,max(nlevs,1))

  integer(i_kind) i,j,k,l,ny,nx,nz

  ny=grd_loc%nlat ; nx=grd_loc%nlon ; nz=nlevs

  if(vvlocal)then
!$omp parallel do schedule(static,1) private(k,j,i,l)
     do k=1,nz

        if(iadvance == 1) then
           do j=1,nx
              do i=1,ny
                 f(i,j,k)=xnorm_new(i,j,k,ig)*f(i,j,k)
              enddo
           enddo
        end if

        do j=1,nx
           do l=1,min(2,j-1)
              do i=1,ny
                 f(i,j,k)=f(i,j,k)-fmatx(i,l,j,iadvance,k,ig)*f(i,j-l,k)
              enddo
           enddo
           do i=1,ny
              f(i,j,k)=fmat0x(i,j,iadvance,k,ig)*f(i,j,k)
           enddo
        enddo

        do j=nx,1,-1
           do l=1,min(2,nx-j)
              do i=1,ny
                 f(i,j,k)=f(i,j,k)-fmatx(i,l,j+l,iback,k,ig)*f(i,j+l,k)
              enddo
           enddo
           do i=1,ny
              f(i,j,k)=fmat0x(i,j,iback,k,ig)*f(i,j,k)
           enddo
        enddo

        if(iadvance == 2) then
           do j=1,nx
              do i=1,ny
                 f(i,j,k)=xnorm_new(i,j,k,ig)*f(i,j,k)
              enddo
           enddo
        end if

     enddo
  else 
!$omp parallel do schedule(static,1) private(k,j,i,l)
     do k=1,nz

        if(iadvance == 1) then
           do j=1,nx
              do i=1,ny
                 f(i,j,k)=xnorm_new(i,j,1,ig)*f(i,j,k)
              enddo
           enddo
        end if

        do j=1,nx
           do l=1,min(2,j-1)
              do i=1,ny
                 f(i,j,k)=f(i,j,k)-fmatx(i,l,j,iadvance,1,ig)*f(i,j-l,k)
              enddo
           enddo
           do i=1,ny
              f(i,j,k)=fmat0x(i,j,iadvance,1,ig)*f(i,j,k)
           enddo
        enddo

        do j=nx,1,-1
           do l=1,min(2,nx-j)
              do i=1,ny
                 f(i,j,k)=f(i,j,k)-fmatx(i,l,j+l,iback,1,ig)*f(i,j+l,k)
              enddo
           enddo
           do i=1,ny
              f(i,j,k)=fmat0x(i,j,iback,1,ig)*f(i,j,k)
           enddo
        enddo

        if(iadvance == 2) then
           do j=1,nx
              do i=1,ny
                 f(i,j,k)=xnorm_new(i,j,1,ig)*f(i,j,k)
              enddo
           enddo
        end if

     enddo
  endif
  return
end subroutine new_factorization_rf_x

subroutine new_factorization_rf_y(f,iadvance,iback,nlevs,ig)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    new_factorization_rf_y
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  apply new factorization Purser 1-d high-order filter in y (latitude) direction.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  adjust for possibility that nlevs=0
!   2014-05-22  wu  modification to allow vertically varying localization scales in regional
!
!   input argument list:
!     f        - input field to be filtered
!     iadvance - =1  for forward operator, =2 for adjoint operator
!     iback    - =2  for forward operator, =1 for adjoint operator
!     nlevs    - number of vertical levels for smoothing
!     ig       - number for smoothing scales
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use hybrid_ensemble_parameters, only: grd_loc,vvlocal
                      !                  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,iback,nlevs,ig
  real(r_kind)   ,intent(inout) :: f(grd_loc%nlat,grd_loc%nlon,max(nlevs,1))

  integer(i_kind) i,j,k,l,nx,ny,nz

  nx=grd_loc%nlon ; ny=grd_loc%nlat ; nz=nlevs

  if(vvlocal)then
     do k=1,nz
        do j=1,nx

           if(iadvance == 1) then
              do i=1,ny
                 f(i,j,k)=ynorm_new(i,k,ig)*f(i,j,k)
              enddo
           end if

           do i=1,ny
              do l=1,min(2,i-1)
                 f(i,j,k)=f(i,j,k)-fmaty(l,i,iadvance,k,ig)*f(i-l,j,k)
              enddo
              f(i,j,k)=fmat0y(i,iadvance,k,ig)*f(i,j,k)
           enddo

           do i=ny,1,-1
              do l=1,min(2,ny-i)
                 f(i,j,k)=f(i,j,k)-fmaty(l,i+l,iback,k,ig)*f(i+l,j,k)
              enddo
              f(i,j,k)=fmat0y(i,iback,k,ig)*f(i,j,k)
           enddo

           if(iadvance == 2) then
              do i=1,ny
                 f(i,j,k)=ynorm_new(i,k,ig)*f(i,j,k)
              enddo
           end if

        enddo
     enddo
  else
     do k=1,nz
        do j=1,nx

           if(iadvance == 1) then
              do i=1,ny
                 f(i,j,k)=ynorm_new(i,1,ig)*f(i,j,k)
              enddo
           end if

           do i=1,ny
              do l=1,min(2,i-1)
                 f(i,j,k)=f(i,j,k)-fmaty(l,i,iadvance,1,ig)*f(i-l,j,k)
              enddo
              f(i,j,k)=fmat0y(i,iadvance,1,ig)*f(i,j,k)
           enddo

           do i=ny,1,-1
              do l=1,min(2,ny-i)
                 f(i,j,k)=f(i,j,k)-fmaty(l,i+l,iback,1,ig)*f(i+l,j,k)
              enddo
              f(i,j,k)=fmat0y(i,iback,1,ig)*f(i,j,k)
           enddo

           if(iadvance == 2) then
              do i=1,ny
                 f(i,j,k)=ynorm_new(i,1,ig)*f(i,j,k)
              enddo
           end if

        enddo
     enddo
  endif
  return
end subroutine new_factorization_rf_y

subroutine normal_new_factorization_rf_z
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_z get normalization factor in z direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in z (vertical dimension)
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-04-14  kleist   modifications for ln(p) localization
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens
  use hybrid_ensemble_parameters, only: naensgrp,naensloc
  use constants, only: zero,one
  implicit none

  integer(i_kind) k,iadvance,iback,nxy,ig
  real(r_kind) f(grd_ens%latlon11,grd_ens%nsig),diag(grd_ens%latlon11,grd_ens%nsig)

  if(allocated(znorm_new)) deallocate(znorm_new)
  allocate(znorm_new(grd_ens%latlon11,grd_ens%nsig,naensloc))

  nxy=grd_ens%latlon11

  znorm_new=one

  do ig=1,naensgrp
     do k=1,grd_ens%nsig
        f=zero
        f(:,k)=one

        iadvance=1 ; iback=2
        call new_factorization_rf_z(f,iadvance,iback,ig)
        iadvance=2 ; iback=1
        call new_factorization_rf_z(f,iadvance,iback,ig)
        
        diag(:,k)=sqrt(one/f(:,k))
     enddo

     do k=1,grd_ens%nsig
        znorm_new(:,k,ig)=diag(:,k)
     enddo
  enddo !ig loop

! Check result:
  if(debug)then
    do k=1,grd_ens%nsig
       f=zero
       f(:,k)=one

       iadvance=1 ; iback=2
       call new_factorization_rf_z(f,iadvance,iback,1)
       iadvance=2 ; iback=1
       call new_factorization_rf_z(f,iadvance,iback,1)

       diag(:,k)=sqrt(one/f(:,k))
    enddo

    write(6,*)'in normal_new_factorization_rf_z, min,max(diag)=',minval(diag),maxval(diag)
  end if
  return

end subroutine normal_new_factorization_rf_z

subroutine normal_new_factorization_rf_x
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_x get normalization factor in x direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in longitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-11  parrish  correct error that can lead to infinite loop, and introduce grd_ens%kend_alloc
!                         in dimension statements
!   2013-04-17  wu       use grd_loc instead of grd_ens when defining normalization factor
!   2014-05-22  wu  modification to allow vertically varying localization scales in regional
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_loc,vvlocal
  use hybrid_ensemble_parameters, only: naensgrp,naensloc
  use constants, only: zero,one

  implicit none

  integer(i_kind) i,j,k,iadvance,iback,kl,ig
  real(r_kind) f(grd_loc%nlat,grd_loc%nlon,grd_loc%kend_alloc+1-grd_loc%kbegin_loc)
  real(r_kind),allocatable:: diag(:,:,:)
!  real(r_kind) diag(grd_loc%nlat,grd_loc%nlon)

!                       possible to have kend_loc - kbegin_loc-1 for processors not involved
!                          which results in infinite loops

  if(grd_loc%kend_loc < grd_loc%kbegin_loc) return

  if(vvlocal)then
     kl=grd_loc%kend_alloc+1-grd_loc%kbegin_loc
  else
     kl=1
  endif
  if(allocated(xnorm_new)) deallocate(xnorm_new)
  allocate(xnorm_new(grd_loc%nlat,grd_loc%nlon,kl,naensloc))
  if(allocated(diag)) deallocate(diag)
  allocate(diag(grd_loc%nlat,grd_loc%nlon,kl))
  xnorm_new=one

  do ig=1,naensgrp
     do j=1,grd_loc%nlon
        f=zero
        do k=1,kl
           do i=1,grd_loc%nlat
              f(i,j,k)=one
           enddo
        enddo
        iadvance=1 ; iback=2
        call new_factorization_rf_x(f,iadvance,iback,kl,ig)
        iadvance=2 ; iback=1
        call new_factorization_rf_x(f,iadvance,iback,kl,ig)
        do k=1,kl
           do i=1,grd_loc%nlat
              diag(i,j,k)=sqrt(one/f(i,j,k))
           enddo
        enddo
     enddo
     do k=1,kl
        do j=1,grd_loc%nlon
           do i=1,grd_loc%nlat
              xnorm_new(i,j,k,ig)=diag(i,j,k)
           enddo
        enddo
     enddo
  enddo !ig loop
!           check accuracy of xnorm
  if(debug) then
     do j=1,grd_loc%nlon
        f=zero
        do k=1,kl
           do i=1,grd_loc%nlat
              f(i,j,k)=one
           enddo
        enddo
        iadvance=1 ; iback=2
        call new_factorization_rf_x(f,iadvance,iback,kl,1)
        iadvance=2 ; iback=1
        call new_factorization_rf_x(f,iadvance,iback,kl,1)
        do k=1,kl
           do i=1,grd_loc%nlat
              diag(i,j,k)=f(i,j,k)
           enddo
        enddo
     enddo
     write(6,*)' in normal_new_factorization_rf_x,min,max(diag)=',minval(diag),maxval(diag)
  endif
  return

end subroutine normal_new_factorization_rf_x

subroutine normal_new_factorization_rf_y
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normal_new_factorization_rf_y  get normalization factor in y direction
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in latitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2013-04-17  wu       use grd_loc instead of grd_ens when defining normalization factor
!   2014-05-22  wu  modification to allow vertically varying localization scales in regional
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_loc,vvlocal
  use hybrid_ensemble_parameters, only: naensgrp,naensloc
  use constants, only: zero,one
  implicit none

  integer(i_kind) i,k,lend,lcount,iadvance,iback,kl,loop,ll,iend,ig
!  real(r_kind) f(grd_loc%nlat,grd_loc%nlon*(grd_loc%kend_alloc+1-grd_loc%kbegin_loc)),diag(grd_loc%nlat)
  real(r_kind) f(grd_loc%nlat,grd_loc%nlon,grd_loc%kend_alloc+1-grd_loc%kbegin_loc)
  real(r_kind),allocatable:: diag(:,:)

!                       possible to have kend_loc - kbegin_loc-1 for processors not involved
!                          which results in infinite loops

  if(grd_loc%kend_loc < grd_loc%kbegin_loc) return

  if(vvlocal)then
     kl=grd_loc%kend_alloc+1-grd_loc%kbegin_loc
  else
     kl=1
  endif

  if(allocated(ynorm_new)) deallocate(ynorm_new)
  allocate(ynorm_new(grd_loc%nlat,kl,naensloc))

  if(allocated(diag)) deallocate(diag)
  allocate(diag(grd_loc%nlat,kl))

  ynorm_new=one

  if(grd_loc%nlat <= grd_loc%nlon)then
    lend=1
    iend=grd_loc%nlat 
  else
    lend=grd_loc%nlat/grd_loc%nlon + 1
    iend=grd_loc%nlon 
  endif
              
  do ig=1,naensgrp
     do loop=1,lend
        ll=(loop-1)*iend
        f=zero
        do k=1,kl
           do i=1,iend
              lcount=ll+i
              f(lcount,i,k)=one
              if(lcount == grd_loc%nlat) exit
           enddo
        enddo

        iadvance=1 ; iback=2
        call new_factorization_rf_y(f,iadvance,iback,kl,ig)
        iadvance=2 ; iback=1
        call new_factorization_rf_y(f,iadvance,iback,kl,ig)

        do k=1,kl
           do i=1,iend
              lcount=ll+i
              diag(lcount,k)=sqrt(one/f(lcount,i,k))
              ynorm_new(lcount,k,ig)=diag(lcount,k)
              if(lcount == grd_loc%nlat) exit
           enddo
        enddo
     enddo
  enddo !ig loop
!               check that ynorm is corect
  if(debug) then
     do loop=1,lend
        ll=(loop-1)*iend
        f=zero
        do k=1,kl
           do i=1,iend
              lcount=ll+i
              f(lcount,i,k)=one
              if(lcount ==  grd_loc%nlat) exit
           enddo
        enddo

        iadvance=1 ; iback=2
        call new_factorization_rf_y(f,iadvance,iback,kl,1)
        iadvance=2 ; iback=1
        call new_factorization_rf_y(f,iadvance,iback,kl,1) 

        do k=1,kl
           do i=1,iend
              lcount=ll+i
              diag(lcount,k)=sqrt(one/f(lcount,i,k))
              if(lcount ==  grd_loc%nlat) exit
           enddo
        enddo
    enddo
    write(6,*)' in normal_new_factorization_rf_y, min,max(diag)=',minval(diag),maxval(diag)
  endif
  return
end subroutine normal_new_factorization_rf_y

  subroutine create_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_ensemble        allocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: allocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2011-02-28  parrish - introduce more complete use of gsi_bundlemod to eliminate hard-wired variables
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,ntlevs_ens
    use hybrid_ensemble_parameters, only: nelen,en_perts,ps_bar
    use hybrid_ensemble_parameters, only: ntotensgrp

    implicit none

    type(gsi_grid)  :: grid_ens

    integer(i_kind) n,istatus,m,ig
    character(len=*),parameter::myname_=trim(myname)//'*create_ensemble'

    nelen=grd_ens%latlon11*(max(0,nc3d)*grd_ens%nsig+max(0,nc2d))
!   create ensemble perturbations bundles (using newly added r_single capability

    allocate(en_perts(n_ens,ntotensgrp,ntlevs_ens))
    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
 
    do m=1,ntlevs_ens
       do ig=1,ntotensgrp
          do n=1,n_ens
             call gsi_bundlecreate(en_perts(n,ig,m),grid_ens,'ensemble perts',istatus, &
                                   names2d=cvars2d,names3d=cvars3d,bundle_kind=r_single)
             if(istatus/=0) then
                write(6,*)trim(myname_),': trouble creating en_perts bundle'
                call stop2(999)
             endif
          enddo
       enddo
    enddo


    allocate(ps_bar(grd_ens%lat2,grd_ens%lon2,ntlevs_ens) )
    if(debug) then
       write(6,*)' in create_ensemble, grd_ens%latlon11,grd_ens%latlon1n,n_ens,ntotensgrp,ntlevs_ens=', &
                                 grd_ens%latlon11,grd_ens%latlon1n,n_ens,ntotensgrp,ntlevs_ens
       write(6,*)' in create_ensemble, total bytes allocated=',4*nelen*n_ens*ntotensgrp*ntlevs_ens
    end if
    return

  end subroutine create_ensemble

  subroutine load_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    load_ensemble    read/generate ensemble perturbations
!   prgmmr: parrish          org: np22                date: 2009-09-11
!
! abstract: read or generate (if generate_ens=.true.) ensemble
!             perturbations used for hybrid ensemble option.
!
! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-15  zhu      make changes using cstate
!   2010-03-28  todling  update to use gsi_bundle
!   2010-06-03  parrish  multiple fixes for dual resolution
!   2011-02-28  parrish - introduce more complete use of gsi_bundlemod to eliminate hard-wired variables
!   2011-12-01  todling/el akkraoui - add ability to write out internally generated ens perturbations
!   2011-07-05  carley  - add call to get_nmmb_ensperts
!   2011-12-07  tong    - add the option to read wrf_nmm ensemble
!   2012-01-30  parrish - remove wrf_nmm_regional,wrf_mass_regional,netcdf,nems_nmmb_regional
!   2013-10-25  todling - nullify work pointer
!   2015-01-22  Hu      - add namelist (i_en_perts_io) and functions to save and 
!                         read ensemble perturbations in ensemble grid.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use gridmod, only: regional
    use constants, only: zero,one
    use hybrid_ensemble_parameters, only: n_ens,generate_ens,grd_ens,grd_anl,ntlevs_ens, &
                                          pseudo_hybens,regional_ensemble_option,&
                                          i_en_perts_io
    use hybrid_ensemble_parameters, only: nelen,en_perts,ps_bar
    use hybrid_ensemble_parameters, only: l_both_fv3sar_gfs_ens 
    use gsi_enscouplermod, only: gsi_enscoupler_put_gsi_ens
    use mpimod, only: mype
    use get_pseudo_ensperts_mod, only: get_pseudo_ensperts_class
    use get_wrf_mass_ensperts_mod, only: get_wrf_mass_ensperts_class
    use get_fv3_regional_ensperts_mod, only: get_fv3_regional_ensperts_class
    use get_wrf_nmm_ensperts_mod, only: get_wrf_nmm_ensperts_class
    use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens
    use mpimod, only: mpi_comm_world

    implicit none

   type(get_pseudo_ensperts_class) :: pseudo_enspert
   type(get_wrf_mass_ensperts_class) :: wrf_mass_enspert
   type(get_wrf_nmm_ensperts_class) :: wrf_nmm_enspert
   type(get_fv3_regional_ensperts_class) :: fv3_regional_enspert
    type(gsi_bundle),allocatable:: en_bar(:)
    type(gsi_bundle):: bundle_anl,bundle_ens
    type(gsi_grid)  :: grid_anl,grid_ens
    integer(i_kind) i,j,n,ii,m
    integer(i_kind) istatus
    real(r_kind),allocatable:: seed(:,:)
    real(r_kind),pointer,dimension(:,:)   :: cv_ps=>NULL()
    real(r_kind) sig_norm,bar_norm
    character(len=*),parameter::myname_=trim(myname)//'*load_ensemble'

!      create simple regular grid

    if(generate_ens) then


       call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
       call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

       allocate(en_bar(ntlevs_ens))

       do m=1,ntlevs_ens
          call gsi_bundlecreate(en_bar(m),grid_ens,'ensemble',istatus, &
                                names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       enddo
       if(istatus/=0) then
          write(6,*)trim(myname_),': trouble creating en_bar bundle'
          call stop2(999)
       endif
       sig_norm=sqrt(one/max(one,n_ens-one))
       bar_norm=one/n_ens
       if(n_ens == 1) bar_norm=zero

!                        initialize subdomain to slab routine special_sd2h
       call special_sd2h0
       allocate(seed(nval2f,nscl))
       seed=-one
 
       do m=1,ntlevs_ens
         en_bar(m)%values=zero
       enddo

!      create two internal bundles, one on analysis grid and one on ensemble grid

       call gsi_bundlecreate (bundle_anl,grid_anl,'ensemble work',istatus, &
                                 names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       if(istatus/=0) then
          write(6,*)trim(myname_),': trouble creating bundle_anl bundle'
          call stop2(999)
       endif
       call gsi_bundlecreate (bundle_ens,grid_ens,'ensemble work ens',istatus, &
                                 names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       if(istatus/=0) then
          write(6,*)trim(myname_),': trouble creating bundle_ens bundle'
          call stop2(999)
       endif

       do m=1,ntlevs_ens
          do n=1,n_ens
             call generate_one_ensemble_perturbation(bundle_anl,bundle_ens,seed)
             do ii=1,nelen
                en_perts(n,1,m)%valuesr4(ii)=bundle_ens%values(ii)
                en_bar(m)%values(ii)=en_bar(m)%values(ii)+bundle_ens%values(ii)
             enddo
          enddo
       
! Load ps_bar for use with vertical localization later
          call gsi_bundlegetpointer (en_bar(m),'ps' ,cv_ps ,istatus)
          do j=1,grd_ens%lon2
             do i=1,grd_ens%lat2
                ps_bar(i,j,m)=cv_ps(i,j)*bar_norm
             enddo
          enddo
       enddo

! do some cleanning
       call gsi_bundledestroy(bundle_anl,istatus)
       if(istatus/=0) then
          write(6,*)trim(myname_),': trouble destroying bundle_anl bundle'
          call stop2(999)
       endif
       call gsi_bundledestroy(bundle_ens,istatus)
       if(istatus/=0) then
          write(6,*)trim(myname_),': trouble destroying bundle_ens bundle'
          call stop2(999)
       endif
!                          remove mean, which is locally significantly non-zero, due to sample size.
!                           with real ensembles, the mean of the actual sample will be removed.

       do m=1,ntlevs_ens
          do n=1,n_ens
             do ii=1,nelen
                en_perts(n,1,m)%valuesr4(ii)=(en_perts(n,1,m)%valuesr4(ii)-en_bar(m)%values(ii)*bar_norm)*sig_norm
             enddo
             call gsi_enscoupler_put_gsi_ens(grd_ens,n,m,en_perts(n,1,m),istatus)
             if(istatus/=0) then
                 write(6,*)trim(myname_),': trouble writing perts'
                 call stop2(999)
             endif
          enddo

          call gsi_bundledestroy(en_bar(m),istatus)
          if(istatus/=0) then
          write(6,*)trim(myname_),': trouble destroying en_bar bundle'
          call stop2(999)
         end if
       enddo

       deallocate(en_bar)
       deallocate(seed)

    else

!            read in ensembles
       if (.not.regional) then

          call get_gefs_ensperts_dualres

       else

          if(regional_ensemble_option < 1 .or. regional_ensemble_option > 5) then
             if(mype==0) then
                write(6,'(" IMPROPER CHOICE FOR ENSEMBLE INPUT IN SUBROUTINE LOAD_ENSEMBLE")')
                write(6,'(" regional_ensemble_option = ",i5)') regional_ensemble_option
                write(6,'(" allowed values of regional_ensemble_option:")')
                write(6,'("   regional_ensemble_option=1:")')
                write(6,'("                    use GEFS internally interpolated to ensemble grid.")')
                write(6,'("                    can add pseudo ensemble hybrid option for hwrf.")')
                write(6,'("   regional_ensemble_option=2:")')
                write(6,'("                    ensembles are WRF NMM (HWRF) format.")')
                write(6,'("   regional_ensemble_option=3:")')
                write(6,'("                    ensembles are ARW netcdf format.")')
                write(6,'("   regional_ensemble_option=4:")')
                write(6,'("                    ensembles are NEMS NMMB format.")')
             end if
             call stop2(999)
          end if
          select case(regional_ensemble_option)

             case(1)

!     regional_ensemble_option = 1: use GEFS internally interpolated to ensemble grid.

                if(i_en_perts_io==2) then ! get en_perts from save files
                   call en_perts_get_from_save
                elseif(i_en_perts_io==3) then ! get en_perts from save files
                   call en_perts_get_from_save_fulldomain
                else
                   call get_gefs_for_regional
                endif

!     pseudo_hybens = .true.: pseudo ensemble hybrid option for hwrf
!                             GEFS ensemble perturbations in TC vortex area
!                             are replaced with TC vortex library perturbations
                if (pseudo_hybens) then
                   call pseudo_enspert%get_pseudo_ensperts(en_perts,nelen)
                end if
             case(2)

!     regional_ensemble_option = 2: ensembles are WRF NMM (HWRF) format

                call wrf_nmm_enspert%get_wrf_nmm_ensperts(en_perts,nelen,region_lat_ens,region_lon_ens,ps_bar)

             case(3)

!     regional_ensemble_option = 3: ensembles are ARW netcdf format.

                call wrf_mass_enspert%get_wrf_mass_ensperts(en_perts,nelen,ps_bar)

             case(4)

!     regional_ensemble_option = 4: ensembles are NEMS NMMB format.

                call get_nmmb_ensperts
             case(5)
                if (l_both_fv3sar_gfs_ens) then ! first read in gfs ensembles for regional 
                   call get_gefs_for_regional
                endif
    
!     regional_ensemble_option = 5: ensembles are fv3 regional.
                call fv3_regional_enspert%get_fv3_regional_ensperts(en_perts,nelen,ps_bar)
   

          end select

       end if

    end if
    return

  end subroutine load_ensemble

  subroutine generate_one_ensemble_perturbation(bundle_anl,bundle_ens,seed)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    generate_one_ensemble_perturbation
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract:  compute normalization factor in latitude direction
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-14  zhu     - make changes using cstate
!   2010-03-15  derber  - fix qvar3d for ensemble
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-18  parrish  reactivate dual resolution
!   2010-06-04  parrish  multiple fixes for dual resolution
!   2011-02-28  parrish - introduce more complete use of gsi_bundlemod to eliminate hard-wired variables
!   2011-12-01  todling - bug fix: lapack routine(s) s/dlarnv had incorrect interface
!   2011-09-27  parrish - ckgcov is called here, which uses sqrt_smootrf.  The variable nval_lenz is passed
!                         to sqrt_smoothrf through module jfunc, but if lsqrtb=F, then nval_lenz=0.
!                         To fix this, nval_lenz is passed in to this subroutine through module jfunc,
!                         then is temporarily replaced with the proper non-zero value, and restored back
!                         to its original value after exiting from the call to ckgcov.  Also, this is
!                         now an argument in ckgcov, which was missing in this call to ckgcov.
!   2012-06-12  parrish - remove variable nvar_pe (not used)
!   2013-01-12  parrish - remove extra argument nnnn1o from call ckgcov--no longer used
!   2013-10-25  todling - nullify work pointers
!
!   input argument list:
!     seed     - old random number seeds (used for bit reproducibility of
!                 generated random ensemble perturbations on different 
!                 numbers of processors)
!
!   output argument list:
!     seed     - new random number seeds
!     st       - stream function part of generated ensemble perturbation
!     vp       - velocity potential part of generated ensemble perturbation
!     t        - virtual temperature part of generated ensemble perturbation
!     rh       - relative humidity part of generated ensemble perturbation
!     oz       - ozone part of generated ensemble perturbation
!     cw       - cloud water part of generated ensemble perturbation
!     p        - surface pressure part of generated ensemble perturbation
!     sst      - skin temperature part of generated ensemble perturbation
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: r_kind,i_kind,i_llong
    use gridmod, only: vlevs,nnnn1o,regional
    use mpimod, only: mype,mpi_rtype,mpi_comm_world,ierror
    use hybrid_ensemble_parameters, only: uv_hyb_ens,grd_ens,grd_anl,p_e2a
    use general_sub2grid_mod, only: general_suba2sube
    use constants, only: zero,one
    use jfunc, only: nval_lenz
    implicit none

    character(len=*),parameter::myname_=myname//'*generate_one_ensemble_perturbation'
    real(r_kind)    ,intent(inout) :: seed(nval2f,nscl)
    type(gsi_bundle),intent(inout) :: bundle_anl,bundle_ens

    real(r_kind),dimension(nval2f,nnnn1o,nscl):: z
    real(r_kind) vert1(vlevs)
    integer(i_llong) iseed
    integer(4) iiseed(4) ! must be integer*4 given lapack interface
    integer(i_kind) nvert,i,is,naux,k,ic3
    integer(i_kind) istat_st,istat_vp
    integer(i_kind) nval_lenz_save
    real(r_kind),dimension(nh_0:nh_1,vlevs,nscl):: zsub
    real(r_kind),dimension(:,:,:),allocatable:: ua,va
    real(r_kind),pointer,dimension(:,:,:):: st=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: vp=>NULL()

    naux=0
    nvert=vlevs
    zsub=zero
    if(maxval(seed) <  zero) then

!       create initial seed for random numbers for each horizontal location.
  
       if(mype == 0) then
          call random_number(seed)
          do is=1,nscl
             do i=1,nval2f
                iseed=1+nint(seed(i,is)*1234567._r_kind)
                seed(i,is)=iseed
             enddo
          enddo
       end if
       call mpi_bcast(seed,nval2f*nscl,mpi_rtype,0,mpi_comm_world,ierror)

    end if

    do is=1,nscl
       do i=nh_0,nh_1
#ifdef ibm_sp
#ifdef _REAL4_
          call snrand(seed(i,is),nvert,vert1,aux,naux)
#else
          call dnrand(seed(i,is),nvert,vert1,aux,naux)
#endif
#else /* ibm_sp */
! Generate a Vector of Normally Distributed Random Numbers (function from Lapack lib)
          iiseed=seed(i,is)
          iiseed(4)=mod(iiseed(4),2)+1 ! must be odd
#ifdef _REAL4_
          call slarnv(3,iiseed,nvert,vert1)
#else
          call dlarnv(3,iiseed,nvert,vert1)
#endif
          seed(i,is)=iiseed(1) ! update seed; is this good enough?
#endif /* ibm_sp */
          do k=1,nvert
             zsub(i,k,is)=vert1(k)
          enddo
       enddo
    enddo
    call special_sd2h(zsub,z)

!     if this is a global run, then need to fix tropical belt part of z so periodic overlap is correct
    if(.not.regional) call fix_belt(z)

!     temporarily redefine nval_lenz
    nval_lenz_save=nval_lenz
    nval_lenz=nval2f*nnnn1o*nscl
    call ckgcov(z,bundle_anl,nval_lenz)
!     restore nval_lenz
    nval_lenz=nval_lenz_save

!     if uv_hyb_ens=.true., then convert st,vp to u,v
    if(uv_hyb_ens) then
       allocate(ua(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig))
       allocate(va(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig))
       istat_st=-999
       istat_vp=-999
       do ic3=1,nc3d
          if(trim(cvars3d(ic3))=='sf') call gsi_bundlegetpointer (bundle_anl, cvars3d(ic3),st, istat_st)
          if(trim(cvars3d(ic3))=='vp') call gsi_bundlegetpointer (bundle_anl, cvars3d(ic3),vp, istat_vp)
       enddo
       if(istat_st/=0.or.istat_vp/=0) then
          write(6,*) myname_,': error getting sf/vp pointers, aborting ...'
          call stop2(999)
       endif
       call getuv(ua,va,st,vp,0)
       st=ua
       vp=va
       deallocate(va)
       deallocate(ua)
    end if

    if(p_e2a%identity) then
       bundle_ens%values=bundle_anl%values
    else
       call general_suba2sube(grd_anl,grd_ens,p_e2a,bundle_anl%values,bundle_ens%values,regional)
    end if
    return


  end subroutine generate_one_ensemble_perturbation

  subroutine fix_belt(z)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    fix_belt
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-28
!
! abstract: when generating random vector representation of control variable z for
!            global case, need to make adjustment to tropical belt part to
!            properly account for periodicity in overlap zone
!
! program history log:
!   2009-09-28  parrish  initial documentation
!   2010-02-20  parrish  modifications for dual resolution
!
!   input argument list:
!     z        - field to be adjusted
!
!   output argument list:
!     z        - adjusted field
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


    use kinds, only: r_kind,i_kind
    use gridmod, only: nnnn1o
    use berror, only: nx,ny
    use hybrid_ensemble_parameters, only: grd_ens
    implicit none

    real(r_kind),intent(inout) :: z(nval2f,nnnn1o,nscl)

    real(r_kind) zloc1(ny,nx)
    integer(i_kind) i,ii,j,jj,k

!$omp parallel do schedule(static,1) private(j,k,i,jj,ii,zloc1)
    do j=1,nscl
       do k=1,nnnn1o
          i=0
          do jj=1,nx
             do ii=1,ny
                i=i+1
                zloc1(ii,jj)=z(i,k,j)
             enddo
          enddo
          do jj=grd_ens%nlon+1,nx
             do ii=1,ny
                zloc1(ii,jj)=zloc1(ii,jj-grd_ens%nlon)
             enddo
          enddo
          i=0
          do jj=1,nx
             do ii=1,ny
                i=i+1
                z(i,k,j)=zloc1(ii,jj)
             enddo
          enddo
       enddo
    enddo
    return
    
  end subroutine fix_belt

  subroutine rescale_ensemble_rh_perturbations
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rescale_ensemble_rh_perturbations
!   prgmmr: parrish          org: np22                date: 2009-10-15
!
! abstract: rescale internally generated ensemble rh perturbations
!
! program history log:
!   2009-10-15  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2011-02-28  parrish  bundle changes
!   2011-07-08  todling  adapt to fit upd interface to general_sub2grid
!   2013-10-25  todling  nullify work pointer
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: regional
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,grd_a1,grd_e1,p_e2a,ntlevs_ens
    use hybrid_ensemble_parameters, only: en_perts
    use hybrid_ensemble_parameters, only: ntotensgrp
    use general_sub2grid_mod, only: general_suba2sube
    use berror, only: qvar3d
    implicit none

    integer(i_kind) i,j,k,n,istatus,m,ig
    real(r_kind) qvar3d_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1)
    real(r_single),pointer,dimension(:,:,:):: w3=>NULL()

    call gsi_bundlegetpointer(en_perts(1,1,1),'q',w3,istatus)
    if(istatus/=0) then
       write(6,*)' rh variable not available, skip subroutine rescale_ensemble_rh_perturbations'
       return
    end if

    if(grd_anl%latlon11 == grd_ens%latlon11) then
       qvar3d_ens=reshape(qvar3d,(/size(qvar3d,1),size(qvar3d,2),size(qvar3d,3),1/))
    else
       call general_suba2sube(grd_a1,grd_e1,p_e2a, &
            reshape(qvar3d,(/size(qvar3d,1),size(qvar3d,2),size(qvar3d,3),1/)),qvar3d_ens,regional)
    end if
    do m=1,ntlevs_ens
       do ig=1,ntotensgrp
!$omp parallel do schedule(static,1) private(n,i,j,k,w3,istatus)
          do n=1,n_ens
             call gsi_bundlegetpointer(en_perts(n,ig,m),'q',w3,istatus)
             if(istatus/=0) then
                write(6,*)' error retrieving pointer to rh variable for ensemble number ',n
                call stop2(999)
             end if
             do k=1,grd_ens%nsig
                do j=1,grd_ens%lon2
                   do i=1,grd_ens%lat2
                      w3(i,j,k)=qvar3d_ens(i,j,k,1)*w3(i,j,k)
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo
    return
 
  end subroutine rescale_ensemble_rh_perturbations

  subroutine destroy_ensemble
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ensemble       deallocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: deallocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
!   2011-02-28  parrish, replace specific ensemble perturbation arrays with pseudo-bundle en_perts array
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: l_hyb_ens,n_ens,ntlevs_ens
    use hybrid_ensemble_parameters, only: en_perts,ps_bar
    use hybrid_ensemble_parameters, only: ntotensgrp
    use hybrid_ensemble_parameters, only: l_mgbf_loc
    implicit none

    integer(i_kind) istatus,n,m,ig

    if(l_hyb_ens) then
       do m=1,ntlevs_ens
          do ig=1,ntotensgrp
             do n=1,n_ens
                call gsi_bundleunset(en_perts(n,ig,m),istatus)
                if(istatus/=0) then
                   write(6,*)'in destroy_ensemble: trouble destroying en_perts bundle'
                   call stop2(999)
                endif
             enddo
          enddo
       enddo
       deallocate(ps_bar)
       deallocate(en_perts)
       if(l_mgbf_loc) call print_mg_timers("mgbf_timing_cpu.csv", print_cpu, mype)
    end if
    return

  end subroutine destroy_ensemble

  subroutine ensemble_forward_model(cvec,a_en,ibin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model  add ensemble part to anl vars
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: For the hybrid ensemble method, add ensemble contribution
!             to standard analysis control variables.  (This follows,
!             method outlined in Wang et al, MWR, 2008).

! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2010-03-23  zhu - use cstate
!   2010-05-07  parrish - remove error stop for dual resolution
!   2010-04-28  todling - update to use gsi_bundle
!   2011-02-28  parrish - bundle changes
!   2011-10-03  wu - add option to weight ensemble contribution to surface pressure with vertical profile
!   2011-10-26  kleist - 4d capability for ensemble/hybrid
!   2011-12-01  todling - explicit dim for a_en()
!
!   input argument list:
!     cvec      - 
!     a_en      - 
!     ibin      - integer bin number for ensemble perturbations  
! 
!   output argument list:
!     cvec      - 
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: n_ens,pwgtflg,pwgt
    use hybrid_ensemble_parameters, only: en_perts
    use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp
    use hybrid_ensemble_parameters, only: ensgrp2aensgrp
    use constants, only: zero

    implicit none
    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(in)    :: a_en(naensgrp,n_ens)
    integer,intent(in)             :: ibin

    character(len=*),parameter :: myname_=trim(myname)//'*ensemble_forward_model'
    logical :: nogood
    integer(i_kind) :: i,j,k,n,im,jm,km,ic2,ic3,ipic,ipx,km_tmp
    integer(i_kind) :: ipc3d(nc3d),ipc2d(nc2d),istatus,ig,iaens

    im=cvec%grid%im
    jm=cvec%grid%jm
    km=cvec%grid%km

!   Check resolution consistency between static and ensemble components
    nogood=im/=a_en(1,1)%grid%im.or.jm/=a_en(1,1)%grid%jm.or.km/=a_en(1,1)%grid%km
    if (nogood) then
       write(6,*) myname_,': static&ensemble vectors have inconsistent dims'
       call stop2(999)
    endif

!   Request ensemble-corresponding fields from control vector
!    NOTE:  because ensemble perturbation bundle structure is same as control vector, use same ipc3d and
!             ipc2d indices for cvec and en_perts bundles.
    call gsi_bundlegetpointer (cvec,cvars3d,ipc3d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 3d pointers'
      call stop2(999)
    endif
    call gsi_bundlegetpointer (cvec,cvars2d,ipc2d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 2d pointers'
      call stop2(999)
    endif
 
    ipx=1

!$omp parallel do schedule(static,1) private(j,n,ic3,k,i,ipic,ig,iaens)
    do k=1,km
       do ic3=1,nc3d
          ipic=ipc3d(ic3)
          do j=1,jm
             do i=1,im
                cvec%r3(ipic)%q(i,j,k)=zero
             enddo
          enddo
          do ig=1,ntotensgrp
             iaens=ensgrp2aensgrp(ig,ic3,ibin)
             if(iaens>0) then
                do n=1,n_ens
                   do j=1,jm
                      do i=1,im
                         cvec%r3(ipic)%q(i,j,k)=cvec%r3(ipic)%q(i,j,k) &
                              +a_en(iaens,n)%r3(ipx)%q(i,j,k)*en_perts(n,ig,ibin)%r3(ipic)%qr4(i,j,k)
                      enddo
                   enddo
                enddo
             endif ! iaens>0
          enddo
       enddo
    enddo

    do ic2=1,nc2d
       ipic=ipc2d(ic2)
       do j=1,jm
          do i=1,im
             cvec%r2(ipic)%q(i,j)=zero
          enddo
       enddo

       select case ( trim(StrUpCase(cvars2d(ic2))) )
 
          case('PS')
   
             if ( pwgtflg ) then
                km_tmp = km
             else
                km_tmp = 1
             endif

             do ig=1,ntotensgrp
                iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
                if(iaens>0) then
                   do n=1,n_ens
                      do k=1,km_tmp
                         do j=1,jm
                            do i=1,im
                               cvec%r2(ipic)%q(i,j)=cvec%r2(ipic)%q(i,j) &
                                    +a_en(iaens,n)%r3(ipx)%q(i,j,k)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)*pwgt(i,j,k)
                            enddo
                         enddo
                      enddo
                   enddo ! enddo n_ens
                endif ! iaens>0
             enddo

          case('SST')
 
             do ig=1,ntotensgrp
                iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
                if(iaens>0) then
                   do n=1,n_ens
                      do j=1,jm
                         do i=1,im
                            cvec%r2(ipic)%q(i,j)=cvec%r2(ipic)%q(i,j) &
                                 +a_en(iaens,n)%r3(ipx)%q(i,j,1)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)
                         enddo
                      enddo
                   enddo ! enddo n_ens
                endif ! iaens>0
             enddo
 
       end select

    enddo
    return

  end subroutine ensemble_forward_model

  subroutine ensemble_forward_model_dual_res(cvec,a_en,ibin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model_dual_res  use for dualres option
!   prgmmr: parrish          org: np22                date: 2010-02-20
!
! abstract: Copy of ensemble_forward_model for use with dual resolution.

! program history log:
!   2010-02-20  parrish
!   2010-03-23  zhu - use cstate
!   2010-04-06  parrish - add deallocate(sube_vars)
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-07  parrish - remove error stop for dual resolution, and add cstate again
!                         to analysis variables after interpolation from
!                         ensemble grid.
!                         the ensemble part is still not updated for
!                         generalized control variable
!   2010-05-18  todling - revisited bundle usage in light of Dave's change (2010-05-07)
!   2011-02-28  parrish - bundle changes
!   2011-10-03  wu - add option to weight ensemble contribution to surface pressure with vertical profile
!   2011-10-26  kleist  - 4d capability for ensemble/hybrid
!   2011-12-01  todling - explicit dim for a_en()
!
!   input argument list:
!     cvec      -
!     a_en      -
!     ibin      - integer bin number for ensemble perturbations
!
!   output argument list:
!     cvec      - 
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: n_ens,pwgtflg,pwgt
    use hybrid_ensemble_parameters, only: grd_ens,grd_anl,p_e2a
    use hybrid_ensemble_parameters, only: en_perts
    use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp
    use hybrid_ensemble_parameters, only: ensgrp2aensgrp
    use general_sub2grid_mod, only: general_sube2suba
    use gridmod,only: regional
    use constants, only: zero
    implicit none

    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(in)    :: a_en(naensgrp,n_ens)
    integer,intent(in)             :: ibin

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model_dual_res'
    type(gsi_grid)   :: grid_ens,grid_anl
    type(gsi_bundle) :: work_ens,work_anl
    integer(i_kind) :: i,j,k,n,im,jm,km,ic2,ic3,ipic,ipx,km_tmp
    integer(i_kind) :: ipc2d(nc2d),ipc3d(nc3d),istatus,ig,iaens

!   Request ensemble-corresponding fields from control vector
!    NOTE:  because ensemble perturbation bundle structure is same as control vector, use same ipc3d and
!             ipc2d indices for cvec and en_perts bundles.
    call gsi_bundlegetpointer (cvec,cvars3d,ipc3d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 3d pointers'
      call stop2(999)
    endif
    call gsi_bundlegetpointer (cvec,cvars2d,ipc2d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 2d pointers'
      call stop2(999)
    endif

    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
    call gsi_bundlecreate (work_ens,grid_ens,'ensemble work',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble creating work_ens bundle'
       call stop2(999)
    endif
    call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
    call gsi_bundlecreate (work_anl,grid_anl,'analysis work',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble creating work_anl bundle'
       call stop2(999)
    endif


    ipx=1
    im=work_ens%grid%im
    jm=work_ens%grid%jm
    km=work_ens%grid%km
!$omp parallel do schedule(static,1) private(j,n,ic3,k,i,ipic,ig,iaens)
    do k=1,km
       do ic3=1,nc3d
          ipic=ipc3d(ic3)
          do j=1,jm
             do i=1,im
                work_ens%r3(ipic)%q(i,j,k)=zero
             enddo
          enddo
          do ig=1,ntotensgrp
             iaens=ensgrp2aensgrp(ig,ic3,ibin)
             if(iaens>0) then
                do n=1,n_ens
                   do j=1,jm
                      do i=1,im
                         work_ens%r3(ipic)%q(i,j,k)=work_ens%r3(ipic)%q(i,j,k) &
                              +a_en(iaens,n)%r3(ipx)%q(i,j,k)*en_perts(n,ig,ibin)%r3(ipic)%qr4(i,j,k)
                      enddo
                   enddo
                enddo
             endif ! iaens>0
          enddo
       enddo
    enddo
    do ic2=1,nc2d
       ipic=ipc2d(ic2)
       do j=1,jm
          do i=1,im
             work_ens%r2(ipic)%q(i,j)=zero
          enddo
       enddo

       select case ( trim(StrUpCase(cvars2d(ic2))) )

          case('PS')

             if ( pwgtflg ) then
                km_tmp = km
             else
                km_tmp = 1
             endif

             do ig=1,ntotensgrp
                iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
                if(iaens>0) then
                   do n=1,n_ens
                      do k=1,km_tmp
                         do j=1,jm
                            do i=1,im
                               work_ens%r2(ipic)%q(i,j)=work_ens%r2(ipic)%q(i,j) &
                                    +a_en(iaens,n)%r3(ipx)%q(i,j,k)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)*pwgt(i,j,k)
                            enddo
                         enddo
                      enddo
                   enddo ! enddo n_ens
                endif ! iaens>0
             enddo

          case('SST')

             do ig=1,ntotensgrp
                iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
                if(iaens>0) then
                   do n=1,n_ens
                      do j=1,jm
                         do i=1,im
                            work_ens%r2(ipic)%q(i,j)=work_ens%r2(ipic)%q(i,j) &
                                 +a_en(iaens,n)%r3(ipx)%q(i,j,1)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)
                         enddo
                      enddo
                   enddo ! enddo n_ens
                endif ! iaens>0
             enddo

       end select

    enddo

    call general_sube2suba(grd_ens,grd_anl,p_e2a,work_ens%values,work_anl%values,regional)
    call gsi_bundledestroy(work_ens,istatus)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble destroying work ens bundle'
       call stop2(999)
    endif
    do ic3=1,nc3d
       cvec%r3(ipc3d(ic3))%q=work_anl%r3(ipc3d(ic3))%q
    enddo
    do ic2=1,nc2d
       cvec%r2(ipc2d(ic2))%q=work_anl%r2(ipc2d(ic2))%q
    enddo
    call gsi_bundledestroy(work_anl,istatus)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble destroying work anl bundle'
       call stop2(999)
    endif
    return

  end subroutine ensemble_forward_model_dual_res

  subroutine ensemble_forward_model_ad(cvec,a_en,ibin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model  add ensemble part to anl vars
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: For the hybrid ensemble method, add ensemble contribution
!             to standard analysis control variables.  (This follows,
!             method outlined in Wang et al, MWR, 2008).

! program history log:
!   2009-09-11  parrish
!   2010-02-20  parrish - adapt for dual resolution
!   2010-03-23  zhu - use cstate
!   2010-04-28  todling - update to use gsi_bundle
!   2011-10-03  wu - add option to weight ensemble contribution to surface pressure with vertical profile
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2011-12-01  todling - explicit dim for a_en()
!
!   input argument list:
!     cvec      -
!     a_en      -
!     ibin      - integer bin number for ensemble perturbations
!
!   output argument list:
!     cvec      -
!     a_en      -
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use hybrid_ensemble_parameters, only: n_ens,pwgtflg,pwgt
    use hybrid_ensemble_parameters, only: en_perts
    use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp
    use hybrid_ensemble_parameters, only: ensgrp2aensgrp
    implicit none

    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(inout) :: a_en(naensgrp,n_ens)
    integer,intent(in)             :: ibin

    character(len=*),parameter :: myname_=trim(myname)//'*ensemble_forward_model_ad'
    logical :: nogood
    integer(i_kind) :: i,j,k,n,im,jm,km,ic2,ic3,ipx,ipic,km_tmp
    integer(i_kind) :: ipc3d(nc3d),ipc2d(nc2d),istatus,ig,iaens

    im=cvec%grid%im
    jm=cvec%grid%jm
    km=cvec%grid%km
!   Check resolution consistency between static and ensemble components
    nogood=im/=a_en(1,1)%grid%im.or.jm/=a_en(1,1)%grid%jm.or.km/=a_en(1,1)%grid%km
    if (nogood) then
       write(6,*) myname_,': static/ensemble vectors have inconsistent dims'
       call stop2(999)
    endif

!   Request ensemble-corresponding fields from control vector
!    NOTE:  because ensemble perturbation bundle structure is same as control vector, use same ipc3d and
!             ipc2d indices for cvec and en_perts bundles.
    call gsi_bundlegetpointer (cvec,cvars3d,ipc3d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 3d pointers'
      call stop2(999)
    endif
    call gsi_bundlegetpointer (cvec,cvars2d,ipc2d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 2d pointers'
      call stop2(999)
    endif

    ipx=1
!$omp parallel do schedule(static,1) private(j,n,ic3,k,i,ic2,ipic,ig,iaens)
    do n=1,n_ens
       do ig=1,ntotensgrp
          do ic3=1,nc3d
             iaens=ensgrp2aensgrp(ig,ic3,ibin)
             if(iaens>0) then
                ipic=ipc3d(ic3)
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         a_en(iaens,n)%r3(ipx)%q(i,j,k)=a_en(iaens,n)%r3(ipx)%q(i,j,k) &
                              +cvec%r3(ipic)%q(i,j,k)*en_perts(n,ig,ibin)%r3(ipic)%qr4(i,j,k)
                      enddo
                   enddo
                enddo
             endif ! iaens>0
          enddo

          do ic2=1,nc2d
             iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
             if(iaens>0) then
                ipic=ipc2d(ic2)
                select case ( trim(StrUpCase(cvars2d(ic2))) )
 
                   case('PS')
 
                      if ( pwgtflg ) then
                         km_tmp = km
                      else
                         km_tmp = 1
                      endif

                      do k=1,km_tmp
                         do j=1,jm
                            do i=1,im
                               a_en(iaens,n)%r3(ipx)%q(i,j,k)=a_en(iaens,n)%r3(ipx)%q(i,j,k) &
                                    +cvec%r2(ipic)%q(i,j)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)*pwgt(i,j,k)
                            enddo
                         enddo
                      enddo
  
                   case('SST')
  
                      do j=1,jm
                         do i=1,im
                            a_en(iaens,n)%r3(ipx)%q(i,j,1)=a_en(iaens,n)%r3(ipx)%q(i,j,1) &
                                 +cvec%r2(ipic)%q(i,j)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)
                         enddo
                      enddo
 
                end select
             endif ! iaens>0
          enddo
       enddo
    enddo ! enddo n_ens
    return
  end subroutine ensemble_forward_model_ad

  subroutine ensemble_forward_model_ad_dual_res(cvec,a_en,ibin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensemble_forward_model_ad_dual_res  use for dualres option
!   prgmmr: parrish          org: np22                date: 2010-02-20
!
! abstract: Copy of ensemble_forward_model_ad for use with dual resolution.

! program history log:
!   2010-02-20  parrish
!   2010-03-23  zhu - use cstate
!   2010-04-06  parrish - correct dimensions of st,vp,t,rh,oz,cw,p,sst. add deallocate(suba_vars)
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-07  parrish - remove error stop for dual resolution, and add cstate again
!                           to analysis variables before adjoint interpolation
!                           to ensemble grid.
!                           the ensemble part is still not updated for
!                           generalized control variable
!   2010-05-18  todling - revisited bundle usage in light of Dave's change (2010-05-07)
!   2011-02-28  parrish - bundle changes
!   2011-10-03  wu - add option to weight ensemble contribution to surface pressure with vertical profile
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2011-12-01  todling - explicit dim for a_en()
!
!   input argument list:
!     cvec      -
!     a_en      -
!     ibin      - integer bin number for ensemble perturbations
!
!   output argument list:
!     cvec      -
!     a_en      -
!
! remarks:  
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: n_ens,pwgtflg,pwgt
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a
    use hybrid_ensemble_parameters, only: en_perts
    use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp
    use hybrid_ensemble_parameters, only: ensgrp2aensgrp
    use general_sub2grid_mod, only: general_sube2suba_ad
    use gridmod,only: regional
    use constants, only: zero
    implicit none

    type(gsi_bundle),intent(inout) :: cvec
    type(gsi_bundle),intent(inout) :: a_en(naensgrp,n_ens)
    integer,intent(in)             :: ibin

    character(len=*),parameter::myname_=trim(myname)//'*ensemble_forward_model_ad_dual_res'
    type(gsi_grid)   :: grid_ens,grid_anl
    type(gsi_bundle) :: work_ens,work_anl
    integer(i_kind) :: i,j,k,n,im,jm,km,ic2,ic3,ipx,ipic,km_tmp
    integer(i_kind) :: ipc2d(nc2d),ipc3d(nc3d),istatus,ig,iaens

!   Request ensemble-corresponding fields from control vector
!    NOTE:  because ensemble perturbation bundle structure is same as control vector, use same ipc3d and
!             ipc2d indices for cvec and en_perts bundles.
    call gsi_bundlegetpointer (cvec,cvars3d,ipc3d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 3d pointers'
      call stop2(999)
    endif
    call gsi_bundlegetpointer (cvec,cvars2d,ipc2d,istatus)
    if(istatus/=0) then
      write(6,*) myname_,': cannot find 2d pointers'
      call stop2(999)
    endif

    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
    call gsi_bundlecreate (work_ens,grid_ens,'ensemble work',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble creating work_ens bundle'
       call stop2(999)
    endif
    call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
    call gsi_bundlecreate (work_anl,grid_anl,'analysis work',istatus, &
                              names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble creating work_anl bundle'
       call stop2(999)
    endif

    do ic3=1,nc3d
       work_anl%r3(ipc3d(ic3))%q=cvec%r3(ipc3d(ic3))%q
    enddo
    do ic2=1,nc2d
       work_anl%r2(ipc2d(ic2))%q=cvec%r2(ipc2d(ic2))%q
    enddo
    work_ens%values=zero
    call general_sube2suba_ad(grd_ens,grd_anl,p_e2a,work_ens%values,work_anl%values,regional)
    call gsi_bundledestroy(work_anl,istatus)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble destroying work anl bundle'
       call stop2(999)
    endif

    ipx=1
    im=a_en(1,1)%grid%im
    jm=a_en(1,1)%grid%jm
    km=a_en(1,1)%grid%km
!$omp parallel do schedule(static,1) private(j,n,ic3,k,i,ic2,ipic,ig,iaens)
    do n=1,n_ens
       do ig=1,ntotensgrp
          do ic3=1,nc3d
             iaens=ensgrp2aensgrp(ig,ic3,ibin)
             if(iaens>0) then
                ipic=ipc3d(ic3)
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         a_en(iaens,n)%r3(ipx)%q(i,j,k)=a_en(iaens,n)%r3(ipx)%q(i,j,k) &
                              +work_ens%r3(ipic)%q(i,j,k)*en_perts(n,ig,ibin)%r3(ipic)%qr4(i,j,k)
                      enddo
                   enddo
                enddo
             endif ! iaens>0
          enddo
          do ic2=1,nc2d
             iaens=ensgrp2aensgrp(ig,ic2+nc3d,ibin)
             if(iaens>0) then
                ipic=ipc2d(ic2)
                select case ( trim(StrUpCase(cvars2d(ic2))) )

                   case('PS')

                      if ( pwgtflg ) then
                         km_tmp = km
                      else
                         km_tmp = 1
                      endif

                      do k=1,km_tmp
                         do j=1,jm
                            do i=1,im
                               a_en(iaens,n)%r3(ipx)%q(i,j,k)=a_en(iaens,n)%r3(ipx)%q(i,j,k) &
                                    +work_ens%r2(ipic)%q(i,j)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)*pwgt(i,j,k)
                            enddo
                         enddo
                      enddo

                   case('SST')

                      do j=1,jm
                         do i=1,im
                            a_en(iaens,n)%r3(ipx)%q(i,j,1)=a_en(iaens,n)%r3(ipx)%q(i,j,1) &
                                 +work_ens%r2(ipic)%q(i,j)*en_perts(n,ig,ibin)%r2(ipic)%qr4(i,j)
                         enddo
                      enddo

                end select
             endif ! iaens>0
          enddo
       enddo
    enddo ! enddo n_ens
    call gsi_bundledestroy(work_ens,istatus)
    if(istatus/=0) then
       write(6,*)trim(myname_),': trouble destroying work ens bundle'
       call stop2(999)
    endif
    return

  end subroutine ensemble_forward_model_ad_dual_res

  subroutine special_sd2h0
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_sd2h0  initialize subroutine special_sd2h
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: initialize subroutine special_sd2h (subdomain to slab for  
!             variable a_en).
!
! program history log:
!   2009-06-16  parrish
!   2010-02-10  parrish, correct allocate error on ndrecv_sd2h, found by Arthur Mizzi.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: r_kind,i_kind
    use mpimod, only: npe,mype,mpi_comm_world,ierror,mpi_rtype
    use gridmod, only: nlat,nlon,nnnn1o,regional,vlevs
    use berror, only: nx,ny,nf
    implicit none

    integer(i_kind),dimension(0:npe-1):: nh_0_all,nh_1_all,nv_0_all,nv_1_all
    integer(i_kind) nvert,nh_tot,nh_this,nn,nv_tot,nv_this,kchk,n,kk,i,k
    real(r_kind),allocatable:: zsub(:,:),z(:)

!    set nval2f (loosely paraphrased from jfunc.f90)

    nscl=3          !  hard-wired here, later generalize when generalizing control variables
    if(regional) then
       nval2f=nlat*nlon
    else
       nval2f=ny*nx + 2*(2*nf+1)*(2*nf+1)
    end if


    allocate(nsend_sd2h(0:npe-1),ndsend_sd2h(0:npe),nrecv_sd2h(0:npe-1),ndrecv_sd2h(0:npe))
    allocate(i_recv(nval2f*nnnn1o),k_recv(nval2f*nnnn1o))
    nvert=vlevs

!  compute nv_0,nv_1

    nv_tot=nvert
    nv_this=nv_tot/npe
    if(mod(nv_tot,npe)==0) then
       kchk=npe
    else
       nv_this=nv_this+1
       kchk=mod(nv_tot,npe)
    end if

    nv_0_all=-1
    nv_1_all=-2
    nn=0
    do n=1,npe
       if(n<=kchk) then
          kk=nv_this
       else
          kk=nv_this-1
       end if
       if(kk>0) then
          nv_0_all(n-1)=nn+1
          nv_1_all(n-1)=nn+kk
       end if
       nn=nn+kk
    enddo
    nv_0=nv_0_all(mype)
    nv_1=nv_1_all(mype)

!     compute nh_0, nh_1

    nh_tot=nval2f
    nh_this=nh_tot/npe
    if(mod(nh_tot,npe)/=0) nh_this=nh_this+1
    if(mod(nh_tot,npe)==0) then
       kchk=npe
    else
       kchk=mod(nh_tot,npe)
    end if

    nh_0_all=-1
    nh_1_all=-2
    nn=0
    do n=1,npe
       if(n<=kchk) then
          kk=nh_this
       else
          kk=nh_this-1
       end if
       if(kk>0) then
          nh_0_all(n-1)=nn+1
          nh_1_all(n-1)=nn+kk
       end if
       nn=nn+kk
    enddo
    nh_0=nh_0_all(mype)
    nh_1=nh_1_all(mype)

!   compute nsend_sd2h,ndsend_sd2h,nrecv_sd2h,ndrecv_sd2h

    ndsend_sd2h(0)=0
    ndrecv_sd2h(0)=0
    do n=0,npe-1
       nsend_sd2h(n)=max(0,(nv_1_all(n)-nv_0_all(n)+1)*(nh_1-nh_0+1))
       ndsend_sd2h(n+1)=ndsend_sd2h(n)+nsend_sd2h(n)
       nrecv_sd2h(n)=max(0,(nv_1-nv_0+1)*(nh_1_all(n)-nh_0_all(n)+1))
       ndrecv_sd2h(n+1)=ndrecv_sd2h(n)+nrecv_sd2h(n)
    enddo
    allocate(zsub(nh_0:nh_1,nvert),z(nval2f*(nv_1-nv_0+1)))
    do k=1,nvert
       do i=nh_0,nh_1
          zsub(i,k)=i
       enddo
    enddo
    call mpi_alltoallv(zsub,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                       z,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
    do i=1,nval2f*(nv_1-nv_0+1)
       i_recv(i)=nint(z(i))
    enddo

    do k=1,nvert
       do i=nh_0,nh_1
          zsub(i,k)=k
       enddo
    enddo
    call mpi_alltoallv(zsub,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                       z,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
    do i=1,nval2f*(nv_1-nv_0+1)
       k_recv(i)=nint(z(i))
    enddo

    deallocate(zsub,z)
    return

  end subroutine special_sd2h0

  subroutine special_sd2h(zsub,z)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    special_sd2h  subdomain to slab for variable a_en
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: subdomain to slab for variable a_en.
!
! program history log:
!   2009-06-16  parrish
!
!   input argument list:
!     zsub     - input array on "subdomains"
!
!   output argument list:
!     z        - output array on slabs (form expected for input argument to ckgcov)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: vlevs
  use constants, only: zero
  use mpimod, only: mpi_rtype,ierror,mpi_comm_world
  implicit none

  real(r_kind),dimension(nh_0:nh_1,vlevs,nscl),intent(in   ) :: zsub
  real(r_kind),dimension(nval2f,nv_0:nv_1,nscl)         ,intent(  out) :: z

  real(r_kind) zsub1(nh_0:nh_1,vlevs),work(nval2f*(nv_1-nv_0+1))
  integer(i_kind) i,ii,is,k
! integer(i_kind) ibadp,ibadm,kbadp,kbadm
! logical good

!      1 <= nh_0 <= nh_1 <= nval2f

!      1 <= nv_0 <= nv_1 <= vlevs

  z=zero
  do is=1,nscl
     do k=1,vlevs
        do i=nh_0,nh_1
           zsub1(i,k)=zsub(i,k,is)
        enddo
     enddo
     call mpi_alltoallv(zsub1,nsend_sd2h,ndsend_sd2h,mpi_rtype,&
                        work,nrecv_sd2h,ndrecv_sd2h,mpi_rtype,mpi_comm_world,ierror)
     do ii=1,nval2f*(nv_1-nv_0+1)
        i=i_recv(ii) ; k=k_recv(ii)
        z(i,k,is)=work(ii)
     enddo
  enddo
  return
  
end subroutine special_sd2h

subroutine sqrt_beta_s_mult_cvec(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_beta_s_mult_cvec  multiply grady by sqrt_beta_s
!   prgmmr: parrish          org: np22                date: 2016-05-13
!
! abstract: Multiply static part of grady by sqrt_beta_s.
!
! program history log:
!   2009-10-12  parrish  initial documentation
!   2010-03-29  kleist   comment out beta_s0 for SST
!   2010-04-28  todling  update to use gsi_bundle
!   2011-06-13  wu       used height dependent beta for regional
!   2012-05-12  el akkraoui  hybrid beta parameters now vertically varying
!   2015-09-18  todling - add sst_staticB to control use of ensemble SST error covariance 
!
!   input argument list:
!     grady    - input field  grady_x1
!
!   output
!     grady    - sqrt_beta_s*grady_x1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: nsubwin
  use hybrid_ensemble_parameters, only: oz_univ_static
  use hybrid_ensemble_parameters, only: sqrt_beta_s
  use hybrid_ensemble_parameters, only: sst_staticB
  use constants, only:  one
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use control_vectors,only: control_vector
  use timermod, only: timer_ini,timer_fnl

  use gridmod, only: nsig,lat2,lon2

  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: grady

! Declare local variables
  character(len=*),parameter::myname_=myname//'*sqrt_beta_s_mult_cvec'
  integer(i_kind) :: i,j,k,ii,ic2,ic3,istatus
  integer(i_kind) :: ipc3d(nc3d),ipc2d(nc2d)

  ! Initialize timer
  call timer_ini('sqrt_beta_s_mult_cvec')

  ! Request CV pointers to vars pertinent to ensemble
  call gsi_bundlegetpointer ( grady%step(1), cvars3d, ipc3d, istatus )
  if ( istatus /= 0 ) then
     write(6,*) myname_,': cannot proceed, CV does not contain ens-required 3d fields'
     call stop2(999)
  endif
  call gsi_bundlegetpointer ( grady%step(1), cvars2d, ipc2d, istatus )
  if ( istatus /= 0 ) then
     write(6,*) myname_,': cannot proceed, CV does not contain ens-required 2d fields'
     call stop2(999)
  endif

  ! multiply by sqrt_beta_s
!$omp parallel do schedule(static,1) private(ic3,ic2,k,j,i,ii)
  do j=1,lon2
     do ii=1,nsubwin
        do ic3=1,nc3d
           ! check for ozone and skip if oz_univ_static = true
           if ( trim(StrUpCase(cvars3d(ic3))) == 'OZ' .and. oz_univ_static ) cycle
           do k=1,nsig
              do i=1,lat2
                 grady%step(ii)%r3(ipc3d(ic3))%q(i,j,k) = sqrt_beta_s(k)*grady%step(ii)%r3(ipc3d(ic3))%q(i,j,k)
              enddo
           enddo
        enddo
        do ic2=1,nc2d
           ! Default to static B estimate for SST
           if ( trim(StrUpCase(cvars2d(ic2))) == 'SST' ) then
              if(sst_staticB) then
                 cycle
              else
                  if(j==1.and.mype==0) write(6,*) myname_, ': scale static SST B-error by ', sqrt_beta_s(1)
              endif
           endif
           do i=1,lat2
              grady%step(ii)%r2(ipc2d(ic2))%q(i,j) = sqrt_beta_s(1)*grady%step(ii)%r2(ipc2d(ic2))%q(i,j)
           enddo
        enddo
     enddo
  enddo

  ! Finalize timer
  call timer_fnl('sqrt_beta_s_mult_cvec')

  return
end subroutine sqrt_beta_s_mult_cvec

subroutine sqrt_beta_s_mult_bundle(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_beta_s_mult_bundle  multiply grady by sqrt_beta_s
!   prgmmr: parrish          org: np22                date: 2016-05-13
!
! abstract: Multiply static part of grady by sqrt_beta_s.
!
! program history log:
!   2009-10-12  parrish  initial documentation
!   2010-03-29  kleist   comment out sqrt_beta_s for SST
!   2010-04-28  todling  update to use gsi_bundle
!   2011-06-13  wu       used height dependent beta for regional
!   2012-05-12  el akkraoui  hybrid beta parameters now vertically varying
!   2015-09-18  todling - add sst_staticB to control use of ensemble SST error covariance 
!
!   input argument list:
!     grady    - input field  grady_x1
!
!   output
!     grady    - sqrt_beta_s*grady_x1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: oz_univ_static
  use hybrid_ensemble_parameters, only: sqrt_beta_s
  use hybrid_ensemble_parameters, only: sst_staticB
  use constants, only:  one
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use timermod, only: timer_ini,timer_fnl

  use gridmod, only: nsig,lat2,lon2

  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: grady

! Declare local variables
  character(len=*),parameter::myname_=myname//'*sqrt_beta_s_mult_bundle'
  integer(i_kind) :: i,j,k,ic2,ic3,istatus
  integer(i_kind) :: ipc3d(nc3d),ipc2d(nc2d)

  ! Initialize timer
  call timer_ini('sqrt_beta_s_mult_bundle')

  ! Request CV pointers to vars pertinent to ensemble
  call gsi_bundlegetpointer ( grady, cvars3d, ipc3d, istatus )
  if ( istatus /= 0 ) then
     write(6,*) myname_,': cannot proceed, CV does not contain ens-required 3d fields'
     call stop2(999)
  endif
  call gsi_bundlegetpointer ( grady, cvars2d, ipc2d, istatus )
  if ( istatus /= 0 ) then
     write(6,*) myname_,': cannot proceed, CV does not contain ens-required 2d fields'
     call stop2(999)
  endif

  ! multiply by sqrt_beta_s
!$omp parallel do schedule(static,1) private(ic3,ic2,k,j,i)
  do j=1,lon2
     do ic3=1,nc3d
        ! check for ozone and skip if oz_univ_static = true
        if ( trim(StrUpCase(cvars3d(ic3))) == 'OZ' .and. oz_univ_static ) cycle
        do k=1,nsig
           do i=1,lat2
              grady%r3(ipc3d(ic3))%q(i,j,k) = sqrt_beta_s(k)*grady%r3(ipc3d(ic3))%q(i,j,k)
           enddo
        enddo
     enddo
     do ic2=1,nc2d
        ! Default to static B estimate for SST
        if ( trim(StrUpCase(cvars2d(ic2))) == 'SST' ) then
           if(sst_staticB) then
              cycle
           else
              if(mype==0) write(6,*) myname_, ': scale static SST B-error by ', sqrt_beta_s(1)
           endif
        endif
        do i=1,lat2
           grady%r2(ipc2d(ic2))%q(i,j) = sqrt_beta_s(1)*grady%r2(ipc2d(ic2))%q(i,j)
        enddo
     enddo
  enddo

  ! Finalize timer
  call timer_fnl('sqrt_beta_s_mult_bundle')

  return
end subroutine sqrt_beta_s_mult_bundle

subroutine sqrt_beta_e_mult_cvec(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_beta_e_mult_cvec  multiply grady by sqrt_beta_e
!   prgmmr: parrish          org: np22                date: 2016-05-13
!
! abstract: Multiply ensemble amplitude fields by sqrt_beta_e.
!
! program history log:
!   2009-10-12  parrish  initial documentation
!   2010-03-29  kleist   comment out sqrt_beta_e for SST
!   2010-04-28  todling  update to use gsi_bundle
!   2011-06-13  wu       used height dependent beta for regional
!   12-05-2012  el akkraoui  hybrid beta parameters now vertically varying
!
!   input argument list:
!     grady    - input field  grady_a_en
!
!   output
!     grady    - sqrt_beta_e*grady_a_en
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: nsubwin
  use hybrid_ensemble_parameters, only: n_ens,sqrt_beta_e,grd_ens
  use hybrid_ensemble_parameters, only: naensgrp
  use control_vectors,only: control_vector
  use timermod, only: timer_ini,timer_fnl

  use gridmod, only: nsig

  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: grady

! Declare local variables
  character(len=*),parameter::myname_=myname//'*sqrt_beta_e_mult'
  integer(i_kind) :: i,j,k,ii,nn,ig

  ! Initialize timer
  call timer_ini('sqrt_beta_e_mult')

  ! multiply by sqrt_beta_e
!$omp parallel do schedule(static,1) private(nn,k,j,i,ii,ig)
  do nn=1,n_ens
     do ii=1,nsubwin
        do ig=1,naensgrp
           do k=1,nsig
              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    grady%aens(ii,ig,nn)%r3(1)%q(i,j,k) = sqrt_beta_e(k)*grady%aens(ii,ig,nn)%r3(1)%q(i,j,k)
                 enddo
              enddo
           enddo
        enddo
     enddo
  enddo

  ! Finalize timer
  call timer_fnl('sqrt_beta_e_mult')

  return
end subroutine sqrt_beta_e_mult_cvec

subroutine sqrt_beta_e_mult_bundle(aens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrt_beta_e_mult_bundle  multiply grady by sqrt_beta_e
!   prgmmr: parrish          org: np22                date: 2016-05-13
!
! abstract: Multiply ensemble amplitude fields by sqrt_beta_e.
!
! program history log:
!   2009-10-12  parrish  initial documentation
!   2010-03-29  kleist   comment out sqrt_beta_e for SST
!   2010-04-28  todling  update to use gsi_bundle
!   2011-06-13  wu       used height dependent beta for regional
!   12-05-2012  el akkraoui  hybrid beta parameters now vertically varying
!
!   input argument list:
!     grady    - input field  grady_a_en
!
!   output
!     grady    - sqrt_beta_e*grady_a_en
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,sqrt_beta_e,grd_ens
  use hybrid_ensemble_parameters, only: naensgrp
  use gsi_bundlemod, only: gsi_bundle
  use timermod, only: timer_ini,timer_fnl
  use gridmod, only: nsig

  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: aens(naensgrp,n_ens)

! Declare local variables
  character(len=*),parameter::myname_=myname//'*sqrt_beta_e_mult'
  integer(i_kind) :: i,j,k,nn,ig

  ! Initialize timer
  call timer_ini('sqrt_beta_e_mult')

  ! multiply by sqrt_beta_e
!$omp parallel do schedule(static,1) private(nn,k,j,i,ig)
  do nn=1,n_ens
     do ig=1,naensgrp
        do k=1,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 aens(ig,nn)%r3(1)%q(i,j,k) = sqrt_beta_e(k)*aens(ig,nn)%r3(1)%q(i,j,k)
              enddo
           enddo
        enddo
     enddo
  enddo

  ! Finalize timer
  call timer_fnl('sqrt_beta_e_mult')

  return
end subroutine sqrt_beta_e_mult_bundle

subroutine init_sf_xy(jcap_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sf_xy
!   prgmmr: parrish          org: np23                date: 2009-12-17
!
! abstract: initialize horizontal spectral localization sf_xy
!
! program history log:
!   2009-12-17  parrish
!   2010-06-29  parrish, modify so localization length can be different for each vertical level.
!                 to do this, add new variable array s_ens_hv(nsig), which is read in if readin_localization=.true.
!                 Otherwise, s_ens_hv is set equal to s_ens_h.
!
!   input argument list:
!     jcap_in - maximum spectral truncation allowed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_hv,sp_loc,grd_ens,grd_loc,sp_ens,n_ens,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use hybrid_ensemble_parameters,only: naensgrp,naensloc
  use gridmod,only: use_sp_eqspace
  use general_specmod, only: general_init_spec_vars
  use constants, only: zero,half,one,two,three,rearth,pi
  use constants, only: rad2deg
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use gsi_enscouplermod, only: gsi_enscoupler_localization_grid
  use gsi_io, only: verbose
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i,ii,j,igg,k,l,n,jcap,kk,nsigend,ig
  real(r_kind),allocatable::g(:),gtemp(:)
  real(r_kind) factor
  real(r_kind),allocatable::rkm(:),f(:,:),f0(:,:)
  real(r_kind) ftest(grd_loc%nlat,grd_loc%nlon,grd_loc%kbegin_loc:grd_loc%kend_alloc)
  real(r_single) out1(grd_ens%nlon,grd_ens%nlat)
  real(r_single) pn0_npole
  real(r_kind) s_ens_h_min
  real(r_kind) rlats_ens_local(grd_ens%nlat)
  real(r_kind) rlons_ens_local(grd_ens%nlon)
  character(5) mapname
  logical make_test_maps
  integer(i_kind) nord_sploc2ens
  integer(i_kind) nlon_sploc0,nlon_sploc,nlat_sploc,num_fields
  logical print_verbose

  print_verbose = .false. .and. mype == 0
  if(verbose .and. mype == 0)print_verbose=.true.
  make_test_maps=.false.
  nord_sploc2ens=4

!    make sure s_ens_hv is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_hv <= 5500 )

  s_ens_h_min=pi*rearth*.001_r_kind/jcap_in
  do k=1,grd_ens%nsig
     do ig=1,naensloc
        if(s_ens_hv(k,ig) <  s_ens_h_min) then
           if(mype == 0) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k,ig),' km--too small, min value = ', &
                                           s_ens_h_min,' km.'
           if(mype == 0) write(6,*)' s_ens_hv(',k,') reset to min value'
           s_ens_hv(k,ig)=s_ens_h_min
        else if(s_ens_hv(k,ig) >  5500._r_kind) then
           if(mype == 0) write(6,*)' s_ens_hv(',k,') = ',s_ens_hv(k,ig),' km--too large, max value = 5500 km.'
           if(mype == 0) write(6,*)' s_ens_hv(',k,') reset to max value'
           s_ens_hv(k,ig)=5500._r_kind
        end if
     enddo
  enddo


  jcap=nint(1.2_r_kind*pi*rearth*.001_r_kind/minval(s_ens_hv))
  jcap=min(jcap,jcap_in)

!  if it is desired to have a different localization grid to apply the spectral
!  localization, do that here, with resolution based on the value of jcap:
  call gsi_enscoupler_localization_grid (rlats_ens_local,rlons_ens_local)
  if(use_localization_grid) then
     nlat_sploc=min(grd_ens%nlat-4,max(int(half*grd_ens%nlat),2*jcap+4))
     if(mod(nlat_sploc,2)/=0) nlat_sploc=nlat_sploc+1   ! make number of gaussian lats even
     nlon_sploc0=2*nlat_sploc
     call acceptable_for_essl_fft(nlon_sploc0,nlon_sploc)
  else
     nlat_sploc=grd_ens%nlat
     nlon_sploc=grd_ens%nlon
     nlon_sploc0=nlon_sploc
  end if
  if(print_verbose)then
     write(6,*)' nlat_sploc,nlon_sploc0,nlon_sploc=',nlat_sploc,nlon_sploc0,nlon_sploc
     write(6,*)' nlat_ens  ,nlon_ens              =',grd_ens%nlat,grd_ens%nlon
  end if
  num_fields=grd_ens%nsig*n_ens
  call general_sub2grid_create_info(grd_sploc,1,nlat_sploc,nlon_sploc,grd_ens%nsig,num_fields,.false.)

!  set up spectral variables for jcap

  call general_init_spec_vars(sp_loc,jcap,jcap,nlat_sploc,nlon_sploc,eqspace=use_sp_eqspace)
  if(print_verbose) then
     if( grd_ens%nlon == nlon_sploc .and. grd_ens%nlat == nlat_sploc)then
        write(6,*)' ensemble and analysis nlat,nlon are the same '
     else
        do j=1,grd_ens%nlon
           if(j.le.nlon_sploc) then
              write(6,'(" j,rlon_sploc(j),rlon_ens(j)=",i4,2f12.3)') &
                  j,rad2deg*sp_loc%rlons(j),rad2deg*sp_ens%rlons(j)
           else
              write(6,'(" j,              rlon_ens(j)=",i4,12x,f12.3)') &
                  j,rad2deg*sp_ens%rlons(j)
           end if
        enddo
        do i=1,grd_ens%nlat
           if(i.le.nlat_sploc) then
              write(6,'(" i,rlat_sploc(i),rlat_ens(i)=",i4,2f12.3)') &
                  i,rad2deg*sp_loc%rlats(i),rad2deg*sp_ens%rlats(i)
           else
              write(6,'(" i,              rlat_ens(i)=",i4,12x,f12.3)') &
                  i,rad2deg*sp_ens%rlats(i)
           end if
        enddo
     end if
  end if

!   regardless of whether or not nlat_sploc=grd_ens%nlat and nlon_sploc=grd_ens%nlon, compute
!    interpolation structure variable that will be used for interpolation from sp_loc grid to grd_ens.
!   if they are identical, then the interpolation is just an identity op.
  call g_create_egrid2agrid(grd_ens%nlat,rlats_ens_local,grd_ens%nlon,rlons_ens_local,&
                            nlat_sploc,sp_loc%rlats,nlon_sploc,sp_loc%rlons, &
                            nord_sploc2ens,p_sploc2ens,.true.,eqspace=use_sp_eqspace)

!    the following code is used to compute the desired spectrum to get a
!     gaussian localization of desired length-scale.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   how spectrum is obtained:
!
!     Correlation matrix:  C = Y*D*Ytrans
!
!       where Y is matrix of spherical harmonics, evaluated on gaussian grid, and D is a diagonal matrix
!
!     To obtain D, exploit fact that for D a function only of total wave-number n, then C is homogeneous
!       and isotropic on the sphere.
!
!     So look at the special case of a test point centered on the north pole.  The correlation function
!       is then only a function of latitude, call it c(phi), where c(pi/2) = 1.
!
!     Now we have C = P*D*Ptrans, where we have reduced the problem to 1 dimension, latitude, and in
!       spectral space, total wave number n.  P is the zonal component only of Y.
!
!     Next, form the product
!                             C*e1 =P*D*Ptrans*e1,
!
!            where e1 is a vector of all 0, except for 1 at the north pole.
!
!     Then have P*D*Ptrans*e1 = sum(n) p(n,j)*d(n)*p(n,1) = c(j,1)
!
!        where j=1 corresponds to north pole point in this formulation.
!
!     Now if we have available C(j,1), a gaussian of desired length-scale evaluated (note, doesn't have to
!            be gaussian!) on the gaussian grid, then applying the inverse transform subroutine g2s0 to
!     C yields the product
!
!             Chat(n) = d(n)*p(n,1)
!
!     So finally the desired spectrum is
!
!               d(n) = chat(n)/p(n,1)
!
!     To create the full spectral transform of the desired correlation, d(n) is copied to all non-zero
!      zonal wave numbers on lines of constant total wave number n, multiplied by 1/2 to account for
!      two degrees of freedom for non-zero zonal wave numbers.
!
!     Note that while creating this routine, a bug was discovered in s2g0 routine for evaluation of pole
!       value.  There was a missing factor of 1/sqrt(2) apparently.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                 create reference gaussian centered on north pole with desired length-scale.

!     compute latitudes in km from north pole

  allocate(rkm(grd_sploc%nlat),f(grd_sploc%nlat,grd_sploc%nlon),f0(grd_sploc%nlat,grd_sploc%nlon))
  rkm(grd_sploc%nlat)=zero
  rkm(1)=two*asin(one)*rearth*.001_r_kind
  do i=1,(grd_sploc%nlat-2)/2
     rkm(grd_sploc%nlat-i)=(asin(one)-asin(sp_loc%slat(i)))*rearth*.001_r_kind
     rkm(1+i)=(asin(one)+asin(sp_loc%slat(i)))*rearth*.001_r_kind
  enddo
  if(print_verbose) write(6,*)' in init_sf_xy, lat,max(dlat)=', &
           rkm(1+(grd_sploc%nlat-2)/2), &
          -rkm(grd_sploc%nlat-(grd_sploc%nlat-2)/2)+rkm(1+(grd_sploc%nlat-2)/2),' km'

  if(.not.allocated(spectral_filter)) allocate(spectral_filter(naensloc,sp_loc%nc,grd_sploc%nsig))
  if(.not.allocated(sqrt_spectral_filter)) allocate(sqrt_spectral_filter(naensloc,sp_loc%nc,grd_sploc%nsig))
  allocate(g(sp_loc%nc),gtemp(sp_loc%nc))
  do ig=1,naensloc
     spectral_filter(ig,:,:)=zero
     level_loop: do k=1,grd_sploc%nsig
        do kk=1,k-1
           if(s_ens_hv(k,ig) == s_ens_hv(kk,ig))then
              spectral_filter(ig,:,k)=spectral_filter(ig,:,k-1)
              cycle level_loop
           end if
        end do
        if(ig > 1)then
           do igg=1,ig-1
              do kk=1,grd_sploc%nsig
                 if(s_ens_hv(k,ig) == s_ens_hv(kk,igg))then
                    spectral_filter(ig,:,k)=spectral_filter(igg,:,kk)
                    cycle level_loop
                 end if
              end do
           end do
        end if

        do i=1,grd_sploc%nlat
           f0(i,1)=exp(-half*(rkm(i)/s_ens_hv(k,ig))**2)
        enddo


        do j=2,grd_sploc%nlon
           do i=1,grd_sploc%nlat
              f0(i,j)=f0(i,1)
           enddo
        end do


        call general_g2s0(grd_sploc,sp_loc,g,f0)

        call general_s2g0(grd_sploc,sp_loc,g,f)

!       adjust so value at np = 1
        f=f/f(grd_sploc%nlat,1)
        f0=f
        call general_g2s0(grd_sploc,sp_loc,g,f)
        call general_s2g0(grd_sploc,sp_loc,g,f)
        if(mype == 0)then
           nsigend=k
           do kk=k+1,grd_sploc%nsig
              if(s_ens_hv(kk,ig) /= s_ens_hv(k,ig))exit
              nsigend=nsigend+1
           enddo
           write(6,900)k,nsigend,sp_loc%jcap,s_ens_hv(k,ig),maxval(abs(f0-f))
900        format(' in init_sf_xy, jcap,s_ens_hv(',i5,1x,'-',i5,'), max diff(f0-f)=', &
                                           i10,f10.2,e20.10)
        end if

!       correct spectrum by dividing by pn0_npole

!       obtain pn0_npole
!$omp parallel do schedule(static,1) private(n,gtemp,f)
        do n=0,sp_loc%jcap
           gtemp=zero
           gtemp(2*n+1)=one
           call general_s2g0(grd_sploc,sp_loc,gtemp,f)
           pn0_npole=f(grd_sploc%nlat,1)
           g(2*n+1)=g(2*n+1)/pn0_npole
        enddo
   

!       obtain spectral_filter

        ii=0
        do l=0,sp_loc%jcap
           if(ig>naensgrp) then
              factor=one/g(1)
           else
              factor=one
              if(l>0) factor=half
           end if
           do n=l,sp_loc%jcap
              ii=ii+1
              if(sp_loc%factsml(ii)) then
                 spectral_filter(ig,ii,k)=zero
              else
                 spectral_filter(ig,ii,k)=factor*g(2*n+1)
              end if
              ii=ii+1
              if(l == 0 .or. sp_loc%factsml(ii)) then
                 spectral_filter(ig,ii,k)=zero
              else
                 spectral_filter(ig,ii,k)=factor*g(2*n+1)
              end if
           enddo
        enddo
     enddo level_loop
  enddo !ig loop
  deallocate(g,gtemp)

! Compute sqrt(spectral_filter).  Ensure spectral_filter >=0 zero
!$omp parallel do schedule(static,1) private(k,i)
  do ig=1,naensloc
     do k=1,grd_sploc%nsig
        do i=1,sp_loc%nc
           spectral_filter(ig,i,k) = max(spectral_filter(ig,i,k),zero)
           sqrt_spectral_filter(ig,i,k) = sqrt(spectral_filter(ig,i,k))
        end do
     end do
  enddo !ig loop

!  assign array k_index for each processor, based on grd_loc%kbegin_loc,grd_loc%kend_loc

  if(.not.allocated(k_index)) allocate(k_index(grd_loc%kbegin_loc:grd_loc%kend_alloc))
  k_index=0
  do k=grd_loc%kbegin_loc,grd_loc%kend_loc
     k_index(k)=1+mod(k-1,grd_loc%nsig)
!!     write(6,*) 'k_index(',k,')=',k_index(k)
  enddo

  if(make_test_maps) then
   ftest=zero
   do k=grd_loc%kbegin_loc,grd_loc%kend_loc
      ftest(grd_ens%nlat/2,grd_ens%nlon/2,k)=one
   enddo
   call sf_xy(1,ftest,grd_loc%kbegin_loc,grd_loc%kend_loc)
   if(mype==0) then
      do j=1,grd_ens%nlon
        do i=1,grd_ens%nlat
           out1(j,i)=ftest(i,j,grd_loc%kbegin_loc)
        enddo
     enddo
     write(mapname,'("out_",i2.2)')1+mod(grd_loc%kbegin_loc-1,grd_ens%nsig)
     call outgrads1(out1,grd_ens%nlon,grd_ens%nlat,mapname)
   end if
  end if
  deallocate(rkm,f,f0)
  return

end subroutine init_sf_xy

subroutine sf_xy(ig,f,k_start,k_end)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sf_xy       spectral isotropic localization for global domain
!
!   prgrmmr:  parrish        org: np22                              date: 2009-09-30
!
! abstract:  use spherical harmonic transform to do horizontal isotropic localization of hybrid
!             ensemble control variable a_en.
!
! program history log:
!   2009-10-16  parrish  initial documentation
!   2010-03-11  parrish - adjust dimensions for f to allow for nlevs=0
!
!   input argument list:
!     ig       - number for smoothing scales
!     f        - input field to be filtered
!     k_start  - starting horizontal slab index
!     k_end    - ending horizontal slab index    (k_end can be less than k_start, meaning there is
!                                                   no work on this processor)
!                 NOTE: above args allow horizontal localization length to vary in vertical
!
!   output argument list:
!     f        - filtered output
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens,sp_loc,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use hybrid_ensemble_parameters,only: naensgrp
  use egrid2agrid_mod,only: g_egrid2agrid,g_egrid2agrid_ad,g_agrid2egrid
  implicit none

  integer(i_kind),intent(in   ) :: ig
  integer(i_kind),intent(in   ) :: k_start,k_end
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat,grd_ens%nlon,k_start:max(k_start,k_end))

  real(r_kind) g(sp_loc%nc)
  real(r_kind) work(grd_sploc%nlat,grd_sploc%nlon,1)
  integer(i_kind) k
  logical vector(k_start:max(k_start,k_end))

  if(.not.use_localization_grid) then

    if(ig>naensgrp) then
!$omp parallel do schedule(static,1) private(k,g)
       do k=k_start,k_end
          call general_g2s0(grd_ens,sp_loc,g,f(:,:,k))
          g(:)=g(:)*spectral_filter(ig,:,k_index(k))
          call general_s2g0(grd_ens,sp_loc,g,f(:,:,k))
       enddo
    else
!$omp parallel do schedule(static,1) private(k)
       do k=k_start,k_end
          call sfilter(grd_ens,sp_loc,spectral_filter(ig,:,k_index(k)),f(1,1,k))
       enddo
    endif

  else

    vector=.false.
    if(ig>naensgrp) then
!$omp parallel do schedule(static,1) private(k,g,work)
       do k=k_start,k_end
          call g_agrid2egrid(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
          call general_g2s0(grd_ens,sp_loc,g,f(:,:,k))
          g(:)=g(:)*spectral_filter(ig,:,k_index(k))
          call general_s2g0(grd_ens,sp_loc,g,f(:,:,k))
          call g_egrid2agrid(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
       enddo
    else
!$omp parallel do schedule(static,1) private(k,work)
       do k=k_start,k_end
          call g_egrid2agrid_ad(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
          call sfilter(grd_ens,sp_loc,spectral_filter(ig,:,k_index(k)),f(1,1,k))
          call g_egrid2agrid(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
       enddo
    end if

  endif
  return

end subroutine sf_xy

subroutine sqrt_sf_xy(ig,z,f,k_start,k_end)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sqrt_sf_xy  sqrt(sf_xy)
!
!   prgrmmr:  parrish        org: np22                              date: 2011-06-28
!
! abstract:  sqrt of spectral localization operator sf_xy.
!
! program history log:
!   2011-06-28  parrish  initial documentation
!
!   input argument list:
!     ig       - number for smoothing scales
!     z        - input spectral space variable
!     k_start  - starting horizontal slab index
!     k_end    - ending horizontal slab index    (k_end can be less than k_start, meaning there is
!                                                   no work on this processor)
!                 NOTE: above args allow horizontal localization length to vary in vertical
!
!   output argument list:
!     f        - output grid space variable
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens,sp_loc,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use egrid2agrid_mod,only: g_egrid2agrid
  implicit none

  integer(i_kind),intent(in   ) :: ig
  integer(i_kind),intent(in   ) :: k_start,k_end
  real(r_kind)   ,intent(in   ) :: z(sp_loc%nc,k_start:max(k_start,k_end))
  real(r_kind)   ,intent(  out) :: f(grd_ens%nlat,grd_ens%nlon,k_start:max(k_start,k_end))

  real(r_kind) g(sp_loc%nc)
  real(r_kind) work(grd_sploc%nlat,grd_sploc%nlon,1)
  integer(i_kind) k
  logical vector(k_start:max(k_start,k_end))

  if(.not.use_localization_grid) then

!$omp parallel do schedule(static,1) private(k,g)
    do k=k_start,k_end
       g(:)=z(:,k)*sqrt_spectral_filter(ig,:,k_index(k))
       call general_s2g0(grd_ens,sp_loc,g,f(:,:,k))
    enddo

  else

     vector=.false.
!$omp parallel do schedule(static,1) private(k,g,work)
     do k=k_start,k_end
        g(:)=z(:,k)*sqrt_spectral_filter(ig,:,k_index(k))
        call general_s2g0(grd_sploc,sp_loc,g,work)
        call g_egrid2agrid(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
     enddo

  end if
  return

end subroutine sqrt_sf_xy

subroutine sqrt_sf_xy_ad(ig,z,f,k_start,k_end)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sqrt_sf_xy_ad  adjoint of sqrt_sf_xy
!
!   prgrmmr:  parrish        org: np22                              date: 2011-06-28
!
! abstract:  adjoint of sqrt_sf_xy.
!
! program history log:
!   2011-06-28  parrish  initial documentation
!
!   input argument list:
!     ig       - number for smoothing scales
!     f        - grid space variable
!     k_start  - starting horizontal slab index
!     k_end    - ending horizontal slab index    (k_end can be less than k_start, meaning there is
!                                                   no work on this processor)
!                 NOTE: above args allow horizontal localization length to vary in vertical
!
!   output argument list:
!     z        - spectral space variable
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens,sp_loc,p_sploc2ens,grd_sploc

  use hybrid_ensemble_parameters,only: use_localization_grid
  use egrid2agrid_mod,only: g_egrid2agrid_ad
  implicit none

  integer(i_kind),intent(in   ) :: ig
  integer(i_kind),intent(in   ) :: k_start,k_end
  real(r_kind)   ,intent(inout) :: z(sp_loc%nc,k_start:max(k_start,k_end))
  real(r_kind)   ,intent(inout) :: f(grd_ens%nlat,grd_ens%nlon,k_start:max(k_start,k_end))

  real(r_kind) g(sp_loc%nc)
  real(r_kind) work(grd_sploc%nlat,grd_sploc%nlon,1)
  integer(i_kind) k
  logical vector(k_start:max(k_start,k_end))

  if(.not.use_localization_grid) then

!$omp parallel do schedule(static,1) private(k,g)
    do k=k_start,k_end
       call general_s2g0_ad(grd_ens,sp_loc,g,f(:,:,k))
       z(:,k)=g(:)*sqrt_spectral_filter(ig,:,k_index(k))
    enddo

  else

     vector=.false.
!$omp parallel do schedule(static,1) private(k,g,work)
     do k=k_start,k_end
        call g_egrid2agrid_ad(p_sploc2ens,work,f(:,:,k:k),k,k,vector(k:k))
        call general_s2g0_ad(grd_sploc,sp_loc,g,work)
        z(:,k)=g(:)*sqrt_spectral_filter(ig,:,k_index(k))
     enddo

  end if
  return

end subroutine sqrt_sf_xy_ad


subroutine get_new_alpha_beta(aspect,ng,fmat_out,fmat0_out)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    get_new_alpha_beta
!
!   prgrmmr:
!
! abstract:  compute various constants for new factorization Purser 1-d high-order filter.
!            adapted as simplification from new_alpha_betaa4 in raflib.f90 for use with
!            simple homogeneous isotropic localization filter.
!
! program history log:
!   2009-09-28  parrish  initial documentation
!
!   input argument list:
!     aspect   - squared correlation scale, in grid units squared
!     ng       - length of string
!     m        - filter order
!
!   output argument list:
!     fmat_out,fmat0_out - filter parameters for special factorization
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_double
  use constants, only: one
  use raflib, only: stringop
  implicit none

  integer(i_kind)            , intent(in   ) :: ng
  real(r_kind), dimension(ng), intent(in   ) :: aspect
  real(r_kind)               , intent(  out) :: fmat_out(2,ng,2),fmat0_out(ng,2)

  integer(i_kind) i
  real(r_double) sig(0:ng-1),fmat(0:ng-1,-2:0,2)

  do i=1,ng
     sig(i-1)=sqrt(aspect(i))
  enddo
  call stringop(ng-1,sig,fmat)

  do i=1,ng
     fmat_out(2,i,1)=fmat(i-1,-2,1)
     fmat_out(1,i,1)=fmat(i-1,-1,1)
     fmat0_out(i,1)=one/fmat(i-1,0,1)
     fmat_out(2,i,2)=fmat(i-1,-2,2)
     fmat_out(1,i,2)=fmat(i-1,-1,2)
     fmat0_out(i,2)=one/fmat(i-1,0,2)
  enddo
  return

end subroutine get_new_alpha_beta

subroutine bkerror_a_en(grady)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkerror_a_en  copy of bkerror for hybrid ensemble          
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: copy of bkerror for applying localization recursive filter
!            to hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish  initial creation of code from a copy of bkerror
!   2010-05-20  todling  update to use bundle
!   2022-09-15  yokota   add scale/variable/time-dependent localization
!
!   input argument list:
!     grady    - input field  
!
!   output
!     grady    - background structure * grady 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gsi_4dvar, only: nsubwin
  use control_vectors, only: control_vector
  use timermod, only: timer_ini,timer_fnl
  use hybrid_ensemble_parameters, only: n_ens
  use hybrid_ensemble_parameters, only: naensgrp
  use hybrid_ensemble_parameters, only: alphacvarsclgrpmat
  use gsi_bundlemod,only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: grady

! Declare local variables
  integer(i_kind) ii,ip,istatus,k,ig,ig2
  real(r_kind),allocatable,dimension(:,:) :: z
  real(r_kind),allocatable,dimension(:) :: z2

! Initialize timer
  call timer_ini('bkerror_a_en')

! Put things in grady first since operations change input variables
  call gsi_bundlegetpointer ( grady%aens(1,1,1),'a_en',ip,istatus)
  if(istatus/=0) then
     write(6,*)'bkerror_a_en: trouble getting pointer to ensemble CV'
     call stop2(317)
  endif

!  multiply by sqrt_beta_e_mult
  call sqrt_beta_e_mult(grady)

! Apply variances, as well as vertical & horizontal parts of background error
  if (naensgrp==1) then
     do ii=1,nsubwin
        call bkgcov_a_en_new_factorization(1,grady%aens(ii,1,1:n_ens))
     end do
  else
     allocate(z(nval_loc_en,naensgrp))
     allocate(z2(nval_loc_en))
     do ii=1,nsubwin
        do ig=1,naensgrp
           call ckgcov_a_en_new_factorization_ad(ig,z(1,ig),grady%aens(ii,ig,1:n_ens))
        enddo
        do ig=1,naensgrp
           z2=zero
           do ig2=1,naensgrp
              do k=1,nval_loc_en
                 z2(k) = z2(k) + z(k,ig2) * alphacvarsclgrpmat(ig,ig2)  
              enddo
           enddo
           call ckgcov_a_en_new_factorization(ig,z2,grady%aens(ii,ig,1:n_ens))
        enddo
     enddo
     deallocate(z,z2)
  endif

!  multiply by sqrt_beta_e_mult
  call sqrt_beta_e_mult(grady)

! Finalize timer
  call timer_fnl('bkerror_a_en')

  return
end subroutine bkerror_a_en

subroutine bkgcov_a_en_new_factorization(ig,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgcov_a_en copy of bkgcov for hybrid ens var a_en 
!   prgmmr: parrish        org: np22                date: 2009-09-17
!
! abstract: copy of bkgcov to apply localization with recursive filters
!            to hybrid ensemble control variable a_en.
!
! program history log:
!   2009-09-17  parrish
!   2010-02-20  parrish, adapt for dual resolution
!   2010-05-21  todling, update to use bundle
!   2010-07-02  parrish, modify arguments to call sf_xy to allow for vertical variation
!                 of horizontal localization length
!
!   input argument list:
!     ig       - number for smoothing scales
!     a_en     - control variable for ensemble contribution to background error
!     nlevs    - number of vertical levels for smoothing
!
!   output argument list:
!                 all after smoothing, combining scales
!     a_en     - control variable for ensemble contribution to background error
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: regional
  use hybrid_ensemble_parameters, only: n_ens,grd_loc
  use hybrid_ensemble_parameters, only: l_mgbf_loc,naensgrp
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants, only: zero

  implicit none

! Passed Variables
! real(r_kind),dimension(grd_loc%latlon1n,n_ens),intent(inout) :: a_en
  integer(i_kind),intent(in   ) :: ig
  type(gsi_bundle),intent(inout) :: a_en(n_ens)

! Local Variables
  integer(i_kind) ii,k,iadvance,iback,is,ie,ipnt,istatus
  real(r_kind) hwork(grd_loc%inner_vars,grd_loc%nlat,grd_loc%nlon,grd_loc%kbegin_loc:grd_loc%kend_alloc)
  real(r_kind),allocatable,dimension(:):: a_en_work

  ipnt=1

! MGBF-based localization (now available only in regional=.true.)
!  (Note that MGBF is applied only in ig<=naensgrp
!   because recursive filter is applied for ig>naensgrp
!   to separate scales for scale-dependent localization
!   even in MGBF-based localization)
  if(l_mgbf_loc.and.ig<=naensgrp) then

! Apply vertical smoother on each ensemble member
     allocate(work_mgbf(obj_mgbf(1)%km_a_all,obj_mgbf(1)%nm,obj_mgbf(1)%mm))
     work_mgbf=zero
     iadvance=1 ; iback=2
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        ii=(k-1)*grd_loc%nsig
        is=ii+1
        ie=ii+grd_loc%nsig
        if(.not.obj_mgbf(1)%l_vertical_filter) call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,1)
        call map_work_mgbf(a_en(k)%r3(ipnt)%q,work_mgbf(is:ie,:,:),iadvance,1)
     enddo

! Mapping from analysis grid to filter grid
     call obj_mgbf(1)%anal_to_filt_allmap(work_mgbf)

! Apply horizontal smoother for number of horizontal scales
     call obj_mgbf(1)%filtering_procedure(obj_mgbf(1)%mgbf_proc,0)

! Mapping from filter grid to analysis grid
     call obj_mgbf(1)%filt_to_anal_allmap(work_mgbf)

! Apply vertical smoother on each ensemble member
     iadvance=2 ; iback=1
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        ii=(k-1)*grd_loc%nsig
        is=ii+1
        ie=ii+grd_loc%nsig
        call map_work_mgbf(a_en(k)%r3(ipnt)%q,work_mgbf(is:ie,:,:),iadvance,1)
        if(.not.obj_mgbf(1)%l_vertical_filter) call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,1)
     enddo
     deallocate(work_mgbf)

! Recursive/Spectral filter-based localization(ig<=naensgrp)
! or scale-separation(ig>naensgrp)
  else

! Apply vertical smoother on each ensemble member
! To avoid my having to touch the general sub2grid and grid2sub,
! get copy for ensemble components to work array
     allocate(a_en_work(n_ens*a_en(1)%ndim),stat=istatus)
     if(istatus/=0) then
        write(6,*)'bkgcov_a_en_new_factorization: trouble in alloc(a_en_work)'
        call stop2(999)
     endif
     iadvance=1 ; iback=2
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)
        ii=(k-1)*a_en(1)%ndim
        is=ii+1
        ie=ii+a_en(1)%ndim
        a_en_work(is:ie)=a_en(k)%values(1:a_en(k)%ndim)
     enddo

! Convert from subdomain to full horizontal field distributed among processors
     call general_sub2grid(grd_loc,a_en_work,hwork)

! Apply horizontal smoother for number of horizontal scales
     if(regional) then
        iadvance=1 ; iback=2
        call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
        call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
        iadvance=2 ; iback=1
        call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
        call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
     else
        call sf_xy(ig,hwork,grd_loc%kbegin_loc,grd_loc%kend_loc)
     end if

! Put back onto subdomains
     call general_grid2sub(grd_loc,hwork,a_en_work)

! Retrieve ensemble components from long vector
! Apply vertical smoother on each ensemble member
     iadvance=2 ; iback=1
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        ii=(k-1)*a_en(1)%ndim
        is=ii+1
        ie=ii+a_en(1)%ndim
        a_en(k)%values(1:a_en(k)%ndim)=a_en_work(is:ie)
        call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)
     enddo
     deallocate(a_en_work)

  endif

  return
end subroutine bkgcov_a_en_new_factorization

subroutine ckgcov_a_en_new_factorization(ig,z,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov_a_en_new_factorization sqrt(bkgcov_a_en_new_factorization)
!   prgmmr: parrish        org: np22                date: 2011-06-27
!
! abstract: make a copy of bkgcov_a_en_new_factorization and form sqrt.
!
! program history log:
!   2011-06-27  parrish, initial documentation
!
!   input argument list:
!     ig       - number for smoothing scales
!     z        - long vector containing sqrt control vector for ensemble extended control variable
!
!   output argument list:
!     a_en     - bundle containing intermediate control variable after multiplication by sqrt(S), the
!                    ensemble localization correlation.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional
  use hybrid_ensemble_parameters, only: n_ens,grd_loc
  use hybrid_ensemble_parameters, only: l_mgbf_loc
  use general_sub2grid_mod, only: general_grid2sub
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Passed Variables
  integer(i_kind),intent(in   ) :: ig
  type(gsi_bundle),intent(inout) :: a_en(n_ens)
  real(r_kind),dimension(nval_loc_en),intent(in   ) :: z
!NOTE:
! nval_loc_en is the number of horizontally-filtered variables in the domain of each processor.
! In MGBF-based localization, it is horizontally-local and vertically-global as
!   nval_loc_en = nhoriz * obj_mgbf(ig)%km_all
!      and nhoriz = ( obj_mgbf(ig)%im + obj_mgbf(ig)%hx*2 ) * ( obj_mgbf(ig)%jm + obj_mgbf(ig)%hy*2 )
! In recursive/spectral filter-based localization, it is horizontally-global and vertically-local as
!   nval_loc_en = nhoriz * ( grd_loc%kend_alloc - grd_loc%kbegin_loc + 1 )
!      and nhoriz =  grd_loc%nlat     *  grd_loc%nlon     (for regional recursive filter)
!          nhoriz = ( sp_loc%jcap+1 ) * ( sp_loc%jcap+2 ) (for global spectral filter)
! but internal array hwork always has
! dimension grd_loc%nlat * grd_loc%nlon * ( grd_loc%kend_alloc - grd_loc%kbegin_loc + 1 )
! which would be used as nval_loc_en when the recursive filter is used.

! Local Variables
  integer(i_kind) ii,i,j,k,iadvance,iback,is,ie,ipnt,istatus
  real(r_kind) hwork(grd_loc%nlat*grd_loc%nlon*(grd_loc%kend_alloc-grd_loc%kbegin_loc+1))
  real(r_kind),allocatable,dimension(:):: a_en_work

  call gsi_bundlegetpointer(a_en(1),'a_en',ipnt,istatus)
  if(istatus/=0) then
     write(6,*)'ckgcov_a_en_new_factorization: trouble getting pointer to ensemble CV'
     call stop2(999)
  endif

! MGBF-based localization (now available only in regional=.true.)
  if(l_mgbf_loc) then

! Apply horizontal smoother for number of horizontal scales
     ii=0
     do k=1,obj_mgbf(ig)%km_all
        do j=1-obj_mgbf(ig)%hy,obj_mgbf(ig)%jm+obj_mgbf(ig)%hy
           do i=1-obj_mgbf(ig)%hx,obj_mgbf(ig)%im+obj_mgbf(ig)%hx
              ii=ii+1
              obj_mgbf(ig)%VALL(k,i,j)=z(ii)
           enddo
        enddo
     enddo
     call obj_mgbf(ig)%filtering_procedure(obj_mgbf(ig)%mgbf_proc,1)

! Mapping from filter grid to analysis grid
     allocate(work_mgbf(obj_mgbf(ig)%km_a_all,obj_mgbf(ig)%nm,obj_mgbf(ig)%mm))
     work_mgbf=zero
     call obj_mgbf(ig)%filt_to_anal_allmap(work_mgbf)

! Apply vertical smoother on each ensemble member
     iadvance=2 ; iback=1
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        ii=(k-1)*grd_loc%nsig
        is=ii+1
        ie=ii+grd_loc%nsig
        call map_work_mgbf(a_en(k)%r3(ipnt)%q,work_mgbf(is:ie,:,:),iadvance,ig)
        if(.not.obj_mgbf(ig)%l_vertical_filter) call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)
     enddo
     deallocate(work_mgbf)

! Recursive/Spectral filter-based localization
  else

     if(grd_loc%kend_loc+1-grd_loc%kbegin_loc==0) then
!     no work to be done on this processor, but hwork still has allocated space, since
!                     grd_loc%kend_alloc = grd_loc%kbegin_loc in this case, so set to zero.
        hwork=zero
     else
! Apply horizontal smoother for number of horizontal scales
        if(regional) then
! Make a copy of input variable z to hwork
           hwork=z
           iadvance=2 ; iback=1
           call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
           call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
        else
#ifdef LATER
           call sqrt_sf_xy(ig,z,hwork,grd_loc%kbegin_loc,grd_loc%kend_loc)
#else
           write(6,*) ' problem with ibm compiler with "use hybrid_ensemble_isotropic, only: sqrt_sf_xy"'
#endif /*LATER*/
        end if
     end if

! Put back onto subdomains
     allocate(a_en_work(n_ens*a_en(1)%ndim),stat=istatus)
     if(istatus/=0) then
        write(6,*)'ckgcov_a_en_new_factorization: trouble in alloc(a_en_work)'
        call stop2(999)
     endif
     call general_grid2sub(grd_loc,hwork,a_en_work)

! Retrieve ensemble components from long vector
     ii=0
     do k=1,n_ens
        is=ii+1
        ie=ii+a_en(1)%ndim
        a_en(k)%values(1:a_en(k)%ndim)=a_en_work(is:ie)
        ii=ii+a_en(1)%ndim
     enddo
     deallocate(a_en_work)

! Apply vertical smoother on each ensemble member
     iadvance=2 ; iback=1
!$omp parallel do schedule(static,1) private(k)
     do k=1,n_ens

        call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)

     enddo

  endif

  return
end subroutine ckgcov_a_en_new_factorization

subroutine ckgcov_a_en_new_factorization_ad(ig,z,a_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov_a_en_new_factorization_ad adjoint of ckgcov_a_en_new_factorization
!   prgmmr: parrish        org: np22                date: 2011-06-27
!
! abstract: adjoint of ckgcov_a_en_new_factorization.  Calling ckgcov_a_en_new_factorization_ad,
!            followed by ckgcov_a_en_new_factorization is the equivalent of one call to
!            subroutine bkgcov_a_en_new_factorization.
!
! program history log:
!   2011-06-27  parrish, initial documentation
!
!   input argument list:
!     ig       - number for smoothing scales
!     z        - long vector containing sqrt control vector for ensemble extended control variable
!     a_en     - bundle containing intermediate control variable after multiplication by sqrt(S), the
!                    ensemble localization correlation.
!
!   output argument list:
!     z        - long vector containing sqrt control vector for ensemble extended control variable
!     a_en     - bundle containing intermediate control variable after multiplication by sqrt(S), the
!                    ensemble localization correlation.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional
  use hybrid_ensemble_parameters, only: n_ens,grd_loc
  use hybrid_ensemble_parameters, only: l_mgbf_loc
  use general_sub2grid_mod, only: general_sub2grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Passed Variables
  integer(i_kind),intent(in   ) :: ig
  type(gsi_bundle),intent(inout) :: a_en(n_ens)
  real(r_kind),dimension(nval_loc_en),intent(inout) :: z
!NOTE:
! nval_loc_en is the number of horizontally-filtered variables in the domain of each processor.
! In MGBF-based localization, it is horizontally-local and vertically-global as
!   nval_loc_en = nhoriz * obj_mgbf(ig)%km_all
!      and nhoriz = ( obj_mgbf(ig)%im + obj_mgbf(ig)%hx*2 ) * ( obj_mgbf(ig)%jm + obj_mgbf(ig)%hy*2 )
! In recursive/spectral filter-based localization, it is horizontally-global and vertically-local as
!   nval_loc_en = nhoriz * ( grd_loc%kend_alloc - grd_loc%kbegin_loc + 1 )
!      and nhoriz =  grd_loc%nlat     *  grd_loc%nlon     (for regional recursive filter)
!          nhoriz = ( sp_loc%jcap+1 ) * ( sp_loc%jcap+2 ) (for global spectral filter)
! but internal array hwork always has
! dimension grd_loc%nlat * grd_loc%nlon * ( grd_loc%kend_alloc - grd_loc%kbegin_loc + 1 )
! which would be used as nval_loc_en when the recursive filter is used.

! Local Variables
  integer(i_kind) ii,i,j,k,iadvance,iback,is,ie,ipnt,istatus
  real(r_kind) hwork(grd_loc%nlat*grd_loc%nlon*(grd_loc%kend_alloc-grd_loc%kbegin_loc+1))
  real(r_kind),allocatable,dimension(:):: a_en_work

  call gsi_bundlegetpointer(a_en(1),'a_en',ipnt,istatus)
  if(istatus/=0) then
     write(6,*)'ckgcov_a_en_new_factorization_ad: trouble getting pointer to ensemble CV'
     call stop2(999)
  endif

! MGBF-based localization (now available only in regional=.true.)
  if(l_mgbf_loc) then

! Apply vertical smoother on each ensemble member
     allocate(work_mgbf(obj_mgbf(ig)%km_a_all,obj_mgbf(ig)%nm,obj_mgbf(ig)%mm))
     work_mgbf=zero
     iadvance=1 ; iback=2
!$omp parallel do schedule(static,1) private(k,ii,is,ie)
     do k=1,n_ens
        ii=(k-1)*grd_loc%nsig
        is=ii+1
        ie=ii+grd_loc%nsig
        if(.not.obj_mgbf(ig)%l_vertical_filter) call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)
        call map_work_mgbf(a_en(k)%r3(ipnt)%q,work_mgbf(is:ie,:,:),iadvance,ig)
     enddo

! Mapping from analysis grid to filter grid
     call obj_mgbf(ig)%anal_to_filt_allmap(work_mgbf)
     deallocate(work_mgbf)

! Apply horizontal smoother for number of horizontal scales
     call obj_mgbf(ig)%filtering_procedure(obj_mgbf(ig)%mgbf_proc,-1)
     ii=0
     do k=1,obj_mgbf(ig)%km_all
        do j=1-obj_mgbf(ig)%hy,obj_mgbf(ig)%jm+obj_mgbf(ig)%hy
           do i=1-obj_mgbf(ig)%hx,obj_mgbf(ig)%im+obj_mgbf(ig)%hx
              ii=ii+1
              z(ii)=obj_mgbf(ig)%VALL(k,i,j)
           enddo
        enddo
     enddo

! Recursive/Spectral filter-based localization
  else

! Apply vertical smoother on each ensemble member
     iadvance=1 ; iback=2
!$omp parallel do schedule(static,1) private(k)
     do k=1,n_ens

        call new_factorization_rf_z(a_en(k)%r3(ipnt)%q,iadvance,iback,ig)

     enddo

! To avoid my having to touch the general sub2grid and grid2sub,
! get copy for ensemble components to work array
     allocate(a_en_work(n_ens*a_en(1)%ndim),stat=istatus)
     if(istatus/=0) then
        write(6,*)'ckgcov_a_en_new_factorization_ad: trouble in alloc(a_en_work)'
        call stop2(999)
     endif
     ii=0
     do k=1,n_ens
        is=ii+1
        ie=ii+a_en(1)%ndim
        a_en_work(is:ie)=a_en(k)%values(1:a_en(k)%ndim)
        ii=ii+a_en(1)%ndim
     enddo

! Convert from subdomain to full horizontal field distributed among processors
     call general_sub2grid(grd_loc,a_en_work,hwork)
     deallocate(a_en_work)

     if(grd_loc%kend_loc+1-grd_loc%kbegin_loc==0) then
!     no work to be done on this processor, but z still has allocated space, since
!                     grd_loc%kend_alloc = grd_loc%kbegin_loc in this case, so set to zero.
        z=zero
     else
! Apply horizontal smoother for number of horizontal scales
        if(regional) then
           iadvance=1 ; iback=2
           call new_factorization_rf_x(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
           call new_factorization_rf_y(hwork,iadvance,iback,grd_loc%kend_loc+1-grd_loc%kbegin_loc,ig)
           z=hwork
        else
           call sqrt_sf_xy_ad(ig,z,hwork,grd_loc%kbegin_loc,grd_loc%kend_loc)
        end if
     end if

  endif

  return
end subroutine ckgcov_a_en_new_factorization_ad

subroutine map_work_mgbf(f,g,iadvance,ig)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    map_work_mgbf
!   prgrmmr:  yokota         org: NCEP/EMC                          date: 2024-02-20
!
! abstract:  mapping field for MGBF
!
! program history log:
!
!   input argument list:
!     f        - field to be filtered
!     g        - field for MGBF
!     iadvance - =1  to map from f to g, =2 to map from g to f
!     ig       - number for smoothing scales
!
!   output argument list:
!     f        - field to be filtered
!     g        - field for MGBF
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use constants, only: zero
  use hybrid_ensemble_parameters, only: grd_loc
  implicit none

  integer(i_kind),intent(in   ) :: iadvance,ig
  real(r_kind)   ,intent(inout) :: f(grd_loc%lat2,grd_loc%lon2,grd_loc%nsig)
  real(r_kind)   ,intent(inout) :: g(grd_loc%nsig,obj_mgbf(ig)%nm,obj_mgbf(ig)%mm)

  real(r_kind) :: work_tmp(grd_loc%lon2,grd_loc%lat2)
  integer(i_kind) i,j,k

  if(iadvance == 1) then
     do k=1,grd_loc%nsig
        do j=1,grd_loc%lat2
           do i=1,grd_loc%lon2
              work_tmp(i,j)=f(j,i,k)
           enddo
        enddo
        do j=1,obj_mgbf(ig)%mm
           do i=1,obj_mgbf(ig)%nm
              g(k,i,j)=work_tmp(i+1,j+1)
           enddo
        enddo
     enddo
  elseif(iadvance == 2) then
     do k=1,grd_loc%nsig
        work_tmp=zero
        do j=1,obj_mgbf(ig)%mm
           do i=1,obj_mgbf(ig)%nm
              work_tmp(i+1,j+1)=g(k,i,j)
           enddo
        enddo
        do j=1,grd_loc%lat2
           do i=1,grd_loc%lon2
              f(j,i,k)=work_tmp(i,j)
           enddo
        enddo
     enddo
  endif
  return

end subroutine map_work_mgbf

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

subroutine hybens_grid_setup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hybrid_ensemble_setup  initialize everything for hybrid ensemble
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: put everything for initializing hybrid ensemble in one subroutine.
!
! program history log:
!   2009-09-17  parrish
!   2010-02-20  parrish, adapt for dual resolution
!   2011-01-30  parrish, fix so regional application depends only on parameters regional
!                  and dual_res.  Rename subroutine get_regional_gefs_grid to get_regional_dual_res_grid.
! 
!   2022-03-01  X.Lu & X.Wang - add vars for hafs dual ens.  POC: xuguang.wang@ou.edu
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: aniso_a_en,n_ens,&
                      nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,&
                      grd_ens,grd_loc,grd_a1,grd_e1,grd_anl,sp_ens,p_e2a,&
                      dual_res,uv_hyb_ens,grid_ratio_ens
  use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens,&
                                        region_dx_ens,region_dy_ens
  use gridmod,only: regional,nsig,nlon,nlat,rlats,rlons,use_sp_eqspace
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use general_specmod, only: general_init_spec_vars
  use egrid2agrid_mod,only: g_create_egrid2agrid,create_egrid2agrid
  use mpimod, only: mype
  use constants, only: zero,one
  use control_vectors, only: cvars3d,nc2d,nc3d
  use gridmod, only: region_lat,region_lon,region_dx,region_dy
  use hybrid_ensemble_parameters, only:nsclgrp,spc_multwgt,spcwgt_params,global_spectral_filter_sd
  use hybrid_ensemble_parameters, only:regional_ensemble_option
  use gsi_rfv3io_mod,only:gsi_rfv3io_get_ens_grid_specs

  implicit none

  integer(i_kind) inner_vars,num_fields,ic3,k
  integer(i_kind) nord_e2a
  logical,allocatable::vector(:)
  real(r_kind) eps,r_e
  real(r_kind) rlon_a(nlon),rlat_a(nlat),rlon_e(nlon),rlat_e(nlat)
  character(:),allocatable:: fv3_ens_spec_grid_filename
  integer :: ierr

  nord_e2a=4       !   soon, move this to hybrid_ensemble_parameters

  if(aniso_a_en) then
     if(mype == 0) write(6,*)' anisotropic option not available yet for hybrid ensemble localization'
     if(mype >  -10) then
        call stop2(999)
        stop
     end if
  end if

  eps=zero
  r_e=grid_ratio_ens

! Initialize dual_res logical
  if (.not.regional) then
    dual_res = (nlon /= nlon_ens .or. nlat /= nlat_ens)
  else
    dual_res=.false.
  end if
  if(mype==0) write(6,*)' before compute nlat_ens,nlon_ens, nlat,nlon,nlat_ens,nlon_ens,r_e,eps=',&
                          nlat,nlon,nlat_ens,nlon_ens,r_e,eps

!  if regional set up for possible dual-res application:

  if(regional) then
     call get_regional_dual_res_grid(eps,r_e,nlon,nlon_ens,rlon_a,rlon_e)
     call get_regional_dual_res_grid(eps,r_e,nlat,nlat_ens,rlat_a,rlat_e)
     call create_egrid2agrid(nlat,rlat_a,nlon,rlon_a,nlat_ens,rlat_e,nlon_ens,rlon_e,nord_e2a,p_e2a)
     dual_res=.not.p_e2a%identity
!  NOTE:  key dual-res on egrid2agrid parameter p_e2a%identity, not nlat_ens==nlat .or. nlon_ens==nlon
     allocate(region_lat_ens(nlat_ens,nlon_ens))
     allocate(region_lon_ens(nlat_ens,nlon_ens))
     allocate(region_dx_ens(nlat_ens,nlon_ens))
     allocate(region_dy_ens(nlat_ens,nlon_ens))
     if(dual_res) then
        call get_region_lat_lon_ens(region_lat_ens,region_lon_ens, &
                                       rlat_e,rlon_e,nlat_ens,nlon_ens)
     else
        region_lon_ens=region_lon
        region_lat_ens=region_lat
     end if
  end if
  if(mype==0) write(6,*)' dual_res,nlat,nlon,nlat_ens,nlon_ens,r_e,eps=',&
                               dual_res,nlat,nlon,nlat_ens,nlon_ens,r_e,eps

  if(nlon_ens<=0 .or. nlat_ens<=0) then
     nlon_ens=nlon ; nlat_ens=nlat
  end if

!     2.  create grid info for ensemble, including stuff for ensemble general_sub2grid, general_grid2sub
  num_fields=nsig*n_ens
  inner_vars=1
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_loc,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  num_fields=max(0,nc3d)*nsig+max(0,nc2d)
  deallocate(vector)
  allocate(vector(num_fields))
  vector=.false.
  do ic3=1,nc3d
     if(trim(cvars3d(ic3))=='sf'.or.trim(cvars3d(ic3))=='vp') then
        do k=1,nsig
           vector((ic3-1)*nsig+k)=uv_hyb_ens
        enddo
     end if
  enddo
  call general_sub2grid_create_info(grd_ens,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  call general_sub2grid_create_info(grd_anl,inner_vars,nlat,nlon,nsig,num_fields,regional,vector)
  deallocate(vector)
  num_fields=nsig
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_e1,inner_vars,nlat_ens,nlon_ens,nsig,num_fields,regional,vector)
  call general_sub2grid_create_info(grd_a1,inner_vars,nlat,nlon,nsig,num_fields,regional,vector)
  deallocate(vector)

!    setup dual-resolution feature, if needed

  if(.not.regional) then
     call general_init_spec_vars(sp_ens,jcap_ens,jcap_ens_test,grd_ens%nlat,grd_ens%nlon,eqspace=use_sp_eqspace)
     call g_create_egrid2agrid(nlat,rlats,nlon,rlons,grd_ens%nlat,sp_ens%rlats,grd_ens%nlon,sp_ens%rlons, &
                               nord_e2a,p_e2a,.true.,eqspace=use_sp_eqspace)
  else
     if(dual_res) then
        call get_region_dx_dy_ens(region_dx_ens,region_dy_ens)
        if(regional_ensemble_option == 5) then
           fv3_ens_spec_grid_filename="fv3_ens_grid_spec"
           call gsi_rfv3io_get_ens_grid_specs(fv3_ens_spec_grid_filename,ierr)
        endif
     else
        region_dx_ens=region_dx
        region_dy_ens=region_dy
     end if
  end if

  if(global_spectral_filter_sd .and. nsclgrp > 1)then
     allocate(spc_multwgt(sp_ens%nc,nsclgrp))
     allocate(spcwgt_params(4,nsclgrp))
     spc_multwgt=1.0

     ! The below parameters are used in Huang et al. (2021, MWR)
     spcwgt_params(1,1)=4000.0_r_kind
     spcwgt_params(2,1)=100000000.0_r_kind
     spcwgt_params(3,1)=1.0_r_kind
     spcwgt_params(4,1)=3000.0_r_kind

     if( nsclgrp >=3 )then
       spcwgt_params(1,3)=0.0_r_kind
       spcwgt_params(2,3)=500.0_r_kind
       spcwgt_params(3,3)=1.0_r_kind
       spcwgt_params(4,3)=500.0_r_kind
     end if

     call init_mult_spc_wgts(jcap_ens)

  end if

  return
end subroutine hybens_grid_setup

subroutine hybens_localization_setup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hybens_localization_setup   setup for hybrid localization
!   prgmmr: kleist          org: np22                date: 2010-07-30
!
! abstract: setup arrays used for control variable localization
!
! program history log:
!   2010-07-30  kleist
!   2011-10-03  wu - add call to setup_ens_wgt, which computes vertical weighting for ensemble contribution
!                     to psfc.
!   12-05-2012  el akkraoui  hybrid beta parameters now vertically varying
!   2012-10-16  wu - only call setup_ens_wgt if necessary
!   2014-05-22  wu  modification to allow vertically varying localization scales in regional
!   2022-09-15  yokota - add scale/variable/time-dependent localization
!   2022-12-09  Y. Wang and X. Wang - add a variable-dependent localization option (assign_vdl_nml=.true.),
!                                     poc: xuguang.wang@ou.edu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,i_kind
   use constants, only: one,zero
   use mpimod, only: mype
   use gridmod,only: regional
   use gfs_stratosphere, only: use_gfs_stratosphere,blend_rm
   use hybrid_ensemble_parameters, only: grd_ens,jcap_ens,n_ens,grd_loc,sp_loc,&
                                         nval_lenz_en,regional_ensemble_option
   use hybrid_ensemble_parameters, only: readin_beta,beta_s,beta_e,beta_s0,beta_e0,sqrt_beta_s,sqrt_beta_e
   use hybrid_ensemble_parameters, only: readin_localization,create_hybens_localization_parameters, &
                                         vvlocal,s_ens_h,s_ens_hv,s_ens_v,s_ens_vv
   use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp,naensloc,ntlevs_ens,nsclgrp,assign_vdl_nml
   use hybrid_ensemble_parameters, only: en_perts,vdl_scale,vloc_varlist,global_spectral_filter_sd
   use hybrid_ensemble_parameters, only: ngvarloc
   use hybrid_ensemble_parameters, only: l_mgbf_loc
   use gsi_io, only: verbose
   use string_utility, only: StrLowCase

   implicit none

   integer(i_kind),parameter   :: lunin = 47
   character(len=40),parameter :: fname = 'hybens_info'
   integer(i_kind) :: k,msig,istat,nz,kl
   integer(i_kind) :: n,nk,m,ig,ic3,ic2,ipic,istatus,ii
   integer(i_kind) :: ipc3d(nc3d),ipc2d(nc2d)
   logical         :: lexist,print_verbose
   real(r_kind),allocatable:: s_ens_h_gu_x(:,:),s_ens_h_gu_y(:,:)
   logical :: l_read_success
   type(gsi_bundle) :: a_en(n_ens)
   type(gsi_bundle),allocatable :: en_pertstmp(:,:),en_pertstmp1(:,:)
   type(gsi_grid)  :: grid_ens
   real(r_kind), pointer :: values(:) => NULL()
   integer(i_kind) :: iscl, iv, smooth_scales_num
   character(len=*),parameter::myname_=myname//'*hybens_localization_setup'
   character(len=40) :: mgbfname='mgbf_locXX.nml'

   l_read_success=.false.
   print_verbose=.false. .and. mype == 0
   if(verbose .and. mype == 0)print_verbose=.true.

   ! Allocate
   call create_hybens_localization_parameters

   if ( readin_localization .or. readin_beta ) then ! read info from file

      inquire(file=trim(fname),exist=lexist)
      if ( lexist ) then 
         open(lunin,file=trim(fname),form='formatted')
         rewind(lunin)
         read(lunin,100,iostat=istat) msig
         if ( istat /= 0 ) then
            write(6,*) 'HYBENS_LOCALIZATION_SETUP:  ***ERROR*** error in ',trim(fname)
            write(6,*) 'HYBENS_LOCALIZATION_SETUP:  error reading file, iostat = ',istat
            call stop2(123)
         endif
         if ( msig /= grd_ens%nsig ) then 
            write(6,*) 'HYBENS_LOCALIZATION_SETUP:  ***ERROR*** error in ',trim(fname)
            write(6,*) 'HYBENS_LOCALIZATION_SETUP:  levels do not match,msig[read in],nsig[defined] = ',msig,grd_ens%nsig
            close(lunin)
            call stop2(123)
         endif
         if(mype==0) write(6,'(" LOCALIZATION, BETA_S, BETA_E VERTICAL PROFILES FOLLOW")')
         do k = 1,grd_ens%nsig
            read(lunin,101) s_ens_hv(k,1), s_ens_vv(k,1), beta_s(k), beta_e(k)
            if(mype==0) write(6,101) s_ens_hv(k,1), s_ens_vv(k,1), beta_s(k), beta_e(k)
         enddo
         do ig=2,naensloc
            do k = 1,grd_ens%nsig
               read(lunin,101,end=300) s_ens_hv(k,ig),s_ens_vv(k,ig)
            enddo
         enddo
         l_read_success=.true.
         close(lunin)
300      continue
         if(.not.l_read_success) then
            do ig=2,naensloc
               do k = 1,grd_ens%nsig
                  s_ens_hv(k,ig)=s_ens_hv(k,1)
                  s_ens_vv(k,ig)=s_ens_vv(k,1)
               enddo
            enddo
         endif

      else

         write(6,*) 'HYBENS_LOCALIZATION_SETUP:  ***ERROR*** INPUT FILE MISSING -- ',trim(fname)
         call stop2(999)

      endif

      if ( readin_localization ) then
         vvlocal = .true.
         nz = msig
         kl = grd_loc%kend_alloc-grd_loc%kbegin_loc+1
      endif


   endif ! if ( readin_localization .or. readin_beta )

100 format(I4)
!101 format(F8.1,3x,F5.1,2(3x,F8.4))
101 format(F8.1,3x,F8.3,F8.4,3x,F8.4)

   if ( .not. readin_beta ) then ! assign all levels to same value, sum = 1.0
      beta_s = beta_s0
      if (beta_e0 < 0) then
         beta_e = one - beta_s0
      else
         beta_e = beta_e0
      endif
   endif

   if ( regional_ensemble_option == 2 .and. use_gfs_stratosphere .and. .not. readin_beta ) then
      do k = 1,grd_ens%nsig
         beta_e(k) = beta_e(k) * blend_rm(k)
         beta_s(k) = one - beta_e(k)
         if (print_verbose) write(6,*)'beta_s, beta_e=', &
                          k,beta_s(k),beta_e(k)
      enddo
   endif

   if ( .not. readin_localization ) then ! assign all levels to same value, s_ens_h, s_ens_v
      nz = 1
      kl = 1
      do ig=1,naensloc
         s_ens_hv(:,ig) = s_ens_h(ig)
         s_ens_vv(:,ig) = s_ens_v(ig)
      enddo
   endif

   ! Set up localization filters

   call init_rf_z(s_ens_vv)
   call normal_new_factorization_rf_z

   if ( regional ) then ! convert s_ens_h from km to grid units.
      if ( l_mgbf_loc ) then
         allocate(obj_mgbf(naensgrp))
         do ig=1,naensgrp
            write(mgbfname(9:10),'(i2.2)') ig
            call obj_mgbf(ig)%mg_initialize(trim(mgbfname))
         enddo
      endif
      ! Even for MGBF-localization, recursive filter is applied for scale-separation
      ! in scale-dependent localization, so init_rf_[xy] should be called in nsclgrp>1
      if( .not. l_mgbf_loc .or. nsclgrp > 1 ) then
         if ( vvlocal ) then
            allocate(s_ens_h_gu_x(grd_loc%nsig*n_ens,naensloc))
            allocate(s_ens_h_gu_y(grd_loc%nsig*n_ens,naensloc))
            call convert_km_to_grid_units(s_ens_h_gu_x(1:nz,:),s_ens_h_gu_y(1:nz,:),nz)
            do n=2,n_ens
               nk=(n-1)*nz
               do k=1,nz
                  s_ens_h_gu_x(nk+k,:)=s_ens_h_gu_x(k,:)
                  s_ens_h_gu_y(nk+k,:)=s_ens_h_gu_y(k,:)
               enddo
            enddo
            call init_rf_x(s_ens_h_gu_x(grd_loc%kbegin_loc:grd_loc%kend_alloc,:),kl)
            call init_rf_y(s_ens_h_gu_y(grd_loc%kbegin_loc:grd_loc%kend_alloc,:),kl)
         else
            allocate(s_ens_h_gu_x(1,naensloc))
            allocate(s_ens_h_gu_y(1,naensloc))
            call convert_km_to_grid_units(s_ens_h_gu_x,s_ens_h_gu_y,nz)
            call init_rf_x(s_ens_h_gu_x,kl)
            call init_rf_y(s_ens_h_gu_y,kl)
         endif
         call normal_new_factorization_rf_x
         call normal_new_factorization_rf_y
         deallocate(s_ens_h_gu_x)
         deallocate(s_ens_h_gu_y)
      endif
   else
      call init_sf_xy(jcap_ens)
   endif

   if(ntotensgrp>1 .and. (.not. global_spectral_filter_sd)) then
      call gsi_bundlegetpointer(en_perts(1,1,1),cvars3d,ipc3d,istatus)
      if(istatus/=0) then
         write(6,*) myname_,': cannot find 3d pointers'
         call stop2(999)
      endif
      call gsi_bundlegetpointer(en_perts(1,1,1),cvars2d,ipc2d,istatus)
      if(istatus/=0) then
         write(6,*) myname_,': cannot find 2d pointers'
         call stop2(999)
      endif
      if(nsclgrp>1) then
         call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
         allocate(values(grd_ens%latlon11*grd_ens%nsig*n_ens))
         if( .not. assign_vdl_nml )then
            do ig=1,nsclgrp-1
               ii=0
               do n=1,n_ens
                  a_en(n)%values => values(ii+1:ii+grd_ens%latlon11*grd_ens%nsig)
                  call gsi_bundleset(a_en(n),grid_ens,'Ensemble Bundle',istatus,names3d=(/'a_en'/),bundle_kind=r_kind)
                  if (istatus/=0) then
                     write(6,*) myname_,': error alloc(ensemble bundle)'
                     call stop2(999)
                  endif
                  ii=ii+grd_ens%latlon11*grd_ens%nsig
               enddo
               do m=1,ntlevs_ens
                  do n=1,n_ens
                     en_perts(n,ig+1,m)%valuesr4=en_perts(n,ig,m)%valuesr4
                  enddo
                  do ic3=1,nc3d
                     ipic=ipc3d(ic3)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           a_en(n)%r3(1)%q(:,:,k)=en_perts(n,ig,m)%r3(ipic)%qr4(:,:,k)
                        enddo
                     enddo
                     call bkgcov_a_en_new_factorization(naensgrp+ig,a_en)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           en_perts(n,ig,m)%r3(ipic)%qr4(:,:,k)=a_en(n)%r3(1)%q(:,:,k)
                        enddo
                     enddo
                  enddo
                  do ic2=1,nc2d
                     ipic=ipc2d(ic2)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           a_en(n)%r3(1)%q(:,:,k)=en_perts(n,ig,m)%r2(ipic)%qr4(:,:)
                        enddo
                     enddo
                     call bkgcov_a_en_new_factorization(naensgrp+ig,a_en)
                     do n=1,n_ens
                        en_perts(n,ig,m)%r2(ipic)%qr4(:,:)=a_en(n)%r3(1)%q(:,:,1)
                     enddo
                  enddo
                  do n=1,n_ens
                     en_perts(n,ig+1,m)%valuesr4=en_perts(n,ig+1,m)%valuesr4-en_perts(n,ig,m)%valuesr4
                  enddo
               enddo
               do n=1,n_ens
                  call gsi_bundleunset(a_en(n),istatus)
               enddo
            enddo
         else ! assign_vdl_nml
            smooth_scales_num = naensloc - naensgrp
            ngvarloc = 1 ! forced to 1 in this option
            allocate(en_pertstmp(n_ens,ntlevs_ens))
            allocate(en_pertstmp1(n_ens,ntlevs_ens))
            do n = 1, n_ens
              do m = 1, ntlevs_ens
                 call gsi_bundlecreate(en_pertstmp(n,m),grid_ens,'ensemble2',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_single)
                 call gsi_bundlecreate(en_pertstmp1(n,m),grid_ens,'ensemble1',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_single)
              end do
            end do
            ig = 1
            do iscl=1,smooth_scales_num + 1
               ii=0
               do n=1,n_ens
                  a_en(n)%values => values(ii+1:ii+grd_ens%latlon11*grd_ens%nsig)
                  call gsi_bundleset(a_en(n),grid_ens,'Ensemble Bundle',istatus,names3d=(/'a_en'/),bundle_kind=r_kind)
                  if (istatus/=0) then
                     write(6,*) myname_,': error alloc(ensemble bundle)'
                     call stop2(999)
                  endif
                  ii=ii+grd_ens%latlon11*grd_ens%nsig
               enddo
           
               do m=1,ntlevs_ens
                  if( ig == 1 )then
                    do n=1,n_ens
                       en_pertstmp(n,m)%valuesr4=en_perts(n,ig,m)%valuesr4
                    enddo
                  end if
                  do ic3=1,nc3d
                     ipic=ipc3d(ic3)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           a_en(n)%r3(1)%q(:,:,k)=en_pertstmp(n,m)%r3(ipic)%qr4(:,:,k)
                        enddo
                     enddo
                     if(iscl <= smooth_scales_num) call bkgcov_a_en_new_factorization(naensgrp+iscl,a_en)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           en_pertstmp1(n,m)%r3(ipic)%qr4(:,:,k)=a_en(n)%r3(1)%q(:,:,k)
                           if( vdl_scale(ig) == 0 )then
                              en_perts(n,ig,m)%r3(ipic)%qr4(:,:,k)=a_en(n)%r3(1)%q(:,:,k)
                           else  ! VDL is activated
                             do iv = 1, vdl_scale(ig)
                                en_perts(n,ig+iv-1,m)%r3(ipic)%qr4(:,:,k)=0.0_r_single
                                if( any( trim(StrLowCase(cvars3d(ic3))) == vloc_varlist(ig+iv-1,:) ) ) then
                                   en_perts(n,ig+iv-1,m)%r3(ipic)%qr4(:,:,k)=a_en(n)%r3(1)%q(:,:,k)
                                end if
                             end do
                           end if
                        enddo
                     enddo
                  enddo
                  do ic2=1,nc2d
                     ipic=ipc2d(ic2)
                     do n=1,n_ens
                        do k=1,grd_ens%nsig
                           a_en(n)%r3(1)%q(:,:,k)=en_pertstmp(n,m)%r2(ipic)%qr4(:,:)
                        enddo
                     enddo
                     if(iscl <= smooth_scales_num) call bkgcov_a_en_new_factorization(naensgrp+iscl,a_en)
                     do n=1,n_ens
                       en_pertstmp1(n,m)%r2(ipic)%qr4(:,:)=a_en(n)%r3(1)%q(:,:,1)
                       if( vdl_scale(ig) == 0 )then
                          en_perts(n,ig,m)%r2(ipic)%qr4(:,:)=a_en(n)%r3(1)%q(:,:,1)
                       else  ! VDL is activated
                          do iv = 1, vdl_scale(ig)
                             en_perts(n,ig+iv-1,m)%r2(ipic)%qr4(:,:)=0.0_r_single
                             if( any( trim(StrLowCase(cvars2d(ic2)))  == vloc_varlist(ig+iv-1,:) ) ) then
                                en_perts(n,ig+iv-1,m)%r2(ipic)%qr4(:,:)=a_en(n)%r3(1)%q(:,:,1)
                             end if
                          end do
                       end if
                     enddo
                  enddo
                  do n=1,n_ens
                     en_pertstmp(n,m)%valuesr4=en_pertstmp(n,m)%valuesr4-en_pertstmp1(n,m)%valuesr4
                  enddo
               enddo
               do n=1,n_ens
                  call gsi_bundleunset(a_en(n),istatus)
               enddo
               if( vdl_scale(ig) == 0 )then
                  ig = ig + 1
               else
                  ig = ig + vdl_scale(ig)
               end if
            enddo
            do n=1,n_ens
              do m=1,ntlevs_ens
                 call gsi_bundledestroy(en_pertstmp(n,m),istatus)
                 call gsi_bundledestroy(en_pertstmp1(n,m),istatus)
              end do
            end do
            deallocate(en_pertstmp,en_pertstmp1)
          end if
          deallocate(values)
       endif
       do ig=nsclgrp+1,ntotensgrp
          do m=1,ntlevs_ens
             do n=1,n_ens
                en_perts(n,ig,m)%valuesr4=en_perts(n,ig-nsclgrp,m)%valuesr4
             enddo
          enddo
       enddo
   endif

   !!!!!!!! setup beta_s, beta_e!!!!!!!!!!!!
   ! vertical variation of static and ensemble weights

   ! Set defaults
   sqrt_beta_s= sqrt(beta_s)
   sqrt_beta_e= sqrt(beta_e)

   ! set value of nval_lenz_en here for now,
   ! but will need to rearrange so this can be set in control_vectors
   ! and triggered by lsqrtb.
   if ( regional ) then
      nval_lenz_en = grd_loc%nlat*grd_loc%nlon*(grd_loc%kend_alloc-grd_loc%kbegin_loc+1)
   else
      nval_lenz_en = sp_loc%nc*(grd_loc%kend_alloc-grd_loc%kbegin_loc+1)
   endif
   ! nval_loc_en is the number of horizontally-filtered variables in the domain of each processor,
   ! which is the same as nval_lenz_en (horizontally-global and vertically-local) in recursive/spectral filter
   ! but horizontally-local and vertically-global in MGBF.
   if ( l_mgbf_loc ) then
      nval_loc_en = maxval( obj_mgbf(1:naensgrp)%km_all &
           & * (obj_mgbf(1:naensgrp)%im + obj_mgbf(1:naensgrp)%hx*2) &
           & * (obj_mgbf(1:naensgrp)%jm + obj_mgbf(1:naensgrp)%hy*2) )
   else
      nval_loc_en = nval_lenz_en
   endif

   ! setup vertical weighting for ensemble contribution to psfc
   call setup_pwgt

   ! write out final values for s_ens_hv, s_ens_vv, beta_s, beta_e
   if ( print_verbose ) then
      write(6,*) 'HYBENS_LOCALIZATION_SETUP: s_ens_hv(:,1),s_ens_vv(:,1),beta_s,beta_e'
      do k=1,grd_ens%nsig
         write(6,101) s_ens_hv(k,1), s_ens_vv(k,1), beta_s(k), beta_e(k)
      enddo
   endif

   call setup_ensgrp2aensgrp

   return

end subroutine hybens_localization_setup

subroutine convert_km_to_grid_units(s_ens_h_gu_x,s_ens_h_gu_y,nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_km_to_grid_units
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: change horizontal localization length scales from km to grid units.
!
! program history log:
!   2009-09-17  parrish
!   2014-05-22  wu  modification to allow vertically varying localization scales in regional
!
!   input argument list:
!     nz    -- z dimemsion of s_ens_hv; could be one
!
!   output argument list:
!     s_ens_h_gu_x - output x localization length in grid units
!     s_ens_h_gu_y - output y localization length in grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: s_ens_hv
  use hybrid_ensemble_parameters, only: region_dx_ens,region_dy_ens
  use hybrid_ensemble_parameters, only: naensloc
  use gsi_io, only: verbose
  implicit none

  integer(i_kind) ,intent(in   ) ::nz
  real(r_kind),intent(  out) ::s_ens_h_gu_x(nz,naensloc),s_ens_h_gu_y(nz,naensloc)
  logical :: print_verbose
  real(r_kind) dxmax,dymax
  integer(i_kind) k

  print_verbose=.false.
  if(verbose) print_verbose=.true.
  dxmax=maxval(region_dx_ens)
  dymax=maxval(region_dy_ens)
  if(print_verbose)then
     write(6,*)' in convert_km_to_grid_units, min, max region_dx_ens*.001=',&
                 .001_r_kind*minval(region_dx_ens),.001_r_kind*dxmax
     write(6,*)' in convert_km_to_grid_units, min, max region_dy_ens*.001=',&
                 .001_r_kind*minval(region_dy_ens),.001_r_kind*dymax
  end if

  do k=1,nz
     s_ens_h_gu_x(k,:)=s_ens_hv(k,:)/(.001_r_kind*dxmax)
     s_ens_h_gu_y(k,:)=s_ens_hv(k,:)/(.001_r_kind*dymax)
     if(print_verbose) write(6,*)' in convert_km_to_grid_units,s_ens_h(k,1),s_ens_h_gu_x,y(k,1)=', &
                    s_ens_hv(k,1),s_ens_h_gu_x(k,1),s_ens_h_gu_y(k,1)
  enddo

  return

end subroutine convert_km_to_grid_units

subroutine grads1(f,nvert,mype,fname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: create grads output file for variable f on subdomains.  used
!            to visualize the field f using grads software.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     f     - field to generate grads output file
!     nvert - number of vertical levels in f
!     mype  - local processor
!     fname - character string used to identify field f in grads output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use constants, only: one
  use gridmod, only: nlat,nlon,lon2,lat2
  implicit none

  integer(i_kind),intent(in   ) :: nvert,mype
  character(*)   ,intent(in   ) :: fname
  real(r_kind)   ,intent(in   ) :: f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(r_single) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  integer(i_kind) i,k,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc

  if(mype == 0) then
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     enddo
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startp,pinc
     next=next+1
     write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,startp,pinc
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)
 
  end if

  do k=1,nvert
     call sub2grid_1(f(1,1,k),work,0,mype)
     if(mype == 0) then
        do j=1,nlon ; do i=1,nlat
           outfield(j,i)=work(i,j)
        enddo ; enddo
        write(ioutdat)outfield
     end if
  enddo

  if(mype == 0) then
     close(ioutdes)
     close(ioutdat)
  end if

end subroutine grads1

subroutine sub2grid_1(sub,grid,gridpe,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: straightforward, but inefficient code to convert a single variable
!            on subdomains to complete slab on one processor.
!
! program history log:
!   2009-09-17  parrish
!   2010-04-01  treadon - move strip to gridmod
!   2013-10-24  todling - revisit stip interface
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     sub   - field on subdomains
!     gridpe- processor that contains full slab representation of sub
!     mype  - local processor
!
!   output argument list:
!     grid  - field on complete slab
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
         iglobal,ijn,displs_g,itotsub,strip
  use general_commvars_mod, only: ltosi,ltosj
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                  ,intent(in   ) :: gridpe,mype
  real(r_kind),dimension(lat2,lon2),intent(in   ) :: sub
  real(r_kind),dimension(nlat,nlon),intent(  out) :: grid

  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(itotsub):: work1
  integer(i_kind) mm1,i,j,k

  mm1=mype+1

  do j=1,lon1*lat1
     zsm(j)=zero
  enddo
  call strip(sub,zsm)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype, &
                 work1,ijn,displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype == gridpe) then
     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        grid(i,j)=work1(k)
     enddo
  end if

end subroutine sub2grid_1

subroutine grads1_ens(f,nvert,mype,fname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: create grads output file for variable f on subdomains.  used
!            to visualize the field f using grads software.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     f     - field to generate grads output file
!     nvert - number of vertical levels in f
!     mype  - local processor
!     fname - character string used to identify field f in grads output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use constants, only: one
  use gridmod, only: nlat,nlon,lon2,lat2
  implicit none

  integer(i_kind),intent(in   ) :: nvert,mype
  character(*)   ,intent(in   ) :: fname
  real(r_kind)   ,intent(in   ) :: f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(r_single) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  integer(i_kind) i,k,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc

  if(mype == 0) then
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     enddo
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startp,pinc
     next=next+1
     write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,startp,pinc
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)
 
  end if

  do k=1,nvert
     call sub2grid_1(f(1,1,k),work,0,mype)
     if(mype == 0) then
        do j=1,nlon ; do i=1,nlat
           outfield(j,i)=work(i,j)
        enddo ; enddo
        write(ioutdat)outfield
     end if
  enddo

  if(mype == 0) then
     close(ioutdes)
     close(ioutdat)
  end if

end subroutine grads1_ens

subroutine general_grads1(f,nvert,mype,fname,grd)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: create grads output file for variable f on subdomains.  used
!            to visualize the field f using grads software.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     f     - field to generate grads output file
!     nvert - number of vertical levels in f
!     mype  - local processor
!     fname - character string used to identify field f in grads output file
!     grd   - contains communication info for f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,i_kind
  use constants, only: one
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

  type(sub2grid_info),intent(in):: grd
  integer(i_kind),intent(in   ) :: nvert,mype
  character(*)   ,intent(in   ) :: fname
  real(r_kind)   ,intent(in   ) :: f(grd%lat2,grd%lon2,nvert)

  real(r_kind),dimension(grd%nlat,grd%nlon)::work
  real(r_single) outfield(grd%nlon,grd%nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  integer(i_kind) i,k,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(r_single) undef
  real(r_single) startp,pinc

  if(mype == 0) then
     np=nvert
     startp=1._r_single
     pinc=1._r_single
     ioutdes=98550
     ioutdat=98551
     write(filename,'(a,".des")')trim(fname)
     write(dsname,'(a,".dat")')trim(fname)
     open(unit=ioutdes,file=trim(filename),form='formatted')
     open(unit=ioutdat,file=trim(dsname),form='unformatted')
     rewind ioutdes
     rewind ioutdat
     do i=1,50000
        write(datdes(i),'(112a1)')(blank,k=1,112)
     enddo
     write(datdes(1),'("DSET ",a50)')dsname
     write(datdes(2),'("options big_endian sequential")')
     write(datdes(3),'("TITLE ",a50)')title
     write(datdes(4),'("UNDEF ",e11.2)')undef
     next=5
     write(datdes(next),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlon,startp,pinc
     next=next+1
     write(datdes(next),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')grd%nlat,startp,pinc
     next=next+1
     write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
     next=next+1
     koutmax=1
     write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
     next=next+1
     write(datdes(next),'("VARS 1")')
     next=next+1
     write(datdes(next),'("f   ",i5," 99 f   ")')nvert
     next=next+1
     write(datdes(next),'("ENDVARS")')
     last=next
     write(ioutdes,'(a112)')(datdes(i),i=1,last)
 
  end if

  do k=1,nvert
     call general_sub2grid_1_ens(f(1,1,k),work,0,mype,grd)
     if(mype == 0) then
        do j=1,grd%nlon ; do i=1,grd%nlat
           outfield(j,i)=work(i,j)
        enddo ; enddo
        write(ioutdat)outfield
     end if
  enddo

  if(mype == 0) then
     close(ioutdes)
     close(ioutdat)
  end if

end subroutine general_grads1

subroutine general_sub2grid_1_ens(sub,grid,gridpe,mype,grd)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: straightforward, but inefficient code to convert a single variable
!            on subdomains to complete slab on one processor.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     sub   - field on subdomains
!     gridpe- processor that contains full slab representation of sub
!     mype  - local processor
!
!   output argument list:
!     grid  - field on complete slab
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

  type(sub2grid_info),intent(in):: grd
  integer(i_kind)                  ,intent(in   ) :: gridpe,mype
  real(r_kind),dimension(grd%lat2,grd%lon2),intent(in   ) :: sub
  real(r_kind),dimension(grd%nlat,grd%nlon),intent(  out) :: grid

  real(r_kind),dimension(grd%lat1,grd%lon1):: zsm
  real(r_kind),dimension(grd%itotsub):: work1
  integer(i_kind) mm1,i,i0,j,j0,k

  mm1=mype+1

  do j=2,grd%lon2-1
     j0=j-1
     do i=2,grd%lat2-1
        i0=i-1
        zsm(i0,j0)=sub(i,j)
     enddo
  enddo
  call mpi_gatherv(zsm,grd%ijn(mm1),mpi_rtype, &
                 work1,grd%ijn,grd%displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype == gridpe) then
     do k=1,grd%iglobal
        i=grd%ltosi(k) ; j=grd%ltosj(k)
        grid(i,j)=work1(k)
     enddo
  end if

end subroutine general_sub2grid_1_ens

subroutine sub2grid_1_ens(sub,grid,gridpe,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grads1      generate grads output file for f on subdomains
!   prgmmr: parrish          org: np22                date: 2009-09-17
!
! abstract: straightforward, but inefficient code to convert a single variable
!            on subdomains to complete slab on one processor.
!
! program history log:
!   2009-09-17  parrish
!
!   input argument list:
!     sub   - field on subdomains
!     gridpe- processor that contains full slab representation of sub
!     mype  - local processor
!
!   output argument list:
!     grid  - field on complete slab
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use hybrid_ensemble_parameters, only: grd_ens
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                  ,intent(in   ) :: gridpe,mype
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(in   ) :: sub
  real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon),intent(  out) :: grid

  real(r_kind),dimension(grd_ens%lat1,grd_ens%lon1):: zsm
  real(r_kind),dimension(grd_ens%itotsub):: work1
  integer(i_kind) mm1,i,i0,j,j0,k

  mm1=mype+1

  do j=2,grd_ens%lon2-1
     j0=j-1
     do i=2,grd_ens%lat2-1
        i0=i-1
        zsm(i0,j0)=sub(i,j)
     enddo
  enddo
  call mpi_gatherv(zsm,grd_ens%ijn(mm1),mpi_rtype, &
                 work1,grd_ens%ijn,grd_ens%displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype == gridpe) then
     do k=1,grd_ens%iglobal
        i=grd_ens%ltosi(k) ; j=grd_ens%ltosj(k)
        grid(i,j)=work1(k)
     enddo
  end if

end subroutine sub2grid_1_ens

subroutine get_region_lat_lon_ens(region_lat_ens,region_lon_ens,rlat_e,rlon_e,nlat_e,nlon_e)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_region_lat_lon_ens
!   prgmmr: parrish          org: np22                date: 2009-09-25
!
! abstract: compute earth lats and lons on regional ensemble grid, which has the same coordinates
!            as the regional analysis grid, but on a coarser grid with a halo zone to allow full
!            accuracy interpolation of ensemble perturbations to the entire domain of the analysis grid.
!
! program history log:
!   2010-09-25  parrish, initial documentation
!
!   input argument list:
!     rlat_e - regional ensemble grid latitude coordinates in analysis grid units
!     rlon_e - regional ensemble grid longitude coordinates in analysis grid units
!     nlat_e - number of ensemble grid latitudes
!     nlon_e - number of ensemble grid longitudes
!
!   output argument list:
!     region_lat_ens - earth lats at regional ensemble grid points
!     region_lon_ens - earth lons at regional ensemble grid points
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: half,one,two,pi
  use gridmod, only: nlon,nlat,txy2ll
  use constants, only: rad2deg     ! debug only
  use gridmod, only: region_lat,region_lon
  use mpimod, only: mype
  implicit none

  real(r_kind),intent(in):: rlat_e(nlat_e),rlon_e(nlon_e)
  integer(i_kind),intent(in):: nlon_e,nlat_e
  real(r_kind),intent(out):: region_lat_ens(nlat_e,nlon_e),region_lon_ens(nlat_e,nlon_e)

  integer(i_kind) i,j,k
  logical make_test_maps
  real(r_single),allocatable::out1e(:,:)
  real(r_single),allocatable::out1(:,:)
  real(r_kind) twopi

  twopi=two*pi

  do j=1,nlon_e
     do i=1,nlat_e
        call txy2ll(rlon_e(j),rlat_e(i),region_lon_ens(i,j),region_lat_ens(i,j))
        do k=-2,2
           if(region_lon_ens(i,j)<-pi) region_lon_ens(i,j)=region_lon_ens(i,j)+twopi
           if(region_lon_ens(i,j)> pi) region_lon_ens(i,j)=region_lon_ens(i,j)-twopi
        enddo
     enddo
  enddo
                             if(mype==0) write(6,*)' min,max(region_lon_ens)=', &
                             rad2deg*minval(region_lon_ens),rad2deg*maxval(region_lon_ens)

  make_test_maps=.false.
  if(make_test_maps.and.mype==0) then
     allocate(out1e(nlon_e,nlat_e))
     allocate(out1(nlon,nlat))
     do j=1,nlon_e
        do i=1,nlat_e
           out1e(j,i)=region_lon_ens(i,j)
        enddo
     enddo
     call outgrads1(out1e,nlon_e,nlat_e,'region_lon_e')
     do j=1,nlon
        do i=1,nlat
           out1(j,i)=region_lon(i,j)
        enddo
     enddo
     call outgrads1(out1,nlon,nlat,'region_lon')
     do j=1,nlon_e
        do i=1,nlat_e
           out1e(j,i)=region_lat_ens(i,j)
        enddo
     enddo
     call outgrads1(out1e,nlon_e,nlat_e,'region_lat_e')
     do j=1,nlon
        do i=1,nlat
           out1(j,i)=region_lat(i,j)
        enddo
     enddo
     call outgrads1(out1,nlon,nlat,'region_lat')
     deallocate(out1e,out1)
  end if
  return

end subroutine get_region_lat_lon_ens

subroutine get_region_dx_dy_ens(region_dx_ens,region_dy_ens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_region_dx_dy_ens
!   prgmmr: parrish          org: np22                date: 2009-09-25
!
! abstract: compute earth grid dx, dy  on regional ensemble grid, which has the same coordinates
!            as the regional analysis grid, but on a coarser grid.
!
! program history log:
!   2010-09-25  parrish, initial documentation
!
!   input argument list:
!
!   output argument list:
!     region_dx_ens  - ensemble grid increment in x-direction (m)
!     region_dy_ens  - ensemble grid increment in y-direction (m)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens,p_e2a
  use egrid2agrid_mod, only: agrid2egrid
  use constants, only: half,one,two
  use gridmod, only: nlon,nlat
  use gridmod, only: region_dx,region_dy
  use mpimod, only: mype
  implicit none

  real(r_kind),intent(out):: region_dx_ens(nlat_ens,nlon_ens),region_dy_ens(nlat_ens,nlon_ens)

  integer(i_kind) i,j
  logical make_test_maps
  real(r_single),allocatable::out1ens(:,:),out1(:,:)
  real(r_kind) worka(1,nlat,nlon,1),worke(1,nlat_ens,nlon_ens,1)
  real(r_kind) ratio

  ratio=(nlon-one)/(nlon_ens-one)
  worka(1,:,:,1)=region_dx(:,:)
  call agrid2egrid(p_e2a,worka,worke,1,1)
  region_dx_ens(:,:)=worke(1,:,:,1)*ratio
  ratio=(nlat-one)/(nlat_ens-one)
  worka(1,:,:,1)=region_dy(:,:)
  call agrid2egrid(p_e2a,worka,worke,1,1)
  region_dy_ens(:,:)=worke(1,:,:,1)*ratio

  make_test_maps=.false.
  if(make_test_maps.and.mype==0) then
     allocate(out1ens(nlon_ens,nlat_ens))
     allocate(out1(nlon,nlat))
     do j=1,nlon_ens
        do i=1,nlat_ens
           out1ens(j,i)=region_dx_ens(i,j)
        enddo
     enddo
     call outgrads1(out1ens,nlon_ens,nlat_ens,'region_dx_ens')
     do j=1,nlon
        do i=1,nlat
           out1(j,i)=region_dx(i,j)
        enddo
     enddo
     call outgrads1(out1,nlon,nlat,'region_dx')
     do j=1,nlon_ens
        do i=1,nlat_ens
           out1ens(j,i)=region_dy_ens(i,j)
        enddo
     enddo
     call outgrads1(out1ens,nlon_ens,nlat_ens,'region_dy_ens')
     do j=1,nlon
        do i=1,nlat
           out1(j,i)=region_dy(i,j)
        enddo
     enddo
     call outgrads1(out1,nlon,nlat,'region_dy')
     deallocate(out1ens,out1)
  end if
  return

end subroutine get_region_dx_dy_ens

subroutine get_regional_dual_res_grid(eps,r_e,n_a,n_e,x_a,x_e)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_regional_dual_res_grid
!   prgmmr: parrish          org: np22                date: 2009-09-25
!
! abstract: create ensemble grid centered on regional grid domain.
!           The ensemble grid extends beyond the edge of the analysis domain by
!           amount eps (in analysis grid units), and the resolution of the
!           ensemble grid is in ratio r_e to the analysis grid.
!
! program history log:
!   2010-09-21  parrish, initial documentation
!   2011-01-30  parrish -- Rename subroutine get_regional_gefs_grid to get_regional_dual_res_grid.
!
!   input argument list:
!     eps   - width of buffer zone--how far ensemble grid extends beyond analysis grid--
!               in analysis grid units.
!     r_e   - ratio of ensemble grid resolution to analysis grid resolution (approximate--
!               actual resolution can be slightly higher)
!     n_a   - number of analysis grid points in coordinate being processed
!
!   output argument list:
!     n_e   - number of ensemble grid points in coordinate being processed
!     x_a   - analysis coordinate in grid units (1 to n_a)
!     x_e   - ensemble grid coordinate in analysis grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_single
  use constants, only: half,one,two
  implicit none

  real(r_kind),intent(in):: eps      !  width of halo zone that ensemble grid extends beyond analysis grid
                                     !       in analysis grid units
  real(r_kind),intent(in):: r_e      !  ratio of ensemble grid increment to analysis grid increment.
                                     !     because analysis grid increment=1, bigl_e is ensemble grid
                                     !     increment in analysis grid units.
  integer(i_kind),intent(in):: n_a   !  number of analysis coordinate grid points in direction processed
  integer(i_kind),intent(out):: n_e  !  number of ensemble grid points in direction processed
  real(r_kind),intent(out)::x_a(n_a) !  analysis coordinate in analysis grid units
  real(r_kind),intent(out)::x_e(n_a) !  ensemble coordinate in analysis grid units

  real(r_kind) bigl_e,r_e_x
  integer(i_kind) n

!      schematic for one dimension of domain:
!
!       |       |                                |       |
!        < eps >                                  < eps >
!
!               1                               n_a

  bigl_e = n_a - one + two*eps
  n_e = ceiling(one + bigl_e/r_e)
  r_e_x = bigl_e/(n_e-one)
  do n=1,n_a
     x_a(n)=n
  enddo
  do n=1,n_e
     x_e(n) =one - eps + r_e_x*(n-one)
  enddo
  return

end subroutine get_regional_dual_res_grid

subroutine acceptable_for_essl_fft(nin,nout)

  use kinds, only: i_kind,i_llong
  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in)::  nin
  integer(i_kind),intent(out):: nout

  integer(i_kind) i_eleven,i_seven,i_five,i_three,i_two,n_acceptable_total,i,j,k
  integer(i_llong) n_eleven,n_seven,n_five,n_three,n_two,n_this_try
  integer(i_kind),allocatable::n_acceptable_table(:)

!   given desired number of points nin, find closest value nout >= nin which
!   is allowed by ibm ffts.

!     compute table of acceptable length for essl fft
    n_acceptable_total=0
    do i_eleven=0,1
      n_eleven=11**i_eleven
      do i_seven=0,1
        n_seven=7**i_seven
        do i_five=0,1
          n_five=5**i_five
          do i_three=0,2
            n_three=3**i_three
            do i_two=1,25
              n_two=2**i_two
              n_this_try=n_two*n_three*n_five*n_seven*n_eleven
              if(n_this_try.le.37748736_i_llong) n_acceptable_total=n_acceptable_total+1
            enddo
          enddo
        enddo
      enddo
    enddo
    allocate(n_acceptable_table(n_acceptable_total))
    i=0
    do i_eleven=0,1
      n_eleven=11**i_eleven
      do i_seven=0,1
        n_seven=7**i_seven
        do i_five=0,1
          n_five=5**i_five
          do i_three=0,2
            n_three=3**i_three
            do i_two=1,25
              n_two=2**i_two
              n_this_try=n_two*n_three*n_five*n_seven*n_eleven
              if(n_this_try.le.37748736_i_llong) then
                i=i+1
                n_acceptable_table(i)=n_this_try
              end if
            enddo
          enddo
        enddo
      enddo
    enddo
    do i=1,n_acceptable_total-1
      do j=i+1,n_acceptable_total
        if(n_acceptable_table(j).lt.n_acceptable_table(i)) then
           k=n_acceptable_table(j)
           n_acceptable_table(j)=n_acceptable_table(i)
           n_acceptable_table(i)=k
        end if
      enddo
    enddo
    do i=1,n_acceptable_total
      if(mype==0) write(6,*)' i,n_acceptable_table(i)=',i,n_acceptable_table(i)
    enddo
    nout=0
    do i=1,n_acceptable_total
      nout=n_acceptable_table(i)
      if(nout.ge.nin) exit
    enddo
    deallocate(n_acceptable_table)
    return

end subroutine acceptable_for_essl_fft

subroutine setup_pwgt 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_pwgt
!   prgmmr: wu               org: np22                date: 2011-06-14
!
! abstract: setup pwgt
!           pwgt : vertical projection of control variable A for Psfc
!
! program history log:
!   2011_06_14  wu- initial documentation
!   2012-10-16  wu- only setup if the options are on
!   2013-10-19  todling - all guess variables in met-guess
!   2016-03-14  mahajan - remove hybrid weights from this routine, no longer necessary
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use constants,only: zero,one
   use mpimod, only: mype,mpi_comm_world,mpi_rtype,mpi_sum
   use gridmod, only: lat2,lon2,nsig,regional
   use general_sub2grid_mod, only: general_suba2sube
   use balmod, only: wgvk
   use hybrid_ensemble_parameters, only: pwgtflg,pwgt,dual_res,grd_ens,grd_a1,grd_e1,p_e2a

   implicit none

   character(len=*),parameter :: myname='setup_pwgt::'
   integer(i_kind) :: i,j,k
   real(r_kind) :: tmp_sum
   real(r_kind),allocatable,dimension(:,:,:,:) :: wgvk_ens,wgvk_anl

!!!!!!!!!!! setup pwgt !!!!!!!!!!!!!!!!!!!!!
! weigh with balanced projection for pressure

   ! Set defaults
   pwgt(:,:,1) = one

   if ( pwgtflg ) then

      if ( regional ) then

         allocate ( wgvk_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
         allocate ( wgvk_anl(lat2,lon2,nsig,1) )
         if ( dual_res ) then
            wgvk_anl(:,:,:,1) = wgvk(:,:,:)
            call general_suba2sube(grd_a1,grd_e1,p_e2a,wgvk_anl,wgvk_ens,regional)
         else
            wgvk_ens(:,:,:,1) = wgvk(:,:,:)
         endif

         pwgt = zero
         do j=1,grd_ens%lon2
            do i=1,grd_ens%lat2
               tmp_sum = zero
               do k=1,grd_ens%nsig
                  tmp_sum = tmp_sum + wgvk_ens(i,j,k,1)
               enddo
               if ( tmp_sum /= zero ) tmp_sum = one / tmp_sum
               do k=1,grd_ens%nsig
                  pwgt(i,j,k)= tmp_sum * wgvk_ens(i,j,k,1)
               enddo
            enddo
         enddo
         deallocate(wgvk_ens,wgvk_anl)
    
      else ! if ( regional )

         if ( mype == 0 ) then
            write(6,*) 'SETUP_PWGT: routine not built to load pwgt for global application'
            write(6,*) 'SETUP_PWGT: using defaults instead'
         endif

      endif ! if ( regional )

   endif ! if ( pwgtflg )

   return

end subroutine setup_pwgt

subroutine setup_ensgrp2aensgrp
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set a matrix of (naensgrp,naensgrp)
!
! program history log:
!   2022-09-15  yokota  - add scale/variable/time-dependent localization
!
!   input argument list:
!
!   output argument list:
!
! remarks:
!    need to reconcile grid in gsi_bundle w/ grid_ens/grid_anl
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: zero,one
  use hybrid_ensemble_parameters, only: l_timloc_opt,r_ensloccov4tim,r_ensloccov4var,r_ensloccov4scl
  use hybrid_ensemble_parameters, only: ensloccov4tim,ensloccov4var,ensloccov4scl
  use hybrid_ensemble_parameters, only: ntotensgrp,naensgrp,ntlevs_ens,nsclgrp,ngvarloc
  use hybrid_ensemble_parameters, only: ensgrp2aensgrp
  use hybrid_ensemble_parameters, only: idaen2d,idaen3d
  use hybrid_ensemble_parameters, only: alphacvarsclgrpmat
  implicit none

  integer (i_kind):: i,j
  integer (i_kind):: ig,ibin,ic,iscl,itim1,itim2,igvar1,igvar2,iscl1,iscl2,ivargrp
  integer (i_kind):: ntimloc,interval4aens

  if (l_timloc_opt) then
     ntimloc=ntlevs_ens
     interval4aens=ntotensgrp
  else
     ntimloc=1
     interval4aens=0
  endif
  if(naensgrp/=ntimloc*ngvarloc*nsclgrp) then
     write(6,*)'setup_ensgrp2aensgrp: wrong naensgrp'
     call stop2(666)
  endif
  if(ntotensgrp/=ngvarloc*nsclgrp) then
     write(6,*)'setup_ensgrp2aensgrp: wrong ntotensgrp'
     call stop2(666)
  endif
  ensgrp2aensgrp=-999
  do ibin=1,ntlevs_ens
     do ic=1,nc3d+nc2d
        if(ngvarloc>1) then
           if(ic<=nc3d) ivargrp=idaen3d(ic)
           if(ic> nc3d) ivargrp=idaen2d(ic-nc3d)
        else
           ivargrp=1
        endif
        do iscl=1,nsclgrp
           ig=(ivargrp-1)*nsclgrp+iscl
           ensgrp2aensgrp(ig,ic,ibin)=(ibin-1)*interval4aens+(ivargrp-1)*nsclgrp+iscl
        enddo
     enddo
  enddo

  ensloccov4tim=r_ensloccov4tim
  ensloccov4tim(1)=one
  ensloccov4var=r_ensloccov4var
  ensloccov4var(1)=one
  ensloccov4scl=r_ensloccov4scl
  ensloccov4scl(1)=one

  do itim2=1,ntimloc
     do itim1=1,ntimloc
        do igvar1=1,ngvarloc
           do igvar2=1,ngvarloc
              do iscl1=1,nsclgrp
                 do iscl2=1,nsclgrp
                    i=(itim1-1)*interval4aens+(igvar1-1)*nsclgrp+iscl1
                    j=(itim2-1)*interval4aens+(igvar2-1)*nsclgrp+iscl2
                    alphacvarsclgrpmat(i,j)=ensloccov4tim(abs(itim1-itim2)+1) &
                                           *ensloccov4var(abs(igvar1-igvar2)+1) &
                                           *ensloccov4scl(abs(iscl1-iscl2)+1) !first for ttime covariance
                 enddo
              enddo
           enddo
        enddo
     enddo
  enddo

end subroutine setup_ensgrp2aensgrp

end module hybrid_ensemble_isotropic
