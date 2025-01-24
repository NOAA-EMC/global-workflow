module xhat_vordivmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   xhat_vordivmod
!  prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added module doc block
!   2010-04-01  treadon - move strip,reorder,reorder2 to gridmod
!   2012-06-12  parrish - replace mpi_all2allv and all supporting code with general_sub2grid
!                         and general_grid2sub
!
! subroutines included:
!   sub init_
!   sub clean_
!   sub calc_
!   sub calc2_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nsig,&
       regional,nlon,nlat
  use compact_diffs, only: uv2vordiv
  use gsi_4dvar, only: nobs_bins
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_commvars_mod, only: s2guv
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub

  implicit none
  private

  public xhat_vordiv_init
  public xhat_vordiv_calc
  public xhat_vordiv_calc2
  public xhat_vordiv_clean

  interface xhat_vordiv_init;  module procedure init_ ; end interface
  interface xhat_vordiv_calc;  module procedure calc_ ; end interface
  interface xhat_vordiv_calc2; module procedure calc2_; end interface
  interface xhat_vordiv_clean; module procedure clean_; end interface

  real(r_kind),public,allocatable,dimension(:,:,:,:):: xhat_vor
  real(r_kind),public,allocatable,dimension(:,:,:,:):: xhat_div

CONTAINS

subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  allocate(xhat_vor(lat2,lon2,nsig,nobs_bins))
  allocate(xhat_div(lat2,lon2,nsig,nobs_bins))
  xhat_vor=zero
  xhat_div=zero
end subroutine init_

subroutine clean_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(xhat_div)
  deallocate(xhat_vor)
end subroutine clean_

subroutine calc_(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  Caculate increment of vorticity divergence
!
! program history log:
!   2007-07-05  todling - intial code; stripped off from update_guess
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-01  todling - gracefully exit when (u,v) pointers not found
!   2012-06-12  parrish - replace mpi_all2allv and all supporting code with general_sub2grid
!                         and general_grid2sub
!
!   input argument list:
!     sval     - analysis increment in grid space
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: sval(nobs_bins)

! Declare local variables
  integer(i_kind) i,j,k,ii,istatus
  real(r_kind),dimension(nlat,nlon):: usm,vsm
  real(r_kind),dimension(:,:,:,:),allocatable:: work1,worksub
  real(r_kind),pointer,dimension(:,:,:):: uptr,vptr
  logical docalc

!*******************************************************************************

! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

     allocate(work1(2,s2guv%nlat,s2guv%nlon,s2guv%kbegin_loc:s2guv%kend_alloc))
     allocate(worksub(2,s2guv%lat2,s2guv%lon2,s2guv%nsig))
     do ii=1,nobs_bins
!       NCEP GFS interface
  
!       Get pointers to u and v
        call gsi_bundlegetpointer(sval(ii),'u',uptr,istatus);docalc=istatus==0
        call gsi_bundlegetpointer(sval(ii),'v',vptr,istatus);docalc=istatus==0.and.docalc
        if(.not.docalc) exit

!       Put u,v subdomains on global slabs
!       Note:  u --> work1(1,:,:,:), v --> work1(2,:,:,:)

        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 worksub(1,i,j,k)=uptr(i,j,k)
                 worksub(2,i,j,k)=vptr(i,j,k)
              end do
           end do
        end do

        call general_sub2grid(s2guv,worksub,work1)

!       Call u,v --> vor,div routine (conversion uses compact differences)
        do k=s2guv%kbegin_loc,s2guv%kend_loc
           do j=1,nlon
              do i=1,nlat
                 usm(i,j)=work1(1,i,j,k)
                 vsm(i,j)=work1(2,i,j,k)
              end do
           end do
           call uv2vordiv(usm,vsm)
           do j=1,nlon
              do i=1,nlat
                 work1(1,i,j,k)=usm(i,j)
                 work1(2,i,j,k)=vsm(i,j)
              end do
           end do
        end do

!       Get vor,div on subdomains
!       Note:  work1 --> vor, work2 --> div

        
        call general_grid2sub(s2guv,work1,worksub)
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 xhat_vor(i,j,k,ii)=worksub(1,i,j,k)
                 xhat_div(i,j,k,ii)=worksub(2,i,j,k)
              end do
           end do
        end do

!    End of NCEP GFS block
     end do
     deallocate(work1,worksub)
  endif

  return
end subroutine calc_

subroutine calc2_(u,v,vor,div)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  Caculate increment of vorticity divergence
!
! program history log:
!   2007-07-05  todling - intial code; stripped off from update_guess
!   2009-02-23  todling - this is a variation of calc_ above
!   2012-06-12  parrish - replace mpi_all2allv and all supporting code with general_sub2grid
!                         and general_grid2sub
!
!   input argument list:
!     u,v     - increment in grid space
!
!   output argument list:
!     vor,div - increment in grid space
!
!   comments:
!     DO NOT OVERLOAD calc (that is, DON'T ADD INTERFACE FOR 2 ROUTINES)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: u,v
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: vor,div

! Declare local variables
  integer(i_kind) i,j,k
  real(r_kind),dimension(nlat,nlon):: usm,vsm
  real(r_kind),dimension(:,:,:,:),allocatable:: work1,worksub

!*******************************************************************************

! Initialize local arrays
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           vor(i,j,k) = zero
           div(i,j,k) = zero
        end do
     end do
  end do

! The GSI analyzes stream function (sf) and velocity potential (vp).  
! Wind field observations are in terms of zonal (u) and meridional 
! (v) wind components or wind speed.  Thus, the GSI carries wind 
! increments in terms of both u,v and sf,vp.  
!
! The NCEP GFS (global) model uses vorticity and divergence as
! wind field variable.  The code below converts increments in 
! u and v to those in vorticity and divergence.  The wind variables
! in the NCEP regional model are u and v.  Hence, the block of code
! below is only used for the NCEP GFS (.not.regional). 

! Other users may need to change the logical below to obtain the
! proper behavior for their specific guess (model background)

! For NCEP GFS convert increment in u,v to increments in vor,div
  if (.not.regional) then

     allocate(work1(2,s2guv%nlat,s2guv%nlon,s2guv%kbegin_loc:s2guv%kend_alloc))
     allocate(worksub(2,s2guv%lat2,s2guv%lon2,s2guv%nsig))
!    NCEP GFS interface

     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              worksub(1,i,j,k)=u(i,j,k)
              worksub(2,i,j,k)=v(i,j,k)
           end do
        end do
     end do

     call general_sub2grid(s2guv,worksub,work1)

!    Call u,v --> vor,div routine (conversion uses compact differences)
     do k=s2guv%kbegin_loc,s2guv%kend_loc
        do j=1,nlon
           do i=1,nlat
              usm(i,j)=work1(1,i,j,k)
              vsm(i,j)=work1(2,i,j,k)
           end do
        end do
        call uv2vordiv(usm,vsm)
        do j=1,nlon
           do i=1,nlat
              work1(1,i,j,k)=usm(i,j)
              work1(2,i,j,k)=vsm(i,j)
           end do
        end do
     end do

!       Get vor,div on subdomains
!       Note:  work1 --> vor, work2 --> div


     call general_grid2sub(s2guv,work1,worksub)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              vor(i,j,k)=worksub(1,i,j,k)
              div(i,j,k)=worksub(2,i,j,k)
           end do
        end do
     end do

! End of NCEP GFS block
     deallocate(work1,worksub)

  endif

  return
end subroutine calc2_

end module xhat_vordivmod
