module intwspd10mmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intwspd10mmod    module for intwspd10m and its tangent linear intwspd10m_tl
!   prgmmr:
!
! abstract: module for intwspd10m and its tangent linear intwspd10m_tl
!
! program history log:
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intwspd10m
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intwspd10m

contains

subroutine intwspd10m(wspd10mhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intwspd10m      apply nonlin qc obs operator for conv. wspd10m
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional wspd10m
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     wspd10mhead
!     swspd10m    - increment in grid space
!     rwspd10m
!
!   output argument list:
!     rwspd10m    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: wspd10m_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(wspd10m_ob_type),pointer,intent(in   ) :: wspd10mhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_wspd10m,p0,grad,wnotgross,wgross,pg_wspd10m
  real(r_kind),pointer,dimension(:) :: swspd10m
  real(r_kind),pointer,dimension(:) :: rwspd10m
  type(wspd10m_ob_type), pointer :: wspd10mptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'wspd10m',swspd10m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'wspd10m',rwspd10m,istatus);ier=istatus+ier
  if(ier/=0)return

  wspd10mptr => wspd10mhead
  do while (associated(wspd10mptr))
     j1=wspd10mptr%ij(1)
     j2=wspd10mptr%ij(2)
     j3=wspd10mptr%ij(3)
     j4=wspd10mptr%ij(4)
     w1=wspd10mptr%wij(1)
     w2=wspd10mptr%wij(2)
     w3=wspd10mptr%wij(3)
     w4=wspd10mptr%wij(4)

!    Forward model
     val=w1*swspd10m(j1)+w2*swspd10m(j2)&
        +w3*swspd10m(j3)+w4*swspd10m(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*wspd10mptr%raterr2*wspd10mptr%err2
           wspd10mptr%diags%obssen(jiter) = grad
        else
           if (wspd10mptr%luse) wspd10mptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-wspd10mptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. wspd10mptr%pg > tiny_r_kind .and. &
                                wspd10mptr%b  > tiny_r_kind) then
              pg_wspd10m=wspd10mptr%pg*varqc_iter
              cg_wspd10m=cg_term/wspd10mptr%b
              wnotgross= one-pg_wspd10m
              wgross = pg_wspd10m*cg_wspd10m/wnotgross
              p0   = wgross/(wgross+exp(-half*wspd10mptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*wspd10mptr%raterr2*wspd10mptr%err2
           end if
        endif

!       Adjoint
        rwspd10m(j1)=rwspd10m(j1)+w1*grad
        rwspd10m(j2)=rwspd10m(j2)+w2*grad
        rwspd10m(j3)=rwspd10m(j3)+w3*grad
        rwspd10m(j4)=rwspd10m(j4)+w4*grad
     endif

     wspd10mptr => wspd10mptr%llpoint

  end do

  return
end subroutine intwspd10m

end module intwspd10mmod
