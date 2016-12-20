module inthowvmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inthowvmod    module for inthowv and its tangent linear inthowv_tl
!   prgmmr:
!
! abstract: module for inthowv and its tangent linear inthowv_tl
!
! program history log:
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub inthowv
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
PUBLIC inthowv

contains

subroutine inthowv(howvhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inthowv      apply nonlin qc obs operator for conv. howv
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional howv
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     howvhead
!     showv    - increment in grid space
!     rhowv
!
!   output argument list:
!     rhowv    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: howv_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(howv_ob_type),pointer,intent(in   ) :: howvhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_howv,p0,grad,wnotgross,wgross,pg_howv
  real(r_kind),pointer,dimension(:) :: showv
  real(r_kind),pointer,dimension(:) :: rhowv
  type(howv_ob_type), pointer :: howvptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'howv',showv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'howv',rhowv,istatus);ier=istatus+ier
  if(ier/=0)return

  howvptr => howvhead
  do while (associated(howvptr))
     j1=howvptr%ij(1)
     j2=howvptr%ij(2)
     j3=howvptr%ij(3)
     j4=howvptr%ij(4)
     w1=howvptr%wij(1)
     w2=howvptr%wij(2)
     w3=howvptr%wij(3)
     w4=howvptr%wij(4)

!    Forward model
     val=w1*showv(j1)+w2*showv(j2)&
        +w3*showv(j3)+w4*showv(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*howvptr%raterr2*howvptr%err2
           howvptr%diags%obssen(jiter) = grad
        else
           if (howvptr%luse) howvptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-howvptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. howvptr%pg > tiny_r_kind .and. &
                                howvptr%b  > tiny_r_kind) then
              pg_howv=howvptr%pg*varqc_iter
              cg_howv=cg_term/howvptr%b
              wnotgross= one-pg_howv
              wgross = pg_howv*cg_howv/wnotgross
              p0   = wgross/(wgross+exp(-half*howvptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*howvptr%raterr2*howvptr%err2
           end if
        endif

!       Adjoint
        rhowv(j1)=rhowv(j1)+w1*grad
        rhowv(j2)=rhowv(j2)+w2*grad
        rhowv(j3)=rhowv(j3)+w3*grad
        rhowv(j4)=rhowv(j4)+w4*grad
     endif

     howvptr => howvptr%llpoint

  end do

  return
end subroutine inthowv

end module inthowvmod
