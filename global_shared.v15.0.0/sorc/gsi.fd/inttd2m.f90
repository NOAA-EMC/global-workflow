module inttd2mmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inttd2mmod    module for inttd2m and its tangent linear inttd2m_tl
!   prgmmr:
!
! abstract: module for inttd2m and its tangent linear inttd2m_tl
!
! program history log:
!   2014-04-10  pondeca
!
! subroutines included:
!   sub inttd2m
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
PUBLIC inttd2m

contains

subroutine inttd2m(td2mhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inttd2m      apply nonlin qc obs operator for conv. td2m
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional td2m
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     td2mhead
!     std2m    - increment in grid space
!     rtd2m
!
!   output argument list:
!     rtd2m    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: td2m_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(td2m_ob_type),pointer,intent(in   ) :: td2mhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_td2m,p0,grad,wnotgross,wgross,pg_td2m
  real(r_kind),pointer,dimension(:) :: std2m
  real(r_kind),pointer,dimension(:) :: rtd2m
  type(td2m_ob_type), pointer :: td2mptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'td2m',std2m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'td2m',rtd2m,istatus);ier=istatus+ier
  if(ier/=0)return

  td2mptr => td2mhead
  do while (associated(td2mptr))
     j1=td2mptr%ij(1)
     j2=td2mptr%ij(2)
     j3=td2mptr%ij(3)
     j4=td2mptr%ij(4)
     w1=td2mptr%wij(1)
     w2=td2mptr%wij(2)
     w3=td2mptr%wij(3)
     w4=td2mptr%wij(4)

!    Forward model
     val=w1*std2m(j1)+w2*std2m(j2)&
        +w3*std2m(j3)+w4*std2m(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*td2mptr%raterr2*td2mptr%err2
           td2mptr%diags%obssen(jiter) = grad
        else
           if (td2mptr%luse) td2mptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-td2mptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. td2mptr%pg > tiny_r_kind .and. &
                                td2mptr%b  > tiny_r_kind) then
              pg_td2m=td2mptr%pg*varqc_iter
              cg_td2m=cg_term/td2mptr%b
              wnotgross= one-pg_td2m
              wgross = pg_td2m*cg_td2m/wnotgross
              p0   = wgross/(wgross+exp(-half*td2mptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*td2mptr%raterr2*td2mptr%err2
           end if
        endif

!       Adjoint
        rtd2m(j1)=rtd2m(j1)+w1*grad
        rtd2m(j2)=rtd2m(j2)+w2*grad
        rtd2m(j3)=rtd2m(j3)+w3*grad
        rtd2m(j4)=rtd2m(j4)+w4*grad
     endif

     td2mptr => td2mptr%llpoint

  end do

  return
end subroutine inttd2m

end module inttd2mmod
