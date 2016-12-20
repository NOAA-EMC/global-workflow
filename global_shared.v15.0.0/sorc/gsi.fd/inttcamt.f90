module inttcamtmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inttcamtmod    module for inttcamt 
!   prgmmr:
!
! abstract: module for inttcamt 
!
! program history log:
!
! subroutines included:
!   sub inttcamt
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
PUBLIC inttcamt

contains

subroutine inttcamt(tcamthead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inttcamt      apply nonlin qc obs operator for conv. tcamt
!   prgmmr: zhu           org: np23                date: 2012-01-20
!
! abstract: apply observation operator and adjoint for conventional tcamt
!           observations with nonlinear qc operator
!
! program history log:
!
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     tcamthead
!     stcamt    - increment in grid space
!     rtcamt
!
!   output argument list:
!     rtcamt    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: tcamt_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(tcamt_ob_type),pointer,intent(in ) :: tcamthead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_tcamt,p0,grad,wnotgross,wgross,pg_tcamt
  real(r_kind),pointer,dimension(:) :: stcamt
  real(r_kind),pointer,dimension(:) :: rtcamt
  type(tcamt_ob_type), pointer :: tcamtptr

! If no tcamt data return
  if(.not. associated(tcamthead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'tcamt',stcamt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tcamt',rtcamt,istatus);ier=istatus+ier
  if(ier/=0)return

  tcamtptr => tcamthead
  do while (associated(tcamtptr))
     j1=tcamtptr%ij(1)
     j2=tcamtptr%ij(2)
     j3=tcamtptr%ij(3)
     j4=tcamtptr%ij(4)
     w1=tcamtptr%wij(1)
     w2=tcamtptr%wij(2)
     w3=tcamtptr%wij(3)
     w4=tcamtptr%wij(4)

!    Forward model
     val=w1*stcamt(j1)+w2*stcamt(j2)&
        +w3*stcamt(j3)+w4*stcamt(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*tcamtptr%raterr2*tcamtptr%err2
           tcamtptr%diags%obssen(jiter) = grad
        else
           if (tcamtptr%luse) tcamtptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           val=val-tcamtptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. tcamtptr%pg > tiny_r_kind .and. &
                                tcamtptr%b  > tiny_r_kind) then
              pg_tcamt=tcamtptr%pg*varqc_iter
              cg_tcamt=cg_term/tcamtptr%b
              wnotgross= one-pg_tcamt
              wgross = pg_tcamt*cg_tcamt/wnotgross
              p0   = wgross/(wgross+exp(-half*tcamtptr%err2*val**2))
              val = val*(one-p0)
           endif

           grad = val*tcamtptr%raterr2*tcamtptr%err2
        endif

!       Adjoint
        rtcamt(j1)=rtcamt(j1)+w1*grad
        rtcamt(j2)=rtcamt(j2)+w2*grad
        rtcamt(j3)=rtcamt(j3)+w3*grad
        rtcamt(j4)=rtcamt(j4)+w4*grad
     endif

     tcamtptr => tcamtptr%llpoint

  end do

  return
end subroutine inttcamt

end module inttcamtmod
