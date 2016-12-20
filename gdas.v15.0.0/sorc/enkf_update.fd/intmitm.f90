module intmitmmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intmitmmod    module for intmitm and its tangent linear intmitm_tl
!   prgmmr:
!
! abstract: module for intmitm and its tangent linear intmitm_tl
!
! program history log:
!   2014-04-10  pondeca
!
! subroutines included:
!   sub intmitm
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
PUBLIC intmitm

contains

subroutine intmitm(mitmhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intmitm      apply nonlin qc obs operator for conv. mitm
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional mitm
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     mitmhead
!     smitm    - increment in grid space
!     rmitm
!
!   output argument list:
!     rmitm    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: mitm_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(mitm_ob_type),pointer,intent(in   ) :: mitmhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_mitm,p0,grad,wnotgross,wgross,pg_mitm
  real(r_kind),pointer,dimension(:) :: smitm
  real(r_kind),pointer,dimension(:) :: rmitm
  type(mitm_ob_type), pointer :: mitmptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'mitm',smitm,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'mitm',rmitm,istatus);ier=istatus+ier
  if(ier/=0)return

  mitmptr => mitmhead
  do while (associated(mitmptr))
     j1=mitmptr%ij(1)
     j2=mitmptr%ij(2)
     j3=mitmptr%ij(3)
     j4=mitmptr%ij(4)
     w1=mitmptr%wij(1)
     w2=mitmptr%wij(2)
     w3=mitmptr%wij(3)
     w4=mitmptr%wij(4)

!    Forward model
     val=w1*smitm(j1)+w2*smitm(j2)&
        +w3*smitm(j3)+w4*smitm(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*mitmptr%raterr2*mitmptr%err2
           mitmptr%diags%obssen(jiter) = grad
        else
           if (mitmptr%luse) mitmptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-mitmptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. mitmptr%pg > tiny_r_kind .and. &
                                mitmptr%b  > tiny_r_kind) then
              pg_mitm=mitmptr%pg*varqc_iter
              cg_mitm=cg_term/mitmptr%b
              wnotgross= one-pg_mitm
              wgross = pg_mitm*cg_mitm/wnotgross
              p0   = wgross/(wgross+exp(-half*mitmptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*mitmptr%raterr2*mitmptr%err2
           end if
        endif

!       Adjoint
        rmitm(j1)=rmitm(j1)+w1*grad
        rmitm(j2)=rmitm(j2)+w2*grad
        rmitm(j3)=rmitm(j3)+w3*grad
        rmitm(j4)=rmitm(j4)+w4*grad
     endif

     mitmptr => mitmptr%llpoint

  end do

  return
end subroutine intmitm

end module intmitmmod
