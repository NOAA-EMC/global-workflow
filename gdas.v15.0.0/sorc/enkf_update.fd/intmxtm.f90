module intmxtmmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intmxtmmod    module for intmxtm and its tangent linear intmxtm_tl
!   prgmmr:
!
! abstract: module for intmxtm and its tangent linear intmxtm_tl
!
! program history log:
!   2014-04-10  pondeca
!
! subroutines included:
!   sub intmxtm
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
PUBLIC intmxtm

contains

subroutine intmxtm(mxtmhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intmxtm      apply nonlin qc obs operator for conv. mxtm
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional mxtm
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     mxtmhead
!     smxtm    - increment in grid space
!     rmxtm
!
!   output argument list:
!     rmxtm    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: mxtm_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(mxtm_ob_type),pointer,intent(in   ) :: mxtmhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_mxtm,p0,grad,wnotgross,wgross,pg_mxtm
  real(r_kind),pointer,dimension(:) :: smxtm
  real(r_kind),pointer,dimension(:) :: rmxtm
  type(mxtm_ob_type), pointer :: mxtmptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'mxtm',smxtm,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'mxtm',rmxtm,istatus);ier=istatus+ier
  if(ier/=0)return

  mxtmptr => mxtmhead
  do while (associated(mxtmptr))
     j1=mxtmptr%ij(1)
     j2=mxtmptr%ij(2)
     j3=mxtmptr%ij(3)
     j4=mxtmptr%ij(4)
     w1=mxtmptr%wij(1)
     w2=mxtmptr%wij(2)
     w3=mxtmptr%wij(3)
     w4=mxtmptr%wij(4)

!    Forward model
     val=w1*smxtm(j1)+w2*smxtm(j2)&
        +w3*smxtm(j3)+w4*smxtm(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*mxtmptr%raterr2*mxtmptr%err2
           mxtmptr%diags%obssen(jiter) = grad
        else
           if (mxtmptr%luse) mxtmptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-mxtmptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. mxtmptr%pg > tiny_r_kind .and. &
                                mxtmptr%b  > tiny_r_kind) then
              pg_mxtm=mxtmptr%pg*varqc_iter
              cg_mxtm=cg_term/mxtmptr%b
              wnotgross= one-pg_mxtm
              wgross = pg_mxtm*cg_mxtm/wnotgross
              p0   = wgross/(wgross+exp(-half*mxtmptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*mxtmptr%raterr2*mxtmptr%err2
           end if
        endif

!       Adjoint
        rmxtm(j1)=rmxtm(j1)+w1*grad
        rmxtm(j2)=rmxtm(j2)+w2*grad
        rmxtm(j3)=rmxtm(j3)+w3*grad
        rmxtm(j4)=rmxtm(j4)+w4*grad
     endif

     mxtmptr => mxtmptr%llpoint

  end do

  return
end subroutine intmxtm

end module intmxtmmod
