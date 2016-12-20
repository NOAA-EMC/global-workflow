module intpmslmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intpmslmod    module for intpmsl and its tangent linear intpmsl_tl
!   prgmmr:
!
! abstract: module for intpmsl and its tangent linear intpmsl_tl
!
! program history log:
!   2014-04-10  pondeca
!
! subroutines included:
!   sub intpmsl
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
PUBLIC intpmsl

contains

subroutine intpmsl(pmslhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpmsl      apply nonlin qc obs operator for conv. pmsl
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional pmsl
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     pmslhead
!     spmsl    - increment in grid space
!     rpmsl
!
!   output argument list:
!     rpmsl    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: pmsl_ob_type, lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(pmsl_ob_type),pointer,intent(in   ) :: pmslhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_pmsl,p0,grad,wnotgross,wgross,pg_pmsl
  real(r_kind),pointer,dimension(:) :: spmsl
  real(r_kind),pointer,dimension(:) :: rpmsl
  type(pmsl_ob_type), pointer :: pmslptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pmsl',spmsl,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pmsl',rpmsl,istatus);ier=istatus+ier
  if(ier/=0)return

  pmslptr => pmslhead
  do while (associated(pmslptr))
     j1=pmslptr%ij(1)
     j2=pmslptr%ij(2)
     j3=pmslptr%ij(3)
     j4=pmslptr%ij(4)
     w1=pmslptr%wij(1)
     w2=pmslptr%wij(2)
     w3=pmslptr%wij(3)
     w4=pmslptr%wij(4)

!    Forward model
     val=w1*spmsl(j1)+w2*spmsl(j2)&
        +w3*spmsl(j3)+w4*spmsl(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*pmslptr%raterr2*pmslptr%err2
           pmslptr%diags%obssen(jiter) = grad
        else
           if (pmslptr%luse) pmslptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-pmslptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. pmslptr%pg > tiny_r_kind .and. &
                                pmslptr%b  > tiny_r_kind) then
              pg_pmsl=pmslptr%pg*varqc_iter
              cg_pmsl=cg_term/pmslptr%b
              wnotgross= one-pg_pmsl
              wgross = pg_pmsl*cg_pmsl/wnotgross
              p0   = wgross/(wgross+exp(-half*pmslptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*pmslptr%raterr2*pmslptr%err2
           end if
        endif

!       Adjoint
        rpmsl(j1)=rpmsl(j1)+w1*grad
        rpmsl(j2)=rpmsl(j2)+w2*grad
        rpmsl(j3)=rpmsl(j3)+w3*grad
        rpmsl(j4)=rpmsl(j4)+w4*grad
     endif

     pmslptr => pmslptr%llpoint

  end do

  return
end subroutine intpmsl

end module intpmslmod
