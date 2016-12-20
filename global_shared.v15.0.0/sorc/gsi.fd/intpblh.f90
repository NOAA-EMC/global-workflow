module intpblhmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intpblhmod    module for intpblh and its tangent linear intpblh_tl
!   prgmmr:
!
! abstract: module for intpblh and its tangent linear intpblh_tl
!
! program history log:
!
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intpblh
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
PUBLIC intpblh

contains

subroutine intpblh(pblhhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpblh      apply nonlin qc obs operator for conv. pblh
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: apply observation operator and adjoint for conventional pblh
!           observations with nonlinear qc operator
!
! program history log:
!
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     pblhhead
!     spblh    - increment in grid space
!     rpblh
!
!   output argument list:
!     rpblh    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: pblh_ob_type, lsaveobsens, l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(pblh_ob_type),pointer,intent(in   ) :: pblhhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_pblh,p0,grad,wnotgross,wgross,pg_pblh
  real(r_kind),pointer,dimension(:) :: spblh
  real(r_kind),pointer,dimension(:) :: rpblh
  type(pblh_ob_type), pointer :: pblhptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pblh',spblh,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pblh',rpblh,istatus);ier=istatus+ier
  if(ier/=0)return

  pblhptr => pblhhead
  do while (associated(pblhptr))
     j1=pblhptr%ij(1)
     j2=pblhptr%ij(2)
     j3=pblhptr%ij(3)
     j4=pblhptr%ij(4)
     w1=pblhptr%wij(1)
     w2=pblhptr%wij(2)
     w3=pblhptr%wij(3)
     w4=pblhptr%wij(4)

!    Forward model
     val=w1*spblh(j1)+w2*spblh(j2)&
        +w3*spblh(j3)+w4*spblh(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*pblhptr%raterr2*pblhptr%err2
           pblhptr%diags%obssen(jiter) = grad
        else
           if (pblhptr%luse) pblhptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs)   val=val-pblhptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. pblhptr%pg > tiny_r_kind .and. &
                                pblhptr%b  > tiny_r_kind) then
              pg_pblh=pblhptr%pg*varqc_iter
              cg_pblh=cg_term/pblhptr%b
              wnotgross= one-pg_pblh
              wgross = pg_pblh*cg_pblh/wnotgross
              p0   = wgross/(wgross+exp(-half*pblhptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs ) then
           grad = val
           else
           grad = val*pblhptr%raterr2*pblhptr%err2
           end if
        endif

!       Adjoint
        rpblh(j1)=rpblh(j1)+w1*grad
        rpblh(j2)=rpblh(j2)+w2*grad
        rpblh(j3)=rpblh(j3)+w3*grad
        rpblh(j4)=rpblh(j4)+w4*grad
     endif

     pblhptr => pblhptr%llpoint

  end do

  return
end subroutine intpblh

end module intpblhmod
