module intvismod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intvismod    module for intvis and its tangent linear intvis_tl
!   prgmmr:
!
! abstract: module for intvis and its tangent linear intvis_tl
!
! program history log:
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intvis
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
PUBLIC intvis

contains

subroutine intvis(vishead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intvis      apply nonlin qc obs operator for conv. vis
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: apply observation operator and adjoint for conventional vis
!           observations with nonlinear qc operator
!
! program history log:
!
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     vishead
!     svis    - increment in grid space
!     rvis
!
!   output argument list:
!     rvis    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: vis_ob_type, lsaveobsens, l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(vis_ob_type),pointer,intent(in   ) :: vishead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_vis,p0,grad,wnotgross,wgross,pg_vis
  real(r_kind),pointer,dimension(:) :: svis
  real(r_kind),pointer,dimension(:) :: rvis
  type(vis_ob_type), pointer :: visptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vis',svis,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vis',rvis,istatus);ier=istatus+ier
  if(ier/=0)return

  visptr => vishead
  do while (associated(visptr))
     j1=visptr%ij(1)
     j2=visptr%ij(2)
     j3=visptr%ij(3)
     j4=visptr%ij(4)
     w1=visptr%wij(1)
     w2=visptr%wij(2)
     w3=visptr%wij(3)
     w4=visptr%wij(4)

!    Forward model
     val=w1*svis(j1)+w2*svis(j2)&
        +w3*svis(j3)+w4*svis(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*visptr%raterr2*visptr%err2
           visptr%diags%obssen(jiter) = grad
        else
           if (visptr%luse) visptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not. ladtest_obs)  val=val-visptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. visptr%pg > tiny_r_kind .and. &
                                visptr%b  > tiny_r_kind) then
              pg_vis=visptr%pg*varqc_iter
              cg_vis=cg_term/visptr%b
              wnotgross= one-pg_vis
              wgross = pg_vis*cg_vis/wnotgross
              p0   = wgross/(wgross+exp(-half*visptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*visptr%raterr2*visptr%err2
           end if
        endif

!       Adjoint
        rvis(j1)=rvis(j1)+w1*grad
        rvis(j2)=rvis(j2)+w2*grad
        rvis(j3)=rvis(j3)+w3*grad
        rvis(j4)=rvis(j4)+w4*grad
     endif

     visptr => visptr%llpoint

  end do

  return
end subroutine intvis

end module intvismod
