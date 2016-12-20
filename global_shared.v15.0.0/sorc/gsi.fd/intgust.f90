module intgustmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intgustmod    module for intgust and its tangent linear intgust_tl
!   prgmmr:
!
! abstract: module for intgust and its tangent linear intgust_tl
!
! program history log:
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intgust
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
PUBLIC intgust

contains

subroutine intgust(gusthead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intgust      apply nonlin qc obs operator for conv. gust
!   prgmmr: zhu           org: np23                date: 2011-02-20
!
! abstract: apply observation operator and adjoint for conventional gust
!           observations with nonlinear qc operator
!
! program history log:
!
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     gusthead
!     sgust    - increment in grid space
!     rgust
!
!   output argument list:
!     rgust    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: gust_ob_type, lsaveobsens, l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(gust_ob_type),pointer,intent(in   ) :: gusthead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_gust,p0,grad,wnotgross,wgross,pg_gust
  real(r_kind),pointer,dimension(:) :: sgust
  real(r_kind),pointer,dimension(:) :: rgust
  type(gust_ob_type), pointer :: gustptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'gust',sgust,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'gust',rgust,istatus);ier=istatus+ier
  if(ier/=0)return

  gustptr => gusthead
  do while (associated(gustptr))
     j1=gustptr%ij(1)
     j2=gustptr%ij(2)
     j3=gustptr%ij(3)
     j4=gustptr%ij(4)
     w1=gustptr%wij(1)
     w2=gustptr%wij(2)
     w3=gustptr%wij(3)
     w4=gustptr%wij(4)

!    Forward model
     val=w1*sgust(j1)+w2*sgust(j2)&
        +w3*sgust(j3)+w4*sgust(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*gustptr%raterr2*gustptr%err2
           gustptr%diags%obssen(jiter) = grad
        else
           if (gustptr%luse) gustptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-gustptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. gustptr%pg > tiny_r_kind .and. &
                                gustptr%b  > tiny_r_kind) then
              pg_gust=gustptr%pg*varqc_iter
              cg_gust=cg_term/gustptr%b
              wnotgross= one-pg_gust
              wgross = pg_gust*cg_gust/wnotgross
              p0   = wgross/(wgross+exp(-half*gustptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*gustptr%raterr2*gustptr%err2
           end if
        endif

!       Adjoint
        rgust(j1)=rgust(j1)+w1*grad
        rgust(j2)=rgust(j2)+w2*grad
        rgust(j3)=rgust(j3)+w3*grad
        rgust(j4)=rgust(j4)+w4*grad
     endif

     gustptr => gustptr%llpoint

  end do

  return
end subroutine intgust

end module intgustmod
