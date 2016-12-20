module intsstmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intsstmod    module for intsst and its tangent linear intsst_tl
!   prgmmr:
!
! abstract: module for intsst and its tangent linear intsst_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intsst and its tangent linear intsst_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intsst_tl
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
! subroutines included:
!   sub intsst
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
PUBLIC intsst

contains

subroutine intsst(ssthead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsst      apply nonlin qc obs operator for conv. sst
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: apply observation operator and adjoint for conventional sst
!           observations with nonlinear qc operator
!
! program history log:
!   2004-07-20  derber
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsst and intsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2010-05-13  todling  - update to use gsi_bundle; update interface
!   2011-04-01  li       - modify to include Tr analysis
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!
!   input argument list:
!     ssthead
!     ssst    - increment in grid space
!     rsst
!
!   output argument list:
!     rsst    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: sst_ob_type, lsaveobsens, l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use gsi_nstcouplermod, only: nst_gsi
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(sst_ob_type),pointer,intent(in   ) :: ssthead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) tval,tdir
  real(r_kind) cg_sst,p0,grad,wnotgross,wgross,pg_sst
  real(r_kind),pointer,dimension(:) :: ssst
  real(r_kind),pointer,dimension(:) :: rsst
  type(sst_ob_type), pointer :: sstptr

!  If no sst data return
  if(.not. associated(ssthead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'sst',ssst,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'sst',rsst,istatus);ier=istatus+ier
  if(ier/=0)return

  sstptr => ssthead
  do while (associated(sstptr))
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

!    Forward model
     val=w1*ssst(j1)+w2*ssst(j2)&
        +w3*ssst(j3)+w4*ssst(j4)

     if ( nst_gsi > 2 ) then
       tdir = w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)         ! Forward
       val  = tdir*sstptr%tz_tr                                       ! Include contributions from Tz jacobian
     else
       val = w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)          ! Forward
     endif


     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*sstptr%raterr2*sstptr%err2
           sstptr%diags%obssen(jiter) = grad
        else
           if (sstptr%luse) sstptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs) val=val-sstptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and. &
                                sstptr%b  > tiny_r_kind) then
              pg_sst=sstptr%pg*varqc_iter
              cg_sst=cg_term/sstptr%b
              wnotgross= one-pg_sst
              wgross = pg_sst*cg_sst/wnotgross
              p0   = wgross/(wgross+exp(-half*sstptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*sstptr%raterr2*sstptr%err2
           end if
        endif

!      Adjoint
       if ( nst_gsi > 2 ) then
         tval = sstptr%tz_tr*grad                     ! Extract contributions from surface jacobian
         rsst(j1)=rsst(j1)+w1*tval                    ! Distribute adjoint contributions over surrounding grid points
         rsst(j2)=rsst(j2)+w2*tval
         rsst(j3)=rsst(j3)+w3*tval
         rsst(j4)=rsst(j4)+w4*tval
       else
         rsst(j1)=rsst(j1)+w1*grad
         rsst(j2)=rsst(j2)+w2*grad
         rsst(j3)=rsst(j3)+w3*grad
         rsst(j4)=rsst(j4)+w4*grad
       endif

     endif                           ! if (l_do_adjoint) then

     sstptr => sstptr%llpoint

  end do

  return
end subroutine intsst

end module intsstmod
