module adjtest_obs
!$$$ module documentation block
!           .      .    .                                       .
! module:   adjtest_obs
!  prgmmr: Syed RH Rizvi, NCAR/NESL/MMM/DAS
!
! abstract: Performs adjoint test for linear observation operator
!
! program history log:
!   2012-09-14  Rizvi, NCAR/NESL/MMM/DAS - initial code
!   2015-09-03  guo     - obsmod::yobs has been replaced with m_obsHeadBundle,
!                         where yobs is created and destroyed when and where it
!                         is needed.
!   2016-05-05  guo     - merged in 33 obs-types in class-modules.
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-07-25  guo     - Merging with new obs-types (upto 34) are not complete, except
!                         simple USE references to all new obs-types.
!   2016-07-25  guo     - Replaced local get_lhs() with obsLList_getTLDdotprod(),
!                         which completes actuall "LHS" computations, with a
!                         polymorphic imeplementation.
!                       . The new implementation is an incremental step to be
!                         more object-oriented.
!                       . The new implementation also replaced "LHS" with a name
!                         TLDdotprod, to reflect its math definition explicitly.
!
! subroutines included:
!   sub adtest_obs
!   sub get_lhs
!
! variable definition:
!
! attributes:
!   language: f90
!   machine: NCAR/IBM
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: lsqrtb, nsubwin, nobs_bins

  use jfunc, only: jiter
  use constants, only: zero, two
  use mpimod, only: npe, ierror,mpi_comm_world, mpi_sum,mpi_integer4,mpi_rtype,mype
  use control_vectors, only: control_vector,allocate_cv,random_cv, deallocate_cv,assignment(=)
  use state_vectors, only: allocate_state,deallocate_state,prt_state_norms,dot_product
  use gsi_bundlemod, only : gsi_bundle,assignment(=)
  use bias_predictors, only: predictors,allocate_preds,deallocate_preds, assignment(=)
  use intallmod, only: intall

  implicit none
  private
  public adtest_obs

contains

subroutine adtest_obs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    adtest_obs
!  prgmmr: Syed RH Rizvi, NCAR/NESL/MMM/DAS
!
! abstract: Performs adjoint test for linear observation operator
!
! program history log:
!
!   2012-09-14  Rizvi, NCAR/NESL/MMM/DAS - initial code
!
!   input argument list:
!    xhat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine: NCAR/IBM
!
!$$$ end documentation block

  use m_obsdiags, only: obsLLists
  use m_obsLList, only: obsLList_getTLDdotprod
  use control2state_mod, only: control2state

  implicit none

! Declare local variables  
  type(control_vector) :: xtest1
  type(gsi_bundle) :: mval(nsubwin)
  type(gsi_bundle) :: stest1(nobs_bins)
  type(gsi_bundle) :: rval(nobs_bins)
  type(predictors) :: sbias1
  type(predictors) :: rbias1
  integer(i_kind)  :: ii,idig,jj
  integer(i_kind)  :: nob,nnode
  real(r_kind) :: adj_lhs, adj_rhs, adj_lhs_tmp, adj_rhs_tmp
  real(r_kind) :: adj_rhs_tsen_tmp, adj_rhs_tsen
  real(r_kind) :: zz1, zz2, zz3

! ----------------------------------------------------------------------
  if (mype==0) write(6,*)'ADTEST_OBS starting for jiter= ',jiter
! Allocate local variables
  call allocate_cv(xtest1)

  do ii=1,nsubwin
     call allocate_state(mval(ii))
  end do

  do ii=1,nobs_bins 
     call allocate_state(stest1(ii))
  end do
  call allocate_preds(sbias1)

! Initialize control space vectors
  call random_cv(xtest1)

  do ii=1,nsubwin   
     mval(ii)=zero
  enddo
  sbias1=zero
  lsqrtb = .false.
  call control2state(xtest1,mval,sbias1)

  do ii=1,nobs_bins
     stest1(ii)=mval(1)    
  enddo

  do ii = 1, nobs_bins
     call prt_state_norms(stest1(ii),'Initial input')
  end do

! Run obs adj test

  do ii=1,nobs_bins 
     call allocate_state(rval(ii))
  end do
  call allocate_preds(rbias1)


  call intall(stest1,sbias1,rval,rbias1)

  do ii = 1, nobs_bins
     call prt_state_norms(rval(ii),'After Adjoint')
  end do

 
  adj_rhs_tmp =0._r_kind
  adj_rhs     =0._r_kind
  adj_rhs_tsen=0._r_kind
  adj_rhs_tsen_tmp=0._r_kind

  adj_rhs = dot_product(stest1,rval)
  if(mype == 0) write(6,*)'Final obs_adj_RHS= ',adj_rhs
! Now compute LHS
  adj_lhs_tmp = 0._r_kind
  adj_lhs     = 0._r_kind

  nob  =0       ! count of observations
  nnode=0       ! count of obsNode
  do ii=1,size(obsLLists,2)         ! loop over nobs_bins
    do jj=1,size(obsLLists,1)       ! loop over nobs_type
      call obsLList_getTLDdotprod(obsLLists(jj,ii),jiter,adj_lhs_tmp,nnode=nnode,nob=nob)
    enddo
  enddo

  call mpi_allreduce(adj_lhs_tmp,adj_lhs,1,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
  if(mype == 0) write(6,*)'Final obs_adj_LHS= ',adj_lhs

! Diagnostics
  zz1 = adj_rhs
  zz2 = adj_lhs
!
  if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
     zz3=two*abs(zz1-zz2)/(zz1+zz2)
  else
     zz3=abs(zz1-zz2)
  endif
  idig= int(-log(zz3+tiny(zz3))/log(10.0_r_kind))


  if (mype==0) then
     write(6,'(A)')' ADTEST_OBS             0.123456789012345678'
     write(6,'(A,ES25.18)')' ADTEST_OBS <F*F.Y,X>= ',zz1
     write(6,'(A,ES25.18)')' ADTEST_OBS <F.Y,F.Y>= ',zz2
     write(6,'(A,i3,   A)')' ADTEST_OBS ',idig,' digits are identical'
     write(6,'(A,ES25.18)')' ADTEST_OBS rel. err.= ',zz3
     write(6,'(A,ES25.18)')' ADTEST_OBS mach.eps = ',epsilon(zz3)
  endif

! Release local variables
  call deallocate_cv(xtest1)
  do ii=1,nsubwin
     call deallocate_state(stest1(ii))
  enddo
  call deallocate_preds(sbias1)
! ----------------------------------------------------------------------

  if (mype==0) write(6,*)'ADTEST_OBS finished'
  return
end subroutine adtest_obs

end module adjtest_obs
