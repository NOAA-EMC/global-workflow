module m_rhs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_rhs
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: defines persistant workspace for multiple-pass setuprhsall()
!
! program history log:
!   2010-03-22  j guo   - added this document block
!   2010-04-22  tangborn- add co knobs
!   2010-05-27  j guo   - cut off GPS related variables to m_gpsrhs
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

!#define VERBOSE
#include "mytrace.H"

! module interface:

  use kinds, only: r_kind, i_kind, r_single
  use mpeu_util, only: die,perr,tell
  implicit none
  private
  public:: rhs_alloc            ! interface for allocation
  public:: rhs_dealloc          ! interface for deallocation
  public:: rhs_allocated        ! state of all moduel variables

  public:: rhs_awork            ! variables ...
  public:: rhs_bwork
  public:: rhs_aivals
  public:: rhs_stats
  public:: rhs_stats_oz
  public:: rhs_stats_co
  public:: rhs_toss_gps

! Revision history:
!   2009-08-19  guo     - created to support multi-pass setuprhsall().
!                         This module contains all statistics variables
!                         defined for any single pass but all passes.

  !! usage:
  !!    use xyz_mod, only: npres_print,nconvtype,nsig
  !!    use m_rhs, only: rhs_alloc
  !!    use m_rhs, only: rhs_dealloc
  !!    use m_rhs, only: rhs_allocated
  !!    use m_rhs, only: awork => rhs_awork
  !!    use m_rhs, only: bwork => rhs_bwork
  !!
  !!    if(.not.rhs_allocated) &
  !!    call rhs_alloc()
  !!    call xxxx(awork,bwork,...)
  !!    call rhs_dealloc()

  logical,save:: rhs_allocated=.false.
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_awork
  real(r_kind),allocatable,dimension(:,:,:,:),save:: rhs_bwork
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_aivals
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_stats
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_stats_oz
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_stats_co
  real(r_kind),allocatable,dimension(:      ),save:: rhs_toss_gps

  character(len=*),parameter:: myname="m_rhs"

contains
subroutine rhs_alloc(aworkdim2)
  ! supporting information
  use kinds, only: i_kind
  use constants, only: zero

  ! run-time dimensional information
  use obsmod  , only: ndat
  use obsmod  , only: nprof_gps
  use radinfo , only: jpch_rad
  use ozinfo  , only: jpch_oz
  use coinfo  , only: jpch_co
  use qcmod   , only: npres_print
  use gridmod , only: nsig
  use convinfo, only: nconvtype

  ! indirectly used counter
  use obsmod  , only: nchan_total
  implicit none
  integer(i_kind),optional,intent(in):: aworkdim2
  character(len=*),parameter:: myname_=myname//'.alloc'
  integer(i_kind):: aworkdim2_
_ENTRY_(myname_)
  if(rhs_allocated) call die(myname_,'already allocated')
  aworkdim2_=13
  if(present(aworkdim2)) aworkdim2_=aworkdim2

#ifdef VERBOSE
  call tell(myname_,'nsig ='       ,nsig)
  call tell(myname_,'npres_print =',npres_print)
  call tell(myname_,'nconvtype ='  ,nconvtype)
  call tell(myname_,'ndat ='       ,ndat)
  call tell(myname_,'jpch_rad ='   ,jpch_rad)
  call tell(myname_,'jpch_co ='    ,jpch_co)
  call tell(myname_,'jpch_oz ='    ,jpch_oz)
  call tell(myname_,'nprof_gps ='  ,nprof_gps)
  call tell(myname_,'aworkdim2 ='  ,aworkdim2_)
#endif

  rhs_allocated=.true.
  allocate(rhs_awork(7*nsig+100,aworkdim2_))
  allocate(rhs_bwork(npres_print,nconvtype,5,3))
  allocate(rhs_aivals(40,ndat))
  allocate(rhs_stats(7,jpch_rad))
  allocate(rhs_stats_co(9,jpch_co))
  allocate(rhs_stats_oz(9,jpch_oz))

  allocate(rhs_toss_gps(max(1,nprof_gps)))

  rhs_awork    =zero
  rhs_bwork    =zero
  rhs_aivals   =zero
  rhs_stats    =zero
  rhs_stats_co =zero
  rhs_stats_oz =zero
  rhs_toss_gps =zero

  nchan_total      =0
_EXIT_(myname_)
end subroutine rhs_alloc

subroutine rhs_dealloc()
  use kinds, only: i_kind
  implicit none
  character(len=*),parameter:: myname_=myname//'.dealloc'
_ENTRY_(myname_)
  if(.not.rhs_allocated) call die(myname_,'can not be deallocted')

  rhs_allocated=.false.
  deallocate(rhs_awork)
  deallocate(rhs_bwork)
  deallocate(rhs_aivals)
  deallocate(rhs_stats)
  deallocate(rhs_stats_co)
  deallocate(rhs_stats_oz)

  deallocate(rhs_toss_gps)
_EXIT_(myname_)
end subroutine rhs_dealloc

end module m_rhs
