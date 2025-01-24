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
!   2018-08-10  j guo   - moved in all type-indices from setuprhsall().  These
!                         type-indices are now defined from this module itself,
!                         through an enum block.
!                       - removed external dimension argument aworkdim2 of
!                         rhs_alloc().
!   2022-03-15  K. Apodaca - add GNSS-R L2 ocean wind speed index (i_gnssrspd)
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

  ! variable indices to rhs_awork(:,i_work).  e.g.
  !   ps_awork(:) => rhs_awork(:,i_ps)
  public:: i_ps
  public:: i_uv
  public:: i_t
  public:: i_q
  public:: i_pw
  public:: i_rw
  public:: i_dw
  public:: i_gps
  public:: i_sst
  public:: i_tcp
  public:: i_lag
  public:: i_co
  public:: i_gust
  public:: i_vis
  public:: i_pblh
  public:: i_wspd10m
  public:: i_gnssrspd
  public:: i_td2m
  public:: i_mxtm
  public:: i_mitm
  public:: i_pmsl
  public:: i_howv
  public:: i_tcamt
  public:: i_lcbas
  public:: i_cldch
  public:: i_uwnd10m
  public:: i_vwnd10m
  public:: i_swcp
  public:: i_lwcp
  public:: i_light
  public:: i_dbz
  public:: i_fed
  public:: i_cldtot

  public:: awork_size
  public:: awork_lbound
  public:: awork_ubound

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

  enum, bind(C)
    enumerator:: i_zero = 0

    enumerator:: i_ps
    enumerator:: i_uv
    enumerator:: i_t
    enumerator:: i_q
    enumerator:: i_pw
    enumerator:: i_rw
    enumerator:: i_dw
    enumerator:: i_gps
    enumerator:: i_sst
    enumerator:: i_tcp
    enumerator:: i_lag
    enumerator:: i_co
    enumerator:: i_gust
    enumerator:: i_vis
    enumerator:: i_pblh
    enumerator:: i_wspd10m
    enumerator:: i_gnssrspd
    enumerator:: i_td2m
    enumerator:: i_mxtm
    enumerator:: i_mitm
    enumerator:: i_pmsl
    enumerator:: i_howv
    enumerator:: i_tcamt
    enumerator:: i_lcbas
    enumerator:: i_cldch
    enumerator:: i_uwnd10m
    enumerator:: i_vwnd10m
    enumerator:: i_swcp
    enumerator:: i_lwcp
    enumerator:: i_light
    enumerator:: i_dbz
    enumerator:: i_fed
    enumerator:: i_cldtot

    enumerator:: i_outbound
  end enum

  integer(i_kind)        ,parameter:: enum_kind    = kind(i_zero)
  integer(kind=enum_kind),parameter:: awork_lbound = i_zero    +1
  integer(kind=enum_kind),parameter:: awork_ubound = i_outbound-1
  integer(kind=enum_kind),parameter:: awork_size   = awork_ubound-awork_lbound +1

  character(len=*),parameter:: myname="m_rhs"

contains
subroutine rhs_alloc()
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
  use gsi_io, only: verbose
  implicit none
  character(len=*),parameter:: myname_=myname//'.alloc'
  logical print_verbose
_ENTRY_(myname_)
  print_verbose=.false.
  if(verbose) print_verbose=.true.
  if(rhs_allocated) call die(myname_,'already allocated')

  if(print_verbose)then
     call tell(myname_,'nsig ='       ,nsig)
     call tell(myname_,'npres_print =',npres_print)
     call tell(myname_,'nconvtype ='  ,nconvtype)
     call tell(myname_,'ndat ='       ,ndat)
     call tell(myname_,'jpch_rad ='   ,jpch_rad)
     call tell(myname_,'jpch_co ='    ,jpch_co)
     call tell(myname_,'jpch_oz ='    ,jpch_oz)
     call tell(myname_,'nprof_gps ='  ,nprof_gps)
  end if

  rhs_allocated=.true.
  allocate(rhs_awork(7*nsig+100,awork_size))
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
