module m_obdiag
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_obdiag
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-17
!
! abstract: coupler of type(*_ob_type) and type(obsdiags)
!
! program history log:
!   2009-12-01  Guo - initial implementation
!   2009-12-09  Guo - changed an argument name from "size" to "count".
!   2009-12-09  Guo - fixed an error in verification of %nloz
!   2010-03-17  j guo   - added this document block
!   2010-04-27  tangborn - added carbon monoxide
!   2010-05-26  treadon - add tcp_verify to ob_verify interface
!   2011-05-18  todling - add aero, aerol, and pm2_5
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

! module interface:

#include "mytrace.H"
  use kinds, only: i_kind
  use mpeu_util, only: perr,die,tell
  use obsmod, only: obs_diag
  implicit none
  private
  public:: obdiag_locate
  public:: obdiag_buildSearcher
  public:: obdiag_cleanSearcher
  public:: ob_verify

! Usecase - link %diag of an ob_type node to the right node in an obsdiags list.
!
!       use obsmod, only :: obsdiags
!       use m_obdiag,only :: obdiag_buildSearch
!       use m_obdiag,only :: obdiag_cleanSearch
!       use m_obdiag,only :: obdiag_locate
!       use m_obdiag,only :: obdiag_verify
!
!!! for an obsdiags already build
!       call obdiag_buildSearcher(obsdiags)     ! create an search object for the given obsdiags
!       do ...                                  ! loop through a list of x_ob_type
!         my_node => xxx
!         my_node%diags => obdiag_locate(obsdiags,my_node%idv,my_node%iob,1,who=myname_)
!         if(.not.associated(my_diag)) then
!           call perr(myname_,'can not locate, (idv,iob,ich) =',(/idv,iob,ich/))
!           call die(myname_)
!         endif
!       enddo
!       call obdiag_cleanSearcher(obsdiags)

  interface ob_verify; module procedure &
     ps_verify_, &  ! 1
      t_verify_, &  ! 2
      w_verify_, &  ! 3
      q_verify_, &  ! 4
    spd_verify_, &  ! 5
    srw_verify_, &  ! 6
     rw_verify_, &  ! 7
     dw_verify_, &  ! 8
    sst_verify_, &  ! 9
     pw_verify_, &  ! 10
    pcp_verify_, &  ! 11
     oz_verify_, &  ! 12
    o3l_verify_, &  ! 13
    gps_verify_, &  ! 14
    rad_verify_, &  ! 15
    tcp_verify_, &  ! 16
    lag_verify_, &  ! 17
  colvk_verify_, &  ! 18
   aero_verify_, &  ! 19
  aerol_verify_, &  ! 20
  pm2_5_verify_, &  ! 21
   pm10_verify_; end interface


!!! usage:
!!!
!!!    ptr => obdiag_locate(obsdiags(jj,ii),idv,iob,ich)
!!!    if(.not.associated(ptr)) then
!!!      call perr(myname_,'not located, (idv,iob,ich) =',(/idv,iob,ich/))
!!!      call die(myname_)
!!!    endif

  character(len=*),parameter:: myname='m_obdiag'
  character(len=*),parameter:: ob_verify_name=myname//'.ob_verify'

type obdiag_link
  private
  type(obs_diag),pointer:: diag
  integer(i_kind),dimension(3):: keys
end type obdiag_link

type(obdiag_link),dimension(:),allocatable,save:: obdiag_Searcher

logical,save:: SKIP_VERIFY_=.false.
contains
subroutine obdiag_buildSearcher(obsdiags)
  use obsmod, only: obs_diags
  use obsmod, only: obs_diag
  use mpeu_util, only: IndexSet
  use mpeu_util, only: IndexSort
  use timermod, only: timer_ini,timer_fnl
  use mpeu_util, only: die
  implicit none
  type(obs_diags),intent(in):: obsdiags ! as it is named in obsmod

  character(len=*),parameter:: myname_=myname//'.buildSearcher'
  integer(i_kind),allocatable,dimension(:):: indx
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n,i

_ENTRY_(myname_)
    call timer_ini(myname_)

  n=obsdiags%n_alloc
  allocate(obdiag_Searcher(n),indx(n))

  my_diag => obsdiags%head
  i=0
  do while(associated(my_diag))
    i=i+1
    if(i<=n) then
      obdiag_Searcher(i)%diag => my_diag
      obdiag_Searcher(i)%keys(1:3)=(/my_diag%idv,my_diag%iob,my_diag%ich/)
    endif
    my_diag => my_diag%next
  enddo

  if(i/=n) call die(myname_,'bad dimension from obsdiags, (%n_alloc,n_count) =',(/n,i/))

  call IndexSet(indx)
  call IndexSort(indx,obdiag_Searcher(:)%keys(3))
  call IndexSort(indx,obdiag_Searcher(:)%keys(2))
  call IndexSort(indx,obdiag_Searcher(:)%keys(1))

  obdiag_Searcher(1:n) = obdiag_Searcher(indx(1:n))
  deallocate(indx)
    call timer_fnl(myname_)
_EXIT_(myname_)
end subroutine obdiag_buildSearcher

subroutine obdiag_cleanSearcher()
  implicit none
  deallocate(obdiag_Searcher)
end subroutine obdiag_cleanSearcher

function obdiag_locate(obsdiags,idv,iob,ich,who) result(my_diag)
!!! this version implements a simple linear search.  A fater
!!! binary tree search can be implemented for efficiency.
  use obsmod, only: obs_diags
  use obsmod, only: obs_diag
  use timermod, only: timer_ini,timer_fnl
  use mpeu_util, only: die,tell
  implicit none
  type(obs_diags),intent(in):: obsdiags ! as it is named in obsmod
  integer(i_kind),intent(in) :: idv,iob,ich
  character(len=*),optional,intent(in):: who

  character(len=*),parameter:: myname_=myname//".locate"
  type(obs_diag),pointer:: my_diag

  integer(i_kind):: m,i,lb,ub
  logical:: done
    call timer_ini(myname_)

#define BINARY_SEARCH
#ifdef BINARY_SEARCH
  if(.not.allocated(obdiag_Searcher)) call die(myname_,'obdiag_Searcher is not built')

  my_diag => null()      ! return null() if the key is not located.
  done=.false.
  lb=1; ub=size(obdiag_Searcher)
  do while(.not.done)
    i=(lb+ub)/2
    m=compare((/idv,iob,ich/),obdiag_Searcher(i)%keys)
    done = m==0
    if(done) exit

    ! We are searching for EQUAL, so skip the i-th point if not equal.
    if(m<0) then        ! keys < (i)%keys
      ub=i-1
    else                ! keys > (i)%keys
      lb=i+1
    endif

    if(ub<lb) exit      ! termionate the search
  enddo
  if(done) my_diag => obdiag_Searcher(i)%diag

#else
  my_diag => obsdiags%head
  do while(associated(my_diag))
    if( idv == my_diag%idv .and. &
        iob == my_diag%iob .and. &
        ich == my_diag%ich ) exit
    my_diag => my_diag%next
  enddo
#endif
    call timer_fnl(myname_)

contains
function compare(key1,key2) result (m)
  integer,dimension(:),intent(in):: key1,key2
  integer:: m

  integer:: n,i
  m=0
  n=min(size(key1),size(key2))
  do i=1,n
    if    (key1(i)<key2(i)) then
      m=-1; exit
    elseif(key1(i)>key2(i)) then
      m=+1; exit
    endif
  enddo
end function compare
end function obdiag_locate

function ps_verify_(hd,count,perr) result(good)
  use obsmod,only: ps_ob_head
  use obsmod,only: ps_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(ps_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.ps_verify_'

  logical:: perr_
  type(ps_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head     ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function ps_verify_
function t_verify_(hd,count,perr) result(good)
  use obsmod,only: t_ob_head
  use obsmod,only: t_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(t_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.t_verify_'

  logical:: perr_
  type(t_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head    ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function t_verify_
function q_verify_(hd,count,perr) result(good)
  use obsmod,only: q_ob_head
  use obsmod,only: q_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(q_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.q_verify_'

  logical:: perr_
  type(q_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head     ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function q_verify_
function spd_verify_(hd,count,perr) result(good)
  use obsmod,only: spd_ob_head
  use obsmod,only: spd_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(spd_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.spd_verify_'

  logical:: perr_
  type(spd_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function spd_verify_
function dw_verify_(hd,count,perr) result(good)
  use obsmod,only: dw_ob_head
  use obsmod,only: dw_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(dw_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.dw_verify_'

  logical:: perr_
  type(dw_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function dw_verify_
function o3l_verify_(hd,count,perr) result(good)
  use obsmod,only: o3l_ob_head
  use obsmod,only: o3l_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(o3l_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.o3l_verify_'

  logical:: perr_
  type(o3l_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function o3l_verify_
function pcp_verify_(hd,count,perr) result(good)
  use obsmod,only: pcp_ob_head
  use obsmod,only: pcp_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(pcp_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.pcp_verify_'

  logical:: perr_
  type(pcp_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head    ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function pcp_verify_
function pw_verify_(hd,count,perr) result(good)
  use obsmod,only: pw_ob_head
  use obsmod,only: pw_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(pw_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.pw_verify_'

  logical:: perr_
  type(pw_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function pw_verify_
function gps_verify_(hd,count,perr) result(good)
  use obsmod,only: gps_ob_head
  use obsmod,only: gps_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(gps_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.gps_verify_'

  logical:: perr_
  type(gps_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head     ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function gps_verify_
function rw_verify_(hd,count,perr) result(good)
  use obsmod,only: rw_ob_head
  use obsmod,only: rw_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(rw_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.rw_verify_'

  logical:: perr_
  type(rw_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function rw_verify_
function sst_verify_(hd,count,perr) result(good)
  use obsmod,only: sst_ob_head
  use obsmod,only: sst_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(sst_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.sst_verify_'

  logical:: perr_
  type(sst_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function sst_verify_
function tcp_verify_(hd,count,perr) result(good)
  use obsmod,only: tcp_ob_head
  use obsmod,only: tcp_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(tcp_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.tcp_verify_'

  logical:: perr_
  type(tcp_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function tcp_verify_

function w_verify_(hd,count,perr) result(good)
  use obsmod,only: w_ob_head
  use obsmod,only: w_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(w_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.w_verify_'

  logical:: perr_
  type(w_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diagu,my_diagv
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diagu => my_node%diagu
      my_diagv => my_node%diagv
      good = associated(my_diagu).and.associated(my_diagv)
      if(.not.good .and. perr_) then
        if(.not.associated(my_diagu)) call iperr(myname_,'unassociated %diagu, @count =',n)
        if(.not.associated(my_diagv)) call iperr(myname_,'unassociated %diagv, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diagu%idv .and. &
               my_node%iob == my_diagu%iob .and. &
                         1 == my_diagu%ich .and. &
               my_node%idv == my_diagv%idv .and. &
               my_node%iob == my_diagv%iob .and. &
                         2 == my_diagv%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ichu,ichv) =',(/my_node %idv,my_node %iob,1,2/))
          call iperr(myname_,'%diagu%(idv,iob,ich) =',(/my_diagu%idv,my_diagu%iob,my_diagu%ich/))
          call iperr(myname_,'%diagv%(idv,iob,ich) =',(/my_diagv%idv,my_diagv%iob,my_diagv%ich/))
        endif
      endif
    endif

      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 

    my_node => my_node%llpoint ! next
  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function w_verify_
function srw_verify_(hd,count,perr) result(good)
  use obsmod,only: srw_ob_head
  use obsmod,only: srw_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(srw_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.srw_verify_'

  logical:: perr_
  type(srw_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diagu,my_diagv
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diagu => my_node%diagu
      my_diagv => my_node%diagv
      good = associated(my_diagu).and.associated(my_diagv)
      if(.not.good .and. perr_) then
        if(.not.associated(my_diagu)) call iperr(myname_,'unassociated %diagu, @count =',n)
        if(.not.associated(my_diagv)) call iperr(myname_,'unassociated %diagv, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diagu%idv .and. &
               my_node%iob == my_diagu%iob .and. &
                         1 == my_diagu%ich .and. &
               my_node%idv == my_diagv%idv .and. &
               my_node%iob == my_diagv%iob .and. &
                         2 == my_diagv%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ichu,ichv) =',(/my_node %idv,my_node %iob,1,2/))
          call iperr(myname_,'%diagu%(idv,iob,ich) =',(/my_diagu%idv,my_diagu%iob,my_diagu%ich/))
          call iperr(myname_,'%diagv%(idv,iob,ich) =',(/my_diagv%idv,my_diagv%iob,my_diagv%ich/))
        endif
      endif
    endif

      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 

    my_node => my_node%llpoint ! next
  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function srw_verify_
function lag_verify_(hd,count,perr) result(good)
  use obsmod,only: lag_ob_head
  use obsmod,only: lag_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(lag_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.lag_verify_'

  logical:: perr_
  type(lag_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diagx,my_diagy
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diagx => my_node%diag_lon
      my_diagy => my_node%diag_lat
      good = associated(my_diagx).and.associated(my_diagy)
      if(.not.good .and. perr_) then
        if(.not.associated(my_diagx)) call iperr(myname_,'unassociated %diag_lon, @count =',n)
        if(.not.associated(my_diagy)) call iperr(myname_,'unassociated %diag_lat, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diagx%idv .and. &
               my_node%iob == my_diagx%iob .and. &
                         1 == my_diagx%ich .and. &
               my_node%idv == my_diagy%idv .and. &
               my_node%iob == my_diagy%iob .and. &
                         2 == my_diagy%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ichu,ichv) =',(/my_node %idv,my_node %iob,1,2/))
          call iperr(myname_,'%diag_lon%(idv,iob,ich) =',(/my_diagx%idv,my_diagx%iob,my_diagx%ich/))
          call iperr(myname_,'%diag_lat%(idv,iob,ich) =',(/my_diagy%idv,my_diagy%iob,my_diagy%ich/))
        endif
      endif

      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 
    endif

    my_node => my_node%llpoint ! next
  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function lag_verify_
function oz_verify_(hd,count,perr) result(good)
  use obsmod,only: oz_ob_head
  use obsmod,only: oz_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(oz_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.oz_verify_'

  logical:: perr_
  type(oz_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: k,n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head  ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #0
      good = associated(my_node%diags)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated node%diags, @count =',n)
        call iperr(myname_,'node%(idv,iob,nloz) =',(/my_node%idv,my_node%iob,my_node%nloz/))
      endif

      ! check #0.1
      if(good) then
        good = my_node%nloz+1 == size(my_node%diags)
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching [%nloz,size(%diags)], @count =',n)
          call iperr(myname_,'node%(idv,iob,nloz,size(%diags)) =', &
                  (/my_node%idv,my_node%iob,my_node%nloz,size(my_node%diags)/))
        endif
      endif

      if(good) then
        do k=1,my_node%nloz+1
          my_diag => my_node%diags(k)%ptr

      ! check #1
          good = associated(my_diag)
          if(.not.good .and. perr_) then
            call iperr(myname_,'unassociated node%diags(k)%ptr, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
          endif

      ! check #2
          good = my_node%idv == my_diag%idv .and. &
                 my_node%iob == my_diag%iob .and. &
                         k   == my_diag%ich
          if(.not.good .and. perr_) then
            call iperr(myname_,'mismatching keys, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
            call iperr(myname_,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
          endif
        enddo
      endif
      
      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 
    endif

    my_node => my_node%llpoint ! next
  enddo

  ! check #3, is done when some other test is already failed.
  if(present(count)) then
    if(n/=count) then
      good=.false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function oz_verify_
function colvk_verify_(hd,count,perr) result(good)
  use obsmod,only: colvk_ob_head
  use obsmod,only: colvk_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(colvk_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.co_verify_'

  logical:: perr_
  type(colvk_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: k,n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head    ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
                ! check #0
      good = associated(my_node%diags)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated node%diags, @count =',n)
        call iperr(myname_,'node%(idv,iob,nlco) =',(/my_node%idv,my_node%iob,my_node%nlco/))
      endif

                ! check #0.1
      if(good) then
        good = my_node%nlco+1 == size(my_node%diags)
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching [%nlco,size(%diags)], @count =',n)
          call iperr(myname_,'node%(idv,iob,nlco,size(%diags)) =', &
                      (/my_node%idv,my_node%iob,my_node%nlco,size(my_node%diags)/))
        endif
      endif

      if(good) then
        do k=1,my_node%nlco+1
          my_diag => my_node%diags(k)%ptr

                ! check #1
          good = associated(my_diag)
          if(.not.good .and. perr_) then
            call iperr(myname_,'unassociated node%diags(k)%ptr, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
          endif

                ! check #2
          good = my_node%idv == my_diag%idv .and. &
                 my_node%iob == my_diag%iob .and. &
                         k   == my_diag%ich
          if(.not.good .and. perr_) then
            call iperr(myname_,'mismatching keys, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
            call iperr(myname_,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
          endif
        enddo
      endif

      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif
    endif

    my_node => my_node%llpoint  ! next
  enddo

                ! check #3, is done when some other test is already failed.
  if(present(count)) then
    if(n/=count) then
      good=.false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function colvk_verify_


function rad_verify_(hd,count,perr) result(good)
  use obsmod,only: rad_ob_head
  use obsmod,only: rad_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(rad_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.rad_verify_'

  logical:: perr_
  type(rad_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: k,n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #0
      good = associated(my_node%diags)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated node%diags, @count =',n)
        call iperr(myname_,'node%(idv,iob,nchan) =',(/my_node%idv,my_node%iob,my_node%nchan/))
      endif

      ! check #0.1
      if(good) then
        good = my_node%nchan == size(my_node%diags)
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching [%nchan,size(%diags)], @count =',n)
          call iperr(myname_,'node%(idv,iob,nchan,size(%diags)) =', &
                   (/my_node%idv,my_node%iob,my_node%nchan,size(my_node%diags)/))
        endif
      endif

      if(good) then
        do k=1,my_node%nchan
          my_diag => my_node%diags(k)%ptr

      ! check #1
          good = associated(my_diag)
          if(.not.good .and. perr_) then
            call iperr(myname_,'unassociated node%diags(k)%ptr, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,my_node%ich/))
          endif

      ! check #2
          good = my_node%idv    == my_diag%idv .and. &
                 my_node%iob    == my_diag%iob .and. &
                 my_node%ich(k) == my_diag%ich
          if(.not.good .and. perr_) then
            call iperr(myname_,'mismatching keys, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,my_node%ich(k)/))
            call iperr(myname_,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
          endif
        enddo
      endif
      
      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 
    endif

    my_node => my_node%llpoint ! next
  enddo

  ! check #3, is done when some other test is already failed.
  if(present(count)) then
    if(n/=count) then
      good=.false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function rad_verify_

function aero_verify_(hd,count,perr) result(good)
  use obsmod,only: aero_ob_head
  use obsmod,only: aero_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(aero_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.aero_verify_'

  logical:: perr_
  type(aero_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: k,n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #0
      good = associated(my_node%diags)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated node%diags, @count =',n)
        call iperr(myname_,'node%(idv,iob,nlaero) =',(/my_node%idv,my_node%iob,my_node%nlaero/))
      endif

      ! check #0.1
      if(good) then
        good = my_node%nlaero+1 == size(my_node%diags)
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching [%nlaero,size(%diags)], @count =',n)
          call iperr(myname_,'node%(idv,iob,nlaero,size(%diags)) =', &
                   (/my_node%idv,my_node%iob,my_node%nlaero,size(my_node%diags)/))
        endif
      endif

      if(good) then
        do k=1,my_node%nlaero+1
          my_diag => my_node%diags(k)%ptr

      ! check #1
          good = associated(my_diag)
          if(.not.good .and. perr_) then
            call iperr(myname_,'unassociated node%diags(k)%ptr, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
          endif

      ! check #2
          good = my_node%idv == my_diag%idv .and. &
                 my_node%iob == my_diag%iob .and. &
                         k   == my_diag%ich
          if(.not.good .and. perr_) then
            call iperr(myname_,'mismatching keys, @(count,k) =',(/n,k/))
            call iperr(myname_,'node%(idv,iob,ich) =',(/my_node%idv,my_node%iob,k/))
            call iperr(myname_,'diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
          endif
        enddo
      endif
      
      if(.not.(good.or.present(count))) then
        call iperr(myname_,'test failed, @count =',n)
        call timer_fnl(ob_verify_name)
_EXIT_(myname_)
        return
      endif 
    endif

    my_node => my_node%llpoint ! next
  enddo

  ! check #3, is done when some other test is already failed.
  if(present(count)) then
    if(n/=count) then
      good=.false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function aero_verify_

function aerol_verify_(hd,count,perr) result(good)
  use obsmod,only: aerol_ob_head
  use obsmod,only: aerol_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(aerol_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.aerol_verify_'

  logical:: perr_
  type(aerol_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head   ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function aerol_verify_

function pm2_5_verify_(hd,count,perr) result(good)
  use obsmod,only: pm2_5_ob_head
  use obsmod,only: pm2_5_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(pm2_5_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.pm2_5_verify_'

  logical:: perr_
  type(pm2_5_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head    ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function pm2_5_verify_

function pm10_verify_(hd,count,perr) result(good)
  use obsmod,only: pm10_ob_head
  use obsmod,only: pm10_ob_type
  use obsmod,only: obs_diag
  use mpeu_util, only: iperr => perr
  use timermod, only: timer_ini,timer_fnl
  implicit none
  logical:: good
  type(pm10_ob_head),intent(in) :: hd
  integer(i_kind),optional,intent(in) :: count
  logical,optional,intent(in) :: perr

  character(len=*),parameter :: myname_=myname//'.pm10_verify_'

  logical:: perr_
  type(pm10_ob_type),pointer:: my_node
  type(obs_diag),pointer:: my_diag
  integer(i_kind):: n
_ENTRY_(myname_)
  good = .true.
  if(SKIP_VERIFY_) then
_EXIT_(myname_)
    return
  endif
  call timer_ini(ob_verify_name)

  perr_=.false.
  if(present(perr)) perr_=perr

  my_node => hd%head    ! top
  n=0
  do while(associated(my_node))
    n=n+1

    if(good) then
      ! check #1
      my_diag => my_node%diags
      good = associated(my_diag)
      if(.not.good .and. perr_) then
        call iperr(myname_,'unassociated %diags, @count =',n)
        call iperr(myname_,'%(idv,iob,ich) =',(/my_node%idv,my_node%iob,1/))
      endif

      ! check #2
      if(good) then
        good = my_node%idv == my_diag%idv .and. &
               my_node%iob == my_diag%iob .and. &
                         1 == my_diag%ich
        if(.not.good .and. perr_) then
          call iperr(myname_,'mismatching keys, @count =',n)
          call iperr(myname_,'%(idv,iob,ich) ='      ,(/my_node%idv,my_node%iob,          1/))
          call iperr(myname_,'%diags%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
        endif
      endif
    endif

    if(.not.(good.or.present(count))) then
      call iperr(myname_,'test failed, @count =',n)
      call timer_fnl(ob_verify_name)
_EXIT_(myname_)
      return
    endif

    my_node => my_node%llpoint ! i.e. %next

  enddo

  ! check #3
  if(present(count)) then
    if(n/=count) then
      good = .false.
      if(perr_) call iperr(myname_,'mismatching count, (expected,actual) =',(/count,n/))
    endif
  endif
  call timer_fnl(ob_verify_name)
_EXIT_(myname_)
end function pm10_verify_


end module m_obdiag
