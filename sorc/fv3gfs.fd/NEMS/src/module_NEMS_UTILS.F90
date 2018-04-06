#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_NEMS_UTILS

      USE ESMF

      implicit none

      private
      public :: check_esmf_pet, err_msg, message_check

      logical, parameter :: iprint = .false.
      character(esmf_maxstr) :: message_check

      contains

!-----------------------------------------------------------------------
!
      subroutine err_msg_int(rc1,msg,val,rcfinal)
!
      integer, intent(inout)        :: rc1
      integer, intent(out)          :: rcfinal
      character (len=*), intent(in) :: msg
      integer,           intent(in) :: val
      if(esmf_logfounderror(rcToCheck=rc1, msg=msg)) then
          rcfinal = esmf_failure
          print*, 'error happened for ',msg,' ',val,' rc = ', rc1
          write(0,*)' ERROR: ',msg,' ',val,' rc = ', rc1
          rc1     = esmf_success
      else
          if(iprint) print*, 'pass ',msg,' ',val
      end if
      return
      end subroutine err_msg_int

      subroutine err_msg_val(rc1,msg,val,rcfinal)
!
      integer, intent(inout)        :: rc1
      integer, intent(out)          :: rcfinal
      character (len=*), intent(in) :: msg
      real,              intent(in) :: val
      if(esmf_logfounderror(rc1, msg=msg)) then
          rcfinal = esmf_failure
          print*, 'error happened for ',msg,' ',val,' rc = ', rc1
          write(0,*)' ERROR: ',msg,' ',val,' rc = ', rc1
          rc1     = esmf_success
      else
          if(iprint) print*, 'pass ',msg,' ',val
      end if
      return
      end subroutine err_msg_val

      subroutine err_msg_var(rc1,msg,chr,rcfinal)
!
      integer, intent(inout)        :: rc1
      integer, intent(out)          :: rcfinal
      character (len=*), intent(in) :: msg
      character (len=*), intent(in) :: chr
      if(esmf_logfounderror(rc1, msg=msg)) then
          rcfinal = esmf_failure
          print*, 'error happened for ',msg,' ',chr,' rc = ', rc1
          write(0,*)' ERROR: ',msg,' ',chr,' rc = ', rc1
          rc1     = esmf_success
      else
          if(iprint) print*, 'pass ',msg,' ',chr
      end if
      return
      end subroutine err_msg_var

      subroutine err_msg(rc1,msg,rc)
      integer, intent(inout)        :: rc1
      integer, intent(out)          :: rc
      character (len=*), intent(in) :: msg
      if(esmf_logfounderror(rc1, msg=msg)) then
          rc  = esmf_failure
          print*, 'error happened for ',msg, ' rc = ', rc1
          write(0,*)' ERROR: ',trim(msg),' rc = ', rc1
          rc1 = esmf_success
      else
          rc  = esmf_success
          if(iprint) print*, 'pass ',msg
      end if
      return
      end subroutine err_msg

      subroutine err_msg_final(rcfinal,msg,rc)
      integer, intent(inout)        :: rcfinal
      integer, intent(inout)        :: rc
      character (len=*), intent(in) :: msg
      if(rcfinal == esmf_success) then
          if(iprint) print*, "final pass: ",msg
      else
          print*, "final fail: ",msg
          write(0,*)' FINAL ERROR: ',msg
      end if
!     if(present(rc)) then
          rc = rcfinal
!     end if
      return
      end subroutine err_msg_final

!-----------------------------------------------------------------------
!
      subroutine check_esmf_pet(print_esmf)
!
!-----------------------------------------------------------------------
!
  implicit none
  integer :: i,n
  character *256 :: c1,c2
  logical :: opened,print_esmf
!
  do n=101,201
    inquire(n,opened=opened)
    if(.not.opened)then
      open(n,file='model_configure',status='old')  !<-- Open configure file
      exit
    endif
  enddo
!
  print_esmf=.false.
!
  do i=1,10000
    read(n,*,end=22)c1,c2
    if(c1(1:10) == 'print_esmf') then              !<-- Search for print_esmf flag
      if( c2 == 'true'   .or.          &           !<-- Check if print_esmf is true or false
          c2 == '.true.' .or.          &
          c2 == 'TRUE'   .or.          &
          c2 == '.TRUE.' ) print_esmf=.true.
      exit
    endif
  enddo
22  close(n)
    return
!
!-----------------------------------------------------------------------
!
      end subroutine check_esmf_pet
!
!-----------------------------------------------------------------------


      end module module_NEMS_UTILS
