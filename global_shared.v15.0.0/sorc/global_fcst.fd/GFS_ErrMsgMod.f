! !module: gfs_errmsgmod --- initialize module of the esmf
!                                     gridded component of the gfs system.
!
! !description: gfs gridded component error messages
!
! !revision history:
!
!  march 2006 s. moorthi
!
!
! !interface:
!
 module gfs_errmsgmod

!
!!uses:
!
 use esmf_mod                 ! the esmf library.

 implicit none
 contains
 subroutine err_msg(rc1,msg,var,rcfinal)
!
 integer, intent(inout)        :: rc1
 integer, intent(out)          :: rcfinal
 character (len=*), intent(in) :: msg
 character (len=*), intent(in) :: var
 if(esmf_logmsgfounderror(rc1, msg)) then
     rcfinal = esmf_failure
     print*, 'error happened when running the esmf_configgetattribute-', &
     var,' rc = ', rc1
     rc1     = esmf_success
 end if
 end subroutine err_msg
 subroutine err_msg1(rc1,msg,rcfinal)
!
 integer, intent(inout)          :: rc1
 integer, intent(out)            :: rcfinal
 character (len=*), intent(in)   :: msg
 if(esmf_logmsgfounderror(rc1, msg)) then
     rcfinal = esmf_failure
     print*, 'error happened when running the esmf_configgetattribute-', &
     'rc = ', rc1,' msg=',msg
     rc1     = esmf_success
 end if
 end subroutine err_msg1
 subroutine err_msg3(rc1,msg,rc)
 integer, intent(inout)        :: rc1
 integer, intent(out)          :: rc
 character (len=*), intent(in) :: msg
 if(esmf_logmsgfounderror(rc1, msg)) then
     rc  = esmf_failure
     print*, 'error happened for ',msg, ' rc = ', rc1
     rc1 = esmf_success
 end if
 end subroutine err_msg3
 subroutine err_msg4(rc1,msg,rc)
 integer, intent(inout)        :: rc1
 integer, intent(out)          :: rc
 character (len=*), intent(in) :: msg
 if(esmf_logmsgfoundallocerror(rc1, msg)) then
     rc  = esmf_failure
     print*, 'error happened for ',msg, ' rc = ', rc1
     rc1 = esmf_success
 end if
 end subroutine err_msg4
 end module gfs_errmsgmod
