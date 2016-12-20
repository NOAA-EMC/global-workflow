 SUBROUTINE StartTimeGet(yy, mm, dd, hh, mns, sec, kfhour, n1, n2,    &
                         grib_inp, fhrot, cfile, cfile2, rc)

! This subroutine gets and calculates the start time from reading the
! sigma file information.

! !REVISION HISTORY:
!
!  March 2005      Weiyu Yang Initial code.
!
!USES:
!
 USE GFS_ErrMsgMod
 USE ESMF_Mod,     ONLY: ESMF_SUCCESS, ESMF_VM, ESMF_VMGetCurrent,    &
                         ESMF_VMBroadcast, ESMF_KIND_I4, ESMF_KIND_R8
 USE machine,      ONLY: kind_io4, kind_evod
 USE date_def,     ONLY: idate
 use layout1,      only: me
 use sigio_module
 use sigio_r_module
 use gfsio_module
 use mpi_def
 use gfsio_def

 IMPLICIT none

!
! ARGUMENTS:
!-----------

 INTEGER,                INTENT(IN)  :: grib_inp
 INTEGER,                INTENT(out) :: yy, mm, dd, hh, mns, sec
 INTEGER,                INTENT(out) :: n1, n2
 INTEGER,                INTENT(out) :: kfhour
!LOGICAL,                INTENT(in)  :: gfsio_in
 REAL(KIND = kind_evod), INTENT(in)  :: fhrot
 INTEGER,                INTENT(out) :: rc     ! return code

 INTEGER                             :: rc1 = ESMF_SUCCESS
 INTEGER(ESMF_KIND_I4), DIMENSION(1) :: read_sigio_flag = 0
 INTEGER(ESMF_KIND_I4), DIMENSION(1) :: read_gfsio_flag = 0
 REAL(KIND = kind_evod)              :: fhour
 REAL(KIND = kind_io4)               :: fhour4
 TYPE(ESMF_VM)                       :: vm_local
 INTEGER(ESMF_KIND_I4), DIMENSION(4) :: idate_e(4)
 REAL(ESMF_KIND_R8),    DIMENSION(1) :: fhour_e
 type(sigio_head) head
 character (len=*)                   :: cfile, cfile2
 integer iret, khour

 n1    = 11
 n2    = 12
 
 rc = ESMF_SUCCESS

 CALL ESMF_VMGetCurrent(vm_local, rc = rc1)
 CALL ERR_MSG3(rc1,'Get the current ESMF VM in StartTimeGet', rc)

 if (me == 0) then
   print *,' grib_inp=',grib_inp,' n1=',n1
   if (grib_inp .le. 0) then
     call sigio_rropen(n1,cfile,iret)
     call sigio_rrhead(n1,head,iret)

     IF(head%fhour /= fhrot) THEN
       call sigio_rropen(n2,cfile2,iret)
       call sigio_rrhead(n2,head,iret)
     END IF
 
     IF(iret /= 0) read_sigio_flag(1) = iret
     fhour_e(1) = head%fhour
     idate_e = head%idate
   else
     print *,' grib_inp=',grib_inp,' cfile=',cfile
     call gfsio_open(gfile_in,trim(cfile),'read',iret)
     call gfsio_getfilehead(gfile_in,iret=iret,idate=idate,fhour=fhour4)
     fhour_e(1) = fhour4
     idate_e = idate

     print *,' fhour4=',fhour4,' idate=',idate,' iret=',iret
     IF(iret /= 0) read_gfsio_flag(1) = iret
   endif
 endif
 CALL ESMF_VMBroadcast(vm_local, read_sigio_flag, 1, 0, rc = rc1)
 CALL ERR_MSG3(rc1,'ESMF VM broadcast the read_sigio_flag in StartTimeGet', rc)

 CALL ESMF_VMBroadcast(vm_local, read_gfsio_flag, 1, 0, rc = rc1)
 CALL ERR_MSG3(rc1,'ESMF VM broadcast the read_gfsio_flag in StartTimeGet', rc)

 IF(read_sigio_flag(1) /= 0) call mpi_quit(5554)
 IF(read_gfsio_flag(1) /= 0) call mpi_quit(5555)
 
 CALL ESMF_VMBroadcast(vm_local, idate_e, 4, 0, rc = rc1)
 CALL ERR_MSG3(rc1,'ESMF VM broadcast the idate in StartTimeGet', rc)

 CALL ESMF_VMBroadcast(vm_local, fhour_e, 1, 0, rc = rc1)
 CALL ERR_MSG3(rc1,'ESMF VM broadcast the fhour in StartTimeGet', rc)

 fhour = fhour_e(1)
 idate = idate_e
!
!print *,' me=',me,' idate=',idate,' fhour=',fhour
 yy     = idate(4)
 mm     = idate(2)
 dd     = idate(3)
 hh     = idate(1)
 mns    = 0
 sec    = 0
 kfhour = NINT(fhour)
 if(me == 0)print *,' idate=',idate,' fhour=',fhour,' kfhour=',kfhour,' me=',me

 rc = rc1

 END SUBROUTINE StartTimeGet
