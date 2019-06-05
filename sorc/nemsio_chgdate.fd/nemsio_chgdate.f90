program nemsio_chgdate
!$$$  main program documentation block
!
! program:  nemsio_chgdate
!
! prgmmr: mahajan          org: ncep/emc               date: 2018-02-14
!
! abstract:  Replace the date and forecast hour from NCEP/EMC nemsio file
!            with a date and hour provided from input
!
! program history log:
!   2018-02-14  Initial version.
!
! usage:
!   nemsio_chgdate.x filename.nemsio new_idate_YYYYMMDDHH new_nfhour_HH
!
! attributes:
!   language: f95
!
!$$$

use nemsio_module, only:  nemsio_init, nemsio_open, nemsio_close
use nemsio_module, only:  nemsio_intkind
use nemsio_module, only:  nemsio_gfile, nemsio_getfilehead, &
                          nemsio_setheadvar

implicit none

character(len=50) :: filename
character(len=10) :: idatestr, nfhourstr
integer(nemsio_intkind) :: iret
integer(nemsio_intkind) :: idate(7), nfhour

type(nemsio_gfile) :: gfile

! replace idate and nfhour in this file
call getarg(1, filename)

! read idate to replace
call getarg(2, idatestr)

! read nfhour to replace
call getarg(3, nfhourstr)

write(6,'(A)')'NEMSIO_CHGDATE:'
write(6,'(2(A))')'   filename = ',trim(filename)
write(6,'(2(A))')'  new idate = ',trim(idatestr)
write(6,'(2(A))')' new nfhour = ',trim(nfhourstr)

call nemsio_init(iret=iret)
call nemsio_error(iret, 'open to initialize nemsio')

call nemsio_open(gfile, trim(filename), 'RDWR', iret=iret)
call nemsio_error(iret, 'open to READ and WRITE' // trim(filename))

call nemsio_getfilehead(gfile, idate=idate, nfhour=nfhour, iret=iret)
call nemsio_error(iret, 'getfilehead (idate) from ' // trim(filename))

write(6,'(A,7(1X,I6))') 'OLD idate  = ', idate
write(6,'(A,I4)') 'OLD nfhour = ', nfhour

! Replace old date with new dates
read(idatestr(1:4), '(I)') idate(1)
read(idatestr(5:6), '(I)') idate(2)
read(idatestr(7:8), '(I)') idate(3)
read(idatestr(9:10), '(I)')idate(4)
read(nfhourstr, '(I)') nfhour

write(6,'(A,7(1X,I6))') 'NEW idate  = ', idate
write(6,'(A,I4)') 'NEW nfhour = ', nfhour

call nemsio_setheadvar(gfile, 'idate', idate, iret=iret)
call nemsio_error(iret, 'setfilehead (idate) in ' // trim(filename))
call nemsio_setheadvar(gfile, 'nfhour', nfhour, iret=iret)
call nemsio_error(iret, 'setfilehead (nfhour) in ' // trim(filename))

call nemsio_close(gfile, iret=iret)
call nemsio_error(iret, 'close file ' // trim(filename))

stop

contains

subroutine nemsio_error(iret, msg)

implicit none

integer(nemsio_intkind), intent(in) :: iret
character(len=*), intent(in) :: msg

character(len=500) :: msgout

if ( iret /= 0 ) then

    msgout = '***ERROR*** Unable to ' // trim(msg) // ' ABORT!'
    write(6,*) trim(msgout)
    stop 99

endif

end subroutine nemsio_error

END program nemsio_chgdate
