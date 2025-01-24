   subroutine  getcount_bufr(inpfile,nmsg,nsub) 

!$$$  subprogram documentation block
!  ............................................................
!   subprogram:  getcount         
!   prgmmr:     Woollen, Jack, Su, Xiujuan 
!  abstract:   this subroutine is to read the bufr file to get information on the
!              counts of message and subset.

!  program history log
!  2015-03-27  Su    Modify original code from Jack Woollen

!  input argument list
!     inpfile       - input bufr file

!  output argument list:
!     nmsg         - messge count from input bufr file 
!     nsub         - subset count from input bufr file
!$$$

use kinds, only: i_kind
use file_utility, only : get_lun

   implicit none
!  Declare passed variables
   character(len=*)                      ,intent(in ) :: inpfile
   integer(i_kind)                       ,intent(out) :: nmsg,nsub 

!  Declare local parameters

   character(len=8)  :: subset
   integer(i_kind)   lunit,ireadmg,nmsub,idate

   lunit=get_lun()
   nsub=0;nmsg=0
   open(lunit,file=trim(inpfile),form='unformatted')
   call openbf(lunit,'IN',lunit)
   do while(ireadmg(lunit,subset,idate) >=0)
      nmsg = nmsg+1; nsub = nsub+nmsub(lunit)
   enddo
   call closbf(lunit)

   return
   end 
