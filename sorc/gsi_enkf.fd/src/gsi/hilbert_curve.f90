!=============================================================================
subroutine hilbert(mskip,nob,xob,yob,test_set)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    hilbert
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    mskip
!    nob
!    xob,yob
!
!   output argument list:
!    test_set
!
! attributes:
!   language: f90
!   machine:
!
! notes:
! Map these data to a Hilbert curve using xy_to_hil, convert the base-4
! representation of the hilbert parameter to an ordinary real, using
! hil_to_r. Then order the obs along this space-filling curve by invoking
! the efficient sorting procedure, bsort. The resulting linked list of obs
! is accessed by "firsta" and terminated when "next(iob)" ==0.
!
! From this linked list, "A", construct linked subsets, "B", that can be used
! us validation subsets. Members of old A not able to be put into any of the
! mskip B subset are gathered into a reconstritued lined list, new "A". All
! this is done in subroutine getvalsets.
!
!$$$ end documentation block

use phil, only: xy_to_hil,hil_to_r,bsort
use phil1, only: getvalsets
use kinds, only: r_kind,i_kind

implicit none

integer(i_kind),parameter        :: ngen=15
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00035_r_kind
real(r_kind),parameter           :: xhskip=.00030_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00025_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.00001_r_kind
!real(r_kind),parameter          :: delta=.001_r_kind,xhskip=.005_r_kind

integer(i_kind)                ,intent(IN   ) :: mskip,nob
real(r_kind), dimension(nob)   ,intent(IN   ) :: xob,yob

integer(i_kind), dimension(nob),intent(  OUT) :: test_set

real(r_kind),dimension(nob)     :: xh
integer(i_kind),dimension(nob)  :: next
integer(i_kind),dimension(ngen) :: hil4
integer(i_kind)                 :: iob,iskip,isamp         &
                          ,firsta,count
integer(i_kind),dimension(mskip):: firstb

do iob=1,nob
   call xy_to_hil(ngen,xob(iob),yob(iob),hil4)
   call hil_to_r(1,ngen,hil4,xh(iob))
enddo
call bsort(1,nob,xh,next,firsta)

count=0
iob=firsta
do isamp=1,nob
   if(iob==0)exit
   count=count+1
   iob=next(iob)
enddo

call getvalsets(nob, mskip,xhskip,xh,next, firsta,firstb)

test_set=0

do iskip=1,mskip
   count=0
   iob=firstb(iskip)
   do isamp=1,nob
      if(iob==0)exit
      count=count+1
      test_set(iob)=iskip
      iob=next(iob)
   enddo
enddo

count=0
iob=firsta
do isamp=1,nob
   if(iob==0)exit
   count=count+1
   iob=next(iob)
enddo

end subroutine hilbert
