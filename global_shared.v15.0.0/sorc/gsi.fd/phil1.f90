!                                  R. J. Purser NOAA/NCEP/EMC 2006
!=============================================================================
module phil1
!=============================================================================
!$$$ module documentation block
!           .      .    .                                       .
! module:   phil1
!   prgmmr: purser
!
! abstract:
!
! program history list:
!   2009-09-22  lueken - added module doc block
!
! subroutines included:
!   sub getvalsets_s
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind

implicit none

private
public getvalsets
interface getvalsets; module procedure getvalsets_s; end interface

contains

!=============================================================================
subroutine getvalsets_s(nob, mskip,xhskip,xh,next, firsta,firstb)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    getvalsets_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    nob,mskip
!    xhskip
!    xh
!    next
!    firsta
!
!   output argument list:
!    next
!    firsta
!    firstb
!
! note:
! Given a linked list, A={firsta,next}, of hilbert-parameter location data, xh,
! this routine partitions it into MSKIP disjoint linked lists,
! B(j)={firstb(j),next}, with j=1,..MSKIP, and the modified list A of the
! residual data that are not in any of the B(j). The separation between
! consecutive location parameters, xh, in each B-list must be less than a
! predetermined margin, xhskip, which tends to suppress (but does not
! absolutely eliminate) the accidental occurrences of geographically close
! pairs of validation data in each B-subset. This serves to minimize
! redundancy in each validation subset. In data dense regions it is therefore
! not possible to put all the data into only MSKIP validation subsets; the
! data passed over are gathered into the re-constituted set A of residual
! (non-validation) data.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                  intent(IN   ) :: nob,mskip
real(r_kind),                     intent(IN   ) :: xhskip
real(r_kind),dimension(nob),      intent(IN   ) :: xh
integer(i_kind), dimension(nob),  intent(INOUT) :: next
integer(i_kind),                  intent(INOUT) :: firsta
integer(i_kind), dimension(mskip),intent(  OUT) :: firstb
!-----------------------------------------------------------------------------
integer(i_kind)                                :: iskip,jskip,this_old_a,this_new_a &
                                         ,icycle,itrial
integer(i_kind), dimension(mskip)              :: this_b  
real(r_kind)                                   :: xhwait,xha
real(r_kind),dimension(mskip)                  :: xhb
!=============================================================================
firstb=0      ! <- initialize B lists to empty sets
xhb=-xhskip
this_old_a=firsta ! <- initialize present item in original A-list to firsta
this_new_a=0  ! <- initialize present item in new A-list to null item
do icycle=1,2+nob/mskip
   do iskip=1,mskip ! <- Loop through different validation subsets (B-lists)
      xhwait=xhb(iskip)+xhskip ! <- Qualifying new B-list xh must >= xhwait.
      trial: do itrial=1,nob ! <- Keep trying to find a qualifying item from A.
         if(this_old_a==0)then
! Original A-list comes to an end, so it is appropriate to terminate....:
            do jskip=1,mskip
               if(firstb(jskip)/= 0)next(this_b(jskip))=0 ! <-  ..the B-lists..
            enddo
            if(this_new_a/=0)next(this_new_a)=0 ! <- ..and the new A-list.
            return ! <- Proper subr. completion is ONLY via this return.
         endif

! Original A-list still not completely processed; seek to extend this B-list,
! but while xh-increment remains too small, keep putting original A-list
! items on the end of the new A-list.
         xha=xh(this_old_a)

! New item always qualifies if B-list is still empty:
         if(firstb(iskip)==0)then ! B-list still empty:
            firstb(iskip)=this_old_a
            exit trial ! B-list now non-empty; exit trial-loop
         endif

! But if B-list is already non-empty, the xh-increment must be at least
! as big as xhskip to discourage accidental close-pairs:
         if(xha >= xhwait)then
            next(this_b(iskip))=this_old_a
            exit trial ! B-list successfully extended; exit trial-loop
         endif

! Since the conditions for extending this B-list are not met by this_new_A,
! stick this item on the end of the residual  "new" A-list instead:
         if(this_new_a==0)then ! New A-list still empty.
            firsta=this_old_a  ! <- New A-list is now non-empty.
         else
            next(this_new_a)=this_old_a ! <- New A-list already non-empty.
         endif
         this_new_a=this_old_a
         this_old_a=next(this_old_a) ! <- Move along the old linked A-list
!         write(13,'(4i5)') icycle,iskip,itrial,this_new_a
      enddo trial ! <- repeat trial; B-list still awaits qualifying A-item

! Trial was successful; add to this B-list and move on to next old A-list item
! and the next index, iskip, in the cycle of mskip validation subsets:
      xhb(iskip)=xha
      this_b(iskip)=this_old_a
      this_old_a=next(this_old_a)
   enddo
enddo
stop ' In getvalsets; improper completion. Check original list A and data xh'
end subroutine getvalsets_s
!=============================================================================

end module phil1

