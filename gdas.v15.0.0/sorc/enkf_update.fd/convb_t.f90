module convb_t
!$$$   module documentation block
!                .      .    .                                       .
! module:    convb_t
!   prgmmr: su          org: np2                date: 2014-03-28
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations b paramter to read
!            temperature b table, the structure is different from current
!            one, the first 
!
! program history log:
!   2014-03-28  su  - original code - move reading observation b table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have the b information 
!
! Subroutines Included:
!   sub convb_t_read      - allocate arrays for and read in conventional b table 
!   sub convb_t_destroy   - destroy conventional b arrays
!
! Variable Definitions:
!   def btabl_t             -  the array to hold the b table
!   def bptabl_t             -  the array to have vertical pressure values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only:r_kind,i_kind,r_single
use constants, only: zero
use obsmod, only : bflag 
implicit none

! set default as private
  private
! set subroutines as public
  public :: convb_t_read
  public :: convb_t_destroy
! set passed variables as public
  public :: btabl_t,bptabl_t,isuble_bt

  integer(i_kind),save:: ibtabl_t,itypex,itypey,lcount,iflag,k,m,n
  real(r_single),save,allocatable,dimension(:,:,:) :: btabl_t
  real(r_kind),save,allocatable,dimension(:)  :: bptabl_t
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_bt

contains


  subroutine convb_t_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_t_read      read b table 
!
!     prgmmr:    su    org: np2                date: 2014-03-28
!
! abstract:  This routine reads the conventional b table file
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2013-05-14  guo     -- add status and iostat in open, to correctly
!                          handle the b case of "obs b table not
!                          available to 3dvar".
!   2015-03-06  yang    -- add ld = 3000 for the size of nlqc_b table. Remove
!                          the hardwired value in the calculation of table array
!                          index.
!                          ld=300 is sufficient for current conventional
!                          observing systems.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
     use constants, only: half
     implicit none

     integer(i_kind),parameter :: ld=300
     integer(i_kind),intent(in   ) :: mype
     integer(i_kind):: ier

     allocate(btabl_t(ld,33,6),isuble_bt(ld,5))
        allocate(bptabl_t(34))

     btabl_t=1.e9_r_kind
      
     ibtabl_t=19
     open(ibtabl_t,file='btable_t',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVB_T:  ***WARNING*** obs b table ("btabl") not available to 3dvar.'
        lcount=0
        bflag=.false.
        return
     endif

     rewind ibtabl_t
     btabl_t=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ibtabl_t,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ibtabl_t,105,IOSTAT=iflag,end=120) (isuble_bt(itypex,n),n=1,5)
105     format(8x,5i12)
        do k=1,33
           read(ibtabl_t,110)(btabl_t(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVB_T:  ***WARNING*** obs b table not available to 3dvar.'
        bflag=.false.
     else
! use the pressure values of last obs. type, itypex
        if (itypex > 0 ) then
           bptabl_t=zero
           bptabl_t(1)=btabl_t(itypex,1,1)
           do k=2,33
              bptabl_t(k)=half*(btabl_t(itypex,k-1,1)+btabl_t(itypex,k,1))
           enddo
           bptabl_t(34)=btabl_t(itypex,33,1)
        else
            write(6,*)'ERROR IN CONVB_T: NO OBSERVATION TYPE READ IN'
            return
        endif
     endif

     close(ibtabl_t)

     return
  end subroutine convb_t_read


subroutine convb_t_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_t_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from convb_t file
!
! program history log:
!   2007-03-15  su 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
     implicit none

     deallocate(btabl_t,bptabl_t,isuble_bt)
     return
  end subroutine convb_t_destroy

end module convb_t
