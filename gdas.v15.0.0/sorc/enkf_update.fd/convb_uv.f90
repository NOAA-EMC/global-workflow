module convb_uv
!$$$   module documentation block
!                .      .    .                                       .
! module:    convb_uv
!   prgmmr: su          org: np2                date: 2014-03-28
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations non linear 
!             parameter b 
!
! program history log:
!   2007-03-15  su  - original code - move reading observation b table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have b information 
!
! Subroutines Included:
!   sub convb_uv_read      - allocate arrays for and read in conventional b table 
!   sub convb_uv_destroy   - destroy conventional b arrays
!
! Variable Definitions:
!   def btabl_uv             -  the array to hold the b table
!   def bptabl_uv             -  the array to have vertical pressure values
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
  public :: convb_uv_read
  public :: convb_uv_destroy
! set passed variables as public
  public :: btabl_uv,bptabl_uv,isuble_buv

  integer(i_kind),save:: ibtabl_uv,itypex,itypey,lcount,iflag,k,m,n
  real(r_single),save,allocatable,dimension(:,:,:) :: btabl_uv
  real(r_kind),save,allocatable,dimension(:)  :: bptabl_uv
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_buv

contains


  subroutine convb_uv_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_uv      read conventional b table 
!
!     prgmmr:    su    org: np2                date: 2007-03-15
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
     integer(i_kind),parameter     :: ld=300
     integer(i_kind),intent(in   ) :: mype
     integer(i_kind):: ier

     allocate(btabl_uv(ld,33,8),isuble_buv(ld,7))
        allocate(bptabl_uv(34))

     btabl_uv=1.e9_r_kind
      
     ibtabl_uv=19
     open(ibtabl_uv,file='btable_uv',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVB_UV:  ***WARNING*** obs b table ("btable") not available to 3dvar.'
        lcount=0
        bflag=.false.
        return
     endif

     rewind ibtabl_uv
     btabl_uv=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ibtabl_uv,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ibtabl_uv,105,IOSTAT=iflag,end=120) (isuble_buv(itypex,n),n=1,7)
105     format(8x,7i12)
        do k=1,33
           read(ibtabl_uv,110)(btabl_uv(itypex,k,m),m=1,8)
110        format(1x,8e12.5)
        end do
     end do   loopd
120  continue

! use the pressure value of the last obs. type, itypex
     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVB_UV:  ***WARNING*** obs b table not available to 3dvar.'
        bflag=.false.
     else
        if (itypex > 0 ) then
           bptabl_uv=zero
           bptabl_uv(1)=btabl_uv(itypex,1,1)
           do k=2,33
              bptabl_uv(k)=half*(btabl_uv(itypex,k-1,1)+btabl_uv(itypex,k,1))
           enddo
           bptabl_uv(34)=btabl_uv(itypex,33,1)
         else
            write(6,*)'ERROR IN CONVB_UV: NO OBSERVATION TYPE READ IN'
            return
         endif
        if (mype == 0) then
          write(6,*) 'CONVB_UV: nlqc b from user provided table' 
        endif
     endif
     close(ibtabl_uv)
     return
  end subroutine convb_uv_read


subroutine convb_uv_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_uv_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from convb_uv file
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

     deallocate(btabl_uv,bptabl_uv,isuble_buv)
     return
  end subroutine convb_uv_destroy

end module convb_uv
