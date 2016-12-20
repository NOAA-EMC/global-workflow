module convb_ps
!$$$   module documentation block
!                .      .    .                                       .
! module:    convb_ps
!   prgmmr: su          org: np2                date: 2014-03-28
! abstract:  This module contains variables and routines related
!            to the assimilation of non linear qc b parameter for surface
!            pressure
!
! program history log:
!
! Subroutines Included:
!   sub convb_ps_read      - allocate arrays for and read in conventional b table 
!   sub convb_ps_destroy   - destroy conventional b arrays
!
! Variable Definitions:
!   def btabl_ps             -  the array to hold the b table
!   def bptabl_ps             -  the array to have vertical pressure values
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
  public :: convb_ps_read
  public :: convb_ps_destroy
! set passed variables as public
  public :: btabl_ps,bptabl_ps,isuble_bps

  integer(i_kind),save:: ibtabl,itypex,itypey,lcount,iflag,k,m,n
  real(r_single),save,allocatable,dimension(:,:,:) :: btabl_ps
  real(r_kind),save,allocatable,dimension(:)  :: bptabl_ps
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_bps

contains


  subroutine convb_ps_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_ps      read conventional information file
!
!     prgmmr:    su    org: np2                date: 2014-03-28
!
! abstract:  This routine reads the conventional b table file

!   2015-03-06  yang    -- add ld = 3000 for the size of nlqc_b table. Remove
!                          the hardwired value in the calculation of table array index.
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

     allocate(btabl_ps(ld,33,6))
     allocate(isuble_bps(ld,5))
     allocate(bptabl_ps(34))

     btabl_ps=1.e9_r_kind
      
     ibtabl=11
     open(ibtabl,file='btable_ps',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVB_PS:  ***WARNING*** obs b table ("btable") not available to 3dvar.'
        lcount=0
        bflag=.false.
        return
     endif

     rewind ibtabl
     btabl_ps=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ibtabl,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ibtabl,105,IOSTAT=iflag,end=120) (isuble_bps(itypex,n),n=1,5)
105     format(8x,5i12)
        do k=1,33
           read(ibtabl,110)(btabl_ps(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVB_PS:  ***WARNING*** obs b table not available to 3dvar.'
        bflag=.false.
     else
        if(mype == 0) then
           write(6,*)'CONVB_PS:  using nlqc b from user provided table'
        endif
! use the pressure of last obs. type, itypex
        if (itypex > 0 ) then
           bptabl_ps=zero
           bptabl_ps(1)=btabl_ps(itypex,1,1)
           do k=2,33
              bptabl_ps(k)=half*(btabl_ps(itypex,k-1,1)+btabl_ps(itypex,k,1))
           enddo
           bptabl_ps(34)=btabl_ps(itypex,33,1)
        else
            write(6,*)'ERROR IN CONVB_PS: NO OBSERVATION TYPE READ IN'
            return
        endif
     endif

     close(ibtabl)

     return
  end subroutine convb_ps_read


subroutine convb_ps_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convb_ps_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from convb file
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

     deallocate(btabl_ps,bptabl_ps,isuble_bps)
     return
  end subroutine convb_ps_destroy

end module convb_ps
