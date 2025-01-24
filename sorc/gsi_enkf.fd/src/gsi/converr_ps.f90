module converr_ps
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_ps
!   prgmmr: su          org: np2                date: 2007-03-15
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations error for surface
!            pressure
!
! program history log:
!   2007-03-15  su  - original code - move reading observation error table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have the new error information 
!
! Subroutines Included:
!   sub converr_ps_read      - allocate arrays for and read in conventional error table 
!   sub converr_ps_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_ps             -  the array to hold the error table
!   def ptabl_ps             -  the array to have vertical pressure values
!   def isuble_ps            -  the array to have subtype  
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only:r_kind,i_kind,r_single
use constants, only: zero
use obsmod, only : oberrflg 
implicit none

! set default as private
  private
! set subroutines as public
  public :: converr_ps_read
  public :: converr_ps_destroy
! set passed variables as public
  public :: etabl_ps,ptabl_ps,isuble_ps,maxsub_ps

  integer(i_kind),save:: ietabl,itypex,itypey,lcount,iflag,k,m,n,maxsub_ps
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_ps
  real(r_kind),save,allocatable,dimension(:)  :: ptabl_ps
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_ps

contains


  subroutine converr_ps_read(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_err      read conventional information file
!
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine reads the conventional error table file
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!   2013-05-14  guo     -- add status and iostat in open, to correctly
!                          handle the error case of "obs error table not
!                          available to 3dvar".
!   2015-03-06  yang    -- add ld=300, the size of the error table.
!                          Remove the original calculation to get error table
!                          array
!                          index. ld=300 is sufficient for current conventional
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
     integer(i_kind),    parameter :: ld=300
     integer(i_kind),intent(in   ) :: mype
     integer(i_kind):: ier

     maxsub_ps=5
     allocate(etabl_ps(ld,33,6))
     allocate(isuble_ps(ld,5))

     etabl_ps=1.e9_r_kind
      
     ietabl=19
     open(ietabl,file='errtable_ps',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR_ps:  ***WARNING*** obs error table ("errtable") not available to 3dvar.'
        lcount=0
        oberrflg=.false.
        return
     endif

     rewind ietabl
     etabl_ps=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ietabl,105,IOSTAT=iflag,end=120) (isuble_ps(itypex,n),n=1,5)  
        if(mype==0) write(6,'(a16,6I9)') 'READIN ERROR _PS', itypex, (isuble_ps(itypex,n),n=1,5)
105     format(8x,5i12)
        do k=1,33
           read(ietabl,110)(etabl_ps(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR_PS:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     else
        if(mype == 0)  then
           write(6,*)'CONVERR_PS:  using observation errors from user provided table'
        endif
        allocate(ptabl_ps(34))

!use the pressure values of itypex, which is the last observation type. 
        if (itypex > 0 ) then
           ptabl_ps=zero
           ptabl_ps(1)=etabl_ps(itypex,1,1)
           do k=2,33
              ptabl_ps(k)=half*(etabl_ps(itypex,k-1,1)+etabl_ps(itypex,k,1))
           enddo
           ptabl_ps(34)=etabl_ps(itypex,33,1)
        else
            write(6,*)'ERROR IN CONVERR_PS: NO OBSERVATION READ IN itypex', itypex
            return
        endif
     endif

     close(ietabl)
     return
  end subroutine converr_ps_read


subroutine converr_ps_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_ps_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from converr file
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

     deallocate(etabl_ps,ptabl_ps,isuble_ps)
     return
  end subroutine converr_ps_destroy

end module converr_ps

