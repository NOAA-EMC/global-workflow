module converr_uv
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_uv
!   prgmmr: su          org: np2                date: 2007-03-15
! abstract:  This module contains variables and routines related
!            to the assimilation of conventional observations error
!
! program history log:
!   2007-03-15  su  - original code - move reading observation error table 
!                                     from read_prepbufr to here so all the 
!                                     processor can have the new error information 
!
! Subroutines Included:
!   sub converr_uv_read      - allocate arrays for and read in conventional error table 
!   sub converr_uv_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_uv             -  the array to hold the error table
!   def ptabl_uv             -  the array to have vertical pressure values
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
  public :: converr_uv_read
  public :: converr_uv_destroy
! set passed variables as public
  public :: etabl_uv,ptabl_uv,isuble_uv,maxsub_uv

  integer(i_kind),save:: ietabl_uv,itypex,itypey,lcount,iflag,k,m,n,maxsub_uv
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_uv
  real(r_kind),save,allocatable,dimension(:)  :: ptabl_uv
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_uv

contains


  subroutine converr_uv_read(mype)
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
!                          Remove the original calculation to get error table array 
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
     integer(i_kind),parameter    :: ld=300
     integer(i_kind),intent(in   ) :: mype
     integer(i_kind):: ier

     allocate(etabl_uv(ld,33,8),isuble_uv(ld,7))

     etabl_uv=1.e9_r_kind
     maxsub_uv=7
      
     ietabl_uv=19
     open(ietabl_uv,file='errtable_uv',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR_uv:  ***WARNING*** obs error table ("errtable") not available to 3dvar.'
        lcount=0
        oberrflg=.false.
        return
     endif

     rewind ietabl_uv
     etabl_uv=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl_uv,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) exit loopd
!        if (mype == 0) write(6,*)'CONVERR_UV:itypey=',itypey
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ietabl_uv,105,IOSTAT=iflag,end=120) (isuble_uv(itypex,n),n=1,7)
!        if (mype == 0) write(6,*)'CONVERR_UV:itypex,itypex=',itypex,itypex
105     format(8x,7i12)
        do k=1,33
           read(ietabl_uv,110)(etabl_uv(itypex,k,m),m=1,8)
110        format(1x,8e12.5)
        end do
     end do   loopd
120  continue
     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR_UV:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     else
        if(mype == 0) then
           write(6,*)'CONVERR_UV:  using observation errors from user provided table'
        endif
        allocate(ptabl_uv(34))

! use the pressure values of itypex, which is the last valid observation type.
        if (itypex > 0 ) then
           ptabl_uv=zero
           ptabl_uv(1)=etabl_uv(itypex,1,1)
           do k=2,33
              ptabl_uv(k)=half*(etabl_uv(itypex,k-1,1)+etabl_uv(itypex,k,1))
           enddo
           ptabl_uv(34)=etabl_uv(itypex,33,1)
         else
            write(6,*)'ERROR IN CONVERR_UV: NO OBSERVATION TYPE READ IN'
            return
         endif
     endif

     close(ietabl_uv)

     return
  end subroutine converr_uv_read


subroutine converr_uv_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_uv_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from converr_uv file
!
! program history log:
!   2007-03-15  su 
!
!   input argument list:
!
!   output argument list:
!
! atte#ibutes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
     implicit none

     deallocate(etabl_uv,ptabl_uv,isuble_uv)
     return
  end subroutine converr_uv_destroy

end module converr_uv
