module converr_pw
!$$$   module documentation block
!                .      .    .                                       .
! module:    converr_pw
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
!   sub converr_pw_read      - allocate arrays for and read in conventional error table 
!   sub converr_pw_destroy   - destroy conventional error arrays
!
! Variable Definitions:
!   def etabl_pw             -  the array to hold the error table
!   def ptabl_pw             -  the array to have vertical pressure values
!   def isuble_pw            -  the array to have subtype values 
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
  public :: converr_pw_read
  public :: converr_pw_destroy
! set passed variables as public
  public :: etabl_pw,ptabl_pw,isuble_pw,maxsub_pw

  integer(i_kind),save:: ietabl_pw,itypex,itypey,lcount,iflag,k,m,n,maxsub_pw
  real(r_single),save,allocatable,dimension(:,:,:) :: etabl_pw
  real(r_kind),save,allocatable,dimension(:)  :: ptabl_pw
  integer(i_kind),save,allocatable,dimension(:,:)  :: isuble_pw

contains


  subroutine converr_pw_read(mype)
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
     integer(i_kind),parameter :: ld=300  ! maximum number of conventionalintent(in   ) :: mype
     integer(i_kind),intent(in   ) :: mype

     integer(i_kind):: ier

     maxsub_pw=5
     allocate(etabl_pw(ld,33,6),isuble_pw(ld,5))

     etabl_pw=1.e9_r_kind
      
     ietabl_pw=19
     open(ietabl_pw,file='errtable_pw',form='formatted',status='old',iostat=ier)
     if(ier/=0) then
        write(6,*)'CONVERR_pw:  ***WARNING*** obs error table ("errtable") not available to 3dvar.'
        lcount=0
        oberrflg=.false.
        return
     endif

     rewind ietabl_pw
     etabl_pw=1.e9_r_kind
     lcount=0
     loopd : do 
        read(ietabl_pw,100,IOSTAT=iflag,end=120) itypey
        if( iflag /= 0 ) then
           write(6,*)'CONVERR_PW:  READING ERROR'
        exit loopd
        endif
100     format(1x,i3)
        lcount=lcount+1
        itypex=itypey
        read(ietabl_pw,105,IOSTAT=iflag,end=120) (isuble_pw(itypex,n),n=1,5)
105     format(8x,5i12)
        do k=1,33
           read(ietabl_pw,110)(etabl_pw(itypex,k,m),m=1,6)
110        format(1x,6e12.5)
        end do
     end do   loopd
120  continue

     if(lcount<=0 .and. mype==0) then
        write(6,*)'CONVERR_PW:  ***WARNING*** obs error table not available to 3dvar.'
        oberrflg=.false.
     else
        if(mype == 0)  then
           write(6,*)'CONVERR_PW:  using observation errors from user provided table'
           write(6,*)'CONVERR_PW:  end of reading etable_pw',itypex
           write(6,105) (isuble_pw(itypex,m),m=1,5)
           do k=1,33
              write(6,110) (etabl_pw(itypex,k,m),m=1,6)
           enddo
        endif
        allocate(ptabl_pw(34))
! use itypex to get pressure values.  itypex is the last valid observation type
        if (itypex > 0) then
           ptabl_pw=zero
           ptabl_pw(1)=etabl_pw(itypex,1,1)
           do k=2,33
              ptabl_pw(k)=half*(etabl_pw(itypex,k-1,1)+etabl_pw(itypex,k,1))
           enddo
        ptabl_pw(34)=etabl_pw(itypex,33,1)
        endif
     endif
     close(ietabl_pw)

     return
  end subroutine converr_pw_read


subroutine converr_pw_destroy
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    converr_pw_destroy      destroy conventional information file
!     prgmmr:    su    org: np2                date: 2007-03-15
!
! abstract:  This routine destroys arrays from converr_pw file
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

     deallocate(etabl_pw,ptabl_pw,isuble_pw)
     return
  end subroutine converr_pw_destroy

end module converr_pw
