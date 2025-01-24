!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  blacklist --- Setup blacklist information
!
! !INTERFACE:
!

module blacklist 

! !USES:

  use kinds, only: i_kind
  implicit none
!
! !DESCRIPTION:  This module contains variables and routines related
!                 to the assimilation of conventional observations 
!
! !REVISION HISTORY:
!
!   2006-12-01  todling  - original code - blacklist merge (based on convinfo)
!
! Subroutines Included:
!   sub blacklist_read    - allocate arrays for and read in blacklist file
!   sub blacklist_destroy - destroy blacklist arrays
!
! Variable Definitions:
!   def iblktbl        - 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
! !AUTHOR:
!   todling           org: gsfc                date: 2006-12-01
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: blacklist_read
  public :: blacklist_destroy
! set passed variables to public
  public :: ibcnt,blkkx,blkstns

  integer(i_kind) ibcnt,ilcnt
  character(len=8),allocatable,dimension(:) :: blkstns
  integer(i_kind), allocatable,dimension(:) :: blkkx
  logical, save :: blacklist_initialized=.false.

contains

!-------------------------------------------------------------------------
! NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: blacklist_read - read in blacklist file and store info in mem
!
! !INTERFACE:
!
  subroutine blacklist_read(obstype)

! !USES:

    implicit none

! !DESCRIPTION:  This routine reads the conventional information file
!
! !REVISION HISTORY:
!   2006-02-08  todling - brought sienkiewicz's code into this routine
!   2007-02-15  todling - arbitrary lengh for obstype
!   2009-12-03  todling - check to see if initialized already
!
! !INPUT PARAMETERS:

    character(len=*),  intent(in   ) :: obstype ! observation type

! !OUTPUT PARAMETERS:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
! !AUTHOR:
!     prgmmr:    todling    org: gsfc                date: 2006-12-01
!EOP
!-------------------------------------------------------------------------
    

    character(len=1)cflg
    character(len=7) iotype
    character(len=120) crecord
    integer(i_kind) i
    integer(i_kind) iblktbl,ier,ios,ikx
    character(len=8) stnid
    logical eof

    if (blacklist_initialized) return
    iblktbl = 19
    open(iblktbl,file='blacklist',form='formatted')
    rewind iblktbl
    ibcnt = 0
    ilcnt = 0
    eof = .false.
    do while (.not. eof)     ! get number of applicable entries in table
       read(iblktbl,fmt='(a1,a7)',iostat=ios)cflg,iotype
       if (ios /= 0) then
          eof = .true.
          cycle
       endif
       ilcnt = ilcnt + 1
       if (cflg == '!') cycle
       if (iotype/=obstype)cycle
       ibcnt = ibcnt + 1
    end do
    if (ibcnt > 0) then     ! allocate and read in table
        allocate(blkstns(ibcnt),blkkx(ibcnt),stat=ier)
        if (ier /=0) then
           write(6,*)'***READ_PREPBUFR ERROR*** Error allocating ',&
                'memory; blacklist will not be applied'
           ibcnt = 0
        else
           blacklist_initialized=.true.
           rewind iblktbl
           ibcnt = 0
           do i = 1, ilcnt
              read(iblktbl,fmt='(a1,a7,1x,a120)',iostat=ios) &
                   cflg,iotype,crecord
              if (ios /=0) exit
              if (cflg == '!') cycle
              if (iotype == obstype) then
                 read(crecord,'(i3,1x,a8)') ikx,stnid
                 ibcnt = ibcnt + 1
                 blkstns(ibcnt) = stnid
                 blkkx(ibcnt) = ikx
              endif
           enddo
        endif
    endif
    close(iblktbl)
    
    return
  end subroutine blacklist_read

!-------------------------------------------------------------------------
! NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: blacklist_destroy - release memory of arrays in blacklist
!
! !INTERFACE:

  subroutine blacklist_destroy
!
! !DESCRIPTION:  This routine destroys arrays from convinfo file
!
! !INPUT PARAMETERS:
!
! !OUTPUT PARAMETERS:
!
! !REVISION HISTORY:
!
!   2006-12-01  todling - initial code 
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
! !AUTHOR:
!     prgmmr:    todling    org: np2                date: 2006-12-01
!EOP
!-------------------------------------------------------------------------
    implicit none
    integer(i_kind) ier

    deallocate(blkstns,blkkx,stat=ier)

    return
  end subroutine blacklist_destroy
  
end module blacklist
