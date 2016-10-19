!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************
module block_control_mod
#include <fms_platform.h>

use mpp_mod,         only: mpp_error, NOTE, FATAL
use mpp_domains_mod, only: mpp_compute_extent

 public block_control_type

 type ix_type
   integer, dimension(:,:), _ALLOCATABLE :: ix _NULL
 end type ix_type

 type block_control_type
   integer :: nx_block, ny_block
   integer :: nblks
   integer :: isc, iec, jsc, jec
   integer :: npz
   integer, dimension(:), _ALLOCATABLE :: ibs _NULL, &
                                          ibe _NULL, &
                                          jbs _NULL, &
                                          jbe _NULL
   type(ix_type), dimension(:), _ALLOCATABLE :: ix _NULL
 end type block_control_type

public :: define_blocks

contains

!----------------------------------------------------------------------
! set up "blocks" used for OpenMP threading of column-based
! calculations using rad_n[x/y]xblock from coupler_nml
!---------------------------------------------------------------------
  subroutine define_blocks (component, Block, isc, iec, jsc, jec, kpts, &
                            nx_block, ny_block, message)
    character(len=*),         intent(in)    :: component
    type(block_control_type), intent(inout) :: Block
    integer,                  intent(in)    :: isc, iec, jsc, jec, kpts
    integer,                  intent(in)    :: nx_block, ny_block
    logical,                  intent(inout) :: message
!--- local variables
    integer :: blocks
    integer, dimension(nx_block) :: i1, i2
    integer, dimension(ny_block) :: j1, j2
    character(len=132) :: text
    integer :: i, j, nblks, ii, jj

    if (message) then
      if ((mod(iec-isc+1,nx_block) .ne. 0) .or. (mod(jec-jsc+1,ny_block) .ne. 0)) then
        write( text,'(a,a,2i4,a,2i4,a)' ) trim(component),'define_blocks: domain (',&
             (iec-isc+1), (jec-jsc+1),') is not an even divisor with definition (',&
             nx_block, ny_block,') - blocks will not be uniform'
        call mpp_error( NOTE, trim(text) )
      endif
      message = .false.
    endif

!--- set up blocks
    if (iec-isc+1 .lt. nx_block) &
        call mpp_error(FATAL, 'block_control: number of '//trim(component)//' nxblocks .gt. &
                             &number of elements in MPI-domain size')
    if (jec-jsc+1 .lt. ny_block) &
        call mpp_error(FATAL, 'block_control: number of '//trim(component)//' nyblocks .gt. &
                             &number of elements in MPI-domain size')
    call mpp_compute_extent(isc,iec,nx_block,i1,i2)
    call mpp_compute_extent(jsc,jec,ny_block,j1,j2)

    nblks = nx_block*ny_block
    Block%isc = isc
    Block%iec = iec
    Block%jsc = jsc
    Block%jec = jec
    Block%npz = kpts
    Block%nx_block = nx_block
    Block%ny_block = ny_block
    Block%nblks = nblks

    if (.not._ALLOCATED(Block%ibs)) &
         allocate (Block%ibs(nblks), &
                   Block%ibe(nblks), &
                   Block%jbs(nblks), &
                   Block%jbe(nblks), &
                   Block%ix(nblks) )

    blocks=0
    do j = 1, ny_block
      do i = 1, nx_block
        blocks = blocks + 1
        Block%ibs(blocks) = i1(i)
        Block%jbs(blocks) = j1(j)
        Block%ibe(blocks) = i2(i)
        Block%jbe(blocks) = j2(j)
        allocate(Block%ix(blocks)%ix(i1(i):i2(i),j1(j):j2(j)) )
        ix = 0
        do jj = j1(j), j2(j)
          do ii = i1(i), i2(i)
            ix = ix+1
            Block%ix(blocks)%ix(ii,jj) = ix
          enddo
        enddo
      enddo
    enddo

  end subroutine define_blocks

end module block_control_mod
