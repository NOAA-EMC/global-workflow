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
#undef MPP_READ_DISTRIBUTED_ASCII_1D_
#define MPP_READ_DISTRIBUTED_ASCII_1D_ mpp_read_distributed_ascii_r1D
#undef MPP_TYPE_
#define MPP_TYPE_ real
#include <mpp_read_distributed_ascii.h>

#undef MPP_READ_DISTRIBUTED_ASCII_1D_
#define MPP_READ_DISTRIBUTED_ASCII_1D_ mpp_read_distributed_ascii_i1D
#undef MPP_TYPE_
#define MPP_TYPE_ integer
#include <mpp_read_distributed_ascii.h>

subroutine mpp_read_distributed_ascii_a1D(unit,fmt,ssize,data,iostat)
  integer, intent(in)               :: unit
  character(*), intent(in)          :: fmt
  integer, intent(in)               :: ssize
  character(len=*), dimension(:), intent(inout) :: data
  integer, intent(out)              :: iostat

  integer, allocatable :: pelist(:)
  logical              :: is_ioroot=.false.


  if(.not.module_is_initialized) call mpp_error(FATAL,'mpp_read_distributed_ascii_a1D:  module not initialized')

  iostat = 0
  call mpp_dist_io_pelist(ssize,pelist)
  if(.not. ALLOCATED(pelist)) &
           call mpp_error(FATAL,'mpp_read_distributed_ascii_a1D: pelist allocation failed')
  is_ioroot = mpp_is_dist_ioroot(ssize)
  if(is_ioroot) then
    if(trim(fmt)=='*')then
       read(unit,*,iostat=iostat) data
    else
       read(unit,fmt=trim(fmt),iostat=iostat) data
    endif
    if(iostat /= 0) return  ! Calling routine must handle error
  endif

  call mpp_broadcast(data,len(data(1)),pelist(1),pelist)
  deallocate(pelist)  ! Don't forget to deallocate pelist
end subroutine mpp_read_distributed_ascii_a1D
