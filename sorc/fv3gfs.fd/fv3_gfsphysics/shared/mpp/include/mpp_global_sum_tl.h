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
  subroutine MPP_GLOBAL_SUM_TL_( domain, field, field_tl, gsum, gsum_tl, flags, position, tile_count )
    type(domain2D), intent(in) :: domain
    MPP_TYPE_, intent(inout) :: field(:,: MPP_EXTRA_INDICES_ )
    MPP_TYPE_, intent(inout) :: field_tl(:,: MPP_EXTRA_INDICES_ )
    MPP_TYPE_, intent(inout) :: gsum
    MPP_TYPE_, intent(inout) :: gsum_tl
    integer, intent(in), optional :: position
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: tile_count

    gsum = mpp_global_sum(domain, field, flags, position, tile_count )
    gsum_tl = mpp_global_sum(domain, field_tl, flags, position, tile_count )

    return
  end subroutine MPP_GLOBAL_SUM_TL_
