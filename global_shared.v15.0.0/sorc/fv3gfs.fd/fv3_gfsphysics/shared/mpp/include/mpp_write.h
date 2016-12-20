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
subroutine MPP_WRITE_( unit, field, data, tstamp)
      use mpp_parameter_mod, only : NULLUNIT
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      MPP_TYPE_, intent(in) :: data MPP_RANK_
      MPP_TYPE_, intent(in), optional :: tstamp

      if (unit == NULLUNIT) return
      MPP_WRITE_RECORD_
      return
    end subroutine MPP_WRITE_