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
    subroutine MPP_SUM_( a, length, pelist )
!sums array a over the PEs in pelist (all PEs if this argument is omitted)
!result is also automatically broadcast: all PEs have the sum in a at the end
  !we are using f77-style call: array passed by address and not descriptor; further, 
  !the f90 conformance check is avoided.
      integer, intent(in) :: length
      integer, intent(in), optional :: pelist(:)
      MPP_TYPE_, intent(inout) :: a(*)
      integer :: n, errunit
      MPP_TYPE_ :: work(length)

      if( .NOT.module_is_initialized )call mpp_error( FATAL, 'MPP_SUM: You must first call mpp_init.' )
      n = get_peset(pelist); if( peset(n)%count.EQ.1 )return

      if( debug .and. (current_clock.NE.0) )call SYSTEM_CLOCK(start_tick)
      if( verbose )call mpp_error( NOTE, 'MPP_SUM: using MPI_ALLREDUCE...' )
      if( debug ) then
          errunit = stderr()
          write( errunit,* )'pe, n, peset(n)%id=', pe, n, peset(n)%id
      endif
      call MPI_ALLREDUCE( a, work, length, MPI_TYPE_, MPI_SUM, peset(n)%id, error )
      a(1:length) = work(1:length)
      if( debug .and. (current_clock.NE.0) )call increment_current_clock( EVENT_ALLREDUCE, length*MPP_TYPE_BYTELEN_ )
      return
    end subroutine MPP_SUM_

!#######################################################################
#include <mpp_sum.inc>
