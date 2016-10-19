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
module fv_fill_mod
   implicit none
   public fillz

!---- version number -----
   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

 subroutine fillz(im, km, nq, q, dp)
   integer,  intent(in):: im                ! No. of longitudes
   integer,  intent(in):: km                ! No. of levels
   integer,  intent(in):: nq                ! Total number of tracers
   real , intent(in)::  dp(im,km)       ! pressure thickness
   real , intent(inout) :: q(im,km,nq)   ! tracer mixing ratio
! !LOCAL VARIABLES:
   logical:: zfix(im)
   real ::  dm(km)
   integer i, k, ic, k1
   real  qup, qly, dup, dq, sum0, sum1, fac

   do ic=1,nq
#ifdef DEV_GFS_PHYS
! Bottom up:
      do k=km,2,-1
         k1 = k-1
         do i=1,im
           if( q(i,k,ic) < 0. ) then
               q(i,k1,ic) = q(i,k1,ic) + q(i,k,ic)*dp(i,k)/dp(i,k1)
               q(i,k ,ic) = 0.
           endif
         enddo
      enddo
! Top down:
      do k=1,km-1
         k1 = k+1
         do i=1,im
            if( q(i,k,ic) < 0. ) then
                q(i,k1,ic) = q(i,k1,ic) + q(i,k,ic)*dp(i,k)/dp(i,k1)
                q(i,k ,ic) = 0.
            endif
         enddo
      enddo
#else
! Top layer
      do i=1,im
         if( q(i,1,ic) < 0. ) then
             q(i,2,ic) = q(i,2,ic) + q(i,1,ic)*dp(i,1)/dp(i,2)
             q(i,1,ic) = 0.
          endif
      enddo

! Interior
      zfix(:) = .false.
      do k=2,km-1
         do i=1,im
         if( q(i,k,ic) < 0. ) then
             zfix(i) = .true.
             if ( q(i,k-1,ic) > 0. ) then
! Borrow from above
                dq = min ( q(i,k-1,ic)*dp(i,k-1), -q(i,k,ic)*dp(i,k) ) 
                q(i,k-1,ic) = q(i,k-1,ic) - dq/dp(i,k-1)
                q(i,k  ,ic) = q(i,k  ,ic) + dq/dp(i,k  )
             endif
             if ( q(i,k,ic)<0.0 .and. q(i,k+1,ic)>0. ) then
! Borrow from below:
                dq = min ( q(i,k+1,ic)*dp(i,k+1), -q(i,k,ic)*dp(i,k) ) 
                q(i,k+1,ic) = q(i,k+1,ic) - dq/dp(i,k+1)
                q(i,k  ,ic) = q(i,k  ,ic) + dq/dp(i,k  )
             endif
          endif
         enddo
      enddo
 
! Bottom layer
      k = km
      do i=1,im
         if( q(i,k,ic)<0. .and. q(i,k-1,ic)>0.) then
             zfix(i) = .true.
! Borrow from above
             qup =  q(i,k-1,ic)*dp(i,k-1)
             qly = -q(i,k  ,ic)*dp(i,k  )
             dup =  min(qly, qup)
             q(i,k-1,ic) = q(i,k-1,ic) - dup/dp(i,k-1) 
             q(i,k,  ic) = q(i,k,  ic) + dup/dp(i,k  )
          endif
      enddo

! Perform final check and non-local fix if needed
      do i=1,im
         if ( zfix(i) ) then

           sum0 = 0.
           do k=2,km
              dm(k) = q(i,k,ic)*dp(i,k)
              sum0 = sum0 + dm(k)
           enddo

           if ( sum0 > 0. ) then
             sum1 = 0.
             do k=2,km
                sum1 = sum1 + max(0., dm(k))
             enddo
             fac = sum0 / sum1
             do k=2,km
                q(i,k,ic) = max(0., fac*dm(k)/dp(i,k))
             enddo
           endif

         endif
      enddo
#endif

   enddo
 end subroutine fillz


end module fv_fill_mod
