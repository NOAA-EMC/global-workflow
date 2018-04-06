!***********************************************************************
!*                   GNU Lesser General Public License                 
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it 
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or 
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be 
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty 
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.  
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
module fv_fill_mod
!
! Modules Included:
! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>mpp_domains_mod</td>
!     <td>mpp_update_domains, domain2D</td>
!   </tr>
!   <tr>
!     <td>platform_mod</td>
!     <td>kind_phys => r8_kind</td>
!   </tr>
! </table>

   use mpp_domains_mod,     only: mpp_update_domains, domain2D
   use platform_mod,        only: kind_phys => r8_kind

   implicit none
   public fillz
   public fill_gfs
   public fill2D

contains

!>@brief The subroutine 'fillz' is for mass-conservative filling of nonphysical negative values in the tracers. 
!>@details This routine takes mass from adjacent cells in the same column to fill negatives, if possible.
 subroutine fillz(im, km, nq, q, dp)
   integer,  intent(in):: im                !< No. of longitudes
   integer,  intent(in):: km                !< No. of levels
   integer,  intent(in):: nq                !< Total number of tracers
   real , intent(in)::  dp(im,km)           !< pressure thickness
   real , intent(inout) :: q(im,km,nq)      !< tracer mixing ratio
! LOCAL VARIABLES:
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

!>@brief The subroutine 'fill_gfs' is for mass-conservative filling of nonphysical negative values in the tracers. 
!>@details This routine is the same as 'fillz', but only fills one scalar field 
!! using specified q_min instead of 0 with the k-index flipped as in the GFS physics. 
!! It accepts edge pressure instead of delp.
 subroutine fill_gfs(im, km, pe2, q, q_min)
!SJL: this routine is the equivalent of fillz except that the vertical index is upside down
   integer, intent(in):: im, km
   real(kind=kind_phys), intent(in):: pe2(im,km+1)       !< pressure interface
   real(kind=kind_phys), intent(in):: q_min
   real(kind=kind_phys), intent(inout):: q(im,km)
!  LOCAL VARIABLES:
   real(kind=kind_phys) :: dp(im,km)
   integer:: i, k, k1

   do k=1,km
      do i=1,im
         dp(i,k) = pe2(i,k) - pe2(i,k+1)
      enddo
   enddo

! From bottom up:
   do k=1,km-1
      k1 = k+1
      do i=1,im
         if ( q(i,k)<q_min ) then
! Take mass from above so that q >= q_min
              q(i,k1) = q(i,k1) + (q(i,k)-q_min)*dp(i,k)/dp(i,k1)
              q(i,k ) = q_min
         endif
      enddo
   enddo

! From top down:
   do k=km,2,-1
      k1 = k-1
      do i=1,im
         if ( q(i,k)<0.0 ) then
! Take mass from below
              q(i,k1) = q(i,k1) + q(i,k)*dp(i,k)/dp(i,k1)
              q(i,k ) = 0.
         endif
      enddo
   enddo

 end subroutine fill_gfs

!>@brief The subroutine 'fill2D' fills in nonphysical negative values in a single scalar field 
!! using a two-dimensional diffusive approach which conserves mass.
 subroutine fill2D(is, ie, js, je, ng, km, q, delp, area, domain, nested, npx, npy)
! This is a diffusive type filling algorithm
 type(domain2D), intent(INOUT) :: domain
 integer, intent(in):: is, ie, js, je, ng, km, npx, npy
 logical, intent(IN):: nested
 real, intent(in):: area(is-ng:ie+ng, js-ng:je+ng)
 real, intent(in):: delp(is-ng:ie+ng, js-ng:je+ng, km)
 real, intent(inout):: q(is-ng:ie+ng, js-ng:je+ng, km)
! LOCAL VARIABLES:
 real, dimension(is-ng:ie+ng, js-ng:je+ng,km):: qt
 real, dimension(is:ie+1, js:je):: fx
 real, dimension(is:ie, js:je+1):: fy
 real, parameter:: dif = 0.25
 integer:: i, j, k
 integer :: is1, ie1, js1, je1

 if (nested) then
    if (is == 1) then
       is1 = is-1
    else
       is1 = is
    endif
    if (ie == npx-1) then
       ie1 = ie+1
    else
       ie1 = ie
    endif
    if (js == 1) then
       js1 = js-1
    else
       js1 = js
    endif
    if (je == npy-1) then
       je1 = je+1
    else
       je1 = je
    endif
 else
    is1 = is
    ie1 = ie
    js1 = js
    je1 = je
 endif

!$OMP parallel do default(shared)
 do k=1, km
    do j=js1,je1
       do i=is1,ie1
          qt(i,j,k) = q(i,j,k)*delp(i,j,k)*area(i,j)
       enddo
    enddo
 enddo
 call mpp_update_domains(qt, domain, whalo=1, ehalo=1, shalo=1, nhalo=1)

!$OMP parallel do default(shared) private(fx,fy)
 do k=1, km
    fx(:,:) = 0.
    do j=js,je
       do i=is,ie+1
          if( qt(i-1,j,k)*qt(i,j,k)<0. ) fx(i,j) = qt(i-1,j,k) - qt(i,j,k)
       enddo
    enddo
    fy(:,:) = 0.
    do j=js,je+1
       do i=is,ie
          if( qt(i,j-1,k)*qt(i,j,k)<0. ) fy(i,j) = qt(i,j-1,k) - qt(i,j,k)
       enddo
    enddo
    do j=js,je
       do i=is,ie
          q(i,j,k) = q(i,j,k)+dif*(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))/(delp(i,j,k)*area(i,j))
       enddo
    enddo
 enddo

 end subroutine fill2D

end module fv_fill_mod
