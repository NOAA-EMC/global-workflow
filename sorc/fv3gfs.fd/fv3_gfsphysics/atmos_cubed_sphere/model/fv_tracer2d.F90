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
module fv_tracer2d_mod
   use tp_core_mod,       only: fv_tp_2d, copy_corners
   use fv_mp_mod,         only: mp_reduce_max
   use fv_mp_mod,         only: ng, mp_gather, is_master
   use fv_mp_mod,         only: group_halo_update_type
   use fv_mp_mod,         only: start_group_halo_update, complete_group_halo_update
   use mpp_domains_mod,   only: mpp_update_domains, CGRID_NE, domain2d
   use fv_timing_mod,     only: timing_on, timing_off
   use boundary_mod,      only: nested_grid_BC_apply_intT
   use fv_arrays_mod,     only: fv_grid_type, fv_nest_type, fv_atmos_type, fv_grid_bounds_type
   use mpp_mod,           only: mpp_error, FATAL, mpp_broadcast, mpp_send, mpp_recv, mpp_sum, mpp_max

implicit none
private

public :: tracer_2d, tracer_2d_nested, tracer_2d_1L

real, allocatable, dimension(:,:,:) :: nest_fx_west_accum, nest_fx_east_accum, nest_fx_south_accum, nest_fx_north_accum

!---- version number -----
   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

!-----------------------------------------------------------------------
! !ROUTINE: Perform 2D horizontal-to-lagrangian transport
!-----------------------------------------------------------------------



subroutine tracer_2d_1L(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz,   &
                        nq,  hord, q_split, dt, id_divg, q_pack, nord_tr, trdm)

      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(IN) :: npx
      integer, intent(IN) :: npy
      integer, intent(IN) :: npz
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord, nord_tr
      integer, intent(IN) :: q_split
      integer, intent(IN) :: id_divg
      real   , intent(IN) :: dt, trdm
      type(group_halo_update_type), intent(inout) :: q_pack
      real   , intent(INOUT) :: q(bd%isd:bd%ied,bd%jsd:bd%jed,npz,nq)   ! Tracers
      real   , intent(INOUT) :: dp1(bd%isd:bd%ied,bd%jsd:bd%jed,npz)        ! DELP before dyn_core
      real   , intent(INOUT) :: mfx(bd%is:bd%ie+1,bd%js:bd%je,  npz)    ! Mass Flux X-Dir
      real   , intent(INOUT) :: mfy(bd%is:bd%ie  ,bd%js:bd%je+1,npz)    ! Mass Flux Y-Dir
      real   , intent(INOUT) ::  cx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)  ! Courant Number X-Dir
      real   , intent(INOUT) ::  cy(bd%isd:bd%ied,bd%js :bd%je +1,npz)  ! Courant Number Y-Dir
      type(fv_grid_type), intent(IN), target :: gridstruct
      type(domain2d), intent(INOUT) :: domain

! Local Arrays
      real :: qn2(bd%isd:bd%ied,bd%jsd:bd%jed,nq)   ! 3D tracers
      real :: dp2(bd%is:bd%ie,bd%js:bd%je)
      real :: fx(bd%is:bd%ie+1,bd%js:bd%je )
      real :: fy(bd%is:bd%ie , bd%js:bd%je+1)
      real :: ra_x(bd%is:bd%ie,bd%jsd:bd%jed)
      real :: ra_y(bd%isd:bd%ied,bd%js:bd%je)
      real :: xfx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)
      real :: yfx(bd%isd:bd%ied,bd%js: bd%je+1, npz)
      real :: cmax(npz)
      real :: frac
      integer :: nsplt
      integer :: i,j,k,it,iq

      real, pointer, dimension(:,:) :: area, rarea
      real, pointer, dimension(:,:,:) :: sin_sg
      real, pointer, dimension(:,:) :: dxa, dya, dx, dy

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

       area => gridstruct%area
      rarea => gridstruct%rarea

      sin_sg => gridstruct%sin_sg
      dxa    => gridstruct%dxa 
      dya    => gridstruct%dya 
      dx     => gridstruct%dx  
      dy     => gridstruct%dy  

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,xfx,dxa,dy, &
!$OMP                                  sin_sg,cy,yfx,dya,dx,cmax)
  do k=1,npz
     do j=jsd,jed
        do i=is,ie+1
           if (cx(i,j,k) > 0.) then
              xfx(i,j,k) = cx(i,j,k)*dxa(i-1,j)*dy(i,j)*sin_sg(i-1,j,3)
           else
              xfx(i,j,k) = cx(i,j,k)*dxa(i,  j)*dy(i,j)*sin_sg(i,  j,1)
           endif
        enddo
     enddo
     do j=js,je+1
        do i=isd,ied
           if (cy(i,j,k) > 0.) then
              yfx(i,j,k) = cy(i,j,k)*dya(i,j-1)*dx(i,j)*sin_sg(i,j-1,4)
           else
              yfx(i,j,k) = cy(i,j,k)*dya(i,j  )*dx(i,j)*sin_sg(i,j,  2)
           endif
        enddo
     enddo

     cmax(k) = 0.
     if ( k < npz/6 ) then
          do j=js,je
             do i=is,ie
                cmax(k) = max( cmax(k), abs(cx(i,j,k)), abs(cy(i,j,k)) )
             enddo
          enddo
     else
          do j=js,je
             do i=is,ie
                cmax(k) = max( cmax(k), max(abs(cx(i,j,k)),abs(cy(i,j,k)))+1.-sin_sg(i,j,5) )
             enddo
          enddo
     endif
  enddo  ! k-loop

  call mp_reduce_max(cmax,npz)

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,xfx, &
!$OMP                                  cy,yfx,mfx,mfy,cmax)   &
!$OMP                          private(nsplt, frac)
  do k=1,npz

     nsplt = int(1. + cmax(k))
     if ( nsplt > 1 ) then
        frac  = 1. / real(nsplt)
        do j=jsd,jed
           do i=is,ie+1
               cx(i,j,k) =  cx(i,j,k) * frac
              xfx(i,j,k) = xfx(i,j,k) * frac
           enddo
        enddo
        do j=js,je
           do i=is,ie+1
              mfx(i,j,k) = mfx(i,j,k) * frac
           enddo
        enddo
        do j=js,je+1
           do i=isd,ied
              cy(i,j,k) =  cy(i,j,k) * frac
             yfx(i,j,k) = yfx(i,j,k) * frac
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              mfy(i,j,k) = mfy(i,j,k) * frac
           enddo
        enddo
     endif

  enddo
                               call timing_on('COMM_TOTAL')
                         call timing_on('COMM_TRACER')
  call complete_group_halo_update(q_pack, domain)
                        call timing_off('COMM_TRACER')
                              call timing_off('COMM_TOTAL')

! Begin k-independent tracer transport; can not be OpenMPed because the mpp_update call.
  do k=1,npz

!$OMP parallel do default(none) shared(k,is,ie,js,je,isd,ied,jsd,jed,xfx,area,yfx,ra_x,ra_y)
     do j=jsd,jed
        do i=is,ie
           ra_x(i,j) = area(i,j) + xfx(i,j,k) - xfx(i+1,j,k)
        enddo
        if ( j>=js .and. j<=je ) then
           do i=isd,ied
              ra_y(i,j) = area(i,j) + yfx(i,j,k) - yfx(i,j+1,k)
           enddo
        endif
     enddo

     nsplt = int(1. + cmax(k))
     do it=1,nsplt

!$OMP parallel do default(none) shared(k,is,ie,js,je,rarea,mfx,mfy,dp1,dp2)
        do j=js,je
           do i=is,ie
              dp2(i,j) = dp1(i,j,k) + (mfx(i,j,k)-mfx(i+1,j,k)+mfy(i,j,k)-mfy(i,j+1,k))*rarea(i,j)
           enddo
        enddo

!$OMP parallel do default(none) shared(k,nsplt,it,is,ie,js,je,isd,ied,jsd,jed,npx,npy,cx,xfx,hord,trdm, &
!$OMP                                  nord_tr,nq,gridstruct,bd,cy,yfx,mfx,mfy,qn2,q,ra_x,ra_y,dp1,dp2,rarea) &
!$OMP                          private(fx,fy)
        do iq=1,nq
        if ( nsplt /= 1 ) then
           if ( it==1 ) then
              do j=jsd,jed
                 do i=isd,ied
                    qn2(i,j,iq) = q(i,j,k,iq)
                 enddo
              enddo
           endif
           call fv_tp_2d(qn2(isd,jsd,iq), cx(is,jsd,k), cy(isd,js,k), &
                         npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                         gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k))
           if ( it < nsplt ) then   ! not last call
              do j=js,je
              do i=is,ie
                 qn2(i,j,iq) = (qn2(i,j,iq)*dp1(i,j,k)+(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))*rarea(i,j))/dp2(i,j)
              enddo
              enddo
           else
              do j=js,je
              do i=is,ie
                 q(i,j,k,iq) = (qn2(i,j,iq)*dp1(i,j,k)+(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))*rarea(i,j))/dp2(i,j)
              enddo
              enddo
           endif
        else
           call fv_tp_2d(q(isd,jsd,k,iq), cx(is,jsd,k), cy(isd,js,k), &
                         npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                         gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k))
           do j=js,je
              do i=is,ie
                 q(i,j,k,iq) = (q(i,j,k,iq)*dp1(i,j,k)+(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))*rarea(i,j))/dp2(i,j)
              enddo
           enddo
        endif
        enddo   !  tracer-loop

        if ( it < nsplt ) then   ! not last call
             do j=js,je
                do i=is,ie
                   dp1(i,j,k) = dp2(i,j)
                enddo
             enddo
                               call timing_on('COMM_TOTAL')
                         call timing_on('COMM_TRACER')
             call mpp_update_domains(qn2, domain)
                        call timing_off('COMM_TRACER')
                              call timing_off('COMM_TOTAL')
        endif
     enddo  ! time-split loop
  enddo    ! k-loop

end subroutine tracer_2d_1L


subroutine tracer_2d(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz,   &
                     nq,  hord, q_split, dt, id_divg, q_pack, nord_tr, trdm)

      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(IN) :: npx
      integer, intent(IN) :: npy
      integer, intent(IN) :: npz
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord, nord_tr
      integer, intent(IN) :: q_split
      integer, intent(IN) :: id_divg
      real   , intent(IN) :: dt, trdm
      type(group_halo_update_type), intent(inout) :: q_pack
      real   , intent(INOUT) :: q(bd%isd:bd%ied,bd%jsd:bd%jed,npz,nq)   ! Tracers
      real   , intent(INOUT) :: dp1(bd%isd:bd%ied,bd%jsd:bd%jed,npz)        ! DELP before dyn_core
      real   , intent(INOUT) :: mfx(bd%is:bd%ie+1,bd%js:bd%je,  npz)    ! Mass Flux X-Dir
      real   , intent(INOUT) :: mfy(bd%is:bd%ie  ,bd%js:bd%je+1,npz)    ! Mass Flux Y-Dir
      real   , intent(INOUT) ::  cx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)  ! Courant Number X-Dir
      real   , intent(INOUT) ::  cy(bd%isd:bd%ied,bd%js :bd%je +1,npz)  ! Courant Number Y-Dir
      type(fv_grid_type), intent(IN), target :: gridstruct
      type(domain2d), intent(INOUT) :: domain

! Local Arrays
      real :: dp2(bd%is:bd%ie,bd%js:bd%je)
      real :: fx(bd%is:bd%ie+1,bd%js:bd%je )
      real :: fy(bd%is:bd%ie , bd%js:bd%je+1)
      real :: ra_x(bd%is:bd%ie,bd%jsd:bd%jed)
      real :: ra_y(bd%isd:bd%ied,bd%js:bd%je)
      real :: xfx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)
      real :: yfx(bd%isd:bd%ied,bd%js: bd%je+1, npz)
      real :: cmax(npz)
      real :: c_global
      real :: frac, rdt
      integer :: ksplt(npz)
      integer :: nsplt
      integer :: i,j,k,it,iq

      real, pointer, dimension(:,:) :: area, rarea
      real, pointer, dimension(:,:,:) :: sin_sg
      real, pointer, dimension(:,:) :: dxa, dya, dx, dy

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

       area => gridstruct%area
      rarea => gridstruct%rarea

      sin_sg => gridstruct%sin_sg
      dxa    => gridstruct%dxa 
      dya    => gridstruct%dya 
      dx     => gridstruct%dx  
      dy     => gridstruct%dy  

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,xfx,dxa,dy, &  
!$OMP                                  sin_sg,cy,yfx,dya,dx,cmax,q_split,ksplt)
    do k=1,npz
       do j=jsd,jed
          do i=is,ie+1
             if (cx(i,j,k) > 0.) then
                 xfx(i,j,k) = cx(i,j,k)*dxa(i-1,j)*dy(i,j)*sin_sg(i-1,j,3)
             else
                 xfx(i,j,k) = cx(i,j,k)*dxa(i,j)*dy(i,j)*sin_sg(i,j,1)
             endif
          enddo
       enddo
       do j=js,je+1
          do i=isd,ied
              if (cy(i,j,k) > 0.) then
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j-1)*dx(i,j)*sin_sg(i,j-1,4)
              else
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j)*dx(i,j)*sin_sg(i,j,2)
              endif
          enddo
       enddo

       if ( q_split == 0 ) then
         cmax(k) = 0.
         if ( k < npz/6 ) then
            do j=js,je
               do i=is,ie
                  cmax(k) = max( cmax(k), abs(cx(i,j,k)), abs(cy(i,j,k)) )
               enddo
            enddo
         else
            do j=js,je
               do i=is,ie
                  cmax(k) = max( cmax(k), max(abs(cx(i,j,k)),abs(cy(i,j,k)))+1.-sin_sg(i,j,5) )
               enddo
            enddo
         endif
       endif
       ksplt(k) = 1

    enddo

!--------------------------------------------------------------------------------

! Determine global nsplt:
  if ( q_split == 0 ) then
      call mp_reduce_max(cmax,npz)
! find global max courant number and define nsplt to scale cx,cy,mfx,mfy
      c_global = cmax(1)
      if ( npz /= 1 ) then                ! if NOT shallow water test case
         do k=2,npz
            c_global = max(cmax(k), c_global)
         enddo
      endif
      nsplt = int(1. + c_global)
      if ( is_master() .and. nsplt > 4 )  write(*,*) 'Tracer_2d_split=', nsplt, c_global
   else
      nsplt = q_split
   endif

!--------------------------------------------------------------------------------

    if( nsplt /= 1 ) then
!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,xfx,mfx,cy,yfx,mfy,cmax,nsplt,ksplt) &
!$OMP                          private( frac )
        do k=1,npz

#ifdef GLOBAL_CFL
           ksplt(k) = nsplt
#else
           ksplt(k) = int(1. + cmax(k))
#endif
           frac  = 1. / real(ksplt(k))

           do j=jsd,jed
              do i=is,ie+1
                 cx(i,j,k) =   cx(i,j,k) * frac
                 xfx(i,j,k) = xfx(i,j,k) * frac
              enddo
           enddo
           do j=js,je
              do i=is,ie+1
                 mfx(i,j,k) = mfx(i,j,k) * frac
              enddo
           enddo

           do j=js,je+1
              do i=isd,ied
                 cy(i,j,k) =  cy(i,j,k) * frac
                yfx(i,j,k) = yfx(i,j,k) * frac
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                mfy(i,j,k) = mfy(i,j,k) * frac
              enddo
           enddo

        enddo
    endif

    do it=1,nsplt
                        call timing_on('COMM_TOTAL')
                            call timing_on('COMM_TRACER')
      call complete_group_halo_update(q_pack, domain)
                           call timing_off('COMM_TRACER')
                       call timing_off('COMM_TOTAL')

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,dp1,mfx,mfy,rarea,nq,ksplt,&
!$OMP                                  area,xfx,yfx,q,cx,cy,npx,npy,hord,gridstruct,bd,it,nsplt,nord_tr,trdm) &
!$OMP                          private(dp2, ra_x, ra_y, fx, fy)
     do k=1,npz

       if ( it .le. ksplt(k) ) then

         do j=js,je
            do i=is,ie
               dp2(i,j) = dp1(i,j,k) + (mfx(i,j,k)-mfx(i+1,j,k)+mfy(i,j,k)-mfy(i,j+1,k))*rarea(i,j)
            enddo
         enddo

         do j=jsd,jed
            do i=is,ie
               ra_x(i,j) = area(i,j) + xfx(i,j,k) - xfx(i+1,j,k)
            enddo
         enddo
         do j=js,je
            do i=isd,ied
               ra_y(i,j) = area(i,j) + yfx(i,j,k) - yfx(i,j+1,k)
            enddo
         enddo

         do iq=1,nq
         if ( it==1 .and. trdm>1.e-4 ) then
            call fv_tp_2d(q(isd,jsd,k,iq), cx(is,jsd,k), cy(isd,js,k), &
                          npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                          gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k),   &
                          mass=dp1(isd,jsd,k), nord=nord_tr, damp_c=trdm)
         else
            call fv_tp_2d(q(isd,jsd,k,iq), cx(is,jsd,k), cy(isd,js,k), &
                          npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                          gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k))
         endif
            do j=js,je
               do i=is,ie
                  q(i,j,k,iq) = ( q(i,j,k,iq)*dp1(i,j,k) + &
                                (fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))*rarea(i,j) )/dp2(i,j)
               enddo
               enddo
            enddo

         if ( it /= nsplt ) then
              do j=js,je
                 do i=is,ie
                    dp1(i,j,k) = dp2(i,j)
                 enddo
              enddo
         endif

       endif   ! ksplt

     enddo ! npz

      if ( it /= nsplt ) then
                      call timing_on('COMM_TOTAL')
                          call timing_on('COMM_TRACER')
           call start_group_halo_update(q_pack, q, domain)
                          call timing_off('COMM_TRACER')
                      call timing_off('COMM_TOTAL')
      endif

   enddo  ! nsplt


end subroutine tracer_2d


subroutine tracer_2d_nested(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz,   &
                     nq,  hord, q_split, dt, id_divg, q_pack, nord_tr, trdm, &
                     k_split, neststruct, parent_grid)

      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(IN) :: npx
      integer, intent(IN) :: npy
      integer, intent(IN) :: npz
      integer, intent(IN) :: nq    ! number of tracers to be advected
      integer, intent(IN) :: hord, nord_tr
      integer, intent(IN) :: q_split, k_split
      integer, intent(IN) :: id_divg
      real   , intent(IN) :: dt, trdm
      type(group_halo_update_type), intent(inout) :: q_pack
      real   , intent(INOUT) :: q(bd%isd:bd%ied,bd%jsd:bd%jed,npz,nq)   ! Tracers
      real   , intent(INOUT) :: dp1(bd%isd:bd%ied,bd%jsd:bd%jed,npz)        ! DELP before dyn_core
      real   , intent(INOUT) :: mfx(bd%is:bd%ie+1,bd%js:bd%je,  npz)    ! Mass Flux X-Dir
      real   , intent(INOUT) :: mfy(bd%is:bd%ie  ,bd%js:bd%je+1,npz)    ! Mass Flux Y-Dir
      real   , intent(INOUT) ::  cx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)  ! Courant Number X-Dir
      real   , intent(INOUT) ::  cy(bd%isd:bd%ied,bd%js :bd%je +1,npz)  ! Courant Number Y-Dir
      type(fv_grid_type), intent(IN), target :: gridstruct
      type(fv_nest_type), intent(INOUT) :: neststruct
      type(fv_atmos_type), intent(INOUT) :: parent_grid
      type(domain2d), intent(INOUT) :: domain

! Local Arrays
      real :: dp2(bd%is:bd%ie,bd%js:bd%je)
      real :: fx(bd%is:bd%ie+1,bd%js:bd%je )
      real :: fy(bd%is:bd%ie , bd%js:bd%je+1)
      real :: ra_x(bd%is:bd%ie,bd%jsd:bd%jed)
      real :: ra_y(bd%isd:bd%ied,bd%js:bd%je)
      real :: xfx(bd%is:bd%ie+1,bd%jsd:bd%jed  ,npz)
      real :: yfx(bd%isd:bd%ied,bd%js: bd%je+1, npz)
      real :: cmax(npz)
      real :: cmax_t
      real :: c_global
      real :: frac, rdt
      integer :: nsplt, nsplt_parent, msg_split_steps = 1
      integer :: i,j,k,it,iq

      real, pointer, dimension(:,:) :: area, rarea
      real, pointer, dimension(:,:,:) :: sin_sg
      real, pointer, dimension(:,:) :: dxa, dya, dx, dy

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

       area => gridstruct%area
      rarea => gridstruct%rarea

      sin_sg => gridstruct%sin_sg
      dxa    => gridstruct%dxa 
      dya    => gridstruct%dya 
      dx     => gridstruct%dx  
      dy     => gridstruct%dy  

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,xfx,dxa,dy, &
!$OMP                                  sin_sg,cy,yfx,dya,dx)
      do k=1,npz
         do j=jsd,jed
            do i=is,ie+1
               if (cx(i,j,k) > 0.) then
                  xfx(i,j,k) = cx(i,j,k)*dxa(i-1,j)*dy(i,j)*sin_sg(i-1,j,3)
               else
                  xfx(i,j,k) = cx(i,j,k)*dxa(i,j)*dy(i,j)*sin_sg(i,j,1)
               endif
            enddo
         enddo
         do j=js,je+1
            do i=isd,ied
               if (cy(i,j,k) > 0.) then
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j-1)*dx(i,j)*sin_sg(i,j-1,4)
               else
                  yfx(i,j,k) = cy(i,j,k)*dya(i,j)*dx(i,j)*sin_sg(i,j,2)
               endif
            enddo
         enddo
      enddo

!--------------------------------------------------------------------------------
  if ( q_split == 0 ) then
! Determine nsplt

!$OMP parallel do default(none) shared(is,ie,js,je,npz,cmax,cx,cy,sin_sg) &
!$OMP                          private(cmax_t )
      do k=1,npz
         cmax(k) = 0.
         if ( k < 4 ) then
! Top layers: C < max( abs(c_x), abs(c_y) )
            do j=js,je
               do i=is,ie
                  cmax_t  = max( abs(cx(i,j,k)), abs(cy(i,j,k)) )
                  cmax(k) = max( cmax_t, cmax(k) )
               enddo
            enddo
         else
            do j=js,je
               do i=is,ie
                  cmax_t  = max(abs(cx(i,j,k)), abs(cy(i,j,k))) + 1.-sin_sg(i,j,5)
                  cmax(k) = max( cmax_t, cmax(k) )
               enddo
            enddo
         endif
      enddo
      call mp_reduce_max(cmax,npz)

! find global max courant number and define nsplt to scale cx,cy,mfx,mfy
      c_global = cmax(1)
      if ( npz /= 1 ) then                ! if NOT shallow water test case
         do k=2,npz
            c_global = max(cmax(k), c_global)
         enddo
      endif
      nsplt = int(1. + c_global)
      if ( is_master() .and. nsplt > 3 )  write(*,*) 'Tracer_2d_split=', nsplt, c_global
   else
      nsplt = q_split
      if (gridstruct%nested .and. neststruct%nestbctype > 1) msg_split_steps = max(q_split/parent_grid%flagstruct%q_split,1)
   endif

!--------------------------------------------------------------------------------

   frac  = 1. / real(nsplt)

      if( nsplt /= 1 ) then
!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,cx,frac,xfx,mfx,cy,yfx,mfy)
          do k=1,npz
             do j=jsd,jed
                do i=is,ie+1
                   cx(i,j,k) =  cx(i,j,k) * frac
                   xfx(i,j,k) = xfx(i,j,k) * frac
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   mfx(i,j,k) = mfx(i,j,k) * frac
                enddo
             enddo

             do j=js,je+1
                do i=isd,ied
                   cy(i,j,k) =  cy(i,j,k) * frac
                  yfx(i,j,k) = yfx(i,j,k) * frac
                enddo
             enddo

             do j=js,je+1
                do i=is,ie
                  mfy(i,j,k) = mfy(i,j,k) * frac
                enddo
             enddo
          enddo
      endif


    do it=1,nsplt
       if ( gridstruct%nested ) then
          neststruct%tracer_nest_timestep = neststruct%tracer_nest_timestep + 1
       end if
                        call timing_on('COMM_TOTAL')
                            call timing_on('COMM_TRACER')
      call complete_group_halo_update(q_pack, domain)
                           call timing_off('COMM_TRACER')
                       call timing_off('COMM_TOTAL')
	    
      if (gridstruct%nested) then
            do iq=1,nq
                 call nested_grid_BC_apply_intT(q(isd:ied,jsd:jed,:,iq), &
                      0, 0, npx, npy, npz, bd, &
                      real(neststruct%tracer_nest_timestep)+real(nsplt*k_split), real(nsplt*k_split), &
                 neststruct%q_BC(iq), bctype=neststruct%nestbctype  )
           enddo
      endif


!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,dp1,mfx,mfy,rarea,nq, &
!$OMP                                  area,xfx,yfx,q,cx,cy,npx,npy,hord,gridstruct,bd,it,nsplt,nord_tr,trdm) &
!$OMP                          private(dp2, ra_x, ra_y, fx, fy)
      do k=1,npz

         do j=js,je
            do i=is,ie
               dp2(i,j) = dp1(i,j,k) + (mfx(i,j,k)-mfx(i+1,j,k)+mfy(i,j,k)-mfy(i,j+1,k))*rarea(i,j)
            enddo
         enddo

         do j=jsd,jed
            do i=is,ie
               ra_x(i,j) = area(i,j) + xfx(i,j,k) - xfx(i+1,j,k)
            enddo
         enddo
         do j=js,je
            do i=isd,ied
               ra_y(i,j) = area(i,j) + yfx(i,j,k) - yfx(i,j+1,k)
            enddo
         enddo

         do iq=1,nq
         if ( it==1 .and. trdm>1.e-4 ) then
            call fv_tp_2d(q(isd,jsd,k,iq), cx(is,jsd,k), cy(isd,js,k), &
                          npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                          gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k),   &
                          mass=dp1(isd,jsd,k), nord=nord_tr, damp_c=trdm)
         else
            call fv_tp_2d(q(isd,jsd,k,iq), cx(is,jsd,k), cy(isd,js,k), &
                          npx, npy, hord, fx, fy, xfx(is,jsd,k), yfx(isd,js,k), &
                          gridstruct, bd, ra_x, ra_y, mfx=mfx(is,js,k), mfy=mfy(is,js,k))
         endif
            do j=js,je
               do i=is,ie
                  q(i,j,k,iq) = ( q(i,j,k,iq)*dp1(i,j,k) + &
                                (fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))*rarea(i,j) )/dp2(i,j)
               enddo
               enddo
          enddo
      enddo ! npz

      if ( it /= nsplt ) then
                      call timing_on('COMM_TOTAL')
                          call timing_on('COMM_TRACER')
           call start_group_halo_update(q_pack, q, domain)
                          call timing_off('COMM_TRACER')
                      call timing_off('COMM_TOTAL')
      endif
           !Apply nested-grid BCs
           if ( gridstruct%nested ) then
              do iq=1,nq


                 call nested_grid_BC_apply_intT(q(isd:ied,jsd:jed,:,iq), &
                      0, 0, npx, npy, npz, bd, &
                      real(neststruct%tracer_nest_timestep), real(nsplt*k_split), &
                 neststruct%q_BC(iq), bctype=neststruct%nestbctype  )

              end do
           end if


   enddo  ! nsplt

   if ( id_divg > 0 ) then
        rdt = 1./(frac*dt)

!$OMP parallel do default(none) shared(is,ie,js,je,npz,dp1,xfx,yfx,rarea,rdt)
        do k=1,npz
        do j=js,je
           do i=is,ie
              dp1(i,j,k) = (xfx(i+1,j,k)-xfx(i,j,k) + yfx(i,j+1,k)-yfx(i,j,k))*rarea(i,j)*rdt
           enddo
        enddo
        enddo
   endif

 end subroutine tracer_2d_nested

end module fv_tracer2d_mod
