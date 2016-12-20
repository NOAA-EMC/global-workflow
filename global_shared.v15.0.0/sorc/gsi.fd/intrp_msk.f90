! subroutines in this file handle the surface mask dependent interpolations
! int22_msk_glb     : global area, interpolate from 2-d to 2-d
! int21_msk_glb     : global area, interpolate from 2-d to one point
! int22_msk_sub     : sub-domain, interpolate from 2-d to 2-d
! int21_msk_sub     : sub-domain, interpolate from 2-d to one point
! int2_msk_glb_prep : global area, expand the area (grids) with a specified surface type
! int2_msk_sub_prep : global area, expand the area (grids) with a specified surface type
! dtzm_point        : get the vertical mean of the departure from Tf for NSST T-Profile at a point
! dtzm_2d           : get the vertical mean of the departure from Tf for NSST T-Profile 2d array
!

 subroutine int22_msk_glb(a,isli_a,rlats_a,rlons_a,nlat_a,nlon_a, &
                          b,isli_b,rlats_b,rlons_b,nlat_b,nlon_b,istyp)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    int22_msk_glb ---(global 2-d array)
!                interpolates from a to b with ancillary surface mask (e.g.,
!                analysis grid => surface grid) for global arrays
!                to gurantee the interpolated value (b) is determined by the
!                candidates (a) with the identical surface type from (a)
!
! prgrmmr:     li -  initial version; org: np2. 02/01/2014
!
! abstract :      This routine interpolates a grid to b grid with surface mask accounted
! notes    :      (1) Here is a 2-d to 2-d interpolation
!                 (2) The interpolation is performed for specified surface types (istyp)
!
! program history log:
!
!  input argument list:
!    a        - real: 2-d array such as analysis increment at analysis grids
!    isli_a   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = sea ice) for a grids
!    rlats_a  - real: 1-d array: the latitudes of a
!    rlons_a  - real: 1-d array: the logitudes of a
!    nlat_a   - integer: number of latitude of a
!    nlon_a   - integer: number of longitude of a

!    isli_b   - integer: 2-d array: Analysis surface mask (0 = water, 1 = land, 2 = sea ice) for b grids
!    rlats_b  - real: 1-d array: the latitudes of b
!    rlons_b  - real: 1-d array: the logitudes of b
!    nlat_b   - integer: number of latitude of b
!    nlon_b   - integer: number of longitude of b
!    istyp    - integer: target surface type value
!
!  output argument list:
!    b       - real: 2-d array such as analysis increment at surface grids
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nlat_a,nlon_a), intent(in   ) :: a
 integer(i_kind), dimension(nlat_a,nlon_a), intent(in   ) :: isli_a

 real   (r_kind), dimension(nlat_a       ), intent(in   ) :: rlats_a
 real   (r_kind), dimension(nlon_a       ), intent(in   ) :: rlons_a

 integer(i_kind), dimension(nlat_b,nlon_b), intent(in   ) :: isli_b
 real   (r_kind), dimension(nlat_b       ), intent(in   ) :: rlats_b
 real   (r_kind), dimension(nlon_b       ), intent(in   ) :: rlons_b

 integer(i_kind), intent(in   ) :: nlat_a,nlon_a,nlat_b,nlon_b,istyp

!OUTPUT:
 real   (r_kind), dimension(nlat_b,nlon_b), intent(  out) :: b

!Declare local variables
 integer(i_kind) :: i,j,ix,iy,ii,jj,ixa,iya,sfctyp_b
 integer(i_kind) :: nwsum,nfinal
 real(r_kind)    :: dx0,dx1,dx2,dx3,dy0,dy1,dy2,dy3,dx,dy,dr
 real(r_kind)    :: dx0s,dx1s,dx2s,dx3s,dy0s,dy1s,dy2s,dy3s
 real(r_kind)    :: ds00,ds01,ds02,ds03,ds10,ds11,ds12,ds13,ds20,ds21,ds22,ds23,ds30,ds31,ds32,ds33

 real(r_kind)    :: bavg,bout,dlat,dlon,wsum4,wsum16
 real(r_kind), dimension(0:1,0:1) :: w4
 real(r_kind), dimension(0:3,0:3) :: w16

 dr = 8.0_r_kind    ! square of the search radius for 16-point cressman-type analysis

 nwsum  = 0
 nfinal = 0
 b=zero
!Loop over all grids of array b to get interpolated value
 do j = 1, nlon_b
   do i = 1, nlat_b
     sfctyp_b = istyp
     if ( isli_b(i,j) == sfctyp_b ) then
       dlon = rlons_b(j)
       call grdcrd1(dlon,rlons_a,nlon_a,1)
       iy = int(dlon); dy = dlon-iy; dy1 = one-dy
 
       iy  = min(max(0,iy),nlon_a); if(iy == 0) iy = nlon_a

       dlat = rlats_b(i)
       call grdcrd1(dlat,rlats_a,nlat_a,1)
       ix = int(dlat); dx = dlat-ix; dx1 = one-dx

       ix = min(max(1,ix),nlat_a)

       w4(0,0) = dx1*dy1; w4(0,1) = dx1*dy; w4(1,0) = dx*dy1; w4(1,1) = dx*dy
!
!      get the interpolated value with the nearby 4-grids (in a) which has
!      the identical surface mask (in b) only

       wsum4 = zero
       bavg  = zero
       bout  = zero
       do jj = 0, 1
         iya = iy + jj
         if ( iya == nlon_a + 1 ) iya = 1
         do ii = 0, 1
           ixa = min(nlat_a,ix + ii)
           bavg  = bavg + w4(ii,jj)*a(ixa,iya)
           if ( isli_a(ixa,iya) == sfctyp_b ) then
               wsum4 = wsum4 + w4(ii,jj)
             bout  = bout  + w4(ii,jj)*a(ixa,iya)
           endif
         enddo
       enddo

       if ( wsum4 > zero ) then
         bout = bout/wsum4
       else

         nwsum = nwsum + 1

!    use more candidates from a (extending one more grid futher in both x and y
!    direction, which means 16 grids from a will be used) when no same
!    surface type candidates can be found in the nearby 4 grids
!    to perform a Cressman_type Analysis

         ix = ix -1; if(ix == 0) ix = 1
         iy = iy -1; if(iy == 0) iy = nlon_a

         dx0 = dx + one; dx1 = dx; dx2 = one - dx; dx3 = two - dx
         dy0 = dy + one; dy1 = dy; dy2 = one - dy; dy3 = two - dy

         dx0s = dx0*dx0; dx1s = dx1*dx1; dx2s = dx2*dx2; dx3s = dx3*dx3
         dy0s = dy0*dy0; dy1s = dy1*dy1; dy2s = dy2*dy2; dy3s = dy3*dy3

         ds00 = dx0s + dy0s; ds01 = dx0s + dy1s; ds02 = dx0s + dy2s; ds03 = dx0s + dy3s
         ds10 = dx1s + dy0s; ds11 = dx1s + dy1s; ds12 = dx1s + dy2s; ds13 = dx1s + dy3s
         ds20 = dx2s + dy0s; ds21 = dx2s + dy1s; ds22 = dx2s + dy2s; ds23 = dx2s + dy3s
         ds30 = dx3s + dy0s; ds31 = dx3s + dy1s; ds32 = dx3s + dy2s; ds33 = dx3s + dy3s

         w16(0,0) = (dr - ds00)/(dr + ds00)
         w16(0,1) = (dr - ds01)/(dr + ds01)
         w16(0,2) = (dr - ds02)/(dr + ds02)
         w16(0,3) = (dr - ds03)/(dr + ds03)

         w16(1,0) = (dr - ds10)/(dr + ds10)
         w16(1,1) = (dr - ds11)/(dr + ds11)
         w16(1,2) = (dr - ds12)/(dr + ds12)
         w16(1,3) = (dr - ds13)/(dr + ds13)

         w16(2,0) = (dr - ds20)/(dr + ds20)
         w16(2,1) = (dr - ds21)/(dr + ds21)
         w16(2,2) = (dr - ds22)/(dr + ds22)
         w16(2,3) = (dr - ds23)/(dr + ds23)
  
         w16(3,0) = (dr - ds30)/(dr + ds30)
         w16(3,1) = (dr - ds31)/(dr + ds31)
         w16(3,2) = (dr - ds32)/(dr + ds32)
         w16(3,3) = (dr - ds33)/(dr + ds33)

         wsum16 = zero
         do jj = 0, 3
           iya = iy + jj
           if ( iya == nlon_a + 1 ) iya = 1
           if ( iya == nlon_a + 2 ) iya = 2
           if ( iya == nlon_a + 3 ) iya = 3
           do ii = 0, 3
             ixa = min(nlat_a,ix + ii)
             if ( isli_a(ixa,iya) == sfctyp_b ) then
             wsum16  = wsum16  + w16(ii,jj)
             bout  = bout  + w16(ii,jj)*a(ixa,iya)
           endif
          enddo
         enddo

         if ( wsum16 > zero ) then
           bout = bout/wsum16
         else
           nfinal = nfinal + 1
         endif

       endif                  ! if ( wsum4 > zero )

       b(i,j)=bout

     endif                ! if ( isli_b(i,j) == sfctyp_b ) then
   enddo                    ! do i = 1, nlat_b
 enddo                      ! do j = 1, nlon_b


 if ( nwsum > 0 ) then
    write(*,'(a,I4,I3,I5)') 'int22_msk_glb: Number of grids without specified adjacent surface type, istyp,nwsum ; ',istyp,nwsum
 endif
 if ( nfinal > 0 ) then
    write(*,'(a,I4,I3,I5)') 'int22_msk_glb: Number of grids without interpolted value, istyp,nfinal ; ',istyp,nfinal
 endif

 end subroutine int22_msk_glb

 subroutine int21_msk_sub(a,isli,x,dlat,dlon,istyp,nwsum,nfinal,mype)
                        
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    intrp21_msk_sub ---(from 2-d array to obs. location)
!
! prgrmmr:     li -  initial version; org: np2. 03/01/2014
!
! abstract :      This routine interpolates a (2-d array of a subdomain) to a single point with surface mask accounted
! notes    :      (1) Here is a 2-d to one point interpolation
!                 (2) The mask is availabe for both 2-d array and the single point
!
! program history log:
!
!  input argument list:
!    a      - real: 2-d array such as analysis increment at analysis grids of a subdomain
!    isli   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = sea ice) for grid of a
!    dlat   - grid relative latitude (obs location) 
!    dlon   - grid relative longitude (obs location)
!    istyp  - integer: surface type of point x
!    mype   - mpi task id
!    output argument list:
!    x       - real: a variable (same type of a) at a single point 
!$$$ end documentation block
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two
 use gridmod, only: istart,jstart,nlon,nlat,lon1,lon2,lat1,lat2

 implicit none

! INPUT:
 real   (r_kind), dimension(lat2,lon2), intent(in   ) :: a
 integer(i_kind), dimension(lat2,lon2), intent(in   ) :: isli

 real   (r_kind), intent(in   ) :: dlat,dlon
 integer(i_kind), intent(in   ) :: istyp,mype
 integer(i_kind), intent(inout) :: nwsum,nfinal

!OUTPUT:
 real   (r_kind), intent(  out) :: x

!Declare local variables
 integer(i_kind) :: ix1,iy1,ix,iy,ii,jj,ixa,iya,mm1

 real(r_kind)    :: dx0,dx1,dx2,dx3,dy0,dy1,dy2,dy3,dx,dy,dr
 real(r_kind)    :: dx0s,dx1s,dx2s,dx3s,dy0s,dy1s,dy2s,dy3s
 real(r_kind)    :: ds00,ds01,ds02,ds03,ds10,ds11,ds12,ds13,ds20,ds21,ds22,ds23,ds30,ds31,ds32,ds33
 real(r_kind)    :: bavg,bout,wsum4,wsum16
 real(r_kind), dimension(0:1,0:1) :: w4
 real(r_kind), dimension(0:3,0:3) :: w16

 mm1 = mype + 1
 dr = 8.0_r_kind    ! square of the search radius for 16-point cressman-type analysis

 x=zero
!
! to get interpolated value of x with a (array) and mask info
!
 iy1 = int(dlon) 
 dy = dlon-iy1; dy1 = one-dy
 iy =iy1 - jstart(mm1) + 2

 if ( iy < 1 ) then
    iy1 = iy1 + nlon
    iy = iy1 - jstart(mm1) + 2
 endif
 if ( iy > lon1 + 1 ) then
    iy1 = iy1 - nlon
    iy  = iy1 - jstart(mm1) + 2
 endif

 ix1 = int(dlat)
 ix1 = max(1,min(ix1,nlat)) 
 dx  = dlat-ix1; dx1 = one-dx

 ix = ix1 - istart(mm1) + 2

 w4(0,0) = dx1*dy1; w4(0,1) = dx1*dy; w4(1,0) = dx*dy1; w4(1,1) = dx*dy
!
! get the interpolated value with the nearby 4-grids (in a) which has
! the identical surface mask (istyp for x) only
!

 wsum4 = zero
 bavg  = zero
 bout  = zero
 do jj = 0, 1
   iya = min(lon2,iy+jj)
   do ii = 0, 1
     ixa = min(lat2,ix+ii)
     bavg  = bavg + w4(ii,jj)*a(ixa,iya)
     if ( isli(ixa,iya) == istyp ) then
       wsum4 = wsum4 + w4(ii,jj)
       bout  = bout  + w4(ii,jj)*a(ixa,iya)
     endif
   enddo
 enddo

 if ( wsum4 > zero ) then
   bout = bout/wsum4
 else

   nwsum = nwsum + 1

!  use more candidates from a (extending one more grid futher in both x and y
!  direction, which means 16 grids from a will be used) when no same
!  surface type candidates can be found in the nearby 4 grids
!  to perform a Cressman_type Analysis

   ix = ix -1; if(ix == 0) ix = 1
   iy = iy -1; if(iy == 0) iy = 1

   dx0 = dx + one; dx1 = dx; dx2 = one - dx; dx3 = two - dx
   dy0 = dy + one; dy1 = dy; dy2 = one - dy; dy3 = two - dy

   dx0s = dx0*dx0; dx1s = dx1*dx1; dx2s = dx2*dx2; dx3s = dx3*dx3
   dy0s = dy0*dy0; dy1s = dy1*dy1; dy2s = dy2*dy2; dy3s = dy3*dy3

   ds00 = dx0s + dy0s; ds01 = dx0s + dy1s; ds02 = dx0s + dy2s; ds03 = dx0s + dy3s
   ds10 = dx1s + dy0s; ds11 = dx1s + dy1s; ds12 = dx1s + dy2s; ds13 = dx1s + dy3s
   ds20 = dx2s + dy0s; ds21 = dx2s + dy1s; ds22 = dx2s + dy2s; ds23 = dx2s + dy3s
   ds30 = dx3s + dy0s; ds31 = dx3s + dy1s; ds32 = dx3s + dy2s; ds33 = dx3s + dy3s

   w16(0,0) = (dr - ds00)/(dr + ds00)
   w16(0,1) = (dr - ds01)/(dr + ds01)
   w16(0,2) = (dr - ds02)/(dr + ds02)
   w16(0,3) = (dr - ds03)/(dr + ds03)

   w16(1,0) = (dr - ds10)/(dr + ds10)
   w16(1,1) = (dr - ds11)/(dr + ds11)
   w16(1,2) = (dr - ds12)/(dr + ds12)
   w16(1,3) = (dr - ds13)/(dr + ds13)

   w16(2,0) = (dr - ds20)/(dr + ds20)
   w16(2,1) = (dr - ds21)/(dr + ds21)
   w16(2,2) = (dr - ds22)/(dr + ds22)
   w16(2,3) = (dr - ds23)/(dr + ds23)
  
   w16(3,0) = (dr - ds30)/(dr + ds30)
   w16(3,1) = (dr - ds31)/(dr + ds31)
   w16(3,2) = (dr - ds32)/(dr + ds32)
   w16(3,3) = (dr - ds33)/(dr + ds33)

   wsum16 = zero
   do jj = 0, 3
     iya = min(lon2,iy+jj)
     do ii = 0, 3
       ixa = min(lat2,ix + ii)
       if ( isli(ixa,iya) == istyp ) then
       wsum16  = wsum16  + w16(ii,jj)
       bout  = bout  + w16(ii,jj)*a(ixa,iya)
     endif
    enddo
   enddo

   if ( wsum16 > zero ) then
     bout = bout/wsum16
   else
     nfinal = nfinal + 1
   endif

 endif                  ! if ( wsum4 > zero )

 x=bout

! if ( nwsum > 0 ) then
!    write(*,'(a,I4,I3,I5)') 'int21_msk_sub: Number of grids without specified adjacent surface type, mype,istyp,nwsum ; ', mype,istyp,nwsum
! endif
! if ( nfinal > 0 ) then
!    write(*,'(a,I4,I3,I5)') 'int21_msk_sub: Number of grids without interpolted value, mype,istyp,nfinal ; ', mype,istyp,nfinal
! endif

 end subroutine int21_msk_sub

 subroutine int2_msk_glb_prep(a,isli_a,b,isli_b,nx,ny,sfctyp_b,nprep)
!$$$  subprogram documentation block
!                .      .    .
! subroutine:    int2_msk_glb_prep --- (global 2-d array)
!                for a specified surface type (sfctyp_b), expanding the area (grids) with this
!                surface type using the surounded 8 grids with the same surface type
!
!  prgrmmr:     li -  initial version; org: np2
!
!  input argument list:
!    a        - real: 2-d array
!    isli_a   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = seaice) for a grids
!    nx       - integer: number of grids in x-direction
!    ny       - integer: number of grids in y-direction
!    sfctyp_b - integer: the targeted surface type (0=water, 1=land, 2-sea ice)
!    nprep    - integer: number of times to do the extension

!
!  output argument list:
!    b        - real: 2-d array such as analysis increment at surface grids
!    isli_b   - integer: 2-d array such as analysis surface mask
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nx,ny), intent(in   ) :: a
 integer(i_kind), dimension(nx,ny), intent(in   ) :: isli_a
 integer(i_kind),                   intent(in   ) :: nx,ny,sfctyp_b,nprep

!OUTPUT:
 real   (r_kind), dimension(nx,ny), intent(  out) :: b
 integer(i_kind), dimension(nx,ny), intent(  out) :: isli_b

!Declare local variables
 integer(i_kind) :: i,j,k,ii,jj,ix,iy,n
 real(r_kind)    :: bout
 real(r_kind),    dimension(nx,ny) :: wb
 integer(r_kind), dimension(nx,ny) :: wmsk

!
! Initialize b and isli_b
!
  b = a; isli_b = isli_a

  do k = 1, nprep
!
! Initialize/update work array for the value and mask
!
    wb = b; wmsk  = isli_b
!
!   Loop over all grids of array b to update the grids nearby the sfctyp_b surface type
!
    do j = 1, ny
      do i = 1, nx
        if ( wmsk(i,j) /= sfctyp_b ) then
          n = 0
          bout = zero
          do jj = j - 1, j + 1
            do ii = i - 1, i + 1
              iy = jj; if (iy == ny + 1) iy = 1; if (iy == 0) iy = ny
              ix = min(max(ii,1),nx)
              if ( wmsk(ix,iy) == sfctyp_b ) then
                n = n + 1
                bout = bout + wb(ix,iy)
              endif
            enddo
          enddo
          if ( n > 0 ) then
            b(i,j)      = bout/real(n)
            isli_b(i,j) = sfctyp_b
          endif
        endif
      enddo
    enddo
  enddo                  ! do k = 1, nprep

 end subroutine int2_msk_glb_prep

 subroutine int2_msk_sub_prep(a,isli_a,b,isli_b,nx,ny,sfctyp_b,nprep)
!$$$  subprogram documentation block
!                .      .    .
! subroutine:    int2_msk_sub_prep --- (subdomain 2-d array)
!                for a specified surface type (sfctyp_b), expanding the area (grids) with this
!                surface type using the surounded 8 grids with the same surface type
!
!  prgrmmr:     li -  initial version; org: np2
!
!  input argument list:
!    a        - real: 2-d array
!    isli_a   - integer: 2-d array: surface mask (0 = water, 1 = land, 2 = seaice) for a grids
!    nx       - integer: number of grids in x-direction
!    ny       - integer: number of grids in y-direction
!    sfctyp_b - integer: the targeted surface type (0=water, 1=land, 2-sea ice)
!    nprep    - integer: number of times to do the extension

!
!  output argument list:
!    b        - real: 2-d array such as analysis increment at surface grids
!    isli_b   - integer: 2-d array such as analysis surface mask
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
! USES:
 use kinds, only: r_kind,i_kind,r_single
 use constants, only: zero,one,two

 implicit none

! INPUT:
 real   (r_kind), dimension(nx,ny), intent(in   ) :: a
 integer(i_kind), dimension(nx,ny), intent(in   ) :: isli_a
 integer(i_kind),                   intent(in   ) :: nx,ny,sfctyp_b,nprep

!OUTPUT:
 real   (r_kind), dimension(nx,ny), intent(  out) :: b
 integer(i_kind), dimension(nx,ny), intent(  out) :: isli_b

!Declare local variables
 integer(i_kind) :: i,j,k,ii,jj,ix,iy,n
 real(r_kind)    :: bout
 real(r_kind),    dimension(nx,ny) :: wb
 integer(r_kind), dimension(nx,ny) :: wmsk

!
! Initialize b and isli_b
!
  b = a; isli_b = isli_a

  do k = 1, nprep
!
! Initialize/update work array for the value and mask
!
    wb = b; wmsk  = isli_b
!
!   Loop over all grids of array b to update the grids nearby the sfctyp_b surface type
!
    do j = 1, ny
      do i = 1, nx
        if ( wmsk(i,j) /= sfctyp_b ) then
          n = 0
          bout = zero
          do jj = j - 1, j + 1
            do ii = i - 1, i + 1
              iy = min(max(jj,1),ny)
              ix = min(max(ii,1),nx)
              if ( wmsk(ix,iy) == sfctyp_b ) then
                n = n + 1
                bout = bout + wb(ix,iy)
              endif
            enddo
          enddo
          if ( n > 0 ) then
            b(i,j)      = bout/real(n)
            isli_b(i,j) = sfctyp_b
          endif
        endif
      enddo
    enddo
  enddo                  ! do k = 1, nprep

 end subroutine int2_msk_sub_prep

!*******************************************************************************************
subroutine dtzm_point(xt,xz,dt_cool,zc,z1,z2,dtzm)
! ===================================================================== !
!                                                                       !
!  description:  get dtzm = mean of dT(z) (z1 - z2) with NSST dT(z)     !
!                dT(z) = (1-z/xz)*dt_warm - (1-z/zc)*dt_cool            !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call dtzm_point                                                    !
!                                                                       !
!       inputs:                                                         !
!          (xt,xz,dt_cool,zc,z1,z2,                                     !
!       outputs:                                                        !
!          dtzm)                                                        !
!                                                                       !
!  program history log:                                                 !
!                                                                       !
!         2015  -- xu li       createad original code                   !
!  inputs:                                                              !
!     xt      - real, heat content in dtl                            1  !
!     xz      - real, dtl thickness                                  1  !
!     dt_cool - real, sub-layer cooling amount                       1  !
!     zc      - sub-layer cooling thickness                          1  !
!     z1      - lower bound of depth of sea temperature              1  !
!     z2      - upper bound of depth of sea temperature              1  !
!  outputs:                                                             !
!     dtzm   - mean of dT(z)  (z1 to z2)                             1  !
!
  use kinds, only: r_single, i_kind
  use constants, only: zero,one,half

  implicit none

  real (kind=r_single), intent(in)  :: xt,xz,dt_cool,zc,z1,z2
  real (kind=r_single), intent(out) :: dtzm
! Local variables
  real (kind=r_single) :: dt_warm,dtw,dtc

!
! get the mean warming in the range of z=z1 to z=z2
!
  dtw = zero
  if ( xt > zero ) then
    dt_warm = (xt+xt)/xz      ! Tw(0)
    if ( z1 < z2) then
      if ( z2 < xz ) then
        dtw = dt_warm*(one-(z1+z2)/(xz+xz))
      elseif ( z1 < xz .and. z2 >= xz ) then
        dtw = half*(one-z1/xz)*dt_warm*(xz-z1)/(z2-z1)
      endif
    elseif ( z1 == z2 ) then
      if ( z1 < xz ) then
        dtw = dt_warm*(one-z1/xz)
      endif
    endif
  endif
!
! get the mean cooling in the range of z=z1 to z=z2
!
  dtc = zero
  if ( zc > zero ) then
    if ( z1 < z2) then
      if ( z2 < zc ) then
        dtc = dt_cool*(one-(z1+z2)/(zc+zc))
      elseif ( z1 < zc .and. z2 >= zc ) then
        dtc = half*(one-z1/zc)*dt_cool*(zc-z1)/(z2-z1)
      endif
    elseif ( z1 == z2 ) then
      if ( z1 < zc ) then
        dtc = dt_cool*(one-z1/zc)
      endif
    endif
  endif

!
! get the mean T departure from Tf in the range of z=z1 to z=z2
!
  dtzm = dtw - dtc

end subroutine dtzm_point
!*******************************************************************************************

!*******************************************************************************************
subroutine dtzm_2d(xt,xz,dt_cool,zc,slmsk,z1,z2,nx,ny,dtzm)
! ===================================================================== !
!                                                                       !
!  description:  get dtzm = mean of dT(z) (z1 - z2) with NSST dT(z)     !
!                dT(z) = (1-z/xz)*dt_warm - (1-z/zc)*dt_cool            !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call dtzm_2d                                                       !
!                                                                       !
!       inputs:                                                         !
!          (xt,xz,dt_cool,zc,z1,z2,                                     !
!       outputs:                                                        !
!          dtzm)                                                        !
!                                                                       !
!  program history log:                                                 !
!                                                                       !
!         2015  -- xu li       createad original code                   !
!  inputs:                                                              !
!     xt      - real, heat content in dtl                               !
!     xz      - real, dtl thickness                                     !
!     dt_cool - real, sub-layer cooling amount                          !
!     zc      - sub-layer cooling thickness                             !
!     nx      - integer, dimension in x-direction (zonal)               !
!     ny      - integer, dimension in y-direction (meridional)          !
!     z1      - lower bound of depth of sea temperature                 !
!     z2      - upper bound of depth of sea temperature                 !
!  outputs:                                                             !
!     dtzm   - mean of dT(z)  (z1 to z2)                                !
!
  use kinds, only: r_single, i_kind
  use constants, only: zero,one,half

  implicit none

  real(kind=r_single), dimension(nx,ny), intent(in)  :: xt,xz,dt_cool,zc,slmsk
  real(kind=r_single), intent(in) :: z1,z2
  integer(kind=i_kind), intent(in) :: nx,ny
  real(kind=r_single), dimension(nx,ny), intent(out) :: dtzm                    
! Local variables
  integer(kind=i_kind) :: i,j
  real(kind=r_single), dimension(nx,ny) :: dtw,dtc
  real(kind=r_single) :: dt_warm

!
! initialize dtw & dtc as zeros
!
  dtw(:,:) = zero      
  dtc(:,:) = zero      

  do j = 1, ny
    do i= 1, nx

      if ( slmsk(i,j) == zero ) then
!
!       get the mean warming in the range of z=z1 to z=z2
!
        if ( xt(i,j) > zero ) then
          dt_warm = (xt(i,j)+xt(i,j))/xz(i,j)      ! Tw(0)
          if ( z1 < z2) then
            if ( z2 < xz(i,j) ) then
              dtw(i,j) = dt_warm*(one-(z1+z2)/(xz(i,j)+xz(i,j)))
              elseif ( z1 < xz(i,j) .and. z2 >= xz(i,j) ) then
            dtw(i,j) = half*(one-z1/xz(i,j))*dt_warm*(xz(i,j)-z1)/(z2-z1)
            endif
          elseif ( z1 == z2 ) then
            if ( z1 < xz(i,j) ) then
              dtw(i,j) = dt_warm*(one-z1/xz(i,j))
            endif
          endif
        endif
!
!       get the mean cooling in the range of z=0 to z=zsea
!
        if ( zc(i,j) > zero ) then
          if ( z1 < z2) then
            if ( z2 < zc(i,j) ) then
              dtc(i,j) = dt_cool(i,j)*(one-(z1+z2)/(zc(i,j)+zc(i,j)))
            elseif ( z1 < zc(i,j) .and. z2 >= zc(i,j) ) then
              dtc(i,j) = half*(one-z1/zc(i,j))*dt_cool(i,j)*(zc(i,j)-z1)/(z2-z1)
            endif
          elseif ( z1 == z2 ) then
            if ( z1 < zc(i,j) ) then
              dtc(i,j) = dt_cool(i,j)*(one-z1/zc(i,j))
            endif
          endif
        endif
      endif        ! if ( slmsk(i,j) == zero ) then
    enddo
  enddo 
!
! get the mean T departure from Tf in the range of z=z1 to z=z2
!
  where ( slmsk(:,:) == zero )
    dtzm(:,:) = dtw(:,:) - dtc(:,:)
  end where

end subroutine dtzm_2d

