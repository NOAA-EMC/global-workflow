!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module gtg_filter
  use ctlblk_mod, only: SPVAL
  use ctlblk_mod, only: jsta,jend,jsta_2l, jend_2u, jsta_m, jend_m, &
       jsta_m2, jend_m2,im,jm,lm, modelname,global
  use gtg_config, only : SMALL1

  implicit none

  integer, parameter :: nmax = 100

  interface quick_sort !
     module procedure quick_sort_real, quick_sort_int
  end interface

contains

!-----------------------------------------------------------------------
  subroutine filt3d(kmin,kmax,nftxy,nftz,filttype,u)

!$$$ SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Applies filter to input array u and overwrites u with
!    filtered um on output.  um is a work array.
!    filttype=1 specifies 1-2-1 smoothing in each direction (default)
!    filttype=2 specifies median filter
!$$$

    implicit none

    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: nftxy,nftz
    integer, intent(in) :: filttype
    real, intent(inout) :: u(im,jsta_2l:jend_2u,lm)

    real :: um(im,jsta_2l:jend_2u,lm) ! a work array
    
!   --- filttype=1 specifies 1-2-1 smoothing in each direction (default)
!   --- filttype=2 specifies median filter
    if(filttype == 2) then
       call medianFilter3D(kmin,kmax,u,um)
    else
       call meanFilter3D(kmin,kmax,nftxy,nftz,u,um)
    endif

    return
  end subroutine filt3d

!-----------------------------------------------------------------------
  subroutine meanFilter3D(kmin,kmax,nftxy,nftz,u,um)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
! ABSTRACT: Applies 2d 1-2-1 smoother nftxy times in x,y at each vertical
!   level and nftz times in z at each (i,j) column.  Equivalent 
!   to a simple 27-point filter if nftxy=nftz=1.
!   E.g., Haltiner and Williams (1980) eqs. (11-107,11-108).
!$$$

    implicit none
    integer, intent(in) :: kmin,kmax
    integer, intent(in) :: nftxy,nftz
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: u,um

    integer :: i,j,k,ift
    integer :: im1,ip1,jm1,jp1

!   --- xy filter loop
    loop_nftxy: do ift = 1,nftxy
       do k=kmin,kmax

          ! Smooth for each xy filter loop
          call exch(u(1,jsta_2l,k))

!         --- Initialize dummy um
          do j = jsta, jend
          do i = 1,IM
             um(i,j,k)=u(i,j,k)
          end do
          end do

!         --- 1-2-1 smoothing in y direction first 
          do j = jsta_m, jend_m ! No smoothing on j=1 or j=JM point
             jm1=j-1
             jp1=j+1
             do i=1,IM
!               --- Don't include uncomputed u(i,j)
                if(.not.(ABS(u(i,jp1,k)-SPVAL)<SMALL1 .or. &
                         ABS(u(i,j  ,k)-SPVAL)<SMALL1 .or. &
                         ABS(u(i,jm1,k)-SPVAL)<SMALL1)) then
                   um(i,j,k)=0.25*(u(i,jp1,k)+2.*u(i,j,k)+u(i,jm1,k))
                end if
             enddo
          enddo  ! j loop

!         --- Smooth in x direction. Smooth first and last points only if cyclic.
          do i=1,IM

             im1=i-1
             ip1=i+1
             if(im1<1) then
                if(modelname == 'GFS' .or. global) then
                   im1=im1+IM
                else
                   im1=1
                end if
             end if
             if(ip1>IM) then
                if(modelname == 'GFS' .or. global) then
                   ip1=ip1-IM
                else
                   ip1=IM
                end if
             endif

             do j=jsta, jend
!               --- Don't include uncomputed u(i,j)
                if(.not. (ABS(um(im1,j,k)-SPVAL)<SMALL1 .or. &
                          ABS(um(i,  j,k)-SPVAL)<SMALL1 .or. &
                          ABS(um(ip1,j,k)-SPVAL)<SMALL1)) then
                   u(i,j,k) = 0.25*(um(ip1,j,k) +2.*um(i,j,k) + um(im1,j,k))
                endif
             enddo  ! j loop
          end do ! i loop

       enddo  ! k loop
    end do loop_nftxy

!   --- z filter loop
    loop_nftz: do ift = 1,nftz

       do j=jsta,jend
       do i=1,im

!         --- Initialize dummy um
          do k=kmin,kmax
             um(i,j,k)=u(i,j,k)
          enddo

!         --- Smooth in z direction.
          do k=kmin+1,kmax-1 ! Don't include boundary points
!            --- Don't include uncomputed u(i,j)
             if(.not. (ABS(um(i,j,k-1)-SPVAL)<SMALL1 .or. &
                       ABS(um(i,j,k)  -SPVAL)<SMALL1 .or. &
                       ABS(um(i,j,k+1)-SPVAL)<SMALL1)) then
                u(i,j,k)=0.25*(um(i,j,k+1) + 2.*um(i,j,k) + um(i,j,k-1))
             endif
          enddo  ! k loop

       enddo  ! i loop
       enddo  ! j loop
    end do loop_nftz

    return
  end subroutine meanFilter3D

!-----------------------------------------------------------------------
  subroutine filt2d(nftxy,filttype,u)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: Applies filter to input 2d array u and overwrites u with
!   filtered um on output.  um is a work array.
!   filttype=1 specifies 1-2-1 smoothing in each direction (default)
!   filttype=2 specifies median filter
!$$$

    implicit none
    integer, intent(in) :: nftxy
    integer, intent(in) :: filttype
    real, intent(inout) :: u(im,jsta_2l:jend_2u)

    real :: um(im,jsta_2l:jend_2u) ! a work array

!   --- filttype=1 specifies 1-2-1 smoothing in each direction (default)
!   --- filttype=2 specifies median filter
    if(filttype==2) then
       call medianFilter2D(u,um)
    else
       call meanFilter2D(nftxy,u,um)
    endif

    return
  end subroutine filt2d

!-----------------------------------------------------------------------
  subroutine meanFilter2D(nftxy,u,um)

!$$$ SUBPROGRAM DOCUMENTATION BLOCK 
! ABSTRACT: uses a simple 3 point smoothing function on on 2-D arrays
!     --- Note smoothing in x followed by a smoothing in y is equivalent
!     --- to a 9-pt filter.
!     --- E.g., Haltiner and Williams (1980) eqs. (11-107,11-108).
!$$$

    implicit none

    integer, intent(in) :: nftxy
    real, dimension(IM,jsta_2l:jend_2u), intent(inout) :: u, um

    integer :: i,j,ift
    integer :: im1,ip1,jm1,jp1


    do ift = 1,nftxy    ! xy filter loop

       ! Smooth for each xy filter loop
       call exch(u(1,jsta_2l))

!      --- Initialize dummy um
       do j = jsta, jend
       do i = 1,IM
          um(i,j)=u(i,j)
       end do
       end do

!      --- 1-2-1 smoothing in y direction first
       do j = jsta_m, jend_m ! No smoothing on j=1 or j=JM point
          jm1=j-1
          jp1=j+1
          do i=1,IM
!            --- Don't include uncomputed u(i,j)
             if(.not. (ABS(u(i,jp1)-SPVAL)<SMALL1 .or. &
                       ABS(u(i,j  )-SPVAL)<SMALL1 .or. &
                       ABS(u(i,jm1)-SPVAL)<SMALL1)) then
                um(i,j)=0.25*(u(i,jp1)+2.*u(i,j)+u(i,jm1))
             endif
          enddo
       enddo  ! j loop

!      --- Smooth in x direction. Smooth first and last points only if cyclic.
       do i=1,IM

          im1=i-1
          ip1=i+1
          if(im1<1) then
             if(modelname == 'GFS' .or. global) then
                im1=im1+IM
             else
                im1=1
             end if
          end if
          if(ip1>IM) then
             if(modelname == 'GFS' .or. global) then
                ip1=ip1-IM
             else
                ip1=IM
             end if
          endif

          do j=jsta, jend
!            --- Don't include uncomputed u(i,j)
             if(.not. (ABS(um(im1,j)-SPVAL)<SMALL1 .or. &
                       ABS(um(i,  j)-SPVAL)<SMALL1 .or. &
                       ABS(um(ip1,j)-SPVAL)<SMALL1)) then
                u(i,j) = 0.25*(um(ip1,j) +2.*um(i,j) + um(im1,j))
             endif
          enddo ! j loop
       enddo    ! i loop

    enddo  ! nftxy loop

    return
  end subroutine meanFilter2D

!-----------------------------------------------------------------------
  subroutine medianFilter3D(kmin,kmax,u,um)
! --- Applies median filter to input array u and outputs in array um

    implicit none

    integer, intent(in) :: kmin,kmax
    real, dimension(IM,jsta_2l:jend_2u,LM), intent(inout) :: u,um

    integer, parameter :: grid_extent=1
    
    integer :: i,j,k,ii,iii,jj,kk
    real :: ua(nmax)
    integer :: n,nh
    real :: umedian

    do k=kmin,kmax
       call exch(u(1,jsta_2l,k))
    end do

!   --- Copy u into work array um
    um = u

    do k=kmin,kmax
    do j=jsta,jend
    do i=1,IM

       do n=1,nmax
          ua(n)=SPVAL
       enddo

       n=0
       do kk=k-grid_extent,k+grid_extent
       do jj=j-grid_extent,j+grid_extent
       do iii=i-grid_extent,i+grid_extent

          if ( jj < 1 .or. jj > JM .or. &
               kk < 1 .or. kk > LM ) cycle

          ii=iii
          if(iii<1) then
!            --- cyclic boundary conditions u(0)=u(im), etc.
             if(modelname == 'GFS' .or. global) then
                ii=iii+IM
             else
                cycle
             endif
          endif
          if(iii>IM) then
!            --- cyclic boundary conditions u(im+1)=u(1), etc.
             if(modelname == 'GFS' .or. global) then 
                ii=iii-IM
             else
                cycle
             endif
          endif
          if(.not. (ABS(u(ii,jj,kk)-SPVAL)<SMALL1)) then
             n=n+1
             ua(n)=u(ii,jj,kk)
          end if
       enddo
       enddo
       enddo

       if(n<=1) then
          u(i,j,k)=um(i,j,k)
       else
!         --- Sort ua into ascending order
          call quick_sort(ua,n)
!         --- Capture median
          nh=n/2
          if(2*nh==n) then
             umedian=0.5*(ua(nh)+ua(nh+1))
          else
             umedian=ua(nh+1)
          endif
          u(i,j,k)=umedian
       endif
    enddo
    enddo
    enddo

    return
  end subroutine medianFilter3D


!-----------------------------------------------------------------------
  subroutine medianFilter2D(u,um)
!   --- Applies median filter to input array u and outputs in array um

    implicit none

    real, dimension(IM,jsta_2l:jend_2u), intent(inout) :: u, um

    integer, parameter :: grid_extent=1

    integer :: i,j,ii,iii,jj
    real :: ua(nmax)
    integer :: n,nh
    real  ::  umedian

    call exch(u(1,jsta_2l))

!   --- Copy u into work array um
    um =u

    do j=jsta,jend
    do i=1,IM

       do n=1,nmax
          ua(n)=SPVAL
       enddo

       n=0
       do jj=j-grid_extent,j+grid_extent
       do iii=i-grid_extent,i+grid_extent

          if (jj < 1 .or. jj > JM) cycle

          ii=iii
          if(iii<=0) then
!            --- cyclic boundary conditions u(0)=u(im), etc.
             if(modelname == 'GFS' .or. global) then
                ii=iii+IM
             else
                cycle
             endif
          endif
          if(iii>IM) then
!           --- cyclic boundary conditions u(im+1)=u(1), etc.
             if(modelname == 'GFS' .or. global) then
                ii=iii-IM
             else
                cycle
            endif
          endif
          if(.not. (abs(u(ii,jj)-SPVAL)<SMALL1)) then
             n=n+1
             ua(n)=u(ii,jj)
          end if
       enddo
       enddo

       if(n<=1) then
          u(i,j)=um(i,j)
       else
!         --- Sort ua into ascending order
          call quick_sort(ua,n)
!         --- Capture median
          nh=n/2
          if(2*nh==n) then
             umedian=0.5*(ua(nh)+ua(nh+1))
          else
             umedian=ua(nh+1)
          endif
          u(i,j)=umedian
       endif
    enddo
    enddo

    return
  end subroutine medianFilter2D


!-----------------------------------------------------------------------
  subroutine fillybdys3d(kmin,kmax,f)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!     --- fill in y boundary values of 3d variable f at j=1,2 and
!     --- j=ny-1,ny by simple extrapolation.  For global model
!     --- sets pole points (j=1 and j=ny) equal to the average of f at
!     --- the grid points closest to the poles.
!$$$
    implicit none

    integer, intent(in) :: kmin,kmax
    real, intent(inout) :: f(IM,jsta_2l:jend_2u,LM)

    integer :: i,k,ni
    real :: favgsp,favgnp

    if(jsta>=3 .and. jend<=jm-2) return

!   --- Pick up j=3,4 points by 2-pt extrapolation in order to
!   --- avoid problems with undefined map scale factors at poles of
!   --- global model
    if(jsta<=2) then
       do i=1,IM
       do k=kmin,kmax
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(f(i,3,k)-SPVAL) < SMALL1 .or. &
             ABS(f(i,4,k)-SPVAL) < SMALL1) cycle
          f(i,2,k) =2.*f(i,3,k)-f(i,4,k)
       enddo  ! i loop
       enddo  ! k loop
    endif
    if(jsta==1) then
       if(modelname == 'GFS' .or. global) then
!         --- Fill in pole points for spherical model
          do k=kmin,kmax
             favgsp=0.
             ni=0
             do i=1,im
                ni=ni+1
                favgsp=favgsp+f(i,2,k)
             enddo
             favgsp=favgsp/ni
             do i=1,IM
                f(i,1,k)=favgsp
             enddo
          enddo
       else
!         --- fill in j=1 boundary values by extrapolation
          do i=1,IM
          do k=kmin,kmax
             f(i,1,k)=f(i,2,k)
          enddo  ! k loop
          enddo  ! i loop
       endif
    endif

!     --- Pick up j=ny-3,ny-2 points by 2-pt extrapolation in order to
!     --- avoid problems with undefined map scale factors at poles of
!     --- global model
    if(jend >= jm-1) then
       do i=1,IM
       do k=kmin,kmax
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(f(i,jm-2,k)-SPVAL) < SMALL1 .or. &
             ABS(f(i,jm-3,k)-SPVAL) < SMALL1) cycle
          f(i,jm-1,k)=2.*f(i,jm-2,k)-f(i,jm-3,k)
       enddo  ! i loop
       enddo  ! k loop
    endif

!   --- fill in j=ny boundary values by extrapolation
    if(jend==jm) then
       if(modelname == 'GFS' .or. global) then
!         --- Fill in pole points for spherical model
          do k=kmin,kmax
             favgnp=0.
             ni=0
             do i=1,IM
                ni=ni+1
                favgnp=favgnp+f(i,jm-1,k)
             enddo
             favgnp=favgnp/ni
             do i=1,IM
                f(i,jm,k)=favgnp
             enddo
          enddo
       else
!         --- fill in y boundary values by extrapolation
          do i=1,IM
          do k=kmin,kmax
             f(i,jm,k)=f(i,jm-1,k)
          enddo  ! k loop
          enddo  ! i loop
       endif
    endif

    return
  end subroutine fillybdys3d

!-----------------------------------------------------------------------
  subroutine fillybdys2d(f)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!     --- fill in y (NS) boundary values of 2d variable f at j=1,2 and
!     --- j=ny-1,ny by simple extrapolation.  For global model
!     --- sets pole points (j=1 and j=ny) equal to the average of f at
!     --- the grid points closest to the poles.
!$$$
    implicit none

    real, intent(inout) :: f(im,jsta_2l:jend_2u)

    integer :: i,ni
    real    favg,favgnp

    if(jsta>=3 .and. jend<=jm-2) return

!   --- Pick up j=2,3 points by 2-pt extrapolation to avoid problems
!   --- with undefined map scale factors at poles of global model
    if(jsta<=2) then
       do i=1,IM
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(f(i,3)-SPVAL) < SMALL1 .or. &
             ABS(f(i,4)-SPVAL) < SMALL1)  cycle
          f(i,2) =2.*f(i,3)-f(i,4)
       enddo  ! i loop
    endif
    if(jsta==1) then
       if(modelname == 'GFS' .or. global) then
!         --- Fill in pole points for spherical model
          favg=0.
          ni=0
          do i=1,IM
             if(abs(f(i,2)-SPVAL)>SMALL1) then
                ni=ni+1
                favg=favg+f(i,2)
             end if
          enddo
          favg=favg/ni
          do i=1,IM
             f(i,1)=favg
          enddo
       else
!         --- fill in j=1 boundary values by extrapolation
          do i=1,IM
             f(i,1)=f(i,2)
          enddo  ! i loop
       endif
    endif

!   --- Pick up j=ny-2,ny-1 points by 2-pt extrapolation in order to
!   --- avoid problems with undefined map scale factors at poles of
!   --- global model
    if(jend>=jm-1) then
       do i=1,IM
!         --- Don't include uncomputed (i,j,k) or pts below terrain 
          if(ABS(f(i,jm-2)-SPVAL) < SMALL1 .or. &
             ABS(f(i,jm-3)-SPVAL) < SMALL1) cycle
          f(i,jm-1)=2.*f(i,jm-2)-f(i,jm-3)
       enddo  ! i loop
    end if

!   --- fill in j=ny boundary values by extrapolation
    if(jend==jm) then
       if(modelname == 'GFS' .or. global) then
!         --- Fill in pole points for spherical model
          favg=0.
          ni=0
          do i=1,IM
             if(abs(f(i,jm-1)-SPVAL)>SMALL1) then
                ni=ni+1
                favg=favg+f(i,jm-1)
             end if
          enddo
          favg=favg/ni
          do i=1,IM
             f(i,jm)=favg
          enddo
       else
!         --- fill in y boundary values by extrapolation
          do i=1,IM
             f(i,jm)=f(i,jm-1)
          enddo  ! i loop
       endif
    endif

    return
  end subroutine fillybdys2d


!-----------------------------------------------------------------------
! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's
! Guide to Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array
!-----------------------------------------------------------------------
  subroutine quick_sort_int(list, n)
    IMPLICIT NONE
    integer, DIMENSION (:), INTENT(INOUT)  :: list
    INTEGER, intent(in), optional :: n

    ! local float array
    real, allocatable :: rlist(:)
    integer :: len

    if(present(n)) then
       len = n
    else
       len = size(list)
    endif

    allocate(rlist(len))
    rlist = list
    call quick_sort_real(rlist, len)
    list = int(rlist)
    deallocate(rlist)
  end subroutine quick_sort_int

!-----------------------------------------------------------------------
  RECURSIVE SUBROUTINE quick_sort_real(list, n)
    IMPLICIT NONE
    REAL, DIMENSION (:), INTENT(INOUT)  :: list
    INTEGER, intent(in), optional :: n 

    if(present(n)) then
       CALL quick_sort_1(1, n)
    else
       call  quick_sort_1(1, size(list))
    endif

  CONTAINS

!---------------------------------------------------
    RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

      INTEGER, INTENT(IN) :: left_end, right_end
      !     Local variables
      INTEGER             :: i, j
      REAL                :: reference, temp
      INTEGER, PARAMETER  :: max_simple_sort_size = 6
      IF (right_end < left_end + max_simple_sort_size) THEN
         ! Use interchange sort for small lists
         CALL interchange_sort(left_end, right_end)

      ELSE
         ! Use partition ("quick") sort
         reference = list((left_end + right_end)/2)
         i = left_end - 1; j = right_end + 1

         DO
            ! Scan list from left end until element >= reference is found
            DO
               i = i + 1
               IF (list(i) >= reference) EXIT
            END DO
            ! Scan list from right end until element <= reference is found
            DO
               j = j - 1
               IF (list(j) <= reference) EXIT
            END DO


            IF (i < j) THEN
               ! Swap two out-of-order elements
               temp = list(i); list(i) = list(j); list(j) = temp
            ELSE IF (i == j) THEN
               i = i + 1
               EXIT
            ELSE
               EXIT
            END IF
         END DO

         IF (left_end < j) CALL quick_sort_1(left_end, j)
         IF (i < right_end) CALL quick_sort_1(i, right_end)
      END IF

    END SUBROUTINE quick_sort_1

!---------------------------------------------------
    SUBROUTINE interchange_sort(left_end, right_end)
      INTEGER, INTENT(IN) :: left_end, right_end

      !     Local variables
      INTEGER             :: i, j
      REAL                :: temp

      DO i = left_end, right_end - 1
         DO j = i+1, right_end
            IF (list(i) > list(j)) THEN
               temp = list(i); list(i) = list(j); list(j) = temp
            END IF
         END DO
      END DO

    END SUBROUTINE interchange_sort

  END SUBROUTINE quick_sort_real

end module gtg_filter
