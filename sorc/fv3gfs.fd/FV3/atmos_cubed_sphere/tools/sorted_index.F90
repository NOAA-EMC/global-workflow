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
!>@brief The module 'sorted_index' sorts cell corner indices in lat-lon
!! space to ensure the same order of operations regardless of the 
!! orientation in index space.
!>@details  i/jinta are indices of b-grid locations needed for line integrals 
!! around an a-grid cell including ghosting.
!! i/jintb are indices of a-grid locations needed for line integrals
!! around a b-grid cell with no ghosting.
module sorted_index_mod

  use fv_arrays_mod, only: R_GRID

  implicit none
  private
  public :: sorted_inta, sorted_intb

contains

!>@brief The subroutine 'sorted_inta' sorts cell corner indices in latlon space
!! based on grid locations in index space..
!>@details If not the grid is notcubed_sphere, it assumes that
!! the orientations in index  and latlon space are identical.
!! i/jinta are indices of b-grid locations needed for line integrals 
!! around an a-grid cell including ghosting.
!! i/jintb are indices of a-grid locations needed for line integrals
!! around a b-grid cell, no ghosting.
  subroutine sorted_inta(isd, ied, jsd, jed, cubed_sphere, bgrid, iinta, jinta)
    integer, intent(in) :: isd, ied, jsd, jed
    real(kind=R_GRID),    intent(in), dimension(isd:ied+1,jsd:jed+1,2) :: bgrid
    logical, intent(in) :: cubed_sphere

    integer, intent(out), dimension(4,isd:ied,jsd:jed) :: iinta, jinta
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    real,    dimension(4) :: xsort, ysort
    integer, dimension(4) :: isort, jsort
    integer :: i, j
    !------------------------------------------------------------------!
    ! special treatment for cubed sphere                               !
    !------------------------------------------------------------------!
    if (cubed_sphere) then
       !---------------------------------------------------------------!
       ! get order of indices for line integral around a-grid cell     ! 
       !---------------------------------------------------------------!
       do j=jsd,jed
          do i=isd,ied
             xsort(1)=bgrid(i  ,j  ,1); ysort(1)=bgrid(i  ,j  ,2); isort(1)=i  ; jsort(1)=j
             xsort(2)=bgrid(i  ,j+1,1); ysort(2)=bgrid(i  ,j+1,2); isort(2)=i  ; jsort(2)=j+1
             xsort(3)=bgrid(i+1,j+1,1); ysort(3)=bgrid(i+1,j+1,2); isort(3)=i+1; jsort(3)=j+1
             xsort(4)=bgrid(i+1,j  ,1); ysort(4)=bgrid(i+1,j  ,2); isort(4)=i+1; jsort(4)=j
             call sort_rectangle(iinta(1,i,j), jinta(1,i,j))
          enddo
       enddo
    else
       !---------------------------------------------------------------!
       ! default behavior for other grids                              !
       !---------------------------------------------------------------!
       do j=jsd,jed
          do i=isd,ied
             iinta(i,j,1)=i  ; jinta(i,j,1)=j
             iinta(i,j,2)=i  ; jinta(i,j,2)=j+1
             iinta(i,j,3)=i+1; jinta(i,j,3)=j+1
             iinta(i,j,4)=i+1; jinta(i,j,4)=j  
          enddo
       enddo
    endif

  contains
    !------------------------------------------------------------------!
    subroutine sort_rectangle(iind, jind)
      integer, dimension(4), intent(inout) :: iind, jind
      !----------------------------------------------------------------!
      ! local variables                                                !
      !----------------------------------------------------------------!
      real,    dimension(4) :: xsorted, ysorted
      integer, dimension(4) :: isorted, jsorted
      integer :: l, ll, lll
      !----------------------------------------------------------------!
      ! sort in east west                                              !
      !----------------------------------------------------------------!
      xsorted(:)=10.
      ysorted(:)=10.
      isorted(:)=0
      jsorted(:)=0
             
      do l=1,4
         do ll=1,4
            if (xsort(l)<xsorted(ll)) then
               do lll=3,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! sort in north south                                            !
      !----------------------------------------------------------------!
      do l=1,4
         xsort(l)=xsorted(l); ysort(l)=ysorted(l)
         isort(l)=isorted(l); jsort(l)=jsorted(l)
      enddo
      xsorted(:)=10.
      ysorted(:)=10.
      isorted(:)=0
      jsorted(:)=0
      
      do l=1,4
         do ll=1,4
            if (ysort(l)<ysorted(ll)) then
               do lll=3,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! use first two grid point for start and orientation             !
      !----------------------------------------------------------------!
      if ( isorted(1)==i .and. jsorted(1)==j ) then
         if ( isorted(2)==i+1 .and. jsorted(2)==j+1 ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i   .and. jsorted(2)==j+1 ) then
            iind(1)=i  ; jind(1)=j
            iind(2)=i  ; jind(2)=j+1
            iind(3)=i+1; jind(3)=j+1
            iind(4)=i+1; jind(4)=j  
         elseif ( isorted(2)==i+1 .and. jsorted(2)==j ) then
            iind(1)=i  ; jind(1)=j
            iind(2)=i+1; jind(2)=j
            iind(3)=i+1; jind(3)=j+1
            iind(4)=i  ; jind(4)=j+1
         endif
         
      elseif ( isorted(1)==i .and. jsorted(1)==j+1 ) then
         if ( isorted(2)==i+1 .and. jsorted(2)==j ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i+1 .and. jsorted(2)==j+1 ) then
            iind(1)=i  ; jind(1)=j+1
            iind(2)=i+1; jind(2)=j+1
            iind(3)=i+1; jind(3)=j
            iind(4)=i  ; jind(4)=j  
         elseif ( isorted(2)==i   .and. jsorted(2)==j ) then
            iind(1)=i  ; jind(1)=j+1
            iind(2)=i  ; jind(2)=j
            iind(3)=i+1; jind(3)=j
            iind(4)=i+1; jind(4)=j+1
         endif
         
      elseif ( isorted(1)==i+1 .and. jsorted(1)==j+1 ) then
         if ( isorted(2)==i .and. jsorted(2)==j ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i+1 .and. jsorted(2)==j ) then
            iind(1)=i+1; jind(1)=j+1
            iind(2)=i+1; jind(2)=j
            iind(3)=i  ; jind(3)=j
            iind(4)=i  ; jind(4)=j+1  
         elseif ( isorted(2)==i   .and. jsorted(2)==j+1 ) then
            iind(1)=i+1; jind(1)=j+1
            iind(2)=i  ; jind(2)=j+1
            iind(3)=i  ; jind(3)=j
            iind(4)=i+1; jind(4)=j
         endif
         
      elseif ( isorted(1)==i+1 .and. jsorted(1)==j ) then
         if ( isorted(2)==i .and. jsorted(2)==j+1 ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i   .and. jsorted(2)==j ) then
            iind(1)=i+1; jind(1)=j
            iind(2)=i  ; jind(2)=j
            iind(3)=i  ; jind(3)=j+1
            iind(4)=i+1; jind(4)=j+1
         elseif ( isorted(2)==i+1 .and. jsorted(2)==j+1 ) then
            iind(1)=i+1; jind(1)=j
            iind(2)=i+1; jind(2)=j+1
            iind(3)=i  ; jind(3)=j+1
            iind(4)=i  ; jind(4)=j  
         endif
         
      endif

    end subroutine sort_rectangle
    !------------------------------------------------------------------!
  end subroutine sorted_inta

!>@brief The subroutine 'sorted_intb' sorts cell corner indices in latlon space
!!  based on grid locations in index space.
!>@details If not the grid is notcubed_sphere, it assumes that
!! the orientations in index  and latlon space are identical.
!! i/jinta are indices of b-grid locations needed for line integrals 
!! around an a-grid cell including ghosting.
!! i/jintb are indices of a-grid locations needed for line integrals
!! around a b-grid cell, no ghosting.
  subroutine sorted_intb(isd, ied, jsd, jed, is, ie, js, je, npx, npy, &
                          cubed_sphere, agrid, iintb, jintb)
    integer, intent(in) :: isd, ied, jsd, jed, is, ie, js, je, npx, npy
    real(kind=R_GRID),    intent(in), dimension(isd:ied,jsd:jed,2) :: agrid
    logical, intent(in) :: cubed_sphere

    integer, dimension(4,is:ie+1,js:je+1), intent(out) :: iintb, jintb
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    real,    dimension(4) :: xsort, ysort, xsorted, ysorted 
    integer, dimension(4) :: isort, jsort, isorted, jsorted
    integer :: i, j, l, ll, lll
    !------------------------------------------------------------------!
    ! special treatment for cubed sphere                               !
    !------------------------------------------------------------------!
    if (cubed_sphere) then
       !---------------------------------------------------------------!
       ! get order of indices for line integral around b-grid cell     ! 
       !---------------------------------------------------------------!
       do j=js,je+1
          do i=is,ie+1
             xsort(1)=agrid(i  ,j  ,1); ysort(1)=agrid(i  ,j  ,2); isort(1)=i  ; jsort(1)=j
             xsort(2)=agrid(i  ,j-1,1); ysort(2)=agrid(i  ,j-1,2); isort(2)=i  ; jsort(2)=j-1
             xsort(3)=agrid(i-1,j-1,1); ysort(3)=agrid(i-1,j-1,2); isort(3)=i-1; jsort(3)=j-1
             xsort(4)=agrid(i-1,j  ,1); ysort(4)=agrid(i-1,j  ,2); isort(4)=i-1; jsort(4)=j
             call sort_rectangle(iintb(1,i,j), jintb(1,i,j))
          enddo
       enddo
       !---------------------------------------------------------------!
       ! take care of corner points                                    !
       !---------------------------------------------------------------!
       if ( (is==1) .and. (js==1) ) then
          i=1
          j=1
          xsort(1)=agrid(i  ,j  ,1); ysort(1)=agrid(i  ,j  ,2); isort(1)=i  ; jsort(1)=j  
          xsort(2)=agrid(i  ,j-1,1); ysort(2)=agrid(i  ,j-1,2); isort(2)=i  ; jsort(2)=j-1
          xsort(3)=agrid(i-1,j  ,1); ysort(3)=agrid(i-1,j  ,2); isort(3)=i-1; jsort(3)=j
          call sort_triangle()
          iintb(4,i,j)=i-1; jintb(4,i,j)=j-1
       endif

       if ( (ie+1==npx) .and. (js==1) ) then
          i=npx
          j=1
          xsort(1)=agrid(i  ,j  ,1); ysort(1)=agrid(i  ,j  ,2); isort(1)=i  ; jsort(1)=j
          xsort(2)=agrid(i-1,j  ,1); ysort(2)=agrid(i-1,j  ,2); isort(2)=i-1; jsort(2)=j
          xsort(3)=agrid(i-1,j-1,1); ysort(3)=agrid(i-1,j-1,2); isort(3)=i-1; jsort(3)=j-1
          call sort_triangle()
          iintb(4,i,j)=i; jintb(4,i,j)=j-1
       endif

       if ( (ie+1==npx) .and. (je+1==npy) ) then
          i=npx
          j=npy
          xsort(1)=agrid(i-1,j-1,1); ysort(1)=agrid(i-1,j-1,2); isort(1)=i-1; jsort(1)=j-1
          xsort(2)=agrid(i  ,j-1,1); ysort(2)=agrid(i  ,j-1,2); isort(2)=i  ; jsort(2)=j-1
          xsort(3)=agrid(i-1,j  ,1); ysort(3)=agrid(i-1,j  ,2); isort(3)=i-1; jsort(3)=j
          call sort_triangle()
          iintb(4,i,j)=i; jintb(4,i,j)=j
       endif
       
       if ( (is==1) .and. (je+1==npy) ) then
          i=1
          j=npy
          xsort(1)=agrid(i  ,j  ,1); ysort(1)=agrid(i  ,j  ,2); isort(1)=i  ; jsort(1)=j
          xsort(2)=agrid(i-1,j-1,1); ysort(2)=agrid(i-1,j-1,2); isort(2)=i-1; jsort(2)=j-1
          xsort(3)=agrid(i  ,j-1,1); ysort(3)=agrid(i  ,j-1,2); isort(3)=i  ; jsort(3)=j-1
          call sort_triangle()
          iintb(4,i,j)=i-1; jintb(4,i,j)=j
       endif
    else
       !---------------------------------------------------------------!
       ! default behavior for other grids                              !
       !---------------------------------------------------------------!
       do j=js,je+1
          do i=is,ie+1
             iintb(1,i,j)=i  ; jintb(1,i,j)=j
             iintb(2,i,j)=i  ; jintb(2,i,j)=j-1
             iintb(3,i,j)=i-1; jintb(3,i,j)=j-1
             iintb(4,i,j)=i-1; jintb(4,i,j)=j  
          enddo
       enddo
    endif

  contains
    !------------------------------------------------------------------!
    subroutine sort_rectangle(iind, jind)

      integer, dimension(4), intent(inout) :: iind, jind
      !----------------------------------------------------------------!
      ! local variables                                                !
      !----------------------------------------------------------------!
      real,    dimension(4) :: xsorted, ysorted 
      integer, dimension(4) :: isorted, jsorted
      !----------------------------------------------------------------!
      ! sort in east west                                              !
      !----------------------------------------------------------------!
      xsorted(:)=10.
      ysorted(:)=10.
      isorted(:)=0
      jsorted(:)=0
             
      do l=1,4
         do ll=1,4
            if (xsort(l)<xsorted(ll)) then
               do lll=3,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! sort in north south                                            !
      !----------------------------------------------------------------!
      do l=1,4
         xsort(l)=xsorted(l); ysort(l)=ysorted(l)
         isort(l)=isorted(l); jsort(l)=jsorted(l)
      enddo
      xsorted(:)=10.
      ysorted(:)=10.
      isorted(:)=0
      jsorted(:)=0
      
      do l=1,4
         do ll=1,4
            if (ysort(l)<ysorted(ll)) then
               do lll=3,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! use first two grid point for start and orientation             !
      !----------------------------------------------------------------!
      if ( isorted(1)==i .and. jsorted(1)==j ) then
         if ( isorted(2)==i-1 .and. jsorted(2)==j-1 ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i   .and. jsorted(2)==j-1 ) then
            iind(1)=i  ; jind(1)=j
            iind(2)=i  ; jind(2)=j-1
            iind(3)=i-1; jind(3)=j-1
            iind(4)=i-1; jind(4)=j  
         elseif ( isorted(2)==i-1 .and. jsorted(2)==j ) then
            iind(1)=i  ; jind(1)=j
            iind(2)=i-1; jind(2)=j
            iind(3)=i-1; jind(3)=j-1
            iind(4)=i  ; jind(4)=j-1
         endif
         
      elseif ( isorted(1)==i .and. jsorted(1)==j-1 ) then
         if ( isorted(2)==i-1 .and. jsorted(2)==j ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i-1 .and. jsorted(2)==j-1 ) then
            iind(1)=i  ; jind(1)=j-1
            iind(2)=i-1; jind(2)=j-1
            iind(3)=i-1; jind(3)=j
            iind(4)=i  ; jind(4)=j  
         elseif ( isorted(2)==i   .and. jsorted(2)==j ) then
            iind(1)=i  ; jind(1)=j-1
            iind(2)=i  ; jind(2)=j
            iind(3)=i-1; jind(3)=j
            iind(4)=i-1; jind(4)=j-1
         endif
         
      elseif ( isorted(1)==i-1 .and. jsorted(1)==j-1 ) then
         if ( isorted(2)==i .and. jsorted(2)==j ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i-1 .and. jsorted(2)==j ) then
            iind(1)=i-1; jind(1)=j-1
            iind(2)=i-1; jind(2)=j
            iind(3)=i  ; jind(3)=j
            iind(4)=i  ; jind(4)=j-1  
         elseif ( isorted(2)==i   .and. jsorted(2)==j-1 ) then
            iind(1)=i-1; jind(1)=j-1
            iind(2)=i  ; jind(2)=j-1
            iind(3)=i  ; jind(3)=j
            iind(4)=i-1; jind(4)=j
         endif
         
      elseif ( isorted(1)==i-1 .and. jsorted(1)==j ) then
         if ( isorted(2)==i .and. jsorted(2)==j-1 ) then
            isorted(2)=isorted(3); jsorted(2)=jsorted(3)
         endif
         if ( isorted(2)==i   .and. jsorted(2)==j ) then
            iind(1)=i-1; jind(1)=j
            iind(2)=i  ; jind(2)=j
            iind(3)=i  ; jind(3)=j-1
            iind(4)=i-1; jind(4)=j-1
         elseif ( isorted(2)==i-1 .and. jsorted(2)==j-1 ) then
            iind(1)=i-1; jind(1)=j
            iind(2)=i-1; jind(2)=j-1
            iind(3)=i  ; jind(3)=j-1
            iind(4)=i  ; jind(4)=j  
         endif
         
      endif

    end subroutine sort_rectangle
    !------------------------------------------------------------------!
    subroutine sort_triangle()

      xsorted(1:3)=10.
      ysorted(1:3)=10.
      isorted(1:3)=0
      jsorted(1:3)=0
      !----------------------------------------------------------------!
      ! sort in east west                                              !
      !----------------------------------------------------------------!
      do l=1,3
         do ll=1,3
            if (xsort(l)<xsorted(ll)) then
               do lll=2,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! sort in north south                                            !
      !----------------------------------------------------------------!
      do l=1,3
         xsort(l)=xsorted(l); ysort(l)=ysorted(l)
         isort(l)=isorted(l); jsort(l)=jsorted(l)
      enddo
      xsorted(1:3)=10.
      ysorted(1:3)=10.
      isorted(1:3)=0
      jsorted(1:3)=0
      
      do l=1,3
         do ll=1,3
            if (ysort(l)<ysorted(ll)) then
               do lll=2,ll,-1
                  xsorted(lll+1)=xsorted(lll)
                  ysorted(lll+1)=ysorted(lll)
                  isorted(lll+1)=isorted(lll)
                  jsorted(lll+1)=jsorted(lll)
               enddo
               xsorted(ll)=xsort(l)
               ysorted(ll)=ysort(l)
               isorted(ll)=isort(l)
               jsorted(ll)=jsort(l)
               exit
            endif
         enddo
      enddo
      !----------------------------------------------------------------!
      ! use first two grid point for start and orientation             !
      !----------------------------------------------------------------!
      iintb(1,i,j)=isorted(1) ; jintb(1,i,j)=jsorted(1)
      iintb(2,i,j)=isorted(2) ; jintb(2,i,j)=jsorted(2)
      iintb(3,i,j)=isorted(3) ; jintb(3,i,j)=jsorted(3)
   
    end subroutine sort_triangle
    !------------------------------------------------------------------!
  end subroutine sorted_intb

end module sorted_index_mod
