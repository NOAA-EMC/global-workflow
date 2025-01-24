subroutine unhalf_nmm_grid2(gout,nx,ny,gin,igtype,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unhalf_nmm_grid2            reverse of half_nmm_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: interpolates analysis increment from unstaggered A grid 
!           and adds to preexisting contents of staggered E grid.  
!           See comments in half_nmm_grid2.f90 for additional 
!           information.
!
! program history log:
!   2004-06-22  parrish, document
!   2006-03-28  wu,parrish, extrapolate full analysis result to points on eastern
!                          edge of E grid not used by model to eliminate problems
!                           with graphic output of model fields.
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     gout     - input unstaggered half grid  (reorganized for 
!                  distibution to local domains)
!     gin      - preexisting input values on staggered E grid
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid
!                  (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, 
!                  see illustration below)
!     iorder
!
!   output argument list
!     gin      - output on staggered E grid (contains input values + 
!                  interpolated analysis increment)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use gridmod, only: iglobal,itotsub
  use general_commvars_mod, only: ltosi,ltosj,ltosj_s,ltosi_s
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nx,ny,igtype,iorder
  real(r_single),dimension(itotsub),intent(in   ) :: gout
  real(r_single),dimension(nx,ny)  ,intent(inout) :: gin

! Declare local variables  
  integer(i_kind) i,ip,j,jj,jm,jp,jjp
  real(r_single),dimension(nx,ny):: hin
  real(r_single),dimension(nx,(ny+5)/2):: c

  if(iorder==1)then
     do i=1,itotsub
        c(ltosj_s(i),ltosi_s(i))=gout(i)
     end do
  else
     do i=1,iglobal
        c(ltosj(i),ltosi(i))=gout(i)
     end do
  endif
  
  
! Transfer half grid to staggered grid
  if(igtype==1) then
     jj=0
     do j=1,ny,2
        jj=jj+1
        do i=1,nx
           hin(i,j)=c(i,jj)
           gin(i,j)=gin(i,j)+c(i,jj)
        end do
     end do
     do j=2,ny,2
        jm=j-1 ; jp=j+1 ; if(jp>ny) jp=j-1
        do i=1,nx-1
           ip=i+1
           gin(i,j)=gin(i,j)+.25_r_single*(hin(i,jm)+hin(ip,jm)+hin(i,jp)+hin(ip,jp))
        end do
        gin(nx,j)=gin(nx-1,j)*2-gin(nx-2,j)
     end do
  else
     jj=0
     do j=1,ny,2
        jj=jj+1
        do i=1,nx-1
           ip=i+1
           hin(i,j)=.5_r_single*(c(i,jj)+c(ip,jj))
        end do
     end do
     jj=0
     do j=2,ny,2
        jj=jj+1
        jjp=jj+1; if(j==ny) jjp=jj
        do i=1,nx
           hin(i,j)=.5_r_single*(c(i,jj)+c(i,jjp))
        end do
     end do
     do j=1,ny,2
        do i=1,nx-1
           gin(i,j)=gin(i,j)+hin(i,j)
        end do
        gin(nx,j)=gin(nx-1,j)*2-gin(nx-2,j)
     end do
     do j=2,ny,2
        do i=1,nx
           gin(i,j)=gin(i,j)+hin(i,j)
        end do
     end do
  end if
  
end subroutine unhalf_nmm_grid2
