subroutine fill_nmm_grid2(gin,nx,ny,gout,igtype,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_nmm_grid2         fill holes in (wrf) nmm e-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered E grid used 
!           by the wrf nmm.  This is done by interpolation to fill the 
!           holes in the E grid.  This is necessary because the gsi is 
!           not yet able to work with anything other than unstaggered 
!           grids.  This solution minimizes additional interpolation error
!           but doubles the number of grid points.  This routine will be
!           eliminated when the gsi has the capability to work directly 
!           with staggered grids.
!
! program history log:
!   2004-06-22  parrish, document
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     gin      - input staggered E grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid 
!                (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, 
!                see illustration below)
!
!                   igtype=1:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2                x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!                   igtype=2:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2          x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output filled grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: quarter,half,zero
  use gridmod, only: iglobal,itotsub
  use general_commvars_mod, only: ltosj,ltosi,ltosj_s,ltosi_s
  
  implicit none
  
  integer(i_kind),intent(in   ) :: nx,ny,igtype,iorder
  real(r_single) ,intent(in   ) :: gin(nx,ny)
  real(r_single) ,intent(  out) :: gout(itotsub)
  
  real(r_single) b(2*nx-1,ny)
  integer(i_kind) i,im,ip,j,jm,jp
  real(r_single) fill,test
  
  
  fill=0.95_r_kind*huge(fill) ; test=0.95_r_kind*fill
  do j=1,ny
     do i=1,2*nx-1
        b(i,j)=fill
     end do
  end do
  
  
! First transfer all staggered points to appropriate
! points on filled output grid
  if(igtype==1) then
     do j=1,ny,2
        do i=1,nx
           b(2*i-1,j)=gin(i,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx-1
           b(2*i,j)=gin(i,j)
        end do
     end do
  else
     do j=1,ny,2
        do i=1,nx-1
           b(2*i,j)=gin(i,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx
           b(2*i-1,j)=gin(i,j)
        end do
     end do
  end if

  
!  Now fill in holes

! Top and bottom rows:
  do j=1,ny,ny-1
     do i=1,2*nx-1
        if(b(i,j)>test) then
           ip=i+1 ; if(ip>2*nx-1) ip=i-1
           im=i-1 ; if(im<1) im=i+1
           b(i,j)=half*(b(im,j)+b(ip,j))
        end if
     end do
  end do

  
! Left and right rows:
  do j=1,ny
     jp=j+1 ; if(jp>ny)   jp=j-1
     jm=j-1 ; if(jm<1) jm=j+1
     do i=1,2*nx-1,2*nx-2
        if(b(i,j)>test) b(i,j)=half*(b(i,jm)+b(i,jp))
     end do
  end do

  
! Interior points
  do j=1,ny
     jp=j+1 ; if(jp>ny) jp=j-1
     jm=j-1 ; if(jm<1) jm=j+1
     do i=1,2*nx-1
        if(b(i,j)>test) then
           ip=i+1 ; if(ip>2*nx-1) ip=i-1
           im=i-1 ; if(im<1)      im=i+1
           b(i,j)=quarter*(b(ip,j)+b(im,j)+b(i,jp)+b(i,jm))
        end if
     end do
  end do

  
! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero
  end do
  if(iorder==1)then
     do i=1,itotsub
        gout(i)=b(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=b(ltosj(i),ltosi(i))
     end do
  endif
  
end subroutine fill_nmm_grid2
