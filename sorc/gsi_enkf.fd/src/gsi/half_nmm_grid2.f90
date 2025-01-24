subroutine half_nmm_grid2(gin,nx,ny,gout,igtype,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    half_nmm_grid2    make a-grid from every other row of e-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered E grid used by the wrf nmm.
!           This is done by keeping every other row of the original E grid.  If this 
!           is a mass variable (igtype=1), then no interpolation is required.  If this
!           is a wind variable (igtype=2), then interpolation is necessary.  This procedure
!           is necessary because the gsi is not yet able to work with anything other than
!           unstaggered grids.  This solution introduces greater interpolation error
!           compared to the option fill_nmm_grid2, but has the advantage of 4 times fewer
!           grid points compared to the output of fill_nmm__grid2.  This routine will be
!           eliminated when the gsi has the capability to work directly with staggered grids.
!
! program history log:
!   2004-06-22  parrish, document
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     gin      - input staggered E grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, see illustration below)
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
!     gout     - output unstaggered half grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: zero_single
  use gridmod, only: iglobal, itotsub
  use general_commvars_mod, only: ltosi, ltosj, ltosi_s, ltosj_s

  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nx,ny,igtype,iorder
  real(r_single),dimension(nx,ny)  ,intent(in   ) :: gin
  real(r_single),dimension(itotsub),intent(  out) :: gout

! Declare local variables
  integer(i_kind) i,i0,im,j,jj,jm,jp
  real(r_single),dimension(nx,(ny+5)/2):: c
  
  if(igtype==1) then
     jj=0
     do j=1,ny,2
        jj=jj+1
        do i=1,nx
           c(i,jj)=gin(i,j)
        end do
     end do
  else
     jj=0
     do j=1,ny,2
        jj=jj+1
        jp=j+1 ; if(jp>ny)   jp=j-1
        jm=j-1 ; if(jm<1) jm=j+1
        do i=1,nx
           im=i-1 ; if(im<1) im=i
           i0=i      ; if(i==nx)   i0=im
           c(i,jj)=0.25_r_single*(gin(im,j)+gin(i0,j)+gin(i,jp)+gin(i,jm))
        end do
     end do
  end if

  
! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero_single
  end do
  if(iorder==1)then
     do i=1,itotsub
        gout(i)=c(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=c(ltosj(i),ltosi(i))
     end do
  endif
  
end subroutine half_nmm_grid2
