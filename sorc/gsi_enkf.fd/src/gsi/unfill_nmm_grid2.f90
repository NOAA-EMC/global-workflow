subroutine unfill_nmm_grid2(gout,nx,ny,gin,igtype,iorder)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    unfill_nmm_grid2           opposite of fill_nmm_grid2
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: This is almost the reverse of subroutine fill_nmm_grid2.  
!           The input field is an analysis increment on an unstaggered 
!           A grid.  This routine extracts the points which coincide 
!           with the output E grid and adds them to the preexisting
!           contents of the E grid.  Before this is done, the input
!           grid must be reordered from the special ordering required 
!           for gathering up a full horizontal grid from the horizontal 
!           subdomains scattered across the processors.
!
!           See fill_nmm_grid2.f90 for additional comments.
!
! program history log:
!   2004-06-22  parrish, document
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     gout     - input filled grid  (reorganized for distibution to local 
!                 domains)
!     gin      - preexisting input values to be added to on staggered E grid
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid 
!                 (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, see 
!                 illustration below)
!     iorder
!
!   output argument list
!     gin      - output result on staggered E grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use gridmod, only: itotsub,iglobal
  use general_commvars_mod, only: ltosi,ltosj,ltosi_s,ltosj_s
  implicit none
  
  integer(i_kind), intent(in   ) :: nx,ny,igtype,iorder
  real(r_single) , intent(in   ) :: gout(itotsub)
  real(r_single) , intent(inout) :: gin(nx,ny)
  
  real(r_single) b(2*nx-1,ny)
  integer(i_kind) i,j
  
  if(iorder==1)then
     do i=1,itotsub
        b(ltosj_s(i),ltosi_s(i))=gout(i)
     end do
  else
     do i=1,iglobal
        b(ltosj(i),ltosi(i))=gout(i)
     end do
  endif
  
  if(igtype==1) then
     do j=1,ny,2
        do i=1,nx
           gin(i,j)=gin(i,j)+b(2*i-1,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx-1
           gin(i,j)=gin(i,j)+b(2*i,j)
        end do
     end do
  else
     do j=1,ny,2
        do i=1,nx-1
           gin(i,j)=gin(i,j)+b(2*i,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx
           gin(i,j)=gin(i,j)+b(2*i-1,j)
        end do
     end do
  end if
  
end subroutine unfill_nmm_grid2
