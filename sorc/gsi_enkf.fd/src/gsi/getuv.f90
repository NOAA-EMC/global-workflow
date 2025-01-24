subroutine getuv(u,v,st,vp,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getuv     
!
!   prgmmr: kleist           org: np22                date: 2004-10-15
!
! abstract: performs setup and calls routine to get conversion from 
!           streamfunction and velocity potential to u,v
!
! program history log:
!   2004-10-15  kleist - initial routine
!   2005-01-22  parrish - add "use compact_diffs"
!   2008-06-04  safford - rm unused var i
!   2009-04-21  derber - rewrite for improved communication - combine with adjoint
!   2010-04-01  treadon - move strip,reorder,reorder2,vectosub to gridmod
!   2012-06-12  parrish - replace sub2grid/grid2sub with general_sub2grid/general_grid2sub
!   2014-12-03  derber - restructure if and do statements.
!
!   input argument list:
!     st        - stream function grid values 
!     vp        - velocity potential grid values 
!     iflg      = 0 forward model
!               = 1 adjoint model
!
!   output argument list:
!     u         - u grid values 
!     v         - v grid values 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,lon2,nlat,nlon
  use compact_diffs, only: stvp2uv,tstvp2uv
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2guv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: st,vp
  integer(i_kind)                       ,intent(in   ) :: iflg

  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v

! Declare local variables
  integer(i_kind) i,j,k

  real(r_kind),allocatable,dimension(:,:,:,:):: work1,worksub
  real(r_kind),dimension(nlat,nlon)::awork,bwork

  allocate(worksub(2,s2guv%lat2,s2guv%lon2,s2guv%nsig))

  if(iflg == 0)then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              worksub(1,i,j,k)=st(i,j,k)
              worksub(2,i,j,k)=vp(i,j,k)
           end do
        end do
     end do
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              worksub(1,i,j,k)=u(i,j,k)
              worksub(2,i,j,k)=v(i,j,k)
           end do
        end do
     end do
  end if

  allocate(work1(2,s2guv%nlat,s2guv%nlon,s2guv%kbegin_loc:s2guv%kend_alloc))
  call general_sub2grid(s2guv,worksub,work1)

  if(regional)then
     if(iflg == 0)then
        do k=s2guv%kbegin_loc,s2guv%kend_loc
           call psichi2uv_reg(work1(1,:,:,k),work1(2,:,:,k),awork,bwork)
           do j=1,nlon
              do i=1,nlat
                 work1(1,i,j,k)=awork(i,j)
                 work1(2,i,j,k)=bwork(i,j)
              end do
           end do
        end do
     else
        do k=s2guv%kbegin_loc,s2guv%kend_loc
           call psichi2uvt_reg(work1(1,:,:,k),work1(2,:,:,k),awork,bwork)
           do j=1,nlon
              do i=1,nlat
                 work1(1,i,j,k)=awork(i,j)
                 work1(2,i,j,k)=bwork(i,j)
              end do
           end do
        end do
     end if
  else
     if(iflg == 0)then
        do k=s2guv%kbegin_loc,s2guv%kend_loc
           call stvp2uv(work1(1,1,1,k),2)
        end do
     else
        do k=s2guv%kbegin_loc,s2guv%kend_loc
           call tstvp2uv(work1(1,1,1,k),2)
        end do
     end if
  end if

  call general_grid2sub(s2guv,work1,worksub)
  deallocate(work1)
  if(iflg == 0) then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              u(i,j,k)=worksub(1,i,j,k)
              v(i,j,k)=worksub(2,i,j,k)
           end do
        end do
     end do
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              st(i,j,k)=st(i,j,k)+worksub(1,i,j,k)
              vp(i,j,k)=vp(i,j,k)+worksub(2,i,j,k)
           end do
        end do
     end do
  end if

  deallocate(worksub)

  return
end subroutine getuv
