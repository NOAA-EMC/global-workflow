subroutine smoothww(nx,ny,p,wl,nitr,mx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smoothww    smooth two dimensional field
!   prgmmr: derber           org: np22                date: 2004-05-13
!
! abstract: This routine smooths a two dimensional field
!
! program history log:
!   2004-05-13  derber, document
!   2010-10-08  derber - optimize and clean up
!
!   input argument list:
!     nx   - first dimension of two dimensional array
!     ny   - second dimension of two dimensional array
!     p    - nx by ny array (field) to be smoothed (filtered)
!     wl   - smoothing coefficient
!     nitr - number of smoother passes
!     mx   - smoothing coefficient multiplier
!
!   output argument list
!     p    - smoothed (filtered) two dimensional array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: one,two
  implicit none

  integer(i_kind),intent(in   ) :: nx,ny,nitr,mx
  real(r_kind)   ,intent(in   ) :: wl
  real(r_kind)   ,intent(inout) :: p(nx,ny)

  integer(i_kind) nx1,nx2,ny1,ny2,n
  integer(i_kind) im,jm,jp,j,ip,i

  real(r_kind) hwl,hwl2,hwlx

! Initialize local variables
  hwl=wl/nitr
  hwl2=hwl/((hwl+one)+sqrt(hwl*two+one))
  hwl=hwl*mx
  hwlx=hwl/((hwl+one)+sqrt(hwl*two+one))
  hwl=hwl2


! Big loop over number of smoother passes       
  do n=1,nitr


!    Smooth in 2d over all y (second dimension)
     ny1=1
     ny2=ny

!    Adjoint(?) of forward pass
     nx1=1
     nx2=nx-1
     do j=ny1,ny2
        do i=nx1,nx2
           p(i,j)=(one-hwlx)*p(i,j)
        enddo
     enddo
     do i=nx2,nx1,-1
        ip=i+1
        do j=ny1,ny2
           p(i,j)=p(i,j)+hwlx*p(ip,j)
        enddo
     enddo

!    Adjoint(?) of backward pass
     nx1=2
     nx2=nx
     do j=ny1,ny2
        do i=nx1,nx2
           p(i,j)=(one-hwlx)*p(i,j)
        enddo
     enddo
     do i=nx1,nx2   
        im=i-1
        do j=ny1,ny2 
           p(i,j)=p(i,j)+hwlx*p(im,j)
        enddo
     enddo

!    Backward pass
     nx1=1
     nx2=nx-1
     do i=nx2,nx1,-1
        ip=i+1
        do j=ny1,ny2
           p(i,j)=p(i,j)+hwlx*(p(ip,j)-p(i,j))
        enddo
     enddo

!    Forward pass
     nx1=2
     nx2=nx
     do i=nx1,nx2
        im=i-1
        do j=ny1,ny2
           p(i,j)=p(i,j)+hwlx*(p(im,j)-p(i,j))
        enddo
     enddo


!    Smooth in 2d over all x (first dimension)
     nx1=1
     nx2=nx

!    Adjoint(?) of forward pass
     ny1=1
     ny2=ny-1
     do j=ny1,ny2
        do i=nx1,nx2
           p(i,j)=(one-hwl)*p(i,j)
        enddo
     enddo
     do j=ny2,ny1,-1
        jp=j+1
        do i=nx1,nx2
           p(i,j)=p(i,j)+hwl*p(i,jp)
        enddo
     enddo

!    Adjoint(?) of backward pass
     ny1=2
     ny2=1+ny
     do j=ny1,ny2
        do i=nx1,nx2
           p(i,j)=(one-hwl)*p(i,j)
        enddo
     enddo
     do j=ny1,ny2 
        jm=j-1
        do i=nx1,nx2   
           p(i,j)=p(i,j)+hwl*p(i,jm)
        enddo
     enddo

!    Backward pass
     ny1=1
     ny2=ny-1
     do j=ny2,ny1,-1
        jp=j+1
        do i=nx1,nx2
           p(i,j)=p(i,j)+hwl*(p(i,jp)-p(i,j))
        enddo
     enddo

!    Forward pass
     ny1=2
     ny2=ny
     do j=ny1,ny2
        jm=j-1
        do i=nx1,nx2
           p(i,j)=p(i,j)+hwl*(p(i,jm)-p(i,j))
        enddo
     enddo

  enddo

  return
end subroutine smoothww
