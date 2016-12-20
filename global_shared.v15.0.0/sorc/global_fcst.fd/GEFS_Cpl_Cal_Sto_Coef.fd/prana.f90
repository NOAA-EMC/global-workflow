! module prana
! generate a random proper rotation (orthogonal) matrix. 
!
! the procedure is to fill a square matrix of desired order with independent
! gaussian random numbers, then to apply gram-schmidt orthonormalization.
! the "4.... goto 4" loop allows for reiteration in the very rare case that
! the initial random matrix is (to within round-off) essentially singular.
! the final `coin toss' randomization of orientation of each column along
! the line it lies along overcomes what would otherwise be a bias that
! comes from the way the gram-schmidt code works. the even number of reversals
! ensures the preservation of the determinant.
!=============================================================================
module prana
!=============================================================================
use pran; use peuc
implicit none
private
public:: ranrot
interface ranrot; module procedure ranrot_s,ranrot_d; end interface

contains

!=============================================================================
subroutine ranrot_s(a)
!=============================================================================
real(4),dimension(:,:),intent(out)    :: a
!-----------------------------------------------------------------------------
integer                            :: n,i,j,nrank,flip
real(4),dimension(size(a,1),size(a,1)):: as
real(4)                               :: det,xran
!=============================================================================
n=size(a,1); if(size(a,2)/=n)stop 'in ranrot; incompatible dimensions'
4 do j=1,n; do i=1,n; call gauss(as(i,j)); enddo; enddo
print'(t5,10f7.4)',as
call gram(as,a,nrank,det); if(nrank/=n)goto 4
print *, 'nrank=',nrank
print'(t5,10f7.4)',a
flip=1; do j=1,n-1
call uniform(xran);if(xran>.5)then; flip=-flip; a(:,j)=-a(:,j); endif
enddo; a(:,n)=flip*a(:,n)
print'(t5,10f7.4)',a
end subroutine ranrot_s

!=============================================================================
subroutine ranrot_d(a)
!=============================================================================
real(8),dimension(:,:),intent(out)    :: a
!-----------------------------------------------------------------------------
integer                               :: n,i,j,nrank,flip,k
real(8),dimension(size(a,1),size(a,1)):: as
real(8)                               :: det,xran
!=============================================================================
n=size(a,1); if(size(a,2)/=n)stop 'in ranrot; incompatible dimensions'
4 do j=1,n; do i=1,n; call gauss(as(i,j)); enddo; enddo
!a=0.0
!do j=1,n; do i=1,n; do k=1,n; a(i,j)=a(i,j)+as(k,i)*as(k,j); enddo; enddo; enddo
!print *,'raw'
!print'(t5,10f7.4)',as
!print *,'raw variance)'
!print'(t5,10f7.4)',a
!a=0.0
call gram(as,a,nrank,det); if(nrank/=n)goto 4
!as=0.0
!do j=1,n; do i=1,n; do k=1,n; as(i,j)=as(i,j)+a(k,i)*a(k,j); enddo; enddo; enddo
!print *, 'nrank=',nrank
!print *,'after gram'
!print'(t5,10f7.4)',a
!print *,'after gram variance)'
!print'(t5,10f7.4)',as
!flip=1; do j=1,n-1
!call uniform(xran);if(xran>.5d0)then; flip=-flip; a(:,j)=-a(:,j); endif
!enddo; a(:,n)=flip*a(:,n)
!as=0.0
!do j=1,n; do i=1,n; do k=1,n; as(i,j)=as(i,j)+a(k,i)*a(k,j); enddo; enddo; enddo
!print *,'after flipping'
!print'(t5,10f7.4)',a
!print *,'after gram variance)'
!print'(t5,10f7.4)',as
end subroutine ranrot_d

end module prana
