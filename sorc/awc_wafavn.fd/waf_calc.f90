! waf_calc.f90
! module containing subroutines for vector calculus
! George Trojan, SAIC/EMC/NCEP, December 2006
! Last update: 08/13/07

module waf_calc

use kinds 

implicit none

private
public calc_grad, calc_d_dh

contains
!----------------------------------------------------------------------------
subroutine calc_grad(nx, ny, msng, f, dx, dy, df_dx, df_dy)
! calculates gradient on global grid
    integer, intent(in) :: nx, ny ! grid dimensions
    real(kind=r_kind), intent(in) :: msng ! missing value
    real(kind=r_kind), dimension(nx,ny), intent(in) :: f ! input field
    real(kind=r_kind), dimension(nx,ny), intent(in) :: dx, dy ! grid increments
    real(kind=r_kind), dimension(nx,ny), intent(out) :: df_dx, df_dy ! gradient

    integer :: i, i1, i2, j
    real(kind=r_kind) :: f_x2, f_x1, f_y2, f_y1

    df_dx = msng
    df_dy = msng
    do j = 2, ny-1
        do i = 1, nx
            if (i == 1) then
                i1 = nx
                i2 = 2
            else if (i == nx) then
                i1 = nx - 1
                i2 = 1
            else
                i1 = i - 1
                i2 = i + 1
            end if
            f_x2 = f(i2,j)
            f_x1 = f(i1,j)
            f_y2 = f(i,j-1) ! the second dimension is north to south
            f_y1 = f(i,j+1)
            if (f_x2 == msng .or. f_x1 == msng .or. &
                f_y2 == msng .or. f_y1 == msng) cycle
            df_dx(i,j) = 0.5*(f_x2 - f_x1)/dx(i,j)
            df_dy(i,j) = 0.5*(f_y2 - f_y1)/dy(i,j)
        end do
    end do
    df_dx(:,1) = df_dx(:,2)
    df_dx(:,ny) = df_dx(:,ny-1)
    df_dy(:,1) = df_dy(:,2)
    df_dy(:,ny) = df_dy(:,ny-1)
end subroutine calc_grad
    
!----------------------------------------------------------------------------
subroutine calc_d_dh(nx, ny, nz, msng, f, h, df_dh)
! calculates partial derivative df/dh 
    integer, intent(in) :: nx, ny ! grid dimensions 
    integer, intent(in) :: nz ! slice size: 2 for simple, 3 for centered
                              ! difference
    real(kind=r_kind), intent(in) :: msng ! missing value
    real(kind=r_kind), dimension(nx,ny,nz), intent(in) :: f ! nz layers of 
                                                            ! input field
    real(kind=r_kind), dimension(nx,ny,nz), intent(in) :: h ! nz layers of z vrb
    real(kind=r_kind), dimension(nx,ny), intent(out) :: &
        df_dh ! calculated derivative

    where (f(:,:,1) /= msng .and. f(:,:,nz) /= msng .and. &
        h(:,:,1) /= msng .and. h(:,:,nz) /= msng)
        df_dh = (f(:,:,nz)-f(:,:,1))/(h(:,:,nz)-h(:,:,1))
    elsewhere
        df_dh = msng
    end where
end subroutine calc_d_dh
    
!----------------------------------------------------------------------------
end module waf_calc
