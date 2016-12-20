! waf_icng.f90
! contains subroutines to calculate icing
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 03/07/07

module waf_icng

use fuzzy
use funcphys
use kinds
use physcons
use waf_filter
use waf_glob

implicit none

private
public icng_alg_read

contains

!----------------------------------------------------------------------------
! read in potential to do mean/max, no icing algorithm applied
! icing is available on selected pressure levels. 
subroutine icng_alg_read(cfg, model, waf)
    type(cfg_t), intent(in) :: cfg
    type(input_data_t), intent(in) :: model
    type(output_data_t), intent(inout) :: waf

    integer :: lvl, model_lvl, nx, ny
    real(kind=r_kind) :: dy
    real(kind=r_kind), dimension(model%ny) :: dx
    real(kind=r_kind), dimension(:,:,:), allocatable :: icng
    character(len=*), parameter :: myself = 'icng_alg(): '

    nx = model%nx
    ny = model%ny
    dx = model%dx(1,:) ! works with lat-lon grid 
    dy = model%dy(1,1)
    allocate(icng(nx,ny,model%np))
    icng(:,:,:) = model%pot(:,:,:)
    do lvl = 1, waf%num_icng_lvls
        model_lvl = glob_find_index(waf%icng_lvls(lvl), model%p, model%np)
        call filter_avg3d(nx, ny, model%np, model_lvl, icng, dx, dy, &
            glob_delta_p, cfg%icng_cell%dxdy, cfg%icng_cell%dp, &
            cfg%icng_mean_gparms%msng, mask=icng>=0.0, &
            amean=waf%icng_mean(:,:,lvl), amax=waf%icng_max(:,:,lvl))
    end do
    deallocate(icng)
end subroutine icng_alg_read

end module waf_icng
