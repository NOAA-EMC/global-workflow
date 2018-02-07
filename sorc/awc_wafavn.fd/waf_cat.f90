! waf_cat.f90
! contains subroutines to calculate turbulence based on Ellrod algorithm
! George Trojan, SAIC/EMC/NCEP, December 2006
! Last update: 03/07/07

module waf_cat

use kinds
use waf_calc
use waf_filter
use waf_glob
use waf_phys

implicit none

private
public cat_alg

contains
!----------------------------------------------------------------------------
subroutine cat_alg(cfg, model, waf)
    type(cfg_t), intent(in) :: cfg
    type(input_data_t), intent(in) :: model
    type(output_data_t), intent(inout) :: waf

    integer :: lvl, model_lvl, nx, ny
    real(kind=r_kind) :: fp, dy
    real(kind=r_kind), dimension(model%ny) :: dx
    real(kind=r_kind), dimension(:,:), allocatable :: du_dx, du_dy, dv_dx, &
        dv_dy, du_dh, dv_dh
    real(kind=r_kind), dimension(:,:,:), allocatable :: turb
    character(len=*), parameter :: myself = 'cat_alg(): '

    nx = waf%nx
    ny = waf%ny
    dx = model%dx(1,:) ! works with lat-lon grid 
    dy = model%dy(1,1)
    allocate(turb(nx,ny,model%np))
    allocate(du_dx(nx,ny))
    allocate(du_dy(nx,ny))
    allocate(dv_dx(nx,ny))
    allocate(dv_dy(nx,ny))
    allocate(du_dh(nx,ny))
    allocate(dv_dh(nx,ny))
    do lvl = 2, model%np-1 ! requested pressure levels
        fp = 100.0*model%p(lvl)
        call calc_grad(model%nx, model%ny, glob_msng, &
            model%u_wnd(:,:,lvl), model%dx, model%dy, du_dx, du_dy)
        call calc_grad(model%nx, model%ny, glob_msng, &
            model%v_wnd(:,:,lvl), model%dx, model%dy, dv_dx, dv_dy)
        call calc_d_dh(model%nx, model%ny, 3, glob_msng, &
            model%u_wnd(:,:,lvl-1:lvl+1), model%hgt(:,:,lvl-1:lvl+1), du_dh)
        call calc_d_dh(model%nx, model%ny, 3, glob_msng, &
            model%v_wnd(:,:,lvl-1:lvl+1), model%hgt(:,:,lvl-1:lvl+1), dv_dh)
        where (du_dx /= glob_msng .and. du_dy /= glob_msng .and. &
            dv_dx /= glob_msng .and. dv_dy /= glob_msng .and. & 
            du_dh /= glob_msng .and. dv_dh /= glob_msng)
            turb(:,:,lvl) = phys_ti1(du_dx, du_dy, dv_dx, dv_dy, du_dh, dv_dh)
        elsewhere
            turb(:,:,lvl) = cfg%cat_mean_gparms%msng
        end where
!        call filter_bleck(3, nx, ny, turb(:,:,lvl), &
!            msng=cfg%cat_mean_gparms%msng)
        ! set to missing below surface
        where (model%sfc_pres < fp) turb(:,:,lvl) = cfg%cat_mean_gparms%msng
    end do
    deallocate(du_dx)
    deallocate(du_dy)
    deallocate(dv_dx)
    deallocate(dv_dy)
    deallocate(du_dh)
    deallocate(dv_dh)
    do lvl = 1, waf%num_cat_lvls
        fp = 100.0*model%p(lvl)
        model_lvl = glob_find_index(waf%cat_lvls(lvl), model%p, model%np)
        call filter_avg2d(nx, ny, turb(:,:,model_lvl), dx, dy, &
            cfg%cat_cell%dxdy, cfg%cat_mean_gparms%msng, &
            mask=turb(:,:,model_lvl)>=0.0, amean=waf%cat_mean(:,:,lvl), &
            amax=waf%cat_max(:,:,lvl))
    end do
    deallocate(turb)
end subroutine cat_alg
    
end module waf_cat
