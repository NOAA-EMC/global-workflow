! waf_tcld.f90
! contains subroutines to calculate turbulence based on derivative of
! equivalent potential temperature
! George Trojan, SAIC/EMC/NCEP, February 2007
! Last update: 12/07/07

module waf_tcld

use physcons
use funcphys
use fuzzy
use kinds
use waf_calc
use waf_filter
use waf_glob
use waf_phys

implicit none

private
public tcld_alg

contains
!----------------------------------------------------------------------------
elemental function f_turb(min_cld_cover, nocld, the, dthe_dh, cld_cover, &
        conv_cld_cover)
! calculates in-cloud turbulence as negative part of Brunt-Vaisala frequency
! cloudy areas with positive values of B-V are assigned value 0,
! no-cloud areas have value cfg%tcld_nocld
    real(kind=r_kind) :: f_turb
    real(kind=r_kind), intent(in) :: min_cld_cover, nocld
    real(kind=r_kind), intent(in) :: the, dthe_dh, cld_cover, conv_cld_cover

    real(kind=r_kind) :: n2

    if (max(cld_cover, conv_cld_cover) >= min_cld_cover) then
        n2 = con_g*dthe_dh/the 
        if (n2 < 0.0) then
            f_turb = sqrt(-n2)
        else
            f_turb = 0.0
        end if
    else
        f_turb = nocld
    end if
end function f_turb

!----------------------------------------------------------------------------
subroutine tcld_alg(cfg, model, waf)
    type(cfg_t), intent(in) :: cfg
    type(input_data_t), intent(in) :: model
    type(output_data_t), intent(inout) :: waf

    integer :: lvl, model_lvl, nx, ny
    real(kind=r_kind) :: fp, dy
    real(kind=r_kind), dimension(model%ny) :: dx
    real(kind=r_kind), dimension(:,:,:), allocatable :: the, turb
    real(kind=r_kind), dimension(:,:), allocatable :: dthe_dh, conv_cld_cover
    character(len=*), parameter :: myself = 'tcld_alg(): '

    nx = waf%nx
    ny = waf%ny
    dx = model%dx(1,:) ! works with lat-lon grid 
    dy = model%dy(1,1)
    allocate(the(nx,ny,model%np))
    allocate(turb(nx,ny,model%np))
    allocate(dthe_dh(nx,ny))
    allocate(conv_cld_cover(nx,ny))
    ! calculate theta_e and its vertical derivative
    do lvl = 1, model%np ! model pressure levels
        fp = 100.0*model%p(lvl)
        where (model%t(:,:,lvl) /= glob_msng .and. &
            model%rh(:,:,lvl) /= glob_msng)
            the(:,:,lvl) = phys_theta_e(model%t(:,:,lvl), model%rh(:,:,lvl), fp)
        elsewhere
            the(:,:,lvl) = glob_msng
        end where
    end do
    do lvl = 2, model%np-1 ! model pressure levels
        fp = 100.0*model%p(lvl)
        call calc_d_dh(model%nx, model%ny, 3, glob_msng, &
            the(:,:,lvl-1:lvl+1), model%hgt(:,:,lvl-1:lvl+1), dthe_dh)
        where (model%conv_cld_cover /= glob_msng .and. &
            fp > model%conv_pres_top .and. fp <= model%conv_pres_bot)
            conv_cld_cover = model%conv_cld_cover
        elsewhere
            conv_cld_cover = 0.0
        end where
        where (model%cld_cover(:,:,lvl) /= glob_msng .and. &
            dthe_dh /= glob_msng .and. conv_cld_cover /= glob_msng)
            turb(:,:,lvl) = f_turb(cfg%tcld_min_cld_cover, cfg%tcld_nocld, &
                the(:,:,lvl), dthe_dh, model%cld_cover(:,:,lvl), conv_cld_cover)
        elsewhere
            turb(:,:,lvl) = cfg%tcld_mean_gparms%msng
        end where
        where (model%sfc_pres < fp) turb(:,:,lvl) = cfg%tcld_mean_gparms%msng
    end do
    deallocate(the)
    deallocate(dthe_dh)
    deallocate(conv_cld_cover)
    ! average over 3-dimensional cell
    do lvl = 1, waf%num_tcld_lvls ! requested pressure levels
        model_lvl = glob_find_index(waf%tcld_lvls(lvl), model%p, model%np)
        call filter_avg3d(nx, ny, model%np, model_lvl, turb, dx, dy, &
            glob_delta_p, cfg%tcld_cell%dxdy, cfg%tcld_cell%dp, &
            cfg%tcld_mean_gparms%msng, mask=turb>=0.0, &
            amean=waf%tcld_mean(:,:,lvl), amax=waf%tcld_max(:,:,lvl))
    end do
    deallocate(turb)
end subroutine tcld_alg
    
end module waf_tcld
