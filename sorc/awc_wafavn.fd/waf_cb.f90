! waf_cb.f90
! contains subroutines to calculate CB
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 11/05/09

module waf_cb

use kinds
use fuzzy
use physcons
use waf_filter
use waf_glob
use waf_phys

implicit none

private
public cb_alg

contains
!----------------------------------------------------------------------------
subroutine cb_alg(cfg, model, waf)
! calculates CB coverage
    type(cfg_t), intent(in) :: cfg
    type(input_data_t), intent(in) :: model
    type(output_data_t), intent(inout) :: waf

    integer :: i, j, lvl, nx, ny
    real(kind=r_kind) :: dy
    real(kind=r_kind), dimension(model%ny) :: dx
    real(kind=r_kind), dimension(:,:), allocatable :: cb_cover, cb_pres_bot, &
        cb_pres_top
!    logical, dimension(:,:), allocatable :: cb_mask, embd_cb_mask
    logical, dimension(:,:), allocatable :: cb_mask

    nx = waf%nx ! same as model
    ny = waf%ny
    dx = model%dx(1,:) ! works only with lat-lon grid
    dy = model%dy(1,1)
    allocate(cb_cover(nx,ny))
    allocate(cb_pres_bot(nx,ny))
    allocate(cb_pres_top(nx,ny))
    allocate(cb_mask(nx,ny))
!    allocate(embd_cb_mask(nx,ny))
    ! the 3 fields are consistent
    where (model%conv_pres_top < cfg%cb_min_top .and. &
        model%conv_pres_bot - model%conv_pres_top > cfg%cb_min_depth)
        cb_cover = model%conv_cld_cover
!        embd_cb_mask = model%tot_cld_cover > cfg%cb_low_cld_cover .and. &
!            cb_cover > 0.1
    elsewhere
        cb_cover = glob_msng
!        embd_cb_mask = .false.
    end where
    cb_mask = cb_cover > 0.0
    call filter_avg2d(nx, ny, cb_cover, dx, dy, cfg%cb_cell%dxdy, &
        glob_msng, mask=cb_mask, amean=waf%cb_cover)
    call filter_avg2d(nx, ny, model%conv_pres_top, dx, dy, cfg%cb_cell%dxdy, &
        glob_msng, mask=cb_mask, amin=cb_pres_top)
    call filter_avg2d(nx, ny, model%conv_pres_bot, dx, dy, cfg%cb_cell%dxdy, &
        glob_msng, mask=cb_mask, amean=cb_pres_bot)
    ! after averaging insure consistency
    ! FIXME
    where (waf%cb_cover <= 0.0 .or. cb_pres_bot <= 0.0 .or. cb_pres_top <= 0.0)
        waf%cb_cover = cfg%cb_cover_gparms%msng
        waf%cb_hgt_bot = cfg%cb_hgt_bot_gparms%msng
        waf%cb_hgt_top = cfg%cb_hgt_top_gparms%msng
!        embd_cb_mask = .false.
    elsewhere
        waf%cb_hgt_bot = phys_icao_hgt(cb_pres_bot)
        waf%cb_hgt_top = phys_icao_hgt(cb_pres_top)
    end where
!    where (embd_cb_mask)
!        waf%cb_embd_hgt_bot = waf%cb_hgt_bot
!        waf%cb_embd_hgt_top = waf%cb_hgt_top
!    elsewhere
!        waf%cb_embd_hgt_bot = cfg%cb_embd_hgt_bot_gparms%msng
!        waf%cb_embd_hgt_top = cfg%cb_embd_hgt_top_gparms%msng
!    end where
    deallocate(cb_cover)
    deallocate(cb_pres_bot)
    deallocate(cb_pres_top)
    deallocate(cb_mask)
!    deallocate(embd_cb_mask)
end subroutine cb_alg
    
end module waf_cb
