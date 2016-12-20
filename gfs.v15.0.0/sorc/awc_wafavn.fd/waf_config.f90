! waf_config.f90
! contains subroutines to calculate icing
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 11/05/09

module waf_config

use kinds
use cfgini
use fuzzy
use physcons
use tokenize
use waf_glob

implicit none

private
public config_get_parms

contains
!----------------------------------------------------------------------------
subroutine get_cfg_gparms(grib, tag, sections, gparms, iret)
! extracts pds and missing values parameters from cfg sections
    character(len=1), intent(in) :: grib
    character(len=*), intent(in) :: tag
    type(cfg_sect_t), dimension(:) :: sections
    type(gparms_t), intent(out) :: gparms
    integer, intent(out) :: iret ! return code, 0 on success

    type(cfg_sect_t) :: sect
    character(len=*), parameter :: myself = 'get_cfg_gparms(): '

    if(grib == "2") then
      call cfg_get_sect(tag, sections, sect, iret)
      if (iret /= 0) return
      call cfg_get_item('npdt', sect, gparms%npdt, iret)
      if (iret /= 0) return
      call cfg_get_item('icat', sect, gparms%icat, iret)
      if (iret /= 0) return
      call cfg_get_item('iprm', sect, gparms%iprm, iret)
      if (iret /= 0) return
      call cfg_get_item('ilev', sect, gparms%ilev, iret)
      if (iret /= 0) return
      call cfg_get_item('stat', sect, gparms%stat, iret)
      if (iret /= 0) return
      call cfg_get_item('ndrt', sect, gparms%ndrt, iret)
      if (iret /= 0) return
      call cfg_get_item('drt2', sect, gparms%drt2, iret)
      if (iret /= 0) return
      call cfg_get_item('drt3', sect, gparms%drt3, iret)
      if (iret /= 0) return
      call cfg_get_item('drt4', sect, gparms%drt4, iret)
      if (iret /= 0) return
      call cfg_get_item('missing', sect, gparms%msng, iret)
      if (iret /= 0) return
      call cfg_get_item('use_bitmap', sect, gparms%bitmap, iret)
      if (iret /= 0) return
    else
      call cfg_get_sect(tag, sections, sect, iret)
      if (iret /= 0) return
      call cfg_get_item('pds5', sect, gparms%pds5, iret)
      if (iret /= 0) return
      call cfg_get_item('pds6', sect, gparms%pds6, iret)
      if (iret /= 0) return
      call cfg_get_item('pds22', sect, gparms%pds22, iret)
      if (iret /= 0) return
      call cfg_get_item('missing', sect, gparms%msng, iret)
      if (iret /= 0) return
      call cfg_get_item('use_bitmap', sect, gparms%bitmap, iret)
      if (iret /= 0) return
    endif
    return
end subroutine get_cfg_gparms

!----------------------------------------------------------------------------
subroutine get_cfg_cell(tag, sections, cell, iret)
! extracts vertical (dp) and horizontal (dxdy) cell size
    character(len=*), intent(in) :: tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    type(cell_t), intent(out) :: cell
    integer, intent(out) :: iret ! return code, 0 on success

    type(cfg_sect_t) :: sect
    character(len=*), parameter :: myself = 'get_cfg_cell(): '

    call cfg_get_sect(tag, sections, sect, iret)
    if (iret /= 0) return
    call cfg_get_item('dp', sect, cell%dp, iret)
    if (iret /= 0) return
    call cfg_get_item('dxdy', sect, cell%dxdy, iret)
    if (iret /= 0) return
    cell%dxdy = 1000.0*cell%dxdy ! convert to m
end subroutine get_cfg_cell

!----------------------------------------------------------------------------
subroutine config_get_parms(grib, products, filename, cfg, iret)
! reads configuration file
    character(len=1), intent(in) :: grib
    type(product_t), intent(in) :: products ! requested products
    character(len=*), intent(in) :: filename ! configuration file
    type(cfg_t), intent(out) :: cfg ! parameters read from cfg file
    integer, intent(out) :: iret ! return code, 0 on success

    integer, parameter :: num_sect = 40 ! maximum number of sections
    type(cfg_sect_t) :: section
    type(cfg_sect_t), dimension(num_sect) :: cfg_sections
    character(len=64) :: tag
    character(len=*), parameter :: myself = 'get_config(): '

    call cfg_read_file(glob_lu_cfg, filename, num_sect, cfg_sections, iret)
    if (iret /= 0) return
    if (products%do_icng) then
        call get_cfg_gparms(grib, 'icng_mean_gparms', cfg_sections, &
            cfg%icng_mean_gparms, iret)
        if (iret /= 0) return
        call get_cfg_gparms(grib, 'icng_max_gparms', cfg_sections, &
            cfg%icng_max_gparms, iret)
        if (iret /= 0) return
        call cfg_get_item('icng_levels', 'p', cfg_sections, &
            cfg%num_icng_lvls, cfg%icng_lvls, iret)
        if (iret /= 0) return
        call get_cfg_cell('icng_cell', cfg_sections, cfg%icng_cell, iret)
        if (iret /= 0) return
    end if
    if (products%do_tcld) then
        call get_cfg_gparms(grib, 'tcld_mean_gparms', cfg_sections, &
            cfg%tcld_mean_gparms, iret)
        if (iret /= 0) return
        call get_cfg_gparms(grib, 'tcld_max_gparms', cfg_sections, &
            cfg%tcld_max_gparms, iret)
        if (iret /= 0) return
        call cfg_get_sect('tcld_parms', cfg_sections, section, iret)
        if (iret /= 0) return
        call cfg_get_item('min_cloud_cover', section, &
            cfg%tcld_min_cld_cover, iret)
        if (iret /= 0) return
        call cfg_get_item('nocloud_value', section, cfg%tcld_nocld, iret)
        if (iret /= 0) return
        call cfg_get_item('tcld_levels', 'p', cfg_sections, &
            cfg%num_tcld_lvls, cfg%tcld_lvls, iret)
        if (iret /= 0) return
        call get_cfg_cell('tcld_cell', cfg_sections, cfg%tcld_cell, iret)
        if (iret /= 0) return
    end if
    if (products%do_cat) then
        call get_cfg_gparms(grib, 'cat_mean_gparms', cfg_sections, &
            cfg%cat_mean_gparms, iret)
        if (iret /= 0) return
        call get_cfg_gparms(grib, 'cat_max_gparms', cfg_sections, &
            cfg%cat_max_gparms, iret)
        if (iret /= 0) return
        call cfg_get_item('cat_levels', 'p', cfg_sections, &
            cfg%num_cat_lvls, cfg%cat_lvls, iret)
        if (iret /= 0) return
        call get_cfg_cell('cat_cell', cfg_sections, cfg%cat_cell, iret)
        if (iret /= 0) return
    end if
    if (products%do_cb) then
        call get_cfg_gparms(grib, 'cb_cover_gparms', cfg_sections, &
            cfg%cb_cover_gparms, iret)
        if (iret /= 0) return
        call get_cfg_gparms(grib, 'cb_hgt_bot_gparms', cfg_sections, &
            cfg%cb_hgt_bot_gparms, iret)
        if (iret /= 0) return
        call get_cfg_gparms(grib, 'cb_hgt_top_gparms', cfg_sections, &
            cfg%cb_hgt_top_gparms, iret)
        if (iret /= 0) return
!        call get_cfg_gparms(grib, 'cb_embd_hgt_bot_gparms', cfg_sections, &
!            cfg%cb_embd_hgt_bot_gparms, iret)
!        if (iret /= 0) return
!        call get_cfg_gparms(grib, 'cb_embd_hgt_top_gparms', cfg_sections, &
!            cfg%cb_embd_hgt_top_gparms, iret)
!        if (iret /= 0) return
        call cfg_get_sect('cb_parms', cfg_sections, section, iret)
        if (iret /= 0) return
        call cfg_get_item('cloud_min_depth', section, cfg%cb_min_depth, iret)
        if (iret /= 0) return
        cfg%cb_min_depth = 100.0 * cfg%cb_min_depth ! hPa -> Pa
        call cfg_get_item('cloud_min_top', section, cfg%cb_min_top, iret)
        if (iret /= 0) return
        cfg%cb_min_top = 100.0*cfg%cb_min_top ! hPa -> Pa
        call cfg_get_item('low_cloud_cover', section, cfg%cb_low_cld_cover, &
            iret)
        if (iret /= 0) return
        call get_cfg_cell('cb_cell', cfg_sections, cfg%cb_cell, iret)
        if (iret /= 0) return
    end if
    ! for all products
    call cfg_get_sect('pressure_range', cfg_sections, section, iret)
    if (iret /= 0) return
    call cfg_get_item('bottom', section, cfg%pres_bot, iret)
    if (iret /= 0) return
    call cfg_get_item('top', section, cfg%pres_top, iret)
    if (iret /= 0) return
    ! this assumes pressure levels are multiple of 2500 Pa
    if (mod(cfg%pres_bot, 25) /= 0 .or. mod(cfg%pres_top, 25) /= 0 &
        .or. cfg%pres_bot <= cfg%pres_top) then
        iret = 1
        print *, myself, 'Incorrect value pressure_range in ' // &
            'configuration file'
        return
    end if
    call fuzzy_log_from_config('pcp_to_cover', cfg_sections, &
        cfg%pcp2cover, iret)
end subroutine config_get_parms

end module waf_config
