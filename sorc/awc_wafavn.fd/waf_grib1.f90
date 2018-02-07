! waf_grib1.f90
! grib1 IO
! Yali Mao, IMSG/EMC/NCEP, August 2014
!
module waf_grib1

use getoptions
use kinds
use waf_glob
use waf_phys

implicit none

private
public write_output_data1, get_input_data1, get_pdsgds

! array size for w3 library
integer, parameter :: glob_w3_size = 200

! values needed to read data from GRIB file
type pds_t
    integer :: i5   ! pds(5): field number (GRIB table 2)
    integer :: i6   ! pds(6): field level type (GRIB table 3)
end type pds_t

! PDS parameters in the input GRIB file (kpds5, kpds6, kpds7 unused)
type(pds_t), parameter :: &
    pds_sfc_pres = pds_t(1, 1), &
    pds_hgt = pds_t(7, 100), &
    pds_temp = pds_t(11, 100), &
    pds_u_wnd = pds_t(33, 100), &
    pds_v_wnd = pds_t(34, 100), &
    pds_rh = pds_t(52, 100), &
    pds_cld_w = pds_t(153, 100), &
    pds_conv_pres_bot = pds_t(1, 242), &
    pds_conv_pres_top = pds_t(1, 243), &
    pds_conv_pcp_rate = pds_t(214, 1), &
    pds_icng_potential = pds_t(168, 100)!, &
!    pds_icng_severity  = pds_t(175, 100)

contains

!----------------------------------------------------------------------------
subroutine put_grib1(gparms, pds7, def_pds, def_gds, nx, ny, array, iret)
! writes calculated values for one field at all pressure levels
    implicit none
    type(gparms_t), intent(in) :: gparms ! field-specific pds parameters
    integer, intent(in) :: pds7 ! usually pressure level
    integer, dimension(:), intent(in) :: def_pds, def_gds ! PDS and GDS 
        ! values from model data file
    integer, intent(in) :: nx, ny ! grid dimensions
    real(kind=r_kind), dimension(nx,ny) :: array ! data to be written
    integer, intent(out) :: iret ! return code from putgb()

    integer :: npoints
    integer, dimension(glob_w3_size) :: pds, gds ! arrays holding PDS 
        ! and GDS values written to output file
    logical(kind=1), dimension(nx,ny) :: mask
    character(len=*), parameter :: myself = 'put_grib1(): '

    npoints = nx * ny
    pds = def_pds
    gds = def_gds
    if (gparms%bitmap) pds(4) = ior(pds(4), 64)
    pds(5) = gparms%pds5   ! field number (GRIB table 2)
    pds(6) = gparms%pds6   ! field level type (GRIB table 3)
    pds(7) = pds7
    pds(19) = glob_avn_table
    pds(22) = gparms%pds22 ! precision
    if (gparms%bitmap) then
        mask = array /= gparms%msng
    else
        mask = .false.
    end if
    call putgb(glob_lu_out, npoints, pds, gds, mask, array, iret)
    if (iret /= 0) then
        print *, myself, 'failed to store field ', pds(5:7)
    end if
end subroutine put_grib1

!----------------------------------------------------------------------------
subroutine write_output_data1(products, cfg, pds, gds, waf, iret)
! writes output data to GRIB file
    implicit none
    type(product_t), intent(in) :: products ! requested products
    type(cfg_t), intent(in) :: cfg   ! configuration parameters
    integer, dimension(glob_w3_size), intent(in) :: pds, gds ! read from
        ! input file
    type(output_data_t), intent(in) :: waf ! products to write
    integer, intent(out) :: iret ! return code, 0 on success

    integer :: nx, ny, lvl, p

    nx = waf%nx
    ny = waf%ny
    iret = 0
    if (products%do_cb) then
        if (iret == 0) call put_grib1(cfg%cb_hgt_bot_gparms, &
            0, pds, gds, nx, ny, waf%cb_hgt_bot, iret)
        if (iret == 0) call put_grib1(cfg%cb_hgt_top_gparms, &
            0, pds, gds, nx, ny, waf%cb_hgt_top, iret)
!        if (iret == 0) call put_grib1(cfg%cb_embd_hgt_bot_gparms, &
!            0, pds, gds, nx, ny, waf%cb_embd_hgt_bot, iret)
!        if (iret == 0) call put_grib1(cfg%cb_embd_hgt_top_gparms, &
!            0, pds, gds, nx, ny, waf%cb_embd_hgt_top, iret)
        if (iret == 0) call put_grib1(cfg%cb_cover_gparms, &
            0, pds, gds, nx, ny, waf%cb_cover, iret)
    end if
    if (products%do_tcld) then
        do lvl = 1, waf%num_tcld_lvls
            p = waf%tcld_lvls(lvl)
            call put_grib1(cfg%tcld_mean_gparms, p, pds, gds, nx, ny, &
                waf%tcld_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib1(cfg%tcld_max_gparms, p, pds, gds, nx, ny, &
                waf%tcld_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
    if (products%do_cat) then
        do lvl = 1, waf%num_cat_lvls
            p = waf%cat_lvls(lvl)
            call put_grib1(cfg%cat_mean_gparms, p, pds, gds, nx, ny, &
                waf%cat_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib1(cfg%cat_max_gparms, p, pds, gds, nx, ny, &
                waf%cat_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
    if (products%do_icng) then
        do lvl = 1, waf%num_icng_lvls
            p = waf%icng_lvls(lvl)
            call put_grib1(cfg%icng_mean_gparms, p, pds, gds, nx, ny, &
                waf%icng_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib1(cfg%icng_max_gparms, p, pds, gds, nx, ny, &
                waf%icng_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
end subroutine write_output_data1

!----------------------------------------------------------------------------
subroutine get_grib1(pds_def, pres_level, nx, ny, data_array, iret, accum_hour)
! retrieves one field specified by index field_ix at pressure level p
    implicit none
    type(pds_t), intent(in) :: pds_def ! field and level type
    integer, intent(in) :: pres_level ! pressure level index 
    integer, intent(in) :: nx, ny ! grid dimensions
    real(kind=r_kind), dimension(nx,ny), intent(out) :: data_array ! retrieved
                                                                   ! data
    integer, intent(out) :: iret ! return code from getgb()
    real, intent(out), optional :: accum_hour

    integer :: npoints
    integer :: kg, kf, k
    integer, dimension(glob_w3_size) :: pds_mask, gds_mask ! search masks
    integer, dimension(glob_w3_size) :: pds_data, gds_data ! retrieved
        ! values
    logical(kind=1), dimension(nx,ny) :: mask_array

    npoints = nx*ny
    pds_mask = -1
    gds_mask = -1
    pds_mask(5) = pds_def%i5
    pds_mask(6) = pds_def%i6
    pds_mask(7) = pres_level
    call getgb(glob_lu_in, 0, npoints, 0, pds_mask, gds_mask, kf, k, &
        pds_data, gds_data, mask_array, data_array, iret)
    where (.not. mask_array) data_array = glob_msng

    if(present(accum_hour)) accum_hour = pds_data(15) - pds_data(14)
end subroutine get_grib1

!----------------------------------------------------------------------------
subroutine get_input_data1(gds, cfg, model)
! reads input data
    implicit none
    integer, intent(in) :: gds(:) ! only for possible calculation of icing
    type(cfg_t), intent(in) :: cfg ! parameters read from cfg file
    type(input_data_t), intent(inout) :: model ! arrays to hold input data

    integer :: lvl, p, iret, nx, ny
    real(kind=r_kind) :: fp
    real(kind=r_kind), dimension(:,:), allocatable :: buf1, buf2, buf3
    character(len=*), parameter :: myself = 'get_input_data1(): '

    nx = model%nx
    ny = model%ny
    call get_grib1(pds_sfc_pres, 0, nx, ny, model%sfc_pres, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve sfc pres, iret = ', iret
    allocate(buf1(nx,ny))
    allocate(buf2(nx,ny))
    allocate(buf3(nx,ny))
    call get_grib1(pds_conv_pres_bot, 0, nx, ny, buf1, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve pres at bottom of conv cloud, iret = ', iret
    call get_grib1(pds_conv_pres_top, 0, nx, ny, buf2, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve pres at top of conv cloud, iret = ', iret
    call get_grib1(pds_conv_pcp_rate, 0, nx, ny, buf3, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve conv precip rate, iret = ', iret
    ! ensure consistency for convective clouds
    where (buf1 /= glob_msng .and. buf2 /= glob_msng .and. buf3 /= glob_msng)
        model%conv_pres_bot = min(buf1, model%sfc_pres)
        model%conv_pres_top = buf2
        model%conv_cld_cover = fuzzy_log_member(cfg%pcp2cover, 1.0e6*buf3)
    elsewhere
        model%conv_pres_bot = glob_msng
        model%conv_pres_top = glob_msng
        model%conv_cld_cover = glob_msng
    end where
    do lvl = 1, model%np
        p = model%p(lvl)
        call get_grib1(pds_hgt, p, nx, ny, model%hgt(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve Z at ', p, &
            ' hPa, iret = ', iret
        call get_grib1(pds_temp, p, nx, ny, model%t(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve T at ', p, &
            ' hPa, iret = ', iret
        call get_grib1(pds_u_wnd, p, nx, ny, model%u_wnd(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve U at ', p, &
            ' hPa, iret = ', iret
        call get_grib1(pds_v_wnd, p, nx, ny, model%v_wnd(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve V at ', p, &
            ' hPa, iret = ', iret
        call get_grib1(pds_rh, p, nx, ny, model%rh(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve RH at ', p, &
            ' hPa, iret = ', iret
        call get_grib1(pds_cld_w, p, nx, ny, buf1, iret) 
        if (iret /= 0) print *, myself, &
            'failed to retrieve cloud water at ', p, ' hPa, iret = ', iret
        fp = 100.0 * p
        where (buf1 /= glob_msng .and. model%t(:,:,lvl) /= glob_msng .and. &
            model%rh(:,:,lvl) /= glob_msng)
            model%cld_cover(:,:,lvl) = phys_cloud_cover(fp, model%t(:,:,lvl), &
                model%rh(:,:,lvl), buf1)
        elsewhere
            model%cld_cover(:,:,lvl) = glob_msng
        end where
    enddo
    ! testing whether icing is available in grib 1 master file 
    p = model%p(1)
    call get_grib1(pds_icng_potential, p, nx, ny, model%pot(:,:,1), iret)
    if(iret == 0) then
      ! icing is available, read it
      do lvl = 1, model%np
        p = model%p(lvl)
        call get_grib1(pds_icng_potential, p, nx, ny, model%pot(:,:,lvl), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve icing potential at ',p,&
            ' hPa, iret = ', iret
        where(model%pot(:,:,lvl) < -999.)
            model%pot(:,:,lvl) = 0.0
        end where
!        call get_grib1(pds_icng_severity, p, nx, ny, model%sev(:,:,lvl), iret) 
!        if (iret /= 0) print *, myself, 'failed to retrieve icing severity at ', p, &
!            ' hPa, iret = ', iret
      end do
    else
      ! icing is not available, calculate it
      call calc_icing(gds, model)
    endif
    deallocate(buf1)
    deallocate(buf2)
    deallocate(buf3)
end subroutine get_input_data1

!----------------------------------------------------------------------------
subroutine get_pdsgds(nx, ny, pds_data, gds_data, iret)
! returns PDS and GDS section of the first GRIB field
! we assume here that the input grids are the same for all fields
    implicit none
    integer, intent(out) :: nx, ny
    integer, dimension(:), intent(out) :: pds_data, gds_data ! retrieved PDS 
                                                             ! and GDS data
    integer, intent(out) :: iret ! return code, from getgbh()

    integer :: kg, kf, k
    integer, dimension(glob_w3_size), parameter :: pds_mask = -1, &
        gds_mask = -1   ! search mask, -1 is wild card
    character(len=*), parameter :: myself = 'get_pdsgds(): '

    pds_data = 0
    gds_data = 0
    ! getgbh() is in libw3
    call getgbh(glob_lu_in, 0, -1, pds_mask, gds_mask, kg, kf, k, pds_data, &
        gds_data, iret)
    if (iret /= 0) print *, myself, 'getgbh() failed, iret = ', iret
    nx = gds_data(2)
    ny = gds_data(3)
end subroutine get_pdsgds

!----------------------------------------------------------------------------                     
! icing_algo()
! Using the same algorithm as the the one used for  master file in grib2
subroutine calc_icing(gds, model)
    implicit none
    integer, intent(in) :: gds(:)
    type(input_data_t), intent(inout) :: model ! arrays to hold input data

    ! fields for calculate icing
    real, dimension(:,:,:), allocatable :: vvel, cwat, sev
    real, dimension(:,:), allocatable :: cape, cin, xcp, xacp, xalt
    ! for gdswiz, (lat,lon) in degree
    real, dimension(:,:), allocatable :: lat, lon, xpts, ypts, dummy
   
    character(len=*), parameter :: myself = 'calc_icing():'
    
    real :: accum_hour ! accumulation hours for precipitation

    integer :: nx, ny, np
    integer :: i, j, k, p, iret

    type(pds_t) :: pds_vvel = pds_t(39, 100)
    ! pds_cld_w is defined as a paramenter at the beginning
    type(pds_t) :: pds_cape = pds_t(157, 116)
    type(pds_t) :: pds_cin = pds_t(156, 116)
    type(pds_t) :: pds_xcp = pds_t(61, 1)
    type(pds_t) :: pds_xacp = pds_t(63, 1)
    type(pds_t) :: pds_xalt = pds_t(7, 1)                                                         
    
    integer :: pds7_cape = 65280

    nx = model%nx
    ny = model%ny
    np = model%np

    allocate(cwat(nx,ny,np))
    allocate(vvel(nx,ny,np))
    ! severity is dummy now
    allocate(sev(nx,ny,np))
    
    allocate(cape(nx,ny))
    allocate(cin(nx,ny))
    allocate(xcp(nx,ny))
    allocate(xacp(nx,ny))
    allocate(xalt(nx,ny))

    allocate(lat(nx,ny))
    allocate(lon(nx,ny))

    ! get latitude, longitude
    allocate(xpts(nx,ny))
    allocate(ypts(nx,ny))
    allocate(dummy(nx,ny))
    call gdswiz(gds, 0, nx*ny, -9999.0, xpts, ypts, lon, lat, iret, 0, &
        dummy, dummy)
    deallocate(xpts)
    deallocate(ypts)
    deallocate(dummy)

    ! read data from input file
    ! 2D fields
    call get_grib1(pds_cape, pds7_cape, nx, ny, cape, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve cape, iret = ', iret
    call get_grib1(pds_cin,  pds7_cape, nx, ny, cin, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve cin, iret = ', iret
    call get_grib1(pds_xcp,  0, nx, ny, xcp, iret, accum_hour)
    if (iret /= 0) print *, myself, &
        'failed to retrieve xcp, iret = ', iret
    call get_grib1(pds_xacp,  0, nx, ny, xacp, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve xacp, iret = ', iret
    call get_grib1(pds_xalt,  0, nx, ny, xalt, iret)
    if (iret /= 0) print *, myself, &
        'failed to retrieve xalt, iret = ', iret
    ! 3D fields
    do k = 1, np
       p = model%p(k)
       call get_grib1(pds_vvel, p, nx, ny, vvel(:,:,k), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve VVEL at ', p, &
            ' hPa, iret = ', iret
       call get_grib1(pds_cld_w, p, nx, ny, cwat(:,:,k), iret)
        if (iret /= 0) print *, myself, 'failed to retrieve CLW at ', p, &
            ' hPa, iret = ', iret
    end do

    ! calculate icing
    ! pressure for icing algorithm is in Pa, not hPa.
    do j = 1, ny
      do i = 1, nx
        call icing_algo(i, j, 100.*model%p(:), model%t(i,j,:), model%rh(i, j,:), &
             model%hgt(i,j,:),cwat(i, j,:),vvel(i, j,:),np, lat(i,j),lon(i,j), &
             xalt(i,j),xcp(i,j),xacp(i,j),cape(i,j),cin(i,j),accum_hour, &
             model%pot(i,j,:), sev(i,j,:))
      end do
    end do

    deallocate(cwat)
    deallocate(vvel)
    deallocate(sev)

    deallocate(cape)
    deallocate(cin)
    deallocate(xcp)
    deallocate(xacp)
    deallocate(xalt)

    deallocate(lat)
    deallocate(lon)
end subroutine calc_icing

end module waf_grib1
