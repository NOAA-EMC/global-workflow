! waf_main.f90
! contains subroutines to calculate icing
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 11/05/09
! 08/20/2014 Yali Mao
! 1) The subroutine of icng_alg will not be used. Instead, read in icing potential
! directly from master file and use subroutine of icng_alg_read to do MEAN/MAX
! 2) input file can be grib1 and grib2
module waf_main

USE GRIB_MOD

use getoptions
use kinds
use waf_glob

implicit none

public

! default values for testing
character(len=*), parameter :: def_cfg_file = 'waf.cfg'
character(len=*), parameter :: def_output_file = 'waf.grib2'

contains
!----------------------------------------------------------------------------
subroutine usage()
! prints proper usage
    character(len=256) :: progname

    call getarg(0, progname)
    print *, 'Usage: ', trim(progname), &
        ' [-g grib -c config-file -o output-file] -i input-file product ...'
    print *, 'The defaults are:'
    print *, 'grib: ', '2'
    print *, 'config-file: ', def_cfg_file
    print *, 'output-file: ', def_output_file
    print *, 'Products:'
    print *, '\t1: cat - Clear Area Turbulence'
    print *, '\t2: tcld - In-cloud turbulence'
    print *, '\t3: cb - Cumulonimbus'
    print *, '\t4: icng - Icing potential'
end subroutine usage

!----------------------------------------------------------------------------
subroutine prog_args(grib, cfg_file, input_file, output_file, products, iret)
! parses program arguments
    character(len=1), intent(out) :: grib     ! grib 1 or 2 of input data file
    character(len=*), intent(out) :: cfg_file ! configuration file
    character(len=*), intent(out) :: input_file ! input data file (from npost)
    character(len=*), intent(out) :: output_file ! output data file (GRIB)
    type(product_t), intent(out) :: products ! requested products
    integer, intent(out) :: iret ! return code, 0 on success

    character :: okey
    character(len=*), parameter :: options = 'g:c:i:o:'

    ! set defaults
    grib = "2"
    cfg_file = def_cfg_file
    output_file = def_output_file
    input_file = ' '
    products%do_cat = .false.
    products%do_icng = .false.
    products%do_cb = .false.
    products%do_tcld = .false.
    ! process command arguments
    iret = 0
    do
        okey = getopt(options)
        select case (okey)
        case ('>')
            exit
        case ('g')
            grib = optarg
        case ('c')
            cfg_file = optarg
        case ('i')
            input_file = optarg
        case ('o')
            output_file = optarg
        case ('.')
            select case(trim(optarg))
            case ('cb')
                products%do_cb = .true.
            case ('cat')
                products%do_cat = .true.
            case ('tcld')
                products%do_tcld = .true.
            case ('icng')
                products%do_icng = .true.
            case default
                iret = 1
                exit
            end select
        case default
            iret = 1
            exit
        end select
    end do
    if (iret /= 0 .or. input_file == ' ' .or. .not. products%do_cb &
        .and. .not. products%do_cat .and. .not. products%do_tcld &
        .and. .not. products%do_icng) then
        iret = 1
        call usage()
    endif
end subroutine prog_args

!----------------------------------------------------------------------------
subroutine calc_grid_parms(gds, nx, ny, dx, dy, f)
! calculates grid sizes and Coriolis force on lat-lon or Gaussian grid
    integer, dimension(:), intent(in) :: gds ! GDS section data
    integer, intent(in) :: nx, ny ! grid dimensions
    real(kind=r_kind), dimension(nx,ny), intent(out) :: dx, dy, f ! grid 
        ! size [m], Coriolis parameter

    integer :: i, j, k, kx, ky, nret
    real :: fi
    real, dimension(nx*ny) :: xpts, ypts, rlat, rlon, dummy
    real, parameter :: d2r = con_pi/180.0

    call gdswiz(gds, 0, nx*ny, -9999.0, xpts, ypts, rlon, rlat, nret, 0, &
        dummy, dummy)
    if (nret /= nx*ny) stop 98 ! will never happen, I hope
    do k = 1, nx*ny
        fi = d2r*rlat(k)
        i = int(xpts(k))
        j = int(ypts(k))
        f(i,j) = 2.0*con_omega*sin(fi)
        if (i < nx) then
            kx = k+1
        else    ! wrap 
            kx = k-nx+1
        end if
        dx(i,j) = con_rerth*cos(fi)*abs(mod(rlon(kx)-rlon(k), 360.0))*d2r
        if (j < ny) then
            ky = k+nx   ! assumes order, should check whether ypts(ky) = j+1
            dy(i,j) = con_rerth*abs(mod(rlat(ky)-rlat(k), 360.0))*d2r
        end if
    end do
!    print *, '======dx', dx(1:4,1)
!    print *, '======dx', dx(1:4,ny/2)
!    print *, '======dy', dy(1:4,1)
!    print *, '======dy', dy(1:4,ny/2)
!    print *, '======f', f(1:4,1)
!    print *, '======f', f(1:4,ny/2)
!    stop 97
end subroutine calc_grid_parms

!----------------------------------------------------------------------------
subroutine alloc_input_storage(nx, ny, cfg, model)
! allocates arrays allocated for model data storage
    integer, intent(in) :: nx, ny
    type(cfg_t), intent(in) :: cfg   ! configuration parameters
    type(input_data_t), intent(inout) :: model ! arrays to hold input data

    integer :: i, np

    np = (cfg%pres_bot - cfg%pres_top)/glob_delta_p + 1
    model%np = np
    do i = 1, np
        model%p(i) = cfg%pres_bot - (i-1)*glob_delta_p
    end do
    allocate(model%dx(nx,ny))
    allocate(model%dy(nx,ny))
    allocate(model%f(nx,ny))
    allocate(model%sfc_pres(nx,ny))
    model%sfc_pres = glob_msng
    allocate(model%hgt(nx,ny,np))
    model%hgt = glob_msng
    allocate(model%t(nx,ny,np))
    model%t = glob_msng
    allocate(model%u_wnd(nx,ny,np))
    model%u_wnd = glob_msng
    allocate(model%v_wnd(nx,ny,np))
    model%v_wnd = glob_msng
    allocate(model%rh(nx,ny,np))
    model%rh = glob_msng
    allocate(model%cld_cover(nx,ny,np))
    model%cld_cover = glob_msng
    allocate(model%conv_pres_bot(nx,ny))
    model%conv_pres_bot = glob_msng
    allocate(model%conv_pres_top(nx,ny))
    model%conv_pres_top = glob_msng
    allocate(model%conv_cld_cover(nx,ny))
    model%conv_cld_cover = glob_msng
    ! icing, it's special, available on selected pressure levels.
    allocate(model%pot(nx,ny,np))
    model%pot = glob_msng
!    allocate(model%sev(nx,ny,np))
!    model%sev = glob_msng
end subroutine alloc_input_storage

!----------------------------------------------------------------------------
subroutine free_input_storage(model)
! frees arrays allocated for model data storage
    type(input_data_t), intent(inout) :: model ! input data structure

    deallocate(model%dx)
    deallocate(model%dy)
    deallocate(model%f)
    deallocate(model%sfc_pres)
    deallocate(model%hgt)
    deallocate(model%t)
    deallocate(model%u_wnd)
    deallocate(model%v_wnd)
    deallocate(model%rh)
    deallocate(model%cld_cover)
    deallocate(model%conv_pres_bot)
    deallocate(model%conv_pres_top)
    deallocate(model%conv_cld_cover)
    deallocate(model%pot)
!    deallocate(model%sev)
end subroutine free_input_storage

!----------------------------------------------------------------------------
subroutine alloc_output_storage(products, nx, ny, cfg, waf)
    type(product_t), intent(in) :: products ! requested products
    integer, intent(in) :: nx, ny ! grid dimension
    type(cfg_t), intent(in) :: cfg   ! configuration parameters
    type(output_data_t), intent(out) :: waf ! output data structure

    integer :: ni, nc, nt

    waf%nx = nx
    waf%ny = ny
    ni = cfg%num_icng_lvls
    nc = cfg%num_cat_lvls
    nt = cfg%num_tcld_lvls
    waf%num_icng_lvls = ni
    waf%icng_lvls = cfg%icng_lvls
    waf%num_tcld_lvls = nt
    waf%tcld_lvls = cfg%tcld_lvls
    waf%num_cat_lvls = nc
    waf%cat_lvls = cfg%cat_lvls
    if (products%do_cb) then
        allocate(waf%cb_hgt_bot(nx,ny))
        allocate(waf%cb_hgt_top(nx,ny))
!        allocate(waf%cb_embd_hgt_bot(nx,ny))
!        allocate(waf%cb_embd_hgt_top(nx,ny))
        allocate(waf%cb_cover(nx,ny))
    end if
    if (products%do_icng) then
        allocate(waf%icng_mean(nx,ny,ni))
        allocate(waf%icng_max(nx,ny,ni))
    end if
    if (products%do_tcld) then
        allocate(waf%tcld_mean(nx,ny,nt))
        allocate(waf%tcld_max(nx,ny,nt))
    end if
    if (products%do_cat) then
        allocate(waf%cat_mean(nx,ny,nc))
        allocate(waf%cat_max(nx,ny,nc))
    end if
end subroutine alloc_output_storage

!----------------------------------------------------------------------------
subroutine free_output_storage(waf)
! frees arrays allocated for output data storage
    type(output_data_t), intent(inout) :: waf  ! output data structure

    if (allocated(waf%cb_hgt_bot)) deallocate(waf%cb_hgt_bot)
    if (allocated(waf%cb_hgt_top)) deallocate(waf%cb_hgt_top)
!    if (allocated(waf%cb_embd_hgt_bot)) &
!        deallocate(waf%cb_embd_hgt_bot)
!    if (allocated(waf%cb_embd_hgt_top)) &
!        deallocate(waf%cb_embd_hgt_top)
    if (allocated(waf%cb_cover)) deallocate(waf%cb_cover)

    if (allocated(waf%icng_mean)) deallocate(waf%icng_mean)
    if (allocated(waf%icng_max)) deallocate(waf%icng_max)

    if (allocated(waf%tcld_mean)) deallocate(waf%tcld_mean)
    if (allocated(waf%tcld_max)) deallocate(waf%tcld_max)

    if (allocated(waf%cat_mean)) deallocate(waf%cat_mean)
    if (allocated(waf%cat_max)) deallocate(waf%cat_max)
end subroutine free_output_storage

end module waf_main

!===========================================================================
program wafavn
    use funcphys
    use waf_main
    use waf_grib2
    use waf_grib1
    use waf_cb
    use waf_config
    use waf_tcld
    use waf_cat
    use waf_icng

    implicit none

    character(len=1) :: grib
    character(len=256) :: cfg_file, input_file, output_file ! file names
    type(product_t) :: products ! flags indicating product to make
    type(cfg_t) :: cfg ! parameters set in a configuration file
    integer :: iret ! generic return code from subroutines
    type(input_data_t) :: model_data ! input data structure
    type(output_data_t) :: waf_data ! output data structure

    ! for grib 2
    type(gribfield) :: gfld 
    ! for grib 1, gds is for grib 2 as well
    integer :: pds(200), gds(200)

    call prog_args(grib, cfg_file, input_file, output_file, products, iret)
    if (iret /= 0) stop 1
    call config_get_parms(grib, products, cfg_file, cfg, iret)
    if (iret /= 0) stop 2
    call baopenr(glob_lu_in, input_file, iret)
    if (iret /= 0) then
        print *, 'baopenr() failed on ', input_file(:len_trim(input_file)), &
        ', iret = ', iret
        stop 3
    end if
    call gfuncphys ! initialize funcphys package
    if (grib == "2") then
       call get_gfld(model_data%nx, model_data%ny, gfld, iret)
       write(*,*) "grib2 dimension =", model_data%nx, model_data%ny, iret
       call alloc_input_storage(model_data%nx, model_data%ny, cfg, model_data)
       call get_input_data2(cfg, model_data)
       call convert_pdt2gds(gfld, gds)
    else
       call get_pdsgds(model_data%nx, model_data%ny, pds, gds, iret)        
       write(*,*) "grib1 dimension =", model_data%nx, model_data%ny, iret
       if (iret /= 0) stop 4
       call alloc_input_storage(model_data%nx, model_data%ny, cfg, model_data)
       call get_input_data1(gds, cfg, model_data)
    endif
    call baclose(glob_lu_in, iret)
    call alloc_output_storage(products, model_data%nx, model_data%ny, &
                              cfg, waf_data)
    call calc_grid_parms(gds, model_data%nx, model_data%ny, &
                         model_data%dx, model_data%dy, model_data%f)
    if (products%do_cb) call cb_alg(cfg, model_data, waf_data)
    if (products%do_tcld) call tcld_alg(cfg, model_data, waf_data)
    if (products%do_cat) call cat_alg(cfg, model_data, waf_data)
    ! run read-icing-only version
    if (products%do_icng) call icng_alg_read(cfg, model_data, waf_data)
    call free_input_storage(model_data)
    if (grib == "2") then
       ! open write for grib 2 is different from for grib 1
       call baopenw(glob_lu_out, output_file, iret)
       if (iret /= 0) then
          print *, 'baopenw() failed on ', trim(output_file), ', iret = ', iret
          stop 6
       end if
       call write_output_data2(products, cfg, gfld, waf_data, iret)
       call gf_free(gfld)
    else
       call baopenwt(glob_lu_out, output_file, iret)
       if (iret /= 0) then
          print *, 'baopenwt() failed on ', trim(output_file), ', iret = ', iret
          stop 6
       end if
       call write_output_data1(products, cfg, pds, gds, waf_data, iret)
    endif
    call free_output_storage(waf_data)
    if (iret /= 0) stop 7
    call baclose(glob_lu_out, iret)
    stop 0
end program wafavn
