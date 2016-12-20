! waf_grib2.f90
! grib2 IO
! Yali Mao, IMSG/EMC/NCEP, August 2014
module waf_grib2

USE GRIB_MOD

use getoptions
use kinds
use waf_glob
use waf_phys

implicit none

private
public write_output_data2, get_input_data2, get_gfld, convert_pdt2gds 

! values of product def template needed to read data from GRIB 2 file
type pdt_t
    integer :: npdt   ! number of template 4
    integer :: icat   ! catogory 
    integer :: iprm   ! parameter
    integer :: ilev   ! type of level (code table 4.5)
end type pdt_t

! PDT parameters in the input GRIB2 file (template 4 number, category, parameter, type of level)
type(pdt_t), parameter :: &
    pdt_sfc_pres = pdt_t(0, 3, 0, 1), &
    pdt_hgt      = pdt_t(0, 3,  5, 100), &
    pdt_temp     = pdt_t(0, 0,  0, 100), &
    pdt_u_wnd    = pdt_t(0, 2,  2, 100), &
    pdt_v_wnd    = pdt_t(0, 2,  3, 100), &
    pdt_rh       = pdt_t(0, 1,  1, 100), &
    pdt_cld_w    = pdt_t(0, 1, 22, 100), &
    pdt_conv_pres_bot = pdt_t(0, 3, 0, 242), &
    pdt_conv_pres_top = pdt_t(0, 3, 0, 243), &
    pdt_conv_pcp_rate = pdt_t(8, 1, 196, 1), &
!    pdt_icng_potential= pdt_t(0, 1, 206, 100)!, &
    pdt_icng_potential= pdt_t(0, 19, 20, 100)!, &
!   pdt_icng_severity = pdt_t()

contains

!----------------------------------------------------------------------------
subroutine put_grib2(parms, nlevel, gfld, nx, ny, fld, iret)
! basically the same as putgb2, but with flexible template 4 and template 5
! writes calculated values for one field at all pressure levels
    implicit none
    type(gparms_t), intent(in) :: parms    ! grib2 parameters of template 4 & 5
    integer, intent(in) :: nlevel          ! pressure level in Pa, integer
    type(gribfield), intent(in) :: gfld    ! a sample input carrying information
    integer, intent(in) :: nx, ny
    real(4), intent(in) :: fld(nx, ny)     ! the data to be written
    integer, intent(out) :: iret           ! return status code  

    CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CGRIB
    integer(4) :: lcgrib, lengrib
    integer :: listsec0(2)
    integer :: igds(5)
    real    :: coordlist=0.0
    integer :: ilistopt=0
    ! flexible arrays of template 4, 5
    integer, allocatable :: ipdtmpl(:), idrtmpl(:)
    logical(kind=1), dimension(nx,ny) :: bmap
    integer :: ibmap ! indicator whether to use bitmap

    character(len=*), parameter :: myself = 'put_grib2(): '

!   ALLOCATE ARRAY FOR GRIB2 FIELD
    lcgrib=gfld%ngrdpts*4
    allocate(cgrib(lcgrib),stat=iret)
    if ( iret.ne.0 ) then
       print *, myself, iret
       iret=2
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CREATE NEW MESSAGE
     listsec0(1)=gfld%discipline
     listsec0(2)=gfld%version
     if ( associated(gfld%idsect) ) then
        call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,iret)
        if (iret .ne. 0) then
            write(*,*) myself, ' ERROR creating new GRIB2 field = ',iret
        endif
     else
        print *, myself, ' No Section 1 info available. '
        iret=10
        deallocate(cgrib)
        return
     endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ADD GRID TO GRIB2 MESSAGE (Grid Definition Section 3)
      igds(1)=gfld%griddef    ! Source of grid definition (see Code Table 3.0)
      igds(2)=gfld%ngrdpts    ! Number of grid points in the defined grid.
      igds(3)=gfld%numoct_opt ! Number of octets needed for each additional grid points definition
      igds(4)=gfld%interp_opt ! Interpretation of list for optional points definition (Code Table 3.11)
      igds(5)=gfld%igdtnum    ! Grid Definition Template Number (Code Table3.1)
      if ( associated(gfld%igdtmpl) ) then
         call addgrid(cgrib, lcgrib, igds, gfld%igdtmpl, gfld%igdtlen,&
                     ilistopt, gfld%num_opt, iret)
         if (iret.ne.0) then
            write(*,*) myself, ' ERROR adding grid info = ',iret
         endif
      else
         print *, myself, ' No GDT info available. '
         iret=11
         deallocate(cgrib)
         return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ADD DATA FIELD TO GRIB2 MESSAGE
      ! template 4
      if( parms%npdt == 0 .or. parms%npdt == 7) then
         allocate(ipdtmpl(15))
      else
         allocate(ipdtmpl(18))
         ipdtmpl(16) = parms%stat
         ipdtmpl(17) = 3
         ipdtmpl(18) = 1
      endif
      ipdtmpl(1:15) = gfld%ipdtmpl(1:15)
      ipdtmpl(1)    = parms%icat
      ipdtmpl(2)    = parms%iprm
      ipdtmpl(10)   = parms%ilev
      ipdtmpl(12)   = nlevel
      ! template 5
      if( parms%ndrt == 40) then
         allocate(idrtmpl(7))
      endif
      idrtmpl(1) = 0 ! Any value. Will be overwritten
      idrtmpl(2) = parms%drt2
      idrtmpl(3) = parms%drt3
      idrtmpl(4) = parms%drt4
      idrtmpl(5) = 0
      idrtmpl(6) = 0
      idrtmpl(7) = 255
      ! bitmap
      if (parms%bitmap) then
         bmap = fld /= parms%msng
         ibmap = 0
      else
         bmap = .false.
         ibmap = 255
      end if
      ! call addfield
      call addfield(cgrib, lcgrib, parms%npdt, ipdtmpl, & 
                    size(ipdtmpl), coordlist, gfld%num_coord, &
                    parms%ndrt, idrtmpl, size(idrtmpl), &
                    fld, gfld%ngrdpts, ibmap, bmap, iret)
      if (iret .ne. 0) then
         write(*,*) myself, 'ERROR adding data field = ',iret
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CLOSE GRIB2 MESSAGE AND WRITE TO FILE
      call gribend(cgrib, lcgrib, lengrib, iret)
      call wryte(glob_lu_out, lengrib, cgrib)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      deallocate(cgrib)
      deallocate(ipdtmpl)
      deallocate(idrtmpl)
      RETURN
end subroutine put_grib2

!----------------------------------------------------------------------------
subroutine write_output_data2(products, cfg, gfld, waf, iret)
! writes output data to GRIB file
    implicit none
    type(product_t), intent(in) :: products ! requested products
    type(cfg_t), intent(in) :: cfg   ! configuration parameters
    type(gribfield), intent(in) :: gfld    ! a sample input carrying information
    type(output_data_t), intent(in) :: waf ! products to write
    integer, intent(out) :: iret ! return code, 0 on success

    integer :: nx, ny, lvl, p

    nx = waf%nx
    ny = waf%ny
    iret = 0
    if (products%do_cb) then
        if (iret == 0) call put_grib2(cfg%cb_hgt_bot_gparms, &
            0, gfld, nx, ny, waf%cb_hgt_bot, iret)
        if (iret == 0) call put_grib2(cfg%cb_hgt_top_gparms, &
            0, gfld, nx, ny, waf%cb_hgt_top, iret)
!        if (iret == 0) call put_grib2(cfg%cb_embd_hgt_bot_gparms, &
!            0, gfld, nx, ny, waf%cb_embd_hgt_bot, iret)
!        if (iret == 0) call put_grib2(cfg%cb_embd_hgt_top_gparms, &
!            0, gfld, nx, ny, waf%cb_embd_hgt_top, iret)
        if (iret == 0) call put_grib2(cfg%cb_cover_gparms, &
            0, gfld, nx, ny, waf%cb_cover, iret)
    end if
    if (products%do_tcld) then
        do lvl = 1, waf%num_tcld_lvls
            p = waf%tcld_lvls(lvl)*100 ! pressure level is Pa in grib2  
            call put_grib2(cfg%tcld_mean_gparms, p, gfld, nx, ny, &
                waf%tcld_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib2(cfg%tcld_max_gparms, p, gfld, nx, ny, &
                waf%tcld_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
    if (products%do_cat) then
        do lvl = 1, waf%num_cat_lvls
            p = waf%cat_lvls(lvl)*100 ! pressure level is Pa in grib2 
            call put_grib2(cfg%cat_mean_gparms, p, gfld, nx, ny, &
                waf%cat_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib2(cfg%cat_max_gparms, p, gfld, nx, ny, &
                waf%cat_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
    if (products%do_icng) then
        do lvl = 1, waf%num_icng_lvls
            p = waf%icng_lvls(lvl)*100 ! pressure level is Pa in grib2 
            call put_grib2(cfg%icng_mean_gparms, p, gfld, nx, ny, &
                waf%icng_mean(:,:,lvl), iret)
            if (iret /= 0) exit
            call put_grib2(cfg%icng_max_gparms, p, gfld, nx, ny, &
                waf%icng_max(:,:,lvl), iret)
            if (iret /= 0) exit
        end do
    end if
end subroutine write_output_data2

!----------------------------------------------------------------------------
subroutine get_grib2(pdt, pres_level, iret, fld, nx, ny, gfld_def)
! nx ny gfld_def as one group of output; fld as another 
! function 1: if input nx/ny == -1, output nx ny gfld_def as an initialization
! function 2: retrieves one field, fld, specified by pdt at pressure level p
    implicit none
    type(pdt_t), intent(in) :: pdt
    integer, intent(in) :: pres_level ! pressure level in Pa
    integer, intent(out) :: iret
    real, intent(out), optional :: fld(:,:)
    integer, intent(out), optional :: nx, ny
    ! saved for output, will be freed after finishing writing
    type(gribfield), intent(out), optional :: gfld_def  

    type(gribfield) :: gfld
    integer j,jdisc,jpdtn,jgdtn
    integer,dimension(200) :: jids,jpdt,jgdt
    logical :: unpack

    j        = 0          ! search from 0
    jdisc    = 0          ! for met field:0 hydro: 1, land: 2
    jids(:)  = -9999
    !-- set product defination template 4
    jpdtn    = pdt%npdt   ! number of product defination template 4
    jpdt(:)  = -9999
    jpdt(1)  = pdt%icat   ! category 
    jpdt(2)  = pdt%iprm   ! parameter number
    jpdt(10) = pdt%ilev   ! type of level (code table 4.5)
    jpdt(12) = pres_level ! level value
    !-- set grid defination template/section 3
    jgdtn    = -1  
    jgdt(:)  = -9999
    unpack=.true.
    ! Get field from file
    call getgb2(glob_lu_in, 0, j, jdisc, jids, jpdtn, jpdt, &
                jgdtn, jgdt, unpack, j, gfld, iret)
    if( iret /= 0) then
       print *,'call get_grib2, iret=',iret, pdt,"on level=",pres_level 
    endif

    if(present(nx) .and. present(ny) .and. present(gfld_def)) then
       nx = gfld%igdtmpl(8)
       ny = gfld%igdtmpl(9)
       gfld_def = gfld
     elseif(present(fld)) then
       fld = reshape(gfld%fld, (/ gfld%igdtmpl(8), gfld%igdtmpl(9) /))
     else
       print *, 'call get_grib2 without proper optional arguments'
    endif

    call gf_free(gfld)
end subroutine get_grib2

!----------------------------------------------------------------------------
subroutine get_input_data2(cfg, model)
! reads input data
    implicit none
    type(cfg_t), intent(in) :: cfg ! parameters read from cfg file
    type(input_data_t), intent(inout) :: model ! arrays to hold input data

    integer :: lvl, p, iret, nx, ny
    real(kind=r_kind) :: fp
    real(kind=r_kind), dimension(:,:), allocatable :: buf1, buf2, buf3
    character(len=*), parameter :: myself = 'get_input_data2(): '

    nx = model%nx
    ny = model%ny
    call get_grib2(pdt_sfc_pres, 0, iret, fld=model%sfc_pres)
    if (iret /= 0) print *, myself, &
        'failed to retrieve sfc pres, iret = ', iret
    allocate(buf1(nx,ny))
    allocate(buf2(nx,ny))
    allocate(buf3(nx,ny))
    call get_grib2(pdt_conv_pres_bot, 0, iret, fld=buf1)
    if (iret /= 0) print *, myself, &
        'failed to retrieve pres at bottom of conv cloud, iret = ', iret
    call get_grib2(pdt_conv_pres_top, 0, iret, fld=buf2)
    if (iret /= 0) print *, myself, &
        'failed to retrieve pres at top of conv cloud, iret = ', iret
    call get_grib2(pdt_conv_pcp_rate, 0, iret, fld=buf3)
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
        p = model%p(lvl)*100 ! pressure level is Pa in grib2
        call get_grib2(pdt_hgt, p, iret, fld=model%hgt(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve Z at ', p, &
            ' hPa, iret = ', iret
        call get_grib2(pdt_temp, p, iret, fld=model%t(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve T at ', p, &
            ' hPa, iret = ', iret
        call get_grib2(pdt_u_wnd, p, iret, fld=model%u_wnd(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve U at ', p, &
            ' hPa, iret = ', iret
        call get_grib2(pdt_v_wnd, p, iret, fld=model%v_wnd(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve V at ', p, &
            ' hPa, iret = ', iret
        call get_grib2(pdt_rh, p, iret, fld=model%rh(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve RH at ', p, &
            ' hPa, iret = ', iret
        call get_grib2(pdt_cld_w, p, iret, fld=buf1) 
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
        call get_grib2(pdt_icng_potential, p, iret, fld=model%pot(:,:,lvl))
        if (iret /= 0) print *, myself, 'failed to retrieve icing potential at', p, &
            ' hPa, iret = ', iret
        where(model%pot(:,:,lvl) < -999.)
            model%pot(:,:,lvl) = 0.0
        end where
!        call get_grib2(pdt_icng_severity, p, iret, fld=model%sev(:,:,lvl)) 
!        if (iret /= 0) print *, myself, 'failed to retrieve icing severity at', p, &
!            ' hPa, iret = ', iret
    end do
    deallocate(buf1)
    deallocate(buf2)
    deallocate(buf3)
end subroutine get_input_data2

!----------------------------------------------------------------------------
subroutine get_gfld(nx, ny, gfld, iret)
    implicit none
    integer, intent(out) :: nx, ny
    type(gribfield), intent(out) :: gfld
    integer, intent(out) :: iret

    call get_grib2(pdt_sfc_pres, 0, iret, nx=nx, ny=ny, gfld_def=gfld)
end subroutine get_gfld

!----------------------------------------------------------------------------
subroutine convert_pdt2gds(gfld, kgds)
    implicit none
    type(gribfield), intent(in) :: gfld
    integer, intent(out) :: kgds(:)

    ! prepare for gdt2gds() to get kgds used by gdswiz()
    integer :: igds(5)
    integer :: ideflist(1)
    integer :: igrid ! NCEP predefined GRIB1 grid number; 255 if not NCEP grid
    integer :: iret

    igds(1) = gfld%griddef
    igds(2) = gfld%ngrdpts
    igds(3) = gfld%numoct_opt
    igds(4) = gfld%interp_opt
    igds(5) = gfld%igdtnum
    CALL gdt2gds(igds, gfld%igdtmpl, 0, ideflist, kgds, igrid, iret)
end subroutine convert_pdt2gds

end module waf_grib2
