! waf_glob.f90
! global declarations
! George Trojan, SAIC/EMC/NCEP, December 2006
! Last update: 
! 11/05/09
! 08/13/2014 Read pot to input_data_t to do mean/max instead of calculating
!            icing potential

module waf_glob

use kinds
use physcons
use fuzzy

implicit none

public

! GRIB table 2 containing WAF aviation elements
integer, parameter :: glob_avn_table = 140
! FORTRAN logical units
integer, parameter :: glob_lu_in = 11, glob_lu_out = 12, glob_lu_cfg = 13
! pressure range
integer, parameter :: glob_max_lvls = 40
integer, parameter :: glob_delta_p = 25
! missing value for fields read from the model
real(kind=r_kind), parameter :: glob_msng = -999999.0 ! assumed < 0

type gparms_t
    ! values used to write data to GRIB 2 file
    integer :: npdt   ! number of template 4
    integer :: icat   ! catogory
    integer :: iprm   ! parameter
    integer :: ilev   ! type of level (code table 4.5)
    integer :: stat   ! TYPE OF STATISTICAL PROCESSING
    !
    integer :: ndrt   ! number of template 5
    integer :: drt2   ! Binary scale factor
    integer :: drt3   ! Decimal scale factor
    integer :: drt4   ! Number of bits to hold data

    ! values used to write data to GRIB 1 file
     integer :: pds5             ! pds(5): field number (GRIB table 2) 
    integer :: pds6             ! pds(6): field level type (GRIB table 3)
    integer :: pds22            ! pds(22): precision (number of digits)

    real(kind=r_kind) :: msng   ! missing data (below surface)
    logical :: bitmap           ! whether to use bitmap for sparse data

end type gparms_t

type cell_t  ! 3d grid size to calculate mean and max values
    real(kind=r_kind) :: dp    ! vertical size (hPa)
    real(kind=r_kind) :: dxdy  ! horizontal size (km)
end type cell_t

! structure to hold input data
type input_data_t
    integer :: nx, ny, np
    integer, dimension(glob_max_lvls) :: p ! in hPa
    real(kind=r_kind), dimension(:,:), allocatable :: dx, dy ! grid size
    real(kind=r_kind), dimension(:,:), allocatable :: f ! Coriolis
    real(kind=r_kind), dimension(:,:), allocatable :: sfc_pres, &
        conv_pres_bot, conv_pres_top, conv_pcp_rate, conv_cld_cover
    real(kind=r_kind), dimension(:,:,:), allocatable :: hgt, t, u_wnd, &
        v_wnd, rh, cld_cover
    ! read in pot to do mean/max instead of calculating
    real(kind=r_kind), dimension(:,:,:), allocatable :: pot!, sev ! icing 
end type input_data_t

! structure to hold output data
type output_data_t
    integer :: nx, ny, num_icng_lvls, num_tcld_lvls, num_cat_lvls
    integer, dimension(glob_max_lvls) :: tcld_lvls, cat_lvls, icng_lvls
    real(kind=r_kind), dimension(:,:,:), allocatable :: icng_mean, icng_max
    real(kind=r_kind), dimension(:,:,:), allocatable :: cat_mean, cat_max
    real(kind=r_kind), dimension(:,:,:), allocatable :: tcld_mean, tcld_max
    real(kind=r_kind), dimension(:,:), allocatable :: cb_hgt_bot, cb_hgt_top, &
        cb_cover
!        cb_embd_hgt_bot, cb_embd_hgt_top, cb_cover
end type output_data_t

type cfg_t
    integer :: pres_bot, pres_top   ! pressure range to read from input file
    type(fuzzy_set_t) :: pcp2cover  ! convective cloud cover from pcp rate
    ! icing
    type(gparms_t) :: icng_mean_gparms, icng_max_gparms
    integer :: num_icng_lvls     ! vertical levels
    integer, dimension(glob_max_lvls) :: icng_lvls
    type(cell_t) :: icng_cell ! for averaging
    ! in-cloud turbulence 
    type(gparms_t) :: tcld_mean_gparms, tcld_max_gparms
    integer :: num_tcld_lvls     ! vertical levels
    integer, dimension(glob_max_lvls) :: tcld_lvls
    type(cell_t) :: tcld_cell ! for averaging
    real(kind=r_kind) :: tcld_min_cld_cover ! between 0 and 1
    real(kind=r_kind) :: tcld_nocld ! small negative
    ! CAT
    type(gparms_t) :: cat_mean_gparms, cat_max_gparms
    integer :: num_cat_lvls     ! vertical levels
    integer, dimension(glob_max_lvls) :: cat_lvls
    real(kind=r_kind), dimension(glob_max_lvls) :: cat_thresh
    type(cell_t) :: cat_cell ! for averaging
    ! CB parameters
    type(gparms_t) :: cb_cover_gparms, cb_hgt_bot_gparms, cb_hgt_top_gparms
!    type(gparms_t) :: cb_cover_gparms, cb_hgt_bot_gparms, cb_hgt_top_gparms, &
!        cb_embd_hgt_bot_gparms, cb_embd_hgt_top_gparms
    real(kind=r_kind) :: cb_min_depth, cb_min_top, cb_low_cld_cover
    type(cell_t) :: cb_cell ! for averaging
end type cfg_t

! products to made, set in prog_args()
type product_t
    logical :: do_cb
    logical :: do_icng
    logical :: do_cat
    logical :: do_tcld
end type product_t

contains
!----------------------------------------------------------------------------
function glob_find_index(v, array, np)
! used to get model level given forecast field level
    integer :: glob_find_index
    integer, intent(in) :: v ! value to find
    integer, dimension(:), intent(in) :: array ! in this array
    integer, intent(in) :: np ! of this size

    integer :: i

    do i = 1, np
        if (array(i) == v) then
            glob_find_index = i
            return
        end if
    end do
    glob_find_index = 0
end function glob_find_index
    
end module waf_glob
