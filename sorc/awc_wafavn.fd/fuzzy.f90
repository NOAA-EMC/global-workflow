! fuzzy.f90
! fuzzy stuff
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 24/05/07

module fuzzy

use kinds
use cfgini

implicit none

public
private point_t

!-----------------------------------------------------------------------------
! constants
!-----------------------------------------------------------------------------
! maximum number of points in a fuzzy set. Must be <= cfg_max_items, defined
! in cfgini.f90
integer, parameter :: max_points = 32

!-----------------------------------------------------------------------------
! type definitions
!-----------------------------------------------------------------------------
type point_t
    real(kind=r_kind) :: x, y
end type point_t

type fuzzy_set_t
    type(point_t), dimension(max_points) :: p
    integer :: np
end type fuzzy_set_t

contains
!============================================================================
! evaluates membership of val in a fuzzy set fuzzy
elemental function fuzzy_member(f, val)
    real(kind=r_kind) fuzzy_member
    type(fuzzy_set_t), intent(in) :: f
    real(kind=r_kind), intent(in) :: val

    integer :: i
    real(kind=r_kind) :: delta, mem

    if (val <= f%p(1)%x) then
        mem = 0.0
    else if (val >= f%p(f%np)%x) then
        mem = 0.0
    else
        do i = 2, f%np
            if (val < f%p(i)%x) then
                delta = f%p(i)%x -  f%p(i-1)%x
                if (delta <= 0.0) then
                    mem = f%p(i-1)%y
                else
                    mem = (f%p(i)%y * (val-f%p(i-1)%x) + &
                        f%p(i-1)%y * (f%p(i)%x-val)) / delta
                end if
                exit
            end if
        end do
    end if
    fuzzy_member = mem
end function fuzzy_member

!----------------------------------------------------------------------------
! evaluates membership of val in a fuzzy set fuzzy. Assumes f is in x-log scale
elemental function fuzzy_log_member(f, val)
    real(kind=r_kind) fuzzy_log_member
    type(fuzzy_set_t), intent(in) :: f
    real(kind=r_kind), intent(in) :: val

    if (val <= 0.0) then
        fuzzy_log_member = 0.0
    else
        fuzzy_log_member = fuzzy_member(f, log(val))
    end if
end function fuzzy_log_member

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! a fuzzy set defined in a section with the tag sect_tag. If there is no such
! section, or section format is wrong iret is set to 1
! the format of the relevant section is:
! [section-tag]
! point x-value y-value
!  . . . . . . . . . . 
! point x-value y-value
subroutine fuzzy_from_config(sect_tag, sections, fuzzy, iret)
    character(len=*), intent(in) :: sect_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    type(fuzzy_set_t), intent(out) :: fuzzy
    integer, intent(out) :: iret

    integer :: i, num_points
    real(kind=r_kind) :: y
    character(len=cfg_tag_len), dimension(max_points) :: tags
    character(len=cfg_arg_len), dimension(max_points) :: values
    character(len=*), parameter :: me = 'fuzzy_from_config(): '

    num_points = max_points
    call cfg_get_all_items(sect_tag, sections, tags, values, num_points, iret)
    if (iret /= 0) return
    do i = 1, num_points
        if (tags(i) /= 'point') then
            print *, me, 'Invalid item', tags(i)
            cycle
        end if
        read(values(i), *, iostat=iret) fuzzy%p(i)%x, fuzzy%p(i)%y
        if (iret /= 0) then
            print *, me, 'invalid coordinates ', trim(values(i))
            return
        end if
    end do
    ! sanity check
    do i = 2, num_points
        if (fuzzy%p(i)%x < fuzzy%p(i-1)%x) then
            print *, me, 'x coordinates for ', sect_tag, ' must be increasing'
            iret = 1
            exit
        end if
    end do
    fuzzy%np = num_points
end subroutine fuzzy_from_config

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! a fuzzy set defined in a section with the tag sect_tag. If there is no such
! section, or section format is wrong iret is set to 1
! the format of the relevant section is:
! [section-tag]
! point x-value y-value
!  . . . . . . . . . . 
! point x-value y-value
! x_values must be positive
subroutine fuzzy_log_from_config(sect_tag, sections, fuzzy, iret)
    character(len=*), intent(in) :: sect_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    type(fuzzy_set_t), intent(out) :: fuzzy
    integer, intent(out) :: iret

    integer :: i, nx, ny
    real :: x, y
    character(len=*), parameter :: me = 'fuzzy_log_from_config(): '

    call fuzzy_from_config(sect_tag, sections, fuzzy, iret)
    if (iret /= 0) return
    do i = 1, fuzzy%np
        if (fuzzy%p(i)%x <= 0.0) then
            print *, me, 'x coordinates for ', sect_tag, ' must be positive'
            iret = 1
            exit
        end if
        fuzzy%p(i)%x = log(fuzzy%p(i)%x)
    end do
end subroutine fuzzy_log_from_config

end module fuzzy
