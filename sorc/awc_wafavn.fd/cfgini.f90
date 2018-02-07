! cfgini.f90
! functions to parse INI type configuration files
! George Trojan, SAIC/EMC/NCEP, January 2007
! Last update: 03/07/07

! ini configuration file has the following format
!
! # a comment
! [section-tag]
! item-tag value1 value2 ...
! item-tag value ...
! blank line
! [section-tag]
! . . . .

module cfgini

use kinds
use tokenize

implicit none

private
public cfg_arg_len, cfg_tag_len, cfg_sect_t
public cfg_get_sect, cfg_get_all_items, cfg_get_item, cfg_read_file

!-----------------------------------------------------------------------------
! constants
!-----------------------------------------------------------------------------
integer, parameter :: cfg_tag_len = 64
integer, parameter :: cfg_arg_len = 256
integer, parameter :: cfg_max_tokens = 16
integer, parameter :: cfg_token_len = 16

integer, parameter :: max_items = 32

!-----------------------------------------------------------------------------
! type definitions
!-----------------------------------------------------------------------------
type item_t
    character(len=cfg_tag_len) :: tag
    character(len=cfg_arg_len) :: arg
end type item_t

type cfg_sect_t
    character(len=cfg_tag_len) :: tag
    type(item_t), dimension(max_items) :: items
    integer :: nitems
end type cfg_sect_t

!-----------------------------------------------------------------------------
! interfaces
!-----------------------------------------------------------------------------
interface cfg_get_item
    module procedure cfg_get_string_cfg, cfg_get_string_sect, &
        cfg_get_strings_cfg, cfg_get_strings_sect, &
        cfg_get_integer_cfg, cfg_get_integer_sect, &
        cfg_get_integers_cfg, cfg_get_integers_sect, &
        cfg_get_real_cfg, cfg_get_real_sect, &
        cfg_get_reals_cfg, cfg_get_reals_sect, &
        cfg_get_boolean_cfg, cfg_get_boolean_sect
end interface cfg_get_item

contains
!============================================================================
! returns index to the first non-blank character in a string, 0 if there is
! no blank
function first_nonblank(buffer)
    integer first_nonblank
    character(len=*), intent(in) :: buffer

    integer :: i, n

    n = 0
    do i = 1, len(buffer)
        select case (buffer(i:i))
        case (' ', '\t', '\r', '\n')
            cycle
        case default
            n = i
            exit
        end select
    end do
    first_nonblank = n
end function first_nonblank

!----------------------------------------------------------------------------
! public
!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! a section with the tag sect_tag. If there is no such section iret is set to 1
subroutine cfg_get_sect(sect_tag, sections, sect, iret)
    character(len=*), intent(in) :: sect_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    type(cfg_sect_t), intent(out) :: sect
    integer, intent(out) :: iret

    integer :: i
    character(len=*), parameter :: me = 'cfg_get_sect(): '

    iret = 1
    do i = 1, size(sections)
        if (sect_tag == sections(i)%tag) then
            sect = sections(i)
            iret = 0
            return
        end if
    end do
    print *, me, 'Cannot find section ', sect_tag
end subroutine cfg_get_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! list of items (strings) and their tags in a section with the tag sect_tag. 
! If there is no such section iret is set to 1
subroutine cfg_get_all_items(sect_tag, sections, tags, values, num_items, iret)
    character(len=*), intent(in) :: sect_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    character(len=*), dimension(:), intent(out) :: tags, values
    integer, intent(inout) :: num_items ! number of elements in tags and values
    integer, intent(out) :: iret

    integer :: i
    type(cfg_sect_t) :: sect
    character(len=*), parameter :: me = 'cfg_get_all_items(): '

    call cfg_get_sect(sect_tag, sections, sect, iret)
    if (iret /= 0) return
    num_items = min(num_items, sect%nitems)
    do i = 1, num_items
        tags(i) = sect%items(i)%tag
        values(i) = sect%items(i)%arg
    end do
end subroutine cfg_get_all_items
    
!----------------------------------------------------------------------------
! retrieves item value (string) in for item with a tag item_tag in a section
! section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_string_sect(item_tag, section, cval, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    character(len=*), intent(out) :: cval
    integer, intent(out) :: iret

    integer :: j
    character(len=*), parameter :: me = 'cfg_get_string_sect(): '

    iret = 1
    do j = 1, section%nitems 
        if (item_tag == section%items(j)%tag) then
            cval = section%items(j)%arg
            iret = 0
            return
        end if
    end do
    print *, me, 'Cannot find item ', item_tag
end subroutine cfg_get_string_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (string) in for item with a tag item_tag in a section with a tag 
! sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_string_cfg(sect_tag, item_tag, sections, cval, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    character(len=*), intent(out) :: cval
    integer, intent(out) :: iret

    integer :: i, j
    character(len=*), parameter :: me = 'cfg_get_string_cfg(): '

    iret = 1
    do i = 1, size(sections)
        if (sect_tag == sections(i)%tag) then
            do j = 1, sections(i)%nitems 
                if (item_tag == sections(i)%items(j)%tag) then
                    cval = sections(i)%items(j)%arg
                    iret = 0
                    return
                end if
            end do
            return
        end if
    end do
    print *, me, 'Cannot find section ', sect_tag, ' or item ', item_tag
end subroutine cfg_get_string_cfg

!----------------------------------------------------------------------------
! retrieves item value (integer) in for item with a tag item_tag in a section
! section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_integer_sect(item_tag, section, ival, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    integer, intent(out) :: ival
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_integer_sect(): '

    call cfg_get_string_sect(item_tag, section, cval, iret)
    if (iret /= 0) return
    read(cval, *, iostat=iret) ival
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_integer_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (integer) in for item with a tag item_tag in a section with a tag 
! sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_integer_cfg(sect_tag, item_tag, sections, ival, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    integer, intent(out) :: ival
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_integer_cfg(): '

    call cfg_get_string_cfg(sect_tag, item_tag, sections, cval, iret)
    if (iret /= 0) return
    read(cval, *, iostat=iret) ival
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_integer_cfg

!----------------------------------------------------------------------------
! retrieves item value (real) in for item with a tag item_tag in a section
! section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_real_sect(item_tag, section, rval, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    real(kind=r_kind), intent(out) :: rval
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_real_sect(): '

    call cfg_get_string_sect(item_tag, section, cval, iret)
    if (iret /= 0) return
    read(cval, *, iostat=iret) rval
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_real_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (real) in for item with a tag item_tag in a section with a tag 
! sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_real_cfg(sect_tag, item_tag, sections, rval, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    real(kind=r_kind), intent(out) :: rval
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_real_cfg(): '

    call cfg_get_string_cfg(sect_tag, item_tag, sections, cval, iret)
    if (iret /= 0) return
    read(cval, *, iostat=iret) rval
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_real_cfg

!----------------------------------------------------------------------------
! retrieves item value (boolean) in for item with a tag item_tag in a section
! section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_boolean_sect(item_tag, section, bval, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    logical, intent(out) :: bval
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_boolean_sect(): '

    call cfg_get_string_sect(item_tag, section, cval, iret)
    if (iret /= 0) return
    select case(cval)
    case ('y', 'yes', 't', 'true')
        bval = .true.
    case ('n', 'no', 'f', 'false')
        bval = .false.
    case default
        iret = 1
    end select
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_boolean_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (integer) in for item with a tag item_tag in a section with a tag 
! sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_boolean_cfg(sect_tag, item_tag, sections, bval, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    logical, intent(out) :: bval
    integer, intent(out) :: iret

    character(len=cfg_arg_len) :: cval
    character(len=*), parameter :: me = 'cfg_get_integer_cfg(): '

    call cfg_get_string_cfg(sect_tag, item_tag, sections, cval, iret)
    if (iret /= 0) return
    select case(cval)
    case ('y', 'yes', 't', 'true')
        bval = .true.
    case ('n', 'no', 'f', 'false')
        bval = .false.
    case default
        iret = 1
    end select
    if (iret /= 0) print *, me, 'Invalid value ', trim(cval), ' for ', item_tag
end subroutine cfg_get_boolean_cfg

!----------------------------------------------------------------------------
! retrieves item value (list of blank separated strings) in for item with 
! a tag item_tag in a section section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_strings_sect(item_tag, section, numvals, svals, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    integer, intent(out) :: numvals
    character(len=*), dimension(:), intent(out) :: svals
    integer, intent(out) :: iret

    integer :: n
    character(len=cfg_arg_len) :: cval

    call cfg_get_string_sect(item_tag, section, cval, iret)
    if (iret /= 0) return
    n = cfg_max_tokens
    call tokenize_split(cval, cfg_token_len, n, svals)
    numvals = n
end subroutine cfg_get_strings_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (list of blank separated strings) in for item with a tag item_tag
! in a section with a tag sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_strings_cfg(sect_tag, item_tag, sections, numvals, &
    svals, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    integer, intent(out) :: numvals
    character(len=*), dimension(:), intent(out) :: svals
    integer, intent(out) :: iret

    integer :: n
    character(len=cfg_arg_len) :: cval

    call cfg_get_string_cfg(sect_tag, item_tag, sections, cval, iret)
    if (iret /= 0) return
    n = cfg_max_tokens
    call tokenize_split(cval, cfg_token_len, n, svals)
    numvals = n
end subroutine cfg_get_strings_cfg

!----------------------------------------------------------------------------
! retrieves item value (list of blank separated integers) in for item with 
! a tag item_tag in a section section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_integers_sect(item_tag, section, numvals, ivals, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    integer, intent(out) :: numvals
    integer, dimension(:), intent(out) :: ivals
    integer, intent(out) :: iret

    integer :: i
    character(len=cfg_token_len), dimension(cfg_max_tokens) :: tokens
    character(len=*), parameter :: me = 'cfg_get_integers_sect(): '

    call cfg_get_strings_sect(item_tag, section, numvals, tokens, iret)
    if (iret /= 0) return
    do i = 1, numvals
        read(tokens(i), *, iostat=iret) ivals(i)
        if (iret /= 0) then
            print *, me, 'Invalid value ', trim(tokens(i)), ' for ', item_tag
            exit
        end if
    end do
end subroutine cfg_get_integers_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (list of blank separated integers) in for item with a tag item_tag
! in a section with a tag sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_integers_cfg(sect_tag, item_tag, sections, numvals, &
    ivals, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    integer, intent(out) :: numvals
    integer, dimension(:), intent(out) :: ivals
    integer, intent(out) :: iret

    integer :: i
    character(len=cfg_token_len), dimension(cfg_max_tokens) :: tokens
    character(len=*), parameter :: me = 'cfg_get_integers_cfg(): '

    call cfg_get_strings_cfg(sect_tag, item_tag, sections, numvals, &
        tokens, iret)
    if (iret /= 0) return
    do i = 1, numvals
        read(tokens(i), *, iostat=iret) ivals(i)
        if (iret /= 0) then
            print *, me, 'Invalid value ', trim(tokens(i)), ' for ', item_tag
            exit
        end if
    end do
end subroutine cfg_get_integers_cfg

!----------------------------------------------------------------------------
! retrieves item value (list of blank separated reals) in for item with 
! a tag item_tag in a section section. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_reals_sect(item_tag, section, numvals, rvals, iret)
    character(len=*), intent(in) :: item_tag
    type(cfg_sect_t), intent(in) :: section
    integer, intent(out) :: numvals
    real(kind=r_kind), dimension(:), intent(out) :: rvals
    integer, intent(out) :: iret

    integer :: i
    character(len=cfg_token_len), dimension(cfg_max_tokens) :: tokens
    character(len=*), parameter :: me = 'cfg_get_reals_sect(): '

    call cfg_get_strings_sect(item_tag, section, numvals, tokens, iret)
    if (iret /= 0) return
    do i = 1, numvals
        read(tokens(i), *, iostat=iret) rvals(i)
        if (iret /= 0) then
            print *, me, 'Invalid value ', trim(tokens(i)), ' for ', item_tag
            exit
        end if
    end do
end subroutine cfg_get_reals_sect

!----------------------------------------------------------------------------
! given list of sections read from configuration file, the subroutine returns
! item value (list of blank separated reals) in for item with a tag item_tag
! in a section with a tag sect_tag. iret is 0 if such item exists, 1 if not 
subroutine cfg_get_reals_cfg(sect_tag, item_tag, sections, numvals, &
    rvals, iret)
    character(len=*), intent(in) :: sect_tag, item_tag
    type(cfg_sect_t), dimension(:), intent(in) :: sections
    integer, intent(out) :: numvals
    real(kind=r_kind), dimension(:), intent(out) :: rvals
    integer, intent(out) :: iret

    integer :: i, n
    character(len=cfg_token_len), dimension(cfg_max_tokens) :: tokens
    character(len=*), parameter :: me = 'cfg_get_reals_cfg(): '

    call cfg_get_strings_cfg(sect_tag, item_tag, sections, numvals, &
        tokens, iret)
    if (iret /= 0) return
    do i = 1, numvals
        read(tokens(i), *, iostat=iret) rvals(i)
        if (iret /= 0) then
            print *, me, 'Invalid value ', trim(tokens(i)), ' for ', item_tag
            exit
        end if
    end do
end subroutine cfg_get_reals_cfg

!----------------------------------------------------------------------------
! returns list of sections read from the configuration file
subroutine cfg_read_file(lu, filename, num_sect, sections, iret)
    integer, intent(in) :: lu
    character(len=*), intent(in) :: filename
    integer, intent(in) :: num_sect
    type(cfg_sect_t), dimension(num_sect), target, intent(out) :: sections
    integer, intent(out) :: iret

    integer :: i, k, nsect, narg
    character(len=256) :: line
    type(cfg_sect_t), pointer :: p_sect 
    type(item_t), pointer :: p_item 
    character(len=*), parameter :: me = 'cfg_read_file(): '

    iret = 1
    nsect = 0
    open(unit=lu, file=filename, action='read', status='old', iostat=iret) 
    if (iret /= 0) then
        print *, me, 'cannot open ', trim(filename), ', iostat = ', iret
        return
    end if
    do
        read(lu, '(a256)', iostat=iret) line
        if (iret < 0) then
            iret = 0 
            exit
        else if (iret > 0) then
            print *, me, 'error reading line, iostat = ', iret
            cycle
        end if
        if (line(:1) == '#' .or. len(trim(line)) == 0) cycle
        if (line(1:1) == '[') then
            i = index(line, ']')
            if (i > 0) then     ! found section
                if (nsect > num_sect) then
                    print *, me, 'too many sections'
                    exit
                end if
                nsect = nsect + 1
                p_sect => sections(nsect)
                p_sect%tag = line(2:i-1) 
                p_sect%nitems = 0
            else
                print *, me, 'syntax error on line: ', trim(line)
                exit
            end if
        else
            i = index(line, '=')
            if (i > 0 .and. nsect > 0) then     ! found item line
                if (p_sect%nitems >= max_items) then
                    print *, me, 'too many items for: ', p_sect%tag
                    exit
                end if
                p_sect%nitems = p_sect%nitems + 1
                p_item => p_sect%items(p_sect%nitems)
                k = len(trim(line(:i-1)))
                if (k == 0) then
                    print *, me, 'syntax error on line: ', trim(line)
                    exit
                end if
                p_item%tag = line(:k) 
                k = first_nonblank(line(i+1:))
                if (k == 0) then
                    p_item%arg = ' '
                else
                    p_item%arg = trim(line(i+k:))
                end if
            else
                print *, me, 'syntax error on line: ', trim(line)
                exit
            end if
        end if
    end do
    close(lu)
end subroutine cfg_read_file

end module cfgini
