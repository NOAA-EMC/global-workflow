module nc_diag_write_mod
use kinds, only: i_kind,r_single,r_double
private
public nc_diag_init
public nc_diag_header
public nc_diag_metadata
public nc_diag_data2d
public nc_diag_chaninfo_dim_set
public nc_diag_chaninfo
public nc_diag_write
   interface nc_diag_header
          module procedure nc_diag_header_i
          module procedure nc_diag_header_c
          module procedure nc_diag_header_rs
          module procedure nc_diag_header_rd
   end interface
   interface nc_diag_metadata
          module procedure nc_diag_metadata_i
          module procedure nc_diag_metadata_c
          module procedure nc_diag_metadata_rs
          module procedure nc_diag_metadata_rd
   end interface
   interface nc_diag_chaninfo
          module procedure nc_diag_chaninfo_i
          module procedure nc_diag_chaninfo_c
          module procedure nc_diag_chaninfo_rs
          module procedure nc_diag_chaninfo_rd
   end interface
   interface nc_diag_data2d
          module procedure nc_diag_data1d_rs
          module procedure nc_diag_data1d_rd
          module procedure nc_diag_data2d_rs
          module procedure nc_diag_data2d_rd
   end interface
contains
! init
  subroutine nc_diag_init(fname,append)
    character(len=*):: fname
    logical(i_kind),optional :: append
  end subroutine nc_diag_init
! header
  subroutine nc_diag_header_i(vname,ivar)
    character(len=*):: vname
    integer(i_kind) :: ivar
  end subroutine nc_diag_header_i
  subroutine nc_diag_header_c(vname,cvar)
    character(len=*):: vname
    character(len=*):: cvar
  end subroutine nc_diag_header_c
  subroutine nc_diag_header_rs(vname,rvar)
    character(len=*):: vname
    real(r_single) :: rvar
  end subroutine nc_diag_header_rs
  subroutine nc_diag_header_rd(vname,rvar)
    character(len=*):: vname
    real(r_double) :: rvar
  end subroutine nc_diag_header_rd
! metadata
  subroutine nc_diag_metadata_i(vname,ivar)
    character(len=*):: vname
    integer(i_kind) :: ivar
  end subroutine nc_diag_metadata_i
  subroutine nc_diag_metadata_c(vname,cvar)
    character(len=*):: vname
    character(len=*):: cvar
  end subroutine nc_diag_metadata_c
  subroutine nc_diag_metadata_rs(vname,rvar)
    character(len=*):: vname
    real(r_single) :: rvar
  end subroutine nc_diag_metadata_rs
  subroutine nc_diag_metadata_rd(vname,rvar)
    character(len=*):: vname
    real(r_double) :: rvar
  end subroutine nc_diag_metadata_rd
! data2d - not sure why original code no wrap these with metadata interface!
  subroutine nc_diag_data1d_rs(vname,rvar)
    character(len=*):: vname
    real(r_single) :: rvar(:)
  end subroutine nc_diag_data1d_rs
  subroutine nc_diag_data1d_rd(vname,rvar)
    character(len=*):: vname
    real(r_double) :: rvar(:)
  end subroutine nc_diag_data1d_rd
  subroutine nc_diag_data2d_rs(vname,rvar)
    character(len=*):: vname
    real(r_single) :: rvar(:,:)
  end subroutine nc_diag_data2d_rs
  subroutine nc_diag_data2d_rd(vname,rvar)
    character(len=*):: vname
    real(r_double) :: rvar(:,:)
  end subroutine nc_diag_data2d_rd
!
  subroutine nc_diag_chaninfo_dim_set(ivar)
  integer(i_kind) :: ivar
  end subroutine nc_diag_chaninfo_dim_set
! metadata
  subroutine nc_diag_chaninfo_i(vname,ivar)
    character(len=*):: vname
    integer(i_kind) :: ivar
  end subroutine nc_diag_chaninfo_i
  subroutine nc_diag_chaninfo_c(vname,cvar)
    character(len=*):: vname
    character(len=*):: cvar
  end subroutine nc_diag_chaninfo_c
  subroutine nc_diag_chaninfo_rs(vname,rvar)
    character(len=*):: vname
    real(r_single) :: rvar
  end subroutine nc_diag_chaninfo_rs
  subroutine nc_diag_chaninfo_rd(vname,rvar)
    character(len=*):: vname
    real(r_double) :: rvar
  end subroutine nc_diag_chaninfo_rd
! final
  subroutine nc_diag_write
  end subroutine nc_diag_write
end module nc_diag_write_mod
