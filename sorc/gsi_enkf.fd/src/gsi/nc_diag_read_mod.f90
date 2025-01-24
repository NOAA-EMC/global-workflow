module nc_diag_read_mod
use kinds, only: i_kind,r_single,r_double
private
public :: nc_diag_read_init
public :: nc_diag_read_get_var
public :: nc_diag_read_get_global_attr
public :: nc_diag_read_close
public :: nc_diag_read_get_dim
!
interface nc_diag_read_get_var
  module procedure nc_diag_read_get_var_i
  module procedure nc_diag_read_get_var_rd
  module procedure nc_diag_read_get_var_rs
  module procedure nc_diag_read_get_var_i_rank1
  module procedure nc_diag_read_get_var_rd_rank1
  module procedure nc_diag_read_get_var_rs_rank1
  module procedure nc_diag_read_get_var_rd_rank2
  module procedure nc_diag_read_get_var_rs_rank2
end interface
interface nc_diag_read_get_global_attr
  module procedure nc_diag_read_get_global_attr_i
  module procedure nc_diag_read_get_global_attr_c
  module procedure nc_diag_read_get_global_attr_rs
  module procedure nc_diag_read_get_global_attr_rd
end interface
contains
  subroutine nc_diag_read_init(fname,id)
    character(len=*):: fname
    integer(i_kind) :: id
  end subroutine nc_diag_read_init
  integer function nc_diag_read_get_dim(id,vname)
    integer(i_kind) :: id
    character(len=*):: vname
    nc_diag_read_get_dim = 0
  end function nc_diag_read_get_dim
  subroutine nc_diag_read_close(fname)
    character(len=*):: fname
  end subroutine nc_diag_read_close
! get rank 0
  subroutine nc_diag_read_get_var_i(name,mold)
    character(len=*):: name
    integer(i_kind):: mold
  end subroutine nc_diag_read_get_var_i
  subroutine nc_diag_read_get_var_rs(name,mold)
    character(len=*):: name
    real(r_single):: mold
  end subroutine nc_diag_read_get_var_rs
  subroutine nc_diag_read_get_var_rd(name,mold)
    character(len=*):: name
    real(r_double):: mold
  end subroutine nc_diag_read_get_var_rd
! get rank 1
  subroutine nc_diag_read_get_var_i_rank1(name,mold)
    character(len=*):: name
    integer(i_kind):: mold(:)
  end subroutine nc_diag_read_get_var_i_rank1
  subroutine nc_diag_read_get_var_rs_rank1(name,mold)
    character(len=*):: name
    real(r_single):: mold(:)
  end subroutine nc_diag_read_get_var_rs_rank1
  subroutine nc_diag_read_get_var_rd_rank1(name,mold)
    character(len=*):: name
    real(r_double):: mold(:)
  end subroutine nc_diag_read_get_var_rd_rank1
! get rank 1
  subroutine nc_diag_read_get_var_rs_rank2(name,mold)
    character(len=*):: name
    real(r_single):: mold(:,:)
  end subroutine nc_diag_read_get_var_rs_rank2
  subroutine nc_diag_read_get_var_rd_rank2(name,mold)
    character(len=*):: name
    real(r_double):: mold(:,:)
  end subroutine nc_diag_read_get_var_rd_rank2
! global_attr
  subroutine nc_diag_read_get_global_attr_i(imold1,name,mold2)
    character(len=*):: name
    integer(i_kind):: imold1,mold2
  end subroutine nc_diag_read_get_global_attr_i
  subroutine nc_diag_read_get_global_attr_c(imold1,name,mold2)
    character(len=*):: name,mold2
    integer(i_kind):: imold1
  end subroutine nc_diag_read_get_global_attr_c
  subroutine nc_diag_read_get_global_attr_rs(imold1,name,mold2)
    character(len=*):: name
    integer(i_kind):: imold1
    real(r_single):: mold2
  end subroutine nc_diag_read_get_global_attr_rs
  subroutine nc_diag_read_get_global_attr_rd(imold1,name,mold2)
    character(len=*):: name
    integer(i_kind):: imold1
    real(r_double):: mold2
  end subroutine nc_diag_read_get_global_attr_rd
end module nc_diag_read_mod
