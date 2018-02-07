module regrid_nemsio_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use fv3_interface
  use gfs_nems_interface
  use namelist_def

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! regrid_nemsio.f90:

  !-----------------------------------------------------------------------

  subroutine regrid_nemsio()

    !=====================================================================

    ! Define local variables

    call namelistparams()

    ! Check local variable and proceed accordingly

    call fv3_regrid_nemsio()

    !=====================================================================

  end subroutine regrid_nemsio

  !=======================================================================

end module regrid_nemsio_interface
