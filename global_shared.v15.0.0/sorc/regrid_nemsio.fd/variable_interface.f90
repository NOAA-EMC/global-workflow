module variable_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use kinds
  use physcons,     only: rgas => con_rd, cp => con_cp, grav => con_g,     &
       &                  rerth => con_rerth, rocp => con_rocp,            &
       &                  pi => con_pi, con_rog

  !-----------------------------------------------------------------------

  use mpi_interface
  use namelist_def

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: varinfo
  !public :: variable_lookup
  public :: variable_clip

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type varinfo
     character(len=20)                                                 :: var_name
     character(len=20)                                                 :: nems_name
     character(len=20)                                                 :: nems_levtyp
     integer                                                           :: nems_lev
     character(len=20)                                                 :: itrptyp
     logical                                                           :: clip
     integer                                                           :: ndims
  end type varinfo ! type varinfo

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  subroutine variable_clip(grid)


    real(r_double)                                                       :: grid(:)
    real(r_double)                                                       :: clip

    clip = tiny(grid(1))
    where(grid .le. dble(0.0)) grid = clip

  end subroutine variable_clip

  !=======================================================================

end module variable_interface
