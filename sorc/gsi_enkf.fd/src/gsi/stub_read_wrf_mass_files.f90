module read_wrf_mass_files_mod
use abstract_read_wrf_mass_files_mod
  type, extends(abstract_read_wrf_mass_files_class) :: read_wrf_mass_files_class 
  contains
    procedure, pass(this) :: read_wrf_mass_files => read_wrf_mass_files_dummy
  end type read_wrf_mass_files_class 
contains
  subroutine read_wrf_mass_files_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_files   same as read_files, but for wrfmass
  !   prgmmr: parrish          org: np22                date: 2004-06-22
  !
  ! abstract: dummy figure out available time levels of background fields for 
  !             later input. This is patterned after read_wrf_mass_files.
  !
  ! program history log:
  !   2004-06-22  parrish, document
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-12-03  treadon - replace mpe_ibcast (IBM extension) with
  !                         standard mpi_bcast
  !   2005-03-30  treadon - reformat code (cosmetic changes only)
  !   2009-10-09  wu      - reset time reference (using iwinbgn and winlen...) in preparation for 4dvar
  !   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
  !   2010-11-14  whitaker - set nfldsfc = nfldsig
  !   2012-04-13  whitaker - read times from sigf file, instead of using
  !   regional_time. Using regional_time was causing ntguessig and hrdifsig to be
  !   set incorrectly.
  !   
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
    use kinds, only: r_kind,r_single,i_kind
    implicit none
  
  ! Declare passed variables
    class(read_wrf_mass_files_class),intent(inout) :: this
    integer(i_kind),intent(in   ) :: mype

  
    write(6,*)'READ_WRF_MASS_FILES:     ***WARNING*** dummy call ... does nothing!'
  ! End of routine
    return
  end subroutine read_wrf_mass_files_dummy
end module read_wrf_mass_files_mod
