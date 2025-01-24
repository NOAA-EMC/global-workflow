module read_wrf_nmm_files_mod
use abstract_read_wrf_nmm_files_mod
  type, extends(abstract_read_wrf_nmm_files_class) :: read_wrf_nmm_files_class 
  contains
    procedure, pass(this) :: read_wrf_nmm_files => read_wrf_nmm_files_dummy
    procedure, pass(this) :: read_nems_nmmb_files => read_nems_nmmb_files_dummy
  end type read_wrf_nmm_files_class 
contains
  subroutine read_wrf_nmm_files_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_nmm_files   same as read_files, but for wrfnmm
  !   prgmmr: parrish          org: np22                date: 2004-06-22
  !
  ! abstract: dummy figure out available time levels of background fields for 
  !             later input.  This is still evolving for wrf nmm and other
  !             possible wrf input fields.  Initially patterned after 
  !             read_files.
  !
  ! program history log:
  !   2004-06-22  parrish, document
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-12-03  treadon - replace mpe_ibcast (IBM extension) with
  !                         standard mpi_bcast
  !   2005-03-30  treadon - reformat code (cosmetic changes only)
  !   2006-06-19  wu - changes to allow nfldsig=3 (multiple first guess)
  !   2008-04-16  safford - remove unsused vars
  !   2009-10-09  wu - reset time reference (using iwinbgn and winlen...) in preparation for 4dvar
  !   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
  !   2015-05-12  wu - remove check to allow FGAT/4DEnVar guess files beyond
  !                    nhr_half
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
  !$$$  end documentation block
  
    use kinds, only: r_kind,r_single,i_kind
    implicit none
  
  ! Declare passed variables
    integer(i_kind),intent(in   ) :: mype
    class(read_wrf_nmm_files_class),intent(inout) :: this
  
    write(6,*)'READ_WRF_NMM_FILES:     ***WARNING*** dummy call ... does nothing!'
  
  ! End of routine
    return
  end subroutine read_wrf_nmm_files_dummy
  subroutine read_nems_nmmb_files_dummy(this,mype)
  
    use kinds, only: r_kind,r_single,i_kind
    implicit none
  
  ! Declare passed variables
    integer(i_kind),intent(in   ) :: mype
    class(read_wrf_nmm_files_class),intent(inout) :: this
  
    write(6,*)'READ_NEMS_NMMB_FILES:     ***WARNING*** dummy call ... does nothing!'
  
  ! End of routine
    return
  end subroutine read_nems_nmmb_files_dummy
end module read_wrf_nmm_files_mod
