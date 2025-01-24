module wrwrfnmma_mod
use abstract_wrwrfnmma_mod
  type, extends(abstract_wrwrfnmma_class) :: wrwrfnmma_class
  contains
    procedure, pass(this) :: wrwrfnmma_binary => wrwrfnmma_binary_dummy
    procedure, pass(this) :: wrwrfnmma_netcdf => wrwrfnmma_netcdf_dummy
  end type wrwrfnmma_class
contains
  subroutine  wrwrfnmma_binary_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfnmma              write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  dummy call to read wrf NMM guess restart interface file, add 
  !            analysis increment, and write out wrf NMM analysis restart
  !            interface file.
  !
  ! program history log
  !   2005-02-25 todling - add dummy subroutine to skip over wrf code 
  !   2005-03-14 treadon - add write statement to note entry into dummy routine
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    implicit none
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
    if (mype==0) write(6,*)'WRWRFNMMA_BINARY:  enter dummy call, do nothing'
  end subroutine  wrwrfnmma_binary_dummy
  
  subroutine wrnemsnmma_binary_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrnemsnmma_binary      write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  dummy call to read wrf NMM guess restart interface file, add analysis
  !            increment, and write out wrf NMM analysis restart
  !            interface file.
  !
  ! program history log:
  !   2009-08-14  lueken - added dummy subroutine to skip over wrf code
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    implicit none
  
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
    if (mype==0) write(6,*)'WRNEMSNMMA_BINARY:  enter dummy call, do nothing'
  end subroutine  wrnemsnmma_binary_dummy
  
  subroutine wrwrfnmma_netcdf_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfnmma              write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  dummy call to read wrf NMM guess restart interface file, 
  !            add analysis increment, and write out wrf NMM analysis 
  !            restart interface file.
  !
  !
  ! program history log
  !   2005-02-25 todling - add dummy subroutine to skip over wrf code
  !   2005-03-14 treadon - add write statement to note entry into dummy routine
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    implicit none
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
    if (mype==0) write(6,*)'WRWRFNMMA_NETCDF:  enter dummy call, do nothing'
  end subroutine wrwrfnmma_netcdf_dummy
end module wrwrfnmma_mod
