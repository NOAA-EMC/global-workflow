module wrwrfmassa_mod
use abstract_wrwrfmassa_mod
  type, extends(abstract_wrwrfmassa_class) :: wrwrfmassa_class
  contains
    procedure, pass(this) :: wrwrfmassa_binary => wrwrfmassa_binary_dummy
    procedure, pass(this) :: wrwrfmassa_netcdf => wrwrfmassa_netcdf_dummy
  end type wrwrfmassa_class
contains


  subroutine wrwrfmassa_binary_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfmassa              write out wrf MASS restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  dummy call to read wrf MASS guess restart interface file, 
  !            add analysis increment, and write out wrf MASS analysis 
  !            restart interface file.
  !
  ! program history log
  !   2005-02-25 todling - add dummy subroutine to skip over wrf code
  !   2005-03-14 treadon - add write statement to note entry into dummy routine
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
    class(wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
    if (mype==0) write(6,*)'WRWRFMASSA_BINARY:  enter dummy call, do nothing'
  end subroutine wrwrfmassa_binary_dummy
  
  subroutine wrwrfmassa_netcdf_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfmassa              write out wrf MASS restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  dummy call to read wrf MASS guess restart interface file, add analysis
  !            increment, and write out wrf MASS analysis restart 
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2006-02-15  treadon - convert specific humidity to moisture mixing ratio
  !   2006-03-07  treadon - convert virtual temperature to potential temperature
  !   2006-04-06  middlecoff - changed iog  from 11 to lendian_in
  !                            changed ioan from 51 to lendian_out
  !   2006-07-28  derber  - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2008-03-27  safford - rm unused vars and uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2010-03-29  hu     - add code to gether cloud/hydrometeor fields and write out
  !   2010-04-01  treadon - move strip_single to gridmod
  !   2011-04-29  todling - introduce MetGuess and wrf_mass_guess_mod
  !   2011-09-20  hclin   - added 15 wrfchem/gocart fields for aod
  !   2012-04-13  whitaker - don't call GSI_BundleGetPointer if n_actual_clouds = 0
  !   2013-10-19  todling - metguess now holds background 
  !   2013-10-24  todling - general interface to strip 
  !   2014-03-12  hu      - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig   - add code to read Qni and Qnc (cloud ice and water
  !                               number concentration)
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
    use kinds, only: r_kind,r_single,i_kind
    implicit none
  
  ! Declare passed variables
    class(wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
    if (mype==0) write(6,*)'WRWRFMASSA_NETCDF:  enter dummy call, do nothing'
    
  end subroutine wrwrfmassa_netcdf_dummy
end module wrwrfmassa_mod
