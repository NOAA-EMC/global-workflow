!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************

module constants_mod

! <OVERVIEW>
!    Defines useful constants for Earth.
! </OVERVIEW>

! <DESCRIPTION>
!   Constants are defined as real parameters.
!   Constants are accessed through the "use" statement.
! </DESCRIPTION>

implicit none
private

character(len=128) :: version='$Id$'
character(len=128) :: tagname='$Name$'
!dummy variable to use in HUGE initializations
real :: realnumber

!------------ physical constants ---------------
! <DATA NAME="RADIUS" UNITS="m" TYPE="real" DEFAULT="6371.e3">
!   radius of the earth
! </DATA>
! <DATA NAME="OMEGA" UNITS="1/s" TYPE="real" DEFAULT="7.292e-5">
!   rotation rate of the planet (earth)
! </DATA>
! <DATA NAME="GRAV" UNITS="m/s^2" TYPE="real" DEFAULT="9.80">
!   acceleration due to gravity
! </DATA>
! <DATA NAME="RDGAS" UNITS="J/kg/deg" TYPE="real" DEFAULT="287.04">
!   gas constant for dry air
! </DATA>
! <DATA NAME="KAPPA" TYPE="real" DEFAULT="2./7.">
!   RDGAS / CP_AIR
! </DATA>
! <DATA NAME="CP_AIR" UNITS="J/kg/deg" TYPE="real" DEFAULT="RDGAS/KAPPA">
!   specific heat capacity of dry air at constant pressure
! </DATA>
! <DATA NAME="CP_OCEAN" UNITS="J/kg/deg" TYPE="real" DEFAULT="3989.24495292815">
!   specific heat capacity taken from McDougall (2002) "Potential Enthalpy ..."
! </DATA>
! <DATA NAME="RHO0" UNITS="kg/m^3" TYPE="real" DEFAULT="1.035e3">
!   average density of sea water
! </DATA>
! <DATA NAME="RHO0R" UNITS="m^3/kg" TYPE="real" DEFAULT="1.0/RHO0">
!   reciprocal of average density of sea water
! </DATA>
! <DATA NAME="RHO_CP" UNITS="J/m^3/deg" TYPE="real" DEFAULT="RHO0*CP_OCEAN">
!   (kg/m^3)*(cal/kg/deg C)(joules/cal) = (joules/m^3/deg C)
! </DATA>

!---variable for strong typing grid parameters
  integer, public, parameter :: R_GRID=8

#ifdef GFS_PHYS
! real(kind=R_GRID), public, parameter :: RADIUS = 6376000.0_R_GRID
! SJL: the following are from fv3_gfsphysics/gfs_physics/physics/physcons.f90
real, public, parameter :: RADIUS = 6.3712e+6_R_GRID
real(kind=8), public, parameter :: PI_8   = 3.1415926535897931_R_GRID
real, public, parameter ::         PI     = 3.1415926535897931_R_GRID
real, public, parameter :: OMEGA  = 7.2921e-5 
real, public, parameter :: GRAV   = 9.80665_R_GRID
real, public, parameter :: RDGAS  = 287.05_R_GRID
real, public, parameter :: RVGAS  = 461.50_R_GRID
! Extra:
real, public, parameter :: HLV = 2.5e6_R_GRID   
real, public, parameter :: HLF = 3.3358e5_R_GRID   
real, public, parameter :: con_cliq   =4.1855e+3_R_GRID      ! spec heat H2O liq   (J/kg/K)
real, public, parameter :: con_csol   =2.1060e+3_R_GRID      ! spec heat H2O ice   (J/kg/K)
#else

#ifdef SMALL_EARTH
#ifdef DCMIP
       real, private, paramter :: small_fac =  1._R_GRID / 120._R_GRID #only needed for supercell test
#else
#ifdef HIWPP
#ifdef SUPER_K
       real, private, parameter :: small_fac = 1._R_GRID / 120._R_GRID
#else
 real, private, parameter :: small_fac = 1._R_GRID / 166.7_R_GRID
#endif
#else
 real, private, parameter :: small_fac = 1._R_GRID / 10._R_GRID
#endif
#endif
#else
 real, private, parameter :: small_fac = 1._R_GRID
#endif

real, public, parameter :: RADIUS = 6371e+3_R_GRID * small_fac
real(kind=8), public, parameter :: PI_8 = 3.141592653589793_R_GRID
real, public, parameter ::         PI   = 3.141592653589793_R_GRID
real, public, parameter :: OMEGA = 7.292e-5_R_GRID / small_fac
real, public, parameter :: GRAV  = 9.8060226_R_GRID
real, public, parameter :: RDGAS = 287.04_R_GRID 
real, public, parameter :: RVGAS = 461.60_R_GRID 
! Extra:
real, public, parameter :: HLV = 2.501e6_R_GRID   
real, public, parameter :: HLF = 3.50e5_R_GRID   
#endif
real, public, parameter :: CP_AIR = 1004.6_R_GRID
real, public, parameter :: CP_VAPOR = 4.0_R_GRID*RVGAS
real, public, parameter :: KAPPA  = RDGAS/CP_AIR
!!! real, public, parameter :: STEFAN  = 5.670400e-8_R_GRID
real, public, parameter :: STEFAN  = 5.67051e-8_R_GRID 

real, public, parameter :: CP_OCEAN = 3989.24495292815_R_GRID
real, public, parameter :: RHO0    = 1.035e3_R_GRID
real, public, parameter :: RHO0R   = 1.0_R_GRID/RHO0
real, public, parameter :: RHO_CP  = RHO0*CP_OCEAN

!rabreal, public, parameter :: KAPPA  = 2._R_GRID/7._R_GRID
!rabreal, public, parameter :: GRAV   = 9.80_R_GRID    
!rabreal, public, parameter :: CP_AIR = RDGAS/KAPPA 

!------------ water vapor constants ---------------
! <DATA NAME="ES0" TYPE="real" DEFAULT="1.0">
!   Humidity factor. Controls the humidity content of the atmosphere through 
!   the Saturation Vapour Pressure expression when using DO_SIMPLE.
! </DATA>
! <DATA NAME="RVGAS" UNITS="J/kg/deg" TYPE="real" DEFAULT="461.50">
!   gas constant for water vapor
! </DATA>
! <DATA NAME="CP_VAPOR" UNITS="J/kg/deg" TYPE="real" DEFAULT="4.0*RVGAS">
!   specific heat capacity of water vapor at constant pressure
! </DATA>
! <DATA NAME="DENS_H2O" UNITS="kg/m^3" TYPE="real" DEFAULT="1000.">
!   density of liquid water
! </DATA>
! <DATA NAME="HLV" UNITS="J/kg" TYPE="real" DEFAULT="2.500e6">
!   latent heat of evaporation
! </DATA>
! <DATA NAME="HLF" UNITS="J/kg" TYPE="real" DEFAULT="3.34e5">
!   latent heat of fusion
! </DATA>
! <DATA NAME="HLS" UNITS="J/kg" TYPE="real" DEFAULT="2.834e6">
!   latent heat of sublimation
! </DATA>
! <DATA NAME="TFREEZE" UNITS="degK" TYPE="real" DEFAULT="273.16">
!   temp where fresh water freezes
! </DATA>

real, public, parameter :: ES0 = 1.0_R_GRID 
real, public, parameter :: DENS_H2O = 1000._R_GRID 
real, public, parameter :: HLS = HLV + HLF
real, public, parameter :: TFREEZE = 273.15_R_GRID    

!rabreal, public, parameter :: RVGAS = 461.50_R_GRID 
!rabreal, public, parameter :: HLV = 2.500e6_R_GRID   
!rabreal, public, parameter :: HLF = 3.34e5_R_GRID   
!rabreal, public, parameter :: HLS = HLV + HLF
!rabreal, public, parameter :: TFREEZE = 273.16_R_GRID    

!-------------- radiation constants -----------------

! <DATA NAME="WTMAIR" UNITS="AMU" TYPE="real" DEFAULT="2.896440E+01">
!  molecular weight of air 
! </DATA>
! <DATA NAME="WTMH2O" UNITS="AMU" TYPE="real" DEFAULT="1.801534E+01">
!  molecular weight of water
! </DATA>
! <DATA NAME="WTMOZONE" UNITS="AMU" TYPE="real" DEFAULT="4.799820E+01">
!   molecular weight of ozone
! </DATA>
! <DATA NAME="WTMC" UNITS="AMU" TYPE="real" DEFAULT="1.200000+01">
!   molecular weight of carbon 
! <DATA NAME="WTMCO2" UNITS="AMU" TYPE="real" DEFAULT="4.400995+01">
!   molecular weight of carbon dioxide
! <DATA NAME="WTMO2" UNITS="AMU" TYPE="real" DEFAULT="3.19988+01">
!   molecular weight of molecular oxygen
! <DATA NAME="WTMCFC11" UNITS="AMU" TYPE="real" DEFAULT="1.373681+02">
!   molecular weight of CFC-11 (CCl3F)
! <DATA NAME="WTMCFC12" UNITS="AMU" TYPE="real" DEFAULT="1.209135+02">
!   molecular weight of CFC-21 (CCl2F2)
! </DATA>
! <DATA NAME="DIFFAC" TYPE="real" DEFAULT="1.660000E+00">
! diffusivity factor
! </DATA>
! <DATA NAME="SECONDS_PER_DAY" UNITS="seconds" TYPE="real" DEFAULT="8.640000E+04">
! seconds in a day
! </DATA>
! <DATA NAME="AVOGNO" UNITS="atoms/mole" TYPE="real" DEFAULT="6.023000E+23">
!  Avogadro's number 
! </DATA>
! <DATA NAME="PSTD" UNITS="dynes/cm^2" TYPE="real" DEFAULT="1.013250E+06">
!  mean sea level pressure
! </DATA>
! <DATA NAME="PSTD_MKS" UNITS="Newtons/m^2" TYPE="real" DEFAULT="101325.0">
!  mean sea level pressure
! </DATA>

real, public, parameter :: WTMAIR = 2.896440E+01_R_GRID
real, public, parameter :: WTMH2O = WTMAIR*(RDGAS/RVGAS) !pjp OK to change value because not used yet.
!real, public, parameter :: WTMO3  = 47.99820E+01_R_GRID
real, public, parameter :: WTMOZONE =  47.99820_R_GRID
real, public, parameter :: WTMC     =  12.00000_R_GRID
real, public, parameter :: WTMCO2   =  44.00995_R_GRID
real, public, parameter :: WTMO2    =  31.9988_R_GRID
real, public, parameter :: WTMCFC11 = 137.3681_R_GRID
real, public, parameter :: WTMCFC12 = 120.9135_R_GRID
real, public, parameter :: DIFFAC = 1.660000E+00_R_GRID
real, public, parameter :: SECONDS_PER_DAY  = 8.640000E+04_R_GRID, SECONDS_PER_HOUR = 3600._R_GRID, SECONDS_PER_MINUTE=60._R_GRID
real, public, parameter :: AVOGNO = 6.023000E+23_R_GRID
real, public, parameter :: PSTD   = 1.013250E+06_R_GRID
real, public, parameter :: PSTD_MKS    = 101325.0_R_GRID

! <DATA NAME="RADCON" UNITS="deg sec/(cm day)" TYPE="real" DEFAULT="((1.0E+02*GRAV)/(1.0E+04*CP_AIR))*SECONDS_PER_DAY">
!  factor used to convert flux divergence to heating rate in degrees per day
! </DATA>
! <DATA NAME="RADCON_MKS" UNITS="deg sec/(m day)" TYPE="real" DEFAULT="(GRAV/CP_AIR)*SECONDS_PER_DAY">
!  factor used to convert flux divergence to heating rate in degrees per day
! </DATA>
! <DATA NAME="O2MIXRAT" TYPE="real" DEFAULT="2.0953E-01">
! mixing ratio of molecular oxygen in air
! </DATA>
! <DATA NAME="RHOAIR" UNITS="kg/m^3" TYPE="real" DEFAULT="1.292269">
!  reference atmospheric density
! </DATA>
! <DATA NAME="ALOGMIN" TYPE="real" DEFAULT="-50.0">
!  minimum value allowed as argument to log function
! </DATA>

real, public, parameter :: RADCON = ((1.0E+02*GRAV)/(1.0E+04*CP_AIR))*SECONDS_PER_DAY
real, public, parameter :: RADCON_MKS  = (GRAV/CP_AIR)*SECONDS_PER_DAY
real, public, parameter :: O2MIXRAT    = 2.0953E-01_R_GRID
real, public, parameter :: RHOAIR      = 1.292269_R_GRID
real, public, parameter :: ALOGMIN     = -50.0_R_GRID

!------------ miscellaneous constants ---------------
! <DATA NAME="STEFAN" UNITS="W/m^2/deg^4" TYPE="real" DEFAULT="5.6734e-8">
!   Stefan-Boltzmann constant
! </DATA>
! <DATA NAME="VONKARM"  TYPE="real" DEFAULT="0.40">
!   Von Karman constant
! </DATA>
! <DATA NAME="PI" TYPE="real" DEFAULT="3.14159265358979323846">
!    ratio of circle circumference to diameter
! </DATA>
! <DATA NAME="RAD_TO_DEG"  TYPE="real" DEFAULT="180.0/PI">
!   degrees per radian
! </DATA>
! <DATA NAME="DEG_TO_RAD"  TYPE="real" DEFAULT="PI/180.0">
!   radians per degree
! </DATA>
! <DATA NAME="RADIAN"  TYPE="real" DEFAULT="180.0/PI">
!   equal to RAD_TO_DEG. Named RADIAN for backward compatability.
! </DATA>
! <DATA NAME="C2DBARS" UNITS="dbars" TYPE="real" DEFAULT="1.e-4">
!   converts rho*g*z (in mks) to dbars: 1dbar = 10^4 (kg/m^3)(m/s^2)m
! </DATA>
! <DATA NAME="KELVIN" TYPE="real" DEFAULT="273.15">
!   degrees Kelvin at zero Celsius
! </DATA>
! <DATA NAME="EPSLN" TYPE="real" DEFAULT="1.0e-40">
!   a small number to prevent divide by zero exceptions
! </DATA>

real, public, parameter :: VONKARM = 0.40_R_GRID     
real, public, parameter :: RAD_TO_DEG=180._R_GRID/PI
real, public, parameter :: DEG_TO_RAD=PI/180._R_GRID
real, public, parameter :: RADIAN  = RAD_TO_DEG
real, public, parameter :: C2DBARS = 1.e-4_R_GRID
real, public, parameter :: KELVIN  = 273.15_R_GRID
real, public, parameter :: EPSLN   = 1.0e-15_R_GRID

!rabreal, public, parameter :: STEFAN  = 5.6734e-8_R_GRID 
!rabreal, public, parameter :: EPSLN   = 1.0e-40_R_GRID
!rabreal, public, parameter :: PI      = 3.14159265358979323846_R_GRID

!-----------------------------------------------------------------------
! version and tagname published
! so that write_version_number can be called for constants_mod by fms_init
public :: version, tagname
!-----------------------------------------------------------------------
public :: constants_init

contains

subroutine constants_init

! dummy routine.

end subroutine constants_init

end module constants_mod

! <INFO>

!   <FUTURE>               
!   1.  Renaming of constants.
!   </FUTURE>               
!   <FUTURE>               
!   2.  Additional constants.
!   </FUTURE>
!   <NOTE>
!    Constants have been declared as type REAL, PARAMETER.
!
!    The value a constant can not be changed in a users program.
!    New constants can be defined in terms of values from the
!    constants module using a parameter statement.<br><br>
!
!    The name given to a particular constant may be changed.<br><br>
!
!    Constants can be used on the right side on an assignment statement
!    (their value can not be reassigned). 
!
!
!<TESTPROGRAM NAME="EXAMPLE">
!<PRE>
!    use constants_mod, only:  TFREEZE, grav_new => GRAV
!    real, parameter :: grav_inv = 1.0 / grav_new
!    tempc(:,:,:) = tempk(:,:,:) - TFREEZE
!    geopotential(:,:) = height(:,:) * grav_new
!</PRE>
!</TESTPROGRAM>
!   </NOTE>

! </INFO>

