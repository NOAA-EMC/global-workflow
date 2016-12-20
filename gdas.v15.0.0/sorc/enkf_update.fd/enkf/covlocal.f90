module covlocal
!$$$  module documentation block
!
! module: covlocal                     functions for Gaspari-Cohn localization,
!                                      blending northern, tropical and southern
!                                      hemisphere parameter values.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: two functions, one defining the Gaspari-Cohn polynomial for
!  compact localization (taper) and one to smoothly blend parameter
!  values defined separately for the northern hemisphere, southern
!  hemisphere and tropical regions as a function of latitude.
!
! Public Functions:
!  taper: Compute the localization function defined by Gaspari and Cohn, 1999, QJRMS, 
!   p. 723-757, eqn 4.10.
!  latval: blend parameters defined as separate NH, SH and tropical constants.
!   The blending is done linearly in latitude band defined by the
!   parameters latbound and delat.
!
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!
! attributes:
!   language: f95
!
!$$$

use kinds, only : r_single

private
public :: taper, latval

real(r_single) a1,a2,a3,a4,a5,a6,a7,a8,a9
parameter(a1 = -8.0_r_single)
parameter(a2 = 8.0_r_single)
parameter(a3 = 5.0_r_single)
parameter(a4 = 20.0_r_single/3.0_r_single)
parameter(a5 = 1.0_r_single)
parameter(a6 = 8.0_r_single/3.0_r_single)
parameter(a7 = -10.0_r_single)
parameter(a8 =  4.0_r_single)
parameter(a9 = -1.0_r_single/3.0_r_single)

contains

real(r_single) function taper(r)
 ! Gaspari-Cohn taper function.
 ! r should be positive, and normalized so taper = 0 at r = 1
 ! very close to exp(-(r/c)**2), where c = 0.388
 implicit none
 real(r_single), intent(in) :: r
 if(r < a5)then
   if(r > 0.5_r_single)then
      taper = ( ( ( ( a6*r -a2 )*r +a3 )*r +a4 )*r +a7)*r + a8 + a9/r
   else 
      taper = ( ( ( a1*r +a2)*r +a3 )*r -a4)*r*r + a5
   end if
 else
    taper = 0._r_single
 end if
end function taper

real(r_single) function latval(deglat,valnh,valtr,valsh)
 ! blend valnh, valtr and valsh (nh, tropical and sh values)
 ! so they vary linearly from one to another in the latitude
 ! bands defined by latbound and delat.
 ! Inputs: 
 !  delat: latitude (in degrees)
 !  valnh,valtr,valsh: NH, tropical and SH parameter values.
 ! Returns:  the parameter value corresponding the specified
 !           latitude.
 use params, only: latboundpp,latboundpm,latboundmp,latboundmm, delatinv
 !  latbound:  the latitude that defines the poleward extent
 !             of the "tropics".
 !  delat:  the transition zone (in degree latitude) over
 !          which the parameters change linearly from one
 !          value to another.
 implicit none
 real(r_single), intent(in) :: deglat,valnh,valtr,valsh
 if (deglat > latboundpp) then
    latval = valnh
 else if (deglat >= latboundpm) then
    latval = ((latboundpp-deglat)*valtr + (deglat-latboundpm)*valnh)*delatinv
 else if (deglat > latboundmp) then
    latval = valtr
 else if (deglat >= latboundmm) then
    latval = ((latboundpp+deglat)*valtr + (-deglat+latboundmp)*valsh)*delatinv
 else
    latval = valsh
 end if
end function latval

end module covlocal
