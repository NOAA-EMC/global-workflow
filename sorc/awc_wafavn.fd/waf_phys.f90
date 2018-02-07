! waf_phys.f90
! a few elemental functions
! George Trojan, SAIC/EMC/NCEP, May 2007
! Last update: 03/07/07

module waf_phys

use kinds
use physcons
use funcphys

implicit none

private
public phys_icao_hgt, phys_t_rh2td, phys_cloud_cover, phys_theta_e, phys_ti1

contains
!----------------------------------------------------------------------------
elemental function phys_icao_hgt(p)
! calculates ICAO height given pressure
    real(kind=r_kind) :: phys_icao_hgt
    real(kind=r_kind), intent(in) :: p

    real(kind=r_kind), parameter :: lapse = 6.5e-3
    real(kind=r_kind), parameter :: t_sfc = 288.15
    real(kind=r_kind), parameter :: t_strat = 216.65
    real(kind=r_kind), parameter :: alpha_sfc = t_sfc/lapse
    real(kind=r_kind), parameter :: alpha_strat = -con_rd*t_strat/con_g
    real(kind=r_kind), parameter :: kappa = con_rd/con_g*lapse
    real(kind=r_kind), parameter :: p_strat = 22631.7

    if (p >= p_strat) then
        phys_icao_hgt = alpha_sfc * (1.0 - (p/con_p0)**kappa)
    else
        phys_icao_hgt = 11000.0 + alpha_strat * log(p/p_strat)
    end if
end function phys_icao_hgt

!----------------------------------------------------------------------------
elemental function phys_t_rh2td(t, rh)
! calculates dew point given temperature and relative humidity.
    real(kind=r_kind) :: phys_t_rh2td
    real(kind=r_kind), intent(in) :: t, rh

    real(kind=r_kind) :: vp

    vp = rh * fpvsl(t)
    phys_t_rh2td = ftdp(vp)
end function phys_t_rh2td

!----------------------------------------------------------------------------
elemental function phys_rh2q(p, t, rh_percent)
! calculates relative humidity
    real(kind=r_kind) :: phys_rh2q
    real(kind=r_kind), intent(in) :: p, t, rh_percent

    phys_rh2q = 0.01*con_eps*fpvsl(t)*rh_percent/p
end function phys_rh2q

!----------------------------------------------------------------------------
elemental function phys_cloud_cover(p, t, rh_percent, clw)
! calculates cloud cover. Code extractewd from subroutine progcld1
! requires previous call to subroutine gpvsl from module funcphys
    real(kind=r_kind) :: phys_cloud_cover
    real(kind=r_kind), intent(in) :: p, t, rh_percent, clw

    real(kind=r_kind) :: rh, onemrh, qs, tem1, tem2, val

    rh = rh_percent/100.0
    onemrh = max(1.0e-10, 1.0-rh)
    qs = con_eps * fpvsl(t)/p
    tem1 = 2.0e3/min(max(sqrt(sqrt(onemrh*qs)), 1.0e-4), 1.0)
    val = max(min(tem1*clw, 50.0), 0.0)
    tem2 = sqrt(sqrt(rh))
    phys_cloud_cover = max(tem2*(1.0 - exp(-val)), 0.0)
end function phys_cloud_cover

!----------------------------------------------------------------------------
elemental function phys_theta_e(t, rh_percent, p)
! calculates equivalent potential temperature
! Vapour pressure formula 2.19 in R.R.Rogers & M.K.Yau, 
! 'A Short Course in Cloud Physics', Pergamon Press, 1991
    real(kind=r_kind) :: phys_theta_e
    real(kind=r_kind), intent(in) :: t, rh_percent, p

    real(kind=r_kind) :: theta, q, vp, tlcl

    theta = t/fpkap(p)
    q = phys_rh2q(p, t, rh_percent)
    vp = q*p/((1.0-con_eps)*q + con_eps)
    tlcl = ftlcl(t, t-ftdpl(vp))
    phys_theta_e = fthe(tlcl, tlcl/theta)
end function phys_theta_e

!----------------------------------------------------------------------------
elemental function phys_ti1(du_dx, du_dy, dv_dx, dv_dy, du_dh, dv_dh)
! calculates turbulence following Ellrod algoritm
!   (Gary P. Ellrod: Weather and Forecasting, vol 7, pp 150-165, 1992)
!   turb = 1.0e7 * vws * (def - div)
!   where 
!      vws = |d(u,v)/dz|
!      def = sqrt(dst^2+dsh^2)
!          dst = du/dx-dv/dy, dsh = dv/dx+du/dy
!      div = du/dx+dv/dy
!   Index TI1 does not use div
! The output in scaled by a factor 1.0e7
    real(kind=r_kind) :: phys_ti1
    real(kind=r_kind), intent(in) :: du_dx, du_dy, dv_dx, dv_dy, du_dh, dv_dh

    real(kind=r_kind) :: vws2, def2

    vws2 = du_dh**2 + dv_dh**2
    def2 = (du_dx-dv_dy)**2 + (dv_dx+du_dy)**2
!    phys_ti1 = 1.0e7*sqrt(vws2*def2)
!11/23/2009, Chuang: the CAT algorithm is rescaled below to be more consistent 
!with UK CAT products
    phys_ti1 = (1/3.8)*1.0e7*sqrt(vws2*def2)+2.0 
    if(phys_ti1<3.5)phys_ti1=0.0       
end function phys_ti1

end module waf_phys
