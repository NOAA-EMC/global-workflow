module module_nst_water_prop
  use machine, only : kind_phys
  use module_nst_parameters, only : t0k
  !
  private
  public :: rhocoef,density,sw_rad,sw_rad_aw,sw_rad_sum,sw_rad_upper,sw_rad_upper_aw,sw_rad_skin,grv,solar_time_from_julian,compjd, &
            sw_ps_9b,sw_ps_9b_aw
      
  !
  interface sw_ps_9b
     module procedure sw_ps_9b
  end interface
  interface sw_ps_9b_aw
     module procedure sw_ps_9b_aw
  end interface
  !
  interface sw_rad
     module procedure sw_fairall_6exp_v1  ! sw_wick_v1
  end interface
  interface sw_rad_aw
     module procedure sw_fairall_6exp_v1_aw
  end interface
  interface sw_rad_sum
     module procedure sw_fairall_6exp_v1_sum
  end interface
  interface sw_rad_upper
     module procedure sw_soloviev_3exp_v2
  end interface
  interface sw_rad_upper_aw
     module procedure sw_soloviev_3exp_v2_aw
  end interface
  interface sw_rad_skin
     module procedure sw_ohlmann_v1
  end interface
contains
  ! ------------------------------------------------------
  subroutine rhocoef(t, s, rhoref, alpha, beta)
    ! ------------------------------------------------------

    !  compute thermal expansion coefficient (alpha) 
    !  and saline contraction coefficient (beta) using 
    !  the international equation of state of sea water 
    !  (1980). ref: pond and pickard, introduction to 
    !  dynamical oceanography, pp310.  
    !  note: compression effects are not included

    implicit none
    real(kind=kind_phys), intent(in)  :: t, s, rhoref 
    real(kind=kind_phys), intent(out) :: alpha, beta  
    real(kind=kind_phys) :: tc

    tc = t - t0k

    alpha =                                                        & 
         6.793952e-2                                              & 
         - 2.0 * 9.095290e-3 * tc     +  3.0 * 1.001685e-4 * tc**2  & 
         - 4.0 * 1.120083e-6 * tc**3  +  5.0 * 6.536332e-9 * tc**4  & 
         - 4.0899e-3 * s                                            & 
         + 2.0 * 7.6438e-5 * tc * s  -  3.0 * 8.2467e-7 * tc**2 * s & 
         + 4.0 * 5.3875e-9 * tc**3 * s                              & 
         + 1.0227e-4 * s**1.5 -  2.0 * 1.6546e-6 * tc * s**1.5

    ! note: rhoref - specify 
    !
    alpha =  -alpha/rhoref

    beta  =                                             &
         8.24493e-1          -  4.0899e-3 * tc           &
         + 7.6438e-5 * tc**2 -  8.2467e-7 * tc**3        &
         + 5.3875e-9 * tc**4 -  1.5 * 5.72466e-3 * s**.5 &
         + 1.5 * 1.0227e-4 * tc * s**.5                  &
         -  1.5 * 1.6546e-6 * tc**2 * s**.5              &
         + 2.0 * 4.8314e-4 * s

    beta = beta / rhoref

  end subroutine rhocoef
  ! ----------------------------------------
  subroutine density(t, s, rho)
    ! ----------------------------------------
    implicit none

    ! input
    real(kind=kind_phys), intent(in)  :: t     !unit, k
    real(kind=kind_phys), intent(in)  :: s     !unit, 1/1000
    ! output
    real(kind=kind_phys), intent(out) :: rho   !unit, kg/m^3 
    ! local
    real(kind=kind_phys) :: tc

    ! compute density using the international equation 
    ! of state of sea water 1980, (pond and pickard, 
    ! introduction to dynamical oceanography, pp310). 
    ! compression effects are not included

    rho = 0.0
    tc = t - t0k

    !  effect of temperature on density (lines 1-3)
    !  effect of temperature and salinity on density (lines 4-8)
    rho = &
         999.842594                 +  6.793952e-2 * tc     &
         - 9.095290e-3 * tc**2        +  1.001685e-4 * tc**3     &
         - 1.120083e-6 * tc**4        +  6.536332e-9 * tc**5     &
         + 8.24493e-1 * s          -  4.0899e-3 * tc * s         &
         + 7.6438e-5 * tc**2 * s   -  8.2467e-7 * tc**3 * s      &
         + 5.3875e-9 * tc**4 * s   -  5.72466e-3 * s**1.5        &
         + 1.0227e-4 * tc * s**1.5 -  1.6546e-6 * tc**2 * s**1.5 &
         + 4.8314e-4 * s**2

  end subroutine density
  !
  !======================
  !
  elemental subroutine sw_ps_9b(z,fxp)
    !
    ! fraction of the solar radiation absorbed by the ocean at the depth z 
    ! following paulson and simpson, 1981
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! fxp: fraction of the solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real,intent(in):: z
    real,intent(out):: fxp
    real, dimension(9), parameter :: f=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
                                ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    !
    if(z>0) then
      fxp=1.0-(f(1)*exp(-z/gamma(1))+f(2)*exp(-z/gamma(2))+f(3)*exp(-z/gamma(3))+ &
               f(4)*exp(-z/gamma(4))+f(5)*exp(-z/gamma(5))+f(6)*exp(-z/gamma(6))+ &
               f(7)*exp(-z/gamma(7))+f(8)*exp(-z/gamma(8))+f(9)*exp(-z/gamma(9)))
    else
       fxp=0.
    endif
    !
  end subroutine sw_ps_9b
  !
  !======================
  !
  !
  !======================
  !
  elemental subroutine sw_ps_9b_aw(z,aw)
    !
    ! d(fw)/d(z) for 9-band 
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! fxp: fraction of the solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real,intent(in):: z
    real,intent(out):: aw
    real, dimension(9), parameter :: f=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
                                ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    !
    if(z>0) then
      aw=(f(1)/gamma(1))*exp(-z/gamma(1))+(f(2)/gamma(2))*exp(-z/gamma(2))+(f(3)/gamma(3))*exp(-z/gamma(3))+ &
         (f(1)/gamma(4))*exp(-z/gamma(4))+(f(2)/gamma(5))*exp(-z/gamma(5))+(f(6)/gamma(6))*exp(-z/gamma(6))+ &
         (f(1)/gamma(7))*exp(-z/gamma(7))+(f(2)/gamma(8))*exp(-z/gamma(8))+(f(9)/gamma(9))*exp(-z/gamma(9))
    else
       aw=0.
    endif
    !
  end subroutine sw_ps_9b_aw
  !
  !======================
  elemental subroutine sw_fairall_6exp_v1(z,fxp)
    !
    ! fraction of the solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1298)
    ! following paulson and simpson, 1981
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! fxp: fraction of the solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z
    real(kind=kind_phys),intent(out):: fxp
    real(kind=kind_phys), dimension(9), parameter :: f=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
         ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    real(kind=kind_phys),dimension(9) :: zgamma
    real(kind=kind_phys),dimension(9) :: f_c
    !
    if(z>0) then
       zgamma=z/gamma
       f_c=f*(1.-1./zgamma*(1-exp(-zgamma)))
       fxp=sum(f_c)
    else
       fxp=0.
    endif
    !
  end subroutine sw_fairall_6exp_v1
  !
  !======================
  !
  !
  elemental subroutine sw_fairall_6exp_v1_aw(z,aw)
    !
    ! fraction of the solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1298)
    ! following paulson and simpson, 1981
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! aw: d(fxp)/d(z)
    !
    ! fxp: fraction of the solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z
    real(kind=kind_phys),intent(out):: aw
    real(kind=kind_phys) :: fxp
    real(kind=kind_phys), dimension(9), parameter :: f=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
         ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    real(kind=kind_phys),dimension(9) :: zgamma
    real(kind=kind_phys),dimension(9) :: f_aw
    !
    if(z>0) then
       zgamma=z/gamma
       f_aw=(f/z)*((gamma/z)*(1-exp(-zgamma))-exp(-zgamma))
       aw=sum(f_aw)

!      write(*,'(a,f6.2,f12.6,9f10.4)') 'z,aw in sw_rad_aw : ',z,aw,f_aw

    else
       aw=0.
    endif
    !
  end subroutine sw_fairall_6exp_v1_aw
  !
  elemental subroutine sw_fairall_6exp_v1_sum(z,sum)
    !
    ! fraction of the solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1298)
    ! following paulson and simpson, 1981
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! sum: for convection depth calculation
    !
    !
    implicit none
    real(kind=kind_phys),intent(in):: z
    real(kind=kind_phys),intent(out):: sum
    real(kind=kind_phys), dimension(9), parameter :: gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    real(kind=kind_phys),dimension(9) :: zgamma
    real(kind=kind_phys),dimension(9) :: f_sum
    !
!    zgamma=z/gamma
!    f_sum=(zgamma/z)*exp(-zgamma)
!    sum=sum(f_sum)

    sum=(1.0/gamma(1))*exp(-z/gamma(1))+(1.0/gamma(2))*exp(-z/gamma(2))+(1.0/gamma(3))*exp(-z/gamma(3))+ &
        (1.0/gamma(4))*exp(-z/gamma(4))+(1.0/gamma(5))*exp(-z/gamma(5))+(1.0/gamma(6))*exp(-z/gamma(6))+ &
        (1.0/gamma(7))*exp(-z/gamma(7))+(1.0/gamma(8))*exp(-z/gamma(8))+(1.0/gamma(9))*exp(-z/gamma(9))
    !
  end subroutine sw_fairall_6exp_v1_sum
  !
  !======================

  elemental subroutine sw_fairall_simple_v1(f_sol_0,z,df_sol_z)
    !
    ! solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1298)
    ! 
    ! input: 
    ! f_sol_0: solar radiation at the ocean surface (w/m^2)
    ! z:       depth (m)
    !
    ! output:
    ! df_sol_z: solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z,f_sol_0
    real(kind=kind_phys),intent(out):: df_sol_z
    !
    if(z>0) then
       df_sol_z=f_sol_0*(0.137+11.0*z-6.6e-6/z*(1.-exp(-z/8.e-4)))
    else
       df_sol_z=0.
    endif
    !
  end subroutine sw_fairall_simple_v1
  !
  !======================
  !
  elemental subroutine sw_wick_v1(f_sol_0,z,df_sol_z)
    !
    ! solar radiation absorbed by the ocean at the depth z (zeng and beljaars, 2005, p.5)
    ! 
    ! input: 
    ! f_sol_0: solar radiation at the ocean surface (w/m^2)
    ! z:       depth (m)
    !
    ! output:
    ! df_sol_z: solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z,f_sol_0
    real(kind=kind_phys),intent(out):: df_sol_z
    !
    if(z>0) then
       df_sol_z=f_sol_0*(0.065+11.0*z-6.6e-5/z*(1.-exp(-z/8.e-4)))
    else
       df_sol_z=0.
    endif
    !
  end subroutine sw_wick_v1
  !
  !======================
  !
  elemental subroutine sw_soloviev_3exp_v1(f_sol_0,z,df_sol_z)
    !
    ! solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1301)
    ! following soloviev, 1982
    ! 
    ! input: 
    ! f_sol_0: solar radiation at the ocean surface (w/m^2)
    ! z:       depth (m)
    !
    ! output:
    ! df_sol_z: solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z,f_sol_0
    real(kind=kind_phys),intent(out):: df_sol_z
    real(kind=kind_phys),dimension(3) :: f_c
    real(kind=kind_phys), dimension(3), parameter :: f=(/0.45,0.27,0.28/) &
         ,gamma=(/12.8,0.357,0.014/)
    !
    if(z>0) then
       f_c=f*gamma(1-exp(-z/gamma))
       df_sol_z=f_sol_0*(1.0-sum(f_c)/z)
    else
       df_sol_z=0.
    endif
    !
  end subroutine sw_soloviev_3exp_v1
  !
  !======================
  !
  elemental subroutine sw_soloviev_3exp_v2(f_sol_0,z,df_sol_z)
    !
    ! solar radiation absorbed by the ocean at the depth z (fairall et all, 1996, p. 1301)
    ! following soloviev, 1982
    ! 
    ! input: 
    ! f_sol_0: solar radiation at the ocean surface (w/m^2)
    ! z:       depth (m)
    !
    ! output:
    ! df_sol_z: solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z,f_sol_0
    real(kind=kind_phys),intent(out):: df_sol_z
    !
    if(z>0) then
       df_sol_z=f_sol_0*(1.0 &
            -(0.28*0.014*(1.-exp(-z/0.014)) &
            +0.27*0.357*(1.-exp(-z/0.357)) &        
            +.45*12.82*(1.-exp(-z/12.82)))/z &
            )
    else
       df_sol_z=0.
    endif
    !
  end subroutine sw_soloviev_3exp_v2

  elemental subroutine sw_soloviev_3exp_v2_aw(z,aw)
    !
    ! aw = d(fxp)/d(z)
    ! following soloviev, 1982
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! aw: d(fxp)/d(z)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z
    real(kind=kind_phys),intent(out):: aw
    real(kind=kind_phys):: fxp
    !
    if(z>0) then
       fxp=(1.0 &
            -(0.28*0.014*(1.-exp(-z/0.014)) &
            + 0.27*0.357*(1.-exp(-z/0.357)) &
            + 0.45*12.82*(1.-exp(-z/12.82)))/z &
            )
       aw=1.0-fxp-(0.28*exp(-z/0.014)+0.27*exp(-z/0.357)+0.45*exp(-z/12.82))
    else
       aw=0.
    endif
  end subroutine sw_soloviev_3exp_v2_aw
  !
  !
  !======================
  !
  elemental subroutine sw_ohlmann_v1(z,fxp)
    !
    ! fraction of the solar radiation absorbed by the ocean at the depth z
    !
    ! input:
    ! z:       depth (m)
    !
    ! output:
    ! fxp: fraction of the solar radiation absorbed by the ocean at depth z (w/m^2)
    !
    implicit none
    real(kind=kind_phys),intent(in):: z
    real(kind=kind_phys),intent(out):: fxp
    !
    if(z>0) then
       fxp=.065+11.*z-6.6e-5/z*(1.-exp(-z/8.0e-4))
    else
       fxp=0.
    endif
    !
  end subroutine sw_ohlmann_v1
  !

function grv(lat)
  real(kind=kind_phys) :: lat
  real(kind=kind_phys) :: gamma,c1,c2,c3,c4,pi,phi,x
  gamma=9.7803267715
  c1=0.0052790414
  c2=0.0000232718
  c3=0.0000001262
  c4=0.0000000007
  pi=3.141593
                                                                                                                                                             
  phi=lat*pi/180
  x=sin(phi)
  grv=gamma*(1+(c1*x**2)+(c2*x**4)+(c3*x**6)+(c4*x**8))
  !print *,'grav=',grv,lat
end function grv

subroutine solar_time_from_julian(jday,xlon,soltim)
  !
  ! calculate solar time from the julian date
  !
  implicit none
  real(kind=kind_phys), intent(in)  :: jday
  real(kind=kind_phys), intent(in)  :: xlon
  real(kind=kind_phys), intent(out) :: soltim
  real(kind=kind_phys)                            :: fjd,xhr,xmin,xsec,intime
  integer                                        :: nn
  !
  fjd=jday-floor(jday)
  fjd=jday
  xhr=floor(fjd*24.0)-sign(12.0,fjd-0.5)
  xmin=nint(fjd*1440.0)-(xhr+sign(12.0,fjd-0.5))*60
  xsec=0
  intime=xhr+xmin/60.0+xsec/3600.0+24.0
  soltim=mod(xlon/15.0+intime,24.0)*3600.0
end subroutine solar_time_from_julian

!
!***********************************************************************
!
      subroutine compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compjd      computes julian day and fraction
!   prgmmr: kenneth campana  org: w/nmc23    date: 89-07-07
!
! abstract: computes julian day and fraction
!   from year, month, day and time utc.
!
! program history log:
!   77-05-06  ray orzol,gfdl
!   98-05-15  iredell   y2k compliance
!
! usage:    call compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!   input argument list:
!     jyr      - year (4 digits)
!     jmnth    - month
!     jday     - day
!     jhr      - hour
!     jmn      - minutes 
!   output argument list:
!     jd       - julian day.
!     fjd      - fraction of the julian day.
!
! subprograms called:
!   iw3jdn     compute julian day number
!
! attributes:
!   language: fortran.
!
!$$$
      use machine , only :kind_phys
      implicit none
!
      integer jyr,jmnth,jday,jhr,jmn,jd
      integer iw3jdn
      real (kind=kind_phys) fjd
      jd=iw3jdn(jyr,jmnth,jday)
      if(jhr.lt.12) then
        jd=jd-1
        fjd=0.5+jhr/24.+jmn/1440.
      else
        fjd=(jhr-12)/24.+jmn/1440.
      endif
      end subroutine compjd

end module module_nst_water_prop
