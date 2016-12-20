      module module_microphysics
!
      use machine , only : kind_phys
      use funcphys
      use physcons, cp => con_cp, rd => con_rd, rv => con_rv            &
     &,             t0c => con_t0c, hvap => con_hvap, hfus => con_hfus  &
     &,             eps => con_eps, epsm1 => con_epsm1                  &
     &,             eps1 => con_fvirt, pi => con_pi, grav => con_g 
      implicit none
!
!--- common block of constants used in column microphysics
!
      real,private ::  abfr, cbfr, ciacw, ciacr, c_n0r0,                &
     &cn0r0, cn0r_dmrmin, cn0r_dmrmax, cracw, craut, esw0,              &
     &qautx, rfmax, rqr_dr1, rqr_dr2, rqr_dr3, rqr_drmin,               &
     &rqr_drmax, rr_drmin, rr_dr1, rr_dr2, rr_dr3, rr_drmax
!
      integer, private :: mic_step
!
!--- common block for lookup table used in calculating growth rates of
!    nucleated ice crystals growing in water saturated conditions
!--- discretized growth rates of small ice crystals after their nucleation
!     at 1 c intervals from -1 c to -35 c, based on calculations by miller
!     and young (1979, jas) after 600 s of growth.  resultant growth rates
!     are multiplied by physics time step in gsmconst.
!
      integer, private,parameter :: my_t1=1, my_t2=35
      real,private,dimension(my_t1:my_t2) :: my_growth
!
!--- parameters for ice lookup tables, which establish the range of mean ice
!    particle diameters; from a minimum mean diameter of 0.05 mm (dmimin) to a
!    maximum mean diameter of 1.00 mm (dmimax).  the tables store solutions
!    at 1 micron intervals (deldmi) of mean ice particle diameter.
!
      real, private,parameter :: dmimin=.05e-3,      dmimax=1.e-3,      &
     &                           xmimin=1.e6*dmimin, xmimax=1.e6*dmimax,&
     &                           deldmi=1.e-6
      integer, private,parameter :: mdimin=xmimin, mdimax=xmimax
!
!--- various ice lookup tables
!
      real, private,dimension(mdimin:mdimax) ::                         &
     &      accri,massi,sdens,vsnowi,venti1,venti2
!
!--- mean rain drop diameters varying from 50 microns (0.05 mm) to 450 microns
!      (0.45 mm), assuming an exponential size distribution.
!
      real, private,parameter :: dmrmin=.05e-3,      dmrmax=.45e-3,     &
     &                           xmrmin=1.e6*dmrmin, xmrmax=1.e6*dmrmax,&
     &                           deldmr=1.e-6,       nlimin=100.
!    &,                          nlimin=100., nlimax=20.e3
      integer, private,parameter :: mdrmin=xmrmin, mdrmax=xmrmax
!
!--- factor of 1.5 for recimin, resnowmin, & rerainmin accounts for
!    integrating exponential distributions for effective radius
!    (i.e., the r**3/r**2 moments).
!
!     integer, private, parameter :: indexsmin=300
!!    integer, private, parameter :: indexsmin=200
      integer, private, parameter :: indexsmin=100
      real, private, parameter :: rerainmin=1.5*xmrmin                  &
!    &, recimin=1.5*xmimin, resnowmin=1.5*indexsmin, recwmin=8.0
!    &, recimin=1.5*xmimin, resnowmin=1.5*indexsmin, recwmin=7.5
     &, recimin=1.5*xmimin, resnowmin=1.5*indexsmin, recwmin=10.
!    &, recimin=1.5*xmimin, resnowmin=1.5*indexsmin, recwmin=15.
!    &, recimin=1.5*xmimin, resnowmin=1.5*indexsmin, recwmin=5.

!
!--- various rain lookup tables
!--- rain lookup tables for mean rain drop diameters from dmrmin to dmrmax,
!      assuming exponential size distributions for the rain drops
!
      real, private,dimension(mdrmin:mdrmax)::                          &
     &      accrr,massr,rrate,vrain,ventr1,ventr2
!
!--- common block for riming tables
!--- vel_rf - velocity increase of rimed particles as functions of crude
!      particle size categories (at 0.1 mm intervals of mean ice particle
!      sizes) and rime factor (different values of rime factor of 1.1**n,
!      where n=0 to nrime).
!
      integer, private,parameter :: nrime=40
      real, dimension(2:9,0:nrime),private :: vel_rf
!
!--- the following variables are for microphysical statistics
!
      integer, parameter :: itlo=-60, ithi=40
      integer  nstats(itlo:ithi,4)
      real     qmax(itlo:ithi,5),  qtot(itlo:ithi,22)
!
      real, private,  parameter ::                                      &
!    &  t_ice=-10., t_ice_init=-5.      !- ver1
!!!  &, t_ice=-20.                      !- ver2
     &  t_ice=-40., t_ice_init=-15.     !- ver2
!    &  t_ice=-30., t_ice_init=-5.      !- ver2
!
!     some other miscellaneous parameters
!
      real, private, parameter :: thom=t_ice, tnw=50., toler=1.0e-20    &
!     real, private, parameter :: thom=t_ice, tnw=50., toler=5.e-7
!     real, private, parameter :: thom=-35., tnw=50., toler=5.e-7

! assume fixed cloud ice effective radius
     &, epsq=1.0e-20                                                    &
!    &, epsq=1.e-12, flg0p1=0.1, flg0p2=0.2                             &
     &, flg1p0=1.0
!
!
      contains
!
!#######################################################################
!------- initialize constants & lookup tables for microphysics ---------
!#######################################################################
!
      subroutine gsmconst (dtpg,mype,first)
!
      implicit none
!-------------------------------------------------------------------------------
!---  subprogram documentation block
!   prgrmmr: ferrier         org: w/np22     date: february 2001
!-------------------------------------------------------------------------------
! abstract:
!   * reads various microphysical lookup tables used in column_micro
!   * lookup tables were created "offline" and are read in during execution
!   * creates lookup tables for saturation vapor pressure w/r/t water & ice
!-------------------------------------------------------------------------------
!
! usage: call gsmconst from subroutine gsmdrive at model start time
!
!   input argument list:
!       dtph - physics time step (s)
!
!   output argument list:
!     none
!
!   output files:
!     none
!
!
!   subroutines:
!     my_growth_rates - lookup table for growth of nucleated ice
!
!   unique: none
!
!   library: none
!
!
! attributes:
!   language: fortran 90
!   machine : ibm sp
!
      integer mype
      real    dtpg
      logical first
!
!--- parameters & data statement for local calculations
!
      real, parameter :: c1=1./3., dmr1=.1e-3, dmr2=.2e-3, dmr3=.32e-3, &
     & n0r0=8.e6, rhol=1000.,                                           &
     & xmr1=1.e6*dmr1, xmr2=1.e6*dmr2, xmr3=1.e6*dmr3
      integer, parameter :: mdr1=xmr1, mdr2=xmr2, mdr3=xmr3
!
      real dtph, etime1, etime2, timef, bbfr
      integer i
!
!--- added on 5/16/01 for moorthi
!
      logical, parameter :: read_lookup=.false., write_lookup=.false.
!
!------------------------------------------------------------------------
!  *************  parameters used in eta model -- not used in global model *****
!
!--- dphd, dlmd are delta latitude and longitude at the model (not geodetic) equator
!    => "dx" is the hypotenuse of the model zonal & meridional grid increments.
!
!     dx=111.*(dphd**2+dlmd**2)**.5         ! resolution at model equator (km)
!     dx=min(100., max(5., dx) )
!
!--- assume the following functional relationship for key constants that
!    depend on grid resolution from dxmin (5 km) to dxmax (100 km) resolution:
!
!     dxmin=5.
!     dxmax=100.
!     dx=min(dxmax, max(dxmin, dx) )
!
!--- extune determines the degree to which the coefficients change with resolution.
!    the larger extune is, the more sensitive the parameter.
!
!     extune=1.

!
!--- fxtune ==> f(dx) is the grid-resolution tuning parameter (from 0 to 1)
!
!     fxtune=((dxmax-dx)/(dxmax-dxmin))**extune
!     fxtune=max(0., min(1., fxtune))
!
!--- calculate grid-averaged rh for the onset of condensation (rhgrd) based on
!    simple ***assumed*** (user-specified) values at dxmax and at dxmin.
!
!     rh_dxmax=.90              !-- 90% rh at dxmax=100 km
!     rh_dxmin=.98              !-- 98% rh at dxmin=5 km
!
!--- note that rhgrd is right now fixed throughout the domain!!
!
!     rhgrd=rh_dxmax+(rh_dxmin-rh_dxmax)*fxtune
!   ********************************************************************************
!
!
      if (first) then
!
!--- read in various lookup tables
!
      if ( read_lookup ) then
        open (unit=1,file="eta_micro_lookup.dat",form="unformatted")
        read(1) ventr1
        read(1) ventr2
        read(1) accrr
        read(1) massr
        read(1) vrain
        read(1) rrate
        read(1) venti1
        read(1) venti2
        read(1) accri
        read(1) massi
        read(1) vsnowi
        read(1) vel_rf
!       read(1) my_growth    ! applicable only for dtph=180 s for offline testing
        close (1)
      else
        etime1=timef()
        call ice_lookup                   ! lookup tables for ice
        etime2=timef()
        if (mype == 0)                                                  &
     &  print *,'cpu time (sec) in ice_lookup = ',(etime2-etime1)*0.001
        call rain_lookup                  ! lookup tables for rain
        etime1=timef()
        if (mype == 0)                                                  &
     &  print *,'cpu time (sec) in rain_lookup = ',(etime1-etime2)*0.001
        if (write_lookup) then
          open(unit=1,file='micro_lookup.dat',form='unformatted')
          write(1) ventr1
          write(1) ventr2
          write(1) accrr
          write(1) massr
          write(1) vrain
          write(1) rrate
          write(1) venti1
          write(1) venti2
          write(1) accri
          write(1) massi
          write(1) vsnowi
          write(1) vel_rf
!         write(1) my_growth    ! applicable only for dtph=180 s ????
          close (1)
        endif
      endif
!!
!--- constants associated with biggs (1953) freezing of rain, as parameterized
!    following lin et al. (jcam, 1983) & reisner et al. (1998, qjrms).
!
      abfr=-0.66
      bbfr=100.
      cbfr=20.*pi*pi*bbfr*rhol*1.e-21
!
!--- qaut0 is the threshold cloud content for autoconversion to rain
!      needed for droplets to reach a diameter of 20 microns (following
!      manton and cotton, 1977; banta and hanson, 1987, jcam).  it is
!      **strongly** affected by the assumed droplet number concentrations
!     xncw!  for example, qaut0=1.2567, 0.8378, or 0.4189 g/m**3 for
!     droplet number concentrations of 300, 200, and 100 cm**-3, respectively.
!
!--- calculate grid-averaged xncw based on simple ***assumed*** (user-specified)
!    values at dxmax and at dxmin.
!
!     xncw_dxmax=50.e6          !--  50 /cm**3 at dxmax=100 km
!     xncw_dxmin=200.e6         !-- 200 /cm**3 at dxmin=5 km
!
!--- note that xncw is right now fixed throughout the domain!!
!
!     xncw=xncw_dxmax+(xncw_dxmin-xncw_dxmax)*fxtune
!
!     qaut0=pi*rhol*xncw*(20.e-6)**3/6.
      qautx=pi*rhol*1.0e6*(20.e-6)**3/6.
!
!--- based on rain lookup tables for mean diameters from 0.05 to 0.45 mm
!    * four different functional relationships of mean drop diameter as
!      a function of rain rate (rr), derived based on simple fits to
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.
!
      rr_drmin=n0r0*rrate(mdrmin)     ! rr for mean drop diameter of .05 mm
      rr_dr1=n0r0*rrate(mdr1)         ! rr for mean drop diameter of .10 mm
      rr_dr2=n0r0*rrate(mdr2)         ! rr for mean drop diameter of .20 mm
      rr_dr3=n0r0*rrate(mdr3)         ! rr for mean drop diameter of .32 mm
      rr_drmax=n0r0*rrate(mdrmax)     ! rr for mean drop diameter of .45 mm
!
      rqr_drmin=n0r0*massr(mdrmin)    ! rain content for mean drop diameter of .05 mm
      rqr_dr1=n0r0*massr(mdr1)        ! rain content for mean drop diameter of .10 mm
      rqr_dr2=n0r0*massr(mdr2)        ! rain content for mean drop diameter of .20 mm
      rqr_dr3=n0r0*massr(mdr3)        ! rain content for mean drop diameter of .32 mm
      rqr_drmax=n0r0*massr(mdrmax)    ! rain content for mean drop diameter of .45 mm
      c_n0r0=pi*rhol*n0r0
      cn0r0=1.e6/c_n0r0**.25
      cn0r_dmrmin=1./(pi*rhol*dmrmin**4)
      cn0r_dmrmax=1./(pi*rhol*dmrmax**4)
!
      endif                     !  if (first) then loop ends here
!
!     find out what microphysics time step should be
!
      mic_step = max(1, int(dtpg/600.0+0.5))
!     mic_step = max(1, int(dtpg/300.0+0.5))
      dtph     = dtpg / mic_step
      if (mype == 0) print *,' dtpg=',dtpg,' mic_step=',mic_step        &
     &,                ' dtph=',dtph
!
!--- calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
      call my_growth_rates (dtph)
!
!--- ciacw is used in calculating riming rates
!      the assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
!moor ciacw=dtph*0.25*pi*0.5*(1.e5)**c1   ! commented on 20050422
!      ice is =0.1 below:
      ciacw=dtph*0.25*pi*0.1*(1.e5)**c1
!     ciacw = 0.0      ! brad's suggestion 20040614
!
!--- ciacr is used in calculating freezing of rain colliding with large ice
!      the assumed collection efficiency is 1.0
!
      ciacr=pi*dtph
!
!--- cracw is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
!moor cracw=dtph*0.25*pi*1.0                 ! commented on 20050422
!
!      assumed collection efficiency of 0.1)
      cracw=dtph*0.25*pi*0.1
!     cracw = 0.0      ! brad's suggestion 20040614
!
      esw0=fpvsl(t0c)           ! saturation vapor pressure at 0c
      rfmax=1.1**nrime          ! maximum rime factor allowed
!
!------------------------------------------------------------------------
!--------------- constants passed through argument list -----------------
!------------------------------------------------------------------------
!
!--- important parameters for self collection (autoconversion) of
!    cloud water to rain.
!
!--- craut is proportional to the rate that cloud water is converted by
!      self collection to rain (autoconversion rate)
!
      craut=1.-exp(-1.e-3*dtph)
!
!     if (mype == 0)
!    & write(6,"(a, a,f6.2,a, a,f5.4, a,f7.3,a, a,f6.2,a, a,f6.3,a)")
!    &   'key microphysical parameters for '
!    &  ,'dx=',dx,' km:'
!    &  ,'   fxtune=',fxtune
!    &  ,'   rhgrd=',100.*rhgrd,' %'
!    &  ,'   ncw=',1.e-6*xncw,' /cm**3'
!    &  ,'   qaut0=',1.e3*qaut0,' g/kg'
!
!--- for calculating snow optical depths by considering bulk density of
!      snow based on emails from q. fu (6/27-28/01), where optical
!      depth (t) = 1.5*swp/(reff*dens), swp is snow water path, reff
!      is effective radius, and dens is the bulk density of snow.
!
!    swp (kg/m**2)=(1.e-3 kg/g)*swprad, swprad in g/m**2 used in radiation
!    t = 1.5*1.e3*swprad/(reff*dens)
!
!    see derivation for massi(indexs), note equal to rho*qsnow/nsnow
!
!    sdens=1.5e3/dens, dens=massi(indexs)/[pi*(1.e-6*indexs)**3]
!
      do i=mdimin,mdimax
!moorthisdens(i)=pi*1.5e-15*float(i*i*i)/massi(i)
        sdens(i)=pi*1.0e-15*float(i*i*i)/massi(i)
      enddo
!
!-----------------------------------------------------------------------
!
      end subroutine gsmconst

!
!#######################################################################
!--- sets up lookup table for calculating initial ice crystal growth ---
!#######################################################################
!
      subroutine my_growth_rates (dtph)
!
      implicit none
!
!--- below are tabulated values for the predicted mass of ice crystals
!    after 600 s of growth in water saturated conditions, based on 
!    calculations from miller and young (jas, 1979).  these values are
!    crudely estimated from tabulated curves at 600 s from fig. 6.9 of
!    young (1993).  values at temperatures colder than -27c were 
!    assumed to be invariant with temperature.  
!
!--- used to normalize miller & young (1979) calculations of ice growth
!    over large time steps using their tabulated values at 600 s.
!    assumes 3d growth with time**1.5 following eq. (6.3) in young (1993).
!
      real dtph, dt_ice
      real my_600(my_t1:my_t2)
!
!-- 20090714: these values are in g and need to be converted to kg below
      data my_600 /                                                     &
     & 5.5e-8,  1.4e-7,  2.8e-7, 6.e-7,   3.3e-6,                       & !  -1 to  -5 deg c
     & 2.e-6,   9.e-7,   8.8e-7, 8.2e-7,  9.4e-7,                       & !  -6 to -10 deg c
     & 1.2e-6,  1.85e-6, 5.5e-6, 1.5e-5,  1.7e-5,                       & ! -11 to -15 deg c
     & 1.5e-5,  1.e-5,   3.4e-6, 1.85e-6, 1.35e-6,                      & ! -16 to -20 deg c
     & 1.05e-6, 1.e-6,   9.5e-7, 9.0e-7 , 9.5e-7,                       &  ! -21 to -25 deg c
     & 9.5e-7,  9.e-7,   9.e-7,  9.e-7,   9.e-7,                        &  ! -26 to -30 deg c
     & 9.e-7,   9.e-7,   9.e-7,  9.e-7,   9.e-7 /                         ! -31 to -35 deg c
!
!-----------------------------------------------------------------------
!
      dt_ice=(dtph/600.)**1.5
!     my_growth=dt_ice*my_600          ! original version
      my_growth=dt_ice*my_600*1.e-3    !-- 20090714: convert from g to kg
!
!-----------------------------------------------------------------------
!
      end subroutine my_growth_rates
!
!#######################################################################
!--------------- creates lookup tables for ice processes ---------------
!#######################################################################
!
      subroutine ice_lookup
!
      implicit none
!-----------------------------------------------------------------------------------
!
!---- key diameter values in mm
!
!-----------------------------------------------------------------------------------
!
!---- key concepts:
!       - actual physical diameter of particles (d)
!       - ratio of actual particle diameters to mean diameter (x=d/md)
!       - mean diameter of exponentially distributed particles, which is the
!         same as 1./lamda of the distribution (md)
!       - all quantitative relationships relating ice particle characteristics as
!         functions of their diameter (e.g., ventilation coefficients, normalized
!         accretion rates, ice content, and mass-weighted fall speeds) are a result
!         of using composite relationships for ice crystals smaller than 1.5 mm
!         diameter merged with analogous relationships for larger sized aggregates.
!         relationships are derived as functions of mean ice particle sizes assuming
!         exponential size spectra and assuming the properties of ice crystals at
!         sizes smaller than 1.5 mm and aggregates at larger sizes.  
!
!-----------------------------------------------------------------------------------
!
!---- actual ice particle diameters for which integrated distributions are derived
!       - dmini - minimum diameter for integration (.02 mm, 20 microns)
!       - dmaxi - maximum diameter for integration (2 cm)
!       - ddeli - interval for integration (1 micron)
!
      real, parameter :: dmini=.02e-3, dmaxi=20.e-3, ddeli=1.e-6,       &
     &  ximin=1.e6*dmini, ximax=1.e6*dmaxi
      integer, parameter :: idimin=ximin, idimax=ximax
!
!---- meaning of the following arrays:
!        - diam - ice particle diameter (m)
!        - mass - ice particle mass (kg)
!        - vel  - ice particle fall speeds (m/s)
!        - vent1 - 1st term in ice particle ventilation factor
!        - vent2 - 2nd term in ice particle ventilation factor
!
      real diam(idimin:idimax),mass(idimin:idimax),vel(idimin:idimax),  &
     & vent1(idimin:idimax),vent2(idimin:idimax)
!
!-----------------------------------------------------------------------------------
!
!---- found from trial & error that the m(d) & v(d) mass & velocity relationships
!       between the ice crystals and aggregates overlapped & merged near a particle
!       diameter sizes of 1.5 mm.  thus, ice crystal relationships are used for
!       sizes smaller than 1.5 mm and aggregate relationships for larger sizes.
!
      real, parameter :: d_crystal_max=1.5
!
!---- the quantity xmax represents the maximum value of "x" in which the
!       integrated values are calculated.  for xmax=20., this means that
!       integrated ventilation, accretion, mass, and precipitation rates are
!       calculated for ice particle sizes less than 20.*mdiam, the mean particle diameter.
!
      real, parameter :: xmax=20.
!
!-----------------------------------------------------------------------------------
!
!---- meaning of the following arrays:
!        - mdiam - mean diameter (m)
!        - venti1 - integrated quantity associated w/ ventilation effects
!                   (capacitance only) for calculating vapor deposition onto ice
!        - venti2 - integrated quantity associated w/ ventilation effects
!                   (with fall speed) for calculating vapor deposition onto ice
!        - accri  - integrated quantity associated w/ cloud water collection by ice
!        - massi  - integrated quantity associated w/ ice mass 
!        - vsnowi - mass-weighted fall speed of snow, used to calculate precip rates
!
!--- mean ice-particle diameters varying from 50 microns to 1000 microns (1 mm), 
!      assuming an exponential size distribution.  
!
      real mdiam
!
!-----------------------------------------------------------------------------------
!------------- constants & parameters for ventilation factors of ice ---------------
!-----------------------------------------------------------------------------------
!
!---- these parameters are used for calculating the ventilation factors for ice
!       crystals between 0.2 and 1.5 mm diameter (hall and pruppacher, jas, 1976).  
!       from trial & error calculations, it was determined that the ventilation
!       factors of smaller ice crystals could be approximated by a simple linear
!       increase in the ventilation coefficient from 1.0 at 50 microns (.05 mm) to 
!       1.1 at 200 microns (0.2 mm), rather than using the more complex function of
!       1.0 + .14*(sc**.33*re**.5)**2 recommended by hall & pruppacher.
!
      real, parameter :: cvent1i=.86, cvent2i=.28
!
!---- these parameters are used for calculating the ventilation factors for larger
!       aggregates, where d>=1.5 mm (see rutledge and hobbs, jas, 1983; 
!       thorpe and mason, 1966).
!
      real, parameter :: cvent1a=.65, cvent2a=.44
!
      real m_agg,m_bullet,m_column,m_ice,m_plate
!
!---- various constants
!
      real, parameter :: c1=2./3., cexp=1./3.
!
      logical :: iprint
      logical, parameter :: print_diag=.false.
!
!-----------------------------------------------------------------------------------
!- constants & parameters for calculating the increase in fall speed of rimed ice --
!-----------------------------------------------------------------------------------
!
!---- constants & arrays for estimating increasing fall speeds of rimed ice.
!     based largely on theory and results from bohm (jas, 1989, 2419-2427).
!
!-------------------- standard atmosphere conditions at 1000 mb --------------------
!
      real, parameter :: t_std=288., dens_std=1000.e2/(287.04*288.)
!
!---- these "bulk densities" are the actual ice densities in the ice portion of the 
!     lattice.  they are based on text associated w/ (12) on p. 2425 of bohm (jas, 
!     1989).  columns, plates, & bullets are assumed to have an average bulk density 
!     of 850 kg/m**3.  aggregates were assumed to have a slightly larger bulk density 
!     of 600 kg/m**3 compared with dendrites (i.e., the least dense, most "lacy" & 
!     tenous ice crystal, which was assumed to be ~500 kg/m**3 in bohm).  
!
      real, parameter :: dens_crystal=850., dens_agg=600.
!
!--- a value of nrime=40 for a logarithmic ratio of 1.1 yields a maximum rime factor
!      of 1.1**40 = 45.26 that is resolved in these tables.  this allows the largest
!      ice particles with a mean diameter of mdimax=1000 microns to achieve bulk 
!      densities of 900 kg/m**3 for rimed ice.  
!
!     integer, parameter :: nrime=40
      real m_rime,                                                      &
     &     rime_factor(0:nrime), rime_vel(0:nrime),                     &
     &     vel_rime(idimin:idimax,nrime), ivel_rime(mdimin:mdimax,nrime)
!
      integer i, j, jj, k, icount
      real c2,      cbulk, cbulk_ice, px, dynvis_std, crime1            &
     &,    crime2,  crime3, crime4, crime5, d, c_avg, c_agg             &
     &,    c_bullet, c_column, c_plate, cl_agg, cl_bullet               &
     &,    cl_column, cl_plate, v_agg, v_bullet, v_column               &
     &,    v_plate,   wd,       ecc_column                              &
     &,    cvent1,    cvent2, crime_best, rime_m1, rime_m2              &
     &,    x_rime,    re_rime, smom3, pratei, expf                      &
     &,    bulk_dens, xmass,  xmdiam, ecc_plate, dx
!
!-----------------------------------------------------------------------------------
!----------------------------- begin execution -------------------------------------
!-----------------------------------------------------------------------------------
!
!
      c2=1./sqrt(3.)
!     pi=acos(-1.)
      cbulk=6./pi
      cbulk_ice=900.*pi/6.    ! maximum bulk ice density allowed of 900 kg/m**3
      px=.4**cexp             ! convert fall speeds from 400 mb (starr & cox) to 1000 mb
!
!--------------------- dynamic viscosity (1000 mb, 288 k) --------------------------
!
      dynvis_std=1.496e-6*t_std**1.5/(t_std+120.)
      crime1=pi/24.
      crime2=8.*9.81*dens_std/(pi*dynvis_std**2)
      crime3=crime1*dens_crystal
      crime4=crime1*dens_agg
      crime5=dynvis_std/dens_std
      do i=0,nrime
        rime_factor(i)=1.1**i
      enddo
!
!#######################################################################
!      characteristics as functions of actual ice particle diameter 
!#######################################################################
!
!----   m(d) & v(d) for 3 categories of ice crystals described by starr 
!----   & cox (1985). 
!
!----   capacitance & characteristic lengths for reynolds number calculations
!----   are based on young (1993; p. 144 & p. 150).  c-axis & a-axis 
!----   relationships are from heymsfield (jas, 1972; table 1, p. 1351).
!
      icount=60
!
      if (print_diag)                                                   & 
     &  write(7,"(2a)") '---- increase in fall speeds of rimed ice',    &
     &    ' particles as function of ice particle diameter ----'
      do i=idimin,idimax
        if (icount == 60 .and. print_diag) then
          write(6,"(/2a/3a)") 'particle masses (mg), fall speeds ',     &
     &      '(m/s), and ventilation factors',                           &
     &      '  d(mm)  cr_mass   mass_bull   mass_col  mass_plat ',      &
     &      '  mass_agg   cr_vel  v_bul cr_col cr_pla aggreg',          &
     &      '    vent1      vent2 '                               
          write(7,"(3a)") '        <----------------------------------',&
     &      '---------------  rime factor  --------------------------', &
     &      '--------------------------->'
          write(7,"(a,23f5.2)") '  d(mm)',(rime_factor(k), k=1,5),      &
     &       (rime_factor(k), k=6,40,2)
          icount=0
        endif
        d=(float(i)+.5)*1.e-3    ! in mm
        c_avg=0.
        c_agg=0.
        c_bullet=0.
        c_column=0.
        c_plate=0.
        cl_agg=0.
        cl_bullet=0.
        cl_column=0.
        cl_plate=0.
        m_agg=0.
        m_bullet=0.
        m_column=0.
        m_plate=0.
        v_agg=0.
        v_bullet=0.
        v_column=0.
        v_plate=0.
        if (d < d_crystal_max) then
!
!---- this block of code calculates bulk characteristics based on average
!     characteristics of bullets, plates, & column ice crystals <1.5 mm size
!
!---- mass-diameter relationships from heymsfield (1972) & used
!       in starr & cox (1985), units in mg
!---- "d" is maximum dimension size of crystal in mm, 
!
! mass of pure ice for spherical particles, used as an upper limit for the
!   mass of small columns (<~ 80 microns) & plates (<~ 35 microns)
!
          m_ice=.48*d**3   ! mass of pure ice for spherical particle
!
          m_bullet=min(.044*d**3, m_ice)
          m_column=min(.017*d**1.7, m_ice)
          m_plate=min(.026*d**2.5, m_ice)
!
          mass(i)=m_bullet+m_column+m_plate
!
!---- these relationships are from starr & cox (1985), applicable at 400 mb
!---- "d" is maximum dimension size of crystal in mm, dx in microns
!
          dx=1000.*d            ! convert from mm to microns
          if (dx <= 200.) then
            v_column=8.114e-5*dx**1.585
            v_bullet=5.666e-5*dx**1.663
            v_plate=1.e-3*dx
          else if (dx <= 400.) then
            v_column=4.995e-3*dx**.807
            v_bullet=3.197e-3*dx**.902
            v_plate=1.48e-3*dx**.926
          else if (dx <= 600.) then
            v_column=2.223e-2*dx**.558
            v_bullet=2.977e-2*dx**.529
            v_plate=9.5e-4*dx
          else if (dx <= 800.) then
            v_column=4.352e-2*dx**.453
            v_bullet=2.144e-2*dx**.581
            v_plate=3.161e-3*dx**.812
          else 
            v_column=3.833e-2*dx**.472
            v_bullet=3.948e-2*dx**.489
            v_plate=7.109e-3*dx**.691
          endif
!
!---- reduce fall speeds from 400 mb to 1000 mb
!
          v_column=px*v_column
          v_bullet=px*v_bullet
          v_plate=px*v_plate
!
!---- different version!  calculates mass-weighted crystal fall speeds
!
          vel(i)=(m_bullet*v_bullet+m_column*v_column+m_plate*v_plate)/ &
     &           mass(i)
          mass(i)=mass(i)/3.
!
!---- shape factor and characteristic length of various ice habits,
!     capacitance is equal to 4*pi*(shape factor)
!       see young (1993, pp. 143-152 for guidance)
!
!---- bullets:
!
!---- shape factor for bullets (heymsfield, 1975)
          c_bullet=.5*d
!---- length-width functions for bullets from heymsfield (jas, 1972)
          if (d > 0.3) then
            wd=.25*d**.7856     ! width (mm); a-axis
          else
            wd=.185*d**.552
          endif
!---- characteristic length for bullets (see first multiplicative term on right
!       side of eq. 7 multiplied by crystal width on p. 821 of heymsfield, 1975)
          cl_bullet=.5*pi*wd*(.25*wd+d)/(d+wd)
!
!---- plates:
!
!---- length-width function for plates from heymsfield (jas, 1972)
          wd=.0449*d**.449      ! width or thickness (mm); c-axis
!---- eccentricity & shape factor for thick plates following young (1993, p. 144)
          ecc_plate=sqrt(1.-wd*wd/(d*d))         ! eccentricity
          c_plate=d*ecc_plate/asin(ecc_plate)    ! shape factor
!---- characteristic length for plates following young (1993, p. 150, eq. 6.6)
          cl_plate=d+2.*wd      ! characteristic lengths for plates
!
!---- columns:
!
!---- length-width function for columns from heymsfield (jas, 1972)
          if (d > 0.2) then
            wd=.1973*d**.414    ! width (mm); a-axis
          else
            wd=.5*d             ! width (mm); a-axis
          endif
!---- eccentricity & shape factor for columns following young (1993, p. 144)
          ecc_column=sqrt(1.-wd*wd/(d*d))                     ! eccentricity
          c_column=ecc_column*d/log((1.+ecc_column)*d/wd)     ! shape factor
!---- characteristic length for columns following young (1993, p. 150, eq. 6.7)
          cl_column=(wd+2.*d)/(c1+c2*d/wd)       ! characteristic lengths for columns
!
!---- convert shape factor & characteristic lengths from mm to m for 
!       ventilation calculations
!
          c_bullet=.001*c_bullet
          c_plate=.001*c_plate
          c_column=.001*c_column
          cl_bullet=.001*cl_bullet
          cl_plate=.001*cl_plate
          cl_column=.001*cl_column
!
!---- make a smooth transition between a ventilation coefficient of 1.0 at 50 microns
!       to 1.1 at 200 microns
!
          if (d > 0.2) then
            cvent1=cvent1i
            cvent2=cvent2i/3.
          else
            cvent1=1.0+.1*max(0., d-.05)/.15
            cvent2=0.
          endif
!
!---- ventilation factors for ice crystals:
!
          vent1(i)=cvent1*(c_bullet+c_plate+c_column)/3.
          vent2(i)=cvent2*(c_bullet*sqrt(v_bullet*cl_bullet)            &
     &                    +c_plate*sqrt(v_plate*cl_plate)               &
     &                    +c_column*sqrt(v_column*cl_column) )
          crime_best=crime3     ! for calculating best no. of rimed ice crystals
        else
!
!---- this block of code calculates bulk characteristics based on average
!     characteristics of unrimed aggregates >= 1.5 mm using locatelli & 
!     hobbs (jgr, 1974, 2185-2197) data.
!
!----- this category is a composite of aggregates of unrimed radiating 
!-----   assemblages of dendrites or dendrites; aggregates of unrimed
!-----   radiating assemblages of plates, side planes, bullets, & columns;
!-----   aggregates of unrimed side planes (mass in mg, velocity in m/s)
!
          m_agg=(.073*d**1.4+.037*d**1.9+.04*d**1.4)/3.
          v_agg=(.8*d**.16+.69*d**.41+.82*d**.12)/3.
          mass(i)=m_agg
          vel(i)=v_agg
!
!---- assume spherical aggregates
!
!---- shape factor is the same as for bullets, = d/2
          c_agg=.001*.5*d         ! units of m
!---- characteristic length is surface area divided by perimeter
!       (.25*pi*d**2)/(pi*d**2) = d/4
          cl_agg=.5*c_agg         ! units of m
!
!---- ventilation factors for aggregates:
!
          vent1(i)=cvent1a*c_agg
          vent2(i)=cvent2a*c_agg*sqrt(v_agg*cl_agg)
          crime_best=crime4     ! for calculating best no. of rimed aggregates
        endif
!
!---- convert from shape factor to capacitance for ventilation factors
!
        vent1(i)=4.*pi*vent1(i)
        vent2(i)=4.*pi*vent2(i)
        diam(i)=1.e-3*d             ! convert from mm to m
        mass(i)=1.e-6*mass(i)       ! convert from mg to kg
!
!---- calculate increase in fall speeds of individual rimed ice particles
!
        do k=0,nrime
!---- mass of rimed ice particle associated with rime_factor(k)
          rime_m1=rime_factor(k)*mass(i)
          rime_m2=cbulk_ice*diam(i)**3
          m_rime=min(rime_m1, rime_m2)
!---- best number (x) of rimed ice particle combining eqs. (8) & (12) in bohm
          x_rime=crime2*m_rime*(crime_best/m_rime)**.25
!---- reynolds number for rimed ice particle using eq. (11) in bohm
          re_rime=8.5*(sqrt(1.+.1519*sqrt(x_rime))-1.)**2
          rime_vel(k)=crime5*re_rime/diam(i)
        enddo
        do k=1,nrime
          vel_rime(i,k)=rime_vel(k)/rime_vel(0)
        enddo
        if (print_diag) then
   !
   !---- determine if statistics should be printed out.
   !
          iprint=.false.
          if (d <= 1.) then
            if (mod(i,10) == 0) iprint=.true.
          else
            if (mod(i,100) == 0) iprint=.true.
          endif
          if (iprint) then
            write(6,"(f7.4,5e11.4,1x,5f7.4,1x,2e11.4)")                 & 
     &        d,1.e6*mass(i),m_bullet,m_column,m_plate,m_agg,           &
     &        vel(i),v_bullet,v_column,v_plate,v_agg,                   &
     &        vent1(i),vent2(i)
            write(7,"(f7.4,23f5.2)") d,(vel_rime(i,k), k=1,5),          &
     &        (vel_rime(i,k), k=6,40,2)
            icount=icount+1
          endif
        endif
      enddo
!
!#######################################################################
!      characteristics as functions of mean particle diameter
!#######################################################################
!
      venti1=0.
      venti2=0.
      accri=0.
      massi=0.
      vsnowi=0.
      vel_rf=0.
      ivel_rime=0.
      icount=0
      if (print_diag) then
        icount=60
        write(6,"(/2a)") '------------- statistics as functions of ',   &
     &    'mean particle diameter -------------'
        write(7,"(/2a)") '------ increase in fall speeds of rimed ice', &
     &    ' particles as functions of mean particle diameter -----'
      endif
      do j=mdimin,mdimax
        if (icount == 60 .and. print_diag) then
          write(6,"(/2a)") 'd(mm)    vent1      vent2    ',             &
     &       'accrete       mass     vel  dens  '
          write(7,"(3a)") '      <----------------------------------',  &
     &      '---------------  rime factor  --------------------------', &
     &      '--------------------------->'
          write(7,"(a,23f5.2)") 'd(mm)',(rime_factor(k), k=1,5),        &
     &       (rime_factor(k), k=6,40,2)
          icount=0
        endif
        mdiam=deldmi*float(j)       ! in m
        smom3=0.
        pratei=0.
        rime_vel=0.                 ! note that this array is being reused!
        do i=idimin,idimax
          dx=diam(i)/mdiam
          if (dx <= xmax) then      ! to prevent arithmetic underflows
            expf=exp(-dx)*ddeli
            venti1(j)=venti1(j)+vent1(i)*expf
            venti2(j)=venti2(j)+vent2(i)*expf
            accri(j)=accri(j)+diam(i)*diam(i)*vel(i)*expf
            xmass=mass(i)*expf
            do k=1,nrime
              rime_vel(k)=rime_vel(k)+xmass*vel_rime(i,k)
            enddo
            massi(j)=massi(j)+xmass
            pratei=pratei+xmass*vel(i)
            smom3=smom3+diam(i)**3*expf
          else
            exit
          endif
        enddo
   !
   !--- increased fall velocities functions of mean diameter (j),
   !      normalized by ice content, and rime factor (k) 
   !
        do k=1,nrime
          ivel_rime(j,k)=rime_vel(k)/massi(j)
        enddo
   !
   !--- increased fall velocities functions of ice content at 0.1 mm
   !      intervals (j_100) and rime factor (k); accumulations here
   !
        jj=j/100
        if (jj >= 2 .and. jj <= 9) then
          do k=1,nrime
            vel_rf(jj,k)=vel_rf(jj,k)+ivel_rime(j,k)
          enddo
        endif
        bulk_dens=cbulk*massi(j)/smom3
        venti1(j)=venti1(j)/mdiam
        venti2(j)=venti2(j)/mdiam
        accri(j)=accri(j)/mdiam
        vsnowi(j)=pratei/massi(j)
        massi(j)=massi(j)/mdiam
        if (mod(j,10) == 0 .and. print_diag) then
          xmdiam=1.e3*mdiam
          write(6,"(f6.3,4e11.4,f6.3,f8.3)") xmdiam,venti1(j),venti2(j),&
     &      accri(j),massi(j),vsnowi(j),bulk_dens
          write(7,"(f6.3,23f5.2)") xmdiam,(ivel_rime(j,k), k=1,5),      &
     &       (ivel_rime(j,k), k=6,40,2)
          icount=icount+1
        endif
      enddo
!
!--- average increase in fall velocities rimed ice as functions of mean
!      particle diameter (j, only need 0.1 mm intervals) and rime factor (k)
!
      if (print_diag) then
        write(7,"(/2a)") ' ------- increase in fall speeds of rimed ',  &
     &    'ice particles at reduced, 0.1-mm intervals  --------'
        write(7,"(3a)") '        <----------------------------------',  &
     &    '---------------  rime factor  --------------------------',   &
     &    '--------------------------->'
        write(7,"(a,23f5.2)") 'd(mm)',(rime_factor(k), k=1,5),          &
     &    (rime_factor(k), k=6,40,2)
      endif
      do j=2,9
        vel_rf(j,0)=1.
        do k=1,nrime
          vel_rf(j,k)=.01*vel_rf(j,k)
        enddo
        if (print_diag) write(7,"(f4.1,2x,23f5.2)") 0.1*j,              &
     &    (vel_rf(j,k), k=1,5),(vel_rf(j,k), k=6,40,2)
      enddo
!
!-----------------------------------------------------------------------------------
!
      end subroutine ice_lookup
!
!#######################################################################
!-------------- creates lookup tables for rain processes ---------------
!#######################################################################
!
      subroutine rain_lookup
      implicit none
!
!--- parameters & arrays for fall speeds of rain as a function of rain drop
!      diameter.  these quantities are integrated over exponential size
!      distributions of rain drops at 1 micron intervals (ddelr) from minimum 
!      drop sizes of .05 mm (50 microns, dminr) to maximum drop sizes of 10 mm 
!      (dmaxr). 
!
      real, parameter :: dminr=.05e-3, dmaxr=10.e-3, ddelr=1.e-6,       &
     & xrmin=1.e6*dminr, xrmax=1.e6*dmaxr
      integer, parameter :: idrmin=xrmin, idrmax=xrmax
      real diam(idrmin:idrmax), vel(idrmin:idrmax)
!
!--- parameters rain lookup tables, which establish the range of mean drop
!      diameters; from a minimum mean diameter of 0.05 mm (dmrmin) to a 
!      maximum mean diameter of 0.45 mm (dmrmax).  the tables store solutions
!      at 1 micron intervals (deldmr) of mean drop diameter.  
!
      real mdiam
!
      logical, parameter :: print_diag=.false.
!
      real d, cmass, pi2, expf
      integer i, j, i1, i2
!
!-----------------------------------------------------------------------
!------- fall speeds of rain as function of rain drop diameter ---------
!-----------------------------------------------------------------------
!
      do i=idrmin,idrmax
        diam(i)=float(i)*ddelr
        d=100.*diam(i)         ! diameter in cm
        if (d <= .42) then
   !
   !--- rutledge & hobbs (1983); vel (m/s), d (cm)
   !
          vel(i)=max(0., -.267+51.5*d-102.25*d*d+75.7*d**3)
        else if (d > 0.42 .and. d <= .58) then
   !
   !--- linear interpolation of gunn & kinzer (1949) data
   !
          vel(i)=8.92+.25/(.58-.42)*(d-.42)
        else
          vel(i)=9.17
        endif
      enddo
      do i=1,100
        i1=(i-1)*100+idrmin
        i2=i1+90
   !
   !--- print out rain fall speeds only for d<=5.8 mm (.58 cm)
   !
        if (diam(i1) > .58e-2) exit
        if (print_diag) then
          write(6,"(1x)")
          write(6,"('d(mm)->  ',10f7.3)") (1000.*diam(j), j=i1,i2,10)
          write(6,"('v(m/s)-> ',10f7.3)") (vel(j), j=i1,i2,10)
        endif
      enddo
!
!-----------------------------------------------------------------------
!------------------- lookup tables for rain processes ------------------
!-----------------------------------------------------------------------
!
!     pi=acos(-1.)
      pi2=2.*pi
      cmass=1000.*pi/6.
      if (print_diag) then
        write(6,"(/'diam - mean diameter (mm)'                          &
     &          /'ventr1 - 1st ventilation coefficient (m**2)'          &
     &          /'ventr2 - 2nd ventilation coefficient (m**3/s**.5)'    &
     &          /'accrr - accretion moment (m**4/s)'                    &
     &          /'rho*qr - mass content (kg/m**3) for n0r=8e6'          &
     &          /'rrate - rain rate moment (m**5/s)'                    &
     &          /'vr - mass-weighted rain fall speed (m/s)'             &
     &    /' diam      ventr1      ventr2       accrr      ',           &
     &    'rho*qr       rrate    vr')")
      endif
      do j=mdrmin,mdrmax
        mdiam=float(j)*deldmr
        ventr2(j)=0.
        accrr(j)=0.
        massr(j)=0.
        rrate(j)=0.
        do i=idrmin,idrmax
          expf=exp(-diam(i)/mdiam)*ddelr
          ventr2(j)=ventr2(j)+diam(i)**1.5*vel(i)**.5*expf
          accrr(j)=accrr(j)+diam(i)*diam(i)*vel(i)*expf
          massr(j)=massr(j)+diam(i)**3*expf
          rrate(j)=rrate(j)+diam(i)**3*vel(i)*expf
        enddo
   !
   !--- derived based on ventilation, f(d)=0.78+.31*schmidt**(1/3)*reynold**.5,
   !      where reynold=(v*d*rho/dyn_vis), v is velocity, d is particle diameter,
   !      rho is air density, & dyn_vis is dynamic viscosity.  only terms 
   !      containing velocity & diameter are retained in these tables.  
   !
        ventr1(j)=.78*pi2*mdiam**2
        ventr2(j)=.31*pi2*ventr2(j)
   !
        massr(j)=cmass*massr(j)
        rrate(j)=cmass*rrate(j)
        vrain(j)=rrate(j)/massr(j)
        if (print_diag) write(6,"(f6.3,5g12.5,f6.3)") 1000.*mdiam,      &
     &    ventr1(j),ventr2(j),accrr(j),8.e6*massr(j),rrate(j),vrain(j)
      enddo
!
!-----------------------------------------------------------------------
!
      end subroutine rain_lookup
!
!###############################################################################
! ***** version of microphysics designed for higher resolution meso eta model
!       (1) represents sedimentation by preserving a portion of the precipitation
!           through top-down integration from cloud-top.  modified procedure to
!           zhao and carr (1997).
!       (2) microphysical equations are modified to be less sensitive to time
!           steps by use of clausius-clapeyron equation to account for changes in
!           saturation mixing ratios in response to latent heating/cooling.  
!       (3) prevent spurious temperature oscillations across 0c due to 
!           microphysics.
!       (4) uses lookup tables for: calculating two different ventilation 
!           coefficients in condensation and deposition processes; accretion of
!           cloud water by precipitation; precipitation mass; precipitation rate
!           (and mass-weighted precipitation fall speeds).
!       (5) assumes temperature-dependent variation in mean diameter of large ice
!           (houze et al., 1979; ryan et al., 1996).
!        -> 8/22/01: this relationship has been extended to colder temperatures
!           to parameterize smaller large-ice particles down to mean sizes of mdimin,
!           which is 50 microns reached at -55.9c.
!       (6) attempts to differentiate growth of large and small ice, mainly for
!           improved transition from thin cirrus to thick, precipitating ice
!           anvils.
!        -> 8/22/01: this feature has been diminished by effectively adjusting to
!           ice saturation during depositional growth at temperatures colder than
!           -10c.  ice sublimation is calculated more explicitly.  the logic is
!           that sources of are either poorly understood (e.g., nucleation for nwp) 
!           or are not represented in the eta model (e.g., detrainment of ice from 
!           convection).  otherwise the model is too wet compared to the radiosonde
!           observations based on 1 feb - 18 march 2001 retrospective runs.  
!       (7) top-down integration also attempts to treat mixed-phase processes,
!           allowing a mixture of ice and water.  based on numerous observational
!           studies, ice growth is based on nucleation at cloud top &
!           subsequent growth by vapor deposition and riming as the ice particles 
!           fall through the cloud.  effective nucleation rates are a function
!           of ice supersaturation following meyers et al. (jam, 1992).  
!        -> 8/22/01: the simulated relative humidities were far too moist compared 
!           to the rawinsonde observations.  this feature has been substantially 
!           diminished, limited to a much narrower temperature range of 0 to -10c.  
!       (8) depositional growth of newly nucleated ice is calculated for large time
!           steps using fig. 8 of miller and young (jas, 1979), at 1 deg intervals
!           using their ice crystal masses calculated after 600 s of growth in water
!           saturated conditions.  the growth rates are normalized by time step
!           assuming 3d growth with time**1.5 following eq. (6.3) in young (1993).
!        -> 8/22/01: this feature has been effectively limited to 0 to -10c.  
!       (9) ice precipitation rates can increase due to increase in response to
!           cloud water riming due to (a) increased density & mass of the rimed
!           ice, and (b) increased fall speeds of rimed ice.
!        -> 8/22/01: this feature has been effectively limited to 0 to -10c.  
!###############################################################################
!###############################################################################
!
      subroutine gsmcolumn ( araing, asnowg, dtpg, i_index, j_index,    &
     & lsfc, p_col, qi_col, qr_col, qv_col, qw_col, rimef_col, t_col,   &
     & thick_col, wc_col, lm, rhc_col, xncw, flgmin, print_diag, psfc)
!
      implicit none
!
!###############################################################################
!###############################################################################
!
!-------------------------------------------------------------------------------
!----- note:  in this version of the code threading should be done outside!  
!-------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .     
! subprogram:  grid-scale microphysical processes - condensation & precipitation
!   prgrmmr: ferrier         org: w/np22     date: 08-2001
!   updated: moorthi for gfs application
!-------------------------------------------------------------------------------
! abstract:
!   * merges original gscond & precpd subroutines.   
!   * code has been substantially streamlined and restructured.
!   * exchange between water vapor & small cloud condensate is calculated using
!     the original asai (1965, j. japan) algorithm.  see also references to
!     yau and austin (1979, jas), rutledge and hobbs (1983, jas), and tao et al.
!     (1989, mwr).  this algorithm replaces the sundqvist et al. (1989, mwr)
!     parameterization.  
!-------------------------------------------------------------------------------
!     
! usage: 
!   * call gsmcolumn from subroutine gsmdrive
!   * subroutine gsmdrive called from main program ebu
!
! input argument list:
!   dtph       - physics time step (s)
!   i_index    - i index
!   j_index    - j index
!   lsfc       - eta level of level above surface, ground
!   p_col      - vertical column of model pressure (pa)
!   qi_col     - vertical column of model ice mixing ratio (kg/kg)
!   qr_col     - vertical column of model rain ratio (kg/kg)
!   qv_col     - vertical column of model water vapor specific humidity (kg/kg)
!   qw_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   rimef_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   t_col      - vertical column of model temperature (deg k)
!   thick_col  - vertical column of model mass thickness (density*height increment)
!   wc_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   
!
! output argument list: 
!   arain      - accumulated rainfall at the surface (kg)
!   asnow      - accumulated snowfall at the surface (kg)
!   qv_col     - vertical column of model water vapor specific humidity (kg/kg)
!   wc_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   qw_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   qi_col     - vertical column of model ice mixing ratio (kg/kg)
!   qr_col     - vertical column of model rain ratio (kg/kg)
!   rimef_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   t_col      - vertical column of model temperature (deg k)
!     
! output files:
!     none
!     
! subprograms & functions called:
!   * real function condense  - cloud water condensation
!   * real function deposit   - ice deposition (not sublimation)
!
! unique: none
!  
! library: none
!  
! attributes:
!   language: fortran 90
!   machine : ibm sp
!
!------------------------------------------------------------------------- 
!--------------- arrays & constants in argument list --------------------- 
!------------------------------------------------------------------------- 
!
      integer lm
      real araing, asnowg, p_col(lm), qi_col(lm), qr_col(lm), qv_col(lm)&
     &,    qw_col(lm), rimef_col(lm), t_col(lm), thick_col(lm),         &
     &     wc_col(lm), rhc_col(lm), xncw(lm), arain, asnow, dtpg, psfc
      real flgmin
!
      integer i_index, j_index, lsfc
!
!
!------------------------------------------------------------------------- 
!
!--- mean ice-particle diameters varying from 50 microns to 1000 microns
!      (1 mm), assuming an exponential size distribution.  
!
!---- meaning of the following arrays: 
!        - mdiam - mean diameter (m)
!        - venti1 - integrated quantity associated w/ ventilation effects 
!                   (capacitance only) for calculating vapor deposition onto ice
!        - venti2 - integrated quantity associated w/ ventilation effects 
!                   (with fall speed) for calculating vapor deposition onto ice
!        - accri  - integrated quantity associated w/ cloud water collection by ice
!        - massi  - integrated quantity associated w/ ice mass 
!        - vsnowi - mass-weighted fall speed of snow (large ice), used to calculate 
!                   precipitation rates
!
      real,    parameter :: dmimin=.05e-3, dmimax=1.e-3,                &
     &                      xmimin=1.e6*dmimin, xmimax=1.e6*dmimax
      integer, parameter :: mdimin=xmimin, mdimax=xmimax
!
!------------------------------------------------------------------------- 
!------- key parameters, local variables, & important comments ---------
!-----------------------------------------------------------------------
!
!--- key parameters:
!
!---- comments on 14 march 2002
!    * set epsq to the universal value of 1.e-12 throughout the code
!      condensate.  the value of epsq will need to be changed in the other 
!      subroutines in order to make it consistent throughout the eta code.  
!    * set climit=10.*epsq as the lower limit for the total mass of 
!      condensate in the current layer and the input flux of condensate
!      from above (tot_ice, tot_icenew, tot_rain, and tot_rainnew).
!
!-- nlimax - maximum number concentration of large ice crystals (20,000 /m**3, 20 per liter)
!-- nlimin - minimum number concentration of large ice crystals (100 /m**3, 0.1 per liter)
!
      real, parameter ::   rhol=1000.,  xls=hvap+hfus                   &

!    &, t_ice=-10.          !- ver1
!    &, t_ice_init=-5.      !- ver1
!!!  &, t_ice=-20.          !- ver2
!    &, t_ice=-40.          !- ver2
!    &, t_ice_init=-15.,    !- ver2
!
!    & climit=10.*epsq, eps1=rv/rd-1., rcp=1./cp,

     &,climit=10.*epsq, rcp=1./cp,                                      &
     & rcprv=rcp/rv, xls1=xls*rcp, xls2=xls*xls*rcprv,                  &
     & xls3=xls*xls/rv,                                                 &
     & c1=1./3., c2=1./6.,                                              &
     & dmr1=.1e-3, dmr2=.2e-3, dmr3=.32e-3, n0r0=8.e6, n0rmin=1.e4,     &
     & rho0=1.194, xmr1=1.e6*dmr1, xmr2=1.e6*dmr2,                      &
     & xmr3=1.e6*dmr3, xratio=.025
      integer, parameter :: mdr1=xmr1, mdr2=xmr2, mdr3=xmr3
!
!--- if blend=1:
!      precipitation (large) ice amounts are estimated at each level as a 
!      blend of ice falling from the grid point above and the precip ice
!      present at the start of the time step (see tot_ice below).
!--- if blend=0:
!      precipitation (large) ice amounts are estimated to be the precip
!      ice present at the start of the time step.
!
!--- extended to include sedimentation of rain on 2/5/01 
!
      real, parameter :: blend=1.
!
!--- this variable is for debugging purposes (if .true.)
!
      logical  print_diag
!
!--- local variables
!
      real    emairi, n0r,         nlice,       nsmice, nlimax, pfac
      logical clear,  ice_logical, dbg_logical, rain_logical
 
      integer lbef, ipass, ixrf, ixs, itdx, idr                         &
     &,       index_my, indexr, indexr1, indexs                         &
     &,       l, ntimes, item
!    &,       i, j, k, my_600, i1, i2, l, ntimes

      real flimass,  xlimass, vsnow,   qi_min, dum,    piloss           &
     &,    tot_ice,  xsimass, vel_inc, vrimef, rimef1, dum1             &
     &,    dum2,     fws,     denomi                                    &
     &,    xrf,      qw0,     dli,     xli,    fsmall                   &
     &,    prevp,    tk2,     dtph                                      &
     &,    pievp,    picnd,   piacr,   pracw                            &
     &,    praut,    pimlt,   qtice,   qlice                            &
     &,    gammar,   flarge,  wvqw,    dynvis                           &
     &,    tfactor,  gammas,  diffus, therm_cond                        &
     &,    wvnew,    delv,    tnew,    tot_icenew, rimef                &
     &,    deli,     fwr,     crevp,   ventr,      delt                 &
     &,    delw,     fir,     delr,    qsinew,     qswnew               &
     &,    budget,   wsnew,   vrain2,  tot_rainnew                      &
     &,    qtnew,    qt,      wcnew,   abw                              &
     &,    aievp,    tcc,     denomf,  abi                              &
     &,    sfactor,  pidep_max,        didep,      ventis, ventil       &
     &,    dievp,    rqr,     rfactor, dwvr,       rr,     tot_rain     &
     &,    dwv0,     qsw0,    prloss,  qtrain,     vrain1               &
     &,    qsw,      ws,      esi,     esw, wv, wc, rhgrd, rho          &
     &,    rrho,     dtrho,   wsgrd,   qsi, qswgrd, qsigrd              &
     &,    tk,       tc,      pp,      bldtrh                           &
     &,    xlv,      xlv1,    xlf,     xlf1,  xlv2, denomw, denomwi     &
     &,    qwnew,    pcond,   pidep,   qrnew, qi,   qr,     qw          &
     &,    piacw,    piacwi,  piacwr,  qv,    dwvi                      &
     &,    arainnew, thick,   asnownew                                  &
     &,    qinew,    qi_min_0c, qsw_l, qsi_l, qsw0_l, schmit_fac
    
!
!
!#######################################################################
!########################## begin execution ############################
!#######################################################################
!
      dtph   = dtpg / mic_step
      araing = 0.    ! total accumulated rainfall at surface (kg/m**2)
      asnowg = 0.    ! total accumulated snowfall at surface (kg/m**2)
!
      do ntimes =1,mic_step
!
        qi_min_0c = 10.e3*massi(mdimin)   !- ver5
        arain = 0.   ! accumulated rainfall at surface for this step (kg/m**2)
        asnow = 0.   ! accumulated snowfall at surface for this step (kg/m**2)
!
!-----------------------------------------------------------------------
!
        do l=1,lsfc      !      loop from top (l=1) to surface (l=lsfc)

!---      skip this level and go to the next lower level if no condensate 
!         and very low specific humidities
!
          if (qv_col(l) > epsq .or. wc_col(l) > epsq) then
!
!-----------------------------------------------------------------------
!------------ proceed with cloud microphysics calculations -------------
!-----------------------------------------------------------------------
!
            tk = t_col(l)         ! temperature (deg k)
            tc = tk-t0c           ! temperature (deg c)
            pp = p_col(l)         ! pressure (pa)
            qv = qv_col(l)        ! specific humidity of water vapor (kg/kg)
!           wv = qv/(1.-qv)       ! water vapor mixing ratio (kg/kg)
            wv = qv               ! water vapor specific humidity (kg/kg)
            wc = wc_col(l)        ! grid-scale mixing ratio of total condensate
                                ! (water or ice; kg/kg)
!           wc = wc/(1.-wc)
            rhgrd = rhc_col(l)

!
!   pressure dependen scaling factor for flgmin (tunable)
!
!!!         pfac = max(0.5, (min(1.0, pp*0.00002))**2)   ! commented on 02182011
!go         pfac = max(0.5, (sqrt(min(1.0, pp*0.00004))))
            pfac = 1.0
!
            clear = .true.
!    
!--- check grid-scale saturation when no condensate is present
!    
            esw = min(pp, fpvsl(tk))     ! saturation vapor pressure w/r/t water
!           qsw = eps*esw/(pp-esw)       ! saturation mixing ratio w/r/t water
            qsw = eps*esw/(pp+epsm1*esw) ! saturation specific humidity  w/r/t water
            ws  = qsw                    ! general saturation mixing ratio (water/ice)
            qsi = qsw
            if (tc < 0.) then
              esi = min(pp,fpvsi(tk))      ! saturation vapor pressure w/r/t ice
!             qsi = eps*esi/(pp-esi)       ! saturation mixing ratio w/r/t water
              qsi = eps*esi/(pp+epsm1*esi) ! saturation specific humidity w/r/t water
              ws  = qsi                    ! general saturation mixing ratio (water/ice)
              if (pp <= esi) ws = wv / rhgrd
            endif
!
            dum  = min(pp, esw0)
            qsw0 = eps*dum/(pp+epsm1*dum)  ! saturation specific humidity at 0c
!
!--- effective grid-scale saturation mixing ratios
!
            qswgrd = rhgrd*qsw
            qsigrd = rhgrd*qsi
            wsgrd  = rhgrd*ws
            qsw_l  = qswgrd
            qsi_l  = qsigrd
            qsw0_l = qsw0*rhgrd
!
!--- check if air is subsaturated and w/o condensate
!
            if (wv > wsgrd .or. wc > epsq) clear = .false.  ! cloudy case
            if (arain > climit) then ! if any rain is falling into layer from above
              clear = .false.
            else
              arain = 0.
            endif
!
!--- check if any ice is falling into layer from above
!
!--- note that "snow" in variable names is synonomous with 
!    large, precipitation ice particles
!
            if (asnow > climit) then
              clear = .false.
            else
              asnow = 0.
            endif
!
!-----------------------------------------------------------------------
!-- loop to the end if in clear, subsaturated air free of condensate ---
!-----------------------------------------------------------------------
!
            if (.not. clear) then
!
!-----------------------------------------------------------------------
!--------- initialize rho, thick & microphysical processes -------------
!-----------------------------------------------------------------------
!
!
!--- virtual temperature, tv=t*(1./eps-1)*q, q is specific humidity;
!    (see pp. 63-65 in fleagle & businger, 1963)
!
              rho    = pp/(rd*tk*(1.+eps1*qv)) ! air density (kg/m**3)
              rrho   = 1./rho                  ! reciprocal of air density
              dtrho  = dtph*rho                ! time step * air density
              bldtrh = blend*dtrho             ! blend parameter * time step * air density
              thick  = thick_col(l)   ! layer thickness = rho*dz = -dp/g
!
              arainnew = 0.           ! updated accumulated rainfall at surface
              asnownew = 0.           ! updated accumulated snowfall at surface
              qi       = qi_col(l)    ! ice mixing ratio
              qinew    = 0.           ! updated ice mixing ratio
              qr       = qr_col(l)    ! rain mixing ratio
              qrnew    = 0.           ! updated rain ratio
              qw       = qw_col(l)    ! cloud water mixing ratio
              qwnew    = 0.           ! updated cloud water ratio
!
              pcond    = 0.       ! condensation (>0) or evaporation (<0)
                                  ! of cloud water (kg/kg)
              pidep    = 0.       ! deposition (>0) or sublimation (<0)
                                  ! of ice crystals (kg/kg)
              piacw   = 0.        ! cloud water collection (riming)
                                  ! by precipitation ice (kg/kg; >0)
              piacwi  = 0.        ! growth of precip ice by riming (kg/kg; >0)
              piacwr  = 0.        ! shedding of accreted cloud water
                                  ! to form rain (kg/kg; >0)
              piacr   = 0.        ! freezing of rain onto large ice
                                  ! at supercooled temps (kg/kg; >0)
              picnd   = 0.        ! condensation (>0) onto wet, melting
                                  ! ice (kg/kg)
              pievp   = 0.        ! evaporation (<0) from wet, melting
                                  ! ice (kg/kg)
              pimlt   = 0.        ! melting ice (kg/kg; >0)
              praut   = 0.        ! cloud water autoconversion to rain (kg/kg; >0)
              pracw   = 0.        ! cloud water collection (accretion) by rain (kg/kg; >0)
              prevp   = 0.        ! rain evaporation (kg/kg; <0)
!
!---------------------------------------------------------------------------
!--- double check input hydrometeor mixing ratios
!
!             dum  = wc - (qi+qw+qr)
!             dum1 = abs(dum)
!             dum2 = toler * min(wc, qi+qw+qr)
!             if (dum1 >  dum2) then
!               write(6,"(/2(a,i4),a,i2)") '{@ i=',i_index,' j=',j_index,
!     &                                     ' l=',l
!               write(6,"(4(a12,g11.4,1x))") 
!     & '{@ tcold=',tc,'p=',.01*pp,'diff=',dum,'wcold=',wc,
!     & '{@ qiold=',qi,'qwold=',qw,'qrold=',qr
!             endif
!
!***********************************************************************
!*********** main microphysics calculations now follow! ****************
!***********************************************************************
!
!--- calculate a few variables, which are used more than once below
!
!--- latent heat of vaporization as a function of temperature from
!      bolton (1980, jas)
!
              xlv    = 3.148e6 - 2370*tk     ! latent heat of vaporization (lv)
              xlf    = xls-xlv               ! latent heat of fusion (lf)
              xlv1   = xlv*rcp               ! lv/cp
              xlf1   = xlf*rcp               ! lf/cp
              tk2    = 1./(tk*tk)            ! 1./tk**2
              xlv2   = xlv*xlv*qsw_l*tk2/rv  ! lv**2*qsw_l/(rv*tk**2)
              denomw = 1. + xlv2*rcp         ! denominator term, clausius-clapeyron correction
!
!--- basic thermodynamic quantities
!      *      dynvis     - dynamic viscosity           [ kg/(m*s) ]
!      *      therm_cond - thermal conductivity        [ j/(m*s*k) ]
!      *      diffus     - diffusivity of water vapor  [ m**2/s ]
!
!             tfactor    = tk**1.5/(tk+120.)
              tfactor    = tk*sqrt(tk)/(tk+120.)
              dynvis     = 1.496e-6*tfactor
              therm_cond = 2.116e-3*tfactor
              diffus     = 8.794e-5*tk**1.81/pp
              schmit_fac = (rho/(diffus*diffus*dynvis))**c2
!
!--- air resistance term for the fall speed of ice following the
!      basic research by heymsfield, kajikawa, others 
!
              gammas = (1.e5/pp)**c1
!
!--- air resistance for rain fall speed (beard, 1985, jaot, p.470)
!
              gammar = (rho0/rho)**0.4
!
!----------------------------------------------------------------------
!-------------  important microphysics decision tree  -----------------
!----------------------------------------------------------------------
!
!--- determine if conditions supporting ice are present
!
              if (tc < 0. .or. qi > epsq .or. asnow > climit) then
                ice_logical = .true.
              else
                ice_logical = .false.
                qlice = 0.
                qtice = 0.
              endif
!
!--- determine if rain is present
!
              rain_logical = .false.
              if (arain > climit .or. qr > epsq) rain_logical = .true.
!
              if (ice_logical) then
!
!--- important:  estimate time-averaged properties.
!
!---
!  * flarge  - ratio of number of large ice to total (large & small) ice
!  * fsmall  - ratio of number of small ice crystals to large ice particles
!  ->  small ice particles are assumed to have a mean diameter of 50 microns.
!  * xsimass - used for calculating small ice mixing ratio
!---
!  * tot_ice - total mass (small & large) ice before microphysics,
!              which is the sum of the total mass of large ice in the 
!              current layer and the input flux of ice from above
!  * piloss  - greatest loss (<0) of total (small & large) ice by
!              sublimation, removing all of the ice falling from above
!              and the ice within the layer
!  * rimef1  - rime factor, which is the mass ratio of total (unrimed & rimed) 
!              ice mass to the unrimed ice mass (>=1)
!  * vrimef  - the velocity increase due to rime factor or melting (ratio, >=1)
!  * vsnow   - fall speed of rimed snow w/ air resistance correction
!  * emairi  - equivalent mass of air associated layer and with fall of snow into layer
!  * xlimass - used for calculating large ice mixing ratio
!  * flimass - mass fraction of large ice
!  * qtice   - time-averaged mixing ratio of total ice
!  * qlice   - time-averaged mixing ratio of large ice
!  * nlice   - time-averaged number concentration of large ice
!  * nsmice  - number concentration of small ice crystals at current level
!---
!--- assumed number fraction of large ice particles to total (large & small) 
!    ice particles, which is based on a general impression of the literature.
!
                wvqw = wv + qw                ! water vapor + cloud water
!
!--- 6/19/03 - deleted some code here ....
!
!  *********************************************************

!               if (tc >= 0. .or. wvqw < qsigrd) then
!  !
!  !--- eliminate small ice particle contributions for melting & sublimation
!  !
!                 flarge = flarge1
!               else
!  !
!  !--- enhanced number of small ice particles during depositional growth
!  !    (effective only when 0c > t >= t_ice [-10c] )
!  !
!                 flarge = flarge2
!  !
!  !--- larger number of small ice particles due to rime splintering
!  !
!                 if (tc >= -8. .and. tc <= -3.) flarge=.5*flarge
!
!               endif            ! end if (tc >= 0. .or. wvqw < qsigrd)
!               fsmall=(1.-flarge)/flarge
!               xsimass=rrho*massi(mdimin)*fsmall
!  *********************************************************
!
                if (qi <= epsq .and. asnow <= climit) then
                  indexs  = mdimin
                  flarge  = 0.                   !--- begin 6/19/03 changes
                  fsmall  = 1.
                  xsimass = rrho*massi(mdimin)   !--- end 6/19/03 changes
                  tot_ice = 0.
                  piloss  = 0.
                  rimef1  = 1.
                  vrimef  = 1.
                  vel_inc = gammas
                  vsnow   = 0.
                  emairi  = thick
                  xlimass = rrho*rimef1*massi(indexs)
                  flimass = xlimass/(xlimass+xsimass)
                  qlice   = 0.
                  qtice   = 0.
                  nlice   = 0.
                  nsmice  = 0.
                else
   !
   !--- for t<0c mean particle size follows houze et al. (jas, 1979, p. 160), 
   !    converted from fig. 5 plot of lamdas.  similar set of relationships 
   !    also shown in fig. 8 of ryan (bams, 1996, p. 66).
   !
   !--- begin 6/19/03 changes => allow nlimax to increase & flarge to 
   !    decrease at colder temperatures; set flarge to zero (i.e., only small
   !    ice) if the ice mixing ratio is below qi_min

!                 dum    = max(0.05, min(1., exp(.0536*tc)) )
                  dum    = max(0.05, min(1., exp(.0564*tc)) )
                  indexs = min(mdimax, max(mdimin, int(xmimax*dum) ) )
!
                  dum    = max(flgmin*pfac, dum)

                  qi_min = qi_min_0c * dum  !- ver5    ----not used ----
!!                qi_min = qi_min_0c        !- ver5
!!!               qi_min = qi_min_0c/dum    !- ver5

                  nlimax = 10.e3/sqrt(dum)  !- ver3
                  if (tc < 0.) then
                    flarge = dum            !- ver4
                  else
                    flarge = 1.
                  endif
                  fsmall  = (1.-flarge)/flarge
                  xsimass = rrho*massi(mdimin)*fsmall
                  tot_ice = thick*qi + blend*asnow
                  piloss  = -tot_ice/thick
                  lbef    = max(1,l-1)
                  rimef1  = (rimef_col(l)*thick*qi                      &
     &                    + rimef_col(lbef)*blend*asnow)/tot_ice
                  rimef1  = min(rimef1, rfmax)
                  vsnow   = 0.0
                  do ipass=0,1
                    if (rimef1 .le. 1.) then
                      rimef1 = 1.
                      vrimef = 1.
                    else
                      ixs  = max(2, min(indexs/100, 9))
                      xrf  = 10.492*log(rimef1)
                      ixrf = max(0, min(int(xrf), nrime))
                      if (ixrf .ge. nrime) then
                        vrimef = vel_rf(ixs,nrime)
                      else
                        vrimef = vel_rf(ixs,ixrf)+(xrf-float(ixrf))*    &
     &                          (vel_rf(ixs,ixrf+1)-vel_rf(ixs,ixrf))
                      endif
                    endif            ! end if (rimef1 <= 1.)
                    vel_inc = gammas*vrimef
                    vsnow   = vel_inc*vsnowi(indexs)
                    emairi  = thick + bldtrh*vsnow
                    xlimass = rrho*rimef1*massi(indexs)
                    flimass = xlimass/(xlimass+xsimass)
                    qtice   = tot_ice/emairi
                    qlice   = flimass*qtice
                    nlice   = qlice/xlimass
                    nsmice  = fsmall*nlice
   !
                    if ( (nlice >= nlimin .and. nlice <= nlimax)        & 
     &                    .or. ipass == 1) then
                      exit
                    else
                      if(tc < 0) then
                        xli = rho*(qtice/dum-xsimass)/rimef1
                        if (xli <= massi(mdimin) ) then
                          indexs = mdimin
                        else if (xli <= massi(450) ) then
                          dli    = 9.5885e5*xli**.42066       ! dli in microns
                          indexs = min(mdimax, max(mdimin, int(dli) ) )
                        else if (xli <= massi(mdimax) ) then
                          dli    = 3.9751e6*xli**.49870       ! dli in microns
                          indexs = min(mdimax, max(mdimin, int(dli) ) )
                        else 
                          indexs = mdimax
                        endif             ! end if (xli <= massi(mdimin) ) 
                      endif               ! end if (tc < 0)
!
!--- reduce excessive accumulation of ice at upper levels
!    associated with strong grid-resolved ascent
!
!--- force nlice to be between nlimin and nlimax
!
!--- 8/22/01: increase density of large ice if maximum limits
!    are reached for number concentration (nlimax) and mean size
!    (mdimax).  done to increase fall out of ice.
!
!

                      dum = max(nlimin, min(nlimax, nlice) )
                      if (dum >= nlimax .and. indexs >= mdimax)         &
     &                 rimef1 = rho*(qtice/nlimax-xsimass)/massi(indexs)
!
!                write(6,"(4(a12,g11.4,1x))") 
!     & '{$ tc=',tc,'p=',.01*pp,'nlice=',nlice,'dum=',dum,
!     & '{$ xli=',xli,'indexs=',float(indexs),'rho=',rho,'qtice=',qtice,
!     & '{$ xsimass=',xsimass,'rimef1=',rimef1

                    endif    ! end if ( (nlice >=nlimin .and. nlice >= nlimax)
                  enddo      ! end do ipass=0,1
                endif        ! end if (qi <= epsq .and. asnow <= climit)
              endif          ! end if (ice_logical)
!
!----------------------------------------------------------------------
!--------------- calculate individual processes -----------------------
!----------------------------------------------------------------------
!
!--- cloud water autoconversion to rain and collection by rain
!
              if (qw > epsq .and. tc >= t_ice) then
   !
   !--- qw0 could be modified based on land/sea properties, 
   !      presence of convection, etc.  this is why qaut0 and craut
   !      are passed into the subroutine as externally determined
   !      parameters.  can be changed in the future if desired.
   !
!               qw0   = qaut0*rrho
                qw0   = qautx*rrho*xncw(l)
                praut = max(0., qw-qw0)*craut
                if (qlice  >  epsq) then
      !
      !--- collection of cloud water by large ice particles ("snow")
      !    piacwi=piacw for riming, piacwi=0 for shedding
      !
!moor              fws   = min(1., ciacw*vel_inc*nlice*accri(indexs)/pp**c1) ! 20050422
                   fws   = min(0.1, ciacw*vel_inc*nlice*accri(indexs)   &
     &                                                  /pp**c1)
                   piacw = fws*qw
                   if (tc  < 0.) piacwi = piacw    ! large ice riming

                endif             ! end if (qlice > epsq)
              endif               ! end if (qw > epsq .and. tc >= t_ice)
!
!----------------------------------------------------------------------
!--- loop around some of the ice-phase processes if no ice should be present
!----------------------------------------------------------------------
!
              if (ice_logical) then
!
!--- now the pretzel logic of calculating ice deposition
!
                if (tc < t_ice .and. (wv > qsigrd .or. qw > epsq)) then
!
!--- adjust to ice saturation at t<t_ice (-10c) if supersaturated.
!    sources of ice due to nucleation and convective detrainment are
!    either poorly understood, poorly resolved at typical nwp 
!    resolutions, or are not represented (e.g., no detrained 
!    condensate in bmj cu scheme).
!    
                  pcond = -qw
                  dum1  = tk + xlv1*pcond              ! updated (dummy) temperature (deg k)
                  dum2  = wv+qw                        ! updated (dummy) water vapor mixing ratio
                  dum   = min(pp,fpvsi(dum1))          ! updated (dummy) saturation vapor pressure w/r/t ice
                  dum   = rhgrd*eps*dum/(pp+epsm1*dum) ! updated (dummy) saturation specific humidity w/r/t ice
!                 dum   = rhgrd*eps*dum/(pp-dum)       ! updated (dummy) saturation mixing ratio w/r/t ice

                  if (dum2 > dum) pidep = deposit(pp, rhgrd, dum1, dum2)

                  dwvi = 0.                            ! used only for debugging
!
                else if (tc < 0.) then
!
!--- these quantities are handy for ice deposition/sublimation
!    pidep_max - max deposition or minimum sublimation to ice saturation
!
                  denomi    = 1. + xls2*qsi_l*tk2
                  dwvi      = min(wvqw,qsw_l)-qsi_l
                  pidep_max = max(piloss, dwvi/denomi)
                  if (qtice > 0.) then
!
!--- calculate ice deposition/sublimation
!      * sfactor - [vel_inc**.5]*[schmidt**(1./3.)]*[(rho/dynvis)**.5],
!        where schmidt (schmidt number) =dynvis/(rho*diffus)
!      * units: sfactor - s**.5/m ;  abi - m**2/s ;  nlice - m**-3 ;
!               ventil, ventis - m**-2 ;  venti1 - m ;  
!               venti2 - m**2/s**.5 ; didep - unitless
!
!                   sfactor = vel_inc**.5*(rho/(diffus*diffus*dynvis))**c2
                    sfactor = sqrt(vel_inc)*schmit_fac
                    abi     = 1./(rho*xls3*qsi*tk2/therm_cond+1./diffus)
!
!--- ventil - number concentration * ventilation factors for large ice
!--- ventis - number concentration * ventilation factors for small ice
!
!--- variation in the number concentration of ice with time is not
!      accounted for in these calculations (could be in the future).
!
                    ventil = (venti1(indexs) + sfactor*venti2(indexs))  &
     &                                       * nlice
                    ventis = (venti1(mdimin) + sfactor*venti2(mdimin))  &
     &                                       * nsmice
                    didep  = abi*(ventil+ventis)*dtph
!
!--- account for change in water vapor supply w/ time
!
                    if (didep >= xratio)                                & 
     &                didep = (1.-exp(-didep*denomi))/denomi
                    if (dwvi > 0.) then
                      pidep = min(dwvi*didep, pidep_max)
                    else if (dwvi < 0.) then
                      pidep = max(dwvi*didep, pidep_max)
                    endif
!
                  else if (wvqw > qsi_l .and. tc <= t_ice_init) then
!
!--- ice nucleation in near water-saturated conditions.  ice crystal
!    growth during time step calculated using miller & young (1979, jas).
!--- these deposition rates could drive conditions below water saturation,
!    which is the basis of these calculations.  intended to approximate
!    more complex & computationally intensive calculations.
!
                    index_my = max(my_t1, min( int(.5-tc), my_t2 ) )
!
!--- dum1 is the supersaturation w/r/t ice at water-saturated conditions
!
!--- dum2 is the number of ice crystals nucleated at water-saturated 
!    conditions based on meyers et al. (jam, 1992).
!
!--- prevent unrealistically large ice initiation (limited by pidep_max)
!      if dum2 values are increased in future experiments
!
                    dum1  = qsw/qsi - 1.      
                    dum2  = 1.e3*exp(12.96*dum1-0.639)
                    pidep = min(pidep_max,dum2*my_growth(index_my)*rrho)
!
                  endif ! end if (qtice > 0.)
!
                endif   ! end if (tc < t_ice .and. (wv > qsigrd .or. qw > epsq))
!
!------------------------------------------------------------------------
!
              endif     ! end of if (ice_logical)then loop
!
!------------------------------------------------------------------------
!
!--- cloud water condensation
!
              if (tc >= t_ice .and. (qw > epsq .or. wv > qswgrd)) then
                if (piacwi == 0. .and. pidep == 0.) then
                  pcond = condense (pp, qw, rhgrd, tk, wv)
                else  !-- modify cloud condensation in response to ice processes
                  dum     = xlv*qswgrd*rcprv*tk2
                  denomwi = 1. + xls*dum
                  denomf  = xlf*dum
                  dum     = max(0., pidep)
                  pcond   = (wv-qswgrd-denomwi*dum-denomf*piacwi)/denomw
                  dum1    = -qw
                  dum2    = pcond - piacw
                  if (dum2  <  dum1) then    !--- limit cloud water sinks
                    dum    = dum1/dum2
                    pcond  = dum*pcond
                    piacw  = dum*piacw
                    piacwi = dum*piacwi
                  endif ! endif (dum2 <  dum1)
                endif   ! endif (piacwi == 0. .and. pidep == 0.)
              endif     ! endif (tc >= t_ice .and. (qw > epsq .or. wv > qswgrd))
!
!--- limit freezing of accreted rime to prevent temperature oscillations,
!    a crude schumann-ludlam limit (p. 209 of young, 1993). 
!
              tcc = tc + xlv1*pcond + xls1*pidep + xlf1*piacwi
              if (tcc  >  0.) then
                piacwi = 0.
                tcc    = tc + xlv1*pcond + xls1*pidep
              endif
!
              if (tc > 0. .and. tcc > 0. .and. ice_logical) then
!
!--- calculate melting and evaporation/condensation
!      * units: sfactor - s**.5/m ;  abi - m**2/s ;  nlice - m**-3 ;
!               ventil - m**-2 ;  venti1 - m ;  
!               venti2 - m**2/s**.5 ; cievp - /s
!
!               sfactor = vel_inc**.5*(rho/(diffus*diffus*dynvis))**c2
                sfactor = sqrt(vel_inc)*schmit_fac
                ventil  = nlice*(venti1(indexs)+sfactor*venti2(indexs))
                aievp   = ventil*diffus*dtph
                if (aievp  <  xratio) then
                  dievp = aievp
                else
                  dievp = 1. - exp(-aievp)
                endif
!               qsw0 = eps*esw0/(pp-esw0)
!               qsw0 = eps*esw0/(pp+epsm1*esw0)
!!              dum  = min(pp, esw0)
!!              qsw0 = eps*dum/(pp+epsm1*dum)
!               dwv0 = min(wv,qsw)-qsw0
                dwv0 = min(wv,qsw_l)-qsw0_l
                dum  = qw + pcond
                if (wv < qsw_l .and. dum <= epsq) then
   !
   !--- evaporation from melting snow (sink of snow) or shedding
   !    of water condensed onto melting snow (source of rain)
   !
                  dum   = dwv0*dievp
                  pievp = max( min(0., dum), piloss)
                  picnd = max(0., dum)
                endif            ! end if (wv < qsw_l .and. dum <= epsq)
                pimlt = therm_cond*tcc*ventil*rrho*dtph/xlf
   !
   !--- limit melting to prevent temperature oscillations across 0c
   !
                dum1  = max( 0., (tcc+xlv1*pievp)/xlf1 )
                pimlt = min(pimlt, dum1)
   !
   !--- limit loss of snow by melting (>0) and evaporation
   !
                dum = pievp - pimlt
                if (dum < piloss) then
                  dum1  = piloss/dum
                  pimlt = pimlt*dum1
                  pievp = pievp*dum1
                endif       ! end if (dum  > qtice)
              endif         ! end if (tc > 0. .and. tcc > 0. .and. ice_logical)
!
!--- important:  estimate time-averaged properties.
!
!  * tot_rain - total mass of rain before microphysics, which is the sum of
!               the total mass of rain in the current layer and the input 
!               flux of rain from above
!  * vrain1   - fall speed of rain into grid from above (with air resistance correction)
!  * qtrain   - time-averaged mixing ratio of rain (kg/kg)
!  * prloss   - greatest loss (<0) of rain, removing all rain falling from
!               above and the rain within the layer
!  * rqr      - rain content (kg/m**3)
!  * indexr   - mean size of rain drops to the nearest 1 micron in size
!  * n0r      - intercept of rain size distribution (typically 10**6 m**-4)
!
              tot_rain = 0.
              vrain1   = 0.
              qtrain   = 0.
              prloss   = 0.
              rqr      = 0.
              n0r      = 0.
              indexr1  = indexr    ! for debugging only
              indexr   = mdrmin
              if (rain_logical) then
                if (arain <= 0.) then
                  indexr = mdrmin
                  vrain1 = 0.
                else
   !
   !--- indexr (related to mean diameter) & n0r could be modified 
   !      by land/sea properties, presence of convection, etc.
   !
   !--- rain rate normalized to a density of 1.194 kg/m**3
   !
                  rr = arain / (dtph*gammar)
   !
                  if (rr <= rr_drmin) then
        !
        !--- assume fixed mean diameter of rain (0.2 mm) for low rain rates, 
        !      instead vary n0r with rain rate
        !
                    indexr = mdrmin
                  else if (rr <= rr_dr1) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.05 and 0.10 mm:
        !      v(dr)=5.6023e4*dr**1.136, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*5.6023e4*dr**(4+1.136) = 1.408e15*dr**5.136,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.123e-3*rr**.1947 -> dr (microns) = 1.123e3*rr**.1947
        !
                    indexr = int( 1.123e3*rr**.1947 + .5 )
                    indexr = max( mdrmin, min(indexr, mdr1) )

                  else if (rr <= rr_dr2) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.10 and 0.20 mm:
        !      v(dr)=1.0867e4*dr**.958, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*1.0867e4*dr**(4+.958) = 2.731e14*dr**4.958,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.225e-3*rr**.2017 -> dr (microns) = 1.225e3*rr**.2017
        !
                    indexr = int( 1.225e3*rr**.2017 + .5 )
                    indexr = max( mdr1, min(indexr, mdr2) )

                  else if (rr <= rr_dr3) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.20 and 0.32 mm:
        !      v(dr)=2831.*dr**.80, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*2831.*dr**(4+.80) = 7.115e13*dr**4.80, 
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.3006e-3*rr**.2083 -> dr (microns) = 1.3006e3*rr**.2083
        !
                    indexr = int( 1.3006e3*rr**.2083 + .5 )
                    indexr = max( mdr2, min(indexr, mdr3) )

                  else if (rr <= rr_drmax) then
        !
        !--- best fit to mass-weighted fall speeds (v) from rain lookup tables 
        !      for mean diameters (dr) between 0.32 and 0.45 mm:
        !      v(dr)=944.8*dr**.6636, v in m/s and dr in m
        !      rr = pi*1000.*n0r0*944.8*dr**(4+.6636) = 2.3745e13*dr**4.6636,
        !        rr in kg/(m**2*s)
        !      dr (m) = 1.355e-3*rr**.2144 -> dr (microns) = 1.355e3*rr**.2144
        !
                    indexr = int( 1.355e3*rr**.2144 + .5 )
                    indexr = max( mdr3, min(indexr, mdrmax) )
                  else 
        !
        !--- assume fixed mean diameter of rain (0.45 mm) for high rain rates, 
        !      instead vary n0r with rain rate
        !
                    indexr = mdrmax
                  endif               ! end if (rr <= rr_drmin) etc. 
!
                  vrain1 = gammar*vrain(indexr)
                endif                 ! end if (arain <= 0.)
!
                indexr1  = indexr     ! for debugging only
                tot_rain = thick*qr+blend*arain
                qtrain   = tot_rain/(thick+bldtrh*vrain1)
                prloss   = -tot_rain/thick
                rqr      = rho*qtrain
   !
   !--- rqr - time-averaged rain content (kg/m**3)
   !
                if (rqr <= rqr_drmin) then
                  n0r    = max(n0rmin, cn0r_dmrmin*rqr)
                  indexr = mdrmin
                else if (rqr >= rqr_drmax) then
                  n0r    = cn0r_dmrmax*rqr
                  indexr = mdrmax
                else
                  n0r    = n0r0
!                 indexr = max( xmrmin, min(cn0r0*rqr**.25, xmrmax) )
                  item   = cn0r0*sqrt(sqrt(rqr))               ! moorthi 07/31/08
                  indexr = max( mdrmin, min(item, mdrmax) )    ! moorthi 07/31/08
                endif
   !
                if (tc < t_ice) then
                  piacr = -prloss
                else
                  dwvr = wv - pcond - qsw_l
                  dum  = qw + pcond
                  if (dwvr < 0. .and. dum <= epsq) then
!
!--- rain evaporation
!
!    * rfactor - [gammar**.5]*[schmidt**(1./3.)]*[(rho/dynvis)**.5],
!        where schmidt (schmidt number) =dynvis/(rho*diffus)
!
!    * units: rfactor - s**.5/m ;  abw - m**2/s ;  ventr - m**-2 ;  
!             n0r - m**-4 ;  ventr1 - m**2 ;  ventr2 - m**3/s**.5 ;
!             crevp - unitless
!
!                   rfactor = gammar**.5*(rho/(diffus*diffus*dynvis))**c2
                    rfactor = sqrt(gammar)*schmit_fac
                    abw     = 1./(rho*xlv2/therm_cond+1./diffus)
!
!--- note that ventr1, ventr2 lookup tables do not include the 
!      1/davg multiplier as in the ice tables
!
                    ventr = n0r*(ventr1(indexr)+rfactor*ventr2(indexr))
                    crevp = abw*ventr*dtph
                    if (crevp < xratio) then
                      dum = dwvr*crevp
                    else
                      dum = dwvr*(1.-exp(-crevp*denomw))/denomw
                    endif
                    prevp = max(dum, prloss)
                  else if (qw > epsq) then
                    fwr   = cracw*gammar*n0r*accrr(indexr)
!moor               pracw = min(1.,fwr)*qw               ! 20050422
                    pracw = min(0.1,fwr)*qw
                  endif           ! end if (dwvr < 0. .and. dum <= epsq)
!
                  if (tc < 0. .and. tcc < 0.) then
!
!--- biggs (1953) heteorogeneous freezing (e.g., lin et al., 1983)
!   - rescaled mean drop diameter from microns (indexr) to mm (dum) to prevent underflow
!
                    dum   = .001*float(indexr)
                    dum1  = dum * dum
                    dum   = (exp(abfr*tc)-1.)*dum1*dum1*dum1*dum
                    piacr = min(cbfr*n0r*rrho*dum, qtrain)
                    if (qlice > epsq) then
            !
            !--- freezing of rain by collisions w/ large ice
            !
                      dum  = gammar*vrain(indexr)
                      dum1 = dum-vsnow
            !
            !--- dum2 - difference in spectral fall speeds of rain and
            !      large ice, parameterized following eq. (48) on p. 112 of 
            !      murakami (j. meteor. soc. japan, 1990)
            !
                      dum2 = (dum1*dum1+.04*dum*vsnow)**.5
                      dum1 = 5.e-12*indexr*indexr+2.e-12*indexr*indexs      &
     &                     +.5e-12*indexs*indexs
                      fir = min(1., ciacr*nlice*dum1*dum2)
            !
            !--- future?  should collection by small ice should be included???
            !
                      piacr = min(piacr+fir*qtrain, qtrain)
                    endif        ! end if (qlice >  epsq)
                    dum = prevp - piacr
                    if (dum < prloss) then
                      dum1  = prloss/dum
                      prevp = dum1*prevp
                      piacr = dum1*piacr
                    endif        ! end if (dum < prloss)
                  endif          ! end if (tc < 0. .and. tcc < 0.)
                endif            ! end if (tc < t_ice)
              endif              ! end if (rain_logical) 
!
!----------------------------------------------------------------------
!---------------------- main budget equations -------------------------
!----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!--- update fields, determine characteristics for next lower layer ----
!-----------------------------------------------------------------------
!
!--- carefully limit sinks of cloud water
!
              dum1 = piacw + praut + pracw - min(0.,pcond)
              if (dum1 > qw) then
                dum    = qw/dum1
                piacw  = dum*piacw
                piacwi = dum*piacwi
                praut  = dum*praut
                pracw  = dum*pracw
                if (pcond < 0.) pcond=dum*pcond
              endif
              piacwr = piacw - piacwi          ! tc >= 0c
!
!--- qwnew - updated cloud water mixing ratio
!
              delw  = pcond - piacw - praut - pracw
              qwnew = qw+delw
              if (qwnew <=  epsq) qwnew = 0.
              if (qw > 0. .and. qwnew /= 0.) then
                dum = qwnew/qw
                if (dum  < toler) qwnew = 0.
              endif
!
!--- update temperature and water vapor mixing ratios
!
              delt = xlv1 * (pcond+pievp+picnd+prevp)                   &
     &             + xls1 * pidep + xlf1*(piacwi+piacr-pimlt)
              tnew = tk + delt
!
              delv  = -pcond - pidep - pievp - picnd - prevp
              wvnew = wv + delv
!
!--- update ice mixing ratios
!
!---
!  * tot_icenew - total mass (small & large) ice after microphysics,
!                 which is the sum of the total mass of large ice in the 
!                 current layer and the flux of ice out of the grid box below
!  * rimef      - rime factor, which is the mass ratio of total (unrimed & 
!                 rimed) ice mass to the unrimed ice mass (>=1)
!  * qinew      - updated mixing ratio of total (large & small) ice in layer
!      -> tot_icenew=qinew*thick+bldtrh*qlicenew*vsnow
!        -> but qlicenew=qinew*flimass, so
!      -> tot_icenew=qinew*(thick+bldtrh*flimass*vsnow)
!  * asnownew   - updated accumulation of snow at bottom of grid cell
!---
!
              deli  = 0.
              rimef = 1.
              if (ice_logical) then
                deli       = pidep + pievp + piacwi + piacr - pimlt
                tot_icenew = tot_ice + thick*deli
                if (tot_ice > 0. .and. tot_icenew /= 0.) then
                  dum = tot_icenew/tot_ice
                  if (dum  < toler) tot_icenew = 0.
                endif
                if (tot_icenew <= climit) then
                  tot_icenew = 0.
                  rimef      = 1.
                  qinew      = 0.
                  asnownew   = 0.
                else
      !
      !--- update rime factor if appropriate
      !
                  dum = piacwi + piacr
                  if (dum <= epsq .and. pidep <= epsq) then
                    rimef = rimef1
                  else
         !
         !--- rime factor, rimef = (total ice mass)/(total unrimed ice mass)
         !      dum1 - total ice mass, rimed & unrimed
         !      dum2 - estimated mass of *unrimed* ice
         !
                    dum1 = tot_ice + thick*(pidep+dum)
                    dum2 = tot_ice/rimef1 + thick*pidep
                    if (dum2 <= 0.) then
                      rimef = rfmax
                    else
                      rimef = min(rfmax, max(1., dum1/dum2) )
                    endif
                  endif       ! end if (dum <= epsq .and. pidep <= epsq)
                  qinew = tot_icenew/(thick+bldtrh*flimass*vsnow)
                  if (qinew  <=  epsq) qinew = 0.
                  if (qi > 0. .and. qinew /= 0.) then
                    dum = qinew/qi
                    if (dum < toler) qinew = 0.
                  endif
                  asnownew = bldtrh*flimass*vsnow*qinew
                  if (asnow > 0. .and. asnownew /= 0.) then
                    dum = asnownew/asnow
                    if (dum < toler) asnownew = 0.
                  endif
                endif         ! end if (tot_icenew <= climit)
              endif           ! end if (ice_logical)
!
!--- update rain mixing ratios
!
!---
! * tot_rainnew - total mass of rain after microphysics
!                 current layer and the input flux of ice from above
! * vrain2      - time-averaged fall speed of rain in grid and below 
!                 (with air resistance correction)
! * qrnew       - updated rain mixing ratio in layer
!      -> tot_rainnew=qrnew*(thick+bldtrh*vrain2)
!  * arainnew  - updated accumulation of rain at bottom of grid cell
!---
!
              delr        = praut + pracw + piacwr - piacr + pimlt      &
     &                    + prevp + picnd
              tot_rainnew = tot_rain+thick*delr
              if (tot_rain > 0. .and. tot_rainnew /= 0.) then
                dum = tot_rainnew/tot_rain
                if (dum < toler) tot_rainnew = 0.
              endif
              if (tot_rainnew <= climit) then
                tot_rainnew = 0.
                vrain2      = 0.
                qrnew       = 0.
                arainnew    = 0.
              else
   !
   !--- 1st guess time-averaged rain rate at bottom of grid box
   !
                rr = tot_rainnew/(dtph*gammar)
   !
   !--- use same algorithm as above for calculating mean drop diameter
   !      (idr, in microns), which is used to estimate the time-averaged
   !      fall speed of rain drops at the bottom of the grid layer.  this
   !      isn't perfect, but the alternative is solving a transcendental 
   !      equation that is numerically inefficient and nasty to program
   !      (coded in earlier versions of gsmcolumn prior to 8-22-01).
   !
                if (rr <= rr_drmin) then
                  idr = mdrmin
                else if (rr <= rr_dr1) then
                  idr = int( 1.123e3*rr**.1947 + .5 )
                  idr = max( mdrmin, min(idr, mdr1) )
                else if (rr <= rr_dr2) then
                  idr = int( 1.225e3*rr**.2017 + .5 )
                  idr = max( mdr1, min(idr, mdr2) )
                else if (rr <= rr_dr3) then
                  idr = int( 1.3006e3*rr**.2083 + .5 )
                  idr = max( mdr2, min(idr, mdr3) )
                else if (rr <= rr_drmax) then
                  idr = int( 1.355e3*rr**.2144 + .5 )
                  idr = max( mdr3, min(idr, mdrmax) )
                else 
                  idr = mdrmax
                endif              ! end if (rr <= rr_drmin)
                vrain2 = gammar*vrain(idr)
                qrnew  = tot_rainnew / (thick+bldtrh*vrain2)
                if (qrnew <= epsq) qrnew = 0.
                if (qr > 0. .and. qrnew /= 0.) then
                  dum = qrnew / qr
                  if (dum < toler) qrnew = 0.
                endif
                arainnew = bldtrh*vrain2*qrnew
                if (arain > 0. .and. arainnew /= 0.) then
                  dum = arainnew/arain
                  if (dum < toler) arainnew = 0.
                endif
              endif                ! end if (tot_rainnew < climit)
!
              wcnew = qwnew + qrnew + qinew
!
!----------------------------------------------------------------------
!-------------- begin debugging & verification ------------------------
!----------------------------------------------------------------------
!
!--- qt, qtnew - total water (vapor & condensate) before & after microphysics, resp.
!
!             qt=thick*(qv+wc_col(l))+arain+asnow
!             qtnew  = thick*(wvnew/(1.+wvnew)+wcnew/(1.+wcnew))
!    &               + arainnew + asnownew

              qt     = thick*(wv+wc)       + arain    + asnow
              qtnew  = thick*(wvnew+wcnew) + arainnew + asnownew
              budget = qt-qtnew
!
!--- additional check on budget preservation, accounting for truncation effects
!
              dbg_logical=.false.
!             dum=abs(budget)
!             if (dum > toler) then
!               dum=dum/min(qt, qtnew)
!               if (dum > toler) dbg_logical=.true.
!             endif
!
!             dum=(rhgrd+.001)*qsinew
!             if ( (qwnew > epsq .or. qrnew > epsq .or. wvnew > dum)
!     &           .and. tc < t_ice )  dbg_logical=.true.
!
!             if (tc > 5. .and. qinewr > epsq) dbg_logical=.true.
!
              if ((wvnew < epsq .or. dbg_logical) .and. print_diag) then
!
                write(6,"(/2(a,i4),2(a,i2))") '{} i=',i_index,' j=',    &
     &                                j_index, ' l=',l,' lsfc=',lsfc
!
                esw    = min(pp, fpvsl(tnew))
!               qswnew = eps*esw/(pp-esw)
                qswnew = eps*esw/(pp+epsm1*esw)
                if (tc < 0. .or. tnew  <  0.) then
                  esi    = min(pp, fpvsi(tnew))
!                 qsinew = eps*esi/(pp-esi)
                  qsinew = eps*esi/(pp+epsm1*esi)
                else
                  qsi    = qsw
                  qsinew = qswnew
                endif
                wsnew = qsinew
                write(6,"(4(a12,g11.4,1x))")                            &
     & '{} tcold=',tc,'tcnew=',tnew-t0c,'p=',.01*pp,'rho=',rho,         &
     & '{} thick=',thick,'rhold=',wv/ws,'rhnew=',wvnew/wsnew,           &
     &   'rhgrd=',rhgrd,                                                &
     & '{} rhwold=',wv/qsw,'rhwnew=',wvnew/qswnew,'rhiold=',wv/qsi,     &
     &   'rhinew=',wvnew/qsinew,                                        &
     & '{} qswold=',qsw,'qswnew=',qswnew,'qsiold=',qsi,'qsinew=',qsinew,&
     & '{} wsold=',ws,'wsnew=',wsnew,'wvold=',wv,'wvnew=',wvnew,        &
     & '{} wcold=',wc,'wcnew=',wcnew,'qwold=',qw,'qwnew=',qwnew,        &
     & '{} qiold=',qi,'qinew=',qinew,'qrold=',qr,'qrnew=',qrnew,        &
     & '{} arainold=',arain,'arainnew=',arainnew,'asnowold=',asnow,     &
     &   'asnownew=',asnownew,                                          &
     & '{} tot_rain=',tot_rain,'tot_rainnew=',tot_rainnew,              &
     &   'tot_ice=',tot_ice,'tot_icenew=',tot_icenew,                   &
     & '{} budget=',budget,'qtold=',qt,'qtnew=',qtnew
!
                write(6,"(4(a12,g11.4,1x))")                            &
     & '{} delt=',delt,'delv=',delv,'delw=',delw,'deli=',deli,          &
     & '{} delr=',delr,'pcond=',pcond,'pidep=',pidep,'pievp=',pievp,    &
     & '{} picnd=',picnd,'prevp=',prevp,'praut=',praut,'pracw=',pracw,  &
     & '{} piacw=',piacw,'piacwi=',piacwi,'piacwr=',piacwr,'pimlt=',    &
     &    pimlt,                                                        &
     & '{} piacr=',piacr
!
                if (ice_logical) write(6,"(4(a12,g11.4,1x))")           &
     & '{} rimef1=',rimef1,'gammas=',gammas,'vrimef=',vrimef,           &
     &   'vsnow=',vsnow,                                                &
     & '{} indexs=',float(indexs),'flarge=',flarge,'fsmall=',fsmall,    &
     &   'flimass=',flimass,                                            &
     & '{} xsimass=',xsimass,'xlimass=',xlimass,'qlice=',qlice,         &
     &   'qtice=',qtice,                                                &
     & '{} nlice=',nlice,'nsmice=',nsmice,'piloss=',piloss,             &
     &   'emairi=',emairi,                                              &
     & '{} rimef=',rimef
!
                if (tot_rain > 0. .or. tot_rainnew > 0.)                &
     &            write(6,"(4(a12,g11.4,1x))")                          &
     & '{} indexr1=',float(indexr1),'indexr=',float(indexr),            &
     &   'gammar=',gammar,'n0r=',n0r,                                   &
     & '{} vrain1=',vrain1,'vrain2=',vrain2,'qtrain=',qtrain,'rqr=',rqr,&
     & '{} prloss=',prloss,'volr1=',thick+bldtrh*vrain1,                &
     &   'volr2=',thick+bldtrh*vrain2
!
                if (praut > 0.) write(6,"(a12,g11.4,1x)") '{} qw0=',qw0
!
                if (pracw > 0.) write(6,"(a12,g11.4,1x)") '{} fwr=',fwr
!
                if (piacr > 0.) write(6,"(a12,g11.4,1x)") '{} fir=',fir
!
                dum = pimlt + picnd - prevp - pievp
                if (dum > 0. .or. dwvi /= 0.)                           &
     &            write(6,"(4(a12,g11.4,1x))")                          &
     & '{} tfactor=',tfactor,'dynvis=',dynvis,                          &
     &   'therm_con=',therm_cond,'diffus=',diffus
!
                if (prevp < 0.) write(6,"(4(a12,g11.4,1x))")            &
     & '{} rfactor=',rfactor,'abw=',abw,'ventr=',ventr,'crevp=',crevp,  &
     & '{} dwvr=',dwvr,'denomw=',denomw
!
                if (pidep /= 0. .and. dwvi /= 0.)                       &
     &            write(6,"(4(a12,g11.4,1x))")                          &
     & '{} dwvi=',dwvi,'denomi=',denomi,'pidep_max=',pidep_max,         &
     &   'sfactor=',sfactor,                                            &
     & '{} abi=',abi,'ventil=',ventil,'ventil1=',venti1(indexs),        &
     &   'ventil2=',sfactor*venti2(indexs),                             &
     & '{} ventis=',ventis,'didep=',didep
!
                if (pidep > 0. .and. pcond /= 0.)                       &
     &            write(6,"(4(a12,g11.4,1x))")                          &
     & '{} denomw=',denomw,'denomwi=',denomwi,'denomf=',denomf,         &
     &    'dum2=',pcond-piacw
!
                if (fws > 0.) write(6,"(4(a12,g11.4,1x))") '{} fws=',fws
!
                dum = pimlt + picnd - pievp
                if (dum >  0.) write(6,"(4(a12,g11.4,1x))")             &
     & '{} sfactor=',sfactor,'ventil=',ventil,'ventil1=',venti1(indexs),&
     &   'ventil2=',sfactor*venti2(indexs),                             &
     & '{} aievp=',aievp,'dievp=',dievp,'qsw0=',qsw0,'dwv0=',dwv0
   !
              endif
!
!----------------------------------------------------------------------
!-------------- water budget statistics & maximum values --------------
!----------------------------------------------------------------------
!
              if (print_diag) then
                itdx = max( itlo, min( int(tnew-t0c), ithi ) )
                if (qinew > epsq) nstats(itdx,1) = nstats(itdx,1)+1
                if (qinew > epsq .and.  qrnew+qwnew > epsq)             &
     &            nstats(itdx,2) = nstats(itdx,2)+1
                if (qwnew > epsq) nstats(itdx,3) = nstats(itdx,3)+1 
                if (qrnew > epsq) nstats(itdx,4) = nstats(itdx,4)+1
  !
                qmax(itdx,1)  = max(qmax(itdx,1), qinew)
                qmax(itdx,2)  = max(qmax(itdx,2), qwnew)
                qmax(itdx,3)  = max(qmax(itdx,3), qrnew)
                qmax(itdx,4)  = max(qmax(itdx,4), asnownew)
                qmax(itdx,5)  = max(qmax(itdx,5), arainnew)
                qtot(itdx,1)  = qtot(itdx,1)+qinew*thick
                qtot(itdx,2)  = qtot(itdx,2)+qwnew*thick
                qtot(itdx,3)  = qtot(itdx,3)+qrnew*thick
  !
                qtot(itdx,4)  = qtot(itdx,4)+pcond*thick
                qtot(itdx,5)  = qtot(itdx,5)+picnd*thick
                qtot(itdx,6)  = qtot(itdx,6)+pievp*thick
                qtot(itdx,7)  = qtot(itdx,7)+pidep*thick
                qtot(itdx,8)  = qtot(itdx,8)+prevp*thick
                qtot(itdx,9)  = qtot(itdx,9)+praut*thick
                qtot(itdx,10) = qtot(itdx,10)+pracw*thick
                qtot(itdx,11) = qtot(itdx,11)+pimlt*thick
                qtot(itdx,12) = qtot(itdx,12)+piacw*thick
                qtot(itdx,13) = qtot(itdx,13)+piacwi*thick
                qtot(itdx,14) = qtot(itdx,14)+piacwr*thick
                qtot(itdx,15) = qtot(itdx,15)+piacr*thick
  !
                qtot(itdx,16) = qtot(itdx,16)+(wvnew-wv)*thick
                qtot(itdx,17) = qtot(itdx,17)+(qwnew-qw)*thick
                qtot(itdx,18) = qtot(itdx,18)+(qinew-qi)*thick
                qtot(itdx,19) = qtot(itdx,19)+(qrnew-qr)*thick
                qtot(itdx,20) = qtot(itdx,20)+(arainnew-arain)
                qtot(itdx,21) = qtot(itdx,21)+(asnownew-asnow)
                if (qinew > 0.)                                         &
     &            qtot(itdx,22) = qtot(itdx,22)+qinew*thick/rimef
  !
              endif
!
!----------------------------------------------------------------------
!------------------------- update arrays ------------------------------
!----------------------------------------------------------------------
!
              t_col(l)     = tnew                        ! temperature
!
!             qv_col(l)    = max(epsq, wvnew/(1.+wvnew)) ! specific humidity
              qv_col(l)    = max(0.0, wvnew           )  ! specific humidity
              wc_col(l)    = max(0.0, wcnew)             ! total condensate mixing ratio
              qi_col(l)    = max(0.0, qinew)             ! ice mixing ratio
              qr_col(l)    = max(0.0, qrnew)             ! rain mixing ratio
              qw_col(l)    = max(0.0, qwnew)             ! cloud water mixing ratio
              rimef_col(l) = rimef                       ! rime factor
              asnow        = asnownew                    ! accumulated snow
              arain        = arainnew                    ! accumulated rain
!
!#######################################################################
!
            endif   ! end of if (.not. clear) then
          endif     ! end of if (qv_col(l) <= epsq .and. wc_col(l) <= epsq) then
!
        enddo       ! ##### end "l" loop through model levels #####
!
        araing = araing + arain
        asnowg = asnowg + asnow
      enddo              ! do for ntimes=1,mic_step
!
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------------- return to gsmdrive -----------------------
!-----------------------------------------------------------------------
!
      contains
!     end  subroutine gsmcolumn
!
!#######################################################################
!--------- produces accurate calculation of cloud condensation ---------
!#######################################################################
!
      real function condense (pp, qw, rhgrd, tk, wv)
!
      implicit none
!
!---------------------------------------------------------------------------------
!------ the asai (1965) algorithm takes into consideration the release of ------
!------ latent heat in increasing the temperature & in increasing the     ------
!------ saturation mixing ratio (following the clausius-clapeyron eqn.).  ------
!---------------------------------------------------------------------------------
!
      real pp, qw, rhgrd, tk, wv
      integer, parameter :: high_pres=kind_phys
!     integer, parameter :: high_pres=selected_real_kind(15)
      real (kind=high_pres), parameter ::                               &
     & rhlimit=.001, rhlimit1=-rhlimit
      real, parameter :: rcp=1./cp, rcprv=rcp/rv
      real (kind=high_pres) :: cond, ssat, wcdum, tsq
      real wvdum, tdum, xlv, xlv1, xlv2, ws, dwv, esw, rfac
!
!-----------------------------------------------------------------------
!
!--- lv (t) is from bolton (jas, 1980)
!
!     xlv=3.148e6-2370.*tk
!     xlv1=xlv*rcp
!     xlv2=xlv*xlv*rcprv
!
      tdum     = tk
      wvdum    = wv
      wcdum    = qw
      esw      = min(pp, fpvsl(tdum))          ! saturation vapor press w/r/t water
!     ws       = rhgrd*eps*esw/(pp-esw)        ! saturation mixing ratio w/r/t water
      ws       = rhgrd*eps*esw/(pp+epsm1*esw)  ! saturation specific hum w/r/t water
      dwv      = wvdum - ws                    ! deficit grid-scale specific humidity
      ssat     = dwv / ws                      ! supersaturation ratio
      condense = 0.
      rfac     = 0.5                           ! converges faster with 0.5
      do while ((ssat < rhlimit1 .and. wcdum > epsq)                    &
     &           .or. ssat > rhlimit)
!
        xlv   = 3.148e6-2370.*tdum
        xlv1  = xlv*rcp
        xlv2  = xlv*xlv*rcprv
!
!       cond  = dwv/(1.+xlv2*ws/(tdum*tdum))  ! asai (1965, j. japan)
        tsq   = tdum*tdum
        cond  = rfac*dwv*tsq/(tsq+xlv2*ws)    ! asai (1965, j. japan)
!       cond  =      dwv*tsq/(tsq+xlv2*ws)    ! asai (1965, j. japan)
        cond  = max(cond, -wcdum)             ! limit cloud water evaporation
        tdum  = tdum+xlv1*cond                ! updated temperature
        wvdum = wvdum-cond                    ! updated water vapor mixing ratio
        wcdum = wcdum+cond                    ! updated cloud water mixing ratio
        condense = condense + cond            ! total cloud water condensation
        esw  = min(pp, fpvsl(tdum))           ! updated saturation vapor press w/r/t water
!       ws   = rhgrd*eps*esw/(pp-esw)         ! updated saturation mixing ratio w/r/t water
        ws   = rhgrd*eps*esw/(pp+epsm1*esw)   ! updated saturation mixing ratio w/r/t water
        dwv  = wvdum-ws                       ! deficit grid-scale water vapor mixing ratio
        ssat = dwv / ws                       ! grid-scale supersaturation ratio
        rfac = 1.0
      enddo

      end function condense
!
!#######################################################################
!---------------- calculate ice deposition at t<t_ice ------------------
!#######################################################################
!
      real function deposit (pp, rhgrd, tdum, wvdum)
!
      implicit none
!
!--- also uses the asai (1965) algorithm, but uses a different target
!      vapor pressure for the adjustment
!
      real pp, rhgrd, tdum, wvdum
      integer, parameter :: high_pres=kind_phys
!     integer, parameter :: high_pres=selected_real_kind(15)
      real (kind=high_pres), parameter :: rhlimit=.001,                 & 
     & rhlimit1=-rhlimit
      real, parameter :: rcp=1./cp, rcprv=rcp/rv, xls=hvap+hfus         &
     &,                  xls1=xls*rcp, xls2=xls*xls*rcprv
      real (kind=high_pres) :: dep, ssat
      real esi, ws, dwv
!
!-----------------------------------------------------------------------
!
      esi=min(pp, fpvsi(tdum))                  ! saturation vapor press w/r/t ice
!     ws=rhgrd*eps*esi/(pp-esi)                 ! saturation mixing ratio
      ws=rhgrd*eps*esi/(pp+epsm1*esi)           ! saturation mixing ratio
      dwv=wvdum-ws                              ! deficit grid-scale water vapor mixing ratio
      ssat=dwv/ws                               ! supersaturation ratio
      deposit=0.
      do while (ssat > rhlimit .or. ssat < rhlimit1)
   !
   !--- note that xlvs2=ls*lv/(cp*rv)=lv*ws/(rv*t*t)*(ls/cp*dep1),
   !     where ws is the saturation mixing ratio following clausius-
   !     clapeyron (see asai,1965; young,1993,p.405)
   !
        dep=dwv/(1.+xls2*ws/(tdum*tdum))        ! asai (1965, j. japan)
        tdum=tdum+xls1*dep                      ! updated temperature
        wvdum=wvdum-dep                         ! updated ice mixing ratio
        deposit=deposit+dep                     ! total ice deposition
        esi=min(pp, fpvsi(tdum))                ! updated saturation vapor press w/r/t ice
!       ws=rhgrd*eps*esi/(pp-esi)               ! updated saturation mixing ratio w/r/t ice
        ws=rhgrd*eps*esi/(pp+epsm1*esi)         ! updated saturation mixing ratio w/r/t ice
        dwv=wvdum-ws                            ! deficit grid-scale water vapor mixing ratio
        ssat=dwv/ws                             ! grid-scale supersaturation ratio
      enddo
      end function deposit
!
      end  subroutine gsmcolumn


      subroutine rsipath(im, ix, ix2, levs, prsl, prsi, t, q, clw       &
     &,                  f_ice, f_rain, f_rime, flgmin                  &
     &,                  cwatp, cicep, rainp, snowp                     &
     &,                  recwat, rerain, resnow, lprnt, ipr)
!
      implicit none
!
!--------------------cloud----------------------------------------------
      integer im, ix, ix2, levs, ipr
      real    prsl(ix,levs), prsi(ix,levs+1), t(ix,levs), q(ix,levs)    &
     &,       clw(ix2,levs), f_ice(ix2,levs), f_rain(ix2,levs)          &
     &,       f_rime(ix2,levs)                                          &
     &,       cwatp(ix,levs), rainp(ix,levs),  cicep(ix,levs)           &
     &,       snowp(ix,levs), recwat(ix,levs), resnow(ix,levs)          &
     &,       rerain(ix,levs)
      real    flgmin
      real    frice, frrain, qcice, qcwat,  qrain, qsnow,   qtot, sden  &
     &,       cpath, rho,    dsnow, flarge, rimef, xsimass, nlice       &
     &,       tc,    recw1,  drain, xli,    dum,   nlimax, pfac, pp      
!    &,       snofac, tem
!
      real, parameter :: cexp=1./3.
      integer i, l, indexs
      logical lprnt
!

      recw1 = 620.3505 / tnw**cexp         ! cloud droplet effective radius

      do l=1,levs
        do i=1,im
                                           !--- hydrometeor's optical path
           cwatp(i,l) = 0.
           cicep(i,l) = 0.
           rainp(i,l) = 0.
           snowp(i,l) = 0.
                                           !--- hydrometeor's effective radius
           recwat(i,l) = recwmin
           rerain(i,l) = rerainmin
           resnow(i,l) = resnowmin
        enddo
      enddo

      do l=1,levs
        do i=1,im

   !        assume parameterized condensate is 
   !         all water for t>=-10c,
   !         all ice for t<=-30c,
   !         and a linear mixture at -10c > t > -30c
   !
   !    * determine hydrometeor composition of total condensate (qtot)
   !
!         pp   = prsl(i,l) * 1000.0
          pp   = prsl(i,l) / prsi(i,levs+1)
!         pfac = max(0.25, sqrt(sqrt(min(1.0, pp*0.000025))))
!         pfac = max(0.5, sqrt(sqrt(min(1.0, pp*0.000025))))
!         pfac = max(0.5, sqrt(sqrt(min(1.0, pp*0.00002))))
!         pfac = max(0.25, sqrt(sqrt(min(1.0, pp*0.00001))))
!         pfac = max(0.25, sqrt(sqrt(min(1.0, pp))))
!         pfac = max(0.1, sqrt(min(1.0, pp*0.00001)))
!         pfac = max(0.5, sqrt(sqrt(min(1.0, pp*0.000033))))
!         pfac = max(0.5, sqrt(sqrt(min(1.0, pp*0.00004))))
!go       pfac = max(0.5, (sqrt(min(1.0, pp*0.000025))))
          pfac = 1.0
          tc   = t(i,l) - t0c
          qtot = clw(i,l)
          if (qtot > epsq) then
             qcwat=0.
             qcice=0.
             qrain=0.
             qsnow=0.
             frice  = max(0.0, min(1.0, f_ice(i,l)))
             frrain = max(0.0, min(1.0, f_rain(i,l)))
             if(tc <= thom) then
                qcice = qtot
             else
                qcice = frice  * qtot
                qcwat = qtot   - qcice
                qrain = frrain * qcwat
                qcwat = qcwat  - qrain
             endif
    !
    !--- air density (rho), model mass thickness (cpath)
    !
             rho   = prsl(i,l)/(rd*t(i,l)*(1.+eps1*q(i,l)))
             cpath = (prsi(i,l+1)-prsi(i,l))*(1000000.0/grav)

    !! cloud water
    !
    !--- effective radius (recwat) & total water path (cwatp)
    !    assume monodisperse distribution of droplets (no factor of 1.5)
    !
             if(qcwat > 0.) then
                recwat(i,l) = max(recwmin, recw1*(rho*qcwat)**cexp)
                cwatp(i,l)  = cpath*qcwat         ! cloud water path
!               tem         = 5.0*(1+max(0.0,min(1.0,-0.05*tc)))
!               recwat(i,l) = max(recwat(i,l), tem)
             endif

    !! rain
    !
    !--- effective radius (rerain) & total water path (rainp)
    !--- factor of 1.5 accounts for r**3/r**2 moments for exponentially
    !    distributed drops in effective radius calculations
    !    (from m.d. chou's code provided to y.-t. hou)
    !
             if(qrain > 0.) then
                drain       = cn0r0*sqrt(sqrt((rho*qrain)))
                rerain(i,l) = 1.5*max(xmrmin, min(drain, xmrmax))
                rainp(i,l)  = cpath*qrain         ! rain water path
             endif

    !! snow (large ice) & cloud ice
    !
    !--- effective radius (resnow) & total ice path (snowp)
    !--- total ice path (cicep) for cloud ice 
    !--- factor of 1.5 accounts for r**3/r**2 moments for exponentially
    !    distributed ice particles in effective radius calculations 
    !
    !--- separation of cloud ice & "snow" uses algorithm from
    !    subroutine gsmcolumn
    !
             if(qcice > 0.) then
    !
    !--- mean particle size following houze et al. (jas, 1979, p. 160), 
    !    converted from fig. 5 plot of lamdas.  an analogous set of
    !    relationships also shown by fig. 8 of ryan (bams, 1996, p. 66),
    !    but with a variety of different relationships that parallel the
    !    houze curves.
    !
!               dum=max(0.05, min(1., exp(.0536*tc)) )
                dum=max(0.05, min(1., exp(.0564*tc)) )
                indexs=min(mdimax, max(mdimin, int(xmimax*dum) ) )
!               indexs=max(indexsmin, indexs)
!               nlimax=5.e3/sqrt(dum)        !- ver3
                dum=max(flgmin*pfac, dum)
!               dum=max(flgmin, dum)
!               nlimax=20.e3            !- ver3
!               nlimax=50.e3            !- ver3 => comment this line out
                nlimax=10.e3/sqrt(dum)        !- ver3
!               nlimax=5.e3/sqrt(dum)        !- ver3
!               nlimax=6.e3/sqrt(dum)        !- ver3
!               nlimax=7.5e3/sqrt(dum)       !- ver3
!               nlimax=20.e3/dum        !- ver3
!               nlimax=20.e3/max(0.2,dum)        !- ver3
!               nlimax=2.0e3/max(0.1,dum)        !- ver3
!               nlimax=2.5e3/max(0.1,dum)        !- ver3
!               nlimax=10.e3/max(0.2,dum)        !- ver3
!               nlimax=4.e3/max(0.2,dum)        !- ver3
!moorthi        dsnow  = xmimax*exp(.0536*tc)
!moorthi        indexs = max(indexsmin, min(mdimax, int(dsnow)))
    !
    !--- assumed number fraction of large ice to total (large & small)
    !    ice particles, which is based on a general impression of the
    !    literature.
    !
    !    small ice are assumed to have a mean diameter of 50 microns.
    !
                if(tc >= 0.) then
                  flarge=flg1p0
                else
                  flarge = dum
                endif
!------------------------commented by moorthi -----------------------------
!               elseif (tc >= -25.) then
!
!--- note that absence of cloud water (qcwat) is used as a quick
!    substitute for calculating water subsaturation as in gsmcolumn
!
!                  if(qcwat <= 0. .or. tc < -8.
!    &                            .or. tc > -3.)then
!                     flarge=flg0p2
!                  else
      
!--- parameterize effects of rime splintering by increasing
!    number of small ice particles
!
!                     flarge=flg0p1
!                  endif
!               elseif (tc <= -50.) then
!                  flarge=.01
!               else
!                  flarge=.2*exp(.1198*(tc+25))
!               endif
!____________________________________________________________________________

                rimef=max(1., f_rime(i,l))
                xsimass=massi(mdimin)*(1.-flarge)/flarge
!     if (lprnt) print *,' rimef=',rimef,' xsimass=',xsimass
!    &,' indexs=',indexs,' massi=',massi(indexs),' flarge=',flarge
                nlice=rho*qcice/(xsimass+rimef*massi(indexs))
    !
    !--- from subroutine gsmcolumn:
    !--- minimum number concentration for large ice of nlimin=10/m**3 
    !    at t>=0c.  done in order to prevent unrealistically small 
    !    melting rates and tiny amounts of snow from falling to 
    !    unrealistically warm temperatures.
    !
                if(tc >= 0.) then
                   nlice=max(nlimin, nlice)
                elseif (nlice > nlimax) then
      !
      !--- ferrier 6/13/01:  prevent excess accumulation of ice
      !
                   xli=(rho*qcice/nlimax-xsimass)/rimef
 
                   if(xli <= massi(450) ) then
                      dsnow=9.5885e5*xli**.42066
                   else
                      dsnow=3.9751e6*xli**.49870
                   endif
 
                   indexs=min(mdimax, max(indexs, int(dsnow)))
                   nlice=rho*qcice/(xsimass+rimef*massi(indexs))
                endif

!               if (tc > -20.0 .and. indexs >= indexsmin) then
!                 snofac = max(0.0, min(1.0, exp(1.0*(tc+20.0))))
!               if (indexs >= indexsmin) then
!               if (tc > -20.0 .or. indexs >= indexsmin) then
!               if (tc > -40.0) then
!               if (tc >= -40.0 .or. prsl(i,l) > 50.0) then
!!              if (tc >= -20.0) then
!               if (tc >= -20.0 .or. prsl(i,l) > 50.0) then
!               if ((tc >= -20.0 .or.
!    &              prsi(i,levs+1)-prsi(i,l) < 30.0)
                if (prsi(i,levs+1)-prsi(i,l) < 40.0                     &
!               if (prsi(i,levs+1)-prsi(i,l) < 70.0
     &              .and. indexs >= indexsmin) then
!    &              prsi(i,levs)-prsl(i,l) < 20.0) then
!    &              prsi(i,levs)-prsl(i,l) < 30.0) then
!    &              prsi(i,levs)-prsl(i,l) < 40.0) then
!                 snofac = max(0.0, min(1.0, 0.05*(tc+40.0)))
!                 snofac = max(0.0, min(1.0, 0.1*(tc+25.0)))
!                 snofac = max(0.0, min(1.0, 0.0667*(tc+25.0)))
!               if (indexs > indexsmin) then
                  qsnow     = min(qcice, nlice*rimef*massi(indexs)/rho)
!    &                      * snofac
                endif
!               qsnow       = qcice
                qcice       = max(0., qcice-qsnow)
!               qsnow       = 0.0
                cicep (i,l) = cpath*qcice          ! cloud ice path
                resnow(i,l) = 1.5*float(indexs)
                sden        = sdens(indexs)/rimef  ! 1/snow density
                snowp (i,l) = cpath*qsnow*sden     ! snow path / snow density
!               snowp (i,l) = cpath*qsnow          ! snow path / snow density
!               if (lprnt .and. i == ipr) then
!                 print *,' l=',l,' snowp=',snowp(i,l),' cpath=',cpath
!    &,' qsnow=',qsnow,' sden=',sden,' rimef=',rimef,' indexs=',indexs
!    &,' sdens=',sdens(indexs),' resnow=',resnow(i,l)
!    &,' qcice=',qcice,' cicep=',cicep(i,l)
!               endif

                
             endif                                 ! end qcice block
           endif                                   ! qtot if block

        enddo
      enddo
!
      end subroutine rsipath



!-----------------------------------
      subroutine rsipath2                                               &
!...................................

!  ---  inputs:
     &     ( plyr, plvl, tlyr, qlyr, qcwat, qcice, qrain, rrime,        &
     &       im, levs, iflip, flgmin,                                   &
!  ---  outputs:
     &       cwatp, cicep, rainp, snowp, recwat, rerain, resnow, snden  &
     &     )

! =================   subprogram documentation block   ================ !
!                                                                       !
! abstract:  this program is a modified version of ferrier's original   !
!   "rsipath" subprogram.  it computes layer's cloud liquid, ice, rain, !
!   and snow water condensate path and the partical effective radius    !
!   for liquid droplet, rain drop, and snow flake.                      !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (im,levs) : model layer mean pressure in mb (100pa)           !
!   plvl  (im,levs+1):model level pressure in mb (100pa)                !
!   tlyr  (im,levs) : model layer mean temperature in k                 !
!   qlyr  (im,levs) : layer specific humidity in gm/gm                  !
!   qcwat (im,levs) : layer cloud liquid water condensate amount        !
!   qcice (im,levs) : layer cloud ice water condensate amount           !
!   qrain (im,levs) : layer rain drop water amount                      !
!   rrime (im,levs) : mass ratio of total to unrimed ice ( >= 1 )       !
!   im              : horizontal dimention                              !
!   levs            : vertical layer dimensions                         !
!   iflip           : control flag for in/out vertical indexing         !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   flgmin          : minimum large ice fraction                        !
!   lprnt           : logical check print control flag                  !
!                                                                       !
! output variables:                                                     !
!   cwatp (im,levs) : layer cloud liquid water path                     !
!   cicep (im,levs) : layer cloud ice water path                        !
!   rainp (im,levs) : layer rain water path                             !
!   snowp (im,levs) : layer snow water path                             !
!   recwat(im,levs) : layer cloud eff radius for liqid water (micron)   !
!   rerain(im,levs) : layer rain water effective radius      (micron)   !
!   resnow(im,levs) : layer snow flake effective radius      (micron)   !
!   snden (im,levs) : 1/snow density                                    !
!                                                                       !
!                                                                       !
! usage:     call rsipath2                                              !
!                                                                       !
! subroutines called:  none                                             !
!                                                                       !
! program history log:                                                  !
!      xx-xx-2001   b. ferrier     - original program                   !
!      xx-xx-2004   s. moorthi     - modified for use in gfs model      !
!      05-20-2004   y. hou         - modified, added vertical index flag!
!                     to reduce data flipping, and rearrange code to    !
!                     be comformable with radiation part programs.      !
!                                                                       !
!  ====================    end of description    =====================  !
!

      implicit none

!  ---  constant parameter:
      real, parameter :: cexp= 1.0/3.0

!  ---  inputs:
      real, dimension(:,:), intent(in) ::                               &
     &       plyr, plvl, tlyr, qlyr, qcwat, qcice, qrain, rrime

      integer, intent(in) :: im, levs, iflip
      real, dimension(:),   intent(in) :: flgmin
!     logical, intent(in) :: lprnt

!  ---  output:
      real, dimension(:,:), intent(out) ::                              &
     &       cwatp, cicep, rainp, snowp, recwat, rerain, resnow, snden

!  ---  locals:
!     real,    dimension(im,levs) :: delp, pp1, pp2

      real    :: recw1, dsnow, qsnow, qqcice, flarge, xsimass, pfac,    &
     &           nlice, xli, nlimax, dum, tem,                          &
     &           rho, cpath, totcnd, tc

      integer :: i, k, indexs, ksfc, k1
!
!===>  ...  begin here
!
      recw1 = 620.3505 / tnw**cexp         ! cloud droplet effective radius

      do k = 1, levs
        do i = 1, im
                                           !--- hydrometeor's optical path
           cwatp(i,k) = 0.0
           cicep(i,k) = 0.0
           rainp(i,k) = 0.0
           snowp(i,k) = 0.0
           snden(i,k) = 0.0
                                           !--- hydrometeor's effective radius
           recwat(i,k) = recwmin
           rerain(i,k) = rerainmin
           resnow(i,k) = resnowmin
        enddo
      enddo

!  ---  set up pressure related arrays, convert unit from mb to cb (10pa)
!       cause the rest part uses cb in computation

      if (iflip == 0) then        ! data from toa to sfc
        ksfc = levs + 1
        k1   = 0
      else                        ! data from sfc to top
        ksfc = 1
        k1   = 1
      endif                       ! end_if_iflip
!
      do k = 1, levs
        do i = 1, im
          totcnd = qcwat(i,k) + qcice(i,k) + qrain(i,k)
          qsnow = 0.0
          if(totcnd > epsq) then

!  ---  air density (rho), model mass thickness (cpath), temperature in c (tc)

            rho   = 0.1 * plyr(i,k)                                     &
     &            / (rd* tlyr(i,k) * (1.0 + eps1*qlyr(i,k)))
            cpath = abs(plvl(i,k+1) - plvl(i,k)) * (100000.0 / grav)
            tc    = tlyr(i,k) - t0c

!! cloud water
!
!  ---  effective radius (recwat) & total water path (cwatp):
!       assume monodisperse distribution of droplets (no factor of 1.5)

            if (qcwat(i,k) > 0.0) then
              recwat(i,k) = max(recwmin,recw1*(rho*qcwat(i,k))**cexp)
              cwatp (i,k) = cpath * qcwat(i,k)           ! cloud water path
!             tem         = 5.0*(1.0 + max(0.0, min(1.0,-0.05*tc)))
!             recwat(i,k) = max(recwat(i,k), tem)
            endif

!! rain
!
!  ---  effective radius (rerain) & total water path (rainp):
!       factor of 1.5 accounts for r**3/r**2 moments for exponentially
!       distributed drops in effective radius calculations
!       (from m.d. chou's code provided to y.-t. hou)

            if (qrain(i,k) > 0.0) then
              tem         = cn0r0 * sqrt(sqrt(rho*qrain(i,k)))
              rerain(i,k) = 1.5 * max(xmrmin, min(xmrmax, tem))
              rainp (i,k) = cpath * qrain(i,k)           ! rain water path
            endif

!! snow (large ice) & cloud ice
!
!  ---  effective radius (resnow) & total ice path (snowp) for snow, and
!       total ice path (cicep) for cloud ice:
!       factor of 1.5 accounts for r**3/r**2 moments for exponentially
!       distributed ice particles in effective radius calculations
!       separation of cloud ice & "snow" uses algorithm from subroutine gsmcolumn

!           pfac = max(0.5, sqrt(sqrt(min(1.0, pp1(i,k)*0.00004))))
!go         pfac = max(0.5, (sqrt(min(1.0, pp1(i,k)*0.000025))))
            pfac = 1.0

            if (qcice(i,k) > 0.0) then

!  ---  mean particle size following houze et al. (jas, 1979, p. 160),
!       converted from fig. 5 plot of lamdas.  an analogous set of
!       relationships also shown by fig. 8 of ryan (bams, 1996, p. 66),
!       but with a variety of different relationships that parallel
!       the houze curves.

!             dum = max(0.05, min(1.0, exp(0.0536*tc) ))
              dum = max(0.05, min(1.0, exp(0.0564*tc) ))
              indexs = min(mdimax, max(mdimin, int(xmimax*dum) ))
              dum=max(flgmin(i)*pfac, dum)

!  ---  assumed number fraction of large ice to total (large & small) ice
!       particles, which is based on a general impression of the literature.
!       small ice are assumed to have a mean diameter of 50 microns.

              if (tc >= 0.0) then
                flarge = flg1p0
              else
                flarge = dum
!               flarge = max(flgmin*pfac, dum)
              endif
!------------------------commented by moorthi -----------------------------
!             elseif (tc >= -25.0) then
!
!  ---  note that absence of cloud water (qcwat) is used as a quick
!       substitute for calculating water subsaturation as in gsmcolumn
!
!               if (qcwat(i,k) <= 0.0 .or. tc < -8.0                 &
!    &                                .or. tc > -3.0) then
!                 flarge = flg0p2
!               else
!
!  ---  parameterize effects of rime splintering by increasing
!       number of small ice particles
!
!                 flarge = flg0p1
!               endif
!             elseif (tc <= -50.0) then
!               flarge = 0.01
!             else
!               flarge = 0.2 * exp(0.1198*(tc+25.0))
!             endif
!____________________________________________________________________________

              xsimass = massi(mdimin) * (1.0 - flarge) / flarge
!             nlimax = 20.0e3                                      !- ver3
!             nlimax=50.e3                 !- ver3 => comment this line out
              nlimax=10.e3/sqrt(dum)       !- ver3
!             nlimax=5.e3/sqrt(dum)        !- ver3
!             nlimax=6.e3/sqrt(dum)        !- ver3
!             nlimax=7.5e3/sqrt(dum)       !- ver3

!             indexs = min(mdimax, max(mdimin, int(xmimax*dum) ))
!moorthi      dsnow  = xmimax * exp(0.0536*tc)
!moorthi      indexs = max(indexsmin, min(mdimax, int(dsnow)))

!             if (lprnt) print *,' rrime=',rrime,' xsimass=',xsimass,   &
!    &       ' indexs=',indexs,' massi=',massi(indexs),' flarge=',flarge

              tem = rho * qcice(i,k)
              nlice = tem / (xsimass +rrime(i,k)*massi(indexs))

!  ---  from subroutine gsmcolumn:
!       minimum number concentration for large ice of nlimin=10/m**3
!       at t>=0c.  done in order to prevent unrealistically small
!       melting rates and tiny amounts of snow from falling to
!       unrealistically warm temperatures.

              if (tc >= 0.0) then

                nlice = max(nlimin, nlice)

              elseif (nlice > nlimax) then

!  ---  ferrier 6/13/01:  prevent excess accumulation of ice

                xli = (tem/nlimax - xsimass) / rrime(i,k)

                if (xli <= massi(450) ) then
                  dsnow = 9.5885e5 * xli**0.42066
                else
                  dsnow = 3.9751e6 * xli** 0.49870
                endif

                indexs = min(mdimax, max(indexs, int(dsnow)))
                nlice = tem / (xsimass + rrime(i,k)*massi(indexs))

              endif                               ! end if_tc block

!             if (abs(plvl(i,ksfc)-plvl(i,k+k1)) < 300.0                &
!             if (abs(plvl(i,ksfc)-plvl(i,k+k1)) < 400.0                &
!             if (plvl(i,k+k1) > 600.0                                  &
!    &                            .and. indexs >= indexsmin) then
!             if (tc > -20.0 .and. indexs >= indexsmin) then
              if (plvl(i,ksfc) > 850.0 .and.                            &
!    &            plvl(i,k+k1) > 600.0 .and. indexs >= indexsmin) then
     &            plvl(i,k+k1) > 700.0 .and. indexs >= indexsmin) then ! 20060516
!!            if (plvl(i,ksfc) > 800.0 .and.                            &
!!   &            plvl(i,k+k1) > 700.0 .and. indexs >= indexsmin) then
!             if (plvl(i,ksfc) > 700.0 .and.                            &
!    &            plvl(i,k+k1) > 600.0 .and. indexs >= indexsmin) then
                qsnow = min( qcice(i,k),                                &
     &                       nlice*rrime(i,k)*massi(indexs)/rho )
              endif

              qqcice      = max(0.0, qcice(i,k)-qsnow)
              cicep (i,k) = cpath * qqcice          ! cloud ice path
              resnow(i,k) = 1.5 * float(indexs)
              snden (i,k) = sdens(indexs) / rrime(i,k)   ! 1/snow density
              snowp (i,k) = cpath*qsnow             ! snow path
!             snowp (i,k) = cpath*qsnow*snden(i,k)  ! snow path / snow density

!             if (lprnt .and. i == ipr) then
!             if (i == 2) then
!               print *,' l=',k,' snowp=',snowp(i,k),' cpath=',cpath,   &
!    &         ' qsnow=',qsnow,' sden=',snden(i,k),' rrime=',rrime(i,k),&
!    &         ' indexs=',indexs,' sdens=',sdens(indexs),' resnow=',    &
!    &           resnow(i,k),' qcice=',qqcice,' cicep=',cicep(i,k)
!           endif

            endif                                 ! end if_qcice block
          endif                                   ! end if_totcnd block

        enddo
      enddo
!
!...................................
      end subroutine rsipath2
!-----------------------------------

      end module module_microphysics

