
      module efield
!--------------------------------------------------------------------- 
! description: calculates the electric potential for a given year,
!      day of year,UT, F10.7, B_z(K_p)
! - low/midlatitudes electric potential is from an empirical model from
!   L.Scherliess ludger@gaim.cass.usu.edu
! - high latitude electric potential is from Weimer96 model
! - the transition zone is smoothed
! - output is the horizontal global electric field in magnetic coordinates direction
!  at every magnetic local time grid point expressed in degrees (0 deg-0MLT; 360deg 24 MLT)
!
! input 
!      integer :: iday,     ! day number of year
!                 iyear     ! year
!      real:: ut,       ! universal time 
!                 F10.7,    ! solar flux       (see ionosphere module)
!                 bz        ! component of IMF (see ionosphere module)
! output
!      real ::               &
!       ed1(0:nmlon,0:nmlat),    &  ! zonal electric field Ed1  [V/m] 
!       ed2(0:nmlon,0:nmlat)        ! meridional electric field Ed2/sin I_m  [V/m]  
!
! notes:
!
! - !to be done (commented out): input S_a F10.7/ Kp from WACCM and calculate B_z 
!    from these inputs
! - assume regular geomagnetic grid 
! - uses average year 365.24 days/year 30.6001 day/mo s. Weimer
! - get_tilt works only for iyear >= 1900
! - Weimer model 1996, Dan Weimer (not with the updates from B.Emery)
! - fixed parameters: B_z, B_y units nT  CHANGE THIS
!                     F10.7
! - we assume that the reference height is 300km for the emperical potential model
! - as a first approximation the electric field is constant in height
!   WATCH what is the upper boundary condition in WACCM
! - for all the calculation done here we set the reference height to the same 
!   value as in tiegcm (hr=130km)
! - 12/15/03 input value iseasav : replaced by day -> month and day of month
! - 12/15/03 S_aM calculated according to Scherliess draft paper and added
!   S_aM(corrected) = 90*(S_aM+1) to get variation in fig 1 Scherliess draft
!
!   Apr 06 2012 Henry Juang, initial implement for nems
!   Nov 20 2014 Jun   Wang,  change JULDAY to JULDAY_WAM
!
! Author: A. Maute Dec 2003  am 12/30/03 
!------------------------------------------------------------------------------ 

!     use shr_kind_mod,  only: r8 => shr_kind_r8
!     use physconst,     only: pi
!     use abortutils,    only: endrun
!     use cam_logfile,   only: iulog
   
      implicit none

      public :: efield_init,   ! interface routine                     
     &          get_efield     ! interface routine
      public :: ed1,           ! zonal electric field Ed1  [V/m] 
     &          ed2,           ! meridional electric field Ed2 [V/m] 
     &          potent,        ! electric potential [V]
     &	        nmlon, nmlat,  ! dimension of mag. grid 
     &          dlatm, dlonm,  ! grid spacing of mag. grid 
     &	        ylonm, ylatm   ! magnetic longitudes/latitudes (degc)
     &,iday,iyear,iday_m,imo,f107d,by,bz,ut

      public :: Coef , Cn,ML,MM1,MaxL,MaxM,MaxN,ALAMN,ALAMX,ALAMR,
     &STPD,STP2,CSTP,SSTP,CX,ST,CT,AM,EPOCH,TH0,PH0,DIPOLE
!     private

      integer ::   
     &  iday,            ! day number of year
     &  iyear,           ! year
     &  iday_m,          ! day of month
     &  imo              !month
      real ::  ut       ! universal time  

!---------------------------------------------------------------------- 
! solar parameters
!---------------------------------------------------------------------- 
      real ::   f107d           ! 10.7 cm solar flux
      real ::   by              ! By component of IMF [nT]
      real ::   bz              ! Bz component of IMF [nT]
      private
!---------------------------------------------------------------------- 
! mag. grid dimensions (assumed resolution of 2deg)
!---------------------------------------------------------------------- 
      integer, parameter ::   
     &nmlon = 180,          ! mlon 
     &nmlat = 90,           ! mlat
     &nmlath= nmlat/2,      ! mlat/2
     &nmlonh= nmlon/2,      ! mlon/2
     &nmlonp1 = nmlon+1,    ! mlon+1 
     &nmlatp1 = nmlat+1,     ! mlat+1
     &iulog=10

      real ::         
     &  ylatm(0:nmlat),      ! magnetic latitudes (deg)
     &  ylonm(0:nmlon),      ! magnetic longitudes (deg)
     &  dlonm,	             ! delon lon grid spacing
     &  dlatm		     ! delat lat grid spacing

!---------------------------------------------------------------------- 
! array on magnetic grid:    
!---------------------------------------------------------------------- 
      real ::                 
     &  potent(0:nmlon,0:nmlat),! electric potential   [V]  
     &  ed1(0:nmlon,0:nmlat),  ! zonal electric field Ed1  [V/m] 
     &  ed2(0:nmlon,0:nmlat) ! meridional electric field Ed2/sin I_m  [V/m]  
       
      real :: 
     & date,   ! iyear+iday+ut
     & day      ! iday+ut

      logical, parameter :: iutav=.false.   ! .true.  means UT-averaging 
                                        ! .false. means no UT-averaging
!     real, parameter ::  v_sw = 400.      ! solar wind velocity [km/s]
      real, parameter ::  v_sw = 450.      ! solar wind velocity [km/s]

!---------------------------------------------------------------------- 
! boundary for Weimer
!---------------------------------------------------------------------- 
      real, parameter :: bnd_wei = 44. ! colat. [deg]
      integer :: nmlat_wei
      
!---------------------------------------------------------------------- 
! flag for choosing factors for empirical low latitude model      
!---------------------------------------------------------------------- 
      integer, parameter ::  iseasav = 0  ! flag for season 

!---------------------------------------------------------------------- 
! constants:
!---------------------------------------------------------------------- 
      real, parameter ::          
     &r_e  =  6.371e6,     ! radius_earth [m] (same as for apex.F90)
     & h_r  = 130.0e3,    ! reference height [m] (same as for apex.F90)
     &dy2yr= 365.24,     ! day per avg. year used in Weimer
     &dy2mo= 30.6001,    ! day per avg. month used in Weimer
     &pi=3.141592653

      real   
     &  rtd ,         ! radians -> deg
     &	dtr,          ! deg -> radians
     &	sqr2,           
     &	hr2rd,        ! pi/12 hrs
     &	dy2rd,        ! 2*pi/365.24  average year
     &	deg2mlt,      ! for mlon to deg
     &	mlt2deg,      ! for mlt to mlon
     &  sinIm_mag(0:nmlat)    ! sinIm

      integer :: jmin, jmax   ! latitude index for interpolation of 
                              ! northward e-field ed2 at mag. equator

!---------------------------------------------------------------------- 
!  for spherical harmonics
!---------------------------------------------------------------------- 
      integer, parameter ::  
     &	nm   = 19,     
     &	mm   = 18,    					
     &	nmp  = nm + 1, 					       
     &	mmp  = mm + 1	  

      real :: r(0:nm,0:mm)      ! R_n^m
      real :: pmopmmo(0:mm)     ! sqrt(1+1/2m)

!---------------------------------------------------------------------- 
!  index for factors f_m(mlt),f_l(UT),f_-k(d)
!---------------------------------------------------------------------- 
      integer, parameter :: ni = 1091  ! for n=12 m=-18:18
      integer :: imax                                         ! max number of index
      integer,dimension(0:ni) :: kf,lf, mf, nf, jf
      real :: ft(1:3,0:2)  ! used for f_-k(season,k)

      real ::  a_klnm(0:ni)        !  A_klm
      real ::  a_lf(0:ni)          ! A_klmn^lf for minimum  
      real ::  a_hf(0:ni)          ! A_klmn^hf for maximum
!---------------------------------------------------------------------- 
!replace wei96.f common block 
!---------------------------------------------------------------------- 
      real :: Coef(0:1,0:8,0:3) 
      real :: Cn(0:3,0:1,0:4,0:1,0:8,0:3) 
      integer  ML,MM1,MaxL,MaxM,MaxN
      real ALAMN,ALAMX,ALAMR,STPD,STP2,CSTP,SSTP
      real CX(9),ST(6),CT(6),AM(3,3,11)
      real , parameter :: EPOCH=1980.,TH0=11.19,PH0=-70.76,
     & DIPOLE=.30574

!---------------------------------------------------------------------- 
! high_latitude boundary
!---------------------------------------------------------------------- 
      real, parameter ::    
     &ef_max  = 0.015,  ! max e-field for high latitude boundary location [V/m]
     &lat_sft = 54.	 ! shift of highlat_bnd to 54 deg
      integer :: ilat_sft        ! index of shift for high latitude boundary
      integer, parameter :: nmax_sin = 2 ! max. wave number to be represented
      logical, parameter :: debug =.false.
!
      contains

      subroutine efield_init
!hmhj subroutine efield_init(efield_lflux_file, efield_hflux_file, 
!hmhj&efield_wei96_file)
!--------------------------------------------------------------------
! Purpose: read in and set up coefficients needed for electric field
!          calculation (independent of time & geog. location)
!
! Method:
!
! Author: A. Maute Dec 2003  am 12/17/03 
!-------------------------------------------------------------------
!hmhj character(len=*), intent(in) :: efield_lflux_file
!hmhj character(len=*), intent(in) :: efield_hflux_file
!hmhj character(len=*), intent(in) :: efield_wei96_file

      character(len=*), parameter :: 
     &                  efield_lflux_file='global_idea_coeff_lflux.dat',
     &                  efield_hflux_file='global_idea_coeff_hflux.dat',
     &                  efield_wei96_file='global_idea_wei96.cofcnts'

      call constants	 ! calculate constants
!-----------------------------------------------------------------------
! low/midlatitude potential from Scherliess model
!-----------------------------------------------------------------------
      call read_acoef (efield_lflux_file, efield_hflux_file)	! read in A_klnm for given S_aM
      call index_quiet  ! set up index for f_m(mlt),f_l(UT),f_-k(d)
      call prep_fk	! set up the constant factors for f_k
      call prep_pnm	! set up the constant factors for P_n^m & dP/d phi
!-----------------------------------------------------------------------
!following part should be independent of time & location if IMF constant
!-----------------------------------------------------------------------
      call ReadCoef (efield_wei96_file)

      end subroutine efield_init

      subroutine get_efield
!-----------------------------------------------------------------------
! Purpose: calculates the global electric potential field on the
!          geomagnetic grid (MLT in deg) and derives the electric field 
!
! Method:
!
! Author: A. Maute Dec 2003  am 12/17/03    
!-----------------------------------------------------------------------

!     use time_manager,   only : get_curr_calday, get_curr_date
!     use mo_solar_parms, only : get_solar_parms
!     use mag_parms,      only : get_mag_parms
!     use cam_control_mod, only: magfield_fix_year
!     use spmd_utils,      only: masterproc

      integer :: idum1, idum2, tod ! time of day [s] 
      real kp

!-----------------------------------------------------------------------
! get current calendar day of year & date components 
! valid at end of current timestep
!-----------------------------------------------------------------------
!     iday = get_curr_calday()                   ! day of year
!     call get_curr_date (iyear,imo,iday_m,tod)! year, time of day [sec]
!     iyear = magfield_fix_year
!     iyear = 1995

!     if( iyear < 1900 ) then
!       write(iulog,"(/,'>>> get_efield: year < 1900 not possible: 
!    &year=',i5)") iyear
!       call endrun
!     end if

      tod=ut*3600.
!     ut = tod/3600.                   ! UT of day [sec]

!-----------------------------------------------------------------------
! get solar parms
!-----------------------------------------------------------------------
!     call get_solar_parms( f107_s = f107d )
!-----------------------------------------------------------------------
! get mag parms
!-----------------------------------------------------------------------
!     call get_mag_parms( by = by, bz = bz )
!     print*,by,bz,f107d,ut
!#ifdef EFIELD_DIAGS
!      if( masterproc ) then
!         write(iulog,*) 'get_efield: f107d,by,bz = ', f107d,by,bz 
!      end if
!#endif
!-----------------------------------------------------------------------
! ajust S_a
!-----------------------------------------------------------------------
      call adj_S_a
!-----------------------------------------------------------------------
! calculate global electric potential    
!-----------------------------------------------------------------------
      call GlobalElPotential
!     print*,'pot_efield',potent(149,66),potent(149,64)

!-----------------------------------------------------------------------
! calculate derivative of global electric potential 
!-----------------------------------------------------------------------
      call DerivPotential
!     print*,'ed2_efield',ed2(149,65),potent(149,66),potent(149,64)

      end subroutine get_efield

      subroutine GlobalElPotential
!-----------------------------------------------------------------------
! Purpose: calculates the global electric potential field on the
!          geomagnetic grid (MLT in deg) 
!
! Method: rewritten code from Luedger Scherliess (11/20/99 LS)
!     routine to calculate the global electric potential in magnetic
!     Apex coordinates (Latitude and MLT).
!     High Latitude Model is Weimer 1996.
!     Midlatitude model is Scherliess 1999.
!     Interpolation in a transition region at about 60 degree 
!     magnetic apex lat
!
! Author: A. Maute Dec 2003  am 12/17/03 
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------
      integer  :: ilon, ilat, idlat
      integer  :: ihlat_bnd(0:nmlon)     ! high latitude boundary
      integer  :: itrans_width(0:nmlon)  ! width of transition zone
      real :: mlt, mlon, mlat, mlat_90, pot
      real :: pot_midlat(0:nmlon,0:nmlat) ! potential from L. Scherliess model
      real :: pot_highlat(0:nmlon,0:nmlat) ! potential from Weimer model
      real :: pot_highlats(0:nmlon,0:nmlat)! smoothed potential from Weimer model

!-----------------------------------------------------------------------
! Externals
!-----------------------------------------------------------------------
      real,external :: EpotVal        ! in wei96.f

!-----------------------------------------------------------------------
! convert to date and day	
!-----------------------------------------------------------------------
      day  = iday + ut/24.
      date = iyear + day/dy2yr

!-----------------------------------------------------------------------
! low/midlatitude electric potential - empirical model Scherliess 1999  
!-----------------------------------------------------------------------
!$omp parallel do private(ilat, ilon, mlat, pot)
      do ilat = 0,nmlath             ! Calculate only for one magn. hemisphere
	mlat = ylatm(ilat)                      ! mag. latitude
        do ilon = 0,nmlon	                ! lon. loop
          call efield_mid( mlat, ylonm(ilon), pot )
	  pot_midlat(ilon,ilat+nmlath) = pot	! SH/NH symmetry 
	  pot_midlat(ilon,nmlath-ilat) = pot
        end do
      end do
!     print*,'www1','midlat',pot_midlat(149,66)

!-----------------------------------------------------------------------
! hight latitude potential from Weimer model
! at the poles Weimer potential is not longitudinal dependent
!-----------------------------------------------------------------------
      call prep_weimer    ! calculate IMF angle & magnitude, tilt

!$omp parallel do private(ilat, ilon, mlat_90, pot)
      do ilat = 0,nmlat_wei  ! Calculate only for one magn. hemisphere
        mlat_90 = 90. - ylatm(ilat)  ! mag. latitude
        do ilon = 0,nmlon
    	  pot  = 1000.*EpotVal( mlat_90, ylonm(ilon)*deg2mlt ) ! calculate potential (kv -> v)
!-----------------------------------------------------------------------
! NH/SH symmetry
!-----------------------------------------------------------------------
    	  pot_highlat(ilon,ilat)        = pot
    	  pot_highlat(ilon,nmlat-ilat)  = pot
    	  pot_highlats(ilon,ilat)       = pot
    	  pot_highlats(ilon,nmlat-ilat) = pot
! bad value com from EpotVal
!         if(ilat.eq.22.and.ilon.eq.148)
!    & print*,'www2',ilat,ilon,pot,mlat_90,ylonm(ilon)*deg2mlt
        end do
      end do     
!     print*,'www2','highlat',ut,by,bz,pot_highlat(0:180,68),nmlat_wei

!-----------------------------------------------------------------------
! weighted smoothing of high latitude potential
!-----------------------------------------------------------------------
      idlat = 2              ! smooth over -2:2 = 5 grid points
      call pot_latsmo( pot_highlats, idlat )
!     print*,'www2','highlat',ut,pot_highlat(0:180,45)
!-----------------------------------------------------------------------
! calculate the height latitude bounday ihl_bnd
! 1. calculate E field from weimar model
!    boundary is set where the total electric field exceeds
!    0.015 V/m (corresp. approx. to 300 m/s)
! 2. moved halfways to 54 deg 
! output : index 0-pole nmlath-equator
!-----------------------------------------------------------------------
      call highlat_getbnd( ihlat_bnd )
!-----------------------------------------------------------------------
! 3. adjust high latitude boundary sinusoidally
!    calculate width of transition zone
!-----------------------------------------------------------------------
      call bnd_sinus( ihlat_bnd, itrans_width ) 
!-----------------------------------------------------------------------
! 4. ajust high latitude potential to low latitude potential      
!-----------------------------------------------------------------------
!     print*,'www30',ihlat_bnd
      call highlat_adjust( pot_highlats, pot_highlat, pot_midlat, 
     &ihlat_bnd )
!     print*,'www3','highlat',ut,pot_highlat(145:153,68)
!     print*,'www3','midlat',ut,pot_midlat(145:153,68)
!-----------------------------------------------------------------------
! interpolation of high and low/midlatitude potential in the
! transition zone and put it into global potent array
!-----------------------------------------------------------------------
      call interp_poten( pot_highlats, pot_highlat, pot_midlat, 
     &ihlat_bnd, itrans_width) 
!     print*,'www4','potent',ut,by,bz,potent(0:181,68)
!-----------------------------------------------------------------------
! potential weighted smoothing in latitude
!-----------------------------------------------------------------------
      idlat = 2                 ! smooth over -2:2 = 5 grid points
      call pot_latsmo2( potent, idlat )
!     print*,'www5','pot_efield',potent(149,68)
!-----------------------------------------------------------------------
! potential smoothing in longitude
!-----------------------------------------------------------------------
      idlat = nmlon/48          ! smooth over -idlat:idlat grid points
      call pot_lonsmo( potent, idlat )
!     print*,'www6','pot_efield',ut,by,bz,potent(0:180,68)
!-----------------------------------------------------------------------
! output
!-----------------------------------------------------------------------
! output ( change later to netcdf file)
!      do ilat=0,nmlat
!       do ilon=0,nmlon
!         write(iulog,'(4(x,f12.5))') ylatm(ilat),ylonm(ilon), &
!           potent(ilon,ilat),potent(ilon,nmlat-ilat)
!         write(iulog,'(4(x,f12.5))') ylatm(ilat),ylonm(ilon), &
!           potent(ilon,ilat),potent(ilon,nmlat-ilat)
!	write(iulog,'(f10.3)') potent(ilon,ilat)
!       end do
!      end do

      end subroutine GlobalElPotential

      subroutine ff( ph, mt, f )                                                    
!-----------------------------------------------------------------------
!Purpose: calculate F for normalized associated Legendre polynomial P_n^m
!          Ref.: Richmond J.Atm.Ter.Phys. 1974
!
! Method:  f_m(phi) = sqrt(2) sin(m phi) m > 0
!                   = 1                  m = 0
!                   = sqrt(2) cos(m phi) m < 0
!
! Author: A. Maute Nov 2003  am 11/18/03
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
! dummy arguments
!-----------------------------------------------------------------------
      integer,intent(in)   :: mt
      real,intent(in)  :: ph	! geo. longitude of 0SLT (ut*15)
      real,intent(out) :: f(-mt:mt)

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------
      integer  :: m, i, j, mmo
      real :: sp, cp    

      sp   = sin( ph/rtd )
      cp   = cos( ph/rtd )
      f(0) = 1.e0
                                                                
      f(-1) = sqr2*cp
      f(1)  = sqr2*sp      								 
      do m = 2,mt
        mmo   = m - 1  
        f(m)  = f(-mmo)*sp + cp*f(mmo)
        f(-m) = f(-mmo)*cp - sp*f(mmo)
      end do      

      end subroutine ff                                                                      

      subroutine pnm( ct, p )
!----------------------------------------------------------------      
! Purpose: normalized associated Legendre polynomial P_n^m
!          Ref.: Richmond J.Atm.Ter.Phys. 1974
! Method:
!   P_m^m    = sqrt(1+1/2m)*si*P_m-1^m-1                  m>0
!   P_n^m    = [cos*P_n-1^m - R_n-1^m*P_n-2^m ]/R_n^m     n>m>=0
!   dP/d phi = n*cos*P_n^m/sin-(2*n+1)*R_n^m*P_n-1^m/sin  n>=m>=0
!   R_n^m    = sqrt[ (n^2-m^2)/(4n^2-1) ]
!
! Author: A. Maute Nov 2003  am 11/18/03
!--------------------------------------------------------------------                                                                   

      implicit none

!-----------------------------------------------------------------------
! dummy arguments
!-----------------------------------------------------------------------
      real, intent(inout) :: ct ! cos(colat)                 
      real, intent(inout) :: p(0:nm,0:mm)

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------
      integer  :: mp, m, n, np
      real :: pm2, st

!      ct = min( ct,.99 )		! cos(colat)
      st = sqrt( 1. - ct*ct ) 	! sin(colat)

      p(0,0) = 1.  
      do mp = 1,mmp  ! m+1=1,mm+1
        m = mp - 1
	if( m >= 1 ) then
           p(m,m) = pmopmmo(m)*p(m-1,m-1)*st 			
	end if
	pm2 = 0.                                                                  
	do n = mp,nm                    ! n=m+1,N
	   np     = n + 1
	   p(n,m) = (ct*p(n-1,m) - r(n-1,m)*pm2)/r(n,m)
	   pm2    = p(n-1,m)
        end do
      end do

      end subroutine pnm                                                                         

      subroutine prep_pnm
!-----------------------------------------------------------------      
! Purpose: constant factors for normalized associated Legendre polynomial P_n^m
!          Ref.: Richmond J.Atm.Ter.Phys. 1974
!
! Method:
!   PmoPmmo(m) = sqrt(1+1/2m)
!   R_n^m      = sqrt[ (n^2-m^2)/(4n^2-1) ]
!
! Author: A. Maute Nov 2003  am 11/18/03
!-----------------------------------------------------------------     

      implicit none                

!-----------------------------------------------------------------------
! local variables
!-----------------------------------------------------------------------
      integer  :: mp, m, n
      real :: xms, xns, den

      do mp = 1, mmp            ! m+1 = 1,mm+1                                     
	m = mp - 1                                               
	xms = m*m                                                
	if( mp /= 1 ) then
           pmopmmo(m) = sqrt( 1. + .5/M )
	end if
	do n = m,nm      ! n = m,N                                     
	  xns    = n*n                                       
	  den    = max(4.*xns - 1.,1.)
	  r(n,m) = sqrt( (xns  - xms)/den )
	end do                 
      end do 

      end subroutine prep_pnm                                                                         

      subroutine index_quiet
!-----------------------------------------------------------------
! Purpose: set up index for factors f_m(mlt),f_l(UT),f_-k(d) to
!    describe the electric potential Phi for the empirical model   
!
! Method:
!    Phi = sum_k sum_l sum_m sum_n [ A_klmn * P_n^m *f_m(mlt)*f_l(UT)*f_-k(d)]
!    - since the electric potential is symmetric about the equator
!      n+m odd terms are set zero resp. not used
!    - in the summation for calculation Phi the index have the following
!      range n=1,12 and m=-n,n, k=0,2 l=-2,2
!
! Author: A. Maute Nov 2003  am 11/18/03
!----------------------------------------------------------------       

      implicit none

!----------------------------------------------------------------      
!	... local variables
!----------------------------------------------------------------                                                                   
      integer :: i, j, k, l, n, m

      i = 0 	! initialize
      j = 1 
      do k = 2,0,-1
        do l = -2,2
          if( k == 2 .and. abs(l) == 2 ) then
             cycle
          end if
          do n = 1,12
            do m = -18,18 
              if( abs(m) <= n ) then		    !  |m| < n
                if( (((n-m)/2)*2) == (n-m) ) then   ! only n+m even
             	  if( n-abs(m) <= 9 ) then	    ! n-|m| <= 9 why?
             	    kf(i) = 2-k
             	    lf(i) = l
             	    nf(i) = n
             	    mf(i) = m
             	    jf(i) = j
             	    i	  = i + 1	 ! counter
                  end if
                end if
              end if
            end do ! m
          end do ! n
        end do ! l
      end do ! k

      imax = i - 1  
      if(imax /= ni ) then    ! check if imax == ni 
!       write(iulog,'(a19,i5,a18,i5)') 'index_quiet: imax= ',imax,  
!    &     ' not equal to ni =',ni 
        stop
      end if							
!     if(debug) write(iulog,*) 'imax=',imax

      end subroutine index_quiet                                                           

      subroutine read_acoef (efield_lflux_file, efield_hflux_file)
!----------------------------------------------------------------     
! Purpose:  
!    1. read in coefficients A_klmn^lf for solar cycle minimum and
!      A_klmn^hf for maximum 
!    2. adjust S_a (f107d) such that if S_a<80 or S_a > 220 it has reasonable numbers
!      S_aM = [atan{(S_a-65)^2/90^2}-a90]/[a180-a90]
!      a90  = atan [(90-65)/90]^2
!      a180 = atan [(180-65)/90]^2
!    3. inter/extrapolation of the coefficient to the actual flux which is
!      given by the user
!      A_klmn = S_aM [A_klmn^hf-A_klmn^lf]/90. + 2*A_klmn^lf-A_klmn^hf
!
! Method:
!
! Author: A. Maute Nov 2003  am 11/19/03
!---------------------------------------------------------------

!     use ioFileMod,     only : getfil
!     use units,         only : getunit, freeunit

      character(len=*), intent(in) :: efield_lflux_file
      character(len=*), intent(in) :: efield_hflux_file

      integer  :: i,ios,unit,istat
!     character (len=13):: locfn
      character (len=256):: locfn

!------------------------------------------------------------------    
!  get coefficients file for solar minimum: 
!-----------------------------------------------------------------                                                                   
!     unit     = getunit()
      unit     = 11
!     call getfil( efield_lflux_file, locfn, 0 )
      locfn=efield_lflux_file

!------------------------------------------------------------------    
! open datafile with coefficients A_klnm
!------------------------------------------------------------------     
!     write(iulog,*) 'read_acoef: open file ',trim(locfn),
!    &' unit ',unit
      open(unit=unit,file=trim(locfn), 
     &     status = 'old',iostat = ios)
!     if(ios.gt.0) then
!     write(iulog,*) 
!    &'read_acoef: error in opening coeff_lf file',
!    &' unit ',unit
!       call endrun
!     end if

!----------------------------------------------------------------------------                                                                   
! read datafile with coefficients A_klnm
!--------------------------------------------------------------------   
!     write(iulog,*) 'read_acoef: read file ',trim(locfn),' unit ',unit
      read(unit,*,iostat = ios) a_lf
!     if(ios.gt.0) then
!     write(iulog,*) 
!    &'read_acoef: error in reading coeff_lf file',' unit ',unit
!       call endrun
!     end if

!--------------------------------------------------------------------  
! close & free unit      
!--------------------------------------------------------------------  
      close(unit)
!     call freeunit(unit)
!     write(iulog,*) 'read_acoef: free unit ',unit

!--------------------------------------------------------------------  
!  get coefficients file for solar maximum: 
!--------------------------------------------------------------------
!     unit     = getunit()
      unit     = 10
!     call getfil( efield_hflux_file, locfn, 0 )
      locfn= efield_hflux_file

!-------------------------------------------------------------------
! open datafile with coefficients A_klnm
!------------------------------------------------------------------
!     write(iulog,*) 'read_acoef: open file ',trim(locfn),' unit ',unit
      open(unit=unit,file=trim(locfn), 
     &     status = 'old',iostat = ios)
!     if(ios.gt.0) then
!      write(iulog,*) 
!    &'read_acoef: error in opening coeff_hf file',' unit ',unit
!       call endrun
!     end if

!-----------------------------------------------------------------
! read datafile with coefficients A_klnm
!----------------------------------------------------------------
!     write(iulog,*) 'read_acoef: read file ',trim(locfn)
      read(unit,*,iostat = ios) a_hf
!     if(ios.gt.0) then
!      write(iulog,*) 
!    &'read_acoef: error in reading coeff_hf file',' unit ',unit
!       call endrun
!     end if

!---------------------------------------------------------------
! close & free unit      
!-------------------------------------------------------------- 
      close(unit)
!     call freeunit(unit)
!     write(iulog,*) 'read_acoef: free unit ',unit

      end subroutine read_acoef

      subroutine adj_S_a
!------------------------------------------------------------------
! adjust S_a -> S_aM   eqn.8-11 Scherliess draft
!------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------
! local variables
!------------------------------------------------------------------
      integer  :: i
      real :: x2, y2, a90, a180, S_aM

      x2 = 90.*90.
      y2 = (90. - 65.)
      y2 = y2*y2
      a90  = atan2(y2,x2)
      y2 = (180. - 65.)
      y2 = y2*y2
      a180 = atan2(y2,x2)
!     y2 = (S_a-65.)
      y2 = (f107d - 65.)
      y2 = y2*y2
      S_aM = (atan2(y2,x2) - a90)/(a180 - a90) 
      S_aM = 90.*(1. + S_aM)
!     if(debug) write(iulog,*) 'f107d=',f107d,' S_aM =',S_aM
!     if(debug) write(iulog,*) 'By=',by

!-----------------------------------------------------------------
! inter/extrapolate to S_a (f107d)
!----------------------------------------------------------------
      do i = 0,ni                       ! eqn.8 Scherliess draft
        a_klnm(i) = S_aM*(a_hf(i)-a_lf(i))/90.+
     &2.*a_lf(i)- a_hf(i)
! for testing like in original code
!        a_klnm(i)=S_a*(a_hf(i)-a_lf(i))/90.+2.*a_lf(i)-a_hf(i)
!        a_klnm(i)=f107d*(a_hf(i)-a_lf(i))/90.+2.*a_lf(i)-a_hf(i)
      end do

      end subroutine adj_S_a

      subroutine constants
!---------------------------------------------------------------
! Purpose: set up constant values (e.g. magnetic grid, convertion
!      constants etc)
!
! Method:
!
! Author: A. Maute Nov 2003  am 11/19/03
!--------------------------------------------------------------------

!-------------------------------------------------------------------
! local variables
!--------------------------------------------------------------------
      integer  :: i,j
      real :: fac,lat

      rtd     = 180./pi 	        ! radians -> deg
      dtr     = pi/180.	        ! deg -> radians
      sqr2    = sqrt(2.e0)
      hr2rd   = pi/12.	        ! pi/12 hrs
      dy2rd   = 2.*pi/dy2yr          ! 2*pi/365.24  average year
      deg2mlt = 24./360.          ! convert degrees to MLT hours
      mlt2deg = 360./24.          ! for mlt to mlon       

!-------------------------------------------------------------------
! Set grid deltas:
!-------------------------------------------------------------------
      dlatm = 180./nmlat
      dlonm = 360./nmlon

!-------------------------------------------------------------------
! Set magnetic latitude array 
!-------------------------------------------------------------------
      do j = 0,nmlat
        ylatm(j) = j*dlatm
        lat = (ylatm(j) - 90.)*dtr
	fac = cos(lat)    ! sinIm = 2*sin(lam_m)/sqrt[4-3*cos^2(lam_m)]
	fac = 4. - 3.*fac*fac
	fac = 2./sqrt( fac )
	sinIm_mag(j) = fac*sin( lat )
      end do 

!------------------------------------------------------------------
! Set magnetic longitude array
!------------------------------------------------------------------
      do i = 0,nmlon
        ylonm(i) = i*dlonm
      end do ! i=1,nmlonp1

!-----------------------------------------------------------------
! find boundary index for weimer
!------------------------------------------------------------------
      do j = 0,nmlat
        nmlat_wei = j
        if( bnd_wei <= ylatm(j) ) then
           exit
        end if
      end do 

!-------------------------------------------------------------------
! find latitudinal shift
!-------------------------------------------------------------------
      do j = 0,nmlat
        ilat_sft = j
        if( lat_sft <= ylatm(j) ) then
           exit
        end if
      end do 

!------------------------------------------------------------------
! find index for linear interpolation of ed2 at mag.equator 
! use 12 deg - same as in TIEGCM      
!------------------------------------------------------------------
      do j = 0,nmlat
        lat = ylatm(j) - 90.
        if( lat <= -12. ) then
	  jmin = j
        else if( lat > 12. ) then
	  jmax = j
	  exit
       end if
      end do

      end subroutine constants

      subroutine prep_fk
!-------------------------------------------------------------------
! Purpose: set up constants factors for f_-k(day) used for empirical model
!     to calculate the electric potential
!
! Method:
!
! Author: A. Maute Nov 2003  am 11/19/03
!-------------------------------------------------------------------

      ft(1,0) = .75*sqrt( 6.e0 )/pi			
      ft(1,1) = 2.e0*ft(1,0)					      
      ft(1,2) = 1.e0						      
      ft(2,0) = ft(1,0) 					      
      ft(2,1) = -ft(1,1)					      
      ft(2,2) = 1.e0						      
      ft(3,0) = ft(2,1) 					      
      ft(3,1) = 0.						      
      ft(3,2) = 1.e0							   

      end subroutine prep_fk

      subroutine set_fkflfs( fk, fl, fs )
!------------------------------------------------------------------
! Purpose:  set f_-k(day) depending on seasonal flag used for empirical model
!     to calculate the electric potential
!
! Method:
!
! Author: A. Maute Nov 2003  am 11/20/03
!-----------------------------------------------------------------

!-----------------------------------------------------------------
!	... dummy arguments
!-----------------------------------------------------------------
      real, intent(out) ::  
     &	fk(0:2),  	                ! f_-k(day) 
     &	fl(-2:2), 	                ! f_l(ut)  
     &	fs(2)		                ! f_s(f10.7) 
!------------------------------------------------------------------
! local variables
!-------------------------------------------------------------------
      integer  :: lp
      real :: ang
      real :: lon_ut

!------------------------------------------------------------------
! f_-k(day) 
! use factors for iseasav == 0 - Scherliess had iseasav as an input parameter
!------------------------------------------------------------------
      lp = iseasav
      if( iseasav == 0 ) then
        ang   = (day + 9.)*dy2rd
        fk(0) = sqr2*cos( 2.*ang )
        fk(1) = sqr2*cos( ang )
        fk(2) = 1.
      else if( iseasav >= 1 .and. iseasav <= 3 ) then
        fk(0) = ft(lp,0)
        fk(1) = ft(lp,1)
        fk(2) = ft(lp,2)
      else if( iseasav == 4 ) then
        fk(0) =0.
        fk(1) =0.
        fk(2) =1.
      end if

!-----------------------------------------------------------------
! f_l(ut) 
!-----------------------------------------------------------------
      lon_ut = 15.*ut        ! 15.*mlt - xmlon + 69. 
      call ff( lon_ut, 2, fl )                                                 
      if( iutav ) then  	! UT-averaging
     
	ang   = fl(0)
        fl(:) = 0.
        fl(0) = ang
	
      end if

!-----------------------------------------------------------------
! f_s(f10.7)  only fs(1) used  	
!-----------------------------------------------------------------
      fs(1) = 1.
!     fs(2) = S_a			  
      fs(2) = f107d			  

      end subroutine set_fkflfs

      subroutine efield_mid( mlat, mlon, pot )
!------------------------------------------------------------------
! Purpose: calculate the electric potential for low and 
!      midlatitudes from an empirical model (Scherliess 1999)
!
! Method:
!
! Author: A. Maute Nov 2003  am 11/20/03
!-------------------------------------------------------------------

!------------------------------------------------------------------
!	... dummy arguments
!-------------------------------------------------------------------
      real, intent(in)  :: mlat, mlon
      real, intent(out) :: pot               ! electric potential (V)

!-------------------------------------------------------------------
! local variables
!-------------------------------------------------------------------
      integer  :: i, mp, np, nn
      real :: mod_mlat, ct, x
      real :: fk(0:2)      	    ! f_-k(day) 
      real :: fl(-2:2)          ! f_l(ut)  
      real :: fs(2)	            ! f_s(f10.7) 
      real :: f(-18:18)
      real :: p(0:nm,0:mm)      ! P_n^m	 spherical harmonics

      pot = 0. ! initialize                                        

      mod_mlat = mlat
      if( abs(mlat) <= 0.5 ) then
         mod_mlat = 0.5                     ! avoid geomag.equator
      end if

!------------------------------------------------------------------
! set f_-k, f_l, f_s depending on seasonal flag
!------------------------------------------------------------------
      call set_fkflfs( fk, fl, fs ) 
      
!------------------------------------------------------------------
! spherical harmonics 
!------------------------------------------------------------------
      ct = cos( (90. - mod_mlat)*dtr )  ! magnetic colatitude 
      call pnm( ct, p )	                   ! calculate P_n^m
      call ff( mlon, 18, f )               ! calculate f_m (phi) why 18 if N=12                              

      do i = 0,imax  
        mp  = mf(i)                                                      
        np  = nf(i)
        nn  = abs(mp)                      !   P_n^m = P_n^-m  
        x   = a_klnm(i)* fl(lf(i)) * fk(kf(i)) * fs(jf(i))
	pot = pot + x*f(mp)*p(np,nn) 
      end do 
      
      end subroutine efield_mid                                              

      subroutine prep_weimer
!-----------------------------------------------------------------
! Purpose:  for Weimer model calculate IMF angle, IMF magnitude
!  tilt of earth
!
! Method: using functions and subroutines from Weimer Model 1996
!     output:  angle, &  ! IMF angle
!     	       bt,    &  ! IMF magnitude
!     	       tilt      ! tilt of earth
!
! Author: A. Maute Nov 2003  am 11/20/03
!-----------------------------------------------------------------

!-----------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------
      real ::  
     &  angle,  ! IMF angle
     &  bt,    ! IMF magnitude
     &  tilt       ! tilt of earth

!-----------------------------------------------------------------
! function declarations
!-----------------------------------------------------------------
      real, external :: get_tilt	 ! in wei96.f

      if( by == 0. .and. bz == 0.) then
         angle = 0.
      else
         angle = atan2( by,bz )
      end if
      
      angle = angle*rtd
      call adjust( angle )
      bt = sqrt( by*by + bz*bz )
!-------------------------------------------------------------------
! use month and day of month - calculated with average no.of days per month
! as in Weimer
!-------------------------------------------------------------------
!     if(debug) write(iulog,*) 'prep_weimer: day->day of month',
!    &iday,imo,iday_m,ut
      tilt = get_tilt( iyear, imo, iday_m, ut )

!      if(debug) then
!       write(iulog,"(/,'efield prep_weimer:')")
!       write(iulog,*)  '  Bz   =',bz
!       write(iulog,*)  '  By   =',by
!       write(iulog,*)  '  Bt   =',bt
!       write(iulog,*)  '  angle=',angle
!       write(iulog,*)  '  VSW  =',v_sw
!       write(iulog,*)  '  tilt =',tilt
!      end if

      call SetModel( angle, bt, tilt, v_sw )

      end subroutine prep_weimer

      subroutine pot_latsmo( pot, idlat )  ! pots == pot_highlats
!--------------------------------------------------------------------
! Purpose: smoothing in latitude of  potential
!
! Method: weighted smoothing in latitude 
! assume regular grid spacing
!
! Author: A. Maute Nov 2003  am 11/20/03
!-------------------------------------------------------------------

!-------------------------------------------------------------------
!	... dummy arguments
!-------------------------------------------------------------------
      integer, intent(in)     :: idlat
      real, intent(inout) :: pot(0:nmlon,0:nmlat)

!-------------------------------------------------------------------
! local variables
!------------------------------------------------------------------
      integer  :: ilon, ilat, id
      real :: wgt, del
      real :: w(-idlat:idlat)
!     real :: pot_smo(0:nmlat) ! temp array for smooth. potential
      real :: pot_smo(0:nmlon,0:nmlat_wei) ! temp array for smooth. potential

!------------------------------------------------------------------
! weighting factors (regular grid spacing) 
!------------------------------------------------------------------
      wgt = 0. 
      do id = -idlat,idlat
	del   = abs(id)*dlatm	! delta lat_m
	w(id) = 1./(del + 1.)
        wgt   = wgt + w(id)
      end do
      wgt = 1./wgt

!     do ilon = 0,nmlon
!        do ilat = idlat,nmlat_wei-idlat
!       do ilat = idlat,nmlat-idlat
!         pot_smo(ilat) = 0.
!         do id = -idlat,idlat	!  org. was degree now grid points
!           pot_smo(ilat) = pot_smo(ilat) + w(id)*pot(ilon,ilat+id)
!           write(iulog,"('pot_latsmo: ilon=',i3,' ilat=',i3,' id=',i3,' pot(ilon,ilat+id)=',e12.4)") ilon,ilat,id,pot(ilon,ilat+id)
!         end do
!         pot_smo(ilat)       = pot_smo(ilat)*wgt
!         pot_smo(nmlat-ilat) = pot_smo(ilat)
!       end do
!      pot(ilon,idlat:nmlat-idlat) =  &        ! copy back into pot
!         pot_smo(idlat:nmlat-idlat)
!        pot(ilon,idlat:nmlat_wei-idlat)       = pot_smo(idlat:nmlat_wei-idlat)
!       pot(ilon,nmlat-nmlat_wei+idlat:nmlat) = pot_smo(nmlat-nmlat_wei+idlat:nmlat)
!        pot(ilon,nmlat-nmlat_wei+idlat:nmlat-idlat) = pot_smo(nmlat-nmlat_wei+idlat:nmlat-idlat)
!     end do

!$omp parallel do private(ilat)
      do ilat = idlat,nmlat_wei-idlat
         pot_smo(:,ilat) = matmul( pot(:,ilat-idlat:ilat+idlat),w )*wgt
      end do

      do ilat = idlat,nmlat_wei-idlat
         pot(:,ilat)       = pot_smo(:,ilat)
         pot(:,nmlat-ilat) = pot_smo(:,ilat)
      end do

      end subroutine pot_latsmo

      subroutine pot_latsmo2( pot, idlat ) 
!------------------------------------------------------------------
! Purpose:  smoothing in latitude of  potential
!
! Method: weighted smoothing in latitude 
!         assume regular grid spacing
!
! Author: A. Maute Nov 2003  am 11/20/03
!------------------------------------------------------------------

!------------------------------------------------------------------
!	... dummy arguments
!------------------------------------------------------------------
      integer, intent(in)     :: idlat
      real, intent(inout) :: pot(0:nmlon,0:nmlat)

!------------------------------------------------------------------
! local variables
!------------------------------------------------------------------
      integer  :: ilon, ilat, id
      real :: wgt, del
      real :: w(-idlat:idlat)
!     real :: pot_smo(0:nmlat) ! temp array for smooth. potential
      real :: pot_smo(0:nmlon,0:nmlath) ! temp array for smooth. potential

!-------------------------------------------------------------------
! weighting factors (regular grid spacing)  
!-------------------------------------------------------------------
      wgt = 0.
      do id = -idlat,idlat
	del   = abs(id)*dlatm	! delta lat_m
	w(id) = 1./(del + 1.)
        wgt   = wgt + w(id)
      end do
      wgt = 1./wgt

!     do ilon = 0,nmlon
!       do ilat = idlat,nmlath-idlat  ! ilat = 5:175
!         pot_smo(ilat) = 0.
!         do id = -idlat,idlat	!  org. was degree now grid points
!           pot_smo(ilat) = pot_smo(ilat) + w(id)*pot(ilon,ilat+id)
!         end do
!         pot_smo(ilat) = pot_smo(ilat)*wgt
!       end do
!       pot(ilon,idlat:nmlath-idlat) = pot_smo(idlat:nmlath-idlat) ! copy back into pot
!     end do

!$omp parallel do private(ilat)
      do ilat = idlat,nmlath-idlat
         pot_smo(:,ilat) = matmul( pot(:,ilat-idlat:ilat+idlat),w )*wgt
      end do

      do ilat = idlat,nmlath-idlat
         pot(:,ilat) = pot_smo(:,ilat)
      end do

      end subroutine pot_latsmo2

      subroutine pot_lonsmo( pot, idlon ) 
!-------------------------------------------------------------------
! Purpose: smoothing in longitude of potential
!
! Method:  weighted smoothing in longitude
!          assume regular grid spacing
!
! Author: A. Maute Nov 2003  am 11/20/03
!-------------------------------------------------------------------

!-------------------------------------------------------------------
!	... dummy arguments
!-------------------------------------------------------------------
      integer, intent(in)     :: idlon
      real, intent(inout) :: pot(0:nmlon,0:nmlat)

!-------------------------------------------------------------------
! local variables
!-------------------------------------------------------------------
      integer  :: ilon, ilat, id, iabs
      real :: wgt, del
      real :: w(-idlon:idlon)
      real :: pot_smo(0:nmlath) ! temp array for smooth. potential
      real :: tmp(-idlon:nmlon+idlon) ! temp array for smooth. potential

!-------------------------------------------------------------------
! weighting factors (regular grid spacing) 
!-------------------------------------------------------------------
      wgt = 0.
      do id = -idlon,idlon
        del   = abs(id)*dlonm	! delta lon_m
        w(id) = 1./(del + 1.)
        wgt   = wgt + w(id)
      end do
        wgt = 1./wgt

!-------------------------------------------------------------------
! averaging     
!-------------------------------------------------------------------
!     do ilon = 0,nmlon
!       do ilat = 0,nmlath
!         pot_smo(ilat) = 0.
!         do id = -idlon,idlon	                  !  org. was degree now grid points
!           iabs = ilon + id
!           if( iabs > nmlon ) then
!              iabs = iabs - nmlon ! test if wrap around
!           end if
!           if( iabs < 0 ) then
!              iabs = iabs + nmlon ! test if wrap around
!           end if
!           pot_smo(ilat) = pot_smo(ilat) + w(id)*pot(iabs,ilat)
!         end do
!         pot_smo(ilat)  = pot_smo(ilat)*wgt
!         pot(ilon,ilat) = pot_smo(ilat)       ! copy back into pot 
!         pot(ilon,nmlat-ilat) = pot_smo(ilat) ! copy back into pot    
!       end do   
!     end do
!     print*,'www7','pot_efield',pot(149,66),idlon,w

!$omp parallel do private(ilat,ilon,tmp)
      do ilat = 0,nmlath
          tmp(0:nmlon)   = pot(0:nmlon,ilat)
          tmp(-idlon:-1) = pot(nmlon-idlon:nmlon-1,ilat)
          tmp(nmlon+1:nmlon+idlon) = pot(1:idlon,ilat)
          do ilon = 0,nmlon
      pot(ilon,ilat)=dot_product(tmp(ilon-idlon:ilon+idlon),w)*wgt
      pot(ilon,nmlat-ilat) = pot(ilon,ilat)
!         if(ilon.eq.149.and.nmlat-ilat.eq.66) 
!    &print*,'www9',pot(ilon,nmlat-ilat),tmp(ilon-idlon:ilon+idlon),wgt
          end do   
      end do
!     print*,'www8','pot_efield',pot(149,66)
      
      end subroutine pot_lonsmo

      subroutine highlat_getbnd( ihlat_bnd ) 
!------------------------------------------------------------------
! Purpose: calculate the height latitude bounday index ihl_bnd
!
! Method:
! 1. calculate E field from weimar model
!    boundary is set where the total electric field exceeds
!    0.015 V/m (corresp. approx. to 300 m/s)
! 2. moved halfways to 54 deg not necessarily equatorwards as in the
!    original comment from L. Scherliess- or?
!
! Author: A. Maute Nov 2003  am 11/20/03
!-------------------------------------------------------------------

!-------------------------------------------------------------------
!	... dummy arguments
!-------------------------------------------------------------------
      integer, intent(out) :: ihlat_bnd(0:nmlon)

!------------------------------------------------------------------
! local variables
!------------------------------------------------------------------
      integer  :: ilon, ilat, ilat_sft_rvs
      real :: mlat, mlt, es, ez, e_tot

      ilat_sft_rvs = nmlath - ilat_sft          ! pole =0, equ=90
!$omp parallel do private(ilat,ilon,mlt,mlat,es,ez,e_tot)
      do ilon = 0,nmlon                         ! long.
	ihlat_bnd(ilon) = 0
        mlt  = ylonm(ilon)*deg2mlt              ! mag.local time ?
        do ilat = nmlat_wei+1,0,-1              ! lat. loop moving torwards pole
	  mlat = 90. - ylatm(ilat)           ! mag. latitude pole = 90 equator = 0
          call gecmp( mlat, mlt, es, ez )	! get electric field
          e_tot = sqrt( es**2 + ez**2 )
          if( abs(e_tot) >= ef_max ) then                        ! e-filed > limit -> boundary
            ihlat_bnd(ilon) = ilat - (ilat - ilat_sft_rvs)/2     ! shift boundary to lat_sft (54deg)
            exit
          end if
        end do
      end do     

!     write(iulog,"('highlat_getbnd: ihlat_bnd=',/,(12i6))") ihlat_bnd

      end subroutine highlat_getbnd

      subroutine bnd_sinus( ihlat_bnd, itrans_width )  
!------------------------------------------------------------------
! Purpose: 
!   1. adjust high latitude boundary (ihlat_bnd) sinusoidally
!   2. width of transition zone from midlatitude potential to high latitude
!      potential (itrans_width)
!
! Method:
! 1.adjust boundary sinusoidally
!   max. wave number to be represented nmax_sin
!   RHS(mi) = Sum_phi Sum_(mi=-nmax_sin)^_(mi=nmax_sin) f_mi(phi)*hlat_bnd(phi) 
!   U(mi,mk)   = Sum_phi Sum_(mi=-nmax_sin)^_(mi=nmax_sin) f_mi(phi) *
!                Sum_(mk=-nmax_sin)^_(mk=nmax_sin) f_mk(phi)
!   single values decomposition of U
!   solving U*LSG = RHS 
!   calculating hlat_bnd:
!   hlat_bnd = Sum_(mi=-nmax_sin)^_(mi=nmax_sin) f_mi(phi)*LSG(mi)
!
! 2. width of transition zone from midlatitude potential to high latitude
!    potential
!    trans_width(phi)=8.-2.*cos(phi) 
!
! Author: A. Maute Nov 2003  am 11/20/03
!------------------------------------------------------------------

!     use sv_decomp, only : svdcmp, svbksb
     
!----------------------------------------------------------------------------                                                                   
!	... dummy arguments
!----------------------------------------------------------------------------                                                                   
      integer, intent(inout) :: ihlat_bnd(0:nmlon)    ! loaction of boundary
      integer, intent(out)   :: itrans_width(0:nmlon) ! width of transition zone

!-----------------------------------------------------------------
! local variables
!-----------------------------------------------------------------
      integer, parameter :: nmax_a = 2*nmax_sin+1 ! absolute array length
      integer, parameter :: ishf   = nmax_sin+1
      integer  :: ilon, i, i1, j, bnd
      real :: sum, mlon
      real :: rhs(nmax_a)
      real :: lsg(nmax_a)
      real :: u(nmax_a,nmax_a)
      real :: v(nmax_a,nmax_a)
      real :: w(nmax_a,nmax_a)
      real :: f(-nmax_sin:nmax_sin,0:nmlon)

!------------------------------------------------------------------
!    Sinusoidal Boundary calculation
!------------------------------------------------------------------
      rhs(:) = 0.
      lsg(:) = 0.
      u(:,:) = 0.
      v(:,:) = 0.
      w(:,:) = 0.

      do ilon = 0,nmlon                  ! long.
        bnd  = nmlath - ihlat_bnd(ilon) ! switch from pole=0 to pole =90
        call ff( ylonm(ilon), nmax_sin, f(-nmax_sin,ilon) )
        do i = -nmax_sin,nmax_sin
	  i1 = i + ishf
          rhs(i1) = rhs(i1) + f(i,ilon) * bnd
!	  write(iulog,*) 'rhs ',ilon,i1,bnd,f(i,ilon),rhs(i+ishf)
          do j = -nmax_sin,nmax_sin 
            u(i1,j+ishf) = u(i1,j+ishf) + f(i,ilon)*f(j,ilon)
!	    write(iulog,*) 'u ',ilon,i1,j+ishf,u(i+ishf,j+ishf)
          end do
        end do
      end do

!     if (debug) write(iulog,*) ' Single Value Decomposition'
      call svdcmp( u, nmax_a, nmax_a, nmax_a, nmax_a, w, v )

!     if (debug) write(iulog,*) ' Solving'
      call svbksb( u, w, v, nmax_a, nmax_a, nmax_a, nmax_a, rhs, lsg )
!      
      do ilon = 0,nmlon  ! long.
!       sum = 0.
	sum = dot_product( lsg(-nmax_sin+ishf:nmax_sin+ishf),
     &f(-nmax_sin:nmax_sin,ilon) )
!       do i = -nmax_sin,nmax_sin
!         sum = sum + lsg(i+ishf)*f(i,ilon)  
!       end do
        ihlat_bnd(ilon)    = nmlath - int( sum + .5 )                                ! closest point
        itrans_width(ilon) = 
     &int( 8. - 2.*cos( ylonm(ilon)*dtr ) + .5 )/dlatm  ! 6 to 10 deg.
      end do      
!     write(iulog,"('bnd_sinus: ihlat_bnd=',/,(12i6))") ihlat_bnd
!     write(iulog,"('bnd_sinus: itrans_width=',/,(12i6))") itrans_width

      end subroutine bnd_sinus

      subroutine highlat_adjust( pot_highlats, pot_highlat, 
     &pot_midlat, ihlat_bnd )
!------------------------------------------------------------------
! Purpose: Adjust mid/low latitude electric potential and high latitude
!          potential such that there are continous across the mid to high 
!          latitude boundary
!
! Method:
! 1. integrate Phi_low/mid(phi,bnd) along the boundary mid to high latitude
! 2. integrate Phi_high(phi,bnd) along the boundary mid to high latitude
! 3. adjust Phi_high by delta =
!    Int_phi Phi_high(phi,bnd) d phi/360. - Int_phi Phi_low/mid(phi,bnd) d phi/360.
!
! Author: A. Maute Nov 2003  am 11/21/03
!------------------------------------------------------------------

!------------------------------------------------------------------
!	... dummy arguments
!------------------------------------------------------------------
      integer, intent(in)     :: ihlat_bnd(0:nmlon) ! boundary mid to high latitude
      real, intent(in)    :: pot_midlat(0:nmlon,0:nmlat) ! low/mid latitude potentail
      real, intent(inout) :: pot_highlat(0:nmlon,0:nmlat)! high_lat potential
      real, intent(inout) :: pot_highlats(0:nmlon,0:nmlat)! high_lat potential! smoothed high_lat potential

!------------------------------------------------------------------
! local:     
!------------------------------------------------------------------
      integer  :: bnd, ilon, ilat, ilatS, ibnd60, ibnd_hl
      real :: pot60, pot_hl, del

!-------------------------------------------------------------------
! 1. integrate Phi_low/mid(phi,bnd) along the boundary mid to high latitude
! 2. integrate Phi_high(phi,bnd) along the boundary mid to high latitude
!-------------------------------------------------------------------
      pot60  = 0.
      pot_hl = 0.
      do ilon = 1,nmlon  ! long.           ! bnd -> eq to pole 0:90
    	ibnd60  = nmlat - ihlat_bnd(ilon)   ! 0:180 pole to pole
    	ibnd_hl = ihlat_bnd(ilon)         ! colatitude
        pot60   = pot60 + pot_midlat(ilon,ibnd60)
        pot_hl  = pot_hl + pot_highlats(ilon,ibnd_hl)
      end do
      pot60  = pot60/(nmlon)
      pot_hl = pot_hl/(nmlon)
!     print*,'www300',pot60,pot_hl,nmlat_wei,nmlon
      
!     if (debug) write(iulog,*) 'Mid-Latitude Boundary Potential =',
!    &pot60
!     if (debug) write(iulog,*) 'High-Latitude Boundary Potential=',
!    &pot_hl

!-------------------------------------------------------------------
! 3. adjust Phi_high by delta =
!    Int_phi Phi_high(phi,bnd) d phi/360. - Int_phi Phi_low/mid(phi,bnd) d phi/360.
!-------------------------------------------------------------------
      del = pot_hl - pot60

!$omp parallel do private(ilat,ilon,ilats)
      do ilat = 0,nmlat_wei      ! colatitude
        ilats = nmlat - ilat
        do ilon = 0,nmlon
	  pot_highlat(ilon,ilat)   = pot_highlat(ilon,ilat)   - del
	  pot_highlat(ilon,ilats)  = pot_highlat(ilon,ilats)  - del
	  pot_highlats(ilon,ilat)  = pot_highlats(ilon,ilat)  - del
	  pot_highlats(ilon,ilats) = pot_highlats(ilon,ilats) - del
        end do
      end do

      end subroutine highlat_adjust

      subroutine interp_poten( pot_highlats, pot_highlat, pot_midlat, 
     &ihlat_bnd, itrans_width ) 
!-------------------------------------------------------------------
! Purpose: construct a smooth global electric potential field 
!
! Method: construct one global potential field
! 1. low/mid latitude: |lam| < bnd-trans_width
!   Phi(phi,lam) = Phi_low(phi,lam)
! 2. high latitude: |lam| > bnd+trans_width
!   Phi(phi,lam) = Phi_hl(phi,lam)
! 3. transition zone: bnd-trans_width <= lam <= bnd+trans_width 
! a. interpolate between high and low/midlatitude potential
!   Phi*(phi,lam) = 1/15*[ 5/(2*trans_width) * {Phi_low(phi,bnd-trans_width)*
!   [-lam+bnd+trans_width] + Phi_hl(phi,bnd+trans_width)*
!   [lam-bnd+trans_width]} + 10/(2*trans_width) {Phi_low(phi,lam)*
!   [-lam+bnd+trans_width] + Phi_hl(phi,lam)*
!   [lam-bnd+trans_width]}]
! b.  Interpolate between just calculated Potential and the high latitude
!    potential in a 3 degree zone poleward of the boundary:
!    bnd+trans_width < lam <= bnd+trans_width+ 3 deg 
!   Phi(phi,lam) = 1/3 { [3-(lam-bnd-trans_width)]* Phi*(phi,lam) +
!   [lam-bnd-trans_width)]* Phi_hl*(phi,lam) }
!
! Author: A. Maute Nov 2003  am 11/21/03      
!------------------------------------------------------------------

!------------------------------------------------------------------
!	... dummy arguments
!------------------------------------------------------------------
      integer, intent(in)  :: ihlat_bnd(0:nmlon)
      integer, intent(in)  :: itrans_width(0:nmlon)
      real, intent(in) :: pot_highlats(0:nmlon,0:nmlat)
      real, intent(in) :: pot_highlat(0:nmlon,0:nmlat)
      real, intent(in) :: pot_midlat(0:nmlon,0:nmlat)

!-------------------------------------------------------------------
! local variables
!-------------------------------------------------------------------
      real, parameter :: fac = 1./3.
      integer  :: ilon, ilat
      integer  :: ibnd, tw, hb1, hb2, lat_ind
      integer  :: j1, j2
      real :: a, b, lat, b1, b2
      real :: wrk1, wrk2

!$omp parallel do private(ilat,ilon,ibnd,tw)
      do ilon = 0,nmlon
        ibnd = ihlat_bnd(ilon)     ! high latitude boundary index
	tw   = itrans_width(ilon)  ! width of transition zone (index)
!-------------------------------------------------------------------
! 1. low/mid latitude: |lam| < bnd-trans_width
!   Phi(phi,lam) = Phi_low(phi,lam)
!-------------------------------------------------------------------
        do ilat = 0,nmlath-(ibnd+tw+1)
          potent(ilon,nmlath+ilat) = pot_midlat(ilon,nmlath+ilat)
          potent(ilon,nmlath-ilat) = pot_midlat(ilon,nmlath+ilat)
        end do
!------------------------------------------------------------------
! 2. high latitude: |lam| > bnd+trans_width
!   Phi(phi,lam) = Phi_hl(phi,lam)
!------------------------------------------------------------------
        do ilat = 0,ibnd-tw-1
          potent(ilon,ilat)       = pot_highlats(ilon,nmlat-ilat)
          potent(ilon,nmlat-ilat) = pot_highlats(ilon,nmlat-ilat)
        end do
      end do
!------------------------------------------------------------------
! 3. transition zone: bnd-trans_width <= lam <= bnd+trans_width 
!------------------------------------------------------------------
! a. interpolate between high and low/midlatitude potential
! update only southern hemisphere (northern hemisphere is copied
! after smoothing)
!------------------------------------------------------------------
!!$omp parallel do private(ilat,ilon,ibnd,tw,a,b,b1,b2,hb1,hb2,
!    &lat_ind,j1,j2,wrk1,wrk2)
      do ilon = 0,nmlon
        ibnd = ihlat_bnd(ilon)          ! high latitude boundary index
	tw   = itrans_width(ilon)       ! width of transition zone (index)
        a    = 1./(2.*tw)
	b1   = (nmlath - ibnd + tw)*a
	b2   = (nmlath - ibnd - tw)*a
	hb1  = nmlath - (ibnd + tw)
	j1   = nmlath - hb1
	hb2  = nmlath - (ibnd - tw)
	j2   = nmlath - hb2
	wrk1 = pot_midlat(ilon,j1)
	wrk2 = pot_highlats(ilon,j2)
!        write(iulog,*) 'pot_all ',ilon,hb1,hb2,nmlath -ibnd,tw
	do ilat = ibnd-tw,ibnd+tw
	  lat_ind = nmlath - ilat
          potent(ilon,ilat) =  
     &    fac*((wrk1 + 2.*pot_midlat(ilon,ilat))*(b1 - a*lat_ind)  
     &	  + (wrk2 + 2.*pot_highlats(ilon,ilat))*(a*lat_ind - b2))
          potent(ilon,nmlat-ilat) = potent(ilon,ilat)
        end do
!------------------------------------------------------------------
! b.  Interpolate between just calculated Potential and the high latitude
!    potential in a 3 degree zone poleward of the boundary
!------------------------------------------------------------------
	do ilat = hb2+1,nmlath
	  a = max( 3./dlatm - (ilat - hb2 - 1),0. )
	  b = 3./dlatm - a
          potent(ilon,nmlath-ilat) = (a*potent(ilon,nmlath-ilat)   
     &    + b*pot_highlat(ilon,nmlath-ilat))/3.*dlatm
          potent(ilon,nmlath+ilat) = potent(ilon,nmlath-ilat)
        end do
      end do      

      end subroutine interp_poten

      subroutine DerivPotential
!-----------------------------------------------------------------
! Purpose: calulates the electric field [V/m] from the electric potential
!
! Method:  Richmond [1995] eqn 5.9-5.10
! ed1(:,:) = Ed1 = - 1/[R cos lam_m] d PHI/d phi_m
! ed2(:,:) = Ed2 = 1/R d PHI/d lam_m /sin I_m
! R = R_e + h_r we assume a reference height of 130 km which is also
! used in the TIEGCM code
!
! Author: A. Maute Dec 2003  am 12/16/03
!-----------------------------------------------------------------

      integer  :: i, j, ip1f, ip2f, ip3f
      real :: coslm, r, fac, wrk
      real :: wrk1d(0:nmlon)

      r = r_e + h_r  ! earth radius + reference height [m]
!-----------------------------------------------------------------
! ed2= Ed2 is the equatorward/downward component of the electric field, at all 
! geomagnetic grid points (central differencing)
!-----------------------------------------------------------------
      fac = .5/(dlatm*dtr*r)
!$omp parallel do private(j, i, wrk )
      do j = 1,nmlath-1		! southern hemisphere
! idea
        wrk = fac/sinIm_mag(j)
!       wrk = fac
        do i = 0,nmlon
          ed2(i,j) = (potent(i,j+1) - potent(i,j-1))*wrk
        end do
      end do

!$omp parallel do private(j, i, wrk )
      do j = nmlath+1,nmlat-1	! northern hemisphere
        wrk = fac/sinIm_mag(j)
        do i = 0,nmlon
          ed2(i,j) = (potent(i,j+1) - potent(i,j-1))*wrk
        end do
      end do

!-----------------------------------------------------------------------
! Interpolate of ed2 between between -12 <= lam_m <= 12 degrees:
!-----------------------------------------------------------------------
      wrk1d(:) = ed2(:,jmax) - ed2(:,jmin)
      do j = jmin+1,jmax-1
        fac = (ylatm(j) - ylatm(jmin))/(ylatm(jmax) - ylatm(jmin))
        do i = 0,nmlon
	    ed2(i,j) = ed2(i,jmin) + fac*wrk1d(i)
	end do
      end do

!-----------------------------------------------------------------------
! ed1= Ed1 is the zonal component of the electric field, at all 
! geomagnetic grid points (central differencing)
!-----------------------------------------------------------------------
      fac = .5/(dlonm*dtr*r)
!$omp parallel do private(j, i, wrk, coslm )
      do j = 1,nmlat-1
        coslm = ylatm(j) - 90.
        coslm = cos( coslm*dtr )
        wrk = fac/coslm
        do i = 1,nmlon-1
          ed1(i,j) = -(potent(i+1,j) - potent(i-1,j))*wrk
        end do
	i = 0
	ed1(i,j)  = -(potent(i+1,j) - potent(nmlon-1,j))*wrk
	ed1(nmlon,j) = ed1(i,j)
      end do

!-----------------------------------------------------------------------
! Poles:
!-----------------------------------------------------------------------
      do i = 0,nmlon
        ip1f = i + nmlon/4
        if( ip1f > nmlon ) then
           ip1f = ip1f - nmlon
        end if
        ip2f = i + nmlon/2
        if( ip2f > nmlon ) then
           ip2f = ip2f - nmlon
        end if
        ip3f = i + 3*nmlon/4
        if( ip3f > nmlon ) then
           ip3f = ip3f - nmlon
        end if
        ed1(i,0)=.25*(ed1(i,1)-ed1(ip2f,1)+ed2(ip1f,1)-ed2(ip3f,1))
        ed1(i,nmlat) = .25*(ed1(i,nmlat-1) - ed1(ip2f,nmlat-1)  
     &        + ed2(ip1f,nmlat-1) - ed2(ip3f,nmlat-1))
        ed2(i,0)=.25*(ed2(i,1)-ed2(ip2f,1)-ed1(ip1f,1)+ed1(ip3f,1))
        ed2(i,nmlat) = .25*(ed2(i,nmlat-1) - ed2(ip2f,nmlat-1)  
     &        - ed1(ip1f,nmlat-1) + ed1(ip3f,nmlat-1))
      end do

      end subroutine DerivPotential

      end module efield
!      
! Purpose: 
! Subroutines to calculate the electric potentials from the Weimer '96 model of
! the polar cap ionospheric electric potentials.
!
! Method:
!
! To use, first call subroutine ReadCoef once.
! Next, call SetModel with the specified input parameters.
! The function EpotVal(gLAT,gMLT) can then be used repeatively to get the
! electric potential at the desired location in geomagnetic coordinates.
! Subroutines to calculate the electric potentials from the Weimer '96 model of
! the polar cap ionospheric electric potentials.
!
!
! Author: A. Maute Dec 2003  
! This code is protected by copyright and is
! distributed for research or educational use only.
! Commerical use without written permission from Dan Weimer/MRC is prohibited.
!
!*********************** Copyright 1996, Dan Weimer/MRC ***********************
!==================================================================

	FUNCTION EpotVal(gLAT,gMLT)
!
!-----------------------------------------------------------------------
! Return the value of the electric potential in kV at
! corrected geomagnetic coordinates gLAT (degrees) and gMLT (hours).
!
! Must first call ReadCoef and SetModel to set up the model coeficients for
! the desired values of Bt, IMF clock angle, Dipole tilt angle, and SW Vel.
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        use efield, only: Coef =>Coef,ML=>ML,MM=>MM1
        implicit none 
!
!-----------------------------Return Value------------------------------
!
        real EpotVal
!
!-------------------------------Commons---------------------------------
!
!       INTEGER ML,MM
!       REAL Coef(0:1,0:8,0:3),pi
!       COMMON/SetCoef/Coef,pi,ML,MM
        real pi
!
!------------------------------Arguments--------------------------------
!
	REAL gLAT,gMLT
!
!---------------------------Local variables-----------------------------
!
        integer limit,l,m

	Real Theta,Phi,Z,ct,Phim
        real r
	REAL Plm(0:20,0:20)
!
!-----------------------------------------------------------------------
!
        pi=3.141592653
	r=90.-gLAT
	IF(r .LT. 45.)THEN
	  Theta=r*pi/45.
          Phi=gMLT*pi/12.
	  Z=Coef(0,0,0)
	  ct=COS(Theta)
	  CALL Legendre(ct,ML,MM,Plm)
	  DO l=1,ML
	    Z=Z + Coef(0,l,0)*Plm(l,0)
	    IF(l.LT.MM)THEN
	      limit=l
	    ELSE
	      limit=MM
	    ENDIF
	    DO m=1,limit
	      phim=phi*m
              Z=Z + Coef(0,l,m)*Plm(l,m)*COS(phim) +  
     &	   Coef(1,l,m)*Plm(l,m)*SIN(phim)
	    ENDDO
	  ENDDO
	ELSE
	  Z=0.
	ENDIF
	EpotVal=Z
!       print*,'www0',Z,Coef,Plm,ct
	RETURN
	END FUNCTION EpotVal

!================================================================================================

	SUBROUTINE ReadCoef (wei96_file)
!
!-----------------------------------------------------------------------
!
! Read in the data file with the model coefficients
!
!*********************** Copyright 1996, Dan Weimer/MRC ***********************
!
! NCAR addition (Jan 97):  initialize constants used in GECMP
!-----------------------------------------------------------------------
!
!     use shr_kind_mod,  only: r8 => shr_kind_r8
!     use ioFileMod,     only : getfil
!     use units,         only : getunit, freeunit
!     use abortutils,    only : endrun
!     use cam_logfile,   only : iulog
      use efield, only: ALAMN =>ALAMN,ALAMX=>ALAMX,ALAMR=>ALAMR,
     &STPD=>STPD,STP2=>STP2,CSTP=>CSTP,SSTP=>SSTP,
     &Cn=>Cn,MaxL=>MaxL,MaxM=>MaxM,MaxN=>MaxN
      implicit none 
!
!-------------------------------Commons---------------------------------
!
!     real alamn, alamx, alamr, stpd, stp2, cstp, sstp
!     COMMON /CECMP/ ALAMN,ALAMX,ALAMR,STPD,STP2,CSTP,SSTP
!            ALAMN = Absolute min latitude (deg) of model
!            ALAMX = Absolute max latitude (deg) for normal gradient calc.
!            STPD  = Angular dist (deg) of step @ 300km above earth (r=6371km)
!            STP2  = Denominator in gradient calc

!
!------------------------------Arguments--------------------------------
!
      character(len=*), intent(in) :: wei96_file
!
!-----------------------------Parameters------------------------------
!
      real d2r, r2d
      PARAMETER ( D2R =  0.0174532925199432957692369076847 ,  
     &            R2D = 57.2957795130823208767981548147)
!
!---------------------------Local variables-----------------------------
!
      INTEGER udat,unit,ios
      integer ll,mm,k,m,klimit,kk,nn,ii,i,n,ilimit,mlimit,l

      REAL C(0:3)
      real stpr, step

      CHARACTER*15 skip

      INTEGER iulog
!     INTEGER MaxL,MaxM,MaxN,iulog
!     REAL Cn( 0:3 , 0:1 , 0:4 , 0:1 , 0:8 , 0:3 )
!     COMMON /AllCoefs/Cn,MaxL,MaxM,MaxN

      character(len=256) :: locfn
!
!-----------------------------------------------------------------------
      iulog=14
      STEP = 10.
      STPR = STEP/6671.
      STPD = STPR*R2D
      STP2 = 2.*STEP
      CSTP = COS (STPR)
      SSTP = SQRT (1. - CSTP*CSTP)
      ALAMN = 45.
      ALAMX = 90. - STPD
      ALAMR = ALAMN*D2R
!          End NCAR addition
! 
!  get coeff_file  
!     unit= getunit()
      unit= 600
!     print*, 'Weimer: getting file ',trim(wei96_file),
!    &' unit ',unit
!     call getfil( wei96_file, locfn, 0 )
      locfn= wei96_file
!      
!     write(iulog,*) 'Weimer: opening file ',trim(locfn),
!    &' unit ',unit	
!     OPEN(unit=unit,file=trim(locfn),  
      open(unit=unit,file=locfn,status = 'old',iostat = ios)
      if(ios.gt.0) then
       print*, 'Weimer: error in opening wei96.cofcnts',
     &' unit ',unit
!       call endrun
      endif
  900 FORMAT(A15)
c1000 FORMAT(3I8)
 1000 format(3i8)
 2000 FORMAT(3I2)
 3000 FORMAT(2I2,4E15.6)
!     READ(udat,900) skip
!     write(iulog,*) 'Weimer: reading file ',trim(locfn),
!    &' unit ',unit	
!     READ(unit,1000,iostat = ios) MaxL,MaxM,MaxN
      read(unit,1000,iostat = ios) MaxL,MaxM,MaxN
!     print*,'www0',ios,MaxL,MaxM,MaxN
!     if(ios.gt.0) then
!     write(iulog,*) 
!    &'ReadCoef: error in reading wei96.cofcnts file',
!    &' unit ',unit	
!       call endrun
!     endif
      DO l=0,MaxL
        IF(l.LT.MaxM)THEN
          mlimit=l
        ELSE
          mlimit=MaxM
        ENDIF
        DO m=0,mlimit
          IF(m.LT.1)THEN
            klimit=0
          ELSE
            klimit=1
          ENDIF
          DO k=0,klimit
            read(unit,2000,iostat = ios) ll,mm,kk
!           print*,k,ll,mm,kk
!           if(ios.gt.0) then
!     	      write(iulog,*) 
!    &'ReadCoef: error in reading wei96.cofcnts file',' unit ',
!    &unit	
!             call endrun
!           endif
!           IF(ll.NE.l .OR. mm.NE.m .OR. kk.NE.k)THEN
!             WRITE(IULOG,*)'Data File Format Error'
!             CALL ENDRUN
!           ENDIF
            DO n=0,MaxN
              IF(n.LT.1)THEN
        	ilimit=0
              ELSE
        	ilimit=1
              ENDIF
              DO i=0,ilimit
        	READ(unit,3000,iostat = ios) nn,ii,C
!     print*,'www0',nn,ii,C,i,n,k,l,m
!               if(ios.gt.0) then
!     	          write(iulog,*) 'ReadCoef: error in reading',  
!    &                 ' wei96.cofcnts file',' unit ',unit	
!                 call endrun
!               endif
!       	IF(nn.NE.n .OR. ii.NE.i)THEN
!       	  WRITE(IULOG,*)'Data File Format Error'
!       	  CALL ENDRUN
!       	ENDIF
        	Cn(0,i,n,k,l,m)=C(0)
        	Cn(1,i,n,k,l,m)=C(1)
        	Cn(2,i,n,k,l,m)=C(2)
        	Cn(3,i,n,k,l,m)=C(3)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
!      
      close(unit)
!     call freeunit(unit)
!    
      RETURN
      END SUBROUTINE ReadCoef

!================================================================================================

	FUNCTION FSVal(omega,MaxN,FSC)
!
!-----------------------------------------------------------------------
! Evaluate a  Sine/Cosine Fourier series for N terms up to MaxN
! at angle omega, given the coefficients in FSC
!
!*********************** Copyright 1996, Dan Weimer/MRC ***************
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none 
!
!-----------------------------Return Value------------------------------
!
        real FSVal
!
!------------------------------Arguments--------------------------------
!
	INTEGER MaxN
!       REAL omega,FSC(0:1,0:*)
        REAL omega,FSC(0:1,0:4)
!
!---------------------------Local variables-----------------------------
!
	INTEGER n
	REAL Y,theta
!
!-----------------------------------------------------------------------
!
	Y=0.
	DO n=0,MaxN
	  theta=omega*n
	  Y=Y + FSC(0,n)*COS(theta) + FSC(1,n)*SIN(theta)
	ENDDO
	FSVal=Y
!       print*,'www00',Y,FSC
	RETURN
	END FUNCTION FSVal

!================================================================================================

	SUBROUTINE SetModel(angle,Bt,Tilt,SWVel)
!
!-----------------------------------------------------------------------
! Calculate the complete set of spherical harmonic coefficients,
! given an arbitrary IMF angle (degrees from northward toward +Y),
! magnitude Bt (nT), dipole tilt angle (degrees),
! and solar wind velocity (km/sec).
! Returns the Coef in the common block SetCoef.
!
!*********************** Copyright 1996, Dan Weimer/MRC ***********************
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
      use efield, only: Cn=>Cn,MaxL=>MaxL,MaxM=>MaxM,MaxN=>MaxN
     &,Coef=>coef,ML=>ML,MM=>MM1
        implicit none 
!
!-------------------------------Commons---------------------------------
!
!       INTEGER MaxL,MaxM,MaxN
!       REAL Cn( 0:3 , 0:1 , 0:4 , 0:1 , 0:8 , 0:3 )
!       COMMON /AllCoefs/Cn,MaxL,MaxM,MaxN

!       INTEGER ML,MM
!       REAL Coef(0:1,0:8,0:3),pi
!       COMMON/SetCoef/Coef,pi,ML,MM
        real pi
!
!------------------------------Arguments--------------------------------
!
	REAL angle,Bt,Tilt,SWVel
!
!---------------------------Local variables-----------------------------
!
        integer n, k, ilimit, i, klimit, l, m, mlimit
	REAL FSC(0:1,0:4), fsval, omega, sintilt
!
!-----------------------------------------------------------------------
!
	pi=3.141592653
	ML=MaxL
	MM=MaxM
	SinTilt=SIN(Tilt*pi/180.)
!	SinTilt=SIND(Tilt)

	omega=angle*pi/180.

        fsc(1,0) = 0.
	DO l=0,MaxL
	  IF(l.LT.MaxM)THEN
	    mlimit=l
	  ELSE
	    mlimit=MaxM
	  ENDIF
	  DO m=0,mlimit
	    IF(m.LT.1)THEN
	      klimit=0
	    ELSE
	      klimit=1
	    ENDIF
	    DO k=0,klimit
! Retrieve the regression coefficients and evaluate the function
! as a function of Bt,Tilt,and SWVel to get each Fourier coefficient.
	      DO n=0,MaxN
	        IF(n.LT.1)THEN
	          ilimit=0
	        ELSE
	          ilimit=1
	        ENDIF
		DO i=0,ilimit
		  FSC(i,n)=Cn(0,i,n,k,l,m) + Bt*Cn(1,i,n,k,l,m) +  
     &	   SinTilt*Cn(2,i,n,k,l,m) + SWVel*Cn(3,i,n,k,l,m)
		ENDDO
	      ENDDO
! Next evaluate the Fourier series as a function of angle.
      	      Coef(k,l,m)=FSVal(omega,MaxN,FSC)
	    ENDDO
	  ENDDO
	ENDDO
!       print*,'www000',FSC(0,0),Cn,Bt,SinTilt,SWVel
	RETURN
	END SUBROUTINE SetModel

!================================================================================================

	SUBROUTINE LEGENDRE(x,lmax,mmax,Plm)
!
!-----------------------------------------------------------------------
! compute Associate Legendre Function P_l^m(x)
! for all l up to lmax and all m up to mmax.
! returns results in array Plm
! if X is out of range ( abs(x)>1 ) then value is returned as if x=1.
!
!*********************** Copyright 1996, Dan Weimer/MRC ***********************
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
!       use cam_logfile,  only : iulog

        implicit none 
!
!------------------------------Arguments--------------------------------
!
        integer lmax, mmax
	real x, Plm(0:20,0:20)
!
!---------------------------Local variables-----------------------------
!
        integer m, lm2, l, iulog
        real xx, fact
        iulog=14
!
!-----------------------------------------------------------------------
!
	  DO l=0,20
	    DO m=0,20
		Plm(l,m)=0.
	    ENDDO
	  ENDDO
	xx=MIN(x,1.)
	xx=MAX(xx,-1.)
!         IF(lmax .LT. 0 .OR. mmax .LT. 0 .OR. mmax .GT. lmax )THEN
!        write(iulog,*)'Bad arguments to Legendre'
!        RETURN
!        ENDIF
! First calculate all Pl0 for l=0 to l
	Plm(0,0)=1.
	IF(lmax.GT.0)Plm(1,0)=xx
	IF (lmax .GT. 1 )THEN
	  DO L=2,lmax
	    Plm(L,0)=( (2.*L-1)*xx*Plm(L-1,0) - 
     &(L-1)*Plm(L-2,0) )/L
	  ENDDO
	ENDIF
	IF (mmax .EQ. 0 )RETURN
	fact=SQRT( (1.-xx)*(1.+xx) )
	DO M=1,mmax
	  DO L=m,lmax
	    lm2=MAX(L-2,0)
	    Plm(L,M)=Plm(lm2,M) - ( 2*L-1)*fact*Plm(L-1,M-1)
	  ENDDO
	ENDDO
	RETURN
	END SUBROUTINE LEGENDRE

!================================================================================================

!*********************** Copyright 1996, Dan Weimer/MRC ***********************

!CC NCAR MODIFIED (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  The following routines (translib.for) were added to return the dipole tilt. C
!  GET_TILT was initially a procedure (TRANS), here it has been changed into   C
!  a function which returns the dipole tilt.                                   C
! Barbara Emery (emery@ncar.ucar.edu) and William Golesorkhi, HAO/NCAR (3/96)  C
!CC NCAR MODIFIED (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! COORDINATE TRANSFORMATION UTILITIES
!**********************************************************************        
	FUNCTION GET_TILT(YEAR,MONTH,DAY,HOUR)
!
!-----------------------------------------------------------------------
!CC NCAR MODIFIED (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  The following line initially was:                                           C
!       SUBROUTINE TRANS(YEAR,MONTH,DAY,HOUR,IDBUG)                            C
!  It has been changed to return the dipole tilt from this function call.      C
!CC NCAR MODIFIED (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         
!      THIS SUBROUTINE DERIVES THE ROTATION MATRICES AM(I,J,K) FOR 11
!      TRANSFORMATIONS, IDENTIFIED BY K.
!          K=1 TRANSFORMS GSE to GEO
!          K=2     "      GEO to MAG
!          K=3     "      GSE to MAG
!          K=4     "      GSE to GSM
!          K=5     "      GEO to GSM
!          K=6     "      GSM to MAG
!          K=7     "      GSE to GEI
!          K=8     "      GEI to GEO
!          K=9     "      GSM to SM 
!	   K=10    "      GEO to SM 
!	   K=11    "      MAG to SM 
!
!      IF IDBUG IS NOT 0, THEN OUTPUTS DIAGNOSTIC INFORMATION TO
!      FILE UNIT=IDBUG
!
!      The formal names of the coordinate systems are:
!	GSE - Geocentric Solar Ecliptic
!	GEO - Geographic
!	MAG - Geomagnetic
!	GSM - Geocentric Solar Magnetospheric
!	SM  - Solar Magnetic
!	
!      THE ARRAY CX(I) ENCODES VARIOUS ANGLES, STORED IN DEGREES
!      ST(I) AND CT(I) ARE SINES & COSINES.       
!
!      Program author:  D. R. Weimer
!
!      Some of this code has been copied from subroutines which had been
!      obtained from D. Stern, NASA/GSFC.  Other formulas are from "Space 
!      Physics Coordinate Transformations: A User Guide" by M. Hapgood (1991).
!
!      The formulas for the calculation of Greenwich mean sidereal time (GMST)
!      and the sun's location are from "Almanac for Computers 1990",
!      U.S. Naval Observatory.
!
!-----------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        use efield, only:CX=>CX,ST=>ST,CT=>CT,AM=>AM
     &,EPOCH=>EPOCH,TH0=>TH0,PH0=>PH0,DIPOLE=>DIPOLE

        implicit none 
!
!-----------------------------Return Value--------------------------
!
        real get_tilt
!
!-------------------------------Commons---------------------------------
!
!       real cx, st, ct, am
!       COMMON/TRANSDAT/CX(9),ST(6),CT(6),AM(3,3,11)

!       real epoch, th0, ph0, dipole
!       COMMON/MFIELD/EPOCH,TH0,PH0,DIPOLE
!       DATA EPOCH,TH0,PH0,DIPOLE/1980.,11.19,-70.76,.30574/
!
!------------------------------Arguments--------------------------------
!
	INTEGER YEAR, MONTH, DAY
	REAL HOUR
!
!-----------------------------Parameters------------------------------
!
	INTEGER GSEGEO,GEOGSE,GEOMAG,MAGGEO
	INTEGER GSEMAG,MAGGSE,GSEGSM,GSMGSE
	INTEGER GEOGSM,GSMGEO,GSMMAG,MAGGSM
	INTEGER GSEGEI,GEIGSE,GEIGEO,GEOGEI
	INTEGER GSMSM,SMGSM,GEOSM,SMGEO,MAGSM,SMMAG

	PARAMETER (GSEGEO= 1,GEOGSE=-1,GEOMAG= 2,MAGGEO=-2)
	PARAMETER (GSEMAG= 3,MAGGSE=-3,GSEGSM= 4,GSMGSE=-4)
	PARAMETER (GEOGSM= 5,GSMGEO=-5,GSMMAG= 6,MAGGSM=-6)
	PARAMETER (GSEGEI= 7,GEIGSE=-7,GEIGEO= 8,GEOGEI=-8)
	PARAMETER (GSMSM = 9,SMGSM =-9,GEOSM =10,SMGEO=-10)
	PARAMETER (MAGSM =11,SMMAG =-11)
!
!---------------------------Local variables-----------------------------
!
        integer IDBUG
        integer j, k, jd, iyr, i, mjd

        REAL UT, T0, GMSTD, GMSTH, ECLIP, MA, LAMD, SUNLON, pi
        real b32, b33, b3
!
!-------------------------External Functions----------------------------
!
        integer julday_wam
        external julday_wam
!
!-----------------------------------------------------------------------
!
!       EPOCH=1980.
!       TH0=11.19
!       PH0=-70.76
!       DIPOLE=.30574
        pi=3.141592653
!       pi=2.*ASIN(1.)
!CC NCAR MODIFICATION (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! IDBUG=0 to prevent printing data to the screen or writing data to a file.    C
        IDBUG = 0
!CC NCAR MODIFICATION (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF(YEAR.LT.1900)THEN
	  IYR=1900+YEAR
	ELSE
	  IYR=YEAR
	ENDIF
	UT=HOUR
	JD=JULDAY_WAM(MONTH,DAY,IYR)
	MJD=JD-2400001
!       T0=(real(MJD,r8)-51544.5)/36525.0
	T0=(float(MJD)-51544.5)/36525.0
	GMSTD=100.4606184 +36000.770*T0 +3.87933E-4*T0*T0 + 
     &       15.0410686*UT
	CALL ADJUST(GMSTD)
	GMSTH=GMSTD*24./360.
	ECLIP=23.439 - 0.013*T0
        MA=357.528 + 35999.050*T0 + 0.041066678*UT
        CALL ADJUST(MA)
        LAMD=280.460 + 36000.772*T0 + 0.041068642*UT
        CALL ADJUST(LAMD)
        SUNLON=LAMD + (1.915-0.0048*T0)*SIN(MA*pi/180.) + 0.020* 
     &     SIN(2.*MA*pi/180.)
        CALL ADJUST(SUNLON)
!         IF(IDBUG.NE.0)THEN
!         WRITE(IDBUG,*) YEAR,MONTH,DAY,HOUR
!         WRITE(IDBUG,*) 'MJD=',MJD
!         WRITE(IDBUG,*) 'T0=',T0
!         WRITE(IDBUG,*) 'GMSTH=',GMSTH
!         WRITE(IDBUG,*) 'ECLIPTIC OBLIQUITY=',ECLIP
!         WRITE(IDBUG,*) 'MEAN ANOMALY=',MA
!         WRITE(IDBUG,*) 'MEAN LONGITUDE=',LAMD
!         WRITE(IDBUG,*) 'TRUE LONGITUDE=',SUNLON
!         ENDIF

	CX(1)= GMSTD
	CX(2) = ECLIP
	CX(3) = SUNLON
	CX(4) = TH0
	CX(5) = PH0
! Derived later:
!       CX(6) = Dipole tilt angle  
!       CX(7) = Angle between sun and magnetic pole
!       CX(8) = Subsolar point latitude
!       CX(9) = Subsolar point longitude

	DO I=1,5
	  ST(I) = SIN(CX(I)*pi/180.)
	  CT(I) = COS(CX(I)*pi/180.)
	ENDDO
!         
      AM(1,1,GSEGEI) = CT(3)
      AM(1,2,GSEGEI) = -ST(3)
      AM(1,3,GSEGEI) = 0.         
      AM(2,1,GSEGEI) = ST(3)*CT(2)
      AM(2,2,GSEGEI) = CT(3)*CT(2)
      AM(2,3,GSEGEI) = -ST(2)
      AM(3,1,GSEGEI) = ST(3)*ST(2)
      AM(3,2,GSEGEI) = CT(3)*ST(2)
      AM(3,3,GSEGEI) = CT(2)      
!         
      AM(1,1,GEIGEO) = CT(1)      
      AM(1,2,GEIGEO) = ST(1)      
      AM(1,3,GEIGEO) = 0.         
      AM(2,1,GEIGEO) = -ST(1)     
      AM(2,2,GEIGEO) = CT(1)      
      AM(2,3,GEIGEO) = 0.         
      AM(3,1,GEIGEO) = 0.         
      AM(3,2,GEIGEO) = 0.         
      AM(3,3,GEIGEO) = 1.         
!         
      DO I=1,3   
      DO J=1,3   
        AM(I,J,GSEGEO) = AM(I,1,GEIGEO)*AM(1,J,GSEGEI) + 
     &AM(I,2,GEIGEO)*AM(2,J,GSEGEI) +AM(I,3,GEIGEO)*AM(3,J,GSEGEI)
      ENDDO
      ENDDO
!         
      AM(1,1,GEOMAG) = CT(4)*CT(5) 
      AM(1,2,GEOMAG) = CT(4)*ST(5) 
      AM(1,3,GEOMAG) =-ST(4)       
      AM(2,1,GEOMAG) =-ST(5)       
      AM(2,2,GEOMAG) = CT(5)       
      AM(2,3,GEOMAG) = 0.
      AM(3,1,GEOMAG) = ST(4)*CT(5) 
      AM(3,2,GEOMAG) = ST(4)*ST(5) 
      AM(3,3,GEOMAG) = CT(4)       
!         
      DO I=1,3   
      DO J=1,3   
       AM(I,J,GSEMAG) = AM(I,1,GEOMAG)*AM(1,J,GSEGEO) + 
     &AM(I,2,GEOMAG)*AM(2,J,GSEGEO) +AM(I,3,GEOMAG)*AM(3,J,GSEGEO)
      ENDDO
      ENDDO
!         
      B32 = AM(3,2,GSEMAG)         
      B33 = AM(3,3,GSEMAG)         
      B3  = SQRT(B32*B32+B33*B33)       
      IF (B33.LE.0.) B3 = -B3  
!         
      AM(2,2,GSEGSM) = B33/B3      
      AM(3,3,GSEGSM) = AM(2,2,GSEGSM)   
      AM(3,2,GSEGSM) = B32/B3      
      AM(2,3,GSEGSM) =-AM(3,2,GSEGSM)   
      AM(1,1,GSEGSM) = 1.
      AM(1,2,GSEGSM) = 0.
      AM(1,3,GSEGSM) = 0.
      AM(2,1,GSEGSM) = 0.
      AM(3,1,GSEGSM) = 0.
!         
      DO I=1,3   
      DO J=1,3   
        AM(I,J,GEOGSM) = AM(I,1,GSEGSM)*AM(J,1,GSEGEO) + 
     &AM(I,2,GSEGSM)*AM(J,2,GSEGEO) + 
     &AM(I,3,GSEGSM)*AM(J,3,GSEGEO)
      ENDDO
      ENDDO
!         
      DO I=1,3   
      DO J=1,3   
        AM(I,J,GSMMAG) = AM(I,1,GEOMAG)*AM(J,1,GEOGSM) + 
     &AM(I,2,GEOMAG)*AM(J,2,GEOGSM) + 
     &AM(I,3,GEOMAG)*AM(J,3,GEOGSM)
      ENDDO
      ENDDO 
!
	ST(6) = AM(3,1,GSEMAG)        
	CT(6) = SQRT(1.-ST(6)*ST(6))      
	CX(6) = ASIN(ST(6)*pi/180.)  

        AM(1,1,GSMSM) = CT(6)
        AM(1,2,GSMSM) = 0.
        AM(1,3,GSMSM) = -ST(6)
        AM(2,1,GSMSM) = 0.
        AM(2,2,GSMSM) = 1.
        AM(2,3,GSMSM) = 0.
        AM(3,1,GSMSM) = ST(6)
        AM(3,2,GSMSM) = 0.
        AM(3,3,GSMSM) = CT(6)  
!         
      DO I=1,3   
      DO J=1,3   
        AM(I,J,GEOSM) = AM(I,1,GSMSM)*AM(1,J,GEOGSM) +  
     &AM(I,2,GSMSM)*AM(2,J,GEOGSM) +  
     &AM(I,3,GSMSM)*AM(3,J,GEOGSM)
      ENDDO
      ENDDO
!         
      DO I=1,3   
      DO J=1,3   
        AM(I,J,MAGSM) = AM(I,1,GSMSM)*AM(J,1,GSMMAG) +  
     &  AM(I,2,GSMSM)*AM(J,2,GSMMAG) + 
     &AM(I,3,GSMSM)*AM(J,3,GSMMAG)
      ENDDO
      ENDDO
      
!
      CX(7)=ATAN2( AM(2,1,11) , AM(1,1,11) )
      
      CX(7)=CX(7)*180./pi
      CX(8)=ASIN( AM(3,1,1)*pi/180. )
      CX(9)=ATAN2( AM(2,1,1) , AM(1,1,1) )
      CX(9)=CX(9)*180./pi

      IF(IDBUG.NE.0)THEN
!     WRITE(IDBUG,*) 'Dipole tilt angle=',CX(6)
!     WRITE(IDBUG,*) 'Angle between sun and magnetic pole=',
!    &CX(7)
!     WRITE(IDBUG,*) 'Subsolar point latitude=',CX(8)
!     WRITE(IDBUG,*) 'Subsolar point longitude=',CX(9)

        DO K=1,11
!        WRITE(IDBUG,1001) K
         DO I=1,3
!          WRITE(IDBUG,1002) (AM(I,J,K),J=1,3)
         ENDDO
        ENDDO
 1001   FORMAT(' ROTATION MATRIX ',I2)
 1002   FORMAT(3F9.5)
      ENDIF

!CC NCAR MODIFICATION (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  The next line was added to return the dipole tilt from this function call.  C
      GET_TILT = CX(6)
!CC NCAR MODIFICATION (3/96) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      RETURN
      END FUNCTION GET_TILT

!======================================================================

      SUBROUTINE ROTATE (X,Y,Z,I)       
!
!-----------------------------------------------------------------------
!     THIS SUBROUTINE APPLIES TO THE VECTOR (X,Y,Z) THE ITH ROTATION  
!     MATRIX AM(N,M,I) GENERATED BY SUBROUTINE TRANS
!     IF I IS NEGATIVE, THEN THE INVERSE ROTATION IS APPLIED
!-----------------------------------------------------------------------
!
!     use shr_kind_mod, only: r8 => shr_kind_r8
      implicit none 
!
!------------------------------Arguments--------------------------------
!
      integer i
      REAL X,Y,Z
!
!---------------------------Local variables-----------------------------
!
      REAL A(3)
!
!-----------------------------------------------------------------------
!
      A(1)=X
      A(2)=Y
      A(3)=Z
      CALL ROTATEV(A,A,I)
      X=A(1)
      Y=A(2)
      Z=A(3)
    
      RETURN        
      END SUBROUTINE ROTATE

!======================================================================

      SUBROUTINE ROTATEV (A,B,I)       
!         
!-----------------------------------------------------------------------
!     THIS SUBROUTINE APPLIES TO THE VECTOR A(3) THE ITH ROTATION  
!     MATRIX AM(N,M,I) GENERATED BY SUBROUTINE TRANS
!     AND OUTPUTS THE CONVERTED VECTOR B(3), WITH NO CHANGE TO A.
!     IF I IS NEGATIVE, THEN THE INVERSE ROTATION IS APPLIED
!-----------------------------------------------------------------------
!
!     use shr_kind_mod, only: r8 => shr_kind_r8
!     use cam_logfile,  only : iulog
!     use abortutils,   only : endrun
      use efield, only:CX=>CX,ST=>ST,CT=>CT,AM=>AM

      implicit none 
!
!-------------------------------Commons---------------------------------
!
!     real cx, st, ct, am
!     COMMON/TRANSDAT/CX(9),ST(6),CT(6),AM(3,3,11)
!
!------------------------------Arguments--------------------------------
!
      integer i
      REAL A(3),B(3)
!
!---------------------------Local variables-----------------------------
!
      integer id, j, iulog
      real xa, ya, za
      iulog=14
!
!-----------------------------------------------------------------------
!
!     IF(I.EQ.0 .OR. IABS(I).GT.11)THEN
!     WRITE(IULOG,*)'ROTATEV CALLED WITH UNDEFINED TRANSFORMATION'
!     CALL ENDRUN
!     ENDIF

      XA = A(1)
      YA = A(2)
      ZA = A(3)
      IF(I.GT.0)THEN
	ID=I
        DO J=1,3
          B(J) = XA*AM(J,1,ID) + YA*AM(J,2,ID) + ZA*AM(J,3,ID)
        ENDDO
      ELSE
	ID=-I
        DO J=1,3
          B(J) = XA*AM(1,J,ID) + YA*AM(2,J,ID) + ZA*AM(3,J,ID)
        ENDDO
      ENDIF
      RETURN        
      END SUBROUTINE ROTATEV

!================================================================================================

	SUBROUTINE FROMCART(R,LAT,LONG,POS)
!
!-----------------------------------------------------------------------
! CONVERT CARTESIAN COORDINATES POS(3)
! TO SPHERICAL COORDINATES R, LATITUDE, AND LONGITUDE (DEGREES)
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none 
!
!------------------------------Arguments--------------------------------
!
	REAL R, LAT, LONG, POS(3)
!
!---------------------------Local variables-----------------------------
!
        real pi
!
!-----------------------------------------------------------------------
!
!       pi=2.*ASIN(1.)
        pi=3.141592653
	R=SQRT(POS(1)*POS(1) + POS(2)*POS(2) + POS(3)*POS(3))
	IF(R.EQ.0.)THEN
	  LAT=0.
	  LONG=0.
	ELSE
	  LAT=ASIN(POS(3)*pi/180./R)
	  LONG=ATAN2(POS(2),POS(1))
	  LONG=LONG*180./pi
	ENDIF
	RETURN
	END SUBROUTINE FROMCART

!================================================================================================

	SUBROUTINE TOCART(R,LAT,LONG,POS)
!
!-----------------------------------------------------------------------
! CONVERT SPHERICAL COORDINATES R, LATITUDE, AND LONGITUDE (DEGREES)
! TO CARTESIAN COORDINATES POS(3)
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none 
!
!------------------------------Arguments--------------------------------
!
	REAL R, LAT, LONG, POS(3)
!
!---------------------------Local variables-----------------------------
!
        real pi, stc, ctc, sf, cf
!
!-----------------------------------------------------------------------
!
!       pi=2.*ASIN(1.)
        pi=3.141592653
        STC = SIN(LAT*pi/180.)    
        CTC = COS(LAT*pi/180.)    
        SF = SIN(LONG*pi/180.)     
        CF = COS(LONG*pi/180.)     
        POS(1) = R*CTC*CF        
        POS(2) = R*CTC*SF        
        POS(3) = R*STC
	RETURN
	END SUBROUTINE TOCART

!================================================================================================

	SUBROUTINE ADJUST(ANGLE)
!
!-----------------------------------------------------------------------
!	ADJUST AN ANGLE IN DEGREES TO BE IN RANGE OF 0 TO 360.
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none 
!
!------------------------------Arguments--------------------------------
!
        real angle
!
!-----------------------------------------------------------------------
!
 10	CONTINUE
	IF(ANGLE.LT.0.)THEN
	  ANGLE=ANGLE+360.
	  GOTO 10
	ENDIF
 20	CONTINUE
 	IF(ANGLE.GE.360.)THEN
	  ANGLE=ANGLE-360.
	  GOTO 20
	ENDIF
	RETURN
	END SUBROUTINE ADJUST

!================================================================================================

      INTEGER FUNCTION JULDAY_WAM(MM,ID,IYYY)
!
!-----------------------------------------------------------------------
!
!     use shr_kind_mod, only: r8 => shr_kind_r8
      implicit none 
!
!------------------------------Arguments--------------------------------
!
      integer mm, id, iyyy
!
!-----------------------------Parameters------------------------------
!
      integer igreg
      PARAMETER (IGREG=15+31*(10+12*1582))
!
!---------------------------Local variables-----------------------------
!
      integer ja, jm, jy
!
!-----------------------------------------------------------------------
!
!!!compiler warning      IF (IYYY.EQ.0) PAUSE 'There is no Year Zero.'
      IF (IYYY.EQ.0) STOP 'There is no Year Zero.'
      IF (IYYY.LT.0) IYYY=IYYY+1
      IF (MM.GT.2) THEN
        JY=IYYY
        JM=MM+1
      ELSE
        JY=IYYY-1
        JM=MM+13
      ENDIF
      JULDAY_WAM=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
      IF (ID+31*(MM+12*IYYY).GE.IGREG) THEN
        JA=INT(0.01*JY)
        JULDAY_WAM=JULDAY_WAM+2-JA+INT(0.25*JA)
      ENDIF
      RETURN
      END FUNCTION JULDAY_WAM

!================================================================================================

	FUNCTION MLT(MagLong)
!
!-----------------------------------------------------------------------
! given magnetic longitude in degrees, return Magnetic Local Time
! assuming that TRANS has been called with the date & time to calculate
! the rotation matrices.
!
! btf 11/06/03:
! Call sub adjust instead of referencing it as a function
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
      use efield, only: CX=>CX,ST=>ST,CT=>CT,AM=>AM
        implicit none 
!
!-----------------------------Return Value------------------------------
!
        real mlt
!
!-------------------------------Commons---------------------------------
!
!       real cx, st, ct, am
!       COMMON/TRANSDAT/CX(9),ST(6),CT(6),AM(3,3,11)

!
!------------------------------Arguments--------------------------------
!
	REAL MagLong
!
!---------------------------Local variables-----------------------------
!
	REAL angle, rotangle
!
!-----------------------------------------------------------------------
!
	RotAngle=CX(7)
!       MLT=ADJUST(Maglong+RotAngle+180.)/15.
        angle = Maglong+RotAngle+180.
        call adjust(angle)
        mlt = angle/15.
	RETURN
	END FUNCTION MLT

!================================================================================================

	FUNCTION MagLong(MLT)
!
!-----------------------------------------------------------------------
! return magnetic longitude in degrees, given Magnetic Local Time
! assuming that TRANS has been called with the date & time to calculate
! the rotation matrices.
!
! btf 11/06/03:
! Call sub adjust instead of referencing it as a function
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
      use efield, only:CX=>CX,ST=>ST,CT=>CT,AM=>AM
        implicit none 
!
!-----------------------------Return Value------------------------------
!
        real MagLong
!
!-------------------------------Commons---------------------------------
!
!       real cx, st, ct, am
!       COMMON/TRANSDAT/CX(9),ST(6),CT(6),AM(3,3,11)
!
!------------------------------Arguments--------------------------------
!
	REAL MLT
!
!---------------------------Local variables-----------------------------
!
	REAL angle, rotangle
!
!-----------------------------------------------------------------------
!
	RotAngle=CX(7)
	angle=MLT*15.-RotAngle-180.
!       MagLong=ADJUST(angle)
        call adjust(angle)
        MagLong = angle
	RETURN
	END FUNCTION MagLong

!================================================================================================

	SUBROUTINE SunLoc(SunLat,SunLong)
!
!-----------------------------------------------------------------------
! Return latitude and longitude of sub-solar point.
! Assumes that TRANS has previously been called with the
! date & time to calculate the rotation matrices.
!-----------------------------------------------------------------------
!
!       use shr_kind_mod, only: r8 => shr_kind_r8
      use efield, only:CX=>CX,ST=>ST,CT=>CT,AM=>AM
        implicit none 
!
!-------------------------------Commons---------------------------------
!
!       real cx, st, ct, am
!       COMMON/TRANSDAT/CX(9),ST(6),CT(6),AM(3,3,11)
!
!------------------------------Arguments--------------------------------
!
	Real SunLat,SunLong
!
!-----------------------------------------------------------------------
!
	SunLong=CX(9)
	SunLat=CX(8)
	RETURN
	END SUBROUTINE SunLoc

!================================================================================================

      SUBROUTINE GECMP (AMLA,RMLT,ET,EP)
!
!-----------------------------------------------------------------------
!          Get Electric field components for the Weimer electrostatic
!          potential model.  Before use, first load coefficients (CALL
!          READCOEF) and initialize model conditions (CALL SETMODEL).
!
!          INPUTS:
!            AMLA = Absolute value of magnetic latitude (deg)
!            RMLT = Magnetic local time (hours).
!          RETURNS:
!            ET = Etheta (magnetic equatorward*) E field component (V/m)
!            EP = Ephi   (magnetic eastward)     E field component (V/m)
!
!          * ET direction is along the magnetic meridian away from the
!            current hemisphere; i.e., when ET > 0, the direction is
!              southward when RMLA > 0
!              northward when RMLA < 0
!
!          NCAR addition (Jan 97).  R.Barnes
!-----------------------------------------------------------------------
!
!     use shr_kind_mod, only: r8 => shr_kind_r8
      use efield, only: ALAMN =>ALAMN,ALAMX=>ALAMX,ALAMR=>ALAMR,
     &STPD=>STPD,STP2=>STP2,CSTP=>CSTP,SSTP=>SSTP
      implicit none 
!
!-------------------------------Commons---------------------------------
!
!          CECMP contains constants initialized in READCOEF
!     real alamn, alamx, alamr, stpd, stp2, cstp, sstp
!     COMMON /CECMP/ ALAMN,ALAMX,ALAMR,STPD,STP2,CSTP,SSTP
!
!------------------------------Arguments--------------------------------
!
      real amla, rmlt, et, ep
!
!-----------------------------Parameters------------------------------
!
      real d2r, r2d
      PARAMETER ( D2R =  0.0174532925199432957692369076847 , 
     &           R2D = 57.2957795130823208767981548147)
!
!---------------------------Local variables-----------------------------
!
      real p1, p2
      real xmlt, xmlt1, kpol, dphi, amla1
!
!-------------------------External Functions----------------------------
!
      real epotval
      external epotval
!
!-----------------------------------------------------------------------
!
      ET = -99999.
      EP = -99999.
      IF (AMLA .LT. 0.) GO TO 100

!          Calculate -(latitude gradient) by stepping 10 km along the
!          meridian in each direction (flipping coordinates when going
!          over pole to keep lat <= 90).
      KPOL  = 0
      XMLT  = RMLT
   10 XMLT1 = XMLT
      AMLA1 = AMLA + STPD
      IF (AMLA1 .GT. 90.) THEN
	AMLA1 = 180. - AMLA1
	XMLT1 = XMLT1 + 12.
      ENDIF
      P1 = EPOTVAL (AMLA1    ,XMLT1)
      P2 = EPOTVAL (AMLA-STPD,XMLT )
      IF (KPOL .EQ. 1) GO TO 20
      ET = (P1 - P2) / STP2

!          Calculate -(lon gradient).  For most latitudes, step along a
!          great circle.  However, limit minimum latitude to the model
!          minimum (distorting the path onto a latitude line).  Also,
!          avoid a divide by zero at the pole avoid by using Art's trick
!          where Ephi(90,lon) = Etheta(90,lon+90)
      IF (AMLA .LT. ALAMX) THEN
	AMLA1 = MAX (ASIN(SIN(AMLA*D2R)*CSTP) , ALAMR)
	DPHI  = ASIN (SSTP/SIN(AMLA1))*R2D
	AMLA1 = AMLA1*R2D
	P1 = EPOTVAL (AMLA1,XMLT+DPHI)
	P2 = EPOTVAL (AMLA1,XMLT-DPHI)
      ELSE
	AMLA = 90.
	XMLT = XMLT + 6.
	KPOL = 1
	GO TO 10
      ENDIF
   20 EP = (P2 - P1) / STP2
      IF (KPOL .EQ. 1) EP = -EP

!          Below model minimum lat, the potential is value at min lat
      IF (AMLA .LT. ALAMN) THEN
	ET = 0.
	EP = EP * COS(ALAMR)/COS(AMLA*D2R)
      ENDIF

  100 RETURN
      END SUBROUTINE GECMP

!=====================================================================
      subroutine svdcmp( a, m, n, mp, np, w, v )
!------------------------------------------------------------------------- 
! purpose: singular value decomposition
!
! method:
! given a matrix a(1:m,1:n), with physical dimensions mp by np,
! this routine computes its singular value decomposition,
! the matrix u replaces a on output. the
! diagonal matrix of singular values w is output as a vector
! w(1:n). the matrix v (not the transpose v^t) is output as
! v(1:n,1:n).
!
! author: a. maute dec 2003      
! (* copyright (c) 1985 numerical recipes software -- svdcmp *!
! from numerical recipes 1986 pp. 60 or can be find on web-sites
!------------------------------------------------------------------------- 
      implicit none
      integer, parameter :: nmax = 1600
!------------------------------------------------------------------------- 
!	... dummy arguments
!------------------------------------------------------------------------- 
      integer, intent(in)     :: m
      integer, intent(in)     :: n
      integer, intent(in)     :: mp
      integer, intent(in)     :: np
      real, intent(inout) :: a(mp,np)
      real, intent(out)   :: v(np,np)
      real, intent(out)   :: w(np)

!------------------------------------------------------------------------- 
!	... local variables
!------------------------------------------------------------------------- 
      integer  :: i, its, j, k, l, nm
      real :: anorm
      real  :: c
      real  :: f
      real  :: g
      real  :: h
      real  :: s
      real  :: scale
      real  :: x, y, z
      real  :: rv1(nmax)
      logical  :: cnd1
      logical  :: cnd2

      g     = 0.0
      scale = 0.0
      anorm = 0.0

      do i = 1,n  !loop1
        l = i + 1
        rv1(i) = scale*g
        g     = 0.0
        s     = 0.0
        scale = 0.0
        if( i <= m ) then
          do k = i,m
            scale = scale + abs(a(k,i))
          end do
          if( scale /= 0.0 ) then
            do k = i,m
              a(k,i) = a(k,i)/scale
              s = s + a(k,i)*a(k,i)
            end do
            f = a(i,i)
            g = -sign(sqrt(s),f)
            h = f*g - s
            a(i,i) = f - g
            if( i /= n ) then
              do j = l,n
                s = 0.0
                do k = i,m
                  s = s + a(k,i)*a(k,j)
                end do
                f = s/h
                do k = i,m
                  a(k,j) = a(k,j) + f*a(k,i)
                end do
              end do
            end if
            do k = i,m
              a(k,i) = scale*a(k,i)
            end do
          endif
        endif
        w(i) = scale *g
        g     = 0.0
        s     = 0.0
        scale = 0.0
        if( i <= m .and. i /= n ) then
          do k = l,n
            scale = scale + abs(a(i,k))
          end do
          if( scale /= 0.0 ) then
            do k = l,n
              a(i,k) = a(i,k)/scale
              s      = s + a(i,k)*a(i,k)
            end do
            f = a(i,l)
            g = -sign(sqrt(s),f)
            h = f*g - s
            a(i,l) = f - g
            do k = l,n
              rv1(k) = a(i,k)/h
            end do
            if( i /= m ) then
              do j = l,m
                s = 0.0
                do k = l,n
                  s = s + a(j,k)*a(i,k)
                end do
                do k = l,n
                  a(j,k) = a(j,k) + s*rv1(k)
                end do
              end do
            end if
            do k = l,n
              a(i,k) = scale*a(i,k)
            end do
          end if
        end if
        anorm = max( anorm,(abs(w(i)) + abs(rv1(i))) )
      enddo !loop1

      do i = n,1,-1
        if( i < n ) then
          if( g /= 0.0 ) then
            do j = l,n
              v(j,i) = (a(i,j)/a(i,l))/g
            end do
            do j = l,n
              s = 0.0
              do k = l,n
                s = s + a(i,k)*v(k,j)
              end do
              do k = l,n
                v(k,j) = v(k,j) + s*v(k,i)
              end do
            end do
          end if
          do j = l,n
            v(i,j) = 0.0
            v(j,i) = 0.0
          end do
        end if
        v(i,i) = 1.0
        g = rv1(i)
        l = i
      end do

      do i = n,1,-1
        l = i + 1
        g = w(i)
        if( i < n ) then
          do j = l,n
            a(i,j) = 0.0
          end do
        end if
        if( g /= 0.0  ) then
          g = 1./g
          if( i /= n ) then
            do j = l,n
              s = 0.0
              do k = l,m
                s = s + a(k,i)*a(k,j)
              end do
              f = (s/a(i,i))*g
              do k = i,m
                a(k,j) = a(k,j) + f*a(k,i)
              end do
            end do
          end if
          do j = i,m
            a(j,i) = a(j,i)*g
          end do
        else
          do j = i,m
            a(j,i) = 0.0
          end do
        end if
        a(i,i) = a(i,i) + 1.0
      end do

      do k = n,1,-1
        do its = 1,30 !loop2
          do l = k,1,-1
            nm = l - 1
            cnd1 = abs( rv1(l) ) + anorm == anorm
            if( cnd1 ) then
              cnd2 = .false.
              exit
            end if
            cnd2 = abs( w(nm) ) + anorm == anorm
            if( cnd2 ) then
              cnd1 = .true.
              exit
            else if( l == 1 ) then
              cnd1 = .true.
              cnd2 = .true.
            end if
          end do

          if( cnd2 ) then
            c = 0.0
            s = 1.0
            do i = l,k
              f = s*rv1(i)
              if( (abs(f) + anorm) /= anorm ) then
                g = w(i)
                h = sqrt(f*f + g*g)
                w(i) = h
                h = 1.0/h
                c = (g*h)
                s = -(f*h)
                do j = 1,m
                  y = a(j,nm)
                  z = a(j,i)
                  a(j,nm) = (y*c) + (z*s)
                  a(j,i) = -(y*s) + (z*c)
                end do
              end if
            end do
          end if

          if( cnd1 ) then
            z = w(k)
            if( l == k ) then
              if( z < 0.0 ) then
                w(k) = -z
                do j = 1,n
                  v(j,k) = -v(j,k)
                end do
              end if
!             exit loop2
              go to 20
            end if
          end if

          x = w(l)
          nm = k - 1
          y = w(nm)
          g = rv1(nm)
          h = rv1(k)
          f = ((y - z)*(y + z) + (g - h)*(g + h))/(2.0*h*y)
          g = sqrt( f*f + 1.0 )
          f = ((x - z)*(x + z) + h*((y/(f + sign(g,f))) - h))/x
          c = 1.0
          s = 1.0
          do j = l,nm
            i = j + 1
            g = rv1(i)
            y = w(i)
            h = s*g
            g = c*g
            z = sqrt( f*f + h*h )
            rv1(j) = z
            c = f/z
            s = h/z
            f = (x*c)+(g*s)
            g = -(x*s)+(g*c)
            h = y*s
            y = y*c
            do nm = 1,n
              x = v(nm,j)
              z = v(nm,i)
              v(nm,j) = (x*c)+(z*s)
              v(nm,i) = -(x*s)+(z*c)
            end do
            z = sqrt( f*f + h*h )
            w(j) = z
            if( z /= 0.0 ) then
              z = 1.0/z
              c = f*z
              s = h*z
            end if
            f = (c*g)+(s*y)
            x = -(s*g)+(c*y)
            do nm = 1,m
              y = a(nm,j)
              z = a(nm,i)
              a(nm,j) = (y*c)+(z*s)
              a(nm,i) = -(y*s)+(z*c)
            end do
          end do
          rv1(l) = 0.0
          rv1(k) = f
          w(k)   = x
        end do  !loop2
   20 continue
      end do
      
      end subroutine svdcmp

!-------------------------------------------------------------------------      
! purpose: solves a*x = b
!
! method:     
! solves a*x = b for a vector x, where a is specified by the arrays
! u,w,v as returned by svdcmp. m and n
! are the logical dimensions of a, and will be equal for square matrices.
! mp and np are the physical dimensions of a. b(1:m) is the input right-hand 
! side. x(1:n) is the output solution vector. no input quantities are 
! destroyed, so the routine may be called sequentially with different b
!
! author:  a. maute dec 2002   
! (* copyright (c) 1985 numerical recipes software -- svbksb *!
! from numerical recipes 1986 pp. 57 or can be find on web-sites
!-------------------------------------------------------------------------      

      subroutine svbksb( u, w, v, m, n, mp, np, b, x )
!------------------------------------------------------------------------- 
!	... dummy arguments
!------------------------------------------------------------------------- 
      implicit none
      integer, parameter :: nmax = 1600
      integer, intent(in)   :: m
      integer, intent(in)   :: n
      integer, intent(in)   :: mp
      integer, intent(in)   :: np
      real , intent(in)  :: u(mp,np)
      real , intent(in)  :: w(np)
      real , intent(in)  :: v(np,np)
      real , intent(in)  :: b(mp)
      real , intent(out) :: x(np)

!------------------------------------------------------------------------- 
!	... local variables
!------------------------------------------------------------------------- 
      integer  :: i, j, jj
      real :: s
      real :: tmp(nmax)

      do j = 1,n
        s = 0. 
        if( w(j) /= 0. ) then
          do i = 1,m
            s = s + u(i,j)*b(i)
          end do
          s = s/w(j)
        endif
        tmp(j) = s
      end do

      do j = 1,n
        s = 0. 
        do jj = 1,n
          s = s + v(j,jj)*tmp(jj)
        end do
        x(j) = s
      end do

      end subroutine svbksb
