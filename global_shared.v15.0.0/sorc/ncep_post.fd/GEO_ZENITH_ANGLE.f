	SUBROUTINE GEO_ZENITH_ANGLE(i,j,RLAT,RLON,SLAT,SLON,ZA)
!
!************************************************************************
!*				        	                          *
!*	Module Name:    GOES_ZA	        		                  *
!*					                                  *
!*	Language:	Fortran-90	   Library:	                  *
!*	Version.Rev:	1.0 16 JUL 87	Programmer: KLEESPIES    	  *
!*			1.1 16 Mar 89		    Kleespies             *
!*				Fix bug in ZA computation.                *
!*		        Sep. 2010       Chuang: convert to Fortran 90     *
!                                       and incoporate to unified post    *
!*	Calling Seq:	CALL GOES_ZA(RLAT,RLON,SLAT,SLON,ZA)		  *
!*	Description:	Computes local zenith angle from a point on the	  *
!*		earth's surface to geosynchronous altitude.  This uses	  *
!*		spherical geometry, so it isn't fancy.  A ZA gt 90 deg	  *
!*		means the earth station is over the horizon from the	  *
!*		satellite.        	                                  *
!*	Input Args:	R*4  RLAT  latitude of earth point +- 90 deg      *
!*			R*4  RLON  longitude of earth point deg + E       *
!*			R*4  SLAT  latitude of subsatellite point	  *
!*			R*4  SLON  longitude of subsatellite point	  *
!*									  *
!*	Output Args:	R*4  ZA	   zenith angle to geostationary 	  *
!*				   altitude.				  *
!*									  *
!*	Common Blks:	none						  *
!*									  *
!*	Include:	none						  *
!*									  *
!*	Externals:	none						  *
!*									  *
!*	Data Files:	none						  *
!*									  *
!*	Restrictions:	Uses spherical geometry, so is not totally 	  *
!*		accurate.  Makes no check for quadrant changes, so may	  *
!*		have problems around the prime meridian and the date 	  *
!*		line.  Also the poles.					  *
!*									  *
!*	Error Codes:	none						  *
!*									  *
!*	Error Messages:	none						  *
!*									  *
!************************************************************************

!        use params_mod, only: DTR,ERAD,PI,RTD
	IMPLICIT NONE
	REAL*8 DTR/0.017453293 /   ! degrees to radians
	REAL*8 RE / 6370.0/        ! radius of earth
	REAL*8 PI /3.1415926/	   ! pi
	REAL*8 RTD /57.29577951 /  ! radians to degrees
!	REAL*8 RZS /1786245696.0/  ! (geosynchronous height)**2
	REAL*8 RES /40589641.0  /  ! (earth radius)**2
	REAL*8 C1  /1826835337.0/  ! RES + RZS
	REAL*8 C2  /538527888.0 /  ! 2*RE*RES
	
	REAL RZS /1786245696.0/  ! (geosynchronous height)**2 for GOES

!	REAL*8 A,B,C,COSD,COSE,E,P,PP
        INTEGER, INTENT(IN):: I,J
        REAL,INTENT(IN):: SLON,RLAT,RLON,SLAT
	REAL,INTENT(OUT):: ZA
!        REAL C1,C2,RES,RE
	REAL A,B,C,COSD,COSE,E,P,PP
	
!	RE=ERAD/1.0E3
!	RES=RE**2.0
!	C1=RES+RZS
!	C2=2.0*RE*RES/1000.

	A = (90.0 - RLAT) * DTR  !  colatitude of point
	B = (90.0 - SLAT) * DTR  !  colatitude of satellite
	
	IF(RLON>180.)THEN
	 C = ABS(RLON- 360. - SLON) * DTR
	ELSE 
	 C = ABS(RLON - SLON) * DTR 
	END IF 
	
	COSD = COS(A)*COS(B) + SIN(A)*SIN(B)*COS(C) ! great circle arc

	PP = C1 - C2*COSD
	P = SQRT(PP)

	COSE = (PP + RES - RZS) / (2.*RE*P)
	E = ACOS(COSE)

	ZA = PI - E
	ZA = ZA * RTD
	if(abs(RLON-360.-SLON)<1. .and. abs(RLAT-30.)<1.)print*,'Debug GEO_ZENITH',  &
	RLON,RLAT,RES,c1,c2,a,b,c,cosd,pp,p,cose,e,ZA

	RETURN
	END

