      subroutine set_soilveg(me,nlunit)
      use namelist_soilveg
      implicit none

      integer, intent(in) :: nlunit
      integer me
cmy begin locals
      integer i
      REAL WLTSMC1,REFSMC1
C ----------------------------------------------------------------------
C SET TWO SOIL MOISTURE WILT, SOIL MOISTURE REFERENCE PARAMETERS
C ----------------------------------------------------------------------
      REAL SMLOW
      REAL SMLOW_DATA
      DATA SMLOW_DATA /0.5/

      REAL SMHIGH
      REAL SMHIGH_DATA
C     changed in 2.6 from 3 to 6 on June 2nd 2003
C      DATA SMHIGH_DATA /3.0/
      DATA SMHIGH_DATA /6.0/
      NAMELIST /SOIL_VEG/ SLOPE_DATA, RSMTBL, RGLTBL, HSTBL, SNUPX,
     &  BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW,
     &  WLTSMC, QTZ, LPARAM, ZBOT_DATA, SALP_DATA, CFACTR_DATA,
     &  CMCMAX_DATA, SBETA_DATA, RSMAX_DATA, TOPT_DATA,
     &  REFDK_DATA, FRZK_DATA, BARE, DEFINED_VEG, DEFINED_SOIL,
     &  DEFINED_SLOPE, FXEXP_DATA, NROOT_DATA, REFKDT_DATA, Z0_DATA,
     &  CZIL_DATA, LAI_DATA, CSOIL_DATA

cmy end locals
      SLOPE_DATA =(/0.1,  0.6, 1.0, 0.35, 0.55, 0.8,
     &  	       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/)
c     RSMTBL =(/150.0, 100.0, 125.0, 150.0, 100.0, 70.0,
c    &  	    40.0, 300.0, 400.0, 150.0, 400.0, 40.0,
c    &  	   150.0,   0.0,   0.0,   0.0,   0.0,  0.0,
c    &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
c    &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/)
Chelin new table
!     RSMTBL =(/500.0, 300.0, 300.0, 300.0, 300.0, 100.0,
!     RSMTBL =(/500.0, 175.0, 175.0, 300.0, 300.0, 70.0,
      RSMTBL =(/300.0, 175.0, 175.0, 300.0, 300.0, 70.0,
     &              45.0, 225.0, 225.0, 225.0, 400.0, 45.0,
     &  	   150.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/)
c-----------------------------
      RGLTBL =(/30.0,  30.0,  30.0,  30.0,  30.0,  65.0,
     &  	  100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
     &  	  100.0,   0.0,   0.0,   0.0,	0.0,   0.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0/)
      HSTBL =(/41.69, 54.53, 51.93, 47.35,  47.35, 54.53,
     &  	  36.35, 42.00, 42.00, 42.00,  42.00, 36.35,
     &  	  42.00,  0.00,  0.00,  0.00,	0.00,  0.00,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00/)
C     changed for version 2.6 on June 2nd 2003
C      DATA SNUPX  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,
C     &  	   0.040, 0.040, 0.040, 0.040, 0.025, 0.040,
C     &  	   0.025, 0.000, 0.000, 0.000, 0.000, 0.000,
      SNUPX  =(/0.040, 0.040, 0.040, 0.040, 0.040, 0.040,
     *             0.020, 0.020, 0.020, 0.020, 0.013, 0.020,
     *             0.013, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
      BB	 =(/4.26,  8.72, 11.55, 4.74, 10.73,  8.17,
     &  	  6.77,  5.25,  4.26, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00/)
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DRYSMC=(/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      F11  =(/-0.999, -1.116, -2.137, -0.572, -3.201, -1.302,
     &  	 -1.519, -0.329, -0.999,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/)
      MAXSMC=(/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
     &  	  0.404, 0.439, 0.421, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
C
C ----------------------------------------------------------------------
C THE FOLLOWING 5 PARAMETERS ARE DERIVED LATER IN REDPRM.F FROM THE SOIL
C DATA, AND ARE JUST GIVEN HERE FOR REFERENCE AND TO FORCE STATIC
C STORAGE ALLOCATION. -DAG LOHMANN, FEB. 2001
C ----------------------------------------------------------------------
C      DATA REFSMC/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,
C     &  	  0.315, 0.329, 0.283, 0.000, 0.000, 0.000,
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      REFSMC=(/0.248, 0.368, 0.398, 0.281, 0.321, 0.361,
     &            0.293, 0.301, 0.248, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
C ----------------------------------------------------------------------
C SOIL TEXTURE-RELATED ARRAYS.
C ----------------------------------------------------------------------
      SATPSI=(/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,
     &  	  0.14, 0.36, 0.04, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/)
      SATDK =(/1.41E-5, 0.20E-5, 0.10E-5, 0.52E-5, 0.72E-5,
     &  	  0.25E-5, 0.45E-5, 0.34E-5, 1.41E-5, 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/)
      QTZ   =(/0.82, 0.10, 0.25, 0.60, 0.52, 0.35,
     &  	  0.60, 0.40, 0.82, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/)

C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      WLTSMC=(/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      SATDW =(/5.71E-6, 2.33E-5, 1.16E-5, 7.95E-6, 1.90E-5,
     &  	  1.14E-5, 1.06E-5, 1.46E-5, 5.71E-6, 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/)

! PT 5/18/2015 - changed to FALSE to match atm_namelist setting
! PT LPARAM is not used anywhere
!      LPARAM =.TRUE.
      LPARAM =.FALSE.

C     changed for version 2.5.2
C      DATA ZBOT_DATA /-3.0/
      ZBOT_DATA =-8.0
C     changed for version 2.6 June 2nd 2003
C      DATA SALP_DATA /2.6/
      SALP_DATA =4.0
      CFACTR_DATA =0.5
      CMCMAX_DATA =0.5E-3
      SBETA_DATA =-2.0
      RSMAX_DATA =5000.0
      TOPT_DATA =298.0
      REFDK_DATA =2.0E-6
      FRZK_DATA =0.15
      BARE =11

C ----------------------------------------------------------------------
C NUMBER OF DEFINED SOIL-, VEG-, AND SLOPETYPS USED.
C ----------------------------------------------------------------------

      DEFINED_VEG=13
      DEFINED_SOIL=9
      DEFINED_SLOPE=9

      FXEXP_DATA =2.0
      NROOT_DATA =(/4,4,4,4,4,4,3,3,3,2,3,3,2,0,0,
     &  	       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      REFKDT_DATA =3.0
C ----------------------------------------------------------------------
C VEGETATION CLASS-RELATED ARRAYS
C ----------------------------------------------------------------------
      Z0_DATA =(/2.653, 0.826, 0.563, 1.089, 0.854, 0.856,
     &  	    0.035, 0.238, 0.065, 0.076, 0.011, 0.035,
     &  	    0.011, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
C   changed in version 2.6 June 2nd 2003
C      DATA CZIL_DATA /0.2/
      CZIL_DATA =0.075

Clu: change to 3 or 2   Oct 15, 2004
      LAI_DATA =(/3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     &  	     3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     &  	     3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
C      DATA CSOIL_DATA /1.26E+6/
      CSOIL_DATA =2.00E+6
C ----------------------------------------------------------------------
C READ NAMELIST FILE TO OVERRIDE DEFAULT PARAMETERS ONLY ONCE.
C NAMELIST_NAME must be 50 characters or less.
C ----------------------------------------------------------------------
Clu: namelist is set up in run script
! PT         if (me .eq. 0) WRITE(0,*) 'READ NAMELIST SOIL_VEG'
c$$$         READ(5, SOIL_VEG)
! PT         rewind(nlunit)
! PT         READ(nlunit, SOIL_VEG)
!*       WRITE(6, SOIL_VEG)
C 	 OPEN(58, FILE = 'namelist_filename.txt')
C         READ(58,'(A)') NAMELIST_NAME
C         CLOSE(58)
C         WRITE(0,*) 'Namelist Filename is ', NAMELIST_NAME
C         OPEN(59, FILE = NAMELIST_NAME)
C 50      CONTINUE
C         READ(59, SOIL_VEG, END=100)
C         IF (LPARAM) GOTO 50
C 100     CONTINUE
C         CLOSE(59)
         IF (DEFINED_SOIL .GT. MAX_SOILTYP) THEN
            WRITE(0,*) 'Warning: DEFINED_SOIL too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_VEG .GT. MAX_VEGTYP) THEN
            WRITE(0,*) 'Warning: DEFINED_VEG too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_SLOPE .GT. MAX_SLOPETYP) THEN
            WRITE(0,*) 'Warning: DEFINED_SLOPE too large in namelist'
            STOP 222
         ENDIF
         
         SMLOW = SMLOW_DATA
         SMHIGH = SMHIGH_DATA
         
         DO I = 1,DEFINED_SOIL
            SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
            F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
            REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I))
     &           **(1.0/(2.0*BB(I)+3.0))
            REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
            WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
            WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1
            
C     ----------------------------------------------------------------------
C     CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
C     FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
C     ----------------------------------------------------------------------
            DRYSMC(I) = WLTSMC(I)
         END DO
         
!       if (me.eq.0) write(6,soil_veg)
       return
       end
