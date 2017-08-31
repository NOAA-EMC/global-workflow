 PROGRAM SFC_DRV

!----------------------------------------------------------------------
!
!  Stand alone surface cycle driver for the cubed-sphere grid.
!
!  2005-02-03:  Iredell   for global_analysis
!  2014-11-30:  xuli      add nst_anl
!  2015-05-26:  Hang Lei  Added NEMSIO read/write function in the code
!  2017-08-08:  Gayno     Modify to work on cubed-sphere grid
!
!  LUGB         Unit number used in the sfccycle subprogram
!  IDIM,JDIM    i/j dimension of a cubed-sphere tile.
!  IY,IM,ID,IH  Year, month, day, and hour of initial state.
!  FH           Forecast hour
!  IALB         Use modis albedo when '1'. Use brigleb when '0'.
!  ISOT         Use statsgo soil type when '1'. Use zobler when '0'.
!  IVEGSRC      Use igbp veg type when '1'.  Use sib when '2'.
!  SIG1T        Sigma level 1 temperature for dead start.
!               If not dead start, no need for dimension but set to
!               zero as in the example below.
!----------------------------------------------------------------------

 IMPLICIT NONE
!
 INTEGER :: IDIM, JDIM, LSOIL, LUGB, IY, IM, ID, IH, IALB
 INTEGER :: ISOT, IVEGSRC, LENSFC
 REAL    :: FH, DELTSFC
 LOGICAL :: USE_UFO, NST_ANL
!
 NAMELIST/NAMCYC/ IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH,    &
                  DELTSFC,IALB,USE_UFO,NST_ANL,           &
                  ISOT,IVEGSRC
!
 DATA IDIM,JDIM,LSOIL/96,96,4/
 DATA IY,IM,ID,IH,FH/1997,8,2,0,0./
 DATA LUGB/51/, DELTSFC/0.0/, IALB/1/
 DATA ISOT/1/, IVEGSRC/2/ 
!
 PRINT*,"STARTING CYCLE PROGRAM."

 USE_UFO = .FALSE.
 NST_ANL = .FALSE.

 PRINT*,"READ NAMCYC NAMELIST."

 READ(5,NAMCYC)
 WRITE(6,NAMCYC)

 LENSFC = IDIM*JDIM ! TOTAL NUMBER OF POINTS FOR THE CUBED-SPHERE TILE

 PRINT*,"LUGB,IDIM,JDIM,LSOIL,DELTSFC,IY,IM,ID,IH,FH: ", &
              LUGB,IDIM,JDIM,LSOIL,DELTSFC,IY,IM,ID,IH,FH

 CALL SFCDRV(LUGB,IDIM,JDIM,LENSFC,LSOIL,DELTSFC,  &
             IY,IM,ID,IH,FH,IALB,                  &
             USE_UFO,NST_ANL,ISOT,IVEGSRC)
!
 PRINT*,'CYCLE PROGRAM COMPLETED NORMALLY.'

 STOP

 END PROGRAM SFC_DRV
!
 SUBROUTINE SFCDRV(LUGB,IDIM,JDIM,LENSFC,LSOIL,DELTSFC,  &
                   IY,IM,ID,IH,FH,IALB,                  &
                   USE_UFO,NST_ANL,ISOT,IVEGSRC)
!
 USE READ_WRITE_DATA

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: IDIM, JDIM, LENSFC, LSOIL, IALB
 INTEGER, INTENT(IN) :: LUGB, IY, IM, ID, IH
 INTEGER, INTENT(IN) :: ISOT, IVEGSRC

 LOGICAL, INTENT(IN) :: USE_UFO, NST_ANL
 
 REAL, INTENT(IN)    :: FH, DELTSFC

 INTEGER, PARAMETER  :: NLUNIT=35, ME=0

 CHARACTER*500       :: FNOROG, FNGRID, FNBGSI, FNBGSO, GSI_FILE

 INTEGER             :: IFP, I, II, K

 REAL                :: SLMASK(LENSFC), OROG(LENSFC)
 REAL                :: SIHFCS(LENSFC), SICFCS(LENSFC)
 REAL                :: SITFCS(LENSFC), TSFFCS(LENSFC)
 REAL                :: SNOFCS(LENSFC), ZORFCS(LENSFC)
 REAL                :: ALBFCS(LENSFC,4), TG3FCS(LENSFC)
 REAL                :: CNPFCS(LENSFC), SMCFCS(LENSFC,LSOIL)
 REAL                :: STCFCS(LENSFC,LSOIL), SLIFCS(LENSFC)
 REAL                :: AISFCS(LENSFC), F10M(LENSFC)
 REAL                :: VEGFCS(LENSFC), VETFCS(LENSFC)
 REAL                :: SOTFCS(LENSFC), ALFFCS(LENSFC,2)
 REAL                :: CVFCS(LENSFC), CVTFCS(LENSFC)
 REAL                :: CVBFCS(LENSFC), TPRCP(LENSFC)
 REAL                :: SRFLAG(LENSFC), SWDFCS(LENSFC)
 REAL                :: SLCFCS(LENSFC,LSOIL), VMXFCS(LENSFC)
 REAL                :: VMNFCS(LENSFC), T2M(LENSFC)
 REAL                :: Q2M(LENSFC), SLPFCS(LENSFC)
 REAL                :: ABSFCS(LENSFC), OROG_UF(LENSFC)
 REAL                :: ALBFC(LENSFC*4), SMCFC(LENSFC*LSOIL)
 REAL                :: STCFC(LENSFC*LSOIL), ALFFC(LENSFC*2)
 REAL                :: SLCFC(LENSFC*LSOIL), USTAR(LENSFC)
 REAL                :: FMM(LENSFC), FHH(LENSFC)
 REAL                :: RLA(LENSFC), RLO(LENSFC)
 REAL(KIND=4)        :: ZSOIL(LSOIL)
 REAL                :: SIG1T(LENSFC)

 TYPE(NSST_DATA)     :: NSST

!  Defaults file names
!
 DATA FNOROG/'        '/
 DATA FNGRID/'        '/
 DATA FNBGSI/'        '/
 DATA FNBGSO/'        '/
 DATA GSI_FILE/'        '/
!
 DATA IFP/0/
!
 SAVE IFP, FNGRID, FNOROG, FNBGSI, FNBGSO, GSI_FILE

 NAMELIST/NAMSFCD/FNOROG,FNGRID,FNBGSI,FNBGSO,GSI_FILE

!--------------------------------------------------------------------------------
!
!  THIS IS A DRIVER FOR VERSION II SURFACE PROGRAM.
!
!  This program runs in two different modes:
!
!  1.  Analysis mode (FH=0.)
!
!      This program merges climatology, analysis and forecast guess to create
!      new surface fields.  If analysis file is given, the program 
!      uses it if date of the analysis matches with IY,IM,ID,IH (see Note
!      below).
!
!  2.  Forecast mode (FH.GT.0.)
!    
!      This program interpolates climatology to the date corresponding to the 
!      forecast hour.  If surface analysis file is given, for the corresponding
!      dates, the program will use it.  This is forcing-by-observation experiment.
!
!   NOTE:
!
!      If the date of the analysis does not match given IY,IM,ID,IH, (and FH),
!      the program searches an old analysis by going back 6 hours, then 12 hours,
!      then one day upto NREPMX days (parameter statement in the SUBROTINE FIXRD. 
!      Now defined as 15).  This allows the user to provide non-daily analysis to 
!      be used.  If matching field is not found, the forecast guess will be used.
!
!      Use of a combined earlier surface analyses and current analysis is 
!      NOT allowed (as was done in the old version for snow analysis in which
!      old snow analysis is used in combination with initial guess), except
!      for sea surface temperature.  For sst anolmaly interpolation, you need to
!      set LANOM=.TRUE. and must provide sst analysis at initial time.  
!
!      If you want to do complex merging of past and present surface field analysis,
!      YOU NEED TO CREATE a separate file that contains DAILY SURFACE FIELD.
!
!      LUGB is the unit number used in sfccycle subprogram
!      IDIM,JDIM is thegrid dimension in x and y direction, respectively of a tile
!                of the cubed-sphere grid.
!      LSOIL is the number of soil layers 
!      IY,IM,ID,IH is the Year, month, day, and hour of initial state.
!      FH is the forecast hour
!      SIG1T is the sigma level 1 temperature for dead start.  
!
!  Variable naming conventions:
!
!     OROG .. Orography
!     ALB  .. Snow-free albedo
!     SNO  .. Liquid-equivalent snow depth
!     ZOR  .. Surface roughness length
!     VET  .. Vegetation type
!     TSF  .. Surface skin temperature.  Sea surface temp. over ocean.
!     TG3  .. Deep soil temperature (at 500cm)
!     STC  .. Soil temperature (LSOIL layrs)
!     SMC  .. Total soil moisture (LSOIL layrs)
!     AIS  .. Sea ice mask (0 or 1)
!     CNP  .. Canopy water content
!     CV   .. Convective cloud cover
!     CVB  .. Convective cloud base
!     CVT  .. Convective cloud top
!     SLI  .. LAND/SEA/SEA-ICE mask. (1/0/2 respectively)
!     VEG  .. Vegetation cover
!     SOT  .. Soil type
!     SIH  .. Sea ice thickness
!     SIC  .. Sea ice concentration
!     SWD  .. Actual snow depth
!     SLC  .. Liquid soil moisture (LSOIL layers)
!     VMN  .. Vegetation cover minimum
!     VMX  .. Vegetation cover maximum
!     SLP  .. Slope type
!     ABS  .. Maximum snow albedo
!     T2M  .. 2m Temperature
!     Q2M  .. 2m Specific Humidity
!     TICE .. Ice Temperature
!     OROG_UF .. Orography unfiltered
!
!  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
!  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
!  1.0 = USE OF FORECAST
!  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
!
!   These values are set for analysis mode.
!
!   Variables                  Land                 Sea
!   ---------------------------------------------------------
!   Surface temperature        Forecast             Analysis
!   Albedo                     Analysis             Analysis
!   Sea-ice                    Analysis             Analysis
!   Snow                       Analysis             Forecast (over sea ice)
!   Roughness                  Analysis             Forecast
!   Plant resistance           Analysis             Analysis
!   Soil wetness (layer)       Weighted average     Analysis
!   Soil temperature           Forecast             Analysis
!   Canopy waver content       Forecast             Forecast
!   Convective cloud cover     Forecast             Forecast
!   Convective cloud bottm     Forecast             Forecast
!   Convective cloud top       Forecast             Forecast
!   Vegetation cover           Analysis             Analysis
!   vegetation type            Analysis             Analysis
!   soil type                  Analysis             Analysis
!
!--------------------------------------------------------------------------------

 SIG1T = 0.0            ! Not a dead start!

 IF(IFP.EQ.0) THEN
   IFP = 1
   READ (5,NAMSFCD)
   WRITE(6,NAMSFCD)
 ENDIF

 PRINT*,'IN ROUTINE SFCDRV,IDIM=',IDIM,'JDIM=',JDIM,'FH=',FH

!--------------------------------------------------------------------------------
! READ THE OROGRAPHY AND GRID POINT LAT/LONS FOR THE CUBED-SPHERE TILE.
!--------------------------------------------------------------------------------

 CALL READ_LAT_LON_OROG(RLA,RLO,OROG,OROG_UF,FNOROG,FNGRID,  &
                        IDIM,JDIM,LENSFC)

 IF (NST_ANL) THEN
   PRINT*,"WILL PROCESS NSST RECORDS."
   ALLOCATE(NSST%C_0(LENSFC))
   ALLOCATE(NSST%C_D(LENSFC))
   ALLOCATE(NSST%D_CONV(LENSFC))
   ALLOCATE(NSST%DT_COOL(LENSFC))
   ALLOCATE(NSST%IFD(LENSFC))
   ALLOCATE(NSST%QRAIN(LENSFC))
   ALLOCATE(NSST%TREF(LENSFC))
   ALLOCATE(NSST%W_0(LENSFC))
   ALLOCATE(NSST%W_D(LENSFC))
   ALLOCATE(NSST%XS(LENSFC))
   ALLOCATE(NSST%XT(LENSFC))
   ALLOCATE(NSST%XTTS(LENSFC))
   ALLOCATE(NSST%XU(LENSFC))
   ALLOCATE(NSST%XV(LENSFC))
   ALLOCATE(NSST%XZ(LENSFC))
   ALLOCATE(NSST%XZTS(LENSFC))
   ALLOCATE(NSST%Z_C(LENSFC))
   ALLOCATE(NSST%ZM(LENSFC))
 ENDIF

!--------------------------------------------------------------------------------
! READ THE INPUT SURFACE DATA ON THE CUBED-SPHERE TILE.
!--------------------------------------------------------------------------------

 CALL READ_DATA(TSFFCS,SMCFCS,SNOFCS,STCFCS,TG3FCS,ZORFCS,  &
                CVFCS,CVBFCS,CVTFCS,ALBFCS,SLIFCS,          &
                VEGFCS,CNPFCS,F10M,VETFCS,SOTFCS,           &
                ALFFCS,USTAR,FMM,FHH,SIHFCS,SICFCS,         &
                SITFCS,TPRCP,SRFLAG,SWDFCS,VMNFCS,          &
                VMXFCS,SLCFCS,SLPFCS,ABSFCS,T2M,Q2M,        &
                SLMASK,ZSOIL,LSOIL,LENSFC,FNBGSI,NST_ANL,NSST)

 IF (USE_UFO) THEN
   PRINT*,'USE UNFILTERED OROGRAPHY.'
 ELSE
   OROG_UF = 0.0
 ENDIF
 
 DO I=1,LENSFC
   AISFCS(I) = 0.
   IF(NINT(SLIFCS(I)).EQ.2) AISFCS(I) = 1.
 ENDDO

 DO K=1,LSOIL
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     SMCFC(II+I) = SMCFCS(I,K)
     STCFC(II+I) = STCFCS(I,K)
     SLCFC(II+I) = SLCFCS(I,K)
   ENDDO
 ENDDO

 DO K=1,4
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     ALBFC(II+I) = ALBFCS(I,K)
   ENDDO
 ENDDO

 DO K=1,2
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     ALFFC(II+I) = ALFFCS(I,K)
   ENDDO
 ENDDO
 
!--------------------------------------------------------------------------------
! UPDATE SURFACE FIELDS.
!--------------------------------------------------------------------------------

 CALL SFCCYCLE(LUGB,LENSFC,LSOIL,SIG1T,DELTSFC,        &
               IY,IM,ID,IH,FH,RLA,RLO,                 &
               SLMASK,OROG, OROG_UF, USE_UFO, NST_ANL, &
               SIHFCS,SICFCS,SITFCS,SWDFCS,SLCFC,      &
               VMNFCS,VMXFCS,SLPFCS,ABSFCS,            &
               TSFFCS,SNOFCS,ZORFCS,ALBFC,TG3FCS,      &
               CNPFCS,SMCFC,STCFC,SLIFCS,AISFCS,F10M,  &
               VEGFCS,VETFCS,SOTFCS,ALFFC,             &
               CVFCS,CVBFCS,CVTFCS,ME,NLUNIT,IALB,ISOT,IVEGSRC)

 DO K=1,LSOIL
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     SMCFCS(I,K) = SMCFC(II+I)
     STCFCS(I,K) = STCFC(II+I)
     SLCFCS(I,K) = SLCFC(II+I)
   ENDDO
 ENDDO

 DO K=1,4
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     ALBFCS(I,K) = ALBFC(II+I)
   ENDDO
 ENDDO

 DO K=1,2
   II = (K-1)*LENSFC
   DO I=1,LENSFC
     ALFFCS(I,K) = ALFFC(II+I)
   ENDDO
 ENDDO
 
!--------------------------------------------------------------------------------
! IF RUNNING WITH NSST, READ IN GSI FILE WITH THE UPDATED INCREMENTS (ON THE
! GAUSSIAN GRID), INTERPOLATE INCREMENTS TO THE CUBED-SPHERE TILE, AND PERFORM
! REQUIRED ADJUSTMENTS AND QC.
!--------------------------------------------------------------------------------

 IF (NST_ANL) THEN
   CALL READ_GSI_DATA(GSI_FILE)
   CALL ADJUST_NSST(RLA,RLO,SLIFCS,LENSFC)
   STOP
 ENDIF

!--------------------------------------------------------------------------------
! WRITE OUT UPDATED SURFACE DATA ON THE CUBED-SPHERE TILE.
!--------------------------------------------------------------------------------

 CALL WRITE_DATA(SLIFCS,TSFFCS,SNOFCS,TG3FCS,ZORFCS,         &
                 ALBFCS,ALFFCS,VEGFCS,CNPFCS,F10M,           &
                 T2M,Q2M,VETFCS,SOTFCS,USTAR,FMM,FHH,        &
                 SICFCS,SIHFCS,SITFCS,                       &
                 TPRCP,SRFLAG,SWDFCS,                        &
                 VMNFCS,VMXFCS,SLPFCS,ABSFCS,                &
                 SLCFCS,SMCFCS,STCFCS,                       &
                 IDIM,JDIM,LENSFC,LSOIL,FNBGSO,NST_ANL,NSST)

 IF (NST_ANL) THEN
   DEALLOCATE(NSST%C_0)
   DEALLOCATE(NSST%C_D)
   DEALLOCATE(NSST%D_CONV)
   DEALLOCATE(NSST%DT_COOL)
   DEALLOCATE(NSST%IFD)
   DEALLOCATE(NSST%QRAIN)
   DEALLOCATE(NSST%TREF)
   DEALLOCATE(NSST%W_0)
   DEALLOCATE(NSST%W_D)
   DEALLOCATE(NSST%XS)
   DEALLOCATE(NSST%XT)
   DEALLOCATE(NSST%XTTS)
   DEALLOCATE(NSST%XU)
   DEALLOCATE(NSST%XV)
   DEALLOCATE(NSST%XZ)
   DEALLOCATE(NSST%XZTS)
   DEALLOCATE(NSST%Z_C)
   DEALLOCATE(NSST%ZM)
 ENDIF

 RETURN

 END SUBROUTINE SFCDRV
 
 SUBROUTINE ADJUST_NSST(RLA,RLO,SLMSK_TILE,LENSFC)

 USE GDSWZD_MOD
 USE READ_WRITE_DATA, ONLY : IDIM_GAUS, JDIM_GAUS, &
                             SLMSK_GAUS

 IMPLICIT NONE

 INTEGER, INTENT(IN)      :: LENSFC

 REAL, INTENT(IN)         :: SLMSK_TILE(LENSFC)
 REAL, INTENT(INOUT)      :: RLA(LENSFC), RLO(LENSFC)

 INTEGER                  :: IOPT, NRET, KGDS_GAUS(200)
 INTEGER                  :: I, J, IJ, II, JJ, III, JJJ, KRAD
 INTEGER                  :: ISTART, IEND, JSTART, JEND
 INTEGER                  :: MASK_TILE, MASK_GAUS

 REAL                     :: FILL
 REAL, ALLOCATABLE        :: XPTS(:), YPTS(:)

 KGDS_GAUS     = 0
 KGDS_GAUS(1)  = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
 KGDS_GAUS(2)  = IDIM_GAUS  ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
 KGDS_GAUS(3)  = JDIM_GAUS
 KGDS_GAUS(4)  = 90000      ! OCT 11-13 - LAT OF ORIGIN
 KGDS_GAUS(5)  = 0          ! OCT 14-16 - LON OF ORIGIN
 KGDS_GAUS(6)  = 128        ! OCT 17 - RESOLUTION FLAG
 KGDS_GAUS(7)  = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
 KGDS_GAUS(8)  = NINT(-360000./FLOAT(IDIM_GAUS))  ! OCT 21-23 - LON OF EXTREME POINT
 KGDS_GAUS(9)  = NINT((360.0 / FLOAT(IDIM_GAUS))*1000.0)
                            ! OCT 24-25 - LONGITUDE DIRECTION INCR.
 KGDS_GAUS(10) = JDIM_GAUS/2     ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
 KGDS_GAUS(12) = 255        ! OCT 29 - RESERVED
 KGDS_GAUS(20) = 255        ! OCT 5  - NOT USED, SET TO 255

 PRINT*,'ADJUST NSST'

 IOPT = -1
 FILL = -9999.
 ALLOCATE(XPTS(LENSFC))
 ALLOCATE(YPTS(LENSFC))
 XPTS = FILL
 YPTS = FILL

! CALL TO GDSWZD DETERMINES THE NEAREST GAUSSIAN POINT TO EACH
! CUBED-SPHERE TILE POINT.

 CALL GDSWZD(KGDS_GAUS,IOPT,LENSFC,FILL,XPTS,YPTS,RLO,RLA,NRET)

 IF (NRET /= LENSFC) THEN
   PRINT*,'PROBLEM IN GDSWZD'
   STOP 12
 ENDIF

! INTERPOLATE NSST INCREMENTS FROM THE GAUSSIAN GRID TO THE CUBED-SPHERE
! TILE USING NEAREST NEIGHBOR.

 IJ_LOOP : DO IJ = 1, LENSFC

   I = NINT(XPTS(IJ))
   J = NINT(YPTS(IJ))

   MASK_TILE = NINT(SLMSK_TILE(IJ))
   IF (MASK_TILE == 1) CYCLE IJ_LOOP  ! NSST NOT APPLIED AT LAND POINTS.
   IF (MASK_TILE == 2) MASK_TILE = 0  ! WHAT ABOUT SEA ICE??

   MASK_GAUS = NINT(SLMSK_GAUS(I,J))

! IF TILE MASK AND NEAREST GSI POINT MASK ARE BOTH NON-LAND, APPLY 
! NSST INCREMENT (HOW WILL THIS BE DONE?)

   IF (MASK_TILE == MASK_GAUS) THEN

! ADD CODE TO UPDATE NSST.

! IF MASK IS NOT THE SAME, PERFORM A SPIRAL SEARCH TO FIND NEAREST NON-LAND
! POINT ON GAUSSIAN GRID.

   ELSE  

     DO KRAD = 1, 500
       ISTART = I - KRAD
       IEND   = I + KRAD
       JSTART = J - KRAD
       JEND   = J + KRAD
       DO JJ = JSTART, JEND
       DO II = ISTART, IEND

         IF((JJ == JSTART) .OR. (JJ == JEND) .OR.   &
            (II == ISTART) .OR. (II == IEND))  THEN

           IF ((JJ >= 1) .AND. (JJ <= JDIM_GAUS)) THEN

             JJJ = JJ
             IF (II <= 0) THEN
               III = IDIM_GAUS + II
             ELSE IF (II >= (IDIM_GAUS+1)) THEN
               III = II - IDIM_GAUS
             ELSE
               III = II
             END IF

             IF (MASK_TILE == NINT(SLMSK_GAUS(III,JJJ))) THEN

!              ADD CODE TO UPDATE NSST.

               print*,'mismatch at    ',i,j
               PRINT*,'found match at ',iii,jjj,krad
               CYCLE IJ_LOOP
             ENDIF

           ENDIF

         ENDIF

       ENDDO
       ENDDO

     ENDDO

! THE SEARCH SHOULD NEVER FAIL UNLESS THE TILE MASK AND THE
! GAUSSIAN MASK ARE VERY DIFFERENT.  WHAT SHOULD HAPPEN
! IF THIS OCCURS?

     print*,'search failed '
     stop 4

   ENDIF
 ENDDO IJ_LOOP

 DEALLOCATE (XPTS, YPTS)

 END SUBROUTINE ADJUST_NSST
