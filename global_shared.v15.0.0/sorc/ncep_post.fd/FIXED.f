      SUBROUTINE FIXED
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    FIXED       POSTS FIXED FIELDS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-08-30
!     
! ABSTRACT:  THIS ROUTINE POSTS FIXED (IE, TIME INDEPENDENT)
!  ETA MODEL FIELDS.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-08-30  RUSS TREADON
!   96-04-05  MIKE BALDWIN - CHANGED ALBEDO CALC
!   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-07-17  MIKE BALDWIN - REMOVED LABL84
!   00-01-05  JIM TUCCILLO - MPI VERSION
!   02-06-19  MIKE BALDWIN - WRF VERSION
!   11-02-06  JUN WANG     - grib2 option
!     
! USAGE:    CALL FIXED
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     NONE 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - LOOPS
!                  MASKS
!                  LLGRDS
!                  RQSTFLD
!                  PHYS
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
      use vrbls3d, only: pint
      use vrbls2d, only: albedo, avgalbedo, albase, mxsnal, sst, ths, epsr
      use masks, only: gdlat, gdlon, sm, sice, lmh, lmv
      use params_mod, only: small, p1000, capa
      use lookup_mod, only: ITB,JTB,ITBQ,JTBQ
      use ctlblk_mod, only: jsta, jend, grib, cfld, fld_info, datapd, spval, tsrfc,&
              ifhr, ifmin, lm, im, jm
      use rqstfld_mod, only: iget, lvls, iavblfld, id
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
      integer,PARAMETER :: SNOALB=0.55
!     INCLUDE COMMON BLOCKS.
!
!     DECLARE VARIABLES
      REAL,dimension(im,jm) :: GRID1
!     REAL,dimension(im,jm) :: GRID1, GRID2
      integer I,J,ITSRFC,IFINCR
!     
!********************************************************************
!
!     START FIXED HERE.
!
!     LATITUDE (OUTPUT GRID).
      IF (IGET(048).GT.0) THEN
!$omp parallel do private(i,j)
         DO J = JSTA,JEND
            DO I = 1,IM
               GRID1(I,J) = GDLAT(I,J)
            END DO
         END DO
         ID(1:25) = 0
         if(grib=='grib1') then
          CALL GRIBIT(IGET(048),LVLS(1,IGET(048)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(048))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!     
!     LONGITUDE (OUTPUT GRID). CONVERT TO EAST
      IF (IGET(049).GT.0) THEN
         DO J = JSTA,JEND
            DO I = 1,IM
             IF (GDLON(I,J) .LT. 0.)THEN            
               GRID1(I,J) = 360. + GDLON(I,J)
             ELSE
               GRID1(I,J) = GDLON(I,J)
             END IF
             IF (GRID1(I,J).GT.360.)print*,'LARGE GDLON ',      &
             i,j,GDLON(I,J)
            END DO
         END DO
         ID(1:25) = 0
         if(grib=='grib1') then
           CALL GRIBIT(IGET(049),LVLS(1,IGET(049)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(049))
           datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!     
!     LAND/SEA MASK.
      IF (IGET(050).GT.0) THEN
!$omp parallel do private(i,j)
         DO J = JSTA,JEND
           DO I = 1,IM
             GRID1(I,J) = SPVAL
              IF(SM(I,J)   /= SPVAL) GRID1(I,J) = 1. - SM(I,J)
              IF(SICE(I,J) /= SPVAL .AND. SICE(I,J) > 0.1) GRID1(I,J) = 0.
!           if(j.eq.jm/2)print*,'i,mask= ',i,grid1(i,j)
           ENDDO
         ENDDO
         ID(1:25) = 0
         if(grib=='grib1') then
           CALL GRIBIT(IGET(050),LVLS(1,IGET(050)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(050))
           datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!     
!     SEA ICE MASK.
      IF (IGET(051).GT.0) THEN
!$omp parallel do private(i,j)
         DO J = JSTA,JEND
           DO I = 1,IM
             GRID1(I,J) = SICE(I,J)
           ENDDO
         ENDDO
         ID(1:25) = 0
          if(grib=='grib1') then
          CALL GRIBIT(IGET(051),LVLS(1,IGET(051)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(051))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!     
!     MASS POINT ETA SURFACE MASK.
      IF (IGET(052).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = LMH(I,J)
           ENDDO
         ENDDO
         ID(1:25) = 0
         if(grib=='grib1') then
          CALL GRIBIT(IGET(052),LVLS(1,IGET(052)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(052))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!     
!     VELOCITY POINT ETA SURFACE MASK.
      IF (IGET(053).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = LMV(I,J)
           ENDDO
         ENDDO
         ID(1:25) = 0
          if(grib=='grib1') then
          CALL GRIBIT(IGET(053),LVLS(1,IGET(053)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(053))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!
!     SURFACE ALBEDO.
!       NO LONGER A FIXED FIELD, THIS VARIES WITH SNOW COVER
!MEB since this is not a fixed field, move this to SURFCE
!
      IF (IGET(150).GT.0) THEN
!$omp parallel do private(i,j)
       DO J=JSTA,JEND
         DO I=1,IM
!           SNOK = AMAX1(SNO(I,J),0.0)
!           SNOFAC = AMIN1(SNOK*50.0,1.0)
!           EGRID1(I,J)=ALB(I,J)+(1.-VEGFRC(I,J))*SNOFAC
!     1                *(SNOALB-ALB(I,J))
          IF(ABS(ALBEDO(I,J)-SPVAL).GT.SMALL)                   &
           GRID1(I,J)=ALBEDO(I,J)
         ENDDO
       ENDDO
!       CALL E2OUT(150,000,GRID1,GRID2,GRID1,GRID2,IM,JM)
       ID(1:25) = 0
       CALL SCLFLD(GRID1,100.,IM,JM)
       if(grib=='grib1') then
        CALL GRIBIT(IGET(150),LVLS(1,IGET(150)),GRID1,IM,JM)
       elseif(grib=='grib2') then
        cfld=cfld+1
        fld_info(cfld)%ifld=IAVBLFLD(IGET(150))
        datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
       endif
      ENDIF
!      
!     TIME AVERAGED SURFACE ALBEDO.
      IF (IGET(266).GT.0) THEN
            ID(1:25) = 0
            ITSRFC     = NINT(TSRFC)
            IF(ITSRFC .ne. 0) then
             IFINCR     = MOD(IFHR,ITSRFC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITSRFC*60)
            ELSE
              IFINCR     = 0
            endif
            ID(19)     = IFHR
            IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 3
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITSRFC
            ELSE
               ID(18) = IFHR-IFINCR
               IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                IF(ABS(AVGALBEDO(I,J)-SPVAL).GT.SMALL)           &
                  GRID1(I,J) = AVGALBEDO(I,J)*100.
              ENDDO
            ENDDO
       
            if(grib=='grib1') then
             CALL GRIBIT(IGET(266),LVLS(1,IGET(266)),GRID1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(266))
             if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
             datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
            endif
      ENDIF
!
      IF (IGET(226).GT.0) THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IF(ABS(ALBASE(I,J)-SPVAL).GT.SMALL)                     &
     &          GRID1(I,J) = ALBASE(I,J)*100.
         ENDDO
        ENDDO
       ID(1:25) = 0
       ID(02) = 130
       if(grib=='grib1') then
        CALL GRIBIT(IGET(226),LVLS(1,IGET(226)),GRID1,IM,JM)
       elseif(grib=='grib2') then
        cfld=cfld+1
        fld_info(cfld)%ifld=IAVBLFLD(IGET(226))
        datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
       endif
      ENDIF
!  Max snow albedo
      IF (IGET(227).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
! sea point, albedo=0.06 same as snow free albedo
             IF( (abs(SM(I,J)-1.) .lt. 1.0E-5) ) THEN
               MXSNAL(I,J)=0.06
! sea-ice point, albedo=0.60, same as snow free albedo
             ELSEIF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.             &
     &               (abs(SICE(I,J)-1.) .lt. 1.0E-5) ) THEN
               MXSNAL(I,J)=0.60
             ENDIF
           ENDDO
         ENDDO
       
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             IF(ABS(MXSNAL(I,J)-SPVAL).GT.SMALL)                      &
     &         GRID1(I,J) = MXSNAL(I,J)*100.
           ENDDO
         ENDDO
       ID(1:25) = 0
       ID(02) = 130
       if(grib=='grib1') then
        CALL GRIBIT(IGET(227),LVLS(1,IGET(227)),                   &
              GRID1,IM,JM)
       elseif(grib=='grib2') then
        cfld=cfld+1
        fld_info(cfld)%ifld=IAVBLFLD(IGET(227))
        datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
       endif
      ENDIF
!
!     SEA SURFACE TEMPERAURE.
      IF (IGET(151).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             IF( (abs(SM(I,J)-1.) < 1.0E-5) ) THEN
               GRID1(I,J) = SST(I,J)
             ELSE
               GRID1(I,J) = THS(I,J)*(PINT(I,J,LM+1)/P1000)**CAPA
             END IF  
           ENDDO
         ENDDO
         ID(1:25) = 0
         if(grib=='grib1') then
          CALL GRIBIT(IGET(151),LVLS(1,IGET(151)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(151))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF

!     EMISSIVIT.
       IF (IGET(248).GT.0) THEN
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = EPSR(I,J)
            ENDDO
          ENDDO
          ID(1:25) = 0
          ID(02)=133 ! Parameter Table 133
        if(grib=='grib1') then
           CALL GRIBIT(IGET(248),LVLS(1,IGET(248)),GRID1,IM,JM)
          elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(248))
           datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
       ENDIF

!
!     END OF ROUTINE.
!     
      RETURN
      END

