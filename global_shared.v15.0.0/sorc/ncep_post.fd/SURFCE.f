      SUBROUTINE SURFCE
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    SURFCE      POST SURFACE BASED FIELDS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-21       
!     
! ABSTRACT:
!     THIS ROUTINE POSTS SURFACE BASED FIELDS.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-21  RUSS TREADON
!   94-08-04  MICHAEL BALDWIN - ADDED OUTPUT OF SFC FLUXES OF
!                               SENS AND LATENT HEAT AND THETA AT Z0
!   94-11-04  MICHAEL BALDWIN - ADDED INSTANTANEOUS PRECIP TYPE
!   96-03-19  MICHAEL BALDWIN - CHANGE SOIL PARAMETERS
!   96-09-25  MICHAEL BALDWIN - ADDED SNOW RATIO FROM EXPLICIT SCHEME
!   96-10-17  MICHAEL BALDWIN - CHANGED SFCEVP,POTEVP TO ACCUM.  TOOK
!                               OUT -PTRACE FOR ACSNOW,SSROFF,BGROFF.
!   97-04-23  MICHAEL BALDWIN - TOOK OUT -PTRACE FOR ALL PRECIP FIELDS
!   98-06-12  T BLACK         - CONVERSION FROM 1-D TO 2-D
!   98-07-17  MIKE BALDWIN - REMOVED LABL84
!   98-08-18  MIKE BALDWIN - COMPUTE RH OVER ICE
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-11  MIKE BALDWIN - WRF VERSION ASSUMING ALL ACCUM VARS
!                            HAVE BUCKETS THAT FILL FROM T=00H ON
!   02-08-28  H CHUANG - COMPUTE FIELDS AT SHELTER LEVELS FOR WRF
!   04-12-09  H CHUANG - ADD ADDITIONAL LSM FIELDS
!   05-07-07  BINBIN ZHOU - ADD RSM MODEL
!   05-08-24  GEOFF MANIKIN - ADDED DOMINANT PRECIP TYPE
!   11-02-06  JUN WANG  - ADDED GRIB2 OPTION
!   13-08-05  S Moorthi - Eliminate unnecessary arrays (reduce memory)
!                         and some cosmetic changes
!   14-02-26  S Moorthi - threading datapd assignment
!   14-11-26  S Moorthi - cleanup and some bug fix (may be?)
!     
! USAGE:    CALL SURFCE
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       BOUND    - ENFORCE LOWER AND UPPER LIMITS ON ARRAY ELEMENTS.
!       DEWPOINT - COMPUTE DEWPOINT TEMPERATURE.
!       CALDRG   - COMPUTE SURFACE LAYER DRAG COEFFICENT
!       CALTAU   - COMPUTE SURFACE LAYER U AND V WIND STRESSES.
!
!     LIBRARY:
!       COMMON   - CTLBLK
!                  RQSTFLD
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
!     
!     INCLUDE GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
!
      use vrbls3d, only: zint, pint, t, pmid, q, f_rimef
      use vrbls2d, only: ths, qs, qvg, qv2m, tsnow, tg, smstav, smstot,       &
                         cmc, sno, snoavg, psfcavg, t10avg, snonc, ivgtyp,    &
                         si, potevp, dzice, qwbs, vegfrc, isltyp, pshltr,     &
                         tshltr, qshltr, mrshltr, maxtshltr, mintshltr,       &
                         maxrhshltr, minrhshltr, u10, psfcavg, v10, u10max,   &
                         v10max, th10, t10m, q10, wspd10max, prec, sr,        &
                         cprate, avgcprate, avgprec, acprec, cuprec, ancprc,  &
                         lspa, acsnow, acsnom, snowfall,ssroff, bgroff,       &
                         runoff, pcp_bucket, rainnc_bucket, snow_bucket,      &
                         snownc, tmax, graup_bucket, graupelnc, qrmax, sfclhx,&
                         rainc_bucket, sfcshx, subshx, snopcx, sfcuvx,        &
                         sfcvx, smcwlt, suntime, pd, sfcux, sfcevp, z0,       &
                         ustar, mdltaux, mdltauy, gtaux, gtauy, twbs,         &
                         sfcexc, grnflx, islope, czmean, czen, rswin,akhsavg ,&
                         akmsavg, u10h, v10h,snfden,sndepac,qvl1,             &
                         spduv10mean,swradmean,swnormmean,prate_max,fprate_max &
                         ,fieldcapa,edir,ecan,etrans,esnow, &
                         avgedir,avgecan,avgetrans,avgesnow, &
                         acond,maxqshltr,minqshltr,avgpotevp
      use soil,    only: stc, sllevel, sldpth, smc, sh2o
      use masks,   only: lmh, sm, sice, htm, gdlat, gdlon
      use physcons,only: CON_EPS, CON_EPSM1
      use params_mod, only: p1000, capa, h1m12, pq0, a2,a3, a4, h1, d00, d01,&
                            eps, oneps, d001, h99999, h100, small, h10e5,    &
                            elocp, g, xlai, tfrz
      use ctlblk_mod, only: jsta, jend, lm, spval, grib, cfld, fld_info,     &
                            datapd, nsoil, isf_surface_physics, tprec, ifmin,&
                            modelname, tmaxmin, pthresh, dtq2, dt, nphs,     &
                            ifhr, prec_acc_dt, sdat, ihrst, jsta_2l, jend_2u,&
                            lp1, imp_physics, me, asrfc, tsrfc, pt, pdtop,   &
                            mpi_comm_comp, im, jm
      use rqstfld_mod, only: iget, lvls, id, iavblfld, lvlsxml
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      INCLUDE "mpif.h"
!     
!     IN NGM SUBROUTINE OUTPUT WE FIND THE FOLLOWING COMMENT.
!     "IF THE FOLLOWING THRESHOLD VALUES ARE CHANGED, CONTACT
!     TDL/SYNOPTIC-SCALE TECHNIQUES BRANCH (PAUL DALLAVALLE
!     AND JOHN JENSENIUS).  THEY MAY BE USING IT IN ONE OF 
!     THEIR PACKING CODES."  THE THRESHOLD VALUE IS 0.01 INCH
!     OR 2.54E-4 METER.  PRECIPITATION VALUES LESS THAN THIS
!     THRESHOLD ARE SET TO MINUS ONE TIMES THIS THRESHOLD.
      real,PARAMETER :: PTRACE = 0.000254E0
!     
!     SET CELCIUS TO KELVIN AND SECOND TO HOUR CONVERSION.
      integer,parameter :: nalg=5, nosoiltype=9
      real,   PARAMETER :: C2K    = 273.15, SEC2HR = 1./3600.
!     
!     DECLARE VARIABLES.
!     
      integer, dimension(im,jsta:jend)  :: nroots, iwx1
      real, allocatable, dimension(:,:) :: zsfc, psfc, tsfc, qsfc,      &
                                           rhsfc, thsfc, dwpsfc, p1d,   &
                                           t1d, q1d, zwet,              &
                                           smcdry, smcmax,doms, domr,   &
                                           domip, domzr,  rsmin, smcref,&
                                           rcq, rct, rcsoil, gc, rcs
           
      real,    dimension(im,jsta:jend)       :: evp
      real,    dimension(im,jsta_2l:jend_2u) :: egrid1, egrid2
      real,    dimension(im,jm)              :: grid1, grid2
!                                   , ua, va
       real, allocatable, dimension(:,:,:)   :: sleet, rain, freezr, snow
!      real,   dimension(im,jm,nalg) :: sleet, rain, freezr, snow

!GSD
      REAL totprcp, snowratio,t2,rainl

!
      integer I,J,IWX,ITMAXMIN,IFINCR,ISVALUE,II,JJ,                    &
              ITPREC,ITSRFC,L,LS,IVEG,LLMH,                             &
              IVG,IRTN,ISEED, icat, cnt_snowratio(10),icnt_snow_rain_mixed

      real RDTPHS,TLOW,TSFCK,QSAT,DTOP,DBOT,SNEQV,RRNUM,SFCPRS,SFCQ,    &
           RC,SFCTMP,SNCOVR,FACTRS,SOLAR, s,tk,tl,w,t2c,dlt,APE,        &
           qv,e,dwpt,dum1,dum2,dum3,dum1s,dum3s,dum21,dum216,es

      real,external :: fpvsnew

!****************************************************************************
!
!     START SURFCE.
!
!     
!***  BLOCK 1.  SURFACE BASED FIELDS.
!
!     IF ANY OF THE FOLLOWING "SURFACE" FIELDS ARE REQUESTED,
!     WE NEED TO COMPUTE THE FIELDS FIRST.
!     
      IF ( (IGET(024).GT.0).OR.(IGET(025).GT.0).OR.     &
           (IGET(026).GT.0).OR.(IGET(027).GT.0).OR.     &
           (IGET(028).GT.0).OR.(IGET(029).GT.0).OR.     &
           (IGET(154).GT.0).OR.                         &
           (IGET(034).GT.0).OR.(IGET(076).GT.0) ) THEN
!     
         allocate(zsfc(im,jsta:jend),  psfc(im,jsta:jend),  tsfc(im,jsta:jend)&
                 ,rhsfc(im,jsta:jend), thsfc(im,jsta:jend), qsfc(im,jsta:jend))
!$omp parallel do private(i,j,tsfck,qsat)
         DO J=JSTA,JEND
           DO I=1,IM
!
!           SCALE ARRAY FIS BY GI TO GET SURFACE HEIGHT.
!            ZSFC(I,J)=FIS(I,J)*GI

             ZSFC(I,J) = ZINT(I,J,LM+1)
             PSFC(I,J) = PINT(I,J,NINT(LMH(I,J))+1)    ! SURFACE PRESSURE.
!     
!           SURFACE (SKIN) POTENTIAL TEMPERATURE AND TEMPERATURE.
             THSFC(I,J) = THS(I,J)
             TSFC(I,J)  = spval
             IF(THSFC(i,j) /= spval)                                   &
             TSFC(I,J) = THSFC(I,J)*(PSFC(I,J)/P1000)**CAPA 
!     
!       SURFACE SPECIFIC HUMIDITY, RELATIVE HUMIDITY, AND DEWPOINT.
!       ADJUST SPECIFIC HUMIDITY IF RELATIVE HUMIDITY EXCEEDS 0.1 OR 1.0.

             QSFC(I,J)  = MAX(H1M12,QS(I,J))
             TSFCK      = TSFC(I,J)
     
             IF(MODELNAME == 'RAPR') THEN
                QSAT    = MAX(0.0001,PQ0/PSFC(I,J)*EXP(A2*(TSFCK-A3)/(TSFCK-A4)))
             elseif (modelname == 'GFS') then
                es      = fpvsnew(tsfck)
                qsat    = con_eps*es/(psfc(i,j)+con_epsm1*es)
             ELSE
                QSAT    = PQ0/PSFC(I,J)*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
             ENDIF
             RHSFC(I,J) = max(D01, min(H1,QSFC(I,J)/QSAT))

             QSFC(I,J)  = RHSFC(I,J)*QSAT
             RHSFC(I,J) = RHSFC(I,J) * 100.0
             EVP(I,J)   = D001*PSFC(I,J)*QSFC(I,J)/(EPS+ONEPS*QSFC(I,J))
!     
!mp           ACCUMULATED NON-CONVECTIVE PRECIP.
!mp            IF(IGET(034).GT.0)THEN
!mp              IF(LVLS(1,IGET(034)).GT.0)THEN

!           ACCUMULATED PRECIP (convective + non-convective)
!            IF(IGET(087) > 0)THEN
!              IF(LVLS(1,IGET(087)) > 0)THEN
!                write(6,*) 'acprec, ancprc, cuprec: ', ANCPRC(I,J)+CUPREC(I,J),
!     +		 ANCPRC(I,J),CUPREC(I,J)
!                 ACPREC(I,J) = ANCPRC(I,J) + CUPREC(I,J)     ???????
!              ENDIF
!            ENDIF

           ENDDO
         ENDDO
!     
!        INTERPOLATE/OUTPUT REQUESTED SURFACE FIELDS.
!     
!        SURFACE PRESSURE.
         IF (IGET(024).GT.0) THEN
           ID(1:25) = 0
           if(grib == 'grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = psfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(024),LVLS(1,IGET(024)), grid1,IM,JM)
           elseif(grib == 'grib2') then
             cfld = cfld+1
             fld_info(cfld)%ifld = IAVBLFLD(IGET(024))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = PSFC(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     
!        SURFACE HEIGHT.
         IF (IGET(025).GT.0) THEN
!!          CALL BOUND(GRID1,D00,H99999)
            ID(1:25) = 0
            if(grib == 'grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = zsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(025),LVLS(1,IGET(025)), grid1,IM,JM)
            elseif(grib == 'grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld = IAVBLFLD(IGET(025))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = ZSFC(i,jj)
               enddo
             enddo
            endif
         ENDIF
         if (allocated(zsfc)) deallocate(zsfc)
!        if (allocated(psfc)) deallocate(psfc)
!     
!        SURFACE (SKIN) TEMPERATURE.
         IF (IGET(026).GT.0) THEN
            ID(1:25) = 0
            if(grib == 'grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = tsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(026),LVLS(1,IGET(026)), grid1,IM,JM)
            elseif(grib == 'grib2') then
             cfld = cfld+1
             fld_info(cfld)%ifld = IAVBLFLD(IGET(026))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = TSFC(i,jj)
               enddo
             enddo
            endif
         ENDIF
         if (allocated(tsfc)) deallocate(tsfc)
!     
!        SURFACE (SKIN) POTENTIAL TEMPERATURE.
         IF (IGET(027).GT.0) THEN
            ID(1:25) = 0
            if(grib=='grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = thsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(027),LVLS(1,IGET(027)), grid1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(027))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = THSFC(i,jj)
               enddo
             enddo
            endif
         ENDIF
         if (allocated(thsfc)) deallocate(thsfc)
!     
!        SURFACE SPECIFIC HUMIDITY.
         IF (IGET(028).GT.0) THEN
            CALL BOUND(GRID1,H1M12,H99999)
            ID(1:25) = 0
            if(grib=='grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = qsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(028),LVLS(1,IGET(028)), grid1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(028))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = QSFC(i,jj)
               enddo
             enddo
            endif
         ENDIF
         if (allocated(qsfc)) deallocate(qsfc)
!     
!        SURFACE DEWPOINT TEMPERATURE.
         IF (IGET(029).GT.0) THEN
            allocate(dwpsfc(im,jsta:jend))
            CALL DEWPOINT(EVP,DWPSFC)
            ID(1:25) = 0
            if(grib=='grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = dwpsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(029),LVLS(1,IGET(029)), grid1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(029))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = DWPSFC(i,jj)
               enddo
             enddo
            endif
            if (allocated(dwpsfc)) deallocate(dwpsfc)
         ENDIF
!     
!        SURFACE RELATIVE HUMIDITY.
         IF (IGET(076).GT.0) THEN
            CALL BOUND(RHSFC,H1,H100)
            ID(1:25) = 0
            if(grib=='grib1') then
!$omp parallel do private(i,j)
             do j=jsta,jend
               do i=1,im
                 grid1(i,j) = rhsfc(i,j)
               enddo
             enddo
             CALL GRIBIT(IGET(076),LVLS(1,IGET(076)), grid1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(076))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = RHSFC(i,jj)
               enddo
             enddo
            endif
         ENDIF
        if (allocated(rhsfc)) deallocate(rhsfc)
!     
      ENDIF

!     ADDITIONAL SURFACE-SOIL LEVEL FIELDS.
!
!     SURFACE MIXING RATIO
      IF (IGET(762).GT.0) THEN
        ID(1:25) = 0
        if(grib=='grib1') then
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = QVG(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(762),LVLS(1,IGET(762)),  GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(762))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) =  QVG(i,jj)
            enddo
          enddo
        endif
      ENDIF
!    

!     SHELTER MIXING RATIO
      IF (IGET(760).GT.0) THEN
        ID(1:25) = 0
        ISVALUE = 2
        ID(11) = ISVALUE
        if(grib=='grib1') then
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = QV2M(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(760),LVLS(1,IGET(760)), GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(760))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = QV2M(i,jj)
            enddo
          enddo
        endif
      ENDIF

!     SNOW TEMERATURE
      IF (IGET(761).GT.0) THEN
        ID(1:25) = 0
        if(grib=='grib1') then
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = TSNOW(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(761),LVLS(1,IGET(761)),  GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(761))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) =  TSNOW(i,jj)
            enddo
          enddo
        endif
      ENDIF

!     DENSITY OF SNOWFALL
      IF (IGET(724).GT.0) THEN
         ID(1:25) = 0
           if(grib=='grib1') then
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = SNFDEN(I,J)
               ENDDO
             ENDDO
             CALL GRIBIT(IGET(724),LVLS(1,IGET(724)), GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(724))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) =  SNFDEN(i,jj)
               enddo
             enddo
           endif
      ENDIF

!     ACCUMULATED DEPTH OF SNOWFALL
      IF (IGET(725).GT.0) THEN
         ID(1:25) = 0
         if(grib=='grib1') then
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = SNDEPAC(I,J)
              ENDDO
            ENDDO
            CALL GRIBIT(IGET(725),LVLS(1,IGET(725)), GRID1,IM,JM)
         elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(725))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) =  SNDEPAC(i,jj)
              enddo
            enddo
         endif
      ENDIF

!
!     ADDITIONAL SURFACE-SOIL LEVEL FIELDS.
!
!      print *,'in surf,nsoil=',nsoil,'iget(116)=',iget(116),    &
!       'lvls(116)=',LVLS(1:4,IGET(116)),  &
!        'sf_sfc_phys=',iSF_SURFACE_PHYSICS

      DO L=1,NSOIL
        ID(1:25) = 0
!       SOIL TEMPERATURE.
        IF (IGET(116).GT.0) THEN
          IF (LVLS(L,IGET(116)).GT.0) THEN
            IF(iSF_SURFACE_PHYSICS==3)THEN
              ID(9)=111
              IF(L==1)ID(9)=1
              ID(11)=NINT(SLLEVEL(L)*100.)
              if(grib=='grib1') then
!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = STC(I,J,L)
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(116),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
                cfld=cfld+1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(116))
                fld_info(cfld)%lvl=LVLSXML(L,IGET(116))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = STC(i,jj,l)
                  enddo
                enddo
              endif

            ELSE

              DTOP = 0.
              DO LS=1,L-1
                DTOP = DTOP + SLDPTH(LS)
              ENDDO
              DBOT = DTOP + SLDPTH(L)
              ID(10) = NINT(DTOP*100.)
              ID(11) = NINT(DBOT*100.)
              if(grib=='grib1') then
!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = STC(I,J,L)
!                   IF(SM(I,J)>=1.0 .OR. SICE(I,J)>SMALL)
!     +              GRID1(I,J)=SPVAL
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(116),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
                cfld=cfld+1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(116))
                fld_info(cfld)%lvl=LVLSXML(L,IGET(116))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = STC(i,jj,l)
                  enddo
                enddo
              endif

            ENDIF
          ENDIF
        ENDIF
!
!     SOIL MOISTURE.
        IF (IGET(117).GT.0) THEN
          IF (LVLS(L,IGET(117)).GT.0) THEN
            IF(iSF_SURFACE_PHYSICS==3)THEN
              ID(9)=111
              IF(L==1)ID(9)=1
              ID(11)=NINT(SLLEVEL(L)*100.)
              if(grib=='grib1') then
!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = SMC(I,J,L)
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(117),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
                cfld=cfld+1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(117))
                fld_info(cfld)%lvl=LVLSXML(L,IGET(117))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = SMC(i,jj,l)
                  enddo
                enddo
              endif
            ELSE
              DTOP = 0.
              DO LS=1,L-1
                DTOP = DTOP + SLDPTH(LS)
              ENDDO
              DBOT = DTOP + SLDPTH(L)
              ID(10) = NINT(DTOP*100.)
              ID(11) = NINT(DBOT*100.)
              If(grib=='grib1') then
!!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = SMC(I,J,L)
!                   IF(SM(I,J)>=1.0 .OR. SICE(I,J)>SMALL)
!     +                       GRID1(I,J)=SPVAL
                    IF(GRID1(I,J)==spval .and.        &
                       (sm(i,j)<1.0.and.sice(i,j)<1.0e-10))  &
                    print*,'bad smc land point ',i,j
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(117),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
                cfld=cfld+1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(117))
                fld_info(cfld)%lvl=LVLSXML(L,IGET(117))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = SMC(i,jj,l)
                  enddo
                enddo
              endif
            ENDIF
          ENDIF
        ENDIF
!     ADD LIQUID SOIL MOISTURE
        IF (IGET(225).GT.0) THEN
          IF (LVLS(L,IGET(225)).GT.0) THEN
            IF(iSF_SURFACE_PHYSICS==3)THEN
              ID(02) = 130
              ID(9)=111
              IF(L==1)ID(9)=1
              ID(11)=NINT(SLLEVEL(L)*100.)
              If(grib=='grib1') then
!!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = SH2O(I,J,L)
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(225),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(225))
               fld_info(cfld)%lvl=LVLSXML(L,IGET(225))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = SH2O(i,jj,l)
                 enddo
               enddo
              endif
            ELSE
              DTOP = 0.
              DO LS=1,L-1
                DTOP = DTOP + SLDPTH(LS)
              ENDDO
              DBOT = DTOP + SLDPTH(L)
              ID(10) = NINT(DTOP*100.)
              ID(11) = NINT(DBOT*100.)
              ID(02) = 130
              If(grib=='grib1') then
!!$omp parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = SH2O(I,J,L)
!                   IF(SM(I,J)>=1.0 .OR. SICE(I,J)>SMALL)
!     +             GRID1(I,J)=SPVAL
                  ENDDO
                ENDDO
                CALL GRIBIT(IGET(225),L,GRID1,IM,JM)
              elseif(grib=='grib2') then
                cfld=cfld+1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(225))
                fld_info(cfld)%lvl=LVLSXML(L,IGET(225))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = SH2O(i,jj,l)
                  enddo
                enddo
              endif
            ENDIF
          ENDIF
        ENDIF
      ENDDO                   ! END OF NSOIL LOOP
!                               -----------------
!
!     BOTTOM SOIL TEMPERATURE.
      IF (IGET(115).GT.0.or.IGET(571)>0) THEN
        ID(1:25) = 0
        ISVALUE     = 300
        ID(11) = ISVALUE
        if(iget(115)>0) then
          If(grib=='grib1') then
!!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = TG(I,J) !change to tg per discussion with LSM group
              ENDDO
            ENDDO
            CALL GRIBIT(IGET(115),LVLS(1,IGET(115)),GRID1,IM,JM)
          elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(115))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = TG(i,jj)
              enddo
            enddo
          endif
        endif
        if(iget(571)>0.and.grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(571))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = TG(i,jj)
            enddo
          enddo
        endif
      ENDIF
!
!     SOIL MOISTURE AVAILABILITY
      IF (IGET(171).GT.0) THEN
!!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IF(SMSTAV(I,J) /= SPVAL)THEN
              GRID1(I,J) = SMSTAV(I,J)*100.
            ELSE
              GRID1(I,J) = 0.
            ENDIF
          ENDDO
        ENDDO
        ID(1:25) = 0
        ID(10) = 0
        ID(11) = 100
        If(grib=='grib1') then
          CALL GRIBIT(IGET(171),LVLS(1,IGET(171)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(171))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF
!
!     TOTAL SOIL MOISTURE
      IF (IGET(036).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             IF(SMSTOT(I,J)/=SPVAL) THEN
               IF(SM(I,J) > SMALL .AND. SICE(I,J) < SMALL) THEN
                 GRID1(I,J) = 1000.0  ! TEMPORY FIX TO MAKE SURE SMSTOT=1 FOR WATER
               ELSE  
                 GRID1(I,J) = SMSTOT(I,J)
               END IF 
             ELSE
               GRID1(I,J) = 1000.0
             ENDIF
           ENDDO
         ENDDO
         ID(1:25) = 0
         ID(10) = 0
         ID(11) = 200
         If(grib=='grib1') then
          CALL GRIBIT(IGET(036),LVLS(1,IGET(036)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(036))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
         endif
      ENDIF
!
!     PLANT CANOPY SURFACE WATER.
      IF ( IGET(118).GT.0 ) THEN
        IF(MODELNAME == 'RAPR') THEN
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              IF(CMC(I,J) /= SPVAL) then
                GRID1(I,J) = CMC(I,J)
              else
                GRID1(I,J) = spval
              endif
            ENDDO
          ENDDO
        else
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              IF(CMC(I,J) /= SPVAL) then
                GRID1(I,J) = CMC(I,J)*1000.
              else
                GRID1(I,J) = spval
              endif
            ENDDO
          ENDDO
        endif
         ID(1:25) = 0
         If(grib=='grib1') then
          CALL GRIBIT(IGET(118),LVLS(1,IGET(118)),GRID1,IM,JM)
         elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(118))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
         endif
      ENDIF
!
!     SNOW WATER EQUIVALENT.
      IF ( IGET(119).GT.0 ) THEN
!       GRID1 = SPVAL
        ID(1:25) = 0
        If(grib=='grib1') then
          DO J=JSTA,JEND
            DO I=1,IM
!             GRID1(I,J)=SNO(I,J)*1000.
              GRID1(I,J) = SNO(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(119),LVLS(1,IGET(119)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(119))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = SNO(i,jj)
            enddo
          enddo
        endiF
      ENDIF
!
!     Time averaged percent SNOW COVER (for AQ)
      IF ( IGET(500).GT.0 ) THEN
!       GRID1=SPVAL
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = 100.*SNOAVG(I,J)
          ENDDO
        ENDDO
        CALL BOUND(GRID1,D00,H100)
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
        If(grib=='grib1') then
           CALL GRIBIT(IGET(500),LVLS(1,IGET(500)),GRID1,IM,JM)
        elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(500))
           if(ITSRFC>0) then
            fld_info(cfld)%ntrange=1
           else
            fld_info(cfld)%ntrange=0
           endif
           fld_info(cfld)%tinvstat=IFHR-ID(18)
          ! fld_info(cfld)%ntrange=IFHR-ID(18)
          ! fld_info(cfld)%tinvstat=1
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
        endif
      ENDIF

!     Time averaged surface pressure (for AQ)
      IF ( IGET(501).GT.0 ) THEN
!       GRID1 = SPVAL
        ID(1:25) = 0
        ID(19)     = IFHR
        IF (IFHR.EQ.0) THEN
          ID(18) = 0
        ELSE
          ID(18) = IFHR - 1
        ENDIF
        ID(20)     = 3
        If(grib=='grib1') then
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = PSFCAVG(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(501),LVLS(1,IGET(501)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(501))
          fld_info(cfld)%ntrange=IFHR-ID(18)
          fld_info(cfld)%tinvstat=1
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = PSFCAVG(i,jj)
            enddo
          enddo
        endif
      ENDIF

!     Time averaged 10 m temperature (for AQ)
      IF ( IGET(502).GT.0 ) THEN
!       GRID1 = SPVAL
        ID(1:25) = 0
        ID(19)     = IFHR
        IF (IFHR.EQ.0) THEN
          ID(18) = 0
        ELSE
          ID(18) = IFHR - 1
        ENDIF
        ID(20)     = 3
        ISVALUE = 10
        ID(10) = MOD(ISVALUE/256,256)
        ID(11) = MOD(ISVALUE,256)
        If(grib=='grib1') then
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = T10AVG(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(502),LVLS(1,IGET(502)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(502))
          fld_info(cfld)%ntrange=IFHR-ID(18)
          fld_info(cfld)%tinvstat=1
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = T10AVG(i,jj)
            enddo
          enddo
        endif
      ENDIF
!
!     ACM GRID SCALE SNOW AND ICE
      IF ( IGET(244).GT.0 ) THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = SNONC(I,J)
          ENDDO
        ENDDO
        ID(1:25) = 0
        ITPREC     = NINT(TPREC)
!mp
        if (ITPREC .ne. 0) then
          IFINCR     = MOD(IFHR,ITPREC)
          IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
        else
          IFINCR     = 0
        endif
!mp
        ID(18)     = 0
        ID(19)     = IFHR
        IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
        ID(20)     = 4
        IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
        ELSE
          ID(18) = IFHR-IFINCR
          IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
        ENDIF
        IF (ID(18).LT.0) ID(18) = 0

        CALL GRIBIT(IGET(244),LVLS(1,IGET(244)),GRID1,IM,JM)
      ENDIF
!
!     PERCENT SNOW COVER.
      IF ( IGET(120).GT.0 ) THEN
        GRID1=SPVAL
        DO J=JSTA,JEND
          DO I=1,IM
!           GRID1(I,J)=PCTSNO(I,J)
            SNEQV = SNO(I,J)
            IVEG  = IVGTYP(I,J)
            IF(IVEG.EQ.0)IVEG=7
            CALL SNFRAC (SNEQV,IVEG,SNCOVR)
            GRID1(I,J) = SNCOVR*100.
          ENDDO
        ENDDO
        CALL BOUND(GRID1,D00,H100)
        ID(1:25) = 0
        If(grib=='grib1') then
          CALL GRIBIT(IGET(120),LVLS(1,IGET(120)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(120))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF
! ADD SNOW DEPTH
      IF ( IGET(224).GT.0 ) THEN
        ii = im/2
        jj = (jsta+jend)/2
!       GRID1=SPVAL
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = SPVAL
            IF(SI(I,J) /= SPVAL) GRID1(I,J) = SI(I,J)*0.001  ! SI comes out of WRF in mm
          ENDDO
        ENDDO
!        print*,'sample snow depth in GRIBIT= ',si(ii,jj)
        ID(1:25) = 0
        If(grib=='grib1') then
          CALL GRIBIT(IGET(224),LVLS(1,IGET(224)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(224))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF      
! ADD POTENTIAL EVAPORATION
      IF ( IGET(242).GT.0 ) THEN
        ID(1:25) = 0
        If(grib=='grib1') then
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = POTEVP(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(242),LVLS(1,IGET(242)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(242))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = POTEVP(i,jj)
            enddo
          enddo
        endif
      ENDIF
! ADD ICE THICKNESS
      IF ( IGET(349).GT.0 ) THEN
        ID(1:25) = 0
        If(grib=='grib1') then
!$omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = DZICE(I,J)
            ENDDO
          ENDDO
          CALL GRIBIT(IGET(349),LVLS(1,IGET(349)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(349))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = DZICE(i,jj)
            enddo
          enddo
        endif
      ENDIF      

! ADD EC,EDIR,ETRANS,ESNOW,SMCDRY,SMCMAX
! ONLY OUTPUT NEW LSM FIELDS FOR NMM AND ARW BECAUSE RSM USES OLD SOIL TYPES
      IF (MODELNAME == 'NCAR'.OR. MODELNAME == 'NMM'                  &
                             .OR. MODELNAME == 'RAPR') THEN
!       write(0,*)'in surf,isltyp=',maxval(isltyp(1:im,jsta:jend)),   &
!         minval(isltyp(1:im,jsta:jend)),'qwbs=',maxval(qwbs(1:im,jsta:jend)), &
!         minval(qwbs(1:im,jsta:jend)),'potsvp=',maxval(potevp(1:im,jsta:jend)), &
!         minval(potevp(1:im,jsta:jend)),'sno=',maxval(sno(1:im,jsta:jend)), &
!         minval(sno(1:im,jsta:jend)),'vegfrc=',maxval(vegfrc(1:im,jsta:jend)), &
!         minval(vegfrc(1:im,jsta:jend)), 'sh2o=',maxval(sh2o(1:im,jsta:jend,1)), &
!         minval(sh2o(1:im,jsta:jend,1)),'cmc=',maxval(cmc(1:im,jsta:jend)), &
!         minval(cmc(1:im,jsta:jend))
        IF ( IGET(228).GT.0 .OR. IGET(229).GT.0      &
         .OR.IGET(230).GT.0 .OR. IGET(231).GT.0      &
         .OR.IGET(232).GT.0 .OR. IGET(233).GT.0) THEN

          allocate(smcdry(im,jsta:jend), &
                   smcmax(im,jsta:jend))
          DO J=JSTA,JEND
            DO I=1,IM
! ----------------------------------------------------------------------
!             IF(QWBS(I,J).gt.0.001)print*,'NONZERO QWBS',i,j,QWBS(I,J)
!             IF(abs(SM(I,J)-0.).lt.1.0E-5)THEN
! WRF ARW has no POTEVP field. So has to block out RAPR
              IF( (MODELNAME.NE.'RAPR') .AND. (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.   &
     &        (abs(SICE(I,J)-0.) .lt. 1.0E-5) ) THEN
                 CALL ETCALC(QWBS(I,J),POTEVP(I,J),SNO(I,J),VEGFRC(I,J) &
     &                ,  ISLTYP(I,J),SH2O(I,J,1:1),CMC(I,J)         &
     &                ,  ECAN(I,J),EDIR(I,J),ETRANS(I,J),ESNOW(I,J) &
     &                ,  SMCDRY(I,J),SMCMAX(I,J) )
              ELSE
                ECAN(I,J)   = 0.
                EDIR(I,J)   = 0.
                ETRANS(I,J) = 0.
                ESNOW(I,J)  = 0.
                SMCDRY(I,J) = 0.
                SMCMAX(I,J) = 0.
              ENDIF
            ENDDO
          ENDDO

          IF ( IGET(228).GT.0 )THEN
            ID(1:25) = 0
            If(grib=='grib1') then
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = ecan(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(228),LVLS(1,IGET(228)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(228))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = ECAN(i,jj)
                enddo
              enddo
            endiF
          ENDIF

          IF ( IGET(229).GT.0 )THEN
            If(grib=='grib1') then
              ID(1:25) = 0
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = edir(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(229),LVLS(1,IGET(229)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(229))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = EDIR(i,jj)
                enddo
              enddo
            endif
          ENDIF

          IF ( IGET(230).GT.0 )THEN
            If(grib=='grib1') then
              ID(1:25) = 0
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = etrans(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(230),LVLS(1,IGET(230)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(230))
              datapd(1:im,1:jend-jsta+1,cfld) = ETRANS(1:im,jsta:jend)
            endif
          ENDIF

          IF ( IGET(231).GT.0 )THEN
            If(grib=='grib1') then
               ID(1:25) = 0
               ID(02)= 130
!$omp parallel do private(i,j)
               do j=jsta,jend
                 do i=1,im
                   grid1(i,j) = esnow(i,j)
                 enddo
               enddo
              CALL GRIBIT(IGET(231),LVLS(1,IGET(231)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(231))
              datapd(1:im,1:jend-jsta+1,cfld) = ESNOW(1:im,jsta:jend)
            endif
          ENDIF

          IF ( IGET(232).GT.0 )THEN
            If(grib=='grib1') then
              ID(1:25) = 0
              ID(02)= 130
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = smcdry(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(232),LVLS(1,IGET(232)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(232))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = SMCDRY(i,jj)
                enddo
              enddo
            endif
          ENDIF

          IF ( IGET(233).GT.0 )THEN
            If(grib=='grib1') then
              ID(1:25) = 0
              ID(02)= 130
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = smcmax(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(233),LVLS(1,IGET(233)),grid1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(233))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                 jj = jsta+j-1
                do i=1,im
                 datapd(i,j,cfld) = SMCMAX(i,jj)
                enddo
              enddo
            endif
          ENDIF

        ENDIF
!        if (allocated(ecan))   deallocate(ecan)
!        if (allocated(edir))   deallocate(edir)
!        if (allocated(etrans)) deallocate(etrans)
!        if (allocated(esnow))  deallocate(esnow)
        if (allocated(smcdry)) deallocate(smcdry)
        if (allocated(smcmax)) deallocate(smcmax)

      END IF  ! endif for ncar and nmm options

      IF ( IGET(512).GT.0 )THEN
          ID(1:25) = 0
          If(grib=='grib1') then
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = acond(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(512),LVLS(1,IGET(512)),grid1,IM,JM)
          elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(512))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = acond(i,jj)
                enddo
              enddo
          endiF
      ENDIF

      IF ( IGET(513).GT.0 )THEN
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
          If(grib=='grib1') then
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = avgecan(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(513),LVLS(1,IGET(513)),grid1,IM,JM)
          elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(513))
              if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
              else
               fld_info(cfld)%ntrange=0
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = avgECAN(i,jj)
                enddo
              enddo
          endiF
      ENDIF

      IF ( IGET(514).GT.0 )THEN
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
          If(grib=='grib1') then
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = avgedir(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(514),LVLS(1,IGET(514)),grid1,IM,JM)
          elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(514))
              if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
              else
               fld_info(cfld)%ntrange=0
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = avgEDIR(i,jj)
                enddo
              enddo
          endif
      ENDIF

      IF ( IGET(515).GT.0 )THEN
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
          If(grib=='grib1') then
!$omp parallel do private(i,j)
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = avgetrans(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(515),LVLS(1,IGET(515)),grid1,IM,JM)
          elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(515))
              if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
              else
               fld_info(cfld)%ntrange=0
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
              datapd(1:im,1:jend-jsta+1,cfld) = avgETRANS(1:im,jsta:jend)
          endif
      ENDIF

      IF ( IGET(516).GT.0 )THEN
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
          If(grib=='grib1') then
               ID(02)= 130
!$omp parallel do private(i,j)
               do j=jsta,jend
                 do i=1,im
                   grid1(i,j) = avgesnow(i,j)
                 enddo
               enddo
              CALL GRIBIT(IGET(516),LVLS(1,IGET(516)),grid1,IM,JM)
          elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(516))
               if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
              else
               fld_info(cfld)%ntrange=0
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
              datapd(1:im,1:jend-jsta+1,cfld) = avgESNOW(1:im,jsta:jend)
          endif
      ENDIF

!
!     
!
!***  BLOCK 2.  SHELTER (2M) LEVEL FIELDS.
!     
!     COMPUTE/POST SHELTER LEVEL FIELDS.
!     
      IF ( (IGET(106).GT.0).OR.(IGET(112).GT.0).OR.     &
           (IGET(113).GT.0).OR.(IGET(114).GT.0).OR.     &
           (IGET(138).GT.0).OR.(IGET(414).GT.0).OR.     &
           (IGET(546).GT.0).OR.                         &
           (IGET(547).GT.0).OR.(IGET(548).GT.0).OR.     &
           (IGET(771).GT.0)) THEN
!
!HC  COMPUTE SHELTER PRESSURE BECAUSE IT WAS NOT OUTPUT FROM WRF       
        IF(MODELNAME .EQ. 'NCAR' .OR. MODELNAME.EQ.'RSM'.OR. MODELNAME.EQ.'RAPR')THEN
          DO J=JSTA,JEND
            DO I=1,IM
              TLOW        = T(I,J,NINT(LMH(I,J)))
              PSFC(I,J)   = PINT(I,J,NINT(LMH(I,J))+1)   !May not have been set above
              PSHLTR(I,J) = PSFC(I,J)*EXP(-0.068283/TLOW)
            ENDDO
          ENDDO 
        ENDIF 
!
!        print *,'in, surfc,pshltr=',maxval(PSHLTR(1:im,jsta:jend)),  &
!           minval(PSHLTR(1:im,jsta:jend)),PSHLTR(1:3,jsta),'capa=',capa, &
!           'tshlter=',tshltr(1:3,jsta:jsta+2),'psfc=',psfc(1:3,jsta:jsta+2), &
!           'th10=',th10(1:3,jsta:jsta+2),'thz0=',thz0(1:3,jsta:jsta+2)
!
!        SHELTER LEVEL TEMPERATURE
        IF (IGET(106).GT.0) THEN
!          GRID1=spval
           DO J=JSTA,JEND
             DO I=1,IM
!              GRID1(I,J)=TSHLTR(I,J)
!HC CONVERT FROM THETA TO T 
               if(tshltr(i,j)/=spval)GRID1(I,J)=TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
               IF(GRID1(I,J).LT.200)PRINT*,'ABNORMAL 2MT ',i,j,  &
                   TSHLTR(I,J),PSHLTR(I,J)
!                  TSHLTR(I,J)=GRID1(I,J) 
             ENDDO
           ENDDO
!          print *,'2m tmp=',maxval(TSHLTR(1:im,jsta:jend)), &
!             minval(TSHLTR(1:im,jsta:jend)),TSHLTR(1:3,jsta),'grd=',grid1(1:3,jsta)
          if(grib=='grib1') then
            ID(1:25) = 0
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL GRIBIT(IGET(106),LVLS(1,IGET(106)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(106))
            datapd(1:im,1:jend-jsta+1,cfld) = GRID1(1:im,jsta:jend)
           endif
        ENDIF
!
!        SHELTER LEVEL POT TEMP
        IF (IGET(546).GT.0) THEN
!          GRID1=spval
!          DO J=JSTA,JEND
!            DO I=1,IM
!              GRID1(I,J)=TSHLTR(I,J)
!            ENDDO
!          ENDDO
           if(grib=='grib1') then
             ID(1:25) = 0
             ISVALUE = 2
             ID(10) = MOD(ISVALUE/256,256)
             ID(11) = MOD(ISVALUE,256)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=TSHLTR(I,J)
               ENDDO
             ENDDO
             CALL GRIBIT(IGET(546),LVLS(1,IGET(546)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(546))
             datapd(1:im,1:jend-jsta+1,cfld) = TSHLTR(1:im,jsta:jend)
           endif
        ENDIF
!
!        SHELTER LEVEL SPECIFIC HUMIDITY.
        IF (IGET(112).GT.0) THEN       
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = QSHLTR(I,J)
             ENDDO
           ENDDO
           CALL BOUND (GRID1,H1M12,H99999)
           if(grib=='grib1') then
             ID(1:25) = 0
             ISVALUE = 2
             ID(10) = MOD(ISVALUE/256,256)
             ID(11) = MOD(ISVALUE,256)
             CALL GRIBIT(IGET(112),LVLS(1,IGET(112)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(112))
             datapd(1:im,1:jend-jsta+1,cfld) = GRID1(1:im,jsta:jend)
           endif
        ENDIF
!     
!        SHELTER MIXING RATIO.
        IF (IGET(414).GT.0) THEN
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = MRSHLTR(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ISVALUE = 2
           ID(10) = MOD(ISVALUE/256,256)
           ID(11) = MOD(ISVALUE,256)
           CALL GRIBIT(IGET(414),LVLS(1,IGET(414)),GRID1,IM,JM)
        ENDIF
!
!        SHELTER LEVEL DEWPOINT, DEWPOINT DEPRESSION AND SFC EQUIV POT TEMP.
           allocate(p1d(im,jsta:jend), t1d(im,jsta:jend))
        IF ((IGET(113).GT.0) .OR.(IGET(547).GT.0).OR.(IGET(548).GT.0)) THEN

           DO J=JSTA,JEND
             DO I=1,IM

!tgs The next 4 lines are GSD algorithm for Dew Point computation
!tgs Results are very close to dew point computed in DEWPOINT subroutine
               qv   = max(1.E-5,(QSHLTR(I,J)/(1.-QSHLTR(I,J))))
               e    = PSHLTR(I,J)/100.*qv/(0.62197+qv)
               DWPT = (243.5*LOG(E)-440.8)/(19.48-LOG(E))+273.15

!              if(i.eq.335.and.j.eq.295)print*,'Debug: RUC-type DEWPT,i,j'  &
!              if(i.eq.ii.and.j.eq.jj)print*,'Debug: RUC-type DEWPT,i,j'
!              ,   DWPT,i,j,qv,pshltr(i,j),qshltr(i,j)

!              EGRID1(I,J) = DWPT

               EVP(I,J) = PSHLTR(I,J)*QSHLTR(I,J)/(EPS+ONEPS*QSHLTR(I,J))
               EVP(I,J) = EVP(I,J)*D001
             ENDDO
           ENDDO
           CALL DEWPOINT(EVP,EGRID1(1,jsta))
!      print *,' MAX DEWPOINT',maxval(egrid1)
! DEWPOINT
           IF (IGET(113).GT.0) THEN
             GRID1=spval
             if(MODELNAME.EQ.'RAPR')THEN
               DO J=JSTA,JEND
               DO I=1,IM
! DEWPOINT can't be higher than T2
                t2=TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
                if(qshltr(i,j)/=spval)GRID1(I,J)=min(EGRID1(I,J),T2)
               ENDDO
               ENDDO
             else
               DO J=JSTA,JEND
               DO I=1,IM
                if(qshltr(i,j)/=spval) GRID1(I,J) = EGRID1(I,J)
               ENDDO
               ENDDO
             endif
             if(grib=='grib1') then
               ID(1:25) = 0
               ISVALUE = 2
               ID(10) = MOD(ISVALUE/256,256)
               ID(11) = MOD(ISVALUE,256)
               CALL GRIBIT(IGET(113),LVLS(1,IGET(113)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(113))
                datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
             endif
           ENDIF


!-------------------------------------------------------------------------
! DEWPOINT at level 1   ------ p1d and t1d are  undefined !! -- Moorthi
           IF (IGET(771).GT.0) THEN
             DO J=JSTA,JEND
               DO I=1,IM
                 EVP(I,J)=P1D(I,J)*QVl1(I,J)/(EPS+ONEPS*QVl1(I,J))
                 EVP(I,J)=EVP(I,J)*D001
               ENDDO
             ENDDO
             CALL DEWPOINT(EVP,EGRID1(1,jsta))
!             print *,' MAX DEWPOINT at level 1',maxval(egrid1)
             GRID1=spval
             DO J=JSTA,JEND
               DO I=1,IM
!tgs 30 dec 2013 - 1st leel dewpoint can't be higher than 1-st level temperature
                 if(qvl1(i,j)/=spval)GRID1(I,J) = min(EGRID1(I,J),T1D(I,J))
               ENDDO
             ENDDO
             if(grib=='grib1') then
               ID(1:25) = 0
               ISVALUE = 2
               ID(10) = MOD(ISVALUE/256,256)
               ID(11) = MOD(ISVALUE,256)
               CALL GRIBIT(IGET(771),LVLS(1,IGET(771)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(771))
               datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
             endif
           ENDIF
!-------------------------------------------------------------------------

!
           IF ((IGET(547).GT.0).OR.(IGET(548).GT.0)) THEN
            DO J=JSTA,JEND
            DO I=1,IM
! DEWPOINT DEPRESSION in GRID1
             GRID1(i,j)=max(0.,TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA-EGRID1(i,j))

! SURFACE EQIV POT TEMP in GRID2
             APE=(H10E5/PSHLTR(I,J))**CAPA
             GRID2(I,J)=TSHLTR(I,J)*EXP(ELOCP*QSHLTR(I,J)*APE/TSHLTR(I,J))
            ENDDO
            ENDDO
       print *,' MAX/MIN --> DEWPOINT DEPRESSION',maxval(grid1(1:im,jsta:jend)),&
                                                  minval(grid1(1:im,jsta:jend))
       print *,' MAX/MIN -->  SFC EQUIV POT TEMP',maxval(grid2(1:im,jsta:jend)),&
                                                  minval(grid2(1:im,jsta:jend))

             IF (IGET(547).GT.0) THEN
               if(grib=='grib1') then
                 ID(1:25) = 0
                 ISVALUE = 2
                 ID(10) = MOD(ISVALUE/256,256)
                 ID(11) = MOD(ISVALUE,256)
                 CALL GRIBIT(IGET(547),LVLS(1,IGET(547)),GRID1,IM,JM)
               elseif(grib=='grib2') then
                 cfld=cfld+1
                 fld_info(cfld)%ifld=IAVBLFLD(IGET(547))
                 datapd(1:im,1:jend-jsta+1,cfld)=GRID2(1:im,jsta:jend)
               endif

             ENDIF
             IF (IGET(548).GT.0) THEN
               if(grib=='grib1') then
                 ID(1:25) = 0
                 CALL GRIBIT(IGET(548),LVLS(1,IGET(548)),GRID2,IM,JM)
               elseif(grib=='grib2') then
                 cfld=cfld+1
                 fld_info(cfld)%ifld=IAVBLFLD(IGET(548))
                 datapd(1:im,1:jend-jsta+1,cfld)=GRID2(1:im,jsta:jend)
               endif
             ENDIF
           ENDIF


         ENDIF
!     
!        SHELTER LEVEL RELATIVE HUMIDITY AND APPARENT TEMPERATURE
         IF (IGET(114) > 0 .OR. IGET(808) > 0) THEN
           allocate(q1d(im,jsta:jend))
!$omp parallel do private(i,j,llmh)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(MODELNAME.EQ.'RAPR')THEN
                 LLMH = NINT(LMH(I,J))
!                P1D(I,J)=PINT(I,J,LLMH+1)
                 P1D(I,J) = PMID(I,J,LLMH)
                 T1D(I,J) = T(I,J,LLMH)
               ELSE
                 P1D(I,J) = PSHLTR(I,J)
                 T1D(I,J) = TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
               ENDIF
               Q1D(I,J) = QSHLTR(I,J)
             ENDDO
           ENDDO
!            CALL CALRH(PSHLTR,TSHLTR,QSHLTR,EGRID1)
           IF(MODELNAME == 'GFS')THEN
             CALL CALRH_GFS(P1D,T1D,Q1D,EGRID1(1,jsta))
           ELSEIF(MODELNAME == 'RAPR')THEN
             CALL CALRH_GSD(P1D,T1D,Q1D,EGRID1(1,jsta))
           ELSE
             CALL CALRH(P1D,T1D,Q1D,EGRID1(1,jsta))
           END IF
           if (allocated(q1d)) deallocate(q1d)
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               if(qshltr(i,j) /= spval)then
                 GRID1(I,J) = EGRID1(I,J)*100.
               else
                 grid1(i,j) = spval 
               end if 
             ENDDO
           ENDDO
           CALL BOUND(GRID1,H1,H100)
           IF (IGET(114) > 0) THEN
             if(grib == 'grib1') then
                ID(1:25) = 0
                ISVALUE  = 2
                ID(10)   = MOD(ISVALUE/256,256)
                ID(11)   = MOD(ISVALUE,256)
                CALL GRIBIT(IGET(114),LVLS(1,IGET(114)),GRID1,IM,JM)
             elseif(grib == 'grib2') then
                cfld = cfld+1
                fld_info(cfld)%ifld = IAVBLFLD(IGET(114))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
             endif
           ENDIF

           IF(IGET(808)>0)THEN
!$omp parallel do private(i,j,dum1,dum2,dum3,dum216,dum1s,dum3s)
             DO J=JSTA,JEND
               DO I=1,IM
                 DUM1 = (T1D(I,J)-TFRZ)*1.8+32.
                 DUM2 = SQRT(U10H(I,J)**2.0+V10H(I,J)**2.0)/0.44704
                 DUM3 = EGRID1(I,J) * 100.0
!                if(abs(gdlon(i,j)-120.)<1. .and. abs(gdlat(i,j))<1.)         &
!                  print*,'Debug AT: INPUT', T1D(i,j),dum1,dum2,dum3
                 IF(DUM1 <= 50.) THEN
                   DUM216 = DUM2**0.16
                   GRID2(I,J) = 35.74 + 0.6215*DUM1                           &
                              - 35.75*DUM216 + 0.4275*DUM1*DUM216
                   GRID2(I,J) =(GRID2(I,J)-32.)/1.8+TFRZ
                 ELSE IF(DUM1 > 80.) THEN
                   DUM1S = DUM1*DUM1
                   DUM3S = DUM3*DUM3
                   GRID2(I,J) = -42.379 + 2.04901523*DUM1                     &
                              + 10.14333127*DUM3                              &
                              - 0.22475541*DUM1*DUM3                          &
                              - 0.00683783*DUM1S                              &
                              - 0.05481717*DUM3S                              &
                              + 0.00122874*DUM1S*DUM3                         &
                              + 0.00085282*DUM1*DUM3S                         &
                              - 0.00000199*DUM1S*DUM3S
                   GRID2(I,J) = (GRID2(I,J)-32.)/1.8 + TFRZ
                 ELSE
                   GRID2(I,J) = T1D(I,J)
                 END IF
!                if(abs(gdlon(i,j)-120.)<1. .and. abs(gdlat(i,j))<1.) &
!                 print*,'Debug AT: OUTPUT',Grid2(i,j)
               ENDDO
             ENDDO

             if(grib == 'grib1') then
               ID(1:25) = 0
               ISVALUE  = 2
               ID(10)   = MOD(ISVALUE/256,256)
               ID(11)   = MOD(ISVALUE,256)
               CALL GRIBIT(IGET(808),LVLS(1,IGET(808)),GRID2,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld+1
               fld_info(cfld)%ifld = IAVBLFLD(IGET(808))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID2(i,jj)
                  enddo
                enddo
             endif

           ENDIF !for 808

         ENDIF ! ENDIF for shleter RH or apparent T

         if (allocated(p1d)) deallocate (p1d)
         if (allocated(t1d)) deallocate (t1d)
!     
!        SHELTER LEVEL PRESSURE.
         IF (IGET(138).GT.0) THEN
!          DO J=JSTA,JEND
!            DO I=1,IM
!              GRID1(I,J)=PSHLTR(I,J)
!            ENDDO
!          ENDDO
           if(grib=='grib1') then
             ID(1:25) = 0
             ISVALUE = 2
             ID(10) = MOD(ISVALUE/256,256)
             ID(11) = MOD(ISVALUE,256)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=PSHLTR(I,J)
               ENDDO
             ENDDO
             CALL GRIBIT(IGET(138),LVLS(1,IGET(138)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(138))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = PSHLTR(i,jj)
               enddo
             enddo
           endif
         ENDIF
!
      ENDIF
!
!        SHELTER LEVEL MAX TEMPERATURE.
         IF (IGET(345).GT.0) THEN       
!          DO J=JSTA,JEND
!            DO I=1,IM
!              GRID1(I,J)=MAXTSHLTR(I,J)
!            ENDDO
!          ENDDO
!mp
           TMAXMIN = MAX(TMAXMIN,1.)
!mp
           ITMAXMIN = INT(TMAXMIN)
           IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
	     IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
	   ELSE
	     IFINCR     = 0
           endif
           ID(19)     = IFHR
	   IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 2
           IF (IFINCR.EQ.0) THEN
              ID(18) = IFHR-ITMAXMIN
           ELSE
              ID(18) = IFHR-IFINCR
	      IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
	   if(grib=='grib1') then
             ISVALUE = 2
             ID(10) = MOD(ISVALUE/256,256)
             ID(11) = MOD(ISVALUE,256)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = MAXTSHLTR(I,J)
               ENDDO
             ENDDO
             CALL GRIBIT(IGET(345),LVLS(1,IGET(345)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(345))
             if(ITMAXMIN==0) then
               fld_info(cfld)%ntrange=0
             else
               fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
             if(IFHR==0) fld_info(cfld)%tinvstat=0
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) =  MAXTSHLTR(i,jj)
               enddo
             enddo
           endif
         ENDIF
!
!        SHELTER LEVEL MIN TEMPERATURE.
         IF (IGET(346).GT.0) THEN       
!!$omp parallel do private(i,j)
!          DO J=JSTA,JEND
!            DO I=1,IM
!              GRID1(I,J) = MINTSHLTR(I,J)
!            ENDDO
!          ENDDO
           ID(1:25) = 0
           ITMAXMIN     = INT(TMAXMIN)
           IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
           ELSE
             IFINCR     = 0
           endif
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 2
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITMAXMIN
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
             if(grib=='grib1') then
             ISVALUE = 2
             ID(10) = MOD(ISVALUE/256,256)
             ID(11) = MOD(ISVALUE,256)
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = MINTSHLTR(I,J)
               ENDDO
             ENDDO
             CALL GRIBIT(IGET(346),LVLS(1,IGET(346)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(346))
             if(ITMAXMIN==0) then
               fld_info(cfld)%ntrange=0
             else
               fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
             if(IFHR==0) fld_info(cfld)%tinvstat=0
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = MINTSHLTR(i,jj)
               enddo
             enddo
           endif
         ENDIF
!
!        SHELTER LEVEL MAX RH.
         IF (IGET(347).GT.0) THEN       
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=MAXRHSHLTR(I,J)*100.
            ENDDO
            ENDDO
	    ID(1:25) = 0
	    ID(02)=129
	    ITMAXMIN     = INT(TMAXMIN)
            IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
	     IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
	    ELSE
	     IFINCR     = 0
            endif
            ID(19)     = IFHR
	    IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 2
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITMAXMIN
            ELSE
               ID(18) = IFHR-IFINCR
               IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            if(grib=='grib1') then
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL GRIBIT(IGET(347),LVLS(1,IGET(347)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(347))
            if(ITMAXMIN==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=(IFHR-ID(18))/ITMAXMIN
            endif
            fld_info(cfld)%tinvstat=ITMAXMIN
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
         ENDIF
!
!        SHELTER LEVEL MIN RH.
         IF (IGET(348).GT.0) THEN       
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=MINRHSHLTR(I,J)*100.
            ENDDO
            ENDDO
	    ID(1:25) = 0
	    ID(02)=129
	    ITMAXMIN     = INT(TMAXMIN)
            IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
	     IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
	    ELSE
	     IFINCR     = 0
            endif
            ID(19)     = IFHR
	    IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 2
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITMAXMIN
            ELSE
               ID(18) = IFHR-IFINCR
               IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
           if(grib=='grib1') then
            ISVALUE = 2
            ID(10) = MOD(ISVALUE/256,256)
            ID(11) = MOD(ISVALUE,256)
            CALL GRIBIT(IGET(348),LVLS(1,IGET(348)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(348))
            if(ITMAXMIN==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=(IFHR-ID(18))/ITMAXMIN
            endif
            fld_info(cfld)%tinvstat=ITMAXMIN
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
         ENDIF

!
!        SHELTER LEVEL MAX SPFH 
         IF (IGET(510).GT.0) THEN
            ID(1:25) = 0
            ITMAXMIN     = INT(TMAXMIN)
            IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
            ELSE
             IFINCR     = 0
            endif
            ID(19)     = IFHR
            IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 2
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITMAXMIN
            ELSE
               ID(18) = IFHR-IFINCR
               IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            if(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(510))
              if(ITMAXMIN==0) then
                fld_info(cfld)%ntrange=0
              else
                fld_info(cfld)%ntrange=1
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = maxqshltr(i,jj)
                enddo
              enddo
            endif
         ENDIF
!
!        SHELTER LEVEL MIN SPFH
         IF (IGET(511).GT.0) THEN
            ID(1:25) = 0
            ITMAXMIN     = INT(TMAXMIN)
            IF(ITMAXMIN .ne. 0) then
             IFINCR     = MOD(IFHR,ITMAXMIN)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITMAXMIN*60)
            ELSE
             IFINCR     = 0
            endif
            ID(19)     = IFHR
            IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 2
            IF (IFINCR.EQ.0) THEN
               ID(18) = IFHR-ITMAXMIN
            ELSE
               ID(18) = IFHR-IFINCR
               IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
            if(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(511))
              if(ITMAXMIN==0) then
                fld_info(cfld)%ntrange=0
              else
                fld_info(cfld)%ntrange=1
              endif
              fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = minqshltr(i,jj)
                enddo
              enddo
            endif
         ENDIF
!
!
!     BLOCK 3.  ANEMOMETER LEVEL (10M) WINDS, THETA, AND Q.
!
      IF ( (IGET(064).GT.0).OR.(IGET(065).GT.0).OR. &
           (IGET(506).GT.0).OR.(IGET(507).GT.0)  ) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
!
!        ANEMOMETER LEVEL U WIND AND/OR V WIND.
         IF ((IGET(064).GT.0).OR.(IGET(065).GT.0)) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = U10(I,J)
               GRID2(I,J) = V10(I,J)
             ENDDO
           ENDDO
           if(grib=='grib1') then
             IF (IGET(064).GT.0) CALL GRIBIT(IGET(064),     &
                 LVLS(1,IGET(064)),GRID1,IM,JM)
             IF (IGET(065).GT.0) CALL GRIBIT(IGET(065),     &
                 LVLS(1,IGET(065)),GRID2,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(064))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(065))
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID2(i,jj)
               enddo
             enddo
           endif
         ENDIF
! GSD - Time-averaged wind speed
         IF (IGET(730).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=SPDUV10mean(I,J)
            ENDDO
            ENDDO
           if(grib=='grib1') then
            CALL GRIBIT(IGET(730),LVLS(1,IGET(730)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(730))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF
!---

         IF ((IGET(506).GT.0).OR.(IGET(507).GT.0)) THEN
	    ID(02)=129
         ID(20) = 2
         ID(19) = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = U10MAX(I,J)
                GRID2(I,J) = V10MAX(I,J)
              ENDDO
            ENDDO
            if(grib=='grib1') then
            IF (IGET(506).GT.0) CALL GRIBIT(IGET(506),     &
                 LVLS(1,IGET(506)),GRID1,IM,JM)
            IF (IGET(507).GT.0) CALL GRIBIT(IGET(507),     &
                 LVLS(1,IGET(507)),GRID2,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(506))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(507))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID2(i,jj)
              enddo
            enddo
           endif
         ENDIF

      ENDIF
!
!        ANEMOMETER LEVEL (10 M) POTENTIAL TEMPERATURE.
!   NOT A OUTPUT FROM WRF
      IF (IGET(158).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=TH10(I,J)
           ENDDO
         ENDDO
         if(grib=='grib1') then
           CALL GRIBIT(IGET(158),LVLS(1,IGET(158)),GRID1,IM,JM)
         elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(158))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
         endif
       ENDIF

!        ANEMOMETER LEVEL (10 M) SENSIBLE TEMPERATURE.
!   NOT A OUTPUT FROM WRF
      IF (IGET(505).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=T10M(I,J)
           ENDDO
         ENDDO
         if(grib=='grib1') then
           CALL GRIBIT(IGET(505),LVLS(1,IGET(505)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(505))
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
       ENDIF
!
!        ANEMOMETER LEVEL (10 M) SPECIFIC HUMIDITY.
!
      IF (IGET(159).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = Q10(I,J)
           ENDDO
         ENDDO
         if(grib=='grib1') then
           CALL GRIBIT(IGET(159),LVLS(1,IGET(159)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(159))
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
       ENDIF
!
! SRD
!
!        ANEMOMETER LEVEL (10 M) MAX WIND SPEED.
!
      IF (IGET(422).GT.0) THEN
         ID(1:25) = 0
         ISVALUE = 10
         ID(10) = MOD(ISVALUE/256,256)
         ID(11) = MOD(ISVALUE,256)
         ID(20) = 2
         ID(19) = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = WSPD10MAX(I,J)
           ENDDO
         ENDDO
         if(grib=='grib1') then
           CALL GRIBIT(IGET(422),LVLS(1,IGET(422)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(422))
           if (ifhr.eq.0) then
              fld_info(cfld)%tinvstat=0
           else
              fld_info(cfld)%tinvstat=1
           endif
           fld_info(cfld)%ntrange=1
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
! SRD
!
!
!***  BLOCK 4.  PRECIPITATION RELATED FIELDS.
!MEB 6/17/02  ASSUMING THAT ALL ACCUMULATED FIELDS NEVER EMPTY
!             THEIR BUCKETS.  THIS IS THE EASIEST WAY TO DEAL WITH
!             ACCUMULATED FIELDS.  SHORTER TIME ACCUMULATIONS CAN
!             BE COMPUTED AFTER THE FACT IN A SEPARATE CODE ONCE
!             THE POST HAS FINISHED.  I HAVE LEFT IN THE OLD
!             ETAPOST CODE FOR COMPUTING THE BEGINNING TIME OF
!             THE ACCUMULATION PERIOD IF THIS IS CHANGED BACK
!             TO A 12H OR 3H BUCKET.  I AM NOT SURE WHAT
!             TO DO WITH THE TIME AVERAGED FIELDS, SO
!             LEAVING THAT UNCHANGED.
!     
!     SNOW FRACTION FROM EXPLICIT CLOUD SCHEME.  LABELLED AS
!      'PROB OF FROZEN PRECIP' IN GRIB, 
!      DIDN'T KNOW WHAT ELSE TO CALL IT
      IF (IGET(172).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
              IF (PREC(I,J) .LE. PTHRESH .OR. SR(I,J)==spval) THEN
                GRID1(I,J) = -50.
              ELSE
                GRID1(I,J) = SR(I,J)*100.
              ENDIF
           ENDDO
         ENDDO
         if(grib=='grib1') then
           ID(1:25) = 0
           CALL GRIBIT(IGET(172),LVLS(1,IGET(172)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(172))
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
!     INSTANTANEOUS CONVECTIVE PRECIPITATION RATE.
!     SUBSTITUTE WITH CUPPT IN WRF FOR NOW
      IF (IGET(249).GT.0) THEN
         RDTPHS=1000./DTQ2     !--- 1000 kg/m**3, density of liquid water
!        RDTPHS=1000./(TRDLW*3600.)
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = CPRATE(I,J)*RDTPHS
!             GRID1(I,J) = CUPPT(I,J)*RDTPHS
           ENDDO
         ENDDO
         ID(1:25) = 0
         if(grib=='grib1') then
           CALL GRIBIT(IGET(249),LVLS(1,IGET(249)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(249))
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
!     INSTANTANEOUS PRECIPITATION RATE.
      IF (IGET(167).GT.0) THEN
!MEB need to get physics DT
         RDTPHS=1./(DT * NPHS) 
!MEB need to get physics DT
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             IF(MODELNAME /= 'RSM') THEN
              GRID1(I,J) = PREC(I,J)*RDTPHS*1000.
             ELSE        !Add by Binbin
              GRID1(I,J) = PREC(I,J)
             END IF
           ENDDO
         ENDDO
         if(grib=='grib1') then
           ID(1:25) = 0
           CALL GRIBIT(IGET(167),LVLS(1,IGET(167)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(167))
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
! MAXIMUM INSTANTANEOUS PRECIPITATION RATE.
      IF (IGET(508).GT.0) THEN
!-- PRATE_MAX in units of mm/h from NMMB history files
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=PRATE_MAX(I,J)*SEC2HR
           ENDDO
         ENDDO
         ID(1:25)=0
         ID(20) = 2
         ID(19) = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
         if(grib=='grib1') then
           CALL GRIBIT(IGET(508),LVLS(1,IGET(508)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(508))
           fld_info(cfld)%lvl=LVLSXML(1,IGET(508))
           fld_info(cfld)%tinvstat=1
           if (IFHR .gt. 0) then
             fld_info(cfld)%ntrange=1
           else
             fld_info(cfld)%ntrange=0
           endif
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
! MAXIMUM INSTANTANEOUS *FROZEN* PRECIPITATION RATE.
      IF (IGET(509).GT.0) THEN
!-- FPRATE_MAX in units of mm/h from NMMB history files
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=FPRATE_MAX(I,J)*SEC2HR
           ENDDO
         ENDDO
         ID(1:25)=0
         ID(20) = 2
         ID(19) = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
         if(grib=='grib1') then
           CALL GRIBIT(IGET(509),LVLS(1,IGET(509)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(509))
           fld_info(cfld)%lvl=LVLSXML(1,IGET(509))
           fld_info(cfld)%tinvstat=1
           if (IFHR .gt. 0) then
             fld_info(cfld)%ntrange=1
           else
             fld_info(cfld)%ntrange=0
           endif
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
!     TIME-AVERAGED CONVECTIVE PRECIPITATION RATE.
      IF (IGET(272).GT.0) THEN
         RDTPHS=1000./DTQ2     !--- 1000 kg/m**3, density of liquid water
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	 if (ITPREC .ne. 0) then
          IFINCR     = MOD(IFHR,ITPREC)
	  IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	 else
	  IFINCR     = 0
	 endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
	 IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 3
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
	  IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
	 grid1=spval
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             if(AVGCPRATE(I,J)/=spval) GRID1(I,J) = AVGCPRATE(I,J)*RDTPHS
           ENDDO
         ENDDO
        
!          print *,'in surf,iget(272)=',iget(272),'RDTPHS=',RDTPHS, &
!           'AVGCPRATE=',maxval(AVGCPRATE(1:im,jsta:jend)),minval(AVGCPRATE(1:im,jsta:jend)), &
!           'grid1=',maxval(grid1(1:im,jsta:jend)),minval(grid1(1:im,jsta:jend))
 	  if(grib=='grib1') then
         CALL GRIBIT(IGET(272),LVLS(1,IGET(272)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(272))

            if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=1
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!      
!     TIME-AVERAGED PRECIPITATION RATE.
      IF (IGET(271).GT.0) THEN
         RDTPHS=1000./DTQ2     !--- 1000 kg/m**3, density of liquid water
!         RDTPHS=1000./(TRDLW*3600.)
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	 if (ITPREC .ne. 0) then
          IFINCR     = MOD(IFHR,ITPREC)
	  IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	 else
	  IFINCR     = 0
	 endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
	 IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 3
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
          IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         grid1=spval
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             if(avgprec(i,j)/=spval) GRID1(I,J) = AVGPREC(I,J)*RDTPHS
           ENDDO
         ENDDO
        
         if(grib=='grib1') then
         CALL GRIBIT(IGET(271),LVLS(1,IGET(271)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(271))

            if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=1
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!     
!     ACCUMULATED TOTAL PRECIPITATION.
      IF (IGET(087).GT.0) THEN
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
	 IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
	 IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
	  IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF(MODELNAME == 'GFS') THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(AVGPREC(I,J) < SPVAL)THEN
                 GRID1(I,J) = AVGPREC(I,J)*FLOAT(ID(19)-ID(18))*3600.*1000./DTQ2
               ELSE
                 GRID1(I,J) = SPVAL
               END IF 
             ENDDO
           ENDDO
         ELSE   
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = ACPREC(I,J)*1000.
             ENDDO
           ENDDO
         END IF 
!	 IF(IFMIN .GE. 1 .AND. ID(19) .GT. 256)THEN
!	  IF(ITPREC.EQ.3)ID(17)=10
!	  IF(ITPREC.EQ.6)ID(17)=11
!	  IF(ITPREC.EQ.12)ID(17)=12
!	 END IF 
         IF (ID(18).LT.0) ID(18) = 0
!	write(6,*) 'call gribit...total precip'
         if(grib=='grib1') then
         CALL GRIBIT(IGET(087),LVLS(1,IGET(087)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(087))
            if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=1
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!     
!     ACCUMULATED CONVECTIVE PRECIPITATION.
      IF (IGET(033).GT.0) THEN
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
         IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
	 IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
          IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         IF(MODELNAME == 'GFS') THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(avGCPRATE(I,J) < SPVAL)THEN
                 GRID1(I,J) = AVGCPRATE(I,J)*                      &
                              FLOAT(ID(19)-ID(18))*3600.*1000./DTQ2
               ELSE
                 GRID1(I,J) = SPVAL
               END IF
             ENDDO
           ENDDO
         ELSE
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J) = CUPREC(I,J)*1000.
            ENDDO
           ENDDO
         END IF 
!	write(6,*) 'call gribit...convective precip'
         if(grib=='grib1') then
         CALL GRIBIT(IGET(033),LVLS(1,IGET(033)),GRID1,IM,JM)
          elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(033))
            if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!     
!     ACCUMULATED GRID-SCALE PRECIPITATION.
      IF (IGET(034).GT.0) THEN
            
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
         IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
	 IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
          IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         IF(MODELNAME == 'GFS') THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(AVGCPRATE(I,J) < SPVAL .AND. AVGPREC(I,J) < SPVAL) then
                 GRID1(I,J) = ( AVGPREC(I,J) - AVGCPRATE(I,J) ) *          &
                                FLOAT(ID(19)-ID(18))*3600.*1000./DTQ2
               ELSE
                 GRID1(I,J) = SPVAL
               END IF 
             ENDDO
           ENDDO
         ELSE
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = ANCPRC(I,J)*1000.
             ENDDO
            ENDDO
         END IF  
!	write(6,*) 'call gribit...grid-scale precip'
         if(grib=='grib1') then
         CALL GRIBIT(IGET(034),LVLS(1,IGET(034)), GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(034))
            if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!     
!     ACCUMULATED LAND SURFACE PRECIPITATION.
      IF (IGET(256).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
            IF(LSPA(I,J)<=-1.0E-6)THEN
             GRID1(I,J) = ACPREC(I,J)*1000
            ELSE
             GRID1(I,J) = LSPA(I,J)*1000.
            END IF
           ENDDO
         ENDDO
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
         IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
         IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
          ID(18) = IFHR-ITPREC
         ELSE
          ID(18) = IFHR-IFINCR
          IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
         ID(02)= 130
         if(grib=='grib1') then
         CALL GRIBIT(IGET(256),LVLS(1,IGET(256)), GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(256))
            if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
      ENDIF
!     
!     ACCUMULATED SNOWFALL.
      IF (IGET(035).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
!            GRID1(I,J) = ACSNOW(I,J)*1000.
             GRID1(I,J) = ACSNOW(I,J)
           ENDDO
         ENDDO
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
         if (ITPREC .ne. 0) then
           IFINCR     = MOD(IFHR,ITPREC)
           IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
         else
           IFINCR     = 0
         endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
         IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
         ID(20)     = 4
         IF (IFINCR.EQ.0) THEN
           ID(18) = IFHR-ITPREC
         ELSE
           ID(18) = IFHR-IFINCR
           IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
         ENDIF
         IF (ID(18).LT.0) ID(18) = 0
        if(grib=='grib1') then
          CALL GRIBIT(IGET(035),LVLS(1,IGET(035)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(035))
          if(ITPREC>0) then
             fld_info(cfld)%ntrange=1
          else
             fld_info(cfld)%ntrange=0
          endif
          fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF
!     
!     ACCUMULATED SNOW MELT.
      IF (IGET(121).GT.0) THEN
!$omp parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
!            GRID1(I,J) = ACSNOM(I,J)*1000.
             GRID1(I,J) = ACSNOM(I,J)     
           ENDDO
         ENDDO
         ID(1:25) = 0
         ITPREC     = NINT(TPREC)
!mp
         if (ITPREC .ne. 0) then
           IFINCR     = MOD(IFHR,ITPREC)
           IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
         else
           IFINCR     = 0
         endif
!mp
         ID(18)     = 0
         ID(19)     = IFHR
         IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           if(grib=='grib1') then
             CALL GRIBIT(IGET(121),LVLS(1,IGET(121)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(121))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!
!     ACCUMULATED SNOWFALL RATE
         IF (IGET(405).GT.0) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = SNOWFALL(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           IF(ITPREC < 0)ID(1:25)=0
           if(grib=='grib1') then
             CALL GRIBIT(IGET(405),LVLS(1,IGET(405)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(405))
             if(ITPREC>0) then
                fld_info(cfld)%ntrange=1
             else
                fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     
!     ACCUMULATED STORM SURFACE RUNOFF.
         IF (IGET(122).GT.0) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
!              GRID1(I,J) = SSROFF(I,J)*1000.
               GRID1(I,J) = SSROFF(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
!    1-HR RUNOFF ACCUMULATIONS IN RR
           IF (MODELNAME.EQ.'RAPR')  THEN
             IF (IFHR .GT. 0) THEN
               ID(18)=IFHR-1
             ELSE
               ID(18)=0
             ENDIF
           ENDIF
           if(grib=='grib1') then
             CALL GRIBIT(IGET(122),LVLS(1,IGET(122)), GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(122))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     
!     ACCUMULATED BASEFLOW-GROUNDWATER RUNOFF.
         IF (IGET(123).GT.0) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
!              GRID1(I,J) = BGROFF(I,J)*1000.
               GRID1(I,J) = BGROFF(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           ID(18)     = IFHR - 1 
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
!    1-HR RUNOFF ACCUMULATIONS IN RR
           IF (MODELNAME.EQ.'RAPR')  THEN
             IF (IFHR .GT. 0) THEN
               ID(18)=IFHR-1
             ELSE
               ID(18)=0
             ENDIF
           ENDIF
           if(grib=='grib1') then
             CALL GRIBIT(IGET(123),LVLS(1,IGET(123)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(123))
             if(ITPREC>0) then
                fld_info(cfld)%ntrange=1
             else
                fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     
!     ACCUMULATED WATER RUNOFF.
         IF (IGET(343).GT.0) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = RUNOFF(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           if(grib=='grib1') then
             CALL GRIBIT(IGET(343),LVLS(1,IGET(343)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(343))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF

!     PRECIPITATION BUCKETS - accumulated between output times
!     'BUCKET TOTAL PRECIP '
         IF (IGET(434).GT.0.) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF (IFHR .EQ. 0) THEN
                 GRID1(I,J) = 0.0
               ELSE
                 GRID1(I,J) = PCP_BUCKET(I,J)
               ENDIF 
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           if(MODELNAME.EQ.'NCAR' .OR. MODELNAME.EQ.'RAPR') IFINCR = NINT(PREC_ACC_DT)/60
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           if(grib=='grib1') then
             CALL GRIBIT(IGET(434),LVLS(1,IGET(434)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(434))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=(IFHR-ID(18))/ITPREC
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=ITPREC
             if(fld_info(cfld)%ntrange.eq.0) then
               if (ifhr.eq.0) then
                 fld_info(cfld)%tinvstat=0
               else
                 fld_info(cfld)%tinvstat=1
               endif
               fld_info(cfld)%ntrange=1
             end if
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF

!     PRECIPITATION BUCKETS - accumulated between output times
!     'BUCKET CONV PRECIP  '
         IF (IGET(435).GT.0.) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF (IFHR .EQ. 0) THEN
                 GRID1(I,J) = 0.0
               ELSE
                 GRID1(I,J) = RAINC_BUCKET(I,J)
               ENDIF
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif

           if(MODELNAME.EQ.'NCAR' .OR. MODELNAME.EQ.'RAPR') IFINCR = NINT(PREC_ACC_DT)/60
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0

!          print *,'IFMIN,IFHR,ITPREC',IFMIN,IFHR,ITPREC
           if(me==0)print *,'PREC_ACC_DT,ID(18),ID(19)',PREC_ACC_DT,ID(18),ID(19)

           if(grib=='grib1') then
             CALL GRIBIT(IGET(435),LVLS(1,IGET(435)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(435))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=(IFHR-ID(18))/ITPREC
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=ITPREC
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     PRECIPITATION BUCKETS - accumulated between output times
!     'BUCKET GRDSCALE PRCP'
         IF (IGET(436).GT.0.) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               IF (IFHR .EQ. 0) THEN
                 GRID1(I,J) = 0.0
               ELSE
                 GRID1(I,J) = RAINNC_BUCKET(I,J)
               ENDIF
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           if(MODELNAME.EQ.'NCAR' .OR. MODELNAME.EQ.'RAPR') IFINCR = NINT(PREC_ACC_DT)/60
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           if(grib=='grib1') then
             CALL GRIBIT(IGET(436),LVLS(1,IGET(436)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(436))
             if(ITPREC>0) then
                fld_info(cfld)%ntrange=(IFHR-ID(18))/ITPREC
             else
                fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=ITPREC
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF
!     PRECIPITATION BUCKETS - accumulated between output times
!     'BUCKET SNOW  PRECIP '
         IF (IGET(437).GT.0.) THEN
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = SNOW_BUCKET(I,J)
             ENDDO
           ENDDO
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           if(MODELNAME.EQ.'NCAR' .OR. MODELNAME.EQ.'RAPR') IFINCR = NINT(PREC_ACC_DT)/60
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 4
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           IF (ID(18).LT.0) ID(18) = 0
           if(me==0)print*,'maxval BUCKET SNOWFALL: ', maxval(GRID1)
           if(grib=='grib1') then
             CALL GRIBIT(IGET(437),LVLS(1,IGET(437)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(437))
             if(ITPREC>0) then
               fld_info(cfld)%ntrange=(IFHR-ID(18))/ITPREC
             else
               fld_info(cfld)%ntrange=0
             endif
             fld_info(cfld)%tinvstat=ITPREC
             if(fld_info(cfld)%ntrange.eq.0) then
               if (ifhr.eq.0) then
                 fld_info(cfld)%tinvstat=0
               else
                 fld_info(cfld)%tinvstat=1
               endif
               fld_info(cfld)%ntrange=1
             end if
!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
         ENDIF

!     
!     INSTANTANEOUS PRECIPITATION TYPE.
!         print *,'in surfce,iget(160)=',iget(160),'iget(247)=',iget(247)
         IF (IGET(160).GT.0 .OR.(IGET(247).GT.0)) THEN

           allocate(sleet(im,jsta:jend,nalg),  rain(im,jsta:jend,nalg),   &
                    freezr(im,jsta:jend,nalg), snow(im,jsta:jend,nalg))
           allocate(zwet(im,jsta:jend))
           CALL CALWXT_POST(T,Q,PMID,PINT,HTM,LMH,PREC,ZINT,IWX1,ZWET)
!          write(0,*)' after first CALWXT_POST'


           IF (IGET(160).GT.0) THEN 
!$omp parallel do private(i,j,iwx)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX   = IWX1(I,J)
                 SNOW(I,J,1)   = MOD(IWX,2)
                 SLEET(I,J,1)  = MOD(IWX,4)/2
                 FREEZR(I,J,1) = MOD(IWX,8)/4
                 RAIN(I,J,1)   = IWX/8
               ENDDO
             ENDDO
           ENDIF
!     
!     LOWEST WET BULB ZERO HEIGHT
           IF (IGET(247).GT.0) THEN
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = ZWET(I,J)
               ENDDO
             ENDDO
             if(grib=='grib1') then
               ID(1:25) = 0
               CALL GRIBIT(IGET(247),LVLS(1,IGET(247)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(247))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
           ENDIF

!     DOMINANT PRECIPITATION TYPE
!GSM  IF DOMINANT PRECIP TYPE IS REQUESTED, 4 MORE ALGORITHMS
!GSM    WILL BE CALLED.  THE TALLIES ARE THEN SUMMED IN
!GSM    CALWXT_DOMINANT

           IF (IGET(160).GT.0) THEN   
!  RAMER ALGORITHM
             CALL CALWXT_RAMER_POST(T,Q,PMID,PINT,LMH,PREC,IWX1)
!            print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
               
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX   = IWX1(I,J)
                 SNOW(I,J,2)   = MOD(IWX,2)
                 SLEET(I,J,2)  = MOD(IWX,4)/2
                 FREEZR(I,J,2) = MOD(IWX,8)/4
                 RAIN(I,J,2)   = IWX/8
               ENDDO
             ENDDO

! BOURGOUIN ALGORITHM
             ISEED=44641*(INT(SDAT(1)-1)*24*31+INT(SDAT(2))*24+IHRST)+   &
     &             MOD(IFHR*60+IFMIN,44641)+4357
!            write(0,*)'in SURFCE,me=',me,'bef 1st CALWXT_BOURG_POST iseed=',iseed
             CALL CALWXT_BOURG_POST(IM,JM,JSTA_2L,JEND_2U,JSTA,JEND,LM,LP1,&
     &                              ISEED,G,PTHRESH,                       &
     &                              T,Q,PMID,PINT,LMH,PREC,ZINT,IWX1,me)
!            write(0,*)'in SURFCE,me=',me,'aft 1st CALWXT_BOURG_POST'
!            write(0,*)'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA),'PTHRESH=',PTHRESH

!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX   = IWX1(I,J)
                 SNOW(I,J,3)   = MOD(IWX,2)
                 SLEET(I,J,3)  = MOD(IWX,4)/2
                 FREEZR(I,J,3) = MOD(IWX,8)/4
                 RAIN(I,J,3)   = IWX/8
               ENDDO
             ENDDO

! REVISED NCEP ALGORITHM
             CALL CALWXT_REVISED_POST(T,Q,PMID,PINT,HTM,LMH,PREC,ZINT,IWX1)
!           print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX   = IWX1(I,J)
                 SNOW(I,J,4)   = MOD(IWX,2)
                 SLEET(I,J,4)  = MOD(IWX,4)/2
                 FREEZR(I,J,4) = MOD(IWX,8)/4
                 RAIN(I,J,4)   = IWX/8
               ENDDO
             ENDDO
              
! EXPLICIT ALGORITHM (UNDER 18 NOT ADMITTED WITHOUT PARENT OR GUARDIAN)
 
             IF(imp_physics==5 .or. imp_physics==85 .or. imp_physics==95)then
               CALL CALWXT_EXPLICIT_POST(LMH,THS,PMID,PREC,SR,F_RimeF,IWX1)
             else
!$omp parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   IWX1(I,J) = 0
                 ENDDO
               ENDDO
             end if
!           print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX   = IWX1(I,J)
                 SNOW(I,J,5)   = MOD(IWX,2)
                 SLEET(I,J,5)  = MOD(IWX,4)/2
                 FREEZR(I,J,5) =  MOD(IWX,8)/4
                 RAIN(I,J,5)   = IWX/8
               ENDDO
             ENDDO
               
             allocate(domr(im,jsta:jend),  doms(im,jsta:jend),         &
                      domzr(im,jsta:jend), domip(im,jsta:jend))
             CALL CALWXT_DOMINANT_POST(PREC(1,jsta_2l),RAIN,FREEZR,SLEET,SNOW,  &
                                       DOMR,DOMZR,DOMIP,DOMS)
!            if ( me.eq.0) print *,'after CALWXT_DOMINANT, no avrg'
             ID(1:25) = 0
!     SNOW.
             ID(8) = 143 
             grid1 = spval
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 if(prec(i,j) /= spval) GRID1(I,J) = DOMS(I,J)
               ENDDO
             ENDDO
             if(grib=='grib1') then
               CALL GRIBIT(IGET(160),LVLS(1,IGET(160)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(551))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
!     ICE PELLETS.
             ID(8) = 142 
             grid1=spval
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 if(prec(i,j)/=spval) GRID1(I,J) = DOMIP(I,J)
               ENDDO
             ENDDO
             if(grib=='grib1') then
               CALL GRIBIT(IGET(160),LVLS(1,IGET(160)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(552))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
!     FREEZING RAIN.
             ID(8) = 141
             grid1=spval 
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
!                if (DOMZR(I,J) .EQ. 1) THEN
!                  PSFC(I,J)=PINT(I,J,NINT(LMH(I,J))+1)
!                  print *, 'aha ', I, J, PSFC(I,J)
!                  print *, FREEZR(I,J,1), FREEZR(I,J,2),
!     *  FREEZR(I,J,3), FREEZR(I,J,4), FREEZR(I,J,5)
!                endif
                 if(prec(i,j)/=spval)GRID1(I,J) = DOMZR(I,J)
               ENDDO
             ENDDO
             if(grib=='grib1') then
               CALL GRIBIT(IGET(160),LVLS(1,IGET(160)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(553))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
!     RAIN.
             ID(8) = 140
             grid1=spval 
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 if(prec(i,j)/=spval)GRID1(I,J) = DOMR(I,J)
               ENDDO
             ENDDO
             if(grib=='grib1') then
               CALL GRIBIT(IGET(160),LVLS(1,IGET(160)),GRID1,IM,JM)
             elseif(grib=='grib2') then
               cfld=cfld+1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(160))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
           ENDIF
         ENDIF
!     
!     TIME AVERAGED PRECIPITATION TYPE.
         IF (IGET(317).GT.0) THEN

           if (.not. allocated(sleet))  allocate(sleet(im,jsta:jend,nalg))
           if (.not. allocated(rain))   allocate(rain(im,jsta:jend,nalg))
           if (.not. allocated(freezr)) allocate(freezr(im,jsta:jend,nalg))
           if (.not. allocated(snow))   allocate(snow(im,jsta:jend,nalg))
           if (.not. allocated(zwet))   allocate(zwet(im,jsta:jend))
           CALL CALWXT_POST(T,Q,PMID,PINT,HTM,LMH,AVGPREC,ZINT,IWX1,ZWET)

           if (allocated(zwet)) deallocate(zwet)
!          write(0,*)' after second CALWXT_POST me=',me
!          print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
!$omp parallel do private(i,j,iwx)
           DO J=JSTA,JEND
             DO I=1,IM
               IWX   = IWX1(I,J)
               SNOW(I,J,1)   = MOD(IWX,2)
               SLEET(I,J,1)  = MOD(IWX,4)/2
               FREEZR(I,J,1) = MOD(IWX,8)/4
               RAIN(I,J,1)   = IWX/8
             ENDDO
           ENDDO

!     DOMINANT PRECIPITATION TYPE
!GSM  IF DOMINANT PRECIP TYPE IS REQUESTED, 4 MORE ALGORITHMS
!GSM    WILL BE CALLED.  THE TALLIES ARE THEN SUMMED IN
!GSM    CALWXT_DOMINANT

!  RAMER ALGORITHM
           CALL CALWXT_RAMER_POST(T,Q,PMID,PINT,LMH,AVGPREC,IWX1)
!          print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
               
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
           DO J=JSTA,JEND
             DO I=1,IM
               IWX   = IWX1(I,J)
               SNOW(I,J,2)   = MOD(IWX,2)
               SLEET(I,J,2)  = MOD(IWX,4)/2
               FREEZR(I,J,2) = MOD(IWX,8)/4
               RAIN(I,J,2)   = IWX/8
             ENDDO
           ENDDO

! BOURGOUIN ALGORITHM
           ISEED=44641*(INT(SDAT(1)-1)*24*31+INT(SDAT(2))*24+IHRST)+   &
     &           MOD(IFHR*60+IFMIN,44641)+4357
!          write(0,*)'in SURFCE,me=',me,'bef sec CALWXT_BOURG_POST'
           CALL CALWXT_BOURG_POST(IM,JM,JSTA_2L,JEND_2U,JSTA,JEND,LM,LP1,&
     &                        ISEED,G,PTHRESH,                            &
     &                        T,Q,PMID,PINT,LMH,AVGPREC,ZINT,IWX1,me)
!          write(0,*)'in SURFCE,me=',me,'aft sec CALWXT_BOURG_POST'
!          print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)

!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
           DO J=JSTA,JEND
             DO I=1,IM
               IWX   = IWX1(I,J)
               SNOW(I,J,3)   = MOD(IWX,2)
               SLEET(I,J,3)  = MOD(IWX,4)/2
               FREEZR(I,J,3) = MOD(IWX,8)/4
               RAIN(I,J,3)   = IWX/8
             ENDDO
           ENDDO

! REVISED NCEP ALGORITHM
           CALL CALWXT_REVISED_POST(T,Q,PMID,PINT,HTM,LMH,AVGPREC,ZINT,IWX1)
!          write(0,*)'in SURFCE,me=',me,'aft sec CALWXT_REVISED_BOURG_POST'
!          print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
           DO J=JSTA,JEND
             DO I=1,IM
               IWX   = IWX1(I,J)
               SNOW(I,J,4)   = MOD(IWX,2)
               SLEET(I,J,4)  = MOD(IWX,4)/2
               FREEZR(I,J,4) = MOD(IWX,8)/4
               RAIN(I,J,4)   = IWX/8
             ENDDO
           ENDDO
              
! EXPLICIT ALGORITHM (UNDER 18 NOT ADMITTED WITHOUT PARENT OR GUARDIAN)
 
!          write(0,*)'in SURFCE,me=',me,'imp_physics=',imp_physics
           IF(imp_physics == 5)then
             CALL CALWXT_EXPLICIT_POST(LMH,THS,PMID,AVGPREC,SR,F_RimeF,IWX1)
           else
!$omp parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 IWX1(I,J) = 0
               ENDDO
             ENDDO
           end if
!          print *,'in SURFCE,me=',me,'IWX1=',IWX1(1:30,JSTA)
!     DECOMPOSE IWX1 ARRAY
!
!$omp parallel do private(i,j,iwx)
           DO J=JSTA,JEND
             DO I=1,IM
               IWX   = IWX1(I,J)
               SNOW(I,J,5)   = MOD(IWX,2)
               SLEET(I,J,5)  = MOD(IWX,4)/2
               FREEZR(I,J,5) = MOD(IWX,8)/4
               RAIN(I,J,5)   = IWX/8
             ENDDO
           ENDDO
               
!            print *,'me=',me,'before SNOW=',snow(1:10,JSTA,1:5)
!            print *,'me=',me,'before RAIN=',RAIN(1:10,JSTA,1:5)
!            print *,'me=',me,'before FREEZR=',FREEZR(1:10,JSTA,1:5)
!            print *,'me=',me,'before SLEET=',SLEET(1:10,JSTA,1:5)

           if (.not. allocated(domr))  allocate(domr(im,jsta:jend))
           if (.not. allocated(doms))  allocate(doms(im,jsta:jend))
           if (.not. allocated(domzr)) allocate(domzr(im,jsta:jend))
           if (.not. allocated(domip)) allocate(domip(im,jsta:jend))

           CALL CALWXT_DOMINANT_POST(AVGPREC,RAIN,FREEZR,SLEET,SNOW,    &
                                     DOMR,DOMZR,DOMIP,DOMS)
     
           ID(1:25) = 0
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR     = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR     = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 3
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF

!          TPREC,'IFHR=',IFHR,'IFMIN=',IFMIN,'IFINCR=',IFINCR,'ID=',ID
!     SNOW.
            
           ID(8) = 143 
           grid1=spval
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               if(avgprec(i,j) /= spval) GRID1(I,J) = DOMS(I,J)
             ENDDO
           ENDDO
!           print *,'me=',me,'SNOW=',GRID1(1:10,JSTA)
           if(grib=='grib1') then
             CALL GRIBIT(IGET(317),LVLS(1,IGET(317)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(555))
             if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
             else
              fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
!     ICE PELLETS.
           ID(8) = 142 
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR   = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR   = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 3
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           grid1=spval
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               if(avgprec(i,j)/=spval) GRID1(I,J) = DOMIP(I,J)
             ENDDO
           ENDDO
           if(grib=='grib1') then
             CALL GRIBIT(IGET(317),LVLS(1,IGET(317)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(556))
             if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
             else
              fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
!     FREEZING RAIN.
           ID(8) = 141
    
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR   = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR   = 0
           endif
!mp
           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 3
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           grid1=spval
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
!             if (DOMZR(I,J) .EQ. 1) THEN
!               PSFC(I,J)=PINT(I,J,NINT(LMH(I,J))+1)
!               print *, 'aha ', I, J, PSFC(I,J)
!               print *, FREEZR(I,J,1), FREEZR(I,J,2),
!     *  FREEZR(I,J,3), FREEZR(I,J,4), FREEZR(I,J,5)
!             endif
               if(avgprec(i,j)/=spval) GRID1(I,J) = DOMZR(I,J)
             ENDDO
           ENDDO
           if(grib=='grib1') then
             CALL GRIBIT(IGET(317),LVLS(1,IGET(317)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(557))
             if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
             else
              fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif
!     RAIN.
           ID(8) = 140
    
           ITPREC     = NINT(TPREC)
!mp
           if (ITPREC .ne. 0) then
             IFINCR   = MOD(IFHR,ITPREC)
             IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
           else
             IFINCR   = 0
           endif
!mp:w

           ID(18)     = 0
           ID(19)     = IFHR
           IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
           ID(20)     = 3
           IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
           ELSE
             ID(18) = IFHR-IFINCR
             IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
           ENDIF
           grid1=spval 
!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               if(avgprec(i,j)/=spval) GRID1(I,J) = DOMR(I,J)
             ENDDO
           ENDDO
           if(grib=='grib1') then
             CALL GRIBIT(IGET(317),LVLS(1,IGET(317)),GRID1,IM,JM)
           elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(317))
             if(ITPREC==0) then
              fld_info(cfld)%ntrange=0
             else
              fld_info(cfld)%ntrange=1
             endif
             fld_info(cfld)%tinvstat=IFHR-ID(18)

!$omp parallel do private(i,j,jj)
             do j=1,jend-jsta+1
               jj = jsta+j-1
               do i=1,im
                 datapd(i,j,cfld) = GRID1(i,jj)
               enddo
             enddo
           endif

         ENDIF

         if (allocated(rain))   deallocate(rain)
         if (allocated(snow))   deallocate(snow)
         if (allocated(sleet))  deallocate(sleet)
         if (allocated(freezr)) deallocate(freezr)

! GSD PRECIPITATION TYPE
         IF (IGET(407).GT.0 .or. IGET(559).GT.0 .or.  &
             IGET(560).GT.0 .or. IGET(561).GT.0) THEN

           if (.not. allocated(domr))  allocate(domr(im,jsta:jend))
           if (.not. allocated(doms))  allocate(doms(im,jsta:jend))
           if (.not. allocated(domzr)) allocate(domzr(im,jsta:jend))
           if (.not. allocated(domip)) allocate(domip(im,jsta:jend))

!$omp parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               DOMS(I,J)  = 0.  !-- snow
               DOMR(I,J)  = 0.  !-- rain
               DOMZR(I,J) = 0.  !-- freezing rain
               DOMIP(I,J) = 0.  !-- ice pellets
             ENDDO
           ENDDO

           DO J=JSTA,JEND
             DO I=1,IM
!-- TOTPRCP is total 1-hour accumulated precipitation in  [m]
               totprcp = (RAINC_BUCKET(I,J) + RAINNC_BUCKET(I,J))*1.e-3
               snowratio = 0.0
               if(graup_bucket(i,j)*1.e-3 > totprcp)then
                 print *,'WARNING - Graupel is higher that total precip at point',i,j
                 print *,'totprcp,graup_bucket(i,j),snow_bucket(i,j),rainnc_bucket',&
                          totprcp,graup_bucket(i,j),snow_bucket(i,j),rainnc_bucket(i,j)
               endif

!  ---------------------------------------------------------------
!  Minimum 1h precipitation to even consider p-type specification
!      (0.0001 mm in 1h, very light precipitation)
!  ---------------------------------------------------------------
               if (totprcp-graup_bucket(i,j)*1.e-3 > 0.0000001)       &
!          snowratio = snow_bucket(i,j)*1.e-3/totprcp            ! orig
!14aug15 - change from Stan and Trevor
!  ---------------------------------------------------------------
!      Snow-to-total ratio to be used below
!  ---------------------------------------------------------------
               snowratio = snow_bucket(i,j)*1.e-3 / (totprcp-graup_bucket(i,j)*1.e-3)

!              snowratio = SR(i,j)
!-- 2-m temperature
               t2 = TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
!  ---------------------------------------------------------------
!--snow (or rain if T2m > 3 C)
!  ---------------------------------------------------------------
!--   SNOW is time step non-convective snow [m]
!     -- based on either instantaneous snowfall or 1h snowfall and
!     snowratio
                if( (SNOWNC(i,j)/DT .gt. 0.2e-9 .and. snowratio.ge.0.25) &
                       .or.                                         &
               (totprcp.gt.0.00001.and.snowratio.ge.0.25)) DOMS(i,j) = 1.
               if (t2.ge.276.15) then
!              switch snow to rain if 2m temp > 3 deg
                 DOMR(I,J) = 1.
                 DOMS(I,J) = 0.
               end if

!  ---------------------------------------------------------------
!-- rain/freezing rain
!  ---------------------------------------------------------------
!--   compute RAIN [m/s] from total convective and non-convective precipitation
               rainl = (1. - SR(i,j))*prec(i,j)/DT
!-- in RUC RAIN is in cm/h and the limit is 1.e-3,
!-- converted to m/s will be 2.8e-9
               if((rainl .gt. 2.8e-9 .and. snowratio.lt.0.60) .or.      &
                 (totprcp.gt.0.00001 .and. snowratio.lt.0.60)) then

                 if (t2.ge.273.15) then
!--rain
                   DOMR(I,J) = 1.
!               else if (tmax(i,j).gt.273.15) then
!14aug15 - stan
                 else
!-- freezing rain
                   DOMZR(I,J) = 1.
                 endif
               endif

!  ---------------------------------------------------------------
!-- graupel/ice pellets vs. snow or rain
!  ---------------------------------------------------------------
!-- GRAUPEL is time step non-convective graupel in [m]
               if(GRAUPELNC(i,j)/DT .gt. 1.e-9) then
                 if (t2.le.276.15) then
!                 This T2m test excludes convectively based hail
!                   from cold-season ice pellets.

!            check for max rain mixing ratio
!              if it's > 0.05 g/kg, => ice pellets
                   if (qrmax(i,j).gt.0.000005) then
                     if(GRAUPELNC(i,j) .gt. 0.5*SNOWNC(i,j)) then
!                if (instantaneous graupel fall rate > 0.5*
!                     instantaneous snow fall rate, ....
!-- diagnose ice pellets
                       DOMIP(I,J) = 1.

! -- If graupel is greater than rain,
!        report graupel only
! in RUC --> if (3.6E5*gex2(i,j,8).gt.   gex2(i,j,6)) then
                       if ((GRAUPELNC(i,j)/DT) .gt. rainl) then
                         DOMIP(I,J) = 1.
                         DOMZR(I,J) = 0.
                         DOMR(I,J)  = 0.
! -- If rain is greater than 4x graupel,
!        report rain only
! in RUC -->  else if (gex2(i,j,6).gt.4.*3.6E5*gex2(i,j,8)) then
                       else if (rainl .gt. (4.*GRAUPELNC(i,j)/DT)) then
                         DOMIP(I,J) = 0.
                       end if

                     else   !  instantaneous graupel fall rate <
                        !    0.5 * instantaneous snow fall rate
!                snow  -- ensure snow is diagnosed  (no ice pellets)
                       DOMS(i,j)=1.
                     end if
                   else     !  if qrmax is not > 0.00005
!              snow
                     DOMS(i,j)=1.
                   end if

                 else       !  if t2 >= 3 deg C
!              rain
                   DOMR(I,J) = 1.
                 end if     !  End of t2 if/then loop

               end if       !  End of GRAUPELNC if/then loop

             ENDDO
           ENDDO


        write (6,*)' Snow/rain ratio'
        write (6,*)' max/min 1h-SNOWFALL in [cm]',   &
              maxval(snow_bucket)*0.1,minval(snow_bucket)*0.1

        DO J=JSTA,JEND
        DO I=1,IM
           do icat=1,10
           if (snow_bucket(i,j)*0.1.lt.0.1*float(icat).and.     &
               snow_bucket(i,j)*0.1.gt.0.1*float(icat-1)) then
                  cnt_snowratio(icat)=cnt_snowratio(icat)+1
           end if
           end do
        end do
        end do

        write (6,*) 'Snow ratio point counts'
           do icat=1,10
        write (6,*) icat, cnt_snowratio(icat)
           end do

        icnt_snow_rain_mixed = 0
        DO J=JSTA,JEND
          DO I=1,IM
            if (DOMR(i,j).eq.1 .and. DOMS(i,j).eq.1) then
               icnt_snow_rain_mixed = icnt_snow_rain_mixed + 1
            endif
          end do
        end do

        write (6,*) 'No. of mixed snow/rain p-type diagnosed=',   &
            icnt_snow_rain_mixed


           ID(1:25) = 0
!     SNOW.
            ID(8) = 143
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J)=DOMS(I,J)
              ENDDO
            ENDDO
            if(grib=='grib1') then
            CALL GRIBIT(IGET(407),LVLS(1,IGET(407)),       &
                 GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(559))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
!     ICE PELLETS.
            ID(8) = 142
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = DOMIP(I,J)
!             if (DOMIP(I,J) .EQ. 1) THEN
!               print *, 'ICE PELLETS at I,J ', I, J
!             endif
              ENDDO
            ENDDO
            if(grib=='grib1') then
              CALL GRIBIT(IGET(407),LVLS(1,IGET(407)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(560))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
!     FREEZING RAIN.
            ID(8) = 141
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
!             if (DOMZR(I,J) .EQ. 1) THEN
!               PSFC(I,J)=PINT(I,J,NINT(LMH(I,J))+1)
!               print *, 'FREEZING RAIN AT I,J ', I, J, PSFC(I,J)
!             endif
                GRID1(I,J) = DOMZR(I,J)
              ENDDO
            ENDDO
           if(grib=='grib1') then
            CALL GRIBIT(IGET(407),LVLS(1,IGET(407)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(561))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
!     RAIN.
            ID(8) = 140
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = DOMR(I,J)
              ENDDO
            ENDDO
           if(grib=='grib1') then
            CALL GRIBIT(IGET(407),LVLS(1,IGET(407)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(407))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif

        ENDIF
!     
        if (allocated(psfc))  deallocate(psfc)
        if (allocated(domr))  deallocate(domr)
        if (allocated(doms))  deallocate(doms)
        if (allocated(domzr)) deallocate(domzr)
        if (allocated(domip)) deallocate(domip)
!
!
!***  BLOCK 5.  SURFACE EXCHANGE FIELDS.
!     
!     TIME AVERAGED SURFACE LATENT HEAT FLUX.
         IF (IGET(042).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
             GRID1=SPVAL
             ID(1:25)=0
          ELSE  
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
	     IF(SFCLHX(I,J)/=SPVAL)THEN
              GRID1(I,J)=-1.*SFCLHX(I,J)*RRNUM !change the sign to conform with Grib
	     ELSE
	      GRID1(I,J)=SFCLHX(I,J)
	     END IF 
            ENDDO
            ENDDO
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
           if(grib=='grib1') then
            CALL GRIBIT(IGET(042),LVLS(1,IGET(042)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(042))
            if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
          END IF 
         ENDIF
!
!     TIME AVERAGED SURFACE SENSIBLE HEAT FLUX.
         IF (IGET(043).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
	    GRID1=SPVAL
	    ID(1:25)=0
	  ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
	     IF(SFCSHX(I,J)/=SPVAL)THEN
              GRID1(I,J) = -1.* SFCSHX(I,J)*RRNUM !change the sign to conform with Grib
	     ELSE
	      GRID1(I,J)=SFCSHX(I,J)
	     END IF  
            ENDDO
            ENDDO
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
	  END IF  
           if(grib=='grib1') then
          CALL GRIBIT(IGET(043),LVLS(1,IGET(043)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(043))
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
!     TIME AVERAGED SUB-SURFACE SENSIBLE HEAT FLUX.
         IF (IGET(135).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
	    GRID1=SPVAL
	    ID(1:25)=0
	  ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J) = SUBSHX(I,J)*RRNUM
            ENDDO
            ENDDO
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
	  END IF  
           if(grib=='grib1') then
          CALL GRIBIT(IGET(135),LVLS(1,IGET(135)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(135))
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
!     TIME AVERAGED SNOW PHASE CHANGE HEAT FLUX.
         IF (IGET(136).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
	    GRID1=SPVAL
	    ID(1:25)=0
	  ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J) = SNOPCX(I,J)*RRNUM
            ENDDO
            ENDDO
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
	  END IF  
           if(grib=='grib1') then
          CALL GRIBIT(IGET(136),LVLS(1,IGET(136)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(136))
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
!     TIME AVERAGED SURFACE MOMENTUM FLUX.
         IF (IGET(046).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
	    GRID1=SPVAL
	    ID(1:25)=0
	  ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
	     IF(SFCUVX(I,J)/=SPVAL)THEN
              GRID1(I,J) = SFCUVX(I,J)*RRNUM
	     ELSE
	      GRID1(I,J) = SFCUVX(I,J)
	     END IF  
            ENDDO
            ENDDO
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
	  END IF  
           if(grib=='grib1') then
          CALL GRIBIT(IGET(046),LVLS(1,IGET(046)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(046))
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
!     TIME AVERAGED SURFACE ZONAL MOMENTUM FLUX.
         IF (IGET(269).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
            GRID1=SPVAL
            ID(1:25)=0
          ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J) = SFCUX(I,J)*RRNUM
            ENDDO
            ENDDO
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
          END IF
           if(grib=='grib1') then
          CALL GRIBIT(IGET(269),LVLS(1,IGET(269)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(269))
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
!     TIME AVERAGED SURFACE MOMENTUM FLUX.
         IF (IGET(270).GT.0) THEN
          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
             MODELNAME.EQ.'RAPR')THEN
            GRID1=SPVAL
            ID(1:25)=0
          ELSE
            IF(ASRFC.GT.0.)THEN
              RRNUM=1./ASRFC
            ELSE
              RRNUM=0.
            ENDIF
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J) = SFCVX(I,J)*RRNUM
            ENDDO
            ENDDO
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
          END IF
          if(grib=='grib1') then
          CALL GRIBIT(IGET(270),LVLS(1,IGET(270)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(270))
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
!     ACCUMULATED SURFACE EVAPORATION
         IF (IGET(047).GT.0) THEN
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = SFCEVP(I,J)*1000.
              ENDDO
            ENDDO
            ID(1:25) = 0
            ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
	 IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
            ID(18)     = 0
            ID(19)     = IFHR
	    IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
            ELSE
             ID(18) = IFHR-IFINCR
	     IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
          if(grib=='grib1') then
            CALL GRIBIT(IGET(047),LVLS(1,IGET(047)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(047))
            if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)

           endif
         ENDIF
!     
!     ACCUMULATED POTENTIAL EVAPORATION
         IF (IGET(137).GT.0) THEN
            DO J=JSTA,JEND
              DO I=1,IM
               GRID1(I,J) = POTEVP(I,J)*1000.
              ENDDO
            ENDDO
            ID(1:25) = 0
            ITPREC     = NINT(TPREC)
!mp
	if (ITPREC .ne. 0) then
         IFINCR     = MOD(IFHR,ITPREC)
	 IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITPREC*60)
	else
	 IFINCR     = 0
	endif
!mp
            ID(18)     = 0
            ID(19)     = IFHR
	    IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
            ID(20)     = 4
            IF (IFINCR.EQ.0) THEN
             ID(18) = IFHR-ITPREC
            ELSE
             ID(18) = IFHR-IFINCR
	     IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
            ENDIF
            IF (ID(18).LT.0) ID(18) = 0
          if(grib=='grib1') then
            CALL GRIBIT(IGET(137),LVLS(1,IGET(137)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(137))
            if(ITPREC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
        endif
         ENDIF
!     
!     ROUGHNESS LENGTH.
      IF (IGET(044).GT.0) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = Z0(I,J)
            ENDDO
          ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(044),LVLS(1,IGET(044)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(044))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!     FRICTION VELOCITY.
      IF (IGET(045).GT.0) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              GRID1(I,J) = USTAR(I,J)
            ENDDO
          ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(045),LVLS(1,IGET(045)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(045))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!     SURFACE DRAG COEFFICIENT.
      IF (IGET(132).GT.0) THEN
         CALL CALDRG(EGRID1(1,jsta_2l))
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=EGRID1(I,J)
            ENDDO
            ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(132),LVLS(1,IGET(132)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(132))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!     MODEL OUTPUT SURFACE U AND/OR V COMPONENT WIND STRESS
      IF ( (IGET(900).GT.0) .OR. (IGET(901).GT.0) ) THEN
!
!        MODEL OUTPUT SURFACE U COMPONENT WIND STRESS.
         IF (IGET(900).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=MDLTAUX(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(900),LVLS(1,IGET(900)),GRID1,IM,JM)
         ENDIF
!
!        MODEL OUTPUT SURFACE V COMPONENT WIND STRESS
         IF (IGET(901).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=MDLTAUY(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(901),LVLS(1,IGET(901)),GRID1,IM,JM)
         ENDIF
      ENDIF
!
!     SURFACE U AND/OR V COMPONENT WIND STRESS
      IF ( (IGET(133).GT.0) .OR. (IGET(134).GT.0) ) THEN
         CALL CALTAU(EGRID1(1,jsta),EGRID2(1,jsta))
!     
!        SURFACE U COMPONENT WIND STRESS.
         IF (IGET(133).GT.0) THEN
            DO J=JSTA,JEND
              DO I=1,IM
              GRID1(I,J)=EGRID1(I,J)
              ENDDO
            ENDDO
!     
          if(grib=='grib1') then
            ID(1:25) = 0
            CALL GRIBIT(IGET(133),LVLS(1,IGET(133)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(133))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF
!     
!        SURFACE V COMPONENT WIND STRESS
         IF (IGET(134).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=EGRID2(I,J)
            ENDDO
            ENDDO
          if(grib=='grib1') then
            ID(1:25) = 0
            CALL GRIBIT(IGET(134),LVLS(1,IGET(134)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(134))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF
      ENDIF
!     
!     GRAVITY U AND/OR V COMPONENT STRESS
      IF ( (IGET(315).GT.0) .OR. (IGET(316).GT.0) ) THEN
!     
!        GRAVITY U COMPONENT WIND STRESS.
         IF (IGET(315).GT.0) THEN
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = GTAUX(I,J)
              ENDDO
            ENDDO
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
          if(grib=='grib1') then
            CALL GRIBIT(IGET(315),LVLS(1,IGET(315)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(315))
            if(ITSRFC==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=1
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
         ENDIF
!     
!        SURFACE V COMPONENT WIND STRESS
         IF (IGET(316).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=GTAUY(I,J)
            ENDDO
            ENDDO
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
          if(grib=='grib1') then
            CALL GRIBIT(IGET(316),LVLS(1,IGET(316)),GRID1,IM,JM)
            elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(316))
            if(ITSRFC==0) then
              fld_info(cfld)%ntrange=0
            else
              fld_info(cfld)%ntrange=1
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF
      ENDIF      
!     
!     INSTANTANEOUS SENSIBLE HEAT FLUX
      IF (IGET(154).GT.0) THEN
        IF(MODELNAME.EQ.'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
           MODELNAME.EQ.'RAPR')THEN
!4omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J) = TWBS(I,J)
            ENDDO
          ENDDO
        ELSE
!4omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J) = -TWBS(I,J)
            ENDDO
          ENDDO
        END IF
        if(grib=='grib1') then
          ID(1:25) = 0
          CALL GRIBIT(IGET(154),LVLS(1,IGET(154)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(154))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
        endif
      ENDIF
!     
!     INSTANTANEOUS LATENT HEAT FLUX
      IF (IGET(155).GT.0) THEN
        IF(MODELNAME.EQ.'NCAR'.OR.MODELNAME.EQ.'RSM' .OR. &
           MODELNAME.EQ.'RAPR')THEN
!4omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J) = QWBS(I,J)
            ENDDO
          ENDDO
        ELSE
!4omp parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J) = -QWBS(I,J)
            ENDDO
          ENDDO
        END IF
        if(grib=='grib1') then
          ID(1:25) = 0
          CALL GRIBIT(IGET(155),LVLS(1,IGET(155)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(155))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
        endif
      ENDIF
!     
!     SURFACE EXCHANGE COEFF
      IF (IGET(169).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=SFCEXC(I,J)
            ENDDO
            ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(169),LVLS(1,IGET(169)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(169))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!     GREEN VEG FRACTION
      IF (IGET(170).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=VEGFRC(I,J)*100.
            ENDDO
            ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(170),LVLS(1,IGET(170)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(170))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!     INSTANTANEOUS GROUND HEAT FLUX
      IF (IGET(152).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=GRNFLX(I,J)
           ENDDO
         ENDDO
         if(grib=='grib1') then
           ID(1:25) = 0
           CALL GRIBIT(IGET(152),LVLS(1,IGET(152)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(152))
           datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!    VEGETATION TYPE
      IF (IGET(218).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = FLOAT(IVGTYP(I,J))
           ENDDO
         ENDDO
         if(grib=='grib1') then
           ID(1:25) = 0
           CALL GRIBIT(IGET(218),LVLS(1,IGET(218)),GRID1,IM,JM)
         elseif(grib=='grib2') then
           cfld=cfld+1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(218))
           datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF
!
!    SOIL TYPE
      IF (IGET(219).GT.0) THEN
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = FLOAT(ISLTYP(I,J))
           ENDDO
         ENDDO
          if(grib=='grib1') then
         ID(1:25) = 0
         CALL GRIBIT(IGET(219),LVLS(1,IGET(219)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(219))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!    SLOPE TYPE
      IF (IGET(223).GT.0) THEN
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = FLOAT(ISLOPE(I,J))
          ENDDO
        ENDDO
        if(grib=='grib1') then
          ID(1:25) = 0
          ID(02)= 130
          CALL GRIBIT(IGET(223),LVLS(1,IGET(223)),GRID1,IM,JM)
        elseif(grib=='grib2') then
          cfld=cfld+1
          fld_info(cfld)%ifld=IAVBLFLD(IGET(223))
          datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
        endif
      ENDIF
      if (me==0)print*,'starting computing canopy conductance'
!
! CANOPY CONDUCTANCE
! ONLY OUTPUT NEW LSM FIELDS FOR NMM AND ARW BECAUSE RSM USES OLD SOIL TYPES
      IF (MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'NMM' .OR. MODELNAME.EQ.'RAPR')THEN
      IF (IGET(220).GT.0 .OR. IGET(234).GT.0               &
     & .OR. IGET(235).GT.0 .OR. IGET(236).GT.0             &
     & .OR. IGET(237).GT.0 .OR. IGET(238).GT.0             &
     & .OR. IGET(239).GT.0 .OR. IGET(240).GT.0             &
     & .OR. IGET(241).GT.0 .OR. IGET(254).GT.0 ) THEN
        IF (iSF_SURFACE_PHYSICS .EQ. 2) THEN    !NSOIL == 4
          if(me==0)print*,'starting computing canopy conductance'
         allocate(rsmin(im,jsta:jend), smcref(im,jsta:jend), gc(im,jsta:jend), &
                  rcq(im,jsta:jend), rct(im,jsta:jend), rcsoil(im,jsta:jend), rcs(im,jsta:jend))
         DO J=JSTA,JEND
           DO I=1,IM
             IF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.     &
     &           (abs(SICE(I,J)-0.) .lt. 1.0E-5) ) THEN
              IF(CZMEAN(I,J).GT.1.E-6) THEN
               FACTRS = CZEN(I,J)/CZMEAN(I,J)
              ELSE
               FACTRS = 0.0
              ENDIF
!              SOLAR=HBM2(I,J)*RSWIN(I,J)*FACTRS
              LLMH   = NINT(LMH(I,J))
              SOLAR  = RSWIN(I,J)*FACTRS
              SFCTMP = T(I,J,LLMH)
              SFCQ   = Q(I,J,LLMH)
              SFCPRS = PINT(I,J,LLMH+1)
!              IF(IVGTYP(I,J).EQ.0)PRINT*,'IVGTYP ZERO AT ',I,J
!     &        ,SM(I,J)
              IVG = IVGTYP(I,J)
!              IF(IVGTYP(I,J).EQ.0)IVG=7
!              CALL CANRES(SOLAR,SFCTMP,SFCQ,SFCPRS
!     &        ,SMC(I,J,1:NSOIL),GC(I,J),RC,IVG,ISLTYP(I,J))
!
              CALL CANRES(SOLAR,SFCTMP,SFCQ,SFCPRS                       &
     &                   ,SH2O(I,J,1:NSOIL),GC(I,J),RC,IVG,ISLTYP(I,J)   &
     &                   ,RSMIN(I,J),NROOTS(I,J),SMCWLT(I,J),SMCREF(I,J) &
     &                   ,RCS(I,J),RCQ(I,J),RCT(I,J),RCSOIL(I,J),SLDPTH)  
               IF(abs(SMCWLT(I,J)-0.5).lt.1.e-5)print*,       &
     &       'LARGE SMCWLT',i,j,SM(I,J),ISLTYP(I,J),SMCWLT(I,J)
             ELSE
              GC(I,J)     = 0.
              RSMIN(I,J)  = 0.
              NROOTS(I,J) = 0
              SMCWLT(I,J) = 0.
              SMCREF(I,J) = 0.
              RCS(I,J)    = 0.
              RCQ(I,J)    = 0.
              RCT(I,J)    = 0.
              RCSOIL(I,J) = 0.
             END IF
           ENDDO
         ENDDO

         IF (IGET(220).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = GC(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(220),LVLS(1,IGET(220)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(220))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(234).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = RSMIN(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(234),LVLS(1,IGET(234)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(234))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF
	 
         IF (IGET(235).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = FLOAT(NROOTS(I,J))
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(235),LVLS(1,IGET(235)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(235))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(236).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = SMCWLT(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(236),LVLS(1,IGET(236)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(236))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(237).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = SMCREF(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(237),LVLS(1,IGET(237)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(237))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(238).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = RCS(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(238),LVLS(1,IGET(238)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(238))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(239).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = RCT(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(239),LVLS(1,IGET(239)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(239))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         IF (IGET(240).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = RCQ(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(240),LVLS(1,IGET(240)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(240))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF	
         
         IF (IGET(241).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = RCSOIL(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(241),LVLS(1,IGET(241)),GRID1,IM,JM)                                                          
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(241))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

         if (allocated(rsmin))  deallocate(rsmin)
         if (allocated(smcref)) deallocate(smcref)
         if (allocated(rcq))    deallocate(rcq)
         if (allocated(rct))    deallocate(rct)
         if (allocated(rcsoil)) deallocate(rcsoil)
         if (allocated(rcs))    deallocate(rcs)
         if (allocated(gc))     deallocate(gc)

! Time-averaged SWDOWN
         IF (IGET(733).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = SWRADmean(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
          ID(02)= 130
          CALL GRIBIT(IGET(733),LVLS(1,IGET(733)),            &
              GRID1,IM,JM)                                                          
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(733))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF          
! Time-averaged SWNORM
         IF (IGET(734).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = SWNORMmean(I,J)
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
          ID(02)= 130
          CALL GRIBIT(IGET(734),LVLS(1,IGET(734)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(734))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

!---------
!	 print*,'outputting leaf area index= ',XLAI
         IF (IGET(254).GT.0 )THEN
          DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = XLAI
           ENDDO
          ENDDO
          if(grib=='grib1') then
          ID(1:25) = 0
	  ID(02)= 130
          CALL GRIBIT(IGET(254),LVLS(1,IGET(254)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(254))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
         ENDIF

        ENDIF
      END IF
!GPL added endif here
      ENDIF
      IF(MODELNAME .EQ. 'GFS')THEN
! Outputting wilting point and field capacity for TIGGE
       IF(IGET(236).GT.0)THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = smcwlt(i,j)
!            IF(isltyp(i,j)/=0)THEN
!              GRID1(I,J) = WLTSMC(isltyp(i,j))
!            ELSE
!              GRID1(I,J) = spval
!            END IF
          ENDDO
        ENDDO
          if(grib=='grib1') then
        ID(1:25) = 0
	ID(02)= 130
        CALL GRIBIT(IGET(236),LVLS(1,IGET(236)), GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(236))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
       ENDIF
       
       IF(IGET(397).GT.0)THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = fieldcapa(i,j)
!            IF(isltyp(i,j)/=0)THEN
!              GRID1(I,J) = REFSMC(isltyp(i,j))
!            ELSE
!              GRID1(I,J) = spval
!            END IF
          ENDDO
        ENDDO
          if(grib=='grib1') then
        ID(1:25) = 0
	ID(02)= 130
        CALL GRIBIT(IGET(397),LVLS(1,IGET(397)), GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(397))
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
           endif
       ENDIF
      END IF 
      IF(IGET(396).GT.0)THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = suntime(i,j)
          ENDDO
        ENDDO
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
        if(grib=='grib1') then
	  ID(02)= 133
          CALL GRIBIT(IGET(396),LVLS(1,IGET(396)), GRID1,IM,JM)
        elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(396))
            if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
        endif
      ENDIF    

      IF(IGET(517).GT.0)THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = avgpotevp(i,j)
          ENDDO
        ENDDO
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
        if(grib=='grib1') then
          CALL GRIBIT(IGET(517),LVLS(1,IGET(517)), GRID1,IM,JM)
        elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(517))
            if(ITSRFC>0) then
               fld_info(cfld)%ntrange=1
            else
               fld_info(cfld)%ntrange=0
            endif
            fld_info(cfld)%tinvstat=IFHR-ID(18)
!$omp parallel do private(i,j,jj)
            do j=1,jend-jsta+1
              jj = jsta+j-1
              do i=1,im
                datapd(i,j,cfld) = GRID1(i,jj)
              enddo
            enddo
        endif
      ENDIF

!     
!     
!       MODEL TOP REQUESTED BY CMAQ
      IF (IGET(282).GT.0) THEN
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = PT
              ENDDO
            ENDDO
          if(grib=='grib1') then
            ID(1:25) = 0
            CALL GRIBIT(IGET(282),LVLS(1,IGET(282)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(282))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!     
!       PRESSURE THICKNESS REQUESTED BY CMAQ
      IF (IGET(283).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=PDTOP
            ENDDO
            ENDDO
            ID(1:25) = 0
	    IF(ME == 0)THEN 
	     DO L=1,LM
	      IF(PMID(1,1,L).GE.(PDTOP+PT))EXIT
	     END DO
	     PRINT*,'hybrid boundary ',L
            END IF 
            CALL MPI_BCAST(L,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
          if(grib=='grib1') then
	    ID(10)=1
	    ID(11)=L  
            CALL GRIBIT(IGET(283),LVLS(1,IGET(283)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(283))
            fld_info(cfld)%lvl1=1
            fld_info(cfld)%lvl2=L
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF
!      
!       SIGMA PRESSURE THICKNESS REQUESTED BY CMAQ
      IF (IGET(273).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=PD(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
            IF(ME == 0)THEN
             DO L=1,LM
!              print*,'Debug CMAQ: ',L,PINT(1,1,LM+1),PD(1,1),PINT(1,1,L)
              IF((PINT(1,1,LM+1)-PD(1,1)).LE.(PINT(1,1,L)+1.00))EXIT
             END DO
             PRINT*,'hybrid boundary ',L
            END IF
            CALL MPI_BCAST(L,1,MPI_INTEGER,0,mpi_comm_comp,irtn)
            ID(10)=L
            ID(11)=LM+1
           if(grib=='grib1') then
            CALL GRIBIT(IGET(273),LVLS(1,IGET(273)),GRID1,IM,JM)
           elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(273))
            fld_info(cfld)%lvl1=L
            fld_info(cfld)%lvl2=LM+1
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
           endif
      ENDIF


!      TIME-AVERAGED EXCHANGE COEFFICIENTS FOR MASS REQUESTED FOR CMAQ
      IF (IGET(503).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=AKHSAVG(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
	    ID(02)= 133
         ID(19)     = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
            ID(20)     = 3
         if(grib=='grib1') then
            CALL GRIBIT(IGET(503),LVLS(1,IGET(503)),GRID1,IM,JM)
         elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(503))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF

!      TIME-AVERAGED EXCHANGE COEFFICIENTS FOR WIND REQUESTED FOR CMAQ
      IF (IGET(504).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=AKMSAVG(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
	    ID(02)= 133
         ID(19)     = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
            ID(20)     = 3
         if(grib=='grib1') then
            CALL GRIBIT(IGET(504),LVLS(1,IGET(504)),GRID1,IM,JM)
         elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(504))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif

      ENDIF
              

!      TIME-AVERAGED EXCHANGE COEFFICIENTS FOR MASS REQUESTED FOR CMAQ
      IF (IGET(503).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=AKHSAVG(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
            ID(02)= 133
         ID(19)     = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
            ID(20)     = 3
         if(grib=='grib1') then
            CALL GRIBIT(IGET(503),LVLS(1,IGET(503)),GRID1,IM,JM)
         else
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(503))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
       ENDIF

!      TIME-AVERAGED EXCHANGE COEFFICIENTS FOR WIND REQUESTED FOR CMAQ
      IF (IGET(504).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
             GRID1(I,J)=AKMSAVG(I,J)
            ENDDO
            ENDDO
            ID(1:25) = 0
            ID(02)= 133
         ID(19)     = IFHR
         IF (IFHR.EQ.0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
            ID(20)     = 3
         if(grib=='grib1') then
            CALL GRIBIT(IGET(504),LVLS(1,IGET(504)),GRID1,IM,JM)
         else
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(504))
            fld_info(cfld)%ntrange=IFHR-ID(18)
            fld_info(cfld)%tinvstat=1
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
         endif
      ENDIF

      RETURN
      END
