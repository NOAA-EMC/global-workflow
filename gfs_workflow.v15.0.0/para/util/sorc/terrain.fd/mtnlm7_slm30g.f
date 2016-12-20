!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  TERRAIN  TERRAIN MAKER FOR GLOBAL SPECTRAL MODEL
!   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 92-04-16
!
! ABSTRACT: THIS PROGRAM CREATES 7 TERRAIN-RELATED FILES
!   COMPUTED FROM THE NAVY 10-MINUTE TERRAIN DATASET.
!   THE MODEL PHYSICS GRID PARAMETERS AND SPECTRAL TRUNCATION
!   AND FILTER PARAMETERS ARE READ BY THIS PROGRAM AS INPUT.
!   THE 7 FILES PRODUCED ARE RESPECTIVELY:
!     1) SEA-LAND MASK ON MODEL PHYSICS GRID
!     2) GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
!     3) MOUNTAIN STD DEV ON MODEL PHYSICS GRID
!     4) SPECTRAL OROGRAPHY IN SPECTRAL DOMAIN
!     5) UNFILTERED GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
!     6) GRIB SEA-LAND MASK ON MODEL PHYSICS GRID
!     7) GRIB GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
!   THE OROGRAPHY IS ONLY FILTERED FOR WAVENUMBERS GREATER THAN NF0.
!   FOR WAVENUMBERS N BETWEEN NF0 AND NF1, THE OROGRAPHY IS FILTERED
!   BY THE FACTOR 1-((N-NF0)/(NF1-NF0))**2.  THE FILTERED OROGRAPHY
!   WILL NOT HAVE INFORMATION BEYOND WAVENUMBER NF1.
!
! PROGRAM HISTORY LOG:
!   92-04-16  IREDELL
!   98-02-02  IREDELL  FILTER
!   98-05-31  HONG Modified for subgrid orography used in Kim's scheme
!   98-12-31  HONG Modified for high-resolution GTOPO orography
!   99-05-31  HONG Modified for getting OL4 (mountain fraction)
!   00-02-10  Moorthi's modifications including lat/lon grid
!   00-04-11  HONG Modified for reduced grids
!   00-04-12  Iredell Modified for reduced grids
!   02-01-07  (*j*) modified for principal axes of orography
!             There are now 14 files, 4 additional for lm mb
!   04-04-04  (*j*) re-Test on IST/ilen calc for sea-land mask(*j*)
!   04-09-04   minus sign here in MAKEOA IST and IEN as in MAKEMT!
!   05-09-05   if test on HK and HLPRIM for GAMMA SQRT
!   07-08-07   replace 8' with 30" incl GICE, conintue w/ S-Y. lake slm
!   08-08-07  All input 30", UMD option, and filter as described below
! --- Quadratic filter applied by default.
! --- NF0 is normally set to an even value beyond the previous truncation, 
! --- for example, for jcap=382, NF0=254+2
! --- NF1 is set as jcap+2 (and/or nearest even), eg., for t382, NF1=382+2=384
! --- if no filter is desired then NF1=NF0=0 and ORF=ORO
! --- but if no filter but spectral to grid (with gibbs) then NF1=jcap+2, and NF1=jcap+1
!   11-06-22 S. Moorthi - convert the code to "implicit none" added grib
!                         output of unfiltered orography
!   13-02-20 S. Moorthi - Added SPTEZJ so that the filter can be applied
!                         at resolutions t1534 and higher
!                         Also optimized to code to use less memory
!   13-06-19 S. Moorthi - Made it work on wcoss
!       
!
! USAGE:
!
!   INPUT FILES:
!     UNIT5      - PHYSICS LONGITUDES (IM), PHYSICS LATITUDES (JM),
!                  SPECTRAL TRUNCATION (NM), RHOMBOIDAL FLAG (NR),
!                  AND FIRST AND SECOND FILTER PARAMETERS (NF0,NF1).
!                  RESPECTIVELY READ IN FREE FORMAT.
!     UNIT235    - GTOPO 30" AVR for ZAVG elevation
!     UNIT10     - 30" UMD land (lake) cover mask  see MSKSRC switch
!    XUNIT11     - GTOPO AVR
!    XUNIT12     - GTOPO STD DEV
!    XUNIT13     - GTOPO MAX
!     UNIT14     - GTOPO SLM (10' NAVY if switched to get lakes)
!
!   OUTPUT FILES:
!     UNIT51     - SEA-LAND MASK (IM,JM)
!     UNIT52     - GRIDDED OROGRAPHY (IM,JM)
!     UNIT53     - MOUNTAIN STD DEV (IM,JM)
!     UNIT54     - SPECTRAL OROGRAPHY ((NM+1)*((NR+1)*NM+2))
!     UNIT55     - UNFILTERED GRIDDED OROGRAPHY (IM,JM)
!     UNIT56     - GRIB SEA-LAND MASK (IM,JM)
!     UNIT57     - GRIB GRIDDED OROGRAPHY (IM,JM)
!     UNIT58     - GRIB PRINCIPAL COORD THETA (IM,JM)
!     UNIT59     - GRIB PRINCIPAL COORD SIGMA (IM,JM)
!     UNIT60     - GRIB PRINCIPAL COORD GAMMA  (IM,JM)
!     UNIT61     - GRIB MOUNTAIN STD VAR  (IM,JM)
!     UNIT62     - GRIB MOUNTAIN MAX ELEVATION (IM,JM)
!
!   SUBPROGRAMS CALLED:
!     UNIQUE:
!     TERSUB     - MAIN SUBPROGRAM
!     read_g     - read in 30" elevations
!     SPLAT      - COMPUTE GAUSSIAN LATITUDES OR EQUALLY-SPACED LATITUDES
!     LIBRARY:
!     SPTEZ      - SPHERICAL TRANSFORM
!     SPTEZJ     - SPHERICAL TRANSFORM
!     GBYTES     - UNPACK BITS
!
!   REMARKS: FORTRAN 9X EXTENSIONS ARE USED.
!            ITOPO determines if the 43200X21600 topo 30" is read in 
!            from the 30" array record. .DEM tiles are done offline.
!           
! ATTRIBUTES:
!   CRAY YMP & IBM AIX 3 5 00C88D5D4C00.
!C
!$$$
!FPP$ NOCONCUR F
      implicit none
!
      integer MTNRES, IM, JM, NM, NR, NF0, NF1, NW, IMN, JMN, latch
      real    EFAC,BLAT
!
      latch = 1
      READ(5,*) MTNRES,IM,JM,NM,NR,NF0,NF1,EFAC,BLAT

! --- MTNRES defines the input (highest) elev resolution
! --- =1 is topo30 30" in units of 1/2 minute.
!     so MTNRES for old values must be *2.
!     =16 is now Song Yu's 8' orog the old ops standard
! --- other possibilities are =8 for 4' and =4 for 2' see
!     HJ for T1000 test. Must set to 1 for now.

      MTNRES = 1
      print*, MTNRES,IM,JM,NM,NR,NF0,NF1,EFAC,BLAT

      NW  = (NM+1)*((NR+1)*NM+2)
      IMN = 360*120/MTNRES
      JMN = 180*120/MTNRES
      print *, ' Starting terr mtnlm7_slm10.f  IMN,JMN:',IMN,JMN

      call start()

      CALL TERSUB(IMN,JMN,IM,JM,NM,NR,NF0,NF1,NW,EFAC,BLAT,latch)

!     call summary()
      STOP
      END
      SUBROUTINE TERSUB(IMN,JMN,IM,JM,NM,NR,NF0,NF1,NW,EFAC,BLAT,latch)
      implicit none
!
      integer, parameter :: NMT=14
      logical, parameter :: check_nans=.false.
!     logical, parameter :: check_nans=.true.
!
      integer IMN,JMN,IM,JM,NM,NR,NF0,NF1,NW
      real    efac, blat
      INTEGER ZSLMX(2700,1350)

      INTEGER,  allocatable::  ZAVG(:,:),ZSLM(:,:)
      REAL(4),  allocatable::  GICE(:,:)
      integer*1,allocatable:: UMD(:,:)

      integer latch
      integer*1 i3save
      integer*2 glob(IMN,JMN), i2save
      INTEGER KPDS(200),KGDS(200), zsave1,zsave2,itopo,kount
      INTEGER kount2, islmx, jslmx, oldslm, msksrc
      REAL    COSCLT(JM),  WGTCLT(JM),  RCLT(JM),  XLAT(JM),DIFFX(JM/2)

      REAL    SLM(IM,JM),  ORO(IM,JM),  ORS(NW),ORF(IM,JM)

      REAL, allocatable ::  VAR(:,:),   VAR4(:,:),  OA(:,:,:), OL(:,:,:)&
     &,                     THETA(:,:), GAMMA(:,:), SIGMA(:,:)          &
     &,                     ELVMAX(:,:)
      real    oro_s(im,jm)
      integer IST(IM,jm),  IEN(IM,jm),  JST(JM),JEN(JM)
      integer, allocatable :: IWORK(:,:,:)
      real    glat(jmn)
      real, allocatable :: work1(:,:),work2(:,:), work3(:,:)            &
     &,                    work4(:,:), work5(:,:), work6(:,:),          &
     &                     hprime(:,:,:)

      LOGICAL SPECTR, REVLAT, FILTER
      integer numi(jm),ios,iosg,latg2,istat
      integer maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
      integer lonsperlat(jm/2),itest,jtest, i, j, k
     &,       it, jt, i1, jn, js, iw, ie, in, inw, ine, m, n, imt
     &,       is, ise, isw, lb, iret, imb2p1
      real    oaa(4), ola(4), sumdif, avedif, alon, alat, pi
     &,       degrad, rn, rs, slma, oroa, vara, var4a, wgta, xn, xs
     &,       fff, www, phi, delxn
      complex ffj(im/2+1)

      allocate (ZAVG(IMN,JMN))
      allocate (ZSLM(IMN,JMN))
      allocate (GICE(IMN+1,3601))
      allocate (UMD(IMN,JMN))
      allocate (iwork(im,jm,4))
      allocate (work1(im,jm), work2(im,jm), work3(im,jm)                &
     &,         work4(im,jm), work5(im,jm), work6(im,jm)                &
     &,         hprime(im,jm,nmt))
      allocate (VAR(im,jm),   VAR4(im,jm),  OA(im,jm,4),  OL(im,jm,4)   &
     &,         THETA(im,jm), GAMMA(im,jm), SIGMA(im,jm), ELVMAX(im,jm))

!
!  SET CONSTANTS AND ZERO FIELDS
!
      imb2p1 = im/2 + 1
      pi     = 4.0 * atan(1.0)
      DEGRAD = 180./PI
      SPECTR = NM > 0        ! if NM <=0 then grid is assumed to be lat/lon
      FILTER = .TRUE.        ! Spectr Filter defaults true and set by NF1 & NF0

                             ! MSKSRC=0 navy 10 lake msk, 1 UMD 30, -1 no lakes
!     MSKSRC = 0
      MSKSRC = 1

      REVLAT = BLAT  <  0    ! Reverse latitude/longitude for output
      ITOPO  = 1             ! topo 30" read, otherwise tiles (opt offline)

      write(0,*)' In TERSUB, ITOPO=',itopo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! --- old S-Y. files
!- OPEN(UNIT=11,FORM='FORMATTED',ERR=900) ! average
!- OPEN(UNIT=12,FORM='FORMATTED',ERR=900) ! Std Dev
!- OPEN(UNIT=13,FORM='FORMATTED',ERR=900) ! maximum
!- OPEN(UNIT=14,FORM='FORMATTED',ERR=900) ! sea-land-lake-mask
!
! ---      READ(11,11) ZAVG
! ---      READ(12,11) ZVAR
! ---      READ(13,11) ZMAX
! --- 11    FORMAT(20I4)
!
! ---  MSKSRC 0 navy 10' lake mask, =1 for 30" UMD lake mask, 
! ---  MSKSRC internally set if above fails at -1 for no lakes
! ---
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF (MSKSRC  ==  0 ) then 
        READ(14,12,iostat=ios) ZSLMX
   12   FORMAT(80I1)
        if (ios /= 0) then 
          MSKSRC = -1
          print *,' navy10 lake mask rd fail -- ios,MSKSRC:',ios,MSKSRC
        endif
      ELSE
        write(0,*)' Attempt to open/read UMD 30" slmsk MSKSRC=',MSKSRC
! ---   not 0 so MSKSRC=1 and attempt to open/read UMD 30" slmsk

!       open(10,file="/global/noscrub/wx23ja/terrain30/landcover30.fixed
!    &", recl=43200*21600, access='direct',iostat=istat)

        open(10,file="landcover30.fixed", recl=43200*21600,
     &                                    access='direct',iostat=istat)
        IF (istat /= 0) then
          MSKSRC = -1
          print *,' UMD lake mask open failed -- ios,MSKSRC:',ios,MSKSRC
        ELSE 
!
          read(10, rec=1,iostat=istat) UMD

        ENDIF
! --------------
        IF (istat /= 0) then ! --- When UMD read fails attempt to read navy 10'
          print *,' UMD lake mask rd err -- trying navy 10',istat
          MSKSRC = 0
          print *,' ***** MSKSRC set to 0 MSKSRC=',MSKSRC
          if (MSKSRC == 0 ) then 
            rewind 14
            READ(14,12,iostat=ios) ZSLMX
            if (ios /= 0)  then
              MSKSRC = -1
              print *,' navy10 lake mask rd fail - ios,MSKSRC:',ios
     &,                 MSKSRC
            endif
          endif
        ELSE
          print *,' UMD lake, UMD(500,500)=',UMD(500,500),MSKSRC
        ENDIF
! --------------
! ---      good UMD land cover read and MSKSRC = 1
      ENDIF        
!
!- READ_G for global 30" terrain 
!
      print *,' Read 30" topography or call read_g, ITOPO=',ITOPO

      if (itopo /= 0) then
        read(235) glob
        rewind(235)
!     elseif ( ITOPO /= 0 )then
!       call read_g(glob,ITOPO)
      endif

! --- transpose even though glob 30" is from S to N and NCEP std is N to S

      do j=1,jmn/2
        jt = jmn - j + 1
        do I=1,imn
          i2save     = glob(I,j)
          glob(I,j)  = glob(I,jt)
          glob(I,jt) = i2save
        enddo
      enddo 
! --- transpose glob as USGS 30" is from dateline and NCEP std is 0
      do j=1,jmn
        do I=1,imn/2
         it         = imn/2 + i 
         i2save     = glob(i,J)
         glob(i,J)  = glob(it,J)
         glob(it,J) = i2save
        enddo
      enddo 
      print *,' After read_g, glob(500,500)=',glob(500,500)
!

!  --- IMN,JMN
      write(0,*)' IM, JM, NM, NR, NF0, NF1, EFAC, BLAT'
      write(0,*) IM,JM,NM,NR,NF0,NF1,EFAC,BLAT
      write(0,*)'  imn,jmn,glob(imn,jmn)=',imn,jmn,glob(imn,jmn)
      write(0,*)' UBOUND ZAVG=',UBOUND(ZAVG)
      write(0,*)' UBOUND glob=',UBOUND(glob)
      write(0,*)' UBOUND ZSLM=',UBOUND(ZSLM)
      write(0,*)' UBOUND GICE=',UBOUND(GICE)

      kount  = 0
      kount2 = 0
!
! ---  0 is ocean and 1 is land for slm
!
      ZSLM = 1

      SELECTCASE(MSKSRC)

        CASE(1)     !----  30" sea land mask. 0 are water (lake or ocean)
                    !      ----------------------------------------------

! --- transpose even though glob 30" is from S to N and NCEP std is N to S
          do j=1,jmn/2
            jt = jmn - j + 1
            do I=1,imn
              i3save    = UMD(I,j)
              UMD(I,j)  = UMD(I,jt)
              UMD(I,jt) = i3save
            enddo
          enddo 
! --- transpose UMD as USGS 30" is from dateline and NCEP std is 0
          do j=1,jmn
            do i=1,imn/2
              it        = imn/2 + i 
              i3save    = UMD(i,J)
              UMD(i,J)  = UMD(it,J)
              UMD(it,J) = i3save
            enddo
          enddo
! ---   UMD slmsk with 30" lakes  and set ZAVG from glob
          do j=1,jmn
            do i=1,imn
              if ( UMD(i,j) == 0 ) ZSLM(i,j) = 0
              ZAVG(i,j) = glob(i,j)
            enddo
          enddo
!
        CASE(0)     ! ---   When navy 10' mask is set MSKSRC=0
                    !      -----------------------------------

! ---  MSKSRC 0 navy 10' lake mask, =1 for 30" UMD lake mask, -1 no lakes
       write(0,*)' NAVY 10 (8) slmsk for lakes, MSKSRC=',MSKSRC 

          kount  = 0
          kount2 = 0
          do j=1,jmn
            oldslm = ZSLM(IMN,j)
            do i=1,imn
              i1 = i + 1
! ---                    slmsk with 10' lakes  and set ZAVG from 30" glob
              ZAVG(i,j) = glob(i,j)
              if ( glob(i,j) == -9999 ) then 
                ZSLM(i,j) = 0
                kount     = kount + 1
              endif
              islmx = (i-1)/16 + 1
              jslmx = (j-1)/16 + 1
              if ( ZSLMX(islmx,jslmx) == 0 ) then
                if ( j > 8 .and. j < JMN-8 ) then
                  if (i1 > IMN ) i1 = i1 - IMN
! -----
                  if(ZSLM(i,j) == 1 .and. oldslm == 1
     &                              .and. ZSLM(i1,j) == 1) then  
!                   if (i /= 1) oldslm = ZSLM(i,j)
                    ZSLM(i,j) = 0
                    kount2    = kount2 + 1
                  endif 
! -----
                endif
              endif
            enddo
          enddo
! ---
        CASE(-1)
          print *,' **** set ZAVG and slm from 30" glob, MSKSRC=',MSKSRC
          kount  = 0
          kount2 = 0
          do j=1,jmn
            do i=1,imn
              i1 = i + 1
! ---                    UMD slmsk with 10' lakes  and set ZAVG from 30" glob
              ZAVG(i,j) = glob(i,j)
              if ( glob(i,j) == -9999 ) then 
                ZSLM(i,j) = 0
                kount     = kount + 1
              endif
            enddo
          enddo
      END SELECT
! ---
! ---  There was an error in the topo 30" data set at pole (-9999).  
      do i=1,imn
        ZSLM(i,1)   = 0
        ZSLM(i,JMN) = 1
      enddo
!
      write(0,*)' kount,2,ZAVG(1,1),ZAVG(imn,jmn),ZAVG(500,500)',
     &          kount,kount2,ZAVG(1,1),ZAVG(imn,jmn),ZAVG(500,500)

! --- The center of pixel (1,1) is 89.9958333N/179.9958333W with dx/dy 
! --- spacing of 1/120 degrees. 
!
!  READ REDUCED GRID EXTENTS IF GIVEN
!
      read(20,*,iostat=ios) latg2,lonsperlat
      if (ios /= 0 .or. 2*latg2 /= jm) then
        do j=1,jm
          numi(j) = im
        enddo
        write(0,*) ios,latg2,'COMPUTE TERRAIN ON A FULL GAUSSIAN GRID'
      else
        do j=1,jm/2
          numi(j) = lonsperlat(j)
        enddo
        do j=jm/2+1,jm
          numi(j) = lonsperlat(jm+1-j)
        enddo
        write(0,*) ios,latg2,'COMPUTE TERRAIN ON A REDUCED GAUSSIAN'
     &' GRID', numi
!       print *,ios,latg2,'COMPUTE TERRAIN ON A REDUCED GAUSSIAN GRID'
      endif

      write(0,*) ios,latg2,'TERRAIN ON GAUSSIAN GRID',numi
      
!
!    This code assumes that lat runs from north to south for gg!
!
      write(0,*)' SPECTR=',SPECTR,' REVLAT=',REVLAT,'** with GICE-07 **'

      IF (SPECTR) THEN
        CALL SPLAT(4,JM,COSCLT,WGTCLT)
        DO J=1,JM/2
          RCLT(J)      = ACOS(COSCLT(J))
          PHI          = RCLT(J) * DEGRAD
          XLAT(J)      = 90. - PHI
          XLAT(JM-J+1) = PHI - 90.
        ENDDO
      ELSE
        CALL SPLAT(0,JM,COSCLT,WGTCLT)
        DO J=1,JM
          RCLT(J) = ACOS(COSCLT(J))
          XLAT(J) = 90.0 - RCLT(J) * DEGRAD
        ENDDO
      ENDIF
!
!      print *,' cosclt=',cosclt
       print *,' RCLT(1)=',RCLT(1)

       sumdif = 0.
       DO J = JM/2,2,-1
         DIFFX(J) = xlat(J) - XLAT(j-1)
         sumdif   = sumdif + DIFFX(J)
       ENDDO
       avedif = sumdif / (float(JM/2))

       write(0,*)' XLAT= avedif: ',avedif
       write (6,107) (xlat(J)-xlat(j-1),J=JM,2,-1)
       print *,' XLAT='
       write (6,106) (xlat(J),J=JM,1,-1)
  106 format( 10(f7.3,1x))   
  107 format( 10(f9.5,1x))   
!
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
!
      DO J=1,JMN
        GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO

      write(0,*)' Before GICE ZAVG(1,2)=',ZAVG(1,2),ZSLM(1,2)
      write(0,*)' Before GICE ZAVG(1,12)=',ZAVG(1,12),ZSLM(1,12)
      write(0,*)' Before GICE ZAVG(1,52)=',ZAVG(1,52),ZSLM(1,52)
      write(0,*)' Before GICE ZAVG(1,112)=',ZAVG(1,JMN-112),ZSLM(1,112)

! GICE: Grumbine 30" Antarctica orog IMNx3616 from S to N & wraped E-W.
! NB: Zfields are S to N and W-E!

       iosg = 0
       READ(15,iostat=iosg) GICE
       if (iosg /= 0 ) then
         write(0,*)' *** Err on reading GICE record, iosg=',iosg
         write(0,*)' exec continues but NO GICE correction done '
!        stop
       else
         write(0,*)' GICE 30" Antarctica RAMP orog 43200x3616 read OK'
         write(0,*)' Processing! '
         write(0,*)' Processing! '
         write(0,*)' Processing! '
         do j = 1, 3601 
           do i = 1, IMN
             zsave1 = ZAVG(i,j)
             zsave2 = ZSLM(i,j)
             if( GICE(i,j) /= -99. .and.  GICE(i,j) /= -1.0 ) then
               if ( GICE(i,j) >  0.) then 
                 ZAVG(i,j) = int( GICE(i,j) + 0.5 )
!! --- for GICE values less than or equal to 0 (0, -1, or -99) then
!! --- radar-sat (RAMP) values are not valid and revert back to old orog 
                 ZSLM(i,j) = 1
               endif
             endif
             ALON = float(i-1) * 360./float(IMN)
             ALAT = glat(j)

!            if(  ZAVG(i,j) .ne. zsave1 .and. i .lt. 3 )
!    & print *,' antarctica change to ZAVG(i=',i,'j=',j,')=',
!    &    ZAVG(i,j),ZSLM(i,j),' from originally:',zsave1,zsave2
!     &write(6,151)i,j,ZAVG(i,j),ZSLM(i,j),zsave1,zsave2,ALAT,ALON
!  151 format(1x,'antarctica ZAVG(i=',i3,' j=',i3,')=',i5,i3,
!     &' orig:',i5,i3,' Lat=',f8.3,f9.3,'E')

             if(  ZAVG(i,j) /= zsave1 ) then 
               if ( i <= 1201 .and. i > 1200 )then
                 write(6,152) i,j,ZAVG(i,j),ZSLM(i,j),zsave1,zsave2,
     &                        ALAT,ALON,GICE(i,j)
  152            format(1x,' ZAVG(i=',i4,' j=',i4,')=',i5,i3,
     &           ' orig:',i5,i4,' Lat=',f7.3,f8.2,'E',' GICE=',f8.1)
               endif
             endif
           enddo
         enddo
       endif

!      print *,
!     & ' After GICE ZAVG(1,2)=',ZAVG(1,2),ZSLM(1,2)
!      print *,
!     & ' After GICE ZAVG(1,12)=',ZAVG(1,12),ZSLM(1,12)
!      print *,
!     & ' After GICE ZAVG(1,52)=',ZAVG(1,52),ZSLM(1,52)
!      print *,
!     & ' After GICE ZAVG(1,112)=',ZAVG(1,112),ZSLM(1,112)
!
!     COMPUTE MOUNTAIN DATA : ORO SLM VAR (Std Dev) OC
!
      if (numi(1) < im) then
        do j=1,jm
          do i=numi(j)+1,im
            oro(i,j)  = 0.0
            slm(i,j)  = 0.0
            var(i,j)  = 0.0
            var4(i,j) = 0.0
          enddo
        enddo
      endif
!
      CALL MAKEMT(ZAVG,ZSLM,ORO,SLM,VAR,VAR4,GLAT,
     &            IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
       call minmxj(IM,JM,ORO,'     ORO')
       call minmxj(IM,JM,SLM,'     SLM')
       call minmxj(IM,JM,VAR,'     VAR')
       call minmxj(IM,JM,VAR4,'    VAR4')
       if (check_nans) then ! --- check for nands in above
         call nanc(ORO,IM*JM,"MAKEMT_ORO")
         call nanc(SLM,IM*JM,"MAKEMT_SLM")
         call nanc(VAR,IM*JM,"MAKEMT_VAR")
         call nanc(VAR4,IM*JM,"MAKEMT_VAR4")
       endif
!
!   check antarctic pole 
!     DO J = 1,JM
!       DO I = 1,numi(j)
!         if ( i .le. 100 .and. i .ge. 1 )then
!           if (j .ge. JM-1 )then
!             if (height .eq. 0.) print *,'I,J,SLM:',I,J,SLM(I,J)
!             write(6,153)i,j,ORO(i,j),HEIGHT,SLM(i,j)
!           endif
!        endif    
!       ENDDO
!     ENDDO

!     write(0,*)' ORO=',oro(:,:)
!
! ===  Compute mtn principal coord HTENSR: THETA,GAMMA,SIGMA
!
      if (numi(1) < im) then
        do j=1,jm
          do i=numi(j)+1,im
            theta(i,j) = 0.0
            gamma(i,j) = 0.0
            sigma(i,j) = 0.0
          enddo
        enddo
      endif
!
      CALL MAKEPC(ZAVG,ZSLM,THETA,GAMMA,SIGMA,GLAT,
     1            IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
      call minmxj(IM,JM,THETA,'   THETA')
      call minmxj(IM,JM,GAMMA,'   GAMMA')
      call minmxj(IM,JM,SIGMA,'   SIGMA')
      if (check_nans) then ! --- check for nands in above
        call nanc(THETA,IM*JM,"MAKEPC_THETA")
        call nanc(GAMMA,IM*JM,"MAKEPC_GAMMA")
        call nanc(SIGMA,IM*JM,"MAKEPC_SIGMA")
      endif
!
!     COMPUTE MOUNTAIN DATA : OA OL
!
      if (numi(1) < im) then
        do j=1,jm
          do i=numi(j)+1,im
            oa(i,j,:)   = 0.0
            ol(i,j,:)   = 0.0
            elvmax(i,j) = 0.0
          enddo
        enddo
      endif
!
      call minmxj(IM,JM,ORO,'     ORO')
      CALL MAKEOA(ZAVG,VAR,GLAT,OA,OL,IWORK,ELVMAX,ORO,
     &            WORK1,WORK2,WORK3,WORK4,
     &            WORK5,WORK6,
     &            IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
      call minmxj(IM,JM,OA,'      OA')
      call minmxj(IM,JM,OL,'      OL')
      call minmxj(IM,JM,ELVMAX,'  ELVMAX')
      call minmxj(IM,JM,ORO,'     ORO')
      if (check_nans) then ! --- check for nands in above
! --- check for nands in above
        call nanc(OA(1,1,1), IM*JM,"MAKEOA_OA(1,1,1)")
        call nanc(OA(1,1,2), IM*JM,"MAKEOA_OA(1,1,2)")
        call nanc(OA(1,1,3), IM*JM,"MAKEOA_OA(1,1,3)")
        call nanc(OA(1,1,4), IM*JM,"MAKEOA_OA(1,1,4)")
        call nanc(OL(1,1,1), IM*JM,"MAKEOA_OL(1,1,1)")
        call nanc(OL(1,1,2), IM*JM,"MAKEOA_OL(1,1,2)")
        call nanc(OL(1,1,3), IM*JM,"MAKEOA_OL(1,1,3)")
        call nanc(OL(1,1,4), IM*JM,"MAKEOA_OL(1,1,4)")
        call nanc(ELVMAX, IM*JM,"MAKEPC_ELVMAX")
      endif

      maxc3 = 0
      maxc4 = 0
      maxc5 = 0
      maxc6 = 0
      maxc7 = 0
      maxc8 = 0
      DO J = 1,JM
        DO I = 1,numi(j)
          if (ELVMAX(I,J) > 3000.) maxc3 = maxc3 +1
          if (ELVMAX(I,J) > 4000.) maxc4 = maxc4 +1
          if (ELVMAX(I,J) > 5000.) maxc5 = maxc5 +1
          if (ELVMAX(I,J) > 6000.) maxc6 = maxc6 +1
          if (ELVMAX(I,J) > 7000.) maxc7 = maxc7 +1
          if (ELVMAX(I,J) > 8000.) maxc8 = maxc8 +1
        ENDDO
      ENDDO
      write(0,*)' MAXC3:',maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
!
!     itest = 151
!     jtest = 56
!      
      write(0,*)' ===> Replacing ELVMAX with ELVMAX-ORO <=== ' 
      write(0,*)' ===> if ELVMAX<=ORO replace with proxy <=== ' 
      write(0,*)' ===> the sum of mean orog (ORO) and std dev <=== ' 
      DO J = 1,JM
        DO I = 1,numi(j)
          if (ELVMAX(I,J) < ORO(I,J) ) then
!---  subtracting off ORO leaves std dev (this should never happen)
            ELVMAX(I,J) = MAX(  3. * VAR(I,J),0.)
          else
            ELVMAX(I,J) = MAX( ELVMAX(I,J) - ORO(I,J),0.)
          endif
        ENDDO
      ENDDO
      maxc3 = 0
      maxc4 = 0
      maxc5 = 0
      maxc6 = 0
      maxc7 = 0
      maxc8 = 0
      DO J = 1,JM
        DO I = 1,numi(j)
          if (ELVMAX(I,J) > 3000.) maxc3 = maxc3 +1
          if (ELVMAX(I,J) > 4000.) maxc4 = maxc4 +1
          if (ELVMAX(I,J) > 5000.) maxc5 = maxc5 +1
          if (ELVMAX(I,J) > 6000.) maxc6 = maxc6 +1
          if (ELVMAX(I,J) > 7000.) maxc7 = maxc7 +1
          if (ELVMAX(I,J) > 8000.) maxc8 = maxc8 +1
        ENDDO
      ENDDO
      write(0,*)' after MAXC 3-6 km:',maxc3,maxc4,maxc5,maxc6
!
       call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
!     if (JM .gt. 0) stop

      deallocate (ZAVG)
      deallocate (ZSLM)
      deallocate (UMD)
      deallocate (GICE)
      deallocate (work3,work4,work5,work6,iwork)
!
!     ZERO OVER OCEAN
!
      write(0,*)' Testing at point (itest,jtest)=',itest,jtest
!     print *,' SLM(itest,jtest)=',slm(itest,jtest)
      write(0,*)' ORO(itest,jtest)=',oro(itest,jtest)

      DO J = 1,JM
        DO I = 1,numi(j)
          IF(SLM(I,J) == 0.0) THEN
!           VAR(I,J)    = 0.
            VAR4(I,J)   = 0.
            OA(I,J,1)   = 0.
            OA(I,J,2)   = 0.
            OA(I,J,3)   = 0.
            OA(I,J,4)   = 0.
            OL(I,J,1)   = 0.
            OL(I,J,2)   = 0.
            OL(I,J,3)   = 0.
            OL(I,J,4)   = 0.
!           THETA(I,J)  = 0.
!           GAMMA(I,J)  = 0.
!           SIGMA(I,J)  = 0.
!           ELVMAX(I,J) = 0.
          ENDIF
       ENDDO
      ENDDO
!
!  REMOVE ISOLATED POINTS
!
      DO J=2,JM-1
        JN = J - 1
        JS = J + 1
        RN = REAL(NUMI(JN)) / REAL(NUMI(J))
        RS = REAL(NUMI(JS)) / REAL(NUMI(J))
        DO I=1,NUMI(J)
          IW    = MOD(I+IM-2,IM) + 1
          IE    = MOD(I,IM)      + 1
          SLMA  = SLM(IW,J)      + SLM(IE,J)
          OROA  = ORO(IW,J)      + ORO(IE,J)
          VARA  = VAR(IW,J)      + VAR(IE,J)
          VAR4A = VAR4(IW,J)     + VAR4(IE,J)
          DO K=1,4
            OAA(K) = OA(IW,J,K)  + OA(IE,J,K)
! --- (*j*) fix typo: August 27, 2012
!           OLA(K) = OA(IW,J,K)  + OA(IE,J,K)
            OLA(K) = OL(IW,J,K)  + OL(IE,J,K)
          ENDDO
          WGTA = 2
          XN   = RN*(I-1) + 1
          IF (ABS(XN-NINT(XN)) < 1.E-2) THEN
            IN    = MOD(NINT(XN)-1,NUMI(JN))    + 1
            INW   = MOD(IN+NUMI(JN)-2,NUMI(JN)) + 1
            INE   = MOD(IN,NUMI(JN))            + 1
            SLMA  = SLMA  + SLM(INW,JN)  + SLM(IN,JN)  + SLM(INE,JN)
            OROA  = OROA  + ORO(INW,JN)  + ORO(IN,JN)  + ORO(INE,JN)
            VARA  = VARA  + VAR(INW,JN)  + VAR(IN,JN)  + VAR(INE,JN)
            VAR4A = VAR4A + VAR4(INW,JN) + VAR4(IN,JN) + VAR4(INE,JN)
            DO K=1,4
             OAA(K) = OAA(K) + OA(INW,JN,K) + OA(IN,JN,K) + OA(INE,JN,K)
             OLA(K) = OLA(K) + OL(INW,JN,K) + OL(IN,JN,K) + OL(INE,JN,K)
            ENDDO
            WGTA = WGTA + 3
          ELSE
            INW   = INT(XN)
            INE   = MOD(INW,NUMI(JN))  + 1
            SLMA  = SLMA+SLM(INW,JN)   + SLM(INE,JN)
            OROA  = OROA+ORO(INW,JN)   + ORO(INE,JN)
            VARA  = VARA+VAR(INW,JN)   + VAR(INE,JN)
            VAR4A = VAR4A+VAR4(INW,JN) + VAR4(INE,JN)
            DO K=1,4
              OAA(K) = OAA(K) + OA(INW,JN,K) + OA(INE,JN,K)
              OLA(K) = OLA(K) + OL(INW,JN,K) + OL(INE,JN,K)
            ENDDO
            WGTA = WGTA + 2
          ENDIF
          XS = RS*(I-1)+1
          IF(ABS(XS-NINT(XS)) < 1.E-2) THEN
            IS    = MOD(NINT(XS)-1,NUMI(JS))    + 1
            ISW   = MOD(IS+NUMI(JS)-2,NUMI(JS)) + 1
            ISE   = MOD(IS,NUMI(JS))            + 1
            SLMA  = SLMA  + SLM(ISW,JS)  + SLM(IS,JS)  + SLM(ISE,JS)
            OROA  = OROA  + ORO(ISW,JS)  + ORO(IS,JS)  + ORO(ISE,JS)
            VARA  = VARA  + VAR(ISW,JS)  + VAR(IS,JS)  + VAR(ISE,JS)
            VAR4A = VAR4A + VAR4(ISW,JS) + VAR4(IS,JS) + VAR4(ISE,JS)
            DO K=1,4
             OAA(K) = OAA(K) + OA(ISW,JS,K) + OA(IS,JS,K) + OA(ISE,JS,K)
             OLA(K) = OLA(K) + OL(ISW,JS,K) + OL(IS,JS,K) + OL(ISE,JS,K)
            ENDDO
            WGTA = WGTA + 3
          ELSE
            ISW   = INT(XS)
            ISE   = MOD(ISW,NUMI(JS)) + 1
            SLMA  = SLMA  + SLM(ISW,JS)  + SLM(ISE,JS)
            OROA  = OROA  + ORO(ISW,JS)  + ORO(ISE,JS)
            VARA  = VARA  + VAR(ISW,JS)  + VAR(ISE,JS)
            VAR4A = VAR4A + VAR4(ISW,JS) + VAR4(ISE,JS)
            DO K=1,4
              OAA(K) = OAA(K) + OA(ISW,JS,K) + OA(ISE,JS,K)
              OLA(K) = OLA(K) + OL(ISW,JS,K) + OL(ISE,JS,K)
            ENDDO
            WGTA = WGTA + 2
          ENDIF
          OROA  = OROA  / WGTA
          VARA  = VARA  / WGTA
          VAR4A = VAR4A / WGTA
          DO K=1,4
            OAA(K) = OAA(K) / WGTA
            OLA(K) = OLA(K) / WGTA
          ENDDO
          IF(SLM(I,J) == 0..AND.SLMA == WGTA) THEN
            PRINT '("SEA ",2F8.0," MODIFIED TO LAND",2F8.0," AT ",2I8)',
     &       ORO(I,J),VAR(I,J),OROA,VARA,I,J
            SLM(I,J)  = 1.
            ORO(I,J)  = OROA
            VAR(I,J)  = VARA
            VAR4(I,J) = VAR4A
            DO K=1,4
              OA(I,J,K) = OAA(K)
              OL(I,J,K) = OLA(K)
            ENDDO
          ELSEIF(SLM(I,J) == 1. .AND. SLMA == 0.) THEN
            PRINT '("LAND",2F8.0," MODIFIED TO SEA ",2F8.0," AT ",2I8)',
     &       ORO(I,J),VAR(I,J),OROA,VARA,I,J
            SLM(I,J)  = 0.
            ORO(I,J)  = OROA
            VAR(I,J)  = VARA
            VAR4(I,J) = VAR4A
            DO K=1,4
              OA(I,J,K) = OAA(K)
              OL(I,J,K) = OLA(K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!--- print for testing after isolated points removed
      write(0,*)' after isolated points removed'

       call minmxj(IM,JM,ORO,'     ORO')

!     print *,' JM=',JM,' numi=',numi
      write(0,*)' ORO(itest,jtest)=',oro(itest,jtest)
      write(0,*)' VAR(itest,jtest)=',var(itest,jtest)
      write(0,*)' VAR4(itest,jtest)=',var4(itest,jtest)
      write(0,*)' OA(itest,jtest,1)=',oa(itest,jtest,1)
      write(0,*)' OA(itest,jtest,2)=',oa(itest,jtest,2)
      write(0,*)' OA(itest,jtest,3)=',oa(itest,jtest,3)
      write(0,*)' OA(itest,jtest,4)=',oa(itest,jtest,4)
      write(0,*)' OL(itest,jtest,1)=',ol(itest,jtest,1)
      write(0,*)' OL(itest,jtest,2)=',ol(itest,jtest,2)
      write(0,*)' OL(itest,jtest,3)=',ol(itest,jtest,3)
      write(0,*)' OL(itest,jtest,4)=',ol(itest,jtest,4)
      write(0,*)' Testing at point (itest,jtest)=',itest,jtest
      write(0,*)' THETA(itest,jtest)=',theta(itest,jtest)
      write(0,*)' GAMMA(itest,jtest)=',GAMMA(itest,jtest)
      write(0,*)' SIGMA(itest,jtest)=',SIGMA(itest,jtest)
      write(0,*)' ELVMAX(itest,jtest)=',ELVMAX(itest,jtest)
      write(0,*)' EFAC=',EFAC
!
      DO J=1,JM
        DO I=1,numi(j)
          ORO(I,J)       = ORO(I,J) + EFAC*VAR(I,J)
          HPRIME(I,J,1)  = VAR(I,J)
          HPRIME(I,J,2)  = VAR4(I,J)
          HPRIME(I,J,3)  = oa(I,J,1)
          HPRIME(I,J,4)  = oa(I,J,2)
          HPRIME(I,J,5)  = oa(I,J,3)
          HPRIME(I,J,6)  = oa(I,J,4)
          HPRIME(I,J,7)  = ol(I,J,1)
          HPRIME(I,J,8)  = ol(I,J,2)
          HPRIME(I,J,9)  = ol(I,J,3)
          HPRIME(I,J,10) = ol(I,J,4)
          HPRIME(I,J,11) = THETA(I,J)
          HPRIME(I,J,12) = GAMMA(I,J)
          HPRIME(I,J,13) = SIGMA(I,J)
          HPRIME(I,J,14) = ELVMAX(I,J)
        ENDDO
      ENDDO
!
      deallocate (VAR,  VAR4, OA, OL)
!
      call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')

! --- Quadratic filter applied by default.
! --- NF0 is normally set to an even value beyond the previous truncation, 
! --- for example, for jcap=382, NF0=254+2
! --- NF1 is set as jcap+2 (and/or nearest even), eg., for t382, NF1=382+2=384
! --- if no filter is desired then NF1=NF0=0 and ORF=ORO
! --- if no filter but spectral to grid (with gibbs) then NF1=jcap+2, and NF0=jcap+1
!
      oro_s = oro
!
      IF ( NF1 - NF0 == 0 ) FILTER = .FALSE.
      write(0,*)' NF1, NF0, FILTER=',NF1,NF0,FILTER

      IF (FILTER) THEN      !  SPECTRALLY TRUNCATE AND FILTER OROGRAPHY
        do j=1,jm
          if(numi(j) < im) then
            ffj = cmplx(0.,0.)
            call spfft1(numi(j),imb2p1,numi(j),1,ffj,oro(1,j),-1)
            call spfft1(im,     imb2p1,im,     1,ffj,oro(1,j),+1)
          endif
        enddo

!     write(0,*)' calling sptezj -1 nm=',nm,' nw=',nw,' im=',im
!    &,' jm=',jm,' latch=',latch

        CALL SPTEZ(NR,NM,4,IM,JM,ORS,ORO,-1)

!       CALL SPTEZJ(NM,NW,1,4,IM,JM,1,ORS,ORO,latch,-1)
!
        FFF = 1./(NF1-NF0)**2
        I = 0
        DO M=0,NM
          DO N=M,NM+NR*M
            IF(N > NF0) THEN
              WWW      = MAX(1.-FFF*(N-NF0)**2,0.)
              ORS(I+1) = ORS(I+1)*WWW
              ORS(I+2) = ORS(I+2)*WWW
            ENDIF
            I = I + 2
          ENDDO
        ENDDO
!
!     write(0,*),' calling sptezj +1 nm=',nm,' nw=',nw,' im=',im
!    &,' jm=',jm,' latch=',latch

        CALL SPTEZ(NR,NM,4,IM,JM,ORS,ORF,+1)
!       CALL SPTEZJ(NM,NW,1,4,IM,JM,1,ORS,ORF,latch,+1)

        do j=1,jm
          if(numi(j) < im) then
            call spfft1(im,     imb2p1,im,     1,ffj,orf(1,j),-1)
            call spfft1(numi(j),imb2p1,numi(j),1,ffj,orf(1,j),+1)
          endif
        enddo

      ELSE
        IF (REVLAT) THEN
          CALL REVERS(IM, JM, numi, SLM, WORK1)
          CALL REVERS(IM, JM, numi, ORO, WORK1)
          DO IMT=1,NMT
            CALL REVERS(IM, JM, numi, HPRIME(1,1,IMT), WORK1)
          ENDDO
        ENDIF
        ORS = 0.
        ORF = ORO
      ENDIF
      oro = oro_s
      call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')

      write(0,*)' ELVMAX(',itest,jtest,')=',ELVMAX(itest,jtest)
      write(0,*)' after spectral filter is applied'

      call minmxj(IM,JM,ORO,'     ORO')
      call minmxj(IM,JM,ORF,'     ORF')
!
!  USE NEAREST NEIGHBOR INTERPOLATION TO FILL FULL GRIDS
!
      call rg2gg(im,jm,numi,slm)
      call rg2gg(im,jm,numi,oro)
      call rg2gg(im,jm,numi,oro_s)
      call rg2gg(im,jm,numi,orf)
! ---   not apply to new prin coord and ELVMAX (*j*)
      do imt=1,nmt 
        call rg2gg(im,jm,numi,hprime(1,1,imt))
      enddo
! 
!     write(0,*),' after nearest neighbor interpolation applied '
      call minmxj(IM,JM,ORO,'     ORO')
      call minmxj(IM,JM,ORF,'     ORF')
      call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
      write(0,*)' ORO,ORF(itest,jtest),itest,jtest:',
     &          ORO(itest,jtest),ORF(itest,jtest),itest,jtest
      write(0,*)' ELVMAX(',itest,jtest,')=',ELVMAX(itest,jtest)
!   check antarctic pole 
      DO J = 1,JM
        DO I = 1,numi(j)
          if ( i <= min(numi(j),21) .and. i > 0 )then
            if (j == JM ) write(6,153)i,j,ORO(i,j),ELVMAX(i,j),SLM(i,j)
  153       format(1x,' ORO,ELVMAX(i=',i4,' j=',i4,')=',2E14.5,f5.1)
         endif
        ENDDO
      ENDDO

!     OUTPUT BINARY FIELDS

      WRITE(51) REAL(SLM,4)
      WRITE(52) REAL(ORF,4)
      WRITE(53) REAL(HPRIME,4)
      WRITE(54) REAL(ORS,4)
      WRITE(55) REAL(ORO,4)
      WRITE(66) REAL(THETA,4)
      WRITE(67) REAL(GAMMA,4)
      WRITE(68) REAL(SIGMA,4)
!
      call minmxj(IM,JM,ORO,'     ORO')

      write(0,*)' IM=',IM,' JM=',JM,' SPECTR=',SPECTR

!---    Test binary file output:
      WRITE(71) REAL(SLM,4)
      DO IMT=1,NMT
        WRITE(71) REAL(HPRIME(:,:,IMT),4)
        print *,' HPRIME(',itest,jtest,imt,')=',HPRIME(itest,jtest,imt)
      ENDDO
      WRITE(71) REAL(ORO,4)
      IF (SPECTR) THEN
        WRITE(71) REAL(ORF,4)   ! smoothed spectral orography!
      ENDIF

!  OUTPUT GRIB FIELDS

      KPDS     = 0
      KPDS(1)  = 7
      KPDS(2)  = 78
      KPDS(3)  = 255
      KPDS(4)  = 128
      KPDS(5)  = 81
      KPDS(6)  = 1
      kpds(8)  = 2004
      KPDS(9)  = 1
      KPDS(10) = 1
      KPDS(13) = 4
      KPDS(15) = 1
      KPDS(16) = 51
      KPDS(17) = 1
      KPDS(18) = 1
      KPDS(19) = 1
      KPDS(21) = 20
      KPDS(22) = 1

      KGDS     = 0
      KGDS(1)  = 4
      KGDS(2)  = IM
      KGDS(3)  = JM
      KGDS(4)  = 90000-180000/PI*RCLT(1)
      KGDS(6)  = 128
      KGDS(7)  = 180000/PI*RCLT(1)-90000
      KGDS(8)  = -NINT(360000./IM)
      KGDS(9)  =  NINT(360000./IM)
      KGDS(10) = JM/2
      KGDS(20) = 255

      CALL BAOPENwt(56,'fort.56',IRET)
      CALL PUTGB(56,IM*JM,KPDS,KGDS,LB,SLM,IRET)

      write(0,*)' SLM: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET

      KPDS(5)  = 8
      IF (SPECTR) THEN
        CALL BAOPENwt(57,'fort.57',IRET)
        CALL PUTGB(57,IM*JM,KPDS,KGDS,LB,ORF,IRET)
        write(0,*)' ORF (ORO): putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5)
     &          ,IRET
        CALL BAOPENwt(72,'fort.72',IRET)
        CALL PUTGB(72,IM*JM,KPDS,KGDS,LB,ORO_S,IRET)
        write(0,*)' ORO_UF (ORO): putgb-KPDS(22,5),iret:',KPDS(22),
     &                KPDS(5)
     &          ,IRET
!     else    ! grib output for lat/lon grid KPD's need to be defined
!       CALL BAOPENwt(57,'fort.57',IRET)
!       CALL PUTGB(57,IM*JM,KPDS,KGDS,LB,ORO,IRET)
!       print *,' ORO (ORO): putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5)
!    &          ,IRET
      ENDIF
!
! ===  write out theta (angle of land to East) using #101 (wave dir)
! ===  [radians] and since < 1 scale adjust kpds(22)
!
      KPDS(5)  = 101
      CALL BAOPENwt(58,'fort.58',IRET)
      CALL PUTGB(58,IM*JM,KPDS,KGDS,LB,THETA,IRET)

      write(0,*)' THETA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!
! ===  write out (land aspect ratio or anisotropy)  using #102 
! ===  (as in wind wave hgt)
!
      KPDS(22) = 2
      KPDS(5)  = 102
      CALL BAOPENwt(60,'fort.60',IRET)
      CALL PUTGB(60,IM*JM,KPDS,KGDS,LB,SIGMA,IRET)
      write(0,*)' SIGMA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!
! ===  write out (slope parameter sigma)  using #9 
! ===  (as in std hgt)
!
      KPDS(22) = 1
      KPDS(5)  = 103
      CALL BAOPENwt(59,'fort.59',IRET)
      CALL PUTGB(59,IM*JM,KPDS,KGDS,LB,GAMMA,IRET)

      write(0,*)' GAMMA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!
      KPDS(22) = 1
      KPDS(5)  = 9
      CALL BAOPENwt(61,'fort.61',IRET)
      CALL PUTGB(61,IM*JM,KPDS,KGDS,LB,HPRIME,IRET)

      write(0,*)' HPRIME: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!
!
      KPDS(22) = 1
      KPDS(5)  = 8
      CALL BAOPENwt(62,'fort.62',IRET)
      CALL PUTGB(62,IM*JM,KPDS,KGDS,LB,ELVMAX,IRET)

      write(0,*)' ELVMAX: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!
      RETURN
      END
      SUBROUTINE MAKEMT(ZAVG,ZSLM,ORO,SLM,VAR,VAR4,
     &                  GLAT,IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
!
      implicit none
!
      integer im, jm, imn, jmn
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
     &,       IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM),numi(jm)
      real    ORO(IM,JM),SLM(IM,JM),VAR(IM,JM),VAR4(IM,JM)
     &,       GLAT(JMN),XLAT(JM)
!
      LOGICAL FLAG, DEBUG
!==== DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
!
      integer i, j, im1, jm1, ii1, i1, j1
      real    delx, delxn, faclon, xnsum, xland, xwatr, xl1, xs1
     &,       xw1, xw2, xw4, height, xxlat
!
      print *,' _____ SUBROUTINE MAKEMT '
!---- GLOBAL XLAT AND XLON ( DEGREE )
!
      JM1   = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
!
      DO J=1,JMN
        GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
!
!---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
!
!  (*j*)  for hard wired zero offset (lambda s =0) for terr05 
      DO J=1,JM
        DO I=1,numi(j)
          IM1      = numi(j) - 1
          DELX     = 360./numi(j)       ! GAUSSIAN GRID RESOLUTION
          FACLON   = DELX / DELXN
          IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5 + 1
          IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5 + 1

!         IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!         IEN(I,j) = FACLON * FLOAT(I)   + 0.0001
!
          IF (IST(I,j) <= 0)       IST(I,j) = IST(I,j) + IMN
          IF (IEN(I,j) < IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
!
!          if ( I .lt. 10  .and. J .ge. JM-1 )
!    1   PRINT*,' MAKEMT: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
        ENDDO
!       if ( J .ge. JM-1 ) then
!         print *,' *** FACLON=',FACLON, 'numi(j=',j,')=',numi(j)
!       endif
      ENDDO
      print *,' DELX=',DELX,' DELXN=',DELXN
      DO J=1,JM-1
        FLAG=.TRUE.
        DO J1=1,JMN
          XXLAT = (XLAT(J)+XLAT(J+1))*0.5
          IF(FLAG .AND. GLAT(J1) > XXLAT) THEN
            JST(J)   = J1
            JEN(J+1) = J1 - 1
            FLAG     = .FALSE.
          ENDIF
        ENDDO
!X      PRINT*, ' J JST JEN ',J,JST(J),JEN(J),XLAT(J),GLAT(J1)
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2)    + (JEN(2)-JST(2)),JMN)      

!      PRINT*, ' JM JST JEN=',JST(JM),JEN(JM),XLAT(JM),GLAT(JMN)
!
!...FIRST, AVERAGED HEIGHT
!
      DO J=1,JM
        DO I=1,numi(j)
          ORO(I,J)  = 0.0
          VAR(I,J)  = 0.0
          VAR4(I,J) = 0.0
          XNSUM = 0.0
          XLAND = 0.0
          XWATR = 0.0
          XL1   = 0.0
          XS1   = 0.0
          XW1   = 0.0
          XW2   = 0.0
          XW4   = 0.0
          DO II1 = 1, IEN(I,J) - IST(I,J) + 1
            I1 = IST(I,J) + II1 - 1
            IF(I1 <= 0)  I1 = I1 + IMN
            IF(I1 > IMN) I1 = I1 - IMN

!           if ( i .le. 10 .and. i .ge. 1 ) then 
!             if (j .eq. JM )
!    &print *,' J,JST,JEN,IST,IEN,I1=',
!    &J,JST(j),JEN(J),IST(I,j),IEN(I,j),I1
!           endif

            DO J1=JST(J),JEN(J)
              XLAND = XLAND + FLOAT(ZSLM(I1,J1))
              XWATR = XWATR + FLOAT(1-ZSLM(I1,J1))
              XNSUM = XNSUM + 1.
              HEIGHT = FLOAT(ZAVG(I1,J1)) 
              IF(HEIGHT < -990.) HEIGHT = 0.0
!.........
              XL1 = XL1 + HEIGHT * FLOAT(ZSLM(I1,J1))
              XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(I1,J1))
              XW1 = XW1 + HEIGHT
              XW2 = XW2 + HEIGHT * HEIGHT

!   check antarctic pole 
!             if ( i .le. 10 .and. i .ge. 1 )then
!               if (j .ge. JM-1 )then
!                 write(6,153)i,j,ORO(i,j),HEIGHT,SLM(i,j)
!=== degub testing
!     print *," I,J,I1,J1,XL1,XS1,XW1,XW2:",I,J,I1,J1,XL1,XS1,XW1,XW2
! 153 format(1x,' ORO,ELVMAX(i=',i4,' j=',i3,')=',2E14.5,3f5.1)
!               endif
!             endif

            ENDDO
          ENDDO
          IF(XNSUM > 1.) THEN
             SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
             IF(SLM(I,J) /= 0.) THEN
               if (xland > 0.0) ORO(I,J)= XL1 / XLAND
             ELSE
               if (xwatr > 0.0) ORO(I,J)= XS1 / XWATR
             ENDIF
             VAR(I,J) = SQRT(MAX(XW2/XNSUM-(XW1/XNSUM)**2,0.))
             DO II1 = 1, IEN(I,j) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
               IF(I1 <= 0.) I1 = I1 + IMN
               IF(I1 > IMN) I1 = I1 - IMN
               DO J1=JST(J),JEN(J)
                 HEIGHT = FLOAT(ZAVG(I1,J1)) 
                 IF(HEIGHT < -990.) HEIGHT = 0.0
                 XW4 = XW4 + (HEIGHT-ORO(I,J)) ** 4
               ENDDO
             ENDDO
             IF(VAR(I,J) > 1.) THEN
!              if ( I .lt. 20  .and. J .ge. JM-19 ) then
!                print *,'I,J,XW4,XNSUM,VAR(I,J)',I,J,XW4,XNSUM,VAR(I,J)
!              endif
               VAR4(I,J) = MIN(XW4/XNSUM/VAR(I,J) **4,10.)
             ENDIF
          ENDIF
        ENDDO
      ENDDO

      WRITE(6,*) "! MAKEMT ORO SLM VAR VAR4 DONE"
!

      RETURN
      END
      SUBROUTINE MAKEPC(ZAVG,ZSLM,THETA,GAMMA,SIGMA,
     1           GLAT,IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
!
!===  PC: principal coordinates of each Z avg orog box for L&M
!
      implicit none
!
      real,   parameter :: REARTH=6.3712E+6
      integer IM,JM,IMN,JMN
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
     &,       IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM),numi(jm)
      real    GLAT(JMN),XLAT(JM),DELTAX(JMN)
     &,       ORO(IM,JM),SLM(IM,JM),HL(IM,JM),HK(IM,JM)
      real    HX2(IM,JM),HY2(IM,JM),HXY(IM,JM),HLPRIM(IM,JM)
     &,       THETA(IM,JM),GAMMA(IM,JM),SIGMA2(IM,JM),SIGMA(IM,JM)
      LOGICAL FLAG, DEBUG
!===  DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
!
      integer i, j, jm1, ii1, i0, i1, j1, ip1, ijax
      real    pi, certh, delxn, deltay, delx, faclon, xxlat
     &,       xnsum, xland, xwatr, xl1, xs1, xfp, yfp, xfpyfp, xfp2
     &,       yfp2, height, hi0, hi1, hip1, hijax, hi1j1, hj0, hjp1
!
      PI = 4.0 * ATAN(1.0)
      CERTH = PI * REARTH
!---- GLOBAL XLAT AND XLON ( DEGREE )
!
      JM1    = JM - 1
      DELXN  = 360./IMN      ! MOUNTAIN DATA RESOLUTION
      DELTAY =  CERTH / FLOAT(JMN)
      print *, 'MAKEPC: DELTAY=',DELTAY
!
      DO J=1,JMN
        GLAT(J)   = -90. + (J-1) * DELXN + DELXN * 0.5
        DELTAX(J) = DELTAY * COSD(GLAT(J))
      ENDDO
!
!---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
!
      DO J=1,JM
        DO I=1,numi(j)
!         IM1 = numi(j) - 1
          DELX     = 360. / numi(j)       ! GAUSSIAN GRID RESOLUTION
          FACLON   = DELX / DELXN
          IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5
          IST(I,j) = IST(I,j) + 1
          IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5

!         if (debug) then
!           if ( I < 10 .and. J < 10 )
!    1      PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
!         endif
!         IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!         IEN(I,j) = FACLON * FLOAT(I)   + 0.0001

          IF (IST(I,j) <= 0)       IST(I,j) = IST(I,j) + IMN
          IF (IEN(I,j) < IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
          if (debug) then
            if ( I < 10 .and. J < 10 )
     1      PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
          endif
          IF (IEN(I,j) .LT. IST(I,j)) 
     1      print *,' MAKEPC: IEN < IST: I,J,IST(I,J),IEN(I,J)',
     2       I,J,IST(I,J),IEN(I,J)
        ENDDO
      ENDDO
      DO J=1,JM-1
        FLAG=.TRUE.
        DO J1=1,JMN
          XXLAT = (XLAT(J)+XLAT(J+1))*0.5
          IF(FLAG .AND. GLAT(J1) > XXLAT) THEN
            JST(J)   = J1
            JEN(J+1) = J1 - 1
            FLAG     = .FALSE.
          ENDIF
        ENDDO
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2)    + (JEN(2)-JST(2)),JMN)
      if (debug) then
        PRINT*, ' IST,IEN(1,1-numi(1,JM))',IST(1,1),IEN(1,1), 
     1    IST(numi(JM),JM),IEN(numi(JM),JM), numi(JM)
        PRINT*, ' JST,JEN(1,JM) ',JST(1),JEN(1),JST(JM),JEN(JM) 
      endif
!
!... DERIVITIVE TENSOR OF HEIGHT
!
      DO J=1,JM
        DO I=1,numi(j)
          ORO(I,J)    = 0.0
          HX2(I,J)    = 0.0
          HY2(I,J)    = 0.0
          HXY(I,J)    = 0.0
          XNSUM       = 0.0
          XLAND       = 0.0
          XWATR       = 0.0
          XL1         = 0.0
          XS1         = 0.0
          xfp         = 0.0
          yfp         = 0.0
          xfpyfp      = 0.0
          xfp2        = 0.0
          yfp2        = 0.0
          HL(I,J)     = 0.0
          HK(I,J)     = 0.0
          HLPRIM(I,J) = 0.0
          THETA(I,J)  = 0.0 
          GAMMA(I,J)  = 0.
          SIGMA2(I,J) = 0.
          SIGMA(I,J)  = 0.
!
          DO II1 = 1, IEN(I,J) - IST(I,J) + 1
            I1 = IST(I,J) + II1 - 1
            IF(I1.LE.0.)  I1 = I1 + IMN
            IF(I1.GT.IMN) I1 = I1 - IMN
!
!===  set the rest of the indexs for ave: 2pt staggered derivitive
!
            i0 = i1 - 1
            if (i1 - 1 <= 0 )  i0 = i0 + imn
            if (i1 - 1 > imn)  i0 = i0 - imn

            ip1 = i1 + 1
            if (i1 + 1 <= 0 )  ip1 = ip1 + imn
            if (i1 + 1 > imn)  ip1 = ip1 - imn
!
            DO J1=JST(J),JEN(J)
              if (debug) then
                if ( I1 == IST(I,J) .and. J1 == JST(J) ) 
     1   PRINT*, ' J, J1,IST,JST,DELTAX,GLAT ',
     2          J,J1,IST(I,J),JST(J),DELTAX(J1),GLAT(J1)  
                if ( I1 .eq. IEN(I,J) .and. J1 .eq. JEN(J) ) 
     1   PRINT*, ' J, J1,IEN,JEN,DELTAX,GLAT ',
     2          J,J1,IEN(I,J),JEN(J),DELTAX(J1),GLAT(J1)  
              endif
              XLAND = XLAND + FLOAT(ZSLM(I1,J1))
              XWATR = XWATR + FLOAT(1-ZSLM(I1,J1))
              XNSUM = XNSUM + 1.
!
              HEIGHT = FLOAT(ZAVG(I1,J1))
              hi0 =  float(zavg(i0,j1))
              hip1 =  float(zavg(ip1,j1))
!
              IF(HEIGHT < -990.) HEIGHT = 0.0
              if(hi0   < -990.)  hi0  = 0.0
              if(hip1  < -990.)  hip1 = 0.0
!........     xfp  = xfp + 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
              xfp  = 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
              xfp2 = xfp2 + 0.25 * ( ( hip1 - hi0 )/DELTAX(J1) )** 2 
!
! --- not at boundaries
              if ( J1 /= JST(1)  .and. J1 /= JEN(JM) ) then
                hj0  =  float(zavg(i1,j1-1))
                hjp1 =  float(zavg(i1,j1+1))
                if(hj0  < -990.)  hj0  = 0.0
                if(hjp1 < -990.)  hjp1 = 0.0
!.......        yfp  = yfp + 0.5 * ( hjp1 - hj0 ) / DELTAY
                yfp  = 0.5 * ( hjp1 - hj0 ) / DELTAY
                yfp2 = yfp2 + 0.25 * ( ( hjp1 - hj0 )/DELTAY )**2   

!..............  elseif ( J1 == JST(J) .or. J1 == JEN(JM) ) then
! ===     the NH pole: NB J1 goes from High at NP to Low toward SP
!
              elseif ( J1 == JST(1) ) then
                ijax = i1 + imn/2 
                if (ijax <= 0 )  ijax = ijax + imn
                if (ijax > imn)  ijax = ijax - imn
!..... at N pole we stay at the same latitude j1 but cross to opp side
                hijax = float(zavg(ijax,j1))
                hi1j1 = float(zavg(i1,j1))
                if(hijax < -990.)  hijax = 0.0
                if(hi1j1 < -990.)  hi1j1 = 0.0
!.......        yfp = yfp + 0.5 * ( ( 0.5 * ( hijax + hi1j1) ) - hi1j1 )/DELTAY
                yfp = 0.5 * ( ( 0.5 * ( hijax - hi1j1 ) ) )/DELTAY
                yfp2 = yfp2 + 0.25 * ( ( 0.5 *  ( hijax - hi1j1) ) 
     1                                              / DELTAY )**2
!
! ===     the SH pole: NB J1 goes from High at NP to Low toward SP
!
              elseif ( J1 == JEN(JM) ) then
                ijax = i1 + imn/2 
                if (ijax <= 0 )   ijax = ijax + imn
                if (ijax >  imn)  ijax = ijax - imn
                hijax = float(zavg(ijax,j1))
                hi1j1 = float(zavg(i1,j1))
                if(hijax < -990.)  hijax = 0.0
                if(hi1j1 < -990.)  hi1j1 = 0.0
                if ( i1 < 5 )
     &             print *,' S.Pole i1,j1 :',i1,j1,hijax,hi1j1
!.....          yfp  = yfp + 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
                yfp  = 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
                yfp2 = yfp2 + 0.25 * (  (0.5 * (hijax - hi1j1) )
     1                                                / DELTAY )**2
              endif
!
! ===    The above does an average across the pole for the bndry in j.
!23456789012345678901234567890123456789012345678901234567890123456789012......
!
              xfpyfp = xfpyfp + xfp * yfp
              XL1    = XL1 + HEIGHT * FLOAT(ZSLM(I1,J1))
              XS1    = XS1 + HEIGHT * FLOAT(1-ZSLM(I1,J1))
!
! === average the HX2, HY2 and HXY
! === This will be done over all land
!
            ENDDO
          ENDDO
!
! ===  HTENSR 
!
          IF(XNSUM > 1.) THEN
            SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
            IF(SLM(I,J) /= 0.) THEN
              ORO(I,J) = XL1    / XLAND
              HX2(I,J) = xfp2   / XLAND
              HY2(I,J) = yfp2   / XLAND
              HXY(I,J) = xfpyfp / XLAND
            ELSE
              ORO(I,J) = XS1 / XWATR
            ENDIF
!=== degub testing
            if (debug) then
              print *," I,J,i1,j1,HEIGHT:", I,J,i1,j1,HEIGHT,
     1                 XLAND,SLM(i,j)
              print *," xfpyfp,xfp2,yfp2:",xfpyfp,xfp2,yfp2
              print *," HX2,HY2,HXY:",HX2(I,J),HY2(I,J),HXY(I,J)
            ENDIF
!
! === make the principal axes, theta, and the degree of anisotropy, 
! === and sigma2, the slope parameter
!
            HK(I,J) = 0.5 * ( HX2(I,J) + HY2(I,J) )
            HL(I,J) = 0.5 * ( HX2(I,J) - HY2(I,J) )
            HLPRIM(I,J) = SQRT(HL(I,J)*HL(I,J) + HXY(I,J)*HXY(I,J))
            IF( HL(I,J) /=  0. .AND. SLM(I,J) /= 0. ) THEN
              THETA(I,J) = 0.5 * ATAN2D(HXY(I,J),HL(I,J))
! ===   for testing print out in degrees
!             THETA(I,J) = 0.5 * ATAN2(HXY(I,J),HL(I,J))
            ENDIF
            SIGMA2(I,J) =  ( HK(I,J) + HLPRIM(I,J) )
            if (SIGMA2(I,J) >= 0.) then 
              SIGMA(I,J) =  SQRT(SIGMA2(I,J) )
              if (sigma2(i,j) /= 0. .and. HK(I,J) >= HLPRIM(I,J) )
     1        GAMMA(I,J) = sqrt( (HK(I,J) - HLPRIM(I,J)) / SIGMA2(I,J) )
            else
              SIGMA(I,J) = 0.
            endif
          ENDIF
          if (debug) then
            print *," I,J,THETA,SIGMA,GAMMA,",I,J,THETA(I,J),
     1                                       SIGMA(I,J),GAMMA(I,J)
            print *," HK,HL,HLPRIM:",HK(I,J),HL(I,J),HLPRIM(I,J)
          endif
        ENDDO
      ENDDO

      WRITE(6,*) "! MAKE Principal Coord  DONE"
!
      RETURN
      END

      SUBROUTINE MAKEOA(ZAVG,VAR,GLAT,OA4,OL,IOA4,ELVMAX,
     1           ORO,oro1,XNSUM,XNSUM1,XNSUM2,XNSUM3,XNSUM4,
     2           IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
!
      implicit none
!
      integer IM,JM,IMN,JMN
     &,       IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM),numi(jm)
     &,       ioa4(im,jm,4), ZAVG(IMN,JMN)
      real    GLAT(JMN),XLAT(JM)
     &,       ORO(IM,JM),ORO1(IM,JM),ELVMAX(IM,JM),ZMAX(IM,JM)
     &,       OA4(IM,JM,4)
      real    XNSUM(IM,JM),XNSUM1(IM,JM),XNSUM2(IM,JM)
     &,       XNSUM3(IM,JM),XNSUM4(IM,JM)
      real    VAR(IM,JM),OL(IM,JM,4)
      LOGICAL FLAG
!
      integer i, j, im1, jm1, i1, ii1, j1, kwd, ii, inci, isttt, jsttt
     &,       ns0, ns1, ns2, ns3, ns4, ns5, ns6, ieddd, jeddd, incj 
      real    delx, delxn, faclon, xxlat, height, xnpu, xnpd, hc, t
     &,       tem
!
!---- GLOBAL XLAT AND XLON ( DEGREE )
!
! --- IM1 = IM - 1 removed (not used in this sub)
      JM1 = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
!
      DO J=1,JMN
        GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO

      write(0,*)'MAKEOA: IM=',IM,' JM=',JM,' IMN=',IMN,' JMN=',JMN
!
!---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
!
      DO j=1,jm
        DO I=1,numi(j)
          DELX     = 360./numi(j)       ! GAUSSIAN GRID RESOLUTION
          FACLON   = DELX / DELXN
! ---  minus sign here in IST and IEN as in MAKEMT!
          IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5
          IST(I,j) = IST(I,j) + 1
          IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5
!         IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!         IEN(I,j) = FACLON * FLOAT(I)   + 0.0001
          IF (IST(I,j) <= 0)       IST(I,j) = IST(I,j) + IMN
          IF (IEN(I,j) < IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
!x          PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
            if ( I .lt. 3  .and. J .lt. 3 )
     1      PRINT*,' MAKEOA: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
           if ( I .lt. 3  .and. J .ge. JM-1 )
     1     PRINT*,' MAKEOA: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
        ENDDO
      ENDDO
      write(0,*)'MAKEOA: DELXN,DELX,FACLON',DELXN,DELX,FACLON
      write(0,*)'  ***** ready to start JST JEN section '
!
      DO J=1,JM-1
         FLAG = .TRUE.
         DO J1=1,JMN
! --- XXLAT added as in MAKEMT and in next line as well
            XXLAT = (XLAT(J)+XLAT(J+1))/2.
            IF(FLAG .AND. GLAT(J1) > XXLAT) THEN
              JST(J) = J1
! ---         JEN(J+1) = J1 - 1
              FLAG = .FALSE.
           if ( J == 1 ) PRINT*,' MAKEOA: XX j JST JEN ',j,JST(j),JEN(j)
            ENDIF
         ENDDO
         if ( J < 3 ) PRINT*,' MAKEOA: j JST JEN ',j,JST(j),JEN(j)
         if ( J >= JM-2 ) PRINT*,' MAKEOA: j JST JEN ',j,JST(j),JEN(j)
!        FLAG=.TRUE.
!        DO J1=JST(J),JMN
!           IF(FLAG.AND.GLAT(J1).GT.XLAT(J)) THEN
!             JEN(J) = J1 - 1
!             FLAG = .FALSE.
!           ENDIF
!        ENDDO
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2) + (JEN(2)-JST(2)),JMN)
      write(0,*)' ***** JST(1) JEN(1) ',JST(1),JEN(1)
      write(0,*)' ***** JST(JM) JEN(JM) ',JST(JM),JEN(JM)
!
      DO J=1,JM
        DO I=1,numi(j)
          XNSUM(I,J) = 0.0
          ELVMAX(I,J) = ORO(I,J)
          ZMAX(I,J)   = 0.0
        ENDDO
      ENDDO
!
! --- # of peaks > ZAVG value and ZMAX(IM,JM) -- ORO is already avg.
! ---  to JM or to JM1
      DO J=1,JM
!       write(0,*)' J=',j,' in xnsum loop'
        DO I=1,numi(j)
            DO II1 = 1, IEN(I,J) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
! --- next line as in makemt (I1 not II1) (*j*) 20070701
             IF(I1 <= 0.)  I1 = I1 + IMN
             IF (I1 > IMN) I1 = I1 - IMN
             DO J1=JST(J),JEN(J)
                HEIGHT = FLOAT(ZAVG(I1,J1))
                IF(HEIGHT < -990.) HEIGHT = 0.0
                IF ( HEIGHT > ORO(I,J) ) then
                   if ( HEIGHT > ZMAX(I,J) ) ZMAX(I,J) = HEIGHT
                   XNSUM(I,J) = XNSUM(I,J) + 1
                  ENDIF
             ENDDO
            ENDDO
           if ( I < 5  .and. J >= JM-5 ) then
              write(0,*) ' I,J,ORO(I,J),XNSUM(I,J),ZMAX(I,J):',
     1                     I,J,ORO(I,J),XNSUM(I,J),ZMAX(I,J)
           endif
        ENDDO
      ENDDO
!
!....     make ELVMAX  ORO from MAKEMT sub
!
! ---  this will make work1 array take on oro's values on return
      DO J=1,JM
        DO I=1,numi(j)

          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO
!........
!      The MAX elev peak (no averaging)
!........
!     DO J=1,JM
!       DO I=1,numi(j)
!           DO II1 = 1, IEN(I,J) - IST(I,J) + 1
!              I1 = IST(I,J) + II1 - 1
!              IF(I1.LE.0.)  I1 = I1 + IMN
!              IF(I1.GT.IMN) I1 = I1 - IMN
!              DO J1=JST(J),JEN(J)
!              if ( ELVMAX(I,J) .lt. ZMAX(I1,J1)) 
!    1              ELVMAX(I,J)   =  ZMAX(I1,J1)
!              ENDDO
!           ENDDO
!        ENDDO
!     ENDDO
C
C---- COUNT NUMBER OF MODE. HIGHER THAN THE HC, CRITICAL HEIGHT
C     IN A GRID BOX
      DO J=1,JM
        DO I=1,numi(j)
          XNSUM1(I,J) = 0.0
          XNSUM2(I,J) = 0.0
          XNSUM3(I,J) = 0.0
          XNSUM4(I,J) = 0.0
        ENDDO
      ENDDO
! ---                 loop 
      DO J=1,JM1
        DO I=1,numi(j)
           HC = 1116.2 - 0.878 * VAR(I,J)
!          print *,' I,J,HC,VAR:',I,J,HC,VAR(I,J)
            DO II1 = 1, IEN(I,J) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
!     IF (I1.LE.0.) print *,' I1 less than 0',I1,II1,IST(I,J),IEN(I,J) 
!               if ( J .lt. 3 .or. J .gt. JM-2 ) then 
!     IF(I1 .GT. IMN)print *,' I1 > IMN',J,I1,II1,IMN,IST(I,J),IEN(I,J) 
!               endif
             IF(I1.GT.IMN) I1 = I1 - IMN
             DO J1=JST(J),JEN(J)
               IF(FLOAT(ZAVG(I1,J1)) .GT. HC)
     1            XNSUM1(I,J) = XNSUM1(I,J) + 1
                  XNSUM2(I,J) = XNSUM2(I,J) + 1
             ENDDO
           ENDDO
!
           INCI  = NINT((IEN(I,j)-IST(I,j)) * 0.5)
           ISTTT = MIN(MAX(IST(I,j)-INCI,1),IMN)
           IEDDD = MIN(MAX(IEN(I,j)-INCI,1),IMN)
!
           INCJ  = NINT((JEN(J)-JST(J)) * 0.5)
           JSTTT = MIN(MAX(JST(J)-INCJ,1),JMN)
           JEDDD = MIN(MAX(JEN(J)-INCJ,1),JMN)
!               if ( J .lt. 3 .or. J .gt. JM-3 )  then
!                 if(I .lt. 3 .or. I .gt. IM-3) then
!        print *,' INCI,ISTTT,IEDDD,INCJ,JSTTT,JEDDD:',
!    1  I,J,INCI,ISTTT,IEDDD,INCJ,JSTTT,JEDDD  
!                 endif
!               endif
!
           DO I1=ISTTT,IEDDD
             DO J1=JSTTT,JEDDD
               IF(FLOAT(ZAVG(I1,J1)) .GT. HC)
     1            XNSUM3(I,J) = XNSUM3(I,J) + 1
                  XNSUM4(I,J) = XNSUM4(I,J) + 1
             ENDDO
           ENDDO
!x         print*,' i j hc var ',i,j,hc,var(i,j)
!x         print*,'xnsum12 ',xnsum1(i,j),xnsum2(i,j)
!x         print*,'xnsum34 ',xnsum3(i,j),xnsum4(i,j)
        ENDDO
      ENDDO
      write(0,*)' IN MAKEOA After XNSUM4'
!
!---- CALCULATE THE 3D OROGRAPHIC ASYMMETRY FOR 4 WIND DIRECTIONS
!---- AND THE 3D OROGRAPHIC SUBGRID OROGRAPHY FRACTION
!     (KWD = 1  2  3  4)
!     ( WD = W  S SW NW)
!
!
      DO KWD = 1, 4
        DO J=1,JM
          DO I=1,numi(j)
            OA4(I,J,KWD) = 0.0
          ENDDO
        ENDDO
      ENDDO
!
      DO J=1,JM-2
        DO I=1,numi(j)
        II = I + 1
        IF (II .GT. numi(j)) II = II - numi(j)
          XNPU = XNSUM(I,J)    + XNSUM(I,J+1)
          XNPD = XNSUM(II,J)   + XNSUM(II,J+1)
          IF (XNPD .NE. XNPU) OA4(II,J+1,1) = 1. - XNPD / MAX(XNPU , 1.)
          tem = XNSUM4(I,J+1) + XNSUM4(II,J+1)
          if (tem > 0.0) then
            OL(II,J+1,1) = (XNSUM3(I,J+1) + XNSUM3(II,J+1)) / tem
          endif
          if ( I .lt. 20  .and. J .ge. JM-19 ) then
         write(0,*)' MAKEOA: I J IST IEN ',I,j,IST(I,J),IEN(I,J)
!        PRINT*,' HC VAR ',HC,VAR(i,j)
!        PRINT*,' MAKEOA: XNSUM(I,J)=',XNSUM(I,J),XNPU, XNPD
!        PRINT*,' MAKEOA: XNSUM3(I,J+1),XNSUM3(II,J+1)',
!    1                    XNSUM3(I,J+1),XNSUM3(II,J+1)
!        PRINT*,' MAKEOA: II, OA4(II,J+1,1), OL(II,J+1,1):',
!    1                    II, OA4(II,J+1,1), OL(II,J+1,1)
           endif
        ENDDO
      ENDDO
      write(0,*)' MAKEOA: after OL loop1'
      DO J=1,JM-2
        DO I=1,numi(j)
        II = I + 1
        IF (II .GT. numi(j)) II = II - numi(j)
          XNPU = XNSUM(I,J+1)   + XNSUM(II,J+1)
          XNPD = XNSUM(I,J)     + XNSUM(II,J)
          IF (XNPD .NE. XNPU) OA4(II,J+1,2) = 1. - XNPD / MAX(XNPU , 1.)
          OL(II,J+1,2) = (XNSUM3(II,J)+XNSUM3(II,J+1))/
     1                   (XNSUM4(II,J)+XNSUM4(II,J+1))
        ENDDO
      ENDDO
      write(0,*)' MAKEOA: after OL loop2'
      DO J=1,JM-2
        DO I=1,numi(j)
        II = I + 1
        IF (II .GT. numi(j)) II = II - numi(j)
          XNPU = XNSUM(I,J+1)  + ( XNSUM(I,J) + XNSUM(II,J+1) )*0.5
          XNPD = XNSUM(II,J)   + ( XNSUM(I,J) + XNSUM(II,J+1) )*0.5
          IF (XNPD .NE. XNPU) OA4(II,J+1,3) = 1. - XNPD / MAX(XNPU , 1.)
          OL(II,J+1,3) = (XNSUM1(II,J)+XNSUM1(I,J+1))/
     1                   (XNSUM2(II,J)+XNSUM2(I,J+1))
        ENDDO
      ENDDO
      write(0,*)' MAKEOA: after OL loop3'
      DO J=1,JM-2
        DO I=1,numi(j)
        II = I + 1
        IF (II .GT. numi(j)) II = II - numi(j)
          XNPU = XNSUM(I,J)    + ( XNSUM(II,J) + XNSUM(I,J+1) )*0.5
          XNPD = XNSUM(II,J+1) + ( XNSUM(II,J) + XNSUM(I,J+1) )*0.5
          IF (XNPD .NE. XNPU) OA4(II,J+1,4) = 1. - XNPD / MAX(XNPU , 1.)
          OL(II,J+1,4) = (XNSUM1(I,J)+XNSUM1(II,J+1))/
     1                   (XNSUM2(I,J)+XNSUM2(II,J+1))
        ENDDO
      ENDDO
      write(0,*)' MAKEOA: after OL loop4'
!
      DO KWD = 1, 4
        DO I=1,numi(j)
          OL(I,1,KWD)  = OL(I,2,KWD)
          OL(I,JM,KWD) = OL(I,JM-1,KWD)
        ENDDO
      ENDDO
!
      write(0,*)' IN MAKEOA Bef OA4'
      DO KWD=1,4
        DO J=1,JM
          DO I=1,numi(j)
            T = OA4(I,J,KWD)
            OA4(I,J,KWD) = SIGN( MIN( ABS(T), 1. ), T )
          ENDDO
        ENDDO
      ENDDO
!
      NS0 = 0
      NS1 = 0
      NS2 = 0
      NS3 = 0
      NS4 = 0
      NS5 = 0
      NS6 = 0
      DO KWD=1,4
      DO J=1,JM
      DO I=1,numi(j)
         T = ABS( OA4(I,J,KWD) )
         IF(T .EQ. 0.) THEN
            IOA4(I,J,KWD) = 0
            NS0 = NS0 + 1
         ELSE IF(T .GT. 0. .AND. T .LE. 1.) THEN
            IOA4(I,J,KWD) = 1
            NS1 = NS1 + 1
         ELSE IF(T .GT. 1. .AND. T .LE. 10.) THEN
            IOA4(I,J,KWD) = 2
            NS2 = NS2 + 1
         ELSE IF(T .GT. 10. .AND. T .LE. 100.) THEN
            IOA4(I,J,KWD) = 3
            NS3 = NS3 + 1
         ELSE IF(T .GT. 100. .AND. T .LE. 1000.) THEN
            IOA4(I,J,KWD) = 4
            NS4 = NS4 + 1
         ELSE IF(T .GT. 1000. .AND. T .LE. 10000.) THEN
            IOA4(I,J,KWD) = 5
            NS5 = NS5 + 1
         ELSE IF(T .GT. 10000.) THEN
            IOA4(I,J,KWD) = 6
            NS6 = NS6 + 1
         ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      WRITE(6,*) "! MAKEOA EXIT"
!
      RETURN
      END
      SUBROUTINE REVERS(IM, JM, numi, F, WRK)
!
      implicit none
!
      integer im, jm
      REAL    F(IM,JM), WRK(IM,JM)
      integer numi(jm), i, j, ir, jr, imb2
      real    tem
!
!     reverse east-west and north-south
!......  fix this routine up to take numi (*j*)
!.....   at least have 5 variables ....and keep REVLAT .FALSE.

      imb2 = im / 2
      do j=1,jm
        do i=1,im
          WRK(i,j) = F(i,j)
        enddo
      enddo
      do j=1,jm
         jr = jm - j + 1
         do i=1,im
            ir = i + imb2
            if (ir > im) ir = ir - im
            f(ir,jr) = WRK(i,j)
         enddo
      enddo
!
      tem = 0.0
      do i=1,im
        tem= tem + F(I,1)
      enddo
      tem = tem / im
      do i=1,im
         F(I,1) = tem
      enddo
!
      RETURN
      END

      subroutine rg2gg(im,jm,numi,a)
!
      implicit none
!
      integer,intent(in):: im,jm,numi(jm)
      real,intent(inout):: a(im,jm)
      integer j,ir,ig
      real r,t(im)
      do j=1,jm
        r = real(numi(j))/real(im)
        do ig=1,im
          ir    = mod(nint((ig-1)*r),numi(j)) + 1
          t(ig) = a(ir,j)
        enddo
        do ig=1,im
          a(ig,j) = t(ig)
        enddo
      enddo
      end subroutine
      subroutine gg2rg(im,jm,numi,a)
!
      implicit none
!
      integer,intent(in):: im,jm,numi(jm)
      real,intent(inout):: a(im,jm)
      integer j,ir,ig
      real r,t(im)
      do j=1,jm
        r = real(numi(j))/real(im)
        do ir=1,numi(j)
          ig    = nint((ir-1)/r) + 1
          t(ir) = a(ig,j)
        enddo
        do ir=1,numi(j)
          a(ir,j) = t(ir)
        enddo
      enddo
      end subroutine
      SUBROUTINE minmxj(IM,JM,A,title)

! this routine is using real*4 on the sp

      implicit none

      integer im, jm
      real    A(IM,JM),rmin,rmax
      integer i,j
      character*8 title

      rmin = 1.e+10
      rmax = -rmin
!sela....................................................
!sela if(rmin.eq.1.e+10)return
!sela....................................................
      DO j=1,JM
        DO i=1,IM
          if (A(i,j) >= rmax) rmax = A(i,j)
          if (A(i,j) <= rmin) rmin = A(i,j)
        ENDDO
      ENDDO
      write(0,150)rmin,rmax,title
150   format('rmin=',e13.4,2x,'rmax=',e13.4,2x,a8,' ')
!
      RETURN
      END
      SUBROUTINE mnmxja(IM,JM,A,imax,jmax,title)

! this routine is using real*4 on the sp

      implicit none

      integer im, jm
      real    A(IM,JM),rmin,rmax
      integer i,j,imax,jmax
      character*8 title

      rmin = 1.e+10
      rmax = -rmin
!sela....................................................
!sela if(rmin.eq.1.e+10)return
!sela....................................................
      DO j=1,JM
        DO i=1,IM
          if (A(i,j) >= rmax) then
            rmax = A(i,j)
            imax = i
            jmax = j
          endif
          if (A(i,j) <= rmin) rmin = A(i,j)
        ENDDO
      ENDDO
      write(6,150)rmin,rmax,title
150   format('rmin=',e13.4,2x,'rmax=',e13.4,2x,a8,' ')
!
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE SPFFT1(IMAX,INCW,INCG,KMAX,W,G,IDIR)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  SPFFT1     PERFORM MULTIPLE FAST FOURIER TRANSFORMS
!   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-20
!
! ABSTRACT: THIS SUBPROGRAM PERFORMS MULTIPLE FAST FOURIER TRANSFORMS
!           BETWEEN COMPLEX AMPLITUDES IN FOURIER SPACE AND REAL VALUES
!           IN CYCLIC PHYSICAL SPACE.
!           SUBPROGRAM SPFFT1 INITIALIZES TRIGONOMETRIC DATA EACH CALL.
!           USE SUBPROGRAM SPFFT TO SAVE TIME AND INITIALIZE ONCE.
!           THIS VERSION INVOKES THE IBM ESSL FFT.
!
! PROGRAM HISTORY LOG:
! 1998-12-18  IREDELL
!
! USAGE:    CALL SPFFT1(IMAX,INCW,INCG,KMAX,W,G,IDIR)
!
!   INPUT ARGUMENT LIST:
!     IMAX     - INTEGER NUMBER OF VALUES IN THE CYCLIC PHYSICAL SPACE
!                (SEE LIMITATIONS ON IMAX IN REMARKS BELOW.)
!     INCW     - INTEGER FIRST DIMENSION OF THE COMPLEX AMPLITUDE ARRAY
!                (INCW >= IMAX/2+1)
!     INCG     - INTEGER FIRST DIMENSION OF THE REAL VALUE ARRAY
!                (INCG >= IMAX)
!     KMAX     - INTEGER NUMBER OF TRANSFORMS TO PERFORM
!     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR>0
!     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR<0
!     IDIR     - INTEGER DIRECTION FLAG
!                IDIR>0 TO TRANSFORM FROM FOURIER TO PHYSICAL SPACE
!                IDIR<0 TO TRANSFORM FROM PHYSICAL TO FOURIER SPACE
!
!   OUTPUT ARGUMENT LIST:
!     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR<0
!     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR>0
!
! SUBPROGRAMS CALLED:
!   SCRFT        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
!   DCRFT        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
!   SRCFT        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
!   DRCFT        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
! REMARKS:
!   THE RESTRICTIONS ON IMAX ARE THAT IT MUST BE A MULTIPLE
!   OF 1 TO 25 FACTORS OF TWO, UP TO 2 FACTORS OF THREE,
!   AND UP TO 1 FACTOR OF FIVE, SEVEN AND ELEVEN.
!
!   THIS SUBPROGRAM IS THREAD-SAFE.
!
!$$$
        IMPLICIT NONE
        INTEGER,INTENT(IN):: IMAX,INCW,INCG,KMAX,IDIR
        COMPLEX,INTENT(INOUT):: W(INCW,KMAX)
        REAL,INTENT(INOUT):: G(INCG,KMAX)
        REAL:: AUX1(25000+INT(0.82*IMAX))
        REAL:: AUX2(20000+INT(0.57*IMAX))
        INTEGER:: NAUX1,NAUX2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        NAUX1=25000+INT(0.82*IMAX)
        NAUX2=20000+INT(0.57*IMAX)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FOURIER TO PHYSICAL TRANSFORM.
        SELECT CASE(IDIR)
        CASE(1:)
          SELECT CASE(DIGITS(1.))
          CASE(DIGITS(1._4))
            CALL SCRFT(1,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL SCRFT(0,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
          CASE(DIGITS(1._8))
            CALL DCRFT(1,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL DCRFT(0,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
          END SELECT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PHYSICAL TO FOURIER TRANSFORM.
        CASE(:-1)
          SELECT CASE(DIGITS(1.))
          CASE(DIGITS(1._4))
            CALL SRCFT(1,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL SRCFT(0,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
          CASE(DIGITS(1._8))
            CALL DRCFT(1,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL DRCFT(0,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
          END SELECT
        END SELECT
      END SUBROUTINE
      subroutine read_g(glob,ITOPO)
!
! --- if ITOPO = 1 then read gtopo30_gg.fine 43200X21600 30" file
! --- if ITOPO = 2 then read  topo 30" .DEM tile files 
! --- in either case, glob will be n Interger*2 array.
! --- This routine write out a grads ctl file for displaying the 
! --- tiles in the output working dir.  The glob array can not be 
! --- acted on with grads, but the tiles can be if lat/lon are reduced slightly
!!
      implicit none
!!
      integer*2    glob(360*120,180*120)
!!
      integer, parameter :: ix=40*120, jx=50*120
     &,                     ia=60*120, ja=30*120
!!
      integer*2    idat(ix,jx),itopo
!!
!!mr  integer*2    m9999 
!!mr  data         m9999    / -9999 /
!!
!!mr  integer      i_count(360*120)
!!mr  integer      j_max_y(360*120)
!!
      integer      i,j,inttyp
!!
      real(kind=8) dloin,dlain,rlon,rlat
!!
      read(235) glob
      rewind(235)
!!
!!
      print*,' '
      call maxmin     (glob,360*120*180*120,'global0')
!!
!!
      dloin = 1.d0/120.d0
      dlain = 1.d0/120.d0
!!
      rlon = -179.995833333333333333333333d0
      rlat =   89.995833333333333333333333d0
!!
      inttyp =-1  !  average rectangular subset
!!mr  inttyp = 1  !  take closest grid point value
!!mr  inttyp = 0  !  interpolate from four closest grid point values
!!
!     call la2ga_gtopo30(glob,360*120,180*120,
!    &           dloin,dlain,rlon,rlat,inttyp,
!    &           .true.,glob,
!    &           0,lonf,latg)
!!
      return
      end
      subroutine maxmin(ia,len,tile)
!!mr
      implicit none
!!mr
      integer len
      integer*2 ia(len)
      character*7 tile
      integer iaamax, iaamin, j, m, ja, kount
      integer(8) sum2,std,mean,isum
      integer i_count_notset,kount_9

! --- missing is -9999
!
      isum    = 0
      sum2    = 0
      kount   = 0
      kount_9 = 0
      iaamax  = -9999999
!!mr  iaamin  = 1
      iaamin  =  9999999
      i_count_notset = 0

      do m=1,len
        ja=ia(m)
!!mr    if ( ja .lt. 0 ) print *,' ja < 0:',ja
!!mr    if ( ja .eq. -9999 ) goto 10
        if ( ja .eq. -9999 ) then
           kount_9 = kount_9 + 1
           cycle
        endif
        if ( ja == -12345 ) i_count_notset = i_count_notset + 1
!!mr    if ( ja .eq. 0 ) goto 11
        iaamax = max0( iaamax, ja )
        iaamin = min0( iaamin, ja )
!       iaamax = max0( iaamax, ia(m,j) )
!       iaamin = min0( iaamin, ia(m,j) )
! 11  continue
        kount = kount + 1
        isum  = isum  + ja
!!mr    sum2  = sum2  + ifix( float(ja) * float(ja) )
        sum2  = sum2  + ja*ja
      enddo
!
      mean = isum/kount
      std  = ifix(sqrt(float((sum2/(kount))-mean**2)))
!
      print*,tile,' max=',iaamax,' min=',iaamin,' sum=',isum,
     &       ' i_count_notset=',i_count_notset
      print*,tile,' mean=',mean,' std.dev=',std,
     &       ' ko9s=',kount,kount_9,kount+kount_9 
      return
      end
      SUBROUTINE minmaxj(IM,JM,A,title)

! this routine is using real*4 on the sp

      implicit none

      integer im, jm
      real(kind=4) A(IM,JM),rmin,rmax,undef
      integer i,j,imax,jmax,imin,jmin,iundef
      character*8 title,chara
      data chara/'        '/
!
      chara  = title
      rmin   = 1.e+10
      rmax   = -rmin
      imax   = 0
      imin   = 0
      jmax   = 0
      jmin   = 0
      iundef = 0
      undef  = -9999.
!sela....................................................
!sela if(rmin.eq.1.e+10)return
!sela....................................................
      DO j=1,JM
        DO i=1,IM
          if (A(i,j) >= rmax)then
            rmax = A(i,j)
            imax = i
            jmax = j
          endif
          if (A(i,j) <= rmin)then
             if ( A(i,j) .eq. undef ) then
               iundef = iundef + 1
             else
                rmin = A(i,j)
                imin = i
                jmin = j
             endif
          endif
        ENDDO
      ENDDO
      write(6,150)chara,rmin,imin,jmin,rmax,imax,jmax,iundef
150   format(1x,a8,2x,'rmin=',e13.4,2i6,2x,'rmax=',e13.4,3i6)
!
      RETURN
      END
      subroutine nanc(a,l,c)
! compiler opt TRAPS= -qinitauto=FF911299 -qflttrap=ov:zero:inv:en -qsig trap
! or call subroutine below
! subroutine to report NaNS and NaNQ within an address
! range for real*8 words.
!  as written the routine prints a single line for each call
!  and prints a message and returns to the caller  on detection of the FIRST
!  NaN in the range.  The message is passed in the  third
!  argument.  If no NaN values are found it returns silently.
!  A real*4 version can be created by making A real*4

!    arguments (all are input only)
!
!    A   real*8 variable or array
!    L   number of words to scan (length of array)
!    C   distinctive message set in caller to indicate where
!        the routine was called.
! 
      integer inan1,inan2,inan3,inan4,inaq1,inaq2,inaq3,inaq4
      real word
      integer itest
      equivalence (itest,word)
!
! signaling NaN
      data inan1/x'7F800001'/
      data inan2/x'7FBFFFFF'/
      data inan3/x'FF800001'/
      data inan4/x'FFBFFFFF'/
!
!  quiet NaN
!
      data inaq1/x'7FC00000'/
      data inaq2/x'7FFFFFFF'/
      data inaq3/x'FFC00000'/
      data inaq4/x'FFFFFFFF'/
!
      real(kind=8)a(l),rtc,t1,t2
      character*24 cn
      character*(*) c
      t1=rtc()
!gwv        print *, ' nanc call ',c
      do k=1,l
        word = a(k)
        if( (itest  .GE. inan1 .AND. itest .LE. inan2) .OR.
     *      (itest  .GE. inan3 .AND. itest .LE. inan4) ) then
           print *,' NaNs detected at  word',k,' ',c
          return
        endif
        if( (itest  .GE. inaq1 .AND. itest .LE. inaq2) .OR.
     *      (itest  .GE. inaq3 .AND. itest .LE. inaq4) ) then
          print *,' NaNq detected at  word',k,' ',c
          return
        endif

 101  format(e20.10)
      end do
      t2=rtc()
!gwv      print 102,l,t2-t1,c
 102  format(' time to check ',i9,' words is ',f10.4,' ',a24)
      return
       end
C-----------------------------------------------------------------------

      SUBROUTINE SPTEZJ(JCAP,NC,KM,IDRT,LONB,LATB,JC,WAVE,GRID
     &,                                              latch,idir)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    SPTEZJ      TRANSFORM A SINGLE SPECTRAL FIELD TO GRID
!   PRGMMR: MOORTHI          ORG: W/NMC23     DATE: 13-02-20
!
! ABSTRACT: TRANSFORMS A SINGLE SPECTRAL FIELDS TO GRID
!
! PROGRAM HISTORY LOG:
!   13-02-20  S. MOORTHI
!
! USAGE:    CALL SPTEZJ(JCAP,NC,KM,IDRT,LONB,LATB,JC,WAVE,GRID,IDIR)
!   INPUT ARGUMENT LIST:
!     JCAP         INTEGER SPECTRAL TRUNCATION
!     NC           INTEGER FIRST DIMENSION (NC>=(JCAP+1)*(JCAP+2))
!     KM           INTEGER NUMBER OF LEVELS
!     IDRT         INTEGER DATA REPRESENTATION TYPE
!                (IDRT=4 FOR GAUSSIAN GRID,
!                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
!                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
!     LONB         INTEGER NUMBER OF LONGITUDES
!     LATB         INTEGER NUMBER OF LATITUDES
!     JC           INTEGER NUMBER OF CPUS
!     WAVE         REAL (NC) WAVE FIELD if IDIR>0
!   OUTPUT ARGUMENT LIST:
!     GRID         REAL (cwLONB,LATB,I,KM) GRID FIELD (E->W, N->S) IF IDIR<0
!
!     IDIR     - INTEGER TRANSFORM FLAG
!                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
!     LATCH    - Latitude chunk used in the transform loop
! SUBPROGRAMS CALLED:
!   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!$$$
      implicit none
!
      integer jcap, nc, km, idrt, lonb, latb, jc, latch, idir
      REAL wave(NC,KM)
      REAL GRID(LONB,LATB,KM)
!
      real, allocatable ::  gridl(:,:)
!
      integer lonb2m, i, j, in, is, latbb2, lonb2, j1, j2, jl, ijl, ijn
     &,       ij, js, jn, ja, jb
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SPECTRAL TRANSFORMS
!
      LATBB2 = (LATB+1)/2
      LONB2  = LONB + LONB
      ijn    = LONB2 * LATCH
      allocate (gridl(ijn,km))
      IN     = 1
      IS     = 1 + LONB
!
!     write(0,*)' lonb=',lonb,' lonb2=',lonb2,' latbb2=',latbb2
!    &, ' latch=',latch,' ijn=',ijn,' idir=',idir,' km=',km
!
      if (idir < 0) wave = 0.0
!
      DO J1=1,LATBB2,LATCH
        J2  = MIN(J1+LATCH-1,LATBB2)

!       JL  = 2*(J2-J1+1)
!       IJL = LONB*JL
!       IJ    = LONB2 * (J2-J1+1)

        if (idir > 0) then
!         write(0,*)' waveb=',wave(1:5,1)
          CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN
     &,               J1,J2,JC,WAVE,GRIDL(IN,1),GRIDL(IS,1),1)
          do j=j1,j2
            jn = j
            js = latb+1-j
            ja = (J-J1)*lonb2
            jb = ja + lonb
            do i=1,lonb
              grid(i,jn,:) = gridl(I+ja,:)
              grid(i,js,:) = gridl(I+jb,:)
            enddo
          enddo
!         write(0,*)' grida=',grid(lonb/2,jn,:)
        else
!         write(0,*)' SPTEZJ: j1=',j1,' j2=',j2
          do j=j1,j2
            jn = j
            js = latb+1-j
            ja = (J-J1)*lonb2
            jb = ja + lonb
            do i=1,lonb
              gridl(I+ja,:) = grid(i,jn,:)
              gridl(I+jb,:) = grid(i,js,:)
            enddo
          enddo
!       write(0,*)' BEF SPTRAN  gridlN=',gridl(ja+1:ja+lonb,1),' j=',j
!       write(0,*)' BEF SPTRAN  gridlS=',gridl(jb+1:jb+lonb,1),' j=',j
          CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN
     &,               J1,J2,JC,WAVE,GRIDL(IN,1),GRIDL(IS,1),-1)
!         write(0,*)' wave=',wave(1:10,1)
        endif
!
      ENDDO              ! j - loop
      deallocate (gridl)
!
      END
C-----------------------------------------------------------------------
      SUBROUTINE SPLAT0(IDRT,JMAX,SLAT,WLAT)
C     SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPLAT      COMPUTE LATITUDE FUNCTIONS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-20
C
C ABSTRACT: COMPUTES COSINES OF COLATITUDE AND GAUSSIAN WEIGHTS
C           FOR ONE OF THE FOLLOWING SPECIFIC GLOBAL SETS OF LATITUDES.
C             GAUSSIAN LATITUDES (IDRT=4)
C             EQUALLY-SPACED LATITUDES INCLUDING POLES (IDRT=0)
C             EQUALLY-SPACED LATITUDES EXCLUDING POLES (IDRT=256)
C           THE GAUSSIAN LATITUDES ARE LOCATED AT THE ZEROES OF THE
C           LEGENDRE POLYNOMIAL OF THE GIVEN ORDER.  THESE LATITUDES
C           ARE EFFICIENT FOR REVERSIBLE TRANSFORMS FROM SPECTRAL SPACE.
C           (ABOUT TWICE AS MANY EQUALLY-SPACED LATITUDES ARE NEEDED.)
C           THE WEIGHTS FOR THE EQUALLY-SPACED LATITUDES ARE BASED ON
C           ELLSAESSER (JAM,1966).  (NO WEIGHT IS GIVEN THE POLE POINT.)
C           NOTE THAT WHEN ANALYZING GRID TO SPECTRAL IN LATITUDE PAIRS,
C           IF AN EQUATOR POINT EXISTS, ITS WEIGHT SHOULD BE HALVED.
C           THIS VERSION INVOKES THE IBM ESSL MATRIX SOLVER.
C
C PROGRAM HISTORY LOG:
C   96-02-20  IREDELL
C   97-10-20  IREDELL  ADJUST PRECISION
C   98-06-11  IREDELL  GENERALIZE PRECISION USING FORTRAN 90 INTRINSIC
C 1998-12-03  IREDELL  GENERALIZE PRECISION FURTHER
C 1998-12-03  IREDELL  USES AIX ESSL BLAS CALLS
C 2009-12-27  DSTARK   updated to switch between ESSL calls on an AIX
C                      platform, and Numerical Recipies calls elsewise.
C 2010-12-30  SLOVACEK update alignment so preprocessor does not cause
C                      compilation failure
C 2012-09-01  E.Mirvis & M.Iredell merging & debugging linux errors 
C			of _d and _8 using generic LU factorization.   
C 2012-11-05  E.Mirvis  generic FFTPACK and LU lapack were removed
C----------------------------------------------------------------
C USAGE:    CALL SPLAT(IDRT,JMAX,SLAT,WLAT)
C
C   INPUT ARGUMENT LIST:
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C
C   OUTPUT ARGUMENT LIST:
C     SLAT     - REAL (JMAX) SINES OF LATITUDE.
C     WLAT     - REAL (JMAX) GAUSSIAN WEIGHTS.
C
C SUBPROGRAMS CALLED:
C   DGEF         MATRIX FACTORIZATION - ESSL
C   DGES         MATRIX SOLVER - ESSL
C   LUDCMP       LU factorization - numerical recipies
C   LUBKSB       Matrix solver - numerical recipies
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C
      REAL SLAT(JMAX),WLAT(JMAX)
      INTEGER,PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: SLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      PARAMETER(JZ=50)
      REAL BZ(JZ)
      DATA BZ        / 2.4048255577,  5.5200781103,
     $  8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679,
     $ 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684,
     $ 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132,
     $ 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550,
     $ 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299,
     $ 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711,
     $ 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819,
     $ 96.6052679510, 99.7468198587, 102.888374254, 106.029930916,
     $ 109.171489649, 112.313050280, 115.454612653, 118.596176630,
     $ 121.737742088, 124.879308913, 128.020877005, 131.162446275,
     $ 134.304016638, 137.445588020, 140.587160352, 143.728733573,
     $ 146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      REAL:: DLT,D1=1.
      REAL AWORK((JMAX+1)/2,((JMAX+1)/2)),BWORK(((JMAX+1)/2))
      INTEGER:: JHE,JHO,J0=0
      INTEGER IPVT((JMAX+1)/2)
      PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1./SQRT((JMAX+0.5)**2+C)
        DO J=1,MIN(JH,JZ)
          SLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          SLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.
          DO J=1,JH
            PKM1(J)=1.
            PK(J)=SLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*SLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.-SLATD(J)**2)/(JMAX*(PKM1(J)-SLATD(J)*PK(J)))
            SLATD(J)=SLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
CDIR$ IVDEP
        DO J=1,JH
          SLAT(J)=SLATD(J)
          WLAT(J)=(2.*(1.-SLATD(J)**2))/(JMAX*PKM1(J))**2
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2./JMAX**2
          DO N=2,JMAX,2
            WLAT(JHE)=WLAT(JHE)*N**2/(N-1)**2
          ENDDO
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        SLAT(1)=1.
        DO J=2,JH
          SLAT(J)=COS((J-1)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*J*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
!#if IBM4 || IBM8
!        CALL DGEF(AWORK,JHE,JHO,IPVT)
!        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
!#endif
!#if LINUX
        call ludcmp(awork,jho,jhe,ipvt)
        call lubksb(awork,jho,jhe,ipvt,bwork)
!#endif
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J+1)=BWORK(J)
        ENDDO
CDIR$ IVDEP
        DO J=1,JH
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        SLAT(1)=1.
        DO J=1,JH
          SLAT(J)=COS((J-0.5)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*(J-0.5)*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
!#if IBM4 || IBM8
!        CALL DGEF(AWORK,JHE,JHO,IPVT)
!        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
!#endif
!#if LINUX
        call ludcmp(awork,jho,jhe,ipvt,d)
        call lubksb(awork,jho,jhe,ipvt,bwork)
!#endif
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J)=BWORK(J)
        ENDDO
CDIR$ IVDEP
        DO J=1,JH
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
