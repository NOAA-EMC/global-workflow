C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  TERRAIN  TERRAIN MAKER FOR GLOBAL SPECTRAL MODEL
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 92-04-16
C
C ABSTRACT: THIS PROGRAM CREATES 7 TERRAIN-RELATED FILES
C   COMPUTED FROM THE NAVY 10-MINUTE TERRAIN DATASET.
C   THE MODEL PHYSICS GRID PARAMETERS AND SPECTRAL TRUNCATION
C   AND FILTER PARAMETERS ARE READ BY THIS PROGRAM AS INPUT.
C   THE 7 FILES PRODUCED ARE RESPECTIVELY:
C     1) SEA-LAND MASK ON MODEL PHYSICS GRID
C     2) GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
C     3) MOUNTAIN STD DEV ON MODEL PHYSICS GRID
C     4) SPECTRAL OROGRAPHY IN SPECTRAL DOMAIN
C     5) UNFILTERED GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
C     6) GRIB SEA-LAND MASK ON MODEL PHYSICS GRID
C     7) GRIB GRIDDED OROGRAPHY ON MODEL PHYSICS GRID
C   THE OROGRAPHY IS ONLY FILTERED FOR WAVENUMBERS GREATER THAN NF0.
C   FOR WAVENUMBERS N BETWEEN NF0 AND NF1, THE OROGRAPHY IS FILTERED
C   BY THE FACTOR 1-((N-NF0)/(NF1-NF0))**2.  THE FILTERED OROGRAPHY
C   WILL NOT HAVE INFORMATION BEYOND WAVENUMBER NF1.
C
C PROGRAM HISTORY LOG:
C   92-04-16  IREDELL
C   98-02-02  IREDELL  FILTER
C   98-05-31  HONG Modified for subgrid orography used in Kim's scheme
C   98-12-31  HONG Modified for high-resolution GTOPO orography
C   99-05-31  HONG Modified for getting OL4 (mountain fraction)
!   00-02-10  Moorthi's modifications
C   00-04-11  HONG Modified for reduced grids
C   00-04-12  Iredell Modified for reduced grids
C   02-01-07  (*j*) modified for principal axes of orography
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
C       
C
C USAGE:
C
C   INPUT FILES:
C     UNIT5      - PHYSICS LONGITUDES (IM), PHYSICS LATITUDES (JM),
C                  SPECTRAL TRUNCATION (NM), RHOMBOIDAL FLAG (NR),
C                  AND FIRST AND SECOND FILTER PARAMETERS (NF0,NF1).
C                  RESPECTIVELY READ IN FREE FORMAT.
C     UNIT235    - GTOPO 30" AVR for ZAVG elevation
C     UNIT10     - 30" UMD land (lake) cover mask  see MSKSRC switch
C    XUNIT11     - GTOPO AVR
C    XUNIT12     - GTOPO STD DEV
C    XUNIT13     - GTOPO MAX
C     UNIT14     - GTOPO SLM (10' NAVY if switched to get lakes 
C     UNIT15     - GICE Grumbine 30" RAMP Antarctica orog IMNx3616
C     UNIT25     - Ocean land-sea mask on gaussian grid         
C
C   OUTPUT FILES:
C     UNIT51     - SEA-LAND MASK (IM,JM)
C     UNIT52     - GRIDDED OROGRAPHY (IM,JM)
C     UNIT54     - SPECTRAL OROGRAPHY ((NM+1)*((NR+1)*NM+2))
C     UNIT55     - UNFILTERED GRIDDED OROGRAPHY (IM,JM)
C     UNIT57     - GRIB GRIDDED OROGRAPHY (IM,JM)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C     TERSUB     - MAIN SUBPROGRAM
C     SPLAT      - COMPUTE GAUSSIAN LATITUDES OR EQUALLY-SPACED LATITUDES
C     LIBRARY:
C     SPTEZ      - SPHERICAL TRANSFORM
C     GBYTES     - UNPACK BITS
C
C   REMARKS: FORTRAN 9X EXTENSIONS ARE USED.
C           
C ATTRIBUTES:
C   CRAY YMP & IBM AIX 3 5 00C88D5D4C00.
C
C$$$
CFPP$ NOCONCUR F
      include 'netcdf.inc'
      logical fexist, opened
      integer fsize, ncid, error, id_dim, nx, ny
      character(len=256) :: OUTGRID = "none"
      character(len=256) :: INPUTOROG = "none"
      logical            :: do_oa = .true. ! create oa and ol data.
      logical            :: grid_from_file = .true.
      integer :: MTNRES,IM,JM,NM,NR,NF0,NF1,EFAC,BLAT,NW
      fsize=65536
      READ(5,*) MTNRES,IM,JM,NM,NR,NF0,NF1,EFAC,BLAT
      READ(5,*) OUTGRID
      READ(5,*) INPUTOROG
!      MTNRES=1
!      IM=48
!      JM=48
!      NM=46
!      NF0=0             
!      NF1=0              
!      efac=0
!      blat=0
!      NR=0
!      OUTGRID = "C48_grid.tile1.nc"
!      INPUTOROG = "oro.288x144.nc"
      print*, "INPUTOROG=", trim(INPUTOROG)
      print*, "IM,JM=", IM, JM
! --- MTNRES defines the input (highest) elev resolution
! --- =1 is topo30 30" in units of 1/2 minute.
!     so MTNRES for old values must be *2.
!     =16 is now Song Yu's 8' orog the old ops standard
! --- other possibilities are =8 for 4' and =4 for 2' see
!     HJ for T1000 test. Must set to 1 for now.
      MTNRES=1
      print*, MTNRES,IM,JM,NM,NR,NF0,NF1,EFAC,BLAT
      NW=(NM+1)*((NR+1)*NM+2)
      IMN = 360*120/MTNRES
      JMN = 180*120/MTNRES
      print *, ' Starting terr12 mtnlm7_slm30.f  IMN,JMN:',IMN,JMN

! --- read the grid resolution if the OUTGRID exists.
      if( trim(OUTGRID) .NE. "none" ) then
         inquire(file=trim(OUTGRID), exist=fexist)
         if(.not. fexist) then
            print*, "file "//trim(OUTGRID)//" does not exist"
            CALL ERREXIT(4)
         endif
         do ncid = 103, 512
           inquire( ncid,OPENED=opened )
           if( .NOT.opened )exit
         end do

         print*, "outgrid=", trim(outgrid)
         error=NF__OPEN(trim(OUTGRID),NF_NOWRITE,fsize,ncid)
         call netcdf_err(error, 'Open file '//trim(OUTGRID) )
         error=nf_inq_dimid(ncid, 'nx', id_dim)
         call netcdf_err(error, 'inquire dimension nx from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,nx)
         call netcdf_err(error, 'inquire dimension nx length '//
     &       'from file '//trim(OUTGRID) )
         
         error=nf_inq_dimid(ncid, 'ny', id_dim)
         call netcdf_err(error, 'inquire dimension ny from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,ny)
         call netcdf_err(error, 'inquire dimension ny length '//
     &       'from file '//trim(OUTGRID) )
         print*, "nx = ", nx
         if(IM .ne. nx/2) then
            print*, "IM=",IM, " /= grid file nx/2=",nx/2
            print*, "Set IM = ", nx/2
            IM = nx/2
         endif
         if(JM .ne. ny/2) then
            print*, "JM=",JM, " /= grid file ny/2=",ny/2
            print*, "Set JM = ", ny/2
            JM = ny/2
         endif
         error=nf_close(ncid)
         call netcdf_err(error, 'close file '//trim(OUTGRID) )
         
      endif         
         
      
      CALL TERSUB(IMN,JMN,IM,JM,NM,NR,NF0,NF1,NW,EFAC,BLAT,
     &            OUTGRID,INPUTOROG)
      STOP
      END
      SUBROUTINE TERSUB(IMN,JMN,IM,JM,NM,NR,NF0,NF1,NW,EFAC,BLAT,
     &     OUTGRID,INPUTOROG)
!jaa      use ipfort
      implicit none
      include 'machine.h'
      include 'netcdf.inc'
C
      integer                      :: IMN,JMN,IM,JM,NW
      character(len=*), intent(in) :: OUTGRID
      character(len=*), intent(in) :: INPUTOROG
      integer :: NR,NF0,NF1
      real, parameter :: MISSING_VALUE=-9999.
      real, PARAMETER :: PI=3.1415926535897931
      integer :: efac, blat
      integer, PARAMETER :: NMT=14
      INTEGER ZSLMX(2700,1350)
      integer NM
      logical LATLONGRID
      INTEGER,allocatable::  ZAVG(:,:),ZSLM(:,:)
      REAL(4),allocatable::  GICE(:,:),OCLSM(:,:)
      real :: DEGRAD
      integer*1,allocatable:: UMD(:,:)
      integer*1 i3save
      integer*2 glob(IMN,JMN), i2save
      logical grid_from_file
      INTEGER KPDS(200),KGDS(200), zsave1,zsave2,itopo,kount
      INTEGER kount2,islmx,jslmx,oldslm,msksrc,mskocn,notocn
      REAL COSCLT(JM),WGTCLT(JM),RCLT(JM),XLAT(JM),DIFFX(JM/2)
      REAL XLON(IM)
      LOGICAL is_south_pole(IM,JM), is_north_pole(IM,JM)
      REAL GEOLON(IM,JM),GEOLAT(IM,JM)
      REAL, allocatable :: tmpvar(:,:)
      REAL GEOLON_C(IM+1,JM+1),GEOLAT_C(IM+1,JM+1)
      REAL DX(IM,JM),DY(IM,JM)
      REAL SLM(IM,JM),ORO(IM,JM),VAR(IM,JM),ORS(NW),ORF(IM,JM)
      REAL land_frac(IM,JM)
      REAL THETA(IM,JM),GAMMA(IM,JM),SIGMA(IM,JM),ELVMAX(IM,JM)
      REAL WZ4(IM,JM),VAR4(IM,JM),OA(IM,JM,4),OL(IM,JM,4),SLMI(IM,JM)
      integer IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM)
      integer IWORK(IM,JM,4)
      real    WORK1(IM,JM),WORK2(IM,JM),WORK3(IM,JM),WORK4(IM,JM)
      real    WORK5(IM,JM),WORK6(IM,JM),GLAT(JMN)
      LOGICAL SPECTR, REVLAT, FILTER
      logical fexist
      real HPRIME(IM,JM,14)
      real oaa(4),ola(4),sumdif,avedif,alon,alat
      real, allocatable :: oa_in(:,:,:), ol_in(:,:,:)
      real, allocatable :: slm_in(:,:), lon_in(:,:), lat_in(:,:)
      integer numi(jm),ios,iosg,latg2,istat
      integer  maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
      integer lonsperlat(jm/2),itest,jtest
      integer i, j, nx, ny, ncid, js, jn, iw, ie, k
      integer it,jt,i1,error,id_dim,id_var,nx_in,ny_in
      integer i_south_pole,j_south_pole,i_north_pole,j_north_pole
      real    maxlat, minlat
      logical opened
      logical LB(IM*JM)
      integer fsize,wgta,IN,INW,INE,IS,ISW,ISE,M,N,IMT,IRET
      complex ffj(im/2+1)
      real    dlat,PHI,DELXN,RS,RN,slma,oroa,vara,var4a,xn,XS,FFF
      real    WWW
      real :: timef,tbeg,tend,tbeg1
      logical :: output_binary
      output_binary = .false.
      tbeg1=timef()
      tbeg=timef()
      fsize = 65536

       allocate (ZAVG(IMN,JMN))
       allocate (ZSLM(IMN,JMN))
       allocate (GICE(IMN+1,3601))
       allocate (UMD(IMN,JMN))
       allocate (OCLSM(IM,JM))

!
!  SET CONSTANTS AND ZERO FIELDS
!
      DEGRAD = 180./PI
      SPECTR = NM .GT. 0     ! if NM <=0 grid is assumed lat/lon
      FILTER = .TRUE.        ! Spectr Filter defaults true and set by NF1 & NF0
!     MSKSRC = 0             ! MSKSRC=0 navy 10 lake msk, 1 UMD 30, -1 no lakes
      MSKSRC = 1             ! MSKSRC=0 navy 10 lake msk, 1 UMD 30, -1 no lakes
      REVLAT = BLAT .LT. 0   ! Reverse latitude/longitude for output
      ITOPO  = 1             ! topo 30" read, otherwise tiles (opt offline)
      MSKOCN = 1             ! Ocean land sea mask =1, =0 if not present
      NOTOCN = 1             ! =1 Ocean lsm input reverse: Ocean=1, land=0 
! --- The LSM Gaussian file from the ocean model sometimes arrives with 
! --- 0=Ocean and 1=Land or it arrives with 1=Ocean and 0=land without 
! --- metadata to distinguish its disposition.  The AI below mitigates this.

      print *,' In TERSUB, ITOPO=',itopo
                      if (mskocn .eq. 1)then
      print *,' Ocean Model LSM Present and '
      print *, ' Overrides OCEAN POINTS in LSM: mskocn=',mskocn
                    if (notocn .eq. 1)then
      print *,' Ocean LSM Reversed:  NOTOCN=',notocn
                    endif
                      endif
C
C --- old S-Y. files
C- OPEN(UNIT=11,FORM='FORMATTED',ERR=900) ! average
C- OPEN(UNIT=12,FORM='FORMATTED',ERR=900) ! Std Dev
C- OPEN(UNIT=13,FORM='FORMATTED',ERR=900) ! maximum
C- OPEN(UNIT=14,FORM='FORMATTED',ERR=900) ! sea-land-lake-mask
C
! ---      READ(11,11) ZAVG
! ---      READ(12,11) ZVAR
! ---      READ(13,11) ZMAX
! --- 11    FORMAT(20I4)
!
! ---  MSKSRC 0 navy 10' lake mask, =1 for 30" UMD lake mask, 
! ---  MSKSRC internally set if above fails at -1 for no lakes
! ---
           IF (MSKSRC .eq. 0 ) then 
              READ(14,12,iostat=ios) ZSLMX
   12    FORMAT(80I1)
      if (ios.ne.0) then 
            MSKSRC=-1
          print *,' navy10 lake mask rd fail -- ios,MSKSRC:',ios,MSKSRC
      endif
           ELSE
      print *,' Attempt to open/read UMD 30" slmsk MSKSRC=',MSKSRC
! --- not 0 so MSKSRC=1 and attempt to open/read UMD 30" slmsk
!     open(10,file=
!    &"/scratch2/portfolios/NCEPDEV/global/noscrub/Jordan.Alpert/wx23ja
!    &/terrain30/landcover30.fixed", 
!    & recl=43200*21600, access='direct',iostat=istat)
       open(10,file="landcover30.fixed",
     & recl=43200*21600, access='direct',iostat=istat)

                  IF (istat.ne.0) then
                   MSKSRC=-1
      print *,' UMD lake mask open failed -- ios,MSKSRC:',istat,MSKSRC
                  ELSE 
!
              read(10, rec=1,iostat=istat) UMD
      print *,' UMD lake mask opened OK   -- ios,MSKSRC:',istat,MSKSRC
!
                  ENDIF
! --------------
              IF (istat.ne.0) then
! --- When UMD read fails attempt to read navy 10'
      print *,' UMD lake mask rd err -- trying navy 10',istat
                MSKSRC=0
          print *,' ***** MSKSRC set to 0 MSKSRC=',MSKSRC
               if (MSKSRC .eq. 0 ) then 
                   READ(14,12,iostat=ios) ZSLMX
                  if (ios.ne.0)  then
                   MSKSRC=-1
           print *,' navy10 lake mask rd fail - ios,MSKSRC:',ios,MSKSRC
                  endif
               endif
              ELSE
           print *,' UMD lake, UMD(50,50)=',UMD(50,50),MSKSRC
              ENDIF
! --------------
! ---      good UMD land cover read and MSKSRC=1
           ENDIF        
C
C- READ_G for global 30" terrain 
C
       print *,' About to call read_g, ITOPO=',ITOPO
          if ( ITOPO .ne. 0 ) call read_g(glob,ITOPO)
! --- transpose even though glob 30" is from S to N and NCEP std is N to S
       do j=1,jmn/2
       do I=1,imn
        jt=jmn - j + 1
        i2save = glob(I,j)
        glob(I,j)=glob(I,jt)
        glob(I,jt) = i2save
       enddo
       enddo 
! --- transpose glob as USGS 30" is from dateline and NCEP std is 0
       do j=1,jmn
       do I=1,imn/2
        it=imn/2 + i 
        i2save = glob(i,J)
        glob(i,J)=glob(it,J)
        glob(it,J) = i2save
       enddo
       enddo 
       print *,' After read_g, glob(500,500)=',glob(500,500)
!

!  --- IMN,JMN
      print*, ' IM, JM, NM, NR, NF0, NF1, EFAC, BLAT'
      print*, IM,JM,NM,NR,NF0,NF1,EFAC,BLAT
       print *,'  imn,jmn,glob(imn,jmn)=',imn,jmn,glob(imn,jmn)
       print *,' UBOUND ZAVG=',UBOUND(ZAVG)
       print *,' UBOUND glob=',UBOUND(glob)
       print *,' UBOUND ZSLM=',UBOUND(ZSLM)
       print *,' UBOUND GICE=',UBOUND(GICE)
       print *,' UBOUND OCLSM=',UBOUND(OCLSM)
!
! ---  0 is ocean and 1 is land for slm
!
C
! --- ZSLM initialize with all land 1, ocean 0
!     ZSLM=1
      do j=1,jmn
      do i=1,imn
      zslm(i,j)=1
      enddo
      enddo

           SELECTCASE(MSKSRC)
C----  30" sea land mask. 0 are water (lake or ocean)
              CASE(1)
! --- transpose even though glob 30" is from S to N and NCEP std is N to S
       do j=1,jmn/2
       do I=1,imn
        jt=jmn - j + 1
        i3save = UMD(I,j)
        UMD(I,j)=UMD(I,jt)
        UMD(I,jt) = i3save
       enddo
       enddo 
! --- transpose UMD as USGS 30" is from dateline and NCEP std is 0
       do j=1,jmn
       do i=1,imn/2
        it=imn/2 + i 
        i3save = UMD(i,J)
        UMD(i,J)=UMD(it,J)
        UMD(it,J) = i3save
       enddo
       enddo
! ---   UMD slmsk with 30" lakes  and set ZAVG from glob
          do j=1,jmn
          do i=1,imn
           if ( UMD(i,j) .eq. 0 ) ZSLM(i,j) = 0
           ZAVG(i,j) = glob(i,j)
          enddo
          enddo
! --- Global land in slm plus lakes on 30" grid and elev set over globe
! ---
! ---   When navy 10' mask is set MSKSRC=0
              CASE(0)
! ---  MSKSRC 0 navy 10' lake mask, =1 for 30" UMD lake mask, -1 no lakes
       print *,' NAVY 10 (8) slmsk for lakes, MSKSRC=',MSKSRC 
         kount = 0
         kount2 = 0
       do j=1,jmn
          oldslm = ZSLM(IMN,j)
        do i=1,imn
          i1 = i + 1
! ---    slmsk with 10' lakes  and set ZAVG from 30" glob
        ZAVG(i,j) = glob(i,j)
            if ( glob(i,j) .eq. -9999 ) then 
               ZSLM(i,j) = 0
               kount = kount + 1
            endif
           islmx=(i-1)/16 + 1
           jslmx=(j-1)/16 + 1
          if ( ZSLMX(islmx,jslmx) .eq. 0 ) then
       if ( j .gt. 8 .and. j .lt. JMN-8 ) then
              if (i1 .gt. IMN ) i1 = i1 - IMN
! -----
      if(ZSLM(i,j).eq.1 .and. oldslm .eq. 1 .and. ZSLM(i1,j).eq.1)then  
       if (i .ne. 1) oldslm = ZSLM(i,j)
       ZSLM(i,j) = 0
       kount2 = kount2 + 1
      endif 
! -----
       endif
          endif
        enddo
       enddo
! ---
              CASE(-1)
      print *,' ***** set ZAVG and slm from 30" glob, MSKSRC=',MSKSRC
         kount = 0
         kount2 = 0
       do j=1,jmn
        do i=1,imn
          i1 = i + 1
! ---   UMD slmsk with 10' lakes  and set ZAVG from 30" glob
        ZAVG(i,j) = glob(i,j)
            if ( glob(i,j) .eq. -9999 ) then 
               ZSLM(i,j) = 0
               kount = kount + 1
            endif
        enddo
       enddo
           END SELECT
! ---
! ---  Fixing an error in the topo 30" data set at pole (-9999).  
            do i=1,imn
            ZSLM(i,1)=0
            ZSLM(i,JMN)=1
            enddo
!
!      print *,' kount1,2,ZAVG(1,1),ZAVG(imn,jmn),ZAVG(500,500)',
!     & kount,kount2,ZAVG(1,1),ZAVG(imn,jmn),ZAVG(500,500)
! --- The center of pixel (1,1) is 89.9958333N/179.9958333W with dx/dy 
! --- spacing of 1/120 degrees. 
!
!  READ REDUCED GRID EXTENTS IF GIVEN
!
      read(20,*,iostat=ios) latg2,lonsperlat
      if(ios.ne.0.or.2*latg2.ne.jm) then
        do j=1,jm
          numi(j)=im
        enddo
        print *,ios,latg2,'COMPUTE TERRAIN ON A FULL GAUSSIAN GRID'
      else
        do j=1,jm/2
          numi(j)=lonsperlat(j)
        enddo
        do j=jm/2+1,jm
          numi(j)=lonsperlat(jm+1-j)
        enddo
        print *,ios,latg2,'COMPUTE TERRAIN ON A REDUCED GAUSSIAN GRID',
     &          numi
C       print *,ios,latg2,'COMPUTE TERRAIN ON A REDUCED GAUSSIAN GRID'
      endif
!       print *,ios,latg2,'TERRAIN ON GAUSSIAN GRID',numi
      
!
!    This code assumes that lat runs from north to south for gg!
!
      print *,' SPECTR=',SPECTR,' REVLAT=',REVLAT,' ** with GICE-07 **'
      IF (SPECTR) THEN
        CALL SPLAT(4,JM,COSCLT,WGTCLT)
        DO J=1,JM/2
          RCLT(J)      = ACOS(COSCLT(J))
        ENDDO
        DO J = 1,JM/2
           PHI = RCLT(J) * DEGRAD
           XLAT(J) = 90. - PHI
           XLAT(JM-J+1) =  PHI - 90.
        ENDDO
      ELSE
        CALL SPLAT(0,JM,COSCLT,WGTCLT)
        DO J=1,JM
          RCLT(J) = ACOS(COSCLT(J))
          XLAT(J) = 90.0 - RCLT(J) * DEGRAD
        ENDDO
      ENDIF
!
c      print *,' cosclt=',cosclt
!       print *,' RCLT(1)=',RCLT(1)
       sumdif = 0.
       DO J = JM/2,2,-1
       DIFFX(J) = xlat(J) - XLAT(j-1)
       sumdif = sumdif + DIFFX(J)
       ENDDO
       avedif=sumdif/(float(JM/2))
!      print *,' XLAT= avedif: ',avedif
!      write (6,107) (xlat(J)-xlat(j-1),J=JM,2,-1)
       print *,' XLAT='
       write (6,106) (xlat(J),J=JM,1,-1)
  106 format( 10(f7.3,1x))   
  107 format( 10(f9.5,1x))   
C
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
      print *,
     & ' Before GICE ZAVG(1,2)=',ZAVG(1,2),ZSLM(1,2)
      print *,
     & ' Before GICE ZAVG(1,12)=',ZAVG(1,12),ZSLM(1,12)
      print *,
     & ' Before GICE ZAVG(1,52)=',ZAVG(1,52),ZSLM(1,52)
      print *,
     & ' Before GICE ZAVG(1,112)=',ZAVG(1,JMN-112),ZSLM(1,112)
! GICE: Grumbine 30" Antarctica orog IMNx3616 from S to N & wraped E-W.
! NB: Zfields are S to N and W-E!
       iosg = 0
       READ(15,iostat=iosg) GICE
       if(iosg .ne. 0 ) then
        print *,' *** Err on reading GICE record, iosg=',iosg
        print *,' exec continues but NO GICE correction done '
!       stop
       else
        print *,' GICE 30" Antarctica RAMP orog 43200x3616 read OK'
        print *,' Processing! '
        print *,' Processing! '
        print *,' Processing! '
         do j = 1, 3601 
         do i = 1, IMN
           zsave1 = ZAVG(i,j)
           zsave2 = ZSLM(i,j)
         if( GICE(i,j) .ne. -99. .and.  GICE(i,j) .ne. -1.0 ) then
           if ( GICE(i,j) .gt. 0.) then 
                ZAVG(i,j) = int( GICE(i,j) + 0.5 )
!! --- for GICE values less than or equal to 0 (0, -1, or -99) then
!! --- radar-sat (RAMP) values are not valid and revert back to old orog 
                ZSLM(i,j) = 1
           endif
         endif
!jaa           ALON = float(i-1) * 360./float(IMN)
!jaa           ALAT = glat(j)
!           if(  ZAVG(i,j) .ne. zsave1 .and. i .lt. 3 )
!    & print *,' antarctica change to ZAVG(i=',i,'j=',j,')=',
!    &    ZAVG(i,j),ZSLM(i,j),' from originally:',zsave1,zsave2
!     &write(6,151)i,j,ZAVG(i,j),ZSLM(i,j),zsave1,zsave2,ALAT,ALON
!  151 format(1x,'antarctica ZAVG(i=',i3,' j=',i3,')=',i5,i3,
!     &' orig:',i5,i3,' Lat=',f8.3,f9.3,'E')
!jaa            if(  ZAVG(i,j) .ne. zsave1 ) then 
!jaa          if ( i .le. 1201 .and. i .gt. 1200 )then
!jaa      write(6,152)i,j,ZAVG(i,j),ZSLM(i,j),zsave1,zsave2,ALAT,ALON,
!jaa     &      GICE(i,j)
!jaa          endif
!jaa            endif
  152 format(1x,' ZAVG(i=',i4,' j=',i4,')=',i5,i3,
     &' orig:',i5,i4,' Lat=',f7.3,f8.2,'E',' GICE=',f8.1)
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
!C
C     COMPUTE MOUNTAIN DATA : ORO SLM VAR (Std Dev) OC
C
! --- The coupled ocean model is already on a Guasian grid if (IM,JM)
! --- Attempt to Open the file if mskocn=1
      istat=0
          if (mskocn .eq. 1) then
!     open(25,form='unformatted',iostat=istat)
!     open(25,form='binary',iostat=istat)
! --- open to fort.25 with link to file in script
      open(25,form='formatted',iostat=istat)
                  if (istat.ne.0) then
                  mskocn = 0
      print *,' Ocean lsm file Open failure: mskocn,istat=',mskocn,istat
                  else
                  mskocn = 1
      print *,' Ocean lsm file Opened OK: mskocn,istat=',mskocn,istat
                  endif
! --- Read it in
      ios=0
      OCLSM=0.
!      read(25,iostat=ios)OCLSM
       read(25,*,iostat=ios)OCLSM
         if (ios.ne.0) then 
         mskocn = 0
! --- did not properly read Gaussian grid ocean land-sea mask, but
!     continue using ZSLMX 
      print *,' Rd fail: Ocean lsm - continue, mskocn,ios=',mskocn,ios
         else
         mskocn = 1
      print *,' Rd OK: ocean lsm:  mskocn,ios=',mskocn,ios
! --- LSM initialized to ocean mask especially for case where Ocean
! --- changed by ocean model to land to cope with its problems
! --- remember, that lake mask is in zslm to be assigned in MAKEMT.
          if ( mskocn .eq. 1 ) then
      DO J = 1,JM
      DO I = 1,numi(j)
            if ( notocn .eq. 0 ) then
            slmi(i,j) = float(NINT(OCLSM(i,j)))
            else
                     if ( NINT(OCLSM(i,j)) .eq. 0) then
                     slmi(i,j) = 1
                     else 
                     slmi(i,j) = 0
                     endif
            endif
      enddo
      enddo
      print *,' OCLSM',OCLSM(1,1),OCLSM(50,50),OCLSM(75,75),OCLSM(IM,JM)
      print *,' SLMI:',SLMI(1,1),SLMI(50,50),SLMI(75,75),SLMI(IM,JM)
! --- Diag
!        WRITE(27,iostat=ios) REAL(SLMI,4)
!        print *,' write SLMI/OCLSM diag input:',ios
          endif
         endif

          else
          print *,' Not using Ocean model land sea mask'
          endif

      if (mskocn .eq. 1)then
      print *,' LSM:',OCLSM(1,1),OCLSM(50,50),OCLSM(75,75),OCLSM(IM,JM)
      endif

!--- reading grid file.
      grid_from_file = .false.
      is_south_pole = .false.
      is_north_pole = .false.
      i_south_pole = 0
      j_south_pole = 0
      i_north_pole = 0
      j_north_pole = 0
      if( trim(OUTGRID) .NE. "none" ) then
         grid_from_file = .true.
         inquire(file=trim(OUTGRID), exist=fexist)
         if(.not. fexist) then
            print*, "file "//trim(OUTGRID)//" does not exist"
            CALL ERREXIT(4)
         endif
         do ncid = 103, 512
           inquire( ncid,OPENED=opened )
           if( .NOT.opened )exit
         end do

         print*, "outgrid=", trim(outgrid)
         error=NF__OPEN(trim(OUTGRID),NF_NOWRITE,fsize,ncid)
         call netcdf_err(error, 'Open file '//trim(OUTGRID) )
         error=nf_inq_dimid(ncid, 'nx', id_dim)
         call netcdf_err(error, 'inquire dimension nx from file '//
     &                   trim(OUTGRID) )
         nx = 2*IM
         ny = 2*JM
!         error=nf_inq_dimlen(ncid,id_dim,nx)
!         print*, "nx = ", nx, id_dim
!         call netcdf_err(error, 'inquire dimension nx length '//
!     &       'from file '//trim(OUTGRID) )
!         error=nf_inq_dimid(ncid, 'ny', id_dim)
!         call netcdf_err(error, 'inquire dimension ny from file '//
!     &                   trim(OUTGRID) )
!         error=nf_inq_dimlen(ncid,id_dim,ny)
!         call netcdf_err(error, 'inquire dimension ny length '//
!     &       'from file '//trim(OUTGRID) )
!        IM should equal nx/2 and JM should equal ny/2
!         if(IM .ne. nx/2) then
!            print*, "IM=",IM, " /= grid file nx/2=",nx/2
!            CALL ERREXIT(4)
!         endif
!         if(JM .ne. ny/2) then
!            print*, "JM=",JM, " /= grid file ny/2=",ny/2
!            CALL ERREXIT(4)
!         endif
         print*, "Read the grid from file "//trim(OUTGRID)

         allocate(tmpvar(nx+1,ny+1))

         error=nf_inq_varid(ncid, 'x', id_var)
         call netcdf_err(error, 'inquire varid of x from file '
     &                   //trim(OUTGRID) )
         error=nf_get_var_double(ncid, id_var, tmpvar)
         call netcdf_err(error, 'inquire data of x from file '
     &                   //trim(OUTGRID) )
         !--- adjust lontitude to be between 0 and 360.
         do j = 1,ny+1; do i = 1,nx+1
            if(tmpvar(i,j) .NE. MISSING_VALUE) then
              if(tmpvar(i,j) .GT. 360) tmpvar(i,j) = tmpvar(i,j) - 360
              if(tmpvar(i,j) .LT. 0) tmpvar(i,j) = tmpvar(i,j) + 360
            endif
         enddo; enddo

         geolon(1:IM,1:JM) = tmpvar(2:nx:2,2:ny:2)
         geolon_c(1:IM+1,1:JM+1) = tmpvar(1:nx+1:2,1:ny+1:2)
         
         error=nf_inq_varid(ncid, 'y', id_var)
         call netcdf_err(error, 'inquire varid of y from file '
     &                   //trim(OUTGRID) )
         error=nf_get_var_double(ncid, id_var, tmpvar)
         call netcdf_err(error, 'inquire data of y from file '
     &                   //trim(OUTGRID) )
         geolat(1:IM,1:JM) = tmpvar(2:nx:2,2:ny:2)
         geolat_c(1:IM+1,1:JM+1) = tmpvar(1:nx+1:2,1:ny+1:2)

         !--- figure out pole location.
         maxlat = -90
         minlat = 90
         i_north_pole = 0
         j_north_pole = 0
         i_south_pole = 0
         j_south_pole = 0
         do j = 1, ny+1; do i = 1, nx+1
            if( tmpvar(i,j) > maxlat ) then
               i_north_pole=i
               j_north_pole=j
               maxlat = tmpvar(i,j)
            endif
            if( tmpvar(i,j) < minlat ) then
               i_south_pole=i
               j_south_pole=j
               minlat = tmpvar(i,j)
            endif
         enddo ; enddo
         !--- only when maxlat is close to 90. the point is north pole
         if(maxlat < 89.9 ) then
            i_north_pole = 0
            j_north_pole = 0
         endif
         if(minlat > -89.9 ) then
            i_south_pole = 0
            j_south_pole = 0
         endif
         print*, "minlat=", minlat, "maxlat=", maxlat
         print*, "north pole supergrid index is ",
     &           i_north_pole, j_north_pole
         print*, "south pole supergrid index is ",
     &           i_south_pole, j_south_pole
         deallocate(tmpvar)

         if(i_south_pole >0 .and. j_south_pole > 0) then
           if(mod(i_south_pole,2)==0) then ! stretched grid
             do j = 1, JM; do i = 1, IM
               if(i==i_south_pole/2 .and. (j==j_south_pole/2 
     &              .or. j==j_south_pole/2+1) ) then
                 is_south_pole(i,j) = .true.
                 print*, "south pole at i,j=", i, j
               endif
             enddo; enddo      
            else
               do j = 1, JM; do i = 1, IM
                  if((i==i_south_pole/2 .or. i==i_south_pole/2+1)
     &             .and. (j==j_south_pole/2 .or.
     &                j==j_south_pole/2+1) ) then
                    is_south_pole(i,j) = .true.
                    print*, "south pole at i,j=", i, j
                  endif
               enddo; enddo
            endif            
         endif
         if(i_north_pole >0 .and. j_north_pole > 0) then
            if(mod(i_north_pole,2)==0) then ! stretched grid
               do j = 1, JM; do i = 1, IM
                  if(i==i_north_pole/2 .and. (j==j_north_pole/2 .or.
     &                j==j_north_pole/2+1) ) then
                    is_north_pole(i,j) = .true.
                    print*, "north pole at i,j=", i, j
                  endif
               enddo; enddo      
            else
               do j = 1, JM; do i = 1, IM
                  if((i==i_north_pole/2 .or. i==i_north_pole/2+1)
     &             .and. (j==j_north_pole/2 .or.
     &                j==j_north_pole/2+1) ) then
                    is_north_pole(i,j) = .true.
                    print*, "north pole at i,j=", i, j
                  endif
               enddo; enddo
            endif            
         endif
         
         
         allocate(tmpvar(nx,ny))
         error=nf_inq_varid(ncid, 'area', id_var)
         call netcdf_err(error, 'inquire varid of area from file '
     &                   //trim(OUTGRID) )
         error=nf_get_var_double(ncid, id_var, tmpvar)
         call netcdf_err(error, 'inquire data of area from file '
     &                   //trim(OUTGRID) )

         do j = 1, jm
            do i = 1, im
               dx(i,j) = sqrt(tmpvar(2*i-1,2*j-1)+tmpvar(2*i,2*j-1)
     &                       +tmpvar(2*i-1,2*j  )+tmpvar(2*i,2*j  ))
               dy(i,j) = dx(i,j)
            enddo
         enddo
!         allocate(tmpvar(nx,ny+1))

!         error=nf_inq_varid(ncid, 'dx', id_var)
!         call netcdf_err(error, 'inquire varid of dx from file '
!     &                   //trim(OUTGRID) )
!         error=nf_get_var_double(ncid, id_var, tmpvar)
!         call netcdf_err(error, 'inquire data of dx from file '
!     &                   //trim(OUTGRID) )
!         dx(1:IM,1:JM+1) = tmpvar(2:nx:2,1:ny+1:2)
!         deallocate(tmpvar)

!          allocate(tmpvar(nx+1,ny))
!         error=nf_inq_varid(ncid, 'dy', id_var)
!         call netcdf_err(error, 'inquire varid of dy from file '
!     &                   //trim(OUTGRID) )
!         error=nf_get_var_double(ncid, id_var, tmpvar)
!         call netcdf_err(error, 'inquire data of dy from file '
!     &                   //trim(OUTGRID) )
!         dy(1:IM+1,1:JM) = tmpvar(1:nx+1:2,2:ny:2)
!         deallocate(tmpvar)         
      endif
      tend=timef()
      write(6,*)' Timer 1 time= ',tend-tbeg
                                !
! --- CALL MAKEMT(ZAVG,ZSLM,ORO,OCLSM,mskocn,SLM,VAR,VAR4,GLAT,
      if(grid_from_file) then
       tbeg=timef()
         CALL MAKEMT2(ZAVG,ZSLM,ORO,SLM,land_frac,VAR,VAR4,GLAT,
     & IM,JM,IMN,JMN,geolon_c,geolat_c)
      tend=timef()
      write(6,*)' MAKEMT2 time= ',tend-tbeg
      else

        CALL MAKEMT(ZAVG,ZSLM,ORO,SLM,VAR,VAR4,GLAT,
     &   IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
      endif
       call minmxj(IM,JM,ORO,'     ORO')
       call minmxj(IM,JM,SLM,'     SLM')
       call minmxj(IM,JM,VAR,'     VAR')
       call minmxj(IM,JM,VAR4,'    VAR4')
C --- check for nands in above
!      call nanc(ORO,IM*JM,"MAKEMT_ORO")
!      call nanc(SLM,IM*JM,"MAKEMT_SLM")
!      call nanc(VAR,IM*JM,"MAKEMT_VAR")
!      call nanc(VAR4,IM*JM,"MAKEMT_VAR4")
!
C   check antarctic pole 
!     DO J = 1,JM
!     DO I = 1,numi(j)
!        if ( i .le. 100 .and. i .ge. 1 )then
!           if (j .ge. JM-1 )then
!      if (height .eq. 0.) print *,'I,J,SLM:',I,J,SLM(I,J)
!      write(6,153)i,j,ORO(i,j),HEIGHT,SLM(i,j)
!             endif
!        endif    
!     ENDDO
!     ENDDO
C
C ===  Compute mtn principal coord HTENSR: THETA,GAMMA,SIGMA
C
       if(grid_from_file) then       
      tbeg=timef()
         CALL MAKEPC2(ZAVG,ZSLM,THETA,GAMMA,SIGMA,GLAT,
     1            IM,JM,IMN,JMN,geolon_c,geolat_c)
      tend=timef()
      write(6,*)' MAKEPC2 time= ',tend-tbeg
       else
         CALL MAKEPC(ZAVG,ZSLM,THETA,GAMMA,SIGMA,GLAT,
     1            IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
       endif
        
       call minmxj(IM,JM,THETA,'   THETA')
       call minmxj(IM,JM,GAMMA,'   GAMMA')
       call minmxj(IM,JM,SIGMA,'   SIGMA')

C --- check for nands in above
!      call nanc(THETA,IM*JM,"MAKEPC_THETA")
!      call nanc(GAMMA,IM*JM,"MAKEPC_GAMMA")
!      call nanc(SIGMA,IM*JM,"MAKEPC_SIGMA")
C
C     COMPUTE MOUNTAIN DATA : OA OL
C
       call minmxj(IM,JM,ORO,'     ORO')
       print*, "inputorog=", trim(INPUTOROG)
       if(grid_from_file) then
         if(trim(INPUTOROG) == "none") then
           print*, "calling MAKEOA2 to compute OA, OL"
      tbeg=timef()
           CALL MAKEOA2(ZAVG,zslm,VAR,GLAT,OA,OL,IWORK,ELVMAX,ORO,
     1            WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,
     2            IM,JM,IMN,JMN,geolon_c,geolat_c,
     3            geolon,geolat,dx,dy,is_south_pole,is_north_pole)
      tend=timef()
      write(6,*)' MAKEOA2 time= ',tend-tbeg
         else
           !-- read the data from INPUTOROG file.
           error=NF__OPEN(trim(INPUTOROG),NF_NOWRITE,fsize,ncid)
           call netcdf_err(error, 'Open file '//trim(INPUTOROG) )
           error=nf_inq_dimid(ncid, 'lon', id_dim)
           call netcdf_err(error, 'inquire dimension lon from file '//
     &                   trim(INPUTOROG) )
           error=nf_inq_dimlen(ncid,id_dim,nx_in)
           call netcdf_err(error, 'inquire dimension lon length '//
     &       'from file '//trim(INPUTOROG) )
           error=nf_inq_dimid(ncid, 'lat', id_dim)
           call netcdf_err(error, 'inquire dimension lat from file '//
     &                   trim(INPUTOROG) )
           error=nf_inq_dimlen(ncid,id_dim,ny_in)
           call netcdf_err(error, 'inquire dimension lat length '//
     &       'from file '//trim(INPUTOROG) )
           
           print*, "extrapolate OA, OL from Gaussian grid with nx=",
     &              nx_in, ", ny=", ny_in
           allocate(oa_in(nx_in,ny_in,4), ol_in(nx_in,ny_in,4))
           allocate(slm_in(nx_in,ny_in) )
           allocate(lon_in(nx_in,ny_in), lat_in(nx_in,ny_in) )
           
           error=nf_inq_varid(ncid, 'oa1', id_var)
           call netcdf_err(error, 'inquire varid of oa1 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, oa_in(:,:,1))
           call netcdf_err(error, 'inquire data of oa1 from file '
     &                   //trim(INPUTOROG) )
           error=nf_inq_varid(ncid, 'oa2', id_var)
           call netcdf_err(error, 'inquire varid of oa2 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, oa_in(:,:,2))
           call netcdf_err(error, 'inquire data of oa2 from file '
     &                   //trim(INPUTOROG) )
           error=nf_inq_varid(ncid, 'oa3', id_var)
           call netcdf_err(error, 'inquire varid of oa3 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, oa_in(:,:,3))
           call netcdf_err(error, 'inquire data of oa3 from file '
     &                   //trim(INPUTOROG) )           
           error=nf_inq_varid(ncid, 'oa4', id_var)
           call netcdf_err(error, 'inquire varid of oa4 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, oa_in(:,:,4))
           call netcdf_err(error, 'inquire data of oa4 from file '
     &                   //trim(INPUTOROG) )

           error=nf_inq_varid(ncid, 'ol1', id_var)
           call netcdf_err(error, 'inquire varid of ol1 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, ol_in(:,:,1))
           call netcdf_err(error, 'inquire data of ol1 from file '
     &                   //trim(INPUTOROG) )
           error=nf_inq_varid(ncid, 'ol2', id_var)
           call netcdf_err(error, 'inquire varid of ol2 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, ol_in(:,:,2))
           call netcdf_err(error, 'inquire data of ol2 from file '
     &                   //trim(INPUTOROG) )
           error=nf_inq_varid(ncid, 'ol3', id_var)
           call netcdf_err(error, 'inquire varid of ol3 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, ol_in(:,:,3))
           call netcdf_err(error, 'inquire data of ol3 from file '
     &                   //trim(INPUTOROG) )           
           error=nf_inq_varid(ncid, 'ol4', id_var)
           call netcdf_err(error, 'inquire varid of ol4 from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, ol_in(:,:,4))
           call netcdf_err(error, 'inquire data of ol4 from file '
     &                   //trim(INPUTOROG) )

           error=nf_inq_varid(ncid, 'slmsk', id_var)
           call netcdf_err(error, 'inquire varid of slmsk from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, slm_in)
           call netcdf_err(error, 'inquire data of slmsk from file '
     &                   //trim(INPUTOROG) )

           error=nf_inq_varid(ncid, 'geolon', id_var)
           call netcdf_err(error, 'inquire varid of geolon from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, lon_in)
           call netcdf_err(error, 'inquire data of geolon from file '
     &                   //trim(INPUTOROG) )

           error=nf_inq_varid(ncid, 'geolat', id_var)
           call netcdf_err(error, 'inquire varid of geolat from file '
     &                   //trim(INPUTOROG) )
           error=nf_get_var_double(ncid, id_var, lat_in)
           call netcdf_err(error, 'inquire data of geolat from file '
     &                   //trim(INPUTOROG) )
           
           ! set slmsk=2 to be ocean (0)
           do j=1,ny_in; do i=1,nx_in
              if(slm_in(i,j) == 2) slm_in(i,j) = 0
           enddo; enddo
           
           error=nf_close(ncid)
           call netcdf_err(error, 'close file '
     &                   //trim(INPUTOROG) )
           
           print*, "calling MAKEOA3 to compute OA, OL"
           CALL MAKEOA3(ZAVG,zslm,VAR,GLAT,OA,OL,IWORK,ELVMAX,ORO,SLM,
     1            WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,
     2            IM,JM,IMN,JMN,geolon_c,geolat_c,
     3            geolon,geolat,is_south_pole,is_north_pole,nx_in,ny_in,
     4            oa_in,ol_in,slm_in,lon_in,lat_in)
         endif  
       else
         CALL MAKEOA(ZAVG,VAR,GLAT,OA,OL,IWORK,ELVMAX,ORO,
     1            WORK1,WORK2,WORK3,WORK4,
     2            WORK5,WORK6,
     3            IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
       endif
       tbeg=timef()
       call minmxj(IM,JM,OA,'      OA')
       call minmxj(IM,JM,OL,'      OL')
       call minmxj(IM,JM,ELVMAX,'  ELVMAX')
       call minmxj(IM,JM,ORO,'     ORO')
C --- check for nands in above
!      call nanc(OA(1,1,1), IM*JM,"MAKEOA_OA(1,1,1)")
!      call nanc(OA(1,1,2), IM*JM,"MAKEOA_OA(1,1,2)")
!      call nanc(OA(1,1,3), IM*JM,"MAKEOA_OA(1,1,3)")
!      call nanc(OA(1,1,4), IM*JM,"MAKEOA_OA(1,1,4)")
!      call nanc(OL(1,1,1), IM*JM,"MAKEOA_OL(1,1,1)")
!      call nanc(OL(1,1,2), IM*JM,"MAKEOA_OL(1,1,2)")
!      call nanc(OL(1,1,3), IM*JM,"MAKEOA_OL(1,1,3)")
!      call nanc(OL(1,1,4), IM*JM,"MAKEOA_OL(1,1,4)")
!      call nanc(ELVMAX, IM*JM,"MAKEPC_ELVMAX")

        maxc3 = 0
        maxc4 = 0
        maxc5 = 0
        maxc6 = 0
        maxc7 = 0
        maxc8 = 0
      DO J = 1,JM
      DO I = 1,numi(j)
         if (ELVMAX(I,J) .gt. 3000.) maxc3 = maxc3 +1
         if (ELVMAX(I,J) .gt. 4000.) maxc4 = maxc4 +1
         if (ELVMAX(I,J) .gt. 5000.) maxc5 = maxc5 +1
         if (ELVMAX(I,J) .gt. 6000.) maxc6 = maxc6 +1
         if (ELVMAX(I,J) .gt. 7000.) maxc7 = maxc7 +1
         if (ELVMAX(I,J) .gt. 8000.) maxc8 = maxc8 +1
      ENDDO
      ENDDO
      print *,' MAXC3:',maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
!
c      itest=151
c      jtest=56
C      
       print *,' ===> Replacing ELVMAX with ELVMAX-ORO <=== ' 
       print *,' ===> if ELVMAX<=ORO replace with proxy <=== ' 
       print *,' ===> the sum of mean orog (ORO) and std dev <=== ' 
      DO J = 1,JM
      DO I = 1,numi(j)
        if (ELVMAX(I,J) .lt. ORO(I,J) ) then
C---  subtracting off ORO leaves std dev (this should never happen)
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
         if (ELVMAX(I,J) .gt. 3000.) maxc3 = maxc3 +1
         if (ELVMAX(I,J) .gt. 4000.) maxc4 = maxc4 +1
         if (ELVMAX(I,J) .gt. 5000.) maxc5 = maxc5 +1
         if (ELVMAX(I,J) .gt. 6000.) maxc6 = maxc6 +1
         if (ELVMAX(I,J) .gt. 7000.) maxc7 = maxc7 +1
         if (ELVMAX(I,J) .gt. 8000.) maxc8 = maxc8 +1
      ENDDO
      ENDDO
      print *,' after MAXC 3-6 km:',maxc3,maxc4,maxc5,maxc6
c
       call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
!     if (JM .gt. 0) stop
C
C     ZERO OVER OCEAN
C
      print *,' Testing at point (itest,jtest)=',itest,jtest
      print *,' SLM(itest,jtest)=',slm(itest,jtest),itest,jtest
      print *,' ORO(itest,jtest)=',oro(itest,jtest),itest,jtest
      DO J = 1,JM
        DO I = 1,numi(j)
          IF(SLM(I,J).EQ.0.) THEN
C           VAR(I,J) = 0.
            VAR4(I,J) = 0.
            OA(I,J,1) = 0.
            OA(I,J,2) = 0.
            OA(I,J,3) = 0.
            OA(I,J,4) = 0.
            OL(I,J,1) = 0.
            OL(I,J,2) = 0.
            OL(I,J,3) = 0.
            OL(I,J,4) = 0.
C           THETA(I,J) =0.
C           GAMMA(I,J) =0.
C           SIGMA(I,J) =0.
C           ELVMAX(I,J)=0.
! --- the sub-grid scale parameters for mtn blocking and gwd retain 
! --- properties even if over ocean but there is elevation within the 
! --- gaussian grid box.
          ENDIF
       ENDDO
      ENDDO
C
! --- if mskocn=1 ocean land sea mask given, =0 if not present
! ---  OCLSM is real(*4)  array with fractional values possible
! ---  0 is ocean and 1 is land for slm
! ---  Step 1: Only change SLM after GFS SLM is applied
! ---  SLM is only field that will be altered by OCLSM
! ---  Ocean land sea mask ocean points made ocean in atm model
! ---  Land and Lakes and all other atm elv moments remain unchanged.  
            if ( mskocn .eq. 1 ) then

      DO j = 1,jm
        DO i = 1,numi(j)
              if (abs (oro(i,j)) .lt. 1. ) then 
              slm(i,j) = slmi(i,j)
              else
      if ( slmi(i,j) .eq. 1. .and. slm(i,j) .eq. 1) slm(i,j) = 1
      if ( slmi(i,j) .eq. 0. .and. slm(i,j) .eq. 0) slm(i,j) = 0
      if ( slmi(i,j) .eq. 0. .and. slm(i,j) .eq. 1) slm(i,j) = 0
      if ( slmi(i,j) .eq. 0. .and. slm(i,j) .eq. 0) slm(i,j) = 0
              endif
        enddo
      enddo
            endif
      print *,' SLM(itest,jtest)=',slm(itest,jtest),itest,jtest
      print *,' ORO(itest,jtest)=',oro(itest,jtest),itest,jtest

C  REMOVE ISOLATED POINTS
      DO J=2,JM-1
        JN=J-1
        JS=J+1
        RN=REAL(NUMI(JN))/REAL(NUMI(J))
        RS=REAL(NUMI(JS))/REAL(NUMI(J))
        DO I=1,NUMI(J)
          IW=MOD(I+IM-2,IM)+1
          IE=MOD(I,IM)+1
          SLMA=SLM(IW,J)+SLM(IE,J)
          OROA=ORO(IW,J)+ORO(IE,J)
          VARA=VAR(IW,J)+VAR(IE,J)
          VAR4A=VAR4(IW,J)+VAR4(IE,J)
          DO K=1,4
            OAA(K)=OA(IW,J,K)+OA(IE,J,K)
! --- (*j*) fix typo:
            OLA(K)=OL(IW,J,K)+OL(IE,J,K)
          ENDDO
          WGTA=2
          XN=RN*(I-1)+1
          IF(ABS(XN-NINT(XN)).LT.1.E-2) THEN
            IN=MOD(NINT(XN)-1,NUMI(JN))+1
            INW=MOD(IN+NUMI(JN)-2,NUMI(JN))+1
            INE=MOD(IN,NUMI(JN))+1
            SLMA=SLMA+SLM(INW,JN)+SLM(IN,JN)+SLM(INE,JN)
            OROA=OROA+ORO(INW,JN)+ORO(IN,JN)+ORO(INE,JN)
            VARA=VARA+VAR(INW,JN)+VAR(IN,JN)+VAR(INE,JN)
            VAR4A=VAR4A+VAR4(INW,JN)+VAR4(IN,JN)+VAR4(INE,JN)
            DO K=1,4
              OAA(K)=OAA(K)+OA(INW,JN,K)+OA(IN,JN,K)+OA(INE,JN,K)
              OLA(K)=OLA(K)+OL(INW,JN,K)+OL(IN,JN,K)+OL(INE,JN,K)
            ENDDO
            WGTA=WGTA+3
          ELSE
            INW=INT(XN)
            INE=MOD(INW,NUMI(JN))+1
            SLMA=SLMA+SLM(INW,JN)+SLM(INE,JN)
            OROA=OROA+ORO(INW,JN)+ORO(INE,JN)
            VARA=VARA+VAR(INW,JN)+VAR(INE,JN)
            VAR4A=VAR4A+VAR4(INW,JN)+VAR4(INE,JN)
            DO K=1,4
              OAA(K)=OAA(K)+OA(INW,JN,K)+OA(INE,JN,K)
              OLA(K)=OLA(K)+OL(INW,JN,K)+OL(INE,JN,K)
            ENDDO
            WGTA=WGTA+2
          ENDIF
          XS=RS*(I-1)+1
          IF(ABS(XS-NINT(XS)).LT.1.E-2) THEN
            IS=MOD(NINT(XS)-1,NUMI(JS))+1
            ISW=MOD(IS+NUMI(JS)-2,NUMI(JS))+1
            ISE=MOD(IS,NUMI(JS))+1
            SLMA=SLMA+SLM(ISW,JS)+SLM(IS,JS)+SLM(ISE,JS)
            OROA=OROA+ORO(ISW,JS)+ORO(IS,JS)+ORO(ISE,JS)
            VARA=VARA+VAR(ISW,JS)+VAR(IS,JS)+VAR(ISE,JS)
            VAR4A=VAR4A+VAR4(ISW,JS)+VAR4(IS,JS)+VAR4(ISE,JS)
            DO K=1,4
              OAA(K)=OAA(K)+OA(ISW,JS,K)+OA(IS,JS,K)+OA(ISE,JS,K)
              OLA(K)=OLA(K)+OL(ISW,JS,K)+OL(IS,JS,K)+OL(ISE,JS,K)
            ENDDO
            WGTA=WGTA+3
          ELSE
            ISW=INT(XS)
            ISE=MOD(ISW,NUMI(JS))+1
            SLMA=SLMA+SLM(ISW,JS)+SLM(ISE,JS)
            OROA=OROA+ORO(ISW,JS)+ORO(ISE,JS)
            VARA=VARA+VAR(ISW,JS)+VAR(ISE,JS)
            VAR4A=VAR4A+VAR4(ISW,JS)+VAR4(ISE,JS)
            DO K=1,4
              OAA(K)=OAA(K)+OA(ISW,JS,K)+OA(ISE,JS,K)
              OLA(K)=OLA(K)+OL(ISW,JS,K)+OL(ISE,JS,K)
            ENDDO
            WGTA=WGTA+2
          ENDIF
          OROA=OROA/WGTA
          VARA=VARA/WGTA
          VAR4A=VAR4A/WGTA
          DO K=1,4
            OAA(K)=OAA(K)/WGTA
            OLA(K)=OLA(K)/WGTA
          ENDDO
          IF(SLM(I,J).EQ.0..AND.SLMA.EQ.WGTA) THEN
            PRINT '("SEA ",2F8.0," MODIFIED TO LAND",2F8.0," AT ",2I8)',
     &       ORO(I,J),VAR(I,J),OROA,VARA,I,J
            SLM(I,J)=1.
            ORO(I,J)=OROA
            VAR(I,J)=VARA
            VAR4(I,J)=VAR4A
            DO K=1,4
              OA(I,J,K)=OAA(K)
              OL(I,J,K)=OLA(K)
            ENDDO
          ELSEIF(SLM(I,J).EQ.1..AND.SLMA.EQ.0.) THEN
            PRINT '("LAND",2F8.0," MODIFIED TO SEA ",2F8.0," AT ",2I8)',
     &       ORO(I,J),VAR(I,J),OROA,VARA,I,J
            SLM(I,J)=0.
            ORO(I,J)=OROA
            VAR(I,J)=VARA
            VAR4(I,J)=VAR4A
            DO K=1,4
              OA(I,J,K)=OAA(K)
              OL(I,J,K)=OLA(K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C--- print for testing after isolated points removed
      print *,' after isolated points removed'
       call minmxj(IM,JM,ORO,'     ORO')
C     print *,' JM=',JM,' numi=',numi
      print *,' ORO(itest,jtest)=',oro(itest,jtest)
      print *,' VAR(itest,jtest)=',var(itest,jtest)
      print *,' VAR4(itest,jtest)=',var4(itest,jtest)
      print *,' OA(itest,jtest,1)=',oa(itest,jtest,1)
      print *,' OA(itest,jtest,2)=',oa(itest,jtest,2)
      print *,' OA(itest,jtest,3)=',oa(itest,jtest,3)
      print *,' OA(itest,jtest,4)=',oa(itest,jtest,4)
      print *,' OL(itest,jtest,1)=',ol(itest,jtest,1)
      print *,' OL(itest,jtest,2)=',ol(itest,jtest,2)
      print *,' OL(itest,jtest,3)=',ol(itest,jtest,3)
      print *,' OL(itest,jtest,4)=',ol(itest,jtest,4)
      print *,' Testing at point (itest,jtest)=',itest,jtest
      print *,' THETA(itest,jtest)=',theta(itest,jtest)
      print *,' GAMMA(itest,jtest)=',GAMMA(itest,jtest)
      print *,' SIGMA(itest,jtest)=',SIGMA(itest,jtest)
      print *,' ELVMAX(itest,jtest)=',ELVMAX(itest,jtest)
      print *,' EFAC=',EFAC
C
      DO J=1,JM
        DO I=1,numi(j)
          ORO(I,J) = ORO(I,J) + EFAC*VAR(I,J)
          HPRIME(I,J,1) = VAR(I,J)
          HPRIME(I,J,2) = VAR4(I,J)
          HPRIME(I,J,3) = oa(I,J,1)
          HPRIME(I,J,4) = oa(I,J,2)
          HPRIME(I,J,5) = oa(I,J,3)
          HPRIME(I,J,6) = oa(I,J,4)
          HPRIME(I,J,7) = ol(I,J,1)
          HPRIME(I,J,8) = ol(I,J,2)
          HPRIME(I,J,9) = ol(I,J,3)
          HPRIME(I,J,10)= ol(I,J,4)
          HPRIME(I,J,11)= THETA(I,J)
          HPRIME(I,J,12)= GAMMA(I,J)
          HPRIME(I,J,13)= SIGMA(I,J)
          HPRIME(I,J,14)= ELVMAX(I,J)
        ENDDO
      ENDDO
!
       call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
! --- Quadratic filter applied by default.
! --- NF0 is normally set to an even value beyond the previous truncation, 
! --- for example, for jcap=382, NF0=254+2
! --- NF1 is set as jcap+2 (and/or nearest even), eg., for t382, NF1=382+2=384
! --- if no filter is desired then NF1=NF0=0 and ORF=ORO
! --- if no filter but spectral to grid (with gibbs) then NF1=jcap+2, and NF1=jcap+1
!
      IF ( NF1 - NF0 .eq. 0 ) FILTER=.FALSE.
      print *,' NF1, NF0, FILTER=',NF1,NF0,FILTER
      IF (FILTER) THEN
C       SPECTRALLY TRUNCATE AND FILTER OROGRAPHY
        do j=1,jm
          if(numi(j).lt.im) then
            ffj=cmplx(0.,0.)
            call spfft1(numi(j),im/2+1,numi(j),1,ffj,oro(1,j),-1)
            call spfft1(im,im/2+1,im,1,ffj,oro(1,j),+1)
          endif
        enddo
        CALL SPTEZ(NR,NM,4,IM,JM,ORS,ORO,-1)
!
      print *,' about to apply spectral filter '
        FFF=1./(NF1-NF0)**2
        I=0
        DO M=0,NM
        DO N=M,NM+NR*M
          IF(N.GT.NF0) THEN
            WWW=MAX(1.-FFF*(N-NF0)**2,0.)
            ORS(I+1)=ORS(I+1)*WWW
            ORS(I+2)=ORS(I+2)*WWW
          ENDIF
        I=I+2
        ENDDO
        ENDDO
!
        CALL SPTEZ(NR,NM,4,IM,JM,ORS,ORF,+1)
        do j=1,jm
          if(numi(j).lt.im) then
            call spfft1(im,im/2+1,im,1,ffj,orf(1,j),-1)
            call spfft1(numi(j),im/2+1,numi(j),1,ffj,orf(1,j),+1)
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
        ORS=0.
        ORF=ORO
      ENDIF
       call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
      print *,' ELVMAX(',itest,jtest,')=',ELVMAX(itest,jtest)
      print *,' after spectral filter is applied'
       call minmxj(IM,JM,ORO,'     ORO')
       call minmxj(IM,JM,ORF,'     ORF')
C
C  USE NEAREST NEIGHBOR INTERPOLATION TO FILL FULL GRIDS
      call rg2gg(im,jm,numi,slm)
      call rg2gg(im,jm,numi,oro)
      call rg2gg(im,jm,numi,orf)
C ---   not apply to new prin coord and ELVMAX (*j*)
      do imt=1,10 
        call rg2gg(im,jm,numi,hprime(1,1,imt))
      enddo
C 
      print *,' after nearest neighbor interpolation applied '
       call minmxj(IM,JM,ORO,'     ORO')
       call minmxj(IM,JM,ORF,'     ORF')
       call mnmxja(IM,JM,ELVMAX,itest,jtest,'  ELVMAX')
       print *,' ORO,ORF(itest,jtest),itest,jtest:',
     &          ORO(itest,jtest),ORF(itest,jtest),itest,jtest
      print *,' ELVMAX(',itest,jtest,')=',ELVMAX(itest,jtest)


C   check antarctic pole 
      DO J = 1,JM
      DO I = 1,numi(j)
         if ( i .le. 21 .and. i .ge. 1 )then
         if (j .eq. JM )write(6,153)i,j,ORO(i,j),ELVMAX(i,j),SLM(i,j)
  153 format(1x,' ORO,ELVMAX(i=',i4,' j=',i4,')=',2E14.5,f5.1)
         endif
      ENDDO
      ENDDO
      tend=timef()
      write(6,*)' Timer 5 time= ',tend-tbeg
      if (output_binary) then
      tbeg=timef()
C       OUTPUT BINARY FIELDS
        print *,' OUTPUT BINARY FIELDS'
        WRITE(51) REAL(SLM,4)
        WRITE(52) REAL(ORF,4)
        WRITE(53) REAL(HPRIME,4)
        WRITE(54) REAL(ORS,4)
        WRITE(55) REAL(ORO,4)
        WRITE(66) REAL(THETA,4)
        WRITE(67) REAL(GAMMA,4)
        WRITE(68) REAL(SIGMA,4)
! --- OCLSM is real(4) write only if ocean mask is present
            if ( mskocn .eq. 1 ) then
        ios=0
         WRITE(27,iostat=ios) OCLSM
         print *,' write OCLSM input:',ios
!      print *,' LSM:',OCLSM(1,1),OCLSM(50,50),OCLSM(75,75),OCLSM(IM,JM)
            endif
C
       call minmxj(IM,JM,ORO,'     ORO')
      print *,' IM=',IM,' JM=',JM,' SPECTR=',SPECTR
C---    Test binary file output:
      WRITE(71) REAL(SLM,4)
      DO IMT=1,NMT
        WRITE(71) REAL(HPRIME(:,:,IMT),4)
        print *,' HPRIME(',itest,jtest,imt,')=',HPRIME(itest,jtest,imt)
      ENDDO
      WRITE(71) REAL(ORO,4)
      IF (SPECTR) THEN
        WRITE(71) REAL(ORF,4)   ! smoothed spectral orography!
      ENDIF
C  OUTPUT GRIB FIELDS
      KPDS=0
      KPDS(1)=7
      KPDS(2)=78
      KPDS(3)=255
      KPDS(4)=128
      KPDS(5)=81
      KPDS(6)=1
      kpds(8)=2004
      KPDS(9)=1
      KPDS(10)=1
      KPDS(13)=4
      KPDS(15)=1
      KPDS(16)=51
      KPDS(17)=1
      KPDS(18)=1
      KPDS(19)=1
      KPDS(21)=20
      KPDS(22)=0
      KGDS=0
      KGDS(1)=4
      KGDS(2)=IM
      KGDS(3)=JM
      KGDS(4)=90000-180000/PI*RCLT(1)
      KGDS(6)=128
      KGDS(7)=180000/PI*RCLT(1)-90000
      KGDS(8)=-NINT(360000./IM)
      KGDS(9)=NINT(360000./IM)
      KGDS(10)=JM/2
      KGDS(20)=255
! --- SLM
      CALL BAOPEN(56,'fort.56',IRET)
      if (iret .ne. 0) print *,' BAOPEN ERROR UNIT 56: IRET=',IRET
      CALL PUTGB(56,IM*JM,KPDS,KGDS,LB,SLM,IRET)
      print *,' SLM: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
      if (iret .ne. 0) print *,' SLM PUTGB ERROR:  UNIT 56: IRET=',IRET
      print *,' SLM: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
! --- OCLSM if present
!           if ( mskocn .eq. 1 ) then
!     CALL BAOPEN(27,'fort.27',IRET)
!     if (iret .ne. 0) print *,' OCLSM BAOPEN ERROR UNIT 27:IRET=',IRET
!     CALL PUTGB(27,IM*JM,KPDS,KGDS,LB,OCLSM,IRET)
!     if (iret .ne. 0) print *,' OCLSM PUTGB ERROR: UNIT 27:IRET=',IRET
!     print *,' OCLSM: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
!           endif

      KPDS(5)=8
      IF (SPECTR) THEN
        CALL BAOPEN(57,'fort.57',IRET)
        CALL PUTGB(57,IM*JM,KPDS,KGDS,LB,ORF,IRET)
      print *,' ORF (ORO): putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
      ENDIF
C
C ===  write out theta (angle of land to East) using #101 (wave dir)
C ===  [radians] and since < 1 scale adjust kpds(22)
C
      KPDS(5)=101
        CALL BAOPEN(58,'fort.58',IRET)
        CALL PUTGB(58,IM*JM,KPDS,KGDS,LB,THETA,IRET)
      print *,' THETA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
C
C ===  write out (land aspect ratio or anisotropy)  using #102 
C ===  (as in wind wave hgt)
C
      KPDS(22)=2
      KPDS(5)=102
        CALL BAOPEN(60,'fort.60',IRET)
        CALL PUTGB(60,IM*JM,KPDS,KGDS,LB,SIGMA,IRET)
      print *,' SIGMA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
C
C ===  write out (slope parameter sigma)  using #9 
C ===  (as in std hgt)
C
      KPDS(22)=1
      KPDS(5)=103
        CALL BAOPEN(59,'fort.59',IRET)
        CALL PUTGB(59,IM*JM,KPDS,KGDS,LB,GAMMA,IRET)
      print *,' GAMMA: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
C
      KPDS(22)=1
      KPDS(5)=9
        CALL BAOPEN(61,'fort.61',IRET)
        CALL PUTGB(61,IM*JM,KPDS,KGDS,LB,HPRIME,IRET)
      print *,' HPRIME: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
C
C
      KPDS(22)=0
      KPDS(5)=8
        CALL BAOPEN(62,'fort.62',IRET)
        CALL PUTGB(62,IM*JM,KPDS,KGDS,LB,ELVMAX,IRET)
      print *,' ELVMAX: putgb-KPDS(22,5),iret:',KPDS(22),KPDS(5),IRET
      endif ! output_binary
C
      DELXN = 360./IM
      do i=1,im
        xlon(i) = DELXN*(i-1)
      enddo
      IF(trim(OUTGRID) == "none") THEN
        do j=1,jm
         do i=1,im
           geolon(i,j) = xlon(i)
           geolat(i,j) = xlat(j)
         enddo
        enddo
      else
        do j = 1, jm
           xlat(j) = geolat(1,j)
        enddo
        do i = 1, im
           xlon(i) = geolon(i,1)
        enddo
      endif
      tend=timef()
      write(6,*)' Binary output time= ',tend-tbeg
      tbeg=timef()
      CALL WRITE_NETCDF(IM,JM,SLM,land_frac,ORO,ORF,HPRIME,1,1,
     1                  GEOLON(1:IM,1:JM),GEOLAT(1:IM,1:JM), XLON,XLAT)
      tend=timef()
      write(6,*)' WRITE_NETCDF time= ',tend-tbeg
      print *,' wrote netcdf file out.oro.tile?.nc'

      print *,' ===== Deallocate Arrays and ENDING MTN VAR OROG program'
      deallocate (ZAVG)
      deallocate (ZSLM)
      deallocate (UMD)
      deallocate (GICE)
      tend=timef()
      write(6,*)' Total runtime time= ',tend-tbeg1
      RETURN
      END
      SUBROUTINE MAKEMT(ZAVG,ZSLM,ORO,SLM,VAR,VAR4,
!     SUBROUTINE MAKEMT(ZAVG,ZSLM,ORO,OCLSM,mskocn,SLM,VAR,VAR4,
     1 GLAT,IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
      DIMENSION GLAT(JMN),XLAT(JM)
!     REAL*4 OCLSM
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      DIMENSION ORO(IM,JM),SLM(IM,JM),VAR(IM,JM),VAR4(IM,JM)
      DIMENSION IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM),numi(jm)
      INTEGER mskocn,isave
      LOGICAL FLAG, DEBUG
C==== DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
C
! ---- OCLSM holds the ocean (im,jm) grid
! ---  mskocn=1 Use ocean model sea land mask, OK and present,
! ---  mskocn=0 dont use Ocean model sea land mask, not OK, not present
      print *,' _____ SUBROUTINE MAKEMT '
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      JM1 = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
C  (*j*)  for hard wired zero offset (lambda s =0) for terr05 
      DO J=1,JM
       DO I=1,numi(j)
         IM1 = numi(j) - 1
         DELX  = 360./numi(j)       ! GAUSSIAN GRID RESOLUTION
         FACLON  = DELX / DELXN
         IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5 + 1
         IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5 + 1
!        IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!        IEN(I,j) = FACLON * FLOAT(I)   + 0.0001
C
         IF (IST(I,j) .LE. 0)      IST(I,j) = IST(I,j) + IMN
         IF (IEN(I,j) .LT. IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
!
!          if ( I .lt. 10  .and. J .ge. JM-1 )
!    1   PRINT*,' MAKEMT: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
       ENDDO
!          if ( J .ge. JM-1 ) then
!     print *,' *** FACLON=',FACLON, 'numi(j=',j,')=',numi(j)
!          endif
      ENDDO
      print *,' DELX=',DELX,' DELXN=',DELXN
      DO J=1,JM-1
         FLAG=.TRUE.
         DO J1=1,JMN
            XXLAT = (XLAT(J)+XLAT(J+1))/2.
            IF(FLAG.AND.GLAT(J1).GT.XXLAT) THEN
              JST(J) = J1
              JEN(J+1) = J1 - 1
              FLAG = .FALSE.
            ENDIF
         ENDDO
CX     PRINT*, ' J JST JEN ',J,JST(J),JEN(J),XLAT(J),GLAT(J1)
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2) + (JEN(2)-JST(2)),JMN)      
!      PRINT*, ' JM JST JEN=',JST(JM),JEN(JM),XLAT(JM),GLAT(JMN)
C
C...FIRST, AVERAGED HEIGHT
C
      DO J=1,JM
        DO I=1,numi(j)
            ORO(I,J)  = 0.0
            VAR(I,J)  = 0.0
            VAR4(I,J) = 0.0
            XNSUM = 0.0
            XLAND = 0.0
            XWATR = 0.0
            XL1 = 0.0
            XS1 = 0.0
            XW1 = 0.0
            XW2 = 0.0
            XW4 = 0.0
            DO II1 = 1, IEN(I,J) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
               IF(I1.LE.0.)  I1 = I1 + IMN
               IF(I1.GT.IMN) I1 = I1 - IMN
!        if ( i .le. 10 .and. i .ge. 1 ) then 
!             if (j .eq. JM )
!    &print *,' J,JST,JEN,IST,IEN,I1=',
!    &J,JST(j),JEN(J),IST(I,j),IEN(I,j),I1
!        endif
               DO J1=JST(J),JEN(J)
                  XLAND = XLAND + FLOAT(ZSLM(I1,J1))
                  XWATR = XWATR + FLOAT(1-ZSLM(I1,J1))
                  XNSUM = XNSUM + 1.
                  HEIGHT = FLOAT(ZAVG(I1,J1)) 
C.........
                  IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                  XL1 = XL1 + HEIGHT * FLOAT(ZSLM(I1,J1))
                  XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(I1,J1))
                  XW1 = XW1 + HEIGHT
                  XW2 = XW2 + HEIGHT ** 2
C   check antarctic pole 
!        if ( i .le. 10 .and. i .ge. 1 )then
!           if (j .ge. JM-1 )then
C=== degub testing
!     print *," I,J,I1,J1,XL1,XS1,XW1,XW2:",I,J,I1,J1,XL1,XS1,XW1,XW2
! 153 format(1x,' ORO,ELVMAX(i=',i4,' j=',i4,')=',2E14.5,3f5.1)
!          endif
!        endif
               ENDDO
            ENDDO
            IF(XNSUM.GT.1.) THEN
! --- SLM initialized with OCLSM calc from all land points except ....
! ---  0 is ocean and 1 is land for slm
! ---  Step 1 is to only change SLM after GFS SLM is applied
                 
               SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
               IF(SLM(I,J).NE.0.) THEN
                  ORO(I,J)= XL1 / XLAND
               ELSE
                  ORO(I,J)= XS1 / XWATR
               ENDIF
               VAR(I,J)=SQRT(MAX(XW2/XNSUM-(XW1/XNSUM)**2,0.))
            DO II1 = 1, IEN(I,j) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
                  IF(I1.LE.0.) I1 = I1 + IMN
                  IF(I1.GT.IMN) I1 = I1 - IMN
                  DO J1=JST(J),JEN(J)
                     HEIGHT = FLOAT(ZAVG(I1,J1)) 
                     IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                     XW4 = XW4 + (HEIGHT-ORO(I,J)) ** 4
                  ENDDO
               ENDDO
               IF(VAR(I,J).GT.1.) THEN
!          if ( I .lt. 20  .and. J .ge. JM-19 ) then
!     print *,'I,J,XW4,XNSUM,VAR(I,J)',I,J,XW4,XNSUM,VAR(I,J)
!          endif
                  VAR4(I,J) = MIN(XW4/XNSUM/VAR(I,J) **4,10.)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      WRITE(6,*) "! MAKEMT ORO SLM VAR VAR4 DONE"
C

      RETURN
      END

      SUBROUTINE get_index(IMN,JMN,npts,lonO,latO,DELXN,
     &           jst,jen,ilist,numx)
        implicit none
        integer, intent(in)  :: IMN,JMN
        integer              :: npts
        real,    intent(in)  :: LONO(npts), LATO(npts)
        real,    intent(in)  :: DELXN
        integer, intent(out) :: jst,jen
        integer, intent(out) :: ilist(IMN)
        integer, intent(out) :: numx
        real    minlat,maxlat,minlon,maxlon
        integer i2, ii, ist, ien
        
         minlat = minval(LATO)
         maxlat = maxval(LATO)
         minlon = minval(LONO)
         maxlon = maxval(LONO)
         ist = minlon/DELXN+1
         ien = maxlon/DELXN+1
         jst = (minlat+90)/DELXN+1 
         jen = (maxlat+90)/DELXN 
         !--- add a few points to both ends of j-direction
         jst = jst - 5
         if(jst<1) jst = 1
         jen = jen + 5
         if(jen>JMN) jen = JMN

         !--- when around the pole, just search through all the points.
         if((jst == 1 .OR. jen == JMN) .and. 
     &            (ien-ist+1 > IMN/2) )then
            numx = IMN
            do i2 = 1, IMN
               ilist(i2) = i2
            enddo
         else if( ien-ist+1 > IMN/2 ) then  ! cross longitude = 0
            !--- find the minimum that greater than IMN/2 
            !--- and maximum that less than IMN/2
            ist = 0
            ien = IMN+1
            do i2 = 1, npts
               ii = LONO(i2)/DELXN+1
               if(ii <0 .or. ii>IMN) print*,"ii=",ii,IMN,LONO(i2),DELXN
               if( ii < IMN/2 ) then
                  ist = max(ist,ii)
               else if( ii > IMN/2 ) then
                  ien = min(ien,ii)
               endif
            enddo 
            if(ist<1 .OR. ist>IMN) then
               print*, "ist<1 .or. ist>IMN"
               call ABORT()
            endif
           if(ien<1 .OR. ien>IMN) then
               print*, "iend<1 .or. iend>IMN"
               call ABORT()
            endif

            numx = IMN - ien + 1
            do i2 = 1, numx
               ilist(i2) = ien + (i2-1)           
            enddo
            do i2 = 1, ist
               ilist(numx+i2) = i2
            enddo
            numx = numx+ist
         else
            numx = ien-ist+1
            do i2 = 1, numx
               ilist(i2) = ist + (i2-1)
            enddo
         endif

      END   
      
      SUBROUTINE MAKEMT2(ZAVG,ZSLM,ORO,SLM,land_frac,VAR,VAR4,
     1 GLAT,IM,JM,IMN,JMN,lon_c,lat_c)
      implicit none
      real, parameter :: D2R = 3.14159265358979/180.
      integer, parameter :: MAXSUM=20000000
      real  hgt_1d(MAXSUM)     
      integer IM, JM, IMN, JMN
      real GLAT(JMN), GLON(IMN)
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      real land_frac(IM,JM)
      real ORO(IM,JM),SLM(IM,JM),VAR(IM,JM),VAR4(IM,JM)
      integer IST,IEN,JST, JEN
      real lon_c(IM+1,JM+1), lat_c(IM+1,JM+1)
      INTEGER mskocn,isave
      LOGICAL FLAG, DEBUG
      real    LONO(4),LATO(4),LONI,LATI
      real    HEIGHT
      integer JM1,i,j,nsum,ii,jj,i1,numx,i2
      integer ilist(IMN)
      real    DELXN,XNSUM,XLAND,XWATR,XL1,XS1,XW1,XW2,XW4
!jaa
      real :: xnsum_j,xland_j,xwatr_j
      logical inside_a_polygon
C==== DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
C
! ---- OCLSM holds the ocean (im,jm) grid
! ---  mskocn=1 Use ocean model sea land mask, OK and present,
! ---  mskocn=0 dont use Ocean model sea land mask, not OK, not present
      print *,' _____ SUBROUTINE MAKEMT2 '
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      JM1 = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
      DO I=1,IMN
         GLON(I) = 0. + (I-1) * DELXN + DELXN * 0.5
      ENDDO
 
      land_frac(:,:) = 0.0     
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
C  (*j*)  for hard wired zero offset (lambda s =0) for terr05 
!$omp parallel do
!$omp* private (j,i,xnsum,xland,xwatr,nsum,xl1,xs1,xw1,xw2,xw4,lono,
!$omp*          lato,jst,jen,ilist,numx,jj,i2,ii,loni,lati,height,
!$omp*          hgt_1d)
      DO J=1,JM
!       print*, "J=", J
       DO I=1,IM
         ORO(I,J)  = 0.0
         VAR(I,J)  = 0.0
         VAR4(I,J) = 0.0
         XNSUM = 0.0
         XLAND = 0.0
         XWATR = 0.0
         nsum = 0
         XL1 = 0.0
         XS1 = 0.0
         XW1 = 0.0
         XW2 = 0.0
         XW4 = 0.0
         
         LONO(1) = lon_c(i,j) 
         LONO(2) = lon_c(i+1,j) 
         LONO(3) = lon_c(i+1,j+1) 
         LONO(4) = lon_c(i,j+1) 
         LATO(1) = lat_c(i,j) 
         LATO(2) = lat_c(i+1,j) 
         LATO(3) = lat_c(i+1,j+1) 
         LATO(4) = lat_c(i,j+1) 
         call get_index(IMN,JMN,4,LONO,LATO,DELXN,jst,jen,ilist,numx)
         do jj = jst, jen; do i2 = 1, numx
            ii = ilist(i2)
            LONI = ii*DELXN
            LATI = -90 + jj*DELXN
            if(inside_a_polygon(LONI*D2R,LATI*D2R,4,
     &          LONO*D2R,LATO*D2R))then

               XLAND = XLAND + FLOAT(ZSLM(ii,jj))
               XWATR = XWATR + FLOAT(1-ZSLM(ii,jj))
               XNSUM = XNSUM + 1.
               HEIGHT = FLOAT(ZAVG(ii,jj)) 
               nsum = nsum+1
               if(nsum > MAXSUM) then
                 print*, "nsum is greater than MAXSUM, increase MAXSUM"
                 call ABORT()
               endif
               hgt_1d(nsum) = HEIGHT
               IF(HEIGHT.LT.-990.) HEIGHT = 0.0
               XL1 = XL1 + HEIGHT * FLOAT(ZSLM(ii,jj))
               XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(ii,jj))
               XW1 = XW1 + HEIGHT
               XW2 = XW2 + HEIGHT ** 2
            endif
         enddo ; enddo

         
         IF(XNSUM.GT.1.) THEN
! --- SLM initialized with OCLSM calc from all land points except ....
! ---  0 is ocean and 1 is land for slm
! ---  Step 1 is to only change SLM after GFS SLM is applied
               land_frac(i,j) = XLAND/XNSUM  
               SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
               IF(SLM(I,J).NE.0.) THEN
                  ORO(I,J)= XL1 / XLAND
               ELSE
                  ORO(I,J)= XS1 / XWATR
               ENDIF
               VAR(I,J)=SQRT(MAX(XW2/XNSUM-(XW1/XNSUM)**2,0.))
               do I1 = 1, NSUM
                  XW4 = XW4 + (hgt_1d(I1) - ORO(i,j)) ** 4
               enddo   

               IF(VAR(I,J).GT.1.) THEN
                  VAR4(I,J) = MIN(XW4/XNSUM/VAR(I,J) **4,10.)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
!$omp end parallel do
      WRITE(6,*) "! MAKEMT ORO SLM VAR VAR4 DONE"
C

      RETURN
      END

      
      SUBROUTINE MAKEPC(ZAVG,ZSLM,THETA,GAMMA,SIGMA,
     1           GLAT,IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
C
C===  PC: principal coordinates of each Z avg orog box for L&M
C
      parameter(REARTH=6.3712E+6)
      DIMENSION GLAT(JMN),XLAT(JM),DELTAX(JMN)
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      DIMENSION ORO(IM,JM),SLM(IM,JM),HL(IM,JM),HK(IM,JM)
      DIMENSION HX2(IM,JM),HY2(IM,JM),HXY(IM,JM),HLPRIM(IM,JM)
      DIMENSION THETA(IM,JM),GAMMA(IM,JM),SIGMA2(IM,JM),SIGMA(IM,JM)
      DIMENSION IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM),numi(jm)
      LOGICAL FLAG, DEBUG
C===  DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
C
      PI = 4.0 * ATAN(1.0)
      CERTH = PI * REARTH
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      JM1 = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
      DELTAY =  CERTH / FLOAT(JMN)
      print *, 'MAKEPC: DELTAY=',DELTAY
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
         DELTAX(J) = DELTAY * COSD(GLAT(J))
      ENDDO
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
      DO J=1,JM
      DO I=1,numi(j)
C        IM1 = numi(j) - 1
         DELX  = 360./numi(j)       ! GAUSSIAN GRID RESOLUTION
         FACLON  = DELX / DELXN
         IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5
         IST(I,j) = IST(I,j) + 1
         IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5
C              if (debug) then
C          if ( I .lt. 10 .and. J .lt. 10 )
C    1     PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
C              endif
!        IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!        IEN(I,j) = FACLON * FLOAT(I)   + 0.0001
         IF (IST(I,j) .LE. 0)      IST(I,j) = IST(I,j) + IMN
         IF (IEN(I,j) .LT. IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
               if (debug) then
           if ( I .lt. 10 .and. J .lt. 10 )
     1     PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
               endif
         IF (IEN(I,j) .LT. IST(I,j)) 
     1 print *,' MAKEPC: IEN < IST: I,J,IST(I,J),IEN(I,J)',
     2 I,J,IST(I,J),IEN(I,J)
      ENDDO
      ENDDO
      DO J=1,JM-1
         FLAG=.TRUE.
         DO J1=1,JMN
            XXLAT = (XLAT(J)+XLAT(J+1))/2.
            IF(FLAG.AND.GLAT(J1).GT.XXLAT) THEN
              JST(J) = J1
              JEN(J+1) = J1 - 1
              FLAG = .FALSE.
            ENDIF
         ENDDO
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2) + (JEN(2)-JST(2)),JMN)
               if (debug) then
        PRINT*, ' IST,IEN(1,1-numi(1,JM))',IST(1,1),IEN(1,1), 
     1  IST(numi(JM),JM),IEN(numi(JM),JM), numi(JM)
        PRINT*, ' JST,JEN(1,JM) ',JST(1),JEN(1),JST(JM),JEN(JM) 
                endif
C
C... DERIVITIVE TENSOR OF HEIGHT
C
      DO J=1,JM
        DO I=1,numi(j)
            ORO(I,J)  = 0.0
            HX2(I,J) = 0.0
            HY2(I,J) = 0.0
            HXY(I,J) = 0.0
            XNSUM = 0.0
            XLAND = 0.0
            XWATR = 0.0
            XL1 = 0.0
            XS1 = 0.0
            xfp = 0.0
            yfp = 0.0
            xfpyfp = 0.0
            xfp2 = 0.0
            yfp2 = 0.0
            HL(I,J) = 0.0
            HK(I,J) = 0.0
            HLPRIM(I,J) = 0.0
            THETA(I,J) = 0.0 
            GAMMA(I,J) = 0.
            SIGMA2(I,J) = 0.
            SIGMA(I,J) = 0.
C
            DO II1 = 1, IEN(I,J) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
               IF(I1.LE.0.)  I1 = I1 + IMN
               IF(I1.GT.IMN) I1 = I1 - IMN
C
C===  set the rest of the indexs for ave: 2pt staggered derivitive
C
                i0 = i1 - 1
                if (i1 - 1 .le. 0 )   i0 = i0 + imn
                if (i1 - 1 .gt. imn)  i0 = i0 - imn
C
                ip1 = i1 + 1
                if (i1 + 1 .le. 0 )   ip1 = ip1 + imn
                if (i1 + 1 .gt. imn)  ip1 = ip1 - imn
C
               DO J1=JST(J),JEN(J)
                if (debug) then
                   if ( I1 .eq. IST(I,J) .and. J1 .eq. JST(J) ) 
     1   PRINT*, ' J, J1,IST,JST,DELTAX,GLAT ',
     2             J,J1,IST(I,J),JST(J),DELTAX(J1),GLAT(J1)  
                   if ( I1 .eq. IEN(I,J) .and. J1 .eq. JEN(J) ) 
     1   PRINT*, ' J, J1,IEN,JEN,DELTAX,GLAT ',
     2             J,J1,IEN(I,J),JEN(J),DELTAX(J1),GLAT(J1)  
                endif
                  XLAND = XLAND + FLOAT(ZSLM(I1,J1))
                  XWATR = XWATR + FLOAT(1-ZSLM(I1,J1))
                  XNSUM = XNSUM + 1.
C
                  HEIGHT = FLOAT(ZAVG(I1,J1))
                  hi0 =  float(zavg(i0,j1))
                  hip1 =  float(zavg(ip1,j1))
C
                  IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                  if(hi0 .lt. -990.)  hi0 = 0.0
                  if(hip1 .lt. -990.)  hip1 = 0.0
C........           xfp = xfp + 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
           xfp = 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
          xfp2 = xfp2 + 0.25 * ( ( hip1 - hi0 )/DELTAX(J1) )** 2 
C
! --- not at boundaries
!RAB                 if ( J1 .ne. JST(1)  .and. J1 .ne. JEN(JM) ) then
                 if ( J1 .ne. JST(JM)  .and. J1 .ne. JEN(1) ) then
                  hj0 =  float(zavg(i1,j1-1))
                  hjp1 =  float(zavg(i1,j1+1))
                  if(hj0 .lt. -990.)  hj0 = 0.0
                  if(hjp1 .lt. -990.)  hjp1 = 0.0
C.......          yfp = yfp + 0.5 * ( hjp1 - hj0 ) / DELTAY
                  yfp = 0.5 * ( hjp1 - hj0 ) / DELTAY
                  yfp2 = yfp2 + 0.25 * ( ( hjp1 - hj0 )/DELTAY )**2   
C
C..............elseif ( J1 .eq. JST(J) .or. J1 .eq. JEN(JM) ) then
C ===     the NH pole: NB J1 goes from High at NP to Low toward SP
C
!RAB                 elseif ( J1 .eq. JST(1) ) then
                 elseif ( J1 .eq. JST(JM) ) then
		 ijax = i1 + imn/2 
               if (ijax .le. 0 )   ijax = ijax + imn
               if (ijax .gt. imn)  ijax = ijax - imn
C..... at N pole we stay at the same latitude j1 but cross to opp side
                 hijax = float(zavg(ijax,j1))
                 hi1j1 = float(zavg(i1,j1))
                  if(hijax .lt. -990.)  hijax = 0.0
                  if(hi1j1 .lt. -990.)  hi1j1 = 0.0
C.......        yfp = yfp + 0.5 * ( ( 0.5 * ( hijax + hi1j1) ) - hi1j1 )/DELTAY
        yfp = 0.5 * ( ( 0.5 * ( hijax - hi1j1 ) ) )/DELTAY
        yfp2 = yfp2 + 0.25 * ( ( 0.5 *  ( hijax - hi1j1) ) 
     1                                              / DELTAY )**2
C
C ===     the SH pole: NB J1 goes from High at NP to Low toward SP
C
!RAB                 elseif ( J1 .eq. JEN(JM) ) then
                 elseif ( J1 .eq. JEN(1) ) then
		 ijax = i1 + imn/2 
               if (ijax .le. 0 )   ijax = ijax + imn
               if (ijax .gt. imn)  ijax = ijax - imn
                 hijax = float(zavg(ijax,j1))
                 hi1j1 = float(zavg(i1,j1))
                  if(hijax  .lt. -990.)  hijax = 0.0
                  if(hi1j1  .lt. -990.)  hi1j1 = 0.0
             if ( i1 .lt. 5 )print *,' S.Pole i1,j1 :',i1,j1,hijax,hi1j1
C.....        yfp = yfp + 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
        yfp = 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
        yfp2 = yfp2 + 0.25 * (  (0.5 * (hijax - hi1j1) )
     1                                                 / DELTAY )**2  
                 endif
C
C ===    The above does an average across the pole for the bndry in j.
C23456789012345678901234567890123456789012345678901234567890123456789012......
C
                  xfpyfp = xfpyfp + xfp * yfp
                  XL1 = XL1 + HEIGHT * FLOAT(ZSLM(I1,J1))
                  XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(I1,J1))
C
C === average the HX2, HY2 and HXY
C === This will be done over all land
C
               ENDDO
            ENDDO
C
C ===  HTENSR 
C
           IF(XNSUM.GT.1.) THEN
               SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
               IF(SLM(I,J).NE.0.) THEN
                  ORO(I,J)= XL1 / XLAND
                  HX2(I,J) =  xfp2  / XLAND
                  HY2(I,J) =  yfp2  / XLAND
		  HXY(I,J) =  xfpyfp / XLAND
               ELSE
                  ORO(I,J)= XS1 / XWATR
               ENDIF
C=== degub testing
      if (debug) then
          print *," I,J,i1,j1,HEIGHT:", I,J,i1,j1,HEIGHT,
     1         XLAND,SLM(i,j)
          print *," xfpyfp,xfp2,yfp2:",xfpyfp,xfp2,yfp2
          print *," HX2,HY2,HXY:",HX2(I,J),HY2(I,J),HXY(I,J)
      ENDIF
C
C === make the principal axes, theta, and the degree of anisotropy, 
C === and sigma2, the slope parameter
C
               HK(I,J) = 0.5 * ( HX2(I,J) + HY2(I,J) )
               HL(I,J) = 0.5 * ( HX2(I,J) - HY2(I,J) )
               HLPRIM(I,J) = SQRT(HL(I,J)*HL(I,J) + HXY(I,J)*HXY(I,J))
           IF( HL(I,J).NE. 0. .AND. SLM(I,J) .NE. 0. ) THEN
C
             THETA(I,J) = 0.5 * ATAN2D(HXY(I,J),HL(I,J))
C ===   for testing print out in degrees
C            THETA(I,J) = 0.5 * ATAN2(HXY(I,J),HL(I,J))
            ENDIF
             SIGMA2(I,J) =  ( HK(I,J) + HLPRIM(I,J) )
        if ( SIGMA2(I,J) .GE. 0. ) then 
             SIGMA(I,J) =  SQRT(SIGMA2(I,J) )
             if (sigma2(i,j) .ne. 0. .and. 
     &        HK(I,J) .GE. HLPRIM(I,J) ) 
     1       GAMMA(I,J) = sqrt( (HK(I,J) - HLPRIM(I,J)) / SIGMA2(I,J) )
        else
             SIGMA(I,J)=0.
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
C
      RETURN
      END

      SUBROUTINE MAKEPC2(ZAVG,ZSLM,THETA,GAMMA,SIGMA,
     1           GLAT,IM,JM,IMN,JMN,lon_c,lat_c)
C
C===  PC: principal coordinates of each Z avg orog box for L&M
C
      implicit none
      real, parameter :: REARTH=6.3712E+6
      real, parameter :: D2R = 3.14159265358979/180. 
      integer :: IM,JM,IMN,JMN
      real  :: GLAT(JMN),DELTAX(JMN)
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      real lon_c(IM+1,JM+1), lat_c(IM+1,JM+1)
      real ORO(IM,JM),SLM(IM,JM),HL(IM,JM),HK(IM,JM)
      real HX2(IM,JM),HY2(IM,JM),HXY(IM,JM),HLPRIM(IM,JM)
      real THETA(IM,JM),GAMMA(IM,JM),SIGMA2(IM,JM),SIGMA(IM,JM)
      real PI,CERTH,DELXN,DELTAY,XNSUM,XLAND,XWATR,XL1,XS1
      real xfp,yfp,xfpyfp,xfp2,yfp2,HEIGHT
      real hi0,hip1,hj0,hjp1,hijax,hi1j1
      real LONO(4),LATO(4),LONI,LATI
      integer i,j,i1,j1,i2,jst,jen,numx,i0,ip1,ijax
      integer ilist(IMN)
      logical inside_a_polygon
      LOGICAL FLAG, DEBUG
C===  DATA DEBUG/.TRUE./
      DATA DEBUG/.FALSE./
C
      PI = 4.0 * ATAN(1.0)
      CERTH = PI * REARTH
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
      DELTAY =  CERTH / FLOAT(JMN)
      print *, 'MAKEPC2: DELTAY=',DELTAY
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
         DELTAX(J) = DELTAY * COSD(GLAT(J))
      ENDDO
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C

C... DERIVITIVE TENSOR OF HEIGHT
C
!$omp parallel do
!$omp* private (j,i,xnsum,xland,xwatr,xl1,xs1,xfp,yfp,xfpyfp,
!$omp*          xfp2,yfp2,lono,lato,jst,jen,ilist,numx,j1,i2,i1,
!$omp*          loni,lati,i0,ip1,height,hi0,hip1,hj0,hjp1,ijax,
!$omp*          hijax,hi1j1)
      DO J=1,JM
!        print*, "J=", J
        DO I=1,IM
          ORO(I,J)  = 0.0
          HX2(I,J) = 0.0
          HY2(I,J) = 0.0
          HXY(I,J) = 0.0
          XNSUM = 0.0
          XLAND = 0.0
          XWATR = 0.0
          XL1 = 0.0
            XS1 = 0.0
            xfp = 0.0
            yfp = 0.0
            xfpyfp = 0.0
            xfp2 = 0.0
            yfp2 = 0.0
            HL(I,J) = 0.0
            HK(I,J) = 0.0
            HLPRIM(I,J) = 0.0
            THETA(I,J) = 0.0 
            GAMMA(I,J) = 0.
            SIGMA2(I,J) = 0.
            SIGMA(I,J) = 0.

            LONO(1) = lon_c(i,j) 
            LONO(2) = lon_c(i+1,j) 
            LONO(3) = lon_c(i+1,j+1) 
            LONO(4) = lon_c(i,j+1) 
            LATO(1) = lat_c(i,j) 
            LATO(2) = lat_c(i+1,j) 
            LATO(3) = lat_c(i+1,j+1) 
            LATO(4) = lat_c(i,j+1) 
            call get_index(IMN,JMN,4,LONO,LATO,DELXN,jst,jen,ilist,numx)

            do j1 = jst, jen; do i2 = 1, numx
              i1 = ilist(i2)         
              LONI = i1*DELXN
              LATI = -90 + j1*DELXN
              if(inside_a_polygon(LONI*D2R,LATI*D2R,4,
     &           LONO*D2R,LATO*D2R))then

C===  set the rest of the indexs for ave: 2pt staggered derivitive
C
                i0 = i1 - 1
                if (i1 - 1 .le. 0 )   i0 = i0 + imn
                if (i1 - 1 .gt. imn)  i0 = i0 - imn
C
                ip1 = i1 + 1
                if (i1 + 1 .le. 0 )   ip1 = ip1 + imn
                if (i1 + 1 .gt. imn)  ip1 = ip1 - imn

                  XLAND = XLAND + FLOAT(ZSLM(I1,J1))
                  XWATR = XWATR + FLOAT(1-ZSLM(I1,J1))
                  XNSUM = XNSUM + 1.
C
                  HEIGHT = FLOAT(ZAVG(I1,J1))
                  hi0 =  float(zavg(i0,j1))
                  hip1 =  float(zavg(ip1,j1))
C
                  IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                  if(hi0 .lt. -990.)  hi0 = 0.0
                  if(hip1 .lt. -990.)  hip1 = 0.0
C........           xfp = xfp + 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
                  xfp = 0.5 * ( hip1 - hi0 ) / DELTAX(J1)
                  xfp2 = xfp2 + 0.25 * ( ( hip1 - hi0 )/DELTAX(J1) )** 2 
C
! --- not at boundaries
!RAB                 if ( J1 .ne. JST(1)  .and. J1 .ne. JEN(JM) ) then
                 if ( J1 .ne. 1  .and. J1 .ne. JMN ) then
                  hj0 =  float(zavg(i1,j1-1))
                  hjp1 =  float(zavg(i1,j1+1))
                  if(hj0 .lt. -990.)  hj0 = 0.0
                  if(hjp1 .lt. -990.)  hjp1 = 0.0
C.......          yfp = yfp + 0.5 * ( hjp1 - hj0 ) / DELTAY
                  yfp = 0.5 * ( hjp1 - hj0 ) / DELTAY
                  yfp2 = yfp2 + 0.25 * ( ( hjp1 - hj0 )/DELTAY )**2   
C
C..............elseif ( J1 .eq. JST(J) .or. J1 .eq. JEN(JM) ) then
C ===     the NH pole: NB J1 goes from High at NP to Low toward SP
C
!RAB                 elseif ( J1 .eq. JST(1) ) then
                 elseif ( J1 .eq. 1 ) then
		   ijax = i1 + imn/2 
                   if (ijax .le. 0 )   ijax = ijax + imn
                   if (ijax .gt. imn)  ijax = ijax - imn
C..... at N pole we stay at the same latitude j1 but cross to opp side
                   hijax = float(zavg(ijax,j1))
                   hi1j1 = float(zavg(i1,j1))
                   if(hijax .lt. -990.)  hijax = 0.0
                   if(hi1j1 .lt. -990.)  hi1j1 = 0.0
C.......        yfp = yfp + 0.5 * ( ( 0.5 * ( hijax + hi1j1) ) - hi1j1 )/DELTAY
                   yfp = 0.5 * ( ( 0.5 * ( hijax - hi1j1 ) ) )/DELTAY
                   yfp2 = yfp2 + 0.25 * ( ( 0.5 *  ( hijax - hi1j1) ) 
     1                                              / DELTAY )**2
C
C ===     the SH pole: NB J1 goes from High at NP to Low toward SP
C
!RAB                 elseif ( J1 .eq. JEN(JM) ) then
                 elseif ( J1 .eq. JMN ) then
		  ijax = i1 + imn/2 
                  if (ijax .le. 0 )   ijax = ijax + imn
                  if (ijax .gt. imn)  ijax = ijax - imn
                  hijax = float(zavg(ijax,j1))
                  hi1j1 = float(zavg(i1,j1))
                  if(hijax  .lt. -990.)  hijax = 0.0
                  if(hi1j1  .lt. -990.)  hi1j1 = 0.0
                  if ( i1 .lt. 5 )print *,' S.Pole i1,j1 :',i1,j1,
     &                      hijax,hi1j1
C.....        yfp = yfp + 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
        yfp = 0.5 *  (0.5 * ( hijax - hi1j1) )/DELTAY  
        yfp2 = yfp2 + 0.25 * (  (0.5 * (hijax - hi1j1) )
     1                                                 / DELTAY )**2  
                 endif
C
C ===    The above does an average across the pole for the bndry in j.
C23456789012345678901234567890123456789012345678901234567890123456789012......
C
                  xfpyfp = xfpyfp + xfp * yfp
                  XL1 = XL1 + HEIGHT * FLOAT(ZSLM(I1,J1))
                  XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(I1,J1))
               ENDIF
C
C === average the HX2, HY2 and HXY
C === This will be done over all land
C
               ENDDO
            ENDDO
C
C ===  HTENSR 
C
           IF(XNSUM.GT.1.) THEN
               SLM(I,J) = FLOAT(NINT(XLAND/XNSUM))
               IF(SLM(I,J).NE.0.) THEN
                  ORO(I,J)= XL1 / XLAND
                  HX2(I,J) =  xfp2  / XLAND
                  HY2(I,J) =  yfp2  / XLAND
		  HXY(I,J) =  xfpyfp / XLAND
               ELSE
                  ORO(I,J)= XS1 / XWATR
               ENDIF
C=== degub testing
      if (debug) then
          print *," I,J,i1,j1,HEIGHT:", I,J,i1,j1,HEIGHT,
     1         XLAND,SLM(i,j)
          print *," xfpyfp,xfp2,yfp2:",xfpyfp,xfp2,yfp2
          print *," HX2,HY2,HXY:",HX2(I,J),HY2(I,J),HXY(I,J)
      ENDIF
C
C === make the principal axes, theta, and the degree of anisotropy, 
C === and sigma2, the slope parameter
C
               HK(I,J) = 0.5 * ( HX2(I,J) + HY2(I,J) )
               HL(I,J) = 0.5 * ( HX2(I,J) - HY2(I,J) )
               HLPRIM(I,J) = SQRT(HL(I,J)*HL(I,J) + HXY(I,J)*HXY(I,J))
           IF( HL(I,J).NE. 0. .AND. SLM(I,J) .NE. 0. ) THEN
C
             THETA(I,J) = 0.5 * ATAN2D(HXY(I,J),HL(I,J))
C ===   for testing print out in degrees
C            THETA(I,J) = 0.5 * ATAN2(HXY(I,J),HL(I,J))
            ENDIF
             SIGMA2(I,J) =  ( HK(I,J) + HLPRIM(I,J) )
        if ( SIGMA2(I,J) .GE. 0. ) then 
             SIGMA(I,J) =  SQRT(SIGMA2(I,J) )
             if (sigma2(i,j) .ne. 0. .and. 
     &        HK(I,J) .GE. HLPRIM(I,J) ) 
     1       GAMMA(I,J) = sqrt( (HK(I,J) - HLPRIM(I,J)) / SIGMA2(I,J) )
        else
             SIGMA(I,J)=0.
        endif
           ENDIF
                  if (debug) then
       print *," I,J,THETA,SIGMA,GAMMA,",I,J,THETA(I,J),
     1                                       SIGMA(I,J),GAMMA(I,J)
       print *," HK,HL,HLPRIM:",HK(I,J),HL(I,J),HLPRIM(I,J)
                  endif
        ENDDO
      ENDDO
!$omp end parallel do
      WRITE(6,*) "! MAKE Principal Coord  DONE"
C
      RETURN
      END
      
      SUBROUTINE MAKEOA(ZAVG,VAR,GLAT,OA4,OL,IOA4,ELVMAX,
     1           ORO,oro1,XNSUM,XNSUM1,XNSUM2,XNSUM3,XNSUM4,
     2           IST,IEN,JST,JEN,IM,JM,IMN,JMN,XLAT,numi)
      DIMENSION GLAT(JMN),XLAT(JM)
      INTEGER ZAVG(IMN,JMN)
      DIMENSION ORO(IM,JM),ORO1(IM,JM),ELVMAX(IM,JM),ZMAX(IM,JM)
      DIMENSION OA4(IM,JM,4),IOA4(IM,JM,4)
      DIMENSION IST(IM,jm),IEN(IM,jm),JST(JM),JEN(JM)
      DIMENSION XNSUM(IM,JM),XNSUM1(IM,JM),XNSUM2(IM,JM)
      DIMENSION XNSUM3(IM,JM),XNSUM4(IM,JM)
      DIMENSION VAR(IM,JM),OL(IM,JM,4),numi(jm)
      LOGICAL FLAG
C
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
! --- IM1 = IM - 1 removed (not used in this sub)
      JM1 = JM - 1
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
      print *,' IM=',IM,' JM=',JM,' IMN=',IMN,' JMN=',JMN
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
      DO j=1,jm
      DO I=1,numi(j)
         DELX  = 360./numi(j)       ! GAUSSIAN GRID RESOLUTION
         FACLON  = DELX / DELXN
C ---  minus sign here in IST and IEN as in MAKEMT!
         IST(I,j) = FACLON * FLOAT(I-1) - FACLON * 0.5
         IST(I,j) = IST(I,j) + 1
         IEN(I,j) = FACLON * FLOAT(I) - FACLON * 0.5
!        IST(I,j) = FACLON * FLOAT(I-1) + 1.0001
!        IEN(I,j) = FACLON * FLOAT(I)   + 0.0001
         IF (IST(I,j) .LE. 0)      IST(I,j) = IST(I,j) + IMN
         IF (IEN(I,j) .LT. IST(I,j)) IEN(I,j) = IEN(I,j) + IMN
cx         PRINT*, ' I j IST IEN ',I,j,IST(I,j),IEN(I,j)
           if ( I .lt. 3  .and. J .lt. 3 )
     1PRINT*,' MAKEOA: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
           if ( I .lt. 3  .and. J .ge. JM-1 )
     1PRINT*,' MAKEOA: I j IST IEN ',I,j,IST(I,j),IEN(I,j)
      ENDDO
      ENDDO
      print *,'MAKEOA: DELXN,DELX,FACLON',DELXN,DELX,FACLON
      print *, '  ***** ready to start JST JEN section '
      DO J=1,JM-1
         FLAG=.TRUE.
         DO J1=1,JMN
! --- XXLAT added as in MAKEMT and in next line as well
            XXLAT = (XLAT(J)+XLAT(J+1))/2.
            IF(FLAG.AND.GLAT(J1).GT.XXLAT) THEN
              JST(J) = J1
! ---         JEN(J+1) = J1 - 1
              FLAG = .FALSE.
           if ( J .eq. 1 )
     1PRINT*,' MAKEOA: XX j JST JEN ',j,JST(j),JEN(j)
            ENDIF
         ENDDO
           if ( J .lt. 3 )
     1PRINT*,' MAKEOA: j JST JEN ',j,JST(j),JEN(j)
           if ( J .ge. JM-2 )
     1PRINT*,' MAKEOA: j JST JEN ',j,JST(j),JEN(j)
C        FLAG=.TRUE.
C        DO J1=JST(J),JMN
C           IF(FLAG.AND.GLAT(J1).GT.XLAT(J)) THEN
C             JEN(J) = J1 - 1
C             FLAG = .FALSE.
C           ENDIF
C        ENDDO
      ENDDO
      JST(JM) = MAX(JST(JM-1) - (JEN(JM-1)-JST(JM-1)),1)
      JEN(1)  = MIN(JEN(2) + (JEN(2)-JST(2)),JMN)
      print *,' ***** JST(1) JEN(1) ',JST(1),JEN(1)
      print *,' ***** JST(JM) JEN(JM) ',JST(JM),JEN(JM)
C
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
        DO I=1,numi(j)
            DO II1 = 1, IEN(I,J) - IST(I,J) + 1
               I1 = IST(I,J) + II1 - 1
! --- next line as in makemt (I1 not II1) (*j*) 20070701
             IF(I1.LE.0.)  I1 = I1 + IMN
             IF (I1 .GT. IMN) I1 = I1 - IMN
             DO J1=JST(J),JEN(J)
                HEIGHT = FLOAT(ZAVG(I1,J1))
                IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                IF ( HEIGHT .gt. ORO(I,J) ) then
                   if ( HEIGHT .gt. ZMAX(I,J) )ZMAX(I,J) = HEIGHT
                   XNSUM(I,J) = XNSUM(I,J) + 1
                  ENDIF
             ENDDO
            ENDDO
           if ( I .lt. 5  .and. J .ge. JM-5 ) then
      print *,' I,J,ORO(I,J),XNSUM(I,J),ZMAX(I,J):',
     1         I,J,ORO(I,J),XNSUM(I,J),ZMAX(I,J)
           endif
        ENDDO
      ENDDO
!
C....     make ELVMAX  ORO from MAKEMT sub
C
! ---  this will make work1 array take on oro's values on return
      DO J=1,JM
        DO I=1,numi(j)

          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO
C........
C      The MAX elev peak (no averaging)
C........
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
C
           INCI = NINT((IEN(I,j)-IST(I,j)) * 0.5)
           ISTTT = MIN(MAX(IST(I,j)-INCI,1),IMN)
           IEDDD = MIN(MAX(IEN(I,j)-INCI,1),IMN)
C
           INCJ = NINT((JEN(J)-JST(J)) * 0.5)
           JSTTT = MIN(MAX(JST(J)-INCJ,1),JMN)
           JEDDD = MIN(MAX(JEN(J)-INCJ,1),JMN)
!               if ( J .lt. 3 .or. J .gt. JM-3 )  then
!                 if(I .lt. 3 .or. I .gt. IM-3) then
!        print *,' INCI,ISTTT,IEDDD,INCJ,JSTTT,JEDDD:',
!    1  I,J,INCI,ISTTT,IEDDD,INCJ,JSTTT,JEDDD  
!                 endif
!               endif
C
           DO I1=ISTTT,IEDDD
             DO J1=JSTTT,JEDDD
               IF(FLOAT(ZAVG(I1,J1)) .GT. HC)
     1            XNSUM3(I,J) = XNSUM3(I,J) + 1
               XNSUM4(I,J) = XNSUM4(I,J) + 1
             ENDDO
           ENDDO
cx         print*,' i j hc var ',i,j,hc,var(i,j)
cx         print*,'xnsum12 ',xnsum1(i,j),xnsum2(i,j)
cx         print*,'xnsum34 ',xnsum3(i,j),xnsum4(i,j)
        ENDDO
      ENDDO
C
C---- CALCULATE THE 3D OROGRAPHIC ASYMMETRY FOR 4 WIND DIRECTIONS
C---- AND THE 3D OROGRAPHIC SUBGRID OROGRAPHY FRACTION
C     (KWD = 1  2  3  4)
C     ( WD = W  S SW NW)
C
C
      DO KWD = 1, 4
        DO J=1,JM
          DO I=1,numi(j)
            OA4(I,J,KWD) = 0.0
          ENDDO
        ENDDO
      ENDDO
C
      DO J=1,JM-2
        DO I=1,numi(j)
        II = I + 1
        IF (II .GT. numi(j)) II = II - numi(j)
          XNPU = XNSUM(I,J)    + XNSUM(I,J+1)
          XNPD = XNSUM(II,J)   + XNSUM(II,J+1)
          IF (XNPD .NE. XNPU) OA4(II,J+1,1) = 1. - XNPD / MAX(XNPU , 1.)
          OL(II,J+1,1) = (XNSUM3(I,J+1)+XNSUM3(II,J+1))/
     1                   (XNSUM4(I,J+1)+XNSUM4(II,J+1))
!          if ( I .lt. 20  .and. J .ge. JM-19 ) then
!        PRINT*,' MAKEOA: I J IST IEN ',I,j,IST(I,J),IEN(I,J)
!        PRINT*,' HC VAR ',HC,VAR(i,j)
!        PRINT*,' MAKEOA: XNSUM(I,J)=',XNSUM(I,J),XNPU, XNPD
!        PRINT*,' MAKEOA: XNSUM3(I,J+1),XNSUM3(II,J+1)',
!    1                    XNSUM3(I,J+1),XNSUM3(II,J+1)
!        PRINT*,' MAKEOA: II, OA4(II,J+1,1), OL(II,J+1,1):',
!    1                    II, OA4(II,J+1,1), OL(II,J+1,1)
!          endif
        ENDDO
      ENDDO
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
C
      DO KWD = 1, 4
        DO I=1,numi(j)
          OL(I,1,KWD)  = OL(I,2,KWD)
          OL(I,JM,KWD) = OL(I,JM-1,KWD)
        ENDDO
      ENDDO
C
      DO KWD=1,4
        DO J=1,JM
          DO I=1,numi(j)
            T = OA4(I,J,KWD)
            OA4(I,J,KWD) = SIGN( MIN( ABS(T), 1. ), T )
          ENDDO
        ENDDO
      ENDDO
C
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
C
      WRITE(6,*) "! MAKEOA EXIT"
C
      RETURN
      END


      function get_lon_angle(dx,lat, DEGRAD)
      implicit none
      real dx, lat, DEGRAD
      
      real get_lon_angle
         real, parameter :: RADIUS = 6371000

         get_lon_angle = 2*asin( sin(dx/RADIUS*0.5)/cos(lat) )*DEGRAD
         
      end function get_lon_angle

      function get_lat_angle(dy, DEGRAD)
      implicit none
      real dy, DEGRAD
      
      real get_lat_angle
         real, parameter :: RADIUS = 6371000

         get_lat_angle = dy/RADIUS*DEGRAD
         
      end function get_lat_angle
      
      SUBROUTINE MAKEOA2(ZAVG,zslm,VAR,GLAT,OA4,OL,IOA4,ELVMAX,
     1           ORO,oro1,XNSUM,XNSUM1,XNSUM2,XNSUM3,XNSUM4,
     2           IM,JM,IMN,JMN,lon_c,lat_c,lon_t,lat_t,dx,dy,
     3           is_south_pole,is_north_pole )
      implicit none
      real, parameter :: MISSING_VALUE = -9999.
      real, parameter :: D2R = 3.14159265358979/180.
      real, PARAMETER :: R2D=180./3.14159265358979
      integer IM,JM,IMN,JMN
      real    GLAT(JMN)
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      real    ORO(IM,JM),ORO1(IM,JM),ELVMAX(IM,JM),ZMAX(IM,JM)
      real    OA4(IM,JM,4)
      integer IOA4(IM,JM,4)
      real    lon_c(IM+1,JM+1), lat_c(IM+1,JM+1)
      real    lon_t(IM,JM), lat_t(IM,JM)
      real    dx(IM,JM), dy(IM,JM)
      logical is_south_pole(IM,JM), is_north_pole(IM,JM)
      real    XNSUM(IM,JM),XNSUM1(IM,JM),XNSUM2(IM,JM)
      real    XNSUM3(IM,JM),XNSUM4(IM,JM)
      real    VAR(IM,JM),OL(IM,JM,4)
      LOGICAL FLAG
      integer i,j,ilist(IMN),numx,i1,j1,ii1
      integer KWD,II,npts
      real    LONO(4),LATO(4),LONI,LATI
      real    DELXN,HC,HEIGHT,XNPU,XNPD,T
      integer NS0,NS1,NS2,NS3,NS4,NS5,NS6
      logical inside_a_polygon
      real    lon,lat,dlon,dlat,dlat_old
      real    lon1,lat1,lon2,lat2
      real    xnsum11,xnsum12,xnsum21,xnsum22,xnsumx
      real    HC_11, HC_12, HC_21, HC_22
      real    xnsum1_11,xnsum1_12,xnsum1_21,xnsum1_22
      real    xnsum2_11,xnsum2_12,xnsum2_21,xnsum2_22
      real    get_lon_angle, get_lat_angle, get_xnsum
      integer ist, ien, jst, jen
      real    xland,xwatr,xl1,xs1,oroavg,slm
C   
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
      print *,' IM=',IM,' JM=',JM,' IMN=',IMN,' JMN=',JMN
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
C
      DO J=1,JM
        DO I=1,IM
          XNSUM(I,J) = 0.0
          ELVMAX(I,J) = ORO(I,J)
          ZMAX(I,J)   = 0.0
C---- COUNT NUMBER OF MODE. HIGHER THAN THE HC, CRITICAL HEIGHT
C     IN A GRID BOX
          XNSUM1(I,J) = 0.0
          XNSUM2(I,J) = 0.0
          XNSUM3(I,J) = 0.0
          XNSUM4(I,J) = 0.0
          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO

! --- # of peaks > ZAVG value and ZMAX(IM,JM) -- ORO is already avg.
! ---  to JM or to JM1
!$omp parallel do
!$omp* private (j,i,hc,lono,lato,jst,jen,ilist,numx,j1,ii1,i1,loni,
!$omp*          lati,height)
      DO J=1,JM
!        print*, "J=", J 
        DO I=1,IM
          HC = 1116.2 - 0.878 * VAR(I,J) 
          LONO(1) = lon_c(i,j) 
          LONO(2) = lon_c(i+1,j) 
          LONO(3) = lon_c(i+1,j+1) 
          LONO(4) = lon_c(i,j+1) 
          LATO(1) = lat_c(i,j) 
          LATO(2) = lat_c(i+1,j) 
          LATO(3) = lat_c(i+1,j+1) 
          LATO(4) = lat_c(i,j+1) 
          call get_index(IMN,JMN,4,LONO,LATO,DELXN,jst,jen,ilist,numx)
          do j1 = jst, jen; do ii1 = 1, numx          
            i1 = ilist(ii1)
            LONI = i1*DELXN
            LATI = -90 + j1*DELXN
            if(inside_a_polygon(LONI*D2R,LATI*D2R,4,
     &          LONO*D2R,LATO*D2R))then

              HEIGHT = FLOAT(ZAVG(I1,J1))
              IF(HEIGHT.LT.-990.) HEIGHT = 0.0
              IF ( HEIGHT .gt. ORO(I,J) ) then
                 if ( HEIGHT .gt. ZMAX(I,J) )ZMAX(I,J) = HEIGHT
              ENDIF   
            endif
          ENDDO ; ENDDO
        ENDDO
      ENDDO
!$omp end parallel do      
C
! ---  this will make work1 array take on oro's values on return
! ---  this will make work1 array take on oro's values on return
      DO J=1,JM
        DO I=1,IM

          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO
      
      DO KWD = 1, 4
        DO J=1,JM
          DO I=1,IM
            OA4(I,J,KWD) = 0.0
            OL(I,J,KWD) = 0.0
          ENDDO
        ENDDO
      ENDDO
                                !
! --- # of peaks > ZAVG value and ZMAX(IM,JM) -- ORO is already avg.
C
C---- CALCULATE THE 3D OROGRAPHIC ASYMMETRY FOR 4 WIND DIRECTIONS
C---- AND THE 3D OROGRAPHIC SUBGRID OROGRAPHY FRACTION
C     (KWD = 1  2  3  4)
C     ( WD = W  S SW NW)
C
C
!$omp parallel do
!$omp* private (j,i,lon,lat,kwd,dlon,dlat,lon1,lon2,lat1,lat2,
!$omp*          xnsum11,xnsum12,xnsum21,xnsum22,xnpu,xnpd,
!$omp*          xnsum1_11,xnsum2_11,hc_11, xnsum1_12,xnsum2_12,
!$omp*          hc_12,xnsum1_21,xnsum2_21,hc_21, xnsum1_22,
!$omp*          xnsum2_22,hc_22)
      DO J=1,JM
!       print*, "j = ", j
        DO I=1,IM
          lon = lon_t(i,j)
          lat = lat_t(i,j)
          !--- for around north pole, oa and ol are all 0
          
          if(is_north_pole(i,j)) then
             print*, "set oa1 = 0 and ol=0 at i,j=", i,j
             do kwd = 1, 4
                  oa4(i,j,kwd) = 0.
                  ol(i,j,kwd) = 0.
             enddo
          else if(is_south_pole(i,j)) then
             print*, "set oa1 = 0 and ol=1 at i,j=", i,j
             do kwd = 1, 4
                oa4(i,j,kwd) = 0.
                ol(i,j,kwd) = 1.
             enddo    
          else
             
          !--- for each point, find a lat-lon grid box with same dx and dy as the cubic grid box
          dlon = get_lon_angle(dx(i,j), lat*D2R, R2D  )
          dlat = get_lat_angle(dy(i,j), R2D)
          !--- adjust dlat if the points are close to pole.
          if( lat-dlat*0.5<-90.) then
             print*, "at i,j =", i,j, lat, dlat, lat-dlat*0.5
             print*, "ERROR: lat-dlat*0.5<-90."
             call ERREXIT(4)
          endif
          if( lat+dlat*2 > 90.) then
             dlat_old = dlat
             dlat = (90-lat)*0.5
             print*, "at i,j=",i,j," adjust dlat from ",
     &              dlat_old, " to ", dlat
          endif   
          !--- lower left 
          lon1 = lon-dlon*1.5
          lon2 = lon-dlon*0.5
          lat1 = lat-dlat*0.5
          lat2 = lat+dlat*0.5

          if(lat1<-90 .or. lat2>90) then
             print*, "at upper left i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          xnsum11 = get_xnsum(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn)          

          !--- upper left 
          lon1 = lon-dlon*1.5
          lon2 = lon-dlon*0.5
          lat1 = lat+dlat*0.5
          lat2 = lat+dlat*1.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower left i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          xnsum12 = get_xnsum(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn)          

          !--- lower right
          lon1 = lon-dlon*0.5
          lon2 = lon+dlon*0.5
          lat1 = lat-dlat*0.5
          lat2 = lat+dlat*0.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at upper right i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          xnsum21 = get_xnsum(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn)          

          !--- upper right 
          lon1 = lon-dlon*0.5
          lon2 = lon+dlon*0.5
          lat1 = lat+dlat*0.5
          lat2 = lat+dlat*1.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower right i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          
          xnsum22 = get_xnsum(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn)          
          
           XNPU = xnsum11 + xnsum12
           XNPD = xnsum21 + xnsum22
           IF (XNPD .NE. XNPU) OA4(I,J,1) = 1. - XNPD / MAX(XNPU , 1.)

           XNPU = xnsum11 + xnsum21
           XNPD = xnsum12 + xnsum22
           IF (XNPD .NE. XNPU) OA4(I,J,2) = 1. - XNPD / MAX(XNPU , 1.)

           XNPU = xnsum11 + (xnsum12+xnsum21)*0.5
           XNPD = xnsum22 + (xnsum12+xnsum21)*0.5
           IF (XNPD .NE. XNPU) OA4(I,J,3) = 1. - XNPD / MAX(XNPU , 1.)

           XNPU = xnsum12 + (xnsum11+xnsum22)*0.5
           XNPD = xnsum21 + (xnsum11+xnsum22)*0.5
           IF (XNPD .NE. XNPU) OA4(I,J,4) = 1. - XNPD / MAX(XNPU , 1.)

           
          !--- calculate OL3 and OL4
          !--- lower left 
          lon1 = lon-dlon*1.5
          lon2 = lon-dlon*0.5
          lat1 = lat-dlat*0.5
          lat2 = lat+dlat*0.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at upper left i=,j=", i, j, lat, dlat,lat1,lat2
          endif          
          call get_xnsum2(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_11, xnsum2_11, HC_11)          

          !--- upper left 
          lon1 = lon-dlon*1.5
          lon2 = lon-dlon*0.5
          lat1 = lat+dlat*0.5
          lat2 = lat+dlat*1.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower left i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          call get_xnsum2(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_12, xnsum2_12, HC_12)          

          !--- lower right
          lon1 = lon-dlon*0.5
          lon2 = lon+dlon*0.5
          lat1 = lat-dlat*0.5
          lat2 = lat+dlat*0.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at upper right i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          call get_xnsum2(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_21, xnsum2_21, HC_21)          

          !--- upper right 
          lon1 = lon-dlon*0.5
          lon2 = lon+dlon*0.5
          lat1 = lat+dlat*0.5
          lat2 = lat+dlat*1.5
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower right i=,j=", i, j, lat, dlat,lat1,lat2
          endif          
          call get_xnsum2(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_22, xnsum2_22, HC_22)           
                  
          OL(i,j,3) = (XNSUM1_22+XNSUM1_11)/(XNSUM2_22+XNSUM2_11)
          OL(i,j,4) = (XNSUM1_12+XNSUM1_21)/(XNSUM2_12+XNSUM2_21)

          !--- calculate OL1 and OL2
          !--- lower left 
          lon1 = lon-dlon*2.0
          lon2 = lon-dlon
          lat1 = lat
          lat2 = lat+dlat
          if(lat1<-90 .or. lat2>90) then
             print*, "at upper left i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          call get_xnsum3(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_11, xnsum2_11, HC_11)          

          !--- upper left 
          lon1 = lon-dlon*2.0
          lon2 = lon-dlon
          lat1 = lat+dlat
          lat2 = lat+dlat*2.0
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower left i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          
          call get_xnsum3(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_12, xnsum2_12, HC_12)          

          !--- lower right
          lon1 = lon-dlon
          lon2 = lon
          lat1 = lat
          lat2 = lat+dlat
          if(lat1<-90 .or. lat2>90) then
             print*, "at upper right i=,j=", i, j, lat, dlat,lat1,lat2
          endif          
          call get_xnsum3(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_21, xnsum2_21, HC_21)          

          !--- upper right 
          lon1 = lon-dlon
          lon2 = lon
          lat1 = lat+dlat
          lat2 = lat+dlat*2.0
          if(lat1<-90 .or. lat2>90) then
             print*, "at lower right i=,j=", i, j, lat, dlat,lat1,lat2
          endif
          
          call get_xnsum3(lon1,lat1,lon2,lat2,IMN,JMN,GLAt,
     &     zavg,zslm,delxn, xnsum1_22, xnsum2_22, HC_22)           
                  
          OL(i,j,1) = (XNSUM1_11+XNSUM1_21)/(XNSUM2_11+XNSUM2_21)
          OL(i,j,2) = (XNSUM1_21+XNSUM1_22)/(XNSUM2_21+XNSUM2_22)         
          ENDIF          
        ENDDO
      ENDDO
!$omp end parallel do
      DO KWD=1,4
        DO J=1,JM
          DO I=1,IM
            T = OA4(I,J,KWD)
            OA4(I,J,KWD) = SIGN( MIN( ABS(T), 1. ), T )
          ENDDO
        ENDDO
      ENDDO
C
      NS0 = 0
      NS1 = 0
      NS2 = 0
      NS3 = 0
      NS4 = 0
      NS5 = 0
      NS6 = 0
      DO KWD=1,4
      DO J=1,JM
      DO I=1,IM
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
C
      WRITE(6,*) "! MAKEOA2 EXIT"
C
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE GL2ANY(IP,KM,G1,IM1,JM1,G2,IM2,JM2,IDRTI,RLON,RLAT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GL2GL       INTERPOLATE GAUSSIAN GRID TO GAUSSIAN GRID
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: LINEARLY INTERPOLATES GAUSSIAN GRID TO GAUSSIAN GRID.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL GL2GL(IP,KM,G1,IM1,JM1,G2,IM2,JM2)
C   INPUT ARGUMENT LIST:
C     IP           INTEGER INTERPOLATION TYPE
C     KM           INTEGER NUMBER OF LEVELS
C     G1           REAL (IM1,JM1,KM) INPUT GAUSSIAN FIELD
C     IM1          INTEGER NUMBER OF INPUT LONGITUDES
C     JM1          INTEGER NUMBER OF INPUT LATITUDES
C     IM2          INTEGER NUMBER OF OUTPUT LONGITUDES
C     JM2          INTEGER NUMBER OF OUTPUT LATITUDES
C   OUTPUT ARGUMENT LIST:
C     G2           REAL (IM2,JM2,KM) OUTPUT GAUSSIAN FIELD
C
C SUBPROGRAMS CALLED:
C   IPOLATES     IREDELL'S POLATE FOR SCALAR FIELDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL G1(IM1,JM1,KM),G2(IM2,JM2,KM)
      LOGICAL*1 L1(IM1,JM1,KM),L2(IM2,JM2,KM)
      REAL, intent(in) :: RLAT(IM2,JM2),RLON(IM2,JM2)
      INTEGER IB1(KM),IB2(KM)
      INTEGER KGDS1(200),KGDS2(200)
      INTEGER IDRTI, IDRTO
      DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/
      DATA KGDS2/4,0,0,90000,0,0,-90000,193*0/
      INTEGER IPOPT(20)
      DATA IPOPT/20*0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      KGDS1(1) = IDRTI
      KGDS2(1) = -1
      NO = IM2*JM2
      IF(IM1.NE.IM2.OR.JM1.NE.JM2) THEN
        IB1=0
        KGDS1(2)=IM1
        KGDS1(3)=JM1
        KGDS1(8)=NINT(-360000./IM1)
        KGDS1(10)=JM1/2
        KGDS2(2)=IM2
        KGDS2(3)=JM2
        KGDS2(8)=NINT(-360000./IM2)
        KGDS2(10)=JM2/2
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1,
     &                NO,RLAT,RLON,IB2,L2,G2,IRET)
      ELSE
        G2=G1
      ENDIF
      END


      function spherical_distance(theta1,phi1,theta2,phi2)

      real, intent(in) :: theta1, phi1, theta2, phi2
      real :: spherical_distance, dot

      if(theta1 == theta2 .and. phi1 == phi2) then
        spherical_distance = 0.0
        return
      endif
  
      dot = cos(phi1)*cos(phi2)*cos(theta1-theta2) + sin(phi1)*sin(phi2)
      if(dot > 1. ) dot = 1.
      if(dot < -1.) dot = -1.
      spherical_distance = acos(dot)

      return

      end function spherical_distance
      
      subroutine get_mismatch_index(im_in, jm_in, geolon_in,geolat_in,
     &           bitmap_in,num_out, lon_out,lat_out, iindx, jindx )
      integer, intent(in) :: im_in, jm_in, num_out
      real,    intent(in) :: geolon_in(im_in,jm_in)
      real,    intent(in) :: geolat_in(im_in,jm_in)
      logical*1, intent(in) :: bitmap_in(im_in,jm_in)   
      real,    intent(in) :: lon_out(num_out), lat_out(num_out)
      integer, intent(out):: iindx(num_out), jindx(num_out)
      real, parameter :: MAX_DIST = 1.e+20
      integer, parameter :: NUMNBR = 20
      integer :: i_c,j_c,jstart,jend
      real    :: lon,lat
      
      print*, "im_in,jm_in = ", im_in, jm_in
      print*, "num_out = ", num_out
      print*, "max and min of lon_in is", minval(geolon_in),
     &                                    maxval(geolon_in)
      print*, "max and min of lat_in is", minval(geolat_in),
     &                                    maxval(geolat_in)   
      print*, "max and min of lon_out is", minval(lon_out),
     &                                     maxval(lon_out)
      print*, "max and min of lat_out is", minval(lat_out),
     &                                     maxval(lat_out)   
      print*, "count(bitmap_in)= ", count(bitmap_in), MAX_DIST
      
      do n = 1, num_out
        !      print*, "n = ", n
        lon = lon_out(n)
        lat = lat_out(n)
        !--- find the j-index for the nearest point
        i_c = 0; j_c = 0
        do j = 1, jm_in-1
          if(lat .LE. geolat_in(1,j) .and.
     &       lat .GE. geolat_in(1,j+1)) then
            j_c = j
          endif
        enddo
        if(lat > geolat_in(1,1)) j_c = 1
        if(lat < geolat_in(1,jm_in)) j_c = jm_in
        !      print*, "lat =", lat, geolat_in(1,jm_in), geolat_in(1,jm_in-1)
        ! The input is Gaussian grid.
        jstart = max(j_c-NUMNBR,1)
        jend = min(j_c+NUMNBR,jm_in)
        dist = MAX_DIST
        iindx(n) = 0
        jindx(n) = 0
        !      print*, "jstart, jend =", jstart, jend
        do j = jstart, jend; do i = 1,im_in   
          if(bitmap_in(i,j) ) then
            !            print*, "bitmap_in is true"
            d = spherical_distance(lon_out(n),lat_out(n),
     &                             geolon_in(i,j), geolat_in(i,j))
            if( dist > d ) then
              iindx(n) = i; jindx(n) = j
              dist = d
            endif
          endif
        enddo; enddo
        if(iindx(n) ==0) then
          print*, "lon,lat=", lon,lat
          print*, "jstart, jend=", jstart, jend, dist
          print*, "ERROR in get mismatch_index: not find nearest points"
          call ERREXIT(4)
        endif
      enddo   

      end subroutine get_mismatch_index
      

      subroutine interpolate_mismatch(im_in, jm_in, data_in,
     &                                num_out, data_out, iindx, jindx)
      integer, intent(in) :: im_in, jm_in, num_out
      real,    intent(in) :: data_in(im_in,jm_in)
      real,    intent(out):: data_out(num_out)
      integer, intent(in) :: iindx(num_out), jindx(num_out)
      
      do n = 1, num_out
        data_out(n) = data_in(iindx(n),jindx(n))
      enddo   

      end subroutine interpolate_mismatch
      
      SUBROUTINE MAKEOA3(ZAVG,zslm,VAR,GLAT,OA4,OL,IOA4,ELVMAX,
     1           ORO,SLM,oro1,XNSUM,XNSUM1,XNSUM2,XNSUM3,XNSUM4,
     2           IM,JM,IMN,JMN,lon_c,lat_c,lon_t,lat_t,
     3           is_south_pole,is_north_pole,IMI,JMI,OA_IN,OL_IN,
     4           slm_in,lon_in,lat_in)
      implicit none
      real, parameter :: MISSING_VALUE = -9999.
      real, parameter :: D2R = 3.14159265358979/180.
      real, PARAMETER :: R2D=180./3.14159265358979
      integer IM,JM,IMN,JMN,IMI,JMI
      real    GLAT(JMN)
      INTEGER ZAVG(IMN,JMN),ZSLM(IMN,JMN)
      real    SLM(IM,JM)
      real    ORO(IM,JM),ORO1(IM,JM),ELVMAX(IM,JM),ZMAX(IM,JM)
      real    OA4(IM,JM,4)
      integer IOA4(IM,JM,4)
      real    OA_IN(IMI,JMI,4), OL_IN(IMI,JMI,4)
      real    slm_in(IMI,JMI)
      real    lon_in(IMI,JMI), lat_in(IMI,JMI)
      real    lon_c(IM+1,JM+1), lat_c(IM+1,JM+1)
      real    lon_t(IM,JM), lat_t(IM,JM)
      logical is_south_pole(IM,JM), is_north_pole(IM,JM)
      real    XNSUM(IM,JM),XNSUM1(IM,JM),XNSUM2(IM,JM)
      real    XNSUM3(IM,JM),XNSUM4(IM,JM)
      real    VAR(IM,JM),OL(IM,JM,4)
      LOGICAL FLAG
      integer i,j,ilist(IMN),numx,i1,j1,ii1
      integer KWD,II,npts
      real    LONO(4),LATO(4),LONI,LATI
      real    DELXN,HC,HEIGHT,XNPU,XNPD,T
      integer NS0,NS1,NS2,NS3,NS4,NS5,NS6
      logical inside_a_polygon
      real    lon,lat,dlon,dlat,dlat_old
      real    lon1,lat1,lon2,lat2
      real    xnsum11,xnsum12,xnsum21,xnsum22,xnsumx
      real    HC_11, HC_12, HC_21, HC_22
      real    xnsum1_11,xnsum1_12,xnsum1_21,xnsum1_22
      real    xnsum2_11,xnsum2_12,xnsum2_21,xnsum2_22
      real    get_lon_angle, get_lat_angle, get_xnsum
      integer ist, ien, jst, jen
      real    xland,xwatr,xl1,xs1,oroavg
      integer int_opt, ipopt(20), kgds_input(200), kgds_output(200)
      integer count_land_output
      integer ij, ijmdl_output, iret, num_mismatch_land, num
      integer ibo(1)
      logical*1, allocatable :: bitmap_input(:,:)
      logical*1, allocatable :: bitmap_output(:)
      integer, allocatable :: ijsav_land_output(:)
      real,    allocatable :: lats_land_output(:)
      real,    allocatable :: lons_land_output(:)
      real,    allocatable :: output_data_land(:)
      real,    allocatable :: lons_mismatch_output(:)
      real,    allocatable :: lats_mismatch_output(:)
      real,    allocatable :: data_mismatch_output(:)
      integer, allocatable :: iindx(:), jindx(:)
C   
C---- GLOBAL XLAT AND XLON ( DEGREE )
C
      DELXN = 360./IMN      ! MOUNTAIN DATA RESOLUTION
C
      ijmdl_output = IM*JM
      
      DO J=1,JMN
         GLAT(J) = -90. + (J-1) * DELXN + DELXN * 0.5
      ENDDO
      print *,' IM=',IM,' JM=',JM,' IMN=',IMN,' JMN=',JMN
C
C---- FIND THE AVERAGE OF THE MODES IN A GRID BOX
C
C
      DO J=1,JM
        DO I=1,IM
          XNSUM(I,J) = 0.0
          ELVMAX(I,J) = ORO(I,J)
          ZMAX(I,J)   = 0.0
C---- COUNT NUMBER OF MODE. HIGHER THAN THE HC, CRITICAL HEIGHT
C     IN A GRID BOX
          XNSUM1(I,J) = 0.0
          XNSUM2(I,J) = 0.0
          XNSUM3(I,J) = 0.0
          XNSUM4(I,J) = 0.0
          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO

! --- # of peaks > ZAVG value and ZMAX(IM,JM) -- ORO is already avg.
! ---  to JM or to JM1
      DO J=1,JM
!        print*, "J=", J 
        DO I=1,IM
          HC = 1116.2 - 0.878 * VAR(I,J) 
          LONO(1) = lon_c(i,j) 
          LONO(2) = lon_c(i+1,j) 
          LONO(3) = lon_c(i+1,j+1) 
          LONO(4) = lon_c(i,j+1) 
          LATO(1) = lat_c(i,j) 
          LATO(2) = lat_c(i+1,j) 
          LATO(3) = lat_c(i+1,j+1) 
          LATO(4) = lat_c(i,j+1) 
          call get_index(IMN,JMN,4,LONO,LATO,DELXN,jst,jen,ilist,numx)
          do j1 = jst, jen; do ii1 = 1, numx          
            i1 = ilist(ii1)
            LONI = i1*DELXN
            LATI = -90 + j1*DELXN
            if(inside_a_polygon(LONI*D2R,LATI*D2R,4,
     &          LONO*D2R,LATO*D2R))then

              HEIGHT = FLOAT(ZAVG(I1,J1))
              IF(HEIGHT.LT.-990.) HEIGHT = 0.0
              IF ( HEIGHT .gt. ORO(I,J) ) then
                 if ( HEIGHT .gt. ZMAX(I,J) )ZMAX(I,J) = HEIGHT
              ENDIF   
            endif
          ENDDO ; ENDDO
        ENDDO
      ENDDO
      
C
! ---  this will make work1 array take on oro's values on return
! ---  this will make work1 array take on oro's values on return
      DO J=1,JM
        DO I=1,IM

          ORO1(I,J) = ORO(I,J)
          ELVMAX(I,J) = ZMAX(I,J) 
        ENDDO
      ENDDO
      
      DO KWD = 1, 4
        DO J=1,JM
          DO I=1,IM
            OA4(I,J,KWD) = 0.0
            OL(I,J,KWD) = 0.0
          ENDDO
        ENDDO
      ENDDO

      !--- use the nearest point to do remapping.
      int_opt = 2
      ipopt=0
      KGDS_INPUT = 0
      KGDS_INPUT(1) = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
      KGDS_INPUT(2) = IMI        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
      KGDS_INPUT(3) = JMI        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
      KGDS_INPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
      KGDS_INPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
      KGDS_INPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
      KGDS_INPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
      KGDS_INPUT(8) = NINT(-360000./IMI)  ! OCT 21-23 - LON OF EXTREME POINT
      KGDS_INPUT(9)  = NINT((360.0 / FLOAT(IMI))*1000.0)
                                 ! OCT 24-25 - LONGITUDE DIRECTION INCR.
      KGDS_INPUT(10) = JMI /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
      KGDS_INPUT(12) = 255       ! OCT 29 - RESERVED
      KGDS_INPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255


      KGDS_OUTPUT = -1
!      KGDS_OUTPUT(1) = IDRT       ! OCT 6 - TYPE OF GRID (GAUSSIAN)
      KGDS_OUTPUT(2) = IM        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
      KGDS_OUTPUT(3) = JM        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
      KGDS_OUTPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
      KGDS_OUTPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
      KGDS_OUTPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
      KGDS_OUTPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
      KGDS_OUTPUT(8) = NINT(-360000./IM)  ! OCT 21-23 - LON OF EXTREME POINT
      KGDS_OUTPUT(9)  = NINT((360.0 / FLOAT(IM))*1000.0)
                                  ! OCT 24-25 - LONGITUDE DIRECTION INCR.
      KGDS_OUTPUT(10) = JM /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
      KGDS_OUTPUT(12) = 255       ! OCT 29 - RESERVED
      KGDS_OUTPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255

      count_land_output=0    
      print*, "sum(slm) = ", sum(slm)
      do ij = 1, ijmdl_output
        j = (ij-1)/IM + 1
        i = mod(ij-1,IM) + 1         
        if (slm(i,j) > 0.0) then
          count_land_output=count_land_output+1
        endif
      enddo
      allocate(bitmap_input(imi,jmi))
      bitmap_input=.false.
      print*, "number of land input=", sum(slm_in)
      where(slm_in > 0.0) bitmap_input=.true.   
      print*, "count(bitmap_input)", count(bitmap_input)
      
      allocate(bitmap_output(count_land_output))
      allocate(output_data_land(count_land_output))
      allocate(ijsav_land_output(count_land_output))
      allocate(lats_land_output(count_land_output))
      allocate(lons_land_output(count_land_output))

      
      
      count_land_output=0
      do ij = 1, ijmdl_output
        j = (ij-1)/IM + 1
        i = mod(ij-1,IM) + 1 
        if (slm(i,j) > 0.0) then
          count_land_output=count_land_output+1
          ijsav_land_output(count_land_output)=ij
          lats_land_output(count_land_output)=lat_t(i,j)
          lons_land_output(count_land_output)=lon_t(i,j)
        endif
      enddo

      oa4 = 0.0
      ol = 0.0
      
      do KWD=1,4
        bitmap_output = .false.
         output_data_land = 0.0
        call ipolates(int_opt, ipopt, kgds_input, kgds_output,   
     &         (IMI*JMI), count_land_output,               
     &          1, 1, bitmap_input, oa_in(:,:,KWD),  
     &          count_land_output, lats_land_output,
     &          lons_land_output, ibo,  
     &          bitmap_output, output_data_land, iret)
        if (iret /= 0) then
          print*,'- ERROR IN IPOLATES ',iret
          call ERREXIT(4)
        endif

        num_mismatch_land = 0     
        do ij = 1, count_land_output
          if (bitmap_output(ij)) then
            j = (ijsav_land_output(ij)-1)/IM + 1
            i = mod(ijsav_land_output(ij)-1,IM) + 1
            oa4(i,j,KWD)=output_data_land(ij)
          else  ! default value
            num_mismatch_land =  num_mismatch_land + 1
          endif
        enddo  
        print*, "num_mismatch_land = ", num_mismatch_land
        
        if(.not. allocated(data_mismatch_output)) then
          allocate(lons_mismatch_output(num_mismatch_land))
          allocate(lats_mismatch_output(num_mismatch_land))      
          allocate(data_mismatch_output(num_mismatch_land))
          allocate(iindx(num_mismatch_land))
          allocate(jindx(num_mismatch_land))
          
          num = 0     
          do ij = 1, count_land_output
            if (.not. bitmap_output(ij)) then   
              num = num+1
              lons_mismatch_output(num) = lons_land_output(ij)
              lats_mismatch_output(num) = lats_land_output(ij)
            endif
          enddo

          ! For those land points that with bitmap_output=.false. use
          ! the nearest land points to interpolate.
          print*,"before get_mismatch_index", count(bitmap_input)
          call get_mismatch_index(imi,jmi,lon_in*D2R,
     &        lat_in*D2R,bitmap_input,num_mismatch_land, 
     &        lons_mismatch_output*D2R,lats_mismatch_output*D2R,
     &        iindx, jindx )
        endif                
 
        data_mismatch_output = 0
        call interpolate_mismatch(imi,jmi,oa_in(:,:,KWD),
     &        num_mismatch_land,data_mismatch_output,iindx,jindx)  
       
        num = 0
        do ij = 1, count_land_output
          if (.not. bitmap_output(ij)) then   
            num = num+1
            j = (ijsav_land_output(ij)-1)/IM + 1
            i = mod(ijsav_land_output(ij)-1,IM) + 1
            oa4(i,j,KWD) = data_mismatch_output(num)
            if(i==372 .and. j== 611) then
            print*, "ij=",ij, num, oa4(i,j,KWD),iindx(num),jindx(num)
            endif
          endif
        enddo     
       
       
      enddo

      !OL
      do KWD=1,4
        bitmap_output = .false.
        output_data_land = 0.0
        call ipolates(int_opt, ipopt, kgds_input, kgds_output,   
     &         (IMI*JMI), count_land_output,               
     &          1, 1, bitmap_input, ol_in(:,:,KWD),  
     &          count_land_output, lats_land_output,
     &          lons_land_output, ibo,  
     &          bitmap_output, output_data_land, iret)
        if (iret /= 0) then
          print*,'- ERROR IN IPOLATES ',iret
          call ERREXIT(4)
        endif

        num_mismatch_land = 0     
        do ij = 1, count_land_output
          if (bitmap_output(ij)) then
            j = (ijsav_land_output(ij)-1)/IM + 1
            i = mod(ijsav_land_output(ij)-1,IM) + 1
            ol(i,j,KWD)=output_data_land(ij)
          else  ! default value
            num_mismatch_land =  num_mismatch_land + 1
          endif
        enddo  
        print*, "num_mismatch_land = ", num_mismatch_land
        
        data_mismatch_output = 0
        call interpolate_mismatch(imi,jmi,ol_in(:,:,KWD),
     &        num_mismatch_land,data_mismatch_output,iindx,jindx)  
       
        num = 0
        do ij = 1, count_land_output
          if (.not. bitmap_output(ij)) then   
            num = num+1
            j = (ijsav_land_output(ij)-1)/IM + 1
            i = mod(ijsav_land_output(ij)-1,IM) + 1
            ol(i,j,KWD) = data_mismatch_output(num)
            if(i==372 .and. j== 611) then
            print*, "ij=",ij, num, ol(i,j,KWD),iindx(num),jindx(num)
            endif
          endif
        enddo     
       
       
      enddo
     
      deallocate(lons_mismatch_output,lats_mismatch_output)
      deallocate(data_mismatch_output,iindx,jindx)
      deallocate(bitmap_output,output_data_land)
      deallocate(ijsav_land_output,lats_land_output)
      deallocate(lons_land_output)
       
      DO KWD=1,4
        DO J=1,JM
          DO I=1,IM
            T = OA4(I,J,KWD)
            OA4(I,J,KWD) = SIGN( MIN( ABS(T), 1. ), T )
          ENDDO
        ENDDO
      ENDDO
C
      NS0 = 0
      NS1 = 0
      NS2 = 0
      NS3 = 0
      NS4 = 0
      NS5 = 0
      NS6 = 0
      DO KWD=1,4
      DO J=1,JM
      DO I=1,IM
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
C
      WRITE(6,*) "! MAKEOA3 EXIT"
C
      RETURN
      END
      
      SUBROUTINE REVERS(IM, JM, numi, F, WRK)
!
      REAL F(IM,JM), WRK(IM,JM)
      integer numi(jm)
C
C     reverse east-west and north-south
c......  fix this routine up to take numi (*j*)
C.....   at least have 5 variables ....and keep REVLAT .FALSE.
       imb2 = im / 2
      do i=1,im*jm
         WRK(i,1) = F(i,1)
      enddo
      do j=1,jm
         jr = jm - j + 1
         do i=1,im
            ir = i + imb2
            if (ir .gt. im) ir = ir - im
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
        implicit none
        integer,intent(in):: im,jm,numi(jm)
        real,intent(inout):: a(im,jm)
        integer j,ir,ig
        real r,t(im)
        do j=1,jm
          r=real(numi(j))/real(im)
          do ig=1,im
            ir=mod(nint((ig-1)*r),numi(j))+1
            t(ig)=a(ir,j)
          enddo
          do ig=1,im
            a(ig,j)=t(ig)
          enddo
        enddo
      end subroutine
      subroutine gg2rg(im,jm,numi,a)
        implicit none
        integer,intent(in):: im,jm,numi(jm)
        real,intent(inout):: a(im,jm)
        integer j,ir,ig
        real r,t(im)
        do j=1,jm
          r=real(numi(j))/real(im)
          do ir=1,numi(j)
            ig=nint((ir-1)/r)+1
            t(ir)=a(ig,j)
          enddo
          do ir=1,numi(j)
            a(ir,j)=t(ir)
          enddo
        enddo
      end subroutine
      SUBROUTINE minmxj(IM,JM,A,title)

c this routine is using real*4 on the sp

      implicit none

      real A(IM,JM),rmin,rmax
      integer i,j,IM,JM
      character*8 title

      rmin=1.e+10
      rmax=-rmin
csela....................................................
csela if(rmin.eq.1.e+10)return
csela....................................................
      DO j=1,JM
      DO i=1,IM
        if(A(i,j).ge.rmax)rmax=A(i,j)
        if(A(i,j).le.rmin)rmin=A(i,j)
      ENDDO
      ENDDO
      write(6,150)rmin,rmax,title
150   format('rmin=',e13.4,2x,'rmax=',e13.4,2x,a8,' ')
C
      RETURN
      END
      SUBROUTINE mnmxja(IM,JM,A,imax,jmax,title)

c this routine is using real*4 on the sp

      implicit none

      real A(IM,JM),rmin,rmax
      integer i,j,IM,JM,imax,jmax
      character*8 title

      rmin=1.e+10
      rmax=-rmin
csela....................................................
csela if(rmin.eq.1.e+10)return
csela....................................................
      DO j=1,JM
      DO i=1,IM
        if(A(i,j).ge.rmax)then
                         rmax=A(i,j)
                        imax=i
                        jmax=j
        endif
        if(A(i,j).le.rmin)rmin=A(i,j)
      ENDDO
      ENDDO
      write(6,150)rmin,rmax,title
150   format('rmin=',e13.4,2x,'rmax=',e13.4,2x,a8,' ')
C
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SPFFT1(IMAX,INCW,INCG,KMAX,W,G,IDIR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPFFT1     PERFORM MULTIPLE FAST FOURIER TRANSFORMS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-20
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS MULTIPLE FAST FOURIER TRANSFORMS
C           BETWEEN COMPLEX AMPLITUDES IN FOURIER SPACE AND REAL VALUES
C           IN CYCLIC PHYSICAL SPACE.
C           SUBPROGRAM SPFFT1 INITIALIZES TRIGONOMETRIC DATA EACH CALL.
C           USE SUBPROGRAM SPFFT TO SAVE TIME AND INITIALIZE ONCE.
C           THIS VERSION INVOKES THE IBM ESSL FFT.
C
C PROGRAM HISTORY LOG:
C 1998-12-18  IREDELL
C
C USAGE:    CALL SPFFT1(IMAX,INCW,INCG,KMAX,W,G,IDIR)
C
C   INPUT ARGUMENT LIST:
C     IMAX     - INTEGER NUMBER OF VALUES IN THE CYCLIC PHYSICAL SPACE
C                (SEE LIMITATIONS ON IMAX IN REMARKS BELOW.)
C     INCW     - INTEGER FIRST DIMENSION OF THE COMPLEX AMPLITUDE ARRAY
C                (INCW >= IMAX/2+1)
C     INCG     - INTEGER FIRST DIMENSION OF THE REAL VALUE ARRAY
C                (INCG >= IMAX)
C     KMAX     - INTEGER NUMBER OF TRANSFORMS TO PERFORM
C     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR>0
C     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR<0
C     IDIR     - INTEGER DIRECTION FLAG
C                IDIR>0 TO TRANSFORM FROM FOURIER TO PHYSICAL SPACE
C                IDIR<0 TO TRANSFORM FROM PHYSICAL TO FOURIER SPACE
C
C   OUTPUT ARGUMENT LIST:
C     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR<0
C     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR>0
C
C SUBPROGRAMS CALLED:
C   SCRFT        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
C   DCRFT        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
C   SRCFT        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
C   DRCFT        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C REMARKS:
C   THE RESTRICTIONS ON IMAX ARE THAT IT MUST BE A MULTIPLE
C   OF 1 TO 25 FACTORS OF TWO, UP TO 2 FACTORS OF THREE,
C   AND UP TO 1 FACTOR OF FIVE, SEVEN AND ELEVEN.
C
C   THIS SUBPROGRAM IS THREAD-SAFE.
C
C$$$
        IMPLICIT NONE
        INTEGER,INTENT(IN):: IMAX,INCW,INCG,KMAX,IDIR
        COMPLEX,INTENT(INOUT):: W(INCW,KMAX)
        REAL,INTENT(INOUT):: G(INCG,KMAX)
        REAL:: AUX1(25000+INT(0.82*IMAX))
        REAL:: AUX2(20000+INT(0.57*IMAX))
        INTEGER:: NAUX1,NAUX2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        NAUX1=25000+INT(0.82*IMAX)
        NAUX2=20000+INT(0.57*IMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FOURIER TO PHYSICAL TRANSFORM.
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PHYSICAL TO FOURIER TRANSFORM.
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
cc
      implicit none
cc
      include 'machine.h'
      include 'resevod.h'
cc
      integer*2    glob(360*120,180*120)
cc
      integer      ix,jx
      integer      ia,ja
cc
      parameter   (ix=40*120,jx=50*120)
      parameter   (ia=60*120,ja=30*120)
cc
      integer*2    idat(ix,jx),itopo
cc
ccmr  integer*2    m9999 
ccmr  data         m9999    / -9999 /
cc
ccmr  integer      i_count(360*120)
ccmr  integer      j_max_y(360*120)
cc
      integer      i,j,inttyp
cc
      real(kind=8) dloin,dlain,rlon,rlat
cc
      open(235, file="./fort.235", access='direct', recl=43200*21600*2)
           read(235,rec=1)glob
         rewind(235)
cc
cc
      print*,' '
      call maxmin     (glob,360*120*180*120,'global0')
cc
cc
      dloin=1.d0/120.d0
      dlain=1.d0/120.d0
cc
      rlon= -179.995833333333333333333333d0
      rlat=   89.995833333333333333333333d0
cc
      inttyp=-1  !  average rectangular subset
ccmr  inttyp= 1  !  take closest grid point value
ccmr  inttyp= 0  !  interpolate from four closest grid point values
cc
!     call la2ga_gtopo30(glob,360*120,180*120,
!    &           dloin,dlain,rlon,rlat,inttyp,
!    &           .true.,glob,
!    &           0,lonf,latg)
cc
      return
      end
      subroutine maxmin(ia,len,tile)
ccmr
      implicit none
ccmr
      integer*2 ia(len)
      character*7 tile
      integer iaamax, iaamin, len, j, m, ja, kount
      integer(8) sum2,std,mean,isum
      integer i_count_notset,kount_9
! --- missing is -9999
c
      isum = 0
      sum2 = 0
      kount = 0
      kount_9 = 0
      iaamax = -9999999
ccmr  iaamin = 1
      iaamin =  9999999
      i_count_notset=0
           do 10 m=1,len
      ja=ia(m)
ccmr  if ( ja .lt. 0 ) print *,' ja < 0:',ja
ccmr  if ( ja .eq. -9999 ) goto 10
      if ( ja .eq. -9999 ) then
           kount_9=kount_9+1
           goto 10
      endif
      if ( ja .eq. -12345 ) i_count_notset=i_count_notset+1
ccmr  if ( ja .eq. 0 ) goto 11
      iaamax = max0( iaamax, ja )
      iaamin = min0( iaamin, ja )
!     iaamax = max0( iaamax, ia(m,j) )
!     iaamin = min0( iaamin, ia(m,j) )
  11  continue
      kount = kount + 1
      isum = isum + ja
ccmr  sum2 = sum2 + ifix( float(ja) * float(ja) )
      sum2 = sum2 + ja*ja
  10  continue
!
      mean = isum/kount
      std = ifix(sqrt(float((sum2/(kount))-mean**2)))
      print*,tile,' max=',iaamax,' min=',iaamin,' sum=',isum,
     &       ' i_count_notset=',i_count_notset
      print*,tile,' mean=',mean,' std.dev=',std,
     &       ' ko9s=',kount,kount_9,kount+kount_9 
      return
      end
      SUBROUTINE minmaxj(IM,JM,A,title)

c this routine is using real*4 on the sp

      implicit none

      real(kind=4) A(IM,JM),rmin,rmax,undef
      integer i,j,IM,JM,imax,jmax,imin,jmin,iundef
      character*8 title,chara
      data chara/'        '/
      chara=title
      rmin=1.e+10
      rmax=-rmin
      imax=0
      imin=0
      jmax=0
      jmin=0
      iundef=0
      undef=-9999.
csela....................................................
csela if(rmin.eq.1.e+10)return
csela....................................................
      DO j=1,JM
      DO i=1,IM
        if(A(i,j).ge.rmax)then
                        rmax=A(i,j)
                       imax=i
                       jmax=j
        endif
         if(A(i,j).le.rmin)then
            if ( A(i,j) .eq. undef ) then
                iundef = iundef + 1
            else
                        rmin=A(i,j)
                       imin=i
                       jmin=j
            endif
        endif
      ENDDO
      ENDDO
      write(6,150)chara,rmin,imin,jmin,rmax,imax,jmax,iundef
150   format(1x,a8,2x,'rmin=',e13.4,2i6,2x,'rmax=',e13.4,3i6)
C
      RETURN
      END

      
      !routine to map (lon, lat) to (x,y,z)
      subroutine latlon2xyz(siz,lon, lat, x, y, z)
      implicit none
      integer, intent(in) :: siz
      real, intent(in) :: lon(siz), lat(siz)
      real, intent(out) :: x(siz), y(siz), z(siz)
      
      integer n

      do n = 1, siz
        x(n) = cos(lat(n))*cos(lon(n))
        y(n) = cos(lat(n))*sin(lon(n))
        z(n) = sin(lat(n))
      enddo
      end

      FUNCTION spherical_angle(v1, v2, v3)
        implicit none
        real, parameter :: EPSLN30 = 1.e-30
        real, parameter :: PI=3.1415926535897931
        real v1(3), v2(3), v3(3)
        real  spherical_angle
 
        real px, py, pz, qx, qy, qz, ddd;
  
        ! vector product between v1 and v2 
        px = v1(2)*v2(3) - v1(3)*v2(2)
        py = v1(3)*v2(1) - v1(1)*v2(3)
        pz = v1(1)*v2(2) - v1(2)*v2(1)
        ! vector product between v1 and v3 
        qx = v1(2)*v3(3) - v1(3)*v3(2);
        qy = v1(3)*v3(1) - v1(1)*v3(3);
        qz = v1(1)*v3(2) - v1(2)*v3(1);

        ddd = (px*px+py*py+pz*pz)*(qx*qx+qy*qy+qz*qz);
        if ( ddd <= 0.0 ) then
          spherical_angle = 0. 
        else 
          ddd = (px*qx+py*qy+pz*qz) / sqrt(ddd);
          if( abs(ddd-1) < EPSLN30 ) ddd = 1;
          if( abs(ddd+1) < EPSLN30 ) ddd = -1;
          if ( ddd>1. .or. ddd<-1. ) then
            !FIX to correctly handle co-linear points (angle near pi or 0) */
            if (ddd < 0.) then
              spherical_angle = PI
            else
              spherical_angle = 0.
            endif
          else
            spherical_angle = acos( ddd )
          endif
        endif  

        return
      END  
      
      FUNCTION inside_a_polygon(lon1, lat1, npts, lon2, lat2)
        implicit none
        real, parameter :: EPSLN10 = 1.e-10
        real, parameter :: EPSLN8 = 1.e-8
        real, parameter :: PI=3.1415926535897931
        real, parameter :: RANGE_CHECK_CRITERIA=0.05
        real :: anglesum, angle, spherical_angle
        integer i, ip1
        real lon1, lat1
        integer npts
        real lon2(npts), lat2(npts)
        real x2(npts), y2(npts), z2(npts)
        real lon1_1d(1), lat1_1d(1)
        real x1(1), y1(1), z1(1)
        real pnt0(3),pnt1(3),pnt2(3)
        logical inside_a_polygon
        real max_x2,min_x2,max_y2,min_y2,max_z2,min_z2
        !first convert to cartesian grid */
        call latlon2xyz(npts,lon2, lat2, x2, y2, z2);
        lon1_1d(1) = lon1
        lat1_1d(1) = lat1
        call latlon2xyz(1,lon1_1d, lat1_1d, x1, y1, z1);
        inside_a_polygon = .false.
        max_x2 = maxval(x2)
        if( x1(1) > max_x2+RANGE_CHECK_CRITERIA ) return
        min_x2 = minval(x2)
        if( x1(1)+RANGE_CHECK_CRITERIA < min_x2 ) return
        max_y2 = maxval(y2)
        if( y1(1) > max_y2+RANGE_CHECK_CRITERIA ) return
        min_y2 = minval(y2)
        if( y1(1)+RANGE_CHECK_CRITERIA < min_y2 ) return
        max_z2 = maxval(z2)
        if( z1(1) > max_z2+RANGE_CHECK_CRITERIA ) return
        min_z2 = minval(z2)
        if( z1(1)+RANGE_CHECK_CRITERIA < min_z2 ) return

        pnt0(1) = x1(1)
        pnt0(2) = y1(1)
        pnt0(3) = z1(1)
        
        anglesum = 0;
        do i = 1, npts
           if(abs(x1(1)-x2(i)) < EPSLN10 .and.
     &          abs(y1(1)-y2(i)) < EPSLN10 .and.
     &         abs(z1(1)-z2(i)) < EPSLN10 ) then ! same as the corner point
              inside_a_polygon = .true.
              return
           endif
           ip1 = i+1
           if(ip1>npts) ip1 = 1
           pnt1(1) = x2(i)
           pnt1(2) = y2(i)
           pnt1(3) = z2(i)
           pnt2(1) = x2(ip1)
           pnt2(2) = y2(ip1)
           pnt2(3) = z2(ip1)

           angle = spherical_angle(pnt0, pnt2, pnt1);
!           anglesum = anglesum + spherical_angle(pnt0, pnt2, pnt1);
           anglesum = anglesum + angle
        enddo

        if(abs(anglesum-2*PI) < EPSLN8) then
           inside_a_polygon = .true.
        else
           inside_a_polygon = .false.
        endif

        return
        
      end


      function get_xnsum(lon1,lat1,lon2,lat2,IMN,JMN,
     &                   glat,zavg,zslm,delxn)
        implicit none

        real get_xnsum
        logical verbose
        real lon1,lat1,lon2,lat2,oro,delxn
        integer IMN,JMN
        real    glat(JMN)
        integer zavg(IMN,JMN),zslm(IMN,JMN)
        integer i, j, ist, ien, jst, jen, i1
        real    HEIGHT
        real    xland,xwatr,xl1,xs1,slm,xnsum
        !---figure out ist,ien,jst,jen
        do j = 1, JMN
           if( GLAT(J) .GT. lat1 ) then
              jst = j
              exit
           endif
        enddo
        do j = 1, JMN
           if( GLAT(J) .GT. lat2 ) then
              jen = j
              exit
           endif
        enddo

        
        ist = lon1/delxn + 1
        ien = lon2/delxn
        if(ist .le.0) ist = ist + IMN
        if(ien < ist) ien = ien + IMN
!        if(verbose) print*, "ist,ien=",ist,ien,jst,jen

        !--- compute average oro
          oro = 0.0
          xnsum = 0
          xland = 0
          xwatr = 0
          xl1 = 0
          xs1 = 0
          do j = jst,jen
             do i1 = 1, ien - ist + 1
                i = ist + i1 -1
                if( i .LE. 0) i = i + imn
                if( i .GT. IMN) i = i - imn
                XLAND = XLAND + FLOAT(ZSLM(I,J))
                XWATR = XWATR + FLOAT(1-ZSLM(I,J))
                XNSUM = XNSUM + 1.
                HEIGHT = FLOAT(ZAVG(I,J)) 
                IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                XL1 = XL1 + HEIGHT * FLOAT(ZSLM(I,J))
                XS1 = XS1 + HEIGHT * FLOAT(1-ZSLM(I,J))
             enddo
          enddo
          if( XNSUM > 1.) THEN
             SLM = FLOAT(NINT(XLAND/XNSUM))
               IF(SLM.NE.0.) THEN
                  ORO= XL1 / XLAND
               ELSE
                  ORO = XS1 / XWATR
               ENDIF
          ENDIF
          
         get_xnsum = 0
         do j = jst, jen
            do i1= 1, ien-ist+1
               i = ist + i1 -1
               if( i .LE. 0) i = i + imn
               if( i .GT. IMN) i = i - imn
               HEIGHT = FLOAT(ZAVG(I,J))
               IF(HEIGHT.LT.-990.) HEIGHT = 0.0
               IF ( HEIGHT .gt. ORO ) get_xnsum = get_xnsum + 1
            enddo       
         enddo
!         if(verbose) print*, "get_xnsum=", get_xnsum, oro
         
      end function get_xnsum  
      

      subroutine get_xnsum2(lon1,lat1,lon2,lat2,IMN,JMN,
     &                   glat,zavg,zslm,delxn,xnsum1,xnsum2,HC)
        implicit none

        real, intent(out) :: xnsum1,xnsum2,HC
        logical verbose
        real lon1,lat1,lon2,lat2,oro,delxn
        integer IMN,JMN
        real    glat(JMN)
        integer zavg(IMN,JMN),zslm(IMN,JMN)
        integer i, j, ist, ien, jst, jen, i1
        real    HEIGHT, var
        real    XW1,XW2,slm,xnsum
        !---figure out ist,ien,jst,jen
        do j = 1, JMN
           if( GLAT(J) .GT. lat1 ) then
              jst = j
              exit
           endif
        enddo
        do j = 1, JMN
           if( GLAT(J) .GT. lat2 ) then
              jen = j
              exit
           endif
        enddo

        
        ist = lon1/delxn + 1
        ien = lon2/delxn
        if(ist .le.0) ist = ist + IMN
        if(ien < ist) ien = ien + IMN
!        if(verbose) print*, "ist,ien=",ist,ien,jst,jen

        !--- compute average oro
          xnsum = 0
          XW1 = 0
          XW2 = 0
          do j = jst,jen
             do i1 = 1, ien - ist + 1
                i = ist + i1 -1
                if( i .LE. 0) i = i + imn
                if( i .GT. IMN) i = i - imn
                XNSUM = XNSUM + 1.
                HEIGHT = FLOAT(ZAVG(I,J)) 
                IF(HEIGHT.LT.-990.) HEIGHT = 0.0
                XW1 = XW1 + HEIGHT
                XW2 = XW2 + HEIGHT ** 2
             enddo
          enddo
          var = SQRT(MAX(XW2/XNSUM-(XW1/XNSUM)**2,0.))
          HC = 1116.2 - 0.878 * VAR
         xnsum1 = 0
         xnsum2 = 0
         do j = jst, jen
            do i1= 1, ien-ist+1
               i = ist + i1 -1
               if( i .LE. 0) i = i + imn
               if( i .GT. IMN) i = i - imn
               HEIGHT = FLOAT(ZAVG(I,J))
               IF ( HEIGHT .gt. HC ) xnsum1 = xnsum1 + 1
                xnsum2 = xnsum2 + 1
            enddo       
         enddo
         
      end subroutine get_xnsum2 


      subroutine get_xnsum3(lon1,lat1,lon2,lat2,IMN,JMN,
     &                   glat,zavg,zslm,delxn,xnsum1,xnsum2,HC)
        implicit none

        real, intent(out) :: xnsum1,xnsum2
        real lon1,lat1,lon2,lat2,oro,delxn
        integer IMN,JMN
        real    glat(JMN)
        integer zavg(IMN,JMN),zslm(IMN,JMN)
        integer i, j, ist, ien, jst, jen, i1
        real    HEIGHT, HC
        real    XW1,XW2,slm,xnsum
        !---figure out ist,ien,jst,jen
        ! if lat1 or lat 2 is 90 degree. set jst = JMN
        jst = JMN
        jen = JMN
        do j = 1, JMN
           if( GLAT(J) .GT. lat1 ) then
              jst = j
              exit
           endif
        enddo
        do j = 1, JMN
           if( GLAT(J) .GT. lat2 ) then
              jen = j
              exit
           endif
        enddo

        
        ist = lon1/delxn + 1
        ien = lon2/delxn
        if(ist .le.0) ist = ist + IMN
        if(ien < ist) ien = ien + IMN
!        if(verbose) print*, "ist,ien=",ist,ien,jst,jen

         xnsum1 = 0
         xnsum2 = 0
         do j = jst, jen
            do i1= 1, ien-ist+1
               i = ist + i1 -1
               if( i .LE. 0) i = i + imn
               if( i .GT. IMN) i = i - imn
               HEIGHT = FLOAT(ZAVG(I,J))
               IF ( HEIGHT .gt. HC ) xnsum1 = xnsum1 + 1
                xnsum2 = xnsum2 + 1
            enddo       
         enddo
         
      end subroutine get_xnsum3
      
      subroutine nanc(a,l,c)
c compiler opt TRAPS= -qinitauto=FF911299 -qflttrap=ov:zero:inv:en -qsig trap
c or call subroutine below
c subroutine to report NaNS and NaNQ within an address
c range for real*8 words.
c  as written the routine prints a single line for each call
c  and prints a message and returns to the caller  on detection of the FIRST
c  NaN in the range.  The message is passed in the  third
c  argument.  If no NaN values are found it returns silently.
c  A real*4 version can be created by making A real*4

c    arguments (all are input only)
c
c    A   real*8 variable or array
c    L   number of words to scan (length of array)
c    C   distinctive message set in caller to indicate where
c        the routine was called.
c 
      integer inan1,inan2,inan3,inan4,inaq1,inaq2,inaq3,inaq4
       real word
       integer itest
       equivalence (itest,word)
c
c signaling NaN
      data inan1/x'7F800001'/
      data inan2/x'7FBFFFFF'/
      data inan3/x'FF800001'/
      data inan4/x'FFBFFFFF'/
c
c  quiet NaN
c
      data inaq1/x'7FC00000'/
      data inaq2/x'7FFFFFFF'/
      data inaq3/x'FFC00000'/
      data inaq4/x'FFFFFFFF'/
c
      real(kind=8)a(l),rtc,t1,t2
      character*24 cn
      character*(*) c
      t1=rtc()
cgwv        print *, ' nanc call ',c
      do k=1,l
      word=a(k)
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
cgwv      print 102,l,t2-t1,c
 102  format(' time to check ',i9,' words is ',f10.4,' ',a24)
      return
       end
      real function timef
      character(8) :: date
      character(10) :: time
      character(5) :: zone
      integer,dimension(8) :: values
      integer :: total
      real :: elapsed
      call date_and_time(date,time,zone,values)
      total=(3600*values(5))+(60*values(6))
     *      +values(7)
      elapsed=float(total) + (1.0e-3*float(values(8)))
      timef=elapsed
      return
      end
