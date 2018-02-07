C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: TRPSFCMV
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2001-02-09
C
C ABSTRACT:
C       Plots the following in the tropical strip: analyzed wind
C    barbs and temperatures at several hundred locations, contours of
C    1000 mb analyzed streamfunction, gridded winds, station plots
C    and tropical cyclone positions.  It uses NCAR graphics to
C    produce a metafile which is rasterized in subsequent program
C    executions.   It also generates titles and hurricane bulletin
C    text (if needed) for processing by the Bedient packer.  The
C    gross job flow is gendata  --> redsat  --> trpsfcmv  --->
C    ictrans ----> ras2bity ----> sixbit2 (bedient packer) ---> and
C    out the door to OSO, NCDC, TPC and FAX.  Input is current GLOBAL
C    SPECTRAL MODEL ANALYSIS winds in GRIB on type 3 (1x1) grids and
C    also station coordinates and observed parameters (temp, dewpoint,
C    pressure, tendancy, windspeed, and direction for plotting by
C    NOAA graphics (with superior fonts) downstream.  The station
C    coordinate file is an AFOS PLOTFILE with all linefeeds (Z'0d')
C    removed by subroutine afosread.  The station coordinates are
C    converted to pixel values for downstream use by the bedient
C    packer.
C
C PROGRAM HISTORY LOG:
C 1996-12-18  George VandenBerghe   Original  ancestor code        
C 1996-12-31  George VandenBerghe   Documented
C 1997-01-30  George VandenBerghe   Made grib errors fatal. (stop 17)
C 1997-04-30  George VandenBerghe   Removed grid station
C                     interpolation, NCAR station plotting, changed to
C                     mercator plot of single streamfunction, added
C                     streamfunction generator, grid plot, lat/lon
C                     labels, contouring capability, AFOS plotfile
C                     read capability, and code to write putlab input
C                     for downstream use by a new station plotter in
C                     the bedient packer.  Also changed name.
C 1998-08-10  Dennis Keyser        Modified format for reading
C                     tcvitals records to prevent the ocassional
C                     failures that were occurring when previous
C                     program QCTROPCY writes a 'C' into character 65
C                     of the record.  This (climatology indicator) is
C                     a rare occurrence (format not changed if maxwind
C                     happens to be > 99 m/s (not sure this is even
C                     possible).
C 1999-01-25  Krishna Kumar        Modified the code to run on IBM 
C                     RS/6000 SP system.
C 2001-02-09  Dennis Keyser        Updated subroutine HBULL to handle
C                     tcvitals records with 4-digit year (and to still
C                     work properly for bulletins with a 4-digit
C                     year), this had not been working properly since
C                     late 1999 when 4-digit years were written to the
C                     tcvitals file (always stamped out "NO TROPICAL
C                     CYCLONE ACTIVITY REPORTED"); also changed HBULL
C                     to recognize expanded test storm id range (now
C                     80-99, was 90-99) implemented by TPC.
C
C USAGE:
C   INPUT FILES:
C     unit 11  - GFS 00 hour GRIB forecast file containing grib type
C              - 3 u, and v grids       
C     unit 12  - Grib index to above
C     unit 31  - optional quality controlled hurricane text bulletins
C                ( in $COMIN/gblav.$cycle.syndata.tcvitals.tm00 for GFS
C                 and $COMIN/gdas1.$cycle.syndata.tcvitals.tm00 for FNL)
C     unit 38  - Satellite low level wind observations 
C     unit 43  - AFOS station plotfile with linefeeds
C     unit 44  - AFOS station plotfile (sans linefeeds)
C                (see output file list)
C
C   OUTPUT FILES:
C     gmeta    - implicitly opened as fortran unit 2 by ncar
C                package
C     unit 06  - standard output print
C     unit 74  - text file containing tropical cyclone bulletin info
C     unit 87  - AFOS station plotfile sans linefeeds to be
C                read later in this program as unit 44
C     unit 88    Map title for downstream use by bedient packer
C     unit 89    putlab argument file used by bedient packer.
C
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - closen   openn    get3     qcnt     afosread
C                - l2p      cg       mpr      fill     ccpllb
C                - color    sfill    stream3  ridctlm  afix
C                - satread  redsat   dayowk   i3to53   i53to3
C                - i3to8    hbull
C     LIBRARY:
C       NCAR GRAPHICS:
C                - mappos   wmsetr   wmseti   wmgetr   maptra
C                - pcseti   plccmq   plchmq   wmbarb   gslwsc
C                - gsclip   mapsti   mapstc   maproj   mapset
C                - mapdrw   cpseti   cpsetr   gopks    gopwk
C                - gacwk    gdawk    gclwk    gclks    setusv
C                - mapros   dpseti   frame    gsln     stream3
C                - sfnorm   sfsetr   hlsrgb   gclwk    gclks
C                - gsfaci   gscr     gfa      gsclip   gsfais
C                - cprect   cppkcl   plchhq   cpcldr   cplbdr
C                - gdawk    
C       W3LIB    - getgb    w3fp11   getgb1   errexit  w3tagb
C                - w3tage   putgb
C       IPLIB    - ipolates makgds
C       SPLIB    - sptrunv
C       BACIO    - baopen   baclos
C       BUFRLIB  - openbf   readmg   readsb   ufbint
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            179 - Grib error on input or index file (unwise to
C                   continue
C          =NNNN - system only                                       
C
C REMARKS:  IBM NCAR graphics are in /usrx/local/ncar401 
C       CAVEAT  requires ncar graphics version 4.0 or above
C               as of this writing
C               setenv NCARG_ROOT /usr/local/ncar4.0          
C               setenv PATH "$PATH":/usr/local/ncar4.0/bin
C                   (or the bourne/korn analogs)
C               handles this; only the first setenv is required
C               but the other  will likely be 
C               needed by ictrans downstream
C             
C               The latitude to pixel coordinate converter
C               assumes a 6912x6912 domain and that 
C               a 6912x1728 slice will be cut horizontally
C               out of the domain downstream. The slice
C               is then supposed to be rotated 90 degrees
C                counterclockwise so that the long axis becomes
C               the y axis maximizing fax paper use.  This code
C               cannot detect if these assumptions are
C               in fact met downstream, if not, stations
C               will be plotted in incorrect locations.
C               The algorithm is however NOT sensitive to
C               map projection.
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90 
C   MACHINE:  IBM
C
C$$$

      PROGRAM TRPSFCMV

      parameter(nsta=124)
      character*3 cmonth(12) 
      data cmonth/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     1 'AUG','SEP','OCT','NOV','DEC'/
       dimension f3(360,181)
      character*4 ctemp,cday
      character*3 cdir
      character*80  ctext
      character*80 ctext2,ctext3
      character*4 cda
      COMMON/PACKRA/IRAS(10)
       common/oth/u(70,40),v(70,40),rlats(nsta),rlons(nsta),
     1 up(nsta),vp(nsta),tp(nsta)
c      paraMETER (RMNLON=-0.,RMXLON=359.,
      paraMETER (RMNLON=-0.,RMXLON=359.,
     1 RMNLAT=-60.,RMXLAT=60.)
      dimension kds(25)
      dimension rl(2),rl2(2),rl3(2),rl4(2)
      data rl,rl2,rl3,rl4/rmnlat,0.,rmxlat,0.,
     1rmnlon,0.,rmxlon,0./
      dimension grid(360,181)
      dimension g2(360,121),z2(360,121)
        dimension u2(360,121), v2(360,121)
      dimension f53(117,51)
      character*40 ctit
      dimension ifeet(8)
      character*132 mtitle
c
      character*11 envvar            ! for ibm_sp_6000
      character*80 fileg,filegi      ! for ibm_sp_6000
c
      CALL W3TAGB('TRPSFCMV',2001,0038,0059,'NP22')                   
 6    continue
c
c****** added for ibm_sp_6000
c
       lun=11
       envvar='FORT   '
       write(envvar(5:6),fmt='(I2)') lun
       call getenv(envvar,fileg)
       call baopen(lun,fileg,iret)
c
       luni=12
       envvar='FORT   '
       write(envvar(5:6),fmt='(I2)') luni
       call getenv(envvar,filegi)
       call baopen(luni,filegi,iret)
c
c******
c
       call openn
c
      do 777 imap=1,1
      if(imap .gt. 1) then
      lun=13
      luni=14
      endif
      do 1 k=1,25
 1    kds(k)=-1
       ix=360
       iy=121
c        do 1000 ,ll=1,1,-1
       call mappos (0.0,1.0,0.0,1.0) 
C get streamfunction
        call stream3(1000,-1,11,12,grid)
        pi=3.1415927
        sin45=sqrt(2.)/2
        omega=2*pi/86400.
        g=9.81
        fcoril=2*omega*sin45
        factor=fcoril/g
        factor=factor/10. 
C**    CONVERT TO METERS 
       do k=1,360
       do j=1,121
        g2(k,j)=grid(k+0,152-j ) * factor
       end do
       end do
c**
c  get temperature (fossil code)
c**    get u for wind barbs
       kds(5)=33
       kds(6)=100
       kds(7)=1000
       call get3(lun,luni,kds,grid,mtitle)
       do k=1,360
       do j=1,121
        u2(k,j)=grid(k+0,152-j ) * 1.93
       end do
       end do
c**
c**  get v for wind barbs
       kds(5)=34
       kds(6)=100
       kds(7)=1000
       call get3(lun,luni,kds,grid,mtitle)
       do k=1,360
       do j=1,121
        v2(k,j)=grid(k+0,152-j )*1.93
       end do
       end do
c**
       call cpseti('CIS',3)
       call gslwsc(2.0)
       call qcnt (g2,ix,iy,rl,rl2,rl3,rl4,mtitle)
       call hbull
       call cpseti('CIS',3)
       call gsln(0)
       call cg( g2,ix,iy,1)
       call cpseti('CIS',5)
      CALL WMSETR('WBS',0.003)
      CALL WMSETR('WBD',0.17)
      CALL WMSETI('COL',1)
      CALL WMGETR('WBS',WSLEN)
      irr=1
       flat=40.
        flon=270.
      do 14,k=1,120,5
      call gslwsc  (1.0)
      do 14,l=1,360,5
      if (irr .eq. 1) then
      irr=2
      else
      irr=1
      endif
       if(irr .eq. 1) then
       l2=l-1
       k2=k
       else
       l2=l-1
c       k2=k+1
       k2=k
       endif
       flon=l2
       flat=k2-61.
       call maptra(flat,flon,uu,vv)
       iflat=flat+5.
       if(iflat .lt. 0) then
        write(ctemp,1087)-iflat
 1087  format(i2,x,'S')
       else
       write(ctemp,1088) iflat
 1088  format(i2,x,'N')
       endif
c        call plchmq( uuf,vvf,ctemp,1.,0.,0.)
        call gslwsc(1.0)
c       write(102,*) flat,flon,uu,vv,u2(l2,k2),v2(l2,k2)
       call WMSETR('WBA',70.)
       if(flat .lt. 0.) call WMSETR('WBA',-70.)
       if (l2 .eq. 0) l2 = 1
       call wmbarb(uu,vv,-u2(l2,k2),-V2(L2,K2))
 14    continue
          do 15,lat=-60,60,30
          do 15 lon=27,360,30
          flon=lon
          flat=lat
         call maptra(flat,flon,uu,vv)
       if(lat .lt. 0) then
        write(ctemp,1087)-lat
       else
       write(ctemp,1088) lat
       endif
       call gslwsc(4.0)
        call plchmq( uu,vv,ctemp,4.,0.,0.)
       call gslwsc(1.0)
 15    continue
          do 16,lat=-33,60,30
          do 16 lon=0,360,30
          flon=lon
          flat=lat
         call maptra(flat,flon,uu,vv)
       if( lon .lt. 180) then
        write(ctemp,1089)lon
 1089   format(i3,'E')
 1091   format(i3,'W')
       else
       write(ctemp,1091)(360-lon) 
       endif
       call gslwsc(4.0)
        call plchmq( uu,vv,ctemp,4.,0.,0.)
       call gslwsc(1.0)
 16    continue
       write(ctext,193)'TROPICAL SURFACE ANALYSIS  VALID'
       call DAYOWK(IRAS(8),IRAS(9),IRAS(10),idayy,cday)
c USAGE:    CALL DAYOWK(IDAY,IMONTH,IYEAR,IDAYWK,IHDAYW)
       write(ctext2,194)iras(7),cday,cmonth(iras(9)),iras(8),iras(10)
       write(88,1961)ctext,ctext2
       write(ctext3,196)'STREAM FUNCTION AND GRID WINDS'
        write(79,1962)ctext2
 1962  format(a30)
 196   format(a30)
 1961  format('STRIP ','      ',2a30)
 194   format(i2,'Z',x,a4,x,a3,x,i2,x,i4)
 193   format(a25,x,i2,'Z' ,i2,'/',i2,'/',i4)
       call maptra(28.,230.,uu,vv)
       call maptra(-12.,230.,uu2,vv2)
       call maptra(-32.,270.,uu3,vv3)
       call maptra(-32.,320.,uu4,vv4)
       call maptra(-32.,80.,uu5,vv5)
         call gslwsc(4.0)
        call gslwsc(1.0)
          do 17,lat=-37,60,10
          do 17 lon=0,360,10
          flon=lon
          flat=lat
         call maptra(flat,flon,uu,vv)
       if( lon .lt. 180) then
        write(ctemp,1089)lon
       else
       write(ctemp,1091)(360-lon)
       endif
       call plchmq( uu,vv,ctemp,1.,0.,0.)
       call gslwsc(1.0)
 17    continue
          do 18,lat=-60,60,10
          do 18 lon=2,360,10
          flon=lon
          flat=lat
         call maptra(flat,flon,uu,vv)
       if(lat .lt. 0) then
        write(ctemp,1087)-lat
       else
       write(ctemp,1088) lat
       endif
       call plchmq( uu,vv,ctemp,1.,0.,0.)
       call gslwsc(1.0)
 18    continue
       do 19,lon=1,360,1
       do 19,lat=-60,60,1
          flon=lon
          flat=lat
       call maptra(flat,flon,uu,vv)
c       call plchlq(uu,vv,'.',1.,0.,0.)
 19    continue
       call gsln(4)
c       call cg(z2,ix,iy,2)
       call gsln(1)
        call frame
 777    continue 
      call baclose(11,iret)
      call baclose(12,iret)
      call closen 
      CALL W3TAGE('TRPSFCMV') 
       stop
       end
c
c**************************************************************
c
       subroutine mpr(flat,flon) 
       call maptra(flat,flon,u,v)
       print 101,flat,flon,u,v
 101   format('lat lon ' , 4f15.5)
       return
       end
c
c**************************************************************
c
      subroutine qcnt(grid,ix,iy,rl,rl2,rl3,rl4,mtitle)
      character*132 mtitle
      integer (kind=8) ibb1,ibb2
      common/staa/ibitz
      Data ibb1/Z'F0F0F0F0F0f0ffff'/
      data ibb2/Z'F0F0F0F0F0f0aa55'/
      dimension grid(ix,iy)
      dimension rl(2),rl2(2),rl3(2),rl4(2)
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
c       dimension   RLAT1(2), RLAT2(2), RLON1(2), RLON2(2)
      dimension grid2 (ix,iy)
      common/icc/icount
       common/ctrxxx/junk1,jink2,iflag
       iflag=0
       Rmnlon=rl3(1)
       rmxlon=rl4(1)
       rmnlat=rl(1)
       rmxlat=rl2(1) 
       icount = 0        !modified for ibmsp
       do 1 k=1,ix
       do 1 j=1,iy
c       grid2(k,(iy+1-j))=(grid(k,j)) 
       grid2(k,j)=grid(k,j) 
c       grid2(k,j)=(grid(k,j)-273)*1.8 +32 
c       z(k,j)=k+j**2
 1     continue
      CALL GSCLIP (0)
C
C Draw Lat/Lon lines at 10 degree intervals.
C Draw political & continental outlines.
C
      CALL MAPSTI ('GR - GRID',90)
      CALL MAPSTC ('OU - OUTLINE DATASET','CO')
C
C Draw a Satellite view over the United States
C
       CALL MAPROJ ('ME - SATELLITE-VIEW',00.,-180.,00.)
c       CALL MAPROJ ('CE - SATELLITE-VIEW',0.,0.,00.)
c      CALL MAPSET ('MA',RLAT1,RLON1,RLAT2,RLON2)
c      call mapset('MA',rlatmin,rlonmin,rlatmax,rlonmax) 
C
C Don't draw a square around the globe
C
      CALL MAPSTI ('PE - PERIMETER FLAG', 0)
C
C Draw map.
C
c      call gslwsc(4.0)
c HEAVY HEAVY
      call setusv('LW',4000)
      CALL MAPDRW
c     read synoptic data from AFOS PLOTFILE
      call afosread
c     read satellite winds directly from bufr input on unit 10 
c      call satread
      call setusv('LW', 1000)
      CALL MAPSTI ('GR - GRID',10)
      CALL MAPSTC ('OU - OUTLINE DATASET','PS')
      CALL MAPDRW

      CALL CPSETI ('SET - DO SET-CALL FLAG',0)
        CALL CPSETR ('DPV - DASH PATTERN VECTOR SIZE',.0010)
         call DPSETI('PCF',0)
c      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',-30)

      CALL CPSETI ('MAP - MAPPING FLAG',1)
       call cpsetr('XC1',rmnlon)
       call cpsetr('XCM',rmxlon)
       call cpsetr('YC1',rmnlat)
       call cpsetr('YCN',rmxlat)
       ibitz=ibb1
c       call cg(grid2,ix,iy)   
       ibitz=ibb2
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
c      CALL GDAWK (1)
c      CALL GCLWK (1)
c       CALL GCLKS
c     CALL W3TAGE('TRPSFCMV') 
c      STOP
c      hunt for centers
       do 2 k=5,ix-4
       do 2 j=5,iy-4
       lat=-61+j
       lon=k-1
       gradck=2.2
       fmin=grid2(k,j)
       fmax=grid2(k,j)
       do 4 kk=k-4,k+4
       do 4 jj=j-4,j+4
       if(grid2(kk,jj) .lt. fmin) fmin=grid2(kk,jj)
       if(grid2(kk,jj) .gt. fmax) fmax=grid2(kk,jj)
c     1   grid2(k,j) .lt. grid2(k+1,j)-gradck  .and.
c     1   grid2(k,j) .lt. grid2(k-1,j)-gradck  .and.
c     1   grid2(k,j) .lt. grid2(k,j+1)-gradck  .and.
c     1   grid2(k,j) .lt. grid2(k,j-1)-gradck  .and.
c     2   grid2(k,j) .lt. grid2(k+2,j) .and.
c     2   grid2(k,j) .lt. grid2(k-2,j) .and.
c     2   grid2(k,j) .lt. grid2(k,j+2) .and.
c     2   grid2(k,j) .lt. grid2(k,j-2)
c     3   )    then
 4     continue
       if( (fmax-fmin)  .lt. gradck) go to 2
       if (fmin .eq. grid2(k,j) .or. fmax .eq. grid2(k,j)) then
       flat=lat
       flon=lon
       call maptra(flat,flon,uu,vv)
        if (flat .gt. 0.) then
       if (fmin .eq. grid2(k,j))  call plchhq(uu,vv,':F22:C',3.5,0.,0.)
       if (fmax .eq. grid2(k,j))  call plchhq(uu,vv,':F22:A',3.5,0.,0.)
       else
        if (fmin .eq. grid2(k,j))  call plchhq(uu,vv,':F22:A',3.5,0.,0.)
       if (fmax .eq. grid2(k,j))  call plchhq(uu,vv,':F22:C',3.5,0.,0.) 
       endif
       endif
 2     continue
      call satread
      return
      END
c
c**************************************************************
c
      SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
 
      DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)
 
      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10   CONTINUE
 
      IF (IAREA3 .GT. 0) THEN
cC
C If the area is defined by 3 or more points, fill it
C
         CALL GSFACI(IAREA3+2)
         CALL GFA(NWRK,XWRK,YWRK)
      ENDIF
 
C
C Otherwise, do nothing
C
      RETURN
      END
c
c**************************************************************
c
      subroutine cg(zreg,ix,iy,ifld)
C 
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      
      real ZREG(ix,iy)
      
      EXTERNAL COLOR
C
C Call Conpack color fill routine
       do k=1,ix
ckumar       print *,' zreg',(zreg(k,j),j=1,iy,5)      
       end do
C
      CALL CCPLLB(ZREG,ix,iy,COLOR,IWKID,ifld)
C
      
      RETURN
      END
c
c**************************************************************
c
      SUBROUTINE CCPLLB(ZREG,MREG,NREG,COLOR,IWKID,ifld)
      common/staa/ibitz
      common/windz/u2(360,121),v2(360,121)
       common/oth/u(70,40),v(70,40),rlats(65),rlons(65),up(65),vp(65)
      PARAMETER (LRWK=50000,LIWK=50000,LMAP=900000,NWRK=50000,NOGRPS=6)
      REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
      INTEGER MREG,NREG,IWRK(LIWK)
      INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)
      integer ibts(16,300)
      DATA IBTS / 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
     1   1600*0,
     1     1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
     2     1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
     3     1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
     4     1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
     5     1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1,
     6     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     7     160*0, 2928*0  /       ! modified for ibmsp
      EXTERNAL FILL
c      data ibitz/Z'F0F0F0F0F0F03333'/
      external sfill
      EXTERNAL CPDRPL
      EXTERNAL COLOR
C
C Set fill style to solid and turn off clipping
C
      CALL GSFAIS(1)
      CALL GSCLIP(0)
C
C Set up label box options
C
c      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',-30)
      NOCL=300
      CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',4)
c      CALL CPSETI('LLB - LINE LABEL BOX FLAG',4)
      CALL CPSETI('LLB - LINE LABEL BOX FLAG',3)
      CALL CPSETI('HLB - HIGH/LOW LABEL BOX FLAG',3)
c      CALL CPSETI('HLB - HIGH/LOW LABEL BOX FLAG',4)
      CALL CPSETI('ILB - INFORMATIONAL LABEL BOX FLAG',0)
      CALL CPSETI('LBC - LABEL BOX COLOR INDEX',1)
c       call cpseti('CIS',6)
cc        call cpseti('LIS',1)
        call cpseti('LIS',100)
      call cpseti('LLP',2)
      CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
      call cpsetr('RC1',0.15)
      call cpsetr('LLS',0.00002)
      call cpsetr('RC2',0.15)
      call cpsetr('LLW',0.00001)
c      call cpsetr('HLW',0.001)
c      call cpsetr('HLS',0.002)
      call cpsetr('HLW',0.0000001)
      call cpsetr('HLS',0.0000002)
      call cpsetc('HLT','A''C')

C
C Initialize Conpack

C
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Set up color table
C
      call cpsetc('HLT','A''C')

C
C Initialize Conpack

C
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Set up color table
C
      CALL CPPKCL (ZREG, RWRK, IWRK)
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCL)
      DO 111 I=1,NCL
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
         CALL CPSETI('AIA - AREA IDENTIFIER ABOVE',0)
         CALL CPSETI('AIB - AREA IDENTIFIER BELOW',0)
 111  CONTINUE
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
      CALL CPSETI('NCL - NUMBER OF CONTOUR LEVELS',NCL+2)
      CALL CPSETI ('PAI',ncl+1)
       call cpsetr('CLV',0.0)
       call cpseti('AIB',5)
       call cpseti('AIA',0)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
       if(zreg(50,50) .lt. 500) then
c      CALL CPSETI ('PAI',ncl+2)
c       call cpsetr('CLV',15.0)
c       call cpseti('AIB',0)
c       call cpseti('AIA',6)
        endif
c       call cpsetr('CLV',55.0)
c       call cpseti('AIB',6)
c       call cpseti('AIA',0)
      DO 11 I=1,ncl
        ivarv=ibitz
         print 198,ivarv
 198     format(z16)
c         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',i)
c         CALL CPSETI('CLD - CONTOUR LINE DASH PATTERN',
c     +        ivarv)
 11   continue
      CALL COLOR(NCL+1,IWKID)
      call PCSETI('CC',0)
        

C
C Draw Perimeter
C
c      CALL CPBACK(ZREG, RWRK, IWRK)
       call gslwsc(1.0)
C
C Initialize Areas
C
cc      CALL ARINAM(MAP, LMAP)
C
C Add label boxes to area map
C
c      if(zreg(50,50) .lt. 50.)  
      if(ifld .eq. 2)
     1 call gsln(1)
c cc     CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C
C Draw Labels
C
      call gsln(0)
c      CALL CPLBDR(ZREG, RWRK, IWRK)
C
C Add contours to area map
C
c      CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
      call gslwsc(3.0)
c      if(zreg(50,50) .lt. 50.)  
      if(ifld .eq. 2)
     1 call gsln(4)
       call cpcldr(zreg,rwrk,iwrk,map,cpdrpl)
c NEW LINE
       call gslwsc(2.0)
       call cplbdr(zreg,rwrk,iwrk) 
       
c       call cpcldm(zreg,rwrk,iwrk,map,cpdrpl)
c       call gsln (1)
C
C Fill contours
C
c      CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, SFILL)
 99       continue 
      CALL WMSETR('WBS',0.004)
      CALL WMSETI('COL',1)
      CALL WMGETR('WBS',WSLEN)
      RETURN
      END
c
c**************************************************************
c
      SUBROUTINE COLOR (N,IWKID)
C
C BACKGROUND COLOR
C
C
C BLACK
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C
C First foreground color is white
C
      CALL GSCR(IWKID,1,1.,1.,1.)
C
C Second foreground color is gray
C
      CALL GSCR(IWKID,2,0.5,0.5,0.5)
C
C Choose other foreground colors spaced equally around the spectrum
C
      ICNT=0
      HUES=360./N
C
C REDLN is intended to be the line between red and violet values
C
      REDLN=36.0
      LAP=INT(REDLN/HUES)
      DO 10, I=1,N
         XHUE=I*HUES
         CALL HLSRGB(XHUE,60.,75.,RED,GREEN,BLUE)
C
C Sort colors so that the redest is first, and violetest is last
C
         IF (XHUE.LE.REDLN) THEN
            CALL GSCR(IWKID,(N+2)-(LAP-I),RED,GREEN,BLUE)
            ICNT=ICNT+1
         ELSE
            CALL GSCR(IWKID,I-ICNT+2,RED,GREEN,BLUE)
         ENDIF
 10   CONTINUE
      
      RETURN
      END
c
c**************************************************************
c
           SUBROUTINE SFILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
      REAL XWRK(*),YWRK(*),ISCR(5000)
      INTEGER IAREA(*),IGRP(*),RSCR(5000)
 
      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10   CONTINUE
 
      IF (IAREA3 .eq. 5) THEN
C
C If the area is defined by 3 or more points, fill it
C
         CALL SFSETR('SPACING',.006)
         CALL SFSETR('AN', 0.)
         CALL SFNORM(XWRK,YWRK,NWRK,RSCR,5000,ISCR,5000)
      ENDIF
      IF (IAREA3 .eq. 6) THEN
C
C If the area is defined by 3 or more points, fill it
C
         CALL SFSETR('SPACING',.006)
         CALL SFSETR('AN', 90.)
         CALL SFNORM(XWRK,YWRK,NWRK,RSCR,5000,ISCR,5000)
      ENDIF
C  
C Otherwise, do nothing
C
      RETURN
      END
c
c**************************************************************
c
      subroutine i3to53(F3,F53)
      parameter(ji=360*181)
      parameter(ig53=53,jo53=117*51)
      dimension F3(ji),F53(jo53)
C
      real rlat_03(ji),rlon_03(ji)
      logical lo_03(ji)
C
      real rlat_53(ji),rlon_53(ji)
      equivalence( rlat_53(1), rlat_03(1) )
      equivalence( rlon_53(1), rlon_03(1) )
      logical lo_53(ji)
      equivalence( lo_53(1), lo_03(1) )
C
      integer ibi,ibo
      integer         kgdsi(22)
C
C
      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42),gdsi(42)
      INTEGER      LENGDS
      ibi=0
cc           define 360x181  grid
            call makgds(3,kgdsi,gdsi,lengds,iret)
cc           define 117x51 grid
            call makgds(ig53,kgdso,gdso,lengds,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'makgd' 
            endif
C
            ipopt=0
            ip = 0
            ji2=ji
            call ipolates(ip,ipopt,kgdsi,kgdso,ji,ji2,1,
     1                 ibi,lo_03,F3,ko,rlat_53,rlon_53,
     2                 ibo,lo_53,F53,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'ipol' 
            endif
         return
       end
c
c**************************************************************
c
      subroutine i53to3(F53,F3)
      parameter(ji=360*181)
      parameter(ig53=53,jo53=117*51)
      dimension F3(ji),F53(jo53)
C
      real rlat_03(ji),rlon_03(ji)
      logical lo_03(ji)
C
      real rlat_53(ji),rlon_53(ji)
      equivalence( rlat_53(1), rlat_03(1) )
      equivalence( rlon_53(1), rlon_03(1) )
      logical lo_53(ji)
      logical log3(360,181)
      equivalence (lo_03,log3)
      equivalence( lo_53(1), lo_03(1) )
C
      integer ibi,ibo
      integer         kgdsi(22)
C
C
      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42),gdsi(42)
      INTEGER      LENGDS

      do k=1,360
      log3(k,1)=.FALSE.
      log3(k,181)=.FALSE.
      end do
      ibi=0
cc           define 360x181  grid
            call makgds(53,kgdsi,gdsi,lengds,iret)
cc           define 117x51 grid
            call makgds(3,kgdso,gdso,lengds,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'makgd'
            endif
C
            ipopt=0
            ip = 0
            ji2=ji
            call ipolates(ip,ipopt,kgdsi,kgdso,jo53,ji2,1,
     1                 ibi,lo_03,F53,ko,rlat_53,rlon_53,
     2                 ibo,lo_53,F3,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'ipol'
            endif
         return
       end
c
c**************************************************************
c
      subroutine i3to8(F3,F53)
      parameter(ji=360*181)
      parameter(ig53=53,jo53=117*51)
      dimension F3(ji),F53(116,44)
C
      real rlat_03(ji),rlon_03(ji)
      logical lo_03(ji)
C
      real rlat_53(ji),rlon_53(ji)
      equivalence( rlat_53(1), rlat_03(1) )
      equivalence( rlon_53(1), rlon_03(1) )
      logical lo_53(ji)
      equivalence( lo_53(1), lo_03(1) )
C
      integer ibi,ibo
      integer         kgdsi(22)
C
C
      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42),gdsi(42)
      INTEGER      LENGDS
      ibi=0
cc           define 360x181  grid
            call makgds(3,kgdsi,gdsi,lengds,iret)
cc           define 117x51 grid
            call makgds(8,kgdso,gdso,lengds,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'makgd'
            endif
C
            ipopt=0
            ip = 0
            ji2=ji
            call ipolates(ip,ipopt,kgdsi,kgdso,ji,ji2,1,
     1                 ibi,lo_03,F3,ko,rlat_53,rlon_53,
     2                 ibo,lo_53,F53,iret)
            if(iret.ne.0) then
      CALL W3TAGE('TRPSFCMV') 
               stop 'ipol'
            endif
         return
       end
c
c**************************************************************
c
      subroutine closen
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
       CALL GCLKS
       return
       end
c
c**************************************************************
c
       subroutine openn
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
      return
       end
c
c**************************************************************
c
      Subroutine get3(lun,luni,jpds,a2,c132)
      common/grbpds/kpds
      COMMON/PACKRA/IRAS(10)
c  array is an NMC GRIB TYPE 3 field on output
c   jpds is set in the CALLER!! except for fields 1-3
      dimension array(360,181),a2(360,181)
      dimension jpds(25),jgds(25)
      dimension grib(360,181)
      dimension kpds(25),kgds(25)
      character*132 c132
      logical lb(360,181)
      jf=360*181
      jflag=-1 
      kf=jf
      do i = 1, 25
         kpds(i) = 0
         kgds(i) = 0
      end do
      k = 0
      ier = 0
c      jpds(2)=77
      jpds(1)=7
c      jpds(3)=104
      jpds(3)=3
        print 101,lun,luni
       print 109,(jpds(ll),ll=1,25)
 109   format(5z17)
      call getgb1(lun,luni,jf,jflag,jpds,jgds,
     1 grib,kf,k,kpds,kgds,lb,array,ier)
c      if ( ier .ne. 0) then
c      CALL W3TAGE('TRPSFCMV') 
c          call errexit(99)
c       endif
       call w3fp11(grib,grib(2,1),c132,ierr)
      print 106,k,c132
 106  format(i6, 'LAB ',a132)
      print 101,ier,k,kf
      if(ier .ne.0 ) then
      CALL W3TAGE('TRPSFCMV') 
         call errexit(9999)
      endif
      print 102,(array(90,k),k=1,181)
 102  format(10f8.2)
 101   format(i9)
cj       do 45,k=1,181
c         do 45,j=1,360
c 45        if(j .gt. 70  .or. k .gt. 70) array(j,k)=array(70,70)
c  SET IRAS VARIABLES
      iras(1)=kpds(14)
      iras(7)=kpds(11)
      iras(8)=kpds(10)
      iras(9)=kpds(9)
C  This should generate the proper 4-digit year no matter what!!
       mcen=max(0,kpds(21)-1)
       iras(10)=mcen*100+kpds(8)
       if(iras(10) .le. 20) then
          iras(10)=iras(10)+2000
       else  if(iras(10).le.99)  then
          iras(10)=iras(10)+1900
       end if
c      iras(3)=iras(7)
c      iras(4)=iras(8)
c       iras(5)=iras(9)
c       iras(6)=iras(10)
c       flip type 3 grid
       do k=1,181
       do j=1,360
c       a2(j,182-k)=array(j,k)
       a2(j,k)=array(j,k)
       end do
       end do
       return
       end
c
c**************************************************************
c
      Subroutine stream3(mbars,itime,lupgb,lupgi,F3)
c
c    George VandenBerghe 11/17/96
c
c      subroutine to accept pressure level (mbars),
c     grib unit number (lupgb), and grib index unit
c    number (lupgi) and read u, and v wind fields,
c   at mbars, and calculate stream function.  Stream
c    function is stored in F3 and returned to caller.
c    The Grib fields on lupgb MUST be grib type 3 (360x181)
c    fields.  These are output automatically by the NCEP
c   GFS forecast model.  
c     These stream functions are dimensionally correct
c     m**2/sec streamfunctions 
c   11/17/96  add on !!
c      logic added to handle tropopause level.  Passing
c    in pressure as 7 mbars tells code to wildcard
c    pressure and take tropopause level values
      parameter(im=360,jm=181,km=16)
      dimension F3(im,jm)
      dimension iprs(km)
      integer kpds(100), kgds(100)
      integer jpds(100), jgds(100)
      logical lbms(im*jm), luv, ldz, lps
      real u(im,jm), v(im,jm), psi(im,jm),chi(im,jm)
      data iromb/0/,maxwv/126/,idrti/0/,imaxi/360/,jmaxi/181/
      data idrto/0/,imaxo/360/,jmaxo/181/,kmax/1/
      data iprime/0/,iskipi/0/,jskipi/0/,kspipi/0/
      data iskipo/0/,jskipo/0/,kspipo/0/,jcpu/0/
c --- coordinate testing code
      x1=50
      y1=60
      x2=180
      y2=60
      x3=180
      y3=120

      luv = .False.
      ldz = .False.
      lps = .True.
      ijm = im * jm
      fim = im
      lskip=-1
      k=0
      do jj = 1, 100
        jpds(jj) = -1
      enddo
      do jj = 1, 20
        jgds(jj) = -1
      enddo
        jpds(5) = 33
        jpds(6) = 100
        jpds(7) =mbars
        jpds(14)=itime
c  special code for troposphere.  If mbars is set
c to seven, ignore pressure level and take tropopause
c values
         if(mbars .eq. 7) then
           jpds(6)=7
           jpds(7)=-1
           endif
c end special  trop code
        call getgb(lupgb,lupgi,ijm,-1,jpds,jgds,ndata,
     &  lskip,kpds,kgds,lbms,u,iret)
c         print *,' KPDS AFTER U'
c        print 189,kpds
        if(iret.ne.0) then
        endif
        jpds(5) = 34
        jpds(6) = 100
        jpds(7) = mbars
c  special code for troposphere.  If mbars is set
c to seven, ignore pressure level and take tropopause
c values
         if(mbars .eq. 7) then
           jpds(6)=7
           jpds(7)=-1
           endif
c end special  trop code
        call getgb(lupgb,lupgi,ijm,-1,jpds,jgds,ndata,
     &  lskip,kpds,kgds,lbms,v,iret)
        if(iret.ne.0) then
        endif
        call     SPTRUNV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,
     &                   IDRTO,IMAXO,JMAXO,KMAX,
     &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,
     &                   ISKIPO,JSKIPO,KSKIPO,JCPU,U,V,
     &                   LUV,GRIDUO,GRIDVO,LDZ,GRIDDO,GRIDZO,
     &                   LPS,chi,psi)
        print 109,k
 109   format(' K is ',i9)
c
        do j=1,jm
        do k=1,im
           f3(k,j)=psi(k,j)  
        end do
        end do
       kpds(5)=35
c         print *,' KPDS to be put'
c        print 189,kpds
 189   format(5Z20) 
c   WRITE THE STREAMFUNCTIONS TO UNIT  51!!
c       call putgb(51,ijm,kpds,kgds,lbms,f3,iret)
c    end write
       if(iret .ne. 0) print *,
     1 ' WARNING PUT OF STREAMFUNCTION FAILED return code ', iret,
     1  ' from putgb '
        print *,' FEW STREAMS',psi(50,50),psi(100,100)
 990  continue
      return
      end
c
c**************************************************************
c
       subroutine afosread
c      field 4 time (4 digits)
c      field 5 name ( 5 digits)
c      field 6 sky cover i A aircraft S sat M missing
c7      field 7 wind ddfff
c        field 8 pressure (mb*10. significant digits)
c        field 9,10  temp and dew
c      11 present weather (code or text)
c      12 visibility
c      13 pressure change
c      14 barograoh trace (code table 12)
c      15 low/middle/high cloud
c      16 precip
c  17  remarks
c      lat lon are 18 and 19
c     20 ship course and speed
       dimension points(100000,2)
       character*160  cline
         character*160 cout
       integer icom(40)
       character*8 ccom(40),cf
c
       do nn=1,2
          do mm=1,100000
             points(mm,nn)=0
          enddo
       enddo
c
       call ridctlm 
       do k=1,20
       ccom(k)='        '
       end do
c
       open(44,file='afosplot')
c
c       do jjj=1,2
c          read(44,101)cline
c       enddo
c
       do 10000 kkk=1,100000
 101   format(a160)
       read(44,101,end=999,err=997) cline 
       write(85,101) cline
c       print 101,cline
c      find the commas
       idx=1
       is=1
       cf='        '
       ico=0
       do 10,k=1,160
       if(cline(k:k) .eq. ',' .or.cline(k:k).eq.';') then
       ico=ico+1
c
       if(ico.ge.40) then
       print *,' MORE THAN 40 COMMAS in line ',kkk
       go to 10000
       endif
c
       icom(ico)=k
       ccom(ico)=cf
       cf='        '
       idx=1
       if(cline(k:k) .eq. ';') go to 19
       else
       cf(idx:idx)=cline(k:k) 
       idx=idx+1
c
       if(idx .gt. 8) then
       print *,' MORE THAN  8 blanks between commas in line ',kkk
       go to 10000
       endif
c
       endif
 10    continue
 19    continue
       do  k=1,20
       if(ccom(k) .eq. '        ') ccom(k)='-9999   '
       end do
       do k=1,5
       if(ccom(15)(k:k).eq.'/') ccom(15)(k:k)='0'
        end do
       if(ccom(6)(8:8).eq. 'M') ccom(6)='-9999   '
       if(ccom(6)(1:1).eq. 'M') ccom(6)='-9999   '
       if (kkk .lt. 5) print 129,ccom
 119   format(i3,a3,20i3)
       read(ccom(7),179)id,isp
 179   format(i3,i2)
       read(ccom(9),149)itemp
       read(ccom(6),149) icover
       read(ccom(11),149) iwx
        if(iwx .gt.99 .or. iwx .lt.0) iwx=-9999
       print *,' ICOVERC',icover
       read(ccom(10),149)idew
       read(ccom(18),159)ilat
       read(ccom(15),149)icloud
       if(icloud .gt. 0) print *,' ICLUD',icloud
       read(ccom(19),169)ilon
       read(ccom(8),149) ipres
       read(ccom(14),149)itrace
       read(ccom(13),149) itend
       if (ccom(18)(4:4) .eq. 'S') ilat=-ilat
       if(ccom(19)(5:5) .eq.'W') ilon=-ilon+3600.
        flat=(ilat+0.5)/10.
        flon=(ilon+0.5)/10.
        call maptra(flat,flon,uu,vv)
       if(id.eq.-99)id=-999
       dir=id
       theta=(90-dir)*3.1415927/180.    
       u=cos(theta)*isp
       v=sin(theta)*isp
       dir=dir-90.
       if(dir .le. 0) dir=dir+360
       if(flat .lt. 0) dir=-dir
       id=dir
       ilat=flat 
       ilon=floN
       points(kkk,1)=400. 
       points(kkk,2)=400.
       do 39,l=1,kkk-1
       xd=(points(l,1)-ilat)*2.0
       yd=points(l,2)-ilon
       sargg=xd**2 + yd**2
c           write(102,*)' square root  ',kkk,sargg
       if(sargg .lt. 0.) then
         write(102,*)' negative square root potential ',kkk,sargg
         go to 10000
       endif
c
        dist=sqrt(xd**2 + yd**2)
c       dist=sqrt( (points(l,1)-ilat)**2 + (points(l,2)-ilon)**2) 
       if (dist .lt. 3.9) go to 10000
       if(ccom(3)(1:1) .eq. '7') go to 10000
c       if (dist .lt. 0.5) go to 10000
 39    continue 
       points(kkk,1)=ilat
       points(kkk,2)=ilon
 149   format(i8)
 159   format(i3)
 169   format(i4)
       print *,'STATION TEMPS ',itemp,idew,' WIND ',id,isp,ilat,ilon,iwx
       if(iwx .gt. 0) 
     1 print *,
     1 'WX STATION TEMPS ',itemp,idew,' WIND ',id,isp,ilat,ilon,iwx
       call l2p(flat,flon,iprx,ipry)
       if(iprx .lt. -9999. .or. iprx .gt. 9999 .or.
     1   ipry .lt. -9999 . .or. ipry .gt. 9999)
     1   go to  10000
       write(89,105)'STATION PLOT',
     1 iprx,ipry,itemp,idew,ipres,itend,id,isp,icover,iwx,itrace,icloud
     1 ,ccom(5)
     1 ,ccom(18),ccom(19)
c     1 iprx,ipry,itemp,idew,ipres,itend,id,isp
c     1 iprx,ipry,itemp,idew,ilon,ilat,id,isp,icover,ccom(5)
 105    format(a16,12i5,x,3a9)
 139   format(96x,2i8)
 129   format(20a8)
10000  continue
       print *,'WARNING, INPUT AFOS PLOTFILE NOT EXHAUSTED'
 997   continue
c       print *,' ENDED DATA WITH ERROR'
 999   continue
       return
       end
c
c**************************************************************
c
      subroutine l2p(flat,flon,iprx,ipry)
      call maptra(flat,flon,uu,vv)
      fx=cufx(uu)
      fy=cufy(vv)
      px=6912*fx
c1      py=-6912*(fy-.625555555)
      py=-6912*(fy-.575555555)
      iprx=py
      ipry=px 
      print *,'L2P ', 'flat,flon,px,py,iprx,ipry',
     1 flat,flon,px,py,iprx,ipry
      return
      end  
c
c**************************************************************
c
         subroutine ridctlm
c        remove all ctlm characters from fort.43 and write to fort.87
         dimension jstat(100)
c         is=stat('fort.43',jstat)
c         lnn=jstat(8)
          read(5,101)lnn
 101      format(i10)
         print*,'file size of fort.43   ',lnn
         call afix(lnn)
         return
         end
c
c**************************************************************
c
         subroutine afix(lnn)
         character*1 bytes(lnn)
         integer (kind=8) ip
         data ip/Z'0D'/          ! CTRL-M
         character*1 c1(8)
         equivalence(c1,ip)
ckumar
         open(43,file='NHPLOT',access='direct',recl=1280)
c
         na=1 ; nb=1280
c
         nrec=lnn/1280
         print*,'In afix lnn & nrec  :   ',lnn,nrec
c
         do ir=1,nrec
            read(43,rec=ir)bytes(na:nb)
            na=na+1280
            nb=nb+1280
ckumar            print*,'In afix ir, na & nb ',ir,na,nb
         enddo
c
         do 10,k=1,lnn
         if(bytes(k) .eq. c1(8)) bytes(k)=' '
 10      continue
c
         write(87) bytes
         close(87)
         return
         end
c
c**************************************************************
c

C          DATA SET DAYOWK     AT LEVEL 001 AS OF 04/16/93
      SUBROUTINE DAYOWK(IDAY,IMONTH,IYEAR,IDAYWK,IHDAYW)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  DAYOWK        FIND NUMERICAL DAY OF WEEK.
C   PRGMMR: HENRICHSEN       ORG: NMC41       DATE:93-05-11
C
C ABSTRACT: GIVEN DAY MONTH AND YEAR AS INTEGERS RETURN DAY OF WEEK AS
C   AND INTEGER NUMBER AND AS FOUR CHARACTER HOLLERTH TEXT.
C
C PROGRAM HISTORY LOG:
C   84-MM-DD  WICK
C   85-MM-DD  HENRICHSEN REMOVE OLD KEY PUNCH CHARACTERS AND CLEAN UP.
C   87-12-10  HENRICHSEN CONVERT TO VS FORTAN 77.
C
C USAGE:    CALL DAYOWK(IDAY,IMONTH,IYEAR,IDAYWK,IHDAYW)
C   INPUT ARGUMENT LIST:
C     IDAY     - INTEGER TWO DIGET DAY OF MONTH( 1 THRU 31 ).
C     IMONTH   - INTEGER TWO DIGET MONTH OF YEAR( 1 THRU 12).
C     IYEAR    - INTEGER TWO OR FOUR DIGET YEAR.
C                IYEAR SHOULD BE THE COMPLETE FOUR DIGIT YEAR.
C                IF ONLY LAST 2 DIGITS OF YEAR ARE GIVEN,
C                ASSUMES 20TH CENTURY...
C
C   OUTPUT ARGUMENT LIST:
C     IDAYWK   - INTEGER NUMBER OF DAY OF WEEK ( 1 THRU 7 ).
C     IHDAYW   - CHARACTER*4 WORD CONTAINING THE THE THREE LETTER
C              - DAY OF THE WEEK LEFT JUSTIFED IN THE WORD.
C              - IE 'MON ', .... 'FRI ', .... 'SUN '.
C     ERRFLAG  - ERROR OF OUT OF RANGE ARGUEMENT IS INDICATED BY THE
C              - RESULTING IDAYWK = 0, AND IHDAYW  = BLANKS.
C
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     FT06F001 - ERROR PRINT WHEN ARGUEMENTS OUT OF RANGE.
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
      CHARACTER*4   IHDAYW
      CHARACTER*4   ITEXT(2)
      CHARACTER*4   KHDAY(7)
      CHARACTER*4   KLANK
      CHARACTER*4   NHDAY(2)
      CHARACTER*4   NHMON(2)
      CHARACTER*4   NHYR(2)
C
      INTEGER     KCEN(5)
      INTEGER     KDAYS(12)
      INTEGER     MONTAB(12)
C
      DATA          KHDAY   /'SUN ','MON','TUE ',
     1                       'WED ','THU ','FRI ','SAT '/
      DATA          KLANK   /'    '/
      DATA          NHDAY   /'DAY ','    '/
      DATA          NHMON   /'MONT','H   '/
      DATA          NHYR    /'YEAR','    '/
C
      DATA        KCEN     /4,2,0,6,4/
      DATA        KDAYS    /31,29,31,30,31,30,31,31,30,31,30,31/
      DATA        MONTAB   /1, 4, 4, 0, 2, 5, 0, 3, 6, 1, 4, 6/
C
      IDAYWK = 0
      IHDAYW = KLANK
      ISFEB = 0
      IDA = IDAY
      IMO = IMONTH
      IYR = IYEAR
      IF(IMO) 911,911,122
  122 IF(IMO .GT. 12) GO TO 911
      IF(IDA) 922,922,133
  133 IF(IDA .GT. KDAYS(IMO)) GO TO 922
      ICENT = IYR / 100
      IYR2 = IYR - 100 * ICENT
      IF(ICENT) 933,140,144
  140 CONTINUE
C     ...IF ONLY LAST 2 DIGITS OF YR WERE GIVEN, ASSUME ITS 1900 +
      ICENT = 19
      IYR = IYR + 1900
  144 CONTINUE
      ICENTX = ICENT - 16
      IF(ICENTX) 933,933,155
  155 IF(ICENTX .GT. 5) GO TO 933
C     ...THE GIVEN DATE INFO IS W/I RANGE...
      ISUM = KCEN(ICENTX) + IYR2 +IYR2/4 + MONTAB(IMO) + IDA
      IF(IMO - 2) 222,211,611
C     ...OTHERWISE, THIS IS JAN OR FEB, SO CHECK FOR LEAP YR...
  211 ISFEB = 1
  222 CONTINUE
      IF(MOD(IYR,4)) 533,511,533
C     ...MOST LIKELY  A LEAP YR.  TEST FOR CENTURY YR...
  511 CONTINUE
      IF(IYR2)522,515,522
  515 CONTINUE
      IF(MOD(IYR,400))533,522,533
C     ...COMES TO 522 IF LEAP YR CORRECTION IS NEEDED...
  522 CONTINUE
      ISUM = ISUM - 1
      GO TO 611
  533 CONTINUE
C     ...COMES TO 533 IF NOT A LEAP YR, SO  IF FEB, MUST RETEST IDA...
      IF(ISFEB)544,611,544
  544 CONTINUE
C     ...THIS IS FEB OF A NON-LEAP YR.
      IF (IDA .GE. KDAYS(2)) GO TO 922
      GO TO 611
  611 CONTINUE
      IDAW = MOD(ISUM,7)
      IF(IDAW) 644,633,644
  633 CONTINUE
      IDAW = 7
      GO TO 644
  644 CONTINUE
      IDAYWK = IDAW
      IHDAYW = KHDAY(IDAW)
      RETURN
C
  911 CONTINUE
C     ...COMES HERE IF GIVEN MONTH OUT-OF-RANGE...
      ITEXT(1) = NHMON(1)
      ITEXT(2) = NHMON(2)
      GO TO 955
  922 CONTINUE
C     ...COMES HERE IF GIVEN DAY OUT-OF-RANGE
      ITEXT(1) = NHDAY(1)
      ITEXT(2) = NHDAY(2)
      GO TO 955
  933 CONTINUE
C     ...COMES HERE IF GIVEN YR OUT OF RANGE
      ITEXT(1) = NHYR(1)
      ITEXT(2) = NHYR(2)
      GO TO 955
  955 CONTINUE
      PRINT  956, ITEXT(1),ITEXT(2),IDAY,IMONTH,IYEAR
  956 FORMAT(1H0,10X,'ERROR EXIT FROM DAYOWK. GIVEN ', A4, A1, 1X,
     X       'OUT-OF-RANGE', /1H0,15X,'IDAY = Z', Z8, 4X,'IMONTH = Z',
     X       Z8, 4X, 'IYEAR = Z', Z8)
      RETURN
      END
c
c**************************************************************
c
       subroutine satread 
c  READS SATELLITE WIND DATA AND DRAWS WIND BARBS
c  ON ANY NCAR GRAPHICS PLOTTING FRAME WITH  LATLON 
c  FRAME MAPPING DEFINED (general for weather plots)  
       dimension ary(6)  
      CALL WMSETR('WBS',0.003)
      CALL WMSETR('WBD',0.17)
      CALL WMSETI('COL',1)
      CALL WMGETR('WBS',WSLEN)
       do 10,k=1,9999999
       read(38,101,end=99)ary
 101   format(6x,6f10.4)
       flat=ary(1)
       flon=ary(2)
       dir=ary(4)
       speed=ary(5)
       qcm=ary(6)
       u=speed*sin(dir*3.1415927/180.)
       v=speed*cos(dir*3.1415927/180.)
       call maptra(flat,flon,uu,vv)
c       print *,'calling wmbarb',uu,vv,u,v
        call WMSETR('WBA',70.)
       if(flat .lt. 0.) call WMSETR('WBA',-70.)
       call wmbarb(uu,vv,u,v)
       call plchhq(uu,vv,':F19:c',1.5,0.,0.)
c       call plchhq(uu,vv,':F22:C',9.5,0.,0.)
 10    continue
 99    continue
       return
       end
c
c**************************************************************
c
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .  
C SUBPROGRAM:    REDSAT      READ UPPER AIR BUFR FILE                   
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 97-06-03             
C                                                                       
C ABSTRACT: REDSAT READS THE PREPBUFR OBSERVATIONAL FILE AND            
C   RETURNS A SINGLE SATWND REPORT IN ARRAY DATA                        
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   97-06-03  LARRY SAGER                                               
C                                                                       
C USAGE:    CALL REDSAT  (RLIMS, IUNS, DATA, IRET)
C   INPUT ARGUMENT LIST:                                                
C     RLIMS    - LAT/LONG LIMITS TO DUMP
C     IUNS     - UNIT NUMBER OF PREPBUFR FILE
C                                                                       
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                  
C     DATA     - THE RETURNED SATWND REPORT.                            
C              - LAT, LONG, PRESALT, WDIR, WSP, WQM                     
C     IRET     - FLAG:   IRET=0  NORMAL RETURN
C                        IRET=-1 END OF FILE    
C                                                                       
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)             
C     FT55F001 - PREPBUFR UPPER AIR DATA FILE                           
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C                                                                       
C$$$                                                                    
      SUBROUTINE REDSAT(RLIMS, IUNS, DATA, IRET)
C
C     REDUPA READS THE PREPBUFR FILE AND RETURNS AN UNPACKED      
C     SATWND REPORT.
C
      CHARACTER*8   INOUT
      CHARACTER*8   SUBSET
      CHARACTER*8   CTEMP
      CHARACTER*40  CATH, STRING        
C
      DIMENSION     HDR (10)
C
      REAL          CRR (5,255)
      REAL          RLIMS (4)
      REAL          DATA(6)
C 
      EQUIVALENCE   (CTEMP,ITEMP)
      EQUIVALENCE   (RTEMP,ITEMP)
C
      DATA STRING  /'SID XOB YOB DHR ELV TYP SQN'/
      DATA CATH    /'CAT=6 POB DDO FFO WQM'/
      DATA IX      /0/
C
C----------------------------------------------------------------------|
C   OPEN THE IBM-SP BUFR TANK FILE                                     |
C----------------------------------------------------------------------|
C
      SAVE 
      IRET = 0
      IMASS = 0
      IWIND = 0
      LUBFR = IUNS
      IF( IX .EQ. 0) THEN
        IX = 1
        INOUT = 'IN'
        LUNDX = 10 
        CALL  OPENBF( LUBFR, INOUT, LUNDX )
 10     CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
        IF(IRET1 .NE. 0) GOTO 50
        ITYP = 0
        IF(SUBSET .NE. 'SATWND' ) GOTO 10                    
      ENDIF
C
C----------------------------------------------------------------------|
C     READ THE BUFR DATA TANKS
C----------------------------------------------------------------------|
C
 20   CALL READSB (LUBFR,IRET2)
      IF (IRET2 .LT. 0) THEN    
 22        CALL READMG(LUBFR, SUBSET, IDATE, IRET1)
           IF (IRET1 .LT. 0) GOTO 50
           IF(SUBSET .EQ. 'SATWND' ) GOTO 20
           GOTO 22
      END IF
C
C----------------------------------------------------------------------|
C     READ IN THE STATION INFORMATION                                  |
C----------------------------------------------------------------------|
C
      CALL UFBINT (LUBFR, HDR, 10, 1, NRET, STRING)
C     PRINT 119,HDR(1),HDR(6)
 119  FORMAT(' STATION ',A8,' subtype ',F10.1)
C
C     MAKE SURE THIS STATION IS WITHIN THE LAT/LONG LIMITS
C
      IF((HDR(3) .GT. RLIMS(1)) .OR. (HDR(3) .LT. RLIMS(2)))
     1    GOTO 20
      IF((HDR(2) .LT. RLIMS(3)) .OR. (HDR(2) .GT. RLIMS(4)))
     1    GOTO 20 
C
C----------------------------------------------------------------------|
C     UNPACK THE BUFR REPORT
C----------------------------------------------------------------------|
C
      IF((HDR(6) .GE. 240.) .AND. (HDR(6) .LE. 246.))THEN 
          CALL UFBINT (LUBFR, CRR, 5, 255, NRET2, CATH)
C         PRINT *,'SATWNDS ',CRR(2,1),CRR(3,1)
          DATA(1) = HDR(3)    
          DATA(2) = HDR(2)    
          DATA(3) = CRR(1,1)  
          DATA(4) = CRR(2,1)
          DATA(5) = CRR(3,1)
          DATA(6) = CRR(4,1)
C         PRINT *,' DATA ',(DATA(KK),KK=1,6)            
      ELSE
          GOTO 20
      END IF
      RETURN
C
  50  IRET = -1
      RETURN
      END
c
c**************************************************************
c
      subroutine hbull
c     reads quality controlled hurricane bulletins from either:
c               $COMIN/gblav.$cycle.syndata.tcvitals.tm00 for GFS
c               $COMIN/gdas1.$cycle.syndata.tcvitals.tm00 for FNL
c     and writes putlab call information and box drawing
c     information for use downstream by bedient packing
c     and drawing program sixbitb2.  Input is fortran
c     unit 31 and output is fortran unit 74. 

C 2001-02-09  Dennis Keyser        Updated subroutine HBULL to handle
C                     tcvitals records with 4-digit year (and to still
C                     work properly for bulletins with a 4-digit
C                     year), this had not been working properly since
C                     late 1999 when 4-digit years were written to the
C                     tcvitals file (always stamped out "NO TROPICAL
C                     CYCLONE ACTIVITY REPORTED"); also changed HBULL
C                     to recognize expanded test storm id range (now
C                     80-99, was 90-99) implemented by TPC.


      COMMON/PACKRA/IRAS(10)

      character*10 names(16)
      character*128 clines(100)
      character*128 cline
      character*128 dumy2k
      character*10 cname
      character*13 cdate
      character*2 c2
      character*80 cout,cnull

      print *, ' '
      print *, ' ===> ENTERING SUBROUTINE HBULL'
      print *, ' '

      write(c2,'(i2.2)') iras(7)

      cnull='NO TROPICAL CYCLONE ACTIVITY REPORTED'
      icount=0
      icounta=0
      klines=0
      names='xxxxxxxxxx'

      do k=1,100  
         read(31,fmt='(a128)',end=9) cline

C AT THIS POINT WE DO NOT KNOW IF A 2-DIGIT YEAR BEGINS IN COLUMN 20
C  OF THE RECORD (OLD NON-Y2K COMPLIANT FORM) OR IF A 4-DIGIT YEAR
C  BEGINS IN COLUMN 20 (NEW Y2K COMPLIANT FORM) - TEST ON LOCATION OF
C  LATITUDE N/S INDICATOR TO FIND OUT ...

         if(cline(35:35).eq.'N' .or.
     .      cline(35:35).eq.'S')  then

C ... THIS RECORD STILL CONTAINS THE OLD 2-DIGIT FORM OF THE YEAR -
C ... THIS PROGRAM WILL CONVERT THE RECORD TO A 4-DIGIT YEAR USING THE
C      "WINDOWING" TECHNIQUE SINCE SUBSEQUENT LOGIC EXPECTS THIS

            PRINT *, ' '
            PRINT *, '==> Read in RECORD from tcvitals file -- ',
     .       'contains a 2-digit year "',cline(20:21),'"'
            PRINT *, ' '
            PRINT *, 'From unit 31; cline: ',cline
            PRINT *, ' '
            DUMY2K(1:19) = cline(1:19)
            IF(cline(20:21).GT.'20')  THEN
               DUMY2K(20:21) = '19'
            ELSE
               DUMY2K(20:21) = '20'
            ENDIF
            DUMY2K(22:128) = cline(20:126)
            cline = DUMY2K
            PRINT *, ' '
            PRINT *, '==> 2-digit year converted to 4-digit year "',
     .       cline(20:23),'" via windowing technique'
            PRINT *, ' '
            PRINT *, 'From unit 31; cline: ',cline
            PRINT *, ' '
         ELSE  IF(cline(37:37).eq.'N' .OR.
     .            cline(37:37).eq.'S')  THEN

C ... THIS RECORD CONTAINS THE NEW 4-DIGIT FORM OF THE YEAR
C ... NO CONVERSION NECESSARY SINCE THIS SUBSEQUENT LOGIC EXPECTS THIS

            PRINT *, ' '
            PRINT *, '==> Read in RECORD from tcvitals file -- ',
     .       'contains a 4-digit year "',cline(20:23),'"'
            PRINT *, ' '
            PRINT *, 'From unit 31; cline: ',cline
            PRINT *, ' '
            PRINT *, '==> No conversion necessary'
            PRINT *, ' '
         ELSE
            PRINT *, ' '
            PRINT *, '***** Cannot determine if this record contains ',
     .       'a 2-digit year or a 4-digit year - skip it and try ',
     .       'reading the next record'
            PRINT *, ' '
            CYCLE
         END IF

         clines(k)=cline
         klines=k
      enddo

 9    continue

      LOOP1: do k=klines,1,-1
         cline=clines(k)
         print *, ' '
         print *, 'Look at record: ',cline
         print *, ' '

         LOOP1n1: do j=1,16 

c  look for same name and time

            if(cline(29:30) .ne. c2) then
               print *, ' '
               print *, 'Do not process this record because its hour ',
     .                  '(=',cline(29:30),') is different than the ',
     .                  'cycle hour (=',c2,')'
               print *, ' '
               cycle LOOP1
            endif
            if(cline(6:6) .eq. '8' .or. cline(6:6) .eq. '9')  then
               print *, ' '
               print *, 'Do not process this record because it is a ',
     .                  'test storm, storm id =',cline(6:7)
               print *, ' '
               cycle LOOP1
            endif
            if(names(j) .eq. cline(10:19) .and.
     .         names(j) .ne. 'NAMELESS' ) then
               print *, ' '
               print *, 'Do not process this record because it has a',
     .                  ' name that has already been processed (name=',
     .                  cline(10:19),')'
               print *, ' '
               cycle LOOP1
            endif
            print *, ' '
            print *, 'NAMES ',cline(10:19),names(j)
            print *, ' '
         enddo LOOP1n1

         LOOP1n2: do j=1,16 

c  assign new name

            print *, ' '
            print *, 'threeloop' 
            print *, ' '
            if(names(j) .eq. 'xxxxxxxxxx' ) then
               names(j)=cline(10:19)
               cname=cline(10:19)
               cdate=cline(20:32)

c - On rare occasions, upstream program QCTROPCY can stamp a 'C' into
c    character 67 - this had resulted in a failure in this program
c    (note 104 format).  Change by Keyser (08/10/1998) uses 9104 format
c    if mwind < 100 .

               if(cline(67:67).eq.'1')  then
                  read(cline,104)latd,lond,idir,isp,icp,mwind
 104              format(33x,i3,2x,i4,x,i4,i4,x,i4,10x,i3)
               else
                  read(cline,9104)latd,lond,idir,isp,icp,mwind
9104              format(33x,i3,2x,i4,x,i4,i4,x,i4,11x,i2)
               end if
               flat=latd/10.
               flon=lond/10.
               fdir=idir
               fsp=isp/10.
               central_pressure=icp 

               write(cout,105) cname,cdate,flat,cline(37:37),flon,
     .          cline(43:43),'MV ',fdir,'AT',fsp,'M/SEC',
     .          central_pressure,mwind
               ic=530+10*j
               iatl=0
               ipac=0
               if(lond  -latd .lt. 800) iatl =1
               if(lond-latd .gt. 720 .and. latd .lt. 150) ipac=1
               if(lond-latd .gt. 800 )  ipac=1

c     test for atlantic or Pacific storm

               if(iatl .eq. 1) then
                  icounta=icounta+1
                  ic=icounta*20+620
ccccc             ic=icounta*20+520
                  write(74,114) 'PUTLA ',ic,
     .             '6420 01.0 90.0 038 1 0 0  ',cout(1:80)
               endif
               if(ipac .eq. 1) then
                  icount=icount+1
                  ic=icount*20+520
                  write(74,114) 'PUTLA ',ic,
     .             '4201 01.0 90.0 038 1 0 0  ',cout(1:80)
               endif
ccccc          write(74,114) 'PUTLA ',ic,'4201 04.0 90.0 080 1 0 0  ',
ccccc.          cout(1:80)
 114           format(a6,i5,a27,a80)
               print 105, cname,cdate,flat,cline(37:37),flon,
     .          cline(43:43),'MV ',fdir,'AT',fsp,'M/SEC',
     .          central_pressure,mwind
 105           format(a10,x,a13,x,f4.1,a1,x,f5.1,a1,x,a3,f6.0,x,a3,f5.1,
     .          a5,x,f6.1,' MB',i3,'M/SEC'   ) 
               if(cline(37:37).eq. 'S') flat=-flat
               if(cline(43:43).eq. 'W') flon=360-flon
               call maptra(flat,flon,uu,vv)
               if ((mwind*2.24) .gt. 38) then
                  call plchhq(uu,vv,':F35:m',7.5,0.,0.)
               else
                  call plchhq(uu,vv,':F30:TD',2.0,0.,0.)
                  call plchhq(uu,vv,':F37:S',6.0,0.,0.)
               endif
               if((mwind*2.24) .gt. 74.) then

c    close hurricane symbol with black fill

                  call plchhq(uu,vv-0.005,':F37:Z',4.5,0.,0.)
               endif
               call gslwsc(3.0)
               call plchmq(uu+0.02,vv-0.055,cname,2.5,0.,0.)
               call gslwsc(1.0)
               exit LOOP1n2
            endif
         enddo LOOP1n2
      enddo LOOP1
ccccc isizea=(icounta+1)/2 +2
      isize=(icount+1)/2   +2
      isize=icount+2
      isizea=icounta+2
      if(isize .eq. 2) then
         write(74,114)'PUTLA ',540,'4201 01.0 90.0 060 1 0 0  ',
     .    cnull(1:80)
         isize=isize+1
         print *, cnull(1:80)
      endif
      if(isizea .eq. 2) then
         write(74,114)'PUTLA ',640,'6420 01.0 90.0 060 1 0 0  ',
     .    cnull(1:80)
          isizea=isizea+1
         print *, cnull(1:80)
      endif
      write(74,115)'BOX ',510,4190,isize,25
      write(74,115)'BOX ',610,6401,isizea,25
 115  format(a4,2i5,i5,i5)

      print *, ' '
      print *, ' ===> LEAVING SUBROUTINE HBULL'
      print *, ' '

      return

      end

c**************************************************************

