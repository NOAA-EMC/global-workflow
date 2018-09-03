      program WNDANFTF      
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: GRAPH_WNDANFTF
C   PRGMMR: FACEY            ORG: NP12        DATE: 2000-02-01
C
C ABSTRACT:  This program plots wind barbs and temperatures at 
C   several U.S. cities.  It uses NCAR graphics to produce cgm a
C   metafile which is rasterized in other job steps.
C
C PROGRAM HISTORY LOG:
C   96-12-18  VandenBerghe    Original code         
c   97-01-17  VandenBerghe    added geometic levels and subordinated 
c                             map background (dots) and this documentation.
C   98-08-05  Kumar           Converted to F90 and Y2K compliancy
C   00-01-27  Facey           Converted to to run on IBM SP
C
C USAGE:
c   INPUT FILES:
c      fort.20   formatted station file  
c   OUTPUT FILES:    
c      gmeta     (implicitly fortran unit 2 ) used by ncar 
c                  graphics
c       CAVEAT:  requires ncar graphics version 4.0 or above
c               as of this writing
c               setenv NCARG_ROOT /usr/local/ncar4.0          
c               setenv PATH "$PATH":/usr/local/ncar4.0/bin
c                   (or the bourne/korn analogs)
c               handles this; only the first setenv is required
c               but the other  will likely be 
c               needed by ictrans downstream
c               
c               The quality of the formatted report
c               numbers determines the quality of the
c               plotted numbers.  Problems
c               with the data are overwhelmingly likely
c               to be upstream redupa or in the original
c               input file, this program just plots
c               what it is fed.  
c
C   SUBPROGRAMS CALLED: 
C     UNIQUE:    - ROUTINES THAT ACCOMPANY SOURCE FOR COMPILE
c                - closen,openn,qcnt,
c                -  updatr,
c                - dayowk (these last two  are to avoid search of
c                  graphics libraries at load time)
c
C     LIBRARY:
C       COMMON   - NCAR UTILITIES (mappos,WMSETR,WMSETI,WMGETR,
c                - maptra,pcseti,plccmq,plchmq,wmbarb,gslwsc,
c                - gsclip,mapsti,mapstc,maproj,mapset,mapdrw,
c                - cpseti,cpsetr,gopks,gopwk,gacwk,gdawk,gclwk,
c                - gclks.)
c                - all are brought in by /usr/local/ncar4.0/
c                - ncargf77 command used for ncar builds
c                - ncar 4.0 or higher required (available on
c                - all three crays and newest workstations also)
c
C       W3LIB    - W3TAGB/E
C
c MACHINE:    IBM SP
c
c   LANGUAGE:   IBM fortran 90.
c
c REMARKS:
c       The gross job flow is
c       redupa -->
c      WNDALFTF---> ictrans ----> ras2bit ----> bedient.pack --->
c        and out the door to OSO. 
c
c      input is prepda soundings preformatted upstream
c      into ascii.  Each line corresponds to one
c      level at one station and contains wind speed,direction,
c      temperature, dewpoint, and geopotential height along
c       with station name, longitude, and latitude and
c      some quality flags.  Both standard pressure
c      and standard geopotential levels are supplied.
c        Upstream redupa checks the quality
c      flags and marks nonstandard quality as missing.   Missing  data
c      is denoted with -999999. 
c      
c      The output product is a four panel fax chart containing
c      second standard level (lower left), 14000 foot (lower right)
c      400mb (24000 foot)(upper left) and 250mb (34000 foot) plots
c      (upper right).  Only wind information is plotted at the
c      lower two levels.
c
C$$$
      character*3 cmonth(12) 
      data cmonth/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     1 'AUG','SEP','OCT','NOV','DEC'/
      parameter(nsta=1240)
      character*4 ctemp,cdew,cgpm
      character*3 cdir
      character*4 cda
      character*8 csta
      COMMON/PACKRA/IRAS(10)
       common/oth/u(70,40),v(70,40),rlats(nsta),rlons(nsta),
     1 up(nsta),vp(nsta),tp(nsta)
      dimension kds(25)
      parameter (rmnlon=231.,rmxlon=300.,rmnlat=21.,rmxlat=50.)
      dimension rl(2),rl2(2),rl3(2),rl4(2)
      data rl,rl2,rl3,rl4/rmnlat,0.,rmxlat,0.,
     1rmnlon,0.,rmxlon,0./
      dimension g2(70,40)
      dimension llv(8)
      character*40 ctit
      dimension state(10)
      data llv/-07,400,177,177,-07,250,177,177/ 
      dimension ifeet(8)
      data ifeet/6000,18000,30000,39000,9000,24000,34000,99999/
      dimension boxes(4,8)
      data boxes/0.0, 0.5, 0.0, 0.24,
     2           0.0, 0.5, 0.25, 0.49,
     3           0.0, 0.5, 0.50, 0.74,
     4           0.0, 0.5, 0.74, 0.99,
     5         0.38, 0.83, 0.00, 0.24,
     6         0.38, 0.83, 0.25, 0.49,
     7         0.38, 0.83, 0.50, 0.74,
     8         0.38, 0.83, 0.75, 0.99/
      character*132 mtitle
      character*160 cline
      CALL W3TAGB('GRAPH_WNDANFTF',2000,0032,0096,'NP12')                   
       ix=70
       iy=30
       tx = 0.0
       ty = 0.0
       dpx = 0.0
       dpy = 0.0
       do i =1, 70
       do j =1, 40
          g2(i,j) = 0.0
       end do
       end do
       mtitle = "  "
       call openn
        do 1000 ,ll=8,1,-1
c   skip top boxes with goto below
        if(ll .ne. 1 .and. ll .ne. 2 .and. ll .ne. 5 .and.
     1 ll .ne. 6) go to 1000
c      bottom two boxes are standard height level
c      top two boxes are standard pressure level
       call mappos (boxes(1,ll),boxes(2,ll),boxes(3,ll),boxes(4,ll))
       iras(1)=00
c       draw the map
      call qcnt (g2,ix,iy,rl,rl2,rl3,rl4,mtitle)
 191   format('NO95 ',i2,'HR UPPER WIND PROG D169'  )
c       read   date from a station file
       rewind (20) 
        read(20,161)cline
 161   format(a160) 
       read(cline,401)iras(6),iras(5),iras(4),iras(3)
       rewind (20) 
 401   format(135x,i2,i2,i2,i2)
       print *, iras(6),iras(5),iras(4),iras(3)
       iyear=iras(6)
       if(iyear .gt. 50) iyear=iyear+1900
       if(iyear .le. 50) iyear=iyear+2000
       call dayowk(iras(4),iras(5),iyear,iday,cda,ist)
      call WMSETR('WBD barb size',0.11)
       CALL WMSETI('WBF circle on or off',1)
       call WMSETR('WBC circle size ',0.1)
       call WMSETR('WBA angle       ',60.0)
        call pcseti('CD ',1) 
        call pcseti('FN ',25)
c  Coordinates for map titles
       call maptra(27.,-87.,tx,ty)
c  Coordinates for product title
       call maptra(27.,-118.,dpx,dpy)
c        call plchmq( tx,ty,ctit,03.,0.,0.)
       write(ctit,195)llv(ll)
 195   format(i5,'MB   WINDS  AND TEMPS  ')
        if(ll .eq. 1) ctit=' 2D STANDARD LVL ABOVE SFC'
        if(ll .eq. 5) ctit='14000 FT. (600MB) WINDS'
        if(ll .eq. 2) ctit='    24000 FT. (400MB)  '
        if(ll .eq. 6) ctit='    34000 FT. (250MB)  '
        call gslwsc(2.)
        call plchmq( tx+0.010,ty-0.008,ctit,04.,0.,0.)
        if(ll .eq. 1)  then
          call plchmq(dpx,dpy,' WINDS ALOFT OBS.',4.0,0.,0.)
          if(iras(3) .eq. 12)
     1    call plchmq(dpx,dpy-0.010,'N106..  D142.. ',4.0,0.,0.)
          if(iras(3) .ne. 12)
     1    call plchmq(dpx,dpy-0.010,'N106.. D031.. ',4.0,0.,0.)
        call plchmq(dpx,dpy-0.018,'DATA CUTOFF 2+40  ',3.4,0.,0.)
        endif
        call gslwsc(1.)
       write(ctit,197)iras(3),cda,cmonth(iras(5)),iras(4),iyear
 197   format(' OBS. ',1x,i2.2,'Z',1x,a3,1x,a3,1x,i2,1x,i4)
        call plchmq( tx,ty-0.016,ctit,03.,0.,0.)
       if (ll .ne. 1 .and. ll .ne. 5) then
       ctit='GFS PREPBUFR WINDS AND TEMPS' 
       else
       ctit='     GFS PREPBUFR WINDS     ' 
       endif
       call plchmq(tx,ty-0.024,ctit,03.,0.,0.)
      CALL WMSETR('WBS',0.013)
      CALL WMSETI('COL',1)
      CALL WMGETR('WBS',WSLEN)
       do 500,irec=1,999999
       read(20,161,end=999) cline
       read(cline,301)  csta,slon,slat,(state(kk),kk=1,10)
c        slon=360.-slon
 301      format(a8,1x,f6.2,1x,f6.2,1x,10(f10.2,1x))
       if (abs (state(1) - llv(ll)) .gt. 1.) go to 500
c     special code for ll=1
c     using station elevation compute "second standard level"
c     which is the second integral thousand feet above
c     the surface.  Then plot variables at that level
      if(ll .eq. 1) then
       elev=state(10)*100/(2.54*12)
       pelev=state(2)*100/(2.54*12)
      ielv=elev/1000. +2
      stdlev_2=ielv*1000
      if(abs(pelev-stdlev_2) .gt. 100) go to 500 
      endif
      if(ll .eq. 5) then
      pelev=state(2)*100/(2.54*12)
      if(abs(pelev-14000. ) .gt. 100) go to 500
      endif
c
      do 12,k=1,1
       call maptra(slat,slon,uu,vv)
c         print *, ' COORDS',slon,slat,uu,vv
c   now rotate winds in map  projection space
c   rotation angle is zero at longitude 110
        phi=(110. + slon-360. ) *3.1415927/180.
        phi=phi*0.7
         dir=state(5)
c        note speed is in knots!!! not meters/second
         speed=state(6)
         dirr=dir*3.1415927/180.
         ru=-sin(dirr)*speed
         rv=-cos(dirr)*speed
         up(k)=ru
         vp(k)=rv
c rotate winds to match map projection 
        uprime=ru*cos(phi)-rv*sin(phi)
        vprime =ru*sin(phi)+rv*cos(phi)      
c      call wmbarb(uu,vv,-uprime,-vprime)
      if( ll .ne. 1 .and. ll .ne. 5) then
      dewpoint=state(4)
      temps=state(3)
      if(dewpoint .lt. -9999) dewpoint=-9999
      if(temps .lt. -9999) temps=-9999
c
      if(temps .lt. -9998. .or. dewpoint .lt. -9998 
     1   .or. (temps-dewpoint) .lt. 5.)  then
c
        call plchhq(uu,vv,':F37:Z',2.5,0.,0.)
        else
        call plchhq(uu,vv,':F37:R',2.5,0.,0.)
        endif
c
      else
      call plchhq(uu,vv,':F37:R',2.5,0.,0.)
      endif
       write(ctemp,193) ifix(state(3))
       if (state(2) .lt. 5790 .and. state(2) .gt. 5780)
     1 print *,' CTEMP',' ',csta,ctemp,state(3),ifix(state(3))
       if (state(4). gt. -9999) then
       write(cdew,193) ifix(state(4))
       write(cgpm,193) ifix(state(2)/10.)
       end if
       if(state(4) .lt. -9999) cdew='  M '
       if(state(2) .lt. -9999) cgpm='  M '
       if(state(3) .lt. -9999) ctemp=' M  '
c       if(speed .gt. 500. .or. speed .lt. 0. .or. 
c     1   dir .lt. -999 .or. dir .gt. 999) go to 500
c WIND BARB ERASE STUFF
      call GSLWSC(4.)
      CALL WMSETI('COL',0)
       if(speed .gt. 500. .or. speed .lt. 0. .or. 
     1   dir .lt. -999 .or. dir .gt. 999) go to 591 
       call wmbarb(uu,vv,-uprime,-vprime)
 591   continue
c end wind barb erase
      call GSLWSC(1.)
      CALL WMSETI('COL',1)
c end stuff
       if(speed .gt. 500. .or. speed .lt. 0. .or. 
     1   dir .lt. -999 .or. dir .gt. 999) go to 592 
      call wmbarb(uu,vv,-uprime,-vprime)
 592  continue
 193  format(i4 )    
         rad=0.0139
c
          theta=90-dir
         idir=dir
         if(idir .lt. 0) idir=idir+360
         write(cdir,196)idir
  196    format(i3)
         rad=0.0139
c   calculate write position for direction number
c     Rotate ten plus phi degrees
         theta=theta+10. +phi*180./3.1415927
         di=rad*(cos(theta*3.1415927/180.))
         dj=rad*(sin(theta*3.1415927/180.))
        if( vp(k) .lt. 0) djj=-rad/3.
        if( vp(k) .ge. 0) djj=rad/3.
        call gslwsc(2.0)
cc        call plchmq( uu,vv+djj,ctemp,4.0,0.,0.)
        call gslwsc(1.0)
c        call plchhq( uu-di/3.,vv-dj/3.,ctemp,4.0,0.,0.)
       if(speed .gt. 500. .or. speed .lt. 0. .or.
     1   dir .lt. -999 .or. dir .gt. 999) go to 593 
         call plchmq(uu+di,vv+dj,cdir(2:2),2.4,0.,0.)
 593    continue
        theta=theta-65. +phi*180./3.1415927
         di=rad*(cos(theta*3.1415927/180.))
         dj=rad*(sin(theta*3.1415927/180.))
         center=1.0
         if(vprime .lt. 0) center=0.0 
         call gslwsc(2.0)
        if( ll .ne. 1 .and. ll .ne. 5) then
        call plchmq( uu+rad/6.,vv+rad/3.0,ctemp,3.2,0.,0.)
        endif
c        call plchmq( uu-rad/2.,vv-rad/6.,cdew,2.5,0.,0.)
        call gslwsc(1.0)
c        call plchmq( uu+rad/3.,vv+rad/10.,cgpm,3.2,0.,0.)
c        call plchmq( uu+di/3.5,vv+dj/2.5,ctemp,3.2,0.,center)
        call gslwsc(1.0)
 12    continue
 500    continue
 999     continue
 1000  continue
      call closen 
      CALL W3TAGE('GRAPH_WNDANFTF') 
       stop
       end
      subroutine qcnt(grid,ix,iy,rl,rl2,rl3,rl4,mtitle)
      character*132 mtitle
      dimension grid(ix,iy)
      dimension rl(2),rl2(2),rl3(2),rl4(2)
      common/icc/icount
       common/ctrxxx/rmnlat,rmxlat,iflag
       iflag=0
       Rmnlon=rl3(1)
       rmxlon=rl4(1)
       rmnlat=rl(1)
       rmxlat=rl2(1) 
       icount=0
      CALL GSCLIP (0)
C Draw political & continental outlines.
      CALL MAPSTI ('GR - GRID',10)
      CALL MAPSTC ('OU - OUTLINE DATASET','PS')
C
c draw polar stereo grid
      CALL MAPROJ ('ST - STEREOGRAPHIC ',40.,-110.,00.)

         rlatmin=rl(1)+3
         rlonmin=rl3(1)+5
         rlonmax=rl4(1)-5
         rlatmax=rl2(1)-3
      call mapset('co',rlatmin,rlonmin,rlatmax,rlonmax) 
      CALL MAPSTI ('PE - PERIMETER FLAG', 0)
C Draw map.
c   dot the map
      call MPSETI('DO',1)
      call MPSETI('DD',4)
      CALL MAPDRW
      CALL CPSETI ('SET - DO SET-CALL FLAG',0)
      CALL CPSETI ('MAP - MAPPING FLAG',1)
      return
      END
       subroutine openn
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
      return
       end
      subroutine closen
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
       CALL GCLKS
       return
       end
C     NOAA UTILITIES!! (saves having to load cntr libraries)
      SUBROUTINE UPDATR
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  UPDATR        UPDATE OR BACK DATE A DATE/TIME.
C   PRGMMR: LUKE LIN         ORG: NMC41       DATE:95-10-06
C
C ABSTRACT: UPDATE OR BACKDATE A DATE TIME PASSED IN VIA COMMON
C  PACKRA.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR ????
C   88-10-11  HENRICHSEN  ADD DOC BLOCK AND CONVERT TO MVS FORTRAN 77.
C   95-10-06  LUKE LIN    CONVERT IT CFT-77.
C
C USAGE:    CALL UPDATR
C   INPUT ARGUMENT LIST:
C     COMMON   _ /PACKRA/JTIME(2),IVLDHR,IVLDAY,IVLDMO,IVLDYR,
C                        IHOUR,IDAY,IMONTH,IYR
C              - JTIME(1) IS NUMBER OF HOURS TO UPDATE
C              - (IF JTIME IS NEGATIVE IT WILL BACKDATE)
C              - THE ORIGINAL DATE/TIME IS GIVEN IN
C              - IHOUR,IDAY,IMONTH,IYR.
C
C   OUTPUT ARGUMENT LIST:
C     COMMON   _ /PACKRA/JTIME(2),IVLDHR,IVLDAY,IVLDMO,IVLDYR,
C                        IHOUR,IDAY,IMONTH,IYR
C              - THE UPDATED DATE/TIME IS RETURNED IN
C              - IVLDHR,IVLDAY,IVLDMO,IVLDYR.
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: CFT77
C   MACHINE:  CRAY
C
C$$$
C
      COMMON   /PACKRA/JTIME(2),IVLDHR,IVLDAY,IVLDMO,IVLDYR,
     1                 IHOUR,IDAY,IMONTH,IYR
C
      INTEGER    ITABYR(13)
      INTEGER    LPTB(13)
      INTEGER    NOLPTB(13)
C
      DATA       LPTB      /0,744,1440,2184,2904,3648,4368,5112,
     1                      5856,6576,7320,8040,8784/
      DATA       NOLPTB    /0,744,1416,2160,2880,3624,4344,5088,
     1                      5832,6552,7296,8016,8760/
C
      ASSIGN 211 TO KABUL
      IVLDYR = IYR
      GO TO 500
C     ...WHERE 500 IS SUBROUTINE TO INITIALIZE ITABYR
C     ...AND RETURN THRU KABUL
  211 IHRYR = IHOUR + 24*(IDAY - 1) + ITABYR(IMONTH)
      IHRYR2 = IHRYR + JTIME(1)
C     ...TO TEST FOR BACKDATED INTO PREVIOUS YEAR...
  213 IF(IHRYR2 .LT. 0) GO TO 400
      DO  215  M=2,13
      IF(IHRYR2 .LT. ITABYR(M)) GO TO 222
  215 CONTINUE
C     ...IF IT FALLS THRU LOOP TO HERE, IT IS INTO NEXT YEAR...
      IVLDYR = IVLDYR + 1
      IHRYR2 = IHRYR2 - ITABYR(13)
      ASSIGN 219 TO KABUL
      GO TO 500
  219 GO TO 213
  222 IVLDMO = M - 1
      IHRMO = IHRYR2 - ITABYR(IVLDMO)
      NODAYS = IHRMO/24
      IVLDAY = NODAYS + 1
      IVLDHR = IHRMO - NODAYS*24
      GO TO 666
C     ...ALL FINISHED.  RETURN TO CALLING PROGRAM.......................
C     ...COMES TO 400 IF NEG TOTAL HRS. BACK UP INTO PREVIOUS YEAR
  400 IVLDYR = IVLDYR - 1
      ASSIGN 411 TO KABUL
      GO TO 500
C     ...WHICH IS CALL TO INITIALIZE ITABYR AND RETURN THRU KABUL
  411 IHRYR2 = ITABYR(13) + IHRYR2
      GO TO 213
C
C     ...SUBROUTINE INITYR...
C     ...CALLED BY GO TO 500 AFTER ASSIGNING RETURN NO. TO KABUL...
C     ...ITABYR HAS MONTHLY ACCUMULATING TOTAL HRS REL TO BEGIN OF YR.
C     ...DEPENDS ON WHETHER IVLDYR IS LEAP YEAR OR NOT.
  500 IQUOT = IVLDYR/4
      IRMNDR = IVLDYR - 4*IQUOT
      IF(IRMNDR .NE. 0) GO TO 511
C     ...WAS MODULO 4, SO MOST LIKELY A LEAP YEAR,
      IQUOT = IVLDYR/100
      IRMNDR = IVLDYR - 100*IQUOT
      IF(IRMNDR .NE. 0) GO TO 522
C     ...COMES THIS WAY IF A CENTURY YEAR...
      IQUOT = IVLDYR/400
      IRMNDR = IVLDYR - 400*IQUOT
      IF(IRMNDR .EQ. 0) GO TO 522
C     ...COMES TO 511 IF NOT A LEAP YEAR...
  511 DO  513  I = 1,13
      ITABYR(I) = NOLPTB(I)
  513 CONTINUE
      GO TO 533
C     ...COMES TO 522 IF LEAP YEAR
  522 DO  525  I = 1,13
      ITABYR(I) = LPTB(I)
  525 CONTINUE
  533 GO TO KABUL,(211,219,411)
C     ...WHICH RETURNS TO SECTION FROM WHICH CALLED.....................
  666 CONTINUE
      RETURN
      END
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
C   LANGUAGE: VS FORTRAN 77 WITH EXTENDED LOGIC.
C   MACHINE:  NAS
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
