      program RAS2BITY
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: GRAPH_RAS2BITY
C   PRGMMR: KUMAR            ORG: NP12        DATE: 2000-02-14
C
C ABSTRACT: read generic pixel image with generic unspecified header
C   and compress the pixels to bits. 0 is off while 1-255 are on  
C
C PROGRAM HISTORY LOG:
C   96-11-18  George VandenBerghe  first written as ras2bit
C   96-12-29  George VandenBerghe  standardized documentation
c   97-04-30  George VandenBerghe  added rotation logic and changed name
c 1999-01-04  Krishna Kumar        modified to run for the IBM RS/6000
c                                  replaced PACK a CRAY specific routine
c                                  with SBYTES in the W3LIB90. At the
c                                  script level a header file 
c                                  "ras2bity.header" is cat-ed to
c                                  the bitmap output on f59 in order to 
c                                  attach CNTR header to generic bitmap 
c                                  thereby eliminating the glue program 
c                                  HGLUE.
C
C USAGE:
C   INPUT FILES:
C     f11      - pixels with undefined and unused header 
C                (ncar graphics output - gmeta file)
c     fort.5   - standard input resolution information
c
C     PARM     - from fort.5  file size, pixels/scanline and #scanlines
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     f59      - bitmap output (scans any header)            
C     FT06F001 - a few lines 
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - SB                                        
C     LIBRARY:
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - system errors only                      
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C          Replaced CRAY specific pack routine with SBYTES from
C          the W3LIB90 
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90                 
C   MACHINE:  IBM 
C
C$$$
C
C    CONVERT X WINDOW DUMP PIXELS TO RAW BITMAP.
C   inverted bitmap is written (i.e bit rectangle is flipped 
C    on scanline axis ) 
C    bit rectangle is first rotated 90 degrees counterclockwise (ras2bit
C    ZERO PIXELS SET TO 'OFF', ALL OTHER VALUES SET TO 'ON'
C
      CALL W3TAGB('GRAPH_RAS2BITY',2000,0045,0090,'NP12')                   

      print *,
     1 ' SPECIFY TOTAL XWD SIZE (BYTES), PIXELS/LINE AND SCANLINES'
c
       read(5,101) itotal,ix,iy
 101   format(i10)
      print *,
     1 '  TOTAL XWD SIZE (BYTES), PIXELS/LINE AND SCANLINES ARE'
     1,itotal,ix,iy 
      iwd=ix*iy/8
      print 194,itotal,ix,iy,iwd
 194   format(4i10)
c***
       call sb(itotal,ix,iy,iwd)
c***
       print *,' WROTE ',iy,' SCANLINES',ix,' PIXELS/LINE to UNIT 9'
c
      CALL W3TAGE('GRAPH_RAS2BITY') 
c
      stop
      end
C
C********************************************************************
C
      subroutine sb(itotal,ix,iy,iwd) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PGM-NAME    DESCRIPTIVE TITLE NOT PAST COL 70
C   PRGMMR: KRISHNA KUMAR   ORG: W/NP12     DATE: 1999-01-04 
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  SEE NMC HANDBOOK SECTION 3.1.1. FOR DETAILS
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD   ORIGINAL AUTHOR(S)'S NAME(S) HERE
c
c   96-12-17   George VandenBerghe
C 1999-01-04   Krishna Kumar Converted this code from CRAY to IBM
C                            RS/6000
C
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL PGM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
c      all arguments are input.  THe subroutine packs the
c      8 bit pixel array "itotal" dimensioned ix,iy into
c      a second array of length ix*iy/8 and writes this
c      to fortran unit 59 
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     DDNAME1  - GENERIC NAME & CONTENT
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     DDNAME2  - GENERIC NAME & CONTENT AS ABOVE
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM 
C
C$$$
c
       dimension ib(itotal)
       dimension i5(ix,iy)
       dimension in(ix/8,iy),ibytes(iwd)
c
       character*1 i5, ib
       character*1 ibytes
cc       dimension ifull(ix,iy)
       dimension ifull(iy,ix)
c
c      equivalence (in,i5)
c
ckumar file name for unit 11 has not been defined
ckumar in the open statement
c
       ierr=11
c
       open(11,access='direct',recl=itotal,status='old',
     &      err=999)
       read(11,rec=1) ib
c
       l=itotal
       do 10,k=1,iy
       do 10,j=1,ix
       jj=ix+1-j
       ifull(k,jj)=0
       if( mova2i(ib(l)) .gt. 0) ifull(k,jj)=1
       l=l-1
 10    continue 
c
c Used sbytes from w3lib instead of CRAY specific PACK
c
       nbits = 1
c
ckumar call pack(ibytes,1,ifull,ix*iy) ! CRAY specific 
c
       call sbytes(ibytes,ifull,0,nbits,0,ix*iy) ! From W3LIB90
c
c
ckumar file name for unit 59 has not been defined
ckumar in the open statement
c
       open(59,access='direct',recl=ix*iy/8)
c
       call byteswap(ibytes,4,ix*iy/8)
       write(59,rec=1) ibytes 
c
      return
 999  print*,'Unable to open file on unit # 11'
      call errexit(ierr)
      end 
