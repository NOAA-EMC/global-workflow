      PROGRAM RAS2BIT 
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: GRAPH_RAS2BIT
C   PRGMMR: KUMAR            ORG: NP12        DATE: 2000-02-14
C
C ABSTRACT: read generic pixel image with generic unspecified header
C   and compress the pixels to bits. 0 is off while 1-255 are on  
C
C PROGRAM HISTORY LOG:
C   96-11-18  George VandenBerghe  first written
C   96-12-29  George VandenBerghe  standardized documentation
C   99-01-21  Weidong Jiang converted to IBM RS6000 SP and tested 
C 1999-12-20  Krishna Kumar        modified to run for the IBM RS/6000
C                                  replaced PACK a CRAY specific routine
C                                  with SBYTES in the W3LIB90. At the
C                                  script level a header file
C                                  "ras2bity.header" is cat-ed to
C                                  the bitmap output on f59 in order to
C                                  attach CNTR header to generic bitmap
C                                  thereby eliminating the glue program
C                                  HGLUE.
C
C USAGE:
C   INPUT FILES:
C     f11      - pixels with undefined and unused header 
c     fort.5   - standard input resolution information
c
C     PARM     - from fort.5  file size, pixels/scanline and #scanlines
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     f59      -  bitmap output (sans any header)            
C     FT06F001 - a few lines 
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - SB                                        
C     LIBRARY:
C       W3LIB    
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
C   MACHINE:  IBM RS6000 SP 
C
C$$$
c
c    CONVERT X WINDOW DUMP PIXELS TO RAW BITMAP.
c   inverted bitmap is written (i.e bit rectangle is flipped 
c    on scanline axis ) 
C    ZERO PIXELS SET TO 'OFF', ALL OTHER VALUES SET TO 'ON'
c
C   INPUT .. f11 (stream of bits)
c           FORT.5  reads size of window dump and resolution.
c
c   OUTPUT .. f59
c
c   PROGRAMMER   George VandenBerghe 11/25/1996
c
c    HISTORY    11/25/1996  written
c    LANGUAGE   Cray fortran 77. 
c
      CALL W3TAGB('GRAPH_RAS2BIT',2000,0045,0093,'NP12')                   


      print *,
     1 ' SPECIFY TOTAL XWD SIZE (BYTES), PIXELS/LINE AND SCANLINES'
      read(5,101) itotal,ix,iy
 101   format(i10)
      print *,
     1 '  TOTAL XWD SIZE (BYTES), PIXELS/LINE AND SCANLINES ARE'
     1,itotal,ix,iy 
      iwd=ix*iy/8
c      print 194,itotal,ix,iy,iwd
 194   format(4i10)
       call sb(itotal,ix,iy,iwd)
       print *,' WROTE ',iy,' SCANLINES',ix,' PIXELS/LINE to UNIT 9'
      CALL W3TAGE('GRAPH_RAS2BIT') 
       stop
      end

      subroutine sb(itotal,ix,iy,iwd) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PGM-NAME    DESCRIPTIVE TITLE NOT PAST COL 70
C   PRGMMR: YOUR NAME        ORG: W/NMCXX    DATE: YY-MM-DD
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  SEE NMC HANDBOOK SECTION 3.1.1. FOR DETAILS
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD   ORIGINAL AUTHOR(S)'S NAME(S) HERE
C
C   96-12-17   George VandenBerghe
C   99-01-21   Weidong Jiang made it portable between ibm and cray
C 1999-12-20   Krishna Kumar replaced the CRAY specific PACK using
C              sbytes of the w3lib90
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
c          An 8-bit integer array is added in the original
c          code and explicit conversion from integer to 
c          character is made before writing to fort.59.
c          This is necessary when testing this program on
c          Cray.  IBM compiler can handle implicit integer-
c          character conversion, so the original code is ok
c          when running on IBM RS6000 SP.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
      dimension ib(itotal)
      dimension i5(ix,iy)
      dimension in(ix/8,iy),ibytes(iwd)
c
      character*1 i5, ib
      character*1 ibytes
       dimension ifull(ix,iy)
c
c      equivalence (in,i5)
c
ckumar file name for unit 11 has not been defined 
ckumar in the open statement
c
      ierr=11
c
      open(11,access='direct',recl=itotal,status='old',
     &     err=999)
      read(11,rec=1) ib
c
      l=itotal
      do 10,k=1,iy
      do 10,j=1,ix
       ifull(j,k)=0
       if( mova2i(ib(l)) .gt. 0) ifull(j,k)=1
         l=l-1
 10   continue 
c
c Used sbytes from w3lib instead of CRAY specific PACK
c
       nbits = 1
c
ckumar call pack(ibytes,1,ifull,ix*iy)   ! CRAY specific
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
c
       write(59,rec=1) ibytes 
c
       return
 999   print*,'Unable to open file on unit # 11 '
       call errexit(ierr)
       end

