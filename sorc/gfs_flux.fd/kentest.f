      program kentest
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM: METEOFLX
C   PRGMMR: PAN              ORG: NP23        DATE: 1999-07-22
C
C ABSTRACT: COMPUTES AND WRITES BUFR FORMAT METEOGRAM FILES.
C
C PROGRAM HISTORY LOG:
C   99-07-21  HUALU PAN                       
C
C USAGE:
C   INPUT FILES:
C     FTxxF001 - UNITS 11 THRU 49
C     PARM     - UNIT 5 (STANDARD READ)
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     FTxxF001 - UNITS 51 THRU 79
C     FTxxF001 - UNIT 6 (STANDARD PRINTFILE)
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - ROUTINES THAT ACCOMPANY SOURCE FOR COMPILE
C     LIBRARY:
C       W3LIB    -
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - TROUBLE OR SPECIAL FLAG - SPECIFY NATURE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
      parameter(nsta=2000)
      real rlat(nsta), rlon(nsta)
      integer istat(nsta), imask(nsta)
      character*1 ns, ew
      character*4 t3
      character*32 desc
      logical f00
      namelist /namken/nstart,nend,nint,nout,nzero,f00,
     &                 nsfc,lonf,latg
      CALL W3TAGB('METEOFLX',1999,0203,0081,'NP23')
      imask = 1
      read(5,namken)
      write(6,namken)
      npoint = 0
   99 FORMAT (I6, F6.2,A1, F7.2,A1, A4, A32, I4)
      do np = 1, nsta + 2
        read(8,99,end=200) IST, ALAT,NS, ALON,EW, T3,DESC,IELEV
        if(alat.lt.95.) then
          npoint = npoint + 1
                            RLA = 9999.
          IF (NS .EQ. 'N')  RLA =  ALAT
          IF (NS .EQ. 'S')  RLA = -ALAT
                            RLO = 9999.
          IF (EW .EQ. 'E')  RLO =  ALON
          IF (EW .EQ. 'W')  RLO = -ALON
          rlat(npoint) = rla
          rlon(npoint) = rlo
          istat(npoint) = ist
        endif
      enddo
 200  continue
      if(npoint.le.0) then
        print *, ' station list file is empty, abort program'
        call abort
      elseif(npoint.gt.nsta) then
        print *, ' number of station exceeds nsta, abort program'
        call abort
      endif
      call kenmain(npoint,nsfc,lonf,latg,rlat,rlon,istat,
     &             nstart,nend,nint,nout,nzero,f00,imask)
      CALL W3TAGE('METEOFLX')
      end
