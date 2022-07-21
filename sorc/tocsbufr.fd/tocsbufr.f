      PROGRAM TOCSBUFR
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: TOCSBUFR
C   PRGMMR: GILBERT          ORG: NP11        DATE: 2004-02-23
C
C ABSTRACT: Reads each BUFR message from a standard fortran blocked (f77)
C           file and adds a TOC
C           Flag Field separator block and WMO Header in front of each BUFR
C           field, and writes them out to a new file.  The output file
C           is in the format required for TOC's FTP Input Service, which
C           can be used to disseminate the BUFR messages.
C           This service is described at http://weather.gov/tg/ftpingest.html.
C
C           TOCSBUFR contains two options that are selected using
C           a namelist on unit 5 ( see INPUT FILES below ):
C           1)  The specified WMO HEADER can be added to each BUFR
C               message in the file OR once at the beginning of the 
C               file.
C           2)  The BUFR messages can be used "as is", or if they
C               in NCEP format they can be "standardized" for external
C               users.
C
C PROGRAM HISTORY LOG:
C 2001-03-01  Gilbert      modified from WMOGRIB
C 2004-02-23  Gilbert      modified from WMOBUFR to write out BUFR
C                          messages in the NTC/FTP Input Service format
C                          instead of the old STATFILE format.
C 2005-04-07  Gilbert      This version was created from original program
C                          TOCBUFR.  A new more thorough "standardizing"
C                          routine is being used to create WMO standard
C                          BUFR messages for AWIPS.
C 2009-06-16  J. Ator      The program was modified in response to BUFRLIB
C                          changes, including a change to the WRITSA call
C                          sequence.  Also added a call to MAXOUT to stop
C                          BUFR messages larger than 10k bytes from being
C                          truncated when standardizing.  The program can
C                          now standardize BUFR messages as large as the
C                          MAXOUT limit without any loss of data.
C 2012-12-06  J. Ator      modified for WCOSS
C
C USAGE:
C   INPUT FILES:
C      5       - STANDARD INPUT - NAMELIST /INPUT/.
C                     BULHED = "TTAAII" part of WMO Header (CHAR*6)
C                     KWBX = "CCCC" orig center part of WMO Header (CHAR*4)
C                     NCEP2STD = .true.  - will convert NCEP format
C                                          BUFR messages to standard WMO
C                                          format.
C                              = .false. - No conversion done to BUFR
C                                          messages.
C                     SEPARATE = .true.  - Add Flag Field Separator and WMO
C                                          Header to each BUFR message in 
C                                          file.
C                              = .false. - Add Flag Field Separator and WMO
C                                          Header once at beginning of 
C                                          output file.
C                     MAXFILESIZE = Max size of output file in bytes.
C                                   Used only when SEPARATE = .false.
C     11       - INPUT BUFR FILE
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      6       - STANDARD FORTRAN PRINT FILE
C     51       - AWIPS BUFR FILE WITH WMO HEADERS ADDED
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - makwmo mkfldsep 
C     LIBRARY:
C       W3LIB    - W3TAGB W3UTCDAT
C                  W3TAGE 
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C             19 - ERROR READING COMMAND LINE ARGS FOR WMOHEADER
C             20 - Error opening output BUFR transmission file
C             30 - NO BUFR MESSSAGES FOUND
C
C REMARKS: This utility was written for the ETA BUFR sounding 
C          collectives, and assumes all BUFR messages in the input
C          file require the same WMO Header.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
C
      PARAMETER (MXSIZE=500000,MXSIZED4=MXSIZE/4)
      INTEGER,PARAMETER :: INBUFR=11,OUTBUFR=51,TMPBUFR=91,iopt=2
C
      INTEGER,dimension(8):: ITIME=(/0,0,0,-500,0,0,0,0/)
      INTEGER,dimension(MXSIZED4):: MBAY
      INTEGER         NBUL
      INTEGER         iday,hour
      INTEGER   ::    MAXFILESIZE=1000000
C
      CHARACTER * 80  fileo
      CHARACTER * 11  envvar
      CHARACTER * 8   SUBSET
      CHARACTER * 6 :: BULHED="CHEK12"
      CHARACTER * 1   BUFR(MXSIZE)
      CHARACTER * 4 :: ctemp,KWBX="OUTT"
      CHARACTER * 1   CSEP(80)
      integer,parameter :: lenhead=21
      CHARACTER * 1   WMOHDR(lenhead)
      character*1,allocatable :: filebuf(:)
      LOGICAL :: NCEP2STD=.false.,SEPARATE=.true.
C
      EQUIVALENCE (BUFR(1), MBAY(1))
C
      NAMELIST /INPUT/ BULHED,KWBX,NCEP2STD,SEPARATE,MAXFILESIZE
C
      CALL W3TAGB('TOCSBUFR',2012,0341,0083,'NP12')                  
C
C     Read input values from namelist
C
      READ(5,INPUT)

      PRINT *
      PRINT *,'- Adding WMO Header: ',BULHED,' ',KWBX
      IF (NCEP2STD) then
        print *,'- Convert BUFR messages from NCEP format to standard',
     &          ' BUFR Format.'
      else
        print *,'- No conversion of BUFR messages will be done.'
      endif
      IF (SEPARATE) then
        print *,'- Add Flag Field Separator and WMO Header to each ',
     &          'BUFR message in file.'
      else
        print *,'- Add Flag Field Separator and WMO Header once at', 
     &          ' beginning of file.'
        allocate(filebuf(MAXFILESIZE))
      endif
      PRINT *

C
C        Read output BUFR file name from FORT
C        environment variable, and open file.
C
      envvar='FORT   '
      write(envvar(5:6),fmt='(I2)') outbufr
      call get_environment_variable(envvar,fileo)
      call baopenw(outbufr,fileo,iret1)
      if ( iret1  .ne. 0 ) then
        write(6,fmt='(" Error opening BUFR file: ",A80)') fileo
        write(6,fmt='(" baopenw error = ",I5)') iret1
        stop 20
      endif
C
C        Open input NCEP formatted BUFR file, if NCEP2STD = .true.
C
      if (NCEP2STD) then
         call OPENBF(INBUFR,'IN',INBUFR)
         CALL MAXOUT(0)
         call OPENBF(TMPBUFR,'NUL',INBUFR)
         CALL STDMSG('Y')
      endif

C
C        Get system date and time
C 
      call w3utcdat(itime)
C
C        loop through input control records.
C
      NBUL   = 0
      nrec = 0
      itot = 0
      foreachbufrmessage: do

        if (NCEP2STD) then
           if ( IREADMG (INBUFR,SUBSET,JDATE) .ne. 0 ) exit
           if ( NMSUB(INBUFR) .gt. 0 ) then
              nrec = nrec + 1
              CALL OPENMG (TMPBUFR,SUBSET,JDATE)
              DO WHILE ( ICOPYSB(INBUFR,TMPBUFR) .eq. 0 )
                  CONTINUE
              END DO
              CALL WRITSA( (-1)*TMPBUFR, MXSIZED4, MBAY, LMBAY)
           else
              cycle
           endif
        else
           read(INBUFR,iostat=ios) BUFR
C             print *,'Error reading message from input BUFR file.',
C     &               ' iostat = ',ios
           if ( ios .le. 0 ) then
             exit
           endif
           nrec = nrec + 1
        endif
C
C  Extract BUFR edition number
        ied = iupbs01(MBAY,'BEN')
C  Calculate length of BUFR message
        if (ied.le.1) then
          call getlens(MBAY,5,len0,len1,len2,len3,len4,len5)
          ILEN = len0+len1+len2+len3+len4+len5
        else
          ILEN = iupbs01(MBAY,'LENM')
        endif
C  Check ending 7777 to see if we have a complete BUFR message
        ctemp=BUFR(ILEN-3)//BUFR(ILEN-2)//BUFR(ILEN-1)//BUFR(ILEN)
        if ( ctemp.ne.'7777') then
           print *,' INVALID BUFR MESSAGE FOUND...SKIPPING '
           exit
        endif
C
C        MAKE WMO HEADER
C
        iday=ITIME(3)
        hour=ITIME(5)
        CALL MAKWMO (BULHED,iday,hour,KWBX,WMOHDR)
C
        NBUL    = NBUL + 1
C
        IF (SEPARATE) THEN
C
C         ADD Flag Field Separator AND WMO HEADERS 
C         TO BUFR MESSAGE. WRITE BUFR MESSAGE IN FILE
C
           call mkfldsep(csep,iopt,insize,ilen+lenhead,lenout)
           call wryte(outbufr,lenout,csep)
           call wryte(outbufr,lenhead,WMOHDR)
           call wryte(outbufr,ilen,bufr)
        ELSE
C
C         APPEND NEW BUFR MESSAGE TO filebuf ARRAY
C
          if ((itot+ilen).lt.(MAXFILESIZE-101)) then
             filebuf(itot+1:itot+ilen)=BUFR(1:ilen)
             itot=itot+ilen
          else
             print *,' Internal Buffer of ',MAXFILESIZE,' bytes is ',
     &               'full.  Increase MAXFILESIZE in NAMELIST.'
             exit
          endif
        ENDIF
C
      enddo foreachbufrmessage
C
      IF (.not.SEPARATE) THEN
C
C         ADD Flag Field Separator AND WMO HEADERS 
C         TO BUFR MESSAGE. WRITE BUFR MESSAGE IN FILE
C
         call mkfldsep(csep,iopt,insize,itot+lenhead,lenout)
         call wryte(outbufr,lenout,csep)
         call wryte(outbufr,lenhead,WMOHDR)
         call wryte(outbufr,itot,filebuf)
         deallocate(filebuf)
      ENDIF
C
C*     CLOSING SECTION
C
      IF (NBUL .EQ. 0 ) THEN
        WRITE (6,FMT='('' SOMETHING WRONG WITH INPUT BUFR FILE...'',
     &         ''NOTHING WAS PROCESSED'')')
        CALL W3TAGE('TOCSBUFR')                                       
        call errexit(30)
      ELSE
        CALL BACLOSE (OUTBUFR,iret)
        WRITE (6,FMT='(//,'' ******** RECAP OF THIS EXECUTION '',
     &    ''********'',/,5X,''READ  '',I6,'' BUFR MESSAGES'',
     &    /,5X,''WROTE '',I6,'' BULLETINS OUT FOR TRANSMISSION'',
     &    //)') NREC, NBUL
      ENDIF
C
      CALL W3TAGE('TOCSBUFR')                                       
      STOP
      END
