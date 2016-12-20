      program meteormrf
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM: METEOMRF
C   PRGMMR: PAN              ORG: NP23        DATE: 1999-07-21
C
C ABSTRACT: Creates BUFR meteogram files for the AVN and MRF.
C
C PROGRAM HISTORY LOG:
C   99-07-21  Hualu Pan
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
      use sigio_module
      implicit none
      include 'mpif.h'
      integer,parameter:: nsta=3000
      integer(sigio_intkind),parameter:: lusig=11
      integer(sigio_intkind):: irets
      type(sigio_head):: sighead
      integer ncfsig, nsig
      integer istat(nsta), idate(4), jdate
      integer :: iromb, maxwv, levs,nstart,nend,nint,nsfc,levsi
      integer :: kwskip,npoint,np,ist,is,iret,lss,nss,nf,nsk,nfile
      integer :: ielev
      real :: alat,alon,rla,rlo
      real :: wrkd(1),dummy
      real rlat(nsta), rlon(nsta), elevstn(nsta)
      character*1 ns, ew
      character*4 t3
      character*32 desc
      character*150 dird, fnsig
      logical f00, makebufr
      CHARACTER*150  FILESEQ
      CHARACTER*8      SBSET
      LOGICAL         SEQFLG(4)
      CHARACTER*80     CLIST(4)
      INTEGER            NPP(4)
      CHARACTER*8     SEQNAM(4)
      integer ierr, mrank, msize
      integer n0, ntot
C
      DATA             SBSET / 'ABCD1234' /
C
      DATA            SEQFLG / .FALSE., .TRUE., .FALSE.,  .FALSE. /
C
      DATA            SEQNAM / 'HEADR', 'PROFILE', 'CLS1' ,'D10M' /
c      DATA         SEQNAM / 'HEADR', 'PRES TMDB UWND VWND SPFH OMEG',
c     &                      'CLS1' ,'D10M' /
C
      namelist /nammet/ iromb, maxwv, levs, makebufr, dird,
     &                  nstart, nend, nint, nsfc, f00

      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,mrank,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,msize,ierr)
      if(mrank.eq.0) then
        CALL W3TAGB('METEOMRF',1999,0202,0087,'NP23')
      endif
      read(5,nammet)
      write(6,nammet)
      kwskip = (maxwv + 1) * ((iromb+1) * maxwv + 2)
      npoint = 0
   99 FORMAT (I6, F6.2,A1, F7.2,A1,1X,A4, A31, I4)
      do np = 1, nsta+2
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
          elevstn(npoint) = ielev
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
      if (mrank.eq.0.and.makebufr) then
        REWIND 1
        READ (1,100) SBSET
  100   FORMAT ( ////// 2X, A8 )
        PRINT 120, SBSET
  120   FORMAT ( ' SBSET=#', A8, '#' )
        REWIND 1
C
C     READ PARM NAMES AND NUMBER OF PARM NAMES FROM BUFR TABLE.
        DO IS = 1,4
           CALL BFRHDR ( 1, SEQNAM(IS), SEQFLG(IS),
     X                   CLIST(IS), NPP(IS), IRET )
           IF ( IRET .NE. 0 ) THEN
              PRINT*, ' CALL BFRHDR  IRET=', IRET
           ENDIF
        ENDDO
      lss = len ( dird )
      DO WHILE ( dird (lss:lss) .eq. ' ' )
        lss = lss - 1
      END DO
C
      endif
      nsig = 11
      nss = nstart + nint
      if(f00) nss = nstart
c     do nf = nss, nend, nint
      ntot = (nend - nss) / nint + 1
      do n0 = 1, ntot, msize
        nf = (n0 + mrank - 1) * nint + nss
c        print*,'n0 ntot nint nss mrank msize',n0,ntot,nint,
c     &  nss,mrank,msize
        if(n0.eq.1.and.mrank.gt.0) then
c          print*,'min(mrank,ntot-1) = ',min(mrank,ntot-1)
          do nsk = 1, min(mrank,ntot-1)
            read(12) dummy
          enddo
        endif
        if(n0.gt.1.and.msize.gt.1.and.nf.le.nend) then
          do nsk = 2, msize
            read(12) dummy
          enddo
        endif
        nfile = 21 + (nf / nint)
        print*, 'nfile = ',nfile
        if(nf.le.nend) then
          if(nf.lt.10) then
            fnsig = 'sigf0'
            write(fnsig(6:6),'(i1)') nf
            ncfsig = 6
          elseif(nf.lt.100) then
            fnsig = 'sigf'
            write(fnsig(5:6),'(i2)') nf
            ncfsig = 6
          else
            fnsig = 'sigf'
            write(fnsig(5:7),'(i3)') nf
            ncfsig = 7
          endif
           print *, 'Opening file : ',fnsig
          call sigio_sropen(nsig,fnsig(1:ncfsig),irets)
          if(irets.ne.0) then
           call errmsg('sighdr: error opening file '//fnsig(1:ncfsig))
           call errexit(2)
          endif
          call sigio_srhead(nsig,sighead,irets)
          if(irets.ne.0) then
           call errmsg('sighdr: error reading header from file
     &      '//fnsig(1:ncfsig))
           call errexit(2)
          endif
          levsi = sighead%levs
          call sigio_sclose(nsig,irets)
          if(irets.ne.0) then
           call errmsg('sighdr: error closing header from file
     &      '//fnsig(1:ncfsig))
           call errexit(2)
          endif
          call meteorg(npoint,rlat,rlon,istat,elevstn,
     &             nf,nfile,fnsig,jdate,idate,
     &             iromb,maxwv,kwskip,levs,levsi,nsfc,mrank)
        endif
      enddo
      call mpi_barrier(mpi_comm_world,ierr)
      call mpi_finalize(ierr)
      if(mrank.eq.0) then
      print *, ' starting to make bufr files'
      if(makebufr) call buff(nint,nend,npoint,idate,jdate,levs,
     &        dird,lss,istat,sbset,seqflg,clist,npp,wrkd)
      CALL W3TAGE('METEOMRF')
      endif
      end
