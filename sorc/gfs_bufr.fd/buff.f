      subroutine buff(nint,nend,npoint,idate,jdate,levs,
     &           dird,lss,istat,sbset,seqflg,clist,npp,wrkd)
      character*150 dird, fnbufr, fmto
      integer nint, nend, npoint, idate(4), levs, jdate
      real*8 data(6*levs+24), wrkd(1)
      integer idtln, nf, nfile, np, kf
      integer lss, istat(npoint), ios
      CHARACTER*150  FILESEQ
      CHARACTER*8      SBSET
      LOGICAL         SEQFLG(4)
      CHARACTER*80     CLIST(4)
      INTEGER            NPP(4)
      CHARACTER*8     SEQNAM(4)
        FMTO = '(A,".",I6.6,".",I10)'
        idtln = 8
        nfile = 20
C        print *, 'inside buff.f nend and nint,npoint,lss,jdate='
C        print *, nend, nint,npoint,lss,jdate
        do nf = 0, nend, nint
          nfile = nfile + 1
          rewind nfile
        enddo
        do np = 1, npoint
C       OPEN BUFR OUTPUT FILE.
        write(fnbufr,fmto) dird(1:lss),istat(np),jdate
        print *, ' fnbufr =', fnbufr
        open(unit=19,file=fnbufr,form='unformatted', 
     &     status='new', iostat=ios)
        IF ( ios .ne. 0 ) THEN
            WRITE (6,*) ' CANNOT open ', 19
            STOP
        END IF
        CALL OPENBF ( 19, 'OUT', 1 )
          nfile = 20
          do nf = 0, 120, 1
            nfile = nfile + 1
            kf = nfile - 20
c           read(nfile,end=300) data
            read(nfile) data
C              print *, 'read in fort(nfile) =', nfile
            if(np.eq.1) then
              print *, ' creating bufr file for np, nfile =',
     &          np, nfile
            endif
CC          WRITE DATA MESSAGE TO BUFR OUTPUT FILE.
CC          LUNTBL=-9     BECAUSE BUFR TABLE FILE NOT USED HERE.
CC          SEQNAM=XXXXXX BECAUSE MNEMONIC SEQUENCE NAMES NOT USED HERE.
            CALL BFRIZE ( -9, 19, SBSET,
     &                 idate(4), iDATE(2),
     &                 iDATE(3), iDATE(1),
     &                 'XXXXXX', SEQFLG, 4, .FALSE., DATA, levs,
     &                 CLIST, NPP, WRKD, IRET )
            IF ( IRET .NE. 0 ) THEN
              PRINT *,' BFRIZE FAILED '
            ENDIF
c 300        continue
          enddo
C 3hourly output starts here
            nfile = 141
          do nf = 123, nend, 3
            nfile = nfile + 1
            kf = nfile - 20
            read(nfile) data
C              print *, 'read2 in fort(nfile) =', nfile
CC          WRITE DATA MESSAGE TO BUFR OUTPUT FILE.
CC          LUNTBL=-9     BECAUSE BUFR TABLE FILE NOT USED HERE.
CC          SEQNAM=XXXXXX BECAUSE MNEMONIC SEQUENCE NAMES NOT USED HERE.
            CALL BFRIZE ( -9, 19, SBSET,
     &                 idate(4), iDATE(2),
     &                 iDATE(3), iDATE(1),
     &                 'XXXXXX', SEQFLG, 4, .FALSE., DATA, levs,
     &                 CLIST, NPP, WRKD, IRET )
            IF ( IRET .NE. 0 ) THEN
              PRINT *,' BFRIZE FAILED '
            ENDIF
          enddo
          CALL BFRIZE ( 0, 19, SBSET,
     &                 IDATE(4), IDATE(2),
     &                 IDATE(3), IDATE(1),
     &                 'XXXXXX', SEQFLG, 4, .FALSE., DATA, levs,
     &                 CLIST, NPP, WRKD, IRET )
          call closbf(19)
        enddo
      return
      end
