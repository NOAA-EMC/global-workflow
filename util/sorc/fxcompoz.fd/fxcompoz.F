       PROGRAM FXCOMPOZ
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: FXCOMPOZ     TO COMPOSE UP-TO-4 PARTS OF A FAX CHART
C   PRGMMR: HENRICHSEN       ORG: NP12        DATE: 1999-11-15  
C
C ABSTRACT: TO EMULATE WHAT CLIFF FRIDLIND DOES TO COMPOSE ONE FAX-
C   TRANSMITTED CHART FROM UP-TO-FOUR COMPONENTS.  FOR THE PURPOSE
C   OF VERIFYING THAT OUR "ISCHED" CONSTANTS, WHICH ARE INCLUDED IN
C   THE TRAILER RECORD OF NMC 6-BIT ENCODED FAX PRODUCTS, ARE CORRECT.
C
C PROGRAM HISTORY LOG:
C   96-08-22  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   97-02-21  SHIMOMURA - MODIFIED TO DO DOUBLE SCALE OPTION
C   97-07-08  SHIMOMURA - renamed fxcompos.f to fxcompoz.f so that
C                         I could add another output unit for IFID
C                         which I need to pass to Program ncdpur6b
C   97-08-26  LIN         FIX THE DOCBLOCK.
c   98-08-11  caruso      removed CDIR$ INTEGER=64 from top of this
c                         code.  upgrading to fortran 90 and making
c                         y2k-compliant.
C 1999-11-05  HENRICHSEN  modified to run on the IBM SP MODIFY
C                          DEFINED THE FUNCTION iargc and 
C                          nargsinline integer(4) IN SUB ALLARGV.
C                          ALSO MODIFIED ALL integer ARGS TO THE 
C                          COMMENTED OUT COMMON /ARGBFINC/ !
C 1999-11-15  HENRICHSEN  modified TO USE DIRECT ACCESS IO ON UNIT 66.
C                                           
C
C USAGE:    
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     assign -R
C     a direct access file outfinam1.pur     u:66
C     assign -a  outfinam2.crd -s text       u:67
C     assign -a  outfinam3.fid -s text       u:68
C
C     fxcompoz.x infinam1.pk6 [infinam2.pk6 infinam3.pk6 infinam4.pk6] \
C                      parm='ssno=1288'
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C          (In order to display the results on the HP-LJ printer,)
C
C     dispuras.x outfinam1.pur raster.pcl parm='lrecl=216,byskp=0,\
C        byshw=216,lnskp=0,lnshw=775,dotpi=200,swaby=0,hdrby=0,large=1'
C
C     lpr -h -P dataman1 raster.pcl
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C   INPUT FILES:
C     infinam1.pk6 - input filename of the NMC 6-bit packed FAX map file
C                        Must have at least one source data file.
C    [infinam2.pk6] - Other source-data files are optional, depending
C    [infinam3.pk6]      on how the components of the composite chart
C    [infinam4.pk6]      are distributed among the several files.
C         
C
C   OUTPUT FILES:
C     outfinam1.pur - destination file for pure-raster, uncompressed,
C                        product containing the one composited chart.
C                        assign -a  outfinam1.pur -s unblocked  u:66
C
C     outfinam2.crd - output file containing one "card" of 72 characters
C                        containing, among other ASCII data, the width 
C                        (which for this application is always 1728 pels
C                        in 27 I*8 integer words), and the height 
C                        (the number of scanlines) of the pure-raster 
C                        image contained in outfinam1.pur;
C                        This card is grouped into 8-character "words":
C                        word(5) = scanlines-count (in ASCII),
C                                   right-justified in the word;    
C                        word(6) = '00000027' = (in ASCII) which is 
C                                   the scanline width (in I*8 words)
C                        
C                        assign -a  outfinam2.crd -s text       u:67
C
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C   EXIT STATES:
C     ISTOP=   0 - SUCCESSFUL RUN
C
C REMARKS: 
C   (Here is an example of contents of outfinam2.crd:)
C     (1)      (2)      (3)      (4)      (5)      (6)      (7)
C   PURERAST 00020925 00000000 ngmfax   00000775 00000027 00001728 
C   960822  
C     (8)      (9)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C                                                      27-Jun-1996/dss
C   INPUT ARGUMENT LIST:
C     parm     - the one parm field for the Subset No. is required;
C                   since that Subset No. will point to the main
C                   subset of the composite chart which we will make;
C                   (the "1288" is an example of a subset number).
C
C
C      ... copied ~/ig2/pcl/cry/fxdumpC.f
C            into ~/ig2/ncod/chk/fxcompos.f
C      ...   in order to build, on that program base, a FAXCUT logic 
C      ...   Purpose: to check the ISCHED and the strip-titles by
C      ...            emulating Cliff Fridlind 
C      ...            to compose a FAX transmission from its component
C      ...            subsets and insets;
C
C                                                      25-Jun-1996/dss
C      ... copied ~/pcl/cry/vxdumpC.f into fxdumpC.f in order to make a
C      ...   FAX dumper which would work with 1440-byte records instead
C      ...   of the 512-byte records of vxdump
C
C      ... mods for sys upgrade of 3-June-1996          5-June-1996/dss
C      ...   which killed all char args into ***99.c
C
C      ... file dumper in the VAX-style of printout    19-Jul-1995/dss
C
C
C      ...............................................................
C      ... PLAN ...
C      Modify this to not dump the records; with exceptions;
C      more like the "scan" function;   
C      Scan up to 4 input FAX files;
C         save the pointers found during scan in a database;
C         save the IFIDs in a database;
C         save the ISCHEDs in a database;
C              Database structure:
C              I*8 Product_outline(MAXITMDBA,LMTPRODTOT)
C
C              I*8 IFID(6,LMTPRODTOT)
C              I*8 ISCHED(180,LMTPRODTOT)
C              C*112 CINPFINAM(MAXNFILS)
C              I*8   KUNINP(MAXNFILS)  / 31,32,33,34 /
C              L     LOPENU(MAXNFILS)
C
C              MAXNFILS=4
C              MAXNPRODS=20   	!... N PER INPUT FILE
C              LMTPRODTOT = MAXNFILS*MAXNPRODS
C              MAXITMDBA = 10
c
C         DO for as many as given input filenames(up to Max=4):
C              ----------------------------------------
C              ... for every product (MAXPRODS=20) within the file ...
C              (1.) File_seq. no.
C              (2.) Product seq. no. within File Seq No.
C              (3.) Rec. No. of Start-of-Map Block;
C              (4.) Ptr to saved IFID;
C              (5.) Rec. No. of Strip-Titles Block;
C              (6.) Rec. No. of ISCHED Block;
C              (7.) Ptr to saved ISCHED;
C              (8.) FLAG whether this is last product in file, or not
C                        =0;  not last product in file
C                        =Rec. No. of END-of-ALL-MAPS block
C                        = -1; Hit End-of-file instead of EOAM
C              (9.)  Derived No. of main-map blocks in this product
C              (10.) Derived No. of Strip-title blocks in this product 
C              ----------------------------------------
C              repeat (1.) to (10.) as many times as necessary
C                     until End-of-all-maps block is found
C                     of until physical E-O-F
C         ENDDO     
C
C         
C         ... Dump the ISCHEDs as you are scanning the file
C          
C         ... After everything has been read in, then              
C         ... Analyze the ischeds;    (look for obvious errors)
C         ...   look for linked items;
C         ...      print the linkages found;
C         ...         (do not try to do any more than that yet)
C
C      ...............................................................   
C      ... FUNCTIONS ...
       external  lastch
       integer   lastch
       external  notrail
       integer   notrail
       
       integer open99
       external   open99
       integer close99
       external   close99
       integer rd1byt99
       external   rd1byt99
       integer reposi99
       external   reposi99
       integer rewind99
       external   rewind99
       integer wheren99
       external   wheren99
       
C
C      . . . . . . . . . . . . . . . . . . .
C      ==============================================================
       integer    LUNOUT		!... output pure-raster file
       parameter (LUNOUT=66)
       INTEGER    LUNSPEC		!... header-like specs of pure
       PARAMETER (LUNSPEC=67)
       INTEGER    LUNIFID
       PARAMETER (LUNIFID=68)

       INTEGER    MAXNFILS
       PARAMETER (MAXNFILS=4)

       INTEGER    MAXNPRODS
       PARAMETER (MAXNPRODS=20)   	!... MAX PRODS PER FILE

       INTEGER    LMTPRODTOT
       PARAMETER (LMTPRODTOT=MAXNFILS*MAXNPRODS)   	!... 80 = 4*20

       INTEGER    MAXITMDBA
       PARAMETER (MAXITMDBA=10)

       COMMON  /PRODDESC/numprods,IFID(6,LMTPRODTOT),
     1                            ISCHED(180,LMTPRODTOT),
     2                            IPROD_DBA(MAXITMDBA,LMTPRODTOT)

C      ==============================================================
       INTEGER    IMAXWIDWRD
       PARAMETER (IMAXWIDWRD=27)  	!...27*64 = 1728 PELS WIDE
       INTEGER    JMAXHGTLNS
       PARAMETER (JMAXHGTLNS=7500)

       COMMON    /SPAC4IMG/ NPELS_WID, NLNS_NEED, JLNADDCON, JLN_CURR,
     1                      IMAGE(IMAXWIDWRD,JMAXHGTLNS)

C      ==============================================================
C
       integer      kbytpwrd
       parameter   (kbytpwrd=8)     	!... CRAY version
       integer      maxnwrds
       parameter   (maxnwrds=112/kbytpwrd)
       integer      narg_lmt
       parameter   (narg_lmt=10)

       integer   krecsizbyt
       parameter   (krecsizbyt=1440)
       integer      krecsizwrd
       parameter   (krecsizwrd=krecsizbyt/kbytpwrd)
       

       INTEGER        MAXNREM
       PARAMETER     (MAXNREM=200)   	!... 200 LINES ALLOC FOR REMARKS


C      --------------------------------------------
C      . . .   CALL SEQ FOR SUBR FINDLINK() . . . .
       INTEGER      MAXPLTARG   	!... I-DIMEN OF IPLOTARG(I,4)
       PARAMETER   (MAXPLTARG=20)

       INTEGER      MAXNPART
       PARAMETER   (MAXNPART=4)

       INTEGER      ISSNO
       integer      npartfound
       integer      iplotarg(MAXPLTARG,MAXNPART)
       INTEGER      IRET_FND
C      --------------------------------------------
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . used by reader of input file  . . . . .
C       COMMON /ARGBFINC/ LUNINP,INISOPNQ,NBUFINP,
C     1                   istartrec,nrecdo,jcushion,
C     2                   cfinm_inp

       LOGICAL       INISOPNQ
       INTEGER       LUNINP
       integer    NBUFINP
       integer    istartrec
       integer    nrecdo
       integer    jcushion
       character*112 cfinm_inp
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... call seq. args for allargv():

       integer      jarg(maxnwrds,narg_lmt)
       character*112 carg(narg_lmt)
       equivalence (jarg,carg)

       integer      narg_got
       integer      iret_args
C
C
       integer      nchars
       character*1  NULLCHR


C      . . . . . .  keyvals()  . . . . . . . . . . . . . . . . . . .
       integer        maxnkeys
       parameter     (maxnkeys=2)

       character*5    keyword(maxnkeys)
       data           keyword   / 'ssno ','dumy ' /

       integer        ixssno
       data           ixssno   / 1 /

       integer        ixdumy
       data           ixdumy   / 2 /

       logical        foundkey(maxnkeys)
       integer        numvalkey(maxnkeys)

       integer        many_recs
       data           many_recs    / 5999 /
C      . . . . . .  keyvals()  . . . . . . . . . . . . . . . . . . .

       integer        intarray(krecsizwrd)
       character*1    c1array(krecsizbyt)
       equivalence   (intarray,c1array)

       integer        i8dump(14)
       character*112  c112dump
       equivalence   (i8dump(1),c112dump)

       integer        itmpifid(6)
       character*1    c1ifidisp(48)
       equivalence   (itmpifid(1),c1ifidisp(1))

       character*1    c1ifidasc(48)
       CHARACTER*48   C48IFIDASC
       CHARACTER*53   MAPNAME
       EQUIVALENCE   (C48IFIDASC,C1IFIDASC)

       INTEGER        NCHREMARK
       INTEGER        NUMREM

       INTEGER        MSK4BITS
       DATA           MSK4BITS    / X'000000000000000F' /

       INTEGER        MSKFFFFF
       DATA           MSKFFFFF    / X'FFFFF00000000000' /

       INTEGER        KLANKS
       DATA           KLANKS      / X'2020202020202020' /

       INTEGER        KIND
       INTEGER        IXKIND
       CHARACTER*7    CBLOKHDR(5)
       DATA           CBLOKHDR    / 'START_M', 
     2                              'STITLES',
     3                              'ISCHEDS',
     4                              'END_ALL',
     5                              'UNKNOWN' /

       INTEGER        LOGREM(10,MAXNREM)
       CHARACTER*80   CLOGREM(MAXNREM)
       EQUIVALENCE   (LOGREM(1,1),CLOGREM(1))

       LOGICAL        LDUMMYQ
       INTEGER        ISCHDUMMY(24)
       DATA           ISCHDUMMY   / 
     1                         X'00A2000000400E10',X'8000310000000000',
     2                         X'0E73001F0703079E',X'81516C00FFE21372',
     3                         X'1372009500040019',X'0100D86C00010E74',
     4                         X'0E74001F0703079E',X'41436C00079E1373',
     5                         X'1373009702740019',X'0000D80000000000',
     6                         X'0ECA000406090AF0',X'8100D800FFE20F26',
     7                         X'0F26009904DF0019',X'0100D8000AF00F28',
     8                         X'0F28009C019B0019',X'0000D80000000000',
     9                         X'0E9A000406090AF0',X'8180D800FFE20EE4',
     A                         X'0EE4009E05D70019',X'0100D8000AF00EE6',
     B                         X'0EE600A1029C0019',X'0000D80000000000',
     C                                           0,0                  /
       integer        iret_key
       integer     iret_wher
       integer     iret_repo
       integer     iret_open
       integer     iret_clos
       integer     iret_rds
       integer     iret_rew
       integer     loc_EOF


       integer        jstartrec
       integer     ioffset
       integer        ibytoffset0
       integer        nbytoffset9
       integer     index_from
       integer        ivaltail
       integer        ivalrec2
       character*3    ciomode
       integer     numbyts
       integer     numbytsred
       integer        mrecno
       integer        ivalscan
       logical        lskip_prntq
       LOGICAL        LVARIANQ
       logical        lparmqq

       CHARACTER*112  CINPFINAM(MAXNFILS)
       INTEGER        KUNINP(MAXNFILS)
       DATA           KUNINP          / 31, 32, 33, 34 /
       LOGICAL        LOPENU(MAXNFILS)
       INTEGER        NTOTINPFIL

       integer        jextrema(2,MAXNPART)
       integer        jlowestln
       integer        jhightestln
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C       . . . . . .   for the output pure-raster file specs . . . . .

        INTEGER        KDIMHED     	!... no. of I*8 words in header
        parameter     (KDIMHED=9)
C
        INTEGER        JHED1D(kdimhed)  	!... 9 longwrd = 72 byt
        CHARACTER*8    CHED(kdimhed)
        EQUIVALENCE   (JHED1D,CHED)
        CHARACTER*72   CHED1D
        EQUIVALENCE   (JHED1D(1),CHED1D)

        INTEGER        NWDHED
        INTEGER        IRET_HDR

        integer        lwr_or_upr
        integer        nchinpfn
        integer        nchstripped
        integer        iret_strp
        character*16   cstrmapnam
        character*8    c8mapnam

        integer      ntotwrds
        integer      iacc
        character*8  cacc
        equivalence (iacc,cacc)
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       integer        istat
       
       CHARACTER*44  STRMSG

       SAVE

       CALL W3TAGB('FXCOMPOZ',1999,0319,0080,'NP12   ')
        
       WRITE(STRMSG(1:44),
     1  FMT='(''RUNNING FXCOMPOZ IBM VERSION 1999.319 1425L:'')')
C              '12345678901234567890123456789012345678901234'
C               1234567890123456789 
C---------------------------------------------------------------
C     . . .   S T A R T   . . .
C---------------------------------------------------------------
       WRITE(6,FMT='('' MAIN: '',A)')STRMSG(1:44)
       
      CALL CONSOL(STRMSG)

C
       istat = 0
       LDUMMYQ = .FALSE.

       NULLCHR = char(0)
       NUMREM = 0

       lskip_prntq = .TRUE.

C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... insert logic of teskeyva.f here which will get PARM
C      ... from command line

       call ekeyvals(maxnkeys,keyword,foundkey,numvalkey,iret_key)

       write(6,135)iret_key
  135  format(' fxcompoz::keyvals: returned w/ code=',I5)
       if(iret_key .ne. 0) then
         istat = 1
         go to 999
       endif
C
       do  ik = 1,maxnkeys
         if(foundkey(ik)) then

           write(6,145)ik,keyword(ik)(1:lastch(keyword(ik))), 
     *                 numvalkey(ik)
  145      format(' fxcompoz:',I3,'... "',A,'" =',I9)
         else
C          ... failed to find a match for this keyword ...
           write(6,155)keyword(ik)(1:lastch(keyword(ik)))
  155      format(' fxcompoz: did not find, in cparm, the', 
     *            ' keyword= "',A,'"')
         endif
       enddo

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... the starting and ending records:
C      ... default for    starting output record number = 1;
C      ... default for number of output records to scan = 5999;
C      ... where output record is in krecsizbyt=1440 bytes;


       istartrec = 1
       nrecdo = 5999
       lskip_prntq = .true.
       INISOPNQ = .false.

       lvarianq = .false.
       LDUMMYQ = .FALSE.
       if (foundkey(ixssno)) then
         issno = numvalkey(ixssno)
         if(issno .LE. 0) then
           write(6,162)issno
  162      format(' fxcompoz: Failed on given bad-valued SSNO=',I6)
           istat = 2
           go to 999
         else
           write(6,164)issno
  164      format(' fxcompoz: go with desired Subset No.=',I6)
         endif
       endif

       if (foundkey(ixdumy)) then
         ivaldumy = numvalkey(ixdumy)
         if(ivaldumy .LE. 0) then
           LDUMMYQ = .FALSE.
         else
C          ... ivaldumy = positive, non-zero, like dumy=1;
           LDUMMYQ = .TRUE.
         endif
       endif
C      . . . . . . . .
C      ...
       write(6,179)LDUMMYQ
  179  format(/,' fxcompoz: from PARM,' 
     *        /,'       Over-write sched w/ dummy data?? = .',L1,'.',
     *        /,' . . . . . . . . . . . . . . . . . . . . . . . . .')
 
C      ----------------------------------------------------------------

C      . . . .  TO GET THE INPUT FILENAME  . . . . . . . . . . . . .
       NTOTINPFIL = 0

       lparmqq = .false.

       DO  I = 1,MAXNFILS
         LOPENU(I) = .FALSE.
       ENDDO

       DO  I = 1,MAXNFILS
         CINPFINAM(I)(1:112) = ' '
       ENDDO

       call allargv(narg_lmt,carg,narg_got,iret_args)

       write(6,215)narg_lmt,narg_got,iret_args
  215  format(' Main::allargv:narg_lmt=',I3,'; narg_got=',I3,
     *        '; return_code=',Z16.16)
       if(narg_got .GT. 0) then
         do iar = 1,narg_got
           lparmqq = .false.
           nchars = lastch(carg(iar))
C
           if(nchars .le. 0) then
             write(6,225)iar
  225        format(' Main:          Empty Arg(',I3,')' )
             iwdss = 1
             write(6,231)iwdss,jarg(iwdss,iar)
  231          format(' Main: jarg(',I3,') is last word = HEX',Z16.16)
           else
             write(6,235)nchars,iar
  235        format(' Main: nchars=',I3,'; in Arg(',I3,')' )
             write(6,237)carg(iar)(1:nchars)
  237        format(1X,A)
             if(carg(iar)(1:5) .EQ. 'parm=') then
               write(6,2372)
 2372          format(' Main: this Arg is PARM; not finame')
               lparmqq = .true.
             endif
 
             iwdss = ((nchars - 1) / kbytpwrd) + 1
C            ... where iwdss points to last word so I could hex dump
C            ...    that last word, to see if NULL is there
             if(iwdss .GE. maxnwrds) then     
               write(6,231)iwdss,jarg(iwdss,iar)
C 231      ... format(' Main: jarg(',I3,') is last word = HEX',Z16.16)
             else
               write(6,2391)iwdss,jarg(iwdss,iar),jarg(iwdss+1,iar)
 2391          format(' Main: jarg(',I3,') is last word = HEX',Z16.16,
     *                1X,Z16.16)
             endif
C            ... I tried to append a NULL terminator;
C            ... Is there a NULL-terminator???
C            ... Yes there is, followed by blank fill.

C              ... expected to be the input filename for pure-ras
               if(.NOT. lparmqq) then
                 if(ntotinpfil .LT. MAXNFILS) THEN
                   ntotinpfil = ntotinpfil + 1
                   CINPFINAM(ntotinpfil)(1:112) = carg(iar)(1:112)
                 ENDIF
               endif
           endif
         enddo
       endif
C      ----------------------------------------------------------------
C      ... initialize FAX-cut constants and arrays ...
       numprods   = 0
       NPRODWIFIL = 0
       do  j = 1,LMTPRODTOT
         DO  I = 1,6
           IFID(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,LMTPRODTOT
         DO  I = 1,180
           ISCHED(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,LMTPRODTOT
         DO  I = 1,MAXITMDBA
           IPROD_DBA(I,J) = 0
C ...           IFID(I,J) = 0		!... wrong!!!!  8-JUL-1997/DSS
         ENDDO
       ENDDO
C      ----------------------------------------------------------------
C      ... OPEN THE INPUT FILE ...
       IF(NTOTINPFIL .LE. 0) THEN
         WRITE(6,FMT='('' fxcompoz: FAILED ON LACK OF INPUT '',
     1                 ''FILENAME(S)'')')
         ISTAT = 1
         GO TO 999
       ENDIF
C      ... OTHERWISE, SOME INPUT FILENAME(S) WERE GIVEN, SO OPEN ...

       DO 888 NAM = 1,NTOTINPFIL
         LUNINP   =  kuninp(NAM)
         INISOPNQ = .false.
         LOPENU(NAM) = .FALSE.
         NPRODWIFIL = 0

         cfinm_inp(1:112) = CINPFINAM(NAM)(1:112)
C      ... SHOULD INQUIRE AS TO EXISTENCE OF THIS INPUT FILE ...

         ciomode(1:3) = ' '
         ciomode(1:3) = 'r'//NULLCHR//NULLCHR         !... for READ_ONLY
         c112dump(1:112) = cfinm_inp(1:112)
         write(6,305)(i8dump(i),i=1,14)
  305    format(' tesopen: the filename in HEX is  (see next lines)',
     *       /,4Z17.16,/,4Z17.16,
     *       /,4Z17.16,/,2Z17.16)
         nchinpfn = lastch(cfinm_inp)
         nchinpfn0 = nchinpfn + 1      !... includes NULL in count
         write(6,3052)LUNINP,nchinpfn0,cfinm_inp(1:nchinpfn0),
     *                ciomode(1:3)
 3052    format(' tesopen: will call open99 w/ LUNINP=',I4,
     *          /,'   finm(1:',I5,') =',A,
     *          /,'   ciomode(1:3) = ',A)

         iret_open= open99(LUNINP,cfinm_inp(1:nchinpfn0),ciomode(1:2))

         if(iret_open .NE. 0) then
           istat = 30
           go to 8990
         else
           INISOPNQ = .TRUE.
           LOPENU(NAM) = .TRUE.
         endif

C      ... ...
C      ... do I need to assign -s unblocked ??? ...

C      ... Where is the End-of-File located?
C      ... I will reposition to EOF, then ask where am I?
         ioffset = 0
         index_from = 2      		!... from EOF

         iret_repo = reposi99(LUNINP,ioffset,index_from)

         if(iret_repo .NE. 0) then
           istat = 31
           go to 800
         endif

         iret_wher = wheren99(LUNINP, loc_EOF)
         if(iret_wher .NE. 0) then
           istat = 32
           go to 800
         endif

         write(6,315) loc_EOF
  315    format(' fxcompoz: Location of E-O-F (in bytes, ',
     *          'rel to start) =',I7)

C      ... How many FAX blocks is that?
         maxblocks = loc_EOF / krecsizbyt
         if(mod(loc_EOF,krecsizbyt) .NE. 0) then
           maxblocks = maxblocks + 1
         endif
         write(6,317)maxblocks
  317    format('        which file-size will fit within', I6,
     *          '  FAX blocks')
C
C      ... Now let us position input file ptr to equiv. of istartrec
         iret_rew = rewind99(LUNINP)
         jcushion = 0

C      ... when it comes thru those istartrec cases, the input file
C      ... is positioned for read
         numbyts = krecsizbyt
         NBUFINP = 0   
         do  ired = 1,nrecdo
           do  i = 1,krecsizwrd
             intarray(i) = 0
           enddo
           iret_rds = rd1byt99(LUNINP,numbyts,c1array,numbytsred)
#if defined(_LITTLE_ENDIAN)
           CALL SWAP_BYTES_IN_PLACE (INTARRAY, 8_8, NUMBYTSRED / 8_8)
#endif
           if(iret_rds .NE. 0) then
             if(iret_rds .EQ. -1)then
               write(6,415)NBUFINP
  415          format(' fxcompoz: Hit End-of-File after reading',I7,
     *                ' records of 1440-bytes each')
               go to 800
             else
               write(6,425)iret_rds
  425          format(' fxcompoz::rd1byt99:error code=',I5)
               istat = 36
               go to 800
             endif
           endif
           NBUFINP = NBUFINP + 1
           mrecno = NBUFINP + jcushion
C        ... 
           if(.NOT. lskip_prntq) THEN

C ...           call rec_dump(mrecno,intarray,krecsizwrd)

           ENDIF

           IF(IAND(INTARRAY(1),MSKFFFFF) .EQ. MSKFFFFF) THEN
C          ... THIS IS A SPECIALLY HEADED FAX BLOCK ...
C          ...    what kind of block header is this?
  
             KIND = ISHFT(INTARRAY(1),-40)   	!... RSHIFT 5 BYTES
             KIND = IAND(KIND,MSK4BITS)
             IXKIND = 5				!... FOR UNKNOWN
             IF(KIND .EQ. 15) THEN 		!... == X'F'
               IXKIND = 1
             ELSE IF(KIND .EQ. 14) THEN  		!... == X'E'
               IXKIND = 2
             ELSE IF(KIND .EQ. 13) THEN    	!... == X'D'
               IXKIND = 3
             ELSE IF(KIND .EQ. 12) THEN  		!... == X'C'
               IXKIND = 4
             ENDIF

             IF(IXKIND .EQ. 1) THEN 
C            ... THIS RECORD IS START OF A PRODUCT ...
               NUMPRODS = NUMPRODS + 1      
               NPRODWIFIL = NPRODWIFIL + 1  

               IPROD_DBA(1,NUMPRODS) = LUNINP
               IPROD_DBA(2,NUMPRODS) = NPRODWIFIL
               IPROD_DBA(3,NUMPRODS) = MRECNO
               IPROD_DBA(4,NUMPRODS) = NUMPRODS
               JP = NUMPRODS
               DO  IP = 1,6
                  IFID(IP,JP) = INTARRAY(IP)
               ENDDO
C            ... WHICH COPIED IFID INTO HOLDING TANK ...

             ELSE IF(IXKIND .EQ. 2) THEN
C            ... THIS RECORD IS THE STRIP-TITLES BLOCK ...
               IPROD_DBA(5,NUMPRODS) = MRECNO

             ELSE IF(IXKIND .EQ. 3) THEN
C             ... THIS RECORD IS ISCHED BLOCK ...
               JP = NUMPRODS
               DO  IP = 1,180
                  ISCHED(IP,JP) = INTARRAY(IP)
               ENDDO
C            ... WHICH COPIED ISCHED INTO HOLDING TANK ...
               isched(2,jp) = NUMPRODS
C              ... hide the backward-link from isched to product-no.
C              ...   in the unused I*8 word(2) of ISCHED(,JP)

C              . . . . . . . .  DUMMY DATA OVERWRITING REAL INPUT . . .
               IF(LDUMMYQ) THEN
                 IF(JP .EQ. 1) THEN
C                  ... OVERWRITE THE ISCHED() WITH DUMMY DATA 
C                  ...    WITH MORE INTERESTING SETS ...
                   DO  IP = 1,24
                     ISCHED(IP+2,JP) = ISCHDUMMY(IP)
                   ENDDO
                 ENDIF
               ENDIF
C              . . . . . . . . . . . . . . . . . . . . . . . . . . . .

               IPROD_DBA(6,NUMPRODS) = MRECNO
               IPROD_DBA(7,NUMPRODS) = NUMPRODS
              
C             ... DUMP (PRINT) THIS ISCHED RECORD ...
               call rec_dump(mrecno,intarray,krecsizwrd)

             ELSE IF(IXKIND .EQ. 4) THEN
C            ... THIS RECORD IS END-OF-ALL-MAPS BLOCK ...

               IF ( NUMPRODS .EQ. 0 ) THEN
                    NUMPRODS = 1
               END IF 

               print*,'In fxcompoz.F after END-OF-ALL-MAPS 
     &                 NUMPRODS = ',NUMPRODS
               IPROD_DBA(8,NUMPRODS) = 1

             ENDIF    		!... END OF IXKIND CASES ...

             IF(NUMREM .LT. MAXNREM) THEN
               NUMREM = NUMREM + 1

               DO  I = 1,10
                 LOGREM(I,NUMREM) = KLANKS
               ENDDO
               NCHREMARK = 56 + 6

               write(clogrem(numrem)(1:nchremark),fmt='(Z17.16,
     *               '' ... FOUND '',A7,''  IN REC. NO. ='',I5,
     *               '';  U:'',I2)')
     *               INTARRAY(1),CBLOKHDR(IXKIND), MRECNO,LUNINP
             ENDIF
           ENDIF
         ENDDO


  800    CONTINUE

         WRITE(6,815)NBUFINP,LUNINP
  815    FORMAT(' fxcompoz:after reading NBUFINP=',I7,
     *          ';    U:',I3)

         IF(NUMREM .GT. 0) THEN
           WRITE(6,825)
  825      FORMAT(' * * * * *   S U M M A R Y   * * * * * * * * * *')
           DO  LINE = 1,NUMREM
             NCHRE = NOTRAIL(CLOGREM(LINE))
             WRITE(6,827)CLOGREM(LINE)(1:NCHRE)
  827        FORMAT(A)
           enddo
           WRITE(6,FMT='('' * * * * * * * * * * * * * * * * * * * '',
     *                     ''* * * * *'')')
         endif
  888  CONTINUE   	!... ENDDO ON INPUT FILES BY NAM = 1,NTOTINPFIL

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... ALL INPUT FILES HAVE BEEN SCANNED AND DATABASE ENTRIES MADE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       WRITE(6,FMT='(''  UNIT PRDWI START  PROD STRIP SCHED'',
     *               ''  PROD ENDALL'')') 
       JPP = 0
  890  CONTINUE
       JPP = JPP + 1
       JP = IPROD_DBA(4,JPP)
       IF(JP .GT. 0) THEN
         WRITE(6,FMT='((12I6))')
     *        (IPROD_DBA(IP,JP),IP=1,MAXITMDBA)
C         ... NO MORE ITEMS IN DATABASE ...
         GO TO 890		!... LOOP BACK FOR NEXT DBA PROD ITEM
       ENDIF

       CALL FINDLINK(ISSNO, npartfound, iplotarg, IRET_FND)

       IF(IRET_FND .NE. 0) THEN

         WRITE(6,FMT='('' fxcompoz::FINDLINK:FAILED TO FIND ALL '',
     *                 ''PARTS FOR SSNO. ='',I6)') ISSNO

         ISTAT = 3
         GO TO 8990

       ENDIF
C      ... OTHERWISE, FINDLINK WAS SUCCESSFUL ...

C      ... OUTPUT THE IFID FOR EACH PART ...


       rewind LUNIFID
       write(6,FMT='('' fxcompoz: npartfound='',I3,
     *               '';  IFIDs for each part ...'')')
     *       npartfound

       do  jp = 1,npartfound
         jprodno = iplotarg(14,jp)
         do  i = 1,6
            itmpifid(i) = IFID(i,jprodno)
         enddo
#if defined (_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (itmpifid, 8_8, 6_8)
#endif
         call cdc2asc(48,c1ifidisp,c1ifidasc,iret_cdc)
   
C       MAKE MESSAGE FOR CONSOL.

          MAPNAME(01:07) = 'Faxmap '
          MAPNAME(08:52) = c48ifidasc(04:48)
           iend = 53
           kend = 52
         do  k = 1,iend
             if(MAPNAME(kend:kend) .ne. ' ')then
              ilastbyt = kend + 1
              MAPNAME(ilastbyt:ilastbyt) = ':'              
              goto 100
             else
              kend = kend - 1
             endif
         enddo
 100     continue
                     
          write(6,FMT='('' FXCOMPOZ: ilastbyt='',i3,A)')
     *          ilastbyt,MAPNAME(01:ilastbyt)
         CALL CONSOL(MAPNAME)          
         c48ifidasc(1:3) = 'FFF'
         write(6,FMT='(1x,I3,''... prodno='',I4,'': '',A48)')
     *           jp,jprodno,c48ifidasc(1:48)
     


         write(LUNIFID,8948,ERR=920) C48IFIDASC(1:48)
 8948    format(A48)
       enddo
       ENDFILE LUNIFID       

C      ... DETERMINE HOW MANY SCANLINES NEEDED FOR THIS COMPOSITE FAX

       do  j = 1,MAXNPART
         jextrema(1,j) = 0
         jextrema(2,j) = 0
       enddo    

       do  jpart = 1,npartfound
         jextrema(1,jpart) = iplotarg(3,jpart)
         if(iplotarg(13,jpart) .NE. 0) then
C          ... doubling option ... so needs double the hgt
           jextrema(2,jpart) = jextrema(1,jpart) + 2*iplotarg(6,jpart)

         else
C          ... the usual not-doubled case,
           jextrema(2,jpart) = jextrema(1,jpart) + iplotarg(6,jpart)
         endif
       enddo

       jlowestln  = jextrema(1,1)
       jhighestln = jextrema(2,1)
       if(npartfound .GT. 1) then
         do jj = 2,npartfound
           if(jextrema(1,jj) .LT. jlowestln) then
             jlowestln = jextrema(1,jj)
           endif
           if(jextrema(2,jj) .GT. jhighestln) then
             jhighestln = jextrema(2,jj)
           endif
         enddo
       endif
C      ... what additive constant, when added to jlowestln-value,
C      ...   will yield a line subscript value of (1)  ???
       jlnaddcon = 1 - jlowestln
       nlns_need = jhighestln + jlnaddcon - 1
       jln_curr = 0
       npels_wid = 64*IMAXWIDWRD

       write(6,FMT='('' fxcompoz: preparing image space for'',I6,
     *               '' pels by '',I6,'' scanlines'',
     *          /,7X,''jlowestln='',I6,'';  jhighestln='',I6,
     *               ''; jlnaddcon='',I6)')
     *         npels_wid, nlns_need, jlowestln, jhighestln, jlnaddcon

       do  j = 1,nlns_need
         do  i = 1,IMAXWIDWRD
           image(i,j) = 0
         enddo
       enddo
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... to adapt the pure-raster file header logic
C      ...    to the fxcompoz environment, in which I want to
C      ...    write the header info to a separate file as text
C      ...      (all I really need is width, height)

       lwr_or_upr = 1
       nchinpfn = lastch(cinpfinam(1))
       call strpfnam(lwr_or_upr,cinpfinam(1)(1:nchinpfn),nchinpfn,
     *               cstrmapnam,nchstripped,iret_strp)

       c8mapnam(1:8) = ' '
       ncs = nchstripped
       if((ncs .LE. 0) .OR.(iret_strp .NE. 0)) then
         write(6,FMT='('' scn_x6bB::strpfnam: Failed to '',
     *                 ''extract innermost filename;'',
     *            /,7X,''nchstripped='',I4,'';  iret_strp='',I4)')
     *                   nchstripped,iret_strp
         c8mapnam = 'unknown '
       else if(ncs .GT. 8) then
         ncs = 8
         c8mapnam(1:8) = cstrmapnam(1:8)
       else
         c8mapnam(1:) = cstrmapnam(1:ncs)
       endif          

       write(6,FMT='('' scn_x6bB: before mkpurhdr; c8mapnam= "'',
     *               A,''"'')')
     *               c8mapnam(1:8)
       npxlpoutlin = 1728
       nwdpoutlin = npxlpoutlin / 64

       call mkpurhdr(c8mapnam,nwdpoutlin,npxlpoutlin,CHED,KDIMHED,
     *               NWDHED,IRET_HDR)

       IF(IRET_HDR .NE. 0) THEN
         WRITE(6,FMT='('' scn_x6bB::mkpurhdr: WARNING code ='',I4)')
     *              IRET_HDR

       endif
C         ... where do I put the HEIGHT of product???           
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C          ... At this point I should set the total word count 
C          ...    in the HEADER RECORD.

       NTOTWRDS = nlns_need * nwdpoutlin
       NCH = 8
       CALL BIN2CH(NTOTWRDS,CACC(1:8),NCH,'R999')
       ched(2)(1:8) = cacc(1:8)

       NCH = 8
       CALL BIN2CH(nlns_need,CACC(1:8),NCH,'R999')
       ched(5)(1:8) = cacc(1:8)   	!... count of scanlines in here

       write(6,FMT='('' fxcompoz: specs of pure-raster output:'',
     *              /,8(A8,1X), /,8(A8,1X))')
     *              (ched(i)(1:8),i=1,KDIMHED)
       write(LUNSPEC,8955,ERR=910) CHED1D(1:72)
 8955  format(A)      
        
C     OPEN THE OUT PUT RASTER FILE.  
    
      OPEN (LUNOUT, ACCESS='DIRECT', RECL=216 )
      NRECRAS = 0
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       DO  JPART = 1,NPARTFOUND
         call vx6b_pur('init',iplotarg(1,jpart),MAXPLTARG,iret_pur)

         CALL PLOTPART(JPART,npartfound,IPLOTARG,IRET_PLP)
       ENDDO

       do  j = 1,nlns_need
        NRECRAS = NRECRAS + 1
#if defined(_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (image(1,j), 8_8, IMAXWIDWRD)
#endif
         write(LUNOUT,REC=NRECRAS,err=900)(image(i,j),i=1,IMAXWIDWRD)
       enddo

 8990  CONTINUE
C      ... COMES HERE TO CLOSE ANY OPEN INPUT FILES
       DO  IU = 1,MAXNFILS
         IF(LOPENU(IU)) THEN
           LUNINP = KUNINP(IU)
           iret_clos = close99(LUNINP)
           WRITE(6,FMT= '('' fxcompoz: after CLOSE99('',I3,
     *                    '') retcode='',I7 )')
     *             LUNINP,iret_clos
         ENDIF
       ENDDO
       GO TO 999

  900  continue
       write(6,905)LUNOUT
  905  format(' fxcompoz:FAILED on WRITE PARITY ERROR on UNIT=',I3)
       istat = 66
       go to 999
  910  continue
       write(6,915)LUNSPEC
  915  format(' fxcompoz:FAILED on WRITE PARITY ERROR on UNIT=',I3)
       istat = 67
       go to 999
  920  continue
       write(6,925)LUNIFID
  925  format(' fxcompoz:FAILED on WRITE PARITY ERROR on UNIT=',I3)
       istat = 68
       go to 999

  999  continue
       CALL W3TAGE('FXCOMPOZ')
       CALL ERREXIT(istat)
       STOP
       END
