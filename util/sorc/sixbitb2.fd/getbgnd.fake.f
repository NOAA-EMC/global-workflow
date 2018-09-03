       subroutine getbgnd(lunbgd,CPATHNAME,IWINDOW,IMAGE,IRET_BG)
C                                                       6-MAR-1996/DSS
C      ... subroutines/functions called:
C      ...      strpfnam()
C      ...      bgln_cut()
C      ...      atoi()     		!... function
C      ...      lastch()		!... function
C      ...      itoa_ds()   		!... subroutine
C      ...      ASNFILE()               !... CRAY ASSIGN subr
C
C Remarks: Krishna Kumar modified this code to run for IBM SP/RS 6000
C          This code reads the map background files in direct access
C          mode. Relevant modifications have been done in all the
C          read statements  - 08/20/1999

       integer       lastch
       external      lastch
       
       integer   (kind=8)    atoi
       external      atoi

       INTEGER       NOTRAIL
       EXTERNAL      NOTRAIL

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER       LUNBGD
       CHARACTER*(*) CPATHNAME
       INTEGER       IWINDOW(30)
       INTEGER       IMAGE(*)
       INTEGER       IRET_BG
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... INTEGER       IWINDOW(30)
C      ... IWINDOW(1) = (J_BG_SKP)              !... NSKIP BG AT START
C      ... IWINDOW(2) = (J_BG_ORG) 		!... J0 BG IN IMAGE WORK 
C      ... IWINDOW(4) = (J_FR_MAX)  = 1876      !... ENTIRE IMAGE WORK-J
C      ... IWINDOW(5) = (IPXL_BG_SKP)        !... DISCARD PXL @ LN START
C      ... IWINDOW(6) = IOPTNBITS
C      ... IWINDOW(7) = (IPXL_BG_ORG)         	!... I0 BG IN IMAGE WORK
C      ... IWINDOW(16) = (IPXL_FR_MAX) = 1728   !... MAX PXL IN RESULT
C      ... IWINDOW(15) = (IWRD_FR_MAX) = 27     !... ENTIRE IMAGE WORK-I
C      ... IWINDOW(17) = MAPBGNAME = nh2500x
C      ... IWINDOW(18) = (J_BG_SPA)          !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(19) = (IPXL_BG_SPA)       !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(20) = (IPXL_BG_CUT)       !... PXLS TO USE FROM BG LN
       
       INTEGER    BGNAME
       PARAMETER (BGNAME=17)
       INTEGER    OPTIONS
       PARAMETER (OPTIONS=6)
       INTEGER    J_FR_MAX
       PARAMETER (J_FR_MAX=4)
       INTEGER    IWRD_FR_MAX
       PARAMETER (IWRD_FR_MAX=15)
       INTEGER    IPXL_FR_MAX
       PARAMETER (IPXL_FR_MAX=16)

       INTEGER    J_BG_SPA
       PARAMETER (J_BG_SPA=18)
       INTEGER    IPXL_BG_SPA
       PARAMETER (IPXL_BG_SPA=19)

       INTEGER    J_BG_SKP
       PARAMETER (J_BG_SKP=1)
       INTEGER    IPXL_BG_SKP
       PARAMETER (IPXL_BG_SKP=5)
       INTEGER    IPXL_BG_CUT
       PARAMETER (IPXL_BG_CUT=20)

       INTEGER    J_BG_ORG
       PARAMETER (J_BG_ORG=2)
       INTEGER    IPXL_BG_ORG
       PARAMETER (IPXL_BG_ORG=7)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... IWINDOW(BGNAME) = MAPBGNAME = nh2500x
C      ... IWINDOW(OPTIONS) = IOPTNBITS
C
C
       INTEGER    LUNRAS
       PARAMETER (LUNRAS=71)  	!... CHECKOUT PURE-RASTER OUTPUT UNIT

       INTEGER    NWRDRECSIZ
       PARAMETER (NWRDRECSIZ=1024)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       INTEGER     NWRDPLIN_MXMX   	!... LONGEST LINE LENGTH OF ANY
       PARAMETER  (NWRDPLIN_MXMX=66)   	!...    MAP  (IN LONGWORDS)
 
       INTEGER     INPLINBUF(NWRDPLIN_MXMX)

       INTEGER       IHDR_BG(64)

       integer       mapcon_pur(10)  	!... for derived from ihdr_bg
   
       integer       lmt_nlines_lib
       equivalence  (mapcon_pur(5),lmt_nlines_lib)
       integer       lmt_nwrdplin_lib
       equivalence  (mapcon_pur(6),lmt_nwrdplin_lib)
       integer       LMT_NPXLPLIN_LIB
       equivalence  (mapcon_pur(7),lmt_npxlplin_lib)

       INTEGER       NLINRED
       INTEGER       NLINUSED
       INTEGER       NLINSKPED

       CHARACTER*16  CBGNAME
       character*80  cfullname
       character*132 cassi

       integer       i8acc
       character*1   c1acc(8)
       equivalence  (i8acc,c1acc(1))
       character*8   c8acc
       equivalence  (i8acc,c8acc)
       integer (kind=8) ifox
       data ifox/z'ffffffffffffffff'/

       integer       idigits(2)
       character*16  cdigits
       equivalence  (idigits(1),cdigits)

       INTEGER       JOFDEST
       INTEGER       IOPTNBITS
       INTEGER       KCYCLBIT
       DATA          KCYCLBIT         / X'0020' /


       logical       lmapbg_use
       LOGICAL       LBGEXIST
       LOGICAL       LCYCLICQ
       logical       LCKPRNTQ

       INTEGER       IOSOPEN
       INTEGER       IORSUM

       integer       lwr_or_upr
       integer       nc
       integer       iret_str

       character*1   NULL
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ... INITIALIZE ...

       NULL = char(0)
       IRET_BG = 0
       lmapbg_use = .false.

       IOPTNBITS = IWINDOW(OPTIONS)
       LCYCLICQ = .FALSE.
       IF(IAND(IOPTNBITS,KCYCLBIT) .NE. 0) THEN
         LCYCLICQ = .TRUE.
       ENDIF

C      ... INITIALIZE THE FRAMED SPACE ...
       NTOTWRDS = IWINDOW(J_FR_MAX) * IWINDOW(IWRD_FR_MAX)
       DO  I = 1,NTOTWRDS
         IMAGE(I) = 0
       ENDDO
 
C      ... GET MAP BGND NAME FROM IWINDOW(BGNAME),
       i8acc = iwindow(BGNAME)
       nc = 8
       do ir = 8,1,-1
         if((c1acc(ir) .eq. ' ') .or.
     1      (c1acc(ir) .eq. NULL)) then
           nc = nc -1
         else
C          ... is rightmost non-blank non-NULL char, so jump out
           go to 220
         endif
       enddo
  220  continue
C      ... now given map-bgnd name is in c1acc(1):(nc) ...
       if(nc .LE. 0) then
         write(6,225)
  225    format(1h ,'getbgnd: given MAP-BGND NAME is NULL')
C        ... so I cannot fetch a map bgnd.  So, continue to initialize
C        ...   image plane, but skip the laying on of the map bgnd.
         lmapbg_use = .false.
         iret_bg = 1
         go to 999   		! ... jump arnd all the map-bgnd input
       endif

C      ... to clean given map bgnd name,

       lwr_or_upr = 1   		! ... to force lower-case result

       call strpfnam(lwr_or_upr,c1acc,nc,cbgname,nchnam,iret_str)
       if(iret_str .ne. 0) then
         write(6,235)c8acc(1:nc),iret_str
  235    format(1h ,'strpfnam:FAILED to process map-bgnd name= "',A,
     1             '"',
     2        /1h ,'          with return code=',I3)
         lmapbg_use = .false.
         iret_bg = 2
         go to 999
       endif
       
C      ... MAKE FULLY QUALIFIED MAP-BGND NAME FROM MAP(1) NAME.PUR
C      ... PATH = /CODES/GRAPHICS/OK/BG/
       cfullname(1:) = cpathname(1:lastch(cpathname))// 
     1                 cbgname(1:nchnam)//'.pur'//NULL
       write(6,245)cfullname(1:lastch(cfullname))
  245  format(1h ,'getbgnd: map-bgnd filename = (see next line)',
     1       /1h ,'"',A,'"')


C      ... TEST FOR THE EXISTENCE OF SUCH A BGND NAME ...
       INQUIRE(FILE=CFULLNAME(1:LASTCH(CFULLNAME)),EXIST=LBGEXIST)
       IF(.NOT. LBGEXIST) THEN
         WRITE(6,265)
  265    FORMAT(1H ,'GETBGND: FAILED ON NON-EXISTENT MAP-BACKGROUND ',
     1              'FILE W/ NAME=',
     2         /1H ,'       "',A,'"')
         iret_bg = 3
         go to 999
       ENDIF
         
C      -------------------------------------------------------------
C      ... to compose an assign statement which will work like:
C      ...     "assign -a ../bg/nh4004.pur  -s unblocked u:11"
C      ... by composing:
C      ...     "assign -s unblocked f:fullyqualifiedname"

       cassi(1:) = ' '
       cassi(1:) = 'assign -s unblocked f:'//
     1              CFULLNAME(1:LASTCH(CFULLNAME))//NULL
ckumar       WRITE(6,271)CASSI(1:NOTRAIL(CASSI))
ckumar  271  FORMAT(1H ,'getbgnd:THE COMPOSED ASSIGN STMNT= (see next line)',
ckumar     1       /1H ,'   "',A,'"')

ckumar       CALL ASSIGN(CASSI(1:NOTRAIL(CASSI)))

C      --------------------------------------------------------------
       IOSOPEN = 0
C      ... SINCE IT EXISTS, THEN OPEN IT FOR READING ...
ckumar       OPEN(LUNBGD,FILE=CFULLNAME(1:LASTCH(CFULLNAME)),
ckumar     1      FORM='UNFORMATTED',STATUS='OLD',
ckumar     2      IOSTAT=IOSOPEN,ERR=900)
       open (lunbgd,file=CFULLNAME(1:LASTCH(CFULLNAME)),
     1    form='unformatted',access='direct',recl=8)
C      
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C
C      ... FROM MAP CONSTANTS, DETERMINE HOW MANY MAP-BGND
C      ...   SCANLINES TO SKIP AT FRONT
C      ...        nskipln = 121   !... FOR AVPOLAR
       nskipln = IWINDOW(J_BG_SKP)


C      ... when trying for multi-panel app, I got invalid header
C      ..   on second panel; so try w/ rewind ...

ckumar       REWIND LUNBGD

C      ... READ BG HEADER INTO IHDR_BG FROM ~/bg/nh2500x.pur ...
ckumar       READ(LUNBGD,ERR=910,END=920)(IHDR_BG(I),I=1,64)
ckumar
        krec=1
        do krd=1,64
        READ(LUNBGD,ERR=910,rec=krd) IHDR_BG(krd)
        krec=krec+1
        end do
ckumar
C      ... from the header determine constants of this background
C      ...   in the original library file; such as,
C      ....  HOW MANY WORDS TO READ TO GET ONE SCANLINE
       do  i = 1,10
         mapcon_pur(i) = 0
       enddo

       idigits(1) = ihdr_bg(5)  	!... =ascii '00001876'
       idigits(2) = 0   		!... to put a NULL terminator
       call byteswap(idigits(1), 8, 1)
       lmt_nlines_lib = atoi(cdigits(1:9))
C ...       mapcon_pur(5) == lmt_nlines_lib

       idigits(1) = ihdr_bg(6)   	!... =ascii '00000027'
       idigits(2) = 0   		!... to put a NULL terminator
       call byteswap(idigits(1), 8, 1)
       lmt_nwrdplin_lib = atoi(cdigits(1:9))
C ...       mapcon_pur(6) == lmt_nwrdplin_lib
       idigits(1) = ihdr_bg(7)   	!... =ascii '00001728'
       idigits(2) = 0   		!... to put a NULL terminator
       call byteswap(idigits(1), 8, 1)
       LMT_NPXLPLIN_LIB = atoi(cdigits(1:9))
C ...       mapcon_pur(7) == lmt_npxlplin_lib

       WRITE(6,325)LMT_NLINES_LIB, LMT_NWRDPLIN_LIB, LMT_NPXLPLIN_LIB
  325  FORMAT(1H ,'GETBGND: DIMENSIONS OF THIS MAP FROM LIBRARY ARE:',
     1       /1H ,'         TOTAL COUNT OF SCANLINES  =',I6, 
     2       /1H ,'         NO. OF LONG-WORDS PER LINE=',I6,
     3       /1H ,'         NO. OF PIXELS PER LINE    =',I6 )
       IF(LMT_NWRDPLIN_LIB .GT. NWRDPLIN_MXMX) THEN
         WRITE(6,335)NWRDPLIN_MXMX,LMT_NWRDPLIN_LIB
  335    FORMAT(1H ,'GETBGND:FAILED ON ILLEGAL WIDTH OF MAP BGND ...',
     1         /1H ,'       MAX POSSIBLE LINE LENGTH (IN WORDS) =',I8,
     2         /1H ,'         THIS MAP,S LINE LENGTH (IN WORDS) =',I8)
         IRET_BG = 4
         GO TO 999
       ENDIF 

C      ... READ BG INTO IMAGE FROM ~/bg/nh2500x.pur ...
       NLINRED = 0
       NLINSKPED = 0
       NLINUSED = 0
       JOFDEST = 0
       JOFSORC = 0
       IF(NSKIPLN .GT. 0) THEN
C        ... READ NO OF SKIPPED BGND SCANLINES INTO VOID
         do  j = 1,nskipln
           JOFSORC = JOFSORC + 1
ckumar           READ(LUNBGD,ERR=930,END=800)
ckumar     1                               (INPLINBUF(I),I=1,LMT_NWRDPLIN_LIB)
            krec=krec+LMT_NWRDPLIN_LIB
           NLINRED = NLINRED + 1
           NLINSKPED = NLINSKPED + 1
         ENDDO
       ENDIF
ckumar
C      ... NOW BGND INPUT FILE POINTER IS AT THE FIRST SCANLINE TO USE

C      ... FROM OTHER MAP CONSTANTS, DETERMINE WHERE IN THE FRAME DO
C      ...   I POSITION THE FIRST USED MAP-BGND SCANLINE.
       JOFDEST = IWINDOW(J_BG_ORG)

C      ... DO UNTIL REQUESTED SIZE OF BACKGROUND SPACE IS FILLED ...
       DO 477  JBU = 1,IWINDOW(J_BG_SPA)
           IF(JBU .LT. 5) THEN
             LCKPRNTQ = .TRUE.
           else
             LCKPRNTQ = .FALSE.
           ENDIF
           JOFDEST = JOFDEST + 1
C          ... CHANGE THE FOLLOWING READ TO READ INTO A WORK SCANLINE
           IF(JOFSORC .GE. LMT_NLINES_LIB)THEN
C            ... EXHAUSTED EXISTING LINES OF MAP BGND FROM LIB ...
C            ... I RAN OUT OF AVAILABLE BGND LINES BEFORE THE REQUEST
C            ... WAS SATISFIED;
             IF(LCYCLICQ) THEN
C              ... IF THE BGND IS CYCLIC, THEN REPOSITION BGND FILE
C              ... TO FIRST SCANLINE OF SOURCE AND KEEP ON READING ... 
ckumar               REWIND LUNBGD
                   krec=65
C              ... TO REPOSITION PAST THE HEADER RECORD ...
ckumar               READ(LUNBGD,ERR=910,END=920)(IHDR_BG(I),I=1,64)
C              ... DO NOT SKIP ANY REAL SCANLINE ON THIS RECYCLED
               JOFSORC = 0
             ELSE
C              ... HERE IS A CASE OF A NON-CYCLIC BACKGROUND AND
C              ... WE HAVE EXHAUSTED THE DATA IN THE MAP-BGND, 
C              ... EVEN THOUGH THE DESIRED GEOGRAPHY DEST SPACE 
C              ... HAS NOT YET BEEN FILLED.  
C              ... SET A WARNING AND JUMP OUT
C      ... HOW TO SET A WARNING???               IRET_BG = 2
               GO TO 999

             ENDIF
           ENDIF
C          -------------------------------------------------------------
C     ...   READ A SCANLINE WHICH IS TO BE USED, INTO A LONG LINE BUFFER
C     ...     WHICH IS BIG ENOUGH TO HOLD THE LONGEST SCANLINE OF ANY
           JOFSORC = JOFSORC + 1
ckumar           READ(LUNBGD,ERR=930,END=800)
ckumar     1                               (INPLINBUF(I),I=1,LMT_NWRDPLIN_LIB)
ckumar
        do I=1,LMT_NWRDPLIN_LIB
        READ(LUNBGD,ERR=910,rec=krec) INPLINBUF(I)
        krec=krec+1
        end do
        call byteswap(INPLINBUF, 8, LMT_NWRDPLIN_LIB)
ckumar
           NLINRED = NLINRED + 1
C
C          ... NOW I HAVE ONE ORIGINAL BG SCANLINE
C          ...      IN (INPLINBUF(1),(LMT_NWRDPLIN_LIB) ...
C          ...      so,  CUT / SHIFT TO FIT WITHIN FRAME
C          ...      and  TRANSFER TO IMAGE PLANE ...
           call bgln_cut(inplinbuf,image,jofdest,iwindow,mapcon_pur,
     1                   LCKPRNTQ,iret_cut)
C         
C          . . . . . . . . . . . . . . . . . . . . . . .
           NLINUSED = NLINUSED + 1
  477  CONTINUE
C      ...     WHICH IS ENDDO
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C

  800  CONTINUE

       WRITE(6,805) NLINRED,NLINSKPED,NLINUSED
  805  FORMAT(1H ,'getbgnd: READ BACKGROUND.  SCANLINES COUNTED=',I6,
     1       /1H ,'         OF WHICH SKIPPED=',I6,'; USED=',I6)
       
C      ... for checkout only.   ... the bgnd is not visible on image
C      ... is there anything there?
C      ... INITIALIZE THE FRAMED SPACE ...
       iorsum = 0
       DO  I = 1,NTOTWRDS
         iorsum = ior(iorsum,image(i))
       ENDDO
       if(iorsum .EQ. 0) then
         write(6,815)NTOTWRDS
  815    format(1h ,' * * * * * * *   W A R N I N G   * * * * * * * *',
     1         /1H ,'getbgnd: At finish, there is no geography in ',
     2              'the image plane.',
     3         /1H ,'         Word count of IMAGE space =',I8,
     3         /1H ,' * * * * * * * * * * * * * * * * * * * * * * * *')
       endif


       GO TO 999
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  900  CONTINUE
       WRITE(6,905)LUNBGD,IOSOPEN
  905  FORMAT(1H ,'getbgnd: FAILED on OPENing map background ',
     1            'on UNIT=',I3,
     2       /1H ,'     ERROR-CODE IOSTAT=',I8)
       IRET_BG = 6
       go to 999

  910  CONTINUE
       WRITE(6,915)LUNBGD
  915  FORMAT(1H ,'getbgnd: FAILED on read ERR on map background ',
     1            'HEADER record on UNIT=',I3)
       IRET_BG = 7
       go to 999

  920  CONTINUE
       WRITE(6,925)LUNBGD
  925  FORMAT(1H ,'getbgnd: FAILED on read EOF on map background ',
     1            'HEADER record on UNIT=',I3)
       IRET_BG = 8
       go to 999

  930  CONTINUE
       WRITE(6,935)LUNBGD
  935  FORMAT(1H ,'getbgnd: FAILED on read ERR on map background ',
     1            'data record on UNIT=',I3)
       IRET_BG = 9
       go to 999


  999  CONTINUE
       CLOSE(LUNBGD)
       RETURN
       END

