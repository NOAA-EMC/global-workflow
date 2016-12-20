      BLOCK DATA
C                                                                               
      COMMON   /BLK1/
C               REAL*4                                                          
     1               ULCLAT,  ULCLON,
C               INTEGER*4                                                       
     2               ISEQ,    KTRAN,   IDDFF,   ITEMP,   ITROP,
     3               MAXHGT,  KEYS,    NCAT,    KHEM,    KCNT,
     4               KEYCNT,  LABEL,   LVL,     ITIME,   LVLNNN,
     6               MTAU,
C               CHARACTER*4                                                     
     7               TROP1,   KWBC1,   MAXW1,   KWMO,
C               INTEGER*2                                                       
     8               JTAU,    MAXTRP,  LONINC,  LATINC,  NLVLS,
     9               NLON,    NLAT,    KTAU,
C               CHARACTER*1                                                     
     2               ACOMM,
     3               DATEND,  KTTY,    SPACE,   MOUT,
     4               A,       B,       LF,      CR,
     5               HEADR1,
     6               ALFAA,   ALFAE,   ALFAF,   ALFAM,
     7               ALFAN,   ALFAP,   ALFAS,   ALFAW,
     8               ALFAZ,   KWMO1
C                                                                               
      save
      REAL         ULCLAT(200)
      REAL         ULCLON(200)
C                                                                               
      INTEGER      ISEQ(200),   KTRAN,   KEYS
      data         ktran/51/, keys/05/
      INTEGER      NCAT(200),     KHEM
      INTEGER      KCNT(200),     KEYCNT,        LABEL(12)
      INTEGER      IDDFF(145,37,10)
      INTEGER      ITEMP(145,37,9)
      INTEGER      ITROP(145,37), MAXHGT(145,37)
      INTEGER      LVL(9,100),      ITIME(8)
      INTEGER      LVLNNN(9)
      data         lvlnnn/50,100,180,240,300,340,390,450,500/
C                                                                               
      INTEGER     JTAU(5),      MAXTRP(200),   LONINC(200)
      INTEGER     LATINC(200),  NLVLS(200),    NLON(200)
      INTEGER     NLAT(200),    KTAU(200)
      CHARACTER*40  ACOMM
      data          acomm/'H100000                                '/
      CHARACTER*4  KWMO(5)
      data         kwmo/'XXXX','JJ K','WBC ','DDGG','GG  '/
C                                                                               
C                                                                               
      CHARACTER*1   DATEND
      data          DATEND/'9'/

      CHARACTER*4   KWBC1,MAXW1
      data          KWBC1/'KWBC'/,MAXW1/'MAXW'/
      CHARACTER*4   TROP1
      data          TROP1/'TROP'/
C                                                                               
      CHARACTER*1   A(200)
      CHARACTER*1   B(200),       KTTY(69,160),  SPACE
      data          SPACE/' '/
      CHARACTER*1   MOUT(1280)
      CHARACTER*1   KWMO1(18),LF,CR
      data          LF/'@'/,CR/'<'/
      CHARACTER*1   HEADR1(4,100)
      CHARACTER*1   ALFAA
      data          ALFAA/'A'/
      CHARACTER*1   ALFAN,ALFAE,ALFAS,ALFAW
      data          ALFAN/'N'/,ALFAE/'E'/,ALFAS/'S'/,ALFAW/'W'/
      CHARACTER*1   ALFAF
      data          ALFAF/'F'/
      CHARACTER*1   ALFAZ,ALFAM, ALFAP
      data          ALFAZ/'Z'/,ALFAM/'M'/, ALFAP/'P'/
                   END
