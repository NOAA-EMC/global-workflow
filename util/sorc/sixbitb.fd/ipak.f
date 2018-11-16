       subroutine ipak(cpak,isub) 
C      BUILD IPAK STRING FOR OSO.   IPAK(1:4) is handled
C      BY CALLER BUT FAX CUT NUMBER, CYCLE, DATE, DAY OF WEEK
C      AND PRODUCT NAME MUST BE INSERTED HERE.  THESE ARE
C      calculated from knowledge of the subset number
c      (passed in ISUB) and the date (stored in environment
c      variable PDY) and the Forecast cycle (stored in environment
c      variable CYCLE.  These latter two
c      are input with the GETENV fortran function.
c
c      arguments
c      CPAK (56 character ascii array to store IPAK string)
c      ISUB  subset number used for fax cuts and here
c      used to infer the product name also , only CPAK is modified
c
       common/packra/iras(10)
       dimension cm(12)
       character*3 cm
       data cm/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1 'SEP','OCT','NOV','DEC'/
       character*56 cpak
       character*8 cdate
       character*4 cycle
       character*4 cwk 
       character*24 cprod
ckumar       integer getenv
       integer istat
       cpak='                                                      '
c
ckumar       ifound=getenv ('PDY',cdate,istat)
ckumar       ifound=getenv('CYCLE',cycle,istat)
ckumar IBM replacement routine for GETENV
ckumar
       call getenv('PDY',cdate)
       call getenv('CYCLE',cycle)
c
       print*,'In ipak '
       print*,'cdate in ipak  ',cdate
       print*,'PDY in ipak  ',PDY
       print*,'CYCLE in ipak  ',CYCLE
c
       read(cdate,102)iy,im,id
       read(cycle(2:3),109)ih
       iras(7)=ih
       iras(8)=id
       iras(9)=im
       iras(10)=iy
       iadd=0
 102   format(i4,2i2)
       if(isub .eq. 50 .or. isub .eq. 51) iadd=12
       iras(1)=iadd
       call updatr
       id=iras(4)
       im=iras(5)
       iy=iras(6)
       ih=iras(3)
c
       call dayowk(iras(4),iras(5),iras(6),idwk,cwk)
       write(cpak(4:9),103)isub
       if(isub .eq. 50) cprod='FAXPLOT          '
       if(isub .eq. 51) cprod='FAXPLOT          '
       if(isub .eq. 2927) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2977) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2929) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2979) cprod='   WINDS ALOFT  02+40 '
cB   50 TIU 00Z 13 FEB FAXPLOT
cB   50 TUE  12Z 18 FEB 97  FDFAX UPPER WHNDS
  103  format('B',i5)
       write(cycle(2:3),109)ih
 109   format(i2.2)
       write(cpak(10:56),105)cwk,cycle(2:4),' ',
     1 id,' ',cm(im),' ',cprod
 105   format(1x,a4,a3,a1,i2,a1,a3,a1,a24)
       return 
       end
