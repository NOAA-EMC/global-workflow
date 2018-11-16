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
       integer istat
       cpak='                                                       ' 
c
       call getenv("PDY",cdate)
       call getenv("cycle",cycle)
c
       print*,'In ipak '
       print*,'cdate in ipak  ',cdate
       print*,'cycle in ipak  ',cycle
c
       read(cdate,101)iy,im,id
       read(cycle(2:3),102)ih
       iras(7)=ih
       iras(8)=id
       iras(9)=im
       iras(10)=iy
       iadd=0
 101   format(i4,2i2)
 102   format(i2)
       print *,' date ',cdate
       if(isub .eq. 50 .or. isub .eq. 51) iadd=12
       iras(1)=iadd
       call updatr
       id=iras(4)
       im=iras(5)
       iy=iras(6)
       ih=iras(3)
       print*,'id,im,iy,ih before dayowk   ',id,im,iy,ih
ckumar       if(iras(6) .gt. 50) iy=1900+iras(6)
ckumar       if(iras(6) .le. 50) iy=2000+iras(6)
c
ckumar       call dayowk(id,im,iy,idwk,cwk)
       call dayowk(iras(4),iras(5),iras(6),idwk,cwk)
ckumar       call dayowk(iras(4),iras(5),iy,idwk,cwk)
c      special code for trpsfcmv
       if(isub .eq. 3820) isub=2700
       if(isub .eq. 3675) isub=2701
       write(cpak(4:9),103)isub
       if(isub .eq. 50) cprod='FAXPLOT          '
       if(isub .eq. 51) cprod='FAXPLOT          '
       if(isub .eq. 2927) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2977) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2929) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2979) cprod='   WINDS ALOFT  02+40 '
       if(isub .eq. 2700) cprod=' TROPICAL SFC ANAL      '
       if(isub .eq. 2701) cprod=' TROPICAL SFC ANAL      '
       if(isub .eq. 3658) cprod=' TROPICAL SFC ANAL      '
       if(isub .eq. 3659) cprod=' TROPICAL SFC ANAL      '
       if(isub .eq. 3670) cprod=' TROPICAL SFC ANAL      '
       if(isub .eq. 3671) cprod=' TROPICAL SFC ANAL      '
cB   50 TIU 00Z 13 FEB FAXPLOT
cB   50 TUE  12Z 18 FEB 97  FDFAX UPPER WHNDS
  103  format('B',i4)
       write(cycle(2:3),109)ih
 109   format(i2.2)
       write(cpak(10:56),105)cwk,cycle(2:4),' ',
     1 id,' ',cm(im),' ',iy,' ',cprod
c     1 id,' ',cm(im),' ',cprod
c     1 id,' ',cm(im),' ',iy,' ',cprod
 105   format(1x,a4,a3,a1,i2,a1,a3,a1,i4,a1,a24)
c 105   format(1x,a4,a3,a1,i2,a1,a3,a1,i4,x,a24)
       print * ,'cpak ',cpak
       return 
       end
