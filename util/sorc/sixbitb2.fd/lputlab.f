      SUBROUTINE lPUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KPRIORS,ITAG)
      common/counts/icount
ckumar      data icount/0/
      dimension kprior(2)
      character*(*) ctext
c     parse kprior
ckumar 1 line below added by kumar ! for ibmsp1
      icount = 0
      kprior(1)=0
      kprior(2)=0
      if (kpriors .eq. 0) then
         kprior(1)=0
         kprior(2)=0
      else 
         kprior(2)=kpriors
         kprior(1)=0
      endif
      icount=icount+1
      print 901,ipt,jpt,height,ctext,angle,nchar,kprior,itaG
      print 904,ctext,nchaR
 904   format('LPUTLAB TEXT',A132,i4)
 901  format
     1 ('LPUTLAB ARGS',i6,i6,f6.2, '>',a16,'<',f6.2,i3,i6,i6,i8)
      if(icount  .le. 1) then
      print 902  
 902  format('LPUTLAB',/,'LPUTLAB HDRS  IDOT  JDOT',2x,
     1'  HGT', 7x,'CHAR',7x,'  ANG',
     1 '  NCH KP(1) KP(2)  ITG ',/,'LPUTLAB')
      endif
      if (icount .ge. 20)icount=0
C      call PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,8,KPRIOR,ITAG)
      call PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KPRIOR,ITAG)
      return
      end

