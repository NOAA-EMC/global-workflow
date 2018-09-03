      SUBROUTINE lPUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KPRIORS,ITAG)
      dimension kprior(2)
      character*(*) ctext
c     parse kprior
      kprior(1)=0
      kprior(2)=0
      if (kpriors .eq. 0) then
         kprior(1)=0
         kprior(2)=0
      else 
         kprior(2)=kpriors
         kprior(1)=0
      endif
 904   format('LPUTLAB TEXT',A132,i4)
 901  format
     1 ('LPUTLAB ARGS',i6,i6,f6.2, '>',a16,'<',f6.2,i3,i6,i6,i8)
      print 901,ipt,jpt,height,ctext,angle,nchar,kprior,itaG
      print 904,ctext,nchar
      call PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KPRIOR,ITAG)
      return
      end

