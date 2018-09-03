       subroutine isch (jsched)
       dimension isched(8,50)
       dimension jsched(8,50),csched(8,50)
       character*8 csched
       equivalence (csched,isched )
       dimension ip(10)
       do 5,k=1,50
       do 5 l=1,8
 5     isched(l,k)=jsched(l,k)
       do 10,k=1,15
       ip(1)=isched(1,k)
       ip(2)=isched(2,k)
       ip(3)=isched(3,k)
       ip(4)=isched(4,k)
       ip(5)=mova2i(csched(5,k)(7:7))
       ip(6)=mova2i(csched(5,k)(8:8))
       ip(7)=mova2i(csched(6,k)(7:7))
       ip(8)=mova2i(csched(6,k)(8:8))
       ip(9)=isched(7,k)
        ip(10)=isched(8,k)
       print 102,k,(ip(l),l=1,10)
 102   format(i6,' ischedd ',10i6)
   10   continue
        return
        end
  
       
