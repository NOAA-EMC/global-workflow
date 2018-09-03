       subroutine fonts2 
       dimension kp(2)
       character*1 cv
       character*2 c2
      do 45,k=1,99
       cv=char(k)
       do 42,ic=1,4
       ric=ic
       kp(1)=1
       kp(2)=3
       write(c2,102)ic
       call putlab(300+30*ic,010,15.,c2,90.0,2,kp,0)
       call putlab(300+30*ic,3100,15.,c2,90.0,2,kp,0)
       ric=27.
       kp(1)=ic-1
        call putlab(300+30*ic,040+30*k,ric,cv,0.0,1,kp,0)
 42    continue
        write(c2,102)k
 102   format(i2)
       call putlab(250,040+30*k,15.,c2,90.0,2,kp,0)
       call putlab(1700,040+30*k,15.,c2,90.0,2,kp,0)
 45    continue
       return
       end
