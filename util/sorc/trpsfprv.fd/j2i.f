      subroutine j2i(m,n)
      COMMON/NSCHED/ISLOTA,IPANA,ISUB,IFLAB,INSET,IRLAB,ISCHED(8,50)
      common/nsched2/jsched
      character*1 jsched(16,50)
       character*8 c8(8,50) 
       equivalence(c8,isched)
       madd=m/2
       mnew=madd*2
       if(mnew .eq. m) mnew2=madd 
       if(mnew .ne. m) mnew2=madd+1 
       if(mnew .eq. m)  ibb=8 
       if(mnew .ne. m)  ibb=7 
       if (mnew .eq. m) c8(mnew2,n)(1:1)=jsched(m,n)
       if (mnew .ne. m) c8(mnew2 ,n)(2:2)=jsched(m,n)
       print *,' from j2i assigning jsched( ',m,n,
     1') to isched( ', mnew2,n, '), byte ',ibb,' with value',
     1 mova2i(jsched(m,n))
        return 
       end

