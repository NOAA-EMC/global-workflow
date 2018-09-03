       subroutine  lcntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
     1           il,label,ick,jfid,isched,index,
     1            nflds,
     x      FLD1,DASH1,DW1,SHAD,
     x      FLD2,DASH2,DW2,SHAD2,
     x      FLD3,DASH3,DW3,SHAD3,
     x      fld4,dash4,dw4,shad4)      ! added by kumar
        dimension fld1(*),image(*),il(15),isched(8,50)
     1 ,index(*)
         dimension jfid(14)
           dimension label(*)
        dimension fld2(*),fld3(*),fld4(*),dw1(2),dw2(2),dw3(2),dw4(2),
     1  dash1(2),
     1  dash2(2),
     1  dash3(2),dash4(2),shad(20),shad2(20),shad3(20),shad4(20)
         integer dash1,dash2,dash3,dash4,shad,shad2,
     1    shad3 ,shad4,
     1    dw1,dw2,dw3,dw4
        print *,' arg1 (iret_cnt) = ',iret_cnt
        print *,' arg2 (image) is scratch space '
        print *,' arg3 (imagsize_wrds )   is ', imagsiz_wrds
        print 104,' arg 4 (IL OR MAP), is ',il
 104    format(a20,2a9,2(/,5i16),/,2i16,z20)
        print *,' arg5 (label) is ',label(1)
        print *,' arg6, ick,   ',ick
        print 103,' arg 7 jfid(1-14) ',jfid
 103    format(a20,5(/,5z20))  
        print 105,isched
        print 106,isched
 106    format(' arg8 ISCHED',8(1x,z16))
 105    format(' arg8 isched ',8(1x,z8))
        print *,'arg9 (index )' ,index(1)
     1 ,(index(kk),kk=2,6)
        print *,' arg 10 (nflds ) ',nflds
        do 77,k=1,20
        print 1077,' SHADES ',shad(k),shad2(k),shad3(k),k
 77     continue
 1077   format(a20,3i20,'  k ',i4)
        print 107,' args 11-14 (field dash dfw shad ) ',
     1  (fld1(k),k=1,10),dash1,dw1,shad
 107    format(a50,2(/,5f16.4),/,2z17,4x,2z17,4x,5(/,4z20),/)
        print 107,' args 15-18 (field2 dash,dfw,shad ) ',
     1  (fld2(k),k=1,10),dash2,dw2,shad
        print 107,' args 19-22  (field3,dash,dfw,shad ) ',
     1  (fld3(k),k=1,10),dash3,dw3,shad
         call   cntr(iret_cnt, IMAGE, IMAGSIZ_WRDS,
     1           il,label,ick,jfid,isched,index,
     1            nflds,
     x      FLD1,DASH1,DW1,SHAD,
     x      FLD2,DASH2,DW2,SHAD2,
     x      FLD3,DASH3,DW3,SHAD3,
     x      fld4,dash4,dw4,shad4)
         return
         end
