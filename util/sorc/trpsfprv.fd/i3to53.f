      subroutine i3to53(F3,F53)
c  code to interpolate fro 360x181 type 3
c  latlon grid to 117x51 type 53 mercator grid
c   simple example of Mark Iredell's ploates
c  routine 
      parameter(ji=360*181)
      parameter(ig53=53,jo53=117*51)
      dimension F3(ji),F53(jo53)
       dimension ipopt(2)
C
      real rlat_03(ji),rlon_03(ji)
      logical lo_03(ji)
C
      real rlat_53(jo53),rlon_53(jo53)
      equivalence( rlat_53(1), rlat_03(1) )
      equivalence( rlon_53(1), rlon_03(1) )
      logical lo_53(jo53)
      equivalence( lo_53(1), lo_03(1) )
C
      integer ibi,ibo
      integer         kgdsi(22)
C
C
      INTEGER      KGDSO(22)
      CHARACTER    GDSO(42),gdsi(42)
      INTEGER      LENGDS
      ibi=0
cc           define 360x181  grid
            call makgds(3,kgdsi,gdsi,lengds,iret)
cc           define 117x51 grid
            call makgds(ig53,kgdso,gdso,lengds,iret)
            if(iret.ne.0) stop 'makgd' 
C
c            ipopt=0
             ipopt(1)=1
              ipopt(2)=-1
            ip = 0
c BICUBIC
             ip=4
            
            call ipolates(ip,ipopt,kgdsi,kgdso,ji,jo53,1,
     1                 ibi,lo_06,F3,ko,rlat_53,rlon_53,
     2                 ibo,lo_53,F53,iret)
            print *, iret
            if(iret.ne.0) stop 'ipol' 
           print * ,'IPOLATES RETURNING'
         write(96) f53
         return
       end
