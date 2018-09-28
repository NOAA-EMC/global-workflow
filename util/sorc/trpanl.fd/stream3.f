      SUBROUTINE stream3(mbars,itime,lupgb,lupgi,F3)
c      subroutine to accept pressure level (mbars),
c     grib unit number (lupgb), and grib index unit
c    number (lupgi) and read u, and v wind fields,
c   at mbars, and calculate stream function.  Stream
c    function is stored in F3 and returned to caller.
c    The Grib fields on lupgb MUST be grib type 3 (360x181)
c    fields.  These are output automatically by the NCEP
c   GFS forecast model
      parameter(im=360,jm=181,km=16)
      dimension F3(im,jm)
      dimension iprs(km)
      integer kpds(100), kgds(100)
      integer jpds(100), jgds(100)
      logical lbms(im*jm), luv, ldz, lps
      real u(im,jm), v(im,jm), psi(im,jm),chi(im,jm)
      data iromb/0/,maxwv/126/,idrti/0/,imaxi/360/,jmaxi/181/
      data idrto/0/,imaxo/360/,jmaxo/181/,kmax/1/
      data iprime/0/,iskipi/0/,jskipi/0/,kspipi/0/
      data iskipo/0/,jskipo/0/,kspipo/0/,jcpu/0/
      luv = .FALSE.
      ldz = .FALSE.
      lps = .TRUE.
      ijm = im * jm
      fim = im
      lskip=-1
      k=0
      do jj = 1, 100
        jpds(jj) = -1
      enddo
      do jj = 1, 20
        jgds(jj) = -1
      enddo
        jpds(5) = 33
        jpds(6) = 100
        jpds(7) = mbars 
        jpds(14)=itime
        call getgb(lupgb,lupgi,ijm,-1,jpds,jgds,ndata,
     &  lskip,kpds,kgds,lbms,u,iret)
        if(iret.ne.0) then
          print *, ' iret =', iret
          print *, ' k =', k
        endif
        jpds(5) = 34
        jpds(6) = 100
        jpds(7) = mbars 
        call getgb(lupgb,lupgi,ijm,-1,jpds,jgds,ndata,
     &  lskip,kpds,kgds,lbms,v,iret)
        if(iret.ne.0) then
          print *, ' iret =', iret
          print *, ' k =', k
        endif
        call     SPTRUNV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,
     &                   IDRTO,IMAXO,JMAXO,KMAX,
     &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,
     &                   ISKIPO,JSKIPO,KSKIPO,JCPU,U,V,
     &                   LUV,GRIDUO,GRIDVO,LDZ,GRIDDO,GRIDZO,
     &                   LPS,chi,psi)
        print 109,k
 109   format(' K is ',i9)
        do j=1,jm
        do k=1,im
           f3(k,j)=psi(k,j)        
        end do
        end do
      return
      end
