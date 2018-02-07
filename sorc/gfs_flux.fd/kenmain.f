      subroutine kenmain(n,kdim,idim,jdim,rlat,rlon,istat,
     &                   nstart,nend,nint,nout,nzero,f00,imask)
      integer ib(n), jb(n), ibp(n), jbp(n), n, idim, jdim
      real colat(jdim), di(n), dj(n)
      real cosclt(jdim), wsclt(jdim), elevstn(n), elevgrd(n)
      real rlat(n), rlon(n)
      integer istat(n), imask(n)
      real missing
      integer kp1, kp2, kp
      integer avgflg(100),accflg(100)
      character*80 fngrib
      real sfc2(kdim,n,2),sfc(kdim,n)
      logical f00
      data avgflg/12*0,0,1,0,0,1,1,1,1,1,0,0,0,
     &            1,1,1,1,1,71*0/
      data accflg/10*0,1,1,88*0/
      missing = 9.9e20
      call splat(4,jdim,cosclt,wsclt)
      radi = 180. / (4. * atan(1.))
      hour = 3600.
      do j = 1, jdim
        colat(j) = acos(cosclt(j)) * radi
      enddo
c     open(unit=1,file='/user/g01/wx23ph/kenpt/kenll2.dat',
c    & form='FORMATTED',status='OLD')
c     do k = 1, n
c       read(1,5100) elevgrd(k)
c     enddo
c     close(unit=1)
c     open(unit=1,file='/user/g01/wx23ph/kenpt/kenll3.dat',
c    &    form='FORMATTED',status='OLD')
c     do k = 1, n
c       read(1,5200) blat(k),blon(k),elevstn(k)
c     enddo
c     close(unit=1)
 5100 format(22x,f10.4)
 5200 format(2x,3f10.4)
 6100   format(2x,9a8)
      call kengrd(ib,ibp,jb,jbp,di,dj,rlat,rlon,n,
     &     idim,jdim,colat)
      kp1 = 1
      kp2 = 2
      fngrib='flxf00'
      sfc2 = 0.
      ns = nstart + nint
      nmax = max(nzero,nout)
      if(f00) ns = nstart
      do nf = ns, nend, nint
        dt = (mod(nf-1,nzero)+1) * hour 
        if(dt.eq.0.) dt = 1.
c        print *, ' nf, modnf, dt =', nf, mod(nf-1,nzero)+1, dt
        if(nf.lt.10) then
          fngrib='flxf0'
          write(fngrib(6:6),'(i1)') nf
        elseif(nf.lt.100) then
          fngrib='flxf'
          write(fngrib(5:6),'(i2)') nf
        else
          fngrib='flxf'
          write(fngrib(5:7),'(i3)') nf
        endif
        call extflx(11,fngrib,sfc2(1,1,kp1),kdim,n,ib,ibp,jb,jbp,
     &   di,dj,idim,jdim,colat,imask)
        k = 3
c        print *, ' sfc2 at point 3 =', (sfc2(j,k,kp1),j=11,12)
c
c  restore averaged fields to accumulated fields
c
        do k = 1, n
          do j = 1, kdim
            if((avgflg(j).eq.1.or.accflg(j).eq.1)
     &         .and.sfc2(j,k,kp1).ne.missing) then
              sfc2(j,k,kp1) = sfc2(j,k,kp1) * dt
            endif
          enddo
        enddo
        k = 3
c        print *, ' sfc2 at point 3 after *dt =',
c     &   (sfc2(j,k,kp1),j=11,12)
c        print *, ' sfc2kp2 at point 3 after *dt =',
c     &   (sfc2(j,k,kp2),j=11,12)
        if(mod(nf-1,nmax)+1.ne.nint.and.nf.gt.0) then
c          print *, ' doing avg and acc work, kp1, kp2 =', kp1, kp2
          do k = 1, n
            do j = 1, kdim
              if(avgflg(j).eq.1.or.accflg(j).eq.1) then
                if(nzero.gt.nint) then
                  sfc2(j,k,kp2) = sfc2(j,k,kp1) - sfc2(j,k,kp2)
                else
                  sfc2(j,k,kp2) = sfc2(j,k,kp1) + sfc2(j,k,kp2)
                endif
              else
                sfc2(j,k,kp2) = sfc2(j,k,kp1)
              endif
            enddo
          enddo
          kp = kp1
          kp1 = kp2
          kp2 = kp
        endif
        k = 3
c        print *, ' sfc2 at point 3 after subtraction =',
c     &   (sfc2(j,k,kp1),j=11,12)
        if(mod(nf,nout).eq.0) then
          dtout = (mod((nf-1),nout)+1) * hour
          if(dtout.eq.0.) dtout = 1.
c          print *, ' dtout =', dtout
          do k = 1, n
            do j = 1, kdim
              sfc(j,k) = sfc2(j,k,kp1)
              if(avgflg(j).eq.1) then
                sfc(j,k) = sfc(j,k) / dtout
              endif
              if(accflg(j).eq.1.and.sfc(j,k).lt.0.) sfc(j,k) = 0.
            enddo
          enddo
        k = 3
c        print *, ' sfc at point 3 =', (sfc(j,k),j=11,12)
c
c  surface and 2-meter temperature corrections
c
          do k = 1, n
c         dtemp = .0060 * (elevgrd(k) - elevstn(k))
c
c  do height correction if there is no snow or if the temp is less than 0
c
c         if(sfc(10,k).eq.0.) then
c           sfc(30,k) = sfc(30,k) + dtemp
c           sfc(5,k) = sfc(5,k) + dtemp
c         endif
c         if(sfc(10,k).gt.0..and.sfc(5,k).lt.273.16) then
c           sfc(5,k) = sfc(5,k) + dtemp
c           if(sfc(5,k).gt.273.16) then
c             dtemp = sfc(5,k) - 273.16
c             sfc(5,k) = 273.16
c           endif
c           sfc(30,k) = sfc(30,k) + dtemp
c         endif
            dir = 0.
            if(sfc(34,k).ne.0..or.sfc(35,k).ne.0.) then
              dir = 270. -
     &            atan2(sfc(35,k),sfc(34,k)) * 180. / 3.14159
            endif
            if(dir.ge.360.) dir = dir - 360.
            sfc(36,k) = dir
          enddo
          do j = 1, kdim
            write(17) (sfc(j,k),k=1,n)
          enddo
          write(18) sfc
        endif
        if(mod(nf-1,nout)+1.eq.nint) then
          kp = kp1
          kp1 = kp2
          kp2 = kp
        endif
      enddo
      end
