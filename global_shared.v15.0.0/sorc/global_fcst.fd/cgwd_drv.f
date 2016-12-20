      subroutine cgwd_drv(im,ix,iy,levs,lat,dlength, rcs
     &,                   uin, vin, tin, cuhr,prsl, prsi, del
     &,                   ktop, kbot, kuo, gwdcu, gwdcv
     &                    dusfcg,dvsfcg,lprnt,ipr, fhour)

!
!        convective gravity wave drag driver
!
      use machine , only : kind_phys

      use physcons, rocp  => con_rocp,  cp => con_cp, fv => con_fvirt
     &,             grav  => con_g,     rd => con_rd
     &,             rv    => con_rv,    hvap => con_hvap
     &,             hfus  => con_hfus
     &,             rerth => con_rerth, pi => con_pi
      implicit none

!
      logical lprnt
      integer im, ix, iy, levs, lat, ipr
      real    dtf, fhour

      integer               kbot(im), ktop(im), kuo(im)
      real(kind=kind_phys)  uin(ix,levs),   vin(ix,levs), tin(ix,levs)
      real(kind=kind_phys)  prsl(ix,levs),  prsi(ix,levs+1),
     &                      del(ix,levs),   cuhr(im,levs)
     &                      gwdcu(im,levs), gwdcv(im,levs),
     &,                     dlength(im),    rcs(im)
!
!
       real(kind=kind_phys) cumchr(im,levs),cumabs(im)
       real(kind=kind_phys) qmax(im)
     &                      diagn1(im,levs),     diagn2(im,levs)

      integer i, k1
!-----------------------------------------------------------------------
!        calculate maximum convective heating rate            qmax [k/s]
!        cuhr = temperature change due to deep convection
!-----------------------------------------------------------------------

      do i=1,im
        qmax(i)   = 0.
        cumabs(i) = 0.
      enddo
      do k=1,levs
        do i=1,im
          cumchr(i,k) = 0.
          gwdcu(i,k)  = 0.
          gwdcv(i,k)  = 0.
!         diagn1(i,k)=0.
!         diagn2(i,k)=0.
        enddo
      enddo
      do k=1,levs
        do i=1,im
          qmax(i)   = max(qmax(i),cuhr(i,k))
          cumabs(i) = cuhr(i,k) + cumabs(i)
        enddo
      enddo

      do i=1,im
        do k=kbot(i),ktop(i)
          do k1=kbot(i),k
            cumchr(i,k) = cuhr(i,k1) + cumchr(i,k)
          enddo
          cumchr(i,k) = cumchr(i,k) / cumabs(i)
        enddo
      enddo
      if (lprnt) then
      if (kbot(ipr).le.ktop(ipr)) then
       lonipr=xlon(ipr)*57.29578
       latipr=xlat(ipr)*57.29578
       write(*,*) 'kbot <= ktop     for (lat,lon) = ',latipr,lonipr
       write(*,*) 'kuo kbot ktop qmax dlength  ',
     + kuo(ipr),kbot(ipr),ktop(ipr),(86400.*qmax(ipr)),dlength(ipr)
       write(*,9000) kdt
       do k=ktop(ipr),kbot(ipr),-1
         write(*,9010) k,(86400.*chr(ipr,k)),(100.*cumchr(ipr,k))
       enddo
      endif
      endif

 9000 format(/,3x,'k',5x,'chr(k)',4x,'cumchr(k)',5x,'at kdt = ',i4,/)
 9010 format(2x,i2,2x,f8.2,5x,f6.0)

c-----------------------------------------------------------------------
c        call gwdc routine
c-----------------------------------------------------------------------
      fhourpr = 0.

      if (lprnt) then
        print *,' before gwdc in gbphys fhour ',fhour
        if (fhour.ge.fhourpr) then
          print *,' before gwdc in gbphys start print'
          write(*,*) 'fhour ix im levs = ',fhour,ix,im,levs
          print *,'dtp  dtf  rcs = ',dtp,dtf,rcs(ipr)

          write(*,9100)
          ilev=levs+1
          write(*,9110) ilev,(10.*prsi(ipr,ilev))
          do ilev=levs,1,-1
            write(*,9120) ilev,(10.*prsl(ipr,ilev)),(10.*del(ipr,ilev))
            write(*,9110) ilev,(10.*prsi(ipr,ilev))
          enddo

 9100 format(//,14x,'pressure levels',//,
     +' ilev',7x,'prsi',8x,'prsl',8x,'delp',/)
 9110 format(i4,2x,f10.3)
 9120 format(i4,12x,2(2x,f10.3))

          write(*,9130)
          do ilev=levs,1,-1
            write(*,9140) ilev,ugrs(ipr,ilev),gu0(ipr,ilev),
     +      vgrs(ipr,ilev),gv0(ipr,ilev),
     +      tgrs(ipr,ilev),gt0(ipr,ilev),gt0b(ipr,ilev),
     +      dudt(ipr,ilev),dvdt(ipr,ilev)
          enddo

 9130 format(//,10x,'before gwdc in gbphys',//,' ilev',6x,
     +'ugrs',9x,'gu0',8x,'vgrs',9x,'gv0',8x,
     +'tgrs',9x,'gt0',8x,'gt0b',8x,'dudt',8x,'dvdt',/)
 9140 format(i4,9(2x,f10.3))

          print *,' before gwdc in gbphys end print'

        endif
      endif
!
      call gwdc(im, ix, im, levs, lat, ugrs, vgrs, tgrs,
     &          rcs, prsl, prsi, del, qmax, cumchr, ktop, kbot, kuo,
     &          gwdcu, gwdcv, grav, cp, rd, dlength, lprnt, ipr, fhour,
     &          dusfcg,dvsfcg,diagn1,diagn2)
!
      if (lprnt) then
        if (fhour.ge.fhourpr) then
          print *,' after gwdc in gbphys start print'

          write(*,9131)
          do ilev=levs,1,-1
            write(*,9141) ilev,ugrs(ipr,ilev),gu0(ipr,ilev),
     +      vgrs(ipr,ilev),gv0(ipr,ilev),
     +      tgrs(ipr,ilev),gt0(ipr,ilev),gt0b(ipr,ilev),
     +      gwdcu(ipr,ilev),gwdcv(ipr,ilev)
          enddo

 9131 format(//,10x,'after gwdc in gbphys',//,' ilev',6x,
     +'ugrs',9x,'gu0',8x,'vgrs',9x,'gv0',8x,
     +'tgrs',9x,'gt0',8x,'gt0b',7x,'gwdcu',7x,'gwdcv',/)
 9141 format(i4,9(2x,f10.3))

          print *,' after gwdc in gbphys end print'
        endif
      endif
!
      return
      end

