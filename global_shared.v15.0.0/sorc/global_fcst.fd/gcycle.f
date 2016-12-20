      subroutine gcycle(me,lats_node_r,lonsperlar,global_lats_r,
     &                  ipt_lats_node_r,idate,fhour,fhcyc,
     &                  xlon ,xlat,sfc_fld,ialb)
!
      use machine
      use physcons, pi => con_pi
      use resol_def
      use sfc_flx_esmfmod
      use namelist_def, only : use_ufo
      implicit none
!
      type(sfc_var_data)        :: sfc_fld
!
      integer lonsperlar(latr),lons_lat,ialb
      integer global_lats_r(latr),ipt_lats_node_r
      integer me,idate(4),lats_node_r,lat
      real fhour, fhcyc
      real xlon  (lonr,lats_node_r),xlat  (lonr,lats_node_r)

!
!     local variables
!     ---------------
      integer ilat,len,il,ilon,i,l
!
      real  rla(lonr*lats_node_r),           rlo(lonr*lats_node_r),
     &      slmask(lonr*lats_node_r),        orog(lonr*lats_node_r),
     &                                       orog_uf(lonr*lats_node_r),
     &      tsffcs(lonr*lats_node_r),        snofcs(lonr*lats_node_r),
     &      zorfcs(lonr*lats_node_r),        albfcs(lonr*lats_node_r,4),
     &      tg3fcs(lonr*lats_node_r),        cnpfcs(lonr*lats_node_r),
     &      smcfcs(lonr*lats_node_r,lsoil),
     &      stcfcs(lonr*lats_node_r,lsoil),
     &      slifcs(lonr*lats_node_r),        aisfcs(lonr*lats_node_r),
     &      f10mfcs(lonr*lats_node_r),       vegfcs(lonr*lats_node_r),
     &      vetfcs(lonr*lats_node_r),        sotfcs(lonr*lats_node_r),
     &      alffcs(lonr*lats_node_r,2),      cvfcs(lonr*lats_node_r),
     &      cvbfcs(lonr*lats_node_r),        cvtfcs(lonr*lats_node_r),
!
     &      smcfc1(lonr*lats_node_r*lsoil),
     &      stcfc1(lonr*lats_node_r*lsoil),
     &      albfc1(lonr*lats_node_r*4),       alffc1(lonr*lats_node_r*2)
!clux add swdfcs, sihfcs, sicfcs
     +,     swdfcs(lonr*lats_node_r)
     +,     sihfcs(lonr*lats_node_r),sicfcs(lonr*lats_node_r)
     &,     sitfcs(lonr*lats_node_r)
!clux add vmnfcs, vmxfcs, slpfcs, absfcs, slcfc1, slcfcs
     +,     vmnfcs(lonr*lats_node_r),vmxfcs(lonr*lats_node_r)
     +,     slpfcs(lonr*lats_node_r),absfcs(lonr*lats_node_r)
     +,     slcfc1(lonr*lats_node_r*lsoil)
     +,     slcfcs(lonr*lats_node_r,lsoil)


      real  sig1t, pifac
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!     if (me .eq. 0) print *,' nlats=',nlats,' lonsinpe='
!    *,lonsinpe(0,1)
      sig1t = 0.0
!
      pifac = 180.0 / pi
      len = 0
      do ilat=1,lats_node_r  !-----begin latitude loop------------------
        lat = global_lats_r(ipt_lats_node_r-1+ilat)
        lons_lat = lonsperlar(lat)
        do ilon=1,lons_lat   !-----begin longitude loop-----------------

!     print *,' calling gcycle for ilat',ilat,' me=',me,' nlats='
!    *,nlats,' lonsinpe=',lonsinpe(:,ilat)
!     if (ilat .eq. nlats) stop
!
          len = len + 1
          rla(len)      = xlat(ilon,ilat) * pifac
          rlo(len)      = xlon(ilon,ilat) * pifac
          orog(len)     = sfc_fld%oro(ilon,ilat)
          orog_uf(len)  = sfc_fld%oro_uf(ilon,ilat)
          tsffcs(len)   = sfc_fld%tsea(ilon,ilat)
          snofcs(len)   = sfc_fld%sheleg(ilon,ilat)
          zorfcs(len)   = sfc_fld%zorl(ilon,ilat)
          albfcs(len,1) = sfc_fld%alvsf(ilon,ilat)
          albfcs(len,2) = sfc_fld%alvwf(ilon,ilat)
          albfcs(len,3) = sfc_fld%alnsf(ilon,ilat)
          albfcs(len,4) = sfc_fld%alnwf(ilon,ilat)
          tg3fcs(len)   = sfc_fld%tg3(ilon,ilat)
          cnpfcs(len)   = sfc_fld%canopy(ilon,ilat)
          smcfcs(len,:) = sfc_fld%smc(ilon,:,ilat)
          stcfcs(len,:) = sfc_fld%stc(ilon,:,ilat)
          slifcs(len)   = sfc_fld%slmsk(ilon,ilat)
          f10mfcs(len)  = sfc_fld%f10m(ilon,ilat)
          vegfcs(len)   = sfc_fld%vfrac(ilon,ilat)
          vetfcs(len)   = sfc_fld%vtype(ilon,ilat)
          sotfcs(len)   = sfc_fld%stype(ilon,ilat)
          alffcs(len,1) = sfc_fld%facsf(ilon,ilat)
          alffcs(len,2) = sfc_fld%facwf(ilon,ilat)
          cvfcs(len)    = sfc_fld%cv(ilon,ilat)
          cvbfcs(len)   = sfc_fld%cvb(ilon,ilat)
          cvtfcs(len)   = sfc_fld%cvt(ilon,ilat)
!clux add swdfcs, sihfcs, sicfcs
          swdfcs(len)   = sfc_fld%snwdph(ilon,ilat)
          sihfcs(len)   = sfc_fld%hice(ilon,ilat)
          sicfcs(len)   = sfc_fld%fice(ilon,ilat)
          sitfcs(len)   = sfc_fld%tisfc(ilon,ilat)
!clux add slcfcs, vmnfcs, vmxfcs, slpfcs, absfcs
          slcfcs(len,:) = sfc_fld%slc(ilon,:,ilat)
          vmnfcs(len)   = sfc_fld%shdmin(ilon,ilat)
          vmxfcs(len)   = sfc_fld%shdmax(ilon,ilat)
          slpfcs(len)   = sfc_fld%slope(ilon,ilat)
          absfcs(len)   = sfc_fld%snoalb(ilon,ilat)

!
          if (slifcs(len) .lt. 0.1 .or. slifcs(len) .gt. 1.5) then
             slmask(len) = 0
          else
             slmask(len) = 1
          endif

          if (slifcs(len) .eq. 2) then
            aisfcs(len) = 1.
          else
            aisfcs(len) = 0.
          endif

!     if (me .eq. 0)
!    &   print *,' len=',len,' rla=',rla(len),' rlo=',rlo(len)
        enddo                 !-----end longitude loop------------------------------

      enddo                   !-----end latitude loop-------------------------------
!
      do l=1,lsoil
        il = (l-1)*len
        do i=1,len
          smcfc1(il+i) = smcfcs(i,l)
          stcfc1(il+i) = stcfcs(i,l)
!clux add slcfc1
          slcfc1(il+i) = slcfcs(i,l)
        enddo
      enddo
      do l=1,4
        il = (l-1)*len
        do i=1,len
          albfc1(il+i) = albfcs(i,l)
        enddo
      enddo
      do l=1,2
        il = (l-1)*len
        do i=1,len
          alffc1(il+i) = alffcs(i,l)
        enddo
      enddo
!
      call sfccycle(101,len,lsoil,sig1t,fhcyc
     &,             idate(4), idate(2), idate(3), idate(1), fhour
     &,             rla, rlo, slmask, orog, orog_uf, use_ufo
!cwu [+1l] add sihfcs and sicfcs
     &,             sihfcs,   sicfcs, sitfcs
!clu [+2l] add swd, slc, vmn, vmx, slp, abs
     &,             swdfcs,   slcfc1
     &,             vmnfcs,   vmxfcs, slpfcs, absfcs
     &,             tsffcs,   snofcs, zorfcs, albfc1, tg3fcs
     &,             cnpfcs,   smcfc1, stcfc1, slifcs, aisfcs, f10mfcs
     &,             vegfcs,   vetfcs, sotfcs, alffc1
     &,             cvfcs,    cvbfcs, cvtfcs, me, nlunit, ialb)
!
      do l=1,lsoil
        il = (l-1)*len
        do i=1,len
          smcfcs(i,l) = smcfc1(il+i)
          stcfcs(i,l) = stcfc1(il+i)
!clux add slcfcs
          slcfcs(i,l) = slcfc1(il+i)
        enddo
      enddo
      do l=1,4
        il = (l-1)*len
        do i=1,len
          albfcs(i,l) = albfc1(il+i)
        enddo
      enddo
      do l=1,2
        il = (l-1)*len
        do i=1,len
          alffcs(i,l) = alffc1(il+i)
        enddo
      enddo
!
      il = 0
      do ilat=1,lats_node_r  !-----begin latitude loop------------------
        lat = global_lats_r(ipt_lats_node_r-1+ilat)
        lons_lat = lonsperlar(lat)
!
        do ilon=1,lons_lat   !-----begin longitude loop-----------------
          il = il + 1
          sfc_fld%tsea(ilon,ilat)   = tsffcs(il)
          sfc_fld%sheleg(ilon,ilat) = snofcs(il)
          sfc_fld%zorl(ilon,ilat)   = zorfcs(il)
          sfc_fld%alvsf(ilon,ilat)  = albfcs(il,1)
          sfc_fld%alvwf(ilon,ilat)  = albfcs(il,2)
          sfc_fld%alnsf(ilon,ilat)  = albfcs(il,3)
          sfc_fld%alnwf(ilon,ilat)  = albfcs(il,4)
          sfc_fld%tg3(ilon,ilat)    = tg3fcs(il)
          sfc_fld%canopy(ilon,ilat) = cnpfcs(il)
          sfc_fld%smc(ilon,:,ilat)  = smcfcs(il,:)
          sfc_fld%stc(ilon,:,ilat)  = stcfcs(il,:)
          sfc_fld%slmsk(ilon,ilat)  = slifcs(il)
          sfc_fld%f10m(ilon,ilat)   = f10mfcs(il)
          sfc_fld%vfrac(ilon,ilat)  = vegfcs(il)
          sfc_fld%vtype(ilon,ilat)  = vetfcs(il)
          sfc_fld%stype(ilon,ilat)  = sotfcs(il)
          sfc_fld%facsf(ilon,ilat)  = alffcs(il,1)
          sfc_fld%facwf(ilon,ilat)  = alffcs(il,2)
          sfc_fld%cv(ilon,ilat)     = cvfcs(il)
          sfc_fld%cvb(ilon,ilat)    = cvbfcs(il)
          sfc_fld%cvt(ilon,ilat)    = cvtfcs(il)
!clux add snwdph, hice, fice
          sfc_fld%snwdph(ilon,ilat) = swdfcs(il)
          sfc_fld%hice(ilon,ilat)   = sihfcs(il)
          sfc_fld%fice(ilon,ilat)   = sicfcs(il)
          sfc_fld%tisfc(ilon,ilat)  = sitfcs(il)
!clux add slc, shdmin, shdmax, slope, snoalb
          sfc_fld%slc(ilon,:,ilat)  = slcfcs(il,:)
          sfc_fld%shdmin(ilon,ilat) = vmnfcs(il)
          sfc_fld%shdmax(ilon,ilat) = vmxfcs(il)
          sfc_fld%slope(ilon,ilat)  = slpfcs(il)
          sfc_fld%snoalb(ilon,ilat) = absfcs(il)
!
        enddo     !-----end longitude loop------------------------------
!
      enddo       !-----end latitude loop-------------------------------
!
      if (me .eq. 0) print*,'executed gcycle during hour=',fhour
      
      return
      end

