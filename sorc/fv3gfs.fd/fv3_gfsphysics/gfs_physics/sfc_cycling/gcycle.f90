      SUBROUTINE GCYCLE(ME,len,nblks,lsoil,IDATE,FHOUR,FHCYC,Dyn_parms, &
     &                  Sfc_props,Cld_props,Tbd_data,ialb,use_ufo,nst_anl)
!
!RAB      USE MACHINE
      USE PHYSCONS, PI => con_PI
      use nuopc_physics, only: dynamic_parameters, sfc_properties, &
                               cloud_properties, tbd_ddt
      use fms_io_mod,    only: open_namelist_file, close_file
      implicit none
!
      TYPE(dynamic_parameters), intent(in)    :: Dyn_parms(nblks)
      TYPE(sfc_properties),     intent(inout) :: Sfc_props(nblks)
      TYPE(cloud_properties),   intent(inout) :: Cld_props(nblks)
      TYPE(Tbd_ddt),            intent(inout) :: Tbd_data(nblks)
!
      INTEGER, intent(in) :: len, nblks, lsoil, IALB
      INTEGER, intent(in) :: ME, IDATE(4)
      logical, intent(in) :: use_ufo, nst_anl
      real(kind=kind_phys), intent(in) :: fhour, fhcyc
!      real(kind=kind_phys), intent(in) :: XLON(len), XLAT(len)

      integer :: nlunit
!
!     Local variables
!     ---------------
      integer il, i, l
!
      real(kind=kind_phys) ::     SLMASK(len),       &
     &      RLA(len),           RLO(len),          &
     &      OROG(len),          OROG_UF(len),      &
     &      TSFFCS(len),        SNOFCS(len),       &
     &      ZORFCS(len),        ALBFCS(len,4),     &
     &      TG3FCS(len),        CNPFCS(len),       &
     &      SMCFCS(len,LSOIL),  STCFCS(len,LSOIL), &
     &      SLIFCS(len),        AISFCS(len),       &
     &      F10MFCS(len),       VEGFCS(len),       &
     &      VETFCS(len),        SOTFCS(len),       &
     &      ALFFCS(len,2),      CVFCS(len),        &
     &      CVBFCS(len),        CVTFCS(len),       &
     &      SMCFC1(len*LSOIL),  STCFC1(len*LSOIL), &
     &      ALBFC1(len*4),      ALFFC1(len*2),     &
!CluX add swdfcs, sihfcs, sicfcs
     &      SWDFCS(len),        SIHFCS(len),       &
     &      SICFCS(len),        SITFCS(len),       &
!CluX add vmnfcs, vmxfcs, slpfcs, absfcs, slcfc1, slcfcs
     &      VMNFCS(len),        VMXFCS(len),       &
     &      SLPFCS(len),        ABSFCS(len),       &
     &      SLCFC1(len*LSOIL),  SLCFCS(len,LSOIL)


      real(kind=kind_phys) :: sig1t, pifac
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!     if (me .eq. 0) print *,' nlats=',nlats,' lonsinpe='
!    *,lonsinpe(0,1)

      sig1t = 0.0
!
      pifac = 180.0 / pi

      il=0
      DO i = 1,nblks
        DO l = 1,size(Dyn_parms(i)%xlat,1)
          il=il+1
!     print *,' calling gcycle for ilat',ilat,' me=',me,' nlats='
!    *,nlats,' lonsinpe=',lonsinpe(:,ilat)
!     if (ilat .eq. nlats) stop
!
          RLA(il)      = Dyn_parms(i)%xlat(l) * pifac
          RLO(il)      = Dyn_parms(i)%xlon(l) * pifac
          OROG(il)     = Sfc_props(i)%ORO(l)
          OROG_UF(il)  = Sfc_props(i)%ORO_UF(l)
          TSFFCS(il)   = Sfc_props(i)%TSFC(l)
          SNOFCS(il)   = Sfc_props(i)%WEASD(l)
          ZORFCS(il)   = Sfc_props(i)%ZORL(l)
          ALBFCS(il,1) = Sfc_props(i)%ALVSF(l)
          ALBFCS(il,2) = Sfc_props(i)%ALVWF(l)
          ALBFCS(il,3) = Sfc_props(i)%ALNSF(l)
          ALBFCS(il,4) = Sfc_props(i)%ALNWF(l)
          TG3FCS(il)   = Sfc_props(i)%TG3(l)
          CNPFCS(il)   = Sfc_props(i)%CANOPY(l)
          SMCFCS(il,:) = Tbd_data(i)%SMC(l,:)
          STCFCS(il,:) = Tbd_data(i)%STC(l,:)
          SLIFCS(il)   = Sfc_props(i)%SLMSK(l)
          F10MFCS(il)  = Sfc_props(i)%F10M(l)
          VEGFCS(il)   = Sfc_props(i)%VFRAC(l)
          VETFCS(il)   = Sfc_props(i)%VTYPE(l)
          SOTFCS(il)   = Sfc_props(i)%STYPE(l)
          ALFFCS(il,1) = Sfc_props(i)%FACSF(l)
          ALFFCS(il,2) = Sfc_props(i)%FACWF(l)
          CVFCS(il)    = Cld_props(i)%CV(l)
          CVBFCS(il)   = Cld_props(i)%CVB(l)
          CVTFCS(il)   = Cld_props(i)%CVT(l)
!CluX add swdfcs, sihfcs, sicfcs
          SWDFCS(il)   = Sfc_props(i)%SNOWD(l)
          SIHFCS(il)   = Sfc_props(i)%HICE(l)
          SICFCS(il)   = Sfc_props(i)%FICE(l)
          SITFCS(il)   = Sfc_props(i)%TISFC(l)
!CluX add slcfcs, vmnfcs, vmxfcs, slpfcs, absfcs
          SLCFCS(il,:) = Tbd_data(i)%SLC(l,:)
          VMNFCS(il)   = Sfc_props(i)%SHDMIN(l)
          VMXFCS(il)   = Sfc_props(i)%SHDMAX(l)
          SLPFCS(il)   = Sfc_props(i)%SLOPE(l)
          ABSFCS(il)   = Sfc_props(i)%SNOALB(l)

!
          IF (SLIFCS(il) .LT. 0.1 .OR. SLIFCS(il) .GT. 1.5) THEN
             SLMASK(il) = 0
          ELSE
             SLMASK(il) = 1
          ENDIF

          IF (SLIFCS(il) .EQ. 2) THEN
            AISFCS(il) = 1.
          ELSE
            AISFCS(il) = 0.
          ENDIF

!     if (me .eq. 0) & 
!    &   print *,' len=',il,' rla=',rla(il),' rlo=',rlo(il)
        ENDDO       
      ENDDO       !-----END nblks LOOP-------------------------------
!
! check
!     print *,' total points = ',il
!
      do l=1,lsoil
        il = (l-1)*len
        do i=1,len
          SMCFC1(il+i) = SMCFCS(i,l)
          STCFC1(il+i) = STCFCS(i,l)
          SLCFC1(il+i) = SLCFCS(i,l)
        enddo
      enddo
      do l=1,4
        il = (l-1)*len
        do i=1,len
          ALBFC1(il+i) = ALBFCS(i,l)
        enddo
      enddo
      do l=1,2
        il = (l-1)*len
        do i=1,len
          ALFFC1(il+i) = ALFFCS(i,l)
        enddo
      enddo
! check
!     call mymaxmin(slifcs,len,len,1,'slifcs')
!     call mymaxmin(slmask,len,len,1,'slmsk')
!
      nlunit = open_namelist_file()
      CALL SFCCYCLE(1001,LEN,LSOIL,SIG1T,fhcyc,                         &
     &              idate(4), idate(2), idate(3), idate(1), fhour,     &
     &              RLA, RLO, SLMASK, OROG, OROG_UF, USE_UFO, nst_anl, &
     &              SIHFCS,   SICFCS, SITFCS,                          &
     &              SWDFCS,   SLCFC1,                                  &
     &              VMNFCS,   VMXFCS, SLPFCS, ABSFCS,                  &
     &              TSFFCS,   SNOFCS, ZORFCS, ALBFC1, TG3FCS,          &
     &              CNPFCS,   SMCFC1, STCFC1, SLIFCS, AISFCS, F10MFCS, &
     &              VEGFCS,   VETFCS, SOTFCS, ALFFC1,                  &
     &              CVFCS,    CVBFCS, CVTFCS, me, nlunit, ialb)
      call close_file(nlunit)
!
      do l=1,lsoil
        il = (l-1)*len
        do i=1,len
          SMCFCS(i,l) = SMCFC1(il+i)
          STCFCS(i,l) = STCFC1(il+i)
          SLCFCS(i,l) = SLCFC1(il+i)
        enddo
      enddo
      do l=1,4
        il = (l-1)*len
        do i=1,len
          ALBFCS(i,l) = ALBFC1(il+i)
        enddo
      enddo
      do l=1,2
        il = (l-1)*len
        do i=1,len
          ALFFCS(i,l) = ALFFC1(il+i)
        enddo
      enddo
!
      il=0
      DO i = 1,nblks
        DO l = 1,size(Dyn_parms(i)%xlat,1)
          il=il+1
          Sfc_props(i)%TSFC(l)   = TSFFCS(il)
          Sfc_props(i)%WEASD(l)  = SNOFCS(il)
          Sfc_props(i)%ZORL(l)   = ZORFCS(il)
          Sfc_props(i)%ALVSF(l)  = ALBFCS(il,1)
          Sfc_props(i)%ALVWF(l)  = ALBFCS(il,2)
          Sfc_props(i)%ALNSF(l)  = ALBFCS(il,3)
          Sfc_props(i)%ALNWF(l)  = ALBFCS(il,4)
          Sfc_props(i)%TG3(l)    = TG3FCS(il)
          Sfc_props(i)%CANOPY(l) = CNPFCS(il)
          Tbd_data(i)%SMC(l,:)   = SMCFCS(il,:)
          Tbd_data(i)%STC(l,:)   = STCFCS(il,:)
          Sfc_props(i)%SLMSK(l)  = SLIFCS(il)
          Sfc_props(i)%F10M(l)   = F10MFCS(il)
          Sfc_props(i)%VFRAC(l)  = VEGFCS(il)
          Sfc_props(i)%VTYPE(l)  = VETFCS(il)
          Sfc_props(i)%STYPE(l)  = SOTFCS(il)
          Sfc_props(i)%FACSF(l)  = ALFFCS(il,1)
          Sfc_props(i)%FACWF(l)  = ALFFCS(il,2)
          Cld_props(i)%CV(l)     = CVFCS(il)
          Cld_props(i)%CVB(l)    = CVBFCS(il)
          Cld_props(i)%CVT(l)    = CVTFCS(il)
          Sfc_props(i)%SNOWD(l)  = SWDFCS(il)
          Sfc_props(i)%HICE(l)   = SIHFCS(il)
          Sfc_props(i)%FICE(l)   = SICFCS(il)
          Sfc_props(i)%TISFC(l)  = SITFCS(il)
          Tbd_data(i)%SLC(l,:)   = SLCFCS(il,:)
          Sfc_props(i)%SHDMIN(l) = VMNFCS(il)
          Sfc_props(i)%SHDMAX(l) = VMXFCS(il)
          Sfc_props(i)%SLOPE(l)  = SLPFCS(il)
          Sfc_props(i)%SNOALB(l) = ABSFCS(il)
        ENDDO       
      ENDDO       !-----END nblks LOOP-------------------------------
!
!     if (me .eq. 0) print*,'executed gcycle during hour=',fhour
      
      RETURN
      END

