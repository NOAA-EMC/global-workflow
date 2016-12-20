     subroutine getlvls(param,ithfld,ifld,found_fld,kpv,pv)
!
!   03_10_2015  Lin Gan  - Using flat file data
!

      use xml_perl_data,   only : param_t
      use ctlblk_mod,      only : lsm, spl, nsoil, isf_surface_physics, &
                                  me, nfd, htfd, nbnd, petabnd
      use RQSTFLD_mod,     only : lvls, lvlsxml, mxlvl, lvls, iget, ident, &
                                  iavblfld
      use soil,            only : SLDPTH,SLLEVEL
      implicit none
!
      type(param_t),intent(in) :: param
      integer, intent(in)      :: ithfld,kpv
      integer, intent(inout)   :: ifld
      logical, intent(inout)   :: found_fld
      real,intent(in)          :: pv(1:kpv)
!
      real,parameter :: small=1.e-5
      real,parameter :: small1=1.e-3
      real,parameter :: small2=1
      integer i,j,nlevel,scalef,lvlcape,lvlcin
      logical :: lincfld
!
      lincfld=.false.
      nlevel=size(param%level)
!
      if(trim(param%fixed_sfc1_type)=='isobaric_sfc') then
        do j=1, nlevel
        iloop:  do i=1, lsm
         
             if(abs(param%level(j)-SPL(i))<small1)then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              exit iloop
             endif
           enddo  iloop
        enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='hybrid_lvl') then
        do j=1, nlevel
        iloop1:  do i=1, mxlvl
             if(nint(param%level(j))==i)then
              LVLS(i,ifld)=1
              LVLSXML(i,ifld)=j
              exit iloop1
             endif
           enddo iloop1
        enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='depth_bel_land_sfc'.and. &
          trim(param%fixed_sfc2_type)=='depth_bel_land_sfc' ) then
!         if(me==0)print *,'nsoil=',nsoil,'iSF_SURFACE_PHYSICS=',iSF_SURFACE_PHYSICS, &
!          'level=',param%level,'sldpth=',SLDPTH(1:nsoil),'sum=',sum(SLDPTH(1:nsoil))*100.
         do j=1, nlevel
           iloop2: do i=1, nsoil
            if(iSF_SURFACE_PHYSICS  ==3) then
              if(nint(param%level(j))==NINT(SLLEVEL(i)*100.)) then
                LVLS(i,ifld)=1
                LVLSXML(i,ifld)=j
                exit iloop2
              endif
            else
              if(nint(param%level2(j))==NINT(sum(SLDPTH(1:i))*100.) ) then
                LVLS(i,ifld)=1
                LVLSXML(i,ifld)=j
                exit iloop2
              endif
            endif
           enddo iloop2
         enddo
         if(trim(param%pname)=='TSOIL') then
           iget(116)=ifld
           IDENT(IFLD) = 116
           IAVBLFLD(IFLD)=ithfld
           FOUND_FLD=.true.
         elseif(trim(param%pname)=='TMP') then
           iget(574)=ifld
           IDENT(IFLD) = 574
           IAVBLFLD(IFLD)=ithfld
           FOUND_FLD=.true.
         endif
!      elseif(trim(param%fixed_sfc1_type)=='depth_bel_land_sfc'.and. &
!        trim(param%fixed_sfc2_type)==''.and.(trim(param%pname)=='TSOIL' &
!        .or.trim(param%pname)=='TMP')) then
!         iget(115)=ifld
!         IDENT(IFLD) = 115
!         IAVBLFLD(IFLD)=ithfld
!         FOUND_FLD=.true.
!         LVLS(1,ifld)=1
!         LVLSXML(1,ifld)=1
      endif
!
!for sigma level, need to check iget
      if(trim(param%pname)=='RH'.and.trim(param%fixed_sfc1_type)=='sigma_lvl'.and.  &
          trim(param%fixed_sfc2_type)=='sigma_lvl') then
         do j=1, nlevel
           if(abs(param%level(j)-33)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(066)=ifld
            IDENT(IFLD) = 066
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-67)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(081)=ifld
            IDENT(IFLD) = 081
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-33)<small.and.abs(param%level2(j)-67)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(082)=ifld
            IDENT(IFLD) = 082
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-47)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(099)=ifld
            IDENT(IFLD) = 099
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-47)<small.and.abs(param%level2(j)-96)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(100)=ifld
            IDENT(IFLD) = 100
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-18)<small.and.abs(param%level2(j)-47)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(101)=ifld
            IDENT(IFLD) = 101
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-84)<small.and.abs(param%level2(j)-98)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(102)=ifld
            IDENT(IFLD) = 102
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-44)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(318)=ifld
            IDENT(IFLD) = 318
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-44)<small.and.abs(param%level2(j)-72)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(320)=ifld
            IDENT(IFLD) = 320
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-72)<small.and.abs(param%level2(j)-94)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(319)=ifld
            IDENT(IFLD) = 319
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           endif
!          print *,'n getlvls,RH_sigma_lvl,ifld=',ifld-1,'ident(ifld)=',IDENT(IFLD-1), &
!           'iget=',iget(IDENT(IFLD-1)),'IAVBLFLD(IFLD-1)=',IAVBLFLD(IFLD-1),'lvl=',lvls(1,ifld-1), &
!           'iget(66)=',iget(66)
         enddo
         ifld=ifld-1
      endif
!
      if(trim(param%pname)=='RH'.and.trim(param%fixed_sfc1_type)=='sigma_lvl'.and.  &
          trim(param%fixed_sfc2_type)=='') then
         do j=1, nlevel
           if(abs(param%level(j)-9823)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(094)=ifld
            IDENT(IFLD) = 094
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-9950)<small)then
            LVLS(1,ifld)=j
            LVLSXML(1,ifld)=j
            iget(323)=ifld
            IDENT(IFLD) = 323
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           endif
!          print *,'in getlvls,RH_sigma_lvl,1 lvl,ifld=',ifld-1,'ident(ifld)=',IDENT(IFLD-1), &
!           'iget=',iget(IDENT(IFLD-1)),'IAVBLFLD(IFLD-1)=',IAVBLFLD(IFLD-1),'lvl=',lvls(1,ifld-1), &
!           'iget(66)=',iget(66)
         enddo
         ifld=ifld-1
      endif
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='PRES') then
         do j=1, nlevel
           if(abs(param%level(j)-98230)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(091)=ifld
            IDENT(IFLD) = 091
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
           endif
         enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='TMP') then
         do j=1, nlevel
           if(abs(param%level(j)-9823)<small1)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(092)=ifld
            IDENT(IFLD) = 092
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            lincfld=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-8967)<small1)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(097)=ifld
            IDENT(IFLD) = 097
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            lincfld=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-7848)<small1)then
            if(me==0)print *,'indie tmp sigma 7848'
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(098)=ifld
            IDENT(IFLD) = 098
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            lincfld=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-9950)<small1)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(321)=ifld
            IDENT(IFLD) = 321
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            lincfld=.true.
            ifld=ifld+1
           endif
         enddo
         if(lincfld) ifld=ifld-1
         do j=1, nlevel
           if(abs(param%level(j)-7000)<small1.or. abs(param%level(j)-7500)<small1 &
            .or. abs(param%level(j)-8000)<small1 .or. abs(param%level(j)-8500)<small1 &
            .or. abs(param%level(j)-9000)<small )then
            iget(296)=ifld
            IDENT(IFLD) = 296
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            if(abs(param%level(j)-7000)<small1)then
              LVLS(1,ifld)=1
              LVLSXML(1,ifld)=j
            else if(abs(param%level(j)-7500)<small1)then
              LVLS(2,ifld)=1
              LVLSXML(2,ifld)=j
            else if(abs(param%level(j)-8000)<small1)then
              LVLS(3,ifld)=1
              LVLSXML(3,ifld)=j
            else if(abs(param%level(j)-8500)<small1)then
              LVLS(4,ifld)=1
              LVLSXML(4,ifld)=j
            else if(abs(param%level(j)-9000)<small1)then
              LVLS(5,ifld)=1
              LVLSXML(5,ifld)=j
            endif
           endif
         enddo
      endif
!
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='SPF_H') then
         do j=1, nlevel
           if(abs(param%level(j)-98230)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(093)=ifld
            IDENT(IFLD) = 093
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
           endif
         enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='U_GRD') then
         do j=1, nlevel
           if(abs(param%level(j)-98230)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(095)=ifld
            IDENT(IFLD) = 095
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-9950)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(324)=ifld
            IDENT(IFLD) = 324
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           endif
         enddo
         ifld=ifld-1
      endif
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='V_GRD') then
         do j=1, nlevel
           if(abs(param%level(j)-98230)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(096)=ifld
            IDENT(IFLD) = 096
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           else if(abs(param%level(j)-9950)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(325)=ifld
            IDENT(IFLD) = 325
            IAVBLFLD(IFLD)=ithfld
            FOUND_FLD=.true.
            ifld=ifld+1
           endif
         enddo
         ifld=ifld-1
      endif
!
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='PWAT') then
         do j=1, nlevel
           if(abs(param%level(j)-33)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(104)=ifld
           endif
         enddo
      endif
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='MCONV') then
         do j=1, nlevel
           if(abs(param%level(j)-85)<small.and.abs(param%level2(j)-100)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(103)=ifld
           endif
         enddo
      endif
      if(trim(param%fixed_sfc1_type)=='sigma_lvl'.and.trim(param%pname)=='V_VEL') then
         do j=1, nlevel
           if(abs(param%level(j)-9950)<small)then
            LVLS(1,ifld)=1
            LVLSXML(1,ifld)=j
            iget(326)=ifld
           endif
         enddo
      endif
!
!for CAPE
      if(trim(param%pname)=='CAPE') then
        if(trim(param%fixed_sfc1_type)=='surface') then
          IGET(032)=ifld
          LVLS(1,IFLD)=ifld
          LVLSXML(1,ifld)=j
          IDENT(IFLD) = 032
          IAVBLFLD(IFLD)=ithfld
        elseif (trim(param%fixed_sfc1_type)=='spec_pres_above_grnd'.and.   &
          trim(param%fixed_sfc2_type)=='spec_pres_above_grnd' ) then
          do j=1, nlevel
            if(abs(param%level(j)-18000.)<small1.and.abs(param%level2(j)-0.)<small1)then
              IGET(566)=ifld
              LVLS(1,IFLD)=1
              LVLSXML(1,ifld)=j
              IDENT(IFLD) = 566
              IAVBLFLD(IFLD)=ithfld
!jw            elseif(abs(param%level(j)-9000.)<small1.and.abs(param%level2(j)-0.)<small1)then
!jw              LVLS(2,IFLD)=1
!jw              LVLSXML(2,ifld)=j
!jw            elseif(abs(param%level(j)-25500.)<small1.and.abs(param%level2(j)-0.)<small1)then
!jw              LVLS(3,IFLD)=1
!jw              LVLSXML(3,ifld)=j
            endif
          enddo
        endif
        return
      endif
!
!
!for CIN
      if(trim(param%pname)=='CIN') then
        if(trim(param%fixed_sfc1_type)=='surface') then
          IGET(107)=ifld
          LVLS(1,IFLD)=IFLD
          IDENT(IFLD) = 107
          IAVBLFLD(IFLD)=ithfld
        elseif (trim(param%fixed_sfc1_type)=='spec_pres_above_grnd'.and.   &
          trim(param%fixed_sfc2_type)=='spec_pres_above_grnd' ) then
          do j=1, nlevel
            if(abs(param%level(j)-18000.)<small1.and.abs(param%level2(j)-0.)<small1)then
              IGET(567)=ifld
              LVLS(1,IFLD)=1
              LVLSXML(1,ifld)=j
              IDENT(IFLD) = 567
              IAVBLFLD(IFLD)=ithfld
            endif
          enddo
        endif
        return
      endif
!
!for pv sfc
      if(trim(param%fixed_sfc1_type)=='pot_vort_sfc') then
         do j=1, nlevel
           scalef=param%scale_fact_fixed_sfc1(j)-6
           if(param%scale_fact_fixed_sfc1(j)<6) scalef=0
        iloop3:  do i=1, kpv
           if(pv(i)/=0.and.abs(param%level(j)*10.**(-1*scalef)-pv(i))<=1.e-5) then
            LVLS(i,ifld)=1
            LVLSXML(i,ifld)=j
            exit iloop3
           endif
         enddo iloop3
         enddo
!        print *,'for level type pv,nlevel=',nlevel,'level=',  &
!          param%level(1:nlevel)*10.**(-1*scalef), &
!          'pv=',pv(1:kpv),lvls1(1:kpv),'ifld=',ifld,'var=',trim(param%pname), &
!          'lvl type=',trim(param%fixed_sfc1_type)
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_alt_above_mean_sea_lvl') then
         do j=1, nlevel
        iloop4:  do i=1, NFD
           if(nint(param%level(j))==nint(HTFD(i)) )then
            if(HTFD(i)>300.) then
              LVLS(i,ifld)=1
            else
              LVLS(i,ifld)=2
            endif
            LVLSXML(i,ifld)=j
            exit iloop4
           endif
         enddo iloop4
         enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_pres_above_grnd') then
         do j=1, nlevel
        iloop5:  do i=1, NBND
           if(nint(param%level(j)/100.)==nint(PETABND(i)+15.))then
            LVLS(i,ifld)=1
            LVLSXML(i,ifld)=j
            exit iloop5
           endif
         enddo iloop5
         if(nint(param%level(j)/100.)==255) then
           LVLS(NBND+1,ifld)=1
           LVLSXML(NBND+1,ifld)=j
	 endif
         enddo
      endif
!
      if(trim(param%fixed_sfc1_type)=='spec_hgt_lvl_above_grnd') then
         do j=1, nlevel
            LVLS(j,ifld)=1
            LVLSXML(j,ifld)=j
         enddo
      endif
!     
      end
