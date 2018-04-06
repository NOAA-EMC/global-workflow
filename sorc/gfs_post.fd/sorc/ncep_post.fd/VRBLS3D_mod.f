!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-04-17  BALDWIN  - MODIFIED TO INCLUDE ALL 3D ARRAYS
!   11-10-18  SARAH LU - MODIFIED TO INCLUDE AEROSOL OPTICAL PROPERTIES
!   11-12-15  SARAH LU - MODIFIED TO INCLUDE AEROSOL DIAG FIELDS
!   12-01-06  SARAH LU - MODIFIED TO INCLUDE AIR DENSITY AND LAYER THICKNESS
      module vrbls3d
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      real, allocatable :: UH(:,:,:),VH(:,:,:),WH(:,:,:)             &
      ,U(:,:,:),V(:,:,:),T(:,:,:),Q(:,:,:)                           &
      ,CWM(:,:,:),Q2(:,:,:),PMID(:,:,:),PMIDV(:,:,:)                 &
      ,PINT(:,:,:),ALPINT(:,:,:),ZMID(:,:,:)                         &
      ,ZINT(:,:,:),OMGA(:,:,:)                                       &
      ,T_ADJ(:,:,:)                                                  &
      ,F_ice(:,:,:),F_rain(:,:,:),F_RimeF(:,:,:)                     &
      ,QQW(:,:,:), QQI(:,:,:), QQR(:,:,:), QQS(:,:,:), QQG(:,:,:)    &
      ,QQNW(:,:,:), QQNI(:,:,:),QQNR(:,:,:)                          &
      ,CFR(:,:,:), DBZ(:,:,:), DBZR(:,:,:), DBZI(:,:,:), DBZC(:,:,:) &
      ,TTND(:,:,:),RSWTT(:,:,:),RLWTT(:,:,:), REF_10CM(:,:,:)        &
      ,EXCH_H(:,:,:),TRAIN(:,:,:),TCUCN(:,:,:),EL_PBL(:,:,:)         &
      ,MCVG(:,:,:),EXTCOF55(:,:,:),NLICE(:,:,:),CFR_RAW(:,:,:)       &
!! Wm Lewis: added
      ,NRAIN(:,:,:)                                                  &
      ,radius_cloud(:,:,:),radius_ice(:,:,:),radius_snow(:,:,:)      &
! Add GFS fields     
      ,O3(:,:,:),O(:,:,:),O2(:,:,:)              &
! Add GFS D3D fields
      ,vdifftt(:,:,:)         &
      ,tcucns(:,:,:)          &
      ,vdiffmois(:,:,:)       &
      ,dconvmois(:,:,:)       &
      ,sconvmois(:,:,:)       &
      ,nradtt(:,:,:)          &  
      ,o3vdiff(:,:,:)         &
      ,o3prod(:,:,:)          &
      ,o3tndy(:,:,:)          &
      ,mwpv(:,:,:)            &
      ,unknown(:,:,:)         &
      ,vdiffzacce(:,:,:)      &
      ,zgdrag(:,:,:)          &
      ,cnvctummixing(:,:,:)   &
      ,vdiffmacce(:,:,:)      &
      ,mgdrag(:,:,:)          &
      ,cnvctvmmixing(:,:,:)   &
      ,ncnvctcfrac(:,:,:)     &
      ,cnvctumflx(:,:,:)      &
      ,cnvctdmflx(:,:,:)      &  
      ,cnvctdetmflx(:,:,:)    &
      ,cnvctzgdrag(:,:,:)     &
      ,cnvctmgdrag(:,:,:)     &   
      ,QQNWFA(:,:,:)          &
      ,QQNIFA(:,:,:)          &
!
! Add aerosol optical properties for GOCART (NGAC)
      ,ext(:,:,:), asy(:,:,:)           &
      ,ssa(:,:,:)                       &
! Add aerosol diagnosis fields for GOCART (NGAC)
      ,duem(:,:,:), dusd(:,:,:)         &
      ,dudp(:,:,:), duwt(:,:,:)         &
      ,suem(:,:,:), susd(:,:,:)         &
      ,sudp(:,:,:), suwt(:,:,:)         &
      ,ssem(:,:,:), sssd(:,:,:)         &
      ,ssdp(:,:,:), sswt(:,:,:)         &
      ,ocem(:,:,:), ocsd(:,:,:)         &
      ,ocdp(:,:,:), ocwt(:,:,:)         &
      ,bcem(:,:,:), bcsd(:,:,:)         &
      ,bcdp(:,:,:), bcwt(:,:,:)         &
! Add air density and thickness for GOCART (NGAC)
      ,dpres(:,:,:),rhomid(:,:,:)       &  

! Add NCAR GFIP ICING
      ,icing_gfip(:,:,:),icing_gfis(:,:,:) &
! Add NCAR GTG turbulence
      ,catedr(:,:,:),mwt(:,:,:),gtg(:,:,:)

      end module vrbls3d
