      module vrbls2d
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      real, allocatable ::                                                   &
      U10   (:,:),AKMS  (:,:),AKHS  (:,:),THS   (:,:),QS(:,:)                &
      ,UZ0(:,:),VZ0(:,:),THZ0(:,:),QZ0(:,:)                                  &
      ,SNO   (:,:),TSHLTR   (:,:),QSHLTR(:,:), MRSHLTR(:,:)                  &
      ,V10(:,:),ACPREC(:,:),CUPREC(:,:),ANCPRC(:,:),CUPPT(:,:)               &
      ,SMSTAV(:,:),SSROFF(:,:),BGROFF(:,:),VEGFRC(:,:)                       &
      ,ACSNOW(:,:),ACSNOM(:,:),CMC(:,:),SST(:,:)                             &
      ,RSWIN(:,:),RLWIN(:,:),RLWTOA(:,:)                                     &
      ,TG(:,:),SFCSHX(:,:),PSLP(:,:)                                         &
      ,SFCLHX(:,:),FIS(:,:),T500(:,:),Z1000(:,:),SLP(:,:)                    &
      ,CFRACL(:,:),CFRACM(:,:),CFRACH(:,:),ACFRST(:,:)                       &
      ,ACFRCV(:,:),NCFRST(:,:),NCFRCV(:,:),HBOT(:,:)                         &
      ,HTOP(:,:),ASWIN(:,:),ALWIN(:,:),ASWOUT(:,:)                           &
      ,ALWOUT(:,:),ASWTOA(:,:),ALWTOA(:,:),CZEN(:,:)                         &
      ,CZMEAN(:,:),SIGT4(:,:),RSWOUT(:,:),RADOT(:,:)                         &
      ,SMSTOT(:,:),PCTSNO(:,:),PSHLTR(:,:),TH10(:,:)                         &
      ,Q10(:,:),SR(:,:),PREC(:,:),SUBSHX(:,:)                                &
      ,SNOPCX(:,:),SFCUVX(:,:),SFCEVP(:,:),POTEVP(:,:)                       &
      ,Z0(:,:),USTAR(:,:),TWBS(:,:),QWBS(:,:)                                &
      ,SFCEXC(:,:),GRNFLX(:,:),SOILTB(:,:),F(:,:)                            &
      ,ALBEDO(:,:),CLDFRA(:,:),CPRATE(:,:),CNVCFR(:,:)                       &
      ,PBLH(:,:),HBOTD(:,:),HTOPD(:,:),HBOTS(:,:),HTOPS(:,:)                 &
      ,CLDEFI(:,:),ALBASE(:,:),SI(:,:),LSPA(:,:)                             &
      ,RSWINC(:,:),VIS(:,:),PD(:,:),MXSNAL(:,:),MIXHT(:,:)                   &
      ,SNONC(:,:),EPSR(:,:)                                                  &
! HWRF additions
      ,MDLTAUX(:,:),MDLTAUY(:,:)                                             &
! NAMB additions
      ,SNOAVG(:,:),PSFCAVG(:,:),T10AVG(:,:),AKHSAVG(:,:),AKMSAVG(:,:)        &
      ,T10M(:,:),U10MAX(:,:),V10MAX(:,:),u10h(:,:),v10h(:,:)                 &
      ,PRATE_MAX(:,:),FPRATE_MAX(:,:)                                        &
! GSD addition
      ,WSPD10MAX(:,:),W_UP_MAX(:,:),W_DN_MAX(:,:),REFD_MAX(:,:)              &
      ,UP_HELI_MAX(:,:),UP_HELI_MAX16(:,:),GRPL_MAX(:,:),QRMAX(:,:)          &
      ,UP_HELI(:,:),UP_HELI16(:,:),LTG1_MAX(:,:),LTG2_MAX(:,:),LTG3_MAX(:,:) &
      ,NCI_LTG(:,:),NCA_LTG(:,:),NCI_WQ(:,:),NCA_WQ(:,:)                     &
      ,NCI_REFD(:,:),NCA_REFD(:,:)                                           &
      ,RAINC_BUCKET(:,:),RAINNC_BUCKET(:,:),SNOW_BUCKET(:,:)                 &
      ,GRAUP_BUCKET(:,:),PCP_BUCKET(:,:)                                          &
      ,SNOWNC(:,:),GRAUPELNC(:,:),TMAX(:,:),W_MEAN(:,:)                      &
      ,TSNOW(:,:),QVG(:,:),QV2m(:,:),QVl1(:,:)                               &
      ,REFC_10CM(:,:), REF1KM_10CM(:,:), REF4KM_10CM(:,:)                    &
      ,SWRADmean(:,:),U10mean(:,:),V10mean(:,:),SPDUV10mean(:,:)             &
      ,SWNORMmean(:,:),SNFDEN(:,:),SNDEPAC(:,:),SWDDNI(:,:),SWDDIF(:,:)      &
! add new fields for GFS
      ,SFCUX(:,:),SFCVX(:,:),AVGALBEDO(:,:),AVGCPRATE(:,:)                   &
      ,AVGPREC(:,:),PTOP(:,:),PBOT(:,:),AVGCFRACH(:,:)                       &
      ,AVGCFRACM(:,:),AVGCFRACL(:,:),AVGTCDC(:,:)                            &
      ,AUVBIN(:,:),AUVBINC(:,:)                                              &
      ,ptopl(:,:),pbotl(:,:),Ttopl(:,:)                                      &
      ,ptopm(:,:),pbotm(:,:),Ttopm(:,:)                                      &
      ,ptoph(:,:),pboth(:,:),Ttoph(:,:)                                      &
      ,sfcugs(:,:),sfcvgs(:,:),PBLCFR(:,:)                                   &
      ,cldwork(:,:),gtaux(:,:),gtauy(:,:),runoff(:,:)                        &
      ,maxtshltr(:,:),mintshltr(:,:),maxrhshltr(:,:)                         &
      ,minrhshltr(:,:),dzice(:,:),maxqshltr(:,:),minqshltr(:,:)              &
      ,alwinc(:,:),alwoutc(:,:),alwtoac(:,:)                                 &
      ,aswinc(:,:),aswoutc(:,:),aswtoac(:,:),aswintoa(:,:)                   &
      ,smcwlt(:,:),suntime(:,:),fieldcapa(:,:)                               &
      ,avisbeamswin(:,:),avisdiffswin(:,:),airbeamswin(:,:)                  &
      ,airdiffswin(:,:),snowfall(:,:),acond(:,:),edir(:,:),ecan(:,:) &
      ,etrans(:,:),esnow(:,:),avgedir(:,:),avgecan(:,:),avgetrans(:,:)&
      ,avgesnow(:,:),avgpotevp(:,:)
      integer, allocatable :: IVGTYP(:,:),ISLTYP(:,:),ISLOPE(:,:) 
! Add 2d aerosol diagnosis fields for GOCART (NGAC)
      real, allocatable ::                                                   &
       DUSMASS(:,:),DUCMASS(:,:),DUSMASS25(:,:),DUCMASS25(:,:)               &
      ,SUSMASS(:,:),SUCMASS(:,:),SUSMASS25(:,:),SUCMASS25(:,:)               &
      ,OCSMASS(:,:),OCCMASS(:,:),OCSMASS25(:,:),OCCMASS25(:,:)               &
      ,BCSMASS(:,:),BCCMASS(:,:),BCSMASS25(:,:),BCCMASS25(:,:)               &
      ,SSSMASS(:,:),SSCMASS(:,:),SSSMASS25(:,:),SSCMASS25(:,:) 
!
      end module vrbls2d
