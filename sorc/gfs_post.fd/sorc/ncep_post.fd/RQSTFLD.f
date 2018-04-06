    module RQSTFLD_mod
!--------------------------------------------------------------------
! revision history
!   2011-02-06 Jun Wang add grib2 option
!   2011-10-18 Sarah Lu add GOCART aerosol fields
!   2011-12-18 Sarah Lu add GOCART aerosol optical properties, mass
!                           fields, and production/removal fluxes
!   2011-12-29 Sarah Lu add GOCART AOD at multiple channels
!   2012-01-06 Sarah Lu add GOCART SS, OC, BC, SU aerosols
!   2012-01-07 Sarah Lu add air density and dpres
!   2012-01-27 Sarah Lu use index 601-700 for GOCART
!   2012-01-30 Jun Wang add post available fields from xml file for grib2
!   2012-05-07 Tricia Slovacek use index 800-899 for satellite
!                       use index 900-949 for HWRF
!   2014-12-09 William Lewis added MSG/SEVIRI imager, 
!                      GOES-13 and GOES-15 imagers,
!                      and completed SSMI and SSMIS (F13-F20)
!--------------------------------------------------------------------

      implicit none
!
!     increase MXFLD each time you add a new field
      INTEGER, PARAMETER :: MXFLD=950,MXLVL=70
      CHARACTER*20 AVBL(MXFLD),FIELD(MXFLD)
      CHARACTER*50 AVBLGRB2(MXFLD)
      CHARACTER*6 DATSET      
!
      LOGICAL RITEHD,RITE2
!
      integer :: KGTYPE,IOUTYP,SVALUE,NFLD,IGET(MXFLD),           &
                 IQ(MXFLD),IS(MXFLD),ISMSTG(MXFLD),               &
                 ISMFUL(MXFLD),ISMOUT(MXFLD),LVLS(MXLVL,MXFLD),   &
                 IDENT(MXFLD),IFILV(MXFLD),IAVBLFLD(MXFLD),       &
                 ID(25),IGDS(18)
      real    :: DEC(MXFLD)
      integer :: num_post_afld
      integer,allocatable :: LVLSXML(:,:)
!
!initialization
!
!     THIS FILE CONTAINS ALL THE UNIQUE FIELDS THE
!     ETA POST PROCESSOR CAN CURRENTLY GENERATE.  
!
!	IFILV IS FLAG FOR IDENTIFYING MASS OR VELOCITY POINT
!	   =0 DATA IS VELOCITY POINT
!	   =1 DATA IS MASS POINT
!	AVBL IS CHARACTER STRING IDENTIFYING THE FIELD.
!	IQ  IS THE GRIB PDS OCTET 9 - PARAMETER (TABLE 2)
!	IS  IS THE GRIB PDS OCTET 10 - LEVEL TYPE (TABLE 3 & 3a)
!
!     WANT MORE/DIFFERENT FIELDS? 
!	(1) ADD CODE TO CALCULATE FIELD(S) IN APPROPRIATE ROUTINE(S),
!       (2) ADD FIELD(S) TO THIS LIST WITH A UNIQUE ITAG TAG,
!       (3) EDIT INPUT (CONTROL) FILE ACCORDINGLY,
!       (3) INCREASE PARAMETER MXFLD IN COMMON BLOCK RQSTFLD.comm.
!
!     CURRENT NUMBER OF FIELDS LISTED:  180
!
!0       1         2         3         4         5         6         7
!234567890123456789012345678901234567890123456789012345678901234567890
!
      DATA IFILV(001),AVBL(001),IQ(001),IS(001),AVBLGRB2(001)      &
     &                      /1,'PRESS ON MDL SFCS   ',001,109,     &
     &                       'PRES ON hybrid_lvl'/
      DATA IFILV(077),AVBL(077),IQ(077),IS(077),AVBLGRB2(077)      &
     &                      /1,'HEIGHT ON MDL SFCS  ',007,109,     &
     &                       'HGT ON hybrid_lvl'/
      DATA IFILV(002),AVBL(002),IQ(002),IS(002),AVBLGRB2(002)      &
     &                      /1,'TEMP ON MDL SFCS    ',011,109,     &
     &                       'TMP ON hybrid_lvl'/
      DATA IFILV(003),AVBL(003),IQ(003),IS(003),AVBLGRB2(003)      &
     &                      /1,'POT TEMP ON MDL SFCS',013,109,     &
     &                       'POT ON hybrid_lvl'/
      DATA IFILV(004),AVBL(004),IQ(004),IS(004),AVBLGRB2(004)      &
     &                      /1,'DWPT TEMP ON MDL SFC',017,109,     &
     &                       'DPT ON hybrid_lvl'/
      DATA IFILV(005),AVBL(005),IQ(005),IS(005),AVBLGRB2(005)      &
     &                      /1,'SPEC HUM ON MDL SFCS',051,109,     &
     &                       'SPF_H ON hybrid_lvl'/
      DATA IFILV(006),AVBL(006),IQ(006),IS(006),AVBLGRB2(006)      &
     &                      /1,'REL HUM ON MDL SFCS ',052,109,     &
     &                       'RH ON hybrid_lvl'/
      DATA IFILV(083),AVBL(083),IQ(083),IS(083),AVBLGRB2(083)      &
     &                      /1,'MST CNVG ON MDL SFCS',135,109,     &
     &                       'MCONV ON hybrid_lvl'/
      DATA IFILV(007),AVBL(007),IQ(007),IS(007),AVBLGRB2(007)      &
     &                      /0,'U WIND ON MDL SFCS  ',033,109,     &
     &                       'U_GRD ON hybrid_lvl'/
      DATA IFILV(008),AVBL(008),IQ(008),IS(008),AVBLGRB2(008)      &
     &                      /0,'V WIND ON MDL SFCS  ',034,109,     &
     &                       'V_GRD ON hybrid_lvl'/
      DATA IFILV(009),AVBL(009),IQ(009),IS(009),AVBLGRB2(009)      &
     &                      /1,'OMEGA ON MDL SFCS   ',039,109,     &
     &                       'V_VEL ON hybrid_lvl'/
      DATA IFILV(010),AVBL(010),IQ(010),IS(010),AVBLGRB2(010)      &
     &                      /1,'ABS VORT ON MDL SFCS',041,109,     &
     &                       'ABS_V ON hybrid_lvl'/
      DATA IFILV(084),AVBL(084),IQ(084),IS(084),AVBLGRB2(084)      &
     &                      /1,'STRMFUNC ON MDL SFCS',035,109,     &
     &                       'STRM ON hybrid_lvl'/
      DATA IFILV(011),AVBL(011),IQ(011),IS(011),AVBLGRB2(011)      &
     &                      /1,'TRBLNT KE ON MDL SFC',158,109,     &
     &                       'TKE ON hybrid_lvl'/
      DATA IFILV(111),AVBL(111),IQ(111),IS(111),AVBLGRB2(111)      &
     &                      /1,'RCHDSN NO ON MDL SFC',254,109,     &
     &                       'RI ON hybrid_lvl'/
      DATA IFILV(146),AVBL(146),IQ(146),IS(146),AVBLGRB2(146)      &
     &                      /1,'MASTER LENGTH SCALE ',226,109,     &
     &                       'BMIXL ON hybrid_lvl'/
      DATA IFILV(147),AVBL(147),IQ(147),IS(147),AVBLGRB2(147)      &
     &                      /1,'ASYMPT MSTR LEN SCL ',227,109,     &
     &                       'AMIXL ON hybrid_lvl'/
      DATA IFILV(012),AVBL(012),IQ(012),IS(012),AVBLGRB2(012)      &
     &                      /1,'HEIGHT OF PRESS SFCS',007,100,     &
     &                       'HGT ON isobaric_sfc'/
      DATA IFILV(013),AVBL(013),IQ(013),IS(013),AVBLGRB2(013)      &
     &                      /1,'TEMP ON PRESS SFCS  ',011,100,     &
     &                       'TMP ON isobaric_sfc'/
      DATA IFILV(014),AVBL(014),IQ(014),IS(014),AVBLGRB2(014)      &
     &                      /1,'POT TEMP ON P SFCS  ',013,100,     &
     &                       'POT ON isobaric_sfc'/
      DATA IFILV(015),AVBL(015),IQ(015),IS(015),AVBLGRB2(015)      &
     &                      /1,'DWPT TEMP ON P SFCS ',017,100,     &
     &                       'DPT ON isobaric_sfc'/
      DATA IFILV(016),AVBL(016),IQ(016),IS(016),AVBLGRB2(016)      &
     &                      /1,'SPEC HUM ON P SFCS  ',051,100,     &
     &                       'SPF_H ON isobaric_sfc'/
      DATA IFILV(017),AVBL(017),IQ(017),IS(017),AVBLGRB2(017)      &
     &                      /1,'REL HUMID ON P SFCS ',052,100,     &
     &                       'RH ON isobaric_sfc'/
      DATA IFILV(085),AVBL(085),IQ(085),IS(085),AVBLGRB2(085)      &
     &                      /1,'MST CNVG ON P SFCS  ',135,100,     &
     &                       'MCONV ON isobaric_sfc'/
      DATA IFILV(018),AVBL(018),IQ(018),IS(018),AVBLGRB2(018)      &
     &                      /1,'U WIND ON PRESS SFCS',033,100,     &
     &                       'U_GRD ON isobaric_sfc'/
      DATA IFILV(019),AVBL(019),IQ(019),IS(019),AVBLGRB2(019)      &
     &                      /1,'V WIND ON PRESS SFCS',034,100,     &
     &                       'V_GRD ON isobaric_sfc'/
      DATA IFILV(020),AVBL(020),IQ(020),IS(020),AVBLGRB2(020)      &
     &                      /1,'OMEGA ON PRESS SFCS ',039,100,     &
     &                       'V_VEL ON isobaric_sfc'/
      DATA IFILV(021),AVBL(021),IQ(021),IS(021),AVBLGRB2(021)      &
     &                      /1,'ABS VORT ON P SFCS  ',041,100,     &
     &                       'ABS_V ON isobaric_sfc'/
      DATA IFILV(086),AVBL(086),IQ(086),IS(086),AVBLGRB2(086)      &
     &                      /1,'STRMFUNC ON P SFCS  ',035,100,     &
     &                       'STRM ON isobaric_sfc'/
      DATA IFILV(022),AVBL(022),IQ(022),IS(022),AVBLGRB2(022)      &
     &                      /1,'TRBLNT KE ON P SFCS ',158,100,     &
     &                       'TKE ON isobaric_sfc'/
      DATA IFILV(153),AVBL(153),IQ(153),IS(153),AVBLGRB2(153)      &
     &                      /1,'CLOUD WATR ON P SFCS',153,100,     &
     &                       'CLWMR ON isobaric_sfc'/
      DATA IFILV(166),AVBL(166),IQ(166),IS(166),AVBLGRB2(166)      &
     &                      /1,'CLOUD ICE ON P SFCS ',058,100,     &
     &                       'C_ICE ON isobaric_sfc'/
      DATA IFILV(023),AVBL(023),IQ(023),IS(023),AVBLGRB2(023)      &
     &                      /1,'MESINGER MEAN SLP   ',130,102,     &
     &                       'MSLET ON mean_sea_lvl'/
      DATA IFILV(105),AVBL(105),IQ(105),IS(105),AVBLGRB2(105)      &
     &                      /1,'SHUELL MEAN SLP     ',002,102,     &
     &                       'PRES ON mean_sea_lvl'/
      DATA IFILV(445),AVBL(445),IQ(445),IS(445),AVBLGRB2(445)      &         !445
     &                      /1,'MAPS SLP            ',129,102,     &
     &                       'MAPS PRMSL ON mean_sea_lvl'/
      DATA IFILV(138),AVBL(138),IQ(138),IS(138),AVBLGRB2(138)      &
     &                      /1,'SHELTER PRESSURE    ',001,105,     &
     &                       'PRES ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(106),AVBL(106),IQ(106),IS(106),AVBLGRB2(106)      &
     &                      /1,'SHELTER TEMPERATURE ',011,105,     &
     &                       'TMP ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(112),AVBL(112),IQ(112),IS(112),AVBLGRB2(112)      &
     &                      /1,'SHELTER SPEC HUMID  ',051,105,     &
     &                       'SPF_H ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(414),AVBL(414),IQ(414),IS(414),AVBLGRB2(414)      &
     &                      /1,'SHELTER MIX RATIO   ',053,105,     &
     &                       'MIXR ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(113),AVBL(113),IQ(113),IS(113),AVBLGRB2(113)      &
     &                      /1,'SHELTER DEWPOINT    ',017,105,     &
     &                       'DPT ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(114),AVBL(114),IQ(114),IS(114),AVBLGRB2(114)      &
     &                      /1,'SHELTER REL HUMID   ',052,105,     &
     &                       'RH ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(064),AVBL(064),IQ(064),IS(064),AVBLGRB2(064)      &
     &                      /1,'U WIND AT ANEMOM HT ',033,105,     &
     &                       'U_GRD ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(065),AVBL(065),IQ(065),IS(065),AVBLGRB2(065)      &
     &                      /1,'V WIND AT ANEMOM HT ',034,105,     &
     &                       'V_GRD ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(158),AVBL(158),IQ(158),IS(158),AVBLGRB2(158)      &
     &                      /1,'POT TEMP AT 10 M    ',013,105,     &
     &                       'POT ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(159),AVBL(159),IQ(159),IS(159),AVBLGRB2(159)      &
     &                      /1,'SPEC HUM AT 10 M    ',051,105,     &
     &                       'SRF_H ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(024),AVBL(024),IQ(024),IS(024),AVBLGRB2(024)      &
     &                      /1,'SURFACE PRESSURE    ',001,001,     &
     &                       'PRES ON surface'/
      DATA IFILV(025),AVBL(025),IQ(025),IS(025),AVBLGRB2(025)      &
     &                      /1,'SURFACE HEIGHT      ',007,001,     &
     &                       'HGT ON surface'/
      DATA IFILV(027),AVBL(027),IQ(027),IS(027),AVBLGRB2(027)      &
     &                      /1,'SURFACE POT TEMP    ',013,001,     &
     &                       'POT ON surface'/
      DATA IFILV(028),AVBL(028),IQ(028),IS(028),AVBLGRB2(028)      &
     &                      /1,'SURFACE SPEC HUMID  ',051,001,     &
     &                       'SPF_H ON surface'/
      DATA IFILV(029),AVBL(029),IQ(029),IS(029),AVBLGRB2(029)      &
     &                      /1,'SURFACE DEWPOINT    ',017,001,     &
     &                       'DPT ON surface'/
      DATA IFILV(076),AVBL(076),IQ(076),IS(076),AVBLGRB2(076)      &
     &                      /1,'SURFACE REL HUMID   ',052,001,     &
     &                       'RH ON surface'/
      DATA IFILV(026),AVBL(026),IQ(026),IS(026),AVBLGRB2(026)      &
     &                      /1,'SFC (SKIN) TEMPRATUR',011,001,     &
     &                       'TMP ON surface'/
      DATA IFILV(115),AVBL(115),IQ(115),IS(115),AVBLGRB2(115)      &
     &                      /1,'BOTTOM SOIL TEMP    ',085,111,     &
     &                       'TSOIL ON depth_bel_land_sfc'/
      DATA IFILV(116),AVBL(116),IQ(116),IS(116),AVBLGRB2(116)      &
     &                      /1,'SOIL TEMPERATURE    ',085,112,     &
     &                       'TSOIL ON depth_bel_land_sfc'/
      DATA IFILV(117),AVBL(117),IQ(117),IS(117),AVBLGRB2(117)      &
     &                      /1,'SOIL MOISTURE       ',144,112,     &
     &                       'SOILW ON depth_bel_land_sfc'/
      DATA IFILV(036),AVBL(036),IQ(036),IS(036),AVBLGRB2(036)      &
     &                      /1,'TOTAL SOIL MOISTURE ',086,112,     &
     &                       'SOILM ON depth_bel_land_sfc'/
      DATA IFILV(118),AVBL(118),IQ(118),IS(118),AVBLGRB2(118)      &
     &                      /1,'PLANT CANOPY SFC WTR',223,001,     &
     &                       'CNWAT ON surface'/
      DATA IFILV(119),AVBL(119),IQ(119),IS(119),AVBLGRB2(119)      &
     &                      /1,'SNOW WATER EQUIVALNT',065,001,     &
     &                       'INST WEASD ON surface'/
      DATA IFILV(120),AVBL(120),IQ(120),IS(120),AVBLGRB2(120)      &
     &                      /1,'PERCENT SNOW COVER  ',238,001,     &
     &                       'SNOWC ON surface'/
      DATA IFILV(169),AVBL(169),IQ(169),IS(169),AVBLGRB2(169)      &
     &                      /1,'SFC EXCHANGE COEF   ',208,001,     &
     &                       'SFEXC ON surface'/
      DATA IFILV(170),AVBL(170),IQ(170),IS(170),AVBLGRB2(170)      &
     &                      /1,'GREEN VEG COVER     ',087,001,     &
     &                       'VEG ON surface'/
      DATA IFILV(171),AVBL(171),IQ(171),IS(171),AVBLGRB2(171)      &
     &                      /1,'SOIL MOISTURE AVAIL ',207,112,     &
     &                       'MSTAV ON depth_bel_land_sfc'/
      DATA IFILV(152),AVBL(152),IQ(152),IS(152),AVBLGRB2(152)      &
     &                      /1,'INST GROUND HEAT FLX',155,001,     &
     &                       'INST GFLUX ON surface'/
      DATA IFILV(030),AVBL(030),IQ(030),IS(030),AVBLGRB2(030)      &
     &                      /1,'LIFTED INDEX--SURFCE',131,101,     &
     &                       'LFT_X ON isobaric_sfc'/
      DATA IFILV(031),AVBL(031),IQ(031),IS(031),AVBLGRB2(031)      &
     &                      /1,'LIFTED INDEX--BEST  ',132,116,     &
     &                       '4LFTX ON spec_pres_above_grnd'/
      DATA IFILV(075),AVBL(075),IQ(075),IS(075),AVBLGRB2(075)      &
     &                      /1,'LIFTED INDEX--BNDLYR',024,116,     &
     &                       'PLI ON spec_pres_above_grnd'/
      DATA IFILV(032),AVBL(032),IQ(032),IS(032),AVBLGRB2(032)      &
     &                      /1,'CNVCT AVBL POT ENRGY',157,001,     &
     &                       'CAPE ON surface'/
      DATA IFILV(107),AVBL(107),IQ(107),IS(107),AVBLGRB2(107)      &
     &                      /1,'CNVCT INHIBITION    ',156,001,     &
     &                       'CIN ON surface'/
      DATA IFILV(080),AVBL(080),IQ(080),IS(080),AVBLGRB2(080)      &
     &                      /1,'PRECIPITABLE WATER  ',054,200,     &
     &                       'PWAT ON entire_atmos_single_lyr'/
      DATA IFILV(162),AVBL(162),IQ(162),IS(162),AVBLGRB2(162)      &
     &                      /1,'STORM REL HELICITY  ',190,106,     &
     &                       'HLCY ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(163),AVBL(163),IQ(163),IS(163),AVBLGRB2(163)      &
     &                      /1,'U COMP STORM MOTION ',196,106,     &
     &                       'USTM ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(164),AVBL(164),IQ(164),IS(164),AVBLGRB2(164)      &
     &                      /1,'V COMP STORM MOTION ',197,106,     &
     &                       'VSTM ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(087),AVBL(087),IQ(087),IS(087),AVBLGRB2(087)      &
     &                      /1,'ACM TOTAL PRECIP    ',061,001,     &
     &                       'ACM A_PCP ON surface'/
      DATA IFILV(033),AVBL(033),IQ(033),IS(033),AVBLGRB2(033)      &
     &                      /1,'ACM CONVCTIVE PRECIP',063,001,     &
     &                       'ACM ACPCP ON surface'/
      DATA IFILV(034),AVBL(034),IQ(034),IS(034),AVBLGRB2(034)      &
     &                      /1,'ACM GRD SCALE PRECIP',062,001,     &
     &                       'ACM NCPCP ON surface'/
      DATA IFILV(035),AVBL(035),IQ(035),IS(035),AVBLGRB2(035)      &
     &                      /1,'ACM SNOWFALL        ',065,001,     &
     &                       'ACM WEASD ON surface'/
      DATA IFILV(746),AVBL(746),IQ(746),IS(746),AVBLGRB2(746)      &    
     &                      /1,'ACM GRAUPEL         ',079,001,     &    
     &                       'ACM WEAGD ON surface'/
      DATA IFILV(724),AVBL(724),IQ(724),IS(724),AVBLGRB2(724)      &
     &                      /1,'SNOWFALL DENSITY    ',089,001,     &
     &                       'SNOWFALL DEN surface  '/
      DATA IFILV(725),AVBL(725),IQ(725),IS(725),AVBLGRB2(725)      &
     &                      /1,'ACM SNOW DEPTH      ',066,001,     &
     &                       'ACM SNOWDEP on surface'/
      DATA IFILV(244),AVBL(244),IQ(244),IS(244),AVBLGRB2(244)      &
     &                      /1,'ACM GRD SCALE SW ICE',079,001,     &
     &                       'ACM SNO_L ON surface'/
      DATA IFILV(121),AVBL(121),IQ(121),IS(121),AVBLGRB2(121)      &
     &                      /1,'ACM SNOW TOTAL/MELT ',099,001,     &
     &                       'ACM SNO_M ON surface'/
      DATA IFILV(122),AVBL(122),IQ(122),IS(122),AVBLGRB2(122)      &
     &                      /1,'ACM STORM SFC RNOFF ',235,001,     &
     &                       'ACM SSRUN ON surface'/
      DATA IFILV(123),AVBL(123),IQ(123),IS(123),AVBLGRB2(123)      &
     &                      /1,'ACM BSFL-GDWR RNOFF ',234,001,     &
     &                       'ACM BGRUN ON surface'/
      DATA IFILV(160),AVBL(160),IQ(160),IS(160),AVBLGRB2(160)      &
     &                      /1,'INSTANT PRECIP TYPE ',140,001,     &
     &                       'INST CRAIN ON surface'/
      DATA IFILV(407),AVBL(407),IQ(407),IS(407),AVBLGRB2(407)      &    !407
     &                      /1,'GSD PRECIP TYPE     ',140,001,     &
     &                       'GSD INST CRAIN ON surface'/
      DATA IFILV(167),AVBL(167),IQ(167),IS(167),AVBLGRB2(167)      &
     &                      /1,'INSTANT PRECIP RATE ',059,001,     &
     &                       'INST PRATE ON surface'/
      DATA IFILV(172),AVBL(172),IQ(172),IS(172),AVBLGRB2(172)      &
     &                      /1,'FROZEN FRAC CLD SCHM',194,001,     &
     &                       'CPOFP ON surface'/
      DATA IFILV(124),AVBL(124),IQ(124),IS(124),AVBLGRB2(124)      &
     &                      /1,'CLD WTR ON MDL SFCS ',153,109,     &
     &                       'CLWMR ON hybrid_lvl'/
      DATA IFILV(125),AVBL(125),IQ(125),IS(125),AVBLGRB2(125)      &
     &                      /1,'CLD ICE ON MDL SFCS ',058,109,     &
     &                       'C_ICE ON hybrid_lvl'/
      DATA IFILV(145),AVBL(145),IQ(145),IS(145),AVBLGRB2(145)      &
     &                      /1,'CLD FRAC ON MDL SFCS',071,109,     &
     &                       'T_CDC ON hybrid_lvl'/
      DATA IFILV(037),AVBL(037),IQ(037),IS(037),AVBLGRB2(037)      &
     &                      /1,'LOW CLOUD FRACTION  ',073,214,     &
     &                       'L_CDC ON low_cloud_lyr'/
      DATA IFILV(038),AVBL(038),IQ(038),IS(038),AVBLGRB2(038)      &
     &                      /1,'MID CLOUD FRACTION  ',074,224,     &
     &                       'M_CDC ON mid_cloud_lyr'/
      DATA IFILV(039),AVBL(039),IQ(039),IS(039),AVBLGRB2(039)      &
     &                      /1,'HIGH CLOUD FRACTION ',075,234,     &
     &                       'H_CDC ON high_cloud_lyr'/
      DATA IFILV(161),AVBL(161),IQ(161),IS(161),AVBLGRB2(161)      &
     &                      /1,'TOTAL CLD FRACTION  ',071,200,     &
     &                       'INST T_CDC ON entire_atmos'/
      DATA IFILV(144),AVBL(144),IQ(144),IS(144),AVBLGRB2(144)      &
     &                      /1,'AVG TOTAL CLD FRAC  ',071,200,     &
     &                       'AVE T_CDC ON entire_atmos'/
      DATA IFILV(139),AVBL(139),IQ(139),IS(139),AVBLGRB2(139)      &
     &                      /1,'AVG STRAT CLD FRAC  ',213,200,     &
     &                       'AVE CDLYR ON entire_atmos'/
      DATA IFILV(143),AVBL(143),IQ(143),IS(143),AVBLGRB2(143)      &
     &                      /1,'AVG CNVCT CLD FRAC  ',072,200,     &
     &                       'AVE CDCON ON entire_atmos'/
      DATA IFILV(148),AVBL(148),IQ(148),IS(148),AVBLGRB2(148)      &
     &                      /1,'CLOUD BOT PRESSURE  ',001,002,     &
     &                       'PRES ON cloud_base'/
      DATA IFILV(787),AVBL(787),IQ(787),IS(787),AVBLGRB2(787)      &
     &                      /1,'GSD CLD BOT PRESSURE',001,002,     &
     &                       'GSD PRES ON cloud_base'/
      DATA IFILV(149),AVBL(149),IQ(149),IS(149),AVBLGRB2(149)      &
     &                      /1,'CLOUD TOP PRESSURE  ',001,003,     &
     &                       'PRES ON cloud_top'/
      DATA IFILV(406),AVBL(406),IQ(406),IS(406),AVBLGRB2(406)      &
     &                      /1,'GSD CLD TOP PRESSURE',001,003,     &
     &                       'PRES ON cloud_top'/                         !406
      DATA IFILV(109),AVBL(109),IQ(109),IS(109),AVBLGRB2(109)      &
     &                      /1,'LCL AGL HEIGHT      ',007,005,     &
     &                       'HGT ON lvl_of_adiab_cond_from_sfc'/
      DATA IFILV(110),AVBL(110),IQ(110),IS(110),AVBLGRB2(110)      &
     &                      /1,'LCL PRESSURE        ',001,005,     &
     &                       'PRES ON lvl_of_adiab_cond_from_sfc'/
      DATA IFILV(078),AVBL(078),IQ(078),IS(078),AVBLGRB2(078)      &
     &                      /1,'AVE GRDSCL RN TMPTDY',241,109,     &
     &                       'AVE LRGHR ON hybrid_lvl'/
      DATA IFILV(079),AVBL(079),IQ(079),IS(079),AVBLGRB2(079)      &
     &                      /1,'AVE CNVCT RN TMPTDY ',242,109,     &
     &                       'AVE CNVHR ON hybrid_lvl'/
      DATA IFILV(168),AVBL(168),IQ(168),IS(168),AVBLGRB2(168)      &
     &                      /1,'CLOUD TOP TEMPS     ',011,003,     &
     &                       'TMP ON cloud_top'/
      DATA IFILV(140),AVBL(140),IQ(140),IS(140),AVBLGRB2(140)      &
     &                      /1,'RADFLX CNVG TMP TNDY',216,109,     &
     &                       'TTRAD ON hybrid_lvl'/
      DATA IFILV(040),AVBL(040),IQ(040),IS(040),AVBLGRB2(040)      &
     &                      /1,'SW RAD TEMP TNDY    ',250,109,     &
     &                       'SWHR ON hybrid_lvl'/
      DATA IFILV(041),AVBL(041),IQ(041),IS(041),AVBLGRB2(041)      &
     &                      /1,'LW RAD TEMP TNDY    ',251,109,     &
     &                       'LWHR ON hybrid_lvl'/
      DATA IFILV(141),AVBL(141),IQ(141),IS(141),AVBLGRB2(141)      &
     &                      /1,'INSTN OUT SFC SW RAD',211,001,     &
     &                       'INST USWRF ON surface'/
      DATA IFILV(142),AVBL(142),IQ(142),IS(142),AVBLGRB2(142)      &
     &                      /1,'INSTN OUT SFC LW RAD',212,001,     &
     &                       'INST ULWRF ON surface'/
      DATA IFILV(126),AVBL(126),IQ(126),IS(126),AVBLGRB2(126)      &
     &                      /1,'AVE INCMG SFC SW RAD',204,001,     &
     &                       'AVE DSWRF ON surface'/
      DATA IFILV(127),AVBL(127),IQ(127),IS(127),AVBLGRB2(127)      &
     &                      /1,'AVE INCMG SFC LW RAD',205,001,     &
     &                       'AVE DLWRF ON surface'/
      DATA IFILV(128),AVBL(128),IQ(128),IS(128),AVBLGRB2(128)      &
     &                      /1,'AVE OUTGO SFC SW RAD',211,001,     &
     &                       'AVE USWRF ON surface'/
      DATA IFILV(129),AVBL(129),IQ(129),IS(129),AVBLGRB2(129)      &
     &                      /1,'AVE OUTGO SFC LW RAD',212,001,     &
     &                       'AVE ULWRF ON surface'/
      DATA IFILV(130),AVBL(130),IQ(130),IS(130),AVBLGRB2(130)      &
     &                      /1,'AVE OUTGO TOA SW RAD',211,008,     &
     &                       'AVE USWRF ON top_of_atmos'/
      DATA IFILV(131),AVBL(131),IQ(131),IS(131),AVBLGRB2(131)      &
     &                      /1,'AVE OUTGO TOA LW RAD',212,008,     &
     &                       'AVE ULWRF ON top_of_atmos'/
      DATA IFILV(156),AVBL(156),IQ(156),IS(156),AVBLGRB2(156)      &
     &                      /1,'INSTN INC SFC SW RAD',204,001,     &
     &                       'INST DSWRF ON surface'/
      DATA IFILV(157),AVBL(157),IQ(157),IS(157),AVBLGRB2(157)      &
     &                      /1,'INSTN INC SFC LW RAD',205,001,     &
     &                       'INST DLWRF ON surface'/
      DATA IFILV(044),AVBL(044),IQ(044),IS(044),AVBLGRB2(044)      &
     &                      /1,'ROUGHNESS LENGTH    ',083,001,     &
     &                       'SFC_R ON surface'/
      DATA IFILV(045),AVBL(045),IQ(045),IS(045),AVBLGRB2(045)      &
     &                      /1,'FRICTION VELOCITY   ',253,001,     &
     &                       'FRICV ON surface'/
      DATA IFILV(132),AVBL(132),IQ(132),IS(132),AVBLGRB2(132)      &
     &                      /1,'SFC DRAG COEFFICIENT',252,001,     &
     &                       'CD ON surface'/
      DATA IFILV(133),AVBL(133),IQ(133),IS(133),AVBLGRB2(133)      &
     &                      /1,'SFC U WIND STRESS   ',124,001,     &
     &                       'U_FLX ON surface'/
      DATA IFILV(134),AVBL(134),IQ(134),IS(134),AVBLGRB2(134)      &
     &                      /1,'SFC V WIND STRESS   ',125,001,     &
     &                       'V_FLX ON surface'/
      DATA IFILV(043),AVBL(043),IQ(043),IS(043),AVBLGRB2(043)      &
     &                      /1,'AVE SFC SENHEAT FX  ',122,001,     &
     &                       'AVE SHTFL ON surface'/
      DATA IFILV(135),AVBL(135),IQ(135),IS(135),AVBLGRB2(135)      &
     &                      /1,'AVE GROUND HEAT FX  ',155,001,     &
     &                       'AVE GFLUX ON surface'/
      DATA IFILV(136),AVBL(136),IQ(136),IS(136),AVBLGRB2(136)      &
     &                      /1,'AVE SNO PHSCNG HT FX',229,001,     &
     &                       'AVE SNOHF ON surface'/
      DATA IFILV(042),AVBL(042),IQ(042),IS(042),AVBLGRB2(042)      &
     &                      /1,'AVE SFC LATHEAT FX  ',121,001,     &
     &                       'AVE LHTFL ON surface'/
      DATA IFILV(046),AVBL(046),IQ(046),IS(046),AVBLGRB2(046)      &
     &                      /1,'AVE SFC MOMENTUM FX ',172,001,     &
     &                       'AVE M_FLX ON surface'/
      DATA IFILV(047),AVBL(047),IQ(047),IS(047),AVBLGRB2(047)      &
     &                      /1,'ACC SFC EVAPORATION ',057,001,     &
     &                       'ACM EVP ON surface'/
      DATA IFILV(137),AVBL(137),IQ(137),IS(137),AVBLGRB2(137)      &
     &                      /1,'ACC POT EVAPORATION ',228,001,     &
     &                       'ACM PEVAP ON surface'/
      DATA IFILV(154),AVBL(154),IQ(154),IS(154),AVBLGRB2(154)      &
     &                      /1,'INST SFC SENHEAT FX ',122,001,     &
     &                       'INST SHTFL ON surface'/
      DATA IFILV(155),AVBL(155),IQ(155),IS(155),AVBLGRB2(155)      &
     &                      /1,'INST SFC LATHEAT FX ',121,001,     &
     &                       'INST LHTFL ON surface'/
      DATA IFILV(048),AVBL(048),IQ(048),IS(048),AVBLGRB2(048)      &
     &                      /1,'LATITUDE            ',176,001,     &
     &                       'NLAT ON surface'/
      DATA IFILV(049),AVBL(049),IQ(049),IS(049),AVBLGRB2(049)      &
     &                      /1,'LONGITUDE           ',177,001,     &
     &                       'ELON ON surface'/
      DATA IFILV(050),AVBL(050),IQ(050),IS(050),AVBLGRB2(050)      &
     &                      /1,'LAND/SEA MASK       ',081,001,     &
     &                       'LAND ON surface'/
      DATA IFILV(051),AVBL(051),IQ(051),IS(051),AVBLGRB2(051)      &
     &                      /1,'SEA ICE MASK        ',091,001,     &
     &                       'ICE_C ON surface'/
      DATA IFILV(052),AVBL(052),IQ(052),IS(052),AVBLGRB2(052)      &
     &                      /1,'MASS POINT MDL SFC  ',173,001,     &
     &                       'LMH ON surface'/
      DATA IFILV(053),AVBL(053),IQ(053),IS(053),AVBLGRB2(053)      &
     &                      /1,'VEL POINT MDL SFC   ',174,001,     &
     &                       'LMV ON surface'/
      DATA IFILV(150),AVBL(150),IQ(150),IS(150),AVBLGRB2(150)      &
     &                      /1,'SFC MIDDAY ALBEDO   ',084,001,     &
     &                       'ALBDO ON surface'/
      DATA IFILV(151),AVBL(151),IQ(151),IS(151),AVBLGRB2(151)      &
     &                      /1,'SEA SFC TEMPERATURE ',080,001,     &
     &                       'WTMP ON surface'/
      DATA IFILV(054),AVBL(054),IQ(054),IS(054),AVBLGRB2(054)      &
     &                      /1,'PRESS AT TROPOPAUSE ',001,007,     &
     &                       'PRES ON tropopause'/
      DATA IFILV(055),AVBL(055),IQ(055),IS(055),AVBLGRB2(055)      &
     &                      /1,'TEMP AT TROPOPAUSE  ',011,007,     &
     &                       'TMP ON tropopause'/
      DATA IFILV(108),AVBL(108),IQ(108),IS(108),AVBLGRB2(108)      &
     &                      /1,'POTENTL TEMP AT TROP',013,007,     &
     &                       'POT ON tropopause'/
      DATA IFILV(056),AVBL(056),IQ(056),IS(056),AVBLGRB2(056)      &
     &                      /1,'U WIND AT TROPOPAUSE',033,007,     &
     &                       'U_GRD ON tropopause'/
      DATA IFILV(057),AVBL(057),IQ(057),IS(057),AVBLGRB2(057)      &
     &                      /1,'V WIND AT TROPOPAUSE',034,007,     &
     &                       'V_GRD ON tropopause'/
      DATA IFILV(058),AVBL(058),IQ(058),IS(058),AVBLGRB2(058)      &
     &                      /1,'SHEAR AT TROPOPAUSE ',136,007,     &
     &                       'VW_SH ON tropopause'/
      DATA IFILV(059),AVBL(059),IQ(059),IS(059),AVBLGRB2(059)      &
     &                      /1,'TEMP AT FD HEIGHTS  ',011,103,     &
     &                       'TMP ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(060),AVBL(060),IQ(060),IS(060),AVBLGRB2(060)      &
     &                      /1,'U WIND AT FD HEIGHTS',033,103,     &
     &                      'U_GRD ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(061),AVBL(061),IQ(061),IS(061),AVBLGRB2(061)      &
     &                      /1,'V WIND AT FD HEIGHTS',034,103,     &
     &                      'V_GRD ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(062),AVBL(062),IQ(062),IS(062),AVBLGRB2(062)      &
     &                      /1,'HEIGHT OF FRZ LVL   ',007,004,     &
     &                       'HGT ON 0C_isotherm'/
      DATA IFILV(063),AVBL(063),IQ(063),IS(063),AVBLGRB2(063)      &
     &                      /1,'REL HUMID AT FRZ LVL',052,004,     &
     &                       'RH ON 0C_isotherm'/
      DATA IFILV(165),AVBL(165),IQ(165),IS(165),AVBLGRB2(165)      &
     &                      /1,'HIGHEST FREEZE LVL  ',007,204,     &
     &                       'HGT ON hghst_trop_frz_lvl'/
      DATA IFILV(350),AVBL(350),IQ(350),IS(350),AVBLGRB2(350)      &
     &                      /1,'HIGHEST FRZ LVL RH  ',052,204,     &
     &                       'RH ON hghst_trop_frz_lvl'/
      DATA IFILV(067),AVBL(067),IQ(067),IS(067),AVBLGRB2(067)      &
     &                      /1,'PRESS IN BNDRY LYR  ',001,116,     &
     &                       'PRES ON spec_pres_above_grnd'/
      DATA IFILV(068),AVBL(068),IQ(068),IS(068),AVBLGRB2(068)      &
     &                      /1,'TEMP IN BNDRY LYR   ',011,116,     &
     &                       'TMP ON spec_pres_above_grnd'/
      DATA IFILV(069),AVBL(069),IQ(069),IS(069),AVBLGRB2(069)      &
     &                      /1,'POT TMP IN BNDRY LYR',013,116,     &
     &                       'POT ON spec_pres_above_grnd'/
      DATA IFILV(070),AVBL(070),IQ(070),IS(070),AVBLGRB2(070)      &
     &                      /1,'DWPT IN BNDRY LYR   ',017,116,     &
     &                       'DPT ON spec_pres_above_grnd'/
      DATA IFILV(071),AVBL(071),IQ(071),IS(071),AVBLGRB2(071)      &
     &                      /1,'SPC HUM IN BNDRY LYR',051,116,     &
     &                       'SPF_H ON spec_pres_above_grnd'/
      DATA IFILV(072),AVBL(072),IQ(072),IS(072),AVBLGRB2(072)      &
     &                      /1,'REL HUM IN BNDRY LYR',052,116,     &
     &                       'RH ON spec_pres_above_grnd'/
      DATA IFILV(088),AVBL(088),IQ(088),IS(088),AVBLGRB2(088)      &
     &                      /1,'MST CNV IN BNDRY LYR',135,116,     &
     &                       'MCONV ON spec_pres_above_grnd'/
      DATA IFILV(089),AVBL(089),IQ(089),IS(089),AVBLGRB2(089)      &
     &                      /1,'P WATER IN BNDRY LYR',054,116,     &
     &                       'PWAT ON spec_pres_above_grnd'/
      DATA IFILV(073),AVBL(073),IQ(073),IS(073),AVBLGRB2(073)      &
     &                      /1,'U WIND IN BNDRY LYR ',033,116,     &
     &                       'U_GRD ON spec_pres_above_grnd'/
      DATA IFILV(074),AVBL(074),IQ(074),IS(074),AVBLGRB2(074)      &
     &                      /1,'V WIND IN BNDRY LYR ',034,116,     &
     &                       'V_GRD ON spec_pres_above_grnd'/
      DATA IFILV(090),AVBL(090),IQ(090),IS(090),AVBLGRB2(090)      &
     &                      /1,'OMEGA IN BNDRY LYR  ',039,116,     &
     &                       'V_VEL ON spec_pres_above_grnd'/
      DATA IFILV(066),AVBL(066),IQ(066),IS(066),AVBLGRB2(066)      &
     &                      /1,'LFM 0.33-1.00 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(081),AVBL(081),IQ(081),IS(081),AVBLGRB2(081)      &
     &                      /1,'LFM 0.66-1.00 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(082),AVBL(082),IQ(082),IS(082),AVBLGRB2(082)      &
     &                      /1,'LFM 0.33-0.66 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(104),AVBL(104),IQ(104),IS(104),AVBLGRB2(104)      &
     &                      /1,'LFM 0.33-1.00 PWAT  ',054,108,     &
     &                       'PWAT ON sigma_lvl'/
      DATA IFILV(091),AVBL(091),IQ(091),IS(091),AVBLGRB2(091)      &
     &                      /1,'NGM 0.98230 PRESSURE',001,107,     &
     &                       'PRES ON sigma_lvl'/
      DATA IFILV(092),AVBL(092),IQ(092),IS(092),AVBLGRB2(092)      &
     &                      /1,'NGM 0.98230 TMPRATUR',011,107,     &
     &                       'TMP ON sigma_lvl'/
      DATA IFILV(093),AVBL(093),IQ(093),IS(093),AVBLGRB2(093)      &
     &                      /1,'NGM 0.98230 SPC HUM ',051,107,     &
     &                       'SPF_H ON sigma_lvl'/
      DATA IFILV(094),AVBL(094),IQ(094),IS(094),AVBLGRB2(094)      &
     &                      /1,'NGM 0.98230 REL HUM ',052,107,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(095),AVBL(095),IQ(095),IS(095),AVBLGRB2(095)      &
     &                      /1,'NGM 0.98230 U WIND  ',033,107,     &
     &                       'U_GRD ON sigma_lvl'/
      DATA IFILV(096),AVBL(096),IQ(096),IS(096),AVBLGRB2(096)      &
     &                      /1,'NGM 0.98230 V WIND  ',034,107,     &
     &                       'V_GRD ON sigma_lvl'/
      DATA IFILV(097),AVBL(097),IQ(097),IS(097),AVBLGRB2(097)      &
     &                      /1,'NGM 0.89671 TMPRATUR',011,107,     &
     &                       'TMP ON sigma_lvl'/
      DATA IFILV(098),AVBL(098),IQ(098),IS(098),AVBLGRB2(098)      &
     &                      /1,'NGM 0.78483 TMPRATUR',011,107,     &
     &                       'TMP ON sigma_lvl'/
      DATA IFILV(099),AVBL(099),IQ(099),IS(099),AVBLGRB2(099)      &
     &                      /1,'NGM 0.47-1.00 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(100),AVBL(100),IQ(100),IS(100),AVBLGRB2(100)      &
     &                      /1,'NGM 0.47-0.96 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(101),AVBL(101),IQ(101),IS(101),AVBLGRB2(101)      &
     &                      /1,'NGM 0.18-0.47 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(102),AVBL(102),IQ(102),IS(102),AVBLGRB2(102)      &
     &                      /1,'NGM 0.84-0.98 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(103),AVBL(103),IQ(103),IS(103),AVBLGRB2(103)      &
     &                      /1,'NGM 0.85-1.00 QCONVG',135,108,     &
     &                       'MCONV ON sigma_lvl'/
      DATA IFILV(173),AVBL(173),IQ(173),IS(173),AVBLGRB2(173)      &
     &                      /1,'MAX WIND PRESS LEVEL',001,006,     &
     &                       'PRES ON max_wind'/
      DATA IFILV(174),AVBL(174),IQ(174),IS(174),AVBLGRB2(174)      &
     &                      /1,'MAX WIND HGHT LEVEL ',007,006,     &
     &                       'HGT ON max_wind'/
      DATA IFILV(175),AVBL(175),IQ(175),IS(175),AVBLGRB2(175)      &
     &                      /1,'U COMP MAX WIND     ',033,006,     &
     &                       'U_GRD ON max_wind'/
      DATA IFILV(176),AVBL(176),IQ(176),IS(176),AVBLGRB2(176)      &
     &                      /1,'V COMP MAX WIND     ',034,006,     &
     &                       'V_GRD ON max_wind'/
      DATA IFILV(177),AVBL(177),IQ(177),IS(177),AVBLGRB2(177)      &
     &                      /1,'HEIGHT AT TROPOPAUSE',007,007,     &
     &                       'HGT ON tropopause'/
      DATA IFILV(178),AVBL(178),IQ(178),IS(178),AVBLGRB2(178)      &
     &                      /1,'CLOUD BOTTOM HEIGHT ',007,002,     &
     &                       'HGT ON cloud_base'/
      DATA IFILV(179),AVBL(179),IQ(179),IS(179),AVBLGRB2(179)      &
     &                      /1,'CLOUD TOP HEIGHT    ',007,003,     &
     &                       'HGT ON cloud_top'/
      DATA IFILV(180),AVBL(180),IQ(180),IS(180),AVBLGRB2(180)      &
     &                      /1,'VISIBILITY          ',020,001,     &
     &                       'VIS ON surface'/
      DATA IFILV(408),AVBL(408),IQ(408),IS(408),AVBLGRB2(408)      &
     &                      /1,'GSD CLD BOT HEIGHT  ',007,002,     &
     &                       'GSD HGT ON cloud_base'/                     !408
      DATA IFILV(409),AVBL(409),IQ(409),IS(409),AVBLGRB2(409)      &
     &                      /1,'GSD CLD TOP HEIGHT  ',007,003,     &
     &                       'GSD HGT ON cloud_top'/                      !408
      DATA IFILV(410),AVBL(410),IQ(410),IS(410),AVBLGRB2(410)      &
     &                      /1,'GSD VISIBILITY      ',020,003,     &
     &                       'GSD VIS ON cloud top'/                      !410
! CRA
      DATA IFILV(411),AVBL(411),IQ(411),IS(411),AVBLGRB2(411)      &
     &                      /1,'INSTN WIND POWER AGL',126,105,     &
     &                       'INST WMIXE ON spec_hgt_lvl_above_grnd'/     !411
      DATA IFILV(412),AVBL(412),IQ(412),IS(412),AVBLGRB2(412)      &
     &                      /1,'U WIND AT 80M AGL   ',049,105,     &
     &                       'UGRD ON 80M spec_hgt_lvl_above_grnd'/       !412
      DATA IFILV(413),AVBL(413),IQ(413),IS(413),AVBLGRB2(413)      &
     &                      /1,'V WIND AT 80M AGL   ',050,105,     &
     &                      'VGRD ON 80M spec_hgt_lvl_above_grnd'/        !413
!
      DATA IFILV(181),AVBL(181),IQ(181),IS(181),AVBLGRB2(181)      &
     &                      /1,'RAIN ON MDL SFCS    ',170,109,     &
     &                       'RWMR ON hybrid_lvl'/
      DATA IFILV(182),AVBL(182),IQ(182),IS(182),AVBLGRB2(182)      &
     &                      /1,'SNOW ON MDL SFCS    ',171,109,     &
     &                       'SNMR ON hybrid_lvl'/
      DATA IFILV(183),AVBL(183),IQ(183),IS(183),AVBLGRB2(183)      &
     &                      /1,'RAIN ON P SFCS      ',170,100,     &
     &                       'RWMR ON isobaric_sfc'/
      DATA IFILV(184),AVBL(184),IQ(184),IS(184),AVBLGRB2(184)      &
     &                      /1,'SNOW ON P SFCS      ',171,100,     &
     &                       'SNMR ON isobaric_sfc'/
      DATA IFILV(415),AVBL(415),IQ(415),IS(415),AVBLGRB2(415)      &
     &                      /1,'GRAUPEL ON MDL SFCS ',179,109,     &
     &                       'GRMR ON hybrid_lvl'/                         !415
      DATA IFILV(416),AVBL(416),IQ(416),IS(416),AVBLGRB2(416)      &
     &                      /1,'GRAUPEL ON P SFCS   ',179,100,     &
     &                       'GRMR ON isobaric_sfc'/                       !416

! SRD
      DATA IFILV(420),AVBL(420),IQ(420),IS(420),AVBLGRB2(420)      &
     &                      /1,'MAX UPDRAFT HELICITY',236,106,     &
     &                       'MAX UPHL ON spec_hgt_lvl_above_grnd'/        !420
      DATA IFILV(421),AVBL(421),IQ(421),IS(421),AVBLGRB2(421)      &
     &                      /1,'MAX 1km REFLECTIVITY',235,105,     &
     &                       'MAX REF ON 1000M spec_hgt_lvl_above_grnd'/   !421
      DATA IFILV(422),AVBL(422),IQ(422),IS(422),AVBLGRB2(422)      &
     &                      /1,'MAX 10m WIND SPEED  ',229,105,     &
     &                       'MAX WIND ON 10M spec_hgt_lvl_above_grnd'/    !422
      DATA IFILV(423),AVBL(423),IQ(423),IS(423),AVBLGRB2(423)      &
     &                      /1,'MAX UPDRAFT VERT VEL',237,106,     &
     &                       'MAX UPDZDT ON spec_hgt_lvl_above_grnd'/      !423
      DATA IFILV(424),AVBL(424),IQ(424),IS(424),AVBLGRB2(424)      &
     &                      /1,'MAX DNDRAFT VERT VEL',238,106,     &
     &                       'MAX DNDZDT ON spec_hgt_lvl_above_grnd'/     !424
      DATA IFILV(425),AVBL(425),IQ(425),IS(425),AVBLGRB2(425)      &
     &                      /1,'MEAN VERT VEL       ',040,108,     &
     &                       'AVE DZDT ON spec_hgt_lvl_above_grnd'/        !425
      DATA IFILV(426),AVBL(426),IQ(426),IS(426),AVBLGRB2(426)      &
     &                      /1,'ECHO TOPS IN KFT    ',007,105,     &
     &                       'HGT ON spec_hgt_lvl_above_grnd'/             !426
      DATA IFILV(427),AVBL(427),IQ(427),IS(427),AVBLGRB2(427)      &
     &                      /1,'UPDRAFT HELICITY PRM',227,106,     &
     &                       'UPHL ON spec_hgt_lvl_above_grnd'/            !427
      DATA IFILV(428),AVBL(428),IQ(428),IS(428),AVBLGRB2(428)      &
     &                      /1,'VERT INTEG GRAUP    ',179,200,     &
     &                       'GRMR ON entire_atmos_single_lyr'/            !428
      DATA IFILV(429),AVBL(429),IQ(429),IS(429),AVBLGRB2(429)      &
     &                      /1,'MAX VERT INTEG GRAUP',239,200,     &
     &                       'MAXVIG ON entire_atmos_single_lyr'/          !429
! SRD
! CRA
      DATA IFILV(430),AVBL(430),IQ(430),IS(430),AVBLGRB2(430)      &
     &                      /1,'U COMP 0-1 KM SHEAR ',230,106,     &
     &                       'UUCSH ON spec_hgt_lvl_above_grnd'/          !430
      DATA IFILV(431),AVBL(431),IQ(431),IS(431),AVBLGRB2(431)      &
     &                      /1,'V COMP 0-1 KM SHEAR ',238,106,     &
     &                       'VVCSH ON spec_hgt_lvl_above_grnd'/          !431
      DATA IFILV(432),AVBL(432),IQ(432),IS(432),AVBLGRB2(432)      &
     &                      /1,'U COMP 0-6 KM SHEAR ',239,106,     &
     &                       'UUCSH ON spec_hgt_lvl_above_grnd'/          !432
      DATA IFILV(433),AVBL(433),IQ(433),IS(433),AVBLGRB2(433)      &
     &                      /1,'V COMP 0-6 KM SHEAR ',241,106,     &
     &                       'VVCSH ON spec_hgt_lvl_above_grnd'/          !433
! CRA

! Add precipitation buckets between outputs
      DATA IFILV(434),AVBL(434),IQ(434),IS(434),AVBLGRB2(434)      &
     &                      /1,'BUCKET TOTAL PRECIP ',061,001,     &
     &                       'A_PCP ON surface'/                         !434
      DATA IFILV(435),AVBL(435),IQ(435),IS(435),AVBLGRB2(435)      &
     &                      /1,'BUCKET CONV PRECIP  ',063,001,     &
     &                       'ACPCP ON surface'/                         !435
      DATA IFILV(436),AVBL(436),IQ(436),IS(436),AVBLGRB2(436)      &
     &                      /1,'BUCKET GRDSCALE PRCP',062,001,     &
     &                       'NCPCP ON surface'/                         !436
      DATA IFILV(437),AVBL(437),IQ(437),IS(437),AVBLGRB2(437)      &
     &                      /1,'BUCKET SNOW  PRECIP ',065,001,     &
     &                       'WEASD ON surface'/                         !437
      DATA IFILV(487),AVBL(487),IQ(487),IS(487),AVBLGRB2(487)      &
     &                      /1,'GSD CEILING         ',008,002,     &
     &                       'HGT ON cloud_ceiling'/                     !487
!JSK - add model-state cloud fraction; not the same as field 145 ("TCDC")
      DATA IFILV(774),AVBL(774),IQ(774),IS(774),AVBLGRB2(774)      &
     &                      /1,'RAW CLD FRA MDL SFCS',071,109,     &
     &                       'FRACCC ON hybrid_lvl'/
!tgs - need to find the correct Grid table number for 775 field
      DATA IFILV(775),AVBL(775),IQ(775),IS(775),AVBLGRB2(775)      &     
     &                      /1,'BUCKET GRAUP PRECIP ',179,001,     &     !775
     &                       'GRAUP ON surface'/  
!CRA - -10C and -20C isothermal heigths, RH, pressure
      DATA IFILV(776),AVBL(776),IQ(776),IS(776),AVBLGRB2(776)      &    
     &                      /1,'HIGHEST -10C LVL    ',007,020,     &    
     &                       'HGT ON -10C_isotherm'/                     !776
      DATA IFILV(777),AVBL(777),IQ(777),IS(777),AVBLGRB2(777)      &    
     &                      /1,'HIGHEST -10C RH     ',052,020,     &    
     &                       'RH ON -10C_isotherm'/                      !777
      DATA IFILV(778),AVBL(778),IQ(778),IS(778),AVBLGRB2(778)      &    
     &                      /1,'HIGHEST -10C PRES   ',001,020,     &    
     &                       'PRES ON -10C_isotherm'/                    !778
      DATA IFILV(779),AVBL(779),IQ(779),IS(779),AVBLGRB2(779)      &    
     &                      /1,'HIGHEST -20C LVL    ',007,020,     &    
     &                       'HGT ON -20C_isotherm'/                     !779
      DATA IFILV(780),AVBL(780),IQ(780),IS(780),AVBLGRB2(780)      &    
     &                      /1,'HIGHEST -20C RH     ',052,020,     &    
     &                       'RH ON -20C_isotherm'/                      !780
      DATA IFILV(781),AVBL(781),IQ(781),IS(781),AVBLGRB2(781)      &    
     &                      /1,'HIGHEST -20C PRES   ',001,020,     &    
     &                       'PRES ON -20C_isotherm'/                    !781
      DATA IFILV(782),AVBL(782),IQ(782),IS(782),AVBLGRB2(782)      &    
     &                      /1,'ACM FRAIN           ',193,001,     &    
     &                       'ACM FRAIN ON surface'/                     !782
! CRA
!
!--- Added new cloud microphysics fields & displaying more
!    convective cloud properties  (Jin, '01;  Ferrier, Feb '02)     
!
!
!--- The following fields have been added to the post under
!    PDS Octet 4 = 129.  All other fields above are with PDS Octet
!    4 = 2.  Most of the fields below, except for the cloud top
!    and cloud base pressures, have PDS Octet 4 = 129.  These new
!    grib parameters are listed in Table 129 of the GRIB documentation.
!    See Table 2 in Office Note 388 (ON388) for more details.
!
!--- F_rain, F_ice, F_RimeF => PDS Octet 4 = 129
!
      DATA IFILV(185),AVBL(185),IQ(185),IS(185),AVBLGRB2(185)      &
     &                      /1,'F_rain ON MDL SFCS  ',131,109,     &
     &                       'FRAIN ON hybrid_lvl'/
      DATA IFILV(186),AVBL(186),IQ(186),IS(186),AVBLGRB2(186)      &
     &                      /1,'F_ice ON MDL SFCS   ',132,109,     &
     &                       'FICE ON hybrid_lvl'/
      DATA IFILV(187),AVBL(187),IQ(187),IS(187),AVBLGRB2(187)      &
     &                      /1,'F_RimeF ON MDL SFCS ',133,109,     &
     &                       'RIME ON hybrid_lvl'/
!
!--- The following cloud pressure fields have PDS Octet 4 = 2
!
      DATA IFILV(188),AVBL(188),IQ(188),IS(188),AVBLGRB2(188)      &
     &                      /1,'CONV CLOUD BOT PRESS',001,242,     &
     &                       'PRES ON convective_cloud_bot_lvl'/
      DATA IFILV(189),AVBL(189),IQ(189),IS(189),AVBLGRB2(189)      &
     &                      /1,'CONV CLOUD TOP PRESS',001,243,     &
     &                       'PRES ON convective_cloud_top_lvl'/
      DATA IFILV(190),AVBL(190),IQ(190),IS(190),AVBLGRB2(190)      &
     &                      /1,'SHAL CU CLD BOT PRES',001,248,     &
     &                       'PRES ON shall_convective_cloud_bot_lvl'/
      DATA IFILV(191),AVBL(191),IQ(191),IS(191),AVBLGRB2(191)      &
     &                      /1,'SHAL CU CLD TOP PRES',001,249,     &
     &                       'PRES ON shall_convective_cloud_top_lvl'/
      DATA IFILV(192),AVBL(192),IQ(192),IS(192),AVBLGRB2(192)      &
     &                      /1,'DEEP CU CLD BOT PRES',001,251,     &
     &                       'PRES ON deep_convective_cloud_bot_lvl'/
      DATA IFILV(193),AVBL(193),IQ(193),IS(193),AVBLGRB2(193)      &
     &                      /1,'DEEP CU CLD TOP PRES',001,252,     &
     &                       'PRES ON deep_convective_cloud_top_lvl'/
      DATA IFILV(194),AVBL(194),IQ(194),IS(194),AVBLGRB2(194)      &
     &                      /1,'GRID CLOUD BOT PRESS',001,206,     &
     &                       'PRES ON grid_scale_cloud_bot_lvl'/
      DATA IFILV(195),AVBL(195),IQ(195),IS(195),AVBLGRB2(195)      &
     &                      /1,'GRID CLOUD TOP PRESS',001,207,     &
     &                       'PRES ON grid_scale_cloud_top_lvl'/
      DATA IFILV(196),AVBL(196),IQ(196),IS(196),AVBLGRB2(196)      &
     &                      /1,'CONV CLOUD FRACTION ',072,200,     &
     &                       'T_CDC ON entire_atmos_single_lyr'/
!     DATA IFILV(196),AVBL(196),IQ(196),IS(196),AVBLGRB2(196)      &
!    &                      /1,'CONV CLOUD FRACTION ',072,200,     &
!    &                       'T_CDC ON convective_cloud_lyr'/
!
!--- These remaining fields have PDS Octet 4 = 129 (Table 129, ON388)      
!
      DATA IFILV(197),AVBL(197),IQ(197),IS(197),AVBLGRB2(197)      &
     &                      /1,'CU CLOUD EFFICIENCY ',134,200,     &
     &                       'CUEFI ON entire_atmos_single_lyr'/
      DATA IFILV(198),AVBL(198),IQ(198),IS(198),AVBLGRB2(198)      &
     &                      /1,'CONDENSATE ON P SFCS',135,100,     &
     &                       'TCOND ON isobaric_sfc'/
      DATA IFILV(199),AVBL(199),IQ(199),IS(199),AVBLGRB2(199)      &
     &                      /1,'CONDENSATE MDL SFCS ',135,109,     &
     &                       'TCOND ON hybrid_lvl'/
      DATA IFILV(200),AVBL(200),IQ(200),IS(200),AVBLGRB2(200)      &
     &                      /1,'TOTAL COLUMN CLD WTR',136,200,     &
     &                       'TCOLW ON entire_atmos_single_lyr'/
      DATA IFILV(201),AVBL(201),IQ(201),IS(201),AVBLGRB2(201)      &
     &                      /1,'TOTAL COLUMN CLD ICE',137,200,     &
     &                       'TCOLI ON entire_atmos_single_lyr'/
      DATA IFILV(202),AVBL(202),IQ(202),IS(202),AVBLGRB2(202)      &
     &                      /1,'TOTAL COLUMN RAIN   ',138,200,     &
     &                       'TCOLR ON entire_atmos_single_lyr'/
      DATA IFILV(203),AVBL(203),IQ(203),IS(203),AVBLGRB2(203)      &
     &                      /1,'TOTAL COLUMN SNOW   ',139,200,     &
     &                       'TCOLS ON entire_atmos_single_lyr'/
      DATA IFILV(204),AVBL(204),IQ(204),IS(204),AVBLGRB2(204)      &
     &                      /1,'TOTAL COL CONDENSATE',140,200,     &
     &                       'TCOLC ON entire_atmos_single_lyr'/
! See below for total supercooled liquid & melting ice ... IFILV(285)     
! H CHUANG--ADD INTERPOLATED FIELDS ON SIGMA LEVELS
      DATA IFILV(205),AVBL(205),IQ(205),IS(205),AVBLGRB2(205)      &
     &                      /1,'HEIGHT OF SIGMA SFCS',007,107,     &
     &                       'HGT ON sigma_lvl'/
      DATA IFILV(206),AVBL(206),IQ(206),IS(206),AVBLGRB2(206)      &
     &                      /1,'TEMP ON SIGMA SFCS  ',011,107,     &
     &                       'TMP ON sigma_lvl'/
      DATA IFILV(207),AVBL(207),IQ(207),IS(207),AVBLGRB2(207)      &
     &                      /1,'SPEC HUM ON S SFCS  ',051,107,     &
     &                       'SPF_H ON sigma_lvl'/
      DATA IFILV(208),AVBL(208),IQ(208),IS(208),AVBLGRB2(208)      &
     &                      /0,'U WIND ON SIGMA SFCS',033,107,     &
     &                       'U_GRD ON sigma_lvl'/
      DATA IFILV(209),AVBL(209),IQ(209),IS(209),AVBLGRB2(209)      &
     &                      /0,'V WIND ON SIGMA SFCS',034,107,     &
     &                       'V_GRD ON sigma_lvl'/
      DATA IFILV(210),AVBL(210),IQ(210),IS(210),AVBLGRB2(210)      &
     &                      /1,'OMEGA ON SIGMA SFCS ',039,107,     &
     &                       'V_VEL ON sigma_lvl'/
      DATA IFILV(211),AVBL(211),IQ(211),IS(211),AVBLGRB2(211)      &
     &                      /1,'CLOUD WATR ON S SFCS',153,107,     &
     &                       'CLWMR ON sigma_lvl'/
      DATA IFILV(212),AVBL(212),IQ(212),IS(212),AVBLGRB2(212)      &
     &                      /1,'CLOUD ICE ON S SFCS ',058,107,     &
     &                       'C_ICE ON sigma_lvl'/
      DATA IFILV(213),AVBL(213),IQ(213),IS(213),AVBLGRB2(213)      &
     &                      /1,'RAIN ON S SFCS      ',170,107,     &
     &                       'RWMR ON sigma_lvl'/
      DATA IFILV(214),AVBL(214),IQ(214),IS(214),AVBLGRB2(214)      &
     &                      /1,'SNOW ON S SFCS      ',171,107,     &
     &                       'SNMR ON sigma_lvl'/
      DATA IFILV(215),AVBL(215),IQ(215),IS(215),AVBLGRB2(215)      &
     &                      /1,'CONDENSATE ON S SFCS',135,107,     &
     &                       'TCOND ON sigma_lvl'/
      DATA IFILV(216),AVBL(216),IQ(216),IS(216),AVBLGRB2(216)      &
     &                      /1,'PRESS ON SIG SFCS   ',001,107,     &
     &                       'PRES ON sigma_lvl'/
      DATA IFILV(217),AVBL(217),IQ(217),IS(217),AVBLGRB2(217)      &
     &                      /1,'TRBLNT KE ON S SFCS ',158,107,     &
     &                       'TKE ON sigma_lvl'/
      DATA IFILV(222),AVBL(222),IQ(222),IS(222),AVBLGRB2(222)      &
     &                      /1,'CLD FRAC ON SIG SFCS',071,107,     &
     &                       'T_CDC ON sigma_lvl'/
      DATA IFILV(255),AVBL(255),IQ(255),IS(255),AVBLGRB2(255)      &       !255
     &                      /1,'GRAUPEL ON S SFCS   ',179,107,     &
     &                       'GRLE ON sigma_lvl'/
! H CHUANG--ADD FIXED AND LSM FIELDS
      DATA IFILV(218),AVBL(218),IQ(218),IS(218),AVBLGRB2(218)      &
     &                      /1,'VEGETATION TYPE     ',225,001,     &
     &                       'VGTYP ON surface'/
      DATA IFILV(219),AVBL(219),IQ(219),IS(219),AVBLGRB2(219)      &
     &                      /1,'SOIL TYPE           ',224,001,     &
     &                       'SOTYP ON surface'/
      DATA IFILV(220),AVBL(220),IQ(220),IS(220),AVBLGRB2(220)      &
     &                      /1,'CANOPY CONDUCTANCE  ',181,001,     &
     &                       'CCOND ON surface'/
      DATA IFILV(221),AVBL(221),IQ(221),IS(221),AVBLGRB2(221)      &
     &                      /1,'PBL HEIGHT          ',221,001,     &
     &                       'HPBL ON surface'/
      DATA IFILV(223),AVBL(223),IQ(223),IS(223),AVBLGRB2(223)      &
     &                      /1,'SLOPE TYPE          ',222,001,     &
     &                       'SLTYP ON surface'/
      DATA IFILV(224),AVBL(224),IQ(224),IS(224),AVBLGRB2(224)      &
     &                      /1,'SNOW DEPTH          ',066,001,     &
     &                       'SNO_D ON surface'/
      DATA IFILV(225),AVBL(225),IQ(225),IS(225),AVBLGRB2(225)      &
     &                      /1,'LIQUID SOIL MOISTURE',160,112,     &
     &                       'SOILL ON depth_bel_land_sfc'/
      DATA IFILV(226),AVBL(226),IQ(226),IS(226),AVBLGRB2(226)      &
     &                      /1,'SNOW FREE ALBEDO    ',170,001,     &
     &                       '/SNFALB ON surface'/
      DATA IFILV(227),AVBL(227),IQ(227),IS(227),AVBLGRB2(227)      &
     &                      /1,'MAXIMUM SNOW ALBEDO ',159,001,     &
     &                       'MXSALB ON surface'/
      DATA IFILV(228),AVBL(228),IQ(228),IS(228),AVBLGRB2(228)      &
     &                      /1,'CANOPY WATER EVAP   ',200,001,     &
     &                       'EVCW ON surface'/
      DATA IFILV(229),AVBL(229),IQ(229),IS(229),AVBLGRB2(229)      &
     &                      /1,'DIRECT SOIL EVAP    ',199,001,     &
     &                       'EVBS ON surface'/
      DATA IFILV(230),AVBL(230),IQ(230),IS(230),AVBLGRB2(230)      &
     &                      /1,'PLANT TRANSPIRATION ',210,001,     &
     &                       'TRANS ON surface'/
      DATA IFILV(231),AVBL(231),IQ(231),IS(231),AVBLGRB2(231)      &
     &                      /1,'SNOW SUBLIMATION    ',198,001,     &
     &                       'SBSNO ON surface'/
      DATA IFILV(232),AVBL(232),IQ(232),IS(232),AVBLGRB2(232)      &
     &                      /1,'AIR DRY SOIL MOIST  ',231,001,     &
     &                       'SMDRY ON surface'/
      DATA IFILV(233),AVBL(233),IQ(233),IS(233),AVBLGRB2(233)      &
     &                      /1,'SOIL MOIST POROSITY ',240,001,     &
     &                       'POROS ON surface'/
      DATA IFILV(234),AVBL(234),IQ(234),IS(234),AVBLGRB2(234)      &
     &                      /1,'MIN STOMATAL RESIST ',203,001,     &
     &                       'RSMIN ON surface'/
      DATA IFILV(235),AVBL(235),IQ(235),IS(235),AVBLGRB2(235)      &
     &                      /1,'NO OF ROOT LAYERS   ',171,001,     &
     &                       'RLYRS ON surface'/
      DATA IFILV(236),AVBL(236),IQ(236),IS(236),AVBLGRB2(236)      &
     &                      /1,'SOIL MOIST WILT PT  ',219,001,     &
     &                       'WILT ON surface'/
      DATA IFILV(237),AVBL(237),IQ(237),IS(237),AVBLGRB2(237)      &
     &                      /1,'SOIL MOIST REFERENCE',230,001,     &
     &                       'SMREF ON surface'/
      DATA IFILV(238),AVBL(238),IQ(238),IS(238),AVBLGRB2(238)      &
     &                      /1,'CANOPY COND SOLAR   ',246,001,     &
     &                       'RCS ON surface'/
      DATA IFILV(239),AVBL(239),IQ(239),IS(239),AVBLGRB2(239)      &
     &                      /1,'CANOPY COND TEMP    ',247,001,     &
     &                       'RCT ON surface'/
      DATA IFILV(240),AVBL(240),IQ(240),IS(240),AVBLGRB2(240)      &
     &                      /1,'CANOPY COND HUMID   ',248,001,     &
     &                       'RCQ ON surface'/
      DATA IFILV(241),AVBL(241),IQ(241),IS(241),AVBLGRB2(241)      &
     &                      /1,'CANOPY COND SOILM   ',249,001,     &
     &                       'RCSOL ON surface'/
      DATA IFILV(242),AVBL(242),IQ(242),IS(242),AVBLGRB2(242)      &
     &                      /1,'POTENTIAL EVAP      ',145,001,     &
     &                       'PEVPR ON surface'/
      DATA IFILV(243),AVBL(243),IQ(243),IS(243),AVBLGRB2(243)      &
     &                      /1,'DIFFUSION H RATE S S',182,107,     &
     &                       'VEDH ON sigma_lvl'/
!
      DATA IFILV(245),AVBL(245),IQ(245),IS(245),AVBLGRB2(245)      &
     &                      /1,'SFC WIND GUST       ',180,001,     &
     &                       'GUST ON surface'/
      DATA IFILV(246),AVBL(246),IQ(246),IS(246),AVBLGRB2(246)      &
     &                      /1,'LIFT PCL LVL PRESS  ',141,116,     &
     &                       'PLPL ON spec_pres_above_grnd'/
      DATA IFILV(247),AVBL(247),IQ(247),IS(247),AVBLGRB2(247)      &
     &                      /1,'LOW WET BULB ZERO HT',007,245,     &
     &                       'HGT ON lwst_lvl_of_wet_bulb_zero'/
      DATA IFILV(248),AVBL(248),IQ(248),IS(248), AVBLGRB2(248)     &
     &                      /1,'EMISSIVITY          ',193,001,     &
                             'EMISSIVITY ON surface'/
      DATA IFILV(249),AVBL(249),IQ(249),IS(249),AVBLGRB2(249)      &
     &                      /1,'CONV PRECIP RATE    ',214,001,     &
     &                       'CPRAT ON surface'/
!--- USING Table 129
!
      DATA IFILV(250),AVBL(250),IQ(250),IS(250),AVBLGRB2(250)      &
     &                      /1,'RADAR REFL MDL SFCS ',211,109,     &
     &                       'REFD ON hybrid_lvl'/
      DATA IFILV(251),AVBL(251),IQ(251),IS(251),AVBLGRB2(251)      &
     &                      /1,'RADAR REFL ON P SFCS',211,100,     &
     &                       'REFD ON isobaric_sfc'/
      DATA IFILV(252),AVBL(252),IQ(252),IS(252),AVBLGRB2(252)      &
     &                      /1,'COMPOSITE RADAR REFL',212,200,     &
     &                       'REFC ON entire_atmos_single_lyr'/
      DATA IFILV(253),AVBL(253),IQ(253),IS(253),AVBLGRB2(253)      &
     &                      /1,'RADAR REFL AGL      ',211,105,     &
     &                       'REFD ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(254),AVBL(254),IQ(254),IS(254),AVBLGRB2(254)      &
     &                      /1,'LEAF AREA INDEX     ',182,001,     &
     &                       'LAI ON surface'/
!
      DATA IFILV(256),AVBL(256),IQ(256),IS(256),AVBLGRB2(256)      &
     &                      /1,'ACM LSM PRECIP      ',154,001,     &
     &                       'ACM LSPA ON surface'/
!
!--- FOLLOWINGS ARE AVIATION-RELATED FIELDS: ADDED BY BINBIN ZHOU
!
      DATA IFILV(257),AVBL(257),IQ(257),IS(257),AVBLGRB2(257)      &
     &                      /1,'IN-FLIGHT ICING     ',186,100,     &
     &                       'TIPD ON isobaric_sfc'/
      DATA IFILV(258),AVBL(258),IQ(258),IS(258),AVBLGRB2(258)      &
     &                      /1,'CLEAR AIR TURBULENCE',185,100,     &
     &                       'TPFI ON isobaric_sfc'/
      DATA IFILV(259),AVBL(259),IQ(259),IS(259),AVBLGRB2(259)      &
     &                      /1,'0-2000FT LLWS       ',136,106,     &
     &                       'VW_SH ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(260),AVBL(260),IQ(260),IS(260),AVBLGRB2(260)      &
     &                      /1,'CEILING             ',007,215,     &
     &                       'HGT ON cloud_ceilng'/
      DATA IFILV(261),AVBL(261),IQ(261),IS(261),AVBLGRB2(261)      &
     &                      /1,'FLIGHT RESTRICTION  ',020,002,     &
     &                       'VIS ON cloud_base'/
!
      DATA IFILV(262),AVBL(262),IQ(262),IS(262),AVBLGRB2(262)      &
     &                      /1,'INSTN CLR INC SFC SW',161,001,     &
     &                       'INST CSDSF ON surface'/
      DATA IFILV(263),AVBL(263),IQ(263),IS(263),AVBLGRB2(263)      &
     &                      /1,'F_RimeF ON P SFCS   ',133,100,     &
     &                       'RIME ON isobaric_sfc'/
      DATA IFILV(264),AVBL(264),IQ(264),IS(264),AVBLGRB2(264)      &
     &                      /1,'W WIND ON MDL SFCS  ',040,109,     &
     &                       'DZDT ON hybrid_lvl'/
!      DATA IFILV(265),AVBL(265),IQ(265),IS(265),AVBLGRB2(265)      &
!     &                      /1,'BRIGHTNESS TEMP     ',118,008,     &
      DATA IFILV(265),AVBL(265),IQ(265),IS(265),AVBLGRB2(265)      &
     &                      /1,'BRIGHTNESS TEMP     ',213,008,     &
     &                       'SBT122 ON top_of_atmos'/
! H CHUANG--ADD GFS products
      DATA IFILV(266),AVBL(266),IQ(266),IS(266),AVBLGRB2(266)      &
     &                      /1,'AVE ALBEDO          ',084,001,     &
     &                       'AVE ALBDO ON surface'/
      DATA IFILV(267),AVBL(267),IQ(267),IS(267),AVBLGRB2(267)      &
     &                      /1,'OZONE ON MDL SFCS   ',154,109,     &
     &                       'O3MR ON hybrid_lvl'/
      DATA IFILV(268),AVBL(268),IQ(268),IS(268),AVBLGRB2(268)      &
     &                      /1,'OZONE ON P SFCS     ',154,100,     &
     &                       'O3MR ON isobaric_sfc'/
      DATA IFILV(269),AVBL(269),IQ(269),IS(269),AVBLGRB2(269)      &
     &                      /1,'SFC ZONAL MOMEN FX  ',124,001,     &
     &                       'AVE U_FLX ON surface'/
      DATA IFILV(270),AVBL(270),IQ(270),IS(270),AVBLGRB2(270)      &
     &                      /1,'SFC MERID MOMEN FX  ',125,001,     &
     &                       'AVE V_FLX ON surface'/
      DATA IFILV(271),AVBL(271),IQ(271),IS(271),AVBLGRB2(271)      &
     &                      /1,'AVE PRECIP RATE     ',059,001,     &
     &                       'AVE PRATE ON surface'/
      DATA IFILV(272),AVBL(272),IQ(272),IS(272),AVBLGRB2(272)      &
     &                      /1,'AVE CONV PRECIP RATE',214,001,     &
     &                       'AVE CPRAT ON surface'/
! CMAQ requested fields     
      DATA IFILV(273),AVBL(273),IQ(273),IS(273),AVBLGRB2(273)      &
     &                      /1,'HYBRID SIGMA DP     ',001,110,     &
     &                       'PRES ON hybrid_lvl_1L'/
      DATA IFILV(274),AVBL(274),IQ(274),IS(274),AVBLGRB2(274)      &
     &                      /1,'INSTN OUT TOA LW RAD',212,008,     &
     &                       'INST ULWRF ON top_of_atmos'/
!      DATA IFILV(275),AVBL(275),IQ(275),IS(275),AVBLGRB2(002)      &
!     &                      /1,'BRIGHTNESS TEMP NCAR',213,008,     &
      DATA IFILV(275),AVBL(275),IQ(275),IS(275),AVBLGRB2(275)      &
     &                      /1,'BRIGHTNESS TEMP NCAR',118,008,     &
     &                       'BRTMP ON top_of_atmos'/
      DATA IFILV(282),AVBL(282),IQ(282),IS(282),AVBLGRB2(282)      &
     &                      /1,'MODEL TOP PRESSURE  ',001,008,     &
     &                       'PRES ON top_of_atmos'/
      DATA IFILV(283),AVBL(283),IQ(283),IS(283),AVBLGRB2(283)      &
     &                      /1,'HYBRID PRESSURE DP  ',001,110,     &
     &                       'PRES ON hybrid_lvl_LLM'/
!
!--- USING Table 129
!
      DATA IFILV(276),AVBL(276),IQ(276),IS(276),AVBLGRB2(276)      &
     &                      /1,'COMPOSITE RAIN REFL ',165,200,     &
     &                       'REFZR ON entire_atmos_single_lyr'/
      DATA IFILV(277),AVBL(277),IQ(277),IS(277),AVBLGRB2(277)      &
     &                      /1,'COMPOSITE ICE REFL  ',166,200,     &
     &                       'REFZI ON entire_atmos_single_lyr'/
      DATA IFILV(278),AVBL(278),IQ(278),IS(278),AVBLGRB2(278)      &
     &                      /1,'COMPOSITE CONV REFL ',167,200,     &
     &                       'REFZC ON entire_atmos_single_lyr'/
      DATA IFILV(279),AVBL(279),IQ(279),IS(279),AVBLGRB2(279)      &
     &                      /1,'RAIN RADAR REFL AGL ',165,105,     &
     &                       'REFZR ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(280),AVBL(280),IQ(280),IS(280),AVBLGRB2(280)      &
     &                      /1,'ICE RADAR REFL AGL  ',166,105,     &
     &                       'REFZI ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(281),AVBL(281),IQ(281),IS(281),AVBLGRB2(281)      &
     &                      /1,'CONV RADAR REFL AGL ',167,105,     &
     &                       'REFZC ON spec_hgt_lvl_above_grnd'/
!
!--- USING Table 2
!
      DATA IFILV(284),AVBL(284),IQ(284),IS(284),AVBLGRB2(284)      &
     &                      /1,'W WIND ON P SFCS    ',040,100,     &
     &                       'DZDT ON isobaric_sfc'/
!
!--- USING Table 129
!
      DATA IFILV(285),AVBL(285),IQ(285),IS(285),AVBLGRB2(285)      &
     &                      /1,'TOTAL COLD LIQUID   ',168,200,     &
     &                       'TCLSW ON entire_atmos_single_lyr'/
      DATA IFILV(286),AVBL(286),IQ(286),IS(286),AVBLGRB2(286)      &
     &                      /1,'TOTAL MELTING ICE   ',169,200,     &
     &                       'TCOLM ON entire_atmos_single_lyr'/
!
!--- USING Table 2
!
      DATA IFILV(287),AVBL(287),IQ(287),IS(287),AVBLGRB2(287)      &
     &                      /1,'COLD LIQ BOT HEIGHT ',007,253,     &
     &                       'HGT ON lwst_bot_lvl_of_supercooled_liq_water_lyr'/
      DATA IFILV(288),AVBL(288),IQ(288),IS(288),AVBLGRB2(288)      &
     &                      /1,'COLD LIQ TOP HEIGHT ',007,254,     &
     &                       'HGT ON hghst_top_lvl_of_supercooled_liq_water_lyr'/
      DATA IFILV(289),AVBL(289),IQ(289),IS(289),AVBLGRB2(289)      &
     &                      /1,'RICH NO PBL HEIGHT  ',007,220,     &
     &                       'HGT ON planetary_bound_lyr'/
!
!---- New Column-integrated fields
      DATA IFILV(290),AVBL(290),IQ(290),IS(290),AVBLGRB2(290)      &
     &                      /1,'TOT COL SW T TNDY   ',250,200,     &
     &                       'SWHR ON entire_atmos_single_lyr'/
      DATA IFILV(291),AVBL(291),IQ(291),IS(291),AVBLGRB2(291)      &
     &                      /1,'TOT COL LW T TNDY   ',251,200,     &
     &                       'LWHR ON entire_atmos_single_lyr'/
      DATA IFILV(292),AVBL(292),IQ(292),IS(292),AVBLGRB2(292)      &
     &                      /1,'TOT COL GRD T TNDY  ',241,200,     &
     &                       'AVE LRGHR ON entire_atmos_single_lyr'/
      DATA IFILV(293),AVBL(293),IQ(293),IS(293),AVBLGRB2(293)      &
     &                      /1,'TOT COL CNVCT T TNDY',242,200,     &
     &                       'AVE CNVHR ON entire_atmos_single_lyr'/
      DATA IFILV(294),AVBL(294),IQ(294),IS(294),AVBLGRB2(294)      &
     &                      /1,'RADFLX TMP TNDY ON P',216,100,     &
     &                       'TTRAD ON isobaric_sfc'/
      DATA IFILV(295),AVBL(295),IQ(295),IS(295),AVBLGRB2(295)      &
     &                      /1,'TOT COL MST CNVG    ',135,200,     &
     &                       'MCONV ON entire_atmos_single_lyr'/
      DATA IFILV(296),AVBL(296),IQ(296),IS(296),AVBLGRB2(296)      &
     &                      /1,'HPC T ON SIGMA SFCS ',011,107,     &
     &                       'TMP ON sigma_lvl'/
! H CHUANG--ADD GFS products
      DATA IFILV(297),AVBL(297),IQ(297),IS(297),AVBLGRB2(297)      &
     &                      /1,'AVE CLR INC UV-B SW ',201,001,     &
     &                       'AVE CDUVB ON surface'/
      DATA IFILV(298),AVBL(298),IQ(298),IS(298),AVBLGRB2(298)      &
     &                      /1,'AVE INC UV-B SW     ',200,001,     &
     &                       'AVE DUVB ON surface'/
      DATA IFILV(299),AVBL(299),IQ(299),IS(299),AVBLGRB2(299)      &
     &                      /1,'TOT COL OZONE       ',010,200,     &
     &                       'TOZNE ON entire_atmos_single_lyr'/
      DATA IFILV(300),AVBL(300),IQ(300),IS(300),AVBLGRB2(300)      &
     &                      /1,'AVE LOW CLOUD FRAC  ',071,214,     &
     &                       'AVE T_CDC ON low_cloud_lyr'/
      DATA IFILV(301),AVBL(301),IQ(301),IS(301),AVBLGRB2(301)      &
     &                      /1,'AVE MID CLOUD FRAC  ',071,224,     &
     &                       'AVE T_CDC ON mid_cloud_lyr'/
      DATA IFILV(302),AVBL(302),IQ(302),IS(302),AVBLGRB2(302)      &
     &                      /1,'AVE HIGH CLOUD FRAC ',071,234,     &
     &                       'AVE T_CDC ON high_cloud_lyr'/
      DATA IFILV(303),AVBL(303),IQ(303),IS(303),AVBLGRB2(303)      &
     &                      /1,'AVE LOW CLOUD BOT P ',001,212,     &
     &                       'AVE PRES ON low_cloud_bot_lvl'/
      DATA IFILV(304),AVBL(304),IQ(304),IS(304),AVBLGRB2(304)      &
     &                      /1,'AVE LOW CLOUD TOP P ',001,213,     &
     &                       'AVE PRES ON low_cloud_top_lvl'/
      DATA IFILV(305),AVBL(305),IQ(305),IS(305),AVBLGRB2(305)      &
     &                      /1,'AVE LOW CLOUD TOP T ',011,213,     &
     &                       'AVE TMP ON low_cloud_top_lvl'/
      DATA IFILV(306),AVBL(306),IQ(306),IS(306),AVBLGRB2(306)      &
     &                      /1,'AVE MID CLOUD BOT P ',001,222,     &
     &                       'AVE PRES ON mid_cloud_bot_lvl'/
      DATA IFILV(307),AVBL(307),IQ(307),IS(307),AVBLGRB2(307)      &
     &                      /1,'AVE MID CLOUD TOP P ',001,223,     &
     &                       'AVE PRES ON mid_cloud_top_lvl'/
      DATA IFILV(308),AVBL(308),IQ(308),IS(308),AVBLGRB2(308)      &
     &                      /1,'AVE MID CLOUD TOP T ',011,223,     &
     &                       'AVE TMP ON mid_cloud_top_lvl'/
      DATA IFILV(309),AVBL(309),IQ(309),IS(309),AVBLGRB2(309)      &
     &                      /1,'AVE HIGH CLOUD BOT P',001,232,     &
     &                       'AVE PRES ON high_cloud_bot_lvl'/
      DATA IFILV(310),AVBL(310),IQ(310),IS(310),AVBLGRB2(310)      &
     &                      /1,'AVE HIGH CLOUD TOP P',001,233,     &
     &                       'AVE PRES ON high_cloud_top_lvl'/
      DATA IFILV(311),AVBL(311),IQ(311),IS(311),AVBLGRB2(311)      &
     &                      /1,'AVE HIGH CLOUD TOP T',011,233,     &
     &                       'AVE TMP ON high_cloud_top_lvl'/
      DATA IFILV(312),AVBL(312),IQ(312),IS(312),AVBLGRB2(312)      &
     &                      /1,'TOT COL REL HUM     ',052,200,     &
     &                       'RH ON entire_atmos_single_lyr'/
      DATA IFILV(313),AVBL(313),IQ(313),IS(313),AVBLGRB2(313)      &
     &                      /1,'CLOUD WORK FUNCTION ',146,200,     &
     &                       'AVE CWORK ON entire_atmos_single_lyr'/
      DATA IFILV(314),AVBL(314),IQ(314),IS(314),AVBLGRB2(314)      &
     &                      /1,'MAX WIND TEMPERATURE',011,006,     &
     &                       'TMP ON max_wind'/
      DATA IFILV(315),AVBL(315),IQ(315),IS(315),AVBLGRB2(315)      &
     &                      /1,'AVE Z GRAVITY STRESS',147,001,     &
     &                       'AVE U_GWD ON surface'/
      DATA IFILV(316),AVBL(316),IQ(316),IS(316),AVBLGRB2(316)      &
     &                      /1,'AVE M GRAVITY STRESS',148,001,     &
     &                       'AVE V_GWD ON surface'/
      DATA IFILV(317),AVBL(317),IQ(317),IS(317),AVBLGRB2(317)      &
     &                      /1,'AVE PRECIP TYPE     ',140,001,     &
     &                       'AVE CRAIN ON surface'/
      DATA IFILV(318),AVBL(318),IQ(318),IS(318),AVBLGRB2(318)      &
     &                      /1,'LFM 0.44-1.00 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(319),AVBL(319),IQ(319),IS(319),AVBLGRB2(319)      &
     &                      /1,'LFM 0.72-0.94 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(320),AVBL(320),IQ(320),IS(320),AVBLGRB2(320)      &
     &                      /1,'LFM 0.44-0.72 RELHUM',052,108,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(321),AVBL(321),IQ(321),IS(321),AVBLGRB2(321)      &
     &                      /1,'NGM 0.9950 TEMP     ',011,107,     &
     &                       'TMP ON sigma_lvl'/
      DATA IFILV(322),AVBL(322),IQ(322),IS(322),AVBLGRB2(322)      &
     &                      /1,'NGM 0.9950 POT TEMP ',013,107,     &
     &                       'POT ON sigma_lvl'/
      DATA IFILV(323),AVBL(323),IQ(323),IS(323),AVBLGRB2(323)      &
     &                      /1,'NGM 0.9950 REL HUM  ',052,107,     &
     &                       'RH ON sigma_lvl'/
      DATA IFILV(324),AVBL(324),IQ(324),IS(324),AVBLGRB2(324)      &
     &                      /1,'NGM 0.9950 U WIND   ',033,107,     &
     &                       'U_GRD ON sigma_lvl'/
      DATA IFILV(325),AVBL(325),IQ(325),IS(325),AVBLGRB2(325)      &
     &                      /1,'NGM 0.9950 V WIND   ',034,107,     &
     &                       'V_GRD ON sigma_lvl'/
      DATA IFILV(326),AVBL(326),IQ(326),IS(326),AVBLGRB2(326)      &
     &                      /1,'NGM 0.9950 OMEGA    ',039,107,     &
     &                       'V_VEL ON sigma_lvl'/
      DATA IFILV(327),AVBL(327),IQ(327),IS(327),AVBLGRB2(327)      &
     &                      /1,'GOES TB - CH 2      ',213,008,     & !table 129
     &                       'SBT122 ON top_of_atmos'/
      DATA IFILV(328),AVBL(328),IQ(328),IS(328),AVBLGRB2(328)      &
     &                      /1,'GOES TB - CH 3      ',214,008,     & !table 129
     &                       'SBT123 ON top_of_atmos'/
      DATA IFILV(329),AVBL(329),IQ(329),IS(329),AVBLGRB2(329)      &
     &                      /1,'GOES TB - CH 4      ',215,008,     & !table 129
     &                       'SBT124 ON top_of_atmos'/
      DATA IFILV(330),AVBL(330),IQ(330),IS(330),AVBLGRB2(330)      &
     &                      /1,'GOES TB - CH 5      ',216,008,     & !table 129     
     &                       'SBT125 ON top_of_atmos'/
      DATA IFILV(331),AVBL(331),IQ(331),IS(331),AVBLGRB2(331)      &
     &                      /1,'CLD FRAC ON P SFCS  ',071,100,     &
     &                       'T_CDC ON isobaric_sfc'/
      DATA IFILV(332),AVBL(332),IQ(332),IS(332),AVBLGRB2(332)      &
     &                      /1,'U WIND ON THETA SFCS',033,113,     &
     &                       'U_GRD ON isentropic_lvl'/
      DATA IFILV(333),AVBL(333),IQ(333),IS(333),AVBLGRB2(333)      &
     &                      /1,'V WIND ON THETA SFCS',034,113,     &
     &                       'V_GRD ON isentropic_lvl'/
      DATA IFILV(334),AVBL(334),IQ(334),IS(334),AVBLGRB2(334)      &
     &                      /1,'TEMP ON THETA SFCS  ',011,113,     &
     &                       'TMP ON isentropic_lvl'/
      DATA IFILV(335),AVBL(335),IQ(335),IS(335),AVBLGRB2(335)      &  
     &                      /1,'PV ON THETA SFCS    ',004,113,     &
     &                       'PVORT ON isentropic_lvl'/
      DATA IFILV(353),AVBL(353),IQ(353),IS(353),AVBLGRB2(353)      &    !353
     &                      /1,'M STRMFUNC ON THETA ',037,113,     &
     &                       'MNTSF ON isentropic_lvl'/
      DATA IFILV(351),AVBL(351),IQ(351),IS(351),AVBLGRB2(351)      &    !351
     &                      /1,'S STAB ON THETA SFCS',019,113,     &
     &                       'LAPR ON isentropic_lvl'/
      DATA IFILV(352),AVBL(352),IQ(352),IS(352),AVBLGRB2(352)      &    !352
     &                      /1,'RH ON THETA SFCS    ',052,113,     &
     &                       'RH ON isentropic_lvl'/
      DATA IFILV(336),AVBL(336),IQ(336),IS(336),AVBLGRB2(336)      &
     &                      /1,'U WIND ON PV SFCS   ',033,117,     &
     &                       'U_GRD ON pot_vort_sfc'/
      DATA IFILV(337),AVBL(337),IQ(337),IS(337),AVBLGRB2(337)      &
     &                      /1,'V WIND ON PV SFCS   ',034,117,     &
     &                       'V_GRD ON pot_vort_sfc'/
      DATA IFILV(338),AVBL(338),IQ(338),IS(338),AVBLGRB2(338)      &
     &                      /1,'TEMP ON PV SFCS     ',011,117,     &
     &                       'TMP ON pot_vort_sfc'/
      DATA IFILV(339),AVBL(339),IQ(339),IS(339),AVBLGRB2(339)      &
     &                      /1,'HEIGHT ON PV SFCS   ',007,117,     &
     &                       'HGT ON pot_vort_sfc'/
      DATA IFILV(340),AVBL(340),IQ(340),IS(340),AVBLGRB2(340)      &
     &                      /1,'PRESSURE ON PV SFCS ',001,117,     &
     &                       'PRES ON pot_vort_sfc'/
      DATA IFILV(341),AVBL(341),IQ(341),IS(341),AVBLGRB2(341)      &
     &                      /1,'SHEAR ON PV SFCS    ',136,117,     &
     &                       'VW_SH ON pot_vort_sfc'/
      DATA IFILV(342),AVBL(342),IQ(342),IS(342),AVBLGRB2(342)      &
     &                      /1,'PBL CLD FRACTION    ',071,211,     &
     &                       'AVE T_CDC ON bound_lyr_cloud_lyr'/
      DATA IFILV(343),AVBL(343),IQ(343),IS(343),AVBLGRB2(343)      &
     &                      /1,'AVE WATER RUNOFF    ',090,001,     &
     &                       'AVE WATR ON surface'/
      DATA IFILV(344),AVBL(344),IQ(344),IS(344),AVBLGRB2(344)      &
     &                      /1,'PBL REGIME          ',220,001,     &
     &                       'PBLREG ON surface'/
      DATA IFILV(345),AVBL(345),IQ(345),IS(345),AVBLGRB2(345)      &
     &                      /1,'MAX SHELTER TEMP    ',015,105,     &
     &                       'TMAX ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(346),AVBL(346),IQ(346),IS(346),AVBLGRB2(346)      &
     &                      /1,'MIN SHELTER TEMP    ',016,105,     &
     &                       'TMIN ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(347),AVBL(347),IQ(347),IS(347),AVBLGRB2(347)      &
     &                      /1,'MAX SHELTER RH      ',218,105,     & !table129
     &                       'RHMAX ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(348),AVBL(348),IQ(348),IS(348),AVBLGRB2(348)      &
     &                      /1,'MIN SHELTER RH      ',217,105,     & !table129
     &                       'RHMIN ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(510),AVBL(510),IQ(510),IS(510),AVBLGRB2(510)      &
     &                      /1,'MAX SHELTER SPFH    ',051,105,     & !table129
     &                       'QMAX ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(511),AVBL(511),IQ(511),IS(511),AVBLGRB2(511)      &
     &                      /1,'MIN SHELTER SPFH    ',051,105,     & !table129
     &                       'QMIN ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(512),AVBL(512),IQ(512),IS(512),AVBLGRB2(512)      &
     &                      /1,'AERO CONDUCTANCE    ',179,001,     & !table129
     &                       'ACOND ON surface'/
      DATA IFILV(349),AVBL(349),IQ(349),IS(349),AVBLGRB2(349)      &
     &                      /1,'ICE THICKNESS       ',092,001,     &
     &                       'ICETK ON surface'/
      DATA IFILV(354),AVBL(354),IQ(354),IS(354),AVBLGRB2(354)      &
     &                      /1,'SW TNDY ON P SFCS   ',250,100,     &
     &                       'SWHR ON isobaric_sfc'/
      DATA IFILV(355),AVBL(355),IQ(355),IS(355),AVBLGRB2(355)      &
     &                      /1,'LW TNDY ON P SFCS   ',251,100,     &
     &                       'LWHR ON isobaric_sfc'/
      DATA IFILV(356),AVBL(356),IQ(356),IS(356),AVBLGRB2(356)      &
     &                      /1,'VDIFF TNDY ON P SFCS',246,100,     &
     &                       'VDFHR ON isobaric_sfc'/
      DATA IFILV(357),AVBL(357),IQ(357),IS(357),AVBLGRB2(357)      &
     &                      /1,'D CNVCT TNDY ON P SF',242,100,     &
     &                       'CNVHR ON isobaric_sfc'/
      DATA IFILV(358),AVBL(358),IQ(358),IS(358),AVBLGRB2(358)      &
     &                      /1,'S CNVCT TNDY ON P SF',244,100,     &
     &                       'SHAHR ON isobaric_sfc'/
      DATA IFILV(359),AVBL(359),IQ(359),IS(359),AVBLGRB2(359)      &
     &                      /1,'GRDSCL TNDY ON P SFC',241,100,     &
     &                       'LRGHR ON isobaric_sfc'/
      DATA IFILV(360),AVBL(360),IQ(360),IS(360),AVBLGRB2(360)      &
     &                      /1,'VDIFF MOIS ON P SFCS',249,100,     &
     &                       'VDFMR ON isobaric_sfc'/
      DATA IFILV(361),AVBL(361),IQ(361),IS(361),AVBLGRB2(361)      &
     &                      /1,'D CNVCT MOIS ON P SF',243,100,     &
     &                       'CNVMR ON isobaric_sfc'/
      DATA IFILV(362),AVBL(362),IQ(362),IS(362),AVBLGRB2(362)      &
     &                      /1,'S CNVCT MOIS ON P SF',245,100,     &
     &                       'SHAMR ON isobaric_sfc'/
      DATA IFILV(363),AVBL(363),IQ(363),IS(363),AVBLGRB2(363)      &
     &                      /1,'N RAD TNDY ON P SFCS',173,100,     &
     &                       'LRGMR ON isobaric_sfc'/
      DATA IFILV(364),AVBL(364),IQ(364),IS(364),AVBLGRB2(364)      &
     &                      /1,'OZONE VDIFF ON P SFC',174,100,     &
     &                       'VDFOZ ON isobaric_sfc'/
      DATA IFILV(365),AVBL(365),IQ(365),IS(365),AVBLGRB2(365)      &
     &                      /1,'OZONE PROD ON P SFCS',175,100,     &
     &                       'POZ ON isobaric_sfc'/
      DATA IFILV(366),AVBL(366),IQ(366),IS(366),AVBLGRB2(366)      &
     &                      /1,'OZONE TNDY ON P SFCS',188,100,     &
     &                       'TOZ ON isobaric_sfc'/
      DATA IFILV(367),AVBL(367),IQ(367),IS(367),AVBLGRB2(367)      &
     &                      /1,'MASS WEIGHTED PV    ',139,100,     &
     &                       'PV_MW ON isobaric_sfc'/
      DATA IFILV(368),AVBL(368),IQ(368),IS(368),AVBLGRB2(368)      &
     &                      /1,'UNKNOWN D3D ON P SFC',239,100,     &
     &                       'SNOT ON isobaric_sfc'/
      DATA IFILV(369),AVBL(369),IQ(369),IS(369),AVBLGRB2(369)      &
     &                      /1,'VDIFF Z ACCE ON P SF',247,100,     &
     &                       'VDFUA ON isobaric_sfc'/
      DATA IFILV(370),AVBL(370),IQ(370),IS(370),AVBLGRB2(370)      &
     &                      /1,'G DRAG Z ACCE ON P S',181,100,     &
     &                       'GWDU ON isobaric_sfc'/
      DATA IFILV(371),AVBL(371),IQ(371),IS(371),AVBLGRB2(371)      &
     &                      /1,'CNVCT U M MIX ON P S',183,100,     &
     &                       'CNVU ON isobaric_sfc'/
      DATA IFILV(372),AVBL(372),IQ(372),IS(372),AVBLGRB2(372)      &
     &                      /1,'VDIFF M ACCE ON P SF',248,100,     &
     &                       'VDFVA ON isobaric_sfc'/
      DATA IFILV(373),AVBL(373),IQ(373),IS(373),AVBLGRB2(373)      &
     &                      /1,'G DRAG M ACCE ON P S',182,100,     &
     &                       'GWDV ON isobaric_sfc'/
      DATA IFILV(374),AVBL(374),IQ(374),IS(374),AVBLGRB2(374)      &
     &                      /1,'CNVCT V M MIX ON P S',184,100,     &
     &                       'CNVV ON isobaric_sfc'/
      DATA IFILV(375),AVBL(375),IQ(375),IS(375),AVBLGRB2(375)      &
     &                      /1,'N CNVCT CLD FRA ON P',213,100,     &
     &                       'CDLYR ON isobaric_sfc'/
      DATA IFILV(376),AVBL(376),IQ(376),IS(376),AVBLGRB2(376)      &
     &                      /1,'GOES BRIGHTNESS-CH 3',221,008,     & !table 129
     &                       'SBC123 ON top_of_atmos'/
      DATA IFILV(377),AVBL(377),IQ(377),IS(377),AVBLGRB2(377)      &
     &                      /1,'GOES BRIGHTNESS-CH 4',222,008,     & !table 129 
     &                       'SBC124 ON top_of_atmos'/
      DATA IFILV(378),AVBL(378),IQ(378),IS(378),AVBLGRB2(378)      &
     &                      /1,'OMEGA ON THETA SFCS ',039,113,     &
     &                       'V_VEL ON isentropic_lvl'/
! D3D fields
      DATA IFILV(379),AVBL(379),IQ(379),IS(379),AVBLGRB2(379)      &
     &                      /1,'T DIAB TNDY ON P SFC',215,100,     &
     &                       'TTDIA ON isobaric_sfc'/
      DATA IFILV(391),AVBL(391),IQ(391),IS(391),AVBLGRB2(391)      &
     &                      /1,'CNVCT U M FLX ON P S',202,100,     &
     &                       'CNVUMF ON isobaric_sfc'/
      DATA IFILV(392),AVBL(392),IQ(392),IS(392),AVBLGRB2(392)      &
     &                      /1,'CNVCT D M FLX ON P S',209,100,     &
     &                       'CNVDMF ON isobaric_sfc'/
      DATA IFILV(393),AVBL(393),IQ(393),IS(393),AVBLGRB2(393)      &
     &                      /1,'CNVCT DET M FLX ON P',219,100,     &
     &                       'CNVDEMF ON isobaric_sfc'/
      DATA IFILV(394),AVBL(394),IQ(394),IS(394),AVBLGRB2(394)      &
     &                      /1,'CNVCT Z G DRAG ON P ',196,100,     &
     &                       'CNGWDU ON isobaric_sfc'/
      DATA IFILV(395),AVBL(395),IQ(395),IS(395),AVBLGRB2(395)      &
     &                      /1,'CNVCT M G DRAG ON P ',197,100,     &
     &                       'CNGWDV ON isobaric_sfc'/
!---- Using table 129   !aqm PLee 1/07
      DATA IFILV(380),AVBL(380),IQ(380),IS(380),AVBLGRB2(380)      &
     &                      /1,'DIFFUSION H RATE MDL',182,109,     &
     &                       'VEDH ON hybrid_lvl'/
!---- Using table 2     !aqm PLee 3/07
      DATA IFILV(381),AVBL(381),IQ(381),IS(381),AVBLGRB2(381)      &
     &                      /1,'MIXHT HEIGHT        ',067,001,     &
     &                       'MIXHT ON surface'/
! NEW GFS FLUX FILE FIELDS
      DATA IFILV(382),AVBL(382),IQ(382),IS(382),AVBLGRB2(382)      &
     &                      /1,'AVE CLR INC SFC LW  ',163,001,     &
     &                       'AVE CSDLF ON surface'/
      DATA IFILV(383),AVBL(383),IQ(383),IS(383),AVBLGRB2(383)      &
     &                      /1,'AVE CLR INC SFC SW  ',161,001,     &
     &                       'AVE CSDSF ON surface'/
      DATA IFILV(384),AVBL(384),IQ(384),IS(384),AVBLGRB2(384)      &
     &                      /1,'AVE CLR OUT SFC LW  ',162,001,     &
     &                       'AVE CSULF ON surface'/
      DATA IFILV(385),AVBL(385),IQ(385),IS(385),AVBLGRB2(385)      &
     &                      /1,'AVE CLR OUT TOA LW  ',162,008,     &
     &                       'AVE CSULF ON top_of_atmos'/
      DATA IFILV(386),AVBL(386),IQ(386),IS(386),AVBLGRB2(386)      &
     &                      /1,'AVE CLR OUT SFC SW  ',160,001,     &
     &                       'AVE CSUSF ON surface'/
      DATA IFILV(387),AVBL(387),IQ(387),IS(387),AVBLGRB2(387)      &
     &                      /1,'AVE CLR OUT TOA SW  ',160,008,     &
     &                       'AVE CSUSF ON top_of_atmos'/
      DATA IFILV(388),AVBL(388),IQ(388),IS(388),AVBLGRB2(388)      &
     &                      /1,'AVE INC TOA SW      ',204,008,     &
     &                       'AVE DSWRF ON top_of_atmos'/
      DATA IFILV(389),AVBL(389),IQ(389),IS(389),AVBLGRB2(389)      &  
     &                      /1,'TRANSPORT U WIND    ',033,220,     &
     &                       'U_GRD ON planetary_bound_lyr'/
      DATA IFILV(390),AVBL(390),IQ(390),IS(390),AVBLGRB2(390)      &
     &                      /1,'TRANSPORT V WIND    ',034,220,     &
     &                       'V_GRD ON planetary_bound_lyr'/
! Add TIGGE FIELDS
      DATA IFILV(396),AVBL(396),IQ(396),IS(396),AVBLGRB2(396)      & 
     &                      /1,'SUNSHINE DURATION   ',191,001,     & !table 133
     &                       'SUNSD ON surface'/
      DATA IFILV(397),AVBL(397),IQ(397),IS(397),AVBLGRB2(397)      &
     &                      /1,'FIELD CAPACITY      ',220,001,     & !table 130
     &                       'FLDCP ON surface'/
! Add ICAO FIELDS
      DATA IFILV(398),AVBL(398),IQ(398),IS(398),AVBLGRB2(398)      & 
     &                      /1,'ICAO HGHT MAX WIND  ',005,006,     & 
     &                       'ICAHT ON max_wind'/
      DATA IFILV(399),AVBL(399),IQ(399),IS(399),AVBLGRB2(399)      &
     &                      /1,'ICAO HGHT AT TROP   ',005,007,     &
     &                       'ICAHT ON tropopause'/
      DATA IFILV(400),AVBL(400),IQ(400),IS(400),AVBLGRB2(400)      &
     &                      /1,'RADAR ECHO TOP      ',240,200,     &
     &                       'RETOP ON entire_atmos_single_lyr'/
!
! Add MORE CFSRR FIELDS
! surface Visible beam downward solar flux
      DATA IFILV(401),AVBL(401),IQ(401),IS(401),AVBLGRB2(401)      &
     &                      /1,'AVE IN SFC VIS SW BE',166,001,     &
     &                       'AVE VBDSF ON surface'/
!surface Visible diffuse downward solar flux
      DATA IFILV(402),AVBL(402),IQ(402),IS(402),AVBLGRB2(402)      &
     &                      /1,'AVE IN SFC VIS SW DF',167,001,     &
     &                       'AVE VDDSF ON surface'/
!surface Near IR beam downward solar flux
      DATA IFILV(403),AVBL(403),IQ(403),IS(403),AVBLGRB2(403)      &
     &                      /1,'AVE IN SFC IR SW BE ',168,001,     &
     &                       'AVE NBDSF ON surface'/
!surface Near IR diffuse downward solar flux
      DATA IFILV(404),AVBL(404),IQ(404),IS(404),AVBLGRB2(404)      &
     &                      /1,'AVE IN SFC IR SW DF ',169,001,     &
     &                       'AVE NDDSF ON surface'/
! SNOWFALL RATE
      DATA IFILV(405),AVBL(405),IQ(405),IS(405),AVBLGRB2(405)      &
     &                      /1,'AVE SNOWFALL RATE   ',064,001,     &
     &                       'AVE SRWEQ ON surface'/
! ADD DUST FIELDS ON P SFCS (GOCART)
      DATA IFILV(438),AVBL(438),IQ(438),IS(438),AVBLGRB2(438)      &
     &                      /1,'DUST 1 ON P SFCS    ',240,100,     &
     &                       'DU1 ON isobaric_sfc'/
      DATA IFILV(439),AVBL(439),IQ(439),IS(439),AVBLGRB2(439)      &
     &                      /1,'DUST 2 ON P SFCS    ',241,100,     &
     &                       'DU2 ON isobaric_sfc'/
      DATA IFILV(440),AVBL(440),IQ(440),IS(440),AVBLGRB2(440)      &
     &                      /1,'DUST 3 ON P SFCS    ',242,100,     &
     &                       'DU3 ON isobaric_sfc'/
      DATA IFILV(441),AVBL(441),IQ(441),IS(441),AVBLGRB2(441)      &
     &                      /1,'DUST 4 ON P SFCS    ',243,100,     &
     &                       'DU4 ON isobaric_sfc'/
      DATA IFILV(442),AVBL(442),IQ(442),IS(442),AVBLGRB2(442)      &
     &                      /1,'DUST 5 ON P SFCS    ',244,100,     &
     &                       'DU5 ON isobaric_sfc'/
!
      DATA IFILV(443),AVBL(443),IQ(443),IS(443),AVBLGRB2(443)      &
     &                      /1,'EQUIL LEVEL HEIGHT  ',007,247,     &
     &                       'HGT ON equil_lvl'/
      DATA IFILV(444),AVBL(444),IQ(444),IS(444),AVBLGRB2(444)      &
     &                      /1,'LIGHTNING           ',187,001,     &
     &                       'LTNG ON surface'/

! GOES WEST
      DATA IFILV(446),AVBL(446),IQ(446),IS(446),AVBLGRB2(446)      &
     &                      /1,'GOES W TB - CH 2    ',241,008,     &
     &                       'SBT112 ON top_of_atmos'/ !Table 130
      DATA IFILV(447),AVBL(447),IQ(447),IS(447),AVBLGRB2(447)      &
     &                      /1,'GOES W TB - CH 3    ',242,008,     &
     &                       'SBT113 ON top_of_atmos'/ !Table 130
      DATA IFILV(448),AVBL(448),IQ(448),IS(448),AVBLGRB2(448)      &
     &                      /1,'GOES W TB - CH 4    ',243,008,     &
     &                       'SBT114 ON top_of_atmos'/ !Table 130
      DATA IFILV(449),AVBL(449),IQ(449),IS(449),AVBLGRB2(449)      &
     &                      /1,'GOES W TB - CH 5    ',244,008,     &
     &                       'SBT115 ON top_of_atmos'/ !Table 130

! NCAR GFIP
      DATA IFILV(450),AVBL(450),IQ(450),IS(450),AVBLGRB2(450)      &
     &                      /1,'NCAR IN-FLIGHT ICING',168,100,     &
     &                       'TIPD ON isobaric_sfc'/
! Flight level Q
      DATA IFILV(451),AVBL(451),IQ(451),IS(451),AVBLGRB2(451)      &
     &                      /1,'SPE HUM AT FD HEIGHT',051,103,     &
     &                       'SPF_H ON spec_alt_above_mean_sea_lvl'/
! Virtual T based CAPE
      DATA IFILV(452),AVBL(452),IQ(452),IS(452),AVBLGRB2(452)      &
     &                      /1,'TV CNVCT AVBL POT EN',202,001,     &
     &                       'VTCAPE ON surface'/
      DATA IFILV(453),AVBL(453),IQ(453),IS(453),AVBLGRB2(453)      &
     &                      /1,'TV CNVCT INHIBITION ',201,001,     &
     &                       'VTCIN ON surface'/
      DATA IFILV(454),AVBL(454),IQ(454),IS(454),AVBLGRB2(454)      &
     &                      /1,'VENTILATION RATE    ',241,220,     &
     &                       'VRATE ON planetary_bound_lyr'/
      DATA IFILV(455),AVBL(455),IQ(455),IS(455),AVBLGRB2(455)      &
     &                      /1,'HAINES INDEX        ',250,001,     &
     &                       'HINDEX ON surface'/
      DATA IFILV(456),AVBL(456),IQ(456),IS(456),AVBLGRB2(456)      &
     &                      /1,'GOESE TB-2 NON NADIR',213,008,     &
     &                       'SBT122 ON top_of_atmos'/ !table 129
      DATA IFILV(457),AVBL(457),IQ(457),IS(457),AVBLGRB2(457)      &
     &                      /1,'GOESE TB-3 NON NADIR',214,008,     &
     &                       'SBT123 ON top_of_atmos'/ !table 129
      DATA IFILV(458),AVBL(458),IQ(458),IS(458),AVBLGRB2(458)      &
     &                      /1,'GOESE TB-4 NON NADIR',215,008,     &
     &                       'SBT124 ON top_of_atmos'/ !table 129
      DATA IFILV(459),AVBL(459),IQ(459),IS(459),AVBLGRB2(459)      &
     &                      /1,'GOESE TB-5 NON NADIR',216,008,     &
     &                       'SBT126 ON top_of_atmos'/ !table 129
      DATA IFILV(460),AVBL(460),IQ(460),IS(460),AVBLGRB2(460)      &
     &                      /1,'GOESW TB-2 NON NADIR',241,008,     &
     &                       'SBT112 ON top_of_atmos'/ !table 130
      DATA IFILV(461),AVBL(461),IQ(461),IS(461),AVBLGRB2(461)      &
     &                      /1,'GOESW TB-3 NON NADIR',242,008,     &
     &                       'SBT113 ON top_of_atmos'/ !table 130
      DATA IFILV(462),AVBL(462),IQ(462),IS(462),AVBLGRB2(462)      &
     &                      /1,'GOESW TB-4 NON NADIR',243,008,     &
     &                       'SBT114 ON top_of_atmos'/ !table 130
      DATA IFILV(463),AVBL(463),IQ(463),IS(463),AVBLGRB2(463)      &
     &                      /1,'GOESW TB-5 NON NADIR',244,008,     &
     &                       'SBT115 ON top_of_atmos'/ !table 130


! NCAR GFIP Severity
      DATA IFILV(480),AVBL(480),IQ(480),IS(480),AVBLGRB2(480)      &
     &                      /1,'NCAR INFLT ICING SEV',175,100, &
     &                       'SEV ON isobaric_sfc'/    !table 129  
!
      DATA IFILV(482),AVBL(482),IQ(482),IS(482),AVBLGRB2(482)      &
     &                      /1,'PRESS AT FD HEIGHTS ',001,103,     &
                             'PRES ON spec_alt_above_mean_sea_lvl'/

      DATA IFILV(483),AVBL(483),IQ(483),IS(483),AVBLGRB2(483)      &
     &                      /1,'AMSRE TB - CH 9     ',176,008,     &
                             'AMSRETBCH9 ON top_of_atmos'/ !table 133
      DATA IFILV(484),AVBL(484),IQ(484),IS(484),AVBLGRB2(484)      &
     &                      /1,'AMSRE TB - CH 10    ',177,008,     &
                             'AMSRETBCH10 ON top_of_atmos'/ !table 133
      DATA IFILV(485),AVBL(485),IQ(485),IS(485),AVBLGRB2(485)      &
     &                      /1,'AMSRE TB - CH 11    ',178,008,     &
                             'AMSRETBCH11 ON top_of_atmos'/ !table 133
      DATA IFILV(486),AVBL(486),IQ(486),IS(486),AVBLGRB2(486)      &
     &                      /1,'AMSRE TB - CH 12    ',179,008,     &
                             'AMSRETBCH12 ON top_of_atmos'/ !table 133
!
      DATA IFILV(488),AVBL(488),IQ(488),IS(488),AVBLGRB2(488)      &
     &                      /1,'TMI TB - CH 6       ',176,008,     &
     &                       'TMITBCH6 ON top_of_atmos'/ !table 133
      DATA IFILV(489),AVBL(489),IQ(489),IS(489),AVBLGRB2(489)      &
     &                      /1,'TMI TB - CH 7       ',177,008,     &
     &                       'TMITBCH7 ON top_of_atmos'/ !table 133
      DATA IFILV(490),AVBL(490),IQ(490),IS(490),AVBLGRB2(490)      &
     &                      /1,'TMI TB - CH 8       ',178,008,     &
     &                       'TMITBCH8 ON top_of_atmos'/ !table 133
      DATA IFILV(491),AVBL(491),IQ(491),IS(491),AVBLGRB2(491)      &
     &                      /1,'TMI TB - CH 9       ',179,008,     &
     &                       'TMITBCH9 ON top_of_atmos'/ !table 133

!
! NAMB additions
      DATA IFILV(500),AVBL(500),IQ(500),IS(500),AVBLGRB2(500)      &
     &                      /1,'TIME AVG PCT SNW CVR',238,001,     &
     &                       'AVG SNOWC ON surface' /
      DATA IFILV(501),AVBL(501),IQ(501),IS(501),AVBLGRB2(501)      &
     &                      /1,'TIME AVG SFC PRESS  ',001,001,     &
     &                       'AVG PRES ON surface' /
      DATA IFILV(502),AVBL(502),IQ(502),IS(502),AVBLGRB2(502)      &
     &                      /1,'TIME AVG TMP AT 10M ',011,105,     &
     &                       'AVG TMP ON spec_hgt_lvl_above_grnd' /
      DATA IFILV(503),AVBL(503),IQ(503),IS(503),AVBLGRB2(503)      &
     &                      /1,'TAVG MASS EXCH COEF ',185,001,     &
     &                       'AVG AKHS ON surface' /
      DATA IFILV(504),AVBL(504),IQ(504),IS(504),AVBLGRB2(504)      &
     &                      /1,'TAVG WIND EXCH COEF ',186,001,     &
     &                       'AVG AKMS ON surface' /
      DATA IFILV(505),AVBL(505),IQ(505),IS(505),AVBLGRB2(505)      &
     &                      /1,'TEMP AT 10 M        ',011,105,     &
     &                       'TMP ON spec_hgt_lvl_above_grnd' /
      DATA IFILV(506),AVBL(506),IQ(506),IS(506),AVBLGRB2(506)      &
     &                      /1,'U COMP MAX 10 M WIND',253,105,     &
     &                       'MAXUW ON spec_hgt_lvl_above_grnd' /
      DATA IFILV(507),AVBL(507),IQ(507),IS(507),AVBLGRB2(507)      &
     &                      /1,'V COMP MAX 10 M WIND',254,105,     &
     &                       'MAXVW ON spec_hgt_lvl_above_grnd' /
      DATA IFILV(508),AVBL(508),IQ(508),IS(508),AVBLGRB2(508)      &
     &                      /1,'MAX PRECIP RATE     ',059,001,     &
     &                       'MAX PRATE ON surface' /
      DATA IFILV(509),AVBL(509),IQ(509),IS(509),AVBLGRB2(509)      &
     &                      /1,'MAX FROZ PRECIP RATE',064,001,     &
     &                       'MAX FROZ PRATE ON surface' /
      DATA IFILV(513),AVBL(513),IQ(513),IS(513),AVBLGRB2(513)      &
     &                      /1,'AV CANOPY WATER EVAP',200,001,     &
     &                       'AVE EVCW ON surface'/
      DATA IFILV(514),AVBL(514),IQ(514),IS(514),AVBLGRB2(514)      &
     &                      /1,'DIRECT SOIL EVAP    ',199,001,     &
     &                       'AVE EVBS ON surface'/
      DATA IFILV(515),AVBL(515),IQ(515),IS(515),AVBLGRB2(515)      &
     &                      /1,'PLANT TRANSPIRATION ',210,001,     &
     &                       'AVE TRANS ON surface'/
      DATA IFILV(516),AVBL(516),IQ(516),IS(516),AVBLGRB2(516)      &
     &                      /1,'SNOW SUBLIMATION    ',198,001,     &
     &                       'AVE SBSNO ON surface'/
      DATA IFILV(517),AVBL(517),IQ(517),IS(517),AVBLGRB2(517)      &
     &                      /1,'AVE POTENTIAL EVAP  ',145,001,     &
     &                       'AVE PEVPR ON surface'/

!
! Reserving Index 550-600 for Jun Wang
!

!
!for grid2
!-- ADD for INST CRAIN, CSNOW,CICEP,CFRZR  (CRAIN 160)
      DATA IFILV(551),AVBL(551),IQ(551),IS(551),AVBLGRB2(551)      &
     &                      /1,'INST CATEG SNOW     ',143,001,    &
     &                       'INST CSNOW ON surface'/
      DATA IFILV(552),AVBL(552),IQ(552),IS(552),AVBLGRB2(552)      &
     &                      /1,'INST CATEG ICE PELLE',142,001,    &
     &                       'INST CICEP ON surface'/
      DATA IFILV(553),AVBL(553),IQ(553),IS(553),AVBLGRB2(553)      &
     &                      /1,'INST CATEG FRZ RAIN ',141,001,    &
     &                       'INST CFRZR ON surface'/
!
!-- ADD for AVE CSNOW,CICEP,CFRZR, (CRAIN 317)
      DATA IFILV(555),AVBL(555),IQ(555),IS(555),AVBLGRB2(555)      &
     &                      /1,'AVE CATEG SNOW     ',143,001,     &
     &                       'AVE CSNOW ON surface'/
      DATA IFILV(556),AVBL(556),IQ(556),IS(556),AVBLGRB2(556)      &
     &                      /1,'AVE CATEG ICE PELLE',142,001,     &
     &                       'AVE CICEP ON surface'/
      DATA IFILV(557),AVBL(557),IQ(557),IS(557),AVBLGRB2(557)      &
     &                      /1,'AVE CATEG FRZ RAIN ',141,001,     &
     &                       'AVE CFRZR ON surface'/
!
!-- ADD for INST CRAIN, CSNOW,CICEP,CFRZR  (CRAIN 160)
      DATA IFILV(559),AVBL(559),IQ(559),IS(559),AVBLGRB2(559)      &
     &                      /1,'INST CATEG SNOW     ',143,001,     &
     &                       'INST CSNOW ON surface'/
      DATA IFILV(560),AVBL(560),IQ(560),IS(560),AVBLGRB2(560)      &
     &                      /1,'INST CATEG ICE PELLE',142,001,     &
     &                       'INST CICEP ON surface'/
      DATA IFILV(561),AVBL(561),IQ(561),IS(561),AVBLGRB2(561)      &
     &                      /1,'INST CATEG FRZ RAIN ',141,001,     &
     &                       'INST CFRZR ON surface'/
!
!-- ADD for AVE CSNOW,CICEP,CFRZR, (CRAIN 317)
      DATA IFILV(563),AVBL(563),IQ(563),IS(563),AVBLGRB2(563)      &
     &                      /1,'AVE CATEG SNOW      ',143,001,     &
     &                       'AVE CSNOW ON surface'/
      DATA IFILV(564),AVBL(564),IQ(564),IS(564),AVBLGRB2(564)      &
     &                      /1,'AVE CATEG ICE PELLET',142,001,     &
     &                       'AVE CICEP ON surface'/
      DATA IFILV(565),AVBL(565),IQ(565),IS(565),AVBLGRB2(565)      &
     &                      /1,'AVE CATEG FRZ RAIN  ',141,001,     &
     &                       'AVE CFRZR ON surface'/
!
!-- ADD BEST CAPE CIN
      DATA IFILV(566),AVBL(566),IQ(566),IS(566),AVBLGRB2(566)      &
     &                      /1,'BEST CAPE ON PRESLEV',157,116,     &
     &                       'CAPE ON spec_pres_above_grnd'/
      DATA IFILV(567),AVBL(567),IQ(567),IS(567),AVBLGRB2(567)      &
     &                      /1,'BEST CIN ON PRESLEV ',156,116,     &
     &                       'CIN ON spec_pres_above_grnd'/
!-- add PRES Mean sea level---gfs
      DATA IFILV(568),AVBL(568),IQ(568),IS(568),AVBLGRB2(568)      &
     &                      /1,'PRES ON MEAN SLP    ',001,102,     &
     &                       'PRES ON mean_sea_lvl'/
!-- add T_CDC on convective_cloud_lyr  --gfs
      DATA IFILV(569),AVBL(569),IQ(569),IS(569),AVBLGRB2(569)      &
     &                      /1,'AVG CNVCT CLDLY FRAC',072,200,     &
     &                       'AVE T_CDC ON convective_cloud_lyr'/
!-- add T_CDC on entire_atmos_single_lyr  --gfs
      DATA IFILV(570),AVBL(570),IQ(570),IS(570),AVBLGRB2(570)      &
     &                      /1,'CONV CLOUD FRACTION ',072,200,     &
     &                       'T_CDC ON convective_cloud_lyr'/
!
!-- GFS use different kpds5 and kpds6 for surface lifted index,
!-- best lifted index(surface), soil temperature(TMP),
!-- total column cloud water
      DATA IFILV(571),AVBL(571),IQ(571),IS(571),AVBLGRB2(571)      &
     &                      /1,'BOTTOM SOIL TEMP    ',085,111,     &
     &                       'TMP ON depth_bel_land_sfc'/
! Chuang: remove this redendent index after communicating with Smirnova
!      DATA IFILV(572),AVBL(572),IQ(572),IS(572),AVBLGRB2(572)      &
!     &                      /1,'LIFTED INDEX--SURFCE',131,101,     &
!     &                       'LFT_X ON surface'/
      DATA IFILV(573),AVBL(573),IQ(573),IS(573),AVBLGRB2(573)      &
     &                      /1,'LIFTED INDEX--BEST  ',132,116,     &
     &                       '4LFTX ON surface'/
      DATA IFILV(574),AVBL(574),IQ(574),IS(574),AVBLGRB2(574)      &
     &                      /1,'GFS SOIL TEMPERATURE',085,112,     &
     &                       'TMP ON depth_bel_land_sfc'/
      DATA IFILV(575),AVBL(575),IQ(575),IS(575),AVBLGRB2(575)      &
     &                      /1,'TOTAL COLUMN CLD WTR',136,200,     &
     &                       'C_WAT ON entire_atmos_single_lyr'/
!-- NMMB grib2
      DATA IFILV(576),AVBL(576),IQ(576),IS(576),AVBLGRB2(576)      &
     &                      /1,'UWD AT FDHEIGHTS HGT',033,105,     &
     &                       'U_GRD ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(577),AVBL(577),IQ(577),IS(577),AVBLGRB2(577)      &
     &                      /1,'VWD AT FDHEIGHTS HGT',034,105,     &
     &                       'V_GRD ON spec_hgt_lvl_above_grnd'/
!grib2: use same fld #451 for two diffent level types
      DATA IFILV(578),AVBL(578),IQ(578),IS(578),AVBLGRB2(578)      &
     &                      /1,'SPFH AT FDHEIGHT HGT',051,105,     &
     &                       'SPF_H ON spec_hgt_lvl_above_grnd'/
!grib2: use same fld #482 for two diffent level types
      DATA IFILV(579),AVBL(579),IQ(579),IS(579),AVBLGRB2(579)      &
     &                      /1,'PRES AT FDHEIGHT HGT',001,105,     &
                             'PRES ON spec_hgt_lvl_above_grnd'/
      DATA IFILV(580),AVBL(580),IQ(580),IS(580),AVBLGRB2(580)      &
     &                      /1,'ICING AT FD HEIGHTS ',168,103,     &
                             'ICI ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(581),AVBL(581),IQ(581),IS(581),AVBLGRB2(581)      &
     &                      /1,'RADAR DERIVED VIL   ',206,200,     &
                             'VIL ON entire_atmos'/ 
!
!-- ADD mixed layer CAPE CIN
      DATA IFILV(582),AVBL(582),IQ(582),IS(582),AVBLGRB2(582)      &
     &                      /1,'MIXED LAYER CAPE    ',157,116,     &
     &                       'CAPE ON spec_pres_above_grnd'/
      DATA IFILV(583),AVBL(583),IQ(583),IS(583),AVBLGRB2(583)      &
     &                      /1,'MIXED LAYER CIN     ',156,116,     &
     &                       'CIN ON spec_pres_above_grnd'/
!-- ADD MOST UNSTABLE CAPE/CIN -LOWEST 300 MB
      DATA IFILV(584),AVBL(584),IQ(584),IS(584),AVBLGRB2(584)      &
     &                      /1,'MOST UNSTABLE CAPE  ',157,116,     &
     &                       'CAPE ON spec_pres_above_grnd'/
      DATA IFILV(585),AVBL(585),IQ(585),IS(585),AVBLGRB2(585)      &
     &                      /1,'MOST UNSTABLE CIN   ',156,116,     &
     &                       'CIN ON spec_pres_above_grnd'/
!-- tmp at fd hgt (specified hgt level above ground)
      DATA IFILV(586),AVBL(586),IQ(586),IS(586),AVBLGRB2(586)      &
     &                      /1,'TEMP AT FDHEIGHT HGT',011,105,     &
     &                       'TMP ON spec_hgt_above_mean_sea_lvl'/
!icing at fd hgt (specified hgt level above ground)
      DATA IFILV(587),AVBL(587),IQ(587),IS(587),AVBLGRB2(587)      &
     &                      /1,'ICING FDHEIGHT HGT  ',168,105,     &
                             'ICI ON spec_alt_above_mean_sea_lvl'/

! Reserving Index 550-600 for grib2

! Reserving Index 601-700 for GOCART
! ADD DUST AT FD HEIGHTS (GOCART)
      DATA IFILV(601),AVBL(601),IQ(601),IS(601),AVBLGRB2(601)      &
     &                      /1,'DUST 1 AT FD HEIGHTS',240,103,     &
                             'DU1 ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(602),AVBL(602),IQ(602),IS(602),AVBLGRB2(602)      &
     &                      /1,'DUST 2 AT FD HEIGHTS',241,103,     &
                             'DU2 ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(603),AVBL(603),IQ(603),IS(603),AVBLGRB2(603)      &
     &                      /1,'DUST 3 AT FD HEIGHTS',242,103,     &
                             'DU3 ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(604),AVBL(604),IQ(604),IS(604),AVBLGRB2(604)      &
     &                      /1,'DUST 4 AT FD HEIGHTS',243,103,     &
                             'DU4 ON spec_alt_above_mean_sea_lvl'/
      DATA IFILV(605),AVBL(605),IQ(605),IS(605),AVBLGRB2(605)      &
     &                      /1,'DUST 5 AT FD HEIGHTS',244,103,     &
                             'DU5 ON spec_alt_above_mean_sea_lvl'/
! ADD AEROSOL OPTICAL PROPERTIES (GOCART)
      DATA IFILV(606),AVBL(606),IQ(606),IS(606),AVBLGRB2(606)      &
     &                      /1,'AEROSOL EXTINCTION  ',128,109,     &
                             'Aerosol extinction coefficient    '/
      DATA IFILV(607),AVBL(607),IQ(607),IS(607),AVBLGRB2(607)      &
     &                      /1,'AER ASYMMETRY FACTOR',130,109,     &
                             'Aerosol asymmetry factor          '/
      DATA IFILV(608),AVBL(608),IQ(608),IS(608),AVBLGRB2(608)      &
     &                      /1,'SINGLE SCATTER ALBD ',131,109,     &
                             'Aerosol single scatter albedo     '/
! ADD AEROSOL OPTICAL DEPTH AT 550 NM (GOCART)
      DATA IFILV(609),AVBL(609),IQ(609),IS(609),AVBLGRB2(609)      &
     &                      /1,'AER OPT DEP AT 550  ',129,200,     &
                             'Total aerosol optical depth at 550'/
      DATA IFILV(610),AVBL(610),IQ(610),IS(610),AVBLGRB2(610)      &
     &                      /1,'DU AER OPT DEP 550  ',133,200,     &
                             'Dust aerosol optical depth at 550 '/
      DATA IFILV(611),AVBL(611),IQ(611),IS(611),AVBLGRB2(611)      &
     &                      /1,'SS AER OPT DEP 550  ',134,200,     &
                             'Seasalt aer optical depth at 550  '/
      DATA IFILV(612),AVBL(612),IQ(612),IS(612),AVBLGRB2(612)      &
     &                      /1,'SU AER OPT DEP 550  ',135,200,     &
                             'Sulfate aer optical depth at 550  '/
      DATA IFILV(613),AVBL(613),IQ(613),IS(613),AVBLGRB2(613)      &
     &                      /1,'OC AER OPT DEP 550  ',136,200,     &
                             'Organic carbon aer opt dep at 550 '/
      DATA IFILV(614),AVBL(614),IQ(614),IS(614),AVBLGRB2(614)      &
     &                      /1,'BC AER OPT DEP 550  ',137,200,     &
                             'Black carbon aer opt dep at 550   '/
! ADD DUST PRODUCTION AND REMOVAL FLUXES (GOCART)
      DATA IFILV(615),AVBL(615),IQ(615),IS(615),AVBLGRB2(615)      &
     &                      /1,'DUST EMISSION FLUX  ',151,200,     &
                             'dust emission fluxes              '/
      DATA IFILV(616),AVBL(616),IQ(616),IS(616),AVBLGRB2(616)      &
     &                      /1,'DUST SEDIMENTATION  ',152,200,     &
                             'dust sedimentation fluxes         '/
      DATA IFILV(617),AVBL(617),IQ(617),IS(617),AVBLGRB2(617)      &
     &                      /1,'DUST DRY DEPOSITION ',153,200,     &
                             'dust dry deposition fluxes        '/
      DATA IFILV(618),AVBL(618),IQ(618),IS(618),AVBLGRB2(618)      &
     &                      /1,'DUST WET DEPOSITION ',154,200,     &
                             'dust wet deposition fluxes        '/
! ADD AEROSOL SURFACE MASS CONCENTRATION (GOCART), use table 129
      DATA IFILV(619),AVBL(619),IQ(619),IS(619),AVBLGRB2(619)      &
     &                      /1,'CR AER SFC MASS CON ',156,001,     &
                             'coarse aer sfc mass concentration '/
      DATA IFILV(620),AVBL(620),IQ(620),IS(620),AVBLGRB2(620)      &
     &                      /1,'FN AER SFC MASS CON ',157,001,     &
                             'fine aer sfc mass concentration   '/
! ADD AEROSOL COLUMN MASS DENSITY  (GOCART)
      DATA IFILV(621),AVBL(621),IQ(621),IS(621),AVBLGRB2(621)      &
     &                      /1,'CR AER COL MASS DEN ',155,200,     &
                             'coarse aerosol col mass density   '/
      DATA IFILV(622),AVBL(622),IQ(622),IS(622),AVBLGRB2(622)      &
     &                      /1,'FN AER COL MASS DEN ',158,200,     &
                             'fine aerosol col mass density     '/
! ADD AEROSOL OPTICAL DEPTH AT OTHER CHANNELS (GOCART)
      DATA IFILV(623),AVBL(623),IQ(623),IS(623),AVBLGRB2(623)      &
     &                      /1,'AER OPT DEP AT 340  ',129,200,     &
                             'Total aerosol optical depth at 340'/
      DATA IFILV(624),AVBL(624),IQ(624),IS(624),AVBLGRB2(624)      &
     &                      /1,'AER OPT DEP AT 440  ',129,200,     &
                             'Total aerosol optical depth at 440'/
      DATA IFILV(625),AVBL(625),IQ(625),IS(625),AVBLGRB2(625)      &
     &                      /1,'AER OPT DEP AT 660  ',129,200,     &
                             'Total aerosol optical depth at 660'/
      DATA IFILV(626),AVBL(626),IQ(626),IS(626),AVBLGRB2(626)      &
     &                      /1,'AER OPT DEP AT 860  ',129,200,     &
                             'Total aerosol optical depth at 860'/
      DATA IFILV(627),AVBL(627),IQ(627),IS(627),AVBLGRB2(627)      &
     &                      /1,'AER OPT DEP AT 1630 ',129,200,     &
                             'Total aer optical depth at 1630   '/
      DATA IFILV(628),AVBL(628),IQ(628),IS(628),AVBLGRB2(628)      &
     &                      /1,'AER OPT DEP AT 11100',129,200,     &
                             'Total aer optical dep at 11 micron'/
! ADD DUST FIELDS ON MDL SFCS (GOCART)
      DATA IFILV(629),AVBL(629),IQ(629),IS(629),AVBLGRB2(629)      &
     &                      /1,'DUST 1 ON MDL SFCS  ',240,109,     &
     &                       'DU1 ON hybrid_lvl'/
      DATA IFILV(630),AVBL(630),IQ(630),IS(630),AVBLGRB2(630)      &
     &                      /1,'DUST 2 ON MDL SFCS  ',241,109,     &
     &                       'DU2 ON hybrid_lvl'/
      DATA IFILV(631),AVBL(631),IQ(631),IS(631),AVBLGRB2(631)      &
     &                      /1,'DUST 3 ON MDL SFCS  ',242,109,     &
     &                       'DU3 ON hybrid_lvl'/
      DATA IFILV(632),AVBL(632),IQ(632),IS(632),AVBLGRB2(632)      &
     &                      /1,'DUST 4 ON MDL SFCS  ',243,109,     &
     &                       'DU4 ON hybrid_lvl'/
      DATA IFILV(633),AVBL(633),IQ(633),IS(633),AVBLGRB2(633)      &
     &                      /1,'DUST 5 ON MDL SFCS  ',244,109,     &
     &                       'DU5 ON hybrid_lvl'/
! ADD NON-DUST AEROSOL FIELDS ON MDL SFCS (GOCART)
      DATA IFILV(634),AVBL(634),IQ(634),IS(634),AVBLGRB2(634)      &
     &                      /1,'SEASALT 1 ON MDL SFC',245,109,     &
     &                       'SS1 ON hybrid_lvl'/
      DATA IFILV(635),AVBL(635),IQ(635),IS(635),AVBLGRB2(635)      &
     &                      /1,'SEASALT 2 ON MDL SFC',246,109,     &
     &                       'SS2 ON hybrid_lvl'/
      DATA IFILV(636),AVBL(636),IQ(636),IS(636),AVBLGRB2(636)      &
     &                      /1,'SEASALT 3 ON MDL SFC',247,109,     &
     &                       'SS3 ON hybrid_lvl'/
      DATA IFILV(637),AVBL(637),IQ(637),IS(637),AVBLGRB2(637)      &
     &                      /1,'SEASALT 4 ON MDL SFC',248,109,     &
     &                       'SS4 ON hybrid_lvl'/
      DATA IFILV(638),AVBL(638),IQ(638),IS(638),AVBLGRB2(638)      &
     &                      /1,'SEASALT 0 ON MDL SFC',253,109,     &
     &                       'SS0 ON hybrid_lvl'/
      DATA IFILV(639),AVBL(639),IQ(639),IS(639),AVBLGRB2(639)      &
     &                      /1,'SULFATE ON MDL SFC  ',254,109,     &
     &                       'SO4 ON hybrid_lvl'/
      DATA IFILV(640),AVBL(640),IQ(640),IS(640),AVBLGRB2(640)      &
     &                      /1,'OC DRY ON MDL SFC   ',249,109,     &
     &                       'OC_DRY hybrid_lvl'/
      DATA IFILV(641),AVBL(641),IQ(641),IS(641),AVBLGRB2(641)      &
     &                      /1,'OC WET ON MDL SFC   ',250,109,     &
     &                       'OC_WET hybrid_lvl'/
      DATA IFILV(642),AVBL(642),IQ(642),IS(642),AVBLGRB2(642)      &
     &                      /1,'BC DRY ON MDL SFC   ',251,109,     &
     &                       'BC_DRY hybrid_lvl'/
      DATA IFILV(643),AVBL(643),IQ(643),IS(643),AVBLGRB2(643)      &
     &                      /1,'BC WET ON MDL SFC   ',252,109,     &
     &                       'BC_WET hybrid_lvl'/
! ADD AIR DENSITY AND LAYER THICKNESS
      DATA IFILV(644),AVBL(644),IQ(644),IS(644),AVBLGRB2(644)      &
     &                      /1,'AIR DEN ON MDL SFCS ',189,109,     &
     &                       'AIRDEN hybrid_lvl'/
      DATA IFILV(645),AVBL(645),IQ(645),IS(645),AVBLGRB2(645)      &
     &                      /1,'DPRES ON MDL SFCS   ',1,110,     &
     &                       'DPRES hybrid_lvl '/
! Reserving Index 601-700 for GOCART
!

! Reserve index 700-799 for GSD
! Chuang: remove DUST 1-5 output from GSD because GOCART also outputs
! the same variables above
      DATA IFILV(771),AVBL(771),IQ(771),IS(771),AVBLGRB2(771)      &
     &                      /1,'FIRST LEVEL DEWPOINT',017,105,     &
     &                       'DPT ON spec_hgt_lvl_above_grnd'/

! GSD HRRR-CHEM output
      DATA IFILV(720),AVBL(720),IQ(720),IS(720),AVBLGRB2(720)      &
     &                      /1,'PM 2.5 ON MDL SFCS  ',240,107,   &
     &                         'PM 2.5 ON MDL SFCS  '/
      DATA IFILV(721),AVBL(721),IQ(721),IS(721),AVBLGRB2(721)      &
     &                      /1,'PM 10 ON MDL SFCS   ',241,107,   &
     &                         'PM 10 ON MDL SFCS   '/
      DATA IFILV(722),AVBL(722),IQ(722),IS(722),AVBLGRB2(722)      &
     &                      /1,'SO2 ON MDL SFCS     ',242,107,   &
     &                         'SO2 ON MDL SFCS     '/
      DATA IFILV(723),AVBL(723),IQ(723),IS(723),AVBLGRB2(723)      &
     &                      /1,'PM 2.5 ON P SFCS    ',240,100,   &
     &                         'PM 2.5 ON MDL SFCS  '/
! no entry in GRIB table for NCLOUD use 147 - Zonal flux of gravity wave stress
      DATA IFILV(747),AVBL(747),IQ(747),IS(747),AVBLGRB2(747)      &    
     &                      /1,'NCCLOUD ON MDL SFCS ',147,109,     &    
     &                         'NCCLOUD ON MDL SFCS '/
      DATA IFILV(750),AVBL(750),IQ(750),IS(750),AVBLGRB2(750)      &
     &                      /1,'WV MIX R ON MDL SFCS',053,109,     &
     &                         'WV MIX R ON MDL SFCS'/
      DATA IFILV(751),AVBL(751),IQ(751),IS(751),AVBLGRB2(751)      &
     &                      /1,'VP TEMP ON MDL SFCS ',189,109,     &
     &                         'VP TEMP ON MDL SFCS '/
      DATA IFILV(752),AVBL(752),IQ(752),IS(752),AVBLGRB2(752)      &
     &                      /1,'NCICE ON MDL SFCS   ',198,109,     &
     &                         'NCICE ON MDL SFCS   '/

! no entry in GRIB table for NRAIN, use 148 - Meridional flux of gravity wave stress
      DATA IFILV(754),AVBL(754),IQ(754),IS(754),AVBLGRB2(754)      &
     &                      /1,'NCRAIN ON MDL SFCS  ',148,109,     &
     &                         'NCRAIN ON MDL SFCS  '/
! water friendly aerosol: entry 157, Table 129
      DATA IFILV(766),AVBL(766),IQ(766),IS(766),AVBLGRB2(766)      &
     &                      /1,'NWFA ON MDL SFCS    ',157,109,     &
     &                         'NWFA ON MDL SFCS    '/
! ice friendly aerosol: entry 156, Table 129
      DATA IFILV(767),AVBL(767),IQ(767),IS(767),AVBLGRB2(767)      &
     &                      /1,'NIFA ON MDL SFCS    ',156,109,     &
     &                         'NIFA ON MDL SFCS    '/

! ---
      DATA IFILV(546),AVBL(546),IQ(546),IS(546),AVBLGRB2(546)      &
     &                      /1,'SHELTER POT TEMP    ',013,105,     &
     &                         'SHELTER POT TEMP    '/
      DATA IFILV(547),AVBL(547),IQ(547),IS(547),AVBLGRB2(547)      &
     &                      /1,'SHELTER DEWP DEPRES ',018,105,     &
     &                         'SHELTER DEWP DEPRES '/
      DATA IFILV(548),AVBL(548),IQ(548),IS(548),AVBLGRB2(548)      &
     &                      /1,'SURFACE EQ POT TEMP ',014,001,     &
     &                         'SURFACE EQ POT TEMP '/
      DATA IFILV(755),AVBL(755),IQ(755),IS(755),AVBLGRB2(755)      &
     &                      /1,'EQUIL LEVEL HEIGHT  ',007,247,     &
     &                         'EQUIL LEVEL HEIGHT  '/
      DATA IFILV(753),AVBL(753),IQ(753),IS(753),AVBLGRB2(753)      &
     &                      /1,'PRESSURE OF FRZ LVL ',001,004,     &
     &                         'PRESSURE OF FRZ LVL '/
      DATA IFILV(756),AVBL(756),IQ(756),IS(756),AVBLGRB2(756)      &
     &                      /1,'HIGHEST FRZ LVL PRES',001,204,     &
     &                         'HIGHEST FRZ LVL PRES'/
      DATA IFILV(700),AVBL(700),IQ(700),IS(700),AVBLGRB2(700)      &
     &                      /1,'MAX UPDR HELICITY16 ',216,106,     &
     &                         'MAX UPDR HELICITY16 '/
      DATA IFILV(701),AVBL(701),IQ(701),IS(701),AVBLGRB2(701)      &
     &                      /1,'UPDRAFT HELICITY16  ',214,106,     &
     &                         'UPDRAFT HELICITY16  '/
      DATA IFILV(702),AVBL(702),IQ(702),IS(702),AVBLGRB2(702)      &
     &                      /1,'MAX LTG THREAT1     ',188,200,     &
     &                         'MAX LTG THREAT1     '/
      DATA IFILV(703),AVBL(703),IQ(703),IS(703),AVBLGRB2(703)      &
     &                      /1,'MAX LTG THREAT2     ',186,200,     &
     &                         'MAX LTG THREAT2     '/
      DATA IFILV(704),AVBL(704),IQ(704),IS(704),AVBLGRB2(704)      &
     &                      /1,'MAX LTG THREAT3     ',187,200,     &
     &                         'MAX LTG THREAT3     '/
      DATA IFILV(705),AVBL(705),IQ(705),IS(705),AVBLGRB2(705)      &
     &                      /1,'NCI_LTG             ',241,200,     &
     &                         'NCI_LTG             '/
      DATA IFILV(706),AVBL(706),IQ(706),IS(706),AVBLGRB2(706)      &
     &                      /1,'NCA_LTG             ',242,200,     &
     &                         'NCA_LTG             '/
      DATA IFILV(707),AVBL(707),IQ(707),IS(707),AVBLGRB2(707)      &
     &                      /1,'NCI_WQ              ',243,200,     &
     &                         'NCI_WQ              '/
      DATA IFILV(708),AVBL(708),IQ(708),IS(708),AVBLGRB2(708)      &
     &                      /1,'NCA_WQ              ',244,200,     &
     &                         'NCA_WQ              '/
      DATA IFILV(709),AVBL(709),IQ(709),IS(709),AVBLGRB2(709)      &
     &                      /1,'NCI_REFL            ',245,200,     &
     &                         'NCI_REFL            '/
      DATA IFILV(710),AVBL(710),IQ(710),IS(710),AVBLGRB2(710)      &
     &                      /1,'NCA_REFL            ',246,200,     &
     &                         'NCA_REFL            '/

! Add variables to produce the same output as in RUC
      DATA IFILV(749),AVBL(749),IQ(749),IS(749),AVBLGRB2(749)      &
     &                      /1,'RH WRT PRECIP WATER ',230,200,     &
     &                         'RH WRT PRECIP WATER '/
      DATA IFILV(748),AVBL(748),IQ(748),IS(748),AVBLGRB2(748)      &
     &                      /1,'RADAR REFLECT - 1km ',211,105,     &
     &                         'RADAR REFLECT - 1km '/
      DATA IFILV(757),AVBL(757),IQ(757),IS(757),AVBLGRB2(757)      &
     &                      /1,'RADAR REFLECT - 4km ',211,105,     &
     &                         'RADAR REFLECT - 4km '/
      DATA IFILV(758),AVBL(758),IQ(758),IS(758),AVBLGRB2(758)      &
     &                      /1,'CONV CLD TOP HGHT   ',007,243,     &
     &                         'CONV CLD TOP HGHT   '/
      DATA IFILV(760),AVBL(760),IQ(760),IS(760),AVBLGRB2(760)      &
     &                      /1,'SHELTER MIXING RATIO',053,105,     &
     &                         'SHELTER MIXING RATIO'/
      DATA IFILV(762),AVBL(762),IQ(762),IS(762),AVBLGRB2(762)      &
     &                      /1,'SURFACE MIXING RATIO',053,001,     &
     &                         'SURFACE MIXING RATIO'/
      DATA IFILV(761),AVBL(761),IQ(761),IS(761),AVBLGRB2(761)      &
     &                      /1,'TEMP INSIDE SNOW    ',011,001,     &
     &                         'TEMP INSIDE SNOW    '/
! CRA Add variables to produce NCAR fields
      DATA IFILV(768),AVBL(768),IQ(768),IS(768),AVBLGRB2(768)      &
     &                      /1,'ECHOTOP             ',240,200,     &
     &                         'ECHOTOP             '/
      DATA IFILV(769),AVBL(769),IQ(769),IS(769),AVBLGRB2(769)      &
     &                      /1,'VIL                 ',206,200,     &
     &                         'VIL                 '/
      DATA IFILV(770),AVBL(770),IQ(770),IS(770),AVBLGRB2(770)      &
     &                      /1,'RADARVIL            ',206,200,     &
     &                         'RADARVIL            '/
      DATA IFILV(727),AVBL(727),IQ(727),IS(727),AVBLGRB2(727)      &
     &                      /1,'GSD UPDRAFT HELICITY',227,106,     &
     &                       'GSD UPHL ON spec_hgt_lvl_above_grnd'/
! CRA
! CRA
! RAP/HRRR Time-averaged variables
      DATA IFILV(730),AVBL(730),IQ(730),IS(730),AVBLGRB2(730)      &
     &                      /1,'AVE 10m WIND SPEED  ',229,105,     &
     &                       'AVE WIND ON 10M spec_hgt_lvl_above_grnd'/    !422
      DATA IFILV(731),AVBL(731),IQ(731),IS(731),AVBLGRB2(731)      &
     &                      /1,'AVE 10m U           ',229,105,     &
     &                       'AVE WIND ON 10M spec_hgt_lvl_above_grnd'/    !422
      DATA IFILV(732),AVBL(732),IQ(732),IS(732),AVBLGRB2(732)      &
     &                      /1,'AVE 10m V           ',229,105,     &
     &                       'AVE WIND ON 10M spec_hgt_lvl_above_grnd'/    !422
      DATA IFILV(733),AVBL(733),IQ(733),IS(733),AVBLGRB2(733)      &
     &                      /1,'AVE INCOMING SW RAD ',204,001,     &
     &                       'AVE NSWRF ON surface'/
      DATA IFILV(734),AVBL(734),IQ(734),IS(734),AVBLGRB2(734)      &
     &                      /1,'AVE NORMAL SW RAD   ',254,001,     &
     &                       'AVE NSWRF ON surface'/
! E. James
! 11 May 2015
! Adding instantaneous direct normal and diffuse horizontal irradiance
      DATA IFILV(772),AVBL(772),IQ(772),IS(772),AVBLGRB2(772)      &
     &                      /1,'INSTN DIR NOR IRRAD ',166,001,     &
     &                       'INST SWDDNI ON surface'/
      DATA IFILV(773),AVBL(773),IQ(773),IS(773),AVBLGRB2(773)      &
     &                      /1,'INSTN DIF HOR IRRAD ',167,001,     &
     &                       'INST SWDDIF ON surface'/
! E. James
! 28 Mar 2016
! Adding clear-sky surface up and downwelling short and longwave irradiance
      DATA IFILV(742),AVBL(742),IQ(742),IS(742),AVBLGRB2(742)      &
     &                      /1,'INSTN CLRSKY SHWV DN',161,001,     &
     &                       'INST SWDNBC ON surface'/
      DATA IFILV(743),AVBL(743),IQ(743),IS(743),AVBLGRB2(743)      &
     &                      /1,'INSTN CLRSKY SHWV UP',160,001,     &
     &                       'INST SWUPBC ON surface'/
      DATA IFILV(744),AVBL(744),IQ(744),IS(744),AVBLGRB2(744)      &
     &                      /1,'INSTN CLRSKY LGWV DN',163,001,     &
     &                       'INST LWDNBC ON surface'/
      DATA IFILV(745),AVBL(745),IQ(745),IS(745),AVBLGRB2(745)      &
     &                      /1,'INSTN CLRSKY LGWV UP',162,001,     &
     &                       'INST LWUPBC ON surface'/
!
! satellite index 800-899

!    2014-12-09 WM LEWIS ADDED SSMI_F13-F15, SSMIS_F16-F20
!    WITH LVLS-DRIVEN CONTROL OF CHANNEL SELECTION
!    SSMI_F13 (L(1)-L(6) -> ID8: 176-181 -> 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(800),AVBL(800),IQ(800),IS(800),AVBLGRB2(800)      &
     &                      /1,'F13 SSMI NON-NADIR  ',118,109,     &
     &                       'SSMI TB top_of_atmos  '/ !table 133

!    SSMI_F14 (L(1)-L(6) -> ID8: 182-187 -> 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(806),AVBL(806),IQ(806),IS(806),AVBLGRB2(806)      &
     &                      /1,'F14 SSMI NON-NADIR  ',118,109,     &
     &                       'SSMI TB top_of_atmos  '/ !table 133

!    SSMI_F15 (L(1)-L(6) -> ID8: 188-193 -> 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(812),AVBL(812),IQ(812),IS(812),AVBLGRB2(812)      &
     &                      /1,'F15 SSMI NON-NADIR  ',118,109,     &
     &                       'SSMI TB top_of_atmos  '/ !table 133

!    SSMIS_F16 (L(1)-L(7) -> ID8: 194-200 -> 183H, 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(818),AVBL(818),IQ(818),IS(818),AVBLGRB2(818)      &
     &                      /1,'F16 SSMIS NON-NADIR ',118,109,     &
     &                       'SSMIS TB top_of_atmos '/ !table 133

!    SSMIS_F17 (L(1)-L(7) -> ID8: 201-207 -> 183H, 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(825),AVBL(825),IQ(825),IS(825),AVBLGRB2(825)      &
     &                      /1,'F17 SSMIS NON-NADIR ',118,109,     &
     &                       'SSMIS TB top_of_atmos '/ !table 133

!    SSMIS_F18 (L(1)-L(7) -> ID8: 208-214 -> 183H, 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(832),AVBL(832),IQ(832),IS(832),AVBLGRB2(832)      &
     &                      /1,'F18 SSMIS NON-NADIR ',118,109,     &
     &                       'SSMIS TB top_of_atmos '/ !table 133

!    SSMIS_F19 (L(1)-L(7) -> ID8: 215-221 -> 183H, 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(839),AVBL(839),IQ(839),IS(839),AVBLGRB2(839)      &
     &                      /1,'F19 SSMIS NON-NADIR ',118,109,     &
     &                       'SSMIS TB top_of_atmos '/ !table 133

!    SSMIS_F20 (L(1)-L(7) -> ID8: 222-228 -> 183H, 19H, 19V, 37H, 37V, 85H, 85V)
      DATA IFILV(846),AVBL(846),IQ(846),IS(846),AVBLGRB2(846)      &
     &                      /1,'F20 SSMIS NON-NADIR ',118,109,     &
     &                       'SSMIS TB top_of_atmos '/ !table 133

! Apparent Temperature, 
! March 2013: use faked Grib1 and Grib2 IDs
! Sib will no longer support new Grib1 ID and
! is in the process of getting Grib2 ID from WMO.
      DATA IFILV(808),AVBL(808),IQ(808),IS(808),AVBLGRB2(808)      &
     &                      /1,'APPARENT TEMPERATURE',168,105,     &
     &                       'TIPD ON entire_atmos_single_lyr'/

!     2014-12-09 WM LEWIS MODIFIED MTSTAT-2 and MTSAT-1r to COMPLY WITH NEW
!     LVLS-DRIVEN CHANNEL SELECTION (L(1)-L(4)> CH1-CH4)
!     HWRF satellite additions: MTSAT-2 imager:
      DATA IFILV(860),AVBL(860),IQ(860),IS(860),AVBLGRB2(860)  &
     &                      /1,'MTSAT2 NON-NADIR    ',118,109, & !table 130
     &                       'MTSAT2 CH1 NON NAD top_of_atmos'/

!     LVLS-DRIVEN CHANNEL SELECTION (L(1)-L(4)> CH1-CH4)
!     HWRF satellite additions: MTSAT-1r imager (MTSAT-2 backup satellite):
      DATA IFILV(864),AVBL(864),IQ(864),IS(864),AVBLGRB2(864)  &
     &                      /1,'MTSAT1R NON-NADIR   ',118,109, & !table 130
     &                       'MTSAT1RCH1 NON NAD top_of_atmos'/

!     LVLS-DRIVEN CHANNEL SELECTION (L(1)-L(4)> IR CH1-CH4)
!     HWRF satellite additions: INSAT-3D imager
      DATA IFILV(865),AVBL(865),IQ(865),IS(865),AVBLGRB2(865)  &
     &                      /1,'INSAT 3D NON-NADIR  ',118,109, & !table 130
     &                       'INSAT 3D NON NAD top_of_atmos'/

!     2014-12-09 WM LEWIS ADDED GOES-13, GOES-15, MSG-10
!     GOES-13 imager (L(1)-L(4) -> ID8: 237-240 -> CH2, CH3, CH4, CH5)
      DATA IFILV(868),AVBL(868),IQ(868),IS(868),AVBLGRB2(868)      &
     &                      /1,'GOES-13 NON-NADIR   ',118,109,     &
     &                       'GOES-13 IMGR TB TOA   '/ !Table 130

!    GOES-15 imager (L(1)-L(4) -> ID8: 241-244 -> CH2, CH3, CH4, CH5)
      DATA IFILV(872),AVBL(872),IQ(872),IS(872),AVBLGRB2(872)      &
     &                      /1,'GOES-15 NON-NADIR   ',118,109,     &
     &                       'GOES-15 IMGR TB TOA   '/ !Table 130

!    MSG/SEVIRI imager (L(1)-L(7) -> ID8: 230-236 -> 
!    CH5, CH6, CH7, CH8, CH9, CH10, CH11)
      DATA IFILV(876),AVBL(876),IQ(876),IS(876),AVBLGRB2(876)      &
     &                      /1,'SEVIRI NON-NADIR    ',118,109,     &
     &                       'MSG/SEVIRI TB TOA     '/ !Table 130

! HWRF additions (900-949)
      DATA IFILV(900),AVBL(900),IQ(900),IS(900),AVBLGRB2(900)      &
     &                      /1,'MODEL SFC U WIND STR',124,001,     &
     &                       'U_FLX ON surface' /
      DATA IFILV(901),AVBL(901),IQ(901),IS(901),AVBLGRB2(901)      &
     &                      /1,'MODEL SFC V WIND STR',125,001,     &
     &                       'V_FLX ON surface' /
      DATA IFILV(902),AVBL(902),IQ(902),IS(902),AVBLGRB2(902)      &
     &                      /1,'INSTN OUT TOA SW RAD',211,008,     &
     &                       'INST USWRF ON top_of_atmos'/
! HWRF reflectivity output from wrf
! Passed-through wrf derived variable, works for non-ferrier
! physics.
! Use Table 129 (PDS Octet 4 = 129)
      DATA IFILV(903),AVBL(903),IQ(903),IS(903),AVBLGRB2(903)      &
     &                      /1,'WRFOUT REFL 10CM MDL',211,109,     &
     &                       'WRFOUT REFL 10CM ON model '/
      DATA IFILV(904),AVBL(904),IQ(904),IS(904),AVBLGRB2(904)      &
     &                      /1,'WRFOUT COMP MAX REF ',212,200,     &
     &                       'WRFOUT COMP MAX REFLC'/
! Add Radiation variables output from RRTMG and CAM
! radiation schemes in wrf. (SWUPT,ACSWUPT,SWDNT,ACSWDNT)
      DATA IFILV(905),AVBL(905),IQ(905),IS(905),AVBLGRB2(905)      &
     &                      /1,'INST SW UP TOA RAD  ',211,008,     &
     &                       'INST SW UPWELL RAD top_of_atmos'/
      DATA IFILV(906),AVBL(906),IQ(906),IS(906),AVBLGRB2(906)      &
     &                      /1,'AVE SW UP TOA RAD   ',211,008,     &
     &                       'AVE SW UPWELL RAD top_of_atmos'/
      DATA IFILV(907),AVBL(907),IQ(907),IS(907),AVBLGRB2(907)      &
     &                      /1,'INST SW DOWN TOA RAD',204,008,     &
     &                       'INST SW DOWNWELL RAD top_of_atmos'/
      DATA IFILV(908),AVBL(908),IQ(908),IS(908),AVBLGRB2(908)      &
     &                      /1,'AVE SW DOWN TOA RAD ',204,008,     &
     &                       'AVE SW DOWNWELL RAD top_of_atmos'/
      DATA IFILV(909),AVBL(909),IQ(909),IS(909),AVBLGRB2(909)      &
     &                      /1,'VTEMP ON MDL SFCS   ',012,109,     &
     &                       'VTMP ON hybrid_lvl'/
      DATA IFILV(910),AVBL(910),IQ(910),IS(910),AVBLGRB2(910)      &
     &                      /1,'VTEMP ON PRESS SFCS ',012,100,     &
     &                       'VTMP ON isobaric_sfc'/
      DATA IFILV(911),AVBL(911),IQ(911),IS(911),AVBLGRB2(911)      &
     &                      /1,'VTEMP AT FD HEIGHTS ',012,103,     &
     &                       'VTMP ON spec_alt_above_mean_sea_lvl'/
!end initialization
!
   end module RQSTFLD_mod
