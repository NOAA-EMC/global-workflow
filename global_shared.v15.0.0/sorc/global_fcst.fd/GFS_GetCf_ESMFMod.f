!
! !MODULE: GFS_GetCf_ESMFMod --- Configure data module of the ESMF 
!                                gridded component of the GFS system.
!
! !DESCRIPTION: This program uses the ESMF configure class software
!               to get all parameters of the original namelists of the GFS
!               GFS and all trun-on/turn-off switch flags for the ESMF
!               import state and the ESMF export state.
!
! !REVISION HISTORY:
!
!  November 2004 Weiyu Yang    Initial code for ESMF GFS model.
!  May      2005 Weiyu Yang    For the updated GFS version.
!  March    2006 S. Moorthi    Modified for the new GFS.
!  September2006 Weiyu Yang    Modified for the Ensemble Coupler System.
!
! !INTERFACE:
!
 MODULE GFS_GetCf_ESMFMod

!
!!USES:
!

! The ESMF internal state contents.
!----------------------------------

! Coupling insertion->
      USE ATM_cc, ONLY: MPI_COMM_Atmos
!        All occurences of MPI_COMM_WORLD in this unit are replaced
!        by MPI_COMM_Atmos to enable coupling
!<-Coupling insertion

 USE GFS_InternalState_ESMFMod
 USE GFS_ErrMsgMod

 IMPLICIT none

 CONTAINS

!------------------------------------------------------------------------
!BOP
!
! !IROUTINE:
!
! !INTERFACE:
!
 SUBROUTINE GFS_GetCf(gcGFS, Int_State, rc)
!SUBROUTINE GFS_GetCf(gcGFS, Int_State, Total_Member, Member_Id, rc)

 TYPE(ESMF_GridComp),       INTENT(inout) :: gcGFS     ! ESMF GFS grid component.
 TYPE(GFS_InternalState),   INTENT(inout) :: Int_State ! ESMF internal state.
 TYPE(ESMF_VM)                            :: vm        ! ESMF virtual machine.
 TYPE(ESMF_VM)                            :: vm_local  ! ESMF virtual machine.
 INTEGER, OPTIONAL,         INTENT(out)   :: rc        ! Error signal variable.
!
! !DESCRIPTION: Load run parameters from the resource file into internal
!               state.

!
! !REVISION HISTORY:
!
!  November 3, 2004 Weiyu Yang, for NCEP GFS model.
!  May         2005 Weiyu Yang  for the updated GFS version.
!
!EOP
!------------------------------------------------------------------------

!INTEGER, INTENT(IN) :: Total_Member, Member_Id
 TYPE(ESMF_Config) :: cf
 INTEGER           :: i, rc1, rcfinal
 CHARACTER*20      :: clab, pelab
 integer           :: pe_member,j, i1, me, tasks, totpe


! Initialize the error signal variables.
!---------------------------------------
 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! Get the ESMF config from the GFS grid component.
!-------------------------------------------------
 CALL ESMF_GridCompGet(gcGFS, config = cf, rc = rc1)

! If error happens, set up the error signal variables and print out the error message.
!-------------------------------------------------------------------------------------
CALL ERR_MSG1(rc1, "Grid Component Get Configure", rcfinal)

! Start to get the configure data.
!---------------------------------

! The original GFS has three namelist files, "nam_gfs", "SOIL_VEG" and "NAMSFC".
! Besides to get the information from these three namelist files, the GetCf
! process will get the "ESMF_State_Namelist" which contains all turn-on/turn-off
! switch flags for all possible fields in the ESMF import and export states.
! The single ESMF config file contains all information of the four namelist files.
!---------------------------------------------------------------------------------

! For "nam_gfs".
!---------------

! "ESMF_ConfigGetAttribute" is an ESMF config utility routine which is used
! to get information from the config file.  The first argument is the GFS ESMF config.
! The second argument is the destination variable to which the value of the namelist 
! variable will be sent. The third one is the label in the config file which is used
! to identify the required namelist variable.  The last one is the error signal variable.
! "ESMF_ConfigGetAttribute" is a generic interface name which can be used to get the 
! the different data types data from the config file, such as real(4), real(8), 
! integer(4), integer(8), character(...), etc..
!----------------------------------------------------------------------------------------
 CALL ESMF_VMGetGlobal(vm, rc = rc1)
 CALL ESMF_VMGet(vm, localPet = me, peCount = tasks, rc = rc1)

 CALL ESMF_ConfigGetAttribute(Cf, Int_State%nam_gfs%Total_member,   &
                              label = 'TOTAL_MEMBER:', rc    = rc)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TOTAL_MEMBER:',rcfinal)

 CALL ESMF_ConfigGetAttribute(Cf, Int_State%grib_inp,                &
                              label = 'GRIB_INPUT:', rc    = rc)

 totpe = 0
 i1 = 0
 do j=1,Int_State%nam_gfs%Total_Member
   write(PELAB,'("PE_MEMBER",I2.2,":")') j
   CALL ESMF_ConfigGetAttribute(Cf, pe_member, label = PELAB, rc = rc)
   if (pe_member == 0) pe_member = tasks / Int_State%nam_gfs%Total_Member
   totpe = totpe + pe_member
   DO i = 1, pe_member
     IF (me == i1) THEN
        Int_State%nam_gfs%Member_Id = j
     END IF
     i1 = i1+1
   END DO
 END DO
 if (totpe /= tasks) then
  print *,' totpe=',totpe,' and tasks=',tasks, ' do not match'
  stop 9999
 endif
!print *,' me=',me,' Total_member=',Int_State%nam_gfs%Total_Member &
!,' Member_Id=',Int_State%nam_gfs%Member_Id

 CALL ESMF_VMGetCurrent(vm_local, rc = rc1)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%nam_gfs%nlunit,         &
                              label = 'NLUNIT:',  rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','NLUNIT',rcfinal)

!print *,' Int_State%nam_gfs%nlunit=',Int_State%nam_gfs%nlunit
!print *,' Member_Id=',Int_State%nam_gfs%Member_Id

 write(clab,'("NAMELIST",I2.2,":")') Int_State%nam_gfs%Member_Id
!print *,' clab=',clab
 CALL ESMF_ConfigGetAttribute(cf, Int_State%nam_gfs%gfs_namelist,   &
                              label = 'NAMELIST:',  rc = rc1)
!                             label = clab,  rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','NAMELIST',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%nam_gfs%DELTIM,         &
                              label = 'DELTIM:', rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','DELTIM',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%nam_gfs%FHROT,         &
                              label = 'FHROT:', rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','FHROT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%advanceCount_SetUp, &
                              label = 'ADVANCECOUNT_SETUP:', rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ADVANCECOUNT_SETUP',rcfinal)

 if (Int_State%nam_gfs%Total_Member <= 1) then
   Int_State%nam_gfs%sig_ini  = 'sig_ini'
   Int_State%nam_gfs%sfc_ini  = 'sfc_ini'
   Int_State%nam_gfs%sig_ini2 = 'sig_ini2'
   Int_State%nam_gfs%nst_ini  = 'nst_ini'
 else
   write(Int_State%nam_gfs%sig_ini, '("sig_ini_",I2.2)') Int_State%nam_gfs%Member_Id
   write(Int_State%nam_gfs%sfc_ini, '("sfc_ini_",I2.2)') Int_State%nam_gfs%Member_Id
   write(Int_State%nam_gfs%sig_ini2,'("sig_ini2_",I2.2)') Int_State%nam_gfs%Member_Id
 write(Int_State%nam_gfs%nst_ini, '("nst_ini_",I2.2)') Int_State%nam_gfs%Member_Id
 endif
!print *,' me=',me,' sig_ini=',Int_State%nam_gfs%sig_ini,' sfc_ini=',  &
!  Int_State%nam_gfs%sfc_ini,' sig_ini2=',Int_State%nam_gfs%sig_ini2, &
!  ' nst_ini=',Int_State%nam_gfs%nst_ini

!
! For "ESMF_State_Namelist"
!--------------------------

 CALL ESMF_ConfigGetAttribute(cf, Int_State%SPS,                                         &
                              label = 'ENS_SPS:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ENS_SPS:',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%HOUTASPS,                                    &
                              label = 'HOUTASPS:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','HOUTASPS:',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%idate1_import,                 &
                              label = 'IDATE1_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','IDATE1_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%z_import,                      &
                              label = 'Z_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','Z_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ps_import,                     &
                              label = 'PS_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','PS_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vor_import,                    &
                              label = 'VOR_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VOR_IMPORT',rcfinal)

 IF(ESMF_LogMsgFoundError(rc1, "Grid Component Get Configure Data")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running the ESMF_ConfigGetAttribute-VOR_IMPORT, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%div_import,                    &
                              label = 'DIV_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','DIV_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%temp_import,                   &
                              label = 'TEMP_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TEMP_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%q_import,                      &
                              label = 'Q_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','Q_IMPORT',rcfinal)


 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%oz_import,                     &
                              label = 'OZ_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','OZ_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%scld_import,                   &
                              label = 'SCLD_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SCLD_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%trieo_import,                  &
                              label = 'TRIEO_IMPORT:',   rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TRIEO_IMPORT',rcfinal)

!----------------------------------

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%idate1_export,                 &
                              label = 'IDATE1_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','IDATE1_EXPORT',rcfinal)


 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%z_export,                      &
                              label = 'Z_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','Z_EXPORT',rcfinal)

 IF(ESMF_LogMsgFoundError(rc1, "Grid Component Get Configure Data")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running the ESMF_ConfigGetAttribute-Z_EXPORT, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ps_export,                     &
                              label = 'PS_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','PS_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vor_export,                    &
                              label = 'VOR_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VOR_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%div_export,                    &
                              label = 'DIV_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','DIV_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%temp_export,                   &
                              label = 'TEMP_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TEMP_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%q_export,                      &
                              label = 'Q_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','Q_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%oz_export,                     &
                              label = 'OZ_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','OZ_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%scld_export,                   &
                              label = 'SCLD_EXPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SCLD_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%trieo_export,                  &
                              label = 'TRIEO_EXPORT:',   rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TRIEO_EXPORT',rcfinal)

!----------------------------------

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%orography_import,               &
                              label = 'OROGRAPHY_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','OROGRAPHY_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%t_skin_import,                  &
                              label = 'T_SKIN_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','T_SKIN_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_mois_import,               &
                              label = 'SOIL_MOIS_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_MOIS_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%snow_depth_import,              &
                              label = 'SNOW_DEPTH_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SNOW_DEPTH_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_t_import,                  &
                              label = 'SOIL_T_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_T_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%deep_soil_t_import,             &
                              label = 'DEEP_SOIL_T_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','DEEP_SOIL_T_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%roughness_import,               &
                              label = 'ROUGHNESS_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ROUGHNESS_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_cover_import,        &
                              label = 'CONV_CLOUD_COVER_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_COVER_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_base_import,         &
                              label = 'CONV_CLOUD_BASE_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_BASE_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_top_import,          &
                              label = 'CONV_CLOUD_TOP_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_TOP_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_visible_scattered_import,&
                              label = 'ALBEDO_VISIBLE_SCATTERED_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_VISIBLE_SCATTERED_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_visible_beam_import,     &
                              label = 'ALBEDO_VISIBLE_BEAM_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_VISIBLE_BEAMS_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_nearIR_scattered_import, &
                              label = 'ALBEDO_NEARIR_SCATTERED_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_VISIBLE_SCATTERED_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_nearIR_beam_import,      &
                              label = 'ALBEDO_NEARIR_BEAM_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_NEARIR_BEAM_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_level_ice_mask_import,      &
                              label = 'SEA_LEVEL_ICE_MASK_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_LEVEL_ICE_MASK_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_import,        &
                              label = 'VEGETATION_COVER_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%canopy_water_import,            &
                              label = 'CANOPY_WATER_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CANOPY_WATER_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%m10_wind_fraction_import,       &
                              label = 'M10_WIND_FRACTION_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','M10_WIND_FRACTION_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_type_import,         &
                              label = 'VEGETATION_TYPE_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_TYPE_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_type_import,               &
                              label = 'SOIL_TYPE_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_TYPE_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%zeneith_angle_facsf_import,     &
                              label = 'ZENEITH_ANGLE_FACSF_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ZENEITH_ANGLE_FACSF_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%zeneith_angle_facwf_import,     &
                              label = 'ZENEITH_ANGLE_FACWF_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ZENEITH_ANGLE_FACWF_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%uustar_import,                  &
                              label = 'UUSTAR_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','UUSTAR_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ffmm_import,                    &
                              label = 'FFMM_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','FFMM_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ffhh_import,                    &
                              label = 'FFHH_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','FFHH_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_ice_thickness_import,       &
                              label = 'SEA_ICE_THICKNESS_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_ICE_THICKNESS_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_ice_concentration_import,   &
                              label = 'SEA_ICE_CONCENTRATION_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_ICE_CONCENTRATION_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%tprcp_import,                   &
                              label = 'TPRCP_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TPRCP_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%srflag_import,                  &
                              label = 'SRFLAG_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SRFLAG_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%actual_snow_depth_import,       &
                              label = 'ACTUAL_SNOW_DEPTH_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ACTUAL_SNOW_DEPTH_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%liquid_soil_moisture_import,    &
                              label = 'LIQUID_SOIL_MOISTURE_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','LIQUID_SOIL_MOISTURE_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_min_import,    &
                              label = 'VEGETATION_COVER_MIN_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_MIN_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_max_import,    &
                              label = 'VEGETATION_COVER_MAX_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_MAX_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%slope_type_import,              &
                              label = 'SLOPE_TYPE_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SLOPE_TYPE_IMPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%snow_albedo_max_import,         &
                              label = 'SNOW_ALBEDO_MAX_IMPORT:',    rc = rc1)
 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SNOW_ALBEDO_MAX_IMPORT',rcfinal)

!----------------------------------

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%orography_export,               &
                              label = 'OROGRAPHY_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','OROGRAPHY_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%t_skin_export,                  &
                              label = 'T_SKIN_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','T_SKIN_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_mois_export,               &
                              label = 'SOIL_MOIS_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_MOIS_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%snow_depth_export,              &
                              label = 'SNOW_DEPTH_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SNOW_DEPTH_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_t_export,                  &
                              label = 'SOIL_T_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_T_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%deep_soil_t_export,             &
                              label = 'DEEP_SOIL_T_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','DEEP_SOIL_T_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%roughness_export,               &
                              label = 'ROUGHNESS_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ROUGHNESS_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_cover_export,        &
                              label = 'CONV_CLOUD_COVER_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_COVER_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_base_export,         &
                              label = 'CONV_CLOUD_BASE_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_BASE_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%conv_cloud_top_export,          &
                              label = 'CONV_CLOUD_TOP_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CONV_CLOUD_TOP_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_visible_scattered_export,&
                              label = 'ALBEDO_VISIBLE_SCATTERED_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_VISIBLE_SCATTERED_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_visible_beam_export,     &
                              label = 'ALBEDO_VISIBLE_BEAM_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_VISIBLE_BEAM_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_nearIR_scattered_export, &
                              label = 'ALBEDO_NEARIR_SCATTERED_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_NEARIR_SCATTERED_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%albedo_nearIR_beam_export,      &
                              label = 'ALBEDO_NEARIR_BEAM_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ALBEDO_NEARIR_BEAM_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_level_ice_mask_export,      &
                              label = 'SEA_LEVEL_ICE_MASK_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_LEVEL_ICE_MASK_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_export,        &
                              label = 'VEGETATION_COVER_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%canopy_water_export,            &
                              label = 'CANOPY_WATER_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','CANOPY_WATER_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%m10_wind_fraction_export,       &
                              label = 'M10_WIND_FRACTION_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','M10_WIND_FRACTION_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_type_export,         &
                              label = 'VEGETATION_TYPE_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_TYPE_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%soil_type_export,               &
                              label = 'SOIL_TYPE_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SOIL_TYPE_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%zeneith_angle_facsf_export,     &
                              label = 'ZENEITH_ANGLE_FACSF_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ZENEITH_ANGLE_FACSF_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%zeneith_angle_facwf_export,     &
                              label = 'ZENEITH_ANGLE_FACWF_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ZENEITH_ANGLE_FACWF_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%uustar_export,                  &
                              label = 'UUSTAR_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','UUSTAR_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ffmm_export,                    &
                              label = 'FFMM_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','FFMM_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%ffhh_export,                    &
                              label = 'FFHH_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','FFHH_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_ice_thickness_export,       &
                              label = 'SEA_ICE_THICKNESS_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_ICE_THICKNESS_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%sea_ice_concentration_export,   &
                              label = 'SEA_ICE_CONCENTRATION_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SEA_ICE_CONCENTRATION_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%tprcp_export,                   &
                              label = 'TPRCP_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','TPRCP_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%srflag_export,                  &
                              label = 'SRFLAG_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SRFLAG_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%actual_snow_depth_export,       &
                              label = 'ACTUAL_SNOW_DEPTH_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','ACTUAL_SNOW_DEPTH_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%liquid_soil_moisture_export,    &
                              label = 'LIQUID_SOIL_MOISTURE_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','LIQUID_SOIL_MOISTURE_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_min_export,    &
                              label = 'VEGETATION_COVER_MIN_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_MIN_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%vegetation_cover_max_export,    &
                              label = 'VEGETATION_COVER_MAX_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','VEGETATION_COVER_MAX_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%slope_type_export,              &
                              label = 'SLOPE_TYPE_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SLOPE_TYPE_EXPORT',rcfinal)

 CALL ESMF_ConfigGetAttribute(cf, Int_State%ESMF_Sta_List%snow_albedo_max_export,         &
                              label = 'SNOW_ALBEDO_MAX_EXPORT:',    rc = rc1)

 CALL ERR_MSG(rc1,'Grid Component Get Configure Data','SNOW_ALBEDO_MAX_EXPORT',rcfinal)

!----------------------------------

 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: GFS_GetCf_ESMFMod.F90"
 ELSE
     if (me == 0) PRINT*, "PASS: GFS_GetCf_ESMFMod.F90"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GFS_GetCf

 END MODULE GFS_GetCf_ESMFMod
