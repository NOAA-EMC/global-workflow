!
! !MODULE: NameList_ESMFMod  ---                Definition of the name list
!                                               in the ESMF internal state.
!
! !DESCRIPTION: NameList_ESMFMod ---            Define the name list variables
!                                               in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  November 2004      Weiyu Yang Initial code.
!  February 2006      Took out model namelists
!
! !INTERFACE:
!
 MODULE NameList_ESMFMod


 IMPLICIT none

 TYPE nam_gfs_Namelist
      INTEGER                :: nlunit, Total_Member, Member_Id
      REAL                   :: DELTIM, FHROT
      CHARACTER(80)          :: gfs_namelist
      CHARACTER(20)          :: sig_ini, sig_ini2, sfc_ini
      CHARACTER(20)          :: nst_ini
 END TYPE nam_gfs_Namelist
!
 TYPE ESMF_State_Namelist
!
! For the sigma file.
!--------------------
      LOGICAL                :: idate1_import
      LOGICAL                :: z_import
      LOGICAL                :: ps_import
      LOGICAL                :: vor_import
      LOGICAL                :: div_import
      LOGICAL                :: temp_import
      LOGICAL                :: q_import
      LOGICAL                :: oz_import
      LOGICAL                :: scld_import
      LOGICAL                :: trieo_import

      LOGICAL                :: idate1_export
      LOGICAL                :: z_export
      LOGICAL                :: ps_export
      LOGICAL                :: vor_export
      LOGICAL                :: div_export
      LOGICAL                :: temp_export
      LOGICAL                :: q_export
      LOGICAL                :: oz_export
      LOGICAL                :: scld_export
      LOGICAL                :: trieo_export
!For the surface file.
!---------------------
      LOGICAL                :: orography_import
      LOGICAL                :: t_skin_import
      LOGICAL                :: soil_mois_import
      LOGICAL                :: snow_depth_import
      LOGICAL                :: soil_t_import
      LOGICAL                :: deep_soil_t_import
      LOGICAL                :: roughness_import
      LOGICAL                :: conv_cloud_cover_import
      LOGICAL                :: conv_cloud_base_import
      LOGICAL                :: conv_cloud_top_import
      LOGICAL                :: albedo_visible_scattered_import
      LOGICAL                :: albedo_visible_beam_import
      LOGICAL                :: albedo_nearIR_scattered_import
      LOGICAL                :: albedo_nearIR_beam_import
      LOGICAL                :: sea_level_ice_mask_import
      LOGICAL                :: vegetation_cover_import
      LOGICAL                :: canopy_water_import
      LOGICAL                :: m10_wind_fraction_import
      LOGICAL                :: vegetation_type_import
      LOGICAL                :: soil_type_import
      LOGICAL                :: zeneith_angle_facsf_import
      LOGICAL                :: zeneith_angle_facwf_import
      LOGICAL                :: uustar_import
      LOGICAL                :: ffmm_import
      LOGICAL                :: ffhh_import
      LOGICAL                :: sea_ice_thickness_import
      LOGICAL                :: sea_ice_concentration_import
      LOGICAL                :: tprcp_import
      LOGICAL                :: srflag_import
      LOGICAL                :: actual_snow_depth_import
      LOGICAL                :: liquid_soil_moisture_import
      LOGICAL                :: vegetation_cover_min_import
      LOGICAL                :: vegetation_cover_max_import
      LOGICAL                :: slope_type_import
      LOGICAL                :: snow_albedo_max_import

      LOGICAL                :: orography_export
      LOGICAL                :: t_skin_export
      LOGICAL                :: soil_mois_export
      LOGICAL                :: snow_depth_export
      LOGICAL                :: soil_t_export
      LOGICAL                :: deep_soil_t_export
      LOGICAL                :: roughness_export
      LOGICAL                :: conv_cloud_cover_export
      LOGICAL                :: conv_cloud_base_export
      LOGICAL                :: conv_cloud_top_export
      LOGICAL                :: albedo_visible_scattered_export
      LOGICAL                :: albedo_visible_beam_export
      LOGICAL                :: albedo_nearIR_scattered_export
      LOGICAL                :: albedo_nearIR_beam_export
      LOGICAL                :: sea_level_ice_mask_export
      LOGICAL                :: vegetation_cover_export
      LOGICAL                :: canopy_water_export
      LOGICAL                :: m10_wind_fraction_export
      LOGICAL                :: vegetation_type_export
      LOGICAL                :: soil_type_export
      LOGICAL                :: zeneith_angle_facsf_export
      LOGICAL                :: zeneith_angle_facwf_export
      LOGICAL                :: uustar_export
      LOGICAL                :: ffmm_export
      LOGICAL                :: ffhh_export
      LOGICAL                :: sea_ice_thickness_export
      LOGICAL                :: sea_ice_concentration_export
      LOGICAL                :: tprcp_export
      LOGICAL                :: srflag_export
      LOGICAL                :: actual_snow_depth_export
      LOGICAL                :: liquid_soil_moisture_export
      LOGICAL                :: vegetation_cover_min_export
      LOGICAL                :: vegetation_cover_max_export
      LOGICAL                :: slope_type_export
      LOGICAL                :: snow_albedo_max_export
 END TYPE ESMF_State_Namelist

 END MODULE NameList_ESMFMod
