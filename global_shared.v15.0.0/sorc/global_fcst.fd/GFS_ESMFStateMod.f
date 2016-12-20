  MODULE GFS_ESMFStateMod

!  June  2005 Weiyu Yang            Initial code.
!  May   2008 Weiyu Yang            Updated to use the ESMF 3.1.0r library.
!  April 2009 Shrinivas Moorthi     Merge GFS and GEFS versions
!  May   2013 Weiyu Yang            Modified to match the internal data structure 
!                                    of the Cpl (STTP) module, which is
!                                    different from GFS module 

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.

! The derived type of the internal state.
!----------------------------------------
 USE GFS_InternalState_ESMFMod

! Routines which can be used to add a fortran array to 
! an ESMF state and to get a fortran array from an 
! ESMF state
!-----------------------------------------------------
 USE Lib_ESMFStateAddGetMod
 USE GFS_ErrMsgMod

 USE mpi_def

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_ESMFImportState2InternalState(gcGFS, impGFS, Int_State, rc)

! This subroutine can be used to update the initial condition 
! fields of the internal state from the ESMF inport state.
!------------------------------------------------------------

! Every possible import state has its own turn-on/turn-off switch flag
! which can be used to fit different interface requirement from different
! outside grid component systems.
!------------------------------------------------------------------------

 USE physcons, ONLY: con_rerth, con_g

 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS     ! ESMF grid component which contains
                                                              ! the ESMF import state.
 TYPE(ESMF_State)                                :: impGFS    ! the ESMF import state.
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: Int_State ! the internal state which contains the initial
                                                              ! condition fields to run the GFS model.
 INTEGER, OPTIONAL,                INTENT(out)   :: rc        ! error signal variable.

 TYPE(ESMF_VM)                                        :: vm   ! ESMF virtual machine,
 INTEGER                                              :: rc1, rcfinal        ! error signal variable.
 INTEGER                                              :: mm1, i, i1, i2, i3, j, l, ls_dim2, ii1(2)
 INTEGER                                              :: n, n1, n2, k
 INTEGER                                              :: indlsev, jbasev
 INTEGER                                              :: indlsod, jbasod
 INTEGER,                DIMENSION(lonr, latr)        :: kmsk
 REAL(KIND = kind_evod)                               :: GA2
 REAL(KIND = kind_evod), DIMENSION(lnt2)              :: TRISCA
 REAL(KIND = kind_io4),  DIMENSION(lnt2)              :: TRISCA_4
 REAL(KIND = kind_io8),  DIMENSION(lonr, lats_node_r) :: buffo
 REAL(KIND = kind_io8),  DIMENSION(lonr, lats_node_r, 3) :: buff2

 REAL(KIND = kind_io4),  DIMENSION(lonr, latr)        :: buf, buf11, buf12, buf13
 REAL(KIND = kind_io4),  DIMENSION(:, :), POINTER     :: buf1
 INTEGER,                DIMENSION(Int_State%nodes, 2):: ijn_g, displs_g
 CHARACTER(5),           DIMENSION(:),    POINTER     :: SMC_name, STC_name, SLC_name

 INCLUDE 'function_indlsev'
 INCLUDE 'function_indlsod'

! Initialize the error signal variables.
!---------------------------------------
 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

 mm1     = Int_State%mm1

 CALL ESMF_LogWrite("Begining the Run, Update Internal State with the ESMF Import State", &
                    ESMF_LOG_INFO, rc = rc1)

! Getting the global VM for the gathering data purpose.
!------------------------------------------------------
 CALL ESMF_GridCompGet(gcGFS, vm = vm, rc = rc1)
 CALL ERR_MSG1(rc1,'Error Happened When Getting the GLobal VM',rcfinal)

! Create the gathering parameter arrays which will be used
! to gather the distributed ESMF state local array to the global array.
!----------------------------------------------------------------------

! ijn_g(1) and displs_g(1) are for the spectral space sigma file arrays,
! ijn_g(2) and displs_g(2) are for the Gaussian grid surface data arrays.
!------------------------------------------------------------------------
 displs_g(1, :) = 0
 DO i = 1, Int_State%nodes
     IF(me == i-1) THEN
         ii1(1) = Int_State%lnt2_s
         ii1(2) = Int_State%lonr_s
     END IF
     CALL ESMF_VMBroadcast(vm, ii1, 2, i-1, blockingflag = ESMF_BLOCKING, rc = rc1)
     ijn_g(i, 1) = ii1(1)
     ijn_g(i, 2) = ii1(2)

     CALL ERR_MSG1(rc1,'Error Happened When VMBroadcast ijn_g',rcfinal)
     IF(i /= 1) THEN
         displs_g(i, 1) = displs_g(i-1, 1) + ijn_g(i-1, 1)
         displs_g(i, 2) = displs_g(i-1, 2) + ijn_g(i-1, 2)
     END IF
 END DO

! Get the start time and date from the ESMF import state and update the 
! fhour and idate.
!----------------------------------------------------------------------

! idate1_im:  (1) --- bfhour (integer), (2) - (5) --- idate.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%idate1_import) THEN

! Get the ESMF import state date and time information and put
! into the internal state. "GetF90ArrayFromState" is an user
! created software to get a fortran array from an ESMF state.
! The first argument is the ESMF state from which the fortran
! array will be gotten.  The second argument is the user defined
! name to identify the array in the ESMF state.  The third one
! is the obtained fortran array and the last one is the error
! signal variable.
!---------------------------------------------------------------
   CALL GetF90ArrayFromState(impGFS, 'DATE', Int_State%idate1_im, 0, rc = rc1)
   CALL ERR_MSG1(rc1,'Error Happened When Getting ESMF State - DATE_im',rcfinal)

! Put the date and time information to fhour and idate
! which will be used to run the model.
!-----------------------------------------------------
   fhour = Int_State%idate1_im(1, 1)
   DO i = 1, 4
       idate(i) = Int_State%idate1_im(1, i+1)
   END DO
 END IF

! Get the surface orography array from the ESMF import state.
!------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%z_import) THEN

! Get the local surface orography array from the ESMF import state.
!------------------------------------------------------------------
   CALL GetF90ArrayFromState(impGFS, 'HS', Int_State%z_im, 0, rc = rc1)

   CALL ERR_MSG1(rc1,"Get ESMF State - HS_im",rcfinal)

! Gather the local surface orography array into the global array.
!----------------------------------------------------------------
   CALL mpi_allgatherv(Int_State%z_im, ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                      displs_g, MPI_R_IO, mpi_comm_all, rc1)

   CALL ERR_MSG1(rc1,"MPI_allgatherv - z_im",rcfinal)
 
! Change the real size 4 to real size 8 for GFS run.
!---------------------------------------------------
   TRISCA = TRISCA_4

! Distribute the global surface orography array into the GFS
! internal structure array TRIE_LS and TRIO_LS which are in
! the internal state.
!-----------------------------------------------------------
   CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_GZ), &
                         Int_State%TRIO_LS(1, 1, Int_State%P_GZ), &
                         1, Int_State%LS_NODE)

! Some necessary pre-set up computations.
!----------------------------------------
   GA2 = con_g/(con_rerth * con_rerth)

   DO j = 1, LS_MAX_NODE
       L      = Int_State%LS_NODE(j)
       jbasev = Int_State%LS_NODE(j + ls_dim)
       i1     = indlsev(L, L)
       IF(MOD(L, 2) == MOD(jcap+1, 2)) THEN
           i2 = indlsev(jcap+1, L)
       ELSE
           i2 = indlsev(jcap, L)
       END IF
       DO i = i1 , i2
           Int_State%TRIE_LS(i, 1,  Int_State%P_GZ)                        &
               = Int_State%TRIE_LS(i, 1,  Int_State%P_GZ)*Int_State%SNNP1EV(i)*GA2
           Int_State%TRIE_LS(i, 2,  Int_State%P_GZ)                        &
               = Int_State%TRIE_LS(i, 2,  Int_State%P_GZ)*Int_State%SNNP1EV(i)*GA2
       END DO
   END DO

   ls_dim2 = ls_dim * 2
   DO j = 1, LS_MAX_NODE
       L      = Int_State%LS_NODE(j)
       jbasod = Int_State%LS_NODE(j + ls_dim2)
       i1     = indlsod(L+1, L)
       IF(MOD(L, 2) == MOD(jcap+1, 2)) THEN
           i2 = indlsod(jcap, L) 
       ELSE
           i2 = indlsod(jcap+1, L)
       END IF
       DO i = i1 , i2
           Int_State%TRIO_LS(i, 1,  Int_State%P_GZ)                        &
               = Int_State%TRIO_LS(i, 1,  Int_State%P_GZ)*Int_State%SNNP1OD(i)*GA2
           Int_State%TRIO_LS(i, 2,  Int_State%P_GZ)                        &
               = Int_State%TRIO_LS(i, 2,  Int_State%P_GZ)*Int_State%SNNP1OD(i)*GA2
       END DO
   END DO
 END IF

! Get the surface pressure array from the ESMF import state.
! For the detailed comments for every computational steps
! please refer to the surface orography array code.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ps_import) THEN
   CALL GetF90ArrayFromState(impGFS, 'PS', Int_State%ps_im, 0, rc = rc1)

   CALL ERR_MSG1(rc1,"Get ESMF State - PS_im",rcfinal)

   CALL mpi_allgatherv(Int_State%ps_im, ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                    displs_g, MPI_R_IO, mpi_comm_all, rc1)

   CALL ERR_MSG1(rc1,"MPI_allgatherv - ps_im",rcfinal)

   TRISCA = TRISCA_4

   CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_QM), &
                         Int_State%TRIO_LS(1, 1, Int_State%P_QM), &
                         1, Int_State%LS_NODE)
 END IF

! Get the temperature array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%temp_import) THEN

! Get the local temperature array from the ESMF import state.
!------------------------------------------------------------
   CALL GetF90ArrayFromState(impGFS, 'T', Int_State%temp_im, 0, rc = rc1)

   CALL ERR_MSG1(rc1,"Get ESMF State - T_im",rcfinal)

! Do loop over the vertical levels.
!----------------------------------
   DO i = 1, Int_State%levs

! Gather each level local temperature array into the global array.
!-----------------------------------------------------------------
         CALL mpi_allgatherv(Int_State%temp_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - temp_im",rcfinal)

! Change the real size 4 to real size 8 for GFS run.
!---------------------------------------------------
         TRISCA = TRISCA_4

! Distribute the global temperature array into the GFS
! internal structure array TRIE_LS and TRIO_LS which are in
! the internal state.
!----------------------------------------------------------
         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_TEM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_TEM+i-1), &
                               1, Int_State%LS_NODE)
     END DO
 END IF

! Get the divergence array from the ESMF import state.
! For detailed line by line comments please refer to 
! the temperature code.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%div_import) THEN
     CALL GetF90ArrayFromState(impGFS, 'D', Int_State%div_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - D_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%div_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - div_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_DIM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_DIM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the vorticity array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vor_import) THEN
     CALL GetF90ArrayFromState(impGFS, 'Z', Int_State%vor_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - Z_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%vor_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - vor_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_ZEM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_ZEM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the moisture array from the ESMF import state.
!---------------------------------------------------
 IF(Int_State%ESMF_Sta_List%q_import) THEN
     CALL GetF90ArrayFromState(impGFS, 'SHUM', Int_State%q_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SHUM_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%q_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - q_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the ozone array from the ESMF import state.
!------------------------------------------------
 IF(Int_State%ESMF_Sta_List%oz_import) THEN
     CALL GetF90ArrayFromState(impGFS, 'SOZ', Int_State%oz_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Get ESMF State - SOZ_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%oz_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - oz_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+Int_State%levs+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+Int_State%levs+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the cloud liquid water array from the ESMF import state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%scld_import) THEN
     CALL GetF90ArrayFromState(impGFS, 'SCLD', Int_State%scld_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SCLD_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%scld_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - scld_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+Int_State%levs*2+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+Int_State%levs*2+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

!**********************************************************************
 IF(Int_State%ESMF_Sta_List%trieo_import) THEN
     CALL GetF90ArrayFromState(impGFS, Int_State%TRIEO_STATE_NAME, &
                                       Int_State%trieo_ls_im,      &
                                       0,                          &
                                       rc            = rc1)
     i3 = 0
     DO k = 1, 7
         IF(k == 1) THEN
             n1 = Int_State%P_GZ
             n2 = Int_State%P_QM - 1
         END IF
         IF(k == 2) THEN
             n1 = Int_State%P_RM
             n2 = Int_State%P_RQ - 1
         END IF
         IF(k == 3) THEN
             n1 = Int_State%P_QM
             n2 = Int_State%P_Q - 1
         END IF
         IF(k == 4) THEN
             n1 = Int_State%P_RQ
             n2 = Int_State%P_RQ + Int_State%levh - 1
         END IF
         IF(k == 5) THEN
             n1 = Int_State%P_Q
             n2 = Int_State%P_ZQ - 1
         END IF
         IF(k == 6) THEN
             n1 = Int_State%P_RT
             n2 = Int_State%P_RM - 1
         END IF
         IF(k == 7) THEN
             n1 = Int_State%P_ZQ
             n2 = Int_State%P_RT - 1
         END IF

         DO l = n1, n2
             DO j = 1, 2
                 i3 = i3 + 1
                 i1 = 3
                 DO i = 1, Int_State%TRIE_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%trie_ls(i, j, l) = Int_State%trieo_ls_im(i1, i3)
                 END DO
                 DO i = 1, Int_State%TRIO_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%trio_ls(i, j, l) = Int_State%trieo_ls_im(i1, i3)
                 END DO
             END DO
         END DO
     END DO
     CALL ERR_MSG1(rc1, "Get ESMF State - TRIEO_LS_im", rcfinal)
 END IF

!**********************************************************************

! For the Gaussian grid surface data.
!------------------------------------

! Get the sea level ice mask data array from the ESMF import state.
!------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_level_ice_mask_import) THEN

     kmsk = 0
! Get the local sea level ice mask data array from the ESMF import state.
!------------------------------------------------------------------------
     CALL GetF90ArrayFromState(impGFS, 'SLMSK', Int_State%sea_level_ice_mask_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SLMSK_im",rcfinal)

! Do loop over the latitudes.
!----------------------------
     DO i = 1, latr

! Gather each latitude local sea level ice mask data array into the whole latitude array.
!----------------------------------------------------------------------------------------
         CALL mpi_gatherv(Int_State%sea_level_ice_mask_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_level_ice_mask_im",rcfinal)
     END DO

! Distribute the global sea level ice mask array into the GFS
! internal structure array SLMSK which are in the internal state.
!----------------------------------------------------------------
     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SLMSK, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the orography array from the ESMF import state.
! For detailed line by line comments please refer to
! the sea level ice mask code.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%orography_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'OROG', Int_State%orography_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - OROG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%orography_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - orography_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ORO, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the skin temperature array from the ESMF import state.
! For detailed line by line comments please refer to
! the sea level ice mask code.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%t_skin_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TSEA', Int_State%t_skin_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - TSEA_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%t_skin_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - t_skin_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TSEA, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the snow depth array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_depth_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SHELEG', Int_State%snow_depth_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SHELEG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%snow_depth_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - snow_depth_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHELEG, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the deep soil temperature array from the ESMF import state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%deep_soil_t_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TG3', Int_State%deep_soil_t_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - TG3_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%deep_soil_t_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - deep_soil_t_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TG3, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the surface roughness data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%roughness_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ZORL', Int_State%roughness_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - ZORL_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%roughness_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - roughness_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ZORL, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo visible scattered array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_scattered_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALVSF', Int_State%albedo_visible_scattered_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Get ESMF State - ALVSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_visible_scattered_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_visible_scattered_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALVSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo visible beam array from the ESMF import state.
!--------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_beam_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALVWF', Int_State%albedo_visible_beam_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - ALVWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_visible_beam_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_visible_beam_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALVWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo IR scattered array from the ESMF import state.
!--------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_scattered_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALNSF', Int_State%albedo_nearIR_scattered_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - ALNSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_nearIR_scattered_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_nearIR_scattered_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALNSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo IR beam array from the ESMF import state.
!---------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_beam_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALNWF', Int_State%albedo_nearIR_beam_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - ALNWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_nearIR_beam_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_nearIR_beam_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALNWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the vegetation cover data array from the ESMF import state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VFRAC', Int_State%vegetation_cover_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - VFRAC_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%VFRAC, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the canopy water data array from the ESMF import state.
!------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%canopy_water_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CANOPY', Int_State%canopy_water_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - CANOPY_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%canopy_water_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - canopy_water_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CANOPY, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the 10 meter wind fraction data array from the ESMF import state.
!----------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%m10_wind_fraction_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'F10M', Int_State%m10_wind_fraction_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - F10M_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%m10_wind_fraction_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - m10_wind_fraction_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%F10M, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the vegetation type data array from the ESMF import state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_type_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VTYPE', Int_State%vegetation_type_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - VTYPE_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%VTYPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the soil type data array from the ESMF import state.
!---------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_type_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'STYPE', Int_State%soil_type_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - STYPE_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%soil_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_type_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%STYPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the zeneith angle facsf data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facsf_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FACSF', Int_State%zeneith_angle_facsf_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - FACSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%zeneith_angle_facsf_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - zeneith_angle_facsf_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FACSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the zeneith angle facwf data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facwf_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FACWF', Int_State%zeneith_angle_facwf_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - FACWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%zeneith_angle_facwf_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - zeneith_angle_facwf_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FACWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the uustar data array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%uustar_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'UUSTAR', Int_State%uustar_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - UUSTAR_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%uustar_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - uustar_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%UUSTAR, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the ffmm data array from the ESMF import state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffmm_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FFMM', Int_State%ffmm_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - FFMM_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%ffmm_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - ffmm_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FFMM, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the ffhh data array from the ESMF import state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffhh_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FFHH', Int_State%ffhh_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - FFHH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%ffhh_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - ffhh_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FFHH, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the sea ice thickness data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_thickness_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SIH', Int_State%sea_ice_thickness_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SIH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%sea_ice_thickness_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_ice_thickness_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%HICE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the sea ice concentration data array from the ESMF import state.
!---------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_concentration_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SIC', Int_State%sea_ice_concentration_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SIC_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%sea_ice_concentration_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_ice_concentration_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FICE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the TpRCp data array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%tprcp_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TPRCP', Int_State%tprcp_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - TPRCP_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%tprcp_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - tprcp_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TPRCP, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the srflag data array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%srflag_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SRFLAG', Int_State%srflag_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SRFLAG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%srflag_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - srflag_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SRFLAG, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the actual snow depth data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%actual_snow_depth_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SNWDPH', Int_State%actual_snow_depth_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SNWDPH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%actual_snow_depth_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - actual_snow_depth_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SNWDPH, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the minimum vegetation cover data array from the ESMF import state.
!------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_min_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VMN', Int_State%vegetation_cover_min_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - VMN_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_min_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_min_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHDMIN, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the maximum vegetation cover data array from the ESMF import state.
!------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_max_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VMX', Int_State%vegetation_cover_max_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - VMX_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_max_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_max_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHDMAX, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the slope type data array from the ESMF import state.
!----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%slope_type_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SLP', Int_State%slope_type_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - SLP_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%slope_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - slope_type_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SLOPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the maximum snow albedo data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_albedo_max_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ABS', Int_State%snow_albedo_max_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - ABS_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%snow_albedo_max_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - snow_albedo_max_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SNOALB, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the soil temperature data array from the ESMF import state.
! It contains multiple level data.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_t_import) THEN
     kmsk = 0

     ALLOCATE(STC_name(Int_State%lsoil), stat = rc1)
     CALL ERR_MSG1(rc1, " - Allocate the working array - STC_name", rcfinal)

     DO i = 1, Int_State%lsoil
        WRITE(STC_name(i), 2004) i
     END DO
     CALL GetF90ArrayFromState(impGFS, STC_name(1), Int_State%soil_t_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, STC_name(2), Int_State%soil_t_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, STC_name(3), Int_State%soil_t_im3, 0, rc = rc1)
     DEALLOCATE(STC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - STC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%soil_t_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_t_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_t_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_t_im",rcfinal)
     END DO

     CALL split2d(buf11, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%STC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2004 FORMAT('STC_', i1)
 END IF

 IF(Int_State%ESMF_Sta_List%soil_mois_import) THEN
     kmsk = 0

     ALLOCATE(SMC_name(Int_State%lsoil),    stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the working array - SMC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SMC_name(i), 2005) i
     END DO

     CALL GetF90ArrayFromState(impGFS, SMC_name(1), Int_State%soil_mois_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SMC_name(2), Int_State%soil_mois_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SMC_name(3), Int_State%soil_mois_im3, 0, rc = rc1)
     DEALLOCATE(SMC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - SMC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%soil_mois_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_mois_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_mois_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_mois_im",rcfinal)
     END DO

     CALL split2d(buf11,  buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%SMC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2005 FORMAT('SMC_', i1)
 END IF

 IF(Int_State%ESMF_Sta_List%liquid_soil_moisture_import) THEN
     kmsk = 0

     ALLOCATE(SLC_name(Int_State%lsoil),    stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the working array - SLC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SLC_name(i), 2003) i
     END DO

     CALL GetF90ArrayFromState(impGFS, SLC_name(1), Int_State%liquid_soil_moisture_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SLC_name(2), Int_State%liquid_soil_moisture_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SLC_name(3), Int_State%liquid_soil_moisture_im3, 0, rc = rc1)
     DEALLOCATE(SLC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - SLC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - liquid_soil_moisture_im",rcfinal)
     END DO

     CALL split2d(buf11,  buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%SLC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2003 FORMAT('SLC_', i1)
 END IF

! Get the convective cloud cover data array from the ESMF import state.
!----------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_cover_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CV', Int_State%conv_cloud_cover_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - CV_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_cover_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_cover_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CV, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the convective cloud base data array from the ESMF import state.
!---------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_base_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CVB', Int_State%conv_cloud_base_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - CVB_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_base_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_base_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CVB, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the convective cloud top data array from the ESMF import state.
!--------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_top_import) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CVT', Int_State%conv_cloud_top_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Get ESMF State - CVT_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_top_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_top_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CVT, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Print out the final error signal message and put it to rc.
!-----------------------------------------------------------
 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: GFS_ESMFImportState2InternalStateMod.F90"
!ELSE
!    PRINT*, "PASS: GFS_ESMFImportState2InternalStateMod.F90"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GFS_ESMFImportState2InternalState



 SUBROUTINE GFS_InternalState2ESMFExportState(Int_State)

! This subroutine reads the last written sigma file and surface file and put them
! into the GFS ESMF export state.  This subroutine will be changed that all export
! ESMF states will get data directly from the GFS internal structure data arrays! instead of from the sigma file and surface file.

!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: Int_State ! the internal state which contains the
                                                              ! GFS final output arrays.
 INTEGER               :: mm1, i, j, i1, i2, i3, l, n, n1, n2, k

 mm1     = Int_State%mm1

 IF(Int_State%ESMF_Sta_List%trieo_export) THEN

! Getting the trieo export state.
!--------------------------------
     n = 11*Int_State%levs + 3*Int_State%levh + 6

     i2 = 0
     i3 = 0
     DO k = 1, 7
         IF(k == 1) THEN
             n1 = Int_State%P_GZ
             n2 = Int_State%P_QM - 1
         END IF
         IF(k == 2) THEN
             n1 = Int_State%P_RM
             n2 = Int_State%P_RQ - 1
         END IF
         IF(k == 3) THEN
             n1 = Int_State%P_QM
             n2 = Int_State%P_Q - 1
         END IF
         IF(k == 4) THEN
             n1 = Int_State%P_RQ
             n2 = Int_State%P_RQ + Int_State%levh - 1
         END IF
         IF(k == 5) THEN
             n1 = Int_State%P_Q
             n2 = Int_State%P_ZQ - 1
         END IF
         IF(k == 6) THEN
             n1 = Int_State%P_RT
             n2 = Int_State%P_RM - 1
         END IF
         IF(k == 7) THEN
             n1 = Int_State%P_ZQ
             n2 = Int_State%P_RT - 1
         END IF

!     write(0,*) *, 'DHOUWYT A', k,n1,n2,i2,i3            
         DO l = n1, n2
             DO j = 1, 2
                 i2 = i2 + 1

!     IF (i2 > 2572)  PRINT *, 'DHOUWYT i2', k,n1,n2,l,j,i2            
                 Int_State%write_work8_ini(1, i2) = FLOAT(Int_State%TRIE_LS_SIZE(mm1))
                 Int_State%write_work8_ini(2, i2) = FLOAT(Int_State%TRIO_LS_SIZE(mm1))
                 Int_State%write_work8_ini(3, i2) = FLOAT(n)
                 i1 = 3
                 DO i = 1, Int_State%TRIE_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%write_work8_ini(i1, i2) = Int_State%trie_ls_ini(i, j, l)
                 END DO
                 DO i = 1, Int_State%TRIO_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%write_work8_ini(i1, i2) = Int_State%trio_ls_ini(i, j, l)
                 END DO
             END DO
         END DO

         DO l = n1, n2
             DO j = 1, 2
                 i3 = i3 + 1
!     IF (i3 > 2572) PRINT *, 'DHOUWYT i3', k,n1,n2,l,j,i3            
                 Int_State%write_work8(1, i3) = FLOAT(Int_State%TRIE_LS_SIZE(mm1))
                 Int_State%write_work8(2, i3) = FLOAT(Int_State%TRIO_LS_SIZE(mm1))
                 Int_State%write_work8(3, i3) = FLOAT(n)
                 i1 = 3
                 DO i = 1, Int_State%TRIE_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%write_work8(i1, i3) = Int_State%trie_ls(i, j, l)
                 END DO
                 DO i = 1, Int_State%TRIO_LS_SIZE(mm1)
                     i1 = i1 + 1
                     Int_State%write_work8(i1, i3) = Int_State%trio_ls(i, j, l)
                 END DO
             END DO
         END DO
     END DO
 END IF

 END SUBROUTINE GFS_InternalState2ESMFExportState

! End of the ESMF state module.
!------------------------------
 END MODULE GFS_ESMFStateMod
