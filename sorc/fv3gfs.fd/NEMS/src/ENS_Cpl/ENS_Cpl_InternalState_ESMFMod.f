#include "../ESMFVersionDefine.h"

!
! !MODULE: ENS_Cpl_InternalState_ESMFMod --- Internal state definition of the
!                                            ESMF coupler gridded component of 
!                                            the EARTH ensemble system.
!
! !DESCRIPTION: ENS_Cpl_InternalState_ESMFMod --- Define the EARTH ensemble coupler 
!                                                  internal state.
!----------------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  April 2006         Weiyu Yang Initial code.
!  March 2007         Dingchen Hou added Stochatic Perturbation Combination Coefficient array.
!  January to November 2007      Dingchen and Weiyu Yang
!                     Added Broadcasting procedure and Global variables/arrays
!  November 2007      Dingchen, added minimum documentation, mainly for the arrays added during 2007
!  March    2009      Weiyu Yang, Modified for the NEMS model.
!  September2011      Weiyu Yang, Modified for using the ESMF 5.2.0r library.
!
! !INTERFACE:
!
 MODULE ENS_Cpl_InternalState_ESMFMod

!!USES:
!------
      USE ESMF
 USE machine, ONLY: KIND_EVOD, KIND_PHYS

 IMPLICIT none

 TYPE ENS_Cpl_InternalState

      INTEGER                        :: me, mm1, nodes   !nodes is the number of CPUs. me is id of the cpu
      INTEGER                        :: Total_member     !number of ensemble members
      INTEGER                        :: hh_start         !The number of hours of integration has benn done
      INTEGER                        :: hh_final         !The number of hours of integration should be done
      INTEGER                        :: hh_increase      !The number of hours of integration to do
                                                         !before next application of coupling 
      INTEGER                        :: Cpl_Run_Calling_Number
                                                         !for =N, this is the N'th application of Cpl 
      INTEGER                        :: Cpl_Run_Calling_Start
                                                         !for =N, the N'th application of Cpl is the first 
      INTEGER                        :: Cpl_Run_Calling_Final
                                                         !for =N, the N'th application of Cpl is the last,wrt_rst only 
      INTEGER                        :: MC_COMP
      INTEGER                        :: MPI_R_MPI_R
      INTEGER                        :: nodes_comp, me_comp
      INTEGER                        :: lats_node_a, ipt_lats_node_a
      INTEGER                        :: ntrac
      INTEGER                        :: jcap
      INTEGER                        :: latg, lonf
      INTEGER                        :: CENTER
      INTEGER                        :: USES1
      INTEGER                        :: Jul_Day

      INTEGER                        :: arraysize_1
      INTEGER                        :: arraysize_2
      INTEGER                        :: arraysize_3
      INTEGER                        :: arraysize_4   ! (arraysize_4 = arraysize_1 * arraysize_2)

      INTEGER                        :: ARRAY_ONE_SIZ2
      INTEGER                        :: ARRAY_ONE_SIZ3
      INTEGER                        :: ARRAY_ONE_SIZ4

      INTEGER                        :: ARRAY_TOT_SIZ2
      INTEGER                        :: ARRAY_TOT_SIZ3
      INTEGER                        :: ARRAY_TOT_SIZ4

      CHARACTER(ESMF_MAXSTR)         :: Core

!  Nov. 2007, Added global arrays
      INTEGER, DIMENSION(:), POINTER :: member_id        !global array specifying the member_id for each CPU

      INTEGER, DIMENSION(:), POINTER :: lonsperlat
      INTEGER, DIMENSION(:), POINTER :: lats_node_global

      INTEGER, DIMENSION(:, :), POINTER :: lats_global

!March 2007,  Global arrys for perturbation combination
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: Sto_Coef 
                                               !Stochastic combination coefficients 

!Nov. 2007,  Global Variables and arrys for Regional Rescaling, 
      INTEGER                         :: latmax, nreg
      INTEGER, DIMENSION(:), POINTER  :: global_lats_a
                          !Number of gaussian latitudes; Number of latitudes on each cpu; 
                          !Number of regions in regional rescaling (latitude dependent)
      REAL(KIND=KIND_EVOD), DIMENSION(:),    ALLOCATABLE :: slat_work     !sin(each Gauss latitude)
      REAL(KIND=KIND_EVOD), DIMENSION(:),    ALLOCATABLE :: wlat_work     !cos(each Gauss latitude)
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), ALLOCATABLE :: factor1_work
                                            !rescaling factor for each GAUSS Latitude of each MEMBER
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER     :: KE_work

      REAL(KIND=KIND_EVOD)                           :: RS
      REAL(KIND=KIND_EVOD)                           :: RS_GLOBAL
      REAL(KIND=KIND_EVOD), DIMENSION(4)             :: PARAM
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: work1
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: KEMAX
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: work3
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: work5

      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: t_mean
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: u_mean
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: v_mean

      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: ps_mean

      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: tm_mean
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: um_mean
      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: vm_mean

      REAL(KIND=KIND_EVOD), DIMENSION(:),    POINTER :: psm_mean

      CHARACTER(20),        DIMENSION(:, :), POINTER :: vname

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: tracer_mean
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: tracerm_mean

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: t_step1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: u_step1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: v_step1

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: ps_step1

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: tm_step1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: um_step1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: vm_step1

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: psm_step1

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: t
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: u
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: v

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: ps

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: psw

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: tm
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: um
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: vm

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: psm

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: pswm

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: t6
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: u6
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: v6

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: ps6

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: t6m
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: u6m
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: v6m

      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: ps6m

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tw
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: uw
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: vw

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: twm
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: uwm
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: vwm

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracer_step1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracerm_step1

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracer
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracerm
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracer6
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: tracer6m

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: work2

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :, :), POINTER :: tracerw
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :, :), POINTER :: tracerwm
 END TYPE ENS_Cpl_InternalState

! This state is supported by C pointer not F90 pointer, thus
! need this wrap.
!-----------------------------------------------------------
 TYPE ENS_Cpl_wrap
     TYPE (ENS_Cpl_InternalState), POINTER :: Cpl_Int_State
 END TYPE ENS_Cpl_wrap

 END MODULE ENS_Cpl_InternalState_ESMFMod
