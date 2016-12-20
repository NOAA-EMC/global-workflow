!
! !MODULE: GEFS_Cpl_InternalState_ESMFMod --- Internal state definition of the
!                                             ESMF coupler gridded component of 
!                                             the GFS ensemble system.
!
! !DESCRIPTION: GEFS_Cpl_InternalState_ESMFMod --- Define the GFS ensemble coupler 
!                                                  internal state.
!---------------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  April 2006         Weiyu Yang Initial code.
!  March 2007         Dingchen Hou added Stochatic Perturbation Combination Coefficient array.
!  January to November 2007      Dingchen and Weiyu Yang
!                     Added Broadcasting procedure and Global variables/arrays
!  November 2007      Dingchen, added minimum documentation, mainly for the arrays added during 2007
!
! !INTERFACE:
!
 MODULE GEFS_Cpl_InternalState_ESMFMod

!!USES:
!------
 USE ESMF_Mod
 USE machine, ONLY: KIND_EVOD, KIND_PHYS

 IMPLICIT none

 TYPE GEFS_Cpl_InternalState

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
      INTEGER                        :: TRIEO_LSTOT_SIZ1
      INTEGER                        :: TRIEO_LSTOT_SIZ2
      INTEGER                        :: TRIEO_LSTOT_SIZ3
      INTEGER                        :: TRIEO_LSTOT_SIZ4
      INTEGER                        :: TRIEO_MAX_SIZE
      INTEGER                        :: TRIEO_VERTICAL_SIZE
      INTEGER                        :: TRIE_LS_SIZE, TRIO_LS_SIZE
      INTEGER                        :: jcap, jintmx, ls_dim, ls_max_node
      INTEGER                        :: levs, levh, latgd, latr, MC_COMP
      INTEGER                        :: MPI_R_MPI, nodes_comp, me_comp
      INTEGER                        :: latl, latl2, nvars, latdims
      INTEGER                        :: dimg, lats_node, ipt_lats_node
      INTEGER                        :: londi
      INTEGER                        :: mpi_comm_cplcomp

!  Nov. 2007, Added global arrays
      INTEGER, DIMENSION(:), POINTER :: member_id        !global array specifying the member_id for each CPU
      INTEGER, DIMENSION(:), POINTER :: lats_node_global !global array specifying number of latitudes for each CPU
      INTEGER, DIMENSION(:, :), POINTER :: lats_global   !global array specifying the INDEX of Gaussian latitude 
                                                         !for each LATITUDE and each CPU

! In GFS, the vertical variable distribution is:
!-----------------------------------------------
!      parameter(P_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
!                P_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
!                P_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
!                P_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
!                P_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
!                P_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
!                P_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
!                P_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
!                P_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
!                P_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
!                P_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
!                P_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
!                P_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
!                P_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
!                P_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
!                P_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
!                P_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
!                P_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
!                P_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
!                P_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)

      INTEGER                        :: P_gz, P_zem, P_dim, P_tem, P_rm, P_qm, P_ze
      INTEGER                        :: P_di, P_te, P_rq, P_q, P_dlam, P_dphi, P_uln
      INTEGER                        :: P_vln, P_w, P_x, P_y, P_rt, P_zq
      LOGICAL                        :: lslag
      TYPE(ESMF_LOGICAL)             :: lslag_1

      INTEGER, DIMENSION(:), POINTER :: TRIEO_ST_SIZE
      INTEGER, DIMENSION(:), POINTER :: TRIEO_LS_SIZE
      INTEGER, DIMENSION(:), POINTER :: lat1s
      INTEGER, DIMENSION(:), POINTER :: lon_dims
      INTEGER, DIMENSION(:), POINTER :: lons_lat
      INTEGER, DIMENSION(:), POINTER :: max_ls_nodes
      INTEGER, DIMENSION(:), POINTER :: lats_nodes
      INTEGER, DIMENSION(:), POINTER :: global_lats

      CHARACTER(ESMF_MAXSTR) :: TRIEO_STATE_NAME
      CHARACTER(ESMF_MAXSTR) :: TRIEO_STINI_NAME

      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: trieo_ls_write
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: trie_ls
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: trio_ls

!March 2007,  Global arrys for perturbation combination
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: Sto_Coef 
                                               !Stochastic combination coefficients 

!Nov. 2007,  Global Variables and arrys for Regional Rescaling, 
      INTEGER                         :: latmax, latnode, nreg
                          !Number of gaussian latitudes; Number of latitudes on each cpu; 
                          !Number of regions in regional rescaling (latitude dependent)
      REAL(KIND=KIND_EVOD), DIMENSION(:),    ALLOCATABLE :: slat_work     !sin(each Gauss latitude)
      REAL(KIND=KIND_EVOD), DIMENSION(:),    ALLOCATABLE :: wlat_work     !cos(each Gauss latitude)
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER     :: KE_work
                                            !kinetic energy for each GAUSS Latitude of each MEMBER
      REAL(KIND=KIND_EVOD), DIMENSION(:, :), ALLOCATABLE :: factor1_work
                                            !rescaling factor for each GAUSS Latitude of each MEMBER

      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: vor
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: div
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: t
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: q
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: oz
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: clw
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: u
      REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: v

      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: ps
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: dpdlam
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: dpdphi

      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: trieo_work
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: plnev
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: plnod
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: plnew
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: plnow
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: four_gr1
      REAL(KIND=KIND_EVOD), DIMENSION(:, :),    POINTER :: four_gr2

      INTEGER,              DIMENSION(:, :),    POINTER :: ls_node
      INTEGER,              DIMENSION(:, :),    POINTER :: ls_nodes

      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: epse
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: epso
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: epsedn
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: epsodn
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: snnp1ev
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: snnp1od

!Nov. 2007, each of these 1-d arrays represent of one model state. Dimension is Cpl_Int_State%TRIEO_LSTOT_SIZ3 
!and (11*LEVS+3*LEVH+6) vertical levelsi---each level is a 2-d global field. 
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: trieo_ls            !current state
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: trieo_ls_ini        !past state (e.g, t-6h)
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: trieo_ls_w1         !work array
!     REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: trieo_ls_w2         !work array
      REAL(KIND=KIND_EVOD), DIMENSION(:),       POINTER :: trieo_ls_max        !a special work array

      REAL(ESMF_KIND_R8),   DIMENSION(10)               :: PARM1,   PARM2,   PARM3
      INTEGER,              DIMENSION(10)               :: PARM1_i, PARM2_i, PARM3_i
 END TYPE GEFS_Cpl_InternalState

! This state is supported by C pointer not F90 pointer, thus
! need this wrap.
!-----------------------------------------------------------
 TYPE GEFS_Cpl_wrap
     TYPE (GEFS_Cpl_InternalState), POINTER :: Cpl_Int_State
 END TYPE GEFS_Cpl_wrap

 END MODULE GEFS_Cpl_InternalState_ESMFMod
