! May 2011 Weiyu yang, Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!--------------------------------------------------------------------------

 SUBROUTINE DistributeForStep1_1(Array, Cpl_Int_State, rc)

#include "../ESMFVersionDefine.h"

 USE machine, ONLY: KIND_EVOD
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 INCLUDE 'mpif.h'

 TYPE(ENS_Cpl_InternalState),                                   INTENT(inout) :: Cpl_Int_State
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%ARRAY_ONE_SIZ3), INTENT(inout) :: Array
 INTEGER,                                                       INTENT(out)   :: rc

 INTEGER                                                                      :: i, j, k

 rc = ESMF_SUCCESS

 k = 1
 DO j = 1, Cpl_Int_State%arraysize_2
     DO i = 1, Cpl_Int_State%arraysize_1
         Cpl_Int_State%work3(k) = Cpl_Int_State%work1(i, j)
         k = k + 1
     END DO
 END DO

! Set all remaining array elements to be not NaNQ.
!-------------------------------------------------
 DO i = k, Cpl_Int_State%ARRAY_ONE_SIZ3
     Cpl_Int_State%work3(i) = 1.1
 END DO

 CALL mpi_alltoall(Cpl_Int_State%work3, Cpl_Int_State%ARRAY_ONE_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   Array,               Cpl_Int_State%ARRAY_ONE_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   MPI_COMM_WORLD, rc)

 IF(ESMF_LogFoundError(rc, msg="Distribute 1-D State in Cpl")) THEN
     PRINT*, 'Error Happened When Distributing 1-D State in Cpl, rc = ', rc
 END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: DistributeForStep1 -- PS"
 ELSE
     PRINT*, "FAIL: DistributeForStep1 -- PS"
 END IF

 END SUBROUTINE DistributeForStep1_1





 SUBROUTINE DistributeForStep1(Array, Name, Cpl_Int_State, rc)

 USE machine, ONLY: KIND_EVOD
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 INCLUDE 'mpif.h'

 CHARACTER(ESMF_MAXSTR),                                        INTENT(in)    :: Name
 TYPE(ENS_Cpl_InternalState),                                   INTENT(inout) :: Cpl_Int_State
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%ARRAY_TOT_SIZ3), INTENT(inout) :: Array
 INTEGER,                                                       INTENT(out)   :: rc

 INTEGER                                                                      :: i, j, k, l

 rc = ESMF_SUCCESS

 k  = 1
 DO l = 1, Cpl_Int_State%arraysize_3
     DO j = 1, Cpl_Int_State%arraysize_2
         DO i = 1, Cpl_Int_State%arraysize_1
             Cpl_Int_State%work5(k) = Cpl_Int_State%work2(i, j, l)
             k = k + 1
         END DO
     END DO
 END DO

! Set all remaining array elements to be not NaNQ.
!-------------------------------------------------
 DO j = k, Cpl_Int_State%ARRAY_TOT_SIZ3
     Cpl_Int_State%work5(j) = 1.1
 END DO

 CALL mpi_alltoall(Cpl_Int_State%work5, Cpl_Int_State%ARRAY_TOT_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   Array,               Cpl_Int_State%ARRAY_TOT_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   MPI_COMM_WORLD, rc)

 IF(ESMF_LogFoundError(rc, msg="Distribute Multi_Level State in Cpl")) THEN
     PRINT*, 'Error Happened When Distributing Multi_level State in Cpl, Name, rc = ', TRIM(Name), rc
 END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: DistributeForStep1 -- ", TRIM(Name)
 ELSE
     PRINT*, "FAIL: DistributeForStep1 -- ", TRIM(Name)
 END IF

 END SUBROUTINE DistributeForStep1





 SUBROUTINE DistributeBackFromStep1_1(Input, Output, Cpl_Int_State, rc)

 USE machine, ONLY: KIND_EVOD
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 INCLUDE 'mpif.h'

 TYPE(ENS_Cpl_InternalState),                                   INTENT(inout) :: Cpl_Int_State
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%ARRAY_ONE_SIZ3), INTENT(in)    :: Input
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%arraysize_4),    INTENT(inout) :: Output
 INTEGER,                                                       INTENT(inout) :: rc

 INTEGER                                                                      :: i

 CALL mpi_alltoall(Input,               Cpl_Int_State%ARRAY_ONE_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   Cpl_Int_State%work3, Cpl_Int_State%ARRAY_ONE_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   MPI_COMM_WORLD, rc)

 IF(ESMF_LogFoundError(rc, msg="Alltoall Back From Step1 -- PS in Cpl")) THEN
     PRINT*, 'Error Happened When Alltoalling Back From Step1 -- PS in Cpl, rc = ', rc
 END IF

 DO i = 1, Cpl_Int_State%arraysize_4
     Output(i) = Cpl_Int_State%work3(i)
 END DO

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: DistributeBackFromStep1 -- PS"
 ELSE
     PRINT*, "FAIL: DistributeBackFromStep1 -- PS"
 END IF

 END SUBROUTINE DistributeBackFromStep1_1





 SUBROUTINE DistributeBackFromStep1(Input, Output, Name, Cpl_Int_State, rc)

 USE machine, ONLY: KIND_EVOD
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 INCLUDE 'mpif.h'

 CHARACTER(ESMF_MAXSTR),                                                                INTENT(in)    :: Name
 TYPE(ENS_Cpl_InternalState),                                                           INTENT(inout) :: Cpl_Int_State
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%ARRAY_ONE_SIZ3),                         INTENT(in)    :: Input
 REAL(KIND=KIND_EVOD), DIMENSION(Cpl_Int_State%arraysize_4, Cpl_Int_State%arraysize_3), INTENT(inout) :: Output
 INTEGER,                                                                               INTENT(out)   :: rc

 INTEGER                                                                                              :: i, j, k

 CALL mpi_alltoall(Input,               Cpl_Int_State%ARRAY_TOT_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   Cpl_Int_State%work5, Cpl_Int_State%ARRAY_TOT_SIZ2, Cpl_Int_State%MPI_R_MPI_R, &
                   MPI_COMM_WORLD, rc)

 IF(ESMF_LogFoundError(rc, msg="Alltoall Multi_Level State Back From Step1 in Cpl")) THEN
     PRINT*, 'Error Happened When Alltoalling Multi_Level State Back From Step1 in Cpl, Name, rc = ', TRIM(Name), rc
 END IF

 k  = 1
 DO j = 1, Cpl_Int_State%arraysize_3
     DO i = 1, Cpl_Int_State%arraysize_4
         Output(i, j) = Cpl_Int_State%work5(k)
         k = k + 1
     END DO
 END DO

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: DistributeBackFromStep1 -- ", TRIM(Name)
 ELSE
     PRINT*, "FAIL: DistributeBackFromStep1 -- ", TRIM(Name)
 END IF

 END SUBROUTINE DistributeBackFromStep1
