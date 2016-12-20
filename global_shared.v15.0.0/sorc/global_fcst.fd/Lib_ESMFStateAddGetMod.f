!BOP
!
! !MODULE: Lib_ESMFStateAddGetMod --- a class attaching a F90 array to an 
!                                     ESMF state, getting F90 pointer and 
!                                     providing related services
!
! !INTERFACE:
!
 MODULE Lib_ESMFStateAddGetMod

!USES:
  USE ESMF_Mod, ONLY :                          &
      ESMF_Grid,                                &
      ESMF_GridGet,                             &
      ESMF_DistGrid,                            &
      ESMF_State,                               &
      ESMF_StateAdd,                            &
      ESMF_StateGet,                            &
      ESMF_SUCCESS,                             &
      ESMF_Array,                               &
      ESMF_ArrayCreate,                         &
      ESMF_ArrayGet,                            &
      ESMF_ArrayDestroy,                        &
      ESMF_CopyFlag

 IMPLICIT NONE
 PRIVATE
  
!
! !PUBLIC TYPES:.
!
! !PUBLIC MEMBER FUNCTIONS:
!
  Public AddF90ArrayToState  ! Adds new allocated F90 array to an ESMF State
  Public GetF90ArrayFromState
!
! !DESCRIPTION:

! !REVISION HISTORY:
!
! 20oct2003 Zaslavsky   Initial code.
! 07Dec2003 Cruz        Added real(4) methods for 2D, 3D
! March 26, 2004, Weiyu Yang, modified for the ESMF 1.0.6 version.
! September 3, 2004, Weiyu Yang, modified for the NCEP GFS model.
! July 2005,            Weiyu Yang added real(4) for 1D and changed real to real(8).
! May 2006, Weiyu Yang modIFied the code for the ESMF 3.0.0 library
!                      and adding the releasing memory option after
!                      getting a fortran array from the state.
! June 2006, Weiyu Yang, modified the code to reduce the maximum memory requirement.
! April 2007 , S. Moorthi Added Weiyu's upgrades to use ESMF 3.0.0 library
! September 2007       Weiyu Yang updated to use the ESMF 3.0.3 library.
! May 2008             Weiyu Yang updated to use the ESMF 3.1.0r library
!
!EOP
!-------------------------------------------------------------------------

!BOC

  Interface AddF90ArrayToState
     module procedure AddF90IntegerArrayToState1D
     module procedure AddF90IntegerArrayToState2D
     module procedure AddF90Real8ArrayToState1D
     module procedure AddF90Real4ArrayToState1D
     module procedure AddF90Real8ArrayToState2D
     module procedure AddF90Real4ArrayToState2D
     module procedure AddF90Real8ArrayToState3D
     module procedure AddF90Real4ArrayToState3D
     module procedure AddF90Real8ArrayToState4D
     module procedure AddF90Real4ArrayToState4D
  end interface AddF90ArrayToState
  
  Interface GetF90ArrayFromState
     module  procedure GetF90IntegerArrayFromState1D
     module  procedure GetF90IntegerArrayFromState2D
     module  procedure GetF90Real8ArrayFromState1D
     module  procedure GetF90Real4ArrayFromState1D
     module  procedure GetF90Real8ArrayFromState2D
     module  procedure GetF90Real4ArrayFromState2D
     module  procedure GetF90Real8ArrayFromState3D
     module  procedure GetF90Real4ArrayFromState3D
     module  procedure GetF90Real8ArrayFromState4D
     module  procedure GetF90Real4ArrayFromState4D
  end interface GetF90ArrayFromState

!EOC

!---------------------------------------------------------------------------

CONTAINS


!BOP
!!
! !IROUTINE: Adds an allocated 1D integer array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90IntegerArrayToState1D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 INTEGER, DIMENSION(:), POINTER       :: F90array
 TYPE(ESMF_Grid),       INTENT(inout) :: grid
 CHARACTER(LEN = *),    INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),      INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,     INTENT(out)   :: rc ! 0 sucess;  
                                            ! 1 F90array is not associated; 
                                            ! 2 failure to get the ESMF DistGrid.
                                            ! 3 failure to create an ESMF array.
                                            ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               an adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                  :: distgrid
 TYPE(ESMF_Array)                     :: ESMFArray
 INTEGER                              :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc=status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90IntegerArrayToState1D





!BOP
!!
! !IROUTINE: Adds an allocated 2D integer array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90IntegerArrayToState2D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 INTEGER, DIMENSION(:, :), POINTER       :: F90array
 TYPE(ESMF_Grid),          INTENT(inout) :: grid
 CHARACTER(LEN = *),       INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),         INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,        INTENT(out)   :: rc ! 0 sucess;  
                                               ! 1 F90array is not associated; 
                                               ! 2 failure to get the ESMF DistGrid.
                                               ! 3 failure to create an ESMF array.
                                               ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                     :: distgrid
 TYPE(ESMF_Array)                        :: ESMFArray
 INTEGER                                 :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90IntegerArrayToState2D





!BOP
!!
! !IROUTINE: Adds an allocated 1D real 8 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real8ArrayToState1D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(8), DIMENSION(:), POINTER       :: F90array
 TYPE(ESMF_Grid),       INTENT(inout) :: grid
 CHARACTER(LEN = *),    INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),      INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,     INTENT(out)   :: rc ! 0 sucess;  
                                            ! 1 F90array is not associated; 
                                            ! 2 failure to get the ESMF DistGrid.
                                            ! 3 failure to create an ESMF array.
                                            ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                  :: distgrid
 TYPE(ESMF_Array)                     :: ESMFArray
 INTEGER                              :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real8ArrayToState1D





!BOP
!!
! !IROUTINE: Adds an allocated 1D real 4 array to ESMF State (through an
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real4ArrayToState1D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(4), DIMENSION(:), POINTER       :: F90array
 TYPE(ESMF_Grid),       INTENT(inout) :: grid
 CHARACTER(LEN = *),    INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),      INTENT(inout) :: state

!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,     INTENT(out)   :: rc ! 0 sucess;
                                            ! 1 F90array is not associated;
                                            ! 2 failure to get the ESMF DistGrid.
                                            ! 3 failure to create an ESMF array.
                                            ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                  :: distgrid
 TYPE(ESMF_Array)                     :: ESMFArray
 INTEGER                              :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real4ArrayToState1D





!BOP
!!
! !IROUTINE: Adds an allocated 2D real 8 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real8ArrayToState2D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(8), DIMENSION(:, :), POINTER       :: F90array
 TYPE(ESMF_Grid),          INTENT(inout) :: grid
 CHARACTER(LEN = *),       INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),         INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,        INTENT(out)   :: rc ! 0 sucess;  
                                               ! 1 F90array is not associated; 
                                               ! 2 failure to get the ESMF DistGrid.
                                               ! 3 failure to create an ESMF array.
                                               ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                     :: distgrid
 TYPE(ESMF_Array)                        :: ESMFArray
 INTEGER                                 :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real8ArrayToState2D





!BOP
!!
! !IROUTINE: Adds an allocated 2D real 4 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real4ArrayToState2D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(4), DIMENSION(:, :), POINTER       :: F90array
 TYPE(ESMF_Grid),          INTENT(inout) :: grid
 CHARACTER(LEN = *),       INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),         INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,        INTENT(out)   :: rc ! 0 sucess;  
                                               ! 1 F90array is not associated; 
                                               ! 2 failure to get the ESMF DistGrid.
                                               ! 3 failure to create an ESMF array.
                                               ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                     :: distgrid
 TYPE(ESMF_Array)                        :: ESMFArray
 INTEGER                                 :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real4ArrayToState2D





!BOP
!!
! !IROUTINE: Adds an allocated 3D real 8 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real8ArrayToState3D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(8), DIMENSION(:, :, :), POINTER       :: F90array
 TYPE(ESMF_Grid),             INTENT(inout) :: grid
 CHARACTER(LEN = *),          INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),  INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),            INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,           INTENT(out)   :: rc ! 0 sucess;  
                                                  ! 1 F90array is not associated; 
                                                  ! 2 failure to get the ESMF DistGrid.
                                                  ! 3 failure to create an ESMF array.
                                                  ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                        :: distgrid
 TYPE(ESMF_Array)                           :: ESMFArray
 INTEGER                                    :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real8ArrayToState3D





!BOP
!!
! !IROUTINE: Adds an allocated 3D real 4 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real4ArrayToState3D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(4), DIMENSION(:, :, :), POINTER       :: F90array
 TYPE(ESMF_Grid),             INTENT(inout) :: grid
 CHARACTER(LEN = *),          INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),  INTENT(in), OPTIONAL :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),            INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,           INTENT(out)   :: rc ! 0 sucess;  
                                                  ! 1 F90array is not associated; 
                                                  ! 2 failure to get the ESMF DistGrid.
                                                  ! 3 failure to create an ESMF array.
                                                  ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                        :: distgrid
 TYPE(ESMF_Array)                           :: ESMFArray
 INTEGER                                    :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real4ArrayToState3D





!BOP
!!
! !IROUTINE: Adds an allocated 4D real 8 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real8ArrayToState4D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(8), DIMENSION(:, :, :, :), POINTER       :: F90array
 TYPE(ESMF_Grid),                INTENT(inout) :: grid
 CHARACTER(LEN = *),             INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL   :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),               INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,              INTENT(out)   :: rc ! 0 sucess;  
                                                     ! 1 F90array is not associated; 
                                                     ! 2 failure to get the ESMF DistGrid.
                                                     ! 3 failure to create an ESMF array.
                                                     ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                           :: distgrid
 TYPE(ESMF_Array)                              :: ESMFArray
 INTEGER                                       :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real8ArrayToState4D





!BOP
!!
! !IROUTINE: Adds an allocated 4D real 4 array to ESMF State (through an 
!            ESMF array and related ESMF DistGrid in the ESMF grid).
!
! !INTERFACE:
!
 SUBROUTINE AddF90Real4ArrayToState4D(state, grid, name, F90Array, copyflag, rc)

!
! !USES:
!
 IMPLICIT none

! INPUT PARAMETERS:

 REAL(4), DIMENSION(:, :, :, :), POINTER       :: F90array
 TYPE(ESMF_Grid),                INTENT(inout) :: grid
 CHARACTER(LEN = *),             INTENT(in)    :: name

 TYPE(ESMF_CopyFlag),   INTENT(in), OPTIONAL   :: copyflag

! INPUT/OUTPUT PARAMETERS:

 TYPE(ESMF_State),               INTENT(inout) :: state
    
!
! !OUTPUT PARAMETERS:
!

 INTEGER, OPTIONAL,              INTENT(out)   :: rc ! 0 sucess;  
                                                     ! 1 F90array is not associated; 
                                                     ! 2 failure to get the ESMF DistGrid.
                                                     ! 3 failure to create an ESMF array.
                                                     ! 4 failure to add the ESMF array to the ESMF state.
!
! !DESCRIPTION: This subroutine takes an allocated F90 array, creates
!               an ESMF Array, associated wirh the ESMF DistGrid in the given grid,
!               and adds it to a given ESMF state.
!
! !REVISION HISTORY:
!
! 20oct2003  Zaslavsky   Initial code.
! 10/01/2007 Weiyu Yang  Rewritting for the ESFM 3.0.3 version.
! May 2008   Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
!EOP
!-------------------------------------------------------------------------
 TYPE(ESMF_DistGrid)                           :: distgrid
 TYPE(ESMF_Array)                              :: ESMFArray
 INTEGER                                       :: status

 status = ESMF_SUCCESS
 IF(ASSOCIATED(F90Array)) THEN
      CALL ESMF_GridGet(grid, DistGrid = distgrid, rc = status)

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 2
         RETURN
      END IF

      IF(PRESENT(copyflag)) THEN
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              copyflag = copyflag, name = name, rc = status)
      ELSE
          ESMFArray = ESMF_ArrayCreate(F90Array, distgrid, &
              name = name, rc = status)
      END IF

      IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         return
      END IF

      CALL ESMF_StateAdd(state, ESMFArray, rc = status)

      IF(status /= ESMF_SUCCESS ) THEn
           IF(PRESENT(rc)) rc = 4
           RETURN
      END IF
 ELSE
      IF(PRESENT(rc)) rc = 1
          PRINT *, ' AddF90...F90 array is NOT ASSOCIATED '
      RETURN
 END IF
   
 IF(PRESENT(rc)) rc = status

 END SUBROUTINE AddF90Real4ArrayToState4D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 1D integer array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90IntegerArrayFromState1D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 INTEGER, DIMENSION(:),           POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90IntegerArrayFromState1D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 2D integer array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90IntegerArrayFromState2D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 INTEGER, DIMENSION(:, :),        POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90IntegerArrayFromState2D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 1D real 8 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real8ArrayFromState1D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(8), DIMENSION(:),           POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real8ArrayFromState1D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 1D real 4 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real4ArrayFromState1D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(4), DIMENSION(:),           POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real4ArrayFromState1D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 2D real 8 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real8ArrayFromState2D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(8), DIMENSION(:, :),        POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real8ArrayFromState2D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 2D real 4 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real4ArrayFromState2D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(4), DIMENSION(:, :),        POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real4ArrayFromState2D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 3D real 8 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real8ArrayFromState3D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(8), DIMENSION(:, :, :),     POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real8ArrayFromState3D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 3D real 4 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real4ArrayFromState3D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(4), DIMENSION(:, :, :),     POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real4ArrayFromState3D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 4D real 8 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real8ArrayFromState4D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(8), DIMENSION(:, :, :, :),  POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real8ArrayFromState4D





!BOP
!!
! !IROUTINE: Gets a F90 pointer to a F90 4D real 4 array 
!            from the ESMF array which is in the ESMF State.
!
! !INTERFACE:

 SUBROUTINE GetF90Real4ArrayFromState4D(state, name, F90Array, &
     localPE, nestedStateName, DestroyArray, rc)

!
! !USES:
!
 IMPLICIT NONE

! INPUT PARAMETERS:

 TYPE(ESMF_State)                          :: state  
                                                 ! ESMF state to extract F90 array from
 CHARACTER(LEN = *)                        :: name   
                                                 ! name of the ESMF array to extract 
                                                 ! the fortran array from

 INTEGER,            INTENT(in)            :: localPE 
                                                 ! PE id of the local PE.

 CHARACTER(LEN = *), INTENT(in), OPTIONAL  :: nestedStateName   
                                                 ! Name of the nested ESMF state which contains
                                                 ! the ESMF array.

 INTEGER,            INTENT(in), OPTIONAL  :: DestroyArray  
                                                 ! If 1, THEN destroy the ESMF array.

! OUTPUT PARAMETERS:

 REAL(4), DIMENSION(:, :, :, :),  POINTER  :: F90Array
 INTEGER,            INTENT(out), OPTIONAL :: rc ! 0 sucess;  
                                                 ! 1 failure to get ESMF array
                                                 !   from the ESMF state; 
                                                 ! 2 failure to get a F90 POINTER from
                                                 !   the ESMF array.
                                                 ! 3 failure to destroy the ESMF array.

!
! !DESCRIPTION: This subroutine gets a F90 pointer to F90 array from 
!               given ESMF state assuming that the name of corresponding 
!               ESMF array is provided.

! !REVISION HISTORY:
!
! 20oct2003        Zaslavsky  Initial code.
! March 26, 2004,  Weiyu Yang modified for the ESMF 1.0.6 version.
! April 05, 2007,  S. Moorthi added WeiYu's modifications for ESMF 3.0.0 (adding
!                             the destroy field option
! Ootober 01, 2007 Weiyu Yang Rewritting for the ESFM 3.0.3 version.
! May 2008         Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!EOP
!-------------------------------------------------------------------------    
 TYPE(ESMF_Array)                          :: ESMFArray
 INTEGER                                   :: status

 status = ESMF_SUCCESS

 IF(PRESENT(nestedStateName)) THEN
     CALL ESMF_StateGet(state, name, ESMFArray,           &
         nestedStateName = nestedStateName, rc = status)
 ELSE
     CALL ESMF_StateGet(state, name, ESMFArray, rc = status)
 END IF

 IF(status /= ESMF_SUCCESS) THEN
     IF(PRESENT(rc)) rc = 1
     RETURN
 END IF

 IF(ASSOCIATED(F90Array)) NULLIFY(F90Array)

 CALL ESMF_ArrayGet(ESMFArray, localPE, F90Array, rc = status)

 IF(status /= ESMF_SUCCESS ) THEN
     IF(PRESENT(rc)) rc = 2
     RETURN
 END IF

 IF(PRESENT(DestroyArray) .AND. DestroyArray == 1) THEN
     CALL ESMF_ArrayDestroy(ESMFArray, rc = status)

     IF(status /= ESMF_SUCCESS) THEN
         IF(PRESENT(rc)) rc = 3
         RETURN
     END IF
 END IF

 IF(PRESENT(rc)) rc = status

 END SUBROUTINE GetF90Real4ArrayFromState4D

 END MODULE Lib_ESMFStateAddGetMod
