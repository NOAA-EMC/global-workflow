!-----------------------------------------------------------------------
      module write_internal_state
!
!-----------------------------------------------------------------------
!***  the internal state of the write component.
!-----------------------------------------------------------------------
!***
!***  revision history
!***
!       Feb 2017:  J. Wang - Initial code
!
!-----------------------------------------------------------------------
!
      use esmf
!
!-----------------------------------------------------------------------
!
      implicit none
!
!-----------------------------------------------------------------------
!
      type wrt_internal_state

!--------------------------------
! pe information and task layout
!--------------------------------
!
      integer :: mype
      integer :: petcount
!
!--------------------
!*** grid information
!--------------------
      character(64)   :: output_grid
      type(esmf_grid) :: wrtgrid
!
!-----------------------------
!***  full domain information
!-----------------------------
!
      integer,dimension(:),allocatable :: im
      integer,dimension(:),allocatable :: jm
      integer,dimension(:),allocatable :: lm
!
!--------------------------
!*** file bundle for output
!--------------------------
      integer :: FBCount
      integer,dimension(:), allocatable           :: ncount_attribs
      integer,dimension(:), allocatable           :: ncount_fields
      character(128),dimension(:),allocatable     :: wrtFB_names
      character(128),dimension(:,:),allocatable   :: field_names
!
!-----------------------------------------------------------------------
!***  THE OUTPUT FILE
!-----------------------------------------------------------------------
!
      integer                 :: num_files
!
!-----------------------------------------------------------------------
!***  THE OUTPUT FILE
!-----------------------------------------------------------------------
!
      type(ESMF_FieldBundle),dimension(:),allocatable  :: wrtFB
!
!-------------------------------------
!***  Times used in history filenames
!-------------------------------------
!
      type(ESMF_Time)         :: io_basetime
      type(ESMF_TimeInterval) :: io_currtimediff
      real                    :: nfhour
!
!-----------------------------------------
!***  I/O direction flags (Read or Write)
!-----------------------------------------
!
      logical :: write_nemsioflag
      logical :: write_netcdfflag
!
!-----------------------------------------
!***  POST flags  (place holder for now)
!-----------------------------------------
!
      logical                  :: WRITE_DOPOST
      character(esmf_maxstr)   :: POST_GRIBVERSION
      integer                  :: nlunit           
      character(80)            :: post_namelist
 
!-----------------------------------------------------------------------
!
      end type wrt_internal_state
!
!-----------------------------------------------------------------------
!***  THIS STATE IS SUPPORTED BY C POINTERS BUT NOT F90 POINTERS
!***  THEREFORE WE NEED THIS WRAP.
!-----------------------------------------------------------
!
      type write_wrap
        type(wrt_internal_state),pointer :: write_int_state
      end type write_wrap

!-----------------------------------------------------------
!
      end module write_internal_state
!
!-----------------------------------------------------------
