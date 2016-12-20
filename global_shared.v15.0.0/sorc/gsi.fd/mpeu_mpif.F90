module mpeu_mpif
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module mpeu_mpif
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: a portable interface to include "mpif.h" for MPI.
!
! program history log:
!   2010-03-22  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

   implicit none
   private          ! except

   public :: MPI_INTEGER1
   public :: MPI_INTEGER2
   public :: MPI_INTEGER4
   public :: MPI_INTEGER8

   public :: MPI_INTEGER
   public :: MPI_REAL
   public :: MPI_DOUBLE_PRECISION
   public :: MPI_LOGICAL
   public :: MPI_CHARACTER

   public :: MPI_2INTEGER
   public :: MPI_2REAL
   public :: MPI_2DOUBLE_PRECISION

   public :: MPI_REAL4
   public :: MPI_REAL8
   public :: MPI_REAL16

   public :: MPI_COMM_WORLD
   public :: MPI_COMM_NULL
   public :: MPI_REQUEST_NULL

   public :: MPI_SUM
   public :: MPI_PROD
   public :: MPI_MIN
   public :: MPI_MAX
   public :: MPI_MINLOC
   public :: MPI_MAXLOC

   public :: MPI_MAX_ERROR_STRING
   public :: MPI_STATUS_SIZE
   public :: MPI_ERROR

!#if !defined(sysLinux)
   public :: MPI_OFFSET_KIND
   public :: MPI_INFO_NULL
   public :: MPI_MODE_RDONLY
   public :: MPI_MODE_RDWR
   public :: MPI_SEEK_SET
!#endif
   public :: MPI_BYTE

#ifdef MPICH_
   public :: MPIPRIV     ! the common block name
#endif

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: mpeu_mpif - a portable interface to the MPI "mpif.h" COMMONs.
!
! !DESCRIPTION:
!
!   The purpose of \verb"mpeu_mpif" module is to provide a portable
!   interface of \verb"mpif.h" with different MPI implementation.
!   By combining module \verb"mpeu_mpif" and \verb"mpeu_mpif90", it may be
!   possible to build a Fortran 90 MPI binding module graduately.
!
!   Although it is possible to use \verb'include "mpif.h"' directly
!   in individual modules, it has several problems:
!   \begin{itemize}
!   \item It may conflict with either the source code of a {\sl fixed}
!      format or the code of a {\sl free} format;
!   \item It does not provide the protection and the safety of using
!     these variables as what a \verb"MODULE" would provide.
!   \end{itemize}
!
!   More information may be found in the module \verb"mpeu_mpif90".
!
! !INTERFACE:

include "mpif.h"

! !REVISION HISTORY:
!    01Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!    16Feb05 - Todling - added a number of vars needed by GSI
!    18Mar05 - Todling - added a few more mpi vars used by GSI
!    20Mar05 - Todling - see no reason for sysLinux ifdef (commented out)
!    18Feb09 - Jing Guo <Jing.Guo@nasa.gov>
!     - Copied from GMAO_mpeu/m_mpif.F here to avoid the
!       dependency of GSI_GridComp/ on GMAO_mpeu.
!     - Renamed to mpeu_mpif to avoid possible name conflict
!       with GMAO_mpeu/.
!    23Feb09 - Jing Guo <Jing.Guo@nasa.gov>
!     - Renamed to *.F90, by assuming the system mpif.h is
!       compatible with free-format Fortran.
!EOP
!_______________________________________________________________________
    character(len=*),parameter :: myname='mpeu_mpif'

end module mpeu_mpif
!.
