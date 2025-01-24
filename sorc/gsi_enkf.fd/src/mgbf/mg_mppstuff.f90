submodule(mg_parameter) mg_mppstuff
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_mppstuff
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Everything related to mpi communication
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   init_mg_MPI -
!   barrierMPI -
!   finishMPI -
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind
implicit none

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine init_mg_MPI(this)
!***********************************************************************
!                                                                      !
!     Initialize mpi                                                   !
!     Create group for filter grid                                     !
!                                                                      !
!***********************************************************************
use mpi

implicit none
class (mg_parameter_type),target:: this
integer(i_kind):: g,m
integer(i_kind), dimension(this%npes_filt):: out_ranks
integer(i_kind):: nf
integer(i_kind)::ierr
integer(i_kind):: color
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------

!***
!***  Initial MPI calls
!***
      call MPI_COMM_RANK(MPI_COMM_WORLD,mype,ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,npes,ierr)
!      call MPI_Barrier(MPI_COMM_WORLD, ierr)

      ! Create a new communicator with MPI_Comm_split
      color=1  ! just create an communicator now for the whole processes
      call MPI_Comm_split(MPI_COMM_WORLD, color, mype, mpi_comm_comp, ierr)
      call MPI_COMM_SIZE(mpi_comm_comp,npes,ierr)

      rTYPE = MPI_REAL
      dTYPE = MPI_DOUBLE
      iTYPE = MPI_INTEGER

!***
!*** Analysis grid
!***

    nx = mod(mype,nxm)+1
    my = (mype/nxm)+1

!***
!***  Define PEs that handle high generations
!***
   
      mype_hgen=-1
      my_hgen=-1

      if( mype < maxpe_filt-nxy(1)) then
        mype_hgen=mype+nxy(1)
      endif
      do g=1,gm
        if(maxpe_fgen(g-1)<= mype_hgen .and. mype_hgen< maxpe_fgen(g)) then
            my_hgen=g
         endif
      enddo
      l_hgen = mype_hgen >-1

!***
!***  Chars
!***
      write(c_mype,1000) mype
 1000 format(i5.5)

!-----------------------------------------------------------------------
!
      call MPI_BARRIER(mpi_comm_comp,ierr)
!
!-----------------------------------------------------------------------
!***
!***  Define group communicator for higher generations
!***
!
!  Associate a group with communicator this@mpi_comm_comp
!
      call MPI_COMM_GROUP(mpi_comm_comp,group_world,ierr)
!
!  Create a new group out of exising group
!
     do nf = 1,npes_filt
       out_ranks(nf)=nf-1
     enddo 

     call MPI_GROUP_INCL(group_world,npes_filt,out_ranks,group_work,ierr)
!
!  Now create a new communicator associated with new group
!    
     call MPI_COMM_CREATE(mpi_comm_comp, group_work, mpi_comm_work, ierr)

    if( mype < npes_filt) then

      call MPI_COMM_RANK(mpi_comm_work,mype_gr,ierr)
      call MPI_COMM_SIZE(mpi_comm_work,npes_gr,ierr)

   else
       
      mype_gr= -1
      npes_gr= npes_filt
 
   endif 

!-----------------------------------------------------------------------
!
      call MPI_BARRIER(mpi_comm_comp,ierr)
!
!-----------------------------------------------------------------------
endsubroutine init_mg_MPI

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine barrierMPI(this)
!***********************************************************************
!                                                                      !
!     Call barrier for all                                             !
!                                                                      !
!***********************************************************************
use mpi

implicit none
class(mg_parameter_type),target::this
integer(i_kind):: ierr
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------

      call MPI_BARRIER(mpi_comm_comp,ierr)

!-----------------------------------------------------------------------
endsubroutine barrierMPI

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine finishMPI(this)
!***********************************************************************
!                                                                      !
!     Finalize MPI                                                     !
!                                                                      !
!***********************************************************************
use mpi

implicit none
class(mg_parameter_type),target::this
!
! don't need mpi_finalize if mgbf is a lib to be called from outside
!
      call MPI_FINALIZE(this%ierr)
      stop
!
!-----------------------------------------------------------------------
endsubroutine finishMPI

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_mppstuff

