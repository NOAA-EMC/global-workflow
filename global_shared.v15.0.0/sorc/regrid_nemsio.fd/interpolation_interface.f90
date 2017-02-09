module interpolation_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use namelist_def
  use netcdf
  use netcdfio_interface
  use mpi_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: interpolation_initialize_gridvar
  public :: interpolation_initialize_esmf
  public :: interpolation_define_gridvar
  public :: interpolation_define_gridvar_out
  public :: interpolation_esmf
  public :: interpolation_esmf_vect
  public :: gridvar
  public :: esmfgrid

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type esmfgrid
     character(len=500)                                                :: filename
     real(r_double),                    dimension(:),      allocatable :: s
     integer,                           dimension(:),      allocatable :: col
     integer,                           dimension(:),      allocatable :: row
     real(r_double),                    dimension(:),      allocatable :: inlats
     real(r_double),                    dimension(:),      allocatable :: inlons
     real(r_double),                    dimension(:),      allocatable :: outlats
     real(r_double),                    dimension(:),      allocatable :: outlons
     integer                                                           :: n_s,n_a,n_b
  end type esmfgrid ! type esmfgrid

  type gridvar
     logical,                           dimension(:),      allocatable :: check
     real(r_double),                    dimension(:),      allocatable :: var
     integer                                                           :: ncoords
     integer                                                           :: nx
     integer                                                           :: ny
  end type gridvar ! type gridvar

  ! Define global variables

  integer                                                              :: ncfileid
  integer                                                              :: ncvarid
  integer                                                              :: ncdimid
  integer                                                              :: ncstatus

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  subroutine interpolation_define_gridvar(grid,xdim,ydim,ngrid,input)
!  collapses the cubed grid into a 1-d array
!  Define variables passed to routine

    use nemsio_module, only: nemsio_realkind
    integer,intent(in)                         :: ngrid
    integer,intent(in)                         :: xdim,ydim
    type(gridvar),intent(inout)                :: grid
    real(nemsio_realkind),intent(in)           :: input(ngrid,xdim,ydim)

! locals
    integer                 	     :: i,j,k,ncount

    ncount = 1
    do k = 1, ngrid
       do j = 1, ydim
          do i = 1, xdim
             grid%var(ncount) = input(k,i,j)
             ncount           = ncount + 1
          end do
       end do
    end do


  end subroutine interpolation_define_gridvar

!=======================================================================


  subroutine interpolation_define_gridvar_out(grid,xdim,ydim,output)
! make a 2-d array for output
    ! Define variables passed to routine

    integer,intent(in)                :: xdim,ydim
    type(gridvar),intent(in)          :: grid
    real(r_double),intent(out)         :: output(xdim,ydim)

! locals
    integer               :: i,j,ncount

    ncount = 1 
    do j = 1, ydim
       do i = 1, xdim
          output(j,i) = grid%var(ncount)
          ncount      = ncount + 1
       enddo
    enddo

  end subroutine interpolation_define_gridvar_out

  !=======================================================================

  subroutine interpolation_initialize_gridvar(grid)

    ! Define variables passed to routine

    type(gridvar)                                                        :: grid

    allocate(grid%var(grid%ncoords))

  end subroutine interpolation_initialize_gridvar


!======================================================================= 
  
  subroutine interpolation_initialize_esmf(grid)

    ! Define variables passed to routine

    type(esmfgrid)                                                       :: grid

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(grid%filename)),mode=           &
         & nf90_nowrite,ncid=ncfileid)
    ncstatus = nf90_inq_dimid(ncfileid,'n_s',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_s)
    ncstatus = nf90_inq_dimid(ncfileid,'n_a',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_a)
    ncstatus = nf90_inq_dimid(ncfileid,'n_b',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_b)
    

    ! Allocate memory for local variables

    allocate(grid%s(grid%n_s))
    allocate(grid%row(grid%n_s))
    allocate(grid%col(grid%n_s))
    
    allocate(grid%inlats(grid%n_a))
    allocate(grid%inlons(grid%n_a))
    allocate(grid%outlats(grid%n_b))
    allocate(grid%outlons(grid%n_b))

    ncstatus = nf90_inq_varid(ncfileid,'col',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%col)
    ncstatus = nf90_inq_varid(ncfileid,'row',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%row)
    ncstatus = nf90_inq_varid(ncfileid,'S',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%s)
    ncstatus = nf90_inq_varid(ncfileid,'yc_a',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%inlats)
    ncstatus = nf90_inq_varid(ncfileid,'xc_a',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%inlons)
    where(grid%inlons .LT. 0.0) 
       grid%inlons=360+grid%inlons
    endwhere
    ncstatus = nf90_inq_varid(ncfileid,'yc_b',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%outlats)
    ncstatus = nf90_inq_varid(ncfileid,'xc_b',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%outlons)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine interpolation_initialize_esmf


!=======================================================================


  subroutine interpolation_esmf(invar,outvar,grid,is_nrstnghbr)

    ! Define variables passed to routine

    type(gridvar)                                                        :: invar
    type(gridvar)                                                        :: outvar
    logical                                                              :: is_nrstnghbr 

    type(esmfgrid)                                                       :: grid

    integer                                                              :: i, j, k, l

    outvar%var   = dble(0.0)

    if(is_nrstnghbr) then
       do i = 1, grid%n_s
          outvar%var(grid%row(i)) = invar%var(grid%col(i))
       enddo
    else
       do i = 1, grid%n_s
          outvar%var(grid%row(i)) = outvar%var(grid%row(i)) + grid%s(i)*invar%var(grid%col(i))
       end do
    end if
    
  end subroutine interpolation_esmf
!=====================================================================  

    subroutine interpolation_esmf_vect(invaru,invarv,grid,outvaru,outvarv)

    ! Define variables passed to routine

    type(gridvar)                                                        :: invaru,invarv
    type(gridvar)                                                        :: outvaru,outvarv
    type(esmfgrid)                                                       :: grid

    integer                                                              :: i, j, k, l
    real(r_double) :: cxy,sxy,urot,vrot


    outvaru%var   = dble(0.0)
    outvarv%var   = dble(0.0)

    do i = 1, grid%n_s
       CALL MOVECT(grid%inlats(grid%col(i)),grid%inlons(grid%col(i)),&
                   grid%outlats(grid%row(i)),grid%outlons(grid%row(i)),&
                           cxy,sxy)
       urot=cxy*invaru%var(grid%col(i))-sxy*invarv%var(grid%col(i))
       vrot=sxy*invaru%var(grid%col(i))+cxy*invarv%var(grid%col(i))
       outvaru%var(grid%row(i)) = outvaru%var(grid%row(i)) +  grid%s(i)*urot
       outvarv%var(grid%row(i)) = outvarv%var(grid%row(i)) +  grid%s(i)*vrot
       
    end do

  end subroutine interpolation_esmf_vect

!===================================================================== 

 SUBROUTINE MOVECT(FLAT,FLON,TLAT,TLON,CROT,SROT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  MOVECT     MOVE A VECTOR ALONG A GREAT CIRCLE
!   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
!
! ABSTRACT: THIS SUBPROGRAM PROVIDES THE ROTATION PARAMETERS
!           TO MOVE A VECTOR ALONG A GREAT CIRCLE FROM ONE
!           POSITION TO ANOTHER WHILE CONSERVING ITS ORIENTATION
!           WITH RESPECT TO THE GREAT CIRCLE.  THESE ROTATION
!           PARAMETERS ARE USEFUL FOR VECTOR INTERPOLATION.
!
! PROGRAM HISTORY LOG:
!   96-04-10  IREDELL
! 1999-04-08  IREDELL  GENERALIZE PRECISION
!
! USAGE:    CALL MOVECT(FLAT,FLON,TLAT,TLON,CROT,SROT)
!
!   INPUT ARGUMENT LIST:
!     FLAT     - REAL LATITUDE IN DEGREES FROM WHICH TO MOVE THE VECTOR
!     FLON     - REAL LONGITUDE IN DEGREES FROM WHICH TO MOVE THE VECTOR
!     TLAT     - REAL LATITUDE IN DEGREES TO WHICH TO MOVE THE VECTOR
!     TLON     - REAL LONGITUDE IN DEGREES TO WHICH TO MOVE THE VECTOR
!
!   OUTPUT ARGUMENT LIST:
!     CROT     - REAL CLOCKWISE VECTOR ROTATION COSINE
!     SROT     - REAL CLOCKWISE VECTOR ROTATION SINE
!                (UTO=CROT*UFROM-SROT*VFROM;
!                 VTO=SROT*UFROM+CROT*VFROM)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
 IMPLICIT NONE
!
 INTEGER,         PARAMETER     :: KD=SELECTED_REAL_KIND(15,45)
!
 REAL(KIND=r_double),            INTENT(IN   ) :: FLAT, FLON
 REAL(KIND=r_double),            INTENT(IN   ) :: TLAT, TLON
 REAL(KIND=r_double),            INTENT(  OUT) :: CROT, SROT
!
 REAL(KIND=r_double),   PARAMETER     :: CRDLIM=0.9999999
 REAL(KIND=r_double),   PARAMETER     :: PI=3.14159265358979
 REAL(KIND=r_double),   PARAMETER     :: DPR=180./PI
!
 REAL(KIND=r_double)                  :: CTLAT,STLAT,CFLAT,SFLAT
 REAL(KIND=r_double)                  :: CDLON,SDLON,CRD
 REAL(KIND=r_double)                  :: SRD2RN,STR,CTR,SFR,CFR
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE COSINE OF THE RADIAL DISTANCE BETWEEN THE POINTS.
 CTLAT=COS(TLAT/DPR)
 STLAT=SIN(TLAT/DPR)
CFLAT=COS(FLAT/DPR)
 SFLAT=SIN(FLAT/DPR)
 CDLON=COS((FLON-TLON)/DPR)
 SDLON=SIN((FLON-TLON)/DPR)
 CRD=STLAT*SFLAT+CTLAT*CFLAT*CDLON
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE ROTATIONS AT BOTH POINTS WITH RESPECT TO THE GREAT CIRCLE
!  AND COMBINE THEM TO GIVE THE TOTAL VECTOR ROTATION PARAMETERS.
 IF(ABS(CRD).LE.CRDLIM) THEN
   SRD2RN=-1/(1-CRD**2)
   STR=CFLAT*SDLON
   CTR=CFLAT*STLAT*CDLON-SFLAT*CTLAT
   SFR=CTLAT*SDLON
   CFR=CTLAT*SFLAT*CDLON-STLAT*CFLAT
   CROT=SRD2RN*(CTR*CFR-STR*SFR)
   SROT=SRD2RN*(CTR*SFR+STR*CFR)
!  USE A DIFFERENT APPROXIMATION FOR NEARLY COINCIDENT POINTS.
!  MOVING VECTORS TO ANTIPODAL POINTS IS AMBIGUOUS ANYWAY.
 ELSE
   CROT=CDLON
   SROT=SDLON*STLAT
 ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 END SUBROUTINE MOVECT

  !=======================================================================

end module interpolation_interface
