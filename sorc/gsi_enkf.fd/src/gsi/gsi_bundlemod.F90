!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_BundleMod --- GSI Bundle
!
! !INTERFACE:
!
! program change log:
! 2018-01-18 G. Ge: change pointer,intent(out) to pointer,intent(inout)
!                   to solve the GSI crash under INTEL v18+
!

module GSI_BundleMod
   
! !USES:

   use kinds, only: i_kind,r_single,r_kind,r_double,r_quad
   use constants, only: zero_single,zero,zero_quad
   use m_rerank, only: rerank
   use mpeu_util, only: perr, die

   implicit none
   private

!
! !PUBLIC MEMBER FUNCTIONS:
!
!  public GSI_1D
!  public GSI_2D
!  public GSI_3D
   public GSI_Bundle           ! Bundle
   public GSI_BundleCreate     ! Create a Bundle
   public GSI_BundleDup        ! Duplicate a Bundle
   public GSI_BundleDPlevs     ! dot product w/ possible "halo"
   public GSI_BundleSum        ! dot product w/ possible "halo"
   public GSI_BundleSet        ! Set Bundle
   public GSI_BundleInquire    ! Inquire about Bundle contents
   public GSI_BundleMerge      ! Merge two Bundles
   public GSI_BundlePrint      ! Print contents of Bundle
   public GSI_BundleGetPointer ! Get pointer to variable
   public GSI_BundleGetVar     ! Get contents of variable
   public GSI_BundlePutVar     ! Put contents in variable
   public GSI_BundleUnset      ! Unset Bundle
   public GSI_BundleDestroy    ! Destroy Bundle
   public assignment(=)        ! Assign to Bundle contents
   public GSI_BundleAssign     ! The same functions as assignment(=) above
   public self_add             ! Add contents of bundles
   public gsi_bundleAddmul     ! Add scaled contents of a bundle
   public self_mul             ! Add contents of bundles
   public gsi_bundlehadamard   ! Hadamard product of contents of two bundles

!  These should be moved out of the bundle soon (gridmod?)
   public GSI_Grid             ! Grid (not yet general)
   public GSI_GridCreate       ! Create a grid

! !METHOD OVERLOADING:

   interface GSI_BundleCreate        ! create bundle from ...
          module procedure create1_  !   scratch
          module procedure create2_  !   existing bundle
          module procedure create3_  !   merging two bundles
   end interface
   interface GSI_BundleDup           ! duplicate a bundle
          module procedure dup_      ! dup(x,y)   -- y = x
          module procedure scl_dup_  !  dup(a,x,y) -- y = a*x
          module procedure sclR4_dup_ !  dup(a,x,y) -- y = a*x
   end interface
   interface GSI_BundleSet           ! set pointer to bundle contents
          module procedure set0_
          module procedure set1_
   end interface
   interface GSI_BundleInquire            ! inquire about bundle ...
          module procedure inquire_char_  !   character contents
   end interface
   interface GSI_BundleMerge         ! merge bundles
          module procedure merge_
   end interface
   interface GSI_BundlePrint         ! print summary of bundle contents
          module procedure print_
   end interface
   interface GSI_BundleGetVar        ! get fiedl(s) from bundle
          module procedure getvar1dr4_   ! real*4 rank-1 field
          module procedure getvar1dr8_   ! real*8 rank-1 field
          module procedure getvar2dr4_   ! real*4 rank-2 field
          module procedure getvar2dr8_   ! real*8 rank-2 field
          module procedure getvar2dp1r4_ ! real*4 rank-2+1 field
          module procedure getvar2dp1r8_ ! real*8 rank-2+1 field
          module procedure getvar3dr4_   ! real*4 rank-3 field
          module procedure getvar3dr8_   ! real*8 rank-3 field
          module procedure getvar3dp1r4_ ! real*4 rank-3+1 field
          module procedure getvar3dp1r8_ ! real*8 rank-3+1 field
   end interface
   interface GSI_BundlePutVar          ! put field(s) in bundle ...
          module procedure putvar0dr4_   !  assign field to real*4 constant
          module procedure putvar0dr8_   !  assign field to real*8 constant
          module procedure putvar1dr4_   !  write to real*4 rank-1 content
          module procedure putvar1dr8_   !  write to real*8 rank-1 content
          module procedure putvar2dr4_   !  write to real*4 rank-2 content
          module procedure putvar2dr8_   !  write to real*8 rank-2 content
          module procedure putvar2dp1r4_ !  write to real*4 rank-2+1 content
          module procedure putvar2dp1r8_ !  write to real*8 rank-2+1 content
          module procedure putvar3dr4_   !  write to real*4 rank-3 content
          module procedure putvar3dr8_   !  write to real*8 rank-3 content
          module procedure putvar3dp1r4_ !  write to real*4 rank-3+1 content
          module procedure putvar3dp1r8_ !  write to real*8 rank-3+1 content
   end interface
   interface GSI_BundleGetPointer    ! get pointer to field(s) in bundle
          module procedure get1_     !   single-field 
          module procedure get2_     !   many-field
          module procedure get31r4_  !   real*4 rank-1 explict pointer (real*4)
          module procedure get31r8_  !   real*8 rank-1 explict pointer (real*8)
          module procedure get32r4_  !   real*4 rank-2 explict pointer (real*8)
          module procedure get32r8_  !   real*8 rank-2 explict pointer (real*4)
          module procedure get33r4_  !   real*4 rank-3 explict pointer (real*4)
          module procedure get33r8_  !   real*8 rank-3 explict pointer (real*8)
   end interface
   interface GSI_BundleUnSet         ! nullify pointers in bundle
          module procedure unset_
   end interface
   interface GSI_BundleDestroy       ! deallocate contents of bundle
          module procedure destroy_
   end interface
   interface GSI_BundleAssign
          module procedure copy_
          module procedure assignR4_const_
          module procedure assignR8_const_
   end interface
   interface assignment (=)
          module procedure copy_
          module procedure assignR4_const_
          module procedure assignR8_const_
   end interface

   interface self_add  ! What we really want here is ASSIGNMENT (+=)
          module procedure self_add_st
          module procedure self_add_R4scal
          module procedure self_add_R8scal
   end interface
   interface gsi_bundleAddmul  ! I believe "addmul" is the conventional name
   ! gs_bundleAddmul(y,a,x) := y+=a*x
          module procedure self_add_R4scal
          module procedure self_add_R8scal
   end interface
   interface self_mul  ! What we really want here is ASSIGNMENT (+=)
          module procedure self_mulR4_
          module procedure self_mulR8_
   end interface
   interface gsi_bundlehadamard
          module procedure hadamard_upd_
   end interface
   interface gsi_bundledplevs  ! needs to be generalized to operate on bundle
          module procedure dplevs2dr4_
          module procedure dplevs2dr8_
          module procedure dplevs3dr4_
          module procedure dplevs3dr8_
   end interface
   interface gsi_bundlesum  ! needs to be generalized to operate on bundle
          module procedure sum2dR4_
          module procedure sum2dR8_
          module procedure sum3dR4_
          module procedure sum3dR8_
   end interface


! !PRIVATE TYPES:

   integer(i_kind), parameter :: MAXSTR=256

   type GSI_Grid                ! simple regular grid for now
      integer(i_kind) :: im=-1  ! dim of 1st rank
      integer(i_kind) :: jm=-1  ! dim of 2nd rank
      integer(i_kind) :: km=-1  ! dim of 3nd rank
                                          ! A more general grid would include
!!    integer(i_kind) :: ihalo  ! halo of 1st dim
!!    integer(i_kind) :: jhalo  ! halo of 2nd dim
!!    integer(i_kind) :: khalo  ! halo of 3nd dim (usually not needed)
!!    real(r_kind), pointer :: lat(:,:)   ! field of latitudes
!!    real(r_kind), pointer :: lon(:,:)   ! field of longitudes
!!    real(r_kind), pointer :: pm (:,:,:) ! field of mid-layer pressures 
!!    real(r_kind), pointer :: pe (:,:,:) ! field of edge pressures
   end type GSI_Grid

   type GSI_1D
      character(len=MAXSTR) :: shortname           ! name, e.g., 'ps'
      character(len=MAXSTR) :: longname            ! longname, e.g., 'Surface Pressure'
      character(len=MAXSTR) :: units               ! units, e.g. 'hPa'
      integer(i_kind)       :: myKind = -1         ! no default
      real(r_single), pointer :: qr4(:) => null()  ! rank-1 real*4 field
      real(r_double), pointer :: qr8(:) => null()  ! rank-1 real*8 field
      real(r_kind),   pointer :: q  (:) => null()  ! points to intrisic rank-1 default precision
   end type GSI_1D

   type GSI_2D
      character(len=MAXSTR) :: shortname
      character(len=MAXSTR) :: longname
      character(len=MAXSTR) :: units
      integer(i_kind)       :: myKind = -1            ! no default
      real(r_single), pointer :: qr4(:,:) => null()   ! rank-2 real*4 field
      real(r_double), pointer :: qr8(:,:) => null()   ! rank-2 real*8 field
      real(r_kind),   pointer :: q  (:,:) => null()   ! points to intrisic rank-2 default precision
   end type GSI_2D

   type GSI_3D
      character(len=MAXSTR) :: shortname
      character(len=MAXSTR) :: longname
      character(len=MAXSTR) :: units
      integer(i_kind)       :: level                  ! level: size of rank3 other than km
      integer(i_kind)       :: myKind = -1            ! no default
      real(r_single), pointer :: qr4(:,:,:) => null() ! rank-3 real*4 field
      real(r_double), pointer :: qr8(:,:,:) => null() ! rank-3 real*8 field
      real(r_kind),   pointer :: q  (:,:,:) => null() ! points to intrisic rank-3 default precision
   end type GSI_3D

! !PUBLIC TYPES:
!
   type GSI_Bundle
      character(len=MAXSTR) :: name
!!#ifdef HAVE_ESMF
!!      type(ESMF_FieldBundle), pointer :: Bundle ! Associated ESMF bundle
!!      type(ESMF_Grid) :: grid                   ! Associated ESMF grid
!!#endif /* HAVE_ESMF */
      integer(i_kind) :: n1d=-1     ! number of 1-d variables
      integer(i_kind) :: n2d=-1     ! number of 2-d variables
      integer(i_kind) :: n3d=-1     ! number of 3-d variables
      integer(i_kind) :: NumVars=-1 ! total number of variables (n1d+n2d+n3d)
      integer(i_kind) :: ndim=-1    ! size of pointer values
      integer(i_kind) :: AllKinds=-1! overall bundle kind (see Remark 9)
      type(GSI_Grid)  :: grid 
      type(GSI_1D),    pointer :: r1(:) => null()
      type(GSI_2D),    pointer :: r2(:) => null()
      type(GSI_3D),    pointer :: r3(:) => null()
      integer(i_kind), pointer :: ival1(:)  => null()
      integer(i_kind), pointer :: ival2(:)  => null()
      integer(i_kind), pointer :: ival3(:)  => null()
      real(r_single),  pointer :: valuesr4(:) => null()
      real(r_double),  pointer :: valuesr8(:) => null()
      real(r_kind),    pointer :: values  (:) => null()
   end type GSI_Bundle

   interface init_     ! internal procedure only - not to become public
          module procedure init1d_
          module procedure init2d_
          module procedure init3d_
   end interface
   interface copy_item_ ! internal procedure only - not to become public
          module procedure copy_item1d_
          module procedure copy_item2d_
          module procedure copy_item3d_
   end interface
   interface clean_    ! internal procedure only - not to become public
          module procedure clean1d_
          module procedure clean2d_
          module procedure clean3d_
   end interface

!
! !DESCRIPTION: This module implements the bundle structure for GSI. 
!  It is meant to be general enough to allow its use in GSI within 
!  both the control and the state vectors. Ultimately, the guess-vector of 
!  GSI could also aim at using the GSI\_Bundle as a general approach to 
!  gathering various fields needed to define the guess.
!
!  A first example of the use of GSI\_Bundle is used in the module 
!  gsi\_chemguess\_mod.F90 that allows adding an arbitrary number of 
!  chemical constituents and species into GSI  --- with a note that 
!  only CO and CO2 are currently known by the internal GSI guess module.
!
!  The GSI\_Bundle is a collection of fields defined on a grid. By definition,
!  the bundle can only keep fields on the same grid. The concept of a GSI\_Bundle
!  is similar to that of an ESMF Bundle.
!
!  This version of GSI\_Bundle is MPI-free. This modules knows nothing about
!  the distribution of the grid. Indeed, it does not need to know. The only
!  procedure that could have an MPI support here is GSI\_BundlePrint, but for
!  now it is simple and ignorant of distributed calling codes.
!
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - initial code, based on discussion w/ Arlindo da Silva 
!                      and his f90/ESMF's SimpleBundle.
!  18Aug2010      Hu - declared GSI_1D, GSI_2D, and GSI_3D as public.
!  28Apr2011 Todling - complete overload to support REAL*4 and REAL*8
!  04Jul2011 Todling - large revision of REAL*4 or REAL*8 implementation
!  27Jun2012 Parrish - set verbose_ to .false. to turn off diagnostic print in subroutine merge_.
!  05Oct2014 Todling - add 4d-like interfaces to getvars
!  26Aug2017   G. Ge - change names(nd) to names(:) to make the passing of assumed size character
!                      array consistent between nested calls                   
!
! !SEE ALSO:  
!           gsi_metguess_mod.F90
!           gsi_chemguess_mod.F90
!
! !REMARKS: 
!
!  1. This module should never depend on more than the following GSI modules:
!      kinds
!      constants
!      m_rerank
!
!  2. Currently the Bundle uses a very simple (im,jm,km) grid. The grid could 
!     be generalized. In doing so, it should not be a part of the Bundle, but
!     rather an outside entity that can them be used by the Bundle; instead
!     of passing im,jm,km the routines would pass the Grid type.
!
!  3. Bundle does not accept redundancy in variable names.
!
!  4. Routines and interfaces are only written if they are needed and can
!     be tested in GSI. There is no need to create code that is not being 
!     used.
!
!  5. Not all prologues will show in "protex -s" since I have purposefully
!     placed BOC and EOC strategically to eliminate interfaces that users
!     have no need to be concerned with.
!
!  6. This module uses the following conventions:
!
!       6.a) all public procedures are named with the prefix GSI_Bundle
!
!       6.b) all public procedures must be declared via an interface
!            declaration.
! 
!       6.c) name of internal procedures end with an underscore; the 
!            corresponding public names are created via an interface with a 
!            similar name without the underscore, e.g., internal procedure 
!            print_ is made public with the name GSI_BundlePrint
!
!  7. For the time being the GSI stop2 routine is being used to kill 
!     certain error conditions. Ultimately, this module should never
!     call stop2 or be killed. All procedures should return an error 
!     code. It is up to the calling program to check on the error code
!     and abort in case of error.
!
!  8. An error messaging system has not yet been developed. Ideally,
!     this module should have its own error codes and not depend on the
!     GSI error codes.
!
!  9. In principle the bundle should be able to handle mix-kind variables,
!     however, because of the need to link the fields in the bundle to
!     a long array-like entity (values), it turns out that all fields
!     in a given bundle must be created with the same kind, therefore 
!     the existence of AllKinds.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

   character(len=*), parameter :: myname='GSI_BundleMod'
   logical, parameter :: VERBOSE_=.false.
   integer, parameter :: bundle_kind_def = r_kind ! default kind

CONTAINS

!noEOC
!............................................................................................
!_BOP
!  
! !IROUTINE:  Init1d_ --- Initialze rank-1 meta-data
!
! !INTERFACE:

 subroutine init1d_(flds,nd,names,istatus,longnames,units,thisKind)

! !INPUT PARAMETERS:

 integer(i_kind), intent(in):: nd
 character(len=*),intent(in):: names(:)
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)
 integer(i_kind), OPTIONAL,intent(in):: thisKind

! !INPUT/OUTPUT PARAMETERS:

 type(GSI_1D),    intent(inout):: flds(nd)

! !OUTPUT PARAMETERS:

 integer(i_kind), intent(out):: istatus

! !DESCRIPTION: Initialize rank-1 meta-data
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!_EOP
!-------------------------------------------------------------------------
!noBOC
 
 integer(i_kind) i
 
 do i=1,nd
    flds(i)%myKind = thisKind
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init1d_

 subroutine clean1d_(flds,nd,istatus)
 integer(i_kind),intent(in)   :: nd
 type(GSI_1D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out)  :: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%myKind    = -1
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean1d_
!............................................................................................
 subroutine init2d_(flds,nd,names,istatus,longnames,units,thisKind)
 integer(i_kind), intent(in) :: nd
 type(GSI_2D),    intent(inout):: flds(nd)
 character(len=*),intent(in):: names(:)
 integer(i_kind), intent(out):: istatus
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)
 integer(i_kind), OPTIONAL,intent(in):: thisKind
 
 integer(i_kind) i
   
 do i=1,nd
    flds(i)%myKind = thisKind
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init2d_

 subroutine clean2d_(flds,nd,istatus)
 integer(i_kind),intent(in) :: nd
 type(GSI_2D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out):: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%myKind    = -1
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean2d_
!............................................................................................
 subroutine init3d_(flds,nd,names,istatus,longnames,units,thisKind)
 integer(i_kind), intent(in) :: nd
 type(GSI_3D),    intent(inout):: flds(nd)
 character(len=*),intent(in):: names(:)
 integer(i_kind), intent(out):: istatus
 character(len=*),OPTIONAL,intent(in):: longnames(nd)
 character(len=*),OPTIONAL,intent(in):: units(nd)
 integer(i_kind), OPTIONAL,intent(in):: thisKind
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%myKind = thisKind
    flds(i)%shortname = trim(names(i))
    if (present(longnames)) then
        flds(i)%longname  = trim(longnames(i))
    endif
    if (present(units)) then
        flds(i)%units     = trim(units(i))
    endif
 enddo
 istatus=0

 end subroutine init3d_

 subroutine clean3d_(flds,nd,istatus)
 integer(i_kind),intent(in) :: nd
 type(GSI_3D),   intent(inout):: flds(nd)
 integer(i_kind),intent(out):: istatus
 
 integer(i_kind) i
    
 do i=1,nd
    flds(i)%myKind    = -1
    flds(i)%shortname = ""
    flds(i)%longname  = ""
    flds(i)%units     = ""
 enddo
 istatus=0

 end subroutine clean3d_

!............................................................................................
 subroutine copy_item1d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_1D),   intent(in) :: fld1(:)
 type(GSI_1D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%myKind    = fld1(i1(i))%myKind
    if (fld1(i1(i))%myKind == r_single ) then
#ifdef _REAL4_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr4   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr4   = fld1(i1(i))%qr4
#endif
    else if ( fld1(i1(i))%myKind == r_double ) then
#ifdef _REAL8_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr8   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr8   = fld1(i1(i))%qr8
#endif
    else
        istatus=1
        return 
    endif
 enddo
 end subroutine copy_item1d_
 subroutine copy_item2d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_2D),   intent(in) :: fld1(:)
 type(GSI_2D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%myKind    = fld1(i1(i))%myKind
    if (fld1(i1(i))%myKind == r_single ) then
#ifdef _REAL4_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr4   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr4   = fld1(i1(i))%qr4
#endif
    else if ( fld1(i1(i))%myKind == r_double ) then
#ifdef _REAL8_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr8   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr8   = fld1(i1(i))%qr8
#endif
    else
        istatus=1
        return 
    endif
 enddo
 end subroutine copy_item2d_
 subroutine copy_item3d_ (i1,i2,fld1,fld2,istatus)
 integer(i_kind),intent(in) :: i1(:),i2(:)
 type(GSI_3D),   intent(in) :: fld1(:)
 type(GSI_3D),   intent(inout):: fld2(:)
 integer(i_kind),intent(out) :: istatus
 integer(i_kind) i
 istatus=0
 if(size(i1)>size(i2)) then
   istatus=1
   return
 endif
 do i = 1, size(i1)
    fld2(i2(i))%shortname = fld1(i1(i))%shortname
    fld2(i2(i))%longname  = fld1(i1(i))%longname
    fld2(i2(i))%units     = fld1(i1(i))%units
    fld2(i2(i))%myKind    = fld1(i1(i))%myKind
    if (fld1(i1(i))%myKind == r_single ) then
#ifdef _REAL4_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr4   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr4   = fld1(i1(i))%qr4
#endif
    else if ( fld1(i1(i))%myKind == r_double ) then
#ifdef _REAL8_
        fld2(i2(i))%q     = fld1(i1(i))%q
        fld2(i2(i))%qr8   =>fld2(i2(i))%q
#else
        fld2(i2(i))%qr8   = fld1(i1(i))%qr8
#endif
    else
        istatus=1
        return 
    endif
 enddo
 end subroutine copy_item3d_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Set0_ --- Set pointers to bundle; all vars interface
!
! !INTERFACE:
!

 subroutine set0_ ( Bundle, grid, name, istatus, &
                    names1d, names2d, names3d, levels, bundle_kind )

! !INPUT PARAMETERS:

    type(GSI_Grid),  intent(in) :: grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names
    character(len=*),OPTIONAL,intent(in) :: names2d(:) ! 2-d variable names
    character(len=*),OPTIONAL,intent(in) :: names3d(:) ! 3-d variable names
    integer(i_kind), OPTIONAL,intent(in) :: levels(:)  ! array of size(names3d)
                                                       ! indicating third dim
                                                       ! of fields (possibly
                                                       ! diff from km)
    integer,         OPTIONAL,intent(in) :: bundle_kind! overall bundle kind

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Set pointers to bundle (all-variable interface).
! 
! 
! !SEE ALSO: 
!           set1_
!
! !REMARKS: 
!  1. This does not allocate dimension for vectors (only set pointers).
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  12May2010 Todling  Add handle for edges.
!  16May2010 Todling  Pass the grid instead of im,jm,km.
!  22Oct2013 Todling  Replace edges with levels.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*set0_'

    integer(i_kind) :: im,jm,km,i,ii,nd,n1d,n2d,n3d,ndim1d,ndim2d,ndim3d,ntotal
    integer(i_kind) :: mold2(2,2), mold3(2,2,2),ndim3d1,km1

    n1d = -1
    n2d = -1
    n3d = -1
    Bundle%name = name
! ... need grid create for more general grids
!   copy external grid ...
    im=grid%im
    jm=grid%jm
    km=grid%km
!   ... to internal grid
    Bundle%grid%im=im
    Bundle%grid%jm=jm
    Bundle%grid%km=km
    ndim1d=im
    ndim2d=ndim1d*jm
    ndim3d=ndim2d*km
    if ( present(bundle_kind)) then
       Bundle%AllKinds = bundle_kind
    else
       Bundle%AllKinds = bundle_kind_def
    endif

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            call init_ (Bundle%r1(:),n1d,names1d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
!   2-d arrays ...
    if (present(names2d)) then
        nd=size(names2d)
        if (nd>0) then
            n2d=nd
            allocate(Bundle%r2(n2d), stat=istatus) 
            call init_ (Bundle%r2(:),n2d,names2d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init2), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n2d*ndim2d
        endif
    endif
!   and 3-d
    if (present(names3d)) then
        nd=size(names3d)
        if (nd>0) then
            n3d=nd
            allocate(Bundle%r3(n3d), stat=istatus) 
            call init_ (Bundle%r3(:),n3d,names3d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init3), ', istatus
               call stop2(999)
            endif
            if (present(levels)) then
               do i=1,n3d
                  Bundle%r3(i)%level = levels(i)
                  if(levels(i)/=km) then
                     ntotal=ntotal+ndim2d*levels(i)
                  else
                     ntotal=ntotal+ndim3d
                  endif
               enddo
            else
               do i=1,n3d
                  Bundle%r3(i)%level = km
               enddo
               ntotal=ntotal+n3d*ndim3d
            endif
        endif
    endif

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(n2d>0) allocate(Bundle%ival2(n2d),stat=istatus)
    if(n3d>0) allocate(Bundle%ival3(n3d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif

    ii=0
    if (n3d>0) then
        do i = 1, n3d
           km1=km; ndim3d1=ndim3d
           if(Bundle%r3(i)%level/=km) then
              km1=Bundle%r3(i)%level
              ndim3d1=ndim2d*km1
           endif
           if (Bundle%r3(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r3(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
               Bundle%r3(i)%qr4 => Bundle%r3(i)%q
#else
               Bundle%r3(i)%qr4 => rerank(Bundle%valuesr4(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
#endif
           else if (Bundle%r3(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r3(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
               Bundle%r3(i)%qr8 => Bundle%r3(i)%q
#else
               Bundle%r3(i)%qr8 => rerank(Bundle%valuesr8(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r3, ', istatus
               call stop2(999)
           endif
           Bundle%ival3(i) =  ii+1
           ii=ii+ndim3d1
        enddo
    endif
    if (n2d>0) then
        do i = 1, n2d
           if (Bundle%r2(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r2(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim2d),mold2,(/im,jm/))
               Bundle%r2(i)%qr4 => Bundle%r2(i)%q
#else
               Bundle%r2(i)%qr4 => rerank(Bundle%valuesr4(ii+1:ii+ndim2d),mold2,(/im,jm/))
#endif
           else if (Bundle%r2(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r2(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim2d),mold2,(/im,jm/))
               Bundle%r2(i)%qr8 => Bundle%r2(i)%q
#else
               Bundle%r2(i)%qr8 => rerank(Bundle%valuesr8(ii+1:ii+ndim2d),mold2,(/im,jm/))
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r2, ', istatus
               call stop2(999)
           endif
           Bundle%ival2(i) =  ii+1
           ii=ii+ndim2d
        enddo
    endif
    if (n1d>0) then
        do i = 1, n1d
           if (Bundle%r1(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr4 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr4 => Bundle%valuesr4(ii+1:ii+ndim1d)
#endif
           else if (Bundle%r1(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr8 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr8 => Bundle%valuesr8(ii+1:ii+ndim1d)
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r1, ', istatus
               call stop2(999)
           endif
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif

    Bundle%NumVars=max(0,n1d)+max(0,n2d)+max(0,n3d)
    Bundle%n1d=n1d
    Bundle%n2d=n2d
    Bundle%n3d=n3d

 end subroutine set0_
!noEOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Set1_ --- Set pointers to bundle; 1-d interface
!
! !INTERFACE:
!
 subroutine set1_ ( Bundle, im, name, istatus, &
                    names1d, bundle_kind )

! !INPUT PARAMETERS:

    integer(i_kind), intent(in) :: im    ! first  dimension of grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names
    integer,         OPTIONAL,intent(in) :: bundle_kind! overall bundle kind

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Set pointers to bundle (1d-variable interface).
!
!
! !SEE ALSO: 
!           set0_
!
! !REMARKS:
!  1. This does not allocate dimension for vectors (only set pointers).
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*set2_'

    integer(i_kind) :: i,ii,nd,n1d,ndim1d,ntotal

    n1d = -1
    Bundle%name = name
! ... need grid create for more general grids
    Bundle%grid%im=im
    ndim1d=im
    if ( present(bundle_kind)) then
       Bundle%AllKinds = bundle_kind
    else
       Bundle%AllKinds = bundle_kind_def
    endif

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            call init_ (Bundle%r1(:),n1d,names1d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
    Bundle%ndim = ntotal

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif

    ii=0
    if (n1d>0) then
        do i = 1, n1d
           if (Bundle%r1(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr4 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr4 => Bundle%valuesr4(ii+1:ii+ndim1d)
#endif
           else if (Bundle%r3(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr8 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr8 => Bundle%valuesr8(ii+1:ii+ndim1d)
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r1, ', istatus
               call stop2(999)
           endif
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif

    Bundle%NumVars=max(0,n1d)
    Bundle%n1d=n1d

 end subroutine set1_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Create1_ --- Create generel bundle from grid specification and var names
!
! !INTERFACE:
!
 subroutine create1_ ( Bundle, grid, name, istatus, &
                       names1d, names2d, names3d, levels, bundle_kind )

    implicit none

! !INPUT PARAMETERS:

    type(GSI_Grid),  intent(in) :: grid  ! GSI grid
    character(len=*),intent(in) :: name  ! define name of this bundle
    character(len=*),OPTIONAL,intent(in) :: names1d(:) ! 1-d variable names
    character(len=*),OPTIONAL,intent(in) :: names2d(:) ! 2-d variable names
    character(len=*),OPTIONAL,intent(in) :: names3d(:) ! 3-d variable names
    integer(i_kind), OPTIONAL,intent(in) :: levels(:)  ! arrays of size(names3d)
                                                       ! indicating level of 3d
                                                       ! fields (may diff from km)
    integer,         OPTIONAL,intent(in) :: bundle_kind! overall bundle kind


! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle         ! The Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: 
!               Create bundle from grid specification and user-defined
!               variable names; allocation of memory performed here.
!
!
! !SEE ALSO: 
!           create2_, create3_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  03May2010 Treadon  Add (:) to Bundle%r1, %r2, %r3 when calling init_.
!  10May2010 Todling  Add handling for edge-like fields.
!  16May2010 Todling  Pass the grid instead of im,jm,km.
!  22Oct2013 Todling  Replace edges with levels.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    character(len=*),parameter :: myname_=myname//'*create1_'

    integer(i_kind) :: i,ii,nd,n1d,n2d,n3d,ndim1d,ndim2d,ndim3d,ntotal
    integer(i_kind) :: im,jm,km,km1,ndim3d1
    integer(i_kind) :: mold2(2,2), mold3(2,2,2)

    n1d = -1
    n2d = -1
    n3d = -1
    Bundle%name = name
! ... need grid create for more general grids
    im=grid%im
    jm=grid%jm
    km=grid%km
    Bundle%grid%im=im
    Bundle%grid%jm=jm
    Bundle%grid%km=km
    ndim1d=im
    ndim2d=ndim1d*jm
    ndim3d=ndim2d*km
    if (present(bundle_kind)) then
        Bundle%AllKinds = bundle_kind
    else
        Bundle%AllKinds = bundle_kind_def
    endif

!   First count vector size for ...
!   1-d arrays ...
    ntotal=0
    if (present(names1d)) then
        nd=size(names1d)
        if (nd>0) then
            n1d=nd
            allocate(Bundle%r1(n1d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(r1), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n1d*ndim1d
        endif
    endif
!   2-d arrays ...
    if (present(names2d)) then
        nd=size(names2d)
        if (nd>0) then
            n2d=nd
            allocate(Bundle%r2(n2d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(r2), ', istatus
               call stop2(999)
            endif
            ntotal=ntotal+n2d*ndim2d
        endif
    endif
!   and 3-d
    if (present(names3d)) then
        nd=size(names3d)
        if (nd>0) then
            n3d=nd
            allocate(Bundle%r3(n3d), stat=istatus) 
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(r3), ', istatus
               call stop2(999)
            endif
            if (present(levels)) then
               do i=1,n3d
                  Bundle%r3(i)%level = levels(i)
                  if(levels(i)/=km) then
                     ntotal=ntotal+ndim2d*levels(i)
                  else
                     ntotal=ntotal+ndim3d
                  endif
               enddo
            else
               do i=1,n3d
                  Bundle%r3(i)%level = km
               enddo
               ntotal=ntotal+n3d*ndim3d
            endif
        endif
    endif

!   Now allocate long vector
    if (Bundle%AllKinds == r_single) then
#ifdef _REAL4_
        allocate(Bundle%values  (ntotal),stat=istatus)
        Bundle%values  =zero_single
        Bundle%valuesR4=>Bundle%values
#else
        allocate(Bundle%valuesR4(ntotal),stat=istatus)
        Bundle%valuesR4=zero_single
#endif
    else if (Bundle%AllKinds == r_double) then
#ifdef _REAL8_
        allocate(Bundle%values  (ntotal),stat=istatus)
        Bundle%values  =zero
        Bundle%valuesR8=>Bundle%values
#else
        allocate(Bundle%valuesR8(ntotal),stat=istatus)
        Bundle%valuesR8=zero
#endif
    else
        istatus = 999
    endif
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating ',trim(name),' values, ', istatus
       call stop2(999)
    endif

    if(n1d>0) allocate(Bundle%ival1(n1d),stat=istatus)
    if(n2d>0) allocate(Bundle%ival2(n2d),stat=istatus)
    if(n3d>0) allocate(Bundle%ival3(n3d),stat=istatus)
    if(istatus/=0) then
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif

    if (present(names3d)) then
        if (n3d>0) then
            call init_ (Bundle%r3(:),n3d,names3d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init3), ', istatus
               call stop2(999)
            endif
        endif
    endif
    if (present(names2d)) then
        if (n2d>0) then
            call init_ (Bundle%r2(:),n2d,names2d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init2), ', istatus
               call stop2(999)
            endif
        endif
    endif
    if (present(names1d)) then
        if (n1d>0) then
            call init_ (Bundle%r1(:),n1d,names1d,istatus,thisKind=Bundle%AllKinds)
            if(istatus/=0)then
               write(6,*) myname_, ':trouble allocating ',trim(name),'(init1), ', istatus
               call stop2(999)
            endif
        endif
    endif

    ii=0
    if (n3d>0) then
        do i = 1, n3d
           km1=km; ndim3d1=ndim3d
           if(Bundle%r3(i)%level/=km) then
              km1=Bundle%r3(i)%level
              ndim3d1=ndim2d*km1
           endif
           if (Bundle%r3(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r3(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
               Bundle%r3(i)%qr4 => Bundle%r3(i)%q
#else
               Bundle%r3(i)%qr4 => rerank(Bundle%valuesr4(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
#endif
           else if (Bundle%r3(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r3(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
               Bundle%r3(i)%qr8 => Bundle%r3(i)%q
#else
               Bundle%r3(i)%qr8 => rerank(Bundle%valuesr8(ii+1:ii+ndim3d1),mold3,(/im,jm,km1/))
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r3, ', istatus
               call stop2(999)
           endif
           Bundle%ival3(i) =  ii+1
           ii=ii+ndim3d1
        enddo
    endif
    if (n2d>0) then
        do i = 1, n2d
           if (Bundle%r2(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r2(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim2d),mold2,(/im,jm/))
               Bundle%r2(i)%qr4 => Bundle%r2(i)%q
#else
               Bundle%r2(i)%qr4 => rerank(Bundle%valuesr4(ii+1:ii+ndim2d),mold2,(/im,jm/))
#endif
           else if (Bundle%r2(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r2(i)%q   => rerank(Bundle%values  (ii+1:ii+ndim2d),mold2,(/im,jm/))
               Bundle%r2(i)%qr8 => Bundle%r2(i)%q
#else
               Bundle%r2(i)%qr8 => rerank(Bundle%valuesr8(ii+1:ii+ndim2d),mold2,(/im,jm/))
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r2, ', istatus
               call stop2(999)
           endif
           Bundle%ival2(i) =  ii+1
           ii=ii+ndim2d
        enddo
    endif
    if (n1d>0) then
        do i = 1, n1d
           if (Bundle%r1(i)%myKind == r_single) then
#ifdef _REAL4_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr4 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr4 => Bundle%valuesr4(ii+1:ii+ndim1d)
#endif
           else if (Bundle%r1(i)%myKind == r_double) then
#ifdef _REAL8_
               Bundle%r1(i)%q   => Bundle%values  (ii+1:ii+ndim1d)
               Bundle%r1(i)%qr8 => Bundle%r1(i)%q
#else
               Bundle%r1(i)%qr8 => Bundle%valuesr8(ii+1:ii+ndim1d)
#endif
           else
               istatus = 999
               write(6,*) myname_, ':trouble assigining ',trim(name),' r1, ', istatus
               call stop2(999)
           endif
           Bundle%ival1(i) =  ii+1
           ii=ii+ndim1d
        enddo
    endif
    if(ii==ntotal) then
       Bundle%ndim = ntotal
    else
       istatus=1
       write(6,*) myname_, ':trouble allocating ',trim(name),' ivals, ', istatus
       call stop2(999)
    endif


    Bundle%NumVars=max(0,n1d)+max(0,n2d)+max(0,n3d)
    Bundle%n1d=n1d
    Bundle%n2d=n2d
    Bundle%n3d=n3d

    if ( redundant_(Bundle) ) then
        write(6,*) myname_, ': ',trim(name),' has redundant names, aborting ...'
        call stop2(999)
    endif

  end subroutine create1_
!noEOC

!BOP
!
! !IROUTINE:  Create2_ ---  Create new bundle from an existing bundle
!
! !INTERFACE:
!
  subroutine create2_ ( NewBundle, Bundle, name, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in)  :: name
    type(GSI_Bundle),intent(in)  :: Bundle

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle)             :: NewBundle

! !OUTPUT PARAMETERS:

    integer,intent(out)::istatus

! !DESCRIPTION: Create new bundle from another existing bundle.
!
!
! !SEE ALSO: 
!           create1_, create3_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  10May2010 Todling  Update to handle edges.
!  22Oct2013 Todling  Replace edges with levels.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*),parameter::myname_=myname//'*create2_'
    integer(i_kind) :: k,n1d,n2d,n3d,this_bundle_kind
    character(len=MAXSTR),allocatable::names1d(:),names2d(:),names3d(:)
    integer(i_kind),allocatable::levels(:)

    n1d = max(0,Bundle%n1d)
    n2d = max(0,Bundle%n2d)
    n3d = max(0,Bundle%n3d)
    allocate(names1d(n1d))
    allocate(names2d(n2d))
    allocate(names3d(n3d))

    do k=1,n1d
       names1d(k)=trim(Bundle%r1(k)%shortname)
    enddo

    do k=1,n2d
       names2d(k)=trim(Bundle%r2(k)%shortname)
    enddo

    allocate(levels(n3d))
    do k=1,n3d
       names3d(k)=trim(Bundle%r3(k)%shortname)
       levels(k)=Bundle%r3(k)%level
    enddo
    this_bundle_kind = Bundle%AllKinds

    call create1_ ( NewBundle, Bundle%grid, trim(name), istatus, &
                    names1d=names1d,names2d=names2d,names3d=names3d, &
                    levels=levels, bundle_kind=this_bundle_kind )

    deallocate(levels)
    deallocate(names3d)
    deallocate(names2d)
    deallocate(names1d)

  end subroutine create2_
!noEOC
!BOP
!
! !IROUTINE:  Create3_ ---  Create new bundle from merge of two existing bundles
!
! !INTERFACE:
!
  subroutine create3_ ( MergeBundle, Bundle1, Bundle2, Name, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in) :: name  ! define name of new bundle

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle1     ! 1st existing bundle (must be inout)
    type(GSI_Bundle) :: Bundle2     ! 2nd existing bundle (must be inout)
    type(GSI_Bundle) :: MergeBundle ! newly created merged bundle

! !OUTPUT PARAMETERS:

    integer(i_kind), intent(out) :: istatus  ! return error code

! !DESCRIPTION: Create new bundle from merge of two previously existing
!               bundles.
!
!
! !SEE ALSO: 
!           create1_, create2_
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  10May2010 Todling  Update to handle edges.
!  22Oct2013 Todling  Replace edges with levels.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*),parameter::myname_=myname//'*create3_'
    integer(i_kind) ::  i,k,n1d,n2d,n3d,im,jm,km,this_bundle_kind
    character(len=MAXSTR),allocatable::names1d(:),names2d(:),names3d(:)
    integer(i_kind),allocatable::levels(:)
    type(GSI_Grid) :: grid

    istatus=0

!   Defining the grid the following way is dangerous ...
    im = max(Bundle1%grid%im,Bundle2%grid%im) 
    jm = max(Bundle1%grid%jm,Bundle2%grid%jm)
    km = max(Bundle1%grid%km,Bundle2%grid%km)
    call GSI_GridCreate(grid,im,jm,km)

    n1d = max(0,Bundle1%n1d)+max(0,Bundle2%n1d)
    n2d = max(0,Bundle1%n2d)+max(0,Bundle2%n2d)
    n3d = max(0,Bundle1%n3d)+max(0,Bundle2%n3d)
    allocate(names1d(n1d))
    allocate(names2d(n2d))
    allocate(names3d(n3d))
    allocate(levels(n3d))

    i=0
    do k=1,Bundle1%n1d
       i=i+1
       names1d(i)=trim(Bundle1%r1(k)%shortname)
    enddo
    do k=1,Bundle2%n1d
       i=i+1
       names1d(i)=trim(Bundle2%r1(k)%shortname)
    enddo


    i=0
    do k=1,Bundle1%n2d
       i=i+1
       names2d(i)=trim(Bundle1%r2(k)%shortname)
    enddo
    do k=1,Bundle2%n2d
       i=i+1
       names2d(i)=trim(Bundle2%r2(k)%shortname)
    enddo


    i=0
    do k=1,Bundle1%n3d
       i=i+1
       names3d(i)=trim(Bundle1%r3(k)%shortname)
       levels (i)=Bundle1%r3(k)%level
    enddo
    do k=1,Bundle2%n3d
       i=i+1
       names3d(i)=trim(Bundle2%r3(k)%shortname)
       levels (i)=Bundle2%r3(k)%level
    enddo
    if (Bundle1%AllKinds/=Bundle2%AllKinds) then
        print*, 'bundles have diff Kinds: ', Bundle1%AllKinds,Bundle2%AllKinds
        write(6,*) myname_, ': not possible to merge bundles, aborting ...'
        call stop2(999)
    endif
    this_bundle_kind = Bundle1%AllKinds

    call create1_ ( MergeBundle, grid, name, istatus, &
                    names1d=names1d, names2d=names2d, names3d=names3d, &
                    levels=levels, bundle_kind=this_bundle_kind )

    if ( redundant_(MergeBundle) ) then
        print*, MergeBundle%n1d
        print*, MergeBundle%n2d
        print*, MergeBundle%n3d
        print*, MergeBundle%ndim
        print*, MergeBundle%numvars
        write(6,*) myname_, ': merge bundle has redundant names, aborting ...'
        call stop2(999)
    endif

    deallocate(levels)
    deallocate(names3d)
    deallocate(names2d)
    deallocate(names1d)

  end subroutine create3_
!noEOC
!BOP
!
! !IROUTINE:  dup_ ---  duplicate a given bundle
!
! !INTERFACE:
!
  subroutine dup_ ( Bundi, Bundo, Name, istatus )

! !ARGUMENTS

    implicit none
    type(GSI_Bundle),intent(in )   :: Bundi ! an existing bundle
    type(GSI_Bundle),intent(inout) :: Bundo ! the newly created bundle

    character(len=*), optional, intent(in ) :: name     ! name of the new bundle
    integer(i_kind) , optional, intent(out) :: istatus  ! return status code

! !DESCRIPTION: Duplicate a bundle.  It is simply a call to create2_()
!   and a call to copy_(), with an optional attribute to arguments name
!   and istatus, to simplify its user interface.
!
! !SEE ALSO: 
!           create2_, copy_
!
! !REVISION HISTORY:
!
!  06Jun2010 Guo      initial code, for a common application
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*), parameter:: myname_=myname//'*dup_'
    integer(i_kind)::  ier

    if(present(istatus)) istatus=0

    if(present(name)) then
      call create2_(Bundo,Bundi,name,istatus=ier)
      if(ier/=0) call perr(myname_, &
         'create2_(name="'//trim(name)//'"), istatus =',ier)
    else
      call create2_(Bundo,Bundi,Bundi%name,istatus=ier)
      if(ier/=0) call perr(myname_, &
             'create2_(name="'//trim(Bundi%name)//'"), istatus =',ier)
    endif
    if(ier/=0) then
      if(.not.present(istatus)) call die(myname_)
      istatus=ier
      return
    endif

    call copy_(Bundo,Bundi)
  end subroutine dup_
!noEOC

!BOP
!
! !IROUTINE:  scl_dup_ ---  duplicate a given bundle
!
! !INTERFACE:
!
  subroutine scl_dup_ ( a, Bundi, Bundo, Name, istatus )

! !ARGUMENTS

    implicit none
    real(r_double),  intent(in ) :: a
    type(GSI_Bundle),intent(in ) :: Bundi ! an existing bundle
    type(GSI_Bundle),intent(out) :: Bundo ! the newly created bundle

    character(len=*), optional, intent(in ) :: name     ! name of the new bundle
    integer(i_kind) , optional, intent(out) :: istatus  ! return status code

! !DESCRIPTION: Duplicate a bundle.  It is simply a call to create2_()
!   and a call to copy_(), with an optional attribute to arguments name
!   and istatus, to simplify its user interface.
!
! !SEE ALSO: 
!           create2_, copy_
!
! !REVISION HISTORY:
!
!  06Jun2010 Guo      initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*), parameter:: myname_=myname//'*scl_dup_'
    integer(i_kind)::  ier

    if(present(istatus)) istatus=0

    if(present(name)) then
      call create2_(Bundo,Bundi,name,istatus=ier)
      if(ier/=0) call perr(myname_, &
          'create2_(name="'//trim(name)//'"), istatus =',ier)
    else
      call create2_(Bundo,Bundi,Bundi%name,istatus=ier)
      if(ier/=0) call perr(myname_, &
          'create2_(name="'//trim(Bundi%name)//'"), istatus =',ier)
    endif
    if(ier/=0) then
       if(.not.present(istatus)) call die(myname_)
       istatus=ier
       return
    endif

    call gsi_bundleAssign(Bundo,0._r_double)
    call gsi_bundleAddmul(Bundo,a,Bundi)
  end subroutine scl_dup_
!noEOC

!BOP
!
! !IROUTINE:  sclR4_dup_ ---  duplicate a given bundle
!
! !INTERFACE:
!
  subroutine sclR4_dup_ ( a, Bundi, Bundo, Name, istatus )

! !ARGUMENTS

    implicit none
    real(r_single)  ,intent(in ) :: a
    type(GSI_Bundle),intent(in ) :: Bundi ! an existing bundle
    type(GSI_Bundle),intent(out) :: Bundo ! the newly created bundle

    character(len=*), optional, intent(in ) :: name     ! name of the new bundle
    integer(i_kind) , optional, intent(out) :: istatus  ! return status code

! !DESCRIPTION: Duplicate a bundle.  It is simply a call to create2_()
!   and a call to copy_(), with an optional attribute to arguments name
!   and istatus, to simplify its user interface.
!
! !SEE ALSO: 
!           create2_, copy_
!
! !REVISION HISTORY:
!
!  06Jun2010 Guo      initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*), parameter:: myname_=myname//'*sclR4_dup_'
    integer(i_kind)::  ier

    if(present(istatus)) istatus=0

    if(present(name)) then
      call create2_(Bundo,Bundi,name,istatus=ier)
      if(ier/=0) call perr(myname_, &
          'create2_(name="'//trim(name)//'"), istatus =',ier)
    else
      call create2_(Bundo,Bundi,Bundi%name,istatus=ier)
      if(ier/=0) call perr(myname_, &
           'create2_(name="'//trim(Bundi%name)//'"), istatus =',ier)
    endif
    if(ier/=0) then
       if(.not.present(istatus)) call die(myname_)
       istatus=ier
       return
    endif

    call gsi_bundleAssign(Bundo,0._r_single)
    call gsi_bundleAddmul(Bundo,a,Bundi)
  end subroutine sclR4_dup_
!noEOC

!............................................................................................
!BOP
!
! !IROUTINE:  Get0_ ---  Get pointer for a field in bundle
!
! !INTERFACE:
!
  subroutine get0_ ( Bundle, fldname, ipnt, istatus, irank, ival )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname        ! required field name

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnt           ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus        ! status error code
    integer(i_kind),intent(out) :: irank          ! field rank (e.g., 1, or 2, or 3)
    integer(i_kind),intent(out) :: ival           ! optional pointer to long vector form

! !DESCRIPTION: Retrieve pointer for required field
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Fixed interface (no optionals, per Guo's suggestion) to
!                     avoid problem found Kokron of referencing undef variables
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i, n1d, n2d, n3d

    istatus=0
    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d
    ipnt=-1; irank=-1; ival=-1
    do i=1,n1d
       if (trim(fldname).eq.trim(Bundle%r1(i)%shortname)) then
          ipnt=i
          irank=1
          ival=Bundle%ival1(i)
          return
       endif
    enddo
    do i=1,n2d
       if (trim(fldname).eq.trim(Bundle%r2(i)%shortname)) then
          ipnt=i
          irank=2
          ival=Bundle%ival2(i)
          return
       endif
    enddo
    do i=1,n3d
       if (trim(fldname).eq.trim(Bundle%r3(i)%shortname)) then
          ipnt=i
          irank=3
          ival=Bundle%ival3(i)
          return
       endif
    enddo

    if(ipnt<0) istatus=1
  end subroutine get0_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get1_ ---  Get pointer for a field in bundle
!
! !INTERFACE:
!
  subroutine get1_ ( Bundle, fldname, ipnt, istatus, irank, ival )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname        ! required field name

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnt           ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus        ! status error code
    integer(i_kind),OPTIONAL,intent(out) :: irank ! field rank (e.g., 1, or 2, or 3)
    integer(i_kind),OPTIONAL,intent(out) :: ival  ! optional pointer to long vector form

! !DESCRIPTION: Retrieve pointer for required field
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Use call to fixed-interface get0_
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank_
    integer(i_kind) :: ival_

    istatus=0
    call get0_ ( Bundle, fldname, ipnt, istatus, irank_, ival_ )
    if(present(irank)) then
       irank=irank_
    endif
    if(present(ival)) then
       ival=ival_
    endif

  end subroutine get1_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get2_ ---  Get pointers for require fields in bundle
!
! !INTERFACE:
  subroutine get2_ ( Bundle, fldnames, ipnts, istatus, iranks, ivals )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldnames(:)    ! list with field names

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnts(:)          ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus           ! status error code
    integer(i_kind),OPTIONAL,intent(out) :: iranks(:)! fields rank (e.g., 1, or 2, or 3)
    integer(i_kind),OPTIONAL,intent(out) :: ivals(:) ! optional pointers to long vector form

! !DESCRIPTION: Retrieve pointers for required fields.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  07Jul2010 Todling  Use call to fixed-interface get0_
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: i,nflds
    integer(i_kind),allocatable,dimension(:) :: iranks_
    integer(i_kind),allocatable,dimension(:) :: ivals_

    istatus=0
    nflds = size(fldnames)
    allocate(iranks_(nflds),ivals_(nflds))
    do i=1,nflds
       call get0_ ( Bundle, fldnames(i), ipnts(i), istatus, iranks_(i), ivals_(i) )
    enddo
    if(present(iranks)) then
       iranks=iranks_
    endif
    if(present(ivals)) then
       ivals=ivals_
    endif
    deallocate(iranks_,ivals_)

  end subroutine get2_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get31r8_ ---  Get pointer to rank-1 field
!
! !INTERFACE:
  subroutine get31r8_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),target,intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_double),pointer,intent(inout) :: pntr(:)  ! actual pointer to individual field
    integer(i_kind),       intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-1 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  13May2010 Todling  Also return rank-N into rank-1
!  11Nov2010 Treadon  Subtract 1 from upper array bound of Bundle%values
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt,ival,nsz

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank, ival=ival )
    if (istatus==0) then
        select case (irank)
          case(1)
             pntr => Bundle%r1(ipnt)%qr8
          case(2)
!            pntr => rerank(Bundle%r2(ipnt)%qr8)
             nsz=size(Bundle%r2(ipnt)%qr8)
             pntr => Bundle%valuesr8(ival:ival+nsz-1)
          case(3)
!            pntr => rerank(Bundle%r3(ipnt)%qr8)
             nsz=size(Bundle%r3(ipnt)%qr8)
             pntr => Bundle%valuesr8(ival:ival+nsz-1)
          case default
             istatus=1
          end select
    else 
        istatus=1
    endif

  end subroutine get31r8_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  Get31r4_ ---  Get pointer to rank-1 field
!
! !INTERFACE:
  subroutine get31r4_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),target,intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_single),pointer,intent(inout) :: pntr(:)  ! actual pointer to individual field
    integer(i_kind),       intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-1 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  13May2010 Todling  Also return rank-N into rank-1
!  11Nov2010 Treadon  Subtract 1 from upper array bound of Bundle%values
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt,ival,nsz

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank, ival=ival )
    if (istatus==0) then
        select case (irank)
          case(1)
             pntr => Bundle%r1(ipnt)%qr4
          case(2)
             nsz=size(Bundle%r2(ipnt)%qr4)
             pntr => Bundle%valuesr4(ival:ival+nsz-1)
          case(3)
             nsz=size(Bundle%r3(ipnt)%qr4)
             pntr => Bundle%valuesr4(ival:ival+nsz-1)
          case default
             istatus=1
          end select
    else 
        istatus=1
    endif

  end subroutine get31r4_
!noEOC
!BOP
!
! !IROUTINE:  Get32r8_ ---  Get pointer to rank-2 field
!
!
! !INTERFACE:
  subroutine get32r8_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_double),pointer,intent(inout) :: pntr(:,:)  ! actual pointer to individual field
    integer(i_kind),       intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-2 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==2) then
        pntr => Bundle%r2(ipnt)%qr8
    else
        istatus=1
    endif

  end subroutine get32r8_
!noEOC
!BOP
!
! !IROUTINE:  Get32r4_ ---  Get pointer to rank-2 field
!
!
! !INTERFACE:
  subroutine get32r4_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_single),pointer,intent(inout) :: pntr(:,:)  ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-2 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==2) then
        pntr => Bundle%r2(ipnt)%qr4
    else
        istatus=1
    endif

  end subroutine get32r4_
!noEOC
!BOP
!
! !IROUTINE:  Get33r8_ ---  Get pointer to rank-3 field
!
! !INTERFACE:
  subroutine get33r8_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_double),pointer,intent(inout) :: pntr(:,:,:)  ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-3 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==3) then
        pntr => Bundle%r3(ipnt)%qr8
    else
        istatus=1
    endif

  end subroutine get33r8_
!noEOC
!BOP
!
! !IROUTINE:  Get33r4_ ---  Get pointer to rank-3 field
!
! !INTERFACE:
  subroutine get33r4_ ( Bundle, fldname, pntr, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname  ! name of field

! !OUTPUT PARAMETERS:

    real(r_single),pointer,intent(inout) :: pntr(:,:,:)  ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus  ! status error code

! !DESCRIPTION: Retrieve pointer to specific rank-3 field.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if (istatus==0.and.irank==3) then
        pntr => Bundle%r3(ipnt)%qr4
    else
        istatus=1
    endif

  end subroutine get33r4_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar0r8_ ---  Set request field to a constant value
!
! !INTERFACE:

  subroutine putvar0dr8_ ( Bundle, fldname, cnst, istatus )
    
! !INPUT PARAMETERS:

    character(len=*),     intent(in) :: fldname          ! name of field
    real(r_double),intent(in) :: cnst             ! constant value

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code

! !DESCRIPTION: Set user-specified field in bundle to a constant value. 
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%qr8 = cnst
    else if( irank==2 ) then
        Bundle%r2(ipnt)%qr8 = cnst
    else if( irank==3 ) then
        Bundle%r3(ipnt)%qr8 = cnst
    endif

  end subroutine putvar0dr8_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar0r4_ ---  Set request field to a constant value
!
! !INTERFACE:

  subroutine putvar0dr4_ ( Bundle, fldname, cnst, istatus )
    
! !INPUT PARAMETERS:

    character(len=*),intent(in) :: fldname          ! name of field
    real(r_single),    intent(in) :: cnst             ! constant value

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code

! !DESCRIPTION: Set user-specified field in bundle to a constant value. 
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%qr4 = cnst
    else if( irank==2 ) then
        Bundle%r2(ipnt)%qr4 = cnst
    else if( irank==3 ) then
        Bundle%r3(ipnt)%qr4 = cnst
    endif

  end subroutine putvar0dr4_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar1dr8_ ---  Set request field to given input field values; 1d to Nd
!
! !INTERFACE:

  subroutine putvar1dr8_ ( Bundle, fldname, fld, istatus )


! !INPUT PARAMETERS:

    character(len=*),intent(in) :: fldname          ! field name
    real(r_double),  intent(in) :: fld(:)           ! input field values

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code
     
! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               rank-1 input to rank-N output.
!
!
! !REMARKS: 
!   1. This routine also allows overwritting a 2d-/3d-array in bundle
!      with user-specified field.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    integer(i_kind) :: irank,ipnt,im,jm,km

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

    im=Bundle%grid%im
    jm=Bundle%grid%jm
    km=Bundle%grid%km

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%qr8 = fld
    else if( irank==2 ) then
        Bundle%r2(ipnt)%qr8 = reshape(fld,(/im,jm/))
    else if( irank==3 ) then
        Bundle%r3(ipnt)%qr8 = reshape(fld,(/im,jm,km/))
    endif

  end subroutine putvar1dr8_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar1dr4_ ---  Set request field to given input field values; 1d to Nd
!
! !INTERFACE:

  subroutine putvar1dr4_ ( Bundle, fldname, fld, istatus )


! !INPUT PARAMETERS:

    character(len=*),intent(in) :: fldname          ! field name
    real(r_single),  intent(in) :: fld(:)           ! input field values

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle  ! the Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus          ! status error code
     
! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               rank-1 input to rank-N output.
!
!
! !REMARKS: 
!   1. This routine also allows overwritting a 2d-/3d-array in bundle
!      with user-specified field.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    integer(i_kind) :: irank,ipnt,im,jm,km

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

    im=Bundle%grid%im
    jm=Bundle%grid%jm
    km=Bundle%grid%km

!   retrieve variable
    if( irank==1 ) then
        Bundle%r1(ipnt)%qr4 = fld
    endif
    if( irank==2 ) then
        Bundle%r2(ipnt)%qr4 = reshape(fld,(/im,jm/))
    endif
    if( irank==3 ) then
        Bundle%r3(ipnt)%qr4 = reshape(fld,(/im,jm,km/))
    endif

  end subroutine putvar1dr4_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar2dr8_ ---  Set request field to given input field values; 2d to 2d
!
! !INTERFACE:
  subroutine putvar2dr8_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:
    character(len=*),intent(in) :: fldname
    real(r_double),  intent(in) :: fld(:,:)

! !INPUT/OUTPUT PARAMETERS:
    type(GSI_Bundle),intent(inout) :: Bundle

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               2d-input to 2d output.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==2 ) then
        Bundle%r2(ipnt)%qr8 = fld
    endif

  end subroutine putvar2dr8_
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar2dr4_ ---  Set request field to given input field values; 2d to 2d
!
! !INTERFACE:
  subroutine putvar2dr4_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(in) :: fld(:,:)

! !INPUT/OUTPUT PARAMETERS:
    type(GSI_Bundle),intent(inout) :: Bundle

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               2d-input to 2d output.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==2 ) then
        Bundle%r2(ipnt)%qr4 = fld
    endif

  end subroutine putvar2dr4_
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar2dp1r8_ ---  Set request field to given input field values; 2d to 2d
!
! !INTERFACE:
  subroutine putvar2dp1r8_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:
    character(len=*),intent(in) :: fldname
    real(r_double),  intent(in) :: fld(:,:,:)

! !INPUT/OUTPUT PARAMETERS:
    type(GSI_Bundle),intent(inout) :: Bundle(:)

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               2d-input to 2d output, for each instance of Bundle.
!
!
! !REVISION HISTORY:
!
!  05Oct2014 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!     get pointer to desired variable
      call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
      if(istatus/=0) return

!     retrieve variable
      if( irank==2 ) then
          Bundle(ii)%r2(ipnt)%qr8 = fld(:,:,ii)
      endif

    enddo

  end subroutine putvar2dp1r8_
!............................................................................................
!BOP
!
! !IROUTINE:  PutVar2dp1r4_ ---  Set request field to given input field values; 2d to 2d
!
! !INTERFACE:
  subroutine putvar2dp1r4_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(in) :: fld(:,:,:)

! !INPUT/OUTPUT PARAMETERS:
    type(GSI_Bundle),intent(inout) :: Bundle(:)

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Set user-specified field in bundle the given input field. 
!               2d-input to 2d output, for each instance of Bundle.
!
!
! !REVISION HISTORY:
!
!  05Oct2014 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
       if(istatus/=0) return

!      retrieve variable
       if( irank==2 ) then
           Bundle(ii)%r2(ipnt)%qr4 = fld(:,:,ii)
       endif

    enddo

  end subroutine putvar2dp1r4_
  subroutine putvar3dr8_ ( Bundle, fldname, fld, istatus )
! This routine also allows putting a 1d-array into a 2d-/3d-array
    
    type(GSI_Bundle),intent(inout) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_double),  intent(in) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==3 ) then
        Bundle%r3(ipnt)%qr8 = fld
    endif

  end subroutine putvar3dr8_
  subroutine putvar3dr4_ ( Bundle, fldname, fld, istatus )
! This routine also allows putting a 1d-array into a 2d-/3d-array
    
    type(GSI_Bundle),intent(inout) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(in) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    if( irank==3 ) then
        Bundle%r3(ipnt)%qr4 = fld
    endif

  end subroutine putvar3dr4_
  subroutine putvar3dp1r8_ ( Bundle, fldname, fld, istatus )
! This routine also allows putting a 1d-array into a 2d-/3d-array+1
    
    type(GSI_Bundle),intent(inout) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_double),  intent(in) :: fld(:,:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
       if(istatus/=0) return

!      retrieve variable
       if( irank==3 ) then
           Bundle(ii)%r3(ipnt)%qr8 = fld(:,:,:,ii)
       endif

    enddo

  end subroutine putvar3dp1r8_
  subroutine putvar3dp1r4_ ( Bundle, fldname, fld, istatus )
! This routine also allows putting a 1d-array into a 2d-/3d-array+1
    
    type(GSI_Bundle),intent(inout) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(in) :: fld(:,:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
       if(istatus/=0) return

!      retrieve variable
       if( irank==3 ) then
           Bundle(ii)%r3(ipnt)%qr4 = fld(:,:,:,ii)
       endif

    enddo

  end subroutine putvar3dp1r4_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE:  GetVar1dr8_ ---  Retrieve request field from bundle; Nd to 1d
!
! !INTERFACE:

  subroutine getvar1dr8_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname        ! request field name

! !INPUT/OUTPUT PARAMETERS:
    real(r_double),intent(inout) :: fld(:)      ! request field values

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus        ! status error code

! !DESCRIPTION: Retrieve request field from bundle and return as 1d-array.
!               Nd-input to 1d output.
!
!
! !REMARKS: 
!   1. This routine also allows retrieving a 2d-/3d-array in bundle
!      into the 1d output array.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: n,irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    select case (irank)
      case(1)
        fld = Bundle%r1(ipnt)%qr8
      case(2)
        n=size(Bundle%r2(ipnt)%qr8)
        fld = reshape(Bundle%r2(ipnt)%qr8,(/n/))
      case(3)
        n=size(Bundle%r3(ipnt)%qr8)
        fld = reshape(Bundle%r3(ipnt)%qr8,(/n/))
      case default
        istatus=1
    end select

  end subroutine getvar1dr8_
!............................................................................................
!BOP
!
! !IROUTINE:  GetVar1dr4_ ---  Retrieve request field from bundle; Nd to 1d
!
! !INTERFACE:

  subroutine getvar1dr4_ ( Bundle, fldname, fld, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle   ! the Bundle
    character(len=*),intent(in) :: fldname        ! request field name

! !INPUT/OUTPUT PARAMETERS:
    real(r_single),  intent(inout) :: fld(:)      ! request field values

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out) :: istatus        ! status error code

! !DESCRIPTION: Retrieve request field from bundle and return as 1d-array.
!               Nd-input to 1d output.
!
!
! !REMARKS: 
!   1. This routine also allows retrieving a 2d-/3d-array in bundle
!      into the 1d output array.
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    
    integer(i_kind) :: n,irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    select case (irank)
      case(1)
        fld = Bundle%r1(ipnt)%qr4
      case(2)
        n=size(Bundle%r2(ipnt)%qr4)
        fld = reshape(Bundle%r2(ipnt)%qr4,(/n/))
      case(3)
        n=size(Bundle%r3(ipnt)%qr4)
        fld = reshape(Bundle%r3(ipnt)%qr4,(/n/))
      case default
        istatus=1
    end select

  end subroutine getvar1dr4_
  subroutine getvar2dr8_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_double),intent(inout) :: fld(:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r2(ipnt)%qr8

  end subroutine getvar2dr8_
  subroutine getvar2dr4_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(inout) :: fld(:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank=irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r2(ipnt)%qr4

  end subroutine getvar2dr4_
  subroutine getvar2dp1r8_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_double),intent(inout) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
       if(istatus/=0) return

!      retrieve variable
       fld(:,:,ii) = Bundle(ii)%r2(ipnt)%qr8

    enddo

  end subroutine getvar2dp1r8_
  subroutine getvar2dp1r4_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(inout) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
    
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)

    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank=irank )
       if(istatus/=0) return

!      retrieve variable
       fld(:,:,ii) = Bundle(ii)%r2(ipnt)%qr4

    enddo

  end subroutine getvar2dp1r4_
  subroutine getvar3dr8_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_double),intent(inout) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
   
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r3(ipnt)%qr8

  end subroutine getvar3dr8_
  subroutine getvar3dr4_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(inout) :: fld(:,:,:)
    integer(i_kind),intent(out) :: istatus
   
    integer(i_kind) :: irank,ipnt

    istatus=0

!   get pointer to desired variable
    call GSI_BundleGetPointer ( Bundle, fldname, ipnt, istatus, irank )
    if(istatus/=0) return

!   retrieve variable
    fld = Bundle%r3(ipnt)%qr4

  end subroutine getvar3dr4_
  subroutine getvar3dp1r8_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_double),intent(inout) :: fld(:,:,:,:)
    integer(i_kind),intent(out) :: istatus
   
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt = size(Bundle)

!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank )
       if(istatus/=0) return

!      retrieve variable
       fld(:,:,:,ii) = Bundle(ii)%r3(ipnt)%qr8

    enddo

  end subroutine getvar3dp1r8_
  subroutine getvar3dp1r4_ ( Bundle, fldname, fld, istatus )
    
    type(GSI_Bundle),intent(in) :: Bundle(:)
    character(len=*),intent(in) :: fldname
    real(r_single),  intent(inout) :: fld(:,:,:,:)
    integer(i_kind),intent(out) :: istatus
   
    integer(i_kind) :: irank,ipnt,ii,nt

    istatus=0
    nt=size(Bundle)
 
!   loop over bundle instances
    do ii=1,nt

!      get pointer to desired variable
       call GSI_BundleGetPointer ( Bundle(ii), fldname, ipnt, istatus, irank )
       if(istatus/=0) return

!      retrieve variable
       fld(:,:,:,ii) = Bundle(ii)%r3(ipnt)%qr4

    enddo

  end subroutine getvar3dp1r4_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Inquire_Char_ ---  Inquire about character-type meta-data
!
! !INTERFACE:

  subroutine inquire_char_ ( Bundle, what, vars, istatus )
    
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in)  :: Bundle    ! the Bundle
    character(len=*),intent(in)  :: what      ! type of inquire (shortname,longname,etc)

! !OUTPUT PARAMETERS:

    character(len=*),intent(inout) :: vars(:) ! variable names/units
    integer(i_kind), intent(out)   :: istatus
    
! !DESCRIPTION: Inquire about (character-type) meta-data-like variables
!               in bundle.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!  22Oct2013 Todling  Add refinement to inq 1d/2d/3d vars separately.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: i,ii

!   if(size(vars)<Bundle%NumVars) then
!      istatus=1
!      return
!   endif
    istatus=1
    if(trim(what)=='shortnames::1d') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%shortname
       enddo
       istatus=0
    endif
    if(trim(what)=='shortnames::2d') then
       ii=0
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%shortname
       enddo
       istatus=0
    endif
    if(trim(what)=='shortnames::3d') then
       ii=0
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%shortname
       enddo
       istatus=0
    endif
    if(trim(what)=='shortnames') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%shortname
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%shortname
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%shortname
       enddo
       istatus=0
    endif
    if(trim(what)=='longnames') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%longname
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%longname
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%longname
       enddo
       istatus=0
    endif
    if(trim(what)=='units') then
       ii=0
       do i=1,Bundle%n1d
          ii=ii+1
          vars(ii)=Bundle%r1(i)%units
       enddo
       do i=1,Bundle%n2d
          ii=ii+1
          vars(ii)=Bundle%r2(i)%units
       enddo
       do i=1,Bundle%n3d
          ii=ii+1
          vars(ii)=Bundle%r3(i)%units
       enddo
       istatus=0
    endif

  end subroutine inquire_char_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Merge_ ---  Merge two existing Bundles into new Bundle
!
! !INTERFACE:

  subroutine merge_ ( MergeBundle, Bundle1, Bundle2, NewName, istatus )

! !INPUT PARAMETERS:

    character(len=*),intent(in) :: NewName

! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle1
    type(GSI_Bundle),intent(inout) :: Bundle2
    type(GSI_Bundle) :: MergeBundle

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Merge two existing Bundles into new Bundle.
!
!
! !REVISION HISTORY:
!
!  05May2010 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: ie, is, i, n1d, n2d, n3d
    integer(i_kind),allocatable:: idi(:),ido(:)

!   Check for redundancy in bundles
!   to be done ....

    call create3_ ( MergeBundle, Bundle1, Bundle2, Newname, istatus )
              if(istatus/=0)return

    n1d=MergeBundle%n1d
    n2d=MergeBundle%n2d
    n3d=MergeBundle%n3d
    istatus=0

!   Handle 1d-part of bundles
    if(n1d>0) then
       is=1
       ie=max(0,Bundle1%n1d)
       if (ie>0) then
          allocate(idi(Bundle1%n1d),ido(Bundle1%n1d))
          idi=(/(i,i=1,Bundle1%n1d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r1(:),MergeBundle%r1(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n1d
       if (ie>=is) then
          allocate(idi(Bundle2%n1d),ido(Bundle2%n1d))
          idi=(/(i,i=1,Bundle2%n1d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r1(:),MergeBundle%r1(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif

!   Handle 2d-part of bundles
    if(n2d>0) then
       is=1
       ie=max(0,Bundle1%n2d)
       if (ie>0) then
          allocate(idi(Bundle1%n2d),ido(Bundle1%n2d))
          idi=(/(i,i=1,Bundle1%n2d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r2(:),MergeBundle%r2(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n2d
       if (ie>=is) then
          allocate(idi(Bundle2%n2d),ido(Bundle2%n2d))
          idi=(/(i,i=1,Bundle2%n2d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r2(:),MergeBundle%r2(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif

!   Handle 3d-part of bundles
    if(n3d>0) then
       is=1
       ie=max(0,Bundle1%n3d)
       if (ie>0) then
          allocate(idi(Bundle1%n3d),ido(Bundle1%n3d))
          idi=(/(i,i=1,Bundle1%n3d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle1%r3(:),MergeBundle%r3(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
       is=ie+1
       ie=n3d
       if (ie>=is) then
          allocate(idi(Bundle2%n3d),ido(Bundle2%n3d))
          idi=(/(i,i=1,Bundle2%n3d)/)
          ido=(/(i,i=is,ie)/)
          call copy_item_ (idi,ido,Bundle2%r3(:),MergeBundle%r3(:),istatus)
              if(istatus/=0)return
          deallocate(idi,ido)
       endif
    endif
    if(verbose_) print*, 'complete merge'

  end subroutine merge_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Copy_ --- Copy one Bundle into another 
!
! !INTERFACE:

  subroutine copy_(Bundo,Bundi)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  type(GSI_Bundle), intent(in   ) :: bundi

! !INPUT/OUTPUT PARAMETERS:

  type(GSI_Bundle), intent(inout) :: bundo

! !DESCRIPTION: Copy contents of one bundle into another.
!
!
! !REVISION HISTORY:
!
!       2007 Tremolet Initial code.
!  27Apr2010 Todling  Adapted from control-vector.
!  14May2010 Treadon  Bug fix in copying names to new bundle
!
!EOP
!-------------------------------------------------------------------------
!noBOC

  character(len=*),parameter::myname_='copy_'
  integer(i_kind) :: ii
  logical :: samedim

  samedim = bundo%ndim==bundi%ndim.and.&
            bundo%n1d ==bundi%n1d .and.&
            bundo%n2d ==bundi%n2d .and.&
            bundo%n3d ==bundi%n3d
  if (.not.samedim) then
     write(6,*)trim(myname_),': error length',bundi%ndim,bundo%ndim,&
                                              bundi%n1d,bundo%n1d,&
                                              bundi%n2d,bundo%n2d,&
                                              bundi%n3d,bundo%n3d
     call stop2(999)
  end if
  if (bundi%AllKinds<0 .or. bundo%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',bundi%AllKinds,bundo%AllKinds
     call stop2(999)
  endif

  if(bundi%n1d>0) then
     do ii=1,bundi%n1d
        bundo%r1(ii)%shortname=bundi%r1(ii)%shortname
        bundo%r1(ii)%longname =bundi%r1(ii)%longname
        bundo%r1(ii)%units    =bundi%r1(ii)%units
        bundo%r1(ii)%mykind   =bundi%r1(ii)%mykind
     enddo
  endif
  if(bundi%n2d>0) then
     do ii=1,bundi%n2d
        bundo%r2(ii)%shortname=bundi%r2(ii)%shortname
        bundo%r2(ii)%longname =bundi%r2(ii)%longname
        bundo%r2(ii)%units    =bundi%r2(ii)%units
        bundo%r2(ii)%mykind   =bundi%r2(ii)%mykind
     enddo
  endif
  if(bundi%n3d>0) then
     do ii=1,bundi%n3d
        bundo%r3(ii)%shortname=bundi%r3(ii)%shortname
        bundo%r3(ii)%longname =bundi%r3(ii)%longname
        bundo%r3(ii)%units    =bundi%r3(ii)%units
        bundo%r3(ii)%level    =bundi%r3(ii)%level
        bundo%r3(ii)%mykind   =bundi%r3(ii)%mykind
     enddo
  endif

  if (bundo%AllKinds==r_single) then
!$omp parallel do
     do ii=1,bundo%ndim
#ifdef _REAL4_
        bundo%values  (ii)=bundi%values  (ii)
#else
        bundo%valuesr4(ii)=bundi%valuesr4(ii)
#endif
     enddo
!$omp end parallel do
  else if (bundo%AllKinds==r_double) then
!$omp parallel do
     do ii=1,bundo%ndim
#ifdef _REAL8_
        bundo%values  (ii)=bundi%values  (ii)
#else
        bundo%valuesr8(ii)=bundi%valuesr8(ii)
#endif
     enddo
!$omp end parallel do
  endif

  do ii=1,bundo%n1d
     bundo%ival1(ii)=bundi%ival1(ii)
  enddo
  do ii=1,bundo%n2d
     bundo%ival2(ii)=bundi%ival2(ii)
  enddo
  do ii=1,bundo%n3d
     bundo%ival3(ii)=bundi%ival3(ii)
  enddo

  return
  end subroutine copy_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: AssignR8_Const_ --- Assign values of bundle to a give constant
!
! !INTERFACE:

  subroutine assignR8_const_(Bundo,cnst)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  real(r_double),intent(in) :: cnst

! !INPUT/OUTPUT PARAMETERS:

  type(GSI_Bundle), intent(inout) :: bundo

! !DESCRIPTION: Assign values of bundle to a constant
!
!
! !REVISION HISTORY:
!
!       2007 Tremolet Initial code.
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

  character(len=*),parameter::myname_='assignR8_const_'
  integer(i_kind) :: ii

  if (bundo%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',bundo%AllKinds
     call stop2(999)
  endif

!$omp parallel do
  do ii=1,bundo%ndim
     bundo%valuesr8(ii)=cnst
  enddo
!$omp end parallel do

  return
  end subroutine assignR8_const_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: AssignR4_Const_ --- Assign values of bundle to a give constant
!
! !INTERFACE:

  subroutine assignR4_const_(Bundo,cnst)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  real(r_single),     intent(in   ) :: cnst

! !INPUT/OUTPUT PARAMETERS:

  type(GSI_Bundle), intent(inout) :: bundo

! !DESCRIPTION: Assign values of bundle to a constant
!
!
! !REVISION HISTORY:
!
!       2007 Tremolet Initial code.
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

  character(len=*),parameter::myname_='assignR4_const_'
  integer(i_kind) :: ii

  if (bundo%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',bundo%AllKinds
     call stop2(999)
  endif

!$omp parallel do
  do ii=1,bundo%ndim
     bundo%valuesr4(ii)=cnst
  enddo
!$omp end parallel do

  return
  end subroutine assignR4_const_
!noEOC
subroutine hadamard_upd_(zst,yst,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hadamard_upd
!   prgmmr: todling
!
! abstract: calculate element-by-element product of two state vector
!           and update input vector accordingly.
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    xst
!
!   output argument list:
!    zst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: zst
  type(gsi_bundle), intent(in   ) :: yst
  type(gsi_bundle), intent(in   ) :: xst
  character(len=*),parameter::myname_='hadamard_upd_st_'
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim.or.yst%ndim/=zst%ndim) then
     write(6,*)trim(myname_), ': error length'
     call stop2(313)
  endif

  if (zst%AllKinds<0.or.xst%AllKinds<0.or.yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',zst%AllKinds,xst%AllKinds,yst%AllKinds
     call stop2(999)
  endif

  if(zst%AllKinds==r_single .and. &
     xst%AllKinds==r_single .and. &
     yst%AllKinds==r_single ) then
     DO ii=1,zst%ndim
        zst%valuesR4(ii)=zst%valuesR4(ii) + xst%valuesR4(ii)*yst%valuesR4(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_double .and. &
     xst%AllKinds==r_double .and. &
     yst%AllKinds==r_double ) then
     DO ii=1,zst%ndim
        zst%valuesR8(ii)=zst%valuesR8(ii) + xst%valuesR8(ii)*yst%valuesR8(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_single .and. &
     xst%AllKinds==r_double .and. &
     yst%AllKinds==r_double ) then
     DO ii=1,zst%ndim
        zst%valuesR4(ii)=zst%valuesR4(ii) + xst%valuesR8(ii)*yst%valuesR8(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_double .and. &
     xst%AllKinds==r_single .and. &
     yst%AllKinds==r_double ) then
     DO ii=1,zst%ndim
        zst%valuesR8(ii)=zst%valuesR8(ii) + xst%valuesR4(ii)*yst%valuesR8(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_double .and. &
     xst%AllKinds==r_double .and. &
     yst%AllKinds==r_single ) then
     DO ii=1,zst%ndim
        zst%valuesR8(ii)=zst%valuesR8(ii) + xst%valuesR8(ii)*yst%valuesR4(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_single .and. &
     xst%AllKinds==r_single .and. &
     yst%AllKinds==r_double ) then
     DO ii=1,zst%ndim
        zst%valuesR4(ii)=zst%valuesR4(ii) + xst%valuesR4(ii)*yst%valuesR8(ii)
     ENDDO
  endif
  if(zst%AllKinds==r_single .and. &
     xst%AllKinds==r_double .and. &
     yst%AllKinds==r_single ) then
     DO ii=1,zst%ndim
        zst%valuesR4(ii)=zst%valuesR4(ii) + xst%valuesR8(ii)*yst%valuesR4(ii)
     ENDDO
  endif

  return
end subroutine hadamard_upd_
! ----------------------------------------------------------------------
subroutine self_add_st(yst,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    self_add_st
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  type(gsi_bundle), intent(in   ) :: xst
  character(len=*),parameter::myname_='self_add_st'
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim) then
     write(6,*)trim(myname_),': error length'
     call stop2(313)
  endif
  if (xst%AllKinds<0.or.yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',xst%AllKinds,yst%AllKinds
     call stop2(999)
  endif

  if(xst%AllKinds==r_double)then
     if(yst%AllKinds==r_double)then
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+xst%valuesR8(ii)
        ENDDO
     else if(yst%AllKinds==r_single)then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+xst%valuesR8(ii)
        ENDDO
     endif
  else if(xst%AllKinds==r_single)then
     if(yst%AllKinds==r_double )then
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+xst%valuesR4(ii)
        ENDDO
     else if(yst%AllKinds==r_single)then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+xst%valuesR4(ii)
        ENDDO
     endif
  end if

  return
end subroutine self_add_st
! ----------------------------------------------------------------------
subroutine self_add_R8scal(yst,pa,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    self_add_scal
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    pa
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_double),   intent(in   ) :: pa
  type(gsi_bundle), intent(in   ) :: xst
  character(len=*),parameter::myname_='self_add_R8scal_'
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim) then
     write(6,*)trim(myname_),': error length'
     call stop2(313)
  endif
  if (xst%AllKinds<0.or.yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',xst%AllKinds,yst%AllKinds
     call stop2(999)
  endif

  if(xst%AllKinds==r_double ) then 
     if(yst%AllKinds==r_double )then 
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+pa*xst%valuesR8(ii)
        ENDDO
     else if(yst%AllKinds==r_single) then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+pa*xst%valuesR8(ii)
        ENDDO
     endif
  else if(xst%AllKinds==r_single ) then
     if(yst%AllKinds==r_double) then
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+pa*xst%valuesR4(ii)
        ENDDO
     else if(yst%AllKinds==r_single)then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+pa*xst%valuesR4(ii)
        ENDDO
     end if
  endif

  return
end subroutine self_add_R8scal
! ----------------------------------------------------------------------
subroutine self_add_R4scal(yst,pa,xst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    self_add_scal
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-12  lueken - added subprogram doc block
!   2010-05-15  todling - update to use gsi_bundle
!
!   input argument list:
!    yst
!    pa
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_single),   intent(in   ) :: pa
  type(gsi_bundle), intent(in   ) :: xst
  character(len=*),parameter::myname_='self_add_R4scal_'
  integer(i_kind) :: ii

  if(yst%ndim/=xst%ndim) then
     write(6,*)trim(myname_),': error length'
     call stop2(313)
  endif
  if (xst%AllKinds<0.or.yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',xst%AllKinds,yst%AllKinds
     call stop2(999)
  endif

  if(xst%AllKinds==r_double ) then 
     if(yst%AllKinds==r_double )then 
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+pa*xst%valuesR8(ii)
        ENDDO
     else if(yst%AllKinds==r_single) then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+pa*xst%valuesR8(ii)
        ENDDO
     endif
  else if(xst%AllKinds==r_single ) then
     if(yst%AllKinds==r_double) then
        DO ii=1,yst%ndim
           yst%valuesR8(ii)=yst%valuesR8(ii)+pa*xst%valuesR4(ii)
        ENDDO
     else if(yst%AllKinds==r_single)then
        DO ii=1,yst%ndim
           yst%valuesR4(ii)=yst%valuesR4(ii)+pa*xst%valuesR4(ii)
        ENDDO
     end if
  endif

  return
end subroutine self_add_R4scal
! ----------------------------------------------------------------------
subroutine self_mulR8_(yst,pa)
!   2010-05-15  todling - update to use gsi_bundle
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_double),   intent(in   ) :: pa
  character(len=*),parameter::myname_='self_mulR8_'
  integer(i_kind) :: ii

  if (yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',yst%AllKinds
     call stop2(999)
  endif

  if (yst%AllKinds==r_double) then
     DO ii=1,yst%ndim
        yst%valuesR8(ii)=pa*yst%valuesR8(ii)
     ENDDO
  else if (yst%AllKinds==r_single) then
     DO ii=1,yst%ndim
        yst%valuesR4(ii)=pa*yst%valuesR4(ii)
     ENDDO
  endif

  return
end subroutine self_mulR8_
subroutine self_mulR4_(yst,pa)
!   2010-05-15  todling - update to use gsi_bundle
  implicit none
  type(gsi_bundle), intent(inout) :: yst
  real(r_single),     intent(in   ) :: pa
  character(len=*),parameter::myname_='self_mulR4_'
  integer(i_kind) :: ii

  if (yst%AllKinds<0 ) then
     write(6,*)trim(myname_),': error bundle precision ',yst%AllKinds
     call stop2(999)
  endif

  if (yst%AllKinds==r_double) then
     DO ii=1,yst%ndim
        yst%valuesR8(ii)=pa*yst%valuesR8(ii)
     ENDDO
  else if (yst%AllKinds==r_single) then
     DO ii=1,yst%ndim
        yst%valuesR4(ii)=pa*yst%valuesR4(ii)
     ENDDO
  endif

  return
end subroutine self_mulR4_

real(r_quad) function dplevs2dr8_(dx,dy,ihalo)

  implicit none
  real(r_double) ,intent(in) :: dx(:,:),dy(:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_quad) dplevs
  integer(i_kind) :: im,jm,ii,jj,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do jj=1+ihalo_,jm-ihalo_
     do ii=1+ihalo_,im-ihalo_
        dplevs=dplevs+dx(ii,jj)*dy(ii,jj)
     end do
  end do
  dplevs2dr8_=dplevs

return
end function dplevs2dr8_
real(r_double) function dplevs2dr4_(dx,dy,ihalo)

  implicit none
  real(r_single) ,intent(in) :: dx(:,:),dy(:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_double) dplevs
  integer(i_kind) :: im,jm,ii,jj,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do jj=1+ihalo_,jm-ihalo_
     do ii=1+ihalo_,im-ihalo_
        dplevs=dplevs+dx(ii,jj)*dy(ii,jj)
     end do
  end do
  dplevs2dr4_=dplevs

return
end function dplevs2dr4_
real(r_quad) function dplevs3dr8_(dx,dy,ihalo)

  implicit none
  real(r_double) ,intent(in) :: dx(:,:,:),dy(:,:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_quad) dplevs
  integer(i_kind) :: im,jm,km,ii,jj,kk,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  km=size(dx,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do kk=1,km
     do jj=1+ihalo_,jm-ihalo_
        do ii=1+ihalo_,im-ihalo_
           dplevs=dplevs+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do
  dplevs3dr8_=dplevs

return
end function dplevs3dr8_
real(r_double) function dplevs3dr4_(dx,dy,ihalo)

  implicit none
  real(r_single) ,intent(in) :: dx(:,:,:),dy(:,:,:)
  integer(i_kind),optional,intent(in) :: ihalo

  real(r_double) dplevs
  integer(i_kind) :: im,jm,km,ii,jj,kk,ihalo_

  im=size(dx,1)
  jm=size(dx,2)
  km=size(dx,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  dplevs=zero_quad
  do kk=1,km
     do jj=1+ihalo_,jm-ihalo_
        do ii=1+ihalo_,im-ihalo_
           dplevs=dplevs+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do
  dplevs3dr4_=dplevs

return
end function dplevs3dr4_

real(r_double) function sum2dR8_(field,ihalo)
  implicit none
  real(r_double),dimension(:,:),intent(in) :: field
  integer(i_kind),optional   ,intent(in) :: ihalo

! local variables
  real(r_double) :: sum_mask
  integer(i_kind) :: im,jm,i,j,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do j=1+ihalo_,jm-ihalo_
     do i=1+ihalo_,im-ihalo_
        sum_mask=sum_mask+field(i,j)
     end do
  end do
  sum2dR8_=sum_mask
  return
end function sum2dR8_
real(r_single) function sum2dR4_(field,ihalo)
  implicit none
  real(r_single),dimension(:,:),intent(in) :: field
  integer(i_kind),optional     ,intent(in) :: ihalo

! local variables
  real(r_double) :: sum_mask
  integer(i_kind) :: im,jm,i,j,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do j=1+ihalo_,jm-ihalo_
     do i=1+ihalo_,im-ihalo_
        sum_mask=sum_mask+field(i,j)
     end do
  end do
  sum2dR4_=sum_mask
  return
end function sum2dR4_

real(r_double) function sum3dR8_(field,ihalo)
  implicit none
  real(r_double),dimension(:,:,:),intent(in) :: field
  integer(i_kind),optional     ,intent(in) :: ihalo

! local variables
  real(r_double) :: sum_mask
  integer(i_kind) :: im,jm,km,i,j,k,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  km=size(field,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do k=1,km
     do j=1+ihalo_,jm-ihalo_
        do i=1+ihalo_,im-ihalo_
           sum_mask=sum_mask+field(i,j,k)
        end do
     end do
  end do
  sum3dR8_=sum_mask
  return
end function sum3dR8_
real(r_single) function sum3dR4_(field,ihalo)
  implicit none
  real(r_single),dimension(:,:,:),intent(in) :: field
  integer(i_kind),optional       ,intent(in) :: ihalo

! local variables
  real(r_double) :: sum_mask
  integer(i_kind) :: im,jm,km,i,j,k,ihalo_
 
  im=size(field,1)
  jm=size(field,2)
  km=size(field,3)
  ihalo_=0
  if(present(ihalo)) then
     ihalo_=ihalo
  endif

  sum_mask=zero
  do k=1,km
     do j=1+ihalo_,jm-ihalo_
        do i=1+ihalo_,im-ihalo_
           sum_mask=sum_mask+field(i,j,k)
        end do
     end do
  end do
  sum3dR4_=sum_mask
  return
end function sum3dR4_
!............................................................................................
!BOP
!
! !IROUTINE: Unset_ --- Unset pointers within Bundle
!
! !INTERFACE:
  subroutine unset_ ( Bundle, istatus )
    
    type(GSI_Bundle),intent(inout) :: Bundle
    integer(i_kind),intent(out) :: istatus

! !DESCRIPTION: Nullify bundle pointers.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    integer(i_kind) :: i, is, n1d, n2d, n3d

    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d

    is=0
    istatus=0
    if(n1d>0) then
       call clean_(Bundle%r1,n1d,is)
       istatus=istatus+is
       do i = 1, n1d
          nullify(Bundle%r1(i)%q)
          if (Bundle%r1(i)%myKind==r_single) then
              nullify(Bundle%r1(i)%qr4)
          else if (Bundle%r1(i)%myKind==r_double) then
              nullify(Bundle%r1(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r1,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival1,stat=is)
    endif
    istatus=istatus+is
    if(n2d>0) then
       call clean_(Bundle%r2,n2d,is)
       istatus=istatus+is
       do i = 1, n2d
          nullify(Bundle%r2(i)%q)
          if (Bundle%r2(i)%myKind==r_single) then
              nullify(Bundle%r2(i)%qr4)
          else if (Bundle%r2(i)%myKind==r_double) then
              nullify(Bundle%r2(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r2,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival2,stat=is)
    endif
    istatus=istatus+is
    if(n3d>0) then
       call clean_(Bundle%r3,n3d,is)
       istatus=istatus+is
       do i = 1, n3d
          nullify(Bundle%r3(i)%q)
          if (Bundle%r3(i)%myKind==r_single) then
              nullify(Bundle%r3(i)%qr4)
          else if (Bundle%r3(i)%myKind==r_double) then
              nullify(Bundle%r3(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r3,stat=is)
       istatus=istatus+is
       deallocate(Bundle%ival3,stat=is)
    endif
    istatus=istatus+is

!  .... need grid clean for more general grids
    Bundle%grid%im=-1
    Bundle%grid%jm=-1
    Bundle%grid%km=-1

    Bundle%n1d=-1
    Bundle%n2d=-1
    Bundle%n3d=-1
    Bundle%ndim=-1

  end subroutine unset_
!noEOC
!............................................................................................
!BOP
!
! !IROUTINE: Destroy_ --- Deallocate contents of Bundle
!
! !INTERFACE:

  subroutine destroy_ ( Bundle, istatus )
    
! !INPUT/OUTPUT PARAMETERS:

    type(GSI_Bundle),intent(inout) :: Bundle

! !OUTPUT PARAMETERS:

    integer(i_kind),optional,intent(out) :: istatus

! !DESCRIPTION: Deallocate contents of bundle.
!
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling  Adapted from control-vector.
!
!EOP
!-------------------------------------------------------------------------
!noBOC

    character(len=*), parameter:: myname_=myname//'*destroy_'
    integer(i_kind) :: i, is, n1d, n2d, n3d
    integer(i_kind) :: istatus_

    if(present(istatus)) istatus=0

    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d
    istatus_=0
    is=0

!   In opposite order of creation
    if(n3d>0) deallocate(Bundle%ival3,stat=istatus_); istatus_=abs(istatus_)
    is=istatus_+is
    if(n2d>0) deallocate(Bundle%ival2,stat=istatus_); istatus_=abs(istatus_)
    is=istatus_+is
    if(n1d>0) deallocate(Bundle%ival1,stat=istatus_); istatus_=abs(istatus_)
    is=istatus_+is
    if(is/=0) then
      if(.not.present(istatus)) call die(myname_,'failed(ival),istatus =',is)
      istatus=is
      return
    endif

    if (Bundle%AllKinds==r_single) then
        deallocate(Bundle%valuesr4,stat=istatus_); istatus_=abs(istatus_)
    else if (Bundle%AllKinds==r_double) then
        deallocate(Bundle%valuesr8,stat=istatus_); istatus_=abs(istatus_)
    endif
    if(associated(Bundle%values)) nullify(Bundle%values)
    is=istatus_+is
    if(is/=0) then
      if(.not.present(istatus)) call die(myname_,'failed(values),istatus =',is)
      istatus=is
      return
    endif

    if(n1d>0) then
       call clean_(Bundle%r1,n1d,istatus_); istatus_=abs(istatus_)
       is=istatus_+is
       do i = 1, n1d
          nullify(Bundle%r1(i)%q)
          if (Bundle%r1(i)%myKind==r_single) then
              nullify(Bundle%r1(i)%qr4)
          else if (Bundle%r1(i)%myKind==r_double) then
              nullify(Bundle%r1(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r1,stat=istatus_); istatus_=abs(istatus_)
    endif
    is=istatus_+is
    if(is/=0) then
      if(.not.present(istatus)) call die(myname_,'failed(r1),istatus =',is)
      istatus=is
      return
    endif
    if(n2d>0) then
       call clean_(Bundle%r2,n2d,istatus_); istatus_=abs(istatus_)
       is=istatus_+is
       do i = 1, n2d
          nullify(Bundle%r2(i)%q)
          if (Bundle%r2(i)%myKind==r_single) then
              nullify(Bundle%r2(i)%qr4)
          else if (Bundle%r2(i)%myKind==r_double) then
              nullify(Bundle%r2(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r2,stat=istatus_); istatus_=abs(istatus_)
    endif
    is=istatus_+is
    if(is/=0) then
      if(.not.present(istatus)) call die(myname_,'failed(r2),istatus =',is)
      istatus=is
      return
    endif
    if(n3d>0) then
       call clean_(Bundle%r3,n3d,istatus_); istatus_=abs(istatus_)
       is=istatus_+is
       do i = 1, n3d
          nullify(Bundle%r3(i)%q)
          if (Bundle%r3(i)%myKind==r_single) then
              nullify(Bundle%r3(i)%qr4)
          else if (Bundle%r3(i)%myKind==r_double) then
              nullify(Bundle%r3(i)%qr8)
          endif
       enddo
       deallocate(Bundle%r3,stat=istatus_); istatus_=abs(istatus_)
    endif
    is=istatus_+is

!  .... need grid clean for more general grids
    Bundle%grid%im=-1
    Bundle%grid%jm=-1
    Bundle%grid%km=-1

    Bundle%n1d=-1
    Bundle%n2d=-1
    Bundle%n3d=-1
    Bundle%NumVars=-1
    Bundle%ndim=-1

    if(is/=0) then
      if(.not.present(istatus)) call die(myname_,'istatus =',is)
      istatus=is
      return
    endif
  end subroutine destroy_
!............................................................................................
!BOP
!
! !IROUTINE: Print_ --- Print max/min of bundle contents
!
! !INTERFACE:

  subroutine print_ ( Bundle )

! !INPUT PARAMETERS:

    type(GSI_Bundle) :: Bundle
    
! !DESCRIPTION: Summarize contents of bundle by echoing max/min values.
!
!
! !REMARKS:
!   1. As the rest of the Bundle, this routine is MPI-free, so user
!      should be cautions when calling it each process will write its own.
!
! !REVISION HISTORY:
!
!       2010 da Silva  Initial code
!  27Apr2010 Todling   Adapt to GSI_Bundle
!
!EOP
!-------------------------------------------------------------------------
!noBOC
    integer(i_kind) :: i 
    character(len=*),parameter::myname_='print_'
    if (Bundle%AllKinds<0 ) then
       write(6,*) myname_, ':trouble with bundle precision bundle ', Bundle%AllKinds
       call stop2(999)
    endif
    print *
    print *, 'Bundle: ', trim(Bundle%name)
    do i = 1, Bundle%n1d
       if (Bundle%r1(i)%myKind==r_single) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [1d] '//Bundle%r1(i)%shortname, &
                       minval(Bundle%r1(i)%qr4), &
                       maxval(Bundle%r1(i)%qr4)
       else if (Bundle%r1(i)%myKind==r_double) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [1d] '//Bundle%r1(i)%shortname, &
                       minval(Bundle%r1(i)%qr8), &
                       maxval(Bundle%r1(i)%qr8)
       endif
    end do
    do i = 1, Bundle%n2d
       if (Bundle%r2(i)%myKind==r_single) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [2d] '//Bundle%r2(i)%shortname, &
                       minval(Bundle%r2(i)%qr4), &
                       maxval(Bundle%r2(i)%qr4)
       else if (Bundle%r2(i)%myKind==r_double) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [2d] '//Bundle%r2(i)%shortname, &
                       minval(Bundle%r2(i)%qr8), &
                       maxval(Bundle%r2(i)%qr8)
       endif
    end do
    do i = 1, Bundle%n3d
       if (Bundle%r3(i)%myKind==r_single) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [3d] '//Bundle%r3(i)%shortname, &
                       minval(Bundle%r3(i)%qr4), &
                       maxval(Bundle%r3(i)%qr4)
       else if (Bundle%r3(i)%myKind==r_double) then
           write(*,'(a20,2x,1p,e11.4,2x,e11.4)') '  [3d] '//Bundle%r3(i)%shortname, &
                       minval(Bundle%r3(i)%qr8), &
                       maxval(Bundle%r3(i)%qr8)
       endif
    end do
                             
  end subroutine print_
!noEOC

  logical function redundant_ ( Bundle )
  type(gsi_bundle),intent(in) :: Bundle
  integer(i_kind) i,j,ic,n1d,n2d,n3d,nvars,istatus
  character(len=MAXSTR),allocatable::fnames(:)

  redundant_=.false.

  n1d=Bundle%n1d
  n2d=Bundle%n2d
  n3d=Bundle%n3d
  nvars=Bundle%Numvars
  allocate(fnames(nvars))
  call inquire_char_ ( Bundle, 'shortnames', fnames, istatus )
  if (istatus/=0) then
     redundant_=.true. ! what else to do?
     deallocate(fnames)
     return
  endif
  if (nvars>0) then
      do j = 1,nvars
         ic=0
         do i = j,nvars
            if(trim(fnames(i))==trim(fnames(j))) ic=ic+1 ! this is stupid
         enddo
         if(ic/=1) then 
            redundant_=.true.
            deallocate(fnames)
            return
         endif
      enddo
  endif
  deallocate(fnames)
  end function redundant_

  subroutine GSI_GridCreate ( grid, im, jm, km )
! this does not belong to the bundle ... gridmod?
  implicit none
  integer,        intent(in)  :: im, jm, km
  type(GSI_Grid), intent(out) :: grid
  grid%im=im
  grid%jm=jm
  grid%km=km
  end subroutine GSI_GridCreate

end module GSI_BundleMod
!EOC
