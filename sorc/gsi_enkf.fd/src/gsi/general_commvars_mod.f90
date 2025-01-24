module general_commvars_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   general_commvars_mod
!   prgmmr: parrish     org: np22                date: 2012-06-25
!
! abstract: Replace subroutine init_commvars with this module to replace specialized sub2grid/grid2sub
!           type operations with the easier to use general_sub2grid.
!
! program history log:
!   2012-06-25 parrish
!   2013-10-24 todling - move vars ltosj/i to from gridmod here; same for load and fill routines
!
! Subroutines Included:
!   sub init_general_commvars - initialize type(sub2grid_info) structure variables 
!   sub destroy_general_commvars - deallocate various pointer arrays in type(sub2grid_info) structures
!
! Variable Definitions:
!   def s2g_raf - used for subdomain to horizontal grid transfers of full control vector with motley variables
!   def s2g_cv  - used in bkerror.f90 (full control vector without motley variables)
!   def s2g2    - used in getprs.f90
!   def s2g4    - used in get_derivatives2.f90 
!   def s1g4    - used in get_derivatives2.f90 
!   def s2guv   - used in getuv.f90
!   def s2g_d   - used in get_derivatives.f90
!   def g1      - used in get_derivatives.f90
!   def g3      - used in bkgcov_rewgt.f90
!   def g33p1   - used in bkgcov_rewgt.f90
!
! attributes:
!   langauge: f90
!   machgine:
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use general_sub2grid_mod, only: sub2grid_info

   implicit none

! set default to private
   private
! set subroutines to public
   public :: init_general_commvars
   public :: destroy_general_commvars
! set passed variables to public
   public :: s2g_raf                 !  structure used with all grid components of control vector,
                                     !    including motley variables.
   public :: s2g_cv                  !  structure used in bkerror.f90
   public :: s2g2                    !  structure used in getprs.f90
   public :: s2g4                    !  structure used in get_derivatives2.f90
   public :: s1g4                    !  structure used in get_derivatives2.f90 (uv version)
   public :: s2guv                   !  structure used in getuv.f90
   public :: s2g_d                   !  structure used in get_derivatives.f90
   public :: g1                      !  structure used in get_derivatives.f90
   public :: g3                      !  structure used in bkgcov_rewgt.f90
   public :: g33p1                   !  for 3 3d fields + 1 2d field, no particular order

   public :: fill_ns
   public :: fill2_ns
   public :: filluv_ns
   public :: filluv2_ns
   public :: load_grid
   public :: ltosj_s,ltosi_s,ltosj,ltosi

   integer(i_kind),allocatable,dimension(:):: ltosi   !   lats in iglobal array excluding buffer
   integer(i_kind),allocatable,dimension(:):: ltosj   !   lons in iglobal array excluding buffer
   integer(i_kind),allocatable,dimension(:):: ltosi_s !   lats in itotsub array including buffer
   integer(i_kind),allocatable,dimension(:):: ltosj_s !   lons in itotsub array including buffer

! Declare types

   type(sub2grid_info),save :: s2g_raf,s2g_cv,s2g2,s1q4,s1g4,s2g4,s2guv,s2g_d,g1,g3,g33p1

contains

!   create general_sub2grid structure variables currently made locally for get_derivatives, etc.

   subroutine init_general_commvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_commvars
!     prgmmr:    parrish     org: np22                date: 2012-06-25
!
! abstract:  create various type(sub2grid_info) struture variables for use in transformations between
!            subdomain and full horizontal grid data array decomposition.
!
! program history log:
!   2012-06-25  parrish
!   2013-10-28  todling - rename p3d to prse
!   2018-05-09  mtong - use derivative vector to structure variable s2g_d
!   2018-05-09  eliu - construct variable s2g_d for derivatives when derivative variables
!                      are set (drv_set_ = .true.)   
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

      use gridmod, only: regional,nlat,nlon,nsig,nnnn1o,nsig1o
      use gridmod, only: displs_s,ird_s,itotsub,&
                         ijn_s,irc_s,ijn,displs_g,isc_g,isd_g,vlevs
      use mpimod, only: npe,levs_id,nvar_id,nvar_pe
      use control_vectors, only: cvars2d,cvars3d,mvars,cvarsmd,nrf_var
      use derivsmod, only: dvars2d, dvars3d, drv_set_ 
      use general_sub2grid_mod, only: general_sub2grid_create_info
      use mpeu_util, only: getindex

      implicit none

!  Local Variables
      integer(i_kind) i,j,k,kk,num_fields,inner_vars,l,n,n_one,n2d,n3d
      character(len=64),allocatable,dimension(:,:) :: names_s2g_d,names_s2g_raf
      integer(i_kind),allocatable,dimension(:,:) :: lnames_s2g_raf
      logical,allocatable,dimension(:) :: vector_s2g_d
!     character(len=8) names(4*nsig+2)
      character(len=64) names2(2,2*nsig+1)
      integer(i_kind) lnames2(2,2*nsig+1)
      character(len=64) names3(1,4*nsig+1)
      integer(i_kind) lnames3(1,4*nsig+1)


!  create general_sub2grid structure variable s2g_raf, which is used in sub2grid.f90
!  NOTE:  the order of names and lnames corresponds to the order that the control + motley variables
!               are stored in the merge of control vector bundle with motley bundle.

      inner_vars=1
      n2d=size(cvars2d)
      n3d=size(cvars3d)
      num_fields=n2d+nsig*n3d+mvars
      vlevs=num_fields
      allocate(names_s2g_raf(inner_vars,num_fields),lnames_s2g_raf(inner_vars,num_fields))
      kk=0
      do k=1,n3d
         do l=1,nsig
            kk=kk+1
            names_s2g_raf(1,kk)=cvars3d(k)
            lnames_s2g_raf(1,kk)=l
         end do
      end do
      do k=1,n2d
         kk=kk+1
         names_s2g_raf(1,kk)=cvars2d(k)
         lnames_s2g_raf(1,kk)=1
      end do
      do k=1,mvars
         kk=kk+1
         names_s2g_raf(1,kk)=cvarsmd(k)
         lnames_s2g_raf(1,kk)=1
      end do

      call general_sub2grid_create_info(s2g_raf,inner_vars,nlat,nlon,nsig,num_fields,regional, &
             names=names_s2g_raf,lnames=lnames_s2g_raf)

!   set various constants previously defined in init_mpi_vars

      nsig1o=s2g_raf%nlevs_alloc
      nnnn1o=s2g_raf%nlevs_loc
      allocate(levs_id(nsig1o),nvar_id(nsig1o))
      allocate(nvar_pe(s2g_raf%num_fields,2))
      levs_id=0
      nvar_id=0
      nvar_pe=-999
      kk=0
      do k=s2g_raf%kbegin_loc,s2g_raf%kend_loc
         kk=kk+1
         levs_id(kk)=s2g_raf%lnames(1,k)
         nvar_id(kk)=getindex(nrf_var,trim(s2g_raf%names(1,k)))
      end do
      kk=0
      do n=1,npe
         do k=s2g_raf%kbegin(n-1),s2g_raf%kend(n-1)
            kk=kk+1
            nvar_pe(kk,1)=n-1
            nvar_pe(kk,2)=k-s2g_raf%kbegin(n-1)+1
         end do
      end do

!   set constants previously defined in init_commvars:

      ijn=s2g_raf%ijn
      ijn_s=s2g_raf%ijn_s
      irc_s=s2g_raf%irc_s
      isc_g=s2g_raf%isc_g
      allocate(ltosi(nlat*nlon),ltosj(nlat*nlon))
      ltosi=s2g_raf%ltosi
      ltosj=s2g_raf%ltosj
      isd_g=s2g_raf%isd_g
      displs_g=s2g_raf%displs_g
      ird_s=s2g_raf%ird_s
      displs_s=s2g_raf%displs_s
      itotsub=s2g_raf%itotsub
      allocate(ltosi_s(itotsub),ltosj_s(itotsub))
      ltosi_s=s2g_raf%ltosi_s
      ltosj_s=s2g_raf%ltosj_s

!  create general_sub2grid structure variable s2g_cv

      inner_vars=1
      num_fields=size(cvars2d)+nsig*size(cvars3d)

      call general_sub2grid_create_info(s2g_cv,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g_d, which is used in get_derivatives.f90

      if (drv_set_) then 

         inner_vars=1
         num_fields=size(dvars2d)+nsig*size(dvars3d)

!  obtain pointer to each variable in bundle, then populate corresponding names in names_s2g_d for
!        general_sub2grid_create_info.  this is needed for replacing nvar_id.
         allocate(names_s2g_d(inner_vars,num_fields),vector_s2g_d(num_fields))
!                 bundlemod stores 3d fields first, followed by 2d fields, followed by 1d fields
         i=0
         do k=1,size(dvars3d)
            do j=1,nsig
               i=i+1
               names_s2g_d(1,i)=dvars3d(k)
               vector_s2g_d(i)=names_s2g_d(1,i) == 'u'.or.names_s2g_d(1,i) == 'v'
            end do
         end do
         do k=1,size(dvars2d)
            i=i+1
            names_s2g_d(1,i)=dvars2d(k)
            vector_s2g_d(i)=names_s2g_d(1,i) == 'u'.or.names_s2g_d(1,i) == 'v'
         end do
         call general_sub2grid_create_info(s2g_d,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                        vector=vector_s2g_d,names=names_s2g_d,s_ref=s2g_raf)
         deallocate(names_s2g_d,vector_s2g_d)

      endif

!  create general_sub2grid structure variable g1, which is used in get_derivatives.f90

      inner_vars=1
      num_fields=1
      n_one=1
      call general_sub2grid_create_info(g1,inner_vars,nlat,nlon,n_one,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable g3, which is used in bkgcov_rewgt.f90

      inner_vars=1
      num_fields=nsig
      call general_sub2grid_create_info(g3,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable g33p1

      inner_vars=1
      num_fields=3*nsig+1
      call general_sub2grid_create_info(g33p1,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g4, which is used in
!  get_derivatives2.f90

      num_fields=2*nsig+1
      inner_vars=2
      kk=0
      do k=1,nsig
         kk=kk+1
         names2(1,kk)='sf'
         names2(2,kk)='vp'
         lnames2(1,kk)=k
         lnames2(2,kk)=k
      end do
      do k=1,nsig
         kk=kk+1
         names2(1,kk)='prse'
         names2(2,kk)='t'
         lnames2(1,kk)=k
         lnames2(2,kk)=k
      end do
      kk=kk+1
      names2(1,kk)='prse'
      names2(2,kk)='X'
      lnames2(1,kk)=nsig+1
      lnames2(2,kk)=0

      call general_sub2grid_create_info(s2g4,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                names=names2,lnames=lnames2,s_ref=s2g_raf)

!  create general_sub2grid structure variable s1g4, which is used in get_derivatives2.f90 (uv version)

      num_fields=4*nsig+1
      inner_vars=1
      kk=0
      do k=1,nsig
         kk=kk+1
         names3(1,kk)='u'
         lnames3(1,kk)=k
      end do
      do k=1,nsig
         kk=kk+1
         names3(1,kk)='v'
         lnames3(1,kk)=k
      end do
      do k=1,nsig
         kk=kk+1
         names3(1,kk)='t'
         lnames3(1,kk)=k
      end do
      do k=1,nsig+1
         kk=kk+1
         names3(1,kk)='prse'
         lnames3(1,kk)=k
      end do
      call general_sub2grid_create_info(s1g4,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                names=names3,lnames=lnames3,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g2, used in getprs.f90

      num_fields=nsig+1
      inner_vars=1
      call general_sub2grid_create_info(s2g2,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                              s_ref=s2g_raf)

!  create general_sub2grid structure variable s2guv

      num_fields=nsig
      inner_vars=2
      call general_sub2grid_create_info(s2guv,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

   end subroutine init_general_commvars

   subroutine destroy_general_commvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_general_commvars
!     prgmmr:    parrish     org: np22                date: 2012-06-25
!
! abstract:  deallocate all pointer arrays in struture variables created in subroutine init_general_commvars
!
! program history log:
!   2012-06-25  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
 
       use general_sub2grid_mod, only: general_sub2grid_destroy_info
       use mpimod, only: levs_id,nvar_id,nvar_pe
       implicit none
 
       deallocate(ltosi,ltosj,ltosi_s,ltosj_s)
       deallocate(levs_id,nvar_id,nvar_pe)
       call general_sub2grid_destroy_info(s2g_cv,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g2,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g4,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2guv,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g_d,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g1,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g3,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g33p1,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g_raf)
 
    end subroutine destroy_general_commvars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  load_grid --- strip off south/north latitude rows
!
! !INTERFACE:
!
 subroutine load_grid(grid_in,grid_out)

! !USES:

   use gridmod, only: iglobal,itotsub,nlon,nlat
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(max(iglobal,itotsub)),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(nlon,nlat-2)  ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine prepares grids for use in splib
!               grid to spectral tranforms.  This preparation
!               entails to two steps
!                  1) reorder indexing of the latitude direction.
!                     The GSI ordering is south to north.  The 
!                     ordering assumed in splib routines is north
!                     to south.
!                  2) The global GSI adds two latitude rows, one
!                     for each pole.  These latitude rows are not
!                     needed in the grid to spectral transforms of
!                     splib.  The code below strips off these
!                     "pole rows"
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - move from gridmod to this module
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
   integer(i_kind) i,j,k

!  Transfer input grid from 1d to 2d local array.  As loading
!  local array, reverse direction of latitude index.  Coming
!  into the routine the order is south --> north.  On exit
!  the order is north --> south
   do k=1,iglobal
      i=nlat-ltosi(k)
      if(i >= 1 .and. i <= nlat-2)then
         j=ltosj(k)
         grid_out(j,i)=grid_in(k)
      end if
   end do
   
   return
 end subroutine load_grid
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fill_ns --- add southern/northern latitude rows
!
! !INTERFACE:
!
 subroutine fill_ns(grid_in,grid_out)

! !USES:

   use constants, only: zero,one
   use gridmod, only: itotsub,nlon,nlat
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(nlon,nlat-2),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(itotsub)    ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - move from gridmod to this module
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj,nlatm2
   real(r_kind) rnlon,sumn,sums
   real(r_kind),dimension(nlon,nlat):: grid

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=grid_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=nlat-2
   do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   end do
   rnlon=one/real(nlon,r_kind)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Load means into local work array
   do i=1,nlon
      grid(i,1)   =sums
      grid(i,nlat)=sumn
   end do
   
!  Transfer local work array to output grid
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      grid_out(k)=grid(j,i)
   end do
   
   return
 end subroutine fill_ns
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fill2_ns --- add southern/northern latitude rows output 2d array
!
! !INTERFACE:
!
 subroutine fill2_ns(grid_in,grid_out,nlat,nlon)

! !USES:

   use constants, only: zero,one
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)                    ,intent(in   ) :: nlat,nlon
   real(r_kind),dimension(nlon,nlat-2),intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(nlat,nlon)  ,intent(  out) :: grid_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is consistent with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - move from gridmod to this module
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,jj,nlatm2
   real(r_kind) rnlon,sumn,sums
   real(r_kind),dimension(nlon,nlat):: grid

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=grid_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=nlat-2
   do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   end do
   rnlon=one/real(nlon,r_kind)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Load means into local work array
   do i=1,nlon
      grid(i,1)   =sums
      grid(i,nlat)=sumn
   end do
   
!  Transfer local work array to output grid
   do j=1,nlon
      do i=1,nlat
        grid_out(i,j)=grid(j,i)
      end do
   end do
   
   return
 end subroutine fill2_ns

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  filluv_ns --- add southern/northern latitude rows
!
! !INTERFACE:
!
 subroutine filluv_ns(gridu_in,gridv_in,gridu_out,gridv_out)

! !USES:

   use constants, only: zero
   use gridmod, only: itotsub,nlon,nlat
   use gridmod, only: coslon,sinlon
   implicit none

! !INPUT PARAMETERS:

   real(r_kind),dimension(nlon,nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(itotsub)    ,intent(  out) :: gridu_out,gridv_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is a one-dimensional array read in
!               an order consisten with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - move from gridmod to this module
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,k,jj
   real(r_kind) polnu,polnv,polsu,polsv
   real(r_kind),dimension(nlon,nlat):: grid,grid2

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=gridu_in(i,jj)
         grid2(i,j)=gridv_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   do i=1,nlon
      polnu=polnu+grid(i,nlat-1)*coslon(i)-grid2(i,nlat-1)*sinlon(i)
      polnv=polnv+grid(i,nlat-1)*sinlon(i)+grid2(i,nlat-1)*coslon(i)
      polsu=polsu+grid(i,2        )*coslon(i)+grid2(i,2        )*sinlon(i)
      polsv=polsv+grid(i,2        )*sinlon(i)-grid2(i,2        )*coslon(i)
   end do
   polnu=polnu/real(nlon,r_kind)
   polnv=polnv/real(nlon,r_kind)
   polsu=polsu/real(nlon,r_kind)
   polsv=polsv/real(nlon,r_kind)
   do i=1,nlon
      grid (i,nlat)= polnu*coslon(i)+polnv*sinlon(i)
      grid2(i,nlat)=-polnu*sinlon(i)+polnv*coslon(i)
      grid (i,1   )= polsu*coslon(i)+polsv*sinlon(i)
      grid2(i,1   )= polsu*sinlon(i)-polsv*coslon(i)
   end do

!  Transfer local work array to output grid
   do k=1,itotsub
      i=ltosi_s(k)
      j=ltosj_s(k)
      gridu_out(k)=grid(j,i)
      gridv_out(k)=grid2(j,i)
   end do
   
   return
 end subroutine filluv_ns
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  filluv2_ns --- add southern/northern latitude rows
!
! !INTERFACE:
!
 subroutine filluv2_ns(gridu_in,gridv_in,gridu_out,gridv_out,nlat,nlon,sinlon,coslon)

! !USES:

   use constants, only: zero
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)                    ,intent(in   ) :: nlat,nlon
   real(r_kind),dimension(nlon,nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(nlat,nlon)  ,intent(  out) :: gridu_out,gridv_out ! output grid
   real(r_kind),dimension(nlon)       ,intent(in   ) :: sinlon,coslon

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the input grid.  The southern row contains
!               the longitudinal mean of the adjacent latitude row.
!               The northern row contains the longitudinal mean of
!               the adjacent northern row.
!               
!               The added rows correpsond to the south and north poles.
!
!               In addition to adding latitude rows corresponding to the
!               south and north poles, the routine reorder the output 
!               array so that it is in
!               an order consistent with that assumed for total domain
!               gsi grids.
!
!               The assumed order for the input grid is longitude as
!               the first dimension with array index increasing from 
!               east to west.  The second dimension is latitude with
!               the index increasing from north to south.  This ordering
!               differs from that used in the GSI.  
!
!               The GSI ordering is latitude first with the index 
!               increasing from south to north.  The second dimension is
!               longitude with the index increasing from east to west.
!
!               Thus, the code below also rearranges the indexing and
!               order of the dimensions to make the output grid 
!               consistent with that which is expected in the rest of
!               gsi.
!               
!
! !REVISION HISTORY:
!   2004-08-27  treadon
!   2013-10-25  todling - move from gridmod to this module
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   integer(i_kind) i,j,jj
   real(r_kind) polnu,polnv,polsu,polsv
   real(r_kind),dimension(nlon,nlat):: grid,grid2

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         grid(i,j)=gridu_in(i,jj)
         grid2(i,j)=gridv_in(i,jj)
      end do
   end do
   
!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   do i=1,nlon
      polnu=polnu+grid(i,nlat-1)*coslon(i)-grid2(i,nlat-1)*sinlon(i)
      polnv=polnv+grid(i,nlat-1)*sinlon(i)+grid2(i,nlat-1)*coslon(i)
      polsu=polsu+grid(i,2        )*coslon(i)+grid2(i,2        )*sinlon(i)
      polsv=polsv+grid(i,2        )*sinlon(i)-grid2(i,2        )*coslon(i)
   end do
   polnu=polnu/real(nlon,r_kind)
   polnv=polnv/real(nlon,r_kind)
   polsu=polsu/real(nlon,r_kind)
   polsv=polsv/real(nlon,r_kind)
   do i=1,nlon
      grid (i,nlat)= polnu*coslon(i)+polnv*sinlon(i)
      grid2(i,nlat)=-polnu*sinlon(i)+polnv*coslon(i)
      grid (i,1   )= polsu*coslon(i)+polsv*sinlon(i)
      grid2(i,1   )= polsu*sinlon(i)-polsv*coslon(i)
   end do

!  Transfer local work array to output grid
   do j=1,nlon
      do i=1,nlat
         gridu_out(i,j)=grid(j,i)
         gridv_out(i,j)=grid2(j,i)
      end do
   end do
   
   return
 end subroutine filluv2_ns
end module general_commvars_mod
