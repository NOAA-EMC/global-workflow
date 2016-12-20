module general_sub2grid_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    generic_sub2grid_mod  generalized sub2grid and grid2sub style routines
!   prgmmr: parrish          org: np22                date: 2010-02-05
!
! abstract: This module contains generalized sub2grid and grid2sub like routines
!             which are largely independent of other gsi routines/modules.
!             This has been created first to allow easier introduction of dual
!             resolution capability for the hybrid ensemble option.  But
!             it may be used eventually to replace most of the specialized
!             sub2grid/grid2sub routines.
!             NOTE: Since this will initially be used only for ensemble space
!             where it is not necessary to have haloes on subdomains, the
!             routine general_grid2sub_ strips off the haloes from the output,
!             while still including them internally.  They are included internally
!             since this version still uses the same computation of mpi_alltoallv
!             arguments used for existing grid2sub routine.  Also, the input for
!             general_sub2grid_ has no halo.
!             The initial use for this module will be to manipulate fields of ensemble
!             perturbations as required when running GSI with the hybrid ensemble mode
!             turned on.  In this case, the haloes are not required.
!             To make this a more useful generalized code, later add option of halo size,
!             from 0 to any value.
!
! program history log:
!   2010-02-05  parrish, initial documentation
!   2010-03-02  parrish - restore halo to size 1 and duplicate what is in current sub2grid/grid2sub.
!                         also, make sure that periodic and periodic_s are properly specified.
!                         there is a bug in existing use of periodic when running in regional mode.
!                         periodic should always be false when regional=.true.  this has been
!                         corrected in this version.
!   2010-03-11  parrish - add parameter kend_alloc to type sub2grid_info.  this is to fix problem
!                           of allocating arrays when kend_loc = kbegin_loc-1, which happens for
!                           processors not involved with sub2grid/grid2sub.
!                           to fix this, use kend_alloc = max(kend_loc,kbegin_loc) for allocation.
!   2011-07-26  todling  - generalize single/double prec and rank interfaces
!   2011-08-29  todling  - add rank11 for sub2grid and grid2sub interfaces (nned for hybrid stuff)
!   2012-02-08  parrish  - add changes for regional dual resolution hybrid ensemble application.
!   2012-06-06  parrish  - add general_gather2grid and general_scatter2sub.  These are to be used with
!                           single 2-d fields when switching between subdomains spread across all processors
!                           and full 2-d field on one processor (defined as additional user argument).
!                           The usual single-double prec and rank interfaces are provided.
!   2012-06-06  parrish  - add additional variables to type sub2grid_info.
!                             nlevs_loc: number of active local full 2d fields 
!                             nlevs_alloc:  number of allocated 2d fields.
!                             lnames:    optional level index for each variable (assigned as user desires)
!                             names:     optional names for each variable (assigned as desired)
!   2012-06-25  parrish  - add subroutine general_sub2grid_destroy_info.
!   2013-08-03  todling  - protect write-out with verbose (set to false)
!   2013-10-25  todling  - nullify work pointers
!   2014-12-03  derber   - optimization changes
!
! subroutines included:
!   sub general_sub2grid_r_single  - convert from subdomains to grid for real single precision (4 byte)
!   sub general_grid2sub_r_single  - convert from grid to subdomains for real single precision (4 byte)

! Variable Definitions:
!   def sub2grid_info              - contains all information needed for general_sub2grid and general_grid2sub
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_double,i_kind,i_long,r_single,r_kind

   implicit none

! set default to private
   private
! set subroutines to public
   public :: general_sub2grid
   public :: general_grid2sub
   public :: general_gather2grid
   public :: general_scatter2sub
   public :: general_sub2grid_create_info
   public :: general_sub2grid_destroy_info
   public :: general_sube2suba
   public :: general_sube2suba_ad
   public :: general_suba2sube
! set passed variables to public
   public :: sub2grid_info

   interface general_sub2grid
     module procedure general_sub2grid_r_single_rank11
     module procedure general_sub2grid_r_single_rank14
     module procedure general_sub2grid_r_single_rank4
     module procedure general_sub2grid_r_double_rank11
     module procedure general_sub2grid_r_double_rank14
     module procedure general_sub2grid_r_double_rank4
   end interface

   interface general_grid2sub
     module procedure general_grid2sub_r_single_rank11
     module procedure general_grid2sub_r_single_rank41
     module procedure general_grid2sub_r_single_rank4
     module procedure general_grid2sub_r_double_rank11
     module procedure general_grid2sub_r_double_rank41
     module procedure general_grid2sub_r_double_rank4
   end interface

   interface general_gather2grid
     module procedure general_gather2grid_r_single_rank11
     module procedure general_gather2grid_r_single_rank13
     module procedure general_gather2grid_r_single_rank3
     module procedure general_gather2grid_r_double_rank11
     module procedure general_gather2grid_r_double_rank13
     module procedure general_gather2grid_r_double_rank3
   end interface

   interface general_scatter2sub
     module procedure general_scatter2sub_r_single_rank11
     module procedure general_scatter2sub_r_single_rank31
     module procedure general_scatter2sub_r_single_rank3
     module procedure general_scatter2sub_r_double_rank11
     module procedure general_scatter2sub_r_double_rank31
     module procedure general_scatter2sub_r_double_rank3
   end interface

   interface general_suba2sube
     module procedure general_suba2sube_r_single_rank1
     module procedure general_suba2sube_r_single_rank4
     module procedure general_suba2sube_r_double_rank1
     module procedure general_suba2sube_r_double_rank4
   end interface

   interface general_sube2suba
      module procedure general_sube2suba_r_single_rank1
      module procedure general_sube2suba_r_single_rank4
      module procedure general_sube2suba_r_double_rank1
      module procedure general_sube2suba_r_double_rank4
   end interface

   interface general_sube2suba_ad
      module procedure general_sube2suba_r_single_rank1_ad
      module procedure general_sube2suba_r_single_rank4_ad
      module procedure general_sube2suba_r_double_rank1_ad
      module procedure general_sube2suba_r_double_rank4_ad
   end interface

   type sub2grid_info

      integer(i_kind) inner_vars      ! number of inner-most loop variables
      integer(i_kind) lat1            ! no. of lats on subdomain (no buffer)
      integer(i_kind) lon1            ! no. of lons on subdomain (no buffer)
      integer(i_kind) lat2            ! no. of lats on subdomain (buffer)
      integer(i_kind) lon2            ! no. of lons on subdomain (buffer)
      integer(i_kind) latlon11        ! no. of points on subdomain (including buffer)
      integer(i_kind) latlon1n        ! latlon11*nsig
      integer(i_kind) nlat            ! no. of latitudes
      integer(i_kind) nlon            ! no. of longitudes
      integer(i_kind) nsig            ! no. of vertical levels
      integer(i_kind) num_fields      ! total number of fields/levels
      integer(i_kind) iglobal         ! number of horizontal points on global grid
      integer(i_kind) itotsub         ! number of horizontal points of all subdomains combined
      integer(i_kind) kbegin_loc      ! starting slab index for local processor
      integer(i_kind) kend_loc        ! ending slab index for local processor
      integer(i_kind) kend_alloc      ! kend_loc can = kbegin_loc - 1, for a processor not involved.
                                      !  this causes problems with array allocation:
                                      !  to correct this, use kend_alloc=max(kend_loc,kbegin_loc)
      integer(i_kind) nlevs_loc       ! number of active local levels ( = kend_loc-kbegin_loc+1)
      integer(i_kind) nlevs_alloc     ! number of allocatec local levels ( = kend_alloc-kbegin_loc+1)
      integer(i_kind) npe             ! total number of processors
      integer(i_kind) mype            ! local processor
      integer(i_kind) nskip           ! # of processors skipped between full horizontal fields in grid mode.
      logical periodic                ! logical flag for periodic e/w domains
      logical,pointer :: periodic_s(:) => NULL()    ! logical flag for periodic e/w subdomain (all tasks)
      logical,pointer :: vector(:)     => NULL()    ! logical flag, true for vector variables
      integer(i_kind),pointer :: ilat1(:)       => NULL()    !  no. of lats for each subdomain (no buffer)
      integer(i_kind),pointer :: jlon1(:)       => NULL()    !  no. of lons for each subdomain (no buffer)
      integer(i_kind),pointer :: istart(:)      => NULL()    !  start lat of the whole array on each pe
      integer(i_kind),pointer :: jstart(:)      => NULL()    !  start lon of the whole array on each pe
      integer(i_kind),pointer :: recvcounts(:)  => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::  displs_g(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: rdispls(:)     => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sendcounts(:)  => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sdispls(:)     => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ijn(:)         => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ltosj(:)       => NULL()    !  lat index for reordering slab
      integer(i_kind),pointer :: ltosi(:)       => NULL()    !  lon index for reordering slab
      integer(i_kind),pointer :: recvcounts_s(:)=> NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     irc_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     ird_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     isc_g(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::     isd_g(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer ::  displs_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: rdispls_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sendcounts_s(:)=> NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: sdispls_s(:)   => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ijn_s(:)       => NULL()    !  for mpi_alltoallv (sub2grid)
      integer(i_kind),pointer :: ltosj_s(:)     => NULL()    !  lat index for reordering slab
      integer(i_kind),pointer :: ltosi_s(:)     => NULL()    !  lon index for reordering slab
      integer(i_kind),pointer :: kbegin(:)      => NULL()    !  starting slab index for each processor
      integer(i_kind),pointer :: kend(:)        => NULL()    !  ending slab index for each processor
      integer(i_kind),pointer :: lnames(:,:)    => NULL()    !  optional level index for each variable
      character(64),pointer   :: names(:,:)     => NULL()    !  optional variable names
      logical:: lallocated = .false.
    

   end type sub2grid_info

   logical :: verbose=.false.

!  other declarations  ...

   contains

   subroutine general_sub2grid_create_info(s,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                           vector,names,lnames,nskip,s_ref)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_create_info populate info variable s
!   prgmmr: parrish          org: np22                date: 2010-02-12
!
! abstract: given dimensions of horizontal domain and various other 
!              information, obtain all required constants to allow
!              use of general_sub2grid_ and general_grid2sub_ and store them
!              in structure variable s.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - add regional flag to input.  if regional=.true., 
!                           then periodic, periodic_s=.false. always.  this corrects a bug
!                           in existing code.  (never a problem, except when npe=1).
!   2011-04-07  todling - call general_deter_subdomain
!   2012-06-06  parrish - add optional arrays names, lnames (see description below)
!   2012-08-14  parrish - add optional parameter nskip to replicate Jim Taft's hardwired optimization changes
!                          to specific mpi_all2allv calls.
!   2012-10-29  parrish - add optional type(sub2grid_info) variable s_ref, which contains sub2grid 
!                            information for same horizontal grid layout as will be in new
!                            type(sub2grid_info) variable s, which is the output of this subroutine.
!
!   input argument list:
!     s          - structure variable, waiting for all necessary information for
!                    use with general_sub2grid and general_grid2sub.
!     inner_vars - inner index, reserved for eventually putting all ensemble members 
!                    on 1st (most rapidly varying) array index.
!     nlat       - number of horizontal grid points in "latitude" direction
!     nlon       - number of horizontal grid points in "longitude"
!     nsig       - number of vertical levels for 1 3d variable.
!     num_fields - total number of 2d fields to be processed.
!     regional   - if true, then no periodicity in "longitude" direction
!     vector     - optional logical array of length num_fields, set to true for
!                    each field which will be a vector component.
!                  if not present, s%vector = .false.
!     names      - optional character array containing variable name for each of the num_fields 2d arrays.
!     lnames     - optional integer array containing level index for each of the num_fields 2d arrays.
!     nskip      - optional variable, gives number of processes to skip between horizontal 2d fields.
!                   Jim Taft has demonstrated large improvement in performance on zeus when threading
!                   is available.
!     s_ref      - optional type(sub2grid_info) variable with same horizontal dimensions/grid layout.
!                   This is used to save memory space by pointing large arrays in s to those in s_ref
!                   with the same values.
!
!   output argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_single
      use mpimod, only: mpi_comm_world
      implicit none

      type(sub2grid_info),     intent(inout) :: s
      integer(i_kind),         intent(in   ) :: inner_vars,nlat,nlon,nsig,num_fields
      logical,                 intent(in   ) :: regional 
      logical,optional,        intent(in   ) :: vector(num_fields)
      character(64),optional,  intent(in   ) :: names(inner_vars,num_fields)
      integer(i_kind),optional,intent(in   ) :: lnames(inner_vars,num_fields)
      integer(i_kind),optional,intent(in   ) :: nskip
      type(sub2grid_info),optional,intent(inout) :: s_ref

      integer(i_kind) i,ierror,j,k,num_loc_groups,nextra,mm1,n,ns,npe_used,iadd
      integer(i_kind),allocatable:: idoit(:)

      call mpi_comm_size(mpi_comm_world,s%npe,ierror)
      call mpi_comm_rank(mpi_comm_world,s%mype,ierror)
      s%inner_vars=inner_vars
      s%nlat=nlat
      s%nlon=nlon
      s%iglobal=nlat*nlon
      s%nsig=nsig
      s%num_fields=num_fields
      s%nskip=1
      if(present(nskip)) s%nskip=nskip
      if(s%lallocated) then
         if(present(s_ref)) then
            call general_sub2grid_destroy_info(s,s_ref)
         else
            call general_sub2grid_destroy_info(s)
         end if
      end if
      allocate(s%periodic_s(s%npe),s%jstart(s%npe),s%istart(s%npe),s%ilat1(s%npe),s%jlon1(s%npe))
      allocate(s%ijn(s%npe),s%ijn_s(s%npe))
      allocate(s%vector(num_fields))
      allocate(s%names(inner_vars,num_fields))
      allocate(s%lnames(inner_vars,num_fields))
      if(present(vector)) then
         s%vector=vector
      else
         s%vector=.false.
      end if
      if(present(names)) then
         s%names=names
      else
         s%names='X'
      end if
      if(present(lnames)) then
         s%lnames=lnames
      else
         s%lnames=0
      end if

!      first determine subdomains
      call general_deter_subdomain(s%npe,s%mype,s%nlat,s%nlon,regional, &
            s%periodic,s%periodic_s,s%lon1,s%lon2,s%lat1,s%lat2,s%ilat1,s%istart,s%jlon1,s%jstart)
      s%latlon11=s%lat2*s%lon2
      s%latlon1n=s%latlon11*s%nsig

      allocate(s%isc_g(s%npe),s%isd_g(s%npe),s%displs_g(s%npe),s%displs_s(s%npe))
      allocate(s%ird_s(s%npe),s%irc_s(s%npe))
 
      s%ijn=s%ilat1*s%jlon1
      s%ijn_s=(s%ilat1+2)*(s%jlon1+2)
      mm1=s%mype+1
      do i=1,s%npe
         s%irc_s(i)=s%ijn_s(mm1)
         s%isc_g(i)=s%ijn(mm1)
      end do

!        obtain ltosi,ltosj
      if(present(s_ref)) then
         s%ltosi => s_ref%ltosi
         s%ltosj => s_ref%ltosj
      else
         allocate(s%ltosi(s%nlat*s%nlon),s%ltosj(s%nlat*s%nlon))
         do i=1,s%nlat*s%nlon
            s%ltosi(i)=0
            s%ltosj(i)=0
         end do
      end if
!                       load arrays dealing with global grids
      s%isd_g(1)=0
      s%displs_g(1)=0
      do n=1,s%npe
         if(n/=1) then
            s%isd_g(n)=s%isd_g(n-1)+s%isc_g(n-1)
            s%displs_g(n)=s%displs_g(n-1)+s%ijn(n-1)
         end if
         if(.not.present(s_ref)) then
            do j=1,s%jlon1(n)
               ns=s%displs_g(n)+(j-1)*s%ilat1(n)
               do i=1,s%ilat1(n)
                  ns=ns+1
                  s%ltosi(ns)=s%istart(n)+i-1
                  s%ltosj(ns)=s%jstart(n)+j-1
               end do
            end do
         end if
      end do

! Load arrays dealing with subdomain grids
      s%ird_s(1)=0
      s%displs_s(1)=0
      do n=1,s%npe
         if(n/=1) then
            s%ird_s(n)=s%ird_s(n-1)+s%irc_s(n-1)
            s%displs_s(n)=s%displs_s(n-1)+s%ijn_s(n-1)
         end if
      end do
! set total number of points from all subdomain grids
      s%itotsub=s%displs_s(s%npe)+s%ijn_s(s%npe)

!        obtain ltosi_s,ltosj_s
      if(present(s_ref)) then
         s%ltosi_s => s_ref%ltosi_s
         s%ltosj_s => s_ref%ltosj_s
      else
         allocate(s%ltosi_s(s%itotsub),s%ltosj_s(s%itotsub))
         do i=1,s%itotsub
            s%ltosi_s(i)=0
            s%ltosj_s(i)=0
         end do
      end if

      if(.not.present(s_ref)) then
         if(regional)then

            do n=1,s%npe
               do j=1,s%jlon1(n)+2
                  ns=s%displs_s(n)+(j-1)*(s%ilat1(n)+2)
                  do i=1,s%ilat1(n)+2
                     ns=ns+1
                     s%ltosi_s(ns)=s%istart(n)+i-2
                     s%ltosj_s(ns)=s%jstart(n)+j-2
                     if(s%ltosi_s(ns)==0) s%ltosi_s(ns)=1
                     if(s%ltosi_s(ns)==nlat+1) s%ltosi_s(ns)=s%nlat
                     if(s%ltosj_s(ns)==0) s%ltosj_s(ns)=1
                     if(s%ltosj_s(ns)==nlon+1) s%ltosj_s(ns)=s%nlon
                  end do
               end do
            end do  ! end do over npe
         else
            do n=1,s%npe
               do j=1,s%jlon1(n)+2
                  ns=s%displs_s(n)+(j-1)*(s%ilat1(n)+2)
                  do i=1,s%ilat1(n)+2
                     ns=ns+1
                     s%ltosi_s(ns)=s%istart(n)+i-2
                     s%ltosj_s(ns)=s%jstart(n)+j-2
                     if(s%ltosi_s(ns)==0) s%ltosi_s(ns)=1
                     if(s%ltosi_s(ns)==nlat+1) s%ltosi_s(ns)=nlat
                     if(s%ltosj_s(ns)==0) s%ltosj_s(ns)=nlon
                     if(s%ltosj_s(ns)==nlon+1) s%ltosj_s(ns)=1
                  end do
               end do
            end do  ! end do over npe
         endif
      endif

!      next, determine vertical layout:
      allocate(idoit(0:s%npe-1))
      if(.not.present(nskip).and.s%num_fields<s%npe) then
         call get_iuse_pe(s%npe,s%num_fields,idoit)
         npe_used=s%num_fields
         if(s%mype==0.and.verbose) &
           write(6,*)' npe,num_fields,npe_used,idoit=',s%npe,s%num_fields,npe_used,idoit
      else
         idoit=0
         npe_used=0
         do n=0,s%npe-1,s%nskip
            npe_used=npe_used+1
            idoit(n)=1
         end do
      end if
      allocate(s%kbegin(0:s%npe),s%kend(0:s%npe-1))
      num_loc_groups=s%num_fields/npe_used
      nextra=s%num_fields-num_loc_groups*npe_used
      s%kbegin(0)=1
      k=0
      iadd=1
      do n=1,s%npe
         k=k+idoit(n-1)
         if(k>nextra) iadd=0
         s%kbegin(n)=s%kbegin(n-1)+idoit(n-1)*(iadd+num_loc_groups)
      end do
      do k=0,s%npe-1
         s%kend(k)=s%kbegin(k+1)-1
      end do
      if(s%mype == 0.and.verbose) then
         do k=0,s%npe-1
            write(6,*)' in general_sub2grid_create_info, k,kbegin,kend,nlevs_loc,nlevs_alloc=', &
               k,s%kbegin(k),s%kend(k),s%kend(k)-s%kbegin(k)+1,max(s%kbegin(k),s%kend(k))-s%kbegin(k)+1
         end do
      end if

      s%kbegin_loc=s%kbegin(s%mype)
      s%kend_loc=s%kend(s%mype)
      s%kend_alloc=max(s%kend_loc,s%kbegin_loc)
      s%nlevs_loc=s%kend_loc-s%kbegin_loc+1
      s%nlevs_alloc=s%kend_alloc-s%kbegin_loc+1

!         get alltoallv indices for sub2grid
      allocate(s%sendcounts(0:s%npe-1),s%sdispls(0:s%npe))
      allocate(s%recvcounts(0:s%npe-1),s%rdispls(0:s%npe))
      s%sdispls(0)=0
      do k=0,s%npe-1
         s%sendcounts(k)=s%ijn(k+1)*(s%kend_loc-s%kbegin_loc+1)
         s%sdispls(k+1)=s%sdispls(k)+s%sendcounts(k)
      end do
      s%rdispls(0)=0
      do k=0,s%npe-1
         s%recvcounts(k)=s%ijn(s%mype+1)*(s%kend(k)-s%kbegin(k)+1)
         s%rdispls(k+1)=s%rdispls(k)+s%recvcounts(k)
      end do

!         get alltoallv indices for grid2sub
      allocate(s%sendcounts_s(0:s%npe-1),s%sdispls_s(0:s%npe))
      allocate(s%recvcounts_s(0:s%npe-1),s%rdispls_s(0:s%npe))
      s%sdispls_s(0)=0
      do k=0,s%npe-1
         s%sendcounts_s(k)=s%ijn_s(k+1)*(s%kend_loc-s%kbegin_loc+1)
         s%sdispls_s(k+1)=s%sdispls_s(k)+s%sendcounts_s(k)
      end do
      s%rdispls_s(0)=0
      do k=0,s%npe-1
         s%recvcounts_s(k)=s%ijn_s(s%mype+1)*(s%kend(k)-s%kbegin(k)+1)
         s%rdispls_s(k+1)=s%rdispls_s(k)+s%recvcounts_s(k)
      end do

      s%lallocated=.true.

   end subroutine general_sub2grid_create_info

subroutine get_iuse_pe(npe,nz,iuse_pe)

  use constants, only: one,zero
  use mpimod, only: mype
  implicit none

  integer(i_kind),intent(in) ::npe,nz
  integer(i_kind),intent(out)::iuse_pe(0:npe-1)

  integer(i_kind) i,icount,nskip,ipoint
  real(r_kind) :: point,skip2


     iuse_pe=1
     if(npe <= nz) then
        write(6,*)' nz,npe=',nz,npe,' ---- no iskip found, all processors used'
     else                    
        nskip=npe-nz
        if(nskip > 0)then
          skip2=float(npe)/float(nskip)
          point=zero
          do i=1,nskip
            ipoint=min(max(0,nint(point)),npe) 
            iuse_pe(ipoint)=0
            point=point+skip2
          end do
        end if
        icount=0
        do i=0,npe-1
           if(iuse_pe(i) > 0)icount = icount+1
        end do
        if(icount /= nz) then
           write(6,*)' get_pe2 - inconsistent icount,nz ',nz,icount,'program stops',npe,skip2
           call stop2(999)
        end if
        if(mype == 0)write(6,*) ' in get_pe2 ',nz,icount,npe,skip2
   
     end if
     return
     
end subroutine get_iuse_pe

   subroutine general_sub2grid_destroy_info(s,s_ref)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_destroy_info deallocate variable s
!   prgmmr: parrish          org: np22                date: 2010-02-12
!
! abstract: deallocate all components of type(sub2grid_info)  variable s.
!
! program history log:
!   2012-06-18  parrish, initial documentation
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!
!   output argument list:
!     s          - returned with all allocatable pointers pointed to NULL and structure
!                    variable s%lallocated set to .false.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mpi_comm_world
      implicit none

      type(sub2grid_info),     intent(inout) :: s
      type(sub2grid_info),optional,intent(in) :: s_ref

      if(s%lallocated) then
         deallocate(s%periodic_s,s%vector,s%ilat1,s%jlon1,s%istart,s%jstart,s%recvcounts,s%displs_g)
         deallocate(s%rdispls,s%sendcounts,s%sdispls,s%ijn,s%recvcounts_s)
         deallocate(s%irc_s,s%ird_s,s%isc_g,s%isd_g,s%displs_s,s%rdispls_s,s%sendcounts_s,s%sdispls_s)
         deallocate(s%ijn_s,s%kbegin,s%kend,s%lnames,s%names)
         if(present(s_ref)) then
            s%ltosj   => NULL()
            s%ltosi   => NULL()
            s%ltosj_s => NULL()
            s%ltosi_s => NULL()
         else
            deallocate(s%ltosj,s%ltosi,s%ltosj_s,s%ltosi_s)
         end if
         s%lallocated=.false.
      end if

   end subroutine general_sub2grid_destroy_info

   subroutine general_deter_subdomain(npe,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_subdomain          perform domain decomposition
!   prgmmr: da silva       org: np20                date: 2006-06-28
!
! abstract: The nxPE and nyPE defines the layout, that is, nxPE is the number of
!           processors used to decompose the longitudinal dimensional, while nyPE 
!           the number of processors used to decompose the latitudinal dimension.
!           By construction, nPE = nxPE * nyPE. If a layout is not specified in
!           the namelist, it defaults to nxPE=nyPE=-1 and we revert back to
!           NCEP's original decomposition.
!
! program history log:
!   2006-06-28  da Silva - added option to perform an ESMF-like
!                          domain decomposition based on a layout.
!                          If no layout is defined in mpimod then
!                          it reverts back to NCEP's original algorithm.
!   2011-04-07 todling   - embed in this package; update argument list
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use mpimod, only: nxPE, nyPE
  use mpeu_util, only: die
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: npe,mype,nlat,nlon
  logical        ,intent(in   ) :: regional
  logical        ,intent(  out) :: periodic,periodic_s(npe)
  integer(i_kind),intent(  out) :: lon1,lon2,lat1,lat2
  integer(i_kind),intent(  out) :: ilat1(npe),istart(npe),jlon1(npe),jstart(npe)

  character(len=*), parameter :: myname_='general_deter_subdomain'
! integer(i_kind)  :: npe2,npsqrt

! npe2=npe/2
! npsqrt=sqrt(npe2)
! if(2*npsqrt*npsqrt == npe)then
!    nxpe=2*npsqrt
!    nype=npsqrt
!    if(mype == 0)write(6,*) ' using nxpe and nype in deter_subdomain ',nxpe,nype
! end if
! If a layout is provided, use it for the domain decomposition
! ------------------------------------------------------------
  if ( nxPE > 0 .AND. nyPE > 0 ) then

     if( npe/=nxpe*nype ) then
         call die(myname_,'NPE inconsistent from  NxPE NyPE ',npe)
     endif
     call general_deter_subdomain_withLayout(npe,nxPE,nyPE,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)

! Otherwise, use NCEP original algorithm
! --------------------------------------
  else

     call general_deter_subdomain_nolayout(npe,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)

  endif

  end subroutine general_deter_subdomain

!-------------------------------------------------------------------------
!BOP

  subroutine general_deter_subdomain_withLayout(npe,nxpe,nype,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)

! !USES:

  use kinds, only: i_kind

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind),intent(in   ) :: mype,nxpe,nype
  integer(i_kind),intent(in   ) :: npe,nlat,nlon
  logical        ,intent(in   ) :: regional
  logical        ,intent(  out) :: periodic,periodic_s(npe)  ! ??
  integer(i_kind),intent(  out) :: lon1,lon2,lat1,lat2
  integer(i_kind),intent(  out) :: ilat1(npe),istart(npe),jlon1(npe),jstart(npe)


! !OUTPUT PARAMETERS:

  ! all the variables in "use gridmod" are defined here

! !DESCRIPTION: determine GSI subdomains using a layout
!
! !REVISION HISTORY:
!
!   2006-06-27  cruz
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    cruz           org: gmao                date: 2006-06-27
!
! !REVISION HISTORY:
!   2011-04-07 todling  embed in this package; update argument list
!
!EOP
!-------------------------------------------------------------------------

! Declare local variables

  integer(i_kind) i,j,k,iinum,jjnum,iistart,jjstart
  integer(i_kind) lsetx,lsety,nxseg,nyseg
  integer(i_kind),allocatable,dimension(:) :: imxy, jmxy
  integer(i_kind) im,jm,mm1,ierr

! start

  periodic=.false.
  periodic_s=.false.
  im=nlon; jm=nlat
  allocate(imxy(0:nxpe-1),jmxy(0:nype-1), stat=ierr)
  if(ierr /= 0) then
     write(6,*)' DETER_SUBDOMAIN: ALLOCATE ERROR.'
     call stop2(30)
  end if
 
  call GET_LOCAL_DIMS_ ( im,imxy,nxpe )
  call GET_LOCAL_DIMS_ ( jm,jmxy,nype )

! compute subdomain boundaries  (axis indices)

  k=0
  iinum=imxy(0)
  jjnum=jmxy(0)
  nxseg=2
  nyseg=2
  istart=1
  jstart=1
  iistart=1
  jjstart=1
  lsetx=npe/nype
  lsety=npe/nype
  do j=0,nype-1
     do i=0,nxpe-1
        k=k+1
        if(i>0) then
           if(imxy(i)<imxy(i-1)) iinum = imxy(i)
        end if
        if(j>0) then
           if(jmxy(j)<jmxy(j-1)) jjnum = jmxy(j)
        end if
        ilat1(k)=jjnum
        jlon1(k)=iinum
            if (jlon1(k)==nlon.and..not.regional) then  ! _RT I have no idea if
                                                        !     this is correct
               periodic=.true.
               periodic_s(k)=.true.
            endif
        if(k>1) then
           if(nxseg<=lsetx) then
              jstart(k)=iistart+jlon1(k)
              iistart=jstart(k)
              nxseg=nxseg+1
           else
              jstart(k)=1
              iistart=1
              nxseg=2
           end if
           if(nyseg<=lsety) then
              istart(k)=jjstart
              nyseg=nyseg+1
           else
              if(ilat1(k)<ilat1(k-1)) then
                 istart(k)=jjstart+ilat1(k)+1
              else
                 istart(k)=jjstart+ilat1(k)
              end if
              jjstart=istart(k)
              nyseg=2
           end if
        end if
        if(mype == 0 .and. verbose) &
             write(6,100) k,istart(k),jstart(k),ilat1(k),jlon1(k)
     end do
  end do

100 format('general_DETER_SUBDOMAIN_withlayout:  task,istart,jstart,ilat1,jlon1=',5(i6,1x))
  
        
! Set number of latitude and longitude for given subdomain
  mm1=mype+1
  lat1=ilat1(mm1)
  lon1=jlon1(mm1)
  lat2=lat1+2
  lon2=lon1+2

  deallocate(imxy,jmxy, stat=ierr)
  if(ierr /= 0) then
     write(6,*)' DETER_SUBDOMAIN: DEALLOCATE ERROR.'
     call stop2(30)
  end if 


  return

  end subroutine general_deter_subdomain_withLayout

  subroutine GET_LOCAL_DIMS_ ( dim_world,dim,NDEs )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    GET_LOCAL_DIMS
!   prgmmr:                  org                      date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    dim_world
!    NDEs
!    dim
!
!   output argument list:
!    dim
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit   none

   integer(i_kind),intent(in   ) :: dim_world, NDEs
   integer(i_kind),intent(inout) :: dim(0:NDEs-1)

   integer(i_kind)    n,im,rm

   im = dim_world/NDEs
   rm = dim_world-NDEs*im
   do n=0,NDEs-1
      dim(n) = im
      if( n<=rm-1 ) dim(n) = im+1
   enddo
   end subroutine GET_LOCAL_DIMS_

   subroutine general_deter_subdomain_nolayout(npe,mype,nlat,nlon,regional, &
                    periodic,periodic_s,lon1,lon2,lat1,lat2,ilat1,istart,jlon1,jstart)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_deter_subdomain_nolayout   perform domain decomposition
!   prgmmr: weiyu yang       org: np20                date: 1998-05-14
!
! abstract: Given an array of the observation computation load and
!           the number of available mpi tasks (npe), this routine 
!           decomposes the total analysis grid into npe subdomains
!
! program history log:
!   1998-05-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-01  treadon - simplify algorithm
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2005-10-17  derber - rewrite routine using simpler algorithm
!   2005-10-26  treadon - correct error in 100 format text
!   2008-06-04  safford - rm unused vars
!   2008-09-05  lueken - merged ed's changes into q1fy09 code
!   2010-02-12  parrish - make copy for use in general_sub2grid_mod
!   2010-03-02  parrish - add regional flag to input.  if regional=.true., 
!                           then periodic, periodic_s=.false. always.  this corrects a bug
!                           in existing code.  (never a problem, except when npe=1).
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      implicit none

!     Declare passed variables
      integer(i_kind),intent(in   ) :: npe,mype,nlat,nlon
      logical        ,intent(in   ) :: regional
      logical        ,intent(  out) :: periodic,periodic_s(npe)
      integer(i_kind),intent(  out) :: lon1,lon2,lat1,lat2
      integer(i_kind),intent(  out) :: ilat1(npe),istart(npe),jlon1(npe),jstart(npe)

!     Declare local variables
      integer(i_kind) npts,nrnc,iinum,iileft,jrows,jleft,k,i,jjnum
      integer(i_kind) j,mm1,iicnt,ipts,jjleft
      integer(i_kind),dimension(npe+1):: iiend,jjend,iistart
      real(r_kind):: anperpe

!************************************************************************
      periodic=.false.
      periodic_s=.false.
!     Compute number of points on full grid and target number of
!     point per mpi task (pe)
      npts=nlat*nlon
      anperpe=float(npts)/float(npe)

!     Start with square subdomains
      nrnc=sqrt(anperpe)
      iinum=nlon/nrnc
      if(iinum==0) iinum=1
      iicnt=nlon/iinum
      iileft=nlon-iicnt*iinum
      jrows=npe/iinum
      jleft=npe-jrows*iinum

!     Adjust subdomain boundaries
      k=0
      istart=1
      jstart=1
      iistart(1)=1
      do i=1,iinum
         ipts = iicnt
         if(i <= iileft)ipts=ipts+1
         iiend(i)=iistart(i)+ipts-1
         iistart(i+1)=iiend(i)+1
         jjnum=jrows
         if(i <= jleft)jjnum=jrows+1
         do j=1,jjnum
            k=k+1
            jlon1(k)=ipts
            jstart(k)= iistart(i)
            ilat1(k)=nlat/jjnum
            jjleft=nlat-ilat1(k)*jjnum
            if(j <= jjleft)ilat1(k)=ilat1(k)+1
            if(j > 1)istart(k)=jjend(j-1)+1
            jjend(j)=istart(k)+ilat1(k)-1

            if (jlon1(k)==nlon.and..not.regional) then
               periodic=.true.
               periodic_s(k)=.true.
            endif
            if(mype == 0 .and. verbose) &
                 write(6,100) k-1,istart(k),jstart(k),ilat1(k),jlon1(k)
         end do
      end do
    100 format('general_DETER_SUBDOMAIN_nolayout:  task,istart,jstart,ilat1,jlon1=',6(i6,1x))


! Set number of latitude and longitude for given subdomain
      mm1=mype+1
      lat1=ilat1(mm1)
      lon1=jlon1(mm1)
      lat2=lat1+2
      lon2=lon1+2
  
      return

   end subroutine general_deter_subdomain_nolayout

   subroutine general_sub2grid_r_single_rank11(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_single_rank11  rank-1x1 interface to general_sub2grid_r
!   prgmmr: todling          org: np22                date: 2011-08-29
!
! abstract: see general_sub2grid_r_single_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!   2014-12-03  derber - make similar optimization changes already in code for
!                      double precision.
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(:)
      real(r_single),    intent(  out)  :: grid_vars(:)

      real(r_single),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      real(r_single),pointer,dimension(:,:,:,:) :: grid_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4  => rerank(sub_vars ,mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))
      grid_vars_r4 => rerank(grid_vars,mold4,(/s%inner_vars,s%nlat,s%nlon,s%kend_alloc-s%kbegin_loc+1/))

      call general_sub2grid_r_single_rank4(s,sub_vars_r4,grid_vars_r4)

   end subroutine general_sub2grid_r_single_rank11

   subroutine general_sub2grid_r_single_rank14(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_single_rank14  rank-1x4 interface to general_sub2grid_r
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sub2grid_r_single_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(:)
      real(r_single),     intent(  out) :: grid_vars(:,:,:,:)

      real(r_single),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4  => rerank(sub_vars,mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_sub2grid_r_single_rank4(s,sub_vars_r4,grid_vars)

   end subroutine general_sub2grid_r_single_rank14

   subroutine general_sub2grid_r_single_rank4(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_single_rank4  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of sub2grid--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with single precision (4-byte) real variables.
!              Input sub_vars, the desired arrays on horizontal subdomains, has one
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)
      real(r_single),     intent(  out) :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)

      real(r_single) :: sub_vars0(s%inner_vars,s%lat1,s%lon1,s%num_fields)
      real(r_single) :: work(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+1)) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,k,n,k_in,ilat,jlon,ierror,ioffset
      integer(i_long) mpi_string

!    remove halo row
!$omp parallel do  schedule(dynamic,1) private(k,j,j0,i0,i,ii)
      do k=1,s%num_fields
         do j=2,s%lon2-1
            j0=j-1
            do i=2,s%lat2-1
               i0=i-1
               do ii=1,s%inner_vars
                  sub_vars0(ii,i0,j0,k)=sub_vars(ii,i,j,k)
               end do
            end do
         end do
      end do
      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(sub_vars0,s%recvcounts,s%rdispls,mpi_string, &
                        work,s%sendcounts,s%sdispls,mpi_string,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)

      k_in=s%kend_loc-s%kbegin_loc+1

! Load grid_vars array in desired order
!$omp parallel do  schedule(dynamic,1) private(k,iskip,iloc,n,i,ilat,jlon,ii,ioffset)
      do k=s%kbegin_loc,s%kend_loc
         iskip=0
         iloc=0
         do n=1,s%npe
            if (n/=1) then
               iskip=iskip+s%ijn(n-1)*k_in
            end if
            ioffset=iskip+(k-s%kbegin_loc)*s%ijn(n)
            do i=1,s%ijn(n)
               iloc=iloc+1
               ilat=s%ltosi(iloc)
               jlon=s%ltosj(iloc)
               do ii=1,s%inner_vars
                  grid_vars(ii,ilat,jlon,k)=work(ii,i + ioffset)
               end do
            end do
         end do
      end do

   end subroutine general_sub2grid_r_single_rank4

   subroutine general_grid2sub_r_single_rank11(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_grid2sub_r_single_rank11  rank-1x1 interface to general_grid2sub_r
!   prgmmr: todling          org: np22                date: 2011-08-29
!
! abstract: see general_grid2sub_r_single_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(:)
      real(r_single),     intent(  out) :: sub_vars(:)

      real(r_single),pointer,dimension(:,:,:,:) :: grid_vars_r4=>NULL()
      real(r_single),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      grid_vars_r4=> rerank(grid_vars,mold4,(/s%inner_vars,s%nlat,s%nlon,s%kend_alloc-s%kbegin_loc+1/))
      sub_vars_r4 => rerank(sub_vars, mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_grid2sub_r_single_rank4(s,grid_vars_r4,sub_vars_r4)

   end subroutine general_grid2sub_r_single_rank11

   subroutine general_grid2sub_r_single_rank41(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_grid2sub_r_single_rank41  rank-4x1 interface to general_grid2sub_r
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_grid2sub_r_single_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_single),     intent(  out) :: sub_vars(:)

      real(r_single),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4 => rerank(sub_vars,mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_grid2sub_r_single_rank4(s,grid_vars,sub_vars_r4)

   end subroutine general_grid2sub_r_single_rank41

   subroutine general_grid2sub_r_single_rank4(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of grid2sub--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with single precision (4-byte) real variables.
!              Output sub_vars, the desired arrays on horizontal subdomains, has one 
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - remove setting halo to zero in output
!   2014-12-03  derber - make similar optimization changes already in code for
!                      double precision.
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use constants, only: zero
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_single),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)

      real(r_single) :: temp(s%inner_vars,s%itotsub*(s%kend_loc-s%kbegin_loc+1))
      integer(i_kind) iloc,i,ii,k,n,ilat,jlon,ierror,icount
      integer(i_kind),dimension(s%npe) ::iskip
      integer(i_long) mpi_string

!     reorganize for eventual distribution to local domains
      iskip(1)=0
      do n=2,s%npe
        iskip(n)=iskip(n-1)+s%ijn_s(n-1)*(s%kend_loc-s%kbegin_loc+1)
      end do
!$omp parallel do  schedule(dynamic,1) private(n,k,i,jlon,ii,ilat,iloc,icount)
      do k=s%kbegin_loc,s%kend_loc
         icount=0
         do n=1,s%npe
            iloc=iskip(n)+(k-s%kbegin_loc)*s%ijn_s(n)
            do i=1,s%ijn_s(n)
               iloc=iloc+1
               icount=icount+1
               ilat=s%ltosi_s(icount)
               jlon=s%ltosj_s(icount)
               do ii=1,s%inner_vars
                  temp(ii,iloc)=grid_vars(ii,ilat,jlon,k)
               end do
            end do
         end do
      end do


      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(temp,s%sendcounts_s,s%sdispls_s,mpi_string, &
                        sub_vars,s%recvcounts_s,s%rdispls_s,mpi_string,mpi_comm_world,ierror)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_grid2sub_r_single_rank4

   subroutine general_sub2grid_r_double_rank11(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_double_rank11 rank-1x1 interface
!   prgmmr: todling          org: np22                date: 2011-08-29
!
! abstract: see general_sub2grid_r_double_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(:)
      real(r_double),     intent(  out) :: grid_vars(:)

      real(r_double),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      real(r_double),pointer,dimension(:,:,:,:) :: grid_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4  => rerank(sub_vars, mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))
      grid_vars_r4 => rerank(grid_vars,mold4,(/s%inner_vars,s%nlat,s%nlon,s%kend_alloc-s%kbegin_loc+1/))

      call general_sub2grid_r_double_rank4(s,sub_vars_r4,grid_vars_r4)

   end subroutine general_sub2grid_r_double_rank11

   subroutine general_sub2grid_r_double_rank14(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_double_rank14 rank-1x4 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sub2grid_r_double_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(:)
      real(r_double),     intent(  out) :: grid_vars(:,:,:,:)

      real(r_double),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4  => rerank(sub_vars,mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_sub2grid_r_double_rank4(s,sub_vars_r4,grid_vars)

   end subroutine general_sub2grid_r_double_rank14

   subroutine general_sub2grid_r_double_rank4(s,sub_vars,grid_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sub2grid_r_double_rank4  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of sub2grid--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with double precision (8-byte) real variables.
!              Input sub_vars, the desired arrays on horizontal subdomains, has one
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)
      real(r_double),    intent(  out)  :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)

      real(r_double) :: sub_vars0(s%inner_vars,s%lat1,s%lon1,s%num_fields)
      real(r_double) :: work(s%inner_vars,s%itotsub*(s%kend_alloc-s%kbegin_loc+1)) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,k,n,k_in,ilat,jlon,ierror,ioffset
      integer(i_long) mpi_string

!    remove halo row
!$omp parallel do  schedule(dynamic,1) private(k,j,j0,i0,i,ii)
      do k=1,s%num_fields
         do j=2,s%lon2-1
            j0=j-1
            do i=2,s%lat2-1
               i0=i-1
               do ii=1,s%inner_vars
                  sub_vars0(ii,i0,j0,k)=sub_vars(ii,i,j,k)
               end do
            end do
         end do
      end do
      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(sub_vars0,s%recvcounts,s%rdispls,mpi_string, &
                        work,s%sendcounts,s%sdispls,mpi_string,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)

      k_in=s%kend_loc-s%kbegin_loc+1


! Load grid_vars array in desired order
!$omp parallel do  schedule(dynamic,1) private(k,iskip,iloc,n,i,ilat,jlon,ii,ioffset)
      do k=s%kbegin_loc,s%kend_loc
         iskip=0
         iloc=0
         do n=1,s%npe
            ioffset=iskip+(k-s%kbegin_loc)*s%ijn(n)
            do i=1,s%ijn(n)
               iloc=iloc+1
               ilat=s%ltosi(iloc)
               jlon=s%ltosj(iloc)
               do ii=1,s%inner_vars
                grid_vars(ii,ilat,jlon,k)=work(ii,i + ioffset)
               end do
            end do
            iskip=iskip+s%ijn(n)*k_in
         end do
      end do

   end subroutine general_sub2grid_r_double_rank4

   subroutine general_grid2sub_r_double_rank11(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_grid2sub_r_double_rank11  rank-1x1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_grid2sub_r_double_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_single,i_kind
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: grid_vars(:)
      real(r_double),     intent(  out) :: sub_vars(:)

      real(r_double),pointer,dimension(:,:,:,:) :: grid_vars_r4=>NULL()
      real(r_double),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      grid_vars_r4 => rerank(grid_vars,mold4,(/s%inner_vars,s%nlat,s%nlon,s%kend_alloc-s%kbegin_loc+1/))
      sub_vars_r4  => rerank(sub_vars, mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_grid2sub_r_double_rank4(s,grid_vars_r4,sub_vars_r4)

   end subroutine general_grid2sub_r_double_rank11

   subroutine general_grid2sub_r_double_rank41(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_grid2sub_r_double_rank41  rank-4x1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_grid2sub_r_double_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_double),     intent(  out) :: sub_vars(:)

      real(r_double),pointer,dimension(:,:,:,:) :: sub_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sub_vars_r4 => rerank(sub_vars,mold4,(/s%inner_vars,s%lat2,s%lon2,s%num_fields/))

      call general_grid2sub_r_double_rank4(s,grid_vars,sub_vars_r4)

   end subroutine general_grid2sub_r_double_rank41

   subroutine general_grid2sub_r_double_rank4(s,grid_vars,sub_vars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_grid2sub_r_double_rank4  convert from subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2010-02-11
!
! abstract: generalized version of grid2sub--uses only gsi module kinds.
!              All information needed is contained in the structure variable
!              "s", instead of various modules.  This allows
!              for easy adaptation for any collection/ordering of variables
!              defined on subdomains, which need to be made available on
!              full horizontal grid for horizontal operations.
!              The structure variable is specified by subroutine general_sub2grid_setup.
!              This version works with double precision (8-byte) real variables.
!              Output sub_vars, the desired arrays on horizontal subdomains, has one 
!              halo row, for now, which is filled with zero, since for ensemble use,
!              there is no need for a halo, but is easiest for now to keep it.
!              A later version will have variable number of halo rows, filled with proper values.
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2010-03-02  parrish - remove setting halo to zero in output
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use constants, only: zero
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon,s%kbegin_loc:s%kend_alloc)
      real(r_double),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2,s%num_fields)

      real(r_double) :: temp(s%inner_vars,s%itotsub*(s%kend_loc-s%kbegin_loc+1))
      integer(i_kind) iloc,icount,i,ii,k,n,ilat,jlon,ierror
      integer(i_long) mpi_string
      integer(i_kind),dimension(s%npe)::iskip

!     reorganize for eventual distribution to local domains
      iskip(1)=0
      do n=2,s%npe
        iskip(n)=iskip(n-1)+s%ijn_s(n-1)*(s%kend_loc-s%kbegin_loc+1)
      end do
!$omp parallel do  schedule(dynamic,1) private(n,k,i,jlon,ii,ilat,iloc,icount)
      do k=s%kbegin_loc,s%kend_loc
         icount=0
         do n=1,s%npe
            iloc=iskip(n)+(k-s%kbegin_loc)*s%ijn_s(n)
            do i=1,s%ijn_s(n)
               iloc=iloc+1
               icount=icount+1
               ilat=s%ltosi_s(icount)
               jlon=s%ltosj_s(icount)
               do ii=1,s%inner_vars
                  temp(ii,iloc)=grid_vars(ii,ilat,jlon,k)
               end do
            end do
         end do
      end do

      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_alltoallv(temp,s%sendcounts_s,s%sdispls_s,mpi_string, &
                        sub_vars,s%recvcounts_s,s%rdispls_s,mpi_string,mpi_comm_world,ierror)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_grid2sub_r_double_rank4

   subroutine general_gather2grid_r_single_rank11(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_single_rank11  rank-1x1 interface to general_gather2grid
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_gather2grid_r_single_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(:)
      real(r_single),    intent(  out)  :: grid_vars(:)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_single),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      real(r_single),pointer,dimension(:,:,:) :: grid_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3  => rerank(sub_vars ,mold3,(/s%inner_vars,s%lat2,s%lon2/))
      grid_vars_r3 => rerank(grid_vars,mold3,(/s%inner_vars,s%nlat,s%nlon/))

      call general_gather2grid_r_single_rank3(s,sub_vars_r3,grid_vars_r3,gridpe)

   end subroutine general_gather2grid_r_single_rank11

   subroutine general_gather2grid_r_single_rank13(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_single_rank13  rank-1x3 interface to general_gather2grid
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_gather2grid_r_single_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
! abstract: see general_sub2grid_r_single_rank4
!
! program history log:
!   2010-02-11  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(:)
      real(r_single),     intent(  out) :: grid_vars(:,:,:)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_single),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3  => rerank(sub_vars,mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_gather2grid_r_single_rank3(s,sub_vars_r3,grid_vars,gridpe)

   end subroutine general_gather2grid_r_single_rank13

   subroutine general_gather2grid_r_single_rank3(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_single_rank3  gather subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: generalization of subroutine gather_stuff2 that used to be located in bkgvar_rewgt.f90.  
!              Similar to general_sub2grid, but the difference is that it only works on a single 2d field and
!              gathers together the subdomains from all processors to a single full 2-d grid on 
!              user specified processor pe.  All vertical/variable related parts of the structure
!              variable s are ignored.  This routine is also intended to be a straightforward
!              replacement for the current messy mpi_allgatherv and associated code used for gathering
!              subdomain variables to a single processor.
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode (contains one halo row)
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2)
      real(r_single),     intent(  out) :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_single) :: sub_vars0(s%inner_vars,s%lat1,s%lon1)
      real(r_single) :: work(s%inner_vars,s%itotsub) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,n,ilat,jlon,ierror,ioffset
      integer(i_long) mpi_string


!    remove halo row

      do j=2,s%lon2-1
         j0=j-1
         do i=2,s%lat2-1
            i0=i-1
            do ii=1,s%inner_vars
               sub_vars0(ii,i0,j0)=sub_vars(ii,i,j)
            end do
         end do
      end do

      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_gatherv(sub_vars0,s%ijn(s%mype+1),mpi_string, &
                        work,s%ijn,s%displs_g,mpi_string,gridpe,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)

      if(s%mype==gridpe) then

! Load temp array in desired order

         iskip=0
         iloc=0
         do n=1,s%npe
            ioffset=iskip
            do i=1,s%ijn(n)
               iloc=iloc+1
               ilat=s%ltosi(iloc)
               jlon=s%ltosj(iloc)
               do ii=1,s%inner_vars
                  grid_vars(ii,ilat,jlon)=work(ii,i + ioffset)
               end do
            end do
            iskip=iskip+s%ijn(n)
         end do

      end if

   end subroutine general_gather2grid_r_single_rank3

   subroutine general_gather2grid_r_double_rank11(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_double_rank11 rank-1x1 interface
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_gather2grid_r_double_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(:)
      real(r_double),     intent(  out) :: grid_vars(:)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_double),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      real(r_double),pointer,dimension(:,:,:) :: grid_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3  => rerank(sub_vars, mold3,(/s%inner_vars,s%lat2,s%lon2/))
      grid_vars_r3 => rerank(grid_vars,mold3,(/s%inner_vars,s%nlat,s%nlon/))

      call general_gather2grid_r_double_rank3(s,sub_vars_r3,grid_vars_r3,gridpe)

   end subroutine general_gather2grid_r_double_rank11

   subroutine general_gather2grid_r_double_rank13(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_double_rank13 rank-1x3 interface
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_gather2grid_r_double_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(:)
      real(r_double),     intent(  out) :: grid_vars(:,:,:)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_double),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3  => rerank(sub_vars,mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_gather2grid_r_double_rank3(s,sub_vars_r3,grid_vars,gridpe)

   end subroutine general_gather2grid_r_double_rank13

   subroutine general_gather2grid_r_double_rank3(s,sub_vars,grid_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_gather2grid_r_double_rank3  gather subdomains to full horizontal grid
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: generalization of subroutine gather_stuff2 that used to be located in bkgvar_rewgt.f90.  
!              Similar to general_sub2grid, but the difference is that it only works on a single 2d field and
!              gathers together the subdomains from all processors to a single full 2-d grid on 
!              user specified processor pe.  All vertical/variable related parts of the structure
!              variable s are ignored.  This routine is also intended to be a straightforward
!              replacement for the current messy mpi_allgatherv and associated code used for gathering
!              subdomain variables to a single processor.
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     sub_vars   - input grid values in vertical subdomain mode
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     grid_vars  - output grid values in horizontal slab mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: sub_vars(s%inner_vars,s%lat2,s%lon2)
      real(r_double),    intent(  out)  :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_double) :: sub_vars0(s%inner_vars,s%lat1,s%lon1)
      real(r_double) :: work(s%inner_vars,max(s%iglobal,s%itotsub)) 
      real(r_double) :: temp(s%inner_vars,max(s%iglobal,s%itotsub)) 
      integer(i_kind) iloc,iskip,i,i0,ii,j,j0,n,ilat,jlon,ierror
      integer(i_long) mpi_string

!    remove halo row

      do j=2,s%lon2-1
         j0=j-1
         do i=2,s%lat2-1
            i0=i-1
            do ii=1,s%inner_vars
               sub_vars0(ii,i0,j0)=sub_vars(ii,i,j)
            end do
         end do
      end do

      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_gatherv(sub_vars0,s%ijn(s%mype+1),mpi_string, &
                        work,s%ijn,s%displs_g,mpi_string,gridpe,mpi_comm_world,ierror)

      call mpi_type_free(mpi_string,ierror)


      if(s%mype==gridpe) then

! Load temp array in desired order
         iskip=0
         iloc=0
         do n=1,s%npe
            do i=1,s%ijn(n)
               iloc=iloc+1
               do ii=1,s%inner_vars
                temp(ii,iloc)=work(ii,i + iskip)
               end do
            end do
            iskip=iskip+s%ijn(n)
         end do

! Transfer array temp to output array grid_vars
         do n=1,s%iglobal
            ilat=s%ltosi(n)
            jlon=s%ltosj(n)
            do ii=1,s%inner_vars
               grid_vars(ii,ilat,jlon)=temp(ii,n)
            end do
         end do

      end if

   end subroutine general_gather2grid_r_double_rank3

   subroutine general_scatter2sub_r_single_rank11(s,grid_vars,sub_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_scatter2sub_r_single_rank11  rank 1x1 interface to general_scatter2sub
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_scatter2sub_r_single_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(:)
      real(r_single),     intent(  out) :: sub_vars(:)
      integer(i_kind),intent(in   )     :: gridpe

      real(r_single),pointer,dimension(:,:,:) :: grid_vars_r3=>NULL()
      real(r_single),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      grid_vars_r3=> rerank(grid_vars,mold3,(/s%inner_vars,s%nlat,s%nlon/))
      sub_vars_r3 => rerank(sub_vars, mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_scatter2sub_r_single_rank3(s,grid_vars_r3,sub_vars_r3,gridpe)

   end subroutine general_scatter2sub_r_single_rank11

   subroutine general_scatter2sub_r_single_rank31(s,grid_vars,sub_vars,gridpe) 
!$$$  subprogram documentation block 
!                .      .    .                                       .  
! subprogram:    general_scatter2sub_r_single_rank31  rank 3x1 interface to general_scatter2sub
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_scatter2sub_r_single_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      real(r_single),     intent(  out) :: sub_vars(:)
      integer(i_kind),intent(in   )     :: gridpe

      real(r_single),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3 => rerank(sub_vars,mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_scatter2sub_r_single_rank3(s,grid_vars,sub_vars_r3,gridpe)

   end subroutine general_scatter2sub_r_single_rank31

   subroutine general_scatter2sub_r_single_rank3(s,grid_vars,sub_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_scatter2sub_r_single_rank3  scatter one full horizontal grid to subdomains.
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: generalization of subroutine scatter_stuff2 that used to be located in bkgvar_rewgt.f90.  
!              Similar to general_grid2sub, but the difference is that it only works on a single 2d field and
!              scatters it to the subdomains on all processors from the user specified processor where the
!              2d field resides.  All vertical/variable related parts of the structure
!              variable s are ignored.  This routine is also intended to be a straightforward
!              replacement for the current messy mpi_allscatterv and associated code used for scattering
!              a 2-d field from a single processor to subdomains.
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use constants, only: zero
      use mpimod, only: mpi_comm_world,mpi_real4
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_single), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      real(r_single),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2)
      integer(i_kind),intent(in   )     :: gridpe

      real(r_single) :: temp(s%inner_vars,s%itotsub)
      integer(i_kind) ii,n,ilat,jlon,ierror
      integer(i_long) mpi_string

!     reorganize for eventual distribution to local domains

      if(s%mype==gridpe) then

         do n=1,s%itotsub
            ilat=s%ltosi_s(n) ; jlon=s%ltosj_s(n)
            do ii=1,s%inner_vars
               temp(ii,n)=grid_vars(ii,ilat,jlon)
            end do
         end do

      end if

      call mpi_type_contiguous(s%inner_vars,mpi_real4,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_scatterv(temp,s%ijn_s,s%displs_s,mpi_string, &
                        sub_vars,s%ijn_s(s%mype+1),mpi_string,gridpe,mpi_comm_world,ierror)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_scatter2sub_r_single_rank3

   subroutine general_scatter2sub_r_double_rank11(s,grid_vars,sub_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_scatter2sub_r_double_rank11  rank 1x1 interface to general_scatter2sub
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_scatter2sub_r_double_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: grid_vars(:)
      real(r_double),     intent(  out) :: sub_vars(:)
      integer(i_kind),intent(in   )     :: gridpe

      real(r_double),pointer,dimension(:,:,:) :: grid_vars_r3=>NULL()
      real(r_double),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      grid_vars_r3 => rerank(grid_vars,mold3,(/s%inner_vars,s%nlat,s%nlon/))
      sub_vars_r3  => rerank(sub_vars, mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_scatter2sub_r_double_rank3(s,grid_vars_r3,sub_vars_r3,gridpe)

   end subroutine general_scatter2sub_r_double_rank11

   subroutine general_scatter2sub_r_double_rank31(s,grid_vars,sub_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_scatter2sub_r_double_rank31  rank 3x1 interface to general_scatter2sub
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: see general_scatter2sub_r_single_rank3
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double),     intent(in   ) :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      real(r_double),     intent(  out) :: sub_vars(:)
      integer(i_kind),    intent(in   ) :: gridpe

      real(r_double),pointer,dimension(:,:,:) :: sub_vars_r3=>NULL()
      integer(i_kind) mold3(2,2,2)

      sub_vars_r3 => rerank(sub_vars,mold3,(/s%inner_vars,s%lat2,s%lon2/))

      call general_scatter2sub_r_double_rank3(s,grid_vars,sub_vars_r3,gridpe)

   end subroutine general_scatter2sub_r_double_rank31

   subroutine general_scatter2sub_r_double_rank3(s,grid_vars,sub_vars,gridpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_scatter2sub_r_double_rank3  scatter one full horizontal grid to subdomains.
!   prgmmr: parrish          org: np22                date: 2012-06-06
!
! abstract: generalization of subroutine scatter_stuff2 that used to be located in bkgvar_rewgt.f90.  
!              Similar to general_grid2sub, but the difference is that it only works on a single 2d field and
!              scatters it to the subdomains on all processors from the user specified processor where the
!              2d field resides.  All vertical/variable related parts of the structure
!              variable s are ignored.  This routine is also intended to be a straightforward
!              replacement for the current messy mpi_allscatterv and associated code used for scattering
!              a 2-d field from a single processor to subdomains.
!
! program history log:
!   2012-06-06  parrish, initial documentation
!
!   input argument list:
!     s          - structure variable, contains all necessary information for
!                    moving this set of subdomain variables sub_vars to
!                    the corresponding set of full horizontal grid variables.
!     grid_vars  - input grid values in horizontal slab mode.
!     gridpe     - processor where output grid resides
!
!   output argument list:
!     sub_vars   - output grid values in vertical subdomain mode.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use constants, only: zero
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),intent(in   ) :: s
      real(r_double), intent(in   )     :: grid_vars(s%inner_vars,s%nlat,s%nlon)
      real(r_double),     intent(  out) :: sub_vars(s%inner_vars,s%lat2,s%lon2)
      integer(i_kind),intent(in   )     :: gridpe

      real(r_double) :: temp(s%inner_vars,s%itotsub)
      integer(i_kind) ii,n,ilat,jlon,ierror
      integer(i_long) mpi_string

!     reorganize for eventual distribution to local domains

      if(s%mype==gridpe) then

         do n=1,s%itotsub
            ilat=s%ltosi_s(n) ; jlon=s%ltosj_s(n)
            do ii=1,s%inner_vars
               temp(ii,n)=grid_vars(ii,ilat,jlon)
            end do
         end do

      end if

      call mpi_type_contiguous(s%inner_vars,mpi_real8,mpi_string,ierror)
      call mpi_type_commit(mpi_string,ierror)

      call mpi_scatterv(temp,s%ijn_s,s%displs_s,mpi_string, &
                        sub_vars,s%ijn_s(s%mype+1),mpi_string,gridpe,mpi_comm_world,ierror)
      call mpi_type_free(mpi_string,ierror)

   end subroutine general_scatter2sub_r_double_rank3

   subroutine general_sube2suba_r_single_rank1(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_single_rank1  rank-1 inerface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sube2suba_r_single_rank4
!
! program history log:
!   2010-02-27  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_egrid2agrid,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(in   ) :: sube_vars(:)
      real(r_single),        intent(  out) :: suba_vars(:)
      logical,               intent(in   ) :: regional

      real(r_single),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      real(r_single),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))
      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))

      call general_sube2suba_r_single_rank4(se,sa,p_e2a,sube_vars_r4,suba_vars_r4,regional)

   end subroutine general_sube2suba_r_single_rank1

   subroutine general_sube2suba_r_single_rank4(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_single_rank4  interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-27
!
! abstract: interpolate ensemble grid variables to analysis grid variables,
!              where input and output are in the respective subdomains as defined
!              by the structure variables se and sa.
!
! program history log:
!   2010-02-27  parrish, initial documentation
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_egrid2agrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(in   ) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_single),        intent(  out) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_single),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      call general_sub2grid_r_single_rank4(se,sube_vars,gride_vars)
      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         write(6,*)' not ready for regional dual_res yet'
         call mpi_finalize(k)
         stop
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_egrid2agrid(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(gride_vars,vectorx)
      call general_grid2sub_r_single_rank4(sa,grida_vars,suba_vars)
      deallocate(grida_vars)

   end subroutine general_sube2suba_r_single_rank4

   subroutine general_sube2suba_r_double_rank1(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double_rank1  rank-1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sube2suba_r_double_rank4
!
! program history log:
!   2010-02-27  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_egrid2agrid,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: sube_vars(:)
      real(r_double),        intent(  out) :: suba_vars(:)
      logical,               intent(in   ) :: regional

      real(r_double),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      real(r_double),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))
      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))

      call general_sube2suba_r_double_rank4(se,sa,p_e2a,sube_vars_r4,suba_vars_r4,regional)

   end subroutine general_sube2suba_r_double_rank1

   subroutine general_sube2suba_r_double_rank4(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double_rank4  interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-27
!
! abstract: interpolate ensemble grid variables to analysis grid variables,
!              where input and output are in the respective subdomains as defined
!              by the structure variables se and sa.
!
! program history log:
!   2010-02-27  parrish, initial documentation
!   2012-02-08  parrish - add code for regional dual-res application.
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: egrid2agrid,g_egrid2agrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_double),        intent(  out) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      call general_sub2grid_r_double_rank4(se,sube_vars,gride_vars)
      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         call egrid2agrid(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc)
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_egrid2agrid(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(gride_vars,vectorx)
      call general_grid2sub_r_double_rank4(sa,grida_vars,suba_vars)
      deallocate(grida_vars)

   end subroutine general_sube2suba_r_double_rank4

   subroutine general_sube2suba_r_single_rank1_ad(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_single_rank1_ad  rank-1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sube2suba_r_single_rank4_ad
!
! program history log:
!   2010-02-28  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_egrid2agrid_ad,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(  out) :: sube_vars(:)
      real(r_single),        intent(in   ) :: suba_vars(:)
      logical,               intent(in   ) :: regional

      real(r_single),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      real(r_single),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))
      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))

      call general_sube2suba_r_single_rank4_ad(se,sa,p_e2a,sube_vars_r4,suba_vars_r4,regional)

   end subroutine general_sube2suba_r_single_rank1_ad

   subroutine general_sube2suba_r_single_rank4_ad(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_single_rank4_ad  adjoint of interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-28
!
! abstract: adjoint of general_sube2suba_r_double.
!
! program history log:
!   2010-02-28  parrish, initial documentation
!   2012-02-08  parrish - add code for regional dual-res application.
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: egrid2agrid_ad,g_egrid2agrid_ad,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_single),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_single),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_single_rank4(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         call egrid2agrid_ad(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc)
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_egrid2agrid_ad(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(grida_vars,vectorx)
      call general_grid2sub_r_single_rank4(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_sube2suba_r_single_rank4_ad

   subroutine general_sube2suba_r_double_rank1_ad(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double_rank1_ad  rank-1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_sube2suba_r_double_rank4_ad
!
! program history log:
!   2010-02-28  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_egrid2agrid_ad,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(  out) :: sube_vars(:)
      real(r_double),        intent(in   ) :: suba_vars(:)
      logical,               intent(in   ) :: regional

      real(r_double),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      real(r_double),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))
      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))

      call general_sube2suba_r_double_rank4_ad(se,sa,p_e2a,sube_vars_r4,suba_vars_r4,regional)

   end subroutine general_sube2suba_r_double_rank1_ad

   subroutine general_sube2suba_r_double_rank4_ad(se,sa,p_e2a,sube_vars,suba_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_sube2suba_r_double_rank4_ad  adjoint of interpolate ens grid to anl grid
!   prgmmr: parrish          org: np22                date: 2010-02-28
!
! abstract: adjoint of general_sube2suba_r_double.
!
! program history log:
!   2010-02-28  parrish, initial documentation
!   2012-02-08  parrish - add code for regional dual-res application.
!
!   input argument list:
!     se         - ensemble grid structure variable
!     sa         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     sube_vars  - input ensemble grid values in ensemble subdomain mode (as defined by se)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     suba_vars  - output analysis grid values in analysis subdomain mode (as defined by sa)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: egrid2agrid_ad,g_egrid2agrid_ad,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: se,sa
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      real(r_double),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_double_rank4(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         call egrid2agrid_ad(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc)
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_egrid2agrid_ad(p_e2a,gride_vars,grida_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(grida_vars,vectorx)
      call general_grid2sub_r_double_rank4(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_sube2suba_r_double_rank4_ad

   subroutine general_suba2sube_r_single_rank1(sa,se,p_e2a,suba_vars,sube_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_suba2sube_r_single_rank1  rank-1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_suba2sube_r_single_rank4
!
! program history log:
!   2010-03-01  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     sa         - ensemble grid structure variable
!     se         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     suba_vars  - input analysis grid values in analysis subdomain mode (as defined by sa)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     sube_vars  - output ensemble grid values in ensemble subdomain mode (as defined by se)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_agrid2egrid,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: sa,se
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(in   ) :: suba_vars(:)
      real(r_single),        intent(  out) :: sube_vars(:)
      logical,               intent(in   ) :: regional

      real(r_single),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      real(r_single),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))
      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))

      call general_suba2sube_r_single_rank4(sa,se,p_e2a,suba_vars_r4,sube_vars_r4,regional)

   end subroutine general_suba2sube_r_single_rank1

   subroutine general_suba2sube_r_single_rank4(sa,se,p_e2a,suba_vars,sube_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_suba2sube_r_single_rank4  smoothing interpolate anl grid to ens grid
!   prgmmr: parrish          org: np22                date: 2010-03-01
!
! abstract: smoothing interpolation from analysis grid to ensemble grid (analysis subdomain
!            input, ensemble subdomain output).
!
! program history log:
!   2010-03-01  parrish, initial documentation
!
!   input argument list:
!     sa         - ensemble grid structure variable
!     se         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     suba_vars  - input analysis grid values in analysis subdomain mode (as defined by sa)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     sube_vars  - output ensemble grid values in ensemble subdomain mode (as defined by se)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_agrid2egrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: sa,se
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_single),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      real(r_single),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      logical,               intent(in   ) :: regional

      real(r_single),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_single_rank4(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         write(6,*)' not ready for regional dual_res yet'
         call mpi_finalize(k)
         stop
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_agrid2egrid(p_e2a,grida_vars,gride_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(grida_vars,vectorx)
      call general_grid2sub_r_single_rank4(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_suba2sube_r_single_rank4

   subroutine general_suba2sube_r_double_rank1(sa,se,p_e2a,suba_vars,sube_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_suba2sube_r_double_rank1  rank-1 interface
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see general_suba2sube_r_double_rank4
!
! program history log:
!   2010-03-01  parrish, initial documentation
!   2011-07-26  todling, rank-1 interface
!
!   input argument list:
!     sa         - ensemble grid structure variable
!     se         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     suba_vars  - input analysis grid values in analysis subdomain mode (as defined by sa)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     sube_vars  - output ensemble grid values in ensemble subdomain mode (as defined by se)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: g_agrid2egrid,egrid2agrid_parm
      use m_rerank, only: rerank
      implicit none

      type(sub2grid_info),   intent(in   ) :: sa,se
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: suba_vars(:)
      real(r_double),        intent(  out) :: sube_vars(:)
      logical,               intent(in   ) :: regional

      real(r_double),pointer,dimension(:,:,:,:) :: suba_vars_r4=>NULL()
      real(r_double),pointer,dimension(:,:,:,:) :: sube_vars_r4=>NULL()
      integer(i_kind) mold4(2,2,2,2)

      suba_vars_r4 => rerank(suba_vars,mold4,(/sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields/))
      sube_vars_r4 => rerank(sube_vars,mold4,(/se%inner_vars,se%lat2,se%lon2,se%num_fields/))

      call general_suba2sube_r_double_rank4(sa,se,p_e2a,suba_vars_r4,sube_vars_r4,regional)

   end subroutine general_suba2sube_r_double_rank1

   subroutine general_suba2sube_r_double_rank4(sa,se,p_e2a,suba_vars,sube_vars,regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_suba2sube_r_double_rank4  smoothing interpolate anl grid to ens grid
!   prgmmr: parrish          org: np22                date: 2010-03-01
!
! abstract: smoothing interpolation from analysis grid to ensemble grid (analysis subdomain
!            input, ensemble subdomain output).
!
! program history log:
!   2010-03-01  parrish, initial documentation
!   2012-02-08  parrish - add code for regional dual-res application.
!
!   input argument list:
!     sa         - ensemble grid structure variable
!     se         - analysis grid structure variable
!     p_e2a      - interpolation from ensemble to grid to analysis grid structure variable
!     suba_vars  - input analysis grid values in analysis subdomain mode (as defined by sa)
!     regional   - true for regional grids--this code currently works only with global grids
!                     need to fix this.
!
!   output argument list:
!     sube_vars  - output ensemble grid values in ensemble subdomain mode (as defined by se)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use egrid2agrid_mod, only: agrid2egrid,g_agrid2egrid,egrid2agrid_parm
      use mpimod, only: mpi_comm_world,mpi_real8
      implicit none

      type(sub2grid_info),   intent(in   ) :: sa,se
      type(egrid2agrid_parm),intent(in   ) :: p_e2a
      real(r_double),        intent(in   ) :: suba_vars(sa%inner_vars,sa%lat2,sa%lon2,sa%num_fields)
      real(r_double),        intent(  out) :: sube_vars(se%inner_vars,se%lat2,se%lon2,se%num_fields)
      logical,               intent(in   ) :: regional

      real(r_double),allocatable:: gride_vars(:,:,:,:),grida_vars(:,:,:,:)
      logical,allocatable :: vectorx(:)
      integer(i_kind) k

      allocate(grida_vars(sa%inner_vars,sa%nlat,sa%nlon,sa%kbegin_loc:sa%kend_alloc))
      call general_sub2grid_r_double_rank4(sa,suba_vars,grida_vars)
      allocate(gride_vars(se%inner_vars,se%nlat,se%nlon,se%kbegin_loc:se%kend_alloc))
      allocate(vectorx(sa%kbegin_loc:sa%kend_alloc))
      if(regional) then
         call agrid2egrid(p_e2a,grida_vars,gride_vars,se%kbegin_loc,se%kend_loc)
      else
         do k=se%kbegin_loc,se%kend_loc
           vectorx(k)=se%vector(k)
         end do
         call g_agrid2egrid(p_e2a,grida_vars,gride_vars,se%kbegin_loc,se%kend_loc,vectorx)
      end if
      deallocate(grida_vars,vectorx)
      call general_grid2sub_r_double_rank4(se,gride_vars,sube_vars)
      deallocate(gride_vars)

   end subroutine general_suba2sube_r_double_rank4

end module general_sub2grid_mod
