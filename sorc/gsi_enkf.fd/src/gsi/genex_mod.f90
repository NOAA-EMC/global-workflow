module genex_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    genex_mod  utility to rearrange grids using minimal memory
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: The routines in this module are intended to replace sub2grid and grid2sub with
!             more general routines which are not limited to just the options of xy subdomains
!             and xy full fields represented on every processor.  GSI was designed around full
!             horizontal grids being available on one processor for spectral transforms,
!             recursive filter computations, I/O of model fields, etc.  Sub2grid/grid2sub
!             was originally created for this purpose.  However, model resolutions and computer
!             architecture have made this design obsolete.
!
!
! program history log:
!   2017-01-03  parrish, initial documentation
!   2017-11-12  mahajan, fold into GSI
!
! subroutines included:
!   sub genex_create_info   - generic call for setup of grid rearrangement over multiple processors
!   sub genex_destroy_info  - generic call for deallocate/set to null genex_info variable
!   sub genex               - generic call for grid rearrangement over multiple processors

! Variable Definitions:
!   def genex_info          - contains information needed for grid rearrangement across processors
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  BEGIN BRIEF TUTORIAL:

!   The following illustrates a sample input domain A and output domain B on one processor
!   for a 2-dimensional array distributed across multiple processors (with arbitrary overlap).
!   Note that the indices are defined generally in the simplest n-dimensional cartesian space.
!   The full extent of the 2-dimensional domain need not be specified for genex to work.
!   However, it is assumed that the subdomain indices are based on a common origin point.
!   Each subdomain contains an interior defined by
!
!                            ias  <= i <= iae , jas  <= j <= jae     for input
!
!                            ibs  <= i <= ibe , jbs  <= j <= jbe     for output
!
!  and the actual allocated array size (which may or may not overlap with subdomains from other
!  processors)
!
!                            iasm <= i <= iaem, jasm <= j <= jaem    for input
!
!                            ibsm <= i <= ibem, jbsm <= j <= jbem    for output
!
!  Note that iasm <= ias <=iaem, jasm <= jas <= jaem  and
!            ibsm <= ibs <=ibem, jbsm <= jbs <= jbem
!
!  The i and j indices are defined in a fixed global coordinate of unspecified size.
!
!   However, iae < ias and/or jae < jas is allowed.  In this case, A and/or B is
!   non-existant on this processor.
!
!   Using this simple definition for an array with arbitrary rectilinear
!   distributions  A and B,  a trivial setup procedure allows for very easy
!   conversion of distribution A to distribution B.  See illustration below:
!
!     iasm                      iaem
!
!  jasm * - - - - - - - - - - - - *
!       |                         |  <---- array A
!       |  ias              iae   |                       i
!  jas  |   +----------------+    |                     ---->
!       |   |                |    |
!       |   |                |    |                           |
!       |   |   * - - - - - -|- - | - - - - *  jbsm           |
!       |   |   |            |    |         |                 |  j
!       |   |   |   +--------|----|------+  |  jbs            |
!       |   |   |   |        |    |      |  |                 v
!       |   |   |   |        |    |      |  |
!  jae  |   +------ ---------+    |      |  |
!       |       |   |             |      |  |  <---- array B
!  jaem * - - - - - | - - - - - - *      |  |
!               |   |                    |  |
!               |   +--------------------+  |  jbe
!               |                           |
!               * - - - - - - - - - - - - - *  jbem
!                  ibs                 ibe
!             ibsm                        ibem
!
!  The following code sample shows how to set this up:
!    (assumes ias, ..., jbem are all defined and arrays a and b already contain desired contents)
!
!     type(genex_info) :: s2d
!     integer(i_kind) ::         ias ,iae ,jas ,jae , ibs ,ibe ,jbs ,jbe
!     integer(i_kind) ::         iasm,iaem,jasm,jaem, ibsm,ibem,jbsm,jbem
!     real(r_single)  ::  a(iasm:iaem,jasm:jaem)
!     real(r_single)  ::  b(ibsm:ibem,jbsm:jbem)
!
!     call genex_create_info(s2d,ias ,iae ,jas ,jae , ibs ,ibe ,jbs ,jbe , &
!                                iasm,iaem,jasm,jaem, ibsm,ibem,jbsm,jbem)
!     call genex(s2d,a,b)
!
!   Now array "b" contains redistribution from array "a" distribution across processors.
!   This is a pretty robust code, which can be expanded (in principle at least) to cover
!   any number of desired dimensions.
!
!  END BRIEF TUTORIAL:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use kinds, only: i_kind
   use kinds, only: r_single,r_double

   use mpimod, only: mpi_status_size,mpi_comm_world
   use mpimod, only: mpi_integer4
   use mpimod, only: mpi_real4,mpi_real8
   use mpimod, only: npe,mype

   implicit none

! set default to private
   private
! set subroutines to public
   public :: genex_create_info
   public :: genex_destroy_info
   public :: genex
! set passed variables to public
   public :: genex_info

   interface genex_create_info
      module procedure genex_create_info2
      module procedure genex_create_info3
      module procedure genex_create_info4
   end interface

   interface genex
      module procedure genex2_r_single
      module procedure genex3_r_single
      module procedure genex4_r_single
      module procedure genex2_r_double
      module procedure genex3_r_double
      module procedure genex4_r_double
   end interface

   type genex_info

!---interior coordinate range for input submatrix
      integer(i_kind) ias,iae         !  input 1st dimension range
      integer(i_kind) jas,jae         !  input 2nd dimension range
      integer(i_kind) kas,kae         !  input 3rd dimension range
      integer(i_kind) mas,mae         !  input 4th dimension range

!---allocated y,x coordinate range for input submatrix
      integer(i_kind) iasm,iaem,iaemz !  allocated 1st dimension range
      integer(i_kind) jasm,jaem,jaemz !  allocated 2nd dimension range
      integer(i_kind) kasm,kaem,kaemz !  allocated 3rd dimension range
      integer(i_kind) masm,maem,maemz !  allocated 4th dimension range

!---interior coordinate range for output submatrix
      integer(i_kind) ibs,ibe         ! output 1st dimension range
      integer(i_kind) jbs,jbe         ! output 2nd dimension range
      integer(i_kind) kbs,kbe         ! output 3rd dimension range
      integer(i_kind) mbs,mbe         ! output 4th dimension range

!---allocated coordinate range for output submatrix
      integer(i_kind) ibsm,ibem,ibemz ! allocated 1st dimension range
      integer(i_kind) jbsm,jbem,jbemz ! allocated 2nd dimension range
      integer(i_kind) kbsm,kbem,kbemz ! allocated 3rd dimension range
      integer(i_kind) mbsm,mbem,mbemz ! allocated 4th dimension range

!---internally generated variables used when calling genex for this setup.
      integer(i_kind) iainc
      integer(i_kind) maxbuf          ! max buffer size
      integer(i_kind) myis,myie
      integer(i_kind) myjs,myje
      integer(i_kind) myks,myke
      integer(i_kind) myms,myme
      integer(i_kind) npe             ! total number of processors
      integer(i_kind) mype            ! local processor
      logical:: lallocated = .false.
      integer(i_kind),pointer ::   lefts(:) => NULL()
      integer(i_kind),pointer ::  rights(:) => NULL()
      integer(i_kind),pointer ::    numl(:) => NULL()
      integer(i_kind),pointer ::   numrc(:) => NULL()
      integer(i_kind),pointer ::  iabs_l(:) => NULL()
      integer(i_kind),pointer ::  iabe_l(:) => NULL()
      integer(i_kind),pointer ::  jabs_l(:) => NULL()
      integer(i_kind),pointer ::  jabe_l(:) => NULL()
      integer(i_kind),pointer ::  kabs_l(:) => NULL()
      integer(i_kind),pointer ::  kabe_l(:) => NULL()
      integer(i_kind),pointer ::  mabs_l(:) => NULL()
      integer(i_kind),pointer ::  mabe_l(:) => NULL()
      integer(i_kind),pointer :: iabs_rc(:) => NULL()
      integer(i_kind),pointer :: iabe_rc(:) => NULL()
      integer(i_kind),pointer :: jabs_rc(:) => NULL()
      integer(i_kind),pointer :: jabe_rc(:) => NULL()
      integer(i_kind),pointer :: kabs_rc(:) => NULL()
      integer(i_kind),pointer :: kabe_rc(:) => NULL()
      integer(i_kind),pointer :: mabs_rc(:) => NULL()
      integer(i_kind),pointer :: mabe_rc(:) => NULL()

   end type genex_info

   contains

subroutine genex_create_info2(s,ias ,iae ,jas ,jae , &
                                ibs ,ibe ,jbs ,jbe , &
                                iasm,iaem,jasm,jaem, &
                                ibsm,ibem,jbsm,jbem)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex_create_info2
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: setup structure variable for 2-d array rearrangement over processors
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     ias ,iae ,jas ,jae  :  input array dimension ranges
!     ibs ,ibe ,jbs ,jbe  :  output array dimension ranges
!     iasm,iaem,jasm,jaem :  allocated input array dimension ranges
!     ibsm,ibem,jbsm,jbem :  allocated output array dimension ranges
!     s                   :  type(genex_info) structure variable
!
!   output argument list:
!     s                   :  type(genex_info) structure variable
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!  iasm <= ias <= iae <= iaem  ... etc.

   implicit none

   integer(i_kind) ,intent(in   ) :: ias ,iae ,jas ,jae
   integer(i_kind) ,intent(in   ) :: ibs ,ibe ,jbs ,jbe
   integer(i_kind) ,intent(in   ) :: iasm,iaem,jasm,jaem
   integer(i_kind) ,intent(in   ) :: ibsm,ibem,jbsm,jbem
   type(genex_info),intent(inout):: s

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,irecv_r,isend_l,isend_r,iserr,irerr,ierr
   integer(i_kind) ibuf0(4),ibuf1(4),ibuf2(4),ibuf3(4)
   integer(i_kind) ias_l,iae_l,jas_l,jae_l
   integer(i_kind) numx,numy

   s%npe=npe
   s%mype=mype

   s%ias=ias ; s%iae=iae
   s%jas=jas ; s%jae=jae
   s%ibs=ibs ; s%ibe=ibe
   s%jbs=jbs ; s%jbe=jbe

   s%iasm=iasm ; s%iaem=iaem ; s%iaemz=max(iasm,iaem)
   s%jasm=jasm ; s%jaem=jaem ; s%jaemz=max(jasm,jaem)
   s%ibsm=ibsm ; s%ibem=ibem ; s%ibemz=max(ibsm,ibem)
   s%jbsm=jbsm ; s%jbem=jbem ; s%jbemz=max(jbsm,jbem)

! -- do copy first of intersection of a and b on mype ---------------------
   s%myis=max(ias,ibs) ; s%myie=min(iae,ibe)
   s%myjs=max(jas,jbs) ; s%myje=min(jae,jbe)
   if(s%lallocated) then
      deallocate(s%lefts,s%rights,s%numl,s%numrc,s%iabs_l,s%iabe_l)
      deallocate(s%jabs_l,s%jabe_l,s%iabs_rc,s%iabe_rc,s%jabs_rc,s%jabe_rc)
   end if
   s%lallocated=.true.

   allocate(s%lefts(npe-1),s%rights(npe-1))
   do n=1,npe-1
      s%lefts(n)=mype-n
      if(s%lefts(n) < 0) s%lefts(n)=s%lefts(n)+npe
      s%rights(n)=mype+n
      if(s%rights(n) > npe-1) s%rights(n)=s%rights(n)-npe
   end do

   allocate(s%numl(npe-1),s%numrc(npe-1))
   allocate(s%iabs_l(npe-1),s%iabe_l(npe-1))
   allocate(s%iabs_rc(npe-1),s%iabe_rc(npe-1))
   allocate(s%jabs_l(npe-1),s%jabe_l(npe-1))
   allocate(s%jabs_rc(npe-1),s%jabe_rc(npe-1))
   s%maxbuf=0
   do n=1,npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(ibuf0,4,mpi_integer4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ibuf2(1)=ias ; ibuf2(2)=iae ; ibuf2(3)=jas ; ibuf2(4)=jae
         call mpi_issend(ibuf2,4,mpi_integer4,my_neb_r,mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ias_l=ibuf0(1) ; iae_l=ibuf0(2) ; jas_l=ibuf0(3) ; jae_l=ibuf0(4)
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

      s%iabs_l(n)=max(ias_l,ibs)
      s%iabe_l(n)=min(iae_l,ibe)
      s%jabs_l(n)=max(jas_l,jbs)
      s%jabe_l(n)=min(jae_l,jbe)

! -- Receive from right ------------------
      if(my_neb_r >= 0) then
         call mpi_irecv(ibuf1,4,mpi_integer4,my_neb_r,my_neb_r,mpi_comm_world,irecv_r,irerr)
      end if

! -- Send to left ------------------------
      if(my_neb_l >= 0) then
         ibuf3(1)=s%iabs_l(n) ; ibuf3(2)=s%iabe_l(n) ; ibuf3(3)=s%jabs_l(n) ; ibuf3(4)=s%jabe_l(n)
         call mpi_issend(ibuf3,4,mpi_integer4,my_neb_l,mype,mpi_comm_world,isend_l,iserr)
      end if

! -- Store from right --------------------
      if(my_neb_r >=0) then
         call mpi_wait(irecv_r,istat,ierr)
         s%iabs_rc(n)=ibuf1(1) ; s%iabe_rc(n)=ibuf1(2) ; s%jabs_rc(n)=ibuf1(3) ; s%jabe_rc(n)=ibuf1(4)
      end if

! -- Release issend ---------------------
      if(my_neb_l >= 0) then
         call mpi_wait(isend_l,istat,ierr)
      end if

      numy=s%iabe_l(n)+1-s%iabs_l(n)
      numx=s%jabe_l(n)+1-s%jabs_l(n)
      if( numx <= 0 .or. numy <= 0 ) then
         s%numl(n)=-1 ; s%lefts(n)=-1
      else
         s%numl(n)=numx*numy
      end if

      numy=s%iabe_rc(n)+1-s%iabs_rc(n)
      numx=s%jabe_rc(n)+1-s%jabs_rc(n)
      if( numx <= 0 .or. numy <= 0 ) then
         s%numrc(n)=-1 ; s%rights(n)=-1
      else
         s%numrc(n)=numx*numy
      end if

      s%maxbuf=max(s%maxbuf,s%numl(n),s%numrc(n))

   end do

end subroutine genex_create_info2

subroutine genex_create_info3(s,ias ,iae ,jas ,jae ,kas ,kae , &
                                ibs ,ibe ,jbs ,jbe ,kbs ,kbe , &
                                iasm,iaem,jasm,jaem,kasm,kaem, &
                                ibsm,ibem,jbsm,jbem,kbsm,kbem)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex_create_info3
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: setup structure variable for 2-d array rearrangement over processors
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     ias ,iae ,jas ,jae ,kas ,kae :  input array dimension ranges
!     ibs ,ibe ,jbs ,jbe ,kbs ,kbe :  output array dimension ranges
!     iasm,iaem,jasm,jaem,kasm,kaem:  allocated input array dimension ranges
!     ibsm,ibem,jbsm,jbem,kbsm,kbem:  allocated output array dimension ranges
!     s                   :  type(genex_info) structure variable
!
!   output argument list:
!     s                   :  type(genex_info) structure variable
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   integer(i_kind),intent(in   ) :: ias ,iae ,jas ,jae ,kas ,kae
   integer(i_kind),intent(in   ) :: ibs ,ibe ,jbs ,jbe ,kbs ,kbe
   integer(i_kind),intent(in   ) :: iasm,iaem,jasm,jaem,kasm,kaem
   integer(i_kind),intent(in   ) :: ibsm,ibem,jbsm,jbem,kbsm,kbem
   type(genex_info),intent(inout):: s

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,irecv_r,isend_l,isend_r,iserr,irerr,ierr
   integer(i_kind) ibuf0(6),ibuf1(6),ibuf2(6),ibuf3(6)
   integer(i_kind) ias_l,iae_l,jas_l,jae_l,kas_l,kae_l
   integer(i_kind) numx,numy,numz

   s%npe=npe
   s%mype=mype

   s%ias=ias ; s%iae=iae
   s%jas=jas ; s%jae=jae
   s%kas=kas ; s%kae=kae
   s%ibs=ibs ; s%ibe=ibe
   s%jbs=jbs ; s%jbe=jbe
   s%kbs=kbs ; s%kbe=kbe

   s%iasm=iasm ; s%iaem=iaem ; s%iaemz=max(ias,iae)
   s%jasm=jasm ; s%jaem=jaem ; s%jaemz=max(jas,jae)
   s%kasm=kasm ; s%kaem=kaem ; s%kaemz=max(kas,kae)
   s%ibsm=ibsm ; s%ibem=ibem ; s%ibemz=max(ibs,ibe)
   s%jbsm=jbsm ; s%jbem=jbem ; s%jbemz=max(jbs,jbe)
   s%kbsm=kbsm ; s%kbem=kbem ; s%kbemz=max(kbs,kbe)

! -- do copy first of intersection of a and b on mype ---------------------
   s%myis=max(ias,ibs) ; s%myie=min(iae,ibe)
   s%myjs=max(jas,jbs) ; s%myje=min(jae,jbe)
   s%myks=max(kas,kbs) ; s%myke=min(kae,kbe)
   if(s%lallocated) then
      deallocate(s%lefts,s%rights,s%numl,s%numrc,s%iabs_l,s%iabe_l)
      deallocate(s%jabs_l,s%jabe_l,s%iabs_rc,s%iabe_rc,s%jabs_rc,s%jabe_rc)
      deallocate(s%kabs_l,s%kabe_l,s%kabs_rc,s%kabe_rc)
   end if
   s%lallocated=.true.

   allocate(s%lefts(npe-1),s%rights(npe-1))
   do n=1,npe-1
      s%lefts(n)=mype-n
      if(s%lefts(n) < 0) s%lefts(n)=s%lefts(n)+npe
      s%rights(n)=mype+n
      if(s%rights(n) > npe-1) s%rights(n)=s%rights(n)-npe
   end do

   allocate(s%numl(npe-1),s%numrc(npe-1))
   allocate(s%iabs_l(npe-1),s%iabe_l(npe-1))
   allocate(s%iabs_rc(npe-1),s%iabe_rc(npe-1))
   allocate(s%jabs_l(npe-1),s%jabe_l(npe-1))
   allocate(s%jabs_rc(npe-1),s%jabe_rc(npe-1))
   allocate(s%kabs_l(npe-1),s%kabe_l(npe-1))
   allocate(s%kabs_rc(npe-1),s%kabe_rc(npe-1))
   s%maxbuf=0
   do n=1,npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(ibuf0,6,mpi_integer4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ibuf2(1)=ias ; ibuf2(2)=iae ; ibuf2(3)=jas ; ibuf2(4)=jae
         ibuf2(5)=kas ; ibuf2(6)=kae
         call mpi_issend(ibuf2,6,mpi_integer4,my_neb_r,mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ias_l=ibuf0(1) ; iae_l=ibuf0(2) ; jas_l=ibuf0(3) ; jae_l=ibuf0(4)
         kas_l=ibuf0(5) ; kae_l=ibuf0(6)
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

      s%iabs_l(n)=max(ias_l,ibs)
      s%iabe_l(n)=min(iae_l,ibe)
      s%jabs_l(n)=max(jas_l,jbs)
      s%jabe_l(n)=min(jae_l,jbe)
      s%kabs_l(n)=max(kas_l,kbs)
      s%kabe_l(n)=min(kae_l,kbe)

! -- Receive from right ------------------
      if(my_neb_r >= 0) then
         call mpi_irecv(ibuf1,6,mpi_integer4,my_neb_r,my_neb_r,mpi_comm_world,irecv_r,irerr)
      end if

! -- Send to left ------------------------
      if(my_neb_l >= 0) then
         ibuf3(1)=s%iabs_l(n) ; ibuf3(2)=s%iabe_l(n) ; ibuf3(3)=s%jabs_l(n) ; ibuf3(4)=s%jabe_l(n)
         ibuf3(5)=s%kabs_l(n) ; ibuf3(6)=s%kabe_l(n)
         call mpi_issend(ibuf3,6,mpi_integer4,my_neb_l,mype,mpi_comm_world,isend_l,iserr)
      end if

! -- Store from right --------------------
      if(my_neb_r >=0) then
         call mpi_wait(irecv_r,istat,ierr)
         s%iabs_rc(n)=ibuf1(1) ; s%iabe_rc(n)=ibuf1(2) ; s%jabs_rc(n)=ibuf1(3) ; s%jabe_rc(n)=ibuf1(4)
         s%kabs_rc(n)=ibuf1(5) ; s%kabe_rc(n)=ibuf1(6)
      end if

! -- Release issend ---------------------
      if(my_neb_l >= 0) then
         call mpi_wait(isend_l,istat,ierr)
      end if

      numy=s%iabe_l(n)+1-s%iabs_l(n)
      numx=s%jabe_l(n)+1-s%jabs_l(n)
      numz=s%kabe_l(n)+1-s%kabs_l(n)
      if( numx <= 0 .or. numy <= 0 .or. numz <= 0 ) then
         s%numl(n)=-1 ; s%lefts(n)=-1
      else
         s%numl(n)=numx*numy*numz
      end if

      numy=s%iabe_rc(n)+1-s%iabs_rc(n)
      numx=s%jabe_rc(n)+1-s%jabs_rc(n)
      numz=s%kabe_rc(n)+1-s%kabs_rc(n)
      if( numx <= 0 .or. numy <= 0 .or. numz <= 0 ) then
         s%numrc(n)=-1 ; s%rights(n)=-1
      else
         s%numrc(n)=numx*numy*numz
      end if

      s%maxbuf=max(s%maxbuf,s%numl(n),s%numrc(n))

   end do

end subroutine genex_create_info3

subroutine genex_create_info4(s,ias ,iae ,jas ,jae ,kas ,kae ,mas ,mae , &
                                ibs ,ibe ,jbs ,jbe ,kbs ,kbe ,mbs ,mbe , &
                                iasm,iaem,jasm,jaem,kasm,kaem,masm,maem, &
                                ibsm,ibem,jbsm,jbem,kbsm,kbem,mbsm,mbem)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex_create_info4
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: setup structure variable for 2-d array rearrangement over processors
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     ias ,iae ,jas ,jae ,kas ,kae ,mas ,mae :  input array dimension ranges
!     ibs ,ibe ,jbs ,jbe ,kbs ,kbe ,mbs ,mbe :  output array dimension ranges
!     iasm,iaem,jasm,jaem,kasm,kaem,masm,maem:  allocated input array dimension ranges
!     ibsm,ibem,jbsm,jbem,kbsm,kbem,mbsm,mbem:  allocated output array dimension ranges
!     s                   :  type(genex_info) structure variable
!
!   output argument list:
!     s                   :  type(genex_info) structure variable
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   integer(i_kind),intent(in   ) :: ias ,iae ,jas ,jae ,kas ,kae ,mas ,mae
   integer(i_kind),intent(in   ) :: ibs ,ibe ,jbs ,jbe ,kbs ,kbe ,mbs ,mbe
   integer(i_kind),intent(in   ) :: iasm,iaem,jasm,jaem,kasm,kaem,masm,maem
   integer(i_kind),intent(in   ) :: ibsm,ibem,jbsm,jbem,kbsm,kbem,mbsm,mbem
   type(genex_info),intent(inout):: s

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,irecv_r,isend_l,isend_r,iserr,irerr,ierr
   integer(i_kind) ibuf0(8),ibuf1(8),ibuf2(8),ibuf3(8)
   integer(i_kind) ias_l,iae_l,jas_l,jae_l,kas_l,kae_l,mas_l,mae_l
   integer(i_kind) numx,numy,numz,numt

   s%npe=npe
   s%mype=mype

   s%ias=ias ; s%iae=iae
   s%jas=jas ; s%jae=jae
   s%kas=kas ; s%kae=kae
   s%mas=mas ; s%mae=mae
   s%ibs=ibs ; s%ibe=ibe
   s%jbs=jbs ; s%jbe=jbe
   s%kbs=kbs ; s%kbe=kbe
   s%mbs=mbs ; s%mbe=mbe

   s%iasm=iasm ; s%iaem=iaem ; s%iaemz=max(iasm,iaem)
   s%jasm=jasm ; s%jaem=jaem ; s%jaemz=max(jasm,jaem)
   s%kasm=kasm ; s%kaem=kaem ; s%kaemz=max(kasm,kaem)
   s%masm=masm ; s%maem=maem ; s%maemz=max(masm,maem)
   s%ibsm=ibsm ; s%ibem=ibem ; s%ibemz=max(ibsm,ibem)
   s%jbsm=jbsm ; s%jbem=jbem ; s%jbemz=max(jbsm,jbem)
   s%kbsm=kbsm ; s%kbem=kbem ; s%kbemz=max(kbsm,kbem)
   s%mbsm=mbsm ; s%mbem=mbem ; s%mbemz=max(mbsm,mbem)

! -- do copy first of intersection of a and b on mype ---------------------
   s%myis=max(ias,ibs) ; s%myie=min(iae,ibe)
   s%myjs=max(jas,jbs) ; s%myje=min(jae,jbe)
   s%myks=max(kas,kbs) ; s%myke=min(kae,kbe)
   s%myms=max(mas,mbs) ; s%myme=min(mae,mbe)
   if(s%lallocated) then
      deallocate(s%lefts,s%rights,s%numl,s%numrc,s%iabs_l,s%iabe_l)
      deallocate(s%jabs_l,s%jabe_l,s%iabs_rc,s%iabe_rc,s%jabs_rc,s%jabe_rc)
      deallocate(s%kabs_l,s%kabe_l,s%kabs_rc,s%kabe_rc)
      deallocate(s%mabs_l,s%mabe_l,s%mabs_rc,s%mabe_rc)
   end if
   s%lallocated=.true.

   allocate(s%lefts(npe-1),s%rights(npe-1))
   do n=1,npe-1
      s%lefts(n)=mype-n
      if(s%lefts(n) < 0) s%lefts(n)=s%lefts(n)+npe   ! later change to modulo
      s%rights(n)=mype+n
      if(s%rights(n) > npe-1) s%rights(n)=s%rights(n)-npe
   end do

   allocate(s%numl(npe-1),s%numrc(npe-1))
   allocate(s%iabs_l(npe-1),s%iabe_l(npe-1))
   allocate(s%iabs_rc(npe-1),s%iabe_rc(npe-1))
   allocate(s%jabs_l(npe-1),s%jabe_l(npe-1))
   allocate(s%jabs_rc(npe-1),s%jabe_rc(npe-1))
   allocate(s%kabs_l(npe-1),s%kabe_l(npe-1))
   allocate(s%kabs_rc(npe-1),s%kabe_rc(npe-1))
   allocate(s%mabs_l(npe-1),s%mabe_l(npe-1))
   allocate(s%mabs_rc(npe-1),s%mabe_rc(npe-1))
   s%maxbuf=0
   do n=1,npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(ibuf0,8,mpi_integer4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ibuf2(1)=ias ; ibuf2(2)=iae ; ibuf2(3)=jas ; ibuf2(4)=jae
         ibuf2(5)=kas ; ibuf2(6)=kae ; ibuf2(7)=mas ; ibuf2(8)=mae
         call mpi_issend(ibuf2,8,mpi_integer4,my_neb_r,mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ias_l=ibuf0(1) ; iae_l=ibuf0(2) ; jas_l=ibuf0(3) ; jae_l=ibuf0(4)
         kas_l=ibuf0(5) ; kae_l=ibuf0(6) ; mas_l=ibuf0(7) ; mae_l=ibuf0(8)
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

      s%iabs_l(n)=max(ias_l,ibs)
      s%iabe_l(n)=min(iae_l,ibe)
      s%jabs_l(n)=max(jas_l,jbs)
      s%jabe_l(n)=min(jae_l,jbe)
      s%kabs_l(n)=max(kas_l,kbs)
      s%kabe_l(n)=min(kae_l,kbe)
      s%mabs_l(n)=max(mas_l,mbs)
      s%mabe_l(n)=min(mae_l,mbe)

! -- Receive from right ------------------
      if(my_neb_r >= 0) then
         call mpi_irecv(ibuf1,8,mpi_integer4,my_neb_r,my_neb_r,mpi_comm_world,irecv_r,irerr)
      end if

! -- Send to left ------------------------
      if(my_neb_l >= 0) then
         ibuf3(1)=s%iabs_l(n) ; ibuf3(2)=s%iabe_l(n) ; ibuf3(3)=s%jabs_l(n) ; ibuf3(4)=s%jabe_l(n)
         ibuf3(5)=s%kabs_l(n) ; ibuf3(6)=s%kabe_l(n) ; ibuf3(7)=s%mabs_l(n) ; ibuf3(8)=s%mabe_l(n)
         call mpi_issend(ibuf3,8,mpi_integer4,my_neb_l,mype,mpi_comm_world,isend_l,iserr)
      end if

! -- Store from right --------------------
      if(my_neb_r >=0) then
         call mpi_wait(irecv_r,istat,ierr)
         s%iabs_rc(n)=ibuf1(1) ; s%iabe_rc(n)=ibuf1(2) ; s%jabs_rc(n)=ibuf1(3) ; s%jabe_rc(n)=ibuf1(4)
         s%kabs_rc(n)=ibuf1(5) ; s%kabe_rc(n)=ibuf1(6) ; s%mabs_rc(n)=ibuf1(7) ; s%mabe_rc(n)=ibuf1(8)
      end if

! -- Release issend ---------------------
      if(my_neb_l >= 0) then
         call mpi_wait(isend_l,istat,ierr)
      end if

      numy=s%iabe_l(n)+1-s%iabs_l(n)
      numx=s%jabe_l(n)+1-s%jabs_l(n)
      numz=s%kabe_l(n)+1-s%kabs_l(n)
      numt=s%mabe_l(n)+1-s%mabs_l(n)
      if( numx <= 0 .or. numy <= 0 .or. numz <= 0 .or. numt <= 0 ) then
         s%numl(n)=-1 ; s%lefts(n)=-1
      else
         s%numl(n)=numx*numy*numz*numt
      end if

      numy=s%iabe_rc(n)+1-s%iabs_rc(n)
      numx=s%jabe_rc(n)+1-s%jabs_rc(n)
      numz=s%kabe_rc(n)+1-s%kabs_rc(n)
      numt=s%mabe_rc(n)+1-s%mabs_rc(n)
      if( numx <= 0 .or. numy <= 0 .or. numz <= 0 .or. numt <= 0 ) then
         s%numrc(n)=-1 ; s%rights(n)=-1
      else
         s%numrc(n)=numx*numy*numz*numt
      end if

      s%maxbuf=max(s%maxbuf,s%numl(n),s%numrc(n))

   end do

end subroutine genex_create_info4

subroutine genex_destroy_info(s)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex_destroy_info
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: deallocate/set pointers to null in genex_info type structure variable
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable (1-4 dimensions)
!
!   output argument list:
!     s                   :  type(genex_info) structure variable
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(inout):: s

   if(associated(s%lefts)) deallocate(s%lefts)
   if(associated(s%rights)) deallocate(s%rights)
   if(associated(s%numl)) deallocate(s%numl)
   if(associated(s%numrc)) deallocate(s%numrc)
   if(associated(s%iabs_l)) deallocate(s%iabs_l)
   if(associated(s%iabe_l)) deallocate(s%iabe_l)
   if(associated(s%jabs_l)) deallocate(s%jabs_l)
   if(associated(s%jabe_l)) deallocate(s%jabe_l)
   if(associated(s%kabs_l)) deallocate(s%kabs_l)
   if(associated(s%kabe_l)) deallocate(s%kabe_l)
   if(associated(s%mabs_l)) deallocate(s%mabs_l)
   if(associated(s%mabe_l)) deallocate(s%mabe_l)
   if(associated(s%iabs_rc)) deallocate(s%iabs_rc)
   if(associated(s%iabe_rc)) deallocate(s%iabe_rc)
   if(associated(s%jabs_rc)) deallocate(s%jabs_rc)
   if(associated(s%jabe_rc)) deallocate(s%jabe_rc)
   if(associated(s%kabs_rc)) deallocate(s%kabs_rc)
   if(associated(s%kabe_rc)) deallocate(s%kabe_rc)
   if(associated(s%mabs_rc)) deallocate(s%mabs_rc)
   if(associated(s%mabe_rc)) deallocate(s%mabe_rc)
   s%lallocated=.false.

end subroutine genex_destroy_info

subroutine genex2_r_single(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex2_r_single
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 2-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable (1-4 dimensions)
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_single), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz)
   real(r_single), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j
   real(r_single), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje ) then
      do j=s%myjs,s%myje
         do i=s%myis,s%myie
            b(i,j)=a(i,j)
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do j=s%jabs_rc(n),s%jabe_rc(n)
            do i=s%iabs_rc(n),s%iabe_rc(n)
               ii=ii+1
               buf2(ii)=a(i,j)
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real4,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do j=s%jabs_l(n),s%jabe_l(n)
            do i=s%iabs_l(n),s%iabe_l(n)
               ii=ii+1
               b(i,j)=buf0(ii)
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex2_r_single

subroutine genex2_r_double(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex2_r_double
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 2-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_double), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz)
   real(r_double), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j
   real(r_double), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje ) then
      do j=s%myjs,s%myje
         do i=s%myis,s%myie
            b(i,j)=a(i,j)
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real8,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do j=s%jabs_rc(n),s%jabe_rc(n)
            do i=s%iabs_rc(n),s%iabe_rc(n)
               ii=ii+1
               buf2(ii)=a(i,j)
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real8,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do j=s%jabs_l(n),s%jabe_l(n)
            do i=s%iabs_l(n),s%iabe_l(n)
               ii=ii+1
               b(i,j)=buf0(ii)
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex2_r_double

subroutine genex3_r_single(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex3_r_single
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 3-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_single), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz,s%kasm:s%kaemz)
   real(r_single), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz,s%kbsm:s%kbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j,k
   real(r_single), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje .and. s%myks <= s%myke ) then
      do k=s%myks,s%myke
         do j=s%myjs,s%myje
            do i=s%myis,s%myie
               b(i,j,k)=a(i,j,k)
            end do
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do k=s%kabs_rc(n),s%kabe_rc(n)
            do j=s%jabs_rc(n),s%jabe_rc(n)
               do i=s%iabs_rc(n),s%iabe_rc(n)
                  ii=ii+1
                  buf2(ii)=a(i,j,k)
               end do
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real4,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do k=s%kabs_l(n),s%kabe_l(n)
            do j=s%jabs_l(n),s%jabe_l(n)
               do i=s%iabs_l(n),s%iabe_l(n)
                  ii=ii+1
                  b(i,j,k)=buf0(ii)
               end do
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex3_r_single

subroutine genex3_r_double(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex3_r_double
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 3-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_double), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz,s%kasm:s%kaemz)
   real(r_double), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz,s%kbsm:s%kbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j,k
   real(r_double), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje .and. s%myks <= s%myke ) then
      do k=s%myks,s%myke
         do j=s%myjs,s%myje
            do i=s%myis,s%myie
               b(i,j,k)=a(i,j,k)
            end do
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real8,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do k=s%kabs_rc(n),s%kabe_rc(n)
            do j=s%jabs_rc(n),s%jabe_rc(n)
               do i=s%iabs_rc(n),s%iabe_rc(n)
                  ii=ii+1
                  buf2(ii)=a(i,j,k)
               end do
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real8,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do k=s%kabs_l(n),s%kabe_l(n)
            do j=s%jabs_l(n),s%jabe_l(n)
               do i=s%iabs_l(n),s%iabe_l(n)
                  ii=ii+1
                  b(i,j,k)=buf0(ii)
               end do
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex3_r_double

subroutine genex4_r_single(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex4_r_single
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 4-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_single), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz,s%kasm:s%kaemz,s%masm:s%maemz)
   real(r_single), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz,s%kbsm:s%kbemz,s%mbsm:s%mbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j,k,m
   real(r_single), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje .and. s%myks <= s%myke .and.  s%myms <= s%myme ) then
      do m=s%myms,s%myme
         do k=s%myks,s%myke
            do j=s%myjs,s%myje
               do i=s%myis,s%myie
                  b(i,j,k,m)=a(i,j,k,m)
               end do
            end do
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real4,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do m=s%mabs_rc(n),s%mabe_rc(n)
            do k=s%kabs_rc(n),s%kabe_rc(n)
               do j=s%jabs_rc(n),s%jabe_rc(n)
                  do i=s%iabs_rc(n),s%iabe_rc(n)
                     ii=ii+1
                     buf2(ii)=a(i,j,k,m)
                  end do
               end do
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real4,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do m=s%mabs_l(n),s%mabe_l(n)
            do k=s%kabs_l(n),s%kabe_l(n)
               do j=s%jabs_l(n),s%jabe_l(n)
                  do i=s%iabs_l(n),s%iabe_l(n)
                     ii=ii+1
                     b(i,j,k,m)=buf0(ii)
                  end do
               end do
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex4_r_single

subroutine genex4_r_double(s,a,b)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genex4_r_double
!   prgmmr: parrish          org: np22                date: 2017-01-03
!
! abstract: rearrange input 4-d distributed arrays "a" to output distributed arrays "b"
!
! program history log:
!   2017-01-03  parrish
!
!   input argument list:
!     s                   :  type(genex_info) structure variable
!     a                   :  input array
!
!   output argument list:
!     b                   :  output array
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   implicit none

   type(genex_info),intent(in):: s
   real(r_double), intent(in   ) :: a(s%iasm:s%iaemz,s%jasm:s%jaemz,s%kasm:s%kaemz,s%masm:s%maemz)
   real(r_double), intent(  out) :: b(s%ibsm:s%ibemz,s%jbsm:s%jbemz,s%kbsm:s%kbemz,s%mbsm:s%mbemz)

   integer(i_kind) istat(mpi_status_size)
   integer(i_kind) n,my_neb_l,my_neb_r
   integer(i_kind) irecv_l,isend_r,iserr,irerr,ierr
   integer(i_kind) i,ii,j,k,m
   real(r_double), allocatable :: buf0(:),buf2(:)

! -- do copy first of intersection of a and b on mype ---------------------
   if( s%myis <= s%myie .and. s%myjs <= s%myje .and. s%myks <= s%myke .and.  s%myms <= s%myme ) then
      do m=s%myms,s%myme
         do k=s%myks,s%myke
            do j=s%myjs,s%myje
               do i=s%myis,s%myie
                  b(i,j,k,m)=a(i,j,k,m)
               end do
            end do
         end do
      end do
   end if

!   now transfer remainder of a to b.

   allocate(buf0(s%maxbuf),buf2(s%maxbuf))
   do n=1,s%npe-1

      my_neb_l = s%lefts(n)
      my_neb_r = s%rights(n)

! -- Receive from left -------------------
      if(my_neb_l >= 0) then
         call mpi_irecv(buf0,s%numl(n),mpi_real8,my_neb_l,my_neb_l,mpi_comm_world,irecv_l,irerr)
      end if

! -- Send to right -----------------------
      if(my_neb_r >= 0) then
         ii=0
         do m=s%mabs_rc(n),s%mabe_rc(n)
            do k=s%kabs_rc(n),s%kabe_rc(n)
               do j=s%jabs_rc(n),s%jabe_rc(n)
                  do i=s%iabs_rc(n),s%iabe_rc(n)
                     ii=ii+1
                     buf2(ii)=a(i,j,k,m)
                  end do
               end do
            end do
         end do
         call mpi_issend(buf2,s%numrc(n),mpi_real8,my_neb_r,s%mype,mpi_comm_world,isend_r,iserr)
      end if

! -- Store from left ---------------------
      if(my_neb_l >=0) then
         call mpi_wait(irecv_l,istat,ierr)
         ii=0
         do m=s%mabs_l(n),s%mabe_l(n)
            do k=s%kabs_l(n),s%kabe_l(n)
               do j=s%jabs_l(n),s%jabe_l(n)
                  do i=s%iabs_l(n),s%iabe_l(n)
                     ii=ii+1
                     b(i,j,k,m)=buf0(ii)
                  end do
               end do
            end do
         end do
      end if

! -- Release issend ---------------------
      if(my_neb_r >= 0) then
         call mpi_wait(isend_r,istat,ierr)
      end if

   end do

   deallocate(buf0,buf2)

end subroutine genex4_r_double

end module genex_mod
