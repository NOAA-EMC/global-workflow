module egrid2agrid_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    egrid2agrid_mod module to move between analysis and ensemble grid
!   prgmmr: parrish          org: np22                date: 2010-02-05
!
! abstract: Adaptation of module fgrid2agrid_mod.f90.  Here both ensemble grid
!             and analysis grid dimensions and coordinates are input, so that general
!             grid spacing is allowed.  Also, it is not necessary for the grids
!             to fully overlap.
!
! program history log:
!   2010-02-05  parrish, initial documentation
!   2010-03-04  parrish - add ability to interpolate from full global grid to general collecition of points.
!   2010-04-06  parrish - in subroutine get_3ops, correct for possible divide by zero when obtaining
!                          normalized adjoint interpolation weights, used for smoothing interpolation
!                          from fine to coarse grid.
!   2011-07-26  todling  - generalize single/double prec and rank interface
!   2012-02-08  parrish  - add single/double prec and rank interface for routines egrid2agrid,
!                            egrid2agrid_ad, and agrid2egrid, to be used for regional dual resolution.
!
! subroutines included:
!   sub init_egrid2agrid         - initialize interpolation variables and constants to defaults
!   sub create_egrid2agrid       - compute all necessary interpolation weights from input coordinate info
!   sub get_3ops                 - called by create_egrid2agrid--compute interpolation operators
!   sub destroy_egrid2agrid      - free space used by interpolation constants
!   sub egrid2agrid              - interpolate from ensemble grid to analysis grid
!   sub egrid2agrid_ad           - adjoint of egrid2agrid
!   sub agrid2egrid              - full weight interpolate from analysis grid to ensemble grid
!                                   (same code as egrid2agrid_ad, but weights normalized to 1)
!   sub create_egrid2points_slow - similar to create_egrid2agrid, but for general collection of points
!   sub egrid2points             - interpolate to general set of points.
!   sub g_create_egrid2agrid     - used for full global lat-lon egrid and agrid
!   sub g_egrid2agrid            - for full global lat-lon egrid and agrid,
!                                   interpolate from ensemble grid to analysis grid
!   sub g_egrid2agrid_ad         - adjoint of g_egrid2agrid
!   sub g_agrid2egrid            - full weight interpolate from analysis grid to ensemble grid
!                                   (same code as g_egrid2agrid_ad, but weights normalized to 1)
!   sub g_create_egrid2points    - used for full global lat-lon egrid to general collection of points
!   sub g_egrid2points           - for full global lat-lon egrid, interpolate to general set of points.
!
! Variable Definitions:
!   def nord_e2a       - order of interpolation
!
!   def nlate          - number of lats on ensemble grid
!   def nlone          - number of lons on ensemble grid
!   def nlata          - number of lats on analysis grid
!   def nlona          - number of lons on analysis grid
!   def nextend        - number of extra rows added internally around ensemble grid
!                         for full global case (only used by routines with g_ prefix)
!   def identity       - if true, then ensemble grid is same as analysis grid,
!                         so no interpolation required
!   def e2a_lon        - structure variable containing interpolation info in longitude
!                         direction between filter and analysis grids
!   def e2a_lat        - structure variable containing interpolation info in latitude
!                         direction between filter and analysis grids
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind,r_double,r_single

   implicit none

! set default to private
   private
! set subroutines to public
   public :: create_egrid2agrid
   public :: get_3ops
   public :: destroy_egrid2agrid
   public :: egrid2agrid
   public :: egrid2agrid_ad
   public :: agrid2egrid
   public :: create_egrid2points_slow
   public :: egrid2points
   public :: g_create_egrid2agrid
   public :: g_egrid2agrid
   public :: g_egrid2agrid_ad
   public :: g_agrid2egrid
   public :: g_create_egrid2points_slow
   public :: g_egrid2points_faster
! set passed variables to public
   public :: egrid2agrid_parm
   public :: egrid2agrid_cons

   type egrid2agrid_cons

      integer(i_kind) ngride
      integer(i_kind) ngrida
      integer(i_kind) mgride
      integer(i_kind) mgrida
      integer(i_kind),pointer::iwin(:,:) => NULL()
      integer(i_kind),pointer::nwin(:) => NULL()
      integer(i_kind),pointer::itwin(:,:) => NULL()
      integer(i_kind),pointer::ntwin(:) => NULL()
      real(r_kind),pointer::win(:,:) => NULL()
      real(r_kind),pointer::twin(:,:) => NULL()
      real(r_kind),pointer::swin(:,:) => NULL()
      logical:: lallocated = .false.

   end type egrid2agrid_cons

   type egrid2agrid_parm
      integer(i_kind):: nlata,nlona,nlate,nlone,nlate_ex,nlone_ex,nextend
      integer(i_kind):: nlone_half
      logical:: identity
      real(r_kind),pointer::blend(:) => NULL()
      real(r_kind),pointer::xa_e(:) => NULL()
      real(r_kind),pointer::ya_e(:) => NULL()
      logical:: lallocated = .false.
      type(egrid2agrid_cons):: e2a_lon,e2a_lat
   end type egrid2agrid_parm

   interface egrid2agrid
      module procedure egrid2agrid_r4
      module procedure egrid2agrid_r8
      module procedure egrid2agrid_r4_rank4
      module procedure egrid2agrid_r8_rank4
   end interface

   interface egrid2agrid_ad
      module procedure egrid2agrid_r4_ad
      module procedure egrid2agrid_r8_ad
      module procedure egrid2agrid_r4_rank4_ad
      module procedure egrid2agrid_r8_rank4_ad
   end interface 

   interface agrid2egrid
      module procedure agrid2egrid_r4
      module procedure agrid2egrid_r8
      module procedure agrid2egrid_r4_rank4
      module procedure agrid2egrid_r8_rank4
   end interface

   interface g_egrid2agrid
      module procedure g_egrid2agrid_r4
      module procedure g_egrid2agrid_r8
      module procedure g_egrid2agrid_r4_rank4
      module procedure g_egrid2agrid_r8_rank4
   end interface

   interface g_egrid2agrid_ad
      module procedure g_egrid2agrid_r4_ad
      module procedure g_egrid2agrid_r8_ad
      module procedure g_egrid2agrid_r4_rank4_ad
      module procedure g_egrid2agrid_r8_rank4_ad
   end interface 

   interface g_agrid2egrid
      module procedure g_agrid2egrid_r4
      module procedure g_agrid2egrid_r8
      module procedure g_agrid2egrid_r4_rank4
      module procedure g_agrid2egrid_r8_rank4
   end interface

   contains


   subroutine create_egrid2agrid(nlata,rlata,nlona,rlona,nlate,rlate,nlone,rlone,nord_e2a,p)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_egrid2agrid  create interpolation variables
!   prgmmr: parrish          org: np22                date: 2010-01-05
!
! abstract: given coordinates and dimensions of analysis and ensemble grids, obtain
!             interpolation weights and indices for ensemble to analysis, 
!             adjoint of ensemble to analysis,
!             and smoothing interpolation analysis to ensemble.  all information is
!             output in structure variable p.
!
!
! program history log:
!   2010-02-05  parrish, initial documentation
!
!   input argument list:
!     nlata,rlata:  number and value of analysis latitudes
!     nlona,rlona:  number and value of analysis longitudes
!     nlate,rlate:  number and value of ensemble latitudes
!     nlone,rlone:  number and value of ensemble longitudes
!     nord_e2a:     order of interpolation from ensemble to analysis grid
!
!   output argument list:
!     p
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero,one
      implicit none

      integer(i_kind),intent(in) :: nlata,nlona,nlate,nlone,nord_e2a
      real(r_kind),intent(in) :: rlata(nlata),rlona(nlona),rlate(nlate),rlone(nlone) 
      type(egrid2agrid_parm),intent(inout) :: p

      integer(i_kind) i
      real(r_kind) diffmax,range_lat,range_lon
      logical e2a_only
 
      p%nlata=nlata
      p%nlona=nlona
      p%nlate=nlate
      p%nlone=nlone
      p%nlate_ex=nlate
      p%nlone_ex=nlone
      p%identity=.false.
      if(nlata == nlate.and.nlona == nlone) then
         range_lat=max(abs(rlata(nlata)-rlata(1)),abs(rlate(nlate)-rlate(1)))
         if(nlata == 1) range_lat=one
         range_lon=max(abs(rlona(nlona)-rlona(1)),abs(rlone(nlone)-rlone(1)))
         if(nlona == 1) range_lon=one
         diffmax=zero
         do i=1,nlata
            diffmax=max(diffmax,abs(rlata(i)-rlate(i))/range_lat)
         end do
         do i=1,nlona
            diffmax=max(diffmax,abs(rlona(i)-rlone(i))/range_lon)
         end do
         if(diffmax < .0000001_r_kind) p%identity=.true.
      end if

      if(.not.p%identity) then

         e2a_only=.false.
         call get_3ops(p%e2a_lon,nlona,rlona,nlone,rlone,nord_e2a,e2a_only)
         call get_3ops(p%e2a_lat,nlata,rlata,nlate,rlate,nord_e2a,e2a_only)

      end if

   end subroutine create_egrid2agrid

   subroutine get_3ops(e2a,ngrida,rgrida,ngride,rgride,iord,e2a_only)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_3ops          compute interpolation operators
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: obtain one-dimensional interpolation operators.
!
!
! program history log:
!   2010-02-06  parrish, initial documentation
!   2010-03-09  parrish - add optional logical variable e2a_only, which if true, only compute 
!                          egrid to agrid interpolation operator.
!   2010-04-06  parrish - correct for possible divide by zero when obtaining normalized adjoint
!                          interpolation weights, used for smoothing interpolation from fine to coarse grid.
!   2010-07-28  parrish - fix bug introduced by above correction.  forgot to remove previous
!                           multiply by workc.
!   2010-07-28  parrish - make array hbig allocatable.  segfault sometimes occurs upon exit from
!                           this routine when optional logical variable e2a_only is present and true.
!                           Even though hbig is not used in this case, it can be very large (it was
!                           4.7Gb for a high res nems-nmmb window run by Ed Colon, which seg-faulted).
!
!   input argument list:
!     e2a           - structure variable with previous/default interpolation information
!     ngrida        - number of grid points on analysis grid for direction being considered.
!     rgrida        - analysis grid coordinates for direction being considered
!     ngride        - number of grid points on ensemble grid for direction being considered.
!     rgride        - ensemble grid coordinates for direction being considered
!     iord          - order of interpolation
!
!   output argument list:
!     e2a           - structure variable with new interpolation information
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

!     obtain 3 types of interpolation/smoothing operators:

!         they are:

!             win, iwin:   interpolation weights, addresses for ensemble grid --> analysis grid
!            twin, itwin:  adjoint of interpolation weights, addresses, analysis grid --> ensemble grid
!            swin, iswin:  smoothing interpolation weights, addresses for analysis grid --> ensemble grid
!
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_cons),intent(inout) :: e2a
      integer(i_kind)       ,intent(in   ) :: iord,ngrida,ngride
      real(r_kind)          ,intent(in   ) :: rgrida(ngrida),rgride(ngride)
      logical,optional      ,intent(in   ) :: e2a_only

      integer(i_kind) i,ii,ipmaxmax,ipminmin,j,jord,k,lbig,n,ntwinmax
      integer(i_kind) ixi(0:iord)
      real(r_kind) tl(iord+1,iord+1,2*ngride),alocal(2*ngride),blocal(2*ngride),wgts(ngrida,iord+1)
      integer(i_kind) iwgts(ngrida,iord+1),iflag(ngrida)
      real(r_kind) workc(ngride)
      real(r_kind),allocatable::hbig(:,:)
      integer(i_kind) ipmax(ngride),ipmin(ngride)
!
!    actually, there are 4 types of operations, only 3 of which get saved.  We start by getting
!     basic interpolation


!--------------------------------------------------------
!-----get interpolation weights, indices for interpolation from coarse grid to fine grid
!--------------------------------------------------------

      if(e2a%lallocated) then
         deallocate(e2a%iwin,e2a%nwin,e2a%itwin,e2a%ntwin)
         deallocate(e2a%win,e2a%twin,e2a%swin)
      end if

      e2a%ngride=ngride
      e2a%ngrida=ngrida
      e2a%mgride=iord+1
      allocate(e2a%iwin(iord+1,ngrida))
      allocate(e2a%nwin(ngrida))
      allocate(e2a%win(iord+1,ngrida))
      e2a%iwin=1
      e2a%nwin=0
      e2a%win=zero

      do jord=1,iord
         lbig=jord+1
         call simpin1_init(ixi,tl,alocal,blocal,jord,lbig,rgride,ngride)
         call simpin1(wgts,wgts,wgts,iwgts,iflag,rgrida,ngrida,jord,lbig, &
                      rgride,ngride,1,0,0,ixi,tl,alocal,blocal)
         do i=1,ngrida
            if(iflag(i)==1) then
               e2a%nwin(i)=lbig
               do k=1,lbig
                  e2a%win(k,i)=wgts(i,k)
                  e2a%iwin(k,i)=iwgts(i,k)
               end do
            end if
         end do
      end do

      if(present(e2a_only)) then
         if(e2a_only) then
            allocate(e2a%twin(1,1),e2a%itwin(1,1),e2a%ntwin(1),e2a%swin(1,1))
            e2a%lallocated=.true.
            return
         end if
      end if
            

!--------------------------------------------------------
!   next get adjoint weights and addresses by brute force
!--------------------------------------------------------

      ipminmin=ngrida+1
      ipmaxmax=0
      ipmin=1
      ipmax=-1
      allocate(hbig(ngrida,ngride))
      do j=1,ngride
         do k=1,ngride
            workc(k)=zero
         end do
         workc(j)=one
         do i=1,ngrida
            hbig(i,j)=zero
            do n=1,e2a%nwin(i)
               hbig(i,j)=hbig(i,j)+e2a%win(n,i)*workc(e2a%iwin(n,i))
            end do
         end do
         do i=1,ngrida
            if(hbig(i,j)/=zero) then
               ipmin(j)=i
               ipminmin=min(ipmin(j),ipminmin)
               exit
            end if
         end do
         do i=ngrida,1,-1
            if(hbig(i,j)/=zero) then
               ipmax(j)=i
               ipmaxmax=max(ipmax(j),ipmaxmax)
               exit
            end if
         end do
      end do
      ntwinmax=ipmaxmax-ipminmin+1
      e2a%mgrida=ntwinmax
      allocate(e2a%twin(ntwinmax,ngride),e2a%itwin(ntwinmax,ngride),e2a%ntwin(ngride))
      e2a%itwin=1
      e2a%twin=zero
      do j=1,ngride
         e2a%ntwin(j)=max(0,ipmax(j)-ipmin(j)+1)
         ii=0
         do i=ipmin(j),ipmax(j)
            ii=ii+1
            e2a%itwin(ii,j)=i
            e2a%twin(ii,j)=hbig(i,j)
         end do
      end do
      deallocate(hbig)

!--------------------------------------------------------
!   next get smoothing interpolation from fine to coarse
!--------------------------------------------------------

      allocate(e2a%swin(ntwinmax,ngride))
      e2a%swin=zero
      do j=1,ngride
         workc(j)=zero
         do i=1,e2a%ntwin(j)
            workc(j)=workc(j)+e2a%twin(i,j)
         end do
         do i=1,e2a%ntwin(j)
            e2a%swin(i,j)=e2a%twin(i,j)/workc(j)
         end do
      end do

      e2a%lallocated=.true.

   end subroutine get_3ops

   subroutine destroy_egrid2agrid(p)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_egrid2agrid   release space used by egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: release space used by egrid2agrid.
!
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p --- parameters for egrid2agrid
!
!   output argument list:
!     p --- parameters for egrid2agrid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      implicit none

      type(egrid2agrid_parm),intent(inout) :: p

      if(p%e2a_lon%lallocated) then
         deallocate(p%e2a_lon%iwin,p%e2a_lon%nwin,p%e2a_lon%itwin)
         deallocate(p%e2a_lon%ntwin)
         deallocate(p%e2a_lon%win,p%e2a_lon%twin,p%e2a_lon%swin)
         p%e2a_lon%lallocated=.false.
      end if
      if(p%e2a_lat%lallocated) then
         deallocate(p%e2a_lat%iwin,p%e2a_lat%nwin,p%e2a_lat%itwin)
         deallocate(p%e2a_lat%ntwin)
         deallocate(p%e2a_lat%win,p%e2a_lat%twin,p%e2a_lat%swin)
         p%e2a_lat%lallocated=.false.
      end if

   end subroutine destroy_egrid2agrid

   subroutine egrid2agrid_r4_rank4(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r4_rank4   rank-4 interface to egrid2agrid_r4
!   prgmmr: parrish          org: np22                date: 2011-11-23
!
! abstract: interpolate from ensemble to analysis grid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(in   ) :: e(:,:,:,:)
      real(r_single)          ,intent(  out) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call egrid2agrid_r4(p,e(i,:,:,:),a(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' egrid2agrid_r4_rank4: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif

   end subroutine egrid2agrid_r4_rank4

   subroutine egrid2agrid_r4(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid   interpolate from ensemble to analysis grid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: interpolate from ensemble to analysis grid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(in   ) :: e(p%nlate,p%nlone,kb:ke)
      real(r_single)          ,intent(  out) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  a(i,j,kk)=e(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlata
                  w(i,j)=zero
                  do k=1,p%e2a_lat%nwin(i)
                     w(i,j)=w(i,j)+p%e2a_lat%win(k,i)*e(p%e2a_lat%iwin(k,i),j,kk)
                  end do
               end do
            end do
            do j=1,p%nlona
               do i=1,p%nlata
                  a(i,j,kk)=zero
               end do
               do k=1,p%e2a_lon%nwin(j)
                  j1=p%e2a_lon%iwin(k,j)
                  w1=p%e2a_lon%win(k,j)
                  do i=1,p%nlata
                     a(i,j,kk)=a(i,j,kk)+w1*w(i,j1)
                  end do
               end do
            end do
         end do
      end if

   end subroutine egrid2agrid_r4

   subroutine egrid2agrid_r8_rank4(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r8_rank4   rank-4 interface to egrid2agrid_r8
!   prgmmr: parrish          org: np22                date: 2011-11-23
!
! abstract: interpolate from ensemble to analysis grid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)          ,intent(in   ) :: e(:,:,:,:)
      real(r_double)          ,intent(  out) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call egrid2agrid_r8(p,e(i,:,:,:),a(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' egrid2agrid_r8_rank4: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif

   end subroutine egrid2agrid_r8_rank4

   subroutine egrid2agrid_r8(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid   interpolate from ensemble to analysis grid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: interpolate from ensemble to analysis grid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(in   ) :: e(p%nlate,p%nlone,kb:ke)
      real(r_double)        ,intent(  out) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  a(i,j,kk)=e(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlata
                  w(i,j)=zero
                  do k=1,p%e2a_lat%nwin(i)
                     w(i,j)=w(i,j)+p%e2a_lat%win(k,i)*e(p%e2a_lat%iwin(k,i),j,kk)
                  end do
               end do
            end do
            do j=1,p%nlona
               do i=1,p%nlata
                  a(i,j,kk)=zero
               end do
               do k=1,p%e2a_lon%nwin(j)
                  j1=p%e2a_lon%iwin(k,j)
                  w1=p%e2a_lon%win(k,j)
                  do i=1,p%nlata
                     a(i,j,kk)=a(i,j,kk)+w1*w(i,j1)
                  end do
               end do
            end do
         end do
      end if

   end subroutine egrid2agrid_r8

   subroutine egrid2agrid_r4_rank4_ad(p,a,e,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r4_ad_rank4  rank-4 interface to egrid2agrid_r4
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: adjoint of egrid2agrid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(  out) :: e(:,:,:,:)
      real(r_single)          ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call egrid2agrid_r4_ad(p,e(i,:,:,:),a(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' egrid2agrid_r4_rank4_ad: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine egrid2agrid_r4_rank4_ad

   subroutine egrid2agrid_r4_ad(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r4_ad  adjoint of egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: adjoint of egrid2agrid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      real(r_single)          ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlata
                  w(i,j)=zero
               end do
            end do
            do j=1,p%nlona
               do k=1,p%e2a_lon%nwin(j)
                  j1=p%e2a_lon%iwin(k,j)
                  w1=p%e2a_lon%win(k,j)
                  do i=1,p%nlata
                     w(i,j1)=w(i,j1)+w1*a(i,j,kk)
                  end do
               end do
            end do
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=zero
               end do
               do i=1,p%nlata
                  do k=1,p%e2a_lat%nwin(i)
                     e(p%e2a_lat%iwin(k,i),j,kk)=e(p%e2a_lat%iwin(k,i),j,kk)+p%e2a_lat%win(k,i)*w(i,j)
                  end do
               end do
            end do
         end do
      end if

   end subroutine egrid2agrid_r4_ad

   subroutine egrid2agrid_r8_rank4_ad(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r8_rank4_ad  rank-4 interface to egrid2agrid_r8
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: adjoint of egrid2agrid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)          ,intent(  out) :: e(:,:,:,:)
      real(r_double)          ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call egrid2agrid_r8_ad(p,e(i,:,:,:),a(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' egrid2agrid_r8_rank4_ad: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine egrid2agrid_r8_rank4_ad

   subroutine egrid2agrid_r8_ad(p,e,a,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2agrid_r8_ad  adjoint of egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: adjoint of egrid2agrid
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e             - filter grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)          ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      real(r_double)          ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlata
                  w(i,j)=zero
               end do
            end do
            do j=1,p%nlona
               do k=1,p%e2a_lon%nwin(j)
                  j1=p%e2a_lon%iwin(k,j)
                  w1=p%e2a_lon%win(k,j)
                  do i=1,p%nlata
                     w(i,j1)=w(i,j1)+w1*a(i,j,kk)
                  end do
               end do
            end do
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=zero
               end do
               do i=1,p%nlata
                  do k=1,p%e2a_lat%nwin(i)
                     e(p%e2a_lat%iwin(k,i),j,kk)=e(p%e2a_lat%iwin(k,i),j,kk)+p%e2a_lat%win(k,i)*w(i,j)
                  end do
               end do
            end do
         end do
      end if

   end subroutine egrid2agrid_r8_ad

   subroutine agrid2egrid_r4_rank4(p,a,e,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    agrid2egrid_r4_rank4   rank-4 interface to agrid2egrid_r4
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: interpolation from a grid to e grid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(  out) :: e(:,:,:,:)
      real(r_single)        ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call agrid2egrid_r4(p,a(i,:,:,:),e(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' agrid2egrid_r4_rank4: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine agrid2egrid_r4_rank4

   subroutine agrid2egrid_r4(p,a,e,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    agrid2egrid  smoothing interpolate from agrid to egrid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: interpolate from agrid to egrid
!
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      real(r_single)          ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               j1=p%e2a_lon%itwin(1,j)
               w1=p%e2a_lon%swin(1,j)
               do i=1,p%nlata
                  w(i,j)=w1*a(i,j1,kk)
               end do
               do k=2,p%e2a_lon%ntwin(j)
                  j1=p%e2a_lon%itwin(k,j)
                  w1=p%e2a_lon%swin(k,j)
                  do i=1,p%nlata
                     w(i,j)=w(i,j)+w1*a(i,j1,kk)
                  end do
               end do
            end do
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=p%e2a_lat%swin(1,i)*w(p%e2a_lat%itwin(1,i),j)
                  do k=2,p%e2a_lat%ntwin(i)
                     e(i,j,kk)=e(i,j,kk)+p%e2a_lat%swin(k,i)*w(p%e2a_lat%itwin(k,i),j)
                  end do
               end do
            end do
         end do
      end if

   end subroutine agrid2egrid_r4

   subroutine agrid2egrid_r8_rank4(p,a,e,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    agrid2egrid_r8_rank4   rank-4 interface to agrid2egrid_r8
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: interpolation from a grid to e grid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(  out) :: e(:,:,:,:)
      real(r_double)        ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call agrid2egrid_r8(p,a(i,:,:,:),e(i,:,:,:),kb,ke)
         enddo
      else
         write(6,*)' agrid2egrid_r8_rank4: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine agrid2egrid_r8_rank4

   subroutine agrid2egrid_r8(p,a,e,kb,ke)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    agrid2egrid  smoothing interpolate from agrid to egrid
!   prgmmr: parrish          org: np22                date: 2010-02-06
!
! abstract: interpolate from agrid to egrid
!
!
! program history log:
!   2010-02-06  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)          ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      real(r_double)          ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,k,kk
      real(r_kind) w1,w(p%nlata,p%nlone)

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else
         do kk=kb,ke
            do j=1,p%nlone
               j1=p%e2a_lon%itwin(1,j)
               w1=p%e2a_lon%swin(1,j)
               do i=1,p%nlata
                  w(i,j)=w1*a(i,j1,kk)
               end do
               do k=2,p%e2a_lon%ntwin(j)
                  j1=p%e2a_lon%itwin(k,j)
                  w1=p%e2a_lon%swin(k,j)
                  do i=1,p%nlata
                     w(i,j)=w(i,j)+w1*a(i,j1,kk)
                  end do
               end do
            end do
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=p%e2a_lat%swin(1,i)*w(p%e2a_lat%itwin(1,i),j)
                  do k=2,p%e2a_lat%ntwin(i)
                     e(i,j,kk)=e(i,j,kk)+p%e2a_lat%swin(k,i)*w(p%e2a_lat%itwin(k,i),j)
                  end do
               end do
            end do
         end do
      end if

   end subroutine agrid2egrid_r8

   subroutine create_egrid2points_slow(np,yp,xp,nye,ye,nxe,xe,nord_e2a,p,nord_blend,nmix)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_egrid2points_slow 1st version of grid to points
!   prgmmr: parrish          org: np22                date: 2010-10-29
!
! abstract: setup routine for egrid2points_slow, inefficient version of interpolation
!             from regional grid to a general set of points.
!
!
! program history log:
!   2010-10-29  parrish, initial documentation
!   2012-11-28  tong - added p%lallocated=.true. after arrays of p are allocated
!
!   input argument list:
!     np:     number of points to interpolate to
!     yp:     y (latitude) points in ye grid units
!     xp:     x (longitude) points in xe grid units
!     nye:    number of y (lat) points for egrid
!     ye:     y points in grid units (1 to nye)
!     nxe:    number of x (lon) points for egrid
!     xe:     x points in grid units (1 to nxe)
!     nord_e2a:     order of interpolation from egrid to points
!     nord_blend:  optional variable used to create variable blend mask
!     nmix:        optional variable defining width of blend zone
!
!   output argument list:
!     p
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero,half,one,two
      use blendmod, only: blend
      implicit none

      integer(i_kind),intent(in) :: np,nye,nxe,nord_e2a
      real(r_kind),intent(in) :: yp(np),xp(np),ye(nye),xe(nxe)
      type(egrid2agrid_parm),intent(inout) :: p
      integer(i_kind),optional,intent(in) :: nord_blend,nmix

      integer(i_kind) i,j
      logical e2a_only
      integer(i_kind),dimension(0:40):: iblend
      integer(i_kind) mm
      real(r_kind) dxx,x,y
      real(r_kind),allocatable::blendx(:),wgt_xe(:),wgt_ye(:),wgt_e(:,:),wgt_a(:)

      p%nlata=np
      p%nlona=-np
      p%nlate=nye
      p%nlone=nxe
      p%nlate_ex=nye
      p%nlone_ex=nxe
      p%identity=.false.


      e2a_only=.true.
      call get_3ops(p%e2a_lat,np,yp,nye,ye,nord_e2a,e2a_only)
      call get_3ops(p%e2a_lon,np,xp,nxe,xe,nord_e2a,e2a_only)

!       create blend mask:
      if(p%lallocated) then
         deallocate(p%blend,p%ya_e,p%xa_e)
         p%lallocated=.false.
      end if
      allocate(p%blend(np),p%ya_e(np),p%xa_e(np))
      p%blend=one
      p%ya_e=yp
      p%xa_e=xp
      if(present(nord_blend).and.present(nmix)) then

!     set up blend function

         mm=nord_blend
         call blend(mm,iblend)
         allocate(blendx(nmix))
         blendx(nmix)=one
         dxx=one/nmix
         blendx(1)=zero
         do i=2,nmix
            x=(i-one)*dxx
            y=iblend(mm)
            do j=mm-1,0,-1
               y=x*y+iblend(j)
            end do
            y=y*x**(mm+1)
            blendx(i)=y
         end do
         allocate(wgt_xe(nxe),wgt_ye(nye))
         wgt_xe=one ; wgt_ye=one
         do j=1,nmix
            wgt_xe(j)=blendx(j)
            wgt_xe(nxe+1-j)=blendx(j)
            wgt_ye(j)=blendx(j)
            wgt_ye(nye+1-j)=blendx(j)
         end do
         allocate(wgt_e(nye,nxe),wgt_a(np))
         do j=1,nxe
            do i=1,nye
               wgt_e(i,j)=wgt_xe(j)*wgt_ye(i)
            end do
         end do

!           interpolate wgt_e to a-grid
         call egrid2points(p,wgt_e,wgt_a)
         do i=1,np
            p%blend(i)=wgt_a(i)
         end do
         deallocate(wgt_e,wgt_a,wgt_xe,wgt_ye,blendx)

      end if

      p%lallocated=.true.

   end subroutine create_egrid2points_slow

   subroutine egrid2points(p,e,a)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    egrid2points interp from grid to set of points
!   prgmmr: parrish          org: np22                date: 2010-10-29
!
! abstract: interpolate from grid to general set of points (faster version)
!
! program history log:
!   2010-10-29  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid
!
!   output argument list:
!     a              - interpolated values on general set of points
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_kind)          ,intent(in   ) :: e(p%nlate,p%nlone)
      real(r_kind)          ,intent(  out) :: a(p%nlata)

      real(r_kind) w(p%nlone)                  !  this array is too big by nlone/(nord_e2a+1)
                                               !   which is why this is the slow version.
      integer(i_kind) i,j,j1,k
      real(r_kind) w1

!       for each point, first interpolate in latitude at longitudes required for longitude interpolation,
!         then finish up with longitude interpolation

      do i=1,p%nlata
         a(i)=zero
         do j=1,p%e2a_lon%nwin(i)
            j1=p%e2a_lon%iwin(j,i)
            w1=p%e2a_lon%win(j,i)
            w(j1)=zero
            do k=1,p%e2a_lat%nwin(i)
               w(j1)=w(j1)+p%e2a_lat%win(k,i)*e(p%e2a_lat%iwin(k,i),j1)
            end do
            a(i)=a(i)+w1*w(j1)
         end do
      end do

   end subroutine egrid2points

   subroutine g_create_egrid2agrid(nlata,rlata,nlona,rlona,nlate,rlate,nlone,rlone,nord_e2a, &
            p,e2a_only,eqspace)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_create_egrid2agrid  create interpolation variables for full global grids
!   prgmmr: parrish          org: np22                date: 2010-01-05
!
! abstract: given coordinates and dimensions of analysis and ensemble grids, obtain
!             interpolation weights and indices for ensemble to analysis,
!             adjoint of ensemble to analysis,
!             and smoothing interpolation analysis to ensemble.  all information is
!             output in structure variable p.  this routine is special for full global grids
!             which include north and south pole point, common to both ensemble and analysis grid,
!             and starting longitude at 0.  also, coordinates are in radians.
!
!
! program history log:
!   2010-02-09  parrish, initial documentation
!   2011-09-14  todling, add eqspace to handle equal-distance grid
!
!   input argument list:
!     nlata:  number of analysis latitudes
!     rlata:  analysis latitudes in radians from -pi/2 to pi/2, including pole points
!     nlona:  number of analysis longitudes (must be even)
!     rlona:  analysis longitudes in radians from 0 to 360, not including point at 360
!     nlate:  number of ensemble latitudes
!     rlate:  ensemble latitudes in radians from -pi/2 to pi/2, including pole points
!     nlone:  number of ensemble longitudes (must be even)
!     rlone:  ensemble longitudes in radians from 0 to 360, not including point at 360
!     nord_e2a:     order of interpolation from ensemble to analysis grid
!
!   output argument list:
!     p
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero,half,one,two,pi
      use mpimod, only: mype
      implicit none

      integer(i_kind),intent(in) :: nlata,nlona,nlate,nlone,nord_e2a
      real(r_kind),intent(in) :: rlata(nlata),rlona(nlona),rlate(nlate),rlone(nlone)
      type(egrid2agrid_parm),intent(inout) :: p
      logical,intent(in):: e2a_only
      logical,intent(in),optional:: eqspace

      integer(i_kind) i,ilona,ilone,j,nextend,nlate_ex,nlone_ex,nlone_half
      real(r_single) errtest
      real(r_kind) half_pi,two_pi,dlona,dlone,diffmax,range_lat,range_lon
      real(r_kind),allocatable::rlate_ex(:),rlone_ex(:)
      logical fail_tests

!   first check to see if grids are the same, in which case no interpolation required
      p%nlata=nlata
      p%nlona=nlona
      p%nlate=nlate
      p%nlone=nlone
      p%nlate_ex=nlate
      p%nlone_ex=nlone
      p%identity=.false.
      if(nlata == nlate.and.nlona == nlone) then
         if(present(eqspace)) then
            if(eqspace) then
               if(mype==0) then
                  write(6,*) 'g_create_egrid2agrid: WARNING, forced p%identity true '
               endif
               p%identity=.true.
             endif
         endif
         range_lat=max(abs(rlata(nlata)-rlata(1)),abs(rlate(nlate)-rlate(1)))
         if(nlata == 1) range_lat=one
         range_lon=max(abs(rlona(nlona)-rlona(1)),abs(rlone(nlone)-rlone(1)))
         if(nlona == 1) range_lon=one
         diffmax=zero
         do i=1,nlata
            diffmax=max(diffmax,abs(rlata(i)-rlate(i))/range_lat)
         end do
         do i=1,nlona
            diffmax=max(diffmax,abs(rlona(i)-rlone(i))/range_lon)
         end do
         if(diffmax < .0000001_r_kind) p%identity=.true.
      end if

      if(p%identity) return

!   check that lats and lons satisfy requirements:

      errtest=10._r_single*pi*epsilon(1._r_single)
      half_pi=half*pi
      two_pi=two*pi
      fail_tests=.false.

!       analysis grid tests:
      if(abs(rlata(1)+half_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlata(1) not within tolerance for south pole value',rlata(1)
         fail_tests=.true.
      end if
      if(abs(rlata(nlata)-half_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlata(nlata) not within tolerance for north pole value',rlata(nlata) 
         fail_tests=.true.
      end if
      if(abs(rlona(1)) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlona(1) not within tolerance for 0 meridian',rlona(1) 
         fail_tests=.true.
      end if
      dlona=rlona(2)-rlona(1)
      ilona=0
      do j=1,nlona-1
         if(abs(rlona(j+1)-rlona(j)-dlona) > errtest) then
            fail_tests=.true.
            ilona=ilona+1
         end if
      end do
      if(ilona > 0) write(6,*)' in g_create_egrid2agrid, dlona not constant to within tolerance'
      if(abs(rlona(nlona)+dlona-two_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlona(nlona) + dlona not within tolerance for 0 meridian',rlona(nlona)
         fail_tests=.true.
      end if
      if(mod(nlona,2) /= 0) then
         write(6,*)' in g_create_egrid2agrid, nlona not even' 
         fail_tests=.true.
      end if

!       ensemble grid tests:
      if(abs(rlate(1)+half_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlate(1) not within tolerance for south pole value' 
         fail_tests=.true.
      end if
      if(abs(rlate(nlate)-half_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlate(nlate) not within tolerance for north pole value' 
         fail_tests=.true.
      end if
      if(abs(rlone(1)) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlone(1) not within tolerance for 0 meridian' 
         fail_tests=.true.
      end if
      dlone=rlone(2)-rlone(1)
      ilone=0
      do j=1,nlone-1
         if(abs(rlone(j+1)-rlone(j)-dlone) > errtest) then
            fail_tests=.true.
            ilone=ilone+1
         end if
      end do
      if(ilone > 0) write(6,*)' in g_create_egrid2agrid, dlone not constant to within tolerance'
      if(abs(rlone(nlone)+dlone-two_pi) > errtest) then
         write(6,*)' in g_create_egrid2agrid, rlone(nlone) + dlone not within tolerance for 0 meridian' 
         fail_tests=.true.
      end if
      if(mod(nlone,2) /= 0) then
         write(6,*)' in g_create_egrid2agrid, nlone not even' 
         fail_tests=.true.
      end if
 
      if(fail_tests) then
         write(6,*)' incorrect input grid coordinates in subroutine g_create_egrid2agrid, program stops'
         call stop2(999)
      end if

!      construct extended ensemble grid used for actual interpolation 
!               (note that analysis grid needs no extension)

      nextend=1+(nord_e2a+2)/2
      p%nextend=nextend
      nlone_half=nlone/2
      p%nlone_half=nlone_half

      nlate_ex=nlate+2*nextend
      p%nlate_ex=nlate_ex
      allocate(rlate_ex(nlate_ex))
      do i=1,nlate
         rlate_ex(nextend+i)=rlate(i)
      end do
      do i=1,nextend
         rlate_ex(i)=-pi-rlate(nextend+2-i)
         rlate_ex(nextend+nlate+i)=pi-rlate(nlate-i)
      end do

      nlone_ex=nlone+2*nextend
      p%nlone_ex=nlone_ex
      allocate(rlone_ex(nlone_ex))
      do j=1,nlone
         rlone_ex(nextend+j)=rlone(j)
      end do
      do j=1,nextend
         rlone_ex(j)=rlone(nlone-nextend+j)-two_pi
         rlone_ex(nextend+nlone+j)=two_pi+rlone(j)
      end do
      call get_3ops(p%e2a_lon,nlona,rlona,nlone_ex,rlone_ex,nord_e2a,e2a_only)
      call get_3ops(p%e2a_lat,nlata,rlata,nlate_ex,rlate_ex,nord_e2a,e2a_only)

   end subroutine g_create_egrid2agrid

   subroutine g_egrid2agrid_r8_rank4(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r8_rank4   rank4 interface to g_egrid2agrid_r8
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see g_egrid2agrid_r8
!
! program history log:
!   2010-02-09  parrish, initial documentation
!   2011-07-26  todling, rank-4 interface
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(in   ) :: e(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)        ,intent(  out) :: a(:,:,:,:)

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call g_egrid2agrid_r8(p,e(i,:,:,:),a(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_egrid2agrid_r8_rank4: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif
   end subroutine g_egrid2agrid_r8_rank4

   subroutine g_egrid2agrid_r8(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r8   interpolate full global ensemble to analysis grid
!   prgmmr: parrish          org: np22                date: 2010-02-09
!
! abstract: for ensemble and analysis grids of full global extent, interpolate from
!            ensemble grid to analysis grid.
!
! program history log:
!   2010-02-09  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(in   ) :: e(p%nlate,p%nlone,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)        ,intent(  out) :: a(p%nlata,p%nlona,kb:ke)

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
           do j=1,p%nlone
              do i=1,p%nlate
                 a(i,j,kk)=e(i,j,kk)
              end do
            end do
         end do
      else

!           construct e_ex from input array e

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         factor=one
         if(vector(kk)) factor=-one
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=zero
            end do
         end do
         do j=1,p%nlone
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            do i=1,p%nlate
               e_ex(p%nextend+i)=e(i,j,kk)
            end do
            do i=1,p%nextend
               e_ex(p%nextend+1-i)=factor*e(i+1,jr,kk)
               e_ex(p%nlate+p%nextend+i)=factor*e(p%nlate-i,jr,kk)
            end do
            do i=1,p%nlata
               do k=1,p%e2a_lat%nwin(i)
                  w(i,j)=w(i,j)+p%e2a_lat%win(k,i)*e_ex(p%e2a_lat%iwin(k,i))
               end do
            end do
         end do
         do j=1,p%nlona
            do i=1,p%nlata
               a(i,j,kk)=zero
            end do
         end do

!         next get w_ex, extension of w in longitude

         do j=1,p%nlone
            do i=1,p%nlata
               w_ex(i,p%nextend+j)=w(i,j)
            end do
         end do
         do j=1,p%nextend
            do i=1,p%nlata
               w_ex(i,p%nextend+1-j)=w(i,p%nlone+1-j)
               w_ex(i,p%nlone+p%nextend+j)=w(i,j)
            end do
         end do
         do j=1,p%nlona
            do k=1,p%e2a_lon%nwin(j)
               j1=p%e2a_lon%iwin(k,j)
               w1=p%e2a_lon%win(k,j)
               do i=1,p%nlata
                  a(i,j,kk)=a(i,j,kk)+w1*w_ex(i,j1)
               end do
            end do
         end do
        end do
      end if

   end subroutine g_egrid2agrid_r8

   subroutine g_egrid2agrid_r4_rank4(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r4_rank4   rank4 interface to g_egrid2agrid_r4
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: see g_egrid2agrid_r4
!
! program history log:
!   2010-02-09  parrish, initial documentation
!   2011-07-26  todling, rank4 interface
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(in   ) :: e(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)        ,intent(  out) :: a(:,:,:,:)

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call g_egrid2agrid_r4(p,e(i,:,:,:),a(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_egrid2agrid_r4_rank4: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif
   end subroutine g_egrid2agrid_r4_rank4

   subroutine g_egrid2agrid_r4(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r4   interpolate full global ensemble to analysis grid
!   prgmmr: parrish          org: np22                date: 2010-02-09
!
! abstract: for ensemble and analysis grids of full global extent, interpolate from
!            ensemble grid to analysis grid.
!
! program history log:
!   2010-02-09  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     a              - analysis grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)          ,intent(in   ) :: e(p%nlate,p%nlone,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)          ,intent(  out) :: a(p%nlata,p%nlona,kb:ke)

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
           do j=1,p%nlone
              do i=1,p%nlate
                 a(i,j,kk)=e(i,j,kk)
              end do
            end do
         end do
      else

!           construct e_ex from input array e

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         factor=one
         if(vector(kk)) factor=-one
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=zero
            end do
         end do
         do j=1,p%nlone
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            do i=1,p%nlate
               e_ex(p%nextend+i)=e(i,j,kk)
            end do
            do i=1,p%nextend
               e_ex(p%nextend+1-i)=factor*e(i+1,jr,kk)
               e_ex(p%nlate+p%nextend+i)=factor*e(p%nlate-i,jr,kk)
            end do
            do i=1,p%nlata
               do k=1,p%e2a_lat%nwin(i)
                  w(i,j)=w(i,j)+p%e2a_lat%win(k,i)*e_ex(p%e2a_lat%iwin(k,i))
               end do
            end do
         end do
         do j=1,p%nlona
            do i=1,p%nlata
               a(i,j,kk)=zero
            end do
         end do

!         next get w_ex, extension of w in longitude

         do j=1,p%nlone
            do i=1,p%nlata
               w_ex(i,p%nextend+j)=w(i,j)
            end do
         end do
         do j=1,p%nextend
            do i=1,p%nlata
               w_ex(i,p%nextend+1-j)=w(i,p%nlone+1-j)
               w_ex(i,p%nlone+p%nextend+j)=w(i,j)
            end do
         end do
         do j=1,p%nlona
            do k=1,p%e2a_lon%nwin(j)
               j1=p%e2a_lon%iwin(k,j)
               w1=p%e2a_lon%win(k,j)
               do i=1,p%nlata
                  a(i,j,kk)=a(i,j,kk)+w1*w_ex(i,j1)
               end do
            end do
         end do
        end do
      end if

   end subroutine g_egrid2agrid_r4

   subroutine g_agrid2egrid_r8_rank4(p,a,e,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_agrid2egrid_r4_rank4   rank-4 interface to g_agrid2egrid_r4
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!   2011-07-26  todling, rank-4 interface
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(  out) :: e(:,:,:,:)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)        ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call g_agrid2egrid_r8(p,a(i,:,:,:),e(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_agrid2egrid_r8_rank4: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine g_agrid2egrid_r8_rank4

   subroutine g_agrid2egrid_r8(p,a,e,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_agrid2egrid_r8   smoothing inverse of g_egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-10
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)          ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)          ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
           do j=1,p%nlone
              do i=1,p%nlate
                 e(i,j,kk)=a(i,j,kk)
              end do
           end do
         end do
      else

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         factor=one
         if(vector(kk)) factor=-one
         w_ex=zero
         do j=1,p%nlone_ex
            do k=1,p%e2a_lon%ntwin(j)
               j1=p%e2a_lon%itwin(k,j)
               w1=p%e2a_lon%swin(k,j)
               do i=1,p%nlata
                  w_ex(i,j)=w_ex(i,j)+w1*a(i,j1,kk)
               end do
            end do
         end do

!         inverse of next get w_ex, extension of w in longitude
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=w_ex(i,p%nextend+j)
            end do
         end do

!           adjoint of construct e_ex from input array e

         do j=1,p%nlone
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            e_ex=zero
            do i=1,p%nlate_ex
               do k=1,p%e2a_lat%ntwin(i)
                  e_ex(i)=e_ex(i)+p%e2a_lat%swin(k,i)*w(p%e2a_lat%itwin(k,i),j)
               end do
            end do
            do i=1,p%nlate
               e(i,j,kk)=e_ex(p%nextend+i)
            end do
         end do
        end do

      end if

   end subroutine g_agrid2egrid_r8

   subroutine g_agrid2egrid_r4_rank4(p,a,e,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_agrid2egrid_r4_rank4   rank-4 interface to g_agrid2egrid_r4
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!   2011-07-26  todling, rank-4 interface
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(  out) :: e(:,:,:,:)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)        ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(a,1)
      if(idim==size(e,1)) then
         do i=1,idim
            call g_agrid2egrid_r4(p,a(i,:,:,:),e(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_agrid2egrid_r4_rank4: inconsistent dim(e,a)', idim, size(e,1)
         call stop2(999)
      endif

   end subroutine g_agrid2egrid_r4_rank4

   subroutine g_agrid2egrid_r4(p,a,e,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_agrid2egrid_r4   smoothing inverse of g_egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-10
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)        ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
           do j=1,p%nlone
              do i=1,p%nlate
                 e(i,j,kk)=a(i,j,kk)
              end do
           end do
         end do
      else

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         factor=one
         if(vector(kk)) factor=-one
         w_ex=zero
         do j=1,p%nlone_ex
            do k=1,p%e2a_lon%ntwin(j)
               j1=p%e2a_lon%itwin(k,j)
               w1=p%e2a_lon%swin(k,j)
               do i=1,p%nlata
                  w_ex(i,j)=w_ex(i,j)+w1*a(i,j1,kk)
               end do
            end do
         end do

!         inverse of next get w_ex, extension of w in longitude
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=w_ex(i,p%nextend+j)
            end do
         end do

!           adjoint of construct e_ex from input array e

         do j=1,p%nlone
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            e_ex=zero
            do i=1,p%nlate_ex
               do k=1,p%e2a_lat%ntwin(i)
                  e_ex(i)=e_ex(i)+p%e2a_lat%swin(k,i)*w(p%e2a_lat%itwin(k,i),j)
               end do
            end do
            do i=1,p%nlate
               e(i,j,kk)=e_ex(p%nextend+i)
            end do
         end do
        end do

      end if

   end subroutine g_agrid2egrid_r4

   subroutine g_egrid2agrid_r8_rank4_ad(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r8_rank4_ad   rank-4 interface to g_egrid2agrid_r8_ad
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!   2011-07-26  todling, rank-4 interface
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(  out) :: e(:,:,:,:)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)        ,intent(in   ) :: a(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call g_egrid2agrid_r8_ad(p,e(i,:,:,:),a(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_egrid2agrid_r8_rank4_ad: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif
   end subroutine g_egrid2agrid_r8_rank4_ad

   subroutine g_egrid2agrid_r8_ad(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r8_ad   adjoint of g_egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-10
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_double)        ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_double)        ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         w_ex=zero
         do j=1,p%nlona
            do k=1,p%e2a_lon%nwin(j)
               j1=p%e2a_lon%iwin(k,j)
               w1=p%e2a_lon%win(k,j)
               do i=1,p%nlata
                  w_ex(i,j1)=w_ex(i,j1)+w1*a(i,j,kk)
               end do
            end do
         end do

!         adjoint of next get w_ex, extension of w in longitude

         w=zero
         do j=1,p%nextend
            do i=1,p%nlata
               w(i,p%nlone+1-j)=w(i,p%nlone+1-j)+w_ex(i,p%nextend+1-j)
               w(i,j)=w(i,j)+w_ex(i,p%nlone+p%nextend+j)
            end do
         end do
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=w(i,j)+w_ex(i,p%nextend+j)
            end do
         end do

!        adjoint of construct e_ex from input array e

         factor=one
         if(vector(kk)) factor=-one

         do j=1,p%nlone
           do i=1,p%nlate
             e(i,j,kk)=zero
           end do
         end do
         do j=1,p%nlone
            e_ex=zero
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            do i=1,p%nlata
               do k=1,p%e2a_lat%nwin(i)
                  e_ex(p%e2a_lat%iwin(k,i))=e_ex(p%e2a_lat%iwin(k,i))+p%e2a_lat%win(k,i)*w(i,j)
               end do
            end do
            do i=1,p%nextend
               e(i+1,jr,kk)=e(i+1,jr,kk)+factor*e_ex(p%nextend+1-i)
               e(p%nlate-i,jr,kk)=e(p%nlate-i,jr,kk)+factor*e_ex(p%nlate+p%nextend+i)
            end do
            do i=1,p%nlate
               e(i,j,kk)=e(i,j,kk)+e_ex(p%nextend+i)
            end do
         end do
        end do

      end if

   end subroutine g_egrid2agrid_r8_ad

   subroutine g_egrid2agrid_r4_rank4_ad(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r4_ad   rank-4 interface to g_egrid2agrid_r4_ad
!   prgmmr: todling          org: np22                date: 2011-07-26
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!   2011-07-26  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(  out) :: e(:,:,:,:)
      integer(i_kind)       ,intent(in   ) :: kb,ke
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)        ,intent(in   ) :: a(:,:,:,:)

      integer(i_kind) i,idim

      idim=size(e,1)
      if(idim==size(a,1)) then
         do i=1,idim
            call g_egrid2agrid_r4_ad(p,e(i,:,:,:),a(i,:,:,:),kb,ke,vector)
         enddo
      else
         write(6,*)' g_egrid2agrid_r4_ad: inconsistent dim(e,a)', idim, size(a,1)
         call stop2(999)
      endif
   end subroutine g_egrid2agrid_r4_rank4_ad

   subroutine g_egrid2agrid_r4_ad(p,e,a,kb,ke,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2agrid_r4_ad   adjoint of g_egrid2agrid
!   prgmmr: parrish          org: np22                date: 2010-02-10
!
! abstract: adjoint of g_egrid2agrid
!
! program history log:
!   2010-02-10  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     a              - analysis grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!     kb             - starting level
!     ke             - ending level
!
!   output argument list:
!     e              - ensemble grid on full global domain
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_single)        ,intent(  out) :: e(p%nlate,p%nlone,kb:ke)
      logical               ,intent(in   ) :: vector(kb:ke)
      real(r_single)        ,intent(in   ) :: a(p%nlata,p%nlona,kb:ke)
      integer(i_kind)       ,intent(in   ) :: kb,ke

      integer(i_kind) i,j,j1,jr,k,kk
      real(r_kind) e_ex(p%nlate_ex),w_ex(p%nlata,p%nlone_ex)
      real(r_kind) w1,w(p%nlata,p%nlone),factor

      if(p%identity) then
         do kk=kb,ke
            do j=1,p%nlone
               do i=1,p%nlate
                  e(i,j,kk)=a(i,j,kk)
               end do
            end do
         end do
      else

!$omp parallel do  schedule(dynamic,1) private(kk,j,i,k,jr,j1) &
!$omp private(e_ex,w_ex,w1,w,factor)
        do kk=kb,ke
         w_ex=zero
         do j=1,p%nlona
            do k=1,p%e2a_lon%nwin(j)
               j1=p%e2a_lon%iwin(k,j)
               w1=p%e2a_lon%win(k,j)
               do i=1,p%nlata
                  w_ex(i,j1)=w_ex(i,j1)+w1*a(i,j,kk)
               end do
            end do
         end do

!         adjoint of next get w_ex, extension of w in longitude

         w=zero
         do j=1,p%nextend
            do i=1,p%nlata
               w(i,p%nlone+1-j)=w(i,p%nlone+1-j)+w_ex(i,p%nextend+1-j)
               w(i,j)=w(i,j)+w_ex(i,p%nlone+p%nextend+j)
            end do
         end do
         do j=1,p%nlone
            do i=1,p%nlata
               w(i,j)=w(i,j)+w_ex(i,p%nextend+j)
            end do
         end do

!        adjoint of construct e_ex from input array e

         factor=one
         if(vector(kk)) factor=-one

         do j=1,p%nlone
           do i=1,p%nlate
             e(i,j,kk)=zero
           end do
         end do
         do j=1,p%nlone
            e_ex=zero
            jr=j+p%nlone_half
            if(jr > p%nlone) jr=jr-p%nlone
            do i=1,p%nlata
               do k=1,p%e2a_lat%nwin(i)
                  e_ex(p%e2a_lat%iwin(k,i))=e_ex(p%e2a_lat%iwin(k,i))+p%e2a_lat%win(k,i)*w(i,j)
               end do
            end do
            do i=1,p%nextend
               e(i+1,jr,kk)=e(i+1,jr,kk)+factor*e_ex(p%nextend+1-i)
               e(p%nlate-i,jr,kk)=e(p%nlate-i,jr,kk)+factor*e_ex(p%nlate+p%nextend+i)
            end do
            do i=1,p%nlate
               e(i,j,kk)=e(i,j,kk)+e_ex(p%nextend+i)
            end do
         end do
        end do

      end if

   end subroutine g_egrid2agrid_r4_ad

   subroutine g_create_egrid2points_slow(na,rlata,rlona,nlate,rlate,nlone,rlone,nord_e2a,p)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_create_egrid2points_slow 1st version of grid to points
!   prgmmr: parrish          org: np22                date: 2010-03-04
!
! abstract: setup routine for g_egrid2points_slow, inefficient version of interpolation
!             from full global grid to general set of points.
!
!
! program history log:
!   2010-03-04  parrish, initial documentation
!   2010-03-09  parrish - add logical flag e2a_only, which is set to true and passed in as
!                            optional variable to get_3ops, so only forward interpolation from
!                            grid to points is computed.
!
!   input argument list:
!     na:     number of points to interpolate to
!     rlata:  latitudes of points in radians within range from -pi/2 to pi/2 
!     rlona:  longitudes of points in radians from 0 to 2*pi (values are transferred
!               and then internally adjusted to be in this range modulo 2*pi)
!     nlate:  number of ensemble latitudes
!     rlate:  ensemble latitudes in radians from -pi/2 to pi/2, including pole points
!     nlone:  number of ensemble longitudes (must be even)
!     rlone:  ensemble longitudes in radians from 0 to 360, not including point at 360
!     nord_e2a:     order of interpolation from ensemble to analysis grid
!
!   output argument list:
!     p
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

      use constants, only: zero,half,one,two,pi
      implicit none

      integer(i_kind),intent(in) :: na,nlate,nlone,nord_e2a
      real(r_kind),intent(in) :: rlata(na),rlona(na),rlate(nlate),rlone(nlone)
      type(egrid2agrid_parm),intent(inout) :: p

      integer(i_kind) i,ilone,j,nextend,nlate_ex,nlone_ex,nlone_half
      real(r_kind) half_pi,two_pi,dlone,errtest
      real(r_kind),allocatable::rlate_ex(:),rlone_ex(:)
      real(r_kind) rlona0(na)
      logical fail_tests,e2a_only

      p%nlata=na
      p%nlate=nlate
      p%nlone=nlone
      p%nlate_ex=nlate
      p%nlone_ex=nlone
      p%identity=.false.

!   check that lats and lons satisfy requirements:

      errtest=10._r_kind*pi*epsilon(one)
      half_pi=half*pi
      two_pi=two*pi
      fail_tests=.false.

!       points tests:
      if(minval(rlata) < -half_pi ) then
         write(6,*)' in g_create_egrid2points, some points beyond south pole--unphysical'
         fail_tests=.true.
      end if
      if(maxval(rlata) >  half_pi ) then
         write(6,*)' in g_create_egrid2points, some points beyond north pole--unphysical'
         fail_tests=.true.
      end if

!       ensemble grid tests:
      if(abs(rlate(1)+half_pi) > errtest) then
         write(6,*)' in g_create_egrid2points, rlate(1) not within tolerance for south pole value' 
         fail_tests=.true.
      end if
      if(abs(rlate(nlate)-half_pi) > errtest) then
         write(6,*)' in g_create_egrid2points, rlate(nlate) not within tolerance for north pole value' 
         fail_tests=.true.
      end if
      if(abs(rlone(1)) > errtest) then
         write(6,*)' in g_create_egrid2points, rlone(1) not within tolerance for 0 meridian' 
         fail_tests=.true.
      end if
      dlone=rlone(2)-rlone(1)
      ilone=0
      do j=1,nlone-1
         if(abs(rlone(j+1)-rlone(j)-dlone) > errtest) then
            fail_tests=.true.
            ilone=ilone+1
         end if
      end do
      if(ilone > 0) write(6,*)' in g_create_egrid2points, dlone not constant to within tolerance'
      if(abs(rlone(nlone)+dlone-two_pi) > errtest) then
         write(6,*)' in g_create_egrid2points, rlone(nlone) + dlone not within tolerance for 0 meridian' 
         fail_tests=.true.
      end if
      if(mod(nlone,2) /= 0) then
         write(6,*)' in g_create_egrid2points, nlone not even' 
         fail_tests=.true.
      end if
 
      if(fail_tests) then
         write(6,*)' incorrect input grid coordinates in subroutine g_create_egrid2points, program stops'
         call stop2(999)
      end if

!     copy rlona to internal array rlona0, adjusting values modulo 2*pi to be in range 0 to 2*pi

      do j=1,na
         rlona0(j)=rlona(j)
      end do
      do while(minval(rlona0) <= -errtest)
         do j=1,na
            if(rlona0(j) < zero) rlona0(j)=rlona0(j)+two_pi
         end do
      end do
      do while(maxval(rlona0) >= two_pi+errtest)
         do j=1,na
            if(rlona0(j) >= two_pi) rlona0(j)=rlona0(j)-two_pi
         end do
      end do
      

!      construct extended ensemble grid used for actual interpolation 
!               (note that analysis grid needs no extension)

      nextend=1+(nord_e2a+2)/2
      p%nextend=nextend
      nlone_half=nlone/2
      p%nlone_half=nlone_half

      nlate_ex=nlate+2*nextend
      p%nlate_ex=nlate_ex
      allocate(rlate_ex(nlate_ex))
      do i=1,nlate
         rlate_ex(nextend+i)=rlate(i)
      end do
      do i=1,nextend
         rlate_ex(i)=-pi-rlate(nextend+2-i)
         rlate_ex(nextend+nlate+i)=pi-rlate(nlate-i)
      end do

      nlone_ex=nlone+2*nextend
      p%nlone_ex=nlone_ex
      allocate(rlone_ex(nlone_ex))
      do j=1,nlone
         rlone_ex(nextend+j)=rlone(j)
      end do
      do j=1,nextend
         rlone_ex(j)=rlone(nlone-nextend+j)-two_pi
         rlone_ex(nextend+nlone+j)=two_pi+rlone(j)
      end do
      e2a_only=.true.
      call get_3ops(p%e2a_lat,na,rlata,nlate_ex,rlate_ex,nord_e2a,e2a_only)
      call get_3ops(p%e2a_lon,na,rlona0,nlone_ex,rlone_ex,nord_e2a,e2a_only)

   end subroutine g_create_egrid2points_slow

   subroutine g_egrid2points_faster(p,e,a,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    g_egrid2points_slow interp full global grid to set of points
!   prgmmr: parrish          org: np22                date: 2010-03-04
!
! abstract: interpolate from full global grid to general set of points (slow version)
!
! program history log:
!   2010-03-04  parrish, initial documentation
!
!   input argument list:
!     p              - parameters for egrid2agrid
!     e              - ensemble grid on full global domain
!     vector         - if true, then interpolating a vector component, so
!                        need to multiply interpolating weights by p%vector
!
!   output argument list:
!     a              - interpolated values on general set of points
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      use constants, only: zero,one
      implicit none

      type(egrid2agrid_parm),intent(in   ) :: p
      real(r_kind)          ,intent(in   ) :: e(p%nlate,p%nlone)
      logical               ,intent(in   ) :: vector
      real(r_kind)          ,intent(  out) :: a(p%nlata)

      real(r_kind) e_ex(p%nlate_ex,p%nlone_ex)
      real(r_kind) w(p%nlone_ex)                  !  this array is too big by nlone/(nord_e2a+1)
                                                    !   which is why this is the slow version.
      integer(i_kind) i,j,j1,je,jr,k
      real(r_kind) w1,factor
!               
!           construct e_ex from input array e

      factor=one
      if(vector) factor=-one
!           extend in latitude first
      do j=1,p%nlone
         je=j+p%nextend
         jr=j+p%nlone_half
         if(jr > p%nlone) jr=jr-p%nlone
         do i=1,p%nlate
            e_ex(p%nextend+i,je)=e(i,j)
         end do
         do i=1,p%nextend
            e_ex(p%nextend+1-i,je)=factor*e(i+1,jr)
            e_ex(p%nlate+p%nextend+i,je)=factor*e(p%nlate-i,jr)
         end do
      end do
!           next extend in longitude
      do j=1,p%nextend
         je=j+p%nextend
         do i=1,p%nlate_ex
            e_ex(i,j)=e_ex(i,j+p%nlone)
            e_ex(i,je+p%nlone)=e_ex(i,je)
         end do
      end do

!       for each point, first interpolate in latitude at longitudes required for longitude interpolation,
!         then finish up with longitude interpolation

      do i=1,p%nlata
         a(i)=zero
         do j=1,p%e2a_lon%nwin(i)
            j1=p%e2a_lon%iwin(j,i)
            w1=p%e2a_lon%win(j,i)
            w(j1)=zero
            do k=1,p%e2a_lat%nwin(i)
               w(j1)=w(j1)+p%e2a_lat%win(k,i)*e_ex(p%e2a_lat%iwin(k,i),j1)
            end do
            a(i)=a(i)+w1*w(j1)
         end do
      end do

   end subroutine g_egrid2points_faster

end module egrid2agrid_mod
