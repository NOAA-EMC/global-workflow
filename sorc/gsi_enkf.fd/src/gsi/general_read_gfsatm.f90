module gfsreadmod

contains
subroutine general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vdflag,g_cf)  
! !USES:
  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info

  implicit none
! !INPUT PARAMETERS:

  type(sub2grid_info),                intent(in   ) :: grd
  integer(i_kind),                    intent(inout) :: icount
  integer(i_kind),dimension(npe),     intent(inout) :: ilev,iflag
  real(r_kind),dimension(grd%itotsub),intent(in   ) :: work
  logical,                            intent(in   ) :: uvflag,vdflag

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(  out) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(inout) :: g_z
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out),optional :: g_cf


! !DESCRIPTION: Transfer contents of 2-d array global to 3-d subdomain array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2014-12-03  derber     - introduce vdflag and optimize routines
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) i,j,k,ij,klev
   real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

   call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
        sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
        mpi_comm_world,ierror)

!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
   do k=1,icount
      if ( iflag(k) == 1 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_z(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 2 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ps(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 3 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_tv(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 4 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_vor(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_u(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 5 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_div(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_v(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 6 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  u '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_u(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 7 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  v '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_v(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 8 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_q(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 9 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_oz(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 10 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_cwmr(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 11 .and. present(g_cf) ) then  
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_cf(i,j,klev)=sub(ij,k)
            enddo
         enddo
      endif
   enddo ! do k=1,icount

   icount=0
   ilev=0
   iflag=0

   return

end subroutine general_reload
subroutine general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
           g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vdflag,g_ni,g_nr,g_cf)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  use ncepnems_io, only: imp_physics
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info),                intent(in   ) :: grd
  integer(i_kind),                    intent(inout) :: icount
  integer(i_kind),dimension(npe),     intent(inout) :: ilev,iflag
  real(r_kind),dimension(grd%itotsub),intent(in   ) :: work
  logical,                            intent(in   ) :: uvflag,vdflag

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(  out) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(inout) :: g_z
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_q,g_oz,g_tv,g_ql,g_qi,g_qr,g_qs,g_qg,g_ni,g_nr

  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out),optional :: g_cf


! !DESCRIPTION: Transfer contents of 2-d array global to 3-d subdomain array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2014-12-03  derber     - introduce vdflag and optimize routines
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) i,j,k,ij,klev
   real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

   call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
        sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
        mpi_comm_world,ierror)

!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
   do k=1,icount
      if ( iflag(k) == 1 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_z(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 2 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ps(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 3 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_tv(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 4 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_vor(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_u(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 5 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_div(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_v(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 6 ) then
         if ( .not. uvflag) then
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_u(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 7 ) then
         if ( .not. uvflag) then
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_v(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 8 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_q(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 9 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_oz(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 10 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ql(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 11 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qi(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 12 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qr(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 13 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qs(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 14 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qg(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 15 .and. imp_physics == 8) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ni(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 16 .and. imp_physics == 8) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_nr(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 17 .and. present(g_cf) ) then 
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_cf(i,j,klev)=sub(ij,k)
            enddo
         enddo
      endif
   enddo ! do k=1,icount

   icount=0
   ilev=0
   iflag=0

   return

end subroutine general_reload2

! 2m reload
subroutine general_reload_sfc(grd,g_t2m, g_q2m,g_ps,icount,iflag,work)
! !USES:
  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info

  implicit none
! !INPUT PARAMETERS:

  type(sub2grid_info),                intent(in   ) :: grd
  integer(i_kind),                    intent(inout) :: icount
  integer(i_kind),dimension(npe),     intent(inout) :: iflag
  real(r_kind),dimension(grd%itotsub),intent(in   ) :: work

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(  out) :: g_t2m,&
                                                                      g_q2m, g_ps

! !DESCRIPTION: version of general_reload, for 2m variables.
!
! !REVISION HISTORY:
!  2023-03-2    Draper
!-------------------------------------------------------------------------

   integer(i_kind) i,j,ij,k
   real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

   call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
        sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
        mpi_comm_world,ierror)

!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij)

   do k=1,icount
      if ( iflag(k) == 2 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_t2m(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 3 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_q2m(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 4 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ps(i,j)=sub(ij,k)
            enddo
         enddo
      endif
   enddo ! do k=1,icount

   icount=0
   iflag=0

   return

end subroutine general_reload_sfc

end module gfsreadmod

subroutine general_read_gfsatm(grd,sp_a,sp_b,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,init_head,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm  adaptation of read_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from read_gfsatm, primarily for reading in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  kleist     - modifications to allow for st/vp perturbations instead of u,v
!   2012-01-17  wu         - increase character length for variable "filename"
!   2014-11-30  todling    - genelize interface to handle bundle instead of fields;
!                            internal code should be generalized
!   2014-12-03  derber     - introduce vordivflag, zflag and optimize routines
!   2023-03-23  draper     - added option to read sfc files (for 2m variables)
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!     init_head- flag to read header record.  Usually .true. unless repeatedly
!                reading similar files (ensembles)
!
!   output argument list:
!     gfs_bundle  - guess fields in bundle form
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use mpimod, only: mype
   use gridmod, only: ncepgfs_head,idpsfc5,idthrm5,&
                      ntracer,idvc5,cp5,idvm5
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use sigio_module, only: sigio_intkind
   use sigio_r_module, only: sigio_dbti,sigio_rrhead,sigio_rropen,&
                             sigio_rrdbti,sigio_rclose
   use ncepgfs_io, only: sigio_cnvtdv8,sighead
   use gsi_bundlemod, only: gsi_bundle,gsi_bundlegetpointer
   use gfsreadmod, only: general_reload

   implicit none

   ! Declare local parameters
   integer(sigio_intkind):: lunges = 11

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a,sp_b
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag,init_head
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   ! Declare local variables
   integer(i_kind):: iret,nlatm2,nlevs,icm
   integer(i_kind):: ier,istatus,iredundant
   integer(i_kind):: i,j,k,icount
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid

   real(r_kind),dimension(sp_b%nc),target ::  specwrk_4,specdiv_4
   real(r_kind),dimension(sp_b%nc):: spec_work

   real(r_kind),dimension(grd%itotsub):: work
   real(r_kind),allocatable,dimension(:):: spec_div
   real(r_kind),allocatable,dimension(:,:):: grid_v
   logical :: procuse

   type(sigio_dbti):: sigdati

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_cwmr,g_q,g_oz,g_tv
   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.
   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount

   ! All tasks open and read header with RanRead
   if ( procuse ) then
      rewind(lunges)
      call sigio_rropen(lunges,filename,iret)
      if ( init_head .or. mype == 0 ) then
         call sigio_rrhead(lunges,sighead,iret_read)
         if ( iret_read /= 0 ) then
            write(6,*)'GENERAL_READ_GFSATM:  ***ERROR*** reading ',&
                trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
            return
         end if
      endif
      if ( nlevs /= sighead%levs ) then
         write(6,*)'GENERAL_READ_GFSATM:  ***ERROR*** reading ',&
             trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
         return
      end if
   endif

   ! Get pointer to relevant variables (this should be made flexible and general)
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm: ERROR'
         write(6,*) 'cannot handle having both sf and div'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm: ERROR'
         write(6,*) 'cannot handle having both vp and vor'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm: ERROR'
         write(6,*) 'cannot handle having both t and tv'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   istatus=0
   call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
   if ( istatus /= 0 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   ! Process guess fields according to type of input file.   NCEP_SIGIO files
   ! are spectral coefficient files and need to be transformed to the grid.
   ! Once on the grid, fields need to be scattered from the full domain to
   ! sub-domains.

   icount=0

   ! Only read Terrain when zflag is true.
   if ( zflag ) then
   ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1
      if (mype==mype_use(icount)) then
         ! read hs
          sigdati%i = 1 ! hs
          sigdati%f => specwrk_4
          call sigio_rrdbti(lunges,sighead,sigdati,iret)
         do i=1,sp_b%nc
            spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
         enddo
         do i=1,sp_b%nc
            if ( sp_b%factsml(i) ) spec_work(i)=zero
         enddo
         call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
         call general_fill_ns(grd,grid,work)
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif
   endif ! if ( zflag )

   icount=icount+1
   iflag(icount)=2
   ilev(icount)=1

   ! Surface pressure:  same procedure as terrain
   if (mype==mype_use(icount)) then
      ! read ps
       sigdati%i = 2 ! ps
       sigdati%f => specwrk_4
       call sigio_rrdbti(lunges,sighead,sigdati,iret)
       do i=1,sp_b%nc
          spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
       enddo
       do i=1,sp_b%nc
          if ( sp_b%factsml(i) ) spec_work(i)=zero
       enddo
       call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
       call general_fill_ns(grd,grid,work)
   endif
   if ( icount == icm ) then
      call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vordivflag)
   endif

   ! Thermodynamic variable:  s-->g transform, communicate to all tasks
   ! For multilevel fields, each task handles a given level.  Periodic
   ! mpi_alltoallv calls communicate the grids to all mpi tasks.
   ! Finally, the grids are loaded into guess arrays used later in the
   ! code.
   do k=1,nlevs

      icount=icount+1
      iflag(icount)=3
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! read T/Tv/etc.
         sigdati%i = 2+k ! T
         sigdati%f => specwrk_4
         call sigio_rrdbti(lunges,sighead,sigdati,iret)
         do i=1,sp_b%nc
            spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
         enddo
         do i=1,sp_b%nc
            if ( sp_b%factsml(i) ) spec_work(i)=zero
         enddo
         call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
         ! Load values into rows for south and north pole
         call general_fill_ns(grd,grid,work)
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      if ( vordivflag .or. .not. uvflag ) then

         icount=icount+1
         iflag(icount)=4
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Vorticity
            sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
            sigdati%f => specwrk_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            ! Convert spectral coefficients of vor to grid space
            do i=1,sp_b%nc
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
            enddo
            do i=1,sp_b%nc
               if ( sp_b%factvml(i))spec_work(i)=zero
            enddo
            call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
            ! Convert grid u,v to div and vor
            call general_fill_ns(grd,grid,work)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=5
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Divergence
            sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
            sigdati%f => specwrk_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            ! Convert spectral coefficients of div to grid space
            do i=1,sp_b%nc
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !div
            enddo
            do i=1,sp_b%nc
               if ( sp_b%factvml(i))spec_work(i)=zero
            enddo
            call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
            ! Convert grid u,v to div and vor
            call general_fill_ns(grd,grid,work)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( vordivflag .or. .not. uvflag ) then

      if ( uvflag ) then

         icount=icount+1
         iflag(icount)=6
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! U  Compute u and v from div and vor
            ! Divergence
            sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
            sigdati%f => specdiv_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            ! Vorticity
            sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
            sigdati%f => specwrk_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            allocate(spec_div(sp_b%nc),grid_v(grd%nlon,grd%nlat-2))
            do i=1,sp_b%nc
               spec_div(i)=sp_b%test_mask(i)*specdiv_4(i)   !div
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
            enddo
            do i=1,sp_b%nc
               if ( sp_b%factvml(i) ) then
                  spec_div(i)=zero
                  spec_work(i)=zero
               endif
            enddo
            call general_sptez_v_b(sp_a,sp_b,spec_div,spec_work,grid,grid_v,1,1)
            call general_fillu_ns(grd,sp_a,grid,grid_v,work)
            deallocate(spec_div,grid_v)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=7
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! V Compute u and v from div and vor
            ! Divergence
            sigdati%i = nlevs + 2 + (k-1) * 2 + 1     ! Divergence
            sigdati%f => specdiv_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            ! Vorticity
            sigdati%i = nlevs + 2 + (k-1) * 2 + 2     ! Vorticity
            sigdati%f => specwrk_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            allocate(spec_div(sp_b%nc),grid_v(grd%nlon,grd%nlat-2))
            do i=1,sp_b%nc
               spec_div(i)=sp_b%test_mask(i)*specdiv_4(i)   !div
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)   !vor
            enddo
            do i=1,sp_b%nc
               if ( sp_b%factvml(i) ) then
                  spec_div(i)=zero
                  spec_work(i)=zero
               endif
            enddo
            call general_sptez_v_b(sp_a,sp_b,spec_div,spec_work,grid,grid_v,1,-1)
            call general_fillv_ns(grd,sp_a,grid,grid_v,work)
            deallocate(spec_div,grid_v)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( uvflag )

      icount=icount+1
      iflag(icount)=8
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Specific humidity
         sigdati%i = nlevs * (2+1) + 2 + k    ! q
         sigdati%f => specwrk_4
         call sigio_rrdbti(lunges,sighead,sigdati,iret)
         do i=1,sp_b%nc
            spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
         enddo
         do i=1,sp_b%nc
            if ( sp_b%factsml(i))spec_work(i)=zero
         enddo
         call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
         call general_fill_ns(grd,grid,work)
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=9
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Ozone mixing ratio
         sigdati%i = nlevs * (2+2) + 2 + k    ! oz
         sigdati%f => specwrk_4
         call sigio_rrdbti(lunges,sighead,sigdati,iret)
         do i=1,sp_b%nc
            spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
         enddo
         do i=1,sp_b%nc
            if ( sp_b%factsml(i))spec_work(i)=zero
         enddo
         call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
         call general_fill_ns(grd,grid,work)
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=10
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud condensate mixing ratio.
         if (sighead%ntrac>2 .or. sighead%ncldt>=1) then
            sigdati%i = nlevs * (2+3) + 2 + k    ! cw, 3rd tracer
            sigdati%f => specwrk_4
            call sigio_rrdbti(lunges,sighead,sigdati,iret)
            do i=1,sp_b%nc
               spec_work(i)=sp_b%test_mask(i)*specwrk_4(i)
            enddo
            do i=1,sp_b%nc
               if(sp_b%factsml(i))spec_work(i)=zero
            enddo
            call general_sptez_s_b(sp_a,sp_b,spec_work,grid,1)
            call general_fill_ns(grd,grid,work)
         else
            work=zero
         endif
      endif
      if ( icount == icm .or. k == nlevs ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

   enddo ! do k=1,nlevs

   if ( procuse ) then
      ! Close sigio data unit
      call sigio_rclose(lunges,iret)
      !if ( init_head .or. mype == 0)deallocate(sighead%vcoord,sighead%cfvars)
   endif

   ! Surface pressure.
   ! NCEP SIGIO has two options for surface pressure.  Variable idpsfc5
   ! indicates the type:
   ! idpsfc5= 0,1 for ln(psfc)
   ! idpsfc5= 2 for psfc
   ! If input ln(ps), take exponential to convert to ps in cb
   if (idpsfc5 /= 2) then
      do j=1,grd%lon2
         do i=1,grd%lat2
            g_ps(i,j)=exp(g_ps(i,j))
         enddo
      enddo
   endif

   ! NCEP SIGIO has three possible thermodynamic variables
   ! Variable idthrm5 indicates the type
   ! idthrm5 = 0,1 = virtual temperature (Tv)
   ! idthrm5 = 2   = sensible (dry) temperature (T)
   ! idthrm5 = 3   = enthalpy (h=CpT)
   ! The GSI analysis variable is Tv

   if (idthrm5==2 .or. idthrm5==3) then
      ! Convert input enthalpy to dry temperature
      if (idthrm5==3) then
         call sigio_cnvtdv8(grd%lat2*grd%lon2,grd%lat2*grd%lon2,&
              grd%nsig,idvc5,idvm5,ntracer,iret,g_tv,g_q,cp5,1)
      endif

      ! Convert dry temperature to virtual temperature
      do k=1,grd%nsig
         do j=1,grd%lon2
            do i=1,grd%lat2
               g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
            enddo
         enddo
      enddo
   endif

   ! Print date/time stamp
   if ( mype == 0) then
      write(6,700) sighead%lonb,sighead%latb,nlevs,grd%nlon,nlatm2,&
           sighead%fhour,sighead%idate
700   format('GENERAL_READ_GFSATM:  ges read/scatter, lonb,latb,levs=',&
            3i6,', nlon,nlat=',2i6,', hour=',f10.1,', idate=',4i5)
   endif

   ! Load u->div and v->vor slot when uv are used instead
   if (uvflag) then
      call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
      if ( ier == 0) then
         ptr3d=g_u
         call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
         if ( ier == 0) ptr3d=g_v
      else ! in this case, overload: return u/v in sf/vp slot
         call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
         if ( ier == 0) then
            ptr3d=g_u
            call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
            if ( ier == 0) ptr3d=g_v
         endif
      endif
   else ! in this case, overload: return u/v in sf/vp slot
      call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
      if ( ier == 0) ptr3d=g_u
      call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
      if ( ier == 0) ptr3d=g_v
   endif
   if (zflag) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)

   return

end subroutine general_read_gfsatm

subroutine general_read_gfsatm_nems(grd,sp_a,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,init_head,iret_read,it)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm  adaptation of read_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from read_gfsatm, primarily for reading in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  kleist     - modifications to allow for st/vp perturbations instead of u,v
!   2012-01-17  wu         - increase character length for variable "filename"
!   2014-11-30  todling    - genelize interface to handle bundle instead of fields;
!                            internal code should be generalized
!   2014-12-03  derber     - introduce vordivflag, zflag and optimize routines
!   2019-06-06  eliu       - add cloud fraction 
!   2019-07-10  zhu        - add convective clouds
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!     init_head- flag to read header record.  Usually .true. unless repeatedly
!                reading similar files (ensembles)
!
!   output argument list:
!     gfs_bundle  - bundle carrying guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use mpimod, only: mype
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close,nemsio_charkind
   use ncepnems_io, only: error_msg,imp_physics
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv,nemsio_getrechead 
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use jfunc, only: cnvw_option
   use gfsreadmod, only: general_reload
   use guess_grids, only: ifilesfc

   implicit none

   ! Declare local parameters
   real(r_kind),parameter:: r0_001 = 0.001_r_kind

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag,init_head
   integer(i_kind)                       ,intent(in   ) :: it
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_cwmr,g_q,g_oz,g_tv,g_cf

   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_GFSATM_NEMS'
   character(len=24)       :: filenamesfc
   character(len=1)   :: null = ' '
   integer(i_kind):: jrec,nrec 
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk
   integer(i_kind) :: ier,istatus,iredundant
   integer(i_kind) :: latb, lonb, levs, nframe
   integer(i_kind) :: latb2, lonb2
   integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   integer(i_kind),dimension(7):: idate
   integer(i_kind),dimension(4):: odate
   real(r_kind) :: fhour

   real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
   real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
        grid_vor, grid_div, grid_b, grid_b2
   real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
   real(r_kind),allocatable,dimension(:)   :: work, work_v
   real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1
   real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
   real(4),allocatable,dimension(:) :: r4lats,r4lons

   logical :: procuse,diff_res,eqspace,has_cf
   character(nemsio_charkind),allocatable:: recname(:)
   type(nemsio_gfile) :: gfile
   type(nemsio_gfile) :: gfilesfc
   type(egrid2agrid_parm) :: p_high
   logical,dimension(1) :: vector

   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.
   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount
   allocate( work(grd%itotsub),work_v(grd%itotsub) )
   work=zero
   work_v=zero

   if ( procuse ) then

      if ( init_head)call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)

      if (  nframe /= 0 ) then
         if ( mype == 0 ) &
            write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      endif

      ! check if cloud fraction (cld_amt) is in the file
      call nemsio_getfilehead(gfile,nrec=nrec,iret=iret)
      allocate(recname(nrec))
      call nemsio_getfilehead(gfile,recname=recname,iret=iret)  
      has_cf = .false.
      do jrec=1,nrec
        if (recname(jrec)=='cld_amt') has_cf=.true. 
      enddo
      if (mype==0) write(6,*) trim(my_name), ' has_cf = ', has_cf 

      fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
              real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

      diff_res=.false.
      if ( latb /= nlatm2 ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
         !call stop2(101)
      endif
      if ( lonb /= grd%nlon ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
         !call stop2(101)
      endif
      if ( levs /= grd%nsig ) then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      endif

      if (cnvw_option) then
         write(filenamesfc,'(''sfcf'',i2.2)') ifilesfc(it)
         call nemsio_open(gfilesfc,filenamesfc,'READ',iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filenamesfc),null,'open',istop+2,iret)

         call nemsio_getfilehead(gfilesfc,iret=iret,dimx=lonb2, dimy=latb2)
         if (iret == 0) then
            if ( latb2 /= nlatm2 ) then
               if ( mype == 0 ) write(6, &
                  '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb2 ='',i4)') &
                  trim(my_name),nlatm2,latb2
               call stop2(101)
            endif
            if ( lonb2 /= grd%nlon ) then
               if ( mype == 0 ) write(6, &
                  '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb2 ='',i4)') &
                  trim(my_name),grd%nlon,lonb2
               call stop2(101)
            endif
         endif
      end if

      allocate( spec_vor(sp_a%nc), spec_div(sp_a%nc) )
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if ( diff_res ) then
         allocate(grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate(grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      endif
      allocate(rwork1d0(latb*lonb))
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
         rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      enddo
      do j=1,lonb
         rlons(j)=deg2rad*r4lons(j)
      enddo
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      enddo

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

   endif ! if ( procuse )

   ! Get pointer to relevant variables (this should be made flexible and general)
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both sf and div'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both vp and vor'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both t and tv'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   istatus=0
   call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
   if(has_cf) call gsi_bundlegetpointer(gfs_bundle,'cf',g_cf,  ier);istatus=istatus+ier

   if ( istatus /= 0 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   icount=0

   !   Process guess fields according to type of input file.   NCEP_SIGIO files
   !   are spectral coefficient files and need to be transformed to the grid.
   !   Once on the grid, fields need to be scattered from the full domain to
   !   sub-domains.

   !  Only read Terrain when zflag is true.
   if ( zflag ) then

      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1

      ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      if (mype==mype_use(icount)) then
         ! read hs
         call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','read',istop+2,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         if (has_cf) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
         else
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag) 
         endif
      endif
   endif

   icount=icount+1
   iflag(icount)=2
   ilev(icount)=1

   ! Surface pressure:  same procedure as terrain
   if (mype==mype_use(icount)) then
      ! read ps
      call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop+3,iret)
      rwork1d1 = r0_001*rwork1d0 ! convert Pa to cb
      if ( diff_res ) then
         vector(1)=.false.
         grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
         call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
         call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
         do kk=1,grd%itotsub
            i=grd%ltosi_s(kk)
            j=grd%ltosj_s(kk)
            work(kk)=grid2(i,j,1)
         enddo
      else
         grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/))
         call general_fill_ns(grd,grid,work)
      endif
   endif
   if ( icount == icm ) then
      if (has_cf) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
      else
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag) 
      endif
   endif

   !   Thermodynamic variable:  s-->g transform, communicate to all tasks
   !   For multilevel fields, each task handles a given level.  Periodic
   !   mpi_alltoallv calls communicate the grids to all mpi tasks.
   !   Finally, the grids are loaded into guess arrays used later in the
   !   code.

   do k=1,nlevs

      icount=icount+1
      iflag(icount)=3
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! read T/Tv/etc.
         call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop+7,iret)
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d1,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+7,iret)
         rwork1d0=rwork1d0*(one+fv*rwork1d1)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         if (has_cf) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
         else
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag) 
         endif
      endif

      if ( vordivflag .or. .not. uvflag ) then

         icount=icount+1
         iflag(icount)=4
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Vorticity
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_vor(grd%nlon,nlatm2))
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_vor,work)
            deallocate(grid_vor)
         endif
         if ( icount == icm ) then
            if (has_cf) then
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
            else
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag) 
            endif
         endif

         icount=icount+1
         iflag(icount)=5
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Divergence
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_div(grd%nlon,nlatm2) )
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_div,work)
            deallocate(grid_div)
         endif
         if ( icount == icm ) then
            if (has_cf) then
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
            else
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag) 
            endif
         endif

      endif ! if ( vordivflag .or. .not. uvflag )

      if ( uvflag ) then

         icount=icount+1
         iflag(icount)=6
         ilev(icount)=k

         if (mype==mype_use(icount)) then

            ! U
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
         endif
         if ( icount == icm ) then
            if (has_cf) then
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
            else
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag) 
            endif
         endif

         icount=icount+1
         iflag(icount)=7
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! V
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               ! Note work_v and work are switched because output must be in work.
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work_v,work)
            endif
         endif
         if ( icount == icm ) then
            if (has_cf) then
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
            else
               call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag) 
            endif
         endif

      endif ! if ( uvflag )

      icount=icount+1
      iflag(icount)=8
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Specific humidity
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+6,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         if (has_cf) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag,g_cf)  
         else
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)  
         endif
      endif

      icount=icount+1
      iflag(icount)=9
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Ozone mixing ratio
         call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','read',istop+8,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         if (has_cf) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag,g_cf)  
         else
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)  
         endif
      endif

      icount=icount+1
      iflag(icount)=10
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud condensate mixing ratio.
         work=zero
         call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
         if (imp_physics == 11) then
            call nemsio_readrecv(gfile,'icmr','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) then
               call error_msg(trim(my_name),trim(filename),'icmr','read',istop+10,iret)
            else
               rwork1d0 = rwork1d0 + rwork1d1
            endif
         endif
         if (cnvw_option) then
            call nemsio_readrecv(gfilesfc,'cnvcldwat','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) then
               call error_msg(trim(my_name),trim(filenamesfc),'cnvcldwat','read',istop+11,iret)
            else
               rwork1d0 = rwork1d0 + rwork1d1
            endif
         endif
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
    ! if ( icount == icm .or. k == nlevs ) then 
      if ( icount == icm  .or. ( (.not. has_cf) .and. k==nlevs) ) then  
         if (has_cf) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag,g_cf)  
         else
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag) 
         endif
      endif

      if (has_cf) then
         icount=icount+1
         iflag(icount)=11
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! cloud amount 
            call nemsio_readrecv(gfile,'cld_amt','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'cld_amt','read',istop+11,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               vector(1)=.false.
               call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               call general_fill_ns(grd,grid,work)
            endif
         endif
         if ( icount == icm .or. k==nlevs ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                    icount,iflag,ilev,work,uvflag,vordivflag,g_cf) 
         endif
      endif

   enddo ! do k=1,nlevs

   if ( procuse ) then
      if ( diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
      call destroy_egrid2agrid(p_high)
      deallocate(spec_div,spec_vor)
      deallocate(rwork1d1,clons,slons)
      deallocate(rwork1d0)
      deallocate(grid,grid_v)
      call nemsio_close(gfile,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop+9,iret)
      if (cnvw_option) then
         call nemsio_close(gfilesfc,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filenamesfc),null,'close',istop+10,iret)
      end if
   endif
   deallocate(work)

   ! Convert dry temperature to virtual temperature
   !do k=1,grd%nsig
   !   do j=1,grd%lon2
   !      do i=1,grd%lat2
   !         g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
   !      enddo
   !   enddo
   !enddo

   ! Load u->div and v->vor slot when uv are used instead
   if ( uvflag ) then
      call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
      if ( ier == 0 ) then
         ptr3d=g_u
         call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
         if ( ier == 0 ) ptr3d=g_v
      else ! in this case, overload: return u/v in sf/vp slot
         call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
         if ( ier == 0 ) then
            ptr3d=g_u
            call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
            if ( ier == 0 ) ptr3d=g_v
         endif
      endif
   else ! in this case, overload: return u/v in sf/vp slot
      call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_u
      call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_v
   endif
   if (zflag) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0 ) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)
   if (allocated(recname)) deallocate(recname)


   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,filename(1:24)
700   format('GENERAL_READ_GFSATM_NEMS: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

end subroutine general_read_gfsatm_nems

subroutine general_read_gfsatm_nc(grd,sp_a,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,init_head,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm_nc  adaptation of read_gfsatm for general resolutions
!   prgmmr: martin          org: NCEP/EMC                date: 2019-09-25
!
! abstract: copied from read_gfsatm_nems
!
! program history log:
!   2019-09-25  martin     - original; copied from general_read_gfsatm_nems
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input netcdf file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!     init_head- flag to read header record.  Usually .true. unless repeatedly
!                reading similar files (ensembles)
!
!   output argument list:
!     gfs_bundle  - bundle carrying guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe,mype,mpi_comm_world,ierror,mpi_integer,mpi_max,setcomm
   use constants, only: zero,one,fv,r0_01
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata,get_idate_from_time_units
   use gfsreadmod, only: general_reload, general_reload_sfc

   implicit none

   ! Declare local parameters
   real(r_kind),parameter:: r0_001 = 0.001_r_kind

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag,init_head
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:)       :: g_t2m, g_q2m
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_cwmr,g_q,g_oz,g_tv

   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_GFSATM_NC'
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk,kr
   integer(i_kind) :: ier,istatus,iredundant
   integer(i_kind) :: latb, lonb, levs
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   integer(i_kind),dimension(6):: idate
   integer(i_kind),dimension(4):: odate
   integer(i_kind) :: nread,iworld,iworld_group,mpi_comm_read
   integer(i_kind),dimension(npe):: mype_read,mype_read_max,mype_read_rank
   real(r_kind),allocatable,dimension(:) :: fhour

   real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
   real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
        grid_vor, grid_div, grid_b, grid_b2
   real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
   real(r_single),allocatable,dimension(:,:,:) :: rwork3d0, rwork3d1
   real(r_single),allocatable,dimension(:,:) :: rwork2d
   real(r_kind),allocatable,dimension(:)   :: work, work_v
   real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
   real(r_kind),allocatable,dimension(:) :: rlats_tmp,rlons_tmp

   logical :: procuse,diff_res,eqspace
   type(egrid2agrid_parm) :: p_high
   logical,dimension(1) :: vector
   type(Dataset) :: filges
   type(Dimension) :: ncdim
   logical :: read_2m, read_z

   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.

   if (filename(1:3) == 'sfc') then
        read_2m = .true.
        read_z = .false.
        if ( mype == 0 ) write(6,* ) &
            trim(my_name), ': reading 2m variables from ', trim(filename)
   else
       read_2m = .false.
       read_z = zflag
       if ( mype == 0 ) write(6,* ) &
             trim(my_name), ': reading atmos variables from ', trim(filename)
   endif

   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount

   mype_read=-1
   if (procuse) mype_read(mype+1) = mype

   mype_read_max = -1
   call mpi_allreduce(mype_read,mype_read_max,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

   nread=0
   mype_read_rank=-1
   do i=1,npe
      if (mype_read_max(i)>=0) then
         nread=nread+1
         mype_read_rank(nread)=mype_read_max(i)
      endif
   end do

   call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

   allocate( work(grd%itotsub),work_v(grd%itotsub) )
   work=zero
   work_v=zero

   if ( procuse ) then

      filges = open_dataset(filename, paropen=.true., mpicomm=mpi_comm_read)

      ! get dimension sizes
      ncdim = get_dim(filges, 'grid_xt'); lonb = ncdim%len
      ncdim = get_dim(filges, 'grid_yt'); latb = ncdim%len
      if (.not.  read_2m) &
      ncdim = get_dim(filges, 'pfull'); levs = ncdim%len

      ! get time information
      idate = get_idate_from_time_units(filges)
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year
      call read_vardata(filges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from
                                               ! Jeff Whitaker
      fhour = real(nint(fhour),r_kind)

      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

      diff_res=.false.
      if ( latb /= nlatm2 ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
         !call stop2(101)
      endif
      if ( lonb /= grd%nlon ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
         !call stop2(101)
      endif
      if  (.not. read_2m)  then
          if ( levs /= grd%nsig ) then
             if ( mype == 0 ) write(6, &
                '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
                trim(my_name),grd%nsig,levs
             call stop2(101)
          endif
      endif

      allocate( spec_vor(sp_a%nc), spec_div(sp_a%nc) )
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if ( diff_res ) then
         allocate(grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate(grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      endif
      allocate(rwork3d0(lonb,latb,1))
      allocate(rwork3d1(lonb,latb,1))
      allocate(rwork2d(lonb,latb))
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb))
      call read_vardata(filges, 'grid_xt', rlons_tmp)
      call read_vardata(filges, 'grid_yt', rlats_tmp)
      do j=1,latb
        rlats(latb+2-j)=deg2rad*rlats_tmp(j)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*rlons_tmp(j)
      end do
      deallocate(rlats_tmp,rlons_tmp)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      enddo

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

   endif ! if ( procuse )

   ! Get pointer to relevant variables (this should be made flexible and general)
   if (.not. read_2m) then
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_nc: ERROR'
             write(6,*) 'cannot handle having both sf and div'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_nc: ERROR'
             write(6,*) 'cannot handle having both vp and vor'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_nc: ERROR'
             write(6,*) 'cannot handle having both t and tv'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif

       istatus=0
       call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
       if ( istatus /= 0 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_nc: ERROR'
             write(6,*) 'Missing some of the required fields'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
   else ! read 2m vars
       istatus=0
       call gsi_bundlegetpointer(gfs_bundle,'t2m',g_t2m  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'q2m',g_q2m  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
       if ( istatus /= 0 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_nc: ERROR'
             write(6,*) 'Missing 2m required variables'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
   endif
   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   icount=0

   !   Process guess fields according to type of input file.   NCEP_SIGIO files
   !   are spectral coefficient files and need to be transformed to the grid.
   !   Once on the grid, fields need to be scattered from the full domain to
   !   sub-domains.

   !  Only read Terrain when read_z is true.
   if ( read_z ) then

      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1

      ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      if (mype==mype_use(icount)) then
         ! read hs
         call read_vardata(filges, 'hgtsfc', rwork2d)
         if ( diff_res ) then
            grid_b=rwork2d
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=rwork2d
            call general_fill_ns(grd,grid,work)
         endif
      endif
       if ( icount == icm  ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif
   endif

   if (.not. read_2m)  then

       icount=icount+1
       iflag(icount)=2
       ilev(icount)=1

       ! Surface pressure:  same procedure as terrain
       if (mype==mype_use(icount)) then
          ! read ps
          call read_vardata(filges, 'pressfc', rwork2d)
          rwork2d = r0_001*rwork2d ! convert Pa to cb
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif
       if ( icount == icm ) then
          call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
               icount,iflag,ilev,work,uvflag,vordivflag)
       endif

       !   Thermodynamic variable:  s-->g transform, communicate to all tasks
       !   For multilevel fields, each task handles a given level.  Periodic
       !   mpi_alltoallv calls communicate the grids to all mpi tasks.
       !   Finally, the grids are loaded into guess arrays used later in the
       !   code.

       do k=1,nlevs

          icount=icount+1
          iflag(icount)=3
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'spfh', rwork3d1, nslice=kr, slicedim=3)
             call read_vardata(filges, 'tmp', rwork3d0, nslice=kr, slicedim=3)
             rwork2d = rwork3d0(:,:,1) * (one+fv*rwork3d1(:,:,1))
             if ( diff_res ) then
                grid_b=rwork2d
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork2d
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          endif
       end do

       if ( vordivflag .or. .not. uvflag ) then
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip
             icount=icount+1
             iflag(icount)=4
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                ! Vorticity
                ! Convert grid u,v to div and vor
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work_v(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid_v(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
                allocate( grid_vor(grd%nlon,nlatm2))
                call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
                call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)
                ! Load values into rows for south and north pole
                call general_fill_ns(grd,grid_vor,work)
                deallocate(grid_vor)
             endif
             if ( icount == icm ) then
                call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                     icount,iflag,ilev,work,uvflag,vordivflag)
             endif

          end do
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip

             icount=icount+1
             iflag(icount)=5
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                ! Divergence
                ! Convert grid u,v to div and vor
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work_v(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid_v(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
                allocate( grid_div(grd%nlon,nlatm2) )
                call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
                call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
                ! Load values into rows for south and north pole
                call general_fill_ns(grd,grid_div,work)
                deallocate(grid_div)
             endif
             if ( icount == icm ) then
                call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                     icount,iflag,ilev,work,uvflag,vordivflag)
             endif

          end do
       endif ! if ( vordivflag .or. .not. uvflag )
       if ( uvflag ) then
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip
             icount=icount+1
             iflag(icount)=6
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)

                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
             endif
             if ( icount == icm ) then
                call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                     icount,iflag,ilev,work,uvflag,vordivflag)
             endif

             icount=icount+1
             iflag(icount)=7
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                ! V
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   ! Note work_v and work are switched because output must be in work.
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work_v,work)
                endif
             endif
             if ( icount == icm ) then
                call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                     icount,iflag,ilev,work,uvflag,vordivflag)
             endif
          end do
       endif ! if ( uvflag )
       do k=1,nlevs
          kr = levs+1-k ! netcdf is top to bottom, need to flip
          icount=icount+1
          iflag(icount)=8
          ilev(icount)=k

          if (mype==mype_use(icount)) then
             ! Specific humidity
             call read_vardata(filges, 'spfh', rwork3d0, nslice=kr, slicedim=3)
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid = rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          endif
       end do
       do k=1,nlevs
          kr = levs+1-k ! netcdf is top to bottom, need to flip
       
          icount=icount+1
          iflag(icount)=9
          ilev(icount)=k

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'o3mr', rwork3d0, nslice=kr, slicedim=3)
             ! Ozone mixing ratio
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                  icount,iflag,ilev,work,uvflag,vordivflag)
          endif
       end do

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=10
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'clwmr', rwork3d0, nslice=kr, slicedim=3)
             call read_vardata(filges, 'icmr', rwork3d1, nslice=kr, slicedim=3)
             ! Cloud condensate mixing ratio.
             rwork2d = rwork3d0(:,:,1)+rwork3d1(:,:,1)
             if ( diff_res ) then
                grid_b=rwork2d
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork2d
                call general_fill_ns(grd,grid,work)
             endif

          endif

             if ( icount == icm .or. k == nlevs ) then
                call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                     icount,iflag,ilev,work,uvflag,vordivflag)
             endif

       enddo ! do k=1,nlevs
   else ! read_2m

       icount=icount+1
       iflag(icount)=2

       ! 2m temperature from sfc file
       if (mype==mype_use(icount)) then
          call read_vardata(filges, 'tmp2m', rwork2d)
          
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       icount=icount + 1
       iflag(icount)=3

       ! 2m humidity from sfc file
       if (mype==mype_use(icount)) then
          call read_vardata(filges, 'spfh2m', rwork2d)

          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       icount=icount + 1
       iflag(icount)=4

       if (mype==mype_use(icount)) then
          ! read ps 
          call read_vardata(filges, 'pressfc', rwork2d)
          rwork2d = r0_001*rwork2d ! convert Pa to cb
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       ! not using all procs. doesn't trigger. todo: figure out trigger 
       ! for when reading fewer vars.
       !if ( icount == icm ) then
          call general_reload_sfc(grd,g_t2m, g_q2m, g_ps, icount,iflag,work)
       !endif

   endif ! read_2m

   if ( procuse ) then
      if ( diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
      call destroy_egrid2agrid(p_high)
      deallocate(spec_div,spec_vor)
      deallocate(rwork3d1,rwork3d0,clons,slons)
      deallocate(rwork2d)
      deallocate(grid,grid_v)
      call close_dataset(filges)
   endif
   deallocate(work)

   ! Convert dry temperature to virtual temperature
   !do k=1,grd%nsig
   !   do j=1,grd%lon2
   !      do i=1,grd%lat2
   !         g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
   !      enddo
   !   enddo
   !enddo

   ! Load u->div and v->vor slot when uv are used instead
   if ( .not. read_2m ) then
       if ( uvflag ) then
          call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
          if ( ier == 0 ) then
             ptr3d=g_u
             call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
             if ( ier == 0 ) ptr3d=g_v
          else ! in this case, overload: return u/v in sf/vp slot
             call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
             if ( ier == 0 ) then
                ptr3d=g_u
                call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
                if ( ier == 0 ) ptr3d=g_v
             endif
          endif
       else ! in this case, overload: return u/v in sf/vp slot
          call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
          if ( ier == 0 ) ptr3d=g_u
          call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
          if ( ier == 0 ) ptr3d=g_v
       endif
   endif ! read_2m
   if (read_z) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0 ) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)

   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,trim(filename)
700   format('GENERAL_READ_GFSATM_NC: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

end subroutine general_read_gfsatm_nc

subroutine general_read_gfsatm_allhydro_nc(grd,sp_a,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm_nc  adaptation of read_gfsatm for general resolutions
!   prgmmr: martin          org: NCEP/EMC                date: 2019-09-25
!
! abstract: copied from read_gfsatm_nems
!
! program history log:
!   2019-09-25  martin     - original; copied from general_read_gfsatm_nems
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input netcdf file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!
!   output argument list:
!     gfs_bundle  - bundle carrying guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use mpimod, only: mype
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                          close_dataset, get_dim, read_vardata,get_idate_from_time_units
   use gfsreadmod, only: general_reload2, general_reload_sfc
   use ncepnems_io, only: imp_physics

   implicit none

   ! Declare local parameters
   real(r_kind),parameter:: r0_001 = 0.001_r_kind

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:)       :: g_t2m, g_q2m
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_q,g_oz,g_tv
   real(r_kind),pointer,dimension(:,:,:)     :: g_ql,g_qi,g_qr,g_qs,g_qg,g_ni,g_nr

   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_GFSATM_ALLHYDRO_NC'
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk,kr
   integer(i_kind) :: ier,istatus,istatus1,iredundant
   integer(i_kind) :: latb, lonb, levs
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   integer(i_kind),dimension(6):: idate
   integer(i_kind),dimension(4):: odate
   real(r_kind),allocatable,dimension(:) :: fhour

   real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
   real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
        grid_vor, grid_div, grid_b, grid_b2
   real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
   real(r_single),allocatable,dimension(:,:,:) :: rwork3d0, rwork3d1
   real(r_single),allocatable,dimension(:,:) :: rwork2d
   real(r_kind),allocatable,dimension(:)   :: work, work_v
   real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
   real(r_kind),allocatable,dimension(:) :: rlats_tmp,rlons_tmp

   logical :: procuse,diff_res,eqspace
   type(egrid2agrid_parm) :: p_high
   logical,dimension(1) :: vector
   type(Dataset) :: filges
   type(Dimension) :: ncdim
   logical :: read_2m, read_z



   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.

   if (filename(1:3) == 'sfc') then
        read_2m = .true.
        read_z = .false.
        if ( mype == 0 ) write(6,* ) &
            trim(my_name), ': reading 2m variables from ', trim(filename)
   else
       read_2m = .false.
       read_z = zflag
       if ( mype == 0 ) write(6,* ) &
             trim(my_name), ': reading atmos variables from ', trim(filename)
   endif

   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount
   allocate( work(grd%itotsub))
   work=zero

   if ( procuse ) then

      filges = open_dataset(filename, paropen=.true.)

      ! get dimension sizes
      ncdim = get_dim(filges, 'grid_xt'); lonb = ncdim%len
      ncdim = get_dim(filges, 'grid_yt'); latb = ncdim%len
      if (.not.  read_2m) &
      ncdim = get_dim(filges, 'pfull'); levs = ncdim%len

      ! get time information
      idate = get_idate_from_time_units(filges)
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year
      call read_vardata(filges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from
                                               ! Jeff Whitaker
      fhour = real(nint(fhour),r_kind)

      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

      diff_res=.false.
      if ( latb /= nlatm2 ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
         !call stop2(101)
      endif
      if ( lonb /= grd%nlon ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
         !call stop2(101)
      endif
      if  (.not. read_2m)  then
          if ( levs /= grd%nsig ) then
             if ( mype == 0 ) write(6, &
                '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
                trim(my_name),grd%nsig,levs
             call stop2(101)
          endif
      endif

      allocate( spec_vor(sp_a%nc), spec_div(sp_a%nc) )
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if ( diff_res ) then
         allocate(grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate(grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      endif
      allocate(rwork3d0(lonb,latb,1))
      allocate(rwork3d1(lonb,latb,1))
      allocate(rwork2d(lonb,latb))
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb))
      call read_vardata(filges, 'grid_xt', rlons_tmp)
      call read_vardata(filges, 'grid_yt', rlats_tmp)
      do j=1,latb
        rlats(latb+2-j)=deg2rad*rlats_tmp(j)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*rlons_tmp(j)
      end do
      deallocate(rlats_tmp,rlons_tmp)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      enddo

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

   endif ! if ( procuse )

   ! Get pointer to relevant variables (this should be made flexible and general)
   if (.not. read_2m) then
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_allhydro_nc: ERROR'
             write(6,*) 'cannot handle having both sf and div'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_allhydro_nc: ERROR'
             write(6,*) 'cannot handle having both vp and vor'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
       iredundant=0
       call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
       if ( ier == 0 ) iredundant = iredundant + 1
       if ( iredundant==2 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_allhydro_nc: ERROR'
             write(6,*) 'cannot handle having both t and tv'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
       istatus=0
       call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
    !  call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
       istatus1=0   
       call gsi_bundlegetpointer(gfs_bundle,'ql',g_ql  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'qi',g_qi  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'qr',g_qr  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'qs',g_qs  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'qg',g_qg  ,ier);istatus1=istatus1+ier
    !  call gsi_bundlegetpointer(gfs_bundle,'cf',g_cf  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'ni',g_ni  ,ier);istatus1=istatus1+ier
       call gsi_bundlegetpointer(gfs_bundle,'nr',g_nr  ,ier);istatus1=istatus1+ier
       if ( istatus1 /= 0 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_allhydro_nc: ERROR'
             write(6,*) 'Missing some of the required hydrometeor fields for imp_physics = ', imp_physics
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
   else ! read 2m vars
       istatus=0
       call gsi_bundlegetpointer(gfs_bundle,'t2m',g_t2m  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'q2m',g_q2m  ,ier);istatus=istatus+ier
       call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
       if ( istatus /= 0 ) then
          if ( mype == 0 ) then
             write(6,*) 'general_read_gfsatm_allhydro_nc: ERROR'
             write(6,*) 'Missing 2m required variables'
             write(6,*) 'Aborting ... '
          endif
          call stop2(999)
       endif
   endif

   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   icount=0

   !   Process guess fields according to type of input file.   NCEP_SIGIO files
   !   are spectral coefficient files and need to be transformed to the grid.
   !   Once on the grid, fields need to be scattered from the full domain to
   !   sub-domains.

   !  Only read Terrain when zflag is true.

   if ( read_z ) then

      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1

      ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      if (mype==mype_use(icount)) then
         ! read hs
         call read_vardata(filges, 'hgtsfc', rwork2d)
         if ( diff_res ) then
            grid_b=rwork2d
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=rwork2d
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
      endif
   endif

   if (.not. read_2m)  then

       allocate( work_v(grd%itotsub) )
       work_v=zero

       icount=icount+1
       iflag(icount)=2
       ilev(icount)=1

       ! Surface pressure:  same procedure as terrain
       if (mype==mype_use(icount)) then
          ! read ps
          call read_vardata(filges, 'pressfc', rwork2d)
          rwork2d = r0_001*rwork2d ! convert Pa to cb
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif
       if ( icount == icm ) then
          call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
               g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
       endif

       !   Thermodynamic variable:  s-->g transform, communicate to all tasks
       !   For multilevel fields, each task handles a given level.  Periodic
       !   mpi_alltoallv calls communicate the grids to all mpi tasks.
       !   Finally, the grids are loaded into guess arrays used later in the
       !   code.

       do k=1,nlevs

          icount=icount+1
          iflag(icount)=3
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'spfh', rwork3d1, nslice=kr, slicedim=3)
             call read_vardata(filges, 'tmp', rwork3d0, nslice=kr, slicedim=3)
             rwork2d = rwork3d0(:,:,1) * (one+fv*rwork3d1(:,:,1))
             if ( diff_res ) then
                grid_b=rwork2d
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork2d
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       end do

       if ( vordivflag .or. .not. uvflag ) then
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip
             icount=icount+1
             iflag(icount)=4
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                ! Vorticity
                ! Convert grid u,v to div and vor
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work_v(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid_v(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
                allocate( grid_vor(grd%nlon,nlatm2))
                call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
                call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)
                ! Load values into rows for south and north pole
                call general_fill_ns(grd,grid_vor,work)
                deallocate(grid_vor)
             endif
             if ( icount == icm ) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
             endif
          end do
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip

             icount=icount+1
             iflag(icount)=5
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                ! Divergence
                ! Convert grid u,v to div and vor
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work_v(kk)=grid2(i,j,1)
                   enddo
                   do j=1,grd%nlon
                      do i=2,grd%nlat-1
                         grid_v(j,grd%nlat-i)=grid2(i,j,1)
                      enddo
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
                allocate( grid_div(grd%nlon,nlatm2) )
                call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
                call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
                ! Load values into rows for south and north pole
                call general_fill_ns(grd,grid_div,work)
                deallocate(grid_div)
             endif
             if ( icount == icm ) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
             endif

          end do
       endif ! if ( vordivflag .or. .not. uvflag )

       if ( uvflag ) then
          do k=1,nlevs
             kr = levs+1-k ! netcdf is top to bottom, need to flip
             icount=icount+1
             iflag(icount)=6
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)

                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
                endif
             endif
             if ( icount == icm ) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
             endif

             icount=icount+1
             iflag(icount)=7
             ilev(icount)=k

             if (mype==mype_use(icount)) then
                ! V
                call read_vardata(filges, 'ugrd', rwork3d0, nslice=kr, slicedim=3)
                call read_vardata(filges, 'vgrd', rwork3d1, nslice=kr, slicedim=3)
                if ( diff_res ) then
                   grid_b = rwork3d0(:,:,1)
                   grid_b2 = rwork3d1(:,:,1)
                   vector(1)=.true.
                   call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
                   call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid = rwork3d0(:,:,1)
                   grid_v = rwork3d1(:,:,1)
                   ! Note work_v and work are switched because output must be in work.
                   call general_filluv_ns(grd,slons,clons,grid,grid_v,work_v,work)
                endif
             endif
             if ( icount == icm ) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
             endif
          end do
       endif ! if ( uvflag )

       do k=1,nlevs
          kr = levs+1-k ! netcdf is top to bottom, need to flip
          icount=icount+1
          iflag(icount)=8
          ilev(icount)=k

          if (mype==mype_use(icount)) then
             ! Specific humidity
             call read_vardata(filges, 'spfh', rwork3d0, nslice=kr, slicedim=3)
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid = rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       end do
      
       do k=1,nlevs
          kr = levs+1-k ! netcdf is top to bottom, need to flip
       
          icount=icount+1
          iflag(icount)=9
          ilev(icount)=k

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'o3mr', rwork3d0, nslice=kr, slicedim=3)
             ! Ozone mixing ratio
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       end do

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=10
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'clwmr', rwork3d0, nslice=kr, slicedim=3)
             ! Cloud liquid water mixing ratio.
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif

          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       enddo ! do k=1,nlevs

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=11
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'icmr', rwork3d0, nslice=kr, slicedim=3)
             ! Cloud ice water mixing ratio. 
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       enddo ! do k=1,nlevs

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=12
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'rwmr', rwork3d0, nslice=kr, slicedim=3)
             ! Rain water mixing ratio. 
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       enddo ! do k=1,nlevs

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=13
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'snmr', rwork3d0, nslice=kr, slicedim=3)
             ! Snow water mixing ratio. 
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm ) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       enddo ! do k=1,nlevs

       do k=1,nlevs
          icount=icount+1
          iflag(icount)=14
          ilev(icount)=k
          kr = levs+1-k ! netcdf is top to bottom, need to flip

          if (mype==mype_use(icount)) then
             call read_vardata(filges, 'grle', rwork3d0, nslice=kr, slicedim=3)
             ! Graupel mixing ratio. 
             if ( diff_res ) then
                grid_b=rwork3d0(:,:,1)
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,grd%itotsub
                   i=grd%ltosi_s(kk)
                   j=grd%ltosj_s(kk)
                   work(kk)=grid2(i,j,1)
                enddo
             else
                grid=rwork3d0(:,:,1)
                call general_fill_ns(grd,grid,work)
             endif
          endif
          if ( icount == icm .or. k==nlevs) then
             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
          endif
       enddo ! do k=1,nlevs

       ! Read fields specific to Thompson microphysics
       if (imp_physics == 8) then
          do k=1,nlevs
             icount=icount+1
             iflag(icount)=15
             ilev(icount)=k
             kr = levs+1-k ! netcdf is top to bottom, need to flip

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'nccice', rwork3d0, nslice=kr, slicedim=3)
                ! cloud ice water number concentration.
                if ( diff_res ) then
                   grid_b=rwork3d0(:,:,1)
                   vector(1)=.false.
                   call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid=rwork3d0(:,:,1)
                   call general_fill_ns(grd,grid,work)
                endif
             endif
             if ( icount == icm .or. k==nlevs ) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
             endif
          enddo ! do k=1,nlevs

          do k=1,nlevs
             icount=icount+1
             iflag(icount)=16
             ilev(icount)=k
             kr = levs+1-k ! netcdf is top to bottom, need to flip

             if (mype==mype_use(icount)) then
                call read_vardata(filges, 'nconrd', rwork3d0, nslice=kr, slicedim=3)
                ! rain number concentration.
                if ( diff_res ) then
                   grid_b=rwork3d0(:,:,1)
                   vector(1)=.false.
                   call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                   call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                   do kk=1,grd%itotsub
                      i=grd%ltosi_s(kk)
                      j=grd%ltosj_s(kk)
                      work(kk)=grid2(i,j,1)
                   enddo
                else
                   grid=rwork3d0(:,:,1)
                   call general_fill_ns(grd,grid,work)
                endif
             endif
             if ( icount == icm .or. k==nlevs) then
                call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                     g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_ni,g_nr)
                endif
          enddo ! do k=1,nlevs
       endif ! imp_physics
!       do k=1,nlevs
!          icount=icount+1
!          iflag(icount)=17
!          ilev(icount)=k
!          kr = levs+1-k ! netcdf is top to bottom, need to flip
!
!          if (mype==mype_use(icount)) then
!             call read_vardata(filges, 'cld_amt', rwork3d0, nslice=kr, slicedim=3)
!             ! Cloud amount (cloud fraction). 
!             if ( diff_res ) then
!                grid_b=rwork3d0(:,:,1)
!                vector(1)=.false.
!                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
!                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
!                do kk=1,grd%itotsub
!                   i=grd%ltosi_s(kk)
!                   j=grd%ltosj_s(kk)
!                   work(kk)=grid2(i,j,1)
!                enddo
!             else
!                grid=rwork3d0(:,:,1)
!                call general_fill_ns(grd,grid,work)
!             endif
!          endif
!          if ( icount == icm .or. k==nlevs ) then
!             call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
!                  g_ql,g_qi,g_qr,g_qs,g_qg,icount,iflag,ilev,work,uvflag,vordivflag,g_cf)
!          endif
!       enddo ! do k=1,nlevs

   else ! read_2m

       icount=icount+1
       iflag(icount)=2

       ! 2m temperature from sfc file
       if (mype==mype_use(icount)) then
          call read_vardata(filges, 'tmp2m', rwork2d)
          
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       icount=icount + 1
       iflag(icount)=3

       ! 2m humidity from sfc file
       if (mype==mype_use(icount)) then
          call read_vardata(filges, 'spfh2m', rwork2d)

          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       icount=icount + 1
       iflag(icount)=4

       if (mype==mype_use(icount)) then
          ! read ps 
          call read_vardata(filges, 'pressfc', rwork2d)
          rwork2d = r0_001*rwork2d ! convert Pa to cb
          if ( diff_res ) then
             vector(1)=.false.
             grid_b=rwork2d
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,grd%itotsub
                i=grd%ltosi_s(kk)
                j=grd%ltosj_s(kk)
                work(kk)=grid2(i,j,1)
             enddo
          else
             grid=rwork2d
             call general_fill_ns(grd,grid,work)
          endif
       endif

       ! not necessarily using all assigned tasks (fewer vars), so below doesn't trigger. 
       ! todo: figure out what icm should be here.
       !if ( icount == icm ) then
          call general_reload_sfc(grd,g_t2m, g_q2m, g_ps, icount,iflag,work)
       !endif

   endif ! read_2m



   if ( procuse ) then
      if ( diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
      call destroy_egrid2agrid(p_high)
      deallocate(spec_div,spec_vor)
      deallocate(rwork3d1,rwork3d0,clons,slons)
      deallocate(rwork2d)
      deallocate(grid,grid_v)
      call close_dataset(filges)
   endif
   deallocate(work)
   if (allocated(work_v)) deallocate(work_v)

   ! Convert dry temperature to virtual temperature
   !do k=1,grd%nsig
   !   do j=1,grd%lon2
   !      do i=1,grd%lat2
   !         g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
   !      enddo
   !   enddo
   !enddo

   ! Load u->div and v->vor slot when uv are used instead
   if ( .not. read_2m ) then
       if ( uvflag ) then
          call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
          if ( ier == 0 ) then
             ptr3d=g_u
             call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
             if ( ier == 0 ) ptr3d=g_v
          else ! in this case, overload: return u/v in sf/vp slot
             call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
             if ( ier == 0 ) then
                ptr3d=g_u
                call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
                if ( ier == 0 ) ptr3d=g_v
             endif
          endif
       else ! in this case, overload: return u/v in sf/vp slot
          call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
          if ( ier == 0 ) ptr3d=g_u
          call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
          if ( ier == 0 ) ptr3d=g_v
       endif
   endif !read_2m
   if (read_z) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0 ) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)

   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,trim(filename)
700   format('GENERAL_READ_GFSATM_ALLHYDRO_NC: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

end subroutine general_read_gfsatm_allhydro_nc
subroutine general_fill_ns(grd,grid_in,grid_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero,one
   use general_sub2grid_mod, only: sub2grid_info
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info),                           intent(in   ) :: grd
   real(r_kind),dimension(grd%nlon,grd%nlat-2),   intent(in   ) :: grid_in  ! input grid
   real(r_kind),dimension(grd%itotsub),           intent(  out) :: grid_out ! output grid

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
   ! Declare local variables
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) rnlon,sumn,sums

   ! Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlatm2)
   enddo
   rnlon=one/real(grd%nlon,r_kind)
   sumn=sumn*rnlon
   sums=sums*rnlon

   ! Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      if ( j == grd%nlat-1) then
        grid_out(k)=sums
      elseif ( j == 0) then
        grid_out(k) = sumn
      else
        i=grd%ltosj_s(k)
        grid_out(k)=grid_in(i,j)
      endif
   enddo

   return
end subroutine general_fill_ns

subroutine general_filluv_ns(grd,slons,clons,gridu_in,gridv_in,gridu_out,gridv_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info),                        intent(in   ) :: grd
   real(r_kind),dimension(grd%nlon),           intent(in   ) :: slons,clons
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub),        intent(  out) :: gridu_out,gridv_out ! output grid

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
   ! Declare local variables
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


   ! Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*clons(i)-gridv_in(i,1     )*slons(i)
      polnv=polnv+gridu_in(i,1     )*slons(i)+gridv_in(i,1     )*clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*clons(i)+gridv_in(i,nlatm2)*slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*slons(i)-gridv_in(i,nlatm2)*clons(i)
   enddo
   polnu=polnu/real(grd%nlon,r_kind)
   polnv=polnv/real(grd%nlon,r_kind)
   polsu=polsu/real(grd%nlon,r_kind)
   polsv=polsv/real(grd%nlon,r_kind)

   ! Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if ( j == grd%nlat-1 ) then
        gridu_out(k) = polsu*clons(i)+polsv*slons(i)
        gridv_out(k) = polsu*slons(i)-polsv*clons(i)
      elseif ( j == 0) then
        gridu_out(k) = polnu*clons(i)+polnv*slons(i)
        gridv_out(k) = -polnu*slons(i)+polnv*clons(i)
      else
        gridu_out(k)=gridu_in(i,j)
        gridv_out(k)=gridv_in(i,j)
      endif
   enddo

   return
end subroutine general_filluv_ns
subroutine general_fillu_ns(grd,sp,gridu_in,gridv_in,gridu_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info),                        intent(in   ) :: grd
   type(spec_vars),                            intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub),        intent(  out) :: gridu_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the u input grid.  The southern row contains
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
!   2014-12-03  derber     - create specialized routine to just update u
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
   ! Declare local variables
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


   ! Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*sp%clons(i)-gridv_in(i,1     )*sp%slons(i)
      polnv=polnv+gridu_in(i,1     )*sp%slons(i)+gridv_in(i,1     )*sp%clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*sp%clons(i)+gridv_in(i,nlatm2)*sp%slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*sp%slons(i)-gridv_in(i,nlatm2)*sp%clons(i)
   enddo
   polnu=polnu/real(grd%nlon,r_kind)
   polnv=polnv/real(grd%nlon,r_kind)
   polsu=polsu/real(grd%nlon,r_kind)
   polsv=polsv/real(grd%nlon,r_kind)

   ! Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if ( j == grd%nlat-1 ) then
        gridu_out(k) = polsu*sp%clons(i)+polsv*sp%slons(i)
      elseif ( j == 0) then
        gridu_out(k) = polnu*sp%clons(i)+polnv*sp%slons(i)
      else
        gridu_out(k)=gridu_in(i,j)
      endif
   enddo

   return
end subroutine general_fillu_ns

subroutine general_fillv_ns(grd,sp,gridu_in,gridv_in,gridv_out)

! !USES:

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   implicit none

! !INPUT PARAMETERS:

   type(sub2grid_info),                        intent(in   ) :: grd
   type(spec_vars),                            intent(in   ) :: sp
   real(r_kind),dimension(grd%nlon,grd%nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real(r_kind),dimension(grd%itotsub),        intent(  out) :: gridv_out ! output grid

! !DESCRIPTION: This routine adds a southern and northern latitude
!               row to the v input grid.  The southern row contains
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
!   2014-12-03  derber     - create specialized routine to just update v
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
   ! Declare local variables
   integer(i_kind) i,j,k,nlatm2
   real(r_kind) polnu,polnv,polsu,polsv


   ! Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm2=grd%nlat-2
   do i=1,grd%nlon
      polnu=polnu+gridu_in(i,1     )*sp%clons(i)-gridv_in(i,1     )*sp%slons(i)
      polnv=polnv+gridu_in(i,1     )*sp%slons(i)+gridv_in(i,1     )*sp%clons(i)
      polsu=polsu+gridu_in(i,nlatm2)*sp%clons(i)+gridv_in(i,nlatm2)*sp%slons(i)
      polsv=polsv+gridu_in(i,nlatm2)*sp%slons(i)-gridv_in(i,nlatm2)*sp%clons(i)
   enddo
   polnu=polnu/real(grd%nlon,r_kind)
   polnv=polnv/real(grd%nlon,r_kind)
   polsu=polsu/real(grd%nlon,r_kind)
   polsv=polsv/real(grd%nlon,r_kind)

   ! Transfer local work array to output grid
   do k=1,grd%itotsub
      j=grd%nlat-grd%ltosi_s(k)
      i=grd%ltosj_s(k)
      if ( j == grd%nlat-1 ) then
        gridv_out(k) = polsu*sp%slons(i)-polsv*sp%clons(i)
      elseif ( j == 0) then
        gridv_out(k) = -polnu*sp%slons(i)+polnv*sp%clons(i)
      else
        gridv_out(k)=gridv_in(i,j)
      endif
   enddo

   return
end subroutine general_fillv_ns
