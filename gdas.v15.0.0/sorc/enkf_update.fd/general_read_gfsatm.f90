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
         if ( iret_read /= 0 ) goto 1000
      endif
      if ( nlevs /= sighead%levs ) goto 1000
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

   ! ERROR detected while reading file
1000 continue
   write(6,*)'GENERAL_READ_GFSATM:  ***ERROR*** reading ',&
         trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
   return

   ! End of routine.  Return

    return
end subroutine general_read_gfsatm

subroutine general_read_gfsatm_nems(grd,sp_a,filename,uvflag,vordivflag,zflag, &
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
   use gridmod, only: ntracer,ncloud,itotsub,jcap_b
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns,ltosj_s,ltosi_s
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer

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
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_cwmr,g_q,g_oz,g_tv

   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_GFSATM_NEMS'
   character(len=1)   :: null = ' '
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk
   integer(i_kind) :: ier,istatus,iredundant
   integer(i_kind) :: latb, lonb, levs, nframe
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

   logical :: procuse,diff_res,eqspace
   type(nemsio_gfile) :: gfile
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

      fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
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
   call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=ier
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
   call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
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
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
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
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
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
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
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
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
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
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
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
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
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
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
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
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=10
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud condensate mixing ratio.
         work=zero
         call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
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

         if ( icount == icm .or. k == nlevs ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
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

   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,trim(filename)
700   format('GENERAL_READ_GFSATM_NEMS: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

   ! ERROR detected while reading file
1000 continue
   write(6,*)'GENERAL_READ_GFSATM_NEMS:  ***ERROR*** reading ',&
       trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
   return

   ! End of routine.  Return

   return
end subroutine general_read_gfsatm_nems

subroutine general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vdflag)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype,mype
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
      endif
   enddo ! do k=1,icount

   icount=0
   ilev=0
   iflag=0

   return

end subroutine general_reload

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
   rnlon=one/float(grd%nlon)
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
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

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
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

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
   polnu=polnu/float(grd%nlon)
   polnv=polnv/float(grd%nlon)
   polsu=polsu/float(grd%nlon)
   polsv=polsv/float(grd%nlon)

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

subroutine preproc_read_gfsatm(grd,filename,iret)

   use kinds, only: r_kind,i_kind
   use constants, only: zero
   use mpimod, only: mpi_comm_world,ierror,mype
   use mpimod, only: mpi_mode_rdonly,mpi_info_null,mpi_rtype,mpi_offset_kind
   use mpi, only: mpi_status_ignore
   use general_sub2grid_mod, only: sub2grid_info,general_grid2sub
   use gsi_bundlemod, only: gsi_bundle,gsi_bundlegetpointer

   implicit none

   type(sub2grid_info), intent(in   ) :: grd
   character(len=*),    intent(in   ) :: filename
   integer(i_kind),     intent(  out) :: iret

   real(r_kind),dimension(grd%lat2,grd%lon2) :: g_z,g_ps
   real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: &
                g_u,g_v,g_vor,g_div,g_cwmr,g_q,g_oz,g_tv

   real(r_kind),dimension(:,:,:,:),allocatable:: work_grd,work_sub
   integer(i_kind) :: count,lunges
   integer(i_kind) :: i,j,k,im,jm,km
   integer(mpi_offset_kind) :: offset
 
   ! Assume all goes well
   iret = 0

   im=grd%lat2
   jm=grd%lon2
   km=grd%nsig

   allocate(work_grd(grd%inner_vars,grd%nlat,grd%nlon,grd%kbegin_loc:grd%kend_alloc))

   call mpi_file_open(mpi_comm_world,trim(adjustl(filename)), &
                      mpi_mode_rdonly,mpi_info_null,lunges,ierror)
   if ( ierror /= 0 ) then
      write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_OPEN failed on task = ', mype, ' ierror = ', ierror
      iret = ierror
      goto 1000
   endif

   count  = grd%nlat * grd%nlon *  grd%nlevs_alloc
   offset = grd%nlat * grd%nlon * (grd%kbegin_loc-1) * r_kind
   call mpi_file_read_at(lunges,offset,work_grd,count,mpi_rtype,mpi_status_ignore,ierror)
   if ( ierror /= 0 ) then
      write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_READ_AT failed on task = ', mype, ' ierror = ', ierror
      iret = ierror
      goto 1000
   endif

   call mpi_file_close(lunges,ierror)
   if ( ierror /= 0 ) then
      write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_CLOSE failed on task = ', mype, ' ierror = ', ierror
      iret = ierror
      goto 1000
   endif

   allocate(work_sub(grd%inner_vars,im,jm,grd%num_fields))

   call general_grid2sub(grd,work_grd,work_sub)

   deallocate(work_grd)

   !$omp parallel do schedule(dynamic,1) private(k,j,i)
   do k = 1,km
      do j = 1,jm
         do i = 1,im
         g_u(   i,j,k) = work_sub(1,i,j,k+0*km)
         g_v(   i,j,k) = work_sub(1,i,j,k+1*km)
         g_tv(  i,j,k) = work_sub(1,i,j,k+2*km)
         g_q(   i,j,k) = work_sub(1,i,j,k+3*km)
         g_oz(  i,j,k) = work_sub(1,i,j,k+4*km)
         g_cwmr(i,j,k) = work_sub(1,i,j,k+5*km)
         enddo
      enddo
   enddo

   g_vor = zero
   g_div = zero

   !$omp parallel do schedule(dynamic,1) private(j,i)
   do j = 1,jm
      do i = 1,im
         g_ps(i,j) = work_sub(1,i,j,grd%num_fields-1)
         g_z( i,j) = work_sub(1,i,j,grd%num_fields  )
      enddo
   enddo

   deallocate(work_sub)

   return

1000 continue

   write(6,*)'PREPROC_READ_GFSATM: ***ERROR*** reading ',&
              trim(filename),' IRET=',iret
   return

end subroutine preproc_read_gfsatm
