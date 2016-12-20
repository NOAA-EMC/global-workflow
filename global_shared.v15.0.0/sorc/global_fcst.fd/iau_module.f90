module iau_module

! module for iau data, io and time interpolation.

 use layout1, only: me, len_trie_ls, len_trio_ls, ls_dim, nodes, lats_node_r, &
                    lats_node_a, me_l_0
 use machine, only: kind_evod, kind_phys, kind_io4
 use namelist_def, only: iaufiles_fg,iaufiles_anl,iaufhrs,iau,iau_delthrs,ens_mem
 use resol_def, only: nlevs=>levs,ntrac,lonf,lonr,latg,num_p2d,num_a2d,num_a3d,num_p3d,latr,latr2
 use mpi_def, only: mc_comp

! iaufiles_fg: filenames for first-guess fields.
! iaufiles_anl: filenames for analysis fields or increment field if iaufiles_fg is empty
! iaufhrs: forecast hours for input files.
! iau_delthrs: length of iau window (in hours).

 implicit none
 private

 public :: init_iau, destroy_iau, getiauforcing

 real(kind_evod), dimension(:,:,:,:),allocatable ::  vrtspec_e,divspec_e,virtempspec_e,&
   vrtspec_o,divspec_o,virtempspec_o
 real(kind_evod), dimension(:,:,:,:),allocatable :: tracerspec_e,tracerspec_o
 real(kind_evod), dimension(:,:,:), allocatable :: lnpsspec_e,lnpsspec_o
 integer, public :: nfiles
 logical, public :: iau_initialized = .false.
 logical, public :: iauforcing = .false.

 contains

 subroutine init_iau(ls_node,ls_nodes,max_ls_nodes)

! read in first-guess and analysis files, compute and store increments in
! spectral space.
 
   integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
     max_ls_nodes(nodes)
   integer, allocatable, dimension(:) :: idt
   character(len=120) filename,filename2
   character(len=3) charmem
   real(kind=kind_evod) fhour,pdryini
   integer i,n,nfilesall,iprint,idate(4),iunit,iunit2,ierr
   logical       :: iauinc
   iau_initialized = .true.
   iauforcing = .true.
   nfilesall = size(iaufiles_anl)
   ! check is see if iaufiles_fg is empty,  if so, then iaufiles_anl are increments
   iauinc=.false.
   if (trim(iaufiles_fg(1)) .eq. '' .and. trim(iaufiles_anl(1)) .ne. '') then
      iauinc=.true.
   endif
  if (me .eq. me_l_0) print*,'iauinc=',iauinc
   nfiles = 0
   iprint = 1
!   iunit is the updated analysis
   iunit = 77
!   iunit2 is the background field
   iunit2 = 78
   do n=1,nfilesall
      if (me .eq. me_l_0) then
         print *,n,trim(adjustl(iaufiles_anl(n)))
         if (.not. iauinc) print *,n,trim(adjustl(iaufiles_fg(n)))
      endif
      filename = iaufiles_anl(n)
      if (trim(filename) .eq. '' .or. iaufhrs(n) .lt. 0) exit
      nfiles = nfiles + 1
   enddo
   if (me .eq. me_l_0) print *,'nfiles = ',nfiles
!  call mpi_barrier(mc_comp,ierr)
   if (nfiles < 1) then
     print *,'must be at least one file in iaufiles_anal'
     call mpi_quit(9999)
   endif
   allocate(idt(nfiles-1))
   idt = iaufhrs(2:nfiles)-iaufhrs(1:nfiles-1)
   do n=1,nfiles-1
      if (idt(n) .ne. iaufhrs(2)-iaufhrs(1)) then
        print *,'forecast intervals in iaufhrs must be constant'
        call mpi_quit(9999)
      endif
   enddo
   if (me .eq. me_l_0) print *,'iau interval = ',iau_delthrs,' hours'
   deallocate(idt)
   allocate(vrtspec_e(len_trie_ls,2,nlevs,nfiles),vrtspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(divspec_e(len_trie_ls,2,nlevs,nfiles),divspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(virtempspec_e(len_trie_ls,2,nlevs,nfiles),virtempspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(tracerspec_e(len_trie_ls,2,nlevs*ntrac,nfiles),tracerspec_o(len_trio_ls,2,nlevs*ntrac,nfiles))
   allocate(lnpsspec_e(len_trie_ls,2,nfiles),lnpsspec_o(len_trio_ls,2,nfiles))

   do n=1,nfiles
      filename = iaufiles_anl(n)
      if (ens_mem > 0) then
          write(charmem,'(i3.3)') ens_mem
          filename = trim(filename) // charmem
      end if
      if (.not. iauinc) then
          filename2 = iaufiles_fg(n)
          if (ens_mem > 0) then
              filename2 = trim(filename2) // charmem
         endif
      endif
      if (me .eq. me_l_0) print *,'reading ',trim(filename),trim(filename2)

      call treadeo_iau(iunit,iunit2,fhour,idate, &
                   lnpsspec_e(1,1,n), &
                   virtempspec_e(1,1,1,n), divspec_e(1,1,1,n), &
                   vrtspec_e(1,1,1,n), tracerspec_e(1,1,1,n), &
                   lnpsspec_o(1,1,n), &
                   virtempspec_o(1,1,1,n), divspec_o(1,1,1,n), &
                   vrtspec_o(1,1,1,n), tracerspec_o(1,1,1,n), &
                   ls_node,ls_nodes,max_ls_nodes, &
                   trim(filename),trim(filename2),iauinc)
   enddo
 end subroutine init_iau

 subroutine getiauforcing(vrt_e,div_e,virtemp_e,tracer_e,lnps_e,&
       vrt_o,div_o,virtemp_o,tracer_o,lnps_o,dtiau,t)
        
! compute an iau forcing by interpolating increments to model time set
! and dividing by length of iau window (in seconds).
      
   real(kind_evod), dimension(len_trie_ls,2,nlevs), intent(inout) :: &
     vrt_e,div_e,virtemp_e
   real(kind_evod), dimension(len_trio_ls,2,nlevs), intent(inout) :: &
     vrt_o,div_o,virtemp_o
   real(kind_evod), dimension(len_trie_ls,2,nlevs*ntrac), intent(inout) ::  &
     tracer_e
   real(kind_evod), dimension(len_trio_ls,2,nlevs*ntrac), intent(inout) ::  &
     tracer_o
   real(kind_evod), dimension(len_trie_ls,2), intent(inout) :: &
     lnps_e
   real(kind_evod), dimension(len_trio_ls,2), intent(inout) :: &
     lnps_o
   real(kind_evod), intent(in) :: t,dtiau
   real(kind_evod) delt, dt,t1,t2, wgt1,wgt2
   integer n
   dt = iau_delthrs*3600.
   ! set forcing to zero and return if outside iau window.
   if(me .eq. me_l_0)print *,' beginning of iau forcing ',t,iaufhrs(1), &
          iaufhrs(nfiles),nfiles
   if ( nfiles > 1) then  ! IAU forcing files bookend interval
      if (t <= iaufhrs(1)*3600. .or. t > iaufhrs(nfiles)*3600.) then
         if (me .eq. me_l_0) print *,'no iau forcing'
         if (t > iaufhrs(nfiles)*3600.) iauforcing = .false.
         return
      endif
      if (t .eq. 3600.*iaufhrs(nfiles)) then
!!$omp workshare
         wgt2=dtiau/dt
         vrt_e = vrt_e+vrtspec_e(:,:,:,nfiles)*wgt2
         div_e = div_e+divspec_e(:,:,:,nfiles)*wgt2
         virtemp_e = virtemp_e+virtempspec_e(:,:,:,nfiles)*wgt2
         tracer_e = tracer_e+tracerspec_e(:,:,:,nfiles)*wgt2
         vrt_o = vrt_o+vrtspec_o(:,:,:,nfiles)*wgt2
         div_o = div_o+divspec_o(:,:,:,nfiles)*wgt2
         virtemp_o = virtemp_o+virtempspec_o(:,:,:,nfiles)*wgt2
         tracer_o = tracer_o+tracerspec_o(:,:,:,nfiles)*wgt2
!!$omp end workshare
         lnps_e = lnps_e+lnpsspec_e(:,:,nfiles)*wgt2
         lnps_o = lnps_o+lnpsspec_o(:,:,nfiles)*wgt2
         return
      else if (t .eq. 3600.*iaufhrs(1)) then
!!$omp workshare
         wgt1=dtiau/dt
         vrt_e = vrt_e+vrtspec_e(:,:,:,1)*wgt1
         div_e = div_e+divspec_e(:,:,:,1)*wgt1
         virtemp_e = virtemp_e+virtempspec_e(:,:,:,1)*wgt1
         tracer_e = tracer_e+tracerspec_e(:,:,:,1)*wgt1
         vrt_o = vrt_o+vrtspec_o(:,:,:,1)*wgt1
         div_o = div_o+divspec_o(:,:,:,1)*wgt1
         virtemp_o = virtemp_o+virtempspec_o(:,:,:,1)*wgt1
         tracer_o = tracer_o+tracerspec_o(:,:,:,1)*wgt1
!!$omp end workshare
         lnps_e = lnps_e+lnpsspec_e(:,:,1)*wgt1
         lnps_o = lnps_o+lnpsspec_o(:,:,1)*wgt1
         return
      endif

      do n=1,nfiles
         if (iaufhrs(n)*3600. > t) exit
      enddo
      if (me .eq. me_l_0) print *,'n,t,to',n,t/3600.,iaufhrs(n)
      delt = (iaufhrs(n)-(t/3600.))/(iaufhrs(n)-iaufhrs(n-1))
      wgt1=dtiau*(1.-delt)/dt
      wgt2=dtiau*delt/dt
!!$omp workshare
      vrt_e = vrt_e+wgt1*vrtspec_e(:,:,:,n) + wgt2*vrtspec_e(:,:,:,n-1)
      div_e = div_e+wgt1*divspec_e(:,:,:,n) + wgt2*divspec_e(:,:,:,n-1)
      virtemp_e = virtemp_e+wgt1*virtempspec_e(:,:,:,n) + wgt2*virtempspec_e(:,:,:,n-1)
      tracer_e = tracer_e+wgt1*tracerspec_e(:,:,:,n) + wgt2*tracerspec_e(:,:,:,n-1)
      vrt_o = vrt_o+wgt1*vrtspec_o(:,:,:,n) + wgt2*vrtspec_o(:,:,:,n-1)
      div_o = div_o+wgt1*divspec_o(:,:,:,n) + wgt2*divspec_o(:,:,:,n-1)
      virtemp_o = virtemp_o+wgt1*virtempspec_o(:,:,:,n) + wgt2*virtempspec_o(:,:,:,n-1)
      tracer_o = tracer_o+wgt1*tracerspec_o(:,:,:,n) + wgt2*tracerspec_o(:,:,:,n-1)
!!$omp end workshare
      lnps_e = lnps_e+wgt1*lnpsspec_e(:,:,n) + wgt2*lnpsspec_e(:,:,n-1)
      lnps_o = lnps_o+wgt1*lnpsspec_o(:,:,n) + wgt2*lnpsspec_o(:,:,n-1)
      if (me .eq. me_l_0) print *,'getiauforcing:',t/3600.,1.-delt,n,iaufhrs(n),delt,n-1,iaufhrs(n-1)
   else  ! single file at middle of window
      t1=iaufhrs(1)*3600 - dt*0.5
      t2=iaufhrs(1)*3600 + dt*0.5
      if ( t <= t1 .or. t > t2 ) then
         if (me .eq. me_l_0) print *,'no iau forcing'
         if (t > t2) iauforcing = .false.
         return
      endif
!!$omp workshare
      wgt1=dtiau/dt
      vrt_e = vrt_e+vrtspec_e(:,:,:,1)*wgt1
      div_e = div_e+divspec_e(:,:,:,1)*wgt1
      virtemp_e = virtemp_e+virtempspec_e(:,:,:,1)*wgt1
      tracer_e = tracer_e+tracerspec_e(:,:,:,1)*wgt1
      vrt_o = vrt_o+vrtspec_o(:,:,:,1)*wgt1
      div_o = div_o+divspec_o(:,:,:,1)*wgt1
      virtemp_o = virtemp_o+virtempspec_o(:,:,:,1)*wgt1
      tracer_o = tracer_o+tracerspec_o(:,:,:,1)*wgt1
!!$omp end workshare
      lnps_e = lnps_e+lnpsspec_e(:,:,1)*wgt1
      lnps_o = lnps_o+lnpsspec_o(:,:,1)*wgt1
   endif

 end subroutine getiauforcing

 subroutine destroy_iau()

! deallocate arrays.

   deallocate(vrtspec_e,vrtspec_o)
   deallocate(divspec_e,divspec_o)
   deallocate(virtempspec_e,virtempspec_o)
   deallocate(tracerspec_e,tracerspec_o)
   deallocate(lnpsspec_e,lnpsspec_o)

 end subroutine destroy_iau

end module iau_module
