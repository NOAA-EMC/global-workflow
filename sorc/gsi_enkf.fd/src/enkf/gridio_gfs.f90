 module gridio
!$$$  module documentation block
!
! module: gridio                     subroutines for reading and writing
!                                    ensemble members files using
!                                    EnKF internal format.  A separate
!                                    program must be run before and
!                                    after the EnKF analysis to convert
!                                    to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: I/O for ensemble member files.
!
! Public Functions:
!  readgriddata, writegriddata
!
! this version reads and writes NCEP GFS sigma files.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Initial version.
!   2015-06-29  Add ability to read/write multiple time levels
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP, DPDT
!               removed)
!   2016-11-29  shlyaeva: Add reading/calculating tsen, qi, ql. Pass filenames and
!               hours to read routine to read separately state and control files.
!               Pass levels and dimenstions to read/write routines (dealing with
!               prse: nlevs + 1 levels). Pass "reducedgrid" parameter.
!   2017-06-14  Adding functionality to optionally write non-inflated ensembles,
!               a required input for EFSO calculations
!   2019-03-13  Add precipitation components
!   2019-07-10  Add convective clouds
!   2022-07-21  Draper: added read/write for sfc file for nc io (writeincrements, and readgridata)
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,fv,rd,tiny_r_kind,max_varname_length,t0c,r0_05
 use params, only: nlons,nlats,nlevs,use_gfs_nemsio,pseudo_rh, &
                   cliptracers,datapath,imp_physics,use_gfs_ncio,cnvw_option, &
                   nanals
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: ntrunc,npts  ! gridinfo must be called first!
 use specmod, only: sptezv_s, sptez_s, init_spec_vars, ndimspec => nc, &
                    isinitialized
 use reducedgrid_mod, only: regtoreduced, reducedtoreg
 use mpisetup, only: nproc, numproc
 use mpimod, only: mpi_comm_world, mpi_sum, mpi_real4, mpi_real8, mpi_rtype
 use mpeu_util, only: getindex
 implicit none
 private
 public :: readgriddata, readgriddata_pnc, writegriddata, writegriddata_pnc
 public :: writeincrement, writeincrement_pnc

 contains

 subroutine readgriddata_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,ntimes, &
                             fileprefixes,filesfcprefixes,reducedgrid,grdin,qsat)
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         quantize_data,read_attribute, close_dataset, get_dim, read_vardata
  implicit none

  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d, n3d
  integer, dimension(0:n3d), intent(in) :: levels
  integer, intent(in) :: ndim, ntimes
  character(len=120), dimension(7), intent(in)  :: fileprefixes
  character(len=120), dimension(7), intent(in)  :: filesfcprefixes
  logical, intent(in) :: reducedgrid
  real(r_single), dimension(npts,ndim,ntimes,1), intent(out) :: grdin
  real(r_double), dimension(npts,nlevs,ntimes,1), intent(out) :: qsat

  character(len=500) :: filename,sfcfilename
  character(len=7) charnanal

  real(r_kind) :: kap,kapr,kap1,clip,qi_coef

  real(r_kind), allocatable, dimension(:,:)     :: vmassdiv
  real(r_single), allocatable, dimension(:,:)   :: pressi,pslg,values_2d
  real(r_kind), dimension(nlons*nlats)          :: ug,vg
  real(r_single), dimension(npts,nlevs)         :: tv, q, cw
  real(r_kind), dimension(ndimspec)             :: vrtspec,divspec
  real(r_kind), allocatable, dimension(:)       :: psg,pstend,ak,bk
  real(r_single),allocatable,dimension(:,:,:)   :: ug3d,vg3d
  type(Dataset) :: dset, dset_sfc
  type(Dimension) :: londim,latdim,levdim

  integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer(i_kind) :: qr_ind, qs_ind, qg_ind
  integer(i_kind) :: tsen_ind, ql_ind, qi_ind, prse_ind
  integer(i_kind) :: ps_ind, pst_ind, sst_ind
  ! surface 
  integer(i_kind) :: tmp2m_ind, spfh2m_ind, soilt1_ind, soilt2_ind, soilt3_ind
  integer(i_kind) :: soilt4_ind,slc1_ind, slc2_ind, slc3_ind, slc4_ind

  integer(i_kind) :: k,iret,nb,i,imem,idvc,nlonsin,nlatsin,nlevsin,ne,nanal
  ! surface
  integer(i_kind) :: nlonsin_sfc,nlatsin_sfc

  logical ice
  logical use_full_hydro
  integer(i_kind), allocatable, dimension(:) :: mem_pe, lev_pe1, lev_pe2, iocomms
  integer(i_kind) :: iope, ionumproc, iolevs, krev, ierr
  integer(i_kind) :: ncstart(3), nccount(3)

  ! mpi gatherv things
  integer(i_kind), allocatable, dimension(:) :: recvcounts, displs
  real(r_single), dimension(nlons,nlats,nlevs) :: ug3d_0, vg3d_0
  logical :: read_sfc_file, read_atm_file

  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, read_sfc_file, read_atm_file)

  ! figure out what member to read and do MPI sub-communicator things
  allocate(mem_pe(0:numproc-1))
  allocate(iocomms(nanals))
  imem = 1
  do i=0,numproc-1
    mem_pe(i) = imem
    imem = imem + 1
    if (imem > nanals) imem = 1
  end do
  nanal = mem_pe(nproc)

  call mpi_comm_split(mpi_comm_world, mem_pe(nproc), nproc, iocomms(mem_pe(nproc)), iret)
  call mpi_comm_rank(iocomms(mem_pe(nproc)), iope, iret)
  call mpi_comm_size(iocomms(mem_pe(nproc)), ionumproc, iret)

  ! figure out what levels to read on this sub-communicator's PE
  allocate(lev_pe1(0:ionumproc-1))
  allocate(lev_pe2(0:ionumproc-1))
  iolevs = nlevs/ionumproc
  do i=0,ionumproc-1
     lev_pe1(i) = (i * iolevs) + 1
     lev_pe2(i) = ((i + 1) * iolevs)
     if (i == ionumproc-1) lev_pe2(i) = lev_pe2(i) + modulo(nlevs, ionumproc)
  end do
  ncstart = (/1, 1, lev_pe1(iope)/)
  nccount = (/nlons, nlats, lev_pe2(iope) - lev_pe1(iope)+1/)

  ! some mpi gatherv calculations
  allocate(recvcounts(ionumproc))
  allocate(displs(ionumproc))
  do i=0, ionumproc-1
     recvcounts(i+1) = (lev_pe2(i) - lev_pe1(i)+1)*nlons*nlats
     displs(i+1) = ((lev_pe1(i)-1)*nlons*nlats)
  end do

 if (read_atm_file) then  

  ! loop through times and do the read
  ne = 1
  backgroundloop: do nb=1,ntimes

  write(charnanal,'(a3, i3.3)') 'mem', nanal
  filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)
  if (use_gfs_ncio) then
     dset = open_dataset(filename, paropen=.true., mpicomm=iocomms(mem_pe(nproc)))
     londim = get_dim(dset,'grid_xt'); nlonsin = londim%len
     latdim = get_dim(dset,'grid_yt'); nlatsin = latdim%len
     levdim = get_dim(dset,'pfull');   nlevsin = levdim%len
     idvc=2
  else
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** parallel read only supported for netCDF' , ' PROGRAM STOPS'
     call stop2(23)
  end if
  ice = .false. ! calculate qsat w/resp to ice?
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one
  clip = tiny(vg(1))

  u_ind   = getindex(vars3d, 'u')   !< indices in the state or control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  prse_ind = getindex(vars3d, 'prse')
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero
  sst_ind = getindex(vars2d, 'sst')
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0  .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )

  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pslg(npts,nlevs))
  allocate(psg(nlons*nlats))
  if (pst_ind > 0) allocate(vmassdiv(nlons*nlats,nlevs),pstend(nlons*nlats))

  call read_vardata(dset, 'pressfc', values_2d,errcode=iret)
  if (iret /= 0) then
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading ps, iret= ',iret,' PROGRAM STOPS'
     call stop2(31)
  endif
  psg = 0.01_r_kind*reshape(values_2d,(/nlons*nlats/))
  call read_attribute(dset, 'ak', ak)
  call read_attribute(dset, 'bk', bk)
  if (nanal .eq. 1 .and. iope==0) then
     print *,'time level ',nb
     print *,'---------------'
  endif
  ! pressure at interfaces
  do k=1,nlevs+1
     ! k=1 in ak,bk is model top
     pressi(:,k) = 0.01_r_kind*ak(nlevs-k+2)+bk(nlevs-k+2)*psg
     if (nanal .eq. 1 .and. iope==0) print *,'netcdf, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
  enddo
  deallocate(ak,bk,values_2d)

  call read_vardata(dset, 'ugrd', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading ugrd, iret= ',iret,' PROGRAM STOPS'
     call stop2(22)
  endif
  call read_vardata(dset, 'vgrd', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading vgrd, iret= ',iret,' PROGRAM STOPS'
     call stop2(23)
  endif
  call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                   mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
  call mpi_gatherv(vg3d, recvcounts(iope+1), mpi_real4, vg3d_0, recvcounts, displs,&
                   mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
  if (iope==0) then
     do k=1,nlevs
        krev = nlevs-k+1
        ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
        vg = reshape(vg3d_0(:,:,krev),(/nlons*nlats/))
        if (u_ind > 0) call copytogrdin(ug,grdin(:,levels(u_ind-1) + k,nb,ne))
        if (v_ind > 0) call copytogrdin(vg,grdin(:,levels(v_ind-1) + k,nb,ne))
        ! calculate vertical integral of mass flux div (ps tendency)
        ! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           krev = nlevs-k+1
           ug = ug*(pressi(:,krev)-pressi(:,krev+1))
           vg = vg*(pressi(:,krev)-pressi(:,krev+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,krev),1) ! divspec to divgrd
        endif
     enddo
  end if
  call read_vardata(dset,'tmp', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading tmp, iret= ',iret,' PROGRAM STOPS'
     call stop2(24)
  endif
  call read_vardata(dset,'spfh', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading spfh, iret= ',iret,' PROGRAM STOPS'
     call stop2(25)
  endif
  call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                   mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
  call mpi_gatherv(vg3d, recvcounts(iope+1), mpi_real4, vg3d_0, recvcounts, displs,&
                   mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
  if (iope==0) then
     do k=1,nlevs
        krev = nlevs-k+1
        ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
        vg = reshape(vg3d_0(:,:,krev),(/nlons*nlats/))
        if (tsen_ind > 0) call copytogrdin(ug,grdin(:,levels(tsen_ind-1)+k,nb,ne))
        call copytogrdin(vg, q(:,k))
        ug = ug * ( 1.0 + fv*vg ) ! convert T to Tv
        call copytogrdin(ug,tv(:,k))
        if (tv_ind > 0)   grdin(:,levels(tv_ind-1)+k,nb,ne) = tv(:,k)
        if (q_ind > 0)    grdin(:,levels( q_ind-1)+k,nb,ne) =  q(:,k)
     end do
  end if

  if (oz_ind > 0) then
     call read_vardata(dset, 'o3mr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
     if (iret /= 0) then
        write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading o3mr, iret= ',iret,' PROGRAM STOPS'
        call stop2(26)
     endif
     if (cliptracers)  where (ug3d < clip) ug3d = clip
     call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                      mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
     if (iope==0) then
        do k=1,nlevs
           krev = nlevs-k+1
           ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
           call copytogrdin(ug,grdin(:,levels(oz_ind-1)+k,nb,ne))
        end do
     end if
  endif
  ! Read in hydrometeor fields based on control/state variables listed in anavinfo table 
  if (use_full_hydro) then
     if(ql_ind > 0) then
        call read_vardata(dset, 'clwmr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
        if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading clwmr, iret= ',iret,' PROGRAM STOPS'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,grdin(:,levels(ql_ind-1)+k,nb,ne))
           end do
        end if
     endif
     if(qi_ind > 0) then
        call read_vardata(dset, 'icmr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
        if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading icmr, iret= ',iret,' PROGRAM STOPS'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,grdin(:,levels(qi_ind-1)+k,nb,ne))
           end do
        end if
     endif
     if(qr_ind > 0) then
        call read_vardata(dset, 'rwmr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
        if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading rwmr, iret= ',iret,' PROGRAM STOPS'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,grdin(:,levels(qr_ind-1)+k,nb,ne))
           end do
        end if
     endif
     if(qs_ind > 0) then
       call read_vardata(dset, 'snmr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
       if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading snmr, iret= ',iret,' PROGRAM STOPS'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,grdin(:,levels(qs_ind-1)+k,nb,ne))
           end do
        end if
     endif
     if(qg_ind > 0) then
        call read_vardata(dset, 'grle', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
        if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading grle, iret= ',iret,' PROGRAM STOPS'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,grdin(:,levels(qg_ind-1)+k,nb,ne))
           end do
        end if
     endif
  else
     ! Handle non-precipiting hydrometeors 
     ! if control or state variable is cw, make sure combine background ql and qi to cw
     if (cw_ind > 0 .or. ql_ind > 0 .or. qi_ind > 0) then
        call read_vardata(dset, 'clwmr', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
        if (iret /= 0) then
           write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading clwmr, iret= ',iret,' PROGRAM STOPS'
           call stop2(27)
        endif
        if (imp_physics == 11) then
           call read_vardata(dset, 'icmr', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
           if (iret /= 0) then
              write(6,*)'READGRIDDATA_PNC:  ***FATAL ERROR*** reading icmr, iret= ',iret,' PROGRAM STOPS'
              call stop2(28)
           endif
           ug3d = ug3d + vg3d
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        call mpi_gatherv(ug3d, recvcounts(iope+1), mpi_real4, ug3d_0, recvcounts, displs,&
                         mpi_real4, 0, iocomms(mem_pe(nproc)),iret)
        if (iope==0) then
           do k=1,nlevs
              krev = nlevs-k+1
              ug = reshape(ug3d_0(:,:,krev),(/nlons*nlats/))
              call copytogrdin(ug,cw(:,k))
              if (cw_ind > 0) grdin(:,levels(cw_ind-1)+k,nb,ne) = cw(:,k)
           end do
        end if
     endif
  endif
  deallocate(ug3d,vg3d)

  ! surface pressure
  if (ps_ind > 0 .and. iope==0) then
    call copytogrdin(psg,grdin(:,levels(n3d) + ps_ind,nb,ne))
  endif

  ! surface pressure tendency
  if (pst_ind > 0 .and. iope==0) then
     pstend = sum(vmassdiv,2)
     if (nanal .eq. 1 .and. iope==0) &
     print *,nanal,'min/max first-guess ps tend',minval(pstend),maxval(pstend)
     call copytogrdin(pstend,grdin(:,levels(n3d) + pst_ind,nb,ne))
  endif

  if (iope==0) then
     do k=1,nlevs
        ! pressure at bottom of layer interface (for gps jacobian, see prsltmp in setupbend.f90)
        if (prse_ind > 0) then
           ug(:) = pressi(:,k)
           call copytogrdin(ug,pslg(:,k))
           ! Jacobian for gps in pressure is saved in different units in GSI; need to
           ! multiply pressure by 0.1
           grdin(:,levels(prse_ind-1)+k,nb,ne) = 0.1*pslg(:,k)
        endif
        ! layer pressure from phillips vertical interolation (used for qsat
        ! calculation)
        ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
                (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
        call copytogrdin(ug,pslg(:,k))
     end do
     if (pseudo_rh) then
        call genqsat1(q,qsat(:,:,nb,ne),pslg,tv,ice,npts,nlevs)
     else
        qsat(:,:,nb,ne) = 1._r_double
     end if
  end if

  ! cloud derivatives
  if (.not. use_full_hydro .and. iope==0) then
     if (ql_ind > 0 .or. qi_ind > 0) then
        do k=1,nlevs
           do i = 1, npts
              qi_coef        = -r0_05*(tv(i,k)/(one+fv*q(i,k))-t0c)
              qi_coef        = max(zero,qi_coef)
              qi_coef        = min(one,qi_coef)    ! 0<=qi_coef<=1
              if (ql_ind > 0) then
                grdin(i,levels(ql_ind-1)+k,nb,ne) = cw(i,k)*(one-qi_coef)
              endif
              if (qi_ind > 0) then
                grdin(i,levels(qi_ind-1)+k,nb,ne) = cw(i,k)*qi_coef
              endif
           enddo
        enddo
     endif
  endif

  if (sst_ind > 0 .and. iope==0) then
    grdin(:,levels(n3d)+sst_ind, nb,ne) = zero
  endif

  ! bring all the subdomains back to the main PE
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)

  deallocate(pressi,pslg)
  deallocate(psg)
  if (pst_ind > 0) deallocate(vmassdiv,pstend)
  call close_dataset(dset)
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)


  end do backgroundloop ! loop over backgrounds to read in

 end if   !read_atm_file

 if (read_sfc_file) then 
   ! loop through times and do the read
   ne = 1
   sfcbackgroundloop: do nb=1,ntimes
   
   write(charnanal,'(a3, i3.3)') 'mem', nanal
   sfcfilename = trim(adjustl(datapath))//trim(adjustl(filesfcprefixes(nb)))//trim(charnanal)
   if (use_gfs_ncio) then
     dset_sfc = open_dataset(sfcfilename, paropen=.true., mpicomm=iocomms(mem_pe(nproc)))
   else
     write(6,*)'READGRIDDATA_PNC sfc:  ***FATAL ERROR*** parallel read only supported for netCDF' , ' PROGRAM STOPS'
     call mpi_barrier(mpi_comm_world,ierr)
     call mpi_finalize(ierr)
   end if
   if ( reducedgrid ) then
       write(6,*) "READGRIDDATA_PNC sfc: reducedgrid=T interpolation not valid for writing sfc files"
       call mpi_barrier(mpi_comm_world,ierr)
       call mpi_finalize(ierr)
   endif

   ! land sfc DA variables
   tmp2m_ind  = getindex(vars2d, 't2m')
   spfh2m_ind = getindex(vars2d, 'q2m')
   soilt1_ind = getindex(vars2d, 'st1')
   slc1_ind = getindex(vars2d, 'sl1')
   soilt2_ind = getindex(vars2d, 'st2')
   slc2_ind = getindex(vars2d, 'sl2')
   soilt3_ind = getindex(vars2d, 'st3')
   slc3_ind = getindex(vars2d, 'sl3')
   soilt4_ind = getindex(vars2d, 'st4')
   slc4_ind = getindex(vars2d, 'sl4')

   ! read in sfc vars, if requested
   if (tmp2m_ind > 0) then
       call read_vardata(dset_sfc, 'tmp2m', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading tmp2m'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + tmp2m_ind,nb,ne))
   endif
   if (spfh2m_ind > 0) then
       call read_vardata(dset_sfc, 'spfh2m', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading spfh2m'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + spfh2m_ind,nb,ne))
   endif
   if (soilt1_ind > 0) then
       call read_vardata(dset_sfc, 'soilt1', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soilt1'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + soilt1_ind,nb,ne))
   endif
   if (soilt2_ind > 0) then
       call read_vardata(dset_sfc, 'soilt2', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soilt2'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + soilt2_ind,nb,ne))
   endif
   if (soilt3_ind > 0) then
       call read_vardata(dset_sfc, 'soilt3', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soilt3'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + soilt3_ind,nb,ne))
   endif
   if (soilt4_ind > 0) then
       call read_vardata(dset_sfc, 'soilt4', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soilt2'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + soilt4_ind,nb,ne))
   endif
   if (slc1_ind > 0) then
       call read_vardata(dset_sfc, 'soill1', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soill1'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + slc1_ind,nb,ne))
   endif
   if (slc2_ind > 0) then
       call read_vardata(dset_sfc, 'soill2', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soill2'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + slc2_ind,nb,ne))
   endif
   if (slc3_ind > 0) then
       call read_vardata(dset_sfc, 'soill3', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soill3'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + slc3_ind,nb,ne))
   endif
   if (slc4_ind > 0) then
       call read_vardata(dset_sfc, 'soill4', values_2d, errcode=iret)
       if (iret /= 0) then
               print *,'READGRIDDATA_PNC: error reading soill4'
               call stop2(22)
       endif
       ug = reshape(values_2d,(/nlons*nlats/))
       if (iope==0) call copytogrdin(ug,grdin(:,levels(n3d) + slc4_ind,nb,ne))
   endif

   ! bring all the subdomains back to the main PE
   call mpi_barrier(iocomms(mem_pe(nproc)), iret) 
   if (allocated(values_2d)) deallocate(values_2d)
   call close_dataset(dset_sfc)
   call mpi_barrier(iocomms(mem_pe(nproc)), iret)

   end do sfcbackgroundloop ! loop over backgrounds to read in
 end if   !if (read_sfc_file)

  ! remove the sub communicators
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  call mpi_comm_free(iocomms(mem_pe(nproc)), iret)
  call mpi_barrier(mpi_comm_world, iret)


  return

  contains
 ! copying to grdin (calling regtoreduced if reduced grid)
  subroutine copytogrdin(field, grdin)
  implicit none

  real(r_kind), dimension(:), intent(in)      :: field
  real(r_single), dimension(:), intent(inout) :: grdin

  if (reducedgrid) then
    call regtoreduced(field, grdin)
  else
    grdin = field
  endif

  end subroutine copytogrdin

 end subroutine readgriddata_pnc


 subroutine readgriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes, &
                         fileprefixes,filesfcprefixes,reducedgrid,grdin,qsat)
  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_aldata, sigio_axdata
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getfilehead,nemsio_getheadvar,nemsio_realkind,nemsio_charkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         quantize_data,read_attribute, close_dataset, get_dim, read_vardata
  implicit none

  integer, intent(in) :: nanal1,nanal2
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d, n3d
  integer, dimension(0:n3d), intent(in) :: levels
  integer, intent(in) :: ndim, ntimes
  character(len=120), dimension(7), intent(in)  :: fileprefixes
  character(len=120), dimension(7), intent(in)  :: filesfcprefixes
  logical, intent(in) :: reducedgrid
  real(r_single), dimension(npts,ndim,ntimes,nanal2-nanal1+1), intent(out) :: grdin
  real(r_double), dimension(npts,nlevs,ntimes,nanal2-nanal1+1), intent(out) :: qsat

  character(len=500) :: filename
  character(len=500) :: filenamesfc
  character(len=7) charnanal

  real(r_kind) :: kap,kapr,kap1,clip,qi_coef

  real(r_kind), allocatable, dimension(:,:)     :: vmassdiv
  real(r_single), allocatable, dimension(:,:)   :: pressi,pslg,values_2d
  real(r_kind), dimension(nlons*nlats)          :: ug,vg
  real(r_single), dimension(npts,nlevs)         :: tv, q, cw
  real(r_single), dimension(npts,nlevs)         :: ql, qi, qr, qs, qg
  real(r_kind), dimension(ndimspec)             :: vrtspec,divspec
  real(r_kind), allocatable, dimension(:)       :: psg,pstend,ak,bk
  real(r_single),allocatable,dimension(:,:,:)   :: nems_vcoord, ug3d,vg3d
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  type(sigio_head)   :: sighead
  type(sigio_data)   :: sigdata
  type(nemsio_gfile) :: gfile
  type(Dataset) :: dset, dset_sfc
  type(Dimension) :: londim,latdim,levdim
  type(nemsio_gfile) :: gfilesfc

  integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer(i_kind) :: qr_ind, qs_ind, qg_ind
  integer(i_kind) :: tsen_ind, ql_ind, qi_ind, prse_ind
  integer(i_kind) :: ps_ind, pst_ind, sst_ind
  integer(i_kind) :: tmp2m_ind, spfh2m_ind, soilt1_ind, soilt2_ind, soilt3_ind
  integer(i_kind) :: soilt4_ind,slc1_ind, slc2_ind, slc3_ind, slc4_ind

  integer(i_kind) :: k,iunitsig,iret,nb,i,idvc,nlonsin,nlatsin,nlevsin,ne,nanal
  integer(i_kind) :: nlonsin_sfc,nlatsin_sfc
  logical ice
  logical use_full_hydro
  logical read_sfc_file, read_atm_file

  use_full_hydro = .false.

  ! determine which files will be read in
  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, read_sfc_file, read_atm_file)

  ne = 0
  ensmemloop: do nanal=nanal1,nanal2
  ne = ne + 1
  backgroundloop: do nb=1,ntimes

  if (nanal > 0) then
    write(charnanal,'(a3, i3.3)') 'mem', nanal
  else
    charnanal = 'ensmean'
  endif
  iunitsig = 77
  filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)
  filenamesfc = trim(adjustl(datapath))//trim(adjustl(filesfcprefixes(nb)))//trim(charnanal)
  if (use_gfs_nemsio) then
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_init, iret=',iret
        call stop2(23)
     end if
     call nemsio_open(gfile,filename,'READ',iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_open, iret=',iret
        call stop2(23)
     endif
     call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                             dimz=nlevsin,idvc=idvc)
     if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
       print *,'incorrect dims in nemsio file'
       print *,'expected',nlons,nlats,nlevs
       print *,'got',nlonsin,nlatsin,nlevsin
       call stop2(23)
     end if

     if (cnvw_option) then
        call nemsio_open(gfilesfc,filenamesfc,'READ',iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with sfc nemsio_open, iret=',iret
        else
           call nemsio_getfilehead(gfilesfc,iret=iret, dimx=nlonsin_sfc, dimy=nlatsin_sfc)
           if (nlons /= nlonsin_sfc .or. nlats /= nlatsin_sfc) then
              print *,'incorrect dims in nemsio sfc file'
              print *,'expected',nlons,nlats
              print *,'got',nlonsin_sfc,nlatsin_sfc
           end if
        endif
     endif
  else if (use_gfs_ncio) then
     dset = open_dataset(filename)
     londim = get_dim(dset,'grid_xt'); nlonsin = londim%len
     latdim = get_dim(dset,'grid_yt'); nlatsin = latdim%len
     levdim = get_dim(dset,'pfull');   nlevsin = levdim%len
     idvc=2
  else
     call sigio_srohdc(iunitsig,trim(filename), &
                       sighead,sigdata,iret)
     if (iret /= 0) then
        print *,'error reading file in gridio ',trim(filename)
        call stop2(23)
     end if
  endif
  ice = .false. ! calculate qsat w/resp to ice?
  kap = rd/cp
  kapr = cp/rd
  kap1 = kap+one

  u_ind   = getindex(vars3d, 'u')   !< indices in the state or control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  prse_ind = getindex(vars3d, 'prse')
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero
  sst_ind = getindex(vars2d, 'sst')
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0  .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )

!  if (nproc == 0) then
!    print *, 'indices: '
!    print *, 'u: ', u_ind, ', v: ', v_ind, ', tv: ', tv_ind, ', tsen: ', tsen_ind
!    print *, 'q: ', q_ind, ', oz: ', oz_ind, ', cw: ', cw_ind, ', qi: ', qi_ind
!    print *, 'ql: ', ql_ind, ', prse: ', prse_ind
!    print *, 'ps: ', ps_ind, ', pst: ', pst_ind, ', sst: ', sst_ind
!  endif
  if (read_atm_file) then

  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

  allocate(pressi(nlons*nlats,nlevs+1))
  allocate(pslg(npts,nlevs))
  allocate(psg(nlons*nlats))
  if (pst_ind > 0) allocate(vmassdiv(nlons*nlats,nlevs),pstend(nlons*nlats))

  if (use_gfs_nemsio) then
     call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
         write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ps), iret=',iret
         call stop2(23)
     endif
     psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.

     if (allocated(nems_vcoord))     deallocate(nems_vcoord)
     allocate(nems_vcoord(nlevs+1,3,2))
     call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
     if ( iret /= 0 ) then
        write(6,*)' gridio:  ***ERROR*** problem reading header ', &
           'vcoord, Status = ',iret
        call stop2(99)
     endif

     allocate(ak(nlevs+1),bk(nlevs+1))

     if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
        ak = zero
        bk = nems_vcoord(1:nlevs+1,1,1)
     elseif ( idvc == 1 ) then                     ! sigma coordinate
        ak = zero
        bk = nems_vcoord(1:nlevs+1,2,1)
     elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
        ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1) ! convert to mb
        bk = nems_vcoord(1:nlevs+1,2,1)
     else
        write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
        call stop2(85)
     endif
     if (nanal .eq. 1) then
        print *,'time level ',nb
        print *,'---------------'
     endif
     ! pressure at interfaces
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg
        if (nanal .eq. 1) print *,'nemsio, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
     enddo
     deallocate(ak,bk)
  else if (use_gfs_ncio) then
     call read_vardata(dset, 'pressfc', values_2d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading ps'
        call stop2(31)
     endif
     psg = 0.01_r_kind*reshape(values_2d,(/nlons*nlats/))
     call read_attribute(dset, 'ak', ak)
     call read_attribute(dset, 'bk', bk)
     if (nanal .eq. 1) then
        print *,'time level ',nb
        print *,'---------------'
     endif
     ! pressure at interfaces
     do k=1,nlevs+1
        ! k=1 in ak,bk is model top
        pressi(:,k) = 0.01_r_kind*ak(nlevs-k+2)+bk(nlevs-k+2)*psg
        if (nanal .eq. 1) print *,'netcdf, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
     enddo
     deallocate(ak,bk)
  else
     vrtspec = sigdata%ps
     call sptez_s(vrtspec,psg,1)
     !==> input psg is ln(ps) in centibars - convert to ps in millibars.
     psg = 10._r_kind*exp(psg)
     allocate(ak(nlevs+1),bk(nlevs+1))
     if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
         ak = zero
         bk = sighead%si(1:nlevs+1)
     else if (sighead%idvc == 1) then ! sigma coordinate
         ak = zero
         bk = sighead%vcoord(1:nlevs+1,2)
     else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
         bk = sighead%vcoord(1:nlevs+1,2)
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',sighead%idvc
         call stop2(32)
     end if
     !==> pressure at interfaces.
     if (nanal .eq. 1) then
        print *,'time level ',nb
        print *,'---------------'
     endif
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg
        if (nanal .eq. 1) print *,'sigio, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
     enddo
     deallocate(ak,bk)
  endif

  !==> get U,V,temp,q,ps on gaussian grid.
  ! u is first nlevs, v is second, t is third, then tracers.
  if (use_gfs_nemsio) then
     clip=tiny_r_kind
     do k=1,nlevs
        call nemsio_readrecv(gfile,'ugrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
            call stop2(23)
        endif
        ug = nems_wrk
        call nemsio_readrecv(gfile,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
            call stop2(23)
        endif
        vg = nems_wrk
        if (u_ind > 0)       call copytogrdin(ug,grdin(:,levels(u_ind-1) + k,nb,ne))
        if (v_ind > 0)       call copytogrdin(vg,grdin(:,levels(v_ind-1) + k,nb,ne))
        ! calculate vertical integral of mass flux div (ps tendency)
        ! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           ug = ug*(pressi(:,k)-pressi(:,k+1))
           vg = vg*(pressi(:,k)-pressi(:,k+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif
        call nemsio_readrecv(gfile,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
        call nemsio_readrecv(gfile,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif
        if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
        ug = nems_wrk
        if (tsen_ind > 0)    call copytogrdin(ug,grdin(:,levels(tsen_ind-1)+k,nb,ne))
        nems_wrk = nems_wrk * ( 1.0 + fv*nems_wrk2 ) ! convert T to Tv
        ug = nems_wrk
        vg = nems_wrk2
        call copytogrdin(ug,tv(:,k))
        call copytogrdin(vg, q(:,k))
        if (tv_ind > 0)               grdin(:,levels(tv_ind-1)+k,nb,ne) = tv(:,k)
        if (q_ind > 0)                grdin(:,levels( q_ind-1)+k,nb,ne) =  q(:,k)
        if (oz_ind > 0) then
           call nemsio_readrecv(gfile,'o3mr','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
              call stop2(23)
           endif
           if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
           ug = nems_wrk2
           call copytogrdin(ug,grdin(:,levels(oz_ind-1)+k,nb,ne))
        endif
        if (.not. use_full_hydro) then
           if (cw_ind > 0 .or. ql_ind > 0 .or. qi_ind > 0) then
              call nemsio_readrecv(gfile,'clwmr','mid layer',k,nems_wrk2,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
                 call stop2(23)
              endif
              if (imp_physics == 11) then
                 call nemsio_readrecv(gfile,'icmr','mid layer',k,nems_wrk,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(icmr), iret=',iret
                    call stop2(23)
                 else
                    nems_wrk2 = nems_wrk2 + nems_wrk
                 endif
              endif
              if (cnvw_option) then
                 call nemsio_readrecv(gfilesfc,'cnvcldwat','mid layer',k,nems_wrk,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(cnvw), iret=',iret
                    call stop2(23)
                 else
                    nems_wrk2 = nems_wrk2 + nems_wrk
                 end if
              end if
              if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
              ug = nems_wrk2
              call copytogrdin(ug,cw(:,k))
              if (cw_ind > 0)            grdin(:,levels(cw_ind-1)+k,nb,ne) = cw(:,k)
           endif
        else
           if (ql_ind > 0) then
              call nemsio_readrecv(gfile,'clwmr','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
                 call stop2(23)
              endif
              if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
              ug = nems_wrk
              call copytogrdin(ug,ql(:,k))
              grdin(:,levels(ql_ind-1)+k,nb,ne) = ql(:,k)
           endif
           if (qi_ind > 0) then
              call nemsio_readrecv(gfile,'icmr','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(icmr), iret=',iret
                 call stop2(23)
              endif
              if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
              ug = nems_wrk
              call copytogrdin(ug,qi(:,k))
              grdin(:,levels(qi_ind-1)+k,nb,ne) = qi(:,k)
           endif
           if (qr_ind > 0) then
              call nemsio_readrecv(gfile,'rwmr','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(rwmr), iret=',iret
                 call stop2(23)
              endif
              if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
              ug = nems_wrk
              call copytogrdin(ug,qr(:,k))
              grdin(:,levels(qr_ind-1)+k,nb,ne) = qr(:,k)
           endif
           if (qs_ind > 0) then
              call nemsio_readrecv(gfile,'snmr','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(snmr), iret=',iret
                 call stop2(23)
              endif
              if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
              ug = nems_wrk
              call copytogrdin(ug,qs(:,k))
              grdin(:,levels(qs_ind-1)+k,nb,ne) = qs(:,k)
           endif
           if (qg_ind > 0) then
              call nemsio_readrecv(gfile,'grle','mid layer',k,nems_wrk,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/readgriddata: gfs model: problem with nemsio_readrecv(grle), iret=',iret
                 call stop2(23)
              endif
              if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
              ug = nems_wrk
              call copytogrdin(ug,qg(:,k))
              grdin(:,levels(qg_ind-1)+k,nb,ne) = qg(:,k)
           endif
        endif  ! use_full_hydro
     enddo
  else if (use_gfs_ncio) then
     clip=tiny_r_kind
     call read_vardata(dset, 'ugrd', ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading ugrd'
        call stop2(22)
     endif
     call read_vardata(dset, 'vgrd', vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading vgrd'
        call stop2(23)
     endif
     do k=1,nlevs
        ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
        vg = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
        if (u_ind > 0) call copytogrdin(ug,grdin(:,levels(u_ind-1) + k,nb,ne))
        if (v_ind > 0) call copytogrdin(vg,grdin(:,levels(v_ind-1) + k,nb,ne))
        ! calculate vertical integral of mass flux div (ps tendency)
        ! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           ug = ug*(pressi(:,k)-pressi(:,k+1))
           vg = vg*(pressi(:,k)-pressi(:,k+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif
     enddo
     call read_vardata(dset,'tmp', ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading tmp'
        call stop2(24)
     endif
     call read_vardata(dset,'spfh', vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading spfh'
        call stop2(25)
     endif
     do k=1,nlevs
        ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
        vg = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
        if (tsen_ind > 0) call copytogrdin(ug,grdin(:,levels(tsen_ind-1)+k,nb,ne))
        call copytogrdin(vg, q(:,k))
        ug = ug * ( 1.0 + fv*vg ) ! convert T to Tv
        call copytogrdin(ug,tv(:,k))
        if (tv_ind > 0)   grdin(:,levels(tv_ind-1)+k,nb,ne) = tv(:,k)
        if (q_ind > 0)    grdin(:,levels( q_ind-1)+k,nb,ne) =  q(:,k)
     enddo
     if (oz_ind > 0) then
        call read_vardata(dset, 'o3mr', ug3d,errcode=iret)
        if (iret /= 0) then
           print *,'error reading o3mr'
           call stop2(26)
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        do k=1,nlevs
           ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
           call copytogrdin(ug,grdin(:,levels(oz_ind-1)+k,nb,ne))
        enddo
     endif
     if (cw_ind > 0 .or. ql_ind > 0 .or. qi_ind > 0) then
        call read_vardata(dset, 'clwmr', ug3d,errcode=iret)
        if (iret /= 0) then
           print *,'error reading clwmr'
           call stop2(27)
        endif
        if (imp_physics == 11) then
           call read_vardata(dset, 'icmr', vg3d,errcode=iret)
           if (iret /= 0) then
              print *,'error reading icmr'
              call stop2(28)
           endif
           ug3d = ug3d + vg3d
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
        do k=1,nlevs
           ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
           call copytogrdin(ug,cw(:,k))
           if (cw_ind > 0) grdin(:,levels(cw_ind-1)+k,nb,ne) = cw(:,k)
        enddo
     endif
     deallocate(ug3d,vg3d)
  else
!$omp parallel do private(k,ug,vg,divspec,vrtspec)  shared(sigdata,pressi,vmassdiv,grdin,tv,q,cw,u_ind,v_ind,pst_ind,q_ind,tsen_ind,cw_ind,qi_ind,ql_ind)
     do k=1,nlevs

        vrtspec = sigdata%z(:,k); divspec = sigdata%d(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,1)
        if (u_ind > 0) then
           call copytogrdin(ug,grdin(:,levels(u_ind-1)+k,nb,ne))
        endif
        if (v_ind > 0) then
           call copytogrdin(vg,grdin(:,levels(v_ind-1)+k,nb,ne))
        endif

! calculate vertical integral of mass flux div (ps tendency)
! this variable is analyzed in order to enforce mass balance in the analysis
        if (pst_ind > 0) then
           ug = ug*(pressi(:,k)-pressi(:,k+1))
           vg = vg*(pressi(:,k)-pressi(:,k+1))
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif

        divspec = sigdata%t(:,k)
        call sptez_s(divspec,ug,1)
        call copytogrdin(ug,tv(:,k))
        if (tv_ind > 0)          grdin(:,levels(tv_ind-1)+k,nb,ne) = tv(:,k)

        divspec = sigdata%q(:,k,1)
        call sptez_s(divspec,vg,1)
        call copytogrdin(vg,q(:,k))
        if (q_ind > 0)           grdin(:,levels( q_ind-1)+k,nb,ne) =  q(:,k)

        if (tsen_ind > 0)        grdin(:,levels(tsen_ind-1)+k,nb,ne) = tv(:,k) / (one + fv*max(0._r_kind,q(:,k)))

        if (oz_ind > 0) then
           divspec = sigdata%q(:,k,2)
           call sptez_s(divspec,ug,1)
           call copytogrdin(ug,grdin(:,levels(oz_ind-1)+k,nb,ne))
        endif

        if (cw_ind > 0 .or. ql_ind > 0 .or. qi_ind > 0) then
           divspec = sigdata%q(:,k,3)
           call sptez_s(divspec,ug,1)
           call copytogrdin(ug,cw(:,k))
           if (cw_ind > 0)       grdin(:,levels(cw_ind-1)+k,nb,ne) = cw(:,k)
        endif

     enddo
!$omp end parallel do
  endif

  ! surface pressure
  if (ps_ind > 0) then
    call copytogrdin(psg,grdin(:,levels(n3d) + ps_ind,nb,ne))
  endif
  if (.not. use_gfs_nemsio) call sigio_axdata(sigdata,iret)

  ! surface pressure tendency
  if (pst_ind > 0) then
     pstend = sum(vmassdiv,2)
     if (nanal .eq. 1) &
     print *,nanal,'min/max first-guess ps tend',minval(pstend),maxval(pstend)
     call copytogrdin(pstend,grdin(:,levels(n3d) + pst_ind,nb,ne))
  endif

  ! compute saturation q.
  do k=1,nlevs
    ! pressure at bottom of layer interface (for gps jacobian, see prsltmp in setupbend.f90)
    if (prse_ind > 0) then
       ug(:) = pressi(:,k)
       call copytogrdin(ug,pslg(:,k))
       ! Jacobian for gps in pressure is saved in different units in GSI; need to
       ! multiply pressure by 0.1
       grdin(:,levels(prse_ind-1)+k,nb,ne) = 0.1*pslg(:,k)
    endif
    ! layer pressure from phillips vertical interolation (used for qsat
    ! calculation)
    ug(:) = ((pressi(:,k)**kap1-pressi(:,k+1)**kap1)/&
            (kap1*(pressi(:,k)-pressi(:,k+1))))**kapr
    call copytogrdin(ug,pslg(:,k))
  end do
  if (pseudo_rh) then
     call genqsat1(q,qsat(:,:,nb,ne),pslg,tv,ice,npts,nlevs)
  else
     qsat(:,:,nb,ne) = 1._r_double
  end if

  ! cloud derivatives
  if (.not. use_full_hydro) then
  if (ql_ind > 0 .or. qi_ind > 0) then
     do k = 1, nlevs
        do i = 1, npts
           qi_coef        = -r0_05*(tv(i,k)/(one+fv*q(i,k))-t0c)
           qi_coef        = max(zero,qi_coef)
           qi_coef        = min(one,qi_coef)    ! 0<=qi_coef<=1
           if (ql_ind > 0) then
             grdin(i,levels(ql_ind-1)+k,nb,ne) = cw(i,k)*(one-qi_coef)
           endif
           if (qi_ind > 0) then
             grdin(i,levels(qi_ind-1)+k,nb,ne) = cw(i,k)*qi_coef
           endif
        enddo
     enddo
  endif
  endif

  if (sst_ind > 0) then
    grdin(:,levels(n3d)+sst_ind, nb,ne) = zero
  endif

  deallocate(pressi,pslg)
  deallocate(psg)
  if (pst_ind > 0) deallocate(vmassdiv,pstend)
  endif ! read_atm_file

  if (use_gfs_nemsio) call nemsio_close(gfile,iret=iret)
  if (use_gfs_ncio) call close_dataset(dset)
  if (use_gfs_nemsio) call nemsio_close(gfilesfc,iret=iret)

  if ( read_sfc_file ) then

     if ( .not.  use_gfs_ncio ) then
        write(6,*) 'griddio/griddata for sfc update vars only coded for nc io'
        call stop2(23)
     endif
     if ( reducedgrid ) then
        write(6,*) "reducedgrid=T interpolation not valid for writing sfc files"
        call stop2(22)
     endif

     ! land sfc DA variables
     tmp2m_ind  = getindex(vars2d, 't2m')
     spfh2m_ind = getindex(vars2d, 'q2m')
     soilt1_ind = getindex(vars2d, 'st1')
     slc1_ind = getindex(vars2d, 'sl1')
     soilt2_ind = getindex(vars2d, 'st2')
     slc2_ind = getindex(vars2d, 'sl2')
     soilt3_ind = getindex(vars2d, 'st3')
     slc3_ind = getindex(vars2d, 'sl3')
     soilt4_ind = getindex(vars2d, 'st4')
     slc4_ind = getindex(vars2d, 'sl4')

     dset_sfc = open_dataset(filenamesfc)
     ! read in sfc vars, if requested
     if (tmp2m_ind > 0) then
        call read_vardata(dset_sfc, 'tmp2m', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading tmp2m'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + tmp2m_ind,nb,ne))
     endif
     if (spfh2m_ind > 0) then
        call read_vardata(dset_sfc, 'spfh2m', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading spfh2m'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + spfh2m_ind,nb,ne))
     endif
     if (soilt1_ind > 0) then
        call read_vardata(dset_sfc, 'soilt1', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soilt1'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + soilt1_ind,nb,ne))
     endif
     if (soilt2_ind > 0) then
        call read_vardata(dset_sfc, 'soilt2', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soilt2'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + soilt2_ind,nb,ne))
     endif
     if (soilt3_ind > 0) then
        call read_vardata(dset_sfc, 'soilt3', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soilt3'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + soilt3_ind,nb,ne))
     endif
     if (soilt4_ind > 0) then
        call read_vardata(dset_sfc, 'soilt4', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soilt2'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + soilt4_ind,nb,ne))
     endif
     if (slc1_ind > 0) then
        call read_vardata(dset_sfc, 'soill1', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soill1'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + slc1_ind,nb,ne))
     endif
     if (slc2_ind > 0) then
        call read_vardata(dset_sfc, 'soill2', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soill2'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + slc2_ind,nb,ne))
     endif
     if (slc3_ind > 0) then
        call read_vardata(dset_sfc, 'soill3', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soill3'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + slc3_ind,nb,ne))
     endif
     if (slc4_ind > 0) then
        call read_vardata(dset_sfc, 'soill4', values_2d, errcode=iret)
        if (iret /= 0) then
                print *,'error reading soill4'
                call stop2(22)
        endif
        ug = reshape(values_2d,(/nlons*nlats/))
        call copytogrdin(ug,grdin(:,levels(n3d) + slc4_ind,nb,ne))
     endif

     call close_dataset(dset_sfc)

  endif ! sfc read

  if ( allocated(values_2d) )  deallocate(values_2d)

  end do backgroundloop ! loop over backgrounds to read in
  end do ensmemloop ! loop over ens members to read in

  return

  contains
 ! copying to grdin (calling regtoreduced if reduced grid)
  subroutine copytogrdin(field, grdin)
  implicit none

  real(r_kind), dimension(:), intent(in)      :: field
  real(r_single), dimension(:), intent(inout) :: grdin

  if (reducedgrid) then
    call regtoreduced(field, grdin)
  else
    grdin = field
  endif

  end subroutine copytogrdin

 end subroutine readgriddata

 subroutine writegriddata_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  use netcdf
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         read_attribute, close_dataset, get_dim, read_vardata,&
                         create_dataset, get_idate_from_time_units, &
                         get_time_units_from_idate, write_vardata, &
                         write_attribute, quantize_data, has_var, has_attr
  use constants, only: grav, zero
  use params, only: nbackgrounds,anlfileprefixes,fgfileprefixes,reducedgrid,&
                    nccompress
  implicit none

  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d,n3d,ndim
  integer, dimension(0:n3d), intent(in) :: levels
  real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
  logical, intent(in) :: no_inflate_flag
  logical:: use_full_hydro
  character(len=500):: filenamein, filenameout
  real(r_kind), allocatable, dimension(:,:) :: vmassdiv,dpanl,dpfg,pressi
  real(r_kind), allocatable, dimension(:,:) :: vmassdivinc
  real(r_kind), allocatable, dimension(:,:) :: ugtmp,vgtmp,ugtmp2,vgtmp2
  real(r_kind), allocatable,dimension(:) :: pstend1,pstend2,pstendfg,vmass
  real(r_kind), dimension(nlons*nlats) :: ug,vg,uginc,vginc,psfg,psg
  real(r_kind), allocatable, dimension(:) :: delzb,work,values_1d
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec
  real(r_single), allocatable, dimension(:,:,:) :: &
     ug3d,vg3d,values_3d,tmp_anal,tv_anal,tv_bg
  real(r_single), allocatable, dimension(:,:) :: values_2d
  integer iadate(4),idate(4),nfhour,idat(7),iret,jdat(6)
  integer,dimension(8):: ida,jda
  real(r_double),dimension(5):: fha
  real(r_kind) fhour
  type(Dataset) :: dsfg, dsanl
  character(len=3) charnanal
  character(len=nf90_max_name) :: time_units

  real(r_kind) kap,kapr,kap1,clip
  real(r_single) compress_err
  real(r_kind), dimension(nlevs+1) :: ak,bk

  integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind, nbits
  integer :: ql_ind, qi_ind, qr_ind, qs_ind, qg_ind

  integer k,nt,iunitsig,nb,i,ne,nanal,imem

  integer(i_kind), allocatable, dimension(:) :: mem_pe, lev_pe1, lev_pe2, iocomms
  integer(i_kind) :: iope, ionumproc, iolevs, krev, ki
  integer(i_kind) :: ncstart(4), nccount(4)
  logical :: nocompress

  logical :: write_sfc_file, write_atm_file
  character(len=max_varname_length), dimension(n3d) :: no_vars3d
  character(len=max_varname_length), dimension(n2d) :: no_vars2d

  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, write_sfc_file, write_atm_file)

  if (write_sfc_file ) then
        ! adding the sfc increments requires adjusting several other variables.
        ! This is done is a separate program.
        if (nproc == 0) write(6,*) 'gridio/writegriddata_pnc: not coded to write sfc analysis, will write increment for sfc fields'
        no_vars3d=''
        call writeincrement_pnc(no_vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  endif  

  nocompress = .true.

  if (nccompress) nocompress = .false.

  use_full_hydro = .false.
  iunitsig = 78
  kapr = cp/rd
  kap = rd/cp
  kap1 = kap+one
  clip = tiny_r_kind

  ! figure out what member to write and do MPI sub-communicator things
  allocate(mem_pe(0:numproc-1))
  allocate(iocomms(nanals))
  imem = 1
  do i=0,numproc-1
    mem_pe(i) = imem
    imem = imem + 1
    if (imem > nanals) imem = 1
  end do
  nanal = mem_pe(nproc)

  call mpi_comm_split(mpi_comm_world, mem_pe(nproc), nproc, iocomms(mem_pe(nproc)), iret)
  call mpi_comm_rank(iocomms(mem_pe(nproc)), iope, iret)
  call mpi_comm_size(iocomms(mem_pe(nproc)), ionumproc, iret)

  ! figure out what levels to write on this sub-communicator's PE
  allocate(lev_pe1(0:ionumproc-1))
  allocate(lev_pe2(0:ionumproc-1))
  iolevs = nlevs/ionumproc
  do i=0,ionumproc-1
     lev_pe1(i) = (iope * iolevs) + 1
     lev_pe2(i) = ((iope + 1) * iolevs)
     if (i == ionumproc-1) lev_pe2(i) = lev_pe2(i) + modulo(nlevs, ionumproc)
  end do
  ncstart = (/1, 1, lev_pe1(iope),1/)
  nccount = (/nlons, nlats, lev_pe2(iope) - lev_pe1(iope)+1,1/)

  ! need to distribute grdin to all PEs in this subcommunicator
  ! bring all the subdomains back to the main PE
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  do nb=1,nbackgrounds
     call mpi_bcast(grdin(1,1,nb,1),npts*ndim, mpi_real4, 0, iocomms(mem_pe(nproc)), iret)
  enddo

  ! loop through times and do the read
  ne = 1
  backgroundloop: do nb=1,nbackgrounds
  write(charnanal,'(i3.3)') nanal

  if(no_inflate_flag) then
    filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"nimem"//charnanal
  else
    filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal
  end if
  filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal

  clip = tiny(vg(1))
  dsfg = open_dataset(filenamein, paropen=.true., mpicomm=iocomms(mem_pe(nproc)))
  jdat = get_idate_from_time_units(dsfg)
  idat(4) = jdat(1) ! yr
  idat(2) = jdat(2) ! mon
  idat(3) = jdat(3) ! day
  idat(1) = jdat(4) ! hr
  call read_vardata(dsfg,'time',values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading time'
     call stop2(29)
  endif
  nfhour = int(values_1d(1))
  call read_attribute(dsfg, 'ak', values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading ak'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     ak(nlevs-k+2) = 0.01_r_kind*values_1d(k)
  enddo
  call read_attribute(dsfg, 'bk', values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading bk'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     bk(nlevs-k+2) = values_1d(k)
  enddo

  u_ind   = getindex(vars3d, 'u')   !< indices in the control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                    ! old logical massbal_adjust, if non-zero
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0 .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )

  if (pst_ind > 0) then
     allocate(vmassdiv(nlons*nlats,nlevs))
     allocate(vmassdivinc(nlons*nlats,nlevs))
     allocate(dpfg(nlons*nlats,nlevs))
     allocate(dpanl(nlons*nlats,nlevs))
     allocate(pressi(nlons*nlats,nlevs+1))
     allocate(pstendfg(nlons*nlats))
     allocate(pstend1(nlons*nlats))
     allocate(pstend2(nlons*nlats),vmass(nlons*nlats))
     allocate(ugtmp(nlons*nlats,nlevs),vgtmp(nlons*nlats,nlevs))
     allocate(ugtmp2(nlons*nlats,nlevs),vgtmp2(nlons*nlats,nlevs))
  endif

  if (imp_physics == 11 .and. (.not. use_full_hydro) ) allocate(work(nlons*nlats))

  idate(3)=idat(3)  !day
  idate(2)=idat(2)  !mon
  idate(4)=idat(4)  !yr
  idate(1)=idat(1)  !hr
  fhour = nfhour

  fha=zero; ida=0; jda=0
  fha(2)=fhour    ! relative time interval in hours
  ida(1)=idate(4) ! year
  ida(2)=idate(2) ! month
  ida(3)=idate(3) ! day
  ida(4)=0                ! time zone
  ida(5)=idate(1) ! hour
  call w3movdat(fha,ida,jda)
!
!   INPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!                (JDAT IS LATER THAN IDAT IF TIME INTERVAL IS POSITIVE.)
  iadate(1)=jda(5) ! hour
  iadate(2)=jda(2) ! mon
  iadate(3)=jda(3) ! day
  iadate(4)=jda(1) ! year
  if (nproc .eq. 0) then
     print *,'idate = ',idate
     print *,'iadate = ',iadate
  end if

  dsanl = create_dataset(filenameout, dsfg, copy_vardata=.true., &
          nocompress=nocompress, paropen=.true., mpicomm=iocomms(mem_pe(nproc)), errcode=iret)
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  if (iret /= 0) then
     print *,'error creating netcdf file'
     call stop2(29)
  endif
  deallocate(values_1d)
  allocate(values_1d(1))
  values_1d(1)=zero
  call write_vardata(dsanl,'time',values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error writing time'
     call stop2(29)
  endif
  jdat(1) = iadate(4)
  jdat(2) = iadate(2)
  jdat(3) = iadate(3)
  jdat(4) = iadate(1)
  jdat(5) = jda(6); jdat(6) = jda(7)
  time_units = get_time_units_from_idate(jdat)
  call write_attribute(dsanl,'units',time_units,'time',errcode=iret)
  if (iret /= 0) then
     print *,'error writing time units attribute'
     call stop2(29)
  endif
  call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading pressfc'
     call stop2(29)
  endif
  psfg = 0.01*reshape(values_2d,(/nlons*nlats/))
  ug = 0_r_kind
  if (ps_ind > 0) then
    call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),ug)
  endif
  ! add increment to background.
  psg = psfg + ug ! analysis pressure in mb.
  values_2d = 100.*reshape(psg,(/nlons,nlats/))
  call write_vardata(dsanl,'pressfc',values_2d, ncstart=(/1,1,1/), nccount=(/nlons,nlats,1/), errcode=iret)
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  if (iret /= 0) then
     print *,'error writing pressfc'
     call stop2(29)
  endif
  if (has_var(dsfg,'dpres')) then
     call read_vardata(dsfg,'dpres',ug3d, ncstart=ncstart, nccount=nccount)
     allocate(vg3d(nlons,nlats,nccount(3)))
     do k=lev_pe1(iope), lev_pe2(iope)
        krev = nlevs-k+1
        ki = k - lev_pe1(iope) + 1
        vg = ug*(bk(krev)-bk(krev+1)) ! ug is ps increment
        vg3d(:,:,ki) = ug3d(:,:,ki) +&
           100_r_kind*reshape(vg,(/nlons,nlats/))
     end do
     if (has_attr(dsfg, 'nbits', 'dpres') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'dpres')
       ug3d = vg3d
       call quantize_data(ug3d, vg3d, nbits, compress_err)
       ! below is not technically correct but I'm not worried about
       ! the exact value of this attribute right now
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'dpres',errcode=iret)
       if (iret /= 0) then
          print *,'error writing dpres attribute'
          call stop2(29)
       end if
     endif
     call write_vardata(dsanl,'dpres',vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
     if (iret /= 0) then
        print *,'error writing dpres'
        call stop2(29)
     endif
  end if
  if (pst_ind > 0) then
     !==> first guess pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psfg ! psfg in mb, ak has been scaled by 0.01
     enddo
     do k=1,nlevs
        dpfg(:,k) = pressi(:,k)-pressi(:,k+1)
     enddo
     !==> analysis pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg ! psg in mb, ak has been scaled by 0.01
     enddo
     do k=1,nlevs
        dpanl(:,k) = pressi(:,k)-pressi(:,k+1)
     end do
     ! have to read in the full arrays here because of vertical integral...
     call read_vardata(dsfg, 'ugrd', ug3d, errcode=iret)
     if (iret /= 0) then
        print *,'error reading ugrd'
        call stop2(29)
     endif
     call read_vardata(dsfg, 'vgrd', vg3d, errcode=iret)
     if (iret /= 0) then
        print *,'error reading vgrd'
        call stop2(29)
     endif
     do k=1,nlevs
        krev = nlevs-k+1
        ug = reshape(ug3d(:,:,krev),(/nlons*nlats/))
        vg = reshape(vg3d(:,:,krev),(/nlons*nlats/))
        ug = ug*dpfg(:,k)
        vg = vg*dpfg(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo

     ! analyzed ps tend increment
     call copyfromgrdin(grdin(:,levels(n3d) + pst_ind,nb,ne),pstend2)
     pstendfg = sum(vmassdiv,2)
     vmassdivinc = vmassdiv
     if (nanal .eq. 1 .and. iope==0) then
     print *,'time level ',nb
     print *,'--------------------'
     print *,nanal,'min/max pstendfg',minval(pstendfg),maxval(pstendfg)
     print *,nanal,'min/max pstend inc',minval(pstend2),maxval(pstend2)
     endif
     pstend2 = pstend2 + pstendfg ! add to background ps tend

     ugtmp(:,:) = zero
     ugtmp2(:,:) = zero
     vgtmp(:,:) = zero
     vgtmp2(:,:) = zero
  endif ! if pst_ind > 0


  ! now do parallel read and apply increments
  call read_vardata(dsfg, 'ugrd', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading ugrd'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     ug = 0_r_kind
     if (u_ind > 0) then
       call copyfromgrdin(grdin(:,levels(u_ind-1) + krev,nb,ne),ug)
     endif
     values_2d = reshape(ug,(/nlons,nlats/))
     ug3d(:,:,ki) = ug3d(:,:,ki) + values_2d
     if (pst_ind > 0) then
        ugtmp2(:,krev) = reshape(ug3d(:,:,k),(/nlons*nlats/))
     endif
  enddo
  if (has_attr(dsfg, 'nbits', 'ugrd') .and. .not. nocompress) then
    call read_attribute(dsfg, 'nbits', nbits, 'ugrd')
    if (.not. allocated(vg3d)) allocate(vg3d(nlons,nlats,nccount(3)))
    vg3d = ug3d
    call quantize_data(vg3d, ug3d, nbits, compress_err)
    ! same as before, below is not ideal
    call write_attribute(dsanl,&
    'max_abs_compression_error',compress_err,'ugrd',errcode=iret)
    if (iret /= 0) then
      print *,'error writing ugrd attribute'
      call stop2(29)
    end if
  endif
  call write_vardata(dsanl, 'ugrd', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  call read_vardata(dsfg, 'vgrd', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading vgrd'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     vg = 0_r_kind
     if (v_ind > 0) then
       call copyfromgrdin(grdin(:,levels(v_ind-1) + krev,nb,ne),vg)
     endif
     values_2d = reshape(vg,(/nlons,nlats/))
     vg3d(:,:,ki) = vg3d(:,:,ki) + values_2d
     if (pst_ind > 0) then
        vgtmp2(:,krev) = reshape(vg3d(:,:,k),(/nlons*nlats/))
     endif
  enddo
  if (has_attr(dsfg, 'nbits', 'vgrd')  .and. .not. nocompress) then
    call read_attribute(dsfg, 'nbits', nbits, 'vgrd')
    if (.not. allocated(ug3d)) allocate(ug3d(nlons,nlats,nccount(3)))
    ug3d = vg3d
    call quantize_data(ug3d, vg3d, nbits, compress_err)
    ! same as before, below is not ideal
    call write_attribute(dsanl,&
    'max_abs_compression_error',compress_err,'vgrd',errcode=iret)
    if (iret /= 0) then
      print *,'error writing ugrd attribute'
      call stop2(29)
    end if
  endif
  call write_vardata(dsanl, 'vgrd', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (pst_ind > 0) then
     do k=1,nlevs
        call mpi_allreduce(ugtmp2(:,k), ugtmp(:,k), nlons*nlats, mpi_rtype, &
                           mpi_sum, iocomms(mem_pe(nproc)),iret)
        call mpi_allreduce(vgtmp2(:,k), vgtmp(:,k), nlons*nlats, mpi_rtype, &
                           mpi_sum, iocomms(mem_pe(nproc)),iret)
     end do
  end if
  if (pst_ind > 0) then
     do k=1,nlevs
        ug = ugtmp(:,k)*dpanl(:,k)
        vg = vgtmp(:,k)*dpanl(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo
  end if
  ! read sensible temperature and specific humidity
  call read_vardata(dsfg, 'tmp', ug3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading tmp'
     call stop2(29)
  endif
  call read_vardata(dsfg, 'spfh', vg3d, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading spfh'
     call stop2(29)
  endif
  allocate(tv_bg(nlons,nlats,nccount(3)),tv_anal(nlons,nlats,nccount(3)))
  tv_bg = ug3d * ( 1.0 + fv*vg3d ) !Convert T to Tv
  call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading pressfc'
     call stop2(29)
  endif
  if (allocated(values_1d)) deallocate(values_1d)
  allocate(values_1d(nlons*nlats))
  values_1d = reshape(values_2d,(/nlons*nlats/))
  ! add Tv,q increment to background
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     ug = 0_r_kind
     if (tv_ind > 0) then
       call copyfromgrdin(grdin(:,levels(tv_ind-1)+krev,nb,ne),ug)
     endif
     vg = 0_r_kind
     if (q_ind > 0) then
       call copyfromgrdin(grdin(:,levels(q_ind-1)+krev,nb,ne),vg)
     endif
     values_2d = reshape(ug,(/nlons,nlats/))
     tv_anal(:,:,ki) = tv_bg(:,:,ki) + values_2d
     values_2d = reshape(vg,(/nlons,nlats/))
     vg3d(:,:,ki) = vg3d(:,:,ki) + values_2d
  enddo
  ! now tv_anal is analysis Tv, vg3d is analysis spfh
  if (cliptracers)  where (vg3d < clip) vg3d = clip

  ! write analysis T
  allocate(values_3d(nlons,nlats,nccount(3)))
  allocate(tmp_anal(nlons,nlats,nccount(3)))
  tmp_anal = tv_anal/(1. + fv*vg3d) ! convert Tv back to T, save q as vg3d
  values_3d = tmp_anal
  if (has_attr(dsfg, 'nbits', 'tmp') .and. .not. nocompress) then
    call read_attribute(dsfg, 'nbits', nbits, 'tmp')
    call quantize_data(tmp_anal, values_3d, nbits, compress_err)
    ! yet again, below not technically correct...
    call write_attribute(dsanl,&
    'max_abs_compression_error',compress_err,'tmp',errcode=iret)
    if (iret /= 0) then
      print *,'error writing tmp attribute'
      call stop2(29)
    end if
  endif
  call write_vardata(dsanl,'tmp',values_3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write T
  if (iret /= 0) then
     print *,'error writing tmp'
     call stop2(29)
  endif
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)

  ! write analysis delz
  if (has_var(dsfg,'delz')) then
     allocate(delzb(nlons*nlats))
     call read_vardata(dsfg, 'delz', values_3d, ncstart=ncstart, nccount=nccount, errcode=iret)
     vg = 0_r_kind
     if (ps_ind > 0) then
        call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),vg)
     endif
     vg = values_1d + (vg*100_r_kind) ! analysis ps (values_1d is background ps)
     do k=lev_pe1(iope), lev_pe2(iope)
        krev = nlevs-k+1 ! k=1 is model top
        ki = k - lev_pe1(iope) + 1
        ug=(rd/grav)*reshape(tv_anal(:,:,ki),(/nlons*nlats/))
        ! ps in Pa here, need to multiply ak by 100.
        ! ug is hydrostatic analysis delz (should be negative)
        ! (note that ak,bk are already reversed to go from bottom to top)
        ug=ug*log((100_r_kind*ak(krev+1)+bk(krev+1)*vg)/(100_r_kind*ak(krev)+bk(krev)*vg))
        ! ug is hydrostatic analysis delz inferred from analysis ps,Tv
        ! delzb is (negative) hydrostatic background delz inferred from background ps,Tv
        delzb=(rd/grav)*reshape(tv_bg(:,:,ki),(/nlons*nlats/))
        delzb=delzb*log((100_r_kind*ak(krev+1)+bk(krev+1)*values_1d)/(100_r_kind*ak(krev)+bk(krev)*values_1d))
        ug3d(:,:,ki)=values_3d(:,:,ki) + reshape(ug-delzb,(/nlons,nlats/)) 
     end do
     if (has_attr(dsfg, 'nbits', 'delz') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'delz')
       values_3d = ug3d
       call quantize_data(values_3d, ug3d, nbits, compress_err)
       ! again, only the first PE's error is being accounted for in this attribute
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'delz',errcode=iret)
       if (iret /= 0) then
         print *,'error writing delz attribute'
         call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'delz',ug3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write delz
     if (iret /= 0) then
        print *,'error writing delz'
        call stop2(29)
     endif
  endif
  deallocate(tv_anal,tv_bg) ! keep tmp_anal

  ! write analysis q (still stored in vg3d)
  if (has_attr(dsfg, 'nbits', 'spfh') .and. .not. nocompress) then
    call read_attribute(dsfg, 'nbits', nbits, 'spfh')
    values_3d = vg3d
    call quantize_data(values_3d, vg3d, nbits, compress_err)
    call write_attribute(dsanl,&
    'max_abs_compression_error',compress_err,'spfh',errcode=iret)
    if (iret /= 0) then
      print *,'error writing spfh attribute'
      call stop2(29)
    end if
  endif
  call write_vardata(dsanl,'spfh',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write q
  if (iret /= 0) then
     print *,'error writing spfh'
     call stop2(29)
  endif
  ! write clwmr, icmr
  call read_vardata(dsfg,'clwmr',ug3d,ncstart=ncstart,nccount=nccount,errcode=iret)
  if (iret /= 0) then
     print *,'error reading clwmr'
     call stop2(29)
  endif
  if (imp_physics == 11) then
     call read_vardata(dsfg,'icmr',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret)
     if (iret /= 0) then
        print *,'error reading icmr'
        call stop2(29)
     endif
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     ug = 0_r_kind
     if (cw_ind > 0) then
        call copyfromgrdin(grdin(:,levels(cw_ind-1)+krev,nb,ne),ug)
     endif
     if (imp_physics == 11) then
        work = -r0_05 * (reshape(tmp_anal(:,:,ki),(/nlons*nlats/)) - t0c)
        do i=1,nlons*nlats
           work(i) = max(zero,work(i))
           work(i) = min(one,work(i))
        enddo
        vg = ug * work          ! cloud ice
        ug = ug * (one - work)  ! cloud water
        vg3d(:,:,ki) = vg3d(:,:,ki) + reshape(vg,(/nlons,nlats/))
     endif
     ug3d(:,:,ki) = ug3d(:,:,ki) + reshape(ug,(/nlons,nlats/))
  enddo
  deallocate(tmp_anal)
  if (cw_ind > 0) then
     if (has_attr(dsfg, 'nbits', 'clwmr') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'clwmr')
       values_3d = ug3d
       call quantize_data(values_3d, ug3d, nbits, compress_err)
       if (cliptracers)  where (ug3d < clip) ug3d = clip
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'clwmr',errcode=iret)
       if (iret /= 0) then
          print *,'error writing clwmr attribute'
          call stop2(29)
       end if
     endif
     if (cliptracers)  where (ug3d < clip) ug3d = clip
  endif
  call write_vardata(dsanl,'clwmr',ug3d,ncstart=ncstart,nccount=nccount,errcode=iret)
  if (iret /= 0) then
     print *,'error writing clwmr'
     call stop2(29)
  endif
  if (imp_physics == 11) then
     if (cw_ind > 0) then
        if (has_attr(dsfg, 'nbits', 'clwmr') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'clwmr')
          values_3d = vg3d
          call quantize_data(values_3d, vg3d, nbits, compress_err)
          if (cliptracers)  where (vg3d < clip) vg3d = clip
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'icmr',errcode=iret)
          if (iret /= 0) then
             print *,'error writing icmr attribute'
             call stop2(29)
          end if
        endif
        if (cliptracers)  where (vg3d < clip) vg3d = clip
     endif
     call write_vardata(dsanl,'icmr',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write icmr
     if (iret /= 0) then
        print *,'error writing icmr'
        call stop2(29)
     endif
  endif

  ! write analysis ozone
  call read_vardata(dsfg,'o3mr',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret)
  if (iret /= 0) then
     print *,'error reading o3mr'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     ug = 0_r_kind
     if (oz_ind > 0) then
        call copyfromgrdin(grdin(:,levels(oz_ind-1)+krev,nb,ne),ug)
     endif
     vg3d(:,:,ki) = vg3d(:,:,ki) + reshape(ug,(/nlons,nlats/))
  enddo
  if (oz_ind > 0) then
     if (has_attr(dsfg, 'nbits', 'o3mr') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'o3mr')
       values_3d = vg3d
       call quantize_data(values_3d, vg3d, nbits, compress_err)
       if (cliptracers)  where (vg3d < clip) vg3d = clip
       ! again, below is lazy/not ideal/bad
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'o3mr',errcode=iret)
       if (iret /= 0) then
          print *,'error writing o3mr attribute'
          call stop2(29)
       end if
     endif
     if (cliptracers)  where (vg3d < clip) vg3d = clip
  endif
  call write_vardata(dsanl,'o3mr',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write o3mr
  if (iret /= 0) then
     print *,'error writing o3mr'
     call stop2(29)
  end if

  if (allocated(delzb)) deallocate(delzb)
  if (imp_physics == 11 .and. (.not. use_full_hydro)) deallocate(work)

  if (pst_ind > 0) then

     vmassdivinc = vmassdiv - vmassdivinc ! analyis - first guess VIMFD
     ! (VIMFD = vertically integrated mass flux divergence)
     pstend1 = sum(vmassdiv,2)
     if (nanal .eq. 1 .and. iope==0) then
     print *,nanal,'min/max analysis ps tend',minval(pstend1),maxval(pstend1)
     print *,nanal,'min/max analyzed ps tend',minval(pstend2),maxval(pstend2)
     endif
     ! vmass is vertical integral of dp**2
     do k=1,nlevs
        ! case 2 (4.3.1.2) in GEOS DAS document.
        ! (adjustment proportional to mass in layer)
        vmass = vmass + dpanl(:,k)**2
        ! case 3 (4.3.1.3) in GEOS DAS document.
        ! (adjustment propotional to mass-flux div increment)
        !vmass = vmass + vmassdivinc(:,k)**2
     enddo
     ! adjust wind field in analysis so pstend is consistent with pstend2
     ! (analyzed pstend)
!$omp parallel do private(k,nt,ug,vg,uginc,vginc,vrtspec,divspec)  shared(vmassdiv,vmassdivinc,dpanl)
     do k=1,nlevs
        ! case 2
        ug = (pstend2 - pstend1)*dpanl(:,k)**2/vmass
        ! case 3
        !ug = (pstend2 - pstend1)*vmassdivinc(:,k)**2/vmass
        call sptez_s(divspec,ug,-1) ! divgrd to divspec
        vrtspec = 0_r_kind
        call sptezv_s(divspec,vrtspec,uginc,vginc,1) ! div,vrt to u,v
        if (nanal .eq. 1 .and. iope==0) then
          print *,k,'min/max u inc (member 1)',&
          minval(uginc/dpanl(:,k)),maxval(uginc/dpanl(:,k))
        endif
        ugtmp(:,k) = (ugtmp(:,k)*dpanl(:,k) + uginc)/dpanl(:,k)
        vgtmp(:,k) = (vgtmp(:,k)*dpanl(:,k) + vginc)/dpanl(:,k)
        ug = ugtmp(:,k); vg = vgtmp(:,k)
        ug = ug*dpanl(:,k); vg = vg*dpanl(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo
!$omp end parallel do
     ! should be same as analyzed ps tend
     psfg = sum(vmassdiv,2)
     if (nanal .eq. 1 .and. iope==0) then
     print *,nanal,'min/max adjusted ps tend',minval(psfg),maxval(psfg)
     print *,nanal,'min/max diff between adjusted and analyzed ps tend',&
             minval(pstend2-psfg),maxval(pstend2-psfg)
     endif

     do k=lev_pe1(iope), lev_pe2(iope)
        krev = nlevs-k+1
        ki = k - lev_pe1(iope) + 1
        ug3d(:,:,ki) = reshape(ugtmp(:,krev),(/nlons,nlats/))
        vg3d(:,:,ki) = reshape(vgtmp(:,krev),(/nlons,nlats/))
     enddo
     if (has_attr(dsfg, 'nbits', 'ugrd') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'ugrd')
       values_3d = ug3d
       call quantize_data(values_3d, ug3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'ugrd',errcode=iret)
       if (iret /= 0) then
          print *,'error writing ugrd attribute'
          call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'ugrd',ug3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write u
     if (iret /= 0) then
        print *,'error writing ugrd'
        call stop2(29)
     endif
     if (has_attr(dsfg, 'nbits', 'vgrd') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'vgrd')
       values_3d = vg3d
       call quantize_data(values_3d, vg3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'vgrd',errcode=iret)
       if (iret /= 0) then
          print *,'error writing vgrd attribute'
          call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'vgrd',vg3d,ncstart=ncstart,nccount=nccount,errcode=iret) ! write v
     if (iret /= 0) then
        print *,'error writing vgrd'
        call stop2(29)
     endif
     deallocate(ugtmp,vgtmp)
     deallocate(ugtmp2,vgtmp2)
  endif

  if (allocated(ug3d)) deallocate(ug3d)
  if (allocated(vg3d)) deallocate(vg3d)
  if (allocated(values_3d)) deallocate(values_3d)
  if (allocated(values_2d)) deallocate(values_2d)
  if (allocated(values_1d)) deallocate(values_1d)

  if (pst_ind > 0) then
     deallocate(pressi,dpanl,dpfg)
     deallocate(pstend1,pstend2,pstendfg,vmass)
     deallocate(vmassdiv)
     deallocate(vmassdivinc)
  endif
  call close_dataset(dsfg,errcode=iret)
  if (iret/=0) then
     write(6,*)'gridio/writegriddata: gfs model: problem closing netcdf fg dataset, iret=',iret
     call stop2(23)
  endif
  call close_dataset(dsanl,errcode=iret)
  if (iret/=0) then
     write(6,*)'gridio/writegriddata: gfs model: problem closing netcdf anal dataset, iret=',iret
     call stop2(23)
  endif

  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  end do backgroundloop ! loop over backgrounds to write out
  ! remove the sub communicators
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  call mpi_comm_free(iocomms(mem_pe(nproc)), iret)
  call mpi_barrier(mpi_comm_world, iret)

  return

 contains
! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copyfromgrdin(grdin, field)
 implicit none

 real(r_single), dimension(:), intent(in)      :: grdin
 real(r_kind), dimension(:), intent(inout) :: field

 if (reducedgrid) then
   call reducedtoreg(grdin, field)
 else
   field = grdin
 endif

 end subroutine copyfromgrdin

 end subroutine writegriddata_pnc



 subroutine writegriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  use netcdf
  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_axdata, &
                          sigio_aldata, sigio_swohdc
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_readrec,nemsio_writerec,nemsio_intkind,nemsio_charkind,&
                           nemsio_getheadvar,nemsio_realkind,nemsio_getfilehead,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         read_attribute, close_dataset, get_dim, read_vardata,&
                         create_dataset, get_idate_from_time_units, &
                         get_time_units_from_idate, write_vardata, &
                         write_attribute, quantize_data, has_var, has_attr
  use constants, only: grav
  use params, only: nbackgrounds,anlfileprefixes,fgfileprefixes,reducedgrid,&
                    nccompress,write_ensmean
  implicit none

  integer, intent(in) :: nanal1,nanal2
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d,n3d,ndim
  integer, dimension(0:n3d), intent(in) :: levels
  real(r_single), dimension(npts,ndim,nbackgrounds,nanal2-nanal1+1), intent(inout) :: grdin
  logical, intent(in) :: no_inflate_flag
  logical:: use_full_hydro
  character(len=500):: filenamein, filenameout
  real(r_kind), allocatable, dimension(:,:) :: vmassdiv,dpanl,dpfg,pressi
  real(r_kind), allocatable, dimension(:,:) :: vmassdivinc
  real(r_kind), allocatable, dimension(:,:) :: ugtmp,vgtmp
  real(r_kind), allocatable,dimension(:) :: pstend1,pstend2,pstendfg,vmass
  real(r_kind), dimension(nlons*nlats) :: ug,vg,uginc,vginc,psfg,psg
  real(r_kind), allocatable, dimension(:) :: delzb,work,values_1d
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec
  real(r_single), allocatable, dimension(:,:,:) :: &
     ug3d,vg3d,values_3d,tmp_anal,tv_anal,tv_bg
  real(r_single), allocatable, dimension(:,:) :: values_2d
  integer iadate(4),idate(4),nfhour,idat(7),iret,nrecs,jdate(7),jdat(6)
  integer:: nfminute, nfsecondn, nfsecondd
  integer,dimension(8):: ida,jda
  real(r_double),dimension(5):: fha
  real(r_kind) fhour
  type(sigio_head) sighead
  type(sigio_data) sigdata_inc
  type(Dataset) :: dsfg, dsanl
  character(len=3) charnanal
  character(nemsio_charkind),allocatable:: recname(:)
  character(nemsio_charkind) :: field
  character(len=nf90_max_name) :: time_units
  logical :: hasfield
  character(len=max_varname_length), dimension(n3d) :: no_vars3d

  real(r_kind) kap,kapr,kap1,clip
  real(r_single) compress_err
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  real(r_kind), dimension(nlevs+1) :: ak,bk
  real(nemsio_realkind), dimension(nlevs+1,3,2) :: nems_vcoord
  integer(nemsio_intkind) :: nems_idvc
  type(sigio_data) sigdata
  type(nemsio_gfile) :: gfilein,gfileout

  integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind, nbits
  integer :: ql_ind, qi_ind, qr_ind, qs_ind, qg_ind

  integer k,krev,nt,ierr,iunitsig,nb,i,ne,nanal

  logical :: nocompress
  logical :: write_sfc_file, write_atm_file

  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, write_sfc_file, write_atm_file)

  if (write_sfc_file ) then
        ! adding the sfc increments requires adjusting several other variables. This is done is a separate
        ! program.
        if (nproc == 0) write(6,*)'gridio/writegriddata: not coded to write sfc analysis, will write increment for sfc fields'
        no_vars3d=''
        call  writeincrement(nanal1,nanal2,no_vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  endif

  nocompress = .true.
  if (nccompress) nocompress = .false.
  use_full_hydro = .false.
  iunitsig = 78
  kapr = cp/rd
  kap = rd/cp
  kap1 = kap+one
  clip = tiny_r_kind

  ne = 0
  ensmemloop: do nanal=nanal1,nanal2
  ne = ne + 1
  write(charnanal,'(i3.3)') nanal
  backgroundloop: do nb=1,nbackgrounds

  if (nanal == 0 .and. write_ensmean) then
     filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"ensmean"
     filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"ensmean"
  else
     if(no_inflate_flag) then
       filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"nimem"//charnanal
     else
       filenameout = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal
     end if
     filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal
  endif

  if (use_gfs_nemsio) then
     clip = tiny(vg(1))
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_init, iret=',iret
        call stop2(23)
     end if
     call nemsio_open(gfilein,filenamein,'READ',iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_open, iret=',iret
        call stop2(23)
     endif
     call nemsio_getfilehead(gfilein,iret=iret,idate=idat,nfhour=nfhour,&
                             nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd,&
                             nrec=nrecs,&
                             vcoord=nems_vcoord,idvc=nems_idvc)
!     write(6,111) trim(filenamein),idat,nfhour,nfminute,nfsecondn,nfsecondd
!111  format(a32,1x,'idat=',7(i4,1x),' nfh=',i5,' nfm=',i5,' nfsn=',i5,' nfsd=',i5)

     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_getfilehead, iret=',iret
        call stop2(23)
     endif

     allocate(recname(nrecs))
     call nemsio_getfilehead(gfilein,iret=iret,recname=recname)
     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_getfilehead, iret=',iret
        call stop2(23)
     endif

     if (nems_idvc == 1) then ! sigma coordinate
         ak = zero
         bk = nems_vcoord(1:nlevs+1,2,1)
     else if (nems_idvc == 2 .or. nems_idvc == 3) then ! hybrid coordinate
         bk = nems_vcoord(1:nlevs+1,2,1)
         ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',nems_idvc
         call stop2(23)
     end if
  else if (use_gfs_ncio) then
     clip = tiny(vg(1))
     dsfg = open_dataset(filenamein)
     jdat = get_idate_from_time_units(dsfg)
     idat(4) = jdat(1) ! yr
     idat(2) = jdat(2) ! mon
     idat(3) = jdat(3) ! day
     idat(1) = jdat(4) ! hr
     call read_vardata(dsfg,'time',values_1d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading time'
        call stop2(29)
     endif
     nfhour = int(values_1d(1))
     nems_idvc=2
     call read_attribute(dsfg, 'ak', values_1d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading ak'
        call stop2(29)
     endif
     do k=1,nlevs+1
        ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
        ak(nlevs-k+2) = 0.01_r_kind*values_1d(k)
     enddo
     call read_attribute(dsfg, 'bk', values_1d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading bk'
        call stop2(29)
     endif
     do k=1,nlevs+1
        ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
        bk(nlevs-k+2) = values_1d(k)
     enddo
  else
     ! read in first-guess data.
     call sigio_srohdc(iunitsig,trim(filenamein), &
                       sighead,sigdata,ierr)
     if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
         ak = zero
         bk = sighead%si(1:nlevs+1)
     else if (sighead%idvc == 1) then ! sigma coordinate
         ak = zero
         bk = sighead%vcoord(1:nlevs+1,2)
     else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
         bk = sighead%vcoord(1:nlevs+1,2)
         ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
     else
         print *,'unknown vertical coordinate type',sighead%idvc
         call stop2(20)
     end if
  endif

  u_ind   = getindex(vars3d, 'u')   !< indices in the control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                    ! old logical massbal_adjust, if non-zero
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0 .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )


!  if (nproc == 0) then
!    print *, 'indices: '
!    print *, 'u: ', u_ind, ', v: ', v_ind, ', tv: ', tv_ind
!    print *, 'q: ', q_ind, ', oz: ', oz_ind, ', cw: ', cw_ind
!    print *, 'ps: ', ps_ind, ', pst: ', pst_ind
!  endif

  if (pst_ind > 0) then
     allocate(vmassdiv(nlons*nlats,nlevs))
     allocate(vmassdivinc(nlons*nlats,nlevs))
     allocate(dpfg(nlons*nlats,nlevs))
     allocate(dpanl(nlons*nlats,nlevs))
     allocate(pressi(nlons*nlats,nlevs+1))
     allocate(pstendfg(nlons*nlats))
     allocate(pstend1(nlons*nlats))
     allocate(pstend2(nlons*nlats),vmass(nlons*nlats))
     allocate(ugtmp(nlons*nlats,nlevs),vgtmp(nlons*nlats,nlevs))
  endif
! if (imp_physics == 11) allocate(work(nlons*nlats))    !orig
  if (imp_physics == 11 .and. (.not. use_full_hydro) ) allocate(work(nlons*nlats))

! Compute analysis time from guess date and forecast length.
  if (.not. use_gfs_nemsio .and. .not. use_gfs_ncio) then
     idate = sighead%idate
     fhour = sighead%fhour
  else if (use_gfs_ncio) then
     idate(3)=idat(3)  !day
     idate(2)=idat(2)  !mon
     idate(4)=idat(4)  !yr
     idate(1)=idat(1)  !hr
     fhour = nfhour
  else if (use_gfs_nemsio) then
     idate(3)=idat(3)
     idate(2)=idat(2)
     idate(4)=idat(1)
     idate(1)=idat(4)
     fhour = nfhour
  endif
  fha=zero; ida=0; jda=0
  fha(2)=fhour    ! relative time interval in hours
  ida(1)=idate(4) ! year
  ida(2)=idate(2) ! month
  ida(3)=idate(3) ! day
  ida(4)=0                ! time zone
  ida(5)=idate(1) ! hour
  call w3movdat(fha,ida,jda)
!
!   INPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!                (JDAT IS LATER THAN IDAT IF TIME INTERVAL IS POSITIVE.)
  iadate(1)=jda(5) ! hour
  iadate(2)=jda(2) ! mon
  iadate(3)=jda(3) ! day
  iadate(4)=jda(1) ! year
  if (nproc .eq. 0) then
     print *,'idate = ',idate
     print *,'iadate = ',iadate
  end if

  if (.not. use_gfs_nemsio .and. .not. use_gfs_ncio) then ! spectral sigio
     sighead%idate = iadate
     sighead%fhour = zero
     ! ensemble info
     ! http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
     sighead%iens(1) = 3 ! pos pert
     sighead%iens(2) = nanal ! ensemble member number
     sighead%icen2 = 2 ! sub-center, must be 2 or ens info not used
     if (.not. isinitialized) call init_spec_vars(nlons,nlats,sighead%jcap,4)
     ! allocate new sigdata structure for increments.
     call sigio_aldata(sighead,sigdata_inc,ierr)
     ! convert to increment to spectral coefficients.
!$omp parallel do private(k,nt,ug,vg,divspec,vrtspec)  shared(grdin,sigdata_inc)
     do k=1,nlevs
        ug = 0_r_kind
        if (u_ind > 0 ) then
          call copyfromgrdin(grdin(:,levels(u_ind-1) + k,nb,ne),ug)
        endif
        vg = 0_r_kind
        if (v_ind > 0) then
          call copyfromgrdin(grdin(:,levels(v_ind-1) + k,nb,ne),vg)
        endif
        call sptezv_s(divspec,vrtspec,ug,vg,-1)
        sigdata_inc%d(:,k) = divspec
        sigdata_inc%z(:,k) = vrtspec

        ug = 0_r_kind
        if (tv_ind > 0) then
          call copyfromgrdin(grdin(:,levels(tv_ind-1)+k,nb,ne),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%t(:,k) = divspec

        ug = 0_r_kind
        if (q_ind > 0) then
          call copyfromgrdin(grdin(:,levels(q_ind-1)+k,nb,ne),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,1) = divspec

        ug = 0_r_kind
        if (oz_ind > 0) then
          call copyfromgrdin(grdin(:,levels(oz_ind-1)+k,nb,ne),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,2) = divspec

        ug = 0_r_kind
        if (cw_ind > 0) then
          call copyfromgrdin(grdin(:,levels(cw_ind-1)+k,nb,ne),ug)
        endif
        call sptez_s(divspec,ug,-1)
        sigdata_inc%q(:,k,3) = divspec

     enddo
!$omp end parallel do

     divspec = sigdata%ps
     call sptez_s(divspec,vg,1)
     ! increment (in hPa) to reg grid.
     ug = 0_r_kind
     if (ps_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),ug)
     endif
     psfg = 10._r_kind*exp(vg)
     vg = psfg + ug ! first guess + increment
     psg = vg
     vg = log(vg/10._r_kind) ! convert back to centibars.
     call sptez_s(divspec,vg,-1)
     sigdata%ps = divspec

  else if (use_gfs_nemsio) then ! nemsio
     gfileout = gfilein

     nfhour    = 0        !  new forecast hour, zero at analysis time
     nfminute  = 0
     nfsecondn = 0
     nfsecondd = 100      ! default for denominator

     !iadate = hh/mm/dd/yyyy
     !jdate = yyyy/mm/dd/hh/min/secn/secd

     jdate(1) = iadate(4)  ! analysis year
     jdate(2) = iadate(2)  ! analysis month
     jdate(3) = iadate(3)  ! analysis day
     jdate(4) = iadate(1)  ! analysis hour
     jdate(5) = nfminute   ! analysis minute
     jdate(6) = nfsecondn  ! analysis scaled seconds
     jdate(7) = nfsecondd  ! analysis seconds multiplier

     call nemsio_open(gfileout,filenameout,'WRITE',iret=iret,&
          idate=jdate, nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, &
          nfsecondd=nfsecondd)

!     write(6,112) trim(filenameout),jdate,nfhour,nfminute,nfsecondn,nfsecondd
!112 format(a32,1x,'jdate=',7(i4,1x),' nfh=',i5,' nfm=',i5,' nfsn=',i5,' nfsd=',i5)

     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_open for output, iret=',iret
        call stop2(23)
     end if

!    read/write orographay
     call nemsio_readrecv(gfilein,'hgt','sfc',1,nems_wrk,iret=iret)
     call nemsio_writerecv(gfileout,'hgt','sfc',1,nems_wrk,iret=iret)

     call nemsio_readrecv(gfilein,'pres','sfc',1,nems_wrk,iret=iret)
     psfg = 0.01*nems_wrk ! convert ps to millibars.
     ! increment (in hPa) to reg grid.
     ug = 0_r_kind
     if (ps_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),ug)
     endif
     !print *,'nanal,min/max psfg,min/max inc',nanal,minval(psfg),maxval(psfg),minval(ug),maxval(ug)
     field = 'dpres'; hasfield = checkfield(field,recname,nrecs)
     if (hasfield) then
        do k=1,nlevs
           psg = ug*(bk(k)-bk(k+1))
           call nemsio_readrecv(gfilein,'dpres','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(dpres), iret=',iret
              call stop2(23)
           endif
           nems_wrk = nems_wrk + 100_r_kind*psg
           call nemsio_writerecv(gfileout,'dpres','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(dpres), iret=',iret
              call stop2(23)
           endif
        enddo
     endif
     psg = psfg + ug ! first guess + increment
     nems_wrk = 100_r_kind*psg
     ! write out updated surface pressure.
     call nemsio_writerecv(gfileout,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(pres), iret=',iret
        call stop2(23)
     endif
  else if (use_gfs_ncio) then
     dsanl = create_dataset(filenameout, dsfg, copy_vardata=.true., nocompress=nocompress, errcode=iret)
     if (iret /= 0) then
        print *,'error creating netcdf file'
        call stop2(29)
     endif
     deallocate(values_1d)
     allocate(values_1d(1))
     values_1d(1)=zero
     call write_vardata(dsanl,'time',values_1d,errcode=iret)
     if (iret /= 0) then
        print *,'error writing time'
        call stop2(29)
     endif
     jdat(1) = iadate(4)
     jdat(2) = iadate(2)
     jdat(3) = iadate(3)
     jdat(4) = iadate(1)
     jdat(5) = jda(6); jdat(6) = jda(7)
     time_units = get_time_units_from_idate(jdat)
     call write_attribute(dsanl,'units',time_units,'time',errcode=iret)
     if (iret /= 0) then
        print *,'error writing time units attribute'
        call stop2(29)
     endif
     call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading pressfc'
        call stop2(29)
     endif
     psfg = 0.01*reshape(values_2d,(/nlons*nlats/))
     ug = 0_r_kind
     if (ps_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),ug)
     endif
     ! add increment to background.
     psg = psfg + ug ! analysis pressure in mb.
     values_2d = 100.*reshape(psg,(/nlons,nlats/))
     call write_vardata(dsanl,'pressfc',values_2d,errcode=iret)
     if (iret /= 0) then
        print *,'error writing pressfc'
        call stop2(29)
     endif
     !print *,'nanal,min/max psfg,min/max inc',nanal,minval(values_2d),maxval(values_2d),minval(ug),maxval(ug)
     if (has_var(dsfg,'dpres')) then
        call read_vardata(dsfg,'dpres',ug3d)
        allocate(vg3d(nlons,nlats,nlevs))
        ! infer dpres increment from ps increment
        do k=1,nlevs
           vg = ug*(bk(k)-bk(k+1)) ! ug is ps increment
           vg3d(:,:,nlevs-k+1) = ug3d(:,:,nlevs-k+1) +&
           100_r_kind*reshape(vg,(/nlons,nlats/))
        enddo
        if (has_attr(dsfg, 'nbits', 'dpres') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'dpres')
          ug3d = vg3d
          call quantize_data(ug3d, vg3d, nbits, compress_err)
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'dpres',errcode=iret)
          if (iret /= 0) then
            print *,'error writing dpres attribute'
            call stop2(29)
          endif
        endif
        call write_vardata(dsanl,'dpres',vg3d,errcode=iret)
        if (iret /= 0) then
           print *,'error writing dpres'
           call stop2(29)
        endif
     endif
  endif

  if (pst_ind > 0) then
     !==> first guess pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psfg ! psfg in mb, ak has been scaled by 0.01
     enddo
     do k=1,nlevs
        dpfg(:,k) = pressi(:,k)-pressi(:,k+1)
     enddo
     !==> analysis pressure at interfaces.
     do k=1,nlevs+1
        pressi(:,k)=ak(k)+bk(k)*psg ! psg in mb, ak has been scaled by 0.01
     enddo
     do k=1,nlevs
        dpanl(:,k) = pressi(:,k)-pressi(:,k+1)
        !if (nanal .eq. 1) print *,'k,dpanl,dpfg',minval(dpanl(:,k)),&
        !maxval(dpanl(:,k)),minval(dpfg(:,k)),maxval(dpfg(:,k))
     enddo
     if (use_gfs_ncio) then
        call read_vardata(dsfg,'ugrd',ug3d,errcode=iret)
        if (iret /= 0) then
           print *,'error reading ugrd'
           call stop2(29)
        endif
        call read_vardata(dsfg,'vgrd',vg3d,errcode=iret)
        if (iret /= 0) then
           print *,'error reading vgrd'
           call stop2(29)
        endif
     endif
     do k=1,nlevs
!       re-calculate vertical integral of mass flux div for first-guess
        if (use_gfs_nemsio) then
           call nemsio_readrecv(gfilein,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
               write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
               call stop2(23)
           endif
           ug = nems_wrk
           call nemsio_readrecv(gfilein,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
               write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
               call stop2(23)
           endif
           vg = nems_wrk
        else if (use_gfs_ncio) then
           ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
           vg = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
        else
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
        endif
        ug = ug*dpfg(:,k)
        vg = vg*dpfg(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo

     ! analyzed ps tend increment
     call copyfromgrdin(grdin(:,levels(n3d) + pst_ind,nb,ne),pstend2)
     pstendfg = sum(vmassdiv,2)
     vmassdivinc = vmassdiv
     if (nanal .eq. 1) then
     print *,'time level ',nb
     print *,'--------------------'
     print *,nanal,'min/max pstendfg',minval(pstendfg),maxval(pstendfg)
     print *,nanal,'min/max pstend inc',minval(pstend2),maxval(pstend2)
     endif
     pstend2 = pstend2 + pstendfg ! add to background ps tend

  endif ! if pst_ind > 0

  if (.not. use_gfs_nemsio .and. .not. use_gfs_ncio) then
  ! add increment to first guess in spectral space.
!$omp parallel do private(k,nt,ug,vg,vrtspec,divspec)  shared(sigdata,sigdata_inc,vmassdiv,dpanl)
     do k=1,nlevs

! add increments in spectral space
        sigdata%z(:,k) = sigdata%z(:,k) + sigdata_inc%z(:,k)
        sigdata%d(:,k) = sigdata%d(:,k) + sigdata_inc%d(:,k)
        sigdata%t(:,k) = sigdata%t(:,k) + sigdata_inc%t(:,k)
        do nt=1,sighead%ntrac
           sigdata%q(:,k,nt) = sigdata%q(:,k,nt) + sigdata_inc%q(:,k,nt)
        enddo

        if (pst_ind > 0) then
!          calculate vertical integral of mass flux div for updated state
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
           ug = ug*dpanl(:,k)
           vg = vg*dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        endif

     enddo
!$omp end parallel do

     ! don't need sigdata_inc anymore.
     call sigio_axdata(sigdata_inc,ierr)
  else if (use_gfs_nemsio) then
     field = 'delz'; hasfield = checkfield(field,recname,nrecs)
     if (hasfield) allocate(delzb(nlons*nlats))
     ! update u,v,Tv,q,oz,clwmr
     do k=1,nlevs
        call nemsio_readrecv(gfilein,'ugrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(ugrd), iret=',iret
           call stop2(23)
        endif
        ug = 0_r_kind
        if (u_ind > 0) then
          call copyfromgrdin(grdin(:,levels(u_ind-1) + k,nb,ne),ug)
        endif
        ug =  nems_wrk + ug
        if (pst_ind < 0) then
           nems_wrk = ug
           call nemsio_writerecv(gfileout,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(ugrd), iret=',iret
              call stop2(23)
           endif
        else
           ugtmp(:,k) = ug ! save analysis u if pst_ind>0
        endif

        call nemsio_readrecv(gfilein,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(vgrd), iret=',iret
           call stop2(23)
        endif
        vg = 0_r_kind
        if (v_ind > 0) then
           call copyfromgrdin(grdin(:,levels(v_ind-1) + k,nb,ne),vg)
        endif
        vg =  nems_wrk + vg
        if (pst_ind < 0) then
           nems_wrk = vg
           call nemsio_writerecv(gfileout,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(vgrd), iret=',iret
              call stop2(23)
           endif
        else
           vgtmp(:,k) = vg ! save analysis v if pst_ind>0
        endif

        if (pst_ind > 0) then
           ug = ug*dpanl(:,k)
           vg = vg*dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        end if

        call nemsio_readrecv(gfilein,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
        call nemsio_readrecv(gfilein,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif
        nems_wrk = nems_wrk * ( 1.0 + fv*nems_wrk2 ) !Convert T to Tv
        ug = 0_r_kind
        if (tv_ind > 0) then
          call copyfromgrdin(grdin(:,levels(tv_ind-1)+k,nb,ne),ug)
        endif
        vg = 0_r_kind
        if (q_ind > 0) then
          call copyfromgrdin(grdin(:,levels(q_ind-1)+k,nb,ne),vg)
        endif
        ! ug is Tv increment, nems_wrk is background Tv, nems_wrk2 is background spfh
        ug = ug + nems_wrk
        vg = vg + nems_wrk2
        if (cliptracers)  where (vg < clip) vg = clip
        field = 'delz'; hasfield = checkfield(field,recname,nrecs)
        if (hasfield) then
           call nemsio_readrecv(gfilein,'pres','sfc',1,nems_wrk2,iret=iret)
           delzb=(rd/grav)*nems_wrk
           ! ps in Pa here, need to multiply ak by 100. k is bottom to top,
           ! calculate delzb so it is negative.
           delzb=delzb*log((100_r_kind*ak(k+1)+bk(k+1)*nems_wrk2)/(100_r_kind*ak(k)+bk(k)*nems_wrk2))
        endif
        ! convert Tv back to T
        nems_wrk = ug/(1. + fv*vg)
    !   if (imp_physics == 11) then  !orig
        if (imp_physics == 11 .and. (.not. use_full_hydro) ) then
           do i=1,nlons*nlats  ! compute work for cloud water partitioning
              work(i) = -r0_05 * (nems_wrk(i) - t0c)
              work(i) = max(zero,work(i))
              work(i) = min(one,work(i))
           enddo
        endif
        call nemsio_writerecv(gfileout,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(tmp), iret=',iret
           call stop2(23)
        endif
        nems_wrk = vg
        call nemsio_writerecv(gfileout,'spfh','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(spfh), iret=',iret
           call stop2(23)
        endif
        field = 'delz'; hasfield = checkfield(field,recname,nrecs)
        if (hasfield) then
           vg = 0_r_kind
           if (ps_ind > 0) then
              call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),vg)
           endif
           vg = nems_wrk2 + vg
           ug=(rd/grav)*ug ! ug is analysis Tv
           ! ps in Pa here, need to multiply ak by 100.
           ! k is bottom to top, calculate delz so it is negative
           ug=ug*log((100_r_kind*ak(k+1)+bk(k+1)*vg)/(100_r_kind*ak(k)+bk(k)*vg))
           ug=ug-delzb ! analysis - background
           call nemsio_readrecv(gfilein,'delz','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(delz), iret=',iret
              call stop2(23)
           endif
           ! flip sign of delz increment if background is positive.
           if (sum(nems_wrk) > 0.0_r_kind) ug = ug * -1.0_r_kind
           nems_wrk = nems_wrk + ug
           call nemsio_writerecv(gfileout,'delz','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(delz), iret=',iret
              call stop2(23)
           endif
        endif

        call nemsio_readrecv(gfilein,'o3mr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(o3mr), iret=',iret
           call stop2(23)
        endif
        ug = 0_r_kind
        if (oz_ind > 0) then
           call copyfromgrdin(grdin(:,levels(oz_ind-1)+k,nb,ne),ug)
        endif
        nems_wrk = nems_wrk + ug
        if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
        call nemsio_writerecv(gfileout,'o3mr','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(o3mr), iret=',iret
           call stop2(23)
        endif

        if ( .not. use_full_hydro) then
           call nemsio_readrecv(gfilein,'clwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
              call stop2(23)
           endif
           ug = 0_r_kind
           if (cw_ind > 0) then
              call copyfromgrdin(grdin(:,levels(cw_ind-1)+k,nb,ne),ug)
           endif
           if (imp_physics == 11) then
              call nemsio_readrecv(gfilein,'icmr','mid layer',k,nems_wrk2,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(icmr), iret=',iret
                 call stop2(23)
              endif
              vg = ug * work  !cloud ice
              ug = ug * (one - work)  !cloud water
              nems_wrk2 = nems_wrk2 + vg
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           if (cliptracers.and.imp_physics==11)  where (nems_wrk2 < clip) nems_wrk2 = clip
           call nemsio_writerecv(gfileout,'clwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(clwmr), iret=',iret
              call stop2(23)
           endif
           if (imp_physics == 11) then
              call nemsio_writerecv(gfileout,'icmr','mid layer',k,nems_wrk2,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(icmr), iret=',iret
                 call stop2(23)
              endif

              field = 'rwmr'; hasfield = checkfield(field,recname,nrecs)
              if (hasfield) then
                 call nemsio_readrecv(gfilein,'rwmr','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(rwmr), iret=',iret
                    call stop2(23)
                 endif
                 call nemsio_writerecv(gfileout,'rwmr','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(rwmr), iret=',iret
                    call stop2(23)
                 endif

                 call nemsio_readrecv(gfilein,'snmr','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(snmr), iret=',iret
                    call stop2(23)
                 endif
                 call nemsio_writerecv(gfileout,'snmr','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(snmr), iret=',iret
                    call stop2(23)
                 endif

                 call nemsio_readrecv(gfilein,'grle','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(grle), iret=',iret
                    call stop2(23)
                 endif
                 call nemsio_writerecv(gfileout,'grle','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(grle), iret=',iret
                    call stop2(23)
                 endif
              endif
              call nemsio_readrecv(gfilein,'cld_amt','mid layer',k,nems_wrk2,iret=iret)
              if (iret == 0 ) then
                 call nemsio_writerecv(gfileout,'cld_amt','mid layer',k,nems_wrk2,iret=iret)
                 if (iret/=0) then
                    write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(cld_amt), iret=',iret
                    call stop2(23)
                 endif
              endif
           endif
        else
           ! Update clwmr
           call nemsio_readrecv(gfilein,'clwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(clwmr), iret=',iret
              call stop2(23)
           endif
           ug = 0.
           if (ql_ind > 0) then
              call copyfromgrdin(grdin(:,levels(ql_ind-1)+k,nb,ne),ug)
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           call nemsio_writerecv(gfileout,'clwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(clwmr), iret=',iret
              call stop2(23)
           endif
           ! Update icmr
           call nemsio_readrecv(gfilein,'icmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(icrm), iret=',iret
              call stop2(23)
           endif
           ug = 0.
           if (qi_ind > 0) then
              call copyfromgrdin(grdin(:,levels(qi_ind-1)+k,nb,ne),ug)
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           call nemsio_writerecv(gfileout,'icmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(icmr), iret=',iret
              call stop2(23)
           endif
           ! Update rwmr
           call nemsio_readrecv(gfilein,'rwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(rwmr), iret=',iret
              call stop2(23)
           endif
           ug = 0.
           if (qr_ind > 0) then
              call copyfromgrdin(grdin(:,levels(qr_ind-1)+k,nb,ne),ug)
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           call nemsio_writerecv(gfileout,'rwmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(rwmr), iret=',iret
              call stop2(23)
           endif
           ! Update snmr
           call nemsio_readrecv(gfilein,'snmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(snmr), iret=',iret
              call stop2(23)
           endif
           ug = 0.
           if (qs_ind > 0) then
              call copyfromgrdin(grdin(:,levels(qs_ind-1)+k,nb,ne),ug)
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           call nemsio_writerecv(gfileout,'snmr','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(snmr), iret=',iret
              call stop2(23)
           endif
           ! Update grle
           call nemsio_readrecv(gfilein,'grle','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(grle), iret=',iret
              call stop2(23)
           endif
           ug = 0.
           if (qg_ind > 0) then
              call copyfromgrdin(grdin(:,levels(qg_ind-1)+k,nb,ne),ug)
           endif
           nems_wrk = nems_wrk + ug
           if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
           call nemsio_writerecv(gfileout,'grle','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(grle), iret=',iret
              call stop2(23)
           endif
           ! No enkf update for cld_amt, just copy the background into analysis
           call nemsio_readrecv(gfilein,'cld_amt','mid layer',k,nems_wrk2,iret=iret)
           if (iret == 0 ) then
              call nemsio_writerecv(gfileout,'cld_amt','mid layer',k,nems_wrk2,iret=iret)
              if (iret/=0) then
                 write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(cld_amt), iret=',iret
                 call stop2(23)
              endif
           endif
        endif  ! use_full_hydro
        !Additional variables needed for Unified Post Processor
        field = 'dzdt'; hasfield = checkfield(field,recname,nrecs)
        if (hasfield) then
           call nemsio_readrecv(gfilein,'dzdt','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_readrecv(dzdt), iret=',iret
              call stop2(23)
           endif
           call nemsio_writerecv(gfileout,'dzdt','mid layer',k,nems_wrk2,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(dzdt), iret=',iret
              call stop2(23)
           endif
        endif
    enddo
  else if (use_gfs_ncio) then
     call read_vardata(dsfg,'ugrd',ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading ugrd'
        call stop2(29)
     endif
     do k=1,nlevs
        ug = 0_r_kind
        if (u_ind > 0) then
          call copyfromgrdin(grdin(:,levels(u_ind-1) + k,nb,ne),ug)
        endif
        values_2d = reshape(ug,(/nlons,nlats/))
        ug3d(:,:,nlevs-k+1) = ug3d(:,:,nlevs-k+1) + values_2d
        if (pst_ind > 0) then
           ugtmp(:,k) = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
        endif
     enddo
     if (has_attr(dsfg, 'nbits', 'ugrd') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'ugrd')
       if (.not. allocated(vg3d)) allocate(vg3d(nlons,nlats,nlevs))
       vg3d = ug3d
       call quantize_data(vg3d, ug3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'ugrd',errcode=iret)
       if (iret /= 0) then
         print *,'error writing ugrd attribute'
         call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'ugrd',ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error writing ugrd'
        call stop2(29)
     endif
     call read_vardata(dsfg,'vgrd',vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading vgrd'
        call stop2(29)
     endif
     do k=1,nlevs
        vg = 0_r_kind
        if (v_ind > 0) then
          call copyfromgrdin(grdin(:,levels(v_ind-1) + k,nb,ne),vg)
        endif
        values_2d = reshape(vg,(/nlons,nlats/))
        vg3d(:,:,nlevs-k+1) = vg3d(:,:,nlevs-k+1) + values_2d
        if (pst_ind > 0) then
           vgtmp(:,k) = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
        endif
     enddo
     if (has_attr(dsfg, 'nbits', 'vgrd') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'vgrd')
       ug3d = vg3d
       call quantize_data(ug3d, vg3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'vgrd',errcode=iret)
       if (iret /= 0) then
         print *,'error writing vgrd attribute'
         call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'vgrd',vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error writing vgrd'
        call stop2(29)
     endif
     if (pst_ind > 0) then
        do k=1,nlevs
           ug = ugtmp(:,k)*dpanl(:,k)
           vg = vgtmp(:,k)*dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
        enddo
     end if

     ! read sensible temp and specific humidity
     call read_vardata(dsfg,'tmp',ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading tmp'
        call stop2(29)
     endif
     call read_vardata(dsfg,'spfh',vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading spfh'
        call stop2(29)
     endif
     allocate(tv_bg(nlons,nlats,nlevs),tv_anal(nlons,nlats,nlevs))
     tv_bg = ug3d * ( 1.0 + fv*vg3d ) !Convert T to Tv
     call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading pressfc'
        call stop2(29)
     endif
     if (allocated(values_1d)) deallocate(values_1d)
     allocate(values_1d(nlons*nlats))
     values_1d = reshape(values_2d,(/nlons*nlats/))
     ! add Tv,q increment to background
     do k=1,nlevs
        ug = 0_r_kind
        if (tv_ind > 0) then
          call copyfromgrdin(grdin(:,levels(tv_ind-1)+k,nb,ne),ug)
        endif
        vg = 0_r_kind
        if (q_ind > 0) then
          call copyfromgrdin(grdin(:,levels(q_ind-1)+k,nb,ne),vg)
        endif
        values_2d = reshape(ug,(/nlons,nlats/))
        tv_anal(:,:,nlevs-k+1) = tv_bg(:,:,nlevs-k+1) + values_2d
        values_2d = reshape(vg,(/nlons,nlats/))
        vg3d(:,:,nlevs-k+1) = vg3d(:,:,nlevs-k+1) + values_2d
     enddo
     ! now tv_anal is analysis Tv, vg3d is analysis spfh
     if (cliptracers)  where (vg3d < clip) vg3d = clip

     ! write analysis T
     allocate(values_3d(nlons,nlats,nlevs))
     allocate(tmp_anal(nlons,nlats,nlevs))
     tmp_anal = tv_anal/(1. + fv*vg3d) ! convert Tv back to T, save q as vg3d
     values_3d = tmp_anal
     if (has_attr(dsfg, 'nbits', 'tmp') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'tmp')
       call quantize_data(tmp_anal, values_3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'tmp',errcode=iret)
       if (iret /= 0) then
         print *,'error writing tmp attribute'
         call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'tmp',values_3d,errcode=iret) ! write T
     if (iret /= 0) then
        print *,'error writing tmp'
        call stop2(29)
     endif

     ! write analysis delz
     if (has_var(dsfg,'delz')) then
        allocate(delzb(nlons*nlats))
        call read_vardata(dsfg,'delz',values_3d)
        vg = 0_r_kind
        if (ps_ind > 0) then
           call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),vg)
        endif
        vg = values_1d + vg*100_r_kind ! analysis ps (values_1d is background ps)
        do k=1,nlevs
           krev = nlevs-k+1  ! k=1 is model top
           ug=(rd/grav)*reshape(tv_anal(:,:,k),(/nlons*nlats/))
           ! ps in Pa here, need to multiply ak by 100.
           ! ug is analysis delz, calculate so it is negative
           ! (note that ak,bk are already reversed to go from bottom to top)
           ug=ug*log((100_r_kind*ak(krev+1)+bk(krev+1)*vg)/(100_r_kind*ak(krev)+bk(krev)*vg))
           ! ug is hydrostatic analysis delz inferred from analysis ps,Tv
           ! delzb is hydrostatic background delz inferred from background ps,Tv
           ! calculate so it is negative
           delzb=(rd/grav)*reshape(tv_bg(:,:,k),(/nlons*nlats/))
           delzb=delzb*log((100_r_kind*ak(krev+1)+bk(krev+1)*values_1d)/(100_r_kind*ak(krev)+bk(krev)*values_1d))
           ug3d(:,:,k)=values_3d(:,:,k) +&
           reshape(ug-delzb,(/nlons,nlats/))
        enddo
        !  minval(ug3d),maxval(ug3d)
        if (has_attr(dsfg, 'nbits', 'delz') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'delz')
          values_3d = ug3d
          call quantize_data(values_3d, ug3d, nbits, compress_err)
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'delz',errcode=iret)
          if (iret /= 0) then
            print *,'error writing delz attribute'
            call stop2(29)
          endif
        endif
        call write_vardata(dsanl,'delz',ug3d,errcode=iret) ! write delz
        if (iret /= 0) then
           print *,'error writing delz'
           call stop2(29)
        endif
     endif
     deallocate(tv_anal,tv_bg) ! keep tmp_anal

     ! write analysis q (still stored in vg3d)
     if (has_attr(dsfg, 'nbits', 'spfh') .and. .not. nocompress) then
       call read_attribute(dsfg, 'nbits', nbits, 'spfh')
       values_3d = vg3d
       call quantize_data(values_3d, vg3d, nbits, compress_err)
       call write_attribute(dsanl,&
       'max_abs_compression_error',compress_err,'spfh',errcode=iret)
       if (iret /= 0) then
         print *,'error writing spfh attribute'
         call stop2(29)
       endif
     endif
     call write_vardata(dsanl,'spfh',vg3d,errcode=iret) ! write q
     if (iret /= 0) then
        print *,'error writing spfh'
        call stop2(29)
     endif

     ! write clwmr, icmr
     call read_vardata(dsfg,'clwmr',ug3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading clwmr'
        call stop2(29)
     endif
     if (imp_physics == 11) then
        call read_vardata(dsfg,'icmr',vg3d,errcode=iret)
        if (iret /= 0) then
           print *,'error reading icmr'
           call stop2(29)
        endif
     endif
     do k=1,nlevs
        ug = 0_r_kind
        if (cw_ind > 0) then
           call copyfromgrdin(grdin(:,levels(cw_ind-1)+k,nb,ne),ug)
        endif
        if (imp_physics == 11) then
           work = -r0_05 * (reshape(tmp_anal(:,:,nlevs-k+1),(/nlons*nlats/)) - t0c)
           do i=1,nlons*nlats
              work(i) = max(zero,work(i))
              work(i) = min(one,work(i))
           enddo
           vg = ug * work          ! cloud ice
           ug = ug * (one - work)  ! cloud water
           vg3d(:,:,nlevs-k+1) = vg3d(:,:,nlevs-k+1) +&
           reshape(vg,(/nlons,nlats/))
        endif
        ug3d(:,:,nlevs-k+1) = ug3d(:,:,nlevs-k+1) + &
        reshape(ug,(/nlons,nlats/))
     enddo
     deallocate(tmp_anal)
     if (cw_ind > 0) then
        if (has_attr(dsfg, 'nbits', 'clwmr') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'clwmr')
          values_3d = ug3d
          call quantize_data(values_3d, ug3d, nbits, compress_err)
          if (cliptracers)  where (ug3d < clip) ug3d = clip
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'clwmr',errcode=iret)
          if (iret /= 0) then
            print *,'error writing clwmr attribute'
            call stop2(29)
          endif
        endif
        if (cliptracers)  where (ug3d < clip) ug3d = clip
     endif
     call write_vardata(dsanl,'clwmr',ug3d,errcode=iret) ! write clwmr
     if (iret /= 0) then
        print *,'error writing clwmr'
        call stop2(29)
     endif
     if (imp_physics == 11) then
        if (cw_ind > 0) then
           if (has_attr(dsfg, 'nbits', 'clwmr') .and. .not. nocompress) then
             call read_attribute(dsfg, 'nbits', nbits, 'clwmr')
             values_3d = vg3d
             call quantize_data(values_3d, vg3d, nbits, compress_err)
             if (cliptracers)  where (vg3d < clip) vg3d = clip
             call write_attribute(dsanl,&
             'max_abs_compression_error',compress_err,'icmr',errcode=iret)
             if (iret /= 0) then
               print *,'error writing icmr attribute'
               call stop2(29)
             endif
           endif
           if (cliptracers)  where (vg3d < clip) vg3d = clip
        endif
        call write_vardata(dsanl,'icmr',vg3d,errcode=iret) ! write icmr
        if (iret /= 0) then
           print *,'error writing icmr'
           call stop2(29)
        endif
     endif

     ! write analysis ozone
     call read_vardata(dsfg, 'o3mr', vg3d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading o3mr'
        call stop2(29)
     endif
     do k=1,nlevs
        ug = 0_r_kind
        if (oz_ind > 0) then
           call copyfromgrdin(grdin(:,levels(oz_ind-1)+k,nb,ne),ug)
        endif
        vg3d(:,:,nlevs-k+1) = vg3d(:,:,nlevs-k+1) + &
        reshape(ug,(/nlons,nlats/))
     enddo
     if (oz_ind > 0) then
        if (has_attr(dsfg, 'nbits', 'o3mr') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'o3mr')
          values_3d = vg3d
          call quantize_data(values_3d, vg3d, nbits, compress_err)
          if (cliptracers)  where (vg3d < clip) vg3d = clip
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'o3mr',errcode=iret)
          if (iret /= 0) then
            print *,'error writing o3mr attribute'
            call stop2(29)
          endif
        endif
        if (cliptracers)  where (vg3d < clip) vg3d = clip
     endif
     call write_vardata(dsanl,'o3mr',vg3d) ! write o3mr
     if (iret /= 0) then
        print *,'error writing o3mr'
        call stop2(29)
     endif
  endif

  if (allocated(delzb)) deallocate(delzb)
  if (allocated(recname)) deallocate(recname)
  if (imp_physics == 11 .and. (.not. use_full_hydro)) deallocate(work)

  if (pst_ind > 0) then

     vmassdivinc = vmassdiv - vmassdivinc ! analyis - first guess VIMFD
     ! (VIMFD = vertically integrated mass flux divergence)
     pstend1 = sum(vmassdiv,2)
     if (nanal .eq. 1) then
     print *,nanal,'min/max analysis ps tend',minval(pstend1),maxval(pstend1)
     print *,nanal,'min/max analyzed ps tend',minval(pstend2),maxval(pstend2)
     endif
     ! vmass is vertical integral of dp**2
     vmass = 0_r_kind
     do k=1,nlevs
        ! case 2 (4.3.1.2) in GEOS DAS document.
        ! (adjustment proportional to mass in layer)
        vmass = vmass + dpanl(:,k)**2
        ! case 3 (4.3.1.3) in GEOS DAS document.
        ! (adjustment propotional to mass-flux div increment)
        !vmass = vmass + vmassdivinc(:,k)**2
     enddo
     ! adjust wind field in analysis so pstend is consistent with pstend2
     ! (analyzed pstend)
!$omp parallel do private(k,nt,ug,vg,uginc,vginc,vrtspec,divspec)  shared(sigdata,vmassdiv,vmassdivinc,dpanl)
     do k=1,nlevs
        ! case 2
        ug = (pstend2 - pstend1)*dpanl(:,k)**2/vmass
        ! case 3
        !ug = (pstend2 - pstend1)*vmassdivinc(:,k)**2/vmass
        call sptez_s(divspec,ug,-1) ! divgrd to divspec
        vrtspec = 0_r_kind
        call sptezv_s(divspec,vrtspec,uginc,vginc,1) ! div,vrt to u,v
        if (nanal .eq. 1) then
          print *,k,'min/max u inc (member 1)',&
          minval(uginc/dpanl(:,k)),maxval(uginc/dpanl(:,k))
        endif
        if (use_gfs_nemsio .or. use_gfs_ncio) then
           ugtmp(:,k) = (ugtmp(:,k)*dpanl(:,k) + uginc)/dpanl(:,k)
           vgtmp(:,k) = (vgtmp(:,k)*dpanl(:,k) + vginc)/dpanl(:,k)
           ug = ugtmp(:,k); vg = vgtmp(:,k)
        else
           ! adjust spectral div,vort
           ! (vrtspec,divspec to u,v, add increment to u,v, then convert
           ! back go vrtspec,divspec)
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
           ug = (ug*dpanl(:,k) + uginc)/dpanl(:,k)
           vg = (vg*dpanl(:,k) + vginc)/dpanl(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
           sigdata%d(:,k) = divspec; sigdata%z(:,k) = vrtspec
           ! recompute u,v
           divspec = sigdata%d(:,k); vrtspec = sigdata%z(:,k)
           call sptezv_s(divspec,vrtspec,ug,vg,1)
        endif
! check result..
        ug = ug*dpanl(:,k); vg = vg*dpanl(:,k)
        call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
        call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
     enddo
!$omp end parallel do

     ! should be same as analyzed ps tend
     psfg = sum(vmassdiv,2)
     !if (nanal .eq. 1) then
     !   open(919,file='pstend.dat',form='unformatted',access='direct',recl=nlons*nlats)
     !   write(919,rec=1) pstendfg
     !   write(919,rec=2) pstend2
     !   write(919,rec=3) psfg
     !   write(919,rec=4) pstend1
     !   close(919)
     !endif
     if (nanal .eq. 1) then
     print *,nanal,'min/max adjusted ps tend',minval(psfg),maxval(psfg)
     print *,nanal,'min/max diff between adjusted and analyzed ps tend',&
             minval(pstend2-psfg),maxval(pstend2-psfg)
     endif

  endif ! if pst_ind > 0

  if (.not. use_gfs_nemsio .and. .not. use_gfs_ncio) then
  ! clip tracers.
     if (cliptracers) then
        clip = tiny_r_kind
!$omp parallel do private(k,nt,vg,divspec)  shared(sigdata,clip)
        do k=1,nlevs
           if (q_ind > 0) then
             divspec = sigdata%q(:,k,1)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,1) = divspec
          endif
          if (oz_ind > 0) then
             divspec = sigdata%q(:,k,2)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,2) = divspec
          endif
          if (cw_ind > 0) then
             divspec = sigdata%q(:,k,3)
             call sptez_s(divspec,vg,1)
             where (vg < clip) vg = clip
             call sptez_s(divspec,vg,-1)
             sigdata%q(:,k,3) = divspec
          endif
        enddo
!$omp end parallel do
     end if

     ! write out analysis.
     call sigio_swohdc(iunitsig,filenameout,sighead,sigdata,ierr)
     ! deallocate sigdata structure.
     call sigio_axdata(sigdata,ierr)
  else if (use_gfs_ncio) then
     if (pst_ind > 0) then
        do k=1,nlevs
           ug3d(:,:,nlevs-k+1) = reshape(ugtmp(:,k),(/nlons,nlats/))
           vg3d(:,:,nlevs-k+1) = reshape(vgtmp(:,k),(/nlons,nlats/))
        enddo
        if (has_attr(dsfg, 'nbits', 'ugrd') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'ugrd')
          values_3d = ug3d
          call quantize_data(values_3d, ug3d, nbits, compress_err)
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'ugrd',errcode=iret)
          if (iret /= 0) then
            print *,'error writing ugrd attribute'
            call stop2(29)
          endif
        endif
        call write_vardata(dsanl,'ugrd',ug3d,errcode=iret) ! write u
        if (iret /= 0) then
           print *,'error writing ugrd'
           call stop2(29)
        endif
        if (has_attr(dsfg, 'nbits', 'vgrd') .and. .not. nocompress) then
          call read_attribute(dsfg, 'nbits', nbits, 'vgrd')
          values_3d = vg3d
          call quantize_data(values_3d, vg3d, nbits, compress_err)
          call write_attribute(dsanl,&
          'max_abs_compression_error',compress_err,'vgrd',errcode=iret)
          if (iret /= 0) then
            print *,'error writing vgrd attribute'
            call stop2(29)
          endif
        endif
        call write_vardata(dsanl,'vgrd',vg3d,errcode=iret) ! write v
        if (iret /= 0) then
           print *,'error writing ugrd'
           call stop2(29)
        endif
        deallocate(ugtmp,vgtmp)
     endif
  else if (use_gfs_nemsio) then
     if (pst_ind > 0) then
        ! update u,v
        do k=1,nlevs
           nems_wrk = ugtmp(:,k)
           call nemsio_writerecv(gfileout,'ugrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(ugrd), iret=',iret
              call stop2(23)
           endif
           nems_wrk = vgtmp(:,k)
           call nemsio_writerecv(gfileout,'vgrd','mid layer',k,nems_wrk,iret=iret)
           if (iret/=0) then
              write(6,*)'gridio/writegriddata: gfs model: problem with nemsio_writerecv(vgrd), iret=',iret
              call stop2(23)
           endif
        enddo
        deallocate(ugtmp,vgtmp)
     endif
  endif

  if (allocated(ug3d)) deallocate(ug3d)
  if (allocated(vg3d)) deallocate(vg3d)
  if (allocated(values_3d)) deallocate(values_3d)
  if (allocated(values_2d)) deallocate(values_2d)
  if (allocated(values_1d)) deallocate(values_1d)

  if (pst_ind > 0) then
     deallocate(pressi,dpanl,dpfg)
     deallocate(pstend1,pstend2,pstendfg,vmass)
     deallocate(vmassdiv)
     deallocate(vmassdivinc)
  endif

  if (use_gfs_nemsio) then
      call nemsio_close(gfilein,iret=iret)
      if (iret/=0) then
         write(6,*)'gridio/writegriddata: gfs model: problem closing nemsio fg dataset, iret=',iret
         call stop2(23)
      endif
      call nemsio_close(gfileout,iret=iret)
      if (iret/=0) then
         write(6,*)'gridio/writegriddata: gfs model: problem closing nemsio anal dataset, iret=',iret
         call stop2(23)
      endif
  else if (use_gfs_ncio) then
      call close_dataset(dsfg,errcode=iret)
      if (iret/=0) then
         write(6,*)'gridio/writegriddata: gfs model: problem closing netcdf fg dataset, iret=',iret
         call stop2(23)
      endif
      call close_dataset(dsanl,errcode=iret)
      if (iret/=0) then
         write(6,*)'gridio/writegriddata: gfs model: problem closing netcdf anal dataset, iret=',iret
         call stop2(23)
      endif
  endif

  end do backgroundloop ! loop over backgrounds to write out
  end do ensmemloop ! loop over ens members to write out

 contains
! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copyfromgrdin(grdin, field)
 implicit none

 real(r_single), dimension(:), intent(in)      :: grdin
 real(r_kind), dimension(:), intent(inout) :: field

 if (reducedgrid) then
   call reducedtoreg(grdin, field)
 else
   field = grdin
 endif

 end subroutine copyfromgrdin

 end subroutine writegriddata

 subroutine writeincrement(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  use netcdf
  use params, only: nbackgrounds,incfileprefixes,fgfileprefixes,reducedgrid,&
                    datestring,nhr_anal,write_ensmean, fgsfcfileprefixes,incsfcfileprefixes
  use constants, only: grav
  use mpi
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         read_attribute, close_dataset, get_dim, read_vardata,&
                         create_dataset, get_idate_from_time_units, &
                         get_time_units_from_idate, write_vardata, &
                         write_attribute, quantize_data, has_var, has_attr
  implicit none

  integer, intent(in) :: nanal1,nanal2
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d,n3d,ndim
  integer, dimension(0:n3d), intent(in) :: levels
  real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
  logical, intent(in) :: no_inflate_flag
  logical:: use_full_hydro
  character(len=500):: filenamein, filenameout
  integer(i_kind) :: i,j,k, nb, ne, nanal
  character(len=3) charnanal
  type(Dataset) :: dsfg

  integer(i_kind) :: krev, iret
  real(r_kind), dimension(nlevs+1) :: ak,bk
  real(r_kind) clip

  integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind
  integer :: ql_ind, qi_ind, qr_ind, qs_ind, qg_ind

  ! netcdf things
  integer(i_kind) :: dimids3(3), ncstart(3), nccount(3), dimids2(2) 
  integer(i_kind) :: ncid_out, lon_dimid, lat_dimid, lev_dimid, ilev_dimid
  integer(i_kind) :: lonvarid, latvarid, levvarid, pfullvarid, ilevvarid, &
                     hyaivarid, hybivarid, uvarid, vvarid, delpvarid, delzvarid, &
                     tvarid, sphumvarid, liqwatvarid, o3varid, icvarid, &
                     tmp2mvarid, spfh2mvarid, soilt1varid, soilt2varid, &
                     soilt3varid, soilt4varid, slc1varid, slc2varid, &
                     slc3varid, slc4varid, maskvarid
  integer(i_kind) :: tmp2m_ind, spfh2m_ind, soilt1_ind, soilt2_ind, soilt3_ind, &
                     soilt4_ind,slc1_ind, slc2_ind, slc3_ind, slc4_ind
  integer(i_kind) :: iadateout

  ! fixed fields such as lat, lon, levs
  real(r_kind),dimension(nlons) :: deglons
  real(r_kind),dimension(nlats) :: deglats
  real(r_kind),dimension(nlevs) :: levsout
  real(r_kind),dimension(nlevs+1) :: ilevsout

  ! increment
  real(r_kind), dimension(nlons*nlats) :: psinc, inc, ug, vg, work
  real(r_single), allocatable, dimension(:,:,:) :: inc3d, inc3d2, inc3dout
  real(r_single), allocatable, dimension(:,:) :: inc2d,  inc2dout
  real(r_single), allocatable, dimension(:,:,:) :: tv, tvanl, tmp, tmpanl, q, qanl
  real(r_kind), allocatable, dimension(:,:) :: values_2d
  real(r_kind), allocatable, dimension(:) :: psges, delzb, values_1d
  ! soil / snow mask (not fixed)
  integer(i_kind), dimension(nlons,nlats) :: mask
  logical :: write_sfc_file, write_atm_file
  real(r_double)  :: t1,t2

  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, write_sfc_file, write_atm_file)

  use_full_hydro = .false.
  clip = tiny_r_kind
  read(datestring,*) iadateout

  ncstart = (/1, 1, 1/)
  nccount = (/nlons, nlats, nlevs/)

  if ( write_atm_file) then
  if (nproc == 0) t1 = mpi_wtime()
  ne = 0
  ensmemloop: do nanal=nanal1,nanal2
  ne = ne + 1
  write(charnanal,'(i3.3)') nanal
  backgroundloop: do nb=1,nbackgrounds

  if (nanal == 0 .and. write_ensmean) then
     filenameout = trim(adjustl(datapath))//trim(adjustl(incfileprefixes(nb)))//"ensmean"
     filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"ensmean"
  else
     if(no_inflate_flag) then
       filenameout = trim(adjustl(datapath))//trim(adjustl(incfileprefixes(nb)))//"nimem"//charnanal
     else
       filenameout = trim(adjustl(datapath))//trim(adjustl(incfileprefixes(nb)))//"mem"//charnanal
     end if
     filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal
  endif

  ! create the output netCDF increment file
  call nccheck_incr(nf90_create(path=trim(filenameout), cmode=nf90_netcdf4, ncid=ncid_out))

  ! create dimensions based on analysis resolution, not guess
  call nccheck_incr(nf90_def_dim(ncid_out, "lon", nlons, lon_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "lat", nlats, lat_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "lev", nlevs, lev_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "ilev", nlevs+1, ilev_dimid))
  dimids3 = (/ lon_dimid, lat_dimid, lev_dimid /)
  ! create variables
  call nccheck_incr(nf90_def_var(ncid_out, "lon", nf90_real, (/lon_dimid/), lonvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "lat", nf90_real, (/lat_dimid/), latvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "lev", nf90_real, (/lev_dimid/), levvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "pfull", nf90_real, (/lev_dimid/), pfullvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "ilev", nf90_real, (/ilev_dimid/), ilevvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "hyai", nf90_real, (/ilev_dimid/), hyaivarid))
  call nccheck_incr(nf90_def_var(ncid_out, "hybi", nf90_real, (/ilev_dimid/), hybivarid))
  call nccheck_incr(nf90_def_var(ncid_out, "u_inc", nf90_real, dimids3, uvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "v_inc", nf90_real, dimids3, vvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "delp_inc", nf90_real, dimids3, delpvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "delz_inc", nf90_real, dimids3, delzvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "T_inc", nf90_real, dimids3, tvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "sphum_inc", nf90_real, dimids3, sphumvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "liq_wat_inc", nf90_real, dimids3, liqwatvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "o3mr_inc", nf90_real, dimids3, o3varid))
  call nccheck_incr(nf90_def_var(ncid_out, "icmr_inc", nf90_real, dimids3, icvarid))
  ! place global attributes to serial calc_increment output
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI EnKF"))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                    "global analysis increment from writeincrement"))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "analysis_time", iadateout))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "IAU_hour_from_guess", nhr_anal(nb)))
  ! add units to lat/lon because that's what the calc_increment utility has
  call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
  call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
  ! end the netCDF file definition
  call nccheck_incr(nf90_enddef(ncid_out))

  u_ind   = getindex(vars3d, 'u')   !< indices in the control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                    ! old logical massbal_adjust, if non-zero
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0 .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )

  dsfg = open_dataset(filenamein)
  call read_attribute(dsfg, 'ak', values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading ak'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     ak(nlevs-k+2) = 0.01_r_kind*values_1d(k)
  enddo
  call read_attribute(dsfg, 'bk', values_1d,errcode=iret)
  if (iret /= 0) then
     print *,'error reading bk'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     bk(nlevs-k+2) = values_1d(k)
  enddo

  ! levels
  do k=1,nlevs
    levsout(k) = float(k)
    ilevsout(k) = float(k)
  end do
  ilevsout(nlevs+1) = float(nlevs+1)

  ! longitudes
  call read_vardata(dsfg, 'grid_xt', values_1d, errcode=iret)
  deglons(:) = values_1d
  call nccheck_incr(nf90_put_var(ncid_out, lonvarid, deglons, &
                       start = (/1/), count = (/nlons/)))

  call read_vardata(dsfg, 'grid_yt', values_1d, errcode=iret)
  ! latitudes
  do j=1,nlats
    deglats(nlats-j+1) = values_1d(j)
  end do

  call nccheck_incr(nf90_put_var(ncid_out, latvarid, deglats, &
                       start = (/1/), count = (/nlats/)))

  ! write to file
  call nccheck_incr(nf90_put_var(ncid_out, levvarid, sngl(levsout), &
                    start = (/1/), count = (/nlevs/)))
  ! pfull
  call nccheck_incr(nf90_put_var(ncid_out, pfullvarid, sngl(levsout), &
                    start = (/1/), count = (/nlevs/)))
  ! ilev
  call nccheck_incr(nf90_put_var(ncid_out, ilevvarid, sngl(ilevsout), &
                    start = (/1/), count = (/nlevs+1/)))
  ! hyai
  call nccheck_incr(nf90_put_var(ncid_out, hyaivarid, sngl(ilevsout), &
                    start = (/1/), count = (/nlevs+1/)))
  ! hybi
  call nccheck_incr(nf90_put_var(ncid_out, hybivarid, sngl(ilevsout), &
                    start = (/1/), count = (/nlevs+1/)))

  allocate(inc3d(nlons,nlats,nccount(3)))
  allocate(inc3d2(nlons,nlats,nccount(3)))
  allocate(inc3dout(nlons,nlats,nccount(3)))
  ! u increment
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     if (u_ind > 0) then
       call copyfromgrdin(grdin(:,levels(u_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('u_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, uvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))
  ! v increment
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     if (u_ind > 0) then
       call copyfromgrdin(grdin(:,levels(v_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('v_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, vvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! delp increment
  psinc(:) = zero
  if (ps_ind > 0) then
    call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),psinc)
  endif
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     inc = psinc*(bk(krev)-bk(krev+1))*100_r_kind
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('delp_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, delpvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! sphum increment
  allocate(tmp(nlons,nlats,nccount(3)),tv(nlons,nlats,nccount(3)),q(nlons,nlats,nccount(3)))
  allocate(tvanl(nlons,nlats,nccount(3)),tmpanl(nlons,nlats,nccount(3)),qanl(nlons,nlats,nccount(3)))
  call read_vardata(dsfg, 'spfh', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading spfh'
     call stop2(29)
  endif
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     if (q_ind > 0) then
       call copyfromgrdin(grdin(:,levels(q_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
     qanl(:,:,k) = q(:,:,k) + inc3d(:,:,k)
  end do
  if (cliptracers)  where (qanl < clip) qanl = clip
  inc3d = qanl - q
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('sphum_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, sphumvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! t increment
  call read_vardata(dsfg, 'tmp', tmp, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     print *,'error reading tmp'
     call stop2(29)
  endif
  tv = tmp * ( 1.0 + fv*q)
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     if (tv_ind > 0) then
       call copyfromgrdin(grdin(:,levels(tv_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
     tvanl(:,:,k) = tv(:,:,k) + inc3d(:,:,k)
     tmpanl(:,:,k) = tvanl(:,:,k)/(1. + fv*qanl(:,:,k))
  end do
  inc3d = tmpanl - tmp
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('T_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, tvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! delz increment
  inc3d(:,:,:) = zero
  if (has_var(dsfg,'delz')) then
     allocate(delzb(nlons*nlats))
     call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
     if (allocated(psges)) deallocate(psges)
     allocate(psges(nlons*nlats))
     psges = reshape(values_2d,(/nlons*nlats/))
     vg = psges + (psinc*100_r_kind)
     do k=1,nlevs
        krev = nlevs-k+1 ! k=1 is model top
        ug=(rd/grav)*reshape(tvanl(:,:,k),(/nlons*nlats/))
        ! ps in Pa here, need to multiply ak by 100.
        ! calculate ug (analysis delz) so it is negative. 
        ! (note that ak,bk are already reversed to go from bottom to top)
        ug=ug*log((100_r_kind*ak(krev+1)+bk(krev+1)*vg)/(100_r_kind*ak(krev)+bk(krev)*vg))
        ! ug is hydrostatic analysis delz inferred from analysis ps,Tv
        ! delzb is hydrostatic background delz inferred from background ps,Tv
        delzb=(rd/grav)*reshape(tv(:,:,k),(/nlons*nlats/))
        delzb=delzb*log((100_r_kind*ak(krev+1)+bk(krev+1)*psges)/(100_r_kind*ak(krev)+bk(krev)*psges))
        inc3d(:,:,k)=reshape(ug-delzb,(/nlons,nlats/))
     end do
  end if
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('delz_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, delzvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! o3mr increment
  do k=1,nlevs
     krev = nlevs-k+1
     inc(:) = zero
     if (oz_ind > 0) then
       call copyfromgrdin(grdin(:,levels(oz_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,k) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('o3mr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, o3varid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! liq wat increment
  ! icmr increment
  do k=1,nlevs
     krev = nlevs-k+1
     ug = zero
     if (cw_ind > 0) then
        call copyfromgrdin(grdin(:,levels(cw_ind-1)+krev,nb,ne),ug)
     end if
     if (imp_physics == 11) then
        work = -r0_05 * (reshape(tmpanl(:,:,k),(/nlons*nlats/)) - t0c)
        do i=1,nlons*nlats
           work(i) = max(zero,work(i))
           work(i) = min(one,work(i))
        enddo
        vg = ug * work          ! cloud ice
        ug = ug * (one - work)  ! cloud water
        inc3d2(:,:,k) = reshape(vg,(/nlons,nlats/))
     endif
     inc3d(:,:,k) = reshape(ug,(/nlons,nlats/))
  enddo
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('liq_wat_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, liqwatvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d2(:,j,:)
  end do
  if (should_zero_increments_for('icmr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, icvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  call close_dataset(dsfg,errcode=iret)
  if (iret/=0) then
    write(6,*)'gridio/writeincrement gfs model: problem closing netcdf fg dataset, iret=',iret
    call stop2(23)
  endif
  ! deallocate things
  deallocate(inc3d,inc3d2,inc3dout)
  deallocate(tmp,tv,q,tmpanl,tvanl,qanl)
  if (allocated(delzb))  deallocate(delzb)
  if (allocated(psges)) deallocate(psges)

  end do backgroundloop ! loop over backgrounds to read in
  end do ensmemloop ! loop over ens members to read in

  if (nproc == 0) then
       t2 = mpi_wtime()
       print *,'time in writeincrement atm_file on root',t2-t1,'secs'
  endif
  endif ! write_atm_file

  if (write_sfc_file) then

     if (nproc == 0) t1 = mpi_wtime() 
     ne = 0
     sfcensmemloop: do nanal=nanal1,nanal2
     ne = ne + 1
     write(charnanal,'(i3.3)') nanal
     sfcbackgroundloop: do nb=1,nbackgrounds

     if (nanal == 0 ) then
        filenamein = trim(adjustl(datapath))//trim(adjustl(fgsfcfileprefixes(nb)))//"ensmean"
        filenameout = trim(adjustl(datapath))//trim(adjustl(incsfcfileprefixes(nb)))//"ensmean"
     else
        if(no_inflate_flag) then
          filenameout = trim(adjustl(datapath))//trim(adjustl(incsfcfileprefixes(nb)))//"nimem"//charnanal
        else
          filenameout = trim(adjustl(datapath))//trim(adjustl(incsfcfileprefixes(nb)))//"mem"//charnanal
        end if
        filenamein = trim(adjustl(datapath))//trim(adjustl(fgsfcfileprefixes(nb)))//"mem"//charnanal
     endif

     ! create the output netCDF increment file
     call nccheck_incr(nf90_create(path=trim(filenameout), cmode=nf90_netcdf4, ncid=ncid_out))

     ! create dimensions based on analysis resolution, not guess
     call nccheck_incr(nf90_def_dim(ncid_out, "longitude", nlons, lon_dimid))
     call nccheck_incr(nf90_def_dim(ncid_out, "latitude", nlats, lat_dimid))
     dimids2 = (/ lon_dimid, lat_dimid /)
     ! create variables
     call nccheck_incr(nf90_def_var(ncid_out, "longitude", nf90_real, (/lon_dimid/), lonvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "latitude", nf90_real, (/lat_dimid/), latvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "tmp2m_inc", nf90_real, dimids2, tmp2mvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "spfh2m_inc", nf90_real, dimids2, spfh2mvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt1_inc", nf90_real, dimids2, soilt1varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt2_inc", nf90_real, dimids2, soilt2varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt3_inc", nf90_real, dimids2, soilt3varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt4_inc", nf90_real, dimids2, soilt4varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc1_inc", nf90_real, dimids2, slc1varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc2_inc", nf90_real, dimids2, slc2varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc3_inc", nf90_real, dimids2, slc3varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc4_inc", nf90_real, dimids2, slc4varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilsnow_mask", nf90_int, dimids2, maskvarid))
     ! place global attributes to serial calc_increment output
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI EnKF"))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                       "global landsfc anal increment from writeincrement"))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "analysis_time", iadateout))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "IAU_hour_from_guess", nhr_anal(nb)))
     ! add units to lat/lon because that's what the calc_increment utility has
     call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
     call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
     ! end the netCDF file definition
     call nccheck_incr(nf90_enddef(ncid_out))

     tmp2m_ind  = getindex(vars2d, 't2m')   !< indices in the state or control var arrays
     spfh2m_ind = getindex(vars2d, 'q2m')
     soilt1_ind = getindex(vars2d, 'st1')
     slc1_ind = getindex(vars2d, 'sl1')
     soilt2_ind = getindex(vars2d, 'st2')
     slc2_ind = getindex(vars2d, 'sl2')
     soilt3_ind = getindex(vars2d, 'st3')
     slc3_ind = getindex(vars2d, 'sl3')
     soilt4_ind = getindex(vars2d, 'st4')
     slc4_ind = getindex(vars2d, 'sl4')

     dsfg = open_dataset(filenamein)

     ! longitudes
     call read_vardata(dsfg, 'grid_xt', values_1d, errcode=iret)
     deglons(:) = values_1d
     call nccheck_incr(nf90_put_var(ncid_out, lonvarid, deglons, &
                          start = (/1/), count = (/nlons/)))

     call read_vardata(dsfg, 'grid_yt', values_1d, errcode=iret)
     ! latitudes
     do j=1,nlats
       deglats(nlats-j+1) = values_1d(j)
     end do

     call nccheck_incr(nf90_put_var(ncid_out, latvarid, deglats, &
                          start = (/1/), count = (/nlats/)))

     ! construct mask (1 - soil, 2 - snow, 0 - not snow)
     ! note: same logic/threshold used in global_cycle to produce
     ! mask on model grid.

     call read_vardata(dsfg, 'soill1', values_2d, errcode=iret)

     mask = 0
     do j=1,nlats
       do i = 1, nlons
           if (values_2d(i,j) .LT. 1.0) then
           mask(i,nlats-j+1) = 1
           endif
       enddo
     end do

     call read_vardata(dsfg, 'weasd', values_2d, errcode=iret)
     do j=1,nlats
       do i = 1, nlons
           if (values_2d(i,j) .GT. 0.001) then
           mask(i,nlats-j+1) = 2
           endif
       enddo
     end do
     call nccheck_incr(nf90_put_var(ncid_out, maskvarid, mask, &
                       start = ncstart(1:2), count = nccount(1:2)))

     allocate(inc2d(nlons,nlats))
     allocate(inc2dout(nlons,nlats))

     ! tmp2m increment
     inc(:) = zero
     if (tmp2m_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d) + tmp2m_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, tmp2mvarid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! spfh2m increment
     inc(:) = zero
     if (spfh2m_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+spfh2m_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, spfh2mvarid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! soilt1 increment
     inc(:) = zero
     if (soilt1_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+soilt1_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     inc2dout=0.
     do j=1,nlats
       do i = 1, nlons
           if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
       enddo
     end do
     call nccheck_incr(nf90_put_var(ncid_out, soilt1varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! soilt2 increment
     inc(:) = zero
     if (soilt2_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+soilt2_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     inc2dout=0.
     do j=1,nlats
       do i = 1, nlons
           if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
       enddo
     end do
     call nccheck_incr(nf90_put_var(ncid_out, soilt2varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! soilt3 increment
     inc(:) = zero
     if (soilt3_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+soilt3_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     inc2dout=0.
     do j=1,nlats
       do i = 1, nlons
           if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
       enddo
     end do
     call nccheck_incr(nf90_put_var(ncid_out, soilt3varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! soilt4 increment
     inc(:) = zero
     if (soilt4_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+soilt4_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     inc2dout=0.
     do j=1,nlats
       do i = 1, nlons
           if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
       enddo
     end do
     call nccheck_incr(nf90_put_var(ncid_out, soilt4varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! slc1 increment
     inc(:) = zero
     if (slc1_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+slc1_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, slc1varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! slc2 increment
     inc(:) = zero
     if (slc2_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+slc2_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, slc2varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! slc3 increment
     inc(:) = zero
     if (slc3_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+slc3_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, slc3varid, sngl(inc2dout), &
                         start = ncstart(1:2), count = nccount(1:2)))
     ! slc4 increment
     inc(:) = zero
     if (slc4_ind > 0) then
       call copyfromgrdin(grdin(:,levels(n3d)+slc4_ind,nb,ne),inc)
     endif
     inc2d(:,:) = reshape(inc,(/nlons,nlats/))
     do j=1,nlats
       inc2dout(:,nlats-j+1) = inc2d(:,j)
     end do
     call nccheck_incr(nf90_put_var(ncid_out, slc4varid, sngl(inc2dout), &
                       start = ncstart(1:2), count = nccount(1:2)))

     call close_dataset(dsfg,errcode=iret)
     if (iret/=0) then
       write(6,*)'gridio/writeincrement gfs model: problem closing netcdf sfc fg dataset, iret=',iret
       call stop2(23)
     endif
     ! deallocate things
     deallocate(inc2d,inc2dout)

  end do sfcbackgroundloop ! loop over backgrounds to read in
  end do sfcensmemloop ! loop over ens members to read in
  if (nproc == 0) then
       t2 = mpi_wtime()
       print *,'time in writeincrement sfc_file on root',t2-t1,'secs'
  endif

  endif ! write_sfc_file

  return

 contains
! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copyfromgrdin(grdin, field)
 implicit none

 real(r_single), dimension(:), intent(in)      :: grdin
 real(r_kind), dimension(:), intent(inout) :: field

 if (reducedgrid) then
   call reducedtoreg(grdin, field)
 else
   field = grdin
 endif

 end subroutine copyfromgrdin

 end subroutine writeincrement

 subroutine writeincrement_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
  use netcdf
  use params, only: nbackgrounds,incfileprefixes,fgfileprefixes,reducedgrid,&
                    datestring,nhr_anal, &
                    incsfcfileprefixes,fgsfcfileprefixes
  use constants, only: grav,qcmin
  use mpi
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         read_attribute, close_dataset, get_dim, read_vardata,&
                         create_dataset, get_idate_from_time_units, &
                         get_time_units_from_idate, write_vardata, &
                         write_attribute, quantize_data, has_var, has_attr
  implicit none

  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d,n3d,ndim
  integer, dimension(0:n3d), intent(in) :: levels
  real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
  logical, intent(in) :: no_inflate_flag
  logical:: use_full_hydro
  character(len=500):: filenamein, filenameout
  integer(i_kind) :: i,j,k, nb, ne, nanal, imem
  character(len=3) charnanal
  type(Dataset) :: dsfg

  integer(i_kind), allocatable, dimension(:) :: mem_pe, lev_pe1, lev_pe2, iocomms
  integer(i_kind) :: iope, ionumproc, iolevs, krev, ki, iret
  real(r_kind), dimension(nlevs+1) :: ak,bk
  real(r_kind) clip

  integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer :: ps_ind, pst_ind
  integer :: ql_ind, qi_ind, qr_ind, qs_ind, qg_ind

  ! netcdf things
  integer(i_kind) :: dimids3(3),nccount(3),ncstart(3), dimids2(2)
  integer(i_kind) :: ncid_out, lon_dimid, lat_dimid, lev_dimid, ilev_dimid
  integer(i_kind) :: lonvarid, latvarid, levvarid, pfullvarid, ilevvarid, &
                     hyaivarid, hybivarid, uvarid, vvarid, delpvarid, delzvarid, &
                     tvarid, sphumvarid, liqwatvarid, o3varid, icvarid, &
                     rwmrvarid, snmrvarid, grlevarid, &
                     tmp2mvarid, spfh2mvarid, soilt1varid, soilt2varid, &
                     soilt3varid, soilt4varid, slc1varid, slc2varid, &
                     slc3varid, slc4varid, maskvarid
  integer(i_kind) :: tmp2m_ind, spfh2m_ind, soilt1_ind, soilt2_ind,soilt3_ind, &
                     soilt4_ind,slc1_ind, slc2_ind, slc3_ind, slc4_ind 
  integer(i_kind) :: iadateout

  ! fixed fields such as lat, lon, levs
  real(r_kind),dimension(nlons) :: deglons
  real(r_kind),dimension(nlats) :: deglats
  real(r_kind),dimension(nlevs) :: levsout
  real(r_kind),dimension(nlevs+1) :: ilevsout

  ! increment
  real(r_kind), dimension(nlons*nlats) :: psinc, inc, ug, vg, work
  real(r_single), allocatable, dimension(:,:,:) :: inc3d, inc3d2, inc3dout
  real(r_single), allocatable, dimension(:,:) :: inc2d,  inc2dout
  real(r_single), allocatable, dimension(:,:,:) :: tv, tvanl, tmp, tmpanl, q, qanl
  real(r_single), allocatable, dimension(:,:,:) :: q2, qanl2
  real(r_kind), allocatable, dimension(:,:) :: values_2d
  real(r_kind), allocatable, dimension(:) :: psges, delzb, values_1d

  ! soil / snow mask (not fixed)
  integer(i_kind), dimension(nlons,nlats) :: mask

  logical :: write_sfc_file, write_atm_file
  real(r_double)  :: t1,t2

  call set_ncio_file_flags(vars3d, n3d, vars2d, n2d, write_sfc_file, write_atm_file) 
  
  use_full_hydro = .false.
  clip = tiny_r_kind
  read(datestring,*) iadateout

  ! figure out what member to write and do MPI sub-communicator things
  allocate(mem_pe(0:numproc-1))
  allocate(iocomms(nanals))
  imem = 1
  do i=0,numproc-1
    mem_pe(i) = imem
    imem = imem + 1
    if (imem > nanals) imem = 1
  end do
  nanal = mem_pe(nproc)

  call mpi_comm_split(mpi_comm_world, mem_pe(nproc), nproc, iocomms(mem_pe(nproc)), iret)
  call mpi_comm_rank(iocomms(mem_pe(nproc)), iope, iret)
  call mpi_comm_size(iocomms(mem_pe(nproc)), ionumproc, iret)

  ! figure out what levels to write on this sub-communicator's PE
  allocate(lev_pe1(0:ionumproc-1))
  allocate(lev_pe2(0:ionumproc-1))
  iolevs = nlevs/ionumproc
  do i=0,ionumproc-1
     lev_pe1(i) = (iope * iolevs) + 1
     lev_pe2(i) = ((iope + 1) * iolevs)
     if (i == ionumproc-1) lev_pe2(i) = lev_pe2(i) + modulo(nlevs, ionumproc)
  end do
  ncstart = (/1, 1, lev_pe1(iope)/)
  nccount = (/nlons, nlats, lev_pe2(iope) - lev_pe1(iope)+1/)

  ! need to distribute grdin to all PEs in this subcommunicator
  ! bring all the subdomains back to the main PE
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  do nb=1,nbackgrounds
     call mpi_bcast(grdin(1,1,nb,1),npts*ndim, mpi_real4, 0, iocomms(mem_pe(nproc)), iret)
  enddo

  if (write_atm_file ) then   
   
  if (nproc == 0) t1 = mpi_wtime()
  ! loop through times and do the read
  ne = 1
  backgroundloop: do nb=1,nbackgrounds
  write(charnanal,'(i3.3)') nanal

  if(no_inflate_flag) then
    filenameout = trim(adjustl(datapath))//trim(adjustl(incfileprefixes(nb)))//"nimem"//charnanal
  else
    filenameout = trim(adjustl(datapath))//trim(adjustl(incfileprefixes(nb)))//"mem"//charnanal
  end if
  filenamein = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal

  ! create the output netCDF increment file
  call nccheck_incr(nf90_create(path=trim(filenameout), cmode=ior(nf90_netcdf4, nf90_mpiio), ncid=ncid_out, &
                    comm = iocomms(mem_pe(nproc)), info = mpi_info_null))

  ! create dimensions based on analysis resolution, not guess
  call nccheck_incr(nf90_def_dim(ncid_out, "lon", nlons, lon_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "lat", nlats, lat_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "lev", nlevs, lev_dimid))
  call nccheck_incr(nf90_def_dim(ncid_out, "ilev", nlevs+1, ilev_dimid))
  dimids3 = (/ lon_dimid, lat_dimid, lev_dimid /)
  ! create variables
  call nccheck_incr(nf90_def_var(ncid_out, "lon", nf90_real, (/lon_dimid/), lonvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "lat", nf90_real, (/lat_dimid/), latvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "lev", nf90_real, (/lev_dimid/), levvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "pfull", nf90_real, (/lev_dimid/), pfullvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "ilev", nf90_real, (/ilev_dimid/), ilevvarid))
  call nccheck_incr(nf90_def_var(ncid_out, "hyai", nf90_real, (/ilev_dimid/), hyaivarid))
  call nccheck_incr(nf90_def_var(ncid_out, "hybi", nf90_real, (/ilev_dimid/), hybivarid))
  call nccheck_incr(nf90_def_var(ncid_out, "u_inc", nf90_real, dimids3, uvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, uvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "v_inc", nf90_real, dimids3, vvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, vvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "delp_inc", nf90_real, dimids3, delpvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, delpvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "delz_inc", nf90_real, dimids3, delzvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, delzvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "T_inc", nf90_real, dimids3, tvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, tvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "sphum_inc", nf90_real, dimids3, sphumvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, sphumvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "liq_wat_inc", nf90_real, dimids3, liqwatvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, liqwatvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "o3mr_inc", nf90_real, dimids3, o3varid))
  call nccheck_incr(nf90_var_par_access(ncid_out, o3varid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "icmr_inc", nf90_real, dimids3, icvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, icvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "rwmr_inc", nf90_real, dimids3, rwmrvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, rwmrvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "snmr_inc", nf90_real, dimids3, snmrvarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, snmrvarid, nf90_collective))
  call nccheck_incr(nf90_def_var(ncid_out, "grle_inc", nf90_real, dimids3, grlevarid))
  call nccheck_incr(nf90_var_par_access(ncid_out, grlevarid, nf90_collective))
  ! place global attributes to parallel calc_increment output
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI EnKF"))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                    "global analysis increment from writeincrement_pnc"))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "analysis_time", iadateout))
  call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "IAU_hour_from_guess", nhr_anal(nb)))
  ! add units to lat/lon because that's what the calc_increment utility has
  call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
  call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
  ! end the netCDF file definition
  call nccheck_incr(nf90_enddef(ncid_out))

  u_ind   = getindex(vars3d, 'u')   !< indices in the control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                    ! old logical massbal_adjust, if non-zero
  use_full_hydro = ( ql_ind > 0 .and. qi_ind > 0 .and. &
                     qr_ind > 0 .and. qs_ind > 0 .and. qg_ind > 0 )

  dsfg = open_dataset(filenamein, paropen=.true., mpicomm=iocomms(mem_pe(nproc)))
  call read_attribute(dsfg, 'ak', values_1d,errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading ak, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     ak(nlevs-k+2) = 0.01_r_kind*values_1d(k)
  enddo
  call read_attribute(dsfg, 'bk', values_1d,errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading bk, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=1,nlevs+1
     ! k=1 in values_1d is model top, flip so k=1 in ak is bottom
     bk(nlevs-k+2) = values_1d(k)
  enddo

  if (iope==0) then
    ! levels
    do k=1,nlevs
      levsout(k) = float(k)
      ilevsout(k) = float(k)
    end do
    ilevsout(nlevs+1) = float(nlevs+1)

    ! longitudes
    call read_vardata(dsfg, 'grid_xt', values_1d, errcode=iret)
    deglons(:) = values_1d
    call nccheck_incr(nf90_put_var(ncid_out, lonvarid, deglons, &
                         start = (/1/), count = (/nlons/)))

    call read_vardata(dsfg, 'grid_yt', values_1d, errcode=iret)
    ! latitudes
    do j=1,nlats
      deglats(nlats-j+1) = values_1d(j)
    end do

    call nccheck_incr(nf90_put_var(ncid_out, latvarid, deglats, &
                         start = (/1/), count = (/nlats/)))

    ! write to file
    call nccheck_incr(nf90_put_var(ncid_out, levvarid, sngl(levsout), &
                      start = (/1/), count = (/nlevs/)))
    ! pfull
    call nccheck_incr(nf90_put_var(ncid_out, pfullvarid, sngl(levsout), &
                      start = (/1/), count = (/nlevs/)))
    ! ilev
    call nccheck_incr(nf90_put_var(ncid_out, ilevvarid, sngl(ilevsout), &
                      start = (/1/), count = (/nlevs+1/)))
    ! hyai
    call nccheck_incr(nf90_put_var(ncid_out, hyaivarid, sngl(ilevsout), &
                      start = (/1/), count = (/nlevs+1/)))
    ! hybi
    call nccheck_incr(nf90_put_var(ncid_out, hybivarid, sngl(ilevsout), &
                      start = (/1/), count = (/nlevs+1/)))

  end if

  allocate(inc3d(nlons,nlats,nccount(3)))
  allocate(inc3d2(nlons,nlats,nccount(3)))
  allocate(inc3dout(nlons,nlats,nccount(3)))
  ! u increment
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (u_ind > 0) then
       call copyfromgrdin(grdin(:,levels(u_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('u_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, uvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! v increment
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (u_ind > 0) then
       call copyfromgrdin(grdin(:,levels(v_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('v_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, vvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! delp increment
  psinc(:) = zero
  if (ps_ind > 0) then
    call copyfromgrdin(grdin(:,levels(n3d) + ps_ind,nb,ne),psinc)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     inc = psinc*(bk(krev)-bk(krev+1))*100_r_kind
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('delp_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, delpvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! sphum increment
  allocate(tmp(nlons,nlats,nccount(3)),tv(nlons,nlats,nccount(3)),q(nlons,nlats,nccount(3)))
  allocate(tvanl(nlons,nlats,nccount(3)),tmpanl(nlons,nlats,nccount(3)),qanl(nlons,nlats,nccount(3)))
  call read_vardata(dsfg, 'spfh', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading spfh, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (q_ind > 0) then
       call copyfromgrdin(grdin(:,levels(q_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
     qanl(:,:,ki) = q(:,:,ki) + inc3d(:,:,ki)
  end do
  if (cliptracers)  where (qanl < clip) qanl = clip
  inc3d = qanl - q
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('sphum_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, sphumvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! t increment
  call read_vardata(dsfg, 'tmp', tmp, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading tmp, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  tv = tmp * ( 1.0 + fv*q)
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (tv_ind > 0) then
       call copyfromgrdin(grdin(:,levels(tv_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
     tvanl(:,:,ki) = tv(:,:,ki) + inc3d(:,:,ki)
     tmpanl(:,:,ki) = tvanl(:,:,ki)/(1. + fv*qanl(:,:,ki))
  end do
  inc3d = tmpanl - tmp
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('T_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, tvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! delz increment
  inc3d(:,:,:) = zero
  if (has_var(dsfg,'delz')) then
     allocate(delzb(nlons*nlats))
     call read_vardata(dsfg,'pressfc',values_2d,errcode=iret)
     if (allocated(psges)) deallocate(psges)
     allocate(psges(nlons*nlats))
     psges = reshape(values_2d,(/nlons*nlats/))
     vg = psges + (psinc*100_r_kind)
     do k=lev_pe1(iope), lev_pe2(iope)
        krev = nlevs-k+1
        ki = k - lev_pe1(iope) + 1
        ug=(rd/grav)*reshape(tvanl(:,:,ki),(/nlons*nlats/))
        ! ps in Pa here, need to multiply ak by 100.
        ! calculate analysis delz so it is negative.
        ! (note that ak,bk are already reversed to go from bottom to top)
        ug=ug*log((100_r_kind*ak(krev+1)+bk(krev+1)*vg)/(100_r_kind*ak(krev)+bk(krev)*vg))
        ! ug is hydrostatic analysis delz inferred from analysis ps,Tv
        ! delzb is hydrostatic background delz inferred from background ps,Tv
        ! calculate delzb so it is negative
        delzb=(rd/grav)*reshape(tv(:,:,ki),(/nlons*nlats/))
        delzb=delzb*log((100_r_kind*ak(krev+1)+bk(krev+1)*psges)/(100_r_kind*ak(krev)+bk(krev)*psges))
        inc3d(:,:,ki)=reshape(ug-delzb,(/nlons,nlats/))
     end do
  end if
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('delz_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, delzvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! o3mr increment
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (oz_ind > 0) then
       call copyfromgrdin(grdin(:,levels(oz_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
  end do
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('o3mr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, o3varid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! For hydrometeors, following the treatment for specific humidity increment
  ! Need to make sure the analysis value is not negative
  ! Read in background + increment and make sure the minimum is qcmin
  ! Adjust increment accordingly

  ! liq wat increment
  ! icmr increment
  ! if cw increment, make sure split the cw increment into ql and qi increments
  allocate(q2(nlons,nlats,nccount(3)),qanl2(nlons,nlats,nccount(3)))
  call read_vardata(dsfg, 'clwmr', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading clwmr, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  call read_vardata(dsfg, 'icmr', q2, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading icmr, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     ug = zero; vg = zero
     if (cw_ind > 0) then
        call copyfromgrdin(grdin(:,levels(cw_ind-1)+krev,nb,ne),ug)
     else if (ql_ind > 0) then
        call copyfromgrdin(grdin(:,levels(ql_ind-1)+krev,nb,ne),ug)
     end if
     ! analysis control variable is cw, need to split cw analysis to ql and qi 
     if (cw_ind > 0) then
        if (imp_physics == 11) then
           work = -r0_05 * (reshape(tmpanl(:,:,ki),(/nlons*nlats/)) - t0c)
           do i=1,nlons*nlats
              work(i) = max(zero,work(i))
              work(i) = min(one,work(i))
           enddo
           vg = ug * work          ! cloud ice
           ug = ug * (one - work)  ! cloud water
        endif
     else if (qi_ind > 0) then
        call copyfromgrdin(grdin(:,levels(qi_ind-1)+krev,nb,ne),vg)
     endif
     inc3d(:,:,ki) = reshape(ug,(/nlons,nlats/))  ! cloud water
     qanl(:,:,ki) = q(:,:,ki) + inc3d(:,:,ki)
     inc3d2(:,:,ki) = reshape(vg,(/nlons,nlats/)) ! cloud ice
     qanl2(:,:,ki) = q2(:,:,ki) + inc3d(:,:,ki)
  enddo

  ! adjust hydrometeor increment to make sure analysis is positive
  if (cliptracers)  where (qanl < qcmin) qanl = qcmin
  inc3d = qanl - q     ! ql
  if (cliptracers)  where (qanl2 < qcmin) qanl2 = qcmin
  inc3d2 = qanl2 - q2  ! qi

  ! output ql increment 
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('liq_wat_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, liqwatvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))
  ! output qi increment
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d2(:,j,:)
  end do
  if (should_zero_increments_for('icmr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, icvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! rwmr increment
  call read_vardata(dsfg, 'rwmr', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading rwmr, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (qr_ind > 0) then
       call copyfromgrdin(grdin(:,levels(qr_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
     qanl(:,:,ki) = q(:,:,ki) + inc3d(:,:,ki)
  end do
  if (cliptracers)  where (qanl < qcmin) qanl = qcmin
  inc3d = qanl - q   ! updated rwmr increment
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('rwmr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, rwmrvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! snmr increment
  call read_vardata(dsfg, 'snmr', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading snmr, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (qs_ind > 0) then
       call copyfromgrdin(grdin(:,levels(qs_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
     qanl(:,:,ki) = q(:,:,ki) + inc3d(:,:,ki)
  end do
  if (cliptracers)  where (qanl < qcmin) qanl = qcmin
  inc3d = qanl - q   ! updated snmr increment
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('snmr_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, snmrvarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  ! grle increment
  call read_vardata(dsfg, 'grle', q, ncstart=ncstart, nccount=nccount, errcode=iret)
  if (iret /= 0) then
     write(6,*)'WRITEINCREMENT_PNC:  ***FATAL ERROR*** reading grle, iret= ',iret,' PROGRAM STOPS'
     call stop2(29)
  endif
  do k=lev_pe1(iope), lev_pe2(iope)
     krev = nlevs-k+1
     ki = k - lev_pe1(iope) + 1
     inc(:) = zero
     if (qg_ind > 0) then
       call copyfromgrdin(grdin(:,levels(qg_ind-1) + krev,nb,ne),inc)
     endif
     inc3d(:,:,ki) = reshape(inc,(/nlons,nlats/))
     qanl(:,:,ki) = q(:,:,ki) + inc3d(:,:,ki)
  end do
  if (cliptracers)  where (qanl < qcmin) qanl = qcmin
  inc3d = qanl - q   ! updated grle increment
  do j=1,nlats
    inc3dout(:,nlats-j+1,:) = inc3d(:,j,:)
  end do
  if (should_zero_increments_for('grle_inc')) inc3dout = zero
  call nccheck_incr(nf90_put_var(ncid_out, grlevarid, sngl(inc3dout), &
                      start = ncstart, count = nccount))

  call mpi_barrier(iocomms(mem_pe(nproc)), iret)

  ! deallocate things
  deallocate(inc3d,inc3d2,inc3dout)
  deallocate(tmp,tv,q,tmpanl,tvanl,qanl)
  deallocate(q2,qanl2)
  if (allocated(delzb)) deallocate(delzb)
  if (allocated(psges)) deallocate(psges)

  !closing file 
  call nccheck_incr(nf90_close(ncid_out))

  end do backgroundloop ! loop over backgrounds to write out
  if (nproc == 0) then
       t2 = mpi_wtime()
       print *,'time in writeincrement_pnc atm_file on root',t2-t1,'secs'
  endif
 end if ! if (write_atm_file)

  if (write_sfc_file ) then

   if (nproc == 0) t1 = mpi_wtime()

   tmp2m_ind  = getindex(vars2d, 't2m')   !< indices in the state or control var arrays
   spfh2m_ind = getindex(vars2d, 'q2m')
   soilt1_ind = getindex(vars2d, 'st1')
   slc1_ind = getindex(vars2d, 'sl1')
   soilt2_ind = getindex(vars2d, 'st2')
   slc2_ind = getindex(vars2d, 'sl2')
   soilt3_ind = getindex(vars2d, 'st3')
   slc3_ind = getindex(vars2d, 'sl3')
   soilt4_ind = getindex(vars2d, 'st4')
   slc4_ind = getindex(vars2d, 'sl4')

   ! loop through times and do the read
   ne = 1
   write(charnanal,'(i3.3)') nanal
   sfcbackgroundloop: do nb=1,nbackgrounds

   if(no_inflate_flag) then
      filenameout = trim(adjustl(datapath))//trim(adjustl(incsfcfileprefixes(nb)))//"nimem"//charnanal
   else
      filenameout = trim(adjustl(datapath))//trim(adjustl(incsfcfileprefixes(nb)))//"mem"//charnanal
   end if
   filenamein = trim(adjustl(datapath))//trim(adjustl(fgsfcfileprefixes(nb)))//"mem"//charnanal
   
   !! note: only iope=0 is writing the outputs. Having all pes in iocomm write to a file slows it down.
   !! 
   if (iope==0) then
      dsfg = open_dataset(filenamein)
     ! create the output netCDF increment file
     call nccheck_incr(nf90_create(path=trim(filenameout), cmode=nf90_netcdf4,ncid=ncid_out))

     ! create dimensions based on analysis resolution, not guess
     call nccheck_incr(nf90_def_dim(ncid_out, "longitude", nlons, lon_dimid))
     call nccheck_incr(nf90_def_dim(ncid_out, "latitude", nlats, lat_dimid))
     dimids2 = (/ lon_dimid, lat_dimid /)
     ! create variables
     call nccheck_incr(nf90_def_var(ncid_out, "longitude", nf90_real,(/lon_dimid/), lonvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "latitude", nf90_real,(/lat_dimid/), latvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "tmp2m_inc", nf90_real, dimids2,tmp2mvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "spfh2m_inc", nf90_real, dimids2,spfh2mvarid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt1_inc", nf90_real, dimids2,soilt1varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt2_inc", nf90_real, dimids2,soilt2varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt3_inc", nf90_real, dimids2,soilt3varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilt4_inc", nf90_real, dimids2,soilt4varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc1_inc", nf90_real, dimids2,slc1varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc2_inc", nf90_real, dimids2,slc2varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc3_inc", nf90_real, dimids2,slc3varid))
     call nccheck_incr(nf90_def_var(ncid_out, "slc4_inc", nf90_real, dimids2,slc4varid))
     call nccheck_incr(nf90_def_var(ncid_out, "soilsnow_mask", nf90_int,dimids2, maskvarid))
     ! place global attributes to serial calc_increment output
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "source", "GSI EnKF"))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "comment", &
                       "global landsfc anal increment from writeincrement"))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global, "analysis_time",iadateout))
     call nccheck_incr(nf90_put_att(ncid_out, nf90_global,"IAU_hour_from_guess", nhr_anal(nb)))
     ! add units to lat/lon because that's what the calc_increment utility has
     call nccheck_incr(nf90_put_att(ncid_out, lonvarid, "units","degrees_east"))
     call nccheck_incr(nf90_put_att(ncid_out, latvarid, "units","degrees_north"))
     ! end the netCDF file definition
     call nccheck_incr(nf90_enddef(ncid_out))

      ! longitudes
      call read_vardata(dsfg, 'grid_xt', values_1d, errcode=iret)
      deglons(:) = values_1d
      call nccheck_incr(nf90_put_var(ncid_out, lonvarid, deglons, &
                           start = (/1/), count = (/nlons/)))

      call read_vardata(dsfg, 'grid_yt', values_1d, errcode=iret)
      ! latitudes
      do j=1,nlats
        deglats(nlats-j+1) = values_1d(j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, latvarid, deglats, &
                           start = (/1/), count = (/nlats/)))
      ! construct mask (1 - soil, 2 - snow, 0 - not snow)
      ! note: same logic/threshold used in global_cycle to produce
      ! mask on model grid.
      call read_vardata(dsfg, 'soill1', values_2d, errcode=iret)
      mask = 0
      do j=1,nlats
         do i = 1, nlons
            if (values_2d(i,j) .LT. 1.0) then
            mask(i,nlats-j+1) = 1
            endif
         enddo
      end do
      call read_vardata(dsfg, 'weasd', values_2d, errcode=iret)
      do j=1,nlats
         do i = 1, nlons
            if (values_2d(i,j) .GT. 0.001) then
            mask(i,nlats-j+1) = 2
            endif
         enddo
      end do
      call nccheck_incr(nf90_put_var(ncid_out, maskvarid, mask, &
                        start = ncstart(1:2), count = nccount(1:2)))

      allocate(inc2d(nlons,nlats))
      allocate(inc2dout(nlons,nlats))

      ! tmp2m increment
      inc(:) = zero
      if (tmp2m_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d) + tmp2m_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, tmp2mvarid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! spfh2m increment
      inc(:) = zero
      if (spfh2m_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+spfh2m_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, spfh2mvarid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! soilt1 increment
      inc(:) = zero
      if (soilt1_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+soilt1_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      inc2dout=0.
      do j=1,nlats
         do i = 1, nlons
            if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
         enddo
      end do
      call nccheck_incr(nf90_put_var(ncid_out, soilt1varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! soilt2 increment
      inc(:) = zero
      if (soilt2_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+soilt2_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      inc2dout=0.
      do j=1,nlats
         do i = 1, nlons
            if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
         enddo
      end do
      call nccheck_incr(nf90_put_var(ncid_out, soilt2varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! soilt3 increment
      inc(:) = zero
      if (soilt3_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+soilt3_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      inc2dout=0.
      do j=1,nlats
         do i = 1, nlons
            if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
         enddo
      end do
      call nccheck_incr(nf90_put_var(ncid_out, soilt3varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! soilt4 increment
      inc(:) = zero
      if (soilt4_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+soilt4_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      inc2dout=0.
      do j=1,nlats
         do i = 1, nlons
            if (mask(i,nlats-j+1) .NE. 0) inc2dout(i,nlats-j+1) = inc2d(i,j)
         enddo
      end do
      call nccheck_incr(nf90_put_var(ncid_out, soilt4varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! slc1 increment
      inc(:) = zero
      if (slc1_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+slc1_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, slc1varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! slc2 increment
      inc(:) = zero
      if (slc2_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+slc2_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, slc2varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! slc3 increment
      inc(:) = zero
      if (slc3_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+slc3_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, slc3varid, sngl(inc2dout), &
                           start = ncstart(1:2), count = nccount(1:2)))
      ! slc4 increment
      inc(:) = zero
      if (slc4_ind > 0) then
         call copyfromgrdin(grdin(:,levels(n3d)+slc4_ind,nb,ne),inc)
      endif
      inc2d(:,:) = reshape(inc,(/nlons,nlats/))
      do j=1,nlats
         inc2dout(:,nlats-j+1) = inc2d(:,j)
      end do
      call nccheck_incr(nf90_put_var(ncid_out, slc4varid, sngl(inc2dout), &
                        start = ncstart(1:2), count = nccount(1:2)))

      call close_dataset(dsfg,errcode=iret)
      if (iret/=0) then
         write(6,*)'gridio/writeincrement_par: problem closing netcdf sfc fg dataset, iret=',iret
         call stop2(23)
      endif
      ! deallocate things
      deallocate(inc2d,inc2dout)

      call nccheck_incr(nf90_close(ncid_out))

   end if
   
   end do sfcbackgroundloop ! loop over backgrounds to read in   
   if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time in writeincrement_pnc sfc_file on root',t2-t1,'secs'
   endif   
  endif  !write_Sfc

  ! remove the sub communicators
  call mpi_barrier(iocomms(mem_pe(nproc)), iret)
  call mpi_comm_free(iocomms(mem_pe(nproc)), iret)
  call mpi_barrier(mpi_comm_world, iret)

  return

 contains
! copying to grdin (calling regtoreduced if reduced grid)
 subroutine copyfromgrdin(grdin, field)
 implicit none

 real(r_single), dimension(:), intent(in)      :: grdin
 real(r_kind), dimension(:), intent(inout) :: field

 if (reducedgrid) then
   call reducedtoreg(grdin, field)
 else
   field = grdin
 endif

 end subroutine copyfromgrdin

 end subroutine writeincrement_pnc

 subroutine set_ncio_file_flags(vars3d, n3d, vars2d, n2d, sfc_file, atm_file)
 ! determine if variables are in sfc and/or atm file, for ncio case.
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  integer, intent(in) :: n2d, n3d
  logical, intent(out) :: sfc_file, atm_file

  integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind, cw_ind
  integer(i_kind) :: qr_ind, qs_ind, qg_ind
  integer(i_kind) :: tsen_ind, ql_ind, qi_ind, prse_ind
  integer(i_kind) :: ps_ind, pst_ind, sst_ind
  integer(i_kind) :: tmp2m_ind, spfh2m_ind, soilt1_ind, soilt2_ind, soilt3_ind
  integer(i_kind) :: soilt4_ind,slc1_ind, slc2_ind, slc3_ind, slc4_ind

 ! atmos file variables
  u_ind   = getindex(vars3d, 'u')   !< indices in the state or control var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
  cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
  tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
  ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
  qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
  prse_ind = getindex(vars3d, 'prse')
  qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
  qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
  qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
  pst_ind = getindex(vars2d, 'pst') ! Ps tendency (2D)   // equivalent of
                                     ! old logical massbal_adjust, if non-zero
  sst_ind = getindex(vars2d, 'sst') ! is this really in the atmos file?

  ! for nc gfs io determine if requested variables are in sfc and/or atmos file
  atm_file = ( u_ind>0 .or. v_ind>0 .or. tv_ind>0 .or. q_ind>0 .or. sst_ind>0 .or. &
                  oz_ind>0 .or. cw_ind>0 .or. tsen_ind>0 .or. ql_ind>0 .or. &
                  qi_ind>0 .or. prse_ind>0 .or. qr_ind>0 .or. qs_ind>0 .or. qg_ind>0 )

  ! sfc file variables
  tmp2m_ind  = getindex(vars2d, 't2m')
  spfh2m_ind = getindex(vars2d, 'q2m')
  soilt1_ind = getindex(vars2d, 'st1')
  slc1_ind = getindex(vars2d, 'sl1')
  soilt2_ind = getindex(vars2d, 'st2')
  slc2_ind = getindex(vars2d, 'sl2')
  soilt3_ind = getindex(vars2d, 'st3')
  slc3_ind = getindex(vars2d, 'sl3')
  soilt4_ind = getindex(vars2d, 'st4')
  slc4_ind = getindex(vars2d, 'sl4')

  sfc_file = ( tmp2m_ind > 0 .or. spfh2m_ind > 0 .or. soilt1_ind > 0 .or. &
       slc1_ind > 0 .or. soilt2_ind > 0 .or. slc2_ind > 0 .or.    &
       soilt3_ind > 0 .or. slc3_ind > 0 .or. soilt4_ind > 0 .or.  &
       slc4_ind  > 0 )

 end subroutine set_ncio_file_flags


 logical function checkfield(field,fields,nrec) result(hasfield)
   use nemsio_module, only: nemsio_charkind
   integer, intent(in) :: nrec
   character(nemsio_charkind), intent(in) :: fields(nrec),field
   integer n
   hasfield = .false.
   do n=1,nrec
      if (field == fields(n)) hasfield=.true.
   enddo
 end function checkfield

  subroutine nccheck_incr(status)
    use netcdf
    integer, intent (in   ) :: status
    if (status /= nf90_noerr) then
      print *, "fv3_increment netCDF error ", trim(nf90_strerror(status))
      call stop2(999)
    end if
  end subroutine nccheck_incr

  !! Is this variable in incvars_to_zero?
  logical function should_zero_increments_for(check_var)
    use params, only : incvars_to_zero

    character(len=*), intent(in) :: check_var !! Variable to search for

    ! Local variables

    character(len=12) :: varname ! temporary string for storing variable names
    integer :: i ! incvars_to_zero loop index

    should_zero_increments_for=.false.

    zeros_loop: do i=1,size(incvars_to_zero)
       varname = incvars_to_zero(i)
       if ( trim(varname) == check_var ) then
          should_zero_increments_for=.true.
          return
       endif
    end do zeros_loop

  end function should_zero_increments_for

end module gridio
