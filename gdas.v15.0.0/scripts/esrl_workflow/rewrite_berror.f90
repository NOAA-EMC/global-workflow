  program rewrite_berror
  ! rewrite GFS berror_stats file with one less level (for FV3)
  implicit none
  integer inerr,nsig,nlon,nlat,ier,isig,istat,cwoption,oerr
  character*5 var

  real,allocatable,dimension(:,:,:):: agvin
  real,allocatable,dimension(:,:) :: wgvin,bvin
  real,allocatable,dimension(:,:):: hwllin
  real,allocatable,dimension(:,:):: corzin
  real,allocatable,dimension(:,:):: corq2
  real,allocatable,dimension(:,:):: vscalesin
  real,allocatable,dimension(:,:) :: corsst
  real,allocatable,dimension(:,:) :: hsst

  real,allocatable,dimension(:,:,:):: agvout
  real,allocatable,dimension(:,:) :: wgvout,bvout
  real,allocatable,dimension(:,:):: hwllino
  real,allocatable,dimension(:,:):: corzino
  real,allocatable,dimension(:,:):: corq2o
  real,allocatable,dimension(:,:):: vscalesino

  cwoption = 0
  inerr = 77
  oerr = 99
  open(inerr,file='berror_stats',form='unformatted',status='old',iostat=ier)
  open(oerr,file='berror_stats_out',form='unformatted',iostat=ier)
  rewind(inerr)
  rewind(oerr)
  read(inerr,iostat=ier)nsig,nlat,nlon
  write(oerr) nsig-1,nlat,nlon
  print *,'nsig,nlat',nsig,nlat,nlon
  allocate(agvin(nlat,nsig,nsig),wgvin(nlat,nsig),bvin(nlat,nsig))
  allocate(agvout(nlat,nsig-1,nsig-1),wgvout(nlat,nsig-1),bvout(nlat,nsig-1))
  read(inerr,iostat=ier) agvin,bvin,wgvin
  agvout = agvin(:,1:nsig-1,1:nsig-1)  
  bvout = bvin(:,1:nsig-1)
  wgvout = wgvin(:,1:nsig-1)
  write(oerr) agvout,bvout,wgvout
  deallocate(agvin,agvout,bvin,bvout,wgvin,wgvout)
! Read amplitudes
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat/=0) exit
     if (isig > 1) then
        write(oerr) var,isig-1
     else
        write(oerr) var,isig
     endif
     print *,'var,isig ',trim(var),isig
     allocate ( corzin(nlat,isig) )
     allocate ( corzino(nlat,isig-1) )
     if (var=='q' .or. var=='cw') then
         allocate ( corq2(nlat,isig),corq2o(nlat,isig-1) )
     endif
     allocate ( hwllin(nlat,isig) )
     allocate ( hwllino(nlat,isig-1) )
     if (isig>1) then
         allocate ( vscalesin(nlat,isig),vscalesino(nlat,isig-1) )
     endif

     if (var/='sst') then
        if (var=='q' .or. var=='Q' .or. (var=='cw' .and. cwoption==2)) then
           read(inerr,iostat=ier) corzin,corq2
           if (isig > 1) then
              corzino = corzin(:,1:isig-1)
              corq2o = corq2(:,1:isig-1)
              write(oerr) corzino,corq2o
           else
              write(oerr) corzin,corq2
           endif
        else
           read(inerr,iostat=ier) corzin
           if (isig > 1) then
              corzino = corzin(:,1:isig-1)
              write(oerr) corzino
           else
              write(oerr) corzin
           endif
        end if
        read(inerr,iostat=ier) hwllin
        if (isig > 1) then
           hwllino = hwllin(:,1:isig-1)
           write(oerr) hwllino
        else
           write(oerr) hwllin
        endif
        if (isig>1) then
           read(inerr,iostat=ier) vscalesin
           vscalesino = vscalesin(:,1:isig-1)
           write(oerr) vscalesino
        endif
     else
        allocate(corsst(nlat,nlon),hsst(nlat,nlon))
        read(inerr,iostat=ier) corsst
        read(inerr,iostat=ier) hsst
        write(oerr) corsst
        write(oerr) hsst
     end if
     if (allocated(corzin)) deallocate(corzin)
     if (allocated(corq2)) deallocate(corq2)
     if (allocated(hwllin)) deallocate(hwllin)
     if (allocated(vscalesin)) deallocate(vscalesin)
     if (allocated(corzino)) deallocate(corzino)
     if (allocated(corq2o)) deallocate(corq2o)
     if (allocated(hwllino)) deallocate(hwllino)
     if (allocated(vscalesino)) deallocate(vscalesino)
     if (allocated(corsst)) deallocate(corsst)
     if (allocated(hsst)) deallocate(hsst)
  enddo read 
  close(oerr)
  close(inerr)
  end program rewrite_berror
