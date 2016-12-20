module mp_compact_diffs_mod1
!$$$ module documentation block
!           .      .    .                                       .
! module:   mp_compact_diffs_mod1
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added module doc block
!
! subroutines included:
!   sub init_mp_compact_diffs1
!   sub destroy_mp_compact_diffs1
!   sub cdiff_sd2ew0
!   sub cdiff_sd2ew1
!   sub cdiff_ew2sd1
!   sub cdiff_sd2ew
!   sub cdiff_sd2ew2
!   sub cdiff_ew2sd
!   sub cdiff_ew2sd2
!   sub cdiff_sd2ns0
!   sub cdiff_sd2ns1
!   sub cdiff_ns2sd1
!   sub cdiff_sd2ns
!   sub cdiff_sd2ns2
!   sub cdiff_ns2sd
!   sub cdiff_ns2sd2
!   sub mp_compact_dlon
!   sub mp_compact_dlon_ad
!   sub mp_compact_dlat
!   sub mp_compact_dlat_ad
!   sub mp_uv_pole
!   sub mp_uv_pole_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_mp_compact_diffs1
  public :: destroy_mp_compact_diffs1
  public :: cdiff_sd2ew0
  public :: cdiff_sd2ew1
  public :: cdiff_ew2sd1
  public :: cdiff_sd2ew
  public :: cdiff_sd2ew2
  public :: cdiff_ew2sd
  public :: cdiff_ew2sd2
  public :: cdiff_sd2ns0
  public :: cdiff_sd2ns1
  public :: cdiff_ns2sd1
  public :: cdiff_sd2ns
  public :: cdiff_sd2ns2
  public :: cdiff_ns2sd
  public :: cdiff_ns2sd2
  public :: mp_compact_dlon
  public :: mp_compact_dlon_ad
  public :: mp_compact_dlat
  public :: mp_compact_dlat_ad
  public :: mp_uv_pole
  public :: mp_uv_pole_ad
! set passed variables to public
  public :: nlat_1,slow_pole,nlat_0,nlon_0,nlon_1

  integer(i_kind),allocatable::list_sd2ew(:,:)   ! list_sd2ew(1,j) = lat index 1 for ew strip j
                                                 ! list_sd2ew(2,j) = lat index 2 for ew strip j
                                                 ! list_sd2ew(3,j) = vert index for ew strip j
                                                 ! list_sd2ew(4,j) = pe of this lat/vert index strip
  integer(i_kind) nlat_0,nlat_1
  integer(i_kind) nallsend_sd2ew,nallrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ew,nrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ew,ndrecv_sd2ew
  integer(i_kind),allocatable,dimension(:,:)::info_send_sd2ew,info_recv_sd2ew
  integer(i_kind) nallsend_ew2sd,nallrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::nsend_ew2sd,nrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ew2sd,ndrecv_ew2sd
  integer(i_kind),allocatable,dimension(:,:)::info_send_ew2sd,info_recv_ew2sd

  integer(i_kind),allocatable::list_sd2ns(:,:)   ! list_sd2ns(1,j) = lon index 1 for ew strip j
                                                 ! list_sd2ns(2,j) = lon index 2 for ew strip j
                                                 ! list_sd2ns(3,j) = vert index for ew strip j
                                                 ! list_sd2ns(4,j) = pe of this lat/vert index strip
  integer(i_kind) nlon_0,nlon_1
  integer(i_kind) nallsend_sd2ns,nallrecv_sd2ns
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ns,nrecv_sd2ns
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ns,ndrecv_sd2ns
  integer(i_kind),allocatable,dimension(:,:)::info_send_sd2ns,info_recv_sd2ns
  integer(i_kind) nallsend_ns2sd,nallrecv_ns2sd
  integer(i_kind),allocatable,dimension(:)::nsend_ns2sd,nrecv_ns2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ns2sd,ndrecv_ns2sd
  integer(i_kind),allocatable,dimension(:,:)::info_send_ns2sd,info_recv_ns2sd

  logical slow_pole

contains

subroutine init_mp_compact_diffs1(nlev,mype,slow_pole_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mp_compact_diffs1
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    slow_pole_in
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype
  logical        ,intent(in   ) :: slow_pole_in

  slow_pole=slow_pole_in
  call cdiff_sd2ew0(nlev,mype)
  call cdiff_sd2ew1(nlev,mype)
  call cdiff_ew2sd1(mype)
  call cdiff_sd2ns0(nlev,mype)
  call cdiff_sd2ns1(nlev,mype)
  call cdiff_ns2sd1(mype)

end subroutine init_mp_compact_diffs1

subroutine destroy_mp_compact_diffs1
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_mp_compact_diffs1
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(list_sd2ew,nsend_sd2ew,nrecv_sd2ew,ndsend_sd2ew,ndrecv_sd2ew)
  deallocate(info_send_sd2ew,info_recv_sd2ew,nsend_ew2sd,nrecv_ew2sd)
  deallocate(ndsend_ew2sd,ndrecv_ew2sd,info_send_ew2sd,info_recv_ew2sd)

  deallocate(list_sd2ns,nsend_sd2ns,nrecv_sd2ns,ndsend_sd2ns,ndrecv_sd2ns)
  deallocate(info_send_sd2ns,info_recv_sd2ns,nsend_ns2sd,nrecv_ns2sd)
  deallocate(ndsend_ns2sd,ndrecv_ns2sd,info_send_ns2sd,info_recv_ns2sd)

end subroutine destroy_mp_compact_diffs1

subroutine cdiff_sd2ew0(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew0
!   prgmmr:
!
! abstract: create ew (lat strips) subdivision for use in global spectral transform
!
!  output:
!
!     nlat_0,nlat_1:   range of lat/vert index on processor mype
!
!                    1 <= nlat_0 <= nlat_1 <= ((nlat+1)/2)*nlev
!                    if npe > ((nlat+1)/2)*nlev, then will have nlat_0 = -1, nlat_1 = -2
!                    on some processors, and nlat_0=nlat_1 on the
!                    remaining ((nlat+1)/2)*nlev processors
!
!     list_sd2ew(4,((nlat+1)/2)*nlev):  global definition of contents of each lat/vert strip
!                      list_sd2ew(1,j) = lat index 1 for ew strip j
!                      list_sd2ew(2,j) = lat index 2 for ew strip j
!                      list_sd2ew(3,j) = vert level for ew strip j
!                      list_sd2ew(4,j) = pe of this lat/vert strip
!
!                      because pole values are computed from the row adjacent to the pole,
!                      the latitudes are kept in adjacent pairs, ie (1,2),(3,4),...,(nlat-1,nlat)
!
!                      if the number of lats is odd, then the second pair contains two duplicate
!                      latitudes, ie  (1,2),(3,3),(4,5),...,(nlat-1,nlat)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat
  use mpimod, only: npe
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) nlat_this,nlat_tot,kchk,i,k,kk,n,nn

  allocate(list_sd2ew(4,((nlat+1)/2)*nlev))

  nlat_tot=((nlat+1)/2)*nlev
  nlat_this=nlat_tot/npe
  if(mod(nlat_tot,npe)/=0) nlat_this=nlat_this+1
  if(mod(nlat_tot,npe)==0) then
     kchk=npe
  else
     kchk=mod(nlat_tot,npe)
  end if

  nn=0
  do k=1,nlev
     nn=nn+1
     list_sd2ew(1,nn)=1
     list_sd2ew(2,nn)=2
     list_sd2ew(3,nn)=k
     list_sd2ew(4,nn)=-1
     if(mod(nlat,2)/=0) then
!                           nlat odd:
        nn=nn+1
        list_sd2ew(1,nn)=3
        list_sd2ew(2,nn)=3
        list_sd2ew(3,nn)=k
        list_sd2ew(4,nn)=-1
        do i=4,nlat-1,2
           nn=nn+1
           list_sd2ew(1,nn)=i
           list_sd2ew(2,nn)=i+1
           list_sd2ew(3,nn)=k
           list_sd2ew(4,nn)=-1
        end do

     else
!                           nlat even:
        do i=3,nlat-1,2
           nn=nn+1
           list_sd2ew(1,nn)=i
           list_sd2ew(2,nn)=i+1
           list_sd2ew(3,nn)=k
           list_sd2ew(4,nn)=-1
        end do

     end if

  end do

!  if(mype==0) write(0,*)' nn,nlat_tot,nlat,nlev=',nn,nlat_tot,nlat,nlev

  nlat_0=-1
  nlat_1=-2
  nn=0
  do n=1,npe
     if(n<=kchk) then
        kk=nlat_this
     else
        kk=nlat_this-1
     end if
     if(kk>0) then
        if(mype+1==n) then
           nlat_0=nn+1
           nlat_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           list_sd2ew(4,nn)=n
        end do
     end if
  end do
! write(0,*) '  mype,nlat_0,nlat_1,nlat_1-nlat0+1=',mype,nlat_0,nlat_1,nlat_1-nlat_0+1
! if(mype==0) then
!    do i=1,nlat_tot
!       write(0,'(" i,list_sd2ew(:,i)=",i5,4i6)')i,list_sd2ew(1:4,i)
!    end do
! end if

end subroutine cdiff_sd2ew0

subroutine cdiff_sd2ew1(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew1
!   prgmmr:
!
! abstract: continue with setup for subdomain to lat strip interchanges
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) list2(nlat,nlev)
  integer(i_kind) i,ii,ii0,ilat,ilat_1,ilat_2,ivert,j,mm1,nn,nlonloc,ipe,ilatm,ilon,mpi_string1
  integer(i_kind) isig,nlat_tot,i12

  allocate(nsend_sd2ew(npe),nrecv_sd2ew(npe),ndsend_sd2ew(npe+1),ndrecv_sd2ew(npe+1))
  mm1=mype+1
  nlat_tot=((nlat+1)/2)*nlev

  nn=0
  list2=0
  do j=1,nlat_tot
     ilat_1=list_sd2ew(1,j)
     ilat_2=list_sd2ew(2,j)
     ivert=list_sd2ew(3,j)
     if(list2(ilat_1,ivert)/=0.or.list2(ilat_2,ivert)/=0) then
        if(mype==0) write(0,*)' problem in cdiff_sd2ew1'
        call mpi_finalize(i)
        stop
     end if
     list2(ilat_1,ivert)=j
     list2(ilat_2,ivert)=j
  end do
  do ivert=1,nlev
     do ilat=1,nlat
        if(list2(ilat,ivert)==0) then
           if(mype==0) write(0,*)' problem in cdiff_sd2ew1'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend_sd2ew=0
  nlonloc=lon2-2
  do ivert=1,nlev
     do i=2,lat2-1
        ilat=i+istart(mm1)-2
        j=list2(ilat,ivert)
        ipe=list_sd2ew(4,j)
        nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+nlonloc
     end do
  end do

  ndsend_sd2ew(1)=0
  do i=2,npe+1
     ndsend_sd2ew(i)=ndsend_sd2ew(i-1)+nsend_sd2ew(i-1)
  end do
  nallsend_sd2ew=ndsend_sd2ew(npe+1)
  allocate(info_send_sd2ew(4,nallsend_sd2ew))
  nsend_sd2ew=0
  do ivert=1,nlev
     do i=2,lat2-1
        ilat=i+istart(mm1)-2
        ilatm=list2(ilat,ivert)
        ilat_1=list_sd2ew(1,ilatm)
        ilat_2=list_sd2ew(2,ilatm)
        i12=0
        if(ilat_1==ilat) i12=1
        if(ilat_2==ilat) i12=2
        isig =list_sd2ew(3,ilatm)
        ipe=list_sd2ew(4,ilatm)
        do ii=2,lon2-1
           ilon=ii+jstart(mm1)-2
           nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+1
           ii0=ndsend_sd2ew(ipe)+nsend_sd2ew(ipe)
           info_send_sd2ew(1,ii0)=ilon
           info_send_sd2ew(2,ii0)=ilatm
           info_send_sd2ew(3,ii0)=i12
           info_send_sd2ew(4,ii0)=isig
        end do
     end do
  end do

  call mpi_alltoall(nsend_sd2ew,1,mpi_integer4,nrecv_sd2ew,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ew(1)=0
  do i=2,npe+1
     ndrecv_sd2ew(i)=ndrecv_sd2ew(i-1)+nrecv_sd2ew(i-1)
  end do
  nallrecv_sd2ew=ndrecv_sd2ew(npe+1)
  allocate(info_recv_sd2ew(4,nallrecv_sd2ew))
  call mpi_type_contiguous(4,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ew,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     info_recv_sd2ew,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_sd2ew1

subroutine cdiff_ew2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,jstart,istart,ilat1,jlon1
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,i1,i2,ilat_1,ilat_2,ivert,j,k,mm1,ipe,ilon,mpi_string1,nn

  allocate(nsend_ew2sd(npe),nrecv_ew2sd(npe),ndsend_ew2sd(npe+1),ndrecv_ew2sd(npe+1))
  mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
     nn=0
     do k=nlat_0,nlat_1
        ilat_1=list_sd2ew(1,k)
        ilat_2=list_sd2ew(2,k)
        ivert=list_sd2ew(3,k)
        i1=ilat_1-istart(ipe)+2
        if(i1>=1.and.i1<=ilat1(ipe)+2) then
           do j=1,jlon1(ipe)+2
              nn=nn+1
           end do
        end if
        if(ilat_1==ilat_2) cycle
        i2=ilat_2-istart(ipe)+2
        if(i2>=1.and.i2<=ilat1(ipe)+2) then
           do j=1,jlon1(ipe)+2
              nn=nn+1
           end do
        end if
     end do
     nsend_ew2sd(ipe)=nn
  end do

  ndsend_ew2sd(1)=0
  do i=2,npe+1
     ndsend_ew2sd(i)=ndsend_ew2sd(i-1)+nsend_ew2sd(i-1)
  end do
  nallsend_ew2sd=ndsend_ew2sd(npe+1)
  allocate(info_send_ew2sd(4,nallsend_ew2sd))
  nn=0
  do ipe=1,npe
     do k=nlat_0,nlat_1
        ilat_1=list_sd2ew(1,k)
        ilat_2=list_sd2ew(2,k)
        ivert=list_sd2ew(3,k)
        i1=ilat_1-istart(ipe)+2
        if(i1>=1.and.i1<=ilat1(ipe)+2) then
           do j=1,jlon1(ipe)+2
              ilon=j+jstart(ipe)-2
              if(ilon<1) ilon=ilon+nlon
              if(ilon>nlon) ilon=ilon-nlon
              nn=nn+1
              info_send_ew2sd(1,nn)=ilon
              info_send_ew2sd(2,nn)=j
              info_send_ew2sd(3,nn)=k
              info_send_ew2sd(4,nn)=1
           end do
        end if
        if(ilat_1==ilat_2) cycle
        i2=ilat_2-istart(ipe)+2
        if(i2>=1.and.i2<=ilat1(ipe)+2) then
           do j=1,jlon1(ipe)+2
              ilon=j+jstart(ipe)-2
              if(ilon<1) ilon=ilon+nlon
              if(ilon>nlon) ilon=ilon-nlon
              nn=nn+1
              info_send_ew2sd(1,nn)=ilon
              info_send_ew2sd(2,nn)=j
              info_send_ew2sd(3,nn)=k
              info_send_ew2sd(4,nn)=2
           end do
        end if
     end do
  end do

  call mpi_alltoall(nsend_ew2sd,1,mpi_integer4,nrecv_ew2sd,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ew2sd(1)=0
  do i=2,npe+1
     ndrecv_ew2sd(i)=ndrecv_ew2sd(i-1)+nrecv_ew2sd(i-1)
  end do
  nallrecv_ew2sd=ndrecv_ew2sd(npe+1)
  allocate(info_recv_ew2sd(4,nallrecv_ew2sd))
  call mpi_type_contiguous(4,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ew2sd,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     info_recv_ew2sd,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_ew2sd1

subroutine cdiff_sd2ew(u_sd,u_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ew (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_sd
!
!   output argument list:
!    u_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u_ew

  integer(i_kind) i12,ilat,ilatm,ilon,ivert,j,mm1
  real(r_kind),allocatable::sendbuf(:),recvbuf(:)

  mm1=mype+1

  allocate(sendbuf(nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)
     ilatm=info_send_sd2ew(2,j)
     i12=info_send_sd2ew(3,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     sendbuf(j)=u_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
  end do
  allocate(recvbuf(nallrecv_sd2ew))
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_rtype, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     i12=info_recv_sd2ew(3,j)
     u_ew(i12,ilon,ilatm)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ew

subroutine cdiff_sd2ew2(u1_sd,u2_sd,u1_ew,u2_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ew (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ew,u2_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u2_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u1_ew
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u2_ew

  integer(i_kind) i12,ilat,ilatm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+1

  allocate(sendbuf(2,nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)
     ilatm=info_send_sd2ew(2,j)
     i12=info_send_sd2ew(3,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
  end do
  allocate(recvbuf(2,nallrecv_sd2ew))
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     i12=info_recv_sd2ew(3,j)
     u1_ew(i12,ilon,ilatm)=recvbuf(1,j)
     u2_ew(i12,ilon,ilatm)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ew2

subroutine cdiff_ew2sd(u_sd,u_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_ew
!
!   output argument list:
!    u_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: u_ew

  real(r_kind),allocatable::sendbuf(:),recvbuf(:)
  integer(i_kind) i12,ilat,ivert,j,mm1,ilatm,ilon,ilonloc

  mm1=mype+1

  allocate(sendbuf(nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     i12=info_send_ew2sd(4,j)
     sendbuf(j)=u_ew(i12,ilon,ilatm)
  end do
  allocate(recvbuf(nallrecv_ew2sd))
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_rtype, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     i12=info_recv_ew2sd(4,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     u_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(j)
!--------------check for north or south pole
     ilat=-1
     if(list_sd2ew(i12,ilatm)==nlat) ilat=nlat+1
     if(list_sd2ew(i12,ilatm)==1) ilat=0
     if(ilat==-1) cycle
!-----------------do repeat rows for north/south pole
     u_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_ew2sd

subroutine cdiff_ew2sd2(u1_sd,u2_sd,u1_ew,u2_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ew,u2_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: u1_ew,u2_ew

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,mm1,ilatm,ilon,ilonloc,mpi_string1

  mm1=mype+1

  allocate(sendbuf(2,nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     i12=info_send_ew2sd(4,j)
     sendbuf(1,j)=u1_ew(i12,ilon,ilatm)
     sendbuf(2,j)=u2_ew(i12,ilon,ilatm)
  end do
  allocate(recvbuf(2,nallrecv_ew2sd))
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     i12=info_recv_ew2sd(4,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     u1_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(2,j)
!--------------check for north or south pole
     ilat=-1
     if(list_sd2ew(i12,ilatm)==nlat) ilat=nlat+1
     if(list_sd2ew(i12,ilatm)==1) ilat=0
     if(ilat==-1) cycle
!-----------------do repeat rows for north/south pole
     u1_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_ew2sd2

subroutine cdiff_sd2ns0(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns0
!   prgmmr:
!
! abstract: create ns (lon strips) subdivision for use with compact differences in latitude
!
!  output:
!
!     nlon_0,nlon_1:   range of lon/vert index on processor mype
!
!                    1 <= nlon_0 <= nlon_1 <= (nlon/2)*nlev
!                    if npe > (nlon/2)*nlev, then will have nlon_0 = -1, nlon_1 = -2
!                    on some processors, and nlon_0=nlon_1 on the
!                    remaining (nlon/2)*nlev processors
!
!     list_sd2ns(4,(nlon/2)*nlev):  global definition of contents of each lon/vert strip
!                      list_sd2ns(1,j) = lon index 1 for ns strip j
!                      list_sd2ns(2,j) = lon index 2 for ns strip j
!                      list_sd2ns(3,j) = vert level for ns strip j
!                      list_sd2ns(4,j) = pe of this lon/vert strip
!
!       NOTE:  only works for nlon even, because longitudes must be in pairs to
!                complete a great circle through the poles.
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon
  use mpimod, only: npe
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) nlon_this,nlon_tot,kchk,i,k,kk,n,nn,nlonh

  if(mod(nlon,2)/=0) then
     write(6,*)' FAILURE IN cdiff_sd2ns0, nlon not even'
     call stop2(99)
  end if
  nlonh=nlon/2
  allocate(list_sd2ns(4,nlonh*nlev))

  nlon_tot=nlonh*nlev
  nlon_this=nlon_tot/npe
  if(mod(nlon_tot,npe)/=0) nlon_this=nlon_this+1
  if(mod(nlon_tot,npe)==0) then
     kchk=npe
  else
     kchk=mod(nlon_tot,npe)
  end if

  nn=0
  do k=1,nlev
     do i=1,nlonh
        nn=nn+1
        list_sd2ns(1,nn)=i
        list_sd2ns(2,nn)=i+nlonh
        list_sd2ns(3,nn)=k
        list_sd2ns(4,nn)=-1
     end do

  end do

!  if(mype==0) write(0,*)' nn,nlon_tot,nlon,nlonh,nlev=',nn,nlon_tot,nlon,nlonh,nlev

  nlon_0=-1
  nlon_1=-2
  nn=0
  do n=1,npe
     if(n<=kchk) then
        kk=nlon_this
     else
        kk=nlon_this-1
     end if
     if(kk>0) then
        if(mype+1==n) then
           nlon_0=nn+1
           nlon_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           list_sd2ns(4,nn)=n
        end do
     end if
  end do
!  write(0,*) '  mype,nlon_0,nlon_1,nlon_1-nlon0+1=',mype,nlon_0,nlon_1,nlon_1-nlon_0+1
!  if(mype==0) then
!     do i=1,nlon_tot
!        write(0,'(" i,list_sd2ns(:,i)=",i5,4i6)')i,list_sd2ns(1:4,i)
!     end do
!  end if

end subroutine cdiff_sd2ns0

subroutine cdiff_sd2ns1(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns1
!   prgmmr:
!
! abstract: continue with setup for subdomain to lat strip interchanges
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) list2(nlon,nlev)
  integer(i_kind) i,ii,ii0,ilat,ilon_1,ilon_2,ivert,j,mm1,nn,ipe,ilon,mpi_string1
  integer(i_kind) isig,nlon_tot,i12,nlonh,nlatloc,ilonm

  allocate(nsend_sd2ns(npe),nrecv_sd2ns(npe),ndsend_sd2ns(npe+1),ndrecv_sd2ns(npe+1))
  mm1=mype+1
  nlonh=nlon/2
  nlon_tot=nlonh*nlev

  nn=0
  list2=0
  do j=1,nlon_tot
     ilon_1=list_sd2ns(1,j)
     ilon_2=list_sd2ns(2,j)
     ivert=list_sd2ns(3,j)
     if(list2(ilon_1,ivert)/=0.or.list2(ilon_2,ivert)/=0) then
        if(mype==0) write(0,*)' problem in cdiff_sd2ns1'
        call mpi_finalize(i)
        stop
     end if
     list2(ilon_1,ivert)=j
     list2(ilon_2,ivert)=j
  end do
  do ivert=1,nlev
     do ilon=1,nlon
        if(list2(ilon,ivert)==0) then
           if(mype==0) write(0,*)' problem in cdiff_sd2ns1'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend_sd2ns=0
  nlatloc=lat2-2
  do ivert=1,nlev
     do i=2,lon2-1
        ilon=i+jstart(mm1)-2
        j=list2(ilon,ivert)
        ipe=list_sd2ns(4,j)
        nsend_sd2ns(ipe)=nsend_sd2ns(ipe)+nlatloc
     end do
  end do

  ndsend_sd2ns(1)=0
  do i=2,npe+1
     ndsend_sd2ns(i)=ndsend_sd2ns(i-1)+nsend_sd2ns(i-1)
  end do
  nallsend_sd2ns=ndsend_sd2ns(npe+1)
  allocate(info_send_sd2ns(4,nallsend_sd2ns))
  nsend_sd2ns=0
  do ivert=1,nlev
     do i=2,lon2-1
        ilon=i+jstart(mm1)-2
        ilonm=list2(ilon,ivert)
        ilon_1=list_sd2ns(1,ilonm)
        ilon_2=list_sd2ns(2,ilonm)
        i12=0
        if(ilon_1==ilon) i12=1
        if(ilon_2==ilon) i12=2
        isig =list_sd2ns(3,ilonm)
        ipe=list_sd2ns(4,ilonm)
        do ii=2,lat2-1
           ilat=ii+istart(mm1)-2
           nsend_sd2ns(ipe)=nsend_sd2ns(ipe)+1
           ii0=ndsend_sd2ns(ipe)+nsend_sd2ns(ipe)
           info_send_sd2ns(1,ii0)=ilat
           info_send_sd2ns(2,ii0)=ilonm
           info_send_sd2ns(3,ii0)=i12
           info_send_sd2ns(4,ii0)=isig
        end do
     end do
  end do

  call mpi_alltoall(nsend_sd2ns,1,mpi_integer4,nrecv_sd2ns,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ns(1)=0
  do i=2,npe+1
     ndrecv_sd2ns(i)=ndrecv_sd2ns(i-1)+nrecv_sd2ns(i-1)
  end do
  nallrecv_sd2ns=ndrecv_sd2ns(npe+1)
  allocate(info_recv_sd2ns(4,nallrecv_sd2ns))
  call mpi_type_contiguous(4,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ns,nsend_sd2ns,ndsend_sd2ns,mpi_string1, &
                     info_recv_sd2ns,nrecv_sd2ns,ndrecv_sd2ns,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_sd2ns1

subroutine cdiff_ns2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ns2sd1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lon strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,jstart,istart,ilat1,jlon1
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,i1,i2,ilat,ilon_1,ilon_2,ivert,j,k,mm1,ipe,mpi_string1,nn
  integer(i_kind) iloop

  allocate(nsend_ns2sd(npe),nrecv_ns2sd(npe),ndsend_ns2sd(npe+1),ndrecv_ns2sd(npe+1))
  mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
     nn=0
     do k=nlon_0,nlon_1
        ilon_1=list_sd2ns(1,k)
        ilon_2=list_sd2ns(2,k)
        ivert=list_sd2ns(3,k)
        i1=ilon_1-jstart(ipe)+2
        do iloop=-1,1
           if(i1+iloop*nlon>=1.and.i1+iloop*nlon<=jlon1(ipe)+2) then
              do j=1,ilat1(ipe)+2
                 ilat=j+istart(ipe)-2
                 if(ilat<1.or.ilat>nlat) cycle
                 nn=nn+1
              end do
           end if
        end do
        i2=ilon_2-jstart(ipe)+2
        do iloop=-1,1
           if(i2+iloop*nlon>=1.and.i2+iloop*nlon<=jlon1(ipe)+2) then
              do j=1,ilat1(ipe)+2
                 ilat=j+istart(ipe)-2
                 if(ilat<1.or.ilat>nlat) cycle
                 nn=nn+1
              end do
           end if
        end do
     end do
     nsend_ns2sd(ipe)=nn
  end do

  ndsend_ns2sd(1)=0
  do i=2,npe+1
     ndsend_ns2sd(i)=ndsend_ns2sd(i-1)+nsend_ns2sd(i-1)
  end do
  nallsend_ns2sd=ndsend_ns2sd(npe+1)
  allocate(info_send_ns2sd(4,nallsend_ns2sd))
  nn=0
  do ipe=1,npe
     do k=nlon_0,nlon_1
        ilon_1=list_sd2ns(1,k)
        ilon_2=list_sd2ns(2,k)
        ivert=list_sd2ns(3,k)
        i1=ilon_1-jstart(ipe)+2
        do iloop=-1,1
           if(i1+iloop*nlon>=1.and.i1+iloop*nlon<=jlon1(ipe)+2) then
              do j=1,ilat1(ipe)+2
                 ilat=j+istart(ipe)-2
                 if(ilat<1.or.ilat>nlat) cycle
                 nn=nn+1
                 info_send_ns2sd(1,nn)=ilat
                 info_send_ns2sd(2,nn)=i1+iloop*nlon
                 info_send_ns2sd(3,nn)=k
                 info_send_ns2sd(4,nn)=1
              end do
           end if
        end do
        i2=ilon_2-jstart(ipe)+2
        do iloop=-1,1
           if(i2+iloop*nlon>=1.and.i2+iloop*nlon<=jlon1(ipe)+2) then
              do j=1,ilat1(ipe)+2
                 ilat=j+istart(ipe)-2
                 if(ilat<1.or.ilat>nlat) cycle
                 nn=nn+1
                 info_send_ns2sd(1,nn)=ilat
                 info_send_ns2sd(2,nn)=i2+iloop*nlon
                 info_send_ns2sd(3,nn)=k
                 info_send_ns2sd(4,nn)=2
              end do
           end if
        end do
     end do
  end do

  call mpi_alltoall(nsend_ns2sd,1,mpi_integer4,nrecv_ns2sd,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ns2sd(1)=0
  do i=2,npe+1
     ndrecv_ns2sd(i)=ndrecv_ns2sd(i-1)+nrecv_ns2sd(i-1)
  end do
  nallrecv_ns2sd=ndrecv_ns2sd(npe+1)
  allocate(info_recv_ns2sd(4,nallrecv_ns2sd))
  call mpi_type_contiguous(4,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ns2sd,nsend_ns2sd,ndsend_ns2sd,mpi_string1, &
                     info_recv_ns2sd,nrecv_ns2sd,ndrecv_ns2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_ns2sd1

subroutine cdiff_sd2ns(u_sd,u_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ns (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_sd
!
!   output argument list:
!    u_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: u_ns

  integer(i_kind) i12,ilat,ilonm,ilon,ivert,j,mm1
  real(r_kind),allocatable::sendbuf(:),recvbuf(:)

  mm1=mype+1

  allocate(sendbuf(nallsend_sd2ns))
  do j=1,nallsend_sd2ns
     ilat=info_send_sd2ns(1,j)
     ilonm=info_send_sd2ns(2,j)
     i12=info_send_sd2ns(3,j)
     ilon=list_sd2ns(i12,ilonm)
     ivert=list_sd2ns(3,ilonm)
     sendbuf(j)=u_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
  end do
  allocate(recvbuf(nallrecv_sd2ns))
  call mpi_alltoallv(sendbuf,nsend_sd2ns,ndsend_sd2ns,mpi_rtype, &
                     recvbuf,nrecv_sd2ns,ndrecv_sd2ns,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ns
     ilat=info_recv_sd2ns(1,j)
     ilonm=info_recv_sd2ns(2,j)
     i12=info_recv_sd2ns(3,j)
     u_ns(i12,ilat,ilonm)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ns

subroutine cdiff_sd2ns2(u1_sd,u2_sd,u1_ns,u2_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ns (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ns,u2_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: u1_ns,u2_ns

  integer(i_kind) i12,ilat,ilonm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+1

  allocate(sendbuf(2,nallsend_sd2ns))
  do j=1,nallsend_sd2ns
     ilat=info_send_sd2ns(1,j)
     ilonm=info_send_sd2ns(2,j)
     i12=info_send_sd2ns(3,j)
     ilon=list_sd2ns(i12,ilonm)
     ivert=list_sd2ns(3,ilonm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,ivert)
  end do
  allocate(recvbuf(2,nallrecv_sd2ns))
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ns,ndsend_sd2ns,mpi_string1, &
                     recvbuf,nrecv_sd2ns,ndrecv_sd2ns,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ns
     ilat=info_recv_sd2ns(1,j)
     ilonm=info_recv_sd2ns(2,j)
     i12=info_recv_sd2ns(3,j)
     u1_ns(i12,ilat,ilonm)=recvbuf(1,j)
     u2_ns(i12,ilat,ilonm)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ns2

subroutine cdiff_ns2sd(u_sd,u_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ns2sd
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_ns
!
!   output argument list:
!    u_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: u_ns

  real(r_kind),allocatable::sendbuf(:),recvbuf(:)
  integer(i_kind) i12,ilat,ivert,j,k,mm1,ilonm,ilonloc

  mm1=mype+1

  allocate(sendbuf(nallsend_ns2sd))
  do j=1,nallsend_ns2sd
     ilat=info_send_ns2sd(1,j)
     ilonm=info_send_ns2sd(3,j)
     i12=info_send_ns2sd(4,j)
     sendbuf(j)=u_ns(i12,ilat,ilonm)
  end do
  allocate(recvbuf(nallrecv_ns2sd))
  call mpi_alltoallv(sendbuf,nsend_ns2sd,ndsend_ns2sd,mpi_rtype, &
                     recvbuf,nrecv_ns2sd,ndrecv_ns2sd,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ns2sd
     ilat=info_recv_ns2sd(1,j)
     ilonloc=info_recv_ns2sd(2,j)
     ilonm=info_recv_ns2sd(3,j)
     ivert=list_sd2ns(3,ilonm)
     u_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(j)
  end do
  deallocate(recvbuf)

!-----------------do repeat rows for north/south pole
  if(nlat+1-istart(mm1)+2==lat2) then
     do k=1,nlev
        do j=1,lon2
           u_sd(lat2,j,k)=u_sd(lat2-1,j,k)
        end do
     end do
  end if
  if(2-istart(mm1)==1) then
     do k=1,nlev
        do j=1,lon2
           u_sd(1,j,k)=u_sd(2,j,k)
        end do
     end do
  end if

end subroutine cdiff_ns2sd

subroutine cdiff_ns2sd2(u1_sd,u2_sd,u1_ns,u2_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mp_compact_diffs1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_ns,u2_ns
!
!   output argument list:
!    u1_sd,u2_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: u1_ns,u2_ns

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,k,mm1,ilonm,ilonloc,mpi_string1

  mm1=mype+1

  allocate(sendbuf(2,nallsend_ns2sd))
  do j=1,nallsend_ns2sd
     ilat=info_send_ns2sd(1,j)
     ilonm=info_send_ns2sd(3,j)
     i12=info_send_ns2sd(4,j)
     sendbuf(1,j)=u1_ns(i12,ilat,ilonm)
     sendbuf(2,j)=u2_ns(i12,ilat,ilonm)
  end do
  allocate(recvbuf(2,nallrecv_ns2sd))
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ns2sd,ndsend_ns2sd,mpi_string1, &
                     recvbuf,nrecv_ns2sd,ndrecv_ns2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ns2sd
     ilat=info_recv_ns2sd(1,j)
     ilonloc=info_recv_ns2sd(2,j)
     ilonm=info_recv_ns2sd(3,j)
     ivert=list_sd2ns(3,ilonm)
     u1_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2,ilonloc,ivert)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

!-----------------do repeat rows for north/south pole
  if(nlat+1-istart(mm1)+2==lat2) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(lat2,j,k)=u1_sd(lat2-1,j,k)
           u2_sd(lat2,j,k)=u2_sd(lat2-1,j,k)
        end do
     end do
  end if
  if(2-istart(mm1)==1) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(1,j,k)=u1_sd(2,j,k)
           u2_sd(1,j,k)=u2_sd(2,j,k)
        end do
     end do
  end if

end subroutine cdiff_ns2sd2

subroutine mp_compact_dlon(b,dbdx,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!
!   output argument list:
!    dbdx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: b
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: dbdx

  integer(i_kind) ny,nxh,nbp,nya,nxa
  integer(i_kind) lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1,lacoy2,lbcoy2,lcy
  integer(i_kind) ix,iy,k,i12,ilat
  real(r_kind),dimension(nlon):: work3,grid3,grid3pol
  real(r_kind) polu,polv

  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1

!  outer loop over lat strips
  do k=nlat_0,nlat_1
     do i12=1,2
        ilat=list_sd2ew(i12,k)
        iy=ilat-1
        if(iy>=1.and.iy<=ny) then

! Initialize output arrays to zero
           do ix=1,nlon
              dbdx(i12,ix,k)=zero
           end do

! Transfer scaler input field to work array.
! Zero other work arrays.
           do ix=1,nlon
              work3(ix)=b(i12,ix,k)
              grid3(ix)=zero
           end do

! Compute x (east-west) derivatives on sphere
           call mp_xdcirdp(work3,grid3,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2), &
                        nlon,noq,nxh)

! Make corrections for convergence of meridians:
           do ix=1,nlon
              grid3(ix)=grid3(ix)*coef(lcy+iy)
           end do

           if(iy==1.or.iy==ny) then
              if(.not.vector) then
                 polu=zero
                 polv=zero
                 do ix=1,nlon
                    polu=polu+grid3(ix)*coslon(ix)
                    polv=polv+grid3(ix)*sinlon(ix)
                 end do
                 polu=polu/float(nlon)
                 polv=polv/float(nlon)
                 do ix=1,nlon
                    grid3pol(ix)=polu*coslon(ix)+polv*sinlon(ix)
                 end do
              else
                 do ix=1,nlon
                    grid3pol(ix)=zero
                 end do
              end if
           end if

! Load result into output array
           do ix=1,nlon
              dbdx(i12,ix,k)=grid3(ix)
           end do

! Load pole row if we are adjacent to pole
           if(iy==1) then
              do ix=1,nlon
                 dbdx(1,ix,k)=grid3pol(ix)
              end do
           else if(iy==ny) then
              do ix=1,nlon
                 dbdx(2,ix,k)=grid3pol(ix)
              end do
           end if

        end if

     end do
  end do

end subroutine mp_compact_dlon

subroutine mp_compact_dlon_ad(b,dbdx,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!    dbdx
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: b
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: dbdx

  integer(i_kind) ny,nxh,nbp,nya,nxa
  integer(i_kind) lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1,lacoy2,lbcoy2,lcy
  integer(i_kind) ix,iy,k,i12,ilat
  real(r_kind),dimension(nlon):: work3,grid3,grid3pol
  real(r_kind) polu,polv

  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1


!  outer loop over lat strips
  do k=nlat_0,nlat_1
     do i12=1,2
        ilat=list_sd2ew(i12,k)
        iy=ilat-1
        if(iy>=1.and.iy<=ny) then

! adjoint of Load pole row if we are adjacent to pole
           if(iy==1) then
              do ix=1,nlon
                 grid3pol(ix)=dbdx(1,ix,k)
              end do
           else if(iy==ny) then
              do ix=1,nlon
                 grid3pol(ix)=dbdx(2,ix,k)
              end do
           end if
 
! adjoint of Load result into output array
           do ix=1,nlon
              grid3(ix)=dbdx(i12,ix,k)
           end do
 
           if(iy==1.or.iy==ny) then
              if(.not.vector) then
                 polu=zero
                 polv=zero
                 do ix=1,nlon
                    polu=polu+grid3pol(ix)*coslon(ix)
                    polv=polv+grid3pol(ix)*sinlon(ix)
                 end do
                 polu=polu/float(nlon)
                 polv=polv/float(nlon)
                 do ix=1,nlon
                    grid3(ix)=grid3(ix)+polu*coslon(ix)+polv*sinlon(ix)
                 end do
              else
                 do ix=1,nlon
                    grid3pol(ix)=zero
                 end do
              end if
           end if

! adjoint Make corrections for convergence of meridians:
           do ix=1,nlon
              grid3(ix)=grid3(ix)*coef(lcy+iy)
           end do

! adjoint Compute x (east-west) derivatives on sphere
           call mp_xdcirdp(grid3,work3,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2), &
                        nlon,noq,nxh)

! Transfer scaler input field to work array.
! Zero other work arrays.
           do ix=1,nlon
!             NOTE:  Adjoint of first derivative is its negative
              b(i12,ix,k)=b(i12,ix,k)-work3(ix)
           end do
        end if

     end do
  end do

end subroutine mp_compact_dlon_ad

subroutine mp_compact_dlat(b,dbdy,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!
!   output argument list:
!    dbdy
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,half
  use gridmod, only: nlon,nlat
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) ::  b
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: dbdy

  integer(i_kind) ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer(i_kind) lbcoy2,lacoy2,iy,i,lcy,k
  real(r_kind),dimension(2,nlat-2):: work2,grid4
  real(r_kind) grid4n,grid4s


! Set parameters for calls to subsequent routines
  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp
  
  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1

!  outer loop over lon strips
  do k=nlon_0,nlon_1

! Initialize output arrays to zero
     do i=1,nlat
        dbdy(1,i,k)=zero
        dbdy(2,i,k)=zero
     end do

! Transfer scalar input field to work array.
! Zero other work arrays.
     do i=1,ny
        work2(1,i) = b(1,i+1,k)
        work2(2,i) = b(2,i+1,k)
        grid4(1,i)=zero
        grid4(2,i)=zero
     end do

     if(vector) then
!    multiply by cos(lat) ( 1/coef(lcy+iy) )
        do iy=1,ny
           work2(1,iy)=work2(1,iy)/coef(lcy+iy)
           work2(2,iy)=work2(2,iy)/coef(lcy+iy)
        enddo
     end if

! Compute y (south-north) derivatives on sphere
     call mp_ydsphdp(work2,grid4, &
          coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),ny,noq)

     if(vector) then
!  divide by cos(lat)
        do iy=1,ny
           grid4(1,iy)=grid4(1,iy)*coef(lcy+iy)
           grid4(2,iy)=grid4(2,iy)*coef(lcy+iy)
        enddo
        grid4n= zero
        grid4s= zero
     else
        grid4n=half*(grid4(1,ny)-grid4(2,ny))
        grid4s=half*(grid4(1, 1)-grid4(2, 1))
     end if

! Load result into output array
     dbdy(1,1,k)=grid4s
     dbdy(2,1,k)=-grid4s
     dbdy(1,nlat,k)=grid4n
     dbdy(2,nlat,k)=-grid4n
     do i=1,ny
        dbdy(1,i+1,k) = grid4(1,i)
        dbdy(2,i+1,k) = grid4(2,i)
     end do
  
  end do

end subroutine mp_compact_dlat

subroutine mp_compact_dlat_ad(b,dbdy,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!    dbdy
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,half
  use gridmod, only: nlon,nlat
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(inout) :: b
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: dbdy

  integer(i_kind) ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer(i_kind) lbcoy2,lacoy2,iy,i,lcy,k
  real(r_kind),dimension(2,nlat-2):: work2,grid4
  real(r_kind) grid4n,grid4s


! Set parameters for calls to subsequent routines
  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp
  
  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1

!  outer loop over lon strips
  do k=nlon_0,nlon_1

! adjoint Load result into output array
     do i=1,ny
        grid4(1,i) = dbdy(1,i+1,k)
        grid4(2,i) = dbdy(2,i+1,k)
     end do
   ! grid4s=dbdy(1,1,k)-dbdy(2,1,k)
   ! grid4n=dbdy(1,nlat,k)-dbdy(2,nlat,k)
     grid4s=-dbdy(1,1,k)+dbdy(2,1,k)
     grid4n=-dbdy(1,nlat,k)+dbdy(2,nlat,k)

     if(vector) then
!  divide by cos(lat)
        grid4n= zero
        grid4s= zero
        do iy=1,ny
           grid4(1,iy)=grid4(1,iy)*coef(lcy+iy)
           grid4(2,iy)=grid4(2,iy)*coef(lcy+iy)
        enddo
     else
   !    grid4(1,ny)=grid4(1,ny)+half*grid4n
   !    grid4(2,ny)=grid4(2,ny)-half*grid4n
   !    grid4(1, 1)=grid4(1, 1)+half*grid4s
   !    grid4(2, 1)=grid4(2, 1)-half*grid4s
        grid4(1,ny)=grid4(1,ny)-half*grid4n
        grid4(2,ny)=grid4(2,ny)+half*grid4n
        grid4(1, 1)=grid4(1, 1)-half*grid4s
        grid4(2, 1)=grid4(2, 1)+half*grid4s
     end if

! adjoint Compute y (south-north) derivatives on sphere
     work2=zero
     call mp_tydsphdp(work2,grid4, &
          coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),ny,noq)

     if(vector) then
!    multiply by cos(lat) ( 1/coef(lcy+iy) )
        do iy=1,ny
           work2(1,iy)=work2(1,iy)/coef(lcy+iy)
           work2(2,iy)=work2(2,iy)/coef(lcy+iy)
        enddo
     end if

! accumulate to output field
     do i=1,ny
        b(1,i+1,k) = b(1,i+1,k) - work2(1,i)
        b(2,i+1,k) = b(2,i+1,k) - work2(2,i)
     end do

  end do

end subroutine mp_compact_dlat_ad

subroutine mp_uv_pole(u,v)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_uv_pole
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    u,v
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  implicit none

  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: u,v

  integer(i_kind) ilat1,ilat2,ix,k
  real(r_kind) polnu,polnv,polsu,polsv


  do k=nlat_0,nlat_1
     ilat1=list_sd2ew(1,k)
     ilat2=list_sd2ew(2,k)
     if(ilat1==1) then

!       do south pole
        polsu=zero
        polsv=zero
        do ix=1,nlon
           polsu=polsu+u(2,ix,k)*coslon(ix)+v(2,ix,k)*sinlon(ix)
           polsv=polsv+u(2,ix,k)*sinlon(ix)-v(2,ix,k)*coslon(ix)
        end do
        polsu=polsu/float(nlon)
        polsv=polsv/float(nlon)
        do ix=1,nlon
           u(1,ix,k)=polsu*coslon(ix)+polsv*sinlon(ix)
           v(1,ix,k)=polsu*sinlon(ix)-polsv*coslon(ix)
        end do

     else if(ilat2==nlat) then

!       do north pole
        polnu=zero
        polnv=zero
        do ix=1,nlon
           polnu=polnu+u(1,ix,k)*coslon(ix)-v(1,ix,k)*sinlon(ix)
           polnv=polnv+u(1,ix,k)*sinlon(ix)+v(1,ix,k)*coslon(ix)
        end do
        polnu=polnu/float(nlon)
        polnv=polnv/float(nlon)
        do ix=1,nlon
           u(2,ix,k)= polnu*coslon(ix)+polnv*sinlon(ix)
           v(2,ix,k)=-polnu*sinlon(ix)+polnv*coslon(ix)
        end do

     else
        cycle
     end if

  end do

end subroutine mp_uv_pole

subroutine mp_uv_pole_ad(u,v)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_uv_pole_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    u,v
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  implicit none

  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: u,v

  integer(i_kind) ilat1,ilat2,ix,k
  real(r_kind) polnu,polnv,polsu,polsv


  do k=nlat_0,nlat_1
     ilat1=list_sd2ew(1,k)
     ilat2=list_sd2ew(2,k)
     if(ilat1==1) then
 
!       do south pole
        polsu=zero
        polsv=zero
        do ix=1,nlon
           polsu=polsu+u(1,ix,k)*coslon(ix)+v(1,ix,k)*sinlon(ix)
           polsv=polsv+u(1,ix,k)*sinlon(ix)-v(1,ix,k)*coslon(ix)
           u(1,ix,k)=zero
           v(1,ix,k)=zero
        end do
        polsu=polsu/float(nlon)
        polsv=polsv/float(nlon)
        do ix=1,nlon
           u(2,ix,k)=u(2,ix,k)+polsu*coslon(ix)+polsv*sinlon(ix)
           v(2,ix,k)=v(2,ix,k)+polsu*sinlon(ix)-polsv*coslon(ix)
        end do

     else if(ilat2==nlat) then

!       do north pole
        polnu=zero
        polnv=zero
        do ix=1,nlon
           polnu=polnu+u(2,ix,k)*coslon(ix)-v(2,ix,k)*sinlon(ix)
           polnv=polnv+u(2,ix,k)*sinlon(ix)+v(2,ix,k)*coslon(ix)
           u(2,ix,k)=zero
           v(2,ix,k)=zero
        end do
        polnu=polnu/float(nlon)
        polnv=polnv/float(nlon)
        do ix=1,nlon
           u(1,ix,k)=u(1,ix,k)+polnu*coslon(ix)+polnv*sinlon(ix)
           v(1,ix,k)=v(1,ix,k)-polnu*sinlon(ix)+polnv*coslon(ix)
        end do

     else
        cycle
     end if

  end do

end subroutine mp_uv_pole_ad

end module mp_compact_diffs_mod1

