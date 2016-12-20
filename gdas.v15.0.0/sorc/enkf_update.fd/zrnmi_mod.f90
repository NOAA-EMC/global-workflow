module zrnmi_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    zrnmi_mod
!
! abstract: Contains all routines necessary for regional normal mode
!             projection/initialization.  These tools are used to 
!             implement weak and strong dynamic constraints.
!
!             The normal modes are based on double sine series
!             expansions, following briere, ??add ref here.
!
!             In this version, fields are first multiplied by a boundary 
!             function which smoothly reduces fields to zero at first point outside domain.
!
! program history log:
!   2006-11-03  parrish
!   2008-03-26  safford -- rm unused vars, add subroutine doc blocks
!   2011-07-04  todling - fixes to run either single or double precision
!
! subroutines included:
!   sub zrnmi_initialize       --- 
!   sub zrnmi_sd2x0            --- setup to convert from subdomains to x direction
!   sub zrnmi_sd2x1            --- additional setup
!   sub zrnmi_x2sd1            --- additional setup
!   sub zrnmi_sd2x2            --- subdomains to x
!   sub zrnmi_sd2x3            --- subdomains to x
!   sub zrnmi_x2sd             --- x to subdomains
!   sub zrnmi_x2sd2            --- x to subdomains
!   sub zrnmi_x2sd3            --- x to subdomains
!
!    following required for finite difference derivatives and laplacians
!   sub zrnmi_sd2y0            --- setup to convert from subdomains to y direction
!   sub zrnmi_sd2y1            --- additional setup
!   sub zrnmi_y2sd1            --- additional setup
!   sub zrnmi_sd2y2            --- subdomains to y
!   sub zrnmi_y2sd2            --- y to subdomains
!
!   sub zrnmi_x_strans0         - initialize constants for x transforms
!   sub zrnmi_x_strans          - sin trans in x direction from grid to spectral
!
!   sub zrnmi_uvm2dzmhat
!   sub zrnmi_uvm2dzmhat_ad
!   sub zrnmi_pcmhat2uvm
!   sub zrnmi_pcmhat2uvm_orig
!   sub zrnmi_pcmhat2uvm_ad_orig
!   sub zrnmi_pcmhat2uvm_ad
!
!   sub zrnmi_x2y0             - setup to convert from x to y
!   sub zrnmi_x2y1             - additional setup for x to y
!   sub zrnmi_x2y3             - x to y
!   sub zrnmi_y2x3             - y to x
!
!   sub zrnmi_y_strans0         - initialize constants for y transforms
!   sub zrnmi_y_strans          - almost copy of zrnmi_x_strans
!
!   sub zrnmi_delx_general
! * sub zrnmi_delx              - equivalent(*) to delx_reg in psichi2uv_reg.f90, but more scalable
! * sub zrnmi_delx_ad           - equivalent(*) to tdelx_reg in psichi2uvt_reg.f90, but more scalable
!   sub zrnmi_dely_general
! * sub zrnmi_dely              - equivalent(*) to dely_reg in psichi2uv_reg.f90, but more scalable
! * sub zrnmi_dely_ad           - equivalent(*) to tdely_reg in psichi2uvt_reg.f90, but more scalable
!                                    (*) :  except corner points are treated differently.
!
!   sub zrnmi_uv2dz
!   sub zrnmi_uv2dz_ad
!   sub zrnmi_pc2uv_orig
!   sub zrnmi_pc2uv_ad_orig
!   sub zrnmi_constants
!   sub zrnmi_strong_bal_correction
!   sub zrnmi_strong_bal_correction_ad
!   sub zrnmi_filter_uvm
!   sub zrnmi_filter_uvm_ad
!   sub zrnmi_filter_uvm2
!   sub zrnmi_filter_uvm2_ad
!   sub zrnmi_uvm2uvmhat
!   sub zrnmi_uvm2uvmhat_ad
!   sub zrnmi_uvmhat2uvm
!   sub zrnmi_uvmhat2uvm_ad
!
! Variable Definitions:
!      
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: pi
  implicit none

! set default to private
  private
! set subroutines to public
  public :: zrnmi_initialize
  public :: zrnmi_sd2x0
  public :: zrnmi_sd2x1
  public :: zrnmi_x2sd1
  public :: zrnmi_sd2x2
  public :: zrnmi_sd2x3
  public :: zrnmi_x2sd
  public :: zrnmi_x2sd2
  public :: zrnmi_x2sd3
  public :: zrnmi_sd2y0
  public :: zrnmi_sd2y1
  public :: zrnmi_y2sd1
  public :: zrnmi_sd2y2
  public :: zrnmi_y2sd2
  public :: zrnmi_x_strans0
  public :: zrnmi_x_strans
  public :: zrnmi_uvm2dzmhat
  public :: zrnmi_uvm2dzmhat_ad
  public :: zrnmi_pcmhat2uvm
  public :: zrnmi_pcmhat2uvm_orig
  public :: zrnmi_pcmhat2uvm_ad_orig
  public :: zrnmi_pcmhat2uvm_ad
  public :: zrnmi_x2y0
  public :: zrnmi_x2y1
  public :: zrnmi_x2y3
  public :: zrnmi_y2x3
  public :: zrnmi_y_strans0
  public :: zrnmi_y_strans
  public :: zrnmi_delx_general
  public :: zrnmi_delx
  public :: zrnmi_delx_ad
  public :: zrnmi_dely_general
  public :: zrnmi_dely
  public :: zrnmi_dely_ad
  public :: zrnmi_uv2dz
  public :: zrnmi_uv2dz_ad
  public :: zrnmi_pc2uv_orig
  public :: zrnmi_pc2uv_ad_orig
  public :: zrnmi_constants
  public :: zrnmi_strong_bal_correction
  public :: zrnmi_strong_bal_correction_ad
  public :: zrnmi_filter_uvm
  public :: zrnmi_filter_uvm_ad
  public :: zrnmi_filter_uvm2
  public :: zrnmi_filter_uvm2_ad
  public :: zrnmi_uvm2uvmhat
  public :: zrnmi_uvm2uvmhat_ad
  public :: zrnmi_uvmhat2uvm
  public :: zrnmi_uvmhat2uvm_ad

  integer(i_kind) nx,ny
  integer(i_kind) nvert

!  communication info for sd <--> x strips
  integer(i_kind) ny_0,ny_1
  integer(i_kind),allocatable::list_sd2x(:,:)   ! list_sd2x(1,j) = y index for x strip j
                                                 ! list_sd2x(2,j) = vert index for x strip j
                                                 ! list_sd2x(3,j) = pe of this y/vert index strip
  integer(i_kind),dimension(:),allocatable::nsend_sd2x,nrecv_sd2x,ndsend_sd2x,ndrecv_sd2x
  integer(i_kind) nallsend_sd2x,nallrecv_sd2x
  integer(i_kind),allocatable::  info_send_sd2x(:,:),info_recv_sd2x(:,:)
  integer(i_kind),dimension(:),allocatable::nsend_x2sd,nrecv_x2sd,ndsend_x2sd,ndrecv_x2sd
  integer(i_kind) nallsend_x2sd,nallrecv_x2sd
  integer(i_kind),allocatable::  info_send_x2sd(:,:),info_recv_x2sd(:,:)

!  communication info for sd <--> y strips
  integer(i_kind) nx_0,nx_1
  integer(i_kind),allocatable::list_sd2y(:,:)   ! list_sd2y(1,j) = x index for y strip j
                                                 ! list_sd2y(2,j) = vert index for y strip j
                                                 ! list_sd2y(3,j) = pe of this x/vert index strip
  integer(i_kind),dimension(:),allocatable::nsend_sd2y,nrecv_sd2y,ndsend_sd2y,ndrecv_sd2y
  integer(i_kind) nallsend_sd2y,nallrecv_sd2y
  integer(i_kind),allocatable::  info_send_sd2y(:,:),info_recv_sd2y(:,:)
  integer(i_kind),dimension(:),allocatable::nsend_y2sd,nrecv_y2sd,ndsend_y2sd,ndrecv_y2sd
  integer(i_kind) nallsend_y2sd,nallrecv_y2sd
  integer(i_kind),allocatable::  info_send_y2sd(:,:),info_recv_y2sd(:,:)

!   x sine transform info
  real(r_kind),allocatable:: sinx(:,:)

!  communication info for x <--> y strips
  integer(i_kind) mx_0,mx_1
  integer(i_kind),allocatable::list_x2y(:,:)   ! list_x2y(1,j) = x index for y strip j
                                               ! list_x2y(2,j) = vert index for y strip j
                                               ! list_x2y(3,j) = pe of this x/vert index strip
  integer(i_kind),dimension(:),allocatable::nsend_x2y,nrecv_x2y,ndsend_x2y,ndrecv_x2y
  integer(i_kind) nallsend_x2y,nallrecv_x2y
  integer(i_kind),allocatable::  info_send_x2y(:,:),info_recv_x2y(:,:)

!   y sine transform info
  real(r_kind),allocatable:: siny(:,:)

!  constants for gravity projection operators
  real(r_kind),allocatable,dimension(:,:):: am2,f_sm2_am2,sm2,a2_p0_sm2,f_p0_sm2,p0_sm2
  real(r_kind) fbar
  real(r_kind),allocatable,dimension(:,:):: pmask
  real(r_kind) zrnmi_period_max,zrnmi_period_width

contains


  subroutine zrnmi_initialize(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_initialize
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use mod_strong, only: period_max,period_width
    use mod_vtrans, only: nvmodes_keep
    implicit none

    integer(i_kind),intent(in   ) :: mype

    zrnmi_period_max=period_max
    zrnmi_period_width=period_width
    nvert=nvmodes_keep
    call zrnmi_sd2x0(mype)
    call zrnmi_sd2x1(mype)
    call zrnmi_x2sd1(mype)
    call zrnmi_sd2y0(mype)
    call zrnmi_sd2y1(mype)
    call zrnmi_y2sd1(mype)
    call zrnmi_x_strans0
    call zrnmi_x2y0(mype)
    call zrnmi_x2y1(mype)
    call zrnmi_y_strans0
    call zrnmi_constants(mype)

  end subroutine zrnmi_initialize

  subroutine zrnmi_sd2x0(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2x0
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  create x (y strips) subdivision for use with sine transform in x direction

!  output:

!     ny_0,ny_1:  range of y/vert index on processor mype

!           1 <= ny_0 <= ny_1 <= ny*nvert
!           if npe > ny*nvert, then will have ny_0 = -1, ny_1 = -2 on some processors,
!               and ny_0=ny_1 on the remaining ny*nvert processors
!
!     list_sd2x(3,ny*nvert):  global definition of contents of each y/vert strip
!                      list_sd2x(1,j) = y index for x strip j
!                      list_sd2x(2,j) = vert level for x strip j
!                      list_sd2x(3,j) = pe of this y/vert strip

  use gridmod, only: nlon,nlat
  use mpimod, only:  npe
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,k,kchk,kk,n,nn,ny_this,ny_tot

!  set analysis grid dimensions
  nx=nlon
  ny=nlat

  allocate(list_sd2x(3,ny*nvert))

  ny_tot=ny*nvert
  ny_this=ny_tot/npe
  if(mod(ny_tot,npe)/=0) ny_this=ny_this+1
  if(mod(ny_tot,npe)==0) then
     kchk=npe
  else
     kchk=mod(ny_tot,npe)
  end if

  nn=0
  do k=1,nvert
     do i=1,ny
        nn=nn+1
        list_sd2x(1,nn)=i
        list_sd2x(2,nn)=k
        list_sd2x(3,nn)=-1
     end do
  end do

  if(mype == 0) write(6,*)' nn,ny_tot,ny,nvert=',nn,ny_tot,ny,nvert

  ny_0=-1
  ny_1=-2
  nn=0
  do n=1,npe
     if(n <= kchk) then
        kk=ny_this
     else
        kk=ny_this-1
     end if
     if(kk > 0) then
        if(mype+1 == n) then
           ny_0=nn+1
           ny_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           list_sd2x(3,nn)=n
        end do
     end if
  end do

  end subroutine zrnmi_sd2x0

  subroutine zrnmi_sd2x1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2x1
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  continue with setup for subdomain to lat strip interchanges

    use gridmod, only: lon2,lat2,jstart,istart
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) list2(ny,nvert)
    integer(i_kind) i,ii,ii0,iy,ivert,j,mm1,nn,nxloc,ipe,iym,ix,mpi_string1
    integer(i_kind) ny_tot

    allocate(nsend_sd2x(npe),nrecv_sd2x(npe),ndsend_sd2x(npe+1),ndrecv_sd2x(npe+1))
    mm1=mype+1
    ny_tot=ny*nvert


    nn=0
    list2=0
    do j=1,ny_tot
       iy=list_sd2x(1,j)
       ivert=list_sd2x(2,j)
       if(list2(iy,ivert) /= 0) then
          if(mype == 0) write(0,*)' problem in zrnmi_sd2x1'
          call mpi_finalize(i)
          stop
       end if
       list2(iy,ivert)=j
    end do

!  obtain counts of points to send to each pe from this pe

    nsend_sd2x=0
    nxloc=lon2-2
    do ivert=1,nvert
       do i=2,lat2-1
          iy=i+istart(mm1)-2
          j=list2(iy,ivert)
          ipe=list_sd2x(3,j)
          nsend_sd2x(ipe)=nsend_sd2x(ipe)+nxloc
       end do
    end do

    ndsend_sd2x(1)=0
    do i=2,npe+1
       ndsend_sd2x(i)=ndsend_sd2x(i-1)+nsend_sd2x(i-1)
    end do
    nallsend_sd2x=ndsend_sd2x(npe+1)
    allocate(info_send_sd2x(3,nallsend_sd2x))
    nsend_sd2x=0
    do ivert=1,nvert
       do i=2,lat2-1
          iy=i+istart(mm1)-2
          iym=list2(iy,ivert)
          ipe=list_sd2x(3,iym)
          do ii=2,lon2-1
             ix=ii+jstart(mm1)-2
             nsend_sd2x(ipe)=nsend_sd2x(ipe)+1
             ii0=ndsend_sd2x(ipe)+nsend_sd2x(ipe)
             info_send_sd2x(1,ii0)=ix
             info_send_sd2x(2,ii0)=iym
             info_send_sd2x(3,ii0)=ivert
          end do
       end do
    end do

    call mpi_alltoall(nsend_sd2x,1,mpi_integer4,nrecv_sd2x,1,mpi_integer4,mpi_comm_world,ierror)
    ndrecv_sd2x(1)=0
    do i=2,npe+1
       ndrecv_sd2x(i)=ndrecv_sd2x(i-1)+nrecv_sd2x(i-1)
    end do
    nallrecv_sd2x=ndrecv_sd2x(npe+1)
    allocate(info_recv_sd2x(3,nallrecv_sd2x))
    call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(info_send_sd2x,nsend_sd2x,ndsend_sd2x,mpi_string1, &
                     info_recv_sd2x,nrecv_sd2x,ndrecv_sd2x,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)

  end subroutine zrnmi_sd2x1

  subroutine zrnmi_x2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2sd1
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_x (y strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: jstart,istart,ilat1,jlon1
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
    implicit none


    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,iy,ivert,j,k,mm1,ipe,ix,mpi_string1,nn

    allocate(nsend_x2sd(npe),nrecv_x2sd(npe),ndsend_x2sd(npe+1),ndrecv_x2sd(npe+1))
    mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
    do ipe=1,npe
       nn=0
       do k=ny_0,ny_1
          iy=list_sd2x(1,k)
          ivert=list_sd2x(2,k)
          i=iy-istart(ipe)+2
          if(i >= 1.and.i <= ilat1(ipe)+2) then
             do j=1,jlon1(ipe)+2
                ix=j+jstart(ipe)-2
                if(ix >= 1.and.ix <= nx) nn=nn+1
             end do
          end if
       end do
       nsend_x2sd(ipe)=nn
    end do

    ndsend_x2sd(1)=0
    do i=2,npe+1
       ndsend_x2sd(i)=ndsend_x2sd(i-1)+nsend_x2sd(i-1)
    end do
    nallsend_x2sd=ndsend_x2sd(npe+1)
    allocate(info_send_x2sd(3,nallsend_x2sd))
    nn=0
    do ipe=1,npe
       do k=ny_0,ny_1
          iy=list_sd2x(1,k)
          ivert=list_sd2x(2,k)
          i=iy-istart(ipe)+2
          if(i >= 1.and.i <= ilat1(ipe)+2) then
             do j=1,jlon1(ipe)+2
                ix=j+jstart(ipe)-2
                if(ix >= 1.and.ix <= nx) then
                   nn=nn+1
                   info_send_x2sd(1,nn)=ix
                   info_send_x2sd(2,nn)=j
                   info_send_x2sd(3,nn)=k
                end if
             end do
          end if
       end do
    end do

    call mpi_alltoall(nsend_x2sd,1,mpi_integer4,nrecv_x2sd,1,mpi_integer4,mpi_comm_world,ierror)
    ndrecv_x2sd(1)=0
    do i=2,npe+1
       ndrecv_x2sd(i)=ndrecv_x2sd(i-1)+nrecv_x2sd(i-1)
    end do
    nallrecv_x2sd=ndrecv_x2sd(npe+1)
    allocate(info_recv_x2sd(3,nallrecv_x2sd))
    call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(info_send_x2sd,nsend_x2sd,ndsend_x2sd,mpi_string1, &
                       info_recv_x2sd,nrecv_x2sd,ndrecv_x2sd,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)

  end subroutine zrnmi_x2sd1

  subroutine zrnmi_sd2x2(u1_sd,u2_sd,u1_x,u2_x,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2x2
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_sd,u2_sd
!
!   output argument list:
!     u1_x,u2_x
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_sd (subdomains) to u_x (y strips)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,jstart,istart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype

    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u1_sd,u2_sd
    real(r_kind),dimension(nx,ny_0:ny_1)   ,intent(  out) :: u1_x,u2_x

    integer(i_kind) iy,iym,ix,ivert,j,mm1,mpi_string1
    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

    mm1=mype+1

    allocate(sendbuf(2,nallsend_sd2x))
    do j=1,nallsend_sd2x
       ix=info_send_sd2x(1,j)
       iym=info_send_sd2x(2,j)
       iy=list_sd2x(1,iym)
       ivert=list_sd2x(2,iym)
       sendbuf(1,j)=u1_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
       sendbuf(2,j)=u2_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
    end do
    call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(2,nallrecv_sd2x))
    call mpi_alltoallv(sendbuf,nsend_sd2x,ndsend_sd2x,mpi_string1, &
                       recvbuf,nrecv_sd2x,ndrecv_sd2x,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)

    do j=1,nallrecv_sd2x
       ix=info_recv_sd2x(1,j)
       iym=info_recv_sd2x(2,j)
       u1_x(ix,iym)=recvbuf(1,j)
       u2_x(ix,iym)=recvbuf(2,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_sd2x2

  subroutine zrnmi_sd2x3(u1_sd,u2_sd,u3_sd,u1_x,u2_x,u3_x,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2x3
!
!   prgrmmr: 
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_sd,u2_sd,u2_sd
!
!   output argument list:
!     u1_x,u2_x,u3_x
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_sd (subdomains) to u_x (y strips)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,jstart,istart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype

    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u1_sd,u2_sd,u3_sd
    real(r_kind),dimension(nx,ny_0:ny_1)   ,intent(  out) :: u1_x,u2_x,u3_x

    integer(i_kind) iy,iym,ix,ivert,j,mm1,mpi_string1
    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

    mm1=mype+1

    allocate(sendbuf(3,nallsend_sd2x))
    do j=1,nallsend_sd2x
       ix=info_send_sd2x(1,j)
       iym=info_send_sd2x(2,j)
       iy=list_sd2x(1,iym)
       ivert=list_sd2x(2,iym)
       sendbuf(1,j)=u1_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
       sendbuf(2,j)=u2_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
       sendbuf(3,j)=u3_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
    end do
    call mpi_type_contiguous(3,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(3,nallrecv_sd2x))
    call mpi_alltoallv(sendbuf,nsend_sd2x,ndsend_sd2x,mpi_string1, &
                       recvbuf,nrecv_sd2x,ndrecv_sd2x,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)

    do j=1,nallrecv_sd2x
       ix=info_recv_sd2x(1,j)
       iym=info_recv_sd2x(2,j)
       u1_x(ix,iym)=recvbuf(1,j)
       u2_x(ix,iym)=recvbuf(2,j)
       u3_x(ix,iym)=recvbuf(3,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_sd2x3

  subroutine zrnmi_x2sd(u_sd,u_x,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2sd
!
!   prgrmmr: 
!
! abstract:      move u_x (lat strips) to u_sd (subdomains)
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u_x      - lat strips
!
!   output argument list:
!     u_sd     - subdomains
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,istart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use constants, only: zero
    implicit none


    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u_sd
    real(r_kind),dimension(nx,ny_0:ny_1)   ,intent(in   ) :: u_x

    real(r_kind),allocatable::sendbuf(:),recvbuf(:)
    integer(i_kind) iy,ivert,j,mm1,iym,ix,ixloc

    mm1=mype+1

    u_sd=zero
    allocate(sendbuf(nallsend_x2sd))
    do j=1,nallsend_x2sd
       ix=info_send_x2sd(1,j)
       iym=info_send_x2sd(3,j)
       sendbuf(j)=u_x(ix,iym)
    end do
    allocate(recvbuf(nallrecv_x2sd))
    call mpi_alltoallv(sendbuf,nsend_x2sd,ndsend_x2sd,mpi_rtype, &
                       recvbuf,nrecv_x2sd,ndrecv_x2sd,mpi_rtype,mpi_comm_world,ierror)
    deallocate(sendbuf)
    do j=1,nallrecv_x2sd
       ixloc=info_recv_x2sd(2,j)
       iym=info_recv_x2sd(3,j)
       iy=list_sd2x(1,iym)
       ivert=list_sd2x(2,iym)
       u_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_x2sd

  subroutine zrnmi_x2sd2(u1_sd,u2_sd,u1_x,u2_x,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2sd2
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_x     - 
!     u2_x     - 
!
!   output argument list:
!     u1_sd    - 
!     u2_sd    - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,istart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use constants, only: zero
    implicit none


    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u1_sd,u2_sd
    real(r_kind),dimension(nx,ny_0:ny_1)   ,intent(in   ) :: u1_x,u2_x

    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
    integer(i_kind) iy,ivert,j,mm1,iym,ix,ixloc,mpi_string1

    mm1=mype+1

    u1_sd=zero
    u2_sd=zero
    allocate(sendbuf(2,nallsend_x2sd))
    do j=1,nallsend_x2sd
       ix=info_send_x2sd(1,j)
       iym=info_send_x2sd(3,j)
       sendbuf(1,j)=u1_x(ix,iym)
       sendbuf(2,j)=u2_x(ix,iym)
    end do
    call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(2,nallrecv_x2sd))
    call mpi_alltoallv(sendbuf,nsend_x2sd,ndsend_x2sd,mpi_string1, &
                       recvbuf,nrecv_x2sd,ndrecv_x2sd,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)
    do j=1,nallrecv_x2sd
       ixloc=info_recv_x2sd(2,j)
       iym=info_recv_x2sd(3,j)
       iy=list_sd2x(1,iym)
       ivert=list_sd2x(2,iym)
       u1_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(1,j)
       u2_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(2,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_x2sd2

  subroutine zrnmi_x2sd3(u1_sd,u2_sd,u3_sd,u1_x,u2_x,u3_x,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2sd3
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_x     - 
!     u2_x     - 
!     u3_x     - 
!
!   output argument list:
!     u1_sd    - 
!     u2_sd    - 
!     u3_sd    - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,istart
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
    use constants, only: zero
    implicit none


    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u1_sd,u2_sd,u3_sd
    real(r_kind),dimension(nx,ny_0:ny_1)   ,intent(in   ) :: u1_x,u2_x,u3_x

    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
    integer(i_kind) iy,ivert,j,mm1,iym,ix,ixloc, mpi_string1

    mm1=mype+1

    u1_sd=zero
    u2_sd=zero
    u3_sd=zero
    allocate(sendbuf(3,nallsend_x2sd))
    do j=1,nallsend_x2sd
       ix=info_send_x2sd(1,j)
       iym=info_send_x2sd(3,j)
       sendbuf(1,j)=u1_x(ix,iym)
       sendbuf(2,j)=u2_x(ix,iym)
       sendbuf(3,j)=u3_x(ix,iym)
    end do
    call mpi_type_contiguous(3,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(3,nallrecv_x2sd))
    call mpi_alltoallv(sendbuf,nsend_x2sd,ndsend_x2sd,mpi_string1, &
                       recvbuf,nrecv_x2sd,ndrecv_x2sd,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)
    do j=1,nallrecv_x2sd
       ixloc=info_recv_x2sd(2,j)
       iym=info_recv_x2sd(3,j)
       iy=list_sd2x(1,iym)
       ivert=list_sd2x(2,iym)
       u1_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(1,j)
       u2_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(2,j)
       u3_sd(iy-istart(mm1)+2,ixloc,ivert)=recvbuf(3,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_x2sd3

  subroutine zrnmi_sd2y0(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2y0
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  create y (x strips) subdivision for use with derivatives in the y direction

!  output:

!     nx_0,nx_1:  range of x/vert index on processor mype

!           1 <= nx_0 <= nx_1 <= nx*nvert
!           if npe > nx*nvert, then will have nx_0 = -1, nx_1 = -2 on some processors,
!               and nx_0=nx_1 on the remaining nx*nvert processors
!
!     list_sd2y(3,nx*nvert):  global definition of contents of each x/vert strip
!                      list_sd2y(1,j) = x index for y strip j
!                      list_sd2y(2,j) = vert level for y strip j
!                      list_sd2y(3,j) = pe of this x/vert strip

  use mpimod, only:  npe
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,k,kchk,kk,n,nn,nx_this,nx_tot

  allocate(list_sd2y(3,nx*nvert))

  nx_tot=nx*nvert
  nx_this=nx_tot/npe
  if(mod(nx_tot,npe)/=0) nx_this=nx_this+1
  if(mod(nx_tot,npe)==0) then
     kchk=npe
  else
     kchk=mod(nx_tot,npe)
  end if

  nn=0
  do k=1,nvert
     do i=1,nx
        nn=nn+1
        list_sd2y(1,nn)=i
        list_sd2y(2,nn)=k
        list_sd2y(3,nn)=-1
     end do
  end do

  if(mype == 0) write(6,*)' in zrnmi_sd2x0, nn,nx_tot,nx,nvert=',nn,nx_tot,nx,nvert

  nx_0=-1
  nx_1=-2
  nn=0
  do n=1,npe
     if(n <= kchk) then
        kk=nx_this
     else
        kk=nx_this-1
     end if
     if(kk >  0) then
        if(mype+1 == n) then
           nx_0=nn+1
           nx_1=nn+kk
        end if
        do k=1,kk
           nn=nn+1
           list_sd2y(3,nn)=n
        end do
     end if
  end do

  end subroutine zrnmi_sd2y0

  subroutine zrnmi_sd2y1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2y1
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  continue with setup for subdomain to x strip interchanges

    use gridmod, only: lon2,lat2,jstart,istart
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) list2(nx,nvert)
    integer(i_kind) i,ii,ii0,ix,ivert,j,mm1,nn,nyloc,ipe,ixm,iy,mpi_string1
    integer(i_kind) nx_tot

    allocate(nsend_sd2y(npe),nrecv_sd2y(npe),ndsend_sd2y(npe+1),ndrecv_sd2y(npe+1))
    mm1=mype+1
    nx_tot=nx*nvert


    nn=0
    list2=0
    do j=1,nx_tot
       ix=list_sd2y(1,j)
       ivert=list_sd2y(2,j)
       if(list2(ix,ivert) /= 0) then
          if(mype == 0) write(0,*)' problem in zrnmi_sd2y1'
          call mpi_finalize(i)
          stop
       end if
       list2(ix,ivert)=j
    end do
    do ivert=1,nvert
       do ix=1,nx
          if(list2(ix,ivert) == 0) then
             if(mype == 0) write(0,*)' problem in zrnmi_sd2y1'
             call mpi_finalize(i)
             stop
          end if
       end do
    end do

!  obtain counts of points to send to each pe from this pe

    nsend_sd2y=0
    nyloc=lat2-2
    do ivert=1,nvert
       do i=2,lon2-1
          ix=i+jstart(mm1)-2
          j=list2(ix,ivert)
          ipe=list_sd2y(3,j)
          nsend_sd2y(ipe)=nsend_sd2y(ipe)+nyloc
       end do
    end do

    ndsend_sd2y(1)=0
    do i=2,npe+1
       ndsend_sd2y(i)=ndsend_sd2y(i-1)+nsend_sd2y(i-1)
    end do
    nallsend_sd2y=ndsend_sd2y(npe+1)
    allocate(info_send_sd2y(3,nallsend_sd2y))
    nsend_sd2y=0
    do ivert=1,nvert
       do i=2,lon2-1
          ix=i+jstart(mm1)-2
          ixm=list2(ix,ivert)
          ipe=list_sd2y(3,ixm)
          do ii=2,lat2-1
             iy=ii+istart(mm1)-2
             nsend_sd2y(ipe)=nsend_sd2y(ipe)+1
             ii0=ndsend_sd2y(ipe)+nsend_sd2y(ipe)
             info_send_sd2y(1,ii0)=iy
             info_send_sd2y(2,ii0)=ixm
             info_send_sd2y(3,ii0)=ivert
          end do
       end do
    end do

    call mpi_alltoall(nsend_sd2y,1,mpi_integer4,nrecv_sd2y,1,mpi_integer4,mpi_comm_world,ierror)
    ndrecv_sd2y(1)=0
    do i=2,npe+1
       ndrecv_sd2y(i)=ndrecv_sd2y(i-1)+nrecv_sd2y(i-1)
    end do
    nallrecv_sd2y=ndrecv_sd2y(npe+1)
    allocate(info_recv_sd2y(3,nallrecv_sd2y))
    call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(info_send_sd2y,nsend_sd2y,ndsend_sd2y,mpi_string1, &
                       info_recv_sd2y,nrecv_sd2y,ndrecv_sd2y,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)

  end subroutine zrnmi_sd2y1

  subroutine zrnmi_y2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_y2sd1
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_y (x strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: jstart,istart,ilat1,jlon1
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
    implicit none


    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,ix,ivert,j,k,mm1,mpi_string1,ipe,iy,nn

    allocate(nsend_y2sd(npe),nrecv_y2sd(npe),ndsend_y2sd(npe+1),ndrecv_y2sd(npe+1))
    mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
    do ipe=1,npe
       nn=0
       do k=nx_0,nx_1
          ix=list_sd2y(1,k)
          ivert=list_sd2y(2,k)
          i=ix-jstart(ipe)+2
          if(i >= 1.and.i <= jlon1(ipe)+2) then
             do j=1,ilat1(ipe)+2
                iy=j+istart(ipe)-2
                if(iy >= 1.and.iy <= ny) nn=nn+1
             end do
          end if
       end do
       nsend_y2sd(ipe)=nn
    end do

    ndsend_y2sd(1)=0
    do i=2,npe+1
       ndsend_y2sd(i)=ndsend_y2sd(i-1)+nsend_y2sd(i-1)
    end do
    nallsend_y2sd=ndsend_y2sd(npe+1)
    allocate(info_send_y2sd(3,nallsend_y2sd))
    nn=0
    do ipe=1,npe
       do k=nx_0,nx_1
          ix=list_sd2y(1,k)
          ivert=list_sd2y(2,k)
          i=ix-jstart(ipe)+2
          if(i >= 1.and.i <= jlon1(ipe)+2) then
             do j=1,ilat1(ipe)+2
                iy=j+istart(ipe)-2
                if(iy >= 1.and.iy <= ny) then
                   nn=nn+1
                   info_send_y2sd(1,nn)=iy
                   info_send_y2sd(2,nn)=j
                   info_send_y2sd(3,nn)=k
                end if
             end do
          end if
       end do
    end do

    call mpi_alltoall(nsend_y2sd,1,mpi_integer4,nrecv_y2sd,1,mpi_integer4,mpi_comm_world,ierror)
    ndrecv_y2sd(1)=0
    do i=2,npe+1
       ndrecv_y2sd(i)=ndrecv_y2sd(i-1)+nrecv_y2sd(i-1)
    end do
    nallrecv_y2sd=ndrecv_y2sd(npe+1)
    allocate(info_recv_y2sd(3,nallrecv_y2sd))
    call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(info_send_y2sd,nsend_y2sd,ndsend_y2sd,mpi_string1, &
                       info_recv_y2sd,nrecv_y2sd,ndrecv_y2sd,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)

  end subroutine zrnmi_y2sd1

  subroutine zrnmi_sd2y2(u1_sd,u2_sd,u1_y,u2_y,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_sd2y2
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_sd    -
!     u2_sd    -
!
!   output argument list:
!     u1_y     -
!     u2_y     -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_sd (subdomains) to u_y (x strips)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,jstart,istart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype

    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u1_sd,u2_sd
    real(r_kind),dimension(ny,nx_0:nx_1)   ,intent(  out) :: u1_y,u2_y

    integer(i_kind) ix,ixm,iy,ivert,j,mm1,mpi_string1
    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

    mm1=mype+1

    allocate(sendbuf(2,nallsend_sd2y))
    do j=1,nallsend_sd2y
       iy=info_send_sd2y(1,j)
       ixm=info_send_sd2y(2,j)
       ix=list_sd2y(1,ixm)
       ivert=list_sd2y(2,ixm)
       sendbuf(1,j)=u1_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
       sendbuf(2,j)=u2_sd(iy-istart(mm1)+2,ix-jstart(mm1)+2,ivert)
    end do
    call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(2,nallrecv_sd2y))
    call mpi_alltoallv(sendbuf,nsend_sd2y,ndsend_sd2y,mpi_string1, &
                       recvbuf,nrecv_sd2y,ndrecv_sd2y,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)

    do j=1,nallrecv_sd2y
       iy=info_recv_sd2y(1,j)
       ixm=info_recv_sd2y(2,j)
       u1_y(iy,ixm)=recvbuf(1,j)
       u2_y(iy,ixm)=recvbuf(2,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_sd2y2

  subroutine zrnmi_y2sd2(u1_sd,u2_sd,u1_y,u2_y,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_y2sd2
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_y     -
!     u2_y     -
!
!   output argument list:
!     u1_sd    -
!     u2_sd    -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  use mpi_alltoallv to move u_y (x strips) to u_sd (subdomains)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lon2,lat2,jstart
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use constants, only: zero
    implicit none


    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u1_sd,u2_sd
    real(r_kind),dimension(ny,nx_0:nx_1)   ,intent(in   ) :: u1_y,u2_y

    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
    integer(i_kind) ix,ivert,j,mm1,ixm,iy,iyloc,mpi_string1

    mm1=mype+1

    u1_sd=zero
    u2_sd=zero
    allocate(sendbuf(2,nallsend_y2sd))
    do j=1,nallsend_y2sd
       iy=info_send_y2sd(1,j)
       ixm=info_send_y2sd(3,j)
       sendbuf(1,j)=u1_y(iy,ixm)
       sendbuf(2,j)=u2_y(iy,ixm)
    end do
    call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(2,nallrecv_y2sd))
    call mpi_alltoallv(sendbuf,nsend_y2sd,ndsend_y2sd,mpi_string1, &
                       recvbuf,nrecv_y2sd,ndrecv_y2sd,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)
    do j=1,nallrecv_y2sd
       iyloc=info_recv_y2sd(2,j)
       ixm=info_recv_y2sd(3,j)
       ix=list_sd2y(1,ixm)
       ivert=list_sd2y(2,ixm)
       u1_sd(iyloc,ix-jstart(mm1)+2,ivert)=recvbuf(1,j)
       u2_sd(iyloc,ix-jstart(mm1)+2,ivert)=recvbuf(2,j)
    end do
    deallocate(recvbuf)

  end subroutine zrnmi_y2sd2

  subroutine zrnmi_x_strans0
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x_strans0
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: one,two,pi
    implicit none

    integer(i_kind) k,n

    allocate(sinx(nx,nx))
    do k=1,nx
       do n=1,nx
          sinx(n,k)=sqrt(two/(nx+one))*sin(pi*k*n/(nx+one))
       end do
    end do

  end subroutine zrnmi_x_strans0

  subroutine zrnmi_x_strans(g_x,gt_x)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x_strans
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     g_x      - 
!
!   output argument list:
!     gt_x     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero,half,one
    implicit none

    real(r_kind),intent(in   ) :: g_x(nx,ny_0:ny_1)
    real(r_kind),intent(  out) :: gt_x(nx,ny_0:ny_1)

    integer(i_kind) i,i1,j,k,k1
    real(r_kind) diff,factor,sum,diff1,sum1

    do j=ny_0,ny_1
       do k=1,nx
          gt_x(k,j)=zero
       end do
       do i=1,((nx+1)/2),2
          factor=one
          if(nx+1-i == i) factor=half
          sum =factor*(g_x(i,j)+g_x(nx+1-i,j))
          diff=        g_x(i,j)-g_x(nx+1-i,j)
          i1=i+1
          if(i1 <= (nx+1)/2) then
             factor=one
             if(nx+1-i1 == i1) factor=half
             sum1 =factor*(g_x(i1,j)+g_x(nx+1-i1,j))
             diff1=        g_x(i1,j)-g_x(nx+1-i1,j)
             do k=1,nx-1,2
                k1=k+1
                gt_x(k ,j)=gt_x(k ,j)+sinx(k ,i)*sum +sinx(k ,i1)*sum1
                gt_x(k1,j)=gt_x(k1,j)+sinx(k1,i)*diff+sinx(k1,i1)*diff1
             end do
             if(mod(nx,2) == 1) gt_x(nx,j)=gt_x(nx,j)+sinx(nx,i)*sum+sinx(nx,i1)*sum1
          else
             do k=1,nx-1,2
                k1=k+1
                gt_x(k,j)=gt_x(k,j)+sinx(k,i)*sum
                gt_x(k1,j)=gt_x(k1,j)+sinx(k1,i)*diff
             end do
             if(mod(nx,2) == 1) gt_x(nx,j)=gt_x(nx,j)+sinx(nx,i)*sum
          end if
       end do
    end do

  end subroutine zrnmi_x_strans
 
  subroutine zrnmi_uvm2dzmhat(u,v,m,dhat,zhat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uvm2dzmhat
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u        - 
!     v        - 
!     m        - 
!
!   output argument list:
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(  out) :: dhat,zhat,mhat

    real(r_kind),dimension(lat2,lon2,nvert):: d,z
    real(r_kind),dimension(nx,ny_0:ny_1):: d_x,z_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: d4_x,z4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: d_y,z_y,m_y

    call zrnmi_uv2dz(u,v,d,z,mype)
    call zrnmi_sd2x3(d,z,m,d_x,z_x,m_x,mype)
    call zrnmi_x_strans(d_x,d4_x)
    call zrnmi_x_strans(z_x,z4_x)
    call zrnmi_x_strans(m_x,m4_x)
    call zrnmi_x2y3(d4_x,z4_x,m4_x,d_y,z_y,m_y,mype)
    call zrnmi_y_strans(d_y,dhat)
    call zrnmi_y_strans(z_y,zhat)
    call zrnmi_y_strans(m_y,mhat)

  end subroutine zrnmi_uvm2dzmhat

  subroutine zrnmi_uvm2dzmhat_ad(u,v,m,dhat,zhat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uvm2dzmhat_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
!   output argument list:
!     u        - 
!     v        - 
!     m        - 
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(inout) :: dhat,zhat,mhat

    real(r_kind),dimension(lat2,lon2,nvert):: d,z
    real(r_kind),dimension(nx,ny_0:ny_1):: d_x,z_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: d4_x,z4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: d_y,z_y,m_y

    call zrnmi_y_strans(mhat,m_y)
    call zrnmi_y_strans(zhat,z_y)
    call zrnmi_y_strans(dhat,d_y)
    call zrnmi_y2x3(d4_x,z4_x,m4_x,d_y,z_y,m_y,mype)
    call zrnmi_x_strans(m4_x,m_x)
    call zrnmi_x_strans(z4_x,z_x)
    call zrnmi_x_strans(d4_x,d_x)
    call zrnmi_x2sd3(d,z,m,d_x,z_x,m_x,mype)
    call zrnmi_uv2dz_ad(u,v,d,z,mype)

  end subroutine zrnmi_uvm2dzmhat_ad

  subroutine zrnmi_pcmhat2uvm(p,c,m,phat,chat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_pcmhat2uvm
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     phat     - 
!     chat     - 
!     mhat     - 
!
!   output argument list:
!     p        - 
!     c        - 
!     m        - 
!     phat     - 
!     chat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$
 
    use gridmod, only: lon2,lat2
    implicit none
 
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: p,c,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(inout) :: phat,chat,mhat
 
    real(r_kind),dimension(nx,ny_0:ny_1):: p_x,c_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: p4_x,c4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: p_y,c_y,m_y

    call zrnmi_y_strans(phat,p_y)
    call zrnmi_y_strans(chat,c_y)
    call zrnmi_y_strans(mhat,m_y)
    call zrnmi_y2x3(p4_x,c4_x,m4_x,p_y,c_y,m_y,mype)
    call zrnmi_x_strans(p4_x,p_x)
    call zrnmi_x_strans(c4_x,c_x)
    call zrnmi_x_strans(m4_x,m_x)
    call zrnmi_x2sd3(p,c,m,p_x,c_x,m_x,mype)
 
  end subroutine zrnmi_pcmhat2uvm

  subroutine zrnmi_pcmhat2uvm_orig(u,v,m,phat,chat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_pcmhat2uvm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     phat     -
!     chat     -
!     mhat     -
!
!   output argument list:
!     p        -
!     c        -
!     m        -
!     phat     -
!     chat     -
!     mhat     -
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

    use gridmod, only: lon2,lat2

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(inout) :: phat,chat,mhat

    real(r_kind),dimension(lat2,lon2,nvert):: p,c
    real(r_kind),dimension(nx,ny_0:ny_1):: p_x,c_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: p4_x,c4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: p_y,c_y,m_y

    call zrnmi_y_strans(phat,p_y)
    call zrnmi_y_strans(chat,c_y)
    call zrnmi_y_strans(mhat,m_y)
    call zrnmi_y2x3(p4_x,c4_x,m4_x,p_y,c_y,m_y,mype)
    call zrnmi_x_strans(p4_x,p_x)
    call zrnmi_x_strans(c4_x,c_x)
    call zrnmi_x_strans(m4_x,m_x)
    call zrnmi_x2sd3(p,c,m,p_x,c_x,m_x,mype)
    call zrnmi_pc2uv_orig(p,c,u,v,mype)

  end subroutine zrnmi_pcmhat2uvm_orig

  subroutine zrnmi_pcmhat2uvm_ad_orig(u,v,m,phat,chat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zrnmi_pcmhat2uvm_ad_orig
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    u,v,m
!
!   output argument list:
!    u,v,m
!    phat,chat,mhat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use constants, only: zero
    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(  out) :: phat,chat,mhat

    real(r_kind),dimension(lat2,lon2,nvert):: p,c
    real(r_kind),dimension(nx,ny_0:ny_1):: p_x,c_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: p4_x,c4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: p_y,c_y,m_y

    call zrnmi_pc2uv_ad_orig(p,c,u,v,mype)
    call zrnmi_sd2x3(p,c,m,p_x,c_x,m_x,mype)
    call zrnmi_x_strans(p_x,p4_x)
    call zrnmi_x_strans(c_x,c4_x)
    call zrnmi_x_strans(m_x,m4_x)
    call zrnmi_x2y3(p4_x,c4_x,m4_x,p_y,c_y,m_y,mype)
    call zrnmi_y_strans(p_y,phat)
    call zrnmi_y_strans(c_y,chat)
    call zrnmi_y_strans(m_y,mhat)

  end subroutine zrnmi_pcmhat2uvm_ad_orig

  subroutine zrnmi_pcmhat2uvm_ad(p,c,m,phat,chat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_pcmhat2uvm_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     p        - 
!     c        - 
!     m        - 
!
!   output argument list:
!     p        - 
!     c        - 
!     m        - 
!     phat     - 
!     chat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$
 
    use gridmod, only: lon2,lat2
    implicit none
 
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: p,c,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(  out) :: phat,chat,mhat
 
    real(r_kind),dimension(nx,ny_0:ny_1):: p_x,c_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: p4_x,c4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: p_y,c_y,m_y

    call zrnmi_sd2x3(p,c,m,p_x,c_x,m_x,mype)
    call zrnmi_x_strans(p_x,p4_x)
    call zrnmi_x_strans(c_x,c4_x)
    call zrnmi_x_strans(m_x,m4_x)
    call zrnmi_x2y3(p4_x,c4_x,m4_x,p_y,c_y,m_y,mype)
    call zrnmi_y_strans(p_y,phat)
    call zrnmi_y_strans(c_y,chat)
    call zrnmi_y_strans(m_y,mhat)
 
  end subroutine zrnmi_pcmhat2uvm_ad

  subroutine zrnmi_x2y0(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2y0
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  create y (x strips) subdivision for use with sine transform in y direction

!  output:

!     mx_0,mx_1:  range of x/vert index on processor mype

!           1 <= mx_0 <= mx_1 <= nx*nvert
!           if npe > nx*nvert, then will have mx_0 = -1, mx_1 = -2 on some processors,
!               and mx_0=mx_1 on the remaining nx*nvert processors
!
!     list_x2y(3,nx*nvert):  global definition of contents of each x/vert strip
!                      list_x2y(1,j) = x index for y strip j
!                      list_x2y(2,j) = vert level for y strip j
!                      list_x2y(3,j) = pe of this x/vert strip

    use mpimod, only:  npe
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,k,kchk,kk,n,nn,mx_this,mx_tot

    allocate(list_x2y(3,nx*nvert))

    mx_tot=nx*nvert
    mx_this=mx_tot/npe
    if(mod(mx_tot,npe)/=0) mx_this=mx_this+1
    if(mod(mx_tot,npe)==0) then
       kchk=npe
    else
       kchk=mod(mx_tot,npe)
    end if

    nn=0
    do k=1,nvert
       do i=1,nx
          nn=nn+1
          list_x2y(1,nn)=i
          list_x2y(2,nn)=k
          list_x2y(3,nn)=-1
       end do
    end do

    if(mype == 0) write(6,*)' in zrnmi_x2y0, nn,mx_tot,nx,nvert=',nn,mx_tot,nx,nvert

    mx_0=-1
    mx_1=-2
    nn=0
    do n=1,npe
       if(n <= kchk) then
          kk=mx_this
       else
          kk=mx_this-1
       end if
       if(kk >  0) then
          if(mype+1 == n) then
             mx_0=nn+1
             mx_1=nn+kk
          end if
          do k=1,kk
             nn=nn+1
             list_x2y(3,nn)=n
          end do
       end if
    end do

  end subroutine zrnmi_x2y0

  subroutine zrnmi_x2y1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2y1
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

!  continue with setup for x strip to y strip communication

    use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) list2(nx,nvert)
    integer(i_kind) i,ii0,ix,ivert,j,k,ipe,ixm,iy,mpi_string1
    integer(i_kind) mx_tot

    allocate(nsend_x2y(npe),nrecv_x2y(npe),ndsend_x2y(npe+1),ndrecv_x2y(npe+1))
    mx_tot=nx*nvert

    list2=0
    do j=1,mx_tot
       ix=list_x2y(1,j)
       ivert=list_x2y(2,j)
       if(list2(ix,ivert) /= 0) then
          if(mype == 0) write(0,*)' problem in zrnmi_x2y1'
          call mpi_finalize(i)
          stop
       end if
       list2(ix,ivert)=j
    end do
    do ivert=1,nvert
       do ix=1,nx
          if(list2(ix,ivert) == 0) then
             if(mype == 0) write(0,*)' problem in zrnmi_x2y1'
             call mpi_finalize(i)
             stop
          end if
       end do
    end do

!  obtain counts of points to send to each pe from this pe

    nsend_x2y=0
    do k=ny_0,ny_1
       ivert=list_sd2x(2,k)
       do ix=1,nx
          j=list2(ix,ivert)
          ipe=list_x2y(3,j)
          nsend_x2y(ipe)=nsend_x2y(ipe)+1
       end do
    end do

    ndsend_x2y(1)=0
    do i=2,npe+1
       ndsend_x2y(i)=ndsend_x2y(i-1)+nsend_x2y(i-1)
    end do
    nallsend_x2y=ndsend_x2y(npe+1)
    allocate(info_send_x2y(4,nallsend_x2y))
    nsend_x2y=0
    do k=ny_0,ny_1
       iy=list_sd2x(1,k)
       ivert=list_sd2x(2,k)
       do ix=1,nx
          ixm=list2(ix,ivert)
          ipe=list_x2y(3,ixm)
          nsend_x2y(ipe)=nsend_x2y(ipe)+1
          ii0=ndsend_x2y(ipe)+nsend_x2y(ipe)
          info_send_x2y(1,ii0)=ix
          info_send_x2y(2,ii0)=k
          info_send_x2y(3,ii0)=iy
          info_send_x2y(4,ii0)=ixm
       end do
    end do

    call mpi_alltoall(nsend_x2y,1,mpi_integer4,nrecv_x2y,1,mpi_integer4,mpi_comm_world,ierror)
    ndrecv_x2y(1)=0
    do i=2,npe+1
       ndrecv_x2y(i)=ndrecv_x2y(i-1)+nrecv_x2y(i-1)
    end do
    nallrecv_x2y=ndrecv_x2y(npe+1)
    allocate(info_recv_x2y(4,nallrecv_x2y))
    call mpi_type_contiguous(4,mpi_integer4,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(info_send_x2y,nsend_x2y,ndsend_x2y,mpi_string1, &
                     info_recv_x2y,nrecv_x2y,ndrecv_x2y,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)

  end subroutine zrnmi_x2y1

  subroutine zrnmi_x2y3(u1_x,u2_x,u3_x,u1_y,u2_y,u3_y,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_x2y3
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!     u1_x     -
!     u2_x     -
!     u3_x     -
!
!   output argument list:
!     u1_y     -
!     u2_y     -
!     u3_y     -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    implicit none

    integer(i_kind)                     ,intent(in   ) :: mype
    real(r_kind),dimension(nx,ny_0:ny_1),intent(in   ) :: u1_x,u2_x,u3_x
    real(r_kind),dimension(ny,mx_0:mx_1),intent(  out) :: u1_y,u2_y,u3_y

    integer(i_kind) ixm,ix,iy,iym,j,mm1,mpi_string1
    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

    mm1=mype+1

    allocate(sendbuf(3,nallsend_x2y))
    do j=1,nallsend_x2y
       ix=info_send_x2y(1,j)
       iym=info_send_x2y(2,j)
       sendbuf(1,j)=u1_x(ix,iym)
       sendbuf(2,j)=u2_x(ix,iym)
       sendbuf(3,j)=u3_x(ix,iym)
    end do
    call mpi_type_contiguous(3,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    allocate(recvbuf(3,nallrecv_x2y))
    call mpi_alltoallv(sendbuf,nsend_x2y,ndsend_x2y,mpi_string1, &
                       recvbuf,nrecv_x2y,ndrecv_x2y,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(sendbuf)

    do j=1,nallrecv_x2y
       iy=info_recv_x2y(3,j)
       ixm=info_recv_x2y(4,j)
       u1_y(iy,ixm)=recvbuf(1,j)
       u2_y(iy,ixm)=recvbuf(2,j)
       u3_y(iy,ixm)=recvbuf(3,j)
    end do
    deallocate(recvbuf)
    
  end subroutine zrnmi_x2y3

  subroutine zrnmi_y2x3(u1_x,u2_x,u3_x,u1_y,u2_y,u3_y,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_y2x3
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u1_y     -
!     u2_y     -
!     u3_y     -
!
!   output argument list:
!     u1_x     -
!     u2_x     -
!     u3_x     -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    implicit none

    integer(i_kind)                     ,intent(in   ) :: mype
    real(r_kind),dimension(nx,ny_0:ny_1),intent(  out) :: u1_x,u2_x,u3_x
    real(r_kind),dimension(ny,mx_0:mx_1),intent(in   ) :: u1_y,u2_y,u3_y

    integer(i_kind) ixm,ix,iy,iym,j,mm1,mpi_string1
    real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

    mm1=mype+1

    allocate(recvbuf(3,nallrecv_x2y))
    do j=1,nallrecv_x2y
       iy=info_recv_x2y(3,j)
       ixm=info_recv_x2y(4,j)
       recvbuf(1,j)=u1_y(iy,ixm)
       recvbuf(2,j)=u2_y(iy,ixm)
       recvbuf(3,j)=u3_y(iy,ixm)
    end do
    allocate(sendbuf(3,nallsend_x2y))
    call mpi_type_contiguous(3,mpi_rtype,mpi_string1,ierror)
    call mpi_type_commit(mpi_string1,ierror)
    call mpi_alltoallv(recvbuf,nrecv_x2y,ndrecv_x2y,mpi_string1, &
                       sendbuf,nsend_x2y,ndsend_x2y,mpi_string1,mpi_comm_world,ierror)
    call mpi_type_free(mpi_string1,ierror)
    deallocate(recvbuf)
    do j=1,nallsend_x2y
       ix=info_send_x2y(1,j)
       iym=info_send_x2y(2,j)
       u1_x(ix,iym)=sendbuf(1,j)
       u2_x(ix,iym)=sendbuf(2,j)
       u3_x(ix,iym)=sendbuf(3,j)
    end do
    deallocate(sendbuf)

  end subroutine zrnmi_y2x3

  subroutine zrnmi_y_strans0
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_y_strans0
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: one,two,pi
    implicit none

    integer(i_kind) k,n

    allocate(siny(ny,ny))
    do k=1,ny
       do n=1,ny
          siny(n,k)=sqrt(two/(ny+one))*sin(pi*k*n/(ny+one))
       end do
    end do

  end subroutine zrnmi_y_strans0

  subroutine zrnmi_y_strans(g_y,gt_y)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_y_strans
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     g_y      - 
!
!   output argument list:
!     gt_y     -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero,half,one
    implicit none

    real(r_kind),intent(in   ) :: g_y(ny,mx_0:mx_1)
    real(r_kind),intent(  out) :: gt_y(ny,mx_0:mx_1)

    integer(i_kind) i,i1,j,k,k1
    real(r_kind) diff,factor,sum,diff1,sum1

    do j=mx_0,mx_1
       do k=1,ny
          gt_y(k,j)=zero
       end do
       do i=1,((ny+1)/2),2
          factor=one
          if(ny+1-i == i) factor=half
          sum =factor*(g_y(i,j)+g_y(ny+1-i,j))
          diff=        g_y(i,j)-g_y(ny+1-i,j)
          i1=i+1
          if(i1 <= (ny+1)/2) then
             factor=one
             if(ny+1-i1 == i1) factor=half
             sum1 =factor*(g_y(i1,j)+g_y(ny+1-i1,j))
             diff1=        g_y(i1,j)-g_y(ny+1-i1,j)
             do k=1,ny-1,2
                k1=k+1
                gt_y(k ,j)=gt_y(k ,j)+siny(k ,i)*sum +siny(k ,i1)*sum1
                gt_y(k1,j)=gt_y(k1,j)+siny(k1,i)*diff+siny(k1,i1)*diff1
             end do
             if(mod(ny,2) == 1) gt_y(ny,j)=gt_y(ny,j)+siny(ny,i)*sum+siny(ny,i1)*sum1
          else
             do k=1,ny-1,2
                k1=k+1
                gt_y(k,j)=gt_y(k,j)+siny(k,i)*sum
                gt_y(k1,j)=gt_y(k1,j)+siny(k1,i)*diff
             end do
             if(mod(ny,2) == 1) gt_y(ny,j)=gt_y(ny,j)+siny(ny,i)*sum
          end if
       end do
    end do

  end subroutine zrnmi_y_strans

  subroutine zrnmi_delx_general(f_x,fx_x,vector,iord)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zrnmi_delx_general
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    f_x
!    vector
!    iord
!
!   output argument list:
!    fx_x
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use gridmod, only: region_dy,region_dyi

    real(r_kind)   ,intent(in   ) ::  f_x(nx,ny_0:ny_1)
    real(r_kind)   ,intent(  out) :: fx_x(nx,ny_0:ny_1)
    logical        ,intent(in   ) :: vector
    integer(i_kind),intent(in   ) :: iord

    real(r_kind) work1(nx,ny_0:ny_1)
    real(r_kind) work2(nx,ny_0:ny_1)
    integer(i_kind) i,j,iy

    if(vector) then
       do i=ny_0,ny_1
          iy=list_sd2x(1,i)
          do j=1,nx
             work1(j,i)=f_x(j,i)*region_dy(iy,j)
          end do
       end do
    else
       do i=ny_0,ny_1
          do j=1,nx
             work1(j,i)=f_x(j,i)
          end do
       end do
    end if

    if(iord == 2) call zrnmi_delx(work1,work2)
   !if(iord == 4) call zrnmi_delx_4th_ord(work1,work2)

    if(vector) then
       do i=ny_0,ny_1
          iy=list_sd2x(1,i)
          do j=1,nx
             fx_x(j,i)=work2(j,i)*region_dyi(iy,j)
          end do
       end do
    else
       do i=ny_0,ny_1
          do j=1,nx
             fx_x(j,i)=work2(j,i)
          end do
       end do
    end if

  end subroutine zrnmi_delx_general

  subroutine zrnmi_delx(f_x,fx_x)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_delx
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     f_x      -
!
!   output argument list:
!     fx_x     -
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: coeffx
    implicit none

    real(r_kind)  ,intent(in   ) :: f_x(nx,ny_0:ny_1)
    real(r_kind),intent(  out) :: fx_x(nx,ny_0:ny_1)

    integer(i_kind) i,j,iy

    do i=ny_0,ny_1
       iy=list_sd2x(1,i)
       fx_x(1,i)=(f_x(3,i)-f_x(1,i))*coeffx(iy,1)
       do j=2,nx-1
          fx_x(j,i)=(f_x(j+1,i)-f_x(j-1,i))*coeffx(iy,j)
       end do
       fx_x(nx,i)=(f_x(nx,i)-f_x(nx-2,i))*coeffx(iy,nx)
    end do

  end subroutine zrnmi_delx

  subroutine zrnmi_delx_ad(f_x,fx_x)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_delx_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     fx_x     - 
!
!   output argument list:
!     f_x      - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: coeffx
    implicit none

    real(r_kind)  ,intent(  out) :: f_x(nx,ny_0:ny_1)
    real(r_kind)  ,intent(in   ) :: fx_x(nx,ny_0:ny_1)

    integer(i_kind) i,j,iy

    f_x=zero
    do i=ny_0,ny_1
       iy=list_sd2x(1,i)
       f_x(nx  ,i)=f_x(nx  ,i)+fx_x(nx,i)*coeffx(iy,nx)
       f_x(nx-2,i)=f_x(nx-2,i)-fx_x(nx,i)*coeffx(iy,nx)
       do j=2,nx-1
          f_x(j+1,i)=f_x(j+1,i)+fx_x(j,i)*coeffx(iy,j)
          f_x(j-1,i)=f_x(j-1,i)-fx_x(j,i)*coeffx(iy,j)
       end do
       f_x(1,i)=f_x(1,i)-fx_x(1,i)*coeffx(iy,1)
       f_x(3,i)=f_x(3,i)+fx_x(1,i)*coeffx(iy,1)
    end do

  end subroutine zrnmi_delx_ad

  subroutine zrnmi_dely_general(f_y,fy_y,vector,iord)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zrnmi_dely_general
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    f_y
!    vector
!    iord
!
!   output argument list:
!    fy_y
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use gridmod, only: region_dx,region_dxi
    implicit none

    real(r_kind)   ,intent(in   ) ::  f_y(ny,nx_0:nx_1)
    real(r_kind)   ,intent(  out) :: fy_y(ny,nx_0:nx_1)
    logical        ,intent(in   ) :: vector
    integer(i_kind),intent(in   ) :: iord

    real(r_kind) work1(ny,nx_0:nx_1)
    real(r_kind) work2(ny,nx_0:nx_1)
    integer(i_kind) i,j,ix

    if(vector) then
       do i=nx_0,nx_1
          ix=list_sd2y(1,i)
          do j=1,ny
             work1(j,i)=f_y(j,i)*region_dx(j,ix)
          end do
       end do
    else
       do i=nx_0,nx_1
          do j=1,ny
             work1(j,i)=f_y(j,i)
          end do
       end do
    end if

    if(iord == 2) call zrnmi_dely(work1,work2)
   !if(iord == 4) call zrnmi_dely_4th_ord(work1,work2)

    if(vector) then
       do i=nx_0,nx_1
          ix=list_sd2y(1,i)
          do j=1,ny
             fy_y(j,i)=work2(j,i)*region_dxi(j,ix)
          end do
       end do
    else
       do i=nx_0,nx_1
          do j=1,ny
             fy_y(j,i)=work2(j,i)
          end do
       end do
    end if

  end subroutine zrnmi_dely_general

  subroutine zrnmi_dely(f_y,fy_y)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_dely
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     f_y      - 
!
!   output argument list:
!     fy_y     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: coeffy
    implicit none

    real(r_kind)  ,intent(in   ) :: f_y(ny,nx_0:nx_1)
    real(r_kind)  ,intent(  out) :: fy_y(ny,nx_0:nx_1)

    integer(i_kind) i,j,ix

    do i=nx_0,nx_1
       ix=list_sd2y(1,i)
       fy_y(1,i)=(f_y(3,i)-f_y(1,i))*coeffy(1,ix)
       do j=2,ny-1
          fy_y(j,i)=(f_y(j+1,i)-f_y(j-1,i))*coeffy(j,ix)
       end do
       fy_y(ny,i)=(f_y(ny,i)-f_y(ny-2,i))*coeffy(ny,ix)
    end do

  end subroutine zrnmi_dely

  subroutine zrnmi_dely_ad(f_y,fy_y)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_dely_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     fy_y     - 
!
!   output argument list:
!     f_y      - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: coeffy
    implicit none

    real(r_kind)  ,intent(  out) :: f_y(ny,nx_0:nx_1)
    real(r_kind)  ,intent(in   ) :: fy_y(ny,nx_0:nx_1)

    integer(i_kind) i,j,ix

    f_y=zero
    do i=nx_0,nx_1
       ix=list_sd2y(1,i)
       f_y(ny  ,i)=f_y(ny  ,i)+fy_y(ny,i)*coeffy(ny,ix)
       f_y(ny-2,i)=f_y(ny-2,i)-fy_y(ny,i)*coeffy(ny,ix)
       do j=2,ny-1
          f_y(j+1,i)=f_y(j+1,i)+fy_y(j,i)*coeffy(j,ix)
          f_y(j-1,i)=f_y(j-1,i)-fy_y(j,i)*coeffy(j,ix)
       end do
       f_y(3,i)=f_y(3,i)+fy_y(1,i)*coeffy(1,ix)
       f_y(1,i)=f_y(1,i)-fy_y(1,i)*coeffy(1,ix)
    end do

  end subroutine zrnmi_dely_ad

  subroutine zrnmi_uv2dz(u,v,div,vort,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uv2dz
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!     u        - 
!     v        - 
!
!   output argument list:
!     div      - 
!     vort     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: lat2,lon2
    implicit none

! Declare passed variables
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u,v
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: div,vort

! Declare local variables
    real(r_kind),dimension(nx,ny_0:ny_1)::u_x,v_x,ux_x,vx_x
    real(r_kind),dimension(ny,nx_0:nx_1)::u_y,v_y,uy_y,vy_y
    real(r_kind),dimension(lat2,lon2,nvert)::ux,vx,uy,vy

    u_x=zero ; v_x=zero
    call zrnmi_sd2x2(u,v,u_x,v_x,mype)
    u_y=zero ; v_y=zero
    call zrnmi_sd2y2(u,v,u_y,v_y,mype)
    ux_x=zero
    call zrnmi_delx_general(u_x,ux_x,.true.,2)
    vx_x=zero
    call zrnmi_delx_general(v_x,vx_x,.true.,2)
    uy_y=zero
    call zrnmi_dely_general(u_y,uy_y,.true.,2)
    vy_y=zero
    call zrnmi_dely_general(v_y,vy_y,.true.,2)
    ux=zero ; vx=zero
    call zrnmi_x2sd2(ux,vx,ux_x,vx_x,mype)
    uy=zero ; vy=zero
    call zrnmi_y2sd2(uy,vy,uy_y,vy_y,mype)
    div=ux+vy
    vort=vx-uy

  end subroutine zrnmi_uv2dz

  subroutine zrnmi_uv2dz_ad(u,v,div,vort,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uv2dz_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!     div      - 
!     vort     - 
!
!   output argument list:
!     u        -
!     v        -
!     div      - 
!     vort     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: lat2,lon2
    implicit none

! Declare passed variables
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: div,vort

! Declare local variables
    real(r_kind),dimension(nx,ny_0:ny_1)::u_x,v_x,ux_x,vx_x
    real(r_kind),dimension(ny,nx_0:nx_1)::u_y,v_y,uy_y,vy_y
    real(r_kind),dimension(lat2,lon2,nvert)::ux,vx,uy,vy

    ux=div
    uy=-vort
    vx=vort
    vy=div
    uy_y=zero ; vy_y=zero
    call zrnmi_sd2y2(uy,vy,uy_y,vy_y,mype)
    ux_x=zero ; vx_x=zero
    call zrnmi_sd2x2(ux,vx,ux_x,vx_x,mype)
    v_y=zero
    call zrnmi_dely_ad(v_y,vy_y)
    u_y=zero
    call zrnmi_dely_ad(u_y,uy_y)
    v_x=zero
    call zrnmi_delx_ad(v_x,vx_x)
    u_x=zero
    call zrnmi_delx_ad(u_x,ux_x)
    ux=zero ; vx=zero
    call zrnmi_y2sd2(ux,vx,u_y,v_y,mype)
    u=zero ; v=zero
    call zrnmi_x2sd2(u,v,u_x,v_x,mype)
    u=u+ux ; v=v+vx

  end subroutine zrnmi_uv2dz_ad

  subroutine zrnmi_pc2uv_orig(psi,chi,u,v,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zrnmi_pc2uv_orig
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    psi,chi
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: lat2,lon2
    implicit none

! Declare passed variables
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: psi,chi
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v

! Declare local variables
    real(r_kind),dimension(nx,ny_0:ny_1)::p_x,c_x,px_x,cx_x
    real(r_kind),dimension(ny,nx_0:nx_1)::p_y,c_y,py_y,cy_y
    real(r_kind),dimension(lat2,lon2,nvert)::px,cx,py,cy

    p_x=zero ; c_x=zero
    call zrnmi_sd2x2(psi,chi,p_x,c_x,mype)
    p_y=zero ; c_y=zero
    call zrnmi_sd2y2(psi,chi,p_y,c_y,mype)
    px_x=zero
    call zrnmi_delx(p_x,px_x)
    cx_x=zero
    call zrnmi_delx(c_x,cx_x)
    py_y=zero
    call zrnmi_dely(p_y,py_y)
    cy_y=zero
    call zrnmi_dely(c_y,cy_y)
    px=zero ; cx=zero
    call zrnmi_x2sd2(px,cx,px_x,cx_x,mype)
    py=zero ; cy=zero
    call zrnmi_y2sd2(py,cy,py_y,cy_y,mype)
    u=cx-py
    v=px+cy

  end subroutine zrnmi_pc2uv_orig

  subroutine zrnmi_pc2uv_ad_orig(psi,chi,u,v,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zrnmi_pc2uv_ad_orig
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    u,v
!
!   output argument list:
!    psi,chi
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: lat2,lon2
    implicit none

! Declare passed variables
    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: psi,chi
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: u,v

! Declare local variables
    real(r_kind),dimension(nx,ny_0:ny_1)::p_x,c_x,px_x,cx_x
    real(r_kind),dimension(ny,nx_0:nx_1)::p_y,c_y,py_y,cy_y
    real(r_kind),dimension(lat2,lon2,nvert)::px,cx,py,cy

    cx=u ; cy=v ; px=v ; py=-u
    py_y=zero ; cy_y=zero
    call zrnmi_sd2y2(py,cy,py_y,cy_y,mype)
    px_x=zero ; cx_x=zero
    call zrnmi_sd2x2(px,cx,px_x,cx_x,mype)
    c_y=zero
    call zrnmi_dely_ad(c_y,cy_y)
    p_y=zero
    call zrnmi_dely_ad(p_y,py_y)
    c_x=zero
    call zrnmi_delx_ad(c_x,cx_x)
    p_x=zero
    call zrnmi_delx_ad(p_x,px_x)
    py=zero ; cy=zero
    call zrnmi_y2sd2(py,cy,p_y,c_y,mype)
    px=zero ; cx=zero
    call zrnmi_x2sd2(px,cx,p_x,c_x,mype)
    psi=px+py ; chi=cx+cy

  end subroutine zrnmi_pc2uv_ad_orig

  subroutine zrnmi_constants(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_constants
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$


    use constants, only: zero,half,one,two,omega,pi,one_tenth,r3600
    use gridmod, only: region_dx,region_dy
    use mod_vtrans, only: depths,speeds
    use mpimod,only: mpi_rtype,mpi_integer,mpi_max,mpi_min,mpi_sum,mpi_comm_world,ierror
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,j,k,mode
    real(r_kind) bigk,bigl,dxbar,dybar,a2,ak2,s2
    real(r_kind) thisperiod,thislength
    real(r_kind) rlenmax(nvert),permax(nvert)
    integer(i_kind) numkeep(nvert),numtot(nvert)
    real(r_kind) rlenmax0(nvert),permax0(nvert)
    integer(i_kind) numkeep0(nvert),numtot0(nvert)
    real(r_kind) pmaskmax(nvert),pmaskmin(nvert),pmaskmax0(nvert),pmaskmin0(nvert)

    fbar=two*omega
    dxbar=zero
    dybar=zero
    do j=1,nx
       do i=1,ny
          dxbar=dxbar+region_dx(i,j)
          dybar=dybar+region_dy(i,j)
       end do
    end do
    dxbar=dxbar/(nx*ny)
    dybar=dybar/(nx*ny)
    if(mype == 0) then
       write(6,*)' in zrnmi_constants, dxbar=',dxbar
       write(6,*)' in zrnmi_constants, dybar=',dybar
       write(6,*)' in zrnmi_constants,  fbar=', fbar
       do k=1,nvert
          write(6,*)' in zrnmi_constants, k,depths(k)=',k,depths(k)
       end do
    end if

    allocate(am2(ny,mx_0:mx_1),f_sm2_am2(ny,mx_0:mx_1),sm2(ny,mx_0:mx_1))
    allocate(a2_p0_sm2(ny,mx_0:mx_1),f_p0_sm2(ny,mx_0:mx_1),p0_sm2(ny,mx_0:mx_1))
    allocate(pmask(ny,mx_0:mx_1))

    permax=zero
    rlenmax=zero
    pmask=zero
    am2=zero
    sm2=zero
    f_sm2_am2=zero
    p0_sm2=zero
    a2_p0_sm2=zero
    f_p0_sm2=zero
    pmaskmax=-huge(pmaskmax)
    pmaskmin= huge(pmaskmin)
    do j=mx_0,mx_1
       k=list_x2y(1,j)
       bigk=k
       ak2=( (two/dxbar)*sin(pi*bigk/(two*(nx+one))) )**2
       mode=list_x2y(2,j)
       do i=1,ny
          bigl=i
          a2 = ak2 + ( (two/dybar)*sin(pi*bigl/(two*(ny+one))) )**2
          s2=depths(mode)*a2+fbar**2
          am2(i,j)=one/a2
          sm2(i,j)=one/s2
          f_sm2_am2(i,j)=fbar*sm2(i,j)*am2(i,j)
          p0_sm2(i,j)=depths(mode)*sm2(i,j)
          a2_p0_sm2(i,j)=a2*p0_sm2(i,j)
          f_p0_sm2(i,j)=fbar*p0_sm2(i,j)
 
          thislength=two*pi*sqrt(am2(i,j))
      !   if(thislength <  3._r_kind*max(dxbar,dybar)) cycle
          thisperiod=thislength/(speeds(mode)*r3600)
        ! pmask(i,j)=half*(one-tanh((thisperiod-zrnmi_period_max)/zrnmi_period_width))
          if(thisperiod <= zrnmi_period_max) pmask(i,j)=one
          permax(mode)=max(permax(mode),thisperiod)
          rlenmax(mode)=max(rlenmax(mode),thislength)
       end do
    end do
    numkeep=zero
    numtot=zero
    do j=mx_0,mx_1
       mode=list_x2y(2,j)
       do i=1,ny
          numtot(mode)=numtot(mode)+1
          if(pmask(i,j) >  one_tenth) numkeep(mode)=numkeep(mode)+1
          pmaskmax(mode)=max(pmask(i,j),pmaskmax(mode))
          pmaskmin(mode)=min(pmask(i,j),pmaskmin(mode))
       end do
    end do
    call mpi_allreduce(rlenmax,rlenmax0,nvert,mpi_rtype,mpi_max,mpi_comm_world,ierror)
    call mpi_allreduce(permax,permax0,nvert,mpi_rtype,mpi_max,mpi_comm_world,ierror)
    call mpi_allreduce(pmaskmax,pmaskmax0,nvert,mpi_rtype,mpi_max,mpi_comm_world,ierror)
    call mpi_allreduce(pmaskmin,pmaskmin0,nvert,mpi_rtype,mpi_min,mpi_comm_world,ierror)
    call mpi_allreduce(numkeep,numkeep0,nvert,mpi_integer,mpi_sum,mpi_comm_world,ierror)
    call mpi_allreduce(numtot,numtot0,nvert,mpi_integer,mpi_sum,mpi_comm_world,ierror)
    if(mype == 0) then
       do k=1,nvert
          write(6,*)' in zrnmi_constants, k,period_max,numkeep,numtot,lenmax,permax=', &
                      k,zrnmi_period_max,numkeep0(k),numtot0(k),rlenmax0(k),permax0(k)
       end do
       do k=1,nvert
          write(6,*)' in zrnmi_constants, k,pmaskmax,min=',k,pmaskmax0(k),pmaskmin0(k)
       end do
    end if

 
  end subroutine zrnmi_constants

  subroutine zrnmi_strong_bal_correction(ut,vt,tt,pst,psi,chi,t,ps,baldiag,fullfield,update,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_strong_bal_correction
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!   2009-11-27  parrish - add uv_hyb_ens.  if present and true, then
!                          input/output variables psi=u, chi=v.
!
!   input argument list:
!     mype     - mpi task id
!     ut       - 
!     vt       - 
!     tt       - 
!     pst      - 
!     psi      - 
!     chi      - 
!     t        - 
!     ps       - 
!     baldiag  - 
!     update   - 
!     fullfield- 
!
!   output argument list:
!     ut       - 
!     vt       - 
!     tt       - 
!     pst      - 
!     u        - 
!     v        - 
!     t        - 
!     ps       - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: lat2,lon2,nsig
    use mod_vtrans, only: vtrans,vtrans_inv
    use mod_vtrans, only: depths
    use mpimod,only: mpi_rtype,mpi_sum,mpi_comm_world,ierror
    use jfunc,only: jiter
    use hybrid_ensemble_parameters, only: uv_hyb_ens
    implicit none

!   initially just generate projections and make maps

    real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: ut,vt,tt
    real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: pst
    real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: psi,chi,t
    real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: ps
    logical                               ,intent(in   ) :: baldiag,update,fullfield
    integer(i_kind)                       ,intent(in   ) :: mype

    real(r_kind),dimension(ny,mx_0:mx_1,2)::rbalg
    real(r_kind),dimension(lat2,lon2,nvert)::utilde,vtilde,mtilde
    real(r_kind),dimension(ny,mx_0:mx_1)::dhat,zhat,mhat
    real(r_kind),dimension(ny,mx_0:mx_1)::phat2,chat2,mhat2
    real(r_kind),dimension(lat2,lon2,nsig)::dpsi,dchi,dt
    real(r_kind),dimension(lat2,lon2)::dps
    real(r_kind),dimension(nvert)::baldt(nvert),balagt(nvert)
    real(r_kind),dimension(nvert)::baldt0(nvert),balagt0(nvert)
    real(r_kind) baldt_all,balagt_all
    integer(i_kind) i,j,k,mode

!      vertical mode transform

    utilde=zero ; vtilde=zero ; mtilde=zero
    call vtrans(ut,vt,tt,pst,utilde,vtilde,mtilde)

!      transform to spectral space

    call zrnmi_uvm2dzmhat(utilde,vtilde,mtilde,dhat,zhat,mhat,mype)

!      apply mask to input spectral fields to limit periods considered
    dhat=pmask*dhat
    zhat=pmask*zhat
    mhat=pmask*mhat



    if(update) then
!      compute gravity projected corrections:
    !  chat2=f_sm2_am2*zhat+sm2*mhat                     !  original, similar to bourke-mcgregor scheme B
       chat2=               sm2*mhat                     !  emulate bourke-mcgregor scheme A
       phat2=-f_sm2_am2*dhat
       mhat2=p0_sm2*dhat

!   transform to grid space
       if(uv_hyb_ens) then
          call zrnmi_pcmhat2uvm_orig(utilde,vtilde,mtilde,phat2,chat2,mhat2,mype)
       else
          call zrnmi_pcmhat2uvm(utilde,vtilde,mtilde,phat2,chat2,mhat2,mype)
       end if
       dt=zero
       dpsi=zero
       dchi=zero
       dps=zero
       call vtrans_inv(utilde,vtilde,mtilde,dpsi,dchi,dt,dps)

       t=t-dt ; ps=ps-dps
       psi=psi-dpsi ; chi=chi-dchi
    end if


!     compute baldt, balagt, and bal=baldt+balagt
    if(baldiag) then

!      compute gravity tendency amplitudes
       do j=mx_0,mx_1
          mode=list_x2y(2,j)
          do i=1,ny
             rbalg(i,j,1)=sqrt(am2(i,j)*depths(mode))*dhat(i,j)
             rbalg(i,j,2)=sqrt(a2_p0_sm2(i,j))*(fbar*am2(i,j)*zhat(i,j)+mhat(i,j))
          end do
       end do
       baldt=zero
       balagt=zero
       do j=mx_0,mx_1
          mode=list_x2y(2,j)
          do i=1,ny
             baldt(mode)=baldt(mode)  +rbalg(i,j,1)**2
             balagt(mode)=balagt(mode)+rbalg(i,j,2)**2
          end do
       end do
       call mpi_allreduce(balagt,balagt0,nvert,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       call mpi_allreduce(baldt,baldt0,nvert,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
       if(mype == 0) then

          if (fullfield) then
             write(6,*) 'ZRNMI_STRONG_BAL:   FULL FIELD BALANCE DIAGNOSTICS --  '
          else
             write(6,*) 'ZRNMI_STRONG_BAL:   INCREMENTAL BALANCE DIAGNOSTICS --  '
          end if

          baldt_all=zero ; balagt_all=zero
          do k=1,nvert
             baldt_all=baldt_all+baldt0(k)
             balagt_all=balagt_all+balagt0(k)
             write(6,*)' jiter, k,baldt,balagt,bal=',jiter,k,baldt0(k),balagt0(k),baldt0(k)+balagt0(k)
          end do
          write(6,*)' jiter, baldt,balagt,bal=',jiter,baldt_all,balagt_all,baldt_all+balagt_all
       end if
    end if

  end subroutine zrnmi_strong_bal_correction

  subroutine zrnmi_strong_bal_correction_ad(ut,vt,tt,pst,psi,chi,t,ps,update,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_strong_bal_correction_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!   2009-11-27  parrish - add uv_hyb_ens.  if present and true, then
!                          input/output variables psi=u, chi=v.
!
!   input argument list:
!     mype     - mpi task id
!     ut       - 
!     vt       - 
!     tt       - 
!     pst      - 
!     u        - 
!     v        - 
!     t        - 
!     ps       - 
!     update   - 
!
!   output argument list:
!     ut       - 
!     vt       - 
!     tt       - 
!     pst      - 
!     ps       - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lat2,lon2,nsig
    use constants, only: zero
    use mod_vtrans, only: vtrans_ad,vtrans_inv_ad
    use hybrid_ensemble_parameters, only: uv_hyb_ens
    implicit none

!   initially just generate projections and make maps

    real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: ut,vt,tt
    real(r_kind),dimension(lat2,lon2)     ,intent(inout) :: pst
    real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: psi,chi,t
    real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: ps
    logical                               ,intent(in   ) :: update
    integer(i_kind)                       ,intent(in   ) :: mype

    real(r_kind),dimension(lat2,lon2,nvert)::utilde,vtilde,mtilde
    real(r_kind),dimension(ny,mx_0:mx_1)::dhat,zhat,mhat
    real(r_kind),dimension(ny,mx_0:mx_1)::phat2,chat2,mhat2
    real(r_kind),dimension(lat2,lon2,nsig)::dpsi,dchi,dt
    real(r_kind),dimension(lat2,lon2)::dps

    dhat=zero ; zhat=zero ; mhat=zero
    dpsi=zero ; dchi=zero ; dt=zero ; dps=zero
    if(update) then
       dpsi=-psi ; dchi=-chi ; dt=-t ; dps=-ps
    
       utilde=zero ; vtilde=zero ; mtilde=zero
       call vtrans_inv_ad(utilde,vtilde,mtilde,dpsi,dchi,dt,dps)
       phat2=zero ; chat2=zero ; mhat2=zero
       if(uv_hyb_ens) then
          call zrnmi_pcmhat2uvm_ad_orig(utilde,vtilde,mtilde,phat2,chat2,mhat2,mype)
       else
          call zrnmi_pcmhat2uvm_ad(utilde,vtilde,mtilde,phat2,chat2,mhat2,mype)
       end if
!       adjoint of gravity projected corrections
       dhat=p0_sm2*mhat2-f_sm2_am2*phat2
       zhat=f_sm2_am2*chat2
       mhat=sm2*chat2
    end if

!      adjoint of apply mask to input spectral fields to limit periods considered
    dhat=pmask*dhat
    zhat=pmask*zhat
    mhat=pmask*mhat

!      adjoint of transform to spectral space
    utilde=zero ; vtilde=zero ; mtilde=zero
    call zrnmi_uvm2dzmhat_ad(utilde,vtilde,mtilde,dhat,zhat,mhat,mype)

!      vertical mode transform

    call vtrans_ad(ut,vt,tt,pst,utilde,vtilde,mtilde)

  end subroutine zrnmi_strong_bal_correction_ad

  subroutine zrnmi_filter_uvm(utilde,vtilde,mtilde,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    filter low frequency scales from input
!
!   prgrmmr: parrish
!
! abstract:      
!
! program history log:
!   2009-08-28  parrish
!
!   input argument list:
!     mype     - mpi task id
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
!   output argument list:
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: lat2,lon2

!   initially just generate projections and make maps

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: utilde,vtilde,mtilde

    real(r_kind),dimension(ny,mx_0:mx_1)::dhat,zhat,mhat

!      transform to spectral space

    call zrnmi_uvm2dzmhat(utilde,vtilde,mtilde,dhat,zhat,mhat,mype)

!      apply mask to input spectral fields to limit periods considered
    dhat=pmask*dhat*am2
    zhat=pmask*zhat*am2
    mhat=pmask*mhat

!      transform back to grid space
    call zrnmi_pcmhat2uvm_orig(utilde,vtilde,mtilde,zhat,dhat,mhat,mype)

  end subroutine zrnmi_filter_uvm

  subroutine zrnmi_filter_uvm_ad(utilde,vtilde,mtilde,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_filter_uvm_ad
!
!   prgrmmr:  parrish
!
! abstract:      
!
! program history log:
!   2009-08-28  parrish
!
!   input argument list:
!     mype     - mpi task id
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
!   output argument list:
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lat2,lon2
    use constants, only: zero

!   initially just generate projections and make maps

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: utilde,vtilde,mtilde

    real(r_kind),dimension(ny,mx_0:mx_1)::dhat,zhat,mhat

    dhat=zero ; zhat=zero ; mhat=zero
    call zrnmi_pcmhat2uvm_ad_orig(utilde,vtilde,mtilde,zhat,dhat,mhat,mype)

!      adjoint of apply mask to input spectral fields to limit periods considered
    dhat=pmask*dhat*am2
    zhat=pmask*zhat*am2
    mhat=pmask*mhat

!      adjoint of transform to spectral space
    utilde=zero ; vtilde=zero ; mtilde=zero
    call zrnmi_uvm2dzmhat_ad(utilde,vtilde,mtilde,dhat,zhat,mhat,mype)

  end subroutine zrnmi_filter_uvm_ad

  subroutine zrnmi_filter_uvm2(utilde,vtilde,mtilde,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    filter low frequency scales from input
!
!   prgrmmr: parrish
!
! abstract:      
!
! program history log:
!   2009-08-28  parrish
!
!   input argument list:
!     mype     - mpi task id
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
!   output argument list:
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use constants, only: zero
    use gridmod, only: lat2,lon2

!   initially just generate projections and make maps

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: utilde,vtilde,mtilde

    real(r_kind),dimension(ny,mx_0:mx_1)::uhat,vhat,mhat

!      transform to spectral space

    call zrnmi_uvm2uvmhat(utilde,vtilde,mtilde,uhat,vhat,mhat,mype)

!      apply mask to input spectral fields to limit periods considered
    uhat=pmask*uhat
    vhat=pmask*vhat
    mhat=pmask*mhat

!      transform back to grid space
    call zrnmi_uvmhat2uvm(utilde,vtilde,mtilde,uhat,vhat,mhat,mype)

  end subroutine zrnmi_filter_uvm2

  subroutine zrnmi_filter_uvm2_ad(utilde,vtilde,mtilde,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_filter_uvm2_ad
!
!   prgrmmr:  parrish
!
! abstract:      
!
! program history log:
!   2009-08-28  parrish
!
!   input argument list:
!     mype     - mpi task id
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
!   output argument list:
!     utilde   - 
!     vtilde   - 
!     mtilde   - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lat2,lon2
    use constants, only: zero
    implicit none

!   initially just generate projections and make maps

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: utilde,vtilde,mtilde

    real(r_kind),dimension(ny,mx_0:mx_1)::uhat,vhat,mhat

    uhat=zero ; vhat=zero ; mhat=zero
    call zrnmi_uvmhat2uvm_ad(utilde,vtilde,mtilde,uhat,vhat,mhat,mype)

!      adjoint of apply mask to input spectral fields to limit periods considered
    uhat=pmask*uhat
    vhat=pmask*vhat
    mhat=pmask*mhat

!      adjoint of transform to spectral space
    utilde=zero ; vtilde=zero ; mtilde=zero
    call zrnmi_uvm2uvmhat_ad(utilde,vtilde,mtilde,uhat,vhat,mhat,mype)

  end subroutine zrnmi_filter_uvm2_ad
 
  subroutine zrnmi_uvm2uvmhat(u,v,m,uhat,vhat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uvm2uvmhat
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     u        - 
!     v        - 
!     m        - 
!
!   output argument list:
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(in   ) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(  out) :: uhat,vhat,mhat

    real(r_kind),dimension(nx,ny_0:ny_1):: u_x,v_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: u4_x,v4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: u_y,v_y,m_y

    call zrnmi_sd2x3(u,v,m,u_x,v_x,m_x,mype)
    call zrnmi_x_strans(u_x,u4_x)
    call zrnmi_x_strans(v_x,v4_x)
    call zrnmi_x_strans(m_x,m4_x)
    call zrnmi_x2y3(u4_x,v4_x,m4_x,u_y,v_y,m_y,mype)
    call zrnmi_y_strans(u_y,uhat)
    call zrnmi_y_strans(v_y,vhat)
    call zrnmi_y_strans(m_y,mhat)

  end subroutine zrnmi_uvm2uvmhat

  subroutine zrnmi_uvm2uvmhat_ad(u,v,m,uhat,vhat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uvm2uvmhat_ad
!
!   prgrmmr: 
!
! abstract:      
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
!   output argument list:
!     u        - 
!     v        - 
!     m        - 
!     dhat     - 
!     zhat     - 
!     mhat     - 
!
! attributes:
!   language:  f90
!   machine:   
!
!$$$

    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(inout) :: uhat,vhat,mhat

    real(r_kind),dimension(nx,ny_0:ny_1):: u_x,v_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: u4_x,v4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: u_y,v_y,m_y

    call zrnmi_y_strans(mhat,m_y)
    call zrnmi_y_strans(vhat,v_y)
    call zrnmi_y_strans(uhat,u_y)
    call zrnmi_y2x3(u4_x,v4_x,m4_x,u_y,v_y,m_y,mype)
    call zrnmi_x_strans(m4_x,m_x)
    call zrnmi_x_strans(v4_x,v_x)
    call zrnmi_x_strans(u4_x,u_x)
    call zrnmi_x2sd3(u,v,m,u_x,v_x,m_x,mype)

  end subroutine zrnmi_uvm2uvmhat_ad

  subroutine zrnmi_uvmhat2uvm(u,v,m,uhat,vhat,mhat,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zrnmi_uvmhat2uvm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     phat     -
!     chat     -
!     mhat     -
!
!   output argument list:
!     p        -
!     c        -
!     m        -
!     phat     -
!     chat     -
!     mhat     -
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(  out) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(inout) :: uhat,vhat,mhat

    real(r_kind),dimension(nx,ny_0:ny_1):: u_x,v_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: u4_x,v4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: u_y,v_y,m_y

    call zrnmi_y_strans(uhat,u_y)
    call zrnmi_y_strans(vhat,v_y)
    call zrnmi_y_strans(mhat,m_y)
    call zrnmi_y2x3(u4_x,v4_x,m4_x,u_y,v_y,m_y,mype)
    call zrnmi_x_strans(u4_x,u_x)
    call zrnmi_x_strans(v4_x,v_x)
    call zrnmi_x_strans(m4_x,m_x)
    call zrnmi_x2sd3(u,v,m,u_x,v_x,m_x,mype)

  end subroutine zrnmi_uvmhat2uvm

  subroutine zrnmi_uvmhat2uvm_ad(u,v,m,uhat,vhat,mhat,mype)
!$$$  subprogram doucmentation block
!                .      .    .                                       .
! subprogram:    zrnmi_uvmhat2uvm_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-01-07  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!    u,v,m
!
!   output argument list:
!    uhat,vhat,mhat
!    u,v,m
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use constants, only: zero
    use gridmod, only: lon2,lat2
    implicit none

    integer(i_kind)                        ,intent(in   ) :: mype
    real(r_kind),dimension(lat2,lon2,nvert),intent(inout) :: u,v,m
    real(r_kind),dimension(ny,mx_0:mx_1)   ,intent(  out) :: uhat,vhat,mhat

    real(r_kind),dimension(nx,ny_0:ny_1):: u_x,v_x,m_x
    real(r_kind),dimension(nx,ny_0:ny_1):: u4_x,v4_x,m4_x
    real(r_kind),dimension(ny,mx_0:mx_1):: u_y,v_y,m_y

    call zrnmi_sd2x3(u,v,m,u_x,v_x,m_x,mype)
    call zrnmi_x_strans(u_x,u4_x)
    call zrnmi_x_strans(v_x,v4_x)
    call zrnmi_x_strans(m_x,m4_x)
    call zrnmi_x2y3(u4_x,v4_x,m4_x,u_y,v_y,m_y,mype)
    call zrnmi_y_strans(u_y,uhat)
    call zrnmi_y_strans(v_y,vhat)
    call zrnmi_y_strans(m_y,mhat)

  end subroutine zrnmi_uvmhat2uvm_ad

end module zrnmi_mod
