module raflib
!$$$ module documentation block
!           .      .    .                                       .
! module:   raflib           contains anisotropic filter routines
!   prgmmr: parrish          org: np22                date: 2005-11-30
!
! abstract: contains routines for initializing and applying anisotropic
!             recursive filter.
!
! program history log:
!   2005-11-30  parrish
!   2005-11-30  parrish -- replace blended triad routine gettri4 with
!                          newer version provided by Jim Purser.
!   2011-07-04  todling - fix mpi call to run either single or double prec
!
! subroutines included:
!   sub set_indices                   -
!   sub init_raf4_wrap                -
!   sub raf4_ad_wrap                  -
!   sub raf_sm4_ad_wrap               -
!   sub raf4_wrap                     -
!   sub raf_sm4_wrap                  -
!   sub adjoint_check4                -
!   sub raf4_ad                       -
!   sub raf_sm4_ad                    -
!   sub rad_sm24_ad                   -
!   sub alpha_beta4                   -
!   sub alpha_betaa4                  -
!   sub count_strings                 -
!   sub gethex                        -
!   sub indexxi4                      -
!   sub indexxi8                      -
!   sub init_raf4                     -
!   sub normalize2_raf4               -
!   sub one_color4                    -
!   sub one_color24                   -
!   sub raf4                          -
!   sub raf_sm4                       -
!   sub sort_strings4                 -
!   sub string_assemble4              -
!   sub string_label                  -
!   sub my_gatherv8                   -
!   sub my_scatterv8                  -
!   sub what_color_is                 -
!   sub add_halox0                    -
!   sub add_haloy0                    -
!   sub add_halo_x                    -
!   sub add_halo_y                    -
!   sub one_color4_new_factorization  -
!   sub one_color24_new_factorization -
!   sub new_alpha_beta4               -
!   sub new_alpha_betaa4              -
!   sub stringop                      -
!   sub quadfil                       -
!
! Variable Type Definition:
!   def filter_cons              - structure variable containing information
!                                  for anisotropic filter--created by init_raf4
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds,only: r_double,r_quad,r_single,i_byte,i_long,i_llong,i_short
use mpimod,only: mpi_comm_world,mpi_integer,mpi_integer1,mpi_integer2,mpi_integer4, &
              mpi_integer8,mpi_max,mpi_min,mpi_real4,mpi_real8,mpi_real16,mpi_sum
use constants, only: zero,half,one,two,zero_quad,zero_single
use gsi_io, only: verbose

implicit none

! set default to private
  private
! set subroutines to public
  public :: set_indices
  public :: init_raf4_wrap
  public :: raf4_ad_wrap
  public :: raf_sm4_ad_wrap
  public :: raf4_wrap
  public :: raf_sm4_wrap
  public :: adjoint_check4
  public :: raf4_ad
  public :: raf_sm4_ad
  public :: rad_sm24_ad
  public :: alpha_beta4
  public :: alpha_betaa4
  public :: count_strings
  public :: gethex
  public :: indexxi4
  public :: indexxi8
  public :: init_raf4
  public :: normalize2_raf4
  public :: one_color4
  public :: one_color24
  public :: raf4
  public :: raf_sm4
  public :: sort_strings4
  public :: string_assemble4
  public :: string_label
  public :: my_gatherv8
  public :: my_scatterv8
  public :: what_color_is
  public :: add_halox0
  public :: add_haloy0
  public :: add_halo_x
  public :: add_halo_y
  public :: one_color4_new_factorization
  public :: one_color24_new_factorization
  public :: new_alpha_beta4
  public :: new_alpha_betaa4
  public :: stringop
  public :: quadfil
! set passed variables to public
  public :: filter_cons,filter_indices

!  declare type structure for recursive anisotropic filter constants

         type filter_cons

            sequence
            logical         new_factorization
            integer(i_long) ngauss
            integer(i_long) npass
            integer(i_long) ifilt_ord
            integer(i_long) npointsmaxall
            integer(i_long) npointsmax
            integer(i_long) npoints_send
            integer(i_long) npoints_recv
            integer(i_long) nstrings
            integer(i_long) nsmooth
            integer(i_long) nsmooth_shapiro
            integer(i_long) nrows
            integer(i_long) nsend_halox_loc
            integer(i_long) nrecv_halox_loc
            integer(i_long) nsend_haloy_loc
            integer(i_long) nrecv_haloy_loc
            integer(i_long),pointer:: nrecv_halox(:) => NULL()
            integer(i_long),pointer:: ndrecv_halox(:) => NULL()
            integer(i_long),pointer:: nsend_halox(:) => NULL()
            integer(i_long),pointer:: ndsend_halox(:) => NULL()
            integer(i_long),pointer:: info_send_halox(:,:) => NULL()
            integer(i_long),pointer:: info_recv_halox(:,:) => NULL()
            integer(i_long),pointer:: nrecv_haloy(:) => NULL()
            integer(i_long),pointer:: ndrecv_haloy(:) => NULL()
            integer(i_long),pointer:: nsend_haloy(:) => NULL()
            integer(i_long),pointer:: ndsend_haloy(:) => NULL()
            integer(i_long),pointer:: info_send_haloy(:,:) => NULL()
            integer(i_long),pointer:: info_recv_haloy(:,:) => NULL()
            integer(i_long),pointer::istart(:) => NULL()
            integer(i_long),pointer::ib(:) => NULL()
            integer(i_long),pointer::nrecv(:) => NULL()
            integer(i_long),pointer::ndrecv(:) => NULL()
            integer(i_long),pointer::nsend(:) => NULL()
            integer(i_long),pointer::ndsend(:) => NULL()
            real(r_single),pointer:: gnorm_halox(:) => NULL()
            real(r_single),pointer:: gnorm_haloy(:) => NULL()
            real(r_single),pointer::lnf(:,:,:,:) => NULL()
            real(r_single),pointer::bnf(:,:,:) => NULL()
            real(r_single),pointer::fmat(:,:,:,:,:) => NULL()
            real(r_single),pointer::fmat0(:,:,:,:) => NULL()
            real(r_single),pointer::amp(:,:,:,:) => NULL()
            integer(i_short),pointer::ia(:) => NULL()
            integer(i_short),pointer::ja(:) => NULL()
            integer(i_short),pointer::ka(:) => NULL()

         end type filter_cons

!---------------------------------------------------------------
! Type valiable to simplify the subroutine interfaces
!---------------------------------------------------------------
         type filter_indices
            integer(i_long)::ids, ide, jds, jde, kds, kde, &   ! domain indices
                             ips, ipe, jps, jpe, kps, kpe      ! patch indices
         end type filter_indices

contains

subroutine set_indices(indices, &
                       ids, ide, jds, jde, kds, kde, &
                       ips, ipe, jps, jpe, kps, kpe  )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    set_indices
!
!   prgmmr: sato
!
! abstract:  initialize type_indices
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!     ids, ide, jds, jde, kds, kde      - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!
!   output argument list:
!     indices - type variable which contains all indices
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_indices),intent(  out) :: indices
  integer(i_long)     ,intent(in   ) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                                        ips, ipe, jps, jpe, kps, kpe      ! patch indices
  indices%ids=ids; indices%ide=ide
  indices%jds=jds; indices%jde=jde
  indices%kds=kds; indices%kde=kde
  indices%ips=ips; indices%ipe=ipe
  indices%jps=jps; indices%jpe=jpe
  indices%kps=kps; indices%kpe=kpe

end subroutine set_indices

subroutine init_raf4_wrap(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter, &
                          nsmooth,nsmooth_shapiro, &
                          nvars,idvar,kvar_start,kvar_end,var_names,idx,mype,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_raf4_wrap
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-25  lueken - added subprogram doc block
!
!   input argument list:
!    triad4,binom
!    ngauss,normal,npass,ifilt_ord,nvars
!    rguass
!    idvar
!    kvar_start
!    kvar_end
!    var_names
!    mype,npes
!    nsmooth
!    nsmooth_shapiro
!    filter
!    idx
!    aspect
!
!   output argument list:
!    filter
!    idx
!    nsmooth
!    aspect
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_cons),   intent(inout) :: filter(7)
  type(filter_indices),intent(inout) :: idx

  real(r_single),      intent(inout) :: aspect(7, &
                          idx%ips:idx%ipe, &
                          idx%jps:idx%jpe, &
                          idx%kps:idx%kpe)

  logical,             intent(in   ) :: triad4,binom
  integer(i_long),     intent(in   ) :: ngauss,normal,npass,ifilt_ord,nvars
  real(r_double),      intent(in   ) :: rgauss(ngauss)
  integer(i_long),     intent(in   ) :: idvar(idx%kds:idx%kde)
  integer(i_long),     intent(in   ) :: kvar_start(nvars)
  integer(i_long),     intent(in   ) :: kvar_end(nvars)
  character(len=80),   intent(in   ) :: var_names(nvars)
  integer(i_long),     intent(in   ) :: mype, npes

  integer(i_long),     intent(inout) :: nsmooth
  integer(i_long),     intent(in   ) :: nsmooth_shapiro   !  number of 2nd moment preserving shapiro smoothers

  call init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter, &
                 nsmooth,nsmooth_shapiro, &
                 nvars,idvar,kvar_start,kvar_end,var_names, &
                 idx%ids, idx%ide, idx%jds, idx%jde, idx%kds, idx%kde, & ! domain idx
                 idx%ips, idx%ipe, idx%jps, idx%jpe, idx%kps, idx%kpe, & ! patch idx
                 mype, npes)

end subroutine init_raf4_wrap

subroutine raf4_ad_wrap(g,filter,ngauss,idx,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4_ad_wrap
!
!   prgrmmr: sato
!
! abstract:  interface for raf4_ad with type_indices
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!    filter
!    idx
!    ngauss
!    npes
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_cons)   ,intent(in   ) :: filter(7)
  type(filter_indices),intent(in   ) :: idx
  integer(i_long)     ,intent(in   ) :: ngauss, npes

  real(r_single)      ,intent(  out) :: g( ngauss, &
                              idx%ips:idx%ipe, &
                              idx%jps:idx%jpe, &
                              idx%kps:idx%kpe )

  call raf4_ad(g,filter,ngauss, &
               idx%ids, idx%ide, idx%jds, idx%jde, & ! domain idx
               idx%ips, idx%ipe, idx%jps, idx%jpe, idx%kps, idx%kpe, & ! patch idx
               npes)

end subroutine raf4_ad_wrap


subroutine raf_sm4_ad_wrap(g,filter,ngauss,idx,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4_ad_wrap
!
!   prgrmmr: sato
!
! abstract:  interface for raf4_ad_sm4 with type_indices
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!     filter
!     idx
!     ngauss
!     npes
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_cons)   ,intent(in   ) :: filter(7)
  type(filter_indices),intent(in   ) :: idx
  integer(i_long)     ,intent(in   ) :: ngauss, npes

  real(r_single)      ,intent(  out) :: g( ngauss, &
                              idx%ips:idx%ipe, &
                              idx%jps:idx%jpe, &
                              idx%kps:idx%kpe )

  call raf_sm4_ad(g,filter,ngauss, &
                  idx%ips, idx%ipe, idx%jps, idx%jpe, idx%kps, idx%kpe, & ! patch idx
                  npes)

end subroutine raf_sm4_ad_wrap

subroutine raf4_wrap(g,filter,ngauss,idx,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4_wrap
!
!   prgrmmr: sato
!
! abstract:  interface for raf4 with type_indices
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!     filter
!     idx
!     ngauss
!     npes
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_cons)   ,intent(in   ) :: filter(7)
  type(filter_indices),intent(in   ) :: idx
  integer(i_long)     ,intent(in   ) :: ngauss, npes

  real(r_single)      ,intent(  out) :: g( ngauss, &
                              idx%ips:idx%ipe, &
                              idx%jps:idx%jpe, &
                              idx%kps:idx%kpe )

  call raf4(g,filter,ngauss, &
            idx%ids, idx%ide, idx%jds, idx%jde, & ! domain idx
            idx%ips, idx%ipe, idx%jps, idx%jpe, idx%kps, idx%kpe, & ! patch idx
            npes)

end subroutine raf4_wrap


subroutine raf_sm4_wrap(g,filter,ngauss,idx,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf_sm4_wrap
!
!   prgrmmr: sato
!
! abstract:  interface for raf_sm4 with type_indices
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!     filter
!     idx
!     ngauss
!     npes
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  type(filter_cons)   ,intent(in   ) :: filter(7)
  type(filter_indices),intent(in   ) :: idx
  integer(i_long)     ,intent(in   ) :: ngauss, npes

  real(r_single)      ,intent(  out) :: g( ngauss, &
                              idx%ips:idx%ipe, &
                              idx%jps:idx%jpe, &
                              idx%kps:idx%kpe )

  call raf_sm4(g,filter,ngauss, &
               idx%ips, idx%ipe, idx%jps, idx%jpe, idx%kps, idx%kpe, & ! patch idx
               npes)

end subroutine raf_sm4_wrap


subroutine adjoint_check4(filter,ngauss,ips,ipe,jps,jpe,kps,kpe,mype,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    adjoint_check4
!
!   prgrmmr:
!
! abstract:  test that filter is symmetric
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     mype                              - mpi task id
!     npes                              - total num of mpi tasks
!     ngauss                            - num of Gaussian
!     filter
!
!   output argument list:
!     filter
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block
  implicit none


  INTEGER(i_long)  ,INTENT(IN   ) :: ips,ipe,jps,jpe,kps,kpe
  INTEGER(i_long)  ,INTENT(IN   ) :: mype,npes,ngauss

  TYPE(filter_cons),intent(inout) :: filter(7)            ! structure defining recursive filter

  real(r_single) xvec( ngauss,ips:ipe, jps:jpe, kps:kpe )
  real(r_single) yvec( ngauss,ips:ipe, jps:jpe, kps:kpe )
  real(r_single) zvec( ngauss,ips:ipe, jps:jpe, kps:kpe )

  integer(i_long) i,igauss,j,k
  real(r_quad) yty,xtz,one_quad
  real(r_quad) yty0(npes),xtz0(npes)
  integer(i_long) ierror

  one_quad=zero_quad
  xvec=zero_single
  yvec=zero_single
  zvec=zero_single
  call random_number(xvec)
  yvec=xvec
  call raf_sm4(yvec,filter,ngauss,ips,ipe,jps,jpe,kps,kpe,npes)
  zvec=yvec
  call raf_sm4_ad(zvec,filter,ngauss,ips,ipe,jps,jpe,kps,kpe,npes)

  yty=zero_quad
  xtz=zero_quad
  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              yty=yty+one_quad*yvec(igauss,i,j,k)**2
              xtz=xtz+one_quad*xvec(igauss,i,j,k)*zvec(igauss,i,j,k)
           end do
        end do
     end do
  end do
  if(r_double==r_quad) then
     call mpi_gather(yty,1,mpi_real8 ,yty0,1,mpi_real8 ,0,mpi_comm_world,ierror)
     call mpi_gather(xtz,1,mpi_real8 ,xtz0,1,mpi_real8 ,0,mpi_comm_world,ierror)
  else
     call mpi_gather(yty,1,mpi_real16,yty0,1,mpi_real16,0,mpi_comm_world,ierror)
     call mpi_gather(xtz,1,mpi_real16,xtz0,1,mpi_real16,0,mpi_comm_world,ierror)
  endif
  if(mype==0) then
     yty=zero_quad
     xtz=zero_quad
     do i=1,npes
        yty=yty+yty0(i)
        xtz=xtz+xtz0(i)
     end do
     write(6,*)' adjoint check for raf,ad_raf, yty,xtz=',yty,xtz
  end if

end subroutine adjoint_check4

SUBROUTINE raf4_ad(g,filter,ngauss,ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4_ad
!
!   prgrmmr:
!
! abstract:  2nd half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde                - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     ngauss                            - num of Gaussian
!     npes                              - total num of mpi tasks
!     g
!     filter
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: ids, ide, jds, jde, &
            ips, ipe, jps, jpe, kps, kpe

  INTEGER(i_long)                                              ,INTENT(IN   ) :: ngauss,npes

  real(r_single), DIMENSION( ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons)                                            ,intent(in   ) :: filter(7)       ! structure defining recursive filter

  integer(i_long) i,icolor,igauss,ipass,j,k,iadvance,iback

  if(filter(1)%npass>0) then
     do ipass=filter(1)%npass,1,-1

        do icolor=1,7

           if(filter(icolor)%npointsmaxall>0) then
              if(filter(1)%new_factorization) then
                 iadvance=2_i_long
                 iback=1
                 call one_color4_new_factorization(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart, &
                     ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
              else
                 call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart,ips,ipe,jps,jpe,kps,kpe,npes)
              end if
           end if

        end do

     end do
  end if

!    apply smoothing if nsmooth or nsmooth_shapiro > 0
  if(kps<=kpe) then
     do i=1,max(filter(1)%nsmooth,filter(1)%nsmooth_shapiro)
        call smther(filter(1),g,filter(1)%nrows,ngauss,filter(1)%nsmooth,filter(1)%nsmooth_shapiro, &
                    ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe, &
                    npes,filter(1)%gnorm_halox,filter(1)%gnorm_haloy,.true.)
     end do
  end if

!   finally multiply by amp

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              g(igauss,i,j,k)=filter(1)%amp(igauss,i,j,k)*g(igauss,i,j,k)
           end do
        end do
     end do
  end do

end subroutine raf4_ad

SUBROUTINE raf_sm4_ad(g,filter,ngauss,ips,ipe,jps,jpe,kps,kpe,npes)
!
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf_sm4_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     npes                              -
!     ngauss                            -
!     g
!     filter
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
            ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                                              ,INTENT(IN   ) :: npes,ngauss

  real(r_single), DIMENSION( ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons)                                            ,intent(in   ) :: filter(7)            ! structure defining recursive filter

  integer(i_long) icolor,ipass,iadvance,iback

  if(filter(1)%npass>0) then
     do ipass=filter(1)%npass,1,-1

        do icolor=1,7

           if(filter(icolor)%npointsmaxall>0) then
              if(filter(1)%new_factorization) then
                 iadvance=2_i_long
                 iback=1
                 call one_color4_new_factorization(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart, &
                     ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
              else
                 call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart,ips,ipe,jps,jpe,kps,kpe,npes)
              end if
           end if

        end do

     end do
  end if

end subroutine raf_sm4_ad

SUBROUTINE rad_sm24_ad(g,filter,ngauss,ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    rad_sm24_ad
!
!   prgrmmr:
!
! abstract:     do two fields at same time, each real(4)
!  2nd half of recursive anisotropic self-adjoint filter (full-strings version)
!        (no amplitude factor--used only for local weighted averages)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     ids, ide, jds, jde                - domain indices
!     ips, ipe, jps, jpe, kps, kpe      - patch indices
!     npes                              -
!     ngauss                            -
!     g
!     filter
!
!   output argument list:
!     g
!
! attributes:
!   language:  f90
!   machine:
!
!$$$

  implicit none

  INTEGER(i_long)                                               ,INTENT(IN   ) :: &
            ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe

  INTEGER(i_long)                                               ,INTENT(IN   ) :: npes,ngauss

  real(r_single), DIMENSION(2,ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons)                                             ,intent(in   ) :: filter(7)            ! structure defining recursive filter

  integer(i_long) icolor,ipass,i,ii,j,k,kk,n,iadvance,iback
  real(r_single) gwork(ngauss,ips:ipe,jps:jpe,kps:kpe)

  if(filter(1)%npass>0) then
     do ipass=filter(1)%npass,1,-1

        do icolor=1,7

           if(filter(icolor)%npointsmaxall>0) then
              if(filter(1)%new_factorization) then
                 iadvance=2_i_long
                 iback=1
                 call one_color24_new_factorization(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart, &
                     ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
              else
                 call one_color24(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart,ips,ipe,jps,jpe,kps,kpe,npes)
              end if
           end if

        end do

     end do
  end if

!    apply smoothing if nsmooth or nsmooth_shapiro > 0
  if(max(filter(1)%nsmooth,filter(1)%nsmooth_shapiro) > 0.and.kps<=kpe) then
     do kk=1,2
        do k=kps,kpe
           do j=jps,jpe
              do i=ips,ipe
                 do n=1,ngauss
                    gwork(n,i,j,k)=g(kk,n,i,j,k)
                 end do
              end do
           end do
        end do
        do ii=1,max(filter(1)%nsmooth,filter(1)%nsmooth_shapiro)
           call smther(filter(1),gwork,filter(1)%nrows,ngauss,filter(1)%nsmooth,filter(1)%nsmooth_shapiro, &
                       ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe, &
                       npes,filter(1)%gnorm_halox,filter(1)%gnorm_haloy,.true.)
        end do
        do k=kps,kpe
           do j=jps,jpe
              do i=ips,ipe
                 do n=1,ngauss
                    g(kk,n,i,j,k)=gwork(n,i,j,k)
                 end do
              end do
           end do
        end do
     end do
  end if

end subroutine rad_sm24_ad

subroutine alpha_beta4(info_string,aspect_full,rgauss,lnf,bnf,igauss,ngauss, &
                     istart_out,npoints_mype,binomial,npass,ifilt_ord, &
                     lenbar,lenmax,lenmin,npoints1,nvars)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    alpha_beta4
!
!   prgrmmr:
!
! abstract:   compute recursion constants alpha and beta along unbroken strings
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     npoints_mype,npass,ifilt_ord
!     binomial
!     info_string
!     aspect_full
!     igauss,ngauss
!     rgauss
!     lnf,bnf
!     nvars
!
!   output argument list:
!     istart_out
!     lenmax,lenmin,npoints1
!     lenbar
!     lnf,bnf
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long)                               ,INTENT(IN   ) :: nvars

  INTEGER(i_long)                               ,INTENT(IN   ) :: npoints_mype,npass,ifilt_ord

  REAL(r_double)  , DIMENSION( 10, 10 )         ,INTENT(IN   ) :: &
            binomial

  INTEGER(i_short), DIMENSION( 8, npoints_mype ),INTENT(IN   ) :: &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single)  , DIMENSION( npoints_mype )   ,INTENT(IN   ) :: &
            aspect_full
  integer(i_long)                               ,intent(in   ) :: igauss,ngauss
  real(r_double)                                ,intent(in   ) :: rgauss
  real(r_single)                                ,intent(inout) :: &
                             lnf(ifilt_ord,npoints_mype,npass,ngauss),bnf(npoints_mype,npass,ngauss)
  integer(i_long)                               ,intent(  out) :: istart_out(*)
  integer(i_long)                               ,intent(  out) :: lenmax(nvars),lenmin(nvars),&
                             npoints1(nvars) !  diagnostic output--to look at string
  real(r_double)                                ,intent(  out) :: lenbar(nvars)

  integer(i_long) i,iend,ipass,istart,ivar
  integer(i_long) nstrings

  nstrings=0

  istart=1
  if(npoints_mype>1) then
     do i=2,npoints_mype
        if(info_string(1,i)/=info_string(1,i-1)+1.or. &
               info_string(2,i)/=info_string(2,i-1).or. &
                   info_string(3,i)/=info_string(3,i-1).or. &
                       info_string(4,i)/=info_string(4,i-1).or. &
                           info_string(5,i)/=info_string(5,i-1).or. &
                               info_string(6,i)/=info_string(6,i-1).or. &
                                   info_string(7,i)/=info_string(7,i-1).or. &
                                       info_string(8,i)/=info_string(8,i-1)) then
           iend=i-1
           if(igauss==1) then
              ivar=info_string(8,iend)
              lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
              lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
              lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
              if(iend==istart) npoints1(ivar)=npoints1(ivar)+1
           end if
           nstrings=nstrings+1
           istart_out(nstrings)=istart
           istart_out(nstrings+1)=iend+1

           do ipass=1,npass
              call alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                             lnf(1,istart,ipass,igauss),bnf(istart,ipass,igauss),ifilt_ord)
           end do

           istart=iend+1
        end if
     end do
  end if
  iend=npoints_mype
  if(igauss==1) then
     ivar=info_string(8,iend)
     lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
     lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
     lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
     if(iend==istart) npoints1(ivar)=npoints1(ivar)+1
  end if
  nstrings=nstrings+1
  istart_out(nstrings)=istart
  istart_out(nstrings+1)=iend+1
  do ipass=1,npass

     call alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                    lnf(1,istart,ipass,igauss),bnf(istart,ipass,igauss),ifilt_ord)
  end do

end subroutine alpha_beta4

subroutine alpha_betaa4(aspect,rgauss,ng,binomial,lnf,bnf,m)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    alpha_betaa4
!
!   prgrmmr:
!
! abstract:  compute various constants for Purser 1-d high-order filter
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     aspect   - correlation scale (squared, i think), grid units
!     rgauss   - multiplier of aspect, for multiple gaussians--used for fat-tail filter
!     ng       - length of string
!     binomial - weighting factors (perhaps not needed with high-order filter)
!     m        - filter order
!
!   output argument list:
!     lnf,bnf  - filter parameters
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long)                ,INTENT(IN   ) :: ng,m

  REAL(r_double)                 ,INTENT(IN   ) :: binomial

  real(r_single), DIMENSION( ng ),INTENT(IN   ) :: aspect
  real(r_double)                 ,intent(in   ) :: rgauss
  real(r_single)                 ,intent(  out) :: lnf(m,ng),bnf(ng)

  real(r_double) sig(ng),snu(ng)
  real(r_double) lnf8(m,ng),bnf8(ng)
  integer(i_long) i,j

  do i=1,ng
     sig(i)=sqrt(rgauss*aspect(i)*binomial)
     snu(i)=one
  end do
  call coefrf(sig,snu,ng,m,bnf8,lnf8)
  do i=1,ng
     bnf(i)=bnf8(i)
     do j=1,m
        lnf(j,i)=lnf8(j,i)
     end do
  end do

end subroutine alpha_betaa4

subroutine count_strings(info_string,nstrings,nstrings_var,nvars,npoints_mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    count_strings
!
!   prgrmmr:
!
! abstract:  compute recursion constants alpha and beta along unbroken strings
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     info_string            1       - distance from origin to current point
!                            2,3,4   - origin coordinates
!                            5,6,7,8 - jumpx,jumpy,jumpz,ivar for this string
!     nvars                          -
!     npoints_mype                   -
!
!   output argument list:
!     nstrings,nstrings_var
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long)                               ,INTENT(IN   ) :: npoints_mype,nvars

  INTEGER(i_short), DIMENSION( 8, npoints_mype ),INTENT(IN   ) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  integer(i_long)                               ,intent(  out) :: nstrings,nstrings_var(nvars)

  integer(i_long) i,iend,istart,ivar

  nstrings=0

  istart=1
  if(npoints_mype>1) then
     do i=2,npoints_mype
        if(info_string(1,i)/=info_string(1,i-1)+1.or. &
               info_string(2,i)/=info_string(2,i-1).or. &
                   info_string(3,i)/=info_string(3,i-1).or. &
                       info_string(4,i)/=info_string(4,i-1).or. &
                           info_string(5,i)/=info_string(5,i-1).or. &
                               info_string(6,i)/=info_string(6,i-1).or. &
                                   info_string(7,i)/=info_string(7,i-1).or. &
                                       info_string(8,i)/=info_string(8,i-1)) then
           iend=i-1
           nstrings=nstrings+1
           ivar=info_string(8,iend)
           nstrings_var(ivar)=nstrings_var(ivar)+1
           istart=iend+1
        end if
     end do
     iend=npoints_mype
     nstrings=nstrings+1
     ivar=info_string(8,iend)
     nstrings_var(ivar)=nstrings_var(ivar)+1
  end if

  return
end subroutine count_strings


SUBROUTINE GETHEX(UTARGET,LGUESS,LHEXAD,LUI,WHEXAD,KT)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    GETHEX
!
!   prgrmmr:     Purser 1997
!
! abstract:
!    Apply implicit lattice transformations until the target tensor UTARGET
!  can be represented as a positive combination of the components of a
! "canonical hexad", with coefficients which may each be interpreted as the
! grid-unit "spread" component in the associated generalized grid direction.
! The target tensor is given in basic grid units. If "LBASIS" is
! a triplet of integer 3-vectors, collectively of determinant = +4, and
! with the all Nth component of these vectors either odd or even, the convex
! hull of the six vectors, {LBASIS and -LBASIS}, forms an octahedron. The
! midpoints of the 12 edges form the 2 diametricaly opposite pairs of sets,
! each consisting of 6 distinct integer vectors, LHEXAD, which are the
! hexad of generalized grid steps along which the application of appropriately
! weighted smoothers will result in the target spread. The requisite weights,
! WHEXAD, are the spreads in the individual hexad directions when expressed
! in the natural grid-units of each of these 6 directions. These weights are
! such that the matrix LU multiplied by WHEXAD gives UTARGET, where the Jth
! column of LU is 6-vector representation of the degenerate tensor describing
! a spread in the direction of hexad-J of one unit of the grid spacing in this
! direction. matrix LUI is the transpose of the inverse of LU.
!
! HOW IT WORKS:
!   For the given target tensor, the hexad and weights are defined iteratively.
! A valid "guess" for LHEXAD, LU, LUI, must first be provided. If these are
! not provided by the user, just set the flag LGUESS to 0, and the routine
! will provide feasible default values; otherwise set LGUESS > 0.
!      First let us suppose the given hexad and accompanying LU, LUI are valid.
! Then we may compute the corresponding weights:
!                WHEXAD = (LUI-transpose)*UTARGET
! If the hexad really IS valid, these weights will all be non-negative and
! the task is done. But if some weight is negative, then the hexad is invalid
! and an alternative valid hexad must be sought. In this situation, the
! algorithm first determines the MOST negative weight and its associated
! grid direction and, keeping the other five directions the same, replaces
! the offending one with the unique (up to sign change) alternative such
! that the NEW hexad are also the midpoints of some grid-octahedron formed from
! an integer-vector basis of determinant = 4 and of the required form.
! To preserve algorithmic symmetry, the enumeration of the new grid directions
! undergoes a permutation. Since only one column of LU changes, the work needed
! to update LUI is less that the work needed to compute the new inverse from
! scratch (cf the "simplex method" of linear programming). We can also exploit
! the special property of this problem that the determinant of LU changes from
! +1 to -1 when the offending column is replaced by its alternative. The
! change in the enumeration of the vectors LHEXAD and of the columns of LU and
! LUI can be regarded as a way to preserve the pattern of implied geometrical
! relationships among these vectors, so that the algorithm is relatively simple.
!    By repeating this step, the algorithm eventually homes in on the
! uniquely valid hexad.
!
!   The "cuboctahedron" associated with the default guess basis is shown in 3
!   orthogonal views below, hexad vector shown thus, (); basis vectors, [].
!
!
!      (6)--------(3)
!       |          |  \         (In each view, nearest facet is speckled)
!       |   [1]    |    \[3]
!       |          |      \          <===== top view
!       |          |        \
!      (-4)-------(5)--------(1)                    south view
!         \::::::::|          |                    //
!           \::::::|          |                   //
!         [-3]\::::|     [2]  |                  //         east view
!               \::|          |                 //              ||
!                 (2)--------(-6)              //               ||
!                                             //                \/
!      (-4)------ (2)                        //      (2)------- (5)
!       |          |::\                     //        |          |  \
!       |          |::::\[2]               //         |          |    \
!       |          |::::::\         <=====//          |          |      \
!       |          |::::::::\                         |          |        \
!      (-1)------(-3)--------(-6)                   (-6)--------(1)--------(3)
!         \        |          |                         \::::::::|          |
!           \      |          |                           \::::::|          |
!             \    |          |                             \::::|          |
!               \  |          |                               \::|          |
!                 (-5)-------(4)                                (4)-------(-2)
!
!
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     UTARGET    - 6-vector comprising components of the target aspect tensor
!     LGUESS     - Code to tell whether input LHEXAD are feasible (LGUESS.NE.0)
!     LHEXAD     - 6 integer basis vectors giving the canonical grid steps
!     LUI        - 6 6-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!
!   output argument list:
!     LHEXAD     - 6 integer basis vectors giving the canonical grid steps
!     LUI        - 6 6-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!     WHEXAD     - 6 real "spread" components in generalized grid-step units
!     KT         - The number of iterations it required to find the valid hexad
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  real(r_double) ,intent(in   ) :: utarget(6)
  integer(i_long),intent(in   ) :: lguess
  real(r_double) ,intent(  out) :: whexad(6)
  integer(i_long),intent(inout) :: lhexad(3,6),lui(6,6)
  integer(i_long),intent(  out) :: kt

  integer(i_long) ihexad(3,6),ilui(6,6)        ! defaults
  integer(i_long) newlhex(3,2:6),newlui(6,2:6),lui1(6)
  real(r_double) wt(6)
  integer(i_long) kp(6,6),ksg(6,6)
  real(r_double) bcmin,bcmins,u,w1
  integer(i_long) i,it,j,k,k1or3,kfirst,ksign,l

  DATA KP /1,2,6,3,4,5, 2,1,4,6,5,3 & ! Permutation code.
          ,3,4,2,5,6,1, 4,3,6,2,1,5 & ! This line is previous + 2 (modulo 6)
          ,5,6,4,1,2,3, 6,5,2,4,3,1/  ! This line is previous + 2 (modulo 6)
  DATA KSG/1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
          ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1 &
          ,1, 1,  1, 1, -1,-1,              1, 1, -1, 1,  1,-1/
  DATA IHEXAD/ 1, 0, 0,  0,-1, 1 &
             , 0, 1, 0,  1, 0,-1 &
             , 0, 0, 1, -1, 1, 0/

  DATA ILUI/1, 0, 0,  0, 1, 1,    0, 0, 0, -1, 0, 0 &
           ,0, 1, 0,  1, 0, 1,    0, 0, 0,  0,-1, 0 &
           ,0, 0, 1,  1, 1, 0,    0, 0, 0,  0, 0,-1/


  bcmins=-epsilon(utarget) !  a criterion slightly < 0 avoids roundoff worries


  IF(LGUESS==0)THEN
     DO J=1,6
        DO I=1,3
           LHEXAD(I,J)=IHEXAD(I,J)
        ENDDO
        DO I=1,6
           LUI(I,J)=ILUI(I,J)
        ENDDO
     ENDDO
  ENDIF

! Use initial estimate of hexad to compute implied weights directly.
! (Subsequent updates of these weights are done perturbatively to save time).
  DO I=1,6
     WHEXAD(I)=zero
  ENDDO
  DO I=1,6
     U=UTARGET(I)
     DO J=1,6
        WHEXAD(J)=WHEXAD(J)+LUI(I,J)*U
     ENDDO
  ENDDO
  K1OR3=1              !  At iteration 1, WHEXAD(1) and (2) might be < 0.

  DO IT=1,100! 4000          !  this should be ample
     KT=IT               !  report back how many iterations were needed
     L=0
     BCMIN=BCMINS
     DO K=K1OR3,6
        IF(WHEXAD(K)<BCMIN)THEN
           L=K
           BCMIN=WHEXAD(L)
        ENDIF
     ENDDO
     IF(L==0)RETURN ! If there are no negetive weights to offend, return
!  Permute the columns of LHEXAD and of LUI according to the permutation
!  scheme encoded by KP(J,L):
     DO J=2,6    ! J=1 corresponds to the NEW direction. (Treat separately).
        K=KP(J,L)
        KSIGN=KSG(J,L)
        WT(J)=WHEXAD(K)
        DO I=1,3
           NEWLHEX(I,J)=KSIGN*LHEXAD(I,K)
        ENDDO
        DO I=1,6
           NEWLUI(I,J)=LUI(I,K)
        ENDDO
     ENDDO

!  Set a temporary vector to what becomes the new column J=1 of LUI
     DO I=1,6
        LUI1(I)=-LUI(I,L)
     ENDDO

!  Replace the first hexad member, J=1, in this new arrangement:
     DO I=1,3
        LHEXAD(I,1)=NEWLHEX(I,4)+NEWLHEX(I,5) ! [  = NEWLHEX(I,3)-NEWLHEX(6) ]
     ENDDO

!  ..and make the corresponding update to the inverse-transpose of the
!  aspect-tensor basis LUI implied by the hexad:
     W1=-WHEXAD(L) ! new weight for J=1
     DO J=3,6
        WHEXAD(J)=WT(J)-W1 ! These weights become more negative than before..
        DO I=1,6
           LUI(I,J)=NEWLUI(I,J)-LUI1(I)
        ENDDO
     ENDDO
     WHEXAD(2)=WT(2)+W1  ! ..this one becomes more positive than before..
     WHEXAD(1)=W1        ! ..and this one simply switches sign to "positive"
     DO I=1,6
        LUI(I,2)=NEWLUI(I,2)+LUI1(I)
        LUI(I,1)=LUI1(I)
     ENDDO

!  copy the remaining new hexad of grid-steps back to array LHEXAD:
     DO J=2,6      !  (data for J=1 are already in place)
        DO I=1,3
           LHEXAD(I,J)=NEWLHEX(I,J)
        ENDDO
     ENDDO
     KFIRST=3_i_long ! After iteration 1, WHEXAD(1) and (2) are always > 0.
  ENDDO
  write(6,*)'GETHEX:  ALL ITERATIONS USED UP.  This should never happen'
  call stop2(68)
END subroutine gethex

subroutine indexxi4(n,arrin4,indx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    indexxi4
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    n
!    arrin4
!    indx
!
!   output argument list:
!    indx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  !-------- indexes an array arrin of length n, i.e. outputs the array indx
  !-------- such that arrin(indx(j)) is in ascending order for j=1,2,...,n.  The
  !-------- input quantities n and arrin are not changed.

  implicit none

  integer(i_long),intent(in   ) :: n
  integer(i_long),intent(in   ) :: arrin4(n)
  integer(i_long),intent(inout) :: indx(n)

  integer(i_long) i,indxt,ir,j,l,q4

  do j=1,n
     indx(j)=j
  end do
  if(n==1) return

  l=n/2+1
  ir=n

  do

    if(l>1) then
       l=l-1
       indxt=indx(l)
       q4=arrin4(indxt)
    else
       indxt=indx(ir)
       q4=arrin4(indxt)
       indx(ir)=indx(1)
       ir=ir-1
       if(ir==1) then
          indx(1)=indxt
          return
       end if
    end if

    i=l
    j=l+l

    do while (j<=ir) 
       if(j<ir) then
          if(arrin4(indx(j))<arrin4(indx(j+1)))j=j+1
       end if
       if(q4<arrin4(indx(j))) then
          indx(i)=indx(j)
          i=j
          j=j+j
       else
          j=ir+1
       end if
    end do

    indx(i)=indxt
  end do

end subroutine indexxi4

subroutine indexxi8(n,arrin8,indx)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    indexxi8
!
!   prgrmmr:
!
! abstract:  indexes an array arrin of length n, i.e. outputs the array indx
!            such that arrin(indx(j)) is in ascending order for j=1,2,...,n.
!            The input quantities n and arrin are not changed.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     n      -  length of input array
!     arrin8 -  input array
!
!   output argument list:
!     indx   - array index
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  integer(i_long) ,intent(in   ) :: n
  integer(i_llong),intent(in   ) :: arrin8(n)
  integer(i_long) ,intent(  out) :: indx(n)

  integer(i_llong) q8
  integer(i_long) i,indxt,ir,j,l

  do j=1,n
     indx(j)=j
  end do
  if(n==1) return

  l=n/2+1
  ir=n

  do

     if(l>1) then
        l=l-1
        indxt=indx(l)
        q8=arrin8(indxt)
     else
        indxt=indx(ir)
        q8=arrin8(indxt)
        indx(ir)=indx(1)
        ir=ir-1
        if(ir==1) then
           indx(1)=indxt
           return
        end if
     end if

     i=l
     j=l+l

     do while (j<=ir) 
        if(j<ir) then
           if(arrin8(indx(j))<arrin8(indx(j+1)))j=j+1
        end if
        if(q8<arrin8(indx(j))) then
           indx(i)=indx(j)
           i=j
           j=j+j
        else
           j=ir+1
        end if

     end do

     indx(i)=indxt
  end do

end subroutine indexxi8

SUBROUTINE init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter, &
                 nsmooth,nsmooth_shapiro, &
                 nvars,idvar,kvar_start,kvar_end,var_names, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_raf4
!
!   prgrmmr:
!
! abstract:  Obtain filtering constants for recursive anisotropic filter.
!     This form is based on assembling full strings, distributed evenly over processors.
!     No attempt is made in this version to treat any points specially when gathering
!     the full strings required for each stage of the filter.  This is the simplest and
!     probably least efficient parallel version of the recursive anisotropic filter.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     aspect                           - aspect tensor for each point (destroyed)
!     triad4                           - switch to turn on 4 color triad smoothing
!                                         for 2-dim variables
!     ngauss                           - number of gaussians to be added together
!     rgauss(ngauss)                   - multipying factors on aspect tensor for each term
!     npass                            - 1/2 num of binomial weighted filter apps--npass <= 10
!     normal                           - number of stocastic samples to use for normalization
!     binom                            - .false., then uniform factors,
!                                         .true., then binomial weighted factors
!     ifilt_ord                        - filter order
!     nvars                            - number of variables
!     idvar(kds:kde)                   - variable number of each level
!     kvar_start(nvars)                - starting global vertical index for each variable
!     kvar_end(nvars)                  - ending global vertical index for each variable
!     var_names(nvars)                 - descriptive name of each variable
!     ids, ide, jds, jde, kds, kde     - domain indices
!     ips, ipe, jps, jpe, kps, kpe     - patch indices
!     mype                             - mpi task id
!     npes                             -
!     nsmooth
!     nsmooth_shapiro
!
!   output argument list:
!     aspect                           - aspect tensor for each point (destroyed)
!     filter                           - structure which contains everything necessary to
!                                         apply recursive anisotropic filter based on input
!                                         aspect tensor
!     nsmooth
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                             ,INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                                                                                ips, ipe, jps, jpe, kps, kpe      ! patch indices
  INTEGER(i_long)                                             ,INTENT(IN   ) :: &
     mype, npes

  TYPE(filter_cons), DIMENSION(7)                             ,INTENT(  OUT) :: &
                       filter         !  structure which contains everything necessary to
                                      !     apply recursive anisotropic filter based on input
                                      !     aspect tensor

  logical                                                     ,intent(in   ) :: triad4             !  switch to turn on 4 color triad smoothing for 2-dim variables
  integer(i_long)                                             ,intent(in   ) :: ngauss    ! number of gaussians to be added together
  real(r_double)                                              ,intent(in   ) ::    rgauss(ngauss)  ! multipying factors on aspect tensor for each term
  INTEGER(i_long)                                             ,INTENT(IN   ) :: npass     ! 1/2 num of binomial weighted filter apps--npass <= 10
  integer(i_long)                                             ,intent(in   ) :: normal    ! number of stocastic samples to use for normalization
                                                                                       !  (if < 0, then use more expensive form which is
                                                                                       !   independent of number of processors)
                                                                                       !  (if = 0, then bypass normalization)
  integer(i_long)                                             ,intent(in   ) :: ifilt_ord ! filter order
                                                                                       !  NOTE:  if ifilt_ord = -4, this is the special case
                                                                                       !   of a new approximate 4th order factorization that
                                                                                       !   only requires 2nd order amount of work.
                                                                                       !   In this case, must multiply binomial by 2 to account
                                                                                       !   for only doing half as many passes.
  integer(i_long)                                             ,intent(inout) :: nsmooth   !  number of 1-2-1 smoothing passes to apply
  integer(i_long)                                             ,intent(in   ) :: nsmooth_shapiro   !  number of 2nd moment preserving shapiro smoothers
  logical                                                     ,intent(in   ) :: binom      !   .false., then uniform factors,
                                                                                           !   .true., then binomial weighted factors

  real(r_single)   , DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            aspect                 ! aspect tensor for each point (destroyed)
                                   !    (1-xx,2--yy,3-zz,4-yz,5-xz,6-xy)
  integer(i_long)                                             ,intent(in   ) :: nvars                       ! number of variables
  integer(i_long)                                             ,intent(in   ) :: idvar(kds:kde)              ! variable number of each level
  integer(i_long)                                             ,intent(in   ) :: kvar_start(nvars)           ! starting global vertical index for each variable
  integer(i_long)                                             ,intent(in   ) :: kvar_end(nvars)             ! ending global vertical index for each variable
  character(80)                                               ,intent(in   ) :: var_names(nvars)            ! descriptive name of each variable

  INTEGER(i_short), DIMENSION( 3, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: &
            i1filter              !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: &
            i2filter               !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_long) nstrings

  REAL(r_double) binomial0(20,19),sumbin(19)
  REAL(r_double) binomial(10,10)
  real(r_double) factor_binom
  INTEGER(i_short) lhexadx(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_short) lhexady(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_short) lhexadz(ips-1:ipe+1,jps-1:jpe+1,kps-1:kpe+1,7)
  INTEGER(i_long) lhexadlast(3,6),lui(6,6)
  integer(i_long) ltriadlast(2,4),lui_triad(3,3)
  INTEGER(i_short), DIMENSION( 6, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: label_string
  REAL(r_double) aspect8(6),whexad8(6)

  integer(i_long) npoints_recv(0:npes-1)
  integer(i_short),allocatable:: info_string(:,:)
  real(r_single),allocatable:: aspect_full(:)

  integer(i_long) i,icolor,ierr,im,itest,ivar,ivar_end,ivar_start, &
             ixend,ixinc,ixstart,ixtemp, &
             j,jm,jtest,jumpx,jumpy,jumpz,k, &
             kk,km,kt,ktest,len,lentest,lguess,m,npoints_send,nstringsall

  integer(i_long) nstrings_var(nvars),nstrings_varall(nvars,7)
  integer(i_long) lenmax(nvars),lenmin(nvars),npoints1(nvars)
  integer(i_long) lenmaxall(nvars,7),lenminall(nvars,7)
  integer(i_long) totalpoints,totalpoints1(nvars,7)
  integer(i_long) jumpxmax(nvars),jumpxmaxall(nvars,7)
  integer(i_long) jumpymax(nvars),jumpymaxall(nvars,7)
  integer(i_long) jumpzmax(nvars),jumpzmaxall(nvars,7)
  integer(i_long) jumpxmin(nvars),jumpxminall(nvars,7)
  integer(i_long) jumpymin(nvars),jumpyminall(nvars,7)
  integer(i_long) jumpzmin(nvars),jumpzminall(nvars,7)
  real(r_double) lenbar(nvars),lenbarall(nvars,7)
  real(r_double) epstest
  integer(i_long) igauss
  real(r_single) aspect_max(nvars,3),aspect_min(nvars,3)
  real(r_single) aspect_max_all(nvars,3),aspect_min_all(nvars,3)
  real(r_single) srgauss_min,srgauss_max
  integer(i_long) istat
! DEBUG
  logical:: ldebug,print_verbose
  real(r_single),allocatable:: rout(:,:),routa(:,:)

  print_verbose = .false.
  if(verbose)print_verbose=.true.
!    nsmooth_shapiro has priority over nsmooth
  if(nsmooth_shapiro>0) nsmooth=0
!      set up halo update (only if nsmooth and/or nsmooth_shapiro > 0)
  filter(1)%nrows=0
  if(nsmooth>0) filter(1)%nrows=1
  if(nsmooth_shapiro>0) filter(1)%nrows=3_i_long
  filter(1)%nsmooth=nsmooth
  filter(1)%nsmooth_shapiro=nsmooth_shapiro
  call add_halox0(filter(1),filter(1)%nrows,ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes)
  call add_haloy0(filter(1),filter(1)%nrows,ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes)
  deallocate(filter(1)%gnorm_halox,stat=istat)
  allocate(filter(1)%gnorm_halox(ips:ipe))
  filter(1)%gnorm_halox=one
  deallocate(filter(1)%gnorm_haloy,stat=istat)
  allocate(filter(1)%gnorm_haloy(jps:jpe))
  filter(1)%gnorm_haloy=one
  if(nsmooth_shapiro>0) then
     call smther_two_gnorm(filter(1)%gnorm_halox,ids,ide,ips,ipe)
     call smther_two_gnorm(filter(1)%gnorm_haloy,jds,jde,jps,jpe)
  end if

  filter(1)%ngauss=ngauss
  filter(1)%npass=npass
  if(ifilt_ord==-4_i_long) then
     filter(1)%ifilt_ord=2_i_long
     filter(1)%new_factorization=.true.
  else
     filter(1)%ifilt_ord=ifilt_ord
     filter(1)%new_factorization=.false.
  end if

!  compute binomial coefficients

  factor_binom=one
  if(.not.binom) factor_binom=zero
  binomial0=zero
  binomial0(1,1)=one
  binomial0(2,1)=one
  sumbin(1)=two
  do k=2,19
     binomial0(1,k)=one
     binomial0(k+1,k)=one
     do i=2,k
        binomial0(i,k)=binomial0(i-1,k-1)+binomial0(i,k-1)*factor_binom
     end do
     sumbin(k)=zero
     do i=1,k+1
        sumbin(k)=sumbin(k)+binomial0(i,k)
     end do
  end do
  do k=1,19
     binomial0(:,k)=binomial0(:,k)/sumbin(k)
  end do

  kk=0
  binomial=zero
  do k=1,19,2
     kk=kk+1
     binomial(1:kk,kk)=binomial0(1:kk,k)
  end do
  if(ifilt_ord==-4_i_long) binomial=two*binomial
  if(mype==0 .and. print_verbose) write(6,*)'INIT_RAF4:  binomial weightings used:',binomial(1:npass,npass)

!  gather some stats on input aspect tensor for informational purposes

  aspect_max=0
  aspect_min=huge(aspect_min)
  do k=kps,kpe
     ivar=idvar(k)
     do j=jps,jpe
        do i=ips,ipe
!          if(aspect(1,i,j,k)<.001_r_kind) write(6,*)' ivar,i,j,k,aspect(1,i,j,k)=',ivar,i,j,k,aspect(1,i,j,k)
           aspect_max(ivar,1)=max(aspect_max(ivar,1),sqrt(aspect(1,i,j,k)))
           aspect_min(ivar,1)=min(aspect_min(ivar,1),sqrt(aspect(1,i,j,k)))
           aspect_max(ivar,2)=max(aspect_max(ivar,2),sqrt(aspect(2,i,j,k)))
           aspect_min(ivar,2)=min(aspect_min(ivar,2),sqrt(aspect(2,i,j,k)))
           aspect_max(ivar,3)=max(aspect_max(ivar,3),sqrt(aspect(3,i,j,k)))
           aspect_min(ivar,3)=min(aspect_min(ivar,3),sqrt(aspect(3,i,j,k)))
        end do
     end do
  end do
  call mpi_reduce(aspect_max,aspect_max_all,3*nvars,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
  call mpi_reduce(aspect_min,aspect_min_all,3*nvars,mpi_real4,mpi_min,0,mpi_comm_world,ierr)
  if(mype==0 .and. print_verbose) then

     write(6,*)' corlen multipliers for additive gaussians: '
     do igauss=1,ngauss
        write(6,*)'  sqrt(rgauss(',igauss,') = ',sqrt(rgauss(igauss))
     end do
     srgauss_min=sqrt(minval(rgauss))
     srgauss_max=sqrt(maxval(rgauss))
     do ivar=1,nvars
        if(kvar_start(ivar)==kvar_end(ivar)) then
           write(6,*)' corlen ranges for 2d variable ',trim(var_names(ivar))
           write(6,*)'  min-min, min, max, max-max (1) = ', &
                        srgauss_min*aspect_min_all(ivar,1), &
                                    aspect_min_all(ivar,1), &
                                    aspect_max_all(ivar,1), &
                        srgauss_max*aspect_max_all(ivar,1)
           write(6,*)'  min-min, min, max, max-max (2) = ', &
                        srgauss_min*aspect_min_all(ivar,2), &
                                    aspect_min_all(ivar,2), &
                                    aspect_max_all(ivar,2), &
                        srgauss_max*aspect_max_all(ivar,2)
        else
           write(6,*)' corlen ranges for 3d variable ',trim(var_names(ivar))
           write(6,*)'  min-min, min, max, max-max (1) = ', &
                        srgauss_min*aspect_min_all(ivar,1), &
                                    aspect_min_all(ivar,1), &
                                    aspect_max_all(ivar,1), &
                        srgauss_max*aspect_max_all(ivar,1)
           write(6,*)'  min-min, min, max, max-max (2) = ', &
                        srgauss_min*aspect_min_all(ivar,2), &
                                    aspect_min_all(ivar,2), &
                                    aspect_max_all(ivar,2), &
                        srgauss_max*aspect_max_all(ivar,2)
           write(6,*)'  min-min, min, max, max-max (3) = ', &
                        srgauss_min*aspect_min_all(ivar,3), &
                                    aspect_min_all(ivar,3), &
                                    aspect_max_all(ivar,3), &
                        srgauss_max*aspect_max_all(ivar,3)
        end if
     end do
  end if

!  get all directions and smoothing coefficients

  lhexadx=0 ; lhexady=0 ; lhexadz=0
  epstest=10._r_double*epsilon(epstest)
  do k=kps,kpe
     ivar=idvar(k)
     ivar_start=kvar_start(ivar)
     ivar_end=kvar_end(ivar)
     ixstart=ipe ; ixend=ips ; ixinc=-1
     lguess=0
     do j=jps,jpe
        ixtemp=ixstart ; ixstart=ixend ; ixend=ixtemp ; ixinc=-ixinc
        do i=ixstart,ixend,ixinc
           if(triad4.and.ivar_start==ivar_end) then
              aspect8(1)=aspect(1,i,j,k)
              aspect8(2)=aspect(2,i,j,k)
              aspect8(3)=aspect(6,i,j,k)
              call gettri4(aspect8,lguess,ltriadlast,lui_triad,whexad8)
              lhexadlast=0
              do kk=1,4
                 lhexadlast(1,kk)=ltriadlast(1,kk)
                 lhexadlast(2,kk)=ltriadlast(2,kk)
                 lhexadlast(3,kk)=0
              end do
              whexad8(5)=zero
              whexad8(6)=zero
           else
              aspect8(1:6)=aspect(1:6,i,j,k)
              call gethex(aspect8,lguess,lhexadlast,lui,whexad8,kt)
           end if
           aspect(1:7,i,j,k)=zero
           do kk=1,6
              if(whexad8(kk)>epstest) then
                 jumpx=lhexadlast(1,kk)       !  make all directions positive and
                 jumpy=lhexadlast(2,kk)       !  assign color
                 jumpz=lhexadlast(3,kk)
                 if(jumpz/=0.and.ivar_start==ivar_end) exit ! if 2-d, strings of interest are x-y only
                 if(jumpz<0) then
                    jumpx=-jumpx ; jumpy=-jumpy ; jumpz=-jumpz
                 end if
                 if(jumpz==0) then
                    if(jumpy<0) then
                       jumpx=-jumpx ; jumpy=-jumpy
                    end if
                    if(jumpy==0.and.jumpx<0) jumpx=-jumpx
                 end if
                 if(triad4.and.ivar_start==ivar_end) then
                    call what_color_is_triad(jumpx,jumpy,icolor,3_i_long)
                 else
                    call what_color_is(jumpx,jumpy,jumpz,icolor)
                 end if
                 lhexadx(i,j,k,icolor)=jumpx ; lhexady(i,j,k,icolor)=jumpy ; lhexadz(i,j,k,icolor)=jumpz
                 aspect(icolor,i,j,k)=whexad8(kk)
              end if
           end do
           lguess=1
        end do
     end do
  end do

!                 big loop over all colors

  do icolor=1,7

!    get all string starting addresses and lengths

     m=0
     do k=kps,kpe
        ivar=idvar(k)
        ivar_start=kvar_start(ivar)
        do j=jps,jpe
           do i=ips,ipe
              jumpz=lhexadz(i,j,k,icolor)
              km=k-jumpz   !  note jumpz always >= 0 by construction
              jumpx=lhexadx(i,j,k,icolor) ; jumpy=lhexady(i,j,k,icolor)
              if(km<ivar_start) then
                 m=m+1
                 i1filter(1,m)=jumpx ; i1filter(2,m)=jumpy ; i1filter(3,m)=jumpz
                 i2filter(1,m)=i ; i2filter(2,m)=j ; i2filter(3,m)=k ; i2filter(5,m)=ivar
                 cycle
              end if
              if(jumpx/=0.or.jumpy/=0.or.jumpz/=0) then
                 im=max(ips-1,min(i-jumpx,ipe+1))
                 jm=max(jps-1,min(j-jumpy,jpe+1))
                 km=max(kps-1,min(km     ,kpe+1))
                 if(lhexadx(im,jm,km,icolor)/=jumpx.or. &
                    lhexady(im,jm,km,icolor)/=jumpy.or. &
                    lhexadz(im,jm,km,icolor)/=jumpz) then
                    m=m+1
                    i1filter(1,m)=jumpx ; i1filter(2,m)=jumpy ; i1filter(3,m)=jumpz
                    i2filter(1,m)=i ; i2filter(2,m)=j ; i2filter(3,m)=k ; i2filter(5,m)=ivar
                 end if
              end if
           end do
        end do
     end do
     nstrings=m
     if(npes==1) then
        nstringsall=nstrings
     else
        call mpi_allreduce(nstrings,nstringsall,1,mpi_integer4,mpi_sum,mpi_comm_world,ierr)
     end if
     npoints_send=0
     npoints_recv=0
     nstrings_var=0
     filter(icolor)%nstrings=0
     filter(icolor)%npoints_send=0
     filter(icolor)%npoints_recv=0
     filter(icolor)%npointsmax=0
     filter(icolor)%npointsmaxall=0
     lenbar=zero
     lenmax=-huge(lenmax)
     lenmin=huge(lenmin)
     npoints1=0
     jumpxmax=-huge(jumpxmax) ; jumpymax=-huge(jumpxmax) ; jumpzmax=-huge(jumpxmax)
     jumpxmin= huge(jumpxmin) ; jumpymin= huge(jumpxmin) ; jumpzmin= huge(jumpxmin)
     if(nstringsall>0) then
        if(nstrings>0) then
           do m=1,nstrings
              len=1
              jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
              ivar=i2filter(5,m)
              jumpxmax(ivar)=max(jumpx,jumpxmax(ivar))
              jumpymax(ivar)=max(jumpy,jumpymax(ivar))
              jumpzmax(ivar)=max(jumpz,jumpzmax(ivar))
              jumpxmin(ivar)=min(jumpx,jumpxmin(ivar))
              jumpymin(ivar)=min(jumpy,jumpymin(ivar))
              jumpzmin(ivar)=min(jumpz,jumpzmin(ivar))
              i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m)
              ivar_end=kvar_end(ivar)
              do
                 lentest=len+1
                 ktest=k+jumpz
                 if(ktest>ivar_end) then
                    i2filter(4,m)=len
                    npoints_send=npoints_send+len
                    exit
                 end if
                 itest=max(ips-1,min(i+jumpx,ipe+1))
                 jtest=max(jps-1,min(j+jumpy,jpe+1))
                 ktest=max(kps-1,min(ktest  ,kpe+1))
                 if(jumpx/=lhexadx(itest,jtest,ktest,icolor).or. &
                    jumpy/=lhexady(itest,jtest,ktest,icolor).or. &
                    jumpz/=lhexadz(itest,jtest,ktest,icolor)) then
                    i2filter(4,m)=len
                    npoints_send=npoints_send+len
                    exit
                 end if
                 len=lentest
                 i=itest ; j=jtest ; k=ktest
              end do
           end do
        end if

!         Begin computation of alpha, beta, the recursive filter coefficients.
!           It is necessary to have contiguous strings for this process, so first
!             must label strings and assign them to processors so the load is evenly distributed
!             when string pieces are gathered together into full strings


!         assign global label, origin and destination pe number to each string piece
!            (global label is starting i,j,k closest to edge of global domain)

        call string_label(i1filter,i2filter,nstrings,label_string,npoints_recv, &
                         nvars,kvar_start,kvar_end, &
                         ids, ide, jds, jde, kds, kde, &         ! domain indices
                         ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                         mype,npes)

        filter(icolor)%npointsmax=max(npoints_recv(mype),npoints_send)
        if(npes==1) then
           filter(icolor)%npointsmaxall=filter(icolor)%npointsmax
        else
           call mpi_allreduce(filter(icolor)%npointsmax, &
                           filter(icolor)%npointsmaxall,1,mpi_integer4,mpi_max,mpi_comm_world,ierr)
        end if
        filter(icolor)%npoints_send=npoints_send
        filter(icolor)%npoints_recv=npoints_recv(mype)
        allocate(info_string(8,max(1,npoints_recv(mype))))
        allocate(aspect_full(max(1,npoints_recv(mype))))

!       assemble full strings

        deallocate(filter(icolor)%nsend,stat=istat)
        deallocate(filter(icolor)%ndsend,stat=istat)
        deallocate(filter(icolor)%nrecv,stat=istat)
        deallocate(filter(icolor)%ndrecv,stat=istat)
        deallocate(filter(icolor)%ia,stat=istat)
        deallocate(filter(icolor)%ja,stat=istat)
        deallocate(filter(icolor)%ka,stat=istat)
        allocate(filter(icolor)%nsend(0:npes-1))
        allocate(filter(icolor)%ndsend(0:npes))
        allocate(filter(icolor)%nrecv(0:npes-1))
        allocate(filter(icolor)%ndrecv(0:npes))
        allocate(filter(icolor)%ia(max(1,npoints_send)))
        allocate(filter(icolor)%ja(max(1,npoints_send)))
        allocate(filter(icolor)%ka(max(1,npoints_send)))
        call string_assemble4(i1filter,i2filter,nstrings,label_string, &
                        npoints_send,npoints_recv(mype),aspect,icolor, &
                        info_string,aspect_full, &
                        filter(icolor)%nsend,filter(icolor)%ndsend, &
                        filter(icolor)%nrecv,filter(icolor)%ndrecv, &
                        filter(icolor)%ia,filter(icolor)%ja,filter(icolor)%ka, &
                        ips,ipe,jps,jpe,kps,kpe,mype,npes)

!       organize full strings for processing

        deallocate(filter(icolor)%ib,stat=istat)
        allocate(filter(icolor)%ib(max(1,npoints_recv(mype))))
        if(npoints_recv(mype)>0) then
           call sort_strings4(info_string,aspect_full,npoints_recv(mype),filter(icolor)%ib,nvars)

!      count number of strings

           call count_strings(info_string,filter(icolor)%nstrings,nstrings_var,nvars,npoints_recv(mype))

!      compute desired alpha and beta for final filter

           deallocate(filter(icolor)%istart,stat=istat)
           allocate(filter(icolor)%istart(filter(icolor)%nstrings+1))

           if(filter(1)%new_factorization) then
              deallocate(filter(icolor)%fmat,stat=istat)
              deallocate(filter(icolor)%fmat0,stat=istat)
              allocate(filter(icolor)%fmat(2,npoints_recv(mype),npass,ngauss,2))
              allocate(filter(icolor)%fmat0(npoints_recv(mype),npass,ngauss,2))
              do igauss=1,ngauss
                 call new_alpha_beta4(info_string,aspect_full,rgauss(igauss), &
                             filter(icolor)%fmat,filter(icolor)%fmat0,igauss,ngauss, &
                             filter(icolor)%istart,npoints_recv(mype),binomial,npass, &
                             lenbar,lenmax,lenmin,npoints1,nvars)

              end do
           else
              deallocate(filter(icolor)%lnf,stat=istat)
              deallocate(filter(icolor)%bnf,stat=istat)
              allocate(filter(icolor)%lnf(ifilt_ord,npoints_recv(mype),npass,ngauss))
              allocate(filter(icolor)%bnf(npoints_recv(mype),npass,ngauss))
              do igauss=1,ngauss
                 call alpha_beta4(info_string,aspect_full,rgauss(igauss), &
                            filter(icolor)%lnf,filter(icolor)%bnf,igauss,ngauss, &
                            filter(icolor)%istart,npoints_recv(mype),binomial,npass, &
                            ifilt_ord,lenbar,lenmax,lenmin,npoints1,nvars)
 
              end do
           end if

        end if
        deallocate(info_string)
        deallocate(aspect_full)

     else

        filter(icolor)%npoints_send=0       !  here if no strings for this color
        filter(icolor)%npoints_recv=0
        filter(icolor)%npointsmax=0
        filter(icolor)%npointsmaxall=0

     end if
     if(npes==1) then
        lenbarall(:,icolor)=lenbar
        nstrings_varall(:,icolor)=nstrings_var
        lenmaxall(:,icolor)=lenmax
        lenminall(:,icolor)=lenmin
        totalpoints1(:,icolor)=npoints1
        jumpxmaxall(:,icolor)=jumpxmax
        jumpymaxall(:,icolor)=jumpymax
        jumpzmaxall(:,icolor)=jumpzmax
        jumpxminall(:,icolor)=jumpxmin
        jumpyminall(:,icolor)=jumpymin
        jumpzminall(:,icolor)=jumpzmin
     else
        call mpi_reduce(lenbar,lenbarall(1,icolor),nvars,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
        call mpi_reduce(nstrings_var,nstrings_varall(1,icolor),nvars, &
                     mpi_integer4,mpi_sum,0,mpi_comm_world,ierr)
        call mpi_reduce(lenmax,lenmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
        call mpi_reduce(lenmin,lenminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
        call mpi_reduce(npoints1,totalpoints1(1,icolor),nvars,mpi_integer4,mpi_sum,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpxmax,jumpxmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpymax,jumpymaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpzmax,jumpzmaxall(1,icolor),nvars,mpi_integer4,mpi_max,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpxmin,jumpxminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpymin,jumpyminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
        call mpi_reduce(jumpzmin,jumpzminall(1,icolor),nvars,mpi_integer4,mpi_min,0,mpi_comm_world,ierr)
     end if

  end do         !     end big loop over all colors

!  print out diagnostics for each variable

  if(mype==0 .and. print_verbose) then
     do ivar=1,nvars

        write(6,*)' STRING STATS FOLLOW FOR VARIABLE #',ivar,':  ',trim(var_names(ivar))
        do icolor=1,7

           if(nstrings_varall(ivar,icolor).gt.0) then
              totalpoints=nint(lenbarall(ivar,icolor))
              lenbarall(ivar,icolor)=lenbarall(ivar,icolor)/nstrings_varall(ivar,icolor)
              write(6,*)'  string stats for ivar,icolor=',ivar,icolor
              write(6,'("        ave string length=",f8.2)')lenbarall(ivar,icolor)
              write(6,'("        max string length=",i8)')lenmaxall(ivar,icolor)
              write(6,'("        min string length=",i8)')lenminall(ivar,icolor)
              write(6,'("        total num strings=",i9)')nstrings_varall(ivar,icolor)
              write(6,'("        total num  points=",i9)')totalpoints
              write(6,'("        num len 1 strings=",i9)')totalpoints1(ivar,icolor)
              write(6,'("             jumpxmin,max=",2i12)')jumpxminall(ivar,icolor),jumpxmaxall(ivar,icolor)
              write(6,'("             jumpymin,max=",2i12)')jumpyminall(ivar,icolor),jumpymaxall(ivar,icolor)
              write(6,'("             jumpzmin,max=",2i12)')jumpzminall(ivar,icolor),jumpzmaxall(ivar,icolor)
           end if
 
        end do
     end do
  end if

      !       call adjoint_check4(filter,ngauss,ips,ipe,jps,jpe,kps,kpe,mype,npes)
  call normalize2_raf4(filter,filter(1)%ngauss,normal, &
                      ids, ide, jds, jde, kde, &                    ! domain indices
                      ips, ipe, jps, jpe, kps, kpe, &                    ! patch indices
                      mype, npes)

!DEBUG test output
  ldebug=.false.
  if(ldebug) then
     if(mype==0 .and. print_verbose) write(6,*) 'normalization finished'
     allocate(rout (ips:ipe,jps:jpe))
     allocate(routa(ips:ipe,jps:jpe))
     if(mype==0) open(32,form='unformatted')
     do k=kds,kde
        if(k>=kps.and.k<=kpe) then
           kk=k-kps+1
           do j=jps,jpe
              do i=ips,ipe
                 rout(i,j)=filter(1)%amp(1,i,j,k)
              end do
           end do
        else
           rout=zero_single
        end if
        call mpi_reduce(rout,routa,(ipe-ips+1)*(jpe-jps+1), &
                        mpi_real4,mpi_sum,0,mpi_comm_world,ierr)
        if(mype==0 .and. print_verbose) then
           write(32) routa
           write(6,*) 'amp:',k,maxval(routa),minval(routa)
        end if
     end do
     if(mype==0) close(32)
     deallocate(rout)
     deallocate(routa)
  end if
!DEBUG

return
end subroutine init_raf4

subroutine normalize2_raf4(filter,ngauss,normal, &
                         ids, ide, jds, jde, kde, &                ! domain indices
                         ips, ipe, jps, jpe, kps, kpe, &                ! patch indices
                         mype, npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    normalize2_raf4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ngauss                        -
!     normal                        -
!     ids, ide, jds, jde, kde  - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     mype                          - mpi task id
!     npes                          -
!     filter
!
!   output argument list:
!     filter
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  INTEGER(i_long)  ,INTENT(IN   ) :: ids, ide, jds, jde, kde, &   ! domain indices
                                     ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)  ,INTENT(IN   ) :: &
     ngauss,normal,mype, npes

  TYPE(filter_cons),INTENT(INOUT) :: filter(7)

  real(r_single) ranvec(2, ngauss,ips:ipe, jps:jpe, kps:kpe )
  real(r_single) bigg( ngauss,ips:ipe, jps:jpe, kps:kpe )

  integer(i_long) i,igauss,j,k,loop,nsamples,ierror
  real(r_single) this_one1,this_one2

  integer(i_long) istat,ii
  integer(i_llong) jj,j1,j2,j3
  integer(i_byte) flips(2**27-8)

  logical:: independent_of_npes,print_verbose

  print_verbose = .false.
  if(verbose)print_verbose=.true.

  deallocate(filter(1)%amp,stat=istat)
  allocate(filter(1)%amp(ngauss,ips:ipe,jps:jpe,kps:kpe))

  if(normal==0) then
     filter(1)%amp=one
     return
  end if

!   read in fixed set of random flips
  if(mype==0) then
     open(997433,file='random_flips',form='unformatted')
     read(997433) flips
     close(997433)
  end if
  call mpi_bcast(flips,2**27-8,mpi_integer1,0,mpi_comm_world,ierror)

  nsamples=abs(normal)/2
  independent_of_npes=normal<0

  bigg=zero
  ii=-1

  if(independent_of_npes) then;  jj= 0
  else;                          jj=(2**27-8)*mype/npes
  end if

  do loop=1,nsamples
     if(mype==0.and.mod(loop-1,10_i_long)==0 .and. print_verbose) write(6,*) 'normalization step:',loop,'/',nsamples
     j1=(int(loop,i_llong)-1)*kde
     ranvec=zero_single
     do k=kps,kpe
        j2=(j1+k-1)*jde
        do j=jps,jpe
           j3=(j2+j-1)*ide
           do i=ips,ipe

              if(independent_of_npes) then
                 jj=(j3+i-1)*2+8
                 ii=mod(jj,8_i_llong)
                 jj=mod(jj/8_i_llong,2_i_llong**27-8)
              else
                 ii=mod(ii+1,8_i_long)
                 if(ii==0) jj=jj+1_i_llong
                 if(jj>2**27-8_i_llong) jj=1_i_llong
              end if

              this_one1=-one
              if(btest(flips(jj),ii)) this_one1=one

              ii=mod(ii+1,8_i_long)
              if(.not.independent_of_npes) then
                 if(ii==0) jj=jj+1_i_llong
                 if(jj>2**27-8_i_llong) jj=1_i_llong
              end if

              this_one2=-one
              if(btest(flips(jj),ii)) this_one2=one

              do igauss=1,ngauss
                 ranvec(1,igauss,i,j,k)=this_one1
                 ranvec(2,igauss,i,j,k)=this_one2
              end do
           end do
        end do
     end do

     call rad_sm24_ad(ranvec,filter,ngauss,ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
     do k=kps,kpe
        do j=jps,jpe
           do i=ips,ipe
              do igauss=1,ngauss
                 bigg(igauss,i,j,k)=bigg(igauss,i,j,k)+ranvec(1,igauss,i,j,k)**2+ranvec(2,igauss,i,j,k)**2
              end do
           end do
        end do
     end do
  end do

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              filter(1)%amp(igauss,i,j,k)=one/sqrt(bigg(igauss,i,j,k)/(two*nsamples))
           end do
        end do
     end do
  end do

end subroutine normalize2_raf4

subroutine one_color4(g,filter,ngauss,ipass,ifilt_ord, &
                      nstrings,istart,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    one_color4
!
!   prgrmmr:
!
! abstract:  apply one forward-backward recursive filter for one color
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ipass                         - total number of contiguous string points
!     ifilt_ord                     -
!     nstrings                      -
!     istart                        -
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     npes                          -
!
!   output argument list:
!     g        -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
     npes,ngauss

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
            ipass          !  total number of contiguous string points
  integer(i_long)                                              ,intent(in   ) :: ifilt_ord

  real(r_single), DIMENSION( ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long)                                              ,intent(in   ) :: nstrings
  integer(i_long)                                              ,intent(in   ) :: istart(nstrings+1)
  type(filter_cons)                                            ,intent(in   ) :: filter

  real(r_single) work(ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           work(igauss,i,1)=g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,i,2)=work(igauss,i,1)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                     work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv>0) then
     do igauss=1,ngauss
        do i=1,filter%npoints_recv
           work2(i)=work(igauss,filter%ib(i),2)
        end do

        do j=1,nstrings
           do i=istart(j)+1,istart(j+1)-1
              do l=1,min(ifilt_ord,i-istart(j))
                 work2(i)=work2(i)-filter%lnf(l,i,ipass,igauss)*work2(i-l)
              end do
           end do
           do i=istart(j),istart(j+1)-1
              work2(i)=filter%bnf(i,ipass,igauss)*work2(i)
           end do
           do i=istart(j+1)-2_i_long,istart(j),-1
              do l=1,min(ifilt_ord,istart(j+1)-i-1)
                 work2(i)=work2(i)-filter%lnf(l,i+l,ipass,igauss)*work2(i+l)
              end do
           end do
        end do

        do i=1,filter%npoints_recv
           work(igauss,i,1)=work2(i)
        end do
     end do

!-- send strings back

     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,filter%ib(i),2)=work(igauss,i,1)
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,i,1)=work(igauss,i,2)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                        work(1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(igauss,i,1)
        end do
     end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color4

subroutine one_color24(g,filter,ngauss,ipass,ifilt_ord, &
             nstrings,istart,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    one_color24
!
!   prgrmmr:
!
! abstract:  apply one forward-backward recursive filter for one color
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ipass                         - total number of contiguous string points
!     ifilt_ord                     -
!     nstrings                      -
!     istart                        -
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
     ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
     npes,ngauss

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
            ipass          !  total number of contiguous string points
  integer(i_long)                                                ,intent(in   ) :: ifilt_ord

  real(r_single), DIMENSION(2,ngauss, ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long)                                                ,intent(in   ) :: nstrings
  integer(i_long)                                                ,intent(in   ) :: istart(nstrings+1)
  type(filter_cons)                                              ,intent(in   ) :: filter

  real(r_single) work(2,ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,ii,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(2*ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           work(1,igauss,i,1)=g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
           work(2,igauss,i,1)=g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,i,2)=work(1,igauss,i,1)
           work(2,igauss,i,2)=work(2,igauss,i,1)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                     work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv>0) then
     do igauss=1,ngauss
        do ii=1,2
           do i=1,filter%npoints_recv
              work2(i)=work(ii,igauss,filter%ib(i),2)
           end do

           do j=1,nstrings
              do i=istart(j)+1,istart(j+1)-1
                 do l=1,min(ifilt_ord,i-istart(j))
                    work2(i)=work2(i)-filter%lnf(l,i,ipass,igauss)*work2(i-l)
                 end do
              end do
              do i=istart(j),istart(j+1)-1
                 work2(i)=filter%bnf(i,ipass,igauss)*work2(i)
              end do
              do i=istart(j+1)-2,istart(j),-1
                 do l=1,min(ifilt_ord,istart(j+1)-i-1)
                    work2(i)=work2(i)-filter%lnf(l,i+l,ipass,igauss)*work2(i+l)
                 end do
              end do
           end do

           do i=1,filter%npoints_recv
              work(ii,igauss,i,1)=work2(i)
           end do
        end do
     end do

!-- send strings back

     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,filter%ib(i),2)=work(1,igauss,i,1)
           work(2,igauss,filter%ib(i),2)=work(2,igauss,i,1)
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,i,1)=work(1,igauss,i,2)
           work(2,igauss,i,1)=work(2,igauss,i,2)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                       work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(1,igauss,i,1)
           g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(2,igauss,i,1)
        end do
     end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color24

SUBROUTINE raf4(g,filter,ngauss,ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf4
!
!   prgrmmr:
!
! abstract:  1st half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ids, ide, jds, jde            - domain indices
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: ids,ide,jds,jde, &
            ips,ipe,jps,jpe,kps,kpe

  INTEGER(i_long)                                              ,INTENT(IN   ) :: npes,ngauss

  real(r_single), DIMENSION(ngauss, ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons)                                            ,intent(in   ) :: filter(7)             !  structure defining recursive filter

  integer(i_long) i,icolor,ipass,igauss,j,k,iadvance,iback

!   multiply by amp first

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do igauss=1,ngauss
              g(igauss,i,j,k)=filter(1)%amp(igauss,i,j,k)*g(igauss,i,j,k)
           end do
        end do
     end do
  end do

!    apply smoothing if nsmooth or nsmooth_shapiro > 0
  if(kps<=kpe) then
     do i=1,max(filter(1)%nsmooth,filter(1)%nsmooth_shapiro)
        call smther(filter(1),g,filter(1)%nrows,ngauss,filter(1)%nsmooth,filter(1)%nsmooth_shapiro, &
                   ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe, &
                   npes,filter(1)%gnorm_halox,filter(1)%gnorm_haloy,.false.)
     end do
  end if

  if(filter(1)%npass>0) then
     do ipass=1,filter(1)%npass

        do icolor=7,1,-1

           if(filter(icolor)%npointsmaxall>0) then
              if(filter(1)%new_factorization) then
                 iadvance=1
                 iback=2_i_long
                 call one_color4_new_factorization(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart, &
                     ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
              else
                 call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart,ips,ipe,jps,jpe,kps,kpe,npes)
              end if
           end if

        end do

     end do
  end if

end subroutine raf4

SUBROUTINE raf_sm4(g,filter,ngauss,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    raf_sm4
!
!   prgrmmr:
!
! abstract:  1st half of recursive anisotropic self-adjoint filter (full-strings version)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!     filter                        -
!     ngauss                        -
!     ips, ipe, jps, jpe, kps, kpe  - patch indices
!     npes                          -
!
!   output argument list:
!     g                             -  input--field on grid, output--filtered field on grid
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
     npes,ngauss

  real(r_single), DIMENSION( ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons)                                            ,intent(in   ) :: filter(7)             !  structure defining recursive filter

  integer(i_long) icolor,ipass,iadvance,iback

  if(filter(1)%npass>0) then
     do ipass=1,filter(1)%npass

        do icolor=7,1,-1

           if(filter(icolor)%npointsmaxall>0) then
              if(filter(1)%new_factorization) then
                 iadvance=1
                 iback=2_i_long
                 call one_color4_new_factorization(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart, &
                     ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
              else
                 call one_color4(g,filter(icolor),ngauss,ipass,filter(1)%ifilt_ord, &
                     filter(icolor)%nstrings,filter(icolor)%istart,ips,ipe,jps,jpe,kps,kpe,npes)
              end if
           end if
 
        end do

     end do
  end if

end subroutine raf_sm4

subroutine sort_strings4(info_string,aspect_full,npoints_recv,ib,nvars)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sort_strings4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     info_string                   - 1       - distance from origin to current point
!                                     2,3,4   - origin coordinates
!                                     5,6,7,8 - jumpx,jumpy,jumpz,ivar for this string
!     aspect_full                   -
!     npoints_recv                  -
!
!   output argument list:
!     info_string                   -
!     aspect_full                   -
!     ib
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  integer(i_long)                               ,intent(in   ) :: nvars

  INTEGER(i_long)                               ,INTENT(IN   ) :: npoints_recv

  INTEGER(i_short), DIMENSION( 8, npoints_recv ),INTENT(INOUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single)  , DIMENSION( npoints_recv )   ,INTENT(INOUT) :: &
            aspect_full

  integer(i_long)                               ,intent(  out) :: ib(npoints_recv)

  integer(i_long) ib0(npoints_recv),ib1(npoints_recv)
  integer(i_llong) ij_origin(npoints_recv)
  integer(i_short) info2(8,npoints_recv)
  integer(i_long) icountvar(nvars),istartvar(nvars)
  real(r_single) aspect2(npoints_recv)

  integer(i_long) i,j
  integer(i_llong) idist,idistlen,idistmax,idistmin,idjxlen,idjxylen,idjxyzlen,idjxyzx0len,idjxyzxy0len
  integer(i_long) ii,k
  integer(i_llong) ix0,ix0len,ix0max,ix0min,iy0,iy0len,iy0max,iy0min,iz0,iz0len,iz0max
  integer(i_llong) iz0min,jumpx,jumpxlen,jumpxmax,jumpxmin,jumpy,jumpylen,jumpymax,jumpymin
  integer(i_llong) jumpz,jumpzlen,jumpzmax,jumpzmin

!  sort by var first (but not with indexxi8, because destroys order of points within a subgroup)

  ii=0
  icountvar=0
  istartvar=0
  ib0=0
  do k=1,nvars
     do i=1,npoints_recv
        if(info_string(8,i)==k) then
           icountvar(k)=icountvar(k)+1
           ii=ii+1
           if(icountvar(k)==1) istartvar(k)=ii
           ib0(ii)=i
           do j=1,8
              info2(j,ii)=info_string(j,i)
           end do
           aspect2(ii)=aspect_full(i)
        end if
     end do
  end do
  if(minval(ib0)==0) then
     write(6,*)' error in sort_strings4, problem with ib0'
     call stop2(68)
  end if

!  now sort each section in usual way

!   obtain range of jumpx,jumpy,originx,originy

  ij_origin=0
  do k=1,nvars
     if(icountvar(k)==0) cycle
     jumpxmin=huge(jumpxmin) ; jumpxmax=-jumpxmin
     jumpymin=jumpxmin ; jumpymax=jumpxmax
     jumpzmin=jumpxmin ; jumpzmax=jumpxmax
     ix0min=jumpxmin ; ix0max=jumpxmax
     iy0min=jumpxmin ; iy0max=jumpxmax
     iz0min=jumpxmin ; iz0max=jumpxmax
     idistmin=jumpxmin ; idistmax=jumpxmax
     do i=istartvar(k),istartvar(k)+icountvar(k)-1
        jumpx=info2(5,i) ; jumpy=info2(6,i) ; jumpz=info2(7,i)
        ix0=info2(2,i) ; iy0=info2(3,i) ; iz0=info2(4,i)
        idist=info2(1,i)
        jumpxmin=min(jumpx,jumpxmin) ; jumpxmax=max(jumpx,jumpxmax)
        jumpymin=min(jumpy,jumpymin) ; jumpymax=max(jumpy,jumpymax)
        jumpzmin=min(jumpz,jumpzmin) ; jumpzmax=max(jumpz,jumpzmax)
        ix0min=min(ix0,ix0min) ; ix0max=max(ix0,ix0max)
        iy0min=min(iy0,iy0min) ; iy0max=max(iy0,iy0max)
        iz0min=min(iz0,iz0min) ; iz0max=max(iz0,iz0max)
        idistmin=min(idist,idistmin) ; idistmax=max(idist,idistmax)
     end do
     jumpxlen=jumpxmax-jumpxmin+1 ; jumpylen=jumpymax-jumpymin+1 ; jumpzlen=jumpzmax-jumpzmin+1
     idistlen=idistmax-idistmin+1
     ix0len=ix0max-ix0min+1 ; iy0len=iy0max-iy0min+1 ; iz0len=iz0max-iz0min+1
     idjxlen=idistlen*jumpxlen
     idjxylen=idjxlen*jumpylen
     idjxyzlen=idjxylen*jumpzlen
     idjxyzx0len=idjxyzlen*ix0len
     idjxyzxy0len=idjxyzx0len*iy0len

     do i=istartvar(k),istartvar(k)+icountvar(k)-1
        jumpx=info2(5,i) ; jumpy=info2(6,i) ; jumpz=info2(7,i)
        ix0=info2(2,i) ; iy0=info2(3,i) ; iz0=info2(4,i)
        idist=info2(1,i)
        ij_origin(i)=idist-idistmin &
                     +idistlen*(jumpx-jumpxmin) &
                        +idjxlen*(jumpy-jumpymin) &
                           +idjxylen*(jumpz-jumpzmin) &
                              +idjxyzlen*(ix0-ix0min) &
                                 +idjxyzx0len*(iy0-iy0min) &
                                    +idjxyzxy0len*(iz0-iz0min)
     end do
     call indexxi8(icountvar(k),ij_origin(istartvar(k)),ib1(istartvar(k)))
     do i=istartvar(k),istartvar(k)+icountvar(k)-1
        ib1(i)=ib1(i)+istartvar(k)-1
     end do
  end do
  do i=1,npoints_recv
     ib(i)=ib0(ib1(i))
  end do
  do i=1,npoints_recv
     do j=1,8
        info_string(j,i)=info2(j,ib1(i))
     end do
     aspect_full(i)=aspect2(ib1(i))
  end do

end subroutine sort_strings4

SUBROUTINE string_assemble4(i1filter,i2filter,nstrings,label_string, &
                     npoints_send,npoints_recv,aspect,icolor, &
                     info_string,aspect_full,nsend,ndsend,nrecv,ndrecv,ia,ja,ka, &
                     ips,ipe,jps,jpe,kps,kpe,mype,npes)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    string_assemble4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     i1filter                     - i1filter(1-3,.)=jumpx,jumpy,jumpz
!     i2filter                     - i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar
!     nstrings                     -
!     label_string                 -  label_string(1-3,.)=originx,originy,originz
!     npoints_send                 -  number of points to send for assembling strings
!     npoints_recv                 -  number of points for assembled strings
!     aspect                       - aspect tensor numbers
!     icolor                       -
!     ips, ipe, jps, jpe, kps, kpe - patch indices
!     mype                         - mpi task id
!     npes                         -
!
!   output argument list:
!     info_string                  - 1---- distance from origin to current point
!                                  - 2,3,4-- origin coordinates
!                                  - 5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
!     aspect_full                  -
!     nsend,ndsend,nrecv,ndrecv
!     ia,ja,ka
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                            ,INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe      ! patch indices
  INTEGER(i_long)                                            ,INTENT(IN   ) :: mype, npes

  INTEGER(i_long)                                            ,INTENT(IN   ) :: nstrings,icolor

  INTEGER(i_short), DIMENSION( 3, * )                        ,INTENT(IN   ) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, * )                        ,INTENT(IN   ) :: &
            i2filter                       !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_short), DIMENSION( 6, * )                        ,INTENT(IN   ) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5-6,.)=dest pe, ivar

  INTEGER(i_long)                                            ,INTENT(IN   ) :: &
            npoints_send, &         !  number of points to send for assembling strings
            npoints_recv            !  number of points for assembled strings

  real(r_single)  , DIMENSION( 7, ips:ipe, jps:jpe, kps:kpe ),INTENT(IN   ) :: &
            aspect                   !  aspect tensor numbers (recursive filter parameters derived
                                     !            from these)
  INTEGER(i_short), DIMENSION( 8, max(1,npoints_recv) )   ,INTENT(  OUT) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single), DIMENSION( max(1,npoints_recv) )        ,INTENT(  OUT) :: &
            aspect_full
  integer(i_long)                                            ,intent(  out) :: &
            nsend(0:npes-1),ndsend(0:npes),nrecv(0:npes-1),ndrecv(0:npes)
  integer(i_short)                                           ,intent(  out) :: ia(npoints_send),ja(npoints_send),ka(npoints_send)

  integer(i_short) string_info(8,npoints_send)
  real(r_single) full_aspect(npoints_send)

  integer(i_long) i,i0,idestpe,idist,ierr,ivar,j,j0,jumpx,jumpy,jumpz,k,k0,kk,len,m,mbuf,mpe,mpi_string1
  integer(i_long) idestlast

  mbuf=0

!       setup string_info array

  nsend=0
  if(nstrings>0) then
     idestlast=-1
     do m=1,nstrings
        len=i2filter(4,m)
        jumpx=i1filter(1,m) ; jumpy=i1filter(2,m) ; jumpz=i1filter(3,m)
        i=i2filter(1,m) ; j=i2filter(2,m) ; k=i2filter(3,m) ; ivar=i2filter(5,m)
        i0=label_string(1,m) ; j0=label_string(2,m) ; k0=label_string(3,m)
        idist=label_string(4,m)
        idestpe=label_string(5,m)
        if(idestpe<idestlast) then
           write(6,*)'STRING_ASSEMBLE4:  ***PROBLEM*** destination pes out of order for label_string'
           call stop2(68)
        end if
        idestlast=idestpe
        do kk=1,len
           mbuf=mbuf+1
           string_info(1,mbuf)=idist
           string_info(2,mbuf)=i0 ; string_info(3,mbuf)=j0 ; string_info(4,mbuf)=k0
           string_info(5,mbuf)=jumpx ; string_info(6,mbuf)=jumpy ; string_info(7,mbuf)=jumpz
           string_info(8,mbuf)=ivar
           ia(mbuf)=i ; ja(mbuf)=j ; ka(mbuf)=k
           nsend(idestpe)=nsend(idestpe)+1
           full_aspect(mbuf)=aspect(icolor,i,j,k)
           i=i+jumpx ; j=j+jumpy ; k=k+jumpz
           if(idist>=0) idist=idist+1
        end do
     end do
  end if
  if(mbuf/=npoints_send) then
     write(6,*)'STRING_ASSEMBLE4:   ***PROBLEM***  mbuf /= npoints_send, mype,mbuf,npoints_send=', &
          mype,mbuf,npoints_send
     call stop2(68)
  end if

!  now get remaining info necessary for using alltoall command

  ndsend(0)=0
  do mpe=1,npes
     ndsend(mpe)=ndsend(mpe-1)+nsend(mpe-1)
  end do

  if(npes==1) then
     nrecv(0)=nsend(0)
  else
     call mpi_alltoall(nsend,1,mpi_integer, &
         nrecv,1,mpi_integer,mpi_comm_world,ierr)
  end if
  ndrecv(0)=0
  do mpe=1,npes
     ndrecv(mpe)=ndrecv(mpe-1)+nrecv(mpe-1)
  end do
  if(npes==1) then
     do j=1,max(1,npoints_recv)
        do i=1,8
           info_string(i,j)=string_info(i,j)
        end do
        aspect_full(j)=full_aspect(j)
     end do
  else
     call mpi_type_contiguous(8,mpi_integer2,mpi_string1,ierr)
     call mpi_type_commit(mpi_string1,ierr)
     call mpi_alltoallv(string_info,nsend,ndsend,mpi_string1, &
                        info_string,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierr)
     call mpi_type_free(mpi_string1,ierr)
     call mpi_alltoallv(full_aspect,nsend,ndsend,mpi_real4, &
                        aspect_full,nrecv,ndrecv,mpi_real4,mpi_comm_world,ierr)
  end if

end subroutine string_assemble4

SUBROUTINE string_label(i1filter,i2filter,nstrings,label_string,npoints_recv, &
                     nvars,kvar_start,kvar_end, &
                     ids, ide, jds, jde, kds, kde, &                          ! domain indices
                     ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
                     mype, npes)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    string_label
!
!   prgrmmr:
!
! abstract:  assign global string labels to each string
!             (global label is first i,j,k inside global domain)
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     i1filter                     -  i1filter(1-3,.)=jumpx,jumpy,jumpz
!     i2filter                     -  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar
!     nstrings                     -
!     npoints_recv                 -  number of points for assembled strings
!     nvars
!     kvar_start,kvar_end
!     ids, ide, jds, jde, kds, kde - domain indices
!     ips, ipe, jps, jpe, kps, kpe - patch indices
!     mype                         - mpi task id
!     npes                         -
!
!   output argument list:
!     i1filter                     -  i1filter(1-3,.)=jumpx,jumpy,jumpz
!     i2filter                     -  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar
!     label_string                 -  label_string(1-3,.)=originx,originy,originz
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                    ,INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                                                       ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                    ,intent(in   ) :: nstrings

  INTEGER(i_short), DIMENSION( 3, * ),INTENT(INout) :: &
            i1filter                       !  i1filter(1-3,.)=jumpx,jumpy,jumpz

  INTEGER(i_short), DIMENSION( 5, * ),INTENT(INout) :: &
            i2filter                       !  i2filter(1-5,.)=beginx,beginy,beginz,lenstring,ivar

  INTEGER(i_short), DIMENSION( 6, * ),INTENT(  OUT) :: &
            label_string            !  label_string(1-3,.)=originx,originy,originz
                                    !  label_string(4,.)=distance from origin to start of string
                                    !  label_string(5,.)=destination pe for string piece
                                    !  label_string(6,.)=ivar

  integer(i_long)                    ,intent(in   ) :: mype,npes
  integer(i_long)                    ,intent(  out) :: npoints_recv(0:npes-1)
  integer(i_long)                    ,intent(in   ) :: nvars
  integer(i_long)                    ,intent(in   ) :: kvar_start(nvars),kvar_end(nvars)

  integer(i_long) i,idist,idisttest,ierr,istring_pe,itest,ivar,ivar_end,ivar_start
  integer(i_long) j,jtest,jumpx,jumpy,jumpz,k,ktest,mpe,n,nstrings0
  integer(i_llong) lastlabel

  INTEGER(i_short), DIMENSION( 6, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: label_string2
  INTEGER(i_short), DIMENSION( 3, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: i1filter2
  INTEGER(i_short), DIMENSION( 5, (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1) ) :: i2filter2

  integer(i_llong) labelijk(max(1,nstrings))
  integer(i_long) nrecv(0:npes-1),ndrecv(0:npes)
  integer(i_llong),allocatable::labelijk0(:)
  integer(i_long),allocatable::index(:)

  if(nstrings>0) then
     do n=1,nstrings

        jumpx=i1filter(1,n) ; jumpy=i1filter(2,n) ; jumpz=i1filter(3,n)
        i=i2filter(1,n) ; j=i2filter(2,n) ; k=i2filter(3,n) ; ivar=i2filter(5,n)
        ivar_start=kvar_start(ivar)
        ivar_end=kvar_end(ivar)
        idist=0
        do
           idisttest=idist+1
           ktest=k-jumpz
           if(ktest<ivar_start) then
              label_string2(1,n)=i ; label_string2(2,n)=j ; label_string2(3,n)=k
              label_string2(4,n)=idist ; label_string2(6,n)=ivar
              exit
           end if
           itest=i-jumpx
           if(itest<ids.or.itest>ide) then
              label_string2(1,n)=i ; label_string2(2,n)=j ; label_string2(3,n)=k
              label_string2(4,n)=idist ; label_string2(6,n)=ivar
              exit
           end if
           jtest=j-jumpy
           if(jtest<jds.or.jtest>jde) then
              label_string2(1,n)=i ; label_string2(2,n)=j ; label_string2(3,n)=k
              label_string2(4,n)=idist ; label_string2(6,n)=ivar
              exit
           end if
           i=itest ; j=jtest ; k=ktest
           idist=idisttest
        end do
        ivar=label_string2(6,n)
        labelijk(n)=label_string2(1,n)+(ide-ids+1)*(label_string2(2,n)-1+(jde-jds+1)*(label_string2(3,n)-1 &
                               +(kde-kds+1)*(ivar-1)))

     end do
  end if

!--  assemble all string labels to pe 0, for assignment of pe numbers

  nrecv=0
  if(npes==1) then
     nrecv(0)=nstrings
  else
     call mpi_allgather(nstrings,1,mpi_integer4,nrecv,1,mpi_integer4,mpi_comm_world,ierr)
  end if
  ndrecv(0)=0
  do i=1,npes
     ndrecv(i)=ndrecv(i-1)+nrecv(i-1)
  end do
  nstrings0=ndrecv(npes)
  allocate(labelijk0(nstrings0))
  if(npes==1) then
     do i=1,nstrings0
        labelijk0(i)=labelijk(i)
     end do
  else
     call my_gatherv8(labelijk,nstrings,labelijk0,nstrings0,nrecv,ndrecv,npes)
  end if

!------ sort strings so strings with same labels are adjacent, then assign adjacent strings to same pe.
!------   then when we assemble all pieces, it is guaranteed that all pieces of every contiguous string
!------   will end up on the same processor.

  if(mype==0) then
     allocate(index(nstrings0))
     call indexxi8(nstrings0,labelijk0,index)
     lastlabel=-huge(lastlabel)
     istring_pe=0
     do i=1,nstrings0
        j=index(i)
        if(labelijk0(j)/=lastlabel) then
           lastlabel=labelijk0(j)
           istring_pe=mod(istring_pe+1,npes)
        end if
        labelijk0(j)=istring_pe
     end do
     deallocate(index)
  end if

!---- now scatter pe destination numbers back

  if(npes==1) then
     do i=1,nstrings0
        labelijk(i)=labelijk0(i)
     end do
  else
     call my_scatterv8(labelijk0,nstrings0,labelijk,nstrings,nrecv,ndrecv,npes)
  end if
  deallocate(labelijk0)

!---- assign destination pe numbers and count up number of points at each destination pe

  nrecv=0
  if(nstrings>0) then
     do i=1,nstrings
        mpe=labelijk(i)
        nrecv(mpe)=nrecv(mpe)+i2filter(4,i)
        label_string2(5,i)=mpe
     end do

!---- reorder label_string in order of increasing pe number
     allocate(index(nstrings))
     call indexxi8(nstrings,labelijk,index)
     do i=1,nstrings
        do j=1,6
           label_string(j,i)=label_string2(j,index(i))
        end do
        do j=1,3
           i1filter2(j,i)=i1filter(j,index(i))
        end do
        do j=1,5
           i2filter2(j,i)=i2filter(j,index(i))
        end do
     end do
     do i=1,nstrings
        do j=1,3
           i1filter(j,i)=i1filter2(j,i)
        end do
        do j=1,5
           i2filter(j,i)=i2filter2(j,i)
        end do
     end do
     deallocate(index)
  end if
  if(npes==1) then
     npoints_recv=nrecv
  else
     call mpi_allreduce(nrecv,npoints_recv,npes,mpi_integer4,mpi_sum,mpi_comm_world,ierr)
  end if

end subroutine string_label

subroutine my_gatherv8(local,nlocal,global,nglobal,nrecv,ndrecv,npes)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    my_gatherv8
!   prgmmr:
!
! abstract: workaround for possible problem with mpi_gatherv failure when nlocal = 0 for some processors
!
! program history log:
!   2009-08-27  lueken - added subprogram doc block
!
!   input argument list:
!    nlocal
!    npes
!    nglobal
!    local
!    global
!    nrecv
!    ndrecv
!
!   output argument list:
!    global
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_long) ,intent(in   ) :: nlocal,npes,nglobal
integer(i_llong),intent(in   ) :: local(max(1,nlocal))
integer(i_llong),intent(inout) :: global(max(1,nglobal))
integer(i_long) ,intent(in   ) :: nrecv(0:npes-1),ndrecv(0:npes)

integer(i_long) nrecv1(0:npes-1),ndrecv1(0:npes)
integer(i_long) i,n,nlocal1,ierr
integer(i_llong) local1(max(1,nlocal))
integer(i_llong) global1(nglobal+npes+1)

do i=0,npes-1
   nrecv1(i)=max(nrecv(i),1)
end do
ndrecv1(0)=0
do i=1,npes
   ndrecv1(i)=ndrecv1(i-1)+nrecv1(i-1)
end do
local1(1)=0
if(nlocal>0) then
   do i=1,nlocal
      local1(i)=local(i)
   end do
end if
nlocal1=max(1,nlocal)
call mpi_gatherv(local1,nlocal1,mpi_integer8,global1,nrecv1,ndrecv1,mpi_integer8,0,mpi_comm_world,ierr)
do n=0,npes-1
   if(nrecv(n)>0) then
      do i=1,nrecv(n)
         global(i+ndrecv(n))=global1(i+ndrecv1(n))
      end do
   end if
end do

end subroutine my_gatherv8

subroutine my_scatterv8(global,nglobal,local,nlocal,nrecv,ndrecv,npes)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    my_scatterv8
!   prgmmr:
!
! abstract: workaround for possible problem with mpi_scatterv failure when nlocal = 0 for some processors
!
! program history log:
!   2009-08-27  lueken - added subprogram doc block
!
!   input argument list:
!    nlocal
!    npes
!    global
!    nrecv
!    ndrecv
!    local
!    nglobal
!
!   output argument list:
!    local
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_long) ,intent(in   ) :: nlocal,npes,nglobal
integer(i_llong),intent(inout) :: local(max(1,nlocal))
integer(i_llong),intent(in   ) :: global(max(1,nglobal))
integer(i_long) ,intent(in   ) :: nrecv(0:npes-1),ndrecv(0:npes)

integer(i_long) nrecv1(0:npes-1),ndrecv1(0:npes)
integer(i_long) i,n,nlocal1,ierr
integer(i_llong) local1(max(1,nlocal))
integer(i_llong) global1(nglobal+npes+1)

do i=0,npes-1
   nrecv1(i)=max(nrecv(i),1)
end do
ndrecv1(0)=0
do i=1,npes
   ndrecv1(i)=ndrecv1(i-1)+nrecv1(i-1)
end do
do n=0,npes-1
   if(nrecv(n)>0) then
      do i=1,nrecv(n)
         global1(i+ndrecv1(n))=global(i+ndrecv(n))
      end do
   else
      global1(1+ndrecv1(n))=0
   end if
end do
nlocal1=max(1,nlocal)
call mpi_scatterv(global1,nrecv1,ndrecv1,mpi_integer8,local1,nlocal1,mpi_integer8,0,mpi_comm_world,ierr)
if(nlocal>0) then
   do i=1,nlocal
      local(i)=local1(i)
   end do
else
   local(1)=0
end if

end subroutine my_scatterv8

subroutine what_color_is(i1,i2,i3,color)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    what_color_is
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     i1, i2, i3 -
!
!   output argument list:
!     color      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

implicit none

integer(i_long),intent(IN   ) :: i1,i2,i3
integer(i_long),intent(  OUT) :: color

integer(i_long),dimension(3):: v,vh,vh2,b124
logical same
integer(i_long):: i,itest
data b124/1,2,4/
!----------------------------------------------------------------
vh(1)=i1; vh(2)=i2; vh(3)=i3
do itest=1,20
   v=vh; vh=v/2; vh2=vh*2
!  if(.NOT.same(vh2,v,3))exit
   same=.true. ; do i=1,3; if(vh2(i)/=v(i)) same=.false. ; enddo
   if(.not.same) exit
enddo
v=modulo(v,2)
color=dot_product(v,b124)
end subroutine what_color_is

subroutine add_halox0(filter,nrows,ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_halox0
!   prgmmr:
!
! abstract: setup everything needed for adding halo rows when required
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    nrows
!    ids,ide,jds,jde                     - domain indices
!    ips,ipe,jps,jpe                     - patch indices
!    mype                                - mpi task id
!    npes
!    filter
!
!   output argument list:
!    filter
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  TYPE(filter_cons),INTENT(inOUT) :: filter
  integer(i_long)  ,intent(in   ) :: nrows
  integer(i_long)  ,intent(in   ) :: ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes

  integer(i_long) ijglob_pe(ids:ide,jds:jde),ijglob_pe0(ids:ide,jds:jde)
  integer(i_long) nrecv_halo(0:npes-1),ndrecv_halo(0:npes)
  integer(i_long) nsend_halo(0:npes-1),ndsend_halo(0:npes)
  integer(i_long) i,i0,ii,ir,istat,j,mpe
  integer(i_long) info_recv_halo(2,npes*2*nrows*(ide-ids+1))
  integer(i_long) info_send_halo(2,npes*2*nrows*(ide-ids+1))
  integer(i_long) iorigin(npes*2*nrows*(ide-ids+1))
  integer(i_long) indx(npes*2*nrows*(ide-ids+1))
  integer(i_long) iwork(npes*2*nrows*(ide-ids+1))
  integer(i_long) nrecv_halo_loc,nsend_halo_loc,mpi_string1,ierror

  if(npes==1.or.nrows==0) return
  if(ips==ids.and.ipe==ide) return

  ijglob_pe0=0
  do j=jps,jpe
     do i=ips,ipe
        ijglob_pe0(i,j)=mype
     end do
  end do
  call mpi_allreduce(ijglob_pe0,ijglob_pe,(ide-ids+1)*(jde-jds+1),mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!  create list of all points to be recieved
  ii=0
  nrecv_halo=0
  do i0=ips-nrows,ipe+1,ipe+1-ips+nrows
     do ir=0,nrows-1
        i=i0+ir
        if(i<ids.or.i>ide) cycle
        do j=jps,jpe
           ii=ii+1
           info_recv_halo(1,ii)=i ; info_recv_halo(2,ii)=j
           iorigin(ii)=ijglob_pe(i,j)
           nrecv_halo(ijglob_pe(i,j))=nrecv_halo(ijglob_pe(i,j))+1
        end do
     end do
  end do

  ndrecv_halo(0)=0
  do mpe=1,npes
     ndrecv_halo(mpe)=ndrecv_halo(mpe-1)+nrecv_halo(mpe-1)
  end do

  call mpi_alltoall(nrecv_halo,1,mpi_integer4,nsend_halo,1,mpi_integer4,mpi_comm_world,ierror)
  ndsend_halo(0)=0
  do mpe=1,npes
     ndsend_halo(mpe)=ndsend_halo(mpe-1)+nsend_halo(mpe-1)
  end do
  nsend_halo_loc=ndsend_halo(npes)
  nrecv_halo_loc=ndrecv_halo(npes)

!   sort origin pe numbers from smallest to largest
  if(ii>0) then
     call indexxi4(ii,iorigin,indx)

!     use sort index to reorder
     do j=1,2
        do i=1,ii
           iwork(i)=info_recv_halo(j,indx(i))
        end do
        do i=1,ii
           info_recv_halo(j,i)=iwork(i)
        end do
     end do
  end if

  call mpi_type_contiguous(2_i_long,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_recv_halo,nrecv_halo,ndrecv_halo,mpi_string1, &
                     info_send_halo,nsend_halo,ndsend_halo,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

  filter%nsend_halox_loc=nsend_halo_loc
  filter%nrecv_halox_loc=nrecv_halo_loc
  deallocate(filter%nrecv_halox,stat=istat)
  allocate(filter%nrecv_halox(0:npes-1))
  do i=0,npes-1
     filter%nrecv_halox(i)=nrecv_halo(i)
  end do
  deallocate(filter%ndrecv_halox,stat=istat)
  allocate(filter%ndrecv_halox(0:npes))
  do i=0,npes
     filter%ndrecv_halox(i)=ndrecv_halo(i)
  end do
  deallocate(filter%nsend_halox,stat=istat)
  allocate(filter%nsend_halox(0:npes-1))
  do i=0,npes-1
     filter%nsend_halox(i)=nsend_halo(i)
  end do
  deallocate(filter%ndsend_halox,stat=istat)
  allocate(filter%ndsend_halox(0:npes))
  do i=0,npes
     filter%ndsend_halox(i)=ndsend_halo(i)
  end do
  deallocate(filter%info_send_halox,stat=istat)
  allocate(filter%info_send_halox(2,nsend_halo_loc))
  do i=1,nsend_halo_loc
     do j=1,2
        filter%info_send_halox(j,i)=info_send_halo(j,i)
     end do
  end do
  deallocate(filter%info_recv_halox,stat=istat)
  allocate(filter%info_recv_halox(2,nrecv_halo_loc))
  do i=1,nrecv_halo_loc
     do j=1,2
        filter%info_recv_halox(j,i)=info_recv_halo(j,i)
     end do
  end do

end subroutine add_halox0

subroutine add_haloy0(filter,nrows,ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_haloy0
!   prgmmr:
!
! abstract: setup everything needed for adding halo rows when required
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    nrows
!    ids,ide,jds,jde      - domain indices
!    ips,ipe,jps,jpe      - patch indices
!    mype                 - mpi task id
!    npes
!    filter
!
!   output argument list:
!    filter
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  TYPE(filter_cons),INTENT(inout) :: filter
  integer(i_long)  ,intent(in   ) :: nrows
  integer(i_long)  ,intent(in   ) :: ids,ide,jds,jde,ips,ipe,jps,jpe,mype,npes

  integer(i_long) ijglob_pe(ids:ide,jds:jde),ijglob_pe0(ids:ide,jds:jde)
  integer(i_long) nrecv_halo(0:npes-1),ndrecv_halo(0:npes)
  integer(i_long) nsend_halo(0:npes-1),ndsend_halo(0:npes)
  integer(i_long) i,ii,istat,j,j0,jr,mpe
  integer(i_long) info_recv_halo(2,npes*2*nrows*(jde-jds+1))
  integer(i_long) info_send_halo(2,npes*2*nrows*(jde-jds+1))
  integer(i_long) iorigin(npes*2*nrows*(jde-jds+1))
  integer(i_long) indx(npes*2*nrows*(jde-jds+1))
  integer(i_long) iwork(npes*2*nrows*(jde-jds+1))
  integer(i_long) nrecv_halo_loc,nsend_halo_loc,mpi_string1,ierror

  if(npes==1.or.nrows==0) return
  if(jps==jds.and.jpe==jde) return

  ijglob_pe0=0
  do j=jps,jpe
     do i=ips,ipe
        ijglob_pe0(i,j)=mype
     end do
  end do
  call mpi_allreduce(ijglob_pe0,ijglob_pe,(ide-ids+1)*(jde-jds+1),mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!  create list of all points to be recieved
  ii=0
  nrecv_halo=0
  do j0=jps-nrows,jpe+1,jpe+1-jps+nrows
     do jr=0,nrows-1
        j=j0+jr
        if(j<jds.or.j>jde) cycle
        do i=ips,ipe
           ii=ii+1
           info_recv_halo(1,ii)=i ; info_recv_halo(2,ii)=j
           iorigin(ii)=ijglob_pe(i,j)
           nrecv_halo(ijglob_pe(i,j))=nrecv_halo(ijglob_pe(i,j))+1
        end do
     end do
  end do

  ndrecv_halo(0)=0
  do mpe=1,npes
     ndrecv_halo(mpe)=ndrecv_halo(mpe-1)+nrecv_halo(mpe-1)
  end do

  call mpi_alltoall(nrecv_halo,1,mpi_integer4,nsend_halo,1,mpi_integer4,mpi_comm_world,ierror)
  ndsend_halo(0)=0
  do mpe=1,npes
     ndsend_halo(mpe)=ndsend_halo(mpe-1)+nsend_halo(mpe-1)
  end do
  nsend_halo_loc=ndsend_halo(npes)
  nrecv_halo_loc=ndrecv_halo(npes)

!   sort origin pe numbers from smallest to largest
  if(ii>0) then
     call indexxi4(ii,iorigin,indx)

!     use sort index to reorder
     do j=1,2
        do i=1,ii
           iwork(i)=info_recv_halo(j,indx(i))
        end do
        do i=1,ii
           info_recv_halo(j,i)=iwork(i)
        end do
     end do
  end if

  call mpi_type_contiguous(2_i_long,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_recv_halo,nrecv_halo,ndrecv_halo,mpi_string1, &
                     info_send_halo,nsend_halo,ndsend_halo,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

  filter%nsend_haloy_loc=nsend_halo_loc
  filter%nrecv_haloy_loc=nrecv_halo_loc
  deallocate(filter%nrecv_haloy,stat=istat)
  allocate(filter%nrecv_haloy(0:npes-1))
  do i=0,npes-1
     filter%nrecv_haloy(i)=nrecv_halo(i)
  end do
  deallocate(filter%ndrecv_haloy,stat=istat)
  allocate(filter%ndrecv_haloy(0:npes))
  do i=0,npes
     filter%ndrecv_haloy(i)=ndrecv_halo(i)
  end do
  deallocate(filter%nsend_haloy,stat=istat)
  allocate(filter%nsend_haloy(0:npes-1))
  do i=0,npes-1
     filter%nsend_haloy(i)=nsend_halo(i)
  end do
  deallocate(filter%ndsend_haloy,stat=istat)
  allocate(filter%ndsend_haloy(0:npes))
  do i=0,npes
     filter%ndsend_haloy(i)=ndsend_halo(i)
  end do
  deallocate(filter%info_send_haloy,stat=istat)
  allocate(filter%info_send_haloy(2,nsend_halo_loc))
  do i=1,nsend_halo_loc
     do j=1,2
        filter%info_send_haloy(j,i)=info_send_halo(j,i)
     end do
  end do
  deallocate(filter%info_recv_haloy,stat=istat)
  allocate(filter%info_recv_haloy(2,nrecv_halo_loc))
  do i=1,nrecv_halo_loc
     do j=1,2
        filter%info_recv_haloy(j,i)=info_recv_halo(j,i)
     end do
  end do

end subroutine add_haloy0

subroutine add_halo_x(f,g,filter,ngauss,nrows,ids,ide,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_halo_x
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    filter
!    ngauss,nrows
!    ids,ide                  - domain indices
!    ips,ipe,jps,jpe,kps,kpe  - patch indices
!    npes
!    f
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  TYPE(filter_cons),INTENT(in   ) :: filter
  integer(i_long)  ,intent(in   ) :: ngauss,nrows
  integer(i_long)  ,intent(in   ) :: ids,ide,ips,ipe,jps,jpe,kps,kpe,npes
  real(r_single)   ,intent(in   ) :: f(ngauss,ips:ipe,jps:jpe,kps:kpe)
  real(r_single)   ,intent(  out) :: g(ngauss,max(ids,ips-nrows):min(ide,ipe+nrows),jps:jpe,kps:kpe)

  integer(i_long) i,j,k,mpi_string2,n,ierror
  real(r_single),allocatable:: bufsend(:,:,:),bufrecv(:,:,:)

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=f(n,i,j,k)
           end do
        end do
     end do
  end do

  if(npes==1.or.nrows==0.or.(ips==ids.and.ipe==ide)) return

!   now gather up points to send

  allocate(bufsend(ngauss,kps:kpe,filter%nsend_halox_loc),bufrecv(ngauss,kps:kpe,filter%nrecv_halox_loc))
  do i=1,filter%nsend_halox_loc
     do k=kps,kpe
        do n=1,ngauss
           bufsend(n,k,i)=f(n,filter%info_send_halox(1,i),filter%info_send_halox(2,i),k)
        end do
     end do
  end do
  call mpi_type_contiguous(ngauss*(kpe-kps+1),mpi_real4,mpi_string2,ierror)
  call mpi_type_commit(mpi_string2,ierror)
  call mpi_alltoallv(bufsend,filter%nsend_halox,filter%ndsend_halox,mpi_string2, &
                     bufrecv,filter%nrecv_halox,filter%ndrecv_halox,mpi_string2,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string2,ierror)
!   finally distribute points back
  do i=1,filter%nrecv_halox_loc
     do k=kps,kpe
        do n=1,ngauss
           g(n,filter%info_recv_halox(1,i),filter%info_recv_halox(2,i),k)=bufrecv(n,k,i)
        end do
     end do
  end do
  deallocate(bufsend,bufrecv)

end subroutine add_halo_x

subroutine add_halo_y(f,g,filter,ngauss,nrows,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_halo_y
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    filter
!    ngauss,nrows
!    jds,jde                    - domain indices
!    ips,ipe,jps,jpe,kps,kpe    - patch indices
!    npes
!    f
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  TYPE(filter_cons),INTENT(in   ) :: filter
  integer(i_long)  ,intent(in   ) :: ngauss,nrows
  integer(i_long)  ,intent(in   ) :: jds,jde,ips,ipe,jps,jpe,kps,kpe,npes
  real(r_single)   ,intent(in   ) :: f(ngauss,ips:ipe,jps:jpe,kps:kpe)
  real(r_single)   ,intent(  out) :: g(ngauss,ips:ipe,max(jds,jps-nrows):min(jde,jpe+nrows),kps:kpe)

  integer(i_long) i,j,k,mpi_string2,n,ierror
  real(r_single),allocatable:: bufsend(:,:,:),bufrecv(:,:,:)

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=f(n,i,j,k)
           end do
        end do
     end do
  end do

  if(npes==1.or.nrows==0.or.(jps==jds.and.jpe==jde)) return

!   now gather up points to send

  allocate(bufsend(ngauss,kps:kpe,filter%nsend_haloy_loc),bufrecv(ngauss,kps:kpe,filter%nrecv_haloy_loc))
  do i=1,filter%nsend_haloy_loc
     do k=kps,kpe
        do n=1,ngauss
           bufsend(n,k,i)=f(n,filter%info_send_haloy(1,i),filter%info_send_haloy(2,i),k)
        end do
     end do
  end do
  call mpi_type_contiguous(ngauss*(kpe-kps+1),mpi_real4,mpi_string2,ierror)
  call mpi_type_commit(mpi_string2,ierror)
  call mpi_alltoallv(bufsend,filter%nsend_haloy,filter%ndsend_haloy,mpi_string2, &
                     bufrecv,filter%nrecv_haloy,filter%ndrecv_haloy,mpi_string2,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string2,ierror)
!   finally distribute points back
  do i=1,filter%nrecv_haloy_loc
     do k=kps,kpe
        do n=1,ngauss
           g(n,filter%info_recv_haloy(1,i),filter%info_recv_haloy(2,i),k)=bufrecv(n,k,i)
        end do
     end do
  end do
  deallocate(bufsend,bufrecv)

end subroutine add_halo_y

!!!!!!!!!!!!!!!!!!!!!!!!!!!  following is new code for implementation of approximate 4th order factorization
!!!!!!!!!!!!!!!!!!!!!!!!!!!      which yields almost a 4th order result for the cost of a 2nd order filter.
!!!!!!!!!!!!!!!!!!!!!!!!!!!  this is because the approximate factorization breaks the 4th order operator
!!!!!!!!!!!!!!!!!!!!!!!!!!!    into 4 factors, which are then recombined in such a way that the last

!???????????? -- there may still be a bug in the new factorization code, because there is
!????????????     bad behaviour at the boundaries for homogeneous, isotropic run.
!???????????      however, in the interior, new filter matches very well with exact 4th order.

subroutine one_color4_new_factorization(g,filter,ngauss,ipass,ifilt_ord, &
             nstrings,istart,ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    one_color4_new_factorization
!   prgmmr:
!
! abstract: apply one forward-backward recursive filter for one color.
!            use new approximate 4th order factorization
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    ips,ipe,jps,jpe,kps,kpe      - patch indices
!    npes
!    ngauss,iadvance,iback
!    ipass                        - total number of contiguous string points
!    ifilt_ord
!    nstrings
!    istart
!    filter
!    g
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
     ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
     npes,ngauss,iadvance,iback

  INTEGER(i_long)                                              ,INTENT(IN   ) :: &
            ipass          !  total number of contiguous string points
  integer(i_long)                                              ,intent(in   ) :: ifilt_ord

  real(r_single), DIMENSION( ngauss,ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long)                                              ,intent(in   ) :: nstrings
  integer(i_long)                                              ,intent(in   ) :: istart(nstrings+1)
  type(filter_cons)                                            ,intent(in   ) :: filter

  real(r_single) work(ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           work(igauss,i,1)=g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,i,2)=work(igauss,i,1)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                     work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv>0) then
     do igauss=1,ngauss
        do i=1,filter%npoints_recv
           work2(i)=work(igauss,filter%ib(i),2)
        end do

        do j=1,nstrings
           do i=istart(j),istart(j+1)-1
              do l=1,min(ifilt_ord,i-istart(j))
                 work2(i)=work2(i)-filter%fmat(l,i,ipass,igauss,iadvance)*work2(i-l)
              end do
              work2(i)=filter%fmat0(i,ipass,igauss,iadvance)*work2(i)
           end do
           do i=istart(j+1)-1,istart(j),-1
              do l=1,min(ifilt_ord,istart(j+1)-i-1)
                 work2(i)=work2(i)-filter%fmat(l,i+l,ipass,igauss,iback)*work2(i+l)
              end do
              work2(i)=filter%fmat0(i,ipass,igauss,iback)*work2(i)
           end do
        end do

        do i=1,filter%npoints_recv
           work(igauss,i,1)=work2(i)
        end do
     end do

!-- send strings back

     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,filter%ib(i),2)=work(igauss,i,1)
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(igauss,i,1)=work(igauss,i,2)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                       work(1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           g(igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(igauss,i,1)
        end do
     end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color4_new_factorization

subroutine one_color24_new_factorization(g,filter,ngauss,ipass,ifilt_ord, &
             nstrings,istart,ips,ipe,jps,jpe,kps,kpe,npes,iadvance,iback)
!$$$  subprogram documentation block
!                .      .    .                                                 .
! subprogram:    one_color24_new_factorization
!   prgmmr:
!
! abstract: apply one forward-backward recursive filter for one color
!
! program history log:
!   2009-09-29  lueken - added subprogram doc block
!
!   input argument list:
!    ips,ipe,jps,jpe,kps,kpe    - patch indices
!    npes
!    ngauss,iadvance,iback
!    ipass
!    ifilt_ord
!    nstrings
!    istart
!    filter
!    g
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
     ips, ipe, jps, jpe, kps, kpe      ! patch indices

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
     npes,ngauss,iadvance,iback

  INTEGER(i_long)                                                ,INTENT(IN   ) :: &
            ipass          !  total number of contiguous string points
  integer(i_long)                                                ,intent(in   ) :: ifilt_ord

  real(r_single), DIMENSION(2,ngauss, ips:ipe, jps:jpe, kps:kpe ),INTENT(INOUT) :: &
            g                      !  input--field on grid, output--filtered field on grid

  integer(i_long)                                                ,intent(in   ) :: nstrings
  integer(i_long)                                                ,intent(in   ) :: istart(nstrings+1)
  type(filter_cons)                                              ,intent(in   ) :: filter

  real(r_single) work(2,ngauss,max(1,filter%npointsmax),2)
  real(r_single) work2(max(1,filter%npointsmax))

  integer(i_long) i,ierr,igauss,ii,j,l,mpi_string

!-- gather up strings

  call mpi_type_contiguous(2*ngauss,mpi_real4,mpi_string,ierr)
  call mpi_type_commit(mpi_string,ierr)

  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           work(1,igauss,i,1)=g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
           work(2,igauss,i,1)=g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,i,2)=work(1,igauss,i,1)
           work(2,igauss,i,2)=work(2,igauss,i,1)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string, &
                     work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_recv>0) then
     do igauss=1,ngauss
        do ii=1,2
           do i=1,filter%npoints_recv
              work2(i)=work(ii,igauss,filter%ib(i),2)
           end do

           do j=1,nstrings
              do i=istart(j),istart(j+1)-1
                 do l=1,min(ifilt_ord,i-istart(j))
                    work2(i)=work2(i)-filter%fmat(l,i,ipass,igauss,iadvance)*work2(i-l)
                 end do
                 work2(i)=filter%fmat0(i,ipass,igauss,iadvance)*work2(i)
              end do
              do i=istart(j+1)-1,istart(j),-1
                 do l=1,min(ifilt_ord,istart(j+1)-i-1)
                    work2(i)=work2(i)-filter%fmat(l,i+l,ipass,igauss,iback)*work2(i+l)
                 end do
                 work2(i)=filter%fmat0(i,ipass,igauss,iback)*work2(i)
              end do
           end do
 
           do i=1,filter%npoints_recv
              work(ii,igauss,i,1)=work2(i)
           end do
        end do
     end do

!-- send strings back

     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,filter%ib(i),2)=work(1,igauss,i,1)
           work(2,igauss,filter%ib(i),2)=work(2,igauss,i,1)
        end do
     end do
  end if
  if(npes==1.and.filter%npoints_recv>0) then
     do i=1,filter%npoints_recv
        do igauss=1,ngauss
           work(1,igauss,i,1)=work(1,igauss,i,2)
           work(2,igauss,i,1)=work(2,igauss,i,2)
        end do
     end do
  else
     call mpi_alltoallv(work(1,1,1,2),filter%nrecv,filter%ndrecv,mpi_string, &
                       work(1,1,1,1),filter%nsend,filter%ndsend,mpi_string,mpi_comm_world,ierr)
  end if
  if(filter%npoints_send>0) then
     do i=1,filter%npoints_send
        do igauss=1,ngauss
           g(1,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(1,igauss,i,1)
           g(2,igauss,filter%ia(i),filter%ja(i),filter%ka(i))=work(2,igauss,i,1)
        end do
     end do
  end if

  call mpi_type_free(mpi_string,ierr)

end subroutine one_color24_new_factorization

subroutine new_alpha_beta4(info_string,aspect_full,rgauss,fmat,fmat0,igauss,ngauss, &
                     istart_out,npoints_mype,binomial,npass, &
                     lenbar,lenmax,lenmin,npoints1,nvars)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    new_alpha_beta4
!   prgmmr:
!
! abstract: compute recursion constants alpha and beta along unbroken strings
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    nvars
!    npoints_mype
!    npass
!    binomial
!    info_string
!    igauss,ngauss
!    rgauss
!    fmat,fmat0
!
!   output argument list:
!    fmat,fmat0
!    istart_out
!    lenmax,lenmin,npoints1
!    lenbar
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long)                               ,INTENT(IN   ) :: nvars

  INTEGER(i_long)                               ,INTENT(IN   ) :: npoints_mype,npass

  REAL(r_double)  , DIMENSION( 10, 10 )         ,INTENT(IN   ) :: binomial

  INTEGER(i_short), DIMENSION( 8, npoints_mype ),INTENT(IN   ) ::  &
            info_string      !      1---- distance from origin to current point
                             !      2,3,4-- origin coordinates
                             !      5,6,7,8-- jumpx,jumpy,jumpz,ivar for this string
  real(r_single), DIMENSION( npoints_mype )     ,INTENT(IN   ) :: &
            aspect_full
  integer(i_long)                               ,intent(in   ) :: igauss,ngauss
  real(r_double)                                ,intent(in   ) :: rgauss
  real(r_single)                                ,intent(inout) :: &
            fmat(2,npoints_mype,npass,ngauss,2),fmat0(npoints_mype,npass,ngauss,2)
  integer(i_long)                               ,intent(  out) :: istart_out(*)
  integer(i_long)                               ,intent(  out) :: lenmax(nvars),lenmin(nvars),npoints1(nvars) !  diagnostic output--to look at string
  real(r_double)                                ,intent(  out) :: lenbar(nvars)

  integer(i_long) i,iend,ipass,istart,ivar
  integer(i_long) nstrings

  nstrings=0

  istart=1
  if(npoints_mype>1) then
     do i=2,npoints_mype
        if(info_string(1,i)/=info_string(1,i-1)+1.or. &
               info_string(2,i)/=info_string(2,i-1).or. &
                   info_string(3,i)/=info_string(3,i-1).or. &
                       info_string(4,i)/=info_string(4,i-1).or. &
                           info_string(5,i)/=info_string(5,i-1).or. &
                               info_string(6,i)/=info_string(6,i-1).or. &
                                   info_string(7,i)/=info_string(7,i-1).or. &
                                       info_string(8,i)/=info_string(8,i-1)) then
           iend=i-1
           if(igauss==1) then
              ivar=info_string(8,iend)
              lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
              lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
              lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
              if(iend==istart) npoints1(ivar)=npoints1(ivar)+1
           end if
           nstrings=nstrings+1
           istart_out(nstrings)=istart
           istart_out(nstrings+1)=iend+1

           do ipass=1,npass
              call new_alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                                           fmat(1,istart,ipass,igauss,1),fmat0(istart,ipass,igauss,1), &
                                           fmat(1,istart,ipass,igauss,2),fmat0(istart,ipass,igauss,2))
           end do

           istart=iend+1
        end if
     end do
  end if
  iend=npoints_mype
  if(igauss==1) then
     ivar=info_string(8,iend)
     lenbar(ivar)=lenbar(ivar)+(iend-istart+1)
     lenmax(ivar)=max(iend-istart+1,lenmax(ivar))
     lenmin(ivar)=min(iend-istart+1,lenmin(ivar))
     if(iend==istart) npoints1(ivar)=npoints1(ivar)+1
  end if
  nstrings=nstrings+1
  istart_out(nstrings)=istart
  istart_out(nstrings+1)=iend+1
  do ipass=1,npass

     call new_alpha_betaa4(aspect_full(istart),rgauss,iend-istart+1,binomial(ipass,npass), &
                         fmat(1,istart,ipass,igauss,1),fmat0(istart,ipass,igauss,1), &
                         fmat(1,istart,ipass,igauss,2),fmat0(istart,ipass,igauss,2))
  end do

end subroutine new_alpha_beta4

subroutine new_alpha_betaa4(aspect,rgauss,ng,binomial,fmat1,fmat01,fmat2,fmat02)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    new_alpha_betaa4
!   prgmmr:
!
! abstract: compute constants for special Purser 4th order factorization
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    aspect:   correlation scale (squared, i think), grid units
!    rgauss:   multiplier of aspect, for multiple gaussians--used for fat-tail filter
!    ng:       length of string
!    binomial: weighting factors (perhaps not needed with high-order filter)
!
!   output argument list:
!    fmat1,fmat01,fmat2,fmat02 : filter parameters for special factorization
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_long)                ,INTENT(IN   ) :: ng

  REAL(r_double)                 ,INTENT(IN   ) :: binomial

  real(r_single), DIMENSION( ng ),INTENT(IN   ) :: aspect
  real(r_double)                 ,intent(in   ) :: rgauss
  real(r_single)                 ,intent(  out) :: fmat1(2,ng),fmat01(ng)
  real(r_single)                 ,intent(  out) :: fmat2(2,ng),fmat02(ng)

  real(r_double) sig(0:ng-1)
  real(r_double) fmat(0:ng-1,-2:0,2)
  integer(i_long) i

  do i=1,ng
     sig(i-1)=sqrt(rgauss*aspect(i)*binomial)
  end do
  call stringop(ng-1,sig,fmat)

  do i=1,ng
     fmat1(2,i)=fmat(i-1,-2,1)
     fmat1(1,i)=fmat(i-1,-1,1)
     fmat01(i)=one/fmat(i-1,0,1)
     fmat2(2,i)=fmat(i-1,-2,2)
     fmat2(1,i)=fmat(i-1,-1,2)
     fmat02(i)=one/fmat(i-1,0,2)
  end do

end subroutine new_alpha_betaa4

!=============================================================================
subroutine stringop(n,sig,fmat)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stringop
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    n
!    sig
!
!   output argument list:
!    fmat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_long)                     ,intent(IN   ) :: n
real(r_double),dimension(0:n)       ,intent(IN   ) :: sig
real(r_double),dimension(0:n,-2:0,2),intent(  OUT) :: fmat
!----------------------------------------------------------------------------
complex(r_double),dimension(2)                  :: rts
real(r_double),   dimension(0:n,-1:1)           :: dmat
real(r_double)                                  :: dmatt
integer(i_long)                                 :: i,im
data rts/ &
 (-3.4588884621354108_r_double,  1.7779487522437312_r_double), &
 (-0.54111153786458903_r_double, 5.0095518087248694_r_double)/
!=============================================================================
dmat=0
do i=1,n
   im=i-1
   dmatt=sig(im)*sig(i)
   dmat(im,1)=-dmatt
   dmat(i,-1)=-dmatt
   dmat(im,0)= dmat(im,0)+dmatt
   dmat(i,0 )= dmat(i,0 )+dmatt
enddo
do i=1,2
   call quadfil(n,dmat,rts(i),fmat(:,:,i))
enddo
end subroutine stringop

!=============================================================================
subroutine quadfil(n,dmat,rts,fmat)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    quadfil
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    n
!    dmat
!    rts
!
!   output argument list:
!    fmat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use module_pmat2,only: mulbb,l1lb
implicit none

integer(i_long)                   ,intent(IN   ) :: n
real(r_double),dimension(0:n,-1:1),intent(IN   ) :: dmat
complex(r_double)                 ,intent(IN   ) :: rts
real(r_double),dimension(0:n,-2:0),intent(  OUT) :: fmat
!-----------------------------------------------------------------------------
real(r_double),dimension(0:n,-1:1)            :: tmat
real(r_double),dimension(0:n,-2:2)            :: umat
real(r_double)                                :: a1,a2,rrtsi,qrtsi
complex(r_double)                             :: rtsi
integer(i_long)                               :: np
!=============================================================================
np=n+1
rtsi=one/rts
rrtsi=real(rtsi)
qrtsi=imag(rtsi)
a1=-2*rrtsi
a2=rrtsi**2+qrtsi**2
tmat(0:n,-1:1)=a2*dmat
tmat(0:n,0)=tmat(0:n,0)+a1
call mulbb(dmat,tmat(0:n,-1:1),umat(0:n,-2:2), np,np, 1,1, 1,1, 2,2)
umat(0:n,0)=umat(0:n,0)+one
if(n==zero) then
  fmat=zero
  fmat(0,0)=umat(0,0)
else
  call l1lb(umat,fmat,np,2)
end if

end subroutine quadfil
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end of added code for new factorization

end module raflib


SUBROUTINE EIGEN(A,R,N,MV)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    EIGEN
!
!   prgrmmr:     R. J. Purser, NCEP 2005
!
! abstract:  COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
!            MATRIX
!
!        REMARKS
!           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
!           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
!           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
!           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
!           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!         RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!         MATRIX A IN DESCENDING ORDER.
!     R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!         IN SAME SEQUENCE AS EIGENVALUES)
!     N - ORDER OF MATRICES A AND R
!     MV- INPUT CODE
!         0   COMPUTE EIGENVALUES AND EIGENVECTORS
!         1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
!             DIMENSIONED BUT MUST STILL APPEAR IN CALLING SEQUENCE)
!
!   output argument list:
!     A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!         RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!         MATRIX A IN DESCENDING ORDER.
!     R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!         IN SAME SEQUENCE AS EIGENVALUES)
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
      use kinds, only: r_kind,i_kind
      use constants, only: zero,half,one,two
      implicit none

!     REAL(4) A(1),R(1)
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
      real(r_kind)   ,intent(inout) :: A(*), R(*)
      integer(i_kind),intent(in   ) :: N, MV

      REAL(r_kind) ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX, &
                       COSX2,SINCS,RANGE,ONEMYY
      integer(i_kind) I,J,K,IA,IQ,IJ,IL,IM,ILR,IMR,IND,L,M,MQ,LQ,LM, &
                       JQ,MM,LL,ILQ,IMQ

!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
!        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
!        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
!        BE CHANGED TO 1.0D-12.
!
!        ...............................................................
!
!        GENERATE IDENTITY MATRIX
!
      RANGE=1.0E-12_r_kind
      if(mv/=1) then
         IQ=-N
         DO J=1,N
            IQ=IQ+N
            DO I=1,N
               IJ=IQ+I
               if(i==j) then
                  R(IJ)=one
               else
                  R(IJ)=zero
               end if
          20   CONTINUE
            end do
         end do
      end if
!
!        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
!
      ANORM=zero
      DO I=1,N
         DO J=I,N
            if(i/=j) then
               IA=I+(J*J-J)/2
               ANORM=ANORM+A(IA)*A(IA)
             end if
         end do
      end do
      if(anorm>zero) then
         ANORM=1.414_r_kind*SQRT(ANORM)
         ANRMX=ANORM*RANGE/real(N,r_kind)
!
!        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
!
         IND=0
         THR=ANORM
         loop1: do
            THR=THR/real(N,r_kind)
            loop2: do
               L=1
               loop3: do
                  M=L+1
!
!              COMPUTE SIN AND COS
!
                  loop4: do
                     MQ=(M*M-M)/2
                     LQ=(L*L-L)/2
                     LM=L+MQ
!     62             continue
                     if(abs(a(lm))-thr>=zero) then
                        IND=1
                        LL=L+LQ
                        MM=M+MQ
                        X=half*(A(LL)-A(MM))
                        Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
                        if(x<zero) then
                           Y=-Y
                        end if
!DP75 SINX=Y/ SQRT(two*(one+( SQRT(one-Y*Y))))
                        SINX=Y/ SQRT(two*(one+( SQRT(MAX(zero,one-Y*Y)))))
                        ONEMYY=one-Y*Y
                        IF(one-Y*Y<zero) write(6,*)' IN EIGEN, 1-Y*Y=',ONEMYY
                        SINX2=SINX*SINX
!DP78 COSX= SQRT(one-SINX2)
                        COSX= SQRT(MAX(zero,one-SINX2))
                        COSX2=COSX*COSX
                        SINCS =SINX*COSX
!
!                    ROTATE L AND M COLUMNS
!
                        ILQ=N*(L-1)
                        IMQ=N*(M-1)
                        DO 125 I=1,N
                           IQ=(I*I-I)/2
                           if(i /= l .and. i /= m) then
                              if(i>m) then
                                 IM=M+IQ
                              else
                                 IM=I+MQ
                              end if
                              if(i>=l) then
                                 IL=L+IQ
                              else
                                 IL=I+LQ
                              end if
                              X=A(IL)*COSX-A(IM)*SINX
                              A(IM)=A(IL)*SINX+A(IM)*COSX
                              A(IL)=X
                           end if
                           if(mv/=1) then
                              ILR=ILQ+I
                              IMR=IMQ+I
                              X=R(ILR)*COSX-R(IMR)*SINX
                              R(IMR)=R(ILR)*SINX+R(IMR)*COSX
                              R(ILR)=X
                           end if
      125               continue
                        X=two*A(LM)*SINCS
                        Y=A(LL)*COSX2+A(MM)*SINX2-X
                        X=A(LL)*SINX2+A(MM)*COSX2+X
                        A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
                        A(LL)=Y
                        A(MM)=X
                     end if
!
!                    TESTS FOR COMPLETION
!
!                    TEST FOR M = LAST COLUMN
!
                     if(m==n) exit loop4
                     M=M+1
                  end do loop4
 
!
!              TEST FOR L = SECOND FROM LAST COLUMN
!
                  if(l==n-1) exit loop3
                  L=L+1
               end do loop3
               if(ind/=1) exit loop2
               IND=0
            end do loop2
!
!        COMPARE THRESHOLD WITH FINAL NORM
!
            if(thr<=anrmx) exit loop1
         end do loop1
!
!        SORT EIGENVALUES AND EIGENVECTORS
!
         end if

         IQ=-N
         DO I=1,N
            IQ=IQ+N
            LL=I+(I*I-I)/2
            JQ=N*(I-2)
            DO J=I,N
               JQ=JQ+N
               MM=J+(J*J-J)/2
               if(a(ll)>=a(mm)) cycle
               X=A(LL)
               A(LL)=A(MM)
               A(MM)=X
               if(mv==1) cycle
               DO K=1,N
                  ILR=IQ+K
                  IMR=JQ+K
                  X=R(ILR)
                  R(ILR)=R(IMR)
                  R(IMR)=X
               end do
            end do
         end do
         RETURN
END SUBROUTINE EIGEN


SUBROUTINE gettri4(us,lguess,lv,lui,w4)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gettri4
!
!   prgrmmr:     Purser 2003
!
! abstract:  Blended 4-color triads with bridging function of 2nd degree
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     US:      3-vector comprising components of the target aspect tensor
!     LGUESS:  Code to tell whether input LTRIAD are feasible (LGUESS.NE.0)
!     LV:      4 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!                where LU are the aspect vectors of the first three of LV.
!
!   output argument list:
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!                where LU are the aspect vectors of the first three of LV.
!     W4:      4 real "spread" weights, in generalized grid-step units, that
!                correspond to the 4 grid-step generators, LV.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use constants, only: quarter,half,one,two,three
IMPLICIT NONE

REAL(r_kind),DIMENSION(3)     ,INTENT(IN   ) :: us
INTEGER(i_kind)               ,INTENT(IN   ) :: lguess
INTEGER(i_kind),DIMENSION(2,4),INTENT(INOUT) :: lv
INTEGER(i_kind),DIMENSION(3,3),INTENT(INOUT) :: lui
REAL(r_kind),DIMENSION(4)     ,INTENT(  OUT) :: w4
!-----------------------------------------------------------------------------
REAL(r_kind)                                :: c,aoc,boc,d,dlim
REAL(r_kind),DIMENSION(3)                   :: v
REAL(r_kind),DIMENSION(4)                   :: w4c
REAL(r_kind),DIMENSION(3,3)                 :: b123
REAL(r_kind),DIMENSION(4,3)                 :: w4l
INTEGER(i_kind)                             :: kt
DATA b123/.5_r_kind,0._r_kind,.5_r_kind,  -.5_r_kind,0._r_kind,.5_r_kind,  0._r_kind,1._r_kind,1._r_kind/
DATA w4c/ 1._r_kind,1._r_kind,-.5_r_kind,-.5_r_kind/
DATA w4l/ 1._r_kind,-1._r_kind,0._r_kind,0._r_kind, 0._r_kind,0._r_kind,.5_r_kind,-.5_r_kind, &
          -1._r_kind,-1._r_kind,1._r_kind,1._r_kind/
!=============================================================================
CALL gettri3(us,lguess,lv(:,1:3),lui,v,kt)
lv(:,4)=lv(:,1)-lv(:,2)
v=MATMUL(b123,v)
c=v(3)
aoc=v(1)/c
boc=v(2)/c
d=boc/(two-boc)
dlim=(one-abs(aoc))/(three+abs(aoc))
IF(d<dlim)THEN
   v(3)=(two+dlim+d*d/dlim)*quarter
ELSE
   v(3)=(one+d)*half
ENDIF
v(1)=aoc*v(3)
v(2)=boc*v(3)
c=c/v(3)
w4=(w4c+MATMUL(w4l,v))*c
END SUBROUTINE gettri4


SUBROUTINE gettri3(utarget,lguess,ltriad,lui,wtriad,kt)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    gettri3
!
!   prgrmmr:     Purser 1997
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     UTARGET: 3-vector comprising components of the target aspect tensor
!     LGUESS:  Code to tell whether input LTRIAD are feasible (LGUESS.NE.0)
!     LTRIAD:  3 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!
!   output argument list:
!     LTRIAD:  3 integer basis 2-vectors giving the canonical grid steps
!     LUI:     3 3-vectors dual to those of LU: [LUI]^t*[LU]=[I]
!     WTRIAD:  3 real "spread" components in generalized grid-step units
!     KT:      The number of iterations it required to find the valid triad
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use constants, only: zero
IMPLICIT NONE

REAL(r_kind),DIMENSION(3)     ,INTENT(IN   ) :: utarget
INTEGER(i_kind)               ,INTENT(IN   ) :: lguess
INTEGER(i_kind),DIMENSION(2,3),INTENT(INOUT) :: ltriad
INTEGER(i_kind),DIMENSION(3,3),INTENT(INOUT) :: lui
REAL(r_kind),DIMENSION(3)     ,INTENT(OUT  ) :: wtriad
INTEGER(i_kind)               ,INTENT(OUT  ) :: kt
!-----------------------------------------------------------------------------
INTEGER(i_kind),DIMENSION(2,3):: itriad
INTEGER(i_kind),DIMENSION(3,3):: ilui,nj
INTEGER(i_kind),DIMENSION(3)  :: luil,kl,ml
REAL(r_kind),PARAMETER        :: bcmins=-1.e-14_r_kind
REAL(r_kind)                  :: u,wl
INTEGER(i_kind)               :: i,j,k,l,m,n,it
DATA itriad/ 1, 0,  0,1,-1,-1/
DATA kl/3,1,2/,ml/2,3,1/,nj/-2,2,2,2,-2,2,2,2,-2/
DATA ilui/1, 0,-1,  0, 1,-1,  0, 0, 1/
!=============================================================================
IF(lguess==0)THEN
   DO j=1,3
      DO i=1,2
         ltriad(i,j)=itriad(i,j)
      ENDDO
      DO i=1,3
         lui(i,j)=ilui(i,j)
      ENDDO
   ENDDO
ENDIF

! Use initial estimate of triad to compute implied weights directly.
! (Subsequent updates of these weights are done perturbatively to save time).
DO i=1,3
   wtriad(i)=zero
ENDDO
DO i=1,3
   u=utarget(i)
   DO j=1,3
      wtriad(j)=wtriad(j)+lui(i,j)*u
   ENDDO
ENDDO

ITER_LOOP:DO it=1,100 ! 4000       !  this should be ample
   DO l=1,3
      IF(wtriad(l)<bcmins)THEN
	 k=kl(l)
	 m=ml(l)
	 DO i=1,2
            ltriad(i,l)=ltriad(i,m)-ltriad(i,k)
            ltriad(i,m)=-ltriad(i,m)
	 ENDDO
	 DO i=1,3
            luil(i)=lui(i,l)
	 ENDDO
	 wl=wtriad(l)
	 DO j=1,3
            n=nj(j,l)
            DO i=1,3
               lui(i,j)=lui(i,j)+luil(i)*n
            ENDDO
            wtriad(j)=wtriad(j)+wl*n
	 ENDDO
	 CYCLE ITER_LOOP
      ENDIF
   ENDDO
   kt=it		   !  report back how many iterations were needed
! Rotate triad so that smallest wtriad is associated with third member:
   i=1
   IF(wtriad(2)<wtriad(1))i=2
   IF(wtriad(3)>wtriad(i))THEN
      DO j=1,2
         l=ltriad(j,i)
         ltriad(j,i)=ltriad(j,3)
         ltriad(j,3)=l
      ENDDO
      DO j=1,3
         l=lui(j,i)
         lui(j,i)=lui(j,3)
         lui(j,3)=l
      ENDDO
      wl=wtriad(i)
      wtriad(i)=wtriad(3)
      wtriad(3)=wl
   ENDIF
   RETURN
ENDDO ITER_LOOP
write(6,*)'GETTRI3:  ALL 40 ITERATIONS USED UP.  This should never happen'
call stop2(68)
END SUBROUTINE gettri3


SUBROUTINE what_color_is_triad(i1,i2,color,p)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    what_color_is_triad
!
!   prgrmmr:     R. J. Purser, NCEP, August 2001
!
! abstract:  Find the color index associated with the given grid displacement vector
! for the chromatic triad of type "p", where p is prime.
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     i1,i2   - displacement indices in x, y.
!     p       - is the "chromatic prime index", presently one of {2, 3, 5}.
!
!   output argument list:
!     color   - color index in the range [1, (p +1)], where,
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
IMPLICIT NONE

INTEGER(i_kind),INTENT(IN   ) :: i1,i2,p
INTEGER(i_kind),INTENT(  OUT) :: color
!----------------------------------------------------------------
INTEGER(i_kind),DIMENSION(2)  :: v,vf,vfp,bxy2,bxy3,bxy5
INTEGER(i_kind),DIMENSION(8)  :: color3
INTEGER(i_kind),DIMENSION(24) :: color5
LOGICAL same4
INTEGER(i_kind)               :: itest
DATA bxy2/1,2/,bxy3/1,3/,bxy5/1,5/
DATA color3 &
/ 1,1,  2, 3, 4,  2, 4, 3/
DATA color5                                                                   &
/  1, 1, 1, 1,  2, 3, 4, 5, 6,  2, 5, 3, 6, 4,  2, 4, 6, 3, 5,  2, 6, 5, 4, 3/
!=============================================================================
vf(1)=i1; vf(2)=i2
DO itest=1,20
   v=vf; vf=v/p; vfp=vf*p; IF(.NOT.same4(vfp,v,2))EXIT
ENDDO
v=MODULO(v,p)
SELECT CASE(p)
   CASE (2)
      color=dot_PRODUCT(v,bxy2)
   CASE (3)
      color=color3(dot_PRODUCT(v,bxy3))
   CASE (5)
      color=color5(dot_PRODUCT(v,bxy5))
   CASE default
      write(6,*)'WHAT_COLOR_IS_TRIAD:  ***PROBLEM***  no tabulation available for p-value supplied'
      call stop2(68)
END SELECT
END SUBROUTINE what_color_is_triad


FUNCTION same4(v1,v2,n)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    same4
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-22  safford -- add subprogram doc block
!
!   input argument list:
!     v1, v2   -
!     n        -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
implicit none

INTEGER(i_kind),             INTENT(IN   ) :: n
INTEGER(i_kind),DIMENSION(n),INTENT(IN   ) :: v1,v2

LOGICAL                                :: same4
INTEGER(i_kind)                        :: i
!=============================================================================
same4=.TRUE.
DO i=1,n; IF(v1(i) /= v2(i))same4=.FALSE.; ENDDO
END FUNCTION same4

subroutine smther(filter,g,nrows,ngauss,nsmooth,nsmooth_shapiro, &
               ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes,gnormx,gnormy,adjoint)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smther
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    filter
!    nrows,ngauss,nsmooth,nsmooth_shapiro
!    adjoint
!    ids,ide,jds,jde                      - domain indices
!    ips,ipe,jps,jpe,kps,kpe              - patch indices
!    npes
!    gnormx,gnormy
!    g
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  use raflib,only: add_halo_x,add_halo_y,filter_cons
  implicit none

  TYPE(filter_cons),intent(in   ) :: filter                               ! structure defining recursive filter
  integer(i_long)  ,intent(in   ) :: nrows,ngauss,nsmooth,nsmooth_shapiro
  logical          ,intent(in   ) :: adjoint
  integer(i_long)  ,intent(in   ) :: ids,ide,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes
  real(r_single)   ,intent(inout) :: g(ngauss,ips:ipe,jps:jpe,kps:kpe)
  real(r_single)   ,intent(in   ) :: gnormx(ips:ipe),gnormy(jps:jpe)

  integer(i_long) i,j,k,n
  real(r_single),allocatable:: gt(:,:,:,:)

!   smoothing in x direction
  allocate(gt(ngauss,max(ids,ips-nrows):min(ide,ipe+nrows),jps:jpe,kps:kpe))
  if(nsmooth_shapiro>0.and..not.adjoint) then
     do k=kps,kpe
        do j=jps,jpe
           do i=ips,ipe
              do n=1,ngauss
                 g(n,i,j,k)=gnormx(i)*g(n,i,j,k)
              end do
           end do
        end do
     end do
  end if
  call add_halo_x(g,gt,filter,ngauss,nrows,ids,ide,ips,ipe,jps,jpe,kps,kpe,npes)

  if(nsmooth>0) &
      call smther_one_x(gt,g,ngauss,ids,ide,ips,ipe,jps,jpe,kps,kpe)
  if(nsmooth_shapiro>0) &
      call smther_two_x(gt,g,ngauss,ids,ide,ips,ipe,jps,jpe,kps,kpe)
  deallocate(gt)
  if(nsmooth_shapiro>0.and.adjoint) then
     do k=kps,kpe
        do j=jps,jpe
           do i=ips,ipe
              do n=1,ngauss
                 g(n,i,j,k)=gnormx(i)*g(n,i,j,k)
              end do
           end do
        end do
     end do
  end if

!   smoothing in y direction
  allocate(gt(ngauss,ips:ipe,max(jds,jps-nrows):min(jde,jpe+nrows),kps:kpe))
  if(nsmooth_shapiro>0.and..not.adjoint) then
     do k=kps,kpe
        do j=jps,jpe
           do i=ips,ipe
              do n=1,ngauss
                 g(n,i,j,k)=gnormy(j)*g(n,i,j,k)
              end do
           end do
        end do
     end do
  end if
  call add_halo_y(g,gt,filter,ngauss,nrows,jds,jde,ips,ipe,jps,jpe,kps,kpe,npes)
  if(nsmooth>0) &
      call smther_one_y(gt,g,ngauss,jds,jde,ips,ipe,jps,jpe,kps,kpe)
  if(nsmooth_shapiro>0) &
      call smther_two_y(gt,g,ngauss,jds,jde,ips,ipe,jps,jpe,kps,kpe)
  deallocate(gt)
  if(nsmooth_shapiro>0.and.adjoint) then
     do k=kps,kpe
        do j=jps,jpe
           do i=ips,ipe
              do n=1,ngauss
                 g(n,i,j,k)=gnormy(j)*g(n,i,j,k)
              end do
           end do
        end do
     end do
  end if

end subroutine smther

subroutine smther_one_x(gx,g,ngauss,ids,ide,ips,ipe,jps,jpe,kps,kpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smther_one_x
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    ngauss
!    ids,ide                 - domain indices
!    ips,ipe,jps,jpe,kps,kpe - patch indices
!    gx
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  implicit none

  integer(i_long),intent(in   ) :: ngauss
  integer(i_long),intent(in   ) :: ids,ide,ips,ipe,jps,jpe,kps,kpe
  real(r_single) ,intent(in   ) :: gx(ngauss,max(ids,ips-1):min(ide,ipe+1),jps:jpe,kps:kpe)
  real(r_single) ,intent(  out) :: g(ngauss,ips:ipe,jps:jpe,kps:kpe)

  integer(i_long) i,im,ip,j,k,n

  do k=kps,kpe
     do j=jps,jpe
        do i=ips,ipe
           ip=min(i+1,ide) ; im=max(ids,i-1)
           do n=1,ngauss
              g(n,i,j,k)=.25_r_single*(gx(n,ip,j,k)+gx(n,im,j,k)) +.5_r_single*gx(n,i,j,k)
           end do
        end do
     end do
  end do

end subroutine smther_one_x

subroutine smther_one_y(gy,g,ngauss,jds,jde,ips,ipe,jps,jpe,kps,kpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smther_one_y
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    ngauss
!    jds,jde                 - domain indices
!    ips,ipe,jps,jpe,kps,kpe - patch indices
!    gy
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  implicit none

  integer(i_long),intent(in   ) :: ngauss
  integer(i_long),intent(in   ) :: jds,jde,ips,ipe,jps,jpe,kps,kpe
  real(r_single) ,intent(in   ) :: gy(ngauss,ips:ipe,max(jds,jps-1):min(jde,jpe+1),kps:kpe)
  real(r_single) ,intent(  out) :: g(ngauss,ips:ipe,jps:jpe,kps:kpe)

  integer(i_long) i,j,jm,jp,k,n

  do k=kps,kpe
     do j=jps,jpe
        jp=min(j+1,jde) ; jm=max(jds,j-1)
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.25_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k)) +.5_r_single*gy(n,i,j,k)
           end do
        end do
     end do
  end do

end subroutine smther_one_y

subroutine smther_two_x(gx,g,ngauss,ids,ide,ips,ipe,jps,jpe,kps,kpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   smther_two
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: apply second moment preserving "shapiro smoother"
! in each direction of data slab
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!    ngauss
!    ids,ide                 - domain indices
!    ips,ipe,jps,jpe,kps,kpe - patch indices
!    gx
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  implicit none

  integer(i_long),intent(in   ) :: ngauss
  integer(i_long),intent(in   ) :: ids,ide,ips,ipe,jps,jpe,kps,kpe

  real(r_single) ,intent(in   ) :: gx(ngauss,max(ids,ips-3):min(ide,ipe+3),jps:jpe,kps:kpe)
  real(r_single) ,intent(  out) :: g(ngauss,ips:ipe,jps:jpe,kps:kpe)
                                   !  on input: original data slab
                                   !  on ouput: filtered data slab

  integer(i_long) i,im,im3,ip,ip3,istart,iend,j,k,n

  istart=ips ; iend=ipe
  if(ips==ids) then
     i=ids ; ip=i+1 ; ip3=i+3_i_long
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*gx(n,ip,j,k)-.03125_r_single*gx(n,ip3,j,k)
           end do
        end do
     end do
     i=ids+1 ; ip=i+1 ; ip3=i+3_i_long ; im=i-1
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*(gx(n,ip,j,k)+gx(n,im,j,k))-.03125_r_single*gx(n,ip3,j,k)
           end do
        end do
     end do
     i=ids+2_i_long ; ip=i+1 ; ip3=i+3_i_long ; im=i-1
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*(gx(n,ip,j,k)+gx(n,im,j,k))-.03125_r_single*gx(n,ip3,j,k)
           end do
        end do
     end do
     istart=ids+3_i_long
  end if
  if(ipe==ide) then
     i=ide-2_i_long ; ip=i+1 ; im3=i-3_i_long ; im=i-1
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*(gx(n,ip,j,k)+gx(n,im,j,k))-.03125_r_single*gx(n,im3,j,k)
           end do
        end do
     end do
     i=ide-1 ; ip=i+1 ; im3=i-3_i_long ; im=i-1
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*(gx(n,ip,j,k)+gx(n,im,j,k))-.03125_r_single*gx(n,im3,j,k)
           end do
        end do
     end do
     i=ide ; im3=i-3_i_long ; im=i-1
     do k=kps,kpe
        do j=jps,jpe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gx(n,i,j,k)+.28125_r_single*gx(n,im,j,k)-.03125_r_single*gx(n,im3,j,k)
           end do
        end do
     end do
     iend=ide-3_i_long
  end if
  do k=kps,kpe
     do j=jps,jpe
        do i=istart,iend
           ip=i+1 ; im=i-1 ; ip3=i+3_i_long ; im3=i-3_i_long
           do n=1,ngauss
              g(n,i,j,k)=.28125_r_single*(gx(n,ip,j,k)+gx(n,im,j,k))+.5_r_single*gx(n,i,j,k) &
                         -.03125_r_single*(gx(n,ip3,j,k)+gx(n,im3,j,k))
           end do
        end do
     end do
  end do

end subroutine smther_two_x

subroutine smther_two_y(gy,g,ngauss,jds,jde,ips,ipe,jps,jpe,kps,kpe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   smther_two
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: apply second moment preserving "shapiro smoother"
! in each direction of data slab
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!    ngauss
!    jds,jde                 - domain indices
!    ips,ipe,jps,jpe,kps,kpe - patch indices
!    gy
!
!   output argument list:
!    g
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  implicit none

  integer(i_long),intent(in   ) :: ngauss
  integer(i_long),intent(in   ) :: jds,jde,ips,ipe,jps,jpe,kps,kpe

  real(r_single) ,intent(in   ) :: gy(ngauss,ips:ipe,max(jds,jps-3):min(jde,jpe+3),kps:kpe)
  real(r_single) ,intent(  out) :: g(ngauss,ips:ipe,jps:jpe,kps:kpe)
                                   !  on input: original data slab
                                   !  on ouput: filtered data slab

  integer(i_long) i,jm,jm3,jp,jp3,jstart,jend,j,k,n

  jstart=jps ; jend=jpe
  if(jps==jds) then
     j=jds ; jp=j+1 ; jp3=j+3_i_long
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*gy(n,i,jp,k)-.03125_r_single*gy(n,i,jp3,k)
           end do
        end do
     end do
     j=jds+1 ; jp=j+1 ; jp3=j+3_i_long ; jm=j-1
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k))-.03125_r_single*gy(n,i,jp3,k)
           end do
        end do
     end do
     j=jds+2_i_long ; jp=j+1 ; jp3=j+3_i_long ; jm=j-1
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k))-.03125_r_single*gy(n,i,jp3,k)
           end do
        end do
     end do
     jstart=jds+3_i_long
  end if
  if(jpe==jde) then
     j=jde-2_i_long ; jp=j+1 ; jm3=j-3_i_long ; jm=j-1
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k))-.03125_r_single*gy(n,i,jm3,k)
           end do
        end do
     end do
     j=jde-1 ; jp=j+1 ; jm3=j-3_i_long ; jm=j-1
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k))-.03125_r_single*gy(n,i,jm3,k)
           end do
        end do
     end do
     j=jde ; jm3=j-3_i_long ; jm=j-1
     do k=kps,kpe
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.5_r_single*gy(n,i,j,k)+.28125_r_single*gy(n,i,jm,k)-.03125_r_single*gy(n,i,jm3,k)
           end do
        end do
     end do
     jend=jde-3_i_long
  end if
  do k=kps,kpe
     do j=jstart,jend
        jp=j+1 ; jm=j-1 ; jp3=j+3_i_long ; jm3=j-3_i_long
        do i=ips,ipe
           do n=1,ngauss
              g(n,i,j,k)=.28125_r_single*(gy(n,i,jp,k)+gy(n,i,jm,k))+.5_r_single*gy(n,i,j,k) &
                         -.03125_r_single*(gy(n,i,jp3,k)+gy(n,i,jm3,k))
           end do
        end do
     end do
  end do

end subroutine smther_two_y

subroutine smther_two_gnorm(gnorm,ids,ide,ips,ipe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smther_two_gnorm
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-30  lueken - added subprogram doc block
!
!   input argument list:
!    ids,ide           - domain indices
!    ips,ipe           - patch indices
!
!   output argument list:
!    gnorm
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_single,r_double,i_long
  use constants,only: three,four
  implicit none

  integer(i_long),intent(in   ) :: ids,ide,ips,ipe

  real(r_single) ,intent(  out) :: gnorm(ips:ipe)

  integer(i_long) i,istart,iend
  real(r_double) const81,const82

  const81=four/three
  const82=32._r_double/33._r_double
  istart=ips ; iend=ipe
  if(ips==ids) then
     i=ids
     gnorm(i)=const81     !  1./(.5+.28125-.03125)
     i=ids+1
     gnorm(i)=const82     !  1./(.5+.28125*2.-.03125)
     i=ids+2_i_long
     gnorm(i)=const82     !  1./(.5+.28125*2.-.03125)
     istart=ids+3_i_long
  end if
  if(ipe==ide) then
     i=ide-2_i_long
     gnorm(i)=const82     !  1./(.5+.28125*2.-.03125)
     i=ide-1
     gnorm(i)=const82     !  1./(.5+.28125*2.-.03125)
     i=ide
     gnorm(i)=const81     !  1./(.5+.28125-.03125)
     iend=ide-3_i_long
  end if
  do i=istart,iend
     gnorm(i)=1._r_single !  1./(.28125*2.+.5-.03125*2.)
  end do

end subroutine smther_two_gnorm
