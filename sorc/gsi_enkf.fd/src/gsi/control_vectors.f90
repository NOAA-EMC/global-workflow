module control_vectors
!$$$ module documentation block
!           .      .    .                                       .
! module:   control_vectors
!   prgmmr: tremolet
!
! abstract: define control vectors and basic operators
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-08-03  todling  - using get_lun for file unit definition
!   2008-01-04  tremolet - improve allocate/deallocate
!   2008-12-29  todling  - add omp to various loops
!   2009-01-27  todling  - rename prt_norms to prevent IBM compiler confusion
!   2009-08-12  lueken   - updated documentation
!   2009-09-20  parrish  - add pointer variable a_en to definition of type control_state
!                           also, add module variables n_ens
!   2009-11-10  todling  - remove redundant dot products
!   2010-02-17  treadon  - fix in predictor part of random vector
!   2010-02-20  parrish  - add functions dplevs_ens for use with dual-resolution hybrid ensemble option.
!   2010-03-11  zhu      - add changes for generalizing control variables,i.e.,
!                          init_anacv,nvars,nrf*,allocate_cs,deallocate_cs,
!                          assign_cs2array,assign_array2cs
!   2010-05-01  todling  - introduce gsi_bundle
!   2010-05-05  derber   - omp commands removed
!   2010-05-22  todling  - add a wired-in set of variables composing a motley (not fully part of CVector)
!   2010-05-28  todling  - remove all nrf2/3_VAR-specific "pointers"
!   2011-07-04  todling  - fixes to run either single or double precision
!   2013-05-20  zhu      - add aircraft temperature bias correction coefficients as control variables
!   2016-02-15  Johnson, Y. Wang, X. Wang - add variables to control reading
!                                           state variables for radar DA. POC: xuguang.wang@ou.edu
!   2019-03-14  eliu     - add logic to turn on using full set of hydrometeors
!                          in obs operator and analysis 
!   2019-07-11  Todling  - move WRF specific variables w_exist and dbz_exit to a new wrf_vars_mod.f90.
!                        . move imp_physics and lupp to ncepnems_io.f90.
!   2019-09-13  martin   - added incvars_to_zero variable for writing out fv3 netCDF increments
!   2019-10-28  martin   - added incvars_zero_strat variable for zeroing out increments above tropopause
!
! subroutines included:
!   sub init_anacv   
!   sub final_anacv   
!   sub setup_control_vectors
!   sub allocate_cv
!   sub deallocate_cv
!   sub assign_scalar2cv
!   sub assign_cv2cv
!   sub assign_array2cv
!   sub assign_cv2array
!   sub qdot_prod_vars
!   sub prt_norms
!   sub prt_norms_vars
!   sub axpy
!   sub random_cv
!   sub write_cv
!   sub read_cv
!   sub inquire_cv
!
! functions included:
!   dplevs_ens
!   qdot_prod_sub
!   dot_prod_cv
!   qdot_prod_cv
!   qdot_prod_cv_eb
!   maxval_cv
!   qdot_product
!
! variable definitions:
!   def n_ens     - number of ensemble perturbations (=0 except when hybrid ensemble option turned on)
!   def lcalc_gfdl_cfrac - if T, calculate and use GFDL cloud fraction in obs operator 
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,r_double,r_single,i_kind,r_quad
use mpimod, only: mpi_comm_world,mpi_max,mpi_rtype,mype,npe,ierror
use constants, only: zero, one, two, three, zero_quad, tiny_r_kind
use gsi_4dvar, only: iadatebgn
use file_utility, only : get_lun
use mpl_allreducemod, only: mpl_allreduce
use hybrid_ensemble_parameters, only: beta_s0,l_hyb_ens
use hybrid_ensemble_parameters, only: grd_ens
use constants, only : max_varname_length
use gridmod, only : minmype

use m_rerank, only : rerank
use GSI_BundleMod, only : GSI_BundleCreate
use GSI_BundleMod, only : GSI_BundleSet
use GSI_BundleMod, only : GSI_Bundle
use GSI_BundleMod, only : GSI_BundleGetPointer
use GSI_BundleMod, only : dplevs => GSI_BundleDpLevs
use GSI_BundleMod, only : GSI_BundleUnSet
use GSI_BundleMod, only : GSI_BundleDestroy

use GSI_BundleMod, only : GSI_Grid
use GSI_BundleMod, only : GSI_GridCreate

use mpeu_util, only: gettablesize
use mpeu_util, only: gettable

implicit none
save
private
! 
! Public functions
!
public control_vector
public allocate_cv
public deallocate_cv
public assignment(=)
public dot_product  
public prt_control_norms, axpy, random_cv, setup_control_vectors, &
     write_cv, read_cv, inquire_cv, maxval, qdot_prod_sub, init_anacv, &
     final_anacv,c2sset_flg,e2sset_flg

! 
! Public variables
!
public cvars2d, cvars3d, cvarsmd, evars2d, evars3d, nrf_var
public nc2d        ! number of 2d static control fields
public nc3d        ! number of 3d static control fields
public mvars       ! number of motley fields
public nrf         ! total number of static control fields
public nvars       ! total number of static plus motley fields
public nrf_3d      ! when .t., indicates 3d-fields
public as3d        ! normalized scale factor for background error 3d-variables
public as2d        ! normalized scale factor for background error 2d-variables
public atsfc_sdv   ! standard deviation of surface temperature error over (1) land (and (2) ice
public an_amp0     ! multiplying factors on reference background error variances
public lcalc_gfdl_cfrac ! when .t., calculate and use GFDL cloud fraction in obs operator 

public nrf2_loc,nrf3_loc,nmotl_loc   ! what are these for??
public ntracer

public :: incvars_to_zero ! array of fieldnames to zero out increments for
public :: incvars_zero_strat ! array of fieldnames to zero out increments above tropopause
public :: incvars_efold ! scale factor x in which e^(-(k-ktrop)/x) for above fields

type control_vector
   integer(i_kind) :: lencv
   real(r_kind), pointer :: values(:) => NULL()
   type(GSI_Grid)  :: grid_step
   type(GSI_Bundle), pointer :: step(:)
   type(GSI_Bundle), pointer :: motley(:)
   type(GSI_Grid)  :: grid_aens
   type(GSI_Bundle), pointer :: aens(:,:,:)
   real(r_kind), pointer :: predr(:) => NULL()
   real(r_kind), pointer :: predp(:) => NULL()
   real(r_kind), pointer :: predt(:) => NULL()
   logical :: lallocated = .false.
end type control_vector

character(len=*),parameter:: myname='control_vectors'

integer(i_kind) :: nclen,nclen1,nsclen,npclen,ntclen,nrclen,nsubwin,nval_len
integer(i_kind) :: latlon11,latlon1n,lat2,lon2,nsig,n_ens
integer(i_kind) :: nval_lenz_en
logical,save :: lsqrtb,lcalc_gfdl_cfrac  
logical :: c2sset_flg,e2sset_flg  

integer(i_kind) :: m_vec_alloc, max_vec_alloc, m_allocs, m_deallocs

logical,allocatable,dimension(:):: nrf_3d
integer(i_kind),allocatable,dimension(:):: nrf2_loc,nrf3_loc,nmotl_loc
integer(i_kind) nrf,nvars
integer(i_kind) ntracer

integer(i_kind) :: nc2d,nc3d,mvars
character(len=max_varname_length),allocatable,dimension(:) :: nrf_var  ! names of all variables
character(len=max_varname_length),allocatable,dimension(:) :: cvars2d  ! 2-d fields for static   CV
character(len=max_varname_length),allocatable,dimension(:) :: cvars3d  ! 3-d fields for static   CV
character(len=max_varname_length),allocatable,dimension(:) :: evars2d  ! 2-d fields for ensemble CV
character(len=max_varname_length),allocatable,dimension(:) :: evars3d  ! 3-d fields for ensemble CV
character(len=max_varname_length),allocatable,dimension(:) :: cvarsmd  ! motley variable names
real(r_kind)    ,allocatable,dimension(:) :: as3d
real(r_kind)    ,allocatable,dimension(:) :: as2d
real(r_kind)    ,allocatable,dimension(:) :: atsfc_sdv
real(r_kind)    ,allocatable,dimension(:) :: an_amp0

logical :: llinit = .false.
character(len=12),allocatable,dimension(:) :: incvars_to_zero 
character(len=12),allocatable,dimension(:) :: incvars_zero_strat 
real(r_kind) :: incvars_efold

! ----------------------------------------------------------------------
INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_scalar2cv, assign_array2cv, assign_cv2array, assign_cv2cv
END INTERFACE

INTERFACE DOT_PRODUCT
MODULE PROCEDURE dot_prod_cv,qdot_prod_cv,qdot_prod_cv_eb
END INTERFACE

INTERFACE MAXVAL
MODULE PROCEDURE maxval_cv
END INTERFACE

INTERFACE PRT_CONTROL_NORMS
MODULE PROCEDURE prt_norms
END INTERFACE

! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine setup_control_vectors(ksig,klat,klon,katlon11,katlon1n, &
                                 ksclen,kpclen,ktclen,kclen,ksubwin,kval_len,ldsqrtb,k_ens,&
                                 kval_lenz_en)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_control_vectors
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional input variable k_ens, which communicates size
!                         of ensemble used when hybrid ensemble option is turned on.
!   2010-05-19  todling - k_ens no longer optional
!   2010-05-23  todling - move lev pointer init from jfunc here
!
!   input argument list:
!    ksig
!    klat,klon
!    katlon11
!    katlon1n
!    ksclen
!    kpclen
!    ktclen
!    kclen
!    ksubwin
!    kval_len
!    ldsqrtb
!    k_ens     - if applicable, size of ensemble used in hybrid ensemble option
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)          , intent(in   ) :: ksig,klat,klon,katlon11,katlon1n, &
                                 ksclen,kpclen,ktclen,kclen,ksubwin,kval_len,k_ens,&
                                 kval_lenz_en
  logical                  , intent(in   ) :: ldsqrtb

  nsig=ksig
  lat2=klat
  lon2=klon
  latlon11=katlon11
  latlon1n=katlon1n
  nsclen=ksclen
  npclen=kpclen
  ntclen=ktclen
  nrclen=nsclen+npclen+ntclen
  nclen =kclen
  nclen1=nclen-nrclen
  nsubwin=ksubwin
  nval_len=kval_len
  lsqrtb=ldsqrtb
  n_ens=k_ens
  nval_lenz_en=kval_lenz_en

  llinit = .true.
  m_vec_alloc=0
  max_vec_alloc=0
  m_allocs=0
  m_deallocs=0

  call inquire_cv

  return
end subroutine setup_control_vectors
! ----------------------------------------------------------------------
subroutine init_anacv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_anacv
!   prgmmr: zhu          org: np23               date:  2008-03-29
!
! abstract: read in control variables information
!
! program history log:
!   2010-03-11  zhu     - initial code
!   2010-05-30  todling - revamp initial code
!   2014-02-11  todling - rank-2 must have lev=1, anything else is rank-3
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
use hybrid_ensemble_parameters,only:idaen3d,idaen2d
implicit none
!character(len=*),parameter:: rcname='anavinfo.txt'
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='control_vector::'
character(len=256),allocatable,dimension(:):: utable
character(len=20) var,source,funcof
character(len=*),parameter::myname_=myname//'*init_anacv'
integer(i_kind) luin,ii,ntot
integer(i_kind) ilev, itracer
real(r_kind) aas,amp

! load file
luin=get_lun()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nvars)

! Get contents of table
allocate(utable(nvars))
call gettable(tbname,luin,ntot,nvars,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
nc3d=0; nc2d=0;mvars=0
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, aas, amp, source, funcof
   if(trim(adjustl(source))=='motley') then
      mvars=mvars+1
   else
      if(ilev==1) then
          nc2d=nc2d+1
      else
          nc3d=nc3d+1
      endif
   endif
enddo

allocate(nrf_var(nvars),cvars3d(nc3d),cvars2d(nc2d))
allocate(as3d(nc3d),as2d(nc2d))
allocate(cvarsmd(mvars))
allocate(atsfc_sdv(mvars))
allocate(an_amp0(nvars))
allocate(idaen3d(nc3d),idaen2d(nc2d))
allocate(incvars_to_zero(nvars))
allocate(incvars_zero_strat(nvars))
incvars_to_zero(:) = 'NONE'
incvars_zero_strat(:) = 'NONE'
incvars_efold = 5.0_r_kind

! want to rid code from the following ...
nrf=nc2d+nc3d
allocate(nrf_3d(nrf),nrf2_loc(nc2d),nrf3_loc(nc3d),nmotl_loc(max(1,mvars)))

! Now load information from table
nc3d=0;nc2d=0;mvars=0
nrf_3d=.false.
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, aas, amp, source, funcof
   if(trim(adjustl(source))=='motley') then
       mvars=mvars+1
       cvarsmd(mvars)=trim(adjustl(var))
       nmotl_loc(mvars)=ii
       atsfc_sdv(mvars)=aas
   else
      if(ilev==1) then
         nc2d=nc2d+1
         cvars2d(nc2d)=trim(adjustl(var))
         nrf2_loc(nc2d)=ii  ! rid of soon
         as2d(nc2d)=aas
         if(itracer>10) then
            idaen2d(nc2d)=2
         else
            idaen2d(nc2d)=1
         endif
      else
         nc3d=nc3d+1
         cvars3d(nc3d)=trim(adjustl(var))
         nrf3_loc(nc3d)=ii  ! rid of soon
         nrf_3d(ii)=.true.
         as3d(nc3d)=aas
         if(itracer>10) then
            idaen3d(nc3d)=2
         else
            idaen3d(nc3d)=1
         endif
      endif
   endif
   nrf_var(ii)=trim(adjustl(var))
   if(amp>zero) then
      an_amp0(ii)=amp
   else
      an_amp0(ii)=one/three
   endif
enddo

deallocate(utable)

! right now, ens is made ideantical to static CV
allocate(evars2d(nc2d),evars3d(nc3d))
evars2d=cvars2d
evars3d=cvars3d

if (mype==0) then
    write(6,*) myname_,': 2D-CONTROL VARIABLES ARE ', cvars2d
    write(6,*) myname_,': 3D-CONTROL VARIABLES ARE ', cvars3d
    write(6,*) myname_,': MOTLEY CONTROL VARIABLES ', cvarsmd
    write(6,*) myname_,': ALL CONTROL VARIABLES    ', nrf_var
end if
lcalc_gfdl_cfrac = .false.
c2sset_flg = .true.   ! set to true in setup.  set to false after first (only) call to c2sset
e2sset_flg = .true.   ! set to true in setup.  set to false after first (only) call to ensctl2state_set

end subroutine init_anacv
subroutine final_anacv
  implicit none
  deallocate(nrf_var)
  deallocate(nrf_3d,nrf2_loc,nrf3_loc,nmotl_loc)
  deallocate(as3d,as2d)
  deallocate(an_amp0)
  deallocate(atsfc_sdv)
  deallocate(cvarsmd)
  deallocate(cvars2d,cvars3d)
  deallocate(evars2d,evars3d)
end subroutine final_anacv

! ----------------------------------------------------------------------
subroutine allocate_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    allocate_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional allocation of hybrid ensemble control variable a_en
!   2010-02-20  parrish - add structure variable grd_ens as part of changes for dual-resolution
!                           hybrid ensemble system.
!   2010-02-25  zhu     - use nrf_var and nrf_3d to specify the order control variables
!   2010-05-01  todling - update to use gsi_bundle
!   2010-05-17  todling - add back ens control; w/ a twist from original
!   2010-05-22  todling - add support for motley variables
!
!   input argument list:
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: grd_ens
  use hybrid_ensemble_parameters, only: naensgrp
  implicit none
  type(control_vector), intent(  out) :: ycv
  integer(i_kind) :: ii,jj,nn,ndim,ierror,n_step,n_aens
  integer(i_kind) :: ig
  character(len=256)::bname
  character(len=max_varname_length)::ltmp(1) 
  type(gsi_grid) :: grid_motley

  if (ycv%lallocated) then
     write(6,*)'allocate_cv: vector already allocated'
     call stop2(108)
  end if

  ycv%lallocated=.true.
  ycv%lencv = nclen
  ALLOCATE(ycv%values(ycv%lencv))

! If so, define grid of regular control vector
  n_step=0
! if (beta_s0>tiny_r_kind) then
      ALLOCATE(ycv%step(nsubwin))
      call GSI_GridCreate(ycv%grid_step,lat2,lon2,nsig)
         if (lsqrtb) then
            n_step=nval_len
         else
            n_step=nc3d*latlon1n+nc2d*latlon11
         endif
      if(mvars>0) then
         ALLOCATE(ycv%motley(nsubwin))
         call GSI_GridCreate(grid_motley,lat2,lon2,-1) ! this is sort of wired-in
      endif
! endif

! If so, define grid of ensemble control vector
  n_aens=0
  if (l_hyb_ens) then
      ALLOCATE(ycv%aens(nsubwin,naensgrp,n_ens))
      call GSI_GridCreate(ycv%grid_aens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
         if (lsqrtb) then
            n_aens=nval_lenz_en
         else
            n_aens=grd_ens%latlon11*grd_ens%nsig
         endif
  endif

! Loop over sub-windows in time
  ii=0; ndim=0
  do jj=1,nsubwin

!    Set static part of control vector (non-ensemble-based)
!    if (beta_s0>tiny_r_kind) then
         ycv%step(jj)%values => ycv%values(ii+1:ii+n_step)

         write(bname,'(a,i3.3)') 'Static Control Bundle subwin-',jj
         call GSI_BundleSet(ycv%step(jj),ycv%grid_step,bname,ierror,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
         if (ierror/=0) then
             write(6,*)'allocate_cv: error alloc(static bundle)'
             call stop2(109)
         endif
         ndim=ndim+ycv%step(jj)%ndim

         ii=ii+n_step

!        If motley variables needed (these don't contribute to CV size)
!        Presently, there are thankfully only 2d-fields in this category
         if(mvars>0) then
            write(bname,'(a,i3.3,a,i4.4)') 'Motley Control Bundle subwin-',jj
            call GSI_BundleCreate(ycv%motley(jj),grid_motley,bname,ierror,names2d=cvarsmd,bundle_kind=r_kind)
            if (ierror/=0) then
                write(6,*)'allocate_cv: error alloc(motley bundle)'
                call stop2(109)
            endif
         endif
!    endif ! beta_s0

!    Set ensemble-based part of control vector
     if (l_hyb_ens) then

         ltmp(1)='a_en'
         do ig=1,naensgrp
            do nn=1,n_ens
               ycv%aens(jj,ig,nn)%values => ycv%values(ii+1:ii+n_aens)
               write(bname,'(a,i3.3,a,i4.4)') 'Ensemble Control Bundle subwin-',jj,' and member-',nn
               call GSI_BundleSet(ycv%aens(jj,ig,nn),ycv%grid_aens,bname,ierror,names3d=ltmp,bundle_kind=r_kind)
               if (ierror/=0) then
                  write(6,*)'allocate_cv: error alloc(ensemble bundle)'
                  call stop2(109)
               endif
               ndim=ndim+ycv%aens(jj,ig,nn)%ndim

               ii=ii+n_aens
            enddo
         enddo

     endif

  enddo

  ycv%predr => ycv%values(ii+1:ii+nsclen)
  ii=ii+nsclen
  ycv%predp => ycv%values(ii+1:ii+npclen)
  ii=ii+npclen
  if (ntclen>0) then
     ycv%predt => ycv%values(ii+1:ii+ntclen)
     ii=ii+ntclen
  end if

  if (ii/=nclen) then
     write(6,*)'allocate_cv: error length',ii,nclen
     call stop2(109)
  end if

! Construct a list of integer pointers to the static and motley part of the 
! control vector (this is to support operations on current (mpi) distribution)
! it's enough to get these pointers to nsubwin=1, since they only serve the 
! purpose of indexing arrays operating on single window (e.g. see grid2sub)
! allocate(ycv%ivalues(n3d+n2d+mvars))
! ii=0
! do i=1,nc3d
!    call gsi_bundlegetpointer (ycv%step(1),cvars3d,iptr,istatus,ival=ival)
!    if(istatus==0) then
!       ii=ii+1
!       ycv%ivalues(ii)=ival
!    endif
! enddo
! do i=1,nc2d
!    call gsi_bundlegetpointer (ycv%step(1),cvars2d,iptr,istatus,ival=ival)
!    if(istatus==0) then
!       ii=ii+1
!       ycv%ivalues(ii)=ival
!    endif
! enddo
! do i=1,mvars
!    call gsi_bundlegetpointer (ycv%motley(1),cvarsmd,iptr,istatus,ival=ival)
!    if(istatus==0) then
!       ii=ii+1
!       ycv%ivalues(ii)=ival
!    endif
! enddo


  m_allocs=m_allocs+1
  m_vec_alloc=m_vec_alloc+1
  max_vec_alloc=MAX(max_vec_alloc,m_vec_alloc)

  return
end subroutine allocate_cv
! ----------------------------------------------------------------------
subroutine deallocate_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deallocate_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add optional removal of pointer to hybrid ensemble control variable a_en
!   2010-02-25  zhu     - add flexibility to control variable
!   2010-05-01  todling - update to use gsi_bundle
!   2010-05-17  todling - add back ens control; w/ a twist from original
!   2010-05-22  todling - add support for motley variables
!   2011-09-05  todling - replace destroy calls with unset calls
!
!   input argument list:
!    ycv
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: naensgrp
  implicit none
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii,nn,ierror
  integer(i_kind) :: ig

  if (ycv%lallocated) then
     do ii=1,nsubwin
        if (l_hyb_ens) then
           do ig=1,naensgrp
              do nn=n_ens,1,-1
                 call GSI_BundleUnset(ycv%aens(ii,ig,nn),ierror)
              enddo
           enddo
        endif
!       if (beta_s0>tiny_r_kind) then
           if(mvars>0) then
              call GSI_BundleDestroy(ycv%motley(ii),ierror)
           endif
           call GSI_BundleUnset(ycv%step(ii),ierror)
!       endif ! beta_s0
     end do
     NULLIFY(ycv%predr)
     NULLIFY(ycv%predp)
     NULLIFY(ycv%predt)

     if(l_hyb_ens) DEALLOCATE(ycv%aens)
     if(mvars>0) DEALLOCATE(ycv%motley)
     DEALLOCATE(ycv%step)
     DEALLOCATE(ycv%values)

     ycv%lallocated=.false.

     m_deallocs=m_deallocs+1
     m_vec_alloc=m_vec_alloc-1
  else
     if (mype==0) write(6,*)'deallocate_cv warning: vector not allocated'
  endif

  return
end subroutine deallocate_cv
! ----------------------------------------------------------------------
subroutine assign_scalar2cv(ycv,pval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_scalar2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    pval
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind)        , intent(in   ) :: pval
  integer(i_kind) :: ii

  DO ii=1,ycv%lencv
     ycv%values(ii)=pval
  ENDDO

  return
end subroutine assign_scalar2cv
! ----------------------------------------------------------------------
subroutine assign_cv2cv(ycv,xcv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_cv2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    xcv
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  type(control_vector), intent(in   ) :: xcv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'assign_cv2cv: error length',xcv%lencv,ycv%lencv
     call stop2(110)
  end if

  DO ii=1,ycv%lencv
     ycv%values(ii)=xcv%values(ii)
  ENDDO

  return
end subroutine assign_cv2cv
! ----------------------------------------------------------------------
subroutine assign_array2cv(ycv,parray)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_array2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    parray
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind)        , intent(in   ) :: parray(:)
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
     write(6,*)'assign_array2cv: array wrong length',size(parray),ycv%lencv
     call stop2(111)
  end if


  DO ii=1,ycv%lencv
     ycv%values(ii)=parray(ii)
  ENDDO

  return
end subroutine assign_array2cv
! ----------------------------------------------------------------------
subroutine assign_cv2array(parray,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assign_array2cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!
!   output argument list:
!    parray
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind)        , intent(  out) :: parray(:)
  type(control_vector), intent(in   ) :: ycv
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
     write(6,*)'assign_cv2array: array wrong length',size(parray),ycv%lencv
     call stop2(112)
  end if

  DO ii=1,ycv%lencv
     parray(ii)=ycv%values(ii)
  ENDDO

  return
end subroutine assign_cv2array
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_sub(xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_sub
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2009-09-20  parrish - add hybrid ensemble control variable a_en contribution to dot product
!   2010-05-17  todling - update to use bundle
!   2010-06-02  parrish - add contribution from ensemble control variable to dot product
!   2010-10-08  derber  - optimize and clean up
!   2011-09-05  todling - add hybrid to sqrt part of code
!
!   input argument list:
!    xcv,ycv
!
!   output argument list:
!    prods
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: naensgrp
  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv
  integer(i_kind) :: ii,nn,m3d,m2d,i,j,itot
  integer(i_kind) :: ig,nigtmp
  real(r_quad),allocatable,dimension(:) :: partsum

  qdot_prod_sub=zero_quad

! Independent part of vector
  if (lsqrtb) then
     do ii=1,nsubwin
        qdot_prod_sub=qdot_prod_sub+qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
     end do
     if(l_hyb_ens) then
        do ig=1,naensgrp
           do nn=1,n_ens
              do ii=1,nsubwin
                 qdot_prod_sub=qdot_prod_sub+qdot_product( xcv%aens(ii,ig,nn)%values(:) ,ycv%aens(ii,ig,nn)%values(:) )
              end do
           end do
        end do
     endif
  else
     m3d=xcv%step(1)%n3d
     m2d=xcv%step(1)%n2d
     itot=max(m3d,0)+max(m2d,0)
     if(l_hyb_ens)itot=itot+n_ens*naensgrp
     allocate(partsum(itot))
     partsum=zero_quad
     do ii=1,nsubwin
!$omp parallel do  schedule(dynamic,1) private(i)
        do i = 1,m3d
           partsum(i) = partsum(i)+dplevs(xcv%step(ii)%r3(i)%q,ycv%step(ii)%r3(i)%q,ihalo=1)
        enddo
!$omp parallel do  schedule(dynamic,1) private(i)
        do i = 1,m2d
           partsum(m3d+i) = partsum(m3d+i)+dplevs(xcv%step(ii)%r2(i)%q,ycv%step(ii)%r2(i)%q,ihalo=1)
        enddo
        if(l_hyb_ens) then
           do ig=1,naensgrp
              nigtmp=n_ens*(ig-1)
!$omp parallel do  schedule(dynamic,1) private(i)
              do i = 1,n_ens
                 partsum(m3d+m2d+nigtmp+i) = partsum(m3d+m2d+nigtmp+i) + &
                        dplevs(xcv%aens(ii,ig,i)%r3(1)%q,ycv%aens(ii,ig,i)%r3(1)%q,ihalo=1)
              end do
           end do
        end if
        do i=1,itot
          qdot_prod_sub = qdot_prod_sub + partsum(i)
        end do
     end do
     deallocate(partsum)
  end if

! Duplicated part of vector
  if(mype == minmype)then
     do j=nclen1+1,nclen
        qdot_prod_sub=qdot_prod_sub+xcv%values(j)*ycv%values(j) 
     end do
  end if

return
end function qdot_prod_sub
! ----------------------------------------------------------------------
subroutine qdot_prod_vars_eb(xcv,ycv,prods,eb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_vars_eb  copy of qdot_prod_vars for J_ens
!   prgmmr: parrish          org: np22                date: 2009-09-20
!
! abstract: copy qdot_prod_vars and add extra code to compute J_b or
!            J_ens when running in hybrid_ensemble mode.  extra input
!            character string eb is used to signal if J_b or J_ens is
!            to be computed.
!
! program history log:
!   2009-09-20  parrish - initial documentation
!   2010-05-17  todling - update to use bundle
!   2010-10-08  derber - optimize and clean up
!   2011-09-05  todling - add hybrid to sqrt part of code
!
!   input argument list:
!    xcv,ycv
!    eb        - eb= 'cost_b' then return J_b in prods
!                  = 'cost_e' then return J_ens in prods
!
!   output argument list:
!    prods
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: naensgrp
  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv
  character(len=*)    , intent(in   ) :: eb
  real(r_quad)        , intent(  out) :: prods(nsubwin+1)

  integer(i_kind) :: ii,i,nn,m3d,m2d
  real(r_quad),allocatable,dimension(:) :: partsum
  integer(i_kind) :: ig
  integer(i_kind) ::ngtmp,nn0

  prods(:)=zero_quad

! Independent part of vector
  if (lsqrtb) then
     if(trim(eb) == 'cost_b') then
        do ii=1,nsubwin
           prods(ii)=prods(ii)+qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
        end do
     endif
     if(trim(eb) == 'cost_e') then
        do ig=1,naensgrp
           do nn=1,n_ens
              do ii=1,nsubwin
                 prods(ii)=prods(ii)+qdot_product( xcv%aens(ii,ig,nn)%values(:) ,ycv%aens(ii,ig,nn)%values(:) )
              end do
           end do
        end do
     endif
  else
     if(trim(eb) == 'cost_b') then
        m3d=xcv%step(1)%n3d
        m2d=xcv%step(1)%n2d
        allocate(partsum(m2d+m3d))
        do ii=1,nsubwin                                                         
!$omp parallel do  schedule(dynamic,1) private(i)
           do i = 1,m3d
              partsum(i)= dplevs(xcv%step(ii)%r3(i)%q,ycv%step(ii)%r3(i)%q,ihalo=1)
           enddo
!$omp parallel do  schedule(dynamic,1) private(i)
           do i = 1,m2d
              partsum(m3d+i)= dplevs(xcv%step(ii)%r2(i)%q,ycv%step(ii)%r2(i)%q,ihalo=1)
           enddo
           do i = 1,m2d+m3d
              prods(ii)=prods(ii) + partsum(i)
           end do
        end do
        deallocate(partsum)
     end if
     if(trim(eb) == 'cost_e') then
        allocate(partsum(n_ens*naensgrp))
        do ii=1,nsubwin 
!$omp parallel do  schedule(dynamic,1) private(nn,m3d,m2d,ig,ngtmp,nn0)
           do ig=1,naensgrp
              ngtmp=(ig-1)*n_ens
              do nn=1,n_ens
                 nn0=nn+ngtmp
                 partsum(nn0) = zero_quad
                 m3d=xcv%aens(ii,ig,nn)%n3d
                 do i = 1,m3d
                    partsum(nn0)= partsum(nn0) + dplevs(xcv%aens(ii,ig,nn)%r3(i)%q,ycv%aens(ii,ig,nn)%r3(i)%q,ihalo=1)
                 enddo
                 m2d=xcv%aens(ii,ig,nn)%n2d
                 do i = 1,m2d
                    partsum(nn0)= partsum(nn0) + dplevs(xcv%aens(ii,ig,nn)%r2(i)%q,ycv%aens(ii,ig,nn)%r2(i)%q,ihalo=1)
                 enddo
              enddo
           end do
           do nn=1,n_ens*naensgrp
              prods(ii)=prods(ii)+partsum(nn)
           end do
        end do
        deallocate(partsum)
     end if
  end if

! Duplicated part of vector
  if(mype == minmype .and. trim(eb) == 'cost_b' ) then
     if (nsclen>0) then
        prods(nsubwin+1) = qdot_product(xcv%predr(:),ycv%predr(:))
     endif
     if (npclen>0) then
        prods(nsubwin+1) = prods(nsubwin+1) + qdot_product(xcv%predp(:),ycv%predp(:))
     endif
     if (ntclen>0) then
        prods(nsubwin+1) = prods(nsubwin+1) + qdot_product(xcv%predt(:),ycv%predt(:))
     endif
  end if

  call mpl_allreduce(nsubwin+1,qpvals=prods)


  return
end subroutine qdot_prod_vars_eb
! ----------------------------------------------------------------------
real(r_kind) function dot_prod_cv(xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dot_prod_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv,ycv
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: dd(1)

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'dot_prod_cv: error length',xcv%lencv,ycv%lencv
     call stop2(113)
  end if

  dd(1) = qdot_prod_sub(xcv,ycv)
  call mpl_allreduce(1,qpvals=dd)
  dot_prod_cv = dd(1)

return
end function dot_prod_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_cv(xcv,ycv,mold)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_cv
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2017-03-06  todling - rename interface variable to mold
!
!   input argument list:
!    xcv,ycv
!    mold      - interface device
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)     , intent(in   ) :: mold
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: dd(1)

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'qdot_prod_cv: error length',xcv%lencv,ycv%lencv
     call stop2(114)
  end if

  dd(1) = qdot_prod_sub(xcv,ycv)
  call mpl_allreduce(1,qpvals=dd)
  qdot_prod_cv = dd(1)

return
end function qdot_prod_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_cv_eb(xcv,ycv,mold,eb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_prod_cv_eb  copy of qdot_prod_cv for J_ens
!   prgmmr: parrish          org: np22                date: 2009-09-20
!
! abstract: copy qdot_prod_cv and add extra code to compute J_b or
!            J_ens when running in hybrid_ensemble mode.  extra input
!            character string eb is used to signal if J_b or J_ens is
!            to be computed.
!
! program history log:
!   2009-09-20  parrish - initial documentation
!   2017-03-06  todling - rename interface variable to mold
!
!   input argument list:
!    xcv,ycv
!    mold      - interface device
!    eb        - eb= 'cost_b' then return J_b in prods
!                  = 'cost_e' then return J_ens in prods
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind)     , intent(in   ) :: mold
  character(len=*)    , intent(in   ) :: eb
  type(control_vector), intent(in   ) :: xcv, ycv

! local variables
  real(r_quad) :: zz(nsubwin+1)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'qdot_prod_cv_eb: error length',xcv%lencv,ycv%lencv
      call stop2(114)
  end if

  call qdot_prod_vars_eb(xcv,ycv,zz,eb)

  qdot_prod_cv_eb= zero_quad
  do ii=1,nsubwin+1
     qdot_prod_cv_eb = qdot_prod_cv_eb + zz(ii)
  enddo

return
end function qdot_prod_cv_eb
! ----------------------------------------------------------------------
subroutine prt_norms(xcv,sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_norms
!   prgmmr:                  org:                     date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    sgrep
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: sgrep

  real(r_quad) :: zt

  zt = qdot_prod_cv(xcv,xcv,r_quad)
  zt=sqrt(zt)

  if (mype==0) then
     write(6,*)sgrep,' global  norm =',zt
  endif

!_RT  call prt_norms_vars(xcv,sgrep) --->> this routine is hanging

  return
end subroutine prt_norms
! ----------------------------------------------------------------------
subroutine prt_norms_vars(xcv,sgrep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prt_norms_vars
!   prgmmr: Jing Guo         org:  gmao               date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2010-05-06  todling- update to use gsi_bundle-like vector
!
!   input argument list:
!    xcv
!    sgrep
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use m_stats,only : stats_sum,stats_allreduce
  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: sgrep

  real   (r_kind),allocatable,dimension(:) :: vdot,vsum,vmin,vmax
  integer(i_kind),allocatable,dimension(:) :: vnum
  integer :: jj,nsw,iv,nv
  real(r_kind),pointer,dimension(:) :: piv

  nsw=size(xcv%step)

! process 3d variables first
  allocate(vdot(nc3d),vsum(nc3d),vmin(nc3d),vmax(nc3d),vnum(nc3d))
  do iv=1,nc3d
     do jj=1,nsw
        piv => rerank(xcv%step(jj)%r3(iv)%q)

        call stats_sum(piv, &
                       vdot(iv),vsum(iv),vmin(iv),vmax(iv),vnum(iv),add=jj>1)
     enddo

     call stats_allreduce(vdot(iv),vsum(iv),vmin(iv),vmax(iv),  &
                          vnum(iv),MPI_comm_world)
     nv=max(vnum(iv),1)
  
     if(mype==0) then
        write(6,'(2(1x,a),4(1x,ES20.12),1x,i10)')               &
          sgrep,cvars3d(iv),sqrt(vdot(iv)/nv),vsum(iv)/nv,       &
          vmin(iv),vmax(iv),vnum(iv)
     endif
  end do
  deallocate(vdot,vsum,vmin,vmax,vnum)

! process 2d now
  allocate(vdot(nc2d),vsum(nc2d),vmin(nc2d),vmax(nc2d),vnum(nc2d))
  do iv=1,nc2d
     do jj=1,nsw
        piv => rerank(xcv%step(jj)%r2(iv)%q)

        call stats_sum(piv, &
                       vdot(iv),vsum(iv),vmin(iv),vmax(iv),vnum(iv),add=jj>1)
     enddo

     call stats_allreduce(vdot(iv),vsum(iv),vmin(iv),vmax(iv),  &
                          vnum(iv),MPI_comm_world)
     nv=max(vnum(iv),1)
  
     if(mype==0) then
        write(6,'(2(1x,a),4(1x,ES20.12),1x,i10)')               &
          sgrep,cvars2d(iv),sqrt(vdot(iv)/nv),vsum(iv)/nv,       &
          vmin(iv),vmax(iv),vnum(iv)
     endif
  end do
  deallocate(vdot,vsum,vmin,vmax,vnum)

! release pointer
  piv => null()
  
end subroutine prt_norms_vars
! ----------------------------------------------------------------------
subroutine axpy(alpha,xcv,ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    axpy
!   prgmmr: todling          org: gmao                date:
!
! abstract: similar to BLAS axpy
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    alpha
!
!   output argument list:
!    ycv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind)        , intent(in   ) :: alpha
  type(control_vector), intent(in   ) :: xcv
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
     write(6,*)'axpy: error length',xcv%lencv,ycv%lencv
     call stop2(115)
  end if

  DO ii=1,ycv%lencv
     ycv%values(ii) = ycv%values(ii) + alpha * xcv%values(ii)
  ENDDO

  return
end subroutine axpy
! ----------------------------------------------------------------------
subroutine random_cv(ycv,kseed)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    random_cv
!   prgmmr: tremolet         org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!    kseed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use hybrid_ensemble_parameters, only: naensgrp
implicit none
type(control_vector)     , intent(inout) :: ycv
integer(i_kind), optional, intent(in   ) :: kseed

integer(i_kind):: ii,jj,nn,iseed
integer, allocatable :: nseed(:) ! Intentionaly default integer
real(r_kind), allocatable :: zz(:)
integer(i_kind) :: ig

iseed=iadatebgn
if (present(kseed)) iseed=iseed+kseed
call random_seed(size=jj)
allocate(nseed(jj))
nseed(1:jj)=iseed
! The following because we don't want all procs to get
! exactly the same sequence (which would be repeated in
! the then not so random vector) but it makes the test
! not reproducible if the number of procs is changed.
nseed(1)=iseed+mype
call random_seed(put=nseed)
deallocate(nseed)

allocate(zz(nval_len))
do jj=1,nsubwin
   call random_number(zz)
   do ii=1,nval_len
      ycv%step(jj)%values(ii) = two*zz(ii)-one
   enddo
enddo
deallocate(zz)

if (nval_lenz_en>0) then
   allocate(zz(nval_lenz_en))
   do nn=1,n_ens
      do ig=1,naensgrp
         do jj=1,nsubwin
            call random_number(zz)
            do ii=1,nval_lenz_en
               ycv%aens(jj,ig,nn)%values(ii) = two*zz(ii)-one
            enddo
         enddo
      enddo
   enddo
   deallocate(zz)
endif

if (nsclen>0) then
   allocate(zz(nsclen))
   call random_number(zz)
   do ii=1,nsclen
      ycv%predr(ii) = two*zz(ii)-one
   enddo
   deallocate(zz)
endif

if (npclen>0) then
   allocate(zz(npclen))
   call random_number(zz)
   do ii=1,npclen
      ycv%predp(ii) = two*zz(ii)-one
   enddo
   deallocate(zz)
endif

if (ntclen>0) then
   allocate(zz(ntclen))
   call random_number(zz)
   do ii=1,ntclen
      ycv%predt(ii) = two*zz(ii)-one
   enddo
   deallocate(zz)
endif

return
end subroutine random_cv
! ----------------------------------------------------------------------
subroutine write_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_cv
!   prgmmr: tremolet         org:  gmao               date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(in   ) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Writing control vector to file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  write(iunit)xcv%lencv
  write(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine write_cv
! ----------------------------------------------------------------------
subroutine read_cv(xcv,cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_cv
!   prgmmr:  tremolet        org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    xcv
!    cdfile
!
!   output argument list:
!    xcv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  type(control_vector), intent(inout) :: xcv
  character(len=*)    , intent(in   ) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit,ilen

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Reading control vector from file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  read(iunit)ilen
  if (ilen/=xcv%lencv) then
     write(6,*)'read_cv: wrong length',ilen,xcv%lencv
     call stop2(116)
  end if
  read(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine read_cv
! ----------------------------------------------------------------------
subroutine inquire_cv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inquire_cv
!   prgmmr: tremolet         org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
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
real(r_kind) :: zz

if (mype==0) then
   zz=real(max_vec_alloc*nclen,r_kind)*8.0_r_kind/1.048e6_r_kind
   write(6,*)'control_vectors: length=',nclen
   write(6,*)'control_vectors: currently allocated=',m_vec_alloc
   write(6,*)'control_vectors: maximum allocated=',max_vec_alloc
   write(6,*)'control_vectors: number of allocates=',m_allocs
   write(6,*)'control_vectors: number of deallocates=',m_deallocs
   write(6,'(A,F8.1,A)')'control_vectors: Estimated max memory used= ',zz,' Mb'
endif

end subroutine inquire_cv
! ----------------------------------------------------------------------
real(r_kind) function maxval_cv(ycv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    maxval_cv
!   prgmmr: tremolet         org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ycv
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
type(control_vector), intent(in   ) :: ycv
real(r_kind) :: zloc(1),zglo(1)

zloc(1)=maxval(ycv%values(:))

call mpi_allreduce(zloc,zglo,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
if (ierror/=0) then
   write(6,*)'maxval_cv: MPI error',ierror
   call stop2(117)
end if

maxval_cv=zglo(1)

return
end function maxval_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_product(x,y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    qdot_product
!   prgmmr: todling         org: gmao                date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    x,y
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  real(r_kind),intent(in   ) :: x(:),y(:)
  real(r_quad):: zz
  integer(i_kind) :: nx,ny,i
  nx=size(x)
  ny=size(y)
  if(nx/=ny) then
     write(6,*)'qdot_product: inconsistent dims',nx,ny
     call stop2(118)
  end if
  zz=zero_quad

  do i=1,nx
     zz = zz + x(i)*y(i)
  enddo
  qdot_product=zz
end function qdot_product
! ----------------------------------------------------------------------
end module control_vectors

