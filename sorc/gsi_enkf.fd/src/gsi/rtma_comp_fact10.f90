!===========================================================================
!===========================================================================
module aux2dvarflds
!$$$ module documentation block
!           .      .    .                                       .
! module:   ndfdgrids
!   prgmmr: pondeca          org: np23                date: 2019-08-27
!
! abstract: get navigational information pertaining to ndfd grid and
!           initialize other variables used with the rtma applications
!           of the gsi.
!
! program history log:
!   2019-08-27  pondeca - 
!
!
! subroutines included:
!   sub init_aux2dvarflds
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_aux2dvarflds
  public :: destroy_aux2dvarflds
  public :: rtma_comp_fact10

  real(r_kind),allocatable,dimension(:,:,:  )::g_presgsfc
  real(r_kind),allocatable,dimension(:,:,:  )::g_presgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::g_presgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::g_tmpgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::g_tmpgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::g_qgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::g_qgrid2
  real(r_kind),allocatable,dimension(:,:,:  )::g_ugrid1
  real(r_kind),allocatable,dimension(:,:,:  )::g_vgrid1
  real(r_kind),allocatable,dimension(:,:,:  )::g_hgtgrid1

contains

subroutine init_aux2dvarflds(mype)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: 
!
! program history log:
!   2019-08-27  pondeca
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use guess_grids, only: nfldsig
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_bundle
  use mpeu_util, only: die

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  character(len=*),parameter:: myname='init_aux2dvarflds'
  integer(i_kind) it, ier, istatus, n1,n2
  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()

  call gsi_bundlegetpointer(gsi_metguess_bundle(1),'presgsfc',rank2,istatus)
  if (istatus==0) then
     n1=size(rank2,1)
     n2=size(rank2,2)
     allocate(g_presgsfc  (n1,n2,nfldsig))
     allocate(g_presgrid1 (n1,n2,nfldsig))
     allocate(g_presgrid2 (n1,n2,nfldsig))
     allocate(g_tmpgrid1  (n1,n2,nfldsig))
     allocate(g_tmpgrid2  (n1,n2,nfldsig))
     allocate(g_qgrid1    (n1,n2,nfldsig))
     allocate(g_qgrid2    (n1,n2,nfldsig))
     allocate(g_ugrid1    (n1,n2,nfldsig))
     allocate(g_vgrid1    (n1,n2,nfldsig))
     allocate(g_hgtgrid1  (n1,n2,nfldsig))
   else
  endif

  do it=1,nfldsig
     ier=0
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'presgsfc',rank2,istatus)
     if (istatus==0) g_presgsfc(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'presgrid1',rank2,istatus)
     if (istatus==0) g_presgrid1(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'presgrid2',rank2,istatus)
     if (istatus==0) g_presgrid2(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tmpgrid1',rank2,istatus)
     if (istatus==0) g_tmpgrid1(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tmpgrid2',rank2,istatus)
     if (istatus==0) g_tmpgrid2(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'qgrid1',rank2,istatus)
     if (istatus==0) g_qgrid1(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'qgrid2',rank2,istatus)
     if (istatus==0) g_qgrid2(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'ugrid1',rank2,istatus)
     if (istatus==0) g_ugrid1(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'vgrid1',rank2,istatus)
     if (istatus==0) g_vgrid1(:,:,it)=rank2
     ier=ier+istatus

     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'hgtgrid1',rank2,istatus)
     if (istatus==0) g_hgtgrid1(:,:,it)=rank2
     ier=ier+istatus

     ier=ier+istatus
     if (ier/=0) then 
         if (mype==0) print*,'in init_aux2dvarflds: trouble for it=',it
        call die(myname,'not all fields available, ier=',ier)
     endif
  end do


end subroutine init_aux2dvarflds

subroutine destroy_aux2dvarflds
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2019-08-27
!
! abstract: 
!
! program history log:
!   2019-08-27  pondeca
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables

     if (allocated(g_presgsfc))  deallocate(g_presgsfc)
     if (allocated(g_presgrid1)) deallocate(g_presgrid1)
     if (allocated(g_presgrid2)) deallocate(g_presgrid2)
     if (allocated(g_tmpgrid1))  deallocate(g_tmpgrid1)
     if (allocated(g_tmpgrid2))  deallocate(g_tmpgrid2)
     if (allocated(g_qgrid1))    deallocate(g_qgrid1)
     if (allocated(g_qgrid2))    deallocate(g_qgrid2)
     if (allocated(g_ugrid1))    deallocate(g_ugrid1)
     if (allocated(g_vgrid1))    deallocate(g_vgrid1)
     if (allocated(g_hgtgrid1))  deallocate(g_hgtgrid1)
end subroutine destroy_aux2dvarflds

!===========================================================================
!===========================================================================
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  rtma_comp_fact10 --- Compute the factw with similarity theory
!
! !INTERFACE:
!

subroutine rtma_comp_fact10(dlat,dlon,dtime,zob,skint,sfcrough,islimsk,mype,factw)

! !USES:

  use guess_grids, only: nfldsig,hrdifsig
  use constants, only: zero,one,r100,fv
  implicit none
  
! !REVISION HISTORY:
!
!   2019-08-12  zhang/pondeca - initial code 
!
!
!
! Declare passed variables

  real(r_kind)   ,intent(in   ) :: dlat,dlon,dtime,skint,sfcrough,zob
  real(r_kind)   ,intent(out) :: factw
  integer(i_kind),intent(in   ) :: mype,islimsk


! Declare local parameters
  real(r_kind),parameter::  r0_001     = 0.001_r_kind
  character(len=*),parameter:: myname='rtma_comput_fact10'

! Declare external calls for code analysis
  external:: intrp2a11,tintrp2a11
  external:: SFC_WTQ_FWD

! Declare local variables
  integer(i_kind) islimsk2

! for similarity theory
  integer(i_kind) :: regime
  logical iqtflg
  real(r_kind) :: psges,tgges,roges
  real(r_kind) :: tv1,tv2
  real(r_kind) :: u10ges,v10ges,t2ges,q2ges
  real(r_kind) :: pres1,pres2,tmp1,tmp2,qq1,qq2,uu1,vv1,hgt1

  iqtflg=.true.  !.true. means output is virtual temperature

  islimsk2=islimsk
  if(islimsk2 > 2)islimsk2=islimsk2-3

  call tintrp2a11(g_presgsfc,psges,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_presgrid1,pres1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_presgrid2,pres2,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_tmpgrid1,tmp1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_tmpgrid2,tmp2,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_qgrid1,qq1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_qgrid2,qq2,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_ugrid1,uu1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_vgrid1,vv1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)
  call tintrp2a11(g_hgtgrid1,hgt1,dlat,dlon,dtime,hrdifsig,&
                  mype,nfldsig)

  !overwrite hgt1. Assume local height of model's first half-sigma level
  !                to be zob, that is, the ob height
  hgt1=zob

  tgges=skint

  !x convert input pressure variables from Pa to cb.
  psges  = r0_001*psges
  pres1  = r0_001*pres1
  pres2  = r0_001*pres2

  !convert sensible temperature to virtual temperature
  tv1=tmp1*((one+fv*qq1))
  tv2=tmp2*((one+fv*qq2))
  !!!tgges=tgges*((one+fv*qq1)) !no need for virtual temp conversion

  if (hgt1 <= sfcrough) then 
     factw=zero
  else

     islimsk2=islimsk
     if(islimsk2 > 2)islimsk2=islimsk2-3

     !unit change: originally m --> cm
     roges=sfcrough*r100

     call SFC_WTQ_FWD (psges, tgges,&
          pres1, tv1, qq1, uu1, vv1, &
          pres2, tv2, qq2, hgt1, roges, islimsk2, & !why is "comp_fact10" using islimsk instead of islimsk2?
          !output variables
          factw,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)
       
          if (factw > zero) factw=one/factw !you need this in the 2dvar application
                                            !since the goal is to reduce the
                                            !downscaled background 10-m wind
                                            !to the zob elevation
  endif
  end subroutine rtma_comp_fact10
!----------------------------------------------------------------------------------
end module aux2dvarflds
