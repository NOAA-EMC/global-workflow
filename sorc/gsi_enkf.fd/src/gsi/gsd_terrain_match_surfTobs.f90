subroutine gsd_terrain_match_surfTobs(mype,nreal,ndata,cdata_all)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  surfaceCrct_scanT      surface correction for T
!   prgmmr: Dezso            org: np22                date: 2009-10-05
!
! abstract:  This routine correction surface temperature based on terrain
!
! program history log:
!   2009-10-05  Dezso  
!   2010-06-09  Hu  move this subroutine to setupt  
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     mype         - mpi task id
!     nreal        - number of data elements per observation
!     ndata        - number of observations
!     cdata_all    - observation 
!
!   output argument list:
!     cdata_all    - observation after tunning
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: one
  use guess_grids, only: ntguessig,ntguessfc,geop_hgti,ges_tsen
  use convinfo, only: ictype
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer  
  use mpeu_util, only: die

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nreal
  integer(i_kind),intent(in):: ndata
  real(r_kind),dimension(nreal,ndata),intent(inout):: cdata_all

! Declare external calls for code analysis
  external:: intrp2a11

! Declare local parameters
  real(r_kind),parameter:: r0_5 = 0.5_r_kind
!  
!  gamd : maximum temperature lapse rate 
!  gami : minimum temperature lapse rate 
!  gamd and gami define the range of temperature lapse rate allowed
  real(r_kind),parameter:: gamd=0.0100_r_kind   !DEDE 29 April 2009
  real(r_kind),parameter:: gami=0.0005_r_kind   !DEDE 29 April 2009

! Declare local variables

  character(len=*),parameter::myname='gsd_terrain_match_surfTobs'
  real(r_kind) pres1,topo1,temp1,temp5,tvold,gamcorr,tvnew,factor 
  real(r_kind) geop5
  integer(i_kind) iobsout, kx, nc, ier, istatus
  real(r_kind) toe,dlat,dlon
  real(r_kind) stnelev, dlnpob, usage

  real(r_kind),dimension(:,:  ),pointer:: ges_ps_nt=>NULL()
  real(r_kind),dimension(:,:  ),pointer:: ges_z_nt =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tv_nt=>NULL()

  real(r_double) rstation_id
  logical iqtflg

!**************************************************************************

  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessfc),'ps',ges_ps_nt,  istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessfc),'z' ,ges_z_nt ,  istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'tv',ges_tv_nt,  istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,'missing fields, ier= ', ier)

  do iobsout=1,ndata
     rstation_id=cdata_all(6,iobsout)
     nc=int(cdata_all(8,iobsout))
     kx=ictype(nc)
     iqtflg=nint(cdata_all(9,iobsout)) == 0

!here starts surface data correction   DEDE 28 April 2009
     if(kx==181.or.kx==187) then
        toe     = cdata_all(1,iobsout)
        dlon    = cdata_all(2,iobsout)
        dlat    = cdata_all(3,iobsout)
        dlnpob  = cdata_all(4,iobsout)
        tvold   = cdata_all(5,iobsout)
        usage   = cdata_all(12,iobsout)
        stnelev = cdata_all(19,iobsout)

        call intrp2a11(geop_hgti(1,1,5,ntguessig),geop5,dlat,dlon,mype)
        call intrp2a11(ges_z_nt,topo1,dlat,dlon,mype)
        call intrp2a11(ges_ps_nt,pres1,dlat,dlon,mype)
        if(iqtflg)then
           call intrp2a11(ges_tv_nt(:,:,1),temp1,dlat,dlon,mype)
           call intrp2a11(ges_tv_nt(:,:,5),temp5,dlat,dlon,mype)
        else
           call intrp2a11(ges_tsen(:,:,1,ntguessig),temp1,dlat,dlon,mype)
           call intrp2a11(ges_tsen(:,:,5,ntguessig),temp5,dlat,dlon,mype)
        endif

        gamcorr=(temp1-temp5)/geop5  
        gamcorr=min(gamd,max(gamcorr,gami))

        factor=(topo1-stnelev)*gamcorr
        tvnew=tvold-factor 
        dlnpob=log(pres1)

        toe=cdata_all(1,iobsout)
        if(kx>179.and.kx<190) toe=toe*r0_5  !DEDE 12 Feb 2009

        cdata_all(1,iobsout)=toe
        cdata_all(4,iobsout)=dlnpob
        cdata_all(5,iobsout)=tvnew              !DEDE 5 May 2009
        cdata_all(19,iobsout)=topo1             !DEDE 14 May 2009

      endif !  kx == 181 and 187
  enddo   ! iobsout

! End of routine
  return
end subroutine gsd_terrain_match_surfTobs
