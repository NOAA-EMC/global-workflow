subroutine genqsat(qsat,tsen,prsl,lat2,lon2,nsig,ice,iderivative)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genqsat
!   prgmmr: derber           org: np23                date: 1998-01-14
!
! abstract: obtain saturation specific humidity for given temperature.
!
! program history log:
!   1998-01-14  derber
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1903-10-07  Wei Gu, bug fixes,if qs<0,then set qs=0; merge w/ GSI by R Todling
!   2003-12-23  kleist, use guess pressure, adapt module framework
!   2004-05-13  kleist, documentation
!   2004-06-03  treadon, replace ggrid_g3 array with ges_* arrays
!   2005-02-23  wu, output dlnesdtv
!   2005-11-21  kleist, derber  add dmax array to decouple moisture from temp and
!               pressure for questionable qsat
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-09-18  derber - modify to limit saturated values near top
!   2006-11-22  derber - correct bug:  es<esmax should be es<=esmax
!   2008-06-04  safford - rm unused vars
!   2010-03-23  derber - simplify and optimize
!   2010-03-24  derber - generalize so that can be used for any lat,lon,nsig and any tsen and prsl (for hybrid)
!   2010-12-17  pagowski - add cmaq
!   2011-08-15  gu/todling - add pseudo-q2 options
!   2014-12-03  derber - add additional threading
!
!   input argument list:
!     tsen      - input sensibile temperature field (lat2,lon2,nsig)
!     prsl      - input layer mean pressure field (lat2,lon2,nsig)
!     lat2      - number of latitudes                              
!     lon2      - number of longitudes                             
!     nsig      - number of levels                              
!     ice       - logical flag:  T=include ice and ice-water effects,
!                 depending on t, in qsat calcuations.
!                 otherwise, compute qsat with respect to water surface
!     iderivative - if > 0 update derivatives in jfunc
!                 - if == 1 only update dqdrh
!                 - if == 2 update dqdrh, dqdt and dqdp
!
!   output argument list:
!     qsat      - saturation specific humidity (output)
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp,half,one_tenth
  use derivsmod, only:  qgues,dqdt,dqdrh,dqdp
  use jfunc, only:  pseudo_q2
  use gridmod, only:  wrf_nmm_regional,wrf_mass_regional,nems_nmmb_regional,aeta2_ll,regional,cmaq_regional
  use guess_grids, only: tropprs,ges_prslavg,ges_psfcavg
  implicit none

  logical                               ,intent(in   ) :: ice
  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: qsat
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: tsen,prsl
  integer(i_kind)                       ,intent(in   ) :: lat2,lon2,nsig,iderivative


  integer(i_kind) k,j,i,kpres,k150
  real(r_kind) pw,tdry,tr,es,es2
  real(r_kind) w,onep3,esmax
  real(r_kind) desidt,deswdt,dwdt,desdt,esi,esw
  real(r_kind),dimension(lat2):: mint,estmax
  integer(i_kind),dimension(lat2):: lmint
  logical:: idtupdate,idpupdate

! Declare local parameters
  real(r_kind),parameter:: r015 = 0.15_r_kind

  onep3 = 1.e3_r_kind

  if(iderivative > 0)then
    if (regional) then
        k150 = nsig
        do k=1,nsig
           if (ges_prslavg(k)<r015*ges_psfcavg) then
              k150 = k
              exit
           endif
        end do
    end if
    if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional) then
        kpres = nsig
        do k=1,nsig
           if (aeta2_ll(k)==zero) then
              kpres = k
              exit
           endif
        end do
    end if
  end if
!$omp parallel do  schedule(dynamic,1) private(k,j,i,tdry,tr,es,esw,esi,w) &
!$omp private(pw,esmax,es2,idpupdate,idtupdate,desdt,dwdt,deswdt,desidt) &
!$omp private(mint,lmint,estmax)
  do j=1,lon2
     do i=1,lat2
        mint(i)=340._r_kind
        lmint(i)=1
     end do
     do k=1,nsig
        do i=1,lat2
           if((prsl(i,j,k) < 30._r_kind .and.  &
               prsl(i,j,k) > 2._r_kind) .and.  &
               tsen(i,j,k) < mint(i))then
              lmint(i)=k
              mint(i)=tsen(i,j,k)
           end if
        end do
     end do
     do i=1,lat2
        tdry = mint(i)
        tr = ttp/tdry
        if (tdry >= ttp .or. .not. ice) then
           estmax(i) = psat * (tr**xa) * exp(xb*(one-tr))
        elseif (tdry < tmix) then
           estmax(i) = psat * (tr**xai) * exp(xbi*(one-tr))
        else
           w  = (tdry - tmix) / (ttp - tmix)
           estmax(i) =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                   + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
        endif
     end do

     do k = 1,nsig
        do i = 1,lat2

           tdry = tsen(i,j,k)
           tr = ttp/tdry
           if (tdry >= ttp .or. .not. ice) then
              es = psat * (tr**xa) * exp(xb*(one-tr))
           elseif (tdry < tmix) then
              es = psat * (tr**xai) * exp(xbi*(one-tr))
           else
              esw = psat * (tr**xa) * exp(xb*(one-tr)) 
              esi = psat * (tr**xai) * exp(xbi*(one-tr)) 
              w  = (tdry - tmix) / (ttp - tmix)
!             es =  w * esw + (one-w) * esi
              es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                       + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))

           endif

           pw = onep3*prsl(i,j,k)
           esmax = es
           if(lmint(i) < k)then
              esmax=0.1_r_kind*pw
              esmax=min(esmax,estmax(i))
           end if
           es2=min(es,esmax)
           qsat(i,j,k) = eps * es2 / (pw - omeps * es2)

           if(iderivative > 0)then
            if(es <= esmax .and. iderivative == 2)then
              idpupdate=.true.
              idtupdate=.true.

              if(regional)then

!    Special block to decouple temperature and pressure from moisture
!    above specified levels.  For mass core decouple T and p above
!    same level (approximately 150 hPa).  For nmm core decouple T
!    above ~150 hPa and p above level where aeta2_ll goes to zero

                if(wrf_mass_regional .and. k >= k150)then
!       For mass core, decouple T and p above 150 hPa
                  idpupdate=.false.
                  idtupdate=.false.
                end if
                if(wrf_nmm_regional .or. nems_nmmb_regional.or.&
                     cmaq_regional) then
!       Decouple T and p at different levels for nmm core
                  if(k >= kpres)idpupdate = .false.
                  if(k >= k150 )idtupdate = .false.
                end if
             
              else
!                Decouple Q from T above the tropopause for global
                if(prsl(i,j,k) < (one_tenth*tropprs(i,j)))then
                   idpupdate=.false.  
                   idtupdate=.false.
                end if
              end if

              if(idtupdate)then
                if (tdry >= ttp .or. .not. ice) then
                   desdt = es * (-xa/tdry + xb*ttp/(tdry*tdry))
                elseif (tdry < tmix) then
                   desdt = es * (-xai/tdry + xbi*ttp/(tdry*tdry))
                else
                   dwdt = one/(ttp-tmix)
                   deswdt = esw * (-xa/tdry + xb*ttp/(tdry*tdry))
                   desidt = esi * (-xai/tdry + xbi*ttp/(tdry*tdry))
                   desdt = dwdt*esw + w*deswdt - dwdt*esi + (one-w)*desidt
                endif
                if(pseudo_q2)then
                  dqdt(i,j,k)=zero
                else
                  dqdt(i,j,k)=(desdt/es)*qgues(i,j,k)
                endif
              else
                dqdt(i,j,k)=zero
              end if
              if(idpupdate)then
                if(pseudo_q2)then
                  dqdp(i,j,k)=zero
                else
                  dqdp(i,j,k)=half*qgues(i,j,k)/prsl(i,j,k)
                endif
              else
                dqdp(i,j,k)=zero
              end if
            else
              dqdt(i,j,k)=zero
              dqdp(i,j,k)=zero
            end if
            dqdrh(i,j,k) = qsat(i,j,k)
           end if

        end do
     end do
  end do
  return
end subroutine genqsat

