module intlightmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlightmod    int module for the observation operator for lightning flash rate (LFR)
!                 
!   prgmmr: k apodaca <karina.apodaca@colostate.edu>
!      org: CSU/CIRA, Data Assimilation Group
!     date: 2016-05-04
!
! abstract: module for the tangent linear (flashrate_TL) and adjoint models (flashrate_AD)
!           of LFR
!
! program history log:
!   2016-05-04  apodaca  - implement TL and AD of the LFR observation operator  
!   2018-02-08  apodaca  - replaced ob_type with polymorphic obsNode through type casting
!   2019-03-01  j guo    - encapsulated access to obsdiagNode through obsdiagNode_set()
!
! subroutines included:
!   sub intlight_
!
! variable definitions:
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$ end documentation block
use m_obsNode, only: obsNode
use m_lightNode, only: lightNode
use m_lightNode, only: lightNode_typecast
use m_lightNode, only: lightNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intlight

interface intlight; module procedure &
          intlight_
end interface

contains

subroutine intlight_(lighthead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlight      TL and subsequent AD of the forward observation operator for LFR
!     prgmmr:    k apodaca          
!        org:    CSU/CIRA, Data Assimilation Group             
!       date:    2016-05-04
!
! abstract: In this program, the tangent linear and adjoint models of a 
!           lightning flash rate observation operator are calculated 
!           using a 12-point horizontal grid to calculate finite-difference 
!           derivatives and for interpolation in specific quadrants.
! 
!           The tangent linear equations represent a way to map 
!           the perturbation vectors for the control variables 
!           q, qi, qs, qg, t, u, and v.
!
!           Moreover, the adjoint equations map the sensitivity gradient 
!           vectors for the control variables (q, qi, qs, qg, t, u, v), 
!           thus providing a first order aproximation or linear projection
!           of the sesitivity (impact) of observations.
!
! program history log:
!     2018-01-18 k apodaca revision of AD code
!     2018-08-18 k apodaca add a the TL and AD of second oservation operator for lightning 
!                          observations suitable for non-hydrostatic, cloud-resolving models
!                          with additional ice-phase hydrometeor control variables
!
!   input argument list:
!     lighthead   - obs type pointer to obs structure
!     sq          - q  increment in grid space
!     sqi         - qi increment in grid space
!     sqs         - qs increment in grid space
!     sqg         - qg increment in grid space
!     st          - t  increment in grid space
!     su          - u  increment in grid space
!     sv          - v  increment in grid space
!
!   output argument list:
!     rq, rqi, rqs, rqg       - control variabble updates resulting from 
!     rt, ru, rv                the assimilation of lightning flash rate 
!                               observations 
!
!   comments:
!
! attributes:
!   language: Fortran 90 and/or above
!   machine: 
!
!$$$ end subprogram documentation block

  use kinds,         only: r_kind,i_kind
  use obsmod,        only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use gridmod,       only: nsig
  use gridmod,       only: wrf_mass_regional,regional
  use qcmod,         only: nlnqc_iter,varqc_iter
  use constants,     only: zero,fv,one,half,two,tiny_r_kind,cg_term
  use jfunc,         only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar,     only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: lighthead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) k,ier,istatus
  integer(i_kind),dimension(nsig)           :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12
  real(r_kind) val,w1,w2,w3,w4
  real(r_kind) cg_light,grad,p0,wnotgross,wgross,pg_light
  real(r_kind),pointer,dimension(:)         :: sq,sqi,sqs,sqg,su,sv,st
  real(r_kind),pointer,dimension(:)         :: rq,rqi,rqs,rqg,ru,rv,rt
  type(lightNode),  pointer             :: lightptr

! Variables for TL and AD of lightning flash rate
  real(r_kind),dimension(1:nsig)            :: z_TL 
  real(r_kind),dimension(1:nsig)            :: horiz_adv_TL
  real(r_kind),dimension(1:nsig)            :: vert_adv_TL
  real(r_kind),dimension(1:nsig)            :: w_TL
  real(r_kind)                              :: wmaxi1_TL,wmaxi2_TL,wmaxi3_TL,wmaxi4_TL
  real(r_kind)                              :: flashrate_TL,flashratei1_TL,flashratei2_TL
  real(r_kind)                              :: flashratei3_TL, flashratei4_TL
  real(r_kind)                              :: h1i1_TL,h1i2_TL,h1i3_TL,h1i4_TL
  real(r_kind)                              :: h2i1_TL,h2i2_TL,h2i3_TL,h2i4_TL
  real(r_kind)                              :: totice_colinti1_TL,totice_colinti2_TL
  real(r_kind)                              :: totice_colinti3_TL,totice_colinti4_TL
  real(r_kind)                              :: htot_TL,htoti1_TL,htoti2_TL,htoti3_TL,htoti4_TL
  real(r_kind)                              :: flashrate_AD,flashratei1_AD,flashratei2_AD
  real(r_kind)                              :: flashratei3_AD,flashratei4_AD
  real(r_kind)                              :: wmaxi1_AD,wmaxi2_AD,wmaxi3_AD,wmaxi4_AD
  real(r_kind)                              :: h1i1_AD,h1i2_AD,h1i3_AD,h1i4_AD
  real(r_kind)                              :: h2i1_AD,h2i2_AD,h2i3_AD,h2i4_AD
  real(r_kind)                              :: totice_colinti1_AD,totice_colinti2_AD
  real(r_kind)                              :: totice_colinti3_AD,totice_colinti4_AD
  real(r_kind)                              :: htot_AD,htoti1_AD,htoti2_AD,htoti3_AD,htoti4_AD     
  real(r_kind),dimension(1:nsig)            :: z_AD
  real(r_kind),dimension(1:nsig)            :: w_AD
  real(r_kind),dimension(1:nsig)            :: vert_adv_AD,horiz_adv_AD
  real(r_kind),dimension(1:nsig)            :: diffq
  real(r_kind),dimension(1:nsig)            :: difft
  real(r_kind),dimension(1:nsig)            :: diffz
!  wmax variables for lightning flash rate
  real(r_kind)                             :: wmax   
  real(r_kind),parameter                   :: k3=0.95_r_kind
  
!  Output files
!  character :: tlh_file*40
     

!  If no light data return
  if(.not. associated(lighthead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'tsen',st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'qi',sqi,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'qi',rqi,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'qg',sqg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'qs',sqs,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
  if(ier/=0)return



  lightptr => lightNode_typecast(lighthead)
  do while (associated(lightptr))

! Load location information into local variables

     w1=lightptr%wij(1)
     w2=lightptr%wij(2)
     w3=lightptr%wij(3)
     w4=lightptr%wij(4)

     do k=1,nsig
        i1(k)=lightptr%ij(1,k)
        i2(k)=lightptr%ij(2,k)
        i3(k)=lightptr%ij(3,k)
        i4(k)=lightptr%ij(4,k)
        i5(k)=lightptr%ij(5,k)
        i6(k)=lightptr%ij(6,k)
        i7(k)=lightptr%ij(7,k)
        i8(k)=lightptr%ij(8,k)
        i9(k)=lightptr%ij(9,k)
        i10(k)=lightptr%ij(10,k)
        i11(k)=lightptr%ij(11,k)
        i12(k)=lightptr%ij(12,k)  
     end do 
     
!                .      .    .                                       .

! In the case of lightning observations (e.g. GOES/GLM), the schematic shown below is
! used for bi-linear interpolation of background fields to the location of an observation 
! (+) and for the finite-difference derivation method used in the calculation of the TL of
! the observation operator for lightning flash rate. Calculations are done
! at each quadrant, i.e., central, north, south, east, and west.
!
!         i6-------i8
!          |       |
!          |       |
! i10-----i2-------i4-----i12
!  |       |       |       |
!  |       |     + |       |
! i9------i1-------i3-----i11
!          |       |
!          |       |
!         i5-------i7
!

!                .      .    .                                       .
     
! In the following section, the tangent linear of the lightning flash rate observation  
! operator is calculated by being broken into parts.                                               

! Tangent linear of height (z)

     z_TL(:)=zero
     horiz_adv_TL(:)=zero

     do k=2,nsig-1

        z_TL(i1(1))=lightptr%jac_z0i1
        z_TL(i2(1))=lightptr%jac_z0i2
        z_TL(i3(1))=lightptr%jac_z0i3
        z_TL(i4(1))=lightptr%jac_z0i4
        z_TL(i5(1))=lightptr%jac_z0i5
        z_TL(i6(1))=lightptr%jac_z0i6
        z_TL(i7(1))=lightptr%jac_z0i7
        z_TL(i8(1))=lightptr%jac_z0i8
        z_TL(i9(1))=lightptr%jac_z0i9
        z_TL(i10(1))=lightptr%jac_z0i10
        z_TL(i11(1))=lightptr%jac_z0i11
        z_TL(i12(1))=lightptr%jac_z0i12


        z_TL(i1(k))=z_TL(i1(k-1))+lightptr%jac_vertti1(k)*st(i1(k))          &
                   +lightptr%jac_vertqi1(k)*sq(i1(k))

        z_TL(i2(k))=z_TL(i2(k-1))+lightptr%jac_vertti2(k)*st(i2(k))          &
                   +lightptr%jac_vertqi2(k)*sq(i2(k)) 

        z_TL(i3(k))=z_TL(i3(k-1))+lightptr%jac_vertti3(k)*st(i3(k))          &
                   +lightptr%jac_vertqi3(k)*sq(i3(k))
   
        z_TL(i4(k))=z_TL(i4(k-1))+lightptr%jac_vertti4(k)*st(i4(k))          &
                   +lightptr%jac_vertqi4(k)*sq(i4(k))

        z_TL(i5(k))=z_TL(i5(k-1))+lightptr%jac_vertti5(k)*st(i5(k))          &
                   +lightptr%jac_vertqi5(k)*sq(i5(k))

        z_TL(i6(k))=z_TL(i6(k-1))+lightptr%jac_vertti6(k)*st(i6(k))          &
                   +lightptr%jac_vertqi6(k)*sq(i6(k))

        z_TL(i7(k))=z_TL(i7(k-1))+lightptr%jac_vertti7(k)*st(i7(k))          &
                   +lightptr%jac_vertqi7(k)*sq(i7(k))

        z_TL(i8(k))=z_TL(i8(k-1))+lightptr%jac_vertti8(k)*st(i8(k))          &
                   +lightptr%jac_vertqi8(k)*sq(i8(k))

        z_TL(i9(k))=z_TL(i9(k-1))+lightptr%jac_vertti9(k)*st(i9(k))          &
                   +lightptr%jac_vertqi9(k)*sq(i9(k))

        z_TL(i10(k))=z_TL(i10(k-1))+lightptr%jac_vertti10(k)*st(i10(k))      &
                    +lightptr%jac_vertqi10(k)*sq(i10(k))

        z_TL(i11(k))=z_TL(i11(k-1))+lightptr%jac_vertti11(k)*st(i11(k))      & 
                  +lightptr%jac_vertqi11(k)*sq(i11(k))

        z_TL(i12(k))=z_TL(i12(k-1))+lightptr%jac_vertti12(k)*st(i12(k))      &
                  +lightptr%jac_vertqi12(k)*sq(i12(k))


! Tangent Linear of the Horizontal Advection Section

 
        horiz_adv_TL(i1(k))=lightptr%jac_zdxi1(k)*su(i1(k))                  &
                           +lightptr%jac_zdyi1(k)*sv(i1(k))                  &
                           +lightptr%jac_udxi1(k)*(z_TL(i3(k))-z_TL(i9(k)))  &
                           +lightptr%jac_vdyi1(k)*(z_TL(i2(k))-z_TL(i5(k)))
        horiz_adv_TL(i2(k))=lightptr%jac_zdxi2(k)*su(i2(k))                  &
                           +lightptr%jac_zdyi2(k)*sv(i2(k))                  &
                           +lightptr%jac_udxi2(k)*(z_TL(i4(k))-z_TL(i10(k))) &
                           +lightptr%jac_vdyi2(k)*(z_TL(i6(k))-z_TL(i1 (k)))

        horiz_adv_TL(i3(k))=lightptr%jac_zdxi3(k)*su(i3(k))                  &
                           +lightptr%jac_zdyi3(k)*sv(i3(k))                  &
                           +lightptr%jac_udxi3(k)*(z_TL(i11(k))-z_TL(i1(k))) &
                           +lightptr%jac_vdyi3(k)*(z_TL(i4 (k))-z_TL(i7(k)))

        horiz_adv_TL(i4(k))=lightptr%jac_zdxi4(k)*su(i4(k))                  &
                           +lightptr%jac_zdyi4(k)*sv(i4(k))                  &
                           +lightptr%jac_udxi4(k)*(z_TL(i12(k))-z_TL(i2(k))) &
                           +lightptr%jac_vdyi4(k)*(z_TL(i8 (k))-z_TL(i3(k)))

     enddo ! do k=2,nsig-1

! Tangent Linear of the Vertical Advection Section

     vert_adv_TL(:)=zero
     w_TL(:)=zero

     do k=1,nsig-1

        vert_adv_TL(i1(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti1(k)*    &
                          (((one+fv*lightptr%jac_qi1(k))*st(i1(k)))           & 
                          +(lightptr%jac_ti1(k)*fv*sq(i1(k))))

        vert_adv_TL(i2(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti2(k)*    &
                          (((one+fv*lightptr%jac_qi2(k))*st(i2(k)))           &
                          +(lightptr%jac_ti2(k)*fv*sq(i2(k))))

        vert_adv_TL(i3(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti3(k)*    &
                          (((one+fv*lightptr%jac_qi3(k))*st(i3(k)))           &
                          +(lightptr%jac_ti3(k)*fv*sq(i3(k))))

        vert_adv_TL(i4(k))=-lightptr%jac_vert(k)*lightptr%jac_sigdoti4(k)*    &
                          (((one+fv*lightptr%jac_qi4(k))*st(i4(k)))           &
                          +(lightptr%jac_ti4(k)*fv*sq(i4(k))))
      


! Tangent Linear of Vertical Velocity


        w_TL(i1(k))=horiz_adv_TL(i1(k))+vert_adv_TL(i1(k))
        w_TL(i2(k))=horiz_adv_TL(i2(k))+vert_adv_TL(i2(k))
        w_TL(i3(k))=horiz_adv_TL(i3(k))+vert_adv_TL(i3(k))
        w_TL(i4(k))=horiz_adv_TL(i4(k))+vert_adv_TL(i4(k))

     enddo !do k=1,nsig-1
 
!                .      .    .                                       .
! Tangent Linear of lightning flash rate

!                .      .    .                                       .
! Regional

     if (regional) then

!-- WRF-ARW

        if (wrf_mass_regional) then

! Tangent linear - Lightning flash rate as a function of
! vertical graupel flux within the mixed-phase region
! (-15 deg C)


           if (lightptr%kboti1 > zero) then
               h1i1_TL=lightptr%jac_qgmai1(lightptr%kboti1)*sqg(i1(lightptr%kboti1))+&
                       lightptr%jac_qgmbi1(lightptr%kboti1)*&
                       (half*(w_TL(i1(lightptr%kboti1))+w_TL(i1(lightptr%kboti1+1))))
               h1i1_TL=h1i1_TL/(abs(h1i1_TL))
           else
               h1i1_TL=zero
           endif

           if (lightptr%kboti2 > zero) then
               h1i2_TL=lightptr%jac_qgmai2(lightptr%kboti2)*sqg(i2(lightptr%kboti2))+&
                       lightptr%jac_qgmbi2(lightptr%kboti2)*&
                       (half*(w_TL(i2(lightptr%kboti2))+w_TL(i2(lightptr%kboti2+1))))
               h1i2_TL=h1i2_TL/(abs(h1i2_TL))
           else
               h1i2_TL=zero
           endif

           if (lightptr%kboti3 > zero) then
               h1i3_TL=lightptr%jac_qgmai3(lightptr%kboti3)*sqg(i3(lightptr%kboti3))+&
                       lightptr%jac_qgmbi3(lightptr%kboti3)*&
                       (half*(w_TL(i3(lightptr%kboti3))+w_TL(i3(lightptr%kboti3+1))))
               h1i3_TL=h1i3_TL/(abs(h1i3_TL))
           else
               h1i3_TL=zero
           endif
        
           if (lightptr%kboti4 > zero) then
               h1i4_TL=lightptr%jac_qgmai4(lightptr%kboti4)*sqg(i4(lightptr%kboti4))+&
                       lightptr%jac_qgmbi4(lightptr%kboti4)*&
                       (half*(w_TL(i4(lightptr%kboti4))+w_TL(i4(lightptr%kboti4+1))))
               h1i4_TL=h1i4_TL/(abs(h1i4_TL))
           else
               h1i4_TL=zero
           endif


! Tangent Linear - Lightning flash rate as a function of total column-integrated
! ice-phase hydrometeors

           totice_colinti1_TL=zero
           totice_colinti2_TL=zero
           totice_colinti3_TL=zero
           totice_colinti4_TL=zero

           do k=1,nsig-1

               totice_colinti1_TL = totice_colinti1_TL+lightptr%jac_icei1(k) * &
                                   (sqi(i1(k))+sqs(i1(k))+sqg(i1(k)))+&
                                    lightptr%jac_zicei1(k)*z_TL(i1(k))

               totice_colinti2_TL = totice_colinti2_TL+lightptr%jac_icei2(k) * &
                                   (sqi(i2(k))+sqs(i2(k))+sqg(i2(k)))+&
                                    lightptr%jac_zicei2(k)*z_TL(i2(k))

               totice_colinti3_TL = totice_colinti3_TL+lightptr%jac_icei3(k) * &
                                   (sqi(i3(k))+sqs(i3(k))+sqg(i3(k)))+&
                                    lightptr%jac_zicei3(k)*z_TL(i3(k))

               totice_colinti4_TL = totice_colinti4_TL+lightptr%jac_icei4(k) * &
                                   (sqi(i4(k))+sqs(i4(k))+sqg(i4(k)))+&
                                    lightptr%jac_zicei4(k)*z_TL(i4(k))

           enddo !do k=1,nsig-1

           h2i1_TL=(1-k3)*totice_colinti1_TL
           h2i2_TL=(1-k3)*totice_colinti2_TL
           h2i3_TL=(1-k3)*totice_colinti3_TL
           h2i4_TL=(1-k3)*totice_colinti4_TL


           htoti1_TL= h1i1_TL+h2i1_TL
           htoti2_TL= h1i2_TL+h2i2_TL
           htoti3_TL= h1i3_TL+h2i3_TL
           htoti4_TL= h1i4_TL+h2i4_TL

!  Interpolation of lightning flash rate to observation location (2D field)
!  Forward Model

           htot_TL = (w1*htoti1_TL + w2*htoti2_TL + &
                      w3*htoti3_TL + w4*htoti4_TL)
           val = htot_TL

        endif ! wrf_mass_regional      

     endif !if (regional) then
   
!                .      .    .                                       .
! Global 
   
     if (.not. regional) then ! Global

! Cloud Mask

! If clouds are present, find the maximum value of vertical velocity
! (wmax_TL) at four points sorounding an observation (+)
! and amongst all vertical levels, otherwise set wmax_TL to zero.

        wmaxi1_TL=zero
        wmaxi2_TL=zero
        wmaxi3_TL=zero
        wmaxi4_TL=zero

        if (lightptr%jac_wmaxflagi1) then
            wmax=-1.e+10_r_kind
            do k=1,nsig-1
               if (w_TL(i1(k)) > wmax) then
                   lightptr%jac_kverti1=k
                   wmaxi1_TL=w_TL(i1(lightptr%jac_kverti1))
               endif
               if (wmaxi1_TL < zero) then
                   wmaxi1_TL=zero
               endif
            enddo ! k loop
        endif

        if (lightptr%jac_wmaxflagi2) then
           wmax=-1.e+10_r_kind
           do k=1,nsig-1
              if (w_TL(i2(k)) > wmax) then
                 lightptr%jac_kverti2=k
                 wmaxi2_TL=w_TL(i2(lightptr%jac_kverti2))
              endif
              if (wmaxi2_TL <  zero) then
                 wmaxi2_TL=zero
              endif
           enddo ! k loop
        endif

        if (lightptr%jac_wmaxflagi3) then
            wmax=-1.e+10_r_kind
            do k=1,nsig-1
               if (w_TL(i3(k)) > wmax) then
                   lightptr%jac_kverti3=k
                   wmaxi3_TL=w_TL(i3(lightptr%jac_kverti3))
               endif
               if (wmaxi3_TL <  zero) then
                   wmaxi3_TL=zero
               endif
            enddo ! k loop
        endif

        if (lightptr%jac_wmaxflagi4) then
            wmax=-1.e+10_r_kind
            do k=1,nsig-1
               if (w_TL(i4(k)) > wmax) then
                   lightptr%jac_kverti4=k
                   wmaxi4_TL=w_TL(i4(lightptr%jac_kverti4))
               endif
               if (wmaxi4_TL < zero) then
                   wmaxi4_TL=zero
               endif
            enddo ! k loop
        endif

! Tangent Linear of Lightning Flash Rate
    
        flashratei1_TL=lightptr%jac_fratei1*wmaxi1_TL
        flashratei2_TL=lightptr%jac_fratei1*wmaxi2_TL
        flashratei3_TL=lightptr%jac_fratei1*wmaxi3_TL
        flashratei4_TL=lightptr%jac_fratei1*wmaxi4_TL

!  Interpolation of lightning flash rate to observation location (2D field)
!  Forward Model

        flashrate_TL = (w1*flashratei1_TL + w2*flashratei2_TL + & 
                     w3*flashratei3_TL + w4*flashratei4_TL)
        val =  flashrate_TL

     end if ! global block

     if (luse_obsdiag)then
         if (lsaveobsens) then
            grad = val*lightptr%raterr2*lightptr%err2
            !-- lightptr%diags%obssen(jiter) = grad
            call obsdiagNode_set(lightptr%diags,jiter=jiter,obssen=grad)
         else
            !-- if (lightptr%luse) lightptr%diags%tldepart(jiter)=val
            if (lightptr%luse) call obsdiagNode_set(lightptr%diags,jiter=jiter,tldepart=val)
         endif
      end if 
   

!                .      .    .                                       .

! Adjoint test
 
     if (l_do_adjoint) then
! Difference from observation
        if (.not. lsaveobsens) then
        if (.not. ladtest_obs)  val=val-lightptr%res

!       needed for gradient of nonlinear qc operator
           if (nlnqc_iter .and. lightptr%pg > tiny_r_kind .and.  &
                                lightptr%b  > tiny_r_kind) then
              pg_light=lightptr%pg*varqc_iter
              cg_light=cg_term/lightptr%b
              wnotgross= one-pg_light
              wgross = pg_light*cg_light/wnotgross
              p0   = wgross/(wgross+exp(-half*lightptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs) then
              grad = val
           else
              grad = val*lightptr%raterr2*lightptr%err2
           end if
         endif


!                .      .    .                                       .

! Adjoint of the Lightning Flash Rate Observation Operator   

!                .      .    .                                       .
! Variable initialization

            z_AD(:)=zero
            w_AD(:)=zero

! Regional

     if (regional) then

!-- WRF-ARW

        if (wrf_mass_regional) then

            htot_AD=grad

! Adjoint - Total lightning flash rate

            htoti1_AD=htoti1_AD+w1*htot_AD
            htoti2_AD=htoti2_AD+w1*htot_AD
            htoti3_AD=htoti3_AD+w1*htot_AD
            htoti4_AD=htoti4_AD+w1*htot_AD

            h1i1_AD=h1i1_AD+htoti1_AD
            h2i1_AD=h2i1_AD+htoti1_AD

            h1i2_AD=h1i2_AD+htoti2_AD
            h2i2_AD=h2i2_AD+htoti2_AD

            h1i3_AD=h1i3_AD+htoti3_AD
            h2i3_AD=h2i3_AD+htoti3_AD

            h1i4_AD=h1i4_AD+htoti4_AD
            h2i4_AD=h2i4_AD+htoti4_AD

            totice_colinti1_AD=totice_colinti1_AD+(1-k3)*h2i1_AD
            totice_colinti2_AD=totice_colinti2_AD+(1-k3)*h2i2_AD
            totice_colinti3_AD=totice_colinti3_AD+(1-k3)*h2i3_AD
            totice_colinti4_AD=totice_colinti4_AD+(1-k3)*h2i4_AD

! Adjoint - Lightning flash rate as a function of total column-integrated
! ice-phase hydrometeors


            do k=nsig-1,1,-1

               z_AD(i1(k))=z_AD(i1(k))+lightptr%jac_zicei1(k)*totice_colinti1_AD
               rqi(i1(k))=rqi(i1(k))+lightptr%jac_icei1(k)*totice_colinti1_AD
               rqs(i1(k))=rqs(i1(k))+lightptr%jac_icei1(k)*totice_colinti1_AD
               rqg(i1(k))=rqg(i1(k))+lightptr%jac_icei1(k)*totice_colinti1_AD
               totice_colinti1_AD=two*totice_colinti1_AD

               z_AD(i2(k))=z_AD(i2(k))+lightptr%jac_zicei2(k)*totice_colinti2_AD
               rqi(i2(k))=rqi(i2(k))+lightptr%jac_icei2(k)*totice_colinti2_AD
               rqs(i2(k))=rqs(i2(k))+lightptr%jac_icei2(k)*totice_colinti2_AD
               rqg(i2(k))=rqg(i2(k))+lightptr%jac_icei2(k)*totice_colinti2_AD
               totice_colinti2_AD=two*totice_colinti2_AD

               z_AD(i3(k))=z_AD(i3(k))+lightptr%jac_zicei3(k)*totice_colinti3_AD
               rqi(i3(k))=rqi(i3(k))+lightptr%jac_icei3(k)*totice_colinti3_AD
               rqs(i3(k))=rqs(i3(k))+lightptr%jac_icei3(k)*totice_colinti3_AD
               rqg(i3(k))=rqg(i3(k))+lightptr%jac_icei3(k)*totice_colinti3_AD
               totice_colinti3_AD=two*totice_colinti3_AD

               z_AD(i4(k))=z_AD(i4(k))+lightptr%jac_zicei4(k)*totice_colinti4_AD
               rqi(i4(k))=rqi(i4(k))+lightptr%jac_icei4(k)*totice_colinti4_AD
               rqs(i4(k))=rqs(i4(k))+lightptr%jac_icei4(k)*totice_colinti4_AD
               rqg(i4(k))=rqg(i4(k))+lightptr%jac_icei4(k)*totice_colinti4_AD
               totice_colinti4_AD=two*totice_colinti4_AD

! Adjoint - Lightning flash rate as a function of
! vertical graupel flux within the mixed-phase region
! (-15 deg C)

               if (lightptr%kboti1 > zero) then
                   h1i1_AD=h1i1_AD+(h1i1_AD/abs(h1i1_AD))
                   rqg(i1(lightptr%kboti1))=rqg(i1(lightptr%kboti1))+&
                                            lightptr%jac_qgmai1(lightptr%kboti1)*h1i1_AD
                   w_AD(i1(lightptr%kboti1))=w_AD(i1(lightptr%kboti1))+&
                                             half*lightptr%jac_qgmbi1(lightptr%kboti1)*h1i1_AD
                   w_AD(i1(lightptr%kboti1+1))=w_AD(i1(lightptr%kboti1+1))+&
                                               half*lightptr%jac_qgmbi1(lightptr%kboti1)*h1i1_AD
               else
                   h1i1_AD=zero
                   rqg(i1(lightptr%kboti1))=zero
                   w_AD(i1(lightptr%kboti1))=zero
                   w_AD(i1(lightptr%kboti1+1))=zero
               endif

               if (lightptr%kboti2 > zero) then
                   h1i2_AD=h1i2_AD+(h1i2_AD/abs(h1i2_AD))
                   rqg(i2(lightptr%kboti2))=rqg(i2(lightptr%kboti2))+&
                                            lightptr%jac_qgmai2(lightptr%kboti2)*h1i2_AD
                   w_AD(i2(lightptr%kboti2))=w_AD(i2(lightptr%kboti2))+&
                                             half*lightptr%jac_qgmbi2(lightptr%kboti2)*h1i2_AD
                   w_AD(i2(lightptr%kboti2+1))=w_AD(i2(lightptr%kboti2+1))+&
                                               half*lightptr%jac_qgmbi2(lightptr%kboti2)*h1i2_AD
               else
                   h1i2_AD=zero
                   rqg(i2(lightptr%kboti2))=zero
                   w_AD(i2(lightptr%kboti2+1))=zero
               endif

               if (lightptr%kboti3 > zero) then
                   h1i3_AD=h1i3_AD+(h1i3_AD/abs(h1i3_AD))
                   rqg(i3(lightptr%kboti3))=rqg(i3(lightptr%kboti3))+&
                                            lightptr%jac_qgmai3(lightptr%kboti3)*h1i3_AD
                   w_AD(i3(lightptr%kboti3))=w_AD(i3(lightptr%kboti3))+&
                                             half*lightptr%jac_qgmbi3(lightptr%kboti3)*h1i3_AD
                   w_AD(i3(lightptr%kboti3+1))=w_AD(i3(lightptr%kboti3+1))+&
                                               half*lightptr%jac_qgmbi3(lightptr%kboti3)*h1i3_AD
               else
                   h1i3_AD=zero
                   rqg(i3(lightptr%kboti3))=zero
                   w_AD(i3(lightptr%kboti3+1))=zero
               endif

               if (lightptr%kboti4 > zero) then
                   h1i4_AD=h1i4_AD+(h1i4_AD/abs(h1i4_AD))
                   rqg(i4(lightptr%kboti4))=rqg(i4(lightptr%kboti4))+&
                                            lightptr%jac_qgmai4(lightptr%kboti4)*h1i4_AD
                   w_AD(i4(lightptr%kboti4))=w_AD(i4(lightptr%kboti4))+&
                                             half*lightptr%jac_qgmbi4(lightptr%kboti4)*h1i4_AD
                   w_AD(i4(lightptr%kboti4+1))=w_AD(i4(lightptr%kboti4+1))+&
                                               half*lightptr%jac_qgmbi4(lightptr%kboti4)*h1i4_AD
               else
                   h1i4_AD=zero
                   rqg(i4(lightptr%kboti4))=zero
                   w_AD(i4(lightptr%kboti4+1))=zero
               endif


            enddo !do k=nsig-1,1,-1

        endif ! wrf_mass_regional

     endif !if (regional) then

!                .      .    .                                       .
! Global

     if (.not. regional) then

            flashrate_AD=grad
 

            flashratei1_AD=flashratei1_AD+w1*flashrate_AD
            flashratei2_AD=flashratei2_AD+w2*flashrate_AD 
            flashratei3_AD=flashratei3_AD+w3*flashrate_AD 
            flashratei4_AD=flashratei4_AD+w4*flashrate_AD

! Adjoint of Maximum Vertical Velocity 

            wmaxi1_AD=wmaxi1_AD+lightptr%jac_fratei1*flashratei1_AD
            wmaxi2_AD=wmaxi2_AD+lightptr%jac_fratei2*flashratei2_AD
            wmaxi3_AD=wmaxi3_AD+lightptr%jac_fratei3*flashratei3_AD
            wmaxi4_AD=wmaxi3_AD+lightptr%jac_fratei4*flashratei4_AD

            if (lightptr%jac_wmaxflagi1) then
                wmax=-1.e+10_r_kind
                do k=nsig-1,1,-1
                   if (wmaxi1_AD <  zero) then
                       wmaxi1_AD=zero
                   endif
                   if (wmaxi1_AD > wmax) then
                       lightptr%jac_kverti1=k
                       w_AD(i1(lightptr%jac_kverti1))=w_AD(i1(lightptr%jac_kverti1))+wmaxi1_AD
                   endif
                enddo 
            endif
                
            if (lightptr%jac_wmaxflagi2) then
                wmax=-1.e+10_r_kind
                do k=nsig-1,1,-1
                   if (wmaxi2_AD <  zero) then
                       wmaxi2_AD=zero
                   endif
                   if (wmaxi2_AD > wmax) then
                       lightptr%jac_kverti2=k
                       w_AD(i2(lightptr%jac_kverti2))=w_AD(i2(lightptr%jac_kverti2))+wmaxi2_AD
                   endif
                enddo
            endif

            if (lightptr%jac_wmaxflagi3) then
                wmax=-1.e+10_r_kind
                do k=nsig-1,1,-1
                   if (wmaxi3_AD <  zero) then
                       wmaxi3_AD=zero
                   endif
                   if (wmaxi3_AD > wmax) then
                       lightptr%jac_kverti3=k
                       w_AD(i3(lightptr%jac_kverti3))=w_AD(i3(lightptr%jac_kverti3))+wmaxi3_AD
                   endif
                enddo
            endif

            if (lightptr%jac_wmaxflagi4) then
                wmax=-1.e+10_r_kind
                do k=nsig-1,1,-1
                   if (wmaxi4_AD <  zero) then
                       wmaxi4_AD=zero
                   endif
                   if (wmaxi4_AD > wmax) then
                       lightptr%jac_kverti4=k
                       w_AD(i4(lightptr%jac_kverti4))=w_AD(i4(lightptr%jac_kverti4))+wmaxi4_AD
                   endif
                enddo
            endif


     endif !  end global block
!                .      .    .                                       .

! Adjoint of Vertical Velocity (from Vertical and Horizontal Advection)

     vert_adv_AD(:)=zero
 
     do k=nsig-1,1,-1

        vert_adv_AD(i4(k))=vert_adv_AD(i4(k))+w_AD(i4(k))
        vert_adv_AD(i3(k))=vert_adv_AD(i3(k))+w_AD(i3(k))
        vert_adv_AD(i2(k))=vert_adv_AD(i2(k))+w_AD(i2(k))
        vert_adv_AD(i1(k))=vert_adv_AD(i1(k))+w_AD(i1(k))

     enddo

     horiz_adv_AD(:)=zero

     do k=nsig-1,2,-1

        horiz_adv_AD(i4(k))=horiz_adv_AD(i4(k))+w_AD(i4(k))
        horiz_adv_AD(i4(k))=horiz_adv_AD(i3(k))+w_AD(i3(k))
        horiz_adv_AD(i2(k))=horiz_adv_AD(i2(k))+w_AD(i2(k))
        horiz_adv_AD(i1(k))=horiz_adv_AD(i1(k))+w_AD(i1(k))

     enddo

! Adjoint of q and t from the Vertical Advection Section

     diffq(:)=zero
     difft(:)=zero

     do k=nsig-1,1,-1

        diffq(i1(k))=-(lightptr%jac_ti1(k)*fv*lightptr%jac_vert(K)        &
                     *lightptr%jac_sigdoti1(k))*vert_adv_AD(i1(k))
        difft(i1(k))=-((one+fv*lightptr%jac_qi1(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti1(k))*vert_adv_AD(i1(k))
        diffq(i2(k))=-(lightptr%jac_ti2(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti2(k))*vert_adv_AD(i2(k))
        difft(i2(k))=-((one+fv*lightptr%jac_qi2(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti2(k))*vert_adv_AD(i2(k))
        diffq(i3(k))=-(lightptr%jac_ti3(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti3(k))*vert_adv_AD(i3(k))
        difft(i3(k))=-((one+fv*lightptr%jac_qi3(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti3(k))*vert_adv_AD(i3(k))
        diffq(i4(k))=-(lightptr%jac_ti4(k)*fv*lightptr%jac_vert(k)        &
                     *lightptr%jac_sigdoti4(k))*vert_adv_AD(i4(k))
        difft(i4(k))=-((one+fv*lightptr%jac_qi4(k))*lightptr%jac_vert(k)  &
                     *lightptr%jac_sigdoti4(k))*vert_adv_AD(i4(k))

        rq(i1(k))=rq(i1(k))+diffq(i1(k))

        rt(i1(k))=rt(i1(k))+difft(i1(k))

        rq(i2(k))=rq(i2(k))+diffq(i2(k))

        rq(i3(k))=rq(i3(k))+diffq(i3(k))

        rt(i3(k))=rt(i3(k))+difft(i3(k))

        rq(i4(k))=rq(i4(k))+diffq(i4(k))

        rt(i4(k))=rt(i4(k))+difft(i4(k))

     enddo


 
! Adjoint of z, u, and v from the Horizontal Advection Section
   
     diffz(:)=zero
     z_AD(:)=zero

     do k=nsig-1,2,-1

        diffz(i5(k))=-lightptr%jac_vdyi1(k)*horiz_adv_AD(i1(k))
        diffz(i9(k))=-lightptr%jac_udxi1(k)*horiz_adv_AD(i1(k))

        z_AD(i5(k))=z_AD(i5(k))+diffz(i5(k))
        z_AD(i2(k))=z_AD(i2(k))+(lightptr%jac_vdyi1(k)*horiz_adv_AD(i1(k)))
        z_AD(i9(k))=z_AD(i9(k))+(diffz(i9(k)))
        z_AD(i3(k))=z_AD(i3(k))+(lightptr%jac_udxi1(k)*horiz_adv_AD(i1(k)))
  
        rv(i1(k))=rv(i1(k))+(lightptr%jac_zdyi1(k)*horiz_adv_AD(i1(k)))
        ru(i1(k))=ru(i1(k))+(lightptr%jac_zdxi1(k)*horiz_adv_AD(i1(k)))

        diffz(i1(k)) =-lightptr%jac_vdyi2(k)*horiz_adv_AD(i2(k))
        diffz(i10(k))=-lightptr%jac_udxi2(k)*horiz_adv_AD(i2(k))

        z_AD(i1(k))=z_AD(i1(k))+(diffz(i1(k)))
        z_AD(i6(k))=z_AD(i6(k))+(lightptr%jac_vdyi2(k)*horiz_adv_AD(i2(k)))
        z_AD(i10(k))=z_AD(i10(k))+(diffz(i10(k)))
        z_AD(i4(k))=z_AD(i4(k))+(lightptr%jac_udxi2(k)*horiz_adv_AD(i2(k)))
        rv(i2(k))=rv(i2(k))+(lightptr%jac_zdyi2(k)*horiz_adv_AD(i2(k)))
        ru(i2(k))=ru(i2(k))+(lightptr%jac_zdxi2(k)*horiz_adv_AD(i2(k)))

        diffz(i7(k))= -lightptr%jac_vdyi3(k)*horiz_adv_AD(i3(k))
        diffz(i1(k))= -lightptr%jac_udxi3(k)*horiz_adv_AD(i3(k))

        z_AD(i7(k)) =  z_AD(i7(k))+diffz(i7(k))
        z_AD(i4(k)) =  z_AD(i4(k))+(lightptr%jac_vdyi3(k)*horiz_adv_AD(i3(k)))
        z_AD(i1(k)) =  z_AD(i1(k))+diffz(i1(k))
        z_AD(i11(k))=  z_AD(i11(k))+(lightptr%jac_udxi3(k)*horiz_adv_AD(i3(k)))
        rv(i3(k)) =  rv(i3(k))+(lightptr%jac_zdyi3(k)*horiz_adv_AD(i3(k)))
        ru(i3(k)) =  ru(i3(k))+(lightptr%jac_zdxi3(k)*horiz_adv_AD(i3(k)))

        diffz(i3(k))=-lightptr%jac_vdyi4(k)*horiz_adv_AD(i4(k))
        diffz(i2(k))=-z_TL(i2(k))-lightptr%jac_udxi4(k)*horiz_adv_AD(i4(k))

        z_AD(i3(k)) =  z_AD(i3(k))+diffz(i3(k))
        z_AD(i8(k)) =  z_AD(i8(k))+(lightptr%jac_vdyi4(k)*horiz_adv_AD(i4(k)))
        z_AD(i2(k)) = z_TL(i2(k))+diffz(i2(k))
        z_AD(i12(k))= z_AD(i12(k))+(lightptr%jac_udxi4(k)*horiz_adv_AD(i4(k)))
        rv(i4(k)) = rv(i4(k))+(lightptr%jac_zdyi4(k)*horiz_adv_AD(i4(k)))
        ru(i4(k)) = ru(i4(k))+(lightptr%jac_zdxi4(k)*horiz_adv_AD(i4(k)))

     enddo

! Adjoint of q and t from the Calculation of Height (z)
  
     do k=nsig-1,2,-1

        rq(i1(k))=rq(i1(k))+lightptr%jac_vertqi1(k)*z_AD(i1(k))
        rt(i1(k))=rt(i1(k))+lightptr%jac_vertti1(k)*z_AD(i1(k)) 
        z_AD(i1(k-1))=z_AD(i1(k-1))+z_AD(i1(k))
        z_AD(i1(k))=zero

        rq(i2(k))=rq(i2(k))+lightptr%jac_vertqi2(k)*z_AD(i2(k)) 
        rt(i2(k))=rt(i2(k))+lightptr%jac_vertti12(k)*z_AD(i2(k))
        z_AD(i2(k-1))=z_AD(i2(k-1))+z_AD(i2(k))
        z_AD(i2(k))=zero

        rq(i3(k))=rq(i3(k))+lightptr%jac_vertqi3(k)*z_AD(i3(k))
        rt(i3(k))=rt(i3(k))+lightptr%jac_vertti3(k)*z_AD(i3(k))
        z_AD(i3(k-1))=z_AD(i3(k-1))+z_AD(i3(k))
        z_AD(i3(k))=zero

        rq(i4(k))=rq(i4(k))+lightptr%jac_vertqi4(k)*z_AD(i4(k))
        rt(i4(k))=rt(i4(k))+lightptr%jac_vertti4(k)*z_AD(i4(k))
        z_AD(i4(k-1))=z_AD(i4(k-1))+z_AD(i4(k))
        z_AD(i4(k))=zero

        rq(i5(k))=rq(i5(k))+lightptr%jac_vertqi5(k)*z_AD(i5(k))
        rt(i5(k))=rt(i5(k))+lightptr%jac_vertti5(k)*z_AD(i5(k))
        z_AD(i5(k-1))=z_AD(i5(k-1))+z_AD(i5(k))
        z_AD(i5(k))=zero

        rq(i6(k))=rq(i6(k))+lightptr%jac_vertqi6(k)*z_AD(i6(k))
        rt(i6(k))=rt(i6(k))+lightptr%jac_vertti6(k)*z_AD(i6(k))
        z_AD(i6(k-1))=z_AD(i6(k-1))+z_AD(i6(k))
        z_AD(i6(k))=zero

        rq(i7(k))=rq(i7(k))+lightptr%jac_vertqi7(k)*z_AD(i7(k))
        rt(i7(k))=rt(i7(k))+lightptr%jac_vertti7(k)*z_AD(i7(k))
        z_AD(i7(k-1))=z_AD(i7(k-1))+z_AD(i7(k))
        z_AD(i7(k))=zero

        rq(i8(k))=rq(i8(k))+lightptr%jac_vertqi8(k)*z_AD(i8(k))
        rt(i8(k))=rt(i8(k))+lightptr%jac_vertti8(k)*z_AD(i8(k))
        z_AD(i8(k-1))=z_AD(i8(k-1))+z_AD(i8(k))
        z_AD(i8(k))=zero

        rq(i9(k))=rq(i9(k))+lightptr%jac_vertqi9(k)*z_AD(i9(k))
        rt(i9(k))=rt(i9(k))+lightptr%jac_vertti9(k)*z_AD(i9(k))
        z_AD(i9(k-1))=z_AD(i9(k-1))+z_AD(i9(k))
        z_AD(i9(k))=zero

        rq(i10(k))=rq(i10(k))+lightptr%jac_vertqi10(k)*z_AD(i10(k))
        rt(i10(k))=rt(i10(k))+lightptr%jac_vertti10(k)*z_AD(i10(k))
        z_AD(i10(k-1))=z_AD(i10(k-1))+z_AD(i10(k))
        z_AD(i10(k))=zero

        rq(i11(k))=rq(i11(k))+lightptr%jac_vertqi11(k)*z_AD(i11(k))   
        rt(i11(k))=rt(i11(k))+lightptr%jac_vertti11(k)*z_AD(i11(k))
        z_AD(i11(k-1))=z_AD(i11(k-1))+z_AD(i11(k))
        z_AD(i11(k))=zero

        rq(i12(k))=rq(i12(k))+lightptr%jac_vertqi12(k)*z_AD(i12(k))
        rt(i12(k))=rt(i12(k))+lightptr%jac_vertti12(k)*z_AD(i12(k))
        z_AD(i12(k-1))=z_AD(i12(k-1))+z_AD(i12(k))
        z_AD(i12(k))=zero

     enddo
 

     endif !Adjoint

     lightptr => lightNode_nextcast(lightptr)

  enddo  ! do while (associated(lightptr))
    
  
  return
end subroutine intlight_

end module intlightmod
