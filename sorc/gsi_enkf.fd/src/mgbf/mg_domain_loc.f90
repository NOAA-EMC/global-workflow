submodule(mg_parameter) mg_domain_loc
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_domain_loc
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Module that defines control paramters for application
!            of MGBF to localization
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   init_domain_loc -
!   sidesend_loc -
!   targup_loc -
!   targdn21_loc -
!   targdn32_loc -
!   targdn43_loc -
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind
implicit none

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine init_domain_loc(this)
!***********************************************************************
!                                                                      !
!  Initialize localization with application of MGBF                    !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type)::this
!----------------------------------------------------------------------

call sidesend_loc(this)
call targup_loc(this)
call targdn21_loc(this)
call targdn32_loc(this)
call targdn43_loc(this)

!----------------------------------------------------------------------
endsubroutine init_domain_loc

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sidesend_loc(this)
!***********************************************************************
!                                                                      !
!  Initialize sidesending pararameters for application MGBF to         !
!  localization                                                        !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
integer(i_kind):: ix_0,jy_0
integer(i_kind):: ix_c,jy_c
integer(i_kind):: ix_cc,jy_cc
integer(i_kind):: ix_ccc,jy_ccc
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------

!  write(10,'(a)') '   Generation 2'
!  write(10,'(a)') '----------------------'
!  write(10,'(a)') 'mype   Flsouth_loc(1) '

!  write(11,'(a)') '   Generation 2'
!  write(11,'(a)') '----------------------'
!  write(11,'(a)') 'mype   Flnorth_loc(1) '

!  write(12,'(a)') '   Generation 2'
!  write(12,'(a)') '----------------------'
!  write(12,'(a)') 'mype   Flwest_loc(1)  '

!  write(13,'(a)') '   Generation 2'
!  write(13,'(a)') '----------------------'
!  write(13,'(a)') 'mype   Fleast_loc(1)  '

!  write(14,'(a)') '   Generation 2'
!  write(14,'(a)') '----------------------'
!  write(14,'(a)') 'mype  Fitarg_s_loc(1)  '

!  write(15,'(a)') '   Generation 2'
!  write(15,'(a)') '----------------------'
!  write(15,'(a)') 'mype  Fitarg_n_loc(1)  '

!  write(16,'(a)') '   Generation 2'
!  write(16,'(a)') '----------------------'
!  write(16,'(a)') 'mype  Fitarg_w_loc(1)  '

!  write(17,'(a)') '   Generation 2'
!  write(17,'(a)') '----------------------'
!  write(17,'(a)') 'mype  Fitarg_e_loc(1)  '

!  do mype=0,nxm*nym-1

!
! Generation 1
!
    jy_0 = mype/nxm
    ix_0 = mype - jy_0*nxm +1
    jy_0 = jy_0 + 1

    Flsouth_loc(1)=jy_0==1
    Flnorth_loc(1)=jy_0==nym
    Flwest_loc(1) =ix_0==1
    Fleast_loc(1) =ix_0==nxm

    if(Flsouth_loc(1)) then
      Fitarg_s_loc(1) = -1
    else
      Fitarg_s_loc(1) = mype-nxm
    endif

    if(Flnorth_loc(1)) then
      Fitarg_n_loc(1) = -1
    else
      Fitarg_n_loc(1) = mype+nxm
    endif

    if(Flwest_loc(1)) then
      Fitarg_w_loc(1) = -1
    else
      Fitarg_w_loc(1) = mype-1
    endif

    if(Fleast_loc(1)) then
      Fitarg_e_loc(1) = -1
    else
      Fitarg_e_loc(1) = mype+1
    endif

!  write(10,'(i5,a,l5)') mype, ' ---> ',Flsouth_loc(1)
!  write(11,'(i5,a,l5)') mype, ' ---> ',Flnorth_loc(1)
!  write(12,'(i5,a,l5)') mype, ' ---> ',Flwest_loc(1)
!  write(13,'(i5,a,l5)') mype, ' ---> ',Fleast_loc(1)
!  write(14,'(i5,a,i5)') mype, ' ---> ',Fitarg_s_loc(1)
!  write(15,'(i5,a,i5)') mype, ' ---> ',Fitarg_n_loc(1)
!  write(16,'(i5,a,i5)') mype, ' ---> ',Fitarg_w_loc(1)
!  write(17,'(i5,a,i5)') mype, ' ---> ',Fitarg_e_loc(1)

!
! Generation 2
!

    if(ix_0 <= nxm/2 .and. jy_0 <= nym/2) then
      ix_c = ix_0
      jy_c = jy_0
    else &
    if( (nxm/2 < ix_0 .and. ix_0 <=nxm) .and. jy_0 <= nym/2) then
      ix_c = ix_0 - nxm/2
      jy_c = jy_0
    else &
    if(ix_0 <= nxm/2 .and. (nym/2 < jy_0 .and. jy_0 <=nym) ) then
      ix_c = ix_0
      jy_c = jy_0 - nym/2
    else &
    if( (nxm/2 < ix_0 .and. ix_0 <=nxm) .and. (nym/2 < jy_0 .and. jy_0 <=nym) ) then
      ix_c = ix_0 - nxm/2
      jy_c = jy_0 - nym/2
    end if

    Flsouth_loc(2)=jy_c==1
    Flnorth_loc(2)=jy_c==nym/2
    Flwest_loc(2) =ix_c==1
    Fleast_loc(2) =ix_c==nxm/2
     
    if(Flsouth_loc(2)) then
      Fitarg_s_loc(2) = -1
    else
      Fitarg_s_loc(2) = mype-nxm
    endif

    if(Flnorth_loc(2)) then
      Fitarg_n_loc(2) = -1
    else
      Fitarg_n_loc(2) = mype+nxm
    endif

    if(Flwest_loc(2)) then
      Fitarg_w_loc(2) = -1
    else
      Fitarg_w_loc(2) = mype-1
    endif

    if(Fleast_loc(2)) then
      Fitarg_e_loc(2) = -1
    else
      Fitarg_e_loc(2) = mype+1
    endif

!  write(10,'(i5,a,l5)') mype, ' ---> ',Flsouth_loc(2)
!  write(11,'(i5,a,l5)') mype, ' ---> ',Flnorth_loc(2)
!  write(12,'(i5,a,l5)') mype, ' ---> ',Flwest_loc(2)
!  write(13,'(i5,a,l5)') mype, ' ---> ',Fleast_loc(2)
!  write(14,'(i5,a,i5)') mype, ' ---> ',Fitarg_s_loc(2)
!  write(15,'(i5,a,i5)') mype, ' ---> ',Fitarg_n_loc(2)

!
! Generation 3
!
  if(ix_c <= nxm/4 .and. jy_c <= nym/4) then
    ix_cc = ix_c
    jy_cc = jy_c
  else &
  if(ix_c >  nxm/4 .and. jy_c <= nym/4) then
    ix_cc = ix_c-nxm/4
    jy_cc =jy_c
  else &
  if(ix_c <= nxm/4 .and. jy_c >  nym/4) then
    ix_cc = ix_c
    jy_cc =jy_c-nym/4
  else &
  if(ix_c > nxm/4 .and. jy_c >  nym/4) then
    ix_cc = ix_c-nxm/4
    jy_cc = jy_c-nym/4
  endif 
    
    Flsouth_loc(3)=jy_cc==1
    Flnorth_loc(3)=jy_cc==nym/4
    Flwest_loc(3) =ix_cc==1
    Fleast_loc(3) =ix_cc==nxm/4
     
    if(Flsouth_loc(3)) then
      Fitarg_s_loc(3) = -1
    else
      Fitarg_s_loc(3) = mype-nxm
    endif

    if(Flnorth_loc(3)) then
      Fitarg_n_loc(3) = -1
    else
      Fitarg_n_loc(3) = mype+nxm
    endif

    if(Flwest_loc(3)) then
      Fitarg_w_loc(3) = -1
    else
      Fitarg_w_loc(3) = mype-1
    endif

    if(Fleast_loc(3)) then
      Fitarg_e_loc(3) = -1
    else
      Fitarg_e_loc(3) = mype+1
    endif

!  write(10,'(i5,a,l5)') mype, ' ---> ',Flsouth_loc(3)
!  write(11,'(i5,a,l5)') mype, ' ---> ',Flnorth_loc(3)
!  write(12,'(i5,a,l5)') mype, ' ---> ',Flwest_loc(3)
!  write(13,'(i5,a,l5)') mype, ' ---> ',Fleast_loc(3)
!  write(14,'(i5,a,i5)') mype, ' ---> ',Fitarg_s_loc(3)
!  write(15,'(i5,a,i5)') mype, ' ---> ',Fitarg_n_loc(3)

!
! Generation 4
!
  if(ix_cc <= nxm/8 .and. jy_cc <= nym/8) then
    ix_ccc = ix_cc; jy_ccc = jy_cc
  else &
  if(ix_cc >  nxm/8 .and. jy_cc <= nym/8) then
    ix_ccc = ix_cc-nxm/8; jy_ccc =jy_cc
  else &
  if(ix_cc <= nxm/8 .and. jy_cc >  nym/8) then
    ix_ccc = ix_cc; jy_ccc =jy_cc-nym/8
  else &
  if(ix_cc > nxm/8 .and. jy_cc >  nym/8) then
    ix_ccc = ix_cc-nxm/8; jy_ccc =jy_cc-nym/8
  endif

    Flsouth_loc(4)=jy_ccc==1
    Flnorth_loc(4)=jy_ccc==nym/8
    Flwest_loc(4) =ix_ccc==1
    Fleast_loc(4) =ix_ccc==nxm/8
     
    if(Flsouth_loc(4)) then
      Fitarg_s_loc(4) = -1
    else
      Fitarg_s_loc(4) = mype-nxm
    endif

    if(Flnorth_loc(4)) then
      Fitarg_n_loc(4) = -1
    else
      Fitarg_n_loc(4) = mype+nxm
    endif

    if(Flwest_loc(4)) then
      Fitarg_w_loc(4) = -1
    else
      Fitarg_w_loc(4) = mype-1
    endif

    if(Fleast_loc(4)) then
      Fitarg_e_loc(4) = -1
    else
      Fitarg_e_loc(4) = mype+1
    endif

!    enddo

!----------------------------------------------------------------------
endsubroutine sidesend_loc

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine targup_loc(this)
!***********************************************************************
!                                                                      !
! Initialize upsending pararameters for application MGBF to            !
! localization                                                         !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
integer(i_kind):: ix_0,jy_0
integer(i_kind):: ix_c,jy_c,mype_c
integer(i_kind):: ix_prox,jy_prox,targup
integer(i_kind):: n,is,js, mj2, il,jl
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!--------------------------------------------------------------------

!do mype=0,nxm*nym-1

  jy_0 = mype/nxm+1
  ix_0 = mype-(jy_0-1)*nxm+1

  mj2=mod(jy_0,2)
  mype_c=(nxm/2)*(jy_0-2+mj2)/2+(ix_0-1)/2

  jy_c = mype_c/(nxm/2)+1
  ix_c = mype_c-(jy_c-1)*(nxm/2)+1

    lsendup_sw_loc=(mod(ix_0,2)==1).and.(mod(jy_0,2)==1)
    lsendup_se_loc=(mod(ix_0,2)==0).and.(mod(jy_0,2)==1)
    lsendup_nw_loc=(mod(ix_0,2)==1).and.(mod(jy_0,2)==0)
    lsendup_ne_loc=(mod(ix_0,2)==0).and.(mod(jy_0,2)==0)

!
! g1 --> g2
!

  do n=1,4
    js=(n-1)/2
    is= n-1 -js*2
    ix_prox=ix_c+is*nxm/2
    jy_prox=jy_c+js*nym/2
    
      Fitargup_loc12(n)=nxm*(jy_prox-1)+ix_prox-1
  enddo

!  write(12,'(i5,a,4i5)') mype,' ---> ', Fitargup_loc12(1),Fitargup_loc12(2),Fitargup_loc12(3),Fitargup_loc12(4)

!
! g2 --> g3
!
    il = (ix_0-1)/(nxm/2)
    jl = (jy_0-1)/(nym/2)
   
  do n=1,4
    js=(n-1)/2
    is= n-1-js*2
    ix_prox=ix_c +is*nxm/4 + il*nxm/4
    jy_prox=jy_c +js*nym/4 + jl*nym/4
    
      Fitargup_loc23(n)=nxm*(jy_prox-1)+ix_prox-1
  enddo

!  write(23,'(i5,a,4i5)') mype,' ---> ', Fitargup_loc23(1),Fitargup_loc23(2),Fitargup_loc23(3),Fitargup_loc23(4)

!
! g3 --> g4
!
    il = (ix_0-1)/(nxm/4)
    jl = (jy_0-1)/(nym/4)
   
  do n=1,4
    js=(n-1)/2
    is= n-1-js*2
    ix_prox=ix_c +is*nxm/8 + il*nxm/8
    jy_prox=jy_c +js*nym/8 + jl*nym/8

      Fitargup_loc34(n)=nxm*(jy_prox-1)+ix_prox-1
   enddo

!  write(34,'(i5,a,4i5)') mype,' ---> ', 
!Fitargup_loc34(1),Fitargup_loc34(2),Fitargup_loc34(3),Fitargup_loc34(4)

!enddo

!----------------------------------------------------------------------
endsubroutine targup_loc

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine targdn21_loc(this)
!***********************************************************************
!                                                                      !
! Initialize downsending pararameters for application MGBF to          !
! localization from g2 go g1                                           !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
integer:: ix_t,jy_t
integer:: ix_l,jy_l
integer:: ix_sw,jy_sw
integer:: ix_se,jy_se
integer:: ix_nw,jy_nw
integer:: ix_ne,jy_ne
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!------------------------------------------------------------------------

!    write(11,'(a)') 'mype     itargdn_xx_loc21  nsq21 '
!    write(11,'(a)') '---------------------------------'

!  do mype=0,nxm*nym-1

    jy_t = mype/nxm+1
    ix_t = mype-(jy_t-1)*nxm+1

!
! Square 1
!
      if(ix_t <= nxm/2 .and. jy_t <= nym/2) then
        ix_l = ix_t
        jy_l = jy_t
        nsq21 = 1
      else &
!
! Square 2
!
      if( (nxm/2 < ix_t .and. ix_t <= nxm) .and. jy_t <= nym/2) then
        ix_l = ix_t-nxm/2
        jy_l = jy_t
        nsq21 = 2
      else &
!
! Square 3
!
      if( ix_t <= nxm/2 .and. (nym/2 < jy_t .and. jy_t <= nym)) then
        ix_l = ix_t
        jy_l = jy_t-nym/2
        nsq21 = 3
      else &
!
! Square 4
!
      if( (nxm/2 < ix_t .and. ix_t <= nxm) .and. (nym/2 < jy_t .and.  jy_t <= nym)) then
        ix_l = ix_t-nxm/2
        jy_l = jy_t-nym/2
        nsq21 = 4
      endif
      
          ix_sw = 2*ix_l-1 
          jy_sw = 2*jy_l-1 
          itargdn_sw_loc21 = nxm*(jy_sw-1)+ix_sw-1

          ix_se = ix_sw+1
          jy_se = jy_sw
          itargdn_se_loc21 = nxm*(jy_se-1)+ix_se-1

          ix_nw = ix_sw
          jy_nw = jy_sw+1
          itargdn_nw_loc21 = nxm*(jy_nw-1)+ix_nw-1

          ix_ne = ix_nw+1
          jy_ne = jy_nw
          itargdn_ne_loc21 = nxm*(jy_ne-1)+ix_ne-1

!    write(11,'(i6,a,2i4)') mype,'  <--  itargdn_sw_loc21 ',itargdn_sw_loc21,nsq
!    write(11,'(i6,a,2i4)') mype,'  <--  itargdn_se_loc21 ',itargdn_se_loc21,nsq
!    write(11,'(i6,a,2i4)') mype,'  <--  itargdn_nw_loc21 ',itargdn_nw_loc21,nsq
!    write(11,'(i6,a,2i4)') mype,'  <--  itargdn_ne_loc21 ',itargdn_ne_loc21,nsq

!  end do
!-----------------------------------------------------------
endsubroutine targdn21_loc

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine targdn32_loc(this)
!***********************************************************************
!                                                                      !
! Initialize downsending pararameters for application MGBF to          !
! localization from g3 go g2                                           !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
integer(i_kind):: ix_t,jy_t
integer(i_kind):: ix_l,jy_l
integer(i_kind):: ix_sw,jy_sw
integer(i_kind):: ix_se,jy_se
integer(i_kind):: ix_nw,jy_nw
integer(i_kind):: ix_ne,jy_ne
integer(i_kind):: facx,facy
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------

!    write(32,'(a)') 'mype     itargdn_xx_loc32  nsq32 '
!    write(32,'(a)') '---------------------------------'

!  do mype=0,nxm*nym-1

    jy_t = mype/nxm+1
    ix_t = mype-(jy_t-1)*nxm+1

!
! Square 1
!
      if(ix_t <= nxm/4 .and. jy_t <= nym/4) then
        ix_l = ix_t
        jy_l = jy_t
        nsq32 = 1
        facx = 0
        facy = 0
      else &
!
! Square 2
!
      if( (nxm/4 < ix_t .and.ix_t<=nxm/2 ) .and. jy_t <= nym/4) then
        ix_l = ix_t-nxm/4
        jy_l = jy_t
        nsq32 = 2
        facx = 0
        facy = 0
      else &
!
! Square 3
!
      if( ix_t <= nxm/4 .and. (nym/4 < jy_t .and. jy_t <= nym/2)) then
        ix_l = ix_t
        jy_l = jy_t-nym/4
        nsq32 = 3
        facx = 0
        facy = 0
      else &
!
! Square 4
!
      if( (nxm/4 < ix_t .and. ix_t <= nxm/2) .and. (nym/4 < jy_t .and.  jy_t <= nym/2)) then
        ix_l = ix_t-nxm/4
        jy_l = jy_t-nym/4
        nsq32 = 4
        facx = 0
        facy = 0
      else &
!
! Square 5
!
      if( (nxm/2 <ix_t .and. ix_t <= 3*nxm/4) .and. jy_t <= nym/4) then 
        ix_l = ix_t-nxm/2
        jy_l = jy_t
        nsq32 = 1
        facx = 1
        facy = 0
       else &
!
! Square 6
!
      if( (3*nxm/4 <ix_t .and. ix_t <= nxm) .and. jy_t <= nym/4) then
        ix_l = ix_t-3*nxm/4
        jy_l = jy_t
        nsq32 = 2
        facx = 1
        facy = 0
       else &
!
! Square 7
!
      if( (nxm/2 < ix_t .and. ix_t <= 3*nxm/4) .and. (nym/4< jy_t .and.  jy_t<= nym/2) ) then
        ix_l = ix_t-nxm/2
        jy_l = jy_t-nym/4
        nsq32 = 3
        facx = 1
        facy = 0
       else &
!
! Square 8
!
      if( (3*nxm/4 < ix_t .and. ix_t <= nxm) .and. (nym/4 < jy_t .and.  jy_t<= nym/2) ) then
        ix_l = ix_t-3*nxm/4
        jy_l = jy_t-nym/4
        nsq32 = 4
        facx = 1
        facy = 0
      else &
!
! Square 9
!
      if(ix_t <= nxm/4 .and. (nym/2 <jy_t .and. jy_t <= 3*nym/4) ) then
        ix_l = ix_t
        jy_l = jy_t-nym/2
        nsq32 = 1
        facx = 0
        facy = 1
      else &
!
! Square 10
!
      if( (nxm/4 < ix_t .and. ix_t <= nxm/2) .and. (nym/2 <jy_t .and.  jy_t <= 3*nym/4) ) then
        ix_l = ix_t-nxm/4
        jy_l = jy_t-nym/2
        nsq32 = 2
        facx = 0
        facy = 1
      else &
!
! Square 11
!
      if( ix_t  <= nxm/4 .and. (3*nym/4 <jy_t .and. jy_t <= nym) ) then
        ix_l = ix_t
        jy_l = jy_t-3*nym/4
        nsq32 = 3
        facx = 0
        facy = 1
       else &
!
! Square 12
!
      if( (nxm/4 < ix_t .and. ix_t <= nxm/2) .and. (3*nym/4 <jy_t .and.  jy_t <= nym) ) then
        ix_l = ix_t-nxm/4
        jy_l = jy_t-3*nym/4
        nsq32 = 4
        facx = 0
        facy = 1
       else &
!
! Square 13
!
      if( (nxm/2 < ix_t .and. ix_t <= 3*nxm/4) .and. (nym/2 <jy_t .and.  jy_t <= 3*nym/4) ) then
        ix_l = ix_t-nxm/2
        jy_l = jy_t-nym/2
        nsq32 = 1
        facx = 1
        facy = 1
       else &
!
! Square 14
!
      if( (3*nxm/4 < ix_t .and. ix_t <= nxm) .and. (nym/2 <jy_t .and.  jy_t <= 3*nym/4) ) then
        ix_l = ix_t-3*nxm/4
        jy_l = jy_t-nym/2
        nsq32 = 2
        facx = 1
        facy = 1
       else &
!
! Square 15
!
      if( (nxm/2 < ix_t .and. ix_t <= 3*nxm/4) .and. (3*nym/4 <jy_t .and. jy_t <= nym) ) then
        ix_l = ix_t-nxm/2
        jy_l = jy_t-3*nym/4
        nsq32 = 3
        facx = 1
        facy = 1
       else &
!
! Square 16
!
      if( (3*nxm/4 < ix_t .and. ix_t <= nxm) .and. (3*nym/4 <jy_t .and.  jy_t <= nym) ) then
        ix_l = ix_t-3*nxm/4
        jy_l = jy_t-3*nym/4
        nsq32 = 4
        facx = 1
        facy = 1

      endif
      
          ix_sw = 2*ix_l-1
          jy_sw = 2*jy_l-1
          itargdn_sw_loc32 = nxm*(jy_sw-1)+ix_sw-1 + facx*nxm/2 + facy*nxm*nym/2

          ix_se = ix_sw+1
          jy_se = jy_sw
          itargdn_se_loc32 = nxm*(jy_se-1)+ix_se-1 + facx*nxm/2+ facy*nxm*nym/2

          ix_nw = ix_sw
          jy_nw = jy_sw+1
          itargdn_nw_loc32 = nxm*(jy_nw-1)+ix_nw-1 + facx*nxm/2 + facy*nxm*nym/2

          ix_ne = ix_nw+1
          jy_ne = jy_nw
          itargdn_ne_loc32 = nxm*(jy_ne-1)+ix_ne-1 + facx*nxm/2 + facy*nxm*nym/2

!    write(32,'(a)') ' '
!    write(32,'(i6,a,2i4)') mype,'  <--  itargdn_sw_loc32 ',itargdn_sw_loc32,nsq
!    write(32,'(i6,a,2i4)') mype,'  <--  itargdn_se_loc32 ',itargdn_se_loc32,nsq
!    write(32,'(i6,a,2i4)') mype,'  <--  itargdn_nw_loc32 ',itargdn_nw_loc32,nsq
!    write(32,'(i6,a,2i4)') mype,'  <--  itargdn_ne_loc32 ',itargdn_ne_loc32,nsq

!  end do
!-----------------------------------------------------------------------
endsubroutine targdn32_loc

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine targdn43_loc(this)
!***********************************************************************
!                                                                      !
! Initialize downsending pararameters for application MGBF to          !
! localization from g4 go g3                                           !
!                                                                      !
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
integer(i_kind):: ix_t,jy_t
integer(i_kind):: ix_l,jy_l
integer(i_kind):: ix_sw,jy_sw
integer(i_kind):: ix_se,jy_se
integer(i_kind):: ix_nw,jy_nw
integer(i_kind):: ix_ne,jy_ne
integer(i_kind):: mx2,my2
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------

!    write(43,'(a)') 'mype     itargdn_xx_loc43  nsq43 '
!    write(43,'(a)') '---------------------------------'

!  do mype=0,nxm*nym-1

    jy_t = mype/nxm+1
    ix_t = mype-(jy_t-1)*nxm+1

    mx2 = mod(ix_t,2)
    my2 = mod(jy_t,2)

!
! Square 1
!
      if( mx2==1 .and. my2==1 ) then
        nsq43 = 1
        itargdn_sw_loc43 = mype
        itargdn_se_loc43 = mype+1
        itargdn_nw_loc43 = mype+nxm
        itargdn_ne_loc43 = mype+nxm+1
      else &
!
! Square 2
!
      if( mx2==0 .and. my2==1 ) then
        nsq43 = 2
        itargdn_sw_loc43 = mype-1
        itargdn_se_loc43 = mype
        itargdn_nw_loc43 = mype+nxm-1
        itargdn_ne_loc43 = mype+nxm
      else &
!
! Square 3
!
      if( mx2==1 .and. my2==0 ) then
        nsq43 = 3
        itargdn_sw_loc43 = mype-nxm
        itargdn_se_loc43 = mype-nxm+1
        itargdn_nw_loc43 = mype
        itargdn_ne_loc43 = mype+1
      else &
!
! Square 4
!
      if( mx2==0 .and. my2==0 ) then
        nsq43 = 4
        itargdn_sw_loc43 = mype-nxm-1
        itargdn_se_loc43 = mype-nxm
        itargdn_nw_loc43 = mype-1
        itargdn_ne_loc43 = mype
      endif

!    write(43,'(a)') ' '
!    write(43,'(i6,a,2i4)') mype,'  <--  itargdn_sw_loc43 ',itargdn_sw_loc43,nsq
!    write(43,'(i6,a,2i4)') mype,'  <--  itargdn_se_loc43 ',itargdn_se_loc43,nsq
!    write(43,'(i6,a,2i4)') mype,'  <--  itargdn_nw_loc43 ',itargdn_nw_loc43,nsq
!    write(43,'(i6,a,2i4)') mype,'  <--  itargdn_ne_loc43 ',itargdn_ne_loc43,nsq
!
!  end do
!-----------------------------------------------------------
endsubroutine targdn43_loc

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_domain_loc
