module m_obsNodeTypeManager
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_obsNodeTypeManager
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2015-08-13
!
! abstract: obsNode type manager, as an enumerated type molder.
!
! program history log:
!   2015-08-13  j guo   - added this document block.
!   2016-05-18  j guo   - finished its initial polymorphic implementation,
!                         with total 33 obs-types.
!   2018-01-23  k apodaca - add a new observation type i.e. lightning (light)
!                           suitable for the GOES/GLM instrument
!
!   2024-04-07 k apodaca - add gnssrspdnode
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use m_psNode   , only:    psNode
  use m_tNode    , only:     tNode
  use m_wNode    , only:     wNode
  use m_qNode    , only:     qNode
  use m_spdNode  , only:   spdNode
  use m_rwNode   , only:    rwNode
  use m_dwNode   , only:    dwNode
  use m_sstNode  , only:   sstNode
  use m_pwNode   , only:    pwNode
  use m_pcpNode  , only:   pcpNode
  use m_ozNode   , only:    ozNode
  use m_o3lNode  , only:   o3lNode
  use m_gpsNode  , only:   gpsNode
  use m_radNode  , only:   radNode
  use m_tcpNode  , only:   tcpNode
  use m_lagNode  , only:   lagNode
  use m_colvkNode, only: colvkNode
  use m_aeroNode , only:  aeroNode
  use m_aerolNode, only: aerolNode
  use m_pm2_5Node, only: pm2_5Node
  use m_gustNode , only:  gustNode
  use m_visNode  , only:   visNode
  use m_pblhNode , only:  pblhNode

  use m_wspd10mNode, only: wspd10mNode
  use m_uwnd10mNode, only: uwnd10mNode
  use m_vwnd10mNode, only: vwnd10mNode
  
  use m_gnssrspdNode, only: gnssrspdNode

  use m_td2mNode , only:  td2mNode
  use m_mxtmNode , only:  mxtmNode
  use m_mitmNode , only:  mitmNode
  use m_pmslNode , only:  pmslNode
  use m_howvNode , only:  howvNode
  use m_tcamtNode, only: tcamtNode
  use m_lcbasNode, only: lcbasNode
  use m_pm10Node , only:  pm10Node
  use m_cldchNode, only: cldchNode

  use m_swcpNode , only:  swcpNode
  use m_lwcpNode , only:  lwcpNode

  use m_lightNode, only: lightNode
  use m_dbzNode  , only:   dbzNode
  use m_fedNode,   only:   fedNode

  use kinds, only: i_kind
  use m_obsNode, only: obsNode
  use mpeu_util, only: perr,die

  implicit none
  private	! except

  public:: obsNodeType_undef
  public:: obsNodeType_lbound
  public:: obsNodeType_ubound
  public:: obsNodeType_count

  public:: iobsNode_kind
  public:: iobsNode_ps
  public:: iobsNode_t
  public:: iobsNode_w
  public:: iobsNode_q
  public:: iobsNode_spd
  public:: iobsNode_rw
  public:: iobsNode_dw
  public:: iobsNode_sst
  public:: iobsNode_pw
  public:: iobsNode_pcp
  public:: iobsNode_oz
  public:: iobsNode_o3l
  public:: iobsNode_gps
  public:: iobsNode_rad
  public:: iobsNode_tcp
  public:: iobsNode_lag
  public:: iobsNode_colvk
  public:: iobsNode_aero
  public:: iobsNode_aerol
  public:: iobsNode_pm2_5
  public:: iobsNode_gust
  public:: iobsNode_vis
  public:: iobsNode_pblh
  public:: iobsNode_wspd10m
  public:: iobsNode_uwnd10m
  public:: iobsNode_vwnd10m
  public:: iobsNode_td2m
  public:: iobsNode_mxtm
  public:: iobsNode_mitm
  public:: iobsNode_pmsl
  public:: iobsNode_howv
  public:: iobsNode_tcamt
  public:: iobsNode_lcbas
  public:: iobsNode_pm10
  public:: iobsNode_cldch
  public:: iobsNode_swcp
  public:: iobsNode_lwcp
  public:: iobsNode_light
  public:: iobsNode_dbz
  public:: iobsNode_fed
  public:: iobsNode_gnssrspd

  public :: obsNode_typeMold
  public :: obsNode_typeIndex

        interface obsNode_typeMold; module procedure &
                index2vmold_, &
                vname2vmold_
        end interface
        interface obsNode_typeIndex; module procedure &
                vmold2index_, &
                vname2index_
        end interface

  type(psNode   ), target, save::    ps_mold
  type(tNode    ), target, save::     t_mold
  type(wNode    ), target, save::     w_mold
  type(qNode    ), target, save::     q_mold
  type(spdNode  ), target, save::   spd_mold
  type(rwNode   ), target, save::    rw_mold
  type(dwNode   ), target, save::    dw_mold
  type(sstNode  ), target, save::   sst_mold
  type(pwNode   ), target, save::    pw_mold
  type(pcpNode  ), target, save::   pcp_mold
  type(ozNode   ), target, save::    oz_mold
  type(o3lNode  ), target, save::   o3l_mold
  type(gpsNode  ), target, save::   gps_mold
  type(radNode  ), target, save::   rad_mold
  type(tcpNode  ), target, save::   tcp_mold
  type(lagNode  ), target, save::   lag_mold
  type(colvkNode), target, save:: colvk_mold
  type(aeroNode ), target, save::  aero_mold
  type(aerolNode), target, save:: aerol_mold
  type(pm2_5Node), target, save:: pm2_5_mold
  type(gustNode ), target, save::  gust_mold
  type(visNode  ), target, save::   vis_mold
  type(pblhNode ), target, save::  pblh_mold

  type(wspd10mNode), target, save:: wspd10m_mold
  type(uwnd10mNode), target, save:: uwnd10m_mold
  type(vwnd10mNode), target, save:: vwnd10m_mold
  
  type(gnssrspdNode), target, save:: gnssrspd_mold

  type(   td2mNode), target, save::    td2m_mold
  type(   mxtmNode), target, save::    mxtm_mold
  type(   mitmNode), target, save::    mitm_mold
  type(   pmslNode), target, save::    pmsl_mold
  type(   howvNode), target, save::    howv_mold
  type(  tcamtNode), target, save::   tcamt_mold
  type(  lcbasNode), target, save::   lcbas_mold
  type(   pm10Node), target, save::    pm10_mold
  type(  cldchNode), target, save::   cldch_mold

  type(   swcpNode), target, save::    swcp_mold
  type(   lwcpNode), target, save::    lwcp_mold
  type(  lightNode), target, save::   light_mold
  type(  dbzNode),   target, save::     dbz_mold
  type(  fedNode),   target, save::     fed_mold
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_obsNodeTypeManager'

! UseCase 1: configuration of a single mold
!
!   use m_obsNodeTypeManager, only: obsNode_typeMold
!   use m_psNode, only: i_psNode
!   ...
!   allocate(psLList%mold, source=obsNode_typeMold(i_psNode))
! or, for Fortran 2008 ALLOCATE() with MOLD= specifier
!   allocate(psLList%mold,   mold=obsNode_typeMold(i_psNode))
!
! UseCase 2: configuration of molds in an array
!
!   use m_obsLList, only: obsLList_moldConfig
!   use m_obsNodeTypeManager, only: obsNode_typeMold
!   ...
!   do jtype=lbound(obsdiags,2),ubound(obsdiags,2)
!     do ibin=lbound(obsdiags,1),ubound(obsdiags,1)
!       call obsLList_moldConfig(obsdiags(ibin,jtype),mold=obsNode_typeMold(jtype))
!     enddo
!   enddo
!

  enum, bind(C)
    enumerator:: iobsNode_zero_   = 0

    enumerator:: iobsNode_ps
    enumerator:: iobsNode_t
    enumerator:: iobsNode_w
    enumerator:: iobsNode_q
    enumerator:: iobsNode_spd
    enumerator:: iobsNode_gnssrspd
    enumerator:: iobsNode_rw
    enumerator:: iobsNode_dw
    enumerator:: iobsNode_sst
    enumerator:: iobsNode_pw
    enumerator:: iobsNode_pcp
    enumerator:: iobsNode_oz
    enumerator:: iobsNode_o3l
    enumerator:: iobsNode_gps
    enumerator:: iobsNode_rad
    enumerator:: iobsNode_tcp
    enumerator:: iobsNode_lag
    enumerator:: iobsNode_colvk
    enumerator:: iobsNode_aero
    enumerator:: iobsNode_aerol
    enumerator:: iobsNode_pm2_5
    enumerator:: iobsNode_gust
    enumerator:: iobsNode_vis
    enumerator:: iobsNode_pblh
    enumerator:: iobsNode_wspd10m
    enumerator:: iobsNode_uwnd10m
    enumerator:: iobsNode_vwnd10m
    enumerator:: iobsNode_td2m
    enumerator:: iobsNode_mxtm
    enumerator:: iobsNode_mitm
    enumerator:: iobsNode_pmsl
    enumerator:: iobsNode_howv
    enumerator:: iobsNode_tcamt
    enumerator:: iobsNode_lcbas
    enumerator:: iobsNode_pm10
    enumerator:: iobsNode_cldch
    enumerator:: iobsNode_swcp
    enumerator:: iobsNode_lwcp
    enumerator:: iobsNode_light
    enumerator:: iobsNode_dbz
    enumerator:: iobsNode_fed

    enumerator:: iobsNode_extra_
  end enum
  
  integer(i_kind),parameter:: iobsNode_kind = kind(iobsNode_zero_)

  integer(iobsNode_kind),parameter:: obsNodeType_undef  = -1_iobsNode_kind
  integer(iobsNode_kind),parameter:: obsNodeType_lbound = iobsNode_zero_ +1
  integer(iobsNode_kind),parameter:: obsNodeType_ubound = iobsNode_extra_-1
  integer(iobsNode_kind),parameter:: obsNodeType_count  = obsNodeType_ubound-obsNodeType_lbound+1

contains
function vname2index_(vname) result(index_)
  use mpeu_util, only: lowercase
  implicit none
  integer(i_kind):: index_
  character(len=*),intent(in):: vname
  character(len=len(vname)):: vname_
  vname_=lowercase(vname)

  index_=0      ! a default return value, if the given name is unknown.
  select case(vname_)
  case("ps"   ,   "[psnode]"); index_ = iobsNode_ps
  case("t"    ,    "[tnode]"); index_ = iobsNode_t
  case("w"    ,    "[wnode]"); index_ = iobsNode_w
  case("q"    ,    "[qnode]"); index_ = iobsNode_q
  case("spd"  ,  "[spdnode]"); index_ = iobsNode_spd
  case("rw"   ,   "[rwnode]"); index_ = iobsNode_rw
  case("dw"   ,   "[dwnode]"); index_ = iobsNode_dw
  case("sst"  ,  "[sstnode]"); index_ = iobsNode_sst
  case("pw"   ,   "[pwnode]"); index_ = iobsNode_pw
  case("pcp"  ,  "[pcpnode]"); index_ = iobsNode_pcp
  case("oz"   ,   "[oznode]"); index_ = iobsNode_oz
  case("o3l"  ,  "[o3lnode]"); index_ = iobsNode_o3l
  case("gps"  ,  "[gpsnode]"); index_ = iobsNode_gps
  case("rad"  ,  "[radnode]"); index_ = iobsNode_rad
  case("tcp"  ,  "[tcpnode]"); index_ = iobsNode_tcp
  case("lag"  ,  "[lagnode]"); index_ = iobsNode_lag
  case("colvk","[colvknode]"); index_ = iobsNode_colvk
  case("aero" , "[aeronode]"); index_ = iobsNode_aero
  case("aerol","[aerolnode]"); index_ = iobsNode_aerol
  case("pm2_5","[pm2_5node]"); index_ = iobsNode_pm2_5
  case("gust" , "[gustnode]"); index_ = iobsNode_gust
  case("vis"  ,  "[visnode]"); index_ = iobsNode_vis
  case("pblh" , "[pblhnode]"); index_ = iobsNode_pblh

  case("wspd10m", &
             "[wspd10mnode]"); index_ = iobsNode_wspd10m
  case("uwnd10m", &
             "[uwnd10mnode]"); index_ = iobsNode_uwnd10m
  case("vwnd10m", &
             "[vwnd10mnode]"); index_ = iobsNode_vwnd10m
  case("gnssrspd", &
             "[gnssrspdnode]"); index_ = iobsNode_gnssrspd

  case("td2m" , "[td2mnode]"); index_ = iobsNode_td2m
  case("mxtm" , "[mxtmnode]"); index_ = iobsNode_mxtm
  case("mitm" , "[mitmnode]"); index_ = iobsNode_mitm
  case("pmsl" , "[pmslnode]"); index_ = iobsNode_pmsl
  case("howv" , "[howvnode]"); index_ = iobsNode_howv
  case("tcamt","[tcamtnode]"); index_ = iobsNode_tcamt
  case("lcbas","[lcbasnode]"); index_ = iobsNode_lcbas

  case("pm10" , "[pm10node]"); index_ = iobsNode_pm10
  case("cldch","[cldchnode]"); index_ = iobsNode_cldch

  case("swcp" , "[swcpnode]"); index_ = iobsNode_swcp
  case("lwcp" , "[lwcpnode]"); index_ = iobsNode_lwcp

  case("light","[lightnode]"); index_ = iobsNode_light
  case("dbz"  ,  "[dbznode]"); index_ = iobsNode_dbz
  case("fed"  ,  "[fednode]"); index_ = iobsNode_fed

  end select
end function vname2index_

function vmold2index_(mold) result(index_)
  implicit none
  integer(i_kind):: index_
  class(obsNode),target,intent(in):: mold

  index_=vname2index_(mold%mytype())
end function vmold2index_

function vmold2index_select_(mold) result(index_)
  implicit none
  integer(i_kind):: index_
  class(obsNode),target,intent(in):: mold

  index_=0
  select type(mold)
  type is(   psNode); index_ = iobsNode_ps
  type is(    tNode); index_ = iobsNode_t
  type is(    wNode); index_ = iobsNode_w
  type is(    qNode); index_ = iobsNode_q
  type is(  spdNode); index_ = iobsNode_spd
  type is(   rwNode); index_ = iobsNode_rw
  type is(   dwNode); index_ = iobsNode_dw
  type is(  sstNode); index_ = iobsNode_sst
  type is(   pwNode); index_ = iobsNode_pw
  type is(  pcpNode); index_ = iobsNode_pcp
  type is(   ozNode); index_ = iobsNode_oz
  type is(  o3lNode); index_ = iobsNode_o3l
  type is(  gpsNode); index_ = iobsNode_gps
  type is(  radNode); index_ = iobsNode_rad
  type is(  tcpNode); index_ = iobsNode_tcp
  type is(  lagNode); index_ = iobsNode_lag
  type is(colvkNode); index_ = iobsNode_colvk
  type is( aeroNode); index_ = iobsNode_aero
  type is(aerolNode); index_ = iobsNode_aerol
  type is(pm2_5Node); index_ = iobsNode_pm2_5
  type is( gustNode); index_ = iobsNode_gust
  type is(  visNode); index_ = iobsNode_vis
  type is( pblhNode); index_ = iobsNode_pblh

  type is(wspd10mNode); index_ = iobsNode_wspd10m
  type is(uwnd10mNode); index_ = iobsNode_uwnd10m
  type is(vwnd10mNode); index_ = iobsNode_vwnd10m
  
  type is(gnssrspdNode); index_ = iobsNode_gnssrspd

  type is( td2mNode); index_ = iobsNode_td2m
  type is( mxtmNode); index_ = iobsNode_mxtm
  type is( mitmNode); index_ = iobsNode_mitm
  type is( pmslNode); index_ = iobsNode_pmsl
  type is( howvNode); index_ = iobsNode_howv
  type is(tcamtNode); index_ = iobsNode_tcamt
  type is(lcbasNode); index_ = iobsNode_lcbas

  type is( pm10Node); index_ = iobsNode_pm10
  type is(cldchNode); index_ = iobsNode_cldch

  type is( swcpNode); index_ = iobsNode_swcp
  type is( lwcpNode); index_ = iobsNode_lwcp

  type is(lightNode); index_ = iobsNode_light
  type is(  dbzNode); index_ = iobsNode_dbz
  type is(  fedNode); index_ = iobsNode_fed

  end select
end function vmold2index_select_

function index2vmold_(i_obType) result(obsmold_)
  implicit none
  class(obsNode),pointer:: obsmold_
  integer(kind=i_kind),intent(in):: i_obType

  character(len=*),parameter:: myname_=myname//"::index2vmold_"

  obsmold_ => null()
  select case(i_obType)
  case(iobsNode_ps   ); obsmold_ =>    ps_mold
  case(iobsNode_t    ); obsmold_ =>     t_mold
  case(iobsNode_w    ); obsmold_ =>     w_mold
  case(iobsNode_q    ); obsmold_ =>     q_mold
  case(iobsNode_spd  ); obsmold_ =>   spd_mold
  case(iobsNode_rw   ); obsmold_ =>    rw_mold
  case(iobsNode_dw   ); obsmold_ =>    dw_mold
  case(iobsNode_sst  ); obsmold_ =>   sst_mold
  case(iobsNode_pw   ); obsmold_ =>    pw_mold
  case(iobsNode_pcp  ); obsmold_ =>   pcp_mold
  case(iobsNode_oz   ); obsmold_ =>    oz_mold
  case(iobsNode_o3l  ); obsmold_ =>   o3l_mold
  case(iobsNode_gps  ); obsmold_ =>   gps_mold
  case(iobsNode_rad  ); obsmold_ =>   rad_mold
  case(iobsNode_tcp  ); obsmold_ =>   tcp_mold
  case(iobsNode_lag  ); obsmold_ =>   lag_mold
  case(iobsNode_colvk); obsmold_ => colvk_mold
  case(iobsNode_aero ); obsmold_ =>  aero_mold
  case(iobsNode_aerol); obsmold_ => aerol_mold
  case(iobsNode_pm2_5); obsmold_ => pm2_5_mold
  case(iobsNode_gust ); obsmold_ =>  gust_mold
  case(iobsNode_vis  ); obsmold_ =>   vis_mold
  case(iobsNode_pblh ); obsmold_ =>  pblh_mold

  case(iobsNode_wspd10m); obsmold_ => wspd10m_mold
  case(iobsNode_uwnd10m); obsmold_ => uwnd10m_mold
  case(iobsNode_vwnd10m); obsmold_ => vwnd10m_mold
  
  case(iobsNode_gnssrspd  ); obsmold_ => gnssrspd_mold

  case(iobsNode_td2m ); obsmold_ =>    td2m_mold
  case(iobsNode_mxtm ); obsmold_ =>    mxtm_mold
  case(iobsNode_mitm ); obsmold_ =>    mitm_mold
  case(iobsNode_pmsl ); obsmold_ =>    pmsl_mold
  case(iobsNode_howv ); obsmold_ =>    howv_mold
  case(iobsNode_tcamt); obsmold_ =>   tcamt_mold
  case(iobsNode_lcbas); obsmold_ =>   lcbas_mold

  case(iobsNode_pm10 ); obsmold_ =>    pm10_mold
  case(iobsNode_cldch); obsmold_ =>   cldch_mold

  case(iobsNode_swcp ); obsmold_ =>    swcp_mold
  case(iobsNode_lwcp ); obsmold_ =>    lwcp_mold

  case(iobsNode_light); obsmold_ =>   light_mold
  case(iobsNode_dbz);   obsmold_ =>     dbz_mold
  case(iobsNode_fed);   obsmold_ =>     fed_mold

  end select
end function index2vmold_

function vname2vmold_(vname) result(obsmold_)
  implicit none
  class(obsNode),pointer:: obsmold_
  character(len=*),intent(in):: vname

  character(len=*),parameter:: myname_=myname//"::vname2vmold_"
  integer(kind=i_kind):: i_obType

  i_obType=vname2index_(vname)
  obsmold_ => index2vmold_(i_obType)
end function vname2vmold_

end module m_obsNodeTypeManager
