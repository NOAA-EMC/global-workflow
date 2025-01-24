module gsi_tOper
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module gsi_tOper
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2018-08-10
!
! abstract: an obOper extension for tNode type
!
! program history log:
!   2018-08-10  j guo   - added this document block
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

  use gsi_obOper, only: obOper
  use m_tNode   , only: tNode
  implicit none
  public:: tOper      ! data stracture

  type,extends(obOper):: tOper
  contains
    procedure,nopass:: mytype
    procedure,nopass:: nodeMold
    procedure:: setup_
    procedure:: intjo1_
    procedure:: stpjo1_
  end type tOper

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='gsi_tOper'
  type(tNode),save,target:: myNodeMold_

contains
  function mytype(nodetype)
    implicit none
    character(len=:),allocatable:: mytype
    logical,optional, intent(in):: nodetype
    mytype="[tOper]"
    if(present(nodetype)) then
      if(nodetype) mytype=myNodeMold_%mytype()
    endif
  end function mytype

  function nodeMold()
  !> %nodeMold() returns a mold of its corresponding obsNode
    use m_obsNode, only: obsNode
    implicit none
    class(obsNode),pointer:: nodeMold
    nodeMold => myNodeMold_
  end function nodeMold

  subroutine setup_(self, lunin, mype, is, nobs, init_pass,last_pass)
    use t_setup, only: setup
    use kinds, only: i_kind
    use gsi_obOper, only: len_obstype
    use gsi_obOper, only: len_isis

    use m_rhs  , only: awork => rhs_awork
    use m_rhs  , only: bwork => rhs_bwork
    use m_rhs  , only: iwork => i_t

    use obsmod  , only: write_diag
    use convinfo, only: diag_conv
    use jfunc   , only: jiter

    use mpeu_util, only: die
    implicit none
    class(tOper ), intent(inout):: self
    integer(i_kind), intent(in):: lunin
    integer(i_kind), intent(in):: mype
    integer(i_kind), intent(in):: is
    integer(i_kind), intent(in):: nobs
    logical        , intent(in):: init_pass     ! supporting multi-pass setup()
    logical        , intent(in):: last_pass     ! with incremental backgrounds.

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::setup_"

    character(len=len_obstype):: obstype
    character(len=len_isis   ):: isis
    integer(i_kind):: nreal,nchanl,ier,nele
    logical:: diagsave

    if(nobs == 0) return

    read(lunin,iostat=ier) obstype,isis,nreal,nchanl
    if(ier/=0) call die(myname_,'read(obstype,...), iostat =',ier)
    nele = nreal+nchanl

    diagsave  = write_diag(jiter) .and. diag_conv

    call setup(self%obsLL(:), self%odiagLL(:), &
        lunin,mype,bwork,awork(:,iwork),nele,nobs,is,diagsave)

  end subroutine setup_

  subroutine intjo1_(self, ibin, rval,sval, qpred,sbias)
    use inttmod, only: intjo => intt
    use gsi_bundlemod  , only: gsi_bundle
    use bias_predictors, only: predictors
    use bias_predictors, only: predictors_getdim
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds     , only: i_kind, r_quad
    implicit none
    class(tOper  ),intent(in   ):: self
    integer(i_kind ),intent(in   ):: ibin
    type(gsi_bundle),intent(inout):: rval   ! (ibin)
    type(gsi_bundle),intent(in   ):: sval   ! (ibin)
    real(r_quad    ),target,dimension(:),intent(inout):: qpred  ! (ibin)
    type(predictors),target,             intent(in   ):: sbias

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::intjo1_"
    integer(i_kind):: i,l,n
    class(obsNode),pointer:: headNode

! Are the different calls to intt() with optional arguments realy needed? 
! There is no checking of present(rpred) or present(spred) inside intt_()
! anyway.  Other logic is used to avoid accessing non-present rpred(:) and
! spred(:).

    call predictors_getdim(lbnd_t=i,ubnd_t=l,size_t=n)
    headNode => obsLList_headNode(self%obsLL(ibin))
    if(n>0) then
      call intjo(headNode, rval,sval, qpred(i:l),sbias%predt)
    else
      call intjo(headNode, rval,sval)
    endif
    headNode => null()

  end subroutine intjo1_

  subroutine stpjo1_(self, ibin, dval,xval,pbcjo,sges,nstep,dbias,xbias)
    use stptmod, only: stpjo => stpt
    use gsi_bundlemod, only: gsi_bundle
    use bias_predictors, only: predictors, predictors_getdim
    use aircraftinfo, only: npredt,ntail,aircraft_t_bc_pof,aircraft_t_bc
    use m_obsNode , only: obsNode
    use m_obsLList, only: obsLList_headNode
    use kinds, only: r_quad,r_kind,i_kind
    implicit none
    class(tOper  ),intent(in):: self
    integer(i_kind ),intent(in):: ibin
    type(gsi_bundle),intent(in):: dval
    type(gsi_bundle),intent(in):: xval
    real(r_quad    ),dimension(:),intent(inout):: pbcjo ! (1:4)
    real(r_kind    ),dimension(:),intent(in   ):: sges
    integer(i_kind),intent(in):: nstep

    type(predictors),target, intent(in):: dbias
    type(predictors),target, intent(in):: xbias

    !----------------------------------------
    character(len=*),parameter:: myname_=myname//"::stpjo1_"
    class(obsNode),pointer:: headNode
    real(r_kind),pointer,dimension(:,:) :: dpred,xpred
    integer(i_kind):: n

! Are the different calls to stpt() with optional arguments realy needed? 
! There is no checking of present(rpred) or present(spred) inside intt_()
! anyway.  Other logic is used to avoid accessing non-present rpred(:) and
! spred(:).

    headNode => obsLList_headNode(self%obsLL(ibin))
    call predictors_getdim(size_t=n)
    if(n<=0 .or. .not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
      call stpjo(headNode,dval,xval,pbcjo(:),sges,nstep)
    else
      dpred(1:npredt,1:ntail) => dbias%predt(1:n)
      xpred(1:npredt,1:ntail) => xbias%predt(1:n)
      call stpjo(headNode,dval,xval,pbcjo(:),sges,nstep,dpred,xpred)
      dpred => null()
      xpred => null()
    endif
    headNode => null()
  end subroutine stpjo1_

end module gsi_tOper
