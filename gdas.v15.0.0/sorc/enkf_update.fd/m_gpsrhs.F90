module m_gpsrhs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_gpsrhs
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-22
!
! abstract: defines persistant workspace for multiple-pass setupbend()/setupref()
!
! program history log:
!   2010-03-22  j guo   - added this document block
!   2010-05-27  j guo   - derived from m_rhs.f90
!   2011-01-04  l cucurull - add nsig_ext in gpsrhs_alloc
!   2012-12-17  l cucurull - remove qcfail_stats_1 and qcfail_stats_2
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

!#define VERBOSE
#include "mytrace.H"

! module interface:

  use kinds, only: r_kind, i_kind, r_single
  use mpeu_util, only: die,perr,tell
  implicit none
  private

  !! GPS processing specific interfaces and variables

  public:: gpsrhs_alloc
  public:: gpsrhs_aliases
  public:: gpsrhs_unaliases
  public:: gpsrhs_dealloc

  public:: muse
  public:: termq
  public:: termpk
  public:: termt
  public:: termtk
  public:: termtl
  public:: termpl1
  public:: termpl2
  public:: pressure
  public:: dpresl
  public:: dbend_loc
  public:: xj
  public:: n_t
  public:: n_q
  public:: n_p
  public:: nrefges
  public:: rges
  public:: gp2gm
  public:: prsltmp_o
  public:: tges_o
  public:: error
  public:: error_adjst
  public:: ratio_errors
  public:: rdiagbuf
  public:: cdiagbuf

  public:: qcfail
  public:: qcfail_loc
  public:: qcfail_high
  public:: qcfail_gross

  public:: data_ier
  public:: data_igps
  public:: data_ihgt

! Revision history:

  type gpsrhs_buffer
    private
    logical:: alloced =.false.
    character(len=max(len('ref'),len('bend'))):: class =""

    logical         , pointer, dimension(  :):: muse => null()

    real(r_kind    ), pointer, dimension(  :):: error    => null()
    real(r_kind    ), pointer, dimension(  :):: error_adjst => null()
    real(r_kind    ), pointer, dimension(  :):: ratio_errors=> null()

       ! case: class=="ref"
    real(r_kind    ), pointer, dimension(  :):: termq  => null()
    real(r_kind    ), pointer, dimension(  :):: termpk => null()
    real(r_kind    ), pointer, dimension(  :):: termt  => null()
    real(r_kind    ), pointer, dimension(  :):: termtk => null()
    real(r_kind    ), pointer, dimension(:,:):: termtl => null()
    real(r_kind    ), pointer, dimension(:,:):: termpl1=> null()
    real(r_kind    ), pointer, dimension(:,:):: termpl2=> null()
    real(r_kind    ), pointer, dimension(  :):: pressure => null()
    real(r_kind    ), pointer, dimension(  :):: dpresl => null()

       ! case: class=="bend"
    real(r_kind    ), pointer, dimension(:,:):: dbend_loc => null()
    real(r_kind    ), pointer, dimension(:,:):: xj      => null()
    real(r_kind    ), pointer, dimension(:,:):: n_t     => null()
    real(r_kind    ), pointer, dimension(:,:):: n_q     => null()
    real(r_kind    ), pointer, dimension(:,:):: n_p     => null()
    real(r_kind    ), pointer, dimension(:,:):: nrefges => null()
    real(r_kind    ), pointer, dimension(:,:):: rges    => null()
    real(r_kind    ), pointer, dimension(:,:):: gp2gm   => null()
    real(r_kind    ), pointer, dimension(:,:):: prsltmp_o => null()
    real(r_kind    ), pointer, dimension(:,:):: tges_o  => null()

    real(r_kind    ), pointer, dimension(:,:):: rdiagbuf => null()
    character(len=8), pointer, dimension(  :):: cdiagbuf => null()

    logical         , pointer, dimension(  :):: qcfail => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_loc  => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_high => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_gross=> null()

    real(r_kind    ), pointer, dimension(  :):: data_ier  => null()
    real(r_kind    ), pointer, dimension(  :):: data_igps => null()
    real(r_kind    ), pointer, dimension(  :):: data_ihgt => null()
  end type gpsrhs_buffer

  type(gpsrhs_buffer),dimension(:),allocatable,target,save:: aGPSRHS_buffer

!! Aliases to all private components of the is-th element of aGPSRHS_buffer(1:ndat).
!! These aliases will let the uses of these variables unchanged in setupref()
!! and setupbend() routines.

  logical         , pointer, dimension(  :), save:: muse

  real(r_kind    ), pointer, dimension(  :), save:: error,error_adjst
  real(r_kind    ), pointer, dimension(  :), save:: ratio_errors

     ! case: class=='ref'
  real(r_kind    ), pointer, dimension(  :), save:: termq,termpk,termt,termtk
  real(r_kind    ), pointer, dimension(:,:), save:: termtl,termpl1,termpl2
  real(r_kind    ), pointer, dimension(  :), save:: dpresl, pressure

     ! case: class=='bend'
  real(r_kind    ), pointer, dimension(:,:), save:: dbend_loc,xj
  real(r_kind    ), pointer, dimension(:,:), save:: n_t,n_q,n_p,nrefges
  real(r_kind    ), pointer, dimension(:,:), save:: rges,gp2gm,prsltmp_o,tges_o

  real(r_kind    ), pointer, dimension(:,:), save:: rdiagbuf
  character(len=8), pointer, dimension(  :), save:: cdiagbuf

  logical         , pointer, dimension(  :), save:: qcfail
  real(r_single  ), pointer, dimension(  :), save:: qcfail_loc,qcfail_high,qcfail_gross

  real(r_kind    ), pointer, dimension(  :), save:: data_ier
  real(r_kind    ), pointer, dimension(  :), save:: data_igps
  real(r_kind    ), pointer, dimension(  :), save:: data_ihgt

  character(len=*),parameter:: myname="m_gpsrhs"

contains

subroutine gpsrhs_alloc(is,class,nobs,nsig,nreal,grids_dim,nsig_ext)
  use constants, only: zero
  use obsmod   , only: ndat
  implicit none
  integer(i_kind),intent(in) :: is
  character(len=*),intent(in) :: class
  integer(i_kind),intent(in) :: nobs
  integer(i_kind),intent(in) :: nsig
  integer(i_kind),intent(in) :: nreal
  integer(i_kind),intent(in) :: grids_dim
  integer(i_kind),intent(in) :: nsig_ext

  type(gpsrhs_buffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'_alloc'
_ENTRY_(myname_)

  if(.not.allocated(aGPSRHS_buffer)) then
    allocate(aGPSRHS_buffer(ndat))
    aGPSRHS_buffer(1:ndat)%alloced=.false.
  endif
  b=>aGPSRHS_buffer(is) ! b is aliased to an entry in aGPSRHS_buffer
  if(b%alloced) call die(myname_,'this object is already allocated')

  b%alloced =.true.
  b%class   = class

  allocate(b%muse(nobs))

  b%muse(:)=.false.

  select case(b%class)
  case('ref')

    allocate(b%termq  (     nobs))
    allocate(b%termpk (     nobs))
    allocate(b%termpl1(nsig,nobs))
    allocate(b%termpl2(nsig,nobs))

    b%termq  (  :)=zero
    b%termpk (  :)=zero
    b%termpl1(:,:)=zero
    b%termpl2(:,:)=zero

    allocate(b%termt  (     nobs))
    allocate(b%termtk (     nobs))
    allocate(b%termtl (nsig,nobs))

    b%termt (  :)=zero
    b%termtk(  :)=zero
    b%termtl(:,:)=zero

    allocate(b%pressure    (nobs))
    allocate(b%dpresl(nobs))

    b%pressure(:)=huge(b%pressure)
    b%dpresl  (:)=huge(b%dpresl  )

  case('bend')

    allocate(b%dbend_loc(grids_dim,nobs))
    allocate(b%xj       (grids_dim,nobs))

    b%dbend_loc(:,:) = HUGE(b%dbend_loc)
    b%xj(:,:)=HUGE(b%xj)

    allocate(b%n_t(nsig,nobs))
    allocate(b%n_q(nsig,nobs))
    allocate(b%n_p(nsig,nobs))
    allocate(b%nrefges(nsig+nsig_ext,nobs))

    b%n_t(:,:)=HUGE(b%n_t)
    b%n_q(:,:)=HUGE(b%n_q)
    b%n_p(:,:)=HUGE(b%n_p)
    b%nrefges(:,:)=HUGE(b%nrefges)

    allocate(b%rges     (nsig,nobs))
    allocate(b%gp2gm    (nsig,nobs))
    allocate(b%prsltmp_o(nsig,nobs))
    allocate(b%tges_o   (nsig,nobs))

    b%rges(:,:) = HUGE(b%rges)
    b%gp2gm(:,:) = HUGE(b%gp2gm)
    b%prsltmp_o(:,:) = HUGE(b%prsltmp_o)
    b%tges_o(:,:) = HUGE(b%tges_o)

  end select

  allocate(b%error       (nobs))
  allocate(b%error_adjst (nobs))
  allocate(b%ratio_errors(nobs))

  b%error       (:)=huge(b%error)
  b%error_adjst (:)=huge(b%error_adjst)
  b%ratio_errors(:)=huge(b%ratio_errors)

  allocate(b%rdiagbuf(nreal,nobs))
  allocate(b%cdiagbuf(      nobs))

  b%rdiagbuf(:,:)=huge(b%rdiagbuf)
  b%cdiagbuf(  :)=""

  allocate(b%qcfail        (nobs))
  allocate(b%qcfail_loc    (nobs))
  allocate(b%qcfail_high   (nobs))
  allocate(b%qcfail_gross  (nobs))

  b%qcfail=.false.
  b%qcfail_loc    =zero
  b%qcfail_high   =zero
  b%qcfail_gross  =zero

  allocate(b%data_ier (nobs))
  allocate(b%data_igps(nobs))
  allocate(b%data_ihgt(nobs))
  
  b%data_ier (:)=huge(b%data_ier)
  b%data_igps(:)=huge(b%data_igps)
  b%data_ihgt(:)=huge(b%data_ihgt)
_EXIT_(myname_)
end subroutine gpsrhs_alloc

subroutine gpsrhs_dealloc(is)
  implicit none
  integer(i_kind),intent(in) :: is

  type(gpsrhs_buffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'_dealloc'

_ENTRY_(myname_)
  if(.not.allocated(aGPSRHS_buffer)) &
    call die(myname_,'object not allocated, aGPSbuffer')

  if(is<lbound(aGPSRHS_buffer,1) .or. is>ubound(aGPSRHS_buffer,1)) then
    call perr(myname_,'out of aGPSRHS_buffer range, is =',is)
    call perr(myname_,'lbound(aGPSRHS_buffer) =',lbound(aGPSRHS_buffer,1))
    call perr(myname_,'ubound(aGPSRHS_buffer) =',ubound(aGPSRHS_buffer,1))
    call die(myname_)
  endif

  b=>aGPSRHS_buffer(is) ! b is aliased to an entry in gpsrhs_buffer
  if(.not.b%alloced) then
    call perr(myname_,'aGPSRHS_buffer(is) not allocated, is =',is)
    call die(myname_)
  endif

  deallocate(b%muse)

  select case(b%class)
  case('ref')
    deallocate(b%termq  )
    deallocate(b%termpk )
    deallocate(b%termpl1)
    deallocate(b%termpl2)

    deallocate(b%termt  )
    deallocate(b%termtk )
    deallocate(b%termtl )

    deallocate(b%pressure)
    deallocate(b%dpresl)

  case('bend')
    deallocate(b%dbend_loc)
    deallocate(b%xj)
    deallocate(b%n_t)
    deallocate(b%n_q)
    deallocate(b%n_p)
    deallocate(b%nrefges)
    deallocate(b%rges)
    deallocate(b%gp2gm)
    deallocate(b%prsltmp_o)
    deallocate(b%tges_o)

  end select

  deallocate(b%error       )
  deallocate(b%error_adjst )
  deallocate(b%ratio_errors)

  deallocate(b%rdiagbuf)
  deallocate(b%cdiagbuf)

  deallocate(b%qcfail        )
  deallocate(b%qcfail_loc    )
  deallocate(b%qcfail_high   )
  deallocate(b%qcfail_gross  )

  deallocate(b%data_ier )
  deallocate(b%data_igps)
  deallocate(b%data_ihgt)

  b%class=""
  b%alloced=.false.
_EXIT_(myname_)
end subroutine gpsrhs_dealloc

subroutine gpsrhs_aliases(is)
  implicit none
  integer(i_kind),intent(in) :: is

  type(gpsrhs_buffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'_aliases'
_ENTRY_(myname_)
  b=>aGPSRHS_buffer(is) ! b is aliased to an entry in gpsrhs_buffer

  muse          => b%muse

  select case(b%class)
  case('ref')
    termq       => b%termq
    termpk      => b%termpk
    termt       => b%termt
    termtk      => b%termtk
    termtl      => b%termtl
    termpl1     => b%termpl1
    termpl2     => b%termpl2
    pressure    => b%pressure
    dpresl      => b%dpresl

  case('bend')
    dbend_loc   => b%dbend_loc
    xj          => b%xj
    n_t         => b%n_t
    n_q         => b%n_q
    n_p         => b%n_p
    nrefges     => b%nrefges
    rges        => b%rges
    gp2gm       => b%gp2gm
    prsltmp_o   => b%prsltmp_o
    tges_o      => b%tges_o
  end select

  error         => b%error
  error_adjst   => b%error_adjst
  ratio_errors  => b%ratio_errors

  rdiagbuf      => b%rdiagbuf
  cdiagbuf      => b%cdiagbuf

  qcfail        => b%qcfail
  qcfail_loc    => b%qcfail_loc
  qcfail_high   => b%qcfail_high
  qcfail_gross  => b%qcfail_gross

  data_ier      => b%data_ier
  data_igps     => b%data_igps
  data_ihgt     => b%data_ihgt
_EXIT_(myname_)
end subroutine gpsrhs_aliases

subroutine gpsrhs_unaliases(is)
  implicit none
  character(len=*),parameter:: myname_=myname//'_unaliases'
  integer,intent(in):: is
  type(gpsrhs_buffer),pointer:: b

_ENTRY_(myname_)

  b=>aGPSRHS_buffer(is)
  if((.not.b%alloced) .or. (.not.associated(muse,b%muse))) then
    if(.not.b%alloced) call perr(myname_,'not allocated, aGPSRHS_buffer #',is)
    if(.not.associated(muse,b%muse)) call perr(myname_,'not associated to aGPSRHS_buffer #',is)
    call die(myname_)
  endif

  nullify(muse)
  nullify(termq)
  nullify(termpk,termpl1,termpl2)
  nullify(termt,termtk,termtl)
  nullify(pressure)
  nullify(dpresl)
  nullify(dbend_loc,xj)
  nullify(n_t,n_q,n_p,nrefges)
  nullify(rges,gp2gm,prsltmp_o,tges_o)
  nullify(error,error_adjst,ratio_errors)
  nullify(rdiagbuf,cdiagbuf)
  nullify(qcfail,qcfail_loc,qcfail_gross)
  nullify(qcfail_high)
  nullify(data_ier,data_igps,data_ihgt)
_EXIT_(myname_)
end subroutine gpsrhs_unaliases

end module m_gpsrhs
