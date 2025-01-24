!BOI

!  !TITLE: GSI\_MetGuess\_Mod: A GSI Bundle to handle Guess Fields

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: 29 Apr 2011

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This module defines the so-called GSI\_MetGuess\_Bundle. Its main purpose is to
allow GSI to ingest guess fields other than those pre-set in guess\_grids,
refered to here as Meteorological Guess.
Eventually it would be nice to see all guess fields defined via the present module, and
a complete revamp of guess\_grids. This is aimed at for example extending the
current ability to run GSI for say analyzing a single field, such as Ozone, and
only have to bring in the necessary background fields, such Ozone it self when 
temperature interdependencies are neglected.

\begin{center}
\fbox{MetGuess Bundle is a way to ingest Meterological Guess (background fields) into GSI}
\end{center}

Before the introduction of this module, all guess fields entered GSI through the 
and the wired-in arrays ges\_x, with x standing for particular fields, defined in 
the guess\_grids module, e.g., ges\_u, ges\_tv, and so on. This becomes cumbersome the
more one wants to add new features to GSI. Chemistry-related fields -- aerosols and trace gases -- are
already handled separately from guess\_grids using GSI\_ChemBundle. The present
modules extends this capability to any new guess field.

\begin{center}
\fbox{MetGuess\_Bundle is a GSI\_Bundle}
\end{center}

The GSI\_MetGuess\_Bundle uses the GSI\_Bundle. But while the state and control vectors 
use the GSI\_Bundle to associate fields used by the observation operator and
those used in the cost function, respectively, the GSI\_MetGuess\_Bundle
is simply aimed at allowing gather together Guess fields in a flexible way. Just
as with the bundle, all parallel distribution must have already been done before 
filling in the fields in the bundle, that is, GSI\_MetGuess\_Bundle does not 
handle distribution.

As guess\_grids does, this module still treats the Meteorological Guess fields
as in a common-block-like structure. That is, the GSI\_Bundle defined here to hold the
Meteorological Guess fields is an internally defined type that cannot be passed
around; cannot be instanciated. This will change in the future, but for the time being 
this is the simplest thing to do given the current code design. This is
identical to what is done in defining the GSI\_ChemBundle.

\begin{center}
\fbox{MetGuess Bundle Module provides an (almost) opaque access to its entries}
\end{center}

One of the ideas behind this module is that it defines an opaque-like object.
That is, functions related to contents of the MetGuess Bundle should only be
extracted via inquires using a ``get-like'' procedures. This is why, only ``methods'' are 
made public to this module, that is, 

\begin{verbatim}
public :: gsi_metguess_create_grids
public :: gsi_metguess_destroy_grids
public :: gsi_metguess_init
public :: gsi_metguess_get
public :: gsi_metguess_final
\end{verbatim}

and never the variables themselves; the only exception being the GSI\_MetGuess\_Bundle itself 
(until it is no longer treated as a common-block).  Some of the public methods above are 
overloaded and all have internal interfaces (name of which appears in the index of this protex 
document. It should be a rule here that any new routine to be made public should
have a declared interface procedure.

\begin{center}
\fbox{MetGuess\_Bundle is defined via the {\it met\_guess} table in a resource file}
\end{center}

\underline{Defining the MetGuess\_Bundle} is done via the table {\it met\_guess}, usually 
embedded in the {\it anavinfo} file. An example of such table follows:
\begin{verbatim}
met_guess::
!#var     level  crtm_use   desc                 user_name
  ql       72       -1      cloud_liquid_mixr    QLTOT
  qi       72       -1      cloud_ice_mixr       QITOT
::
\end{verbatim}

As usual, this table follows INPAK/ESMF convention, begining with a name
(met\_guess), followed by double colons (::), and ending with double colons.
Any line starting with an exclamation mark or a pound sign is taken as a comment.

The current {\it met\_guess} table has five columns defined as follows:

\begin{verbatim}
Column 1: variable name - refers to internally known GSI variable name
Column 2: indicates number of levels (used to distinguish between 2d and 3d fields)
Column 3: indicates whether variable is to be passed to CRTM or not according to 
          the following scheme:
          if<0    general variable; not used in CRTM 
          if=0    general variable; use prescribed global mean data to affect CRTM
          if=1    general variable; use variable in guess field to affect CRTM 
Column 4: description of variable (defined by user)
Column 5: user-defined variable name associated with name read in from file
\end{verbatim}

\begin{center}
\fbox{Examples of extracting information related to the MetGuess\_Bundle}
\end{center}

\underline{Examples} of accessing information related to fields in the MetGuess\_Bundle.
\begin{enumerate}
\item Say a routine wants to whether or not the variable ``cw'' is in
      MetGuess\_Bundle. This can be done simply with the call
      \begin{verbatim}
      call gsi_metguess_get ( 'var::cw', ivar, ier )
      \end{verbatim}
      if ivar is grater than zero, the variable is present in the bundle.
\item Say a routine wants to know how $qi$ is to be used in CRTM. 
      This is done via the {\it i4crtm::} tag, as in:
      \begin{verbatim}
      call gsi_metguess_get ( 'i4crtm::qi', iqi, ier )
      \end{verbatim}
       where the value returned {\it iqi} is an integer following the 
       scheme laid out for entries in column 3 of the resource file (anavinfo). 

\item Say a routine wants to get the number of all 3d cloud fields in the
      MetGuess\_Bundle, this can use the tag {\it clouds::3d}, as in:
      \begin{verbatim}
      call gsi_metguess_get ( 'clouds::3d',n,ier )
      \end{verbatim}
      notice this uses the same interface as in the example above, but returns
      information about something else.

\item Say a routine wants the name of all 3d met-fields
      \begin{verbatim}
      call gsi_metguess_get ('clouds::3d',met3dnames,ier)     
      \end{verbatim}
      now the returned variable {\it met3dnames} is a character array with the 
      names of all 3d-met-guess. Notice it is important to inquire before hand
      about the number of 3d-met-guess fields available and to properly allocate space
      for the character arrays {\it met3dnames}, and only then make the call above.

\end{enumerate}

Other possible mnemonics known by this package can be found in the 
prologue description of the {it get} routines.

\begin{center}
\fbox{Conventions and Remarks}
\end{center}

\underline{Conventions} proposed for entries in this module:
\begin{itemize}
  \item GSI-known variable names should all be lower-case, these are the variables
        defined in column 1 of {\it anavinfo} file.
  \item Only methods should be made public from this module.
  \item New routines should be defined via interface procedure.
\end{itemize}
 
A general remark about the correct {\it met\_guess} table: it is recognized that
the format for general specification related to specific entries in the table is 
not general enough. A better approach is the one used by the Registry used in
GEOS-5 GOCART where a table exists to control a particular functionality
applicable to a certain set of constituents. For example, use of a variable in
CRTM could be control instead by a specific table listing constituents to be
used in the CRTM and at what extent, for example, a table of the form:
\begin{verbatim}
use_in_crtm::
 !var     use  
  qr       1
  ql       0
::
\end{verbatim}
Something of this form should eventually replace some of the columns in the 
{\it met\_guess} table. 

#endif
!EOI


!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP
!  
! !MODULE: GSI_MetGuess_Mod -- Implements Meteorological Guess for GSI
!
! !INTERFACE:

module gsi_metguess_mod
!
! !DESCRIPTION: Module to handle meteorological guess fields
!               This still uses wired-in type arrays to hold the guess. Soon
!               we'll generalize this.
!
! !REMARKS:
!   1. VERY IMPORTANT: No object from this file is to be make
!                      explicitly available to the outside world.
!                      Each object must be opaque with a get and
!                      a put method associated with it.
!   2. This is still functioning as a common-block when it comes
!      to the met type itself - needs some work to make it into 
!      a self-contained type
! 
! !USES:

use kinds, only: i_kind,r_kind
use constants, only: max_varname_length
use mpimod, only : mype
use mpeu_util,only: die
use file_utility, only : get_lun
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy

use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex

implicit none
private
save

! ! !PUBLIC MEMBER FUNCTIONS:

public :: gsi_metguess_create_grids
public :: gsi_metguess_destroy_grids
public :: gsi_metguess_init
public :: gsi_metguess_get
public :: gsi_metguess_final

public :: GSI_MetGuess_Bundle   ! still a common for now, ultimately should 
                                ! be a dynamic "type", passed around in arg list

! !INTERFACE:

interface gsi_metguess_init
          module procedure init_
end interface
interface gsi_metguess_final
          module procedure final_ 
end interface
interface gsi_metguess_create_grids
          module procedure create_
end interface
interface gsi_metguess_destroy_grids
          module procedure destroy_
end interface
interface gsi_metguess_get
          module procedure get_int0d_
          module procedure get_int1d_
          module procedure get_char0d_
          module procedure get_char1d_
end interface

type(GSI_Bundle),pointer :: GSI_MetGuess_Bundle(:)   ! still a common block for now


! !REVISION HISTORY:
!
!   29Apr2011 Todling  Initial code.
!
!EOP
!-------------------------------------------------------------------------

! !PRIVATE ROUTINES:
!BOC

integer(i_kind),parameter::MAXSTR=max_varname_length
logical:: guess_grid_initialized_=.false.
logical:: guess_initialized_=.false.
character(len=*), parameter :: myname = 'gsi_metguess_mod'

integer(i_kind) :: nbundles=-1
integer(i_kind) :: nmguess=0
integer(i_kind) :: ncloud=0
integer(i_kind) :: n2dcloud=0
integer(i_kind) :: n3dcloud=0
integer(i_kind) :: ng3d=-1
integer(i_kind) :: ng2d=-1
character(len=MAXSTR),allocatable :: mguess(:)    ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: mguess3d(:)   ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: mguess2d(:)   ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: metstype(:)   ! indicate type of meteorological field
character(len=MAXSTR),allocatable :: metsty3d(:)   ! indicate 3d type of met-guess
character(len=MAXSTR),allocatable :: metsty2d(:)   ! indicate 3d type of met-guess
character(len=MAXSTR),allocatable :: usrname2d(:)  ! user-defined 2d field names
character(len=MAXSTR),allocatable :: usrname3d(:)  ! user-defined 3d field names
character(len=MAXSTR),allocatable :: usrname(:)    ! user-defined field names
integer(i_kind),allocatable,dimension(:) :: i4crtm ! controls use of gas in CRTM:
                                                   ! < 0 don't use in CRTM
                                                   ! = 0 use predefined global mean
                                                   ! = 1 use gfs yearly global annual mean historical value
                                                   ! = 2 use gfs 3d background field
                                                   ! in interval [10,20) indicates cloud
integer(i_kind),allocatable,dimension(:) :: i4crtm2d ! as above but for 2d fields only
integer(i_kind),allocatable,dimension(:) :: i4crtm3d ! as above but for 3d fields only
integer(i_kind),allocatable,dimension(:) :: levels   ! levels of all variables
integer(i_kind),allocatable,dimension(:) :: levels3d ! levels of all 3d variables
integer(i_kind),allocatable,dimension(:) :: levels2d ! levels of all 2d variables

logical:: verbose_=.true.

contains

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_ --- Initialize MetGuess Bundle (read resource table); alloc internal
!
! !INTERFACE:
!
subroutine init_ (iamroot)
! USES:
implicit none
! !INPUT PARAMETER:
   logical,optional,intent(in) :: iamroot 
! !DESCRIPTION: Define contents of Meteorological Guess Bundle through rc 
!               file (typilcally embedded in anavinfo text file.
!
! !REVISION HISTORY:
!   2011-04-29  todling  initial code
!   2013-09-30  todling  allow 40-char var description
!   2014-02-03  todling  negative level entry in table means rank-3 array
!                         
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2011-04-29
!
!EOP
!-------------------------------------------------------------------------
!BOC
!character(len=*),parameter:: rcname='anavinfo.txt'
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='met_guess::'
integer(i_kind) luin,i,ii,ntot,icrtmuse
character(len=256),allocatable,dimension(:):: utable
character(len=20) ctype
character(len=20) var,oname
character(len=*),parameter::myname_=myname//'*init_'
integer(i_kind) ilev
logical iamroot_

if(guess_initialized_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
luin=get_lun()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nmguess)
if(nmguess<=0) then
   close(luin)
   return
endif

! Get contents of table
allocate(utable(nmguess))
call gettable(tbname,luin,ntot,nmguess,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
ng3d=0; ng2d=0
do ii=1,nmguess
   read(utable(ii),*) var, ilev, icrtmuse
   if(ilev==1) then
      ng2d=ng2d+1
   else
      ng3d=ng3d+1
   endif
enddo

if(ng3d > 0)then
   allocate(mguess3d(ng3d), &
            metsty3d(ng3d), &
            i4crtm3d(ng3d), &
            levels3d(ng3d), &
            usrname3d(ng3d))
end if
if(ng2d > 0)then
   allocate(mguess2d(ng2d), &
            metsty2d(ng2d), &
            i4crtm2d(ng2d), &
            levels2d(ng2d), &
            usrname2d(ng2d))
end if

   allocate(levels(nmguess),i4crtm(nmguess),usrname(nmguess),&
         mguess(nmguess),metstype(nmguess))

! Now load information from table
ng3d=0;ng2d=0
do ii=1,nmguess
   read(utable(ii),*) var, ilev, icrtmuse, ctype, oname
   if(ilev==1) then
      ng2d=ng2d+1
      mguess2d(ng2d)=trim(adjustl(var))
      metsty2d(ng2d)=trim(adjustl(ctype))
      i4crtm2d(ng2d)=icrtmuse
      levels2d(ng2d)=ilev
      usrname2d(ng2d)=trim(adjustl(oname))
      if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) n2dcloud=n2dcloud+1 ! convention, for now
   else
      ng3d=ng3d+1
      mguess3d(ng3d)=trim(adjustl(var))
      metsty3d(ng3d)=trim(adjustl(ctype))
      i4crtm3d(ng3d)=icrtmuse
      levels3d(ng3d)=abs(ilev)
      usrname3d(ng3d)=trim(adjustl(oname))
      if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) n3dcloud=n3dcloud+1 ! convention, for now
   endif
   if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) ncloud=ncloud+1 ! convention, for now
enddo

deallocate(utable)

! Fill in array w/ all var names (must be 3d first, then 2d)
ii=0
do i=1,ng3d
   ii=ii+1
   mguess(ii)=mguess3d(i)
   levels(ii)=levels3d(i)
   i4crtm(ii)=i4crtm3d(i)
   usrname(ii)=usrname3d(i)
   metstype(ii)=trim(adjustl(metsty3d(i)))
enddo
do i=1,ng2d
   ii=ii+1
   mguess(ii)=mguess2d(i)
   levels(ii)=levels2d(i)
   i4crtm(ii)=i4crtm2d(i)
   usrname(ii)=usrname2d(i)
   metstype(ii)=trim(adjustl(metsty2d(i)))
enddo

if (iamroot_) then
    write(6,*) myname_,':  2D-MET STATE VARIABLES: '
    do i=1,ng2d
       write(6,*) trim(mguess2d(i))
    enddo
    write(6,*) myname_,':  3D-MET STATE VARIABLES:'
    do i=1,ng3d
       write(6,*) trim(mguess3d(i))
    enddo
    write(6,*) myname_,': ALL MET STATE VARIABLES:'
    do i=1,nmguess
       write(6,*) trim(mguess(i))
    enddo
end if
guess_initialized_=.true.

end subroutine init_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  final_ --- Deallocate internal MetGuess Bundle info arrays
!
! !INTERFACE:
!
subroutine final_
implicit none

! !DESCRIPTION: Dealloc grids holding trace gases
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC
if(.not.guess_initialized_) return

if(allocated(mguess3d)) deallocate(mguess3d)
if(allocated(mguess2d)) deallocate(mguess2d)
if(allocated(metsty3d)) deallocate(metsty3d)
if(allocated(metsty2d)) deallocate(metsty2d) 
if(allocated(i4crtm3d)) deallocate(i4crtm3d)
if(allocated(i4crtm2d)) deallocate(i4crtm2d)
if(allocated(levels3d)) deallocate(levels2d)
if(allocated(levels))   deallocate(levels)
if(allocated(i4crtm))   deallocate(i4crtm)
if(allocated(usrname3d))deallocate(usrname3d)
if(allocated(usrname2d))deallocate(usrname2d)
if(allocated(usrname))  deallocate(usrname)
if(allocated(mguess))   deallocate(mguess)
if(allocated(metstype)) deallocate(metstype)

guess_initialized_=.false.
end subroutine final_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- Allocate grid and bundle holding metguess
!
! !INTERFACE:
!
!!subroutine create_(GSI_MetGuess_Bundle,im,jm,km,lm,istatus) ! ultimately
  subroutine create_(im,jm,km,lm,istatus)

! !USES:

    use constants,only: zero
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),intent(in)::im,jm,km,lm

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out)::istatus

! !INPUT/OUTPUT PARAMETERS:
!!  type(GSI_Bundle) :: GSI_MetGuess_Bundle

! !DESCRIPTION: allocate grids to hold guess cloud fields
!
! !REVISION HISTORY:
!   2010-04-20  todling  initial code
!   2010-05-17  todling  update create interface to pass a grid
!   2011-07-03  todling  allow running single or double precision
!   2011-10-07  todling  allow for 2d variables
!   2013-10-22  todling  handle for diverse 3d-level fields
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC

    character(len=*), parameter :: myname_ = myname//'*create_'
    integer(i_kind) nt,ier
    type(GSI_Grid):: grid

    istatus=0
    if(nmguess<=0) return

    if(guess_grid_initialized_) return

!   Create simple regular grid
    call gsi_gridcreate ( grid, im, jm, km )

    nbundles = lm
    allocate(GSI_MetGuess_Bundle(nbundles))
    do nt=1,nbundles
       call GSI_BundleCreate ( GSI_MetGuess_Bundle(nt), grid, 'Meteo Guess', ier, &
                               names3d=mguess3d,names2d=mguess2d,levels=levels3d,&
                               bundle_kind=r_kind )
       istatus=istatus+ier
    enddo

    if (istatus/=0) then
       if(mype==0) write(6,*)trim(myname_),':  allocate error1, istatus=',&
                      istatus,im,jm,km,lm
       return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': alloc() for met-guess done'
    endif
    guess_grid_initialized_=.true.

    return
  end subroutine create_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_ --- Deallocate grid and bundle holding metguess
!
! !INTERFACE:
!
!!subroutine destroy_ (GSI_MetGuess_Bundle, istatus) ! ultimately
  subroutine destroy_ (istatus)

! !USES:
  implicit none

! !INPUT PARAMETERS:

! !OUTPUT PARAMETERS:
  integer(i_kind), intent(out) :: istatus

! !INPPUT/OUTPUT PARAMETERS:
!!  type(GSI_Bundle) :: GSI_MetGuess_Bundle

! !DESCRIPTION: Dealloc grids holding for meteorological guess
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC

    character(len=*), parameter :: myname_ = myname//'*destroy_'
    integer(i_kind) :: nt,ier

    istatus=0
    if(.not.guess_grid_initialized_) return

     do nt=1,nbundles
        call GSI_BundleDestroy ( GSI_MetGuess_Bundle(nt), ier )
        istatus=istatus+ier
     enddo
     deallocate(GSI_MetGuess_Bundle,stat=istatus)
     istatus=istatus+ier

    if (istatus/=0) then
       if(mype==0) write(6,*)trim(myname_),':  deallocate error1, istatus=',istatus
       return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': dealloc() for met-guess done'
    endif
    guess_grid_initialized_=.false.

    return
  end subroutine destroy_
!EOC

! ----------------------------------------------------------
! From here down, inquiry function to make all object opaque
! ----------------------------------------------------------

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_int0d_ --- inquire rank-0 integer
!
! !INTERFACE:
  subroutine get_int0d_ ( desc, ivar, istatus )

! !USES:
  implicit none
!
! !DESCRIPTION: Rank-0 integer inquire routine; integer mnemonics:
! \begin{verbatim}
!      Known mnemonics        retrieve
!      ---------------        --------
!      dim                    total number of meteorological guesses 
!      i4crtm::XXX            information related to CRTM usage of gas XXX
!      var::XXX               index of gas XXX in met-bundle
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2011-05-17  todling  protect against use of unavailable label
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),intent(in):: desc
  integer(i_kind),intent(out):: ivar
  integer(i_kind),intent(out):: istatus
  character(len=*),parameter:: myname_=myname//"*get_int0d_"
  character(len=MAXSTR):: work
  integer(i_kind) ii,id,ln
  istatus=1
  ivar=0
  if(.not.guess_initialized_) return
  if(trim(desc)=='dim') then
     ivar = nmguess
     istatus=0
  else if(trim(desc)=='clouds') then
     ivar = ncloud
     istatus=0
  else if(trim(desc)=='clouds::3d') then
     ivar = n3dcloud
     istatus=0
  else if(trim(desc)=='clouds::2d') then
     ivar = n2dcloud
     istatus=0
  else if(trim(desc)=='meteo_4crtm_jac::3d') then
     do ii=1,ng3d
        if (i4crtm3d(ii)==2) ivar=ivar+1
     enddo
     istatus=0
  else if(trim(desc)=='clouds_4crtm_jac::3d') then
     do ii=1,ng3d
        if (i4crtm3d(ii)==12) ivar=ivar+1
     enddo
     istatus=0
  else if(trim(desc)=='clouds_4crtm_fwd::3d') then
     do ii=1,ng3d
        if (i4crtm3d(ii)>10) ivar=ivar+1
     enddo
     istatus=0
  else if(index(trim(desc),'i4crtm::')/=0) then
     ln=len_trim(desc)
     work=desc(9:ln)
     if(allocated(mguess)) then
        id=getindex(mguess,trim(work))
        if(id>0) ivar=i4crtm(id)
     else
        ivar=0
     endif
     istatus=0
  else if(desc(1:5)=='var::') then
     if(allocated(mguess)) then
        id=len_trim(desc)
        if(id>=6) ivar=getindex(mguess,desc(6:id))
     endif
     istatus=0
  else
     call die(myname_,'label unavailable :'//trim(desc),99)
  endif
  return
  end subroutine get_int0d_
!EOC
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_int1d_ --- inquire rank-1 integer
!
! !INTERFACE:
  subroutine get_int1d_ ( desc, ivar, istatus )

! !USES:
  implicit none
!
! !DESCRIPTION: Rank-1 integer inquire routine; integer mnemonics:
! \begin{verbatim}
!      Known mnemonics        retrieve
!      ---------------        --------
!      metguess_level         levels of all variables in metguess
!      clouds_level::3d       levels of all 3d clouds
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2011-05-17  todling  initial code
!   2011-05-17  todling  protect against use of unavailable label
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),intent(in):: desc
  integer(i_kind),intent(out):: ivar(:)
  integer(i_kind),intent(out):: istatus
  character(len=*),parameter:: myname_=myname//"*get_int1d_"
  integer(i_kind) i,ii
  logical labfound
  labfound=.false.
  istatus=1
  ivar=0
  if(.not.guess_initialized_) return
  if(trim(desc)=='guesses_level') then
     labfound=.true.
     do i=1,nmguess
        ivar(i)=levels(i) 
     enddo
     istatus=0
  endif
  if(trim(desc)=='clouds_level') then
     labfound=.true.
     ii=0
     do i=1,nmguess
        if(i4crtm(i)>=10.and.i4crtm(i)<20) then
           ii=ii+1
           ivar(ii)=levels(i) 
        endif
     enddo
     if(ii>0) istatus=0
  endif
  if(trim(desc)=='clouds_level::3d') then
     labfound=.true.
     ii=0
     do i=1,ng3d
        if(i4crtm3d(i)>=10.and.i4crtm3d(i)<20) then
           ii=ii+1
           ivar(ii)=levels3d(i) 
        endif
     enddo
     if(ii>0) istatus=0
  endif
  if (.not.labfound) then
     call die(myname_,'label unavailable :'//trim(desc),99)
  endif
  end subroutine get_int1d_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_char0d_ --- inquire rank-0 character 
!
! !INTERFACE:
  subroutine get_char0d_ ( desc, ivar, istatus )
! !USES:
  implicit none
!
! !DESCRIPTION: Character-string mnemonics (rank-0):
! \begin{verbatim}
!      Known mnemonics        retrieve
!      ---------------        --------
!      list                   list of all guesses
!      list::clouds           list of all cloud-related guesses
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2011-05-17  todling  protect against use of unavailable label
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),intent(in):: desc
  character(len=*),intent(out):: ivar
  integer(i_kind),intent(out):: istatus
  character(len=*),parameter:: myname_=myname//"*get_char0d_"
  character(len=MAXSTR):: gaslist
  character(len=MAXSTR),allocatable:: work(:)
  integer(i_kind) is,ie,i,i0
  logical labfound
  labfound=.false.
  istatus=1
  ivar=''
  if(.not.guess_initialized_) return
  if(trim(desc)=='list'.or.trim(desc)=='olist') then
     labfound=.true.
     if(nmguess>0) then
        allocate(work(size(mguess)))
        work=mguess
        if(desc(1:1)=='o') work=usrname
        gaslist=trim(work(1))
        do i=2,nmguess
           i0=len_trim(gaslist)
           is=i0+1
           ie=is+len_trim(work(i))+1
           gaslist(is:ie)=','//work(i)
        enddo
        if(nmguess>1.and.gaslist(1:1)==',') gaslist=gaslist(2:ie)
        ivar = trim(gaslist)
        if(ivar/='') istatus=0
        deallocate(work)
     endif
  endif
  if(trim(desc)=='list::clouds'.or.trim(desc)=='olist::clouds') then
     labfound=.true.
     if(ncloud>0) then
        allocate(work(size(mguess)))
        work=mguess
        if(desc(1:1)=='o') work=usrname
        gaslist=''
        if(abs(i4crtm(1))>=10.and.abs(i4crtm(1))<20) gaslist=trim(work(1))
        do i=2,nmguess
           if(abs(i4crtm(i))>=10.and.abs(i4crtm(i))<20) then
              i0=len_trim(gaslist)
              is=i0+1
              ie=is+len_trim(work(i))+1
              gaslist(is:ie)=','//work(i)
           endif
        enddo
        if(nmguess>1.and.gaslist(1:1)==',') gaslist=gaslist(2:ie)
        ivar = trim(gaslist)
        if(ivar/='') istatus=0
        deallocate(work)
     endif
  endif
  if (.not.labfound) then
     call die(myname_,'label unavailable :'//trim(desc),99)
  endif
  end subroutine get_char0d_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_char1d_ --- inquire rank-1 character 
!
! !INTERFACE:
  subroutine get_char1d_ ( desc, ivar, istatus )
! !USES:
  implicit none
!
! !DESCRIPTION: Rank-1 character inquire routine; character mnemonics:
! \begin{verbatim}
!      Known mnemonics        retrieve
!      ---------------        --------
!      gsinames               list of short names for met-fields as known in GSI
!      usrnames               list of user-difined met-fields
!      clouds::3d             list of 3d cloud fields
!      meteo_4crtm_jac::3d    list of 3d meteorology fields to participate in CRTM-Jac calc
!      clouds_4crtm_jac::3d   list of 3d cloud fields to participate in CRTM-Jac calc
!      clouds_4crtm_fwd::3d   list of 3d cloud fields to participate in CRTM-fwd calc
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2011-04-06  ho-chung fix return status code
!   2011-05-17  todling  protect against use of unavailable label
!   2015-07-17  zhu      add clouds_4crtm_fwd::3d for variables used in forward
!                        observation operator
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),intent(in):: desc
  character(len=*),intent(out):: ivar(:)
  character(len=*),parameter:: myname_=myname//"*get_char1d_"
  integer(i_kind),intent(out):: istatus
  integer(i_kind) i,ii
  logical labfound
  labfound=.false.
  istatus=1
  ivar=''
  if(.not.guess_initialized_) return
  if(trim(desc)=='gsinames') then
     labfound=.true.
     if(size(ivar)>=size(mguess)) then
        if(allocated(mguess))then
           ivar = mguess
           istatus=0
        endif
     endif
  endif
  if(trim(desc)=='usrnames') then
     labfound=.true.
     if(size(ivar)>=size(usrname)) then
        if(allocated(usrname))then
           ivar = usrname
           istatus=0
        endif
     endif
  endif
  if(trim(desc)=='clouds') then
     labfound=.true.
     if(size(ivar)>=ncloud) then
        ii=0
        do i=1,nmguess
           if(abs(i4crtm(i))>=10.and.abs(i4crtm(i))<20) then
              ii=ii+1
              ivar(ii)=mguess(ii) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='meteo_4crtm_jac::3d') then
     labfound=.true.
     ii=0
     do i=1,ng3d
        if(i4crtm3d(i)==2) then
           ii=ii+1
           ivar(ii)=mguess3d(i) 
        endif
     enddo
     if(ii>0) istatus=0
  endif
  if(trim(desc)=='clouds_4crtm_jac::3d') then
     labfound=.true.
     ii=0
     do i=1,ng3d
        if(i4crtm3d(i)==12) then
           ii=ii+1
           ivar(ii)=mguess3d(i) 
        endif
     enddo
     if(ii>0) istatus=0
  endif
  if(trim(desc)=='clouds_4crtm_fwd::3d') then
     labfound=.true.
     ii=0
     do i=1,ng3d
        if(i4crtm3d(i)>10) then
           ii=ii+1
           ivar(ii)=mguess3d(i)
        endif
     enddo
     if(ii>0) istatus=0
  endif
  if(trim(desc)=='clouds::3d') then
     labfound=.true.
     if(size(ivar)>=n3dcloud) then
        ii=0
        do i=1,ng3d
           if(abs(i4crtm3d(i))>=10.and.abs(i4crtm3d(i))<20) then
              ii=ii+1
              ivar(ii)=mguess3d(i) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='clouds::2d') then
     labfound=.true.
     if(size(ivar)>=n2dcloud) then
        ii=0
        do i=1,ng2d
           if(abs(i4crtm2d(i))>=10.and.abs(i4crtm2d(i))<20) then
              ii=ii+1
              ivar(ii)=mguess2d(i) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='cloud_types::3d') then
     labfound=.true.
     if(size(ivar)>=n3dcloud) then
        ii=0
        do i=1,ng3d
           if(abs(i4crtm3d(i))>=10.and.abs(i4crtm3d(i))<20) then
              ii=ii+1
              ivar(ii)=metsty3d(i)
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if (.not.labfound) then
     call die(myname_,'label unavailable :'//trim(desc),99)
  endif
  end subroutine get_char1d_
end module gsi_metguess_mod
!EOC
