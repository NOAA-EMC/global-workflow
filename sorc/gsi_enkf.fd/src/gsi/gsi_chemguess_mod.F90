!BOI

!  !TITLE: GSI\_ChemGuess\_Mod: A GSI Bundle to handle Trace Gases and Aerosols

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: 12 Oct 2010

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This module defines to so-called GSI\_ChemGuess\_Bundle. Its main purpose is to
allow GSI to ingest guess fields related to trace gases, aerosols, and chemistry
in general. 

\begin{center}
\fbox{Chem Bundle is a way to ingest Chemistry-related backgrounds into GSI}
\end{center}

Before the introduction of this module, all guess fields entered GSI through the 
arrays ges\_x, with x standing for particular fields, defined in the guess\_grids module, 
e.g., ges\_u, ges\_tv, and so on. Extending this approach to handle chemistry-related fields
could become rather complex, particularly, because it is in principle not known which fields are
needed for given application. The GSI\_ChemGuess\_Bundle aims at allowing GSI to ingest 
a general set of fields without a given order or particular specification. 

\underline{Caution}: An important exception is ozone. Since guess\_grids already handles ozone, this
is the only chemistry field that should still be dealt through guess\_gridsi, as ges\_oz.

\begin{center}
\fbox{Chem Bundle is a GSI\_Bundle}
\end{center}

The GSI\_ChemGuess\_Bundle uses the GSI\_Bundle. But while the state and control vectors 
use the GSI\_Bundle to associate fields used by the observation operator and
those used in the cost function, respectively, the GSI\_ChemGuess\_Bundle
is simply aimed at allowing ingestion of Chemistry Guess fields into GSI. The translation
of these guess fields into state and control vectors is still done via the state and
control vectors defining mechanism. 

As guess\_grids does, this module still treats the Chemistry Guess fields via a
common-block-like structure. That is, the GSI\_Bundle defined here to hold the
Chemistry Guess fields is an internally defined type that cannot be passed
around. This will change in the future, but for the time being this is the
simplest thing to do given the current code design.

\begin{center}
\fbox{Chem Bundle Module provides an (almost) opaque access to the entries in
the object}
\end{center}

One of the ideas behind this module is that it defines an opaque-like object.
That is, functions related to contents of the Chem Bundle should can only be
extracted via inquires through a ``get-like'' procedures. This is why, only ``methods'' are 
made public to this module, that is, 

\begin{verbatim}
public :: gsi_chemguess_create_grids
public :: gsi_chemguess_destroy_grids
public :: gsi_chemguess_init
public :: gsi_chemguess_get
public :: gsi_chemguess_final
\end{verbatim}

and never the variables themselves; the only exception being the GSI\_ChemGuess\_Bundle itself 
(until it is no longer treated as a common-block).  Some of the above public methods are 
overloaded and all  have internal interfaces (name of which appears in the index of this protex 
document. It should be a rule here that any new routine to be make public should
have a declared interface procedure.

\begin{center}
\fbox{Chem Bundle is defined via the {\it chem\_guess} table in a resource file}
\end{center}

\underline{Defining the Chem Bundle} is done via the table {\it chem\_guess}, usually 
embedded in the {\it anavinfo} file. An example of such table follows:
\begin{verbatim}
chem_guess::
!#var     level  itracer crtm_use   type              orig_name
  co       72      1        -1       n/a                co
  co2      72      1         0       n/a                co2
!#      GOCART Aerosols
!#    ------ Dust ------  
  du001     72      1         10      dust               DU001
  du002     72      1         10      dust               DU002
  du003     72      1         10      dust               DU003
  du004     72      1         10      dust               DU004
  du005     72      1         10      dust               DU005
!#     ------ Sea-salt ------
  ss001     72      1         10      ssam               SS001
  ss002     72      1         10      sscm1              SS002
  ss003     72      1         10      sscm2              SS003
  ss004     72      1         10      sscm3              SS004
  ss005     72      1         10      sscm4              SS005
!#     ------ Sulfates ------
  so4       72      1         10      sulfate            SO4
!#     ------ Carbonaceous (main) ------
  bcphobic  72      1         10      dry_black_carbon   BCphobic
  bcphilic  72      1         10      wet_black_carbon   BCphilic
  ocphobic  72      1         10      dry_organic_carbon OCphobic
  ocphilic  72      1         10      wet_organic_carbon OCphilic
::
\end{verbatim}
This is what GMAO plans to use in the near future. 

As usual, this table follows INPAK/ESMF convention, begining with a name
(chem\_guess), followed by double colons (::), and ending with double colons.
Any line starting with an exclamation mark or a pound sign is taken as a comment.

The current {\it chem\_guess} table has six columns defined as follows:

\begin{verbatim}
Column 1: variable name - refers to internally known GSI variable name
Column 2: indicates number of levels (used to distinguish between 2d and 3d fields)
Column 3: likely to be redefined sometime soon
Column 4: indicates whether variable is to be passed to CRTM or not according to 
          the following scheme:
          if<0    general chem variable; not used in CRTM 
          if=0    general chem variable; use prescribed global mean data to affect CRTM
          if=1    general chem variable; use variable in guess field to affect CRTM 
          if>10   aerosol variable
Column 5: type of chemical/aerosol
Column 6: original name in file where species is read from
\end{verbatim}

\begin{center}
\fbox{Examples of extracting information related to the Chem Bundle}
\end{center}

\underline{Examples} of accessing information related to fields in the Chem Bundle.
\begin{enumerate}
\item Say a routine wants to know how $CO_2$ is to be used in CRTM. 
      This is done via the {\it i4crtm::} tag, as in:
      \begin{verbatim}
      call gsi_chemguess_get ( 'i4crtm::co2', igfsco2, ier )
      \end{verbatim}
       where the value returned {\it igfsco2} is an integer following the 
       scheme laid out for entries in column 4 of the resource file (anavinfo). 

\item Say a routine wants to get the number of all 3d aerosols available in the
      Chem Bundle, this can use the tag {\it aerosols::3d}, as in:
      \begin{verbatim}
      call gsi_chemguess_get ( 'aerosols::3d',n_aerosols,ier )
      \end{verbatim}
      notice this uses the same interface as in the example above, but returns
      information about something else.

\item Say a routine wants the name of all 3d aerosols
      \begin{verbatim}
      call gsi_chemguess_get ('aerosols::3d',aero_names,ier)     
      \end{verbatim}
      now the returned variable {\it aero\_names} is a character array with the 
      names of all 3d-aerosols. Notice it is important to inquire before hand
      about the number of  3d-aerosols available and to properly allocate space
      for the character arrays {\it aero\_names}, and only then make the call above.

\end{enumerate}

More on the other possible mnemonics known by this package can be found in the 
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
 
A general remark about the correct {\it chem\_guess} table: it is recognized that
the format for general specification related to specific entries in the table is 
not general enough. A better approach is the one used by the Registry used in
GEOS-5 GOCART where a table exists to control a particular functionality
applicable to a certain set of constituents. For example, use of a variable in
CRTM could be control instead by a specific table listing constituents to be
used in the CRTM and at what extent, for example, a table of the form:
\begin{verbatim}
use_in_crtm::
 !var     use  
  co2      1
  co       0
::
\end{verbatim}
Something of this form should eventually replace some of the columns in the 
{\it chem\_guess} table. 

#endif
!EOI


!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP
!  
! !MODULE: ChemguessMod -- Implements Chem Guess capability for GSI
!
! !INTERFACE:

module gsi_chemguess_mod
!
! !DESCRIPTION: Module to handle chemistry fields, tracers, and aerosols.
!               This still uses wired-in type arrays to hold the guess. Soon
!               we'll generalize this.
!
! !REMARKS:
!   1. VERY IMPORTANT: No object from this file is to be make
!                      explicitly available to the outside world.
!                      Each object must be opaque with a get and
!                      a put method associated with it.
!   2. This is still functioning as a common-block when it comes
!      to the chem type itself - needs some work to make it into 
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

public :: gsi_chemguess_create_grids
public :: gsi_chemguess_destroy_grids
public :: gsi_chemguess_init
public :: gsi_chemguess_get
public :: gsi_chemguess_final

public :: GSI_ChemGuess_Bundle   ! still a common for now, ultimately should 
                                 ! be a dynamic "type", passed around in arg list

! !INTERFACE:

interface gsi_chemguess_init
          module procedure init_
end interface
interface gsi_chemguess_final
          module procedure final_ 
end interface
interface gsi_chemguess_create_grids
          module procedure create_
end interface
interface gsi_chemguess_destroy_grids
          module procedure destroy_
end interface
interface gsi_chemguess_get
          module procedure get_int0d_
          module procedure get_char0d_
          module procedure get_char1d_
end interface

type(GSI_Bundle),pointer :: GSI_ChemGuess_Bundle(:)   ! still a common for now


! !REVISION HISTORY:
!
!   20Apr2010 Todling  Initial code.
!   03May2010 Treadon - add iostat error check to ibm_sp read(lu,chemguess) in init_
!   19May2010 Todling - porter Hou's igfsco2 flag from setup namelist to this namelist
!   30May2010 Todling - remove namelist; revamp the way fields/info read in (i90-style)
!   25Jun2010 Treadon - consistently intialize ivar; check/use length of desc (gsi_chemguess_get)
!   07Oct2010 Todling - add entry usrname to differentiate gsi-names w/ in-file names
!   01May2011 Todling - rename module and its bundle for parallelism w/ MetGuess
!
!EOP
!-------------------------------------------------------------------------

! !PRIVATE ROUTINES:
!BOC

integer(i_kind),parameter::MAXSTR=max_varname_length
logical:: chem_grid_initialized_=.false.
logical:: chem_initialized_=.false.
character(len=*), parameter :: myname = 'gsi_chemguess_mod'

integer(i_kind) :: nbundles=-1
integer(i_kind) :: ntgases=0
integer(i_kind) :: naero=0          ! number of aerosols
integer(i_kind) :: nghg =0          ! number of green-house gases
integer(i_kind) :: n2daero=0
integer(i_kind) :: n3daero=0
integer(i_kind) :: ng3d=-1
integer(i_kind) :: ng2d=-1
character(len=MAXSTR),allocatable :: tgases(:)     ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: tgases3d(:)   ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: tgases2d(:)   ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: chemtype(:)   ! indicate type of chem (used for aerosols for now)
character(len=MAXSTR),allocatable :: chemty3d(:)   ! indicate 3d type of chem
character(len=MAXSTR),allocatable :: chemty2d(:)   ! indicate 3d type of chem
character(len=MAXSTR),allocatable :: usrname3d(:)  ! chem user-defined (original) 3d name (in file)
character(len=MAXSTR),allocatable :: usrname2d(:)  ! chem user-defined (original) 2d name (in file)
character(len=MAXSTR),allocatable :: usrname(:)    ! chem user-defined (original) name (in file)
integer(i_kind),allocatable,dimension(:) :: i4crtm ! controls use of gas in CRTM:
                                                   ! < 0 don't use in CRTM
                                                   ! = 0 use predefined global mean co2 mixing ration
                                                   ! = 1 use gfs yearly global annual mean historical co2 value
                                                   ! = 2 use gfs monthly horizontal 2-d historical co2 value
                                                   ! in interval [0,10)  general trace gas
                                                   ! in interval [10,20) indicates aerosol
integer(i_kind),allocatable,dimension(:) :: i4crtm2d ! as above but for 2d fields
integer(i_kind),allocatable,dimension(:) :: i4crtm3d ! as above but for 3d fields

logical:: verbose_=.true.

contains

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_ --- Initialize Chem Bundle (read resource table); alloc internal
!
! !INTERFACE:
!
subroutine init_ (iamroot)
! USES:
implicit none
! !INPUT PARAMETER:
   logical,optional,intent(in) :: iamroot 
! !DESCRIPTION: Define contents of Chem Bundle through rc file (typilcally
!               embedded in anavinfo text file.
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2013-09-30  todling  allow 40-char var description
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
!character(len=*),parameter:: rcname='anavinfo.txt'
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='chem_guess::'
integer(i_kind) luin,i,ii,ntot,icrtmuse
character(len=256),allocatable,dimension(:):: utable
character(len=40) ctype
character(len=20) var,oname
character(len=*),parameter::myname_=myname//'*init_'
integer(i_kind) ilev, itracer
logical iamroot_

if(chem_initialized_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
luin=get_lun()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,ntgases)
if(ntgases==0) then
   close(luin)
   return
endif

! Get contents of table
allocate(utable(ntgases))
call gettable(tbname,luin,ntot,ntgases,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
ng3d=0; ng2d=0
do ii=1,ntgases
   read(utable(ii),*) var, ilev, itracer, icrtmuse
   if(ilev==1) then
       ng2d=ng2d+1
   else
       ng3d=ng3d+1
   endif
enddo

allocate(i4crtm(ntgases),usrname(ntgases),&
         tgases(ntgases),chemtype(ntgases))
if(ng3d > 0)allocate(tgases3d(ng3d),&
                     chemty3d(ng3d),&
                     i4crtm3d(ng3d),&
                     usrname3d(ng3d))
if(ng2d > 0)allocate(tgases2d(ng2d),&
                     chemty2d(ng2d),&
                     i4crtm2d(ng2d),&
                     usrname2d(ng2d))

! Now load information from table
ng3d=0;ng2d=0
do ii=1,ntgases
   read(utable(ii),*) var, ilev, itracer, icrtmuse, ctype, oname
   if(ilev==1) then
      ng2d=ng2d+1
      tgases2d(ng2d)=trim(adjustl(var))
      chemty2d(ng2d)=trim(adjustl(ctype))
      i4crtm2d(ng2d)=icrtmuse
      usrname2d(ng2d)=trim(adjustl(oname))
      if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) n2daero=n2daero+1 ! convention, for now
   else
      ng3d=ng3d+1
      tgases3d(ng3d)=trim(adjustl(var))
      chemty3d(ng3d)=trim(adjustl(ctype))
      i4crtm3d(ng3d)=icrtmuse
      usrname3d(ng3d)=trim(adjustl(oname))
      if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) n3daero=n3daero+1 ! convention, for now
   endif
   if(abs(icrtmuse)< 10)                      nghg =nghg +1 ! GHG  convention, for now
   if(abs(icrtmuse)>=10.and.abs(icrtmuse)<20) naero=naero+1 ! AERO convention, for now
enddo

deallocate(utable)

! Fill in array w/ all var names (must be 3d first, then 2d)
ii=0
do i=1,ng3d
   ii=ii+1
   tgases(ii)=tgases3d(i)
   i4crtm(ii)=i4crtm3d(i)
   usrname(ii)=usrname3d(i)
   chemtype(ii)=trim(adjustl(chemty3d(i)))
enddo
do i=1,ng2d
   ii=ii+1
   tgases(ii)=tgases2d(i)
   i4crtm(ii)=i4crtm2d(i)
   usrname(ii)=usrname2d(i)
   chemtype(ii)=trim(adjustl(chemty2d(i)))
enddo

if (iamroot_) then
   write(6,*) myname_,':  2D-CHEM STATE VARIABLES: '
   do i=1,ng2d
      write(6,*) trim(tgases2d(i))
   enddo
   write(6,*) myname_,':  3D-CHEM STATE VARIABLES:'
   do i=1,ng3d
      write(6,*) trim(tgases3d(i))
   enddo
   write(6,*) myname_,': ALL CHEM STATE VARIABLES:'
   do i=1,ntgases
      write(6,*) trim(tgases(i))
   enddo
end if
chem_initialized_=.true.

end subroutine init_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  final_ --- Deallocate internal Chem Bundle info arrays
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
if(.not.chem_initialized_) return

if(allocated(tgases))   deallocate(tgases)
if(allocated(i4crtm))   deallocate(i4crtm)
if(allocated(chemtype)) deallocate(chemtype)
if(allocated(usrname))  deallocate(usrname)
if(allocated(usrname3d))deallocate(usrname3d)
if(allocated(usrname2d))deallocate(usrname2d)
if(allocated(tgases3d)) deallocate(tgases3d)
if(allocated(tgases2d)) deallocate(tgases2d)
if(allocated(i4crtm3d)) deallocate(i4crtm3d)
if(allocated(i4crtm2d)) deallocate(i4crtm2d)
if(allocated(chemty3d)) deallocate(chemty3d)
if(allocated(chemty2d)) deallocate(chemty2d)

chem_initialized_=.false.
end subroutine final_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- Allocate grid and bundle holding chem guess
!
! !INTERFACE:
!
!!subroutine create_(GSI_ChemGuess_Bundle,im,jm,km,lm,istatus) ! ultimately
  subroutine create_(im,jm,km,lm,istatus)

! !USES:

    use constants,only: zero
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),intent(in)::im,jm,km,lm

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out)::istatus

! !INPUT/OUTPUT PARAMETERS:
!!  type(GSI_Bundle) :: GSI_ChemGuess_Bundle

! !DESCRIPTION: allocate grids to hold guess cloud fields
!
! !REVISION HISTORY:
!   2010-04-20  todling  initial code
!   2010-05-17  todling  update create interface to pass a grid
!   2011-07-03  todling  allow running single or double precision
!   2011-11-16  todling  allow 2d tracers (e.g., AOD)
!   2017-02-22  todling  initialized only what needed(2d,3d,or both)
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------
!BOC

    character(len=*), parameter :: myname_ = myname//'*create_'
    integer(i_kind) nt
    type(GSI_Grid):: grid

    istatus=0
    if(ntgases<=0) return

    if(chem_grid_initialized_) return

!   Create simple regular grid
    call gsi_gridcreate ( grid, im, jm, km )

    nbundles = lm
    allocate(GSI_ChemGuess_Bundle(nbundles))
    do nt=1,nbundles
       if (ng2d>0.and.ng3d>0) then
          call GSI_BundleCreate ( GSI_ChemGuess_Bundle(nt), grid, 'Trace Gases', istatus, &
                                  names2d=tgases2d,names3d=tgases3d,bundle_kind=r_kind )
       else if (ng2d>0) then
          call GSI_BundleCreate ( GSI_ChemGuess_Bundle(nt), grid, 'Trace Gases', istatus, &
                                  names2d=tgases2d,bundle_kind=r_kind )
       else if (ng3d>0) then
          call GSI_BundleCreate ( GSI_ChemGuess_Bundle(nt), grid, 'Trace Gases', istatus, &
                                  names3d=tgases3d,bundle_kind=r_kind )
       else
          istatus=99
       endif
    enddo

    if (istatus/=0) then
        if(mype==0) write(6,*)trim(myname_),':  allocate error1, istatus=',&
                              istatus,im,jm,km,lm
        return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': alloc() for chem-tracer done'
    endif
    chem_grid_initialized_=.true.

    return
  end subroutine create_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_ --- Deallocate grid and bundle holding chem guess
!
! !INTERFACE:
!
!!subroutine destroy_ (GSI_ChemGuess_Bundle, istatus) ! ultimately
  subroutine destroy_ (istatus)

! !USES:
    implicit none

! !INPUT PARAMETERS:

! !OUTPUT PARAMETERS:
  integer(i_kind), intent(out) :: istatus

! !INPPUT/OUTPUT PARAMETERS:
!!  type(GSI_Bundle) :: GSI_ChemGuess_Bundle

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

    character(len=*), parameter :: myname_ = myname//'*destroy_'
    integer(i_kind) :: nt,ier
    istatus=0

    if(.not.chem_grid_initialized_) return

     do nt=1,nbundles
        call GSI_BundleDestroy ( GSI_ChemGuess_Bundle(nt), ier )
        istatus=istatus+ier
     enddo
     deallocate(GSI_ChemGuess_Bundle,stat=istatus)
     istatus=istatus+ier

    if (istatus/=0) then
        if(mype==0) write(6,*)trim(myname_),':  deallocate error1, istatus=',istatus
        return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': dealloc() for chem-tracer done'
    endif
    chem_grid_initialized_=.false.

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
!      dim                    total number of gases 
!      aerosols               number of aerosols
!      aerosols::3d           number of 3d aerosols
!      aerosols::2d           number of 2d aerosols
!      i4crtm::XXX            information related to CRTM usage of gas XXX
!      var::XXX               index of gas XXX in chem-bundle
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2011-05-17  todling  protect against use of unavailable label
!   2015-09-05  zhu      change "i4crtm3d(ii)==11" to "i4crtm3d(ii)>=11" 
!                        for "aerosols_4crtm::3d"
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
  character(len=*),parameter::myname_=myname//'*get_int0d_'
  character(len=MAXSTR):: work
  integer(i_kind) ii,id,ln
  istatus=1
  ivar=0
  if(.not.chem_initialized_) return
  if(trim(desc)=='dim') then
     ivar = ntgases
     istatus=0
  else if(trim(desc)=='ghg') then
     ivar = nghg
  else if(trim(desc)=='aerosols') then
     ivar = naero
     istatus=0
  else if(trim(desc)=='aerosols::3d') then
     ivar = n3daero
     istatus=0
  else if(trim(desc)=='aerosols::2d') then
     ivar = n2daero
     istatus=0
  else if(trim(desc)=='aerosols_4crtm::3d') then
     do ii=1,ng3d
        if (i4crtm3d(ii)>=11) ivar=ivar+1
     enddo
     istatus=0
  else if(trim(desc)=='aerosols_4crtm_jac::3d') then
     do ii=1,ng3d
        if (i4crtm3d(ii)==12) ivar=ivar+1
     enddo
     istatus=0
  else if(index(trim(desc),'i4crtm::')/=0) then
     ln=len_trim(desc)
     work=desc(9:ln)
     if(allocated(tgases)) then
        id=getindex(tgases,trim(work))
        if(id>0) ivar=i4crtm(id)
     else
        ivar=0
     endif
     istatus=0
  else if(desc(1:5)=='var::') then
     if(allocated(tgases)) then
        id=len_trim(desc)
        if(id>=6) ivar=getindex(tgases,desc(6:id))
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
!      list                   list of all gases
!      list::aerolols         list of aerosols only
!      list::tracers          list of trace gases (non-aerosols) only
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
  character(len=*),parameter::myname_=myname//'*get_char0d_'
  character(len=MAXSTR):: gaslist
  character(len=MAXSTR),allocatable:: work(:)
  integer(i_kind) is,ie,i,i0
  logical labfound
  labfound=.false.
  istatus=1
  ivar=''
  if(.not.chem_initialized_) return
  if(trim(desc)=='list'.or.trim(desc)=='olist') then
     labfound=.true.
     if(ntgases>0) then
        allocate(work(size(tgases)))
        work=tgases
        if(desc(1:1)=='o') work=usrname
        gaslist=trim(work(1))
        do i=2,ntgases
           i0=len_trim(gaslist)
           is=i0+1
           ie=is+len_trim(work(i))+1
           gaslist(is:ie)=','//work(i)
        enddo
        if(ntgases>1.and.gaslist(1:1)==',') gaslist=gaslist(2:ie)
        ivar = trim(gaslist)
        if(ivar/='') istatus=0
        deallocate(work)
     endif
  endif
  if(trim(desc)=='list::aerosols'.or.trim(desc)=='olist::aerosols') then
     labfound=.true.
     if(naero>0) then
        allocate(work(size(tgases)))
        work=tgases
        if(desc(1:1)=='o') work=usrname
        gaslist=''
        if(abs(i4crtm(1))>=10.and.abs(i4crtm(1))<20) gaslist=trim(work(1))
        do i=2,ntgases
           if(abs(i4crtm(i))>=10.and.abs(i4crtm(i))<20) then
              i0=len_trim(gaslist)
              is=i0+1
              ie=is+len_trim(work(i))+1
              gaslist(is:ie)=','//work(i)
           endif
        enddo
        if(ntgases>1.and.gaslist(1:1)==',') gaslist=gaslist(2:ie)
        ivar = trim(gaslist)
        if(ivar/='') istatus=0
        deallocate(work)
     endif
  endif
  if(trim(desc)=='list::tracers'.or.trim(desc)=='olist::tracers') then
     labfound=.true.
     if(ntgases>0) then
        allocate(work(size(tgases)))
        work=tgases
        if(desc(1:1)=='o') work=usrname
        gaslist=''
        if(abs(i4crtm(1))>=0.and.abs(i4crtm(1))<10) gaslist=trim(work(1))
        do i=2,ntgases
           if(abs(i4crtm(i))>=0.and.abs(i4crtm(i))<10) then
              i0=len_trim(gaslist)
              is=i0+1
              ie=is+len_trim(work(i))+1
              gaslist(is:ie)=','//work(i)
           endif
        enddo
        if(ntgases>1.and.gaslist(1:1)==',') gaslist=gaslist(2:ie)
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
  subroutine get_char1d_ ( desc, cvar, istatus )
! !USES:
  implicit none
!
! !DESCRIPTION: Rank-1 character inquire routine; character mnemonics:
! \begin{verbatim}
!      Known mnemonics        retrieve
!      ---------------        --------
!      gsinames               list of all trace gas names as known in GSI
!      usrnames               list of all user-defined gas names
!      aerosols               list of all aerosols
!      aerosols::3d           list of 3d aerosols
!      aerosols::2d           list of 2d aerosols
!      aerosols_4crtm::3d     list of 3d aerosols to be passed to CRTM
!      aerosols_4crtm_jac::3d list of 3d aerosols to participate in CRTM-Jac calc
! 
! \end{verbatim}
!  where XXX represents the name of the gas of interest. 
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!   2011-04-06  ho-chung fix return status code
!   2011-05-17  todling  protect against use of unavailable label
!   2012-05-12  todling  fix to return aero-4crtm of all aero's
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
  character(len=*),intent(out):: cvar(:)
  integer(i_kind),intent(out):: istatus
  character(len=*),parameter::myname_=myname//'*get_char0d_'
  integer(i_kind) i,ii,nvar
  logical labfound
  labfound=.false.
  istatus=1
  cvar=''
  if(.not.chem_initialized_) return
  nvar=size(cvar)
  if(trim(desc)=='gsinames') then
     labfound=.true.
     if(nvar>=size(tgases)) then
        if(allocated(tgases))then
          cvar(1:size(tgases)) = tgases
          istatus=0
        endif
     endif
  endif
  if(trim(desc)=='usrnames') then
     labfound=.true.
     if(nvar>=size(usrname)) then
        if(allocated(usrname))then
          cvar(1:size(tgases)) = usrname
          istatus=0
        endif
     endif
  endif
  if(trim(desc)=='ghg') then
     labfound=.true.
     if(nvar>=nghg) then
        ii=0
        do i=1,ntgases
           if(abs(i4crtm(i))<10) then
              ii=ii+1
              cvar(ii)=tgases(ii) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='aerosols') then
     labfound=.true.
     if(nvar>=naero) then
        ii=0
        do i=1,ntgases
           if(abs(i4crtm(i))>=10.and.abs(i4crtm(i))<20) then
              ii=ii+1
              cvar(ii)=tgases(ii) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='aerosols_4crtm::3d') then
     labfound=.true.
     if(nvar>=0) then
        ii=0
        do i=1,ng3d
           if(i4crtm3d(i)>=11) then
              ii=ii+1
              if(ii>nvar)then
                 ii=-1 ! user did not allocate enough space
                 exit  ! exit in error
              endif
              cvar(ii)=tgases(i)
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='aerosols_4crtm_jac::3d') then
     labfound=.true.
     if(nvar>=0) then
        ii=0
        do i=1,ng3d
           if(i4crtm3d(i)==12) then
              ii=ii+1
              if(ii>nvar)then
                 ii=-1 ! user did not allocate enough space
                 exit  ! exit in error
              endif
              cvar(ii)=tgases(i)
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='aerosols::3d') then
     labfound=.true.
     if(nvar>=0) then
        ii=0
        do i=1,ng3d
           if(abs(i4crtm3d(i))>=10.and.abs(i4crtm3d(i))<20) then
              ii=ii+1
              if(ii>nvar)then
                 ii=-1 ! user did not allocate enough space
                 exit  ! exit in error
              endif
              cvar(ii)=tgases3d(i) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if(trim(desc)=='aerosols::2d') then
     labfound=.true.
     if(nvar>=0) then
        ii=0
        do i=1,ng2d
           if(abs(i4crtm2d(i))>=10.and.abs(i4crtm2d(i))<20) then
              ii=ii+1
              if(ii>nvar)then
                 ii=-1 ! user did not allocate enough space
                 exit  ! exit in error
              endif
              cvar(ii)=tgases2d(i) 
           endif
        enddo
        if(ii>0) istatus=0
     endif
  endif
  if (.not.labfound) then
     call die(myname_,'label unavailable :'//trim(desc),99)
  endif
  end subroutine get_char1d_
end module gsi_chemguess_mod
!EOC
