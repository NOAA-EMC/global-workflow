!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module trace.H
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-03-22
!
! abstract: macros for code tracking
!
! program history log:
!   2010-03-22  j guo   - added this document block
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

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: trace.H - macros for code tracing.
!
! !DESCRIPTION:
!
!   This header file contains following six macros:
!
!	_ENTRY_ and _ALLENTRY_	: used at the entry point of a routine
!	_EXIT_ and _ALLEXIT_	: used at the exit of a routine
!	_TRACE_(s) and _ALLTRACE_(s)
!				: used with a string message s=""
!	_ALWAYS_TRACE_(s) and _ALWAYS_ALLTRACE_(s)
!				: are always defined, while other
!				: macros can be turned on only after
!				: -DDEBUG_TRACE is specified during
!				: preprocessing stage of compilation.
!
! where the second macros in all groups are define to do a MPI_Barrier()
! before they write out their messages.
!
! !INTERFACE:
!
!   The interface of this header file is better represented in the
! following example:
!
!   #include "trace.H"
!   use mpeu_util,only : tell
!   implicit none
!   #include "mpif.h"
!	! myname_ is required parameter for all macros.
!   character(len=*),parameter :: myname_='m_this::that'
!	! ier and comm are required parameters for macros
!	! _ALLENTRY_, _ALLEXIT_, and _ALLTRACE("")
!   integer :: ier	
!   integer :: comm
!   comm=MPI_COMM_WORLD		! or any other communicator
!				! if turned on (with -DDEBUG_TRACE),
!   _ENTRY_			! produces "m_this::that: entered";
!   _TRACE("checkpoint")	! produces "m_this::that: checkpoint";
!   _EXIT_			! produces "m_this::that: exiting".
!
! !REVISION HISTORY:
! 	16Feb05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

	! These macros are always redefined, such that one may toggle
	! between turn them on or off.

#ifdef _ALLENTRY_
#undef _ALLENTRY_
#endif
#ifdef _ENTRY_
#undef _ENTRY_
#endif
#ifdef _ALLEXIT_
#undef _ALLEXIT_
#endif
#ifdef _EXIT_
#undef _EXIT_
#endif
#ifdef _ALLTRACE_
#undef _ALLTRACE_
#endif
#ifdef _TRACE_
#undef _TRACE_
#endif
#ifdef _ALLTRACEV_
#undef _ALLTRACEV_
#endif
#ifdef _TRACEV_
#undef _TRACEV_
#endif
#ifdef _PRINT_
#undef _PRINT_
#endif

	! NOw define all of them, if trace is turned on by -DDEBUG_TRACE
#ifdef DEBUG_TRACE

#define _ALLENTRY_(me,comm,ier) call MPI_Barrier(comm,ier);call tell(me,"entered")
#define _ALLEXIT_(me,comm,ier) call MPI_Barrier(comm,ier);call tell(me,"exiting")
#define _ALLTRACE_(me,EX,comm,ier) call MPI_Barrier(comm,ier);call tell(me,EX)
#define _ALLTRACEV_(me,EX,v,comm,ier) call MPI_Barrier(comm,ier);call tell(me,EX,v)

#define _ENTRY_(me) call tell(me,"entered")
#define _EXIT_(me) call tell(me,"exiting")
#define _TRACE_(me,EX) call tell(me,EX)
#define _TRACEV_(me,EX,v) call tell(me,EX,v)

	! Or define them to null, if DEBUG_TRACE is not defined.
#else
#define _ALLENTRY_(me,comm,ier)
#define _ALLEXIT(me,comm,ier)
#define _ALLTRACE_(me,EX,comm,ier)
#define _ALLTRACEV_(me,EX,v,comm,ier)

#define _ENTRY_(me)
#define _EXIT_(me)
#define _TRACE_(me,EX)
#define _TRACEV_(me,EX,v)

#endif

	! Following macros are always defined.
#ifndef _ALWAYS_TRACE_
#define _ALWAYS_TRACE_(me,EX) call tell(me,EX)
#define _PRINT_(me,EX) call tell(me,#EX//" =",EX)
#endif

#ifndef _ALWAYS_ALLTRACE_
#define _ALWAYS_ALLTRACE_(me,EX,comm,ier) call MPI_Barrier(comm,ier);call tell(me,EX)
#define _ALLPRINT_(me,EX,comm,ier) call MPI_Barrier(comm,ier);call tell(me,#EX//" =",EX)
#endif

#ifndef _ALWAYS_TRACEV_
#define _ALWAYS_TRACEV_(me,EX,v) call tell(me,EX,v)
#endif

#ifndef _ALWAYS_ALLTRACEV_
#define _ALWAYS_ALLTRACEV_(me,EX,v,comm,ier) call MPI_Barrier(comm,ier);call tell(me,EX,v)
#endif
