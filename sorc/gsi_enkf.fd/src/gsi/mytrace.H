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
