/*********************************************************************
   avec_timer_lib.c

   Created 20160226, John Michalakes, NOAA-Affiliate, IMSG

 * Documentation (from email 2/26/2016):

   The timer package is a single file, avec_timer_lib.c, and is callable
   from Fortran (at least on systems that support the underscore naming
   convention, which includes the Intel compilers). You should be able to
   compile and link it as part of your build, probably most convenient to add
   it to whatever directories and make rules you might already have for C.
   If you don’t have that then just cc -c avec_timer_lib.c it to produce
   a .o and add it to your link objects.
   
   Interface:
   
   SUBROUTINE avec_timer_init ( taskid, retval )
   	INTEGER, INTENT(IN) :: tasked     ! rank of MPI task on which this is called
   	INTEGER, INTENT(OUT) :: retval    !  0 if success
   
   Call this on every task from which it reads the file named
   avec_timers_config.txt in the current working directory. File has a
   format like the example below:
   
   maxintervals   10000
   verbose
   zone 2 physics
   zone 3 radiation
   zone 1 timestep
   
   Each line has one or more items separated by a space.   The entries can
   be in any order, but otherwise the format’s pretty quick-and-dirty so
   don’t stress it too much.  Meaning of the entries:
   
   •	maxintervals is an integer that specifies the maximum number of
   intervals that will be timed in any zone. Whichever zone is likely to
   have the most intervals, set this to the number of separate start/stops
   of the timer you expect during the run plus some safety factor.
   
   •	lines that start with the word zone specify a timing zone. Second
   item in the entry is an integer zone id. This is what you’ll use to
   specify the zone when you start or stop timers.  The third item in the
   entry is a name for the zone.  The maximum number of zones is ten and
   the zones have to be numbered between 0 and 9.	I didn’t use a zone
   zero in the example above but I could have. The zones can be specified
   in any order and their numbers do not have to be consecutive.
   
   •	The word verbose on a line by itself lets the package write things
   like warnings to standard error during the job.  The package contains
   no termination statements.  It only ever returns, error, warning or no.
   
   The package as currently provided assumes its only ever called in a
   single-threaded region.
        - - - 
   SUBROUTINE avec_timer_start ( zone_id )
	INTEGER, INTENT(IN) :: zone_id		! one of the integer
	zones specified in the config file

   SUBROUTINE avec_timer_stop( zone_id )
	INTEGER, INTENT(IN) :: zone_id		! one of the integer
	zones specified in the config file

   Start and stop a timer for a zone.  Different zones can be nested,
   but the start and and stop of a zone have to match up.  That is,
   a zone that’s started can’t be started again, and a zone can’t
   be stopped unless it’s started.

        - - - 
   SUBROUTINE avec_timers_output

   No arguments.  Call this on each task at the end of the run and
   it will output the timing data for each task, one file per task.
   The name of the file is avec_timer_out_dddd.txt where dddd is the
   zero-padded rank of the task.

 * Requirements (from email 2/19/2016):

   1. The timer should have at least millisecond resolution and
   it should measure real elapsed time (not cpu).

      a.       I often use a timer based on gettimeofday, but there’s
      also the built-in Fortran system timer (don’t remember exact
      name). For geeking out at the hardware cycle level there’s the
      Intel rdtsc timer.

      b.      You should both use the same one.  Ideally, you might also
      consider constructing and sharing the same timer library between
      the models.  I’ll leave it to you two to decide and agree.

   2. Zones that should be timed:

      a.       Each model time step

      b.      Call(s) to physics interface within the time step. Outside
      any threading if possible.  If the physics interface is called
      in a threaded region, store the times for each thread in a shared
      array then compute and store the sum after the parallel region.

      c.       Calls to physics from within the physics interface (to
      allow us to measure the cost of the interface). As above, if the
      physics is called within a threaded region, store the times for
      each thread in a shared array the compute and store the sum after
      the parallel region.

      d.      Rusty: your call but consider putting a timer around the
      vertical remapping that we discussed yesterday. This may allow us
      to adjust for additional passes that occur at some resolutions.
      Michael: if there’s anything like that in MPAS -- computations
      that occur at different frequencies relative to the other cycles
      or subcycles depending on which resolution is being run, you might
      want to consider timing those too.

      e.      If there’s something else you think is important please
      time it.  Easier to discard timings we don’t need than rerun to
      get ones we didn’t realize until later that we needed.

   3. Also include a running time counter.  That is, at the
   beginning of each new timed zone, store time since start of the run
   and zone id and a sequence number for that zone.  This gives us a
   way to measure times over arbitrary intervals and to reconstruct any
   missing time as a residual.

   4. Each MPI task should output its own set of timings either
   to separate files or with a clear task identifier for each set if
   they’re written to the same file.

   5. Timings should be collected up in arrays and output at the end
   of a run to avoid introducing delays for timer output during the run.

   6. Output should be some easily analyzable output ascii
   format. Think in terms of what could be handled using sed, awk, perl,
   and sort and also as input to Excel.  Csv might be a good choice.
   But don’t get hung up on this.  If it’s human readable we can
   manage.

***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

// adjust these if you need more
#define DO_NOT_EXCEED 1048576
#define MAXZONES 10

// enable this to compile as a standalone test program. Must run on 4 MPI tasks.
//#define TESTMAIN

int master_sequence_number = 1 ;
int verbose = 0 ;
int avec_timer_taskid ;
long timers_started_at ;

long
AVEC_MILLICLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    long isec ;  /* seconds */
    long usec ;  /* microseconds */
    long msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000 * isec + usec / 1000 ;
    return(msecs) ;
}
long 
AVEC_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    long isec ;  /* seconds */
    long usec ;  /* microseconds */
    long msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
//fprintf(stderr,"sizes %d %d\n",sizeof(tb.tv_sec),sizeof(tb.tv_usec)) ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}


typedef struct interval {
  long start_time ;
  long stop_time ;
  int master_sequence_number ;
} interval_t ;

typedef struct zone {
  int zoneid ;
  int zone_sequence_number ;
  int maxintervals ;
  interval_t * intervals ;
  char name[32] ;
  char state ;
} zone_t ;

zone_t zones[MAXZONES] ;

void
avec_timer_start_private ( int zoneid, int tid )
{
  if ( zoneid >= 0 && zoneid < MAXZONES ) {
    if ( zones[zoneid].zoneid != -99 ) {
      if ( zones[zoneid].state == 0 ) {
        master_sequence_number++ ;
        if ( zones[zoneid].zone_sequence_number <  zones[zoneid].maxintervals ) {
          zones[zoneid].intervals[zones[zoneid].zone_sequence_number].start_time = AVEC_MICROCLOCK () ;
          zones[zoneid].intervals[zones[zoneid].zone_sequence_number].master_sequence_number = master_sequence_number ;
        }
        zones[zoneid].state = 1 ;
      }
      else {
        if ( verbose ) fprintf(stderr,"AVEC_TIMER: WARNING: timer for zone %d started when already started\n",zoneid) ;
      }
    } else {
      if ( verbose ) fprintf(stderr,"AVEC_TIMER start: WARNING: invalid zone id %d\n",zoneid) ;
    }
  } else {
    if ( verbose ) fprintf(stderr,"AVEC_TIMER start: WARNING: invalid zone id %d\n",zoneid) ;
  }
}

void
avec_timer_stop_private ( int zoneid, int tid )
{
  if ( zoneid >= 0 && zoneid < MAXZONES ) {
    if ( zones[zoneid].zoneid != -99 ) {
      if ( zones[zoneid].state == 1 ) {
        master_sequence_number++ ;
        if ( zones[zoneid].zone_sequence_number <  zones[zoneid].maxintervals ) {
          zones[zoneid].intervals[zones[zoneid].zone_sequence_number].stop_time = AVEC_MICROCLOCK () ;
          zones[zoneid].intervals[zones[zoneid].zone_sequence_number].master_sequence_number = master_sequence_number ;
          zones[zoneid].zone_sequence_number++ ;  /* ZONE SEQUENCE NUMBER INCREMENTED IN STOP */
        }
        zones[zoneid].state = 0 ;
      }
      else {
        if ( verbose ) fprintf(stderr,"AVEC_TIMER: WARNING: timer for zone %d stopped but not started\n",zoneid) ;
      }
    } else {
      if ( verbose ) fprintf(stderr,"AVEC_TIMER stop: WARNING: invalid zone id %d\n",zoneid) ;
    }
  } else {
    if ( verbose ) fprintf(stderr,"AVEC_TIMER stop: WARNING: invalid zone id %d\n",zoneid) ;
  }
}

void
avec_timer_start_thread_ ( int * zoneid, int * tid )
{
  avec_timer_start_private( *zoneid, *tid ) ;
}
void
avec_timer_stop_thread_ ( int * zoneid, int * tid )
{
  avec_timer_stop_private( *zoneid, *tid ) ;
}
void
avec_timer_start_ ( int * zoneid )
{
  avec_timer_start_private( *zoneid, 0 ) ;
}
void
avec_timer_stop_ ( int * zoneid )
{
  avec_timer_stop_private( *zoneid, 0 ) ;
}

void
stripnl(char * s )
{
  char *p ;
  for ( p = s ; *p ; p++ ){if (*p == '\n') *p = '\0' ; }
}

void
avec_timer_init_ ( int * taskid_p, int * retval )
{
  FILE *fp ;
  int maxthreads ;
  char inlne[256] ;
  char *p, *tokens[32] ;
  int i, zoneid, maxintervals ;

  avec_timer_taskid = *taskid_p ;
  maxthreads = 1 ;
  *retval = 0 ;
  if (( fp = fopen("avec_timers_config.txt","r" )) == NULL )
  {
    *retval = 1 ;
    if ( avec_timer_taskid == 0 ) fprintf(stderr,"AVEC_TIMER_LIB: cannot open avec_timers_config.txt\n") ;
    return ;
  }
  for ( i = 0 ; i < MAXZONES ; i++ ) {
    zones[i].zoneid = -99 ;
  }
  while ( fgets( inlne, 256, fp ) != NULL ) {
    stripnl(inlne) ;
    p = (char *)strtok( inlne, " \t" ) ;
    i = 0 ;
    while ( p != NULL && i < 32 ) { 
       tokens[i++] = p ;
       p = (char *)strtok( NULL, " \t" ) ;
    }
    if ( i == 1 )
    {
      if (!strcmp( tokens[0], "verbose" )) {
        verbose = 1 ;
      }
    }
//    if ( i == 2 )
//    {
//      if (!strcmp( tokens[0], "maxintervals" )) {
//        maxintervals = atoi( tokens[1] ) ;
//      }
//    }
// <zone> <number> <name> <maxintervals>
    if ( i == 4 )
    {
      if (!strcmp( tokens[0], "zone" )) {
        zoneid = atoi(tokens[1]) ;
        if ( zoneid >= 0 && zoneid < MAXZONES ) {
          if ( zones[zoneid].zoneid == -99 ) {
            maxintervals = atoi(tokens[3]) ;
            if ( maxintervals < 0 || maxintervals > DO_NOT_EXCEED ) {
              if ( avec_timer_taskid == 0 ) 
                 fprintf(stderr,"AVEC_TIMER_LIB: maxintervals (%d) too large or small, adustig to ",maxintervals) ;
              if ( maxintervals < 0 ) maxintervals = 0 ;
              if ( maxintervals > DO_NOT_EXCEED ) maxintervals = DO_NOT_EXCEED ;
              if ( avec_timer_taskid == 0 ) 
                 fprintf(stderr,"%d\n",maxintervals) ; 
            }
            zones[zoneid].maxintervals = maxintervals ;
            zones[zoneid].zone_sequence_number = 0 ;
            zones[zoneid].zoneid = zoneid ;
            zones[zoneid].state = 0 ;
            zones[zoneid].intervals = (interval_t *)malloc(maxintervals*sizeof(interval_t)) ;
            strcpy( zones[zoneid].name, tokens[2]) ;
          }
        }
      }
    }
  }
  fclose(fp) ;
//
  if ( verbose ) {
    fprintf(stderr,"maxintervals = %d\n", maxintervals ) ;
    for ( i = 0 ; i < MAXZONES ; i++ )
    {
      if ( zones[i].zoneid != -99 ) {
        fprintf(stderr,"AVEC_TIMERS: zone %3d name %s\n", zones[i].zoneid, zones[i].name ) ;
      }
    }
    if (maxthreads > 1 ) {
      fprintf(stderr,"AVEC_TIMERS: WARNING: only single threaded supported now\n") ;
    }
  }
  timers_started_at = AVEC_MICROCLOCK() ;
}


void
avec_timers_output_()
{
  FILE *fp ;
  char fname[256] ;
  int i, j ;
  sprintf(fname,"avec_timer_out_%05d.txt",avec_timer_taskid) ;
  if ((fp=fopen(fname,"w"))!=NULL)
  {
    fprintf(fp,"timer reading at initialization: %ld\n",timers_started_at) ;
    fprintf(fp,"taskid,zoneid,zonename,zonesequencenumber,mastersequencenumber,time,starttime,endtime\n"); 
    for ( j = 0 ; j < MAXZONES ; j++ ) {
      if ( zones[j].zoneid != -99 ) {
        for ( i = 0 ; i < zones[j].zone_sequence_number ; i++ ) {
          fprintf(fp,"%d,%d,%s,%d,%d,%ld,%ld,%ld\n",avec_timer_taskid,zones[j].zoneid,zones[j].name,
                                           i, zones[j].intervals[i].master_sequence_number,
                                           zones[j].intervals[i].stop_time-zones[j].intervals[i].start_time,
                                           zones[j].intervals[i].start_time,zones[j].intervals[i].stop_time ) ;
        }
        free(zones[j].intervals) ;
      }
    }
  } else {
    if ( verbose ) fprintf(stderr,"AVEC_TIMER: unable to open %s for writing\n",fname) ;
  }
  if (fp != NULL) fclose(fp) ;
}

//
//  avec_timer_init_( &taskid, &ierr ) ;

#ifdef TESTMAIN
///////////////////////// test program ////////////////////////////
//from: http://people.sc.fsu.edu/~jburkardt/c_src/laplace_mpi/laplace_mpi.c
/* use this avec_timers_config.txt file:
#verbose
zone 5 exchange        1000
zone 6 packunpack      2000
zone 7 timestep        1000
zone 2 initialization  3
zone 4 dored           1000
zone 3 doblack         1000
zone 1 interate        3
*/
// defined above
//# include <stdlib.h>
//# include <stdio.h>
# include <math.h>

# include "mpi.h"

# define n 48            /* matrix is nxn, excluding boundary values     */
# define nodeedge 24     /* a task works on a nodeedge x nodeedge matrix */
# define nblock n/nodeedge   /* number of tasks per row of matrix            */
# define nproc nblock*nblock /* total number of tasks (processors)           */

int main ( int argc, char **argv );
void doblack ( double w, double M[][nodeedge+2] );
void dored ( double w, double M[][nodeedge+2] );
void exchange ( double M[][nodeedge+2], int comm[], int rank );
void iterate ( double w, double M[][nodeedge+2], double result[][n], int rank, int comm[] );
void setcomm ( int rank, int comm[] );
void setex ( double ex[], double M[][nodeedge+2], int which );
void initialize_matrix ( double M[][nodeedge+2] );
void unpack ( double M[][nodeedge+2], int where, double in[] );

/******************************************************************************/

int main ( int argc, char **argv )

/******************************************************************************/
/*
  Purpose:

    LAPLACE_MPI solves Laplace's equation on a rectangle, using MPI.

  Discussion:

    This program uses a finite difference scheme to solve
    Laplace's equation for a square matrix distributed over a square
    (logical) processor topology.  A complete description of the algorithm
    is found in Fox.

    This program works on the SPMD (single program, multiple data)
    paradigm.  It illustrates 2-d block decomposition, nodes exchanging
    edge values, and convergence checking.

    Each matrix element is updated based on the values of the four
    neighboring matrix elements.  This process is repeated until the data
    converges, that is, until the average change in any matrix element (compared
    to the value 20 iterations previous) is smaller than a specified value.

    To ensure reproducible results between runs, a red/black
    checkerboard algorithm is used.  Each process exchanges edge values
    with its four neighbors.  Then new values are calculated for the upper
    left and lower right corners (the "red" corners) of each node's
    matrix.  The processes exchange edge values again.  The upper right
    and lower left corners (the "black" corners) are then calculated.

    The program is currently configured for a 48x48 matrix
    distributed over four processors.  It can be edited to handle
    different matrix sizes or number of processors, as long as the matrix
    can be divided evenly between the processors.

  Modified:

    14 November 2011

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Reference:

    Geoffrey Fox, Mark Johnson, Gregory Lyzenga, Steve Otto, John Salmon, 
    David Walker,
    Solving Problems on Concurrent Processors,
    Volume 1: General Techniques and Regular Problems, 
    Prentice Hall, 1988,
    ISBN: 0-13-8230226,
    LC: QA76.5.F627.

  Local parameters:

    Local, int COMM[4], contains a 0 (no) or 1 (yes) if
    communication is needed for the UP(0), RIGHT(1), DOWN(2)
    and LEFT(3) neighbors.

    Local, FILE *fp, a pointer to the output file.

    Local, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.

    Local, double RESULT[n][n], the results for the complete problem,
    kept by process 0.

    Local, double W, the SOR factor, which must be strictly between 0 and 2.
*/ 
{
  int comm[4];
  FILE *fp;
  int i;
  int j;
  double M[nodeedge+2][nodeedge+2];
  int ntasks;
  int rank;
  double result[n][n];
  double w;
  double wtime;
  int ierr ; // JM AVEC

  MPI_Init ( &argc, &argv );

  MPI_Comm_rank ( MPI_COMM_WORLD, &rank );
  avec_timer_init_( &rank, &ierr ) ; // JM AVEC

  MPI_Comm_size ( MPI_COMM_WORLD, &ntasks );

  wtime = MPI_Wtime ( );

  if ( rank == 0 ) 
  {
    printf ( "\n" );
    printf ( "LAPLACE_MPI:\n" );
    printf ( "  C/MPI version\n" );
    printf ( "  Solve the Laplace equation using MPI.\n" );
  }

  if ( ntasks != nproc )
  {
    if ( rank == 0 ) 
    {
      printf ( "\n" );
      printf ( "Fatal error!\n" );
      printf ( "  MP_PROCS should be set to %i!\n", nproc );
    }
    MPI_Finalize ( );
    exit ( 1 );
  }

  if ( rank == 0 ) 
  {
    printf ( "\n" );
    printf ( "  MPI has been set up.\n" );
  }
/* 
  Initialize the matrix M.
*/

{ int z = 2 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

  if ( rank == 0 ) 
  {
    printf ( "  Initialize the matrix M.\n" );
  }
  initialize_matrix ( M );
/* 
  Figure out who I communicate with.
*/
  if ( rank == 0 ) 
  {
    printf ( "  Set the list of neighbors.\n" );
  }
  setcomm ( rank, comm );
/* 
  Update M, using SOR value W, until convergence.
*/

{ int z = 2 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

  if ( rank == 0 ) 
  {
    printf ( "  Begin the iteration.\n" );
  }
  w = 1.2;

{ int z = 1 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

  iterate ( w, M, result, rank, comm );

{ int z = 1 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

/* 
  Report timing 
*/ 
  wtime = MPI_Wtime ( ) - wtime;

  printf ( "  Task %i took %6.3f seconds\n", rank, wtime );
/*
  Write the solution to a file.
*/
  if ( rank == 0 )
  {
    fp = fopen ( "laplace_solution.txt", "w" );

    for ( i = 0; i < n; i++ ) 
    {
      for ( j = 0; j < n; j++ )
      {
        fprintf ( fp, "%f \n", result[i][j] );
      }  
    }
    fclose ( fp );
    printf ( "  Solution written to \"laplace_solution.txt\".\n" );
  }
/*
  Terminate MPI.
*/
  MPI_Finalize ( );
/*
  Terminate.
*/
  if ( rank == 0 )
  {
    printf ( "\n" );
    printf ( "LAPLACE_MPI:\n" );
    printf ( "  Normal end of execution.\n" );
  }
  avec_timers_output_() ; // JM AVEC 
  return 0;
}
/******************************************************************************/

void doblack ( double w, double M[][nodeedge+2] )

/******************************************************************************/
/*
  Purpose:

    DOBLACK iterates on the upper right and lower left corners of my matrix.

  Modified:

    16 February 2013

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Input, double W, the SOR factor, which must be strictly between 0 and 2.

    Input/output, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.
*/
{
  int i;
  int j;
/*
  Upper right corner.
*/

{ int z = 3 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

  for ( i = 1; i <= nodeedge / 2; i++ )
  {
    for ( j = nodeedge / 2 + 1; j <= nodeedge; j++ )
    {
      M[i][j] = w / 4.0 * ( M[i-1][j] + M[i][j-1] + M[i+1][j] + M[i][j+1] )
        + ( 1.0 - w ) * M[i][j];
    }
  }
/*
  Lower left corner.
*/
  for ( i = nodeedge / 2 + 1; i <= nodeedge; i++ )
  {
    for ( j = 1; j <= nodeedge / 2; j++ )
    {
      M[i][j] = w / 4.0 * ( M[i-1][j] + M[i][j-1] + M[i+1][j] + M[i][j+1] )
        + ( 1.0 - w ) * M[i][j];
    }
  }

{ int z = 3 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

  return;
}
/******************************************************************************/

void dored ( double w, double M[][nodeedge+2] )

/******************************************************************************/   
/*
  Purpose:

    DORED iterates on the upper left and lower right corners of my matrix.

  Modified:

    16 February 2013

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Input, double W, the SOR factor, which must be strictly between 0 and 2.

    Input/output, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.
*/  
{
  int i;
  int j;

{ int z = 4 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

/*
  Upper left corner.
*/
  for ( i = 1; i <= nodeedge / 2; i++ )
  {
    for ( j = 1; j <= nodeedge / 2; j++ ) 
    {
      M[i][j] = w / 4.0 * ( M[i-1][j] + M[i][j-1] + M[i+1][j] + M[i][j+1] )
        + ( 1.0 - w ) * M[i][j];
    }
  }
/*
  Lower right corner.
*/
  for ( i = nodeedge / 2 + 1; i <= nodeedge; i++ )
  {
    for ( j = nodeedge / 2 + 1; j <= nodeedge; j++ )
    {
      M[i][j] = w / 4.0 * ( M[i-1][j] + M[i][j-1] + M[i+1][j] + M[i][j+1] )
        + ( 1.0 - w ) * M[i][j];
    }
  }

{ int z = 4 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

  return;
}
/******************************************************************************/

void exchange ( double M[][nodeedge+2], int comm[], int rank )

/******************************************************************************/
/*
  Purpose:

   EXCHANGE trades edge values with up to four neighbors.

  Discussion:

    Up to 4 MPI sends are carried out, and up to 4 MPI receives.

  Modified:

    14 November 2011

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Input/output, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.

    Input, int COMM[4], contains a 0 (no) or 1 (yes) if
    communication is needed for the UP(0), RIGHT(1), DOWN(2)
    and LEFT(3) neighbors.

    Input, int RANK, the rank of this process.
*/
{
  double ex0[nodeedge];
  double ex1[nodeedge];
  double ex2[nodeedge];
  double ex3[nodeedge];
  int i;
  double in0[nodeedge];
  double in1[nodeedge];
  double in2[nodeedge];
  double in3[nodeedge];
  int partner;
  MPI_Request requests[8];
  MPI_Status status[8];
  int tag;

{ int z = 5 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

/* 
  Initialize requests.
*/
  for ( i = 0; i < 8; i++ ) 
  {
    requests[i] = MPI_REQUEST_NULL; 
  }
/* 
  Receive from UP neighbor (0).
*/
  if ( comm[0] == 1 )
  {
    partner = rank - nblock;
    tag = 0;
    MPI_Irecv ( &in0, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD, 
      &requests[0] );
  }
/*
  Receive from RIGHT neighbor (1).
*/
  if ( comm[1] == 1 )
  {
    partner = rank + 1;
    tag = 1;
    MPI_Irecv ( &in1, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[1] );
  }
/*
  Receive from DOWN neighbor (2).
*/
  if ( comm[2] == 1 )
  {
    partner = rank + nblock;
    tag = 2;
    MPI_Irecv ( &in2, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[2] );
  }
/*
  Receive from LEFT neighbor (3).
*/
  if ( comm[3] == 1 )
  {
    partner = rank - 1;
    tag = 3;
    MPI_Irecv ( &in3, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[3] );
  }
/*
  Send up from DOWN (2) neighbor.
*/
  if ( comm[0] == 1 )
  {
    partner = rank - nblock;
    tag = 2;
    setex ( ex0, M, 0 );
    MPI_Isend ( &ex0, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[4] );
  }
/*
  Send right form LEFT (3) neighbor.
*/
  if (comm[1] == 1 )
  {
    partner = rank + 1;
    tag = 3;
    setex ( ex1, M, 1 );
    MPI_Isend ( &ex1, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[5] );
  }
/*
  Send down from UP (0) neighbor.
*/
  if ( comm[2] == 1 )
  {
    partner = rank + nblock;
    tag = 0;
    setex ( ex2, M, 2 );
    MPI_Isend ( &ex2, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[6] );
  }
/*
  Send left from RIGHT (1) neighbor.
*/
  if ( comm[3] == 1 )
  {
    partner = rank - 1;
    tag = 1;
    setex ( ex3, M, 3 );
    MPI_Isend ( &ex3, nodeedge, MPI_DOUBLE, partner, tag, MPI_COMM_WORLD,
      &requests[7] );
  }
/* 
  Wait for all communication to complete.
*/ 
  MPI_Waitall ( 8, requests, status );
/*
  Copy boundary values, sent by neighbors, into M.
*/

{ int z = 6 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

  if ( comm[0] == 1 ) 
  {
    unpack ( M, 0, in0 );
  }
  if ( comm[1] == 1 ) 
  {
    unpack ( M, 1, in1 );
  }
  if ( comm[2] == 1 ) 
  {
    unpack ( M, 2, in2 );
  }
  if ( comm[3] == 1 ) 
  {
    unpack ( M, 3, in3 );
  }

{ int z = 6 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC
{ int z = 5 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

  return;
}
/******************************************************************************/

void initialize_matrix ( double M[][nodeedge+2] )

/******************************************************************************/
/*
  Purpose:

    INITIALIZE_MATRIX initializes the partial results array M.

  Modified:

    10 January 2012

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Output, double M[nodeedge+2][nodeedge+2], the initialized partial 
    results array.
*/
{
  double avg;
  double bv[4];
  int i;
  int j;

  bv[0] = 100.0;
  bv[1] = 0.0;
  bv[2] = 0.0;
  bv[3] = 0.0;
/* 
  Put the boundary values into M.
*/ 
  for ( i = 1; i <= nodeedge; i++ )
  { 
    M[0][i] =          bv[0];
    M[i][nodeedge+1] = bv[1];
    M[nodeedge+1][i] = bv[2];
    M[i][0] =          bv[3];
  }
/* 
  Set all interior values to be the average of the boundary values.
*/ 
  avg = ( bv[0] + bv[1] + bv[2] + bv[3] ) / 4.0;

  for ( i = 1; i <= nodeedge; i++ )
  {
    for ( j = 1; j <= nodeedge; j++ )
    {
      M[i][j] = avg;
    }
  }

  return;
}
/******************************************************************************/

void iterate ( double w, double M[][nodeedge+2], double result[][n], int rank, 
  int comm[] )

/******************************************************************************/
/*
  Purpose:

    ITERATE controls the iteration, including convergence checking.

  Modified:

    16 February 2013

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Input, double W, the SOR factor, which must be strictly between 0 and 2.

    Input/output, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.

    Output, double RESULT[n][n], the results for the complete problem,
    kept by process 0.

    Input, int RANK, the rank of the process.

    Input, int COMM[4], contains a 0 (no) or 1 (yes) if
    communication is needed for the UP(0), RIGHT(1), DOWN(2)
    and LEFT(3) neighbors.

  Local parameters:

    Local, int COUNT, the length, in elements, of messages.

    Local, double DIFF, the average absolute difference in elements
    of M since the last comparison.

    Local, int IT, the iteration counter.

    Local, double MM[n*n], a vector, used to gather the data from
    all processes.  This data is then rearranged into a 2D array.
*/
{
  int count;
  double diff;
  int done;
  double ediff;
  int i;
  double in;
  int index;
  int it;
  int j;
  int k;
  int l;
  double MM[n*n];
  double mold[nodeedge+2][nodeedge+2];
  double send[nodeedge][nodeedge];

  it = 0;
  done = 0;
  for ( i = 1; i <= nodeedge; i++ )
  {
    for ( j = 1; j <= nodeedge; j++ )
    {
      mold[i][j] = M[i][j];
    }
  }

  while ( done == 0 )
  {
    it++;
/*
  Exchange values with neighbors, update red squares, exchange values
  with neighbors, update black squares.
*/
    exchange ( M, comm, rank );
    dored ( w, M );
    exchange ( M, comm, rank );
    doblack ( w, M );
/*
  Check for convergence every 20 iterations.
  Find the average absolute change in elements of M.
  Maximum iterations is 5000.
*/

{ int z = 7 ; avec_timer_start_ ( &z ) ; } ; // JM AVEC

    if ( 5000 < it )
    {
      done = 1;
    }

//    if ( ( it % 20.0 == 0.0 ) && ( done != 1 ) )
    if ( ( it % 20 == 0 ) && ( done != 1 ) )
    { 
      diff = 0.0;
      for ( i = 1; i <= nodeedge; i++ )
      {
        for ( j = 1; j <= nodeedge; j++ )
        {
          ediff = M[i][j] - mold[i][j];
          if ( ediff < 0.0 ) 
          {
            ediff = - ediff;
          }
          diff = diff + ediff;
          mold[i][j] = M[i][j];
        }
      }
      diff = diff / ( ( double ) ( nodeedge * nodeedge ) );
/*
  IN = sum of DIFF over all processes.
*/
      MPI_Allreduce ( &diff, &in, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD );

      if ( in < ( double ) nproc * 0.001 ) 
      {
        done = 1;
      }
    }

{ int z = 7 ; avec_timer_stop_ ( &z ) ; } ; // JM AVEC

  }
/* 
  Send results to task 0.
*/ 
  for ( i = 0; i < nodeedge; i++ )
  {
    for ( j = 0; j < nodeedge; j++ )
    {
      send[i][j] = M[i+1][j+1];
    }
  }

  count = nodeedge * nodeedge;

  MPI_Gather ( &send, count, MPI_DOUBLE, &MM, count, MPI_DOUBLE, 0, 
    MPI_COMM_WORLD );

  printf ( "  ITERATE gathered updated results to process 0.\n" );
/* 
  Storage on task 0 has to be consistent with a NBLOCK x NBLOCK decomposition.

  I believe the array form of RESULT is only needed at the end of the
  program (and not even then, really).  So we could probably skip this
  work of rearranging the data here.  JVB, 11 January 2012.
*/
  if ( rank == 0 ) 
  {
    printf ( "did %i iterations\n", it );

    index = 0;
    for ( k = 0; k < nblock; k++ )
    {
      for ( l = 0; l < nblock; l++ )
      {
        for ( i = k * nodeedge; i < ( k + 1 ) * nodeedge; i++ )
        {
          for ( j = l * nodeedge; j < ( l + 1 ) * nodeedge; j++ )
          {
            result[i][j] = MM[index];
            index++;
          }
        }
      }
    }
  }
  return;
}
/******************************************************************************/

void setcomm ( int rank, int comm[] )

/******************************************************************************/
/*
  Purpose:

    SETCOMM determines the active communication directions.

  Discussion:

    In this picture, we're assuming the RESULTS array is split among 
    four processes, numbered 0 through 3 and arranged as suggested by the 
    following:

        0  |  1
     ------+-------
        2  |  3

    Then process 0 must communicate with processes 1 and 2 only,
    so its COMM array would be { 0, 1, 1, 0 }.

  Modified:

    14 November 2011

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Input, int RANK, the rank of the process.

    Output, int COMM[4], contains a 0 (no) or 1 (yes) if
    communication is needed for the UP(0), RIGHT(1), DOWN(2)
    and LEFT(3) neighbors.
*/
{
  int i;
/*
  Start out by assuming all four neighbors exist.
*/
  for ( i = 0; i < 4; i++ ) 
  {
    comm[i] = 1;
  }
/*
  Up neighbor?
*/
  if ( rank < nblock )
  {
    comm[0] = 0;    
  }
/*
  Right neighbor?
*/
  if ( ( rank + 1 ) % nblock == 0 )
  {
    comm[1] = 0;
  }
/*
  Down neighbor?
*/
  if ( rank > (nblock*(nblock-1)-1) )
  {
    comm[2] = 0;
  }
/*
  Left neighbor?
*/
  if ( ( rank % nblock ) == 0 )
  {
    comm[3] = 0;
  }

  return;
}
/******************************************************************************/

void setex ( double ex[], double M[][nodeedge+2], int which )

/******************************************************************************/
/*
  Purpose:

    SETEX pulls off the edge values of M to send to another task.

  Modified:

    14 November 2011

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Output, double EX[NODEEDGE], the values to be exchanged.

    Input, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process. 

    Input, int WHICH, 0, 1, 2, or 3, indicates the edge from which
    the data is to be copied.
*/                  
{
  int i;

  switch ( which ) 
  {
    case 0:
    {
      for ( i = 1; i <= nodeedge; i++) 
      {
        ex[i-1] = M[1][i];
      }
      break;
    }
    case 1:
    {
      for ( i = 1; i <= nodeedge; i++)
      {
        ex[i-1] = M[i][nodeedge];
      }
      break;
    }
    case 2:
    {
      for ( i = 1; i <= nodeedge; i++)
      {
        ex[i-1] = M[nodeedge][i];
      }
      break;
    }
    case 3:
    {
      for ( i = 1; i <= nodeedge; i++)
      {
        ex[i-1] = M[i][1];
      }
      break;
    }
  }
  return;
}
/******************************************************************************/

void unpack ( double M[][nodeedge+2], int where, double in[] )

/******************************************************************************/
/*
  Purpose:

    UNPACK puts the vector of new edge values into the edges of M.

  Modified:

    14 November 2011

  Author:

    Sequential C version by Robb Newman.
    MPI C version by Xianneng Shen.

  Parameters:

    Output, double M[nodeedge+2][nodeedge+2], the part of the results 
    kept by this process.

    Input, int WHERE, 0, 1, 2, or 3, indicates the edge to which the 
    data is to be applied.

    Input, int IN[nodeedge], the boundary data.
*/
{
  int i;

  if ( where == 0 )
  {
    for ( i = 0; i < nodeedge; i++ )
    {
      M[0][i+1] = in[i]; 
    }
  }
  else if ( where == 1 )
  {
    for ( i = 0; i < nodeedge; i++ )
    {
      M[i+1][nodeedge+1] = in[i];
    }
  }
  else if ( where == 2 )
  {
    for ( i = 0; i < nodeedge; i++ )
    {
      M[nodeedge+1][i+1] = in[i];
    }
  }
  else if ( where == 3 )
  {
    for ( i = 0; i < nodeedge; i++ )
    {
      M[i+1][0] = in[i];
    }
  }

  return;
}

#endif
