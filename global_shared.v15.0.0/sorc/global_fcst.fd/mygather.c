#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>

int
my_gather( void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);

void
my_gather_( void *sendbuf, int *sendcnt, MPI_Fint *sendtype, void *recvbuf, int *recvcnt, MPI_Fint *recvtype, int *root, MPI_Fint *comm, int *ierr)
{
  MPI_Comm c_comm;
  MPI_Datatype c_sendt, c_recvt;

  c_comm = MPI_Comm_f2c(*comm);
  c_sendt = MPI_Type_f2c(*sendtype);
  c_recvt = MPI_Type_f2c(*recvtype);

  *ierr = my_gather(sendbuf, *sendcnt, c_sendt, recvbuf, *recvcnt, c_recvt, *root, c_comm);

  return;
}

int
my_gather( void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  int i, j, rv, chunksize = 6000000;
  int size, rank, nchunk, leftover;
  int *recvcnts=NULL, *displs=NULL;
  int stypesz;

  nchunk = sendcnt / chunksize;
  leftover = sendcnt - (nchunk * chunksize);

  MPI_Comm_rank( comm, &rank );
  MPI_Comm_size( comm, &size );
  MPI_Type_size(sendtype, &stypesz);

  if(rank==0) {
    recvcnts = (int*)malloc(size * sizeof(int));
    displs = (int*)malloc(size * sizeof(int));

    for(j=0;j<size;j++)
      recvcnts[j] = chunksize;
  }

  for(i=0;i<nchunk;i++) {
    if(rank==0)
      for(j=0;j<size;j++)
        displs[j] = (j*sendcnt) + (i*chunksize);

    rv = MPI_Gatherv( (char*)sendbuf+(i*chunksize*stypesz), chunksize, sendtype, recvbuf,
         recvcnts, displs, recvtype, root, comm);

    if(rv != MPI_SUCCESS) {
      free(recvcnts);
      free(displs);
      return rv;
    } 
  }

  if(leftover > 0) {
    if(rank==0) {
      for(j=0;j<size;j++) {
        recvcnts[j] = leftover;
        displs[j] = ((j+1)*sendcnt) - leftover;
      }
    }

    rv = MPI_Gatherv( (char*)sendbuf+(sendcnt-leftover)*stypesz, leftover, sendtype, recvbuf,
         recvcnts, displs, recvtype, root, comm);

    if(rv != MPI_SUCCESS) {
      free(recvcnts);
      free(displs);
      return rv;
    } 
  }

  if(rank==0) {
    free(recvcnts);
    free(displs);
  }

  return MPI_SUCCESS;
}
