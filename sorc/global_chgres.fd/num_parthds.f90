 INTEGER FUNCTION NUM_PARTHDS()
 use omp_lib
!!$OMP PARALLEL
!  num_parthds = omp_get_num_threads()
 num_parthds = omp_get_max_threads()
!  num_parthds = 6
!  num_parthds = 4
!!$OMP END PARALLEL

 write(*,*)' NUM_PARTHDS=',num_parthds
 return
 END FUNCTION NUM_PARTHDS
