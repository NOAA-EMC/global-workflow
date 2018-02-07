 integer function num_parthds()
 use omp_lib
!$OMP PARALLEL
 num_parthds=omp_get_num_threads()
!$OMP END PARALLEL
 return
 end function num_parthds
