            INTEGER FUNCTION FFSYNC(fn)

            ! USAGE: rc=ffsync(fn) where fn is an input 
            !        FORTRAN file unit and rc is the return
            !        code given back to calling program 
            !  This function should be called just before closing
            !  a file to make sure everything flushed to disk.

            IMPLICIT NONE

            ! Declare the interface for POSIX fsync function
            interface
              function fsync (fd) bind(c,name="fsync")
              use iso_c_binding, only: c_int
                integer(c_int), value :: fd
                integer(c_int) :: fsync
              end function fsync
            end interface

            ! Variable declaration
            ! rc, return code to send back to caller from fsync
            integer :: rc
            ! fn, fortran file handle passed in to flush and sync
            integer :: fn  
            ! cfd :: C file descriptor returned from getfd
            integer :: cfd 
            ! getfd :: Fortran function to get C file descriptor 
            integer :: getfd
          
#ifdef IBMP6
            !Get file descriptor for interlanguage call to C fsync
            cfd=getfd(fn)

            ! Flush and sync
            flush(fn)

            !Call fsync
            rc=fsync(cfd)
#else
            rc = 0
#endif
            ! pass back return code
            ffsync=rc
            RETURN

            END FUNCTION FFSYNC
