      SUBROUTINE SETUP_SERVERS(MYPE,                         &
     &                         NPES,                         &
     &                         INUMQ,                        &
     &                         MPI_COMM_COMP,                &
     &                         MPI_COMM_INTER)
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
!   SUBROUTINE:  SETUP_SERVERS   SETUP I/O SERVERS      
!   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-03-20
!
! ABSTRACT:  SETUP I/O SERVERS
!
! PROGRAM HISTORY LOG:
!   00-03-11  TUCCILLO - ORIGINATOR
!
! USAGE:  CALL SETUP_SERVERS(MYPE,
!    *                       NPES,
!    *                       INUMQ,
!    *                       MPI_COMM_COMP,
!    *                       MPI_COMM_INTER)
!
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST:
!     MYPE - MY RANK
!     INUMQ - ARRAY THAT HOLDS THE NUMBER OF SERVERS IN EACH GROUP
!     NPES - NUMBER OF MPI TASKS FOR POSTING
!     MPI_COMM_COMP - THE NEW INTRACOMMUNICATOR FOR ALL TASKS
!     MPI_COMM_INTER - THE INTERCOMMUNICATOR FOR THE I/O SERVERS
!
!   INPUT FILES:  NONE
!
!   OUTPUT FILES:  
!
!   SUBPROGRAMS CALLED:
!     UNIQUE:
!            PARA_RANGE
!            MPI_INIT
!            MPI_COMM_RANK
!            MPI_COMM_SIZE
!            MPI_COMM_DUP
!            MPI_COMM_SPLIT
!            MPI_COMM_GROUP
!            MPI_GROUP_EXCL
!            MPI_COMM_CREATE
!            MPI_GROUP_FREE
!            MPI_INTERCOMM_CREATE
!            MPI_BARRIER
!
!   EXIT STATES:
!     COND =   0 - NORMAL EXIT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      include 'mpif.h'

      integer,intent(out) :: MYPE,NPES,INUMQ,MPI_COMM_COMP,  &
              MPI_COMM_INTER
      integer comdup,ierr,npes_mod,iquilt_group,iqserver,   &
              istaq,iendq,icolor,istaxx,iendxx,ixx,irlr,icc,iss,issl, &
              jj,i,kk
      integer iworld,igroup,igroup_x,iworld_minus      
      integer, allocatable :: irank ( : )
      logical yes
      character*4 get
!-----------------------------------------------------------------------
!
!     INITIALIZE MPI
!     RETRIEVE THE NUMBER OF TOTAL MPI TASKS AND MY RANK
!
      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,mype,ierr)
      write(0,*)' mype=',mype,' ierr=',ierr
      call mpi_comm_size(MPI_COMM_WORLD,npes,ierr)
      write(0,*)' npes=',npes,' ierr=',ierr
!     
!     SPECIFY ONE I/O SERVER AS LONG AS THERE ARE MORE THAN 1 MPI TASK
!
      if ( npes > 1 ) then
!         npes_mod = npes - 1
         npes_mod = npes    ! turn off quilt
      else
         npes_mod = 1
      end if
!
!     AT THIS POINT NPES IS THE TOTAL NUMBER OF MPI TASKS. WE WILL
!     RESET THIS AT THE END OF THE SUBROUTINE TO THE NUMBER OF MPI
!     TASKS THAT ARE WORKING ON THE POSTING
!
!     FIRST, HOWEVER, WE NEED TO MAKE SURE THAT A SUFFICIENT NUMBER
!     OF MPI TASKS HAVE BEEN INITIATED. IF NOT, WE WILL STOP.
!
      IF ( NPES .LT. NPES_MOD ) THEN
         PRINT *, ' ***********************************************'
         PRINT *, ' ***********************************************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *
         PRINT *, ' THERE ARE INSUFFICIENT MPI TASKS TO CONTINUE'
         PRINT *, ' YOU MUST SPECIFY AT LEAST ',NPES_MOD,' TASKS'
         PRINT *, ' STOPPING NOW'
         PRINT *, ' HASTA LA VISTA BABY'
         PRINT *
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' *************MAJOR PROBLEM*********************'
         PRINT *, ' ***********************************************'
         PRINT *, ' ***********************************************'
         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
      END IF
!
!     OK, WE HAVE A SUFFICIENT NUMBER OF MPI TASKS TO CONTINUE
!
!     HOW MANY GROUPS OF SERVERS ? THE DEFAULT IS 1 GROUP
!     THE ENVIRONMENT VARIABLE, SERVER_GROUPS, CAN BE USED TO
!     SPECIFY MORE SERVER GROUPS
!
      get = '1'
!was  call getenv('SERVER_GROUPS',get)
      read(get,fmt='(i4)') iquilt_group
      iquilt_group = max(iquilt_group,1)
!
!     ERROR CHECK NUMBER OF GROUPS - THE MAXIMUM IS 100 - THIS IS A LOT
!
      if ( iquilt_group > 100 ) then
         print *, ' ***** IQUILT_GROUP IS GREATER THAN 100'
         print *, ' ***** DO YOU REALLY WANT THIS ?'
         print *, ' ***** IF SO THEN INCREASE SIZE IN mpp.h'
         print *, ' ***** ALSO, CHANGE IF CHECK IN SETUP_SERVERS'
         print *, ' ***** RESETTING THE NUMBER OF SERVER GROUPS TO 100'
         print *, ' ***** WE ARE CONTINUING ....   '
         iquilt_group = 100
      end if
      if ( mype .eq. 0 ) then
      print *, ' we will try to run with ',iquilt_group,' server groups'
      end if
!
!     COMPUTE THE NUMBER OF SERVERS PER GROUP
!     ALL MPI TASKS BEYOND NPES_MOD WILL BE SERVERS
!     IF THE NUMBER OF SERVERS IS NOT EQUALLY DIVISIBLE BY
!     THE NUMBER OF GROUPS OF SERVERS THEN SOME GROUPS MAY HAVE
!     MORE SERVERS THEN OTHERS - THIS IS FINE
!     NOTE THAT WE REQUIrE AT LEAST ONE SERVER PER GROUP
!     WE MAY NEED TO REDUCE THE NUMBER OF SERVER GROUPS IF
!     IT EXCEEDS THE NUMBER OF SERVERS
!
      iqserver = NPES - NPES_MOD
      if ( iqserver == 0 ) then ! iquilt_group=0 for running with no quilt
         if ( mype == 0 ) then
           print *, ' *** you specified 0 I/O servers '
           print *, ' CHKOUT will write a file'
         end if
         iquilt_group = 0
         inumq = 0
      else ! iquilt_group=1 for running with 1 quilt
         call para_range(1,iqserver,1,0,istaq,iendq)
         inumq = iendq-istaq+1
         if ( mype == 0 ) print *, ' i, inumq = ',i+1,inumq
      end if
      if ( iquilt_group > iqserver )  then
          iquilt_group = iqserver
          print *, ' ***** NOT ENOUGH SERVERS'
          print *, ' ***** WE NEED TO REDUCE THE NUMB OF SERVER GROUPS'
          print *, ' ***** NUMB OF SERVER GROUPS IS ', iquilt_group
      end if

!      do i = 0, iquilt_group - 1
!         call para_range(1,iqserver,iquilt_group,i,istaq,iendq)
!         inumq(i+1) = iendq-istaq+1
!      if ( mype == 0 ) print *, ' i, inumq = ',i+1,inumq(i+1)
!      end do
      
      
!
!     SETUP THE "COLOR" FOR MPI_COMM_SPLIT
!     THOSE TASKS WHICH WILL DO MODEL INTEGRATION WILL BE COLOR 0
!     THE SERVER TASKS WILL HAVE THE COLOR OF THE GROUP NUMBER THAT
!     THEY WILL BELONG
!
      if ( mype < NPES_MOD ) then
         icolor = 0
      else 
         istaxx = NPES_MOD
!         do i = 1, 1  ! modification for using only one quilt server group
         iendxx = istaxx + inumq - 1
         if ( mype >= istaxx .and. mype <= iendxx ) then
            icolor = 1
         end if
!         istaxx = iendxx + 1
!         end do
      end if
      print *,'mype=',mype,'icolor=',icolor
!
!     SPLIT THE COMMUNICATOR - THE NEW INTRACOMMUNICATOR FOR ALL TASKS
!     IS MPI_COMM_COMP. MPI_COMM_WORLD IS STILL AVAILABLE BUT IT DOES
!     REFER TO ALL THE MPI TASKS ( MODEL INTEGRATION AND I/O SERVING )
!        
      call mpi_comm_dup(MPI_COMM_WORLD,comdup,ierr)
      call mpi_comm_split(comdup,icolor,mype,mpi_comm_comp,ierr)
      print *,'mype=',mype,'npes=',npes,'after comm split'
!     
!     AT THIS POINT WE HAVE A NEW COMMUNICATOR, MPI_COMM_COMP,
!     THAT CAN BE USED BY THE FORECASTS TASKS AND THE I/O SERVER TASKS
!     FOR THEIR INTERNAL COMMUNICATIONS. ONTO THE INTERCOMMUNICATORS ...
!
!     NOW WE MUST CREATE THE INTERCOMMUNICATORS FOR USE BETWEEN THE MPI
!     TASKS DOING THE MODEL INTEGRATION AND THE MPI TASKS FOR EACH 
!     SERVER GROUP. THE FIRST STEP IS TO EXCLUDE THE TASKS THAT DONT
!     BELONG. WE WILL DO THIS FOR EACH SERVER GROUP BY EXCLUDING THE TASKS
!     FROM ALL OF THE OTHER SERVER GROUPS.
!
      allocate ( irank ( iqserver ) )
      ixx = NPES_MOD
      do i = 1, iquilt_group
         yes = .true.
         if ( mype < NPES_MOD ) then
            irlr = ixx
         else
            irlr = 0
         end if
         icc = 0
         iss = NPES_MOD
!     THIS IS THE FIRST POSSIBLE TASK ID THAT COULD BE EXCLUDED
         do jj = 1, iquilt_group
           if ( jj /= i ) then
            issl = iss
            do kk = 1, inumq
               icc = icc + 1
               irank(icc) = issl
               if ( mype == issl ) yes = .false.
               issl = issl + 1
            end do
           end if
           iss = iss + inumq
         end do
!
!     AT THIS POINT WE HAVE AN ARRAY, IRANK, WITH TASK IDS TO EXCLUDE
!     THERE ARE ICC OF THEM.
!     CREATE A NEW GROUP WITH THE TASKS FROM THE OTHER SERVER GROUPS
!     EXCLUDED AND THEN CREATE A NEW COMMUNICATOR ( IWORLD_MINUS ) THAT
!     CONTAINS ONLY THE MPI TASKS DOING THE MODEL INTEGRATION AND THE
!     TASKS THAT BLONG TO THE SERVER GROUP WE ARE CONSIDERING.
!   
        iworld = MPI_COMM_WORLD
        call mpi_comm_group(iworld,igroup,ierr)
        call mpi_group_excl(igroup,icc,irank,igroup_x,ierr)
        call mpi_comm_create(iworld,igroup_x,iworld_minus,ierr)
        call mpi_group_free(igroup,ierr)
        call mpi_group_free(igroup_x,ierr)
!
!     AT THIS POINT WE HAVE A COMMUNICATOR THAT EXCLUDES THE TASKS WE DONT WANT.
!     CREATE AN INTERCOMMUNICATOR FOR USE BETWEEN THE MPI TASKS DOING THE MODEL
!     INTEGRATION AND THE I/O SERVER GROUP WE ARE CONSIDERING. THIS PROCESS IS
!     A COLLECTIVE ROUTINE SO IT CAN ONLY BE DONE BY THE TASKS THAT HAVE NOT 
!     BEEN EXCLUDED. SAVE THIS NEW COMMUNICATOR IN MPI_COMM_INTER FOR USE BY
!     THE TASKS THAT BELONG TO THE SERVER GROUP THAT WE ARE CONSIDERING. THE
!     TASKS THAT ARE PERFORMING THE MODEL INTEGRATION WILL REFERENCE
!     MPI_COMM_INTER_ARRAY() SINCE WE WILL NEED TO SELECT WHICH SERVER
!     GROUP WE WISH TO COMMUNICATE WITH.
!
        if ( yes ) then
         call mpi_intercomm_create(mpi_comm_comp,0,iworld_minus,irlr,0,   &
           mpi_comm_inter,ierr)
        end if
!
        call mpi_barrier(MPI_COMM_WORLD,ierr)
!
      end do     ! end do for loop over the number of server groups
!
!***
!***  NPES IS REALLY THE NUMBER OF TASKS WORKING ON THE MODEL INTEGRATION
!***
      NPES = NPES  - IQSERVER
      print *,'mype=',mype,'npes_new=',npes
!
      IF(MYPE.EQ.0) THEN
         print *, ' The Posting is using ',npes,' MPI task'
         print *, ' There are ',iqserver,' I/O servers'
      END IF
!***
      deallocate ( irank )
!
      END
