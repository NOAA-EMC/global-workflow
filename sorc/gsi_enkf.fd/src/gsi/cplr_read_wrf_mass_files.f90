module read_wrf_mass_files_mod
use abstract_read_wrf_mass_files_mod
  type, extends(abstract_read_wrf_mass_files_class) :: read_wrf_mass_files_class 
  contains
    procedure, pass(this) :: read_wrf_mass_files => read_wrf_mass_files_wrf
  end type read_wrf_mass_files_class 
contains
  subroutine read_wrf_mass_files_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_files   same as read_files, but for wrfmass
  !   prgmmr: parrish          org: np22                date: 2004-06-22
  !
  ! abstract: figure out available time levels of background fields for 
  !             later input. This is patterned after read_wrf_nmm_files.
  !
  ! program history log:
  !   2004-06-22  parrish, document
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-12-03  treadon - replace mpe_ibcast (IBM extension) with
  !                         standard mpi_bcast
  !   2005-03-30  treadon - reformat code (cosmetic changes only)
  !   2009-10-09  wu      - reset time reference (using iwinbgn and winlen...) in preparation for 4dvar
  !   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
  !   2010-11-14  whitaker - set nfldsfc = nfldsig
  !   2012-04-13  whitaker - read times from sigf file, instead of using
  !   regional_time. Using regional_time was causing ntguessig and hrdifsig to be
  !   set incorrectly.
  !   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
  !   
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
    use kinds, only: r_kind,r_single,i_kind
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
         ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
    use guess_grids, only: hrdifsig_all,hrdifsfc_all
    use gridmod, only: regional_fhr
    use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,nhr_assimilation
    use constants, only: zero,one,zero_single,r60inv
    use obsmod, only: iadate,time_offset
    implicit none
  
  ! Declare passed variables
    class(read_wrf_mass_files_class),intent(inout) :: this
    integer(i_kind),intent(in   ) :: mype

  
  ! Declare local parameters
    real(r_kind),parameter:: r0_001=0.001_r_kind
  
  ! Declare local variables
    logical(4) fexist
    character(6) filename
    integer(i_kind) in_unit
    integer(i_kind) i,j,iwan,npem1
    integer(i_kind) nhr_half
    integer(i_kind) nminanl,nmings,nming2,ndiff
    integer(i_kind),dimension(4):: idateg
    integer(i_kind),dimension(5):: idate5
    real(r_single) hourg4
    real(r_kind) hourg,temp,t4dv
    real(r_kind),dimension(202,2):: time_ges
  
    associate( this => this )
    end associate
  !-----------------------------------------------------------------------------
  ! Start read_wrf_mass_files here.
    nhr_half=nhr_assimilation/2
    if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1
    npem1=npe-1
  
    do i=1,202
       time_ges(i,1) = 999_r_kind
       time_ges(i,2) = 999_r_kind
    end do
  
  
  ! Let a single task query the guess files.
    if(mype==npem1) then
  
  !    Convert analysis time to minutes relative to fixed date
       call w3fs21(iadate,nminanl)
       write(6,*)'READ_wrf_mass_FILES:  analysis date,minutes ',iadate,nminanl
  
  !    Check for consistency of times from sigma guess files.
       in_unit=15
       iwan=0
       do i=0,99
          write(filename,100)i
  100     format('sigf',i2.2)
          inquire(file=filename,exist=fexist)
          if(fexist)then
             open(in_unit,file=filename,form='unformatted')
             read(in_unit) idate5
             close(in_unit)
             hourg4= regional_fhr
             hourg = hourg4
             call w3fs21(idate5,nmings)
             nming2=nmings+60*hourg
             write(6,*)'READ_wrf_mass_FILES:  sigma guess file, nming2 ',hourg,idate5,nming2
             t4dv=real((nming2-iwinbgn),r_kind)*r60inv
             if (l4dvar.or.l4densvar) then
                if (t4dv<zero .OR. t4dv>winlen) cycle
             else
                ndiff=nming2-nminanl
                if(abs(ndiff) > 60*nhr_half ) cycle
             endif
             iwan=iwan+1
             time_ges(iwan,1) = real((nming2-iwinbgn),r_kind)*r60inv
             time_ges(iwan+100,1)=i+r0_001
          end if
       end do
       time_ges(201,1)=one
       time_ges(202,1)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan 
                if(time_ges(j,1) < time_ges(i,1))then
                   temp=time_ges(i+100,1)
                   time_ges(i+100,1)=time_ges(j+100,1)
                   time_ges(j+100,1)=temp
                   temp=time_ges(i,1)
                   time_ges(i,1)=time_ges(j,1)
                   time_ges(j,1)=temp
                end if
             end do
             if(abs(time_ges(i,1)-time_offset) < r0_001)time_ges(202,1) = i
          end do
       end if
       time_ges(201,1) = iwan+r0_001
  
  !?????????????????????????????????????????????????????????????????????????
  !??????rewrite/remove code related to surface file, because in wrf mode???
  !?????????there is no surface file (see comment and temporary fix below)??
  !?????????????????????????????????????????????????????????????????????????
  
  !    Check for consistency of times from surface guess files.
       iwan=0
       do i=0,99
          write(filename,200)i
  200     format('sfcf',i2.2)
          inquire(file=filename,exist=fexist)
          if(fexist)then
             hourg4=zero_single !???????need to think about how wrf restart files define time.
                                !   ???? there appears to be no initial hour/forecast hour, only 
                                !   ???? the valid time of the file.
             idateg(4)=iadate(1); idateg(2)=iadate(2)
             idateg(3)=iadate(3); idateg(1)=iadate(4)
             hourg = hourg4
             idate5(1)=idateg(4); idate5(2)=idateg(2)
             idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=iadate(5)
             call w3fs21(idate5,nmings)
             nming2=nmings+60*hourg
             write(6,*)'READ_wrf_mass_FILES:  surface guess file, nming2 ',hourg,idateg,nming2
             ndiff=nming2-nminanl
             if(abs(ndiff) <= 60*nhr_half ) then
                iwan=iwan+1
                time_ges(iwan,2) = real((nming2-iwinbgn),r_kind)*r60inv
                time_ges(iwan+100,2)=i+r0_001
             end if
          end if
          if(iwan==1) exit
       end do
       time_ges(201,2)=one
       time_ges(202,2)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan 
                if(time_ges(j,2) < time_ges(i,2))then
                   temp=time_ges(i+100,2)
                   time_ges(i+100,2)=time_ges(j+100,2)
                   time_ges(j+100,2)=temp
                   temp=time_ges(i,2)
                   time_ges(i,2)=time_ges(j,2)
                   time_ges(j,2)=temp
                end if
             end do
             if(abs(time_ges(i,2)-time_offset) < r0_001)time_ges(202,2) = i
          end do
       end if
       time_ges(201,2) = iwan+r0_001
    end if
  
  ! Broadcast guess file information to all tasks
    call mpi_bcast(time_ges,404,mpi_rtype,npem1,mpi_comm_world,ierror)
  
    nfldsig   = nint(time_ges(201,1))
  !!nfldsfc   = nint(time_ges(201,2))
    nfldsfc   = nfldsig
  
  ! Allocate space for guess information files
    call create_gesfinfo
  
    do i=1,nfldsig
       ifilesig(i) = -100
       hrdifsig(i) = zero
    end do
  
    do i=1,nfldsfc
       ifilesfc(i) = -100
       hrdifsfc(i) = zero
    end do
  
  ! Load time information for sigma guess field sinfo into output arrays
    ntguessig = nint(time_ges(202,1)) 
    do i=1,nfldsig
       hrdifsig(i) = time_ges(i,1)
       ifilesig(i) = nint(time_ges(i+100,1))
       hrdifsig_all(i) = hrdifsig(i)
    end do
    if(mype == 0) write(6,*)'READ_wrf_mass_FILES:  sigma fcst files used in analysis  :  ',&
         (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig
    
    
  ! Load time information for surface guess field info into output arrays
    ntguessfc = nint(time_ges(202,2))
    do i=1,nfldsfc
       hrdifsfc(i) = time_ges(i,2)
       ifilesfc(i) = nint(time_ges(i+100,2))
       hrdifsfc_all(i) = hrdifsfc(i)
    end do
  
  ! Below is a temporary fix. The wrf_mass regional mode does not have a surface
  ! file.  Instead the surface fields are passed through the atmospheric guess
  ! file.  Without a separate surface file the code above sets ntguessig and 
  ! nfldsig to zero.  This causes problems later in the code when arrays for
  ! the surface fields are allocated --> one of the array dimensions is nfldsfc
  ! and it will be zero.  This portion of the code should be rewritten, but until
  ! it is, the fix below gets around the above mentioned problem.
  
    ntguessfc = ntguessig
  !!nfldsfc   = 1
    do i=1,nfldsfc
       hrdifsfc(i) = hrdifsig(i)
       ifilesfc(i) = ifilesig(i)
       hrdifsfc_all(i) = hrdifsfc(i)
    end do
    if(mype == 0) write(6,*)'READ_wrf_mass_FILES:  surface fcst files used in analysis:  ',&
         (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc
    
  
  ! End of routine
    return
  end subroutine read_wrf_mass_files_wrf
end module read_wrf_mass_files_mod
