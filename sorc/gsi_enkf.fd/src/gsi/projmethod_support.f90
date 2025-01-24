subroutine writeout_gradients(dx,dy,nv,alpha,gamma,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    writeout_gradients
!   prgmmr: pondeca           org: np23                  date: 2006-10-17
!
! abstract: writes out the x and y gradidents of the costfunction
!           also writes out B*y
!
! program history log:
!   2006-10-17  pondeca, document
!   2010-03-29  zhu      - make changes for generalizing control variables
!   2010-04-01  treadon - move strip to grimod
!   2010-04-29  todling - update to use control vectory based on gsi_bundle
!   2010-08-19  lueken  - add only to module use
!   2011-06-06  pondeca - add output for gust,vis,pblh,cldch
!   2011-07-03  todling - avoid explicit reference to internal bundle arrays
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-03-19  pondeca - add output for wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-18  carley/zhu - add tcamt and lcbas
!   2014-08-18  pondeca - add sfwter and vpwter
!   2016-03-07  pondeca  - add uwnd10m,vwnd10m
!
!   input argument list:
!     nv
!     mype
!     dx       - input vector 1
!     dy       - input vector 2
!     alpha
!     gamma
!
!   output argument list
!
!attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!*************************************************************************
  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use gridmod, only: nsig,lat1,lon1,lat2,lon2,nlon,nlat,itotsub,iglobal, & 
                     latlon11,ijn,displs_g,strip
  use general_commvars_mod, only: ltosi,ltosj
  use radinfo, only: npred,jpch_rad
  use pcpinfo, only: npredp,npcptype
  use jfunc, only: iter,jiter
  use gsi_bundlemod, only: gsi_bundlegetvar
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use control_vectors, only: control_vector,allocate_cv,deallocate_cv, &
      assignment(=)
  use control_vectors, only: cvars3d,cvars2d
  use mpeu_util, only: getindex
  implicit none

! Declare passed variables
  integer(i_kind)     ,intent(in   ) :: nv,mype  	
  type(control_vector),intent(in   ) :: dx,dy
  real(r_kind)        ,intent(in   ) :: alpha,gamma


! Declare local variables
  integer(i_kind),save :: nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw
  integer(i_kind),save :: nrf2_ps,nrf2_sst,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_cldch,nrf2_wspd10m,&
                          nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas,&
                          nrf2_uwnd10m,nrf2_vwnd10m,&
                          nrf3_sfwter,nrf3_vpwter

  integer(i_kind) i,k,k1,k2,lun,ifield,icase,ii,istatus

  real(r_kind),allocatable,dimension(:)::tempa
  real(r_kind),allocatable,dimension(:,:)::slab
  real(r_kind),allocatable,dimension(:)::strp
  real(r_kind),allocatable,dimension(:)::field
  real(r_kind),pointer,dimension(:,:)  :: ptr2d=>NULL()
  type(control_vector)::dz
! RTodling: not sure this is the best thing to do here
! This assumes the control vector has variables named
! as below ... the way code now, if the control vector
! does not have the fields below, they won't be written
! out to file
  integer(i_kind),  parameter :: my3d = 8
  character(len=8), parameter :: myvars3d(my3d) = (/  &
                                  'sf    ', &
                                  'vp    ', &
                                  't     ', &
                                  'q     ', &
                                  'oz    ', &
                                  'cw    ', &
                                  'sfwter', &
                                  'vpwter' /)  
  character(2) clun1
  character(3) clun2

  integer(i_kind),save:: indexmax_of_gradvectors
  logical fexist

  namelist /gradvectors_writeout/indexmax_of_gradvectors
!*************************************************************************
  if (iter==0) then
     lun=19
     inquire(file='parmcard_input',exist=fexist)
     if (fexist) then
        open(lun,file='parmcard_input',form='formatted')
        read(lun,gradvectors_writeout)
        close(lun)
      else
        indexmax_of_gradvectors=nv
     endif

     if (mype==0) print*,'in writeout_gradients: indexmax_of_gradvectors=', &
                                                 indexmax_of_gradvectors
  endif

  if (iter > indexmax_of_gradvectors) return

! Get control variable indices
  if (iter==0) then
     nrf3_sf   = getindex(cvars3d,'sf')
     nrf3_vp   = getindex(cvars3d,'vp')
     nrf3_t    = getindex(cvars3d,'t')
     nrf3_q    = getindex(cvars3d,'q')
     nrf3_oz   = getindex(cvars3d,'oz')
     nrf3_cw   = getindex(cvars3d,'cw')
     nrf3_sfwter = getindex(cvars3d,'sfwter')
     nrf3_vpwter = getindex(cvars3d,'vpwter')
     nrf2_ps   = getindex(cvars2d,'ps')
     nrf2_sst  = getindex(cvars2d,'sst')
     nrf2_gust = getindex(cvars2d,'gust')
     nrf2_vis  = getindex(cvars2d,'vis')
     nrf2_pblh = getindex(cvars2d,'pblh')
     nrf2_cldch = getindex(cvars2d,'cldch')
     nrf2_wspd10m = getindex(cvars2d,'wspd10m')
     nrf2_td2m = getindex(cvars2d,'td2m')
     nrf2_mxtm = getindex(cvars2d,'mxtm')
     nrf2_mitm = getindex(cvars2d,'mitm')
     nrf2_pmsl = getindex(cvars2d,'pmsl')
     nrf2_howv = getindex(cvars2d,'howv')
     nrf2_tcamt = getindex(cvars2d,'tcamt')
     nrf2_lcbas = getindex(cvars2d,'lcbas')
     nrf2_uwnd10m = getindex(cvars2d,'uwnd10m')
     nrf2_vwnd10m = getindex(cvars2d,'vwnd10m')
  endif


  call allocate_cv(dz)
  allocate(tempa(itotsub))
  allocate(slab(nlon,nlat))
  allocate(strp(lat1*lon1))
  allocate(field(lat2*lon2*nsig))

  write (clun1(1:2),'(i2.2)') jiter
  write (clun2(1:3),'(i3.3)') iter

  lun=19
  do icase=1,2
 
     if (icase==1) then 
        dz=dx
        open (lun,file='gradx.dat_'//clun1//'_'//clun2,form='unformatted')
     else if (icase==2) then 
        dz=dy
        open (lun,file='grady.dat_'//clun1//'_'//clun2,form='unformatted')
     endif

     write (lun) nlon,nlat,nsig,jpch_rad,npred,npcptype,npredp,jiter,nv,alpha,gamma, &
                 nrf3_sf,nrf3_vp,nrf3_t,nrf3_q,nrf3_oz,nrf3_cw,nrf3_sfwter,nrf3_vpwter, & 
                 nrf2_ps,nrf2_sst,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_cldch,nrf2_wspd10m, &
                 nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas, & 
                 nrf2_uwnd10m,nrf2_vwnd10m

     ii=1
     do ifield=1,my3d
        call gsi_bundlegetvar(dz%step(ii),myvars3d(ifield),field,istatus)
        if (istatus==0) then

           do k=1,nsig
              k1=1+(k-1)*latlon11
              k2=k1+latlon11-1
              call strip(field(k1:k2),strp)
   
              call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
                   tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

              if(mype == 0) then
                 do i=1,iglobal
                    slab(ltosj(i),ltosi(i))=tempa(i)
                 end do
                 write(lun) slab
              endif
           end do

        endif !ip>0
     enddo !ifield

!                               gradient wrt sfcp
     call gsi_bundlegetpointer(dz%step(ii),'ps',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt sfct
     call gsi_bundlegetpointer(dz%step(ii),'sst',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt gust
     call gsi_bundlegetpointer(dz%step(ii),'gust',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0


!                               gradient wrt vis
     call gsi_bundlegetpointer(dz%step(ii),'vis',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt pblh
     call gsi_bundlegetpointer(dz%step(ii),'pblh',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt cldch
     call gsi_bundlegetpointer(dz%step(ii),'cldch',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt wspd10m
     call gsi_bundlegetpointer(dz%step(ii),'wspd10m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt td2m
     call gsi_bundlegetpointer(dz%step(ii),'td2m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt mxtm
     call gsi_bundlegetpointer(dz%step(ii),'mxtm',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt mitm
     call gsi_bundlegetpointer(dz%step(ii),'mitm',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt pmsl
     call gsi_bundlegetpointer(dz%step(ii),'pmsl',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt howv
     call gsi_bundlegetpointer(dz%step(ii),'howv',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt tcamt
     call gsi_bundlegetpointer(dz%step(ii),'tcamt',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt lcbas
     call gsi_bundlegetpointer(dz%step(ii),'lcbas',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt uwnd10m
     call gsi_bundlegetpointer(dz%step(ii),'uwnd10m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                               gradient wrt vwnd10m
     call gsi_bundlegetpointer(dz%step(ii),'vwnd10m',ptr2d,istatus)
     if (istatus==0) then
        call strip(ptr2d,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_rtype, &
             tempa,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,iglobal
              slab(ltosj(i),ltosi(i))=tempa(i)
           end do
           write(lun) slab
        endif
     endif !ip>0

!                   gradient wrt satellite radiance bias correction coefficients
     if (mype==0) write(lun) dz%predr

!                   gradient wrt precipitation bias correction coefficients
     if (mype==0)write(lun) dz%predp

     close(lun)
  end do ! icase

  call deallocate_cv(dz)
  deallocate(tempa)
  deallocate(slab)
  deallocate(strp)
  deallocate(field)

  return

end subroutine writeout_gradients
