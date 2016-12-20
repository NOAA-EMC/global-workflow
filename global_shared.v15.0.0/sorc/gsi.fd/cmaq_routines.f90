subroutine read_cmaq_files(mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_cmaq_files   same as read_files, but cmaq
!pagowski - based on read_wrf_mass_files.f90 but for cmaq
!   abstract: read cmaq input file, add analysis, and write out analysis
!   in cmaq intermediate format, based on other writers
!   all to be replaced when cmaq nemsio is ready
! along with changes to chemmod.f90
!
! abstract: figure out available time levels of background fields for 
!             later input. This is patterned after read_wrf_mass_files.
!
! program history log:
!   2010-09-07  pagowski
!   2015-09-17  Thomas  - add l4densvar to data selection procedure
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
  
  use kinds, only: i_kind,r_kind,r_single
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
  use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
       ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
  use guess_grids, only: hrdifsig_all,hrdifsfc_all
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen, nhr_assimilation,&
       l4densvar
  use gridmod, only: regional_time,regional_fhr
  use constants, only: zero,one,r60inv
  use obsmod, only: iadate,time_offset
  implicit none
  
! declare passed variables
  integer(i_kind),intent(in   ) :: mype
  
! declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind
  
! declare local variables
  logical(4) fexist
  character(6) filename
  integer(i_kind) :: i,j,iwan,npem1
  integer(i_kind) :: nhr_half
  integer(i_kind) :: nminanl,nmings,nming2,ndiff
  integer(i_kind),dimension(4):: idateg
  integer(i_kind),dimension(5):: idate5
  real(r_single) hourg4
  real(r_kind) hourg,temp,t4dv
  real(r_kind),dimension(202,2):: time_ges
  
! start read_cmaq_files here.
  nhr_half=nhr_assimilation/2
  if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1  
  npem1=npe-1

  do i=1,202
     time_ges(i,1) = 999_r_kind
     time_ges(i,2) = 999_r_kind
  end do
  

! let a single task query the guess files.
  if(mype==npem1) then
     
!    convert analysis time to minutes relative to fixed date
     call w3fs21(iadate,nminanl)
     write(6,*)'read_cmaq_files:  analysis date,minutes ',iadate,nminanl
     
!    check for consistency of times from sigma guess files.
     iwan=0
     do i=0,99
        write(filename,100)i
100     format('sigf',i2.2)
        inquire(file=filename,exist=fexist)
        if(fexist)then
           idateg(1)=regional_time(4)  !  hour
           idateg(2)=regional_time(2)  !  month
           idateg(3)=regional_time(3)  !  day
           idateg(4)=regional_time(1)  !  year
           hourg4= regional_fhr        !  fcst hour
           hourg = hourg4
           idate5(1)=idateg(4); idate5(2)=idateg(2)
           idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=regional_time(5)
           call w3fs21(idate5,nmings)
           nming2=nmings+60*hourg
           write(6,*)'read_cmaq_files:  sigma guess file, nming2 ',hourg,idateg,nming2
           t4dv=real((nming2-iwinbgn),r_kind)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .or. t4dv>winlen) go to 110
           else
              ndiff=nming2-nminanl
              if(abs(ndiff) > 60*nhr_half ) go to 110
           endif
           
           iwan=iwan+1
           time_ges(iwan,1) = real((nming2-iwinbgn),r_kind)*r60inv
           time_ges(iwan+100,1)=i+r0_001
        end if
110     continue
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
     
!surface fields in cmaq are assigned to defaults
     
  end if
  
! broadcast guess file information to all tasks
  call mpi_bcast(time_ges,404,mpi_rtype,npem1,mpi_comm_world,ierror)
  nfldsig   = nint(time_ges(201,1))
  
  nfldsfc   = 1
  
! allocate space for guess information files
  call create_gesfinfo
  
  do i=1,nfldsig
     ifilesig(i) = -100
     hrdifsig(i) = zero
  end do
  
  do i=1,nfldsfc
     ifilesfc(i) = -100
     hrdifsfc(i) = zero
  end do
  
! load time information for sigma guess field sinfo into output arrays
  ntguessig = nint(time_ges(202,1))
  
  do i=1,nfldsig
     hrdifsig(i) = time_ges(i,1)
     ifilesig(i) = nint(time_ges(i+100,1))
     hrdifsig_all(i) = hrdifsig(i)
  end do
  
  if(mype == 0) write(6,*)'read_cmaq_files:  sigma fcst files used in analysis  :  ',&
       (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig
  
  ntguessfc = ntguessig
  
  do i=1,nfldsfc
     hrdifsfc(i) = hrdifsig(ntguessig)
     ifilesfc(i) = ifilesig(ntguessig)
     hrdifsfc_all(i) = hrdifsfc(i)
  end do
  
  if(mype == 0) write(6,*)'read_cmaq_files:  surface fcst files used in analysis:  ',&
       (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc
  
  return
  
end subroutine read_cmaq_files

subroutine read_cmaq_guess(mype)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
!read intermediate cmaq binary file that included aerosol species
!and meteorology, all input variables in cmaq binary are on A-grid
!   2010-08-02 pagowski - largely based on reading wrf intermediate binary file
!                         routine read_wrf_mass_netcdf_guess
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     mype     - pe number
!
  
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  
  use kinds, only: r_kind,r_single,i_kind
  
  use mpimod, only: mpi_real4,mpi_comm_world,npe,ierror
  use guess_grids, only: nfldsig,ifilesig,ges_tsen
  use guess_grids, only: isli,fact10,sfct,dsfct,sno,veg_type,veg_frac,&
       soil_type,soil_temp,soil_moi,sfc_rough
  
  use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,&
       nsig,ijn_s,displs_s,itotsub

  use constants, only: zero,one,fv
  use gsi_io, only: lendian_in
  
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_bundle

  use constants, only : max_varname_length
  use chemmod, only : nmet2d_cmaq,nmet3d_cmaq,&
       naero_cmaq,pm2_5_guess,init_pm2_5_guess
    
  use mpeu_util, only: die
  implicit none
  
! declare passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind) :: status

  
!fields below are assigned defaults
!isli,fact10,sfct,dsfct,sno,veg_type,veg_frac,
!soil_type,soil_temp,soil_moi,sfc_rough

!for cmaq declare default values for constant/semi constant sfc fields
  character(len=*),parameter::myname='read_cmaq_guess'
  integer(i_kind),parameter :: isli_default=1
  real(r_kind),parameter :: fact10_default=one,&
       sno_default=zero,veg_type_default=2.0_r_kind,&
       veg_frac_default=0.1_r_kind,soil_type_default=4_r_kind,&
       dsfct_default=zero,soil_moi_default=0.3_r_kind,&
       rough_default=0.05_r_kind
! declare local variables

  
  character(len=max_varname_length) :: cvar

! declare local variables
  
  real(r_kind),parameter:: r0_001=0.001_r_kind
  
! other internal variables
  real(r_single) tempa(itotsub)
  real(r_single),allocatable::temp1(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  character(6) filename 
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
  integer(i_kind) ifld,im,jm,lm,num_cmaq_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v
  
  integer(i_kind) :: kfis,kpsfc,kt,kq,ku,kv,ier,istatus
  
  integer(i_kind) :: nskip
  
  real(r_kind),dimension(:,:  ),pointer::ges_ps_it=>NULL()
  real(r_kind),dimension(:,:  ),pointer::ges_z_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_u_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_v_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_q_it =>NULL()

  if(mype==0) write(6,*)' at 0 in read_cmaq_guess'
  
! big section of operations done only on first outer iteration
  
  im=nlon_regional
  jm=nlat_regional
  lm=nsig
  
  i_fis=1
  i_psfc=i_fis+1
  i_t=i_psfc+1
  i_q=i_t+nsig
  i_u=i_q+nsig
  i_v=i_u+nsig
  
!  CMAQ input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)
  
  
  num_cmaq_fields=nmet2d_cmaq+(nmet3d_cmaq+naero_cmaq)*nsig
  
  num_all_fields=num_cmaq_fields*nfldsig
  num_loc_groups=num_all_fields/npe
  
  if(mype==0) then 
     write(6,'(" at 1 in read_cmaq_guess, lm            =",i6)')lm
     write(6,'(" at 1 in read_cmaq_guess, num_cmaq_fields=",i6)')num_cmaq_fields
     write(6,'(" at 1 in read_cmaq_guess, nfldsig       =",i6)')nfldsig
     write(6,'(" at 1 in read_cmaq_guess, num_all_fields=",i6)')num_all_fields
     write(6,'(" at 1 in read_cmaq_guess, npe           =",i6)')npe
     write(6,'(" at 1 in read_cmaq_guess, num_loc_groups=",i6)')num_loc_groups
  endif

  do 
     num_all_pad=num_loc_groups*npe
     if(num_all_pad >= num_all_fields) exit
     num_loc_groups=num_loc_groups+1
  end do
  
  if(mype==0) then
     write(6,'(" at 2 in read_cmaq_guess, num_all_pad   =",i6)')num_all_pad
     write(6,'(" at 2 in read_cmaq_guess, num_loc_groups=",i6)')num_loc_groups
  endif

  allocate(all_loc(lat2,lon2,num_all_pad))
  
  allocate(temp1(im,jm))
  
  do i=1,npe
     irc_s_reg(i)=ijn_s(mype+1)
  end do
  ird_s_reg(1)=0
  do i=1,npe
     if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
  end do
  
!read cmaq intermediate binary
  
!    this is done by reading in parallel from every pe, and redistributing
!    to local domains once for every npe fields read in, using 
!    mpi_all_to_allv
  
  icount=0
  icount_prev=1
  
  do it=1,nfldsig
     write(filename,'("sigf",i2.2)')ifilesig(it)
     open(lendian_in,file=filename,form='unformatted') 
     rewind(lendian_in)
     
     if (mype==0) &
          write(6,*)'read_cmaq_guess:  open cmaq input=',&
          lendian_in,' to file=',filename
     
     read(lendian_in)nskip
     
     do ifld=1,nskip !need to skip grid variables
        read(lendian_in)
     enddo
     
     do ifld=1,num_cmaq_fields
        icount=icount+1
        if(mype==mod(icount-1,npe)) then
           read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
           call fill_mass_grid2t(temp1,im,jm,tempa,1)
        else
           read(lendian_in)
        end if
!          distribute to local domains everytime we have npe fields
        if(mod(icount,npe) == 0.or.icount==num_all_fields) then
           call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
           icount_prev=icount+1
        end if
     end do
     close(lendian_in)
  end do
  
  do it=1,nfldsig
     i_0=(it-1)*num_cmaq_fields
     kfis=i_0+i_fis
     kpsfc=i_0+i_psfc
     kt=i_0+i_t-1 !since +1 in sigma loop
     kq=i_0+i_q-1 
     ku=i_0+i_u-1
     kv=i_0+i_v-1

     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it ,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it ,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it ,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q_it ,  istatus)
     ier=ier+istatus
     if(ier/=0) call die(myname,'missing fields, ier= ', ier)

     do k=1,nsig
        kt=kt+1
        kq=kq+1
        ku=ku+1
        kv=kv+1
        do i=1,lon2
           do j=1,lat2
              ges_u_it(j,i,k) = all_loc(j,i,ku)
              ges_v_it(j,i,k) = all_loc(j,i,kv)
              ges_tsen(j,i,k,it) = all_loc(j,i,kt)
              ges_q_it(j,i,k) = all_loc(j,i,kq)
!                convert guess mixing ratio to specific humidity
              ges_q_it(j,i,k) = ges_q_it(j,i,k)/(one+ges_q_it(j,i,k))
              ges_tv_it(j,i,k) = ges_tsen(j,i,k,it) * &
                   (one+fv*ges_q_it(j,i,k))
           end do
        end do

     enddo

     do i=1,lon2
        do j=1,lat2
           ges_z_it(j,i) = all_loc(j,i,kfis)
           ges_ps_it(j,i)=r0_001*all_loc(j,i,kpsfc)! convert from Pa to cb
        end do
     end do

     do i=1,lon2
        do j=1,lat2
           isli(j,i,it)=isli_default
           fact10(j,i,it)=fact10_default
           sfct(j,i,it)=ges_tsen(j,i,1,it)
           dsfct(j,i,it)=dsfct_default
           sno(j,i,it)=sno_default
           veg_type(j,i,it)=veg_type_default
           veg_frac(j,i,it)=veg_frac_default
           soil_type(j,i,it)=soil_type_default
           soil_temp(j,i,it)=ges_tsen(j,i,1,it)
           soil_moi(j,i,it)=soil_moi_default
           sfc_rough(j,i,it)=rough_default
        end do
     end do

!do the bundle assignment
     
!the last variable written before aero is "v"
!assign icount for aerosols
     
     call init_pm2_5_guess

     icount=kv

     do ifld=1,naero_cmaq
        
        do k=1,nsig
           
!aerosols come after v - just do the counts
           icount=icount+1
     
           do i=1,lon2
              do j=1,lat2
                 pm2_5_guess(j,i,k)=pm2_5_guess(j,i,k)+all_loc(j,i,icount)
              enddo
           enddo
        enddo

     enddo

     cvar='pm2_5'

     call gsi_bundlegetpointer (gsi_chemguess_bundle(it), &
          cvar, ptr3d, status)
     
     if(status/=0) then 
        write(6,*)'problem with bundle pointers in cmaq_routines'
        call stop2(425)
     endif
     
     do k=1,nsig
        do i=1,lon2
           do j=1,lat2
              ptr3d(j,i,k)=pm2_5_guess(j,i,k)
           enddo
        enddo
     enddo
     
  enddo

  deallocate(all_loc)
  deallocate(temp1)

  return
  
end subroutine read_cmaq_guess

subroutine make_sigf
  
!since cmaq file is read without conversions just link cmaq inout to sigf
!temporary routine until cmaq input is ready in nemsio 
  
!   2010-09-15  pagowski - link cmaq_input to sigf
  
  use gsi_4dvar, only: nhr_assimilation
  use chemmod, only : maxstr,in_fname
  use kinds, only: i_kind
  
  implicit none
  
  character(len=maxstr) :: cmaq_infile_name

  character(len=6) :: filename
  character(len=maxstr) :: command
  
  logical :: fexist
  
  integer(i_kind) :: ihr

  cmaq_infile_name=in_fname
  
! get regional constants
  
  ihr=-999
  
  inquire(file=trim(cmaq_infile_name),exist=fexist)
  if (fexist) then
     ihr=nhr_assimilation
     write(filename,100) nhr_assimilation
100  format('sigf',i2.2)
     command='if [ ! -r '//trim(filename)//' ] ;then ln -sf '//trim(cmaq_infile_name)//' '//trim(filename)//' ;fi'
     call system(command)
  endif
  
end subroutine make_sigf

subroutine write_cmaq(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_cmaq - write out cmaq intermediate binary file
!   prgmmr: pagowski   date: 2010-09-22
!
! program history log:
!   2010-09-23  pagowski 
!   2013-10-24  todling - revisit strip interface
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use guess_grids, only: ntguessig,ifilesig
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use gridmod, only: lat2,iglobal,itotsub,&
       lon2,nsig,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g,&
       strip
  use constants, only: zero_single,&
       tiny_single,max_varname_length
  use gsi_io, only: lendian_in, lendian_out
  use chemmod, only : ngrid2d_cmaq,nmet2d_cmaq,nmet3d_cmaq,&
       naero_cmaq,aeronames_cmaq,pm2_5_guess,diag_incr,maxstr,out_fname,&
       incr_fname
  use gsi_chemguess_mod, only: gsi_chemguess_bundle
  use gsi_bundlemod, only : gsi_bundlegetpointer

  implicit none
  
! declare passed variables
  integer(i_kind),intent(in   ) :: mype
  
! declare local parameters

!wish this could be combined to a module rather then repeat

! declare local variables
  integer(i_kind) im,jm,lm
  real(r_single),allocatable::temp1(:),tempa(:),tempb(:)
  real(r_single),allocatable::all_loc(:,:,:),ratio(:,:,:)
  real(r_single),allocatable::incr(:,:,:)
  real(r_single),allocatable::strp(:)
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  character(6) filename
  integer(i_kind) :: i,j,k,it
  integer(i_kind) :: ncmaqin,nskip
  integer(i_kind) :: regional_time0(6),nlon_regional0,nlat_regional0,nsig0
  real(r_single) aeta10(nsig),eta10(nsig+1),aeta20(nsig),eta20(nsig+1),pt0,pdt0
  integer(i_kind) :: ifld
  integer :: status
  character(len=max_varname_length) :: cvar
  character(len=maxstr) :: cmaq_outfile_name,cmaq_incrementfile_name

  cmaq_outfile_name=out_fname
  cmaq_incrementfile_name=incr_fname

  ncmaqin=ngrid2d_cmaq+nmet2d_cmaq+nmet3d_cmaq*nsig

  im=nlon_regional
  jm=nlat_regional
  lm=nsig
  
  allocate(all_loc(lat2,lon2,nsig),ratio(lat2,lon2,nsig))
  allocate(incr(lat2,lon2,nsig))
  allocate(strp(lat1*lon1))
  
  allocate(temp1(im*jm))
  allocate(tempa(itotsub),tempb(itotsub))

  if(mype == 0) then
     write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
     open (lendian_in,file=filename,form='unformatted')
     open (lendian_out,file=trim(cmaq_outfile_name),form='unformatted')
     rewind lendian_in ; rewind lendian_out
  end if
  
! convert analysis variables to cmaq variables
  it=ntguessig
  
  if(mype == 0) then
     read(lendian_in) nskip
     read(lendian_in) regional_time0,nlon_regional0,nlat_regional0,nsig0,pt0,pdt0
     write(lendian_out) regional_time0,nlon_regional0,nlat_regional0,&
          nsig0

     read(lendian_in) aeta10,aeta20
     read(lendian_in) eta10,eta20
     
     do i=1,ncmaqin
        read(lendian_in)temp1
     enddo

  end if

  cvar='pm2_5'

  call gsi_bundlegetpointer (gsi_chemguess_bundle(it), &
       cvar, ptr3d, status)

  if(status/=0) then
     write(6,*)'problem with bundle pointers in cmaq_routines'
     call stop2(426)
  endif

  do k=1,nsig
     do i=1,lon2
        do j=1,lat2
           incr(j,i,k)=ptr3d(j,i,k)-pm2_5_guess(j,i,k)
           ratio(j,i,k)=max(min(1.0_r_single+incr(j,i,k)/pm2_5_guess(j,i,k),10.0_r_single),&
                tiny_single)
           all_loc(j,i,k)=ratio(j,i,k)
        enddo
     enddo
  enddo

  do ifld=1,naero_cmaq
     
     if (mype==0) then 
        write(lendian_out)aeronames_cmaq(ifld)
     endif

     cvar=trim(aeronames_cmaq(ifld))
     
     do k=1,nsig
        
        call strip(all_loc(:,:,k),strp)
        
        call mpi_gatherv(strp,ijn(mype+1),mpi_real4,&
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
        
        if(mype == 0) then

           read(lendian_in)temp1

           call fill_mass_grid2t(temp1,im,jm,tempb,2)
        
           do i=1,iglobal
              tempa(i)=tempb(i)*tempa(i)
           end do

           do i=1,im*jm
              temp1(i)=zero_single
           enddo

           call unfill_mass_grid2t(tempa,im,jm,temp1)

           write(lendian_out)temp1

        end if

     end do
     
  enddo
  
  if (mype==0) then
     close(lendian_in)
     close(lendian_out)
  endif

  if (diag_incr) then

     open(unit=lendian_out,file=trim(incr_fname),form='unformatted')

     if (mype==0)  write(lendian_out)int(im),int(jm),int(nsig)
     
     do k=1,nsig

        call strip(incr(:,:,k),strp)

        call mpi_gatherv(strp,ijn(mype+1),mpi_real4,&
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)

        if(mype == 0) then
           do i=1,im*jm
              temp1(i)=zero_single
           enddo
           call unfill_mass_grid2t(tempa,im,jm,temp1)
           write(lendian_out)temp1
        endif

     enddo

     if (mype == 0) close(lendian_out)
     
  endif
  
  call mpi_barrier(mpi_comm_world,ierror)
  
  deallocate(temp1)  
  deallocate(all_loc)
  deallocate(strp)
  deallocate(tempa)
  deallocate(tempb)
  deallocate(incr)

end subroutine write_cmaq
