subroutine get_nmmb_ensperts

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_nmmb_ensperts adaptation of get_gefs_ensperts_dualres
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: read ensemble members, and construct ensemble perturbations, for use
!             with hybrid ensemble option.
!
! program history log:
!   2011-07-01 carley - initial adaptation for NMMB (not yet dual-res compat.)
!   2011-09-19 carley - implement single precision bundle changes
!   2017-05-12 Y. Wang and X. wang - add one option to read hydrometeors and W
!                       for radar DA, POC: xuguang.wang@ou.edu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
   use kinds, only: r_kind,i_kind,r_single
   use gridmod, only: pt_ll,pdtop_ll,aeta2_ll,aeta1_ll
   use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen 
   use constants,only: zero,one,one_tenth,ten
   use mpimod, only: mpi_comm_world,ierror,mype
   use hybrid_ensemble_parameters, only: n_ens,grd_ens,q_hyb_ens
   use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
   use gsi_bundlemod, only: gsi_bundlecreate,gsi_bundleset,gsi_grid,gsi_bundle, &
                            gsi_bundlegetpointer,gsi_bundledestroy,gsi_gridcreate

   use mpeu_util, only: getindex
   implicit none

   real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u,v,tv,q,oz,qs,rh,tsen,prsl
   real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: w, qr, qli, ql, dbz, dw, qi
   real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: z,ps,sst2
   real(r_kind),pointer,dimension(:,:,:):: x3
   real(r_single),pointer,dimension(:,:,:) :: w3
   real(r_kind),pointer,dimension(:,:):: x2
   real(r_single),pointer,dimension(:,:):: w2
   type(gsi_bundle):: en_bar
   type(gsi_grid)  :: grid_ens
   real(r_kind) bar_norm,sig_norm

   integer(i_kind) istatus,i,ic2,ic3,j,k,n,iderivative,i_radar_qr,i_radar_qli
   character(70) filename
   logical ice

   call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
   call gsi_bundlecreate(en_bar,grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
   if(istatus/=0) then
      write(6,*)' get_nmmb_ensperts: trouble creating en_bar bundle'
      call stop2(999)
   endif
   
   do n=1,n_ens
      en_perts(n,1,1)%valuesr4=zero
   end do

   en_bar%values=zero
   sst2=zero        !    for now, sst not used in ensemble perturbations, so if sst array is called for
                    !      then sst part of en_perts will be zero when sst2=zero

!    Determine if qr and qli are control variables for radar data assimilation,
     i_radar_qr=0
     i_radar_qli=0
     i_radar_qr=getindex(cvars3d,'qr')
     i_radar_qli=getindex(cvars3d,'qli')

   do n=1,n_ens
      write(filename,100) n               !make the filename
100 format('nmmb_ens_mem',i3.3)    


      if (mype==0)write(6,*) 'CALL GENERAL_READ_NMMB FOR ENS FILE : ',filename  
      if( i_radar_qr > 0 .and. i_radar_qli > 0 )then
         call general_read_nmmb_radar(grd_ens,filename,mype,z,ps,u,v,w,qr,qli,ql,qi,dbz,dw,tv,tsen,q,oz)
      else
         call general_read_nmmb(grd_ens,filename,mype,z,ps,u,v,tv,tsen,q,oz)
      end if

! For regional application (NMMB) use the the u,v option (i.e. uv_hyb_ens)
! Compute RH
! get 3d pressure at layer midpoints  
! using code adapted from subroutine load_prsges for nmmb
!  (in guess_grids.F90)
    
      do k=1,grd_ens%nsig
         do j=1,grd_ens%lon2
            do i=1,grd_ens%lat2         
              prsl(i,j,k)=one_tenth*                                  &
                    (aeta1_ll(k)*pdtop_ll +                     &
                     aeta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                     pt_ll)
            end do
         end do
      end do

      if (.not.q_hyb_ens) then
         ice=.true.
         iderivative=0
         call genqsat(qs,tsen(1,1,1),prsl(1,1,1),grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)
         do k=1,grd_ens%nsig
            do j=1,grd_ens%lon2
               do i=1,grd_ens%lat2
                  rh(i,j,k) = q(i,j,k)/qs(i,j,k)
               end do
            end do
         end do
      end if

      do ic3=1,nc3d

         call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
         if(istatus/=0) then
            write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n,' in get_nmmb_ensperts'
            call stop2(999)
         end if
         call gsi_bundlegetpointer(en_bar,trim(cvars3d(ic3)),x3,istatus)
         if(istatus/=0) then
            write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar in get_nmmb_ensperts'
            call stop2(999)
         end if

         select case (trim(cvars3d(ic3)))

            case('sf','SF')
                
               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = u(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+u(i,j,k)
                     end do
                  end do
               end do

            case('vp','VP')
               
               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = v(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+v(i,j,k)
                     end do
                  end do
               end do

            case('w','W')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = w(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+w(i,j,k)
                     end do
                  end do
               end do

            case('dw','DW')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = dw(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+dw(i,j,k)
                     end do
                  end do
               end do

            case('t','T')
            
               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = tv(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+tv(i,j,k)
                     end do
                  end do
               end do

            case('q','Q')
              if (.not.q_hyb_ens) then   ! use RH
                  do k=1,grd_ens%nsig
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = rh(i,j,k)
                           x3(i,j,k)=x3(i,j,k)+rh(i,j,k)
                        end do
                     end do
                  end do
               else                       ! use Q
                  do k=1,grd_ens%nsig
                     do j=1,grd_ens%lon2
                        do i=1,grd_ens%lat2
                           w3(i,j,k) = q(i,j,k)
                           x3(i,j,k)=x3(i,j,k)+q(i,j,k)
                        end do
                     end do
                  end do
               end if

            case('qr','QR')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = qr(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+qr(i,j,k)
                     end do
                  end do
               end do

            case('qli','QLI')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = qli(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+qli(i,j,k)
                     end do
                  end do
               end do

            case('qi','QI')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = qi(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+qi(i,j,k)
                     end do
                  end do
               end do

            case('ql','QL')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = ql(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+ql(i,j,k)
                     end do
                  end do
               end do

            case('dbz','DBZ')

               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = dbz(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+dbz(i,j,k)
                     end do
                  end do
               end do


            case('oz','OZ')
                      
               do k=1,grd_ens%nsig
                  do j=1,grd_ens%lon2
                     do i=1,grd_ens%lat2
                        w3(i,j,k) = oz(i,j,k)
                        x3(i,j,k)=x3(i,j,k)+oz(i,j,k)
                     end do
                  end do
               end do
                          
         end select
      end do

      do ic2=1,nc2d

         call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
         if(istatus/=0) then
            write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n, ' in get_nmmb_ensperts'
            call stop2(999)
         end if
         call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
         if(istatus/=0) then
            write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar in get_nmmb_ensperts'
            call stop2(999)
         end if

         select case (trim(cvars2d(ic2)))

            case('ps','PS')
                      
               do j=1,grd_ens%lon2
                  do i=1,grd_ens%lat2
                     w2(i,j) = ps(i,j)
                     x2(i,j)=x2(i,j)+ps(i,j)
                  end do
              end do

            case('sst','SST')
                      
               do j=1,grd_ens%lon2
                  do i=1,grd_ens%lat2
                     w2(i,j) = sst2(i,j)
                     x2(i,j)=x2(i,j)+sst2(i,j)
                  end do
               end do

         end select
      end do
   end do ! end do over ensemble

! Convert to mean
  bar_norm = one/real(n_ens,r_kind)
  en_bar%values=en_bar%values*bar_norm
   
! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p 
  do ic2=1,nc2d

     if(trim(cvars2d(ic2)) == 'ps'.or.trim(cvars2d(ic2)) == 'PS') then

        call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar in get_nmmb_ensperts'
           call stop2(999)
        end if

        do j=1,grd_ens%lon2
           do i=1,grd_ens%lat2
              ps_bar(i,j,1)=x2(i,j)
           end do
        end do
        exit
     end if
  end do

  call mpi_barrier(mpi_comm_world,ierror)
   
! Convert ensemble members to perturbations
   sig_norm=sqrt(one/max(one,n_ens-one))  
    
   do n=1,n_ens
      do i=1,nelen
         en_perts(n,1,1)%valuesr4(i)=(en_perts(n,1,1)%valuesr4(i)-en_bar%values(i))*sig_norm      
      end do
   end do
        
   call gsi_bundledestroy(en_bar,istatus)
   if(istatus/=0) then
      write(6,*)' in get_nmmb_ensperts: trouble destroying en_bar bundle in get_nmmb_ensperts'
      call stop2(999)
   endif

   if (mype==0)write(6,*) 'get_nmmb_ensperts DONE'
   return

end subroutine get_nmmb_ensperts
