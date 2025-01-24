subroutine general_read_nmmb_radar(grd,filename,mype,g_z,g_ps,g_u,g_v,g_w,g_qr,g_qli,g_ql,g_qi,g_dbz,g_dw,g_tv,g_tsen,g_q,g_oz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_nmmb_radar is same as general_read_nmmb but for reading in more variables
!   prgmmr: parrish          org: np22                date: 2003-09-05
!
! abstract: copied from read_nems_nmmb_guess, primarily for reading in NMMB NEMSIO files, for now the
!            input resolution and the grid that variables are reconstructed on MUST be the same
!            as the analysis grid/resolution.
!
! program history log:
!   2011-07-01  carley     - Initial adaptation
!   2015-05-12  wu         - changes to read in multiple guess files for FGAT/4DEnVar 
!   2017-05-12  Y. Wang and X. Wang - add this option to read hydrometeors and
!                                     vertical velocity. Ting provides codes to
!                                     convert fraction variables to mixing
!                                     ratios.  POC: xuguang.wang@ou.edu
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     filename - input nemsio file name
!     mype     - mpi task id
!
!   output argument list:
!     g_*      - guess fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$   

  use kinds, only: r_kind,i_kind,r_single                                                                        
  use gridmod, only: pdtop_ll,pt_ll,nmmb_verttype,use_gfs_ozone,regional_ozone
  use constants, only: zero,one_tenth,one,fv,r0_01, ten
  use gsi_nemsio_mod, only: gsi_nemsio_open,gsi_nemsio_close,gsi_nemsio_read,gsi_nemsio_read_fractionnew
  use general_sub2grid_mod, only: sub2grid_info
  use wrf_vars_mod, only :w_exist,dbz_exist
  use obsmod,only:if_model_dbz
  use gridmod, only: pt_ll,aeta2_ll,aeta1_ll,pdtop_ll
  use constants, only: rd, r1000

  implicit none

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    character(70)                         ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_tv,g_tsen,g_q,g_oz,g_w,g_qr,g_qli,g_ql,g_dbz,g_dw, g_qi

!   Declare local variables
    integer(i_kind) i,j,k,kr,nsig,lat2,lon2,mype_input,ierr
    real(r_kind) pd,psfc_this,pd_to_ps,dumtv
    logical good_o3mr
    real(r_kind),dimension(grd%lat2,grd%lon2) :: g_pd
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: dbz, &
                                      Ze, Zer, Zeli,g_prsl, g_rho
    real(r_kind)    :: Cr=3.6308e9_r_kind          ! Rain constant coef.
    real(r_kind)    :: Cli=3.268e9_r_kind          ! Precip. ice constant coef.

!     get conversion factor for pd to psfc

  if(nmmb_verttype=='OLD') then
     pd_to_ps=pdtop_ll+pt_ll
  else
     pd_to_ps=pt_ll
  end if
   

   lat2=grd%lat2
   lon2=grd%lon2
   nsig=grd%nsig

   mype_input=0

     call gsi_nemsio_open(filename,'READ', &
                          'GENERAL_READ_NMMB:  problem with ens input file!',mype,mype_input,ierr)
                          
!                            ! pd

     call gsi_nemsio_read('dpres','hybrid sig lev','H',1,g_pd(:,:),mype,mype_input)
     do i=1,lon2
        do j=1,lat2
!               convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
           pd=r0_01*g_pd(j,i)
           psfc_this=pd+pd_to_ps
           g_ps(j,i)=one_tenth*psfc_this
        end do
     end do

!                          !   fis

     call gsi_nemsio_read('hgt','sfc','H',1,g_z(:,:),mype,mype_input)

!                          !   u,v,q,tsen,tv,cwmr,qr,qli
     do kr=1,nsig
        k=nsig+1-kr
        call gsi_nemsio_read('ugrd','mid layer','V',kr,g_u(:,:,k),   mype,mype_input)
        call gsi_nemsio_read('vgrd','mid layer','V',kr,g_v(:,:,k),   mype,mype_input)
        if(w_exist)then
          call gsi_nemsio_read('w_tot','mid layer','H',kr,g_w(:,:,k), mype,mype_input)
          call gsi_nemsio_read('dwdt','mid layer','H',kr,g_dw(:,:,k), mype,mype_input)
        end if
        call gsi_nemsio_read('spfh','mid layer','H',kr,g_q(:,:,k),   mype,mype_input)
        call gsi_nemsio_read('tmp' ,'mid layer','H',kr,g_tsen(:,:,k),mype,mype_input)

        if( if_model_dbz )&
        call gsi_nemsio_read('refl_10cm' ,'mid layer','H',kr,dbz(:,:,k),mype,mype_input)
        do i=1,lon2
           do j=1,lat2
              g_tv(j,i,k) = g_tsen(j,i,k) * (one+fv*g_q(j,i,k))  !To be consistent with read_nems_nmmb_guess
                                                                 ! compute tv prior to enforcing the limit on q.								 
              g_q(j,i,k)=max(g_q(j,i,k),1.e-10_r_kind)           !step comes from compute_qvar3d.f90    
              dumtv=g_tsen(j,i,k) * (one+fv*g_q(j,i,k))
              g_tsen(j,i,k)=dumtv/(one+fv*max(zero,g_q(j,i,k)))     !Recompute tsen based on the limit enforced on q.	      	      	      

              if( dbz_exist .and. ( .not. if_model_dbz ) ) then
                    g_prsl(j,i,k)=one_tenth* &
                                  (aeta1_ll(k)*pdtop_ll + &
                                   aeta2_ll(k)*(ten*g_ps(j,i)-pdtop_ll-pt_ll) + &
                                   pt_ll)

                    g_rho(j,i,k)=(g_prsl(j,i,k)/(g_tv(j,i,k)*rd))*r1000
              end if


              if( dbz_exist )then
                if ( if_model_dbz )then
                   g_dbz(j,i,k) = max( dbz(j,i,k), 0.0_r_kind )
                end if
              end if
            end do
        end do
        call gsi_nemsio_read_fractionnew('f_rain','f_ice','clwmr','f_rimef','mid layer',kr, &
             g_qi(:,:,k),g_qli(:,:,k),g_qr(:,:,k),g_ql(:,:,k), mype,mype_input)

        if ( dbz_exist .and. (.not. if_model_dbz) )then
            Zer(:,:,k)  = Cr * (g_rho(:,:,k) * g_qr(:,:,k))**(1.75_r_kind)
            Zeli(:,:,k) = Cli * (g_rho(:,:,k) * g_qli(:,:,k))**(2.0_r_kind)
            Ze(:,:,k)=Zer(:,:,k)+Zeli(:,:,k)

            g_dbz(:,:,k) = 0.0_r_kind

            where ( Ze(:,:,k) > 0.0_r_kind )
              g_dbz(:,:,k) = ten * log10(Ze(:,:,k))
            end where
            where( g_dbz(:,:,k) < 0.0_r_kind )
               g_dbz(:,:,k) = 0.0_r_kind
            endwhere
        end if
	
        if(regional_ozone) then
           if(use_gfs_ozone) then
              g_oz(:,:,k)=zero
           else
              good_o3mr=.false.
              call gsi_nemsio_read('o3mr' ,'mid layer','H',kr,g_oz(:,:,k),mype,mype_input,good_o3mr)
              if(.not.good_o3mr) write(6,*)' IN GENERAL_READ_NMMB, O3MR FIELD NOT YET AVAILABLE'
           end if
        end if
     end do

     call gsi_nemsio_close(filename,'GENERAL_READ_NMMB',mype,mype_input)
  return 
end subroutine general_read_nmmb_radar
