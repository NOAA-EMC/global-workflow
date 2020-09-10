 module interp

 implicit none

 private

 real, allocatable      :: sfcp_b4_adj_output(:)
 real, allocatable      :: clwmr_b4_adj_output(:,:)
 real, allocatable      :: dzdt_b4_adj_output(:,:)
 real, allocatable      :: grle_b4_adj_output(:,:)
 real, allocatable      :: cldamt_b4_adj_output(:,:)
 real, allocatable      :: icmr_b4_adj_output(:,:)
 real, allocatable      :: o3mr_b4_adj_output(:,:)
 real, allocatable      :: rwmr_b4_adj_output(:,:)
 real, allocatable      :: snmr_b4_adj_output(:,:)
 real, allocatable      :: spfh_b4_adj_output(:,:)
 real, allocatable      :: tmp_b4_adj_output(:,:)
 real, allocatable      :: ugrd_b4_adj_output(:,:)
 real, allocatable      :: vgrd_b4_adj_output(:,:)

 public                 :: adjust_for_terrain
 public                 :: gaus_to_gaus

 contains

 subroutine adjust_for_terrain

!---------------------------------------------------------------------------------
! Adjust fields based on differences between the interpolated and external
! terrain.
!---------------------------------------------------------------------------------

 use input_data
 use output_data
 use utils
 use setup

 implicit none

 integer :: k

 real, allocatable     :: pres_b4_adj_output(:,:)
 real, allocatable     :: pres_output(:,:)
 real, allocatable     :: q_b4_adj_output(:,:,:), q_output(:,:,:)

!---------------------------------------------------------------------------------
! First, compute the mid-layer pressure using the interpolated surface pressure.
!---------------------------------------------------------------------------------

 allocate(pres_b4_adj_output(ij_output,lev))
 pres_b4_adj_output = 0.0

 print*,'before newpr1, sfcp b4 adj: ', sfcp_b4_adj_output(ij_output/2)

 print*
 print*,"COMPUTE MID-LAYER PRESSURE FROM INTERPOLATED SURFACE PRESSURE."
 call newpr1(ij_output, lev, idvc, idsl, nvcoord_input, vcoord_input, &
             sfcp_b4_adj_output, pres_b4_adj_output)

 print*,'after newpr1, pres b4 adj: ', pres_b4_adj_output(ij_output/2,:)

!---------------------------------------------------------------------------------
! Adjust surface pressure based on differences between interpolated and
! grid terrain.
!---------------------------------------------------------------------------------

 allocate(sfcp_output(ij_output))
 sfcp_output = 0.0

 print*,"ADJUST SURFACE PRESSURE BASED ON TERRAIN DIFFERENCES"
 call newps(hgt_output, sfcp_b4_adj_output, ij_output, &
            lev, pres_b4_adj_output, tmp_b4_adj_output, &
            spfh_b4_adj_output, hgt_external_output, sfcp_output)
 
 print*,'after newps ',sfcp_b4_adj_output(ij_output/2),sfcp_output(ij_output/2)

 deallocate(sfcp_b4_adj_output)

!---------------------------------------------------------------------------------
! Recompute mid-layer pressure based on the adjusted surface pressure.
!---------------------------------------------------------------------------------

 allocate(pres_output(ij_output, lev_output))
 pres_output = 0.0

 allocate(dpres_output(ij_output, lev_output))
 dpres_output = 0.0

 print*,'before newpr1 ',sfcp_output(ij_output/2)
 print*,'before newpr1 ',idvc,idsl,nvcoord,vcoord

 print*,"RECOMPUTE MID-LAYER PRESSURE."
 call newpr1(ij_output, lev_output, idvc, idsl, nvcoord, vcoord, &
             sfcp_output, pres_output, dpres_output)

 do k = 1, lev_output
   print*,'after newpr1 ',pres_output(ij_output/2,k), dpres_output(ij_output/2,k)
 enddo

!---------------------------------------------------------------------------------
! Vertically interpolate from the pre-adjusted to the adjusted mid-layer 
! pressures.
!---------------------------------------------------------------------------------

 allocate(q_b4_adj_output(ij_output,lev,ntrac))
 q_b4_adj_output(:,:,1) = spfh_b4_adj_output(:,:)
 q_b4_adj_output(:,:,2) = o3mr_b4_adj_output(:,:)
 q_b4_adj_output(:,:,3) = clwmr_b4_adj_output(:,:)
 q_b4_adj_output(:,:,4) = rwmr_b4_adj_output(:,:)
 q_b4_adj_output(:,:,5) = icmr_b4_adj_output(:,:)
 q_b4_adj_output(:,:,6) = snmr_b4_adj_output(:,:)
 q_b4_adj_output(:,:,7) = grle_b4_adj_output(:,:)
 q_b4_adj_output(:,:,8) = cldamt_b4_adj_output(:,:)

 allocate(q_output(ij_output,lev_output,ntrac))
 q_output = 0.0

 allocate(dzdt_output(ij_output,lev_output))
 dzdt_output = 0.0

 allocate(ugrd_output(ij_output,lev_output))
 ugrd_output=0.0

 allocate(vgrd_output(ij_output,lev_output))
 vgrd_output=0.0

 allocate(tmp_output(ij_output,lev_output))
 tmp_output=0.0

 print*,"VERTICALLY INTERPOLATE TO NEW PRESSURE LEVELS"
 call vintg(ij_output, lev, lev_output, ntrac, pres_b4_adj_output,  &
            ugrd_b4_adj_output, vgrd_b4_adj_output, tmp_b4_adj_output, q_b4_adj_output,  &
            dzdt_b4_adj_output, pres_output, ugrd_output, vgrd_output, tmp_output, &
            q_output, dzdt_output)

 deallocate (dzdt_b4_adj_output,  q_b4_adj_output)
!deallocate (pres_b4_adj_output, pres_output)

 allocate(spfh_output(ij_output,lev_output))
 spfh_output = q_output(:,:,1)
 allocate(o3mr_output(ij_output,lev_output))
 o3mr_output = q_output(:,:,2)
 allocate(clwmr_output(ij_output,lev_output))
 clwmr_output = q_output(:,:,3)
 allocate(rwmr_output(ij_output,lev_output))
 rwmr_output = q_output(:,:,4)
 allocate(icmr_output(ij_output,lev_output))
 icmr_output = q_output(:,:,5)
 allocate(snmr_output(ij_output,lev_output))
 snmr_output = q_output(:,:,6)
 allocate(grle_output(ij_output,lev_output))
 grle_output = q_output(:,:,7)
 allocate(cldamt_output(ij_output,lev_output))
 cldamt_output = q_output(:,:,8)

 deallocate(q_output)

 do k = 1, lev
 print*,'after vintg tmp b4 ',tmp_b4_adj_output(ij_output/2,k), pres_b4_adj_output(ij_output/2,k)
 enddo
 do k = 1, lev_output
 print*,'after vintg tmp ',tmp_output(ij_output/2,k),pres_output(ij_output/2,k)
 enddo

 deallocate(tmp_b4_adj_output)

 deallocate(ugrd_b4_adj_output)

 deallocate(vgrd_b4_adj_output)

 deallocate(spfh_b4_adj_output)

 deallocate(o3mr_b4_adj_output)

 deallocate(clwmr_b4_adj_output)

 deallocate(rwmr_b4_adj_output)

 deallocate(icmr_b4_adj_output)

 deallocate(snmr_b4_adj_output)

 deallocate(grle_b4_adj_output)

 deallocate(cldamt_b4_adj_output)

 allocate(delz_output(ij_output, lev_output))
 delz_output = 0.0

 call compute_delz(ij_output, lev_output, vcoord(:,1), vcoord(:,2), sfcp_output, hgt_output, &
                   tmp_output, spfh_output, delz_output)

 do k = 1, lev_output
 print*,'after compute_delz ',delz_output(ij_output/2,k)
 enddo

 deallocate(hgt_output)

 end subroutine adjust_for_terrain

 subroutine gaus_to_gaus

!----------------------------------------------------------------------------------
! Interpolate data from the input to output grid using IPOLATES library.
!----------------------------------------------------------------------------------

 use output_data
 use input_data
 use setup

 implicit none

 integer                 :: ip, ipopt(20), i
 integer                 :: num_fields
 integer                 :: iret, numpts
 integer, allocatable    :: ibi(:), ibo(:)

 logical*1, allocatable  :: bitmap_input(:,:), bitmap_output(:,:)
 logical                 :: same_grid

 real, allocatable       :: data_input(:,:)
 real, allocatable       :: data_output(:,:), crot(:), srot(:)

 same_grid=.true.
 do i = 1, 11
   if (kgds_input(i) /= kgds_output(i)) then
     same_grid=.false.
     exit
   endif
 enddo

 if (same_grid) then

   print*
   print*,'INPUT AND OUTPUT GRIDS ARE THE SAME.'
   print*,'NO HORIZ INTERPOLATION REQUIRED.'

   allocate(hgt_output(ij_output))
   hgt_output = hgt_input
   deallocate(hgt_input)

   allocate(sfcp_b4_adj_output(ij_output))
   sfcp_b4_adj_output = sfcp_input
   deallocate(sfcp_input)

   allocate(tmp_b4_adj_output(ij_output,lev))
   tmp_b4_adj_output = tmp_input
   deallocate(tmp_input)

   allocate(clwmr_b4_adj_output(ij_output,lev))
   clwmr_b4_adj_output = clwmr_input
   deallocate(clwmr_input)

   allocate(spfh_b4_adj_output(ij_output,lev))
   spfh_b4_adj_output = spfh_input
   deallocate(spfh_input)

   allocate(o3mr_b4_adj_output(ij_output,lev))
   o3mr_b4_adj_output = o3mr_input
   deallocate(o3mr_input)

   allocate(dzdt_b4_adj_output(ij_output,lev))
   dzdt_b4_adj_output = dzdt_input
   deallocate(dzdt_input)

   allocate(rwmr_b4_adj_output(ij_output,lev))
   rwmr_b4_adj_output = rwmr_input
   deallocate(rwmr_input)

   allocate(snmr_b4_adj_output(ij_output,lev))
   snmr_b4_adj_output = snmr_input
   deallocate(snmr_input)

   allocate(icmr_b4_adj_output(ij_output,lev))
   icmr_b4_adj_output = icmr_input
   deallocate(icmr_input)

   allocate(grle_b4_adj_output(ij_output,lev))
   grle_b4_adj_output = grle_input
   deallocate(grle_input)

   allocate(cldamt_b4_adj_output(ij_output,lev))
   cldamt_b4_adj_output = cldamt_input
   deallocate(cldamt_input)

   allocate(ugrd_b4_adj_output(ij_output,lev))
   ugrd_b4_adj_output = ugrd_input
   deallocate(ugrd_input)

   allocate(vgrd_b4_adj_output(ij_output,lev))
   vgrd_b4_adj_output = vgrd_input
   deallocate(vgrd_input)

 else

   print*
   print*,'INTERPOLATE DATA TO OUTPUT GRID'


 ip    = 0   ! bilinear
 ipopt = 0

!----------------------------------------------------------------------------------
! Do 2-D fields first
!----------------------------------------------------------------------------------

 num_fields = 1

 allocate(ibi(num_fields))
 ibi = 0 ! no bitmap
 allocate(ibo(num_fields))
 ibo = 0 ! no bitmap

 allocate(bitmap_input(ij_input,num_fields))
 bitmap_input = .true.
 allocate(bitmap_output(ij_output,num_fields))
 bitmap_output = .true.

 allocate(rlat_output(ij_output))
 rlat_output = 0.0
 allocate(rlon_output(ij_output))
 rlon_output = 0.0

!----------------
! Surface height
!----------------

 allocate(data_input(ij_input,num_fields))
 data_input(:,num_fields) = hgt_input(:)
 deallocate(hgt_input)

 allocate(data_output(ij_output,num_fields))
 data_output = 0

 print*,"INTERPOLATE SURFACE HEIGHT"
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, data_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               data_output, iret)
 if (iret /= 0) goto 89

 allocate(hgt_output(ij_output))
 hgt_output = data_output(:,num_fields)

!------------------
! surface pressure
!------------------

 data_input(:,num_fields) = sfcp_input(:)
 deallocate(sfcp_input)

 print*,"INTERPOLATE SURFACE PRESSURE"
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, data_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               data_output, iret)
 if (iret /= 0) goto 89

 allocate(sfcp_b4_adj_output(ij_output))
 sfcp_b4_adj_output = data_output(:,num_fields)

 deallocate(ibi, ibo, bitmap_input, bitmap_output, data_input, data_output)

!----------------------------------------------------------------------------------
! 3d scalars
!----------------------------------------------------------------------------------

 num_fields = lev

 allocate(ibi(num_fields))
 ibi = 0 ! no bitmap
 allocate(ibo(num_fields))
 ibo = 0 ! no bitmap

 allocate(bitmap_input(ij_input,num_fields))
 bitmap_input = .true.
 allocate(bitmap_output(ij_output,num_fields))
 bitmap_output = .true.

!-------------
! Temperature
!-------------

 allocate(tmp_b4_adj_output(ij_output,num_fields))
 tmp_b4_adj_output = 0

 print*,'INTERPOLATE TEMPERATURE'
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, tmp_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               tmp_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate(tmp_input)

!--------------------
! Cloud liquid water
!--------------------

 allocate(clwmr_b4_adj_output(ij_output,num_fields))
 clwmr_b4_adj_output = 0

 print*,'INTERPOLATE CLOUD LIQUID WATER'
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, clwmr_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               clwmr_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate(clwmr_input)

!--------------------
! Specific humidity
!--------------------

 allocate(spfh_b4_adj_output(ij_output,num_fields))
 spfh_b4_adj_output = 0

 print*,'INTERPOLATE SPECIFIC HUMIDITY'
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, spfh_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               spfh_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate(spfh_input)

!-----------
! Ozone
!-----------

 allocate(o3mr_b4_adj_output(ij_output,num_fields))
 o3mr_b4_adj_output = 0

 print*,'INTERPOLATE OZONE'
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, o3mr_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               o3mr_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate(o3mr_input)

!-----------
! DZDT
!-----------

 allocate(dzdt_b4_adj_output(ij_output,num_fields))
 dzdt_b4_adj_output = 0

 print*,'INTERPOLATE DZDT'
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, dzdt_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               dzdt_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate(dzdt_input)

!----------------------------------------------------------------------------------
! Interpolate additional 3-d scalars for GFDL microphysics.
!----------------------------------------------------------------------------------


!-------------
!  Rain water
!-------------

   allocate(rwmr_b4_adj_output(ij_output,num_fields))
   rwmr_b4_adj_output = 0

   print*,'INTERPOLATE RWMR'
   call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
                 num_fields, ibi, bitmap_input, rwmr_input,  &
                 numpts, rlat_output, rlon_output, ibo, bitmap_output, &
                 rwmr_b4_adj_output, iret)
   if (iret /= 0) goto 89

   deallocate(rwmr_input)

!-------------
!  Snow water
!-------------

   allocate(snmr_b4_adj_output(ij_output,num_fields))
   snmr_b4_adj_output = 0

   print*,'INTERPOLATE SNMR'
   call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
                 num_fields, ibi, bitmap_input, snmr_input,  &
                 numpts, rlat_output, rlon_output, ibo, bitmap_output, &
                 snmr_b4_adj_output, iret)
   if (iret /= 0) goto 89

   deallocate(snmr_input)

!-------------
!  Ice water
!-------------

   allocate(icmr_b4_adj_output(ij_output,num_fields))
   icmr_b4_adj_output = 0

   print*,'INTERPOLATE ICMR'
   call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
                 num_fields, ibi, bitmap_input, icmr_input,  &
                 numpts, rlat_output, rlon_output, ibo, bitmap_output, &
                 icmr_b4_adj_output, iret)
   if (iret /= 0) goto 89

   deallocate(icmr_input)

!-------------
!  Graupel
!-------------

   allocate(grle_b4_adj_output(ij_output,num_fields))
   grle_b4_adj_output = 0

   print*,'INTERPOLATE GRLE'
   call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
                 num_fields, ibi, bitmap_input, grle_input,  &
                 numpts, rlat_output, rlon_output, ibo, bitmap_output, &
                 grle_b4_adj_output, iret)
   if (iret /= 0) goto 89

   deallocate(grle_input)


!---------------------------
!  Cloud amount
!---------------------------

   allocate(cldamt_b4_adj_output(ij_output,num_fields))
   cldamt_b4_adj_output = 0

   print*,'INTERPOLATE CLD_AMT'
   call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
        num_fields, ibi, bitmap_input, cldamt_input,  &
        numpts, rlat_output, rlon_output, ibo, bitmap_output, &
        cldamt_b4_adj_output, iret)
   if (iret /= 0) goto 89

   deallocate(cldamt_input)



!----------------------------------------------------------------------------------
! 3d u/v winds
!----------------------------------------------------------------------------------

 allocate(crot(ij_output), srot(ij_output))
 crot = 0.
 srot = 0.

 allocate(ugrd_b4_adj_output(ij_output,num_fields))
 ugrd_b4_adj_output = 0
 allocate(vgrd_b4_adj_output(ij_output,num_fields))
 vgrd_b4_adj_output = 0

 print*,'INTERPOLATE WINDS'
 call ipolatev(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, ugrd_input, vgrd_input,  &
               numpts, rlat_output, rlon_output, crot, srot, ibo, bitmap_output, &
               ugrd_b4_adj_output, vgrd_b4_adj_output, iret)
 if (iret /= 0) goto 89

 deallocate (ugrd_input, vgrd_input)
 deallocate (crot, srot)
 deallocate (ibi, ibo, bitmap_input, bitmap_output)

 endif

 return

 89 continue
 print*,"FATAL ERROR IN IPOLATES. IRET IS: ", iret
 call errexit(23)

 end subroutine gaus_to_gaus

 end module interp
