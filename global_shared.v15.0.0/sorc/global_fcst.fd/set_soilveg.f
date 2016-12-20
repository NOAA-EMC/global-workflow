      subroutine set_soilveg(me,nlunit)
      use namelist_soilveg
      implicit none

      integer, intent(in) :: nlunit
      integer me
cmy begin locals
      integer i
      real wltsmc1,refsmc1
c ----------------------------------------------------------------------
c set two soil moisture wilt, soil moisture reference parameters
c ----------------------------------------------------------------------
      real smlow
      real smlow_data
      data smlow_data /0.5/

      real smhigh
      real smhigh_data
c     changed in 2.6 from 3 to 6 on june 2nd 2003
c      data smhigh_data /3.0/
      data smhigh_data /6.0/
      namelist /soil_veg/ slope_data, rsmtbl, rgltbl, hstbl, snupx,
     &  bb, drysmc, f11, maxsmc, refsmc, satpsi, satdk, satdw,
     &  wltsmc, qtz, lparam, zbot_data, salp_data, cfactr_data,
     &  cmcmax_data, sbeta_data, rsmax_data, topt_data,
     &  refdk_data, frzk_data, bare, defined_veg, defined_soil,
     &  defined_slope, fxexp_data, nroot_data, refkdt_data, z0_data,
     &  czil_data, lai_data, csoil_data

cmy end locals
      slope_data =(/0.1,  0.6, 1.0, 0.35, 0.55, 0.8,
     &  	       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/)
c     rsmtbl =(/150.0, 100.0, 125.0, 150.0, 100.0, 70.0,
c    &  	    40.0, 300.0, 400.0, 150.0, 400.0, 40.0,
c    &  	   150.0,   0.0,   0.0,   0.0,   0.0,  0.0,
c    &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
c    &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/)
chelin new table
!     rsmtbl =(/500.0, 300.0, 300.0, 300.0, 300.0, 100.0,
!     rsmtbl =(/500.0, 175.0, 175.0, 300.0, 300.0, 70.0,
      rsmtbl =(/300.0, 175.0, 175.0, 300.0, 300.0, 70.0,
     &              20.0, 225.0, 225.0, 225.0, 400.0, 20.0,
     &  	   150.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/)
c-----------------------------
      rgltbl =(/30.0,  30.0,  30.0,  30.0,  30.0,  65.0,
     &  	  100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
     &  	  100.0,   0.0,   0.0,   0.0,	0.0,   0.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0/)
      hstbl =(/41.69, 54.53, 51.93, 47.35,  47.35, 54.53,
     &  	  36.35, 42.00, 42.00, 42.00,  42.00, 36.35,
     &  	  42.00,  0.00,  0.00,  0.00,	0.00,  0.00,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00/)
c     changed for version 2.6 on june 2nd 2003
c      data snupx  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,
c     &  	   0.040, 0.040, 0.040, 0.040, 0.025, 0.040,
c     &  	   0.025, 0.000, 0.000, 0.000, 0.000, 0.000,
      snupx  =(/0.040, 0.040, 0.040, 0.040, 0.040, 0.040,
     *             0.020, 0.020, 0.020, 0.020, 0.013, 0.020,
     *             0.013, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
      bb	 =(/4.26,  8.72, 11.55, 4.74, 10.73,  8.17,
     &  	  6.77,  5.25,  4.26, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00/)
c !!!!!!!!!!!!!! the following values in the table are not used
c !!!!!!!!!!!!!! and are just given for reference
      drysmc=(/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
c !!!!!!!!!!!!!! the following values in the table are not used
c !!!!!!!!!!!!!! and are just given for reference
      f11  =(/-0.999, -1.116, -2.137, -0.572, -3.201, -1.302,
     &  	 -1.519, -0.329, -0.999,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/)
      maxsmc=(/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
     &  	  0.404, 0.439, 0.421, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
c
c ----------------------------------------------------------------------
c the following 5 parameters are derived later in redprm.f from the soil
c data, and are just given here for reference and to force static
c storage allocation. -dag lohmann, feb. 2001
c ----------------------------------------------------------------------
c      data refsmc/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,
c     &  	  0.315, 0.329, 0.283, 0.000, 0.000, 0.000,
c !!!!!!!!!!!!!! the following values in the table are not used
c !!!!!!!!!!!!!! and are just given for reference
      refsmc=(/0.248, 0.368, 0.398, 0.281, 0.321, 0.361,
     &            0.293, 0.301, 0.248, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
c ----------------------------------------------------------------------
c soil texture-related arrays.
c ----------------------------------------------------------------------
      satpsi=(/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,
     &  	  0.14, 0.36, 0.04, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/)
      satdk =(/1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
     &  	  0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5, 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/)
      qtz   =(/0.82, 0.10, 0.25, 0.60, 0.52, 0.35,
     &  	  0.60, 0.40, 0.82, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/)

c !!!!!!!!!!!!!! the following values in the table are not used
c !!!!!!!!!!!!!! and are just given for reference
      wltsmc=(/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
c !!!!!!!!!!!!!! the following values in the table are not used
c !!!!!!!!!!!!!! and are just given for reference
      satdw =(/5.71e-6, 2.33e-5, 1.16e-5, 7.95e-6, 1.90e-5,
     &  	  1.14e-5, 1.06e-5, 1.46e-5, 5.71e-6, 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/)

      lparam =.true.
c     changed for version 2.5.2
c      data zbot_data /-3.0/
      zbot_data =-8.0
c     changed for version 2.6 june 2nd 2003
c      data salp_data /2.6/
      salp_data =4.0
      cfactr_data =0.5
      cmcmax_data =0.5e-3
      sbeta_data =-2.0
      rsmax_data =5000.0
      topt_data =298.0
      refdk_data =2.0e-6
      frzk_data =0.15
      bare =11

c ----------------------------------------------------------------------
c number of defined soil-, veg-, and slopetyps used.
c ----------------------------------------------------------------------

      defined_veg=13
      defined_soil=9
      defined_slope=9

      fxexp_data =2.0
      nroot_data =(/4,4,4,4,4,4,3,3,3,2,3,3,2,0,0,
     &  	       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      refkdt_data =3.0
c ----------------------------------------------------------------------
c vegetation class-related arrays
c ----------------------------------------------------------------------
      z0_data =(/2.653, 0.826, 0.563, 1.089, 0.854, 0.856,
     &  	    0.035, 0.238, 0.065, 0.076, 0.011, 0.125,
     &  	    0.011, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
c   changed in version 2.6 june 2nd 2003
c      data czil_data /0.2/
      czil_data =0.075

clu: change to 3 or 2   oct 15, 2004
      lai_data =(/3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     &  	     3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     &  	     3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
c      data csoil_data /1.26e+6/
      csoil_data =2.00e+6
c ----------------------------------------------------------------------
c read namelist file to override default parameters only once.
c namelist_name must be 50 characters or less.
c ----------------------------------------------------------------------
clu: namelist is set up in run script
         if (me .eq. 0) write(*,*) 'read namelist soil_veg'
c$$$         read(5, soil_veg)
         rewind(nlunit)
         read(nlunit, soil_veg)
!*       write(6, soil_veg)
c 	 open(58, file = 'namelist_filename.txt')
c         read(58,'(a)') namelist_name
c         close(58)
c         write(*,*) 'namelist filename is ', namelist_name
c         open(59, file = namelist_name)
c 50      continue
c         read(59, soil_veg, end=100)
c         if (lparam) goto 50
c 100     continue
c         close(59)
         if (defined_soil .gt. max_soiltyp) then
            write(*,*) 'warning: defined_soil too large in namelist'
            stop 222
         endif
         if (defined_veg .gt. max_vegtyp) then
            write(*,*) 'warning: defined_veg too large in namelist'
            stop 222
         endif
         if (defined_slope .gt. max_slopetyp) then
            write(*,*) 'warning: defined_slope too large in namelist'
            stop 222
         endif
         
         smlow = smlow_data
         smhigh = smhigh_data
         
         do i = 1,defined_soil
            satdw(i)  = bb(i)*satdk(i)*(satpsi(i)/maxsmc(i))
            f11(i) = alog10(satpsi(i)) + bb(i)*alog10(maxsmc(i)) + 2.0
            refsmc1 = maxsmc(i)*(5.79e-9/satdk(i))
     &           **(1.0/(2.0*bb(i)+3.0))
            refsmc(i) = refsmc1 + (maxsmc(i)-refsmc1) / smhigh
            wltsmc1 = maxsmc(i) * (200.0/satpsi(i))**(-1.0/bb(i))
            wltsmc(i) = wltsmc1 - smlow * wltsmc1
            
c     ----------------------------------------------------------------------
c     current version drysmc values that equate to wltsmc.
c     future version could let drysmc be independently set via namelist.
c     ----------------------------------------------------------------------
            drysmc(i) = wltsmc(i)
         end do
         
!     if (me.eq.0) write(6,soil_veg)
       return
       end
