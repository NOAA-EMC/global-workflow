      module namelist_soilveg
      implicit none
      save

      integer max_slopetyp
      integer max_soiltyp
      integer max_vegtyp

      parameter(max_slopetyp = 30)
      parameter(max_soiltyp = 30)
      parameter(max_vegtyp = 30)

      real slope_data(max_slopetyp)
      real rsmtbl(max_vegtyp)
      real rgltbl(max_vegtyp)
      real hstbl(max_vegtyp)
      real snupx(max_vegtyp)
      real bb(max_soiltyp)
      real drysmc(max_soiltyp)
      real f11(max_soiltyp)
      real maxsmc(max_soiltyp)
      real refsmc(max_soiltyp)
      real satpsi(max_soiltyp)
      real satdk(max_soiltyp)
      real satdw(max_soiltyp)
      real wltsmc(max_soiltyp)
      real qtz(max_soiltyp)
      logical lparam
      real zbot_data
      real salp_data
      real cfactr_data
      real cmcmax_data
      real sbeta_data
      real rsmax_data
      real topt_data
      real refdk_data
      real frzk_data
      integer bare
      integer defined_veg
      integer defined_soil
      integer defined_slope
      real fxexp_data
      integer nroot_data(max_vegtyp)
      real refkdt_data
      real z0_data(max_vegtyp)
      real czil_data
      real lai_data(max_vegtyp)
      real csoil_data
      end module namelist_soilveg
