      module setparms
        integer :: real_single, real_double
        integer :: int_single, int_double
        parameter (real_single = 4, real_double = real_single * 2)
        parameter (int_single  = 4, int_double  = int_single * 2)
      end module setparms
