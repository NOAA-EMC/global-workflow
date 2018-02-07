cc    Machine dependant constants (*j*) added io2 20051101
cc
      integer    kind_io2,kind_io4,kind_io8
      integer    kind_evod
cc
      parameter (kind_io2 =2)
      parameter (kind_io4 =4)
      parameter (kind_io8 =8)
      parameter (kind_evod=8)
      real(kind=kind_evod) mprec ! machine precision to restrict dep
      parameter(mprec  = 1.e-12 )
