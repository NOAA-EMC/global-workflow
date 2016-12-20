c
c***********************************************************************
c
      subroutine out_para(dt)
c
c***********************************************************************
c
      use resol_def
      use layout1
      implicit none
c
      real dt
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      if (me.eq.0) then
        write(6,*)
        write(6,*)'this is the reduced-grid code with new phys'
        write(6,9100) jcap,levs
        write(6,9110) nodes
cjfe    write(6,9115) num_parthds()
        write(6,9120) levs,latg,lonf
        write(6,9140) dt
c        write(6,9130)
      endif
 
 9100 format( /5x,'   t',i4,'-',i3,' forecast model',
     .        /5x,' ===========================')
 9110 format(  5x,' number of mpi tasks:   ',i4)
 9115 format(  5x,' number of threads:     ',i4)
 9120 format( /5x,' number of levels:      ',i4,
     .        /5x,' number of latitudes:   ',i4,
     .        /5x,' number of longitudes:  ',i4)
 9130 format(//'                               performance'
     .        /'                             times in seconds'
     .        /'                        rates in mbyte/s or mflop/s'
     .       //'         transpose    gdsum',
     .         '         fft       flns2fg      fl2fln    fidi/phys'
     .        /' step   time  rate  time  rate ',
     .         ' time  rate  time  rate  time  rate  time  rate '
     .        /' --------------------------------------',
     .         '---------------------------------------')
 9140 format( /5x,' timestep:   ',f6.1,' seconds')
 
      return
      end
c
c***********************************************************************
c
      subroutine bar3(fe,fo,name,levl)
c
c***********************************************************************
c
      use resol_def
      use layout1
      implicit none
      real fe(len_trie_ls,2,levs),ffbar
      real fo(len_trio_ls,2,levs)
      integer i,jlev,levl
      character*(*) name
c
      do jlev=1,levl
        ffbar=0.
        do i=1,len_trie_ls
          ffbar=ffbar+fe(i,1,jlev)*fe(i,1,jlev)
        enddo
        do i=1,len_trie_ls
          ffbar=ffbar+fe(i,2,jlev)*fe(i,2,jlev)
        enddo
        ffbar=ffbar/2.
        do i=1,len_trio_ls
          ffbar=ffbar+fo(i,1,jlev)*fo(i,1,jlev)
        enddo
        do i=1,len_trio_ls
          ffbar=ffbar+fo(i,2,jlev)*fo(i,2,jlev)
        enddo
        ffbar=sqrt(ffbar)
        write(*,101)'rms ',name,' lev ',jlev,' = ',ffbar
      enddo
c
 101  format(3a,i3,a,es24.17)
c
      return
      end
