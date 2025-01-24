Module SSMIS_Spatial_Average_Mod
!
!
! abstract:  This routine reads BUFR format SSMIS radiance 
!            (brightness temperature) files, spatially 
!            averages the data using the AAPP averaging routines
!            and writes the data back into BUFR format
!
!
! Program history log:
!    2011-11-18   collard   - Original version (for ATMS)
!    2011-12-20   eliu      - Modify to apply for SSMIS 
!    2016-03-03   ejones    - Add option for spatial averaging of GMI
!    2016-03-24   ejones    - Add option for spatial averaging of AMSR2
! 

  use kinds, only: r_kind,r_double,i_kind
  use m_uniq
  use m_distance

  implicit none     

! Declare module level parameters
  real(r_kind), parameter    :: Missing_Value=1.e11_r_double

CONTAINS 

  SUBROUTINE SSMIS_Spatial_Average(BufrSat, Method, Num_Obs, NChanl,  &
                                   FOV, Node_InOut, Time, Lat, Lon, BT_InOut, Error_status)

    IMPLICIT NONE
    
    ! Declare passed variables
    integer(i_kind) ,intent(in   ) :: BufrSat  
    integer(i_kind) ,intent(in   ) :: Method        ! 1=simple(1), 2=simple(2), 3=AAPP, 4=GMI (simple 1), 5=AMSR2 (simple1)  
    integer(i_kind) ,intent(in   ) :: Num_Obs, NChanl
    integer(i_kind) ,intent(in   ) :: Fov(num_obs)
    integer(i_kind) ,intent(inout) :: Node_InOut(num_obs)
    real(r_kind)    ,intent(in   ) :: Time(Num_Obs)
    real(r_kind)    ,intent(in   ) :: Lat(Num_Obs)
    real(r_kind)    ,intent(in   ) :: Lon(Num_Obs)
    real(r_kind)    ,intent(inout) :: BT_InOut(NChanl,Num_Obs)
    integer(i_kind) ,intent(  out) :: Error_Status

    ! Declare local parameters
!   integer(i_kind), parameter :: atms1c_h_wmosatid=224
    integer(i_kind), parameter :: lninfile=15
!   integer(i_kind), parameter :: max_fov=96
    integer(i_kind), parameter :: max_fov=60
    integer(i_kind), parameter :: max_fov_gmi=221
    integer(i_kind), parameter :: max_fov_amsr2=243
!   integer(i_kind), parameter :: max_obs=20000000
    integer(i_kind), parameter :: as_node= 1_i_kind
    integer(i_kind), parameter :: ds_node=-1_i_kind
    real(r_kind),    parameter :: btmin=70.0_r_kind
    real(r_kind),    parameter :: btmax=320.0_r_kind
    real(r_kind),    parameter :: sigma = 1.0_r_kind/25.0_r_kind    
!   real(r_kind),    parameter :: sigma = 1.0_r_kind/50.0_r_kind  

    ! Declare local variables
    character(100) :: infile 
    character(30)  :: Cline

    integer(i_kind) :: i,iscan,ifov,ichan,nchannels,wmosatid,version
    integer(i_kind) :: is,ip,ic 
    integer(i_kind) :: iobs   
    integer(i_kind) :: ios,max_scan,min_scan
    integer(i_kind) :: ns1,ns2,np1,np2 
    integer(i_kind) :: nscan       
    integer(i_kind) :: ntime_scan   
    integer(i_kind) :: delta_scan   
    integer(i_kind) :: scanline_new 
    integer(i_kind) :: nxaverage(nchanl),nyaverage(nchanl)
    integer(i_kind) :: channelnumber(nchanl)
    integer(i_kind) :: qc_distx(nchanl),qc_disty(nchanl)
    integer(i_kind), allocatable ::  nodeinfo(:,:)
    integer(i_kind), allocatable ::  scanline(:),scanline_back(:,:)
    real(r_kind), allocatable ::  latitude(:,:), longitude(:,:)
    real(r_kind), allocatable ::  time_scan(:)
    real(r_kind), allocatable ::  dist_scan(:)
    real(r_kind), allocatable ::  dist_scan_avg(:)
    real(r_kind), allocatable ::  alat(:),alon(:),blat(:),blon(:)
    real(r_kind) :: dlat
!   real(r_kind) :: time1,time2

    real(r_kind) :: sampling_distx,sampling_disty,beamwidth(nchanl) 
    real(r_kind) :: newwidth(nchanl),cutoff(nchanl)
    real(r_kind), allocatable, target :: bt_image(:,:,:)
    real(r_kind), allocatable :: bt_image_orig(:,:,:)
    real(r_kind), pointer :: bt_image1(:,:)
    real(r_kind) :: t1,t2,tdiff   
    real(r_kind) :: lat1,lat2,lon1,lon2,dist,wgt 
    real(r_kind) :: xnum,mta   
    logical      :: gaussian_wgt
  
    Error_Status=0

    if (Method == 1) then  ! simple averaging 1

       gaussian_wgt = .false.
!      write(*,*) 'SSMIS_Spatial_Average: using method from Banghua'
       write(*,*) 'SSMIS_Spatial_Average: bufrsat = ', BufrSat
       write(*,*) 'SSMIS_Spatial_Average: Gaussian Weighted Averaging = ', gaussian_wgt 

       ! Determine scanline from time
       !==============================
       allocate(scanline(num_obs))
       t1          = time(1)  ! time for first scanline
       nscan       = 1        ! first scanline
       scanline(1) = nscan
       do iobs = 2, num_obs
          t2    = time(iobs) 
          tdiff = t2-t1
          if (tdiff >= 0.00001_r_kind) then
             nscan = nscan+1 
             t1    = t2
          endif
          scanline(iobs) = nscan
       enddo
       max_scan = maxval(scanline)
       write(*,*) 'SSMIS_Spatial_Average: max_scan,max_fov,nchanl = ', &
                 max_scan,max_fov,nchanl

!      Allocate and initialize variables
       allocate(bt_image_orig(max_fov,max_scan,nchanl))
       allocate(latitude(max_fov,max_scan))
       allocate(longitude(max_fov,max_scan))
       allocate(nodeinfo(max_fov,max_scan))
       allocate(scanline_back(max_fov,max_scan))
       bt_image_orig(:,:,:)= 1000.0_r_kind
       latitude(:,:)       = 1000.0_r_kind 
       longitude(:,:)      = 1000.0_r_kind 
       scanline_back(:,:)  = -1
       nodeinfo(:,:)       = 1000_i_kind 

!      Put data into 2D (fov vs. scanline) array
       do iobs = 1, num_obs
          latitude(fov(iobs),scanline(iobs))       = lat(iobs) 
          longitude(fov(iobs),scanline(iobs))      = lon(iobs) 
          bt_image_orig(fov(iobs),scanline(iobs),:)= bt_inout(:,iobs)
          scanline_back(fov(iobs),scanline(iobs))  = iobs
       enddo

!      Determine AS/DS node information for each scanline
       loop1: do iscan = 1, max_scan-1
          loop2: do ifov = 1, max_fov
             if (scanline_back(ifov,iscan) > 0 .and. scanline_back(ifov,iscan+1) > 0) then
                dlat = latitude(ifov,iscan+1)-latitude(ifov,iscan)
                if (dlat < 0.0_r_kind) then
                   nodeinfo(:,iscan) = ds_node 
                else
                   nodeinfo(:,iscan) = as_node 
                endif
                cycle loop1
             endif
          enddo loop2
       enddo loop1
       nodeinfo(:,max_scan) = nodeinfo(:,max_scan-1)

!      Do spatial averaging in the box centered on each fov for each channel
!$omp parallel do  schedule(dynamic,1)private(ic,iobs,iscan,ifov,ns1,ns2,np1,np2,xnum,mta,is,ip,lat1,lon1,lat2,lon2,dist,wgt)
       scan_loop: do iscan = 1, max_scan 
          fov_loop: do ifov = 1, max_fov 

             iobs = scanline_back(ifov,iscan) 
             if (iobs >0) then 
                node_inout(iobs) = nodeinfo(ifov,iscan)
!               Define grid box (3 (scan direction) x 7 (satellite track dir))
                ns1 = iscan-3          
                ns2 = iscan+3          
                if (ns1 < 1) ns1=1
                if (ns2 > max_scan) ns2=max_scan
                np1 = ifov-1          
                np2 = ifov+1          
                if (np1 < 1) np1=1
                if (np2 > max_fov) np2=max_fov

                channel_loop: do ic = 1, nchanl  
                   xnum   = 0.0_r_kind
                   mta    = 0.0_r_kind
                   if (any(bt_image_orig(np1:np2,ns1:ns2,ic) < btmin .or. &
                           bt_image_orig(np1:np1,ns1:ns2,ic) > btmax)) then 
                      bt_inout(ic,iobs) = 1000.0_r_kind 
                   else
                     ! Calculate distance of each fov to the center fov 
                      box_y1: do is = ns1, ns2 
                      box_x1: do ip = np1, np2 
                         lat1 = latitude(ifov,iscan)    ! lat of the center fov
                         lon1 = longitude(ifov,iscan)   ! lon of the center fov
                         lat2 = latitude(ip,is)          
                         lon2 = longitude(ip,is)
                         dist = distance(lat1,lon1,lat2,lon2) 
                         if (dist > 100.0_r_kind) cycle box_x1  ! outside the box 
                         if (gaussian_wgt) then
                            wgt = exp(-0.5_r_kind*(dist*sigma)*(dist*sigma))
                         else
                            wgt = 1.0
                         endif
                         xnum   = xnum+wgt
                         mta    = mta +wgt*bt_image_orig(ip,is,ic)
                      enddo box_x1
                      enddo box_y1
                      bt_inout(ic,iobs) = mta/xnum
                   endif
                enddo channel_loop 
             endif
          enddo fov_loop
       enddo scan_loop

!      Deallocate arrays
       deallocate(nodeinfo,scanline,scanline_back)
       deallocate(latitude,longitude)
       deallocate(bt_image_orig)

!============================================================================================================

!   Simple method 2 
    else if (Method == 2) then  ! simple averaging 2 

       gaussian_wgt = .false.
       write(*,*) 'SSMIS_Spatial_Average: using method from Emily'
       write(*,*) 'SSMIS_Spatial_Average: bufrsat = ', BufrSat
       write(*,*) 'SSMIS_Spatial_Avearge: Gaussian Weighted Averaging = ', gaussian_wgt 

!      Determine scanline index from time
!      Each uniq time is one uniq scanline
!      call cpu_time(time1)
       ntime_scan = nuniq(time(1:num_obs))
       allocate(scanline(num_obs))
       allocate(time_scan(ntime_scan))
       time_scan(1:ntime_scan) = time(uniq(time(1:num_obs),ntime_scan))
       do iscan = 1, ntime_scan
          where((abs(time(1:num_obs)-time_scan(iscan)))<=0.0001_r_kind) scanline = iscan
       enddo
       max_scan = maxval(scanline)
!      call cpu_time(time2)
!      write(*,*)'CPU time for determining scanline index = ', time2-time1
!      write(*,*)'determine scanline index from time'
!      write(*,*)'ntime_scan        = ', ntime_scan
!      write(*,*)'min/max scanline  = ', minval(scanline), maxval(scanline)
!      write(*,*)'min/max time      = ', minval(time), maxval(time)
!      write(*,*)'min/max time_scan = ', minval(time_scan), maxval(time_scan)

!      Put lat/lon in 2D (fov,scanline) grid to calculate distance between scanlines
!      call cpu_time(time1)
       allocate(latitude(max_fov,max_scan))
       allocate(longitude(max_fov,max_scan))
       allocate(scanline_back(max_fov,max_scan))
       latitude(:,:)      = 9999.0_r_kind   ! filled value
       longitude(:,:)     = 9999.0_r_kind   ! filled value
       scanline_back(:,:) = -1              ! filled value
       do iobs = 1, num_obs
          latitude(fov(iobs),scanline(iobs))     = lat(iobs)
          longitude(fov(iobs),scanline(iobs))    = lon(iobs)
          scanline_back(fov(iobs),scanline(iobs))= iobs
       enddo

!      Calculate distance between scanlines
!      Reassign scanline according to distance between scanlines
       allocate(dist_scan(max_fov))
       allocate(dist_scan_avg(max_scan))
       dist_scan(:)     = 0.0_r_kind
       dist_scan_avg(:) = 0.0_r_kind
       allocate(alat(max_fov),blat(max_fov),alon(max_fov),blon(max_fov))
       scanline_new = 1
       do iscan = 2, max_scan   ! loop through scanlines
          dist_scan(:) = 0.0_r_kind
          alat(:) = latitude(:,iscan-1)
          alon(:) = longitude(:,iscan-1)
          blat(:) = latitude(:,iscan)
          blon(:) = longitude(:,iscan)
!         where(alat>=1000.0_r_kind .or. alon>=1000.0_r_kind .or. blat>=1000.0_r_kind .or. blon>=1000.0_r_kind) ! missing FOVs
          where(scanline_back(:,iscan) <= 0 .or. scanline_back(:,iscan-1) <= 0) ! missing FOVs
             alat(:)=0.0_r_kind
             blat(:)=0.0_r_kind
             alon(:)=0.0_r_kind
             blon(:)=0.0_r_kind
          end where
          dist_scan            = distance(alat,alon,blat,blon)
          dist_scan_avg(iscan) = sum(dist_scan)/count(dist_scan>0.0000001_r_kind)
          delta_scan           = nint(dist_scan_avg(iscan)/12.5_r_kind)
          scanline_new         = scanline_new+delta_scan
          do ifov = 1, max_fov
             iobs = scanline_back(ifov,iscan)
             if (iobs > 0)  scanline(iobs) = scanline_new
          enddo
       enddo
       max_scan = maxval(scanline)
       min_scan = minval(scanline)
!      call cpu_time(time2)
!      write(*,*) 'CPU time for determining dist and reassign scanlines = ', time2-time1
!      write(*,*) 'SSMIS_Spatial_Average: reassigned max_scan = ', max_scan
!      write(*,*) 'SSMIS_Spatial_Average: reassigned max_fov  = ', max_fov
!      write(*,*) 'SSMIS_Spatial_Average: reassigned nchanl   = ', nchanl

       deallocate(time_scan,dist_scan,dist_scan_avg)
       deallocate(latitude,longitude,scanline_back)
       deallocate(alat,alon,blat,blon)

!      Allocate and initialize variables
       allocate(latitude(max_fov,max_scan))
       allocate(longitude(max_fov,max_scan))
       allocate(bt_image(max_fov,max_scan,nchanl))
       allocate(bt_image_orig(max_fov,max_scan,nchanl)) 
       allocate(nodeinfo(max_fov,max_scan))
       allocate(scanline_back(max_fov,max_scan))
       bt_image(:,:,:)     = 1000.0_r_kind
       bt_image_orig(:,:,:)= 1000.0_r_kind
       latitude(:,:)       = 1000.0_r_kind
       longitude(:,:)      = 1000.0_r_kind
       scanline_back(:,:)  = -1
       nodeinfo(:,:)       = 1000_i_kind

!      Put data in 2D (fov,scanline) grid to perform noise reduction
       do i=1,num_obs
          latitude(fov(i),scanline(i))       = lat(i)
          longitude(fov(i),scanline(i))      = lon(i)
          bt_image(fov(i),scanline(i),:)     = bt_inout(:,i)
          bt_image_orig(fov(i),scanline(i),:)= bt_inout(:,i) 
          scanline_back(fov(i),scanline(i))  = i
       enddo 

!      Determine AS/DS node information for each scanline
       loop3: do iscan = 1, max_scan-1
          loop4: do ifov = 1, max_fov
             if (scanline_back(ifov,iscan) > 0 .and. scanline_back(ifov,iscan+1) > 0) then
                dlat = latitude(ifov,iscan+1)-latitude(ifov,iscan)
                if (dlat < 0.0_r_kind) then
                   nodeinfo(:,iscan) = ds_node
                else
                   nodeinfo(:,iscan) = as_node
                endif
                cycle loop3
             endif
          enddo loop4
       enddo loop3
       nodeinfo(:,max_scan) = nodeinfo(:,max_scan-1)

!      write(*,*) 'ssmis_spatial_average'
!      write(*,*) 'min/max fov           = ', minval(fov), maxval(fov)
!      write(*,*) 'min/max time          = ', minval(time), maxval(time)
!      write(*,*) 'min/max scanline_back = ', minval(scanline_back), maxval(scanline_back)
!      write(*,*) 'min/max scanline      = ', minval(scanline), maxval(scanline)
!      write(*,*) 'min/max scan          = ', minval(scan), maxval(scan)

!      Do spatial averaging in the box centered on each fov for each channel
       write(*,*) 'SSMIS_Spatial_Average: do spatial averaging for noise reduction '
       scan_loop2: do iscan = 1, max_scan
          fov_loop2: do ifov = 1, max_fov

!            Define grid box (3 (scan direction) x 7 (satellite track dir))
             ns1 = iscan-3
             ns2 = iscan+3
             if (ns1 < 1) ns1=1
             if (ns2 > max_scan) ns2=max_scan
             np1 = ifov-1
             np2 = ifov+1
             if (np1 < 1) np1=1
             if (np2 > max_fov) np2=max_fov

             channel_loop2: do ic = 1, nchanl
                xnum   = 0.0_r_kind
                mta    = 0.0_r_kind
                if (any(bt_image_orig(np1:np2,ns1:ns2,ic) < btmin .or. bt_image_orig(np1:np1,ns1:ns2,ic) > btmax)) then
                   bt_image(ifov,iscan,ic) = 1000.0_r_kind
                else
                  ! Calculate distance of each fov to the center fov
                   box_y2: do is = ns1, ns2
                   box_x2: do ip = np1, np2
                      lat1 = latitude(ifov,iscan)    ! lat of the center fov
                      lon1 = longitude(ifov,iscan)   ! lon of the center fov
                      lat2 = latitude(ip,is)
                      lon2 = longitude(ip,is)
                      dist = distance(lat1,lon1,lat2,lon2)
                      if (dist > 100.0_r_kind) cycle box_x2  ! outside the box
                      if (gaussian_wgt) then
                         wgt = exp(-0.5_r_kind*(dist*sigma)*(dist*sigma))
                      else
                         wgt = 1.0
                      endif
                      xnum   = xnum+wgt
                      mta    = mta +wgt*bt_image_orig(ip,is,ic)
                   enddo box_x2
                   enddo box_y2
                   bt_image(ifov,iscan,ic) = mta/xnum
                endif
             enddo channel_loop2

          enddo fov_loop2
       enddo scan_loop2

       do iscan = 1, max_scan
          do ifov = 1, max_fov
             if (scanline_back(ifov,iscan) >0) then 
                bt_inout(:,scanline_back(ifov,iscan)) = bt_image(ifov,iscan,:)
                node_inout(scanline_back(ifov,iscan)) = nodeinfo(ifov,iscan)
             endif
          enddo
       enddo
       deallocate(nodeinfo,scanline,scanline_back)
       deallocate(latitude,longitude)
       deallocate(bt_image_orig,bt_image)


!============================================================================================================

    else if (Method == 3) then  !  AAPP method

       write(*,*) 'SSMIS_Spatial_Average: using AAPP method'
       if (bufrsat == 249) infile= 'ssmis_f16_beamwidth.txt'
       if (bufrsat == 285) infile= 'ssmis_f17_beamwidth.txt'
       if (bufrsat == 286) infile= 'ssmis_f18_beamwidth.txt'
       if (bufrsat == 287) infile= 'ssmis_f19_beamwidth.txt'

       ! Read the beamwidth requirements
       OPEN(lninfile,file=infile,form='formatted',status='old', &
            iostat=ios)
       IF (ios /= 0) THEN
          WRITE(*,*) 'Unable to open ',trim(infile)
          Error_Status=1
          RETURN
       ENDIF
       wmosatid=999
       read(lninfile,'(a30)',iostat=ios) Cline
       DO WHILE (wmosatid .NE. BufrSat .AND. ios == 0)
          DO WHILE (Cline(1:1) == '#')
             read(lninfile,'(a30)') Cline
          ENDDO
          READ(Cline,*) wmosatid
       
          read(lninfile,'(a30)') Cline
          DO WHILE (Cline(1:1) == '#')
             read(lninfile,'(a30)') Cline
          ENDDO
          READ(Cline,*) version
       
          read(lninfile,'(a30)') Cline
          DO WHILE (Cline(1:1) == '#')
             read(lninfile,'(a30)') Cline
          ENDDO
          READ(Cline,*) sampling_distx, sampling_disty
       
          read(lninfile,'(a30)') Cline
          DO WHILE (Cline(1:1) == '#')
             read(lninfile,'(a30)') Cline
          ENDDO
          READ(Cline,*) nchannels
      
          read(lninfile,'(a30)') Cline
          if (nchannels > 0) then 
             DO ichan=1,nchannels
                read(lninfile,'(a30)') Cline
                DO WHILE (Cline(1:1) == '#')
                  read(lninfile,'(a30)') Cline
                ENDDO
                READ(Cline,*) channelnumber(ichan),beamwidth(ichan), &
                      &       newwidth(ichan),cutoff(ichan),nxaverage(ichan), &
                      &       nyaverage(ichan), qc_distx(ichan), qc_disty(ichan) 
             ENDDO
          end if
          read(lninfile,'(a30)',iostat=ios) Cline
       ENDDO
       IF (wmosatid /= BufrSat) THEN
          WRITE(*,*) 'SSMIS_Spatial_Averaging: sat id not matched in ', trim(infile)
          Error_Status=1
          RETURN
       ENDIF
       CLOSE(lninfile)
 
!orig for ATMS
!      ! Determine scanline from time
!      MinTime = MINVAL(Time)
!      ALLOCATE(Scanline(Num_Obs))
!      Scanline(:)   = NINT((Time(1:Num_Obs)-MinTime)/Scan_Interval)+1
!      Max_Scan=MAXVAL(Scanline)
!      Min_Scan=MINVAL(Scanline)

!      Determine scanline index from time
!      Each uniq time is one uniq scanline 
!      call cpu_time(time1)
       ntime_scan = nuniq(time(1:num_obs))
       allocate(scanline(num_obs))
       allocate(time_scan(ntime_scan))
       time_scan(1:ntime_scan) = time(uniq(time(1:num_obs),ntime_scan))
       do iscan = 1, ntime_scan
          where((abs(time(1:num_obs)-time_scan(iscan)))<=0.0001_r_kind) scanline = iscan
       enddo
       max_scan = maxval(scanline)
!      call cpu_time(time2)
!      write(*,*)'CPU time for determining scanline index = ', time2-time1 
!      write(*,*)'determine scanline index from time' 
!      write(*,*)'ntime_scan        = ', ntime_scan
!      write(*,*)'min/max scanline  = ', minval(scanline), maxval(scanline)
!      write(*,*)'min/max time      = ', minval(time), maxval(time)
!      write(*,*)'min/max time_scan = ', minval(time_scan), maxval(time_scan)

!      Put lat/lon in 2D (fov,scanline) grid to calculate distance between scanlines

!      call cpu_time(time1)
       allocate(latitude(max_fov,max_scan))
       allocate(longitude(max_fov,max_scan))
       allocate(scanline_back(max_fov,max_scan))
       latitude(:,:)      = 9999.0_r_kind   ! filled value
       longitude(:,:)     = 9999.0_r_kind   ! filled value
       scanline_back(:,:) = -1              ! filled value
       do iobs = 1, num_obs
          latitude(fov(iobs),scanline(iobs))     = lat(iobs)
          longitude(fov(iobs),scanline(iobs))    = lon(iobs)
          scanline_back(fov(iobs),scanline(iobs))= iobs
       enddo

!      Calculate distance between scanlines
!      Reassign scanline according to distance between scanlines

       allocate(dist_scan(max_fov))
       allocate(dist_scan_avg(max_scan))
       dist_scan(:)     = 0.0_r_kind
       dist_scan_avg(:) = 0.0_r_kind
       allocate(alat(max_fov),blat(max_fov),alon(max_fov),blon(max_fov))
       scanline_new = 1
       do iscan = 2, max_scan   ! loop through scanlines
          dist_scan(:) = 0.0_r_kind
          alat(:) = latitude(:,iscan-1)
          alon(:) = longitude(:,iscan-1)
          blat(:) = latitude(:,iscan)
          blon(:) = longitude(:,iscan)
!         where(alat>=1000.0_r_kind .or. alon>=1000.0_r_kind .or. blat>=1000.0_r_kind .or. blon>=1000.0_r_kind) ! missing FOVs
          where(scanline_back(:,iscan) <= 0 .or. scanline_back(:,iscan-1) <= 0) ! missing FOVs
             alat(:)=0.0_r_kind
             blat(:)=0.0_r_kind 
             alon(:)=0.0_r_kind 
             blon(:)=0.0_r_kind
          end where
          dist_scan            = distance(alat,alon,blat,blon)
          dist_scan_avg(iscan) = sum(dist_scan)/count(dist_scan>0.0000001_r_kind)
          delta_scan           = nint(dist_scan_avg(iscan)/12.5_r_kind)
          scanline_new         = scanline_new+delta_scan
          do ifov = 1, max_fov
             iobs = scanline_back(ifov,iscan)
             if (iobs > 0)  scanline(iobs) = scanline_new 
          enddo
       enddo
       max_scan = maxval(scanline)
       min_scan = minval(scanline)
!      call cpu_time(time2)
!      write(*,*)'CPU time for determining dist and reassign scanlines = ', time2-time1 
!      write(*,*) 'SSMIS_Spatial_Average: reassigned max_scan = ', max_scan
!      write(*,*) 'SSMIS_Spatial_Average: reassigned max_fov  = ', max_fov
!      write(*,*) 'SSMIS_Spatial_Average: reassigned nchanl   = ', nchanl

       deallocate(time_scan,dist_scan,dist_scan_avg)
       deallocate(latitude,longitude,scanline_back)

!      Put lat/lon in 2D (fov,scanline) grid to perform noise reduction 
!      call cpu_time(time1)
       ALLOCATE(BT_Image(Max_FOV,Max_Scan,nchanl))
       ALLOCATE(BT_Image_Orig(Max_FOV,Max_Scan,nchanl)) 
       ALLOCATE(Scanline_Back(Max_FOV,Max_Scan))
       BT_Image(:,:,:) = 1000.0_r_kind
    
       ScanLine_Back(:,:) = -1
       DO I=1,Num_Obs
          bt_image(FOV(I),Scanline(I),:)=bt_inout(:,I)
          bt_image_orig(FOV(I),Scanline(I),:)=bt_inout(:,I) 
          Scanline_Back(FOV(I),Scanline(I))=I
       END DO

!      write(*,*) 'ssmis_spatial_average' 
!      write(*,*) 'min/max fov           = ', minval(fov), maxval(fov) 
!      write(*,*) 'min/max time          = ', minval(time), maxval(time) 
!      write(*,*) 'min/max scanline_back = ', minval(scanline_back), maxval(scanline_back) 
!      write(*,*) 'min/max scanline      = ', minval(scanline), maxval(scanline) 
!      write(*,*) 'min/max scan          = ', minval(scan), maxval(scan) 
!      write(*,*) 'min/max bt_inout        ', minval(bt_inout), maxval(bt_inout) 

!      Noise reduction using FFT

       DO IChan=1,nchanl
       
          bt_image1 => bt_image(:,:,ichan)

          ! If the channel number is present in the channelnumber array we should process it 
          ! (otherwise bt_inout just keeps the same value):
          IF (ANY(channelnumber(1:nchannels) == ichan)) THEN

             CALL MODIFY_BEAMWIDTH ( max_fov, max_scan, bt_image1, &
                  sampling_distx, sampling_disty, beamwidth(ichan), newwidth(ichan), &
                  cutoff(ichan), nxaverage(ichan), nyaverage(ichan), &
                  qc_distx(ichan), qc_disty(ichan), IOS)
          
             IF (IOS == 0) THEN
                do iscan=1,max_scan
                   do ifov=1,max_fov
                      if (Scanline_Back(IFov, IScan) > 0) &
                           bt_inout(channelnumber(ichan),Scanline_Back(IFov, IScan)) = &
                           BT_Image1(ifov,iscan)
                   end do
                end do
             ELSE
                Error_Status=1
                RETURN
             END IF
          END IF
       END DO
       DEALLOCATE(BT_Image, Scanline, Scanline_Back)
       NULLIFY(BT_Image1)
!      call cpu_time(time2)
!      write(*,*)'CPU time for noise reduction = ', time2-time1 
    endif ! method=3

!============================================================================================================

!   Simple method for GMI (like method 1)
    if (Method == 4) then  ! simple averaging 1
       gaussian_wgt = .false.
!       write(*,*) 'SSMIS_Spatial_Average for GMI: using method from Banghua'
       write(*,*) 'SSMIS_Spatial_Average for GMI: bufrsat = ', BufrSat
       write(*,*) 'SSMIS_Spatial_Average for GMI: Gaussian Weighted Averaging =',gaussian_wgt

       ! Determine scanline from time
       !==============================
       allocate(scanline(num_obs))
       t1          = time(1)  ! time for first scanline
       nscan       = 1        ! first scanline
       scanline(1) = nscan
       do iobs = 2, num_obs
          t2    = time(iobs)
          tdiff = t2-t1
          if (tdiff >= 0.00001_r_kind) then
             nscan = nscan+1
             t1    = t2
          endif
          scanline(iobs) = nscan
       enddo
       max_scan = maxval(scanline)
       write(*,*) 'SSMIS_Spatial_Average for GMI:max_scan,max_fov,nchanl = ', &
                 max_scan,max_fov,nchanl

!      Allocate and initialize variables
       allocate(bt_image_orig(max_fov_gmi,max_scan,nchanl))
       allocate(latitude(max_fov_gmi,max_scan))
       allocate(longitude(max_fov_gmi,max_scan))
       allocate(nodeinfo(max_fov_gmi,max_scan))
       allocate(scanline_back(max_fov_gmi,max_scan))
       bt_image_orig(:,:,:)= 1000.0_r_kind
       latitude(:,:)       = 1000.0_r_kind
       longitude(:,:)      = 1000.0_r_kind
       scanline_back(:,:)  = -1
       nodeinfo(:,:)       = 1000_i_kind

!      Put data into 2D (fov vs. scanline) array
       do iobs = 1, num_obs
          latitude(fov(iobs),scanline(iobs))       = lat(iobs)
          longitude(fov(iobs),scanline(iobs))      = lon(iobs)
          bt_image_orig(fov(iobs),scanline(iobs),:)= bt_inout(:,iobs)
          scanline_back(fov(iobs),scanline(iobs))  = iobs
       enddo

!      Determine AS/DS node information for each scanline
       gmi_loop1: do iscan = 1, max_scan-1
          gmi_loop2: do ifov = 1, max_fov_gmi
             if (scanline_back(ifov,iscan) > 0 .and. scanline_back(ifov,iscan+1)> 0) then
                dlat = latitude(ifov,iscan+1)-latitude(ifov,iscan)
                if (dlat < 0.0_r_kind) then
                   nodeinfo(:,iscan) = ds_node
                else
                   nodeinfo(:,iscan) = as_node
                endif
                cycle gmi_loop1
             endif
          enddo gmi_loop2
       enddo gmi_loop1
       nodeinfo(:,max_scan) = nodeinfo(:,max_scan-1)

!      Do spatial averaging in the box centered on each fov for each channel
       gmi_scan_loop: do iscan = 1, max_scan
          gmi_fov_loop: do ifov = 1, max_fov_gmi    
             iobs = scanline_back(ifov,iscan)
             if (iobs >0) then
                node_inout(iobs) = nodeinfo(ifov,iscan)
                gmi_channel_loop: do ic = 1, nchanl
!            Define grid box by channel -
!            Ch 1-2: 1 scan direction, 1 track direction
!            Ch 3-13: 3 scan direction, 3 track direction

                   ns1 = iscan-4
                   ns2 = iscan+4
                   if (ns1 < 1) ns1=1
                   if (ns2 > max_scan) ns2=max_scan
                   np1 = ifov-8
                   np2 = ifov+8
                   if (np1 < 1) np1=1
                   if (np2 > max_fov_gmi) np2=max_fov_gmi
                   xnum   = 0.0_r_kind
                   mta    = 0.0_r_kind
                   if (any(bt_image_orig(np1:np2,ns1:ns2,ic) < btmin .or. &
                           bt_image_orig(np1:np1,ns1:ns2,ic) > btmax)) then
                       bt_inout(ic,iobs) = 1000.0_r_kind
                   else
                ! Calculate distance of each fov to the center fov
                      gmi_box_y1: do is = ns1, ns2
                      gmi_box_x1: do ip = np1, np2
                         lat1 = latitude(ifov,iscan)    ! lat of the center fov
                         lon1 = longitude(ifov,iscan)   ! lon of the center fov
                         lat2 = latitude(ip,is)
                         lon2 = longitude(ip,is)
                         dist = distance(lat1,lon1,lat2,lon2)
                         if (dist > 20.0_r_kind) cycle gmi_box_x1  ! outside the box
                         if (gaussian_wgt) then
                            wgt = exp(-0.5_r_kind*(dist/sigma)*(dist/sigma))
                         else
                            wgt = 1.0
                         endif
                         xnum   = xnum+wgt
                         mta    = mta +wgt*bt_image_orig(ip,is,ic)
                      enddo gmi_box_x1
                      enddo gmi_box_y1
                      bt_inout(ic,iobs) = mta/xnum
                   endif
                enddo gmi_channel_loop
             endif
          enddo gmi_fov_loop
       enddo gmi_scan_loop

!      Deallocate arrays
       deallocate(nodeinfo,scanline,scanline_back)
       deallocate(latitude,longitude)
       deallocate(bt_image_orig)
    endif ! Method=4

!============================================================================================================

!   Simple method for AMSR2 (like method 1)
    if (Method == 5) then  ! simple averaging 1
       gaussian_wgt = .false.
!       write(*,*) 'SSMIS_Spatial_Average for AMSR2: using method from Banghua'
       write(*,*) 'SSMIS_Spatial_Average for AMSR2: bufrsat = ', BufrSat
       write(*,*) 'SSMIS_Spatial_Average for AMSR2: Gaussian Weighted Averaging=',gaussian_wgt

       ! Determine scanline from time
       !==============================
       allocate(scanline(num_obs))
       t1          = time(1)  ! time for first scanline
       nscan       = 1        ! first scanline
       scanline(1) = nscan
       do iobs = 2, num_obs
          t2    = time(iobs)
          tdiff = t2-t1
          if (tdiff >= 0.00001_r_kind) then
             nscan = nscan+1
             t1    = t2
          endif
          scanline(iobs) = nscan
       enddo
       max_scan = maxval(scanline)
       write(*,*) 'SSMIS_Spatial_Average for AMSR2:max_scan,max_fov,nchanl = ', &
                 max_scan,max_fov,nchanl

!      Allocate and initialize variables
       allocate(bt_image_orig(max_fov_amsr2,max_scan,nchanl))
       allocate(latitude(max_fov_amsr2,max_scan))
       allocate(longitude(max_fov_amsr2,max_scan))
       allocate(nodeinfo(max_fov_amsr2,max_scan))
       allocate(scanline_back(max_fov_amsr2,max_scan))
       bt_image_orig(:,:,:)= 1000.0_r_kind
       latitude(:,:)       = 1000.0_r_kind
       longitude(:,:)      = 1000.0_r_kind
       scanline_back(:,:)  = -1
       nodeinfo(:,:)       = 1000_i_kind

!      Put data into 2D (fov vs. scanline) array
       do iobs = 1, num_obs
          latitude(fov(iobs),scanline(iobs))       = lat(iobs)
          longitude(fov(iobs),scanline(iobs))      = lon(iobs)
          bt_image_orig(fov(iobs),scanline(iobs),:)= bt_inout(:,iobs)
          scanline_back(fov(iobs),scanline(iobs))  = iobs
       enddo

!      Determine AS/DS node information for each scanline
       amsr2_loop1: do iscan = 1, max_scan-1
          amsr2_loop2: do ifov = 1, max_fov_amsr2
             if (scanline_back(ifov,iscan) > 0 .and. scanline_back(ifov,iscan+1)> 0) then
                dlat = latitude(ifov,iscan+1)-latitude(ifov,iscan)
                if (dlat < 0.0_r_kind) then
                   nodeinfo(:,iscan) = ds_node
                else
                   nodeinfo(:,iscan) = as_node
                endif
                cycle amsr2_loop1
             endif
          enddo amsr2_loop2
       enddo amsr2_loop1
       nodeinfo(:,max_scan) = nodeinfo(:,max_scan-1)

!      Do spatial averaging in the box centered on each fov for each channel
       amsr2_scan_loop: do iscan = 1, max_scan
          amsr2_fov_loop: do ifov = 1, max_fov_amsr2
             iobs = scanline_back(ifov,iscan)
             if (iobs >0) then
                node_inout(iobs) = nodeinfo(ifov,iscan)
                amsr2_channel_loop: do ic = 1, nchanl
!            Define grid box by channel -
!            Ch 1-6: 1 scan direction, 1 track direction
!            Ch 7-14: 3 scan direction, 3 track direction
                   if ((ic >= 1) .and. (ic <= 6)) then
                      ns1 = iscan
                      ns2 = iscan
                      if (ns1 < 1) ns1=1
                      if (ns2 > max_scan) ns2=max_scan
                      np1 = ifov
                      np2 = ifov
                      if (np1 < 1) np1=1
                      if (np2 > max_fov_gmi) np2=max_fov_amsr2
                   else if ((ic >= 7) .and. (ic <= 14)) then
                      ns1 = iscan-1
                      ns2 = iscan+1
                      if (ns1 < 1) ns1=1
                      if (ns2 > max_scan) ns2=max_scan
                      np1 = ifov-1
                      np2 = ifov+1
                      if (np1 < 1) np1=1
                      if (np2 > max_fov_amsr2) np2=max_fov_amsr2
                   endif

                   xnum   = 0.0_r_kind
                   mta    = 0.0_r_kind
                   if (any(bt_image_orig(np1:np2,ns1:ns2,ic) < btmin .or. &
                           bt_image_orig(np1:np1,ns1:ns2,ic) > btmax)) then
                       bt_inout(ic,iobs) = 1000.0_r_kind
                   else
                ! Calculate distance of each fov to the center fov
                      amsr2_box_y1: do is = ns1, ns2
                      amsr2_box_x1: do ip = np1, np2
                         lat1 = latitude(ifov,iscan)    ! lat of the center fov
                         lon1 = longitude(ifov,iscan)   ! lon of the center fov
                         lat2 = latitude(ip,is)
                         lon2 = longitude(ip,is)
                         dist = distance(lat1,lon1,lat2,lon2)
                         if (dist > 50.0_r_kind) cycle amsr2_box_x1  ! outside the box
                         if (gaussian_wgt) then
                            wgt = exp(-0.5_r_kind*(dist/sigma)*(dist/sigma))
                         else
                            wgt = 1.0
                         endif
                         xnum   = xnum+wgt
                         mta    = mta +wgt*bt_image_orig(ip,is,ic)
                      enddo amsr2_box_x1
                      enddo amsr2_box_y1
                      bt_inout(ic,iobs) = mta/xnum
                   endif
                enddo amsr2_channel_loop
             endif
          enddo amsr2_fov_loop
       enddo amsr2_scan_loop

!      Deallocate arrays
       deallocate(nodeinfo,scanline,scanline_back)
       deallocate(latitude,longitude)
       deallocate(bt_image_orig)
    endif ! Method=5

END Subroutine SSMIS_Spatial_Average


SUBROUTINE MODIFY_BEAMWIDTH ( nx, ny, image, sampling_distx, sampling_disty, & 
     beamwidth, newwidth, mtfcutoff, nxaverage, nyaverage, qc_distx, qc_disty, &
     Error)
     
!-----------------------------------------
! Name: $Id$
!
! Purpose:
!   Manipulate the effective beam width of an image. For example, convert ATMS
!   to AMSU-A-like resolution while reducing the noise.
!
! Method:
!   1) Pad the image to a power of 2 in each dimension.
! If FFT technique is to be used then: 
!   2) Assuming Gaussian beam shapes, calcluate the input and output Modulation
!      Transfer Functions (MTF).
!   3) FFT image to frequency domain (2-D).
!   4) Multiply by output MTF divided by input MTF. If a cut-off is specified
!      (when attempting to make the beam width narrower), attenuate further
!      by an exponential function - factor of 2 at the cutoff. 
!   5) FFT back to image domain 
! Finally,
!   6) Over-write the input image, with averaging if requested.
!
! COPYRIGHT
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 1 December 2006, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2010, EUMETSAT, All Rights Reserved.
!
! History:
! Version    Date     Comment
!
!  1.0   22/07/2010   N.C.Atkinson
!  1.1   21/11/2011   Convert to f90. A. Collard
!
! Code Description:
!   FORTRAN 77, following AAPP standards
!
! Declarations:


      IMPLICIT NONE
! Parameters
!     INTEGER(I_KIND), PARAMETER :: nxmax=128   !Max number of spots per scan line
!     INTEGER(I_KIND), PARAMETER :: nymax=8192  !Max number of lines. Allows 6hrs of ATMS.
      INTEGER(I_KIND), PARAMETER :: nxmax=64    !Max number of spots per scan line
      INTEGER(I_KIND), PARAMETER :: nymax=16384 !Max number of lines. Allows 6hrs of ATMS.
      REAL(R_KIND) minval, maxval
      PARAMETER (minval=0.0)   !Values less than this are treated as missing
      PARAMETER (maxval=400.0) !Values greater than this are treated as missing

! Arguments
      INTEGER(I_KIND), INTENT(IN)  :: nx, ny         !Size of image
      REAL(R_KIND), INTENT(INOUT)  :: image(nx,ny)   !BT or radiance image
      REAL(R_KIND), INTENT(IN)     :: sampling_distx !typically degrees
      REAL(R_KIND), INTENT(IN)     :: sampling_disty !typically degrees
      REAL(R_KIND), INTENT(IN)     :: beamwidth      !ditto
      REAL(R_KIND), INTENT(IN)     :: newwidth       !ditto
      REAL(R_KIND), INTENT(IN)     :: mtfcutoff      !0.0 to 1.0
      INTEGER(I_KIND), INTENT(IN)  :: nxaverage      !Number of samples to average (or zero)
      INTEGER(I_KIND), INTENT(IN)  :: nyaverage      !Number of samples to average (or zero)
      INTEGER(I_KIND), INTENT(IN)  :: qc_distx       !Number of samples around missing data to set to 
      INTEGER(I_KIND), INTENT(IN)  :: qc_disty       !Number of samples around missing data to set to 
      INTEGER(I_KIND), INTENT(OUT) :: Error          !Error Status
       
! Local variables
      INTEGER(I_KIND) :: nxpad, nypad, dx, dy
      INTEGER(I_KIND) :: i,j,k,ix,iy, ii, jj
      INTEGER(I_KIND) :: ifirst
      INTEGER(I_KIND) :: xpow2, ypow2
      INTEGER(I_KIND) :: nxav2, nyav2, naverage
!     INTEGER(I_KIND) :: deltax
      INTEGER(I_KIND) :: minii, maxii, minjj, maxjj
      REAL(R_KIND), ALLOCATABLE :: mtfxin(:),mtfxout(:)
      REAL(R_KIND), ALLOCATABLE :: mtfyin(:),mtfyout(:)
      REAL(R_KIND) :: mtfin,mtfout
      REAL(R_KIND) :: mtfx_constant, mtfy_constant   !test
      REAL(R_KIND), ALLOCATABLE :: mtfpad(:,:)
      REAL(R_KIND), ALLOCATABLE :: imagepad(:,:)
      REAL(R_KIND), ALLOCATABLE :: work(:)
      REAL(R_KIND) :: f,df,factor
      REAL(R_KIND) :: PI, LN2, LNcsquared
      LOGICAL :: missing
      LOGICAL, ALLOCATABLE :: gooddata_map(:,:)


! End of declarations
!-----------------------------------------
      
      PI = 4.0*atan(1.0)
      LN2 = LOG(2.0)
!     MTF_Constant=-(PI/(2*sampling_dist))**2/LN2
      MTFX_Constant=-(PI/(2*sampling_distx))**2/LN2   !test
      MTFY_Constant=-(PI/(2*sampling_disty))**2/LN2   !test 
      IF (mtfcutoff .GT. 0.0) LNcsquared = LOG(mtfcutoff)**2
      nxav2 = nxaverage/2
      nyav2 = nyaverage/2
      naverage = nxaverage*nyaverage
      Error = 0

!1) Pad the image up to the nearest power of 2 in each dimension, by reversing
!the points near the edge.

      xpow2 = INT(LOG(nx*1.0)/LN2 + 1.0)
      ypow2 = INT(LOG(ny*1.0)/LN2 + 1.0)
      nxpad = 2**xpow2
      nypad = 2**ypow2
      dx = (nxpad - nx)/2
      dy = (nypad - ny)/2

      IF (nxpad .GT. nxmax) THEN
         write(*,*) 'SSMIS_Spatial_Average: nx too large, maximum allowed value is ',nxmax-1
         Error = 1
         RETURN
      END IF
      
      IF (nypad .GT. nymax) THEN
         write(*,*) 'SSMIS_Spatial_Average: ny value is ', nypad   !test 
         write(*,*) 'SSMIS_Spatial_Average: ny too large, maximum allowed value is ',nymax-1
         Error = 1
         RETURN
      END IF

      ALLOCATE(mtfxin(nxpad),mtfxout(nxpad))
      ALLOCATE(mtfyin(nypad),mtfyout(nypad))
      ALLOCATE(mtfpad(nxpad,nypad))
      ALLOCATE(imagepad(nxpad,nypad))
      ALLOCATE(work(nypad))
      ALLOCATE(gooddata_map(nxmax,nymax))

!Loop over scan positions
      DO j=dy+1,dy+ny
        DO i=dx+1,dx+nx
          if (image(i-dx,j-dy) > maxval .OR. image(i-dx,j-dy) < minval) &
               image(i-dx,j-dy) = minval - 1.0_r_kind
          imagepad(i,j) = image(i-dx,j-dy)   !Take a copy of the input data
          gooddata_map(i,j) = .TRUE.   ! Initialised for step 6)
!111      format(4i12,2f20.5,l6)
        ENDDO

!Interpolate missing points in the along-track direction

        ifirst = -1
        missing = .false.
        
        DO i=dx+1,dx+nx
          IF (.not.missing) THEN
            IF (imagepad(i,j) .GE. minval) THEN   !imagepad = -1 indicates missing data 
              ifirst = i
            ELSE
              missing = .true.
            ENDIF
          ELSE
            IF (imagepad(i,j) .GE. minval) THEN  !First good point after missing
               missing = .false.
               IF (ifirst .eq. -1) THEN
                  DO k=dx+1,i-1
                     imagepad(k,j) = imagepad(i,j)      !Constant
                  ENDDO
               ELSE
                  DO k=ifirst+1,i-1
                     factor = (i-k)*1.0/(i-ifirst)      !Interpolate
                     imagepad(k,j) = imagepad(ifirst,j)*factor + &
                          imagepad(i,j)*(1.0-factor)
                  ENDDO
               ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF (missing) THEN         !Last scan is missing
          IF (ifirst .GE. 1) then
            DO k=ifirst+1,dx+nx
              imagepad(k,j) = imagepad(ifirst,j)     !Constant
            ENDDO
          ENDIF
        ENDIF          

!Continue padding the edges

        DO i=1,dx
          imagepad(i,j) = imagepad(dx+dx+2-i,j)
        ENDDO
        DO i=nx+dx+1,nxpad
          imagepad(i,j) = imagepad(nx+dx+nx+dx-i,j)
        ENDDO
     ENDDO

     DO j=1,dy
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,dy+dy+2-j)
        ENDDO
     ENDDO
     DO j=ny+dy+1,nypad
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,ny+dy+ny+dy-j)
        ENDDO
     ENDDO

!2) Compute the MTF modifications. Assume beams are Gaussian.

      IF (newwidth .GT. 0) THEN
        df = 1.0/nxpad
        DO i=1,nxpad/2+1
          f = df*(i-1)      !DC to Nyquist
!         mtfxin(i) = exp(MTF_Constant*(f*beamwidth)**2)
!         mtfxout(i) = exp(MTF_Constant*(f*newwidth)**2)
          mtfxin(i) = exp(MTFX_Constant*(f*beamwidth)**2)    !test 
          mtfxout(i) = exp(MTFX_Constant*(f*newwidth)**2)    !test
          IF (i.GT.1.AND.i.LT.nxpad/2+1) THEN
            mtfxin(nxpad-i+2) = mtfxin(i)
            mtfxout(nxpad-i+2) = mtfxout(i)
          ENDIF
        ENDDO
        df = 1.0/nypad
        DO i=1,nypad/2+1
          f = df*(i-1)      !DC to Nyquist
!         mtfyin(i) = exp(MTF_Constant*(f*beamwidth)**2)
!         mtfyout(i) = exp(MTF_Constant*(f*newwidth)**2)
          mtfyin(i) = exp(MTFY_Constant*(f*beamwidth)**2)  !test
          mtfyout(i) = exp(MTFY_Constant*(f*newwidth)**2)  !test
          IF (i.GT.1.AND.i.LT.nypad/2+1) THEN
            mtfyin(nypad-i+2) = mtfyin(i)
            mtfyout(nypad-i+2) = mtfyout(i)
          ENDIF
        ENDDO
        DO i=1,nxpad
          DO j=1,nypad
            mtfin = mtfxin(i)*mtfyin(j)
            mtfout = mtfxout(i)*mtfyout(j)
            if (mtfcutoff .GT. 0.0) THEN
              mtfpad(i,j) = (mtfout * &
                exp(-LN2/LNcsquared*(LOG(mtfout))**2))/mtfin
            else
              mtfpad(i,j) = mtfout/mtfin
            endif
          ENDDO
        ENDDO

!3) Fourier transform, line by line then column by column.
!After each FFT, points 1 to nxpad/2+1 contain the real part of the spectrum,
!the rest contain the imaginary part in reverse order.

        DO j=1,nypad
           CALL SFFTCF(imagepad(:,j),nxpad,xpow2)
        ENDDO

        DO i=1,nxpad
           DO j=1,nypad
              work(j) = imagepad(i,j)
           ENDDO
           CALL SFFTCF(work,nypad,ypow2)
           DO j=1,nypad
              imagepad(i,j) = work(j)
           ENDDO
        ENDDO

!4) Multiply the spectrum by the MTF factor

        DO j=1,nypad
           DO i=1,nxpad
            imagepad(i,j) = imagepad(i,j)*mtfpad(i,j)
          ENDDO
        ENDDO

!5) Inverse Fourier transform, column by column then line by line 

        DO i=1,nxpad
          DO j=1,nypad
            work(j) = imagepad(i,j)
          ENDDO
          CALL SFFTCB(work,nypad,ypow2)
          DO j=1,nypad
            imagepad(i,j) = work(j)
          ENDDO
        ENDDO

        DO j=1,nypad
          CALL SFFTCB(imagepad(:,j),nxpad,xpow2)
        ENDDO
     ENDIF   !New width is specified

!6) Reset missing values in gooddata_map, based on qc_dist and the values 
!   in the input image array

     ! Set the ends of the image to missing in the along track direction
     ! (doing the same across track will remove too much data)
!    gooddata_map(:,1:qc_dist)=.FALSE.
!    gooddata_map(:,ny-qc_dist+1:ny)=.FALSE.
     gooddata_map(:,1:qc_disty)=.FALSE.
     gooddata_map(:,ny-qc_disty+1:ny)=.FALSE.
     
     DO j=1,ny
        DO i=1,nx
           IF (image(i,j) <= minval) THEN
!             minjj=max(j+dy-qc_dist,0)
!             maxjj=min(j+dy+qc_dist,nymax)
              minjj=max(j+dy-qc_disty,0)
              maxjj=min(j+dy+qc_disty,nymax)
              DO jj=minjj,maxjj
!                deltax=INT(SQRT(REAL(qc_dist**2 - (jj-j-dy)**2 )))
!                minii=max(i+dx-deltax,0)
!                maxii=min(i+dx+deltax,nxmax)
                 minii=max(i+dx-qc_distx,0)
                 maxii=min(i+dx+qc_distx,nxmax)
                 DO ii=minii,maxii
                    gooddata_map(ii,jj)=.FALSE.
!345                 format(10i10)    
                 END DO
              END DO
           END IF
        END DO
     END DO

!7) Over-write the input image (points that are not missing)

     DO j=1,ny
        DO i=1,nx
           IF (gooddata_map(i+dx,j+dy)) THEN
              IF (nxav2 == 0. .AND. nyav2 == 0) THEN
                 image(i,j) = imagepad(i+dx,j+dy)
              ELSE
                 image(i,j) = 0.0             !Do averaging
                 DO ix = -nxav2,nxav2
                    DO iy = -nyav2,nyav2
                       image(i,j) = image(i,j) + imagepad(i+dx+ix,j+dy+iy)
                    ENDDO
                 ENDDO
                 image(i,j) = image(i,j)/naverage
              ENDIF
           ELSE
              image(i,j) = missing_value
           END IF
        ENDDO
     ENDDO

!8) Deallocate arrays

     DEALLOCATE(mtfxin,mtfxout)
     DEALLOCATE(mtfyin,mtfyout)
     DEALLOCATE(mtfpad)
     DEALLOCATE(imagepad)
     DEALLOCATE(work)
     DEALLOCATE(gooddata_map)

     RETURN
   END SUBROUTINE MODIFY_BEAMWIDTH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix FFT program
!  Real input and output in data array X
!  Length is N = 2 ** M
!  Decimation-in-time, cos/sin in second loop
!  Output in order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     X(k) = sum_{j=0}^{N-1} x(j)*exp(-2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Oct. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named RVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCF( X, N, M )

      IMPLICIT NONE

! ... Parameters ...
      REAL(R_KIND), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(R_KIND), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(I_KIND), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(R_KIND), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(I_KIND)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(I_KIND)  N1, N2, N4, N8
      REAL(R_KIND)  XT, R1, T1, T2, T3, T4, T5, T6
      REAL(R_KIND)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N .EQ. 1 ) RETURN
!
      J = 1
      N1 = N - 1
      DO 104 I = 1, N1
         IF ( I < J ) THEN
            XT = X(J)
            X(J) = X(I)
            X(I) = XT
         END IF
         K = N / 2
         DO WHILE ( K < J )           
            J = J - K
            K = K / 2
         END DO
         J = J + K
 104  CONTINUE
! 
      IS = 1
      ID = 4
      DO
         DO 60 I0 = IS, N, ID
            I1 = I0 + 1
            R1 = X(I0)
            X(I0) = R1 + X(I1)
            X(I1) = R1 - X(I1)
 60      CONTINUE
         IS = 2 * ID - 1
         ID = 4 * ID
         IF ( IS >= N ) EXIT
      END DO
!
      N2 = 2
      DO 10 K = 2, M
         N2 = N2 * 2
         N4 = N2 / 4
         N8 = N2 / 8
         E = TWOPI / N2
         IS = 0
         ID = N2 * 2
         DO
            DO 38 I = IS, N-1, ID
               I1 = I + 1
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               T1 = X(I4) + X(I3)
               X(I4) = X(I4) - X(I3)
               X(I3) = X(I1) - T1
               X(I1) = X(I1) + T1
               IF ( N4 == 1 ) CYCLE
               I1 = I1 + N8
               I2 = I2 + N8
               I3 = I3 + N8
               I4 = I4 + N8
               T1 = ( X(I3) + X(I4) ) / SQRT2
               T2 = ( X(I3) - X(I4) ) / SQRT2
               X(I4) = X(I2) - T1
               X(I3) = - X(I2) - T1
               X(I2) = X(I1) - T2
               X(I1) = X(I1) + T2
 38         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS >= N ) EXIT
         END DO
         A = E
         DO 32 J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
            DO 
               DO 30 I = IS, N-1, ID
                  I1 = I + J
                  I2 = I1 + N4
                  I3 = I2 + N4
                  I4 = I3 + N4
                  I5 = I + N4 - J + 2
                  I6 = I5 + N4
                  I7 = I6 + N4
                  I8 = I7 + N4
                  T1 = X(I3) * CC1 + X(I7) * SS1
                  T2 = X(I7) * CC1 - X(I3) * SS1
                  T3 = X(I4) * CC3 + X(I8) * SS3
                  T4 = X(I8) * CC3 - X(I4) * SS3
                  T5 = T1 + T3
                  T6 = T2 + T4
                  T3 = T1 - T3
                  T4 = T2 - T4
                  T2 = X(I6) + T6
                  X(I3) = T6 - X(I6)
                  X(I8) = T2
                  T2 = X(I2) - T3
                  X(I7) = - X(I2) - T3
                  X(I4) = T2
                  T1 = X(I1) + T5
                  X(I6) = X(I1) - T5
                  X(I1) = T1
                  T1 = X(I5) + T4
                  X(I5) = X(I5) - T4
                  X(I2) = T1
 30            CONTINUE
               IS = 2 * ID - N2
               ID = 4 * ID
               IF ( IS >= N ) EXIT
            END DO
 32      CONTINUE
 10   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCF ...
!
   END SUBROUTINE SFFTCF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix IFFT program
!  Hermitian symmetric input and real output in array X
!  Length is N = 2 ** M
!  Decimation-in-frequency, cos/sin in second loop
!  Input order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     x(j) = (1/N) * sum_{k=0}^{N-1} X(k)*exp(2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Nov. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named IRVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCB( X, N, M )

      use kinds, only: r_kind,r_double,i_kind

      IMPLICIT NONE

! ... Parameters ...
      REAL(R_KIND), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(R_KIND), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(I_KIND), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(R_KIND), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(I_KIND)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(I_KIND)  N1, N2, N4, N8
      REAL(R_KIND)  XT, R1, T1, T2, T3, T4, T5
      REAL(R_KIND)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N .EQ. 1 ) RETURN
!
      N2 = 2 * N
      DO 10 K = 1, M-1
         IS = 0
         ID = N2
         N2 = N2 / 2
         N4 = N2 / 4
         N8 = N4 / 2
         E = TWOPI / N2
         DO 
 17         DO 15 I = IS, N-1, ID
               I1 = I + 1
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               T1 = X(I1) - X(I3)
               X(I1) = X(I1) + X(I3)
               X(I2) = 2 * X(I2)
               X(I3) = T1 - 2 * X(I4)
               X(I4) = T1 + 2 * X(I4)
               IF ( N4 .EQ. 1 ) CYCLE
               I1 = I1 + N8
               I2 = I2 + N8
               I3 = I3 + N8
               I4 = I4 + N8
               T1 = ( X(I2) - X(I1) ) / SQRT2
               T2 = ( X(I4) + X(I3) ) / SQRT2
               X(I1) = X(I1) + X(I2)
               X(I2) = X(I4) - X(I3)
               X(I3) = 2 * ( - T2 - T1 )
               X(I4) = 2 * ( -T2 + T1 )
 15         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS >= N-1 ) EXIT
         END DO
         A = E
         DO 20 J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
            DO 
               DO 30 I = IS, N-1, ID
                  I1 = I + J
                  I2 = I1 + N4
                  I3 = I2 + N4
                  I4 = I3 + N4
                  I5 = I + N4 - J + 2
                  I6 = I5 + N4
                  I7 = I6 + N4
                  I8 = I7 + N4
                  T1 = X(I1) - X(I6)
                  X(I1) = X(I1) + X(I6)
                  T2 = X(I5) - X(I2)
                  X(I5) = X(I2) + X(I5)
                  T3 = X(I8) + X(I3)
                  X(I6) = X(I8) - X(I3)
                  T4 = X(I4) + X(I7)
                  X(I2) = X(I4) - X(I7)
                  T5 = T1 - T4
                  T1 = T1 + T4
                  T4 = T2 - T3
                  T2 = T2 + T3
                  X(I3) = T5 * CC1 + T4 * SS1
                  X(I7) = - T4 * CC1 + T5 * SS1
                  X(I4) = T1 * CC3 - T2 * SS3
                  X(I8) = T2 * CC3 + T1 * SS3
 30           CONTINUE
              IS = 2 * ID - N2
              ID = 4 * ID
              IF ( IS >= N-1 ) EXIT
           END DO
 20      CONTINUE
 10   CONTINUE
!
      IS = 1
      ID = 4
      DO 
         DO 60 I0 = IS, N, ID
            I1 = I0 + 1
            R1 = X(I0)
            X(I0) = R1 + X(I1)
            X(I1) = R1 - X(I1)
 60      CONTINUE
         IS = 2 * ID - 1
         ID = 4 * ID
         IF ( IS >= N ) EXIT
      END DO
!
      J = 1
      N1 = N - 1
      DO 104 I = 1, N1
         IF ( I < J ) THEN
            XT = X(J)
            X(J) = X(I)
            X(I) = XT
         END IF
         K = N / 2
         DO WHILE ( K < J )         
            J = J - K
            K = K / 2
         END DO
         J = J + K
 104  CONTINUE
      XT = 1.0 / real( N,r_kind )
      DO 99 I = 1, N
         X(I) = XT * X(I)
 99   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCB ...
! 
      END SUBROUTINE SFFTCB

END MODULE SSMIS_Spatial_Average_Mod
