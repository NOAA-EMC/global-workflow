;------------------------------------------------------------------
; Denise.Worthen@noaa.gov (Feb 2019)
;
; This script will remap CICE5 output on the tripole grid to 
; a set of rectilinear grids using pre-computed ESMF weights to remap 
; the listed fields to the destination grid and write the results 
; to a new netCDF file
; 
; See ocnpost.ncl for a complete description
;
; Bin.Li@noaa.gov (May 2019)
; This script is revised to be used in the coupled workflow.
; Revised parts are marked by <BL. and BL.>

  load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

;----------------------------------------------------------------------
begin

;************************************************
; specify parameters
;************************************************
; <BL.
; pull from environment
  COMDIR          = getenv("COMOUTice")
  IDATE           = getenv("IDATE")
  FHR             = getenv("FHR")
  ENSMEM          = getenv("ENSMEM")
  DATA_TMP        = getenv("DATA")
  nemsrc          = getenv("FIXreg2grb2")
;   nemsrc     = "/scratch2/NCEPDEV/climate/Bin.Li/S2S/fix/ocean_ice_post/FIXDIR/"

; calculate verification date
  VDATE = tochar(systemfunc("$NDATE "+FHR+" "+IDATE))
; BL.>

   output_masks = False
 ; destination grid sizes and name
  dsttype     = (/"rect."/)
 ;dstgrds     = (/"1p0", "0p5", "0p25"/)
; <BL.
  dstgrds     = (/"0p25"/)
; BL.>

 ; specify a location to use
 ;  nemsrc     = "/scratch4/NCEPDEV/ocean/save/Denise.Worthen/NEMS_INPUT0.1/ocnicepost/"
 ; interpolation methods
   methods        = (/"bilinear" ,"conserve"/)
 ; ocean model output location 
 ;dirsrc = "/scratch3/NCEPDEV/stmp2/Denise.Worthen/BM1_ice/"


   ; variables to be regridded with the native tripole stagger location

   varlist = (/ (/     "hi_h", "Ct", "bilinear"/) \
               ,(/     "hs_h", "Ct", "bilinear"/) \
               ,(/   "Tsfc_h", "Ct", "bilinear"/) \
               ,(/   "aice_h", "Ct", "bilinear"/) \
               ,(/    "sst_h", "Ct", "bilinear"/) \
             /)
     dims = dimsizes(varlist)
    nvars = dims(0)
   delete(dims)
   ;print(varlist)

    ; vectors to be regridded with the native tripole stagger location
    ; and dimensionality 
    ; note: vectors are always unstaggered using bilinear weights, but can
    ; be remapped using conservative
   nvpairs = 1
   veclist = new( (/nvpairs,3,2/),"string")
   veclist = (/ (/ (/"uvel_h", "vvel_h"/), (/"Bu", "Bu"/), (/"bilinear", "bilinear"/) /) \
             /)
   ;print(veclist)

   begTime = get_cpu_time()
;----------------------------------------------------------------------
; make a list of the directories and files from the run 
;----------------------------------------------------------------------
;   idate = "20120101"
;   icefilelist = systemfunc("ls "+dirsrc+"gfs."+idate+"/00/"+"ice*.nc")
;          icef = addfiles(icefilelist,"r")
;   nfiles = dimsizes(icefilelist)
; <BL.
; specify input file name and input directory
   icefilelist = "ice"+VDATE+"."+ENSMEM+"."+IDATE+".nc"
   icef = addfiles(COMDIR+"/"+icefilelist,"r")
; BL.>

  ; get the rotation angle 
    angleT = icef[0]->ANGLET

  ; get a 2 dimensional fields for creating the interpolation mask
  ; the mask2d contain 1's on land and 0's at valid points.
  mask2d = where(ismissing(icef[0]->sst_h),  1.0, 0.0)
  ;printVarSummary(mask2d)

  ; create conformed rotation arrays to make vector rotations cleaner
  angleT2d=conform_dims(dimsizes(mask2d),angleT,(/1,2/)) 

;----------------------------------------------------------------------
; loop over the output resolutions 
;----------------------------------------------------------------------

      jj = 1
      ii = 0

   do jj = 0,dimsizes(dstgrds)-1
    ;outres = "_"+dstgrds(jj)+"x"+dstgrds(jj)
    outres = dstgrds(jj)+"x"+dstgrds(jj)
   outgrid = dstgrds(jj)

   ; regrid a field to obtain the output xy dimensions
    wgtsfile = nemsrc+"/"+"tripole.mx025.Ct.to."+dsttype+dstgrds(jj)+".bilinear.nc"
          tt = ESMF_regrid_with_weights(angleT,wgtsfile,False)
        tt!0 = "lat"
        tt!1 = "lon"
         lat = tt&lat
         lon = tt&lon
        dims = dimsizes(tt)
        nlat = dims(0)
        nlon = dims(1)
     print("fields will be remapped to destination grid size "\
           +nlon+"  "+nlat)
  
     delete(tt)
     delete(dims)

    ; regrid the masks to obtain the interpolation masks. 
    ; the mask2d contain 1's on land and 0's at valid points.
    ; when remapped, any mask value > 0 identifies land values that 
    ; have crept into the field. remapped model fields are then
    ; masked with this interpolation mask

    wgtsfile = nemsrc+"/"+"tripole.mx025.Ct.to."+dsttype+dstgrds(jj)+".bilinear.nc"
    rgmask2d = ESMF_regrid_with_weights(mask2d, wgtsfile,False)

    if(output_masks)then
     testfile = "masks_"+dstgrds(jj)+".nc"
     system("/bin/rm -f "+testfile)
     ; create
     testcdf = addfile(testfile,"c")
     testcdf->rgmask2d = rgmask2d
     ; close
     delete(testcdf)
    end if

    ; create the interpolation mask
    rgmask2d = where(rgmask2d .gt. 0.0, rgmask2d@_FillValue, 1.0)

;----------------------------------------------------------------------
; loop over each file in the icefilelist
;----------------------------------------------------------------------
; <BL. removing the file loop 
;   do ii = 0,nfiles-1
;     infile = icefilelist(ii)
;     print("working on "+infile)
;    ; create the output file name
;    outfile = infile
    ; find the index where the actual filename begins
;     indstr = str_index_of_substr(outfile, "/", -1)
    ; insert a sub-directory name
;    outfile = str_insert(outfile,outgrid+"/",indstr+1)
    ; insert a string indicating regridding resolution
;    outfile = str_insert(outfile,outres,-4)
    ; find the index where the actual filename begins
;     indstr = str_index_of_substr(outfile, "ice", -1)
    ; insert an 'r' after the leading occurence of 'ice' 
    ; to indicate regular grid
;    outfile = str_insert(outfile,"r",indstr+3)
    ; check if the outgrid directory exists and create
    ; it if it does not
;    ret = systemfunc("test -d "+dirsrc+"gfs."+idate+"/00/"+outgrid+"; echo $?")
;    if(ret .eq. 1)then
;     system("mkdir "+dirsrc+"gfs."+idate+"/00/"+outgrid)
;    end if
; BL.>
    ; retrieve the time stamp
       time = icef[0]->time
    delete(time@bounds)

;----------------------------------------------------------------------
; set up the output netcdf file
;----------------------------------------------------------------------
;    system("/bin/rm -f " + outfile)    ; remove if exists
;    outcdf  = addfile (outfile, "c")  ; open output file
;
; <BL.
; specify output file information and open file for output
  FILENAME_REGRID = DATA_TMP+"/icer"+VDATE+"."+ENSMEM+"."+IDATE+"_"+outres+"_CICE.nc"
  if (isfilepresent(FILENAME_REGRID)) then
    system("rm -f "+FILENAME_REGRID)
  end if
  outcdf = addfile(FILENAME_REGRID,"c")
  infile = icefilelist
; BL.>

    ; explicitly declare file definition mode. Improve efficiency.
    setfileoption(outcdf,"DefineMode",True)

    ; create global attributes of the file
    fAtt               = True            ; assign file attributes
    fAtt@creation_date = systemfunc ("date")
    fAtt@source_file   = infile        
    fileattdef( outcdf, fAtt )           ; copy file attributes    

    ; predefine the coordinate variables and their dimensionality
    dimNames = (/"time", "lat", "lon"/)  
    dimSizes = (/ -1   ,  nlat,  nlon/) 
    dimUnlim = (/ True , False, False/)   
    filedimdef(outcdf,dimNames,dimSizes,dimUnlim)

    ; predefine the the dimensionality of the variables to be written out
    filevardef(outcdf, "time", typeof(time), getvardims(time)) 
    filevardef(outcdf,  "lat",  typeof(lat),  getvardims(lat))                          
    filevardef(outcdf,  "lon",  typeof(lon),  getvardims(lon))                          

    ; Copy attributes associated with each variable to the file
    filevarattdef(outcdf, "time", time)                 
    filevarattdef(outcdf,  "lat",  lat)             
    filevarattdef(outcdf,  "lon",  lon)            

    ; predefine variables
    do nv = 0,nvars-1
     varname = varlist(nv,0)
       odims = (/"time", "lat", "lon"/)
      ;print("creating variable "+varname+" in file")
      filevardef(outcdf, varname, "float", odims)
      delete(odims)
    end do

    do nv = 0,nvpairs-1
     do nn = 0,1
     vecname = veclist(nv,0,nn)
      odims = (/"time", "lat", "lon"/)
     ;print("creating variable "+vecname+" in file")
     filevardef(outcdf, vecname, "float", odims) 
     delete(odims)
     end do
    end do
 
    ; explicitly exit file definition mode.
    setfileoption(outcdf,"DefineMode",False)

    lat=lat(::-1)
    ; write the dimensions to the file
    outcdf->time   = (/time/)     
    outcdf->lat    = (/lat/)
    outcdf->lon    = (/lon/) 

;----------------------------------------------------------------------
; loop over nvars variables
;----------------------------------------------------------------------

    ;nv = 1
    do nv = 0,nvars-1
     varname = varlist(nv,0)
     vargrid = varlist(nv,1)
     varmeth = varlist(nv,2)
  
     ;print(nv+"   "+varname+"  "+vargrid+"  "+varmeth)
     icevar = icef[ii]->$varname$
     ndims = dimsizes(dimsizes(icevar))
     ;print(ndims+"   "+dimsizes(icevar))

     if(vargrid .ne. "Ct")then
      ; print error if the variable is not on the Ct grid
      print("Variable is not on Ct grid")
      exit
     end if

     ; regrid to dsttype+dstgrd with method
     ;print("remapping "+varname+" to grid "+dsttype+dstgrds(jj))
     wgtsfile = nemsrc+"/"+"tripole.mx025.Ct.to."+dsttype+dstgrds(jj)+"."+varmeth+".nc"

     rgtt = ESMF_regrid_with_weights(icevar,wgtsfile,False)
     rgtt = where(ismissing(rgmask2d),icevar@_FillValue,rgtt)
     rgtt=rgtt(:,::-1,:)

     ; enter file definition mode to add variable attributes
     setfileoption(outcdf,"DefineMode",True)
     filevarattdef(outcdf, varname, rgtt)                 
     setfileoption(outcdf,"DefineMode",False)

     
     outcdf->$varname$   = (/rgtt/)

     delete(icevar)
     delete(rgtt)
 
    ; nv, loop over number of variables
    end do

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
   
   ;nv = 0
   do nv = 0,nvpairs-1
     vecnames = veclist(nv,0,:)
     vecgrids = veclist(nv,1,:)
     vecmeth  = veclist(nv,2,:)
     ;print(nv+"   "+vecnames+"  "+vecgrids+"  "+vecmeth)

     ; create a vector pair list
     vecpairs = NewList("fifo")
            n = 0
         uvel = icef[ii]->$vecnames(n)$
       vecfld = where(ismissing(uvel),0.0,uvel)
        copy_VarAtts(uvel,vecfld)
     ;print("unstagger "+vecnames(n)+" from "+vecgrids(n)+" to Ct")
     wgtsfile = nemsrc+"/"+"tripole.mx025."+vecgrids(n)+".to.Ct.bilinear.nc"
           ut = ESMF_regrid_with_weights(vecfld,wgtsfile,False)
     delete(ut@remap)

            n = 1
         vvel = icef[ii]->$vecnames(n)$
       vecfld = where(ismissing(vvel),0.0,vvel)
        copy_VarAtts(vvel,vecfld)
     ;print("unstagger "+vecnames(n)+" from "+vecgrids(n)+" to Ct")
     wgtsfile = nemsrc+"/"+"tripole.mx025."+vecgrids(n)+".to.Ct.bilinear.nc"
           vt = ESMF_regrid_with_weights(vecfld,wgtsfile,False)
     delete(vt@remap)

     ListAppend(vecpairs,ut)
     ListAppend(vecpairs,vt)
     ;print(vecpairs)

     ; rotate
     ; first copy Metadata
     urot = vecpairs[0]
     vrot = vecpairs[1]
     urot = cos(angleT2d)*ut - sin(angleT2d)*vt
     vrot = sin(angleT2d)*ut + cos(angleT2d)*vt

     ; change attribute to indicate these are now rotated velocities
     urot@long_name=str_sub_str(urot@long_name,"(x)","zonal")
     vrot@long_name=str_sub_str(vrot@long_name,"(y)","meridional")
     ; copy back
     vecpairs[0] = urot
     vecpairs[1] = vrot
     delete([/urot, vrot/])

     ; remap
     do n = 0,1
      vecfld = vecpairs[n]
      ; regrid to dsttype+dstgrd with method
      ;print("remapping "+vecnames(n)+" to grid "+dsttype+dstgrds(jj))
      wgtsfile = nemsrc+"/"+"tripole.mx025.Ct.to."+dsttype+dstgrds(jj)+"."+vecmeth(n)+".nc"

       rgtt = ESMF_regrid_with_weights(vecfld,wgtsfile,False)
       rgtt = where(ismissing(rgmask2d),vecfld@_FillValue,rgtt)
       rgtt=rgtt(:,::-1,:)

      ; enter file definition mode to add variable attributes
      setfileoption(outcdf,"DefineMode",True)
      filevarattdef(outcdf, vecnames(n), rgtt)                 
      setfileoption(outcdf,"DefineMode",False)
    
      outcdf->$vecnames(n)$   = (/rgtt/)
      delete(rgtt)
     end do
      delete([/uvel,vvel,ut,vt,vecfld,vecpairs/])
      delete([/vecnames,vecgrids,vecmeth/])
    ; nv, loop over number of vector pairs
    end do
  
;----------------------------------------------------------------------
; close the outcdf and continue through filelist
;----------------------------------------------------------------------
   
   delete(outcdf)

   ; ii, loop over files
   ;end do
   ;jj, loop over destination grids
   delete([/lat,lon,nlon,nlat/])
   delete([/rgmask2d/])
  end do
  print("One complete ice file in " + (get_cpu_time() - begTime) + " seconds")
exit
end
