setenv datapath /gpfs/hps/ptmp/Jeffrey.S.Whitaker/C384_C384_test
setenv analdate 2016010300
setenv datapath2 "${datapath}/${analdate}"
set pathout=${datapath2}/ensmean/INPUT
mkdir -p $pathout
module load nco-gnu-sandybridge
echo "computing ensemble mean restart files `date`"
foreach tile (tile1 tile2 tile3 tile4 tile5 tile6)
   foreach filename (fv_core.res.${tile}.nc fv_tracer.res.${tile}.nc fv_srf_wnd.res.${tile}.nc sfc_data.${tile}.nc)
      echo "computing ens mean for $filename"
      nces -O `ls -1 ${datapath2}/mem*/INPUT/${filename}` ${pathout}/${filename}
   end
end
/bin/cp -f ${datapath2}/mem001/INPUT/coupler.res ${pathout}
/bin/cp -f ${datapath2}/mem001/INPUT/fv_core.res.nc ${pathout}
echo "done computing ensemble mean restart files `date`"
