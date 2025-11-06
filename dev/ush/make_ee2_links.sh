#!/bin/bash

set -eu
cwd=${PWD}
cd C384C192/20240610/gdas.20230401/18/analysis/atmos
ln -s gdas.t18z.abias gdas.t18z.abias.txt
ln -s gdas.t18z.abias C384C192/20240610/gdas.20230401/18/analysis/atmos/gdas.t18z.abias.txt
ln -s gdas.t18z.abias_air C384C192/20240610/gdas.20230401/18/analysis/atmos/gdas.t18z.abias_air.txt
ln -s gdas.t18z.abias_int C384C192/20240610/gdas.20230401/18/analysis/atmos/gdas.t18z.abias_int.txt
ln -s gdas.t18z.abias_pc C384C192/20240610/gdas.20230401/18/analysis/atmos/gdas.t18z.abias_pc.txt
ln -s gdas.t18z.radstat C384C192/20240610/gdas.20230401/18/analysis/atmos/gdas.t18z.radstat.tar
cd "${cwd}"

cd C384mx025/20241120/gdas.20210701/00/analysis/atmos
ln -s gdas.t00z.abias gdas.t00z.abias.txt
ln -s gdas.t00z.abias_air gdas.t00z.abias_air.txt
ln -s gdas.t00z.abias_int gdas.t00z.abias_int.txt
ln -s gdas.t00z.abias_pc gdas.t00z.abias_pc.txt
ln -s gdas.t00z.radstat gdas.t00z.radstat.tar
cd "${cwd}"

cd C48C48mx500/20250327/enkfgdas.20210323/12
cwd_4848500=${pwd}
for dir in mem*; do
  cd "${dir}/analysis/ocean"
  ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
  cd "${cwd_4848500}"
done
cd "${cwd}"

cd C48C48mx500/20250327/enkfgdas.20210324/12
cwd_4848500=${PWD}
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t12z.ratmi003.nc enkfgdas.t12z.recentered_increment.i003.nc
  ln -s enkfgdas.t12z.ratmi009.nc enkfgdas.t12z.recentered_increment.i009.nc
  ln -s enkfgdas.t12z.ratminc.nc enkfgdas.t12z.recentered_increment.i006.nc
  cd "${cwd_4848500}"
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t12z.ocninc.nc enkfgdas.t12z.increment.i006.nc
  cd "${cwd_4848500}"
done
cd "${cwd}"

cd C48C48mx500/20250327/enkfgdas.20210324/18
cwd_4848500=${PWD}
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t18z.atmi003.nc enkfgdas.t18z.increment.atm.i003.nc
  ln -s enkfgdas.t18z.atmi009.nc enkfgdas.t18z.increment.atm.i009.nc
  ln -s enkfgdas.t18z.atminc.nc enkfgdas.t18z.increment.atm.i006.nc
  ln -s enkfgdas.t18z.ratmi003.nc enkfgdas.t18z.recentered_increment.i003.nc
  ln -s enkfgdas.t18z.ratmi009.nc enkfgdas.t18z.recentered_increment.i009.nc
  ln -s enkfgdas.t18z.ratminc.nc enkfgdas.t18z.recentered_increment.i006.nc
  cd "${cwd_4848500}"
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t18z.ocninc.nc enkfgdas.t18z.increment.i006.nc
  cd "${cwd_4848500}"
done
cd "${cwd}"

cd C48C48mx500/20250327/gdas.20210323/12/analysis/atmos
ln -s gdas.t12z.abias gdas.t12z.abias.txt
ln -s gdas.t12z.abias_air gdas.t12z.abias_air.txt
ln -s gdas.t12z.abias_int gdas.t12z.abias_int.txt
ln -s gdas.t12z.abias_pc gdas.t12z.abias_pc.txt
ln -s gdas.t12z.radstat gdas.t12z.radstat.tar
cd "${cwd}"

cd C48C48mx500/20250327/gdas.20210324/12/analysis/ocean
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C48C48mx500/20250327/gdas.20210324/18/analysis/atmos
ln -s gdas.t12z.abias gdas.t12z.abias.txt
ln -s gdas.t12z.abias_air gdas.t12z.abias_air.txt
ln -s gdas.t12z.abias_int gdas.t12z.abias_int.txt
ln -s gdas.t12z.abias_pc gdas.t12z.abias_pc.txt
ln -s gdas.t12z.atmi003.nc gdas.t12z.increment.atm.i003.nc
ln -s gdas.t12z.atmi009.nc gdas.t12z.increment.atm.i009.nc
ln -s gdas.t12z.atminc.nc gdas.t12z.increment.atm.i006.nc
ln -s gdas.t12z.radstat gdas.t12z.radstat.tar
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C48C48mx500/20250327/gdas.20210324/18/analysis/atmos
ln -s gdas.t18z.abias gdas.t18z.abias.txt
ln -s gdas.t18z.abias_air gdas.t18z.abias_air.txt
ln -s gdas.t18z.abias_int gdas.t18z.abias_int.txt
ln -s gdas.t18z.abias_pc gdas.t18z.abias_pc.txt
ln -s gdas.t18z.atma003.ensres.nc gdas.t18z.ensres_analysis.atm.i003.nc
ln -s gdas.t18z.atma009.ensres.nc gdas.t18z.ensres_analysis.atm.i009.nc
ln -s gdas.t18z.atmanl.ensres.nc gdas.t18z.ensres_analysis.atm.i006.nc
ln -s gdas.t18z.atmanl.nc gdas.t18z.analysis.atm.a006.nc
ln -s gdas.t18z.atmi003.nc gdas.t18z.increment.atm.i003.nc
ln -s gdas.t18z.atmi009.nc gdas.t18z.increment.atm.i009.nc
ln -s gdas.t18z.atminc.nc gdas.t18z.increment.atm.i006.nc
ln -s gdas.t18z.cnvstat gdas.t18z.cnvstat.tar
ln -s gdas.t18z.dtfanl.nc gdas.t18z.analysis.dtf.a006.nc
ln -s gdas.t18z.gsistat gdas.t18z.gsistat.txt
ln -s gdas.t18z.oznstat gdas.t18z.oznstat.tar
ln -s gdas.t18z.radstat gdas.t18z.radstat.tar
ln -s gdas.t18z.sfcanl.nc gdas.t18z.analysis.sfc.a006.nc
cd "${cwd}"

cd C48C48mx500/20250327/gdas.20210324/18/analysis/ocean
ln -s gdas.t18z.ocninc.nc gdas.t18z.increment.i006.nc
cd "${cwd}"

cd C48C48mx500/20250808/enkfgdas.20210324/12
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t12z.atmi003.nc enkfgdas.t12z.increment.atm.i003.nc
  ln -s enkfgdas.t12z.atmi009.nc enkfgdas.t12z.increment.atm.i009.nc
  ln -s enkfgdas.t12z.atminc.nc enkfgdas.t12z.increment.atm.i006.nc
  cd "${cwd_4848500}"
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t12z.ocninc.nc enkfgdas.t12z.increment.i006.nc
  cd "${cwd_4848500}"
done
cd "${cwd}"

cd C48C48mx500/20250808/enkfgdas.20210324/18
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t18z.atmi003.nc enkfgdas.t18z.increment.atm.i003.nc
  ln -s enkfgdas.t18z.atmi009.nc enkfgdas.t18z.increment.atm.i009.nc
  ln -s enkfgdas.t18z.atminc.nc enkfgdas.t18z.increment.atm.i006.nc
  ln -s enkfgdas.t18z.ratmi003.nc enkfgdas.t18z.recentered_increment.i003.nc
  ln -s enkfgdas.t18z.ratmi009.nc enkfgdas.t18z.recentered_increment.i009.nc
  ln -s enkfgdas.t18z.ratminc.nc enkfgdas.t18z.recentered_increment.i006.nc
  cd "${cwd_4848500}"
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t18z.ocninc.nc enkfgdas.t18z.increment.i006.nc
  cd "${cwd_4848500}"
done
cd "${cwd}"

cd C48C48mx500/20250808/gdas.20210323/12/analysis/ocean
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C48C48mx500/20250808/gdas.20210324/12/analysis/atmos

ln -s gdas.t12z.abias gdas.t12z.abias.txt
ln -s gdas.t12z.abias_air gdas.t12z.abias_air.txt
ln -s gdas.t12z.abias_int gdas.t12z.abias_int.txt
ln -s gdas.t12z.abias_pc gdas.t12z.abias_pc.txt
ln -s gdas.t12z.atmi003.nc gdas.t12z.increment.atm.i003.nc
ln -s gdas.t12z.atmi009.nc gdas.t12z.increment.atm.i009.nc
ln -s gdas.t12z.atminc.nc gdas.t12z.increment.atm.i006.nc
ln -s gdas.t12z.radstat gdas.t12z.radstat.tar
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C48C48mx500/20250808/gdas.20210324/18/analysis/atmos
ln -s gdas.t18z.abias gdas.t18z.abias.txt
ln -s gdas.t18z.abias_air gdas.t18z.abias_air.txt
ln -s gdas.t18z.abias_int gdas.t18z.abias_int.txt
ln -s gdas.t18z.abias_pc gdas.t18z.abias_pc.txt
ln -s gdas.t18z.atma003.ensres.nc gdas.t18z.ensres_analysis.atm.i003.nc
ln -s gdas.t18z.atma009.ensres.nc gdas.t18z.ensres_analysis.atm.i009.nc
ln -s gdas.t18z.atmanl.ensres.nc gdas.t18z.ensres_analysis.atm.i006.nc
ln -s gdas.t18z.atmanl.nc gdas.t18z.analysis.atm.a006.nc
ln -s gdas.t18z.atmi003.nc gdas.t18z.increment.atm.i003.nc
ln -s gdas.t18z.atmi009.nc gdas.t18z.increment.atm.i009.nc
ln -s gdas.t18z.atminc.nc gdas.t18z.increment.atm.i006.nc
ln -s gdas.t18z.cnvstat gdas.t18z.cnvstat.tar
ln -s gdas.t18z.dtfanl.nc gdas.t18z.analysis.dtf.a006.nc
ln -s gdas.t18z.gsistat gdas.t18z.gsistat.txt
ln -s gdas.t18z.oznstat gdas.t18z.oznstat.tar
ln -s gdas.t18z.radstat gdas.t18z.radstat.tar
ln -s gdas.t18z.sfcanl.nc gdas.t18z.analysis.sfc.a006.nc
cd "${cwd}"

cd C48C48mx500/20250808/gdas.20210324/18/analysis/ocean
ln -s gdas.t18z.ocninc.nc gdas.t18z.increment.i006.nc
cd "${cwd}"

cd C96C48/20250327/enkfgdas.20211220/12
cwd_9648=${PWD}
for dir in mem*; do
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t12z.ocninc.nc enkfgdas.t12z.increment.i006.nc
  cd "${cwd_9648}"
done
cd "${cwd}"

cd C96C48/20250327/enkfgdas.20211221/06/ensstat/analysis/atmos
ln -s enkfgdas.t06z.abias.ensmean enkfgdas.t06z.abias.ensmean.txt
ln -s enkfgdas.t06z.abias_air.ensmean enkfgdas.t06z.abias_air.ensmean.txt
ln -s enkfgdas.t06z.abias_int.ensmean enkfgdas.t06z.abias_int.ensmean.txt
ln -s enkfgdas.t06z.abias_pc.ensmean enkfgdas.t06z.abias_pc.ensmean.txt
ln -s enkfgdas.t06z.atmi003.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i003.nc
ln -s enkfgdas.t06z.atmi009.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i009.nc
ln -s enkfgdas.t06z.atminc.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i006.nc
ln -s enkfgdas.t06z.enkfstat enkfgdas.t06z.enkfstat.txt
ln -s enkfgdas.t06z.gsistat.ensmean enkfgdas.t06z.gsistat.ensmean.tar
ln -s enkfgdas.t06z.loginc.txt enkfgdas.t06z.increment.done.txt
ln -s enkfgdas.t06z.oznstat.ensmean enkfgdas.t06z.oznstat.ensmean.tar
ln -s enkfgdas.t06z.sfci003.nc enkfgdas.t06z.increment.sfc.i003.nc
ln -s enkfgdas.t06z.sfci006.nc enkfgdas.t06z.increment.sfc.i006.nc
ln -s enkfgdas.t06z.sfci009.nc enkfgdas.t06z.increment.sfc.i009.nc
cd "${cwd}"

cd C96C48/20250327/enkfgdas.20211221/06
cwd_9648=${PWD}
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t06z.atmi003.nc enkfgdas.t06z.increment.atm.i003.nc
  ln -s enkfgdas.t06z.atmi009.nc enkfgdas.t06z.increment.atm.i009.nc
  ln -s enkfgdas.t06z.atminc.nc enkfgdas.t06z.increment.atm.i006.nc
  ln -s enkfgdas.t06z.ratmi003.nc enkfgdas.t06z.recentered_increment.i003.nc
  ln -s enkfgdas.t06z.ratmi009.nc enkfgdas.t06z.recentered_increment.i009.nc
  ln -s enkfgdas.t06z.ratminc.nc enkfgdas.t06z.recentered_increment.i006.nc
  ln -s enkfgdas.t06z.sfci003.nc enkfgdas.t06z.increment.sfc.i003.nc
  ln -s enkfgdas.t06z.sfci006.nc enkfgdas.t06z.increment.sfc.i006.nc
  ln -s enkfgdas.t06z.sfci009.nc enkfgdas.t06z.increment.sfc.i009.nc
  for tile in {1..6}; do
	 ln -s "sfc_inc.tile${tile}.nc" "enkfgdas.t06z.increment.sfc.i006.tile${tile}.nc"
  done
  cd "${cwd_9648}"
done
cd "${cwd}"

cd C96C48/20250327/enkfgdas.20240223/18
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  for tile in {1..6}; do
	 ln -s "enkfgdas.t18z.cubed_sphere_grid_atminc.tile${tile}.nc" "enkfgdas.t18z.jedi_increment.atm.i006.tile${tile}.nc"
  done
  ln -s enkfgdas.t18z.cubed_sphere_grid_ratminc.nc enkfgdas.t18z.recentered_jedi_increment.atm.i006.nc
  cd "${cwd_9648}"
done
cd "${cwd}"

cd C96C48/20250327/gdas.20211220/12/analysis/atmos
ln -s gdas.t12z.abias gdas.t12z.abias.txt
ln -s gdas.t12z.abias_air gdas.t12z.abias_air.txt
ln -s gdas.t12z.abias_int gdas.t12z.abias_int.txt
ln -s gdas.t12z.abias_pc gdas.t12z.abias_pc.txt
ln -s gdas.t12z.radstat gdas.t12z.radstat.tar
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C96C48/20250327/gdas.20211220/18/analysis/atmos
ln -s gdas.t18z.abias gdas.t18z.abias.txt
ln -s gdas.t18z.abias_air gdas.t18z.abias_air.txt
ln -s gdas.t18z.abias_pc gdas.t18z.abias_pc.txt
ln -s gdas.t18z.radstat gdas.t18z.radstat.tar
cd "${cwd}"

cd C96C48/20250327/gdas.20211221/06/analysis/atmos
ln -s gdas.t06z.abias gdas.t06z.abias.txt
ln -s gdas.t06z.abias_air gdas.t06z.abias_air.txt
ln -s gdas.t06z.abias_int gdas.t06z.abias_int.txt
ln -s gdas.t06z.abias_pc gdas.t06z.abias_pc.txt
ln -s gdas.t06z.atma003.ensres.nc gdas.t06z.ensres_analysis.atm.i003.nc
ln -s gdas.t06z.atma009.ensres.nc gdas.t06z.ensres_analysis.atm.i009.nc
ln -s gdas.t06z.atmanl.ensres.nc gdas.t06z.ensres_analysis.atm.i006.nc
ln -s gdas.t06z.atmanl.nc gdas.t06z.analysis.atm.a006.nc
ln -s gdas.t06z.atmi003.nc gdas.t06z.increment.atm.i003.nc
ln -s gdas.t06z.atmi009.nc gdas.t06z.increment.atm.i009.nc
ln -s gdas.t06z.atminc.nc gdas.t06z.increment.atm.i006.nc
ln -s gdas.t06z.dtfanl.nc gdas.t06z.analysis.dtf.a006.nc
ln -s gdas.t06z.gsistat gdas.t06z.gsistat.txt
ln -s gdas.t06z.loganl.txt gdas.t06z.analysis.done.txt
ln -s gdas.t06z.loginc.txt gdas.t06z.increment.done.txt
ln -s gdas.t06z.oznstat gdas.t06z.oznstat.tar
ln -s gdas.t06z.sfcanl.nc gdas.t06z.analysis.sfc.a006.nc
ln -s sfc_inc.tile1.nc gdas.t06z.increment.sfc.i003.nc
cd "${cwd}"

cd C96C48/20250327/gdas.20211221/18/analysis/atmos
ln -s gdas.t18z.atminc.nc gdas.t18z.increment.atm.i006.nc
ln -s gdas.t18z.cubed_sphere_grid_atminc.tile1.nc gdas.t18z.jedi_increment.atm.i006.tile1.nc
ln -s gdas.t18z.cubed_sphere_grid_ratminc.tile1.nc gdas.t18z.recentered_jedi_increment.atm.i006.tile1.nc
cd "${cwd}"

cd C96C48/20250808/enkfgdas.20211220/12
for dir in mem*; do
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t12z.ocninc.nc enkfgdas.t12z.increment.i006.nc
  cd "${cwd_9648}"
done
cd "${cwd}"

cd C96C48/20250808/enkfgdas.20220515/06/ensstat/analysis/atmos
ln -s enkfgdas.t06z.abias.ensmean enkfgdas.t06z.abias.ensmean.txt
ln -s enkfgdas.t06z.abias_air.ensmean enkfgdas.t06z.abias_air.ensmean.txt
ln -s enkfgdas.t06z.abias_int.ensmean enkfgdas.t06z.abias_int.ensmean.txt
ln -s enkfgdas.t06z.abias_pc.ensmean enkfgdas.t06z.abias_pc.ensmean.txt
ln -s enkfgdas.t06z.atmi003.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i003.nc
ln -s enkfgdas.t06z.atmi009.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i009.nc
ln -s enkfgdas.t06z.atminc.ensmean.nc enkfgdas.t06z.ensmean_increment.atm.i006.nc
ln -s enkfgdas.t06z.cnvstat.ensmean enkfgdas.t06z.cnvstat.ensmean.tar
ln -s enkfgdas.t06z.enkfstat enkfgdas.t06z.enkfstat.txt
ln -s enkfgdas.t06z.gsistat.ensmean enkfgdas.t06z.gsistat.ensmean.tar
ln -s enkfgdas.t06z.loginc.txt enkfgdas.t06z.increment.done.txt
ln -s enkfgdas.t06z.oznstat.ensmean enkfgdas.t06z.oznstat.ensmean.tar
ln -s enkfgdas.t06z.radstat.ensmean enkfgdas.t06z.radstat.ensmean.tar
ln -s enkfgdas.t06z.sfci003.nc enkfgdas.t06z.increment.sfc.i003.nc
ln -s enkfgdas.t06z.sfci006.nc enkfgdas.t06z.increment.sfc.i006.nc
ln -s enkfgdas.t06z.sfci009.nc enkfgdas.t06z.increment.sfc.i009.nc
ln -s enkfgdas.t06z.atmi003.nc enkfgdas.t06z.increment.atm.i003.nc
ln -s enkfgdas.t06z.atmi009.nc enkfgdas.t06z.increment.atm.i009.nc
ln -s enkfgdas.t06z.atminc.nc enkfgdas.t06z.increment.atm.i006.nc
ln -s enkfgdas.t06z.ratmi003.nc enkfgdas.t06z.recentered_increment.i003.nc
ln -s enkfgdas.t06z.ratmi009.nc enkfgdas.t06z.recentered_increment.i009.nc
ln -s enkfgdas.t06z.ratminc.nc enkfgdas.t06z.recentered_increment.i006.nc
ln -s enkfgdas.t06z.sfci003.nc enkfgdas.t06z.increment.sfc.i003.nc
ln -s enkfgdas.t06z.sfci006.nc enkfgdas.t06z.increment.sfc.i006.nc
ln -s enkfgdas.t06z.sfci009.nc enkfgdas.t06z.increment.sfc.i009.nc
ln -s sfc_inc.tile1.nc enkfgdas.t06z.increment.sfc.i003.nc
cd "${cwd}"

cd C96C48/20250808/enkfgdas.20220223/18
for dir in mem*; do
  cd "${dir}/analysis/atmos"
  for tile in {1..6}; do
	 ln -s "enkfgdas.t18z.cubed_sphere_grid_atminc.tile${tile}.nc" "enkfgdas.t18z.jedi_increment.atm.i006.tile${tile}.nc"
  done
  cd "${cwd_9648}"
done
cd "${cwd}"

cd C96C48/20250808/gdas.20211220/12/analysis/atmos
ln -s gdas.t12z.abias gdas.t12z.abias.txt
ln -s gdas.t12z.abias_air gdas.t12z.abias_air.txt
ln -s gdas.t12z.abias_int gdas.t12z.abias_int.txt
ln -s gdas.t12z.abias_pc gdas.t12z.abias_pc.txt
ln -s gdas.t12z.radstat gdas.t12z.radstat.tar
ln -s gdas.t12z.ocninc.nc gdas.t12z.increment.i006.nc
cd "${cwd}"

cd C96C48/20250808/gdas.20211220/18/analysis/atmos
ln -s gdas.t18z.abias gdas.t18z.abias.txt
ln -s gdas.t18z.abias_air gdas.t18z.abias_air.txt
ln -s gdas.t18z.abias_int gdas.t18z.abias_int.txt
ln -s gdas.t18z.abias_pc gdas.t18z.abias_pc.txt
ln -s gdas.t18z.radstat gdas.t18z.radstat.tar
cd "${cwd}"

cd C96C48/20250808/gdas.20220515/06/analysis/atmos
ln -s gdas.t06z.abias gdas.t06z.abias.txt
ln -s gdas.t06z.abias_air gdas.t06z.abias_air.txt
ln -s gdas.t06z.abias_int gdas.t06z.abias_int.txt
ln -s gdas.t06z.abias_pc gdas.t06z.abias_pc.txt
ln -s gdas.t06z.atma003.ensres.nc gdas.t06z.ensres_analysis.atm.i003.nc
ln -s gdas.t06z.atma009.ensres.nc gdas.t06z.ensres_analysis.atm.i009.nc
ln -s gdas.t06z.atmanl.ensres.nc gdas.t06z.ensres_analysis.atm.i006.nc
ln -s gdas.t06z.atmanl.nc gdas.t06z.analysis.atm.a006.nc
ln -s gdas.t06z.atmi003.nc gdas.t06z.increment.atm.i003.nc
ln -s gdas.t06z.atmi009.nc gdas.t06z.increment.atm.i009.nc
ln -s gdas.t06z.atminc.nc gdas.t06z.increment.atm.i006.nc
ln -s gdas.t06z.cnvstat gdas.t06z.cnvstat.tar
ln -s gdas.t06z.dtfanl.nc gdas.t06z.analysis.dtf.a006.nc
ln -s gdas.t06z.gsistat gdas.t06z.gsistat.txt
ln -s gdas.t06z.loganl.txt gdas.t06z.analysis.done.txt
ln -s gdas.t06z.loginc.txt gdas.t06z.increment.done.txt
ln -s gdas.t06z.oznstat gdas.t06z.oznstat.tar
ln -s gdas.t06z.radstat gdas.t06z.radstat.tar
ln -s gdas.t06z.sfcanl.nc gdas.t06z.analysis.sfc.a006.nc
ln -s sfc_inc.tile1.nc gdas.t06z.increment.sfc.i003.nc
cd "${cwd}"

cd C96C48/20250808/gdas.20220515/18/analysis/atmos
ln -s gdas.t18z.cubed_sphere_grid_atminc.tile1.nc gdas.t18z.jedi_increment.atm.i006.tile1.nc
cd "${cwd}"

cd retro_ICs/enkfgdas.20241115/06
for dir in mem*; do
  cd "${dir}/analysis/ocean"
  ln -s enkfgdas.t06z.ocninc.nc enkfgdas.t06z.increment.i006.nc
  cd "${cwd_9648}"
  cd "${dir}/analysis/atmos"
  ln -s enkfgdas.t06z.atmi003.nc enkfgdas.t06z.increment.atm.i003.nc
  ln -s enkfgdas.t06z.atmi009.nc enkfgdas.t06z.increment.atm.i009.nc
  ln -s enkfgdas.t06z.atminc.nc enkfgdas.t06z.increment.atm.i006.nc
  ln -s enkfgdas.t06z.ratmi003.nc enkfgdas.t06z.recentered_increment.i003.nc
  ln -s enkfgdas.t06z.ratmi009.nc enkfgdas.t06z.recentered_increment.i009.nc
  ln -s enkfgdas.t06z.ratminc.nc enkfgdas.t06z.recentered_increment.i006.nc
  for tile in {1..6}; do
	 ln -s "sfc_inc.tile${tile}.nc" "enkfgdas.t06z.increment.sfc.i006.tile${tile}.nc"
  done
  cd "${cwd_9648}"
done
cd "${cwd}"

cd retro_ICs/gdas.20241115/06/analysis/atmos
ln -s gdas.t06z.abias gdas.t06z.abias.txt
ln -s gdas.t06z.abias.orig gdas.t06z.abias.orig.txt
ln -s gdas.t06z.abias_air gdas.t06z.abias_air.txt
ln -s gdas.t06z.abias_air.orig gdas.t06z.abias_air.orig.txt
ln -s gdas.t06z.abias_int gdas.t06z.abias_int.txt
ln -s gdas.t06z.abias_pc gdas.t06z.abias_pc.txt
ln -s gdas.t06z.abias_pc.orig gdas.t06z.abias_pc.orig.txt
ln -s gdas.t06z.atma003.ensres.nc gdas.t06z.ensres_analysis.atm.i003.nc
ln -s gdas.t06z.atma009.ensres.nc gdas.t06z.ensres_analysis.atm.i009.nc
ln -s gdas.t06z.atmanl.ensres.nc gdas.t06z.ensres_analysis.atm.i006.nc
ln -s gdas.t06z.atmanl.nc gdas.t06z.analysis.atm.a006.nc
ln -s gdas.t06z.atmi003.nc gdas.t06z.increment.atm.i003.nc
ln -s gdas.t06z.atmi009.nc gdas.t06z.increment.atm.i009.nc
ln -s gdas.t06z.atminc.nc gdas.t06z.increment.atm.i006.nc
ln -s gdas.t06z.cnvstat gdas.t06z.cnvstat.tar
ln -s gdas.t06z.dtfanl.nc gdas.t06z.analysis.dtf.a006.nc
ln -s gdas.t06z.gsistat gdas.t06z.gsistat.txt
ln -s gdas.t06z.loganl.txt gdas.t06z.analysis.done.txt
ln -s gdas.t06z.loginc.txt gdas.t06z.increment.done.txt
ln -s gdas.t06z.oznstat gdas.t06z.oznstat.tar
ln -s gdas.t06z.radstat gdas.t06z.radstat.tar
ln -s gdas.t06z.sfcanl.nc gdas.t06z.analysis.sfc.a006.nc
ln -s sfc_inc.tile1.nc gdas.t06z.increment.sfc.i003.nc
cd "${cwd}"

cd retro_ICs/gdas.20241115/06/analysis/ocean
ln -s gdas.t06z.ocninc.nc gdas.t06z.increment.i006.nc
cd "${cwd}"

echo "Success!!"
exit 0
