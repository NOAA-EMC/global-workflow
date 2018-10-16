#! /bin/sh
#set -xue

#./setup_case.sh -F -c -p WCOSS_DELL_P3  cases/four_cycle_mode_prfv3rt1_GFS\@C768_ENKF\@384+80MEM-Dell.yaml devtest

#./make_ecflow_files_for.sh -v /gpfs/dell2/emc/modeling/noscrub/Jian.Kuang/nwprod/gfs.v15.0.0pre10/ecf/ecfutils/expdir/test 2018091800 2018092212
#exit 0
#set +e

yes | for x in 00 06 12 18; do
  ecflow_client --delete force /test$x 
  ecflow_client --load /gpfs/dell2/emc/modeling/noscrub/Jian.Kuang/ecflow/defs/test/test$x.def
  ecflow_client --begin /test$x
done
exit 0
ecflow_client --force=complete recursive /test06/gfs
ecflow_client --force=complete recursive /test06/gdas
ecflow_client --run force /test12/gdas/jemc_dump_waiter
ecflow_client --run force /test12/gfs/jemc_dump_waiter
