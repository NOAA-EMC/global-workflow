$ echo ${RUNTESTS_DIR}
/gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI/9924/RUNTESTS
$ echo ${HASH}
78c89a111902f81bdd3c8d51c1ca05ebdf7c5af5
$ mkdir -p ${RUNTESTS_DIR}
$ ${HOMEgfs}/workflow/generate_workflows.sh -G -t ${HASH} ${RUNTESTS_DIR}
The RUNTESTS directory /gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI/9924/RUNTESTS already exists.
Would you like to remove it?
Running all GFS cases in /gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI/9924/global-workflow/ci/cases/pr
Begin link_workflow.sh at Thu 20 Mar 2025 06:18:41 AM UTC
/gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI/9924/global-workflow/sorc/link_workflow.sh: line 59: /gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI/9924/global-workflow/sorc/gfs_utils.fd/ush/detect_machine.sh: No such file or directory
End link_workflow.sh at 06:18:41 with error code 1 (time elapsed: 00:00:00)
link_workflow.sh failed!