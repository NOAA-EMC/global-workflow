==========================================================
Note for EFSOI
==========================================================

2020.06.05 (Liaofan Lin)

There are the following types of files: 
   1) exists and is updated for general purposes and then git-pushed; 
   2) is added for EFSOI and then git-pushed; and
   3) exists and is updated for EFSOI and then git pushed 
   4) exists and is updated but not git-pushed
   5) to bo pushed

(1) Files that exist and are updated for general run setup and then git-pushed
[AFE 2020-11-16: these are user- and EFSOI-run-specific changes to
config files - for now will be not set to repo. Have to ask somebody preferred
way to handle this regarding repo.]

   * parm/config/config.base.emc.dyn
   * parm/config/config.efcs
   * parm/config/config.eobs
[AFE 2020-11-16 also config.resources!]
    
(2) Files that are added for EFSOI and then git-pushed
[AFE 2020-11-16: these have been added and committed]

   for gdaseupdfsoi:
      * jobs/JGLOBAL_EFSOI_UPDATE
      * jobs/rocoto/eupdfsoi.sh
      * scripts/exglobal_efsoi_update_fv3gfs.sh.ecf
      * parm/config/config.eupdfsoi

   for gdasesfcfsoi:
      * jobs/rocoto/esfcfsoi.sh	
      * jobs/JGDAS_EFSOI_SURFACE
      * scripts/exglobal_efsoi_surface_fv3gfs.sh.ecf
      * parm/config/config.esfcfsoi

   for gdasecenfsoi
      * jobs/rocoto/ecenfsoi.sh
      * jobs/JGDAS_EFSOI_RECENTER
      * scripts/exglobal_efsoi_recenter_fv3gfs.sh.ecf
      * parm/config/config.ecenfsoi
 
   for gdasefcsfsoi
      * jobs/rocoto/efcsfsoi.sh
      * jobs/JGDAS_EFSOI_FCST
      * scripts/exglobal_efsoi_fcst_fv3gfs.sh.ecf
      * parm/config/config.efcsfsoi

   for gdaseposfsoi
      * jobs/rocoto/eposfsoi.sh
      * jobs/JGDAS_EFSOI_POST
      * scripts/exglobal_efsoi_post_fv3gfs.sh.ecf
      * parm/config/config.eposfsoi
	                 
(3) Files that exist and are updated for EFSOI and then git-pushed

   for all EFSOI tasks
      * env/HERA.env
      * parm/config/config.resources   
      * ush/rocoto/setup_workflow.py

   for gdasefcsfsoi only
      * scripts/exglobal_fcst_nemsfv3gfs.sh 	  
	     
(4) Files that exist and are updated but not git-pushed:

   * scripts/exglobal_enkf_update_fv3gfs.sh.ecf
   * scripts/exglobal_enkf_surface_fv3gfs.sh.ecf
   * scripts/exglobal_enkf_recenter_fv3gfs.sh.ecf
   * scripts/exglobal_enkf_fcst_fv3gfs.sh.ecf
   
(5) Note, to be pushed

	  
   
	  
	  
