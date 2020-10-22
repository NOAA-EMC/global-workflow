GFS RELEASE NOTES (GFS.v16.0.0) -- October 9, 2020

-------
PRELUDE
-------
  	
* GFS version 16.0 is the first major upgrade to Finite Volume Cubed Sphere (FV3) dynamical core based GFS which replaced the spectral dynamical core in June 2019. In this upgrade, the number of model vertical layers is increased from 64 to 127 and the model top is extended from the upper stratosphere (~55 km height) to the mesopause (~80 km height).   With this upgrade, for the first time, the operational stand alone global deterministic WAVEWATCH III based wave model Multi_1 (wave_multi_1.v3.3) is merged into the GFS system. The WAVEWATCH III model is updated and coupled to the GFS using a one-way coupling scheme where the atmospheric model provides forcing to the wave model using the NOAA Environmental Modeling System (NEMS). Major changes have also been made in other components of the forecast system including model physics,  data assimilation,  system infrastructure, post-processing and product generation.

EMC has conducted a set of retrospective and real-time experiments, covering part of the 2018 hurricane season and the entire period from May 10, 2019 to the present, for a comprehensive evaluation of the model upgrades. GFSv16 showed improved forecast skills in many areas.  For more details please refer to the Science Change Notice: https://docs.google.com/document/d/1pDLqP6ne2grEJ2vMfw7RnkwyzRsGpGPMb1d2DeDuu2E/edit.

*  GFS.v16 has been reorganized to use a COMPONENT directory structure to separate the atmos and wave components.

*  This release note describes the overall changes made to the entire system.  More details about changes in science and structure of the data assimilation system are documented in gfs.v16.0.0/sorc/gsi.fd/doc/Release_Notes.gfsda.v16.0.0.txt.  Details about downstream product generation is documented in Release_Notes.gfs_downstream.v16.0.0.txt.

---------------------------
IMPLEMENTATION INSTRUCTIONS
---------------------------

* The NOAA VLab and both the NOAA-EMC and NCAR organization spaces on GitHub.com are used to manage the GFS.v16 code. The SPA(s) handling the GFS.v16 implementation need to have permissions to clone VLab gerrit repositories and the private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions. Please contact Fanglin.Yang@noaa.gov if there is any VLAB access issue and/or the individual code managers listed under item #6) below.  Please follow the following steps to install the package on WCOSS-Dell:

  1) cd $NWROOTp3
  2) mkdir gfs.v16.0.0
  3) cd gfs.v16.0.0
  4) git  clone  -b EMC-v16.0.0  https://github.com/NOAA-EMC/global-workflow.git .
  5) cd sorc
  6) ./checkout.sh -o
	* This script extracts the following GFS components:
	MODEL		tag GFS.v16.0.13			Jun.Wang@noaa.gov
 	GSI 		tag gfsda.v16.0.0			Russ.Treadon@noaa.gov
	GLDAS		tag gldas_gfsv16_release.v1.11.0	Helin.Wei@noaa.gov
	UFS_UTIL	tag ops-gfsv16.0.0			George.Gayno@noaa.gov
	POST		tag upp_gfsv16_release.v1.1.0		Wen.Meng@noaa.gov
	WAFS		tag gfs_wafs.v6.0.9			Yali.Mao@noaa.gov

  7) ./build_all.sh
	*This script compiles all GFS components. Runtime output from the build for each package is written  to log files in directory logs. To build an individual program, for instance, gsi, use build_gsi.sh.

  8) ./link_fv3gfs.sh nco dell

  9) Please use the script /gpfs/dell6/emc/modeling/noscrub/emc.glopara/para_gfs/misc/copyic_v16rt2_nco.sh on Mars to copy initial conditions from EMC real-time parallel v16rt2 to $COM directory to start NCO’s parallel from the dump step of next cycle.  Please remember to change the COMROOT setting in this script to /gpfs/dell1/nco/ops/com.   It will rsync v16rt2 data from the dev machine to either dev or prod machine.  

Instruction notes:
------------------

* The GSI build script ($HOMEgfs/sorc/build_gsi.sh) must be executed prior to $HOMEgfs/sorc/build_enkf_chgres_recenter_nc.sh.   This automatically happens when executing $HOMEgfs/sorc/build_all.sh to build all GFS v16 executables.

* The RTOFS curfile*h variable settings must be updated in scripts/exgfs_wave_prep.sh when the RTOFS implementation occurs. The “_1hrly” and “_3hrly” text will be removed to update the filenames:

	Before RTOFS implementation (current settings):

        	curfile1h=${COMIN_WAV_CUR}/rtofs_glo_2ds_${fext}${fh3_rtofs}_1hrly_prog.nc
        	curfile3h=${COMIN_WAV_CUR}/rtofs_glo_2ds_${fext}${fh3_rtofs}_3hrly_prog.nc

	After RTOFS implementation:

        	curfile1h=${COMIN_WAV_CUR}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc
        	curfile3h=${COMIN_WAV_CUR}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc

* ecflow suite definition and scripts are saved in gfs.v16.0.0/ecflow/ecf

* POST contains restricted GTG (Graphic Turbulence Guidance) code provided NCAR.  Please do not post the GTG code in any public domain.

-----------
JOB CHANGES
-----------

Many job scripts have been added, removed or renamed to meet NCO script naming conventions for GFS.v16.

Renamed job scripts are:

* JGDAS_ENKF_RECENTER		->	JGDAS_ENKF_ECEN
* JGDAS_GEMPAK 			-> 	JGDAS_ATMOS_GEMPAK
* JGDAS_GEMPAK_META  		-> 	JGDAS_ATMOS_GEMPAK_META_NCDC
* JGDAS_VMINMON			->	JGDAS_ATMOS_VMINMON
* JGDAS_VERFRAD			->	JGDAS_ATMOS_VERFRAD
* JGDAS_VERFOZN			->	JGDAS_ATMOS_VERFOZN
* JGFS_AWIPS_20KM_1P0DEG 	-> 	JGFS_ATMOS_AWIPS_20KM_1P0DEG
* JGFS_AWIPS_G2 		-> 	JGFS_ATMOS_AWIPS_G2
* JGFS_CYCLONE_GENESIS 		-> 	JGFS_ATMOS_CYCLONE_GENESIS
* JGFS_CYCLONE_TRACKER 		-> 	JGFS_ATMOS_CYCLONE_TRACKER
* JGFS_FBWIND			-> 	JGFS_ATMOS_FBWIND
* JGFS_GEMPAK 			-> 	JGFS_ATMOS_GEMPAK
* JGFS_GEMPAK_META  		-> 	JGFS_ATMOS_GEMPAK_META
* JGFS_GEMPAK_NCDC_UPAPGIF	-> 	JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF
* JGFS_GEMPAK_PGRB2_SPEC 	-> 	JGFS_ATMOS_GEMPAK_PGRB2_SPEC
* JGFS_PGRB2_SPEC_NPOESS 	-> 	JGFS_ATMOS_PGRB2_SPEC_NPOESS
* JGFS_POSTSND 			-> 	JGFS_ATMOS_POSTSND
* JGFS_VMINMON			->	JGFS_ATMOS_VMINMON
* JGFS_WAFS			->	JGFS_ATMOS_WAFS
* JGFS_WAFS_BLENDING		->	JGFS_ATMOS_WAFS_BLENDING
* JGFS_WAFS_GCIP		->	JGFS_ATMOS_WAFS_GCIP
* JGFS_WAFS_GRIB2		->	JGFS_ATMOS_WAFS_GRIB2
* JGLOBAL_ANALYSIS		->	JGLOBAL_ATMOS_ANALYSIS
* JGLOBAL_EMCSFC_SFC_PREP 	-> 	JGLOBAL_ATMOS_EMCSFC_SFC_PREP
* JGLOBAL_ENKF_SELECT_OBS	->	JGDAS_ENKF_SELECT_OBS
* JGLOBAL_ENKF_UPDATE		->	JGDAS_ENKF_UPDATE
* JGLOBAL_TROPCY_QC_RELOC 	-> 	JGLOBAL_ATMOS_TROPCY_QC_RELOC
* JGLOBAL_NCEPPOST            	-> 	JGLOBAL_ATMOS_NCEPPOST
* JGLOBAL_POST_MANAGER      	->	JGLOBAL_ATMOS_POST_MANAGER

New job scripts are:

* JGDAS_ATMOS_ANALYSIS_DIAG
* JGDAS_ATMOS_CHGRES_FORENKF
* JGDAS_ATMOS_GLDAS
* JGDAS_ENKF_DIAG
* JGDAS_ENKF_SFC
* JGFS_ATMOS_FSU_GENESIS
* JGFS_ATMOS_WAFS_GRIB2_0P25
* JGFS_ATMOS_WAFS_BLENDING_0P25
* JGLOBAL_ATMOS_ANALYSIS_CALC
* JGLOBAL_WAVE_GEMPAK
* JGLOBAL_WAVE_INIT
* JGLOBAL_WAVE_POST_BNDPNT
* JGLOBAL_WAVE_POST_PNT
* JGLOBAL_WAVE_POST_SBS
* JGLOBAL_WAVE_PRDGEN_BULLS
* JGLOBAL_WAVE_PRDGEN_GRIDDED
* JGLOBAL_WAVE_PREP

Removed job scripts are:

* JGDAS_BULLS_NAVY
* JGDAS_TROPC
* JGFS_FAX
* JGFS_FAX_WAFS
* JGLOBAL_ENKF_INNOVATE_OBS

--------------
SCRIPT CHANGES
--------------

Many scripts have been added, removed or renamed to meet NCO script naming conventions for GFS.v16.

Renamed scripts are:

* exemcsfc_global_sfc_prep.sh.ecf	-> 	exemcsfc_global_sfc_prep.sh
* exgdas_nawips.sh.ecf 			-> 	exgdas_atmos_nawips.sh
* exgdas_nceppost.sh.ecf		-> 	exgdas_atmos_nceppost.sh
* exgdas_vrfminmon.sh.ecf		->	exgdas_atmos_vminmon.sh
* exgdas_vrfyrad.sh.ecf			->	exgdas_atmos_verfrad.sh
* exgdas_vrfyozn.sh.ecf			->	exgdas_atmos_verfozn.sh
* exgempak_gdas_gif_ncdc.sh.ecf 	-> 	exgdas_atmos_gempak_gif_ncdc.sh
* exgempak_gfs_gif_ncdc_skew_t.sh.ecf 	-> 	exgfs_atmos_gempak_gif_ncdc_skew_t.sh
* exgfs_awips_20km_1p0deg.sh.ecf	-> 	exgfs_atmos_awips_20km_1p0deg.sh
* exgfs_fbwind.sh.ecf 			-> 	exgfs_atmos_fbwind.sh
* exgfs_gempak_meta.sh.ecf		-> 	exgfs_atmos_gempak_meta.sh
* exgfs_grib_awips.sh.ecf		-> 	exgfs_atmos_grib_awips.sh
* exgfs_nawips.sh.ecf 			-> 	exgfs_atmos_nawips.sh
* exgfs_nceppost.sh.ecf			-> 	exgfs_atmos_nceppost.sh
* exgfs_pmgr.sh.ecf			-> 	exgfs_pmgr.sh
* exgfs_postsnd.sh.ecf			-> 	exgfs_atmos_postsnd.sh
* exgfs_prdgen_manager.sh.ecf 		-> 	exgfs_prdgen_manager.sh
* exgfs_vrfminmon.sh.ecf		->	exgfs_atmos_vminmon.sh
* exgfs_wafs_blending.sh.ecf		->	exgfs_atmos_wafs_blending.sh
* exgfs_wafs_gcip.sh.ecf		->	exgfs_atmos_wafs_gcip.sh
* exgfs_wafs_grib.sh.ecf		->	exgfs_atmos_wafs_grib.sh
* exgfs_wafs_grib2.sh.ecf		->	exgfs_atmos_wafs_grib2.sh
* exglobal_analysis_fv3gfs.sh.ecf	->	exglobal_atmos_analysis.sh
* exglobal_enkf_fcst_fv3gfs.sh.ecf	->	exgdas_enkf_fcst.sh
* exglobal_enkf_recenter_fv3gfs.sh.ecf	->	exgdas_enkf_ecen.sh
* exglobal_enkf_post_fv3gfs.sh.ecf	->	exgdas_enkf_post.sh
* exglobal_enkf_update_fv3gfs.sh.ecf	->	exgdas_enkf_update.sh
* exglobal_fcst_nemsfv3gfs.sh		-> 	exglobal_forecast.sh
* exglobal_grib2_special_npoess.sh.ecf	-> 	exgfs_atmos_grib2_special_npoess.sh
* exglobal_innovate_obs_fv3gfs.sh.ecf	->	exgdas_enkf_select_obs.sh
* exglobal_pmgr.sh.ecf			-> 	exglobal_atmos_pmgr.sh
* exgoes_nawips.sh.ecf			-> 	exgfs_atmos_goes_nawips.sh
* exnawips.sh.ecf			-> 	exgfs_atmos_nawips.sh
* extropcy_qc_reloc.sh.ecf		-> 	exglobal_atmos_tropcy_qc_reloc.sh

New scripts are:

* exgdas_atmos_gldas.sh
* exgdas_enkf_sfc.sh
* exgfs_atmos_wafs_grib2_0p25.sh
* exgfs_atmos_wafs_blending_0p25.sh
* exgfs_wave_init.sh
* exgfs_wave_nawips.sh
* exgfs_wave_post_bndpnt.sh
* exgfs_wave_post_gridded_sbs.sh
* exgfs_wave_post_pnt.sh
* exgfs_wave_prdgen_bulls.sh
* exgfs_wave_prdgen_gridded.sh
* exgfs_wave_prep.sh
* exgdas_atmos_chgres_forenkf.sh
* exglobal_atmos_analysis_calc.sh
* exglobal_diag.sh

Removed scripts are:

* exgdas_bulls_navy.sh.ecf
* exgdas_tropc.sh.ecf
* exgfs_fax.sh.ecf
* exgfs_fax_wafs.sh.ecf
* exgfs_grib_awips_g2.sh.ecf
* exgfs_grib_wafs.sh.ecf

-------------------
PARM/CONFIG CHANGES
-------------------

All JJOBS except for those used by downstream product generation source config files under ./gfs.v16.0.0/parm/config to set up job-specific parameters. The config.base is sourced by all JJOBS to set parameters that are common to either all JJOBS or are shared by more than one JJOBS. The config.anal is shared by a few analysis steps, config.wave is shared by the wave steps, and config.wafs is shared by the WAFS jobs. Below are the parm (config) files modified or added in GFS.v16.

Modified configs:

* config.anal
* config.arch
* config.awips
* config.base.emc.dyn
* config.base.nco.static
* config.earc
* config.ecen
* config.efcs
* config.eobs
* config.epos
* config.eupd
* config.fcst
* config.fv3
* config.fv3ic
* config.gempak
* config.post
* config.postsnd
* config.prep
* config.prepbufr
* config.resources
* config.vrfy

New configs:

* config.analcalc
* config.analdiag
* config.echgres
* config.ediag
* config.esfc
* config.gldas
* config.metp
* config.wafs
* config.wafsblending
* config.wafsblending0p25
* config.wafsgcip
* config.wafsgrib2
* config.wafsgrib20p25
* config.wave
* config.waveawipsbulls 
* config.waveawipsgridded
* config.wavegempak
* config.waveinit
* config.wavepostbndpnt
* config.wavepostpnt
* config.wavepostsbs
* config.waveprep

-----------
FIX CHANGES
-----------

* All fixed fields used by the system are placed under gfs.v16.0.0/fix, and further categorized based on the type of applications. During the NCO implementation process the fix_gsi and wafs fix files are copied from external repositories via sorc/checkout.sh and linked under /fix via sorc/link_fv3gfs.sh. All other fix files are copied from EMC's local archives via sorc/link_fv3gfs.sh: fix_am, fix_fv3_gmted2010, fix_gldas, fix_orog, fix_verif, fix_wave_gfs

The entire package takes 165 GB disk space to install. This ./fix directory alone takes ~153G.

New fix files:

* fix_am		- new solar constants, Thompson MP climatology, salinity climatology
* fix_fv3_gmted2010	- new fix_sfc subfolder
* fix_gldas 		- new folder with files for GLDAS package
* fix_orog		- new global lake files
* fix_verif		- new grid2obs files
* fix_wave_gfs		- new folder with files for wave component

---------------
PRODUCT CHANGES
---------------

* Please refer to GFSv16 SCN: https://docs.google.com/document/d/1pDLqP6ne2grEJ2vMfw7RnkwyzRsGpGPMb1d2DeDuu2E/edit

--------------------
RESOURCE INFORMATION
--------------------

* Frequency of run - 6 hourly cycle (00, 06, 12, 18Z) - no change from current operations

* Commonly used libraries, compiler, and modules are defined in gfs.v16.0.0/modulefiles. For FV3, GSI, GLDAS, UPP, WAFS they maintain their own module files under gfs.v16.0.0/sorc/(fv3gfs gsi gldas gfs_post gfs_wafs).fd/modulefiles

* Data retention time under $COMROOTp3 for GFS.v16 should be the same as GFS.v15.

* Disk space:  The current operational GFS.v15 takes about 10.7 TB online COM disk space per cycle, while GFS.v16 will require about 8.0 TB per cycle.

* Computational resources and run times:

	* Please refer to the following document for the details of node usage,threading, and walltimes set in ECFLOW job cards for all jobs:
	https://docs.google.com/spreadsheets/d/1XAa5mDWLQJSMgyxhR8W7RRuENJN7koJN-rIHLkTgieo/edit#gid=0

	* Please refer to the following document for the high watermark test results for the overall computational cost of the system:
	https://docs.google.com/presentation/d/1aNi5doryHO_lNhtTq-jGzFh9Wi4Xu1Z5DNb921nhw74/edit#slide=id.ga069802256_0_377

* Information about the major steps and actual runtimes from EMC high watermark tests are listed below:

	* JGLOBAL_FORECAST (GFS)
     	* 484 nodes, 3388 tasks, ptile=7, 4 threads/task
     	* Runtime:  125 minutes

  	* JGLOBAL_FORECAST (GDAS)
     	* 119 nodes, 833 tasks, ptile=7, 4 threads/task
	* Runtime:  22 minutes

  	* JGLOBAL_ATMOS_ANALYSIS (GFS)
     	* 250 nodes, 1000 tasks, ptile=4, 7 threads/task
     	* Runtime:  29 minutes

  	* JGLOBAL_ATMOS_ANALYSIS (GDAS)
     	* 250 nodes, 1000 tasks, ptile=4, 7 threads/task
     	* Runtime:  38 minutes

  	* JGDAS_ENKF_SELECT_OBS
     	* 120 nodes, 480 tasks, ptile=4, 7 threads/task
     	* Runtime:  3.8 minutes

  	* JGDAS_ENKF_UPDATE
     	* 240 nodes, 960 tasks, ptile=4, 7 threads/task
     	* Runtime:  26 minutes

  	* JGDAS_ENKF_ECEN
     	* 20 nodes, 80 tasks, ptile=4, 7 threads/task
	* Runtime:  4.4 minutes per realization
		* Concurrently run 3 realizations of JGDAS_ENKF_ECEN. Total node usage for 3 jobs x 20 nodes each = 60 nodes.

  	* JGDAS_ENKF_FCST
     	* 15 nodes, 420 tasks, ptile=28, 1 threads/task
     	* Runtime:  29  minutes per realization
        	* Concurrently run 40 realizations of JGDAS_ENKF_FCST.  Each job processes 2 EnKF
          	members.  Total node usage for 40 jobs x 15 nodes each = 600 nodes
        	* 40 EnKF forecast groups for GFS.v16 is an increase from the 20 EnKF forecast groups
          	currently run in operations.

  	* JGDAS_ENKF_POST
     	* 20 nodes, 80 tasks, ptile=4, 7 threads/task
        * Runtime:  11  minutes per realization
        	* Concurrently run 7 realizations of JGDAS_ENKF_POST.  7 forecasts processed, one
          	per job.   Total node usage for 7 jobs x 20 nodes each = 140 nodes.

---------------------------------------
PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
	* All components of this package need to be tested.  EMC is running a real-time parallel using the same system.  We will work with the SPA to provide initial conditions from this parallel to run the NCO parallel during the implementation process. We will compare results from EMC and NCO parallels to ensure they reproduce each other.

* Does this change require a 30-day evaluation?
  	* Yes, the entire GFS.v16 package requires a 30-day evaluation

* Suggested evaluators
  	* Please contact fanglin.yang@noaa.gov, russ.treadon@noaa.gov, and kate.friedman@noaa.gov for evaluation.

-------------------------
DISSEMINATION INFORMATION
-------------------------

* Where should this output be sent?
  	* Please refer to GFSv16 SCN.  Additionally, we have sent a request to NCO Dataflow to start sending new files WAFS_0p25_blended_YYYYMMDDHHfFF.grib2 to AWC only.

* Who are the users? 	
	* same as current operations plus multi_1 users

* Which output files should be transferred from PROD WCOSS to DEV WCOSS?
	* Same as current operational gfs, plus wave products. As there are certain changes in product names and types, EMC will provide support for NCO dataflow team to finalize the list.The amount of data to be transferred also depends on NCO’s network bandwidth.

* Directory changes

	* Add $COMPONENT subfolder to gfs, gdas, and enkf paths for atmospheric component underneath the $cyc folder:

		$COMROOTp3/gfs/prod/gfs.$PDY/$cyc/atmos
		$COMROOTp3/gfs/prod/gdas.$PDY/$cyc/atmos
		$COMROOTp3/gfs/prod/enkf.gdas.$PDY/$cyc/atmos/memXXX

	* Introduce wave model via $COMPONENT subfolder under gfs and gfs $cyc folders:

		$COMROOTp3/gfs/prod/gfs.$PDY/$cyc/wave
		$COMROOTp3/gfs/prod/gdas.$PDY/$cyc/wave

* File changes

	* The UPP(post) file changes can be referred to:
	https://docs.google.com/spreadsheets/d/1I-nqfVO67qE3uHah1p9UNbBPgcStXptEj91MBucSTb4/edit?usp=sharing

------------
HPSS ARCHIVE
------------

Please refer to the following document for current operational GFS.v15 archives and the proposed archives for GFS.v16:
https://docs.google.com/spreadsheets/d/1KkyXa-ZyWCjKul_kijUM4241VBzAerMifMOShLy0crY/edit#gid=0

Please check WCOSS /gpfs/dell1/nco/ops/nwprod/runhistory.v2.3.2/parm/gfs to see a full list of GFS.v15 files archived in HPSS tape.

-------------------------------
JOB DEPENDENCIES & FLOW DIAGRAM
-------------------------------

GDAS and GFS flowchart (downstream jobs compressed):
https://docs.google.com/presentation/d/1grydJSn3LxNishdHOxwOQMyxkLsEzlIfj1PHiTUrAkE/edit#slide=id.g6ee6c85d17_0_0 

===========
Prepared by
Kate.Friedman@noaa.gov
Fanglin.Yang@noaa.gov
Russ.Treadon@noaa.gov
Jun.Wang@noaa.gov
Helin.Wei@noaa.gov
George.Gayno@noaa.gov
Wen.Meng@noaa.gov
Yali.Mao@noaa.gov
Jessica.Meixner@noaa.gov
===========
