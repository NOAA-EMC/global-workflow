#!/usr/bin/env python3
"""
This script modifies the default global-workflow
to set up the GFS workflow for NCO.

This includes:
- Copying relevant files from dev/jobs and dev/scripts
- Renaming files where appropriate
- Changing variables/paths in the copied files
- Removing unused files where appropriate
"""
import os
from wxflow import FileHandler, rmdir
# import Jinja if we add Jinja templated J-Jobs
# from wxflow import Jinja
import argparse

# Get the absolute path of the directory containing this file
current_dir_path = os.path.dirname(os.path.abspath(__file__))
# Get the absolute path of the global-workflow directory
# which is assumed to be two directories up from the current file
global_workflow_dir = os.path.abspath(os.path.join(current_dir_path, "../.."))

# Query the list of global J-Jobs in dev/jobs and compare against the J-Jobs called in
# dev/job_cards/rocoto and the commands in dev/workflow/rocoto/gfs_tasks.py to determine
# which J-Jobs to copy to jobs/. Then look for the associated ex-scripts called in the
# J-Jobs and copy those to scripts/ with appropriate renaming.
base_job_list = [
    "JGLOBAL_ANALYSIS_STATS",
    "JGLOBAL_ARCHIVE_TARS",
    "JGLOBAL_ARCHIVE_VRFY",
    "JGLOBAL_ATMOS_ANAL",
    "JGLOBAL_ATMOS_ANAL_CALC",
    "JGLOBAL_ATMOS_ANAL_DIAG",
    "JGLOBAL_ATMOS_PREP_SFC",
    "JGLOBAL_ATMOS_PRODUCTS",
    "JGLOBAL_ATMOS_ANAL_SFC_GCYCLE",
    "JGLOBAL_ATMOS_ANAL_SFC_REGRID",
    "JGLOBAL_ATMOS_TROPCY_QC",
    "JGLOBAL_ATMOS_UPP",
    "JGLOBAL_ATMOS_VMINMON",
    "JGLOBAL_CLEANUP",
    "JGLOBAL_FETCH",
    "JGLOBAL_FCST_FSM",
    "JGLOBAL_FCST",
    "JGLOBAL_FCST_MANAGER",
    "JGLOBAL_MARINE_ANAL_CHKPT",
    "JGLOBAL_MARINE_ENS_RECENTER",
    "JGLOBAL_MARINE_ANAL_FINAL",
    "JGLOBAL_MARINE_ANAL_INIT",
    "JGLOBAL_MARINE_ANAL_VAR",
    "JGLOBAL_MARINE_BMAT",
    "JGLOBAL_MARINE_BMAT_INIT",
    "JGLOBAL_OCEANICE_PRODUCTS",
    "JGLOBAL_MARINE_OBS_DUMP",
    "JGLOBAL_MARINE_OBS_BUFR_DUMP",
    "JGLOBAL_SNOW_ENS_ANAL",
    "JGLOBAL_SNOW_ANAL",
    "JGFS_WAVE_GEMPAK",
    "JGLOBAL_WAVE_INIT",
    "JGLOBAL_WAVE_POST_BNDPNT",
    "JGLOBAL_WAVE_POST_BNDPNT_BULLS",
    "JGLOBAL_WAVE_POST_PNT",
    "JGLOBAL_WAVE_POST_GRIDDED",
    "JGFS_WAVE_AWIPS_BULLS",
    "JGFS_WAVE_AWIPS_GRIDDED",
    "JGDAS_ATMOS_ANAL_WDQMS",
    "JENKFGDAS_ATMOS_ENS_CHANGE_RES",
    "JGDAS_ATMOS_GEMPAK",
    "JGDAS_ATMOS_GEMPAK_META_NCDC",
    "JGDAS_ATMOS_VERFOZN",
    "JGDAS_ATMOS_VERFRAD",
    "JENKFGDAS_ENS_POST",
    "JGDAS_FIT2OBS",
    "JGFS_ATMOS_AWIPS_20KM_1P0",
    "JGFS_ATMOS_CYCLONE_GENESIS",
    "JGFS_ATMOS_CYCLONE_TRACKER",
    "JGFS_ATMOS_FBWIND",
    "JGFS_ATMOS_FSU_GENESIS",
    "JGFS_ATMOS_GEMPAK",
    "JGFS_ATMOS_GEMPAK_META",
    "JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF",
    "JGFS_ATMOS_GEMPAK_PGRB2_SPEC",
    "JGFS_ATMOS_GEMPAK_PGRB2_SPEC_NPOESS",
    "JGFS_ATMOS_POST_SND",
    "JGFS_ATMOS_VERIFICATION",
    "JGLOBAL_ENKF_ARCHIVE_TARS",
    "JGLOBAL_ENKF_ARCHIVE_VRFY",
    "JGLOBAL_ATMOS_ENS_DIAG",
    "JGLOBAL_ATMOS_ENS_RECENTER",
    "JGLOBAL_ATMOS_ENS_OBSERVER",
    "JGLOBAL_ATMOS_ENS_ANAL_SFC_GCYCLE",
    "JGLOBAL_ATMOS_ENS_ANAL_SFC_REGRID",
    "JGLOBAL_ATMOS_ENS_UPDATE",
]

base_script_list = [
    "exgdas_atmos_anal_wdqms.sh",
    "exenkfgdas_atmos_ens_change_res.sh",
    "exgdas_atmos_gempak_gif_ncdc.sh",
    "exgdas_atmos_gempak.sh",
    "exgdas_atmos_verfozn.sh",
    "exgdas_atmos_verfrad.sh",
    "exenkfgdas_ens_post.sh",
    "exgfs_atmos_awips_20km_1p0.sh",
    "exgfs_atmos_fbwind.sh",
    "exgfs_atmos_gempak_gif_ncdc_skew_t.sh",
    "exgfs_atmos_gempak_meta.sh",
    "exgfs_atmos_gempak_pgrb2_spec.sh",
    "exgfs_atmos_gempak_pgrb2_spec_npoess.sh",
    "exgfs_atmos_gempak.sh",
    "exgfs_atmos_post_snd.sh",
    "exglobal_wave_init.sh",
    "exgfs_wave_gempak.sh",
    "exglobal_wave_post_gridded.sh",
    "exglobal_wave_post_pnt.sh",
    "exgfs_wave_awips_bulls.sh",
    "exgfs_wave_awips_gridded.sh",
    "exglobal_analysis_stats.py",
    "exglobal_archive_tars.py",
    "exglobal_archive_vrfy.py",
    "exglobal_atmos_anal.sh",
    "exglobal_atmos_anal_calc.sh",
    "exglobal_atmos_products.sh",
    "exglobal_atmos_anal_sfc_gcycle.sh",
    "exglobal_atmos_anal_sfc_regrid.sh",
    "exglobal_atmos_tropcy_qc.sh",
    "exglobal_atmos_upp.py",
    "exglobal_atmos_vminmon.sh",
    "exglobal_cleanup.sh",
    "exglobal_atmos_diag.sh",
    "exglobal_fetch.py",
    "exglobal_fcst_fsm.sh",
    "exglobal_fcst.sh",
    "exglobal_fcst_manager.sh",
    "exglobal_marine_anal_chkpt.py",
    "exglobal_marine_ens_recenter.py",
    "exglobal_marine_anal_final.py",
    "exglobal_marine_anal_init.py",
    "exglobal_marine_anal_var.py",
    "exglobal_marine_bmat.py",
    "exglobal_marine_bmat_init.py",
    "exglobal_oceanice_products.py",
    "exglobal_marine_obs_dump.py",
    "exglobal_marine_obs_bufr_dump.py",
    "exglobal_prep_sfc.sh",
    "exglobal_snow_anal.py",
    "exglobal_snow_ens_anal.py",
    "exglobal_enkf_earc_tars.py",
    "exglobal_enkf_earc_vrfy.py",
    "exglobal_atmos_ens_recenter.sh",
    "exglobal_atmos_ens_observer.sh",
    "exglobal_atmos_ens_anal_sfc_gcycle.sh",
    "exglobal_atmos_ens_anal_sfc_regrid.sh",
    "exglobal_atmos_ens_update.sh",
]

# If needed, add scripts and jobs that need to be renamed when copied.

rename_script_list = {}
rename_job_list = {}


def copy_files(global_workflow_dir, copy_list=[], rename_dict={}, link_or_copy='copy', file_type='job'):
    """
    Copy or link job files from dev/jobs to jobs directory with optional renaming.
    Parameters
    ----------
    global_workflow_dir : str
        Path to the global workflow directory
    copy_list : list, optional
        List of job files to copy (default is [], which means no files will be copied)
    rename_dict : dict, optional
        Dictionary mapping source job file names to destination job file names for renaming (default is {}, which means no files will be renamed)
    link_or_copy : str, optional
        Whether to 'copy' or 'link' the files (default is 'copy')
    file_type : str, optional
        Type of files being handled, either 'job' or 'script' (default is 'job'). This determines the source and destination directories for the files.
    Note: At least one of copy_list or rename_dict must be provided.

    Returns
    -------
    list
        List of tuples containing (src_path, dest_path) for copied files
    """

    if not copy_list and not rename_dict:
        raise ValueError("At least one of copy_list or rename_dict must be provided.")

    if link_or_copy not in ['copy', 'link']:
        raise ValueError(f"link_or_copy must be either 'copy' or 'link', not '{link_or_copy}'.")

    if file_type not in ['job', 'script']:
        raise ValueError(f"file_type must be either 'job' or 'script', not '{file_type}'.")

    if not os.path.exists(global_workflow_dir):
        raise FileNotFoundError(f"Global workflow directory not found: {global_workflow_dir}")

    if file_type == 'job':
        source_dir = os.path.join(global_workflow_dir, 'dev', 'jobs')
        dest_dir = os.path.join(global_workflow_dir, 'jobs')
    else:  # file_type == 'script'
        source_dir = os.path.join(global_workflow_dir, 'dev', 'scripts')
        dest_dir = os.path.join(global_workflow_dir, 'scripts')
        if os.path.islink(dest_dir):
            os.unlink(dest_dir)

    if not os.path.exists(source_dir):
        raise FileNotFoundError(f"Source directory not found: {source_dir}")

    job_file_copy_list = []
    for file in copy_list:
        if file in rename_dict:
            raise ValueError(f"File '{file}' cannot be in both copy_list and rename_dict.")
        src_path = os.path.join(source_dir, file)
        if not os.path.exists(src_path):
            raise FileNotFoundError(f"Source job file not found: {src_path}")
        dest_path = os.path.join(dest_dir, file)
        job_file_copy_list.append((src_path, dest_path))

    for dest_file, src_file in rename_dict.items():
        src_path = os.path.join(source_dir, src_file)
        if not os.path.exists(src_path):
            raise FileNotFoundError(f"Source job file not found: {src_path}")
        dest_path = os.path.join(dest_dir, dest_file)
        job_file_copy_list.append((src_path, dest_path))

    # Create a FileHandler dictionary
    rmdir(dest_dir, missing_ok=True)
    job_file_handler = {
        'mkdir': [dest_dir],
        link_or_copy: job_file_copy_list,
    }
    # Execute the file operations
    FileHandler(job_file_handler).sync()

    return job_file_copy_list


def remove_unused_executables(global_workflow_dir):
    """
    Remove unused executables from the exec directory.

    Parameters
    ----------
    global_workflow_dir : str
        Path to the global workflow directory

    Returns
    -------
    list
        List of files that were successfully removed
    """
    # TODO Expand this list to include all GFS executables
    desired_executables = [
        "calc_analysis.x",
        "calc_increment_ens_ncio.x",
        "enkf_chgres_recenter_nc.x",
        "gaussian_sfcanl.x",
        "gfs_model.x",
        "gdas_fv3jedi_error_covariance_toolbox.x",
        "gdas_ioda-stats.x",
        "gdas_obsprovider2ioda.x",
        "gdas.x",
        "global_cycle",
        "interp_inc.x",
        "nexus.x",
        "tref_calc.x",
        "upp.x"
    ]

    exec_dir = os.path.join(global_workflow_dir, 'exec')
    removed_files = []

    # Get all files in exec_dir
    if os.path.exists(exec_dir):
        all_files = [f for f in os.listdir(exec_dir) if os.path.isfile(os.path.join(exec_dir, f))]

        # Remove all files except those in desired_executables
        for filename in all_files:
            if filename not in desired_executables:
                file_path = os.path.join(exec_dir, filename)
                try:
                    os.remove(file_path)
                    removed_files.append(filename)
                    print(f"Removed unused executable: {filename}")
                except OSError as e:
                    print(f"Error removing {filename}: {e}")
    else:
        print(f"Exec directory not found: {exec_dir}")

    return removed_files


def setup_gfs_for_nco(link_or_copy='copy'):
    if link_or_copy not in ['copy', 'link']:
        raise ValueError(f"link_or_copy must be either 'copy' or 'link', not '{link_or_copy}'.")

    # first, copy jobs from dev to the global workflow directory
    job_file_copy_list = copy_files(global_workflow_dir, copy_list=base_job_list, rename_dict=rename_job_list, link_or_copy=link_or_copy, file_type='job')

    # Next, copy ex-scripts from dev/scripts to the global workflow directory
    ex_script_file_copy_list = copy_files(
        global_workflow_dir, copy_list=base_script_list,
        rename_dict=rename_script_list,
        link_or_copy=link_or_copy,
        file_type='script')

    # Remove unused executables from the exec directory
    # TODO Call this when the full list of executubles to keep is known.
    # removed_files = remove_unused_executables(global_workflow_dir)

    # Go through the copied job files and replace the scripts they call as appropriate
    jobs_dir = os.path.join(global_workflow_dir, 'jobs')
    for job_name in list(rename_job_list.keys()):
        job_path = os.path.join(jobs_dir, job_name)
        if os.path.exists(job_path):
            with open(job_path, 'r') as f:
                job_contents = f.read()
            for script_name in list(rename_script_list.keys()):
                job_contents = job_contents.replace(rename_script_list[script_name], script_name)
            with open(job_path, 'w') as f:
                f.write(job_contents)
        else:
            print(f"Warning: Job file not found for script replacement: {job_path}")


if __name__ == "__main__":
    # Get command line argument for whether to copy or link files, default to 'copy'
    parser = argparse.ArgumentParser(description="Set up GFS workflow for NCO by linking or copying necessary files from dev to the global workflow directory.")
    parser.add_argument('--copy', action='store_true', )
    args = parser.parse_args()
    link_or_copy = 'copy' if args.copy else 'link'
    setup_gfs_for_nco(link_or_copy=link_or_copy)
