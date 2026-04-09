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
from wxflow import FileHandler, Jinja

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
    "JGLOBAL_ATMOS_ANALYSIS",
    "JGLOBAL_ATMOS_ANALYSIS_CALC",
    "JGLOBAL_ATMOS_ANALYSIS_DIAG",
    "JGLOBAL_ATMOS_PREP_SFC",
    "JGLOBAL_ATMOS_PRODUCTS",
    "JGLOBAL_ATMOS_SFCANL",
    "JGLOBAL_ATMOS_TROPCY_QC_RELOC",
    "JGLOBAL_ATMOS_UPP",
    "JGLOBAL_ATMOS_VMINMON",
    "JGLOBAL_CLEANUP",
    "JGLOBAL_FETCH",
    "JGLOBAL_FORECAST",
    "JGLOBAL_MARINE_ANALYSIS_CHECKPOINT",
    "JGLOBAL_MARINE_ANALYSIS_ECEN",
    "JGLOBAL_MARINE_ANALYSIS_FINALIZE",
    "JGLOBAL_MARINE_ANALYSIS_INITIALIZE",
    "JGLOBAL_MARINE_ANALYSIS_LETKF",
    "JGLOBAL_MARINE_ANALYSIS_VARIATIONAL",
    "JGLOBAL_MARINE_BMAT",
    "JGLOBAL_MARINE_BMAT_INITIALIZE",
    "JGLOBAL_OCEANICE_PRODUCTS",
    "JGLOBAL_PREP_OCEAN_OBS",
    "JGLOBAL_SNOWENS_ANALYSIS",
    "JGLOBAL_SNOW_ANALYSIS",
    "JGLOBAL_WAVE_GEMPAK",
    "JGLOBAL_WAVE_INIT",
    "JGLOBAL_WAVE_POST_BNDPNT",
    "JGLOBAL_WAVE_POST_BNDPNTBLL",
    "JGLOBAL_WAVE_POST_PNT",
    "JGLOBAL_WAVE_POST_SBS",
    "JGLOBAL_WAVE_PRDGEN_BULLS",
    "JGLOBAL_WAVE_PRDGEN_GRIDDED",
    "JGLOBAL_WAVE_PREP",
    "JGDAS_ATMOS_CHGRES_FORENKF",
    "JGDAS_ATMOS_GEMPAK",
    "JGDAS_ATMOS_GEMPAK_META_NCDC",
    "JGDAS_ATMOS_VERFOZN",
    "JGDAS_ATMOS_VERFRAD",
    "JGDAS_ENKF_POST",
    "JGDAS_FIT2OBS",
    "JGFS_ATMOS_AWIPS_20KM_1P0DEG",
    "JGFS_ATMOS_CYCLONE_GENESIS",
    "JGFS_ATMOS_CYCLONE_TRACKER",
    "JGFS_ATMOS_FBWIND",
    "JGFS_ATMOS_FSU_GENESIS",
    "JGFS_ATMOS_GEMPAK",
    "JGFS_ATMOS_GEMPAK_META",
    "JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF",
    "JGFS_ATMOS_GEMPAK_PGRB2_SPEC",
    "JGFS_ATMOS_PGRB2_SPEC_NPOESS",
    "JGFS_ATMOS_POSTSND",
    "JGFS_ATMOS_VERIFICATION",
]

rename_job_list = {
    "JGDAS_ENKF_ARCHIVE_TARS" : "JGLOBAL_ENKF_ARCHIVE_TARS",
    "JGDAS_ENKF_ARCHIVE_VRFY" : "JGLOBAL_ENKF_ARCHIVE_VRFY",
    "JGDAS_ENKF_DIAG" : "JGLOBAL_ENKF_DIAG",
    "JGDAS_ENKF_ECEN" : "JGLOBAL_ENKF_ECEN",
    "JGDAS_ENKF_ECEN_FV3JEDI" : "JGLOBAL_ENKF_ECEN_FV3JEDI",
    "JGDAS_ENKF_SELECT_OBS" : "JGLOBAL_ENKF_SELECT_OBS",
    "JGDAS_ENKF_SFC" : "JGLOBAL_ENKF_SFC",
    "JGDAS_ENKF_UPDATE" : "JGLOBAL_ENKF_UPDATE",
}

base_script_list = [
    "exgdas_atmos_chgres_forenkf.sh",
    "exgdas_atmos_gempak_gif_ncdc.sh",
    "exgdas_atmos_nawips.sh",
    "exgdas_atmos_verfozn.sh",
    "exgdas_atmos_verfrad.sh",
    "exgdas_enkf_post.sh",
    "exgfs_atmos_awips_20km_1p0deg.sh",
    "exgfs_atmos_fbwind.sh",
    "exgfs_atmos_gempak_gif_ncdc_skew_t.sh",
    "exgfs_atmos_gempak_meta.sh",
    "exgfs_atmos_goes_nawips.sh",
    "exgfs_atmos_grib2_special_npoess.sh",
    "exgfs_atmos_nawips.sh",
    "exgfs_atmos_postsnd.sh",
    "exgfs_wave_init.sh",
    "exgfs_wave_nawips.sh",
    "exgfs_wave_post_gridded_sbs.sh",
    "exgfs_wave_post_pnt.sh",
    "exgfs_wave_prdgen_bulls.sh",
    "exgfs_wave_prdgen_gridded.sh",
    "exgfs_wave_prep.sh",
    "exglobal_analysis_stats.py",
    "exglobal_archive_tars.py",
    "exglobal_archive_vrfy.py",
    "exglobal_atmos_analysis.sh",
    "exglobal_atmos_analysis_calc.sh",
    "exglobal_atmos_products.sh",
    "exglobal_atmos_sfcanl.sh",
    "exglobal_atmos_tropcy_qc_reloc.sh",
    "exglobal_atmos_upp.py",
    "exglobal_atmos_vminmon.sh",
    "exglobal_cleanup.sh",
    "exglobal_diag.sh",
    "exglobal_fetch.py",
    "exglobal_forecast.sh",
    "exglobal_marine_analysis_checkpoint.py",
    "exglobal_marine_analysis_finalize.py",
    "exglobal_marine_analysis_initialize.py",
    "exglobal_marine_analysis_variational.py",
    "exglobal_marinebmat.py",
    "exglobal_marinebmat_initialize.py",
    "exglobal_oceanice_products.py",
    "exglobal_prep_ocean_obs.py",
    "exglobal_prep_sfc.sh",
    "exglobal_snow_analysis.py",
    "exglobal_snowens_analysis.py",
]

rename_script_list = {
    "exgdas_enkf_earc_tars.py" : "exglobal_enkf_earc_tars.py"
    "exgdas_enkf_earc_vrfy.py" : "exglobal_enkf_earc_vrfy.py"
    "exgdas_enkf_ecen.sh" : "exglobal_enkf_ecen.sh"
    "exgdas_enkf_ecen_fv3jedi.py" : "exglobal_enkf_ecen_fv3jedi.py"
    "exgdas_enkf_select_obs.sh" : "exglobal_enkf_select_obs.sh"
    "exgdas_enkf_sfc.sh" : "exglobal_enkf_sfc.sh"
    "exgdas_enkf_update.sh" : "exglobal_enkf_update.sh"
}

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
        dest_subdir = os.path.join(global_workflow_dir, 'scripts')
        if os.path.islink(dest_subdir):
            os.unlink(scripts_dir)

    if not os.path.exists(source_dir):
        raise FileNotFoundError(f"Source directory not found: {source_dir}")

    job_file_copy_list = []
    for file in copy_list:
        if file in rename_dict:
            raise ValueError(f"File '{file}' cannot be in both copy_list and rename_dict.")
        src_job_path = os.path.join(global_workflow_dir, 'dev', 'jobs', file)
        if not os.path.exists(src_job_path):
            raise FileNotFoundError(f"Source job file not found: {src_job_path}")
        dest_job_path = os.path.join(global_workflow_dir, 'jobs', file)
        job_file_copy_list.append((src_job_path, dest_job_path))

    for dest_job, src_job in rename_dict.items():
        src_job_path = os.path.join(global_workflow_dir, 'dev', 'jobs', src_job)
        if not os.path.exists(src_job_path):
            raise FileNotFoundError(f"Source job file not found: {src_job_path}")
        dest_job_path = os.path.join(global_workflow_dir, 'jobs', dest_job)
        job_file_copy_list.append((src_job_path, dest_job_path))

    # Create a FileHandler dictionary
    job_file_handler = {
        'mkdir': [os.path.join(global_workflow_dir, 'jobs')],
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
    desired_executables = [
        "calc_analysis.x",
        "calc_increment_ens_ncio.x",
        "enkf_chgres_recenter_nc.x",
        "gaussian_sfcanl.x",
        "gfs_model.x",
        "gdas_fv3jedi_chem_diagb.x",
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
    ex_script_file_copy_list = copy_script_files(global_workflow_dir, copy_list=base_script_list, rename_dict=rename_script_list, link_or_copy=link_or_copy, file_type='script')

    # Remove unused executables from the exec directory
    removed_files = remove_unused_executables(global_workflow_dir)

    # Go through the copied job files and replace the scripts they call as appropriate
    jobs_dir = os.path.join(global_workflow_dir, 'jobs')
    for job_name in list(gfs_jobs.keys()) + list(gcdas_jobs.keys()):

    # Render the JGLOBAL_FORECAST.j2 template for both GCDAS and GCAFS
    template_path = os.path.join(global_workflow_dir, 'dev', 'jobs', 'JGLOBAL_FORECAST.j2')
    for RUN in ['gcafs', 'gcdas']:
        dest_job_path = os.path.join(global_workflow_dir, 'jobs', f"J{RUN.upper()}_FORECAST")
        Jinja(template_path, {'RUN': RUN}).save(dest_job_path)
        print(f"Rendered template for {RUN.upper()} and saved to {dest_job_path}")
        os.chmod(dest_job_path, 0o755)

    # Render the surface analysis template
    surface_template_path = os.path.join(global_workflow_dir, 'dev', 'jobs', 'JGLOBAL_ATMOS_SFCANL.j2')
    dest_surface_job_path = os.path.join(global_workflow_dir, 'jobs', "JGCDAS_SURFACE_INITIALIZE")
    Jinja(surface_template_path, {'RUN': 'gcdas'}).save(dest_surface_job_path)
    print(f"Rendered surface analysis template and saved to {dest_surface_job_path}")
    os.chmod(dest_surface_job_path, 0o755)

    # Render the offline atmospheric analysis template
    offline_atmos_template_path = os.path.join(global_workflow_dir, 'dev', 'jobs', 'JGLOBAL_OFFLINE_ATMOS_ANALYSIS.j2')
    dest_offline_atmos_job_path = os.path.join(global_workflow_dir, 'jobs', "JGCDAS_ATMOS_INITIALIZE")
    Jinja(offline_atmos_template_path, {'RUN': 'gcdas'}).save(dest_offline_atmos_job_path)
    print(f"Rendered offline atmospheric analysis template and saved to {dest_offline_atmos_job_path}")
    os.chmod(dest_offline_atmos_job_path, 0o755)

    # Now for all jobs, we need a line that exports HOMEglobal
    for job_name in os.listdir(jobs_dir):
        job_file_path = os.path.join(jobs_dir, job_name)
        if not os.path.isfile(job_file_path):
            continue
        with open(job_file_path, 'r') as f:
            lines = f.readlines()
        if lines and lines[0].startswith('#!'):
            export_line = 'export HOMEglobal="${HOMEgcafs}"\n'
            if len(lines) < 2 or lines[1] != export_line:
                lines.insert(1, export_line)
                with open(job_file_path, 'w') as f:
                    f.writelines(lines)
                print(f"Added HOMEglobal export to {job_name}")


if __name__ == "__main__":
    setup_gcafs_for_nco()
