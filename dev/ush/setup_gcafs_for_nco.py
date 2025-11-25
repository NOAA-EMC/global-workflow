#!/usr/bin/env python3
"""
This script modifies the default global-workflow
to set up the GCAFS workflow for NCO.

This includes:
- Copying relevant files from dev/jobs and dev/scripts
- Renaming files where appropriate
- Changing variables/paths in the copied files
- Removing unused files where appropriate
"""
import os
from wxflow import FileHandler

# Get the absolute path of the directory containing this file
current_dir_path = os.path.dirname(os.path.abspath(__file__))
# Get the absolute path of the global-workflow directory
# which is assumed to be two directories up from the current file
global_workflow_dir = os.path.abspath(os.path.join(current_dir_path, "../.."))


def replace_gfs_with_gcafs(input_file):
    """
    Replace all instances of FOOgfs with FOOgcafs in the given input file.
    This matches patterns like HOMEgfs -> HOMEgcafs, USHgfs -> USHgcafs, etc.
    
    Parameters
    ----------
    input_file : str
        Path to the file to modify
    
    Returns
    -------
    int
        Number of replacements made
    """
    if not os.path.exists(input_file):
        raise FileNotFoundError(f"File not found: {input_file}")
    
    # Read the file content
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Count and replace all instances of FOOgfs with FOOgcafs
    # This will match patterns like: HOMEgfs, USHgfs, PARMgfs, etc.
    # Does NOT match standalone "gfs" or quoted "gfs"
    import re
    # Match word characters followed by "gfs" at word boundary, but ensure prefix has at least 2 chars
    # This ensures we match variable names like HOMEgfs but not just "gfs" or "Xgfs"
    pattern = r'(\w{2,})gfs\b'
    
    replacement_count = 0
    def replace_func(match):
        nonlocal replacement_count
        replacement_count += 1
        prefix = match.group(1)
        return f"{prefix}gcafs"
    
    modified_content = re.sub(pattern, replace_func, content)
    
    # Write the modified content back to the file
    with open(input_file, 'w') as f:
        f.write(modified_content)
    
    return replacement_count


def setup_gcafs_for_nco():
    # first, copy jobs from dev to the global workflow directory
    gcafs_jobs = {
        "JGCAFS_FORECAST": "JGLOBAL_FORECAST",
        "JGCAFS_PREP_EMISSIONS": "JGLOBAL_PREP_EMISSIONS",
        "JGCAFS_ATMOS_POST_MANAGER": "JGLOBAL_ATMOS_POST_MANAGER",
        "JGCAFS_ATMOS_PRODUCTS": "JGLOBAL_ATMOS_PRODUCTS",
    }
    gcdas_jobs = {
        "JGCDAS_FORECAST": "JGLOBAL_FORECAST",
        "JGCDAS_PREP_EMISSIONS": "JGLOBAL_PREP_EMISSIONS",
        "JGCDAS_ATMOS_POST_MANAGER": "JGLOBAL_ATMOS_POST_MANAGER",
        "JGCDAS_ATMOS_PRODUCTS": "JGLOBAL_ATMOS_PRODUCTS",
        "JGCDAS_ATMOS_INITIALIZE": "JGLOBAL_OFFLINE_ATMOS_ANALYSIS",
        "JGCDAS_SURFACE_INITIALIZE": "JGLOBAL_ATMOS_SFCANL",
        "JGCDAS_AERO_ANALYSIS_INITIALIZE": "JGLOBAL_AERO_ANALYSIS_INITIALIZE",
        "JGCDAS_AERO_ANALYSIS_VARIATIONAL": "JGLOBAL_AERO_ANALYSIS_VARIATIONAL",
        "JGCDAS_AERO_ANALYSIS_FINALIZE": "JGLOBAL_AERO_ANALYSIS_FINALIZE",
        "JGCDAS_AERO_ANALYSIS_CALC": "JGLOBAL_ATMOS_ANALYSIS_CALC",
        "JGCDAS_AERO_ANALYSIS_STATS": "JGLOBAL_ANALYSIS_STATS",
        "JGCDAS_AERO_ANALYSIS_GENERATE_BMATRIX": "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        # JGCDAS_PREPARE_OBS is taken from ObsForge for v1, not in global-workflow, do this manually!!
        # need to add something here for the post job once Yaping's PR is in
    }

    # Next, copy ex-scripts from dev/scripts to the global workflow directory
    gcafs_ex_scripts = {
        "exgcafs_forecast.sh": "exglobal_forecast.sh",
        "exgcafs_prep_emissions.sh": "exglobal_prep_emissions.sh",
        "exgcafs_atmos_post_manager.sh": "exglobal_atmos_pmgr.sh",
        "exgcafs_atmos_products.sh": "exglobal_atmos_products.sh",
    }
    gcdas_ex_scripts = {
        "exgcdas_forecast.sh": "exglobal_forecast.sh",
        "exgcdas_prep_emissions.sh": "exglobal_prep_emissions.sh",
        "exgcdas_atmos_post_manager.sh": "exglobal_atmos_pmgr.sh",
        "exgcdas_atmos_products.sh": "exglobal_atmos_products.sh",
        "exgcdas_atmos_initialize.py": "exglobal_offline_atmos_analysis.py",
        "exgcdas_surface_initialize.sh": "exglobal_atmos_sfcanl.sh",
        "exgcdas_aero_analysis_initialize.py": "exglobal_aero_analysis_initialize.py",
        "exgcdas_aero_analysis_variational.py": "exglobal_aero_analysis_variational.py",
        "exgcdas_aero_analysis_finalize.py": "exglobal_aero_analysis_finalize.py",
        "exgcdas_aero_analysis_calc.sh": "exglobal_atmos_analysis_calc.sh",
        "exgcdas_aero_analysis_stats.py": "exglobal_analysis_stats.py",
        "exgcdas_aero_analysis_generate_bmatrix.py": "exglobal_aero_analysis_generate_bmatrix.py",
        # exgcdas_prepare_obs is taken from ObsForge for v1, not in global-workflow, do this manually!!
        # need to add something here for the post job once Yaping's PR is in
    }

if __name__ == "__main__":
    setup_gcafs_for_nco()
