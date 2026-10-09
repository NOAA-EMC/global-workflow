# Slurm-compatible environment header for Ursa
# Replaces envir-p1.h (PBS_JOBNAME/PBS_JOBID → Slurm equivalents)
export job=${job:-${SLURM_JOB_NAME:-$(basename "${ECF_JOB:-.}")}}
export jobid=${jobid:-${job}.${SLURM_JOB_ID:-$$}}

export RUN_ENVIR=${RUN_ENVIR:-emc}
export envir=%ENVIR%
export RUN=%RUN%
