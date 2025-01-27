#!/usr/bin/env python3

import os

from pygfs.task.archive import Archive
from wxflow import AttrDict, Logger, cast_strdict_as_dtypedict, logit, chdir

# initialize root logger
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=True)


@logit(logger)
def main():

    config = cast_strdict_as_dtypedict(os.environ)

    # Instantiate the Archive object
    archive = Archive(config)

    # update these keys to be 3 digits if they are part of archive.task_config.keys
    for key in ['OCNRES', 'ICERES']:
        try:
            archive.task_config[key] = f"{archive.task_config[key]:03d}"
        except KeyError as ee:
            logger.info(f"key ({key}) not found in archive.task_config!")

    # Pull out all the configuration keys needed to run the rest of archive steps
    keys = ['ATARDIR', 'current_cycle', 'FHMIN', 'FHMAX', 'FHOUT', 'RUN', 'PDY',
            'DO_VERFRAD', 'DO_VMINMON', 'DO_VERFOZN', 'DO_ICE', 'DO_PREP_OBS_AERO',
            'PARMgfs', 'DO_OCN', 'DO_WAVE', 'WRITE_DOPOST', 'PSLOT', 'HPSSARCH', 'DO_MOS',
            'DO_JEDISNOWDA', 'LOCALARCH', 'REALTIME', 'ROTDIR', 'ARCH_WARMICFREQ',
            'ARCH_FCSTICFREQ', 'ARCH_CYC', 'assim_freq', 'ARCDIR', 'SDATE',
            'FHMIN_GFS', 'FHMAX_GFS', 'FHOUT_GFS', 'ARCH_GAUSSIAN', 'MODE',
            'FHOUT_OCN', 'FHOUT_ICE', 'FHOUT_OCN_GFS', 'FHOUT_ICE_GFS', 'DO_BUFRSND', 'DOHYBVAR',
            'ARCH_GAUSSIAN_FHMAX', 'ARCH_GAUSSIAN_FHINC', 'ARCH_GAUSSIAN_FHINC',
            'DOIAU', 'OCNRES', 'ICERES', 'NUM_SND_COLLECTIVES', 'FHOUT_WAV',
            'FHOUT_HF_WAV', 'FHMAX_WAV', 'FHMAX_HF_WAV', 'FHMAX_WAV_GFS',
            'restart_interval_gdas', 'restart_interval_gfs',
            'DO_AERO_ANL', 'DO_AERO_FCST', 'DO_CA', 'DOIBP_WAV', 'DO_JEDIOCNVAR', 'DOHYBVAR_OCN',
            'NMEM_ENS', 'DO_JEDIATMVAR', 'DO_VRFY_OCEANDA', 'FHMAX_FITS', 'waveGRD',
            'IAUFHRS', 'DO_FIT2OBS', 'NET', 'FHOUT_HF_GFS', 'FHMAX_HF_GFS', 'REPLAY_ICS',
            'OFFSET_START_HOUR', 'ARCH_EXPDIR', 'EXPDIR', 'ARCH_EXPDIR_FREQ', 'ARCH_HASHES',
            'ARCH_DIFFS', 'SDATE', 'EDATE', 'HOMEgfs', 'DO_GEMPAK', 'WAVE_OUT_GRIDS']

    archive_dict = AttrDict()
    for key in keys:
        try:
            archive_dict[key] = archive.task_config[key]
        except KeyError as ee:
            logger.warning(f"WARNING: key ({key}) not found in archive.task_config!")

    # Also import all COMIN* and COMOUT* directory and template variables
    for key in archive.task_config.keys():
        if key.startswith(("COM_", "COMIN_", "COMOUT_")):
            archive_dict[key] = archive.task_config.get(key)

    with chdir(config.ROTDIR):

        # Determine which archives to create
        arcdir_set, atardir_sets = archive.configure(archive_dict)

        # Populate the product archive (ARCDIR)
        archive.execute_store_products(arcdir_set)

        # Create the backup tarballs and store in ATARDIR
        for atardir_set in atardir_sets:
            archive.execute_backup_dataset(atardir_set)

        # Clean up any temporary files
        archive.clean()


if __name__ == '__main__':
    main()
