"""
GFS cycled application configuration module.

This module defines the configuration for running the Global Forecast System (GFS)
in cycled mode with data assimilation.
"""

from applications.applications import AppConfig
from typing import Dict, Any
from wxflow import Configuration


class GFSCycledAppConfig(AppConfig):
    """
    Class to define GFS cycled configurations.

    This class handles the configuration specific to running GFS in cycled mode
    with data assimilation, including ensemble configurations.

    Parameters
    ----------
    conf : Configuration
        The configuration object containing all settings

    Attributes
    ----------
    runs : list
        List of all available runs (gfs, enkfgfs, gdas, enkfgdas)
    ens_runs : list
        List of runs that include ensemble configurations
    """

    def __init__(self, conf: Configuration):
        super().__init__(conf)
        # Re-read config.base without RUN specified to get the basic settings for
        # cycled cases to be able to determine valid runs
        base = conf.parse_config('config.base')

        self.ens_runs = []

        if base.get('DOHYBVAR', False):
            ens_run = base.get('EUPD_CYC', 'gdas').lower()
            if ens_run in ['both']:
                self.ens_runs = ['gfs', 'gdas']
            elif ens_run in ['gfs', 'gdas']:
                self.ens_runs = [ens_run]

        # Now construct self.runs the desired XML order (gfs, enkfgfs, gdas, enkfgdas)
        self.runs = []
        self.runs.append('gfs') if base['INTERVAL_GFS'] > 0 else 0
        self.runs.append('enkfgfs') if 'gfs' in self.ens_runs and 'gfs' in self.runs else 0
        self.runs.append('gdas')  # We always have a 'gdas' run
        self.runs.append('enkfgdas') if 'gdas' in self.ens_runs else 0

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:
        """
        Get run-specific options for GFS cycled mode.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing run settings

        Returns
        -------
        Dict[str, Any]
            Dictionary containing run options for each configured run
        """
        run_options = super()._get_run_options(conf)

        for run in self.runs:
            base = conf.parse_config('config.base', RUN=run)

            run_options[run]['do_hybvar'] = base.get('DOHYBVAR', False)
            run_options[run]['do_hybvar_ocn'] = base.get('DOHYBVAR_OCN', False)
            run_options[run]['do_letkf_ocn'] = base.get('DOLETKF_OCN', False)
            run_options[run]['nens'] = base.get('NMEM_ENS', 0)
            if run_options[run]['do_hybvar']:
                run_options[run]['lobsdiag_forenkf'] = base.get('lobsdiag_forenkf', False)

            run_options[run]['do_fit2obs'] = base.get('DO_FIT2OBS', True)
            run_options[run]['do_jediatmvar'] = base.get('DO_JEDIATMVAR', False)
            run_options[run]['do_jediatmens'] = base.get('DO_JEDIATMENS', False)
            run_options[run]['do_jediocnvar'] = base.get('DO_JEDIOCNVAR', False)
            run_options[run]['do_jedisnowda'] = base.get('DO_JEDISNOWDA', False)
            run_options[run]['do_gsisoilda'] = base.get('DO_GSISOILDA', False)
            run_options[run]['do_mergensst'] = base.get('DO_MERGENSST', False)

        return run_options

    def _get_app_configs(self, run):
        """
        Returns the config files that are involved in the cycled app.

        Parameters
        ----------
        run : str
            Name of the run configuration to process

        Returns
        -------
        list
            List of configuration file names needed for the specified run
        """
        options = self.run_options[run]

        configs = ['prep']

        if options['do_prep_sfc']:
            configs += ['prep_sfc']

        if options['do_jediatmvar']:
            configs += ['prepatmiodaobs', 'atmanlinit', 'atmanlvar', 'atmanlfv3inc', 'atmanlfinal', 'analcalc_fv3jedi']
        else:
            configs += ['anal', 'analdiag', 'analcalc']

        if options['do_jediocnvar']:
            configs += ['prepoceanobs', 'marineanlinit', 'marinebmat', 'marineanlvar']
            if options['do_letkf_ocn']:
                configs += ['marineanlletkf']
            if options['do_hybvar']:
                configs += ['ocnanalecen']
            configs += ['marineanlchkpt', 'marineanlfinal']

        if options['do_ocean'] or options['do_ice']:
            configs += ['oceanice_products']

        configs += ['stage_ic', 'sfcanl', 'fcst', 'upp', 'atmos_products', 'arch_vrfy', 'cleanup']

        if options['do_archcom']:
            configs += ['arch_tars']

        if options['do_hybvar']:
            if options['do_jediatmens']:
                configs += ['atmensanlinit', 'atmensanlobs', 'atmensanlsol',
                            'atmensanlletkf', 'atmensanlfv3inc', 'atmensanlfinal',
                            'ecen_fv3jedi']
            else:
                configs += ['eobs', 'ediag', 'eupd', 'echgres', 'ecen']

            configs += ['esfc', 'efcs', 'epos', 'earc_vrfy']

            if options['do_archcom']:
                configs += ['earc_tars', 'earc_groups']

        if options['do_fit2obs']:
            configs += ['fit2obs']

        if options['do_verfozn']:
            configs += ['verfozn']

        if options['do_verfrad']:
            configs += ['verfrad']

        if options['do_vminmon']:
            configs += ['vminmon']

        if options['do_anlstat']:
            configs += ['anlstat']

        if options['do_tracker']:
            configs += ['tracker']

        if options['do_genesis']:
            configs += ['genesis']

        if options['do_genesis_fsu']:
            configs += ['genesis_fsu']

        if options['do_metp']:
            configs += ['metp']

        if options['do_gempak']:
            configs += ['gempak']
            if options['do_goes']:
                configs += ['npoess']

        if options['do_bufrsnd']:
            configs += ['postsnd']

        if options['do_awips']:
            configs += ['awips', 'fbwind']

        if options['do_wave']:
            configs += ['waveinit', 'wavepostsbs', 'wavepostpnt']
            if options['do_wave_bnd']:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
            if options['do_gempak']:
                configs += ['wavegempak']
            if options['do_awips']:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if options['do_aero_anl']:
            configs += ['aeroanlgenb', 'aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
            if options['do_prep_obs_aero']:
                configs += ['prepobsaero']

        if options['do_jedisnowda']:
            configs += ['snowanl']
            if options['do_hybvar']:
                configs += ['esnowanl']

        if options['do_globusarch']:
            configs += ['globus']

        return configs

    @staticmethod
    def _update_base(base_in):
        """
        Update base configuration for cycled mode.

        Parameters
        ----------
        base_in : dict
            Input base configuration dictionary

        Returns
        -------
        dict
            Updated base configuration
        """
        return base_in

    def get_task_names(self):
        """
        Get the task names for each valid run in this cycled configuration.

        This method determines which tasks should be run for each configured run.
        The order of the tasks is important for XML configuration generation.

        Returns
        -------
        dict
            Dictionary with run names as keys and ordered lists of task names as values

        Notes
        -----
        The order of the task names matters in the XML generation.
        """
        # Start with a dictionary of empty task lists for each valid run
        task_names = {run: [] for run in self.runs}

        for run in self.runs:
            options = self.run_options[run]

            # Common gdas and gfs tasks before fcst
            if run in ['gdas', 'gfs']:
                task_names[run] += ['prep']
                if options['do_prep_sfc']:
                    task_names[run] += ['prep_sfc']
                if options['do_jediatmvar']:
                    task_names[run] += ['prepatmiodaobs', 'atmanlinit', 'atmanlvar', 'atmanlfv3inc', 'atmanlfinal', 'analcalc_fv3jedi']
                else:
                    task_names[run] += ['anal', 'analcalc']

                if options['do_jediocnvar']:
                    task_names[run] += ['prepoceanobs', 'marineanlinit', 'marinebmat', 'marineanlvar']
                    if options['do_letkf_ocn']:
                        task_names[run] += ['marineanlletkf']
                    if options['do_hybvar']:
                        task_names[run] += ['ocnanalecen']
                    task_names[run] += ['marineanlchkpt', 'marineanlfinal']

                task_names[run] += ['sfcanl']

                if options['do_jedisnowda']:
                    task_names[run] += ['snowanl']

                wave_prep_tasks = ['waveinit']
                wave_bndpnt_tasks = ['wavepostbndpnt', 'wavepostbndpntbll']
                wave_post_tasks = ['wavepostsbs', 'wavepostpnt']

                # gdas- and gfs-specific analysis tasks
                if run == 'gdas':
                    if not options['do_jediatmvar']:
                        task_names[run] += ['analdiag']

                    if options['do_wave']:
                        task_names[run] += wave_prep_tasks

                    if options['do_aero_anl']:
                        task_names[run] += ['aeroanlgenb']

                else:
                    if options['do_wave']:
                        task_names[run] += wave_prep_tasks

                if options['do_aero_anl']:
                    task_names[run] += ['aeroanlinit', 'aeroanlvar', 'aeroanlfinal']

                    if options['do_prep_obs_aero']:
                        task_names[run] += ['prepobsaero']

                # Staging is gdas-specific
                if run == 'gdas':
                    task_names[run] += ['stage_ic']

                task_names[run] += ['atmanlupp', 'atmanlprod', 'fcst']

                # gfs-specific products
                if run == 'gfs':
                    if options['do_ocean']:
                        task_names[run] += ['ocean_prod']

                    if options['do_ice']:
                        task_names[run] += ['ice_prod']

                if options['do_upp']:
                    task_names[run] += ['atmupp']
                task_names[run] += ['atmos_prod']

                # GOES post-processing (gfs only)
                if run == 'gfs':
                    if options['do_goes']:
                        task_names[run] += ['goesupp']

                # Only fit to obs and verify ozone and radiance during gdas cycles
                if run == "gdas":
                    if options['do_fit2obs']:
                        task_names[run] += ['fit2obs']
                    if options['do_verfozn']:
                        task_names[run] += ['verfozn']
                    if options['do_verfrad']:
                        task_names[run] += ['verfrad']

                if options['do_vminmon']:
                    task_names[run] += ['vminmon']

                if options['do_anlstat']:
                    task_names[run] += ['anlstat']

                # gfs-only verification/tracking
                if run == 'gfs':
                    if options['do_tracker']:
                        task_names[run] += ['tracker']

                    if options['do_genesis']:
                        task_names[run] += ['genesis']

                    if options['do_genesis_fsu']:
                        task_names[run] += ['genesis_fsu']

                    if options['do_metp']:
                        task_names[run] += ['metp']

                if options['do_wave']:
                    if options['do_wave_bnd']:
                        task_names[run] += wave_bndpnt_tasks
                    task_names[run] += wave_post_tasks
                    # wave gempak and awips jobs are gfs-specific
                    if run == 'gfs':
                        if options['do_gempak']:
                            task_names[run] += ['wavegempak']
                        if options['do_awips']:
                            task_names[run] += ['waveawipsbulls', 'waveawipsgridded']

                # gdas- and gfs-specific downstream products
                if run == 'gdas':
                    if options['do_gempak']:
                        task_names[run] += ['gempak', 'gempakmetancdc']
                else:
                    if options['do_bufrsnd']:
                        task_names[run] += ['postsnd']

                    if options['do_gempak']:
                        task_names[run] += ['gempak', 'gempakmeta', 'gempakncdcupapgif']
                        if options['do_goes']:
                            task_names[run] += ['npoess_pgrb2_0p5deg', 'gempakpgrb2spec']

                    if options['do_awips']:
                        task_names[run] += ['awips_20km_1p0deg', 'fbwind']

                # Last items
                task_names[run] += ['arch_vrfy']
                if options['do_archcom']:
                    task_names[run] += ['arch_tars']
                    if options['do_globusarch']:
                        task_names[run] += ['globus_arch']

                task_names[run] += ['cleanup']

            # Ensemble tasks
            elif 'enkf' in run:

                task_names[run] += ['stage_ic']
                if options['do_jediatmens']:
                    task_names[run] += ['atmensanlinit', 'atmensanlfv3inc', 'atmensanlfinal', 'ecen_fv3jedi']
                    if options['lobsdiag_forenkf']:
                        task_names[run] += ['atmensanlobs', 'atmensanlsol']
                    else:
                        task_names[run] += ['atmensanlletkf']
                    task_names[run].append('efcs') if 'gdas' in run else 0
                    task_names[run].append('epos') if 'gdas' in run else 0

                else:
                    task_names[run] += ['eobs', 'eupd', 'ecen']
                    task_names[run].append('echgres') if 'gdas' in run else 0
                    task_names[run] += ['ediag']

                task_names[run].append('esnowanl') if options['do_jedisnowda'] else 0
                task_names[run].append('efcs') if 'gdas' in run else 0
                task_names[run].append('epos') if 'gdas' in run else 0

                task_names[run] += ['esfc']
                task_names[run] += ['earc_vrfy']

                if options['do_archcom']:
                    task_names[run] += ['earc_tars']
                    if options['do_globusarch']:
                        task_names[run] += ['globus_earc']

                task_names[run] += ['cleanup']

        return task_names
