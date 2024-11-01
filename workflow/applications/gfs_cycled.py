from typing import Dict, Any
from applications.applications import AppConfig
from wxflow import Configuration, to_timedelta
from datetime import timedelta


class GFSCycledAppConfig(AppConfig):
    '''
    Class to define GFS cycled configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)
        base = conf.parse_config('config.base')
        self.do_hybvar = base.get('DOHYBVAR', False)
        self.do_fit2obs = base.get('DO_FIT2OBS', True)
        self.do_jediatmvar = base.get('DO_JEDIATMVAR', False)
        self.do_jediatmens = base.get('DO_JEDIATMENS', False)
        self.do_jediocnvar = base.get('DO_JEDIOCNVAR', False)
        self.do_jedisnowda = base.get('DO_JEDISNOWDA', False)
        self.do_mergensst = base.get('DO_MERGENSST', False)
        self.do_vrfy_oceanda = base.get('DO_VRFY_OCEANDA', False)

        self.lobsdiag_forenkf = False
        self.eupd_runs = None
        if self.do_hybvar:
            self.lobsdiag_forenkf = base.get('lobsdiag_forenkf', False)
            eupd_run = base.get('EUPD_CYC', 'gdas').lower()
            if eupd_run in ['both']:
                self.eupd_runs = ['gfs', 'gdas']
            elif eupd_run in ['gfs', 'gdas']:
                self.eupd_runs = [eupd_run]

    def _get_app_configs(self):
        """
        Returns the config_files that are involved in the cycled app
        """

        configs = ['prep']

        if self.do_jediatmvar:
            configs += ['prepatmiodaobs', 'atmanlinit', 'atmanlvar', 'atmanlfv3inc', 'atmanlfinal']
        else:
            configs += ['anal', 'analdiag']

        if self.do_jediocnvar:
            configs += ['prepoceanobs', 'marineanlinit', 'marinebmat', 'marineanlvar']
            if self.do_hybvar:
                configs += ['marineanlletkf', 'ocnanalecen']
            configs += ['marineanlchkpt', 'marineanlfinal']
            if self.do_vrfy_oceanda:
                configs += ['ocnanalvrfy']

        if self.do_ocean or self.do_ice:
            configs += ['oceanice_products']

        configs += ['stage_ic', 'sfcanl', 'analcalc', 'fcst', 'upp', 'atmos_products', 'arch', 'cleanup']

        if self.do_hybvar:
            if self.do_jediatmens:
                configs += ['atmensanlinit', 'atmensanlobs', 'atmensanlsol', 'atmensanlletkf', 'atmensanlfv3inc', 'atmensanlfinal']
            else:
                configs += ['eobs', 'eomg', 'ediag', 'eupd']
            configs += ['ecen', 'esfc', 'efcs', 'echgres', 'epos', 'earc']

        if self.do_fit2obs:
            configs += ['fit2obs']

        if self.do_verfozn:
            configs += ['verfozn']

        if self.do_verfrad:
            configs += ['verfrad']

        if self.do_vminmon:
            configs += ['vminmon']

        if self.do_tracker:
            configs += ['tracker']

        if self.do_genesis:
            configs += ['genesis']

        if self.do_genesis_fsu:
            configs += ['genesis_fsu']

        if self.do_metp:
            configs += ['metp']

        if self.do_gempak:
            configs += ['gempak']
            if self.do_goes:
                configs += ['npoess']

        if self.do_bufrsnd:
            configs += ['postsnd']

        if self.do_awips:
            configs += ['awips']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostpnt']
            if self.do_wave_bnd:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
            if self.do_gempak:
                configs += ['wavegempak']
            if self.do_awips:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_aero:
            configs += ['aeroanlgenb', 'aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
            if self.do_prep_obs_aero:
                configs += ['prepobsaero']

        if self.do_jedisnowda:
            configs += ['snowanl']
            if self.do_hybvar:
                configs += ['esnowrecen']

        if self.do_mos:
            configs += ['mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                        'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                        'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen',
                        'mos_wx_prdgen', 'mos_wx_ext_prdgen']

        return configs

    @staticmethod
    def _update_base(base_in):

        return base_in

    def get_task_names(self):
        """
        Get the task names for all the tasks in the cycled application.
        Note that the order of the task names matters in the XML.
        This is the place where that order is set.
        """

        gdas_gfs_common_tasks_before_fcst = ['prep']
        gdas_gfs_common_cleanup_tasks = ['arch', 'cleanup']

        if self.do_jediatmvar:
            gdas_gfs_common_tasks_before_fcst += ['prepatmiodaobs', 'atmanlinit', 'atmanlvar', 'atmanlfv3inc', 'atmanlfinal']
        else:
            gdas_gfs_common_tasks_before_fcst += ['anal']

        if self.do_jediocnvar:
            gdas_gfs_common_tasks_before_fcst += ['prepoceanobs', 'marineanlinit', 'marinebmat', 'marineanlvar']
            if self.do_hybvar:
                gdas_gfs_common_tasks_before_fcst += ['marineanlletkf', 'ocnanalecen']
            gdas_gfs_common_tasks_before_fcst += ['marineanlchkpt', 'marineanlfinal']
            if self.do_vrfy_oceanda:
                gdas_gfs_common_tasks_before_fcst += ['ocnanalvrfy']

        gdas_gfs_common_tasks_before_fcst += ['sfcanl', 'analcalc']

        if self.do_jedisnowda:
            gdas_gfs_common_tasks_before_fcst += ['snowanl']

        wave_prep_tasks = ['waveinit', 'waveprep']
        wave_bndpnt_tasks = ['wavepostbndpnt', 'wavepostbndpntbll']
        wave_post_tasks = ['wavepostsbs', 'wavepostpnt']

        hybrid_tasks = []
        hybrid_after_eupd_tasks = []
        if self.do_hybvar:
            if self.do_jediatmens:
                hybrid_tasks += ['atmensanlinit', 'atmensanlfv3inc', 'atmensanlfinal', 'echgres']
                hybrid_tasks += ['atmensanlobs', 'atmensanlsol'] if self.lobsdiag_forenkf else ['atmensanlletkf']
            else:
                hybrid_tasks += ['eobs', 'eupd', 'echgres']
                hybrid_tasks += ['ediag'] if self.lobsdiag_forenkf else ['eomg']
            if self.do_jedisnowda:
                hybrid_tasks += ['esnowrecen']
            hybrid_after_eupd_tasks += ['stage_ic', 'ecen', 'esfc', 'efcs', 'epos', 'earc', 'cleanup']

        # Collect all "gdas" cycle tasks
        gdas_tasks = gdas_gfs_common_tasks_before_fcst.copy()

        if not self.do_jediatmvar:
            gdas_tasks += ['analdiag']

        if self.do_wave and 'gdas' in self.wave_runs:
            gdas_tasks += wave_prep_tasks

        if self.do_aero and 'gdas' in self.aero_anl_runs:
            gdas_tasks += ['aeroanlgenb', 'aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
            if self.do_prep_obs_aero:
                gdas_tasks += ['prepobsaero']

        gdas_tasks += ['stage_ic', 'atmanlupp', 'atmanlprod', 'fcst']

        if self.do_upp:
            gdas_tasks += ['atmupp']
        gdas_tasks += ['atmos_prod']

        if self.do_wave and 'gdas' in self.wave_runs:
            if self.do_wave_bnd:
                gdas_tasks += wave_bndpnt_tasks
            gdas_tasks += wave_post_tasks

        if self.do_fit2obs:
            gdas_tasks += ['fit2obs']

        if self.do_verfozn:
            gdas_tasks += ['verfozn']

        if self.do_verfrad:
            gdas_tasks += ['verfrad']

        if self.do_vminmon:
            gdas_tasks += ['vminmon']

        if self.do_gempak:
            gdas_tasks += ['gempak', 'gempakmetancdc']

        gdas_tasks += gdas_gfs_common_cleanup_tasks

        # Collect "gfs" cycle tasks
        gfs_tasks = gdas_gfs_common_tasks_before_fcst.copy()

        if self.do_wave and 'gfs' in self.wave_runs:
            gfs_tasks += wave_prep_tasks

        if self.do_aero and 'gfs' in self.aero_anl_runs:
            gfs_tasks += ['aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
            if self.do_prep_obs_aero:
                gfs_tasks += ['prepobsaero']

        gfs_tasks += ['atmanlupp', 'atmanlprod', 'fcst']

        if self.do_ocean:
            gfs_tasks += ['ocean_prod']

        if self.do_ice:
            gfs_tasks += ['ice_prod']

        if self.do_upp:
            gfs_tasks += ['atmupp']
        gfs_tasks += ['atmos_prod']

        if self.do_goes:
            gfs_tasks += ['goesupp']

        if self.do_vminmon:
            gfs_tasks += ['vminmon']

        if self.do_tracker:
            gfs_tasks += ['tracker']

        if self.do_genesis:
            gfs_tasks += ['genesis']

        if self.do_genesis_fsu:
            gfs_tasks += ['genesis_fsu']

        if self.do_metp:
            gfs_tasks += ['metp']

        if self.do_wave and 'gfs' in self.wave_runs:
            if self.do_wave_bnd:
                gfs_tasks += wave_bndpnt_tasks
            gfs_tasks += wave_post_tasks
            if self.do_gempak:
                gfs_tasks += ['wavegempak']
            if self.do_awips:
                gfs_tasks += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_bufrsnd:
            gfs_tasks += ['postsnd']

        if self.do_gempak:
            gfs_tasks += ['gempak']
            gfs_tasks += ['gempakmeta']
            gfs_tasks += ['gempakncdcupapgif']
            if self.do_goes:
                gfs_tasks += ['npoess_pgrb2_0p5deg']
                gfs_tasks += ['gempakpgrb2spec']

        if self.do_awips:
            gfs_tasks += ['awips_20km_1p0deg', 'fbwind']

        if self.do_mos:
            gfs_tasks += ['mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                          'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                          'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen',
                          'mos_wx_prdgen', 'mos_wx_ext_prdgen']

        gfs_tasks += gdas_gfs_common_cleanup_tasks

        tasks = dict()
        tasks['gdas'] = gdas_tasks

        if self.do_hybvar and 'gdas' in self.eupd_runs:
            enkfgdas_tasks = hybrid_tasks + hybrid_after_eupd_tasks
            tasks['enkfgdas'] = enkfgdas_tasks

        # Add RUN=gfs tasks if running early cycle
        if self.interval_gfs > to_timedelta("0H"):
            tasks['gfs'] = gfs_tasks

            if self.do_hybvar and 'gfs' in self.eupd_runs:
                enkfgfs_tasks = hybrid_tasks + hybrid_after_eupd_tasks
                enkfgfs_tasks.remove("echgres")
                enkfgfs_tasks.remove("esnowrecen")
                tasks['enkfgfs'] = enkfgfs_tasks

        return tasks
