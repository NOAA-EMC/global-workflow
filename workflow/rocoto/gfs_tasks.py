from applications.applications import AppConfig
from rocoto.tasks import Tasks, create_wf_task
from wxflow import timedelta_to_HMS
import rocoto.rocoto as rocoto
import numpy as np


class GFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, cdump: str) -> None:
        super().__init__(app_config, cdump)

    @staticmethod
    def _is_this_a_gdas_task(cdump, task_name):
        if cdump != 'enkfgdas':
            raise TypeError(f'{task_name} must be part of the "enkfgdas" cycle and not {cdump}')

    # Specific Tasks begin here
    def stage_ic(self):

        cpl_ic = self._configs['stage_ic']

        deps = []

        # Atm ICs
        if self.app_config.do_atm:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ATMIC']}/@Y@m@d@H/atmos"
            for file in ['gfs_ctrl.nc'] + \
                        [f'{datatype}_data.tile{tile}.nc'
                         for datatype in ['gfs', 'sfc']
                         for tile in range(1, self.n_tiles + 1)]:
                data = f"{prefix}/{file}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))
        else:  # data-atmosphere
            # TODO - need more information about how these forcings are stored
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_DATM']}/@Y@m@d@H"
            data = f"{prefix}/gefs.@Y@m.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Ocean ICs
        if self.app_config.do_ocean:
            ocn_res = f"{self._base.get('OCNRES', '025'):03d}"
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_OCNIC']}/@Y@m@d@H/ocean"
            data = f"{prefix}/@Y@m@d.@H0000.MOM.res.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
            if ocn_res in ['025']:
                # 0.25 degree ocean model also has these additional restarts
                for res in [f'res_{res_index}' for res_index in range(1, 4)]:
                    data = f"{prefix}/@Y@m@d.@H0000.MOM.{res}.nc"
                    dep_dict = {'type': 'data', 'data': data}
                    deps.append(rocoto.add_dependency(dep_dict))

        # Ice ICs
        if self.app_config.do_ice:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ICEIC']}/@Y@m@d@H/ice"
            data = f"{prefix}/@Y@m@d.@H0000.cice_model.res.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Wave ICs
        if self.app_config.do_wave:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_WAVIC']}/@Y@m@d@H/wave"
            for wave_grid in self._configs['waveinit']['waveGRD'].split():
                data = f"{prefix}/@Y@m@d.@H0000.restart.{wave_grid}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('stage_ic')
        task = create_wf_task('stage_ic', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def prep(self):

        dump_suffix = self._base["DUMP_SUFFIX"]
        gfs_cyc = self._base["gfs_cyc"]
        dmpdir = self._base["DMPDIR"]
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'RUN': 'gdas'})
        dump_path = self._template_to_rocoto_cycstring(self._base["COM_OBSDMP_TMPL"],
                                                       {'DMPDIR': dmpdir, 'DUMP_SUFFIX': dump_suffix})

        gfs_enkf = True if self.app_config.do_hybvar and 'gfs' in self.app_config.eupd_cdumps else False

        deps = []
        dep_dict = {'type': 'metatask', 'name': 'gdasatmprod', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/gdas.t@Hz.atmf009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{dump_path}/{self.cdump}.t@Hz.updated.status.tm00.bufr_d'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = self.cdump
        if self.cdump in ['gfs'] and gfs_enkf and gfs_cyc != 4:
            cycledef = 'gdas'

        resources = self.get_resource('prep')
        task = create_wf_task('prep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def waveinit(self):

        resources = self.get_resource('waveinit')
        dependencies = None
        cycledef = None
        if self.app_config.mode in ['cycled']:
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.cdump in ['gdas']:
                dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
                deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
            cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        task = create_wf_task('waveinit', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies, cycledef=cycledef)

        return task

    def waveprep(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource('waveprep')
        task = create_wf_task('waveprep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies, cycledef=cycledef)

        return task

    def aerosol_init(self):

        input_path = self._template_to_rocoto_cycstring(self._base['COM_ATMOS_INPUT_TMPL'])
        restart_path = self._template_to_rocoto_cycstring(self._base['COM_ATMOS_RESTART_TMPL'])

        deps = []
        # Files from current cycle
        files = ['gfs_ctrl.nc'] + [f'gfs_data.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)]
        for file in files:
            data = f'{input_path}/{file}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Calculate offset based on CDUMP = gfs | gdas
        interval = None
        if self.cdump in ['gfs']:
            interval = self._base['INTERVAL_GFS']
        elif self.cdump in ['gdas']:
            interval = self._base['INTERVAL']
        offset = timedelta_to_HMS(-interval)

        # Files from previous cycle
        files = [f'@Y@m@d.@H0000.fv_core.res.nc'] + \
                [f'@Y@m@d.@H0000.fv_core.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)] + \
                [f'@Y@m@d.@H0000.fv_tracer.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)]

        for file in files:
            data = [f'{restart_path}/', file]
            dep_dict = {'type': 'data', 'data': data, 'offset': [offset, None]}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = 'gfs_seq'
        resources = self.get_resource('aerosol_init')
        task = create_wf_task('aerosol_init', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def anal(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('anal')
        task = create_wf_task('anal', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def sfcanl(self):

        deps = []
        if self.app_config.do_jediatmvar:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jedilandda:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}landanl'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('sfcanl')
        task = create_wf_task('sfcanl', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def analcalc(self):

        deps = []
        if self.app_config.do_jediatmvar:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}sfcanl'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar and self.cdump in ['gdas']:
            dep_dict = {'type': 'task', 'name': 'enkfgdasechgres', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analcalc')
        task = create_wf_task('analcalc', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def analdiag(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analdiag')
        task = create_wf_task('analdiag', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def prepatmiodaobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepatmiodaobs')
        task = create_wf_task('prepatmiodaobs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def atmanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prepatmiodaobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        gfs_cyc = self._base["gfs_cyc"]
        gfs_enkf = True if self.app_config.do_hybvar and 'gfs' in self.app_config.eupd_cdumps else False

        cycledef = self.cdump
        if self.cdump in ['gfs'] and gfs_enkf and gfs_cyc != 4:
            cycledef = 'gdas'

        resources = self.get_resource('atmanlinit')
        task = create_wf_task('atmanlinit', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def atmanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmanlrun')
        task = create_wf_task('atmanlrun', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def atmanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmanlfinal')
        task = create_wf_task('atmanlfinal', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def aeroanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlinit')
        task = create_wf_task('aeroanlinit', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)
        return task

    def aeroanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('aeroanlrun')
        task = create_wf_task('aeroanlrun', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def aeroanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlfinal')
        task = create_wf_task('aeroanlfinal', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def preplandobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('preplandobs')
        task = create_wf_task('preplandobs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def landanl(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}preplandobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('landanl')
        task = create_wf_task('landanl', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)
        return task

    def ocnanalprep(self):

        ocean_hist_path = self._template_to_rocoto_cycstring(self._base["COM_OCEAN_HISTORY_TMPL"], {'RUN': 'gdas'})

        deps = []
        data = f'{ocean_hist_path}/gdas.t@Hz.ocnf009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalprep')
        task = create_wf_task('ocnanalprep',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def ocnanalbmat(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalprep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalbmat')
        task = create_wf_task('ocnanalbmat',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def ocnanalrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalbmat'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalrun')
        task = create_wf_task('ocnanalrun',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def ocnanalchkpt(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_mergensst:
            data = f'&ROTDIR;/{self.cdump}.@Y@m@d/@H/atmos/{self.cdump}.t@Hz.sfcanl.nc'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalchkpt')
        task = create_wf_task('ocnanalchkpt',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def ocnanalpost(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalchkpt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalpost')
        task = create_wf_task('ocnanalpost',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def ocnanalvrfy(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalpost'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalvrfy')
        task = create_wf_task('ocnanalvrfy',
                              resources,
                              cdump=self.cdump,
                              envar=self.envars,
                              dependency=dependencies)

        return task

    def fcst(self):

        fcst_map = {'forecast-only': self._fcst_forecast_only,
                    'cycled': self._fcst_cycled}

        try:
            task = fcst_map[self.app_config.mode]()
        except KeyError:
            raise NotImplementedError(f'{self.app_config.mode} is not a valid type.\n' +
                                      'Currently supported forecast types are:\n' +
                                      f'{" | ".join(fcst_map.keys())}')

        return task

    def _fcst_forecast_only(self):
        dependencies = []

        dep_dict = {'type': 'task', 'name': f'{self.cdump}stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_wave and self.cdump in self.app_config.wave_cdumps:
            wave_job = 'waveprep' if self.app_config.model_app in ['ATMW'] else 'waveinit'
            dep_dict = {'type': 'task', 'name': f'{self.cdump}{wave_job}'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_aero:
            # Calculate offset based on CDUMP = gfs | gdas
            interval = None
            if self.cdump in ['gfs']:
                interval = self._base['INTERVAL_GFS']
            elif self.cdump in ['gdas']:
                interval = self._base['INTERVAL']
            offset = timedelta_to_HMS(-interval)
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.cdump}aerosol_init'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': offset}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies.append(rocoto.create_dependency(dep_condition='or', dep=deps))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        resources = self.get_resource('fcst')
        task = create_wf_task('fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def _fcst_cycled(self):

        dep_dict = {'type': 'task', 'name': f'{self.cdump}sfcanl'}
        dep = rocoto.add_dependency(dep_dict)
        dependencies = rocoto.create_dependency(dep=dep)

        if self.app_config.do_jediocnvar:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalpost'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_aero:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlfinal'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_jedilandda:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}landanl'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        if self.cdump in ['gdas']:
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        if self.app_config.do_wave and self.cdump in self.app_config.wave_cdumps:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}waveprep'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource('fcst')
        task = create_wf_task('fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def atmanlupp(self):
        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': 'f000',
                          'UPP_RUN': 'analysis'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_anl_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_ANALYSIS_TMPL"])
        deps = []
        data = f'{atm_anl_path}/{self.cdump}.t@Hz.atmanl.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_anl_path}/{self.cdump}.t@Hz.sfcanl.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_anl_path}/{self.cdump}.t@Hz.loganl.txt'
        dep_dict = {'type': 'data', 'data': data, 'age': 60}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')
        resources = self.get_resource('upp')
        task = create_wf_task('atmanlupp', resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              cycledef=self.cdump, command='&JOBS_DIR;/upp.sh')

        return task

    def atmanlprod(self):
        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '-f001'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_master_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_MASTER_TMPL"])
        deps = []
        data = f'{atm_master_path}/{self.cdump}.t@Hz.master.grb2anl'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        resources = self.get_resource('atmos_products')
        task = create_wf_task('atmanlprod', resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              cycledef=self.cdump, command='&JOBS_DIR;/atmos_products.sh')

        return task

    @staticmethod
    def _get_ufs_postproc_grps(cdump, config):

        fhmin = config['FHMIN']
        fhmax = config['FHMAX']
        fhout = config['FHOUT']

        # Get a list of all forecast hours
        fhrs = []
        if cdump in ['gdas']:
            fhrs = range(fhmin, fhmax + fhout, fhout)
        elif cdump in ['gfs']:
            fhmax = np.max(
                [config['FHMAX_GFS_00'], config['FHMAX_GFS_06'], config['FHMAX_GFS_12'], config['FHMAX_GFS_18']])
            fhout = config['FHOUT_GFS']
            fhmax_hf = config['FHMAX_HF_GFS']
            fhout_hf = config['FHOUT_HF_GFS']
            fhrs_hf = range(fhmin, fhmax_hf + fhout_hf, fhout_hf)
            fhrs = list(fhrs_hf) + list(range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout))

        nfhrs_per_grp = config.get('NFHRS_PER_GROUP', 1)
        ngrps = len(fhrs) // nfhrs_per_grp if len(fhrs) % nfhrs_per_grp == 0 else len(fhrs) // nfhrs_per_grp + 1

        fhrs = [f'f{fhr:03d}' for fhr in fhrs]
        fhrs = np.array_split(fhrs, ngrps)
        fhrs = [fhr.tolist() for fhr in fhrs]

        grp = ' '.join(f'_{fhr[0]}-{fhr[-1]}' if len(fhr) > 1 else f'_{fhr[0]}' for fhr in fhrs)
        dep = ' '.join([fhr[-1] for fhr in fhrs])
        lst = ' '.join(['_'.join(fhr) for fhr in fhrs])

        return grp, dep, lst

    def atmupp(self):

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_ufs_postproc_grps(self.cdump, self._configs['upp'])
        vardict = {varname2: varval2, varname3: varval3}

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '#lst#',
                          'UPP_RUN': 'forecast'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        deps = []
        data = f'{atm_hist_path}/{self.cdump}.t@Hz.atm#dep#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.cdump}.t@Hz.sfc#dep#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.cdump}.t@Hz.atm.log#dep#.txt'
        dep_dict = {'type': 'data', 'data': data, 'age': 60}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource('upp')
        task = create_wf_task('atmupp', resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              metatask='atmupp', varname=varname1, varval=varval1, vardict=vardict, cycledef=cycledef,
                              command='&JOBS_DIR;/upp.sh')

        return task

    def atmprod(self):

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_ufs_postproc_grps(self.cdump, self._configs['atmos_products'])
        vardict = {varname2: varval2, varname3: varval3}

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '#lst#'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_master_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_MASTER_TMPL"])
        deps = []
        data = f'{atm_master_path}/{self.cdump}.t@Hz.master.grb2#dep#'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource('atmos_products')
        task = create_wf_task('atmprod', resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              metatask='atmprod', varname=varname1, varval=varval1, vardict=vardict, cycledef=cycledef,
                              command='&JOBS_DIR;/atmos_products.sh')

        return task

    def ocnpost(self):

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_ufs_postproc_grps(self.cdump, self._configs['ocnpost'])
        vardict = {varname2: varval2, varname3: varval3}

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '#lst#',
                          'ROTDIR': self._base.get('ROTDIR')}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        deps = []
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        data = f'{atm_hist_path}/{self.cdump}.t@Hz.atm.log#dep#.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource('ocnpost')
        task = create_wf_task('ocnpost', resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              metatask='ocnpost', varname=varname1, varval=varval1, vardict=vardict, cycledef=cycledef)

        return task

    def wavepostsbs(self):
        deps = []
        for wave_grid in self._configs['wavepostsbs']['waveGRD'].split():
            wave_hist_path = self._template_to_rocoto_cycstring(self._base["COM_WAVE_HISTORY_TMPL"])
            data = f'{wave_hist_path}/{self.cdump}wave.out_grd.{wave_grid}.@Y@m@d.@H0000'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('wavepostsbs')
        task = create_wf_task('wavepostsbs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpnt')
        task = create_wf_task('wavepostbndpnt', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavepostbndpntbll(self):
        deps = []
        atmos_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        data = f'{atmos_hist_path}/{self.cdump}.t@Hz.atm.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpntbll')
        task = create_wf_task('wavepostbndpntbll', resources, cdump=self.cdump, envar=self.envars,
                              dependency=dependencies)

        return task

    def wavepostpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_wave_bnd:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostbndpntbll'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('wavepostpnt')
        task = create_wf_task('wavepostpnt', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavegempak(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavegempak')
        task = create_wf_task('wavegempak', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def waveawipsbulls(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostpnt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('waveawipsbulls')
        task = create_wf_task('waveawipsbulls', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def waveawipsgridded(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('waveawipsgridded')
        task = create_wf_task('waveawipsgridded', resources, cdump=self.cdump, envar=self.envars,
                              dependency=dependencies)

        return task

    def postsnd(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('postsnd')
        task = create_wf_task('postsnd', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def fbwind(self):

        atmos_prod_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_GRIB_GRID_TMPL"], {'RUN': self.cdump, 'GRID': '0p25'})
        deps = []
        data = f'{atmos_prod_path}/{self.cdump}.t@Hz.pgrb2.0p25.f006'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.cdump}.t@Hz.pgrb2.0p25.f012'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.cdump}.t@Hz.pgrb2.0p25.f024'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('awips')

        # TODO: It would be better to use task dependencies on the
        # individual post jobs rather than data dependencies to avoid
        # prematurely starting with partial files. Unfortunately, the
        # ability to "group" post would make this more convoluted than
        # it should be and not worth the complexity.
        task = create_wf_task('fbwind', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    @staticmethod
    def _get_awipsgroups(cdump, config):

        fhmin = config['FHMIN']
        fhmax = config['FHMAX']
        fhout = config['FHOUT']

        # Get a list of all forecast hours
        fhrs = []
        if cdump in ['gdas']:
            fhrs = range(fhmin, fhmax + fhout, fhout)
        elif cdump in ['gfs']:
            fhmax = np.max(
                [config['FHMAX_GFS_00'], config['FHMAX_GFS_06'], config['FHMAX_GFS_12'], config['FHMAX_GFS_18']])
            fhout = config['FHOUT_GFS']
            fhmax_hf = config['FHMAX_HF_GFS']
            fhout_hf = config['FHOUT_HF_GFS']
            if fhmax > 240:
                fhmax = 240
            if fhmax_hf > 240:
                fhmax_hf = 240
            fhrs_hf = list(range(fhmin, fhmax_hf + fhout_hf, fhout_hf))
            fhrs = fhrs_hf + list(range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout))

        nawipsgrp = config['NAWIPSGRP']
        ngrps = nawipsgrp if len(fhrs) > nawipsgrp else len(fhrs)

        fhrs = [f'f{fhr:03d}' for fhr in fhrs]
        fhrs = np.array_split(fhrs, ngrps)
        fhrs = [fhr.tolist() for fhr in fhrs]

        grp = ' '.join([f'_{fhr[0]}-{fhr[-1]}' for fhr in fhrs])
        dep = ' '.join([fhr[-1] for fhr in fhrs])
        lst = ' '.join(['_'.join(fhr) for fhr in fhrs])

        return grp, dep, lst

    def awips_20km_1p0deg(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self._base.get('ROTDIR')}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_awipsgroups(self.cdump, self._configs['awips'])
        vardict = {varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')
        task = create_wf_task('awips_20km_1p0deg', resources, cdump=self.cdump, envar=awipsenvars, dependency=dependencies,
                              metatask='awips_20km_1p0deg', varname=varname1, varval=varval1, vardict=vardict)

        return task

    def awips_g2(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self._base.get('ROTDIR')}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_awipsgroups(self.cdump, self._configs['awips'])
        vardict = {varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')
        task = create_wf_task('awips_g2', resources, cdump=self.cdump, envar=awipsenvars, dependency=dependencies,
                              metatask='awips_g2', varname=varname1, varval=varval1, vardict=vardict)

        return task

    def gempak(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempak', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def gempakmeta(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempakmeta', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def gempakmetancdc(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempakmetancdc', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def gempakncdcupapgif(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempakncdcupapgif', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def gempakpgrb2spec(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}npoess_pgrb2_0p5deg'}
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempakpgrb2spec', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def npoess_pgrb2_0p5deg(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('npoess')
        task = create_wf_task('npoess_pgrb2_0p5deg', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def verfozn(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfozn')
        task = create_wf_task('verfozn', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def verfrad(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfrad')
        task = create_wf_task('verfrad', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def vminmon(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('vminmon')
        task = create_wf_task('vminmon', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def tracker(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('tracker')
        task = create_wf_task('tracker', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def genesis(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis')
        task = create_wf_task('genesis', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def genesis_fsu(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis_fsu')
        task = create_wf_task('genesis_fsu', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def fit2obs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('fit2obs')
        task = create_wf_task('fit2obs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def metp(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}arch'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        metpenvars = self.envars.copy()
        if self.app_config.mode in ['cycled']:
            metpenvar_dict = {'SDATE_GFS': self._base.get('SDATE_GFS').strftime("%Y%m%d%H")}
        elif self.app_config.mode in ['forecast-only']:
            metpenvar_dict = {'SDATE_GFS': self._base.get('SDATE').strftime("%Y%m%d%H")}
        metpenvar_dict['METPCASE'] = '#metpcase#'
        for key, value in metpenvar_dict.items():
            metpenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1 = 'metpcase'
        varval1 = 'g2g1 g2o1 pcp1'

        resources = self.get_resource('metp')
        task = create_wf_task('metp', resources, cdump=self.cdump, envar=metpenvars, dependency=dependencies,
                              metatask='metp', varname=varname1, varval=varval1)

        return task

    def mos_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prep')
        task = create_wf_task('mos_stn_prep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_grd_prep')
        task = create_wf_task('mos_grd_prep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_stn_prep')
        task = create_wf_task('mos_ext_stn_prep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_grd_prep')
        task = create_wf_task('mos_ext_grd_prep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_stn_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_fcst')
        task = create_wf_task('mos_stn_fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_grd_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_grd_fcst')
        task = create_wf_task('mos_grd_fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_stn_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_stn_fcst')
        task = create_wf_task('mos_ext_stn_fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_grd_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_grd_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_grd_fcst')
        task = create_wf_task('mos_ext_grd_fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_stn_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prdgen')
        task = create_wf_task('mos_stn_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_grd_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_grd_prdgen')
        task = create_wf_task('mos_grd_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_stn_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_stn_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_stn_prdgen')
        task = create_wf_task('mos_ext_stn_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_ext_grd_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_grd_prdgen')
        task = create_wf_task('mos_ext_grd_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_wx_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_wx_prdgen')
        task = create_wf_task('mos_wx_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def mos_wx_ext_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_ext_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_wx_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_wx_ext_prdgen')
        task = create_wf_task('mos_wx_ext_prdgen', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def arch(self):
        deps = []
        dependencies = []
        if self.app_config.mode in ['cycled']:
            if self.cdump in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_vminmon:
                    dep_dict = {'type': 'task', 'name': f'{self.cdump}vminmon'}
                    deps.append(rocoto.add_dependency(dep_dict))
            elif self.cdump in ['gdas']:  # Block for handling half cycle dependencies
                deps2 = []
                dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlprod'}
                deps2.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_fit2obs:
                    dep_dict = {'type': 'task', 'name': f'{self.cdump}fit2obs'}
                    deps2.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_verfozn:
                    dep_dict = {'type': 'task', 'name': f'{self.cdump}verfozn'}
                    deps2.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_verfrad:
                    dep_dict = {'type': 'task', 'name': f'{self.cdump}verfrad'}
                    deps2.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_vminmon:
                    dep_dict = {'type': 'task', 'name': f'{self.cdump}vminmon'}
                    deps2.append(rocoto.add_dependency(dep_dict))
                dependencies = rocoto.create_dependency(dep_condition='and', dep=deps2)
                dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
                dependencies.append(rocoto.add_dependency(dep_dict))
                dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)
        if self.cdump in ['gfs'] and self.app_config.do_tracker:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}tracker'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.cdump in ['gfs'] and self.app_config.do_genesis:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}genesis'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.cdump in ['gfs'] and self.app_config.do_genesis_fsu:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}genesis_fsu'}
            deps.append(rocoto.add_dependency(dep_dict))
        # Post job dependencies
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_wave:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostpnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.app_config.do_wave_bnd:
                dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostbndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_ocean:
            if self.app_config.mode in ['forecast-only']:  # TODO: fix ocnpost to run in cycled mode
                dep_dict = {'type': 'metatask', 'name': f'{self.cdump}ocnpost'}
                deps.append(rocoto.add_dependency(dep_dict))
        # MOS job dependencies
        if self.cdump in ['gfs'] and self.app_config.do_mos:
            mos_jobs = ["stn_prep", "grd_prep", "ext_stn_prep", "ext_grd_prep",
                        "stn_fcst", "grd_fcst", "ext_stn_fcst", "ext_grd_fcst",
                        "stn_prdgen", "grd_prdgen", "ext_stn_prdgen", "ext_grd_prdgen",
                        "wx_prdgen", "wx_ext_prdgen"]
            for job in mos_jobs:
                dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_{job}'}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps + dependencies)

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource('arch')
        task = create_wf_task('arch', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    # Cleanup
    def cleanup(self):
        deps = []
        if 'enkf' in self.cdump:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}eamn'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}arch'}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('cleanup')
        task = create_wf_task('cleanup', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    # Start of ensemble tasks
    def eobs(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('eobs')
        task = create_wf_task('eobs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def eomg(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eomgenvars = self.envars.copy()
        eomgenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENS'], self._configs['eobs']['NMEM_EOMGGRP'])

        resources = self.get_resource('eomg')
        task = create_wf_task('eomg', resources, cdump=self.cdump, envar=eomgenvars, dependency=dependencies,
                              metatask='eomn', varname='grp', varval=groups)

        return task

    def ediag(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ediag')
        task = create_wf_task('ediag', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def eupd(self):
        deps = []
        if self.app_config.lobsdiag_forenkf:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}ediag'}
        else:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}eomn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('eupd')
        task = create_wf_task('eupd', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def atmensanlinit(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}prepatmiodaobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = "gdas"
        resources = self.get_resource('atmensanlinit')
        task = create_wf_task('atmensanlinit', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def atmensanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlrun')
        task = create_wf_task('atmensanlrun', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def atmensanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmensanlfinal')
        task = create_wf_task('atmensanlfinal', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def ecen(self):

        def _get_ecengroups():

            if self._base.get('DOIAU_ENKF', False):
                fhrs = list(self._base.get('IAUFHRS', '6').split(','))

                necengrp = self._configs['ecen']['NECENGRP']
                ngrps = necengrp if len(fhrs) > necengrp else len(fhrs)

                fhrs = [f'{int(fhr):03d}' for fhr in fhrs]
                fhrs = np.array_split(fhrs, ngrps)
                fhrs = [fhr.tolist() for fhr in fhrs]

                grp = ' '.join([f'{x:03d}' for x in range(0, ngrps)])
                dep = ' '.join([f[-1] for f in fhrs])
                lst = ' '.join(['_'.join(f) for f in fhrs])

            else:
                grp = '000'
                dep = 'f006'
                lst = 'f006'

            return grp, dep, lst

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jediatmens:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        ecenenvars = self.envars.copy()
        ecenenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in ecenenvar_dict.items():
            ecenenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_ecengroups()
        vardict = {varname2: varval2, varname3: varval3}

        resources = self.get_resource('ecen')
        task = create_wf_task('ecen', resources, cdump=self.cdump, envar=ecenenvars, dependency=dependencies,
                              metatask='ecmn', varname=varname1, varval=varval1, vardict=vardict)
        return task

    def esfc(self):

        # eupd_cdump = 'gdas' if 'gdas' in self.app_config.eupd_cdumps else 'gfs'

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jediatmens:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('esfc')
        task = create_wf_task('esfc', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def efcs(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}ecmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}esfc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        dependencies.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        efcsenvars = self.envars.copy()
        efcsenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENS'], self._configs['efcs']['NMEM_EFCSGRP'])

        if self.cdump == "enkfgfs":
            groups = self._get_hybgroups(self._base['NMEM_ENS_GFS'], self._configs['efcs']['NMEM_EFCSGRP_GFS'])
        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')
        resources = self.get_resource('efcs')
        task = create_wf_task('efcs', resources, cdump=self.cdump, envar=efcsenvars, dependency=dependencies,
                              metatask='efmn', varname='grp', varval=groups, cycledef=cycledef)

        return task

    def echgres(self):

        self._is_this_a_gdas_task(self.cdump, 'echgres')

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}efcs01'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump

        resources = self.get_resource('echgres')
        task = create_wf_task('echgres', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def epos(self):

        def _get_eposgroups(epos):
            fhmin = epos['FHMIN_ENKF']
            fhmax = epos['FHMAX_ENKF']
            fhout = epos['FHOUT_ENKF']
            if self.cdump == "enkfgfs":
                fhmax = epos['FHMAX_ENKF_GFS']
                fhout = epos['FHOUT_ENKF_GFS']
            fhrs = range(fhmin, fhmax + fhout, fhout)

            neposgrp = epos['NEPOSGRP']
            ngrps = neposgrp if len(fhrs) > neposgrp else len(fhrs)

            fhrs = [f'f{fhr:03d}' for fhr in fhrs]
            fhrs = np.array_split(fhrs, ngrps)
            fhrs = [f.tolist() for f in fhrs]

            grp = ' '.join([f'{x:03d}' for x in range(0, ngrps)])
            dep = ' '.join([f[-1] for f in fhrs])
            lst = ' '.join(['_'.join(f) for f in fhrs])

            return grp, dep, lst

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}efmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eposenvars = self.envars.copy()
        eposenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in eposenvar_dict.items():
            eposenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_eposgroups(self._configs['epos'])
        vardict = {varname2: varval2, varname3: varval3}

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')

        resources = self.get_resource('epos')
        task = create_wf_task('epos', resources, cdump=self.cdump, envar=eposenvars, dependency=dependencies,
                              metatask='epmn', varname=varname1, varval=varval1, vardict=vardict, cycledef=cycledef)

        return task

    def earc(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}epmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENS'], self._configs['earc']['NMEM_EARCGRP'], start_index=0)

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')

        resources = self.get_resource('earc')
        task = create_wf_task('earc', resources, cdump=self.cdump, envar=earcenvars, dependency=dependencies,
                              metatask='eamn', varname='grp', varval=groups, cycledef=cycledef)

        return task
