from applications.applications import AppConfig
from rocoto.tasks import Tasks, create_wf_task
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
        dep_dict = {'type': 'metatask', 'name': 'gdaspost', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/gdas.t@Hz.atmf009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': '-06:00:00'}
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
        if self.app_config.mode in ['cycled']:
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        task = create_wf_task('waveinit', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def waveprep(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('waveprep')
        task = create_wf_task('waveprep', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

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
        offset = f'-{interval}'

        # Files from previous cycle
        files = [f'@Y@m@d.@H0000.fv_core.res.nc'] + \
                [f'@Y@m@d.@H0000.fv_core.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)] + \
                [f'@Y@m@d.@H0000.fv_tracer.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)]

        for file in files:
            data = [f'{restart_path}', file]
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
            dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': '-06:00:00'}
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
            dep_dict = {'type': 'task', 'name': 'enkfgdasechgres', 'offset': '-06:00:00'}
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
            dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': '-06:00:00'}
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
        dep_dict = {'type': 'data', 'data': data, 'offset': '-06:00:00'}
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
            offset = f'-{interval}'
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

        if self.app_config.do_wave and self.cdump in self.app_config.wave_cdumps:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}waveprep'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_aero:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlfinal'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_jedilandda:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}landanl'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        if self.cdump in ['gdas']:
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource('fcst')
        task = create_wf_task('fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def post(self):
        add_anl_to_post = False
        if self.app_config.mode in ['cycled']:
            add_anl_to_post = True

        return self._post_task('post', add_anl_to_post=add_anl_to_post)

    def ocnpost(self):
        if self.app_config.mode in ['forecast-only']:  # TODO: fix ocnpost in cycled mode
            return self._post_task('ocnpost', add_anl_to_post=False)

    def _post_task(self, task_name, add_anl_to_post=False):
        if task_name not in ['post', 'ocnpost']:
            raise KeyError(f'Invalid post-processing task: {task_name}')

        if task_name in ['ocnpost']:
            add_anl_to_post = False

        def _get_postgroups(cdump, config, add_anl=False):

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

            npostgrp = config['NPOSTGRP']
            ngrps = npostgrp if len(fhrs) > npostgrp else len(fhrs)

            fhrs = [f'f{fhr:03d}' for fhr in fhrs]
            fhrs = np.array_split(fhrs, ngrps)
            fhrs = [fhr.tolist() for fhr in fhrs]
            if add_anl:
                fhrs.insert(0, ['anl'])

            grp = ' '.join(f'_{fhr[0]}-{fhr[-1]}' if len(fhr) > 1 else f'_{fhr[0]}' for fhr in fhrs)
            dep = ' '.join([fhr[-1] for fhr in fhrs])
            lst = ' '.join(['_'.join(fhr) for fhr in fhrs])

            return grp, dep, lst

        deps = []
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        data = f'{atm_hist_path}/{self.cdump}.t@Hz.atm.log#dep#.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#',
                          'ROTDIR': self._base.get('ROTDIR')}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_postgroups(self.cdump, self._configs[task_name], add_anl=add_anl_to_post)
        vardict = {varname2: varval2, varname3: varval3}

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource(task_name)
        task = create_wf_task(task_name, resources, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                              metatask=task_name, varname=varname1, varval=varval1, vardict=vardict, cycledef=cycledef)

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

    def wafs(self):
        return self._wafs_task('wafs')

    def wafsgcip(self):
        return self._wafs_task('wafsgcip')

    def wafsgrib2(self):
        return self._wafs_task('wafsgrib2')

    def wafsgrib20p25(self):
        return self._wafs_task('wafsgrib20p25')

    def _wafs_task(self, task_name):
        if task_name not in ['wafs', 'wafsgcip', 'wafsgrib2', 'wafsgrib20p25']:
            raise KeyError(f'Invalid WAFS task: {task_name}')

        wafs_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_WAFS_TMPL"])

        deps = []
        fhrlst = [6] + [*range(12, 36 + 3, 3)]
        for fhr in fhrlst:
            data = f'{wafs_path}/{self.cdump}.t@Hz.wafs.grb2if{fhr:03d}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource(task_name)
        task = create_wf_task(task_name, resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wafsblending(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wafsgrib2'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wafsblending')
        task = create_wf_task('wafsblending', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wafsblending0p25(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wafsgrib20p25'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wafsblending0p25')
        task = create_wf_task('wafsblending0p25', resources, cdump=self.cdump, envar=self.envars,
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

    def awips(self):

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

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self._base.get('ROTDIR')}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_awipsgroups(self.cdump, self._configs['awips'])
        vardict = {varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')
        task = create_wf_task('awips', resources, cdump=self.cdump, envar=awipsenvars, dependency=dependencies,
                              metatask='awips', varname=varname1, varval=varval1, vardict=vardict)

        return task

    def gempak(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task = create_wf_task('gempak', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def vrfy(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource('vrfy')
        task = create_wf_task('vrfy', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

        return task

    def fit2obs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump

        resources = self.get_resource('fit2obs')
        task = create_wf_task('fit2obs', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies,
                              cycledef=cycledef)

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

    def arch(self):
        deps = []
        if self.app_config.do_vrfy:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}vrfy'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.cdump in ['gdas'] and self.app_config.do_fit2obs:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}fit2obs'}
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
        # If all verification and ocean/wave coupling is off, add the gdas/gfs post metatask as a dependency
        if len(deps) == 0:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

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
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': '-06:00:00'}
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
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': '-06:00:00'}
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
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': '-06:00:00'}
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
        dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
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
