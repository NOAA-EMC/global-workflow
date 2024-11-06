from applications.applications import AppConfig
from rocoto.tasks import Tasks
from wxflow import timedelta_to_HMS, to_timedelta
import rocoto.rocoto as rocoto
import numpy as np


class GFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, run: str) -> None:
        super().__init__(app_config, run)

    @staticmethod
    def _is_this_a_gdas_task(run, task_name):
        if run != 'enkfgdas':
            raise TypeError(f'{task_name} must be part of the "enkfgdas" cycle and not {run}')

    # Specific Tasks begin here
    def stage_ic(self):

        cycledef = 'gdas_half' if self.run in ['gdas', 'enkfgdas'] else self.run

        resources = self.get_resource('stage_ic')
        task_name = f'{self.run}_stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/stage_ic.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prep(self):

        dump_suffix = self._base["DUMP_SUFFIX"]
        dmpdir = self._base["DMPDIR"]
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'RUN': 'gdas'})
        dump_path = self._template_to_rocoto_cycstring(self._base["COM_OBSDMP_TMPL"],
                                                       {'DMPDIR': dmpdir, 'DUMP_SUFFIX': dump_suffix})

        gfs_enkf = True if self.app_config.do_hybvar and 'gfs' in self.app_config.eupd_runs else False

        deps = []
        dep_dict = {'type': 'metatask', 'name': 'gdas_atmos_prod', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/gdas.t@Hz.atmf009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{dump_path}/{self.run}.t@Hz.updated.status.tm00.bufr_d'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = self.run
        if self.run in ['gfs'] and gfs_enkf and self.app_config.interval_gfs != 6:
            cycledef = 'gdas'

        resources = self.get_resource('prep')
        task_name = f'{self.run}_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveinit(self):

        resources = self.get_resource('waveinit')
        dependencies = None
        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run
        if self.app_config.mode in ['cycled']:
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.run in ['gdas']:
                dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
                deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        task_name = f'{self.run}_waveinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveprep(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run
        resources = self.get_resource('waveprep')
        task_name = f'{self.run}_waveprep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveprep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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

        # Calculate offset based on RUN = gfs | gdas
        interval = None
        if self.run in ['gfs']:
            interval = self._base['INTERVAL_GFS']
        elif self.run in ['gdas']:
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
        task_name = f'{self.run}_aerosol_init'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aerosol_init.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def anal(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('anal')
        task_name = f'{self.run}_anal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/anal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def sfcanl(self):

        deps = []
        if self.app_config.do_jediatmvar:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jedisnowda:
            dep_dict = {'type': 'task', 'name': f'{self.run}_snowanl'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('sfcanl')
        task_name = f'{self.run}_sfcanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/sfcanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def analcalc(self):

        deps = []
        if self.app_config.do_jediatmvar:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_sfcanl'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar and self.run in ['gdas']:
            dep_dict = {'type': 'task', 'name': 'enkfgdas_echgres', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analcalc')
        task_name = f'{self.run}_analcalc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/analcalc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def analdiag(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analdiag')
        task_name = f'{self.run}_analdiag'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/analdiag.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepatmiodaobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepatmiodaobs')
        task_name = f'{self.run}_prepatmiodaobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepatmiodaobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prepatmiodaobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        interval_gfs = self._base["INTERVAL_GFS"]
        gfs_enkf = True if self.app_config.do_hybvar and 'gfs' in self.app_config.eupd_runs else False

        cycledef = self.run
        if self.run in ['gfs'] and gfs_enkf and interval_gfs != 6:
            cycledef = 'gdas'

        resources = self.get_resource('atmanlinit')
        task_name = f'{self.run}_atmanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlvar(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmanlvar')
        task_name = f'{self.run}_atmanlvar'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlvar.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlfv3inc(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlvar'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmanlfv3inc')
        task_name = f'{self.run}_atmanlfv3inc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlfv3inc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfv3inc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmanlfinal')
        task_name = f'{self.run}_atmanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepobsaero(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('prepobsaero')
        task_name = f'{self.run}_prepobsaero'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepobsaero.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlgenb(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('aeroanlgenb')
        task_name = f'{self.run}_aeroanlgenb'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gdas_half,gdas',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlgenb.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': 'gdas_aeroanlgenb', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_prep_obs_aero:
            dep_dict = {'type': 'task', 'name': f'{self.run}_prepobsaero'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlinit')
        task_name = f'{self.run}_aeroanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlvar(self):

        deps = []
        dep_dict = {
            'type': 'task', 'name': f'gdas_aeroanlgenb',
            'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}",
        }
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {
            'type': 'task', 'name': f'{self.run}_aeroanlinit',
        }
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlvar')
        task_name = f'{self.run}_aeroanlvar'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlvar.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_aeroanlvar'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlfinal')
        task_name = f'{self.run}_aeroanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepsnowobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepsnowobs')
        task_name = f'{self.run}_prepsnowobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepsnowobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def snowanl(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prepsnowobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('snowanl')
        task_name = f'{self.run}_snowanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/snowanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)
        return task

    def esnowrecen(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_prepsnowobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_snowanl'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('esnowrecen')
        task_name = f'{self.run}_esnowrecen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/esnowrecen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)
        return task

    def prepoceanobs(self):

        ocean_hist_path = self._template_to_rocoto_cycstring(self._base["COM_OCEAN_HISTORY_TMPL"], {'RUN': 'gdas'})

        deps = []
        data = f'{ocean_hist_path}/gdas.ocean.t@Hz.inst.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepoceanobs')
        task_name = f'{self.run}_prepoceanobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepoceanobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlletkf(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'enkfgdas_fcst', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_prepoceanobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marineanlletkf')
        task_name = f'{self.run}_marineanlletkf'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marineanlletkf.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marinebmat(self):

        ocean_hist_path = self._template_to_rocoto_cycstring(self._base["COM_OCEAN_HISTORY_TMPL"], {'RUN': 'gdas'})

        deps = []
        data = f'{ocean_hist_path}/gdas.ocean.t@Hz.inst.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('marinebmat')
        task_name = f'{self.run}_marinebmat'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marinebmat.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prepoceanobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_marinebmat'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'gdas_fcst', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marineanlinit')
        task_name = f'{self.run}_marineanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marineanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlvar(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('marineanlvar')
        task_name = f'{self.run}_marineanlvar'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marineanlvar.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalecen(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlvar'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalecen')
        task_name = f'{self.run}_ocnanalecen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalecen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlchkpt(self):

        deps = []
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'task', 'name': f'{self.run}_ocnanalecen'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlvar'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_mergensst:
            data = f'&ROTDIR;/{self.run}.@Y@m@d/@H/atmos/{self.run}.t@Hz.sfcanl.nc'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marineanlchkpt')
        task_name = f'{self.run}_marineanlchkpt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marineanlchkpt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlchkpt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marineanlfinal')
        task_name = f'{self.run}_marineanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/marineanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalvrfy(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlfinal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalvrfy')
        task_name = f'{self.run}_ocnanalvrfy'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalvrfy.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def fcst(self):

        fcst_map = {'forecast-only': self._fcst_forecast_only,
                    'cycled': self._fcst_cycled}

        try:
            task = fcst_map[self.app_config.mode]()
        except KeyError:
            raise NotImplementedError(f'{self.app_config.mode} is not a valid type.\n'
                                      f'Currently supported forecast types are:\n'
                                      f'{" | ".join(fcst_map.keys())}')

        return task

    def _fcst_forecast_only(self):
        dependencies = []

        dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_wave and self.run in self.app_config.wave_runs:
            wave_job = 'waveprep' if self.app_config.model_app in ['ATMW'] else 'waveinit'
            dep_dict = {'type': 'task', 'name': f'{self.run}_{wave_job}'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_aero and \
           self.run in self.app_config.aero_fcst_runs and \
           not self._base['EXP_WARM_START']:
            # Calculate offset based on RUN = gfs | gdas
            interval = None
            if self.run in ['gfs']:
                interval = to_timedelta(f"{self._base['INTERVAL_GFS']}H")
            elif self.run in ['gdas']:
                interval = self._base['assim_freq']
            offset = timedelta_to_HMS(-interval)
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.run}_aerosol_init'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': offset}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies.append(rocoto.create_dependency(dep_condition='or', dep=deps))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        if self.run in ['gfs']:
            num_fcst_segments = len(self.app_config.fcst_segments) - 1
        else:
            num_fcst_segments = 1

        fcst_vars = self.envars.copy()
        fcst_envars_dict = {'FCST_SEGMENT': '#seg#'}
        for key, value in fcst_envars_dict.items():
            fcst_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('fcst')
        task_name = f'{self.run}_fcst_seg#seg#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': fcst_vars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
        metatask_dict = {'task_name': f'{self.run}_fcst',
                         'is_serial': True,
                         'var_dict': seg_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def _fcst_cycled(self):

        dep_dict = {'type': 'task', 'name': f'{self.run}_sfcanl'}
        dep = rocoto.add_dependency(dep_dict)
        dependencies = rocoto.create_dependency(dep=dep)

        if self.app_config.do_jediocnvar:
            dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlfinal'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_aero and self.run in self.app_config.aero_anl_runs:
            dep_dict = {'type': 'task', 'name': f'{self.run}_aeroanlfinal'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_jedisnowda:
            dep_dict = {'type': 'task', 'name': f'{self.run}_snowanl'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        if self.run in ['gdas']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        if self.app_config.do_wave and self.run in self.app_config.wave_runs:
            dep_dict = {'type': 'task', 'name': f'{self.run}_waveprep'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run

        if self.run in ['gfs']:
            num_fcst_segments = len(self.app_config.fcst_segments) - 1
        else:
            num_fcst_segments = 1

        fcst_vars = self.envars.copy()
        fcst_envars_dict = {'FCST_SEGMENT': '#seg#'}
        for key, value in fcst_envars_dict.items():
            fcst_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('fcst')
        task_name = f'{self.run}_fcst_seg#seg#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': fcst_vars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
        metatask_dict = {'task_name': f'{self.run}_fcst',
                         'is_serial': True,
                         'var_dict': seg_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def atmanlupp(self):
        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '000',
                          'UPP_RUN': 'analysis'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_anl_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_ANALYSIS_TMPL"])
        deps = []
        data = f'{atm_anl_path}/{self.run}.t@Hz.atmanl.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_anl_path}/{self.run}.t@Hz.sfcanl.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_anl_path}/{self.run}.t@Hz.loganl.txt'
        dep_dict = {'type': 'data', 'data': data, 'age': 60}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')
        resources = self.get_resource('upp')
        task_name = f'{self.run}_atmanlupp'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/upp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlprod(self):
        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '-001'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_master_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_MASTER_TMPL"])
        deps = []
        data = f'{atm_master_path}/{self.run}.t@Hz.master.grb2anl'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        resources = self.get_resource('atmos_products')
        task_name = f'{self.run}_atmanlprod'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmos_products.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmupp(self):
        return self._upptask(upp_run='forecast', task_id='atmupp')

    def goesupp(self):
        return self._upptask(upp_run='goes', task_id='goesupp')

    def _upptask(self, upp_run="forecast", task_id="atmupp"):

        VALID_UPP_RUN = ["forecast", "goes", "wafs"]
        if upp_run not in VALID_UPP_RUN:
            raise KeyError(f"{upp_run} is invalid; UPP_RUN options are: {('|').join(VALID_UPP_RUN)}")

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '#fhr#',
                          'UPP_RUN': upp_run}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        deps = []
        data = f'{atm_hist_path}/{self.run}.t@Hz.atmf#fhr#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.run}.t@Hz.sfcf#fhr#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.run}.t@Hz.atm.logf#fhr#.txt'
        dep_dict = {'type': 'data', 'data': data, 'age': 60}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')
        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run
        resources = self.get_resource('upp')

        task_name = f'{self.run}_{task_id}_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/upp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        fhrs = self._get_forecast_hours(self.run, self._configs['upp'])
        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}

        metatask_dict = {'task_name': f'{self.run}_{task_id}',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def atmos_prod(self):
        return self._atmosoceaniceprod('atmos')

    def ocean_prod(self):
        return self._atmosoceaniceprod('ocean')

    def ice_prod(self):
        return self._atmosoceaniceprod('ice')

    def _atmosoceaniceprod(self, component: str):

        products_dict = {'atmos': {'config': 'atmos_products',
                                   'history_path_tmpl': 'COM_ATMOS_MASTER_TMPL',
                                   'history_file_tmpl': f'{self.run}.t@Hz.master.grb2f#fhr#'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.run}.ocean.t@Hz.6hr_avg.f#fhr_next#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.run}.ice.t@Hz.6hr_avg.f#fhr#.nc'}}

        component_dict = products_dict[component]
        config = component_dict['config']
        history_path_tmpl = component_dict['history_path_tmpl']
        history_file_tmpl = component_dict['history_file_tmpl']

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '#fhr#', 'COMPONENT': component}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        history_path = self._template_to_rocoto_cycstring(self._base[history_path_tmpl])
        deps = []
        data = f'{history_path}/{history_file_tmpl}'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='or')

        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run
        resources = self.get_resource(component_dict['config'])

        task_name = f'{self.run}_{component}_prod_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': cycledef,
                     'command': f"{self.HOMEgfs}/jobs/rocoto/{config}.sh",
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        fhrs = self._get_forecast_hours(self.run, self._configs[config], component)

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice'] and 0 in fhrs:
            fhrs.remove(0)

        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}
        if component in ['ocean']:
            fhrs_next = fhrs[1:] + [fhrs[-1] + (fhrs[-1] - fhrs[-2])]
            fhr_var_dict['fhr_next'] = ' '.join([f"{fhr:03d}" for fhr in fhrs_next])
        metatask_dict = {'task_name': f'{self.run}_{component}_prod',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict}

        task = rocoto.create_task(metatask_dict)

        return task

    def wavepostsbs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostsbs')
        task_name = f'{self.run}_wavepostsbs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostsbs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpnt')
        task_name = f'{self.run}_wavepostbndpnt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavepostbndpntbll(self):

        # The wavepostbndpntbll job runs on forecast hours up to FHMAX_WAV_IBP
        last_fhr = self._configs['wave']['FHMAX_WAV_IBP']

        deps = []
        atmos_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        data = f'{atmos_hist_path}/{self.run}.t@Hz.atm.logf{last_fhr:03d}.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpntbll')
        task_name = f'{self.run}_wavepostbndpntbll'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpntbll.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavepostpnt(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_wave_bnd:
            dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpntbll'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('wavepostpnt')
        task_name = f'{self.run}_wavepostpnt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavegempak(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavegempak')
        task_name = f'{self.run}_wavegempak'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavegempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveawipsbulls(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostpnt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('waveawipsbulls')
        task_name = f'{self.run}_waveawipsbulls'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveawipsbulls.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveawipsgridded(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('waveawipsgridded')
        task_name = f'{self.run}_waveawipsgridded'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveawipsgridded.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def postsnd(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('postsnd')
        task_name = f'{self.run}_postsnd'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/postsnd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def fbwind(self):

        atmos_prod_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_GRIB_GRID_TMPL"], {'RUN': self.run, 'GRID': '0p25'})
        deps = []
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pgrb2.0p25.f006'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pgrb2.0p25.f012'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pgrb2.0p25.f024'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('awips')
        # TODO: It would be better to use task dependencies on the
        # individual post jobs rather than data dependencies to avoid
        # prematurely starting with partial files. Unfortunately, the
        # ability to "group" post would make this more convoluted than
        # it should be and not worth the complexity.
        task_name = f'{self.run}_fbwind'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fbwind.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    @staticmethod
    def _get_awipsgroups(run, config):

        fhmin = config['FHMIN']
        fhmax = config['FHMAX']
        fhout = config['FHOUT']

        # Get a list of all forecast hours
        fhrs = []
        if run in ['gdas']:
            fhrs = range(fhmin, fhmax + fhout, fhout)
        elif run in ['gfs']:
            fhmax = config['FHMAX_GFS']
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
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self.rotdir}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_awipsgroups(self.run, self._configs['awips'])
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')

        task_name = f'{self.run}_awips_20km_1p0deg#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': awipsenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/awips_20km_1p0deg.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_awips_20km_1p0deg',
                         'task_dict': task_dict,
                         'var_dict': var_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def gempak(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmos_prod_f#fhr#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        gempak_vars = self.envars.copy()
        gempak_dict = {'FHR3': '#fhr#'}
        for key, value in gempak_dict.items():
            gempak_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('gempak')
        task_name = f'{self.run}_gempak_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': gempak_vars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        fhrs = self._get_forecast_hours(self.run, self._configs['gempak'])
        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}

        fhr_metatask_dict = {'task_name': f'{self.run}_gempak',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        task = rocoto.create_task(fhr_metatask_dict)

        return task

    def gempakmeta(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempak'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.run}_gempakmeta'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakmeta.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakmetancdc(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempak'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.run}_gempakmetancdc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakmetancdc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakncdcupapgif(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempak'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.run}_gempakncdcupapgif'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakncdcupapgif.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakpgrb2spec(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_npoess_pgrb2_0p5deg'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        gempak_vars = self.envars.copy()
        gempak_dict = {'FHR3': '#fhr#'}
        for key, value in gempak_dict.items():
            gempak_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('gempak')
        task_name = f'{self.run}_gempakgrb2spec_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': gempak_vars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakgrb2spec.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        # Override forecast lengths locally to be that of gempak goes job
        local_config = self._configs['gempak']
        goes_times = {
            'FHMAX_HF_GFS': 0,
            'FHMAX_GFS': local_config['FHMAX_GOES'],
            'FHOUT_GFS': local_config['FHOUT_GOES'],
        }
        local_config.update(goes_times)

        fhrs = self._get_forecast_hours(self.run, local_config)
        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}

        fhr_metatask_dict = {'task_name': f'{self.run}_gempakgrb2spec',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        task = rocoto.create_task(fhr_metatask_dict)

        return task

    def npoess_pgrb2_0p5deg(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_goesupp'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('npoess')
        task_name = f'{self.run}_npoess_pgrb2_0p5deg'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/npoess.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def verfozn(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfozn')
        task_name = f'{self.run}_verfozn'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/verfozn.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def verfrad(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfrad')
        task_name = f'{self.run}_verfrad'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/verfrad.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def vminmon(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('vminmon')
        task_name = f'{self.run}_vminmon'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/vminmon.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def tracker(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('tracker')
        task_name = f'{self.run}_tracker'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/tracker.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def genesis(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis')
        task_name = f'{self.run}_genesis'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/genesis.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def genesis_fsu(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis_fsu')
        task_name = f'{self.run}_genesis_fsu'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/genesis_fsu.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def fit2obs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('fit2obs')
        task_name = f'{self.run}_fit2obs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fit2obs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def metp(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_arch'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.interval_gfs < to_timedelta('24H'):
            n_lookback = self.app_config.interval_gfs // to_timedelta('6H')
            for lookback in range(1, n_lookback + 1):
                deps2 = []
                dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_arch', 'condition': 'not'}
                deps2.append(rocoto.add_dependency(dep_dict))
                for lookback2 in range(1, lookback):
                    offset = timedelta_to_HMS(-to_timedelta(f'{6*lookback2}H'))
                    dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': offset}
                    deps2.append(rocoto.add_dependency(dep_dict))

                offset = timedelta_to_HMS(-to_timedelta(f'{6*lookback}H'))
                dep_dict = {'type': 'task', 'name': f'{self.run}_arch', 'offset': offset}
                deps2.append(rocoto.add_dependency(dep_dict))
                deps.append(rocoto.create_dependency(dep_condition='and', dep=deps2))

        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        metpenvars = self.envars.copy()
        metpenvar_dict = {'SDATE_GFS': self._base.get('SDATE_GFS').strftime("%Y%m%d%H"),
                          'EDATE_GFS': self._base.get('EDATE').strftime("%Y%m%d%H")}
        metpenvar_dict['METPCASE'] = '#metpcase#'
        for key, value in metpenvar_dict.items():
            metpenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1 = 'metpcase'
        varval1 = 'g2g1 g2o1 pcp1'
        var_dict = {varname1: varval1}

        resources = self.get_resource('metp')

        task_name = f'{self.run}_metp#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': metpenvars,
                     'cycledef': 'metp,last_gfs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/metp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_metp',
                         'is_serial': True,
                         'task_dict': task_dict,
                         'var_dict': var_dict,
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def mos_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prep')
        task_name = f'{self.run}_mos_stn_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_grd_prep')
        task_name = f'{self.run}_mos_grd_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_stn_prep')
        task_name = f'{self.run}_mos_ext_stn_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_grd_prep')
        task_name = f'{self.run}_mos_ext_grd_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_stn_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_fcst')
        task_name = f'{self.run}_mos_stn_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_grd_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_grd_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_grd_fcst')
        task_name = f'{self.run}_mos_grd_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_stn_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_stn_fcst')
        task_name = f'{self.run}_mos_ext_stn_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_grd_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_grd_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_grd_fcst')
        task_name = f'{self.run}_mos_ext_grd_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_stn_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prdgen')
        task_name = f'{self.run}_mos_stn_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_grd_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_grd_prdgen')
        task_name = f'{self.run}_mos_grd_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_stn_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_stn_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_stn_prdgen')
        task_name = f'{self.run}_mos_ext_stn_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_grd_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_grd_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_stn_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_ext_grd_prdgen')
        task_name = f'{self.run}_mos_ext_grd_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_wx_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_wx_prdgen')
        task_name = f'{self.run}_mos_wx_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_wx_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_wx_ext_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_ext_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_mos_wx_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('mos_wx_ext_prdgen')
        task_name = f'{self.run}_mos_wx_ext_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_wx_ext_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def arch(self):
        deps = []
        if self.app_config.mode in ['cycled']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_vminmon:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                    deps.append(rocoto.add_dependency(dep_dict))
            elif self.run in ['gdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_fit2obs:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_fit2obs'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_verfozn:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_verfozn'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_verfrad:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_verfrad'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_vminmon:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                    deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.app_config.do_tracker:
            dep_dict = {'type': 'task', 'name': f'{self.run}_tracker'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.app_config.do_genesis:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.app_config.do_genesis_fsu:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis_fsu'}
            deps.append(rocoto.add_dependency(dep_dict))
        # Post job dependencies
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_wave:
            dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostsbs'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostpnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.app_config.do_wave_bnd:
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_ocean:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_ice:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        # MOS job dependencies
        if self.run in ['gfs'] and self.app_config.do_mos:
            mos_jobs = ["stn_prep", "grd_prep", "ext_stn_prep", "ext_grd_prep",
                        "stn_fcst", "grd_fcst", "ext_stn_fcst", "ext_grd_fcst",
                        "stn_prdgen", "grd_prdgen", "ext_stn_prdgen", "ext_grd_prdgen",
                        "wx_prdgen", "wx_ext_prdgen"]
            for job in mos_jobs:
                dep_dict = {'type': 'task', 'name': f'{self.run}_mos_{job}'}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('arch')
        task_name = f'{self.run}_arch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/arch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    # Cleanup
    def cleanup(self):
        deps = []
        if 'enkf' in self.run:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_eamn'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_arch'}
            deps.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_gempak:
            if self.run in ['gdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmetancdc'}
                deps.append(rocoto.add_dependency(dep_dict))
            elif self.run in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmeta'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_gempakncdcupapgif'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.do_goes:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempakgrb2spec'}
                    deps.append(rocoto.add_dependency(dep_dict))
                    dep_dict = {'type': 'task', 'name': f'{self.run}_npoess_pgrb2_0p5deg'}
                    deps.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_metp and self.run in ['gfs']:
            deps2 = []
            # taskvalid only handles regular tasks, so just check the first metp job exists
            dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_metpg2g1', 'condition': 'not'}
            deps2.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_metp'}
            deps2.append(rocoto.add_dependency(dep_dict))
            deps.append(rocoto.create_dependency(dep_condition='or', dep=deps2))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('cleanup')
        task_name = f'{self.run}_cleanup'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/cleanup.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    # Start of ensemble tasks
    def eobs(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('eobs')
        task_name = f'{self.run}_eobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def eomg(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eomgenvars = self.envars.copy()
        eomgenvars_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#'
                           }
        for key, value in eomgenvars_dict.items():
            eomgenvars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('eomg')
        task_name = f'{self.run}_eomg_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': eomgenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eomg.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(1, self.nmem + 1)])}
        metatask_dict = {'task_name': f'{self.run}_eomg',
                         'var_dict': member_var_dict,
                         'task_dict': task_dict,
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def ediag(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ediag')
        task_name = f'{self.run}_ediag'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ediag.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def eupd(self):
        deps = []
        if self.app_config.lobsdiag_forenkf:
            dep_dict = {'type': 'task', 'name': f'{self.run}_ediag'}
        else:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_eomg'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('eupd')
        task_name = f'{self.run}_eupd'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eupd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlinit(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_prepatmiodaobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = "gdas"
        resources = self.get_resource('atmensanlinit')
        task_name = f'{self.run}_atmensanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlobs')
        task_name = f'{self.run}_atmensanlobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlsol(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlsol')
        task_name = f'{self.run}_atmensanlsol'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlsol.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlletkf(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlletkf')
        task_name = f'{self.run}_atmensanlletkf'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlletkf.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlfv3inc(self):

        deps = []
        if self.app_config.lobsdiag_forenkf:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlsol'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlletkf'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlfv3inc')
        task_name = f'{self.run}_atmensanlfv3inc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlfv3inc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlfv3inc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmensanlfinal')
        task_name = f'{self.run}_atmensanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ecen(self):

        def _get_ecengroups():

            if self._base.get('DOIAU_ENKF', False):
                fhrs = self._base.get('IAUFHRS', '[6]')

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
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jediatmens:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        ecenenvars = self.envars.copy()
        ecenenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in ecenenvar_dict.items():
            ecenenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_ecengroups()
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        resources = self.get_resource('ecen')

        task_name = f'{self.run}_ecen#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': ecenenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ecen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_ecmn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)
        return task

    def esfc(self):

        # eupd_run = 'gdas' if 'gdas' in self.app_config.eupd_runs else 'gfs'

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf","")}_analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jediatmens:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jedisnowda:
            dep_dict = {'type': 'task', 'name': f'{self.run}_esnowrecen'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('esfc')
        task_name = f'{self.run}_esfc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/esfc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def efcs(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_ecmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_esfc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        efcsenvars = self.envars.copy()
        efcsenvars_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#'
                           }
        for key, value in efcsenvars_dict.items():
            efcsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        cycledef = 'gdas_half,gdas' if self.run in ['enkfgdas'] else self.run.replace('enkf', '')
        resources = self.get_resource('efcs')

        task_name = f'{self.run}_fcst_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': efcsenvars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(1, self.nmem + 1)])}
        metatask_dict = {'task_name': f'{self.run}_fcst',
                         'var_dict': member_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def echgres(self):

        self._is_this_a_gdas_task(self.run, 'echgres')

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run.replace("enkf","")}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_fcst_mem001'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = 'gdas_half,gdas' if self.run in ['enkfgdas'] else self.run

        resources = self.get_resource('echgres')
        task_name = f'{self.run}_echgres'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/echgres.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def epos(self):

        def _get_eposgroups(epos):
            fhmin = epos['FHMIN_ENKF']
            fhmax = epos['FHMAX_ENKF']
            fhout = epos['FHOUT_ENKF']
            if self.run == "enkfgfs":
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
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eposenvars = self.envars.copy()
        eposenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in eposenvar_dict.items():
            eposenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_eposgroups(self._configs['epos'])
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        cycledef = 'gdas_half,gdas' if self.run in ['enkfgdas'] else self.run.replace('enkf', '')

        resources = self.get_resource('epos')

        task_name = f'{self.run}_epos#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': eposenvars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/epos.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_epmn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def earc(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_epmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        # Integer division is floor division, but we need ceiling division
        n_groups = -(self.nmem // -self._configs['earc']['NMEM_EARCGRP'])
        groups = ' '.join([f'{grp:02d}' for grp in range(0, n_groups + 1)])

        resources = self.get_resource('earc')

        var_dict = {'grp': groups}

        task_name = f'{self.run}_earc#grp#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': earcenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/earc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_eamn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task
