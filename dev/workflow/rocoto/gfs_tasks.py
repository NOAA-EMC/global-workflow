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
    def fetch(self):

        cycledef = 'gdas_half' if self.run in ['gdas', 'enkfgdas'] else self.run

        resources = self.get_resource('fetch')
        task_name = f'{self.run}_fetch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fetch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def stage_ic(self):

        dependencies = None
        if self.options['do_fetch_hpss'] or self.options['do_fetch_local']:
            deps = []
            dep_dict = {
                'type': 'task', 'name': f'{self.run}_fetch',
            }
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)

        cycledef = 'gdas_half' if self.run in ['gdas', 'enkfgdas'] else self.run

        resources = self.get_resource('stage_ic')
        task_name = f'{self.run}_stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/stage_ic.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;',
                     'dependency': dependencies
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prep_sfc(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': 'gdas_atmos_prod', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'RUN': 'gdas'})
        data = f'{atm_hist_path}/gdas.t@Hz.atm.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('prep_sfc')
        task_name = f'{self.run}_prep_sfc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/prep_sfc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prep(self):

        dump_suffix = self._base["DUMP_SUFFIX"]
        dmpdir = self._base["DMPDIR"]
        iodadir = self._base["IODADIR"]
        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'RUN': 'gdas'})
        dump_path = self._template_to_rocoto_cycstring(self._base["COM_OBSPROC_TMPL"],
                                                       {'DMPDIR': dmpdir, 'DUMP_SUFFIX': dump_suffix})
        ioda_path = self._template_to_rocoto_cycstring(self._base["COM_OBSFORGE_TMPL"],
                                                       {'IODADIR': iodadir, 'DUMP_SUFFIX': dump_suffix})

        gfs_enkf = True if self.options['do_hybvar'] and 'gfs' in self.app_config.ens_runs else False

        deps = []

        dep_dict = {'type': 'metatask', 'name': 'gdas_atmos_prod', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/gdas.t@Hz.atm.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{dump_path}/{self.run}.t@Hz.updated.status.tm00.bufr_d'
        dep_dict = {'type': 'data', 'data': data}
        if self.options['do_jediatmvar']:
            data = f'{ioda_path}/atmos/{self.run}.t@Hz.obsforge_atmos_bufr_status.log'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        # TODO enable this for marine observations when ready
        # if self.options['do_jediocnvar']:
        #     data = f'{ioda_path}/ocean/{self.run}.t@Hz.obsforge_marine_status.log'
        #     dep_dict = {'type': 'data', 'data': data}
        #     deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_aero_anl']:
            data = f'{ioda_path}/chem/{self.run}.t@Hz.obsforge_aod_status.log'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'gdas_fcst', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_prep_sfc']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_prep_sfc'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = self.run
        if self.run in ['gfs'] and gfs_enkf and self._base['INTERVAL_GFS'] != 6:
            cycledef = 'gdas'

        resources = self.get_resource('prep')
        task_name = f'{self.run}_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/prep.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/waveinit.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/waveprep.sh',
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
        ntiles = self._base['ntiles']
        files = ['gfs_ctrl.nc'] + [f'gfs_data.tile{tile}.nc' for tile in range(1, ntiles + 1)]
        for file in files:
            data = f'{input_path}/{file}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Calculate offset based on RUN = gfs | gdas
        interval = None
        if self.run in ['gfs']:
            interval = self._base['interval_gfs']
        elif self.run in ['gdas']:
            interval = self._base['interval']
        offset = timedelta_to_HMS(-interval)

        # Files from previous cycle
        files = ['@Y@m@d.@H0000.fv_core.res.nc'] + \
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/aerosol_init.sh',
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
        if self.options['do_hybvar']:
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/anal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def sfcanl(self):

        deps = []
        if self.options['do_jediatmvar']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jedisnowda']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_snowanl'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_gsisoilda'] and self.run in ['gdas']:
            dep_dict = {'type': 'task', 'name': 'enkfgdas_eupd'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jedisnowda'] or (self.options['do_gsisoilda'] and self.run in ['gdas']):
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/sfcanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def analcalc(self):

        deps = []
        if self.options['do_jediatmvar'] and not self.options['do_jediatmens']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_sfcanl'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_hybvar'] and self.run in ['gdas']:
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/analcalc.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/analdiag.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_hybvar']:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        interval_gfs = self._base["INTERVAL_GFS"]
        gfs_enkf = True if self.options['do_hybvar'] and 'gfs' in self.app_config.ens_runs else False

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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmanlinit.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmanlvar.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmanlfv3inc.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmanlfinal.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/aeroanlgenb.sh',
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

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlinit')
        task_name = f'{self.run}_aeroanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/aeroanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlvar(self):

        deps = []
        dep_dict = {
            'type': 'task', 'name': 'gdas_aeroanlgenb',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/aeroanlvar.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/aeroanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def snowanl(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('snowanl')
        task_name = f'{self.run}_snowanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/snowanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)
        return task

    def esnowanl(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f"{self.run.replace('enkf', '')}_prep"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('esnowanl')
        task_name = f'{self.run}_esnowanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/esnowanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)
        return task

    def prepoceanobs(self):

        ocean_hist_path = self._template_to_rocoto_cycstring(self._base["COM_OCEAN_HISTORY_TMPL"], {'RUN': 'gdas'})
        dmpdir = self._configs['prepoceanobs']["DMPDIR"]

        deps = []
        data = f'{ocean_hist_path}/gdas.t@Hz.inst.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{dmpdir}/{self.run}.@Y@m@d/@H/ocean/{self.run}.t@Hz.obsforge_marine_status.log'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{dmpdir}/{self.run}.@Y@m@d/@H/ocean/insitu/{self.run}.t@Hz.obsforge_marine_bufr_status.log'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('prepoceanobs')
        task_name = f'{self.run}_prepoceanobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/prepoceanobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlletkf(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f"{self.run.replace('enkf', '')}_prepoceanobs"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f"{self.run.replace('enkf', '')}_marinebmat"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marineanlletkf')
        task_name = f'{self.run}_marineanlletkf'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlletkf.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marinebmatinit(self):

        ocean_hist_path = self._template_to_rocoto_cycstring(self._base["COM_OCEAN_HISTORY_TMPL"], {'RUN': 'gdas'})

        deps = []
        data = f'{ocean_hist_path}/gdas.t@Hz.inst.f009.nc'
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_hybvar_ocn']:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdas_fcst', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('marinebmatinit')
        task_name = f'{self.run}_marinebmatinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marinebmatinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marinebmat(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marinebmatinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('marinebmat')
        task_name = f'{self.run}_marinebmat'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marinebmat.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlinit.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlvar.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlecen(self):

        # can run in parallel with marinebmat
        deps = []
        dep_dict = {'type': 'task', 'name': f"{self.run.replace('enkf', '')}_marinebmatinit"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('marineanlecen')
        task_name = f'{self.run}_marineanlecen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlecen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def marineanlchkpt(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlvar'}
        deps.append(rocoto.add_dependency(dep_dict))
        # if DOHYBVAR_OCN: "YES" and EUPD_CYC: "both"
        if self.options['do_hybvar_ocn'] and \
                (('gfs' in self.app_config.ens_runs and
                 'gdas' in self.app_config.ens_runs) or
                 self.run == "gdas"):
            dep_dict = {'type': 'task', 'name': f'enkf{self.run}_marineanlecen'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_mergensst']:
            data = f'&ROTDIR;/{self.run}.@Y@m@d/@H/atmos/{self.run}.t@Hz.analysis.sfc.a006.nc'
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlchkpt.sh',
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
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('marineanlfinal')
        task_name = f'{self.run}_marineanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/marineanlfinal.sh',
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

        if self.options['do_wave']:
            wave_job = 'waveprep' if self.options['app'] in ['ATMW'] else 'waveinit'
            dep_dict = {'type': 'task', 'name': f'{self.run}_{wave_job}'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_fcst'] and not self._base['EXP_WARM_START']:
            # Calculate offset based on RUN = gfs | gdas
            interval = None
            if self.run in ['gfs']:
                interval = self._base['interval_gfs']
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
            num_fcst_segments = len(self.options['fcst_segments']) - 1
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fcst.sh',
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

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_sfcanl'}
        deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            wave_job = 'waveprep' if self.options['app'] in ['ATMW'] else 'waveinit'
            dep_dict = {'type': 'task', 'name': f'{self.run}_{wave_job}'}
            deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_jediocnvar']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_anl']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_aeroanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_jedisnowda']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_snowanl'}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies1 = rocoto.create_dependency(dep_condition='and', dep=deps)

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
        deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            wave_job = 'waveprep' if self.options['app'] in ['ATMW'] else 'waveinit'
            dep_dict = {'type': 'task', 'name': f'{self.run}_{wave_job}'}
            deps.append(rocoto.add_dependency(dep_dict))

        dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))

        dependencies2 = rocoto.create_dependency(dep_condition='and', dep=deps)

        dependencies = []
        dependencies.append(dependencies1)
        dependencies.append(dependencies2)
        dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        cycledef = 'gdas_half,gdas' if self.run in ['gdas'] else self.run

        if self.run in ['gfs']:
            num_fcst_segments = len(self.options['fcst_segments']) - 1
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fcst.sh',
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
        if self.options['do_jediatmvar'] and self.options['do_jediatmens']:
            data = f'{atm_anl_path}/{self.run}.t@Hz.jedi_analysis.atm.a006.nc'
        else:
            data = f'{atm_anl_path}/{self.run}.t@Hz.analysis.atm.a006.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jediatmvar'] and self.options['do_jediatmens']:
            data = f'{atm_anl_path}/{self.run}.t@Hz.jedi_analysis.sfc.a006.nc'
        else:
            data = f'{atm_anl_path}/{self.run}.t@Hz.analysis.sfc.a006.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_anl_path}/{self.run}.t@Hz.analysis.done.txt'
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/upp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlprod(self):
        postenvars = self.envars.copy()
        postenvar_dict = {'FHR_LIST': '-1'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_master_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_MASTER_TMPL"])
        deps = []
        data = f'{atm_master_path}/{self.run}.t@Hz.master.analysis.grib2'
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmos_products.sh',
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

        VALID_UPP_RUN = ["forecast", "goes"]
        if upp_run not in VALID_UPP_RUN:
            raise KeyError(f"{upp_run} is invalid; UPP_RUN options are: {('|').join(VALID_UPP_RUN)}")

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '#fhr#',
                          'UPP_RUN': upp_run}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        atm_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        deps = []
        data = f'{atm_hist_path}/{self.run}.t@Hz.atm.f#fhr#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.run}.t@Hz.sfc.f#fhr#.nc'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atm_hist_path}/{self.run}.t@Hz.log.f#fhr#.txt'
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/upp.sh',
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
                                   'history_path_tmpl': 'COM_ATMOS_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.run}.t@Hz.log.f#fhr3_last#.txt'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.run}.t@Hz.6hr_avg.f#fhr3_nextp1#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.run}.t@Hz.6hr_avg.f#fhr3_last#.nc'}}

        component_dict = products_dict[component]
        config = component_dict['config']
        history_path_tmpl = component_dict['history_path_tmpl']
        history_file_tmpl = component_dict['history_file_tmpl']

        max_tasks = self._configs[config]['MAX_TASKS']
        resources = self.get_resource(component_dict['config'])

        fhrs = self._get_forecast_hours(self.run, self._configs[config], component)

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice'] and 0 in fhrs:
            fhrs.remove(0)

        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

        # Delay triggering ocean products task to next next forecast hour to ensure all data is available
        if component == 'ocean':
            fhr3_next = fhr_var_dict['fhr3_next'].split(' ')
            fhr3_nextp1 = fhr3_next[1:]
            fhr3_nextp1.append(fhr3_next[-1])  # repeat last forecast hour to maintain same number of groups
            fhr_var_dict['fhr3_nextp1'] = ' '.join(fhr3_nextp1)

        # Adjust walltime based on the largest group
        largest_group = max([len(grp.split(',')) for grp in fhr_var_dict['fhr_list'].split(' ')])
        resources['walltime'] = Tasks.multiply_HMS(resources['walltime'], largest_group)

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR_LIST': '#fhr_list#', 'COMPONENT': component}
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

        task_name = f'{self.run}_{component}_prod_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': cycledef,
                     'command': f"{self.HOMEgfs}/dev/job_cards/rocoto/{config}.sh",
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_{component}_prod',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict}

        task = rocoto.create_task(metatask_dict)

        return task

    def wavepostsbs(self):

        wave_grid = self._configs['base']['waveGRD']
        history_path = self._template_to_rocoto_cycstring(self._base['COM_WAVE_HISTORY_TMPL'])
        history_file = f'{self.run}.t@Hz.{wave_grid}.f#fhr3_last#.log'

        deps = []
        dep_dict = {'type': 'data', 'data': f'{history_path}/{history_file}'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        fhrs = self._get_forecast_hours(self.run, self._configs['wavepostsbs'], 'wave')
        max_tasks = self._configs['wavepostsbs']['MAX_TASKS']
        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

        wave_post_envars = self.envars.copy()
        postenvar_dict = {'FHR_LIST': '#fhr_list#'}
        for key, value in postenvar_dict.items():
            wave_post_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostsbs')
        # Adjust walltime based on the largest group
        largest_group = max([len(grp.split(',')) for grp in fhr_var_dict['fhr_list'].split(' ')])
        resources['walltime'] = Tasks.multiply_HMS(resources['walltime'], largest_group)

        task_name = f'{self.run}_wavepostsbs_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/wavepostsbs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_wavepostsbs',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict}

        task = rocoto.create_task(metatask_dict)

        return task

    def wavepostbndpnt(self):
        return self._wavepostbndpnt('wavepostbndpnt')

    def wavepostbndpntbll(self):
        return self._wavepostbndpnt('wavepostbndpntbll')

    def _wavepostbndpnt(self, name_in):

        # The job runs on forecast hours up to FHMAX_WAV_IBP
        fhmax_wav_ibp = self._configs[name_in]['FHMAX_WAV_IBP']
        history_path = self._template_to_rocoto_cycstring(self._base['COM_WAVE_HISTORY_TMPL'])
        history_file = f'{self.run}.t@Hz.points.f{fhmax_wav_ibp:03d}.log'

        deps = []
        dep_dict = {'type': 'data', 'data': f'{history_path}/{history_file}'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource(name_in)
        task_name = f'{self.run}_{name_in}'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/{name_in}.sh',
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
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostpnt')
        task_name = f'{self.run}_wavepostpnt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/wavepostpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavegempak(self):

        # wave_gempak tasks depend on wave_postsbs tasks
        # wave_postsbs runs on different forecast hours than wave_gempak,
        # so we need to get the forecast hours for wave_postsbs and wave_gempak separately

        # Get the forecast hours for wave_postsbs
        dep_fhrs = self._get_forecast_hours(self.run, self._configs['wavepostsbs'], 'wave')
        dep_max_tasks = self._configs['wavepostsbs']['MAX_TASKS']
        dep_fhr_var_dict = self.get_grouped_fhr_dict(fhrs=dep_fhrs, ngroups=dep_max_tasks)

        # Get the forecast hours for wave_gempak
        fhrs = self._get_forecast_hours(self.run, self._configs['wavegempak'], 'wave')
        max_tasks = self._configs['wavegempak']['MAX_TASKS']
        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

        # Get the right dependency labels for wave_gempak on wave_postsbs groups
        fhr_var_dict = self.get_dep_fhr_label(fhr_var_dict, dep_fhr_var_dict)

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostsbs_#dep_fhr_label#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        wave_post_envars = self.envars.copy()
        postenvar_dict = {'FHR_LIST': '#fhr_list#'}
        for key, value in postenvar_dict.items():
            wave_post_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavegempak')
        # Adjust walltime based on the largest group
        largest_group = max([len(grp.split(',')) for grp in fhr_var_dict['fhr_list'].split(' ')])
        resources['walltime'] = Tasks.multiply_HMS(resources['walltime'], largest_group)

        task_name = f'{self.run}_wavegempak_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/wavegempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_wavegempak',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict}

        task = rocoto.create_task(metatask_dict)

        return task

    def waveawipsbulls(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavepostsbs'}
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/waveawipsbulls.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveawipsgridded(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('waveawipsgridded')
        task_name = f'{self.run}_waveawipsgridded'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/waveawipsgridded.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/postsnd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def fbwind(self):

        atmos_prod_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_GRIB_GRID_TMPL"], {'RUN': self.run, 'GRID': '0p25'})
        deps = []
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pres_a.0p25.f006.grib2'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pres_a.0p25.f012.grib2'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'{atmos_prod_path}/{self.run}.t@Hz.pres_a.0p25.f024.grib2'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('fbwind')
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fbwind.sh',
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

        nawipsgrp = config['MAX_TASKS']
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/awips_20km_1p0deg.sh',
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

        # gempak tasks depend on atmos_prod tasks
        # atmos_prod runs on different forecast hours than gempak,
        # so we need to get the forecast hours for atmos_prod and gempak separately

        # Get the forecast hours for wave_postsbs
        dep_fhrs = self._get_forecast_hours(self.run, self._configs['atmos_products'])
        dep_max_tasks = self._configs['atmos_products']['MAX_TASKS']
        dep_fhr_var_dict = self.get_grouped_fhr_dict(fhrs=dep_fhrs, ngroups=dep_max_tasks)

        # Get the forecast hours for gempak
        fhrs = self._get_forecast_hours(self.run, self._configs['gempak'])
        max_tasks = self._configs['gempak']['MAX_TASKS']
        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

        # Get the right dependency labels for gempak on atmos_prod groups
        fhr_var_dict = self.get_dep_fhr_label(fhr_var_dict, dep_fhr_var_dict)

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmos_prod_#dep_fhr_label#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        # Adjust walltime based on the largest group
        largest_group = max([len(grp.split(',')) for grp in fhr_var_dict['fhr_list'].split(' ')])
        resources['walltime'] = Tasks.multiply_HMS(resources['walltime'], largest_group)

        gempak_vars = self.envars.copy()
        gempak_dict = {'FHR_LIST': '#fhr_list#'}
        for key, value in gempak_dict.items():
            gempak_vars.append(rocoto.create_envar(name=key, value=str(value)))

        task_name = f'{self.run}_gempak_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': gempak_vars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/gempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/gempakmeta.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/gempakmetancdc.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/gempakncdcupapgif.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/gempakgrb2spec.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/npoess.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/verfozn.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/verfrad.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/vminmon.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def anlstat(self):
        deps = []
        if self.options['do_jediatmvar']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_analdiag'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jediocnvar']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jedisnowda']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_snowanl'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_aero_anl']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_aeroanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('anlstat')
        task_name = f'{self.run}_anlstat'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/anlstat.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/tracker.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/genesis.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/genesis_fsu.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fit2obs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def metp(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
        interval_gfs = self._base.get('interval_gfs')
        assim_freq = self._base['assim_freq']

        if interval_gfs < to_timedelta("24H"):
            n_lookback = int(interval_gfs // to_timedelta(f"{assim_freq}H")) - 1
            # Check if the previous up to `n_lookback` arch_vrfy tasks have completed
            # For interval=6, there are no lookbacks
            # For interval=12, check lookback=1
            # For interval=18, check lookback=1,2
            # Only lookback if arch_vrfy is not valid for this cycle
            if n_lookback > 0:
                dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_arch_vrfy', 'condition': 'not'}
                deps2 = []
                deps2.append(rocoto.add_dependency(dep_dict))
                deps3 = []
                for lookback in range(n_lookback):
                    offset = timedelta_to_HMS(-to_timedelta(f'{assim_freq * (lookback + 1)}H'))
                    dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy', 'offset': offset}
                    deps3.append(rocoto.add_dependency(dep_dict))

                deps2.append(rocoto.create_dependency(dep=deps3, dep_condition='or'))
                deps.append(rocoto.create_dependency(dep=deps2, dep_condition='and'))

        # Lastly, check that the last arch_vrfy job is done
        # This only happens if the metp cycle is not aligned with the last_gfs cycle
        sdate_gfs = self._base.get('SDATE_GFS')
        edate = self._base.get('EDATE')
        edate_metp = self._base.get('EDATE').replace(hour=(24 - assim_freq))
        n_intervals = int((edate - sdate_gfs) // interval_gfs)
        edate_gfs = sdate_gfs + n_intervals * interval_gfs
        metp_gfs_offset = edate_metp - edate_gfs
        if metp_gfs_offset > to_timedelta("0H") and metp_gfs_offset < to_timedelta("24H"):
            deps2 = []
            dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_arch_vrfy', 'condition': 'not'}
            deps2.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy', 'offset': timedelta_to_HMS(-metp_gfs_offset)}
            deps2.append(rocoto.add_dependency(dep_dict))
            for i in range(1, int((metp_gfs_offset.seconds / 3600) // assim_freq)):
                dep_dict = {'type': 'cycleexist', 'offset': timedelta_to_HMS(-to_timedelta(f'{assim_freq * i}H')), 'condition': 'not'}
                deps2.append(rocoto.add_dependency(dep_dict))

            deps.append(rocoto.create_dependency(dep=deps2, dep_condition='and'))

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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/metp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_metp',
                         'task_dict': task_dict,
                         'var_dict': var_dict,
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def arch_vrfy(self):
        deps = []
        if self.app_config.mode in ['cycled']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
            elif self.run in ['gdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_fit2obs']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_fit2obs'}
                    deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_tracker']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_tracker'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_genesis']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_genesis_fsu']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis_fsu'}
            deps.append(rocoto.add_dependency(dep_dict))
        # Post job dependencies
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_wave']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavepostsbs'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostpnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_wave_bnd']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpntbll'}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('arch_vrfy')
        task_name = f'{self.run}_arch_vrfy'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/arch_vrfy.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def _arch_tars_deps(self):
        """Common dependencies for all archive tarball jobs"""
        deps = []
        if self.app_config.mode in ['cycled']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_vminmon']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                    deps.append(rocoto.add_dependency(dep_dict))
            elif self.run in ['gdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_fit2obs']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_fit2obs'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_verfozn']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_verfozn'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_verfrad']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_verfrad'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_vminmon']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                    deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_anlstat']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_anlstat'}
                    deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_tracker']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_tracker'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_genesis']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.run in ['gfs'] and self.options['do_genesis_fsu']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_genesis_fsu'}
            deps.append(rocoto.add_dependency(dep_dict))
        # Post job dependencies
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_wave']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavepostsbs'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostpnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_wave_bnd']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpntbll'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            if self.run in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_gempak']:
            if self.run in ['gdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmetancdc'}
                deps.append(rocoto.add_dependency(dep_dict))
            elif self.run in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmeta'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.app_config.mode in ['cycled']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_gempakncdcupapgif'}
                    deps.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_goes']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_npoess_pgrb2_0p5deg'}
                        deps.append(rocoto.add_dependency(dep_dict))
                        dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempakgrb2spec'}
                        deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_awips'] and self.run in ['gfs']:

            dep_dict = {'type': 'metatask', 'name': f'{self.run}_awips_20km_1p0deg'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_fbwind'}
            deps.append(rocoto.add_dependency(dep_dict))

            if self.options['do_wave']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_waveawipsbulls'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_waveawipsgridded'}
                deps.append(rocoto.add_dependency(dep_dict))

        if self.options['do_metp'] and self.run in ['gfs']:
            deps2 = []
            # taskvalid only handles regular tasks, so just check the first metp job exists
            dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_metpg2g1', 'condition': 'not'}
            deps2.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_metp'}
            deps2.append(rocoto.add_dependency(dep_dict))
            deps.append(rocoto.create_dependency(dep_condition='or', dep=deps2))

        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))

        return rocoto.create_dependency(dep_condition='and', dep=deps)

    def arch_tars(self):
        """Create individual archive tarball jobs for parallel execution"""

        # Split up the tarball_types based on the run and configuration options
        # Define all possible tarball types
        if self.run == 'gfs':
            tarball_types = ['gfsa', 'gfsb']

            # Add optional tarballs based on configuration
            if self._configs['arch_tars'].get('ARCH_GAUSSIAN', True):
                tarball_types.extend(['gfs_flux', 'gfs_netcdfb', 'gfs_pgrb2b'])
                if self.app_config.mode == 'cycled':
                    tarball_types.append('gfs_netcdfa')

            if self.options['do_wave']:
                tarball_types.append('gfswave')

            if self.options['do_aero_fcst']:
                tarball_types.append('chem')

            if self.options['do_ocean']:
                tarball_types.extend(['ocean_6hravg', 'ocean_native', 'gfs_flux_1p00'])
                if self.options.get('do_jediocnvar', False) and self.app_config.mode == 'cycled':
                    tarball_types.append('gfsocean_analysis')

            if self.options['do_ice']:
                tarball_types.extend(['ice_6hravg', 'ice_native'])

            if self.options['do_bufrsnd']:
                tarball_types.append('gfs_downstream')

            if self.app_config.mode == 'cycled':
                # Add restart archives (timing logic handled in template)
                tarball_types.append('gfs_restarta')

        elif self.run == 'gdas':
            tarball_types = ['gdas']

            if self.options['do_ice']:
                tarball_types.append('gdasice')

            if self.options['do_ocean']:
                tarball_types.append('gdasocean')

                if self.options['do_jediocnvar'] and self.app_config.mode == 'cycled':
                    tarball_types.append('gdasocean_analysis')

            if self.options['do_wave']:
                tarball_types.append('gdaswave')

            if self.app_config.mode == 'cycled':
                # Add restart archives (timing logic handled in template)
                tarball_types.append('gdas_restarta')
                tarball_types.append('gdas_restartb')
                if self.options['do_ice']:
                    tarball_types.append('gdasice_restart')
                if self.options['do_ocean']:
                    tarball_types.append('gdasocean_restart')
                if self.options['do_wave']:
                    tarball_types.append('gdaswave_restart')

        # Create a metatask that contains all the individual archive jobs
        dependencies = self._arch_tars_deps()

        archenvars = self.envars.copy()
        archenvars.append(rocoto.create_envar(name='TARBALL_TYPE', value='#tartype#'))

        resources = self.get_resource('arch_tars')

        task_name = f'{self.run}_arch_tar_#tartype#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': archenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/arch_tars.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        var_dict = {'tartype': ' '.join(tarball_types)}
        metatask_dict = {'task_name': f'{self.run}_arch_tars',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)
        return task

    # Globus transfer for HPSS archiving
    def globus_arch(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_arch_tars'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('globus')
        task_name = f'{self.run}_globus_arch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/globus_arch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    # Ensemble globus transfer for HPSS archiving
    def globus_earc(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_earc_tars'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        # Integer division is floor division, but we need ceiling division
        n_groups = -(self.nmem // -self._configs['earc_groups']['NMEM_EARCGRP'])
        groups = ' '.join([f'{grp:02d}' for grp in range(0, n_groups + 1)])

        resources = self.get_resource('globus')
        var_dict = {'grp': groups}

        task_name = f'{self.run}_globus_earc_#grp#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': earcenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/globus_earc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_globus_earc',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    # Cleanup
    def cleanup(self):

        # We'll need the first half cycle in YMDH for cycled dependencies
        sdate = self._base.get('SDATE')
        if self.app_config.mode in ['cycled']:
            first_half_cycle = self._base.get('SDATE').strftime("%Y%m%d%H")

        # Build a dependency on the next GFS forecast.
        # This will only be used for GDAS/ENKFGDAS cycles with 6-hourly GFS intervals
        #     to prevent clobbering files needed by the GFS forecast prematurely.
        assim_freq = self._base.get('assim_freq', 6)
        interval_gfs = int(self._base.get('INTERVAL_GFS', 0))

        # Build a dependency for the next forecast cycle
        # This only applies for the last GFS cycle
        dep_next_fcst_seg = None
        if interval_gfs >= assim_freq:
            deps = []
            dep_dict = {'type': 'task', 'name': 'gfs_fcst_seg0', 'offset':
                        f"{timedelta_to_HMS(self._base['interval_gfs'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'cycleexist', 'condition': 'not',
                        'offset': f"{timedelta_to_HMS(self._base['interval_gfs'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            # Only start checking this if we are at/past the first GFS cycle
            sdate_gfs = self._base.get('SDATE_GFS')
            if sdate_gfs:
                n_cycles = int((sdate_gfs - sdate).total_seconds() // 3600 // assim_freq)
                # Start at the first full cycle (1 cycle after SDATE)
                # End two cycles before SDATE_gfs
                #     One cycle before SDATE_gfs must depend on the next forecast segment.
                for cycle in range(1, n_cycles - 1):
                    offset = timedelta_to_HMS(to_timedelta(f'{cycle * assim_freq}H'))
                    skip_date = (sdate + to_timedelta(f'{cycle * assim_freq}H')).strftime("%Y%m%d%H")
                    dep_dict = {'type': 'streq', 'left': '@Y@m@d@H', 'right': skip_date}
                    deps.append(rocoto.add_dependency(dep_dict))

            dep_next_fcst_seg = rocoto.create_dependency(dep_condition='or', dep=deps)

        # Now start building RUN-specific dependencies
        # Full-cycle dependencies
        deps_full = []
        # First half-cycle dependencies
        deps_half = []
        # All of the dependencies (half, full, and common)
        deps_all = []
        if 'enkf' in self.run:

            dep_dict = {'type': 'task', 'name': f'{self.run}_earc_vrfy'}
            deps_full.append(rocoto.add_dependency(dep_dict))
            if self.options['do_archcom']:
                if self.options['do_globusarch']:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_globus_earc'}
                else:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_earc_tars'}
                deps_full.append(rocoto.add_dependency(dep_dict))

            if self.run == 'enkfgdas':
                # Date dependency for the first half cycle (only GDAS ensemble runs on the first half cycle)
                dep_dict = {'type': 'streq', 'left': '@Y@m@d@H', 'right': f'{first_half_cycle}'}
                deps_half.append(rocoto.add_dependency(dep_dict))

                # Add the next forecast segment dependency for ALL cycles (enkfgdas only)
                if dep_next_fcst_seg is not None:
                    deps_all.append(dep_next_fcst_seg)

                # earc_vrfy runs on the full cycles, so the dependency does not exist
                # for the half cycle. Instead, we will need to depend on the epmn
                # metatask and the echgres task for the half cycle.
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_epmn'}
                deps_half.append(rocoto.add_dependency(dep_dict))
                if not self.options['do_jediatmvar']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_echgres'}
                    deps_half.append(rocoto.add_dependency(dep_dict))

        else:
            if self.app_config.mode in ['cycled']:
                if self.run in ['gfs']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_vminmon']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                        deps_full.append(rocoto.add_dependency(dep_dict))
                elif self.run in ['gdas']:
                    # The gdas files are needed by the next forecast segment.
                    if dep_next_fcst_seg is not None:
                        deps_all.append(dep_next_fcst_seg)

                    # Date dependency for the first half cycle
                    dep_dict = {'type': 'streq', 'left': '@Y@m@d@H', 'right': f'{first_half_cycle}'}
                    deps_half.append(rocoto.add_dependency(dep_dict))

                    # Full-cycle dependencies
                    dep_dict = {'type': 'task', 'name': f'{self.run}_atmanlprod'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_fit2obs']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_fit2obs'}
                        deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_verfozn']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_verfozn'}
                        deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_verfrad']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_verfrad'}
                        deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_vminmon']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_vminmon'}
                        deps_full.append(rocoto.add_dependency(dep_dict))

            if self.run == 'gfs':
                if self.options['do_tracker']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_tracker'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                if self.options['do_genesis']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_genesis'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                if self.options['do_genesis_fsu']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_genesis_fsu'}
                    deps_full.append(rocoto.add_dependency(dep_dict))

            # Post-processing job dependencies
            # Atmosphere post-processing happens on all cycles
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
            deps_all.append(rocoto.add_dependency(dep_dict))

            # Other components only happen on full cycles
            if self.options['do_wave']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavepostsbs'}
                deps_full.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostpnt'}
                deps_full.append(rocoto.add_dependency(dep_dict))
                if self.options['do_wave_bnd']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpnt'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                    dep_dict = {'type': 'task', 'name': f'{self.run}_wavepostbndpntbll'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
            if self.options['do_ocean']:
                if self.run in ['gfs']:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
            if self.options['do_ice']:
                if self.run in ['gfs']:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
            if self.options['do_gempak']:
                if self.run in ['gdas']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmetancdc'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                elif self.run in ['gfs']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_gempakmeta'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.app_config.mode in ['cycled']:
                        dep_dict = {'type': 'task', 'name': f'{self.run}_gempakncdcupapgif'}
                        deps_full.append(rocoto.add_dependency(dep_dict))
                        if self.options['do_goes']:
                            dep_dict = {'type': 'task', 'name': f'{self.run}_npoess_pgrb2_0p5deg'}
                            deps_full.append(rocoto.add_dependency(dep_dict))
                            dep_dict = {'type': 'metatask', 'name': f'{self.run}_gempakgrb2spec'}
                            deps_full.append(rocoto.add_dependency(dep_dict))
                    if self.options['do_wave']:
                        dep_dict = {'type': 'metatask', 'name': f'{self.run}_wavegempak'}
                        deps_full.append(rocoto.add_dependency(dep_dict))

            if self.options['do_metp'] and self.run in ['gfs']:
                deps_metp = []
                # taskvalid only handles regular tasks, so just check the first metp job exists
                dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_metpg2g1', 'condition': 'not'}
                deps_metp.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_metp'}
                deps_metp.append(rocoto.add_dependency(dep_dict))
                deps_full.append(rocoto.create_dependency(dep_condition='or', dep=deps_metp))

            if self.options['do_awips'] and self.run in ['gfs']:

                dep_dict = {'type': 'metatask', 'name': f'{self.run}_awips_20km_1p0deg'}
                deps_full.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'{self.run}_fbwind'}
                deps_full.append(rocoto.add_dependency(dep_dict))

                if self.options['do_wave']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_waveawipsbulls'}
                    deps_full.append(rocoto.add_dependency(dep_dict))
                    dep_dict = {'type': 'task', 'name': f'{self.run}_waveawipsgridded'}
                    deps_full.append(rocoto.add_dependency(dep_dict))

            dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
            deps_full.append(rocoto.add_dependency(dep_dict))
            if self.options['do_archcom']:
                if self.options['do_globusarch']:
                    dep_dict = {'type': 'task', 'name': f'{self.run}_globus_arch'}
                else:
                    dep_dict = {'type': 'metatask', 'name': f'{self.run}_arch_tars'}

                deps_full.append(rocoto.add_dependency(dep_dict))

        # Build the rocoto dependencies

        deps_list = []
        # Add half-cycle dependencies if they exist
        if len(deps_half) > 1:
            dependencies_half = rocoto.create_dependency(dep_condition='and', dep=deps_half)
            dependencies_full = rocoto.create_dependency(dep_condition='and', dep=deps_full)
            deps_list = [dependencies_half, dependencies_full]
            # Combine half and full cycle dependencies with OR
            deps_full_half = rocoto.create_dependency(dep_condition='or', dep=deps_list)

        elif len(deps_half) == 1:
            dependencies_half = rocoto.create_dependency(dep=deps_half)
            dependencies_full = rocoto.create_dependency(dep_condition='and', dep=deps_full)
            deps_list = [dependencies_half, dependencies_full]

            # Combine half and full cycle dependencies with OR
            deps_full_half = rocoto.create_dependency(dep_condition='or', dep=deps_list)

        else:
            deps_full_half = rocoto.create_dependency(dep=deps_full)

        # Add full- and half-cycle to the complete list
        deps_all.append(deps_full_half)

        # Combine with AND
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps_all)

        resources = self.get_resource('cleanup')
        cycledef = self.run.replace('enkf', '')
        cycledef = f'gdas_half,{cycledef}' if 'gdas' in cycledef else cycledef
        task_name = f'{self.run}_cleanup'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/cleanup.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    # Start of ensemble tasks
    def eobs(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf", "")}_prep'}
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/eobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/ediag.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def eupd(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_ediag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('eupd')
        task_name = f'{self.run}_eupd'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/eupd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlinit(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf", "")}_prep'}
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlinit.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlobs.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlsol.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlletkf.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlfv3inc(self):

        deps = []
        if self.options['do_jediatmens_split_obssol']:
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlfv3inc.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/atmensanlfinal.sh',
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
        dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf", "")}_analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/ecen.sh',
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

    def ecen_fv3jedi(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f"{self.run.replace('enkf', '')}_atmanlfinal"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlfinal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ecen_fv3jedi')
        task_name = f'{self.run}_ecen_fv3jedi'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/ecen_fv3jedi.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def analcalc_fv3jedi(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f"{self.run}_atmanlfinal"}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_aero_anl']:
            dep_dict = {'type': 'task', 'name': f"{self.run}_aeroanlfinal"}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jedisnowda']:
            dep_dict = {'type': 'task', 'name': f"{self.run}_snowanl"}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analcalc_fv3jedi')
        task_name = f'{self.run}_analcalc_fv3jedi'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/analcalc_fv3jedi.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def esfc(self):

        deps = []
        if self.options['do_jediatmens']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_atmensanlfinal'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run.replace("enkf", "")}_analcalc'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.run}_eupd'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_jedisnowda']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_esnowanl'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('esfc')
        task_name = f'{self.run}_esfc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/esfc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def efcs(self):

        deps = []
        if self.options['do_jediatmens']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_ecen_fv3jedi'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ecmn'}
            deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_esfc'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_hybvar_ocn']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlecen'}
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/fcst.sh',
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
        dep_dict = {'type': 'metatask', 'name': f'{self.run.replace("enkf", "")}_fcst'}
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/echgres.sh',
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
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/epos.sh',
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

    def earc_vrfy(self):

        deps = []
        if 'enkfgdas' in self.run:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_epmn'}
            deps.append(rocoto.add_dependency(dep_dict))
            if not self.options['do_jediatmvar']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_echgres'}
                deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.run}_esfc'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        resources = self.get_resource('earc_vrfy')

        task_name = f'{self.run}_earc_vrfy'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': earcenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/earc_vrfy.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def earc_tars(self):

        deps = []
        if 'enkfgdas' in self.run:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_epmn'}
            deps.append(rocoto.add_dependency(dep_dict))
            if not self.options['do_jediatmens']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_echgres'}
                deps.append(rocoto.add_dependency(dep_dict))
            if self._base.get('DOLETKF_OCN', True):
                dep_dict = {'type': 'task', 'name': f'{self.run}_marineanlletkf'}
                deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:  # early cycle enkf run (enkfgfs)
            dep_dict = {'type': 'task', 'name': f'{self.run}_esfc'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ecmn'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self._base.get('DOHYBVAR_OCN', True):
                dep_dict = {'type': 'task', 'name': f'enkfgfs_marineanlecen'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'task', 'name': f'gfs_marineanlfinal'}
                deps.append(rocoto.add_dependency(dep_dict))
            if self._base.get('DOLETKF_OCN', True):
                dep_dict = {'type': 'task', 'name': f'enkfgfs_marineanlletkf'}
                deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        # Integer division is floor division, but we need ceiling division
        n_groups = -(self.nmem // -self._configs['earc_groups']['NMEM_EARCGRP'])
        groups = ' '.join([f'{grp:02d}' for grp in range(0, n_groups + 1)])

        resources = self.get_resource('arch_tars')

        var_dict = {'grp': groups}

        task_name = f'{self.run}_earc_tars_#grp#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': earcenvars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/job_cards/rocoto/earc_tars.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_earc_tars',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task
