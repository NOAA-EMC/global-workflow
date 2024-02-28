from applications.applications import AppConfig
from rocoto.tasks import Tasks
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
        task_name = f'{self.cdump}stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/stage_ic.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        dep_dict = {'type': 'metatask', 'name': 'gdasatmos_prod', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
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
        task_name = f'{self.cdump}prep'
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
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        if self.app_config.mode in ['cycled']:
            deps = []
            dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.cdump in ['gdas']:
                dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
                deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        task_name = f'{self.cdump}waveinit'
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
        dep_dict = {'type': 'task', 'name': f'{self.cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource('waveprep')
        task_name = f'{self.cdump}waveprep'
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
        task_name = f'{self.cdump}aerosol_init'
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
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('anal')
        task_name = f'{self.cdump}anal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
            dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_jedisnowda:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}snowanl'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('sfcanl')
        task_name = f'{self.cdump}sfcanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
        task_name = f'{self.cdump}analcalc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/analcalc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def analdiag(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('analdiag')
        task_name = f'{self.cdump}analdiag'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/analdiag.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepatmiodaobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepatmiodaobs')
        task_name = f'{self.cdump}prepatmiodaobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepatmiodaobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}atmanlinit'
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

    def atmanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmanlrun')
        task_name = f'{self.cdump}atmanlrun'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlrun.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmanlfinal')
        task_name = f'{self.cdump}atmanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlinit')
        task_name = f'{self.cdump}aeroanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('aeroanlrun')
        task_name = f'{self.cdump}aeroanlrun'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlrun.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}aeroanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlfinal')
        task_name = f'{self.cdump}aeroanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/aeroanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepsnowobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepsnowobs')
        task_name = f'{self.cdump}prepsnowobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepsnowobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def snowanl(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prepsnowobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('snowanl')
        task_name = f'{self.cdump}snowanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/snowanl.sh',
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
        dep_dict = {'type': 'data', 'data': data, 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepoceanobs')
        task_name = f'{self.cdump}prepoceanobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prepoceanobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalprep(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prepoceanobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalprep')
        task_name = f'{self.cdump}ocnanalprep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalprep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalbmat(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalprep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalbmat')
        task_name = f'{self.cdump}ocnanalbmat'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalbmat.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalbmat'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ocnanalrun')
        task_name = f'{self.cdump}ocnanalrun'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalrun.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}ocnanalchkpt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalchkpt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalpost(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalchkpt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalpost')
        task_name = f'{self.cdump}ocnanalpost'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ocnanalpost.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def ocnanalvrfy(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}ocnanalpost'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('ocnanalvrfy')
        task_name = f'{self.cdump}ocnanalvrfy'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
        task_name = f'{self.cdump}fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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

        if self.app_config.do_jedisnowda:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}snowanl'}
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
        task_name = f'{self.cdump}fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}atmanlupp'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/upp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}atmanlprod'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmos_products.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    @staticmethod
    def _get_ufs_postproc_grps(cdump, config, component='atmos'):

        fhrs = Tasks._get_forecast_hours(cdump, config, component=component)

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
        return self._upptask(upp_run='forecast', task_id='atmupp')

    def goesupp(self):
        return self._upptask(upp_run='goes', task_id='goesupp')

    def _upptask(self, upp_run="forecast", task_id="atmupp"):

        VALID_UPP_RUN = ["forecast", "goes", "wafs"]
        if upp_run not in VALID_UPP_RUN:
            raise KeyError(f"{upp_run} is invalid; UPP_RUN options are: {('|').join(VALID_UPP_RUN)}")

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_ufs_postproc_grps(self.cdump, self._configs['upp'])
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '#lst#',
                          'UPP_RUN': upp_run}
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

        task_name = f'{self.cdump}{task_id}#{varname1}#'
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

        metatask_dict = {'task_name': f'{self.cdump}{task_id}',
                         'task_dict': task_dict,
                         'var_dict': var_dict
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
                                   'history_file_tmpl': f'{self.cdump}.t@Hz.master.grb2#dep#'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.cdump}.ocean.t@Hz.6hr_avg.#dep#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.cdump}.ice.t@Hz.6hr_avg.#dep#.nc'}}

        component_dict = products_dict[component]
        config = component_dict['config']
        history_path_tmpl = component_dict['history_path_tmpl']
        history_file_tmpl = component_dict['history_file_tmpl']

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_ufs_postproc_grps(self.cdump, self._configs[config], component=component)
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        postenvars = self.envars.copy()
        postenvar_dict = {'FHRLST': '#lst#', 'COMPONENT': component}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        history_path = self._template_to_rocoto_cycstring(self._base[history_path_tmpl])
        deps = []
        data = f'{history_path}/{history_file_tmpl}'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        cycledef = 'gdas_half,gdas' if self.cdump in ['gdas'] else self.cdump
        resources = self.get_resource(component_dict['config'])

        task_name = f'{self.cdump}{component}_prod#{varname1}#'
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

        metatask_dict = {'task_name': f'{self.cdump}{component}_prod',
                         'task_dict': task_dict,
                         'var_dict': var_dict
                         }

        task = rocoto.create_task(metatask_dict)

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
        task_name = f'{self.cdump}wavepostsbs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostsbs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpnt')
        task_name = f'{self.cdump}wavepostbndpnt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavepostbndpntbll(self):
        deps = []
        atmos_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"])
        data = f'{atmos_hist_path}/{self.cdump}.t@Hz.atm.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavepostbndpntbll')
        task_name = f'{self.cdump}wavepostbndpntbll'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpntbll.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}wavepostpnt'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def wavegempak(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('wavegempak')
        task_name = f'{self.cdump}wavegempak'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavegempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveawipsbulls(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostpnt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('waveawipsbulls')
        task_name = f'{self.cdump}waveawipsbulls'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveawipsbulls.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def waveawipsgridded(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('waveawipsgridded')
        task_name = f'{self.cdump}waveawipsgridded'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveawipsgridded.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def postsnd(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('postsnd')
        task_name = f'{self.cdump}postsnd'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/postsnd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}fbwind'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fbwind.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self.rotdir}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_awipsgroups(self.cdump, self._configs['awips'])
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')

        task_name = f'{self.cdump}awips_20km_1p0deg#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': awipsenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/awips_20km_1p0deg.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.cdump}awips_20km_1p0deg',
                         'task_dict': task_dict,
                         'var_dict': var_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def awips_g2(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        awipsenvars = self.envars.copy()
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self.rotdir}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = self._get_awipsgroups(self.cdump, self._configs['awips'])
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        resources = self.get_resource('awips')

        task_name = f'{self.cdump}awips_g2#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': awipsenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/awips_g2.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.cdump}awips_g2',
                         'task_dict': task_dict,
                         'var_dict': var_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def gempak(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.cdump}gempak'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempak.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakmeta(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.cdump}gempakmeta'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakmeta.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakmetancdc(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.cdump}gempakmetancdc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakmetancdc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakncdcupapgif(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.cdump}gempakncdcupapgif'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakncdcupapgif.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def gempakpgrb2spec(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}npoess_pgrb2_0p5deg'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('gempak')
        task_name = f'{self.cdump}gempakgrb2spec'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/gempakgrb2spec.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def npoess_pgrb2_0p5deg(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmanlprod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('npoess')
        task_name = f'{self.cdump}npoess_pgrb2_0p5deg'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/npoess_pgrb2_0p5deg.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def verfozn(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfozn')
        task_name = f'{self.cdump}verfozn'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/verfozn.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def verfrad(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analdiag'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('verfrad')
        task_name = f'{self.cdump}verfrad'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/verfrad.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def vminmon(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('vminmon')
        task_name = f'{self.cdump}vminmon'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/vminmon.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def tracker(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('tracker')
        task_name = f'{self.cdump}tracker'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/tracker.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def genesis(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis')
        task_name = f'{self.cdump}genesis'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/genesis.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def genesis_fsu(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('genesis_fsu')
        task_name = f'{self.cdump}genesis_fsu'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/genesis_fsu.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def fit2obs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('fit2obs')
        task_name = f'{self.cdump}fit2obs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fit2obs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        var_dict = {varname1: varval1}

        resources = self.get_resource('metp')

        task_name = f'{self.cdump}metp#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': metpenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/metp.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.cdump}metp',
                         'task_dict': task_dict,
                         'var_dict': var_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def mos_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prep')
        task_name = f'{self.cdump}mos_stn_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_grd_prep')
        task_name = f'{self.cdump}mos_grd_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_stn_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_stn_prep')
        task_name = f'{self.cdump}mos_ext_stn_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_ext_grd_prep(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_ext_grd_prep')
        task_name = f'{self.cdump}mos_ext_grd_prep'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_prep.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_stn_fcst(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_fcst')
        task_name = f'{self.cdump}mos_stn_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_grd_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_ext_stn_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_ext_grd_fcst'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_stn_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_stn_fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_stn_prdgen')
        task_name = f'{self.cdump}mos_stn_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_stn_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_grd_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_grd_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_ext_stn_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_stn_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_ext_grd_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_ext_grd_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def mos_wx_prdgen(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}mos_grd_prdgen'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('mos_wx_prdgen')
        task_name = f'{self.cdump}mos_wx_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_wx_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}mos_wx_ext_prdgen'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/mos_wx_ext_prdgen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}atmos_prod'}
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
            if self.cdump in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.cdump}ocean_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_ice:
            if self.cdump in ['gfs']:
                dep_dict = {'type': 'metatask', 'name': f'{self.cdump}ice_prod'}
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
        task_name = f'{self.cdump}arch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
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
        if 'enkf' in self.cdump:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}eamn'}
            deps.append(rocoto.add_dependency(dep_dict))
        else:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}arch'}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('cleanup')
        task_name = f'{self.cdump}cleanup'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('eobs')
        task_name = f'{self.cdump}eobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eobs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def eomg(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eomgenvars = self.envars.copy()
        eomgenvars_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#'
                           }
        for key, value in eomgenvars_dict.items():
            eomgenvars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('eomg')
        task_name = f'{self.cdump}eomg_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': eomgenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eomg.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(1, self.nmem + 1)])}
        metatask_dict = {'task_name': f'{self.cdump}eomg',
                         'var_dict': member_var_dict,
                         'task_dict': task_dict,
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def ediag(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('ediag')
        task_name = f'{self.cdump}ediag'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
            dep_dict = {'type': 'task', 'name': f'{self.cdump}ediag'}
        else:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}eomg'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('eupd')
        task_name = f'{self.cdump}eupd'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/eupd.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        task_name = f'{self.cdump}atmensanlinit'
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

    def atmensanlrun(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'enkfgdasepmn', 'offset': f"-{timedelta_to_HMS(self._base['cycle_interval'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('atmensanlrun')
        task_name = f'{self.cdump}atmensanlrun'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmensanlrun.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def atmensanlfinal(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}atmensanlrun'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('atmensanlfinal')
        task_name = f'{self.cdump}atmensanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
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
        var_dict = {varname1: varval1, varname2: varval2, varname3: varval3}

        resources = self.get_resource('ecen')

        task_name = f'{self.cdump}ecen#{varname1}#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': ecenenvars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/ecen.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.cdump}ecmn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)
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
        task_name = f'{self.cdump}esfc'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.cdump.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/jobs/rocoto/esfc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

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
        efcsenvars_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#'
                           }
        for key, value in efcsenvars_dict.items():
            efcsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')
        resources = self.get_resource('efcs')

        task_name = f'{self.cdump}fcst_mem#member#'
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
        metatask_dict = {'task_name': f'{self.cdump}fcst',
                         'var_dict': member_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def echgres(self):

        self._is_this_a_gdas_task(self.cdump, 'echgres')

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump.replace("enkf","")}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst_mem001'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump

        resources = self.get_resource('echgres')
        task_name = f'{self.cdump}echgres'
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
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}fcst'}
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

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')

        resources = self.get_resource('epos')

        task_name = f'{self.cdump}epos#{varname1}#'
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

        metatask_dict = {'task_name': f'{self.cdump}epmn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def earc(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}epmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        earcenvars = self.envars.copy()
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        # Integer division is floor division, but we need ceiling division
        n_groups = -(self.nmem // -self._configs['earc']['NMEM_EARCGRP'])
        groups = ' '.join([f'{grp:02d}' for grp in range(0, n_groups)])

        cycledef = 'gdas_half,gdas' if self.cdump in ['enkfgdas'] else self.cdump.replace('enkf', '')

        resources = self.get_resource('earc')

        var_dict = {'grp': groups}

        task_name = f'{self.cdump}earc#grp#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': earcenvars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/earc.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.cdump}eamn',
                         'var_dict': var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task
