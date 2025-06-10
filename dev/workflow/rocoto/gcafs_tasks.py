"""
GCAFS (Global Chemistry and Aerosol Forecast System) Tasks module.

This module defines the tasks for the GCAFS workflow using Rocoto.
It provides task definitions for stage initialization, forecast,
post-processing, and archiving tasks.
"""
from applications.applications import AppConfig
from rocoto.tasks import Tasks
import rocoto.rocoto as rocoto
from wxflow import timedelta_to_HMS, to_timedelta


class GCAFSTasks(Tasks):
    """
    Global Chemistry and Aerosol Forecast System (GCAFS) Tasks class.

    This class defines tasks for the GCAFS workflow, including
    initialization, forecast, post-processing, and product generation tasks.

    Parameters
    ----------
    app_config : AppConfig
        Application configuration object
    run : str
        Run type identifier (e.g., 'gcafs', 'gfs')
    """

    def __init__(self, app_config: AppConfig, run: str) -> None:
        """
        Initialize the GCAFSTasks class.

        Parameters
        ----------
        app_config : AppConfig
            Application configuration object
        run : str
            Run type identifier

        Returns
        -------
        None
        """
        # Here we ensure we're using 'gcafs' as the run type internally
        if run == 'gfs':
            run = 'gcafs'

        super().__init__(app_config, run)

    # Specific Tasks begin here
    def fetch(self):

        if self.options['do_fetch_hpss'] or self.options['do_fetch_local']:
            cycledef = 'gcdas_half,gcdas'
        else:
            cycledef = 'gcdas'

        resources = self.get_resource('fetch')
        task_name = f'{self.run}_fetch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/jobs/fetch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def stage_ic(self):
        """
        Create a task for staging initial conditions.

        This task prepares the initial conditions needed for the forecast run.

        Returns
        -------
        str
            XML representation of the task
        """

        dependencies = None
        if self.options['do_fetch_hpss'] or self.options['do_fetch_local']:
            deps = []
            dep_dict = {
                'type': 'task', 'name': f'{self.run}_fetch',
            }
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)

        cycledef = 'gcdas_half' if self.run in ['gcdas', 'enkfgcdas'] else self.run

        resources = self.get_resource('stage_ic')
        task_name = f'{self.run}_stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/jobs/stage_ic.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def prep_emissions(self):
        """
        Create a task for preparing emissions data.

        This task prepares the emissions data needed for aerosol forecasting.

        Returns
        -------
        str
            XML representation of the task
        """

        resources = self.get_resource('prep_emissions')
        task_name = f'{self.run}_prep_emissions'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/jobs/prep_emissions.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def offlineanl(self):
        """
        Create a task for the analysis step.

        This task performs data assimilation to generate analysis fields
        by combining observations with the background forecast.

        Returns
        -------
        str
            XML representation of the task
        """
        resources = self.get_resource('offlineanl')

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_fetch'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'gcdas_fcst', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task_name = f'{self.run}_offlineanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gcdas',
                     'command': f'{self.HOMEgfs}/dev/jobs/offlineanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def sfcanl(self):

        deps = []
        if self.options['do_jediatmvar']:
            dep_dict = {'type': 'task', 'name': f'gcdas_atmanlfinal'}
        else:
            dep_dict = {'type': 'task', 'name': f'gcdas_offlineanl'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('sfcanl')
        task_name = f'{self.run}_sfcanl'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gcdas',
                     'command': f'{self.HOMEgfs}/dev/jobs/sfcanl.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepatmiodaobs(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'gcdas_prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepatmiodaobs')
        task_name = f'{self.run}_prepatmiodaobs'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gcdas',
                     'command': f'{self.HOMEgfs}/dev/jobs/prepatmiodaobs.sh',
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
        if self.options['do_hybvar']:
            dep_dict = {'type': 'metatask', 'name': 'enkfgdas_epmn', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)

        interval_gfs = self._base["INTERVAL_GFS"]
        gfs_enkf = True if self.options['do_hybvar'] and 'gfs' in self.app_config.ens_runs else False

        cycledef = self.run
        if self.run in ['gcafs'] and gfs_enkf and interval_gfs != 6:
            cycledef = 'gcdas'

        resources = self.get_resource('atmanlinit')
        task_name = f'{self.run}_atmanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/jobs/atmanlinit.sh',
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
                     'command': f'{self.HOMEgfs}/dev/jobs/atmanlvar.sh',
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
                     'command': f'{self.HOMEgfs}/dev/jobs/atmanlfv3inc.sh',
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
                     'command': f'{self.HOMEgfs}/dev/jobs/atmanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def prepobsaero(self):

        dump_suffix = self._base["DUMP_SUFFIX"]
        dmpdir = self._base["DMPDIR"]
        dump_path = self._template_to_rocoto_cycstring(self._base["COM_OBSPROC_TMPL"],
                                                       {'DMPDIR': dmpdir, 'DUMP_SUFFIX': dump_suffix, 'RUN': 'gdas'})

        deps = []
        data = f'{dump_path}/gdas.t@Hz.updated.status.tm00.bufr_d'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('prepobsaero')
        task_name = f'{self.run}_prepobsaero'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/jobs/prepobsaero.sh',
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
                     'cycledef': 'gcdas_half,gcdas',
                     'command': f'{self.HOMEgfs}/dev/jobs/aeroanlgenb.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlinit(self):

        deps = []
        dep_dict = {'type': 'task', 'name': 'gcdas_aeroanlgenb', 'offset': f"-{timedelta_to_HMS(self._base['interval_gdas'])}"}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': 'gcdas_prepobsaero'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('aeroanlinit')
        task_name = f'{self.run}_aeroanlinit'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/jobs/aeroanlinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aeroanlvar(self):

        deps = []
        dep_dict = {
            'type': 'task', 'name': f'{self.run}_aeroanlinit',
        }
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('aeroanlvar')
        task_name = f'{self.run}_aeroanlvar'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/jobs/aeroanlvar.sh',
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
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('aeroanlfinal')
        task_name = f'{self.run}_aeroanlfinal'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/jobs/aeroanlfinal.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def aerosol_init(self):
        """
        Create a task for aerosol initialization.

        This task initializes the aerosol component, setting up the necessary
        files and conditions for the aerosol forecast.

        Returns
        -------
        str
            XML representation of the task
        """
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

        # Calculate offset based on RUN = gcafs | gdas
        interval = None
        if self.run in ['gcafs']:
            interval = self._base['interval_gfs']
        elif self.run in ['gdas']:
            interval = self._base['interval']
        offset = timedelta_to_HMS(-interval)

        # Files from previous cycle
        files = ['@Y@m@d.@H0000.fv_core.res.nc'] + \
                [f'@Y@m@d.@H0000.fv_core.res.tile{tile}.nc' for tile in range(1, ntiles + 1)] + \
                [f'@Y@m@d.@H0000.fv_tracer.res.tile{tile}.nc' for tile in range(1, ntiles + 1)]

        for file in files:
            data = [f'{restart_path}/', file]
            dep_dict = {'type': 'data', 'data': data, 'offset': [offset, None]}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = f'{self.run}_seq'
        resources = self.get_resource('aerosol_init')
        task_name = f'{self.run}_aerosol_init'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': cycledef,
                     'command': f'{self.HOMEgfs}/dev/jobs/aerosol_init.sh',
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
        """
        Create a task for the deterministic forecast.

        This task runs the deterministic forecast (member 000) for all forecast segments.
        It depends on stage_ic and optionally on wave_init, prep_emissions, and aerosol_init
        if these components are enabled.

        Returns
        -------
        str
            XML representation of the task
        """
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_fcst']:
            # Add prep_emissions dependency
            dep_dict = {'type': 'task', 'name': f'{self.run}_prep_emissions'}
            dependencies.append(rocoto.add_dependency(dep_dict))

            # Add aerosol_init dependency with cycle offset in nested or
            aerosol_init_deps = []
            interval = self._base['interval_gfs'] if self.run in ['gcafs'] else self._base['interval']
            offset = timedelta_to_HMS(-interval)
            dep_dict = {'type': 'task', 'name': f'{self.run}_aerosol_init', 'offset': offset}
            aerosol_init_deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': offset}
            aerosol_init_deps.append(rocoto.add_dependency(dep_dict))

            dependencies.append(rocoto.create_dependency(dep_condition='or', dep=aerosol_init_deps))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        num_fcst_segments = len(self.options['fcst_segments']) - 1

        fcst_vars = self.envars.copy()
        fcst_envars_dict = {'FCST_SEGMENT': '#seg#'}
        for key, value in fcst_envars_dict.items():
            fcst_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('fcst')
        task_name = f'{self.run}_fcst_mem000_seg#seg#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': fcst_vars,
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/jobs/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
        metatask_dict = {'task_name': f'{self.run}_fcst_mem000',
                         'is_serial': True,
                         'var_dict': seg_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def _fcst_cycled(self):

        anldep = 'gcdas'
        dep_dict = {'type': 'task', 'name': f'{anldep}_sfcanl'}
        dep = rocoto.add_dependency(dep_dict)
        dependencies = rocoto.create_dependency(dep=dep)

        if self.options['do_aero_fcst']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_prep_emissions'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dep_dict = {'type': 'task', 'name': f'{anldep}_aeroanlfinal'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        if self.run in ['gcdas']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='or', dep=dependencies)

        cycledef = 'gcdas_half,gcdas' if self.run in ['gcdas'] else self.run

        if self.run in ['gcafs']:
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
                     'command': f'{self.HOMEgfs}/dev/jobs/fcst.sh',
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

    def efcs(self):
        """
        Create tasks for the ensemble forecast members.

        This task runs the ensemble forecast members (001-N) for all forecast segments.
        It depends on stage_ic and optionally on wave_init and prep_emissions
        if these components are enabled.

        Returns
        -------
        str
            XML representation of the task
        """
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_fcst']:
            dep_dict = {'type': 'task', 'name': f'{self.run}_prep_emissions'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        num_fcst_segments = len(self.options['fcst_segments']) - 1
        resources = self.get_resource('efcs')

        # Kludge to work around bug in rocoto with serial metatasks nested
        #   in a parallel one (see christopherwharrop/rocoto#109). For now,
        #   loop over member to create a separate metatask for each instead
        #   of a metatask of a metatask.
        #
        tasks = []
        for member in [f"{mem:03d}" for mem in range(1, self.nmem + 1)]:

            efcsenvars = self.envars.copy()
            efcsenvars_dict = {'ENSMEM': f'{member}',
                               'MEMDIR': f'mem{member}',
                               'FCST_SEGMENT': '#seg#'
                               }
            for key, value in efcsenvars_dict.items():
                efcsenvars.append(rocoto.create_envar(name=key, value=str(value)))

            task_name = f'{self.run}_fcst_mem{member}_seg#seg#'
            task_dict = {'task_name': task_name,
                         'resources': resources,
                         'dependency': dependencies,
                         'envars': efcsenvars,
                         'cycledef': self.run,
                         'command': f'{self.HOMEgfs}/dev/jobs/fcst.sh',
                         'job_name': f'{self.pslot}_{task_name}_@H',
                         'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                         'maxtries': '&MAXTRIES;'
                         }

            seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
            seg_metatask_dict = {'task_name': f'{self.run}_fcst_mem{member}',
                                 'is_serial': True,
                                 'var_dict': seg_var_dict,
                                 'task_dict': task_dict
                                 }

            tasks.append(rocoto.create_task(seg_metatask_dict))

        return '\n'.join(tasks)

        # Keeping this in hopes the kludge is no longer necessary at some point
        #
        # member_var_dict = {'member': ' '.join([f"{mem:03d}" for mem in range(1, self.nmem + 1)])}
        # mem_metatask_dict = {'task_name': f'{self.run}_fcst_ens',
        #                      'is_serial': False,
        #                      'var_dict': member_var_dict,
        #                      'task_dict': seg_metatask_dict
        #                      }

        # task = rocoto.create_task(mem_metatask_dict)

        # return task

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
                     'command': f'{self.HOMEgfs}/dev/jobs/upp.sh',
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
                     'command': f'{self.HOMEgfs}/dev/jobs/atmos_products.sh',
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
                     'command': f'{self.HOMEgfs}/dev/jobs/upp.sh',
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
        """
        Create a task for atmospheric product generation.

        This task generates products from the atmospheric component output.

        Returns
        -------
        str
            XML representation of the task
        """
        return self._atmosoceaniceprod('atmos')

    def _atmosoceaniceprod(self, component: str):
        """
        Create a task for atmospheric, oceanic, or ice product generation.

        This is a helper method used by atmos_prod, ocean_prod, and ice_prod methods.

        Parameters
        ----------
        component : str
            Component name ('atmos', 'ocean', or 'ice')

        Returns
        -------
        str
            XML representation of the task
        """

        fhout_ocn_gfs = self._configs['base']['FHOUT_OCN_GFS']
        fhout_ice_gfs = self._configs['base']['FHOUT_ICE_GFS']
        products_dict = {'atmos': {'config': 'atmos_products',
                                   'history_path_tmpl': 'COM_ATMOS_MASTER_TMPL',
                                   'history_file_tmpl': f'{self.run}.t@Hz.master.grb2f#fhr3_last#'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.run}.ocean.t@Hz.{fhout_ocn_gfs}hr_avg.f#fhr3_next#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.run}.ice.t@Hz.{fhout_ice_gfs}hr_avg.f#fhr3_last#.nc'}}

        component_dict = products_dict[component]
        config = component_dict['config']
        history_path_tmpl = component_dict['history_path_tmpl']
        history_file_tmpl = component_dict['history_file_tmpl']

        max_tasks = self._configs[config]['MAX_TASKS']
        resources = self.get_resource(config)

        fhrs = self._get_forecast_hours(self.run, self._configs[config], component)

        # when replaying, atmos component does not have fhr 0, therefore remove 0 from fhrs
        is_replay = self._configs[config]['REPLAY_ICS']
        if is_replay and component in ['atmos'] and 0 in fhrs:
            fhrs.remove(0)

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice'] and 0 in fhrs:
            fhrs.remove(0)

        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

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

        cycledef = 'gcdas_half,gcdas' if self.run in ['gcdas'] else self.run

        task_name = f'{self.run}_{component}_prod_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': cycledef,
                     'command': f"{self.HOMEgfs}/dev/jobs/{config}.sh",
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        metatask_dict = {'task_name': f'{self.run}_{component}_prod',
                         'task_dict': task_dict,
                         'var_dict': fhr_var_dict}

        task = rocoto.create_task(metatask_dict)

        return task

    def atmos_ensstat(self):
        """
        Create a task for atmospheric ensemble statistics.

        This task computes statistical products from ensemble atmospheric output.

        Returns
        -------
        str
            XML representation of the task
        """

        resources = self.get_resource('atmos_ensstat')

        deps = []
        for member in range(0, self.nmem + 1):
            task = f'{self.run}_atmos_prod_mem{member:03d}_#fhr_label#'
            dep_dict = {'type': 'task', 'name': task}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        fhrs = self._get_forecast_hours(self.run, self._configs['atmos_ensstat'])

        # when replaying, atmos component does not have fhr 0, therefore remove 0 from fhrs
        is_replay = self._configs['atmos_ensstat']['REPLAY_ICS']
        if is_replay and 0 in fhrs:
            fhrs.remove(0)

        max_tasks = self._configs['atmos_ensstat']['MAX_TASKS']
        fhr_var_dict = self.get_grouped_fhr_dict(fhrs=fhrs, ngroups=max_tasks)

        # Adjust walltime based on the largest group
        largest_group = max([len(grp.split(',')) for grp in fhr_var_dict['fhr_list'].split(' ')])
        resources['walltime'] = Tasks.multiply_HMS(resources['walltime'], largest_group)

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR_LIST': '#fhr_list#'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        task_name = f'{self.run}_atmos_ensstat_#fhr_label#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/jobs/atmos_ensstat.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'}

        fhr_metatask_dict = {'task_name': f'{self.run}_atmos_ensstat',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        task = rocoto.create_task(fhr_metatask_dict)

        return task

    def metp(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self._base["interval_gfs"] < to_timedelta("24H"):
            n_lookback = self._base["interval_gfs"] // to_timedelta("6H")
            for lookback in range(1, n_lookback + 1):
                deps2 = []
                dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_arch_vrfy', 'condition': 'not'}
                deps2.append(rocoto.add_dependency(dep_dict))
                for lookback2 in range(1, lookback):
                    offset = timedelta_to_HMS(-to_timedelta(f'{6*lookback2}H'))
                    dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': offset}
                    deps2.append(rocoto.add_dependency(dep_dict))

                offset = timedelta_to_HMS(-to_timedelta(f'{6*lookback}H'))
                dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy', 'offset': offset}
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
                     'command': f'{self.HOMEgfs}/dev/jobs/metp.sh',
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

    def anlstat(self):
        deps = []
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
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/jobs/anlstat.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def extractvars(self):
        """
        Create a task for extracting variables from model output.

        This task extracts specific variables from the model output for further processing.

        Returns
        -------
        str
            XML representation of the task
        """
        deps = []
        if self.options['do_wave']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_wave_post_grid_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_atm']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        extractvars_envars = self.envars.copy()
        extractvars_dict = {'ENSMEM': '#member#',
                            'MEMDIR': 'mem#member#',
                            }
        for key, value in extractvars_dict.items():
            extractvars_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('extractvars')
        task_name = f'{self.run}_extractvars_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': extractvars_envars,
                     'cycledef': self.run,
                     'command': f'{self.HOMEgfs}/dev/jobs/extractvars.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': f'{self.run}_extractvars',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def arch_vrfy(self):
        """
        Create a task for archiving verification data.

        This task archives data used for verification of model performance.

        Returns
        -------
        str
            XML representation of the task
        """
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if int(self.options['nens'] > 0):
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_ensstat'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ice_prod'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_ocean_prod'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_wave']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_wave_post_grid'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_wave_post_pnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_wave_bnd']:
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_wave_post_bndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'metatask', 'name': f'{self.run}_wave_post_bndpnt_bull'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_extractvars']:
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_extractvars'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('arch_vrfy')
        task_name = f'{self.run}_arch_vrfy'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': self.run,
                     'dependency': dependencies,
                     'command': f'{self.HOMEgfs}/dev/jobs/arch_vrfy.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def arch_tars(self):
        """
        Create a task for creating archive tar files.

        This task creates tar archives of model output data for long-term storage.

        Returns
        -------
        str
            XML representation of the task
        """
        deps = []
        if self.app_config.mode in ['cycled']:
            if self.options['do_anlstat'] and self.options['do_aero_anl']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_anlstat'}
                deps.append(rocoto.add_dependency(dep_dict))
            if self.run in ['gcdas']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_aeroanlgenb'}
                deps.append(rocoto.add_dependency(dep_dict))
        # Post job dependencies
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_metp'] and self.run in ['gcafs']:
            deps2 = []
            # taskvalid only handles regular tasks, so just check the first metp job exists
            dep_dict = {'type': 'taskvalid', 'name': f'{self.run}_metpg2g1', 'condition': 'not'}
            deps2.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': f'{self.run}_metp'}
            deps2.append(rocoto.add_dependency(dep_dict))
            deps.append(rocoto.create_dependency(dep_condition='or', dep=deps2))

        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('arch_tars')
        task_name = f'{self.run}_arch_tars'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': self.run.replace('enkf', ''),
                     'command': f'{self.HOMEgfs}/dev/jobs/arch_tars.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def globus(self):
        """
        Create a task for Globus transfer.

        This task manages the transfer of data using Globus.

        Returns
        -------
        str
            XML representation of the task
        """
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        resources = self.get_resource('globus')
        task_name = 'globus_arch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'dependency': dependencies,
                     'command': f'{self.HOMEgfs}/dev/jobs/globus_arch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def cleanup(self):
        """
        Create a task for cleanup operations.

        This task performs cleanup operations after the workflow is complete.

        Returns
        -------
        str
            XML representation of the task
        """
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.run}_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.run}_arch_vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_archcom']:
            if self.options['do_globusarch']:
                dep_dict = {'type': 'task', 'name': f'{self.run}_globus_arch'}
            else:
                dep_dict = {'type': 'task', 'name': f'{self.run}_arch_tars'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)

        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('cleanup')
        task_name = f'{self.run}_cleanup'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': self.run,
                     'dependency': dependencies,
                     'command': f'{self.HOMEgfs}/dev/jobs/cleanup.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task
