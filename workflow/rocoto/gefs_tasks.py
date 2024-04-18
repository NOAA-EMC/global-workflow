from applications.applications import AppConfig
from rocoto.tasks import Tasks
import rocoto.rocoto as rocoto


class GEFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, cdump: str) -> None:
        super().__init__(app_config, cdump)

    def stage_ic(self):

        cpl_ic = self._configs['stage_ic']

        deps = []

        # Atm ICs
        if self.app_config.do_atm:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ATMIC']}/@Y@m@d@H/mem000/atmos"
            for file in ['gfs_ctrl.nc'] + \
                        [f'{datatype}_data.tile{tile}.nc'
                         for datatype in ['gfs', 'sfc']
                         for tile in range(1, self.n_tiles + 1)]:
                data = f"{prefix}/{file}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        # Ocean ICs
        if self.app_config.do_ocean:
            ocn_res = f"{self._base.get('OCNRES', '025'):03d}"
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_OCNIC']}/@Y@m@d@H/mem000/ocean"
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
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ICEIC']}/@Y@m@d@H/mem000/ice"
            data = f"{prefix}/@Y@m@d.@H0000.cice_model.res.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Wave ICs
        if self.app_config.do_wave:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_WAVIC']}/@Y@m@d@H/mem000/wave"
            for wave_grid in self._configs['waveinit']['waveGRD'].split():
                data = f"{prefix}/@Y@m@d.@H0000.restart.{wave_grid}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        resources = self.get_resource('stage_ic')
        task_name = f'stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/stage_ic.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def waveinit(self):

        resources = self.get_resource('waveinit')
        task_name = f'wave_init'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/waveinit.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def fcst(self):
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_wave:
            dep_dict = {'type': 'task', 'name': f'wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        resources = self.get_resource('fcst')
        task_name = f'fcst_mem000'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def efcs(self):
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_wave:
            dep_dict = {'type': 'task', 'name': f'wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        efcsenvars = self.envars.copy()
        efcsenvars_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#'
                           }
        for key, value in efcsenvars_dict.items():
            efcsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('efcs')

        task_name = f'fcst_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': efcsenvars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([f"{mem:03d}" for mem in range(1, self.nmem + 1)])}
        metatask_dict = {'task_name': 'fcst_ens',
                         'var_dict': member_var_dict,
                         'task_dict': task_dict
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
                                   'history_file_tmpl': f'{self.cdump}.t@Hz.master.grb2f#fhr#'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.cdump}.ocean.t@Hz.6hr_avg.f#fhr#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.cdump}.ice.t@Hz.6hr_avg.f#fhr#.nc'}}

        component_dict = products_dict[component]
        config = component_dict['config']
        history_path_tmpl = component_dict['history_path_tmpl']
        history_file_tmpl = component_dict['history_file_tmpl']

        resources = self.get_resource(config)

        history_path = self._template_to_rocoto_cycstring(self._base[history_path_tmpl], {'MEMDIR': 'mem#member#'})
        deps = []
        data = f'{history_path}/{history_file_tmpl}'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        postenvars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          'FHRLST': '#fhr#',
                          'COMPONENT': component}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        task_name = f'{component}_prod_mem#member#_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/{config}.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'}

        fhrs = self._get_forecast_hours('gefs', self._configs[config])

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice']:
            fhrs.remove(0)

        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}

        fhr_metatask_dict = {'task_name': f'{component}_prod_#member#',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        member_var_dict = {'member': ' '.join([f"{mem:03d}" for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': f'{component}_prod',
                                'task_dict': fhr_metatask_dict,
                                'var_dict': member_var_dict}

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostsbs(self):
        deps = []
        for wave_grid in self._configs['wavepostsbs']['waveGRD'].split():
            wave_hist_path = self._template_to_rocoto_cycstring(self._base["COM_WAVE_HISTORY_TMPL"], {'MEMDIR': 'mem#member#'})
            data = f'{wave_hist_path}/gefswave.out_grd.{wave_grid}.@Y@m@d.@H0000'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        wave_post_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostsbs')

        task_name = f'wave_post_grid_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostsbs.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'wave_post_grid',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        wave_post_bndpnt_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_bndpnt_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostbndpnt')
        task_name = f'wave_post_bndpnt_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_bndpnt_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'wave_post_bndpnt',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostbndpntbll(self):
        deps = []
        atmos_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'MEMDIR': 'mem#member#'})
        # Is there any reason this is 180?
        data = f'{atmos_hist_path}/{self.cdump}.t@Hz.atm.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))

        dep_dict = {'type': 'task', 'name': f'fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        wave_post_bndpnt_bull_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_bndpnt_bull_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostbndpntbll')
        task_name = f'wave_post_bndpnt_bull_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_bndpnt_bull_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostbndpntbll.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'wave_post_bndpnt_bull',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app_config.do_wave_bnd:
            dep_dict = {'type': 'task', 'name': f'wave_post_bndpnt_bull_mem#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        wave_post_pnt_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_pnt_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostpnt')
        task_name = f'wave_post_pnt_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': wave_post_pnt_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/wavepostpnt.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'wave_post_pnt',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task
