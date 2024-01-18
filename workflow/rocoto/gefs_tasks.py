from applications.applications import AppConfig
from rocoto.tasks import Tasks
import rocoto.rocoto as rocoto


class GEFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, cdump: str) -> None:
        super().__init__(app_config, cdump)
        self.nmem = self._base['NMEM_ENS']

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

        # TODO: Add real dependencies
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app_config.do_wave:
            dep_dict = {'type': 'task', 'name': f'wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        fcst_envars = self.envars.copy()
        fcst_envar_dict = {'ENSMEM': '#member#',
                           'MEMDIR': 'mem#member#',
                           }
        for key, value in fcst_envar_dict.items():
            fcst_envars.append(rocoto.create_envar(name=key, value=str(value)))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        resources = self.get_resource('fcst')
        task_name = f'fcst_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': fcst_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'fcst',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def atmprod(self):
        atm_master_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_MASTER_TMPL"], {'MEMDIR': 'mem#member#'})
        deps = []
        data = f'{atm_master_path}/{self.cdump}.t@Hz.master.grb2f#fhr#'
        dep_dict = {'type': 'data', 'data': data, 'age': 120}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        atm_prod_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          'FHRLST': '#fhr#',
                          }
        for key, value in postenvar_dict.items():
            atm_prod_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('atmos_products')

        task_name = f'atm_prod_mem#member#_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': atm_prod_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmos_products.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        fhr_var_dict = {'fhr': ' '.join([str(fhr).zfill(3) for fhr in
                                         self._get_forecast_hours('gefs', self._configs['atmos_products'])])}
        fhr_metatask_dict = {'task_name': 'atm_prod_#member#',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict
                             }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'atm_prod',
                                'task_dict': fhr_metatask_dict,
                                'var_dict': member_var_dict
                                }

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
        data = f'{atmos_hist_path}/{self.cdump}.t@Hz.atm.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

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
