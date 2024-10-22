from applications.applications import AppConfig
from rocoto.tasks import Tasks
import rocoto.rocoto as rocoto


class GEFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, run: str) -> None:
        super().__init__(app_config, run)

    def stage_ic(self):

        resources = self.get_resource('stage_ic')
        task_name = f'gefs_stage_ic'
        task_dict = {'task_name': task_name,
                     'resources': resources,
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
        task_name = f'gefs_wave_init'
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

    def prep_emissions(self):

        resources = self.get_resource('prep_emissions')
        task_name = 'gefs_prep_emissions'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/prep_emissions.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }
        task = rocoto.create_task(task_dict)

        return task

    def fcst(self):
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'gefs_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            dep_dict = {'type': 'task', 'name': f'gefs_wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_fcst']:
            dep_dict = {'type': 'task', 'name': f'gefs_prep_emissions'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        num_fcst_segments = len(self.options['fcst_segments']) - 1

        fcst_vars = self.envars.copy()
        fcst_envars_dict = {'FCST_SEGMENT': '#seg#'}
        for key, value in fcst_envars_dict.items():
            fcst_vars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('fcst')
        task_name = f'gefs_fcst_mem000_seg#seg#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': fcst_vars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/fcst.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
        metatask_dict = {'task_name': f'gefs_fcst_mem000',
                         'is_serial': True,
                         'var_dict': seg_var_dict,
                         'task_dict': task_dict
                         }

        task = rocoto.create_task(metatask_dict)

        return task

    def efcs(self):
        dependencies = []
        dep_dict = {'type': 'task', 'name': f'gefs_stage_ic'}
        dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_wave']:
            dep_dict = {'type': 'task', 'name': f'gefs_wave_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.options['do_aero_fcst']:
            dep_dict = {'type': 'task', 'name': f'gefs_prep_emissions'}
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

            task_name = f'gefs_fcst_mem{member}_seg#seg#'
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

            seg_var_dict = {'seg': ' '.join([f"{seg}" for seg in range(0, num_fcst_segments)])}
            seg_metatask_dict = {'task_name': f'gefs_fcst_mem{member}',
                                 'is_serial': True,
                                 'var_dict': seg_var_dict,
                                 'task_dict': task_dict
                                 }

            tasks.append(rocoto.create_task(seg_metatask_dict))

        return '\n'.join(tasks)

        # Keeping this in hopes the kludge is no longer necessary at some point
        #
        # member_var_dict = {'member': ' '.join([f"{mem:03d}" for mem in range(1, self.nmem + 1)])}
        # mem_metatask_dict = {'task_name': 'gefs_fcst_ens',
        #                      'is_serial': False,
        #                      'var_dict': member_var_dict,
        #                      'task_dict': seg_metatask_dict
        #                      }

        # task = rocoto.create_task(mem_metatask_dict)

        # return task

    def atmos_prod(self):
        return self._atmosoceaniceprod('atmos')

    def ocean_prod(self):
        return self._atmosoceaniceprod('ocean')

    def ice_prod(self):
        return self._atmosoceaniceprod('ice')

    def _atmosoceaniceprod(self, component: str):

        fhout_ocn_gfs = self._configs['base']['FHOUT_OCN_GFS']
        fhout_ice_gfs = self._configs['base']['FHOUT_ICE_GFS']
        products_dict = {'atmos': {'config': 'atmos_products',
                                   'history_path_tmpl': 'COM_ATMOS_MASTER_TMPL',
                                   'history_file_tmpl': f'{self.run}.t@Hz.master.grb2f#fhr#'},
                         'ocean': {'config': 'oceanice_products',
                                   'history_path_tmpl': 'COM_OCEAN_HISTORY_TMPL',
                                   'history_file_tmpl': f'{self.run}.ocean.t@Hz.{fhout_ocn_gfs}hr_avg.f#fhr_next#.nc'},
                         'ice': {'config': 'oceanice_products',
                                 'history_path_tmpl': 'COM_ICE_HISTORY_TMPL',
                                 'history_file_tmpl': f'{self.run}.ice.t@Hz.{fhout_ice_gfs}hr_avg.f#fhr#.nc'}}

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
        dep_dict = {'type': 'metatask', 'name': 'gefs_fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='or')

        postenvars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          'FHR3': '#fhr#',
                          'COMPONENT': component}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        task_name = f'gefs_{component}_prod_mem#member#_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/{config}.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'}

        fhrs = self._get_forecast_hours('gefs', self._configs[config], component)

        # when replaying, atmos component does not have fhr 0, therefore remove 0 from fhrs
        is_replay = self._configs[config]['REPLAY_ICS']
        if is_replay and component in ['atmos'] and 0 in fhrs:
            fhrs.remove(0)

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice'] and 0 in fhrs:
            fhrs.remove(0)

        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}
        if component in ['ocean']:
            fhrs_next = fhrs[1:] + [fhrs[-1] + (fhrs[-1] - fhrs[-2])]
            fhr_var_dict['fhr_next'] = ' '.join([f"{fhr:03d}" for fhr in fhrs_next])

        fhr_metatask_dict = {'task_name': f'gefs_{component}_prod_#member#',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        member_var_dict = {'member': ' '.join([f"{mem:03d}" for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': f'gefs_{component}_prod',
                                'task_dict': fhr_metatask_dict,
                                'var_dict': member_var_dict}

        task = rocoto.create_task(member_metatask_dict)

        return task

    def atmos_ensstat(self):

        resources = self.get_resource('atmos_ensstat')

        deps = []
        for member in range(0, self.nmem + 1):
            task = f'gefs_atmos_prod_mem{member:03d}_f#fhr#'
            dep_dict = {'type': 'task', 'name': task}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        postenvars = self.envars.copy()
        postenvar_dict = {'FHR3': '#fhr#'}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        task_name = f'gefs_atmos_ensstat_f#fhr#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': postenvars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/atmos_ensstat.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'}

        fhrs = self._get_forecast_hours('gefs', self._configs['atmos_ensstat'])

        # when replaying, atmos component does not have fhr 0, therefore remove 0 from fhrs
        is_replay = self._configs['atmos_ensstat']['REPLAY_ICS']
        if is_replay and 0 in fhrs:
            fhrs.remove(0)

        fhr_var_dict = {'fhr': ' '.join([f"{fhr:03d}" for fhr in fhrs])}

        fhr_metatask_dict = {'task_name': f'gefs_atmos_ensstat',
                             'task_dict': task_dict,
                             'var_dict': fhr_var_dict}

        task = rocoto.create_task(fhr_metatask_dict)

        return task

    def wavepostsbs(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'gefs_fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        wave_post_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostsbs')

        task_name = f'gefs_wave_post_grid_mem#member#'
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
        member_metatask_dict = {'task_name': 'gefs_wave_post_grid',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'gefs_fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        wave_post_bndpnt_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_bndpnt_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostbndpnt')
        task_name = f'gefs_wave_post_bndpnt_mem#member#'
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
        member_metatask_dict = {'task_name': 'gefs_wave_post_bndpnt',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostbndpntbll(self):
        deps = []
        atmos_hist_path = self._template_to_rocoto_cycstring(self._base["COM_ATMOS_HISTORY_TMPL"], {'MEMDIR': 'mem#member#'})

        # The wavepostbndpntbll job runs on forecast hours up to FHMAX_WAV_IBP
        last_fhr = self._configs['wave']['FHMAX_WAV_IBP']

        data = f'{atmos_hist_path}/{self.run}.t@Hz.atm.logf{last_fhr:03d}.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))

        dep_dict = {'type': 'metatask', 'name': f'gefs_fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        wave_post_bndpnt_bull_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_bndpnt_bull_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostbndpntbll')
        task_name = f'gefs_wave_post_bndpnt_bull_mem#member#'
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
        member_metatask_dict = {'task_name': 'gefs_wave_post_bndpnt_bull',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def wavepostpnt(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'gefs_fcst_mem#member#'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_wave_bnd']:
            dep_dict = {'type': 'task', 'name': f'gefs_wave_post_bndpnt_bull_mem#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        wave_post_pnt_envars = self.envars.copy()
        postenvar_dict = {'ENSMEM': '#member#',
                          'MEMDIR': 'mem#member#',
                          }
        for key, value in postenvar_dict.items():
            wave_post_pnt_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('wavepostpnt')
        task_name = f'gefs_wave_post_pnt_mem#member#'
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
        member_metatask_dict = {'task_name': 'gefs_wave_post_pnt',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def extractvars(self):
        deps = []
        if self.options['do_wave']:
            dep_dict = {'type': 'task', 'name': 'gefs_wave_post_grid_mem#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_ocean_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_ice_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_atm']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_atmos_prod_#member#'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        extractvars_envars = self.envars.copy()
        extractvars_dict = {'ENSMEM': '#member#',
                            'MEMDIR': 'mem#member#',
                            }
        for key, value in extractvars_dict.items():
            extractvars_envars.append(rocoto.create_envar(name=key, value=str(value)))

        resources = self.get_resource('extractvars')
        task_name = f'gefs_extractvars_mem#member#'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'dependency': dependencies,
                     'envars': extractvars_envars,
                     'cycledef': 'gefs',
                     'command': f'{self.HOMEgfs}/jobs/rocoto/extractvars.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        member_var_dict = {'member': ' '.join([str(mem).zfill(3) for mem in range(0, self.nmem + 1)])}
        member_metatask_dict = {'task_name': 'gefs_extractvars',
                                'task_dict': task_dict,
                                'var_dict': member_var_dict
                                }

        task = rocoto.create_task(member_metatask_dict)

        return task

    def arch(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': 'gefs_atmos_prod'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': 'gefs_atmos_ensstat'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ice']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_ice_prod'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_ocean']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_ocean_prod'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_wave']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_grid'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_pnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_wave_bnd']:
                dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_bndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_bndpnt_bull'}
                deps.append(rocoto.add_dependency(dep_dict))
        if self.options['do_extractvars']:
            dep_dict = {'type': 'metatask', 'name': 'gefs_extractvars'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('arch')
        task_name = 'arch'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'dependency': dependencies,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/arch.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task

    def cleanup(self):
        deps = []
        if self.options['do_extractvars']:
            dep_dict = {'type': 'task', 'name': 'arch'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)
        else:
            dep_dict = {'type': 'metatask', 'name': 'gefs_atmos_prod'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'metatask', 'name': 'gefs_atmos_ensstat'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_ice']:
                dep_dict = {'type': 'metatask', 'name': 'gefs_ice_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_ocean']:
                dep_dict = {'type': 'metatask', 'name': 'gefs_ocean_prod'}
                deps.append(rocoto.add_dependency(dep_dict))
            if self.options['do_wave']:
                dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_grid'}
                deps.append(rocoto.add_dependency(dep_dict))
                dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_pnt'}
                deps.append(rocoto.add_dependency(dep_dict))
                if self.options['do_wave_bnd']:
                    dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_bndpnt'}
                    deps.append(rocoto.add_dependency(dep_dict))
                    dep_dict = {'type': 'metatask', 'name': 'gefs_wave_post_bndpnt_bull'}
                    deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps, dep_condition='and')

        resources = self.get_resource('cleanup')
        task_name = 'gefs_cleanup'
        task_dict = {'task_name': task_name,
                     'resources': resources,
                     'envars': self.envars,
                     'cycledef': 'gefs',
                     'dependency': dependencies,
                     'command': f'{self.HOMEgfs}/jobs/rocoto/cleanup.sh',
                     'job_name': f'{self.pslot}_{task_name}_@H',
                     'log': f'{self.rotdir}/logs/@Y@m@d@H/{task_name}.log',
                     'maxtries': '&MAXTRIES;'
                     }

        task = rocoto.create_task(task_dict)

        return task
