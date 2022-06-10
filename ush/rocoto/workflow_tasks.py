#!/usr/bin/env python

import numpy as np
import rocoto
import workflow_utils as wfu
from applications import Application


class Tasks:

    def __init__(self, app: Application, cdump: str) -> None:

        self.cdump = cdump
        self.app = app

        # Save dict_configs and base in the internal state (never know where it may be needed)
        self._job_configs = self.app.job_configs
        self._base = self._job_configs['base']

        self.n_tiles = 6  # TODO - this needs to be elsewhere

        if self.app.do_hybvar:
            self.hyb_cycledef = 'enkf'  # TODO - sort this out a bit more

        envar_dict = {'RUN_ENVIR': self._base.get('RUN_ENVIR', 'emc'),
                      'HOMEgfs': self._base.get('HOMEgfs'),
                      'EXPDIR': self._base.get('EXPDIR'),
                      'CDUMP': self.cdump,
                      'CDATE': '<cyclestr>@Y@m@d@H</cyclestr>',
                      'PDY': '<cyclestr>@Y@m@d</cyclestr>',
                      'cyc': '<cyclestr>@H</cyclestr>'}
        self.envars = self._set_envars(envar_dict)

    @staticmethod
    def _set_envars(envar_dict) -> list:

        envars = []
        for key, value in envar_dict.items():
            envars.append(rocoto.create_envar(name=key, value=str(value)))

        return envars

    @staticmethod
    def _get_hybgroups(nens: int, nmem_per_group: int):
        ngrps = nens / nmem_per_group
        groups = ' '.join([f'{x:02d}' for x in range(1, int(ngrps) + 1)])
        return groups

    def coupled_ic(self):

        cpl_ic = self._job_configs['coupled_ic']

        deps = []

        # Atm ICs
        atm_res = self._base.get('CASE', 'C384')
        prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ATMIC']}/@Y@m@d@H/&CDUMP;"
        for file in ['gfs_ctrl.nc'] + \
                    [f'{datatype}_data.tile{tile}.nc'
                        for datatype in ['gfs', 'sfc']
                        for tile in range(1, self.n_tiles + 1)]:
            data = f"{prefix}/{atm_res}/INPUT/{file}"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Ocean ICs
        if self.app.do_ocean:
            ocn_res = f"{self._base.get('OCNRES', '025'):03d}"
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_OCNIC']}/@Y@m@d@H/ocn"
            for res in ['res'] + [f'res_{res_index}' for res_index in range(1, 5)]:
                data = f"{prefix}/{ocn_res}/MOM.{res}.nc"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        # Ice ICs
        if self.app.do_ice:
            ice_res = f"{self._base.get('ICERES', '025'):03d}"
            ice_res_dec = f'{float(ice_res) / 100:.2f}'
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_ICEIC']}/@Y@m@d@H/ice"
            data = f"{prefix}/{ice_res}/cice5_model_{ice_res_dec}.res_@Y@m@d@H.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Wave ICs
        if self.app.do_wave:
            prefix = f"{cpl_ic['BASE_CPLIC']}/{cpl_ic['CPL_WAVIC']}/@Y@m@d@H/wav"
            for wave_grid in self._job_configs['waveinit']['waveGRD'].split():
                data = f"{prefix}/{wave_grid}/@Y@m@d.@H0000.restart.{wave_grid}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('coupled_ic', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def getic(self):

        files = ['INPUT/sfc_data.tile6.nc',
                 'RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc']

        deps = []
        for file in files:
            dep_dict = {'type': 'data', 'data': f'&ROTDIR;/&CDUMP;.@Y@m@d/@H/{file}'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='nor', dep=deps)

        task = wfu.create_wf_task('getic', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def init(self):

        files = ['gfs.t@Hz.sanl',
                 'gfs.t@Hz.atmanl.nemsio',
                 'gfs.t@Hz.atmanl.nc',
                 'atmos/gfs.t@Hz.atmanl.nc',
                 'atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc']

        deps = []
        for file in files:
            dep_dict = {'type': 'data', 'data': f'&ROTDIR;/&CDUMP;.@Y@m@d/@H/{file}'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        if self.app.do_hpssarch:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}getic'}
            dependencies.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        task = wfu.create_wf_task('init', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def prep(self):

        suffix = self._base["SUFFIX"]
        dump_suffix = self._base["DUMP_SUFFIX"]
        gfs_cyc = self._base["gfs_cyc"]
        gfs_enkf = True if self.app.do_hybvar and 'gfs' in self.app.eupd_cdumps else False

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{"gdas"}post', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/gdas.@Y@m@d/@H/atmos/gdas.t@Hz.atmf009{suffix}'
        dep_dict = {'type': 'data', 'data': data, 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&DMPDIR;/{self.cdump}{dump_suffix}.@Y@m@d/@H/{self.cdump}.t@Hz.updated.status.tm00.bufr_d'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        cycledef = self.cdump
        if self.cdump in ['gfs'] and gfs_enkf and gfs_cyc != 4:
            cycledef = 'gdas'

        task = wfu.create_wf_task('prep', cdump=self.cdump, envar=self.envars, dependency=dependencies,
                                  cycledef=cycledef)

        return task

    def waveinit(self):

        task = wfu.create_wf_task('waveinit', cdump=self.cdump, envar=self.envars)

        return task

    def waveprep(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('waveprep', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def aerosol_init(self):

        deps = []
        # Files from current cycle
        files = ['gfs_ctrl.nc'] + [f'gfs_data.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)]
        for file in files:
            data = f'&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/{file}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Previous cycle
        dep_dict = {'type': 'cycleexist', 'offset': f'-{self._base["INTERVAL"]}'}
        deps.append(rocoto.add_dependency(dep_dict))

        # Files from previous cycle
        files = [f'@Y@m@d.@H0000.fv_core.res.nc'] + \
                [f'@Y@m@d.@H0000.fv_core.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)] + \
                [f'@Y@m@d.@H0000.fv_tracer.res.tile{tile}.nc' for tile in range(1, self.n_tiles + 1)]

        for file in files:
            data = ['&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RERUN_RESTART/', file]
            offset = [f'-{self._base["INTERVAL"]}', None]
            dep_dict = {'type': 'data', 'data': data, 'offset': offset}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('aerosol_init', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def anal(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app.do_hybvar:
            dep_dict = {'type': 'metatask', 'name': f'{"gdas"}epmn', 'offset': '-06:00:00'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        else:
            dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('anal', cdump=self.cdump, envar=self.envars, dependency=dependencies)
        return task

    def analcalc(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app.do_hybvar and self.cdump in ['gdas']:
            dep_dict = {'type': 'task', 'name': f'{"gdas"}echgres', 'offset': '-06:00:00'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('analcalc', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def analdiag(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('analdiag', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def gldas(self):

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('gldas', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def fcst(self, fcst_type):

        fcst_map = {'free': self._fcst_free,
                    'cycled': self._fcst_cycled}

        try:
            task = fcst_map[fcst_type]
        except KeyError:
            raise NotImplementedError(f'{fcst_type} is not a valid type.\n' +
                                      'Currently supported forecast types are:\n' +
                                      f'{" | ".join(fcst_map.keys())}')

        return task

    @property
    def _fcst_free(self):
        deps = []
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/sfc_data.tile6.nc'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        if self.app.do_wave and self.cdump in self.app.wave_cdumps:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}waveprep'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app.do_aero:  # TODO - check dependency on cycleexist of previous cycle?
            dep_dict = {'type': 'task', 'name': f'{self.cdump}aerosol_init'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        task = wfu.create_wf_task('fcst', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    @property
    def _fcst_cycled(self):

        dep_dict = {'type': 'task', 'name': f'{self.cdump}anal'}
        dep = rocoto.add_dependency(dep_dict)
        dependencies = rocoto.create_dependency(dep=dep)

        if self.app.do_gldas and self.cdump in ['gdas']:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}gldas'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        if self.app.do_wave and self.cdump in self.app.wave_cdumps:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}waveprep'}
            dependencies.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=dependencies)

        task = wfu.create_wf_task('fcst', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def post(self):
        return self._post_task('post', add_anl_to_post=True)

    def ocnpost(self):
        return self._post_task('ocnpost', add_anl_to_post=False)

    def _post_task(self, task_name, add_anl_to_post=False):
        if task_name not in ['post', 'ocnpost']:
            raise KeyError(f'Invalid post-processing task: {task_name}')

        if task_name in 'ocnpost':
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

            anl = ['anl'] if add_anl else []

            grp = ' '.join(anl + [f'_{fhr[0]}-{fhr[-1]}' for fhr in fhrs])
            dep = ' '.join(anl + [fhr[-1] for fhr in fhrs])
            lst = ' '.join(anl + ['_'.join(fhr) for fhr in fhrs])

            return grp, dep, lst

        deps = []
        data = f'&ROTDIR;/{self.cdump}.@Y@m@d/@H/atmos/{self.cdump}.t@Hz.log#dep#.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        postenvars = self.envars
        postenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#',
                          'ROTDIR': self._base.get('ROTDIR')}
        for key, value in postenvar_dict.items():
            postenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_postgroups(self.cdump, self._job_configs[task_name], add_anl=add_anl_to_post)
        vardict = {varname2: varval2, varname3: varval3}

        task = wfu.create_wf_task(task_name, cdump=self.cdump, envar=postenvars, dependency=dependencies,
                                  metatask=task_name, varname=varname1, varval=varval1, vardict=vardict)

        return task

    def wavepostsbs(self):
        deps = []
        for wave_grid in self._job_configs['wavepostsbs']['waveGRD'].split():
            data = f'&ROTDIR;/{self.cdump}.@Y@m@d/@H/wave/rundata/{self.cdump}wave.out_grd.{wave_grid}.@Y@m@d.@H0000'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('wavepostsbs', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavepostbndpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('wavepostbndpnt', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavepostbndpntbll(self):
        deps = []
        data = f'&ROTDIR;/{self.cdump}.@Y@m@d/@H/atmos/{self.cdump}.t@Hz.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('wavepostbndpntbll', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavepostpnt(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        if self.app.do_wave_bnd:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostbndpntbll'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('wavepostpnt', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wavegempak(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('wavegempak', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def waveawipsbulls(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostpnt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('waveawipsbulls', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def waveawipsgridded(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('waveawipsgridded', cdump=self.cdump, envar=self.envars, dependency=dependencies)

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

        deps = []
        fhrlst = [6] + [*range(12, 36 + 3, 3)]
        for fhr in fhrlst:
            data = f'&ROTDIR;/{self.cdump}.@Y@m@d/@H/atmos/{self.cdump}.t@Hz.wafs.grb2if{fhr:03d}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task(task_name, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wafsblending(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wafsgrib2'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wafsblending', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def wafsblending0p25(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}wafsgrib20p25'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wafsblending0p25', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def postsnd(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('postsnd', cdump=self.cdump, envar=self.envars, dependency=dependencies)

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
                fhrs_hf = range(fhmin, fhmax_hf + fhout_hf, fhout_hf)
                fhrs = fhrs_hf + range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout)

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

        awipsenvars = self.envars
        awipsenvar_dict = {'FHRGRP': '#grp#',
                           'FHRLST': '#lst#',
                           'ROTDIR': self._base.get('ROTDIR')}
        for key, value in awipsenvar_dict.items():
            awipsenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_awipsgroups(self.cdump, self._job_configs['awips'])
        vardict = {varname2: varval2, varname3: varval3}

        task = wfu.create_wf_task('awips', cdump=self.cdump, envar=awipsenvars, dependency=dependencies,
                                  metatask='awips', varname=varname1, varval=varval1, vardict=vardict)

        return task

    def gempak(self):

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('gempak', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def vrfy(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('vrfy', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def metp(self):
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        metpenvars = self.envars
        metpenvar_dict = {'SDATE_GFS': self._base.get('SDATE_GFS'),
                          # TODO - in Forecast-only, this is `SDATE` on the RHS
                          'METPCASE': '#metpcase#'}
        for key, value in metpenvar_dict.items():
            metpenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1 = 'metpcase'
        varval1 = 'g2g1 g2o1 pcp1'

        task = wfu.create_wf_task('metp', cdump=self.cdump, envar=metpenvars, dependency=dependencies,
                                  metatask='metp', varname=varname1, varval=varval1)

        return task

    def arch(self):
        deps = []
        if self.app.do_vrfy:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}vrfy'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.app.do_metp and self.cdump in ['gfs']:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}metp'}
            deps.append(rocoto.add_dependency(dep_dict))
        if self.app.do_wave:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostsbs'}
            deps.append(rocoto.add_dependency(dep_dict))
            dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostpnt'}
            deps.append(rocoto.add_dependency(dep_dict))
            if self.app.do_wave_bnd and self.cdump in ['gfs']:
                dep_dict = {'type': 'task', 'name': f'{self.cdump}wavepostbndpnt'}
                deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('arch', cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    # Start of ensemble tasks
    def eobs(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}prep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': f'{"gdas"}epmn', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('eobs', cdump=self.cdump, envar=self.envars, dependency=dependencies,
                                  cycledef=self.hyb_cycledef)

        return task

    def eomg(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        eomgenvars = self.envars
        eomgenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENKF'], self._job_configs['eobs']['NMEM_EOMGGRP'])

        task = wfu.create_wf_task('eomg', cdump=self.cdump, envar=eomgenvars, dependency=dependencies,
                                  metatask='eomn', varname='grp', varval=groups,
                                  cycledef=self.hyb_cycledef)

        return task

    def ediag(self):
        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}eobs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('ediag', cdump=self.cdump, envar=self.envars, dependency=dependencies,
                                  cycledef=self.hyb_cycledef)

        return task

    def eupd(self):
        deps = []
        if self.app.lobsdiag_forenkf:
            dep_dict = {'type': 'task', 'name': f'{self.cdump}ediag'}
        else:
            dep_dict = {'type': 'metatask', 'name': f'{self.cdump}eomn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        task = wfu.create_wf_task('eupd', cdump=self.cdump, envar=self.envars, dependency=dependencies,
                                  cycledef=self.hyb_cycledef)

        return task

    @staticmethod
    def is_this_a_gdas_task(cdump, task_name):
        if cdump != 'gdas':
            raise TypeError(f'{task_name} must be part of the "gdas" cycle and not {cdump}')

    def ecen(self):

        self.is_this_a_gdas_task(self.cdump, 'ecen')

        def _get_ecengroups():

            if self._base.get('DOIAU_ENKF', False):
                fhrs = list(self._base.get('IAUFHRS', '6').split(','))

                necengrp = self._job_configs['ecen']['NECENGRP']
                ngrps = necengrp if len(fhrs) > necengrp else len(fhrs)

                fhrs = [f'{fhr:03d}' for fhr in fhrs]
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

        eupd_cdump = 'gdas' if 'gdas' in self.app.eupd_cdumps else 'gfs'

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{eupd_cdump}eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        ecenenvars = self.envars
        ecenenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in ecenenvar_dict.items():
            ecenenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_ecengroups()
        vardict = {varname2: varval2, varname3: varval3}

        task = wfu.create_wf_task('ecen', cdump=self.cdump, envar=ecenenvars, dependency=dependencies,
                                  metatask='ecmn', varname=varname1, varval=varval1, vardict=vardict,
                                  cycledef=self.hyb_cycledef)
        return task

    def esfc(self):

        self.is_this_a_gdas_task(self.cdump, 'esfc')

        eupd_cdump = 'gdas' if 'gdas' in self.app.eupd_cdumps else 'gfs'

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}analcalc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{eupd_cdump}eupd'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('esfc', cdump='gdas', envar=self.envars, dependency=dependencies,
                                  cycledef=self.hyb_cycledef)

        return task

    def efcs(self):

        self.is_this_a_gdas_task(self.cdump, 'efcs')

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}ecmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': f'{self.cdump}esfc'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        efcsenvars = self.envars
        efcsenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENKF'], self._job_configs['efcs']['NMEM_EFCSGRP'])

        task = wfu.create_wf_task('efcs', cdump=self.cdump, envar=efcsenvars, dependency=dependencies,
                                  metatask='efmn', varname='grp', varval=groups,
                                  cycledef=self.hyb_cycledef)

        return task

    def echgres(self):

        self.is_this_a_gdas_task(self.cdump, 'echgres')

        deps = []
        dep_dict = {'type': 'task', 'name': f'{self.cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}efmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('echgres', cdump=self.cdump, envar=self.envars, dependency=dependencies,
                                  cycledef=self.hyb_cycledef)

        return task

    def epos(self):

        self.is_this_a_gdas_task(self.cdump, 'epos')

        def _get_eposgroups(epos):
            fhmin = epos['FHMIN_ENKF']
            fhmax = epos['FHMAX_ENKF']
            fhout = epos['FHOUT_ENKF']
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

        eposenvars = self.envars
        eposenvar_dict = {'FHRGRP': '#grp#',
                          'FHRLST': '#lst#'}
        for key, value in eposenvar_dict.items():
            eposenvars.append(rocoto.create_envar(name=key, value=str(value)))

        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = _get_eposgroups(self._job_configs['epos'])
        vardict = {varname2: varval2, varname3: varval3}

        task = wfu.create_wf_task('epos', cdump=self.cdump, envar=eposenvars, dependency=dependencies,
                                  metatask='epmn', varname=varname1, varval=varval1, vardict=vardict,
                                  cycledef=self.hyb_cycledef)

        return task

    def earc(self):

        self.is_this_a_gdas_task(self.cdump, 'earc')

        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{self.cdump}epmn'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)

        earcenvars = self.envars
        earcenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENKF'], self._job_configs['efcs']['NMEM_EARCGRP'])

        task = wfu.create_wf_task('earc', cdump=self.cdump, envar=earcenvars, dependency=dependencies,
                                  metatask='eamn', varname='grp', varval=groups,
                                  cycledef=self.hyb_cycledef)

        return task
