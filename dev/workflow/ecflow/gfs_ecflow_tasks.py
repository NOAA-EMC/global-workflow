#!/usr/bin/env python3

"""
GFS ecFlow task definitions.

Mirrors ``rocoto/gfs_tasks.py``: one method per task, each returning an
ecFlow task dictionary consumed by the suite generator.  Dependencies
are expressed as ecFlow trigger strings rather than Rocoto XML.

The task list and option flags come from ``GFSForecastOnlyAppConfig``
(or the cycled variant); the resources come from ``Tasks.get_resource()``.
"""

from logging import getLogger
from typing import Optional

from applications.applications import AppConfig
from ecflow.ecflow_tasks import EcFlowTasks

logger = getLogger(__name__.split('.')[-1])


class GFSEcFlowTasks(EcFlowTasks):
    """Per-task ecFlow definitions for GFS workflows."""

    def __init__(self, app_config: AppConfig, run: str) -> None:
        super().__init__(app_config, run)

    # ── Simple tasks ──────────────────────────────────────────────────

    def fetch(self):
        return self._simple_task(
            'fetch',
            jjob='JGLOBAL_FETCH',
            resource_name='fetch',
        )

    def stage_ic(self):
        trigger = 'fetch == complete' if self._has_task('fetch') else None
        return self._simple_task(
            'stage_ic',
            jjob='JGLOBAL_STAGE_IC',
            trigger=trigger,
            service=True,
        )

    def aerosol_init(self):
        return self._simple_task(
            'aerosol_init',
            jjob='JGLOBAL_AEROSOL_INIT',
        )

    def waveinit(self):
        return self._simple_task(
            'waveinit',
            jjob='JGLOBAL_WAVE_INIT',
        )

    def fcst(self):
        deps = ['stage_ic == complete']
        if self._has_task('waveinit'):
            deps.append('waveinit == complete')
        if self._has_task('aerosol_init'):
            deps.append('aerosol_init == complete')
        trigger = ' and '.join(deps)

        return self._simple_task(
            'fcst',
            jjob='JGLOBAL_FCST',
            trigger=trigger,
        )

    def atmupp(self):
        return self._simple_task(
            'atmupp',
            jjob='JGLOBAL_ATMOS_UPP',
            resource_name='upp',
            trigger='fcst == complete',
        )

    def goesupp(self):
        return self._simple_task(
            'goesupp',
            jjob='JGLOBAL_ATMOS_UPP',
            resource_name='upp',
            trigger='fcst == complete',
        )

    def tracker(self):
        return self._simple_task(
            'tracker',
            jjob='JGFS_ATMOS_CYCLONE_TRACKER',
            trigger='atmos_prod == complete',
        )

    def genesis(self):
        return self._simple_task(
            'genesis',
            jjob='JGFS_ATMOS_CYCLONE_GENESIS',
            trigger='atmos_prod == complete',
        )

    def genesis_fsu(self):
        return self._simple_task(
            'genesis_fsu',
            jjob='JGFS_ATMOS_CYCLONE_GENESIS_FSU',
            trigger='atmos_prod == complete',
        )

    def metp(self):
        return self._simple_task(
            'metp',
            jjob='JGFS_ATMOS_VERIFICATION',
            trigger='atmos_prod == complete',
        )

    def postsnd(self):
        return self._simple_task(
            'postsnd',
            jjob='JGFS_ATMOS_POSTSND',
            trigger='atmos_prod == complete',
        )

    def gempak(self):
        return self._simple_task(
            'gempak',
            jjob='JGFS_ATMOS_GEMPAK',
            trigger='atmos_prod == complete',
        )

    def gempakmeta(self):
        return self._simple_task(
            'gempakmeta',
            jjob='JGFS_ATMOS_GEMPAK_META',
            trigger='atmos_prod == complete',
        )

    def awips_20km_1p0deg(self):
        return self._simple_task(
            'awips_20km_1p0deg',
            jjob='JGFS_ATMOS_AWIPS_20KM_1P0',
            resource_name='awips',
            trigger='atmos_prod == complete',
        )

    def fbwind(self):
        return self._simple_task(
            'fbwind',
            jjob='JGFS_ATMOS_FBWIND',
            trigger='atmos_prod == complete',
        )

    def wavepostpnt(self):
        return self._simple_task(
            'wavepostpnt',
            jjob='JGLOBAL_WAVE_POST_PNT',
            trigger='fcst == complete',
        )

    def wavepostbndpnt(self):
        return self._simple_task(
            'wavepostbndpnt',
            jjob='JGLOBAL_WAVE_POST_BNDPNT',
            trigger='fcst == complete',
        )

    def wavepostbndpntbll(self):
        return self._simple_task(
            'wavepostbndpntbll',
            jjob='JGLOBAL_WAVE_POST_BNDPNTBLL',
            trigger='fcst == complete',
        )

    def wavegempak(self):
        return self._simple_task(
            'wavegempak',
            jjob='JGFS_WAVE_GEMPAK',
            trigger='wavepostgridded == complete',
        )

    def waveawipsbulls(self):
        return self._simple_task(
            'waveawipsbulls',
            jjob='JGFS_WAVE_AWIPS_BULLS',
            trigger='wavepostgridded == complete',
        )

    def waveawipsgridded(self):
        return self._simple_task(
            'waveawipsgridded',
            jjob='JGFS_WAVE_AWIPS_GRIDDED',
            trigger='wavepostgridded == complete',
        )

    def arch_vrfy(self):
        deps = ['atmos_prod == complete']
        if self._has_task('tracker'):
            deps.append('tracker == complete')
        if self._has_task('genesis'):
            deps.append('genesis == complete')
        if self._has_task('genesis_fsu'):
            deps.append('genesis_fsu == complete')
        if self._has_task('ocean_prod'):
            deps.append('ocean_prod == complete')
        if self._has_task('ice_prod'):
            deps.append('ice_prod == complete')
        if self._has_task('wavepostgridded'):
            deps.append('wavepostgridded == complete')
        if self._has_task('wavepostpnt'):
            deps.append('wavepostpnt == complete')
        if self._has_task('wavepostbndpnt'):
            deps.append('wavepostbndpnt == complete')
        if self._has_task('wavepostbndpntbll'):
            deps.append('wavepostbndpntbll == complete')
        trigger = ' and '.join(deps)

        return self._simple_task(
            'arch_vrfy',
            jjob='JGLOBAL_ARCHIVE_VRFY',
            trigger=trigger,
            service=True,
        )

    def arch_tars(self):
        return self._simple_task(
            'arch_tars',
            jjob='JGLOBAL_ARCHIVE_TARS',
            trigger='arch_vrfy == complete',
        )

    def globus_arch(self):
        return self._simple_task(
            'globus_arch',
            jjob='JGLOBAL_GLOBUS_ARCHIVE',
            resource_name='arch_tars',
            trigger='arch_vrfy == complete',
        )

    def cleanup(self):
        deps = ['arch_vrfy == complete']
        if self._has_task('arch_tars'):
            deps.append('arch_tars == complete')
        if self._has_task('globus_arch'):
            deps.append('globus_arch == complete')
        trigger = ' and '.join(deps)

        return self._simple_task(
            'cleanup',
            jjob='JGLOBAL_CLEANUP',
            trigger=trigger,
        )

    # ── Product tasks (emitted as families with fhr children) ─────────

    def atmos_prod(self):
        return self._product_task(
            'atmos_prod',
            jjob='JGLOBAL_ATMOS_PRODUCTS',
            config='atmos_products',
            component='atmos',
            trigger='fcst == complete',
        )

    def ocean_prod(self):
        return self._product_task(
            'ocean_prod',
            jjob='JGLOBAL_OCEANICE_PRODUCTS',
            config='oceanice_products',
            component='ocean',
            trigger='fcst == complete',
        )

    def ice_prod(self):
        return self._product_task(
            'ice_prod',
            jjob='JGLOBAL_OCEANICE_PRODUCTS',
            config='oceanice_products',
            component='ice',
            trigger='fcst == complete',
        )

    def wavepostgridded(self):
        return self._product_task(
            'wavepostgridded',
            jjob='JGLOBAL_WAVE_POST_GRIDDED',
            config='wavepostgridded',
            component='wave',
            trigger='fcst == complete',
        )
