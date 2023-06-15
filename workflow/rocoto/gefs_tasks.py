from applications.applications import AppConfig
from rocoto.tasks import Tasks, create_wf_task
import rocoto.rocoto as rocoto


class GEFSTasks(Tasks):

    def __init__(self, app_config: AppConfig, cdump: str) -> None:
        super().__init__(app_config, cdump)

    def fcst(self):
        # TODO: Add real dependencies
        dependencies = []

        resources = self.get_resource('fcst')
        task = create_wf_task('fcst', resources, cdump=self.cdump, envar=self.envars, dependency=dependencies)

        return task

    def efcs(self):
        # TODO: Add real dependencies
        dependencies = []

        efcsenvars = self.envars.copy()
        efcsenvars.append(rocoto.create_envar(name='ENSGRP', value='#grp#'))

        groups = self._get_hybgroups(self._base['NMEM_ENS'], self._configs['efcs']['NMEM_EFCSGRP'])

        resources = self.get_resource('efcs')
        task = create_wf_task('efcs', resources, cdump=self.cdump, envar=efcsenvars, dependency=dependencies,
                              metatask='efmn', varname='grp', varval=groups, cycledef='gefs')

        return task
