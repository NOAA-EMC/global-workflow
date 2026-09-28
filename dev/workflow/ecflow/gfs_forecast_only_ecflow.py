#!/usr/bin/env python3

"""
GFS forecast-only ecFlow suite generator.

Generates a complete ``.def`` file from the same ``AppConfig`` and task
data that the Rocoto XML generator uses.  The output defines workflow
structure (families, tasks, triggers, edit variables) including per-task
Slurm resource variables (WALLTIME, NODES, etc.) consumed by the
``slurm.h`` header that each ``.ecf`` script includes.  This mirrors
the production WCOSS2 pattern where ``#PBS`` directives live inside the
``.ecf`` files rather than in an external submission wrapper.

Task metadata (triggers, J-Job mapping, product-task flags, resources)
comes from the ``EcFlowTasks`` hierarchy (mirroring
``rocoto/gfs_tasks.py``), instantiated via ``ecflow_tasks_factory``.
This suite generator is a pure consumer — it iterates the task list,
fetches each task dict, and renders it into the ``.def`` format.
"""

import os
from logging import getLogger
from pathlib import Path
from typing import Dict, List

from ecflow.ecflow_suite import EcFlowSuite
from ecflow.ecflow_tasks_factory import ecflow_tasks_factory
from applications.applications import AppConfig
from wxflow import timedelta_to_HMS

logger = getLogger(__name__.split('.')[-1])


class GFSForecastOnlyEcFlowSuite(EcFlowSuite):
    """
    ecFlow suite generator for GFS forecast-only workflows.

    Produces a ``.def`` file that mirrors the Rocoto XML for the same
    ``AppConfig``.  Workflow variables (paths, identity, cycle info)
    and per-task Slurm resources (walltime, nodes, partition, etc.)
    are baked into the ``.def`` as ``edit`` variables.  The ``.ecf``
    scripts include ``slurm.h`` which emits ``#SBATCH`` directives
    from those variables — matching the production WCOSS2 pattern
    where scheduler directives live inside the job scripts.

    Bootstrap sequence::

        ecflow_client --load=<path>.def
        ecflow_client --begin=<suite_name>

    Parameters
    ----------
    app_config : AppConfig
        Application configuration object containing GFS settings.
    ecflow_config : Dict
        Dictionary containing ecFlow-specific configuration
        (currently only ``verbosity``).
    """

    def __init__(self, app_config: AppConfig, ecflow_config: Dict) -> None:
        super().__init__(app_config, ecflow_config)

        self._run = list(app_config.task_names.keys())[0]  # 'gfs'
        self._task_names = app_config.task_names[self._run]
        self._options = app_config.run_options[self._run]
        self._configs = app_config.configs[self._run]

        # Create the tasks object via factory (keyed on NET, e.g. 'gfs')
        self._tasks = ecflow_tasks_factory.create(
            self._base['NET'], app_config, self._run)

    # ── Public interface ──────────────────────────────────────────────

    def get_cycledefs(self):
        """Return a human-readable cycle summary (not used in .def output)."""
        sdate = self._base['SDATE_GFS']
        edate = self._base['EDATE']
        interval = self._base['interval_gfs']
        return (f"# Cycles: {sdate.strftime('%Y%m%d%H')} – "
                f"{edate.strftime('%Y%m%d%H')} every "
                f"{timedelta_to_HMS(interval)}")

    def write(self, def_file: str = None) -> str:
        """
        Generate the ecFlow ``.def`` file, create the ECF_FILES script
        directory, and write both to disk.

        Parameters
        ----------
        def_file : str, optional
            Output path.  Defaults to ``{EXPDIR}/{pslot}.def``.

        Returns
        -------
        str
            The path of the written ``.def`` file.
        """
        if def_file is None:
            def_file = os.path.join(self.expdir, f'{self.pslot}.def')

        suite_name = self.pslot

        # Script directory: all .ecf lookups resolve here.
        self._ecf_scripts_dir = Path(self.expdir) / 'ecf_scripts'
        self._ecf_src_dir = Path(
            os.environ.get('ECF_FILES',
                           os.path.join(self.HOMEglobal, 'dev', 'ecflow',
                                        'scripts')))

        # Collect copies to create: {dest_name: source_ecf_name}
        self._copy_map: Dict[str, str] = {}

        lines: List[str] = []
        lines.append(f'# Auto-generated ecFlow suite definition for {suite_name}')
        lines.append(f'# Mode: {self._app_config.mode}  NET: {self._base["NET"]}')
        lines.append(f'# {self.get_cycledefs()}')
        lines.append('')

        # ── Suite header ──────────────────────────────────────────────
        lines.append(f'suite {suite_name}')
        lines += self._suite_variables(indent=2)
        lines.append('')

        # ── Cycle family (e.g. "2021032312") ─────────────────────────
        indent = 2
        sdate = self._base['SDATE_GFS']
        cycle_str = sdate.strftime('%Y%m%d%H')
        lines.append(f'{" " * indent}family {cycle_str}')
        indent = 4
        lines.append(f'{" " * indent}edit PDY \'{sdate.strftime("%Y%m%d")}\'')
        lines.append(f'{" " * indent}edit CYC \'{sdate.strftime("%H")}\'')
        lines.append('')

        # ── RUN family (e.g. "gfs") ──────────────────────────────────
        lines.append(f'{" " * indent}family {self._run}')
        indent = 6
        lines.append(f'{" " * indent}edit RUN \'{self._run}\'')
        lines.append('')

        # Emit tasks from the tasks object
        for task_name in self._task_names:
            task_dict = self._tasks.get_ecflow_task(task_name)
            task_lines = self._emit_task(task_dict, indent)
            lines += task_lines
            lines.append('')

        # Close cycle family
        indent = 4
        lines.append(f'{" " * indent}endfamily')

        indent = 2
        lines.append(f'{" " * indent}endfamily')
        lines.append('endsuite')
        lines.append('')

        def_content = '\n'.join(lines)

        os.makedirs(os.path.dirname(def_file), exist_ok=True)
        with open(def_file, 'w') as fh:
            fh.write(def_content)

        logger.info(f'ecFlow suite definition written to {def_file}')

        # Create ECF_HOME subdirectories matching the suite tree so
        # ecFlow can write .job files before ECF_JOB_CMD runs.
        rotdir = self._base.get(
            'ROTDIR',
            os.path.join(str(self._base.get('COMROOT', '/tmp')), self.pslot))
        ecf_home = os.path.join(rotdir, 'logs')
        sdate = self._base['SDATE_GFS']
        cycle_str = sdate.strftime('%Y%m%d%H')
        job_dir = os.path.join(ecf_home, suite_name, cycle_str, self._run)
        os.makedirs(job_dir, exist_ok=True)
        # Product families create a subdirectory per family
        for task_name in self._task_names:
            td = self._tasks.get_ecflow_task(task_name)
            if td['product_task']:
                os.makedirs(os.path.join(job_dir, task_name), exist_ok=True)

        # Create the ecf_scripts directory with copies
        self._create_ecf_scripts()

        return def_file

    # ── Task rendering ────────────────────────────────────────────────

    def _emit_task(self, task_dict: Dict, indent: int) -> List[str]:
        """
        Render an ecFlow task dict into .def lines.

        Dispatches to ``_emit_product_family`` for product tasks or
        ``_emit_simple_task`` for all others.
        """
        if task_dict['product_task']:
            return self._emit_product_family(task_dict, indent)
        return self._emit_simple_task(task_dict, indent)

    def _resource_edits(self, resources: Dict, indent: int) -> List[str]:
        """Emit Slurm resource ``edit`` variables for a task or family.

        These variables are consumed by ``slurm.h`` via ``#SBATCH``
        directives in the ``.ecf`` scripts.

        Parameters
        ----------
        resources : dict
            Resource dict from ``get_resource()`` with keys: walltime,
            nodes, ppn, threads, partition, native.
        indent : int
            Current indentation level (number of spaces).

        Returns
        -------
        list of str
            Lines of ``edit VAR 'value'`` statements.
        """
        sp = ' ' * indent
        lines = []
        lines.append(f"{sp}edit WALLTIME '{resources['walltime']}'")
        lines.append(f"{sp}edit NODES '{resources['nodes']}'")
        lines.append(f"{sp}edit TASKS_PER_NODE '{resources['ppn']}'")
        lines.append(f"{sp}edit THREADS_PER_TASK '{resources['threads']}'")
        if resources.get('partition'):
            lines.append(f"{sp}edit PARTITION '{resources['partition']}'")
        if resources.get('native'):
            lines.append(f"{sp}edit NATIVE '{resources['native']}'")
        return lines

    def _emit_simple_task(self, task_dict: Dict, indent: int) -> List[str]:
        """Emit a single non-product task node with Slurm resources."""
        sp = ' ' * indent
        tsp = ' ' * (indent + 2)
        lines = []

        task_name = task_dict['task_name']
        trigger = task_dict['trigger']

        self._copy_map[task_name] = task_name

        lines.append(f'{sp}task {task_name}')
        lines.append(f"{tsp}edit STEP '{task_dict['step']}'")
        lines += self._resource_edits(task_dict['resources'], indent + 2)

        if trigger:
            lines.append(f'{tsp}trigger {trigger}')

        return lines

    def _emit_product_family(self, task_dict: Dict, indent: int) -> List[str]:
        """Emit a product task as a family of per-forecast-hour-group children.

        Slurm resource edits are set on the family node so all child
        tasks inherit them.
        """
        sp = ' ' * indent
        fsp = ' ' * (indent + 2)
        tsp = ' ' * (indent + 4)
        lines = []

        task_name = task_dict['task_name']
        trigger = task_dict['trigger']
        fhrs = task_dict['forecast_hours']
        config_name = task_dict['config']

        max_tasks = self._configs.get(config_name, {}).get('MAX_TASKS', 25)
        ngroups = min(max_tasks, len(fhrs))
        groups = self._group_fhrs(fhrs, ngroups)

        lines.append(f'{sp}family {task_name}')
        lines.append(f"{fsp}edit STEP '{task_dict['step']}'")
        lines += self._resource_edits(task_dict['resources'], indent + 2)
        lines.append(f"{fsp}# {len(fhrs)} forecast hours in {ngroups} groups")

        if trigger:
            lines.append(f'{fsp}trigger {trigger}')

        lines.append('')

        for i, grp in enumerate(groups):
            if len(grp) == 1:
                label = f'f{grp[0]:03d}'
            else:
                label = f'f{grp[0]:03d}_f{grp[-1]:03d}'

            self._copy_map[label] = task_name

            fhr_list_str = ','.join(str(f) for f in grp)

            lines.append(f'{fsp}task {label}')
            lines.append(f"{tsp}edit FHR_LIST '{fhr_list_str}'")
            lines.append('')

        lines.append(f'{sp}endfamily')

        return lines

    # ── ecf_scripts management ────────────────────────────────────────

    def _create_ecf_scripts(self) -> None:
        """Populate the ECF_FILES directory under EXPDIR with copies.

        Creates ``{EXPDIR}/ecf_scripts/`` containing one ``.ecf`` file
        per ecFlow task.  Simple tasks get a copy of their same-named
        source.  Product family children (e.g. ``f000_f002``) get a
        copy of the parent task's ``.ecf`` (e.g. ``atmos_prod.ecf``).

        Also writes ``ecf_scripts.manifest`` — a two-column TSV
        consumed by ``sync_ecf_scripts.sh``.
        """
        scripts_dir = self._ecf_scripts_dir
        src_dir = self._ecf_src_dir

        if scripts_dir.exists():
            for f in scripts_dir.iterdir():
                if f.is_symlink() or f.is_file():
                    f.unlink()
        else:
            scripts_dir.mkdir(parents=True)

        import shutil
        skipped = []
        for dest_name, src_name in self._copy_map.items():
            dest = scripts_dir / f'{dest_name}.ecf'
            src = src_dir / f'{src_name}.ecf'
            if not src.is_file():
                skipped.append(f'{src_name}.ecf')
                continue
            shutil.copy2(str(src), str(dest))

        # Write manifest for sync_ecf_scripts.sh
        manifest = scripts_dir / 'ecf_scripts.manifest'
        with open(manifest, 'w') as fh:
            fh.write(f'# ECF_SRC_DIR={self._ecf_src_dir}\n')
            for dest_name, src_name in sorted(self._copy_map.items()):
                fh.write(f'{dest_name}\t{src_name}\n')

        copied = len(self._copy_map) - len(skipped)
        logger.info(f'Copied {copied} .ecf files to {scripts_dir}')
        if skipped:
            unique = sorted(set(skipped))
            logger.warning(f'Missing source .ecf (skipped): {", ".join(unique)}')

    # ── Suite-level variables ─────────────────────────────────────────

    def _suite_variables(self, indent: int = 2) -> List[str]:
        """Emit suite-level edit variables."""
        sp = ' ' * indent
        base = self._base
        lines = []

        rotdir = base.get('ROTDIR', os.path.join(str(base.get('COMROOT', '/tmp')),
                                                  self.pslot))
        ecf_log_dir = os.path.join(rotdir, 'logs')

        ecf_scripts_dir = os.path.join(self.expdir, 'ecf_scripts')
        ecf_include = os.environ.get('ECF_INCLUDE',
                                     os.path.join(self.HOMEglobal, 'dev', 'ecflow',
                                                  'utils'))

        lines.append(f"{sp}# File locations")
        lines.append(f"{sp}edit ECF_HOME    '{ecf_log_dir}'")
        lines.append(f"{sp}edit ECF_INCLUDE '{ecf_include}'")
        lines.append(f"{sp}edit ECF_FILES   '{ecf_scripts_dir}'")
        lines.append(f"{sp}edit ECF_JOBOUT  '{ecf_log_dir}/%TASK%.%ECF_TRYNO%'")
        lines.append(f"{sp}")

        lines.append(f"{sp}# Slurm job submission — directives are in the .ecf via slurm.h")
        lines.append(f"{sp}edit ECF_JOB_CMD  'sbatch %ECF_JOB%'")
        lines.append(f"{sp}edit ECF_KILL_CMD 'scancel %ECF_RID%'")
        lines.append(f"{sp}edit ECF_STATUS_CMD 'squeue -j %ECF_RID%'")
        lines.append(f"{sp}")

        lines.append(f"{sp}# Experiment variables")
        lines.append(f"{sp}edit ENVIR    '{base.get('envir', 'test')}'")
        lines.append(f"{sp}edit NET      '{base['NET']}'")
        lines.append(f"{sp}edit RUN      '{self._run}'")
        lines.append(f"{sp}edit APP      '{self._options.get('app', 'ATM')}'")
        account = base.get('ACCOUNT', '')
        if not account or account == 'UNDEFINED':
            account = os.environ.get('HPC_ACCOUNT', 'fv3-cpu')
        lines.append(f"{sp}edit ACCOUNT  '{account}'")
        lines.append(f"{sp}edit QUEUE    '{base.get('PARTITION_BATCH', 'batch')}'")
        lines.append(f"{sp}edit PSLOT    '{self.pslot}'")
        lines.append(f"{sp}edit CASE     '{base['CASE']}'")
        lines.append(f"{sp}edit FHMAX_GFS '{base.get('FHMAX_GFS', 120)}'")
        lines.append(f"{sp}")

        sdate = base['SDATE_GFS']
        lines.append(f"{sp}edit PDY      '{sdate.strftime('%Y%m%d')}'")
        lines.append(f"{sp}edit CYC      '{sdate.strftime('%H')}'")
        lines.append(f"{sp}")

        lines.append(f"{sp}# Paths consumed by J-Jobs")
        lines.append(f"{sp}edit HOMEglobal '{self.HOMEglobal}'")
        lines.append(f"{sp}edit EXPDIR     '{self.expdir}'")
        lines.append(f"{sp}edit COMROOT    '{base['COMROOT']}'")
        dataroot = f"{base.get('STMP', '/tmp')}/RUNDIRS/{self.pslot}"
        lines.append(f"{sp}edit DATAROOT   '{dataroot}'")
        lines.append(f"{sp}")

        return lines

    # ── Utility ───────────────────────────────────────────────────────

    @staticmethod
    def _group_fhrs(fhrs: List[int], ngroups: int) -> List[List[int]]:
        """Split forecast hours into *ngroups* roughly equal groups."""
        if ngroups >= len(fhrs):
            return [[f] for f in fhrs]

        groups: List[List[int]] = []
        base_size = len(fhrs) // ngroups
        remainder = len(fhrs) % ngroups
        idx = 0
        for i in range(ngroups):
            size = base_size + (1 if i < remainder else 0)
            groups.append(fhrs[idx:idx + size])
            idx += size
        return groups
