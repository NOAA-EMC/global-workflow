#!/usr/bin/env python3

"""
GFS forecast-only ecFlow suite generator.

Generates a complete ``.def`` file from the same ``AppConfig`` and task
data that the Rocoto XML generator uses.  The output defines workflow
structure (families, tasks, triggers, edit variables).  Per-task Slurm
resources (``#SBATCH`` directives) are baked directly into the ``.ecf``
scripts at config time, matching the production WCOSS2 pattern where
``#PBS`` directives live inside the ``.ecf`` files.

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
from rocoto.tasks import Tasks
from wxflow import timedelta_to_HMS

logger = getLogger(__name__.split('.')[-1])


class GFSForecastOnlyEcFlowSuite(EcFlowSuite):
    """
    ecFlow suite generator for GFS forecast-only workflows.

    Produces a ``.def`` file that mirrors the Rocoto XML for the same
    ``AppConfig``.  Workflow variables (paths, identity, cycle info)
    and per-task Slurm resources (``#SBATCH`` directives) are baked
    directly into the ``.ecf`` scripts at config time — matching the
    production WCOSS2 pattern where scheduler directives live inside
    the job scripts.

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

    # Maps task names to script category subdirectories.
    # Mirrors the layout of dev/ecflow/scripts/.
    TASK_CATEGORY = {
        'stage_ic': 'init',
        'fetch': 'init',
        'aerosol_init': 'init',
        'waveinit': 'init',
        'fcst': 'forecast',
        'atmos_prod': 'product',
        'ocean_prod': 'product',
        'ice_prod': 'product',
        'wavepostgridded': 'product',
        'atmupp': 'product',
        'goesupp': 'product',
        'tracker': 'track',
        'genesis': 'track',
        'genesis_fsu': 'track',
        'metp': 'verf',
        'wavepostpnt': 'verf',
        'wavepostbndpnt': 'verf',
        'wavepostbndpntbll': 'verf',
        'wavegempak': 'verf',
        'waveawipsbulls': 'verf',
        'waveawipsgridded': 'verf',
        'postsnd': 'verf',
        'gempak': 'verf',
        'gempakmeta': 'verf',
        'awips_20km_1p0deg': 'verf',
        'fbwind': 'verf',
        'arch_vrfy': 'post',
        'arch_tars': 'post',
        'globus_arch': 'post',
        'cleanup': 'post',
    }

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

        # Script source directories.
        self._ecf_scripts_dir = Path(self.expdir) / 'ecf_scripts'
        self._ecf_src_dir = Path(
            os.environ.get('ECF_FILES',
                           os.path.join(self.HOMEglobal, 'dev', 'ecflow',
                                        'scripts')))
        self._ecf_include_dir = Path(
            os.environ.get('ECF_INCLUDE',
                           os.path.join(self.HOMEglobal, 'dev', 'ecflow',
                                        'include')))

        # {dest_name: (source_ecf_name, category)}
        self._copy_map: Dict[str, tuple] = {}

        sdate = self._base['SDATE_GFS']
        cycle_str = sdate.strftime('%Y%m%d%H')

        # Absolute path prefix for cross-category trigger references
        self._trigger_base = f'/{suite_name}/{cycle_str}/{self._run}'

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

        # Emit tasks grouped by category family
        from collections import OrderedDict
        category_tasks: Dict[str, List[str]] = OrderedDict()
        for task_name in self._task_names:
            cat = self.TASK_CATEGORY.get(task_name, 'post')
            category_tasks.setdefault(cat, []).append(task_name)

        for cat, cat_task_names in category_tasks.items():
            lines.append(f'{" " * indent}family {cat}')
            cat_indent = indent + 2
            for task_name in cat_task_names:
                task_dict = self._tasks.get_ecflow_task(task_name)
                task_lines = self._emit_task(task_dict, cat_indent)
                lines += task_lines
                lines.append('')
            lines.append(f'{" " * indent}endfamily')
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
        # ecFlow can write .job files.
        rotdir = self._base.get(
            'ROTDIR',
            os.path.join(str(self._base.get('COMROOT', '/tmp')), self.pslot))
        ecf_home = os.path.join(rotdir, 'logs')
        sdate = self._base['SDATE_GFS']
        cycle_str = sdate.strftime('%Y%m%d%H')
        run_dir = os.path.join(ecf_home, suite_name, cycle_str, self._run)
        for task_name in self._task_names:
            cat = self.TASK_CATEGORY.get(task_name, 'post')
            td = self._tasks.get_ecflow_task(task_name)
            cat_dir = os.path.join(run_dir, cat)
            if td['product_task']:
                os.makedirs(os.path.join(cat_dir, task_name), exist_ok=True)
            else:
                os.makedirs(cat_dir, exist_ok=True)

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

    def _resolve_trigger(self, trigger: str, task_category: str,
                         extra_depth: int = 0) -> str:
        """Rewrite trigger references to use absolute paths across category families.

        A trigger like ``stage_ic == complete`` from a task in the
        ``forecast`` category becomes
        ``/suite/cycle/gfs/init/stage_ic == complete``.
        Same-category references are left as bare names.

        Parameters
        ----------
        trigger : str
            Raw trigger expression with bare task names.
        task_category : str
            Category of the task that owns this trigger.
        extra_depth : int
            Unused (kept for API compatibility).
        """
        import re
        base = self._trigger_base

        def _rewrite(match):
            name = match.group(1)
            ref_cat = self.TASK_CATEGORY.get(name)
            if ref_cat and ref_cat != task_category:
                return f'{base}/{ref_cat}/{name}'
            return name

        return re.sub(r'(\b\w+)\s*==', lambda m: _rewrite(m) + ' ==', trigger)

    @staticmethod
    def _scaled_resources(resources: Dict, group_size: int) -> Dict:
        """Return a copy of *resources* with walltime scaled by *group_size*."""
        scaled = dict(resources)
        scaled['walltime'] = Tasks.multiply_HMS(resources['walltime'], group_size)
        return scaled

    def _sbatch_header(self, resources: Dict, task_name: str) -> str:
        """Generate ``#SBATCH`` directive lines for a task.

        Parameters
        ----------
        resources : dict
            Resource dict from ``get_resource()``.
        task_name : str
            Used for the ``--job-name``.

        Returns
        -------
        str
            Multi-line string of ``#SBATCH`` directives (no shebang).
        """
        run = self._run
        cyc = self._base['SDATE_GFS'].strftime('%H')
        account = resources.get('account', '')
        if not account or account == 'UNDEFINED':
            account = os.environ.get('HPC_ACCOUNT', 'fv3-cpu')

        lines = [
            f"#SBATCH --job-name={run}_{task_name}_{cyc}",
            f"#SBATCH --account={account}",
            f"#SBATCH --partition={resources['partition']}",
            f"#SBATCH --time={resources['walltime']}",
            f"#SBATCH --nodes={resources['nodes']}",
            f"#SBATCH --ntasks-per-node={resources['ppn']}",
            f"#SBATCH --cpus-per-task={resources['threads']}",
            "#SBATCH --output=%ECF_JOBOUT%",
        ]
        if resources.get('native'):
            lines.append(f"#SBATCH {resources['native']}")
        return '\n'.join(lines)

    def _emit_simple_task(self, task_dict: Dict, indent: int) -> List[str]:
        """Emit a single non-product task node."""
        sp = ' ' * indent
        tsp = ' ' * (indent + 2)
        lines = []

        task_name = task_dict['task_name']
        trigger = task_dict['trigger']
        category = self.TASK_CATEGORY.get(task_name, 'post')

        self._copy_map[task_name] = (task_name, category, category, task_dict['resources'])

        lines.append(f'{sp}task {task_name}')
        lines.append(f"{tsp}edit STEP '{task_dict['step']}'")

        if trigger:
            trigger = self._resolve_trigger(trigger, category)
            lines.append(f'{tsp}trigger {trigger}')

        return lines

    def _emit_product_family(self, task_dict: Dict, indent: int) -> List[str]:
        """Emit a product task as a family of per-forecast-hour-group children."""
        sp = ' ' * indent
        fsp = ' ' * (indent + 2)
        tsp = ' ' * (indent + 4)
        lines = []

        task_name = task_dict['task_name']
        trigger = task_dict['trigger']
        fhrs = task_dict['forecast_hours']
        config_name = task_dict['config']
        category = self.TASK_CATEGORY.get(task_name, 'product')

        max_tasks = self._configs.get(config_name, {}).get('MAX_TASKS', 25)
        ngroups = min(max_tasks, len(fhrs))
        groups = self._group_fhrs(fhrs, ngroups)

        lines.append(f'{sp}family {task_name}')
        lines.append(f"{fsp}edit STEP '{task_dict['step']}'")
        lines.append(f"{fsp}# {len(fhrs)} forecast hours in {ngroups} groups")

        if trigger:
            trigger = self._resolve_trigger(trigger, category, extra_depth=1)
            lines.append(f'{fsp}trigger {trigger}')

        lines.append('')

        for i, grp in enumerate(groups):
            if len(grp) == 1:
                label = f'f{grp[0]:03d}'
            else:
                label = f'f{grp[0]:03d}_f{grp[-1]:03d}'

            child_res = self._scaled_resources(task_dict['resources'], len(grp))
            self._copy_map[label] = (task_name, category,
                                     f'{category}/{task_name}', child_res)

            fhr_list_str = ','.join(str(f) for f in grp)

            lines.append(f'{fsp}task {label}')
            lines.append(f"{tsp}edit FHR_LIST '{fhr_list_str}'")
            lines.append('')

        lines.append(f'{sp}endfamily')

        return lines

    # ── ecf_scripts management ────────────────────────────────────────

    def _create_ecf_scripts(self) -> None:
        """Populate the ECF_FILES directory under EXPDIR.

        Creates a structured tree::

            {EXPDIR}/ecf_scripts/
              include/          ← head.h, tail.h, envir.h
              scripts/
                init/           ← stage_ic.ecf
                forecast/       ← fcst.ecf
                product/        ← atmos_prod.ecf, f000.ecf, ...
                track/          ← tracker.ecf, genesis.ecf
                verf/           ← metp.ecf
                post/           ← arch_tars.ecf, arch_vrfy.ecf, cleanup.ecf

        Each ``.ecf`` copy gets ``#SBATCH`` directives injected after
        the shebang line, with resource values resolved from the task's
        ``get_resource()`` data.  This matches the production WCOSS2
        pattern where ``#PBS`` directives are baked into the ``.ecf``.
        """
        import shutil

        base_dir = self._ecf_scripts_dir
        src_dir = self._ecf_src_dir
        include_src = self._ecf_include_dir

        if base_dir.exists():
            shutil.rmtree(str(base_dir))
        base_dir.mkdir(parents=True)

        # Copy include files
        dest_include = base_dir / 'include'
        dest_include.mkdir()
        for hdr in include_src.iterdir():
            if hdr.is_file():
                shutil.copy2(str(hdr), str(dest_include / hdr.name))

        # Copy .ecf scripts with #SBATCH injection
        dest_scripts = base_dir / 'scripts'
        headers_dir = base_dir / 'sbatch_headers'
        headers_dir.mkdir()
        skipped = []
        for dest_name, (src_name, src_cat, dest_subdir, resources) in self._copy_map.items():
            dest_dir = dest_scripts / dest_subdir
            dest_dir.mkdir(parents=True, exist_ok=True)

            dest = dest_dir / f'{dest_name}.ecf'
            src = src_dir / src_cat / f'{src_name}.ecf'
            if not src.is_file():
                skipped.append(f'{src_cat}/{src_name}.ecf')
                continue

            content = src.read_text()
            sbatch = self._sbatch_header(resources, dest_name)

            # Save the #SBATCH header for sync_ecf_scripts.sh
            hdr_file = headers_dir / f'{dest_name}.hdr'
            hdr_file.write_text(sbatch + '\n')

            # Insert #SBATCH directives after the #!/bin/bash shebang
            if content.startswith('#!/bin/bash\n'):
                content = '#!/bin/bash\n' + sbatch + '\n' + content[len('#!/bin/bash\n'):]
            else:
                content = '#!/bin/bash\n' + sbatch + '\n' + content

            dest.write_text(content)

        # Write manifest
        manifest = base_dir / 'ecf_scripts.manifest'
        with open(manifest, 'w') as fh:
            fh.write(f'# ECF_SRC_DIR={self._ecf_src_dir}\n')
            for dest_name, (src_name, src_cat, dest_subdir, _res) in sorted(self._copy_map.items()):
                fh.write(f'{dest_subdir}/{dest_name}\t{src_cat}/{src_name}\n')

        copied = len(self._copy_map) - len(skipped)
        logger.info(f'Copied {copied} .ecf files to {base_dir}')
        if skipped:
            unique = sorted(set(skipped))
            logger.warning(f'Missing source .ecf (skipped): {", ".join(unique)}')

    # ── Suite-level variables ─────────────────────────────────────────

    def _suite_variables(self, indent: int = 2) -> List[str]:
        """Emit suite-level edit variables."""
        sp = ' ' * indent
        base = self._base
        lines = []

        default_rotdir = os.path.join(str(base.get('COMROOT', '/tmp')),
                                      self.pslot)
        rotdir = base.get('ROTDIR', default_rotdir)
        ecf_log_dir = os.path.join(rotdir, 'logs')

        ecf_base_dir = os.path.join(self.expdir, 'ecf_scripts')
        ecf_scripts_dir = os.path.join(ecf_base_dir, 'scripts')
        ecf_include_dir = os.path.join(ecf_base_dir, 'include')

        lines.append(f"{sp}# File locations")
        lines.append(f"{sp}edit ECF_HOME    '{ecf_log_dir}'")
        lines.append(f"{sp}edit ECF_INCLUDE '{ecf_include_dir}'")
        lines.append(f"{sp}edit ECF_FILES   '{ecf_scripts_dir}'")
        lines.append(f"{sp}edit ECF_JOBOUT  '{ecf_log_dir}/%TASK%.%ECF_TRYNO%'")
        lines.append(f"{sp}")

        lines.append(f"{sp}# Slurm job submission — #SBATCH directives are baked into .ecf files")
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
