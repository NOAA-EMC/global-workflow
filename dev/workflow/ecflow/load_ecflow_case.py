#!/usr/bin/env python3

"""Generic ecFlow case loader.

Parses a CI case YAML, creates the experiment via setup_expt, generates
the ecFlow .def via setup_workflow (ecflow engine), loads the definition
into the ecFlow server, and optionally begins the suite.

The generated .def contains all edit variables (paths, resources,
partitions) baked in from the experiment's config files, so no
``--alter`` overrides are needed after loading.

Test-specific entry points (e.g. ``c48_atm_ecflow.py``) provide
default YAML paths and delegate to this module's ``run()`` function.

Prerequisites
-------------
1. ecFlow server running (ECF_HOST / ECF_PORT set).
2. global-workflow built and linked.
3. Environment sourced with ECF_HOST, ECF_PORT, ECF_HOME, HOMEglobal::

       python3 dev/workflow/ecflow/load_ecflow_case.py \\
           --yaml dev/ci/cases/pr/<CASE>.yaml
"""

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

# Resolve repo root from this script's location (dev/workflow/ecflow/)
SCRIPT_DIR = Path(__file__).resolve().parent
HOMEglobal = SCRIPT_DIR.parent.parent.parent

# Add workflow and library paths for imports
sys.path.insert(0, str(HOMEglobal / "dev" / "workflow"))
sys.path.insert(0, str(HOMEglobal / "sorc" / "wxflow" / "src"))
sys.path.insert(0, str(HOMEglobal / "ush" / "python"))

import setup_expt  # noqa: E402
import setup_workflow  # noqa: E402
from hosts import Host  # noqa: E402
from wxflow import AttrDict, parse_j2yaml  # noqa: E402

REQUIRED_ENV = ("ECF_HOST", "ECF_PORT", "ECF_HOME", "HOMEglobal")


def ecflow_client(*args: str) -> subprocess.CompletedProcess:
    """Run ecflow_client with the given arguments."""
    cmd = ["ecflow_client", *args]
    try:
        return subprocess.run(cmd, check=True, capture_output=True, text=True,
                              timeout=30)
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] ecflow_client failed: {' '.join(cmd)}")
        if e.stdout:
            print(f"  stdout: {e.stdout.strip()}")
        if e.stderr:
            print(f"  stderr: {e.stderr.strip()}")
        raise


def ecflow_client_quiet(*args: str) -> bool:
    """Run ecflow_client, returning True on success and False on failure."""
    cmd = ["ecflow_client", *args]
    try:
        result = subprocess.run(cmd, capture_output=True, text=True,
                                timeout=30)
        return result.returncode == 0
    except subprocess.TimeoutExpired:
        return False


def parse_args(default_yaml: Path = None) -> argparse.Namespace:
    """Parse command-line arguments.

    Parameters
    ----------
    default_yaml : Path, optional
        Default case YAML path.  When None, ``--yaml`` is required.
    """
    parser = argparse.ArgumentParser(
        description="Create an experiment and load its ecFlow suite "
                    "from a CI case YAML."
    )
    yaml_kwargs = dict(type=Path, help="CI case YAML file")
    if default_yaml is not None:
        yaml_kwargs['default'] = default_yaml
        yaml_kwargs['help'] += f" (default: {default_yaml.relative_to(HOMEglobal)})"
    else:
        yaml_kwargs['required'] = True
    parser.add_argument("-y", "--yaml", **yaml_kwargs)
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite a previously created experiment.",
    )
    parser.add_argument(
        "--suite-name",
        default=None,
        help="ecFlow suite name (default: pslot from the YAML).",
    )
    parser.add_argument(
        "--expdir",
        type=Path,
        default=None,
        help="Override experiment directory base path (the YAML's "
             "expdir value). Sets RUNTESTS so that "
             "EXPDIR = <expdir>/<pslot>.",
    )
    parser.add_argument(
        "--comroot",
        type=Path,
        default=None,
        help="Override COMROOT path. Sets RUNTESTS so that "
             "COMROOT = <comroot>.",
    )
    parser.add_argument(
        "--pslot",
        default=None,
        help="Override experiment name (default: <yaml_stem>_ecflow).",
    )
    parser.add_argument(
        "--stmp",
        type=Path,
        default=None,
        help="Override STMP (scratch/tmp) path used to build "
             "DATAROOT = <stmp>/RUNDIRS/<pslot>.",
    )
    return parser.parse_args()


def validate_environment() -> None:
    """Exit with an error if required environment variables are missing."""
    missing = [v for v in REQUIRED_ENV if not os.environ.get(v)]
    if missing:
        print(f"[ERROR] Missing environment variables: {', '.join(missing)}")
        print()
        print("  Add these to your environment before running:")
        print()
        print("    unset ECF_HOSTFILE")
        print("    module load ecflow")
        print("    export ECF_HOST=<ecflow_server_host>")
        print("    export ECF_PORT=<ecflow_server_port>")
        print("    export ECF_HOME=<path_for_ecflow_job_files>")
        print("    export HOMEglobal=<path_to_global_workflow>")
        print()
        print("  Example (Ursa):")
        print("    unset ECF_HOSTFILE")
        print("    module load ecflow")
        print("    export ECF_HOST=uecflow01")
        print("    export ECF_PORT=23385")
        print("    export ECF_HOME=/scratch3/NCEPDEV/global/$USER/ecflow")
        print("    export HOMEglobal=/scratch3/NCEPDEV/global/$USER/global-workflow")
        sys.exit(1)


def load_case_yaml(yaml_path: Path) -> AttrDict:
    """Parse the CI case YAML with host and environment template variables.

    Sets default values for ``pslot`` and ``RUNTESTS`` in ``os.environ``
    when not already present, so that Jinja2 ``getenv`` filters resolve
    to usable paths instead of the literal string ``UNDEFINED``.
    """
    if not os.environ.get('pslot'):
        os.environ['pslot'] = yaml_path.stem + '_ecflow'
    if not os.environ.get('RUNTESTS'):
        os.environ['RUNTESTS'] = str(HOMEglobal.parent / 'RUNTESTS')

    host = Host()
    data = AttrDict(HOMEglobal=str(HOMEglobal))
    data.update(host.info)
    data.update(os.environ)
    return parse_j2yaml(path=yaml_path, data=data)


def create_experiment(testconf: AttrDict, runtests: Path,
                      overwrite: bool = False) -> Path:
    """Create the experiment via setup_expt.main() and return the EXPDIR."""
    exp = testconf.experiment
    pslot = exp.pslot
    expdir = runtests / "EXPDIR" / pslot

    runtests.mkdir(parents=True, exist_ok=True)

    if expdir.is_dir():
        print("  EXPDIR already exists, recreating with --overwrite.")

    setup_expt_args = [exp.net, exp.mode]
    skip_keys = {"net", "mode", "yaml"}
    for key, val in exp.items():
        if key in skip_keys:
            continue
        setup_expt_args.extend([f"--{key}", str(val)])

    if overwrite:
        setup_expt_args.append("--overwrite")

    setup_expt.main(setup_expt_args)

    config_base = expdir / "config.base"
    if not config_base.is_file():
        print(f"[ERROR] config.base not found in {expdir}.")
        sys.exit(1)

    return expdir


def generate_ecflow_def(expdir: Path) -> Path:
    """Generate the ecFlow .def file via setup_workflow.main(ecflow).

    Returns the path to the generated .def file.
    """
    # Configuration.parse_config diffs env before/after sourcing config
    # files.  Variables already in os.environ are excluded from the
    # result.  Temporarily remove workflow-specific vars so parse_config
    # captures them from the rendered config.base.
    _saved_env = {}
    for var in ('HOMEglobal', 'EXPDIR', 'COMROOT', 'DATAROOT',
                'ROTDIR', 'PSLOT', 'NET', 'RUN',
                'ACCOUNT', 'PARTITION_BATCH', 'PARTITION_SERVICE',
                'QUEUE', 'QUEUE_SERVICE',
                'STMP', 'PTMP', 'HOMEDIR', 'NOSCRUB'):
        if var in os.environ:
            _saved_env[var] = os.environ.pop(var)

    try:
        setup_workflow.main([str(expdir), "ecflow"])
    finally:
        os.environ.update(_saved_env)

    def_files = list(expdir.glob("*.def"))
    if not def_files:
        print(f"[ERROR] No .def file generated in {expdir}.")
        sys.exit(1)

    return def_files[0]


def load_suite(suite_name: str, def_file: Path) -> None:
    """Prompt to delete any existing suite, then load the .def file."""
    # Check if the suite already exists on the server
    if ecflow_client_quiet("--get", f"/{suite_name}"):
        print(f"  Suite '/{suite_name}' already exists on the server.")
        answer = input("  Delete and replace it? [y/N] ").strip().lower()
        if answer not in ('y', 'yes'):
            print("  Aborting load. Existing suite left intact.")
            sys.exit(0)
        print(f"  Stopping /{suite_name}...")
        ecflow_client_quiet("--suspend", f"/{suite_name}")
        ecflow_client_quiet("--kill", f"/{suite_name}")
        import time
        time.sleep(5)
        print(f"  Deleting /{suite_name}...")
        cmd = ["ecflow_client", "--delete=force", "yes",
               f"/{suite_name}"]
        try:
            subprocess.run(cmd, check=True, capture_output=True,
                           text=True, timeout=60)
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            msg = getattr(e, 'stderr', '') or ''
            print(f"[WARN] Delete failed: {msg.strip() if msg else 'timeout'}")
            print(f"  Try manually: ecflow_client --delete=force yes /{suite_name}")
            sys.exit(1)
        print(f"  Deleted /{suite_name}.")
    ecflow_client(f"--load={def_file}")


def cleanup_stale_files(pslot: str, comroot: Path, runtests: Path) -> bool:
    """Remove all runtime directories from a previous run of this case.

    Lists directories to be removed and prompts for confirmation.
    Returns True if cleanup was performed, False if skipped.
    """
    dirs_to_clean = []

    candidates = [
        ("COMROOT", comroot / pslot),
        ("RUNDIRS", runtests / "RUNDIRS" / pslot),
        ("EXPDIR", runtests / "EXPDIR" / pslot),
        # Old ECF_HOME dirs that may have been created inside the repo
        ("repo/ecf (stale)", HOMEglobal / "dev" / "ecflow" / pslot),
        ("repo/ecf (stale)", HOMEglobal / "dev" / "ecflow" / "output"),
    ]

    for label, d in candidates:
        if d.is_dir():
            dirs_to_clean.append((label, d))

    if not dirs_to_clean:
        print("  No previous run directories found.")
        return True

    print("  The following directories will be removed:")
    for label, d in dirs_to_clean:
        print(f"    [{label}] {d}")

    answer = input("  Proceed? [y/N] ").strip().lower()
    if answer not in ('y', 'yes'):
        print("  Skipping cleanup, proceeding without cleaning.")
        return False

    for label, d in dirs_to_clean:
        print(f"  Removing {d}")
        shutil.rmtree(d)
    print("  Clean.")
    return True


def run(default_yaml: Path = None) -> None:
    """Entry point for loading an ecFlow case.

    Parameters
    ----------
    default_yaml : Path, optional
        Default case YAML.  When provided, ``--yaml`` becomes optional.
        Test-specific scripts pass their YAML here; the generic CLI
        requires ``--yaml`` explicitly.
    """
    args = parse_args(default_yaml=default_yaml)

    validate_environment()

    yaml_path = args.yaml.resolve()
    if not yaml_path.is_file():
        print(f"[ERROR] Case YAML not found: {yaml_path}")
        sys.exit(1)

    # CLI overrides take precedence over environment and YAML defaults.
    # Set them in os.environ so load_case_yaml's Jinja2 rendering picks
    # them up via the {{ 'VAR' | getenv }} filters in the case YAML.
    if args.pslot:
        os.environ['pslot'] = args.pslot
    if args.comroot or args.expdir:
        # Both comroot and expdir derive from RUNTESTS in the YAML.
        # When the user provides --comroot, RUNTESTS = comroot.parent.
        # When --expdir is given, RUNTESTS = expdir (since YAML does
        # RUNTESTS/EXPDIR/<pslot>, and --expdir replaces the base).
        if args.comroot:
            os.environ['RUNTESTS'] = str(args.comroot.parent)
        elif args.expdir:
            os.environ['RUNTESTS'] = str(args.expdir)
    if args.stmp:
        os.environ['STMP'] = str(args.stmp)

    testconf = load_case_yaml(yaml_path)
    exp = testconf.experiment
    pslot = exp.pslot
    comroot = Path(exp.comroot)
    runtests = comroot.parent
    expdir_path = runtests / "EXPDIR" / pslot

    suite_name = args.suite_name or pslot

    ecf_host = os.environ["ECF_HOST"]
    ecf_port = os.environ["ECF_PORT"]

    print("=== Load ecFlow case ===")
    print(f"  Case YAML:   {yaml_path.relative_to(HOMEglobal)}")
    print(f"  Suite:       {suite_name}")
    print(f"  ECF_HOST:    {ecf_host}")
    print(f"  ECF_PORT:    {ecf_port}")
    print(f"  HOMEglobal:  {HOMEglobal}")
    print(f"  PSLOT:       {pslot}")
    print(f"  RUNTESTS:    {runtests}")
    print(f"  EXPDIR:      {expdir_path}")
    print(f"  COMROOT:     {comroot}")
    print()

    # Step 0: Clean up stale ecFlow runtime files
    print("[0/4] Cleaning up previous ecFlow runtime files...")
    cleaned = cleanup_stale_files(pslot, comroot, runtests)

    # Step 1: Create the experiment
    print("[1/4] Creating experiment via setup_expt...")
    overwrite = args.overwrite and cleaned
    expdir = create_experiment(testconf, runtests, overwrite=overwrite)
    print(f"  Experiment created in {expdir}.")

    # Step 2: Generate the ecFlow .def and ecf_scripts directory
    print("[2/4] Generating ecFlow .def via setup_workflow (ecflow engine)...")
    def_file = generate_ecflow_def(expdir)
    print(f"  Suite definition generated: {def_file.name}")
    ecf_scripts_dir = expdir / "ecf_scripts"
    if ecf_scripts_dir.is_dir():
        n_ecf = sum(1 for f in ecf_scripts_dir.glob("*.ecf"))
        print(f"  ECF_FILES directory: {ecf_scripts_dir} ({n_ecf} files)")

    # Restore ecFlow server vars — setup_workflow's config parsing
    # may alter the module environment, unsetting ECF_HOST/ECF_PORT.
    os.environ['ECF_HOST'] = ecf_host
    os.environ['ECF_PORT'] = ecf_port

    # Step 3: Load into ecFlow server
    print("[3/4] Loading suite into ecFlow server...")
    if not ecflow_client_quiet("--ping"):
        print(f"[ERROR] Cannot reach ecFlow server at {ecf_host}:{ecf_port}")
        sys.exit(1)
    print("  Server is alive.")
    load_suite(suite_name, def_file)
    print(f"  Suite {suite_name} loaded.")

    print()
    print("=== Done ===")
    print(f"Suite loaded but NOT started. To begin the run:")
    print(f"    ecflow_client --begin={suite_name}")
    print()
    print(f"To delete the suite:")
    print(f"    ecflow_client --delete=force yes /{suite_name}")
    print()
    print(f"Monitor with: ecflow_client --get_state /{suite_name}")
    print("         or:  ecflow_ui  (if X11 available)")
    print()
    print("To refresh .ecf files after editing (without regenerating .def):")
    print(f"  bash dev/workflow/ecflow/sync_ecf_scripts.sh {expdir_path}/ecf_scripts")


def main() -> None:
    """Generic CLI entry point.  Requires ``--yaml``."""
    run()


if __name__ == "__main__":
    main()
