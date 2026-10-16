#!/usr/bin/env python3

"""Generic ecFlow case loader.

Parses a CI case YAML, creates the experiment via setup_expt, generates
the ecFlow .def via setup_workflow (ecflow engine), loads the definition
into the ecFlow server, and optionally begins the suite.

The generated .def contains all edit variables (paths, resources,
partitions) baked in from the experiment's config files, so no
``--alter`` overrides are needed after loading.

Test-specific entry points (e.g. ``run_ecflow_case.py``) provide
default YAML paths and delegate to this module's ``run()`` function.

Prerequisites
-------------
1. ecFlow server running.
2. global-workflow built and linked.
3. Nothing needs to be exported in the login shell.  The server is found
   automatically: the ``<host>.<port>.ecf.log`` files that ecflow_start.sh
   leaves in the server directory (``--ecf-home``, default: ``~/ecflow``)
   name the host and port, and the newest one that answers a
   ping is used.  ``--ecf-host`` / ``--ecf-port`` (or ``$ECF_HOST`` /
   ``$ECF_PORT``) override the search.  The ecflow module is loaded and
   ``ECF_HOSTFILE`` is ignored for ecflow_client calls only::

       python3 dev/workflow/ecflow/load_ecflow_case.py \\
           --yaml dev/ci/cases/pr/<CASE>.yaml  (or dev/ci/cases/ecflow_pr/<CASE>.yaml)
"""

import argparse
import os
import re
import shlex
import shutil
import subprocess
import sys
import time
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

DEFAULT_ECF_HOST = "uecflow01"

# Load the ecflow module only for the ecflow_client child process so that
# the caller's shell and this process keep their own module environment.
_MODULE_WRAPPER = (
    'type module &> /dev/null || source "${MODULESHOME}/init/bash"; '
    'module load ecflow &> /dev/null; exec ecflow_client "$@"'
)


class EcflowServer:
    """ecFlow server coordinates used to build ecflow_client commands.

    The host and port are passed to ecflow_client with ``--host`` and
    ``--port`` and ``ECF_HOSTFILE`` is removed from the child environment,
    so nothing has to be exported in the user's shell.

    Parameters
    ----------
    host : str
        ecFlow server host name.
    port : int or str
        ecFlow server port.
    """

    def __init__(self, host: str, port) -> None:
        self.host = host
        self.port = str(port)

    @property
    def connection_args(self) -> list:
        """Return the ``--host`` and ``--port`` arguments."""
        return [f"--host={self.host}", f"--port={self.port}"]

    def command(self, *args: str) -> list:
        """Return the full ecflow_client command line for ``args``."""
        if shutil.which("ecflow_client"):
            base = ["ecflow_client"]
        else:
            base = ["bash", "-c", _MODULE_WRAPPER, "ecflow_client"]
        return [*base, *args, *self.connection_args]

    @staticmethod
    def environment() -> dict:
        """Return the child environment without ``ECF_HOSTFILE``."""
        return {k: v for k, v in os.environ.items() if k != "ECF_HOSTFILE"}

    def is_alive(self) -> bool:
        """Return True if the server answers a ping."""
        try:
            result = subprocess.run(self.command("--ping"), capture_output=True,
                                    timeout=30, env=self.environment())
            return result.returncode == 0
        except subprocess.TimeoutExpired:
            return False


# ecflow_start.sh names the server log <host>.<port>.ecf.log
SERVER_LOG_PATTERN = re.compile(r"^(?P<host>.+)\.(?P<port>\d+)\.ecf\.log$")


def find_server_candidates(ecf_home: Path, host: str = None) -> list:
    """List the servers that have left a log in ``ecf_home``, newest first.

    Parameters
    ----------
    ecf_home : Path
        Server directory passed to ``ecflow_start.sh -d``.
    host : str, optional
        Only return servers running on this host.
    """
    logs = []
    for log in ecf_home.glob("*.ecf.log"):
        match = SERVER_LOG_PATTERN.match(log.name)
        if match and host in (None, match["host"]):
            logs.append((log.stat().st_mtime, match["host"], match["port"]))
    return [EcflowServer(h, p) for _, h, p in sorted(logs, reverse=True)]


def select_server(host: str, port: str, ecf_home: Path) -> EcflowServer:
    """Return the ecFlow server to use.

    An explicit port wins.  Otherwise the newest server found in
    ``ecf_home`` that answers a ping is used.  When none answers, the
    newest one is returned so the caller can report it, and when there
    is no server log at all the port falls back to ``UID + 1500``.

    Parameters
    ----------
    host : str or None
        Requested host, or None to take it from the server logs.
    port : str or None
        Requested port, or None to search ``ecf_home``.
    ecf_home : Path
        Server directory searched for ``<host>.<port>.ecf.log`` files.
    """
    if port:
        return EcflowServer(host or DEFAULT_ECF_HOST, port)
    candidates = find_server_candidates(ecf_home, host)
    for candidate in candidates:
        if candidate.is_alive():
            return candidate
    if candidates:
        return candidates[0]
    return EcflowServer(host or DEFAULT_ECF_HOST, os.getuid() + 1500)


def start_server_command(server: EcflowServer, ecf_home: Path) -> list:
    """Return the ssh command that starts ``server`` on its own node.

    ``ecflow_start.sh`` only runs on the server node, so it is started
    over ssh.  Its output goes to a file because the server it forks would
    otherwise keep the ssh pipe open.

    Parameters
    ----------
    server : EcflowServer
        Server to start; its host is the node ssh connects to.
    ecf_home : Path
        Server directory passed to ``ecflow_start.sh -d``.
    """
    out = shlex.quote(f"{ecf_home}/start_server.out")
    script = (
        f"module load ecflow; export ECF_HOST={server.host} ECF_PORT={server.port}; "
        f"mkdir -p {shlex.quote(str(ecf_home))}; "
        f"ecflow_start.sh -p {server.port} -d {shlex.quote(str(ecf_home))} "
        f"> {out} 2>&1 < /dev/null; cat {out}"
    )
    return ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
            server.host, f"bash -lc {shlex.quote(script)}"]


def start_server(server: EcflowServer, ecf_home: Path, wait: int = 30) -> bool:
    """Start the ecFlow server over ssh and wait until it answers a ping.

    Parameters
    ----------
    server : EcflowServer
        Server to start.
    ecf_home : Path
        Server directory passed to ``ecflow_start.sh -d``.
    wait : int, optional
        Seconds to wait for the server to answer.

    Returns
    -------
    bool
        True if the server answers a ping.
    """
    print(f"  Starting ecFlow server {server.host}:{server.port} "
          f"in {ecf_home} via ssh...")
    try:
        result = subprocess.run(start_server_command(server, ecf_home),
                                capture_output=True, text=True, timeout=120)
    except subprocess.TimeoutExpired:
        print("[ERROR] Timed out starting the ecFlow server over ssh.")
        return False
    if result.returncode != 0:
        print(f"[ERROR] Could not start the server: {result.stderr.strip() or result.stdout.strip()}")
        return False
    deadline = time.monotonic() + wait
    while time.monotonic() < deadline:
        if server.is_alive():
            return True
        time.sleep(2)
    return server.is_alive()


_server = EcflowServer(DEFAULT_ECF_HOST, os.getuid() + 1500)


def ecflow_client(*args: str) -> subprocess.CompletedProcess:
    """Run ecflow_client with the given arguments."""
    cmd = _server.command(*args)
    try:
        return subprocess.run(cmd, check=True, capture_output=True, text=True,
                              timeout=30, env=_server.environment())
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] ecflow_client failed: {' '.join(args)}")
        if e.stdout:
            print(f"  stdout: {e.stdout.strip()}")
        if e.stderr:
            print(f"  stderr: {e.stderr.strip()}")
        raise


def ecflow_client_quiet(*args: str) -> bool:
    """Run ecflow_client, returning True on success and False on failure."""
    cmd = _server.command(*args)
    try:
        result = subprocess.run(cmd, capture_output=True, text=True,
                                timeout=30, env=_server.environment())
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
    parser.add_argument(
        "--ecf-host",
        default=os.environ.get("ECF_HOST"),
        help="ecFlow server host (default: $ECF_HOST, else found from the "
             "server logs in --ecf-home, else %s)." % DEFAULT_ECF_HOST,
    )
    parser.add_argument(
        "--ecf-port",
        default=os.environ.get("ECF_PORT"),
        help="ecFlow server port (default: $ECF_PORT, else found from the "
             "server logs in --ecf-home, else UID + 1500).",
    )
    parser.add_argument(
        "--start-server",
        action="store_true",
        help="If the ecFlow server does not answer, start it over ssh on its "
             "node with ecflow_start.sh (off by default).",
    )
    parser.add_argument(
        "--ecf-home",
        type=Path,
        default=Path(os.environ.get("ECF_HOME", Path.home() / "ecflow")),
        help="Directory the ecFlow server was started in "
             "(ecflow_start.sh -d); its <host>.<port>.ecf.log files give "
             "the server port (default: $ECF_HOME or %(default)s).",
    )
    return parser.parse_args()


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


def check_host_supported(testconf: AttrDict) -> None:
    """Exit with an error if the current host is in ``skip_ci_on_hosts``.

    Parameters
    ----------
    testconf : AttrDict
        Parsed case YAML.
    """
    skip_hosts = [str(h).lower() for h in testconf.get('skip_ci_on_hosts', None) or []]
    machine = str(Host().machine).lower()
    if machine in skip_hosts:
        print(f"[ERROR] This case is not supported on {machine.upper()} "
              "(listed in skip_ci_on_hosts).")
        sys.exit(1)


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
    skip_keys = {"net", "mode"}
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


def _client_hint() -> str:
    """Return an ecflow_client command prefix users can paste into a shell."""
    return f"ecflow_client --host={_server.host} --port={_server.port}"


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
        time.sleep(5)
        print(f"  Deleting /{suite_name}...")
        cmd = _server.command("--delete=force", "yes", f"/{suite_name}")
        try:
            subprocess.run(cmd, check=True, capture_output=True,
                           text=True, timeout=60, env=_server.environment())
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            msg = getattr(e, 'stderr', '') or ''
            print(f"[WARN] Delete failed: {msg.strip() if msg else 'timeout'}")
            print(f"  Try manually: {_client_hint()} --delete=force yes /{suite_name}")
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

    selected = select_server(args.ecf_host, args.ecf_port, args.ecf_home)
    _server.host = selected.host
    _server.port = selected.port

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
    check_host_supported(testconf)
    exp = testconf.experiment
    pslot = exp.pslot
    comroot = Path(exp.comroot)
    runtests = comroot.parent
    expdir_path = runtests / "EXPDIR" / pslot

    suite_name = args.suite_name or pslot

    print("=== Load ecFlow case ===")
    print(f"  Case YAML:   {yaml_path.relative_to(HOMEglobal)}")
    print(f"  Suite:       {suite_name}")
    print(f"  ecFlow server: {_server.host}:{_server.port}")
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

    # Step 3: Load into ecFlow server
    print("[3/4] Loading suite into ecFlow server...")
    if args.start_server and not _server.is_alive():
        start_server(_server, args.ecf_home)
    if not _server.is_alive():
        print(f"[ERROR] Cannot reach ecFlow server at {_server.host}:{_server.port}")
        print(f"  Searched {args.ecf_home} for <host>.<port>.ecf.log files.")
        print("  Start the server, pass --ecf-host/--ecf-port/--ecf-home, "
              "or use --start-server.")
        sys.exit(1)
    print("  Server is alive.")
    load_suite(suite_name, def_file)
    print(f"  Suite {suite_name} loaded.")

    client = _client_hint()
    print()
    print("=== Done ===")
    print("Suite loaded but NOT started. To begin the run:")
    print(f"    {client} --begin={suite_name}")
    print()
    print("To delete the suite:")
    print(f"    {client} --delete=force yes /{suite_name}")
    print()
    print(f"Monitor with: {client} --get_state /{suite_name}")
    print("         or:  ecflow_ui  (if X11 available)")
    print("(load the module first with: module load ecflow)")
    print()
    print("To refresh .ecf files after editing (without regenerating .def):")
    print(f"  bash dev/workflow/ecflow/sync_ecf_scripts.sh {expdir_path}/ecf_scripts")


def main() -> None:
    """Generic CLI entry point.  Requires ``--yaml``."""
    run()


if __name__ == "__main__":
    main()
