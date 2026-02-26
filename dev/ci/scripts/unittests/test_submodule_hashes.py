"""
Unit tests to verify common submodule hashes across repository components.

Validates that shared components pinned as submodules in multiple places
are at the same commit, preventing build inconsistencies.

Checks:
    - CCPP-physics hash in sorc/ufs_utils.fd matches the one in
      sorc/ufs_model.fd (via UFSATM)
    - GDASApp build.sh uses MOM6 from sorc/ufs_model.fd, ensuring
      GDASApp and ufs-weather-model use the same MOM6 source
"""

import re
import subprocess
from pathlib import Path

import pytest


def get_repo_root() -> Path:
    """Find repository root by looking for .github directory."""
    for parent in [Path(__file__).resolve()] + list(Path(__file__).resolve().parents):
        if (parent / '.github').exists():
            return parent
    raise FileNotFoundError("Could not find repository root (.github directory)")


def get_submodule_hash(repo_path: Path, submodule_path: str) -> str:
    """
    Get the commit hash recorded for a submodule in a git repository.

    Parameters
    ----------
    repo_path : Path
        Path to the git repository containing the submodule reference.
    submodule_path : str
        Relative path to the submodule within the repository.

    Returns
    -------
    str
        The full commit SHA of the submodule.

    Raises
    ------
    RuntimeError
        If git ls-tree fails or returns unexpected output.
    """
    result = subprocess.run(
        ['git', '-C', str(repo_path), 'ls-tree', 'HEAD', submodule_path],
        capture_output=True, text=True, timeout=30
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"git ls-tree failed for {submodule_path} in {repo_path}:\n"
            f"  stdout: {result.stdout!r}\n"
            f"  stderr: {result.stderr!r}"
        )
    # Output format: "<mode> commit <hash>\t<path>"
    parts = result.stdout.strip().split()
    if len(parts) < 3 or parts[1] != 'commit':
        raise RuntimeError(
            f"Unexpected git ls-tree output for {submodule_path}: {result.stdout!r}"
        )
    return parts[2]


def is_submodule_initialized(path: Path) -> bool:
    """
    Check if a git submodule directory is initialized and checked out.

    Parameters
    ----------
    path : Path
        Path to the submodule directory.

    Returns
    -------
    bool
        True if the directory exists and contains a git repository.
    """
    if not path.is_dir():
        return False
    result = subprocess.run(
        ['git', '-C', str(path), 'rev-parse', '--git-dir'],
        capture_output=True, text=True, timeout=30
    )
    return result.returncode == 0


def test_ccpp_physics_hashes_match():
    """
    Verify CCPP-physics submodule hash is identical in UFS_Utils and ufs-weather-model.

    UFS_Utils pins ccpp-physics at sorc/ufs_utils.fd/ccpp-physics.
    ufs-weather-model pins it at sorc/ufs_model.fd/UFSATM/ccpp/physics.
    Both must point to the same commit to ensure consistent physics.
    """
    repo_root = get_repo_root()

    ufs_utils_path = repo_root / 'sorc' / 'ufs_utils.fd'
    ufs_model_path = repo_root / 'sorc' / 'ufs_model.fd'
    ufsatm_path = ufs_model_path / 'UFSATM'

    if not is_submodule_initialized(ufs_utils_path):
        pytest.skip("sorc/ufs_utils.fd submodule is not initialized")

    if not is_submodule_initialized(ufs_model_path):
        pytest.skip("sorc/ufs_model.fd submodule is not initialized")

    if not is_submodule_initialized(ufsatm_path):
        pytest.skip("sorc/ufs_model.fd/UFSATM submodule is not initialized")

    ccpp_hash_ufs_utils = get_submodule_hash(ufs_utils_path, 'ccpp-physics')
    ccpp_hash_ufsatm = get_submodule_hash(ufsatm_path, 'ccpp/physics')

    assert ccpp_hash_ufs_utils == ccpp_hash_ufsatm, (
        f"CCPP-physics hash mismatch:\n"
        f"  sorc/ufs_utils.fd/ccpp-physics:          {ccpp_hash_ufs_utils}\n"
        f"  sorc/ufs_model.fd/UFSATM/ccpp/physics:  {ccpp_hash_ufsatm}\n"
        f"Update one of the submodules so both point to the same CCPP-physics commit."
    )


def test_gdas_mom6_uses_ufs_model_mom6():
    """
    Verify GDASApp uses MOM6 from ufs-weather-model (sorc/ufs_model.fd).

    GDASApp's build.sh symlinks MOM6 from sorc/ufs_model.fd/MOM6-interface/MOM6
    into the SOCA build tree. This test confirms:
      1. The build.sh symlink target matches the MOM6 submodule path registered
         in ufs-weather-model.
      2. The MOM6-interface/MOM6 entry exists as a submodule in ufs-weather-model.
    """
    repo_root = get_repo_root()

    gdas_path = repo_root / 'sorc' / 'gdas.cd'
    ufs_model_path = repo_root / 'sorc' / 'ufs_model.fd'

    if not is_submodule_initialized(gdas_path):
        pytest.skip("sorc/gdas.cd submodule is not initialized")

    if not is_submodule_initialized(ufs_model_path):
        pytest.skip("sorc/ufs_model.fd submodule is not initialized")

    build_sh = gdas_path / 'build.sh'
    assert build_sh.exists(), f"GDASApp build.sh not found at {build_sh}"

    content = build_sh.read_text()

    # Verify build.sh symlinks SOCA's MOM6 from ufs_model.fd.
    # The expected pattern matches the symlink command in build.sh:
    #   ln -sf $HOMEgfs/sorc/ufs_model.fd/MOM6-interface/MOM6/ ...
    mom6_symlink_pattern = r'ufs_model\.fd/MOM6-interface/MOM6'
    assert re.search(mom6_symlink_pattern, content), (
        "GDASApp build.sh must symlink MOM6 from sorc/ufs_model.fd/MOM6-interface/MOM6 "
        "to ensure GDASApp and ufs-weather-model use the same MOM6 source."
    )

    # Verify MOM6-interface/MOM6 is a registered submodule in ufs-weather-model
    try:
        mom6_hash = get_submodule_hash(ufs_model_path, 'MOM6-interface/MOM6')
    except RuntimeError as e:
        pytest.fail(
            f"MOM6-interface/MOM6 is not a valid submodule in sorc/ufs_model.fd: {e}"
        )

    assert mom6_hash, "MOM6-interface/MOM6 submodule hash must not be empty"
