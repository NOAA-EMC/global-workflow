# Submodule_Fixture trees

This directory is the committed **Submodule_Fixture** (Requirement 6.2, 6.7).
It provides minimal, byte-stable stand-ins for the git-submodule-owned source
trees that the deployment pipeline copies verbatim into an EXPDIR, so a
verification deploy can complete **without** emitting a
`Submodule source not found` FATAL ERROR — even when the real submodules
(`sorc/nexus.fd`, `sorc/upp.fd`) are not checked out/fetched.

## How it is used

`deployment/pipeline._stage_submodule_copy(..., policy=SubmodulePolicy.FIXTURE,
fixture_root=<this dir>)` resolves any missing `SUBMODULE_COPY_MANIFEST` source
by joining `fixture_root` with the manifest **source path** (the path relative
to the project root). Pass this directory as `fixture_root`:

```python
from pathlib import Path
from deployment.pipeline import run, SubmodulePolicy

FIXTURE_ROOT = Path("dev/workflow/tests/fixtures/submodules").resolve()

run(
    config=...,
    platform="HERA",
    expdir=...,
    version="v17.0.0",
    submodule_policy=SubmodulePolicy.FIXTURE,
    fixture_root=str(FIXTURE_ROOT),
)
```

## Layout — must mirror `SUBMODULE_COPY_MANIFEST`

The fixture tree mirrors the **source** side of each manifest entry exactly,
because the resolver looks up `fixture_root / <manifest source path>`.

`SUBMODULE_COPY_MANIFEST` (in `dev/workflow/deployment/pipeline.py`):

| Manifest source (relative to project root) | EXPDIR destination     |
| ------------------------------------------ | ---------------------- |
| `sorc/nexus.fd/config/gocart/`             | `parm/chem/nexus/gocart/` |
| `sorc/upp.fd/parm/`                        | `parm/post/`           |

This fixture therefore provides:

```
submodules/
└── sorc/
    ├── nexus.fd/
    │   └── config/
    │       └── gocart/
    │           ├── NEXUS_Config.rc
    │           └── HEMCO_sa_Config.rc
    └── upp.fd/
        └── parm/
            ├── params_grib2_tbl_new
            └── postxconfig-NT-GFS.txt
```

When a fixture-backed deploy runs, these resolve to:

- `<EXPDIR>/parm/chem/nexus/gocart/NEXUS_Config.rc`
- `<EXPDIR>/parm/chem/nexus/gocart/HEMCO_sa_Config.rc`
- `<EXPDIR>/parm/post/params_grib2_tbl_new`
- `<EXPDIR>/parm/post/postxconfig-NT-GFS.txt`

## Determinism & Token_Scan constraints

- **Byte-stable:** the file contents are fixed and checked into the repo, so two
  fixture-backed deploys at the same git commit produce identical manifest
  hashes (preserves Property 1, Deployment Determinism).
- **Token-free:** the fixture files contain **no** runtime atparse (`@[...]`)
  tokens and **no** Jinja2 tokens (`{{`, `{%`, `{#`). The submodule copy is
  verbatim (never rendered), and these files are scanned as part of the rendered
  EXPDIR, so they must stay token-free for the Token_Scan / No-Unresolved-Tokens
  property to pass.

## Reproducing a clean deploy

1. Ensure the venv interpreter is used: `dev/workflow/.venv/bin/python`.
2. From `dev/workflow`, deploy with the FIXTURE policy pointing at this tree
   (see the snippet above), or run the submodule/integration tests that already
   wire `policy=SubmodulePolicy.FIXTURE` and `fixture_root` at this directory.
3. The deploy completes with the four stand-in files copied into the EXPDIR and
   no `Submodule source not found` FATAL ERROR.

## Updating the fixture

If `SUBMODULE_COPY_MANIFEST` changes (a source path is added/renamed), add or
rename the matching subtree here so `fixture_root / <new source path>` exists.
Keep new files minimal and token-free to preserve determinism and the
Token_Scan guarantee.
