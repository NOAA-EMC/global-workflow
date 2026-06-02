"""Verify field_table.j2 renders correctly per the design document specification.

The template normalizes tracer naming and ordering across all suites as specified
in the design document section 2.1. The GFDL variants match legacy files exactly.
For wsm6/thompson, the template uses consistent naming (matching gfdl conventions)
rather than the inconsistent legacy file naming.
"""
import sys
from pathlib import Path

import jinja2

WORKSPACE = Path(__file__).resolve().parents[3]
TEMPLATE_DIR = WORKSPACE / "dev" / "parm" / "ufs" / "fv3"
LEGACY_DIR = WORKSPACE / "parm" / "ufs" / "fv3"


class ModelDict(dict):
    """Dict subclass that supports .get() method for Jinja2 template compatibility."""
    def get(self, key, default=None):
        return dict.get(self, key, default)


def render_template(physics_suite, pbl_scheme='satmedmf', progsigma=True):
    """Render field_table.j2 with given context."""
    env = jinja2.Environment(
        loader=jinja2.FileSystemLoader(str(TEMPLATE_DIR)),
        undefined=jinja2.StrictUndefined,
        keep_trailing_newline=True,
    )
    template = env.get_template("field_table.j2")

    model = ModelDict({
        'physics_suite': physics_suite,
        'pbl_scheme': pbl_scheme,
        'progsigma': progsigma,
    })

    return template.render(model=model)


def normalize(content):
    """Normalize content for comparison: strip trailing whitespace per line, strip trailing newlines."""
    lines = content.rstrip('\n').split('\n')
    return '\n'.join(line.rstrip() for line in lines)


def compare_with_legacy(suite, pbl_scheme, progsigma, legacy_filename):
    """Compare rendered output with legacy file."""
    rendered = render_template(suite, pbl_scheme, progsigma)
    legacy_path = LEGACY_DIR / legacy_filename

    if not legacy_path.exists():
        print(f"  SKIP: {legacy_filename} not found")
        return True

    legacy_content = legacy_path.read_text()

    rendered_norm = normalize(rendered)
    legacy_norm = normalize(legacy_content)

    if rendered_norm == legacy_norm:
        print(f"  PASS: {legacy_filename}")
        return True
    else:
        print(f"  FAIL: {legacy_filename}")
        # Show first few diffs
        rendered_lines = rendered_norm.split('\n')
        legacy_lines = legacy_norm.split('\n')
        max_lines = max(len(rendered_lines), len(legacy_lines))
        diff_count = 0
        for i in range(max_lines):
            r = rendered_lines[i] if i < len(rendered_lines) else "<missing>"
            l = legacy_lines[i] if i < len(legacy_lines) else "<missing>"
            if r != l:
                diff_count += 1
                if diff_count <= 5:
                    print(f"    Line {i+1}:")
                    print(f"      Expected: {repr(l)}")
                    print(f"      Got:      {repr(r)}")
        if diff_count > 5:
            print(f"    ... and {diff_count - 5} more differences")
        return False


def verify_tracer_presence(rendered, expected_tracers, label):
    """Verify that specific tracers are present in the rendered output."""
    missing = []
    for tracer in expected_tracers:
        if f'"atmos_mod", "{tracer}"' not in rendered:
            missing.append(tracer)
    if missing:
        print(f"  FAIL: {label} - missing tracers: {missing}")
        return False
    print(f"  PASS: {label}")
    return True


def verify_tracer_absence(rendered, absent_tracers, label):
    """Verify that specific tracers are NOT present in the rendered output."""
    present = []
    for tracer in absent_tracers:
        if f'"atmos_mod", "{tracer}"' in rendered:
            present.append(tracer)
    if present:
        print(f"  FAIL: {label} - unexpected tracers: {present}")
        return False
    print(f"  PASS: {label}")
    return True


def main():
    print("Verifying field_table.j2 template rendering...\n")

    results = []

    # === GFDL variants match legacy files exactly ===
    print("--- GFDL variants (exact match with legacy) ---")
    gfdl_cases = [
        ("gfdl", "default", False, "field_table_gfdl"),
        ("gfdl", "satmedmf", False, "field_table_gfdl_satmedmf"),
        ("gfdl", "default", True, "field_table_gfdl_progsigma"),
        ("gfdl", "satmedmf", True, "field_table_gfdl_satmedmf_progsigma"),
    ]
    for suite, pbl, progsigma, legacy_file in gfdl_cases:
        result = compare_with_legacy(suite, pbl, progsigma, legacy_file)
        results.append((legacy_file, result))

    # === Design doc specification tests ===
    print("\n--- Design doc specification tests ---")

    # GFDL: base + suite tracers + ozone + cld_amt
    rendered = render_template('gfdl', 'default', False)
    results.append(("gfdl_base_tracers", verify_tracer_presence(
        rendered, ['sphum', 'liq_wat', 'rainwat', 'ice_wat', 'snowwat', 'graupel', 'o3mr', 'cld_amt'],
        "gfdl base tracers")))
    results.append(("gfdl_no_tke", verify_tracer_absence(
        rendered, ['sgs_tke', 'sigmab'], "gfdl no TKE/progsigma")))

    # GFDL with satmedmf: adds sgs_tke
    rendered = render_template('gfdl', 'satmedmf', False)
    results.append(("gfdl_satmedmf_tke", verify_tracer_presence(
        rendered, ['sgs_tke'], "gfdl satmedmf has TKE")))

    # GFDL with progsigma: adds sigmab
    rendered = render_template('gfdl', 'default', True)
    results.append(("gfdl_progsigma", verify_tracer_presence(
        rendered, ['sigmab'], "gfdl progsigma has sigmab")))

    # Thompson: base + suite tracers + ice_nc + rain_nc + ozone
    rendered = render_template('thompson', 'default', False)
    results.append(("thompson_tracers", verify_tracer_presence(
        rendered, ['sphum', 'liq_wat', 'rainwat', 'ice_wat', 'snowwat', 'graupel', 'ice_nc', 'rain_nc', 'o3mr'],
        "thompson tracers")))
    results.append(("thompson_no_cld_amt", verify_tracer_absence(
        rendered, ['cld_amt'], "thompson no cld_amt")))

    # WSM6: base + suite tracers + ozone (no ice_nc, rain_nc, no cld_amt)
    rendered = render_template('wsm6', 'default', False)
    results.append(("wsm6_tracers", verify_tracer_presence(
        rendered, ['sphum', 'liq_wat', 'rainwat', 'ice_wat', 'snowwat', 'graupel', 'o3mr'],
        "wsm6 tracers")))
    results.append(("wsm6_no_extras", verify_tracer_absence(
        rendered, ['ice_nc', 'rain_nc', 'cld_amt'], "wsm6 no extras")))

    # Zhaocarr: base only + ozone (no suite-specific tracers)
    rendered = render_template('zhaocarr', 'default', False)
    results.append(("zhaocarr_tracers", verify_tracer_presence(
        rendered, ['sphum', 'liq_wat', 'o3mr'],
        "zhaocarr tracers")))
    results.append(("zhaocarr_no_extras", verify_tracer_absence(
        rendered, ['rainwat', 'ice_wat', 'snowwat', 'graupel', 'ice_nc', 'rain_nc', 'cld_amt'],
        "zhaocarr no extras")))

    # Zhaocarr sphum surface_value check
    rendered = render_template('zhaocarr', 'default', False)
    if 'surface_value=3.e-6' in rendered:
        print("  PASS: zhaocarr sphum surface_value=3.e-6")
        results.append(("zhaocarr_sphum_sv", True))
    else:
        print("  FAIL: zhaocarr sphum surface_value should be 3.e-6")
        results.append(("zhaocarr_sphum_sv", False))

    # GFDL sphum surface_value check - find the profile_type line after sphum
    rendered = render_template('gfdl', 'default', False)
    lines = rendered.split('\n')
    sphum_idx = next(i for i, l in enumerate(lines) if 'sphum' in l and 'TRACER' in l)
    # The profile_type line is a few lines after sphum
    sphum_block = '\n'.join(lines[sphum_idx:sphum_idx+4])
    if 'surface_value=1.e30' in sphum_block:
        print("  PASS: gfdl sphum surface_value=1.e30")
        results.append(("gfdl_sphum_sv", True))
    else:
        print("  FAIL: gfdl sphum surface_value should be 1.e30")
        results.append(("gfdl_sphum_sv", False))

    print(f"\n{'='*60}")
    passed = sum(1 for _, r in results if r)
    total = len(results)
    print(f"Results: {passed}/{total} passed")

    if passed < total:
        sys.exit(1)


if __name__ == "__main__":
    main()
