import os
import sys

import pytest
import yaml

from wxflow import find_upward

HOMEglobal = find_upward('.github')
sys.path.insert(0, os.path.join(HOMEglobal, 'dev', 'workflow'))

from fix_overlay import build_fix_overlay, FixOverlayError, MANIFEST_NAME  # noqa: E402


def _touch(path):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(path.name)


@pytest.fixture
def base(tmp_path):
    """A small stand-in for ${HOMEglobal}/fix with a few components."""
    root = tmp_path / 'fix'
    for rel in ['orog/C48/C48_oro_data.tile1.nc',
                'orog/C48/C48_grid.tile1.nc',
                'orog/C96/C96_oro_data.tile1.nc',
                'mom6/025/ocean_hgrid.nc',
                'mom6/025/MOM_input',
                'cpl/aC48o500/grid_spec.nc',
                'am/global_o3prdlos.f77']:
        _touch(root / rel)
    return root


@pytest.fixture
def user(tmp_path):
    """User-supplied replacement files."""
    root = tmp_path / 'user'
    for rel in ['MOM/regional.mom6.nc',
                'MOM/ocean_hgrid.nc',
                'MOM/notes.txt',
                'OROG/C48/C48_oro_data.tile1.nc',
                'OROG/C48/C48_new.nc',
                'CPL/aC48o008/grid_spec.nc',
                'wave/mod_def.glo_15m']:
        _touch(root / rel)
    return root


def _target(path):
    assert path.is_symlink(), f'{path} is not a symlink'
    return os.readlink(path)


def _manifest(dest):
    with open(dest / MANIFEST_NAME) as fh:
        return yaml.safe_load(fh)


# --- happy paths ------------------------------------------------------------

def test_no_entries_is_all_links_to_base(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    result = build_fix_overlay(str(base), {}, str(dest))

    assert result == str(dest)
    assert (dest / MANIFEST_NAME).is_file()
    for comp in ['orog', 'mom6', 'cpl', 'am']:
        assert _target(dest / comp) == str(base / comp)


def test_replace_file_expands_only_its_path(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'MOM' / 'ocean_hgrid.nc'
    build_fix_overlay(str(base), {'replace': {'mom6/025/ocean_hgrid.nc': str(src)}}, str(dest))

    # touched path is real directories down to the leaf
    assert (dest / 'mom6').is_dir() and not (dest / 'mom6').is_symlink()
    assert (dest / 'mom6' / '025').is_dir() and not (dest / 'mom6' / '025').is_symlink()
    assert _target(dest / 'mom6' / '025' / 'ocean_hgrid.nc') == str(src)
    assert (dest / 'mom6' / '025' / 'ocean_hgrid.nc').read_text() == 'ocean_hgrid.nc'
    # siblings at each expanded level link back to base
    assert _target(dest / 'mom6' / '025' / 'MOM_input') == str(base / 'mom6' / '025' / 'MOM_input')
    # untouched components stay a single link
    assert _target(dest / 'orog') == str(base / 'orog')
    assert _target(dest / 'cpl') == str(base / 'cpl')


def test_replace_whole_directory(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'OROG' / 'C48'
    build_fix_overlay(str(base), {'replace': {'orog/C48': str(src)}}, str(dest))

    assert _target(dest / 'orog' / 'C48') == str(src)
    assert _target(dest / 'orog' / 'C96') == str(base / 'orog' / 'C96')


def test_add_file_in_new_directory(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'MOM' / 'regional.mom6.nc'
    build_fix_overlay(str(base), {'add': {'mom6/008/regional.mom6.nc': str(src)}}, str(dest))

    assert (dest / 'mom6' / '008').is_dir()
    assert _target(dest / 'mom6' / '008' / 'regional.mom6.nc') == str(src)
    assert _target(dest / 'mom6' / '025') == str(base / 'mom6' / '025')


def test_add_directory(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'CPL' / 'aC48o008'
    build_fix_overlay(str(base), {'add': {'cpl/aC48o008': str(src)}}, str(dest))

    assert _target(dest / 'cpl' / 'aC48o008') == str(src)
    assert _target(dest / 'cpl' / 'aC48o500') == str(base / 'cpl' / 'aC48o500')
    assert (dest / 'cpl' / 'aC48o008' / 'grid_spec.nc').is_file()


def test_add_new_component(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'wave'
    build_fix_overlay(str(base), {'add': {'wave': str(src)}}, str(dest))

    assert _target(dest / 'wave') == str(src)
    assert _target(dest / 'orog') == str(base / 'orog')


def test_merge_directory(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'merge': {'orog/C48': str(user / 'OROG' / 'C48')}}, str(dest))

    c48 = dest / 'orog' / 'C48'
    assert c48.is_dir() and not c48.is_symlink()
    # existing file overridden, new file added, untouched file still from base
    assert _target(c48 / 'C48_oro_data.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc')
    assert _target(c48 / 'C48_new.nc') == str(user / 'OROG' / 'C48' / 'C48_new.nc')
    assert _target(c48 / 'C48_grid.tile1.nc') == str(base / 'orog' / 'C48' / 'C48_grid.tile1.nc')
    assert _target(dest / 'orog' / 'C96') == str(base / 'orog' / 'C96')


def test_merge_glob_filters(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'merge': {'mom6/025': str(user / 'MOM' / '*.nc')}}, str(dest))

    d = dest / 'mom6' / '025'
    assert _target(d / 'regional.mom6.nc') == str(user / 'MOM' / 'regional.mom6.nc')
    assert _target(d / 'ocean_hgrid.nc') == str(user / 'MOM' / 'ocean_hgrid.nc')
    assert not (d / 'notes.txt').exists()
    assert _target(d / 'MOM_input') == str(base / 'mom6' / '025' / 'MOM_input')


def test_replace_wins_over_merge_regardless_of_order(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    special = user / 'special' / 'ocean_hgrid.nc'
    _touch(special)
    # replace written before merge in the yaml; it must still win
    fix_files = {'replace': {'mom6/025/ocean_hgrid.nc': str(special)},
                 'merge': {'mom6/025': str(user / 'MOM' / '*.nc')}}
    build_fix_overlay(str(base), fix_files, str(dest))

    d = dest / 'mom6' / '025'
    assert _target(d / 'ocean_hgrid.nc') == str(special)
    assert _target(d / 'regional.mom6.nc') == str(user / 'MOM' / 'regional.mom6.nc')

    manifest = _manifest(dest)
    assert manifest['links']['mom6/025/ocean_hgrid.nc'] == str(special)
    assert manifest['overrides'] == [{
        'dst': 'mom6/025/ocean_hgrid.nc',
        'from': 'merge: mom6/025',
        'by': 'replace: mom6/025/ocean_hgrid.nc',
        'was': str(user / 'MOM' / 'ocean_hgrid.nc'),
        'now': str(special),
    }]


def test_add_wins_over_merge(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    special = user / 'special' / 'regional.mom6.nc'
    _touch(special)
    fix_files = {'merge': {'mom6/025': str(user / 'MOM' / '*.nc')},
                 'add': {'mom6/025/regional.mom6.nc': str(special)}}
    build_fix_overlay(str(base), fix_files, str(dest))

    assert _target(dest / 'mom6' / '025' / 'regional.mom6.nc') == str(special)
    assert [o['by'] for o in _manifest(dest)['overrides']] == ['add: mom6/025/regional.mom6.nc']


def test_manifest_records_entries_links_and_no_overrides(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {'merge': {'orog/C48': str(user / 'OROG' / 'C48')},
                 'add': {'wave': str(user / 'wave')}}
    build_fix_overlay(str(base), fix_files, str(dest))

    manifest = _manifest(dest)
    assert manifest['base'] == str(base)
    assert manifest['entries'] == fix_files
    assert manifest['links'] == {
        'orog/C48/C48_new.nc': str(user / 'OROG' / 'C48' / 'C48_new.nc'),
        'orog/C48/C48_oro_data.tile1.nc': str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc'),
        'wave': str(user / 'wave'),
    }
    assert manifest['overrides'] == []


def test_rebuild_replaces_previous_overlay(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'add': {'wave': str(user / 'wave')}}, str(dest))
    build_fix_overlay(str(base), {'add': {'cpl/aC48o008': str(user / 'CPL' / 'aC48o008')}}, str(dest))

    assert not (dest / 'wave').exists()
    assert _target(dest / 'cpl' / 'aC48o008') == str(user / 'CPL' / 'aC48o008')


def test_empty_subsections_are_allowed(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'replace': None, 'add': {}, 'merge': None}, str(dest))
    assert _target(dest / 'orog') == str(base / 'orog')


# --- mode/base disagreement -------------------------------------------------

def test_replace_of_missing_path_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {'replace': {'mom6/025/ocean_hgird.nc': str(user / 'MOM' / 'ocean_hgrid.nc')}}  # typo
    with pytest.raises(FixOverlayError, match="not in the fix tree; use 'add'"):
        build_fix_overlay(str(base), fix_files, str(dest))
    assert not dest.exists()


def test_replace_kind_mismatch_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    with pytest.raises(FixOverlayError, match='is a directory in the fix tree but the source is a file'):
        build_fix_overlay(str(base), {'replace': {'orog/C48': str(user / 'wave' / 'mod_def.glo_15m')}}, str(dest))
    with pytest.raises(FixOverlayError, match='is a file in the fix tree but the source is a directory'):
        build_fix_overlay(str(base), {'replace': {'mom6/025/MOM_input': str(user / 'MOM')}}, str(dest))


def test_add_of_existing_path_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {'add': {'mom6/025/ocean_hgrid.nc': str(user / 'MOM' / 'ocean_hgrid.nc')}}
    with pytest.raises(FixOverlayError, match="already exists in the fix tree; use 'replace'"):
        build_fix_overlay(str(base), fix_files, str(dest))
    assert not dest.exists()


def test_merge_into_missing_directory_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    with pytest.raises(FixOverlayError, match="not a directory in the fix tree; use 'add'"):
        build_fix_overlay(str(base), {'merge': {'mom6/008': str(user / 'MOM')}}, str(dest))
    with pytest.raises(FixOverlayError, match="not a directory in the fix tree"):
        build_fix_overlay(str(base), {'merge': {'mom6/025/MOM_input': str(user / 'MOM')}}, str(dest))


def test_merge_source_must_be_directory_or_glob(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    with pytest.raises(FixOverlayError, match='must be a directory or a glob'):
        build_fix_overlay(str(base), {'merge': {'mom6/025': str(user / 'MOM' / 'ocean_hgrid.nc')}}, str(dest))


def test_glob_in_replace_or_add_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    with pytest.raises(FixOverlayError, match="may not be a glob; use 'merge'"):
        build_fix_overlay(str(base), {'replace': {'orog/C48': str(user / 'OROG' / 'C48' / '*')}}, str(dest))
    with pytest.raises(FixOverlayError, match="may not be a glob; use 'merge'"):
        build_fix_overlay(str(base), {'add': {'mom6/008': str(user / 'MOM' / '*.nc')}}, str(dest))


# --- malformed input --------------------------------------------------------

@pytest.mark.parametrize('fix_files, match', [
    ({'add': {'mom6/008/x.nc': '{user}/does/not/exist.nc'}}, 'does not exist'),
    ({'merge': {'orog/C48': '{user}/OROG/C48/*.grb'}}, 'matched nothing'),
    ({'merge': {'orog/C48': '{user}/OROG/C48'}}, None),  # control: valid
    ({'replace': {'orog/C48': 'OROG/C48'}}, 'absolute'),
    ({'replace': {'/orog/C48': '{user}/OROG/C48'}}, 'relative'),
    ({'add': {'../escape': '{user}/wave'}}, r"'\.\.'"),
    ({'replace': {'.': '{user}/wave'}}, 'fix root'),
    ({'add': {'': '{user}/wave'}}, 'non-empty'),
    ({'add': {'wave': None}}, 'non-empty'),
    ({'link': {'wave': '{user}/wave'}}, 'unknown sub-section'),
    ({'add': ['wave']}, 'must be a mapping'),
])
def test_invalid_entries(base, user, tmp_path, fix_files, match):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {mode: ({k: (v.format(user=user) if isinstance(v, str) else v) for k, v in entries.items()}
                        if isinstance(entries, dict) else entries)
                 for mode, entries in fix_files.items()}
    if match is None:
        build_fix_overlay(str(base), fix_files, str(dest))
        return
    with pytest.raises(FixOverlayError, match=match):
        build_fix_overlay(str(base), fix_files, str(dest))
    assert not dest.exists()


def test_section_must_be_a_mapping(base, tmp_path):
    with pytest.raises(FixOverlayError, match='must be a mapping'):
        build_fix_overlay(str(base), ['orog'], str(tmp_path / 'exp' / 'fix'))


def test_refuses_to_clobber_foreign_directory(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    _touch(dest / 'precious')
    with pytest.raises(FixOverlayError, match='not a fix overlay'):
        build_fix_overlay(str(base), {}, str(dest))
    assert (dest / 'precious').is_file()


def test_entry_inside_replaced_directory_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {'replace': {'orog/C48': str(user / 'OROG' / 'C48')},
                 'add': {'orog/C48/extra.nc': str(user / 'OROG' / 'C48' / 'C48_new.nc')}}
    with pytest.raises(FixOverlayError, match='linked wholesale'):
        build_fix_overlay(str(base), fix_files, str(dest))


def test_entry_under_a_base_file_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    fix_files = {'add': {'am/global_o3prdlos.f77/inner': str(user / 'wave')}}
    with pytest.raises(FixOverlayError, match='not a directory'):
        build_fix_overlay(str(base), fix_files, str(dest))


def test_dest_inside_base_is_an_error(base, tmp_path):
    with pytest.raises(FixOverlayError, match='inside the base'):
        build_fix_overlay(str(base), {}, str(base / 'overlay'))
