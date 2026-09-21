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
    """User-supplied files: some names exist in base, some are new."""
    root = tmp_path / 'user'
    for rel in ['MOM/regional.mom6.nc',          # new in mom6/025
                'MOM/ocean_hgrid.nc',            # exists in mom6/025
                'MOM/notes.txt',                 # new in mom6/025
                'OROG/C48/C48_oro_data.tile1.nc',  # exists in orog/C48
                'OROG/C48/C48_grid.tile1.nc',      # exists in orog/C48
                'OROG/C48/C48_new.nc',             # new in orog/C48
                'OROG/C96/C96_oro_data.tile1.nc',  # exists in orog/C96
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


def _build(base, tmp_path, fix_files):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), fix_files, str(dest))
    return dest


def _fails(base, tmp_path, fix_files, match):
    dest = tmp_path / 'exp' / 'fix'
    with pytest.raises(FixOverlayError, match=match):
        build_fix_overlay(str(base), fix_files, str(dest))
    assert not dest.exists()


# --- general behaviour ------------------------------------------------------

def test_no_entries_is_all_links_to_base(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    result = build_fix_overlay(str(base), {}, str(dest))

    assert result == str(dest)
    assert (dest / MANIFEST_NAME).is_file()
    for comp in ['orog', 'mom6', 'cpl', 'am']:
        assert _target(dest / comp) == str(base / comp)


def test_lazy_expansion(base, user, tmp_path):
    src = user / 'MOM' / 'ocean_hgrid.nc'
    dest = _build(base, tmp_path, {'replace': {'mom6/025/ocean_hgrid.nc': str(src)}})

    # touched path is real directories down to the leaf
    assert (dest / 'mom6').is_dir() and not (dest / 'mom6').is_symlink()
    assert (dest / 'mom6' / '025').is_dir() and not (dest / 'mom6' / '025').is_symlink()
    assert _target(dest / 'mom6' / '025' / 'ocean_hgrid.nc') == str(src)
    assert (dest / 'mom6' / '025' / 'ocean_hgrid.nc').read_text() == 'ocean_hgrid.nc'
    # sibling at the expanded level links back to base
    assert _target(dest / 'mom6' / '025' / 'MOM_input') == str(base / 'mom6' / '025' / 'MOM_input')
    # untouched components stay a single link
    assert _target(dest / 'orog') == str(base / 'orog')
    assert _target(dest / 'cpl') == str(base / 'cpl')


def test_empty_subsections_are_allowed(base, tmp_path):
    dest = _build(base, tmp_path, {'replace': None, 'add': {}})
    assert _target(dest / 'orog') == str(base / 'orog')


def test_manifest_records_entries_and_links(base, user, tmp_path):
    fix_files = {'replace': {'orog/C48': str(user / 'OROG' / 'C48' / 'C48_*.tile1.nc')},
                 'add': {'wave': str(user / 'wave')}}
    dest = _build(base, tmp_path, fix_files)

    manifest = _manifest(dest)
    assert manifest['base'] == str(base)
    assert manifest['entries'] == fix_files
    assert manifest['links'] == {
        'orog/C48/C48_grid.tile1.nc': str(user / 'OROG' / 'C48' / 'C48_grid.tile1.nc'),
        'orog/C48/C48_oro_data.tile1.nc': str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc'),
        'wave': str(user / 'wave'),
    }


def test_rebuild_replaces_previous_overlay(base, user, tmp_path):
    _build(base, tmp_path, {'add': {'wave': str(user / 'wave')}})
    dest = _build(base, tmp_path, {'add': {'cpl/aC48o008': str(user / 'CPL' / 'aC48o008')}})

    assert not (dest / 'wave').exists()
    assert _target(dest / 'cpl' / 'aC48o008') == str(user / 'CPL' / 'aC48o008')


def test_refuses_to_clobber_foreign_directory(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    _touch(dest / 'precious')
    with pytest.raises(FixOverlayError, match='not a fix overlay'):
        build_fix_overlay(str(base), {}, str(dest))
    assert (dest / 'precious').is_file()


# --- replace: table ---------------------------------------------------------
# src \ dst      file                 directory             absent

def test_replace_file_onto_file(base, user, tmp_path):
    src = user / 'MOM' / 'ocean_hgrid.nc'
    dest = _build(base, tmp_path, {'replace': {'mom6/025/ocean_hgrid.nc': str(src)}})
    assert _target(dest / 'mom6' / '025' / 'ocean_hgrid.nc') == str(src)


def test_replace_file_onto_directory_is_kind_mismatch(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'orog/C48': str(user / 'wave' / 'mod_def.glo_15m')}},
           'is a directory in the fix tree but the source is a file')


def test_replace_file_onto_absent_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'mom6/025/ocean_hgird.nc': str(user / 'MOM' / 'ocean_hgrid.nc')}},
           "not in the fix tree; use 'add'")


def test_replace_directory_onto_file_is_kind_mismatch(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'mom6/025/MOM_input': str(user / 'MOM')}},
           'is a file in the fix tree but the source is a directory')


def test_replace_directory_onto_directory(base, user, tmp_path):
    src = user / 'OROG' / 'C48'
    dest = _build(base, tmp_path, {'replace': {'orog/C48': str(src)}})
    assert _target(dest / 'orog' / 'C48') == str(src)
    assert _target(dest / 'orog' / 'C96') == str(base / 'orog' / 'C96')


def test_replace_directory_onto_absent_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'cpl/aC48o008': str(user / 'CPL' / 'aC48o008')}},
           "not in the fix tree; use 'add'")


def test_replace_glob_onto_file_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'mom6/025/MOM_input': str(user / 'MOM' / '*.nc')}},
           'is a file in the fix tree; a glob source needs a directory destination')


def test_replace_glob_into_directory(base, user, tmp_path):
    dest = _build(base, tmp_path, {'replace': {'orog/C48': str(user / 'OROG' / 'C48' / 'C48_*.tile1.nc')}})
    c48 = dest / 'orog' / 'C48'
    assert c48.is_dir() and not c48.is_symlink()
    assert _target(c48 / 'C48_oro_data.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc')
    assert _target(c48 / 'C48_grid.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_grid.tile1.nc')
    assert not (c48 / 'C48_new.nc').exists()  # not matched, not touched


def test_replace_glob_with_a_new_match_is_an_error(base, user, tmp_path):
    # C48_new.nc is matched by the glob but is not in orog/C48 in base
    _fails(base, tmp_path, {'replace': {'orog/C48': str(user / 'OROG' / 'C48' / '*')}},
           r"'orog/C48/C48_new.nc' \(matched by 'replace: orog/C48'\) is not in the fix tree")


def test_replace_glob_matching_directories(base, user, tmp_path):
    # matches are the C48/ and C96/ directories; each replaces its counterpart wholesale
    dest = _build(base, tmp_path, {'replace': {'orog': str(user / 'OROG' / 'C*')}})
    assert _target(dest / 'orog' / 'C48') == str(user / 'OROG' / 'C48')
    assert _target(dest / 'orog' / 'C96') == str(user / 'OROG' / 'C96')


def test_replace_glob_kind_mismatch_per_match(base, user, tmp_path):
    # user/OROG/C48 is a directory, base/orog/C48/C48 does not exist -> not-in-tree error;
    # so build a case where the name exists but kinds differ:
    _touch(user / 'X' / 'C48')  # a file named like base's orog/C48 directory
    _fails(base, tmp_path, {'replace': {'orog': str(user / 'X' / '*')}},
           r"'orog/C48' \(matched by 'replace: orog'\) is a directory in the fix tree but the source is a file")


def test_replace_glob_onto_absent_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'replace': {'mom6/008': str(user / 'MOM' / '*.nc')}},
           "'mom6/008' is not in the fix tree; use 'add'")


# --- add: table -------------------------------------------------------------
# src \ dst      file                 directory             absent

def test_add_file_onto_file_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'mom6/025/ocean_hgrid.nc': str(user / 'MOM' / 'ocean_hgrid.nc')}},
           "already exists in the fix tree; use 'replace'")


def test_add_file_onto_directory_is_an_error_with_hint(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'orog/C48': str(user / 'OROG' / 'C48' / 'C48_new.nc')}},
           "already exists in the fix tree; use 'replace'.*name the full path 'orog/C48/C48_new.nc'")


def test_add_file_onto_absent(base, user, tmp_path):
    src = user / 'MOM' / 'regional.mom6.nc'
    dest = _build(base, tmp_path, {'add': {'mom6/008/regional.mom6.nc': str(src)}})
    assert (dest / 'mom6' / '008').is_dir()
    assert _target(dest / 'mom6' / '008' / 'regional.mom6.nc') == str(src)
    assert _target(dest / 'mom6' / '025') == str(base / 'mom6' / '025')


def test_add_directory_onto_existing_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'orog/C48': str(user / 'OROG' / 'C48')}},
           "already exists in the fix tree; use 'replace'")
    _fails(base, tmp_path, {'add': {'mom6/025/MOM_input': str(user / 'MOM')}},
           "already exists in the fix tree; use 'replace'")


def test_add_directory_onto_absent(base, user, tmp_path):
    src = user / 'CPL' / 'aC48o008'
    dest = _build(base, tmp_path, {'add': {'cpl/aC48o008': str(src)}})
    assert _target(dest / 'cpl' / 'aC48o008') == str(src)
    assert _target(dest / 'cpl' / 'aC48o500') == str(base / 'cpl' / 'aC48o500')
    assert (dest / 'cpl' / 'aC48o008' / 'grid_spec.nc').is_file()


def test_add_new_component(base, user, tmp_path):
    dest = _build(base, tmp_path, {'add': {'wave': str(user / 'wave')}})
    assert _target(dest / 'wave') == str(user / 'wave')
    assert _target(dest / 'orog') == str(base / 'orog')


def test_add_glob_onto_file_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'mom6/025/MOM_input': str(user / 'MOM' / '*.nc')}},
           'is a file in the fix tree; a glob source needs a directory destination')


def test_add_glob_into_existing_directory(base, user, tmp_path):
    dest = _build(base, tmp_path, {'add': {'orog/C48': str(user / 'OROG' / 'C48' / 'C48_new*')}})
    c48 = dest / 'orog' / 'C48'
    assert _target(c48 / 'C48_new.nc') == str(user / 'OROG' / 'C48' / 'C48_new.nc')
    assert _target(c48 / 'C48_grid.tile1.nc') == str(base / 'orog' / 'C48' / 'C48_grid.tile1.nc')


def test_add_glob_with_an_existing_match_is_an_error(base, user, tmp_path):
    # ocean_hgrid.nc is matched by *.nc and already exists in mom6/025
    _fails(base, tmp_path, {'add': {'mom6/025': str(user / 'MOM' / '*.nc')}},
           r"'mom6/025/ocean_hgrid.nc' \(matched by 'add: mom6/025'\) already exists")


def test_add_glob_into_absent_directory(base, user, tmp_path):
    dest = _build(base, tmp_path, {'add': {'mom6/008': str(user / 'MOM' / '*.nc')}})
    d = dest / 'mom6' / '008'
    assert d.is_dir() and not d.is_symlink()
    assert _target(d / 'regional.mom6.nc') == str(user / 'MOM' / 'regional.mom6.nc')
    assert _target(d / 'ocean_hgrid.nc') == str(user / 'MOM' / 'ocean_hgrid.nc')
    assert not (d / 'notes.txt').exists()


# --- lists of sources -------------------------------------------------------

def test_list_of_sources_under_one_destination(base, user, tmp_path):
    fix_files = {'replace': {'orog/C48': [str(user / 'OROG' / 'C48' / '*_oro_*'),
                                          str(user / 'OROG' / 'C48' / '*_grid*')]}}
    dest = _build(base, tmp_path, fix_files)
    c48 = dest / 'orog' / 'C48'
    assert _target(c48 / 'C48_oro_data.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc')
    assert _target(c48 / 'C48_grid.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_grid.tile1.nc')
    assert _manifest(dest)['entries'] == fix_files


def test_mixed_directory_is_replace_plus_add(base, user, tmp_path):
    # the idiom for "some of these files are new, some override": two entries, one per mode
    fix_files = {'replace': {'orog/C48': str(user / 'OROG' / 'C48' / '*.tile1.nc')},
                 'add': {'orog/C48/C48_new.nc': str(user / 'OROG' / 'C48' / 'C48_new.nc')}}
    dest = _build(base, tmp_path, fix_files)
    c48 = dest / 'orog' / 'C48'
    assert _target(c48 / 'C48_oro_data.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc')
    assert _target(c48 / 'C48_new.nc') == str(user / 'OROG' / 'C48' / 'C48_new.nc')


def test_empty_source_list_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'wave': []}}, 'empty list of sources')


# --- conflicts and malformed input -----------------------------------------

def test_duplicate_leaf_is_an_error(base, user, tmp_path):
    fix_files = {'replace': {'orog/C48': [str(user / 'OROG' / 'C48' / '*.tile1.nc'),
                                          str(user / 'OROG' / 'C48' / 'C48_grid*')]}}
    _fails(base, tmp_path, fix_files, "'orog/C48/C48_grid.tile1.nc' is set by both")


def test_entry_inside_replaced_directory_is_an_error(base, user, tmp_path):
    fix_files = {'replace': {'orog/C48': str(user / 'OROG' / 'C48')},
                 'add': {'orog/C48/extra.nc': str(user / 'OROG' / 'C48' / 'C48_new.nc')}}
    _fails(base, tmp_path, fix_files, 'linked wholesale')


def test_entry_under_a_base_file_is_an_error(base, user, tmp_path):
    _fails(base, tmp_path, {'add': {'am/global_o3prdlos.f77/inner': str(user / 'wave')}},
           "under 'am/global_o3prdlos.f77', which is a file in the fix tree")


@pytest.mark.parametrize('fix_files, match', [
    ({'add': {'mom6/008/x.nc': '{user}/does/not/exist.nc'}}, 'does not exist'),
    ({'add': {'orog/C48': '{user}/OROG/C48/*.grb'}}, 'matched nothing'),
    ({'replace': {'orog/C48': 'OROG/C48'}}, 'absolute'),
    ({'replace': {'/orog/C48': '{user}/OROG/C48'}}, 'relative'),
    ({'add': {'../escape': '{user}/wave'}}, r"'\.\.'"),
    ({'replace': {'.': '{user}/wave'}}, 'fix root'),
    ({'add': {'': '{user}/wave'}}, 'non-empty'),
    ({'add': {'wave': None}}, 'non-empty'),
    ({'add': {'wave': [None]}}, 'non-empty'),
    ({'merge': {'orog/C48': '{user}/OROG/C48'}}, 'unknown sub-section'),
    ({'add': ['wave']}, 'must be a mapping'),
])
def test_invalid_entries(base, user, tmp_path, fix_files, match):
    def fmt(v):
        if isinstance(v, str):
            return v.format(user=user)
        if isinstance(v, list):
            return [fmt(x) for x in v]
        return v
    fix_files = {mode: ({k: fmt(v) for k, v in entries.items()} if isinstance(entries, dict) else entries)
                 for mode, entries in fix_files.items()}
    _fails(base, tmp_path, fix_files, match)


def test_section_must_be_a_mapping(base, tmp_path):
    with pytest.raises(FixOverlayError, match='must be a mapping'):
        build_fix_overlay(str(base), ['orog'], str(tmp_path / 'exp' / 'fix'))


def test_dest_inside_base_is_an_error(base, tmp_path):
    with pytest.raises(FixOverlayError, match='inside the base'):
        build_fix_overlay(str(base), {}, str(base / 'overlay'))
