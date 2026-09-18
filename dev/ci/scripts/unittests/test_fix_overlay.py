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


def test_no_entries_is_all_links_to_base(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    result = build_fix_overlay(str(base), {}, str(dest))

    assert result == str(dest)
    assert (dest / MANIFEST_NAME).is_file()
    for comp in ['orog', 'mom6', 'cpl', 'am']:
        assert _target(dest / comp) == str(base / comp)


def test_replace_file_expands_only_its_path(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'MOM' / 'regional.mom6.nc'
    build_fix_overlay(str(base), {'mom6/025/regional.mom6.nc': str(src)}, str(dest))

    # touched path is real directories down to the leaf
    assert (dest / 'mom6').is_dir() and not (dest / 'mom6').is_symlink()
    assert (dest / 'mom6' / '025').is_dir() and not (dest / 'mom6' / '025').is_symlink()
    assert _target(dest / 'mom6' / '025' / 'regional.mom6.nc') == str(src)
    # siblings at each expanded level link back to base
    assert _target(dest / 'mom6' / '025' / 'ocean_hgrid.nc') == str(base / 'mom6' / '025' / 'ocean_hgrid.nc')
    assert _target(dest / 'mom6' / '025' / 'MOM_input') == str(base / 'mom6' / '025' / 'MOM_input')
    # untouched components stay a single link
    assert _target(dest / 'orog') == str(base / 'orog')
    assert _target(dest / 'cpl') == str(base / 'cpl')


def test_replace_existing_file(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'MOM' / 'ocean_hgrid.nc'
    build_fix_overlay(str(base), {'mom6/025/ocean_hgrid.nc': str(src)}, str(dest))

    assert _target(dest / 'mom6' / '025' / 'ocean_hgrid.nc') == str(src)
    assert (dest / 'mom6' / '025' / 'ocean_hgrid.nc').read_text() == 'ocean_hgrid.nc'


def test_add_directory(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'CPL' / 'aC48o008'
    build_fix_overlay(str(base), {'cpl/aC48o008': str(src)}, str(dest))

    assert _target(dest / 'cpl' / 'aC48o008') == str(src)
    assert _target(dest / 'cpl' / 'aC48o500') == str(base / 'cpl' / 'aC48o500')
    assert (dest / 'cpl' / 'aC48o008' / 'grid_spec.nc').is_file()


def test_replace_whole_component(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'wave'
    build_fix_overlay(str(base), {'wave': str(src)}, str(dest))

    assert _target(dest / 'wave') == str(src)
    assert _target(dest / 'orog') == str(base / 'orog')


def test_add_new_component_and_nested_path(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    src = user / 'MOM' / 'regional.mom6.nc'
    build_fix_overlay(str(base), {'mom6/008/regional.mom6.nc': str(src)}, str(dest))

    assert (dest / 'mom6' / '008').is_dir()
    assert _target(dest / 'mom6' / '008' / 'regional.mom6.nc') == str(src)
    assert _target(dest / 'mom6' / '025') == str(base / 'mom6' / '025')


def test_merge_glob(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'orog/C48': str(user / 'OROG' / 'C48' / '*')}, str(dest))

    c48 = dest / 'orog' / 'C48'
    assert c48.is_dir() and not c48.is_symlink()
    assert _target(c48 / 'C48_oro_data.tile1.nc') == str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc')
    assert _target(c48 / 'C48_new.nc') == str(user / 'OROG' / 'C48' / 'C48_new.nc')
    assert _target(c48 / 'C48_grid.tile1.nc') == str(base / 'orog' / 'C48' / 'C48_grid.tile1.nc')
    assert _target(dest / 'orog' / 'C96') == str(base / 'orog' / 'C96')


def test_merge_filtered_glob(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'mom6/025': str(user / 'MOM' / '*.nc')}, str(dest))

    d = dest / 'mom6' / '025'
    assert _target(d / 'regional.mom6.nc') == str(user / 'MOM' / 'regional.mom6.nc')
    assert _target(d / 'ocean_hgrid.nc') == str(user / 'MOM' / 'ocean_hgrid.nc')
    assert not (d / 'notes.txt').exists()
    assert _target(d / 'MOM_input') == str(base / 'mom6' / '025' / 'MOM_input')


def test_manifest_records_entries_and_links(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    entries = {'orog/C48': str(user / 'OROG' / 'C48' / '*'),
               'wave': str(user / 'wave')}
    build_fix_overlay(str(base), entries, str(dest))

    with open(dest / MANIFEST_NAME) as fh:
        manifest = yaml.safe_load(fh)
    assert manifest['base'] == str(base)
    assert manifest['entries'] == entries
    assert manifest['links'] == {
        'orog/C48/C48_new.nc': str(user / 'OROG' / 'C48' / 'C48_new.nc'),
        'orog/C48/C48_oro_data.tile1.nc': str(user / 'OROG' / 'C48' / 'C48_oro_data.tile1.nc'),
        'wave': str(user / 'wave'),
    }


def test_rebuild_replaces_previous_overlay(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    build_fix_overlay(str(base), {'wave': str(user / 'wave')}, str(dest))
    build_fix_overlay(str(base), {'cpl/aC48o008': str(user / 'CPL' / 'aC48o008')}, str(dest))

    assert not (dest / 'wave').exists()
    assert _target(dest / 'cpl' / 'aC48o008') == str(user / 'CPL' / 'aC48o008')


def test_refuses_to_clobber_foreign_directory(base, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    _touch(dest / 'precious')
    with pytest.raises(FixOverlayError, match='not a fix overlay'):
        build_fix_overlay(str(base), {}, str(dest))
    assert (dest / 'precious').is_file()


@pytest.mark.parametrize('entries, match', [
    ({'mom6/008/x.nc': '{user}/does/not/exist.nc'}, 'does not exist'),
    ({'orog/C48': '{user}/OROG/C48/*.grb'}, 'matched nothing'),
    ({'orog/C48': 'OROG/C48'}, 'absolute'),
    ({'/orog/C48': '{user}/OROG/C48'}, 'relative'),
    ({'../escape': '{user}/wave'}, r"'\.\.'"),
    ({'.': '{user}/wave'}, 'fix root'),
    ({'': '{user}/wave'}, 'non-empty'),
    ({'orog/C48': None}, 'non-empty'),
])
def test_invalid_entries(base, user, tmp_path, entries, match):
    dest = tmp_path / 'exp' / 'fix'
    entries = {k: (v.format(user=user) if isinstance(v, str) else v) for k, v in entries.items()}
    with pytest.raises(FixOverlayError, match=match):
        build_fix_overlay(str(base), entries, str(dest))
    assert not dest.exists()


def test_duplicate_destination_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    entries = {'mom6/025': str(user / 'MOM' / '*.nc'),
               'mom6/025/ocean_hgrid.nc': str(user / 'MOM' / 'ocean_hgrid.nc')}
    with pytest.raises(FixOverlayError, match='set by both'):
        build_fix_overlay(str(base), entries, str(dest))


def test_entry_inside_replaced_directory_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    entries = {'cpl': str(user / 'CPL'),
               'cpl/aC48o008/grid_spec.nc': str(user / 'CPL' / 'aC48o008' / 'grid_spec.nc')}
    with pytest.raises(FixOverlayError, match='replaced wholesale'):
        build_fix_overlay(str(base), entries, str(dest))


def test_entry_under_a_base_file_is_an_error(base, user, tmp_path):
    dest = tmp_path / 'exp' / 'fix'
    entries = {'am/global_o3prdlos.f77/inner': str(user / 'wave')}
    with pytest.raises(FixOverlayError, match='not a directory'):
        build_fix_overlay(str(base), entries, str(dest))


def test_dest_inside_base_is_an_error(base, user, tmp_path):
    with pytest.raises(FixOverlayError, match='inside the base'):
        build_fix_overlay(str(base), {}, str(base / 'overlay'))


def test_section_must_be_a_mapping(base, tmp_path):
    with pytest.raises(FixOverlayError, match='mapping'):
        build_fix_overlay(str(base), ['orog'], str(tmp_path / 'exp' / 'fix'))
