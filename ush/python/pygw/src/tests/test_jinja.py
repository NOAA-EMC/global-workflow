import pytest

from datetime import datetime
from pygw.jinja import Jinja
from pygw.timetools import to_isotime

current_date = datetime.now()
j2tmpl = """Hello {{ name }}! {{ greeting }} It is: {{ current_date | to_isotime }}"""


@pytest.fixture
def create_template(tmp_path):
    file_path = tmp_path / 'template.j2'
    with open(file_path, 'w') as fh:
        fh.write(j2tmpl)


def test_render_stream():
    data = {"name": "John"}
    j = Jinja(j2tmpl, data, allow_missing=True)
    assert j.render == "Hello John! {{ greeting }} It is: {{ current_date }}"

    data = {"name": "Jane", "greeting": "How are you?", "current_date": current_date}
    j = Jinja(j2tmpl, data, allow_missing=False)
    assert j.render == f"Hello Jane! How are you? It is: {to_isotime(current_date)}"


def test_render_file(tmp_path, create_template):

    file_path = tmp_path / 'template.j2'
    data = {"name": "John"}
    j = Jinja(str(file_path), data, allow_missing=True)
    assert j.render == "Hello John! {{ greeting }} It is: {{ current_date }}"

    data = {"name": "Jane", "greeting": "How are you?", "current_date": current_date}
    j = Jinja(str(file_path), data, allow_missing=False)
    assert j.render == f"Hello Jane! How are you? It is: {to_isotime(current_date)}"


# def test_save():
#    assert False
#
#
# def test_dump():
#    assert False
#
