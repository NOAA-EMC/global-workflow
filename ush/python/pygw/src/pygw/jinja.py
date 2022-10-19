import io
import os
import sys
import jinja2
from pathlib import Path


class Jinja:

    def __init__(self, template_path, data, allow_missing=True):
        """
        Given a path to a (jinja2) template and a data object, substitute the
        template file with data.
        Allow for retaining missing or undefined variables.
        """

        self.data = data
        self.undefined = jinja2.Undefined if allow_missing else jinja2.StrictUndefined

        if Path(template_path).is_file():
            self.template_path = Path(template_path)
            self.output = self._render_file()
        else:
            self.output = self._render_stream()

    def _render_stream(self):
        raise NotImplementedError("Unable to handle templates other than files")

    def _render_file(self):
        template_dir = self.template_path.parent
        template_file = self.template_path.relative_to(template_dir)

        dirname = os.path.dirname(str(self.template_path))
        relpath = os.path.relpath(str(self.template_path), dirname)

        loader = jinja2.FileSystemLoader(template_dir)
        output = self._render(str(template_file), loader)

        return output

    def _render(self, template_name, loader):
        env = jinja2.Environment(loader=loader, undefined=self.undefined)
        template = env.get_template(template_name)
        try:
            rendered = template.render(**self.data)
        except jinja2.UndefinedError as ee:
            raise Exception(f"Undefined variable in Jinja2 template\n{ee}")

        return rendered

    def save(self, output_file):
        with open(output_file, 'wb') as fh:
            fh.write(self.output.encode("utf-8"))

    def dump(self):
        io.TextIOWrapper(sys.stdout.buffer,
                         encoding="utf-8").write(self.output)
