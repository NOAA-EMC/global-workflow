#!/usr/bin/env python3
# gen_ufo_geoval_table.py
# generate a HTML table of UFO validation status
import argparse


def gen_ufo_geoval_table(oblist, results, output):
    """Generate HTML table of UFO validation results using GeoVaLs

    Parameters
    ----------
    oblist: str
        a path to a text file of a list of observation types (one per line)
    results: str
        a path to a text file of test results for ob types
    output: str
        a path to where the output HTML table should be written to
    """
    # first, read the list of ob types from a text file
    with open(oblist) as obfile:
        obtypes = [line.rstrip() for line in obfile]

    # create a dict of the status of each ob type
    status_dict = {}

    # loop through the results and determine the status of each ob type
    with open(results) as resultfile:
        resultlines = [line.rstrip() for line in resultfile]
    for obtype in obtypes:
        status = 'Missing'
        for line in resultlines[1:]:
            yamlname = line.split('.')[0]
            if yamlname == obtype:
                status = line.split(' ')[1]
        status_dict[obtype] = status

    # start writing the HTML file
    color_dict = {
        'Missing': 'Red',
        'Passes': 'Green',
        'Fails': 'Red',
    }
    with open(output, 'w') as f:
        f.write('GDAS UFO validation status using GeoVaLs<br>\n')
        f.write(resultlines[0])
        f.write('<br>\n')
        f.write('<table style="font-size:90%;width:80%;text-align:left;">\n')
        f.write('<tr><th>Observation Type</th><th>Testing YAML</th><th>Passes Validation</th></tr>\n')
        for obtype in obtypes:
            f.write(f"<tr><td style='border-bottom:solid;'>{obtype}</td>")
            if status_dict[obtype] == 'Missing':
                f.write("<td style='background-color:Red;'>No YAML</td><td style='background-color:Gray;'>N/A</td></tr>\n")
            else:
                colorstr = color_dict[status_dict[obtype]]
                statusstr = status_dict[obtype]
                f.write(f"<td style='background-color:Green;'>YAML Exists</td><td style='background-color:{colorstr};'>{statusstr}</td></tr>\n")
        f.write('</table>\n')
    print(f"Wrote HTML table to {output}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--oblist', type=str, help='Path to text file of list of ob types')
    parser.add_argument('--results', type=str, help='Path to text file of test results')
    parser.add_argument('--output', type=str, help='Path to output HTML file')
    args = parser.parse_args()
    gen_ufo_geoval_table(args.oblist, args.results, args.output)
