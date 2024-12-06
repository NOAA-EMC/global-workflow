#!/usr/bin/env python3
import os
from lxml import etree
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

def valid_file_path(path):
    """
    Check if the provided path is a valid file.

    Args:
        path (str): The file path.

    Returns:
        str: The absolute path if valid.

    Raises:
        argparse.ArgumentTypeError: If the path is not a valid file.
    """
    if os.path.isfile(path):
        return os.path.abspath(path)
    else:
        raise argparse.ArgumentTypeError(f"Invalid file path: {path}")

def input_args():

    description = "Extracts the string between <cyclestr> tags within <jobname> tags."
    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)
    
    parser.add_argument('--xmlfile', type=valid_file_path, help='The path to the XML file.')
    return parser.parse_args()

def get_cyclestr_from_jobname(xml_file):
    """
    Extracts the string between <cyclestr> tags within <jobname> tags.

    Args:
        xml_file (str): The path to the XML file.

    Returns:
        list: A list of strings found between <cyclestr> tags within <jobname> tags.
    """
    tree = etree.parse(xml_file)
    root = tree.getroot()
    cyclestr_list = []

    for jobname in root.findall('.//jobname'):
        cyclestr = jobname.find('cyclestr')
        if cyclestr is not None:
            cyclestr_list.append(cyclestr.text)

    return cyclestr_list

# Example usage
if __name__ == "__main__":
    args = input_args()
    xml_file = args.xmlfile
    job_names = get_cyclestr_from_jobname(xml_file)
    for job_name in job_names:
        if "#" not in job_name:
            if job_name.endswith("_@H"):
                job_name = job_name[:-3]
            print(job_name)
