#!/usr/bin/env python3

import sys
import os
from githubpr import GitHubPR
from argparse import ArgumentParser, ArgumentTypeError

def is_file(path):
    """
    Check if path is an existing file.

    Parameters
    ----------
    path : str
        The path to check.

    Returns
    -------
    str
        The path, if it is an existing file.

    Raises
    ------
    argparse.ArgumentTypeError
        If the path is not an existing file.
    """

    if not os.path.isfile(path):
        raise ArgumentTypeError(f"{path} does not exist or is not a file.")
    return path

def input_args():
    """
    Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        The parsed command line arguments.
    """

    description = """Arguments for creating and updating error log files
    """
    parser = ArgumentParser(description=description)

    parser.add_argument('--file', help='path to file for uploading to GitHub', required=True, type=is_file)
    parser.add_argument('--gist', help='create a gist of the file', action='store_true', required=False)
    parser.add_argument('--repo', help='create a file in a repo', nargs=1, metavar='path_in_repo', required=False)
    args = parser.parse_args()
    return args

if __name__ == '__main__':

    args = input_args()
    emcbot_ci_url = "https://github.com/emcbot/ci-global-workflows.git"
    emcbot_gh = GitHubPR(repo_url=emcbot_ci_url)

    full_file_path = os.path.abspath(args.file)
    basename = os.path.basename(full_file_path)

    with open(full_file_path, 'r') as file:
        file_content = file.read()

    if args.gist:
        print(f"Creating gist of {args.file}")
        gist = emcbot_gh.user.create_gist(public=False, files={basename: emcbot_gh.InputFileContent(file_content)}, description="error log file from CI run")
        print(gist.html_url)
        sys.exit(0)

    if args.repo:
        file_path_in_repo = f"ci/error_logs/{args.repo[0]}/" + basename
        file_url = f"{emcbot_ci_url.rsplit('.',1)[0]}/tree/error_logs/ci/error_logs/{args.repo[0]}"
        emcbot_gh.repo.create_file(file_path_in_repo, "Adding error log file", file_content, branch="error_logs")
        print(file_url)


