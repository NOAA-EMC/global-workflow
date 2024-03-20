#!/usr/bin/env python3

import sys
import os
from githubpr import GitHubPR
from argparse import ArgumentParser, ArgumentTypeError, FileType

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


    parser.add_argument('--file', help='path to file for uploading to GitHub', required=True, type=FileType('r'), nargs='+')
    parser.add_argument('--gist', help='create a gist of the file', nargs=1, metavar='identifier_string', required=False)
    parser.add_argument('--repo', help='create a file in a repo', nargs=1, metavar='path_in_repo', required=False)
    args = parser.parse_args()
    return args

if __name__ == '__main__':

    args = input_args()
    emcbot_ci_url = "https://github.com/emcbot/ci-global-workflows.git"
    emcbot_gh = GitHubPR(repo_url=emcbot_ci_url)

    if args.gist:

        gist_files = {}
        for file in args.file:
            file_content = file.read()
            gist_files[file.name] = emcbot_gh.InputFileContent(file_content)

        gist = emcbot_gh.user.create_gist(public=True, files=gist_files, description=f"error log file from CI run {args.gist[0]}")
        print(gist.html_url)

    if args.repo:
        for file in args.file:
            file_content = file.read()
            file_path_in_repo = f"ci/error_logs/{args.repo[0]}/" + str(os.path.basename(file.name))
            emcbot_gh.repo.create_file(file_path_in_repo, "Adding error log file", file_content, branch="error_logs")
        file_url = f"{emcbot_ci_url.rsplit('.',1)[0]}/tree/error_logs/ci/error_logs/{args.repo[0]}"
        print(file_url)
