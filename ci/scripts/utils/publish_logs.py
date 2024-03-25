#!/usr/bin/env python3

import os
import secrets
import re
import string
from githubpr import GitHubPR, GitHubDBError
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

    parser.add_argument('--file', help='path to file for uploading to GitHub', required=False, type=FileType('r'), nargs='+')
    parser.add_argument('--gist', help='create a gist of the file', nargs=1, metavar='identifier_string', required=False)
    parser.add_argument('--repo', help='create a file in a repo', nargs=1, metavar='path_header', required=False)
    parser.add_argument('--delete_gists', help='deletes all the gits from authenticated user', action='store_true', default=False, required=False)
    args = parser.parse_args()
    if not args.delete_gists and not args.file:
        parser.error("--file is required when --delete_gists is not used")
    return args


def random_string() -> str:
    characters = string.ascii_letters + string.digits + string.punctuation
    return re.sub('[^A-Za-z0-9 ]+', '', ''.join(secrets.choice(characters) for _ in range(6)))


if __name__ == '__main__':

    args = input_args()
    emcbot_ci_url = "https://github.com/emcbot/ci-global-workflows.git"
    emcbot_gh = GitHubPR(repo_url=emcbot_ci_url)

    if args.gist:  # Add error logs to a gist in GitHub emcbot's account

        gist_files = {}
        for file in args.file:
            file_content = file.read()
            gist_files[os.path.basename(file.name)] = emcbot_gh.InputFileContent(file_content)

        gist = emcbot_gh.user.create_gist(public=True, files=gist_files, description=f"error log file from CI run {args.gist[0]}")
        print(gist.html_url)

    if args.repo:  # Upload error logs to emcbot's ci-global-workflows error_logs branch
        path_header = args.repo[0]
        file_path_in_repo = f"ci/error_logs/{path_header}/" + str(os.path.basename(args.file[0].name))
        try:
            content = emcbot_gh.repo.get_contents(file_path_in_repo, ref='error_logs')
            path_header = f'{path_header}_{random_string()}'
        except GitHubDBError.GithubException as e:
            pass

        for file in args.file:
            file_content = file.read()
            file_path_in_repo = f"{path_header}/" + str(os.path.basename(file.name))
            emcbot_gh.repo.create_file(file_path_in_repo, "Adding error log file", file_content, branch="error_logs")

        file_url = f"{emcbot_ci_url.rsplit('.',1)[0]}/tree/error_logs/{path_header}"
        print(file_url)

    if args.delete_gists:  # Helper feature to delete all gists from authenticated user account

        confirm = input(f"Are you sure you want to delete all gists from {emcbot_gh.repo.full_name} as {emcbot_gh.user.login} ? Type 'yes' to confirm: ")
        if confirm.lower() != 'yes':
            print("Aborted.")
            exit(0)
        for gist in emcbot_gh.user.get_gists():
            gist.delete()
            print(f"Gist {gist.id} deleted")
        print(f"All gists deleted for authenticated user {emcbot_gh.user.login} in the repository {emcbot_gh.repo.full_name}")
