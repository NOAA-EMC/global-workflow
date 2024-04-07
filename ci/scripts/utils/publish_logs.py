#!/usr/bin/env python3

import os
from githubpr import GitHubPR, GitHubDBError
from argparse import ArgumentParser, FileType


def parse_args():
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
    args = parser.parse_args()
    if bool(args.gist) == bool(args.repo):  # Exactly one of the two is required
        parser.error("Exactly one of --gist and --repo is required")
    return args


def add_logs_to_gist(args, emcbot_gh):
    """
    Adds log files to a GitHub gist.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the command line.
    emcbot_gh : GitHubPR
        The GitHubPR object to interact with GitHub.

    Prints
    ------
    The URL of the created gist.
    """

    gist_files = {}
    for file in args.file:
        file_content = file.read()
        gist_files[os.path.basename(file.name)] = emcbot_gh.InputFileContent(file_content)

    gist = emcbot_gh.user.create_gist(public=True, files=gist_files, description=f"error log file from CI run {args.gist[0]}")
    print(gist.html_url)


def upload_logs_to_repo(args, emcbot_gh, emcbot_ci_url):
    """
    Upload log files to a repository.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the command line.
    emcbot_gh : GitHubPR
        The GitHubPR object to interact with GitHub.
    emcbot_ci_url : str
        The URL of the repository to upload the logs to.

    Prints
    ------
    The URL of the uploaded file in the repository.
    """

    path_header = args.repo[0]
    repo_branch = "error_logs"
    repo_path = "ci/error_logs"
    extra = 0
    while True:
        try:
            extra += 1
            file_path_in_repo = f"{repo_path}/{path_header}/" + str(os.path.basename(args.file[0].name))
            content = emcbot_gh.repo.get_contents(file_path_in_repo, ref='error_logs')
            path_header = f'{args.repo[0]}_{str(extra)}'
        except GitHubDBError.GithubException as e:
            break

    for file in args.file:
        file_content = file.read()
        file_path_in_repo = f"{repo_path}/{path_header}/" + str(os.path.basename(file.name))
        emcbot_gh.repo.create_file(file_path_in_repo, "Adding error log file", file_content, branch="error_logs")

    file_url = f"{emcbot_ci_url.rsplit('.',1)[0]}/tree/{repo_branch}/{repo_path}/{path_header}"
    print(file_url)


if __name__ == '__main__':

    args = parse_args()
    emcbot_ci_url = "https://github.com/emcbot/ci-global-workflows.git"
    emcbot_gh = GitHubPR(repo_url=emcbot_ci_url)

    if args.gist:  # Add error logs to a gist in GitHub emcbot's account
        add_logs_to_gist(args, emcbot_gh)

    if args.repo:  # Upload error logs to emcbot's ci-global-workflows error_logs branch
        upload_logs_to_repo(args, emcbot_gh, emcbot_ci_url)
