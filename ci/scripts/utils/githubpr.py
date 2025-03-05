#!/usr/bin/env python3

import os
import re

from github import Auth, Github, GithubException, InputFileContent, UnknownObjectException
from wxflow import which, Logger

# Initialize logger with environment variable for logging level
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


class GitHubDBError(Exception):
    """
    Base class for GitHubDB exceptions.
    """
    UnknownObjectException = UnknownObjectException
    GithubException = GithubException


class GitHubPR(Github):
    """
    GitHubPR is an inherited class from GitHub in pyGitHub for interacting with GitHub pull requests.

    Attributes
    ----------
    repo : github.Repository.Repository
        The GitHub repository to interact with.
    pulls : github.PaginatedList.PaginatedList of github.PullRequest.PullRequest
        The list of open pull requests in the repository, sorted by last updated.
    user : github.AuthenticatedUser.AuthenticatedUser
        The authenticated user.
    InputFileContent : github.InputFileContent.InputFileContent
        The class used to create file content for gists.

    Methods
    -------
    __init__(self, repo_url=None, TOKEN=None)
        Initialize a new GitHubPR instance.
    get_repo_url(self, repo_url=None)
        Set the repository for the GitHubPR instance
        using an URL directly or from 'REPO_URL' environment variable.
    get_pr_list(self)
        Get the numerical list of all pull requests.
    get_ci_pr_list(self, state='Ready', host=None)
        Get the numerical list of all pull requests with a specific state from labels.
        for example if a PR has a label 'CI-Ready-Hera' of the form CI-[state]-[host]
        its corresponding PR number will be included in the list.
    """

    def __init__(self, repo_url=None, TOKEN=None):
        """
        __init__ Initialize a new GitHubPR instance.

        This method authenticates with the GitHub API using the 'gh' CLI tool
        when the TOKEN is not provided. The repository comes from from the 'REPO_URL'
        environment variable when repo_url is not provided.
        """
        if TOKEN is None:
            TOKEN = os.environ.get('GH_TOKEN') or os.environ.get('GITHUB_TOKEN')
            if TOKEN:
                logger.info("Using TOKEN from environment variable.")
            else:
                gh_cli = which('gh')
                gh_cli.add_default_arg(['auth', 'status', '--show-token'])
                gh_output = gh_cli(output=str, error=str)
                token_match = re.search(r"Token:\s*([a-zA-Z0-9_]+)", gh_output)
                if token_match:
                    TOKEN = token_match.group(1)
                    logger.info("Using TOKEN from gh CLI tool.")
                else:
                    raise ValueError("Token not found in gh CLI output.")
        else:
            logger.info("Using provided TOKEN.")

        auth = Auth.Token(TOKEN)
        super().__init__(auth=auth)

        self.repo = self.get_repo_url(repo_url)
        self.pulls = self.repo.get_pulls(state='open', sort='updated', direction='desc')
        self.user = self.get_user()

        self.InputFileContent = InputFileContent

    def get_repo_url(self, repo_url=None):
        """
        set_repo Set the repository for the GitHubPR instance.

        Parameters
        ----------
        repo_url : Repository URL
            The GitHub repository.
        """
        if repo_url is None:
            repo_url = os.environ.get("REPO_URL")
        match = re.search(r"github\.com/(.+)", repo_url)
        repo_identifier = match.group(1)[:-4]
        return self.get_repo(repo_identifier)

    def get_pr_list(self):
        """
        get_pr_list Get the numerical list of all pull requests.

        Returns
        -------
        list
            A list of all pull request numbers.
        """
        return [pull.number for pull in self.pulls]

    def get_ci_pr_list(self, state='Ready', host=None):
        """
        get_ci_pr_list Get a list of pull requests that match a specified state and host.

        Parameters
        ----------
        state : str, optional
            The state of the pull requests to get (default is 'Ready').
        host : str, optional
            The host of the pull requests to get. If None, all hosts are included (default is None).

        Returns
        -------
        list
            A list of pull request numbers that match the specified state and host.
        """
        pr_list = []
        for pull in self.pulls:
            labels = pull.get_labels()
            ci_labels = [s for s in labels if 'CI' in s.name]
            for label in ci_labels:
                if state in label.name:
                    if host is not None:
                        if host.lower() in label.name.lower():
                            pr_list.append(pull.number)
                            break
                    else:
                        pr_list.append(pull.number)
                        break

        return pr_list
