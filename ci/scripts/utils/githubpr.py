#!/usr/bin/env python3

import os
import re

from github import Github
from github.Repository import Repository
from wxflow import which


class GitHubPR:
    """
    GitHubPR A class for interacting with GitHub pull requests.

    Attributes
    ----------
    gh : Github
        The Github instance for interacting with the GitHub API.
    repo : Repository
        The GitHub repository.
    host : Host
        The host machine.
    """

    def __init__(self, repo_url=None):
        """
        __init__ Initialize a new GitHubPR instance.

        This method authenticates with the GitHub API using the 'gh' CLI tool,
        gets the repository from the 'REPO_URL' environment variable, and
        initializes the host machine.
        """
        gh_cli = which('gh')
        gh_cli.add_default_arg(['auth', 'status', '--show-token'])
        gh_access_token = gh_cli(output=str, error=str).split('\n')[3].split(': ')[1]
        self._gh = Github(gh_access_token)
        repo = self.get_repo_url(repo_url)
        self.pulls = repo.get_pulls(state='open', sort='updated', direction='desc')
        self.repo = repo

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
        return self._gh.get_repo(repo_identifier)

    def get_pr_list(self):
        """
        get_pr_list Get the numerical list of all pull requests.

        Returns
        -------
        list
            A list of all pull request numbers.
        """
        pr_list = []
        for pull in self.pulls:
            pr_list.append(pull.number)
        return pr_list

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
                    else:
                        pr_list.append(pull.number)

        return pr_list
