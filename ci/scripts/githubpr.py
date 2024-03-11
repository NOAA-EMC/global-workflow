#!/usr/bin/env python3

import os, sys
import re

from github import Github
from wxflow import which
from workflow.hosts import Host

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

    def __init__(self):
        """
        __init__ Initialize a new GitHubPR instance.

        This method authenticates with the GitHub API using the 'gh' CLI tool,
        gets the repository from the 'REPO_URL' environment variable, and
        initializes the host machine.
        """
        gh_cli = which('gh')
        gh_cli.add_default_arg(['auth', 'status', '--show-token'])
        gh_access_token=gh_cli(output=str, error=str).split('\n')[3].split(': ')[1]
        gh = Github(gh_access_token)
        self.gh = gh

        repo_url = os.environ.get("REPO_URL")
        match = re.search(r"github\.com/(.+)", repo_url)
        repo_identifier = match.group(1)[:-4]
        repo = gh.get_repo(repo_identifier)
        self.repo = repo
        self.host = Host()

    def get_open_pr_list(self, state='Ready'):
        """
        get_open_pr_list Get a list of open pull requests.

        Parameters
        ----------
        state : str, optional
            The state of the pull requests to get (default is 'Ready').

        Returns
        -------
        list
            A list of pull request numbers that are open and match the specified state.
        list
            A list of pull request numbers that have the 'Kill' label.
        """
        pulls = self.repo.get_pulls(state='closed', sort='updated', direction='desc')
        pr_list = []
        pr_kill_list = []
        for pull in pulls:
            labels = pull.get_labels()
            ci_labels = [s for s in labels if 'CI' in s.name]
            for label in ci_labels:
                if 'Kill' in label.name:
                    pr_kill_list.append(pull.number)
                    continue
                if self.host.machine.capitalize() in label.name:
                    if state in label.name:
                        pr_list.append(pull.number)
        return pr_list, pr_kill_list