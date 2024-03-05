#!/usr/bin/env python3

from github import Github
from wxflow import which
from workflow.hosts import Host

if __name__ == "__main__":

    gh_cli = which('gh')
    gh_cli.add_default_arg(['auth', 'status','--show-token'])
    gh_access_token=gh_cli(output=str, error=str).split('\n')[3].split(': ')[1]
    gh = Github(gh_access_token)

    #repo = g.get_repo("TerrenceMcGuinness-NOAA/global-workflow")
    repo = gh.get_repo("NOAA-EMC/global-workflow")
    pulls = repo.get_pulls(state='open')

    host = Host()
    pr_list = []
    for pull in pulls:
        labels = pull.get_labels()
        ci_labels = [s for s in labels if 'CI' in s.name]
        for label in ci_labels:
            if host.machine.capitalize() in label.name:
                pr_list.append(pull)
                print(f"label: {label.name} PR: {pull.number}")

    print(f"pr_list: {pr_list}")