#! /usr/bin/env python3

import time
from github import Github
from datetime import datetime
from pathlib import Path

now = datetime.now()
date = now.strftime("%m %d %Y")

# Provide your personal access token or use a GitHub token if available

with open(Path.home().joinpath('.gittokens','access_token.txt'), 'r') as file:
    access_token = file.read().strip()

# Create a PyGitHub instance using the access token
githue = Github(access_token)

# Specify the repository where you want to open the Pull Request and modify the label
repo_owner = 'TerrenceMcGuinness-NOAA'
repo_name = 'global-workflow'
pr_title = 'Weekly Tests '+date
pr_body = 'Weekly High Resolution Forecast Tests'
label_name = 'CI-Hera-Ready'

# Get the repository object
repo = github.get_repo(f'{repo_owner}/{repo_name}')
sb = repo.get_branch('develop')

# Create a new branch for the Weekly test Pull Request
branch_name = 'weekly_tests'
repo.create_git_ref(ref=f'refs/heads/{branch_name}', sha=sb.commit.sha)
time.sleep(5)
branch = repo.get_branch(branch_name)

#pr_cases = repo.get_dir_contents(_top+'ci/cases/pr')
print('/ci/cases/pr')
pr_cases = repo.get_contents('/ci/cases/pr', ref=branch.name)
for file in pr_cases:
    if file.name.endswith('.yaml'):
        repo.delete_file(path=file.path, message='deleting file for weekly tests', sha=file.sha, branch=branch_name)

weekly_directory = repo.get_contents('/ci/cases/weekly',branch.name)
cases = [file for file in weekly_directory if file.name.endswith('.yaml')]
for case in cases:
    file_name = case.name
    file_content = case.decoded_content.decode('utf-8')
    repo.create_file(path=file_name, message='adding a new file for weekly tests', content=file_content, branch=branch_name)

new_commit = repo.get_commits()[0]

# Create a new Pull Request
pr = repo.create_pull(title=pr_title, body=pr_body, head='weekly_tests', base='develop')

# Add label to the Pull Request
pr.add_to_labels(label_name)