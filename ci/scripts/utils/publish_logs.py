#!/usr/bin/env python3

import sys
import os
from githubpr import GitHubPR
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, REMAINDER


emcbot_url = "https://github.com/emcbot/global-workflow.git"
emcbot_gh = GitHubPR(emcbot_url)

file_subdir = sys.argv[1]
file_content_path = sys.argv[2]

with open(file_content_path, 'r') as file:
    file_content = file.read()

file_commit_message = "Add new file"
file_path=f"ci/error_logs/{file_subdir}/"+file_content_path.split("/")[-1]

print(f"Uploading {file_path} to {emcbot_url}")

emcbot_gh.repo.create_file(file_path, file_commit_message, file_content, branch="error_logs")
