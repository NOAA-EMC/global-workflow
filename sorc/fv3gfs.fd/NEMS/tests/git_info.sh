#! /bin/sh

echo REPO TOP:
git branch -vv | head -1 | cut -c3- 
git remote show origin | grep 'Fetch URL:' | sed "s,^ *,,g"
git status -suno
echo
git submodule foreach sh -c 'git branch -vv | head -1 | cut -c3- ; git remote show origin | grep "Fetch URL:" | sed "s,^ *,,g" ; git status -suno ; echo'
