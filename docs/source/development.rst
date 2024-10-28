###################################
Contributing to the Global Workflow
###################################

This section is devoted to developers who wish to contribute to the Global Workflow repository. 

.. _managers:

=============
Code managers
=============

 * Kate Friedman - @KateFriedman-NOAA / kate.friedman@noaa.gov
 * Walter Kolczynski - @WalterKolczynski-NOAA / walter.kolczynski@noaa.gov
 * David Huber - @DavidHuber-NOAA / david.huber@noaa.gov

.. _development:

========================
Where to do development?
========================

 * In authoritative (main) repository:

  - Work for upcoming implementation (who: members of global-workflow-developers team)
  - Major new features or port work (who: generally code managers and/or members of global-workflow-developers team)

 * In a fork:

  - Everything and everyone else
  - How do I fork this repository? See the following GitHub documentation on forking repos: https://help.github.com/en/github/getting-started-with-github/fork-a-repo

.. _protected:

==================
Protected branches
==================

The following global-workflow branches are protected by the code management team:

* develop (HEAD)
* dev/gfs.v16 (kept aligned with current production, as well as ingests bug fixes and updates between release branches)

These protected branches require the following to accept changes:

 1. a pull request with at least 1 reviewer sign-off
 2. a code manager to perform the commit

Other authoritative repository branches may also be protected at the request of members of the global-workflow-developers team.

.. _howto: 

=============================================
How to get changes into develop (HEAD) branch
=============================================

The following steps should be followed in order to make changes to the develop branch of global-workflow. Communication with the code managers throughout the process is encouraged.

 #. Issue - Open issue to document changes. Reference this issue in commits to your branches (e.g. ``git commit -m "Issue #23 - blah changes for what-not code"``) Click `here <https://github.com/NOAA-EMC/global-workflow/issues/new/choose>`__ to open a new global-workflow issue.
 #. GitFlow - Follow `GitFlow <https://nvie.com/posts/a-successful-git-branching-model/>`_ procedures for development (branch names, forking vs branching, etc.). Read more `here <https://docs.google.com/document/d/1H5McooP-ZmDIOhcy4zJwdFVk3DyjbJt_Nyqj4QGBRBU/edit?usp=sharing>`__ about GitFlow at EMC.
 #. To fork or not to fork? - If not working within authoritative repository create a fork of the authoritative repository. Read more `here <https://help.github.com/en/github/getting-started-with-github/fork-a-repo>`__ about forking in GitHub.
 #. Branch - Create branch in either authoritative repository or fork of authoritative repository. See the `Where to do development? <development_>`_ section for how to determine where. Follow GitFlow conventions when creating branch.
 #. Development - Perform and test changes in branch. Document work in issue and mention issue number in commit messages to link your work to the issue. See `Commit Messages <commit-standards_>`_ section below. Depending on changes the code manager may request or perform additional pre-commit tests.
 #. Pull request - When ready to merge changes back to develop branch, the lead developer should initiate a pull request (PR) of your branch (either fork or not) into the develop branch. Read `here <https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests>`__ about pull requests in GitHub. Provide some information about the PR in the proper field, add at least one reviewer to the PR and assign the PR to a code manager.
 #. Complete - When review and testing is complete the code manager will complete the pull request and subsequent merge/commit.
 #. Cleanup - When complete the lead developer should delete the branch and close the issue. "Closing keywords" can be used in the PR to automatically close associated issues.

.. _development-tools:

=================
Development Tools
=================

Two sets of testing are available for use by developers.  The first is the capability to run continuous integration tests locally and the second are a set of comparison tools.

---------------------------
Continuous Integration (CI)
---------------------------

The global workflow comes fitted with a suite of system tests that run various types of workflow.  These tests are commonly run for pull requests before they may be merged into the develop branch.  At a minimum, developers are expected to run the CI test(s) that will be impacted by their changes on at least one platform.

The commonly run tests are written in YAML format and can be found in the ``ci/cases/pr`` directory.  The ``workflow/generate_workflows.sh`` tool is available to aid running these cases.  See the help documentation by running ``./generate_workflows.sh -h``.  The script has the capability to prepare the EXPDIR and COMROOT directories for a specified or implied suite of CI tests (see :doc:`setup` for details on these directories).  The script also has options to automatically build and run all tests for a given system (i.e. GFS or GEFS and a placeholder for SFS).  For instance, to build the workflow and run all of the GFS tests, one would execute

::

    cd workflow
    ./generate_workflows.sh -A "your_hpc_account" -b -G -c /path/to/root/directory

where:

    * ``-A`` is used to specify the HPC (slurm or PBS) account to use
    * ``-b`` indicates that the workflow should be built fresh
    * ``-G`` specifies that all of the GFS cases should be run (this also influences the build flags to use)
    * ``-c`` tells the tool to append the rocotorun commands for each experiment to your crontab

More details on how to use the tool are provided by running ``generate_workflows.sh -h``.

----------------
Comparison Tools
----------------

There are several scripts to compare output between two experiments (e.g. control and test). See scripts under ``/test`` folder and read `README` there for information on how to use them.

.. _code-standards:

==============
Code standards
==============

All scripts should be in either bash or python 3.

We have adopted the `Google style guide <https://google.github.io/styleguide/shellguide.html>`_ for shell scripts and `PEP-8 <https://peps.python.org/pep-0008/>`_ for python. Python code should additionally have docstrings following `numpy style <https://numpydoc.readthedocs.io/en/latest/format.html#docstring-standard>`_.

All new code after 2022 Sep 1 will be required to meet these standards. We will slowly be updating existing scripts to comply with the standards. We are also in the process of adding GitHub actions to automatically lint code submitted for PRs.

.. _commit-standards:

======================
Pull request standards
======================

Pull requests should follow the pre-filled template provided when you open the PR. PR titles and descriptions become the commit message when the PR is squashed and merged, so we ask that they follow best practices for commit messages:

 * Limit the subject line (PR title) to 50 characters
 * Capitalize the subject line
 * Do not end the subject line with a period
 * Use the `imperative mood <https://en.wikipedia.org/wiki/Imperative_mood>`_ in the subject line
 * Use the body to explain what and why vs. how
 * The final line of the commit message should include tags to relevant issues (e.g. ``Refs: #217, #300``)

This list is a modified version of the one provided at https://chris.beams.io/posts/git-commit/ with a couple removed that are not relevant to GitHub PRs. That source also provides the motivation for making sure we have good commit messages.

Here is the example commit message from the article linked above; it includes descriptions of what would be in each part of the commit message for guidance:

::
 
   Summarize changes in around 50 characters or less

   More detailed explanatory text, if necessary. Wrap it to about 72
   characters or so. In some contexts, the first line is treated as the
   subject of the commit and the rest of the text as the body. The
   blank line separating the summary from the body is critical (unless
   you omit the body entirely); various tools like `log`, `shortlog`
   and `rebase` can get confused if you run the two together.

   Explain the problem that this commit is solving. Focus on why you
   are making this change as opposed to how (the code explains that).
   Are there side effects or other unintuitive consequences of this
   change? Here's the place to explain them.

   Further paragraphs come after blank lines.

    - Bullet points are okay, too

    - Typically a hyphen or asterisk is used for the bullet, preceded
      by a single space, with blank lines in between, but conventions
      vary here

   If you use an issue tracker, put references to them at the bottom,
   like this:

      Resolves: #123
      See also: #456, #789

A detailed commit message is very useful for documenting changes.

.. _sync:

==================================================
How to sync fork with the authoritative repository
==================================================

As development in the main authoritative repository moves forward you will need to sync your fork branches to stay up-to-date. Below is an example of how to sync your fork copy of a branch with the authoritative repository copy. The branch name for the example will be "feature/new_thing". Click `here <https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests/syncing-a-fork>`__ for documentation on syncing forks.

1. Clone your fork and checkout branch that needs syncing:

::

   git clone https://github.com/JoeSchmo-NOAA/global-workflow.git ./fork
   cd fork
   git checkout feature/my_new_thing

2. Add upstream info to your clone so it knows where to merge from. The term "upstream" refers to the authoritative repository from which the fork was created.

::

   git remote add upstream https://github.com/NOAA-EMC/global-workflow.git

3. Fetch upstream information into clone:

::

   git fetch upstream

Later on you can update your fork remote information by doing the following command:

::

   git remote update

4. Merge upstream ``feature/other_new_thing`` into your branch:

::

   git merge upstream/feature/other_new_thing

5. Resolve any conflicts and perform any needed "add"s or "commit"s for conflict resolution. 

6. Push the merged copy back up to your fork (origin):

::

   git push origin feature/my_new_thing

Done!

Moving forward you'll want to perform the "remote update" command regularly to update the metadata for the remote/upstream repository in your fork (e.g. pull in metadata for branches made in auth repo after you forked it).

::

   git remote update

