<!--
  *** PLEASE READ ***

  Any PRs not following this template will be closed.

  Please delete all these comments before submitting the PR.

  Please use a short (<60 char), descriptive title for the PR title above. It should complete the sentence "If merged, this PR will _____". Capitalize the first word and do not end with a period.

  No content should appear above the "Description" header.

  If this PR is not merge-ready (e.g. it depends on other PRs not yet merged), please mark it as draft until it is ready.

  PRs should meet these guidelines:
  - Each PR should address ONE topic and have an associated issue.
  - No hard-coded paths or personal directories.
  - No temporary or backup files should be committed (including logs).
  - Any code that you disabled by being commented out should be removed or reenabled.
-->
# Description
<!-- This description will become the commit message for the PR. -->
<!--
  Solely pointing to an issue is not an adequate description!

  Please use this format for your description:

  Describe your changes. Focus on the *what* and *why*. The *how* will be evident from the changes. In particular, be sure to note any interface changes, such as command line syntax, that will need to be communicated to users.

  At the end of your description, please be sure to add the issue this PR solves using the word "Resolves". If there are any issues that are related but not yet resolved (including in other repos), you may use "Refs".

  Resolves #1234
  Refs #4321
  Refs NOAA-EMC/repo#5678
-->
<!-- For more on writing good commit messages, see https://cbea.ms/git-commit/ -->

# Type of change
- [ ] Bug fix (fixes something broken)
- [ ] New feature (adds functionality)
- [ ] Maintenance (code refactor, clean-up, new CI test, etc.)

# Change characteristics
<!-- Choose YES or NO from each of the following and delete the other -->
- Is this a breaking change (a change in existing functionality)? YES/NO
- Does this change require a documentation update? YES/NO
- Does this change require an update to any of the following submodules? YES/NO (If YES, please add a link to any PRs that are pending.)
  - [ ] EMC verif-global <!-- NOAA-EMC/EMC_verif-global#1234 -->
  - [ ] GDAS <!-- NOAA-EMC/GDASApp#1234 -->
  - [ ] GFS-utils <!-- NOAA-EMC/gfs-utils#1234 -->
  - [ ] GSI <!-- NOAA-EMC/GSI#1234 -->
  - [ ] GSI-monitor <!-- NOAA-EMC/GSI-Monitor#1234 -->
  - [ ] GSI-utils <!-- NOAA-EMC/GSI-Utils#1234 -->
  - [ ] UFS-utils <!-- ufs-community/UFS_UTILS#1234 -->
  - [ ] UFS-weather-model <!-- ufs-community/ufs-weather-model#1234 -->
  - [ ] wxflow <!-- NOAA-EMC/wxflow#1234 -->

# How has this been tested?
<!-- Please list any test you conducted, including the machine.

Example:
- Clone and build on WCOSS
- Cycled test on Orion
- Forecast-only on Hera
-->

# Checklist
- [ ] Any dependent changes have been merged and published
- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have documented my code, including function, input, and output descriptions
- [ ] My changes generate no new warnings
- [ ] New and existing tests pass with my changes
- [ ] This change is covered by an existing CI test or a new one has been added
- [ ] Any new scripts have been added to the .github/CODEOWNERS file with owners
- [ ] I have made corresponding changes to the system documentation if necessary
