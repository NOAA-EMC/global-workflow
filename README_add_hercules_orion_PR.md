# Description

This PR adds CI/CD pipeline support for the Hercules and Orion HPC platforms to the Global Workflow.

The implementation adds platform-specific configuration files for both Hercules and Orion, updates the GitLab CI pipeline configurations to include test matrices for these platforms, and enhances the CI infrastructure to support GitLab runner deployment on these systems. This follows the existing patterns established for Hera and other supported platforms.

Key changes include:
- Added `dev/ci/platforms/config.hercules` and `dev/ci/platforms/config.orion` with platform-specific paths and settings
- Extended `dev/ci/gitlab-ci-hosts.yml` to include test case matrices for both platforms
- Updated GitLab CI workflow files to support the new platforms
- Enhanced CI scripts to handle the additional platforms

This expansion allows developers to test workflow changes on a broader range of NOAA HPC systems, improving confidence in cross-platform compatibility and reducing the risk of platform-specific issues in production deployments.

Resolves #3936

# Type of change
- [ ] Bug fix (fixes something broken)
- [x] New feature (adds functionality)
- [x] Maintenance (code refactor, clean-up, new CI test, etc.)

# Change characteristics
- Is this a breaking change (a change in existing functionality)? NO
- Does this change require a documentation update? YES
- Does this change require an update to any of the following submodules? NO
 
# How has this been tested?

CI Infrastructure Testing:
- Validated CI script functionality on target platforms (ran branch directly on GitLab)
- Confirmed proper job routing and runner assignment and CI label updates on GitHub PR

Platform Validation:
- Hercules: Verified CI environment setup and basic workflow functionality
- Orion: Confirmed platform detection and CI job execution capabilities
- Cross-platform: Ensured existing CI functionality on Hera remains unchanged

# Checklist
- [x] My code follows the style guidelines of this project
- [x] I have commented my code, particularly in hard-to-understand areas
- [x] I have documented my code, including function, input, and output descriptions
- [x] My changes generate no new warnings
- [x] New and existing tests pass with my changes
- [ ] This change is covered by an existing CI test or a new one has been added