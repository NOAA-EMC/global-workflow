---
name: Production Update
about: Use this template for operational production updates
title:
labels: production update
assignees:
  - WalterKolczynski-NOAA

---

**Description**
<!-- Provide a consise description of the production update. -->
<!-- Include related issues in component repositories. -->
<!-- Include expected new version # (e.g. GFS.v#.#.#) -->

**Workflow Changes**
<!-- What are the anticipated changes coming into the workflow? -->
<!-- Include version variables, checkout tags, config/parm, etc. -->

**Tasks**
- [ ] Create release branch
- [ ] Make workflow changes for upgrade in release branch (add additional checklist items as needed)
- [ ] Create release notes
- [ ] Cut hand-off tag for CDF
- [ ] Submit CDF to NCO
- [ ] Implementation into operations complete
- [ ] Merge release branch into operational branch
- [ ] Cut version tag from operational branch
- [ ] Release new version tag
- [ ] Announce to users
- [ ] Update Read-The-Docs operations status version in develop
