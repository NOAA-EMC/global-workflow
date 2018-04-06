CDDM Workshop Report {#cddm-report}
====================

\warning <b>This is document is retained for historical record; much
information here is out-of-date.</b> This is the report from a
workshop, as described below.  The contents are copied verbatim.  More
recent information can be found in the \ref documentation
"Documentation" page.

\date CDDM workshop was held on September 1-2, 2016

&nbsp;

------------------------------------------------------------------------

Purpose
=======

\note This document requires a management decision about whether it
will be maintained or its contents organized elsewhere. This is one of
the actions identified in the CDDM workshop held on Sept. 1-2, 2016
(“Evaluation and dissemination plan for standards and policies”). It
is understood that its content may be require changes.

This document is intended to describe the goals, nature, and strategy
for code, data, and documentation management of NEMS modeling
applications and suites.

Overall Goals
-------------

* Ensure that NCEP operational requirements are met.

* Maintain all components used in NEMS-based suites as a set of
  community modeling applications that are part of a coordinated,
  unified modeling system.

* Foster community involvement, experimentation, and model improvement
  to accelerate R2X.

* Promote accessible, documented, robust, and portable code. 

Stakeholders and Contributors
-----------------------------

* EMC modeling branch (Vijay Tallapragada)
* NCO
* ESMF and CoG team (Cecelia DeLuca, Gerhard Theurich)
* Developmental Testbed Center (Ligia Bernardet, Laurie Carson)
* HIWPP (Bonny Strong)
* Developers from NOAA Laboratories (GSD, PSD, HRD)
* CESM (Mariana Vertenstein)
* COLA (Jim Kinter, Larry Marx)
* VLab team (Ken Sperow)

Others involved in NOAA model development are welcome to comment and contribute.

Terminology and Background
--------------------------

\note This section was reviewed and approved by Hendrik Tolman. It
contains terminology that will need to be updated for conformance with
the system architecture document glossary.

The NOAA Environmental Modeling System (NEMS) is infrastructure that
underlies models used for predictions of the behavior of Earth's
environment at a range of time scales. NEMS development is centered at
the NOAA Environmental Modeling Center (EMC). Examples of other
coupled modeling systems are the
[Community Earth System Model (CESM)] (http://www2.cesm.ucar.edu/)
and the 
[Met Office Unified Model] (http://www.metoffice.gov.uk/research/modelling-systems/unified-model).

NEMS includes model coupling and utility infrastructure and is
associated with a collection of model components representing major
Earth system domains and processes.  External model components have a
primary development repository that is not at EMC. In general, model
components are coupled through the NEMS mediator (in other modeling
systems this is often called the "coupler").

NEMS can be assembled into a number of different modeling
applications. Modeling applications are associated with a purpose,
like medium-range forecasting; a set of model components; and a set of
parameters that represent a range of supported options, including
grids and resolutions. Different NEMS modeling applications can have
different types and numbers of model components. Also, the same
physical domain may be represented by different model components in
different modeling applications. For example, in some NEMS modeling
applications the ocean component may be HYCOM and in others it may be
MOM5.

The Unified Global Coupled System (UGCS) is a set of NEMS applications
that share model components and can operate across weather,
sub-seasonal, and seasonal scales. UGCS-Seasonal is the first fully
coupled application to be developed with NEMS.

NEMS is built using the 
[Earth System Modeling Framework (ESMF)] (http://www.earthsystemmodeling.org/)
infrastructure software. ESMF provides utilities like generation of
interpolation weights and time-related classes, and also wrappers that
create a standard component calling interface. This enables model
components developed at different sites to be coupled more easily.

The 
[National Unified Operational Prediction Capability (NUOPC)] (https://earthsystemcog.org/projects/nuopc/)
Layer
adds additional rules about how ESMF models interact in order to
increase their technical interoperability. The NUOPC Layer addresses
aspects such as the level of build dependencies, standardization of
initialization phases, and standard names for exchange fields. NEMS is
an example of a modeling system built using the NUOPC Layer
architecture. In order to interface with the NUOPC Layer model
component developers write wrappers or “caps” for the component. The
caps contain translations of native data structures (e.g. grids, field
data, time quantities), by reference or copy, into ESMF data
structures.

The NEMS-based atmospheric models have two main elements: dynamic core
and physical parameterizations. The Physics Interoperability Layer is
a wrapper used to call the physical parameterizations. The physical
parameterizations for the 
[Next-Generation Global Prediction System (NGGPS)] (http://www.nws.noaa.gov/ost/nggps/)
will be contained in the Common Community Physics Package, a
set of parameterizations modularized by physics type (radiation,
microphysics, etc.).

A list of NEMS modeling applications is here:

* [VLAB NEMS Modeling Applications Page] (https://vlab.ncep.noaa.gov/group/guest/welcome?p_p_id=101&p_p_lifecycle=0&p_p_state=maximized&p_p_mode=view&_101_struts_action=%2Fasset_publisher%2Fview_content&_101_returnToFullPageURL=%2Fgroup%2Fguest%2Fwelcome&_101_assetEntryId=2316208&_101_type=content&_101_groupId=95585&_101_urlTitle=nems-applications&_101_redirect=https%3A%2F%2Fvlab.ncep.noaa.gov%2Fgroup%2Fguest%2Fwelcome%3Fp_p_id%3D3%26p_p_lifecycle%3D0%26p_p_state%3Dmaximized%26p_p_mode%3Dview%26_3_groupId%3D0%26_3_keywords%3Dnems%2Bapplication%26_3_struts_action%3D%252Fsearch%252Fsearch%26_3_redirect%3D%252Fgroup%252Fguest%252Fwelcome&inheritRedirect=true)

Workflow components refer to all packages that are used to to produce
an end-to-end run, encompassing not only the modeling application
itself, but also the broader category of software that implement
functions such as pre-processing, data ingest, and post-processing.
Each workflow component will include source code, configuration
settings, and workflow controls for the component.  For example, in
the Hurricane Weather Research and Forecast (HWRF) suite, a workflow
component is the Gridpoint Statistical Interpolation (GSI) data
assimilation, which is executed in different configurations depending
on storm characteristics, available datasets, etc.

A modeling suite is a collection of workflow components assembled for
a particular purpose.  Each operational modeling suite is such a
collection. For example, the North American Mesoscale (NAM) suite
includes a data assimilation workflow component, a NEMS modeling
application, etc.  The Short-Range Ensemble Forecast (SREF) suite
includes a similar NEMS modeling application (the Non-Hydrostatic
Multiscale Model on the B-grid, no ocean) but has additional workflow
components for ensemble processing.

For illustration purposes, Table 1 shows a simplified list of workflow
components for two existing suites, the NAM and the HWRF, along with a
future suite (NGGPS Numerical Weather Prediction - NWP). The table
shows that NEMS-based suites, such as NAM and NGGPS NWP, use several
workflow components which are common to NEMS and/or non-NEMS
suites. The following workflow components are highlighted in the
table: GSI, WRF Preprocessing System (WPS), NEMS Preprocessing System
(NPS), and Unified Post Processor (UPP).

#### Table 1: Workflow components used in a suite (simplified)

| Suite     | GSI    | WPS   | NPS   | NEMS   | UPP    |
| :-------: | :----: | :---: | :---: | :----: | :----: |
| NAM       | YES    |       | YES   | YES    | YES    |
| NGGPS NWP | YES    |       |       | YES    | YES    |
| HWRF      | YES    | YES   |       |        | YES    |

Modeling Applications and Suites
================================

Parts of a NEMS-based Modeling Application
------------------------------------------

* NEMS coupling software
* NEMS model components for atmosphere, land, ocean, ice, ionosphere etc.
* NEMS build system and configuration scripts
* NUOPC caps for each component
* Physics Interoperability Layer 
* Common Community Physics Package

Parts of a Modeling Suite
-------------------------

* Modeling application(s)
* Build system and makefiles for all workflow components
* Pre-processors and data assimilation codes (NPS, GSI etc.)
* Post-processors, product generators, verification, etc.
* Running scripts, configuration files, suite automation files

Recent Changes at NCEP
======================

\note This section was reviewed and approved by Hendrik Tolman.

There are three ways in which new code development at NCEP is
significantly different than what has come before. These changes have
ramifications for repository and code management.

First, there is increasing emphasis on community modeling. Numerous
model components developed external to NCEP through community-based
approaches are being incorporated into NCEP operations. Further, the
model components (such as WAVEWATCH III) and modeling applications
that are developed at NCEP centers increasingly engage the broader
community throughout the development process.


Second, there are many more model components in NCEP modeling
applications than before, with more than eight types of types of
separable model components (e.g. ocean) anticipated in NEMS, and
multiple options for each type (e.g. MOM5, HYCOM, data and stub
versions of the ocean component). This is a fundamental change that
affects code management in a number of ways. Repository plans, testing
and experimentation, and other aspects of code handling require
special consideration. We include in the contributors to this document
experts in the management, community support, and evaluation of
many-component modeling applications.

Third, there is a desire to move to a unified modeling system that can
operate more seamlessly across temporal and spatial scales. The common
NEMS framework will be used to compose multiple modeling applications
that can be used for different purposes. This means that where model
components are common to modeling applications, and where workflow
components are common to suites, changes need to be synchronized
across the modeling applications and suites. For this reason, the
repository and code management strategies for NEMS-based modeling
applications and suites need to be considered holistically.

Modes of Use and Implications
=============================

The expectations about the modes of use for NEMS modeling applications
and suites, including both operations and research, will inform
choices about the repository and code management strategy.

Operations
----------

EMC builds modeling applications to run within production suites.  The
goal is to create end-to-end systems that includes observational
processing, data assimilation, model forecasts, post processing,
verification, and assessment.

Operational Workflows for Community Testing
-------------------------------------------

In order to support community engagement in the testing of modeling
applications in operational suites, it is critical for the broader
community to be able to run the production suites and evaluate changes
according to skill and other prescribed metrics. This mode of
community evaluation offers the most direct path from research to
operations.

For the UGCS-Seasonal application, the expectation is that EMC will
collaborate with the community on the design of a test harness, where
EMC will make the control runs. Various partners and collaborators
that are working on the upgrades to the physics, etc. will experiment
with this basic version. All results will be posted to the EMC
server. EMC will also work with the community on developing
verification methods, such as a climate scorecard. As new
components/upgrades take place, EMC will upgrade the repository trunk
of the application accordingly.

Model Development, Research, and Testing Workflows
--------------------------------------------------

It will be useful from both a scientific and practical perspective for
model developers and researchers to be able to run NEMS modeling
applications and suites in ways that provide more flexibility than the
operational workflows. For example, substantial research is needed to
improve understanding of the interactions and feedbacks between system
components. In addition, the option to configure and run a modeling
suite in a research mode, which may differ from the operational
modeling suites, is important for the transition of research
capabilities to operational systems.  Providing easy access to the
NEMS mediator in an environment conducive to experimentation, such as
NCAR’s CIME, will promote use of the ESMF/NUOPC interfaces and support
research to operations across a broader community.

Intervening at the interface between component models makes it
possible to control feedbacks between component models – a powerful
paradigm for evaluating the role of those feedbacks – and it makes it
possible to artificially alter the interactions among components to
determine sensitivity and predictability. Similarly, alternative
configurations such as the interactive ensemble (multiple simultaneous
instantiations of one or more component models),
super-parameterization (sub-cycled, sub-grid-scale representation of
processes), and others, should be supported with the operational
component models and the NEMS mediator.

To support such controlled experimentation, it is helpful if each
model component can be run in a number of modes: fully prognostic
mode, as a data model component that reads in observational or model
generated data, or as a stub to satisfy interface requirements. This
flexible design permits feedbacks between components to be selectively
turned off, thereby enabling researchers to address a broad range of
scientific questions and also to determine what aspects and components
of the Earth system must be dynamic and fully coupled in order to
satisfy operational forecasting requirements.

In order to be used in a community context, it is important to enable
users to easily create custom configurations for their experiments,
including running numerous out of the box experiments with various
levels of model feedbacks enabled at a number of model resolutions and
configurations.

Non-scientific (e.g. data, stub) versions of components can also be
useful for introducing new components. The technical integration of
existing prognostic components into the coupled system often happens
in three stages. In the first stage, feedbacks are turned off and
validation is carried out in a “stand-alone” configuration. In the
second stage, a few selected feedbacks are turned on. In the final
stage, full coupling with all prognostic components is implemented and
evaluated. This sort of independent development of components followed
by an integration stage may not always be ideal for scientific
development of coupled systems, but it is an approach used frequently
nonetheless.

For any complex software system, and especially for a coupled modeling
system, automated system and unit tests to verify infrastructure
functionality on a variety of machines and compilers are
essential. These tests satisfy a number of high-level software
requirements, such as the ability to give bit-for-bit identical
answers after restarting from a checkpoint, providing answers
independent of processor and/or OpenMP thread count, and ensuring that
code refactorings do not change answers unless they are expected to do
so.  It is useful to be able to run the entire system test suite with
a single command.

Operating Principles
====================

\note Management review of this section is one of the items identified as an action in the CDDM workshop on Sept 1-2, 2016 (“Operating Principles”).

The following are a set of operating principles (OP later in the
document) that apply across multiple categories of requirements.

* Make decisions with the end goal of producing the best possible forecasts, based on evidence rather than assertion.

* Operate efficiently. Avoid redundancy, and favor automation over manual processes.

* Promote transparency. To the extent possible, make code, data, documentation, requirements, policies, and plans openly available and clear.

* Encourage community ownership and participation. Establish processes that enable partners to engage in decision-making.

* Ensure that organizational structures support clear responsibilities and accountability.

Roles needed
============

\note Completion and review of this section is one of the items
identified as an action following the CDDM Sept. 1-2, 2016 workshop
(“Organizational Description”).

* **Component code managers** --- These are people internal to EMC
  who are responsible for the technical aspects of the component code
  and its integration into the unified system.

* **External component points of contact** --- These are people
  associated with external component development who can serve as a
  point of contact for coordination and synchronization with EMC.

* **Application leads** --- People who are responsible for the
  scientific and technical development and delivery of a particular
  application.

The current status at EMC is that there is a technical team with the following members:

* Mark Iredell: manager
* Sam Trahan: NEMS code manager
* Valbona Kunkel: Documentation
* Hang Lei: Physics interface
* Terry McGuinness: workflow
* Kate Howard: scripts

\note It would be useful to describe the responsibilities of this team.

Decision Making about Code, Data, and Documentation Processes and Policies
--------------------------------------------------------------------------

\note Following the Sept 1-2, 2016 CDDM workshop, NCEP/EMC agreed to
propose a process for reviewing software policies and processes. This
has not been delivered.

What this might look like:

1. NCEP/EMC software lead decides whether or not the proposal merits review.

2. NCEP/EMC software lead and the proposer(s) decide on a team of reviewers.

3. One or more calls or meetings are scheduled to discuss and refine the proposal.

4. A decision is made following a clear protocol.

5. The proposal is included in a logical place in a developer guidance
document, announced if appropriate, and posted to a development
website.

6. Actions in the proposal are implemented.

Requirements Format and and Collection
======================================

\note This section requires management review and
concurrence. Currently, although EMC participated in formulating these
guidelines, they are not following them. The convention should be
modified or replaced with the format and procedures NCEP/EMC intends
to follow for collecting and disseminating requirements.

Documented requirements and expectations, collected from appropriate
stakeholders, serve as a foundation for formulating strategies and
making decisions.

This document introduces a convention for requirements collection and
presentation. Each entry includes:

* **Id** --- Requirement short identifier and number, e.g. SM1 (Software Management 1)

* **Type** --- Current classifications include goal (general guidance
  or direction), expectation (exp; an assumption), requirement (req; a
  necessity), and recommendation (rec; a desire or suggestion).

* **Item** --- Description of the entry.

* **Reason** --- Rationale or motivation for the entry.

* **Source** --- Person or group that originated the entry.

* **Status** --- Implementation status or timeline associated with the entry.

A simple requirements collection process typically involves
identifying a scope, coordinator or moderator, a set of stakeholders,
and assembling the requirements through a joint discussion. A new set
of requirements can be expected to evolve and become a basis for
action through ongoing vetting, periodic updates, and being referenced
by those developing plans and products.

Software Management
-------------------

### Goals, Requirements and Expectations

\note This requirements section requires management review and concurrence. 

In the chart below, OP stands for Operating Principles.


### SM1: Minimize Software Repositories Per Component

Minimize the number of software repositories required per
component. Best practices in software configuration management
recommend using a shared common repository for development where
feasible.  New development can be managed using branches (or forks or
equivalent) with the understanding of a common authoritative source
(master/trunk) and a procedure for integrating new development into
the source repository.  This approach utilizes the strengths of
configuration management tools, while minimizing the work and risk
involved in maintaining duplicate repositories.  

**Type**: goal

**Source**: GMTB

**Status**: No policy in place

**Reason**: OP, avoid duplication


### SM2: Source Code Available

All source code for operational modeling applications and suites must
be available on EMC computers.  Availability in case there are
problems.

**Type**: Requirement

**Source**: NCO

**Status** Implemented

**Reason** Availability in case of problems


### SM3: Accessible Repositories

NEMS modeling applications and suites are expected to utilize multiple
development repositories, including repositories managed externally
and by EMC. It must be possible for developers (NOAA and non-NOAA) to
access codes in the development repositories, workspaces, and
trackers, with specific permission levels (read, read/write).

**Type**: EXP

**Source**: OAS

**Status**: Not fully implemented, some key developers do not have access to workspaces and trackers.

**Reason**: Access is needed for development.


### SM3a: Current Operational Code is Available

It is essential that the currently operational code can be checked
out. An exact mirror repository should be maintained that always has
the latest operational code.  

**Type**: Requirement

**Source**: COLA/Kinter

**Status**: No policy in place

**Reason**: This is needed to streamline the transition from research
to operations


### SM4: EMC Repository Developer Responsibilities

The following apply to developers working in an EMC repository:
* Developer maintains own branch of trunk.
* Commit work back to branch from working copy frequently.
* Keep up to date with trunk.
* Use test harness for regression and suite testing prior to commits.
* Use ticket system as required.

**Type**: EXP

**Source**: EMC/Tollman

**Status**:  Unable to implement because of lack of access to computers and trackers..

**Reason**: Follow good software practices for development.


### SM5: External Component Development Responsibilities

The following apply to developers working with external components:

* If procedures used in the external, authoritative repository are
  compatible with NEMS development, the procedures specific to that
  component will be followed.

* If the external, authoritative repository cannot support component
  development for NEMS, a development repository for EMC use will be
  established and the procedures established for EMC repositories
  followed.

**Status**: Implemented.

**Reason**: Balance between low process overhead and control over processes.

**Type**: Requirement

**Source**: OAS


### SM6: Components have Identified Leaders

There is an identified EMC component lead for all model and suite
components. There is an external component lead identified for
external components.

**Type**: Requirement

**Source**: NCO

**Status**: Implemented

**Reason**: OP, accountability.


### SM7: Identified Leaders for NEMS Documentation

There are identified leads for the overall NEMS system development at EMC.

**Type**: Requirement

**Source**: OAS

**Status**: Not implemented.

**Reason**: OP, accountability.


### SM8: Synchronization Path to External Component Repositories

Code changes to external components taking place in their native
repositories must have a path for synchronization with changes made to
these components at EMC, and vice versa. It is up to the EMC component
lead to synchronize changes between development and operational
repositories, in coordination with the external component lead. If
necessary, users can download a tar file of a code release and return
changes via tar file.

**Type**: Requirement

**Source**: EMC/Tollman

**Status**: No controls in place.

**Reason**: Need for synchronization of development to maintain
  coherence in community-based unified system.


### SM9: Synchronization Path of Component Code Between Applications

Changes in components and infrastructure made for any given NEMS
modeling application must have a process for synchronization with
versions of these codes used by other NEMS applications.

**Type**: Requirement

**Source**: OAS

**Status**: No policy in place..

**Reason**: Need for synchronization of development to maintain
  coherence in community-based unified system.


### SM10: Standardized Testing and Implementation System

There is standard regression, suite, operations testing for
respectively software, science, and implementation.

**Type**: Requirement

**Source**: OAS

**Status**: Key processes not implemented.

**Reason**: Critical part of software process.


### SM11: Repository Strategy Supporting Many Components and Applications

The repository strategy must support testing and experimentation with many-component modeling applications and suites.

**Type**: Requirement

**Source**: OAS

**Status**: Repository strategy is not fully defined.

**Reason**: Needed to manage development in multi-component, multi-application NEMS system.


### SM12: Component Versions Associated with Application Versions

It must be possible to easily assemble a version of a particular
modeling application, with versioned constituent components.

**Type**: Requirement

**Source**: OAS

**Status**: Implemented

**Reason**: Needed to manage development in multi-component,
   multi-application NEMS system.


### SM13: Availability of Stub, Data, and Active Model Components

Model components must offer active, stub, and data versions for
testing and experimentation.

**Type**: Requirement

**Source**: OAS, EMC/Grumbine

**Status**: Data versions are not available.

**Reason**: Needed for testing and development.


Tools and Options: Collaboration Environments and Workflow Software
===================================================================

NWP Information Technology Environment (NITE)
---------------------------------------------

* [NWP Information Technology Environment (NITE)] (http://www.dtcenter.org/eval/NITE/)

NITE is the collective name of various elements needed to support a
computational infrastructure for simplified configuration and
execution of experiments with NCEP modeling suites. These elements
are: data management, code management, suite configuration, scripts,
workflow management system, documentation/training, and database of
experiment metadata. The Developmental Testbed Center (DTC) has
created a 
[preliminary NITE design] (http://www.dtcenter.org/eval/NITE/NITE-report-AOP2014.pdf),
with a focus of making code, datasets, and running infrastructure
easily available to NCEP and its external collaborators. EMC
leadership has indicated that this design will be considered for
building the NGGPS infrastructure. Since code management is one of the
NITE elements, the recommendations contained in this document can be
considered as an integral part of NITE.

Virtual Lab (VLab)
------------------

The VLab is a service and IT framework, enabling NOAA employees and
their partners to share ideas, collaborate, engage in software
development, and conduct applied research.  The VLab is comprised of
two main components:

* Virtual Lab Collaboration Services (VLCS)
* Virtual Lab Development Services (VLDS)

The VLCS is built upon a feature rich open source Java portal
framework called Liferay. The VLCS enables users to share and
contribute science and participate in dynamic virtual communities. The
VLCS provides powerful tools for collaborating such as document
libraries, wikis, message boards, blogs, announcements, dynamic forms
with workflow, and a content management system (CMS). All tools within
the VLCS are searchable and centralized.  The VLCS provides a full
featured administrative console and robust roles and permissions
framework.

The VLDS provides web based services to help manage projects via issue
tracking, source control sharing, code review, and continuous
integration. Redmine is being used for issue tracking for all projects
and access control for simple projects’ source code repositories
within the VLab.  Subversion and Git, a distributed configuration
management (CM) system, are being used for source code control within
the VLab and integrate with Redmine.  Gerrit provides web based code
review and project management tools to Git based projects in the VLab.
Jenkins provides a web based continuous integration tool within the
VLab.  Projects can use Jenkins to automate the building of their
code, execute unit tests, and perform custom checks anytime code is
modified.

Earth System CoG
----------------

The Earth System CoG is an open, multi-agency collaboration
environment that supports project hosting and linked project networks
for software development and data dissemination. Project web pages are
wikis that are easy to create and navigate. The environment is set up
as a federation, so that federal centers and universities can install
their own local CoG, but also access and link to projects across the
full set of CoGs. CoG provides views of information across entire
networks (for example, all people or all repository links). CoG also
offers extensive data archival and search services through an
integrated interface to the 
[Earth System Grid Federation (ESGF)] (http://esgf.llnl.gov/),
an international data distribution network. A large network of CoG
site installations around the world is being used to support data
dissemination of model output in support of the Intergovernmental
Panel on Climate Change (IPCC) model intercomparison projects.

A CoG is installed at NOAA ESRL and is being used to host the
[workspace for NEMS model coupling] (http://cog-esgf.esrl.noaa.gov/projects/couplednems/),
along with the
[ESMF site] (https://www.earthsystemcog.org/projects/esmf/), the
[NUOPC Layer site] (http://www.earthsystemcog.org/projects/nuopc/),
and workspaces for individual NEMS-based applications such
as the
[WAM-IPE space weather coupling] (http://cog-esgf.esrl.noaa.gov/projects/wam_ipe/).
There is also a CoG at GFDL, and CoG installation is underway at NCEI.

\note Update link to WAM-IPE webpage when if it is moved to repo

CIME
----

The Common Infrastructure for Modeling the Earth (CIME) is a public,
github based infrastructure repository initiated by the Community
Earth System Model (CESM) software engineering group in April
2015. CIME was developed as a response to the February 2


Tools and Options: Repository Software and Services
===================================================

In this section we describe options related to code access and
developer access (which assumes write access) for infrastructure and
scientific software and documentation.

Repository Software: Git and Subversion
---------------------------------------

[Git] (https://git-scm.com/)
and
[Subversion (SVN)] (https://subversion.apache.org/)
are both widely used version control software packages. There are
several advantages to using Git relative to Subversion and other
version control software. Git is faster, was designed to to be fully
distributed from the start, and provides a forking service which
simplifies the process for developers to submit development work back
to a master repository. In addition, the following advantages of Git
are listed at:

* https://git.wiki.kernel.org/index.php/GitSvnComparison

Several teams that support multi-component systems, including CESM,
mix Git and Subversion in order to get desirable properties from
each. Using both can also be a viable implementation strategy for
non-disruptive transitions from use of Subversion to full use of
Git. Such an approach is described in the Current Practice and
Recommended Evolution section.

Repository Software Service: Github, VLab, or elsewhere (such as NCAR or EMC servers)
-------------------------------------------------------------------------------------

Having the source code accessible is very important for EMC to meet
its mission of fostering community involvement for NWP
development. Therefore it needs to be straightforward for community
members to obtain access to the code repositories that house the
components of NEMS-based modeling applications and suites.


Most EMC NWP codes currently reside in SVN repositories accessible
through EMC servers. Access to those servers is very restricted to
non-NOAA developers. One exception is HWRF, whose code resides at NCAR
and GSD and is managed by DTC. To alleviate access restrictions to the
general community, DTC has also mirrored some parts of the EMC SVN
repositories at NCAR (for example, NEMS and GSI). However, the mirrors
are labor intensive, prone to error, and many times insufficient (when
not all branches are mirrored).

In contrast, access to code repositories served through GitHub is
straightforward. Access to VLab is likely to be easy as
well. Therefore, those alternatives are expected to foster a more
robust collaborative environment at lower cost.

In addition to repository access, both GitHub and VLab have several
tools that greatly facilitate collaborative software
development. Github provides both wikis and issue tracking that can be
utilized to track both repository specific issues or general issues
for the whole project. Github provides tools for inspecting the
history and structure of the repository. It has excellent
documentation that permits both novice users and experts to make the
optimal use of git, and provides an organizational structure that
allows for both public and private repositories on a very cost
effective basis. Finally, GitHub has both excellent uptime and
customer support.  Repository Access and Use Procedures







<table>
<tr><th>&nbsp;</th><th>EMC</th><th>CIME</th><th>VLab</th></tr>
<tr>
  <th>Contents</th><td>
        Science components, coupling infrastructure, workflow infrastructure
  </td><td>
        Infrastructure only; currently CESM and ACME coupling and workflow infrastructure and non-scientific versions of model components
  </td><td>
        Any NOAA R&D project repositories
  </td>
</tr><tr>
  <th>
        Version control software protocol
  </th><td>
        Subversion
  </td><td>
        Git 
  </td><td>
        Git or Subversion
  </td>
</tr><tr>
  <th>
        Version control software service
  </th><td>
        EMC server
  </td><td>
        Github
  </td><td>
        Subversion and Git over https or Git through Gerrit
  </td>
</tr><tr>
  <th>
        Public download?
  </th><td>
        Not for development code.
  </td><td>
        Development and release versions can be downloaded by anyone from https://github.com/CESM-Development/cime. 
  </td><td>
        Not currently.  Must have a NOAA LDAP login or VLab external partner login.
  </td>
</tr><tr>
  <th>
        Information collected from users?
  </th><td>
        NOAA computer access requires submission of fingerprints and detailed personal history for a Federal background investigation. 
  </td><td>
        No information is collected other than that required for a Github account.
  </td><td>
        None required for NOAA accounts.  NOAA External partner sponsors  must supply basic contact information of external partner (e.g., phone, email, name, and justification). 
  </td>
</tr><tr>
  <th>
        Download metrics available?
  </th><td>
        Not easily
  </td><td>
        Github tracks downloads/clones, and this is available to project administrators
  </td><td>
        Some usage statistics are available through Google Analytics
  </td>
</tr><tr>
  <th>
        Typical length of time required for read access?
  </th><td>
        2-6 months
  </td><td>
        Immediate
  </td><td>
        Same or next day
  </td>
</tr><tr>
  <th>
        Typical length of time required for write access?
  </th><td>
        At least 6 months
  <td></td>
        Less than a day to days
  </td><td>
        Same or next day
  </td>
</tr>
</table>




Procedure for Developer Access
------------------------------

### CIME

To write to CIME a user issues a pull request on github. This is documented at

* https://github.com/CESM-Development/cime/wiki/CIME-Development-Guide

Anyone can issue a pull request, regardless of if they are inside or
outside of NOAA. Currently, the gatekeepers for determining whether or
not to accept a pull request are members of the CESM Software
Engineering Group (or CSEG). However, this governance structure can be
extended depending on the collaboration. As an example, CESM and the
DOE/ACME project have created a new github repository,
https://github.com/ESMCI/cime that currently mirrors CIME, but where
the determination of what pull requests to accept are made jointly by
both CSEG and ACME software engineers. A similar CIME repository
structure can also be established with NOAA or the CIME administration
structure can be easily enhanced using github's administration and
organizational tools.

CSEG has established a workflow for using and developing CIME. This
can be found at

* https://github.com/CESM-Development/cime/wiki/CIME-Git-Workflow

Further documentation for dealing with the externals in CIME can also
be found at

* https://github.com/CESM-Development/cime/wiki/Managing-externals-included-in-CIME.

CIME is just in its infancy and as it continues to expand in the
number of collaborators that are interested in using and contributing
to it, a more formal governance structure will need to be established.

### VLab

Any user with a NOAA LDAP ID has access to the VLab.  If the user does
not have a NOAA LDAP ID a NOAA user can request a VLab account for
external partners (e.g., other government agencies, University
Partners) through VLab forms.  Project owners add users as a member of
their project within VLab’s Redmine web interface to give users access
to the underlying repository.  See the 
[VLab Wiki guidance] (https://vlab.ncep.noaa.gov/redmine/projects/vlab/wiki/Help_for_Project_OwnersManagers#Adding-Members-to-a-Project)
for more information.

### EMC Repository

Access to NOAA computers requires the following steps:
     
* Obtain and utilize a NOAA email address to communicate regarding
  NOAA issues. This requires several steps (allow 2-4 weeks of
  processing time).

* Complete NOAA’s annual IT Security Awareness Course ( ITSAC) (1 hour
  of security training).  Repeated annually.

* Complete a National Agency Check and Inquiries (NACI) background
  check. This includes fingerprinting and providing a detailed
  personal history (allow minimum of 6 months).

* Computer account application (allow a minimum of 2-4 weeks of
  processing time).

* Request access to the NEMS repository and group permissions (allow
  1-2 weeks of processing time).

* Users must login every 30 days to keep their proxy certificate up to
  date or their account will be disabled.

* Verify account information on an annual basis.

There is a slightly faster path: acquiring checkout approval for the
EMC NEMS SVN repository takes on the order of a month or two, and the
code can be checked out onto a widely accessible computer at NCAR
(yellowstone). In practice, this solution often requires people to
move files across machines in order to collaborate, and is
error-prone.  













Software Management Current Practice and Recommended Evolution
==============================================================

\note This section requires management review and concurrence. The
assembly of a plan for repository management is one of the items
identified as an action in the CDDM workshop on Sept. 1-2, 2016
(“Repository reference document”). There is also a technical action
associated with this section (“Web-based interface for account
requests”).  NEMS Repository

NEMS modeling applications are becoming more complex due to the fact
that they are now multi-component applications. The matrix view, with
components along the horizontal, and applications along the vertical,
is a useful visualization of this. It can be seen in the 
[NEMS application list] (https://vlab.ncep.noaa.gov/group/guest/welcome?p_p_id=101&p_p_lifecycle=0&p_p_state=maximized&p_p_mode=view&_101_struts_action=%2Fasset_publisher%2Fview_content&_101_returnToFullPageURL=%2Fgroup%2Fguest%2Fwelcome&_101_assetEntryId=2316208&_101_type=content&_101_groupId=95585&_101_urlTitle=nems-applications&_101_redirect=https%3A%2F%2Fvlab.ncep.noaa.gov%2Fgroup%2Fguest%2Fwelcome%3Fp_p_id%3D3%26p_p_lifecycle%3D0%26p_p_state%3Dmaximized%26p_p_mode%3Dview%26_3_groupId%3D0%26_3_keywords%3Dnems%2Bapplication%26_3_struts_action%3D%252Fsearch%252Fsearch%26_3_redirect%3D%252Fgroup%252Fguest%252Fwelcome&inheritRedirect=true)
and in also in the matrix of workflow components shown in the
Terminology and Background section of this document. These matrices
show that the same components are going to be used in several modeling
applications. In other words, there are complex software interactions
going many different ways within the matrices. The desire for a
unified system recognizes the advantage of not treating each
application line in the matrices as separate from the other. This is
also reflected in the software management requirements and
expectations. The following approach carries this awareness through to
the construction of the revision control architecture.

In the proposed revision control architecture, the applications, the
components, and the NEMS infrastructure itself are top level
directories under the EMC /projects repository. EMC refers to each of
these directories as a project. The connection to the matrix
representation is made by introducing SVN externals (here just called
links). The proposal is to recognize the components as the primary
sorting axis, and to introduce an SVN external link to a specific
revision of each component from each application. The application
group still has full control over the component sources, but there is
this recognition that the components are the building blocks. Figure 1
shows a UGCS-Seasonal revision defined as a set of links to specific
revisions of NEMS (which currently includes GSM), MOM5, and CICE.

Building a complex system that supports many components and many
applications is not something that happens without a lot of
effort. It's like a family, each member needs to grow individually,
and they need the freedom to do so, but if you want to still be
recognizable as a family, there are times for each member to put
effort into something that maybe is not directly their interest. Same
here: if the vision of a unified modeling system is going to be
realized, there must be times when changes to common pieces are
reconciled again. The more convenient and obvious we can make this
within the revision control architecture, the better. Requirement SM6,
which requires a liaison internal to EMC for each component, and a
liaison external to EMC for each component, begins to create a
structure for coordination.

It should also be realized that we are trying to set up an
evolutionary rapid development process. This is in many ways a new
approach for EMC, but really the only way that applications can be
delivered in time, while moving toward a unified modeling system. It's
probably fair to say that the era in which you could wait for a
"working system" for a specific application, and then go and modify it
for the operational need, is over. Instead the system as a whole must
be evolvable, ready to react, and to do this systematically and
responsively. On the source level, SVN with the proposed repo
architecture can serve as the necessary backbone for this development
style.

\image html repo-structure.png "Figure 1. Project Directory Structure for NEMS Components and Applications"

Proposed directory structure for the CFS version 3 project.

Component Sets (Compsets)
-------------------------

The concept of "component sets", or "compsets" for short, has been
used successfully by the
[http://www2.cesm.ucar.edu/] (CESM)
project for some time to 
[systematically label different run configurations] (http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/compsets.html).
The labels are associated with scripts that pull
together all the files and inputs needed to run the specified
configurations. This approach offers a number of benefits:

* standard runs can be set up easily and consistently

* it is a very effective way to implement regression testing across a
  system with many possible combinations of components

* it is a systematic way of managing the complexity associated with
  not just many components, but multiple (data, stub) non-prognostic
  versions of components, and facilitates controlled experimentation

Compsets are currently being used only for technical testing in NEMS,
but could also be applied to scientific model configuration and
validation. More information about compsets is in the 
\ref documentation "NEMS Users Guide".

Guidelines for Software Checkins and Updates
--------------------------------------------

The instructions in this section use the following terms to refer to the parts of an URL pointing to a software package “ABC” that is under SVN revision control:

**Complete URL:**

 * https://server.noaa.gov/path/ABC/subdir

This is the complete URL that can be used in a SVN checkout command to obtain an instance of the full source code structure of the ABC software.

**Base URL:**

 * https://server.noaa.gov/path/ABC

This is base URL of the ABC software. Following SVN conventions there
should be exactly three canonical subdirs under the base URL: trunk,
branches, tags. However, this is only a convention and there is no
enforcement mechanism.

**Trunk URL:**

 * https://server.noaa.gov/path/ABC/trunk

This is the complete URL that points to the trunk of the ABC software
(following SVN convention).

**Branch URL:**

 * https://server.noaa.gov/path/ABC/branches/name

This is the complete URL that points to the `name` branch of the ABC
software (following SVN convention).

In order to build, run, and modify a NEMS application the first step
is to check out from the appropriate "complete URL". Depending on the
circumstances, this may be a "trunk URL" or a "branch URL". Also, you
may currently run into a variety of "base URL" options. Assuming that
NEMS application ABC was set up according to the conventions outlined
in the previous section, the "trunk URL" will look like this:

* https://svnemc.ncep.noaa.gov/projects/ABC/trunk

However, currently no applications exist YET directly under the
/projects level, instead most applications are located under the NEMS
project at /projects/nems/apps. This directory was introduced as a
temporary staging area for NEMS application. For example the "trunk
URL" of the current UGCS-Seasonal application is

* https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Seasonal/trunk

It has been the plan for a while to move away from the NEMS project in
the short term for the staging area. Instead a more generic location,
e.g.  /projects/nems-X/apps, should be created.  However, this
transition has not yet happened.  In the long run, sanctified NEMS
applications should end up with a "base URL" directly under /projects.

A few remarks:

* The following instructions apply no matter what the current "base URL" of the NEMS application looks like. 

* It is assumed that the application liaison has provided you with the
  appropriate "complete URL" for the work you are planning to
  perform. This may be a "trunk URL", or a "branch URL". Either way,
  the following instructions apply.

* In order to keep the instructions general the string
  `(complete-URL)` is used to stand for the complete URL you were told
  to use.

 * The string ABC is used to stand for the name of the application.

 * The string REV is used to stand for a specific SVN revision number.

Checking out and building a specific version of the application code

    svn co -r REV (complete-URL) ABC
    cd ABC
    ./NEMS/NEMSAppBuilder

This will check out the source code into directory ABC, and build the
application. It is up to the application liaison to ensure that the
complete URL provided is appropriate. In most cases this means that
it is strictly versioned, i.e. each of the constituent components
pulled in via SVN externals are referenced with specific revision
numbers. This typically also means that you received a revision number
REV together with the complete URL.

It the case of the UGCS-Seasonal application, step 1. above will
currently check out a directory structure similar to this:

* UGCS-Seasonal
  + NEMS --(svn:externals)--> rev on EMC subversion trunk
  +  GSM  --(svn:externals)--> rev on EMC subversion trunk
  +  NMM  --(svn:externals)--> rev on EMC subversion trunk
  +  CICE     --(svn:externals)--> rev on GitHub
  +  CICE_CAP --(svn:externals)--> rev on GitHub
  +  MOM5     --(svn:externals)--> rev on GitHub
  +  MOM5_CAP --(svn:externals)--> rev on GitHub


Note that if you checked out with a specific revision REV, it is
generally unsafe to do an `svn update` from with the UGCS-Seasonal
directory. The update will move the code to the current head of the
checked out complete URL. This also means it will update the SVN
external links to whatever revision the head version is pointing to.

### Checking out, modifying, committing application code

While it is important to stay with the specific revision of the
application code (including the constituent components) when
validating specific version of the software, a head version is
required when doing development work. The reason is that code
modifications can only be committed back to the head, not to a
specific revision.

There are two scenarios to cover: 1) the current application head is
not strictly versioned, or 2) the current application head is strictly
versioned.

1. The convention for the UGCS-Seasonal application is to not strictly
   version the head of the application trunk during development
   phases. This means that under most circumstances if you update to
   the head of the trunk URL of UGCS-Seasonal, all of the SVN
   externals will be pointing to the constituent's head, without
   specific revision number. This allows development on the head for
   each component. When ready, and tested, code changes can easily be
   committed by `svn commit` from within each affected constituent
   component subdir. It is also safe to execute `svn update` from
   within constituent subdirectories.

   Only snapshot revisions of the UGCS-Seasonal trunk are strictly
   versioned, however, they are followed immediately by a commit that
   removes strict versioning again from the head. This allows us to
   provide strictly versioned revisions, while keeping the development
   process simple.

2. Applications where the head revision is strictly versioned require
   more attention when modifying code and commit code
   changes. Executing “svn update” directly under a strictly versioned
   application head will always bring the constituent components back
   to the revision specified by the SVN externals!

   The constituent components can still be moved (individually) to
   their head revision by changing into the respective subdirectly and
   executing `svn update` from within. This then allows local code
   modifications to be committed. However, an update on the
   application level will revert the subdirectory back to the fixed
   revision according to the SVN external definition. This can be very
   confusing.

### Proposed Processes for Coordinating Software Across NEMS

Link to proposal for code, documentation, and script changes from Sam Trahan. 

It is assumed that external components that do not support EMC or
community access have a local, mirrored repository at EMC.

A proposed central rule is that before (or after) a major release or
delivery, an application should merge its changes back to all
constituent component project repositories at EMC, including NEMS
itself. This is understood to be a minimum; it may be more
frequent. Additionally, after a release or delivery, a strongly
versioned tag should be created that includes specific revisions for
all constituent components.

All components have EMC points of contact and native points of
contact. The points of contact should define rules for creating
branches for their component. They should also work out a plan and
process for migrating changes back to the native component repository,
if it is outside of EMC.

Questions about process for each component should be directed to both the EMC point of contact and the native point of contact.

Coding standards
================

\note The proposed standards here need to be reconciled with
Environmental Equivalence 2. The section should be updated to reflect
the standards that NCEP/EMC intends to follow.

The following table specifies coding requirements and recommendations
for a parameterization to be included in CCPP. The intent is to
promote readability, robustness, and portability without being too
onerous. The Kalnay rules and work by the NUOPC Physics
Interoperability Team and EMC personnel had a major impact in creating
this list. The GSM coding standards described at

* https://svnemc.ncep.noaa.gov/trac/gsm/wiki/GSM%20code%20standards

were taken into account and incorporated as applicable. Unless
specified otherwise, the Fortran programming language is assumed.

\note Move the GSM coding standards to a public area


### CS1: Fortran Implicit None

All fortran modules and subroutines will contain `implicit none`

**Type**: Requirement

**Source**: GMTB, GSM

**Reason**:

* Assists in writing bug-free code.

* Understanding implicit type rules is difficult and arcane.

* Understanding where a variable comes from (local, input argument
  list, module) is more difficult with implicit typing


### CS2: Fortran \c Intent Attribute

All arguments to subprograms will contain the `intent` attribute

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**:

* Assists readers in understanding whether a variable is:
  * read-only: intent(in)
  * read/write: intent(inout)
  * effectively uninitialized: intent(out)

* A compiler error will result if code attempts to use a variable
  differently than specified in its \c intent.

* Declared variables without the \c intent attribute can be understood
  to be local.


### CS3: Fortran 2008 Standard Compliance

No modules or subroutines will violate the Fortran 2008 standard

**Type**: Requirement

**Source**: GMTB

**Status**: Undetermined

**Reason**:

* Makes porting to a new compiler easier to near trivial.

* Example: gfortran by default enforces the standard that free-form
  source lines will not exceed 132 characters. Some compilers by
  default allow line lengths to exceed this value. Attempts to port
  codes with line lengths greater than 132 may encounter difficulty.


### CS4: Inline Documentation of Variables

All local and argument list variables will have a comment explaining
the meaning of the variable. An in-line comment on the declaration
line is sufficient

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Status**: Undetermined

**Reason** Allows readers unfamiliar with the code to more quickly
  understand how the code works.


### CS5: Documenting Fortran Subprograms and Modules

All modules and subprograms will have a documentation block describing functionality

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**: Promotes understanding of algorithms and code structure by new users


### CS6: No Fortran Common Blocks

Common blocks are disallowed

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**: Deprecated Fortran feature. Modules provide all the
  functionality of common blocks plus much more.


### CS7: Compatible with GNU gfortran

A package must be compilable with the gfortran compiler (or gcc for
packages coded in C). Runnability and validation can be provided using
whatever compiler(s) the developer prefers.

**Type**: Requirement

**Source**: GMTB

**Reason**: gfortran (and gcc) is free and ubiquitous, and therefore
  is an ideal choice for canonical compiler.


### CS8: Free-Form Fortran

All Fortran source will be free-form

**Type**: Requirement

**Source**: GMTB

**Reason**:

* Fixed-form source is hard to read and archaic.

* A 72-column requirement only makes sense for punch cards.


CS9: Fortran-Callable

All public subprograms will be Fortran-callable

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**:  Fortran is the most commonly used language for geophysical models.


### CS10: Thread-Safe Parameterizations

All parameterizations must be thread-safe (except for initialization
and finalization methods)

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**:

Many geophysical numerical models are threaded these days, and need to
be able to invoke physical parameterizations simultaneously from
multiple threads.

Example code which is NOT thread-safe: Declare a variable `first` and
initialize it to .true. Then test its value and set some static
variables if it is .true. This will likely result in wrong answers
when run in threaded mode.

Solution: Provide an initialization routine which sets the static
variables outside of threaded regions.

Wikipedia provides a brief overview of thread-safety: 

 * https://en.wikipedia.org/wiki/Thread_safety


### CS11: No parameterization will contain a `stop` or `abort` clause

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**: If an error condition arises, it is better to set a flag
and let the caller decide how to handle the condition.

**Status**: Not yet implemented.


### CS12: Use of uninitialized variables is disallowed

**Type**: Requirement

**Source**: GMTB, GSM

**Reason**: Readability.

Not all compilers can be made to initialize static or stack variables
to a known value (e.g. zero).


### CS13: All array indices must fall within their declared bounds.

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**: Debuggers will fail when "tricks" are employed which
  reference arrays outside of their declared bounds.


### CS 14: Self-Reproducible Parameterizations

Multiple runs of the same compiled parameterization given identical
input must produce identical output.  In the case where randomness is
part of the parameterization, a method must be provided to invoke the
same random sequence for test reproducibility.

**Type**: Requirement

**Source**: GMTB, NUOPC PI Team, GSM

**Reason**: Prevents inadvertent errors.


### CS15: Do Not Set Fortran Default Precisions

The use of compiler flags specifying default precision is
disallowed. For example, if 64-bit precision is required, use the
`kind=` attribute to specify the precision rather than a compiler flag
such as \c -r8

**Type**: Requirement

**Source**: GMTB

**Reason**: The behavior of flags is compiler-specific, e.g. if the
  user specifies `real*4 ` does the `-r8` compiler flag


### CS16: List Public Entries in Fortran `module use` Statements

With the exception of common libraries which use a well-defined naming
standard for variables and subroutines, all `module use` statements
must explicitly state which public entities will be referenced. The
MPI library is an example of an acceptable exception: All MPI routines
start with `MPI`, so a blank `use mpi` statement is acceptable.

**Type**: Recommended

**Source**: GMTB

**Reason**: Assists in understanding where various variables and/or
functions or subroutines are defined.


### CS17: No Debugging Code

All code intended for debugging purposes only should be removed prior
to submission for inclusion

**Type**: Recommended

**Source**: GMTB, GSM

**Reason**: Readability


### CS18: Deallocate Allocated Arrays

All arrays explicitly allocated, must be deallocated when no longer
needed

**Type**: Requirement

**Source**: GMTB, GSM

**Reason**: Readability. Minimize memory usage.


### CS19: Default Visibility is `private` 

The default visibility rule for module variables and procedures should
be `private` (specified by a single `private` statement near the
beginning of the module). The `public` attribute is applied to only
those entities which are needed by other subprograms or modules.

**Type**: Recommended

**Source**: GMTB

**Reason**: Limiting variable and subprogram scope is good programming practice


### CS20: Consistent Case in Fortran Code

Consistent use of case is preferred for Fortran code (text strings excepted).

**Type**: Recommended

**Source**: GMTB

**Reason**: While Fortran is a case-insensitive language, variable
`aBc` should also be expressed that way, and not `aBc` in one place,
`abc` in another, and `ABC` in another.


### CS21: Parameterization Steps

A parameterization should contain `init`, `run`, and `finalize`
methods. The `run` method must be thread-safe.

**Type**: Recommended

**Source**: GMTB

**Reason**: Promotes separation of activities which must be done only once at startup or shutdown, from those which are done on multiple time steps.


### CS22: Parameterizations Invoked in Chunks

Parameterizations should be able to be invoked in "chunks", where the
calculations are independent of the fastest-varying subscript.

**Type**: Recommended

**Source**: GMTB

**Reason**:

Computational performance is the main reason for this preference. Many
physical parameterizations in geophysical models contain a dependence
in the vertical, which means this dimension is unavailable for
vectorization. Vectorization can provide up to a 16X speedup on modern
processors.

Example: Outer loop over vertical index `k` can contain vertical
dependence, but if there is also an inner loop over horizontal index
`i` that can be vectorized, the code is likely to run much more
efficiently.


### CS23: Don't `GOTO`

The use of `GOTO` is strongly discouraged, except where no better
option is available.

**Type**: Recommended

**Source**: GMTB

**Reason** Modern languages provide better mechanisms to accomplish
  the same goal in most cases.  `GOTO` promotes "spaghetti" code,
  which can be unreadable.


### CS24: Nested Scope Indentation

Code and declarations within subprograms, loops, and conditional tests
should be indented. Indenting by 2 or 3 or 4 columns is reasonable

**Type**: Recommended

**Source**: GMTB

**Reason**: Readability. Particularly important for multiply nested
  loops and/or `if` tests.


### CS25: Use Symbolic Fortran Comparison Operators

Test operators `\<`, `\<=`, `>`, `>=`, `==`, `/=` are preferred
vs. their deprecated counterparts `.lt.`, `.le.`, `.gt.`, `.ge.`,
`.eq.`, `.ne.`

**Type**: Recommended

**Source**: GMTB, GSM

**Reason** The modern constructs are easier to read, and more
  understandable for those unfamiliar with legacy code.


### CS26: No Bare Constants

The use of bare constants (e.g. `2.7`) inside of computational regions
is strongly discouraged. Instead, a named constant (e.g. `some_variable
= 2.7`) should be declared at the top of the routine or module, along
with an in-line comment stating its purpose


**Type**: Recommended

**Source**: GMTB

**Reason**:

Bare constants buried in code is one of the biggest contributors to
lack of readability and understanding of how code works. "What the
heck does `2.7` mean???" In addition, using a named constant makes it
easier to specify precision, e.g. `real*8 some_var = 35.`


Documentation Management
========================

Goals, Requirements and Expectations
------------------------------------

In the chart below, OP stands for Operating Principles.


### D1: Well-Maintained Documentation

Maintain comprehensive, accessible, and up-to-date documentation for
users, developers, managers, and other stakeholders.

**Type**: Goal

**Source**: Proposed by OAS

**Reason**:  Documentation is necessary to operate NEMS.

**Status**: Completed initial survey.


### D2: Avoid Duplication of Documentation

**Type**: Goal

**Source**: Proposed by OAS

**Reason**:  OP, avoid duplication.

**Status**: No checks in place.


### D3: Documentation Delivered with Software

Documentation should be generated at the time of development and is
considered part of any software delivery.

**Type**: Goal

**Source**: Proposed by OAS

**Status**: No checks in place.

**Reason**:  Encourages timely and accurate documentation.


### D4: Example Code should be in Regression Tests

Any code snippets or code examples in the documentation should be
linked via documentation generation tools to actual code that is
included in regular regression testing.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**:  Minimize the maintenance burden.

**Status**: Some implementation, no checks in place.


### D5: Public Visibility of Documentation

Documentation that does not present a security concern should be publicly visible. 

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**:  OP, open for viewing.

**Status**: No general agreement.


### D6: Documentation is Accessible

Documentation should be accessible to collaborators for contributions.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**:  OP, open for participation.

**Status**: No general agreement.


### D7: Components are Documented

 All NEMS system components (e.g. model components, physics library
 components, workflow components) should have general documentation,
 user documentation, technical documentation, and, as applicable,
 scientific references. We define general documentation to include at
 least a high-level description of the software and its purpose. User
 documentation includes how to checkout, build and run but not
 necessarily modify code. Technical documentation is intended for
 people who need to understand, develop, and change technical code.

**Type**: Requirement

**Source**: Proposesd by OAS-COLA

**Reason**:

User documentation should include how to checkout, compile, install,
configure, run, and analyze outputs of the system. This includes
information on how to perform common tasks, such as changing model
grids and resolutions. Installation documentation should include
detailed itemization of supported compiler(s) (version(s)), MPI
implementation(s) (version(s)), batch system(s) and operating
system(s).  It should also include a step-by-step description for
installation of dependent libraries (e.g. ESMF) and data sets.

**Status**:

Initial survey completed.


### D8: Documentation Versioned with Code

Scientific, user, and technical documentation should be clearly
associated with a version of code. This can be done in a maintainable
manner via documentation generation tools.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**: Versioning is critical for understanding and accuracy.

**Status**: Implementation started.

Tools and Options: Preparation of Documentation
-----------------------------------------------

###Doxygen

This is a popular package used by HWRF. Here is one Doxygen example:

 * http://www.dtcenter.org/HurrWRF/users/support/hwrf_docx/html/index.html

The HWRF developers website is at

 * http://www.dtcenter.org/HurrWRF/developers

By clicking on the various tabs on the left, you can get to the code
management document (which basically is a top SVN repo with all 8
components pulled in as SVN externals, similar to the proposed NEMS
structure). Links to various documentation, including some created
with DOxygen, Latex, and Word are at
http://www.dtcenter.org/HurrWRF/users/docs.

### Protex

This is a Perl script developed at NASA, used by ESMF, NASA, CESM and
other modeling and infrastructure groups. There is a copy included in
the ESMF source:

 * http://sourceforge.net/p/esmf/esmf/ci/master/tree/scripts/doc_templates/templates/protex

Protex commands are placed within comments in source code files. These
commands extract and format sections of the source and embed them in
latex documents. This is used to generate documentation of APIs, code
examples, etc. This documentation is updated automatically as the
source code evolves, which satisfies requirements such as D4. An
example is the ESMF Reference Manual - all of the Class API sections
are pulled from the source code using Protex:

 * http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node1.html

Its main advantage is that it is very simple and easy to understand
and customize, and it creates both text and web
documents. Disadvantages are that it is plain-looking, especially on
the web, and creating web pages requires latex2html or similar.

### Sphinx

Sphinx is a Python-based package that creates documentation that is
more polished-looking than Protex. The ESMF team is using it to
document its ESMPy and OpenClimateGIS packages.

Documentation Current Practice and Recommended Evolution
--------------------------------------------------------

\note Management review is required for the approach to developing and
maintaining technical documentation. An approach is also needed for
organizing documentation overall.

A procedure has been proposed by the NEMS documentation lead, Valbona Kunkel.

At the current time, an outline of the documentation that is available
and desired for NEMS and its applications is being collected on 
[this spreadsheet] (https://docs.google.com/spreadsheets/d/1CLT66uzJrjrsY-um0jB5hU-Gfeh3_VCIJDA4-Ibmu5s/edit#gid=0).
The spreadsheet serves as a survey and gap analysis, and
is intended as a first step in the assembly of comprehensive,
accessible NEMS documentation. Next steps may include:

* disseminate the spreadsheet NGGPS-wide so that other can identify
  additional documentation needed or available

* develop strategies for linking code with documentation to satisfy D4

* restructure and organize documentation into documents and on
  websites so that it is increasingly standardized and easier to find

\note Move spreadsheet to public area if needed.

Input Data Management
=====================

Goals, Requirements and Expectations
------------------------------------

@note In the chart below, OP stands for Operating Principles.


### ID1: Maintain a single source of input data

**Type**: Goal

**Source**: Proposed by OAS

**Reason**:  OP, avoid duplication.

**Status**: No policy in place.


### ID2: Minimize the chances of inadvertent modification of input data.

**Type**: Goal

**Source**: Proposed by OAS

**Reason**: OP, formalize sharing.

**Status**: Not implemented.


### ID3: Easy to Identify and Obtain Input Data for a Configuration

Make it as easy as possible to identify what input data is required
for a configuration and to obtain that data and copy it to a new
location.

**Type**: Goal

**Source**: Proposed by OAS

**Reason**:  OP, engage through clarity..

**Status**: Not implemented.


### ID4: Input Data Not in Personal Directories

Input data should be held in project and shared directories or
repositories and not in personal directories.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**:  OP, formalize sharing.

**Status**: Not implemented.


### ID5: Verifiable Input Data 

A timestamp or checksum approach should be adopted for input data
files to help avoid confusion when files are updated.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**: None

**Status**: Not implemented


### ID6 Metadata provenance should be standardized for input data

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**: None

**Status**: Not implemented.


Input Data Management Current Practice and Recommended Evolution
----------------------------------------------------------------

\note  This section requires management review and concurrence.

\warning This section contains incorrect or incomplete information,
and should be ignored until it is updated.

A proposal for interim NEMS data management is included below. The
motivation for implementing such an interim strategy is to satisfy the
input data design goals above during development of NEMS
applications. These requirements are not satisfied by the current ad
hoc approach, which slows down and can disrupt development.

### Standard NEMS Data Dir

On each system that we support, we determine a location for a "public"
NEMS data directory. This data directory is shared by all different
compsets, and revisions of them. In other words, if you are on Theia,
you know where this data directory lives, and same on yellowstone. Any
input files used by NEMS is located within this data dir. Symbolic
links are allowed within the data dir.

The location of the platform specific NEMS data dir is hard-coded into
the NEMSCompsetRun and documented. It is in general not to be changed.

In subsequent sections the NEMS data dir will be referred to as NEMSDATADIR.

### Compset subdirs

The first sub directory level under NEMSDATADIR will reflect the
compsets that we support. So for example there will be a
20150401_nems_gsm_cice_mom5 subdir.

### Version subdirs

Under each compset subdir, there will be subdirs that indicate
versioning, like v001, v002, ... so on. This level is used by each
compset independent of other compsets to handle the fact that input
data (and regression output) will change during development.

### Component subdirs

The final subdir structure contains a subdir for each component that
is part of the compset. E.g. for 20150401_nems_gsm_cice_mom5 looks
like this:

  * NEMS   (mediator files)
  
  * GSM

  * CICE

  * MOM5

  * REGRESSION_BASELINE

Finally, the NEMSCompsetRun will copy files in the above subdirs to
the RUNDIR when setting up the run. Except for GSM, where we are still
depending on the native scripts, but we point them to files in the GSM
subdir.

Once the above is implemented and documented, it should be more
straightforward to set up the input files for a specific compset, or
version thereof. The directory structure depends on the compset name,
and the version number. Once those two pieces are known, everything
else is fixed, which means that the software knows what to do, and a
user can easily figure out where to look to find the files.

Output Data Management
======================

\note This section requires management review and
concurrence. Preparation of a plan for the estimation, storage and
sharing of outputs by the community is one of the actions identified
in the CDDM workshop on Sept 1-2, 2016 ("Community Data Access Plan").

\note Link to the Community Data Access Plan document

Goals, Requirements and Expectations
------------------------------------

In the chart below, OP stands for Operating Principles.


### OD1: Model Outputs Available to Non-EMC Developers

Model outputs must be available on a platform that is open to
developers and analysts collaborating with EMC.

**Type**: Requirement

**Source**: COLA

**Reason**: OP, avoid duplication.

**Status**: No policy in place.


### OD2: Use Standard Data Formats

Model outputs should be suitable for analysis by the usual tools
(eg. NCL, GrADS, Python-based tools) That implies that they must use
standard data formats (netCDF, GRIB) and possibly satisfy
CF-compliance or even ESGF-compliance.

**Type**: Requirement

**Source**: OAS/COLA

**Reason**: OP, formalize sharing.

**Status**: Not implemented.


### OD3: Model Outputs in Single Space or Transparently Connected Spaces

Model outputs should be stored in a single space, or
easily/transparently connected multiple spaces.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**:  Ease of analysis.

**Status**: Not implemented.


### OD4: Metadata with Model Outputs

Model outputs must be stored with metadata sufficient to reproduce the
runs that generated them.

**Type**: Requirement

**Source**: Proposed by OAS

**Reason**  Ease of analysis.

**Status** Not implemented.



Output Data Management Current Practice and Recommended Evolution
-----------------------------------------------------------------

This is a proposal for the organization of common UCGS Seasonal
results repository. The intent is to facilitate the exchange of
information and ideas and to evaluate the evolution of the coupled
system in its many configurations.  As with other large scale coupled
system projects, such as CMIP5, a certain minimum amount of
organization is needed and can quickly become overwhelming in its
structure.  In CMIP5 mostly unprocessed model output is provided.
Here, even if adequate space can be found, some further processing
will also be needed to reduce repeated processing by individuals and
speed getting out key points and ideas. Thus while this will start
with a similar structure to CMIP5, some key distinctions will arise:

* Any model or processed data need not comply with the strict CMIP5
  requirements, but must still have enough metadata so that
  non-proprietary software can examine it.  Since models being used
  here produce some form of GRIB or NetCDF, this should be enough
  provided the metadata are accurate.

* Some reduction in the 4-dimensional domain (space + time) will
  usually be needed on a per variable basis to aid in examination.
  Vector and tensor components could be stored jointly for easier
  display and calculation.

* In addition to data graphs, charts, maps and pictures and animations
  can also be separately included.  All should be viewable with
  standard browser software.

Results organization will follow CMIP5 notation at the top level with
individual realms each getting a separate directory.  Here a realm
refers to an entire portion of the climate system an is usually
associated with at least one modeling component.  At present the land
realm is found within the atmosphere model component, but may
eventually be separated.  Other applications, such a regional
forecasts, could have a similar division.  The same realm may have
several separate model components, such as MOM and HYCOM for the
ocean.  Additional adjunct components such as atmosphere chemistry or
space weather, should have their results placed in the appropriate
realm for now.  These may be separated later if needed.  In addition
to the four standard CMIP5 directories of atmosphere, ocean, land and
sea ice, a fifth multipurpose directory, labeled X, has also been
added.  This will be detailed later.  Figure 2 shows the basic top
level arrangement.

The arrangement of items within each of the first four directories
will be similar.  The atmosphere directory will be detailed as an
example.

\image html cmip5-arrangement.png "Figure 2"

As with CMIP5 the next level will be variable.  This will permit
easier comparison of the same variable from different experiments.
Since each variable can be examined in many ways and since some
reduction of the model data will be done to further assist in the
examination, some added directory levels are needed.  Typically as
mentioned some domain reduction is done.  This will often be some
sample or simple average of the 4-dimensional data to 2 dimensions.
This would consist of individual levels (atmospheric pressure, height,
model level, potential temperature, etc.) or surface (ground, top of
atmosphere, etc.), some cross section, usually in latitude or
longitude, or some time evolution with a single spatial coordinate.
Within each of these basic 2-d slices, further reduction may be done.
For horizontal slices, global or different regional reductions can be
done.  Further down, instantaneous samples or some time averaging
combinations can down.  Within each of these results from different
revisions would be placed.  These revisions, representing highly
specific model configurations and full input and output dataset and
setting, can consist of any combination of data, graphs, charts, maps
or animations.  Similar reductions would be possible with the other
subdomains.

Ideally the placement of the results from the same variable and
revision in different directories would be uniquely named so that if
copied from the repository, they can be identified as to what they
are.

When analysis requires statistics of the one variable beyond the
simple slicing and averaging, a directory parallel to the subdomain
directory will contain these results.  These would include statistics
between revisions.  In addition to the graphs, charts, maps and
animations a separate directory for tabular data is present.  This
could include Excel files or comma separated variables (.csv) files to
allow spreadsheet examination.  If needed, a subdomain directory could
also be included here for assist in arranging results.  Similarly,
parallel to all the variable directories is a multivariate statistical
directory to examine covariances and similar multivariate
relationships.  It would be arranged similar to the univariate
directories.  Figure 3 shows an idea of how these directories are
related.

\image html directory-idea.png "Figure 3"





The multipurpose (X) directory has been added to deal with some
special needs:

* E(X)change of data between realms (components).  This would include
  input and output fields from the mediator, plus the same fields as
  seen by the components if needed.  These would at least start with
  the unprocessed fields for human review.  Problem fields and
  locations could be highlighted with suitable graphics.  As with the
  other realms, individual variables would be held plus any
  statistics.

* Results from cross(X)-realm evaluations would also be placed here.
  This would include such plots as atmosphere height correlated with
  SST anomaly.

* To expedite getting information out when a suitable pre-specified
  directory cannot be determined, a temporary directory (or perhaps
  permanent if truly unclassifiable) can be set up here to place
  results of unknown (X) assignment.

Release Management for NEMS Applications
========================================

This is a checklist of tasks for preparing a new release of a NEMS
Application.  A release includes making available a revision (and tag)
of a NEMS Application as well as accompanying documentation.

### Preparing the Snapshot Revision

1. Ensure regression tests are passing in all affected applications.
   Changes to common infrastructure such as the Mediator,
   NEMSAppBuilder and NEMSCompsetRun scripts affect multiple
   applications.

2. Prepare the snapshot revision by strictly versioning all
svn:externals:

   a. In your application root directory edit the local svn:externals
      property, strictly versioning all constituent components
      (including NEMS itself).  To determine the strict revision
      numbers, go into the directory of each component, execute svn
      info and find the last change revision number.

   b. \c "$ svn propedit svn:exernals ."

   c. Commit these application svn:externals changes (and potentially
      other changes in the *.appBuilder or *.compsetRun files) back to
      the application repo. Take note of the revision number of this
      commit. This is the revision number of the snapshot revision of
      the application.

   d. Optionally "svn copy" the application trunk (to which you just
      committed) to the application tags directory with a descriptive
      tag name.

   e. Starting with this step, the rest of the steps ensure that your
      development sandbox (and those of others, when they execute svn
      update) is reset to no longer use strict versioning.  Edit the
      local svn:externals property, setting all constituent components
      to float back to the head of their appropriate branch.

   f. Commit the new svn:externals and update.  Those checking out the
      trunk of the NEMS application will receive the head of the
      development branches of all constituent components.  Development
      continues in this mode until it's time to make another snapshot.

Steps for Component NUOPC Caps
------------------------------

1. If the development involved any [Earth System Prediction Suite] (https://www.earthsystemcog.org/projects/esps/)
   components, and during the development for the release changes were
   made to the NUOPC cap, or if a new version of the model was used,
   then it should be documented in the ESPS tables.  Ideally, this
   would include any new test reports, compliance output, and an
   updated description of the cap.  See HYCOM as an example in the
   [ocean components table] (https://www.earthsystemcog.org/projects/esps/ocean_models)

2. Updated components and their NUOPC caps should be pushed back to
   their home repository, and those revision numbers documented.
   Since revision numbers are repository specific, a component and its
   cap will have multiple revision numbers, one associated with each
   repository.  In addition, components with a NUOPC cap included in
   the source distribution that have been formally released will have
   a version number, e.g., 2.1.  In general, precisely documenting
   revision numbers and versions ensures that someone else can get the
   exact same code and reproduce previous results.

Preparing the Release Documentation
-----------------------------------

\warning This section is incorrect.  It does not reflect the fact that
documentation has been moved to the repository.

Note: CoG pages are public documents.  They can be locked so that only
project members can view a page.  A policy should be set as to when
the pages below are made public.

1. Create a new milestone release page, currently on the CoupledNEMS
   CoG site.  Examples of these include [UGCS-Seasonal 0.1] (http://cog-esgf.esrl.noaa.gov/projects/couplednems/drev58214)  and [Regional
   0.2] (http://cog-esgf.esrl.noaa.gov/projects/couplednems/regional02).  Here are the key content pieces that should be documented:

   a. A description of the milestone
   b. The version of each component used.  ESPS components should be linked to the component in the [ESPS tables] (https://www.earthsystemcog.org/projects/esps/).
   c. The model grid of each component
   d. Initial conditions used
   e. All coupling field exchanges - this should be done by updating the [master field spreadsheet] (https://docs.google.com/spreadsheets/d/1X8ByKHfuHZ5x8Ta3Tqv7B0gQBRDXYMK6_yr9tmFu0QE/edit#gid=0) and making a copy of it at a new URL to freeze it.  Link the copied spreadsheet to the milestone page.
   f. The run sequence
   g. How the system was validated, including any plots
   h. Limitations of the system

2. Update (or create) the Build & Run page for the application.   Examples of these include [UGCS-Seasonal] (http://cog-esgf.esrl.noaa.gov/projects/couplednems/ugcs-seasonal) and [Regional] (http://cog-esgf.esrl.noaa.gov/projects/couplednems/regional).

   a. Add an entry to the revisions table.  Since this revision is an actual release, link the revision number and/or tag to the milestone release page (above).
   b. The details column should list, in bullet form, changes since the last release (if there was a previous release) or a description of the new application's features if there was no previous release.
   c. List all compsets that can be run in the released version

3. Prepare a release announcement email including a brief description of the milestone release and links to the milestone release page and the Build & Run page for the application.

4. All documentation should be reviewed and approved before sending out the release notification.  The review should start a minimum of two weeks before the intended release date.

5. Create a CoG news item on the CoupledNEMS site.

\note Move master field spreadsheet to repo







Pilot Projects
==============

NEMS Mediator in CIME
---------------------

One of the features of the NUOPC software architecture is that the
mediator becomes an exchangeable component like an atmosphere or an
ocean model. Like those components, it is possible for the mediator to
be used in different coupled modeling applications.

As described in the earlier Modes of Use section, it is advantageous
to enable coupled NEMS-based modeling applications to be run by the
community both within and outside of operational workflows. Including
the NEMS mediator in CIME will enable a broad community to construct
modeling applications using the NEMS mediator within the mature,
accessible, widely used research environment developed by CESM for
many-component coupled modeling. Components may include community
models with NUOPC interfaces, such as HYCOM, MOM5 and CICE. They may
also include model components from CESM, which shares many of the same
types of components anticipated in NEMS (atmosphere, ocean, sea ice,
land surface, wave, etc.), and NUOPC-compliant components from other
centers.The NEMS mediator could be exercised in a wide variety of
configurations and predictive time scales, and developed as a
community component, with the scrutiny, increased robustness, and
access to innovation that implies. A benefit of the collaboration for
EMC is CIME’s relationship to a community with established research
activities, expertise and outreach in many-component coupled modeling.

Importantly, the standard NUOPC component interfaces on the mediator
and model components maintain a link back to the operational system at
EMC, ensuring that as advances are made they are accessible to
operations.

Figure 4 shows the kinds of infrastructure that would accompany the
NEMS mediator in the CIME repository. CIME enables the user to
download and begin running a complete standalone coupled test system,
where the test components can be replaced by active components with
compliant interfaces.

Specific tasks (FY16) include:

1. With direction from NOAA leadership, implement NOAA Environmental
Modeling Center (EMC) requirements for code distribution of the NEMS
mediator via CIME. This is anticipated to include a download
registration page and a plan for monitoring for requests for code
access originating from ITAR proscribed countries.

2. In collaboration with EMC and teams involved with NEMS development,
establish a working, documented test system using the NEMS mediator
with CIME data models and testing infrastructure. Demonstrate its use
with a limited set of active components.


\image html cime-nems-mediator.png "Figure 4"

VLab Pilot Project
------------------

EMC is beginning to install EMC software in the NCEP/EMC subversion
repository under VLab to enable community collaboration. Mark Potts is
leading this effort.
