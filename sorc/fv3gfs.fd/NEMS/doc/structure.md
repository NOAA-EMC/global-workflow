Repository Structure and Versioning {#structure}
===================================

NEMS modeling applications are multi-component coupled systems that
must pull together, in a systematic way, components from different
locations. The NEMS AppBuilder enables users to construct a specific,
versioned modeling application from a versioned set of model
components and configuration files. The AppBuilder approach also helps
to ensure that changes made to the different applications are
coordinated as they get checked back into the NEMS repository.

The NEMS AppBuilder introduces two levels of configuration control:
the modeling application level and the NEMS level. This split provides
the model developer with full version control over all aspects of the
modeling application code, while hiding many technical details about
the infrastructure at the NEMS level.

Exposed to/changeable by the user:

* Version of NEMS.
* List of external components (e.g. MOM5, CICE, ...) required by the application.
* Version of each required external component.
* Location of source and installation directory for NEMS and each external component.
* Compiler and 3rd party library versions.

Mostly hidden from the user:
* Knowledge base of how to build each of the supported external components.
* Infrastructure to configure and build NEMS according to the
  information specified by the application layer.
* Coupling system based on ESMF/NUOPC.

Repository Version Tracking
---------------------------

NEMS uses Subversion (SVN) and Git in combination for many-component
modeling systems. In this approach, the code associated with a
versioned, coupled modeling system is a collection of external
repository tags that are accessed through the svn:external
property. External components and other external elements which reside
in either other Subversion or GitHub repositories can easily be
included in this process since GitHub provides subversion access to
Git repository tags.

Both the NEMS and CESM use this approach. With the AppBuilder, each
application (e.g. HYCOM-GSM-CICE, NEMSfv3gfs, ...) is under its own SVN
revision controlled directory. This directory contains the
application-specific AppBuilder configuration file and a number of
svn:external links. It is through these links that specific revisions
of every component, and NEMS itself, are checked out when the
application directory is checked out. The actual source code (for each
component and NEMS) then lives in its own SVN or Git
repository. Appendix A shows in detail how this is done in CESM.

\todo Add appendix A

The recommended best management practice for implementing the
application version control is with SVN using externals. SVN externals
allow the application to point to specific versions of NEMS and
specific versions of each of the external components. Only external
components actually used by the application need to be referenced.

Overall Repository Structure
----------------------------

The NEMS repository is divided into three types of areas:

 * NEMS Framework - This repository contains the mediator, build
   system, compset runner, and this documentation

 * model components and caps - These reside in other repositories

 * NEMS Applications - A group of repositories, one for each NEMS
   application.  They contain Subversion Externals to the NEMS
   Framework, and each component.

Application Repository Directories
----------------------------------

Following the above recommendation, the application directory
structure under SVN control looks like this:

 * GSM-MOM5-CICE5 
    
    * GSM-MOM5-CICE5.appBuilder - application-specific AppBuilder
      file, listing required external components

    * ... other *.appBuilder files ...

    * conf/ - build configuration files

    * modulefiles/ - `module` files to load external dependencies such as NetCDF

       * theia/ - `module` files for Theia machine

       * wcoss.cray/ - `module` files for the Cray partition of WCOSS

       * ... more platform support ...
    
    * compsets/ - definition of compsets for this application

    * parm/ - small parameter files for compsets.  Large files are
      located outside of the repository.

    * log/ - subdirectory for log files from execution and compilation.
      These are placed in the repository log/ directory so that they can
      be used to track changes, by re-using compsets as regression tests.
    
    * NEMS  - subversion external to NEMS
    
       * \ref building "NEMSAppBuilder"

       * \ref running "NEMSCompsetRun"

       * exe/ - Built executables are placed here

       * src/

          * ... NEMS code located here ...

       * test/ - Underlying implementation of the NEMS compset runner

    * CICE - Subversion external to CICE model

       * ... CICE code located here ...

    * MOM5 - Subversion external to MOM5 model

       * ... CICE code located here ...

    * MOM5_CAP - Subversion external to the NUOPC CAP of MOM5

       * ... NUOPC CAP code for MOM5 located here ...

    * ... other components ...

This structure gives full control of versioning of all of the model
code to the application.  One can obtain the entire structure,
including components, by checking out the application from Subversion.
For a hypothetical `SomeApp` app,

    svn co https://svnemc.ncep.noaa.gov/projects/nems/apps/SomeApp/trunk SomeApp

This checks out everything needed to build the initial revision of the
`SomeApp` application prototype from source into a directory called
`SomeApp`. 

Compilation instructions can be found in this page: 

 * \ref building


Location of the Modeling Application Directory
----------------------------------------------

The application directory can be located anywhere under revision
control. However, most of the NCEP applications are expected to be
located on the EMC SVN server.  It is very simple to relocate a
directory that is under SVN control to a different location at any
time (or to rename it), without losing any history. For this reason we
have set up a staging area here:

* https://svnemc.ncep.noaa.gov/projects/nems/apps

where applications may first be placed before a final location is
identified. We recommend creating the canonical trunk, tags, branches
directory triplet under each application for consistency.  For
example, the HYCOM-GSM-CICE coupling project has an application here:

 * /projects/nems/apps/HYCOM-GSM-CICE
   * branches
   * tags
   * trunk

At any point in the future the HYCOM-GSM-CICE directory can be renamed
and/or copied to a different location on the SVN server. None of the
revision history will be lost.  This setup provides a very structured
approach for early development work, without committing to anything
that cannot be changed in the future.

#### 6.2.1.2	Handling External Components

NEMS includes some external components whose primary development
repository is outside of EMC. The application level provides the
necessary control over which version of a specific external component
is used in the modeling application.

External components can be located anywhere under revision
control. There is a staging area:

 * https://svnemc.ncep.noaa.gov/projects/nems/external_components

where external components may first be placed before a final location
is identified (in some cases back at the parent organization). We
recommend creating the canonical trunk, tags, branches directory
triplet under each application for consistency.  E.g., at this point
the future XYZ component has this location and directory structure:

 * /projects/nems/external_components/XYZ
   * branches
   * tags
   * trunk

At any point in the future the XYZ directory can be renamed and/or
moved to a different location on the SVN server. None of the revision
history will be lost.  This setup provides a very structured approach
for early development work, without committing to anything that cannot
be changed in the future.

For example, the FV3 component is here:

 * /projects/fv3/trunk
