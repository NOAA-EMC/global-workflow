Standards {#standards}
=========

This page contains itemized requirements for how data, code, and
documentation is managed within the NEMS system.  This should be
thought of as a set of rules; further information can be found
throughout the documentation and milestone release pages.

Operating Principles
====================

\todo Management review of this section is one of the items identified
as an action in the CDDM workshop on Sept 1-2, 2016 (“Operating
Principles”).

The following are a set of operating principles (OP later in the
document) that apply across multiple categories of requirements.

* Make decisions with the end goal of producing the best possible
  forecasts, based on evidence rather than assertion.

* Operate efficiently. Avoid redundancy, and favor automation over
  manual processes.

* Promote transparency. To the extent possible, make code, data,
  documentation, requirements, policies, and plans openly available
  and clear.

* Encourage community ownership and participation. Establish processes
  that enable partners to engage in decision-making.

* Ensure that organizational structures support clear responsibilities
  and accountability.

Roles
=====

* **Components**

   * **Component Code Managers** --- people internal to NCEP who
      * Manage the technical aspects of component code
      * Manage the connection of the code to the 
        \ref building "NEMS Build System" and
        \ref running "NEMS Compset System"

   * **External component points of contact** --- people external to NCEP that:
      * Are the point of contact for coordination and synchronization
        with NCEP developers

* **Applications**

   * **Application Leads** --- responsible for scientific and technical
     development and delivery of a particular application.
      * Provide status reports to the NEMS development community as needed
      * Provide Milestone Release Documents as needed. This task 
        may be delegated.

   * **Application Code Managers** --- responsible for technical
     aspects of application development
      * Maintain documentation, code and data for their application
      * Ensure at least one
        \ref running "compset"
        works, so that NEMS framework updates can be tested against the application
      * Ensure that the 
        \ref building "NEMS build system (NEMSAppBuilder"
        is able to compile their application.
      * Coordinate with other developers to update the application to handle
        incoming changes to the NEMS framework that may change 
        the applications output

* **NEMS**

  * **NEMS Lead** --- responsible for scientific and technical
      development and delivery of a particular application.
    * Provide status reports to the NEMS development community as needed
    * Provide Milestone Release Documents as needed.  This task may be delegated

  * **NEMS Code Manager** --- responsible for technical aspects of code development
    * Tests NEMS framework changes against all applications upon request,
      including just before a NEMS commit.
    * Maintains nightly tests of a limited set of compsets for each app
    * Maintains the NEMS source code, build, and test system.

The current status at EMC is that there is a technical team with the
following members:

* Mark Iredell: manager
* Sam Trahan: NEMS code manager
* Valbona Kunkel: Documentation
* Hang Lei: Physics interface

Other members of EMC that are sometimes involved include:

* Terry McGuinness - GFS workflow
* Kate Howard - GFS workflow




Requirements Format and and Collection
======================================

\todo This section requires management review and
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



<table>
<tr><th rowspan="2">SM1</th><th>Minimize Software Repositories Per Component</th></tr>
<tr><td>
Minimize the number of software repositories required per
component. Best practices in software configuration management
recommend using a shared common repository for development where
feasible.  New development can be managed using branches (or forks or
equivalent) with the understanding of a common authoritative source
(master/trunk) and a procedure for integrating new development into
the source repository.  This approach utilizes the strengths of
configuration management tools, while minimizing the work and risk
involved in maintaining duplicate repositories.  
</td></tr>
<tr><th>Type</th><td>goal
</td></tr>
<tr><th>Source</th><td>GMTB
</td></tr>
<tr><th>Status</th><td>No policy in place
</td></tr>
<tr><th>Reason</th><td>OP, avoid duplication
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM2</th><th>Source Code Available</th></tr>
<tr><td >
All source code for operational modeling applications and suites must
be available on EMC computers.  Availability in case there are
problems.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>NCO<br>
</td></tr>
<tr><th>Status</th><td>Implemented<br>
</td></tr>
<tr><th>Reason</th><td>Availability in case of problems
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM3</th><th>Accessible Repositories</th></tr>
<tr><td >
NEMS modeling applications and suites are expected to utilize multiple
development repositories, including repositories managed externally
and by EMC. It must be possible for developers (NOAA and non-NOAA) to
access codes in the development repositories, workspaces, and
trackers, with specific permission levels (read, read/write).
</td></tr>
<tr><th>Type</th><td>EXP
<br>**Source**: OAS<br>
</td></tr>
<tr><th>Status</th><td>Not fully implemented, some key developers do not have access to workspaces and trackers.<br>
</td></tr>
<tr><th>Reason</th><td>Access is needed for development.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM3a</th><th>Current Operational Code is Available</th></tr>
<tr><td >
It is essential that the currently operational code can be checked
out. An exact mirror repository should be maintained that always has
the latest operational code.  
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>COLA/Kinter<br>
</td></tr>
<tr><th>Status</th><td>No policy in place<br>
</td></tr>
<tr><th>Reason</th><td>This is needed to streamline the transition from research to operations
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM4</th><th>EMC Repository Developer Responsibilities</th></tr>
<tr><td >
The following apply to developers working in an EMC repository:
* Developer maintains own branch of trunk.
* Commit work back to branch from working copy frequently.
* Keep up to date with trunk.
* Use test harness for regression and suite testing prior to commits.
* Use ticket system as required.
</td></tr>
<tr><th>Type</th><td>EXP<br>
</td></tr>
<tr><th>Source</th><td>EMC/Tollman<br>
</td></tr>
<tr><th>Status</th><td>Unable to implement because of lack of access to computers and trackers..<br>
</td></tr>
<tr><th>Reason</th><td>Follow good software practices for development.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM5</th><th>External Component Development Responsibilities</th></tr>
<tr><td >
The following apply to developers working with external components:

* If procedures used in the external, authoritative repository are
  compatible with NEMS development, the procedures specific to that
  component will be followed.

* If the external, authoritative repository cannot support component
  development for NEMS, a development repository for EMC use will be
  established and the procedures established for EMC repositories
  followed.
</td></tr>
<tr><th>Status</th><td>Implemented.<br>
</td></tr>
<tr><th>Reason</th><td>Balance between low process overhead and control over processes.<br>
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM6</th><th>Components have Identified Leaders</th></tr>
<tr><td >
There is an identified EMC component lead for all model and suite
components. There is an external component lead identified for
external components.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>NCO<br>
</td></tr>
<tr><th>Status</th><td>Implemented<br>
</td></tr>
<tr><th>Reason</th><td>OP, accountability.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM7</th><th>Identified Leaders for NEMS Documentation</th></tr>
<tr><td >
There are identified leads for the overall NEMS system development at EMC.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.<br>
</td></tr>
<tr><th>Reason</th><td>OP, accountability.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM8</th><th>Synchronization Path to External Component Repositories</th></tr>
<tr><td >
Code changes to external components taking place in their native
repositories must have a path for synchronization with changes made to
these components at EMC, and vice versa. It is up to the EMC component
lead to synchronize changes between development and operational
repositories, in coordination with the external component lead. If
necessary, users can download a tar file of a code release and return
changes via tar file.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>EMC/Tollman<br>
</td></tr>
<tr><th>Status</th><td>No controls in place.<br>
</td></tr>
<tr><th>Reason</th><td>Need for synchronization of development to maintain coherence in community-based unified system.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM9</th><th>Synchronization Path of Component Code Between Applications</th></tr>
<tr><td >
Changes in components and infrastructure made for any given NEMS
modeling application must have a process for synchronization with
versions of these codes used by other NEMS applications.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS<br>
</td></tr>
<tr><th>Status</th><td>No policy in place..<br>
</td></tr>
<tr><th>Reason</th><td>Need for synchronization of development to maintain
  coherence in community-based unified system.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM10</th><th>Standardized Testing and Implementation System</th></tr>
<tr><td >
There is standard regression, suite, operations testing for
respectively software, science, and implementation.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS<br>
</td></tr>
<tr><th>Status</th><td>Key processes not implemented.<br>
</td></tr>
<tr><th>Reason</th><td>Critical part of software process.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM11</th><th>Repository Strategy Supporting Many Components and Applications</th></tr>
<tr><td >
The repository strategy must support testing and experimentation with many-component modeling applications and suites.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS<br>
</td></tr>
<tr><th>Status</th><td>Repository strategy is not fully defined.<br>
</td></tr>
<tr><th>Reason</th><td>Needed to manage development in multi-component, multi-application NEMS system.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM12</th><th>Component Versions Associated with Application Versions</th></tr>
<tr><td >
It must be possible to easily assemble a version of a particular
modeling application, with versioned constituent components.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS<br>
</td></tr>
<tr><th>Status</th><td>Implemented<br>
</td></tr>
<tr><th>Reason</th><td>Needed to manage development in multi-component,
   multi-application NEMS system.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">SM13</th><th>Availability of Stub, Data, and Active Model Components</th></tr>
<tr><td >
Model components must offer active, stub, and data versions for
testing and experimentation.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS, EMC/Grumbine<br>
</td></tr>
<tr><th>Status</th><td>Data versions are not available.<br>
</td></tr>
<tr><th>Reason</th><td>Needed for testing and development.
</td></tr>
</table>

<br><br>



Coding standards
----------------

\todo The proposed standards here need to be reconciled with
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

\todo Move the GSM coding standards to a public area



<table>
<tr><th rowspan="2">CS1</th><th>Fortran Implicit None</th></tr>
<tr><td >
All fortran modules and subroutines will contain `implicit none`
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, GSM<br>
**Reason**:<br>
* Assists in writing bug-free code.
* Understanding implicit type rules is difficult and arcane.
* Understanding where a variable comes from (local, input argument
  list, module) is more difficult with implicit typing
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS2</th><th>Fortran \c Intent Attribute</th></tr>
<tr><td >
All arguments to subprograms will contain the `intent` attribute
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>

* Assists readers in understanding whether a variable is:
  * read-only: intent(in)
  * read/write: intent(inout)
  * effectively uninitialized: intent(out)

* A compiler error will result if code attempts to use a variable
  differently than specified in its \c intent.

* Declared variables without the \c intent attribute can be understood
  to be local.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS3</th><th>Fortran 2008 Standard Compliance</th></tr>
<tr><td >
No modules or subroutines will violate the Fortran 2008 standard
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Status</th><td>Undetermined<br>
</td></tr>
<tr><th>Reason</th><td>

* Makes porting to a new compiler easier to near trivial.

* Example: gfortran by default enforces the standard that free-form
  source lines will not exceed 132 characters. Some compilers by
  default allow line lengths to exceed this value. Attempts to port
  codes with line lengths greater than 132 may encounter difficulty.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS4</th><th>Inline Documentation of Variables</th></tr>
<tr><td >
All local and argument list variables will have a comment explaining
the meaning of the variable. An in-line comment on the declaration
line is sufficient
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Status</th><td>Undetermined<br>
</td></tr>
<tr><th>Reason</th><td>Allows readers unfamiliar with the code to more quickly
  understand how the code works.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS5</th><th>Documenting Fortran Subprograms and Modules</th></tr>
<tr><td >
All modules and subprograms will have a documentation block describing functionality
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>Promotes understanding of algorithms and code structure by new users
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS6</th><th>No Fortran Common Blocks</th></tr>
<tr><td >
Common blocks are disallowed

</td></tr>
<tr><th>Type</th><td>Requirement

</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM

</td></tr>
<tr><th>Reason</th><td>Deprecated Fortran feature. Modules provide all the
  functionality of common blocks plus much more.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS7</th><th>Compatible with GNU gfortran</th></tr>
<tr><td >
A package must be compilable with the gfortran compiler (or gcc for
packages coded in C). Runnability and validation can be provided using
whatever compiler(s) the developer prefers.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>gfortran (and gcc) is free and ubiquitous, and therefore
  is an ideal choice for canonical compiler.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS8</th><th>Free-Form Fortran</th></tr>
<tr><td >
All Fortran source will be free-form
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>

* Fixed-form source is hard to read and archaic.

* A 72-column requirement only makes sense for punch cards.
</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS9</th><th>Fortran-Callable</th></tr>
<tr><td >
All public subprograms will be Fortran-callable
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>Fortran is the most commonly used language for geophysical models.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS10</th><th>Thread-Safe Parameterizations</th></tr>
<tr><td >
All parameterizations must be thread-safe (except for initialization
and finalization methods)
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>
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
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS11</th><th>No parameterization will contain a `stop` or `abort` clause</th></tr>
<tr><td >
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>If an error condition arises, it is better to set a flag
and let the caller decide how to handle the condition.<br>
</td></tr>
<tr><th>Status</th><td>Not yet implemented.
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS12</th><th>Use of uninitialized variables is disallowed</th></tr>
<tr><td >
</td></tr>
<tr><th>Type</th><td>Requirement
</td></tr>
<tr><th>Source</th><td>GMTB, GSM
</td></tr>
<tr><th>Reason</th><td>Readability.

Not all compilers can be made to initialize static or stack variables
to a known value (e.g. zero).
</td></tr>
</table>

<br><br>

<table>
<tr><th rowspan="2">CS13</th><th>All array indices must fall within their declared bounds.</th></tr>
<tr><td >
</td></tr>
<tr><th>Type</th><td>Requirement
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM
</td></tr>
<tr><th>Reason</th><td>Debuggers will fail when "tricks" are employed which
  reference arrays outside of their declared bounds.
</td></tr></table>

<br><br>

<table>
<tr><th rowspan="2">CS14</th><th>Self-Reproducible Parameterizations</th></tr>
<tr><td>
Multiple runs of the same compiled parameterization given identical
input must produce identical output.  In the case where randomness is
part of the parameterization, a method must be provided to invoke the
same random sequence for test reproducibility.
</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, NUOPC PI Team, GSM<br>
</td></tr>
<tr><th>Reason</th><td>Prevents inadvertent errors.
</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS15</th><th>Do Not Set Fortran Default Precisions</th></tr>
<tr><td >

The use of compiler flags specifying default precision is
disallowed. For example, if 64-bit precision is required, use the
`kind=` attribute to specify the precision rather than a compiler flag
such as \c -r8

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>The behavior of flags is compiler-specific, e.g. if the
  user specifies `real*4 ` does the `-r8` compiler flag


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS16</th><th>List Public Entries in Fortran `module use` Statements</th></tr>
<tr><td >

With the exception of common libraries which use a well-defined naming
standard for variables and subroutines, all `module use` statements
must explicitly state which public entities will be referenced. The
MPI library is an example of an acceptable exception: All MPI routines
start with `MPI`, so a blank `use mpi` statement is acceptable.

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>Assists in understanding where various variables and/or
functions or subroutines are defined.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS17</th><th>No Debugging Code</th></tr>
<tr><td >

All code intended for debugging purposes only should be removed prior
to submission for inclusion

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB, GSM<br>
</td></tr>
<tr><th>Reason</th><td>Readability


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS18</th><th>Deallocate Allocated Arrays</th></tr>
<tr><td >

All arrays explicitly allocated, must be deallocated when no longer
needed

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>GMTB, GSM<br>
</td></tr>
<tr><th>Reason</th><td>Readability. Minimize memory usage.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS19</th><th>Default Visibility is `private` </th></tr>
<tr><td >

The default visibility rule for module variables and procedures should
be `private` (specified by a single `private` statement near the
beginning of the module). The `public` attribute is applied to only
those entities which are needed by other subprograms or modules.

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>Limiting variable and subprogram scope is good programming practice


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS20</th><th>Consistent Case in Fortran Code</th></tr>
<tr><td >

Consistent use of case is preferred for Fortran code (text strings excepted).

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>While Fortran is a case-insensitive language, variable
`aBc` should also be expressed that way, and not `aBc` in one place,
`abc` in another, and `ABC` in another.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS21</th><th>Parameterization Steps</th></tr>
<tr><td >

A parameterization should contain `init`, `run`, and `finalize`
methods. The `run` method must be thread-safe.

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>Promotes separation of activities which must be done only once at startup or shutdown, from those which are done on multiple time steps.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS22</th><th>Parameterizations Invoked in Chunks</th></tr>
<tr><td >

Parameterizations should be able to be invoked in "chunks", where the
calculations are independent of the fastest-varying subscript.

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>

Computational performance is the main reason for this preference. Many
physical parameterizations in geophysical models contain a dependence
in the vertical, which means this dimension is unavailable for
vectorization. Vectorization can provide up to a 16X speedup on modern
processors.

Example: Outer loop over vertical index `k` can contain vertical
dependence, but if there is also an inner loop over horizontal index
`i` that can be vectorized, the code is likely to run much more
efficiently.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS23</th><th>Don't `GOTO`</th></tr>
<tr><td >

The use of `GOTO` is strongly discouraged, except where no better
option is available.

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>Modern languages provide better mechanisms to accomplish
  the same goal in most cases.  `GOTO` promotes "spaghetti" code,
  which can be unreadable.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS24</th><th>Nested Scope Indentation</th></tr>
<tr><td >

Code and declarations within subprograms, loops, and conditional tests
should be indented. Indenting by 2 or 3 or 4 columns is reasonable

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>Readability. Particularly important for multiply nested
  loops and/or `if` tests.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS25</th><th>Use Symbolic Fortran Comparison Operators</th></tr>
<tr><td >

Test operators `<`, `<=`, `>`, `>=`, `==`, `/=` are preferred
vs. their deprecated counterparts `.lt.`, `.le.`, `.gt.`, `.ge.`,
`.eq.`, `.ne.`

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB, GSM<br>
</td></tr>
<tr><th>Reason</th><td>The modern constructs are easier to read, and more
  understandable for those unfamiliar with legacy code.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">CS26</th><th>No Bare Constants</th></tr>
<tr><td >

The use of bare constants (e.g. 2.7) inside of computational regions
is strongly discouraged. Instead, a named constant (e.g. `some_variable
= 2.7`) should be declared at the top of the routine or module, along
with an in-line comment stating its purpose

</td></tr>
<tr><th>Type</th><td>Recommended<br>
</td></tr>
<tr><th>Source</th><td>GMTB<br>
</td></tr>
<tr><th>Reason</th><td>

Bare constants buried in code is one of the biggest contributors to
lack of readability and understanding of how code works. "What the
heck does `2.7` mean???" In addition, using a named constant makes it
easier to specify precision, e.g. `real*8 some_var = 35.`
</td></tr>
</table>

<br><br>



Documentation Management
------------------------

In the sections below, OP stands for Operating Principles.




<table>
<tr><th rowspan="2">D1</th><th>Well-Maintained Documentation</th></tr>
<tr><td >

Maintain comprehensive, accessible, and up-to-date documentation for
users, developers, managers, and other stakeholders.

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>Documentation is necessary to operate NEMS.<br>
</td></tr>
<tr><th>Status</th><td>Completed initial survey.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D2</th><th>Avoid Duplication of Documentation</th></tr>
<tr><td >

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, avoid duplication.<br>
</td></tr>
<tr><th>Status</th><td>No checks in place.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D3</th><th>Documentation Delivered with Software</th></tr>
<tr><td >

Documentation should be generated at the time of development and is
considered part of any software delivery.

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Status</th><td>No checks in place.<br>
</td></tr>
<tr><th>Reason</th><td>Encourages timely and accurate documentation.</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D4</th><th>Example Code should be in Regression Tests</th></tr>
<tr><td >

Any code snippets or code examples in the documentation should be
linked via documentation generation tools to actual code that is
included in regular regression testing.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>Minimize the maintenance burden.<br>
</td></tr>
<tr><th>Status</th><td>Some implementation, no checks in place.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D5</th><th>Public Visibility of Documentation</th></tr>
<tr><td >

Documentation that does not present a security concern should be publicly visible. 

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, open for viewing.<br>
</td></tr>
<tr><th>Status</th><td>No general agreement.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D6</th><th>Documentation is Accessible</th></tr>
<tr><td >

Documentation should be accessible to collaborators for contributions.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, open for participation.<br>
</td></tr>
<tr><th>Status</th><td>No general agreement.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D7</th><th>Components are Documented</th></tr>
<tr><td >

 All NEMS system components (e.g. model components, physics library
 components, workflow components) should have general documentation,
 user documentation, technical documentation, and, as applicable,
 scientific references. We define general documentation to include at
 least a high-level description of the software and its purpose. User
 documentation includes how to checkout, build and run but not
 necessarily modify code. Technical documentation is intended for
 people who need to understand, develop, and change technical code.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposesd by OAS-COLA<br>
</td></tr>
<tr><th>Reason</th><td>

User documentation should include how to checkout, compile, install,
configure, run, and analyze outputs of the system. This includes
information on how to perform common tasks, such as changing model
grids and resolutions. Installation documentation should include
detailed itemization of supported compiler(s) (version(s)), MPI
implementation(s) (version(s)), batch system(s) and operating
system(s).  It should also include a step-by-step description for
installation of dependent libraries (e.g. ESMF) and data sets.

</td></tr>
<tr><th>Status</th><td>

Initial survey completed.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">D8</th><th>Documentation Versioned with Code</th></tr>
<tr><td >

Scientific, user, and technical documentation should be clearly
associated with a version of code. This can be done in a maintainable
manner via documentation generation tools.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>Versioning is critical for understanding and accuracy.<br>
</td></tr>
<tr><th>Status</th><td>Implementation started.



</td></tr>
</table>

<br><br>


Input Data Management
---------------------

In the list below, OP stands for Operating Principles.



<table>
<tr><th rowspan="2">ID1</th><th>Maintain a single source of input data</th></tr>
<tr><td >

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, avoid duplication.<br>
</td></tr>
<tr><th>Status</th><td>No policy in place.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">ID2</th><th>Minimize the chances of inadvertent modification of input data.</th></tr>
<tr><td >

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, formalize sharing.<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">ID3</th><th>Easy to Identify and Obtain Input Data for a Configuration</th></tr>
<tr><td >

Make it as easy as possible to identify what input data is required
for a configuration and to obtain that data and copy it to a new
location.

</td></tr>
<tr><th>Type</th><td>Goal<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, engage through clarity..<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">ID4</th><th>Input Data Not in Personal Directories</th></tr>
<tr><td >

Input data should be held in project and shared directories or
repositories and not in personal directories.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>OP, formalize sharing.<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">ID5</th><th>Verifiable Input Data </th></tr>
<tr><td >

A timestamp or checksum approach should be adopted for input data
files to help avoid confusion when files are updated.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>None<br>
</td></tr>
<tr><th>Status</th><td>Not implemented


### ID6 Metadata provenance should be standardized for input data

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>None<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.
</td></tr>
</table>

<br><br>








Output Data Management
----------------------

\todo This section requires management review and
concurrence. Preparation of a plan for the estimation, storage and
sharing of outputs by the community is one of the actions identified
in the CDDM workshop on Sept 1-2, 2016 ("Community Data Access Plan").

\todo Link to the Community Data Access Plan document

In the list below, OP stands for Operating Principles.



<table>
<tr><th rowspan="2">OD1</th><th>Model Outputs Available to Non-EMC Developers</th></tr>
<tr><td >

Model outputs must be available on a platform that is open to
developers and analysts collaborating with EMC.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>COLA<br>
</td></tr>
<tr><th>Reason</th><td>OP, avoid duplication.<br>
</td></tr>
<tr><th>Status</th><td>No policy in place.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">OD2</th><th>Use Standard Data Formats</th></tr>
<tr><td >

Model outputs should be suitable for analysis by the usual tools
(eg. NCL, GrADS, Python-based tools) That implies that they must use
standard data formats (netCDF, GRIB) and possibly satisfy
CF-compliance or even ESGF-compliance.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>OAS/COLA<br>
</td></tr>
<tr><th>Reason</th><td>OP, formalize sharing.<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">OD3</th><th>Model Outputs in Single Space or Transparently Connected Spaces</th></tr>
<tr><td >

Model outputs should be stored in a single space, or
easily/transparently connected multiple spaces.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>Ease of analysis.<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.


</td></tr>
</table>

<br><br>


<table>
<tr><th rowspan="2">OD4</th><th>Metadata with Model Outputs</th></tr>
<tr><td >

Model outputs must be stored with metadata sufficient to reproduce the
runs that generated them.

</td></tr>
<tr><th>Type</th><td>Requirement<br>
</td></tr>
<tr><th>Source</th><td>Proposed by OAS<br>
</td></tr>
<tr><th>Reason</th><td>Ease of analysis.<br>
</td></tr>
<tr><th>Status</th><td>Not implemented.
</td></tr>
</table>

<br><br>
