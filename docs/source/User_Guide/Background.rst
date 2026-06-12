######################
Background Information
######################

The **Unified Forecast System (UFS)** is a community-based, coupled, comprehensive Earth modeling system. NOAA's operational model suite for numerical weather prediction (NWP) is quickly transitioning to the UFS from a number of different modeling systems. The UFS enables research, development, and contribution opportunities within the broader Weather Enterprise (including government, industry, and academia). For more information about the UFS, visit the `UFS Portal <https://ufs.epic.noaa.gov/>`_. The UFS includes multiple applications that support different forecast durations and spatial domains. 

The  **Global Workflow (GW)** is an end-to-end system designed to run global configurations of medium range weather forecasts for the **UFS-Weather Model (WM)**. It is a combination of several model components seamlessly integrated into an end-to-end workflow to prepare, analyze, generate, and post-process forecast data. In its current form, GW supports **Global Forecast System (GFS)**, **Global Ensemble Forecast System (GEFS)**, **Global Chemistry and Aerosol Forecast System (GCAFS)**, and **Subseasonal Forecast System (SFS)** configurations. It is built to support both development and operational implementations. GW eliminates the need for manual stitching and ensures consistency between operational forecasts and research experiments. It is easily portable for running UFS-WM across diverse computing environments. This documentation describes the GW system. 


==========================
Documentation Organization
==========================

GW Documentation is organized to support both new users and advanced users or developers:

* **User's Guide**

  - **Background Information**: Overview of the workflow, documentation conventions, and user support resources.
  - **GW Components**: Components of Global Workflow and their repositories. 
  - **System Requirements & HPC Setup**: Platforms, prerequisites, and installation steps.
  - **Setup Globus Connection for HPSS**: Setting up connection for NOAA High Performance Storage System (HPSS) for data access.
  - **GFS Configuration**: Global Forecast System Configuration and the list of jobs run in GFS
  - **GCAFS Configuration**: Global Chemistry and Aerosol Forecast System (GCAFS) overview and setting up and running GCAFS.
  - **Running GW on RDHPCS**: How to setup the experiment, configure, launch, monitor GW runs, and view the forecast output.
  - **Setup & Run GW on CSPs**: How to setup and run GW on Cloud Service Providers (CSPs).
  - **Plotting Output**: Instructions for plotting analysis output and forecast output.
  - **Common Errors and Known Issues**: List of common errors/issues and their solutions.

* **Contributor's Guide**

  - **Contributing to the Global Workflow**: A section devoted to developers to contribute to the Global Workflow repository.
  - **Testing Global Workflow Jobs**: a comprehensive CTest framework for testing individual Rocoto workflow job.
  - **GitLab CI/CD Pipeline Infrastructure**: a comprehensive reference for the GitLab CI/CD pipeline infrastructure by GW and Troubleshooting.
  - **Wave Component**: Overview of WAVE-height, WATer depth and Current Hindcasting (WAVEWATCH) III, the community wave modeling framework, a component of the GW.

=========================
Documentation Conventions
=========================

This guide uses Code blocks to indicate commands and code snippets, file and directory paths, variables, and options.

.. code-block:: text

   This presentation style is used for shell commands, code snippets, directory paths, and similar content.

Text rendered as ``AaBbCc123`` typically refers to variables in scripts or the names of files or directories.

**Bold text** highlights important terms or commands. 

.. note::
   
   This presentation style indicates important clarifications or items require special attention.

.. warning::
      
   This presentation style indicates critical information that must be carefully reviewed.

*Italic text* is used for optional concepts or secondary explanations. 

Code that includes angle brackets (e.g., ``build_<component>.sh``) indicates that users should substitute the appropriate options or components for their GW configuration (e.g., ``build_gfs_utils.sh``).

File or directory paths that begin with ``/path/to/`` or with variables such as ``$file or $directory`` should be replaced with the actual path on the user's system. For example, ``/path/to/logs/build_ufs_utils.log`` or ``$log_directory`` might be replaced with ``/Users/Jane.Smith/global-workflow/sorc/logs/build_ufs_utils.log``.


===========================================
GW User Support and Community Contributions
===========================================

^^^^^^^^^
Questions
^^^^^^^^^

The `global-workflow GitHub Discussions <https://github.com/NOAA-EMC/global-workflow/discussions>`_ provides online support for UFS users and developers to post questions and exchange information. When users encounter difficulties running the workflow, this is the place to post. Users can expect an initial response within two business days.

When posting a question, it is recommended that users provide the following information:

* The platform or system being used (e.g., Orion, Hercules, Derecho)
* The version of the GW being used (e.g., develop, release/public-vx.y.z). (To determine this, users can run git branch, and the name of the branch with an asterisk * in front of it is the name of the branch they are working on.) Note that the version of the application being used and the version of the documentation being used should match, or users will run into difficulties.   Stage of the application when the issue appeared (i.e., configuration, build/compilation, or name of a workflow task)
* Configuration file contents (e.g., config.yaml contents)
* Full error message (preferably in text form rather than a screenshot)
* Current shell (e.g., bash) and modules loaded
* Compiler + MPI combination being used

^^^^^^^^^^^
Bug Reports
^^^^^^^^^^^

If users (especially new users) believe they have identified a bug in the system, it is recommended that they first ask about the problem in GitHub Discussions, since many "bugs" do not require a code change/fix - instead, the user may be unfamiliar with the system and/or may have misunderstood some component of the system or the instructions, which is causing the problem. Asking for assistance in a GitHub Discussion post can help clarify whether there is a simple adjustment to fix the problem or whether there is a genuine bug in the code. Users are also encouraged to search open issues to see if their bug has already been identified. If there is a genuine bug, and there is no open issue to address it, users can report the bug by filing a GitHub Issue.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Feature Requests and Enhancements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Users who want to request a feature enhancement or the addition of a new feature can file a GitHub Issue and add (or request that a code manager add) the EPIC Support Requested label. These feature requests will be forwarded to the Earth Prediction Innovation Center (EPIC) management team for prioritization and eventual addition to the GW repository.

^^^^^^^^^^^^^^^^^^^^^^^
Community Contributions
^^^^^^^^^^^^^^^^^^^^^^^

The UFS community is encouraged to contribute to the development efforts of all related utilities, model code, and infrastructure. As described above, users can post issues in the GW to report bugs or to announce upcoming contributions to the code base. Contributions to the `global-workflow GitHub repository <https://github.com/NOAA-EMC/global-workflow>`_  should follow the guidelines contained in the GW Contributor's Guide. Additionally, users can file issues in component repositories for contributions that directly concern those repositories. For code to be accepted into a component repository, users must follow the code management rules of that component's authoritative repository. These rules are usually outlined in the component's User's Guide (see Table 1.1) or GitHub wiki for each respective repository.

^^^^^^^^^^^^^^^^^
Future Directions
^^^^^^^^^^^^^^^^^

Users can expect to see incremental improvements and additional capabilities in upcoming releases of the GW to enhance research opportunities and support operational forecast implementations.
