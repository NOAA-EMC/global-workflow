Step 1. Setting up new folder Structure  {#HowToOldToNewFolder}
==================================
 
Step 1.1:
--------

Determine where your app is going.  In this case, we are moving the
app from WW3TestBed to the UGCSWeather App in a branch called
UpdateStructure.  Most likely in your case, you will just be making a
branch called UpdateStructure in your current App directory.
 
Step 1.2:
----------

Determine which app in the updated structure most closely resembles
your app. To see the status of NEMS Apps go to

 * https://vlab.ncep.noaa.gov/web/environmental-modeling-center/nems-applications

If you have a GSM based app, this app is the NEMSGSM app

 * https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSGSM/trunk/
 
Step 1.3:
---------- 

Create your directory structure. If possible, do this by copying the
reference app trunk.  For us, we will copy the NEMSGSM trunk, so:
 
    svn copy https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSGSM/trunk/ https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/branches/UpdateStructure
 
\todo Make sure all updates put in this branch are in the NEMSGSM trunk now too

Step 1.4:
----------  

Now check out your new app: 
 
    svn co https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/branches/UpdateStructure UpdateStructure 

Step 1.5:
---------- 

Look at the new app structure and externals: 
 
    $ cd UpdateStructure 
    $ ls 
    CHEM      conf    GSM  modulefiles  oldtests  standaloneGSM.appBuilder
    compsets  doc    log  NEMS      parm        standaloneGSM%gocart.appBuilder
    $svn propget svn:externals 
    NEMS     -r92559      https://svnemc.ncep.noaa.gov/projects/nems/trunk
    GSM      -r89107      https://svnemc.ncep.noaa.gov/projects/gsm/trunk
    CHEM     -r85947      https://svnemc.ncep.noaa.gov/projects/aerosol/chem/trunk

At the application level, there are 7 folders and several
`<appconfig>.appBuilder` files. Many things that were previously kept in
NEMS, such as the equivalent of compsets, are now kept at the
application level.  The appBuilder files give information about
components for each application configuration.  A complete description
of an appBuilder file can be found here.

\todo link to appbuilder file description  

The name and contents of each app level folder is below. Note, these
descriptions can also be found here.  \todo link to repository
structure

\todo replace with link to relevant page of compset runner documentation

- conf/ - build configuration files
- compsets/ - definition of compsets for this application
- doc/ - contains application-specific documentation
- modulefiles/ - module files to load external dependencies such as NetCDF
    + theia/ - module files for Theia machine
    + wcoss.cray/ - module files for the Cray partition of WCOSS
    + ... more platform support ...
- parm/ - small parameter files for compsets. Large files are located outside of the repository.
- log/ - subdirectory for log files from execution and compilation. These are placed in the repository log/ directory so that they can be used to track changes, by re-using compsets as regression tests.

\todo The doc folder is not listed in the repository structure 
 
Note that all of the external components are contained in folders with
all caps and that the GSM and CHEM components no longer live in a
subdirectory of NEMS. Now that many application specific items have
been removed from NEMS to the application level, NEMS now contains the
following elements, which are also described 
\ref structure "here".

 
Within NEMS/ resides:

 * NEMSAppBuilder - script to build the NEMS application 
 * NEMSCompsetRun  
 * exe/ - built executables are placed here
 * src/ - NEMS code is located here
 * doc/ - NEMS level documentation.
 * test/ - test execution logic

\todo In my version I also have oldtests (directory of old nems level things that have moved to app level... maybe this folder should be deleted?) and OldCompsetRun (prior version of compset runner)

\todo add description or link to NEMSCcompsetRun (and appbuilder) 

\todo in 
\ref structure "doc is missing"  
  
Step 1.6: 
----------

Make branches from the trunk of the external components that will need to be modified.
For this example, this is GSM and NEMS. Changes to these
branches will be added later.
 
    svn copy https://svnemc.ncep.noaa.gov/projects/gsm/trunk/ \
       https://svnemc.ncep.noaa.gov/projects/gsm/branches/NEMSUpdate/UGCSWeather \
       -m 'Creating branch for updating UGCSWeather to new NEMS'
    svn copy https://svnemc.ncep.noaa.gov/projects/nems/trunk/ \
       https://svnemc.ncep.noaa.gov/projects/nems/branches/NEMSUpdate/UGCSWeather \
       -m 'Creating branch for updating UGCSWeather to new NEMS'
 
Step 1.7: 
----------

Update the externals to include any other components needed in the
App and to point to any new branches made in the
previous step. To do this first set the `$EDITOR`
or `$SVN_EDITOR` environment variable to your preferred editor.  It's
best to add this to your `~/.cshrc` or equivalent file.  Don't forget
to source the file after updating it, with the following changes:
 
    setenv SVN_EDITOR vi

or 

    export SVN_EDITOR=vi
 
Now, update the externals: 
 
    svn propedit svn:externals . 
 
To see the result of this step see the following tag

 *  https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/tags/NEMSTutorial/Step7



