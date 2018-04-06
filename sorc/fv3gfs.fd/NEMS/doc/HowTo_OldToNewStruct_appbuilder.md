Step 2. Updating AppBuilder  {#HowToOldToNewAppBuilder}
======================
 
Step 2.1: 
---------

The next step is to figure out which modules are need for the new
app. The first thing to do is to figure out which module files the
copied app is using. To do this, open the existing *.appBuilder files.
 
    $ ls *.appBuilder
    standaloneGSM.appBuilder  standaloneGSM%gocart.appBuilder
 
For this case there are two choices and the relevant part of the file is: 
 
    case "$FULL_MACHINE_ID" in
        yellowstone|gaea)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_NUOPC
            ;;
        wcoss*|theia)
            CHOSEN_MODULE=$FULL_MACHINE_ID/ESMF_700_gsm
            CONFOPT="gsm_intel_${MACHINE_ID:?}"
            ;;
    esac
     
The variable `CHOSEN_MODULE` corresponds to a file in the
`modulefiles/<FULL_MACHINE_ID>` directory.
 
In this case, we'll use Theia as our first target.  Theia is using
ESMF_700_gsm (therefor the modules listed in
`modulefiles/theia/ESMF_700_gsm`).
 
\note A similar procedure would be used for other platforms.  Note
that wcoss.phase2 is simply a link from wcoss.phase1.
 
Step 2.2:
-------- 

Copy the modulefile used in Step 2.1 to start a new modulefile for your app:

    svn cp modulefiles/theia/ESMF_700_gsm   modulefiles/theia/ESMF_700_gsm_ww3 
 
Step 2.3: 
---------

Compare the modules in your new file (in this case
`modulefiles/theia/ESMF_700_gsm_ww3`) to the module files used in your
old app.  In your old app structure, you can determine which modules
were used by looking in the `*.appBuilder` file in the
`environment_<platform>()` functions.  Update the module file with any
needed modules.  You will need to check for the following things:

The first line must be `#%Module#` This can be followed by additional `#`, eg: 

    #%Module#####################################################################

There should be no source commands, for example: 

    source /etc/profile 

Make sure there are no bash specific code such as export statements. 
Avoid using user installed module files when possible. For example: 

    module use /scratch4/NCEPDEV/nems/save/USER.NAME/Modulefiles
 
\todo These should be put somewhere in documentation about module files and then we 
should link to that location 
 
Step 2.4:  
---------

Check the `*.appBuilder` file to see if there is a variable `CONFOPT`
specified, for example, in the NEMSfv3gfs:

    CONFOPT=configure.fv3.$FULL_MACHINE_ID

This configure file is located in the top level directory conf/.  If
no CONFOPT is specified, the default is to use

    `configure.nems.<platform>.<compiler>`

Most likely, the default configure file is what you will want to use,
but you should confirm that the settings are correct for your
component. In general, these store compilation and link time
variables.
 
To see the current state of the new app, see the following tag: 

* https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/tags/NEMSTutorial/Step11
 
Step 2.5: 
---------

We need to create a new *.appBuilder file.  To do this start by
copying an existing *.appBuilder file that is as similar as possible
to your target application:

    svn copy standaloneGSM.appBuilder coupledGSM_WW3.appBuilder
 
Step 2.6: 
---------

Now you need to update the new `*.appBuilder` file to include your
desired components. For this app, that's adding WW3, which included the
following:
 
1. Added WW3 to the list of components. Note there must be a space
   after the component name and before the ), ie:

       COMPONENTS=( GSM WW3 )
 
2. Add the install and source directories, ie. set the `<APP>_BINDIR`
   and `<APP>_SRCDIR` variables.  You should find the locations of
   these files in your old app structure's `*.appBuilder` file. In
   this case, for WW3:

       # WW3
       WW3_SRCDIR=$ROOTDIR/WW3
       WW3_BINDIR=$ROOTDIR/WW3-INSTALL
 
3. Make sure you are pointing to the correct module files for each
   platform that you have created a new module file for.  In this case,
   we have updated for Theia.


To see the current state, see the tag: 

 * https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/tags/NEMSTutorial/Step13


