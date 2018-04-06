Step 3. Updating Component Branches  {#HowToOldToNewCompBranch}
==============================

 
Step 3.1: 
---------

The next step is to manually merge changes from the old gsm branch to
the new branch created from the trunk. To see the changes made in the
old GSM branch, we can look at the trac page:

 * https://svnemc.ncep.noaa.gov/trac/gsm/changeset?reponame=&new=84116%40branches%2Fmeixner%2Ftwowaywavcoup&old=80525%40trunk
 
Skipping the changes that are simply extra spaces, these changes were
put into the r93335 GSM branch created in Step 6:

 * https://svnemc.ncep.noaa.gov/projects/gsm/branches/NEMSUpdate/UGCSWeather
 
I can confirm that these are in fact the changes I want again using
the trac page:

 * https://svnemc.ncep.noaa.gov/trac/gsm/changeset?reponame=&new=93335%40branches%2FNEMSUpdate%2FUGCSWeather&old=89613%40trunk
 
Step 3.2:
---------

When GSM moved to having a cap like the other components, some files
moved from being stored in the NEMS repository to being stored in GSM,
such as:

    <nems trunk/branch>/src/atmos/share      --->    gsm/trunk/share
    <nems trunk/branch>/src/atmos/post       --->    gsm/trunk/post
 
To see what changes I had made that would need to be merged from the
old version of NEMS to the new branch of GSM, I looked at the trac
page

 * https://svnemc.ncep.noaa.gov/trac/nems/changeset?reponame=&new=84203%40branches%2FUGCS-Seasonal%2FtwoWayWW3_from_r80562&old=80562%40trunk
 
For this example, the only updates were to share/module_CPLFIELDS.F90
These updates are now in the revision 93343 of the fowllowing GSM
branch

 * https://svnemc.ncep.noaa.gov/projects/gsm/branches/NEMSUpdate/UGCSWeather
 
Now all of the GSM updates should be in the new GSM branch.  
 
Step 3.3: 
---------

Now we need to merge the appropriate changes from the old branch of
nems into the new branch of nems.  Again, we can look at the trac page
for what changes were previously made

 * https://svnemc.ncep.noaa.gov/trac/nems/changeset?reponame=&new=84203%40branches%2FUGCS-Seasonal%2FtwoWayWW3_from_r80562&old=80562%40trunk
 
Only modified files that remain in the new nems branch (and are not
files which have been moved to the App level) need to be updated.  In
this case, these files are `NEMSAppBuilder` and
`src/module_EARTH_GRID_COMP.F90`.  Note that although there is still a
file named `NEMSCompsetRun`, this file is not the same as the old
`NEMSCompsetRun` and therefore should not be updated.
 
These changes were checked into r93381 of the follwoing branch of nems

 * https://svnemc.ncep.noaa.gov/projects/nems/branches/NEMSUpdate/UGCSWeather


Step 3.4: 
--------

At this point the changes that remain in your nems branch are either in now
obsolete files or files that moved to the app level. Some files to update on
the app level include: 
  nems/test/nems.configure.*.IN -->  app/parm/nems.configure.*.IN
  nems/test/gsm_config          -->  app/compset/gsm_config
Note there may be more files that you have to update that are not listed here. 


Step 3.5: 
-------- 

If you are using revision numbers in your svn:externals, make sure to update these 
revision numbers to point to the head of the externals (by not specifying a 
revision number).  

