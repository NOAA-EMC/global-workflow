Please use this branch "master-v16" as the FV3 GFS v16 master.
The current master will remain GFS v15.1 and follow any change modified by NCO.
Current master will need to reproduce NCO pre-implementation parallel until FV3 GFS is become operational.

"master-v16" is based on the latest master at commit #d919db638ac4f83604648d571ef36e4798928561
Please create your branch based on "master-v16" and merge your change. The "master-v16" will NOT reproduce NCO GFS v15.x parallel.

EIB global-workflow master (including "master-v16") commit policy:
- Create issue https://vlab.ncep.noaa.gov/redmine/issues/ and summarize your merge purpose and test/verification criteria.
    If your change change result, please include some level of details.
- Synced merge your branch with "master-v16".
- Email let us (Lin/Hang/Kate) know your work is ready for testing.
- We will perform pre-commit testing on supported machines.
- We will perform the final review of the changes with reviewer.
- We will perform commit and post commit tasks.

For any question, please contact Lin Gan, Kate Friedman, Hang Lei
