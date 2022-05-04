PRELUDE
-------

A realtime ecflow workflow is created for developer in WCOSS2 system (Cactus/Dogwood).

INSTALLATION INSTRUCTIONS
---------------------------

Following steps are to help developer install and config ecflow workflow for realtime cycled parallel run

To do ecflow suite installation, step:
```bash
    cd ${HOMEgfs}/ecf/defs
    ecflow_client --load cycled_gfs.def
      (Install GFS v16 realtime ecflow into ecflow server)
  ecflow environment setup step:
    ecflow_client --alter add variable PDY ${PDY_FOR_YOUR_PARALLEL} /cycled_gfs/primary/00 /cycled_gfs/primary/06 /cycled_gfs/primary/12 /cycled_gfs/primary/18
      (This initialize the starting PDY of the parallel; where ${PDY_FOR_YOUR_PARALLEL} is the PDY of the cycle)
    ecflow_client --alter add variable EMC_USER $USER /cycled_gfs
      (This initialize the $USER)
    ecflow_client --alter add variable PSLOT ${YOUR_PSLOT} /cycled_gfs
      (This initialize the PSLOT)
    ecflow_client --alter add variable EDATE ${YOUR_EDATE} /cycled_gfs
      (This initialize the EDATE)
    ecflow_client --alter add variable ECF_INCLUDE ${HOMEgfs}/ecf/include /cycled_gfs
      (This initialize the ecflow workflow configuration for developer)
    ecflow_client --alter add variable OUTPUTDIR ${YOUR_OUTPUT_LOCATION} /cycled_gfs
      (This initialize the jobs log output location)
```

To prepare the COM:

* Ensure the HOMEgfs is built and linked.
* Ensure ecflow job scripts are linked: ${HOMEgfs/ecf/setup_ecf_links.sh)
