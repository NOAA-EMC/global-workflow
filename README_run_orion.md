# Steps to setup and run a free-forecast mode uncoupled atmos-only test on Orion (lack of HPSS, staged C192 ICs)

## 1. Clone/Build/Link

```
$ git clone -b feature/coupled_sprint https://github.com/NOAA-EMC/global-workflow.git
```

While in clone /sorc folder:

```
$ sh checkout.sh
$ sh build_all.sh
$ sh link_workflow.sh emc $MACHINE
```

...where $MACHINE is "cray", "dell", "hera", "orion", "jet", or "stampede" 

## 2. Setup free-forecast mode (single cycle)

While in clone ush/rocoto folder:

```
$ ./setup_expt_fcstonly.py --pslot $PSLOT --res 192 --idate 2020090118 --edate 2020090118 --comrot $PATH_TO_STMP_COMROT --expdir $PATH_TO_EXPDIR_SPACE --gfs_cyc 4 --start cold --configdir $PATH_TO_CLONE/parm/config
$ ./setup_workflow_fcstonly.py --expdir $PATH_TO_GENERATED_EXPDIR
```

...where $PSLOT is the name of your experiment. The generated $EXPDIR will be built with what you pass with "--expdir" plus $PSLOT. Your $COMROT is what you pass with "--comrot" plus $PSLOT.

## 3. Set up COMROT

```
$ cd $COMROT
$ mkdir -p gfs.20200901/18/atmos
$ cd gfs.20200901/18/atmos
$ ln -s /work/noaa/stmp/kfriedma/ICs/2020090118_C192C96L127/gdas.20200901/18/atmos/INPUT INPUT
```

(^ you are technicaly cheating here and symlinking to gdas cold start C192 ICs, it works)

## 4. Start the experiment

```
$ cd $EXPDIR
$ rocotorun -d $DB_FILE -w $XML_FILE
```

When you run rocotorun the gfsfcst job should submit since its dependency is met because you staged ICs. The gfsgetic job will not be there (disabled on Orion). The gfsinit job should not run since its dependencies (ICs pulled by getic in COMROT) are not there. You can rocotocomplete the gfsinit job to be safe.
