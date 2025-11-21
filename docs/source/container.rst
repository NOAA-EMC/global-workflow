.. role:: red-text

##############################################
Clone, Setup, Compile, and Run Global-Workflow
##############################################

The Singularity Container supports the forecast-only,
coupled, and GEFS configurations for global-workflow.
On selected NOAA on-prem machines, and AWS,
the global-workflow can be compiled, and executed
similarly to the on-premises (on-prem) machines.
Currently, the global-workflow supports the following
on the following machines and forecast resolution.

.. list-table::
   :widths: auto
   :header-rows: 1
   :align: center

   * - **Container Availabe Machines**
     - **Global Workflow Resolution**
     - **Global Workflow Application**
     - **Singularity SIF location**
     - **Binding File System**
   * - Amazon Web Services ParallelWorks
     - C48, C96, C192, C384
     - /contrib/containers
     - ``ATM``, ``GEFS``
     - ``/contrib``, ``/lustre``, ``/bucket``
   * - Ursa
     - C48, C96, C192, C384
     - ``ATM``, ``GEFS``
     - /scratch3/NCEPDEV/nems/role.epic/containers
     - ``/scratch3``, ``/scratch4``, ``/scratch5``
   * - Gaea C6
     - C48, C96, C192, C384
     - ``ATM``, ``GEFS``
     - /gpfs/f6/scratch/Wei.Huang/container
     - ``/gpfs/f6/scratch``, ``/ncrc/home1/${USER}``

Instructions regarding clone, setup, compile, and run Global-Workfile follow.

***************************
Clone the Global Workflow
***************************

#. Clone Global-Workflow from EPIC fork, and switch to the container branch.

   .. code-block:: console

      cd your-source-code-dir   #you should have a username and have a directory at /contrib, where we save our permanent files.
      git clone --recursive git@github.com:NOAA-EPIC/global-workflow-cloud.git global-workflow-cloud
      git checkout feature/use_container_spack-stack-1.9.2

***************************
Compile the Global Workflow
***************************

#. Compile global-workflow:

   .. code-block:: console

      cd your-source-code-dir/global-workflow-cloud
      cd sorc/dev/container

#. Use a script to shell-into container and compile Global-Workflow inside container:
   Save the this script in a file, say, ``shell-in-container.sh``,
   make it executable, and then run it.

   .. code-block:: console

      #!/bin/bash

      set -x

      HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
      source "${HOMEgfs}/ush/detect_machine.sh"
      sif=ubuntu22.04-intel-ufs-env-v1.9.2.img

      if [[ ${MACHINE_ID} = ursa* ]] ; then
         img=/scratch3/NCEPDEV/nems/role.epic/containers/${sif}
         bindings="-B /scratch3 -B /scratch4"
      elif [[ ${MACHINE_ID} = gaea* ]] ; then
         img=/gpfs/f6/scratch/${USER}/container/${sif}
         bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
      elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
         img=/contrib/containers/${sif}
         bindings="-B /contrib -B /lustre -B /bucket"
      fi

      singularity shell -e ${bindings} "${img}"

#. Run link_workflow.sh
   Run link_workflow.sh to link fix data, executables, and many others.

   .. code-block:: console

      cd your-source-code-dir/global-workflow-cloud/sorc
      link_workflow.sh

**********************************
Setup the Global Workflow Run Case
**********************************

#. create a script to setup global-workflow test case
   Save the this script in a file, say, ``gen-run-cases.sh``,
   make it executable, and then run it.

   .. code-block:: console
      #!/bin/bash

      set -x

      HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
      source "${HOMEgfs}/ush/detect_machine.sh"

      run_with_container="YES"
      #run_with_container="NO"

       casetype="pr"
      #yamllist="C48_ATM"
       yamllist="C48_S2SW"
      #yamllist="C48_S2SWA_gefs"
      #yamllist="C96mx100_S2S"

      #casetype=hires
      #yamllist="C768_S2SW"

      HOMEDIR=${HOMEgfs}
      img=ubuntu22.04-intel-ufs-env-v1.9.2.img
      if [[ ${MACHINE_ID} = ursa* ]] ; then
         container=/scratch3/NCEPDEV/nems/role.epic/containers/${img}
         rundir=/scratch3/NAGAPE/epic/${USER}/run
         bindings="-B /scratch3 -B /scratch4"
         HPC_ACCOUNT=epic
      
         module load rocoto/1.3.7
         rocotocmd=$(command -v rocotorun)
      elif [[ ${MACHINE_ID} = gaea* ]] ; then
         container=/gpfs/f6/scratch/Wei.Huang/container/${img}
         rundir=/gpfs/f6/scratch/${USER}/run
         bindings="-B /gpfs/f6/scratch -B /ncrc/home1/${USER}"
         HPC_ACCOUNT=bil-fire8
      
         rocotocmd=/autofs/ncrc-svm1_home2/Christopher.W.Harrop/rocoto-1.3.7/bin/rocotorun
      elif [[ ${MACHINE_ID} = noaacloud* ]] ; then
         TOPICDIR=/bucket/global-workflow-shared-data/ICSDIR
         container=/contrib/containers/${img}
         rundir=/lustre/${USER}/run/container
         bindings="--env \"I_MPI_FABRICS=shm:ofi,I_MPI_DEBUG=6\" -B /apps/slurm/default/lib/libpmi2.so -B /contrib -B /lustre -B /bucket"
        #bindings="-B /apps/slurm/default/lib/libpmi2.so -B /contrib -B /lustre -B /bucket"
         HPC_ACCOUNT=${USER}
      
         module load rocoto/1.3.7
         rocotocmd=$(command -v rocotorun)
      fi
      
      set -x
      
      mkdir -p "${rundir}"
      mkdir -p "${HOMEDIR}"/exec
      mkdir -p "${HOMEDIR}"/ush/container
      
      cd "${HOMEDIR}/dev/workflow" || exit 1
      
      if [[ "${run_with_container}" == "YES" ]]; then
         "${HOMEDIR}/dev/container/utils/gen-wrapper.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}" -v
      
         TOPICDIR=${TOPICDIR} \
         RUNTESTS=${rundir} \
         RUNDIRS=${rundir} \
              ./generate_workflows.sh \
              -H "${HOMEDIR}" \
              -y "${yamllist}" \
              -Y "${HOMEDIR}/dev/ci/cases/${casetype}" \
              -A "${HPC_ACCOUNT}" \
              -e "${USER}@noaa.gov" \
              -r "${rocotocmd}" \
              -v -R
      
         "${HOMEDIR}/dev/container/utils/create-atmos-products.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}"
         "${HOMEDIR}/dev/container/utils/create-container-links.sh" -H "${HOMEDIR}" -c "${container}" -b "${bindings}" -M "${MACHINE_ID}"
      else
         TOPICDIR=${TOPICDIR} \
         RUNTESTS=${rundir} \
         RUNDIRS=${rundir} \
              ./generate_workflows.sh \
              -H "${HOMEDIR}" \
              -y "${yamllist}" \
              -Y "${HOMEDIR}/dev/ci/cases/${casetype}" \
              -A "${HPC_ACCOUNT}" \
              -e "${USER}@noaa.gov" \
              -v
      fi

#. run this script to setup global-workflow test case
   In the above script, we select C48_ATM case.

   .. code-block:: console

      cd your-source-code-dir/global-workflow-cloud/dev/container

      ./gen-run-cases.sh

**********************************
Run the Global Workflow Run Case
**********************************

#. run this global-workflow case
   In the above script, we select C48_ATM case.

   .. code-block:: console

      cd run-case-dir/EXPDIR/C48_ATM
      crontab C48_ATM.crontab

EPIC has copied the C48 and C96 ATM, GEFS, and some other data to AWS,
and EMC has all cases data ready on NOAA on-prem machines.
The current code has been set up to use those data.
If users want to run their own case, they need to make changes to the IC path and others to make it work.
