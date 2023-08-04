.. role:: red-text

########################################
Configuring NOAA Cloud Service Providers
########################################

The NOAA Cloud Service Providers (CSP) support the forecast-only
configurations for the global workflow. Once a suitable CSP instance
and cluster is defined/created, the global workflow may be executed as
on the other platforms discussed in the previous sections. In order
successfully execute the global-workflow, a suitable CSP cluster must
be created. Currently the global-workflow supports the following
instance and storage types as a function of CSP and forecast
resolution.

.. list-table::
   :widths: auto
   :header-rows: 1
   :align: center

   * - **Cloud Service Provider**
     - **Global Workflow Resolution**
     - **Instance Type**
     - **Partition**
     - **File System**
   * - Amazon Web Services Parallel Works
     - C48
     - ``c4.8xlarge (36 vCPUs, 60 GB Memory, amd64)``
     - ``compute``
     - ``/lustre``

Instructions regarding configuring the respective CSP instance and
cluster follows.

*********************
Login to the NOAA CSP
*********************

Log in to the `NOAA CSP <http://noaa.parallel.works/login>`_ and into
the resources configuration. The user should arrive at the following
screen.

.. image:: _static/noaacsp_login.png

Note that the ``Username or email`` query is case-sensitive. The user
will then be promoted for their respective RSA token key using the
same application that is used for the other RDPHCS machines (i.e.,
Hera, Jet, etc.,).

*******************************
Configure the NOAA CSP Instance
*******************************

Once logged into the NOAA CSP, navigate to the :red-text:`RESOURCES` section
and click the ``+ Add Resource`` button in the upper-right corner.  as
illustrated below.

.. image:: _static/noaacsp_instance_1.png
   
Next, the mandatory attributes for the respective instance must be
defined as shown in the illustration below.

.. image:: _static/noaacsp_instance_2.png

The annotated attributes and their respective descriptions are as
follows.

1. A unique name for the instance. Best practices suggest one that is
   clear, concise, and relavant to the application.
2. A short description of the instance, i.e., ``This instance supports
   this <task name> task.``
3. Tag(s) descripting and identifying the respective instance. These
   allow for improved bookkeeping, especially when a user has multiple
   or concurrent instance types.

Next, the cluster is defined as shown in the following illustration.

.. image:: _static/noaacsp_instance_3.png

The NOAA Parallel Works initiative currently provides 2 CSPs for the
global-workflow; **AWS** (Amazon Web Services) and **Azure**
(Microsoft Azure). Existing clusters may also be modified. However
this is neither recommended or supported.

Finally, when satisfied with the CSP instance configure, click ``Add
Resource`` as illustrated below.

.. image:: _static/noaacsp_instance_4.png

******************************
Configure the NOAA CSP Cluster
******************************

Navigate to the tab and locate the CSP instance configured in the
previous section and click on the link as illustrated below.

.. image:: _static/noaacsp_cluster_1.png

The respective CSP cluster maybe defined

### TODO: Need to create image where cluster is configured.

### TODO: Update _static/noaacsp_instance_1.png such that black boxes
are aligned.
