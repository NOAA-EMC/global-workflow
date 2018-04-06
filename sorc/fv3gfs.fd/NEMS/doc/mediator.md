Mediator Reference {#mediator}
=======================

\version This page is updated for post-DREV73964 revisions.

This page describes the main NEMS mediator. There is also a 
\ref sw_mediator "NEMS space weather mediator".

In the NUOPC Layer architecture, the mediator (often called the
coupler) handles the scientific and customized aspects of coupling for
a modeling system.  For example, it may handle transfer of coupling
fields, regridding, merging, treatments of coastlines or custom field
transformations.

For mediator diagrams and additional description, see the 
[NEMS mediator overview presentation (Craig, Feb. 6 2015)] (https://esgf.esrl.noaa.gov/site_media/projects/couplednems/pres_1502_NEMS_mediator.pdf)

Overview
--------

In NEMS, the mediator is a separate gridded component with multiple
phases.  Phases can be thought of as different subroutines within the
mediator that can be called in some sequence to carry out the work of
the mediator.  The mediator runs on its own set of PETs (persistent
execution threads, similar to processors).  Often the PETs chosen for
the mediator overlap with other components to optimize resource
utilization.  The PETs and run sequence are specified by the compset.
Mediator phases might have names like prep_atm, prep_ocn, restart,
accumulate, and so forth.  The way the mediator phases are implemented
is somewhat arbitrary and will probably evolve over time as sequencing
requirements change.

Mediator PE layout
------------------

The mediator PETs are set in NEMS in the nems.configure file.  Other
mediator attributes are also defined there in a section that looks
like this,

    MED_model:                         nems
    MED_petlist_bounds:             76 135
    MED_attributes::
      Verbosity = 0
      DumpFields = true
      DumpRHs = false
      coldstart = false
      restart_interval = 0
    ::

Components
----------

The main NEMS mediator is capable of technically coupling atmosphere,
ocean, sea ice, land, hydrology, and wave components. All of these
component types have demonstrated techically correct passage of fields
through the mediator. Only the behavior of atmosphere, ocean, and sea
ice components has been examined to assess correct physical coupling.

All components do not have to be present to operate the NEMS mediator.

Exchange Fields
---------------

The mediator and all components advertise fields during
initialization.  NUOPC reconciles those field names and then coupling
fields are realized and connected.  In order for a field to be
connected between two components, it has to be advertised in the two
components as well as in the mediator. Field names that match between
components are automaticallly coupled in the system.  Any field that
is exported from one component and imported to another component with
the same standard name is coupled between those components.

Separately, there is an ability for the mediator to customize the
coupling interactions via merges or custom calculations where coupling
fields are derived on the fly from each other.  This is not done
automatically and require implementation of the custom calculations in
the mediator.  Typically, these are done in the prep phases.

Coupling Periods
----------------

There is a slow and a fast coupling period in the NEMS mediator.  The
slow coupling period is associated with the ocean model and allows the
ocean model to be coupled at a lower frequency than other components.
The fast coupling period is for the atmosphere and ice model.  They
are coupled at the same frequency in the system.

Accumulation and Averaging
--------------------------

The mediator accumulates all fields between coupling periods for all
components.  For example, the atmosphere and ice coupling fields are
accumulated and averaged between calls to the ocean model.  At the
same time, the ocean fields coupled to the atmosphere and ice models
are held static between the longer ocean coupling periods.

Grids
-----

Model grids are passed to the mediator at initialization.  The
mediator receives those grids and instantiates a decomposition of
those grids on its PETs (persistent execution threads, similar to
processors).  The mediator is then able to receive all model fields on
the native model grids.

The ocean and ice components will generally be run on the same grid
for science reasons, but the mediator is implemented such that this is
not a requirement.

Interpolation (Regridding)
--------------------------

Regridding is performed by the mediator. The regridding weights are
computed at initialization and depend on the grids and regridding
method.  The regridding method is defined in the mediator on a
field-by-field basis when the field is defined. In general, fluxes are
mapped conservatively, and states are mapped bilinearly. The higher
order finite element patch method was not used for any fields because
of an observed reproducibility issue.

In the current revision, fields transferred from the ocean to the sea
ice component are copied rather than interpolated. This can be done
because the ocean and sea ice components are on the same grid. When
different grids are detected for these components, the interpolation
method defaults to bilinear.

Run Sequence
------------

The run sequence is evolving as new requirements are defined.  There
are several mediator phases currently implemented in the mediator and
a typical UGCS-Seasonal run sequence is set up as follows as shown in
a typical nems.configure file,

    runSeq::
     @1800.0
       MED MedPhase_prep_ocn
       MED -> OCN :remapMethod=redist
       OCN
       @900.0
         MED MedPhase_prep_ice
         MED MedPhase_prep_atm
         MED -> ATM :remapMethod=redist
         MED -> ICE :remapMethod=redist
         ATM
         ICE
         ATM -> MED :remapMethod=redist
         ICE -> MED :remapMethod=redist
         MED MedPhase_atm_ocn_flux
         MED MedPhase_accum_fast
       @
       OCN -> MED :remapMethod=redist
       MED MedPhase_write_restart
     @
    ::

In the file above, the sequence of component run calls (ie. OCN, ICE,
ATM), field coupling via connectors (ie. ATM to MED), and mediator
phases (ie. \c "MedPhase_prep_ice", \c "MedPhase_atm_ocn_flux",
MedPhase_write_restart) are indicated.  The coupling periods are also
defined (ie. \@1800.0, \@900.0) for the ocean and atmosphere/ice models.

The current implementation of the mediator phases does the following,

\c MedPhase_prep_ocn - prepares the ocean coupling fields for the ocean
model by averaging the atm/ice coupling fields, interpolating fields
to the ocean grid, computing any merges or custom field calculations,
and then filling the ESMF State sent to the ocean model.

\c MedPhase_prep_atm - prepares the atmosphere coupling fields for the
atmosphere model by interpolating fields to the atmosphere grid,
computing any merges or custom field calculations, and then filling
the ESMF State sent to the atmosphere model.

\c MedPhase_prep_ice - prepares the sea ice coupling fields for the sea
ice model by interpolating fields to the sea ice grid, computing any
merges or custom field calculations, and then filling the ESMF State
sent to the sea ice model.

\c MedPhase_atm_ocn_flux - computes the atmosphere/ocean fluxes from
atmosphere and ocean fields.  The computation is done on the ocean
grid and data from the atmosphere model are interpolated to the ocean
grid.  These fluxes can be used in the atmosphere and ocean model.

\c MedPhase_accum_fast - accumulates the atmosphere and ice coupling
fields for coupling to the ocean model.

\c MedPhase_write_restart - writes mediator restart files.

Reconciliation of Masks
-----------------------

The land mask implementation is described in more detail here.

### Exchange Field Sign and Direction Conventions

The NEMS mediator uses the convention that heat/water/momentum flux is
positive downward. There is also a hierachy of "down" with respect to
models, which from top to bottom is:

 * atm
 * lnd
 * rof
 * ice
 * ocn

If a flux in the coupler is positive, that means it's transferring
heat/water/momentum downward from a higher component to a lower
component.

Some examples:

 * Precip will always be positive from the atm->lnd/ocn/ice

 * Downward shortwave will always be positive from the atm->lnd/ocn/ice

 * Evap will always be negative from the lnd/ocn/ice->atm, that means
   water is transferred from the surface models to the atmosphere.

 * Precip+runoff will always be positive from \c "atm->lnd->rof->ocn"

 * Atm/ocn fluxes are computed in the mediator and are positive into
   the ocean.  So, the same sign of fluxes is passed to both the atm
   and ocean.  For the ocean, positive means heat/water/momentum into
   the ocean and for the atm, positive means heat/water/momentum out
   of the atm.

 * The ice computes atm/ice stresses and ocn/ice stresses.  The
   stresses it computes are stresses on the ice model.  So to meet the
   convention, it has to pass out the atm/ice stress and -ocn/ice
   stress because the sign convention says the flux in the coupler is
   the stress on the ice from the atm and the stress on the ocn from
   the ice.

Models have to adhere to this convention for fluxes that are exported.  They also have to be aware of this convention for fluxes that are imported. 

This sign convention has some problems.  For instance, if the atm/ice
and ocn/ice fluxes were computed OUTSIDE the ice model and passed in
as a merged field, the sign convention would break down.  The sign
convention in NEMS can be changed in the future but a standard has to
be defined.  Custom Field Derivations

There is a section of the mediator that allows for custom coupling
interactions.  In general, coupling fields can be derived and set
there.  This can be used to derive fields from other fields or to
change signs or units.  Custom calculations are used to derive the
downward solar to the ocean component from atmosphere shortwave and
albedo fields.

Flux Field Treatment
--------------------

The NEMS system couples heat, water, and momentum fluxes.  In general,
fluxes are computed at the interface between two components and those
fluxes are often computed in one of the two components or in the
mediator.  Fluxes are normally interpolated conservatively, but the
best regridding method for a particular application is a science
issue.

The mediator is able to compute fluxes as well as regrid and couple
fluxes computed in components.  There is no specific constraint on how
or where the fluxes should be computed.  Often the choice depends on
the relative coupling frequency, the amount of information required to
be coupled to compute the fluxes, the sophistication of the flux
calculation, the relative grid resolution, and whether an exchange
grid is used.

In NEMS, fluxes are computed in a number of places currently.  The
atmosphere model currently computes the atmosphere/ocean,
atmosphere/ice, and atmosphere/land fluxes.  The sea ice model
computes atmosphere/ice fluxes and ocean/ice fluxes.  The mediator is
able to compute atmosphere/ocean fluxes.  Normally, it's important for
the same flux to be used in the components associated with the flux
interface and for the fluxes to be computed in one place, but this is
not a requirement.  Again, this is a science issue.

For some components, fluxes need to be merged.  For instance, on the
atmosphere grid, the ice fraction might evolve in time and the land
fraction might be static.  For conservation, the merging of fluxes is
as important as the regridding and reuse of fluxes.  Again, this is a
science issue.

To compute fluxes,

 * a specific flux method and implementation have to exist or be developed

 * the required coupling fields to compute the flux have to be
   interpolated and passed into the flux calculation

 * the computed fluxes have to be interpolated and passed to the relevant components

 * there needs to be some coordination between components and the
   mediator about what fields are coupled and where computations are
   carried out

In the current implementation, the mediator interpolates the time
evolving ice fraction from the sea ice model and that is sent to the
atmosphere model as a coupling field.  The sea ice model computes both
atmosphere/ice fluxes and ice/ocean fluxes.  The mediator merges the
atmosphere/ocean fluxes computed in the mediator and the ice/ocean
fluxes using the ice fraction and passes those fields to the ocean
model. The mediator also merges the atmosphere/ice fluxes computed in
the ice model and the atmosphere/ocean fluxes computed in the mediator
and sends those fluxes to the atmosphere model.  The atmosphere model
receives the merged atmosphere/ocean and atmosphere/ice fluxes and ice
fraction and then somehow merges that with the atmosphere/ocean fluxes
and atmosphere/land fluxes computed within the gsm.

In particular, the fluxes that are computed at the interface are the
latent and sensible heat flux, evaporation, momentum stress, and
upward longwave flux.  The atmosphere computes the downward longwave,
downward shortwave, and precipitation.  The albedos are also critical
in the computation of the shortwave as it's the net shortwave that has
to match across the flux interface.  To compute these fluxes in
various components, states from the atmosphere, ocean, and sea ice
models such as SST, ocean currents, surface temperature and humidity,
density, surface winds and others are coupled between the relevant
models.

In the current implementation, the atmosphere/ocean flux in the
mediator is computed on the ocean grid.  But again, the location of
these computations and the appropriate grid is a science issue.
Fluxes are computed in the components on their native grid.
Generally, the merges occur on the destination grid after
interpolation.  All coupling is explicit in the current implementation
but implicit coupling may be desired in the future.

The atmosphere/ice and atmosphere/ocean fluxes are merged in the
mediator as follows.  First, we interpolate the ice fraction and
fluxes onto the atmosphere grid using interpolation weights generated
at initialization,

    F_ice2atm_a = WM_i2a * F_ice2atm_i
    F_ocn2atm_a = WM_i2a * F_ocn2atm_i
    ice_fraction_a = WM_i2a * ice_fraction_i

Then In the mediator, the atmosphere/ice and atmosphere/ocn fluxes are
merged such that:

    F_iceocn2atm_a = ice_fraction_a*F_ice2atm_a + (1-ice_fraction_a)*F_ocn2atm_a

That flux is passed to the atmosphere model.  In the atmosphere model,
the land flux, ice flux, and ocn flux are then merged again,

    F_atm_a = ocn_fraction_a * F_iceocn2atm_a + (1 - ocn_fraction_a) * F_lnd2atm_a

where ocn_fraction_a is the clipping destination fraction associated
with the mapped ocean mask.

While this approach is efficient and simple to implement, this suffers
from land-ocean masking problem when land and ocean have different
resolution. One approach to resolve the masking problem is to
introduce an exchange grid where the ambiguous or unclaimed
intersection cells between the land and ocean grids can have exclusive
ownership.  

\todo (NOTE: But I thought we were explicitly specifying the land mask
as the complement of the ocean mask to avoid any missing areas?)

Field Merging
-------------

The mediator contains a generic merge method that allows multiple
fields to be merged together into a destination field. The source
field(s) can be weighted by fractions or other arrays.  The merging
method is used to convert mean_laten_heat_flux to mean_evap_rate in
the atmosphere to ocean coupling.  It is also used to apply fraction
weighting to merge atm and/or ice fields for the ocean component for
\c mean_prec_rate, \c mean_fprec_rate, \c mean_evap_rate, \c mean_sensi_heat_flx,
\c mean_laten_heat_flx, \c mean_down_lw_flx, \c mean_zonal_moment_flx, and
\c mean_merid_moment_flx.

The following are the field merge calls in the mediator:

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_evap_rate' , &
                                is%wrap%FBAtm_o, 'mean_latent_heat_flux' ,customwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_prec_rate' , &
                                is%wrap%FBAtm_o, 'mean_prec_rate' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_fprec_rate' , &
                                is%wrap%FBAtm_o, 'mean_fprec_rate' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_evap_rate' , &
                                is%wrap%FBAtm_o, 'mean_evap_rate' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_sensi_heat_flx' , &
                                is%wrap%FBAtm_o, 'mean_sensi_heat_flx' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_laten_heat_flx' , &
                                is%wrap%FBAtm_o, 'mean_laten_heat_flx' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_down_lw_flx' , &
                                is%wrap%FBAtm_o, 'mean_down_lw_flx' ,atmwgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_zonal_moment_flx' , &
                                is%wrap%FBAtm_o, 'mean_zonal_moment_flx'  ,atmwgt, &
                                is%wrap%FBIce_o, 'stress_on_air_ice_zonal',icewgt, rc=rc)

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_merid_moment_flx' , &
                                is%wrap%FBAtm_o, 'mean_merid_moment_flx'  ,atmwgt, &
                                is%wrap%FBIce_o, 'stress_on_air_ice_merid',icewgt, rc=rc) 