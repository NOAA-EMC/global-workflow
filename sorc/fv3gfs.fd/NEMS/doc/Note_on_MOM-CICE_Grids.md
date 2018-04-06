Note on MOM-CICE Grids {#Note_on_MOM-CICE_Grids}
======================

This page is up-to-date with respect to the version: 
\ref milestone_DREV58214

There have been several discussions about the ice and ocean grids.
Several have suggested that the ice and ocean grids should generally
be the same.  As part of that effort, a CICE grid file was created for
the 360x200 MOM5 grid. When the models pass the grids to the mediator,
the masks are identical, but the grids are not.  A closer examination
was carried out with a conclusion that the differences are small and
probably expected.

These are ~1 degree grids, 360x200.  Both center and corner
information is being sent to the mediator.  The ocean and ice grids
sent to the mediator indicate that the corner points are identical to
about single order precision.  The center points are different in the
ocean and ice grids by less than 0.015 degrees which is much, much
less than half a grid box (which would be about 0.5 degrees).

The following apppears to be happening.  The MOM grid file defines
center and corner lons and lats for its grid as a starting point.  The
CICE model is implemented so the corner points are read in and the
center points are computed on the fly internally.  A separate tool was
created that converted the MOM corner points and mask to a CICE grid
file at single precision.  So the fact that the corner points match up
in the mediator between the ocean and ice at single precision is what
we'd expect and is correct.  This could be improved by doing this in
double precision, but it wouldn't have much impact on the quality of
the science or the differences we're seeing in the center points.

After CICE reads in the corner points, it computes center lons and
lats internally.  Those points are different from the MOM values by <
0.015 degrees as mentioned above.  That is a much larger than
roundoff, but is much smaller than the grid dx and is probably pretty
reasonable in this case, Where the MOM and CICE grids are probably NOT
identical is how their grids are defined.  Each model has it's own
idea of the relationship between the corner, the centers, the edge
lengths and other gridcell metrics.  A simple way to think about that
is that one model might place the centers at the average of the
corners and another model might require that the corners be at the
average of the centers.  With non-uniform grids, two grid defined in
this way can never have both matching corner and center lons and lats.
The grid definition is an inherent part of how models define and
discretize the equations.  If this is all true, then what it means is
that MOM and CICE can never have identical values for both centers and
corners for most grids.

In CESM, the POP and CICE grid centers and corners are identical
because they both were developed with the goal of having them both
operate on exactly the same grid.  So that makes life a little easier,
but the differences in the MOM and CICE grid should not be a problem.

So in summary, I believe the way we've generated the 1 degree grid for
CICE is completely reasonable and that the differences we're seeing
between the ocean and ice grids in the mediator, even though the grids
are "the same", are also reasonable.  I think the next question is how
do we want to deal with this in the mediator and that's a science
question. In the current implementation, the mediator treats these as
unique grids and computes mapping weights between the grids.  To a
large degree, the grids are almost exactly overlapping so that mapping
is close to a copy, but not exactly.  I think that's probably the
right way to do it.  We could also choose to force that fields NOT be
mapped between the ocean and ice when the grid sizes are the same and
artificially force the grids to be treated as if they were identical
in the mediator.  That's also viable in this situation but will
generally be less accurate.

From notes by A. Craig.