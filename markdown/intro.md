Inverse distance weighted interpolation (IDW) is a common
geostatistical method for determining unknown values at locations
using known values from nearby locations.

Values are interpolated for each location on a grid with a user
defined resolution using the input parameters described above.

* Minimum Points
 * The minimum number of points used to determine the interpolated
   value.

* Maximum Points
 * The maximum number of points used to determine the interpolated
   value.
 * If this is set to 1, interpolated surface will resemble a voronio
   diagram.

* Search Distance
 * The maximum distance from known values used to interpolate a given
   location.

* Power
 * A value used to modulate the effect of distance from points on
   interpolate locations.
 * The lower the power, the more homogenous a given interpolated
   surface will be.
 * The higher the power, the more the interpolated surface will
   resemble a voronio diagram.


