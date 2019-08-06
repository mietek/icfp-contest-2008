-module(icfp_circles).
-export([circle_circle_intersection/6]).

circle_circle_intersection(X0, Y0, R0, X1, Y1, R1) ->
  DX = X1 - X0,
  DY = Y1 - Y0,

  % Determine the straight-line distance between the centers.
  D = math:sqrt((DY*DY) + (DX*DX)),

  % Check for solvability.
  Abs_dist = abs(R0 - R1),
  if
	D > (R0 + R1) -> {error, separate}; % circles are separate
	D < Abs_dist -> {error, contains}; % one circle contains other

  % 'point 2' is the point where the line through the circle
  % intersection points crosses the line between the circle
  % centers.  
	true ->
		% Determine the distance from point 0 to point 2.
		A = ((R0*R0) - (R1*R1) + (D*D)) / (2.0 * D),

		% Determine the coordinates of point 2.
		X2 = X0 + (DX * A/D),
		Y2 = Y0 + (DY * A/D),

		% Determine the distance from point 2 to either of the
		% intersection points.

		H = math:sqrt((R0*R0) - (A*A)),

		% Now determine the offsets of the intersection points from
		% point 2.
		RX = -DY * (H/D),
		RY = DX * (H/D),

		% Determine the absolute intersection points.
		Xi = X2 + RX,
		Xi_prime = X2 - RX,
		Yi = Y2 + RY,
		Yi_prime = Y2 - RY,

		{Xi, Yi, Xi_prime, Yi_prime}
		end.
