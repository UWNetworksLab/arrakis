/*
 * drem() wrapper for remainder().
 *
 * Written by J.T. Conklin, <jtc@wimsey.com>
 * Placed into the Public Domain, 1994.
 */

#include <math.h>

//asq: transformed to "new" function declaration style
double
drem(double x, double y)
//	double x, y;
{
	return remainder(x, y);
}
