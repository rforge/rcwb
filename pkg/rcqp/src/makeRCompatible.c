/* ===========================================================================
* File: "makeRCompatible.c"
*                        Created: 2012-01-13 18:49:02
*              Last modification: 2013-06-14 10:36:44
* Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
*          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
* (c) Copyright: 2011-2013
* All rights reserved.
* ===========================================================================
*/

#include "rcqp.h"
#include "makeRCompatible.h"


void rcqp_receive_error(int x)
{
/*	error("cqp error #%d\n", cderrno); */
	Rprintf("Error! Please close and restart R as rcqp may be in unknown state");
	
}

int rcqp_flush() {
  return(1);
}
