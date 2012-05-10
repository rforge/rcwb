// ===========================================================================
// File: "rcqp.h"
//                        Created: 2012-01-13 18:49:02
//              Last modification: 2012-01-19 12:12:35
// Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
//          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
// (c) Copyright: 2011-2012
// All rights reserved.
// ===========================================================================


#ifndef	RCQP_H
#define RCQP_H
#pragma once


#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <unistd.h>
#include <math.h>

#include "cwb/CQi/server.h"
#include "cwb/CQi/auth.h"
#include "cwb/CQi/cqi.h"

#include "cwb/cl/cl.h"
#include "cwb/cl/macros.h"
#include "cwb/cqp/cqp.h"
#include "cwb/cqp/options.h"
#include "cwb/cqp/corpmanag.h"
#include "cwb/cqp/groups.h"
	
#include "rcqpCommands.h"
#include "rcqpUtils.h"


#define send_cl_error() rcqp_send_error()
/* #define exit(x) rcqp_exit((x)) */

#endif  // RCQP_H
