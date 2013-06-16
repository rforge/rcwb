/* ===========================================================================
 * File: "rcqpCreate.h"
 *                        Created: 2013-06-14 10:53:11
 *              Last modification: 2013-06-14 10:55:03
 * Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
 *          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
 * ===========================================================================
 */


#ifndef	RCQP_CREATE_H
#define RCQP_CREATE_H
#pragma once



/* Prototypes
 * ----------
 */

SEXP rcqpCreate_cwb_compress_rdx(SEXP inArgs);

SEXP rcqpCreate_cwb_encode(SEXP inArgs);

SEXP rcqpCreate_cwb_huffcode(SEXP inArgs);

SEXP rcqpCreate_cwb_makeall(SEXP inArgs);

SEXP rcqpCreate_re_rcqpinitialize_cqp(SEXP inArgs);


// Prototypes of redefined main() functions
int R_cwb_compress_rdx(char *corpus_name, char * registry_dir);
int R_cwb_makeall(char *corpus_name, char* registry);
int R_cwb_huffcode(char *corpus_name, char *registry_dir);
int main_cwb_encode(int argc, char **argv);


#endif  /* RCQP_CREATE_H */

