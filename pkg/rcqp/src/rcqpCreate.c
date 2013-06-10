/* ===========================================================================
* File: "rcqpCreate.c"
*                        Created: 2012-10-06 18:49:02
*              Last modification: 2012-10-06 12:02:10
* Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
*          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
* (c) Copyright: 2011-2012
* All rights reserved.
* ===========================================================================
*/

#include "rcqp.h"

// Prototypes of redefined main() functions
int R_cwb_compress_rdx(char *corpus_name, char * registry_dir);
int R_cwb_makeall(char *corpus_name, char* registry);
int R_cwb_huffcode(char *corpus_name, char *registry_dir);
int main_cwb_encode(int argc, char **argv);

/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqpUtils_cwb_encode(SEXP args)" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
SEXP rcqpCreate_cwb_encode(SEXP inArgs)
{
  SEXP result = R_NilValue;
  int nbrArgs;

  if (!isString(inArgs)) error("invalid args");
  nbrArgs = length(inArgs);

  PROTECT(inArgs);

  char *a[nbrArgs];
  for (int i=0; i<nbrArgs; i++) {
    a[i] = (char*)CHAR(STRING_ELT(inArgs,i));
  }

  //main_cwb_encode(int argc, char **argv)
  int res = main_cwb_encode(nbrArgs, a);
  result = PROTECT(allocVector(INTSXP, 1));
  INTEGER(result)[0] = res;

  UNPROTECT(2);

  return result;
}

/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqpUtils_cwb_makeall(SEXP args)" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
SEXP rcqpCreate_cwb_makeall(SEXP inArgs)
{
  SEXP result = R_NilValue;
  int nbrArgs;

  if (!isString(inArgs)) error("invalid args");
  nbrArgs = length(inArgs);

  PROTECT(inArgs);

  char *a[nbrArgs];
  for (int i=0; i<nbrArgs; i++) {
    a[i] = (char*)CHAR(STRING_ELT(inArgs,i));
  }

  //main_cwb_encode(int argc, char **argv);
  //int res = main_cwb_makeall(nbrArgs, a);
  //R_cwb_makeall(char *corpus, char* registry);
  int res = R_cwb_makeall(a[0], a[1]);

  result = PROTECT(allocVector(INTSXP, 1));
  INTEGER(result)[0] = res;

  UNPROTECT(2);

  return result;
}

/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqpUtils_cwb_huffcode(SEXP args)" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
SEXP rcqpCreate_cwb_huffcode(SEXP inArgs)
{
  SEXP result = R_NilValue;
  int nbrArgs;

  if (!isString(inArgs)) error("invalid args");
  nbrArgs = length(inArgs);

  PROTECT(inArgs);

  char *a[nbrArgs];
  for (int i=0; i<nbrArgs; i++) {
    a[i] = (char*)CHAR(STRING_ELT(inArgs,i));
  }

  Rprintf("Corpus...: %s\n", a[0]);

  //int res = main_cwb_huffcode(nbrArgs, a);
  int res = R_cwb_huffcode(a[0], a[1]);
  result = PROTECT(allocVector(INTSXP, 1));
  INTEGER(result)[0] = res;

  UNPROTECT(2);

  return result;
}

/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqpUtils_cwb_compress_rdx(SEXP args)" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
SEXP rcqpCreate_cwb_compress_rdx(SEXP inArgs)
{
  SEXP result = R_NilValue;
  int nbrArgs;

  if (!isString(inArgs)) error("invalid args");
  nbrArgs = length(inArgs);

  PROTECT(inArgs);

  char *a[nbrArgs];
  for (int i=0; i<nbrArgs; i++) {
    a[i] = (char*)CHAR(STRING_ELT(inArgs,i));
  }

  //int res = main_cwb_compress_rdx(nbrArgs, a);
  int res = R_cwb_compress_rdx(a[0], a[1]);
  result = PROTECT(allocVector(INTSXP, 1));
  INTEGER(result)[0] = res;

  UNPROTECT(2);

  return result;
}

// /* 
//  * ------------------------------------------------------------------------
//  * 
//  * "corpus_create()" --
//  * 
//  * This function is automatically called by R when the package is loaded.
//  * 
//  * ------------------------------------------------------------------------
//  */
// void
// corpus_create(SEXP inCorpusdir, SEXP inRegistryfile, SEXP inputfiles, SEXP inPattributes, SEXP inSattributes)
// {
// 	SEXP			result = R_NilValue;
// 	char * corpusdir;
// 	char * registryfile;
// 	int nbr_p_attribute, nbr_s_attribute;
// 
// 	if (!isString(inCorpusdir) || length(inCorpusdir) != 1) error("argument 'inCorpusdir' must be a string");
// 	PROTECT(inCorpusdir);
// 	if (!isString(inRegistryfile) || length(inRegistryfile) != 1) error("argument 'inRegistryfile' must be a string");
// 	PROTECT(inRegistryfile);
// 	if (!isString(inputfiles)) error("argument 'inputfiles' must be a string");
// 	PROTECT(inputfiles);
// 	if (!isString(pattributes)) error("argument 'pattributes' must be a string");
// 	PROTECT(pattributes);
// 	if (!isString(sattributes)) error("argument 'sattributes' must be a string");
// 	PROTECT(sattributes);
// 
// 	corpusdir = (char*)CHAR(STRING_ELT(inCorpusdir,0));
// 	registryfile = (char*)CHAR(STRING_ELT(inRegistryfile,0));
// 
// 	nbr_p_attribute = length(inPattributes);
// 	nbr_s_attribute = length(inSattributes);
// 
// 	for (i=0; i<nbr_s_attribute; i++) {
// 	  str = (char*)CHAR(STRING_ELT(inSattributes,i))
// 	}
// 	
// }
// 
// /* 
//  * ------------------------------------------------------------------------
//  * 
//  * "rcqpImportDataFrame()" --
//  * 
//  * This function allows for creating a CWB corpus with a data frame
//  * 
//  * ------------------------------------------------------------------------
//  */
// SEXP
// rcqpImportDataFrame(SEXP corpus, SEXP positional, SEXP structural, SEXP structuralValues)
// {
// 
//     if (!isFrame(corpus)) {
//         error("argument 'corpus' must be a data frame");
//     }
// 
// 	PROTECT(corpus);
//     
// 	if (!isVector(positional)) {
//         error("argument 'positional' must be a vector of integers");
//     }
// 	PROTECT(positional);
// 
//     if (!isVector(structural)) {
//         error("argument 'structural' must be a vector of integers");
//     }
// 	PROTECT(structural);
// 
//     if (!isVector(structuralValues)) {
//         error("argument 'structuralValues' must be a vector of integers");
//     }
// 	PROTECT(structuralValues);
// 
//     
//     
// 	a = (char*)CHAR(STRING_ELT(inAttribute,0));
// 	len = length(inIds);
// 	
// 	attribute = cqi_lookup_attribute(a, ATT_POS);
// 	if (attribute == NULL) {
// 		UNPROTECT(2);
// 		rcqp_error_code(cqi_errno);
// 	} else {
// 		result = PROTECT(allocVector(STRSXP, len));	
// 		
// 		for (i=0; i<len; i++) {
// 			idx = INTEGER(inIds)[i];	
// 			str = cl_id2str(attribute, idx);
//             // Sends "" if str == NULL (cpos out of range)
// 			if (str != NULL) {
// 				SET_STRING_ELT(result, i, mkChar(str));
// 			} 
// 		}
// 	}
// 	
// 	UNPROTECT(3);
//     
// 	return result;
// 
// }
// 
// 
// ##include <Rdefines.h>
// #
// #SEXP df_fun(SEXP df)
// #{
// #  int i, len = Rf_length(df);
// #  SEXP result;
// #  PROTECT(result = NEW_CHARACTER(len));
// #  for (i = 0; i < len; ++i)
// #    switch(TYPEOF(VECTOR_ELT(df, i))) {
// #      case INTSXP:
// #	SET_STRING_ELT(result, i, mkChar("integer"));
// #	break;
// #      case REALSXP:
// #	SET_STRING_ELT(result, i, mkChar("numeric"));
// #	break;
// #      default:
// #	SET_STRING_ELT(result, i, mkChar("other"));
// #	break;
// #    };
// #  UNPROTECT(1);
// #  return result;
// #}
// #and then after R CMD SHLIB df_fun.c
// #
// #> dyn.load("df_fun.so")
// #> df=data.frame(x=1:5, y=letters[1:5], z=pi, stringsAsFactors=FALSE)
// #  > .Call("df_fun", df)
// #  [1] "integer" "other"   "numeric"
// #  Use GET_CLASS, GET_ATTR and other macros in Rdefines.h (or their equivalent functions, like getAttrib) to discover other information about the data frame. Note though that a data.frame has an API that can differ from its structure. So for instance the R function row.names can return something different from the value stored in the row.names attribute. I think most .Call functions operate on atomic vectors, keeping the manipulation of more complicated objects at the R level.
