// ===========================================================================
// File: "rcqpUtils.c"
//                        Created: 2012-01-13 18:49:02
//              Last modification: 2012-01-19 12:02:10
// Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
//          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
// (c) Copyright: 2011-2012
// All rights reserved.
// ===========================================================================
	
#include "rcqp.h"


#define RCQP_ATT_HASH_SIZE 16384


/* 
 * ------------------------------------------------------------------------
 * 
 * "R_init_rcqp()" --
 * 
 * This function is automatically called by R when the package is loaded.
 * 
 * ------------------------------------------------------------------------
 */
void
R_init_rcqp(DllInfo * info)
{
	char		*envregdir, *stdregdir;
	int			ac = 1;
	char *		av[1];
	
	envregdir = getenv("CORPUS_REGISTRY");
	stdregdir= cl_standard_registry();
	
	if (envregdir == NULL) {
		printf("The environment variable CORPUS_REGISTRY is not defined.\n");
		printf("Using default registry '%s'.\n", stdregdir);
		printf("See ?cqp_registry for more info on how to set the registry.\n");
	} else {
		printf("Using registry '%s'.\n", envregdir);
	}
	
	av[0] = "rcqp";
	which_app = cqp;
	silent = 1; 
	paging = 0;
	autoshow = 0;
	auto_save = 0;
	server_log = 0;
	
	initialize_cqp(ac, av);
	make_attribute_hash(RCQP_ATT_HASH_SIZE);
}



/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqp_send_error()" --
 * 
 * This is a replacement for send_cl_error() in order to intercept the
 * exit() call.
 * 
 * ------------------------------------------------------------------------
 */
void rcqp_send_error()
{
	error("cqp error #%d\n", cderrno);
}



/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqp_error_code()" --
 * 
 * 
 * 
 * ------------------------------------------------------------------------
 */
void rcqp_error_code(int inCode)
{
	if (inCode != 0) {
		error("cqp returned error code #%d\n", inCode);
	} 
}



/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqp_query_has_semicolon()" --
 * 
 * Copied from query_has_semicolon in cqpserver.c.
 * CQP queries must be terminated with a single semicolon;
 * multiple semicolons will produce an error to occur -- so we
 * have to check and add a semicolon if necessary.
 * 
 * ------------------------------------------------------------------------
 */
int
rcqp_query_has_semicolon(char *query)
{
  char *p;

  if (query == NULL || *query == 0)
    return 0;
  p = query + strlen(query); 
  while (--p > query)           /* stop at first non-blank char or at first string character */
    if (!(*p == ' ' || *p == '\t')) break;
  return (*p == ';') ? 1 : 0;
}


/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqp_get_attr_type()" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
int
rcqp_get_attr_type(SEXP inType)
{
	char *		type;
	
	if (!isString(inType) || length(inType) != 1) error("type must be a string");
	type = (char*)CHAR(STRING_ELT(inType,0));
	if (!strcmp("a",type)) {
		return ATT_ALIGN;
	} else if (!strcmp("p",type)) {
		return ATT_POS;
	} else if (!strcmp("s",type)) {
		return ATT_STRUC;
	} else {
		error("invalid attribute type. Must be 'a', 'p', or 's'.");
	} 
}


/* 
 * ------------------------------------------------------------------------
 * 
 * "rcqp_get_field_type()" --
 * 
 * 
 * ------------------------------------------------------------------------
 */
FieldType
rcqp_get_field_type(SEXP inField)
{
	char *		field;
	
	if (!isString(inField) || length(inField) != 1) error("type must be a string");
	field = (char*)CHAR(STRING_ELT(inField,0));
	if (!strcmp("match",field)) {
		return MatchField;
	} else if (!strcmp("matchend",field)) {
		return MatchEndField;
	} else if (!strcmp("target",field)) {
		return TargetField;
	} else if (!strcmp("keyword",field)) {
		return KeywordField;
	} else {
	    return NoField;
	} 
}



