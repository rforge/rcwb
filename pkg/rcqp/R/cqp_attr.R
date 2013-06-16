setClass("cqp_attr",
  contains="cqp_object",
  representation(
    parent="cqp_queryable"
));

setClass("cqp_attr_positional",
  contains="cqp_attr",
);

setClass("cqp_attr_structural",
  contains="cqp_attr",
  representation(
    has_value="logical"
));

###########################################################################
# Constructors
###########################################################################

setMethod("[[", signature(x = "cqp_corpus", i = "character"), function (x, i, exact) {
	stop("deprecated; use '$' operator instead");
})

setMethod("$", signature(x = "cqp_corpus"),
function (x, name) {
  corpus <- x;
  attribute <- name;
  cqp_corpus.name <- .cqp_name(corpus);
  positional <- cqi_attributes(cqp_corpus.name, "p");
  structural <- cqi_attributes(cqp_corpus.name, "s");	

	if (attribute %in% positional) {
        obj <- new("cqp_attr_positional", cqp_name=attribute, parent=corpus);
	} else if (attribute %in% structural) {
		if (cqi_structural_attribute_has_values(qualified.attribute.name)) {
			has_value <- TRUE;
		} else {
			has_value <- FALSE;
		}
        obj <- new("cqp_attr_structural", cqp_name=attribute, parent=corpus, has_value=has_value);
	} else {
		stop("Unknown attribute");
	}		
	
	return(obj);
})

###########################################################################
# Public
###########################################################################

## 
 # ------------------------------------------------------------------------
 # 
 # "get.corpus(attribute)" --
 #
 # Accessor for the corpus this attribute belong to.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              attr <- c$pos
 #              get.corpus(attr);
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("get.corpus", function(attribute) standardGeneric("get.corpus"));

setMethod("get.corpus", "cqp_attr", function(attribute) {
    return(attribute@parent);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "ntypes(corpus, attribute)" --
 #
 # Get the number of types or the actual list of types for a positional attribute
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              attr <- c$pos
 #              ntypes(attr);
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("ntypes", function(attribute) standardGeneric("ntypes"));

setMethod("ntypes", "cqp_attr_positional", function(attribute) {
	name <- .cqp_name(attribute);
	n <- cqi_lexicon_size(qualified.attribute.name);
	return(n);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "nvalues(corpus, attribute)" --
 #
 # Get the number of (unique) values for a structural attribute "with values".
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              nvalues( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("nvalues", function(attribute) standardGeneric("nvalues"));

setMethod("nvalues", "cqp_attr_structural", function(attribute) {

	if (attribute@has_value) {
		n <- length(values(attribute));
	} else {
		stop("no values on this structural attribute");
	}

	return(n);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "ntokens(attribute)" --
 #
 # Get the number of tokens for a positional attribute (the result is the same
 # for all the positional attribute belonging to the same corpus).
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              ntokens( c$pos );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("ntokens", function(attribute) standardGeneric("ntokens"));

setMethod("ntokens", "cqp_attr_positional", function(attribute) {
	name <- .cqp_name(attribute);
	n <- cqi_attribute_size(name);
	return(n);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "nregions(attribute, ...)" --
 #
 # Get the number of regions for a structural attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              nregions( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("nregions", function(attribute) standardGeneric("nregions"));

setMethod("nregions", "cqp_attr_structural", function(attribute) {
	name <- .cqp_name(attribute);
	n <- cqi_attribute_size(name);
	return(n);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "tokens(attribute, ...)" --
 #
 # Get the vecteur of actual tokens for a positional attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              tokens( c$word );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("tokens", function(attribute, from, to) standardGeneric("tokens"));

setMethod("tokens", "cqp_attr_positional", function(attribute, from=0, to=ntokens(attribute) - 1) {
	name <- .cqp_name(attribute, qualified=TRUE);
    if (from < 0)
	  stop("'from' cannot be < 0");
    if (to > ntokens(attribute))
	  stop("'to' cannot be > ntokens(attribute)");
	x <- cqi_cpos2str(qualified.attribute.name, from:to);
	return(x);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "types(attribute, ...)" --
 #
 # Get the vecteur of the (unique) types existing in a positional attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types(c$word);
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("types", function(attribute) standardGeneric("types"));

setMethod("types", "cqp_attr_positional", function(attribute) {
	name <- .cqp_name(attribute, qualified=TRUE);
	max.id <- ntypes(attribute) - 1;
	ids <- 0:max.id;
	str <- cqi_id2str(qualified.attribute.name, ids);
	return(str);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "regions(attribute, ...)" --
 #
 # Get the vecteur of the value of each region (value may thus be repeated)
 # for a structural attribute with values
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              regions( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("regions", function(attribute, from, to) standardGeneric("regions"));

setMethod("regions", "cqp_attr_positional", function(attribute, from=0, to=nregions(attribute)-1) {
	if (!attribute@has_value) {
		stop("cannot list region on structural without value");
	}
	name <- .cqp_name(attribute, qualified=TRUE);
	max <- nregions(attribute) - 1;
	if (from < 0)
	  stop("'from' cannot be < 0");
    if (to > max)
	  stop("'to' cannot be > nregions(attribute)");

	x <- cqi_struc2str(name, from:to);

	return(x);	
});

## 
 # ------------------------------------------------------------------------
 # 
 # "values(attribute, ...)" --
 #
 # Get the vecteur of the different (unique) value for a structural attribute with values
 # Is equivalent to 'unique(regions)'
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              values( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("values", function(attribute, from, to) standardGeneric("values"));

setMethod("values", "cqp_attr_positional", function(attribute, from=0, to=nregions(attribute)-1) {
	return(unique(regions(attribute, from, to)));
});

###########################################################################
# Utilities
###########################################################################

##
 # ------------------------------------------------------------------------
 # 
 # "region_sizes.cqp_attr(attribute)" --
 #
 # Create a vector containing the size (in number of tokens) of the regions of
 # the given structural attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              region_sizes.cqp_corpus(c$np)
 # 
 # ------------------------------------------------------------------------
 ##
 ## TODO il y aurait encore plus simple : table() sur tous les struc.
setGeneric("region_sizes", function(attribute, from, to) standardGeneric("region_sizes"));

setMethod("region_sizes", "cqp_attr_positional", function(attribute, from=0, to=nregions(attribute)-1) {
	name <- .cqp_name(attribute);

	att_size <- cqi_attribute_size(name);
	if (from < 0)
	  stop("'from' cannot be < 0");
    if (to > max)
	  stop("'to' cannot be > nregions(attribute)");

	return(
		sapply(
			from:to,
			function(x) {
				bound <- cqi_struc2cpos(name, x);
				return(bound[2] - bound[1] + 1);
			}
		)
	);
});

setMethod("summary", signature(object = "cqp_attr"), function(object){
	attribute <- object;
	type <- attr(attribute, "type");
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");
	
	cat(paste(type, ": "));
	
	if (type == "positional") {
		number_of_types <- ntypes(object);
		cat(paste(
				qualified.attribute.name, 
				" (",
				number_of_types, " types; ",
				ntokens(object), " tokens",
				")\n",
				sep=""));
		.print_sample_types(cqi_id2str, qualified.attribute.name, number_of_types);
	} else {
		has_value <- attr(attribute, "has_value");
		if (has_value) {
			re <- regions(object);
			t <- unique(re);
			number_of_types <- length(t);
			cat(paste(
				qualified.attribute.name, 
				" (",
				number_of_types, " types; ",
				nregions(object), " regions",
				")\n",
				sep=""));
			.print_sample_types(cqi_struc2str, qualified.attribute.name, number_of_types);
		} else {
			cat(paste(
				qualified.attribute.name, 
				" (",
				nregions(object), " regions",
				")\n",
				sep=""));
		}
	}
});

setMethod("print", signature(x = "cqp_attr"), function(x, ...){
	print(tokens(x));
});

###########################################################################
# Private
###########################################################################

.print_sample_types <- function(type_function, attribute, number_of_types, default=10) {
	max <- min(default, number_of_types) - 1;
	examples <- type_function(attribute, 0:max);
	while (sum(nchar(examples)) >= 50 & length(examples) > 2) {
		examples <- examples[-length(examples)];
	}
	ponct <- ifelse(length(examples) == number_of_types, ".", ", ...");
	examples <- paste("\"", examples, "\"", sep="");
	examples <- paste(examples, collapse=", ");
	examples <- paste(examples, ponct, sep="");
	cat(paste("\t\t", examples, "\n"));
}

