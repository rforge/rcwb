setClass("cqp_attr",
  representation(
    cqp_corpus.name="character",
    qualified.attribute.name="character",
    name="character",
    parent.cqp_corpus="cqp_corpus",
    parent.cqp_corpus.name="character"
));

###########################################################################
# Constructors
###########################################################################

`[[.cqp_corpus` <- function(i, j, ...) {
	.create_cqp_attr(i, j);
}

`$.cqp_corpus` <- function(x, name) {
	.create_cqp_attr(x, name);
}

`[[.cqp_subcorpus` <- function(i, j, ...) {
	.create_cqp_attr_subcorpus(i, j);
}

`$.cqp_subcorpus` <- function(x, name) {
	.create_cqp_attr_subcorpus(x, name);
}

.create_cqp_attr <- function(corpus, attribute) {
	qualified.attribute.name <- .cqp_name(corpus, attribute);
	cqp_corpus.name <- .cqp_name(corpus);
	
	cqp_attr <- 0;
	class(cqp_attr) <- "cqp_attr";
	attr(cqp_attr, "parent.cqp_corpus") <- corpus;
	attr(cqp_attr, "parent.cqp_corpus.name") <- cqp_corpus.name;
	attr(cqp_attr, "name") <- attribute;
	attr(cqp_attr, "qualified.attribute.name") <- qualified.attribute.name;
		
	positional <- cqi_attributes(cqp_corpus.name, "p");
	structural <- cqi_attributes(cqp_corpus.name, "s");	
	if (attribute %in% positional) {
		attr(cqp_attr, "type") <- "positional";
	} else if (attribute %in% structural) {
		attr(cqp_attr, "type") <- "structural";
		if (cqi_structural_attribute_has_values(qualified.attribute.name)) {
			attr(cqp_attr, "has_value") <- TRUE;
		} else {
			attr(cqp_attr, "has_value") <- FALSE;
		}
	} else {
		stop("Unknown attribute");
	}		
	
	return(cqp_attr);
}

.create_cqp_attr_subcorpus <- function(x, name) {
    cqp_corpus.name <- parent.corpus <- attr(x, "parent.cqp_corpus.name");
    cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
    qualified.attribute.name <- paste(parent.cqp_corpus.name, cqp_subcorpus.name, sep=".");
	
	x <- cqi_dump(...)
	start <- x[,1];
	end <- y[,2];
	
	y <- mapply(`:`, start, end, SIMPLIFY=FALSE)
	y <- unlist(y);
	
	z <- cqi_cpos2id(qualified.attribute.name, y);
	cqp_attr_subcorpus <- id2str(qualified.attribute.name, z);
	
	class(cqp_attr) <- "cqp_attr";
	attr(cqp_attr, "cqp_subcorpus") <- x;
	attr(cqp_attr, "cqp_corpus.name") <- cqp_corpus.name;
	attr(cqp_attr, "cqp_subcorpus.name") <- cqp_subcorpus;
	attr(cqp_attr, "name") <- attribute;
	attr(cqp_attr, "qualified.attribute.name") <- qualified.attribute.name;
	attr(cqp_attr, "type") <- "cap_attr_subcorpus";
	return(cqp_attr);

	return(cqp_attr_subcorpus);
}

###########################################################################
# Public
###########################################################################

## 
 # ------------------------------------------------------------------------
 # 
 # "ntype(corpus, attribute, ...)" --
 #
 # Get the number of types or the actual list of types for a positional attribute
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$pos );
 #
 # ------------------------------------------------------------------------
 ##
ntype.cqp_attr <- function(attribute, ...) {
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");

	if (.is.positional(attribute)) {
		n <- cqi_lexicon_size(qualified.attribute.name);
	} else {
		stop("ntype() is intented for positional attribute");
	}

	return(n);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "nvalue(corpus, attribute)" --
 #
 # Get the number of (unique) values for a structural attribute "with values".
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
nvalue.cqp_attr <- function(attribute, ...) {
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");

    if (.is.structural(attribute)) {
		if (.has_value(attribute)) {
			n <- length(values(attribute));
		} else {
			stop("no values on this structural attribute");
		}
	} else {
		stop("nvalue() is intended for structural attribute with value");
	}

	return(n);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "ntoken(attribute, ...)" --
 #
 # Get the number of tokens for a positional attribute (the result is the same
 # for all the positional attribute belonging to the same corpus).
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$pos );
 #
 # ------------------------------------------------------------------------
 ##
## == size(corpus)
ntoken.cqp_attr <- function(attribute, ...) {
	if (!.is.positional(attribute)) {
		stop("cannot count token on non-positional attribute");
	}
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");

	n <- cqi_attribute_size(qualified.attribute.name);
	return(n);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "nregion(attribute, ...)" --
 #
 # Get the number of regions for a structural attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
nregion.cqp_attr <- function(attribute, ...) {
	if (!.is.structural(attribute)) {
		stop("cannot count region on non-structural attribute");
	}
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");
	n <- cqi_attribute_size(qualified.attribute.name);
	return(n);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "tokens(attribute, ...)" --
 #
 # Get the vecteur of actual tokens for a positional attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
tokens.cqp_attr <- function(attribute, ...) {
	if (!.is.positional(attribute)) {
		stop("attribute must be positional");
	}	
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");

	max <- ntoken(attribute) - 1;
	x <- cqi_cpos2id(qualified.attribute.name, 0:max);
	str <- types(attribute);
	x <- str[x+1];

	return(x);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "types(attribute, ...)" --
 #
 # Get the vecteur of the (unique) types existing in a positional attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
types.cqp_attr <- function(attribute, ...) {
	if (!.is.positional(attribute)) {
		stop("attribute must be positional");
	}	
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");
	max.id <- ntype(attribute) - 1;
	ids <- 0:max.id;
	str <- cqi_id2str(qualified.attribute.name, ids);
	return(str);
}

#	else if (.is.structural(attribute)) {
#		s <- size(attr(attribute, "parent.cqp_corpus"));
#		max <- s - 1;
#		x <- cqi_cpos2struc(qualified.attribute.name, 0:max);
#	} else {
#		stop("unknown type");
#	}

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
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
regions.cqp_attr <- function(attribute, ...) {
	if (! .is.structural(attribute)) {
		stop("cannot list region on non-structural attribute");
	}
	if (!.has_value(attribute)) {
		stop("cannot list region on structural without value");
	}
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");
	max <- cqi_attribute_size(qualified.attribute.name) - 1;
	x <- cqi_struc2str(qualified.attribute.name, 0:max);

	return(x);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "regions(attribute, ...)" --
 #
 # Get the vecteur of the different (unique) value for a structural attribute with values
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              types( c$novel_title );
 #
 # ------------------------------------------------------------------------
 ##
value.cqp_attr <- function(attribute, ...) {
	if (! .is.structural(attribute)) {
		stop("cannot list region on non-structural attribute");
	}
	if (!.has_value(attribute)) {
		stop("cannot list region on structural without value");
	}
	return(unique(region(attribute)));
}

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
region_sizes.cqp_attr <- function(attribute) {
	if (!.is.structural(attribute)) {
		stop("cannot list region on non-structural attribute");
	}
	
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");

	att_size <- cqi_attribute_size(qualified.attribute.name);

	return(
		sapply(
			0:(att_size-1),
			function(x) {
				bound <- cqi_struc2cpos(qualified.attribute.name, x);
				return(bound[2] - bound[1] + 1);
			}
		)
	);
}

summary.cqp_attr <- function(object, ...) {
	attribute <- object;
	type <- attr(attribute, "type");
	qualified.attribute.name <- attr(attribute, "qualified.attribute.name");
	
	cat(paste(type, ": "));
	
	if (type == "positional") {
		number_of_types <- ntype(object);
		cat(paste(
				qualified.attribute.name, 
				" (",
				number_of_types, " types; ",
				ntoken(object), " tokens",
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
				nregion(object), " regions",
				")\n",
				sep=""));
			.print_sample_types(cqi_struc2str, qualified.attribute.name, number_of_types);
		} else {
			cat(paste(
				qualified.attribute.name, 
				" (",
				nregion(object), " regions",
				")\n",
				sep=""));
		}
	}
}

print.cqp_attr <- function(x, ...) {
	print(tokens(x));
}

###########################################################################
# Private
###########################################################################

.is.positional <- function (attribute) {
	if(class(attribute) != "cqp_attr") {
		stop("attr must be a cqp_attr object");
	}
	type <- attr(attribute, "type");
	if (type=="positional") {
		return(TRUE);
	} else if (type=="structural") {
		return(FALSE);
	} else {
		stop("type of attribute is unknown");
	}
}

.is.structural <- function (attribute) {
	if(class(attribute) != "cqp_attr") {
		stop("attr must be a cqp_attr object");
	}
	type <- attr(attribute, "type");
	if (type=="positional") {
		return(FALSE);
	} else if (type=="structural") {
		return(TRUE);
	} else {
		stop("type of attribute is unknown");
	}
}

.has_value <- function (attribute) {
	if (!.is.structural(attribute)) stop("Not a structural attribute");
	has_value <- attr(attribute, "has_value");
	if (has_value) {
		return(TRUE);
	} else {
		return(FALSE);
	}
}

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

