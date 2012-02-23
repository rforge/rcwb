# ===========================================================================
# File: "s3.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

###########################################################################
# S3 Object cqp_corpus
###########################################################################



## 
 # ------------------------------------------------------------------------
 # 
 # "corpus(corpus, query)" --
 #
 # Create an S3 object holding a corpus
 # 
 # Example:
 #              corpus("DICKENS")
 # 
 # ------------------------------------------------------------------------
 ##
corpus <- function(corpus.name) {
	x <- 0;
   class(x) <- "cqp_corpus";
   attr(x, "cqp_corpus.name") <- corpus.name;
   return(x);
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(corpus)" --
 #
 # Applying generic method "summary" to corpus object: print information.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              summary(c)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_corpus <- function(object, ...) {
	cqp_corpus.name <- .cqp_name(object);
	cat(paste(cqp_corpus.name, "\n"));
		
	p_attributes <- sort(cqi_attributes(cqp_corpus.name, "p"));
	s_attributes <- sort(cqi_attributes(cqp_corpus.name, "s"));
	a_attributes <- sort(cqi_attributes(cqp_corpus.name, "a"));
	
	tokens <- size(corpus);
	cat(paste("Number or tokens:", tokens, "\n"));

	cat(paste("Positionnal attributes (", length(p_attributes), ")\n", sep=""));
	for (p in p_attributes) {
		qualified_name <- .cqp_name(corpus, p);
		number_of_types <- cqi_lexicon_size(qualified_name);
	    cat(paste("\t", p, " (", number_of_types, " types)\n", sep=""));
		
		default_max <- 9;
		max <- ifelse(number_of_types + 1 < default_max, number_of_types - 1, default_max);
		examples <- cqi_id2str(qualified_name, 0:max);
		while (sum(nchar(examples)) > 50 & length(examples) > 2) {
			examples <- examples[-length(examples)];
		}
		ponct <- ifelse(length(examples) == number_of_types, ".", ", ...");
		examples <- paste("\"", examples, "\"", sep="");
		examples <- paste(examples, collapse=", ");
		examples <- paste(examples, ponct, sep="");
		cat(paste("\t\t", examples, "\n"));
	}

	cat(paste("Structural attributes (", length(s_attributes), ")\n", sep=""));
	for (s in s_attributes) {
		qualified_name <- .cqp_name(corpus, s);
		number_of_tokens <- cqi_attribute_size(qualified_name);
	    cat(paste("\t", s, " (", number_of_tokens, " tokens", sep=""));
		if (cqi_structural_attribute_has_values(qualified_name)) {
			number_of_types <- length(unique(cqi_struc2str(qualified_name, 0:number_of_tokens)));
			cat(paste(" and ", number_of_types, " types)\n", sep=""));
			
			default_max <- 9;
			max <- ifelse(number_of_types + 1 <= default_max, number_of_types - 1, default_max);
			examples <- cqi_struc2str(qualified_name, 0:max);
			while (sum(nchar(examples)) >= 50 & length(examples) > 2) {
				examples <- examples[-length(examples)];
			}
			ponct <- ifelse(length(examples) == number_of_types, ".", ", ...");
			examples <- paste("\"", examples, "\"", sep="");
			examples <- paste(examples, collapse=", ");
			examples <- paste(examples, ponct, sep="");
			cat(paste("\t\t", examples, "\n"));
		} else {
			cat(")\n");
		}
	}

	cat(paste("Alignement attributes (", length(a_attributes), ")\n", sep=""));
	for (a in a_attributes) {
		qualified_name <- .cqp_name(corpus, a);
		number_of_tokens <- cqi_attribute_size(qualified_name);
	    cat(paste("\t", a, " (", number_of_tokens, " tokens).\n", sep=""));
	}
		
}



## 
 # ------------------------------------------------------------------------
 # 
 # "print(corpus)" --
 #
 # Applying generic method "print" to corpus object: print all available information
 # for the given span of tokens of the corpus.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              print(c)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_corpus <- function(x, from=0, to=20, ...) {
	max <- size(x);
	if (any(c(from, to) >= max)) {
		stop("Token ids cannot be greater than corpus size");
	}
	if (from >= to) {
		stop("'from' must be lesser than 'to'");
	}
	if (any(c(from, to) < 0)) {
		stop("Token ids cannot be < 0");
	}

# 	if ((to - from) > 100) {
# 		stop("The difference between 'to' and 'from' is > 100, not a convenient print");
# 	}

	printed <- .cqp_corpus2matrix(x, from, to);

#	qualified_attribute <- paste(cqp_corpus.name, attribute, sep=".");
# 	tokens <- cqi_cpos2str(qualified_attribute, token_id);
# 
# 	line_length <- 20;
# 	nbr_line <- (nbr_token + line_length - 1) %/% line_length;
# 
# 	lines <- split(tokens, rep(1:nbr_line, each=line_length)[1:nbr_token]);
# 	lapply(lines, function(x) {
# 			cat(paste(paste(x, collapse=" "), "\n"));
# 		}
# 	);
	print(printed);
}


## 
 # ------------------------------------------------------------------------
 # 
 # "write.cqp_corpus(corpus, filename)" --
 #
 # Save a cqp corpus as a matrix.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              write.cqp_corpus(c, "dickens.tab")
 #
 # ------------------------------------------------------------------------
 ##
write.cqp_corpus <- function(corpus, filename, from=0, to=1000, ...) {
	cqp_corpus.name <- .cqp_name(corpus);
	size <- size(corpus);
	mat <- .cqp_corpus2matrix(corpus, 0, size);
	write.table(mat, file=filename, ...);
}


.cqp_corpus2matrix <- function(x, from, to) {	
	
	cqp_corpus.name <- .cqp_name(x);
	max <- size(x) - 1;

	token_id=from:to;
	nbr_token <- length(token_id);

	positional <- cqi_attributes(cqp_corpus.name, "p");
	nbr_positional <- length(positional);

	structural <- cqi_attributes(cqp_corpus.name, "s");
	nbr_structural <- length(structural);

	printed <- data.frame(matrix("", nrow=nbr_token, ncol=nbr_positional+nbr_structural));
	rownames(printed) <- token_id;
	colnames(printed) <- c(structural, positional);

	for (i in 1:nbr_structural) {
		qualified_structural_attribute <- .cqp_name(x, structural[i]);
		ids <- cqi_cpos2struc(qualified_structural_attribute, token_id);
		printed[,i] <- ids
	}
	
	for (i in 1:nbr_positional) {
		qualified_positional_attribute <- .cqp_name(x, positional[i]);
		ids <- cqi_cpos2str(qualified_positional_attribute, token_id);
		printed[,i+nbr_structural] <- ids
	}

	return(printed);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "region_sizes.cqp_corpus(corpus, structural_attribute)" --
 #
 # Create a vector containing the size (in number of tokens) of the regions of
 # the given structural attribute.
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              region_sizes.cqp_corpus(c, "np")
 # 
 # ------------------------------------------------------------------------
 ##

## TODO il y aurait encore plus simple : table() sur tous les struc.

region_sizes <- function(corpus, structural_attribute) {
	if (class(corpus) != "cqp_corpus") {
		stop("corpus: not a corpus object");
	}
	
	cqp_corpus.name <- .cqp_name(corpus);

	atts <- cqi_attributes(cqp_corpus.name, "s");
	if (! structural_attribute %in% atts) {
		stop("structural_attribute is not a structural attribute on this corpus");
	}

	qualified_attribute <- .cqp_name(corpus, structural_attribute);
	att_size <- cqi_attribute_size(qualified_attribute);
	return(
		sapply(
			0:(att_size-1),
			function(x) {
				bound <- cqi_struc2cpos(qualified_attribute, x);
				return(bound[2] - bound[1]);
			}
		)
	);
}


###########################################################################
# S3 Object cqp_subcorpus
###########################################################################


## 
 # ------------------------------------------------------------------------
 # 
 # "subcorpus(corpus, query)" --
 #
 # Create an S3 object holding a subcorpus
 # 
 # Example:
 #              subcorpus("DICKENS", "\"interesting\"")
 # 
 # ------------------------------------------------------------------------
 ##
subcorpus <- function(corpus, query) {
	if (class(corpus) != "cqp_corpus" ) {
		stop("corpus: not a corpus object");
	}
	parent.cqp_corpus.name <- .cqp_name(corpus);
	
    cqp_subcorpus.name <- .generate.cqp_subcorpus.name(parent.cqp_corpus.name);
    cqi_query(parent.cqp_corpus.name, cqp_subcorpus.name, query);
	
	x <- 0;
    class(x) <- c("cqp_subcorpus");

    attr(x, "cqp_subcorpus.name") <- cqp_subcorpus.name;
    attr(x, "parent.cqp_corpus.name") <- parent.cqp_corpus.name;
    attr(x, "query") <- query;
	return(x);
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(subcorpus)" --
 #
 # Applying generic method "summary" to subcorpus object: print basic information.
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", "\"interesting\"")
 #              summary(sc)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_subcorpus <- function(object, ...) {
	if (class(object) != "cqp_subcorpus") {
		stop("object must be cqp_subcorpus object");
	}

	parent_corpus.name <- attr(object, "parent.cqp_corpus.name");
	qualified_name <- .cqp_name(object);
	size <- cqi_subcorpus_size(qualified_name);
	query <- attr(object, "query");
	
	cat(paste("Parent corpus name:", parent_corpus.name, "\n"));
	cat(paste("Number of matches:", size, "\n"));
	cat(paste("Query:", query, "\n"));
	# TODO : some samples of the first lines
}


## 
 # ------------------------------------------------------------------------
 # 
 # "print(subcorpus)" --
 #
 # Applying generic method "print" to corpus object: print first kwic lines of the corpus
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", "\"interesting\"")
 #              print(sc)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_subcorpus <- function(x, positional.attribute="word", from=0, to=10, ...) {
	size <- size(x);
	max.line <- size - 1.
	if (to > max.line) {
		stop(paste("Max line:", max.line, "; max lines requested:", to));
	}
	
	k <- cqp_kwic(x);
	print(k, from=from, to=to);
}



.generate.cqp_subcorpus.name <- function(corpus) {
  subcorpora.name <- cqi_list_subcorpora(corpus)
  name <- .generate.name()
  if (length(subcorpora.name) == 0) {
	  return(name);
  }
  while (name %in% subcorpora.name) {
    name <- .generate.name();
  }
  return(name);
}



.generate.name <- function() {
  initial <- LETTERS[sample(1:26, 1)];
  other <- letters[sample(1:26, 9)];
  other <- paste(other, collapse="");
  return(paste(initial, other, sep=""));
}


.cqp_name.cqp_corpus <- function(x, attribute=NULL) {
	if (class(x) != "cqp_corpus") {
		stop("x must be cqp_corpus object");
	}
	
	corpus_name <- attr(x, "cqp_corpus.name");
	if (is.null(attribute)) {
		return(corpus_name);
	} else {
		return(paste(corpus_name, attribute, sep="."));
	}
}

.cqp_name.cqp_subcorpus <- function(x) {
	if (class(x) != "cqp_subcorpus") {
		stop("x must be cqp_subcorpus object");
	}

	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified.sub_corpus.name <- paste(parent.corpus, cqp_subcorpus.name, sep=":");
}

.cqp_name <- function (x, ...) UseMethod(".cqp_name");

size.cqp_corpus <- function(x) {
	if (!class(x) == "cqp_corpus") {
		stop("not a cqp_corpus object");
	}
	
	word.attribute <- .cqp_name(x, "word");
	return(cqi_attribute_size(word.attribute));
}

size.cqp_subcorpus <- function(x) {
	if (!class(x) == "cqp_subcorpus") {
		stop("not a cqp_subcorpus object");
	}
	cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified.name <- paste(cqp_corpus.name, cqp_subcorpus.name, sep=":");
	return(cqi_subcorpus_size(qualified.name));
}

size <- function (x) UseMethod("size");

###########################################################################
# S3 object cqp_flist
###########################################################################



cqp_flist.cqp_corpus <- function(x, attribute, cutoff=0, ...) {
	if (class(x) != "cqp_corpus") {
		stop("x: not a corpus object");
	}
	cqp_corpus.name <- attr(x, "cqp_corpus.name");
	
	positional <- cqi_attributes(cqp_corpus.name, "p");
	structural <- cqi_attributes(cqp_corpus.name, "s");
	
	qualified.attribute.name <- .cqp_name(x, attribute);

	if (attribute %in% positional) {
		max.id <- size(x) - 1;
		ids <- 0:max.id;
		flist <- cqi_id2freq(qualified.attribute.name, ids);
		str <- cqi_id2str(qualified.attribute.name, ids);
		names(flist) <- str;
	} else {
		if (cqi_structural_attribute_has_values(qualified.attribute.name)) {
			ids <- 0:(cqi_attribute_size(qualified.attribute.name)-1);
			values <- cqi_struc2str(qualified.attribute.name, ids);
			t <- table(values);
			flist <- as.numeric(t);
			names(flist) <- names(t);
		} else {
			stop("no values on this structural attribute");
		}
	}

	if (cutoff > 0) {
		if (cutoff < 1) {
			ordered <- order(flist, decreasing=TRUE);
			index <- ordered[1:(length(ordered)*cutoff)];
			flist <- flist[index];
		} else {
			flist <- flist[flist > cutoff];
		}
	}

   class(flist) <- "cqp_flist";

   attr(flist, "cqp_corpus.name") <- cqp_corpus.name;
   attr(flist, "attribute") <- attribute;
   return(flist);	
}



## 
 # ------------------------------------------------------------------------
 # 
 # "cqp_flist(corpus, attribute, field, left.context, right.context)" --
 #
 # Create an S3 object holding a frequency list
 #
 # A cqp_flist is a named numeric vector.
 # 
 # Example:
 #              cqp_flist(my_sub_corpus, "lemma", "match", 4, 4)
 #
 # "left.context" and "right.context" define a span around the target word.
 #
 # if "target" is a character vector of length 2, such as c("match", "matchend"), the frequency list is computed
 # with all the word contained between match and matchend.
 # ------------------------------------------------------------------------
 ##
cqp_flist.cqp_subcorpus <- function(x, anchor, attribute, left.context=0, right.context=0, cutoff=0, offset=0, ...) {

	if (class(x) != "cqp_subcorpus") {
		stop("x is not an S3 object of class cqp_subcorpus");
	}
	
	if (length(anchor) > 2 || length(anchor) < 1) {
		stop("anchor must be a vector of lenth 1 or 2");
	}
	
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	qualified.subcorpus.name <- .cqp_name(x);
	qualified.attribute <- paste(parent.cqp_corpus.name, attribute, sep=".");

	flist <- 0;
	
	if (length(anchor) == 1 & left.context == 0 & right.context == 0) {
		fdist <- cqi_fdist1(qualified.subcorpus.name, anchor, attribute, cutoff=cutoff, offset=offset);
		id <- fdist[,1];
		flist <- fdist[,2];
		names(flist) <- cqi_cpos2str(
			paste(parent.cqp_corpus.name, attribute, sep="."),
			id
		);
	} else {
		dump <- cqi_dump_subcorpus(paste(parent.cqp_corpus.name, cqp_subcorpus.name, sep=":"));
		colnames(dump) <- c("match", "matchend", "target", "keyword");
		left.cpos <- dump[,anchor[1]];
		left.cpos <- left.cpos + offset;
		if (left.context > 0) {
			left.cpos <- left.cpos - left.context;
		}
		right.anchor <- ifelse(length(anchor) > 1, anchor[2], anchor[1])
		right.cpos <- dump[,right.anchor] + right.context;
		
		#nbr_tokens <- sum(right.cpos-left.cpos);
		tokens <- sapply(
			1:length(left.cpos),
			function(x) {
				cqi_cpos2id(qualified.attribute, left.cpos[x]:right.cpos[x]);
			}
		);
		tokens <- as.numeric(tokens);
		
		flist <- table(tokens);
		ids <- as.numeric(names(flist));
		names(flist) <- cqi_id2str(qualified.attribute, ids);
	}		
	
   class(flist) <- "cqp_flist";

   attr(flist, "cqp_subcorpus.name") <- cqp_subcorpus.name;   
   attr(flist, "parent.cqp_corpus.name") <- parent.cqp_corpus.name;
   attr(flist, "anchor") <- anchor;
   attr(flist, "left.context") <- left.context;
   attr(flist, "right.context") <- right.context;
   attr(flist, "attribute") <- attribute;
   attr(flist, "offset") <- offset;

   return(flist);
}




## 
 # ------------------------------------------------------------------------
 # 
 # "summary(cqp_flist)" --
 #
 # Applying generic method "summary" to cqp_flist object: print basic information.
 # 
 # Example:
 # TODO
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_flist <- function(object, ...) {

	if (class(object) != "cqp_flist") {
		stop("object must be cqp_flist object");
	}

	cat("A frequency list\n");
	cat(paste("  Number of tokens:", sum(object), "\n"));
	cat(paste("  Number of types:", length(object), "\n"));

	cqp_corpus.name <- attr(object, "cqp_corpus.name");
	# a cqp_flist made with a corpus
	if (!is.null(cqp_corpus.name)) {
		attribute <- attr(object, "attribute");
		cat(paste("  Corpus:", cqp_corpus.name, "\n"));
		cat(paste("  Attribute:", attribute, "\n"));
	# otherwise, with a subcorpus
	} else {	
		cqp_subcorpus.name     <- attr(object, "cqp_subcorpus.name");   
		parent.cqp_corpus.name <- attr(object, "parent.cqp_corpus.name");
		anchor                 <- attr(object, "anchor");
		left.context           <- attr(object, "left.context");
		right.context          <- attr(object, "right.context");
		attribute              <- attr(object, "attribute");
		offset                 <- attr(object, "offset");
		cat(paste("  Subcorpus:", cqp_subcorpus.name, "\n"));
		cat(paste("  Parent corpus:", parent.cqp_corpus.name, "\n"));
		cat(paste("  anchor:", anchor, "\n"));
		cat(paste("  left.context:", left.context, "\n"));
		cat(paste("  right.context:", right.context, "\n"));
		cat(paste("  attribute:", attribute, "\n"));
		cat(paste("  offset:", offset, "\n"));
	}
}

## 
 # ------------------------------------------------------------------------
 # 
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_flist <- function(x, ...) {
	df <- data.frame(names(x), as.numeric(x));
	colnames(df) <- c("type", "frequency");
	print(df, row.names=FALSE);
}


cqp_flist <- function(x, ...) UseMethod("cqp_flist");

###########################################################################
# S3 Object cqp_ftable
###########################################################################

## 
 # ------------------------------------------------------------------------
 # 
 # "cqp_ftable(corpus, structure, attribute, use.value=F, cutoff=0, subcorpus=NULL)" --
 #
 # Create an S3 object holding a frequency table accordint to various parameters
 #
 # A cqp_ftable is a three-column data.frame : modality_A modality_1 frequency. See reshape package for easy conversion into matrix.
 # 
 # Example:
 #              # case of fdist2
 #              # case of frequency by a structural attribute (each row is a text, a p, a chapter...)
 #              # case of frequency by value of a structural attribute with value
 #              # etc.
 # 
 # ------------------------------------------------------------------------
 ##
cqp_ftable.cqp_corpus <- function(x, attribute1, attribute2, 
	attribute1.use.id=FALSE, attribute2.use.id=FALSE,
	structural.attribute.unique.id=FALSE, subcorpus=NULL,
	...
) {
	cqp_corpus.name <- .cqp_name(x);
	qualified.attribute1 <- .cqp_name(x, attribute1);
	qualified.attribute2 <- .cqp_name(x, attribute2);
	
	corpus_size <- size(x);
	max_id <- corpus_size - 1;
	
	s_atts <- cqi_attributes(cqp_corpus.name, "s");
	p_atts <- cqi_attributes(cqp_corpus.name, "p");

	##
	## extract id.
	##
	
	att1 <- 0;
	if (attribute1 %in% s_atts) {
		att1 <- cqi_cpos2struc(qualified.attribute1, 0:max_id);
	} else if (attribute1 %in% p_atts) {
		# TODO array base
		att1 <- cqi_cpos2id(qualified.attribute1, 0:max_id);
	} else {
		stop(paste("Unknown attribute:", attribute1));
	}

	att2 <- 0;
	if (attribute2 %in% s_atts) {
		# TODO array base
		att2 <- cqi_cpos2struc(qualified.attribute2, 0:max_id);
	} else if (attribute2 %in% p_atts) {
		# TODO array base
		att2 <- cqi_cpos2id(qualified.attribute2, 0:max_id);
	} else {
		stop(paste("Unknown attribute:", attribute2));
	}

	##
	## Create the id matrix
	##

	ids <- matrix(c(att1, att2), ncol=2);

	if (structural.attribute.unique.id) {
		if (! (attribute1 %in% s_atts)
			||
			! (attribute2 %in% s_atts)
		) {
			stop("Both attribute must be structural attributes in order to reduce id");
		}
		ids <- unique(ids);
	}

	##
	## replace id with string if requested.
	##

	res <- data.frame(attribute1=ids[,1], attribute2=ids[,2]);
	if (attribute1 %in% s_atts) {
		if ((!attribute1.use.id) & cqi_structural_attribute_has_values(qualified.attribute1)) {
			res[,1] <- cqi_struc2str(qualified.attribute1, ids[,1]);
		}
	} else {
		if (!attribute1.use.id) {
			res[,1] <- cqi_id2str(qualified.attribute1, ids[,1]);
		}
	}

	if (attribute2 %in% s_atts) {
		if ((!attribute2.use.id) & cqi_structural_attribute_has_values(qualified.attribute2)) {
			res[,2] <- cqi_struc2str(qualified.attribute2, ids[,2]);
		}
	} else {
		if (!attribute2.use.id) {
			res[,2] <- cqi_id2str(qualified.attribute2, ids[,2]);
		}
	}
	
	##
	## Count unique combinaison.
	##
	t <- count(res);

	colnames(t) <- c(
		attribute1,
		attribute2,
		"freq"
	);

	return(t);
}


cqp_ftable.cqp_subcorpus <- function(x, anchor1, attribute1, anchor2, attribute2, cutoff=0, ...) {
	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	qualified.sub_corpus.name <- .cqp_name(x);
	
	m <- cqi_fdist2(qualified.sub_corpus.name, anchor1, attribute1, anchor2, attribute2, cutoff=cutoff);

	attribute1.str <- cqi_id2str(paste(parent.corpus, attribute1, sep="."), m[,1]);
  	attribute2.str <- cqi_id2str(paste(parent.corpus, attribute2, sep="."), m[,2]);
 
	df <- data.frame(attribute1.str, attribute2.str, m[,3]);
	colnames(df) <- c(
		paste(anchor1, attribute1, sep="."),
		paste(anchor2, attribute2, sep="."),
		"freq");
	return(df);
}

cqp_ftable <- function(x, ...) UseMethod("cqp_ftable");


###########################################################################
# S3 Object cqp_kwic
###########################################################################


cqp_kwic.cqp_subcorpus <- function(x,
	right.context=20,
	left.context=20,
	...
) {
	qualified_subcorpus_name <- .cqp_name(x);

	size <- cqi_subcorpus_size(qualified_subcorpus_name);
	if (size == 0) {
		stop("empty subcorpus");
	}
	
	s <- .get.kwic.matrix(x, right.context, left.context);	

	attr(s, "parent.cqp_corpus.name") <- attr(x, "parent.cqp_corpus.name");
	attr(s, "cqp_subcorpus.name") <- attr(x, "cqp_subcorpus.name");
	attr(s, "right.context") <- right.context;
	attr(s, "left.context") <- left.context;
	class(s) <- "cqp_kwic";
	return(s);
}

cqp_kwic <- function(x, ...) UseMethod("cqp_kwic");

sort.cqp_kwic <- function(x, decreasing=FALSE, sort.anchor="match", sort.attribute="word", sort.offset=0, ...) {
	if (!class(x) == "cqp_kwic") {
		stop("x must be a cqp_kwic object");
	}
	
	if (! sort.anchor %in%  c("match", "matchend", "target", "keyword")) {
		stop('sort.anchor must be in c("match", "matchend", "target", "keyword")');
	}
	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified_attribute <- paste(parent.cqp_corpus.name, sort.attribute, sep=".");

	cpos <- x[, sort.anchor];
	if (sort.offset != 0) {
		cpos <- cpos + sort.offset;

		unreachable.small <- cpos < 0;
		cpos[unreachable.small] <- 0;
		
		size <- cqi_attribute_size(qualified_attribute);
		max.id <- size - 1;
		unreachable.big <- cpos > max.id;
		cpos[unreachable.big] <- max.id;		
	} else {
		unreachable.small <- logical(length(cpos));
		unreachable.big <- logical(length(cpos));
	}
	
	str <- cqi_cpos2str(qualified_attribute, cpos);
	str[unreachable.small] <- "";
	str[unreachable.big] <- "";
	i <- order(str, decreasing=decreasing);

	sorted <- x[i,];
	attributes(sorted) <- attributes(x);
	return(sorted);
}

.get.kwic.matrix <- function(x, right.context, left.context) {
	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified_subcorpus_name <- paste(parent.cqp_corpus.name, cqp_subcorpus.name, sep=":");

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	left.boundary <- pmax(dump[,1] - left.context, 0);
	dim(left.boundary) <- c(nrow(dump), 1);

	corpus_size <- size(x);
	
	max_id <- corpus_size - 1;
	right.boundary <- pmin(dump[,2] + right.context, max_id);
	dim(right.boundary) <- c(nrow(dump), 1);

	dump <- cbind(dump, left.boundary, right.boundary);
	colnames(dump) <- c("match", "matchend", "target", "keyword", "left", "right");
	return(dump);
}

print.cqp_kwic <- function(x,
	from=0,
	to=20,
	print_tokens=function(x, cpos) cqi_cpos2str(paste(attr(x, "parent.cqp_corpus.name"), "word", sep="."), cpos),
	left.separator=" <<",
	right.separator=">> ",
	...
)
{
	
	if (from < 0) {
		stop("'from' must be greater than 0");
	}
	if (from > to) {
		stop("'to' must be greater than from");
	}
	if (to >= nrow(x)) {
		stop("'to' must be lesser than the size of the subcorpus");
	}

	requested.left.char= attr(x, "left.context")
	requested.right.char= attr(x, "right.context")

	nbr.lines <- to-from+1;
	lines <- character(nbr.lines);
	matrix.lines <- matrix("", nrow=nbr.lines, ncol=3);
	
	for (i in 0:(nbr.lines - 1)) {
		matrix.lines[i, 1] <- paste(
			print_tokens(x, x[from + i, "left"]:(x[from + i, "match"]-1)),
			collapse=" "
		);

		c1 <- print_tokens(x, x[from + i, "match"]:x[from + i, "matchend"]);
		c2 <- c(left.separator, c1, right.separator);
		matrix.lines[i, 2] <- paste(c2, collapse=" ");

		matrix.lines[i, 3] <- paste(
			print_tokens(x, (x[from + i, "matchend"]+1):x[from + i, "right"]),
			collapse=" "
		);
	}
	
	left.nchar <- nchar(matrix.lines[,1]);
	center.nchar <- nchar(matrix.lines[,2]);
	right.nchar <- nchar(matrix.lines[,3]);
	
	left <- substr(matrix.lines[,1], left.nchar - requested.left.char, left.nchar);
	requested.center.char <- max(center.nchar);
	right <- substr(matrix.lines[,3], 1, right.nchar - (right.nchar - requested.right.char));

	format <- paste("%10d %", requested.left.char, "s%", requested.center.char, "s%-", requested.right.char, "s", sep="");
	lines <- sprintf(format, x[from:to, "match"], left, matrix.lines[,2], right);
	
	for(i in lines) {
		cat(paste(i, "\n", sep=""));
	}
}
