# ===========================================================================
# File: "s3.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

# merger des flist
# contingency table à partir des structures
# format d'entrée de UCS : pour un mot, avoir les quatre paramètres facilement

# fonctions utilitaires comme "frequency(corpus, attribute, c("et", "oui", "non"))"

# section vocabulary.

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
summary.cqp_corpus <- function(x) {
	cqp_corpus.name <- attr(x, "cqp_corpus.name");
	cat(paste(cqp_corpus.name, "\n"));
		
	p_attributes <- sort(cqi_attributes(cqp_corpus.name, "p"));
	s_attributes <- sort(cqi_attributes(cqp_corpus.name, "s"));
	a_attributes <- sort(cqi_attributes(cqp_corpus.name, "a"));
	
	tokens <- cqi_attribute_size(paste(cqp_corpus.name, p_attributes[1], sep="."));
	cat(paste("Number or tokens:", tokens, "\n"));

	cat(paste("Positionnal attributes (", length(p_attributes), ")\n", sep=""));
	for (a in p_attributes) {
		qualified_name <- paste(cqp_corpus.name, a, sep=".");
		number_of_types <- cqi_lexicon_size(qualified_name);
	    cat(paste("\t", a, " (", number_of_types, " types)\n", sep=""));
		
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
		qualified_name <- paste(cqp_corpus.name, s, sep=".");
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
		qualified_name <- paste(cqp_corpus.name, a, sep=".");
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
print.cqp_corpus <- function(x, from=0, to=20) {
	max <- cqi_attribute_size(paste(attr(x, "cqp_corpus.name"), "word", sep="."));
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
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	size <- cqi_attribute_size(paste(cqp_corpus.name, "word", sep="."));
	# TODO
	mat <- .cqp_corpus2matrix(corpus, 0, size);
	write.table(mat, file=filename, ...);
}


.cqp_corpus2matrix <- function(x, from, to) {	
	
	cqp_corpus.name <- attr(x, "cqp_corpus.name");
	max <- cqi_attribute_size(paste(cqp_corpus.name, "word", sep=".")) - 1;

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
		qualified_structural_attribute <- paste(cqp_corpus.name, structural[i], sep=".");
		ids <- cqi_cpos2struc(qualified_structural_attribute, token_id);
		printed[,i] <- ids
	}
	
	for (i in 1:nbr_positional) {
		qualified_positional_attribute <- paste(cqp_corpus.name, positional[i], sep=".");
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

region_sizes.cqp_corpus <- function(corpus, structural_attribute) {
	if (class(corpus) != "cqp_corpus") {
		stop("corpus: not a corpus object");
	}
	
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");

	atts <- cqi_attributes(cqp_corpus.name, "s");
	if (! structural_attribute %in% atts) {
		stop("structural_attribute is not a structural attribute on this corpus");
	}

	qualified_attribute <- paste(cqp_corpus.name, structural_attribute, sep=".");
	att_size <- cqi_attribute_size(qualified_attribute);
	return(
		sapply(
			0:(att_size-1),
			function(x) {
				bound <- cqi_struc2cpos(qualified_attribute, x);
				#print(bound);
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
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	
    cqp_subcorpus.name <- .generate.cqp_subcorpus.name(cqp_corpus.name);
    cqi_query(cqp_corpus.name, cqp_subcorpus.name, query);
	
	x <- 0;
    class(x) <- c("cqp_subcorpus");

    attr(x, "cqp_subcorpus.name") <- cqp_subcorpus.name;
    attr(x, "parent.cqp_corpus.name") <- cqp_corpus.name;
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
summary.cqp_subcorpus <- function(x) {
	if (class(x) != "cqp_subcorpus") {
		stop("x must be cqp_subcorpus object");
	}
	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	query <- attr(x, "query");
	
	size <- cqi_subcorpus_size(paste(parent.corpus, cqp_subcorpus.name, sep=":"));
	
	cat(paste("Parent corpus name:", parent.corpus, "\n"));
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
print.cqp_subcorpus <- function(x, positional.attribute="word", from=0, to=10) {
	if (class(x) != "cqp_subcorpus") {
		stop("x must be cqp_subcorpus object");
	}

	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified.sub_corpus.name <- paste(parent.corpus, cqp_subcorpus.name, sep=":");

	size <- cqi_subcorpus_size(qualified.sub_corpus.name);
	max.line <- size - 1.
	if (to > max.line) {
		stop(paste("Max line:", max.line, "; max lines requested:", to));
	}
	
	k <- kwic(x);
	l <- print(k, from=from, to=to);
	for(i in l) {
		cat(paste(i, "\n", sep=""));
	}
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



###########################################################################
# S3 object cqp_flist
###########################################################################



flist.cqp_corpus <- function(corpus, attribute, cutoff=0) {
	if (class(corpus) != "cqp_corpus") {
		stop("corpus: not a corpus object");
	}
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	
	positional <- cqi_attributes(cqp_corpus.name, "p");
	structural <- cqi_attributes(cqp_corpus.name, "s");
	
	qualified.attribute.name <- paste(cqp_corpus.name, attribute, sep=".");

	if (attribute %in% positional) {
		# TODO : 0-based ou 1-based
		max.id <- cqi_lexicon_size(qualified.attribute.name) - 1;
		ids <- 0:max.id;
		flist <- cqi_id2freq(qualified.attribute.name, ids);
		str <- cqi_id2str(qualified.attribute.name, ids);
		names(flist) <- str;
	} else {
		if (cqi_structural_attribute_has_value(qualified.attribute.name)) {
			# TODO : zero-based ?
			values <- cqi_struc2str(qualified.attribute.name, 0:(cqi_attribute_size(qualified.attribute.name)-1));
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
 # "flist(corpus, attribute, field, left.context, right.context)" --
 #
 # Create an S3 object holding a frequency list
 #
 # A flist is a named numeric vector.
 # 
 # Example:
 #              flist(my_sub_corpus, "lemma", "match", 4, 4)
 #
 # "left.context" and "right.context" define a span around the target word.
 #
 # if "target" is a character vector of length 2, such as c("match", "matchend"), the frequency list is computed
 # with all the word contained between match and matchend.
 # ------------------------------------------------------------------------
 ##
flist.cqp_subcorpus <- function(subcorpus, anchor, attribute, left.context=0, right.context=0, cutoff=0, offset=0) {

	if (class(subcorpus) != "cqp_subcorpus") {
		stop("subcorpus is not an S3 object of class cqp_subcorpus");
	}
	
	if (length(anchor) > 2 || length(anchor) < 1) {
		stop("anchor must be a vector of lenth 1 or 2");
	}
	
	cqp_subcorpus.name <- attr(subcorpus, "cqp_subcorpus.name");
	parent.cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");
	qualified.subcorpus.name <- paste(parent.cqp_corpus.name, cqp_subcorpus.name, sep=":");
	qualified.attribute <- paste(parent.cqp_corpus.name, attribute, sep=".");

	flist <- 0;
	
	if (length(anchor) == 1 & left.context == 0 & right.context == 0) {
 		print(qualified.subcorpus.name);
 		print(anchor);
 		print(attribute);
 		print(cutoff);
 		print(offset);
		print(qualified.subcorpus.name);
		flist <- cqi_fdist1(qualified.subcorpus.name, anchor, attribute, cutoff=cutoff, offset=offset);
		
		names(flist) <- cqi_cpos2str(
			paste(parent.cqp_corpus.name, attribute, sep="."),
			flist[,1]
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
 # "summary(flist)" --
 #
 # Applying generic method "summary" to flist object: print basic information.
 # 
 # Example:
 # TODO
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_flist <- function(x) {

	if (class(x) != "cqp_flist") {
		stop("x must be cqp_flist object");
	}

	cat("A frequency list\n");
	cat(paste("  Number of tokens:", sum(x), "\n"));
	cat(paste("  Number of types:", length(x), "\n"));

	cqp_corpus.name <- attr(flist, "cqp_corpus.name");
	# a cqp_flist made with a corpus
	if (!is.null(cqp_corpus.name)) {
		attribute <- attr(flist, "attribute");
		cat(paste("  Corpus:", cqp_corpus.name, "\n"));
		cat(paste("  Attribute:", attribute, "\n"));
	# otherwise, with a subcorpus
	} else {	
		cqp_subcorpus.name     <- attr(flist, "cqp_subcorpus.name");   
		parent.cqp_corpus.name <- attr(flist, "parent.cqp_corpus.name");
		anchor                 <- attr(flist, "anchor");
		left.context           <- attr(flist, "left.context");
		right.context          <- attr(flist, "right.context");
		attribute              <- attr(flist, "attribute");
		offset                 <- attr(flist, "offset");
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
print.cqp_flist <- function(flist, from=1, to=20) {
	cat("A frequency list\n");
	cat(paste("  Number of tokens:", sum(x), "\n"));
	cat(paste("  Number of types:", length(x), "\n"));
	print(c(flist[from:to]));
}


flist <- function(x, ...) UseMethod("flist");

###########################################################################
# S3 Object cqp_ftable
###########################################################################

## 
 # ------------------------------------------------------------------------
 # 
 # "ftable(corpus, structure, attribute, use.value=F, cutoff=0, subcorpus=NULL)" --
 #
 # Create an S3 object holding a frequency table accordint to various parameters
 #
 # A ftable is a three-column data.frame : modality_A modality_1 frequency. See reshape package for easy conversion into matrix.
 # 
 # Example:
 #              # case of fdist2
 #              # case of frequency by a structural attribute (each row is a text, a p, a chapter...)
 #              # case of frequency by value of a structural attribute with value
 #              # etc.
 # 
 # ------------------------------------------------------------------------
 ##
ftable.cqp_corpus <- function(corpus, attribute1, attribute2, 
	attribute1.use.id=FALSE, attribute2.use.id=FALSE,
	structural.attribute.unique.id=FALSE, subcorpus=NULL
) {
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	qualified.attribute1 <- paste(cqp_corpus.name, attribute1, sep=".");
	qualified.attribute2 <- paste(cqp_corpus.name, attribute2, sep=".");
	
	corpus_size <- cqi_attribute_size(paste(cqp_corpus.name, "word", sep="."));
	
	s_atts <- cqi_attributes(cqp_corpus.name, "s");
	p_atts <- cqi_attributes(cqp_corpus.name, "p");

	##
	## extract id.
	##
	
	att1 <- 0;
	if (attribute1 %in% s_atts) {
		# TODO array base
		att1 <- cqi_cpos2struc(qualified.attribute1, 0:(corpus_size-1));
	} else if (attribute1 %in% p_atts) {
		# TODO array base
		att1 <- cqi_cpos2id(qualified.attribute1, 0:(corpus_size-1));
	} else {
		stop(paste("Unknown attribute:", attribute1));
	}

	att2 <- 0;
	if (attribute2 %in% s_atts) {
		# TODO array base
		att2 <- cqi_cpos2struc(qualified.attribute2, 0:(corpus_size-1));
	} else if (attribute2 %in% p_atts) {
		# TODO array base
		att2 <- cqi_cpos2id(qualified.attribute2, 0:(corpus_size-1));
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


ftable.cqp_subcorpus <- function(subcorpus, anchor1, att1, anchor2, att2, cutoff=0) {
	parent.corpus <- attr(subcorpus, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(subcorpus, "cqp_subcorpus.name");
	qualified.sub_corpus.name <- paste(parent.corpus, cqp_subcorpus.name, sep=":");
	
	m <- cqi_fdist2(qualified.sub_corpus.name, anchor1, att1, anchor2, att2, cutoff=cutoff);

	att1.str <- cqi_id2str(paste(parent.corpus, att1, sep="."), m[,1]);
  	att2.str <- cqi_id2str(paste(parent.corpus, att2, sep="."), m[,2]);
 
	df <- data.frame(att1.str, att2.str, m[,3]);
#  	df <- data.frame(m[,1], m[,2], m[,3]);
	colnames(df) <- c(
		paste(anchor1, att1, sep="."),
		paste(anchor2, att2, sep="."),
		"freq");
	return(df);
}

ftable <- function(x, y, ...) UseMethod("ftable");


###########################################################################
# S3 Object KWIC
###########################################################################


kwic.cqp_subcorpus <- function(subcorpus,
	right.context=5,
	left.context=5,
	sort.anchor="matchend",
	sort.anchor.attribute="word",
	sort.anchor.offset=1
) {
	m <- .get.kwic.matrix(subcorpus, right.context, left.context);
	
	parent.cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");
	s <- .sort.kwic(parent.cqp_corpus.name, m, sort.anchor, sort.anchor.attribute, sort.anchor.offset);

	attr(s, "parent.cqp_corpus.name") <- parent.cqp_corpus.name;
	attr(s, "corpus") <- attr(subcorpus, "corpus");
	attr(s, "subcorpus") <- subcorpus;
	class(s) <- "kwic";
	return(s);
}

.get.kwic.matrix <- function(subcorpus, right.context=5, left.context=5) {
	parent.cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(subcorpus, "cqp_subcorpus.name");
	qualified_subcorpus_name <- paste(parent.cqp_corpus.name, cqp_subcorpus.name, sep=":");

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	left.boundary <- dump[,2] - left.context;
	dim(left.boundary) <- c(nrow(dump), 1);

	right.boundary <- dump[,1] + right.context;
	dim(right.boundary) <- c(nrow(dump), 1);

	dump <- cbind(dump, left.boundary, right.boundary);
	# TODO : anchor?
	colnames(dump) <- c("match", "matchend", "target", "keyword", "left", "right");
	return(dump);
}

.sort.kwic <- function(corpus.name, m, sort.anchor, sort.anchor.attribute, sort.anchor.offset) {
	# attention au -1
	cpos <- m[, sort.anchor];
	if (sort.anchor.offset != 0) {
		cpos + sort.anchor.offset;
	}

	qualified_attribute <- paste(corpus.name, sort.anchor.attribute, sep=".");
	str <- cqi_cpos2str(qualified_attribute, cpos);
	i <- order(str);
	
	sorted <- m[i,];
	return(sorted);
}

print.kwic <- function(kwic,
	print.function=function(x) cqi_cpos2str(paste(attr(kwic, "parent.cqp_corpus.name"), "word", sep="."), x),
	from=0,
	to=20,
	left.separator=" <<",
	right.separator=">> ",
	hit.char=15,
	left.char=40,
	right.char=40
    ) {
		
		if (from < 0 || to-from < 0 || to >= nrow(kwic)) {
			stop("0 <= from < to < nrow(kwic)");
		}
		
		lines <- character(to-from);
		for (i in 1:(to-from)) {
			# TODO : offset ?
			lines[i] <- .do_kwic_line(kwic[from + i,], print.function, left.separator, hit.char=hit.char, left.char=left.char, right.char=right.char);
		}
		
		return(lines);
	}

.do_kwic_line <- function(line,
	print.function,
	left.separator,
	right.separator,
	hit.char,
	left.char,
	right.char)
{
	
  left = print.function(line["left"]:(line["match"]-1));
  hit = print.function(line["match"]:line["matchend"]);
  right = print.function((line["matchend"]+1):line["right"]);

  left <- paste(left, collapse=" ");
  hit <- paste(hit, collapse=" ");
  hit <- paste(left.separator, hit, right.separator, sep="");
  right = paste(right, collapse=" ");
  
  # TODO : may be vectorized.
  format <- paste("%10d %", left.char, "s%", hit.char, "s%-", left.char, "s", sep="");
  line <- sprintf(format, line["match"], left, hit, right);
  return(line);
}

kwic <- function(x, y, ...) UseMethod("kwic");