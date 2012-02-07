# ===========================================================================
# File: "s3.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================


# dickens <- corpus("DICKENS");
# interesting <- subcorpus(dickens, "@[ADV]? \"interesting\"");
# summary(interesting);
#
# # default kwic:
# interesting
#
# # customized kwic:
# print(interesting, left.context=10, right.context=10)
#
# # frequency list of the subcorpus of -10 / +10 word around interesting :
# flist(interesting, "match", "lemma", left.context=10, right.context=10)

# merger des flist
# contingency table ˆ partir des structures
# format d'entrŽe de UCS : pour un mot, avoir les quatre paramtres facilement

# fonctions utilitaires comme "frequency(corpus, attribute, c("et", "oui", "non"))"

#
# Ici : attribute = positionnal attribute ; structure = structural attribute.
#

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
	cqp_corpus.name <- attr(x, "cqp_corpus.name");
	if (any(c(from, to) < 0)) {
		stop("Token ids cannot be < 0");
	}
	max <- cqi_attribute_size(paste(cqp_corpus.name, "word", sep=".")) - 1;
	if (any(c(from, to) > max)) {
		stop("Token ids cannot be greater than corpus size");
	}
	if (from >= to) {
		stop("'from' must be lesser than 'to'");
	}

# 	if ((to - from) > 100) {
# 		stop("The difference between 'to' and 'from' is > 100, not a convenient print");
# 	}

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
print.cqp_subcorpus <- function(x, positional.attribute="word", from=0, to=20) {
	if (class(x) != "cqp_subcorpus") {
		stop("x must be cqp_subcorpus object");
	}

	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	qualified.sub_corpus.name <- paste(parent.corpus, cqp_subcorpus.name, sep=":");

	if (any(c(from, to) < 0)) {
		stop("Token ids cannot be < 0");
	}
	# TODO
	max <- cqi_subcorpus_size(qualified.sub_corpus.name) - 1;
	if (any(c(from, to) > max)) {
		stop("Token ids cannot be greater than corpus size");
	}
	if (from >= to) {
		stop("'from' must be lesser than 'to'");
	}

}



###########################################################################
# S3 object cqp_flist
###########################################################################

region_sizes <- function(corpus, attribute) {
	if (class(corpus) != "cqp_corpus") {
		stop("corpus: not a corpus object");
	}
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	qualified_attribute <- paste(cqp_corpus.name, attribute, sep=".");
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

flist.cqp_corpus <- function(corpus, attribute, cutoff=0) {
	if (class(corpus) != "cqp_corpus") {
		stop("corpus: not a corpus object");
	}
	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	qualified.attribute.name <- paste(cqp_corpus.name, attribute, sep=".");
# TODO : 0-based ou 1-based
	max.id - 1 <- cqi_lexicon_size(qualified.attribute.name);
	ids <- 0:max.id;
	flist <- cqi_id2freq(qualified.attribute.name, ids);
	str <- cqi_id2str(qualified.attribute.name, ids);
	names(flist) <- str;
	
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

   attr(flist, "parent.corpus") <- corpus;
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
 # If the object is not a subcorpus, the arguments "field", "left.context" and "right.context" have no use.
 #
 # "left.context" and "right.context" define a span around the target word.
 #
 # if "target" is a character vector of length 2, such as c("match", "matchend"), the frequency list is computed
 # with all the word contained between match and matchens.
 # ------------------------------------------------------------------------
 ##
flist <- function(corpus, attribute, field, left.context=0, right.context=0, cutoff=0, offset=0) {

	
	if ("cqp_subcorpus" %in% class(corpus)) {
		if (length(target) == 1 & left.context == 0 & right.context == 0) {
			flist <- cqi_fdist1(attr(corpus, "cqp_subcorpus.name"), field, attribute, cutoff, offset);
			names(flist) <- cqi_cpos2str(
				paste(attr(corpus, "parent.cqp_corpus.name"), attribute, sep="."),
				"DICKENS.pos", flist[,1]
			);
		} else {
			stop("Not implemented yet");
			# manipuler les cpos/id jusqu'au bout (cpos, puis cpos2id, puis table(), puis les id2str.
		}		
	} else {

	}
	
   class(flist) <- "cqp_flist";

   attr(flist, "parent.corpus") <- corpus;
   attr(flist, "field") <- field;
   attr(flist, "left.context") <- left.context;
   attr(flist, "right.context") <- right.context;
   
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
summary.flist <- function(x) {

  # TODO : give parent (sub)corpus, parameters, number of tokens, number of types. 
  stop("Not implemented yet.");

}


###########################################################################
# S3 Object cqp_ftable
###########################################################################

####
#### cross tabulation plut™t ; en prenant en compte d'autres cas de figure : deux structural attributes avec value par exemple.
####


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
struct2ftable.cqp_corpus <- function(corpus, structure, attribute, use.value=F, cutoff=0, subcorpus=NULL) {

	cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
	if (! structure %in% cqi_attributes(cqp_corpus.name, "s")) {
		stop(paste("The structure", structure, "does not exist in this corpora."))
	}
	if (! attribute %in% cqi_attributes(cqp_corpus.name, "p")) {
		stop(paste("The attribute", attribute, "does not exist in this corpora."))
	}
	
	qualified.attribute.name <- paste(cqp_corpus.name, attribute, sep=".")
	qualified.structure.name <- paste(cqp_corpus.name, structure, sep=".")

	debug <- TRUE;

	# if cutoff, list the ids to be kept
	max.id <- cqi_lexicon_size(qualified.attribute.name);
	all_ids <- 0:(max.id - 1);
	if (cutoff > 0) {
		# warning: copied from flist
		freq <- cqi_id2freq(qualified.attribute.name, all_ids);
		if (cutoff >= max(freq)) {
			stop(paste("The cutoff (", cutoff, ") is greater than the greatest frequency in the corpus (", max(freq), ").", sep=""));
		}
		
		ids_kept <- all_ids[freq > cutoff];
	} else {
		ids_kept <- all_ids;
	}

    nbr.structure <- cqi_attribute_size(qualified.structure.name);
	all_ids_structure <- 0:(nbr.structure - 1);
	l <- vector(nbr.structure, mode="list")
	if (debug) print("Iteration on part");
	for (i in all_ids_structure) {
		# TODO filtrer les struc si ! is.null(subcorpus)
		boundaries <- cqi_struc2cpos(qualified.structure.name, i);
		ids <- cqi_cpos2id(qualified.attribute.name, boundaries[1]:boundaries[2])
		if (cutoff > 0) {
			ids <- ids[ids %in% ids_kept];
		}
		l[[i+1]] <- ids;
		# maybe not a pb if some vectors are zero-length:
		# the rep function bellow is ok with 0:
# 		> rep(c("un", "deux", "trois"), c(1, 0, 3))
# 		[1] "un"    "trois" "trois" "trois"
	}
	
	if (debug) print("Post-treatment");
	# the tokens are grouped by structure with the same value.
	if (use.value) {
		struc_values <- cqi_struc2str(qualified.structure.name, all_ids_structure);
		grouped <- tapply(l, struc_values, c);
		l <- lapply(grouped, unlist);
	}
	
	# replace all ids by the corresponding str, repeated.
	types <- cqi_id2str(qualified.attribute.name, all_ids); # and not ids_kept, so that the ids bellow address the correct form.
	token_id <- unlist(l);
	var2 <- types[token_id + 1];
	
	# generate a parallel vector with the repeated name of each struct. 
	if (use.value) {
		modalities <- struc_values;
	} else {
		modalities <- as.character(1:nbr.structure);
	}
	time <- sapply(l, length);
	var1 <- rep(modalities, time);
	
	done <- data.frame(var1, var2);
	
	if (debug) print("cross-tabulation");
	done <- xtabs(data=done, sparse=TRUE);

	# class <- cqp_ftable
	
	return(done);
}

ftable.cqp_subcorpus <- function(subcorpus, field1, att1, field2, att2, cutoff) {
	stop("Not implemented yet.");
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(ftable)" --
 #
 # Applying generic method "summary" to ftable object: print basic information.
 # 
 # Example:
 # TODO
 #
 # ------------------------------------------------------------------------
 ##
summary.ftable <- function(x) {

  # TODO : give parent (sub)corpus, parameters, number of modality in both variables, total frequency
  stop("Not implemented yet.");

}



###########################################################################
# S3 Object KWIC
###########################################################################


# c <- corpus("DICKENS")
# sc <- subcorpus(c, "\"interesting\"")
# kwic(sc, "word", 10, 10)

# cqi_query("DICKENS", "Foo", "\"the\"");

kwic <- function(subcorpus, attribute, left.span, right.span) {
	cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");	
    cqp_subcorpus.name <- attr(subcorpus, "cqp_subcorpus.name");
	qualified.cqp_subcorpus.name <- paste(cqp_corpus.name, cqp_subcorpus.name, sep=":");
	
	if (cqi_subcorpus_size(qualified.cqp_subcorpus.name) == 0) {
		stop("cannot create KWIC for zero-length subcorpus");
	}
	
	d <- cqi_dump_subcorpus(qualified.cqp_subcorpus.name);
	l <- .cpos2kwic.list(cqp_corpus.name, d[,1], d[,2], attribute, left.span, right.span);
	class(l) <- "kwic";
	return(l);
}


.cpos2kwic.list <- function(cqp_corpus.name, match, matchend, attribute, left.span, right.span) {
	if (length(match) != length(matchend)) {
		stop("match and matchend must match");
	}
	
	kwic <- vector(length(match), mode="list");
	for (i in 1:length(match)) {
		# TODO : attention ˆ ne pas aller < 0 ou > size-1
		lb <- match[i] - left.span;
		rb <- matchend[i] + right.span;
		
		lc = .range_to_string_vector(cqp_corpus.name, attribute, lb:(match-1));
		mat = .range_to_string_vector(cqp_corpus.name, attribute, match:matchend);
		rc = .range_to_string_vector(cqp_corpus.name, attribute, (matchend+1):rb);
		kwic[[i]] <- list(lc, mat, rc);
	}
	
	return(kwic);
	# 	cat(sprintf("%10d ", match));
	#   cat(sprintf("%40s", paste(lc, collapse=" ")));
	#   cat(sprintf("%10s", paste(" << ",paste(mat, collapse=" ")," >> ", sep="")));
	#   cat(sprintf("%-30s", paste(rc, collapse=" ")));
	#   cat("\n");
}

.range_to_string_vector <- function(corpus, attribute, cpos) {
	word = cqi_cpos2str(paste(corpus, attribute, sep="."), cpos);
	return(word);
}



###########################################################################
# private
###########################################################################


 
# ## 
#  # ------------------------------------------------------------------------
#  # 
#  # "kwic(subcorpus)" --
#  #
#  # Display matches of a subcorpus as key word in context
#  # 
#  # Example:
#  #              kwic("DICKENS", "\"interesting\"")
#  #
#  # ------------------------------------------------------------------------
#  ##
# kwic.cqp_subcorpus <- function(x, positional.attribute="word", from=0, to=20) {
# 	if (class(x) != "cqp_subcorpus") {
# 		stop("x must be cqp_subcorpus object");
# 	}
# 
# 	parent.corpus <- attr(x, "parent.cqp_corpus.name");
# 	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
# 	qualified.sub_corpus.name <- paste(parent.corpus, cqp_subcorpus.name, sep=":");
# 
# 	if (any(c(from, to) < 0)) {
# 		stop("Token ids cannot be < 0");
# 	}
# 	max <- cqi_subcorpus_size(qualified.sub_corpus.name) - 1;
# 	if (any(c(from, to) > max)) {
# 		stop("Token ids cannot be greater than corpus size");
# 	}
# 	if (from >= to) {
# 		stop("'from' must be lesser than 'to'");
# 	}
# }
# 


kwic.cqp_subcorpus <- function(subcorpus,
	right.context=5,
	left.context=5,
	sort.key="matchend",
	sort.key.attribute="word",
	sort.key.offset=1
) {
	m <- .get.kwic.matrix(subcorpus, right.context, left.context);
	
	parent.cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");
	s <- .sort.kwic(parent.cqp_corpus.name, m, sort.key, sort.key.attribute, sort.key.offset);

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
	colnames(dump) <- c("match", "matchend", "anchor", "keyword", "left", "right");
	return(dump);
}

.sort.kwic <- function(corpus.name, m, sort.key, sort.key.attribute, sort.key.offset) {
	# attention au -1
	cpos <- m[, sort.key];
	if (sort.key.offset != 0) {
		cpos + sort.key.offset;
	}

	qualified_attribute <- paste(corpus.name, sort.key.attribute, sep=".");
	str <- cqi_cpos2str(qualified_attribute, cpos);
	i <- order(str);
	
	sorted <- m[i,];
	return(sorted);
}

print.kwic <- function(kwic,
	print.function=function(x) cqi_cpos2str(paste(attr(kwic, "parent.cqp_corpus.name"), "word", sep="."), x),
	from=1,
	to=20,
	left.separator=" <<",
	right.separator=">> "
    ) {		
		if (from < 0 || to-from < 0 || to >= nrow(kwic)) {
			stop("0 <= from < to < nrow(kwic)");
		}
		
	lines <- character(to-from);
	for (i in 1:(to-from)) {
		lines[i] <- .do_kwic_line(kwic[from + i,], print.function, left.separator, right.separator);
	}
	
	return(lines);
}

.do_kwic_line <- function(line, print.function, left.separator, right.separator) {
	
  left = print.function(line["left"]:(line["match"]-1));
  hit = print.function(line["match"]:line["matchend"]);
  right = print.function((line["matchend"]+1):line["right"]);

  left <- paste(left, collapse=" ");
  hit <- paste(hit, collapse=" ");
  hit <- paste(left.separator, hit, right.separator, sep="");
  right = paste(right, collapse=" ");

  # NO : one line
#   left.size <- max(nchar(left));
#   hit.size <- max(nchar(hit));
#   right.size <- max(nchar(right));
  
  format <- paste("%10d %40s%15s%-40s", sep="");
  line <- sprintf(format, line["match"], left, hit, right);
  return(line);
}




###########################################################################
# private
###########################################################################

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

