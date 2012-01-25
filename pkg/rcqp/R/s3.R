# ===========================================================================
# File: "s3.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

# Four s3 objects are defined: 
# - corpus
# - subcorpus (a sub-type of corpus)
# - flist (frequency list)
# - ftable (frequency table)
# Each of these objects has summary(), print()
#
#
# Session example:

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

###########################################################################
# constructors for corpus, subcorps, flist, ftable
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
	if ("cqp_corpus" %in% class(corpus)) {
		stop("corpus: not a corpus object");
	}
	
    cqp_subcorpus.name <- .generate.cqp_subcorpus.name(corpus);
    cqi_query(corpus, cqp_subcorpus.name, query);
	
	x <- 0;
    class(x) <- c("cqp_subcorpus", "cqp_corpus");

    attr(x, "cqp_subcorpus.name") <- paste(attr(corpus, "cqp_corpus.name"), cqp_subcorpus.name, sep=":");
    attr(x, "parent.cqp_corpus.name") <- attr(corpus, "cqp_corpus.name");
    attr(x, "query") <- query;
	return(x);
}



## 
 # ------------------------------------------------------------------------
 # 
 # "flist(corpus, attribute, field, left.context, right.context)" --
 #
 # Create an S3 object holding a frequency list, both with a corpus or a subcorpus.
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
	if ("cqp_corpus" %in% class(corpus)) {
		stop("corpus: not a corpus object");
	}
	
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
		cqp_corpus.name <- attr(corpus, "cqp_corpus.name");
		qualified.attribute.name <- paste(cqp_corpus.name, attribute, sep=".");
		max.id <- cqi_lexicon_size(qualified.attribute.name);
		flist <- cqi_id2freq(qualified.attribute.name, 0:(max.id - 1));
		str <- cqi_id2str(qualified.attribute.name, (0:(max.id - 1)));
		names(flist) <- str;
		
		if (cutoff > 0) {
			flist <- flist[flist > cutoff];
		}
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
 # "ftable(corpus, attribute, field, left.context, right.context)" --
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

	return(done);
}

ftable.cqp_subcorpus <- function(subcorpus, field1, att1, field2, att2, cutoff) {
	stop("Not implemented yet.");
}


###########################################################################


###########################################################################
# summary
###########################################################################



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
  parent.corpus <- att(x, parent.corpus.name);
  subcorpus <- att(x, cqp.name);
  query <- att(x, query);
  size <- cqi_subcorpus_size(paste(parent.corpus, sep=":"));

  cat(paste("Parent corpus name:", parent.corpus, "\n"));
  cat(paste("Query:", query, "\n"));
  cat(paste("Size:", size, "\n"));
  # TODO : some samples of the first lines
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(corpus)" --
 #
 # Applying generic method "summary" to corpus object: print basic information.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              summary(c)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_corpus <- function(x) {

  # TODO : list p and a attribute, size, etc.
  stop("Not implemented yet.");

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


###########################################################################
# print
###########################################################################



## 
 # ------------------------------------------------------------------------
 # 
 # "print(corpus)" --
 #
 # Applying generic method "print" to corpus object: print first lines of the corpus
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              print(c)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_corpus <- function(x) {

  # TODO : print the first lines
  stop("Not implemented yet.");

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
print.cqp_subcorpus <- function(x) {

  # TODO : print the first kwic lines
  stop("Not implemented yet.");

}



###########################################################################


###########################################################################
# private
###########################################################################



.generate.cqp_subcorpus.name <- function(corpus) {
  subcorpora.name <- cqi_list_subcorpora(corpus)
  name <- .generate.name()
  while (name %in% subcorpora) {
    name <- .generate.name();
  }
  return(name);
}



.generate.name <- function() {
  initial <- LETTERS[sample(1:26, 1)];
  other <- c(LETTERS, letters)[sample(1:(26*2), 9)];
  other <- paste(other, collapse="");
  return(paste(initial, other, sep=""));
}

