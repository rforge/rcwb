##############################################################
# 'make easy to read corpus as a tabulated file (as typically produced by part-of-speech tagger).
setClass("cqp_corpus", representation(cqp_corpus.name="character"));

Appeler TabulatedCWB ou CQPCorpus
implémenter les fonctions de cette classe
size() -> N()

##############################################################
setGeneric("N", function(corpus) standardGeneric("N"));




# Constructor
corpus <- function(corpus.name) {
    if (! corpus.name %in% cqi_list_corpora()) {
	    stop("This corpus does not exist, see cqi_list_corpora()");
	}
	new("cqp_corpus", cqp_corpus.name=corpus.name)
}

.is_cqp_corpus <- function(x) {
	if (class(x) == "cqp_corpus") {
		return(TRUE);
	} else {
		return(FALSE);
	}
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
	
	tokens <- size(object);
	cat(paste("Number or tokens in the corpus:", tokens, "\n"));

	cat(paste("Positional attributes (", length(p_attributes), ")\n", sep=""));
	for (p in p_attributes) {
		#FIXME BUG
		summary(object[[ p ]]);
	}

	cat(paste("Structural attributes (", length(s_attributes), ")\n", sep=""));
	for (s in s_attributes) {
		if (tolower(s) != s) next;
		summary(object[[ s ]]);
	}

	cat(paste("Alignement attributes (", length(a_attributes), ")\n", sep=""));
	for (a in a_attributes) {
		summary(object[[ a ]]);
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
print.cqp_corpus <- function(x, from=0, to=20, use_value=TRUE, ...) {
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

	printed <- .cqp_corpus2matrix(x, from, to, use_value=use_value);

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
	size <- size(corpus);
	mat <- .cqp_corpus2matrix(corpus, 0, size);
	write.table(mat, file=filename, ...);
}


.cqp_corpus2matrix <- function(x, from, to, use_value=use_value) {	
	max <- size(x) - 1;
	if (from < 0) {
	    stop("'from' must be equal or greater than 0");
	}
	
	if (to >= max) {
	    stop(paste("'to' must be smaller than the number of tokens (", max, ")"));
	}

	cqp_corpus.name <- .cqp_name(x);	 
	token_id=from:to;
    .cqp_cpos2matrix(cqp_corpus.name, token_id, use_value);
}

.cqp_cpos2matrix <- function(cqp_corpus.name, cpos, use_value=use_value) {	
	positional <- cqi_attributes(cqp_corpus.name, "p");
	nbr_positional <- length(positional);

	structural <- cqi_attributes(cqp_corpus.name, "s");
	# FIXME : upper case name produces "Syntax error" with cqi_cpos2struc ?
	structural <- structural[grep("[a-z]+", structural)]
	nbr_structural <- length(structural);

	nbr_token <- length(cpos);
	printed <- data.frame(matrix("", nrow=nbr_token, ncol=nbr_positional+nbr_structural));
	rownames(printed) <- cpos;
	colnames(printed) <- c(structural, positional);

	for (i in 1:nbr_structural) {
		qualified_structural_attribute <- paste(cqp_corpus.name, structural[i], sep=".");
		ids <- cqi_cpos2struc(qualified_structural_attribute, cpos);
		printed[,i] <- ids
	}
	
	for (i in 1:nbr_positional) {
		qualified_positional_attribute <- paste(cqp_corpus.name, positional[i], sep=".");
		ids <- cqi_cpos2str(qualified_positional_attribute, cpos);
		printed[,i+nbr_structural] <- ids
	}

	return(printed);
}

.cqp_name.cqp_corpus <- function(x, attribute=NULL) {
	corpus_name <- attr(x, "cqp_corpus.name");
	if (is.null(attribute)) {
		return(corpus_name);
	} else {
		return(.attr_qualified_name(corpus_name, attribute);
	}
}

.attr_qualified_name <- function(corpus_name, attribute) {
  return(paste(corpus_name, attribute, sep="."));
}

size.cqp_corpus <- function(x) {	
	word.attribute <- .cqp_name(x, "word");
	return(cqi_attribute_size(word.attribute));
}
