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
 #              subcorpus("DICKENS", '"interesting"')
 # 
 # ------------------------------------------------------------------------
 ##
subcorpus <- function(corpus, query) {
	if (!.is_cqp_corpus(corpus)) {
		stop("corpus: not a corpus object");
	}
	parent.cqp_corpus.name <- .cqp_name(corpus);
	
    cqp_subcorpus.name <- .generate.cqp_subcorpus.name(parent.cqp_corpus.name);
    cqi_query(parent.cqp_corpus.name, cqp_subcorpus.name, query);
	
	x <- 0;
    class(x) <- "cqp_subcorpus";

    attr(x, "cqp_subcorpus.name") <- cqp_subcorpus.name;
    attr(x, "parent.cqp_corpus.name") <- parent.cqp_corpus.name;
    attr(x, "query") <- query;
	return(x);
}

.cqp_subcorpus2matrix <- function(x, use_value=use_value) {
	cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	qualified_subcorpus_name <- .cqp_name(x);

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	cpos_list <- mapply(`:`, dump[,1], dump[,2])
	cpos <- unlist(cpos_list);
    m <- .cqp_cpos2matrix(cqp_corpus.name, cpos, use_value);
#    m$Match <- rep(1:length(cpos_list), each=sapply(cpos_list, length));
    return(m);
}

#Erreur dans `$<-.data.frame`(`*tmp*`, "Match", value = c(1L, 1L, 1L, 1L,  : 
#  replacement has 76475 rows, data has 61443


## 
 # ------------------------------------------------------------------------
 # 
 # "summary(subcorpus)" --
 #
 # Applying generic method "summary" to subcorpus object: print basic information.
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", '"interesting"')
 #              summary(sc)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_subcorpus <- function(object, ...) {
	parent_corpus.name <- attr(object, "parent.cqp_corpus.name");
	cat(paste("Parent corpus name:", parent_corpus.name, "\n"));

	size <- size(object);
	cat(paste("Number of matches:", size, "\n"));

	query <- attr(object, "query");
	cat(paste("Query:", query, "\n"));
}



## 
 # ------------------------------------------------------------------------
 # 
 # "print(subcorpus)" --
 #
 # Applying generic method "print" to corpus object: print first kwic lines of the corpus
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", '"interesting"')
 #              print(sc)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_subcorpus <- function(x, positional.attribute="word", from=0, to=10, ...) {
	size <- size(x);
	max.line <- size - 1;
	if (to > max.line) {
		stop(paste("Max line:", max.line, "; max lines requested:", to));
	}
	
	k <- cqp_kwic(x);
	print(k, from=from, to=to);
}



.generate.cqp_subcorpus.name <- function(corpus) {
  subcorpora.name <- cqi_list_subcorpora(corpus);
  name <- .generate.name();
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



.cqp_name.cqp_subcorpus <- function(x, qualified=TRUE) {
	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	cqp_subcorpus.name <- attr(x, "cqp_subcorpus.name");
	if (qualified) {
		return(paste(parent.corpus, cqp_subcorpus.name, sep=":"));
	} else {
		return(cqp_subcorpus.name);
	}
}



size.cqp_subcorpus <- function(x) {
	qualified.name <- .cqp_name(x);
	return(cqi_subcorpus_size(qualified.name));
}



.is_cqp_subcorpus <- function(x) {
	if (class(x) == "cqp_subcorpus") {
		return(TRUE);
	} else {
		return(FALSE);
	}
}
