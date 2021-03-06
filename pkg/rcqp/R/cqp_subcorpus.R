## 
 # ------------------------------------------------------------------------
 # 
 # A cqp_subcorpus
 #
 # ------------------------------------------------------------------------
 ##
setClass("cqp_subcorpus", contains="cqp_queryable", representation(corpus="cqp_corpus", superset="cqp_queryable", query="character"));
# todo subset plutôt que parent

setMethod(".cqp_name", "cqp_subcorpus", function(x, qualified=TRUE) {
  cqp_name <- x@cqp_name;
  if (qualified) {
    parent <- x@parent@cqp_name
    cqp_name <- paste(parent, cqp_name, sep=":");
  }
  return(cqp_name);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "nmatch(subcorpus)" --
 #
 # Get the number of matches (pattern found) of a subcorpus
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              sc <- subcorpus(c, '"interesting"')
 #              nmatch(sc) 
 # 
 # ------------------------------------------------------------------------
 ##
setGeneric("nmatch", function(subcorpus) standardGeneric("nmatch"));

setMethod("nmatch", "cqp_subcorpus", function(subcorpus) {
	qualified.name <- .cqp_name(x);
	return(cqi_subcorpus_size(qualified.name));
});

## 
 # ------------------------------------------------------------------------
 # 
 # "part_size(subcorpus)" --
 #
 # Get the number of tokens of each part (matched pattern) of a subcorpus
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              sc <- subcorpus(c, '"interesting"')
 #              part_size(sc) 
 # 
 # ------------------------------------------------------------------------
 ##
setGeneric("part_size", function(subcorpus) standardGeneric("part_size"));

setMethod("part_size", "cqp_subcorpus", function(subcorpus) {
	cqp_corpus.name <- attr(subcorpus, "parent.cqp_corpus.name");
	qualified_subcorpus_name <- .cqp_name(subcorpus);

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	size <- mapply(`-`, (dump[,2]+1), dump[,1])
	return(unlist(size));
});

## 
 # ------------------------------------------------------------------------
 # 
 # "N(subcorpus)" --
 #
 # Get the number of tokensof a subcorpus
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              sc <- subcorpus(c, '"interesting"')
 #              N(sc) 
 # 
 # ------------------------------------------------------------------------
 ##

# the generic is in cqp_corpus.R

setMethod("N", "cqp_subcorpus", function(corpus) {
    ntp <- part_size(corpus);
    nt <- sum(ntp);
    return(nt);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "summary(subcorpus)" --
 #
 # Applying generic method "summary" to subcorpus object: print basic information.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              sc <- subcorpus(c, '"interesting"')
 #              summary(sc)
 #
 # ------------------------------------------------------------------------
 ##
setMethod("summary", signature(object = "cqp_subcorpus"), function(object){
	parent_corpus.name <- object@parent@cqp_name;
	cat(paste("Parent corpus name:", parent_corpus.name, "\n"));

	query <- object@query;
	cat(paste("Query:", query, "\n"));

	nm <- nmatch(object);
	cat(paste("Number of matches:", nm, "\n"));

	nt <- N(object);
	cat(paste("Number of tokens:", n, "\n"));
})

## 
 # ------------------------------------------------------------------------
 # 
 # "print(subcorpus)" --
 #
 # Applying generic method "print" to corpus object: print first kwic lines of the corpus
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              sc <- subcorpus(c, '"interesting"')
 #              print(sc)
 #
 # ------------------------------------------------------------------------
 ##
 
# setMethod("show", signature(object="FullText"), function(object) {
#  print(object);
# });

setMethod("print", signature(x="cqp_subcorpus"), function(x, positional.attribute="word", from=0, to=10, ...) {
	nm <- nmatch(x);
	max.line <- nm - 1;
	if (to > max.line) {
		stop(paste("Max line:", max.line, "; max lines requested:", to));
	}
	k <- cqp_kwic(x);
	print(k, from=from, to=to);
});

## 
 # ------------------------------------------------------------------------
 #
 # Convert the corpus into a data.frame representation.
 #
 # ------------------------------------------------------------------------
 ##
setMethod("as.data.frame", "cqp_subcorpus", function(x, from=1, to=nmatch(x), use_value=TRUE) {
   if (from < 1 || from >= to || to < nmatch(x))
     stop("illegal value for 'from' or 'to'");
   return(.cqp_subcorpus2matrix(x, from, to, use_value));
});

## 
 # ------------------------------------------------------------------------
 #
 # Convert the corpus into a list representation (character given by 
 # a positional attribute ; splitted in list elements by a structural attribute).
 #
 # ------------------------------------------------------------------------
 ##
setMethod("as.list",
      c("cqp_subcorpus"),
      function(x, positional=x$word, structural=x$text, from=1, to=nmatch(x))
  {
   if (from < 1 || from >= to || to < nmatch(x))
     stop("illegal value for 'from' or 'to'");

  x <- .cqp_subcorpus2matrix(x, from, to, use_value);

  y <- split(x[, .cqp.name(positional)], x$Match);

  return(y);
});

######################################################################
#
# Privates
#
######################################################################

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

.is_cqp_subcorpus <- function(x) {
	if (class(x) == "cqp_subcorpus") {
		return(TRUE);
	} else {
		return(FALSE);
	}
}

.cqp_subcorpus2matrix <- function(x, from, to, use_value=use_value) {
	corpus_name <- .cqp_name(.get.corpus(x));
	qualified_subcorpus_name <- .cqp_name(x, qualified=TRUE);

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	dump <- dump[from:to,];
	cpos_list <- mapply(`:`, dump[,1], dump[,2])
	cpos <- unlist(cpos_list);
    m <- .cqp_cpos2matrix(corpus_name, cpos, use_value);
    m$Match <- rep(1:length(cpos_list), each=sapply(cpos_list, length));
    return(m);
}
