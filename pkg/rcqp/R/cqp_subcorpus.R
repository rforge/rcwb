## 
 # ------------------------------------------------------------------------
 # 
 # A cqp_subcorpus
 #
 # ------------------------------------------------------------------------
 ##
setClass("cqp_subcorpus", contains="cqp_queryable", representation(parent="cqp_queryable", query="character"));

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
 # Easy extraction of tokens from the subcorpus
 #
 #              c <- corpus("DICKENS");
 #              sc <- subcorpus(c, '"interesting"')
 #              sc[1:10, "word"]
 #              #sc[region(match=1), "word"]
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("[", signature(x = "cqp_subcorpus", i = "missing", j = "character", drop = "logical"),
          function (x, i,j, ..., drop) {

    stop("to be implemented");

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
          
          })

#size.cqp_subcorpus <- function(x) {
#   stop("use 'nmatch' (for the number of matches) or 'N' (for the number of tokens)")
#}

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

setMethod("N", "cqp_subcorpus", function(subcorpus) {
    ntp <- part_size(subcorpus);
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

