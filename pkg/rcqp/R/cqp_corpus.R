setClass("cqp_queryable", contains="cqp_object");

setClass("cqp_corpus", contains="cqp_queryable");

## 
 # ------------------------------------------------------------------------
 # 
 # A cqp_corpus
 #
 # ------------------------------------------------------------------------
 ##
setClass("cqp_corpus", representation(cqp_name="character"));

## 
 # ------------------------------------------------------------------------
 # 
 # Constructor
 # c <- corpus("DICKENS")
 #
 # ------------------------------------------------------------------------
 ##
corpus <- function(name) {
  if (is.null(name)) {
    stop("'name' cannot be null");
  }
  if (! name %in% cqi_list_corpora()) {
    stop(paste("No corpus with name", name, "in the registry; see cqi_list_corpora()"));
  }
  obj <- new("cqp_corpus", cqp_name=name);
  return(obj);
}

## 
 # ------------------------------------------------------------------------
 # 
 # "subcorpus(corpus, query)" --
 #
 # Create a subcorpus.
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              subcorpus(c, '"interesting"')
 # 
 # ------------------------------------------------------------------------
 ##
setGeneric("subcorpus", function(corpus, query) standardGeneric("subcorpus"));

setMethod("subcorpus", "cqp_corpus", function(corpus, query) {
	if (!.is_cqp_corpus(corpus)) {
	  stop("'corpus' must be a cqp_corpus object");
	}
	parent.cqp_name <- .cqp_name(corpus);

    cqp_subcorpus.name <- .generate.cqp_subcorpus.name(parent.cqp_name);
    cqi_query(parent.cqp_name, cqp_subcorpus.name, query);

    obj <- new("cqp_subcorpus", cqp_name=cqp_subcorpus.name, parent=corpus, query=query);
    return(obj);
});

##
 # ------------------------------------------------------------------------
 # 
 # "[" operator : retreive token from the corpus.
 #
 #              c <- corpus("DICKENS");
 #              c[1:10, "word"];
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("[", signature(x = "cqp_corpus", i = "ANY", j = "character", drop = "logical"),
          function (x, i,j, ..., drop) {
  i <- substitute(i);
  if (is.numeric(i)) {
    attr <- x$j;
    return(cqi_cpos2str(.cqp_names(attr), i);
  } else if (is.call(i)) {
    region <- function(...) {
      args <- match.call(expand.dots = FALSE)$`...`;
      if (length(args) > 1) {
        stop("only one argument is allowed in 'region'");
      }
      key <- names(args)[1];
      val <- args[[1]];
      cpos <- integer(0);
      attr <- x$key;
        if (is.numeric(val)) {
          for (v in val) {
            range <- cqi_struc2cpos(.cqp_name(attr), val)
            cpos <- c(cpos, range[1]..range[2]);
          }
        } else if (is.character(val)) {
          if (!is(attr, "cqp_attr_structural")) {
            stop("no character indexing with non-structural attribute");
          }
          strucs <- get.struc(val);
          for (struc in strucs) {
            range <- cqi_struc2cpos(.cqp_name(attr), struc)
            cpos <- c(cpos, range[1]..range[2]);
          }
        }
      }
    }
    query <- function(...) {
    }
    i <- eval(i, envir=1, enclos=parent.frame());
    if (is.numeric(i) || is.integer(i)) {
    } else if {
    } else {
      stop("Illegal request");
    }
  } else {
    stop("unknown call");
  }
});

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
setMethod("summary", signature(object = "cqp_corpus"), function(object){
	cqp_name <- .cqp_name(object);
	cat(paste(cqp_name, "\n"));
		
	p_attributes <- sort(cqi_attributes(cqp_name, "p"));
	s_attributes <- sort(cqi_attributes(cqp_name, "s"));
	a_attributes <- sort(cqi_attributes(cqp_name, "a"));
	
	tokens <- N(object);
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
});

## 
 # ------------------------------------------------------------------------
 # 
 # N = number of tokens of a corpus
 # c <- corpus("DICKENS")
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("N", function(corpus) standardGeneric("N"));

setMethod("N", "cqp_corpus", function(corpus) {
	word.attribute <- .cqp_name(corpus, "word");
	return(cqi_attribute_size(word.attribute));
});

setGeneric("size", function(corpus) standardGeneric("size"));

setMethod("size", "cqp_corpus", function(corpus) {
  warning("deprecated; use 'N' rather");
  N(corpus);
});

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
setMethod("print", signature(x="cqp_corpus"), function(x, from=0, to=20, use_value=TRUE, ...) {
    printed <- as.data.frame(x, from=from, to=to, use_value=use_value);
	print(printed);
})

## 
 # ------------------------------------------------------------------------
 #
 # Convert the corpus into a data.frame representation.
 #
 # ------------------------------------------------------------------------
 ##
setMethod("as.data.frame", "cqp_corpus", function(x, row.names = from:to, optional = FALSE, from, to, use_value=TRUE) {
   .check_cpos(x, from, to);

	df <- .cqp_corpus2matrix(x, from, to, use_value=use_value);
	return(df);
});

## 
 # ------------------------------------------------------------------------
 #
 # Convert the corpus into a list representation (character given by 
 # a positional attribute ; splitted in list elements by a structural attribute).
 #
 # ------------------------------------------------------------------------
 ##
setMethod("as.list", c("cqp_corpus", "cqp_attr_positional", "cqp_attr_structural"), function(x, positional=x$word, structural=x$text, from=0, to=N(x)-1) {
  .check_cpos(x, from, to);

  cpos <- from:to;
  tokens <- cqp_cpos2str(.cqp_name(positional, TRUE), cpos);
  parts <- cqp_cpos2struc(.cqp_name(structural, TRUE), cpos);

  l <- split(tokens, parts);
  return(l);
});

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
	size <- N(corpus);
	mat <- .cqp_corpus2matrix(corpus, 0, size);
	write.table(mat, file=filename, ...);
}

## 
 # ------------------------------------------------------------------------
 #
 # Is the corpus a cqp_corpus ?
 #
 # ------------------------------------------------------------------------
 ##
setGeneric(".is_cqp_corpus", function(corpus) standardGeneric(".is_cqp_corpus"));

setMethod(".is_cqp_corpus", "ANY", function(corpus) {
  if (class(corpus) == "cqp_corpus") {
    return(TRUE);
  } else {
    return(FALSE);
  }
});

## 
 # ------------------------------------------------------------------------
 #
 #
 #
 # ------------------------------------------------------------------------
 ##
.cqp_corpus2matrix <- function(x, from, to, use_value=use_value) {	
   .check_cpos(x, from, to);

	cqp_name <- .cqp_name(x);	 
	token_id=from:to;
    .cqp_cpos2matrix(cqp_name, token_id, use_value);
}

.cqp_cpos2matrix <- function(cqp_name, cpos, use_value=use_value) {	
	positional <- cqi_attributes(cqp_name, "p");
	nbr_positional <- length(positional);

	structural <- cqi_attributes(cqp_name, "s");
	# FIXME : upper case name produces "Syntax error" with cqi_cpos2struc ?
	structural <- structural[grep("[a-z]+", structural)]
	nbr_structural <- length(structural);

	nbr_token <- length(cpos);
	printed <- data.frame(matrix("", nrow=nbr_token, ncol=nbr_positional+nbr_structural));
	rownames(printed) <- cpos;
	colnames(printed) <- c(structural, positional);

	for (i in 1:nbr_structural) {
		qualified_structural_attribute <- paste(cqp_name, structural[i], sep=".");
		ids <- cqi_cpos2struc(qualified_structural_attribute, cpos);
		if (use_value & cqi_structural_attribute_has_values(qualified_structural_attribute) {
		  printed[,i] <- cqi_struc2str(qualified_structural_attribute, ids);
		} else {
		  printed[,i] <- ids
		}
	}
	
	for (i in 1:nbr_positional) {
		qualified_positional_attribute <- paste(cqp_name, positional[i], sep=".");
		ids <- cqi_cpos2str(qualified_positional_attribute, cpos);
		printed[,i+nbr_structural] <- ids
	}

	return(printed);
}

.check_cpos <- function(corpus, from, to) {
	max <- N(corpus) - 1;
	if (from < 0) {
	    stop("'from' must be equal or greater than 0");
	}
	if (to >= max) {
	    stop(paste("'to' must be smaller than the number of tokens (", max, ")"));
	}
	if (from >= to) {
		stop("'from' must be lesser than 'to'");
	}
}

