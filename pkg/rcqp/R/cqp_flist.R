setClass("cqp_flist");

## 
 # ------------------------------------------------------------------------
 # 
 # "cqp_flist(cqp_attribute, cutoff)" --
 #
 # Create an S3 object holding a frequency list
 #
 # A cqp_flist is a named numeric vector.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              cqp_flist(sc, "lemma")
 #
 # ------------------------------------------------------------------------
 ##
setGeneric("cqp_flist", function(corpus, attribute, cutoff, ...) standardGeneric("cqp_flist"));

setMethod("cqp_flist", c("missing", "cqp_attr"), function(corpus, attribute, cutoff=0, ...) {
	c <- get.corpus(attribute);
	name <- .cqp_name(x, qualified=FALSE);
	cqp_flist(c, attribute, cutoff);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "cqp_flist(corpus, attribute, cutoff)" --
 #
 # Create a frequency list as a named numeric vector
 #
 # Example:
 #              c <- corpus("DICKENS")
 #              cqp_flist(c, "lemma")
 #
 # ------------------------------------------------------------------------
 ##

setMethod("cqp_flist", "cqp_corpus", function(corpus, attribute_name, cutoff=0, ...) {
  x <- corpus;
  cqp_corpus.name <- .cqp_name(x);
	positional <- cqi_attributes(cqp_corpus.name, "p");
	structural <- cqi_attributes(cqp_corpus.name, "s");
	
	name <- .cqp_name(attribute, qualified=TRUE);

	if (attribute %in% positional) {
		max.id <- cqi_lexicon_size(qualified.attribute.name) - 1;
		ids <- 0:max.id;
		flist <- cqi_id2freq(qualified.attribute.name, ids);
		str <- cqi_id2str(qualified.attribute.name, ids);
		names(flist) <- str;
	} else if (attribute %in% structural) {
		if (cqi_structural_attribute_has_values(qualified.attribute.name)) {
			ids <- 0:(cqi_attribute_size(qualified.attribute.name)-1);
			values <- cqi_struc2str(qualified.attribute.name, ids);
			t <- table(values);
			flist <- as.numeric(t);
			names(flist) <- names(t);
		} else {
			stop("no values on this structural attribute");
		}
	} else {
		stop("Unknown attribute");
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

   obj <- new("cqp_flist", flist, attribute=corpus$attribute_name);
   class(flist) <- "cqp_flist";

   attr(flist, "cqp_corpus.name") <- cqp_corpus.name;
   attr(flist, "attribute") <- attribute;
   return(flist);	
});

## 
 # ------------------------------------------------------------------------
 # 
 # "cqp_flist(subcorpus, anchor, attribute, left.context, right.context)" --
 #
 # Create a frequency list as a named numeric vector.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              sc <- subcorpus(c, '"interesting"')
 #              cqp_flist(sc, "match", "lemma", 4, 4)
 #
 # "left.context" and "right.context" extend the span of the counted tokens around the anchor.
 #
 # if "target" is a character vector of length 2, such as c("match", "matchend"), the frequency list is computed
 # with all the tokens contained between match and matchend.
 # ------------------------------------------------------------------------
 ##
setMethod("cqp_flist", "cqp_subcorpus",
    function(corpus, attribute_name, cutoff=0, anchor, left.context=0, right.context=0, offset=0, ...) {
	
	if (length(anchor) > 2 || length(anchor) < 1) {
		stop("anchor must be a vector of lenth 1 or 2");
	}
	
	x <- corpus;
	
	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	qualified.subcorpus.name <- .cqp_name(x);
	qualified.attribute <- paste(parent.cqp_corpus.name, attribute, sep=".");

	flist <- 0; 
	if (length(anchor) == 1 & left.context == 0 & right.context == 0) {
		fdist <- cqi_fdist1(qualified.subcorpus.name, anchor, attribute, cutoff=cutoff, offset=offset);
		id <- fdist[,1];
		flist <- fdist[,2];
		names(flist) <- cqi_id2str(
			paste(parent.cqp_corpus.name, attribute, sep="."),
			id
		);
	} else {
		dump <- cqi_dump_subcorpus(qualified.subcorpus.name);
		
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

   attr(flist, "cqp_subcorpus.name") <- attr(x, "cqp_subcorpus.name");  
   attr(flist, "parent.cqp_corpus.name") <- parent.cqp_corpus.name;
   attr(flist, "anchor") <- anchor;
   attr(flist, "left.context") <- left.context;
   attr(flist, "right.context") <- right.context;
   attr(flist, "attribute") <- attribute;
   attr(flist, "offset") <- offset;

   return(flist);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "summary(cqp_flist)" --
 #
 # Applying generic method "summary" to cqp_flist object: print basic information.
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("summary", signature(object = "cqp_corpus"), function(object){
	cat("A frequency list\n");
	cat(paste("  Number of tokens:", sum(object), "\n"));
	cat(paste("  Number of types:", length(object), "\n"));

	cqp_corpus.name <- attr(object, "cqp_corpus.name");

	if (!is.null(cqp_corpus.name)) {
		attribute <- attr(object, "attribute");
		cat(paste("  Corpus:", cqp_corpus.name, "\n"));
		cat(paste("  Attribute:", attribute, "\n"));
	} else {
		cat(paste("  Subcorpus:", attr(object, "cqp_subcorpus.name"), "\n"));
		cat(paste("  Parent corpus:", attr(object, "parent.cqp_corpus.name"), "\n"));
		cat(paste("  anchor:", attr(object, "anchor"), "\n"));
		cat(paste("  left.context:", attr(object, "left.context"), "\n"));
		cat(paste("  right.context:", attr(object, "right.context"), "\n"));
		cat(paste("  attribute:", attr(object, "attribute"), "\n"));
		cat(paste("  offset:", attr(object, "offset"), "\n"));
	}
});

## 
 # ------------------------------------------------------------------------
 # 
 #
 # ------------------------------------------------------------------------
 ##
setMethod("print", signature(x="cqp_kwic"), function(x) {
	df <- data.frame(names(x), as.numeric(x));
	colnames(df) <- c("type", "frequency");
	print(df, row.names=FALSE);
});

