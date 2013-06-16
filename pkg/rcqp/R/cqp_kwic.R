setClass("cqp_kwic", representation(kwic="matrix", corpus="cqp_corpus", subcorpus="cqp_subcorpus", right.context="numeric", left.context="numeric"));

## 
 # ------------------------------------------------------------------------
 # 
 # "kwic(subcorpus)" --
 #
 # Create a kwic representation of a subcorpus.
 # 
 # Example:
 #              c <- corpus("DICKENS");
 #              sc <- subcorpus(c, '"interesting"')
 #              k <- kwic(sc)
 # 
 # ------------------------------------------------------------------------
 ##
setGeneric("kwic", function(subcorpus, ...) standardGeneric("kwic"));

setMethod("kwic", "cqp_subcorpus", function(subcorpus, right.context=20, left.context=20) {
	nm <- nmatch(subcorpus);
	if (nm == 0) {
		stop("empty subcorpus");
	}

	s <- .get.kwic.matrix(subcorpus, right.context, left.context);	

    obj <- new("cqp_kwic", cqp_corpus=subcorpus@parent, subcorpus=subcorpus, right.context=right.context, left.context=left.context);
    return(obj);
});

## 
 # ------------------------------------------------------------------------
 # 
 # "sort(kwick)" --
 #
 # Sort a kwic
 # 
 # Example:
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("sort", "cqp_kwic", function(x, decreasing=FALSE, sort.anchor="match", sort.attribute="word", sort.offset=0, ...) {
	if (!class(x) == "cqp_kwic") {
		stop("x must be a cqp_kwic object");
	}
	
	if (! sort.anchor %in%  c("match", "matchend", "target", "keyword")) {
		stop('sort.anchor must be in c("match", "matchend", "target", "keyword")');
	}

	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	if (! sort.attribute %in%  cqi_attributes(parent.cqp_corpus.name, "p")) {
		stop('sort.attribute must be an existing positional attribute');
	}	
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
});

## 
 # ------------------------------------------------------------------------
 # 
 # "print(kwic)" --
 #
 # Pretty-print of a kwic
 # 
 # Example:
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("print", signature(x="cqp_kwic"), function(x, 
	from=0,
	to=min(20, nrow(x)-1),
	print_tokens=function(x, cpos) cqi_cpos2str(paste(x@corpus, "word", sep="."), cpos),
	left.separator=" <<",
	right.separator=">> ",
	...) {

    matrix.lines <- as.matrix(x, from, to, print_tokens);

	requested.left.char= x@left.context;
	requested.right.char= x@right.context;

	format <- paste("%10d %", requested.left.char, "<<s%", requested.center.char, ">>s%-", requested.right.char, "s", sep="");
	r_from_to <- (from:to) + 1;
	lines <- sprintf(format, x[r_from_to, "match"], left, matrix.lines[,2], right);
	
	for(i in lines) {
		cat(paste(i, "\n", sep=""));
	}
})

## 
 # ------------------------------------------------------------------------
 # 
 # "as.matrix(kwic)" --
 #
 # a matrix of tokens, one line per match.
 # 
 # Example:
 # 
 # ------------------------------------------------------------------------
 ##
setMethod("as.matrix", signature(x="cqp_kwic"), function(x, 
	from=0,
	to=min(20, nrow(x)-1),
	print_tokens=function(x, cpos) cqi_cpos2str(paste(attr(x, "parent.cqp_corpus.name"), "word", sep="."), cpos),
	...) {
	if (from < 0) {
		stop("'from' must be greater than 0");
	}
	if (from > to) {
		stop("'to' must be greater than from");
	}
	if (to >= nrow(x)) {
		stop("'to' must be lesser than the number of match of the subcorpus");
	}

	requested.left.char= x@left.context;
	requested.right.char= x@right.context;

	nbr.lines <- to-from+1;
	matrix.lines <- matrix("", nrow=nbr.lines, ncol=3);
	for (i in 1:nbr.lines) {
		l <- x[from + i,];
		matrix.lines[i, 1] <- paste(
			print_tokens(x, l["left"]:(l["match"]-1)),
			collapse=" "
		);

		c1 <- print_tokens(x, l["match"]:l["matchend"]);
		c2 <- c(c1);
		matrix.lines[i, 2] <- paste(c2, collapse=" ");
		
		matrix.lines[i, 3] <- paste(
			print_tokens(x, (l["matchend"]+1):l["right"]),
			collapse=" "
		);
	}

	left.nchar <- nchar(matrix.lines[,1]);
	center.nchar <- nchar(matrix.lines[,2]);
	right.nchar <- nchar(matrix.lines[,3]);
	
	left <- substr(matrix.lines[,1], left.nchar - requested.left.char, left.nchar);
	requested.center.char <- max(center.nchar);
	right <- substr(matrix.lines[,3], 1, right.nchar - (right.nchar - requested.right.char));

    return(matrix.lines);
});

.get.kwic.matrix <- function(x, right.context, left.context) {
	qualified_subcorpus_name <- .cqp_name(x);

	dump <- cqi_dump_subcorpus(qualified_subcorpus_name);
	left.boundary <- pmax(dump[,1] - left.context, 0);
	dim(left.boundary) <- c(nrow(dump), 1);

	parent.cqp_corpus.name <- attr(x, "parent.cqp_corpus.name");
	corpus_size <- cqi_attribute_size(paste(parent.cqp_corpus.name, "word", sep="."));
	
	max_id <- corpus_size - 1;
	right.boundary <- pmin(dump[,2] + right.context, max_id);
	dim(right.boundary) <- c(nrow(dump), 1);

	dump <- cbind(dump, left.boundary, right.boundary);
	colnames(dump) <- c("match", "matchend", "target", "keyword", "left", "right");
	return(dump);
}

