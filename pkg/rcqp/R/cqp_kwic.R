
###########################################################################
# S3 Object cqp_kwic
###########################################################################


cqp_kwic.cqp_subcorpus <- function(x,
	right.context=20,
	left.context=20,
	...
) {
	size <- size(x);
	if (size == 0) {
		stop("empty subcorpus");
	}
	
	s <- .get.kwic.matrix(x, right.context, left.context);	

	attr(s, "parent.cqp_corpus.name") <- attr(x, "parent.cqp_corpus.name");
	attr(s, "cqp_subcorpus.name") <- attr(x, "cqp_subcorpus.name");
	attr(s, "right.context") <- right.context;
	attr(s, "left.context") <- left.context;
	class(s) <- "cqp_kwic";
	return(s);
}



sort.cqp_kwic <- function(x, decreasing=FALSE, sort.anchor="match", sort.attribute="word", sort.offset=0, ...) {
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
}

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

print.cqp_kwic <- function(x,
	from=0,
	to=min(20, nrow(x)-1),
	print_tokens=function(x, cpos) cqi_cpos2str(paste(attr(x, "parent.cqp_corpus.name"), "word", sep="."), cpos),
	left.separator=" <<",
	right.separator=">> ",
	...
)
{
	if (from < 0) {
		stop("'from' must be greater than 0");
	}
	if (from > to) {
		stop("'to' must be greater than from");
	}
	if (to >= nrow(x)) {
		stop("'to' must be lesser than the size of the subcorpus");
	}

	requested.left.char= attr(x, "left.context")
	requested.right.char= attr(x, "right.context")

	nbr.lines <- to-from+1;
	matrix.lines <- matrix("", nrow=nbr.lines, ncol=3);
	for (i in 1:nbr.lines) {
		l <- x[from + i,];
		matrix.lines[i, 1] <- paste(
			print_tokens(x, l["left"]:(l["match"]-1)),
			collapse=" "
		);

		c1 <- print_tokens(x, l["match"]:l["matchend"]);
		c2 <- c(left.separator, c1, right.separator);
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

	format <- paste("%10d %", requested.left.char, "s%", requested.center.char, "s%-", requested.right.char, "s", sep="");
	r_from_to <- (from:to) + 1;
	lines <- sprintf(format, x[r_from_to, "match"], left, matrix.lines[,2], right);
	
	for(i in lines) {
		cat(paste(i, "\n", sep=""));
	}
}
