# Functions calling the "utilities" program provided by cwb (./utils) for
# encoding, indexing and compressing corpora.

## 
 # ------------------------------------------------------------------------
 # 
 # Create a corpus with a data.frame
 # 
 # ------------------------------------------------------------------------
 ##
# import.data.frame <- function(corpus,
#                               structural.attribute.index,
#                               corpus.name=deparse(quote(corpus)),
#                               corpus.dir,
#                               registry.dir) {
# 
#     if (!is.data.frame(corpus)) stop("argument 'corpus' must be data.frame");
# 
#     if (!is.numeric(structural.attribute.index)) stop("argument 'structural.attribute.index' must be numeric vector");
#     if (length(structural.attribute.index) >= ncol(corpus) ||
#         any(structural.attribute.index > ncol(corpus)) ||
#         any(duplicated(structural.attribute.index))) stop("argument 'structural.attribute.index' must point to some index of 'corpus' column, without duplicate");
# 
#     col.modes <- sapply(corpus, class);
#     if (any(col.modes != "factor")) stop("column of 'corpus' must be of mode 'factor'");
# 
# }

.build.cwb_encode.cmd <- function(
    corpus.dir,
    registry.file,
    input.files,
    p.attributes,
    s.attributes,
    encoding="utf8",
    input.dirs=NULL) {
    args <- "cwb-encode"
    args <- append(args, c("-c", encoding));
    args <- append(args, c("-d", corpus.dir));
    args <- append(args, c("-R", registry.file));

# -f and -F options: files or directories to read
    for (f in input.files) {
        args <- append(args, "-f");
        args <- append(args, f);
    }
    if (! is.null(input.dirs)) {
        for (d in input.dirs) {
            args <- append(args, "-F");
            args <- append(args, d);
        }
    }

# All options (-d, -f, -R, etc.) must precede the attribute declarations (-P,
# -S, etc.) on the command line.
    for (p.attr in p.attributes) {
        args <- append(args, "-P");
        args <- append(args, p.attr);
    }
    for (s.attr in names(s.attributes)) {
        args <- append(args, "-S");
        if (!is.null(s.attributes[[s.attr]])) {
	  attr_text <- paste(c("0", s.attributes[[s.attr]]), collapse="+");
          attr_text <- paste(c(s.attr, attr_text), collapse=":");
	  args <- append(args, attr_text);
	} else {
	  args <- append(args, s.attr);
	}
    }
    return(args);
}

## 
 # ------------------------------------------------------------------------
 # 
 # Create a corpus
 # 
 # ------------------------------------------------------------------------
 ##
create.corpus <- function(
corpus.name,
corpus.dir,
registry.dir,
input.files,
p.attributes,
s.attributes,
encoding="utf8",
input.dirs=NULL,
compression.huffcode=FALSE,
compression.rdx=FALSE
) {

## corpus.name

    if (length(corpus.name) != 1) {
        stop("corpus.name must be of length 1");
    }
    if (length(grep("^[A-Z][A-Z0-9_-]+$", corpus.name)) == 0) {
        stop("The corpus name may contain only alphabetic characters, digits, '-' and '_' and must begin with a letter");
    }

## registry dir

    if (length(registry.dir) != 1) {
        stop("corpus.name must be of length 1");
    }
    if (!file.exists(registry.dir)) {
        stop(paste("Registry directory '", registry.dir, "' does not exists", sep=""));
    }
    registry.dir <- normalizePath(registry.dir);

    registry.file <- paste(registry.dir, tolower(corpus.name), sep="/")

## corpus dir

    if (length(corpus.dir) != 1) {
        stop("corpus.name must be of length 1");
    }
    if (!file.exists(corpus.dir)) {
        stop(paste("Corpus directory '", registry.dir, "' does not exists", sep=""));
    }
    corpus.dir <- normalizePath(corpus.dir);

## P attribute

    for (a in p.attributes) {
        if (length(grep("^[a-zA-Z0-9-]+$", a)) == 0) {
            stop(paste("Positional attribute name\"", a, "\" do not contains only letters, digit and '-'.", sep=""));
	}
    }

## S attribute

    if (!is.list(s.attributes)) {
        stop("s.attributes must be a list");
    }
    for (a in names(s.attributes)) {
        if (length(grep("^[a-zA-Z0-9-]+$", a)) == 0) {
            stop(paste("Structural attribute name\"", a, "\" do not contains only letters, digit and '-'.", sep=""));
	}
    }

##
##
## Encoding
##
##

    args <- .build.cwb_encode.cmd(corpus.dir, registry.file, input.files, p.attributes, s.attributes, encoding="utf8", input.dirs);
    ans <- .Call("rcqpCreate_cwb_encode", args, PACKAGE="rcqp");
    if (ans != 0) {
      stop("Error during cwb-encode")
    }

##
##
## Creating index files
##
##

# The -V switch enables additional validation passes when an index is created
# and when data files are compressed. It should be omitted when encoding very
# large corpora (above 50 million tokens), in order to speed up processing. In
# this case, it is also advisable to limit memory usage with the -M option. The
# amount specified should be somewhat less than the amount of physical RAM
# available (depending on the number of users etc.; too little is better than
# too much). For instance, on a Linux machine with 128 MB of RAM, -M 64 is a
# safe choice. Note that the cwb-make utility applies a default limit of -M 75
# if not explicit -M option is given.

    ans <- .Call("rcqpCreate_cwb_makeall", c(corpus.name, registry.dir), PACKAGE="rcqp" );

##
##
## Compression huffcode
##
##

# The token stream can be compressed with the cwb-huffcode tool. Use the -P
# option to process a single attribute, or compress all p-attributes with -A.

    if(compression.huffcode) {
      ans <- .Call("rcqpCreate_cwb_huffcode", c(corpus.name, registry.dir), PACKAGE="rcqp");
    }

##
##
## Compression rdx
##
##

# Index files can be compressed with the cwb-compress-rdx tool, which accepts the same options.
#         $ cwb-compress-rdx -A EXAMPLE
    if(compression.rdx) {
      ans <- .Call("rcqpCreate_cwb_compress_rdx", c(corpus.name, registry.dir), PACKAGE="rcqp");
    }

    .Call("rcqpCreate_re_rcqpinitialize_cqp", PACKAGE="rcqp");


  invisible(0);
}
