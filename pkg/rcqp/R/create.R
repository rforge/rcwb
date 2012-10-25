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

## 
 # ------------------------------------------------------------------------
 # 
 # Create a corpus with files
 # 
 # ------------------------------------------------------------------------
 ##
create.corpus <- function(corpus.dir, registry.file, input.files, input.dirs=NULL, p.attributes, s.attributes, encoding="utf8") {

    
# TODO :
    #corpus.dir doit exister et être un chemin absolu

    corpus.name <- toupper(basename(registry.file));
    registry.dir <- dirname(registry.file);

    if (length(grep("^[A-Z][A-Z0-9_-]+$", corpus.name)) == 0) {
        stop("The corpus name (ie. the registry filename) may contain only alphabetic characters, digits, '-' and '_' and must begin with a letter");
    }
    print(paste("Corpus:", corpus.name));

    # registry dir doit exister et être un chemin absolu

    # By convention, all attribute names must be lowercase (more precisely,
    # they may only contain the characters a-z, 0-9, -, and , and may not start
    # with a digit).  Therefore, the names of XML elements to be included in
    # the CWB corpus must not contain any non-ASCII or uppercase letters.
    for (a in p.attributes) {
        if (length(grep("^[a-zA-Z0-9-]+$", a)) == 0) {
            stop(paste("Positional attribute name\"", a, "\" do not contains only letters, digit and '-'.", sep=""));
	}
    }
    for (a in s.attributes) {
        if (length(grep("^[a-zA-Z0-9-]+$", a)) == 0) {
            stop(paste("Structural attribute name\"", a, "\" do not contains only letters, digit and '-'.", sep=""));
	}
    }

    args <- "cwb-encode"
    args <- append(args, c("-c", "utf8"));
    args <- append(args, c("-d", corpus.dir));
    args <- append(args, c("-R", registry.file));
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

    for (p.attr in p.attributes) {
        args <- append(args, "-P");
        args <- append(args, p.attr);
    }
    for (s.attr in s.attributes) {
        args <- append(args, "-S");
        args <- append(args, s.attr);
    }


# All options (-d, -f, -R, etc.) must precede the attribute declarations (-P,
# -S, etc.) on the command line.

#cwb-encode
#-d /corpora/data/example
#-f example.vrt
#-R /usr/local/share/cwb/registry/example
#-P pos
#-P lemma
#-S s

print(paste("[cwb-encode]", paste(args, collapse=" ")));
ans <- .Call("rcqpCreate_cwb_encode", args, PACKAGE="rcqp");
if (ans != 0) {

}
#cqi_corpus_info <- function(corpus) {
#    return(invisible())
#}


##
##
## Creating index files
##
##

# cwb-makeall -V EXAMPLE

# The -V switch enables additional validation passes when an index is created
# and when data files are compressed. It should be omitted when encoding very
# large corpora (above 50 million tokens), in order to speed up processing. In
# this case, it is also advisable to limit memory usage with the -M option. The
# amount specified should be somewhat less than the amount of physical RAM
# available (depending on the number of users etc.; too little is better than
# too much). For instance, on a Linux machine with 128 MB of RAM, -M 64 is a
# safe choice. Note that the cwb-make utility applies a default limit of -M 75
# if not explicit -M option is given.

# + -r registry
#print(paste("[cwb-makeall]", paste(args, collapse=" ")));
    ans <- .Call("rcqpCreate_cwb_makeall", c(corpus.name, registry.dir), PACKAGE="rcqp" )

##
##
## Compression
##
##

# The token stream can be compressed with the cwb-huffcode tool. Use the -P
# option to process a single attribute, or compress all p-attributes with -A.

#    ans <- .Call("rcqpCreate_cwb_huffcode", c(corpus.name, registry.dir), PACKAGE="rcqp" )

# Index files can be compressed with the cwb-compress-rdx tool, which accepts the same options.
#         $ cwb-compress-rdx -A EXAMPLE
#    ans <- .Call("rcqpCreate_cwb_compress_rdx", c(corpus.name, registry.dir), PACKAGE="rcqp" )
}
