#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

library(methods);
setOldClass("connection");
setOldClass("sockconn");

#############################################################

corpus <- function(corpus_id, cwb_connection) {
    if (missing(corpus_id)) stop("Corpus ID cannot be ommited");
    if (missing(cwb_connection)) stop("Connection to CWB server cannot be ommited");
    if (! "connection" %in% class(cwb_connection)) {
      stop("cwb_connection must be a \"connection\" object");
    }
    new("Corpus", corpus_id, cwb_connection);
}

#############################################################

check_corpus <- function(object) {
  errors <- character();
  corpora <- cqi_list_corpora(object@con);
  if (! object@id %in% corpora) {
    msg <- paste("Corpus ", object@id, " not found on this CWB server. Available corpora include: ", paste(corpora, collapse=","), ".", sep = "");
    errors <- c(errors, msg);
  }
  if (length(errors) == 0) TRUE else errors;
}

#############################################################

setMethod(
    f="initialize",
    signature="Corpus",
    definition=function(.Object,id,con) {
    .Object@id <- id;
    .Object@con <- con;
    validObject(.Object);
    .Object@size <- cqi_attribute_size(paste(id, "word", sep="."), con);
    return(.Object);
    });

#############################################################

setClass("Corpus",
    representation(
    ),
#    prototype,
    contains="AbstractCorpus",
    validity=check_corpus,
#    access,
#    where=1,
#    version=FALSE
    );

#############################################################

setMethod(f="show",
    signature="Corpus",
    definition=function(object) {

    cat(paste("Corpus ID:", object@id, "\n"));

    full_name <- cqi_full_name(object@id, object@con);
    cat(paste("Corpus full name:", full_name, "\n"));

    cat(paste("Number of tokens:", object@size, "\n"));

    struct_a <- cqi_attributes(object@id, "s", object@con);
    cat(paste("Structural attributes (", length(struct_a), "): " , paste(struct_a, collapse=", "), "\n", sep=""));

    positional_a <- cqi_attributes(object@id, "p", object@con);
    cat(paste("Positional attributes (", length(positional_a), "): ", paste(positional_a, collapse=", "), "\n", sep=""));

#    aligned <- cqi_attributes(object@id, "a", object@con);
#    cat(paste("Alignment attributes (", length(aligned), "): ", paste(aligned, collapse=","), "\n", sep=""));

    charset <- cqi_charset(object@id, object@con);
    cat(paste("Charset:", charset, "\n"));

    });

# #############################################################
# 
# # get an existing subcorpus
# 
# setGeneric("subcorpus", function(object, ...) standardGeneric("subcorpus"));
# setMethod("subcorpus", "Corpus", function(object, subcorpus_id) {
#     if (!is.character(subcorpus_id)) {
#       stop("subcorpora_id must be of class character");
#     }
#     if (length(subcorpus_id) != 1) {
#       stop("subcorpora_id must be of length 1");
#     }
#     subcorpora_ids <- subcorpora(object);
#     if (! subcorpus_id %in% subcorpora_ids) {
#       stop(paste("The subcorpus id", subcorpus_id, "is not a known subcorpora of corpus", object@id));
#     }
#     subCorpus(subcorpus_id, object);
# });
# 
# setGeneric("subcorpus", function(corpus, subcorpus_id) standardGeneric("subcorpus"));
# setMethod("subcorpus", c(corpus="Corpus", subcorpus_id="character"), function(corpus, subcorpus_id) {
#     if (missing(subcorpus_id)) stop("Sub corpus ID cannot be ommited");
#     if (missing(corpus)) stop("Corpus object cannot be ommited");
#     if (!class(corpus) == "Corpus") {
#       stop("corpus must be of class Corpus");
#     }
#     new("Subcorpus", subcorpus_id, corpus);
#     });
# 
# #############################################################

setGeneric("subcorpora", function(object) standardGeneric("subcorpora"));
setMethod("subcorpora", "Corpus", function(object) {
   subc <- cqi_list_subcorpora(object@id, object@con);
   return(subc);
});

#############################################################
#
# Get a positional attribute

setGeneric("positionalAttr", function(corpus, attribute_id) standardGeneric("positionalAttr"));
setMethod("positionalAttr", c("AbstractCorpus", "character"), function(corpus, attribute_id) {
    if (missing(corpus)) stop("Connection to CWB server cannot be ommited");
    #if (!is(corpus, "AbstractCorpus")) stop("Argument corpus must be a Corpus object");

    if (missing(attribute_id)) stop("Corpus name cannot be ommited");
    if (!is.character(attribute_id)) stop("Argument attribute_id must be of class Corpus");
    new("PositionalAttr", corpus, attribute_id);
});

#############################################################
#
# Get a structural attribute

setGeneric("structuralAttr", function(corpus, attribute_id) standardGeneric("structuralAttr"));
setMethod("structuralAttr", c("Corpus", "character"), function(corpus, attribute_id) {
    if (missing(corpus)) stop("Connection to CWB server cannot be ommited");
    if (!is(corpus, "Corpus")) stop("Argument corpus must be a Corpus object");

    if (missing(attribute_id)) stop("Corpus name cannot be ommited");
    if (!is.character(attribute_id)) stop("Argument attribute_id must be of class Corpus");
    new("StructuralAttr", corpus, attribute_id);
});

#############################################################
#
# List the positional attributes

setGeneric("positionalAttr_ids", function(corpus) standardGeneric("positionalAttr_ids"));
setMethod("positionalAttr_ids", c(corpus="AbstractCorpus"), function(corpus) {
    return(cqi_attributes(corpus@id, type="p", corpus@con));
    });

#############################################################
#
# List the structural attributes

setGeneric("structuralAttr_ids", function(corpus) standardGeneric("structuralAttr_ids"));
setMethod("structuralAttr_ids", c(corpus="AbstractCorpus"), function(corpus) {
    return(cqi_attributes(corpus@id, type="s", corpus@con));
    });

#############################################################

setGeneric("alignmentAttr_ids", function(corpus, attribute_id) standardGeneric("alignmentAttr_ids"));
setMethod("alignmentAttr_ids", c(corpus="AbstractCorpus", attribute_id="character"), function(corpus, attribute_id) {
    alignmentAttr(corpus=corpus, attribute_id=attribute_id);
    });

#############################################################

