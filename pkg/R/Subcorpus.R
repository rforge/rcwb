#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

library(methods);
setOldClass("connection");
setOldClass("sockconn");

#############################################################

check_subcorpus <- function(object) {
  errors <- character();
  parent <- object@corpus;
  subcorpora <- cqi_list_subcorpora(object@corpus@id, object@corpus@con);
  if (! object@subcorpus_id %in% subcorpora) {
    msg <- paste("Sub corpus ", object@subcorpus_id, " not found on this CWB server for corpus", object@corpus@id, ". Available subcorpora for this corpus: ", paste(subcorpora, collapse=","), ".", sep = "");
    errors <- c(errors, msg);
  }
  if (length(errors) == 0) TRUE else errors;
}

#############################################################

# new does not call the validity function if called without argument !!!???
# cf. setValidity?

setClass("Subcorpus",
    representation(
      subcorpus_id = "character",
      corpus = "Corpus", ## the root Corpus object
      qualified_subcorpus_id = "character",
      nbr_match = "integer",
      query = "character",
      parent = "AbstractCorpus" ## the direct parent, may be a Subcorpus
    ),
#    prototype,
    contains="AbstractCorpus",
    validity=check_subcorpus,
#    access,
#    where=1,
#    version=FALSE
    );

#############################################################

setMethod(
    f="initialize",
    signature="Subcorpus",
    definition=function(.Object, corpus, query, subcorpus_id) {

    if (!is.character(subcorpus_id) || length(subcorpus_id) != 1) {
      stop("subcorpora_name must be a character vector of length 1");
    }
    if (!grepl("[A-Z][A-Za-z]+", subcorpus_id)) {
      stop("A valid subcorpus name must match the following regexp: \"[A-Z][A-Za-z]+\"");
    }
    if (!is.character(query) || length(query) != 1) {
      stop("query must be a character vector of length 1");
    }

    resp <- cqi_query(corpus@id, subcorpus_id, query, corpus@con);

    resp_group <- bitShiftR(resp, 8);
    if (resp == STATUS_OK) {
    } else {
      msg <- last_error_msg(corpus@con);
      print(msg);
      #CqiCheckResponse(resp, STATUS_OK)
      if (resp_group == ERROR) {
        stop(paste("Unable to create the subcorpus: error", resp));
      } else if (resp_group == CQP_ERROR) {
        stop(paste("Unable to create the subcorpus: cqp error", resp));
      } else {
        stop(paste("Unable to create the subcorpus: unknown error", resp));
      }
    }
    
    .Object@subcorpus_id <- subcorpus_id;
    .Object@parent <- corpus;
    root <- corpus
    while(!is(root, "Corpus")) {
      root <- root@corpus;
    }
    .Object@corpus <- root;
    .Object@qualified_subcorpus_id <- paste(root@id, subcorpus_id, sep=":");
    .Object@id <- .Object@qualified_subcorpus_id;
    .Object@con <- root@con;
    .Object@nbr_match <- cqi_subcorpus_size(.Object@qualified_subcorpus_id, root@con);
    .Object@size <- -1; #.get_size(.Object@qualified_subcorpus_id, .Object@nbr_match, root@con);
    .Object@query <- query;

    validObject(.Object);
    return(.Object);
    });

#############################################################

setMethod(f="show",
    signature="Subcorpus",
    definition=function(object) {

    cat(paste("Sub corpus ID:", object@subcorpus_id, "\n"));
    cat(paste("\tCorpus ID:", object@corpus@id, "\n"));
    cat(paste("\tCorpus query:", object@query, "\n"));
    if (is(object@parent, "Subcorpus")) {
      cat(paste("\tInside subcorpus:", chain(object), "\n"));
    }

    cat(paste("\tNumber of tokens:", object@size, "\n"));

#    struct_a <- cqi_attributes(object@qualified_subcorpus_id, "s", object@corpus@con);
#    cat(paste("Structural attributes (", length(struct_a), "): " , paste(struct_a, collapse=", "), "\n", sep=""));
#
#    positional_a <- cqi_attributes(object@qualified_subcorpus_id, "p", object@corpus@con);
#    cat(paste("Positional attributes (", length(positional_a), "): ", paste(positional_a, collapse=", "), "\n", sep=""));

#    aligned <- cqi_attributes(object@qualified_subcorpus_id, "a", object@corpus@con);
#    cat(paste("Alignment attributes (", length(aligned), "): ", paste(aligned, collapse=","), "\n", sep=""));

#    charset <- cqi_charset(object@id, object@corpus@con);
#    cat(paste("Charset:", charset, "\n"));

    });


#############################################################
#
# A string representing a hierarchy between of subcorpus
setGeneric("chain", function(object) standardGeneric("chain"))
setMethod("chain", "Subcorpus", function(object) {
  str <- paste(" >> ", object@query, sep="");
  if (is(object@parent, "Subcorpus")) {
    str <- append(chain(object@parent), str);
  }
  return(paste(str, collapse=""));
});

#############################################################
#
# nbr_match

setGeneric("nbr_match", function(object) standardGeneric("nbr_match"))
setMethod("nbr_match", "Subcorpus", function(object) object@nbr_match);

#############################################################
#
# Size, ie nbr of occurrences, of the subcorpus ; different from nbr_match!

.get_size <- function(qualified_subcorpus_id, nbr_match, con) {
  if (nbr_match == 0) {
    return(0);
  } else {
    match <- cqi_dump_subcorpus(qualified_subcorpus_id, "MATCH", 0, nbr_match-1, con);
    matchend <- cqi_dump_subcorpus(qualified_subcorpus_id, "MATCHEND", 0, nbr_match-1, con);
    if (length(match) != length(matchend)) stop("Unable to compute subcorpus size");
    # + 1, since for a single word query, match=matchend
    size <- sum((matchend+1) - match);
    return(size);
  }
}

#############################################################
#
# drop

setGeneric("drop", function(object) standardGeneric("drop"))
setMethod("drop", "Subcorpus", function(object) {
    cqi_drop_subcorpus(object@qualified_subcorpus_id, object@corpus@con);
});

