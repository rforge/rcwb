#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

setOldClass("connection");
setOldClass("sockconn");

setClass("AbstractCorpus",
    representation(
      id = "character",
      con = "sockconn",
      size = "numeric",
      "VIRTUAL"
      ),
    );

#############################################################
#
# Size (nbr of occurrences) of the AbstractCorpus

setGeneric("size", function(object) standardGeneric("size"))
setMethod("size", "AbstractCorpus", function(object) object@size);

#############################################################
#
# Id (qualified id for subcorpus) of the AbstractCorpus

setGeneric("id", function(object) standardGeneric("id"));
setMethod("id", "AbstractCorpus", function(object) object@id);

#############################################################
#
# cqi_query

setGeneric("subcorpus", function(corpus, query, subcorpus_name) standardGeneric("subcorpus"));
setMethod("subcorpus", c("AbstractCorpus", "character", "character"), function(corpus, query, subcorpus_name="") {
    sc <- new("Subcorpus", corpus, query, subcorpus_name);
    return(sc);
    });

setMethod("subcorpus", c("AbstractCorpus", "character", "missing"), function(corpus, query, subcorpus_name) {
    subcorpus_id <- paste(c(sample(LETTERS, 1), sample(letters, 30, replace=T)), collapse="");
    sc <- subcorpus(corpus, query, subcorpus_id);
    return(sc);
    });

