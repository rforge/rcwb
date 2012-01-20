#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

setOldClass("Corpus");
setClass("AbstractAttribute",
    representation(
      corpus = "Corpus", # the root corpus
      parent = "AbstractCorpus", # the direct parent ; may be the same as corpus
      attribute_id = "character",
      "VIRTUAL"
      ),
    );

# todo : initialize size slot here
#setGeneric("size", function(object) standardGeneric("size"));
#setMethod("size", "AbstractAttribute", function(object) object@size);

setGeneric("get_id", function(object) standardGeneric("get_id"));
setMethod("get_id", "AbstractAttribute", function(object) object@attribute_id);

#############################################################

