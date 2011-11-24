#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

check_StructuralAttr <- function(object) {
  errors <- character();
  structural_attributes <- cqi_attributes(object@corpus@id, "s", object@corpus@con);
  if (! object@attribute_id %in% structural_attributes) {
    msg <- paste("Structural attribute ", object@attribute_id, " not found in this corpus. Available structural attributes are: ", paste(structural_attributes, collapse=","), ".", sep = "");
    errors <- c(errors, msg);
  }
  if (length(errors) == 0) TRUE else errors;
}

setClass("StructuralAttr",
    representation(
      has_value = "logical",
      qualified_id = "character",
      nbr_region = "numeric"
      ),
    contains="AbstractAttribute",
    validity=check_StructuralAttr
    );

setMethod(
    f="initialize",
    signature="StructuralAttr",
    definition=function(.Object,corpus,attribute_id) {

    parent <- corpus; # TODO to be cleaned
    if (is(corpus, "Corpus")) {
      #parent <- NULL;
    } else {
      corpus <- corpus@corpus;
    }

    .Object@corpus <- corpus;
    .Object@parent <- parent;
    .Object@attribute_id <- attribute_id;
    validObject(.Object);
    qualified_id <- paste(corpus@id, attribute_id, sep=".");
    .Object@qualified_id <- qualified_id;
    .Object@nbr_region <- cqi_attribute_size(qualified_id, corpus@con);
    .Object@has_value <- as.logical(cqi_structural_attribute_has_values(qualified_id, corpus@con));
    return(.Object);
    });


setMethod(f="show",
    signature="StructuralAttr",
    definition=function(object) {

    cat(paste("Structural attribute:", object@attribute_id, "\n"));
    cat(paste("In corpus:", object@corpus@id, "\n"));
    cat(paste("Number of regions:", object@nbr_region, "\n"));

    });

#############################################################

setGeneric("nbr_region", function(object) standardGeneric("nbr_region"));
setMethod("nbr_region", "StructuralAttr", function(object) object@nbr_region);

#############################################################

setGeneric("has_value", function(object) standardGeneric("has_value"));
setMethod("has_value", "StructuralAttr", function(object) object@has_value);

#############################################################

setGeneric("region_id", function(object, ...) standardGeneric("region_id"));
setMethod("region_id", "StructuralAttr", function(object, token_positions) {
  if (missing(token_positions)) {
    stop("The token_positions argument is missing");
  }
  if (!is.numeric(token_positions)) {
    stop("token_positions must be numeric");
  }
  if (any(token_positions >= object@corpus@size)) {
    stop(paste("token_positions out of range (maximum token_positions: ", object@corpus@size - 1, ")", sep=""));
  }
  if (any(token_positions < 0)) {
    stop("token_positions cannot be negative");
  }
  cqi_cpos2struc(object@qualified_id, token_positions, object@corpus@con);
});

#############################################################

setGeneric("right_boundary_position", function (object, ...) standardGeneric("right_boundary_position"));
setMethod("right_boundary_position", "StructuralAttr", function(object, token_positions) {
  if (missing(token_positions)) {
    stop("The token_positions argument is missing");
  }
  if (!is.numeric(token_positions)) {
    stop("token_positions must be numeric");
  }
  if (any(token_positions >= object@corpus@size)) {
    stop(paste("token_positions out of range (maximum token_positions: ", object@corpus@size - 1, ")", sep=""));
  }
  if (any(token_positions < 0)) {
    stop("token_positions cannot be negative");
  }
  cqi_cpos2rbound(object@qualified_id, token_positions, object@corpus@con);
});

#############################################################

setGeneric("left_boundary_position", function(object, ...) standardGeneric("left_boundary_position"));
setMethod("left_boundary_position", "StructuralAttr", function(object, token_positions) {
  if (missing(token_positions)) {
    stop("The token_positions argument is missing");
  }
  if (!is.numeric(token_positions)) {
    stop("token_positions must be numeric");
  }
  if (any(token_positions >= object@corpus@size)) {
    stop(paste("token_positions out of range (maximum token_positions: ", object@corpus@size - 1, ")", sep=""));
  }
  if (any(token_positions < 0)) {
    stop("token_positions cannot be negative");
  }
  cqi_cpos2lbound(object@qualified_id, token_positions, object@corpus@con);
});

#############################################################

setGeneric("region_position", function(object, ...) standardGeneric("region_position"));
setMethod("region_position", "StructuralAttr", function(object, region_id) {
  if (missing(region_id)) {
    stop("The region id argument is missing");
  }
  if (!is.numeric(region_id)) {
    stop("region_id must be numeric");
  }
  if (length(region_id) > 1) {
    stop("Only one region ID at a time");
  }
  if (any(region_id >= object@nbr_region)) {
    stop(paste("region id out of region (maximum region id: ", object@nbr_region - 1, ")", sep=""));
  }
  if (any(region_id < 0)) {
    stop("region_id cannot be negative");
  }
  cqi_struc2cpos(object@qualified_id, region_id, object@corpus@con);
});

#############################################################

setGeneric("region_value", function(object, ...) standardGeneric("region_value"));
setMethod("region_value", "StructuralAttr", function(object, region_ids=NULL) {
  if (!has_value(object)) {
    stop("This structural attribute does not have value");
  }
  if (is.null(region_ids)) {
    region_ids <- 0:(nbr_region(object)-1);
  }
  if (!is.numeric(region_ids)) {
    stop("id must be numeric");
  }
  if (any(region_ids >= object@nbr_region)) {
    stop(paste("region id out of range (maximum id: ", object@nbr_region - 1, ")", sep=""));
  }
  if (any(region_ids < 0)) {
    stop("region_ids cannot be negative");
  }
  cqi_struc2str(object@qualified_id, region_ids, object@corpus@con);
});

#############################################################

setGeneric("values_flist", function(object) standardGeneric("values_flist"));
setMethod("values_flist", "StructuralAttr", function(object) {
  values <- region_value(object);
  return(table(values));
});

#############################################################
