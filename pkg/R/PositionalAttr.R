
check_PositionalAttr <- function(object) {
  errors <- character();
  positional_attributes <- cqi_attributes(object@corpus@id, "p", object@corpus@con);
  if (! object@attribute_id %in% positional_attributes) {
    msg <- paste("Positional attribute ", object@attribute_id, " not found in this corpus. Available positional attributes are: ", paste(positional_attributes, collapse=","), ".", sep = "");
    errors <- c(errors, msg);
  }
  if (length(errors) == 0) TRUE else errors;
}

#############################################################

setClass("PositionalAttr",
    representation(
      qualified_id = "character",
      nbr_form = "numeric"
      ),
    contains="AbstractAttribute",
    validity=check_PositionalAttr
    );

#############################################################

setMethod(
    f="initialize",
    signature="PositionalAttr",
    definition=function(.Object, corpus, attribute_id) {

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
    .Object@qualified_id <- qualified_id
    .Object@nbr_form <- cqi_lexicon_size(qualified_id, corpus@con);
    return(.Object);
    });

#############################################################

setMethod(f="show",
    signature="PositionalAttr",
    definition=function(object) {

    cat(paste("Positional attribute:", object@attribute_id, "\n"));
    cat(paste("In corpus:", object@corpus@id, "\n"));
    cat(paste("Number of forms:", object@nbr_form, "\n"));         # TODO : it is the number of forms in the corpus, may not in the subcorpus

    });

#############################################################

setGeneric("nbr_form", function(object) standardGeneric("nbr_form"));
setMethod("nbr_form", "PositionalAttr", function(object) object@nbr_form);

#############################################################

##
## TODO : - 
# - penser à suprimer les sous corpus temporaires
# - implémenter les sélections d'une fréquence dans un sous corpus (regarder dans TXM ?)
# - argument porte mal son nom : ce ne sont pas des "form_ids" mais des "form" (soit id, soit caractere)
##

setGeneric("flist", function(object, ...) standardGeneric("flist"));
setMethod("flist", "PositionalAttr", function(object, form_ids=0:(nbr_form(object)-1)) {
  if (is.character(form_ids)) {
    check_form_strings(form_ids);
    name_ids <- cqi_str2id(object@qualified_id, form_ids, object@corpus@con);
    unknown <- which(name_ids == -1);
    if (length(unknown > 0)) {
      stop(
        paste(
          "Unknown form(s) (",
          length(unknown),
          " out of ",
          length(form_ids),
          ") in attribute ",
          object@attribute_id,
          ": ",
          paste(form_ids[unknown], collapse=", "),
          sep="")
      );
    }
    form_ids <- name_ids;
  } else if (is.numeric(form_ids)) {
    check_form_ids(object, form_ids);
  }
  freqs <- numeric(length(form_ids));
  if (is(object@parent, "Subcorpus")) {
    if (length(form_ids) == nbr_form(object)) {
      is_selection = FALSE;
    } else {
      is_selection = FALSE;
    }
    freqs <- .id2freq_in_subcorpus(object, form_ids, is_selection);
  } else {
    freqs <- cqi_id2freq(object@qualified_id, form_ids, object@corpus@con);
    names(freqs) <- form_ids;
  }
  return(freqs);
});

.id2freq_in_subcorpus <- function(object, form_ids, is_selection) {
  if (is_selection) {
    stop("Not implemented : freq of a given form in subcorpus");
  }  else {
    m <- cqi_fdist1(object@parent@qualified_subcorpus_id, 0, paste("MATCH", object@attribute_id, sep="."), object@corpus@con);
    freqs <- m[,2];
    names(freqs) <- m[,1];
  }
  return(freqs);

# subcorpus, 0, MATCH.lemma

# #    subcorpus <- object@parent;
# #    tmp <- subcorpus(subcorpus, "Temp", "\".*\"");
# #    #print(class(object@corpus@con));
# #    mat <- cqi_fdist1(tmp@id, 0, paste("MATCH", object@attribute_id, sep="."), object@corpus@con);
# #    return(mat);

# #	int lexiconSize = fdist.length;
# #
# #	int[] freqs = new int[lexiconSize];
# #	int[] ids = new int[lexiconSize];
# #	for (int i = 0; i < fdist.length; i++) {
# #		ids[i] = fdist[i][0];
# #		freqs[i] = fdist[i][1];
# #	}
# #
# #	Lexicon lexicon = new Lexicon(this, property, freqs, ids);
# #
# #	long end = System.currentTimeMillis();
# #	Log.txm.finest(Messages.END_SUBCORPUS_LEXICON + this.name
# #			+ Messages.Subcorpus_1 + (end - start));
# #
# #	// System.out.println("add in "+this.getCqpId()+"cache "+property.getName());
# #	lexicons.put(property.getName(), lexicon);
}

#############################################################
#############################################################
# 
# cqi_regex2id
# cqi_str2id

setGeneric("str2id", function(object, ...) standardGeneric("str2id"));
setMethod("str2id", "PositionalAttr", function(object, form_strings, isRegex=FALSE) {
  form_ids <- numeric(0);
  if (missing(form_strings)) {
    stop("argument form_strings missing");
  }
  check_form_strings(form_strings);
  if (isRegex) {
    if (length(form_strings) != 1) {
      stop("\"form_strings\" must be a vector of length 1 when used as a regex");
    }
    form_ids <- cqi_regex2id(object@qualified_id, form_strings, object@corpus@con);
  } else {
    form_ids <- cqi_str2id(object@qualified_id, form_strings, object@corpus@con);
  }
  return(form_ids);
});

#############################################################
#
# cqi_cpos2id

setGeneric("position2id", function(object, ...) standardGeneric("position2id"));
setMethod("position2id", "PositionalAttr", function(object, token_positions) {
  if (missing(token_positions)) {
    stop("argument token_positions missing");
  }
  check_token_positions(object@corpus, token_positions);
  form_ids <- cqi_cpos2id(object@qualified_id, token_positions, object@corpus@con);
  return(form_ids);
});

#############################################################
#
# cqi_id2str
#
# since the form_ids is not mandaroty, the function will best nammed "string"

setGeneric("id2str", function(object, ...) standardGeneric("id2str"));
setMethod("id2str", "PositionalAttr", function(object, form_ids=0:(nbr_form(object)-1)) {
    check_form_ids(object, form_ids);
    form_string <- character(0);
    form_string <- cqi_id2str(object@qualified_id, form_ids, object@corpus@con);
    return(form_string);
});

#############################################################
#
# cqi_cpos2str

setGeneric("position2str", function(object, ...) standardGeneric("position2str"));
setMethod("position2str", "PositionalAttr", function(object, token_positions) {
  form_string <- character(0);
    if (missing(token_positions)) {
      stop("argument token_positions missing");
    }
    check_token_positions(object@corpus, token_positions);
    form_string <- cqi_cpos2str(object@qualified_id, token_positions, object@corpus@con);
  return(form_string);
});

#############################################################
#############################################################

setGeneric("token_positions", function(object, ...) standardGeneric("token_positions"));
setMethod("token_positions", "PositionalAttr", function(object, form_ids) {

check_form_ids(object, form_ids);
if (length(form_ids) == 1) {
# cqi_id2cpos
cqi_id2cpos(object@qualified_id, form_ids, object@corpus@con);
} else {
# cqi_idlist2cpos
cqi_idlist2cpos(object@qualified_id, form_ids, object@corpus@con);
}
});

#############################################################
# Utilities for checking arguments
#############################################################

check_token_positions <- function(corpus, token_positions) {
  if (!is.numeric(token_positions)) {
    stop("\"token_positions\" must be a numeric vector");
  }
  if (length(token_positions) == 0) {
    stop("\"token_positions\" must be a vector of length > 0");
  }
  if (any(token_positions >= corpus@size) || any(token_positions < 0)) {
    stop("Token positions must be greater than -1 and less than corpus size");
  }
  #if (any(token_positions < 0)) {
  #  stop("A \"token_position\" cannot be < 0");
  #}
  #if (any(token_positions >= object@corpus@size)) {
  #  stop("A \"token_position\" cannot be < 0");
  #}
}

check_form_ids <- function(object, form_ids) {
  if (!is.numeric(form_ids)) {
    stop("\"form_ids\" must be a numeric vector");
  }
  if (length(form_ids) == 0) {
    stop("\"form_ids\" must be a vector of length > 0");
  }
  if (any(form_ids >= object@nbr_form) || any(form_ids < 0)) {
    stop("Positional attribute ids must be greater than -1 and less than (nbr_form - 1)");
  }
}

check_form_strings <- function(form_strings) {
  if (!is.character(form_strings)) {
    stop("\"form_strings\" must be a character vector");
  }
  if (length(form_strings) == 0) {
    stop("\"form_strings\" must be a vector of length > 0");
  }
}

# cqi_cpos2id
# cqi_cpos2str

# cqi_str2id
# cqi_id2str

# cqi_id2cpos
# cqi_idlist2cpos



