#subcorpus <- function(corpus, left_from, left_to, right_from, right_to, con) {
##  query <- paste("[]{", left_from, ",", left_to, "} ["));
##[]{6,6} [lemma="bois"] []{6,6};
#}

#subcorpus <- function(corpus, query, left=10, right=10) {
#  query <- paste("[]{", left, ",", left, "}", query, "[]{", right, ",", right, "};", sep=""));
#  sc <- subcorpus(corpus, "Temp", query);
#}
#
#subcorpus <- function(corpus, query, structural_attribute) {
#  query <- paste("<", structural_attribute, ">", query, "</", structural_attribute, ">;", sep=""));
#  sc <- subcorpus(corpus, "Temp", query);
#}

specificites <- function(corpus, subcorpus, positional_attribute) {
  sc_attr <- positionalAttr(subcorpus, positional_attribute);
  subcorpus.flist <- flist(sc_attr);

  pAttr <- positionalAttr(corpus, positional_attribute);
  form <- id2str(pAttr);
  freq <- flist(pAttr);
  names(freq) <- form;

  # TODO : flist doit retourner la même chose (un vecteur nommé), avec un corpus ou un souscorpus
  
  #specifictes.lexicon(
}

#specificites <- function(corpus, structural_attribute, positional_attribute, use_value=TRUE) {
#  m <- 0;
#  is (use_value) {
#    m <- region_value.ftable(corpus, structural_attribute, positional_attribute);
#  } else {
#    m <- region.ftable(corpus, structural_attribute, positional_attribute);
#  }
#  spe <- specificites(m);
#  return(spe);
#}

################ ################ ################ ################
#
# A frequency table whith as many subcorpus as regions of the the given
# structural attribute
#

region.ftable <- function(corpus, structural_attribute, positional_attribute) {
  if (!is(corpus, "AbstractCorpus")) {
    stop("corpus must be a Corpus or a Subcorpus");
  }
  if (!is(structural_attribute, "character") || length(structural_attribute) != 1) {
    stop("strutural_attribute must be a character vector of length 1");
  }
  if (!is(positional_attribute, "character") || length(positional_attribute) != 1) {
    stop("positional_attribute must be a character vector of length 1");
  }

  sAttr <- structuralAttr(corpus, structural_attribute);
  nrow <- nbr_region(sAttr);

  pAttr <- positionalAttr(corpus, positional_attribute);
  form <- id2str(pAttr);
  ncol <- length(form);

  m <- matrix(0, nrow=nrow, ncol=ncol, dimnames=list(1:nrow, forms));
  for (s in 1:nrow) {
    pos_boundary <- region_position(sAttr, s-1); # -1: zero-based
    position <- pos_boundary[1]:pos_boundary[2];
    id <- position2id(corpus, position);
    fl <- table(id);
    names <- id2str(corpus, as.numeric(names(fl)));
    names(fl) <- names;
    m[s, names(fl)] <- fl;
  }
  return(m);
}

################ ################ ################ ################
#
# A frequency table whith as many subcorpus as distinct values of the the given
# structural attribute
#

region_value.ftable <- function(corpus, structural_attribute, positional_attribute) {
  if (!is(corpus, "AbstractCorpus")) {
    stop("corpus must be a Corpus or a Subcorpus");
  }
  if (!is(structural_attribute, "character") || length(structural_attribute) != 1) {
    stop("strutural_attribute must be a character vector of length 1");
  }
  if (!is(positional_attribute, "character") || length(positional_attribute) != 1) {
    stop("positional_attribute must be a character vector of length 1");
  }

  sAttr <- structuralAttr(corpus, structural_attribute);
  if (!has_value(sAttr)) {
    stop("structural attribute has no value attached")
  }
  val <- region_value(sAttr);
  val <- unique(val);
  attr <- get_id(sAttr);
  nrow <- length(val);

  pAttr <- positionalAttr(corpus, positional_attribute);
  form <- id2str(pAttr);
  ncol <- length(form);

  m <- matrix(0, nrow=nrow, ncol=ncol, dimnames=list(val, form));

  for (s in 1:nrow) {
    query <- paste("[_.", attr, " = \"", val[s], "\"];", sep="");
    sc <- subcorpus(corpus, query);
    sc_attr <- positionalAttr(sc, positional_attribute);
    fl <- flist(sc_attr);
    drop(sc);

    m[ s, names(fl)+1 ] <- fl; # from 0-based to 1-based index
  }
  return(m);
}


