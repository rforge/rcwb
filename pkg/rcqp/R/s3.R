# ===========================================================================
# File: "main.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================



## 
 # ------------------------------------------------------------------------
 # 
 # "corpus(corpus, query)" --
 #
 # Create an S3 object holding a corpus
 # 
 # Example:
 #              corpus("DICKENS")
 # 
 # ------------------------------------------------------------------------
 ##
corpus <- function(corpus) {
   class(x) <- "cqp_corpus";
   attr(x, "cqp.name") <- corpus;
}



## 
 # ------------------------------------------------------------------------
 # 
 # "subcorpus(corpus, query)" --
 #
 # Create an S3 object holding a subcorpus
 # 
 # Example:
 #              subcorpus("DICKENS", "\"interesting\"")
 # 
 # ------------------------------------------------------------------------
 ##
subcorpus <- function(corpus, query) {
   cqp.name <- .get.subcorpus.cqp.name(corpus);
   x <- cqi_query(corpus, cqp.name, query);
   class(x) <- "cqp_subcorpus";

   attr(x, "cqp.name") <- cqp.name;
   attr(x, "parent.corpus.name") <- corpus;
   attr(x, "query") <- query;
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(subcorpus)" --
 #
 # Applying generic method "summary" to subcorpus object: print basic information.
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", "\"interesting\"")
 #              summary(sc)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_subcorpus <- function(x) {
  parent.corpus <- att(x, parent.corpus.name);
  subcorpus <- att(x, cqp.name);
  query <- att(x, query);
  size <- cqi_subcorpus_size(paste(parent.corpus, sep=":"));

  cat(paste("Parent corpus name:", parent.corpus, "\n"));
  cat(paste("Query:", query, "\n"));
  cat(paste("Size:", size, "\n"));
}



## 
 # ------------------------------------------------------------------------
 # 
 # "summary(corpus)" --
 #
 # Applying generic method "summary" to corpus object: print basic information.
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              summary(c)
 #
 # ------------------------------------------------------------------------
 ##
summary.cqp_corpus <- function(x) {

  # TODO : list p and a attribute, size, etc.
  stop("Not implemented yet."):

}



## 
 # ------------------------------------------------------------------------
 # 
 # "print(corpus)" --
 #
 # Applying generic method "print" to corpus object: print first lines of the corpus
 # 
 # Example:
 #              c <- corpus("DICKENS")
 #              print(c)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_corpus <- function(x) {

  # TODO : print the first lines
  stop("Not implemented yet."):

}



## 
 # ------------------------------------------------------------------------
 # 
 # "print(subcorpus)" --
 #
 # Applying generic method "print" to corpus object: print first kwic lines of the corpus
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", "\"interesting\"")
 #              print(sc)
 #
 # ------------------------------------------------------------------------
 ##
print.cqp_subcorpus <- function(x) {

  # TODO : print the first kwic lines
  stop("Not implemented yet."):

}



## 
 # ------------------------------------------------------------------------
 # 
 # "flist(subcorpus)" --
 #
 # Computing frequency list for a given corpus
 # 
 # Example:
 #              sc <- subcorpus("DICKENS", "\"interesting\"")
 #              flist(sc)
 #
 # ------------------------------------------------------------------------
 ##
flist.cqp_corpus <- function(x, attribute) {

  # TODO
  stop("Not implemented yet."):

}



## 
 # ------------------------------------------------------------------------
 # 
 # "flsit(corpus)" --
 #
 # Computing frequency list for a given subcorpus
 # 
 # Example:
 #              sc <- corpus("DICKENS")
 #              flist(sc)
 #
 # ------------------------------------------------------------------------
 ##
flist.cqp_subcorpus <- function (x, attribute) {

  # TODO : use fdist1 OR match..matchend
  stop("Not implemented yet."):

}



.get.subcorpus.cqp.name <- function(corpus) {
  subcorpora.name <- cqi_list_subcorpora(corpus)
  name <- .generate.name()
  while (name %in% subcorpora) {
    name <- .generate.name();
  }
  return(name);
}



.generate.name <- function() {
  initial <- LETTERS[sample(1:26, 1)];
  other <- c(LETTERS, letters)[sample(1:(26*2), 9)];
  other <- paste(other, collapse="");
  return(paste(initial, other, sep=""));
}

