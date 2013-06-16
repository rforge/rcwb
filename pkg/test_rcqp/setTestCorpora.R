
create_test_corpus <- function() {
  root <- path.expand(file.path(getwd(), "unitTests"));
  registry.dir <- paste(root, "registry", sep="/");
  data.dir <- paste(root, "data", sep="/");

create.corpus(
      "TEST",
      data.dir,
      registry.dir,
      paste("unitTests", "corpus.wtc", sep="/"),
      c("pos", "lemma"),
      list("txmcorpus"="lang", "teiCorpus"=NULL, "TEI"=NULL, "text"=c("base", "project", "id"), "div1"=NULL, "div2"=
	c("id", "n"), "p"="id")
      );
}

delete_test_corpus <- function() {
  root <- path.expand(file.path(getwd(), "unitTests"));
  registry.dir <- paste(root, "registry", sep="/");
  data.dir <- paste(root, "data", sep="/");

  unlink(paste(registry.dir, "*", sep="/"), recursive=TRUE, force=TRUE)
    unlink(paste(data.dir, "*", sep="/"), recursive=TRUE, force=TRUE)
}



create_test_corpus();
