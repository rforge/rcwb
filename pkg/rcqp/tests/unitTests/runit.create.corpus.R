### --- Test setup ---
 
if(FALSE) {
  ## Not really needed, but can be handy when writing tests
  library("RUnit")
  library("rcqp")
}

test.create.corpus <- function() {
  print("everything ok");
}

#.setUp <- function() {
#}
#.tearDown <- function() {
#}
 
#test.create.corpus <- function() {
#  root <- path.expand(file.path(getwd(), "unitTests"));
#
#  registry.dir <- paste(root, "registry", sep="/");
#  data.dir <- paste(root, "data", sep="/");
#
#    if(file.exists(registry.dir)) {
#    stop("Registry directory already exists");
#  }
#  if(file.exists(data.dir)) {
#    stop("data directory already exists");
#  }
#  dir.create(registry.dir);
#  dir.create(data.dir);
#
#  create.corpus(
#      "TEST",
#      data.dir,
#      registry.dir,
#      paste("unitTests", "corpus.wtc", sep="/"),
#      c("pos", "lemma"),
#      list("txmcorpus"="lang", "teiCorpus"=NULL, "TEI"=NULL, "text"=c("base", "project", "id"), "div1"=NULL, "div2"=c("id", "n"), "p"="id")
#      );
#  #registry.dir <- "/Users/sylvainloiseau/TXM/registry/"
#  #.Call("rcqpCreate_cwb_huffcode", c("CORPUS", registry.dir), PACKAGE="rcqp" )
#
#  #unlink(paste(path, "data", sep="/"), recursive=TRUE, force=TRUE)
#  #unlink(paste(path, "registry", sep="/"), recursive=TRUE, force=TRUE)
#}

#test.create.corpus.vector.too.long <- function() {
#  root <- path.expand(file.path(getwd(), "unitTests"));
#  registry.dir <- paste(root, "registry", sep="/");
#  data.dir <- paste(root, "data", sep="/");
#
#  if(file.exists(registry.dir)) {
#    stop("Registry directory already exists");
#  }
#  if(file.exists(data.dir)) {
#    stop("data directory already exists");
#  }
#  dir.create(registry.dir);
#  dir.create(data.dir);
#
#  checkException(
#  create.corpus(
#      c("TEST1", "TEST2"),
#      data.dir,
#      registry.dir,
#      paste("unitTests", "corpus.wtc", sep="/"),
#      c("pos", "lemma"),
#      list("txmcorpus"="lang", "teiCorpus"=NULL, "TEI"=NULL, "text"=c("base", "project", "id"), "div1"=NULL, "div2"=c("id", "n"), "p"="id")
#      )
#  );
#  #registry.dir <- "/Users/sylvainloiseau/TXM/registry/"
#  #.Call("rcqpCreate_cwb_huffcode", c("CORPUS", registry.dir), PACKAGE="rcqp" )
#}

