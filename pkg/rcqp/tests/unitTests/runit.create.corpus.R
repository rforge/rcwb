### --- Test setup ---
 
if(FALSE) {
  ## Not really needed, but can be handy when writing tests
  library("RUnit")
  library("rcqp")
}
 
 
test.create <- function()
{
  root <- file.expand(file.path(getwd(), "..", "tests", "unitTests"));

  registry.dir <- paste(root, "registry", sep="/");
  data.dir <- paste(root, "data", sep="/");

  print(registry.dir);
  print(data.dir);

  dir.create(registry.dir);
  dir.create(data.dir);


library(rcqp)
create.corpus("/Users/sylvainloiseau/TXM/corpora/test1/", "/Users/sylvainloiseau/TXM/registry/test1", "/Users/sylvainloiseau/TXM/corpora/montesquieu/wtc/montesquieu.wtc", NULL, c("pos", "lemma"), c("txmcorpus", "teiCorpus", "TEI", "text", "div1"))

corpus.name = "TEST";
registry.dir <- "/Users/sylvainloiseau/TXM/registry/"
.Call("rcqpCreate_cwb_huffcode", c(corpus.name, registry.dir), PACKAGE="rcqp" )

  checkTrue(a < b)
}
