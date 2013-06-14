setClass("cqp_object", representation(cqp_name="character"));

## 
 # ------------------------------------------------------------------------
 #
 # Get the cqp_name
 #
 # ------------------------------------------------------------------------
 ##
setGeneric(".cqp_name", function(x, qualified) standardGeneric(".cqp_name"));

setMethod(".cqp_name", "cqp_corpus", function(x, qualified=TRUE) {
  cqp_name <- x@cqp_name;
  return(cqp_name);
});

setMethod(".cqp_name", "cqp_subcorpus", function(x, qualified=TRUE) {
  cqp_name <- x@cqp_name;
  if (qualified) {
    parent <- x@parent@cqp_name
    cqp_name <- paste(parent, cqp_name, sep=":");
  }
  return(cqp_name);
});

