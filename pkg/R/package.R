.First.lib <- function() {
 library.dynam("Rcwb", "Rcwb", "src");
}

.Last.lib <- function(libpath) {
#  igraph::.onUnload(libpath)
}
