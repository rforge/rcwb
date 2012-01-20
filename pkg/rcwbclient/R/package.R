#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

.First.lib <- function() {
 library.dynam("Rcwb", "Rcwb", "src");
}

.Last.lib <- function(libpath) {
#  igraph::.onUnload(libpath)
}
