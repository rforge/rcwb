# ===========================================================================
# File: "main.R"
#                        Created: 2012-01-25 17:33:00
#              Last modification: 2012-01-25 17:33:00
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

## 
 # ------------------------------------------------------------------------
 # 
 # "install_cqp_testcorpus()" --
 # 
 # Set the correct location in the registry file of the toy-corpus located in
 # system.file("exampleData", package = "rcqp")
 #
 # Example:
 #		install_cqp_testcorpus()
 # 
 # ------------------------------------------------------------------------
 ##
install_cqp_testcorpus <- function() {

	path_registry <- system.file("extdata", "registry", package = "rcqp");
	path_data <- system.file("extdata", "data", package = "rcqp");

	for (lang in c("fr", "ru")) {
		f <- paste(path_registry, "/vie_", lang, sep="");
		lines <- readLines(f);
	
		path_data_corpus <- paste(path_data, "/vie_", lang, sep="");
		lines <- gsub(pattern="HOME", replacement=paste("HOME", path_data_corpus), x=lines);

		writeLines(text=lines, con=f)
		#print(lines);
	}

}