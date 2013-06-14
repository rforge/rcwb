setClass("cqp_ftable", representation());


## 
 # ------------------------------------------------------------------------
 #
 # Create an S3 object holding a frequency table according to various parameters
 # 
 # ------------------------------------------------------------------------
 ##
cqp_ftable.cqp_corpus <- function(x, attribute1, attribute2, 
	attribute1.use.id=FALSE, attribute2.use.id=FALSE,
	structural.attribute.unique.id=FALSE,
	...
) {
	cqp_corpus.name <- .cqp_name(x);
	qualified.attribute1 <- .cqp_name(x, attribute1);
	qualified.attribute2 <- .cqp_name(x, attribute2);
	
	corpus_size <- size(x);
	max_id <- corpus_size - 1;
	
	s_atts <- cqi_attributes(cqp_corpus.name, "s");
	p_atts <- cqi_attributes(cqp_corpus.name, "p");

	##
	## extract id.
	##
	
	att1 <- 0;
	if (attribute1 %in% s_atts) {
		att1 <- cqi_cpos2struc(qualified.attribute1, 0:max_id);
	} else if (attribute1 %in% p_atts) {
		# TODO array base
		att1 <- cqi_cpos2id(qualified.attribute1, 0:max_id);
	} else {
		stop(paste("Unknown attribute:", attribute1));
	}

	att2 <- 0;
	if (attribute2 %in% s_atts) {
		# TODO array base
		att2 <- cqi_cpos2struc(qualified.attribute2, 0:max_id);
	} else if (attribute2 %in% p_atts) {
		# TODO array base
		att2 <- cqi_cpos2id(qualified.attribute2, 0:max_id);
	} else {
		stop(paste("Unknown attribute:", attribute2));
	}

	##
	## Create the id matrix
	##

	ids <- matrix(c(att1, att2), ncol=2);

	if (structural.attribute.unique.id) {
		if (! (attribute1 %in% s_atts)
			||
			! (attribute2 %in% s_atts)
		) {
			stop("Both attribute must be structural attributes in order to reduce id");
		}
		ids <- unique(ids);
	}

	##
	## replace id with string if requested.
	##

	res <- data.frame(attribute1=ids[,1], attribute2=ids[,2]);
	if (attribute1 %in% s_atts) {
		if ((!attribute1.use.id) & cqi_structural_attribute_has_values(qualified.attribute1)) {
			res[,1] <- cqi_struc2str(qualified.attribute1, ids[,1]);
		}
	} else {
		if (!attribute1.use.id) {
#			res[,1] <- cqi_id2str(qualified.attribute1, ids[,1]);
            stop("attribute1.use.id is available only for structural attribute");
		}
	}

	if (attribute2 %in% s_atts) {
		if ((!attribute2.use.id) & cqi_structural_attribute_has_values(qualified.attribute2)) {
			res[,2] <- cqi_struc2str(qualified.attribute2, ids[,2]);
		}
	} else {
		if (!attribute2.use.id) {
#			res[,2] <- cqi_id2str(qualified.attribute2, ids[,2]);
            stop("attribute2.use.id is available only for structural attribute");
		}
	}
	
	##
	## Count unique combinaison.
	##
	t <- count(res);

	colnames(t) <- c(
		attribute1,
		attribute2,
		"freq"
	);

	return(t);
}

cqp_ftable.cqp_subcorpus <- function(x, anchor1, attribute1, anchor2, attribute2, cutoff=0, ...) {
	parent.corpus <- attr(x, "parent.cqp_corpus.name");
	qualified.sub_corpus.name <- .cqp_name(x);
	
	m <- cqi_fdist2(qualified.sub_corpus.name, anchor1, attribute1, anchor2, attribute2, cutoff=cutoff);

	attribute1.str <- cqi_id2str(paste(parent.corpus, attribute1, sep="."), m[,1]);
  	attribute2.str <- cqi_id2str(paste(parent.corpus, attribute2, sep="."), m[,2]);
 
	df <- data.frame(attribute1.str, attribute2.str, m[,3]);
	colnames(df) <- c(
		paste(anchor1, attribute1, sep="."),
		paste(anchor2, attribute2, sep="."),
		"freq");
	return(df);
}


