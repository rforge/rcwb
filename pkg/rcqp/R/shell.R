# ===========================================================================
# File: "s3.R"
#                        Created: 2012-01-23 07:50:09
#              Last modification: 2012-01-23 07:50:09
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

#
# Mimic the cqp program (interactive shell) from within R.
#
# See the implementation in Perl provided by CWB
#

cqpshell <- function() {

  corpora <- cqi_list_corpora();
  print("Availables corpora:");
  print(corpora);

  print("Enter query or corpus change command:");
  corpus <- "";
  query <- "";
  subcorpusname <- "Foo";

  while(TRUE) {
    if (corpus == "") {
      query <- readline(prompt="[no corpus]>");
    } else {
      query <- readline(prompt=paste("[", corpus, "]>", sep=""));
    }

	# quit command
    if (grepl("\\s*(exit|quit)\\s*;?\\s*", query)) {
      break;
    }
	# show available corpora
    else if (query == "show") {
      print("System Corpora:");
      print(corpora);
    }
	# corpus change command
    else if (
        grepl("^\\s*[A-Z_][A-Z0-9_-]*\\s*;?\\s*$", query) 
        ) {
      corpus <- query;
      print(paste("Changing corpus to ", corpus, "... ", sep=""));
      if (any(corpora %in% corpus)) {
        print("ok");
      }
      else {
        print(paste("NO SUCH CORPUS:", corpus, "!"));
        corpus = "";
      }
    }
	# CQP query
    else {
      if (corpus=="") {
        print("Please set corpus first!");
        next;
      }
      print( "Executing CQP query...");

      status <- cqi_query(corpus, subcorpusname, query);
#       if (status != STATUS_OK) {
#         print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
#         next;
#       }
      print("ok");

      size = cqi_subcorpus_size(paste(corpus, subcorpusname, sep=":"));
      print(paste("size of the subcorpus:", size));

      if (size > 0) {
		  dump <-  cqi_dump_subcorpus(paste(corpus, subcorpusname, 
				  sep=":"), 1, size-1);
		  match <- dump[,1];
		  matchend <- dump[,2];
		  for (i in 1:length(match)) {
			  print_kwic_line(match[i], matchend[i], corpus);
		  }
	  }
	  cqi_drop_subcorpus(paste(corpus, subcorpusname, sep=":"));
    }
  }
}


print_kwic_line <- function(match, matchend, corpus) {
  #my ($lb, $rb, $sentence, $dummy);
  #my ($a, @lc, @mat, @rc);

  #$a = "$corpus.s";
  #$sentence = cqi_cpos2struc($a, $match);
  #($lb, $dummy) = cqi_struc2cpos($a, $sentence);
  #$lb = $match if $sentence < 0; # no context if not in <s> region
  #  $sentence = cqi_cpos2struc($a, $matchend);
  #($dummy, $rb) = cqi_struc2cpos($a, $sentence);
  #$rb = $matchend if $sentence < 0; # no context if not in <s> region
  lb <- match - 5;
  rb <- matchend + 5;

  lc = range_to_string(corpus, lb:(match-1));
  mat = range_to_string(corpus, match:matchend);
  rc = range_to_string(corpus, (matchend+1):rb);

  cat(sprintf("%10d ", match));
  cat(sprintf("%40s", paste(lc, collapse=" ")));
  cat(sprintf("%10s", paste(" << ",paste(mat, collapse=" ")," >> ", sep="")));
  cat(sprintf("%-30s", paste(rc, collapse=" ")));
  cat("\n");
}



# convert range of IDs to list of tokens with <nc>..</nc> tags inserted if available
# @tokens = range_to_string($corpus, @cpos);
range_to_string <- function(corpus, cpos) {
  word = cqi_cpos2str(paste(corpus, "word", sep="."), cpos);
  #if ($HaveNCAtt) {
  #  my $nc = "$corpus.np";
  #  my @lb = cqi_cpos2lbound($nc, @_);
  #  for (my $i = 0; $i < @word; $i++) {
  #    $word[$i] = "[".$word[$i]
  #      if $_[$i] == $lb[$i];
  #  }
  #  my @rb = cqi_cpos2rbound($nc, @_);
  #  for (my $i = 0; $i < @word; $i++) {
  #    $word[$i] = $word[$i]."]"
  #      if $cpos[$i] == $rb[$i];
  #  }
  #}
  return(word);
}


