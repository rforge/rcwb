cqpshell <- function(server_extra_arg="") {

  print("------------------------------------------------------------");
  print("Launching server");
  print("------------------------------------------------------------");

  d <- cqi_server(userflags=server_extra_arg);
  print(d);
  Sys.sleep(1);

  print("------------------------------------------------------------");
  print("Connecting socket");
  print("------------------------------------------------------------");

  con <- cqi_connect(user=d["user"], passwd=d["passwd"], host=d["host"], port=d["port"]);

#  cqi_send_word($CWB::CQI::CORPUS_LIST_CORPORA);
#      cqi_flush();
#        return cqi_expect_string_list();



#print(class(con));

  corpora <- cqi_list_corpora(con)
    print("Availables corpora:");
  print(corpora);
#print(cqi_attributes(con, "DESCARTES", "s"));

  print("Connected to server --- enter query or corpus change command:");
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

      status <- cqi_query(corpus, subcorpusname, query, con);
      if (status != STATUS_OK) {
        print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
        next;
      }
      print("ok");

      size = cqi_subcorpus_size(paste(corpus, subcorpusname, sep=":"), con);
      print(paste("size of the subcorpus:", size));

      if (size > 0) {
        match <-
          cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'match', 0, size-1, con);
        matchend <-
          cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'matchend', 0, size-1, con);
        for (i in 1:length(match)) {
          print_kwic_line(match[i], matchend[i], corpus, con);
        }
      }
      cqi_drop_subcorpus(paste(corpus, subcorpusname, sep=":"), con);
    }
  }
  cqi_bye(con);
}

print_kwic_line <- function(match, matchend, corpus, con) {
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

  lc = range_to_string(corpus, lb:(match-1), con);
  mat = range_to_string(corpus, match:matchend, con);
  rc = range_to_string(corpus, (matchend+1):rb, con);

  cat(sprintf("%10d ", match));
  cat(sprintf("%40s", paste(lc, collapse=" ")));
  cat(sprintf("%10s", paste(" << ",paste(mat, collapse=" ")," >> ", sep="")));
  cat(sprintf("%-30s", paste(rc, collapse=" ")));
  cat("\n");
}

# convert range of IDs to list of tokens with <nc>..</nc> tags inserted if available
# @tokens = range_to_string($corpus, @cpos);
range_to_string <- function(corpus, cpos, con) {
  word = cqi_cpos2str(paste(corpus, "word", sep="."), cpos, con);
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
