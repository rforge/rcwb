#library(Rcwb);
#source("client.R");
#source("constantes.R");
#source("server.R");

# ---------------------------------------------

sl <- "-r /Users/sylvainloiseau/TXM/registry"
extra <- sl;
debug <- T;
#registry <-  paste(system.file("example", package=packageDescription("Rcwb")$Package), "registry", sep="/");

# ---------------------------------------------

list_corpora <- c("TINY");

corpus <- "TINY";
s_attribute <- "s";
p_attribute <- c("word", "pos", "lemma");
a_attribute <- character();
charset <- "utf8";

# ---------------------------------------------

before <- function(server_extra_arg=extra) {
  d <- cqi_server(userflags=server_extra_arg);
  Sys.sleep(0.5);
  con <- cqi_connect(user=d["user"], passwd=d["passwd"], host=d["host"], port=d["port"]);
  return(con);
}

after <- function (con) {
  cqi_bye(con);
}

# ---------------------------------------------

compare <- function(found, expected) {
  if (!identical(found, expected)) {
    stop(paste("Expected:", expected, "/ Found:", found));
  }
}

# ---------------------------------------------



# ---------------------------------------------

#
#
#  Error Handling
#
#

test_CqiError <- function() {}
test_CqiErrorCode <- function() {}
test_CqiCheckResponse <- function() {}

#
#
#  Connect to CQi server / Disconnect
#
#

test_cqi_connect <- function() {}
test_cqi_bye <- function() {}
test_cqi_ping <- function() {}

# #
# #
# #  CQi Commands
# #
# #

test_cqi_ask_feature <- function() {}
test_cqi_list_corpora <- function() {}
test_cqi_charset <- function() {}
test_cqi_properties <- function() {
  #info <- cqi_properties(corpus, con);
  #print(info);
}
test_cqi_attributes <- function() {
  for (t in c("s", "p", "a")) {
    con <- before();
    a <- cqi_attributes(corpus, t, con);
    compare(a, get(paste(t, "attribute", sep="_")));
    after(con);
  }
}
test_cqi_structural_attribute_has_values <- function() {}
test_cqi_full_name <- function() {}
test_cqi_corpus_info <- function() {}
test_cqi_drop_corpus <- function() {}
test_cqi_attribute_size <- function() {}
test_cqi_lexicon_size <- function() {}
test_cqi_drop_attribute <- function() {}
test_cqi_str2id <- function() {}
test_cqi_id2str <- function() {}
test_cqi_id2freq <- function() {}
test_cqi_cpos2id <- function() {}
test_cqi_cpos2str <- function() {
  #foo <- cqi_cpos2str("DESCARTES.word", 0:10, con);
  #print(foo);
}
test_cqi_cpos2struc <- function() {}
test_cqi_cpos2lbound <- function() {}
test_cqi_cpos2rbound <- function() {}
test_cqi_cpos2alg <- function() {}
test_cqi_struc2str <- function() {}
test_cqi_id2cpos <- function() {}
test_cqi_idlist2cpos <- function() {}
test_cqi_regex2id <- function() {}
test_cqi_struc2cpos <- function() {}
test_cqi_alg2cpos <- function() {}
test_cqi_query <- function() {}
test_cqi_list_subcorpora <- function() {}
test_cqi_subcorpus_size <- function() {}
test_cqi_get_field_key <- function() {}
test_cqi_subcorpus_has_field <- function() {}
test_cqi_dump_subcorpus <- function() {}
test_cqi_drop_subcorpus <- function() {}
test_cqi_fdist <- function() {}

# #
# #
# #  CQi expect response / data
# #
# #

test_cqi_expect_byte <- function() {}
test_cqi_expect_bool <- function() {}
test_cqi_expect_int <- function() {}
test_cqi_expect_string <- function() {}
test_cqi_expect_byte_list <- function() {}
test_cqi_expect_bool_list <- function() {}
test_cqi_expect_int_list <- function() {}
test_cqi_expect_string_list <- function() {}
test_cqi_expect_int_int <- function() {}
test_cqi_expect_int_int_int_int <- function() {}
test_cqi_expect_int_table <- function() {}
test_cqi_expect_status <- function() {}

# #
# #
# #  Internal subroutines (read / write)
# #
# #

test_cqi_send_byte <- function() {}
test_cqi_send_word <- function() {}
test_cqi_send_int <- function() {}
test_cqi_send_string <- function() {}
test_cqi_send_byte_list <- function() {}
test_cqi_send_word_list <- function() {}
test_cqi_send_int_list <- function() {}
test_cqi_send_string_list <- function() {}
test_cqi_flush <- function() {}
test_cqi_read_byte <- function() {}
test_cqi_read_word <- function() {}
test_cqi_read_int <- function() {}
test_cqi_read_string <- function() {}
test_cqi_read_byte_list <- function() {}
test_cqi_read_word_list <- function() {}
test_cqi_read_int_list <- function() {}
test_cqi_read_string_list <- function() {}
test_cqi_read_int_table <- function() {}


