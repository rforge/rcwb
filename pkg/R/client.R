#
#
#  Error Handling
#
#

get_cwb <- function(server_options="", port=4877, host="localhost") {
  d <- cqi_server(userflags=server_options, port, host);
  Sys.sleep(0.5);
  con <- cqi_connect(user=d["user"], passwd=d["passwd"], host=d["host"], port=d["port"]);
  return(con);
}

######## ######## ######## ######## ######## ######## ######## ######## ######## ########

#our $LastCmd = "<none>"; # keep track of last command in case we receive an error code

#sub CqiError (@) {
#  foreach (@_) {
#    print STDERR "CQi ERROR: $_\n";
#  }
#  croak "CQI::Client -- connection aborted.";
#  exit 1;                       # Perl/Tk seems to catch the croak ... 
#}

CqiError <- function(err, con) {
  close(con);
  stop(paste("Connection aborted; error:", err));
}

#sub CqiErrorCode ($) {
#  my $errcode = shift;
#  my $group = $errcode >> 8;
#  my $command = $errcode & 0xff;
#  my $errhex = sprintf "%02X:%02X", $group, $command;
#  my $name = $CWB::CQI::CommandName{$errcode};
#  
#  if ($name =~ /ERROR/) {
#    CqiError "Received $name  [$errhex]  in response to", "$LastCmd";
#  }
#  else {
#    CqiError "Unexpected response $name  [$errhex]  to", "$LastCmd";
#  }
#}
#
#sub CqiCheckResponse ($@) {
#  my $response = shift;
#  my %expect = map { $_ => 1 } @_;
#  
#  CqiErrorCode $response
#    unless defined $expect{$response};
#}

## TODO more sophisticated
CqiCheckResponse <- function(con, found, expected) {
  if (found != expected) {
    if (found == CL_ERROR) {
      print(paste("Error:", "CL_ERROR"));
    } else if (found == CL_ERROR_CORPUS_ACCESS) {
      print(paste("Error:", "CL_ERROR_CORPUS_ACCESS"));
    } else if (found == CL_ERROR_INTERNAL) {
      print(paste("Error:", "CL_ERROR_INTERNAL"));
    } else if (found == CL_ERROR_NO_SUCH_ATTRIBUTE) {
      print(paste("Error:", "CL_ERROR_NO_SUCH_ATTRIBUTE"));
    } else if (found == CL_ERROR_OUT_OF_MEMORY) {
      print(paste("Error:", "CL_ERROR_OUT_OF_MEMORY"));
    } else if (found == CL_ERROR_OUT_OF_RANGE) {
      print(paste("Error:", "CL_ERROR_OUT_OF_RANGE"));
    } else if (found == CL_ERROR_REGEX) {
      print(paste("Error:", "CL_ERROR_REGEX"));
    } else if (found == CL_ERROR_WRONG_ATTRIBUTE_TYPE) {
      print(paste("Error:", "CL_ERROR_WRONG_ATTRIBUTE_TYPE"));
    } else if (found == CQP_ERROR_NO_SUCH_CORPUS) {
      print(paste("Error:", "CQP_ERROR_NO_SUCH_CORPUS"));
    } else if (found == CQP_ERROR_INVALID_FIELD) {
      print(paste("Error:", "CQP_ERROR_INVALID_FIELD"));
    } else if (found == CQP_ERROR_OUT_OF_RANGE) {
      print(paste("Error:", "CQP_ERROR_OUT_OF_RANGE"));
    }
    stop(paste("Expected:", expected, ";found:", found, " : ", last_error_msg(con)));
  } else {
    return(TRUE);
  }
}

last_error_msg <- function(con) {
   cqi_send_word(CTRL_LAST_GENERAL_ERROR, con);
   #cqi_send_string(corpus, con);
   cqi_flush(con);
   return(cqi_expect_string(con));
}

#
#
#  Connect to CQi server / Disconnect
#
#

# sub cqi_connect {
#   my $user = shift;
#   my $passwd = shift;
#   my $host = shift;             # optional
#     my $port = shift;             # optional
# 
#     $host = 'localhost'
#     unless defined $host;
#   $port = $CWB::CQI::PORT
#     unless defined $port;
# 
#   croak "USAGE: cqi_connect(username, password, [, remotehost [, port]]);"
#     unless defined $user and defined $passwd;
#   $LastCmd = "CQI_CTRL_CONNECT($user, '$passwd', $host, $port)";
# 
#   my $ipaddr = inet_aton($host);
#   my $sockaddr = sockaddr_in($port, $ipaddr);
#   my $protocol = getprotobyname('tcp');
# 
#   socket($conn, PF_INET, SOCK_STREAM, $protocol)
#     or do { croak "cqi_connect(): $!", exit 1};
#   connect($conn, $sockaddr)
#     or do { croak "cqi_connect(): $!", exit 1};
# 
#   cqi_send_word($CWB::CQI::CTRL_CONNECT);
#   cqi_send_string($user);
#   cqi_send_string($passwd);
#   cqi_flush();
# 
#   my $response = cqi_read_word();
#   CqiCheckResponse $response, $CWB::CQI::STATUS_CONNECT_OK;
# }

cqi_connect <- function(user, passwd, host="localhost", port=PORT) {

  con <- socketConnection(host, port, server = FALSE, blocking = TRUE, open = "r+b");

  Sys.sleep(0.2);

  cqi_send_word(as.integer(CTRL_CONNECT), con);
  cqi_send_string(user, con);
  cqi_send_string(passwd, con);

  response <- cqi_read_word(con)
  if (CqiCheckResponse(con, response, STATUS_CONNECT_OK)) {
    print("Connected to cwb server");
  }
  return(con);
}

# sub cqi_bye {
#   $LastCmd = "CQI_CTRL_BYE()";
#   cqi_send_word($CWB::CQI::CTRL_BYE);
#   cqi_flush();
#   my $response = cqi_read_word();
#   CqiCheckResponse $response, $CWB::CQI::STATUS_BYE_OK;
#   $conn->close;
#   $conn = undef;
# }

cqi_bye <- function(con) {
  cqi_send_word(CTRL_BYE, con);
  cqi_flush(con);
  resp <- cqi_read_word(con);
  CqiCheckResponse(con, resp, STATUS_BYE_OK);
  close(con);
}

# sub cqi_ping {
#   $LastCmd = "CQI_CTRL_PING()";
#   cqi_send_word($CWB::CQI::CTRL_PING);
#   cqi_flush();
#   CqiCheckResponse cqi_read_word(), $CWB::CQI::STATUS_PING_OK;
# }

cqi_ping <- function(con) {
   cqi_send_word(CTRL_PING, con);
   cqi_flush(con);
   CqiCheckResponse(con, cqi_read_word(con), STATUS_PING_OK);
}

# #
# #
# #  CQi Commands
# #
# #

# sub cqi_ask_feature {
#   my $feature = lc shift;
#   my %features = (
#                   "cqi1.0" => $CWB::CQI::ASK_FEATURE_CQI_1_0,
#                   "cl2.3"  => $CWB::CQI::ASK_FEATURE_CL_2_3,
#                   "cqp2.3" => $CWB::CQI::ASK_FEATURE_CQP_2_3,
#                   );
#   croak "USAGE: \$supported = cqi_ask_feature('cqi1.0' | 'cl2.3' | 'cqp2.3');"
#     unless defined $features{$feature};
#   $LastCmd = $CWB::CQI::CommandName{$features{$feature}} . "()";
#   cqi_send_word($features{$feature});
#   cqi_flush();
#   return cqi_expect_bool();
# }

# sub cqi_list_corpora {
#   $LastCmd = "CQI_CORPUS_LIST_CORPORA()";
#   croak "USAGE: \@corpora = cqi_list_corpora();"
#     unless @_ == 0;
#   cqi_send_word($CWB::CQI::CORPUS_LIST_CORPORA);
#   cqi_flush();
#   return cqi_expect_string_list();
# }

cqi_list_corpora <- function (con) {
  #$LastCmd = "CQI_CORPUS_LIST_CORPORA()";                                 
  cqi_send_word(as.integer(CORPUS_LIST_CORPORA), con);
  flush(con);
  return(cqi_expect_string_list(con)); 
} 

# sub cqi_charset {
#   my $corpus = shift;
#   $LastCmd = "CQI_CORPUS_CHARSET($corpus)";
#   cqi_send_word($CWB::CQI::CORPUS_CHARSET);
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string();
# }

cqi_charset <- function(corpus, con) {
   cqi_send_word(CORPUS_CHARSET, con);
   cqi_send_string(corpus, con);
   cqi_flush(con);
   return(cqi_expect_string(con));
}

# sub cqi_properties {
#   my $corpus = shift;
#   $LastCmd = "CQI_CORPUS_PROPERTIES($corpus)";
#   cqi_send_word($CWB::CQI::CORPUS_PROPERTIES);
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string_list();
# }

cqi_properties <- function(corpus, con) {
   cqi_send_word(CORPUS_PROPERTIES, con);
   cqi_send_string(corpus, con);
   cqi_flush(con);
   return(cqi_expect_string_list(con));
}

# sub cqi_attributes {
#   my $corpus = shift;
#   my $type = shift;
#   my %types = (
#                'p' => $CWB::CQI::CORPUS_POSITIONAL_ATTRIBUTES,
#                's' => $CWB::CQI::CORPUS_STRUCTURAL_ATTRIBUTES,
#                'a' => $CWB::CQI::CORPUS_ALIGNMENT_ATTRIBUTES,
#               );
#   croak "USAGE: \@attributes = cqi_attributes(\$corpus, ('p'|'s'|'a'));"
#     unless defined $types{$type};
#   $LastCmd = $CWB::CQI::CommandName{$types{$type}} . "($corpus)";
#   cqi_send_word($types{$type});
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string_list();
# }

cqi_attributes <- function (corpus, type, con) {
  types = c(
      'p' = CORPUS_POSITIONAL_ATTRIBUTES,
      's' = CORPUS_STRUCTURAL_ATTRIBUTES,
      'a' = CORPUS_ALIGNMENT_ATTRIBUTES
      );
  if (is.null(types[type])) {
    stop("USAGE: attributes = cqi_attributes(corpus, ('p'|'s'|'a'), con);");
  }
  cqi_send_word(types[type], con);
  cqi_send_string(corpus, con);
  cqi_flush(con);
  return(cqi_expect_string_list(con));
}

# sub cqi_structural_attribute_has_values {
#   my $attribute = shift;
#   $LastCmd = "CQI_CORPUS_STRUCTURAL_ATTRIBUTE_HAS_VALUES($attribute)";
#   cqi_send_word($CWB::CQI::CORPUS_STRUCTURAL_ATTRIBUTE_HAS_VALUES);
#   cqi_send_string($attribute);
#   cqi_flush();
#   return cqi_expect_bool();
# }

cqi_structural_attribute_has_values <- function(attribute, con) {
  cqi_send_word(CORPUS_STRUCTURAL_ATTRIBUTE_HAS_VALUES, con);
  cqi_send_string(attribute, con);
  cqi_flush(con);
  return(cqi_expect_bool(con));
}

# sub cqi_full_name {
#   my $corpus = shift;
#   $LastCmd = "CQI_CORPUS_FULL_NAME($corpus)";
#   cqi_send_word($CWB::CQI::CORPUS_FULL_NAME);
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string();
# }

cqi_full_name <- function(corpus, con) {
  cqi_send_word(CORPUS_FULL_NAME, con);
  cqi_send_string(corpus, con);
  cqi_flush(con);
  return(cqi_expect_string(con));
}

# sub cqi_corpus_info {
#   my $corpus = shift;
#   $LastCmd = "CQI_CORPUS_INFO($corpus)";
#   cqi_send_word($CWB::CQI::CORPUS_INFO);
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string_list();
# }

cqi_corpus_info <- function(corpus, con) {
   cqi_send_word(CORPUS_INFO, con);
   cqi_send_string(corpus, con);
   cqi_flush(con);
   return(cqi_expect_string_list(con));
 }

# sub cqi_drop_corpus {
#   my $corpus = shift;
#   $LastCmd = "CQI_CORPUS_DROP_CORPUS($corpus)";
#   cqi_send_word($CWB::CQI::CORPUS_DROP_CORPUS);
#   cqi_send_string($corpus);
#   cqi_flush();
#   cqi_expect_status($CWB::CQI::STATUS_OK);
# }
# 
# sub cqi_attribute_size {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_ATTRIBUTE_SIZE($attribute)";
#   cqi_send_word($CWB::CQI::CL_ATTRIBUTE_SIZE);
#   cqi_send_string($attribute);
#   cqi_flush();
#   return cqi_expect_int();
# }

cqi_attribute_size <- function(attribute, con) {
  cqi_send_word(CL_ATTRIBUTE_SIZE, con);
  cqi_send_string(attribute, con);
  cqi_flush(con);
  return(cqi_expect_int(con));
}

# sub cqi_lexicon_size {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_LEXICON_SIZE($attribute)";
#   cqi_send_word($CWB::CQI::CL_LEXICON_SIZE);
#   cqi_send_string($attribute);
#   cqi_flush();
#   return cqi_expect_int();
# }

cqi_lexicon_size <- function(attribute, con) {
  cqi_send_word(CL_LEXICON_SIZE, con);
  cqi_send_string(attribute, con);
  cqi_flush(con);
  return(cqi_expect_int(con));
}

# sub cqi_drop_attribute {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_DROP_ATTRIBUTE($attribute)";
#   cqi_send_word($CWB::CQI::CL_DROP_ATTRIBUTE);
#   cqi_send_string($attribute);
#   cqi_flush();
#   cqi_expect_status($CWB::CQI::STATUS_OK);
# }

# # 'scalar' functions which map to lists in the CQi are wrapped
# # in a scalar-safe client interface, so we CAN use them with simple
# # scalars in CQI::Client. 
# sub cqi_str2id {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_STR2ID($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_STR2ID);
#   cqi_send_string($attribute);
#   cqi_send_string_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_str2id <- function(attribute, strs, con) {
  cqi_send_word(CL_STR2ID, con);
  cqi_send_string(attribute, con);
  cqi_send_string_list(strs, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_id2str {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_ID2STR($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_ID2STR);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_string_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_id2str <- function (attribute, ids, con) {
  cqi_send_word(CL_ID2STR, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  res <- cqi_expect_string_list(con);
}

# sub cqi_id2freq {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_ID2FREQ($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_ID2FREQ);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_id2freq <- function(attribute, ids, con) {
  cqi_send_word(CL_ID2FREQ, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_cpos2id {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2ID($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2ID);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2id <- function(attribute, cpos, con) {
  cqi_send_word(CL_CPOS2ID, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(cpos, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_cpos2str {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2STR($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2STR);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_string_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2str <- function(attribute, cpos, con) {
  cqi_send_word(CL_CPOS2STR, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(cpos, con);
  cqi_flush(con);
  res = cqi_expect_string_list(con);
  return(res);
}

# sub cqi_cpos2struc {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2STRUC($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2STRUC);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2struc <- function (attribute, ids, con) {
  cqi_send_word(CL_CPOS2STRUC, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_cpos2lbound {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2LBOUND($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2LBOUND);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2lbound <- function (attribute, ids, con) {
  cqi_send_word(CL_CPOS2LBOUND, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_cpos2rbound {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2RBOUND($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2RBOUND);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2rbound <- function (attribute, cpos, con) {
  cqi_send_word(CL_CPOS2RBOUND, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(cpos, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_cpos2alg {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_CPOS2ALG($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_CPOS2ALG);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_int_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_cpos2alg <- function(attribute, cpos, con) {
  cqi_send_word(CL_CPOS2ALG, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(cpos, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_struc2str {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_STRUC2STR($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_STRUC2STR);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   my @list = cqi_expect_string_list();
#   if (wantarray) {
#     return @list;
#   }
#   else {
#     croak "Called in scalar context with list argument." unless @list == 1;
#     return $list[0];
#   }
# }

cqi_struc2str <- function(attribute, ids, con) {
  cqi_send_word(CL_STRUC2STR, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  res <- cqi_expect_string_list(con);
  return(res);
}

# sub cqi_id2cpos {
#   croak "USAGE: \@cposlist = cqi_id2cpos(\$attribute, \$id);"
#     unless @_ == 2 and wantarray;
#   my $attribute = shift;
#   my $id = shift;
# 
#   $LastCmd = "CQI_CL_ID2CPOS($attribute, $id)";
#   cqi_send_word($CWB::CQI::CL_ID2CPOS);
#   cqi_send_string($attribute);
#   cqi_send_int($id);
#   cqi_flush();
#   return cqi_expect_int_list();
# }

cqi_id2cpos <- function(attribute, id, con) {
  cqi_send_word(CL_ID2CPOS, con);
  cqi_send_string(attribute, con);
  cqi_send_int(id, con);
  cqi_flush(con);
  res <- cqi_expect_int_list(con);
  return(res);
}

# sub cqi_idlist2cpos {
#   my $attribute = shift;
#   $LastCmd = "CQI_CL_IDLIST2CPOS($attribute, [@_])";
#   cqi_send_word($CWB::CQI::CL_IDLIST2CPOS);
#   cqi_send_string($attribute);
#   cqi_send_int_list(@_);
#   cqi_flush();
#   return cqi_expect_int_list();
# }

cqi_idlist2cpos <- function(attribute, ids, con) {
  cqi_send_word(CL_IDLIST2CPOS, con);
  cqi_send_string(attribute, con);
  cqi_send_int_list(ids, con);
  cqi_flush(con);
  return(cqi_expect_int_list(con));
}

# sub cqi_regex2id {
#   croak "USAGE: \@idlist = cqi_regex2id(\$attribute, \$regex);"
#     unless @_ == 2 and wantarray;
#   my $attribute = shift;
#   my $regex = shift;
# 
#   $LastCmd = "CQI_CL_REGEX2ID($attribute, $regex)";
#   cqi_send_word($CWB::CQI::CL_REGEX2ID);
#   cqi_send_string($attribute);
#   cqi_send_string($regex);
#   cqi_flush();
#   return cqi_expect_int_list();
# }

cqi_regex2id <- function(attribute, regex, con) {
  cqi_send_word(CL_REGEX2ID, con);
  cqi_send_string(attribute, con);
  cqi_send_string(regex, con);
  cqi_flush(con);
  return(cqi_expect_int_list(con));
}

# sub cqi_struc2cpos {
#   croak "USAGE: (\$start, \$end) = cqi_struc2cpos(\$attribute, \$struc);"
#     unless @_ == 2 and wantarray;
#   my $attribute = shift;
#   my $struc = shift;
# 
#   $LastCmd = "CQI_CL_STRUC2CPOS($attribute, $struc)";
#   cqi_send_word($CWB::CQI::CL_STRUC2CPOS);
#   cqi_send_string($attribute);
#   cqi_send_int($struc);
#   cqi_flush();
#   return cqi_expect_int_int();
# }

cqi_struc2cpos <- function (attribute, struc, con) {
  cqi_send_word(CL_STRUC2CPOS, con);
  cqi_send_string(attribute, con);
  cqi_send_int(struc, con);
  cqi_flush(con);
  return(cqi_expect_int_int(con));
}

# sub cqi_alg2cpos {
#   croak "USAGE: (\$s1, \$s2, \$t1, \$t2) = cqi_alg2cpos(\$attribute, \$alg);"
#     unless @_ == 2 and wantarray;
#   my $attribute = shift;
#   my $alg = shift;
# 
#   $LastCmd = "CQI_CL_ALG2CPOS($attribute, $alg)";
#   cqi_send_word($CWB::CQI::CL_ALG2CPOS);
#   cqi_send_string($attribute);
#   cqi_send_int($alg);
#   cqi_flush();
#   return cqi_expect_int_int_int_int();
# }
# 
# # cqi_query() returns a CQi response code (CQI_STATUS_OK or error).
# # An error code usually indicates a mistake in the query syntax.
# # It aborts the program unless one of the following responses is received:
# #   CQI_STATUS_OK
# #   CQI_ERROR_*
# #   CQI_CQP_ERROR_*
# sub cqi_query {
#   my ($mother, $child, $query) = @_;
#   croak "USAGE: \$ok = cqi_query(\$mother_corpus, \$subcorpus_name, \$query);"
#     unless @_ == 3 and $mother =~ /^[A-Z0-9_-]+(:[A-Z_][A-Za-z0-9_-]*)?$/
#       and $child =~ /^[A-Z_][A-Za-z0-9_-]*$/;
#   $query .= ";"
#     unless $query =~ /;\s*$/;
#   
#   $LastCmd = "CQI_CQP_QUERY($mother, $child, '$query')";
#   cqi_send_word($CWB::CQI::CQP_QUERY);
#   cqi_send_string($mother);
#   cqi_send_string($child);
#   cqi_send_string($query);
#   cqi_flush();
#   my $response = cqi_read_word();
#   my $group = $response >> 8;
#   CqiError $response
#     unless $response == $CWB::CQI::STATUS_OK or $group == $CWB::CQI::ERROR or $group == $CWB::CQI::CQP_ERROR;
#   return $response;
# }

cqi_query <- function(mother, child, query, con) {
  cqi_send_word(CQP_QUERY, con);
  cqi_send_string(mother, con);
  cqi_send_string(child, con);
  cqi_send_string(query, con);
  flush(con);
  resp <- cqi_read_word(con);
  resp_group <- bitShiftR(resp, 8);
  if (
      resp == STATUS_OK
      || resp_group == ERROR
      || resp_group == CQP_ERROR
     ) {
    return(resp);
  } else {
    CqiError(resp, con);
  }
}

# sub cqi_list_subcorpora {
#   my $corpus = shift;
#   $LastCmd = "CQI_CQP_LIST_SUBCORPORA($corpus)";
#   cqi_send_word($CWB::CQI::CQP_LIST_SUBCORPORA);
#   cqi_send_string($corpus);
#   cqi_flush();
#   return cqi_expect_string_list();
# }

cqi_list_subcorpora <- function(corpus, con) {
  cqi_send_word(CQP_LIST_SUBCORPORA, con);
  cqi_send_string(corpus, con);
  cqi_flush(con);
  return(cqi_expect_string_list(con));
}

# sub cqi_subcorpus_size {
#   my $subcorpus = shift;
#   $LastCmd = "CQI_CQP_SUBCORPUS_SIZE($subcorpus)";
#   cqi_send_word($CWB::CQI::CQP_SUBCORPUS_SIZE);
#   cqi_send_string($subcorpus);
#   cqi_flush();
#   return cqi_expect_int();
# }

cqi_subcorpus_size <- function(subcorpus, con) {
  cqi_send_word(CQP_SUBCORPUS_SIZE, con);
  cqi_send_string(subcorpus, con);
  flush(con);
  return(cqi_expect_int(con));
}

# 
# # used internally
# sub cqi_get_field_key {
#   my $field = uc shift;
#   if ($field =~ /^(MATCH(END)?|TARGET|KEYWORD)$/) {
#     return eval "\$CWB::CQI::CONST_FIELD_$field";
#   }
#   else {
#     return undef;
#   }
# }

cqi_get_field_key <- function(key) {
  key <- toupper(key);
  if (key == "MATCH") {
    return(CONST_FIELD_MATCH);
  } else if (key == "MATCHEND") {
    return(CONST_FIELD_MATCHEND);
  } else if (key == "TARGET") {
    return(CONST_FIELD_TARGET);
  } else if (key == "KEYWORD") {
    return(CONST_FIELD_KEYWORD);
  } else {
    stop(paste("unknwon field:",key));
  }
}

# sub cqi_subcorpus_has_field {
#   my ($subcorpus, $field) = @_;
#   croak "USAGE: \$ok = cqi_subcorpus_has_field(\$subcorpus, 'match'|'matchend'|'target'|'keyword');"
#     unless @_ == 2 and defined (my $field_key = cqi_get_field_key($field));
#   $LastCmd = "CQI_CQP_SUBCORPUS_HAS_FIELD($subcorpus, CQI_CONST_FIELD_".(uc $field).")";
#   cqi_send_word($CWB::CQI::CQP_SUBCORPUS_HAS_FIELD);
#   cqi_send_string($subcorpus);
#   cqi_send_byte($field_key);
#   cqi_flush();
#   return cqi_expect_bool();
# }

# sub cqi_dump_subcorpus {
#   my ($subcorpus, $field, $first, $last) = @_;
#   croak "USAGE: \@column = cqi_dump_subcorpus(\$subcorpus, 'match'|'matchend'|'target'|'keyword', \$from, \$to);"
#     unless @_ == 4 and defined (my $field_key = cqi_get_field_key($field));
#   $LastCmd = "CQI_CQP_DUMP_SUBCORPUS($subcorpus, CQI_CONST_FIELD_".(uc $field).", $first, $last)";
#   cqi_send_word($CWB::CQI::CQP_DUMP_SUBCORPUS);
#   cqi_send_string($subcorpus);
#   cqi_send_byte($field_key);
#   cqi_send_int($first);
#   cqi_send_int($last);
#   cqi_flush();
#   return cqi_expect_int_list();
# }

cqi_dump_subcorpus <- function(subcorpus, field, first, last, con) {
  field_key = cqi_get_field_key(field);

  cqi_send_word(CQP_DUMP_SUBCORPUS, con);
  cqi_send_string(subcorpus, con);
  cqi_send_byte(field_key, con);
  cqi_send_int(first, con);
  cqi_send_int(last, con);
  cqi_flush(con);

  return(cqi_expect_int_list(con));
}

# sub cqi_drop_subcorpus {
#   my $subcorpus = shift;
#   $LastCmd = "CQI_CQP_DROP_SUBCORPUS($subcorpus)";
#   cqi_send_word($CWB::CQI::CQP_DROP_SUBCORPUS);
#   cqi_send_string($subcorpus);
#   cqi_flush();
#   cqi_expect_status($CWB::CQI::STATUS_OK);
# }

cqi_drop_subcorpus <- function(subcorpus, con) {
  cqi_send_word(CQP_DROP_SUBCORPUS, con);
  cqi_send_string(subcorpus, con);
  cqi_flush(con);
  cqi_expect_status(STATUS_OK, con);
}

# ## cqi_fdist() subsumes both cqi_fdist_1() and cqi_fdist_2()
# ## returns list of (id, f) or (id1, id2, f) tuples as hashref's
# sub cqi_fdist {
#   my $subcorpus = shift;
#   my $cutoff = shift;
#   my $key1 = shift;
#   my $key2 = shift;
#   my ($field1, $field2, $att1, $att2, $tmp);
#   ($tmp, $att1) = split /\./, $key1;
#   $field1 = cqi_get_field_key($tmp);
#   if (defined $key2) {
#     ($tmp, $att2) = split /\./, $key2;
#     $field2 = cqi_get_field_key($tmp);
#   }
#   else {
#     $field2 = "";
#     $att2 = "x";
#   }
#   croak "USAGE: \@table = cqi_fdist(\$subcorpus, \$cutoff, \$key1 [, \$key2]);"
#     unless @_ == 0 and defined $field1 and defined $field2 and defined $att1 and defined $att2
#       and $att1 =~ /^[a-z]+$/ and $att2 =~ /^[a-z]+$/ and $cutoff >= 0;
#   if ($field2 ne "") {
#     $LastCmd = "CQI_CQP_FDIST_2($subcorpus, $cutoff, $key1, $key2)";
#     cqi_send_word($CWB::CQI::CQP_FDIST_2);
#     cqi_send_string($subcorpus);
#     cqi_send_int($cutoff);
#     cqi_send_byte($field1);
#     cqi_send_string($att1);
#     cqi_send_byte($field2);
#     cqi_send_string($att2);
#     cqi_flush();
#     return cqi_expect_int_table();
#   }
#   else {
#     $LastCmd = "CQI_CQP_FDIST_1($subcorpus, $cutoff, $key1)";
#     cqi_send_word($CWB::CQI::CQP_FDIST_1);
#     cqi_send_string($subcorpus);
#     cqi_send_int($cutoff);
#     cqi_send_byte($field1);
#     cqi_send_string($att1);
#     cqi_flush();
#     return cqi_expect_int_table();
#   }
# }

# subcorpus, 0, MATCH.lemma
cqi_fdist1 <- function(subcorpus, cutoff, key1, con) {
  field1  <- "";
  att1  <- "";
  tmp_att_1 <- strsplit(key1, "\\.")[[1]];
  field1 = cqi_get_field_key(tmp_att_1[1]);
  att1 <- tmp_att_1[2];
  cqi_send_word(CQP_FDIST_1, con);
  cqi_send_string(subcorpus, con);
  cqi_send_int(cutoff, con);
  cqi_send_byte(field1, con);
  cqi_send_string(att1, con);
  cqi_flush(con);
  return(cqi_expect_int_table(con));
}

## subcorpus, 0, MATCH.lemma
#cqi_fdist <- function(subcorpus, cutoff, con, key1, key2) {
#  field1  <- "";
#  field2  <- "";
#  att1  <- "";
#  att2  <- "";
#  print(paste("-->", key1));
#  tmp_att_1 <- strsplit(key1, "\\.")[[1]];
#  print(paste("-------", tmp_att_1[1]));
#  field1 = cqi_get_field_key(tmp_att_1[1]);
#  att1 <- tmp_att_1[2];
#  if (!missing(key2)) {
#    tmp_att_2 <- strsplit(key2, "\\.")[[1]];
#    field2 = cqi_get_field_key(tmp_att_2[1]);
#    att2 <- tmp_att_2[2];
#  }
#  else {
#    field2 <- "";
#    att2 <- "x";
#  }
#  if (!missing(key2)) {
#    cqi_send_word(CQP_FDIST_2, con);
#    cqi_send_string(subcorpus, con);
#    cqi_send_int(cutoff, con);
#    cqi_send_byte(field1, con);
#    cqi_send_string(att1, con);
#    cqi_send_byte(field2, con);
#    cqi_send_string(att2, con);
#    cqi_flush(con);
#    return(cqi_expect_int_table(con));
#  }
#  else {
#    cqi_send_word(CQP_FDIST_1, con);
#    cqi_send_string(subcorpus, con);
#    cqi_send_int(cutoff, con);
#    cqi_send_byte(field1, con);
#    cqi_send_string(att1, con);
#    cqi_flush(con);
#    return(cqi_expect_int_table(con));
#  }
#}

# #
# #
# #  CQi expect response / data
# #
# #
# sub cqi_expect_byte {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_BYTE;
#   return cqi_read_byte();
# }

# sub cqi_expect_bool {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_BOOL;
#   return cqi_read_byte();
# }

cqi_expect_bool <- function(con) {
  r <- cqi_read_word(con);
  CqiCheckResponse(con, r, DATA_BOOL);
  b <- rawToChar(cqi_read_byte(con));
  return(ifelse(b == "", FALSE, TRUE));
}

# sub cqi_expect_int {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_INT;
#   return cqi_read_int();
# }

cqi_expect_int <- function(con) {
 r <- cqi_read_word(con);
 CqiCheckResponse(con, r, DATA_INT);
 return(cqi_read_int(con));
}

# sub cqi_expect_string {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_STRING;
#   return cqi_read_string();
# }

cqi_expect_string <- function(con) {
   r = cqi_read_word(con);
   CqiCheckResponse(con, r, DATA_STRING);
   return(cqi_read_string(con));
}

# sub cqi_expect_byte_list {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_BYTE_LIST;
#   return cqi_read_byte_list();
# }
# 
# sub cqi_expect_bool_list {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_BOOL_LIST;
#   return cqi_read_byte_list();
# }
# 
# sub cqi_expect_int_list {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_INT_LIST;
#   return cqi_read_int_list();
# }

cqi_expect_int_list <- function(con) {
   r <- cqi_read_word(con);
   CqiCheckResponse(con, r, DATA_INT_LIST);
   return(cqi_read_int_list(con));
}
#Erreur dans CqiCheckResponse(con, r, DATA_INT_LIST) : Expected: 775 ;found: 1026


# sub cqi_expect_string_list {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_STRING_LIST;
#   return cqi_read_string_list();
# }

cqi_expect_string_list <- function(con) {
   r = cqi_read_word(con);
   CqiCheckResponse(con, r, DATA_STRING_LIST);
   return(cqi_read_string_list(con));
}

# sub cqi_expect_int_int {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_INT_INT;
#   return cqi_read_int(), cqi_read_int();
# }

cqi_expect_int_int <- function(con) {
  r <- cqi_read_word(con);
  CqiCheckResponse(con, r, DATA_INT_INT);
  return(c(cqi_read_int(con), cqi_read_int(con)));
}

# sub cqi_expect_int_int_int_int {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_INT_INT_INT_INT;
#   return cqi_read_int(), cqi_read_int(), cqi_read_int(), cqi_read_int();
# }

# sub cqi_expect_int_table {
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, $CWB::CQI::DATA_INT_TABLE;
#   return cqi_read_int_table();
# }

cqi_expect_int_table <- function(con) {
  r <- cqi_read_word(con);
  CqiCheckResponse(con, r, DATA_INT_TABLE);
  return(cqi_read_int_table(con));
}

# sub cqi_expect_status {
#   my @expected = @_;            # arguments are list of acceptable responses
#   my $r = cqi_read_word();
#   CqiCheckResponse $r, @expected;
#   return $r;
# }

cqi_expect_status <- function(expected, con) {
   r = cqi_read_word(con);
   CqiCheckResponse(con, r, expected);
   return(r);
}

# #
# #
# #  Internal subroutines (read / write)
# #
# #

# sub cqi_send_byte ($) {
#   $conn->print((pack "C", shift))
#     or croak "cqi_send_byte(): $!";
# }

cqi_send_byte <- function(byte, con) {
  writeBin(as.raw(byte), con, size=1, endian="big", useBytes=TRUE);
}
 
# sub cqi_send_word ($) {
#   $conn->print((pack "n", shift))
#     or croak "cqi_send_word(): $!";
# }

cqi_send_word <- function(word, con) {
  writeBin(as.integer(word), con, size=2, endian="big", useBytes=TRUE);
}

# sub cqi_send_int ($) {
#   my $number = shift;           # safely convert native int to 32bit value
#   $number = unpack "L", (pack "l", $number); # pack 32bit signed, unpack unsigned -> uses type which can hold unsigned 32bit value
#   $conn->print(pack("N", $number)) # 'N' packs unsigned 32bit integer
#     or croak "cqi_send_int(): $!";
# }

cqi_send_int <- function(int, con) {
  writeBin(as.integer(int), con, size=4, endian="big", useBytes=TRUE);
}

# sub cqi_send_string ($) {
#   my $str = shift;
#   $conn->print((pack "n", length $str), $str)
#     or croak "cqi_send_str(): $!";
# }

cqi_send_string <- function(str, con) {
  writeBin(as.integer(nchar(str)), con, size=2, endian="big", useBytes=TRUE);
  writeChar(str, con, nchar(str), eos=NULL);
}

# 
# sub cqi_send_byte_list (@) {
#   cqi_send_int(scalar @_);
#   map {cqi_send_byte($_)} @_;
# }
# 
# sub cqi_send_word_list (@) {
#   cqi_send_int(scalar @_);
#   map {cqi_send_word($_)} @_;
# }
# 
# sub cqi_send_int_list (@) {
#   cqi_send_int(scalar @_);
#   map {cqi_send_int($_)} @_;
# }

cqi_send_int_list <- function(ints, con) {
  cqi_send_int(length(ints), con);
  for (i in ints) {
    cqi_send_int(i, con);
  }
}

# sub cqi_send_string_list (@) {
#   cqi_send_int(scalar @_);
#   map {cqi_send_string($_)} @_;
# }

cqi_send_string_list <- function(str, con) {
  cqi_send_int(length(str), con);
  for (s in str) {
    cqi_send_string(s, con);
  }
}

# sub cqi_flush () {
#   $conn->flush
#     or croak "cqi_flush(): $!";
# }

cqi_flush <- function(con) {
  flush(con);
}

# sub cqi_read_byte () {
#   my $msg;
#   croak "cqi_read_byte(): $!"
#     unless defined recv $conn, $msg, 1, MSG_WAITALL;
#   return unpack "C", $msg;
# }

cqi_read_byte <- function(con) {
  r <- readBin(con=con, what="raw", n=1, signed=FALSE, endian="big");
  #print("r:");
  #print(r);
  #print("rawToChar");
  #print(rawToChar(r));
  return(r);
}

# sub cqi_read_word () {
#   my $msg;
#   croak "cqi_read_word(): $!"
#     unless defined recv $conn, $msg, 2, MSG_WAITALL;
#   return unpack "N", "\x00\x00$msg"; # this should safely unpack an unsigned short
# }

#cqi_read_word <- function(con) {
#  return(readBin(con, size=2, endian="big", useBytes=T));
#}

cqi_read_word <- function(con) {
  i <- readBin(con=con, what="integer", n=1, size=2, signed=FALSE, endian="big");
  return(i);
}

# sub cqi_read_int () {
#   my $msg;
#   my $number;
#   
#   croak "cqi_read_int(): $!"
#     unless defined recv $conn, $msg, 4, MSG_WAITALL;
#   $number = unpack "N", $msg;   # unpack seems to default to unsigned
#   $number = unpack "l", (pack "L", $number); # convert unsigned 32bit to internal signed int *phew*
#   return $number;
# }

cqi_read_int <- function(con) {
  i <- readBin(con=con, what="integer", n=1, size=4, signed=FALSE, endian="big");
  return(i)
}

# sub cqi_read_string () {
#   my ($msg, $len);
#   $len = cqi_read_word();
#   croak "cqi_read_string(): $!"
#     unless defined recv $conn, $msg, $len, MSG_WAITALL;
#   return $msg;
# }

cqi_read_string <- function(con) {
  #print("about to read");
  len <- cqi_read_word(con);
  #print(paste("length:", len));
  res <- readChar(con, len, TRUE);
  #print(paste("read ok:", res));
  return(res);
}

# sub cqi_read_byte_list() {
#   my ($i, $len, @list);
#   $len = cqi_read_int();
#   for ($i = $len; $i > 0; $i--) {
#     push @list, cqi_read_byte;
#   }
#   return @list;
# }
# 
# sub cqi_read_word_list() {
#   my ($i, $len, @list);
#   $len = cqi_read_int();
#   for ($i = $len; $i > 0; $i--) {
#     push @list, cqi_read_word();
#   }
#   return @list;
# }
# 
# sub cqi_read_int_list() {
#   my ($i, $len, @list);
#   $len = cqi_read_int();
#   for ($i = $len; $i > 0; $i--) {
#     push @list, cqi_read_int();
#   }
#   return @list;
# }

cqi_read_int_list <- function(con) {
   len <- cqi_read_int(con);
   res <- integer(len);
   for (i in len:1) {
     res[i] <- cqi_read_int(con);
   }
   return(res);
}

# sub cqi_read_string_list() {
#   my ($i, $len, @list);
#   $len = cqi_read_int();
#   for ($i = $len; $i > 0; $i--) {
#     push @list, cqi_read_string();
#   }
#   return @list;
# }

cqi_read_string_list <- function(con) {
  len <- cqi_read_int(con);
  res <- character(len);
  for (i in 1:len) {
    res[i] <- cqi_read_string(con);
  }
  return(res);
}

# sub cqi_read_int_table() {
#   my $rows = cqi_read_int();
#   my $columns = cqi_read_int();
#   my @table = ();
#   for (my $i = 0; $i < $rows; $i++) {
#     my @line = ();
#     for (my $j = 0; $j < $columns; $j++) {
#       push @line, cqi_read_int();
#     }
#     push @table, [@line];
#   }
#   return @table;
# }

cqi_read_int_table <- function(con) {
  rows <- cqi_read_int(con);
  columns <- cqi_read_int(con);
  mat <- matrix(0, nrow=rows, ncol=columns);
  for (i in 1:rows) {
    for (j in 1:columns) {
      mat[i,j] <- cqi_read_int(con);
    }
  }
  return(mat);
}
