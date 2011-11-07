#source("package.R");
source("client.R");
source("constantes.R");
source("server.R");
#
source("AbstractAttribute.R");
source("PositionalAttr.R");
source("StructuralAttr.R");
source("AbstractCorpus.R");
source("Corpus.R");
source("Subcorpus.R");
source("AbstractCorpus.R");
source("quantitative.R");

library(bitops);

# TODO : tester les -1 retourné si une forme n'existe pas

#     conv <- function(a, b)
#       .C("convolve",
#          as.double(a),
#          as.integer(length(a)),
#          as.double(b),
#          as.integer(length(b)),
#          ab = double(length(a) + length(b) - 1))$ab


sl <- "-r /Users/sylvainloiseau/TXM/registry"
debug <- T;

#registry <-  paste(system.file("example", package=packageDescription("Rcwb")$Package), "registry", sep="/");

quant <- function() {
  con <- get_cwb(server_options=sl);
  c <- corpus(corpus_id="DICO", cwb_connection=con);

  #lemma <- positionalAttr(c, "lemma")
  #text_id <- structuralAttr(c, "text_id");
  #ft <- region_value.ft(c, text_id, lemma);

  print("------------------------------------------------------------");
  print("region_value.fr");
  print("------------------------------------------------------------");

  ft <- region_value.ftable(c, "text_id", "lemma");
  print(dim(ft));
  return(ft);
}

test_s4 <- function(server_extra_arg=sl) {

  print("------------------------------------------------------------");
  print("get a connexion to cwb");
  print("------------------------------------------------------------");

  con <- get_cwb(server_options=sl);

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("Corpus");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  c <- corpus(corpus_id="DESCARTES", cwb_connection=con);
  print(c);

  # nbr of tokens
  print(size(c));

  # list positional attributes
  print(positionalAttr_ids(c));

  # list structural attributes
  print(structuralAttr_ids(c));

  # list available subcorpora
  #print(subcorpora(c));

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("positional attribute");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  pos_attr <- positionalAttr(c, "word")
  print(pos_attr);

  print("------------------------------------------------------------");
  print("positionalAttr(): create");
  print("------------------------------------------------------------");

  p_attr <- positionalAttr(c, "lemma");
  print(p_attr);
  print(paste("nbr_forms: ", nbr_form(p_attr)));

  #p_attr <- positionalAttr(c, "word");
  #print(p_attr);
  #print(paste("nbr_forms: ", nbr_form(p_attr)));

  # get id of attribute
  print(get_id(p_attr));

  print("------------------------------------------------------------");
  print("flist: freq of lemma 'le' et 'toujours'");
  print("------------------------------------------------------------");

  print(paste("Frequency of \"le\":", flist(p_attr, "le")));
  print(paste("Frequency of \"toujours\":", flist(p_attr, "toujours")));

  print("------------------------------------------------------------");
  print("flist: freq of lemma ID 0 and 1");;
  print("------------------------------------------------------------");

  print(paste("Frequency of 0:", flist(p_attr, 0)));
  print(paste("Frequency of 1:", flist(p_attr, 1)));

  print("------------------------------------------------------------");
  print("str2id: Ids of lemma 'le' et 'toujours'");
  print("------------------------------------------------------------");

  print(str2id(p_attr, form_strings=c("le", "toujours")));

  print("------------------------------------------------------------");
  print("id2str: string of form with id 0 and 1");
  print("------------------------------------------------------------");

  strs <- id2str(p_attr, form_ids=c(0,1));
  print(strs);

  print("------------------------------------------------------------");
  print("position2id: Ids of lemma at position 1:4");
  print("------------------------------------------------------------");

  ids <- position2id(p_attr, token_positions=1:4);
  print(ids);

  print("------------------------------------------------------------");
  print("position2str: string of position 1:4");
  print("------------------------------------------------------------");

  strs <- position2str(p_attr, token_positions=1:4);
  print(strs);

  print("------------------------------------------------------------");
  print("token_positions: positions of forms with id 0 et 1");
  print("------------------------------------------------------------");

  print(length(token_positions(p_attr, 0)));
  print(length(token_positions(p_attr, 1)));

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("structural attribute");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  s_attr <- structuralAttr(c, "text");
  print(s_attr);

  print("------------------------------------------------------------");
  print("nbr_region");
  print("------------------------------------------------------------");

  print(nbr_region(s_attr));

  print("------------------------------------------------------------");
  print("has_value");
  print("------------------------------------------------------------");

  print(has_value(s_attr));

  print("------------------------------------------------------------");
  print("region_id: given token positions, give the corresponding region ids");
  print("------------------------------------------------------------");

  print(region_id(s_attr, 27:30));

  print("------------------------------------------------------------");
  print("right_boundary_position: given token positions, give the right boundary position of the including region");
  print("------------------------------------------------------------");

  print(right_boundary_position(s_attr, 27:30));

  print("------------------------------------------------------------");
  print("left_boundary_position: given token positions, give the left boundary position of the including region");
  print("------------------------------------------------------------");

  print(left_boundary_position(s_attr, 27:30));

  print("------------------------------------------------------------");
  print("region_position: given a region id, give its left and right boundary positions");
  print("------------------------------------------------------------");

  print(region_position(s_attr, 0));

  # if a greater region id than the existing one is requested, an error is raised.

  print("------------------------------------------------------------");
  print("create a structural attribute with value: s_id");
  print("------------------------------------------------------------");

  s_attr <- structuralAttr(c, "s_id");
  print(s_attr);

  print("------------------------------------------------------------");
  print("region_value: the value given a region id");
  print("------------------------------------------------------------");

  print(region_value(s_attr, 1));

  print("------------------------------------------------------------");
  print("values_flist: the frequency list of all value of a structural attribute");
  print("------------------------------------------------------------");

  # print the first five
  print(values_flist(s_attr)[1:5]);

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("Subcorpus");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  #TODO tester sans argument "nom"

  c <- corpus(corpus_id="DESCARTES", cwb_connection=con);
  print(c);

  #sc <- subcorpus(c, "ss", "/region[s_id, a] :: a.s_id = \"discours_de_la_methode_s_92\";");
  sc <- subcorpus(c, "[lemma=\"le\"];", "Subcorpus");
  print(sc);

  # nbr of tokens
  print(paste("size subcorpus:", size(sc)));
  print(paste("subcorpus nbr match:", nbr_match(sc)));

  # id = qualified id
  print(paste("id subcorpus:", id(sc)));

  # NO : cannot list the positional attribute on a Subcorpus
  # print("Positional attribute");
  # print(positionalAttr_ids(sc));
  # print("Structural attribute");
  # print(structuralAttr_ids(sc));

  # subcorpus with automatic name
  sc <- subcorpus(c, "[lemma=\"le\"];");
  print(sc);

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("Subcorpus: drop");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  drop(sc);
  sc <- subcorpus(c, "[lemma=\"le\"];");

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("Attribute dist in Subcorpus");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  # flist on an attribute linked to a subcorpus
  p_attr_sc <- positionalAttr(sc, "word");
  print(p_attr_sc);
  m <- flist(p_attr_sc)
  print(m);

  id <- as.integer(names(m));
  print(id);
  str <- id2str(p_attr_sc, id);
  names(m) <- str;
  print(m);

  print("------------------------------------------------------------");
  print("------------------------------------------------------------");
  print("sub-Subcorpus");
  print("------------------------------------------------------------");
  print("------------------------------------------------------------");

  # Subcorpus in subcorpus
  ssc <- subcorpus(sc, "[word=\"le\"];", "Subsubcorpus");
  print(ssc);

  # get freq of "le" in corpus...
  p_attr <- positionalAttr(ssc, "lemma");
  print(paste("Frequency of \"le\" in subcorpus:", flist(p_attr, "le")));

  cqi_bye(con);
}

# test <- function(server_extra_arg=sl) {
#   print("------------------------------------------------------------");
#   print("Launching server");
#   print("------------------------------------------------------------");
#   d <- cqi_server(userflags=server_extra_arg);
#   Sys.sleep(1);
# 
#   print("------------------------------------------------------------");
#   print("Connecting socket");
#   print("------------------------------------------------------------");
#   con <- cqi_connect(user=d["user"], passwd=d["passwd"], host=d["host"], port=d["port"]);
# 
#   corpus <- "DESCARTES";
#   subcorpus <- "A";
# 
#   info_att_s <- cqi_attributes(corpus, "s", con);
#   print(info_att_s);
# 
#   info_att_p <- cqi_attributes(corpus, "p", con);
#   print(info_att_p);
# 
#   #info_att_a <- cqi_attributes(corpus, "a", con);
#   #print(info_att_a);
# 
#   #info <- cqi_properties(corpus, con);
#   #print(info);
# 
#   foo <- cqi_charset("DESCARTES", con);
#   print(foo);
# 
#   #foo <- cqi_corpus_info("DESCARTES", con);
#   #print(foo);
# 
#   # taille d'un attribut
# 
#   foo <- cqi_attribute_size("DESCARTES.word", con);
#   print(foo);
#   foo <- cqi_attribute_size("DESCARTES.pos", con);
#   print(foo);
#   foo <- cqi_attribute_size("DESCARTES.func", con);
#   print(foo);
#   foo <- cqi_attribute_size("DESCARTES.lemma", con);
#   print(foo);
# 
#   foo <- cqi_lexicon_size("DESCARTES.word", con);
#   print(foo);
#   foo <- cqi_lexicon_size("DESCARTES.pos", con);
#   print(foo);
#   foo <- cqi_lexicon_size("DESCARTES.func", con);
#   print(foo);
#   foo <- cqi_lexicon_size("DESCARTES.lemma", con);
#   print(foo);
# 
#  # print("lexicon size on structural attribute");
#  # foo <- cqi_lexicon_size("DESCARTES.s", con);
#  # print(foo);
#  # # erreur:
#  # # CL_ERROR_NO_SUCH_ATTRIBUTE
#  # # Erreur dans CqiCheckResponse(r, DATA_INT) : Expected: 771 ;found: 1025
# 
#   # print("lexicon size on structural attribute with value");
#   # foo <- cqi_lexicon_size("DESCARTES.s_id", con);
#   # print(foo);
#   # erreur:
#   # Erreur dans CqiCheckResponse(r, DATA_INT) : Expected: 771 ;found: 1025
# 
#   print("--------------------");
#   print("cqi_structural_attribute_has_values");
#   print("--------------------");
# 
#   s_has_value <- cqi_structural_attribute_has_values("DESCARTES.s", con);
#   print(s_has_value);
# 
#   s_id_has_value <- cqi_structural_attribute_has_values("DESCARTES.s_id", con);
#   print(s_id_has_value);
# 
#   ## vocabulaire d'un attribut
# 
#   print("cqi_str2id");
#  str <- c("VER:pper", "PRO:DEM" , "NOM" , "VER:pres", "ADV" , "ADJ" , "PRP" , "VER:infi", "DET:ART" , "PUN" , "PRO:PER" , "VER:futu", "NUM" , "SENT" , "KON" , "PRO:IND" , "NAM" , "PRO:REL" , "DET:POS" , "PRP:det" , "PRO" , "VER:subi", "VER:subp", "VER:ppre", "PRO:POS" , "VER:impf", "VER:simp", "VER:cond", "ABR" , "VER:impe");
#   id_pos <- cqi_str2id("DESCARTES.pos", str, con);
#   print(id_pos);
# 
#   print("--------------------------------")
#   print("cqi_id2str");
#   print("--------------------------------")
# 
#   print("cqi_id2str: positionnal attribute");
#   pos_id <- 0:29;
#   str_pos <- cqi_id2str("DESCARTES.pos", 0:29, con);
#   print(str_pos);
# 
#   #print("cqi_id2str: structural attribute without value");
#   #str_pos <- cqi_id2str("DESCARTES.s", 0:29, con);
#   #print(str_pos);
#   # --> Error
#   # Erreur dans CqiCheckResponse(r, DATA_STRING_LIST) : 
#   #   Expected: 776 ;found: 1025
#   
#   #print("cqi_id2str: structural attribute with value");
#   #str_pos <- cqi_id2str("DESCARTES.s_id", 0:29, con);
#   #print(str_pos);
#   # --> Error
#   # Erreur dans CqiCheckResponse(r, DATA_STRING_LIST) : 
#   #   Expected: 776 ;found: 1025
# 
#   print("--------------------------------")
#   print("cqi_id2freq");
#   print("--------------------------------")
# 
#   print("cqi_id2freq");
#   freq_pos <- cqi_id2freq("DESCARTES.pos", 0:29, con);
#   print(freq_pos);
# 
#   print("--------------------------------")
#   print("summary cqi_id2str, cqi_id2freq");
#   print("--------------------------------")
# 
#   print(data.frame(str_pos, pos_id, freq_pos, rev(freq_pos), str, id_pos));
# 
#   print("--------------------------------")
#   print("cqi_cpos2id");
#   print("--------------------------------")
# 
#   cpos_str_word <- cqi_cpos2str("DESCARTES.word", 0:9, con);
#   print(length(cpos_str_word));
# 
#   cpos_str_pos <- cqi_cpos2str("DESCARTES.pos", 0:9, con);
#   print(length(cpos_str_pos));
# 
#   print("--------------------------------")
#   print("cqi_cpos2id");
#   print("--------------------------------")
# 
#   cpos_pos_id <- cqi_cpos2id ("DESCARTES.pos", 0:9, con);
#   print(cpos_pos_id);
#   print(length(cpos_pos_id));
# 
#   str_pos <- cqi_id2str("DESCARTES.pos", cpos_pos_id, con); # déjà ci-dessus
#   print(str_pos);
#   print(length(str_pos));
# 
#   m <- matrix(c(cpos_pos_id, str_pos, cpos_str_pos), nrow=3, ncol=10, byrow=TRUE, dimnames=list(c("POS id", "POS string", "cpos_str_pos"), cpos_str_word));
#   print(m);
# 
#   # structural attributes
# 
#   # c'est l'id de cette structure (à la combien-t-ième phrase / paragraphe, etc. appartiennent les cpos ?)
#   cpos2struct_p <- cqi_cpos2struc("DESCARTES.s", c(1,2,3,150,151,152), con);
#   print(cpos2struct_p);
# 
#   # c'est le cpos de la phrases/para ouvrant précédent
#   cpos2lbound_p <- cqi_cpos2lbound("DESCARTES.s", c(1,2,3,150,151,152), con);
#   print(cpos2lbound_p);
# 
#   # c'est le cpos de la phrases/para fermant suivant
#   cpos2rbound_p <- cqi_cpos2rbound("DESCARTES.s", c(1,2,3,150,151,152), con);
#   print(cpos2rbound_p);
# 
#   print ("---------------------------------")
#   print("cqi_struc2str");
#   print ("---------------------------------")
#   # texte de str attr ?
# 
#   print("cqi_struc2str : s");
#   print(cqi_struc2str("DESCARTES.s", 1:3, con));
#   print("cqi_struc2str : s_id");
#   print(cqi_struc2str("DESCARTES.s_id", 1:3, con));
# 
#   print ("---------------------------------")
#   print("cqi_cpos2alg ????");
#   print ("---------------------------------")
# 
#   #f <- cqi_cpos2alg("DESCARTES.s_id", 0:9, con);
#   #print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
#   #f <- cqi_cpos2alg("DESCARTES.s", 0:9, con);
#   #print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
#   #f <- cqi_cpos2alg("DESCARTES.word", 0:9, con);
#   #print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
# 
#   print ("---------------------------------")
#   print("cqi_id2cpos");
#   print ("---------------------------------")
#   #f <- cqi_id2cpos("DESCARTES.s", 0:9, con);
#   #print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
#   #f <- cqi_id2cpos("DESCARTES.s_id", 0:9, con);
#   #print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
#   f <- cqi_id2cpos("DESCARTES.word", 1, con);
#   print(f);
#   # -> CL_ERROR_WRONG_ATTRIBUTE_TYPE
# 
#   print ("---------------------------------")
#   print("struc2cpos");
#   print ("---------------------------------")
#   f <- cqi_struc2cpos("DESCARTES.s", 1, con);
#   print(f);
# 
# 
#   #status <- cqi_query(corpus, subcorpus, query, con);
#   #size = cqi_subcorpus_size(paste(corpus, subcorpus, sep=":"), con);
#   #match <- cqi_dump_subcorpus(paste(corpus, subcorpus, sep=":"), 'match', 0, size-1, con);
# 
#   print ("---------------------------------")
#   print("Query");
#   print ("---------------------------------")
# 
#   status <- cqi_query("DESCARTES", "Foo", "\"le\";", con);
#   if (status != STATUS_OK) {
#     print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
#     next;
#   }
#   print("ok");
# 
#   status <- cqi_query("DESCARTES:Foo", "Foo_deux", "\"le\";", con);
#   if (status != STATUS_OK) {
#     print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
#     next;
#   }
#   print("ok");
# 
#   print ("---------------------------------")
#   print("Subcorpus");
#   print ("---------------------------------")
# 
# # marche pas :
# #  freqs <- cqi_id2freq("Foo.word", 1:4, con);
# #  print(freqs);
#   
#   
# 
#   cqi_bye(con);
# }

