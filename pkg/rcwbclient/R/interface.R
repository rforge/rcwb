#
# Sylvain Loiseau
# sylvain.loiseau@univ-paris13.fr
#
# This code is in the public domain

source("client.R");
source("constantes.R");
source("server.R");
source("AbstractAttribute.R");
source("PositionalAttr.R");
source("StructuralAttr.R");
source("AbstractCorpus.R");
source("Corpus.R");
source("Subcorpus.R");
source("AbstractCorpus.R");
source("quantitative.R");

library(bitops);
library(tcltk);

registry_url <- ""
con <- "";
corpora <- character(0);

### Functions
### Connect to cqp server

on.connect <- function () {
  con <<- get_cwb(server_options=tclvalue(Name));
  corpora <<- cqi_list_corpora(con);

  for (i in 1:length(corpora)) {
    tkinsert(corporaListBox,"end",corpora[i]);
  }
}

update.corpus.desc <- function () {
  corpus.name <- corpora[as.numeric(tkcurselection(corporaListBox))+1];
  print(corpus.name);
  if (length(corpus.name) == 0) return();
  tkdelete(corpus.desc.text, "1.0", "end");

  corpus.info <- get.corpus.desc(con, corpus.name);
  corpus.info.title <- names(corpus.info);
  for (i in 1:length(corpus.info)) {
    if (length(corpus.info[[i]]) > 0) {
      tkinsert(corpus.desc.text,"end",paste(corpus.info.title[i], ": ", corpus.info[i], "\n"));
    }
  }
}

get.corpus.desc <- function(con, corpus) {
  corpus.full.name <- cqi_full_name(corpus, con);
  corpus.size <- cqi_attribute_size(paste(corpus, "word", sep="."), con);

  corpus.positional.attributes <- cqi_attributes(corpus, "p", con);
#  print(corpus.positional.attributes);
  corpus.structural.attributes <- cqi_attributes(corpus, "s", con);
#  print(corpus.structural.attributes);
  #corpus.alignment.attributes <- cqi_attributes(corpus, "a", con);
  #print(corpus.alignment.attributes);

#  corpus.property <- cqi_properties(corpus, con);
#  corpus.charset <- cqi_charset(corpus, con);

  return(list(
	"Full name"=corpus.full.name,
	"Size"=corpus.size,
	"Positional attributes"=paste(corpus.positional.attributes, collapse=" "),
	"Structural attributes"=paste(corpus.structural.attributes, collapse=" ")
#,
#	"Alignement attributes"=paste(corpus.alignment.attributes, collapse=" ")
	));
}

call.query.window <- function() {
  corpus.name <- corpora[as.numeric(tkcurselection(corporaListBox))+1];
  print(corpus.name);
  if (length(corpus.name) == 0) return();
  query.window(corpus.name);
}

###
### WIDGETS
###

tt <- tktoplevel();
tktitle(tt) <- "CWB corpus";

### The registry
registry.frame <- tkframe(tt)
Name <- tclVar("-r /Users/sylvainloiseau/TXM/registry");
registryLabel <- tklabel(registry.frame, text="Registry: ");
registryTextField <- tkentry(registry.frame, width="40",textvariable=Name);
connect.tkbutton <- ttkbutton(registry.frame, text="connect to CQP serveur",
    command=on.connect
    );

### The list of the corpora and the corpus description field
corpora.frame <- tkframe(tt)
corporaListBoxScrollBar <- tkscrollbar(corpora.frame, repeatinterval=5, command=function(...)tkyview(corporaListBox,...));
corporaListBox <- tklistbox(corpora.frame, height=1, selectmode="single",yscrollcommand=function(...)tkset(corporaListBoxScrollBar,...),background="white");
# Description of corpus
corpus.desc.text <- tktext(corpora.frame, bg="white",font="courier");
tkbind(corporaListBox, "<Button-1>",update.corpus.desc)

### Global button frame
button.frame <- tkframe(tt)
quit.button <- ttkbutton(button.frame, text="quit", command=function()tkdestroy(tt))
query.button <- ttkbutton(button.frame, text="query", command=function()call.query.window())




#tkconfigure(txt, state="disabled")
#tkinsert(txt,"end","Hello, world!")
#tkfocus(txt)



### Arranging on the window
#tkgrid(registryLabel, registryTextField, sticky="w");
#tkgrid.configure(registryLabel, columnspan=2);
#tkgrid.configure(registryTextField, columnspan=2);
#tkgrid(corporaListBox, corporaListBoxScrollBar, corpus.desc.text);
#tkgrid(connect.tkbutton, quit.button);
#tkgrid.configure(corporaListBox,sticky="ns")
#tkgrid.configure(corporaListBoxScrollBar,sticky="nsw")


# pack inside the registry.frame
tkpack(registryLabel, side="left");
tkpack(registryTextField, side="left");
tkpack(connect.tkbutton, side="left");

# pack inside the corpora.frame
tkpack(corporaListBox, side="left", fill="y");
tkpack(corporaListBoxScrollBar, side="left", fill="y")
tkpack(corpus.desc.text, side="left", fill="both", expand="true");

# pack inside the button.frame
tkpack(quit.button, query.button, side="right");

tkpack(registry.frame)
tkpack(corpora.frame, fill="both", expand="true");
tkpack(button.frame, side="right");

tkfocus(tt);

query <- function (query.string, corpus) {
  subcorpusname = "Last";
  status <- cqi_query(corpus, subcorpusname, query.string, con);
  if (status != STATUS_OK) {
    print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
  }

  print("before cqi_subcorpus_size");
  size = cqi_subcorpus_size(paste(corpus, subcorpusname, sep=":"), con);

  resultmatrix <- matrix("", 0, 0);
  if (size > 0) {
    resultmatrix <- matrix("", nrow=size, ncol=3);
    match <- cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'match', 0, size-1, con);
    matchend <- cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'matchend', 0, size-1, con);
    for (i in 1:length(match)) {
      lb <- match[i] - 5;
      rb <- matchend[i] + 5;
      lc = cqi_cpos2str(paste(corpus, "word", sep="."), lb:(match[i]-1), con);
      mat = cqi_cpos2str(paste(corpus, "word", sep="."), match[i]:matchend[i], con);
      rc = cqi_cpos2str(paste(corpus, "word", sep="."), (matchend[i]+1):rb, con);

      resultmatrix[i, 1] <- paste(lc, collapse=" ");
      resultmatrix[i, 2] <- paste(mat, collapse=" ");
      resultmatrix[i, 3] <- paste(rc, collapse=" ");
    }
  }
  cqi_drop_subcorpus(paste(corpus, subcorpusname, sep=":"), con);
  return(resultmatrix);
}

display.kwic <- function(kwic.matrix, kwic.frame, title="") {

  print("before building tclArray");
  tclArray <- tclArray()
    for (i in (1:nrow(kwic.matrix)))
      for (j in (1:ncol(kwic.matrix)))
	tclArray[[i-1,j-1]] <- kwic.matrix[i,j];
  print("after building tclArray");

  nr<-nrow(kwic.matrix);
  nc<-ncol(kwic.matrix);

  tclRequire("Tktable")
  print("before building table");
  table.kwic <- tkwidget(kwic.frame,"table",rows=nr,cols=nc,titlerows=0,titlecols=0,
                     height=3,width=5,
                     xscrollcommand=function(...) tkset(xscr,...))
# ,yscrollcommand=function(...) tkset(yscr,...)
  print("before building xscr");
  xscr <-tkscrollbar(kwic.frame,orient="horizontal", command=function(...)tkxview(table.kwic,...))
  #yscr <- tkscrollbar(tt,command=function(...)tkyview(table.kwic,...))

  print("before tkpack");
  tkconfigure(table.kwic,variable=tclArray,background="white", resizeborders="col")
  tkpack(table.kwic, side="top", fill="both", expand=TRUE);
  tkpack(xscr);
  #tkgrid(table.kwic,yscr)
  #tkgrid.configure(yscr,sticky="nsw")
  #tkgrid(xscr,sticky="new")
}

################

query.window <- function (corpus) {

  tk.query.window <- tktoplevel();
  tktitle(tk.query.window) <- paste("Query corpus", corpus);

  # kwic frame

  kwic.frame <- tkframe(tk.query.window);

  # number of hit frame

  hit.nbr.frame <- tkframe(tk.query.window);
  hitLabel <- tklabel(hit.nbr.frame , text="Number of match: ");
  hitnbrLabel <- tklabel(hit.nbr.frame , text="");

  # query.frame

  query.frame <- tkframe(tk.query.window);
  Name <- tclVar("");
  queryLabel <- tklabel(query.frame, text="Query: ");
  queryTextField <- tkentry(query.frame, width="40",textvariable=Name);
  query.tkbutton <- ttkbutton(query.frame, text="Query", command= function() {
      query.str <- tclvalue(Name);
      print(query.str);
      result.matrix <- query(query.str, corpus);
      print(dim(result.matrix));
      display.kwic(result.matrix, kwic.frame); 

      #tktext(hitnbrLabel) <- as.character(nrow(result.matrix));
});

# pack inside the query.frame

tkpack(queryLabel, side="left");
tkpack(queryTextField, side="left");
tkpack(query.tkbutton, side="left");

# pack inside the hit.frame

tkpack(hitLabel, side="left");
tkpack(hitnbrLabel);

# pack frames

tkpack(query.frame);
tkpack(hit.nbr.frame);
tkpack(kwic.frame, expand=TRUE, fill="both");

tkfocus(tk.query.window);

}
