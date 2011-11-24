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
library(tcltk);

registry_url <- ""
con <- "";
corpora <- character(0);

tt <- tktoplevel();

### The registry
Name <- tclVar("-r /Users/sylvainloiseau/TXM/registry");
registryLabel <- tklabel(tt,text="Registry: ");
registryTextField <- tkentry(tt,width="40",textvariable=Name);

### The list of the corpora
corporaListBoxScrollBar <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(corporaListBox,...));
corporaListBox <- tklistbox(tt,height=5,selectmode="single",yscrollcommand=function(...)tkset(corporaListBoxScrollBar,...),background="white");

### Connect to cqp server
on.connect <- function () {
  con <<- get_cwb(server_options=tclvalue(Name));
  corpora <<- cqi_list_corpora(con);

  for (i in 1:length(corpora)) {
    tkinsert(corporaListBox,"end",corpora[i]);
  }
}

connect.tkbutton <- ttkbutton(
    tt,
    text="connect to CQP serveur",
    command=on.connect
    );

### quit app
quit.button <- ttkbutton(tt, text="quit", command=function()tkdestroy(tt))

### Description of corpus

corpus.desc.text <- tktext(tt,bg="white",font="courier");

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

tkbind(corporaListBox, "<Button-1>",update.corpus.desc)

#tkconfigure(txt, state="disabled")
#tkinsert(txt,"end","Hello, world!")
#tkfocus(txt)



### Arranging on the window
tkgrid(registryLabel, registryTextField, sticky="w");
tkgrid.configure(registryLabel, columnspan=2);
tkgrid.configure(registryTextField, columnspan=2);
tkgrid(corporaListBox, corporaListBoxScrollBar, corpus.desc.text);
tkgrid(connect.tkbutton, quit.button);
tkgrid.configure(corporaListBox,sticky="ns")
tkgrid.configure(corporaListBoxScrollBar,sticky="nsw")


#tkpack(param.frm, corpus.frm, button.frm);
tkfocus(tt);

get.corpus.desc <- function(con, corpus) {
  corpus.full.name <- cqi_full_name(corpus, con);
  corpus.size <- cqi_attribute_size(paste(corpus, "word", sep="."), con);

  corpus.positional.attributes <- cqi_attributes(corpus, "p", con);
  print(corpus.positional.attributes);
  corpus.structural.attributes <- cqi_attributes(corpus, "s", con);
  print(corpus.structural.attributes);
  corpus.alignment.attributes <- cqi_attributes(corpus, "a", con);
  print(corpus.alignment.attributes);

#  corpus.property <- cqi_properties(corpus, con);
#  corpus.charset <- cqi_charset(corpus, con);

  return(list(
	"Full name"=corpus.full.name,
	"Size"=corpus.size,
	"Positional attributes"=paste(corpus.positional.attributes, collapse=" "),
	"Structural attributes"=paste(corpus.structural.attributes, collapse=" "),
	"Alignement attributes"=paste(corpus.alignment.attributes, collapse=" ")
	));
}

