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

tt <- tktoplevel();

### The registry
Name <- tclVar("-r /Users/sylvainloiseau/TXM/registry");
entry.Name <-tkentry(tt,width="40",textvariable=Name);

### The list of the corpora
tl<-tklistbox(tt,height=10,selectmode="single",background="white");

### Connect to cqp server
on.connect <- function () {
  con <<- get_cwb(server_options=tclvalue(Name));
  corpora <- cqi_list_corpora(con);

  for (i in 1:length(corpora)) {
    tkinsert(tl,"end",corpora[i]);
  }
}
connect.tkbutton <- ttkbutton(
    tt,
    text="connect to CQP serveur",
    command=on.connect
    );


### quit app
quit.button <- ttkbutton(tt, text="quit",
                          command=function()tkdestroy(tt))

tkpack(entry.Name, connect.tkbutton, tl, quit.button);


