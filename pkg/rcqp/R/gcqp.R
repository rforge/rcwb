# ===========================================================================
# File: "gcqp.R"
#                        Created: 2012-01-22 17:31:29
#              Last modification: 2012-01-22 17:31:29
# Authors: Bernard Desgraupes <bernard.desgraupes@u-paris10.fr>
#          Sylvain Loiseau <sylvain.loiseau@univ-paris13.fr>
# (c) Copyright: 2011-2012
# All rights reserved.
# ===========================================================================

library(rcqp);
library(tcltk);
tclRequire("Tktable")

corpora <- cqi_list_corpora();

############################################################
############################################################
##
## The query window
##
############################################################
############################################################

############################################################
#
# Open a query window for a given corpus 
#
############################################################

open.query.window <- function(corpus) {
 if (length(corpus) == 0) {
   tkmessageBox(message = "Please select a corpus first", icon = "warning", type = "ok");
   return();
 }

    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, paste("Corpus: ", corpus))

    result.matrix <- matrix("", 4, 3);

## The result frame (main frame holding a table)
kwic.frame <- tkframe(dlg)

## The query frame, on the top of the window
query.frame <- tkframe(dlg)
queryVar <- tclVar("");
queryLabel <- tklabel(query.frame, text="Query: ");
queryTextField <- tkentry(query.frame, width="40",textvariable=queryVar);
query.tkbutton <- ttkbutton(query.frame, text="Search!",
    command=function(){
      query.str <- tclvalue(queryVar);
      if (length(query.str) == 0) {
      tkmessageBox(message = "Please give a query!", icon = "warning", type = "ok");
        return();
      }
      result.matrix <<- query(query.str, corpus)
      display.kwic(result.matrix, kwic.frame); 
    });

## pack the query frame
tkpack(queryLabel, side="left");
tkpack(queryTextField, side="left");
tkpack(query.tkbutton, side="left");

tkpack(query.frame)

## for the first time print an empty table

  display.kwic(result.matrix, kwic.frame, title="");
tkpack(kwic.frame, fill="both", expand="true");


## The button frame, on the bottom of the window

button.frame <- tkframe(dlg)
  quit.button <- ttkbutton(button.frame, text="quit", command=function() {
      tkgrab.release(dlg)
      tkdestroy(dlg)
      tkfocus(tt)
      })
tkpack(quit.button, side="right");
tkpack(button.frame, side="right");

tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
  tkwait.window(dlg)
}

############################################################
#
# Run a cqp query and return a character matrix containing a
# kwic (key word in context) view of the result: each match 
# is on one line, with left and right context displayed.
#
############################################################

query <- function (query.string, corpus, start <- 1, end <- 10, left.context=5, right.context=5) {

  subcorpusname = "Last";
  #status <- cqi_query(corpus, subcorpusname, query.string);
  cqi_query(corpus, subcorpusname, query.string);
  #if (status != STATUS_OK) {
  #  print(paste("failed [$CWB::CQI::CommandName{$status}]", status));
  #}

  print("before cqi_subcorpus_size");
  size = cqi_subcorpus_size(paste(corpus, subcorpusname, sep=":"));
  print(paste("cqi_subcorpus_size: ", size));

  resultmatrix <- matrix("", 0, 0);
  if (size > 0) {
    resultmatrix <- matrix("", nrow=size, ncol=3);
    #match <- cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'match', 0, size-1);
    #matchend <- cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), 'matchend', 0, size-1);


    ans <- cqi_dump_subcorpus(paste(corpus, subcorpusname, sep=":"), start, end);
    match <- ans[,1];
    matchend <- ans[,2];

    for (i in 1:length(match)) {
      lb <- match[i] - left.context;
      rb <- matchend[i] + right.context;
      lc = cqi_cpos2str(paste(corpus, "word", sep="."), lb:(match[i]-1));
      mat = cqi_cpos2str(paste(corpus, "word", sep="."), match[i]:matchend[i]);
      rc = cqi_cpos2str(paste(corpus, "word", sep="."), (matchend[i]+1):rb);

      resultmatrix[i, 1] <- paste(lc, collapse=" ");
      resultmatrix[i, 2] <- paste(mat, collapse=" ");
      resultmatrix[i, 3] <- paste(rc, collapse=" ");
    }
  }
  return(resultmatrix);
}

############################################################
#
# turn a result of query() (kwic character matrix) into a tk table
#
############################################################

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

############################################################
############################################################
##
## The main window
##
############################################################
############################################################

############################################################
#
# Information on the corpus
#
############################################################

update.corpus.desc <- function () {
  corpus.name <- corpora[as.numeric(tkcurselection(corporaListBox))+1];
  if (length(corpus.name) == 0) return();
  corpus.info <- get.corpus.desc(corpus.name);
  corpus.info.title <- names(corpus.info);
  for (i in 1:length(corpus.info)) {
    if (length(corpus.info[[i]]) > 0) {
      tkinsert(corpus.desc.text,"end",paste(corpus.info.title[i], ": ", corpus.info[i], "\n"));
    }
  }
}

get.corpus.desc <- function(corpus) {
  corpus.full.name <- cqi_full_name(corpus);
  corpus.size <- cqi_attribute_size(paste(corpus, "word", sep="."));
  corpus.positional.attributes <- cqi_attributes(corpus, "p");
  corpus.structural.attributes <- cqi_attributes(corpus, "s");
  return(list(
	"Full name"=corpus.full.name,
	"Size"=corpus.size,
	"Positional attributes"=paste(corpus.positional.attributes, collapse=" "),
	"Structural attributes"=paste(corpus.structural.attributes, collapse=" ")
	));
}

###
### WIDGETS for the main window
###

tt <- tktoplevel();
tktitle(tt) <- "CWB corpus";

### The list of the corpora and the corpus description field
corpora.frame <- tkframe(tt)
corporaListBoxScrollBar <- tkscrollbar(corpora.frame, repeatinterval=5, command=function(...)tkyview(corporaListBox,...));
corporaListBox <- tklistbox(corpora.frame, height=1, selectmode="single",yscrollcommand=function(...)tkset(corporaListBoxScrollBar,...),background="white");
  for (i in 1:length(corpora)) {
    tkinsert(corporaListBox,"end",corpora[i]);
  }
# Description of corpus
corpus.desc.text <- tktext(corpora.frame, bg="white",font="courier");
tkbind(corporaListBox, "<Button-1>",update.corpus.desc)

### Global button frame
button.frame <- tkframe(tt)
quit.button <- ttkbutton(button.frame, text="quit", command=function()tkdestroy(tt))
open.corpus.button <- ttkbutton(button.frame, text="open corpus", command=function()open.query.window(corpora[as.numeric(tkcurselection(corporaListBox))+1]))

tkpack(corporaListBox, side="left", fill="y");
tkpack(corporaListBoxScrollBar, side="left", fill="y")
tkpack(corpus.desc.text, side="left", fill="both", expand="true");

# pack inside the button.frame
tkpack(quit.button, open.corpus.button, side="right");


tkpack(corpora.frame, fill="both", expand="true");
tkpack(button.frame, side="right");

tkfocus(tt);

