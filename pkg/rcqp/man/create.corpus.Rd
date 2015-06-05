\name{create.corpus}
\alias{create.corpus}

\title{                                                       
Create a CWB corpus thanks to input files                     
}                                                             
\description{                                                 
Front-end for the "cwb-encode", "cwb-makeall", "cwb-huffcode" 
and "cwb-compress-rdx" tools : create a CWB corpus using    
files (or directory containing files) in "vrt" format (mix of 
xml tags, delimiting span of tokens, and tabulated section,   
each column giving a type of information on a token.          
}
\usage{
create.corpus(corpus.name, corpus.dir, registry.dir, input.files, p.attributes, s.attributes, encoding = "utf8", input.dirs = NULL, compression.huffcode = FALSE, compression.rdx = FALSE)
}

\arguments{
  \item{corpus.name}{
  (A character vector of length 1): the CWB name of the corpus (uppercase letters, digits, '-' and '_'; first character is a letter).
}
  \item{corpus.dir}{
  (A character vector of length 1): The directory where to store index files. The directory must exist.
}
  \item{registry.dir}{
  (A character vector of length 1) : The directory where to store the registry files. The directory must exist.
}
  \item{input.files}{
  (A character vector) : the vrt files to be imported
}
  \item{p.attributes}{
  (A character vector) : the positional attributes; i.e. the names of the
columns in the vrt files. The first column is always 'word' and this name
should not appear in the vector.
}
  \item{s.attributes}{
  (A named list). Each slot is a structural attribute. The value corresponding
to the name is NULL if the element doesn't have any "XML attribute", or a
character vector of these attributes if it does have any.
}
  \item{encoding}{
Only "utf8" supported so far.
}
  \item{input.dirs}{
Directory may be listed in a character vector ; files ending with "vrt" in these directories will be used.
}
  \item{compression.huffcode}{
If the corpus has to compressed.
}
  \item{compression.rdx}{
If the corpus has to compressed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
return 0 if everything goes well.
}
\references{
 \url{http://cwb.sourceforge.net/documentation.php}
}
\author{
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\examples{
\dontrun{
  create.corpus(
      "DICKENS",
      "~/corpus/data/dickens",
      "~/corpus/registry",
      "dickens.vrt",
      c("pos", "lemma"),
      list("teiCorpus"=NULL, "TEI"=NULL, "text"=c("id"), "div1"=NULL, "div2"=c("id", "n"), "p")
      );

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
