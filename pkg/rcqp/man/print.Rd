\name{print.cqp_flist}
\alias{print.cqp_flist}
\alias{print.cqp_subcorpus}
\alias{print.cqp_corpus}
\alias{print.kwic}
\alias{print.kwic}

\title{
Print a CWB object
}
\description{
Print a CWB object in a exhaustive way. An flist is printed just as an
integer vector; a corpus is printed as a data.frame where row represent tokens
and columns represent attributes (positional as well as structural); a
subcorpus is printed as a kwic list (or concordancier); a kwic object is
printed... as expected.
}
\usage{
## S3 method for class 'cqp_flist'
print(x)

## S3 method for class 'cqp_corpus'
print(x, from = 0, to = 20)

## S3 method for class 'cqp_subcorpus'
print(x, positional.attribute = "word", from = 0, to = 10)

## S3 method for class 'kwic'
print(x, print.function = function(x) cqi_cpos2str(paste(attr(kwic, "parent.cqp_corpus.name"), "word", sep = "."), x), from = 0, to = 20, left.separator = " <<", right.separator = ">> ", hit.char = 15, left.char = 40, right.char = 40)
}

\arguments{
  \item{x}{
An rcqp object, created with \code{\link{flist}}, \code{\link{corpus}}, \code{\link{subcorpus}} or \code{\link{kwic}}.
}
  \item{from}{
Select tokens (for a corpus object) or matches (for a subcorpus or kwic object) to be printed. As for all CWB data, this index is 0-based.
}
  \item{to}{
Select tokens (for a corpus object) or matches (for a subcorpus or kwic object) to be printed. As for all CWB data, this index is 0-based.
}
  \item{positional.attribute}{
Print each token of the subcorpus using the given positional attribute. 
}
  \item{print.function}{
A function allowing to fine access to the way token are displayed in a concordancier.
}
  \item{left.separator}{
String insterted at the left of the keyword in concordancier.
}
  \item{right.separator}{
String insterted at the right of the keyword in concordancier.
}
  \item{hit.char}{
Size (in characters) of the column containing the keyword.
}
  \item{left.char}{
Size (in characters) of the column containing the left context.
}
  \item{right.char}{
Size (in characters) of the column containing the right context.
}

}

\details{

Print on a subcorpus object is a shortcut for :\cr
\code{k <- kwic(subcorpus)} \cr
\code{print(k)} \cr
Creating a kwic object give more control on the printing options.
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
c <- corpus("DICKENS");
c;

sc <- subcorpus(c, '"interesting"');
sc;

k <- kwic(sc);
k;

fl <- flist(c, "word");
fl;

}
}
