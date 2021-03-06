\name{print.cqp_flist}
\alias{print,cqp_flist-method}
\alias{print,cqp_corpus-method}
\alias{print,cqp_subcorpus-method}
\alias{print,cqp_kwic-method}
\alias{print,cqp_attr-method}

\title{
Print a CWB object
}
\description{
Print a CWB object in an exhaustive way. A \code{cqp_flist} object is printed just as an
integer vector; a corpus is printed as a dataframe where rows represent tokens
and columns represent attributes (positional as well as structural); a
subcorpus is printed as a \code{cqp_kwic} list (or concordance); a \code{cqp_kwic} object is
printed... as expected.
}
\usage{

	\S4method{print}{cqp_flist}(x)

	\S4method{print}{cqp_corpus}(x, from = 0, to = 20, use_value = TRUE, ...)

	\S4method{print}{cqp_subcorpus}(x, positional.attribute="word", from=0, to=10, ...)

	\S4method{print}{cqp_kwic}(x, 
	from=0,
	to=min(20, nrow(x)-1),
	print_tokens=function(x, cpos) cqi_cpos2str(paste(x@corpus, "word", sep="."), cpos),
	left.separator=" <<",
	right.separator=">> ",
	...)

	\S4method{print}{cqp_attr}(x, ...)

}

\arguments{
  \item{x}{
An \code{rcqp} object, created with \code{\link{cqp_flist}}, \code{\link{corpus}}, \code{\link{subcorpus}}, \code{\link{cqp_kwic}}, or \code{\link{$,cqp_corpus-method}}.
}
  \item{from}{
Select first tokens (for a corpus object) or matches (for a subcorpus or cqp_kwic object) to be printed. As for all CWB data, this index is 0-based.
}
  \item{to}{
Select last tokens (for a corpus object) or matches (for a subcorpus or cqp_kwic object) to be printed. As for all CWB data, this index is 0-based.
}
  \item{use_value}{
use value or id for a structural attribute with values.
}
  \item{positional.attribute}{
Print each token of the subcorpus using the given positional attribute. 
}
  \item{print_tokens}{
A function allowing a fine access to the way tokens are displayed in a concordance.
}
  \item{left.separator}{
String inserted at the left of the keyword in a concordance.
}
  \item{right.separator}{
String inserted at the right of the keyword in a concordance.
}
  \item{...}{
Ignored.
}

}

\details{

The \code{print} command applied on a subcorpus object is a shortcut for :
\preformatted{
k <- cqp_kwic(subcorpus)
print(k)
}

Creating a \code{cqp_kwic} object gives more control on the printing options.
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{corpus}}, 
\code{\link{subcorpus}}, 
\code{\link{cqp_kwic}}, 
\code{\link{cqp_flist}}, 
\code{\link{cqp_ftable}}, 
\code{\link{summary,cqp_flist-method}}, 
\code{\link{size,cqp_corpus-method}}, 
\code{\link{size,cqp_subcorpus-method}}, 
\code{\link{summary,cqp_corpus-method}}, 
\code{\link{summary,cqp_subcorpus-method}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
c;

sc <- subcorpus(c, '"interesting"');
sc;

k <- cqp_kwic(sc);
k;

fl <- cqp_flist(c, "word");
fl;

}
}
