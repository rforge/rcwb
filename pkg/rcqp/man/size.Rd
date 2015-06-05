\name{size}
\alias{size}
\alias{size,cqp_corpus-method}
\alias{size,cqp_subcorpus-method}
\title{
Size of a corpus or subcorpus
}
\description{
Size of a corpus (number of tokens) or size of a subcorpus (number of matches).
}
\usage{
size(corpus)

\S4method{size}{cqp_corpus}(corpus)

\S4method{size}{cqp_subcorpus}(corpus)

}
\arguments{
  \item{corpus}{
 A \code{cqp_corpus} object (created with the function \code{\link{corpus}})
 or a \code{cqp_subcorpus} object (created with the function \code{\link{subcorpus}})
 
}
}
\value{
An integer vector of length 1: the number of tokens if \code{x} is a corpus
or the number of matches if it is a subcorpus.
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
\code{\link{summary,cqp_corpus-method}}, 
\code{\link{summary,cqp_subcorpus-method}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
size(c);

sc <- subcorpus(c, '"interesting"');
size(sc);
}
}

