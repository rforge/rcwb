\name{subcorpus}
\alias{subcorpus}

\title{
Create a subcorpus.
}
\description{
Create a subcorpus holded in the CWB library.
}
\usage{
subcorpus(corpus, query)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
A rcqp object created with \code{\link{corpus}}.
}
  \item{query}{
A CQP query.
}
}
\details{
A subcorpus create a subcorpus name for the CWB library.
}
\value{
A subcorpus object.
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{corpus}}, \code{\link{flist}}, \code{\link{ftable}}.
}
\examples{
\dontrun{
c <- corpus("DICKENS");
sc <- subcorpus("DICKENS", "interesting");
}
}
