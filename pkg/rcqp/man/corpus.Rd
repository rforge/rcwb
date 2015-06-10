\name{corpus}
\alias{corpus}
\alias{N}
\alias{.is_cqp_corpus,ANY-method}
\alias{.cqp_name,cqp_corpus-method}
\alias{N,cqp_corpus-method}
\alias{as.data.frame,cqp_corpus-method}
\alias{as.list,cqp_corpus-method}
\alias{subcorpus,cqp_corpus-method}

\title{
Create a corpus object
}
\description{
Create a corpus object wrapping a CWB corpus object.
}
\usage{
	corpus(name)
	N(corpus)

	\S4method{subcorpus}{cqp_corpus}(corpus, query)
	\S4method{.is_cqp_corpus}{ANY}(x)
	\S4method{.cqp_name}{cqp_corpus}(x, qualified=TRUE)
	\S4method{N}{cqp_corpus}(corpus)
	\S4method{as.data.frame}{cqp_corpus}(x, from, to, use_value=TRUE)
	\S4method{as.list}{cqp_corpus}(x, positional=x$word, structural=x$text, from=0, to=N(x)-1)
}

\arguments{
	\item{name}{
	The name of a corpus in the CWB registry (must be uppercase).
	}
	\item{x}{An \code{rcqp} object.}
	\item{corpus}{An \code{rcqp} object created with \code{\link{corpus}}.}
	\item{query}{A CQP query.}
	\item{qualified}{A boolean: use qualified name.}
	\item{from}{Starting index}
	\item{to}{End index}
	\item{use_value}{A boolean: use value or id for a structural attribute with values.}
	\item{positional}{A positional attribute.}
	\item{structural}{A structural attribute.}
}

\value{
A corpus object.
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{subcorpus}}, 
\code{\link{print,cqp_corpus-method}}, 
\code{\link{summary,cqp_corpus-method}}, 
\code{\link{cqp_kwic}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
}
}
