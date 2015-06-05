\name{subcorpus}
\alias{subcorpus}
\alias{.cqp_name,cqp_subcorpus-method}
\alias{nmatch,cqp_subcorpus-method}
\alias{part_size,cqp_subcorpus-method}
\alias{N,cqp_subcorpus-method}
\alias{as.data.frame,cqp_subcorpus-method}
\alias{as.list,cqp_subcorpus-method}

\title{
Create a subcorpus.
}

\description{
Create a subcorpus held in the CWB library.
}

\usage{
	subcorpus(corpus, query)

	\S4method{.cqp_name}{cqp_subcorpus}(x, qualified=TRUE)
	\S4method{nmatch}{cqp_subcorpus}(subcorpus)
	\S4method{part_size}{cqp_subcorpus}(subcorpus)
	\S4method{N}{cqp_subcorpus}(corpus)
	\S4method{as.data.frame}{cqp_subcorpus}(x, from=1, to=nmatch(x), use_value=TRUE)
	\S4method{as.list}{cqp_subcorpus}(x, positional=x$word, structural=x$text, from=1, to=nmatch(x))

}
%- maybe also 'usage' for other objects documented here.

\arguments{
	\item{x}{An \code{rcqp} object.}
	\item{corpus}{An \code{rcqp} object created with \code{\link{subcorpus}}.}
	\item{subcorpus}{An \code{rcqp} object created with \code{\link{subcorpus}}.}
	\item{query}{A CQP query.}
	\item{qualified}{A boolean: use qualified name.}
	\item{from}{Starting index}
	\item{to}{End index}
	\item{use_value}{A boolean: use value or id for a structural attribute with values.}
	\item{positional}{A positional attribute.}
	\item{structural}{A structural attribute.}
}

\details{
The \emph{subcorpus} function creates internally a subcorpus name for the CWB library.
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
\code{\link{corpus}}, 
\code{\link{cqp_flist}}, 
\code{\link{print,cqp_subcorpus-method}}, 
\code{\link{summary,cqp_subcorpus-method}}, 
\code{\link{cqp_ftable}}, 
\code{\link{cqp_kwic}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
sc <- subcorpus("DICKENS", "interesting");
}
}
