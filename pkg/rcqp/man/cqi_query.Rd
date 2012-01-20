\name{cqi_query}
\alias{cqi_query}

\title{Query an indexed corpus}

\description{
	The \code{cqi_query} function executes a query on the specified corpus.
	The query syntax is described in the \emph{CQP Query Language Tutorial}
	(see below).
}

\usage{
	cqi_query(mother, child, query)
}

\arguments{
	\item{mother}{(string) the name of the corpus.}
	\item{child}{(string) the name given to the subcorpus containing the results of the query.}
	\item{query}{(string) the query string.}
}

\details{
	The query string must be properly quoted in order to be syntactically
	correct when it is passed to the CQP interpreter.
	
	The name of a corpus is always written in uppercase letters. The name
	of a subcorpus must start with an uppercase letter and can contain both
	uppercase and lowercase letters, underscores, dashes and digits. }

\value{
	\code{cqi_query} does not return anything. In order to manipulate the
	results one must invoke the \emph{cqi_dump_subcorpus} function.
}

\author{
	Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
	Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\source{
	The IMS Open Corpus Workbench (CWB) \url{http://cwb.sourceforge.net/}
}

\references{
\itemize{
	\item \emph{CQP Query Language Tutorial}: \url{http://cwb.sourceforge.net/files/CQP_Tutorial.pdf}

	\item \emph{Corpus Encoding Tutorial}: \url{http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf}

}
}


\seealso{
\code{\link{cqi_dump_subcorpus}}
}

\examples{
# TODO...
}

\keyword{corpus, query}


