\name{cqi_fdist}
\alias{cqi_fdist}

\title{Frequency distributions}

\description{
	Calculate a frequency distribution for 
}

\usage{
	cqi_fdist(subcorpus, field, key, cutoff=0, offset=0)
}

\arguments{
	\item{subcorpus}{(string) qualified name of a subcorpus.}
	\item{field}{(string) the name of the anchor. It can be one of : \emph{'match'}, \emph{'matchend'}, \emph{'target'}, \emph{'keyword'}.}
	\item{key}{(string) the name of a positional attribute.}
	\item{cutoff}{(integer) a floor value under which results are not displayed. Default value is 0.}
	\item{offset}{(integer) an offset relative to the specified anchor.}
}

\details{
	This function builds a frequency table of the occurrences of the
	different values of a positional attribute.

	The results are sorted in decreasing order of frequency. The \code{cut}
	argument specifies a value under which the results will not be
	returned. For instance, if the value is set to 10, only items with a
	frequency greater than or equal to 10 are returned. The default value
	of this argument is 0 which means that all the frequencies are returned
	by default.
	
	The \code{offset} argument lets specify a position relative to the
	anchor specified by the \code{field} argument. For instance, if
	\code{field} is set to \emph{'match'} and offset is equal to -1, the
	frequency table is computed on all the tokens located before the match
	anchor. The default value of this argument is 0.
	
}

\value{
	\code{cqi_fdist} returns a matrix with two columns. The first column
	contains the IDs of the attributes and the second column the
	corresponding frequency (number of occurrences).
}

\author{
	Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
	Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\source{
	The IMS Open Corpus Workbench (CWB) \url{http://cwb.sourceforge.net/}
}

\references{
	\url{http://cwb.sourceforge.net/documentation.php}
}

\seealso{
\code{\link{cqi_dump_subcorpus}}
}

\examples{
cqi_query("DICKENS","Go","[lemma=\"go\"] \"and\" [];")
m <- cqi_fdist("DICKENS:Go","matchend","pos")
cqi_cpos2str("DICKENS.pos", m[,1])

cqi_query("DICKENS","NP","[pos=\"DT\"] @[pos=\"JJ\"]? [pos=\"NNS?\"];")
m <- cqi_fdist("DICKENS:NP","target","lemma",300)
m <- cqi_fdist("DICKENS:NP","match","lemma", cutoff=2000, offset=-1)
}

\keyword{frequency}