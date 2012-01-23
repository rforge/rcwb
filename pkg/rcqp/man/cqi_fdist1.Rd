\name{cqi_fdist1}
\alias{cqi_fdist1}
\alias{cqi_fdist2}

\title{Frequency distributions}

\description{
	Calculate a frequency distribution for 
}

\usage{
	cqi_fdist1(subcorpus, field1, key1, cutoff=0, offset=0)
	cqi_fdist2(subcorpus, field1, key1, field2, key2, cutoff=0)
}

\arguments{
	\item{subcorpus}{(string) qualified name of a subcorpus.}
	\item{field1}{(string) the name of an anchor.}
	\item{key1}{(string) the name of a positional attribute.}
	\item{field2}{(string) the name of an anchor.}
	\item{key2}{(string) the name of a positional attribute.}
	\item{cutoff}{(integer) a floor value under which results are not displayed. Default value is 0.}
	\item{offset}{(integer) an offset relative to the specified anchor.}
}

\details{
	The \code{cqi_fdist1} function builds a frequency table of the
	occurrences of the different values of a positional attribute. It is the
	frequency distribution of a token (or its annotation) at anchor
	points.
	
	The \code{cqi_fdist2} function computes a contingency table of
	frequencies of token/annotation pairs. 
	
	The values of the \code{field1} and \code{field2} arguments can be:
	\emph{'match'}, \emph{'matchend'}, \emph{'target'}, or
	\emph{'keyword'}.
	
	The results are sorted in decreasing order of frequency. The \code{cut}
	argument specifies a value under which the results will not be
	returned. For instance, if the value is set to 10, only items with a
	frequency greater than or equal to 10 are returned. The default value
	of this argument is 0 which means that all the frequencies are returned
	by default.
	
	The \code{offset} argument lets specify a position relative to the
	anchor specified by the \code{field1} arguments. For instance, if
	\code{field1} is set to \emph{'match'} and offset is equal to -1, the
	frequency table is computed on all the tokens located before the match
	anchor. The default value of this argument is 0.
	
}

\value{
	\code{cqi_fdist1} returns a matrix with two columns. The first column
	contains the IDs of the attributes of the token and the second column the
	corresponding frequency (number of occurrences).

	\code{cqi_fdist2} returns a matrix with three columns. The first two
	columns contain the IDs of the attributes of the cross-tabulated tokens,
	and the third column the corresponding frequency.

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
m <- cqi_fdist1("DICKENS:Go","matchend","pos")
cqi_id2str("DICKENS.pos", m[,1])

cqi_query("DICKENS","NP","[pos=\"DT\"] @[pos=\"JJ\"]? [pos=\"NNS?\"];")
cqi_fdist1("DICKENS:NP","target","lemma",300)
cqi_fdist1("DICKENS:NP","match","lemma", cutoff=2000, offset=-1)

cqi_fdist2("DICKENS:Go","matchend", "pos", "matchend","lemma")
cqi_fdist2("DICKENS:NP","target", "lemma", "matchend","word", cutoff=300)
}

\keyword{frequency}