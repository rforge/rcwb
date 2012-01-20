\name{cqi_cpos2id}
\alias{cqi_cpos2id}
\alias{cqi_cpos2lbound}
\alias{cqi_cpos2rbound}
\alias{cqi_cpos2str}
\alias{cqi_cpos2alg}
\alias{cqi_cpos2struc}
\alias{cqi_alg2cpos}

\title{Convert a corpus position}

\description{
	Associate a corpus position to other parameters.
}

\usage{
	cqi_cpos2id(attribute, cpos)
	cqi_cpos2lbound(attribute, cpos)
	cqi_cpos2rbound(attribute, cpos)
	cqi_cpos2str(attribute, cpos)
	cqi_cpos2alg(attribute, cpos)
	cqi_cpos2struc(attribute, cpos)
	cqi_alg2cpos(attribute, alg)

}

\arguments{
	\item{attribute}{(string) the qualified name of an attribute.}
	\item{cpos}{(integer vector) a list of corpus positions}
	\item{alg}{(integer) an alignment ID}
}

\details{
details...
}

\value{
return values...
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
\code{\link{cqi_alg2cpos}}
\code{\link{cqi_id2cpos}},
\code{\link{cqi_id2freq}},
\code{\link{cqi_id2str}},
\code{\link{cqi_regex2id}},
\code{\link{cqi_str2id}},
\code{\link{cqi_struc2cpos}},
\code{\link{cqi_struc2str}}
}

\examples{
# TODO
}

\keyword{corpus positions}

