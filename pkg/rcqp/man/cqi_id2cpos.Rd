\name{cqi_id2cpos}
\alias{cqi_id2cpos}
\alias{cqi_id2freq}
\alias{cqi_id2str}
\alias{cqi_str2id}

\title{Convert the ID of an item}

\description{
	Associate an item's ID to other parameters.
}

\usage{
	cqi_id2cpos(attribute, id)
	cqi_id2freq(attribute, ids)
	cqi_id2str(attribute, ids)
	cqi_str2id(attribute, strs)
}

\arguments{
	\item{attribute}{(string) the qualified name of an attribute.}
	\item{id}{(integer) a single value}
	\item{ids}{(integer vector) }
	\item{strs}{(character vector) }
}

\details{
TODO...
}

\value{
TODO...
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
\code{\link{cqi_cpos2alg}},
\code{\link{cqi_cpos2id}},
\code{\link{cqi_cpos2lbound}},
\code{\link{cqi_cpos2rbound}},
\code{\link{cqi_cpos2str}},
\code{\link{cqi_cpos2struc}},
\code{\link{cqi_regex2id}},
\code{\link{cqi_str2id}},
\code{\link{cqi_struc2cpos}},
\code{\link{cqi_struc2str}}
}

\examples{
# TODO...
}

\keyword{corpus position}

