\name{Type and token}
\alias{nregions}
\alias{regions}
\alias{ntokens}
\alias{tokens}
\alias{ntypes}
\alias{types}
\alias{nvalues}
\alias{values}
\alias{get.corpus}
\alias{ntokens,cqp_attr_positional-method}
\alias{tokens,cqp_attr_positional-method}
\alias{ntypes,cqp_attr_positional-method}
\alias{types,cqp_attr_positional-method}
\alias{nregions,cqp_attr_structural-method}
\alias{regions,cqp_attr_structural-method}
\alias{nvalues,cqp_attr_structural-method}
\alias{values,cqp_attr_structural-method}
\alias{get.corpus,cqp_attr-method}

\title{
Print values of cqp attribute
}
\description{
Get vectors of tokens or types for a cqp attribute.
}
\usage{

	ntokens(attribute)

	tokens(attribute, from, to)

	ntypes(attribute)

	types(attribute)

	nregions(attribute)

	regions(attribute, from, to)

	nvalues(attribute)

	values(attribute, from, to)
	
	get.corpus(attribute)

	\S4method{tokens}{cqp_attr_positional}(attribute, from=0, to=ntokens(attribute) - 1)
	\S4method{ntokens}{cqp_attr_positional}(attribute)
	\S4method{types}{cqp_attr_positional}(attribute)
	\S4method{ntypes}{cqp_attr_positional}(attribute)
	\S4method{regions}{cqp_attr_structural}(attribute, from=0, to=nregions(attribute)-1)
	\S4method{nregions}{cqp_attr_structural}(attribute)
	\S4method{values}{cqp_attr_structural}(attribute, from=0, to=nregions(attribute)-1)
	\S4method{nvalues}{cqp_attr_structural}(attribute)

	\S4method{get.corpus}{cqp_attr}(attribute)
}

\arguments{
  \item{attribute}{
    A \code{cqp_attr} object, created with \code{\link{$,cqp_corpus-method}}.
  }
  \item{from}{Starting index}
  \item{to}{End index}
}

\value{                                                                    
  For positional attributes :
  \itemize{
    \item ntokens is the number of tokens (== size(corpus))
    \item tokens is a character vector with the actual list of tokens
    \item ntypes is the number of different values
    \item types is a character vector with the actual different values
  }
  For structural attributes :
  \itemize{
    \item nregions is the actual number of regions (a numeric vector of length 1)
    \item tokens the region id ("struc") of each token (a numeric vector)
	\item regions (only if the structural attribute "has values") : the values found on each region (a character vector)
  }
} 

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 

\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{$,cqp_corpus-method}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
pos <- c$pos
types(pos);
ntokens(types(pos));
}
}
