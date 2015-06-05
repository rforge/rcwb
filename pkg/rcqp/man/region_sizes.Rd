\name{region_sizes}
\alias{region_sizes}
\alias{region_sizes,cqp_attr_positional-method}

\title{
Size of regions of a structural attribute.
}

\description{
Compute the size (in number of tokens) of the regions of a structural attribute.
}

\usage{
	region_sizes(attribute, from, to)
	\S4method{region_sizes}{cqp_attr_positional}(attribute, from=0, to=nregions(attribute)-1)
}

\arguments{
  \item{attribute}{
	A cqp_attr (type: structural attribute) (created for instance with \code{\link{$,cqp_corpus-method}}).
  }
  \item{from}{Starting index}
  \item{to}{End index}
}

\value{
An integer vector: for each region of this attribute, the number of tokens belonging to that region.
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 

\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{cqp_flist}}, 
\code{\link{cqp_ftable}}, 
\code{\link{cqp_kwic}}, 
\code{\link{corpus}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
s <- region_sizes(c$pos);
}
}
