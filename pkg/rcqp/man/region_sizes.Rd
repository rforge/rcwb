\name{region_sizes}
\alias{region_sizes}

\title{
Size of regions
}

\description{
Compute the size (in number of tokens) of the regions of a structural attribute.
}

\usage{
region_sizes(corpus, structural_attribute)
}

\arguments{
  \item{corpus}{
A object created with \code{\link{corpus}}.
}
  \item{structural_attribute}{
A structural attribute existing in this corpus.
}
}

\value{
An integer vector.
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 

\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{flist}}, 
\code{\link{ftable}}, 
\code{\link{kwic}}, 
\code{\link{corpus}}.
}

\examples{
\dontrun{
c <- corpus("DICKENS");
s <- region_sizes(c, "chapter");
}
}
