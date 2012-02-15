\name{summary.cqp_flist}
\alias{summary.cqp_flist}
\alias{summary.cqp_corpus}
\alias{summary.cqp_subcorpus}

\title{
Print summary for CWB objects.
}
\description{
Print summary for corpus, subcorpus, or frequency list.
}
\usage{

 \method{summary}{cqp_flist}(object, ...)

 \method{summary}{cqp_corpus}(object, ...)

 \method{summary}{cqp_subcorpus}(object, ...)
}

\arguments{
  \item{object}{
An rcqp object, created with \code{\link{flist}}, \code{\link{corpus}}, \code{\link{subcorpus}}.
}
  \item{...}{
Ignored.
}

}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\examples{
\dontrun{
c <- corpus("DICKENS");
c;

sc <- subcorpus(c, '"interesting"');
sc;

k <- kwic(sc);
k;

fl <- flist(c, "word");
fl;
}
}
