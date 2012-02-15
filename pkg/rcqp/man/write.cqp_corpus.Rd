\name{write.cqp_corpus}
\alias{write.cqp_corpus}

\title{
Save a CWB corpus in a file in a tabular representation
}
\description{
Save a CWB corpus in a file as a data frame where each column represent an attribute (positional or structural).
}
\usage{
write.cqp_corpus(corpus, filename, from = 0, to = 1000, ...)
}

\arguments{
  \item{corpus}{
An object created with \code{\link{corpus}}.
}
  \item{filename}{
The file to be used
}
  \item{from}{
First token to save (token cpos of the first line of the dataframe).
}
  \item{to}{
Last token to save (token cpos of the last line of the dataframe).
}
  \item{\dots}{
Not used.
}
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

