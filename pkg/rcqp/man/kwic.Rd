\name{kwic}
\alias{kwic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a "keyword in context" list with a subcorpus.
}
\description{
Create a "keyword in context" list with a subcorpus, with parameters for
arranging and filtering items. The list can then be printed chunk by chunk.
}
\usage{
kwic(x, ...)

## S3 method for class 'cqp_subcorpus'
kwic(subcorpus, right.context = 5, left.context = 5, sort.anchor = "matchend", sort.anchor.attribute = "word", sort.anchor.offset = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object created with \code{\link{subcorpus}}.
}
  \item{right.context}{
The number of tokens to select on the right.
}
  \item{left.context}{
The number of tokens to select on the right.
}
  \item{sort.anchor}{
The anchor (match, matchend, target, keyword) to be used as keyword.
}
  \item{sort.anchor.attribute}{
The attribute (such as word, lemma... according to available attribute in a given corpus) for sorting the lines of the concordancer.
}
  \item{sort.anchor.offset}{
Sort on the anchor selected, or $n$ tokens before or after?
}
}

\value{
Create a kwic object ready to be printed (with more options for the display) with print (see \code{\link{print.kwic}}).
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{print.kwic}}
}

\examples{
}
