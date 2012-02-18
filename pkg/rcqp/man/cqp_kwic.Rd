\name{cqp_kwic}
\alias{cqp_kwic}
\alias{cqp_kwic.cqp_subcorpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a "keyword in context" list with a subcorpus.
}
\description{
Create a "keyword in context" (aka \emph{kwic}) list with a subcorpus, with parameters for
arranging and filtering items. The list can then be printed chunk by chunk.
}
\usage{
cqp_kwic(x, ...)

 \method{cqp_kwic}{cqp_subcorpus}(x, right.context = 5, left.context = 5, sort.anchor = "match", sort.anchor.attribute = "word", sort.anchor.offset = 1, ...)
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
The number of tokens to select on the left.
}
  \item{sort.anchor}{
The anchor (\emph{match, matchend, target, keyword}) to be used as sorting key.
}
  \item{sort.anchor.attribute}{
The attribute (such as \emph{word}, \emph{lemma}\dots according to available attribute in a given corpus) for sorting the lines of the concordance.
}
  \item{sort.anchor.offset}{
Sort on the selected anchor, or \emph{n} tokens before or after it.
}
  \item{...}{
Ignored.
}

}

\value{
Create a \code{cqp_kwic} object ready to be printed (with more options for the display) with \code{print} (see \code{\link{print.cqp_kwic}}).
}

\references{                                                                    
  \url{http://cwb.sourceforge.net/documentation.php}                            
} 
\author{                                                                        
        Bernard Desgraupes - \email{bernard.desgraupes@u-paris10.fr} - University Paris-10.\cr
        Sylvain Loiseau - \email{sylvain.loiseau@univ-paris13.fr} - University Paris-13.\cr
}

\seealso{
\code{\link{corpus}}, 
\code{\link{subcorpus}}, 
\code{\link{print.cqp_kwic}}
\code{\link{write.cqp_corpus}}.
}