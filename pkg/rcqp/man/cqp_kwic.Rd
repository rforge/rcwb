\name{cqp_kwic}
\alias{cqp_kwic}
\alias{kwic}
\alias{kwic,cqp_subcorpus-method}
\alias{as.matrix,cqp_kwic-method}
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
kwic(subcorpus, ...)

	\S4method{kwic}{cqp_subcorpus}(subcorpus, right.context=20, left.context=20)
	\S4method{as.matrix}{cqp_kwic}(x, from=0, to=min(20, nrow(x)-1),
	print_tokens=function(x, cpos) cqi_cpos2str(paste(attr(x, "parent.cqp_corpus.name"), "word", sep="."), cpos),
	...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subcorpus}{
An object created with \code{\link{subcorpus}}.
}
  \item{x}{
An object created with \code{\link{subcorpus}}.
}
  \item{right.context}{
The number of characters on the right.
}
  \item{left.context}{
The number of characters on the left.
}
  \item{from}{Starting index}
  \item{to}{End index}
  \item{print_tokens}{Formatting function}
  \item{...}{Ignored.}

}

\value{
Create a \code{cqp_kwic} object ready to be printed (with more options for the display) with \code{print} (see \code{\link{print,cqp_kwic-method}}).
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
\code{\link{print,cqp_kwic-method}}.
}
