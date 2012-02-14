\name{flist}
\alias{flist}
\alias{flist.cqp_corpus}
\alias{flist.cqp_subcorpus}

\title{
 Create a frequency list
}
\description{
 Create a frequency list either on any phenomenon available in a CWB corpus: frequency of forms of a positional attribute, of a value of a structural attribute, or frequency of forms found in an anchor (match, matchend, target, keyword) of a subcorpus.
}
\usage{
flist(x, ...)

## S3 method for class 'cqp_corpus'
flist(x, attribute, cutoff = 0, ...)

## S3 method for class 'cqp_subcorpus'
flist(x, anchor, attribute, left.context = 0, right.context = 0, cutoff = 0, offset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A corpus or a subcorpus object, created with the function \code{\link{corpus}} or \code{\link{subcorpus}}.
}
  \item{attribute}{
The attribute giving the modality to be counted. May be a structural attribute if x is a corpus.
}
  \item{anchor}{
Where to count the modality : on the match, matchend, target or keyword anchor of a subcorpus.
}
  \item{left.context}{
Consider also n tokens at the left of the selected anchor for counting frequencies.
}
  \item{right.context}{
Consider also n tokens at the right of the selected anchor for counting frequencies.
}
  \item{offset}{
switch the anchor at n token from the left (if negative) or to the right of the anchor given above.
}
  \item{cutoff}{
Keep only frequency above this cutoff. If cutoff is 0, keep all forms. If cutoff is between 0 and 1 (exclusive), the $ (cutoff * 100) \% $ more frequent forms are kept.
}
  \item{...}{
Ignored.
}
}
\details{

A frequency

}
\value{
Create a named numeric vector.
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
fl <- flist(c, "word");

sc <- subcorpus(c, '"interesting" "to" @ []');
fl <- flist(sc, "target", "word");

sc <- subcorpus(c, '"interesting"');
fl <- flist(sc, "target", "lemma", left.context = 10, right.context = 10, cutoff = 5)
}
}
