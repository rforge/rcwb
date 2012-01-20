\name{cqi_attribute_size}
\alias{cqi_attribute_size}
\alias{cqi_structural_attribute_has_values}
\alias{cqi_lexicon_size}

\title{Get the size of an attribute}

\description{
	Find the number of items corresponding to a specified attribute in a
	given corpus.
}

\usage{
	cqi_attribute_size(attribute)
	cqi_structural_attribute_has_values(attribute)
	cqi_lexicon_size(attribute)
}	

\arguments{
	\item{attribute}{(string) the qualified name of an attribute.}
}

\details{
	The \code{attribute} argument is the qualified name of an attribute.
	This concerns any kind of attribute. The syntax is of the form
	\code{name_of_corpus.name_of_attribute}. For instance
	\emph{"DICKENS.lemma"} (positional attribute) or
	\emph{"DICKENS.chapter"} (structural attribute).
	
	The \code{cqi_lexicon_size} function indicates the number of possible
	values of a positional attribute. It applies only to positional
	attributes.
	
	The \code{cqi_attribute_size} function applies only to attributes which
	can have values. This can be checked with the
	\link{cqi_structural_attribute_has_values} function.

	The \code{cqi_attribute_size} function is typically invoked before the
	\link{cqi_attribute_size} function. The latter makes sense only if the
	specified attribute does have values. This concerns only the structural
	attributes. Positional attributes always have values.
	
}

\value{
	\code{cqi_attribute_size} returns an integer which is the number of
	different items in the corpus marked with the specified attribute.

	\code{cqi_structural_attribute_has_values} returns a logical value
	telling whether the specified attribute has values (\code{TRUE}) or not
	(\code{FALSE}).

	\code{cqi_lexicon_size} returns an integer value.
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
\link{cqi_attributes}
}

\examples{
cqi_structural_attribute_has_values("DICKENS.file")
cqi_structural_attribute_has_values("DICKENS.file_name")
cqi_structural_attribute_has_values("DICKENS.chapter")
cqi_structural_attribute_has_values("DICKENS.chapter_title")

cqi_lexicon_size("DICKENS.lemma")
cqi_lexicon_size("DICKENS.pos")
cqi_lexicon_size("DICKENS.word")

# Positional attribute
cqi_attribute_size("DICKENS.lemma")
cqi_attribute_size("DICKENS.pos")
cqi_attribute_size("DICKENS.word")

# Structural attributes
cqi_attribute_size("DICKENS.book")
cqi_attribute_size("DICKENS.file")
cqi_attribute_size("DICKENS.chapter")

}

\keyword{attribute}