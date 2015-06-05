\name{cqp_ftable}
\alias{cqp_ftable}
\alias{cqp_ftable,cqp_corpus-method}
\alias{cqp_ftable,cqp_subcorpus-method}

\title{
Create a frequency table
}

\description{
Create a frequency table either with a corpus or with a subcorpus. With a corpus, a
frequency table is a based on two attributes (both may be structural or positional).
With a subcorpus object, a frequency table is based on two anchors
(\emph{match}, \emph{matchend}, \emph{target}, \emph{keyword})
and a positional attribute for each anchor.
}

\usage{
cqp_ftable(x, ...)

	\S4method{cqp_ftable}{cqp_corpus}(x, attribute1, attribute2, attribute1.use.id = FALSE, attribute2.use.id = FALSE, structural.attribute.unique.id = FALSE, ...)
 
	\S4method{cqp_ftable}{cqp_subcorpus}(x, anchor1, attribute1, anchor2, attribute2, cutoff = 0, ...)
}

\arguments{
  \item{x}{
An rcqp object, created with \code{\link{corpus}} or \code{\link{subcorpus}}.
}
  \item{attribute1}{
The attribute for the modalities of the first variable of the cross-tabulation. If \code{x} is a subcorpus, positional attribute only.
}
  \item{attribute2}{
The attribute for the modalities of the second variable of the cross-tabulation. If \code{x} is a subcorpus, positional attribute only.
}
  \item{attribute1.use.id}{
If \code{attribute1} is a structural attribute and has values (see \code{\link{cqi_structural_attribute_has_values}}), switch between region ids (struc) and values (default).
}
  \item{attribute2.use.id}{
If attribute2 is a structural attribute and has values (see \code{\link{cqi_structural_attribute_has_values}}), switch between region ids (struc) and values (default).
}
  \item{structural.attribute.unique.id}{
Count tokens or ids. See details for more info.
}

  \item{anchor1}{
The anchor for individuals of the first variable, if \code{x} is a subcorpus (anchor may be : \emph{match, matchend, target, keyword}).
}
  \item{anchor2}{
The anchor for individuals of the second variable, if \code{x} is a subcorpus (anchor may be : \emph{match, matchend, target, keyword}).
}
  \item{cutoff}{
Filter the frequency table.
}
  \item{...}{
Ignored.
}

}
\details{
%Some explanations for the \code{structural.attribute.unique.id}
%option (see the vignette \emph{RcqpIntroduction}).

\bold{For corpus:}

Counts are made on token basis, i.e. each token of the corpus is an
individual on which the two modalities (attributes) are considered.

Consider the following toy corpus with 4 tokens, two positional attributes and three
structural attributes, the last two having values.

\preformatted{
corpus :
  attr1  attr2  attr3 attr4  attr4 (value) attr5 attr5 (value)
1 word1  lemma1     1     1          text1     1         title
2 word2  lemma1     2     1          text1     2          body
3 word3  lemma2     2     2          text2     3         title
4 word4  lemma2     2     2          text2     4          body
}

Example with a positional attribute and a structural attribute :

\preformatted{
cqp_ftable(corpus, attribute1="attr2", attribute2="attr4")
}

Result :

\preformatted{
       1  2
lemma1 2  0
lemma2 0  2
}

Example with two positional attributes (a kind of grouping) :

\preformatted{
cqp_ftable(corpus, attribute1="attr1", attribute2="attr2")
}

Result :

\preformatted{
      lemma1 lemma2
word1      1      0
word2      1      0
word3      0      1
word4      0      1
}

If you use two structural attributes as arguments in \code{cqp_ftable},
the count give the number of tokens at the intersection of the regions:

\preformatted{
cqp_ftable(corpus, attribute1="attr3", attribute2="attr4")
}

Result :

\preformatted{
      1  2
1     1  0
2     1  4
}

Structural attributes may have values. With the option "attribute1.use.id"
(or attribute2.use.id) you may ask for grouping the occurrence on value rather
than on region id. It may entail a different
counting, since these values are not unique: occurrences of phenomena belonging
to different region are then counted together if two regions have the same
value. These options
have effect only if the corresponding attribute (attribute1 or attribute2) is
a structural attribute with values. In the following the frequencies of the different word forms (attr1)
are counted according to if their are in a region of value "title" or "body" :

\preformatted{
cqp_ftable(corpus, attribute1="attr1", attribute2="attr5", attribute2.use.id=TRUE)
}

Result :

\preformatted{
       title   body
word1      1      0
word2      0      1
word3      1      0
word4      0      1
}

If you use two structural attributes that both have values, you may want to count the number of
times the modalities are cooccurring, rather than the total number of
tokens included in these cooccurrences. For that purpose, you can use the
\code{structural.attribute.unique.id=TRUE} option. In the following
example, we count the number of time each head appears in each novel.

Consider the following corpus :

\preformatted{
  attr1  attr2  attr3 attr4  attr4 (value) attr5 attr5 (value)
1 word1  lemma1     1     1          text1     1         title
2 word2  lemma1     2     1          text1     2          body
3 word2b lemma1     2     1          text1     2          body
4 word3  lemma2     2     2          text2     3         title
5 word4  lemma2     2     2          text2     4          body
6 word4b lemma2     2     2          text2     4          body
7 word3  lemma2     2     2          text2     5         title
8 word4  lemma2     2     2          text2     6          body
9 word4b lemma2     2     2          text2     6          body
}

without "structural.attribute.unique.id=TRUE", the value give the number of tokens:

\preformatted{
cqp_ftable(corpus, attribute1="attr4", attribute2="attr5",
attribute1.use.id=TRUE, attribute2.use.id=TRUE)
}

Result :

\preformatted{
       title   body
text1      1      2
text2      2      4
}

With "structural.attribute.unique.id=TRUE", you count the number of time each couple of
modality is counted for different region (ids) :

\preformatted{
cqp_ftable(corpus, attribute1="attr4", attribute2="attr5",
attribute1.use.id=TRUE, attribute2.use.id=TRUE, structural.attribute.unique.id=TRUE)
}

Result :

\preformatted{
       title   body
text1      1      1
text2      2      2
}

\bold{For subcorpus:}

Cross-counting of two modalities on two list of tokens. 

}

\value{
A frequency table stored as a flat (3-column) dataframe : for each observed
combination of modalities, the first column gives the modality in the first
variable, the second column the modality in the second variable, and the third
column the observed frequency of the cooccurrence.
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
\code{\link{cqp_kwic}},
\code{\link{subcorpus}}.
}

