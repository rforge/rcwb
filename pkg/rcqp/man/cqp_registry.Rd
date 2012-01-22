\name{cqp_registry}
\alias{cqp_registry}

\title{The CQP registry}

\description{
Setting the CQP registry for rcqp.
}

\details{

Corpora are declared in files located in a directory called "registry"
(see CWB documentation for more details). One of the mechanisms provided by cwb
in order to set the registry directory is to define the environment variable
"CORPUS_REGISTRY".

In rcqp, the CORPUS_REGISTRY environment variable is first read. If this
environment variable is not set, rcqp tries to use the default registry
directory, which varies according to your system (on most Unixes, it is
/usr/local/share/cwb/registry; on Windows it is C:\\CWB\\Registry. If this
directory does not exist, rcqp cannot proceed further.

In order to set the CORPUS_REGISTRY variable, you can either use the
definition mechanisms provided by your system (and in this case, the
CORPUS_REGISTRY variable will be used also if you use CQP outside of R), or
define this variable in the ~/.Renviron file which is read by R at startup
(in that case, the environment variable is defined only for rcqp, and
will not be seen by any other CWB program).
	
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

\keyword{registry}
