\name{HPOPARENTS}
\alias{HPOPARENTS}
\title{Annotation of HPO Identifiers to their Parents} 
\description{
  This data set describes associations between HPO
  terms and their direct parent terms, based on the directed acyclic
  graph (DAG). The format is an R object mapping the HPO terms to 
  all direct parent terms, where a direct parent term is a more general
  HPO term that immediately precedes the given HPO term in the DAG.
}
\details{
  Each HPO  term is mapped to a named vector of HPO  terms. The name
  associated with the parent term will be {isa},
  where {isa} indicates that the child term is a more specific version
  of the parent.
}

\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
  xx <- as.list(HPOPARENTS)
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
     hpoids <- xx[[1]]
     HP(HPOTERM[[hpoids[1]]])
     Term(HPOTERM[[hpoids[1]]])
     Synonym(HPOTERM[[hpoids[1]]])
     Secondary(HPOTERM[[hpoids[1]]])
  }
}

\keyword{datasets}
