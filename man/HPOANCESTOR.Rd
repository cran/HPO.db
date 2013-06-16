\name{HPOANCESTOR}
\alias{HPOANCESTOR}
\title{Annotation of HPO Identifiers to their Ancestors} 
\description{
  This data set describes associations between HPO 
  terms and their ancestor terms, based on the directed acyclic
  graph (DAG) defined by the Phenotype Ontology Consortium. The format is an R
  object mapping the HPO  terms to all ancestor terms, where an
  ancestor term is a more general HPO term that precedes
  the given HPO term in the DAG (in other words, the parents, and all
  their parents, etc.).
}
\details{
  Each HPO  term is mapped to a vector of ancestor HPO  terms.
}
\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
  xx <- as.list(HPOANCESTOR)
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    hpoids <- xx[1:2]
    # Get the ancestor HPO IDs for the first two elents of xx
    print(hpoids)
  }
  
}
\keyword{datasets}
