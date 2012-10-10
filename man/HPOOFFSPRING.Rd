\name{HPOOFFSPRING}
\alias{HPOOFFSPRING}
\title{Annotation of HPO Identifiers to their Offspring} 
\description{
  This data set describes associations between HPO 
  terms and their offspring terms, based on the directed acyclic
  graph (DAG). The format is an R object mapping the HPO  terms 
  to all offspring terms, where an ancestor term is a more specific 
  HPO term that is preceded by the given HPO term in the DAG.
}

\details{
  Each HPO  term is mapped to a vector of offspring HPO  terms.
}
\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
  xx <- as.list(HPOOFFSPRING)
  xx <- xx[!is.na(xx)]
   if(length(xx) > 0){
    hpoids <- xx[1:2]
    print(hpoids)
  }
}
\keyword{datasets}
