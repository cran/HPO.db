\name{HPOCHILDREN}
\alias{HPOCHILDREN}
\title{Annotation of HPO Identifiers to their Children} 
\description{
  This data set describes associations between HPO 
  terms and their direct children terms, based on the directed acyclic
  graph (DAG). The format is an R
  object mapping the HPO terms to all direct children terms, where a
  direct child term is a more specific HPO term that is immediately preceded
  by the given HPO term in the DAG.
}
\details{
   Each HPO  term is mapped to a vector of children HPO  terms.
}
\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
  xx <- as.list(HPOCHILDREN)
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











