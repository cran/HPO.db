\name{HPOSYNONYM}
\alias{HPOSYNONYM}
\title{Map from HPO synonyms to HPO terms}
\description{
  HPOSYNONYM is an R object that provides mapping
  from HPO synonyms to HPO terms
}
\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
    x <- HPOSYNONYM
    sample(x, 3)
    
    HPOTERM[["HP:0002095"]]

    xx <- as.list(HPOSYNONYM)
    xx <- xx[!is.na(xx)]
    hpoids <- xx[1:2]
    print(hpoids)
}
\keyword{ dataset }











