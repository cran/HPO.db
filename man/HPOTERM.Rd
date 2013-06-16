\name{HPOTERM}
\alias{HPOTERM}
\title{Annotation of HPO Identifiers to HPO Terms}
\description{
  This data set gives mappings between HPO identifiers and their respective terms.
}
\details{
  Each HPO identifier is mapped to a \code{HPOTerms} object that has 4 slots:
   HP: HPO Identifier;
   Term: The term for that HPO id;
   Secondary: Secondary terms that have been merged into this term;
   Synonym: other ontology terms that are considered to be synonymous to the primary
            term attached to the HPO id
}
\references{
 \url{http://www.human-phenotype-ontology.org/contao/index.php/hpo_docu.html} 
}

\examples{
    FirstTenHPOBimap <- HPOTERM[1:10] 
    xx <- as.list(FirstTenHPOBimap)
     if(length(xx) > 0){
        HP(xx[[10]])
        Term(xx[[10]])
        Synonym(xx[[10]])
        Secondary(xx[[10]])
    }
}
\keyword{datasets}
