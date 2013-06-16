\name{HPOOBSOLETE}
\alias{HPOOBSOLETE}
\title{Annotation of HPO identifiers by terms and their status are obsolete}
\description{
  This is an R object mapping HPO identifiers to the specific
  terms and their definition are obsolete
}
\details{
  All the obsolete HPO terms that are collected in this index will no longer exist 
  in other mapping objects.
}
\examples{
    xx <- as.list(HPOOBSOLETE)
    if(length(xx) > 0){
         HP(xx[[1]])
         Term(xx[[1]])
    }
}
\keyword{datasets}












