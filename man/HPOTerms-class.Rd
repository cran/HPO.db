\name{HPOTerms-class}
\docType{class}

\alias{class:HPOTerms}
\alias{HPOTerms-class}

\alias{HPOTerms}

\alias{initialize,HPOTerms-method}

\alias{HPOID}
\alias{HPOID,HPOTerms-method}
\alias{HPOID,HPOTermsAnnDbBimap-method}
\alias{HPOID,character-method}
\alias{Term}
\alias{Term,HPOTerms-method}
\alias{Term,HPOTermsAnnDbBimap-method}
\alias{Term,character-method}
\alias{Synonym}
\alias{Synonym,HPOTerms-method}
\alias{Synonym,HPOTermsAnnDbBimap-method}
\alias{Synonym,character-method}
\alias{Secondary}
\alias{Secondary,HPOTerms-method}
\alias{Secondary,HPOTermsAnnDbBimap-method}
\alias{Secondary,character-method}

\alias{show,HPOTerms-method}

\title{Class "HPOTerms"}

\description{A class to represent Human Phenotype Ontology nodes}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{HPOnode(HP, Term, synonym, secondary)}.
  HP, Term are required.
}

\section{Slots}{
  \describe{
    \item{\code{HP}:}{Object of class \code{"character"} A character
      string for the HPO id of a primary node.}
    \item{\code{Term}:}{Object of class \code{"character"} A
      character string that defines the role of gene product
      corresponding to the primary HPO id.}
    \item{\code{Synonym}:}{Object of class \code{"character"} other
      ontology terms that are considered to be synonymous to the primary
      term attached to the HPO id. Synonymous here can mean that the
      synonym is an exact synonym of the primary term, is related to the
      primary term, is broader than the primary term, is more precise
      than the primary term, or name is related to the term, but is not
      exact, broader or narrower.}
    \item{\code{Secondary}:}{Object of class \code{"character"} HPO ids
      that are secondary to the primary HPO id as results of merging HPO
      terms so that One HPO id becomes the primary HPO id and the rest
      become the secondary.}
  }
}

\section{Methods}{
  \describe{
    \item{HP}{\code{signature(object = "HPOTerms")}:
      The get method for slot HPOID.}
    \item{Term}{\code{signature(object = "HPOTerms")}:
      The get method for slot Term.}
    \item{Synonym}{\code{signature(object = "HPOTerms")}:
      The get method for slot Synonym.}
    \item{Secondary}{\code{signature(object = "HPOTerms")}:
      The get method for slot Secondary.}
    \item{show}{\code{signature(x = "HPOTerms")}:
      The method for pretty print.}
  }
}

\note{HPOTerms objects are used to represent primary HPO nodes in the
   SQLite-based annotation data package HPO.db}

\examples{
  HPOnode <- new("HPOTerms", HP="HPOID:1234567", Term="Test")
  HP(HPOnode)
  Term(HPOnode)

  require(HPO.db)
  FirstTenHPOBimap <- HPOTERM[1:10] 
  Term(FirstTenHPOBimap)

  ids = keys(FirstTenHPOBimap)
  Term(ids)
}

\keyword{datasets}
