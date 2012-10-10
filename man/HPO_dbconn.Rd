\name{HPO_dbconn}

\alias{HPO_dbconn}
\alias{HPO_dbfile}
\alias{HPO_dbschema}
\alias{HPO_dbInfo}

\title{Collect information about the package annotation DB}

\description{
  Some convenience functions for getting a connection object to (or collecting
  information about) the package annotation DB.
}

\usage{
  HPO_dbconn()
  HPO_dbfile()
  HPO_dbschema()
  HPO_dbschema()
  HPO_dbInfo()
}

%\arguments{
%  \item{file}{
%    A connection, or a character string naming the file to print to (see
%    the \code{file} argument of the \code{\link[base]{cat}} function for
%    the details).
%  }
%  \item{show.indices}{
%    The CREATE INDEX statements are not shown by default.
%    Use \code{show.indices=TRUE} to get them.
%  }
%}

\details{
  \code{HPO_dbconn} returns a connection object to the
  package annotation DB.  IMPORTANT: Don't call

  \code{\link[DBI:dbConnect]{dbDisconnect}} on the connection object
  returned by \code{HPO_dbconn} or you will break all the

  \code{\link[AnnotationDbi:AnnDbObj-class]{AnnDbObj}} objects defined
  in this package!

  \code{HPO_dbfile} returns the path (character string) to the
  package annotation DB (this is an SQLite file).

  \code{HPO_dbschema} prints the schema definition of the
  package annotation DB.

  \code{HPO_dbInfo} prints other information about the package
  annotation DB.
}

\value{
  \code{HPO_dbconn}: a DBIConnection object representing an
  open connection to the package annotation DB.

  \code{HPO_dbfile}: a character string with the path to the
  package annotation DB.

  \code{HPO_dbschema}: none (invisible \code{NULL}).

  \code{HPO_dbInfo}: none (invisible \code{NULL}).
}

\seealso{
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbconn}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbfile}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbschema}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbInfo}}
}

\examples{
  dbGetQuery(HPO_dbconn(), "SELECT COUNT(*) FROM hpo_term")
  dbConnect(SQLite(), dbname=HPO_dbfile(), cache_size=64000, synchronous=0)
  HPO_dbschema()
  HPO_dbInfo()
}

\keyword{utilities}
\keyword{datasets}












