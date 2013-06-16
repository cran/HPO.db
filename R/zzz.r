datacache <- new.env(hash=TRUE, parent=emptyenv())

HPO <- function() showQCData("HPO", datacache)
HPO_dbconn <- function() dbconn(datacache)
HPO_dbfile <- function() dbfile(datacache)
HPO_dbInfo <- function() dbInfo(datacache)
HPO_dbschema <- function(){writeLines(strwrap(readLines(system.file("DBschemas","schemas_1.0","HPO_DB.sql", package ="HPO.db")),indent=2, exdent=4))}

.onLoad <- function(libname, pkgname)
{
    #require("methods", quietly=TRUE)
    
    #################################
    #set the classes and defined the methods
    #classfile<-system.file("scripts","HPOClasses.R", package=pkgname, lib.loc=libname)
    #source(classfile);
    setClass("HPOTermsAnnDbBimap",contains="AnnDbBimap");
    setClass("HPOTerms",
      representation(
        HP="character",       # a single string (mono-valued)
        Term="character",       # a single string (mono-valued)
        Synonym="character",    # any length including 0 (multi-valued)
        Secondary="character"   # any length including 0 (multi-valued)
      )
    )
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "HPO.sqlite", package=pkgname, lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    
    ann_objs<-createAnnObjs.HPO_DB("HPO", "HPO", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)   
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(HPO_dbconn())
 e <- new.env()
e$.packageName <- "HPO.db"

}

