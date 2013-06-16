setClass("HPOTermsAnnDbBimap",contains="AnnDbBimap");
setClass("HPOTerms",
    representation(
        HP="character",       # a single string (mono-valued)
        Term="character",       # a single string (mono-valued)
        Synonym="character",    # any length including 0 (multi-valued)
        Secondary="character"   # any length including 0 (multi-valued)
    )
)

### The mono-valued slots are also the mandatory slots.
.HPONODE_MONOVALUED_SLOTS <- c("HP", "Term")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Initialization.
###

setMethod("initialize", "HPOTerms",
    function(.Object, ...)
    {
        args <- list(...)
        argnames <- names(args)
        if (is.null(argnames) || any(argnames == ""))
            stop("all arguments must be named")
        argnames <- match.arg(argnames, slotNames(.Object), several.ok=TRUE)
        if (!(all(.HPONODE_MONOVALUED_SLOTS %in% argnames))) {
            s <- paste(.HPONODE_MONOVALUED_SLOTS, collapse=", ")
            stop("arguments ", s, " are mandatory")
        }
        for (i in seq_len(length(args))) {
            argname <- argnames[i]
            value <- args[[i]]
            if ((argname %in% .HPONODE_MONOVALUED_SLOTS)) {
                if (length(value) != 1)
                    stop("can't assign ", length(value),
                         " values to mono-valued slot ", argname)
            } else {
                value <- value[!(value %in% c(NA, ""))]
            }
            slot(.Object, argname) <- value
        }
        .Object
    }
)

#used to construct a HPOTerms object
HPOTerms <- function(HPOId, term,  synonym = "", secondary = ""
                   ){
    return(new("HPOTerms", HP = HPOId, Term = term,
               Synonym = synonym, Secondary = secondary))
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "HP", "Term", "Ontology", "Definition", "Synonym" and "Secondary" generics (accessor methods).
###

setGeneric("HP", function(object) standardGeneric("HP")) 
setGeneric("Term", function(object) standardGeneric("Term"))
#setGeneric("Ontology", function(object) standardGeneric("Ontology"))
#setGeneric("Definition", function(object) standardGeneric("Definition"))
setGeneric("Synonym", function(object) standardGeneric("Synonym"))
setGeneric("Secondary", function(object) standardGeneric("Secondary"))

setMethod("HP", "HPOTerms", function(object) object@HP)

setMethod("Term", "HPOTerms", function(object) object@Term)

#setMethod("Ontology", "HPOTerms", function(object) object@Ontology)

#setMethod("Definition", "HPOTerms", function(object) object@Definition)

setMethod("Synonym", "HPOTerms", function(object) object@Synonym)

setMethod("Secondary", "HPOTerms", function(object) object@Secondary)


##.HPOid2go_termField() retrieves ids of type field from go_term
.HPOid2go_termField <- function(ids, field){
    require("HPO.db")
##     message(cat("Before SQL \n")) ##test
    sql <- sprintf("SELECT hpo_id, %s
                    FROM hpo_term
                    WHERE hpo_id IN ('%s')",
                   field,
                   paste(ids, collapse="','"))
    res <- dbGetQuery(HPO_dbconn(), sql)
    if(dim(res)[1]==0 && dim(res)[2]==0){
        stop("None of your IDs match IDs from HPO.  Are you sure you have valid IDs?")
    }else{
        ans <- res[[2]]
        names(ans) <- res[[1]]
        return(ans[ids]) ##This only works because each HPO ID is unique (and therefore a decent index ID)
    }
}

setMethod("HP", "HPOTermsAnnDbBimap",function(object) .HPOid2go_termField(keys(object),"hpo_id") )
setMethod("HP", "character",function(object) .HPOid2go_termField(object,"hpo_id") )

setMethod("Term", "HPOTermsAnnDbBimap",function(object) .HPOid2go_termField(keys(object),"term") )
setMethod("Term", "character",function(object) .HPOid2go_termField(object,"term") )

##.HPOid2go_synonymField() retrieves ids of type field from go_synonym
.HPOid2go_synonymField <- function(ids, field){
    require("HPO.db")
    sql <- paste("SELECT gt.hpo_id, gs.",field,"
                  FROM hpo_term AS gt, hpo_synonym AS gs
                  WHERE gt._id=gs._id AND hpo_id IN ('",paste(ids, collapse="','"),"')", sep="")
    res <- dbGetQuery(HPO_dbconn(), sql)
    if(dim(res)[1]==0 && dim(res)[2]==0){
        stop("None of your IDs match IDs from HPO.  Are you sure you have valid IDs?")
    }else{
        ans = split(res[,2],res[,1])
        return(ans[ids])##once again (this time in list context), we are indexing with unique IDs.
    }
}

setMethod("Synonym", "HPOTermsAnnDbBimap",function(object) .HPOid2go_synonymField(keys(object),"synonym") )
setMethod("Synonym", "character",function(object) .HPOid2go_synonymField(object,"synonym") )

setMethod("Secondary", "HPOTermsAnnDbBimap",function(object) .HPOid2go_synonymField(keys(object),"secondary") )
setMethod("Secondary", "character",function(object) .HPOid2go_synonymField(object,"secondary") )


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" methods.
###

setMethod("show", "HPOTerms",
    function(object)
    {
        s <- character(0)
        for (slotname in slotNames(object)) {
            x <- slot(object, slotname)
            if ((slotname %in% .HPONODE_MONOVALUED_SLOTS) && length(x) != 1) {
                warning("mono-valued slot ", slotname,
                        " contains ", length(x), " values")
            } else {
                if (length(x) == 0)
                    next
            }
            s <- c(s, paste(slotname, ": ", x, sep=""))
        }
        cat(strwrap(s, exdent=4), sep="\n")
    }
)

setMethod("as.list", "HPOTermsAnnDbBimap",
    function(x, ...)
    {
        y <- AnnotationDbi:::flatten(x, fromKeys.only=TRUE)
        makeHPONode <- function(hpo_id, Term, ...)
        {
            new("HPOTerms", HP=hpo_id[1],
                           Term=Term[1],
                           #Synonym = Synonym[1],
                           #Secondary = Secondary[1],
                           ...)
        }
        AnnotationDbi:::.toListOfLists(y, mode=1, makeHPONode)
    }
)
