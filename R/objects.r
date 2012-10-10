HPO_DB_AnnDbBimap_seeds <- list(
    list(
        objName="PARENTS",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="hpo_term",
                Lcolname="hpo_id",
                Rcolname="_id"
            ),
            list(
                tablename="hpo_parents",
                Lcolname="_id",
              #  tagname=c(RelationshipType="{relationship_type}"), #not very clear here
                Rcolname="_parent_id"
            ),
            list(
                tablename="hpo_term",
                Lcolname="_id",
                Rcolname="hpo_id"
            )
        )
    ),
    
    list(
        objName="ANCESTOR",
        Class="AnnDbBimap",
        L2Rchain=list(
            list(
                tablename="hpo_term",
                Lcolname="hpo_id",
                Rcolname="_id"
            ),
            list(
                tablename="hpo_offspring",
                Lcolname="_offspring_id",
                Rcolname="_id"
            ),
            list(
                tablename="hpo_term",
                Lcolname="_id",
                Rcolname="hpo_id"
            )
        )
    ),
   
    list(
        objName="TERM",
        #Class="GOTermsAnnDbBimap",
       	#Class="AnnDbBimap",
       	Class="HPOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="hpo_term",
                Lcolname="hpo_id",
                Rcolname="hpo_id",
                Rattribnames=c(
                    Term="{term}",
                    #Ontology="{ontology}",
                    #Definition="{definition}",
                    Synonym="hpo_synonym.synonym",
                    Secondary="hpo_synonym.secondary"
                ),
                Rattrib_join="LEFT JOIN hpo_synonym ON {_id}=hpo_synonym._id"
            )
        )
    ),
    list(
        objName="OBSOLETE",
        #Class="GOTermsAnnDbBimap",
        #Class="AnnDbBimap",
       	Class="HPOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="hpo_obsolete",
                Lcolname="hpo_id",
                Rcolname="hpo_id",
                Rattribnames=c(
                    Term="{term}",
                    #Ontology="{ontology}",
                    #Definition="{definition}",
                    ## The RSQLite driver crashes on queries like
                    ##   SELECT NULL, ... FROM ...
                    ## so a temporary workaround is to use
                    ##   SELECT '', ... FROM ...
                    #Synonym="NULL",
                    #Secondary="NULL"
                    Synonym="''",
                    Secondary="''"
                )
            )
        )
    ),
    list(
        objName="SYNONYM",
        #Class="GOTermsAnnDbBimap",
        #Class="AnnDbBimap",
       	Class="HPOTermsAnnDbBimap",
        L2Rchain=list(
            list(
                tablename="hpo_synonym",
                Lcolname="synonym",
                Rcolname="_id",
                filter="{like_hpo_id}=1"
            ),
            list(
                tablename="hpo_term",
                Lcolname="_id",
                Rcolname="hpo_id",
                Rattribnames=c(
                    Term="{term}",
                    #Ontology="{ontology}",
                    #Definition="{definition}",
                    Synonym="hpo_synonym.synonym",
                    Secondary="hpo_synonym.secondary"
                ),
                Rattrib_join="LEFT JOIN hpo_synonym ON {_id}=hpo_synonym._id"
            )
        )
    )
)

createAnnObjs.HPO_DB <- function(prefix, objTarget, dbconn, datacache)
{
    #Now skip here
    #checkDBSCHEMA(dbconn, "HPO_DB") 

    ## AnnDbBimap objects
    seed0 <- list(
        objTarget=objTarget,
        datacache=datacache
    )
    #ann_objs <- createAnnDbBimaps(HPO_DB_AnnDbBimap_seeds, seed0)
    ann_objs <- AnnotationDbi:::createAnnDbBimaps(HPO_DB_AnnDbBimap_seeds, seed0)

    ## Reverse maps
    #I am not sure whether it is suitable for diease ontology
    revmap2 <- function(from, to)
    {
        map <- revmap(ann_objs[[from]], objName=to)
        L2Rchain <- map@L2Rchain
        tmp <- L2Rchain[[1]]@filter
        abc <- L2Rchain[[length(L2Rchain)]]@filter
        L2Rchain[[1]]@filter<-abc
        L2Rchain[[length(L2Rchain)]]@filter <- tmp
        map@L2Rchain <- L2Rchain
        map
    }
    ann_objs$CHILDREN <- revmap2("PARENTS", "CHILDREN")
    ann_objs$OFFSPRING <- revmap2("ANCESTOR", "OFFSPRING")

    ## 1 special map that is not an AnnDbBimap object (just a named integer vector)
    #ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    ann_objs$MAPCOUNTS <- AnnotationDbi:::createMAPCOUNTS(dbconn, prefix)

    #prefixAnnObjNames(ann_objs, prefix)	
    AnnotationDbi:::prefixAnnObjNames(ann_objs, prefix)
}




