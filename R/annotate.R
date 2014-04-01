## This function is designed to be called only inside the
## annotate Table
AnnotateFeature <- function(input,   
                            DB,
                            settings,
                            errf)                            
{               
  
  ## Check weather the intensity is 0 or NA ------------------------- #
  if (is.na(input["I"])) 
      return(which(TRUE & FALSE))   ## Return nothing
  
  if (input["I"] < 1E-5)
      return(which(TRUE & FALSE))
  
  ## If the intensity is good match  --------------------------------- #
  ## calculate the mass tolerance for the feature under analysis
  if (is.null(errf)){
    mztol <-  settings$mzdiff
  } else {
    mztolppm <- predict(errf,
                        data.frame(M = input["mz"],
                                   logI = log10(input["I"])))
    if (mztolppm < settings$ppm) {mztolppm <- settings$ppm}
    mztol <- (mztolppm*1e-6)*input["mz"]
  }
  ## calculate the differences -------------------------------------- #
  deltam <- abs(DB[,"mz"] - input["mz"])
  deltart <- abs(DB[,"rt"] - input["rt"])
  ## combine the tolerances ----------------------------------------- #
  ## Squared sum 
  idm <- deltam < sqrt((DB[,"mz.err"])^2 + (mztol)^2)
  idt <- deltart < settings$rtdiff
  
  which(idm & idt)
}

#############################################################################

AnnotateTable <- function(peaktable,
                          errf,
                          DB,
                          settings) {
  
 
 
 
  ## calculate the tolerance and append a column to the table  
  ## naming the column mz.err
  
  if (is.null(errf)) {
    DB$mz.err <- settings$mzdiff
    printInfo("Fixed mass tolerance", settings$mzdiff)
  } else
  { 
    printInfo("Adaptive mass tolerance")
    ppmtol <- predict(errf,
                      data.frame(M = DB$mz,
                                 logI = log10(DB$maxo)))
    ppmtol[ppmtol < settings$ppm] <- settings$ppm
    DB$mz.err  <- (ppmtol*1e-6)*DB$mz 
  }
 
  ## annotate the full peaklist  -------------------------------------
  ## each peak in the peaktable is associated to
  ## one (or more) entries present in the DB. feat.to.db is a list
  printInfo("Feature-wise Annotation ...")
  feat.to.db <-
    lapply(1:nrow(peaktable),
             function(r) AnnotateFeature(peaktable[r,],
                                         DB = DB,
                                         settings = settings,
                                         errf = errf))
  
  printString("Formatting the output")
  ## -------------------------------------------------------------------
  ## some controls on the results of the annotation                        
  ## 1) no output, return an empty list
  if (length(unlist(feat.to.db)) == 0)
    return (list("annotation.table" = NULL,
                 "compounds" = NULL,
                 "ChemSpiderIDs" = NULL,
                 "multiple.annotations" = NULL,
                 "ann.features" = NULL
                 
    ))  

  ## else go through the list and create a series of tables which
  ## associate features and db entries
  mytable <- lapply(1:length(feat.to.db),
                    function(i) {
                      if (length(feat.to.db[[i]]) == 0) {
                        NA
                      } else {
                        cbind("feature" = i,"db_position" = feat.to.db[[i]])
                      }
                    })
  
  ## mytable <- list()
  ## for (i in seq(along = feat.to.db)){
  ##   ## here I tell what to do if there is no annotation for a feature  
  ##   if(length(feat.to.db[[i]]) == 0) {
  ##     mytable[[i]] <- NA}
  ##   ## otherwise create a table with the features and the associated DB entries
  ##   else {
  ##     mytable[[i]] <- cbind("feature" = i,"db_position" = feat.to.db[[i]])
  ##   }  
  ## }

  ## join everithing in a big table which associates features and db entries

    
  mytable <- do.call(rbind, mytable)
  
  mytable <- mytable[!is.na(mytable[,1]),, drop = FALSE] # remove NAs
  mytable <- as.data.frame(mytable)        # create a data frame
    
  ## add the ChemSpiderID
  mytable[,"ChemSpiderID"] <- DB[mytable[,"db_position"], "ChemSpiderID"]

  ## ------------------------------------------------------------------
  ## At this point I have a table which associates the annotated
  ## features to DB entries namely db_postion and ChemSpiderID
    
    
  ## ------------------------------------------------------------------
  ## To check the quality of the annotation i s necessary to associate each ChemSpiderID 
  ## to the experimental features it is assigned to 
    
  femids <- unique(mytable[, "ChemSpiderID"]) # all the found ChemSpiderID

  ## Create a list of "pseudospectra"
  ## with the experimental features associated
  ## to a specific ChemSpiderID
  ## each elment of the list is a dataframe with the complete
  ## association
    
    
  femid.to.pseudospectrum <- lapply(femids, function(id){
    myid <- mytable[,"ChemSpiderID"] == id
    ## add to the pseudospectrum the info about the db
    output <- mytable[myid,]  
    ## add the  Mass Retention Time and Intensity of the features
    output[,c("mz","rt","I")] <- peaktable[mytable[myid,"feature"],] 
    ## add the mass, retention time and intensity in the DB
    ## to be updated depending on the format of the DB
    output[,c("compound","db_mz","db_rt","db_I","db_ann","mz.err")] <-
        DB[output[,"db_position"],
                 c("compound","mz","rt","maxo","validated", "mz.err")]
    output
  })
  
  
  ## ------------------------------------------------------------------
  ## Each pseudospectrum have to be cleaned and validated
    
    
  ## 1) Erase from the annotations IDs with only one member for
  ## pseudospectrum 
  single.pseudospectra <- sapply(femid.to.pseudospectrum, function(x) dim(x)[1])
  single.no <- single.pseudospectra > 1
  ## update the pseudospectra
  femid.to.pseudospectrum <- femid.to.pseudospectrum[single.no]
    
  if(length(femid.to.pseudospectrum) == 0) {
    return (list("annotation.table" = NULL,
                 "compounds" = NULL,
                 "ChemSpiderIDs" = NULL,
                 "multiple.annotations" = NULL,
                 "ann.features" = NULL))    
  }
  
  ## 2) I need more features with an rt difference < rtval
  femid.to.pseudospectrum.cl <-
      lapply(femid.to.pseudospectrum,
             function(x){
               one <- hclust(dist(x[,"rt"]), method = "complete")
               clid <- cutree(one, h = settings$rtval)
               x[,"clid"] <- clid
               x
             })
  
  ## keep only rows belonging to clusters with enough members
  femid.to.pseudospectrum.final <-
      lapply(femid.to.pseudospectrum.cl,
             function(x) {
               clSize <- table(x[,"clid"])
               bigCl <- as.numeric(names(clSize[clSize >= settings$minfeat]))
               x[x[,"clid"] %in% bigCl,]
             })
  
  ## ## remove the annotation if the number of clid equals the number
  ## ## of elements in the pseudospectrum
  ## which.single.id <-
  ##     sapply(femid.to.pseudospectrum.cl,
  ##            function(x){
  ##              nclass <- length(unique(x[,"clid"]))
  ##              if (nclass == dim(x)[1])
  ##                  {out <- FALSE}
  ##              else (out <- TRUE)
  ##              out
  ##            })
  ## ## update the list of pseudospectra 
  ##   femid.to.pseudospectrum.cl <- femid.to.pseudospectrum.cl[which.single.id]
  
  ## ## 3) clean the pseudospectra from "insulate" features    
  ## femid.to.pseudospectrum.final <-
  ##     lapply(femid.to.pseudospectrum.cl,
  ##            function(x){
  ##              id.in <- which(table(x[,"clid"]) > 1)  ## clusters id with more than one entry
  ##              out <- x[x[,"clid"] %in% id.in,]
  ##              out
  ##            })
    
  if (max(sapply(femid.to.pseudospectrum.final, length)) == 0){
    return (list("annotation.table" = NULL,
                 "compounds" = NULL,
                 "ChemSpiderIDs" = NULL,
                 "multiple.annotations" = NULL,
                 "ann.features" = NULL
                 ))    
  }
  
  ## ----------------------------------------------------------------
  ## Format the output
  format.output <- do.call(rbind, femid.to.pseudospectrum.final)
  ann.table <- table(format.output$feature)
  rownames(format.output) <- NULL
  list("annotation.table" = format.output,
       "compounds" = unique(as.character(format.output[,"compound"])),
       "ChemSpiderIDs" = unique(format.output[,"ChemSpiderID"]),
       "multiple.annotations" = as.numeric(names(ann.table)[ann.table > 1]),
       "ann.features" = unique(format.output[,"feature"])) 
}


