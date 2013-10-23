AnnotateTable <-
function(peaktable,
                           polarity = "pos",
                           chrom = "RP",
                           rttol = 0.5,                      
                           rtval = 0.1){

  ## load the database included in the package
  # data(DBHilic)
   data('stdDB_LC_hilic')
  # data(DBRP)
   data('stdDB_LC_RP')
  
  ## set the DB
  if (chrom == "RP"){db <- DBRP}
  else{db <- DBHilic}
  
  ## setting the DB mode
  if (polarity == "pos"){DB <- db$pos}
  else {DB <- db$neg}  
  
  ## annotate the full peaklist with anotate feature. 
  feat.to.db <-  apply(peaktable,
                       1,
                       function(input) AnnotateFeature(input,polarity,rttol,db))


  if (length(feat.to.db) != 0) {
    mytable <- list()
    for (i in seq(along = feat.to.db)){
      if(length(feat.to.db[[i]]) == 0) {
        mytable[[i]] <- NA}
      else {
        mytable[[i]] <- cbind("feature" = i,"db_position" =feat.to.db[[i]])
      }  
    }
    
    mytable <- do.call(rbind,mytable)
    mytable <- mytable[!is.na(mytable[,1]),]

    mytable <- as.data.frame(mytable)

    ## add the ID
    mytable[,"ID"] <- as.character(DB[mytable[,2],"chemSpider"])
  
    ## table of the found femids
    femids <- unique(mytable[,"ID"])


    ## Create a list of "pseudospectra"
    ## with the experimental features associated
    ## to a specific ID
    ## each elment of the list is a dataframe with the complete
    ## association

    femid.to.pseudospectrum <- list()

    for(id in femids){
      myid <- mytable[,"ID"] == id
      ## add to the pseudospectrum the info about the db
      femid.to.pseudospectrum[[id]] <- mytable[myid,]
      ## add the actual Mass Retention Time and Intensity
      femid.to.pseudospectrum[[id]][,c("mz","rt","I")] <- peaktable[mytable[myid,"feature"],]
      ## add the mass, retention time and intensity in the DB
      mydbid <- femid.to.pseudospectrum[[id]][,"db_position"]
      femid.to.pseudospectrum[[id]][,c("compound","db_mz","db_rt","db_I")] <- DB[mydbid,c("compound","mz","rt","intensity")]
    }

 
    ## Cleaning of the annotation 

    
    ## erease the FEMid with a single associated feature
    single.pseudospectra <- sapply(femid.to.pseudospectrum, function(x) dim(x)[1])
    single.no <- single.pseudospectra > 1
    
    ## update the pseudospectra
    femid.to.pseudospectrum <- femid.to.pseudospectrum[single.no]
  

    ## cluster according to rt each pseudospectrum
    femid.to.pseudospectrum.cl <- lapply(femid.to.pseudospectrum, function(x){
      one <- hclust(dist(x[,"rt"]))
      clid <- cutree(one, h = rtval)
      x[,"clid"] <- clid
      x
    })
  
  
    ## remove the annotation if the number of clid equals the number of
    ## elements in the pseudospectrum
    single2.id <- sapply(femid.to.pseudospectrum.cl, function(x){
      nclass <- length(unique(x[,"clid"]))
      if (nclass == dim(x)[1]){out <- FALSE}
      else (out <- TRUE)
      out
    })

    femid.to.pseudospectrum.cl <- femid.to.pseudospectrum.cl[single2.id]
  

    ## clean the pseudospectra from "insulate" features
    
    femid.to.pseudospectrum.final <- lapply(femid.to.pseudospectrum.cl, function(x){
      id.in <- which(table(x[,"clid"]) > 1)  ## clusters id with more than one entry
      out <- x[x[,"clid"] %in% id.in,]
      out
    })
  
    out <- do.call(rbind,femid.to.pseudospectrum.final)
    rownames(out) <- NULL
    out
  
  }
  else {
    out <- list()
    out
  }
}
