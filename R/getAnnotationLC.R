## RW added DBname as an argument
getAnnotationLC <- function(xs, settings, DB, errf) {
    
    # ------- Peak table with maxo --------------------------------- >
    PkT4ann <- getPeakTable(xs, intval = "maxo")
    
    ## check the number of samples
    if (length(sampnames(xs@xcmsSet)) == 1) {
        dataMatrix <- PkT4ann[, "maxo", drop = FALSE]
        I <- dataMatrix
    } else {
        dataMatrix <- PkT4ann[, make.names(sampnames(xs@xcmsSet)), drop = FALSE]
        I <- apply(dataMatrix, 1, median)  ## use the median intensity in DM for annotation
    }
    
    ## -------- Table to be annotate --------------------------------- >
    tobeannotated <- cbind(mz = PkT4ann$mz, rt = PkT4ann$rt, I = unlist(I))
    
    ## complete annotation information
    out.raw <- AnnotateTable(tobeannotated, errf = errf, DB = DB, settings = settings)
    
    ## subset of the annotation output to be used in the metaMS output
    ann.ID <- rep("", dim(tobeannotated)[1])
    ann.compound <- rep("", dim(tobeannotated)[1])
    annotated.features <- unique(out.raw$annotation.table$feature)
    for (id in annotated.features) {
        ann.ID[id] <- paste(out.raw$annotation.table$ChemSpiderID[out.raw$annotation.table$feature == id], collapse = " ")
        ann.compound[id] <- paste(out.raw$annotation.table$compound[out.raw$annotation.table$feature == id], collapse = " ")
    }
    
    
    ## ------ Format the output -------------------------------------- >
    
    list(raw = out.raw, for_table = data.frame(ChemSpiderID = ann.ID, compound = ann.compound, stringsAsFactors = FALSE))
    
}
