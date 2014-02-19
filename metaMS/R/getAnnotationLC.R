## RW added DBname as an argument
getAnnotationLC <- function(xs,
                            settings,
                            DB,
                            errf)
{
  
  # ------- Peak table with maxo---------------------------------
  
  
  
  PkT4ann <- getPeakTable(xs, intval = "maxo")
  rt.id <- which(colnames(PkT4ann) == "rt")
  DM <- PkT4ann[,((rt.id+1):dim(PkT4ann)[2]), drop = FALSE]
  
  ## check the class of the object to set the the 
  if (class(xs)[1] == "xcmsSet") {
    nsamp  <- length(xs@filepaths)
  } else {
    nsamp  <- length(xs@xcmsSet@filepaths)
  }
  
  if (nsamp == 1) {
    I <- DM
  } else {
    I <- apply(DM,1,max) ## Change this
  }
  
  
  
  ## -------- Table to be annotate
  tobeannotated <- cbind("mz" = PkT4ann$mz, "rt" = PkT4ann$rt, "I" = unlist(I))
  

  
  out.raw <- AnnotateTable(tobeannotated,
                           errf = errf,
                           DB = DB,
                           settings = settings)
  
  ann.ID <- rep("", dim(tobeannotated)[1])
  ann.compound <- rep("", dim(tobeannotated)[1])
  
  
  annotated.features <- unique(out.raw$annotation.table$feature)
  
  
  for (id in annotated.features){
    ann.ID[id] <- paste(out.raw$annotation.table$ChemSpiderID[out.raw$annotation.table$feature == id], collapse = " ")
    ann.compound[id] <- paste(out.raw$annotation.table$compound[out.raw$annotation.table$feature == id], collapse = " ")
  } 
  
  
  ## ------ Format the output 
  
  list(raw = out.raw,
       for_table = data.frame(ChemSpiderID = ann.ID,
                              compound = ann.compound,
                              stringsAsFactors = FALSE))
  
}
