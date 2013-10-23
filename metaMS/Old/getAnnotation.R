getAnnotation <- function(xs,
                          settings){
  
  # ------- Peak table with maxo---------------------------------
  PkT4ann <- getPeakTable(xs, intval = "maxo")
  rt.id <- which(colnames(PkT4ann) == "rt")
  DM <- PkT4ann[,((rt.id+1):dim(PkT4ann)[2])]
  
  # ------- If only one file 
  if (length(settings$files) == 1){
    I <- DM
  }
  else {
    I <- rowMeans(DM)
  }
  
  cbind("mz" = PkT4ann$mz, "rt" = PkT4ann$rt,I)
  
}