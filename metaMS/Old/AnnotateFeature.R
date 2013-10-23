AnnotateFeature <-
function(input,          ## vector of the form mz,rt,I
                            polarity,       ## polarity 
                            rttol,          ## allowed rt tol in mins
                            db)             ## db name
{               


  ## Check weather the intensity is 0 or NA
  if (is.na(input[3]) == TRUE) {
    output <- which(TRUE & FALSE)   ## Return nothing
  }

  else if (input[3] < 1E-5) {
    output <- which(TRUE & FALSE)
  }

  else {
 
    ## calculate the mass tolerance for the feature under analysis
    mztolppm <- predict(db$errf,
                        data.frame(M = input[1],
                                   logI = log10(input[3])))
    if (mztolppm < 5) {mztolppm <- 5}
    mztol <- (mztolppm*1e-6)*input[1]
    
    ## setting the DB mode
    if (polarity == "pos"){DB <- db$pos}
    else {DB <- db$neg}
    
    
    ## calculate the differences
    deltam <- abs(DB[,"mz"] - input[1])
    deltart <- abs(DB[,"rt"] - input[2])
  
    ## combine the tolerances
    idm <- deltam < sqrt((DB[,"tolerance"])^2 + (mztol)^2)
    idt <- deltart < rttol
    
    output <- which(idm & idt)
  }
  output
}
