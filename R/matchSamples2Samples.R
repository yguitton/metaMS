## two patterns from different xset.msp that have the same annotation
## will be the same variable. However, for patterns without annotation
## similar relations can exist. We seek patterns in different files,
## eluting within a given time window with a given spectral
## similarity. 

## This function returns a list of two things: the first element is a
## msp object containing the pseudospectra that are found in multiple
## samples, and the second is an object like the one returned by
## matchSamples2DB, a nested list containing for each pattern in each
## sample either nothing or the index of the spectrum as it is found
## in the DB. 

## For larger data sets, the corresponding yes-no matrix will be way
## too large (the 2007 grape metabolome leads to a 36734x36734
## matrix), but it is very sparse. If argument annotations equals
## NULL, this function will only return the spectra that are found in
## several patterns and not the annotations.

## Note that from July 30 2013 on, xset.work.orig contains SCALED spectra,
## and xset.msp unscaled spectra!

## Aug 13: new structure of annotations, now a three-column data.frame

## Dec 19: addition of RI check, as an alternative to rt check
matchSamples2Samples <- function(xset.msp.scaled,
                                 xset.msp,
                                 annotations,
                                 settings)
{
  if (is.null(annotations)) {
    annotations <- lapply(xset.msp.scaled,
                          function(x) makeAnnotation(0))
    noannot.idx <- lapply(xset.msp, function(x) 1:length(x))
    xset.work <- xset.msp.scaled
  } else {
    ## throw out those patterns that already have an annotation
    noannot.idx <- mapply(function(x, y)
      which(!(1:length(x)) %in% y[,"pattern"] ),
      xset.msp.scaled,
      annotations)

    #ADD this due to an issue where col with same length output a matrix... and that was a problem for next things
    if(is(noannot.idx)[1] == "matrix"){
      noannot.idx <- as.list(as.data.frame(noannot.idx))
    }
    xset.work <- mapply(function(x, y) x[y], xset.msp.scaled, noannot.idx)

    #ADD this due to an issue where col with same length output a matrix... and that was a problem for next things
    if(is(xset.work)[1] == "matrix"){
      xset.work <- as.list(as.data.frame(xset.work))
    }
    #To correct issue when 1 unkn only in noannot.idx (make a list and not a list of list)
    if(unique(lengths(noannot.idx[1])) == 1){
      xset.work <- lapply(xset.work[1], function(x) list(x))
    }
  }
  
  ## do the matching: a simple double loop over all unassigned patterns
  npatterns <- sum(sapply(xset.work, length))
  cumpatterns <- c(0, cumsum(sapply(xset.work, length)))
  names(cumpatterns) <-  NULL
  
  pattern.match.result <- Matrix::Matrix(0, npatterns, npatterns, sparse = TRUE)
  for (i in 1:(length(xset.work) - 1)) {
    for (j in (i+1):length(xset.work)) {
      matchmat <- match.unannot.patterns(xset.work[[i]],
                                         xset.work[[j]],
                                         settings = settings)
      if (length(matchmat) > 0) {
        for (k in 1:nrow(matchmat)) {
          id1 <- cumpatterns[i] + matchmat[k, "ID1"]  
          id2 <- cumpatterns[j] + matchmat[k, "ID2"]  
          pattern.match.result[id1, id2] <- 1         
          pattern.match.result[id2, id1] <- 1
        }        
      }
    }
  }
  
  ## simply take the first unclustered pattern and add all non-zeros
  ## un the corresponding row of the data matrix as the initial
  ## cluster - add all other non-clustered numbers in the new cluster
  ## members to the list until there is no more growth
  
  grow.clust <- function(seed, pmr) 
    unique(c(unlist(apply(pmr[seed,,drop = FALSE],
                          1,
                          function(x) which(x > 0)))))
  ## we start with a list of zeros, which means: unchecked. Every
  ## unchecked pattern will be used as a seed - if it is part of a
  ## cluster, the whole cluster will receive the same cluster number,
  ## if it is a singleton, it will get label -1.
  pmr.classes <- rep(0, npatterns)
  current.cl <- 1
  while (length(seed <- which(pmr.classes == 0)) > 0) {
    seed <- min(seed)
    clstr <- c(seed, grow.clust(seed, pattern.match.result))
    nel <- length(clstr)
    if (nel == 1) {
      pmr.classes[seed] <- -1
    } else {
      while(length(clstr <- grow.clust(clstr, pattern.match.result))>nel)
        nel <- length(clstr)
      
      pmr.classes[clstr] <- current.cl
      current.cl <- current.cl + 1
    }
  }
  
  ## check for minimal cluster size - for a meaningful value should be
  ## at least 2...
  minsize <- max(2,
                 min(settings$min.class.size,
                     settings$min.class.fraction * length(xset.msp)))
  bigClusters <- which(table(pmr.classes[pmr.classes > 0]) >= minsize)
  if (length(bigClusters) == 0) { ## nothing found
    return(list(annotations = annotations, unknowns = NULL))
  } else {
    pmr.classes[!(pmr.classes %in% bigClusters)] <- -1
    ## get rid of unused cluster numbers
    pmr.classes[pmr.classes > 0] <-
      as.integer(factor(pmr.classes[pmr.classes > 0]))
    clusters <- 1:max(pmr.classes)
    nclus <- length(clusters)
    
    ## every cluster now leads to one pattern in a msp-like structure,
    ## that is found in several samples. As the example pseudospectrum
    ## we take the one that is in the middle of the cluster.
    ## Even though we know the total number of new components to be
    ## added, we do not know in which sample they will be found. So we
    ## will allocate the maximum number of new annotations for each
    ## pattern and will remove the empty ones at the end.
    pspc.DB <- vector(nclus, mode = "list")
    new.annotations <- lapply(xset.work,
                              function(x)
                                makeAnnotation(nclus))
    
    for (cl in 1:nclus) {
      p.idx <- which(pmr.classes == cl)
      ## for these ids we need to find the samples as well as the
      ## patterns involved
      sample.idx <- apply(outer(p.idx, cumpatterns[-1], "<="),
                          1,
                          function(x) which(x)[1])
      spec.idx <- p.idx - cumpatterns[sample.idx]
      
      ## the first one with the most matches is taken as the typical pattern
      central.idx <-
        which.max(Matrix::rowSums(pattern.match.result[p.idx, p.idx]))
      fullspec.idx <- noannot.idx[[ sample.idx[central.idx] ]][[ spec.idx[central.idx] ]]
      pspc.DB[[ cl ]] <-              ## store this pseudospectrum!
        xset.msp[[ sample.idx[central.idx] ]][[ fullspec.idx ]]
      
      ## flag the patterns that match this known unknown in the
      ## annotation object. We need to take care to use the numbers for
      ## ALL patterns, not just for the ones without annotations...
      ## We use negative numbers for the annotation
      ## of the known unknowns, to distinguish them from the annotations
      ## from the real DB
      
      for (ii in seq(along = sample.idx)) {
        fullspec.idx <- noannot.idx[[ sample.idx[ii] ]][[ spec.idx[ii] ]]
        new.annotations[[ sample.idx[ii] ]][cl, "pattern"] <- fullspec.idx
        new.annotations[[ sample.idx[ii] ]][cl, "annotation"] <- -cl
      }
    }
  }
  
  ## now clean the new annotations and return the combined lists
  new.annotations <- lapply(new.annotations,
                            function(x)
                              x[!x[,"pattern"] == 0,])
  
  #I probably have to switch new.annotatoins and annotations
  #Because I obtain an error when the first file has no annotation...
  list(annotations = mapply(rbind, new.annotations, annotations,
                            SIMPLIFY = FALSE),
       unknowns = pspc.DB)
}

## Function match.unannot.patterns takes two lists of unannotated
## patterns and returns a matrix with IDs and RTs of matching
## patterns, as well as the match factor
match.unannot.patterns <- function(msp1, msp2, settings) {
  maxdiff <- ifelse(settings$timeComparison == "rt",
                    settings$rtdiff,
                    settings$RIdiff)
  
  rt1 <- sapply(msp1, function(x) mean(x[,settings$timeComparison]))
  rt2 <- sapply(msp2, function(x) mean(x[,settings$timeComparison]))
  
  msp.rtdiffs <- abs(outer(rt1, rt2, "-"))
  close.idx <- which(msp.rtdiffs < maxdiff, arr.ind = TRUE)
  if (length(close.idx) > 0) {
    X1 <- msp1[close.idx[,1]]
    X2 <- msp2[close.idx[,2]]
    
    matchfactors <- mapply(mzmatch, X1, X2)
    if (any(good.idx <- matchfactors > settings$simthresh)) {
      cbind(ID1 = close.idx[good.idx,1],
            RT1 = rt1[close.idx[good.idx,1]],
            ID2 = close.idx[good.idx,2],
            RT2 = rt2[close.idx[good.idx,2]],
            Match = matchfactors[good.idx])
    } else {
      NULL
    }
  } else {
    NULL
  }
}

