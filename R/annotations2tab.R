## convert a nested annotation list to a simple-to-read table.  both annlist and matches are for one injection only. Also
## works when there are no single hits, or no double hits
annotations2tab <- function(annlist, matches) {
    single.hits <- which(sapply(annlist, length) == 1)
    single.result <- makeAnnotation(length(single.hits))
    single.result[, "pattern"] <- single.hits
    single.result[, "annotation"] <- unlist(annlist[single.hits])
    
    hits <- which(sapply(annlist, length) > 1)
    nhits <- length(hits)
    if (nhits == 0) {
        single.result
    } else {
        double.result <- makeAnnotation(nhits)
        
        for (i in 1:nhits) {
            ann <- annlist[[hits[i]]]
            matchfactors <- matches[ann, hits[i]]
            bestone <- which.max(matchfactors)
            double.result[i, 1:2] <- c(hits[i], ann[bestone])
            double.result[i, 3] <- paste(ann[-bestone], collapse = ",")
        }
        
        rbind(single.result, double.result)
    }
}


makeAnnotation <- function(n) {
    data.frame(pattern = rep(0, n), annotation = rep(0, n), alternatives = rep("", n), stringsAsFactors = FALSE)
}
