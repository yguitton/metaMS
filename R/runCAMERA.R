runCAMERA <- function(xset, chrom = c("LC", "GC"), settings, polarity, quick = TRUE) {
    chrom <- match.arg(chrom)
    
    switch(chrom, LC = {
        capture.output(z <- do.call(annotate, c(list(object = xset, polarity = polarity, quick = quick), settings)))
        
        z
    }, GC = {
        y <- xsAnnotate(xset, sample = 1)
        capture.output(z <- do.call(groupFWHM, c(list(object = y), settings)))
        
        z
    })
    
}
