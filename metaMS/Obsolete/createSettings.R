createSettings <- function() {
  instName <- readline("Name for the protocol (no spaces): ")
  
  chrom <- readline("Type of chromatography (LC or GC): ")
  
  ## perhaps we should leave this out and for the moment use
  ## matchedFilter?
  str <- "XCMS peakpicking method (matchedFilter or centWave): "
  ppmethod <- readline(str)
  
  mf <- switch(ppmethod,
               matchedFilter = {
                 str <- "step (matchedFilter) - enter a number: "
                 ppstep <- as.numeric(readline(str))
                 str <- "steps (matchedFilter) - enter a number: "
                 ppsteps <- as.numeric(readline(str))
                 
                 list(step = ppstep, steps = ppsteps)
               },
               centWave = {
                 str <- "ppm (centWave) - enter a number: "
                 cwppm <- as.numeric(readline(str))
                 str <- "snthresh (centWave) - enter a number: "
                 cwsnthresh <- as.numeric(readline(str))
                 
                 list(ppm = cwppm, snthresh = cwsnthresh)
               },
               stop("\nError: unknown peak picking method! Choose either centWave or matchedFilter.")
             )
  
  list(instName = instName,
       chrom = chrom,
       PeakPicking = c(list(method = ppmethod), mf))           
}
