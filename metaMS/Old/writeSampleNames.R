writeSampleNames <- function (xset){
# write sample names and number of features to process.info
#
# writeSampleNames(xset)

# test:
#  source('../Func/writeSampleNames.R')
#

##################################################################################

# require(xcms)

if (class(xset) == "xsAnnotate") xset <- xset@xcmsSet
  
   txt=as.character(paste(c("     number of samples: ",as.character(length(sampnames(xset)))),collapse=''))
   write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt="       name          \tfeatures"
   write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
 

# get sample names and feature-number

   xp.orig <- as.data.frame(peaks(xset))
   ## split: one list element for each sample
   xp <- split(xp.orig, xp.orig[,"sample"])
   # xp <- lapply(unique(xp.orig$sample), function(x) xp.orig[xp.orig$sample==x,])
   # head(xp[[1]])
   ## remove those peaks with NA in sn column (and also other columns)
   # !! xp$sn does not exist anymore
   # xp <- lapply(xp, function(x) x[!is.na(x$sn), c("rt", "mz", "into")])
    # sampnames(xset)[2]
    # nrow(xp[[2]])

   # write samplename and features
   tmp <- sapply(1:length(xp), function(i, xx) {
     txt=as.character(paste(c("       ",as.character(sampnames(xset)[i]),"\t",nrow(xp[[i]]) ),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   }, xp)


}

