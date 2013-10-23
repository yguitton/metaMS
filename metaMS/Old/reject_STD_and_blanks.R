reject_STD_and_blanks <- function (xset){
#
#  reject STD and blank samples from xset object
#
#   xset = reject_STD_and_blanks(xset)
#
#
         
# get sample IDs of STDmix and blanks
   id1 = getSamples(xset,"STD")
   id2 = getSamples(xset,"blank") 
   rmSamples <- c(id1,id2)

# reject samples from xset object
   xsetSplit <- split(xset, f = factor(xset@filepaths, levels = xset@filepaths)) # split xset object into list
   xset      <- xsetSplit[-rmSamples] # reject samples
   xset      <- do.call(c,xset) # combine


return(xset)
}

