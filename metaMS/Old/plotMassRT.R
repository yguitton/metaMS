plotMassRT <- function (X,outputfile="screen",filename="figure_mass_RT_plot.pdf"){
# Scatterplot of RT versus m/z 
#
# plotMassRT(xset)
# plotMassRT(xset, outputfile="pdf")
# plotMassRT(xset, outputfile="pdf", filename="figure_mass_RT_plot.pdf")
#
# outputfile can be set as "pdf"  (default: "screen") 
#
#
# test:
#   source("../Func/plotMassRT.R")
#   source("/home/scholz/Projects_IASMA/V07_R_library_pipeline/Functions/functionsPipeline.R")
#   source("/home/scholz/Projects_IASMA/V07_R_library_pipeline/Functions/plotMassRT.R")
#   load('xcmsXsetObject.RData')
#   plotMassRT(xset)
#   plotMassRT(xset,outputfile="pdf")
#
# 

##################################################

# if input is XSET object
   if (isS4(X)) {
    xset = X
    if (class(xset) == "xsAnnotate") xset <- xset@xcmsSet # in case of CAMERA object
    peakTable <- getPeakTable(xset)
   }

# if input is peak table
   if (is.data.frame(X)) {
    peakTable = X
   }


#####################################################

# plot 
   if  (outputfile == "pdf") {  
    pdf(file = filename,8.26,5.83) # size: A5
   }
   if  (outputfile == "screen") {     
    dev.new(width=8.26, height=5.83) # size: A5
   }
  

     plot(peakTable[,"rt"], peakTable[, "mz"],type = "p",
       main = "XCMS peak detection" , xlab = "Retention Time (min)", ylab = "m/z", col ="blue")
       # RT in peakTable is already converted to min
   
   if  (outputfile == "pdf") {  
    dev.off() # close pdf file
   }


}
