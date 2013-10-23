tomcatRwuiRunXCMS <- function (path=".", runAnnotation=TRUE){
# Main function of the tomcat-Rwui web-interface of the pipeline
#
# tomcatRwuiRunXCMS(path, runAnnotation=TRUE)
#
#
#
# test:
#   library(metaMS)
#   source('../Func/tomcatRwuiRunXCMS.R')
#
#   path <- '/mnt/data/DemoCDFdata/' # 8 samples, inluding QC STD blank
#   path <- '/mnt/data/testing/demo01/' # 3 samples
#   path <- '/mnt/data/DemoGCtripleQ/' # GC-MS
#
#   tomcatRwuiRunXCMS(path, runAnnotation=TRUE)
#


##################################################################

sink("logfile_xcms_output.txt") 


  # write web-link of location of results
   write("============================================\n", file = "process_info.txt", append = TRUE)
   t=as.character(getwd()) 
   txt="Please copy the following web-link to get your\nresults in case the Internet connection breaks.\nLink will be valid 24h after processing.\n"
   write(txt, file = "process_info.txt", append = TRUE)
     # get process ID
        t2 = strsplit(t, '/',fixed = TRUE);
        e = length(t2[[1]]); # end
        t3 = t2[[1]][e-1] # process ID
     t4prefix = "http://fempc0125.intra.ismaa.it:8008/xcms/Results.jsp?resSubID="
     txt=as.character(paste(c(t4prefix,as.character(t3)),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE)
       txt=as.character(paste(c("metabo-server: ",as.character(t)),collapse=''))
       write(txt, file = "process_info.txt", append = TRUE)
   write("\n============================================\n", file = "process_info.txt", append = TRUE)



  # convert windows into unix path
  path <- getRwuiUnixPath(path)


  # split into LC or GC run
  runXCMS(path,runAnnotation=runAnnotation)

   
sink()          

}

