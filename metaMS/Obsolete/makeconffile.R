## Comment RW:
## No need to save this in an RData file. If you build the library all
## commands in this file will be executed so all lists will be
## available as global variables. No need to load the RData file,
## either... Eventually this should be part of the Mylonas system

## RW 2:
## It would be much simpler to use the instName as the name of the
## list element so that we can use metaMSsettings[[instrumentCode]]
## everywhere!

## RW 3: why only positive? Do you want to add settings for negative?
## Are these settings really different? If not then polarity is not
## really a setting!

Synapt.QTOF.NP <- list(instName = "Synapt.QTOF.NP",
                       chrom = "LC",
                       ## ----------------------------------
                       PeakPicking = list(
                         method = "matchedFilter",
                         step = 0.050,
                         fwhm = 20,
                         snthresh = 4,
                         max = 50,
                         nSlaves = 4),
                       ## ----------------------------------      
                       Alignment = list(
                         min.class.fraction = .3,
                         min.class.size = 3,
                         # minfrac=0,
                         # minsamp = 1, 
                         mzwid = 0.1,
                         bws = c(130,10),
                         missingratio = 0.2,
                         extraratio = 0.1,
                         retcormethod = "linear",
                         retcorfamily = "symmetric",            
                         fillPeaks = TRUE),
                       ## ----------------------------------
                       CAMERA = list(
                         perfwhm = 0.6,
                         cor_eic_th = 0.7,
                         ppm= 5),
                       ## ----------------------------------
                       match2DB = list(
                         rttol = 1.5,
                         rtval = 0.1,
                         mzwindow = 0.005,
                         ppm = 5,
                         minfeat = 2),
                       DBconstruction = list(
                         minfeat = 3,
                         rttol = 0.3,
                         mztol = 0.01)
                       
)



Synapt.QTOF.RP<- list(instName = "Synapt.QTOF.RP",
                      chrom = "LC",
                      ## ----------------------------------
                      PeakPicking = list(
                        method = "matchedFilter",
                        step = 0.05,
                        fwhm = 20,
                        snthresh = 4,
                        max = 50,
                        nSlaves = 4),
                      ## ----------------------------------      
                      Alignment = list(
                        min.class.fraction = .3,
                        min.class.size = 3,
                        # minfrac=0,
                        # minsamp = 1, 
                        mzwid = 0.1,
                        bws = c(30,10),
                        missingratio = 0.2,
                        extraratio = 0.1,
                        retcormethod = "linear",
                        retcorfamily = "symmetric",            
                        fillPeaks = TRUE),
                      ## ----------------------------------
                      CAMERA = list(
                        perfwhm = 0.6,
                        cor_eic_th = 0.7,
                        ppm= 5),
                      ## ----------------------------------
                      match2DB = list(
                        rttol = 1.5,
                        rtval = 0.1,
                        mzwindow = 0.005,
                        ppm = 5,
                        minfeat = 2),
                      DBconstruction = list(
                        minfeat = 3,
                        rttol = 0.3,
                        mztol = 0.01)
)



ThermoXLS.QQQ.GC <- list("instName" = "ThermoXLS.QQQ.GC",
                         "chrom" = "GC",
                         PeakPicking = list(
                             method = "matchedFilter",
                             step = 0.5,
                             steps = 2,
                             mzdiff = .5,
                             fwhm = 5,
                             snthresh = 2,
                             max = 500,
                             nSlaves = 4
                             ),
                         CAMERA = list(
                             perfwhm = 1
                             ),                             
                         ## ----------------------------------
                         DBconstruction = list(
                             minintens = 0.0, ## fraction of largest I
                             rttol = .1,
                             intensity.measure = "maxo",
                             DBthreshold = .80, 
                             minfeat = 5
                             ),
                         ## ----------------------------------
                         match2DB = list(
                             annotationThreshold = 0.80,
                             rtdiff = .5,
                             minfeat = 2
                             ),
                         ## ----------------------------------
                         matchIrrelevants = list(
                             irrelevantClasses = c("Bleeding",
                                 "Plasticizers"),
                             annotationThreshold = 0.70,
                             rtdiff = 1e5       ## dont use rt...
                             ),
                         ## ----------------------------------
                         betweenSamples = list(
                             min.class.fraction = .5,
                             min.class.size = 5,
                             rtdiff = .05,     
                             simthresh = .95)        ##,
                         ## ----------------------------------
                         ## msp = list(
                         ##     minintens = .10,  
                         ##     minPeaks = 3,
                         ##     mz.digits = 0,
                         ##     convertRtOutput = FALSE)
                         )              

FEMsettings <- list("Synapt.QTOF.NP" = Synapt.QTOF.NP,
                    "Synapt.QTOF.RP" = Synapt.QTOF.RP,
                    "ThermoXLS.QQQ.GC" = ThermoXLS.QQQ.GC)

## save(metaMSsettings, file = "metaMSsettings.RData")
