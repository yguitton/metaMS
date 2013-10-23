stdDB_GC1_readStdInfo <- function (CDFstandardsDir = '.'){

# CDFstandardsDir <- "/remote/watersnew/GardaLand/grape_metabolome/GC/Standards" # <<<<<
# CDFstandardsDir <- "CDFstandards" # <<<<<

# options(echo = FALSE)

### New 2013 version (Shrek), containing much more information,
## including chemspider IDs, date (currently empty), NIST (yes or no),
## comment, MonoMW and SMILES
huhn <- read.table("standardsInfo.csv", sep = ";", head = TRUE,
                   dec = ",", as.is = TRUE)

## Check if all filenames correspond to real CDF files
# CDFstandardsDir <- "/mnt/GardaLand/grape_metabolome/GC/Standards" # <<<
fnames <- unique(paste(CDFstandardsDir, "/", huhn[,"cdf"], ".cdf", sep = "")) # <<<
if (all(file.exists(fnames)) ) {
  cat("\nAll cdf files containing standards present! Good.\n")
} else {
  cat("\nThe following cdf files of standards are missing:\n")
  cat(paste(huhn[!file.exists(fnames), "cdf"], collapse = "\n\t"))
}

## Check if there are no superfluous cdf files in the directory
allcdfs <- unlist(strsplit(list.files(CDFstandardsDir, pattern = ".cdf"), ".cdf")) # <<<
if (all(allcdfs %in% huhn[,"cdf"])) {
  cat("\nNo superfluous cdfs found in CDF directory. Good.\n")
} else {
  cat("\nThe following cdf files of standards are not used:\n")
  cat(paste(allcdfs[!(allcdfs %in% huhn[,"cdf"])], collapse = "\n\t"))
}

## Temporary fix to remove those standards with the word "artefact" in
## the "comment" column...
## huhn <- huhn[!huhn[,"comment"] == "artefact",]

huhn[, "RTman"] <- as.numeric(huhn[,"RTman"])
huhn[, "monoMW"] <- as.numeric(huhn[,"monoMW"])

stdFileInfo <- huhn
save(stdFileInfo, file = "stdFileInfo.RData")

cat("\nAny previously present stdFileInfo.RData has been overwritten.\n\n")

## June 20:
## 350 standards in 178 injections

## Dec 10:
## 368 standards in 190 injections

}
