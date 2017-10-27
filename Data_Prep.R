# Set directory
# store the factors universe under one folder, ie. "IBdata"
setwd("/sector model/IBdata")

# Read sector code
Mapping <- read.csv("Sector codes.csv", header = TRUE)

# Read the factors data
# Make sure all the factors named using a same pattern, ie. "US_WMRFACTORS"
filenames <- list.files(pattern = "US_WMRFACTORS*", full.names = TRUE)
flist <- lapply(filenames, read.csv)
names(flist) <-
  substr(filenames, 17, nchar(filenames) - 4) # all elements in filenames are "./US_WMRFACTORS_factorname.csv" (factorname starts from 17 char and ends 4th last char). Modify according to filenames

# Convert date to number of days & sector code to name
for (i in 1:length(flist)) {
  flist[[i]][, 1] <-
    as.numeric(as.Date(flist[[i]][, 1], "%m/%d/%Y")) - as.numeric(as.Date("1899-12-30"))
  names(flist[[i]]) <- c("DATE", as.character(Mapping[[2]]))
}

# Apply rolling window and date adjuster
all_sectors <-
  c(
    "AUCO",
    "BANKX",
    "CPGS",
    "COMS",
    "CODU",
    "HOTR",
    "DIVF",
    "ENRSX",
    "FDBT",
    "FDSR",
    "HCES",
    "HOUS",
    "INSU",
    "MATRX",
    "MEDA",
    "PHRM",
    "REAL",
    "RETL",
    "SSEQX",
    "SFTW",
    "TECH",
    "TELSX",
    "TRAN",
    "UTILX"
  )

ex_sectors <- c("REAL", "SSEQX", "BANKX")
sector_list <- all_sectors[!all_sectors %in% ex_sectors]

# master list store factor z & z of z
mlist = {
}

for (i in 1:length(flist)) {
  flist[[i]][, 1] <- dateadjuster.function2(flist[[i]], 5)
  temp <- rolling_window_v(flist[[i]], 10, 2600, sector_list)
  mlist[[i]] <- temp$z
  mlist[[i + length(flist)]] <- temp$zz
  
}

# Read the index data
Sectorindex <- read.csv("US_WMRSP500.csv", header = TRUE)

# Convert date to number of days
Sectorindex[, 1] <-
  as.numeric(as.Date(Sectorindex[, 1], "%m/%d/%Y")) - as.numeric(as.Date("1899-12-30"))

# Convert sector code to name
names(Sectorindex) <- c("DATE", as.character(Mapping[[2]]), "SPX")

# Generate index fwd return
Sectorindex_R <- generate_returns_all(Sectorindex) # forward returns

# Add index forward return data to the master list
mlist[[length(mlist) + 1]] <- Sectorindex_R

# name the master list ####
names(mlist) <-
  c(paste(substr(filenames, 17, nchar(filenames) - 4), "_z", sep = ""),
    paste(substr(filenames, 17, nchar(filenames) - 4), "_zz", sep = ""),
    "Sectorindex_R")
