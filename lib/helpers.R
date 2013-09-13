# -------------------- define paths ------------------------------ #

pathOriginalData <- "~/Documents/Datasets/HILDA10/"  # path of original panel data files, ending with "/"
pathWorking <- "../data/" # path where new files will be stored

# -------------------- study characteristics -------------------- #

firstYearOfStudy <- 2001
lastYearOfStudy <- 2010
startingWave <- 1

masterFile <- "Master_j100c.dta"
waveLabels <- letters[1:(lastYearOfStudy-firstYearOfStudy+1)]  # Wave names as letters
yearsToInclude <- seq(firstYearOfStudy,lastYearOfStudy)  # Years of Study
yearsToInclude2 <- substr(as.character(yearsToInclude), 3, 4)  # Two-digit year for some variable names
originalDataFile <- "Rperson_$100c.dta"
idName <- "xwaveid"
hID <- "hhrhid" # Needs wave prefix
partnerID <- "hhpxid" # Needs wave prefix; this is equivalent to the partner's cross-wave ID.
charsToSub <- "\\$"




getMasterVariable <- function(variableName, masterFileName=masterFile) {
    master <- read.dta(paste(pathOriginalData, masterFileName, sep = ""))
    return(master[,c(idName, variableName)])
}


getVariables <- function(variables, dataFileName=originalDataFile,
                         masterFileName=masterFile, wide=FALSE,
                         waves=waveLabels) {
    master <- read.dta(paste(pathOriginalData, masterFileName, sep = ""))
    master <- data.frame(master[,c(idName)], stringsAsFactors=FALSE)
    names(master) <- idName
    for (i in waves) {
        newDataFileName <- paste(pathOriginalData, sub(charsToSub, i, dataFileName),
                                 sep = "")
        newVariableNames <- paste(i, variables, sep = "")
        data <- read.dta(newDataFileName)
        data <- data[,c(idName, newVariableNames)]
        master <- merge(master, data, by = idName, all.x=TRUE)
    }
    if (wide==FALSE) {
        master <- melt(master, id.vars=idName)
        master$wave <- substring(master$variable,1,1)
        master$variable <- substring(master$variable,2)
        form <- as.formula(paste(idName, " + wave ~ variable", sep=""))
        master <- dcast(master, form, value.var="value")
    }
    return(master)
}

iRecode <- function (x, rstring) { recode (x, rstring) }
