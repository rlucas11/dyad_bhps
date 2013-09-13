# 

# Pull sex from master
sex <- getMasterVariable("sex")
cache('sex')

# Pull satisfaction data and recode missing
satVars <- paste("losat",c("","eo","fs","ft","hl","lc","nl","sf","yh"), sep="")
satData <- getVariables(satVars)
recodeString <- "-10:-1=NA"
satData[, satVars] <- lapply (satData[, satVars], iRecode, recodeString)
cache('satData')

# Pull partner numbers; identify those who have only one partner
partnerNumbers <- getVariables("hhpxid")
partnerNumbers$hhpxid <- recode(partnerNumbers$hhpxid, "''=NA")
partnerNumbers <- partnerNumbers[complete.cases(partnerNumbers),]
#Identify people who only have one partner
partnerCount <- ddply(partnerNumbers, .(xwaveid), function(d) length(unique(d$hhpxid)))
#Should result in 13189 people with just one partner
cache('partnerNumbers')
cache('partnerNumbersUnique')

# Pull marital status data; identify those who are married throughout
maritalStatus <- getVariables("mrcurr")
recodeString <- "'[-3] Dont know '=NA;'[-4] Refused/Not stated '=NA"
maritalStatus$mrcurr <- recode(maritalStatus$mrcurr, recodeString)
maritalStatusCount <- ddply(maritalStatus[complete.cases(maritalStatus),], .(xwaveid),
                            function(d) length(unique(d$mrcurr)))
maritalStatus <- merge(maritalStatus, maritalStatusCount, by="xwaveid")
married <- maritalStatus[which(maritalStatus$V1==1&maritalStatus$mrcurr=='[1] Legally married '), ]
married <- unique(married[,c("xwaveid","mrcurr")])
cache('married')


# Create file with couples data
couples <- merge(married, sex, by="xwaveid")
men <- couples[which(couples$sex=='[1] Male'),] # 3768 Men
women <- couples[which(couples$sex=='[2] Female'),] # 3742 Women

men <- merge(men, partnerNumbersUnique, by="xwaveid") # 3755 matches
names(women) <- c("hhpxid","mrcurr","sex")
women$hhpxid <- as.numeric(women$hhpxid)

finalCouples <- merge(men, women[,c("hhpxid","sex")], by="hhpxid") #3244 matches
finalCouples <- final[,1:2]
names(finalCouples) <- c("wid","hid")
cache('finalCouples')

##################################################################################
# Life Satisfaction
##################################################################################

# Create lifeSat file for analysis
lifeSat <- satData[,c("xwaveid","wave","losat")]
names(lifeSat) <- c("hid","wave","hls")
final <- merge(finalCouples, lifeSat, by = "hid")
names(lifeSat) <- c("wid","wave","wls")
lifeSat$wid <- as.numeric(lifeSat$wid)
final <- merge(final, lifeSat, by = c("wid", "wave"))
final <- final[order(final$hid,final$wave),]

# Create wide file for analysis
finalMelt <- melt(final[,c("hid","wave","hls","wls")], na.rm=TRUE,id.vars=c("hid","wave"))
finalLS <- dcast(finalMelt, hid ~ wave + variable, value.var="value")
names(finalLS) <- c("hid",paste(c("hls","wls"), rep(1:10, each=2), sep=""))
cache('finalLS')
# Create Mplus Data (using MplusAutomation package)
prepareMplusData(finalLS, filename="data/mplusLS.dat", dropCols=1)


##################################################################################
# Satisfaction Files for Couples
##################################################################################


varNames <- names(satData)[3:11]
hSat <- satData
names(hSat) <- c("hid","wave",paste("h",varNames,sep=""))
wSat <- satData
names(wSat) <- c("wid","wave",paste("w",varNames,sep=""))

finalSat <- merge(finalCouples, hSat, by = "hid")
wSat$wid <- as.numeric(wSat$wid)
finalSat <- merge(finalSat, wSat, by = c("wid", "wave"))
finalSat <- finalSat[order(finalSat$hid, finalSat$wave),]

#Create wide file for analysis
firstMelt <- melt(finalSat[,-1], na.rm=TRUE, id.vars=c("hid","wave"))
wideSatCouples <- dcast(firstMelt, hid ~ wave + variable, value.var = "value")

cache('wideSatCouples')

