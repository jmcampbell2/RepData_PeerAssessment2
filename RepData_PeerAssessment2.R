#repdata_peerassessment2
#code for reproducible data project 2
#author: Jennifer Campbell
#data: May 29, 2017-June 4, 2017
#system: Windows 10

##read in data
if(!file.exists("storm.csv.bz2")){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, destfile='storm.csv.bz2', method="wininet", mode="wb")
}
df <- read.csv("storm.csv.bz2", 
               na.strings = "NA", header = TRUE)
dim(df)
names(df)
##subsetting

df$Year <- as.numeric(format(as.Date(df$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
hist(df$Year, main="Total Storm Events by Year", xlab="Year")

stormdata <- df[df$Year %in% c(1990:2011), ]
variables <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                "CROPDMG", "CROPDMGEXP")
stormdata <- stormdata[variables]

##cleaning
stormdata$EVTYPE <- toupper(stormdata$EVTYPE)
stormdata$EVTYPE <- sub("TSTM", "THUNDERSTORM", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("S$", "", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("WINDS", "WIND", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("FLOODING", "FLOOD", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("FLD", "FLOOD", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("SML", "SMALL", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("WIND/HAIL", "WIND HAIL", stormdata$EVTYPE)
stormdata$EVTYPE <- as.factor(stormdata$EVTYPE)

##
sumFatalities <- aggregate(stormdata$FATALITIES, 
                           by = list(stormdata$EVTYPE), FUN = "sum")
sumFatalities <- arrange(sumFatalities, sumFatalities[, 2], decreasing = T)
sumInjuries <- aggregate(stormdata$INJURIES, 
                           by = list(stormdata$EVTYPE), FUN = "sum")
sumInjuries <- arrange(sumInjuries, sumInjuries[, 2], decreasing = T)

topFatalities <- head(sumFatalities, n = 10)
topInjuries <- head(sumInjuries, n = 10)
names(topFatalities) <- c("EventType", "Fatalities")
names(topInjuries) <- c("EventType", "Injuries")

##

unique(stormdata$PROPDMGEXP)
unique(stormdata$CROPDMGEXP)
stormdata$PROPDMGEXP<- toupper(stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP<- toupper(stormdata$CROPDMGEXP)

stormdata$PROPDMGEXP <- sub("H", 2, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("K", 3, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("M", 6, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("B", 9, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- as.numeric(stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP[is.na(stormdata$PROPDMGEXP)] = 0  ##Assume non-alphanumeric values of exp are 0, bad data Quality

stormdata$CROPDMGEXP <- sub("H", 2, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("K", 3, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("M", 6, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("B", 9, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- as.numeric(stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP[is.na(stormdata$CROPDMGEXP)] = 0  ##Assume non-alphanumeric values of exp are 0, bad data Quality

calcDamage <- function(value,exp){value * (10^exp)}
stormdata$PropertyDamage <- mapply(calcDamage, stormdata$PROPDMG, stormdata$PROPDMGEXP)
summary(stormdata$PropertyDamage)
stormdata$CropDamage <- mapply(calcDamage, stormdata$CROPDMG, stormdata$CROPDMGEXP)
summary(stormdata$CropDamage)

sumPropDamage <- aggregate(stormdata$PropertyDamage, 
                           by = list(stormdata$EVTYPE), FUN = "sum")
sumPropDamage <- arrange(sumPropDamage, sumPropDamage[, 2], decreasing = T)
sumCropDamage <- aggregate(stormdata$CropDamage, 
                           by = list(stormdata$EVTYPE), FUN = "sum")
sumCropDamage <- arrange(sumCropDamage, sumCropDamage[, 2], decreasing = T)

topPropDamage <- head(sumPropDamage, n = 10)
topCropDamage <- head(sumCropDamage, n = 10)
names(topPropDamage) <- c("EventType", "PropertyDamage")
names(topCropDamage) <- c("EventType", "CropDamage")

topPropDamage
topCropDamage

##Results
par(mfrow = c(1,2), mar = c(12, 4, 2, 2), cex = 0.8)
barplot(topFatalities$Fatalities, names = topFatalities$EventType, las = 3,
        ylab = "Total Fatalities", main = "10 Most Fatal Weather Events")
barplot(topInjuries$Injuries, names = topInjuries$EventType,las = 3, 
        ylab = "Total Injuries", main = "10 Most Injurious Weather Events")

par(mfrow = c(1,2), mar = c(12, 4, 2, 3), cex = 0.8)
barplot((topPropDamage$PropertyDamage/1000000000), names = topPropDamage$EventType, 
        las = 3, ylab = "Total Property Damage (Billions of $)", 
        main = "10 Most Property Damaging Weather Events")
barplot((topCropDamage$CropDamage/1000000000), names = topCropDamage$EventType, 
        las = 3, ylab = "Total Crop Damage (Billions of $)", 
        main = "10 Most Crop Damaging Weather Events")

