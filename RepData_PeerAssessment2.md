# RepData_PeerAssessment2.Rmd
Jennifer Campbell  
June 9, 2017  

# Impact of Severe Weather Events on the United States

## Synopsis
This analysis investigates the impact of severe weather events on the United States, specifically their effect on public health and their economic consequences. Raw data is provided by the U.S. National Oceanic and Atmospheric Administration's (NOAA). The findings below rank the top ten weather events by total deaths, injuries, and damages done to property and to crops. The results show that some storm events have significant impact in both domains, causing both high rates of death and injuries as well as property and crop damage, such as is seen with Tornadoes and Thunderstorm Wind.  While Floods and Flash Flooding appear across all the highest ranked storms for both fatalities/injuries and damages, they show their consequences most strongly among the highest damaging storms to property, where Floods have a total over an order of magnitude higher than the damages done to crops. Other weather events are more influential in one domain or another, for example Excessive Heat showing the highest fatality rates though not appearing at all in the top ten causes of economic damages, and Drought which is by far the greatest cause of crop damage while not appearing in any of the highest ranks of the other categories. 


## Data Processing
In the first step of the analysis, check whether the input data file activity.csv is in the working directory, and then download and unzip the file if desired (using bunzip2 from the R.utils package is not strictly necessary, as read.csv can read straight from the compressed file, and this step was not significantly faster when the file was decompressed first):


```r
if(!file.exists("storm.csv.bz2")){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, destfile='storm.csv.bz2', method="wininet", mode="wb")
}
```

Then we can read in the data:  

```r
df <- read.csv("storm.csv.bz2", 
               na.strings = "NA", header = TRUE)
dim(df)
```

```
## [1] 902297     37
```

```r
names(df)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

There are a total of 902,297 observations of 37 variables in this dataset.  

### Wrangling the Data
In order to work more efficiently, we will subset the data to only include those years and variables which we are most interested in.  
The data include events from 1950 to 2011, but in the earlier years, there are generally fewer events recorded due to a lack of good records, as shown in the histogram of Total Storm Events by Year:


```r
df$Year <- as.numeric(format(as.Date(df$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
hist(df$Year, main="Total Storm Events by Year", xlab="Year")
```

![](RepData_PeerAssessment2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

We can see a marked increase in the number of recorded storm events after the year 1990. Therefore, to take advantage of the best records, we will focus our examination on the subset of storm data between 1990 and 2011.

In examining our questions, the variables which we will be using in this dataset are:

### Codebook  
| Field | Description|
|:--------|:---------------------|
|EVTYPE | The type of weather event (i.e. flood, tornado; a full listing can be found on page 6 of the  [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) under section 2.1.1 Storm Data Event Table) <br> Factor  variable; No missing ("NA") values|
|FATALITIES | The number of fatalities caused by the weather event <br> Numeric variable, Range: 0-583; No missing values|
|INJURIES | The number of injuries caused by the weather event <br> Numeric variable, Range: 0-1568; No missing values|
|PROPDMG | The estimated property damage caused by the weather event in Dollars <br> Numeric variable, No missing values|
|PROPDMGEXP | The order of magnitude of property damage <br> Factor variable|
|CROPDMG | The estimated crop damage caused by the weather event in Dollars <br> Numeric variable, No missing values|
|CROPDMGEXP | The order of magnitude of crop damage <br> Factor variable|

Thus, the new subsetted dataframe can be defined:


```r
stormdata <- df[df$Year %in% c(1990:2011), ]
variables <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                "CROPDMG", "CROPDMGEXP")
stormdata <- stormdata[variables]
```

so that we are now working with 751,740 observations of 7 variables.

The final step in preparing the data will be to clean up some of the text variables. As visible when using `summary()` on the EVTYPE factor variable, many of the names have been entered with character variations such as lowercase and plural usage, or the abbreviation "TSTM" for thunderstorm. Making these entries more consistent will provide a better overall picture of the health and economic data as functions of the type of weather event.


```r
stormdata$EVTYPE <- toupper(stormdata$EVTYPE)
stormdata$EVTYPE <- sub("TSTM", "THUNDERSTORM", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("S$", "", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("WINDS", "WIND", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("FLOODING", "FLOOD", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("FLD", "FLOOD", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("SML", "SMALL", stormdata$EVTYPE)
stormdata$EVTYPE <- sub("WIND/HAIL", "WIND HAIL", stormdata$EVTYPE)
stormdata$EVTYPE <- as.factor(stormdata$EVTYPE)
```


## Question: Across the United States, which types of events are most harmful with respect to population health?
  
The impact of storm events on population health can be described in terms of the numbers of fatalities and injuries caused. Here, we will look at the sum of all fatalities and the sum of all injuries for each weather event type, and rank the top 10:


```r
library(stats)
library(plyr)
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

topFatalities
```

```
##            EventType Fatalities
## 1     EXCESSIVE HEAT       1903
## 2            TORNADO       1752
## 3        FLASH FLOOD        999
## 4               HEAT        937
## 5          LIGHTNING        816
## 6        RIP CURRENT        572
## 7  THUNDERSTORM WIND        524
## 8              FLOOD        476
## 9          HIGH WIND        283
## 10         AVALANCHE        224
```

```r
topInjuries
```

```
##            EventType Injuries
## 1            TORNADO    26674
## 2  THUNDERSTORM WIND     7422
## 3              FLOOD     6791
## 4     EXCESSIVE HEAT     6525
## 5          LIGHTNING     5230
## 6               HEAT     2100
## 7          ICE STORM     1975
## 8        FLASH FLOOD     1785
## 9          HIGH WIND     1439
## 10      WINTER STORM     1338
```
 
These tables show us that the weather events with the highest impact (as determined by the highest total numbers of deaths and injuries caused) include Excessive Heat, Tornadoes, Thunderstorm Winds, Flash Floods and Lightning, and that these two lists of events hold most of their members in common.  

 
## Question: Across the United States, which types of events have the greatest economic consequences?
 
The economic consequences of weather events can be quantified by the estimated property and crop damage.   

We will first convert the property damage and crop damage data into comparable numerical forms according to the units of magnitude described in the [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) under section 2.7 Damage. The `PROPDMGEXP` and `CROPDMGEXP` columns record a multiplier for each observation, including "K" for thousands, "M" for millions, and "B" for billions. These exponent columns will have to be cleaned up into numeric forms we can use, and we will assume any missing or non-alphanumeric values are 0.


```r
unique(stormdata$PROPDMGEXP)
```

```
##  [1]   M K B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(stormdata$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
stormdata$PROPDMGEXP<- toupper(stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP<- toupper(stormdata$CROPDMGEXP)

stormdata$PROPDMGEXP <- sub("H", 2, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("K", 3, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("M", 6, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- sub("B", 9, stormdata$PROPDMGEXP)
stormdata$PROPDMGEXP <- as.numeric(stormdata$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
stormdata$PROPDMGEXP[is.na(stormdata$PROPDMGEXP)] = 0  ##Assume non-alphanumeric values of exp are 0, bad data Quality

stormdata$CROPDMGEXP <- sub("H", 2, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("K", 3, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("M", 6, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- sub("B", 9, stormdata$CROPDMGEXP)
stormdata$CROPDMGEXP <- as.numeric(stormdata$CROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
stormdata$CROPDMGEXP[is.na(stormdata$CROPDMGEXP)] = 0  ##Assume non-alphanumeric values of exp are 0, bad data Quality
```

Then we can write function to apply the exponents to the amounts of property and crop damage:


```r
calcDamage <- function(value,exp){value * (10^exp)}

stormdata$PropertyDamage <- mapply(calcDamage, stormdata$PROPDMG, stormdata$PROPDMGEXP)
summary(stormdata$PropertyDamage)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 0.000e+00 0.000e+00 5.344e+05 1.000e+03 1.150e+11
```

```r
stormdata$CropDamage <- mapply(calcDamage, stormdata$CROPDMG, stormdata$CROPDMGEXP)
summary(stormdata$CropDamage)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 0.000e+00 0.000e+00 6.532e+04 0.000e+00 5.000e+09
```

Finally, we can assess the economic impact of storm events in terms of the total amounts (in Dollars) of property and crop damage caused. Here, we will look at the sum of all property damage and the sum of all crop damage for each weather event type, and rank the top 10:


```r
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
```

```
##            EventType PropertyDamage
## 1              FLOOD   144772555807
## 2  HURRICANE/TYPHOON    69305840000
## 3        STORM SURGE    43323536000
## 4            TORNADO    30468735507
## 5        FLASH FLOOD    17139166083
## 6               HAIL    15735267513
## 7          HURRICANE    11868319010
## 8  THUNDERSTORM WIND     9914566476
## 9     TROPICAL STORM     7703890550
## 10      WINTER STORM     6688997251
```

```r
topCropDamage
```

```
##            EventType  CropDamage
## 1            DROUGHT 13972566000
## 2              FLOOD  5670873950
## 3        RIVER FLOOD  5057484000
## 4          ICE STORM  5022113500
## 5               HAIL  3025954473
## 6          HURRICANE  2741910000
## 7  HURRICANE/TYPHOON  2607872800
## 8        FLASH FLOOD  1436433150
## 9       EXTREME COLD  1312973000
## 10 THUNDERSTORM WIND  1159564738
```

These tables show us that the weather events with the highest economic impact (as determined by the highest total damages caused to property and crops) include Floods, Hurricane/Typhoons, Tornadoes and Hail. While these two lists of events hold most of their members in common, it is notable that the top Crop Damaging event, Drought, doesn't appear on the Property Damage top 10 list, and it's over twice the next most damaging event.

 
## Results  

The following panel contains the bar plots of the weather events with the highest health impact by total deaths and injuries:


```r
par(mfrow = c(1,2), mar = c(12, 4, 2, 2), cex = 0.8)
barplot(topFatalities$Fatalities, names = topFatalities$EventType, las = 3,
        ylab = "Total Fatalities", main = "10 Most Fatal Weather Events")
barplot(topInjuries$Injuries, names = topInjuries$EventType,las = 3, 
        ylab = "Total Injuries", main = "10 Most Injurious Weather Events")
```

![](RepData_PeerAssessment2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The plot shows that the weather events with the most fatalities are Excessive Heat, Tornadoes, and Flash Floods. Tornadoes also account for the most injuries caused by a weather event, with a total higher than the next top four event types combined. It is also apparent that Injuries make up a much greater portion of the health consequences than Fatalities due to weather events. 

The following panel contains the plots of the weather events with the highest economic impact by total property and crop damage:


```r
par(mfrow = c(1,2), mar = c(12, 4, 2, 3), cex = 0.8)
barplot((topPropDamage$PropertyDamage/1000000000), names = topPropDamage$EventType, 
        las = 3, ylab = "Total Property Damage (Billions of $)", 
        main = "10 Most Property Damaging Weather Events")
barplot((topCropDamage$CropDamage/1000000000), names = topCropDamage$EventType, 
        las = 3, ylab = "Total Crop Damage (Billions of $)", 
        main = "10 Most Crop Damaging Weather Events")
```

![](RepData_PeerAssessment2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The plot shows that the weather events with the most property damage are Flood, Hurricane/typhoon, and Storm Surge. Floods also account for the most property damage caused by a weather event by more than twice the next highest event type. As previously observed, Drought is by far the most damaging to crops, followed by Floods and Ice Storms. It is also apparent that Property Damage makes up a much greater portion of the economic consequences (in Dollars) than Crop Damage due to weather events, as the highest totals of property damages are an order of magnitude higher than those of crop damages.


*End of analysis*
