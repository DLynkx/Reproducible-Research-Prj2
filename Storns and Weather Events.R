## Load Library
library(dplyr)
library(ggplot2)
library(lubridate)


data.source <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"


if(!file.exists("StormData.csv.bz2")){
        download.file(data.source,"./StormData.csv.bz2")
}

stormdata <- read.csv("StormData.csv.bz2")


## Sub setting 
stormdata = select(stormdata, REFNUM, BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

## Transformations 
## Data and time fields
stormdata$BGN_DATE <- as.Date(mdy_hms(stormdata$BGN_DATE))


## PROPDMGEXP and CROPDMGEXP Fields
for (i in c("H", "K", "M", "B", "h", "k", "m", "b")) {
        if( i == "H" | i == "h"){
                j = 100} else if ( i == "K" | i == "k"){
                j = 1000
                } else if ( i == "M" | i == "m"){
                j = 1000000
                } else if ( i == "B" | i == "b"){
                j = 1000000000
                } else {
                
        }
stormdata$PROPDMGEXP <- gsub(i, j, stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP <- gsub(i, j, stormdata$CROPDMGEXP)
}

head(stormdata)


## Health and economic value considerations
stormdata$FATALITIES <- as.numeric(stormdata$FATALITIES)
stormdata$INJURIES <- as.numeric(stormdata$INJURIES)
stormdata$PROPDMG <- as.numeric(stormdata$PROPDMG)
stormdata$CROPDMG <- as.numeric(stormdata$CROPDMG)
stormdata$PROPDMGEXP <- as.numeric(stormdata$PROPDMGEXP)
stormdata$CROPDMGEXP <- as.numeric(stormdata$CROPDMGEXP)

## Health
healthdata <- select(stormdata, -PROPDMG, -PROPDMGEXP, -CROPDMG, -CROPDMGEXP)
healthdata <- mutate(healthdata, HEALTHVALUE = FATALITIES + INJURIES)
healthdata$HEALTHVALUE <- as.numeric(healthdata$HEALTHVALUE)
healthdata <- filter(healthdata, is.na(HEALTHVALUE) == FALSE, HEALTHVALUE > 0)

## Economic
economicdata <- select(stormdata, -FATALITIES, -INJURIES)
economicdata <- mutate(economicdata, ECONOMICVALUE = PROPDMG*PROPDMGEXP + CROPDMG*CROPDMGEXP)
economicdata$ECONOMICVALUE <- as.numeric(economicdata$ECONOMICVALUE)
economicdata <- filter(economicdata, is.na(ECONOMICVALUE) == FALSE, ECONOMICVALUE > 0)



## Impact Analysis
healthimpact <- healthdata %>% group_by(EVTYPE) %>% summarise(IMPACT = sum(HEALTHVALUE)) %>% arrange(desc(IMPACT))
p <- quantile(healthimpact$IMPACT, probs = 0.9)[[1]]
healthimpact <- healthimpact[healthimpact$IMPACT > p, ]

economicimpact <- economicdata %>% group_by(EVTYPE) %>% summarise(IMPACT = sum(ECONOMICVALUE)) %>% arrange(desc(IMPACT))
p <- quantile(economicimpact$IMPACT, probs = 0.9)[[1]]
economicimpact <- economicimpact[economicimpact$IMPACT > p, ]

## Visualization
g <- ggplot(data = healthimpact, aes(x = reorder(EVTYPE,IMPACT), y = IMPACT))
g <- g + geom_bar(stat = "identity", colour = "black", fill = "darkred")
g <- g + labs(title = "Top 10% of weather events in USA with the worst health impact from 1950-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Number of fatalities and injuries", x = "Event Type")
g <- g + coord_flip()
print(g)

#plotting economic loss
g <- ggplot(data = economicimpact, aes(x = reorder(EVTYPE, IMPACT), y = IMPACT))
g <- g + geom_bar(stat = "identity", colour = "black", fill = "green")
g <- g + labs(title = "Top 10% of weather events in USA with the worst economic impact from 1950-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Cost of property and crop loss", x = "Event Type")
g <- g + coord_flip()
print(g)