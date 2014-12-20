
# Download and unzip file
if (!file.exists("ReproResearch2")) {
    dir.create("ReproResearch2")
}

link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destin <- "./ReproResearch2/storm_data.csv.bz2"
downfile <- download.file(link, destfile = destin)

# Read CSV file
storm <- read.csv(bzfile("./ReproResearch2/storm_data.csv.bz2"))

# Identify and clean the event types information
## 1. Combine events mentioned in different cases (e.g. TORNADO and Tornado)
## 2. Combine events mentioned with different punctuation (e.g. 'waterspout tornado' and 'waterspout/tornado')
## 3. Remove leading and trailing space
length(unique(storm$EVTYPE))    #Before cleaning of event type data

event_type <- tolower(storm$EVTYPE)
event_type <- gsub("[[:blank:][:punct:]+]", " ", event_type)
event_type <- gsub("^\\s+|\\s+$", "", event_type)
storm$EVTYPE <- event_type

length(unique(storm$EVTYPE))    #After cleaning of event type data

# Sum casualties and injuries by event type into separate data sets
library(plyr)
casualties <- ddply(storm, .(EVTYPE), summarize, sum_fatality = sum(FATALITIES),
                    sum_injuries = sum(INJURIES))

fatal_events = casualties[order(casualties$sum_fatality, decreasing = T), ]
injury_events = casualties[order(casualties$sum_injuries, decreasing = T), ]


# Data carries units in different columns "PROPDMGEXP" and "CROPDMGEXP"
# Hence the power of exponent needs to be calculated separately
# h -> Hundred (10^2), k -> Thousand (10^3), m -> Million (10^6), b -> Billion (10^9)
pow_exponent <- function(e) {
    if (e %in% c('h', 'H')) return(2)
    else if (e %in% c('k', 'K')) return(3)
    else if (e %in% c('m', 'M')) return(6)
    else if (e %in% c('b', 'B')) return(9)
    else if (!is.na(as.numeric(e))) return(as.numeric(e))   #If Digit
    else if (e %in% c('', '-', '?', '+')) return(0)         #If Space of Special Character
    else stop("Invalid exponent value.")
}

# Calculate actal damage
prop_dmg_power <- sapply(storm$PROPDMGEXP, FUN=pow_exponent)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_power)
crop_dmg_power <- sapply(storm$CROPDMGEXP, FUN=pow_exponent)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_power)

# Calculate economic loss and remove events which caused zero loss
econ_loss <- ddply(storm, .(EVTYPE), summarize, prop_dmg = sum(prop_dmg), crop_dmg = sum(crop_dmg))
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]

prop_dmg_events <- econ_loss[order(econ_loss$prop_dmg, decreasing = T), ]
crop_dmg_events <- econ_loss[order(econ_loss$crop_dmg, decreasing = T), ]

top20_fatal_events <- fatal_events[1:20,]
top20_injury_events <- injury_events[1:20,]
top20_property_dmg_events <- prop_dmg_events[1:20,]
top20_crop_dmg_events <- crop_dmg_events[1:20,]

# Plot Top 20 Fatalilty causing Events

library(ggplot2)
library(gridExtra)

g1 <- ggplot(data=top20_fatal_events, aes(x = reorder(EVTYPE, sum_fatality), y = sum_fatality)) + 
    geom_bar(stat="identity") + ylab("Total number of fatalities") + 
    xlab("Top 20 Fatalilty causing Events") + coord_flip()

# Plot Top 20 Injury causing Events

g2 <- ggplot(data=top20_injury_events, aes(x = reorder(EVTYPE, sum_injuries), y = sum_injuries)) +
    geom_bar(stat="identity") + ylab("Total number of injuries") +
    xlab("Top 20 Injury causing Events") + coord_flip()

grid.arrange(g1, g2, main="Top Fatal and Injury-causing events in the US (1950-2011)")

# Plot Top 20 Events by their Economic Impact on Property
g3 <- ggplot(data=top20_property_dmg_events, aes(x = reorder(EVTYPE, prop_dmg), y = log10(prop_dmg))) + 
    geom_bar(stat="identity") + ylab("Total Economic Damage to Property (log Base 10 Scale)") + 
    xlab("Top 20 Events") + coord_flip()

# Plot Top 20 Events by their Economic Impact on Crops
g4 <- ggplot(data=top20_crop_dmg_events, aes(x = reorder(EVTYPE, crop_dmg), y = crop_dmg)) + 
    geom_bar(stat="identity") + ylab("Total Economic Damage to Crops") + 
    xlab("Top 20 Events") + coord_flip()

grid.arrange(g3, g4, main="Top events by their economic impact in the US (1950-2011)")