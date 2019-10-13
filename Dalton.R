# load necessary packages/install if needed
library(ggplot2); library(UsingR); data(galton) 

library(dplyr)
# constructs table for different combination of parent-child height
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child (in)", "parent (in)", "freq")
# convert to numeric values
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
# filter to only meaningful combinations
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = mean(child))
g <- g + scale_size(range = c(2, 20), guide = "none" )
# plot grey circles slightly larger than data as base (achieve an outline effect) 
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size = freq))
# change the color gradient from default to lightblue -> $white
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g

# calc mean child height for every parent height
t <- aggregate(galton$child, list(parent = galton$parent), mean)
names(t) <- c("parent","child")

# plot regression
g <- g + geom_smooth(method="lm", formula=y~x)
g