install.packages("reshape")
library(UsingR);
data(galton);
library(reshape);
long<-melt(galton)
