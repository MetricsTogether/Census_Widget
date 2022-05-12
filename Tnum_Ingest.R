##Ingest with the new function - test data (acs5_race) can be pulled with the PullingData R script
library(tnum)
library(jsonlite)
library(tidyverse)

creds <- "alison@metricstogether.com:brushfire"
ip <- "metrics.truenum.com:8080"
tnum.authorize(ip=ip,creds=creds)
tnum.setSpace("MetricsTogether")

#this is the original way I pulled the data from Pulling Data. I created a csv of this information for ease of sharing
# set.seed(123)
# race_sample <- acs5_race[sample(1:723287,10),]
race_sample <- read_csv("race_sample.csv")


templates <- c()

for (i in 5:length(race_sample)) {
  templates[[(length(templates) + 1)]] <- c(paste0("tract $(tract) in $(county) has ",names(race_sample)[i],  " population = $(",names(race_sample)[i],")"),"data:USFederal:ACS5:$(year)")
  i <- i+1
}

template <- list(c("tract $(tract) in $(county) has white alone population = $(white alone)", "data:USFederal:ACS5:$(year)"))

#this is the desired result Appling_County:tract:950100 has Over_65_male_population = 258 -- tag==data:USFederal:ACS5:YEAR*
tnum.ingestDataFrame(head(race_sample),template,"testIngest1.txt")


