##Ingest with the new function - test data (acs5_race) can be pulled with the PullingData R script
library(tnum)
library(jsonlite)

creds <- "alison@metricstogether.com:brushfire"
ip <- "metrics.truenum.com:8080"
tnum.authorize(ip=ip,creds=creds)
tnum.setSpace("MetricsTogether")

#this is the original way I pulled the data from Pulling Data. I created a csv of this information for ease of sharing
# set.seed(123)
# race_sample <- acs5_race[sample(1:723287,10),]
race_sample <- read.csv(race_sample)

templates <- list(
  #questions for allen
    #1. previously we made the state the subject path, how can we do that now?
    #2. How do we put in tags?
    #3. Is there a way to use the column headers as the name? ie instead of writing "white alone population" pull that from the column name?
  
  c("$(county) $(tract) has white alone population = $`white alone`")
)

#this is the desired result Appling_County:tract:950100 has Over_65_male_population = 258 -- tag==data:USFederal:ACS5:YEAR*
tnum.ingestDataFrame(head(race_sample),templates,"testIngest1.txt")
