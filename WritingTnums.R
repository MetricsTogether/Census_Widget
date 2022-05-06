##Creating TrueNumbers from Core ACS 5 Data
library(tidyverse)
#For files, please run PullingData.R 



# age <- read.csv("CSV files/age_tract_2009-2019.csv")
# race <- read.csv("CSV files/race_tract_2009-2019.csv")
# ed <- read.csv("CSV files/education_tract_2012-2019.csv")
# res <- read.csv("CSV files/residents_tract_2009-2019.csv")

# tnumtext <- function(data){
# paste0("Population of ",names(data[,i])," in State ",data$state," in ",county , " in tract ", tract, " in year ", year, " is ",column, "people"))
#}
#foreach (i in 5:length(entry),.combine) %do# {tnumtext()}


## this is the old way of doing things, ignore it 
# censustxt <- c(1:31073)
# 
# tnumtext <- function(data){
#   foreach(i =(5:length(data)),.combine='c') %do% {
#     #paste0("Population of ",names(data)[i]," in State ",data$state," in ",data$county , " in tract ", data$tract, " in year ", data$year, " is ",data[,i], " people")
#     
#   }
# }
# censustxt <- tnumtext(ageIL)
# write(censustxt,"census_age.txt")


library(tnum)
library(jsonlite)
creds <- "alison@metricstogether.com:brushfire"
ip <- "metrics.truenum.com:8080"
tnum.authorize(ip=ip,creds=creds)
tnum.setSpace("MetricsTogether")

age_small_19 <- age_small[age_small$year==2019,]

tnum.ingestDataFrame(df=age_small_19,subjectRoot = "",subjectTerms = list(c("","county"),c(":tract:", "tract")),tag="data:USFederal:ACS5:2019" )



#illinois only for Restore Justice

ageIL <- age_small %>% filter(state=="Illinois")

ageIL_2019 <- ageIL %>% filter(year==2019)

tnum.ingestDataFrame(df=head(ageIL_2011),subjectRoot = "Illinois/",subjectTerms = list(c("","county"),c(":tract:", "tract")),tag="USFederal:ACS5:2011" )



"020100 tract of the 2009 census in Autauga county Alabama"

age_small_19 <- age_small%>%filter(year==2019)
tnum.ingestDataFrame(df=(age_small_19),subjectRoot = paste0(state,"/"),
                     subjectTerms = list(c("","county"),c(":tract:", "tract")),
                     propTerms = list(c()),
                     tag="USFederal:ACS5:2011",
                     outfile = "test.txt")
