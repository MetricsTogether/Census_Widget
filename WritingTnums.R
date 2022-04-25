##Creating TrueNumbers from Core ACS 5 Data
library(tidyverse)

age <- read.csv("CSV files/age_tract_2009-2019.csv")
race <- read.csv("CSV files/race_tract_2009-2019.csv")
ed <- read.csv("CSV files/education_tract_2012-2019.csv")
res <- read.csv("CSV files/residents_tract_2009-2019.csv")

# tnumtext <- function(data){
# paste0("Population of ",names(data[,i])," in State ",data$state," in ",county , " in tract ", tract, " in year ", year, " is ",column, "people"))
#}
#foreach (i in 5:length(entry),.combine) %do# {tnumtext()}

censustxt <- c(1:4339722)

tnumtext <- function(data){
  foreach(i =(5:length(data)),.combine='c') %do% {
    paste0("Population of ",names(data)[i]," in State ",data$state," in ",data$county , " in tract ", data$tract, " in year ", data$year, " is ",data[,i], " people")
  }
}
censustxt <- tnumtext(age)
write(censustxt,"census_age.txt")





"020100 tract of the 2009 census in Autauga county Alabama"