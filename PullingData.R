library(tidyverse)
library(tidycensus)
library(censusapi)
library(jsonlite)
library(foreach)
library(tigris)

## Read in variable information from Census API for descriptions, codes, and labels -- only for 2019 assuming the most recent year has the largest number of variables associated for ACS
acsvars2019 <- fromJSON("https://api.census.gov/data/2019/acs/acs5/variables.json")
#Very useful to scroll through variables at the acs api via the website as well 

var_desc <- data.frame(stringsAsFactors = FALSE,matrix(ncol=3,nrow=1))
for (i in 4:length(acsvars2019$variables)){
  var_desc <- data.frame(rbind(var_desc,c(names(acsvars2019$variables)[i],acsvars2019$variables[[i]]$label,acsvars2019$variables[[i]]$concept)),stringsAsFactors = FALSE)
}

rm(acsvars2019)
names(var_desc) <- c("VariableCode","Label","Concept")

#uni is the pulled universe of variables for all surveys, instances of time, and vintages from the Census API
#this can be pulled using the following code or with a large csv (see google drive)
#universe <- censusapi::listCensusApis()
#^ for each uni pull all vars
# universe <- censusapi::listCensusApis()
# 
# #Variable extraction does not work for pums data
# pums <- grepl("pums",universe$name)
# uni2 <- universe[pums==FALSE,]
# 
# 
# vars <- c()
# for (i in 1:length(uni2$title)){
#   t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
#   vars <- c(vars,list(t))
# }
# #did not run for line 86
# 
# 
# vars3 <- c()
# for (i in 87:800){
#   t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
#   vars3 <- c(vars,list(t))
# }
# uni <- rbind(vars,vars3)

uni <- read.csv("universe.csv")


key <-  "YOUR KEY"

#key variables for round 1 census pull are 
# ACS 5 yr
#Age
#race & hispanic
#Educational attainment
#Households (enumeration)
#Housing Units (ownership)
#commuter information
#resident occupations (workforce)


acs5 <- uni[uni$name=="acs/acs5",]
acs5 <- left_join(acs5,var_desc,by=c("vars"="VariableCode"))


## test to trunumbers using acs2019
# acs5_2019 <- acs5[acs5$vintage==2019,]
# sumry <- acs5_2019 %>% group_by(Concept) %>% summarise(varcount=n())
# tnumtext <- function(data){
#   foreach(i =(1:length(sumry$Concept)),.combine='c') %do% {
#     paste0("Total variables of '",sumry$Concept[i],"' is ",sumry$varcount[i])
#   }
# }
# 
# txt <- tnumtext(sumry)
# write(txt,"2019vars.txt")

##because we use the 2019 variables, the resulting acs5 dataset has 246170 NA in the "label" category. Assuming these are for variables that we will not use right away, I'm comfortable not addressing this issue, but it should be on the radar to fix later

var_freq <- acs5 %>% group_by(vars) %>% summarize(length(vars))
## function to retrieve acs years data
yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

## fuction to start the cleaning/naming process (fips is pre-loaded from censusapi)
fips_to_acs <- function(data){
  t <- left_join(data,fips,by=c("state"="state_code","county"="county_code"))
  t
}



##Age
#the Age variables all sit in the family of B01001
age_vars <- grep("B01001_",acs5$vars)
acs5_age_vars <- acs5[age_vars,10]
acs5_age_vars <- as.character(acs5_age_vars)
age_var_names <- unique(acs5_age_vars)

acs5_age <- foreach(i = rep(2009:2019,51), j = rep(1:51,10),.combine = 'rbind') %do% {
  yeargeog(year=i,state=j,vars=age_var_names)
}


acs5_age <- fips_to_acs(acs5_age)

#03-09 (under 21) Male
#27-33 (under 21) Female
#20-25 (over 65) Male
#44 - 49 (over 65) Female

age_small <- acs5_age %>% 
  mutate(over65m=B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E,
         over65f=B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E,
         under21m=B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_007E+B01001_008E+B01001_009E,
         under21f=B01001_027E+B01001_028E+B01001_029E+B01001_030E+B01001_031E+B01001_032E+B01001_033E) %>%
  select("year","state_name","county.y","tract","B01001_001E","B01001_002E","B01001_026E",over65m,over65f,under21m,under21f)
names(age_small) <- c("year","state","county","tract","Total population","Male population","Female population","Over 65 male population", "Over 65 female population","Under 21 male population", "Under 21 female population")

## Race
race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")
acs5_race <-foreach(i = rep(2009:2019,51), j = rep(1:51,10),.combine = 'rbind') %do% {
  yeargeog(year=i,state=j,vars=race_vars)
}

acs5_race <- fips_to_acs(acs5_race)
acs5_race <- acs5_race %>% select (year,state_name,county.y,tract,"B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E" )

names(acs5_race) <- c("year","state","county", "tract",
                      "white alone",
                      "black alone",
                      "american indian or alaska native alone",
                      "asian alone",
                      "native hawaiian or pacific islander alone",
                      "some other race alone",
                      "two or more races",
                      "white alone, not hispanic or latino",
                      "hispanic or latino, any race category"
)

#can cross check with B02001_001:10E
#B02008:14 introduces combinations (expansion of two or more category)



##Educational attainment

# for educational attainment I initially tried to do a more reproducible approach in using text to pull variable, I found this not very useful but it could be expanded on
ed_vars <- var_desc[grep("EDUCATION",var_desc$X3),]

#Educational variable names differ in years 2009-2011 and are thus excluded, can be included later
ed_vars2016 <- c(paste0("B15003_00",2:9,"E"),paste0("B15003_0",10:25,"E"))
ed_vars2009 <- "B15001_001E"

acs5_education <- foreach(i = rep(2012:2019,51), j = rep(1:51,8),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=ed_vars2016)
}

acs5_education <- fips_to_acs(acs5_race)


ed_small <- acs5_education %>% 
  mutate(noschooling=B15003_002E,
         primaryschoolnodiploma = B15003_003E+ B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+B15003_011E+B15003_012E+B15003_013E+B15003_014E
         + B15003_015E+B15003_016E,
         highschooldiploma= B15003_017E,
         somecollegenodegree= B15003_019E+B15003_020E
  ) %>%
  select("year","state_name","county.y","tract",noschooling,primaryschoolnodiploma,highschooldiploma,somecollegenodegree,B15003_021E,B15003_022E,B15003_023E,B15003_024E,B15003_025E)

names(ed_small) <- c("year","state","county","tract","noschooling","primaryschoolnodiploma","highschooldiploma","somecollegenodegree","associatesdegree","bachelorsdegree","masters degree","professionaldegree","doctoratedegree")


##Households (emmuneration)
## total households, family households, married with children, married without children, single parents, other, non-family households, living alone
#B09019_002E -  total (household type) 
#B09019_002E - In households

# B11001_001E total households
# B11001_002E family households
# B11001_003E married couple family
# B11001_004E nonmarried family (	Estimate!!Total:!!Family households:!!Other family:)
# B11001_005E single parent - Male
# B11001_006E single parent - Female
# B11001_007E non family household
# B11001_008E living alone 
# B11001_009E not living alone

res_vars <- c("B11001_001E", "B11001_002E" , "B11001_003E", "B11001_004E","B11001_005E", "B11001_006E", "B11001_007E", "B11001_008E", "B11001_009E" )
acs5_res <- foreach(i = rep(2009:2019,51), j = rep(1:51,10),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=res_vars)
}
acs5_res <- fips_to_acs(acs5_res) 
acs5_res <- acs5_res %>% select (year,state_name,county.y,tract,"B11001_001E", "B11001_002E" , "B11001_003E", "B11001_004E","B11001_005E", "B11001_006E", "B11001_007E", "B11001_008E", "B11001_009E" )

names(acs5_res) <- c("year","state","county","tract","total households","family households","married couple family","nonmarried family (other family)","single parent male","single parent female","nonfamily household","living alone","not living alone")


##Housing units (ownership)
## total housing units, owner occupied, renter occupied, vacant for seasonal or recreational use, 1-unit (attached or detached), 2-9 units, 10-19 units, 20 or more units, built prior to 1940
# B25001_001E  total housing units 
# B25002_002E Occupied
# B25002_003E Vacant
# B25003_002E Owner occupied
# B25003_003E Renter occupied
# B25004_006E vacant for sesional use

# B25032_003E + B25032_004E + B25032_014E + B25032_015E 1 unit (owner and renter occupied )
# B25032_005E:B25032_007E + B25032_016E:B25032_018E (owner and renter occupied) 2-9 units
# B25032_008E + B25032_019E 10-19 units
# B25032_009E+B25032_010E+B25032_020E+B25032_021E 20 or more units

# B25031_001E median gross rent

# B25034_011E built 1939 or earlier

housing_vars <- c("B25001_001E","B25002_002E","B25002_003E","B25003_002E", "B25003_003E","B25004_006E",
                  'B25032_003E' , "B25032_004E" ,"B25032_014E" , "B25032_015E","B25032_005E","B25032_006E",
                  "B25032_007E", "B25032_016E","B25032_017E","B25032_018E","B25032_008E", "B25032_019E", "B25032_009E",
                  "B25032_010E","B25032_020E","B25032_021E", "B25031_001E","B25034_011E")

acs5_housing <- foreach(i = rep(2015:2019,51), j = rep(1:51,4),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=housing_vars)
}

acs5_housing <- fips_to_acs(acs5_housing)

acs5_housing<- acs5_housing %>% 
  mutate( '1 unit'=B25032_003E + B25032_004E + B25032_014E + B25032_015E,
          '2-9 units'= B25032_005E+B25032_006E+B25032_007E + B25032_016E+B25032_017E+B25032_018E,
          '10-19 units'=  B25032_008E + B25032_019E ,
          '20 or more units'= B25032_009E+B25032_010E+B25032_020E+B25032_021E
  ) %>%
  select("year","state_name","county.y","tract", "B25001_001E","B25002_002E","B25002_003E","B25003_002E", "B25003_003E","B25004_006E",'1 unit','2-9 units','10-19 units','20 or more units',"B25031_001E","B25034_011E")
names(acs5_housing) <- c("year","state","county","tract","Total Units","Occupied","Vacant","Owner occupied", "Renter occupied", "Vacant for sesional use",'1 unit','2-9 units','10-19 units','20 or more units',"median gross rent","units built 1939 or earlier")




##Commuter information/Migration
## workers 16 and over, car,truc, or van --drives alone, car truck or van -- carpools, public transporation (excluding taxi), walk, other means, works from home
# B08006_001E - workers 16 and over
# B08006_003E commute by car, truck, van -- alone
# B08006_004E commute by car, truck, van -- carpool
# B08006_008E public transportation excluding taxi
# B08006_014E bicycle
# B08006_015E walk
# B08006_016E other means
# B08006_017E work from home

commute_vars <-c("B08006_001E", "B08006_003E" , "B08006_004E" , "B08006_008E", "B08006_014E" , "B08006_015E", "B08006_016E","B08006_017E")

acs5_commute <- foreach(i = rep(2015:2019,51), j = rep(1:51,4),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=commute_vars)
}

acs5_commute <- fips_to_acs(acs5_commute)
acs5_commute <- acs5_commute %>% select("year","state_name","county.y","tract", c(commute_vars))

names(acs5_commute) <- c("year","state","county","tract","workers 16 and over","commute by car, truck, van -- alone","commute by car, truck, van -- carpool", "public transportation excluding taxi", "bicycle","walk", "other means", "work from home")


##Resident occupations
## employed population 16 and over, sectors of occupation (statsamerica shows top 6 emplyment sectors )


## B24011_001E:B24011_036E (pull names through varnames)

occupation_vars <- c(paste0("B24011_00",1:9,"E"),paste0("B24011_0",10:36,"E"))
acs5_occ_vars <- var_desc[var_desc$VariableCode %in% occupation_vars,]


acs5_occupation <- foreach(i = rep(2013:2019,51), j = rep(1:51,6),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=occupation_vars)
}
# acs5_commute <- fips_to_acs(acs5_commute)
# acs5_commute <- acs5_commute %>% select("year","state_name","county.y","tract", c(commute_vars))
# 
# names(acs5_commute) <- c("year","state","county","tract","workers 16 and over","commute by car, truck, van -- alone","commute by car, truck, van -- carpool", "public transportation excluding taxi", "bicycle","walk", "other means", "work from home")


## Response rates

#for the 2010 census only the Final Self Response rates are available see: https://api.census.gov/data/2010/dec/responserate/variables.html 
resp2010 <-  foreach (state= 1:51,.combine="rbind") %do% {
  getCensus(name= "dec/responserate",vintage = 2010,key = key,vars = "FSRR2010",region = "tract:*",regionin = paste0("state:",unique(fips_codes$state_code)[state]))
}

resp2010 <- fips_to_acs(resp2010)
resp2010 <- resp2010 %>% select(state="state_name",county="county.y",tract="tract","full response rate"="FSRR2010") 

resp2020_vars <- censusapi::makeVarlist(name= "dec/responserate",vintage = 2020,find="*",varsearch = "all",output = "dataframe")


resp2020 <- foreach (state= 1:51,.combine="rbind") %do% {
  getCensus(name= "dec/responserate",vintage = 2020,key = key,vars = c("CRRALL","CRRINT"),region = "tract:*",regionin = paste0("state:",unique(fips_codes$state_code)[state]))
}
resp2020 <- fips_to_acs(resp2020)
resp2020 <- resp2020 %>% select(state="state_name",
                                county="county.y",
                                tract="tract",
                                "Cumulative Self-Response Rate - Overall"="CRRALL",
                                "Cumulative Self-Response Rate - Internet"="CRRINT") 

comp_resp <- left_join(resp2010,resp2020)
comp_resp <- comp_resp%>% mutate(comparison= `full response rate`-`Cumulative Self-Response Rate - Overall`)

#a little exploration -- it would be really cool to compare this to voter statistics :https://electionlab.mit.edu/data
length(comp_resp[comp_resp$comparison<0,1])/length(comp_resp$state)




ggplot(comp_resp)+
  geom_density(mapping=aes(`Cumulative Self-Response Rate - Overall`),color="blue")+
  geom_density(mapping=aes(`Cumulative Self-Response Rate - Internet`),color="lightblue")+
  geom_density(mapping=aes(`full response rate`),color="yellow")



navtracts <- c(grep("9457",comp_resp$tract),grep("9449",comp_resp$tract),grep("9451",comp_resp$tract),grep("9439",comp_resp$tract))
navtracts <- navtracts[-7]
navajo <- comp_resp[navtracts,]
