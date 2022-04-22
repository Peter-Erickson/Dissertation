library("tidyverse")
library("sf")
library("tidycensus")
library("readxl")
library("stringr")
library("here")
library("writexl")


# In this file, I basically clean several data sets from the US
# Census Bureau and the Department of Veterans Affairs. The goal is to obtain one
# ultimate file that contains the names of every county, the 
# percentage of that county population that is a veteran, and the media market that
# that county belongs to.  

## Begin with the Census Bureau Data Sets
# county population through 2008

county_pop_thru_08 <- read_csv(here("data", "co-est2009-alldata.csv")) %>% select(COUNTY:CTYNAME,  ESTIMATESBASE2000, POPESTIMATE2004, POPESTIMATE2008)
county_pop_thru_08 <- transform(county_pop_thru_08, CTYNAME=sub("LaSalle", "La Salle", CTYNAME))
county_pop_thru_08 <- transform(county_pop_thru_08, CTYNAME=sub("Petersburg Census Area", "Petersburg Borough", CTYNAME))
county_pop_thru_08 <- county_pop_thru_08 %>% 
  filter(!str_detect(CTYNAME, "Bedford city"))

# change name of 'Shannon' County, SD to 'Oglala Lakota' County, SD
county_pop_thru_08 <- county_pop_thru_08 %>% mutate(
  CTYNAME=if_else(
    (STNAME=="South Dakota" & CTYNAME=="Shannon County"), "Oglala Lakota County", CTYNAME)) %>% mutate(
      COUNTY=if_else(
        CTYNAME=="Oglala Lakota County", "102", COUNTY))

# county population through 2016
county_pop_thru_16 <- read_csv(here("data", "co-est2020.csv")) %>% select(COUNTY:CTYNAME, POPESTIMATE2012, POPESTIMATE2016)
county_pop_thru_16 <- transform(county_pop_thru_16, CTYNAME=sub("LaSalle", "La Salle", CTYNAME))

#combine rows and columns for Chugach and Copper River, AK,  County ID codes?
county_pop_thru_16 <- transform(county_pop_thru_16, CTYNAME=sub("Copper River", "Valdez-Cordova", CTYNAME))
county_pop_thru_16 <- transform(county_pop_thru_16, CTYNAME=sub("Chugach", "Valdez-Cordova", CTYNAME))
county_pop_thru_16 <- transform(county_pop_thru_16, CTYNAME=sub("Kusilvak", "Wade Hampton", CTYNAME))
county_pop_thru_16 <- county_pop_thru_16 %>% group_by(CTYNAME, STNAME) %>% summarize(POPESTIMATE2012=sum(POPESTIMATE2012, na.rm=T), POPESTIMATE2016=sum(POPESTIMATE2016, na.rm=T))

# join county population dfs 
county_pop <- full_join(county_pop_thru_08, county_pop_thru_16)

county_pop <- transform(county_pop, CTYNAME = gsub(" County", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" city", " City", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub("St. ", "Saint ", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" Borough", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" Census Area", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" Municipality", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" City and Borough", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub(" Parish", "", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub("Ste. ", "Sainte ", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub("Wade Hampton", "Kusilvak", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub("De Witt", "Dewitt", CTYNAME))
county_pop <- transform(county_pop, CTYNAME = gsub("De Soto", "Desoto", CTYNAME))

county_pop <- county_pop %>% filter(COUNTY != '000')
county_pop <- county_pop %>% select(COUNTY, CTYNAME, STNAME, ESTIMATESBASE2000:POPESTIMATE2016) %>% rename(CTYID=COUNTY) 

## make abbreviations for 50 states
st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>% rename(STNAME=state, STABB=abb) 

county_pop <- full_join(county_pop, st_crosswalk, by = "STNAME")

# rename state abbreviation variable
county_pop <- county_pop %>% select(CTYID, CTYNAME, STABB, STNAME:POPESTIMATE2016) %>% select(-STNAME) %>% rename (STNAME=STABB)
county_pop <- transform(county_pop, CTYNAME = sub("\xfc\xbe\x8c\x96\x98\xbc", "n", CTYNAME))
county_pop <- mutate(county_pop, across(where(is.character), .fns = toupper))
county_pop <- transform(county_pop, CTYNAME = gsub(" CITY AND", "", CTYNAME))


## now use VA data to read in 2018 veteran pop (numbers) 
## by county and clean data

vetpop_county <- read_csv(here("data", "VetPop2018_County.csv")) 
vetpop_county <- vetpop_county[-c(1:6), ] %>% select(1:3) 
colnames(vetpop_county) <- c("CTYID","CTYSTATE", "Vet_Pop_18")
vetpop_county <- vetpop_county %>% separate(CTYSTATE, into=c("CTYNAME", "STNAME"), sep='[,]') %>% filter(!is.na(CTYNAME)) %>% rename(VETCTYID=CTYID) %>% select(-VETCTYID)
vetpop_county <- mutate(vetpop_county, across(where(is.character), .fns = toupper))
vetpop_county <- transform(vetpop_county, CTYNAME = gsub("ST. ", "SAINT ", CTYNAME))
vetpop_county <- transform(vetpop_county, CTYNAME = sub("BUENA VISAINT", "BUENA VISTA", CTYNAME))
vetpop_county <- transform(vetpop_county, CTYNAME = sub("LASALLE", "LA SALLE", CTYNAME))

# remove non-50 State counties and territories
vetpop_county <- vetpop_county %>% 
  filter(!str_detect(CTYNAME, "PUERTO RICO")) %>% filter(!str_detect(CTYNAME, "AMERICAN SAMOA")) %>% filter(!str_detect(CTYNAME, "GUAM")) %>% filter(!str_detect(CTYNAME, "NORTHERN MARIANA ISLANDS")) %>% filter(!str_detect(CTYNAME, "VIRGIN ISLANDS")) %>% filter(!str_detect(CTYNAME, "FOREIGN COUNTRIES"))

## join county population df with veteran county population
vet_cty_pop <- full_join(county_pop, vetpop_county, by= c("CTYNAME", "STNAME"))

# now check for NA values
vet_cty_pop_na <- vet_cty_pop[rowSums(is.na(vet_cty_pop)) > 0, ] 



## Now aggregate counties into DMAs
# read in counties by DMA
county_DMA <- read_csv(here("data", "usa-tvdma-county.csv")) %>% select(-Internal_State_Region, -Metro_Grp, -Metropolitan_Statistical_Areas, -STATE) %>% rename(STNAME=STATE_AB, CTYNAME=COUNTY) 

# cleaning county_DMA df for full_joining purposes
county_DMA <- transform(county_DMA, CTYNAME = gsub("DE KALB", "DEKALB", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST CLAIR", "SAINT CLAIR", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 1", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" C.A. 2", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 3", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" C.A. 4", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 5", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 6", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" C.A. 7", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" NO.STAR BOR8.", " NORTH STAR", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 9", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("JUNEAU0", "JUNEAU", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("PENINSULA1", "PENINSULA", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" B1O2", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" ISLAND3", " ISLAND", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR14", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("& PENINSULA", "AND PENINSULA", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" B1O5", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" C.A. 16", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("SLOPE7", "SLOPE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BO1R8.", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 20", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("SKAGWAY-HOO.-ANG.C.A21", "SKAGWAY", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" FRBKS. C.A 22", " FAIRBANKS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("CORDOVA3", "CORDOVA", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("DE WITT", "DEWITT", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("DU PAGE", "DUPAGE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("LA PORTE", "LAPORTE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("O BRIEN", "O'BRIEN", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("DE SOTO", "DESOTO", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST FRANCIS", "SAINT FRANCIS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST. JOHNS", "SAINT JOHNS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST. LUCIE", "SAINT LUCIE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST JOSEPH", "SAINT JOSEPH", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST BERNARD", "SAINT BERNARD", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST CHARLES", "SAINT CHARLES", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST HELENA", "SAINT HELENA", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST JAMES", "SAINT JAMES", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST JOHN THE BAP", "SAINT JOHN THE BAPTIST", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST LANDRY", "SAINT LANDRY", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST MARTIN", "SAINT MARTIN", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST MARY", "SAINT MARY", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST TAMMANY", "SAINT TAMMANY", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("PRINCE GEORGES", "PRINCE GEORGE'S", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("QUEEN ANNES", "QUEEN ANNE'S", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST MARYS", "SAINT MARY'S", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("SAINT MARYS", "SAINT MARY'S", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("LAKE OF WOODS", "LAKE OF THE WOODS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST LOUIS", "SAINT LOUIS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST LAWRENCE", "SAINT LAWRENCE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("STE GENEVIEVE", "SAINTE GENEVIEVE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("LA MOURE", "LAMOURE", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST FRANCOIS", "SAINT FRANCOIS", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("ST CROIX", "SAINT CROIX", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("BEDFORD CITY", "BEDFORD", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("YUKON-KOYUKUK7", "YUKON-KOYUKUK", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub(" BOR. 26", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("-PETERS. C.A25", "", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = gsub("WADE HAMPTON4", "KUSILVAK", CTYNAME))
county_DMA <- transform(county_DMA, CTYNAME = sub("PR.WALES-OUT.KET.C.A 19", "PRINCE OF WALES-HYDER", CTYNAME))


## Now change some specific counties in specific states, MOSTLY VA
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="SD" & CTYNAME=="SHANNON"), "OGLALA LAKOTA", CTYNAME)) 
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="FL" & CTYNAME=="DADE"), "MIAMI-DADE", CTYNAME)) 
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="ALEXANDRIA"), "ALEXANDRIA CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="BRISTOL"), "BRISTOL CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="BUENA VISTA"), "BUENA VISTA CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="CHARLOTTESVILLE"), "CHARLOTTESVILLE CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="CHESAPEAKE"), "CHESAPEAKE CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="COLONIAL HEIGHTS"), "COLONIAL HEIGHTS CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="COVINGTON"), "COVINGTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="DANVILLE"), "DANVILLE CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="EMPORIA"), "EMPORIA CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="FALLS CHURCH"), "FALLS CHURCH CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="FREDERICKSBURG"), "FREDERICKSBURG CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="HAMPTON"), "HAMPTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="HARRISONBURG"), "HARRISONBURG CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="HOPEWELL"), "HOPEWELL CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="LEXINGTON"), "LEXINGTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="LYNCHBURG"), "LYNCHBURG CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="MANASSAS"), "MANASSAS CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="MANASSAS PARK"), "MANASSAS PARK CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="MARTINSVILLE"), "MARTINSVILLE CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="NEWPORT NEWS"), "NEWPORT NEWS CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="NORFOLK"), "NORFOLK CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="NORTON"), "NORTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="PETERSBURG"), "PETERSBURG CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="POQUOSON"), "POQUOSON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="PORTSMOUTH"), "PORTSMOUTH CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="RADFORD"), "RADFORD CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="SALEM"), "SALEM CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="SOUTH BOSTON"), "SOUTH BOSTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="STAUNTON"), "STAUNTON CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="SUFFOLK"), "SUFFOLK CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="VIRGINIA BEACH"), "VIRGINIA BEACH CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="WAYNESBORO"), "WAYNESBORO CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="WILLIAMSBURG"), "WILLIAMSBURG CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="WINCHESTER"), "WINCHESTER CITY", CTYNAME))
county_DMA <- county_DMA %>% mutate(
  CTYNAME=if_else(
    (STNAME=="VA" & CTYNAME=="GALAX"), "GALAX CITY", CTYNAME))


### Join Data Sets

vet_county_DMA <- full_join(vet_cty_pop, county_DMA, by= c("STNAME", "CTYNAME"))

# Then Drop counties you don't need or are a non-match
vet_county_DMA <- vet_county_DMA %>% filter(!STNAME %in% c('STATE_AB')) %>% filter(!CTYNAME %in% c("SOUTH BOSTON CITY", "CLIFTON FORGE"))

# use mutate-else to replace missing DMA values for six counties. 
vet_county_DMA <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="AK" & CTYNAME=="HOONAH-ANGOON"), "Juneau, AK DMA", TVDMA))
vet_county_DMA <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="AK" & CTYNAME=="PETERSBURG"), "Juneau, AK DMA", TVDMA))
vet_county_DMA <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="CO" & CTYNAME=="BROOMFIELD"), "Denver, CO - NE - NV - WY DMA", TVDMA))
vet_county_DMA <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="DC" & CTYNAME=="DISTRICT OF COLUMBIA"), "Washington, DC - MD - PA - VA - WV DMA", TVDMA))
vet_county_DMA <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="HI" & CTYNAME=="KALAWAO"), "Honolulu, HI DMA", TVDMA))

vet_county_DMA_join <- vet_county_DMA %>% mutate(
  TVDMA=if_else(
    (STNAME=="ND" & CTYNAME=="WILLIAMS"), "Fargo - Valley City, ND - MN DMA", TVDMA))

### this is good, but I picked up two extra counties.  Not sure why! Come back to this. 

##Now calculate the total pop and vet pop of each county and then by DMA. 
# remove commas from Vet_Pop_18 and change to class numeric
vet_county_DMA_join <- transform(vet_county_DMA_join, Vet_Pop_18 = gsub(",", "", Vet_Pop_18))

vet_county_DMA_join$Vet_Pop_18 <- as.numeric(vet_county_DMA_join$Vet_Pop_18)


vet_county_DMA_stats <- vet_county_DMA_join %>% mutate(
  county_vet_percent = Vet_Pop_18/POPESTIMATE2016
  ) %>% group_by(TVDMA) %>%
  mutate(
    total_pop_DMA=sum(POPESTIMATE2016), 
    total_vet_DMA=sum(Vet_Pop_18),
    percent_vet_DMA=total_vet_DMA/total_pop_DMA
  )

#remove the word 'DMA' from the TVDMA variable for easier cleaning
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(" DMA", "", TVDMA)) 

# make the DMAs easier to read/understand
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(", NY - MA - VT", "", TVDMA)) 
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(", NM - AZ - CO", "", TVDMA)) 
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("Amarillo, TX -NM - OK", "Amarillo, TX", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(" - AL - NC", "", TVDMA)) 
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(", WV - VA", "", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("Cedar Rapids - Waterloo&Dubuque", "Cedar Rapids - Waterloo", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub(" - And., NC - GA", "", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("Lufkin", "", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("Nacogdoches", "", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("[()&]", "", TVDMA))
vet_county_DMA_stats <- transform(vet_county_DMA_stats, TVDMA = gsub("[*]", "", TVDMA))

vet_DMA_stats <- vet_county_DMA_stats %>% select(TVDMA, percent_vet_DMA)%>% group_by(TVDMA) %>% distinct() 
vet_DMA_stats <- vet_DMA_stats %>% mutate(across(where(is.character), .fns = toupper)) 
vet_DMA_stats %>% write_xlsx(here("data", "vet_DMA_final.xlsx"))


##### Need a DF that has each county, DMA, and percent veteran population #####
##### For Final Map Construction #####

vet_county_DMA_map <- vet_county_DMA_stats %>% select(CTYID, CTYNAME, STNAME, TVDMA, county_vet_percent, percent_vet_DMA) %>% mutate(across(where(is.character), .fns = toupper)) 


#load DMA-county cross-walk data
county_fips_xwalk <- read_csv(here("data", "countygeodata2018.csv")) %>%  
  rename(CTYNAME = "County Name",
         STNAME = "State",
         FIPS = "FIPS County Code") 

county_fips_xwalk <- county_fips_xwalk %>% select(-SSACD, -CBSA, -'CBSA Name') 
county_fips_xwalk <- county_fips_xwalk %>% filter(!STNAME=="PR")

# clean county_fips_xwalk df (variables STNAME and CTYNAME)
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="AL" & CTYNAME=="DE KALB"), "DEKALB", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="AL" & CTYNAME=="ST. CLAIR"), "SAINT CLAIR", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="AK" & CTYNAME=="BRISTOL BAY BOROUGH"), "BRISTOL BAY", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME = gsub(" CENSUS AREA", "", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("KODIAK ISLAND BOROUGH", "KODIAK ISLAND", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub(" BOROUGH", "", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub(" BOROUH", "", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub(" ARTIC", " ARCTIC", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("PR.OF WALES-HYDER CNS AREA", "PRINCE OF WALES-HYDER", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("-YAKUTAT", "", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("WADE HAMPTON", "KUSILVAK", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub(" CITY AND", "", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("LAPAZ", "LA PAZ", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("ST. FRANCIS", "SAINT FRANCIS", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME = gsub("ST. JOHNS", "SAINT JOHNS", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME = gsub("ST. LUCIE", "SAINT LUCIE", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="GA" & CTYNAME=="DE KALB"), "DEKALB", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("THE DISTRICT", "DISTRICT OF COLUMBIA", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("DE SOTO", "DESOTO", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC DUFFIE", "MCDUFFIE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC INTOSH", "MCINTOSH", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="DE KALB"), "DEKALB", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="DE WITT"), "DEWITT", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("DU PAGE", "DUPAGE", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="MC HENRY"), "MCHENRY", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="MC LEAN"), "MCLEAN", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="MC DONOUGH"), "MCDONOUGH", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IL" & CTYNAME=="ST. CLAIR"), "SAINT CLAIR", CTYNAME))
county_fips_xwalk <- county_fips_xwalk %>% mutate(
  CTYNAME=if_else(
    (STNAME=="IN" & CTYNAME=="DE KALB"), "DEKALB", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("LA PORTE", "LAPORTE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("OBRIEN", "O'BRIEN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC CRACKEN", "MCCRACKEN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC LEAN", "MCLEAN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC CREARY", "MCCREARY", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("LASALLE", "LA SALLE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("E. BATON", "EAST BATON", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("ST. ", "SAINT ", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("W. BATON", "WEST BATON", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("SAINT JOHN BAPTIST", "SAINT JOHN THE BAPTIST", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("SAINT MARYS", "SAINT MARY'S", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("QUEEN ANNES", "QUEEN ANNE'S", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("PRINCE GEORGES", "PRINCE GEORGE'S", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("LAKE OF  WOODS", "LAKE OF THE WOODS", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("YELLOW MEDCINE", "YELLOW MEDICINE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("DE KALB", "DEKALB", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("STE. GENEVIEVE", "SAINTE GENEVIEVE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("SCOTT BLUFF", "SCOTTS BLUFF", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC DOWELL", "MCDOWELL", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("LA MOURE", "LAMOURE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC KEAN", "MCKEAN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("NORTHUMBERLND", "NORTHUMBERLAND", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC COOK", "MCCOOK", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC PHERSON", "MCPHERSON", CTYNAME))

county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("OGLALA LAKOTA COUNTY", "OGLALA LAKOTA", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC MINN", "MCMINN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC NAIRY", "MCNAIRY", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("DE WITT", "DEWITT", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC CULLOCH", "MCCULLOCH", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC LENNAN", "MCLENNAN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC MULLEN", "MCMULLEN", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("BUENA VISAINT CITY", "BUENA VISTA CITY", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("POQUOSON", "POQUOSON CITY", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MENOMONEE", "MENOMINEE", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("JEFFRSON DAVIS", "JEFFERSON DAVIS", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC LEOD", "MCLEOD", CTYNAME))
county_fips_xwalk <- transform(county_fips_xwalk, CTYNAME=sub("MC DONALD", "MCDONALD", CTYNAME))


# COMBINE FIPS DATA TO CLEANED COUNTY/DMA AND VETERAN PERCENTAGE DATA
dma_county_df <- full_join(vet_county_DMA_map, county_fips_xwalk, by = c("CTYNAME", "STNAME")) %>% filter(!CTYNAME=="STATEWIDE")

## make two new variables for relative range of values

mean(dma_county_df$county_vet_percent)
mean(dma_county_df$percent_vet_DMA)

dma_county_df <- dma_county_df %>% mutate(
  relative_cty_vet_pop = case_when(
    (county_vet_percent <=.05124) ~ "verylow",  ## 30% lower or more
    (county_vet_percent >.05124 & county_vet_percent <=.06588) ~ "low",  ## 10-30% lower
    (county_vet_percent >.06588 & county_vet_percent <=.08052) ~ "avg", ## within 10% natl avg
    (county_vet_percent >.08052 & county_vet_percent <=.09516) ~ "high", ## 10-30% higher
    (county_vet_percent >= .09516) ~ "veryhigh"), ## more than 30% higher
  relative_DMA_vet_pop = case_when(
    (percent_vet_DMA <=.04991) ~ "verylow",  ## 30% lower or more
    (percent_vet_DMA >.04991 & percent_vet_DMA <=.06417) ~ "low",  ## 10-30% lower
    (percent_vet_DMA >.06417 & percent_vet_DMA <=.07843) ~ "avg", ## within 10% natl avg
    (percent_vet_DMA >.07843 & percent_vet_DMA <=.09269) ~ "high", ## 10-30% higher
    (percent_vet_DMA >.09269) ~ "veryhigh"))  ## more than 30% higher)

# change relative strength of DMA and county vet populations to factors
dma_county_df$relative_cty_vet_pop <- as.factor(dma_county_df$relative_cty_vet_pop)
dma_county_df$relative_DMA_vet_pop <- as.factor(dma_county_df$relative_DMA_vet_pop)

class(dma_county_df$relative_cty_vet_pop)
dma_county_df$relative_cty_vet_pop<-fct_relevel(dma_county_df$relative_cty_vet_pop, "verylow", "low", "avg", "high", "veryhigh")
levels(dma_county_df$relative_cty_vet_pop)

dma_county_df$relative_DMA_vet_pop<-fct_relevel(dma_county_df$relative_DMA_vet_pop, "verylow", "low", "avg", "high", "veryhigh")
levels(dma_county_df$relative_DMA_vet_pop)


# Now Get Geographic Shapes For Each County
county_shapes <- get_decennial(geography = "county", 
                               variables = "P001001", 
                               year = 2010, 
                               geometry = TRUE)
#select homogeneous counties
county_shapes_df <- county_shapes %>%
  filter(!GEOID > 60) %>% #remove Puerto Rico
  rename(population = value,
         FIPS = GEOID) %>% 
  tigris::shift_geometry() #shift Alaska and Hawaii down

#join county_shapes_df to dma_county_df
county_shapes_df <- county_shapes_df %>% select(-NAME, -variable, -population)
dma_county_df <- full_join(dma_county_df, county_shapes_df, by = "FIPS")
dma_county_df <- dma_county_df %>% filter(!relative_cty_vet_pop=="NA")

# create DMA map. 
#First we have to make the shapes of the media markets. 
# We use the st_union function in order to do this. 
dma_df_map <- dma_county_df %>%
  group_by(TVDMA) %>% 
  summarise(geometry = st_union(geometry))

# Then we need a slim data set of just the variables I want to map by DMA. 
dma_vars <- dma_county_df %>% select(TVDMA, percent_vet_DMA, relative_DMA_vet_pop) %>% distinct()

# join the data sets and prepare to graph
dma_df_map <- full_join(dma_df_map, dma_vars, by="TVDMA")

## make labels for the DMA plot
 
dma_df_map <- dma_df_map %>% mutate(
DMA_rank = case_when(
  TVDMA == "NORFOLK - PORTSMOUTH - NEWPORT NEWS, VA - NC" ~ 1,
  TVDMA == "FAIRBANKS, AK" ~ 2,
  TVDMA == "COLORADO SPRINGS - PUEBLO, CO" ~ 3,
  TVDMA == "MOBILE, AL - PENSACOLA, FL - MS" ~ 4,
  TVDMA == "PANAMA CITY, FL" ~ 5,
  TVDMA == "LOS ANGELES, CA" ~ 207,
  TVDMA == "MIAMI - FT. LAUDERDALE, FL" ~ 208,
  TVDMA == "NEW YORK, NY - CT - NJ - PA" ~ 209,
  TVDMA == "HARLINGEN - WESLACO - BROWNSVILLE - MCALLEN, TX" ~ 210,
  TVDMA == "LAREDO, TX" ~ 211))

list(dma_df_map$DMA_rank)

## save to RDS and read back in
saveRDS(dma_county_df, "data/ch4_county_map_data.rds")
saveRDS(dma_df_map, "data/ch4_DMA_map_data.rds")

rm(list=ls())

###### Use this code below this line  in Chapter 4 to print graphs ###

dma_county_df <- readRDS("data/ch4_county_map_data.rds")
dma_df_map <- readRDS("data/ch4_DMA_map_data.rds")

## Data Ready to Map ##

#create county map
ggplot(data = dma_county_df, aes(geometry = geometry, fill=relative_cty_vet_pop)) + 
  geom_sf(lwd=.015) + coord_sf(datum = NA) + theme_bw() +
  scale_fill_manual(values=c("bisque", "pink", "darkorange", "red", "darkred"), name = "Vet %\nof Pop.", breaks=c("verylow", "low", "avg","high","veryhigh"), labels=c("less than 30% lower\nthan national average", "10%-30% lower\nthan national average", "within 10% of\nnational average", "10-30% higher than\nnational average", "more than 30% higher\nthan national average")) + #select colors and legend name
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  labs(title = "Veteran Percentage of Population by County") +
  guides(fill = guide_legend(ncol=3,nrow=2,byrow=TRUE), shape=guide_legend(ncol=3,nrow=2,byrow=TRUE)) + 
  theme(legend.position="bottom") 



# make the DMA plot
ggplot(dma_df_map, aes(geometry = geometry, fill=relative_DMA_vet_pop)) + 
  geom_sf(lwd=.6) + 
  coord_sf(datum = NA) + 
  theme_bw() +
  scale_fill_manual(values=c("bisque", "pink", "darkorange", "red", "darkred"), name = "Vet. %\nof Pop.", breaks=c("verylow", "low", "avg","high","veryhigh"), labels=c("less than 30% lower\nthan national average", "10%-30% lower\nthan national average", "within 10% of\nnational average", "10-30% higher than\nnational average", "more than 30% higher\nthan national average")) + #select colors and legend name
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  labs(title = "Veteran Percentage of Population by Media Market",
       x="",
       y="") +
  guides(fill = guide_legend(ncol=3,nrow=2,byrow=TRUE), shape=guide_legend(ncol=3,nrow=2,byrow=TRUE)) + 
  theme(legend.position="bottom") +
  ggrepel::geom_label_repel(
    aes(label = DMA_rank, geometry= geometry), size = 4 , stat="sf_coordinates", min.segment.length=0, fill="white", show.legend = FALSE)


## alternate version

ggplot(data = dma_df_map, aes(geometry = geometry, fill=relative_DMA_vet_pop)) + 
  geom_sf(lwd=.6) + 
  coord_sf(datum = NA) + theme_bw() +
  scale_fill_manual(values=c("bisque", "pink", "darkorange", "red", "darkred"), name = "Vet. %\nof Pop.", breaks=c("verylow", "low", "avg","high","veryhigh"), labels=c("less than 30% lower\nthan national average", "10%-30% lower\nthan national average", "within 10% of\nnational average", "10-30% higher than\nnational average", "more than 30% higher\nthan national average")) + #select colors and legend name
  theme(plot.title = element_text(hjust = 0.5)) + #center title
  labs(title = "Veteran Percentage of Population by DMA",
       x="",
       y="") +
  guides(fill = guide_legend(ncol=3,nrow=2,byrow=TRUE), shape=guide_legend(ncol=3,nrow=2,byrow=TRUE)) + 
  theme(legend.position="bottom") +
  geom_sf_text(aes(label = DMA_rank), size = 4) 


