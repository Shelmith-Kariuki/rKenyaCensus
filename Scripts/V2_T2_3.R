## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 2.3: Distribution of Population by Sex, Number of Households,
## Land Area, Population Density and Sub County

## Load the libraries required ------
rm(list = ls())
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                  pages = 25:31)

tab_func <- function(num){
## Drop the first 4 rows
df <- data.frame(df_0[[num]][-1:-4,], stringsAsFactors = F)

## Create a copy of the dataset
df_copy <- df %>% select(X1) %>% rename(SubCounty = X1)
df_copy$SubCounty <- trimws(df_copy$SubCounty)

## Clean the variables one by one
## Clean the variables one by one
df_copy$Sex_Total <- trimws(gsub(",|-","",df$X2))
df_copy$Sex_Male <- trimws(gsub(",|-","",df$X3))
df_copy$Sex_Female <- trimws(gsub(",|-","",df$X4))
df_copy$Sex_Female <- ifelse(df_copy$Sex_Female == "", trimws(gsub(",|-","",df$X5)),
                                                              df_copy$Sex_Female)
df_copy$Households_Total <- trimws(gsub(",|-","",df$X6))
df_copy$Households_Conventional <- sub("^(\\w+)\\s?(.*)$","\\1", trimws(gsub(",","",df$X7)))
df_copy$Households_GroupQuarters <- sub("^(\\w+)\\s?(.*)$","\\2", trimws(gsub(",","",df$X7)))
df_copy$`LandArea(Sq km)` <- trimws(gsub(",","",df$X8))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(",","",df$X9))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(" ","",df_copy$`Density(Persons per Sq km)`))
return(df_copy)
}

## Call the function

df_rows <- map_df(1:length(df_0), tab_func)

## Convert the numeric variables to actual numeric
df_rows[,-1] <- data.frame(sapply(df_rows[,-1], function(x) as.numeric(x)))

## Merge the data with V2_T1.2 to distinguish between Counties, SubCounties and Special Areas

df_rows2 <- full_join(df_rows, V2_T1.2)

## Fill in the missing values in County and CountyCode
df_rows2 <- df_rows2 %>%
  mutate(id = row_number(),
         County = ifelse(is.na(County), County[id+1], County),
         CountyCode = ifelse(is.na(CountyCode), CountyCode[id+1], CountyCode))

## Re label the entries on the first two rows.
df_rows2[1:2,"County"] <- c("xxx", "MOMBASA")

## Fill in the missing Counties ,for the "special areas e.g forests"
df_rows2 <- df_rows2 %>%
  mutate(County = zoo::na.locf.default(County))

## Generate a variable that shows whether the Area is a County or SubCounty
df_rows2 <- df_rows2 %>%
  mutate(Area =ifelse(is.na(SubCountyCode)&!is.na(CountyCode), "County",
                      "SubCounty"))

## Re label the CountyCode entry on the first row.
df_rows2[1,"CountyCode"] <- "xxx"

## Fill in the missing Country Codes
df_rows2 <- df_rows2 %>%
  mutate(CountyCode = zoo::na.locf.default(CountyCode))

## There are subcounties that have the same name as the counties, clean this
x <- df_rows2 %>% filter(Area == "County") %>% select(County) %>% pull()
x2 <- unique(V2_T1.2$County[which(!V2_T1.2$County %in% x)])

df_rows2 <- df_rows2 %>%
  #arrange(County, id) %>%
  group_by(County) %>%
  mutate(Area = ifelse(id == min(id) & (County %in% x2), "County", Area),
         SubCountyCode = ifelse(Area == "County", NA, SubCountyCode)) %>%
  select(-id)

df_rows2[1,"Area"] <- "xxx"

## Order the data, so that the County and SubCounty variables are at the beginning.
df_rows2 <- df_rows2 %>%
  select(CountyCode, County, SubCountyCode, SubCounty, Area, everything())


## Save this data externally
write_csv(df_rows2, "../../../Desktop/KenyanCensus2019/Datasets/V2_T2.3.csv")
V2_T2.3 <- df_rows2
usethis::use_data(V2_T2.3, overwrite = TRUE)
