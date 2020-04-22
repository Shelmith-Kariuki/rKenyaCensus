## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 2.2: Distribution of Population by Sex,
## Number of Households, Land Area, Population Density and County

## Load the libraries required ------
rm(list = ls())
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                 pages = 22)

## Drop the first 4 rows
df <- data.frame(df_0[[1]][-1:-4,], stringsAsFactors = F)

## Create a copy of the dataset
df_copy <- df %>% select(X1) %>% rename(County = X1)
df_copy$County <- trimws(df_copy$County)

## Clean the variables one by one
df_copy$Sex_Total <- as.numeric(trimws(gsub(",","",df$X2)))
df_copy$Sex_Male <- as.numeric(sub("^(\\w+)\\s?(.*)$","\\1", trimws(gsub(",","",df$X3))))
df_copy$Sex_Female <- as.numeric(sub("^(\\w+)\\s?(.*)$","\\2", trimws(gsub(",","",df$X3))))
df_copy$Sex_Intersex <- trimws(gsub(",","",df$X5))
df_copy$Sex_Intersex <- as.numeric(gsub(" ","",df_copy$Sex_Intersex))
df_copy$Households_Total <- as.numeric(trimws(gsub(",","",df$X6)))
df_copy$Households_Conventional <- as.numeric(sub("^(\\w+)\\s?(.*)$","\\1", trimws(gsub(",","",df$X7))))
df_copy$Households_GroupQuarters <- as.numeric(sub("^(\\w+)\\s?(.*)$","\\2", trimws(gsub(",","",df$X7))))
df_copy$`LandArea(Sq km)` <- trimws(gsub(",","",df$X8))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(",","",df$X9))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(" ","",df_copy$`Density(Persons per Sq km)`))

## Check to see if the scrapping has been done correctly

# df_checker <- df_copy %>%
#   mutate(sextotal2 = Sex_Male + Sex_Female + Sex_Intersex,
#          check1 = Sex_Total == sextotal2,
#          htotal2 = Households_Conventional + Households_GroupQuarters,
#          check2 = Households_Total == htotal2)

## Save the dataset
write_csv(df_copy, "../../../Desktop/KenyanCensus2019/Datasets/V2_T2.2.csv")
V2_T2.2 <- df_copy
usethis::use_data(V2_T2.2, overwrite = TRUE)
