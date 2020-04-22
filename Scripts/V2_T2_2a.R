## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping Table 2.2a: Distribution of Rural Population by Sex, Number of Households, Land Area,
## Population Density and County

## Load the libraries required ------
rm(list = ls())
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_tables("../../../Desktop/KenyanCensus2019/Resources/2019 KPHC Volume II_.pdf",
                                  pages = 23)

## Drop the first 4 rows
df <- data.frame(df_0[[1]][-1:-4,], stringsAsFactors = F)

## Create a copy of the dataset
df_copy <- df %>% select(X1) %>% rename(County = X1)
df_copy$County <- trimws(df_copy$County)

## Clean the variables one by one
df_copy$Sex_Total <- as.numeric(trimws(gsub(",|-","",df$X3)))
df_copy$Sex_Male <- as.numeric(trimws(gsub(",|-","",df$X4)))
df_copy$Sex_Female <- as.numeric(trimws(gsub(",|-","",df$X5)))
df_copy$Sex_Intersex <- as.numeric(trimws(gsub(",|-","",df$X7)))
df_copy$Households_Total <- as.numeric(trimws(gsub(",|-","",df$X8)))
df_copy$Households_Conventional <- sub("^(\\w+)\\s?(.*)$","\\1", trimws(gsub(",|-","",df$X9)))
df_copy$Households_Conventional <- as.numeric(gsub(" ", "", df_copy$Households_Conventional))
df_copy$Households_GroupQuarters <- sub("^(\\w+)\\s?(.*)$","\\2", trimws(gsub(",|-","",df$X9)))
df_copy$Households_GroupQuarters <- as.numeric(gsub(" ", "", df_copy$Households_GroupQuarters))
df_copy$`LandArea(Sq km)` <- trimws(gsub(",","",df$X11))
df_copy$`LandArea(Sq km)`  <- trimws(gsub(" ","",df_copy$`LandArea(Sq km)` ))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(",","",df$X12))
df_copy$`Density(Persons per Sq km)` <- trimws(gsub(" ","",df_copy$`Density(Persons per Sq km)`))

## Add Nairobi details at the bottom
df_copy[48,] <- c("NAIROBI", rep(NA, length(df_copy)-1))
df_copy <- data.frame(df_copy, check.names = FALSE)

## Convert all the variables except the first, to numeric
df_copy[,-1] <- data.frame(sapply(df_copy[,-1], function(x) as.numeric(x)))

## Check to see if the scrapping has been done correctly

df_checker <- df_copy %>%
  mutate(sextotal2 = Sex_Male + Sex_Female + Sex_Intersex,
         check1 = Sex_Total == sextotal2,
         htotal2 = Households_Conventional + Households_GroupQuarters,
         check2 = Households_Total == htotal2)

## Save the dataset
write_csv(df_copy, "../../../Desktop/KenyanCensus2019/Datasets/V2_T2.2a.csv")
V2_T2.2a <- df_copy
usethis::use_data(V2_T2.2a, overwrite = TRUE)
