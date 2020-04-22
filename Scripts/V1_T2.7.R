## Author: Shelmith
## Date: 20th April, 2020
## Description: Scrapping V1_T2.7

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df_0 <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf",
                               pages = 40:48)
#
# givename <- function(x, vec){
#   y<- tibble(x1 = x[[vec]])
#   return(y)
# }
# vec <- 1:9
# df <- map2(df_0,vec,givename)
#
# for( i in 1: length(df_0)){
#   colnames(df_0[i][[1]]) <- "x1"
# }

#df <- bind_rows(df_0)

tab27_func <- function(num){

  df2 <- data.frame(df_0[[num]])

  ## Drop the first 6 rows ------
  df2$rownum <- 1:nrow(df2)
  df2 <- df2 %>% filter(rownum >=7 ) %>% select(-rownum)

  names(df2) <- "X1"

  ## Remove spaces in the county names, we will revert this later ------
  df2$X1 <- gsub("Homa Bay", "HomaBay", df2$X1)
  df2$X1 <- gsub("Nairobi City", "NairobiCity", df2$X1)
  df2$X1 <- gsub("Trans Nzoia", "TransNzoia", df2$X1)
  df2$X1 <- gsub("West Pokot", "WestPokot", df2$X1)
  df2$X1 <- gsub("Tana River", "TanaRiver", df2$X1)
  df2$X1 <- gsub("Uasin Gishu", "UasinGishu", df2$X1)

  ## Remove the commas ------
  df2 <- map_df(df2, ~gsub(",", "", .x))

  ## Clean the variables a bit ------
  df2$County <- gsub("\\.", "", gsub("[[:digit:]]","",df2$X1))
  df2$Values <- substr(df2$X1, 16, nchar(df2$X1))
  df2$Values <- trimws(gsub("\\.|[a-z]|[A-Z]|\\*", "", df2$Values))
  df2$Values <- str_squish(df2$Values)

  ## Separate the column into multiple columns ------
  df2 <-df2 %>%
    separate(Values, into = c("Population","LandArea(in Sq. Km)",
                              "Population Density(No. per Sq. Km)"), sep = " ")

  ## Drop the original variable ------
  df2$X1 <- NULL

  ## Filter the rows labeled County, Male and the blank row ------
  df2 <- df2 %>%
    filter(!County %in% grep("County", County, value = T, ignore.case = T))

  ## Convert the columns to the right type ------
  df2[,-1] <- map_df(df2[,-1], ~trimws(.x))
  df2[,-1] <- map_df(df2[,-1], ~as.numeric(.x))

  ## Rename the Kenya value to Total ------
  df2 <- df2 %>%
    mutate(County = ifelse(County == "Kenya", "Total", County))

  ## Return the LandArea to its original value ------
  df2 <- df2%>%
    mutate(`LandArea(in Sq. Km)` = `LandArea(in Sq. Km)` / 10)

  ## Re-edit the counties back to their original format ------
  df2$County <- gsub("HomaBay", "Homa Bay", df2$County)
  df2$County <- gsub("NairobiCity", "Nairobi City",  df2$County)
  df2$County <- gsub("TransNzoia", "Trans Nzoia", df2$County)
  df2$County <- gsub("WestPokot", "West Pokot",  df2$County)
  df2$County <- gsub("TanaRiver", "Tana River", df2$County)
  df2$County <- gsub("UasinGishu", "Uasin Gishu",  df2$County)

  ## Filter rows with (Sq Km)
  df2 <- df2 %>% filter(!County %in% grep("\\(Sq Km\\)",County, value = TRUE,
                                         ignore.case = T ))
  return(df2)
}

## Call the function
vec <- 1:9
df_rows <- map_df(vec, tab27_func)

## Remove trailing or leading spaces in the county variable
df_rows$County <- trimws(df_rows$County)

## Filter blank counties
df_rows <- df_rows %>%
  filter(County != "" &!County %in% grep("Demarcated|Excludes|Lodges|Households|County", County, value = TRUE, ignore.case = TRUE))

## Convert the NAs to 0s
df_rows[,-1] <- data.frame(sapply(df_rows[,-1], function(x)
  ifelse(is.na(x), 0,x)))

## Check that we are right
df_checker <- df_rows %>%
  mutate(fig = round(Population / `LandArea(in Sq. Km)`,0)) %>%
  mutate(checker = `Population Density(No. per Sq. Km)`==fig) %>%
  mutate(diff = fig - `Population Density(No. per Sq. Km)`)


## Read in county data
county_data <- read_csv("../../../Desktop/KenyanCensus2019/Datasets/V1_T2.4.csv")
county_data <- county_data %>% select(County) %>% filter(County != "Kenya")
county_data$Area <- paste(county_data$County, "County", sep = " ")
df_rows2 <- full_join(df_rows, county_data)

## Fill in the NAs with their respective counties.

df_rows2 <- df_rows2 %>%
  mutate(Area = ifelse(County == "Kenya", "xxx", Area)) %>%
  mutate(Area = zoo::na.locf(Area))

## Generate a SubCounty variable
df_rows2 <- df_rows2 %>%
  group_by(Area) %>%
  mutate(x = seq_along(County)) %>%
  mutate(SubArea = ifelse(x == 1, "County", "SubCounty")) %>%
  select(-x) %>% ungroup()

## Rename the nation al total row to Total
df_rows2 <- df_rows2 %>%
  mutate(SubArea = ifelse(County == "Kenya", "xxx", SubArea),
         County = ifelse(County == "Kenya", "Total", County))

## The forest areas should be special areas
df_rows2 <- df_rows2 %>%
  mutate(SubArea = ifelse(County %in% grep("forest|\\*", County, value = TRUE, ignore.case = TRUE),
                          "Special Area", SubArea))

## Rename the variables

names(df_rows2) <- c("SubCounty", "Population","LandArea(in Sq. Km)",
                     "Population Density(No. per Sq. Km)", "County", "AdminArea")

## Order the variables
df_rows2 <- df_rows2 %>%
  select(County, SubCounty, AdminArea, Population, `LandArea(in Sq. Km)`, `Population Density(No. per Sq. Km)`)

## Save the dataset

write_csv(df_rows2, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.7.csv")
V1_T2.7 <- df_rows2
#usethis::use_data(V1_T2.7, overwrite = TRUE)

