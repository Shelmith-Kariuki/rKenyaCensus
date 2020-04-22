## Author: Shelmith
## Date: 15th April, 2020
## Description: Scrapping V1_T2.6

## Load the libraries required ------
library(devtools)
library(usethis)
library(tidyverse)
library(tabulizer)

## Extract the table ------
df <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf",
                               pages = 31:39)

tab26_func <- function(num){

  df2 <- data.frame(df[[num]])

  ## Drop the first 7 rows
  df2$rownum <- 1:nrow(df2)
  df2 <- df2 %>% filter(rownum >=8) %>% select(-rownum)

  names(df2) <- "X1"


  ## Remove the commas
  df2 <- map_df(df2, ~gsub(",", "", .x))

  df2$County <- gsub("\\.", "", gsub("[[:digit:]]","",df2$X1))
  df2$Values <- substr(df2$X1, 16, nchar(df2$X1))
  df2$Values <- trimws(gsub("\\.|ark|t|\\*|[a-z]+|[A-Z]+", "", df2$Values))
  #df2$Values <- str_squish(df2$Values)

  ## Separate the variable into different variables
  df2 <-df2 %>%
    separate(Values, into = c("Population","NumberOfHouseholds","AverageHouseholdSize"), sep = " ")
  df2$X1 <- NULL

  ## Filter the rows labeled County, Male and the blank row
  df2 <- df2 %>%
    filter(!County %in% grep("County", County, value = T, ignore.case = T))

  ## Convert the columns to the right type
  df2[,-1] <- map_df(df2[,-1], ~trimws(.x))
  df2[,-1] <- map_df(df2[,-1], ~as.numeric(.x))

  ## Revert the AverageHouseholdSize into its right state.
  df2 <- df2%>%
    mutate(AverageHouseholdSize = AverageHouseholdSize / 10)


  return(df2)
}

## Call the function
vec <- 1:9
df_rows <- map_df(vec, tab26_func)

## Remove trailing or leading spaces in the county variable
df_rows$County <- trimws(df_rows$County)

## Filter blank counties
df_rows <- df_rows %>%
  filter(County != "" &!County %in% grep("Demarcated|Excludes|Lodges|Households", County, value = TRUE, ignore.case = TRUE))

## Convert the NAs to 0s
df_rows[,-1] <- data.frame(sapply(df_rows[,-1], function(x)
  ifelse(is.na(x), 0,x)))

## Check that we are right
df_checker <- df_rows %>%
  mutate(fig = round(Population / NumberOfHouseholds,1)) %>%
  mutate(checker = AverageHouseholdSize==fig)

## Clean Aberdare National Park*
df_rows <- df_rows %>%
  mutate(Population = ifelse(County == "Aberdare National Park*" ,15, Population),
         NumberOfHouseholds = ifelse(County == "Aberdare National Park*" ,11, NumberOfHouseholds),
         AverageHouseholdSize = ifelse(County == "Aberdare National Park*" ,1.4, AverageHouseholdSize))

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

names(df_rows2) <- c("SubCounty", "Population","NumberOfHouseholds","AverageHouseholdSize", "County", "AdminArea")

## Order the variables
df_rows2 <- df_rows2 %>%
  select(County, SubCounty, AdminArea, Population, NumberOfHouseholds, AverageHouseholdSize)

## Save the dataset

write_csv(df_rows2, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.6.csv")
V1_T2.6 <- df_rows2
#usethis::use_data(V1_T2.6, overwrite = TRUE)

