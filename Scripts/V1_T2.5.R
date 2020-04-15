## Author: Shelmith
## Date: 15th April, 2020
## Description: Scrapping V1_T2.5

library(tidyverse)
library(tabulizer)

df <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf",
                               pages = 22)
df2 <- data.frame(df[[1]])

df2$X1 <- NULL
names(df2) <- "X1"

## Drop the first 7 rows

df2$rownum <- 1:nrow(df2)
df2 <- df2 %>% filter(rownum >=8) %>% select(-rownum)

names(df2) <- "X1"


## Remove the commas
df2 <- map_df(df2, ~gsub(",", "", .x))

df2$County <- gsub("\\.", "", gsub("[[:digit:]]","",df2$X1))
df2$Values <- substr(df2$X1, 16, nchar(df2$X1))
df2$Values <- trimws(gsub("\\.|ark|t|\\*|[a-z]+|[A-Z]+", "", df2$Values))
df2$Values <- str_squish(df2$Values)

df2 <-df2 %>%
  separate(Values, into = c("Male","Female",
                            "Intersex", "Total"), sep = " ")

df2$X1 <- NULL

## Filter the rows labeled County, Male and the blank row

df2 <- df2 %>%
  filter(!County %in% grep("County", County, value = T, ignore.case = T))

## Convert the columns to the right type
df2[,-1] <- map_df(df2[,-1], ~trimws(.x))
df2[,-1] <- map_df(df2[,-1], ~as.numeric(.x))
df2a <- df2

df <- tabulizer::extract_areas("../../../Desktop/KenyanCensus2019/Resources/VOLUME 1 KPHC 2019.pdf",
                               pages = 23:30)

tab25_func <- function(num){

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

  df2 <-df2 %>%
    separate(Values, into = c("Male","Female",
                              "Intersex", "Total"), sep = " ")

  df2$X1 <- NULL

  ## Filter the rows labeled County, Male and the blank row

  df2 <- df2 %>%
    filter(!County %in% grep("County", County, value = T, ignore.case = T))

  ## Convert the columns to the right type
  df2[,-1] <- map_df(df2[,-1], ~trimws(.x))
  df2[,-1] <- map_df(df2[,-1], ~as.numeric(.x))

  return(df2)
}

vec <- 1:8
df_rows <- map_df(vec, tab25_func)

df_rows <- bind_rows(df2a,df_rows)
df_rows$County <- trimws(df_rows$County)

## Remove spaces from County

## Filter blank counties
df_rows <- df_rows %>%
  filter(County != "" &!County %in% grep("Demarcated|Intersex", County, value = TRUE, ignore.case = TRUE))


## Convert the NAs to 0s
df_rows[,-1] <- data.frame(sapply(df_rows[,-1], function(x)
                                  ifelse(is.na(x), 0,x)))

## Clean Aberdare National Park* and Nyandarua Central
df_rows <- df_rows %>%
  mutate(Female = ifelse(County == "Aberdare National Park*" ,4, Female),
         Total = ifelse(County == "Aberdare National Park*" ,15, Total))

df_rows <- df_rows %>%
  mutate(Male = ifelse(County == "Nyandarua Central" ,37329, Male)) %>%
  mutate(Female = ifelse(County == "Nyandarua Central" ,37931, Female)) %>%
  mutate(Intersex = ifelse(County == "Nyandarua Central" ,2, Intersex)) %>%
  mutate(Total = ifelse(County == "Nyandarua Central" ,75262, Total))


## Check that we are right
df_rows <- df_rows %>%
  mutate(fig = Male + Female + Intersex ) %>%
  mutate(checker = Total==fig)

## Drop the variables that we do not need.

df_rows <- df_rows %>%
  select(-fig, -checker)

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

df_rows2 <- df_rows2 %>%
  mutate(SubArea = ifelse(County == "Kenya", "xxx", SubArea),
         County = ifelse(County == "Kenya", "Total", County))

##Rename the variables

names(df_rows2) <- c("SubCounty", "Male","Female","Intersex", "Total", "County", "AdminArea")

df_rows2 <- df_rows2 %>%
  select(County, SubCounty, AdminArea, Male, Female, Intersex, Total)

## Save the dataset

write_csv(df_rows2, "../../../Desktop/KenyanCensus2019/Datasets/V1_T2.5.csv")
V1_T2.5 <- df_rows2
#usethis::use_data(V1_T2.2)

