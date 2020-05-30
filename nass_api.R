# Libraries ---------------------------------------------------------------
library(httr)
library(jsonlite)
library(tidyverse)

# change your working directory
# setwd("C:/Users/alexc/Desktop/R_ag")

# PUT YOUR API KEY IN YOUR ENV FILE
# readRenviron(".Renviron")


# NASS API function -------------------------------------------------------AP

getNass <- function(year, level){
  path <- paste("http://quickstats.nass.usda.gov/api/api_GET/?key=", Sys.getenv("API_KEY"), sep = "")
  params <- list(source_desc = "SURVEY", 
                 sector_desc = "CROPS", 
                 agg_level_desc = level, 
                 #group_desc = "FIELD CROPS",
                 statisticcat_desc = "AREA HARVESTED", 
                 unit_desc = "ACRES", 
                 reference_period_desc = "YEAR",
                 format = "JSON", 
                 year = year)
  
  api_query <- httr::GET(url = path, query = params)
  
  as.data.frame(fromJSON(content(api_query, as = "text", encoding = "UTF-8"), flatten = T))
}


# nass county data --------------------------------------------------------

nass_county <- getNass(2013, "COUNTY") %>% 
  dplyr::union(getNass(2014, "COUNTY")) %>% 
  dplyr::union(getNass(2015, "COUNTY")) %>% 
  dplyr::union(getNass(2016, "COUNTY")) %>% 
  dplyr::union(getNass(2017, "COUNTY")) %>% 
  filter(!str_detect(data.Value, ".*\\(.*\\).*.*"), 
         !str_detect(data.commodity_desc, ".*HAY.*"), 
         !str_detect(data.commodity_desc, ".*ALFALF.*")) %>% 
  mutate(acres = parse_number(data.Value), 
         fips = paste(data.state_ansi, data.county_ansi, sep = ""),
         crop = data.commodity_desc) %>% 
  filter(nchar(fips) > 4) %>% 
  group_by(fips, crop) %>% 
  summarize(avg_acres = mean(acres)) %>% 
  ungroup()


write_csv(nass_county, "nass_county.csv")