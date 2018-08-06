## This script takes the raw data from Qualtrics and gives all columns meaningful names %>% 
## Removes duplicates
## Removes potentially identifying feedback and replaces with the redacted version
## Creates csv filed called raw_anon

### add libraries
#devtools::install_github("lingtax/Qualtrics")

library(Qualtrics)
library(readr)
library(dplyr)
library(lubridate)

## Set the filename for the data you want to work with in this session
filename <- "data/final_raw_data.csv"

## Read in data
dat<-read_qualtrics(filename)

##  Read in variable names from metadata and rename according to old name match
metadata <- read_csv("metadata.csv")
names(dat)[match(metadata$import_name,names(dat))] <- metadata$var_name

### Create a frame of duplicates
duplicates<-dat[(duplicated(dat$worker_id) | duplicated(dat$worker_id, fromLast = TRUE)), ]

duplicates <- duplicates %>% 
  select(progress, worker_id, start_date, end_date, response_id) %>% 
  group_by(worker_id) %>%
  arrange(start_date) %>% 
  filter(!any(progress == 1)) # They saw more than the consent form

dat_deduped <- dat %>% 
  anti_join(duplicates, by="response_id") %>% 
  filter(finished == 1)

## Add redacted feedback and drop identifying version

for_redaction <- read_csv("data/for_redaction.csv")

for_redaction$feedback_anon <- for_redaction$feedback

dat_deduped <- inner_join(raw_deduped, for_redaction, by="response_id") %>% 
  select(-feedback.x) %>% 
  select(-feedback.y)

## Drops potentially identifying columns  
## Identify and drop columns to de-identify
## Note, this will throw a warning if a column you're attempting to drop does not exist but will still run. 
## The code includes columns that may not be in the dataset but if present should be dropped

drop_cols <- c("worker_id", 
               "assignment_id",
               "hit_id",
               "ip_address", 
               "recipient_last_name", 
               "recipient_first_name",
               "recipient_email",
               "location_latitude", 
               "location_longitude",
               "external_reference", 
               "distribution_channel")

raw_anon <- dat_deduped %>% select(-one_of(drop_cols))

## Write a file called raw_anon.csv

write_csv(raw_anon, "data/raw_anon.csv")
