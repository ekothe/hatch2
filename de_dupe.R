library(dplyr)
library(readr)
library(lubridate)

dat <- read_csv("data/dat_col_passed.csv")

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

write_csv(dat_deduped, "data/raw_deduped.csv")
