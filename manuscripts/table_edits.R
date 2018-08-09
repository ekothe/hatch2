library(tidyverse)
library(here)

clim_sum <- read_rds(here("data", "dat.rds")) %>% 
  filter(study == "Climate Change") %>% 
  group_by(factor(condition)) %>% 
  summarise(int_sd = sd(cc_int, na.rm=TRUE), int_n = sum(!is.na(cc_int)), 
            re_sd  = sd(cc_re, na.rm=TRUE),  re_n  = sum(!is.na(cc_re)),
            rc_sd  = sd(cc_rc, na.rm=TRUE),  rc_n  = sum(!is.na(cc_rc)),
            mrr_sd = sd(cc_mrr, na.rm=TRUE), mrr_n = sum(!is.na(cc_mrr)),
            se_sd  = sd(cc_se, na.rm=TRUE),  se_n  = sum(!is.na(cc_se)),
            sev_sd = sd(cc_sev, na.rm=TRUE), sev_n = sum(!is.na(cc_sev)),
            sus_sd = sd(cc_sus, na.rm=TRUE), sus_n = sum(!is.na(cc_sus))
            )

names(clim_sum)[1] <- "condition"

tab3 <- clim_sum %>%
  select(condition, int_sd, int_n) %>% 
  mutate(ctr_sd = tab3$int_sd[1],
         ctr_n = tab3$int_n[1], 
         condition = case_when(
           condition == "Maladpative Response Rewards" ~ "Control vs. MRR",
           condition == "Severity" ~ "Control vs. Severity",
           condition == "Susceptibility" ~ "Control vs. Susceptibility",
           condition == "Self-efficacy" ~ "Control vs. Self-efficacy",
           condition == "Response Efficacy" ~ "Control vs. Response Efficacy",
           condition == "Response Costs" ~ "Control vs. Response Costs")
         ) %>% 
  filter(condition != "Control")

tab4 <- clim_sum %>%
  select(-one_of("int_sd", "int_n")) %>% 
  gather(-condition, key = "stat", value = "value") %>%
  separate(stat, into= c("construct", "stat"), sep="_") %>% 
  spread(key = stat, value= value)

tab4_ctr <-tab4 %>% 
  filter(condition=="Control") %>% 
  select(construct, ctr_n = n, ctr_sd = sd)

tab4 <- tab4 %>% 
  filter(condition != "Control") %>% 
  select(condition, construct, int_n = n, int_sd = sd) %>% 
  inner_join(tab4_ctr, by = "construct") %>% 
  mutate(construct = case_when(
          construct == "mrr" ~ "MRR",
          construct == "sev" ~ "Severity",
          construct == "sus" ~ "Susceptibility",
          construct == "se" ~ "Self-efficacy",
          construct == "re" ~ "Response Efficacy",
          construct == "rc" ~ "Response Costs"),
  condition = case_when(
           condition == "Maladpative Response Rewards" ~ "Control vs. MRR",
           condition == "Severity" ~ "Control vs. Severity",
           condition == "Susceptibility" ~ "Control vs. Susceptibility",
           condition == "Self-Efficacy" ~ "Control vs. Self-efficacy",
           condition == "Response Efficacy" ~ "Control vs. Response Efficacy",
           condition == "Response Costs" ~ "Control vs. Response Costs")
  ) 

