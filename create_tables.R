library(readr)
library(tableone)
library(dplyr)
library(labelled)

dat <- read_rds("data/dat.rds")

dat<-dat %>% 
  set_variable_labels(
    age = "Age in years",
    education = "Highest level of education",
    gender = "Gender",
    household_income = "Annual household income", 
    ideology_general = "Ideology - General", 
    ideology_economic = "Ideology - Economic",
    ideology_social = "Ideology - Social")

tab<-CreateTableOne(vars = c("age", 
                             "education",
                             "gender",
                             "household_income", 
                             "ideology_general", 
                             "ideology_economic", 
                             "ideology_social"), 
                    data = dat, strata = "study", test = FALSE)

write.csv(print(tab, varLabels = T), row.names = TRUE, "manuscripts/demo.csv")

climate <- dat %>% 
  filter(study=="Climate Change")

tab<- CreateTableOne(vars = c("age", 
                             "education",
                             "gender",
                             "household_income", 
                             "ideology_general", 
                             "ideology_economic", 
                             "ideology_social"), 
                    data = climate, test = FALSE)

write.csv(print(tab, varLabels = T), row.names = TRUE, "manuscripts/climate_demo.csv")


flu <- dat %>% 
  filter(study=="Flu vaccination")

tab<- CreateTableOne(vars = c("age", 
                              "education",
                              "gender",
                              "household_income", 
                              "ideology_general", 
                              "ideology_economic", 
                              "ideology_social"), 
                     data = flu, test = FALSE)

write.csv(print(tab, varLabels = T), row.names = TRUE, "manuscripts/flu_demo.csv")


dat<-dat %>% 
  set_variable_labels(
    flu_mrr = "Maladaptive response rewards",
    flu_sev = "Severity",
    flu_sus = "Susceptibility",
    flu_re = "Response Efficacy",
    flu_rc = "Response Costs",
    flu_se = "Self-Efficacy",
    flu_int = "Intention")

flu_measures<-CreateTableOne(vars = c("flu_mrr", 
                                      "flu_sev",
                                      "flu_sus",
                                      "flu_re", 
                                      "flu_rc", 
                                      "flu_se", 
                                      "flu_int"), 
                             data = flu, strata = "condition", test = FALSE)

write.csv(print(flu_measures, varLabels = T), row.names = TRUE, "manuscripts/flu_measures.csv")



climate_measures<-CreateTableOne(vars = c("cc_mrr", 
                                          "cc_sev",
                                          "cc_sus",
                                          "cc_re", 
                                          "cc_rc", 
                                          "cc_se", 
                                          "cc_int"), 
                                 data = climate, strata = "condition", test = FALSE)



write.csv(print(climate_measures, varLabels = T), row.names = TRUE, "manuscripts/climate_measures.csv")