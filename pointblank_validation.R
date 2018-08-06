### add libraries
#devtools::install_github("lingtax/Qualtrics")

library(readr)
library(pointblank)
library(dplyr)

## Set the filename for the data you want to work with in this session
filename <- "data/raw_anon.csv"

dat <- read_csv(filename)

## Change column types for the columns I potentially care about 
dat <- dat %>% 
  mutate(start_date = as.POSIXct(start_date),
         end_date = as.POSIXct(end_date),
         progress = as.numeric(progress),
         duration_in_seconds = as.numeric(duration_in_seconds),
         recorded_date = as.POSIXct(recorded_date),
         response_id = as.character(response_id),
         age = as.numeric(age),
         gender = as.character(gender),
         education = as.factor(education),
         household_income = as.factor(household_income),
         ideology_general = as.numeric(ideology_general),
         ideology_economic = as.numeric(ideology_economic),
         ideology_social = as.numeric(ideology_social),
         demographic_first_click = as.numeric(demographic_first_click),
         demographic_last_click = as.numeric(demographic_last_click),
         demographic_page_submit = as.numeric(demographic_page_submit),
         demographic_click_count = as.numeric(demographic_click_count),
         flu_sev_1 = as.numeric(flu_sev_1),
         flu_sev_2 = as.numeric(flu_sev_2),
         flu_sev_3 = as.numeric(flu_sev_3),
         flu_sus_1 = as.numeric(flu_sus_1),
         flu_sus_2 = as.numeric(flu_sus_2),
         flu_sus_3 = as.numeric(flu_sus_3),
         flu_mrr_1 = as.numeric(flu_mrr_1),
         flu_mrr_2 = as.numeric(flu_mrr_2),
         flu_mrr_3 = as.numeric(flu_mrr_3),
         flu_re_1 = as.numeric(flu_re_1),
         flu_re_2 = as.numeric(flu_re_2),
         flu_re_3 = as.numeric(flu_re_3),
         flu_se_1 = as.numeric(flu_se_1),
         flu_se_2 = as.numeric(flu_se_2),
         flu_se_3 = as.numeric(flu_se_3),
         flu_rc_1 = as.numeric(flu_rc_1),
         flu_rc_2 = as.numeric(flu_rc_2),
         flu_rc_3 = as.numeric(flu_rc_3),
         flu_int_1 = as.numeric(flu_int_1),
         flu_int_2 = as.numeric(flu_int_2),
         flu_int_3 = as.numeric(flu_int_3),
         cc_sev_1 = as.numeric(cc_sev_1),
         cc_sev_2 = as.numeric(cc_sev_2),
         cc_sev_3 = as.numeric(cc_sev_3),
         cc_sus_1 = as.numeric(cc_sus_1),
         cc_sus_2 = as.numeric(cc_sus_2),
         cc_sus_3 = as.numeric(cc_sus_3),
         cc_mrr_1 = as.numeric(cc_mrr_1),
         cc_mrr_2 = as.numeric(cc_mrr_2),
         cc_mrr_3 = as.numeric(cc_mrr_3),
         cc_re_1 = as.numeric(cc_re_1),
         cc_re_2 = as.numeric(cc_re_2),
         cc_re_3 = as.numeric(cc_re_3),
         cc_se_1 = as.numeric(cc_se_1),
         cc_se_2 = as.numeric(cc_se_2),
         cc_se_3 = as.numeric(cc_se_3),
         cc_rc_1 = as.numeric(cc_rc_1),
         cc_rc_2 = as.numeric(cc_rc_2),
         cc_rc_3 = as.numeric(cc_rc_3),
         cc_int_1 = as.numeric(cc_int_1),
         cc_int_2 = as.numeric(cc_int_2),
         cc_int_3 = as.numeric(cc_int_3),
         feedback = as.character(feedback),
         cc_condition_control = as.factor(cc_condition_control),
         flu_condition_control = as.factor(flu_condition_control),
         cc = as.factor(cc),
         flu = as.factor(flu),
         cc_condition_sev = as.factor(cc_condition_sev),
         cc_condition_sus = as.factor(cc_condition_sus),
         cc_condition_mrr = as.factor(cc_condition_mrr),
         cc_condition_rc = as.factor(cc_condition_rc),
         cc_condition_re = as.factor(cc_condition_re),
         cc_condition_se = as.factor(cc_condition_se),
         flu_condition_sev = as.factor(flu_condition_sev),
         flu_condition_sus = as.factor(flu_condition_sus),
         flu_condition_mrr = as.factor(flu_condition_mrr),
         flu_condition_se = as.factor(flu_condition_se),
         flu_condition_re = as.factor(flu_condition_re),
         flu_condition_rc = as.factor(flu_condition_rc))



# Check that dataframe includes all expected columns.
# Check that data is in the expected format/structure
# Note that failure will occur if display order information has not been exported from Qualtrics
# This is required for condition checking so is an important requirement
col_check <- 
  create_agent() %>%
  focus_on(
    tbl_name = "dat") %>%
    col_exists(column = start_date) %>%
    col_exists(column = end_date) %>%
    col_exists(column = status) %>%
    col_exists(column = progress) %>%
    col_exists(column = duration_in_seconds) %>%
    col_exists(column = finished) %>%
    col_exists(column = recorded_date) %>%
    col_exists(column = response_id) %>%
    col_exists(column = user_language) %>%
    col_exists(column = age) %>%
    col_exists(column = gender) %>%
    col_exists(column = education) %>%
    col_exists(column = household_income) %>%
    col_exists(column = ideology_general) %>%
    col_exists(column = ideology_economic) %>%
    col_exists(column = ideology_social) %>%
    col_exists(column = demographic_first_click) %>%
    col_exists(column = demographic_last_click) %>%
    col_exists(column = demographic_page_submit) %>%
    col_exists(column = demographic_click_count) %>%
    col_exists(column = flu_sev_1) %>%
    col_exists(column = flu_sev_2) %>%
    col_exists(column = flu_sev_3) %>%
    col_exists(column = flu_sus_1) %>%
    col_exists(column = flu_sus_2) %>%
    col_exists(column = flu_sus_3) %>%
    col_exists(column = flu_mrr_1) %>%
    col_exists(column = flu_mrr_2) %>%
    col_exists(column = flu_mrr_3) %>%
    col_exists(column = flu_re_1) %>%
    col_exists(column = flu_re_2) %>%
    col_exists(column = flu_re_3) %>%
    col_exists(column = flu_se_1) %>%
    col_exists(column = flu_se_2) %>%
    col_exists(column = flu_se_3) %>%
    col_exists(column = flu_rc_1) %>%
    col_exists(column = flu_rc_2) %>%
    col_exists(column = flu_rc_3) %>%
    col_exists(column = flu_int_1) %>%
    col_exists(column = flu_int_2) %>%
    col_exists(column = flu_int_3) %>%
    col_exists(column = cc_sev_1) %>%
    col_exists(column = cc_sev_2) %>%
    col_exists(column = cc_sev_3) %>%
    col_exists(column = cc_sus_1) %>%
    col_exists(column = cc_sus_2) %>%
    col_exists(column = cc_sus_3) %>%
    col_exists(column = cc_mrr_1) %>%
    col_exists(column = cc_mrr_2) %>%
    col_exists(column = cc_mrr_3) %>%
    col_exists(column = cc_re_1) %>%
    col_exists(column = cc_re_2) %>%
    col_exists(column = cc_re_3) %>%
    col_exists(column = cc_se_1) %>%
    col_exists(column = cc_se_2) %>%
    col_exists(column = cc_se_3) %>%
    col_exists(column = cc_rc_1) %>%
    col_exists(column = cc_rc_2) %>%
    col_exists(column = cc_rc_3) %>%
    col_exists(column = cc_int_1) %>%
    col_exists(column = cc_int_2) %>%
    col_exists(column = cc_int_3) %>%
    col_exists(column = feedback) %>%
    col_exists(column = cc_condition_control) %>%
    col_exists(column = flu_condition_control) %>%
    col_exists(column = cc) %>%
    col_exists(column = flu) %>%
    col_exists(column = cc_condition_sev) %>%
    col_exists(column = cc_condition_sus) %>%
    col_exists(column = cc_condition_mrr) %>%
    col_exists(column = cc_condition_rc) %>%
    col_exists(column = cc_condition_re) %>%
    col_exists(column = cc_condition_se) %>%
    col_exists(column = cc_condition_control_2) %>%
    col_exists(column = flu_condition_sev) %>%
    col_exists(column = flu_condition_sus) %>%
    col_exists(column = flu_condition_mrr) %>%
    col_exists(column = flu_condition_se) %>%
    col_exists(column = flu_condition_re) %>%
    col_exists(column = flu_condition_rc) %>%
    col_exists(column = flu_condition_control_2) %>%
    col_exists(column = flumeasures_do_q32) %>%
    col_exists(column = flumeasures_do_q42) %>%
    col_exists(column = flumeasures_do_q31) %>%
    col_exists(column = flumeasures_do_q41) %>%
    col_exists(column = flumeasures_do_q30) %>%
    col_exists(column = flumeasures_do_q43) %>%
    col_exists(column = flumeasures_do_q40) %>%
    col_exists(column = flumeasures_do_q29) %>%
    col_exists(column = flumeasures_do_q39) %>%
    col_exists(column = flumeasures_do_q28) %>%
    col_exists(column = flumeasures_do_q38) %>%
    col_exists(column = flumeasures_do_q27) %>%
    col_exists(column = flumeasures_do_q37) %>%
    col_exists(column = flumeasures_do_q26) %>%
    col_exists(column = flumeasures_do_q36) %>%
    col_exists(column = flumeasures_do_q25) %>%
    col_exists(column = flumeasures_do_q35) %>%
    col_exists(column = flumeasures_do_q24) %>%
    col_exists(column = flumeasures_do_q34) %>%
    col_exists(column = flumeasures_do_q23) %>%
    col_exists(column = flumeasures_do_q44) %>%
    col_exists(column = flumeasures_do_q33) %>%
    col_exists(column = climatechangemeasures_do_q52) %>%
    col_exists(column = climatechangemeasures_do_q65) %>%
    col_exists(column = climatechangemeasures_do_q54) %>%
    col_exists(column = climatechangemeasures_do_q64) %>%
    col_exists(column = climatechangemeasures_do_q53) %>%
    col_exists(column = climatechangemeasures_do_q63) %>%
    col_exists(column = climatechangemeasures_do_q62) %>%
    col_exists(column = climatechangemeasures_do_q51) %>%
    col_exists(column = climatechangemeasures_do_q61) %>%
    col_exists(column = climatechangemeasures_do_q50) %>%
    col_exists(column = climatechangemeasures_do_q60) %>%
    col_exists(column = climatechangemeasures_do_q49) %>%
    col_exists(column = climatechangemeasures_do_q59) %>%
    col_exists(column = climatechangemeasures_do_q48) %>%
    col_exists(column = climatechangemeasures_do_q58) %>%
    col_exists(column = climatechangemeasures_do_q47) %>%
    col_exists(column = climatechangemeasures_do_q57) %>%
    col_exists(column = climatechangemeasures_do_q46) %>%
    col_exists(column = climatechangemeasures_do_q56) %>%
    col_exists(column = climatechangemeasures_do_q45) %>%
    col_exists(column = climatechangemeasures_do_q66) %>%
    col_exists(column = climatechangemeasures_do_q55) %>%
    col_is_posix(column = start_date) %>%
    col_is_posix(column = end_date) %>%
    col_is_numeric(column = progress) %>%
    col_is_numeric(column = duration_in_seconds) %>%
    col_is_posix(column = recorded_date) %>%
    col_is_character(column = response_id) %>%
    col_is_numeric(column = age) %>%
    col_is_character(column = gender) %>%
    col_is_factor(column = education) %>%
    col_is_factor(column = household_income) %>%
    col_is_numeric(column = ideology_general) %>%
    col_is_numeric(column = ideology_economic) %>%
    col_is_numeric(column = ideology_social) %>%
    col_is_numeric(column = demographic_first_click) %>%
    col_is_numeric(column = demographic_last_click) %>%
    col_is_numeric(column = demographic_page_submit) %>%
    col_is_numeric(column = demographic_click_count) %>%
    col_is_numeric(column = flu_sev_1) %>%
    col_is_numeric(column = flu_sev_2) %>%
    col_is_numeric(column = flu_sev_3) %>%
    col_is_numeric(column = flu_sus_1) %>%
    col_is_numeric(column = flu_sus_2) %>%
    col_is_numeric(column = flu_sus_3) %>%
    col_is_numeric(column = flu_mrr_1) %>%
    col_is_numeric(column = flu_mrr_2) %>%
    col_is_numeric(column = flu_mrr_3) %>%
    col_is_numeric(column = flu_re_1) %>%
    col_is_numeric(column = flu_re_2) %>%
    col_is_numeric(column = flu_re_3) %>%
    col_is_numeric(column = flu_se_1) %>%
    col_is_numeric(column = flu_se_2) %>%
    col_is_numeric(column = flu_se_3) %>%
    col_is_numeric(column = flu_rc_1) %>%
    col_is_numeric(column = flu_rc_2) %>%
    col_is_numeric(column = flu_rc_3) %>%
    col_is_numeric(column = flu_int_1) %>%
    col_is_numeric(column = flu_int_2) %>%
    col_is_numeric(column = flu_int_3) %>%
    col_is_numeric(column = cc_sev_1) %>%
    col_is_numeric(column = cc_sev_2) %>%
    col_is_numeric(column = cc_sev_3) %>%
    col_is_numeric(column = cc_sus_1) %>%
    col_is_numeric(column = cc_sus_2) %>%
    col_is_numeric(column = cc_sus_3) %>%
    col_is_numeric(column = cc_mrr_1) %>%
    col_is_numeric(column = cc_mrr_2) %>%
    col_is_numeric(column = cc_mrr_3) %>%
    col_is_numeric(column = cc_re_1) %>%
    col_is_numeric(column = cc_re_2) %>%
    col_is_numeric(column = cc_re_3) %>%
    col_is_numeric(column = cc_se_1) %>%
    col_is_numeric(column = cc_se_2) %>%
    col_is_numeric(column = cc_se_3) %>%
    col_is_numeric(column = cc_rc_1) %>%
    col_is_numeric(column = cc_rc_2) %>%
    col_is_numeric(column = cc_rc_3) %>%
    col_is_numeric(column = cc_int_1) %>%
    col_is_numeric(column = cc_int_2) %>%
    col_is_numeric(column = cc_int_3) %>%
    col_is_character(column = feedback) %>%
    col_is_factor(column = cc_condition_control) %>%
    col_is_factor(column = flu_condition_control) %>%
    col_is_factor(column = cc) %>%
    col_is_factor(column = flu) %>%
    col_is_factor(column = cc_condition_sev) %>%
    col_is_factor(column = cc_condition_sus) %>%
    col_is_factor(column = cc_condition_mrr) %>%
    col_is_factor(column = cc_condition_rc) %>%
    col_is_factor(column = cc_condition_re) %>%
    col_is_factor(column = cc_condition_se) %>%
    col_is_factor(column = flu_condition_sev) %>%
    col_is_factor(column = flu_condition_sus) %>%
    col_is_factor(column = flu_condition_mrr) %>%
    col_is_factor(column = flu_condition_se) %>%
    col_is_factor(column = flu_condition_re) %>%
    col_is_factor(column = flu_condition_rc) %>%
    col_vals_between(
      column = age,
      left = 18,
      right = 100,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = ideology_general,
      left = 0,
      right = 10,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = ideology_social,
      left = 0,
      right = 10,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = ideology_economic,
      left = 0,
      right = 10,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sev_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sev_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sev_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sus_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sus_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_sus_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_mrr_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_mrr_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_mrr_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_re_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_re_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_re_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_se_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_se_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_se_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_rc_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_rc_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_rc_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_int_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_int_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = flu_int_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sev_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sev_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sev_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sus_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sus_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_sus_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_mrr_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_mrr_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_mrr_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_re_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_re_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_re_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_se_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_se_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_se_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_rc_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_rc_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_rc_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_int_1,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_int_2,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
    col_vals_between(
      column = cc_int_3,
      left = 1,
      right = 5,
      incl_na = TRUE,
      notify_count = 1) %>%
  interrogate()
get_html_summary(col_check, output_file = "col_check_validation_report-2018-08-06.html", output_dir = "validation")
